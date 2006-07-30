(*
 *   InstantObjects DBEvolver Support
 *   ZeosDBO Catalog
 *)

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is: InstantObjects DBEvolver Support/ZeosDBO Catalog
 *
 * The Initial Developer of the Original Code is: Enrique Esquivel
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Joao Morais
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantZeosDBOCatalog;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  DB, ZDbcIntfs, InstantPersistence, InstantZeosDBO;

type
  // A TInstantCatalog that reads catalog information from a ZeosDBO
  // database. Can be used with a BDE broker.
  TInstantZeosDBOCatalog = class(TInstantBrokerCatalog)
  private
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    function ColumnTypeToDataType(const ColumnType: TZSQLType;
      out DataType: TInstantDataType;
      out AlternateDataTypes: TInstantDataTypes): Boolean;
    function GetBroker: TInstantZeosDBOBroker;
    function GetConnector: TInstantZeosDBOConnector;
  public
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
      override;
    property Broker: TInstantZeosDBOBroker read GetBroker;
    property Connector: TInstantZeosDBOConnector read GetConnector;
  end;

implementation

uses
  {$IFDEF D7+}Types,{$ENDIF} TypInfo, SysUtils, Classes, ZConnection,
  InstantClasses, InstantConsts, ZCompatibility;

procedure TInstantZeosDBOCatalog.AddFieldMetadatas(
  TableMetadata: TInstantTableMetadata);
var
  Fields: IZResultSet;
  FieldMetadata: TInstantFieldMetadata;
  FieldDataType: TInstantDataType;
  FieldAlternateDataTypes: TInstantDataTypes;
begin
  with Connector.Connection do
    Fields := DbcConnection.GetMetadata.GetColumns(Catalog, '',
      TableMetadata.Name, '');

  Fields.BeforeFirst;
  while Fields.Next do
    // Work around for a ZeosDBO behavior with Interbase and Firebird, where
    // metadata name with wildcards ('_' or '%') receives a '%' after its name,
    // and another drivers where metadata names are searched with LIKE clause
    if SameText(Fields.GetStringByName('TABLE_NAME'), TableMetadata.Name) then
    begin
      if ColumnTypeToDataType(TZSQLType(Fields.GetShortByName('DATA_TYPE')),
        FieldDataType, FieldAlternateDataTypes) then
      begin
        FieldMetadata := TableMetadata.FieldMetadatas.Add;
        FieldMetadata.Name := Fields.GetStringByName('COLUMN_NAME');
        FieldMetadata.DataType := FieldDataType;
        FieldMetadata.AlternateDataTypes := FieldAlternateDataTypes;
        FieldMetadata.Options := [];
        if not Fields.GetBooleanByName('IS_NULLABLE') then
          FieldMetadata.Options := FieldMetadata.Options + [foRequired];
        if TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata) then
          FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
        // work around bug in GetColumns for all drivers where
        // CHAR_OCTET_LENGTH is not assigned
        if (FieldMetadata.DataType in [dtString, dtMemo]) and
          (Fields.GetIntByName('CHAR_OCTET_LENGTH') > 0) then
          FieldMetadata.Size := Fields.GetIntByName('CHAR_OCTET_LENGTH')
        else
          FieldMetadata.Size := Fields.GetIntByName('COLUMN_SIZE');
      end
      else
        DoWarning(Format(SUnsupportedColumnSkipped, [
          TableMetadata.Name, Fields.GetStringByName('COLUMN_NAME'),
          GetEnumName(TypeInfo(TZSQLType), Fields.GetShortByName('DATA_TYPE'))]));
  end;
end;

procedure TInstantZeosDBOCatalog.AddIndexMetadatas(
  TableMetadata: TInstantTableMetadata);
var
  IndexName: string;
  PrimaryKeys: IZResultSet;
  IndexInfo: IZResultSet;
  IndexMetadata: TInstantIndexMetadata;
begin
  with Connector.Connection do
    PrimaryKeys := DbcConnection.GetMetadata.GetPrimaryKeys(Catalog, '',
      TableMetadata.Name);

  IndexMetadata := nil;
  PrimaryKeys.BeforeFirst;
  while PrimaryKeys.Next do
    // Work around for a ZeosDBO behavior with Interbase and Firebird where
    // metadata names are searched with LIKE clause
    if SameText(PrimaryKeys.GetStringByName('TABLE_NAME'), TableMetadata.Name) then
    begin
      IndexName := PrimaryKeys.GetStringByName('PK_NAME');
      // MySQL driver doesn't assign PK_NAME
      if IndexName = '' then
        IndexName := 'PRIMARY';
      if Assigned(IndexMetadata) and SameText(IndexMetadata.Name, IndexName) then
        IndexMetadata.Fields := IndexMetadata.Fields + ';' +
          PrimaryKeys.GetStringByName('COLUMN_NAME')
      else
      begin
        IndexMetadata := TableMetadata.IndexMetadatas.Add;
        IndexMetadata.Name := IndexName;
        IndexMetadata.Fields := PrimaryKeys.GetStringByName('COLUMN_NAME');
        IndexMetadata.Options := [ixPrimary, ixUnique];
      end;
    end;

  with Connector.Connection do
    IndexInfo := DbcConnection.GetMetadata.GetIndexInfo(Catalog, '',
      TableMetadata.Name, False, False);

  IndexMetadata := nil;
  IndexInfo.BeforeFirst;
  while IndexInfo.Next do
  begin
    IndexName := IndexInfo.GetStringByName('INDEX_NAME');
    // Exclude primary keys
    if not Assigned(TableMetadata.IndexMetadatas.Find(IndexName)) then
    begin
      if Assigned(IndexMetadata) and SameText(IndexMetadata.Name, IndexName) then
        IndexMetadata.Fields := IndexMetadata.Fields + ';' +
          IndexInfo.GetStringByName('COLUMN_NAME');
      begin
        IndexMetadata := TableMetadata.IndexMetadatas.Add;
        IndexMetadata.Name := IndexName;
        IndexMetadata.Fields := IndexInfo.GetStringByName('COLUMN_NAME');
        IndexMetadata.Options := [];
        if not IndexInfo.GetBooleanByName('NON_UNIQUE')
         { TODO : This work around must be removed for ZeosDBO versions
           that doesn't have the issue }
         // Work around for MySQL driver that return reverted value
         xor (Broker is TInstantZeosDBOMySQLBroker) then
          IndexMetadata.Options := IndexMetadata.Options + [ixUnique];
        if IndexInfo.GetStringByName('ASC_OR_DESC') = 'D' then
          IndexMetadata.Options := IndexMetadata.Options + [ixDescending];
      end;
    end;
  end;
end;

procedure TInstantZeosDBOCatalog.AddTableMetadatas(
  TableMetadatas: TInstantTableMetadatas);
var
  TableMetadata: TInstantTableMetadata;
  Tables: IZResultSet;
begin
  with Connector.Connection do
  begin
    if not Connected then
      Connect;
    DbcConnection.GetMetadata.ClearCache;
    Tables := DbcConnection.GetMetadata.GetTables(Catalog, '', '', nil);
  end;

  Tables.BeforeFirst;
  while Tables.Next do
  begin
    if SameText(Tables.GetStringByName('TABLE_TYPE'), 'TABLE') then
    begin
      TableMetadata := TableMetadatas.Add;
      TableMetadata.Name := Tables.GetStringByName('TABLE_NAME');
      // Call AddIndexMetadatas first, so that AddFieldMetadatas can see which
      // indexes are defined to correctly set the foIndexed option.
      AddIndexMetadatas(TableMetadata);
      AddFieldMetadatas(TableMetadata);
    end;
  end;
end;

function TInstantZeosDBOCatalog.ColumnTypeToDataType(const ColumnType: TZSQLType;
  out DataType: TInstantDataType;
  out AlternateDataTypes: TInstantDataTypes): Boolean;
begin
  Result := True;
  AlternateDataTypes := [];
  case ColumnType of
    stString,
      stUnicodeString: DataType := dtString;
    stBoolean: DataType := dtBoolean;
    stShort, stByte:
      begin
        DataType := dtBoolean;
        Include(AlternateDataTypes, dtInteger);
      end;
    stInteger,
      stLong: DataType := dtInteger;
    stFloat,
      stDouble:
      begin
        DataType := dtFloat;
        Include(AlternateDataTypes, dtCurrency);
      end;
    stBigDecimal: DataType := dtCurrency;
    stDate,
      stTime,
      stTimeStamp: DataType := dtDateTime;
    stBytes,
      stBinaryStream: DataType := dtBlob;
    stAsciiStream,
      stUnicodeStream: DataType := dtMemo;
  else
    Result := False;
  end;
end;

function TInstantZeosDBOCatalog.GetBroker: TInstantZeosDBOBroker;
begin
  Result := inherited Broker as TInstantZeosDBOBroker;
end;

function TInstantZeosDBOCatalog.GetConnector: TInstantZeosDBOConnector;
begin
  Result := Broker.Connector;
end;

procedure TInstantZeosDBOCatalog.InitTableMetadatas(
  ATableMetadatas: TInstantTableMetadatas);
begin
  ATableMetadatas.Clear;
  AddTableMetadatas(ATableMetadatas);
end;

end.
