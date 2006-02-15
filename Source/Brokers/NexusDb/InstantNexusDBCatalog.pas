(*
 *   InstantObjects DBEvolver Support
 *   NexusDB Catalog
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
 * The Original Code is: InstantObjects DBEvolver Support/NexusDB Catalog
 *
 * The Initial Developer of the Original Code is: Steven Mitchell
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantNexusDBCatalog;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}
{$I InstantNexusDBDefines.inc}

interface

uses
  InstantPersistence;

type
  // A TInstantCatalog that reads catalog information from a NexusDb
  // database. Can be used with a NexusDb broker.
  TInstantNexusDBCatalog = class(TInstantSQLBrokerCatalog)
  private
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    function ColumnTypeToDataType(const ColumnType: string; out DataType:
        TInstantDataType): Boolean;
    function GetSelectFieldsSQL(const ATableName: string): string;
    function GetSelectIndexesSQL(const ATableName: string): string;
    function GetSelectIndexFieldsSQL(const AIndexName: string): string;
    function GetSelectTablesSQL: string;
  public
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
      override;
  end;

implementation

uses
  SysUtils, Classes, DB, InstantConsts;
  
{ TInstantNexusDBCatalog }

procedure TInstantNexusDBCatalog.AddIndexMetadatas(
  TableMetadata: TInstantTableMetadata);
var
  Indexes: TDataSet;
  IndexMetadata: TInstantIndexMetadata;

  function GetIndexFields(const IndexName: string): string;
  var
    IndexFieldList: TStrings;
    IndexFields: TDataSet;
  begin
    IndexFieldList := TStringList.Create;
    try
      IndexFields := Broker.AcquireDataSet(GetSelectIndexFieldsSQL(IndexName));
      try
        IndexFields.Open;
        try
          while not IndexFields.Eof do
          begin
            IndexFieldList.Add(IndexFields.FieldByName('SEGMENT_FIELD').AsString);
            IndexFields.Next;
          end;
        finally
          IndexFields.Close;
        end;
      finally
        Broker.ReleaseDataSet(IndexFields);
      end;
      Result := StringReplace(IndexFieldList.CommaText, ',', ';',
        [rfReplaceAll]);
    finally
      IndexFieldList.Free;
    end;
  end;

begin
  Indexes := Broker.AcquireDataSet(GetSelectIndexesSQL(TableMetadata.Name));
  try
    Indexes.Open;
    try
      while not Indexes.Eof do
      begin
        IndexMetadata := TableMetadata.IndexMetadatas.Add;
        IndexMetadata.Name := Indexes.FieldByName('INDEX_NAME').AsString;

        // Ignore automatically generated 'Sequential Access Index'
        if SameText('Sequential Access Index', IndexMetadata.Name) then
        begin
          TableMetadata.IndexMetadatas.Remove(IndexMetadata);
          Indexes.Next;
          Continue;
        end;

        IndexMetadata.Fields := GetIndexFields(IndexMetadata.Name);
        IndexMetadata.Options := [];
        if
        {$IFDEF NX1}
          (SameText('key0', Indexes.FieldByName('INDEX_NAME').AsString))
        {$ELSE}
          (Pos('$SQL$PRIMARYKEY$', Indexes.FieldByName('INDEX_NAME').AsString) = 1)
        {$ENDIF}
            then
          IndexMetadata.Options := IndexMetadata.Options + [ixPrimary, ixUnique]
        else if not Indexes.FieldByName('INDEX_ALLOWSDUPS').AsBoolean then
          IndexMetadata.Options := IndexMetadata.Options + [ixUnique];
        { TODO : support other Options? }
        Indexes.Next;
      end;
    finally
      Indexes.Close;
    end;
  finally
    Broker.ReleaseDataSet(Indexes);
  end;
end;

procedure TInstantNexusDBCatalog.AddFieldMetadatas(
  TableMetadata: TInstantTableMetadata);
var
  Fields: TDataSet;
  FieldMetadata: TInstantFieldMetadata;
  FieldDataType: String;
  FieldMetaDataType: TInstantDataType;
begin
  Fields := Broker.AcquireDataSet(GetSelectFieldsSQL(TableMetadata.Name));
  try
    Fields.Open;
    try
      while not Fields.Eof do
      begin
        FieldMetadata := TableMetadata.FieldMetadatas.Add;
        FieldMetadata.Name := Fields.FieldByName('FIELD_NAME').AsString;
{$IFDEF NX1}
        FieldDataType := Fields.FieldByName('FIELD_TYPE').AsString;
{$ELSE}
        FieldDataType := Fields.FieldByName('FIELD_TYPE_NEXUS').AsString;
{$ENDIF}
        if ColumnTypeToDataType(FieldDataType, FieldMetaDataType) then
        begin
          FieldMetadata.DataType := FieldMetaDataType;
          FieldMetadata.Options := [];
          if Fields.FieldByName('FIELD_REQUIRED').AsBoolean then
            FieldMetadata.Options := FieldMetadata.Options + [foRequired];
          if TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata) then
            FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
          if FieldMetadata.DataType = dtString then
            FieldMetadata.Size := Fields.FieldByName('FIELD_UNITS').AsInteger
          else
            FieldMetadata.Size := Fields.FieldByName('FIELD_LENGTH').AsInteger;
          Fields.Next;
        end
        else
          DoWarning(Format(SUnsupportedColumnSkipped,
            [TableMetadata.Name, FieldMetadata.Name, FieldDataType]));
      end;
    finally
      Fields.Close;
    end;
  finally
    Broker.ReleaseDataSet(Fields);
  end;
end;

procedure TInstantNexusDBCatalog.AddTableMetadatas(
  TableMetadatas: TInstantTableMetadatas);
var
  Tables: TDataSet;
  TableMetadata: TInstantTableMetadata;
begin
  Tables := Broker.AcquireDataSet(GetSelectTablesSQL());
  try
    Tables.Open;
    try
      while not Tables.Eof do
      begin
        TableMetadata := TableMetadatas.Add;
        TableMetadata.Name := Tables.FieldByName('TABLE_NAME').AsString;
        // Call AddIndexMetadatas first, so that AddFieldMetadatas can see what
        // indexes are defined to correctly set the foIndexed option.
        AddIndexMetadatas(TableMetadata);
        AddFieldMetadatas(TableMetadata);
        Tables.Next;
      end;
    finally
      Tables.Close;
    end;
  finally
    Broker.ReleaseDataSet(Tables);
  end;
end;

function TInstantNexusDBCatalog.ColumnTypeToDataType(const ColumnType: string;
    out DataType: TInstantDataType): Boolean;
begin
  Result := True;
{$IFDEF NX1}
  if SameText(ColumnType, 'ShortString')
          or SameText(ColumnType, 'NullString') then
    DataType := dtString
  else if SameText(ColumnType, 'Int8')
          or SameText(ColumnType, 'Int16')
          or SameText(ColumnType, 'Int32') then
          //or SameText(ColumnType, 'nxtAutoInc') then    // is unsigned 32 bit
    DataType := dtInteger
  else if SameText(ColumnType, 'Single')
          or SameText(ColumnType, 'Double')
          or SameText(ColumnType, 'Extended') then
    DataType := dtFloat
  else if SameText(ColumnType, 'Currency') then
    DataType := dtCurrency
  else if SameText(ColumnType, 'Boolean') then
    DataType := dtBoolean
  else if SameText(ColumnType, 'DateTime')
          or SameText(ColumnType, 'Date')
          or SameText(ColumnType, 'Time')then
    DataType := dtDateTime
  else if SameText(ColumnType, 'BLOB') then
      DataType := dtBlob
  else if SameText(ColumnType, 'BLOB Memo') then
      DataType := dtMemo
{$ELSE}
  if SameText(ColumnType, 'nxtNullString')
          or SameText(ColumnType, 'nxtShortString') then
    DataType := dtString
  else if SameText(ColumnType, 'nxtInt8')
          or SameText(ColumnType, 'nxtInt16')
          or SameText(ColumnType, 'nxtInt32') then
          //or SameText(ColumnType, 'nxtAutoInc') then    // is unsigned 32 bit
    DataType := dtInteger
  else if SameText(ColumnType, 'nxtSingle')
          or SameText(ColumnType, 'nxtDouble')
          or SameText(ColumnType, 'nxtExtended') then
    DataType := dtFloat
  else if SameText(ColumnType, 'nxtCurrency') then
    DataType := dtCurrency
  else if SameText(ColumnType, 'nxtBoolean') then
    DataType := dtBoolean
  else if SameText(ColumnType, 'nxtDateTime')
          or SameText(ColumnType, 'nxtDate')
          or SameText(ColumnType, 'nxtTime')then
    DataType := dtDateTime
  else if SameText(ColumnType, 'nxtBlob') then
      DataType := dtBlob
  else if SameText(ColumnType, 'nxtBlobMemo') then
      DataType := dtMemo
{$ENDIF}
  else
    Result := False;
end;

function TInstantNexusDBCatalog.GetSelectFieldsSQL(
  const ATableName: string): string;
begin
  Result :=
    'select ' +
    '  FIELD_NAME, FIELD_REQUIRED, ' +
{$IFDEF NX1}
    '  FIELD_TYPE, ' +
{$ELSE}
    '  FIELD_TYPE_NEXUS, ' +
{$ENDIF}
    '  FIELD_LENGTH, FIELD_UNITS, ' +
    '  FIELD_DECIMALS, FIELD_INDEX ' +
    'from ' +
    '  "#FIELDS" ' +
    'where ' +
    '  TABLE_NAME = ''' + ATableName + ''' ' +
    'order by ' +
    '  FIELD_INDEX';
end;

function TInstantNexusDBCatalog.GetSelectIndexesSQL(
  const ATableName: string): string;
begin
  Result :=
    'select ' +
    '  INDEX_NAME, INDEX_ALLOWSDUPS, INDEX_INDEX ' +
    'from ' +
    '  "#INDICES" ' +
    'where ' +
    '  TABLE_NAME = ''' + ATableName + ''' ' +
    'order by ' +
    '  INDEX_INDEX';
end;

function TInstantNexusDBCatalog.GetSelectIndexFieldsSQL(
  const AIndexName: string): string;
begin
  Result :=
    'select ' +
    '  SEGMENT_FIELD, SEGMENT_INDEX ' +
    'from ' +
    '  "#INDEXFIELDS" ' +
    'where ' +
    '  INDEX_NAME = ''' + AIndexName + ''' ' +
    'order by ' +
    '  SEGMENT_INDEX';
end;

function TInstantNexusDBCatalog.GetSelectTablesSQL: string;
begin
  Result :=
    'select ' +
    '  TABLE_NAME ' +
    'from ' +
    '  "#TABLES" ' +
    'order by ' +
    '  TABLE_NAME';
end;

procedure TInstantNexusDBCatalog.InitTableMetadatas(
  ATableMetadatas: TInstantTableMetadatas);
begin
  ATableMetadatas.Clear;
  AddTableMetadatas(ATableMetadatas);
end;

end.
