(*
 *   InstantObjects DBEvolver Support
 *   BDE Catalog
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
 * The Original Code is: InstantObjects DBEvolver Support/BDE Catalog
 *
 * The Initial Developer of the Original Code is: Steven Mitchell
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantBDECatalog;

{$I ../../InstantDefines.inc}

interface

uses
  InstantPersistence, DB, DBTables;

type
  // A TInstantCatalog that reads catalog information from a BDE
  // database. Can be used with a BDE broker.
  TInstantBDECatalog = class(TInstantBrokerCatalog)
  private
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    function ColumnTypeToDataType(const ColumnType: TFieldType): TInstantDataType;
  public
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
      override;
  end;

implementation

uses
  SysUtils, Classes, TypInfo, Dialogs, InstantConsts;
  
procedure TInstantBDECatalog.AddFieldMetadatas(
  TableMetadata: TInstantTableMetadata);
var
  vTable: TTable;
  FieldMetadata: TInstantFieldMetadata;
  i: Integer;
begin
  vTable := TTable.Create(nil);
  try
    vTable.DatabaseName := Broker.Connector.DatabaseName;
    vTable.TableName := TableMetadata.Name;
    vTable.FieldDefs.Update;
    for i := 0 to Pred(vTable.FieldDefs.Count) do
    begin
      FieldMetadata := TableMetadata.FieldMetadatas.Add;
      FieldMetadata.Name := Trim(vTable.FieldDefs[i].Name);
      FieldMetadata.DataType := ColumnTypeToDataType(vTable.FieldDefs[i].DataType);
      FieldMetadata.Options := [];
      if vTable.FieldDefs[i].Required then
        FieldMetadata.Options := FieldMetadata.Options + [foRequired];
      if TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata) then
        FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
      { TODO : support ExternalTableName? }
      if FieldMetadata.DataType in [dtString, dtMemo] then
        FieldMetadata.Size := vTable.FieldDefs[i].Size;
    end;
  finally
    vTable.Free;
  end;
end;

{ TInstantBDECatalog }

procedure TInstantBDECatalog.AddIndexMetadatas(
  TableMetadata: TInstantTableMetadata);
var
  vTable: TTable;
  IndexMetadata: TInstantIndexMetadata;
  i: Integer;
begin
  vTable := TTable.Create(nil);
  try
    vTable.DatabaseName := Broker.Connector.DatabaseName;
    vTable.TableName := TableMetadata.Name;
    vTable.IndexDefs.Update;

    for i := 0 to Pred(vTable.IndexDefs.Count) do
    begin
      IndexMetadata := TableMetadata.IndexMetadatas.Add;
      if ixPrimary in vTable.IndexDefs[i].Options then
        IndexMetadata.Name := Trim(vTable.IndexDefs[i].Name)
      else
        IndexMetadata.Name := ChangeFileExt(vTable.TableName, '')
                + Trim(vTable.IndexDefs[i].Name);
      IndexMetadata.Fields := vTable.IndexDefs[i].Fields;
      IndexMetadata.Options := vTable.IndexDefs[i].Options;
    end;
  finally
    vTable.Free;
  end;
end;

procedure TInstantBDECatalog.AddTableMetadatas(
  TableMetadatas: TInstantTableMetadatas);
var
  vDatabaseName: String;
  vTableName: String;
  i: integer;
  TableMetadata: TInstantTableMetadata;
  vTables: TStringList;
begin
  vTables := TStringList.Create;
  try
    vDatabaseName := Broker.Connector.DatabaseName;
    Session.GetTableNames(vDatabaseName, '*.*', true, false, vTables);
    for i := 0 to Pred(vTables.Count) do
    begin
      vTableName := ExtractFileName(Trim(vTables.Strings[i]));
      TableMetadata := TableMetadatas.Add;
      TableMetadata.Name := vTableName;
      // Call AddIndexMetadatas first, so that AddFieldMetadatas can see what
      // indexes are defined to correctly set the foIndexed option.
      AddIndexMetadatas(TableMetadata);
      AddFieldMetadatas(TableMetadata);
      TableMetadata.Name := ChangeFileExt(vTableName, '');
    end;
  finally
    vTables.Free;
  end;
end;

function TInstantBDECatalog.ColumnTypeToDataType(const ColumnType: TFieldType):
    TInstantDataType;
begin
  case ColumnType of
    ftString:       Result := dtString;
    ftSmallint,
    ftInteger:      Result := dtInteger;
    ftBoolean:      Result := dtBoolean;
    ftFloat:        Result := dtFloat;
    ftCurrency:     Result := dtCurrency;
    ftDate,
    ftTime,
    ftDateTime:     Result := dtDateTime;
    ftAutoInc:      Result := dtInteger;
    ftBlob,
    ftGraphic:      Result := dtBlob;
    ftMemo:         Result := dtMemo;
  else
    raise Exception.CreateFmt(SUnsupportedColumnType,
            [GetEnumName(TypeInfo(TFieldType), Ord(ColumnType))]);
  end;
end;

procedure TInstantBDECatalog.InitTableMetadatas(
  ATableMetadatas: TInstantTableMetadatas);
begin
  ATableMetadatas.Clear;
  AddTableMetadatas(ATableMetadatas);
end;

end.
