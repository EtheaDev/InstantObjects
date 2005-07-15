(*
 *   InstantObjects DBEvolver Support
 *   MS-SQL Catalog
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
 * The Original Code is: InstantObjects DBEvolver Support
 *
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s): Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantMSSqlCatalog;

interface

uses
  InstantPersistence;

type
  // A TInstantCatalog that reads catalog information from an MS-SQL server database.
  // Can be used with a SQL broker that accesses MS-SQL databases.
  TInstantMSSqlCatalog = class(TInstantSQLBrokerCatalog)
  private
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    function ColumnTypeToDataType(const ColumnType: string;
      const ColumnSubType, FieldScale: Integer): TInstantDataType;
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

{ TInstantMSSqlCatalog }

procedure TInstantMSSqlCatalog.AddIndexMetadatas(
  TableMetadata: TInstantTableMetadata);
const
  IndexNonUnique   = $0001;
  IndexUnique      = $0002;
  IndexPrimaryKey  = $0004;
var
  IdxType: Integer;
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
            IndexFieldList.Add(Trim(IndexFields.FieldByName('COLUMN_NAME').AsString));
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
        IndexMetadata.Name := Trim(Indexes.FieldByName('INDEX_NAME').AsString);
        IndexMetadata.Fields := GetIndexFields(IndexMetadata.Name);
        IndexMetadata.Options := [];

        IdxType := Indexes.FieldByName('INDEX_TYPE').Value;
        if (IdxType and IndexPrimaryKey) = IndexPrimaryKey then
          IndexMetadata.Options := IndexMetadata.Options + [ixPrimary,ixUnique];
        if (IdxType and IndexUnique) = IndexUnique then
          IndexMetadata.Options := IndexMetadata.Options + [ixUnique];
        if Indexes.FieldByName('SORT_ORDER').Value = 'D' then
          IndexMetadata.Options := IndexMetadata.Options + [ixDescending];
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

procedure TInstantMSSqlCatalog.AddFieldMetadatas(
  TableMetadata: TInstantTableMetadata);
var
  Fields: TDataSet;
  FieldMetadata: TInstantFieldMetadata;
begin
  Fields := Broker.AcquireDataSet(GetSelectFieldsSQL(TableMetadata.Name));
  try
    Fields.Open;
    try
      while not Fields.Eof do
      begin
        FieldMetadata := TableMetadata.FieldMetadatas.Add;
        FieldMetadata.Name := Trim(Fields.FieldByName('COLUMN_NAME').AsString);
        FieldMetadata.DataType := ColumnTypeToDataType(
          Trim(Fields.FieldByName('COLUMN_TYPENAME').AsString),
          Fields.FieldByName('COLUMN_SUBTYPE').AsInteger,
          Fields.FieldByName('COLUMN_SCALE').AsInteger);
        FieldMetadata.Options := [];
        if Fields.FieldByName('COLUMN_NULLABLE').AsInteger <> 1 then
          FieldMetadata.Options := FieldMetadata.Options + [foRequired];
        if TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata) then
          FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
        FieldMetadata.Size := Fields.FieldByName('COLUMN_LENGTH').AsInteger;
        Fields.Next;
      end;
    finally
      Fields.Close;
    end;
  finally
    Broker.ReleaseDataSet(Fields);
  end;
end;

procedure TInstantMSSqlCatalog.AddTableMetadatas(
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
        TableMetadata.Name := Trim(Tables.FieldByName('TABLE_NAME').AsString);
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

function TInstantMSSqlCatalog.ColumnTypeToDataType(const ColumnType: string;
  const ColumnSubType, FieldScale: Integer): TInstantDataType;
begin
  if SameText(ColumnType, 'int') then
    Result := dtInteger
  else if SameText(ColumnType, 'float') then
    Result := dtFloat
  else if SameText(ColumnType, 'money') then
    Result := dtCurrency
  else if SameText(ColumnType, 'bit') then
    Result := dtBoolean
  else if SameText(ColumnType, 'varchar') then
    Result := dtString
  else if SameText(ColumnType, 'text') then
    Result := dtMemo
  else if SameText(ColumnType, 'datetime') then
    Result := dtDateTime
  else if SameText(ColumnType, 'image') then
    Result := dtBlob
  else
    raise Exception.CreateFmt(SUnsupportedColumnType, [ColumnType]);
end;

function TInstantMSSqlCatalog.GetSelectFieldsSQL(
  const ATableName: string): string;
begin
  Result :=
    'SELECT ' +
    '  c.name AS COLUMN_NAME, ' +
    '  c.isnullable AS COLUMN_NULLABLE, ' +
    '  t.name AS COLUMN_TYPENAME, ' +
    '  c.xtype AS COLUMN_SUBTYPE, ' +
    '  CASE ' +
    '    WHEN (d.oledb_data_type = 135) AND (c.xtype in (58, 61)) THEN 16' +
    '    WHEN (d.oledb_data_type = 6) THEN 8' +
    '    WHEN (d.oledb_data_type = 4) THEN 8' +
    '    WHEN (d.oledb_data_type = 20) THEN 34' +
    '    WHEN (d.oledb_data_type = 72) AND (c.xtype = 36) THEN 38' +
    '    ELSE c.length ' +
    '  END ' +
    '  AS COLUMN_LENGTH, ' +
    '  c.prec AS COLUMN_PRECISION, ' +
    '  CAST(c.scale AS SMALLINT) AS COLUMN_SCALE ' +
    'FROM ' +
    '  sysobjects o, syscolumns c, systypes t, master.dbo.spt_provider_types d ' +
    'WHERE ' +
    '  o.type in (''U'', ''V'', ''S'') ' +
    '  AND o.id = c.id ' +
    '  AND c.xusertype = t.xusertype ' +
    '  and c.xtype = d.ss_dtype ' +
    '  AND o.name = ''' + ATableName + ''' ' +
    ' ORDER BY c.colorder';
end;

function TInstantMSSqlCatalog.GetSelectIndexesSQL(
  const ATableName: string): string;
begin
  Result :=
    'SELECT DISTINCT ' +
    '  x.name AS INDEX_NAME, ' +
    '  (CASE WHEN x.status & 0x800 <> 0 THEN 4 ELSE 0 END) + ' +
    '  (CASE WHEN x.status & 0x2 <> 0 THEN 2 ELSE 1 END) ' +
    '  AS INDEX_TYPE, ' +
    '  (CASE WHEN indexkey_property(x.id, x.indid, 1, N''isdescending'') <> 0 THEN ''D'' ELSE ''A'' END) AS SORT_ORDER ' +
    'FROM ' +
    '  sysobjects o, sysindexes x, syscolumns c, sysindexkeys xk ' +
    'WHERE ' +
    '  o.id = x.id ' +
    '  and  o.id = c.id ' +
    '  and  o.id = xk.id ' +
    '  and  x.indid = xk.indid ' +
    '  and  c.colid = xk.colid ' +
    '  and  xk.keyno <= x.keycnt ' +
    '  and  o.xtype<>''S'' ' +
    '  and  LEFT(x.name, 8) <> ''_WA_Sys_'' ' +
    '  AND o.name = ''' + ATableName + ''' ' +
    'ORDER BY x.name';
end;

function TInstantMSSqlCatalog.GetSelectIndexFieldsSQL(
  const AIndexName: string): string;
begin
  Result :=
    'SELECT ' +
    '  c.name AS COLUMN_NAME ' +
    'FROM ' +
    '  sysobjects o, sysindexes x, syscolumns c, sysindexkeys xk ' +
    'WHERE ' +
    '  o.id = x.id ' +
    '  and  o.id = c.id ' +
    '  and  o.id = xk.id ' +
    '  and  x.indid = xk.indid ' +
    '  and  c.colid = xk.colid ' +
    '  and  xk.keyno <= x.keycnt ' +
    '  and  o.xtype<>''S'' ' +
    '  and  LEFT(x.name, 8) <> ''_WA_Sys_'' ' +
    '  AND x.name = ''' + AIndexName + ''' ' +
    'ORDER BY xk.keyno';
end;

function TInstantMSSqlCatalog.GetSelectTablesSQL: string;
begin
  Result :=
    'SELECT ' +
    '  name AS TABLE_NAME ' +
    'FROM sysobjects ' +
    'WHERE type = ''U'' ' +
    'ORDER BY name';
end;

procedure TInstantMSSqlCatalog.InitTableMetadatas(
  ATableMetadatas: TInstantTableMetadatas);
begin
  ATableMetadatas.Clear;
  AddTableMetadatas(ATableMetadatas);
end;

(*
COLUMN_NAME                   COLUMN_TYPENAMECOLUMN_POSITION COLUMN_TYPE COLUMN_DATATYPE COLUMN_SUBTYPE COLUMN_LENGTH COLUMN_PRECISION COLUMN_SCALE COLUMN_NULLABLE
----------------------------- ------------------------------ ----------- --------------- -------------- ------------- ---------------- ------------ ---------------
Class                         varchar        1               0           129             167            32            32               NULL         0
Id                            varchar        2               0           129             167            32            32               NULL         0
UpdateCount                   int            3               0           3               56             4             10               0            1
StringAttr                    varchar        4               0           129             167            10            10               NULL         1
BlobAttr                      image          5               0           128             34             16            NULL             NULL         1
BooleanAttr                   bit            6               0           11              104            1             1                0            1
CurrencyAttr                  money          7               0           6               60             8             19               4            1
DateTimeAttr                  datetime       8               0           135             61             16            23               3            1
FloatAttr                     float          9               0           5               62             8             53               NULL         1
GraphicAttr                   image          10              0           128             34             16            NULL             NULL         1
IntegerAttr                   int            11              0           3               56             4             10               0            1
MemoAttr                      text           12              0           129             35             16            NULL             NULL         1
ReferenceAttributeClass       varchar        13              0           129             167            32            32               NULL         1
ReferenceAttributeId          varchar        14              0           129             167            32            32               NULL         1
EmbeddedPartsAtttribute       image          15              0           128             34             16            NULL             NULL         1
EmbeddedPartAttribute         image          16              0           128             34             16            NULL             NULL         1
ExternalPartAttributeClass    varchar        17              0           129             167            32            32               NULL         1
ExternalPartAttributeId       varchar        18              0           129             167            32            32               NULL         1
EmbeddedReferencesAtttribute  image          19              0           128             34             16            NULL             NULL         1
*)

end.


