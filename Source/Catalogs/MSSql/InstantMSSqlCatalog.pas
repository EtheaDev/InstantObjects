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
 * Contributor(s): Nando Dessena, Bernard Simmons
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantMSSqlCatalog;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  InstantPersistence, InstantBrokers, InstantMetadata, InstantTypes;

type
  // A TInstantCatalog that reads catalog information from an MS-SQL server database.
  // Can be used with a SQL broker that accesses MS-SQL databases.
  TInstantMSSqlCatalog = class(TInstantSQLBrokerCatalog)
  private
    FSQLServerVersion: string;
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    function ColumnTypeToDataType(const ColumnType: string;
      const ColumnSubType, FieldScale: Integer;
      out DataType: TInstantDataType): Boolean;
    function GetSelectFieldsSQL(const ATableName: string): string;
    function GetSelectIndexesSQL(const ATableName: string): string;
    function GetSelectIndexFieldsSQL(const AIndexName: string): string;
    function GetSelectTablesSQL: string;
    {
      Returns a string which is formatted as follows:
      'Microsoft SQL Server 2005 - 9.00.1399.06 (Intel X86)'
      'Microsoft SQL Server  2000 - 8.00.760 (Intel X86)'
      'Microsoft SQL Server  7.00 - 7.00.623 (Intel X86)'
      See http://support.microsoft.com/kb/321185 for complete details
      The string is retrieved on demand and then cached in FSQLServerVersion.
    }
    function GetSQLServerVersion: string;
  public
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas); override;
  end;

implementation

uses
  System.SysUtils
  , System.Classes
  , Data.DB
  , InstantConsts
  ;

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
            IndexFieldList.Add(IndexFields.FieldByName('COLUMN_NAME').AsString);
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
  FieldDataType: TInstantDataType;
begin
  Fields := Broker.AcquireDataSet(GetSelectFieldsSQL(TableMetadata.Name));
  try
    Fields.Open;
    try
      while not Fields.Eof do
      begin
        if ColumnTypeToDataType(
          Fields.FieldByName('COLUMN_TYPENAME').AsString,
          Fields.FieldByName('COLUMN_SUBTYPE').AsInteger,
          Fields.FieldByName('COLUMN_SCALE').AsInteger, FieldDataType) then
        begin
          FieldMetadata := TableMetadata.FieldMetadatas.Add;
          FieldMetadata.Name := Fields.FieldByName('COLUMN_NAME').AsString;
          FieldMetadata.DataType := FieldDataType;
          if FieldDataType = dtDateTime then
            FieldMetadata.AlternateDataTypes := [dtDate, dtTime];
          FieldMetadata.Options := [];
          if Fields.FieldByName('COLUMN_NULLABLE').AsInteger <> 1 then
            FieldMetadata.Options := FieldMetadata.Options + [foRequired];
          if TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata) then
            FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
          FieldMetadata.Size := Fields.FieldByName('COLUMN_LENGTH').AsInteger;
        end
        else
          DoWarning(Format(SUnsupportedColumnSkipped, [
            TableMetadata.Name, Fields.FieldByName('COLUMN_NAME').AsString,
            Fields.FieldByName('COLUMN_TYPENAME').AsString]));
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

function TInstantMSSqlCatalog.ColumnTypeToDataType(const ColumnType: string;
  const ColumnSubType, FieldScale: Integer;
  out DataType: TInstantDataType): Boolean;
begin
  Result := True;
  if SameText(ColumnType, 'int') then
    DataType := dtInteger
  else if SameText(ColumnType, 'float') then
    DataType := dtFloat
  else if SameText(ColumnType, 'money') then
    DataType := dtCurrency
  else if SameText(ColumnType, 'bit') then
    DataType := dtBoolean
  else if SameText(ColumnType, 'varchar') then
    DataType := dtString
  else if SameText(ColumnType, 'text') then
    DataType := dtMemo
  else if SameText(ColumnType, 'datetime') then
    DataType := dtDateTime
  else if SameText(ColumnType, 'image') then
    DataType := dtBlob
  else
    Result := False;
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
    '    WHEN (c.xtype in (58, 61)) THEN 16' +
    '    WHEN (c.xtype = 122) THEN 8' +
    '    WHEN (c.xtype = 59) THEN 8' +
    '    WHEN (c.xtype = 127) THEN 34' +
    '    WHEN (c.xtype = 36) THEN 38' +
    '    ELSE c.length ' +
    '  END ' +
    '  AS COLUMN_LENGTH, ' +
    '  c.prec AS COLUMN_PRECISION, ' +
    '  CAST(c.scale AS SMALLINT) AS COLUMN_SCALE ' +
    'FROM ' +
    '  sysobjects o, syscolumns c, systypes t ' +
    'WHERE ' +
    '  o.type in (''U'', ''V'', ''S'') ' +
    '  AND o.id = c.id ' +
    '  AND c.xusertype = t.xusertype ' +
    '  AND o.name = ''' + ATableName + ''' ' +
    ' ORDER BY c.colorder';
end;

function TInstantMSSqlCatalog.GetSelectIndexesSQL(
  const ATableName: string): string;

  function GetSortOrderExpression: string;
  begin
    // All indexes are reported as ascending in SQL Server 7.
  	if Pos('Microsoft SQL Server  7.00', GetSQLServerVersion) > 0 then
      Result := '''A'''
    else
      Result := '(CASE WHEN indexkey_property(x.id, x.indid, 1, N''isdescending'') <> 0 THEN ''D'' ELSE ''A'' END)';
  end;

begin
  Result :=
    'SELECT DISTINCT ' +
    '  x.name AS INDEX_NAME, ' +
    '  (CASE WHEN x.status & 0x800 <> 0 THEN 4 ELSE 0 END) + ' +
    '  (CASE WHEN x.status & 0x2 <> 0 THEN 2 ELSE 1 END) ' +
    '  AS INDEX_TYPE, ' + GetSortOrderExpression + ' AS SORT_ORDER ' +
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
    'WHERE type = ''U'' and OBJECTPROPERTY(id, N''IsMSShipped'') <> 1' +
    'ORDER BY name';
end;

function TInstantMSSqlCatalog.GetSQLServerVersion: string;
var
  VersionDataSet: TDataSet;
begin
  if FSQLServerVersion = '' then
  begin
    VersionDataSet := Broker.AcquireDataSet('SELECT @@VERSION');
    try
      VersionDataSet.Open;
      try
        FSQLServerVersion := VersionDataSet.Fields[0].AsString;
      finally
        VersionDataSet.Close;
      end;
    finally
      Broker.ReleaseDataSet(VersionDataSet);
    end;
  end;
  Result := FSQLServerVersion;
end;

procedure TInstantMSSqlCatalog.InitTableMetadatas(
  ATableMetadatas: TInstantTableMetadatas);
begin
  FSQLServerVersion := '';
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
DateAttr                      datetime       8               0           135             61             16            23               3            1
TimeAttr                      datetime       8               0           135             61             16            23               3            1
*)

end.


