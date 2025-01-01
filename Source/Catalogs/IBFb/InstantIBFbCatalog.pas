(*
 *   InstantObjects DBEvolver Support
 *   IB/Fb Catalog
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
 * The Initial Developer of the Original Code is: Nando Dessena
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell, David Moorhouse
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantIBFbCatalog;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  InstantPersistence, InstantBrokers, InstantMetadata, InstantTypes;

type
  // A TInstantCatalog that reads catalog information from an InterBase
  // or Firebird database. Can be used with a SQL broker that accesses
  // InterBase or Firebird databases.
  TInstantIBFbCatalog = class(TInstantSQLBrokerCatalog)
  private
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    // Returns True if the TInstantDataType value that matches the supplied
    // combination of ColumnType, ColumnSubType and FieldScale is found. If
    // more than one datatypes apply, alternate data types are returned in
    // AlternateDataTypes, otherwise AlternateDataTypes is [] on exit.
    function ColumnTypeToDataType(const ColumnType: Integer;
      const ColumnSubType, FieldScale: Integer; out DataType: TInstantDataType;
      out AlternateDataTypes: TInstantDataTypes): Boolean;
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
  System.SysUtils
  , System.Classes
  , Data.DB
  , InstantConsts
  ;

{ TInstantIBFbCatalog }

procedure TInstantIBFbCatalog.AddIndexMetadatas(
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
            IndexFieldList.Add(Trim(IndexFields.FieldByName('RDB$FIELD_NAME').AsString));
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
        IndexMetadata.Name := Trim(Indexes.FieldByName('RDB$INDEX_NAME').AsString);
        IndexMetadata.Fields := GetIndexFields(IndexMetadata.Name);
        IndexMetadata.Options := [];
        if Pos('RDB$PRIMARY', Indexes.FieldByName('RDB$INDEX_NAME').AsString) = 1 then
          IndexMetadata.Options := IndexMetadata.Options + [ixPrimary, ixUnique]
        else if Indexes.FieldByName('RDB$UNIQUE_FLAG').AsInteger = 1 then
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

procedure TInstantIBFbCatalog.AddFieldMetadatas(
  TableMetadata: TInstantTableMetadata);
var
  Fields: TDataSet;
  FieldMetadata: TInstantFieldMetadata;
  AlternateDataTypes: TInstantDataTypes;
  FieldMetaDataType: TInstantDataType;
  FieldTypeName: string;
begin
  Fields := Broker.AcquireDataSet(GetSelectFieldsSQL(TableMetadata.Name));
  try
    Fields.Open;
    try
      while not Fields.Eof do
      begin
        FieldMetadata := TableMetadata.FieldMetadatas.Add;
        FieldMetadata.Name := Trim(Fields.FieldByName('RDB$FIELD_NAME').AsString);
        if ColumnTypeToDataType(
          Fields.FieldByName('RDB$FIELD_TYPE').AsInteger,
          Fields.FieldByName('RDB$FIELD_SUB_TYPE').AsInteger,
          Fields.FieldByName('RDB$FIELD_SCALE').AsInteger,
          FieldMetaDataType,
          AlternateDataTypes) then
        begin
          FieldMetadata.DataType := FieldMetaDataType;
          FieldMetadata.AlternateDataTypes := AlternateDataTypes;
          FieldMetadata.Options := [];
          if Fields.FieldByName('RDB$NULL_FLAG').AsInteger <> 0 then
            FieldMetadata.Options := FieldMetadata.Options + [foRequired];
          if TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata) then
            FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
          if FieldMetadata.DataType = dtString then
            FieldMetadata.Size := Fields.FieldByName('RDB$CHARACTER_LENGTH').AsInteger
          else
            FieldMetadata.Size := Fields.FieldByName('RDB$FIELD_LENGTH').AsInteger;
        end
        else
          begin
            FieldTypeName := Trim (Fields.FieldByName('RDB$TYPE_NAME').AsString);
            if FieldTypeName = '' then
              FieldTypeName := Format('[FieldType=%s FieldSubType=%s]',
                [Fields.FieldByName('RDB$FIELD_TYPE').AsString,
                Fields.FieldByName('RDB$FIELD_SUB_TYPE').AsString]);
            DoWarning(Format(SUnsupportedColumnSkipped,
              [TableMetadata.Name, FieldMetadata.Name, FieldTypeName]));
          end;
        Fields.Next;
      end;
    finally
      Fields.Close;
    end;
  finally
    Broker.ReleaseDataSet(Fields);
  end;
end;

procedure TInstantIBFbCatalog.AddTableMetadatas(
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
        TableMetadata.Name := Trim(Tables.FieldByName('RDB$RELATION_NAME').AsString);
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

function TInstantIBFbCatalog.ColumnTypeToDataType(const ColumnType: Integer;
  const ColumnSubType, FieldScale: Integer;  out DataType: TInstantDataType;
  out AlternateDataTypes: TInstantDataTypes): Boolean;
begin
{RDB$FIELDS.RDB$FIELD_TYPE values
• BLOB - 261  • BLOB_ID - 45  • BOOLEAN - 17  • CHAR - 14 • CSTRING - 40
• D_FLOAT - 11  • DOUBLE - 27 • FLOAT - 10  • INT64 - 16  • INTEGER - 8
• QUAD - 9  • SMALLINT - 7  • DATE - 12 (dialect 3 DATE)  • TIME - 13
• TIMESTAMP - 35  • VARCHAR - 37
These values are always present in table metadata,
RDB$TYPES.RDB$TYPE_NAME is not always defined for all types }

  AlternateDataTypes := [];
  Result := True;
  case ColumnType of
    7: // SHORT/SMALLINT
      begin
        DataType := dtBoolean;
        Include(AlternateDataTypes, dtInteger);
      end;
    8:   // INTEGER
      DataType := dtInteger;
    10, 27:  //FLOAT, DOUBLE
      DataType := dtFloat;
    35:  // TIMESTAMP
      DataType := dtDateTime;
    12:  // DATE
      DataType := dtDate;
    13:  // TIME
      DataType := dtTime;
    14, 37:   // TEXT, VARYING
      DataType := dtString;
    16:  // INT64
      if (ColumnSubType = 2) and (FieldScale >= -4) then
        DataType := dtCurrency
      else
        Result := False;
    261:  // BLOB
      if ColumnSubType = 1 then
        DataType := dtMemo
      else
        DataType := dtBlob;
    else
      Result := False;
  end;
end;

function TInstantIBFbCatalog.GetSelectFieldsSQL(
  const ATableName: string): string;
begin
  Result :=
    'select ' +
    '  RF.RDB$FIELD_NAME, RF.RDB$NULL_FLAG, ' +
    '  T.RDB$TYPE_NAME, ' +
    '  F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, F.RDB$FIELD_LENGTH, ' +
    '  F.RDB$FIELD_SCALE, F.RDB$CHARACTER_LENGTH ' +
    'from ' +
    '  RDB$RELATION_FIELDS RF ' +
    'join ' +
    '  RDB$FIELDS F ' +
    'on ' +
    '  RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME ' +
    'left join ' +      //Fix [ 1603022] 
    '  RDB$TYPES T ' +
    'on ' +
    '  F.RDB$FIELD_TYPE = T.RDB$TYPE ' +
    '  and T.RDB$FIELD_NAME = ''RDB$FIELD_TYPE'' ' +
    'where ' +
    '  RF.RDB$RELATION_NAME = ''' + ATableName + ''' ' +
    'order by ' +
    '  RF.RDB$FIELD_POSITION';
end;

function TInstantIBFbCatalog.GetSelectIndexesSQL(
  const ATableName: string): string;
begin
  Result :=
    'select ' +
    '  RDB$INDEX_NAME, RDB$UNIQUE_FLAG ' +
    'from ' +
    '  RDB$INDICES ' +
    'where ' +
    '  RDB$RELATION_NAME = ''' + ATableName + ''' ' +
    'order by ' +
    '  RDB$INDEX_NAME';
end;

function TInstantIBFbCatalog.GetSelectIndexFieldsSQL(
  const AIndexName: string): string;
begin
  Result :=
    'select ' +
    '  RDB$FIELD_NAME ' +
    'from ' +
    '  RDB$INDEX_SEGMENTS ' +
    'where ' +
    '  RDB$INDEX_NAME = ''' + AIndexName + ''' ' +
    'order by ' +
    '  RDB$FIELD_POSITION';
end;

function TInstantIBFbCatalog.GetSelectTablesSQL: string;
begin
  Result :=
    'select ' +
    '  RDB$RELATION_NAME ' +
    'from ' +
    '  RDB$RELATIONS ' +
    'where ' +
    '  RDB$SYSTEM_FLAG = 0 ' +
    '  and RDB$VIEW_BLR is null ' +
    'order by ' +
    '  RDB$RELATION_NAME';
end;

procedure TInstantIBFbCatalog.InitTableMetadatas(
  ATableMetadatas: TInstantTableMetadatas);
begin
  ATableMetadatas.Clear;
  AddTableMetadatas(ATableMetadatas);
end;


{  A. test table definition

CREATE TABLE "Test"
(
  "Class"	VARCHAR(32) NOT NULL,
  "Id"	VARCHAR(32) NOT NULL,
  "UpdateCount"	INTEGER,
  "StringAttr" VARCHAR(256),
  "BlobAttr" BLOB,
  "BooleanAttr" SMALLINT,
  "CurrencyAttr" DECIMAL(14,4),
  "DateTimeAttr" TIMESTAMP,
  "FloatAttr" DOUBLE PRECISION,
  "GraphicAttr" BLOB,
  "IntegerAttr" INTEGER,
  "MemoAttr" BLOB SUB_TYPE 1,
  "ReferenceAttributeClass" VARCHAR(32),
  "ReferenceAttributeId" VARCHAR(32),
  "EmbeddedPartsAtttribute" BLOB,
  "EmbeddedPartAttribute" BLOB,
  "ExternalPartAttributeClass" VARCHAR(32),
  "ExternalPartAttributeId" VARCHAR(32),
  "EmbeddedReferencesAtttribute" BLOB,
  "DateAttr" DATE,
  "TimeAttr" TIME,
 PRIMARY KEY ("Class", "Id")
);

B.  Result of "GetSelectFieldsSQL" method - run against IB 7.1 Win32 server

RDB$FIELD_NAME                    RDB$NULL_FLAG   RDB$TYPE_NAME                     RDB$FIELD_TYPE   RDB$FIELD_SUB_TYPE   RDB$FIELD_LENGTH   RDB$FIELD_SCALE   RDB$CHARACTER_LENGTH
===============================   =============   ===============================   ==============   ==================   ================   ===============   ====================
Class                             1               VARYING                           37                                    32                 0                 32
Id                                1               VARYING                           37                                    32                 0                 32
UpdateCount                                       LONG                              8                                     4                  0
StringAttr                                        VARYING                           37                                    256                0                 256
BlobAttr                                          BLOB                              261              0                    8                  0
BooleanAttr                                       SHORT                             7                                     2                  0
CurrencyAttr                                                                        16               2                    8                  -4
DateTimeAttr                                      TIMESTAMP                         35                                    8                  0
FloatAttr                                         DOUBLE                            27                                    8                  0
GraphicAttr                                       BLOB                              261              0                    8                  0
IntegerAttr                                       LONG                              8                                     4                  0
MemoAttr                                          BLOB                              261              1                    8                  0
ReferenceAttributeClass                           VARYING                           37                                    32                 0                 32
ReferenceAttributeId                              VARYING                           37                                    32                 0                 32
EmbeddedPartsAtttribute                           BLOB                              261              0                    8                  0
EmbeddedPartAttribute                             BLOB                              261              0                    8                  0
ExternalPartAttributeClass                        VARYING                           37                                    32                 0                 32
ExternalPartAttributeId                           VARYING                           37                                    32                 0                 32
EmbeddedReferencesAtttribute                      BLOB                              261              0                    8                  0
DateAttr                                          DATE                              12                                    4                  0
TimeAttr                                          TIME                              13                                    4                  0

}


end.
