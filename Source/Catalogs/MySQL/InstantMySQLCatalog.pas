(*
 *   InstantObjects DBEvolver Support
 *   MySQL Catalog
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
 * The Initial Developer of the Original Code is: David Moorhouse
 *
 * Portions created by the Initial Developer are Copyright (C) 2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * 
 *
 * ***** END LICENSE BLOCK ***** *)

 // TODO: separate out the MysQL4.x and MYSQl 5.x
 // TODO: not sure if different features need supporting for  different db engine options   e.g. MyISAM, InnoDB, etc 

unit InstantMySQLCatalog;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  InstantPersistence, InstantBrokers, InstantMetadata, InstantTypes;

type
  // A TInstantCatalog that reads catalog information from a MySQL
  // database. Can be used with a SQL broker that accesses MySQL databases.
  TInstantMySQLCatalog = class(TInstantSQLBrokerCatalog)
  private
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    // Returns True if the TInstantDataType value that matches the supplied
    // ColumnType. If more than one datatypes apply, alternate data types are
    // returned in AlternateDataTypes, otherwise AlternateDataTypes is [] on exit.
    function ColumnTypeToDataType(const ColumnType: string; out DataType:
        TInstantDataType; out AlternateDataTypes: TInstantDataTypes): Boolean;
    function GetSelectFieldsSQL(const ATableName: string): string;
    function GetSelectIndexesSQL(const ATableName: string): string;
    function GetSelectTablesSQL: string;
  public
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
      override;
  end;

implementation

uses
  SysUtils, Classes, DB, InstantConsts, StrUtils;

{ TInstantMySQLCatalog }

procedure TInstantMySQLCatalog.AddIndexMetadatas(TableMetadata: TInstantTableMetadata);
var
  Indexes: TDataSet;
  IndexMetadata: TInstantIndexMetadata;
  IndexFieldList: TStrings;
  KeyName: string;
begin
  IndexFieldList := TStringList.Create;
  Indexes := Broker.AcquireDataSet(GetSelectIndexesSQL(TableMetadata.Name));
  try
    Indexes.Open;
    try
      while not Indexes.Eof do
      begin
        IndexMetadata := TableMetadata.IndexMetadatas.Add;
        IndexMetadata.Name := Trim(Indexes.FieldByName('Key_name').AsString);
        IndexMetadata.Options := [];
        if Pos('PRIMARY', Indexes.FieldByName('Key_name').AsString) = 1 then
          IndexMetadata.Options := IndexMetadata.Options + [ixPrimary, ixUnique]
        else if Indexes.FieldByName('Non_unique').AsInteger = 0 then
          IndexMetadata.Options := IndexMetadata.Options + [ixUnique];
        { TODO : support other Options?  e.g partial indexes, descending}
        IndexFieldList.Clear;
        KeyName := Indexes.FieldByName('Key_name').AsString;
        while (not Indexes.Eof) and (KeyName = Indexes.FieldByName('Key_name').AsString) do
        begin
          IndexFieldList.Add(Trim(Indexes.FieldByName('Column_name').AsString));
          Indexes.Next;
        end;
        IndexMetadata.Fields := StringReplace(IndexFieldList.CommaText, ',', ';', [rfReplaceAll]);
      end;
    finally
      Indexes.Close;
    end;
  finally
    IndexFieldList.Free;
    Broker.ReleaseDataSet(Indexes);
  end;
end;

procedure TInstantMySQLCatalog.AddFieldMetadatas(TableMetadata: TInstantTableMetadata);
  function GetFieldSize(AFieldType: string): Integer;
  var
    I, J: Integer;
  begin
    Result := 0;
    I := Pos('(', AFieldType);
    if I > 0 then begin
      J := PosEx(',', AFieldType, I);
      if J = 0 then
        J := Pos(')', AFieldType);
      Result := StrToIntDef(Copy(AFieldType, I + 1, J -I - 1), 0);
    end;
  end;

var
  Fields: TDataSet;
  FieldMetadata: TInstantFieldMetadata;
  AlternateDataTypes: TInstantDataTypes;
  FieldMetaDataType: TInstantDataType;
begin
  Fields := Broker.AcquireDataSet(GetSelectFieldsSQL(TableMetadata.Name));
  try
    Fields.Open;
    try
      while not Fields.Eof do
      begin
        FieldMetadata := TableMetadata.FieldMetadatas.Add;
        FieldMetadata.Name := Trim(Fields.FieldByName('Field').AsString);
        if ColumnTypeToDataType(
          Fields.FieldByName('Type').AsString,
          FieldMetaDataType, AlternateDataTypes) then
        begin
          FieldMetadata.DataType := FieldMetaDataType;
          FieldMetadata.AlternateDataTypes := AlternateDataTypes;
          FieldMetadata.Options := [];
          if Fields.FieldByName('Null').AsString <> 'YES' then
            FieldMetadata.Options := FieldMetadata.Options + [foRequired];
          if TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata) then
            FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
          if FieldMetadata.DataType = dtString then
            FieldMetadata.Size := GetFieldSize(Fields.FieldByName('Type').AsString);
        end
        else
          DoWarning(Format(SUnsupportedColumnSkipped, [
            TableMetadata.Name, Fields.FieldByName('Field').AsString,
            Fields.FieldByName('Type').AsString]));
        Fields.Next;
      end;
    finally
      Fields.Close;
    end;
  finally
    Broker.ReleaseDataSet(Fields);
  end;
end;

procedure TInstantMySQLCatalog.AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
// NB: unresolved error
// MySQL and dbx raise a "Commands out of sync" error when using 2 simultaneous datasets
// store table names in stringlist to work around this
var
  Tables: TDataSet;
  TableMetadata: TInstantTableMetadata;
  TableList: TStringList;
  i: Integer;
begin
  TableList := TStringList.Create;
  Tables := Broker.AcquireDataSet(GetSelectTablesSQL());
  try
    Tables.Open;
    try
      while not Tables.Eof do
      begin
        TableList.Add(Trim(Tables.FieldByName('tables_in_' + Broker.Connector.DatabaseName).AsString));
        Tables.Next;
      end;
    finally
      Tables.Close;
    end;
    for i := 0 to TableList.Count - 1 do
    begin
      TableMetadata := TableMetadatas.Add;
      TableMetadata.Name := TableList[i];
      // Call AddIndexMetadatas first, so that AddFieldMetadatas can see what
      // indexes are defined to correctly set the foIndexed option.
      AddIndexMetadatas(TableMetadata);
      AddFieldMetadatas(TableMetadata);
    end;
  finally
    Broker.ReleaseDataSet(Tables);
    TableList.Free;
  end;
end;

function TInstantMySQLCatalog.ColumnTypeToDataType(const ColumnType: string;
    out DataType: TInstantDataType; out AlternateDataTypes: TInstantDataTypes):
    Boolean;
begin
// NB: datatypes at http://dev.mysql.com/doc/refman/4.1/en/data-types.html
  AlternateDataTypes := [];
  Result := True;

  if Pos('bool', ColumnType) = 1 then
    DataType := dtBoolean
  else if Pos('tinyint', ColumnType) = 1 then begin
    DataType := dtBoolean;
    Include(AlternateDataTypes, dtInteger);
  end
  else if Pos('bigint', ColumnType) = 1 then   // prevent "bigint" from matching "int"    i.e Int64
    Result := False
  else if Pos('int', ColumnType) > 0 then      // covers SMALLINT, MEDINT, INT and INTEGER
    DataType := dtInteger
  else if Pos('float', ColumnType) = 1 then
    DataType := dtFloat
  else if Pos('decimal', ColumnType) = 1 then
    DataType := dtCurrency
  else if Pos('char', ColumnType) = 1 then     // MySQL stores varchar(3) or smaller as char
    DataType := dtString
  else if Pos('varchar', ColumnType) = 1 then
    DataType := dtString
  else if Pos('text', ColumnType) > 0 then begin    // i.e. text, tinytext, mediumtext or longtext
    DataType := dtMemo;
    Include(AlternateDataTypes, dtString);   // IO strings longer than 255 stored as MySQL TEXT
  end
  else if Pos('blob', ColumnType) > 0 then     // blob, tinyblob, mediumblob, longblob
    DataType := dtBlob
  else if Pos('datetime', ColumnType) = 1 then
    DataType := dtDateTime
  else if Pos('date', ColumnType) = 1 then
    DataType := dtDate
  // need the following to prevent "timestamp" from matching with "time"
  else if Pos('timestamp', ColumnType) = 1 then  // seconds since UNIX epoch - not valid for IO
    Result := False
  else if Pos('time', ColumnType) = 1 then
    DataType := dtTime
  else
    Result := False;
end;

function TInstantMySQLCatalog.GetSelectFieldsSQL(
  const ATableName: string): string;
begin
  Result := 'DESCRIBE ' + ATableName;     // synonym for MySQL "SHOW COLUMNS FROM"
end;

function TInstantMySQLCatalog.GetSelectIndexesSQL(const ATableName: string): string;
begin
  Result := 'SHOW INDEX FROM ' +  ATableName;
end;

function TInstantMySQLCatalog.GetSelectTablesSQL: string;
begin
  Result := 'SHOW TABLES';
end;

procedure TInstantMySQLCatalog.InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
begin
  ATableMetadatas.Clear;
  AddTableMetadatas(ATableMetadatas);
end;


{  A. test table definition


CREATE TABLE Test
(
  Class	VARCHAR(32) NOT NULL,
  Id	VARCHAR(32) NOT NULL,
  UpdateCount	INTEGER,
  StringAttr VARCHAR(255),
  StringAttr2 VARCHAR(3),         // MySQL stores this as CHAR(3) !!
  StringAttr3 VARCHAR(256),       // MySQL stores this as TEXT
  BlobAttr BLOB,
  BooleanAttr SMALLINT,
  CurrencyAttr DECIMAL(14,4),
  DateTimeAttr TIMESTAMP,
  FloatAttr FLOAT,
  GraphicAttr BLOB,
  IntegerAttr INTEGER,
  MemoAttr TEXT,
  ReferenceAttributeClass VARCHAR(32),
  ReferenceAttributeId VARCHAR(32),
  EmbeddedPartsAtttribute BLOB,
  EmbeddedPartAttribute BLOB,
  ExternalPartAttributeClass VARCHAR(32),
  ExternalPartAttributeId VARCHAR(32),
  EmbeddedReferencesAtttribute BLOB,
  DateAttr DATE,
  TimeAttr TIME,
 PRIMARY KEY (Class, Id)
);


B.  Result of "GetSelectFieldsSQL" method - run against Win32 MySQL 4.1.14  with InnoDB engine

Field                             Type	          Null	      Key	      Default	     Extra
=====                             ====            ====        ===       =======      =====
Class	                            varchar(32)		              PRI
Id	                              varchar(32)		              PRI
UpdateCount	                      int(11)	YES
StringAttr	                      varchar(255)    YES
StringAttr2                       char(3)         YES
StringAttr3                       text            YES
BlobAttr	                        blob	          YES
BooleanAttr	                      smallint(6)	    YES
CurrencyAttr	                    decimal(14,4)	  YES
DateTimeAttr	                    datetime	      YES
FloatAttr	                        double	        YES
GraphicAttr	                      blob	          YES
IntegerAttr	                      int(11)	        YES
MemoAttr	                        text	YES
ReferenceAttributeClass	          varchar(32)	    YES
ReferenceAttributeId	            varchar(32)	    YES
EmbeddedPartsAtttribute	          blob	          YES
EmbeddedPartAttribute	            blob	          YES
ExternalPartAttributeClass	      varchar(32)	    YES
ExternalPartAttributeId	          varchar(32)	    YES
EmbeddedReferencesAtttribute	    blob           	YES
DateAttr	                        date          	YES
TimeAttr	                        time	          YES

}


end.
