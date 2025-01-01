(*
 *   InstantObjects DBEvolver Support
 *   Embarcadero FireDAC Catalog
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
 * The Original Code is: InstantObjects DBEvolver Support/FireDAC Catalog
 *
 * The Initial Developer of the Original Code is: David Taylor
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Carlo Barazzetta
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantFireDACCatalog;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Data.DB
  , InstantPersistence
  , InstantFireDAC
  , InstantBrokers
  , FireDAC.Stan.Def
  , InstantMetadata
  , InstantTypes
  , FireDAC.Phys.Intf
  , FireDAC.Stan.Intf
  ;

type
  //
  // A TInstantCatalog that reads catalog information from a
  // Embarcadero FireDAC connection
  //
  TInstantFireDACCatalog = class(TInstantSQLBrokerCatalog)
  protected
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata;
      const Catalog : string; Schema : string);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata;
      const Catalog : string; Schema : string);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    function ColumnTypeToDataType(const ColumnType: TFDDataType;
      const ColumnLength : variant; out DataType: TInstantDataType;
      out AlternateDataTypes: TInstantDataTypes): Boolean;
    function GetBroker: TInstantFireDACBroker;
    function GetConnector: TInstantFireDACConnector;
  public
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas); override;
    property Broker: TInstantFireDACBroker read GetBroker;
    property Connector: TInstantFireDACConnector read GetConnector;
  end;

implementation

uses
  System.Types
  , System.TypInfo
  , System.SysUtils
  , System.Classes
  , System.Variants
  , InstantClasses
  , InstantConsts
  , FireDAC.DatS
  ;


procedure TInstantFireDACCatalog.AddFieldMetadatas(TableMetadata: TInstantTableMetadata;
  const Catalog: string; Schema: string);
var
  Fields            : TFDDatSView;
  FieldMetadata     : TInstantFieldMetadata;
  FieldDataType     : TInstantDataType;
  FieldAltDataTypes : TInstantDataTypes;
  ConnMetadata      : IFDPhysConnectionMetadata;
  FDFieldName       : string;
  FDFieldType       : TFDDataType;
  FDFieldAttrib     : TFDDataAttributes;
  IntValue          : integer;
  FieldRow          : TFDDatSRow;
  ColumnLength      : variant;
  I                 : integer;
begin
  ConnMetadata := Connector.Connection.ConnectionMetaDataIntf;
  Fields := ConnMetadata.GetTableFields(Catalog,Schema,TableMetadata.Name,'');

  for I := 0 to Fields.Rows.Count - 1 do
    begin
      FieldRow      := Fields.Rows[I];
      FDFieldName   := VarToStr(FieldRow.GetData('COLUMN_NAME'));
      FDFieldType   := TFDDataType(FieldRow.GetData('COLUMN_DATATYPE'));
      IntValue      := FieldRow.GetData('COLUMN_ATTRIBUTES');
      ColumnLength  := FieldRow.GetData('COLUMN_LENGTH');
      FDFieldAttrib := TFDDataAttributes(pointer(@IntValue)^);

      if ColumnTypeToDataType(FDFieldType, ColumnLength, FieldDataType, FieldAltDataTypes) then
        begin
          FieldMetadata := TableMetadata.FieldMetadatas.Add;
          FieldMetadata.Name := FDFieldName;
          FieldMetadata.DataType := FieldDataType;
          FieldMetadata.AlternateDataTypes := FieldAltDataTypes;

          if VarIsOrdinal(ColumnLength) then
            FieldMetadata.Size := Integer(ColumnLength);

          FieldMetadata.Options := [];

          if (not (caAllowNull in FDFieldAttrib)) then
            FieldMetadata.Options := FieldMetadata.Options + [foRequired];

          if (TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata)) then
            FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
      end else
      begin
        DoWarning(Format(SUnsupportedColumnSkipped, [
          TableMetadata.Name, FDFieldName,
          GetEnumName(TypeInfo(TFDDataType), ord(FDFieldType))]));
      end;
    end;
end;

procedure TInstantFireDACCatalog.AddIndexMetadatas(
  TableMetadata: TInstantTableMetadata; const Catalog: string; Schema: string);
var
  ConnMetadata     : IFDPhysConnectionMetadata;
  Indexes          : TFDDatSView;
  IndexFields      : TFDDatSView;
  IndexMetadata    : TInstantIndexMetadata;
  FDIndexName      : string;
  FDIndexKind      : TFDPhysIndexKind;
  FDIndexFieldName : string;
  IndexRow         : TFDDatSRow;
  IndexFieldRow    : TFDDatSRow;
  I                : integer;
  J                : integer;
begin
  ConnMetadata := Connector.Connection.ConnectionMetaDataIntf;

  // Process table primary key
  Indexes := ConnMetadata.GetTablePrimaryKey(Catalog,Schema,TableMetadata.Name);

  for I := 0 to Indexes.Rows.Count - 1 do
    begin
      IndexRow := Indexes.Rows[I];
      FDIndexName := VarToStr(IndexRow.GetData('INDEX_NAME'));
      FDIndexKind := TFDPhysIndexKind(IndexRow.GetData('INDEX_TYPE'));

      // Only add primary key (no other indexes should be present!)
      if (FDIndexKind <> ikPrimaryKey) then
        continue;

      // MySQL driver doesn't assign PK_NAME (Also true for FireDAC?)
      if (FDIndexName = '') then
        FDIndexName := 'PRIMARY';

      IndexMetadata := TableMetadata.IndexMetadatas.Add;
      IndexMetadata.Name    := FDIndexName;
      IndexMetadata.Options := [ixPrimary, ixUnique];

      IndexFields := ConnMetadata.GetTablePrimaryKeyFields(Catalog,Schema,TableMetadata.Name,'');

      for J := 0 to IndexFields.Rows.Count - 1 do
        begin
          IndexFieldRow := IndexFields.Rows[J];
          FDIndexFieldName := VarToStr(IndexFieldRow.GetData('COLUMN_NAME'));
          if (J < 1) then
            IndexMetadata.Fields := FDIndexFieldName else
            IndexMetadata.Fields := IndexMetadata.Fields + ';' + FDIndexFieldName;
        end;
    end;

  // Process other table indexes
  Indexes := ConnMetadata.GetTableIndexes(Catalog,Schema,TableMetadata.Name,'');

  for I := 0 to Indexes.Rows.Count - 1 do
    begin
      IndexRow := Indexes.Rows[I];
      FDIndexName := VarToStr(IndexRow.GetData('INDEX_NAME'));
      FDIndexKind := TFDPhysIndexKind(IndexRow.GetData('INDEX_TYPE'));

      // Skip over primary key since it was already added
      if (FDIndexKind = ikPrimaryKey) then
        continue;

      IndexMetadata := TableMetadata.IndexMetadatas.Add;
      IndexMetadata.Name := FDIndexName;

      if (FDIndexKind = ikUnique) then
        IndexMetadata.Options := [ixUnique];

      IndexFields := ConnMetadata.GetTableIndexFields(Catalog,Schema,TableMetadata.Name,FDIndexName,'');

      for J := 0 to IndexFields.Rows.Count - 1 do
        begin
          IndexFieldRow := IndexFields.Rows[J];
          FDIndexFieldName := VarToStr(IndexFieldRow.GetData('COLUMN_NAME'));
          if (J < 1) then
            IndexMetadata.Fields := FDIndexFieldName else
            IndexMetadata.Fields := IndexMetadata.Fields + ';' + FDIndexFieldName;

          // This is a bit of hack since each segment in the index defines
          // its own sort order. IO does not support this so we record the
          // sort order of the first (and possibly only) key field.
          if (J = 0) then
            begin
              if (VarToStr(IndexFieldRow.GetData('SORT_ORDER')) = 'D') then
                IndexMetadata.Options := IndexMetadata.Options + [ixDescending];
            end;
        end;
    end;
end;

procedure TInstantFireDACCatalog.AddTableMetadatas(
  TableMetadatas: TInstantTableMetadatas);
var
  Metadata  : TInstantTableMetadata;
  Tables    : TStringList;
  Catalog   : string;
  Schema    : string;
  BaseName  : string;
  TableName : string;
  I         : integer;
begin
  Tables := TStringList.Create;

  try
    with Connector.Connection do
      begin
        if not Connected then
          Open;
        RefreshMetadataCache;
        GetTableNames('','','',Tables,[osMy],[tkTable]);
      end;

    for I := 0 to Tables.Count - 1 do
      begin
        with Connector.Connection do
          DecodeObjectName(Tables.Strings[I],Catalog,Schema,BaseName,TableName);
        Metadata := TableMetadatas.Add;
        Metadata.Name := TableName;
        // Call AddIndexMetadatas first, so that AddFieldMetadatas can see
        // which indexes are defined to correctly set the foIndexed option.
        AddIndexMetadatas(Metadata,Catalog,Schema);
        AddFieldMetadatas(Metadata,Catalog,Schema);
      end;
  finally
    Tables.Free;
  end;
end;

function TInstantFireDACCatalog.ColumnTypeToDataType(const ColumnType: TFDDataType;
  const ColumnLength : variant; out DataType: TInstantDataType;
  out AlternateDataTypes: TInstantDataTypes): Boolean;
begin
  Result := True;
  AlternateDataTypes := [];

  case ColumnType of
    FireDAC.Stan.Intf.dtAnsiString,
    FireDAC.Stan.Intf.dtWideString:
      begin
        if VarIsOrdinal(ColumnLength) and (Integer(ColumnLength) = MAXINT) then
          begin
            DataType := InstantTypes.dtMemo;
            Include(AlternateDataTypes, InstantTypes.dtString);
          end else
          begin
            DataType := InstantTypes.dtString;
          end;
      end;
    FireDAC.Stan.Intf.dtByteString:
      begin
        if VarIsOrdinal(ColumnLength) and (Integer(ColumnLength) = MAXINT) then
          DataType := InstantTypes.dtBlob else
          Result := False;
      end;
    FireDAC.Stan.Intf.dtBoolean:       DataType := InstantTypes.dtBoolean;
    FireDAC.Stan.Intf.dtDateTime:      DataType := InstantTypes.dtDateTime;
    FireDAC.Stan.Intf.dtTime:          DataType := InstantTypes.dtTime;
    FireDAC.Stan.Intf.dtDate:          DataType := InstantTypes.dtDate;
    FireDAC.Stan.Intf.dtDateTimeStamp: DataType := InstantTypes.dtDateTime;
    FireDAC.Stan.Intf.dtCurrency:      DataType := InstantTypes.dtCurrency;
    FireDAC.Stan.Intf.dtBCD:           DataType := InstantTypes.dtCurrency;
    FireDAC.Stan.Intf.dtFmtBCD:        DataType := InstantTypes.dtCurrency;
    FireDAC.Stan.Intf.dtDouble:
      begin
        DataType := InstantTypes.dtFloat;
        Include(AlternateDataTypes, InstantTypes.dtCurrency);
      end;
    FireDAC.Stan.Intf.dtBlob:          DataType := InstantTypes.dtBlob;
    FireDAC.Stan.Intf.dtHBlob:         DataType := InstantTypes.dtBlob;
    FireDAC.Stan.Intf.dtMemo:          DataType := InstantTypes.dtMemo;
    FireDAC.Stan.Intf.dtWideMemo:      DataType := InstantTypes.dtMemo;
    FireDAC.Stan.Intf.dtHMemo:         DataType := InstantTypes.dtMemo;
    FireDAC.Stan.Intf.dtWideHMemo:     DataType := InstantTypes.dtMemo;
    FireDAC.Stan.Intf.dtSByte,
    FireDAC.Stan.Intf.dtByte:
      begin
        DataType := InstantTypes.dtBoolean;
        Include(AlternateDataTypes, InstantTypes.dtInteger);
      end;
    FireDAC.Stan.Intf.dtInt16:         DataType := InstantTypes.dtInteger;
    FireDAC.Stan.Intf.dtInt32:         DataType := InstantTypes.dtInteger;
    FireDAC.Stan.Intf.dtInt64:         DataType := InstantTypes.dtInteger;
    FireDAC.Stan.Intf.dtUInt16:        DataType := InstantTypes.dtInteger;
    FireDAC.Stan.Intf.dtUInt32:        DataType := InstantTypes.dtInteger;
    FireDAC.Stan.Intf.dtUInt64:        DataType := InstantTypes.dtInteger;
  else
    Result := False;
  end;
end;

function TInstantFireDACCatalog.GetBroker: TInstantFireDACBroker;
begin
  Result := inherited Broker as TInstantFireDACBroker;
end;

function TInstantFireDACCatalog.GetConnector: TInstantFireDACConnector;
begin
  Result := Broker.Connector;
end;

procedure TInstantFireDACCatalog.InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
begin
  ATableMetadatas.Clear;
  AddTableMetadatas(ATableMetadatas);
end;

end.
