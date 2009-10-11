(*
 *   InstantObjects DBEvolver Support
 *   RemObjects AnyDAC 2.0 Catalog
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
 * The Original Code is: InstantObjects DBEvolver Support/AnyDAC Catalog
 *
 * The Initial Developer of the Original Code is: David Taylor
 *
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantAnyDACCatalog;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  DB, InstantPersistence, InstantAnyDAC, InstantBrokers,
  InstantMetadata, InstantTypes, uADPhysIntf, uADStanIntf;

type
  //
  // A TInstantCatalog that reads catalog information from a
  // RemObjects AnyDAC 2 connection
  //
  TInstantAnyDACCatalog = class(TInstantBrokerCatalog)
  protected
    procedure AddFieldMetadatas(TableMetadata: TInstantTableMetadata;
      const Catalog : string; Schema : string);
    procedure AddIndexMetadatas(TableMetadata: TInstantTableMetadata;
      const Catalog : string; Schema : string);
    procedure AddTableMetadatas(TableMetadatas: TInstantTableMetadatas);
    function ColumnTypeToDataType(const ColumnType: TADDataType;
      out DataType: TInstantDataType; out AlternateDataTypes: TInstantDataTypes): Boolean;
    function GetBroker: TInstantAnyDACBroker;
    function GetConnector: TInstantAnyDACConnector;
  public
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas); override;
    property Broker: TInstantAnyDACBroker read GetBroker;
    property Connector: TInstantAnyDACConnector read GetConnector;
  end;

implementation

uses
  {$IFDEF D7+}Types,{$ENDIF} TypInfo, SysUtils, Classes, Variants,
  InstantClasses, InstantConsts, uADDatSManager;


procedure TInstantAnyDACCatalog.AddFieldMetadatas(TableMetadata: TInstantTableMetadata;
  const Catalog: string; Schema: string);
var
  Fields            : TADDatSView;
  FieldMetadata     : TInstantFieldMetadata;
  FieldDataType     : TInstantDataType;
  FieldAltDataTypes : TInstantDataTypes;
  ConnMetadata      : IADPhysConnectionMetadata;
  ADFieldName       : string;
  ADFieldType       : TADDataType;
  ADFieldAttrib     : TADDataAttributes;
  IntValue          : integer;
  FieldRow          : TADDatSRow;
  ColumnLength      : variant;
  I                 : integer;
begin
  ConnMetadata := Connector.Connection.ConnectionMetaDataIntf;
  Fields := ConnMetadata.GetTableFields(Catalog,Schema,TableMetadata.Name,'');

  for I := 0 to Fields.Rows.Count - 1 do
    begin
      FieldRow      := Fields.Rows[I];
      ADFieldName   := VarToStr(FieldRow.GetData('COLUMN_NAME'));
      ADFieldType   := TADDataType(FieldRow.GetData('COLUMN_DATATYPE'));
      IntValue      := FieldRow.GetData('COLUMN_ATTRIBUTES');
      ADFieldAttrib := TADDataAttributes(pointer(@IntValue)^);

      if ColumnTypeToDataType(ADFieldType, FieldDataType, FieldAltDataTypes) then
        begin
          FieldMetadata := TableMetadata.FieldMetadatas.Add;
          FieldMetadata.Name := ADFieldName;
          FieldMetadata.DataType := FieldDataType;
          FieldMetadata.AlternateDataTypes := FieldAltDataTypes;
          ColumnLength := FieldRow.GetData('COLUMN_LENGTH');
          if VarIsOrdinal(ColumnLength) then
            FieldMetadata.Size :=  Integer(ColumnLength);
          FieldMetadata.Options := [];

          if (not (caAllowNull in ADFieldAttrib)) then
            FieldMetadata.Options := FieldMetadata.Options + [foRequired];

          if (TableMetadata.IndexMetadatas.IsFieldIndexed(FieldMetadata)) then
            FieldMetadata.Options := FieldMetadata.Options + [foIndexed];
      end else
      begin
        DoWarning(Format(SUnsupportedColumnSkipped, [
          TableMetadata.Name, ADFieldName,
          GetEnumName(TypeInfo(TADDataType), ord(ADFieldType))]));
      end;
    end;
end;

procedure TInstantAnyDACCatalog.AddIndexMetadatas(
  TableMetadata: TInstantTableMetadata; const Catalog: string; Schema: string);
var
  ConnMetadata     : IADPhysConnectionMetadata;
  Indexes          : TADDatSView;
  IndexFields      : TADDatSView;
  IndexMetadata    : TInstantIndexMetadata;
  ADIndexName      : string;
  ADIndexKind      : TADPhysIndexKind;
  ADIndexFieldName : string;
  IndexRow         : TADDatSRow;
  IndexFieldRow    : TADDatSRow;
  I                : integer;
  J                : integer;
begin
  ConnMetadata := Connector.Connection.ConnectionMetaDataIntf;

  // Process table primary key
  Indexes := ConnMetadata.GetTablePrimaryKey(Catalog,Schema,TableMetadata.Name);

  for I := 0 to Indexes.Rows.Count - 1 do
    begin
      IndexRow := Indexes.Rows[I];
      ADIndexName := VarToStr(IndexRow.GetData('PKEY_NAME'));
      ADIndexKind := TADPhysIndexKind(IndexRow.GetData('INDEX_TYPE'));

      // Only add primary key (no other indexes should be present!)
      if (ADIndexKind <> ikPrimaryKey) then
        continue;

      // MySQL driver doesn't assign PK_NAME (Also true for AnyDAC?)
      if (ADIndexName = '') then
        ADIndexName := 'PRIMARY';

      IndexMetadata := TableMetadata.IndexMetadatas.Add;
      IndexMetadata.Name    := ADIndexName;
      IndexMetadata.Options := [ixPrimary, ixUnique];

      IndexFields := ConnMetadata.GetTablePrimaryKeyFields(Catalog,Schema,TableMetadata.Name,'');

      for J := 0 to IndexFields.Rows.Count - 1 do
        begin
          IndexFieldRow := IndexFields.Rows[J];
          ADIndexFieldName := VarToStr(IndexFieldRow.GetData('COLUMN_NAME'));
          if (J < 1) then
            IndexMetadata.Fields := ADIndexFieldName else
            IndexMetadata.Fields := IndexMetadata.Fields + ';' + ADIndexFieldName;
        end;
    end;

  // Process other table indexes
  Indexes := ConnMetadata.GetTableIndexes(Catalog,Schema,TableMetadata.Name,'');

  for I := 0 to Indexes.Rows.Count - 1 do
    begin
      IndexRow := Indexes.Rows[I];
      ADIndexName := VarToStr(IndexRow.GetData('INDEX_NAME'));
      ADIndexKind := TADPhysIndexKind(IndexRow.GetData('INDEX_TYPE'));

      // Skip over primary key since it was already added
      if (ADIndexKind = ikPrimaryKey) then
        continue;

      IndexMetadata := TableMetadata.IndexMetadatas.Add;
      IndexMetadata.Name := ADIndexName;

      if (ADIndexKind = ikUnique) then
        IndexMetadata.Options := [ixUnique];

      IndexFields := ConnMetadata.GetTableIndexFields(Catalog,Schema,TableMetadata.Name,ADIndexName,'');

      for J := 0 to IndexFields.Rows.Count - 1 do
        begin
          IndexFieldRow := IndexFields.Rows[J];
          ADIndexFieldName := VarToStr(IndexFieldRow.GetData('COLUMN_NAME'));
          if (J < 1) then
            IndexMetadata.Fields := ADIndexFieldName else
            IndexMetadata.Fields := IndexMetadata.Fields + ';' + ADIndexFieldName;

          // This is a bit of hack since each segment in the index defines
          // its own sort order. IO does not support this so we record the
          // sort order of the first s(and possibly only) key field. 
          if (J = 0) then
            begin
              if (VarToStr(IndexFieldRow.GetData('SORT_ORDER')) = 'D') then
                IndexMetadata.Options := IndexMetadata.Options + [ixDescending];
            end;
        end;
    end;
end;

procedure TInstantAnyDACCatalog.AddTableMetadatas(
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

function TInstantAnyDACCatalog.ColumnTypeToDataType(const ColumnType: TADDataType;
  out DataType: TInstantDataType; out AlternateDataTypes: TInstantDataTypes): Boolean;
begin
  Result := True;
  AlternateDataTypes := [];
  
  case ColumnType of
    uADStanIntf.dtAnsiString:    DataType := InstantTypes.dtString;
    uADStanIntf.dtWideString:    DataType := InstantTypes.dtString;
    uADStanIntf.dtByteString:    DataType := InstantTypes.dtString;
    uADStanIntf.dtBoolean:       DataType := InstantTypes.dtBoolean;
    uADStanIntf.dtDateTime:      DataType := InstantTypes.dtDateTime;
    uADStanIntf.dtTime:          DataType := InstantTypes.dtTime;
    uADStanIntf.dtDate:          DataType := InstantTypes.dtDate;
    uADStanIntf.dtDateTimeStamp: DataType := InstantTypes.dtDateTime;
    uADStanIntf.dtCurrency:      DataType := InstantTypes.dtCurrency;
    uADStanIntf.dtBCD:           DataType := InstantTypes.dtCurrency;
    uADStanIntf.dtFmtBCD:        DataType := InstantTypes.dtCurrency;
    uADStanIntf.dtDouble:
      begin
        DataType := InstantTypes.dtFloat;
        Include(AlternateDataTypes, InstantTypes.dtCurrency);
      end;
    uADStanIntf.dtBlob:          DataType := InstantTypes.dtBlob;
    uADStanIntf.dtHBlob:         DataType := InstantTypes.dtBlob;
    uADStanIntf.dtMemo:          DataType := InstantTypes.dtMemo;
    uADStanIntf.dtWideMemo:      DataType := InstantTypes.dtMemo;
    uADStanIntf.dtHMemo:         DataType := InstantTypes.dtMemo;
    uADStanIntf.dtWideHMemo:     DataType := InstantTypes.dtMemo;
    uADStanIntf.dtSByte,
    uADStanIntf.dtByte:
      begin
        DataType := InstantTypes.dtBoolean;
        Include(AlternateDataTypes, InstantTypes.dtInteger);
      end;
    uADStanIntf.dtInt16:         DataType := InstantTypes.dtInteger;
    uADStanIntf.dtInt32:         DataType := InstantTypes.dtInteger;
    uADStanIntf.dtInt64:         DataType := InstantTypes.dtInteger;
    uADStanIntf.dtUInt16:        DataType := InstantTypes.dtInteger;
    uADStanIntf.dtUInt32:        DataType := InstantTypes.dtInteger;
    uADStanIntf.dtUInt64:        DataType := InstantTypes.dtInteger;
  else
    Result := False;
  end;
end;

function TInstantAnyDACCatalog.GetBroker: TInstantAnyDACBroker;
begin
  Result := inherited Broker as TInstantAnyDACBroker;
end;

function TInstantAnyDACCatalog.GetConnector: TInstantAnyDACConnector;
begin
  Result := Broker.Connector;
end;

procedure TInstantAnyDACCatalog.InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
begin
  ATableMetadatas.Clear;
  AddTableMetadatas(ATableMetadatas);
end;

end.
