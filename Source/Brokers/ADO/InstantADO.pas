(*
 *   InstantObjects
 *   ADO Support
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
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Carlo Barazzetta, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantADO;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Classes, Db, ADODB, SysUtils, InstantPersistence, InstantClasses,
  InstantCommand, InstantBrokers, InstantMetadata, InstantTypes;

type
  TInstantADOProviderType = (ptUnknown, ptMSJet, ptMSSQLServer, ptOracle, ptMySQL, ptIBMDB2);
  TInstantADOBuildMethod = (bmDefault, bmADOX, bmSQL);

  TInstantADOConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FConnectionString: string;
    function GetLinkFileName: string;
    procedure SetLinkFileName(const Value: string);
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
  public
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    function Edit: Boolean; override;
    property LinkFileName: string read GetLinkFileName write SetLinkFileName;
  published
    property ConnectionString: string read FConnectionString write FConnectionString;
  end;

  TInstantADOConnector = class(TInstantConnectionBasedConnector)
  private
    procedure BuildDatabaseADOX(Scheme: TInstantScheme);
    procedure BuildDatabaseSQL(Scheme: TInstantScheme);
    procedure DoBuildDatabase(Scheme: TInstantScheme; BuildMethod: TInstantADOBuildMethod);
    function GetConnection: TADOConnection;
    function GetProviderType: TInstantADOProviderType;
    procedure SetConnection(const Value: TADOConnection);
  protected
    function CreateBroker: TInstantBroker; override;
    function GetCanTransaction: Boolean; virtual;
    function GetDatabaseExists: Boolean; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalCreateDatabase; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
  public
    procedure BuildDatabase(Scheme: TInstantScheme; BuildMethod: TInstantADOBuildMethod); overload;
    procedure CompactDatabase;
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    property CanTransaction: Boolean read GetCanTransaction;
    property ProviderType: TInstantADOProviderType read GetProviderType;
  published
    property Connection: TADOConnection read GetConnection write SetConnection;
  end;

  TInstantADOBroker = class(TInstantRelationalBroker)
  private
    function GetConnector: TInstantADOConnector;
  protected
    function CreateResolver(const TableName: string): TInstantResolver; override;
    function InternalCreateQuery: TInstantQuery; override;
  public
    property Connector: TInstantADOConnector read GetConnector;
  end;

  TInstantADOResolver = class(TInstantResolver)
  private
    function GetBroker: TInstantADOBroker;
    function GetDataSet: TCustomADODataSet;
  protected
    function CreateDataSet: TDataSet; override;
    function CreateNavigationalLinkResolver(const ATableName: string):
        TInstantNavigationalLinkResolver; override;
    function Locate(const AClassName, AObjectId: string): Boolean; override;
  public
    function FormatTableName(const ATableName: string): string; virtual;
    property Broker: TInstantADOBroker read GetBroker;
    property DataSet: TCustomADODataSet read GetDataSet;
  end;

  TInstantADOQuery = class;

  TInstantADOTranslator = class(TInstantRelationalTranslator)
  private
    function GetQuery: TInstantADOQuery;
  protected
    function GetDelimiters: string; override;
    function GetQuote: Char; override;
    function TranslateFunctionName(const FunctionName: string; Writer: TInstantIQLWriter): Boolean; override;
  public
    property Query: TInstantADOQuery read GetQuery;
  end;

  TInstantADOQuery = class(TInstantRelationalQuery)
  private
    FParamsObject: TParams;
    FQuery: TADOQuery;
    function GetQuery: TADOQuery;
    function GetConnector: TInstantADOConnector;
    function GetParamsObject: TParams;
  protected
    function GetDataSet: TDataSet; override;
    function GetParams: TParams; override;
    function GetStatement: string; override;
    procedure SetParams(Value: TParams); override;
    procedure SetStatement(const Value: string); override;
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
    property ParamsObject: TParams read GetParamsObject;
    property Query: TADOQuery read GetQuery;
  public
    destructor Destroy; override;
    property Connector: TInstantADOConnector read GetConnector;
  end;

  TInstantADOLinkResolver = class(TInstantNavigationalLinkResolver)
  private
    function GetBroker: TInstantADOBroker;
    function GetDataSet: TADODataSet;
    function GetResolver: TInstantADOResolver;
  protected
    function CreateDataSet: TDataSet; override;
    procedure SetDatasetParentRange(const AParentClass, AParentId: string);
        override;
  public
    constructor Create(AResolver: TInstantNavigationalResolver; const ATableName:
        string);
    property Broker: TInstantADOBroker read GetBroker;
    property DataSet: TADODataSet read GetDataSet;
    property Resolver: TInstantADOResolver read GetResolver;
  end;

  { MS Jet }

  TInstantADOMSJetBroker = class(TInstantADOBroker)
  end;

  TInstantADOMSJetResolver = class(TInstantADOResolver)
  protected
    procedure Cancel; override;
    procedure Close; override;
    function CreateDataSet: TDataSet; override;
    function TranslateError(AObject: TInstantObject;
      E: Exception): Exception; override;
  end;

  { MS SQL Server }

  TInstantADOMSSQLGenerator = class(TInstantSQLGenerator)
  protected
    function InternalGenerateAlterFieldSQL(OldMetadata,  NewMetadata: TInstantFieldMetadata): string; override;
    function InternalGenerateDropFieldSQL(Metadata: TInstantFieldMetadata): string; override;
    function InternalGenerateDropIndexSQL(Metadata: TInstantIndexMetadata): string; override;
    function EmbraceIndex(const IndexName: string): string; virtual;
  end;

  TInstantADOMSSQLBroker = class(TInstantSQLBroker)
  protected
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetSQLQuote: Char; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure AssignDataSetParams(DataSet : TDataSet; AParams: TParams;
      OnAssignParamValue: TAssignParamValue = nil); override;
  public
    function CreateDBBuildCommand(
      const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; override;
    function CreateDataSet(const Statement: string; Params: TParams;
      OnAssignParamValue: TAssignParamValue = nil): TDataSet; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams;
      OnAssignParamValue: TAssignParamValue = nil): Integer; override;
    class function GeneratorClass: TInstantSQLGeneratorClass; override;
  end;

  TInstantADOMSSQLResolver = class(TInstantSQLResolver)
  end;

  TInstantADOMSSQLQuery = class(TInstantSQLQuery)
  end;

implementation

uses
  ADOInt, ComObj, InstantConsts, InstantUtils, InstantADOX,
  InstantADOConnectionDefEdit, InstantADOTools, Controls, InstantDBBuild,
  InstantMSSqlCatalog;

const
  ADOLinkPrefix = 'FILE NAME=';

procedure AssignParamsToParameters(Params: TParams; Parameters: TParameters;
  OnAssignParamValue: TAssignParamValue = nil);
var
  I: Integer;
  Parameter: TParameter;
  Param: TParam;
begin
  if Assigned(Params) and Assigned(Parameters) then
  begin
    if Parameters.Count = 0 then
      Parameters.Assign(Params)
    else
    begin
      with Parameters do
      for I := 0 to Pred(Count) do
      begin
        Parameter := Items[I];
        Param := Params.ParamByName(Parameter.Name);
        if Assigned(OnAssignParamValue) then
          OnAssignParamValue(Param);
        Parameter.DataType := Param.DataType;
        if Param.ParamType = DB.ptUnknown then
          Parameter.Direction := pdInput
        else
          Parameter.Direction := TParameterDirection(Param.ParamType);
        Parameter.Attributes := [];
        Parameter.NumericScale := 0;
        Parameter.Precision := 0;
        Parameter.Size := 0;
        Parameter.Value := Param.Value;
      end;
    end;
  end;
end;

{ TInstantADOConnectionDef }

class function TInstantADOConnectionDef.ConnectionTypeName: string;
begin
  Result := 'ADO';
end;

class function TInstantADOConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantADOConnector;
end;

function TInstantADOConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
var
  Connection: TADOConnection;
begin
  Connection := TADOConnection.Create(AOwner);
  try
    Connection.ConnectionString := ConnectionString;
  except
    Connection.Free;
    raise;
  end;
  Result := Connection;
end;

function TInstantADOConnectionDef.Edit: Boolean;
begin
  with TInstantADOConnectionDefEditForm.CreateForConnectionDef(nil, Self) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

function TInstantADOConnectionDef.GetLinkFileName: string;
begin
  if SameText(Copy(FConnectionString, 1, Length(ADOLinkPrefix)),
    ADOLinkPrefix) then
    Result := Copy(FConnectionString, Length(ADOLinkPrefix) + 1,
      Length(FConnectionString))
  else
    Result := '';
end;

procedure TInstantADOConnectionDef.SetLinkFileName(const Value: string);
begin
  FConnectionString := ADOLinkPrefix + Value;
end;

{ TInstantADOConnector }

procedure TInstantADOConnector.BuildDatabase(Scheme: TInstantScheme;
  BuildMethod: TInstantADOBuildMethod);
begin
  CreateDatabase;
  Connect;
  DoBuildDatabase(Scheme, BuildMethod);
end;

procedure TInstantADOConnector.BuildDatabaseADOX(Scheme: TInstantScheme);

  procedure RemoveTable(Catalog: _Catalog; const TableName: string);
  var
    I: Integer;
    Table: _Table;
  begin
    with Catalog do
      for I := 0 to Pred(Tables.Count) do
      begin
        Table := Tables[I];
        if SameText(Table.Name, TableName) then
        begin
          Tables.Delete(TableName);
          Break;
        end;
      end;
  end;

  procedure AddColumn(Table: _Table; FieldMetadata: TInstantFieldMetadata);
  const
    ColumnTypes: array[TInstantDataType, TInstantADOProviderType] of Integer = (
      {Unknown,         Jet,             SQL,             Oracle         MySQL            DB2}
      (adInteger,       adInteger,       adInteger,       adNumeric,     adInteger,       adInteger),       // dtInteger
      (adDouble,        adDouble,        adDouble,        adDouble,      adDouble,        adDouble),        // dtFloat
      (adCurrency,      adCurrency,      adCurrency,      adCurrency,    adCurrency,      adCurrency),      // dtCurrency
      (adBoolean,       adBoolean,       adBoolean,       adChar,        adBoolean,       adBoolean),       // dtBoolean
      (adVarChar,       adVarWChar,      adVarChar,       adVarChar,     adVarChar,       adVarChar),       // dtString
      (adLongVarChar,   adLongVarWChar,  adLongVarChar,   adVarBinary,   adLongVarChar,   adLongVarChar),   // dtMemo
      (adDate,          adDate,          adDBTimeStamp,   adDBTimeStamp, adDate,          adDate),          // dtDateTime
      (adLongVarBinary, adLongVarBinary, adLongVarBinary, adVarBinary,   adLongVarBinary, adLongVarBinary), // dtBlob
      (adDate,          adDate,          adDBTimeStamp,   adDBTimeStamp, adDate,          adDate),          // dtDate
      (adDate,          adDate,          adDBTimeStamp,   adDBTimeStamp, adDate,          adDate),          // dtTime
      (adInteger,       adInteger,       adInteger,       adNumeric,     adInteger,       adInteger)        // dtEnum
    );
  var
    Column: _Column;
    ColumnType: Integer;
  begin
    Column := CoColumn.Create;
    ColumnType := ColumnTypes[FieldMetadata.DataType, ProviderType];
    Column.ParentCatalog := Table.ParentCatalog;
    Column.Name := FieldMetadata.Name;
    Column.Type_ := ColumnType;
    Column.DefinedSize := FieldMetadata.Size;

    { All types except Boolean: Not Required = Nullable }
    if not (foRequired in FieldMetadata.Options) and
      (FieldMetadata.DataType <> dtBoolean) then
      Column.Attributes := Column.Attributes or adColNullable;

    case ProviderType of
      ptOracle:
        { ORACLE Boolean = CHAR(1) }
        if FieldMetadata.DataType = dtBoolean then
          Column.DefinedSize := 1;
    end;

    Table.Columns.Append(Column, Column.Type_, Column.DefinedSize);
  end;

  procedure AddIndex(Table: _Table; IndexMetadata: TInstantIndexMetadata);
  var
    Index: _Index;
    Column: _Column;
    IndexName, ColumnName: string;
    FieldList: TStringList;
    I: Integer;
  begin
    IndexName := IndexMetadata.Name;
    if IndexName = '' then
      IndexName := Table.Name + '_PRIMARY';
    Index := CoIndex.Create;
    Index.Name := IndexName;
    Index.PrimaryKey := ixPrimary in IndexMetadata.Options;
    Index.Unique := ixUnique in IndexMetadata.Options;
    FieldList := TStringList.Create;
    try
      InstantStrToList(IndexMetadata.Fields, FieldList, [';']);
      for I := 0 to Pred(FieldList.Count) do
      begin
        ColumnName := FieldList[I];
        Column := Table.Columns[ColumnName];
        if not Assigned(Column) then
          raise Exception.Create('Column not found: ' + ColumnName);
        Index.Columns.Append(ColumnName, Column.Type_, Column.DefinedSize);
      end;
    finally
      FieldList.Free;
    end;
    Table.Indexes.Append(Index, Index.Columns);
  end;

  procedure AddTable(Catalog: _Catalog; TableMetadata: TInstantTableMetadata);
  var
    I: Integer;
    Table: _Table;
    Column: _Column;
  begin
    RemoveTable(Catalog, TableMetadata.Name);
    Table := CoTable.Create;
    Table.ParentCatalog := Catalog;
    Table.Name := TableMetadata.Name;
    with TableMetadata do
    begin
      for I := 0 to Pred(FieldMetadatas.Count) do
        AddColumn(Table, FieldMetadatas[I]);
      if not (ProviderType in [ptOracle]) then
        for I := 0 to Pred(IndexMetadatas.Count) do
          AddIndex(Table, IndexMetadatas[I]);
    end;
    Catalog.Tables.Append(Table);
    if ProviderType = ptMSJet then
      for I := 0 to Pred(Table.Columns.Count) do
      begin
        Column := Table.Columns[I];
        if (Column.Type_ in [adVarWChar]) and
          ((Column.Attributes and adColNullable) = adColNullable) then
          Column.Properties['Jet OLEDB:Allow Zero Length'].Value := True;
      end;
  end;

var
  I: Integer;
  Catalog: _Catalog;
begin
  if not Assigned(Scheme) then
    Exit;
  Catalog := CoCatalog.Create;
  Catalog.Set_ActiveConnection(Connection.ConnectionObject);
  for I := 0 to Pred(Scheme.TableMetadataCount) do
    AddTable(Catalog, Scheme.TableMetadatas[I]);
end;

procedure TInstantADOConnector.BuildDatabaseSQL(Scheme: TInstantScheme);

  procedure CreateTable(TableMetadata: TInstantTableMetadata);

    function DataTypeToColumnType(DataType: TInstantDataType;
      Size: Integer): string;
    const
      Types: array[TInstantDataType] of string = (
        'INTEGER',
        'FLOAT',
        'DECIMAL',
        'LOGICAL',
        'VARCHAR',
        'MEMO',
        'DATETIME',
        'BLOB',
        'DATE',
        'TIME',
        'INTEGER'
      );
    begin
      Result := Types[DataType];
      case ProviderType of
        ptMSJet:
          case DataType of
            dtBlob:
              Result := 'OLEOBJECT';
          end;
        ptMSSQLServer:
          case DataType of
            dtBoolean:
              Result := 'BIT';
            dtCurrency:
              Result := 'MONEY';
            dtMemo:
              Result := 'TEXT';
            dtBlob:
              Result := 'IMAGE';
            dtDate, dtDateTime:
              Result := 'DATETIME';
          end;
        ptOracle:
          case DataType of
            dtBoolean:
              Result := 'CHAR(1)';
            dtCurrency:
              Result := 'DECIMAL(14,4)';
            dtDateTime, dtDate, dtTime:
              Result := 'DATE';
            dtBlob:
              Result := 'BLOB';
            dtMemo:
              Result := 'CLOB';
          end;
        ptIBMDB2:
          case DataType of
            dtCurrency:
              Result := 'DECIMAL(14,4)';
            dtDateTime, dtDate, dtTime:
              Result := 'TIMESTAMP';
            dtBlob:
              Result := 'BLOB (1000 K)';
            dtMemo:
              Result := 'CLOB';
          end;
      end;
      if (DataType in [dtString, dtMemo]) and (Size > 0) then
        Result := Result + '(' + IntToStr(Size) + ')';
    end;

    function Embrace(Symbol: string): string;
    begin
      case ProviderType of
        ptMSJet, ptMSSQLServer:
          Result := Format('[%s]', [Symbol]);
        ptOracle:
          Result := Symbol;
      else
        Result := Symbol;
      end;
    end;

  var
    I: Integer;
    Columns, PrimaryKey, IndexName, IndexColumns, Modifier, S: string;
    FieldMetadata: TInstantFieldMetadata;
    IndexMetadata: TInstantIndexMetadata;
  begin
    with TADOCommand.Create(nil) do
    try
      Connection := Self.Connection;
      CommandType := cmdText;

      { Drop existing table }
      CommandText := Format('DROP TABLE %s', [Embrace(TableMetadata.Name)]);
      try
        Execute;
      except
      end;

      Columns := '';

      { Define columns }
      with TableMetadata do
        for I := 0 to Pred(FieldMetadatas.Count) do
        begin
          FieldMetadata := FieldMetadatas[I];
          if I > 0 then
            Columns := Columns + ', ';
          with FieldMetadata do
          begin
            Columns := Columns + Embrace(Name) + ' ' +
              DataTypeToColumnType(DataType, Size);
            if foRequired in Options then
              Columns := Columns + ' NOT NULL';
          end;
        end;

      { Define primary key }
      PrimaryKey := '';
      with TableMetadata do
        for I := 0 to Pred(IndexMetadatas.Count) do
          with IndexMetadatas[I] do
            if ixPrimary in Options then
            begin
              S := Format('%s', [StringReplace(Fields, ';', ',',
                [rfReplaceAll])]);
              if PrimaryKey <> '' then
                PrimaryKey := PrimaryKey + ',';
              PrimaryKey := PrimaryKey + S;
            end;
      if PrimaryKey <> '' then
        Columns := Columns + ', PRIMARY KEY (' + PrimaryKey + ')';

      { Create new table }
      CommandText := Format('CREATE TABLE %s (%s)',
        [Embrace(TableMetadata.Name), Columns]);
      Execute;

      { Create indices }
      with TableMetadata do
        for I := 0 to Pred(IndexMetadatas.Count) do
        begin
          IndexMetadata := IndexMetadatas[I];
          if not (ixPrimary in IndexMetadata.Options) then
          begin
            IndexName := IndexMetadata.Name;
            if ixUnique in IndexMetadata.Options then
              Modifier := 'UNIQUE ' else
              Modifier := '';
            IndexColumns := StringReplace(IndexMetadata.Fields, ';', ',',
              [rfReplaceAll]);
            CommandText := Format('CREATE %sINDEX %s ON %s (%s)',
              [Modifier, IndexName, TableMetadata.Name, IndexColumns]);
            Execute;
          end;
        end;

    finally
      Free;
    end;
  end;

var
  I: Integer;
begin
  if not Assigned(Scheme) then
    Exit;
  with Scheme do
    for I := 0 to Pred(TableMetadataCount) do
      CreateTable(TableMetadatas[I]);
end;

procedure TInstantADOConnector.CompactDatabase;
begin
  if ProviderType = ptMSJet then
    InstantADOCompactDatabase(Connection);
end;

class function TInstantADOConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantADOConnectionDef;
end;

function TInstantADOConnector.CreateBroker: TInstantBroker;
begin
  case ProviderType of
    ptMSJet:
      Result := TInstantADOMSJetBroker.Create(Self);
    ptMSSQLServer:
      Result := TInstantADOMSSQLBroker.Create(Self);
  else
    Result := TInstantADOBroker.Create(Self);
  end;
end;

procedure TInstantADOConnector.DoBuildDatabase(Scheme: TInstantScheme;
  BuildMethod: TInstantADOBuildMethod);
begin
  case BuildMethod of
    bmDefault:
      if ProviderType in [ptMSJet, ptMSSQLServer] then
        BuildDatabaseADOX(Scheme)
      else
        BuildDatabaseSQL(Scheme);
    bmADOX:
      BuildDatabaseADOX(Scheme);
    bmSQL:
      BuildDatabaseSQL(Scheme);
  end;
end;

function TInstantADOConnector.GetCanTransaction: Boolean;
begin
  Result := not (ProviderType in [ptMySQL]);
end;

function TInstantADOConnector.GetConnection: TADOConnection;
begin
  Result := inherited Connection as TADOConnection;
end;

function TInstantADOConnector.GetDatabaseExists: Boolean;
begin
  if ProviderType = ptMSJet then
    Result := FileExists(DatabaseName)
  else
    Result := inherited GetDatabaseExists;
end;

function TInstantADOConnector.GetDatabaseName: string;
begin
  case ProviderType of
    ptMSJet:
      Result := Connection.Properties['Data Source'].Value;
    ptMSSQLServer:
      Result := Connection.Properties['Initial Catalog'].Value;
  else
    Result := Connection.ConnectionString;
  end;
end;

function TInstantADOConnector.GetDBMSName: string;
var
  WasConnected: Boolean;
begin
  WasConnected := Connected;
  if not Connected then
    Connect;
  try
    Result := Connection.Properties['DBMS Name'].Value;
  finally
    if not WasConnected then
      Disconnect;
  end;
end;

function TInstantADOConnector.GetProviderType: TInstantADOProviderType;

  function HasPrefix(const Prefix: string): Boolean;
  begin
    Result := SameText(Copy(Connection.Provider, 1, Length(Prefix)), Prefix);
  end;

const
  MSJetPrefix = 'Microsoft.Jet';
  MSSQLPrefix = 'SQLOLEDB';
  MSSQL2005Prefix = 'SQLNCLI';
  OraclePrefix = 'ORA';
  MySQLPrefix = 'My';
  IBMDB2Prefix = 'IBMDADB2';
  MSOraclePrefix = 'MSDAORA';
begin
  if HasConnection then
  begin
    if HasPrefix(MSJetPrefix) then
      Result := ptMSJet
    else if HasPrefix(MSSQLPrefix) or HasPrefix(MSSQL2005Prefix) then
      Result := ptMSSQLServer
    else if HasPrefix(OraclePrefix) or HasPrefix(MSOraclePrefix) then
      Result := ptOracle
    else if HasPrefix(MySQLPrefix) then
      Result := ptMySQL
    else if HasPrefix(IBMDB2Prefix) then
      Result := ptIBMDB2
    else
      Result := ptUnknown
  end else
    Result := ptUnknown;
end;

procedure TInstantADOConnector.InternalBuildDatabase(
  Scheme: TInstantScheme);
begin
  inherited;
  DoBuildDatabase(Scheme, bmDefault);
end;

procedure TInstantADOConnector.InternalCommitTransaction;
begin
  if CanTransaction then
    Connection.CommitTrans;
end;

procedure TInstantADOConnector.InternalCreateDatabase;
var
  Catalog: _Catalog;
begin
  if ProviderType = ptMSJet then
  begin
    Catalog := CoCatalog.Create;
    Catalog.Create(Format('Provider=%s;Data Source=%s',
      [Connection.Provider, DatabaseName]));
  end else
    inherited;
end;

procedure TInstantADOConnector.InternalRollbackTransaction;
begin
  if CanTransaction then
    Connection.RollbackTrans;
end;

procedure TInstantADOConnector.InternalStartTransaction;
begin
  if CanTransaction then
    Connection.BeginTrans;
end;

procedure TInstantADOConnector.SetConnection(const Value: TADOConnection);
begin
  inherited Connection := Value;
end;

{ TInstantADOBroker }

function TInstantADOBroker.CreateResolver(
  const TableName: string): TInstantResolver;
begin
  Result := TInstantADOResolver.Create(Self, TableName);
end;

function TInstantADOBroker.GetConnector: TInstantADOConnector;
begin
  Result := inherited Connector as TInstantADOConnector;
end;

function TInstantADOBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantADOQuery.Create(Connector);
end;

{ TInstantADOResolver }

function TInstantADOResolver.CreateDataSet: TDataSet;
begin
  Result:= TADODataSet.Create(nil);
  with TADODataSet(Result) do
  try
    Connection := Broker.Connector.Connection;
    CommandType := cmdTable;
    CommandText := TableName;
  except
    Result.Free;
    raise;
  end;
end;

function TInstantADOResolver.CreateNavigationalLinkResolver(
  const ATableName: string): TInstantNavigationalLinkResolver;
begin
  Result := TInstantADOLinkResolver.Create(Self, ATableName);
end;

function TInstantADOResolver.FormatTableName(
  const ATableName: string): string;
begin
  Result := TableName;
end;

function TInstantADOResolver.GetBroker: TInstantADOBroker;
begin
  Result := inherited Broker as TInstantADOBroker;
end;

function TInstantADOResolver.GetDataSet: TCustomADODataSet;
begin
  Result := inherited DataSet as TCustomADODataSet;
end;

function TInstantADOResolver.Locate(const AClassName,
  AObjectId: string): Boolean;
var
  KeyVals: Variant;
begin
  KeyVals := CreateLocateVarArray(AClassName, AObjectId);
  with DataSet do
    if Supports([coSeek]) then
      Result := Seek(KeyVals)
    else
      Result := Locate(InstantIndexFieldNames, KeyVals, []);
end;

{ TInstantADOTranslator }

function TInstantADOTranslator.GetDelimiters: string;
begin
  case Query.Connector.ProviderType of
    ptMSJet:
      Result := '[]';
  else
    Result := inherited GetDelimiters;
  end;
end;

function TInstantADOTranslator.GetQuery: TInstantADOQuery;
begin
  Result := inherited Query as TInstantADOQuery;
end;

function TInstantADOTranslator.GetQuote: Char;                
begin
  if Query.Connector.ProviderType in [ptMSSQLServer, ptOracle, ptIBMDB2] then
    Result := ''''
  else
    Result := inherited GetQuote;
end;

function TInstantADOTranslator.TranslateFunctionName(
  const FunctionName: string; Writer: TInstantIQLWriter): Boolean;
begin
  Result := True;
  case Query.Connector.ProviderType of
    ptMSJet:
      if SameText(FunctionName, 'UPPER') then
        Writer.WriteString('UCASE')
      else if SameText(FunctionName, 'LOWER') then
        Writer.WriteString('LCASE')
      else if SameText(FunctionName, 'SUBSTRING') then
        Writer.WriteString('MID')
      else
        Result := False;
    ptOracle:
      if SameText(FunctionName, 'SUBSTRING') then
        Writer.WriteString('SUBSTR')
      else
        Result := False;
  else
    Result := False;
  end;
end;

{ TInstantADOQuery }

destructor TInstantADOQuery.Destroy;
begin
  FQuery.Free;
  FParamsObject.Free;
  inherited;
end;

function TInstantADOQuery.GetConnector: TInstantADOConnector;
begin
  Result := inherited Connector as TInstantADOConnector;
end;

function TInstantADOQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TInstantADOQuery.GetParams: TParams;
begin
  Result := ParamsObject;
end;

function TInstantADOQuery.GetParamsObject: TParams;
begin
  if not Assigned(FParamsObject) then
    FParamsObject := TParams.Create(Self);
  Result := FParamsObject;
end;

function TInstantADOQuery.GetQuery: TADOQuery;
begin
  if not Assigned(FQuery) then
  begin
    FQuery := TADOQuery.Create(nil);
    if Connector.HasConnection then
      FQuery.Connection := Connector.Connection as TADOConnection;
  end;
  Result := FQuery;
end;

function TInstantADOQuery.GetStatement: string;
begin
  Result := Query.SQL.Text;
end;

procedure TInstantADOQuery.SetParams(Value: TParams);
begin
  ParamsObject.Assign(Value);
  AssignParamsToParameters(ParamsObject, Query.Parameters);
end;

procedure TInstantADOQuery.SetStatement(const Value: string);
begin
  Query.SQL.Text := Value;
end;

class function TInstantADOQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantADOTranslator;
end;

{ TInstantADOMSJetResolver }

procedure TInstantADOMSJetResolver.Cancel;
begin
  inherited;
  { Avoid zombie state of RecordSet with transactions }
  Close;
end;

procedure TInstantADOMSJetResolver.Close;
begin
  inherited;
  { Avoid sort/filter interface error when reopening a serverside cursor }
  if DataSet is TADODataSet then
    TADODataSet(DataSet).IndexFieldNames := '';
end;

function TInstantADOMSJetResolver.CreateDataSet: TDataSet;
begin
  Result:= TADODataSet.Create(nil);
  with TADODataSet(Result) do
  try
    Connection := Broker.Connector.Connection;
    CommandType := cmdTableDirect;
    CommandText := TableName;
    CursorType := ctKeyset;
    CursorLocation := clUseServer;
    LockType := ltOptimistic;
    IndexName := IndexDefs.FindIndexForFields(InstantIndexFieldNames).Name;
  except
    Result.Free;
    raise ;
  end;
end;

function TInstantADOMSJetResolver.TranslateError(AObject: TInstantObject;
  E: Exception): Exception;
var
  ErrorCodeStr: string;
  ErrorCode: Integer;
begin
  Result := nil;
  if (E is EDatabaseError) and Assigned(Broker.Connector.Connection.Errors) then
    with Broker.Connector.Connection.Errors do
    begin
      if Count > 0 then
      begin
        try
          ErrorCodeStr := Item[0].SQLState;
          ErrorCode := StrToInt(ErrorCodeStr);
        except
          ErrorCode := 0;
        end;
        case ErrorCode of
          3022: Result := KeyViolation(AObject, AObject.Id, E);
        end;
      end;
    end;
end;

{ TInstantADOMSSQLGenerator }

function TInstantADOMSSQLGenerator.EmbraceIndex(
  const IndexName: string): string;
begin
  Result := InstantEmbrace(IndexName, Delimiters);
end;

function TInstantADOMSSQLGenerator.InternalGenerateAlterFieldSQL(OldMetadata,
  NewMetadata: TInstantFieldMetadata): string;
begin
  Result := Format('ALTER TABLE %s ALTER COLUMN %s %s',
    [EmbraceTable(OldMetadata.TableMetadata.Name),
     EmbraceField(OldMetadata.Name),
     Broker.DataTypeToColumnType(NewMetadata.DataType, NewMetadata.Size)]);
end;

function TInstantADOMSSQLGenerator.InternalGenerateDropFieldSQL(
  Metadata: TInstantFieldMetadata): string;
begin
  Result := Format('ALTER TABLE %s DROP COLUMN %s',
    [EmbraceTable(Metadata.TableMetadata.Name),
     EmbraceField(Metadata.Name)]);
end;

function TInstantADOMSSQLGenerator.InternalGenerateDropIndexSQL(
  Metadata: TInstantIndexMetadata): string;
begin
  Result := Format('DROP INDEX %s.%s',
          [EmbraceTable(Metadata.TableMetadata.Name),
           EmbraceIndex(Metadata.Name)]);
end;

{ TInstantADOMSSQLBroker }

class function TInstantADOMSSQLBroker.GeneratorClass: TInstantSQLGeneratorClass;
begin
  Result := TInstantADOMSSQLGenerator;
end;

procedure TInstantADOMSSQLBroker.AssignDataSetParams(DataSet: TDataSet;
  AParams: TParams; OnAssignParamValue: TAssignParamValue);
begin
  AssignParamsToParameters(AParams, TADOQuery(DataSet).Parameters,
    OnAssignParamValue);
end;

function TInstantADOMSSQLBroker.CreateCatalog(
  const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := TInstantMSSqlCatalog.Create(AScheme, Self);
end;

function TInstantADOMSSQLBroker.CreateDataSet(const Statement: string;
  Params: TParams; OnAssignParamValue: TAssignParamValue = nil): TDataSet;
var
  Query: TADOQuery;
begin
  Query := TADOQuery.Create(nil);
  Try
    Query.CursorType := ctOpenForwardOnly;
    Query.LockType := ltReadOnly;
    Query.Connection := (Connector as TInstantADOConnector).Connection;
    Query.SQL.Text := Statement;
    if Assigned(Params) then
      AssignParamsToParameters(Params, Query.Parameters, OnAssignParamValue);
    Result := Query;
  Except
    Query.Free;
    raise;
  End;    
end;

function TInstantADOMSSQLBroker.CreateDBBuildCommand(
  const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  if CommandType = ctAddTable then
    Result := TInstantDBBuildAddTableSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctDropTable then
    Result := TInstantDBBuildDropTableSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctAddField then
    Result := TInstantDBBuildAddFieldSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctAlterField then
    Result := TInstantDBBuildAlterFieldSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctDropField then
    Result := TInstantDBBuildDropFieldSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctAddIndex then
    Result := TInstantDBBuildAddIndexSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctDropIndex then
    Result := TInstantDBBuildDropIndexSQLCommand.Create(CommandType, Connector)
  else
    Result := inherited CreateDBBuildCommand(CommandType);
end;

function TInstantADOMSSQLBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantADOMSSQLResolver.Create(Self, Map);
end;

function TInstantADOMSSQLBroker.DataTypeToColumnType(
  DataType: TInstantDataType; Size: Integer): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'MONEY',
    'BIT',
    'VARCHAR',
    'TEXT',
    'DATETIME',
    'IMAGE',
    'DATETIME',
    'DATETIME',
    'INTEGER');
begin
  Result := Types[DataType];
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantADOMSSQLBroker.Execute(const AStatement: string;
  AParams: TParams; OnAssignParamValue: TAssignParamValue): Integer;
var
  DataSet : TADOQuery;
begin
  DataSet := AcquireDataSet(AStatement, AParams, OnAssignParamValue) as TADOQuery;
  try
    Result := DataSet.ExecSQL;
  finally
    ReleaseDataSet(DataSet);
  end;
end;

function TInstantADOMSSQLBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

function TInstantADOMSSQLBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantADOMSSQLQuery.Create(Connector);
end;

{ TInstantADOMSSQLQuery }

{ TInstantADOLinkResolver }

constructor TInstantADOLinkResolver.Create(
  AResolver: TInstantNavigationalResolver; const ATableName: string);
begin
  inherited Create(AResolver, ATableName);
end;

function TInstantADOLinkResolver.CreateDataSet: TDataSet;
begin
  Result:= TADOTable.Create(nil);
  with TADOTable(Result) do
  try
    Connection := Broker.Connector.Connection;
    TableName := Self.TableName;
    IndexFieldNames := InstantParentClassFieldName + ';' +
        InstantParentIdFieldName;
  except
    Result.Free;
    raise;
  end;
end;

function TInstantADOLinkResolver.GetBroker: TInstantADOBroker;
begin
  Result := inherited Broker as TInstantADOBroker;
end;

function TInstantADOLinkResolver.GetDataSet: TADODataSet;
begin
  Result := inherited DataSet as TADODataSet;
end;

function TInstantADOLinkResolver.GetResolver: TInstantADOResolver;
begin
  Result := inherited Resolver as TInstantADOResolver;
end;

procedure TInstantADOLinkResolver.SetDatasetParentRange(const AParentClass,
  AParentId: string);
begin
//    Dataset.SetRange([AParentClass, AParentId], [AParentClass, AParentId]);
end;

initialization
  RegisterClass(TInstantADOConnectionDef);
  TInstantADOConnector.RegisterClass;

finalization
  TInstantADOConnector.UnregisterClass;

end.
