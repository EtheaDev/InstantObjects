(*
 *   InstantObjects
 *   Zeos Database Objects Support
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
 * The Original Code is: InstantObjects ZeosDBO Support
 *
 * The Initial Developer of the Original Code is: Joao Morais
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Enrique Esquivel, David Taylor
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantZeosDBO;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

//
// Enable this define for ZeosDBO 7.x Alpha support
//
// WARNING: The broker will no longer compile agaist ZeosDBO v6.6.x.
// Zeos v7 support is experimental and has seen very little testing.
//
{.$DEFINE ZEOSDBO_V7+}


// Supported databases

{$DEFINE SYBASE_SUPPORT}
{$DEFINE MSSQL_SUPPORT}
{$DEFINE IBFB_SUPPORT}
{$DEFINE ORACLE_SUPPORT}
{$DEFINE PGSQL_SUPPORT}
{$DEFINE MYSQL_SUPPORT}
{$DEFINE SQLITE_SUPPORT}

interface

uses
  Classes, Db, InstantPersistence, InstantCommand, InstantDBBuild, InstantBrokers,
  InstantMetadata, InstantTypes, ZConnection
  {$IFDEF D10+}, DBCommonTypes{$ENDIF};

type
  // Login event signature changed in v7 Alpha
  {$IFDEF ZEOSDBO_V7+}
    TZeosLoginEvent = TZLoginEvent;
  {$ELSE}
    TZeosLoginEvent = TLoginEvent;
  {$ENDIF}

  TInstantZeosDBOConnectionDef = class(TInstantRelationalConnectionDef)
  private
    FCatalog: string;
    FDatabase: string;
    FHostName: string;
    FLoginPrompt: Boolean;
    FPassword: string;
    FPort: Integer;
    FProperties: string;
    FProtocol: string;
    FUseDelimitedIdents: Boolean;
    FUserName: string;
  protected
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property Catalog: string read FCatalog write FCatalog;
    property Database: string read FDatabase write FDatabase;
    property HostName: string read FHostName write FHostName;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
    property Password: string read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property Properties: string read FProperties write FProperties;
    property Protocol: string read FProtocol write FProtocol;
    property UseDelimitedIdents: Boolean read FUseDelimitedIdents write FUseDelimitedIdents;
    property UserName: string read FUserName write FUserName;
  end;

  TInstantZeosDBOBroker = class;

  TInstantZeosDBOConnector = class(TInstantRelationalConnector)
  private
    FConnection: TZConnection;
    FLoginPrompt: Boolean;
    FOnLogin: TZeosLoginEvent;
    FUseDelimitedIdents: Boolean;
    procedure DoAfterConnectionChange;
    procedure DoBeforeConnectionChange;
    function GetBroker: TInstantZeosDBOBroker;
    function GetConnection: TZConnection;
    function GetLoginPrompt: Boolean;
    procedure SetConnection(Value: TZConnection);
    procedure SetLoginPrompt(const Value: Boolean);
    procedure SetUseDelimitedIdents(const Value: Boolean);
  protected
    procedure AfterConnectionChange; virtual;
    procedure BeforeConnectionChange; virtual;
    procedure AssignLoginOptions; virtual;
    procedure CheckConnection;
    function CreateBroker: TInstantBroker; override;
    function GetConnected: Boolean; override;
    function GetDatabaseExists: Boolean; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalConnect; override;
    procedure InternalCreateDatabase; override;
    procedure InternalDisconnect; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ParamByName(const AName: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    function HasConnection: Boolean;
    property Broker: TInstantZeosDBOBroker read GetBroker;
  published
    property Connection: TZConnection read GetConnection write SetConnection;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt default False;
    property OnLogin: TZeosLoginEvent read FOnLogin write FOnLogin;
    property UseDelimitedIdents: Boolean read FUseDelimitedIdents write SetUseDelimitedIdents default False;
  end;

  TInstantZeosDBOBroker = class(TInstantSQLBroker)
  private
    function GetConnector: TInstantZeosDBOConnector;
  protected
    procedure AssignDataSetParams(DataSet: TDataSet; AParams: TParams); override;
    procedure AssignParam(SourceParam, TargetParam: TParam); virtual;
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    function GetSQLDelimiters: string; override;
    function GetSQLQuote: Char; override;
    procedure InternalCreateDatabase; virtual;
    function InternalCreateQuery: TInstantQuery; override;
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; virtual; abstract;
    function InternalDBNotExistsErrorCode: Integer; virtual;
    function InternalSQLDelimiter: string; virtual;
    function UseBooleanFields: Boolean; virtual; abstract;
  public
    procedure CreateDatabase;
    function CreateDataSet(const AStatement: string; AParams: TParams = nil): TDataSet; override;
    function CreateDBBuildCommand(const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams = nil): Integer; override;
    function DBNotExistsErrorCode: Integer;
    property Connector: TInstantZeosDBOConnector read GetConnector;
  end;

  TInstantZeosDBOResolver = class(TInstantSQLResolver)
  // Read an integer field and convert it to boolean expression
  protected
    function ReadBooleanField(DataSet: TDataSet; const FieldName: string): Boolean; override;
  end;

  TInstantZeosDBOTranslator = class(TInstantRelationalTranslator)
  // Translate boolean expressions to '0' or '1'
  protected
    function TranslateConstant(Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean; override;
  end;

  TInstantZeosDBOQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

  { Sybase broker }

  {$IFDEF SYBASE_SUPPORT}
  TInstantZeosDBOSybaseBroker = class(TInstantZeosDBOBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { MS SQL Server broker }

  {$IFDEF MSSQL_SUPPORT}
  TInstantZeosDBOMSSQLBroker = class(TInstantZeosDBOBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { Interbase and Firebird brokers }

  {$IFDEF IBFB_SUPPORT}
  TInstantZeosDBOIbFbBroker = class(TInstantZeosDBOBroker)
  protected
    procedure InternalCreateDatabase; override;
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function InternalDBNotExistsErrorCode: Integer; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { Oracle broker }

  {$IFDEF ORACLE_SUPPORT}
  TInstantZeosDBOOracleBroker = class(TInstantZeosDBOBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { PostgreSQL broker }

  {$IFDEF PGSQL_SUPPORT}
  TInstantZeosDBOPgSQLBroker = class(TInstantZeosDBOBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { MySQL broker }

  {$IFDEF MYSQL_SUPPORT}
  TInstantMySQLGenerator = class(TInstantSQLGenerator)
  protected
    function InternalGenerateDropIndexSQL(Metadata: TInstantIndexMetadata): string; override;
  end;

  TInstantZeosDBOMySQLBroker = class(TInstantZeosDBOBroker)
  protected
    procedure InternalCreateDatabase; override;
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string;
      override;
    function InternalDBNotExistsErrorCode: Integer; override;
    function InternalSQLDelimiter: string; override;
    function UseBooleanFields: Boolean; override;
  public
    class function GeneratorClass: TInstantSQLGeneratorClass; override;
  end;
  {$ENDIF}

  { SQLite broker }

  {$IFDEF SQLITE_SUPPORT}

  //SQLite doesn´t support ALTER TABLE for supports ADD COLUMN, ALTER COLUMN and
  //DROP COLUMN, is emulated with a couple of CREATE TEMP TABLE, INSERT INTO,
  //DROP TABLE, CREATE TABLE, INSERT INTO and finally DROP TABLE
  TInstantDBBuildSQLiteAlterTableSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    FTmpTableMD: TInstantTableMetadata;
    FOldTableMetadata: TInstantTableMetadata;
    FNewTableMetadata: TInstantTableMetadata;
    FScheme : TInstantScheme;
    function GetNewTableMetadata: TInstantTableMetadata;
    function GetOldTableMetadata: TInstantTableMetadata;
  protected
    procedure Rollback; virtual;
    function GetDescription: string; override;
    function GetSQLStatement(const Index: Integer): string; override;
    function GetSQLStatementCount: Integer; override;
    procedure InternalExecute; override;
  public
    destructor Destroy; override;
    property OldTableMetadata: TInstantTableMetadata read GetOldTableMetadata;
    property NewTableMetadata: TInstantTableMetadata read GetNewTableMetadata;
  end;

  TInstantSQLiteGenerator = class(TInstantSQLGenerator)
  protected
    function InternalGenerateInsertFromSelectSQL(const SourceMetadata, TargetMetadata: TInstantTableMetadata): string; virtual;
    function InternalGenerateCreateTempTableSQL(Metadata: TInstantTableMetadata): string; virtual;
  public
    function GenerateCreateTempTableSQL(Metadata: TInstantTableMetadata): string;
    function GenerateInsertFromSelectSQL(const SourceMetadata, TargetMetadata: TInstantTableMetadata): string;
  end;

  TInstantZeosDBOSQLiteBroker = class(TInstantZeosDBOBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  public
    class function GeneratorClass: TInstantSQLGeneratorClass; override;
    function CreateDBBuildCommand(const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; override;
  end;
  {$ENDIF}

procedure AssignZeosDBOProtocols(Strings: TStrings);
procedure AssignZeosDBOCatalogs(Strings: TStrings; const AProtocol, AHostName,
 APort, AUser, APassword: string; AProperties: TStrings);

implementation

uses
  SysUtils, {$IFDEF D7+}Types,{$ENDIF} Controls, {$IFDEF D5}DBLogDlg,{$ENDIF}
  InstantConsts, InstantClasses, InstantZeosDBOConnectionDefEdit,
  InstantZeosDBOCatalog, InstantUtils, ZClasses, ZCompatibility, ZDbcIntfs,
  {$IFDEF MYSQL_SUPPORT}ZDbcMySql,{$ENDIF} ZDataset;

{$IFDEF SQLITE_SUPPORT}
const
  STmpTableSuffix = '_IOTmp_';
{$ENDIF}

{ Global routines }

procedure AssignZeosDBOProtocols(Strings: TStrings);
var
  i, j: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  Strings.Clear;
  Drivers := DriverManager.GetDrivers;
  Protocols := nil;
  for i := 0 to Pred(Drivers.Count) do
  begin
    Protocols := (Drivers[i] as IZDriver).GetSupportedProtocols;
    for j := Low(Protocols) to High(Protocols) do
      Strings.Add(Protocols[j]);
  end;
end;

procedure AssignZeosDBOCatalogs(Strings: TStrings; const AProtocol, AHostName,
 APort, AUser, APassword: string; AProperties: TStrings);
var
  DbcConnection: IZConnection;
  Url: string;
begin
  Strings.Clear;
  try
    if (APort = '') or (APort = '0') then
      Url := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s',
       [AProtocol, AHostName, '', AUser, APassword])
    else
      Url := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s',
       [AProtocol, AHostName, APort, '', AUser, APassword]);

    DbcConnection := DriverManager.GetConnectionWithParams(Url, AProperties);
    with DbcConnection.GetMetadata.GetCatalogs do
    try
      while Next do
        Strings.Add(string(GetString(1)));
    finally
      Close;
    end;
  except
    // Just return an empty list whenever exception raises
  end;
end;

{ Local routines }

{$IFDEF D5}
function LoginDialogProcCompatibility(const ADatabaseName: string;
 var AUserName, APassword: string): Boolean;
begin
  Result := LoginDialogEx(ADatabaseName, AUserName, APassword, False);
end;
{$ENDIF}

{ TInstantZeosDBOConnectionDef }

class function TInstantZeosDBOConnectionDef.ConnectionTypeName: string;
begin
  Result := 'ZeosDBO';
end;

class function TInstantZeosDBOConnectionDef.ConnectorClass:
  TInstantConnectorClass;
begin
  Result := TInstantZeosDBOConnector;
end;

function TInstantZeosDBOConnectionDef.Edit: Boolean;
begin
  with TInstantZeosDBOConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

procedure TInstantZeosDBOConnectionDef.InitConnector(
  Connector: TInstantConnector);
var
  Connection: TZConnection;
begin
  inherited;
  Connection := TZConnection.Create(Connector);
  try
    (Connector as TInstantZeosDBOConnector).Connection := Connection;
    (Connector as TInstantZeosDBOConnector).LoginPrompt := LoginPrompt;
    (Connector as TInstantZeosDBOConnector).UseDelimitedIdents := UseDelimitedIdents;
    Connection.AutoCommit := False;
    Connection.Database := Database;
    Connection.HostName := HostName;
    Connection.Port := Port;
    Connection.Properties.Text := Properties;
    Connection.Protocol := Protocol;
    Connection.Catalog := Catalog;
    Connection.TransactIsolationLevel := tiReadCommitted;
    Connection.User := UserName;
    Connection.Password := Password;
  except
    Connection.Free;
    raise;
  end;
end;

{ TInstantZeosDBOConnector }

procedure TInstantZeosDBOConnector.AfterConnectionChange;
begin
  if HasConnection then
  begin
    FConnection.Connected := False;
    FConnection.AutoCommit := True;
    FConnection.TransactIsolationLevel := tiReadCommitted;
  end;
end;

procedure TInstantZeosDBOConnector.AssignLoginOptions;
begin
  if HasConnection then
  begin
    FConnection.LoginPrompt := FLoginPrompt;
    if Assigned(FOnLogin) and not Assigned(Connection.OnLogin) then
      FConnection.OnLogin := FOnLogin;
  end;
end;

procedure TInstantZeosDBOConnector.BeforeConnectionChange;
begin
end;

procedure TInstantZeosDBOConnector.CheckConnection;
begin
  if not Assigned(FConnection) then
    raise EInstantError.Create(SUnassignedConnection);
end;

class function TInstantZeosDBOConnector.ConnectionDefClass:
  TInstantConnectionDefClass;
begin
  Result := TInstantZeosDBOConnectionDef;
end;

constructor TInstantZeosDBOConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoginPrompt := False;
  FUseDelimitedIdents := False;
end;

function TInstantZeosDBOConnector.CreateBroker: TInstantBroker;
begin
  CheckConnection;
  Result := nil;

  {$IFDEF SYBASE_SUPPORT}
  if SameText(FConnection.Protocol, 'sybase') then
    Result := TInstantZeosDBOSybaseBroker.Create(Self);
  {$ENDIF}

  {$IFDEF MSSQL_SUPPORT}
  if SameText(FConnection.Protocol, 'mssql') then
    Result := TInstantZeosDBOMSSQLBroker.Create(Self);
  {$ENDIF}

  {$IFDEF IBFB_SUPPORT}
  if SameText(FConnection.Protocol, 'interbase-5') or
   SameText(FConnection.Protocol, 'interbase-6') or
   SameText(FConnection.Protocol, 'firebird-1.0') or
   SameText(FConnection.Protocol, 'firebird-1.5') or
   SameText(FConnection.Protocol, 'firebirdd-1.5') or
   SameText(FConnection.Protocol, 'firebird-2.0') or
   SameText(FConnection.Protocol, 'firebirdd-2.0') or
   SameText(FConnection.Protocol, 'firebird-2.1') or
   SameText(FConnection.Protocol, 'firebirdd-2.1') then
    Result := TInstantZeosDBOIbFbBroker.Create(Self);
  {$ENDIF}

  {$IFDEF ORACLE_SUPPORT}
  if SameText(FConnection.Protocol, 'oracle') or
   SameText(FConnection.Protocol, 'oracle-9i') then
    Result := TInstantZeosDBOOracleBroker.Create(Self);
  {$ENDIF}

  {$IFDEF PGSQL_SUPPORT}
  if SameText(FConnection.Protocol, 'postgresql') or
   SameText(FConnection.Protocol, 'postgresql-6.5') or
   SameText(FConnection.Protocol, 'postgresql-7.2') or
   SameText(FConnection.Protocol, 'postgresql-7.3') or
   SameText(FConnection.Protocol, 'postgresql-7.4') then
    Result := TInstantZeosDBOPgSQLBroker.Create(Self);
  {$ENDIF}

  {$IFDEF MYSQL_SUPPORT}
  if SameText(FConnection.Protocol, 'mysql') or
   SameText(FConnection.Protocol, 'mysql-3.20') or
   SameText(FConnection.Protocol, 'mysql-3.23') or
   SameText(FConnection.Protocol, 'mysql-4.0') or
   SameText(FConnection.Protocol, 'mysql-4.1') then
    Result := TInstantZeosDBOMySQLBroker.Create(Self);
  {$ENDIF}

  {$IFDEF SQLITE_SUPPORT}
  if SameText(FConnection.Protocol, 'sqlite') or
   SameText(FConnection.Protocol, 'sqlite-2.8') then
    Result := TInstantZeosDBOSQLiteBroker.Create(Self);
  {$ENDIF}

  if Result = nil then
    raise EInstantError.CreateFmt(SProtocolNotSupported,
     [FConnection.Protocol]);
end;

procedure TInstantZeosDBOConnector.DoAfterConnectionChange;
begin
  if HasConnection then
    FConnection.FreeNotification(Self);
  AfterConnectionChange;
end;

procedure TInstantZeosDBOConnector.DoBeforeConnectionChange;
begin
  try
    BeforeConnectionChange;
  finally
    if HasConnection then
      FConnection.RemoveFreeNotification(Self);
  end;
end;

function TInstantZeosDBOConnector.GetBroker: TInstantZeosDBOBroker;
begin
  Result := inherited Broker as TInstantZeosDBOBroker;
end;

function TInstantZeosDBOConnector.GetConnected: Boolean;
begin
  if HasConnection then
    Result := Connection.Connected
  else
    Result := inherited GetConnected;
end;

function TInstantZeosDBOConnector.GetConnection: TZConnection;
begin
  if not (csDesigning in ComponentState) then
    CheckConnection;
  Result := FConnection;
end;

function TInstantZeosDBOConnector.GetDatabaseExists: Boolean;
begin
  AssignLoginOptions;
  try
    Connection.Connect;
    Result := True;
    Connection.Disconnect;
  except
    on E : EZSQLException do
      if (E.ErrorCode = Broker.DBNotExistsErrorCode) then
        Result := False
      else
        raise;
  end;
end;

function TInstantZeosDBOConnector.GetLoginPrompt: Boolean;
begin
  if HasConnection then
    FLoginPrompt := FConnection.LoginPrompt;
  Result := FLoginPrompt;
end;

function TInstantZeosDBOConnector.HasConnection: Boolean;
begin
  Result := Assigned(FConnection);
end;

procedure TInstantZeosDBOConnector.InternalBuildDatabase(
  Scheme: TInstantScheme);
begin
  StartTransaction;
  try
    inherited;
    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TInstantZeosDBOConnector.InternalCommitTransaction;
begin
  CheckConnection;
  if not FConnection.AutoCommit then
    FConnection.Commit;
end;

procedure TInstantZeosDBOConnector.InternalConnect;
begin
  CheckConnection;
  FConnection.Connect;
end;

procedure TInstantZeosDBOConnector.InternalCreateDatabase;
begin
  if Connection.Connected then
    raise EInstantError.Create(SDatabaseOpen);
  Broker.CreateDatabase;
end;

procedure TInstantZeosDBOConnector.InternalDisconnect;
begin
  if HasConnection then
    FConnection.Disconnect;
end;

procedure TInstantZeosDBOConnector.InternalRollbackTransaction;
begin
  CheckConnection;
  if not FConnection.AutoCommit then
    FConnection.Rollback;
end;

procedure TInstantZeosDBOConnector.InternalStartTransaction;
begin
  CheckConnection;
  // ZeosDBO check if AutoCommit is true when starting an explicit transaction,
  // changing AutoCommit to False
  if FConnection.AutoCommit then
    FConnection.StartTransaction;
end;

procedure TInstantZeosDBOConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FConnection) and (Operation = opRemove) then
    FConnection := nil;
end;

function TInstantZeosDBOConnector.ParamByName(const AName: string): string;
begin
  Result := Connection.Properties.Values[AName];
end;

procedure TInstantZeosDBOConnector.SetConnection(Value: TZConnection);
begin
  if Value <> FConnection then
  begin
    Disconnect;
    DoBeforeConnectionChange;
    FConnection := Value;
    AssignLoginOptions;
    DoAfterConnectionChange;
  end;
end;

procedure TInstantZeosDBOConnector.SetLoginPrompt(const Value: Boolean);
begin
  FLoginPrompt := Value;
  AssignLoginOptions;
end;

procedure TInstantZeosDBOConnector.SetUseDelimitedIdents(const Value: Boolean);
begin
  FUseDelimitedIdents := Value;
end;

{ TInstantZeosDBOBroker }

procedure TInstantZeosDBOBroker.AssignDataSetParams(DataSet: TDataSet;
  AParams: TParams);
var
  i: Integer;
  SourceParam, TargetParam: TParam;
begin
  //don't call inherited
  for i := 0 to Pred(AParams.Count) do
  begin
    SourceParam := AParams[i];
    TargetParam :=
     (DataSet as TZReadOnlyQuery).Params.FindParam(SourceParam.Name);
    if Assigned(TargetParam) then
      AssignParam(SourceParam, TargetParam);
  end;
end;

procedure TInstantZeosDBOBroker.AssignParam(SourceParam, TargetParam: TParam);
begin
  case SourceParam.DataType of
    ftBoolean:
      if UseBooleanFields then
        TargetParam.Assign(SourceParam)
      else
        TargetParam.AsInteger := Integer(SourceParam.AsBoolean);
    else
      TargetParam.Assign(SourceParam);
  end;
end;

function TInstantZeosDBOBroker.CreateCatalog(
  const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := TInstantZeosDBOCatalog.Create(AScheme, Self);
end;

procedure TInstantZeosDBOBroker.CreateDatabase;
begin
  InternalCreateDatabase;
end;

function TInstantZeosDBOBroker.CreateDataSet(const AStatement: string;
  AParams: TParams): TDataSet;
var
  Query: TZReadOnlyQuery;
begin
  Query := TZReadOnlyQuery.Create(nil);
  try
    Query.Connection := Connector.Connection;
    Query.SQL.Text := AStatement;
    if Assigned(AParams) then
      AssignDatasetParams(Query, AParams);
    Result := Query;
  except
    Query.Free;
    raise;
  end;
end;

function TInstantZeosDBOBroker.CreateDBBuildCommand(
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
  else if CommandType = ctAlterIndex then
    Result := TInstantDBBuildAlterIndexSQLCommand.Create(CommandType, Connector)
  else if CommandType = ctDropIndex then
    Result := TInstantDBBuildDropIndexSQLCommand.Create(CommandType, Connector)
  else
    Result := inherited CreateDBBuildCommand(CommandType);
end;

function TInstantZeosDBOBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  if UseBooleanFields then
    Result := TInstantSQLResolver.Create(Self, Map)
  else
    Result := TInstantZeosDBOResolver.Create(Self, Map);
end;

function TInstantZeosDBOBroker.DataTypeToColumnType(DataType: TInstantDataType;
  Size: Integer): string;
begin
  Result := InternalDataTypeToColumnType(DataType);
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantZeosDBOBroker.DBNotExistsErrorCode: Integer;
begin
  Result := InternalDBNotExistsErrorCode;
end;

function TInstantZeosDBOBroker.Execute(const AStatement: string;
  AParams: TParams): Integer;
var
  DataSet: TZReadOnlyQuery;
begin
  DataSet := AcquireDataSet(AStatement, AParams) as TZReadOnlyQuery;
  try
    DataSet.ExecSQL;
    Result := DataSet.RowsAffected;
  finally
    ReleaseDataSet(DataSet);
  end;
end;

function TInstantZeosDBOBroker.GetConnector: TInstantZeosDBOConnector;
begin
  Result := inherited Connector as TInstantZeosDBOConnector;
end;

function TInstantZeosDBOBroker.GetDatabaseName: string;
begin
  Result := Connector.Connection.Database;
end;

function TInstantZeosDBOBroker.GetDBMSName: string;
begin
  if Connector.Connected then
  {$IFDEF ZEOSDBO_V7+}
    Result := Connector.Connection.DbcConnection.GetMetadata.GetDatabaseInfo.GetDatabaseProductName
  {$ELSE}
    Result := Connector.Connection.DbcConnection.GetMetadata.GetDatabaseProductName
  {$ENDIF}
  else
    Result := Connector.Connection.Protocol;
end;

function TInstantZeosDBOBroker.GetSQLDelimiters: string;
begin
  if not Connector.UseDelimitedIdents then
    Result := ''
  else
  begin
    Result := InternalSQLDelimiter;
    if Result = ' ' then
      Result := '';
  end;
end;

function TInstantZeosDBOBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

procedure TInstantZeosDBOBroker.InternalCreateDatabase;
begin
  raise EInstantError.CreateFmt(SDatabaseCreationNotSupported, [GetDBMSName]);
end;

function TInstantZeosDBOBroker.InternalCreateQuery: TInstantQuery;
begin
  if UseBooleanFields then
    Result := TInstantSQLQuery.Create(Connector)
  else
    Result := TInstantZeosDBOQuery.Create(Connector);
end;

function TInstantZeosDBOBroker.InternalDBNotExistsErrorCode: Integer;
begin
  Result := 0;
end;

// Return the string used to quote SQL identifiers
// If quoting is not supported, returns ' '
function TInstantZeosDBOBroker.InternalSQLDelimiter: string;
begin
  Result := '"';
end;

{ TInstantZeosDBOResolver }

function TInstantZeosDBOResolver.ReadBooleanField(DataSet: TDataSet;
  const FieldName: string): Boolean;
begin
  Result := Boolean(DataSet.FieldByName(FieldName).AsInteger);
end;

{ TInstantZeosDBOTranslator }

function TInstantZeosDBOTranslator.TranslateConstant(
  Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean;
begin
  if SameText(Constant.Value, InstantTrueString) then
  begin
    Writer.WriteChar('1');
    Result := True;
  end
  else if SameText(Constant.Value, InstantFalseString) then
  begin
    Writer.WriteChar('0');
    Result := True;
  end
  else
    Result := inherited TranslateConstant(Constant, Writer);
end;

{ TInstantZeosDBOQuery }

class function TInstantZeosDBOQuery.TranslatorClass:
  TInstantRelationalTranslatorClass;
begin
  Result := TInstantZeosDBOTranslator;
end;

{ TInstantZeosDBOSybaseBroker }

{$IFDEF SYBASE_SUPPORT}
function TInstantZeosDBOSybaseBroker.InternalDataTypeToColumnType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'DOUBLE PRECISION',
    'MONEY',
    'TINYINT',
    'VARCHAR',
    'TEXT',
    'DATETIME',
    'IMAGE',
    'DATETIME',
    'DATETIME',
    'INTEGER');
begin
  Result := Types[DataType];
end;

function TInstantZeosDBOSybaseBroker.UseBooleanFields: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{ TInstantZeosDBOMSSQLBroker }

{$IFDEF MSSQL_SUPPORT}
function TInstantZeosDBOMSSQLBroker.InternalDataTypeToColumnType(
  DataType: TInstantDataType): string;
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
end;

function TInstantZeosDBOMSSQLBroker.UseBooleanFields: Boolean;
begin
  Result := True;
end;
{$ENDIF}

{ TInstantZeosDBOIbFbBroker }

{$IFDEF IBFB_SUPPORT}
procedure TInstantZeosDBOIbFbBroker.InternalCreateDatabase;
var
  OldProperties: string;
  CharacterSet: string;
begin
  // do not call inherited
  with Connector.Connection do
  begin
    OldProperties := Properties.Text;
    CharacterSet := Properties.Values['codepage'];
    if CharacterSet = '' then
      CharacterSet := 'ISO8859_1';
    Properties.Text := Format(
     'createNewDatabase=CREATE DATABASE ''%s'' ' +
     'USER ''%s'' PASSWORD ''%s'' PAGE_SIZE 4096 ' +
     'DEFAULT CHARACTER SET %s', [Database, User, Password, CharacterSet]);
    try
      Connect;
      Disconnect;
    finally
      Properties.Text := OldProperties;
    end;
  end;
end;

function TInstantZeosDBOIbFbBroker.InternalDataTypeToColumnType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'DOUBLE PRECISION',
    'DECIMAL(14,4)',
    'SMALLINT',
    'VARCHAR',
    'BLOB SUB_TYPE 1',
    'TIMESTAMP',
    'BLOB',
    'TIMESTAMP',
    'TIMESTAMP',
    'INTEGER');
begin
  Result := Types[DataType];
end;

function TInstantZeosDBOIbFbBroker.InternalDBNotExistsErrorCode: Integer;
begin
  Result := -902;
end;

function TInstantZeosDBOIbFbBroker.UseBooleanFields: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{ TInstantZeosDBOOracleBroker }

{$IFDEF ORACLE_SUPPORT}
function TInstantZeosDBOOracleBroker.InternalDataTypeToColumnType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'NUMBER(10)',
    'FLOAT',
    'NUMBER(14,4)',
    'NUMBER(1)',
    'VARCHAR2',
    'CLOB',
    'DATE',
    'BLOB',
    'DATE',
    'DATE',
    'INTEGER');
begin
  Result := Types[DataType];
end;

function TInstantZeosDBOOracleBroker.UseBooleanFields: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{ TInstantZeosDBOPgSQLBroker }

{$IFDEF PGSQL_SUPPORT}
function TInstantZeosDBOPgSQLBroker.InternalDataTypeToColumnType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT8',
    'DECIMAL(14,4)',
    'BOOLEAN',
    'VARCHAR',
    'TEXT',
    'TIMESTAMP',
    'BYTEA',
    'TIMESTAMP',
    'TIMESTAMP',
    'INTEGER');
begin
  Result := Types[DataType];
end;

function TInstantZeosDBOPgSQLBroker.UseBooleanFields: Boolean;
begin
  Result := True;
end;
{$ENDIF}

{$IFDEF MYSQL_SUPPORT}

{ TInstantMySQLGenerator }

function TInstantMySQLGenerator.InternalGenerateDropIndexSQL(
  Metadata: TInstantIndexMetadata): string;
begin
  Result := Format('ALTER TABLE %s DROP INDEX %s',
   [Metadata.TableMetadata.Name, Metadata.Name]);
end;

{ TInstantZeosDBOMySQLBroker }

class function TInstantZeosDBOMySQLBroker.GeneratorClass: TInstantSQLGeneratorClass;
begin
  Result := TInstantMySQLGenerator;
end;

procedure TInstantZeosDBOMySQLBroker.InternalCreateDatabase;
var
  MySqlConnection: IZMySqlConnection;
  OldProperties: string;
begin
  // do not call inherited
  with Connector.Connection do
  begin
    OldProperties := Properties.Text;
    Properties.Values['dbless'] := 'TRUE';
    try
      try
        Connect;
        DbcConnection.QueryInterface(IZMySqlConnection, MySqlConnection);
        if Assigned(MySqlConnection) then
          MySqlConnection.GetPlainDriver.
           CreateDatabase(MySqlConnection.GetConnectionHandle, PAnsiChar(AnsiString(Database)))
        else
          inherited;
      finally
        Disconnect;
      end;
    finally
      Properties.Text := OldProperties;
    end;
  end;
end;

function TInstantZeosDBOMySQLBroker.InternalDataTypeToColumnType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'DECIMAL(14,4)',
    'BOOL',
    'VARCHAR',
    'TEXT',
    'DATETIME',
    'BLOB',
    'DATE',
    'TIME',
    'INTEGER');
begin
  Result := Types[DataType];
end;

function TInstantZeosDBOMySQLBroker.InternalDBNotExistsErrorCode: Integer;
begin
  Result := 1049;
end;

function TInstantZeosDBOMySQLBroker.InternalSQLDelimiter: string;
begin
  Result := '`';
end;

function TInstantZeosDBOMySQLBroker.UseBooleanFields: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{$IFDEF SQLITE_SUPPORT}

{ TInstantDBBuildAlterTableSQLCommand }

destructor TInstantDBBuildSQLiteAlterTableSQLCommand.Destroy;
begin
  FNewTableMetadata.Free;
  FTmpTableMD.Free;
  FScheme.Free;
  inherited;
end;

function TInstantDBBuildSQLiteAlterTableSQLCommand.GetDescription: string;
begin
  with Broker.Generator do
    case CommandType of
      ctAddField:
        Result := GenerateAddFieldSQL(NewMetadata as TInstantFieldMetadata);
      ctAlterField:
        Result := GenerateAlterFieldSQL(OldMetadata as TInstantFieldMetadata,
          NewMetadata as TInstantFieldMetadata);
      ctDropField:
        Result := GenerateDropFieldSQL(OldMetadata as TInstantFieldMetadata);
    end;
    Result := Result + ' - emulated with multi-statement SQL.';
end;

function TInstantDBBuildSQLiteAlterTableSQLCommand.GetNewTableMetadata: TInstantTableMetadata;
begin
  if not Assigned(FNewTableMetadata) then
  begin
    FNewTableMetadata := TInstantTableMetadata.Create(OldTableMetadata.Collection);
    FNewTableMetadata.Assign(OldTableMetadata);
    case CommandType of
      ctAddField:
        FNewTableMetadata.FieldMetadatas.Add.Assign(NewMetadata);
      ctAlterField:
      begin
        with FNewTableMetadata.FieldMetadatas do
          Remove(Find(OldMetadata.Name));
        FNewTableMetadata.FieldMetadatas.Add.Assign(NewMetadata);
      end;
      ctDropField:
        with FNewTableMetadata.FieldMetadatas do
          Remove(Find(OldMetadata.Name));
    end;
  end;
  Result := FNewTableMetadata;
end;

function TInstantDBBuildSQLiteAlterTableSQLCommand.GetOldTableMetadata: TInstantTableMetadata;
var
  FieldMetadata : TInstantFieldMetadata;
begin
  if not Assigned(FOldTableMetadata) then
  begin
    //Force to read the table from database
    FScheme := Broker.ReadDatabaseScheme(nil);

    FieldMetadata := nil;
    case CommandType of
      ctDropField, ctAlterField:
        FieldMetadata := TInstantFieldMetadata(OldMetadata);
      ctAddField:
        FieldMetadata := TInstantFieldMetadata(NewMetadata);
    end;

    { TODO : This only works for case-insensitive object names! }
    FOldTableMetadata :=
      FScheme.FindTableMetadata(AnsiUpperCase(FieldMetadata.TableMetadata.Name));
  end;
  Result := FOldTableMetadata;
end;

function TInstantDBBuildSQLiteAlterTableSQLCommand.GetSQLStatement(
  const Index: Integer): string;

  function CreateTmpTableMetadata(TableMetadata: TInstantTableMetadata):
      TInstantTableMetadata;
  begin
    Result := TInstantTableMetadata.Create(TableMetadata.Collection);
    Result.Assign(TableMetadata);
    Result.Name := TableMetadata.Name + STmpTableSuffix;
  end;

begin
  Result := inherited GetSQLStatement(Index);

  if not Assigned(FTmpTableMD) then
    FTmpTableMD := CreateTmpTableMetadata(OldTableMetadata);

  with TInstantSQLiteGenerator(Broker.Generator) do
    case Index of
      0: Result := GenerateCreateTempTableSQL(FTmpTableMD);

      1: Result := GenerateInsertFromSelectSQL(OldTableMetadata, FTmpTableMD);

      2: Result := GenerateDropTableSQL(OldTableMetadata);

      3: Result := GenerateCreateTableSQL(NewTableMetadata);

      4: Result := GenerateInsertFromSelectSQL(FTmpTableMD, NewTableMetadata);

      5: Result := GenerateDropTableSQL(FTmpTableMD);
    end;
end;

function TInstantDBBuildSQLiteAlterTableSQLCommand.GetSQLStatementCount: Integer;
begin
  Result := 6;
end;

procedure TInstantDBBuildSQLiteAlterTableSQLCommand.InternalExecute;
var
  iStatement: Integer;
  InTransaction: Boolean;
begin
  InTransaction := false;
  try
    for iStatement := 0 to Pred(GetSQLStatementCount) do
    begin
      Broker.Execute(GetSQLStatement(iStatement));
      InTransaction := (iStatement > 2) and (iStatement < 5);
    end;
  except
    if InTransaction then Rollback;
    raise;
  end;
end;

procedure TInstantDBBuildSQLiteAlterTableSQLCommand.Rollback;
begin
  { TODO : Some DBMS supports data definition within transactions, ZEOS known that and then the follow code are unnecessary! }
  with Broker, TInstantSQLiteGenerator(Broker.Generator) do
  begin
    try
      Execute(GenerateDropTableSQL(NewTableMetadata));
    except
      //Safe if NewTable doesn´t exist
    end;
    Execute(GenerateCreateTableSQL(OldTableMetadata));
    Execute(GenerateInsertFromSelectSQL(FTmpTableMD, OldTableMetadata));
    Execute(GenerateDropTableSQL(FTmpTableMD));
  end;
end;

{ TInstantSQLiteGenerator }

function TInstantSQLiteGenerator.GenerateCreateTempTableSQL(
  Metadata: TInstantTableMetadata): string;
begin
  Result := InternalGenerateCreateTempTableSQL(Metadata);
end;

function TInstantSQLiteGenerator.GenerateInsertFromSelectSQL(
  const SourceMetadata, TargetMetadata: TInstantTableMetadata): string;
begin
  Result := InternalGenerateInsertFromSelectSQL(SourceMetadata, TargetMetadata);
end;

function TInstantSQLiteGenerator.InternalGenerateCreateTempTableSQL(
  Metadata: TInstantTableMetadata): string;
begin
  Result := InternalGenerateCreateTableSQL(Metadata);
  Insert('TEMP ', Result, Pos('TABLE', Result));
end;

function TInstantSQLiteGenerator.InternalGenerateInsertFromSelectSQL(
  const SourceMetadata, TargetMetadata: TInstantTableMetadata): string;
var
  i: Integer;
  TargetField : TInstantFieldMetadata;
  FieldList : string;
begin
  for i := 0 to Pred(TargetMetadata.FieldMetadatas.Count) do
  begin
    TargetField := TargetMetadata.FieldMetadatas[i];
    { TODO : This only works for case-insensitive object names! }
    if SourceMetadata.FieldMetadatas.IndexOf(AnsiUpperCase(TargetField.Name)) > -1 then
      FieldList := FieldList + EmbraceField(TargetField.Name) + ', ';
  end;
  Delete(FieldList, Length(FieldList) - 1, 2);
  Result := Format('INSERT INTO %s(%s) SELECT %s FROM %s',
    [TargetMetadata.Name, FieldList, FieldList, SourceMetadata.Name]);
end;

{ TInstantZeosDBOSQLiteBroker }

function TInstantZeosDBOSQLiteBroker.CreateDBBuildCommand(
  const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  case CommandType of
    ctAddField, ctAlterField, ctDropField:
      Result := TInstantDBBuildSQLiteAlterTableSQLCommand.Create(CommandType, Connector)
    else
      Result := inherited CreateDBBuildCommand(CommandType);
  end;
end;

class function TInstantZeosDBOSQLiteBroker.GeneratorClass:
  TInstantSQLGeneratorClass;
begin
  Result := TInstantSQLiteGenerator;
end;

function TInstantZeosDBOSQLiteBroker.InternalDataTypeToColumnType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'NUMERIC(14,4)',
    'BOOLEAN',
    'VARCHAR',
    'TEXT',
    'TIMESTAMP',
    'BLOB',
    'TIMESTAMP',
    'TIMESTAMP',
    'INTEGER');
begin
  Result := Types[DataType];
end;

function TInstantZeosDBOSQLiteBroker.UseBooleanFields: Boolean;
begin
  Result := True;
end;
{$ENDIF}

initialization
  RegisterClass(TInstantZeosDBOConnectionDef);
  TInstantZeosDBOConnector.RegisterClass;
  {$IFDEF D5}
  if not Assigned(LoginDialogProc) then
    LoginDialogProc := LoginDialogProcCompatibility;
  {$ENDIF}

finalization
  TInstantZeosDBOConnector.UnregisterClass;
  {$IFDEF D5}
  if @LoginDialogProc = @LoginDialogProcCompatibility then
    LoginDialogProc := nil;
  {$ENDIF}

end.
