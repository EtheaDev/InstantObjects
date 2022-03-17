(*
 *   InstantObjects
 *   Embarcadero FireDAC Support
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
 * The Original Code is: InstantObjects FireDAC Support
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

unit InstantFireDAC;

{$I '..\..\InstantDefines.inc'}

// Supported databases  (only MSSQL and Firebird have been tested)

{$DEFINE SYBASE_SUPPORT}
{$DEFINE MSSQL_SUPPORT}
{$DEFINE IBFB_SUPPORT}
{$DEFINE ORACLE_SUPPORT}
{$DEFINE PGSQL_SUPPORT}
{$DEFINE MYSQL_SUPPORT}
{$DEFINE SQLITE_SUPPORT}

interface

uses
  Classes, Db, InstantPersistence, InstantCommand, InstantDBBuild,
  InstantBrokers, InstantMetadata, InstantTypes, FireDAC.Comp.Client,
  {$IFDEF IBFB_SUPPORT}FireDAC.Phys.IBBase, FireDAC.Phys.FB, FireDAC.Phys.IB,{$ENDIF}
  {$IFDEF MSSQL_SUPPORT}FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLMeta,{$ENDIF}
  {$IFDEF SYBASE_SUPPORT}FireDAC.Phys.ASA, FireDAC.Phys.ASAWrapper,{$ENDIF}
  {$IFDEF ORACLE_SUPPORT}FireDAC.Phys.ORACLE, FireDAC.Phys.ORACLEMeta,{$ENDIF}
  {$IFDEF PGSQL_SUPPORT}FireDAC.Phys.PG, FireDAC.Phys.PGWrapper,{$ENDIF}
  {$IFDEF MYSQL_SUPPORT}FireDAC.Phys.MYSQL, FireDAC.Phys.MYSQLWrapper,{$ENDIF}
  {$IFDEF SQLITE_SUPPORT}FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,{$ENDIF}
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Intf, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet
  {$IFDEF D10+}, Variants, DBCommonTypes{$ENDIF}, System.Typinfo;

type
  TInstantFireDACConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FCatalog: string;
    FDatabase: string;
    FHostName: string;
    FAdditionalParams: string;
    FDriverId: string;
    FProtocol: string;
    FUseDelimitedIdents : boolean;
    FUser_Name: string;
    FPassword: string;
    FEncryptedPassword: string;
    FOSAuthent: Boolean;
    FServer: string;
    FPort: integer;
    FIsolation: TFDTxIsolation;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    {$IFNDEF IO_CONSOLE}
    function Edit: Boolean; override;
    {$ENDIF}
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    function GetConnectionParams: string;
    procedure UpdateConnectionParams(const AParams: string);
  public
  published
    property Catalog: string read FCatalog write FCatalog stored False;
    property Database: string read FDatabase write FDatabase;
    property HostName: string read FHostName write FHostName;
    property Server: string read FServer write FServer;
    property Password: string read FPassword write FPassword;
    property EncryptedPassword: string read FEncryptedPassword write FEncryptedPassword;
    property Port: Integer read FPort write FPort stored False;
    property AdditionalParams: string read FAdditionalParams write FAdditionalParams;
    property Protocol: string read FProtocol write FProtocol stored False;
    property DriverId: string read FDriverId write FDriverId;
    property UseDelimitedIdents: boolean read FUseDelimitedIdents write FUseDelimitedIdents;
    property User_Name: string read FUser_Name write FUser_Name;
    property OSAuthent: Boolean read FOSAuthent write FOSAuthent;
    property Isolation: TFDTxIsolation read FIsolation write FIsolation default xiUnspecified;
    property ConnectionParams: string read GetConnectionParams;
  end;

  TInstantFireDACBroker = class;

  TInstantFireDACConnector = class(TInstantConnectionBasedConnector)
  private
    FOnLogin: TFDConnectionLoginEvent;
    FUseDelimitedIdents : Boolean;
    function GetBroker: TInstantFireDACBroker;
    function GetConnection: TFDConnection;
    procedure SetConnection(Value: TFDConnection);
    procedure SetUseDelimitedIdents(const Value: Boolean);
  protected
    procedure AfterConnectionChange; override;
    procedure BeforeConnectionChange; override;
    procedure AssignLoginOptions; override;
    function CreateBroker: TInstantBroker; override;
    {$IFDEF D22+}
    procedure DoLogin(AConnection: TFDCustomConnection;
      AParams: TFDConnectionDefParams); virtual;
    {$ELSE}
    procedure DoLogin(AConnection: TFDCustomConnection;
      const AConnectionDef: IFDStanConnectionDef); virtual;
    {$ENDIF}
    function GetConnected: Boolean; override;
    function GetDatabaseExists: Boolean; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalCreateDatabase; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
    function ParamByName(const AName: string): string;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    property Broker: TInstantFireDACBroker read GetBroker;
  published
    property Connection: TFDConnection read GetConnection write SetConnection;
    property UseDelimitedIdents: Boolean read FUseDelimitedIdents write SetUseDelimitedIdents default False;
  end;

  TInstantFireDACBroker = class(TInstantSQLBroker)
  private
    function GetConnector: TInstantFireDACConnector;
  protected
    procedure AssignDataSetParams(DataSet: TDataSet; AParams: TParams;
      OnAssignParamValue: TAssignParamValue = nil); override;
    procedure AssignParam(SourceParam: TParam; TargetParam: TFDParam); virtual;
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    class function GetCatalogClass: TInstantCatalogClass; override;
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    function GetSQLDelimiters: string; override;
    function GetSQLQuote: Char; override;
    procedure InternalCreateDatabase; virtual;
    function InternalCreateQuery: TInstantQuery; override;
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; virtual; abstract;
    function InternalDBNotExistsErrorCode: Integer; virtual;
    function UseBooleanFields: Boolean; virtual; abstract;
  public
    procedure CreateDatabase;
    function CreateDataSet(const AStatement: string; AParams: TParams = nil;
      OnAssignParamValue: TAssignParamValue = nil): TDataSet; override;
    function CreateDBBuildCommand(const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams = nil;
      OnAssignParamValue: TAssignParamValue = nil): Integer; override;
    function DBNotExistsErrorCode: Integer;
    property Connector: TInstantFireDACConnector read GetConnector;
  end;

  //
  // Resolver that interprets an integer column value as a boolean
  //
  TInstantFireDACResolver = class(TInstantSQLResolver)
  protected
    function ReadBooleanField(DataSet: TDataSet; const FieldName: string): Boolean; override;
  end;

  //
  // Translator that translates a boolean value to a '0' or '1'
  //
  TInstantFireDACTranslator = class(TInstantRelationalTranslator)
  protected
    function TranslateConstant(Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean; override;
  end;

  TInstantFireDACQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

  { Sybase broker }

  {$IFDEF SYBASE_SUPPORT}
  TInstantFireDACSybaseBroker = class(TInstantFireDACBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { MS SQL Server broker }

  {$IFDEF MSSQL_SUPPORT}
  TInstantFireDACMSSQLBroker = class(TInstantFireDACBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { Interbase and Firebird brokers }

  {$IFDEF IBFB_SUPPORT}
  TInstantFireDACIbFbBroker = class(TInstantFireDACBroker)
  protected
    procedure InternalCreateDatabase; override;
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function InternalDBNotExistsErrorCode: Integer; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { Oracle broker }

  {$IFDEF ORACLE_SUPPORT}
  TInstantFireDACOracleBroker = class(TInstantFireDACBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  end;
  {$ENDIF}

  { PostgreSQL broker }

  {$IFDEF PGSQL_SUPPORT}
  TInstantFireDACPgSQLBroker = class(TInstantFireDACBroker)
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

  TInstantFireDACMySQLBroker = class(TInstantFireDACBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string;
      override;
    function InternalDBNotExistsErrorCode: Integer; override;
    function UseBooleanFields: Boolean; override;
  public
    class function GeneratorClass: TInstantSQLGeneratorClass; override;
  end;
  {$ENDIF}

  { SQLite broker }

  {$IFDEF SQLITE_SUPPORT}

  //SQLite doesn´t support ALTER TABLE. Spport for ADD COLUMN, ALTER COLUMN and
  //DROP COLUMN, is emulated with a couple of CREATE TEMP TABLE, INSERT INTO,
  //DROP TABLE, CREATE TABLE, INSERT INTO and finally DROP TABLE
  TInstantDBBuildSQLiteAlterTableSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    FTmpTableMD       : TInstantTableMetadata;
    FOldTableMetadata : TInstantTableMetadata;
    FNewTableMetadata : TInstantTableMetadata;
    FScheme           : TInstantScheme;
  protected
    procedure Rollback; virtual;
    function GetDescription: string; override;
    function GetSQLStatement(const Index: Integer): string; override;
    function GetSQLStatementCount: Integer; override;
    function GetNewTableMetadata: TInstantTableMetadata;
    function GetOldTableMetadata: TInstantTableMetadata;
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

  TInstantFireDACSQLiteBroker = class(TInstantFireDACBroker)
  protected
    function InternalDataTypeToColumnType(DataType: TInstantDataType): string; override;
    function UseBooleanFields: Boolean; override;
  public
    class function GeneratorClass: TInstantSQLGeneratorClass; override;
    function CreateDBBuildCommand(const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; override;
  end;
  {$ENDIF}

procedure AssignFireDACDriverIds(Strings: TStrings);
procedure AssignFireDACIsolation(Strings: TStrings);

implementation

uses
  SysUtils, Types, StrUtils,
  {$IFNDEF IO_CONSOLE}Controls, InstantFireDACConnectionDefEdit, FireDAC.VCLUI.Login,{$ENDIF}
  InstantConsts, InstantClasses,
  InstantFireDACCatalog, InstantUtils,
  FireDAC.Stan.Consts;

resourcestring
  SInvalidDatabasePageSize = 'Invalid database PageSize value: "%s"';

{$IFDEF SQLITE_SUPPORT}
const
  STmpTableSuffix = '_IOTmp_';
{$ENDIF}

procedure AssignFireDACDriverIds(Strings: TStrings);
begin
  if assigned(Strings) then
    begin
      Strings.Clear;

      {$IFDEF SYBASE_SUPPORT}
      Strings.Add(S_FD_ASAId);
      {$ENDIF}

      {$IFDEF MSSQL_SUPPORT}
      Strings.Add(S_FD_MSSQLId);
      {$ENDIF}

      {$IFDEF IBFB_SUPPORT}
      Strings.Add(S_FD_FBId);
      Strings.Add(S_FD_IBId);
      {$ENDIF}

      {$IFDEF ORACLE_SUPPORT}
      Strings.Add(S_FD_OraId);
      {$ENDIF}

      {$IFDEF PGSQL_SUPPORT}
      Strings.Add(S_FD_PGId);
      {$ENDIF}

      {$IFDEF MYSQL_SUPPORT}
      Strings.Add(S_FD_MySQLId);
      {$ENDIF}

      {$IFDEF SQLITE_SUPPORT}
      Strings.Add(S_FD_SQLiteId);
      {$ENDIF}
    end;
end;


procedure AssignFireDACIsolation(Strings: TStrings);
var
  I: TFDTxIsolation;
begin
  Strings.Clear;
  for I := low(TFDTxIsolation) to High(TFDTxIsolation) do
    Strings.Add(GetEnumName(TypeInfo(TFDTxIsolation), Ord(I)));
end;


{ Local routines }

{ TInstantFireDACConnectionDef }

function TInstantFireDACConnectionDef.GetConnectionParams: string;
var
  LServer: string;
begin
  if DriverId = '' then
    Result := 'DriverID='+Protocol+sLineBreak
  else
    Result := 'DriverID='+DriverId+sLineBreak;
  LServer := Server;
  if LServer = '' then
  begin
    //HostName + Port (old configuration)
    LServer := HostName;
    if (Port > 0) then
      LServer := LServer + ', ' + IntToStr(Port);
  end;
  Result := Result + 'Server='+LServer+sLineBreak;
  if Port <> 0 then
    Result := Result + 'Port='+IntToStr(Port)+sLineBreak;
  if User_Name <> '' then
    Result := Result +'User_Name='+User_Name+sLineBreak;
  if EncryptedPassword <> '' then
    Result := Result + 'EncryptedPassword='+EncryptedPassword+sLineBreak
  else if Password <> '' then
    Result := Result + 'Password='+Password+sLineBreak;
  if Database <> '' then
    Result := Result + 'Database='+ExpandDatabaseName(Database)+sLineBreak;
  if OSAuthent then
    Result := Result + 'OSAuthent=Yes'+sLineBreak
  else
    Result := Result + 'OSAuthent=No'+sLineBreak;
  if LoginPrompt then
    Result := Result + 'LoginPrompt=True'+sLineBreak;
  if UseDelimitedIdents then
    Result := Result + 'UseDelimitedIdents=True'+sLineBreak;
  case BlobStreamFormat of
    sfBinary: Result := Result + 'BlobStreamFormat=sfBinary'+sLineBreak;
    sfXML: Result := Result + 'BlobStreamFormat=sfXML'+sLineBreak;
    {$IFDEF DELPHI_NEON}
    sfJSON: Result := Result + 'BlobStreamFormat=sfJSON'+sLineBreak;
    {$ENDIF}
  end;
  if UseUnicode then
    Result := Result + 'CharacterSet=utf8'+sLineBreak;
  Result := Result + AdditionalParams;
end;

class function TInstantFireDACConnectionDef.ConnectionTypeName: string;
begin
  Result := 'FireDAC';
end;

class function TInstantFireDACConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantFireDACConnector;
end;

{$IFNDEF IO_CONSOLE}
function TInstantFireDACConnectionDef.Edit: Boolean;
var
  EditForm : TInstantFireDACConnectionDefEditForm;
begin
  EditForm := TInstantFireDACConnectionDefEditForm.CreateForConnectionDef(nil,
    Self);
  try
    EditForm.LoadData(Self);
    Result := (EditForm.ShowModal = mrOk);
    if (Result) then
      EditForm.SaveData(Self);
  finally
    EditForm.Free;
  end;
end;
{$ENDIF}

function TInstantFireDACConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
var
  Connection  : TFDConnection;
begin
  Connection := TFDConnection.Create(AOwner);
  try
    Connection.TxOptions.AutoCommit := false;
    Connection.TxOptions.Isolation := Isolation;
    Connection.Params.Text := ConnectionParams;

    //Those parameters speed-up reading
    Connection.ResourceOptions.DirectExecute := True;
    //Esclude use of Macros for SQL-Statements
    Connection.ResourceOptions.MacroCreate := False;
    Connection.ResourceOptions.MacroExpand := False;
    Connection.FetchOptions.Mode := fmAll;
  except
    Connection.Free;
    raise;
  end;
  Result := Connection;
end;

procedure TInstantFireDACConnectionDef.InitConnector(
  Connector: TInstantConnector);
begin
  inherited;
  (Connector as TInstantFireDACConnector).UseDelimitedIdents := UseDelimitedIdents;
end;

procedure TInstantFireDACConnectionDef.UpdateConnectionParams(
  const AParams: string);
var
  LParams: TStringList;
  LPort: string;
  LValue: Integer;
  LIsolation: string;
begin
  LParams := TStringList.Create;
  try
    LParams.Text := AParams;
    Server             := LParams.Values['Server'];
    if Server = '' then
    begin
      HostName           := LParams.Values['HostName'];
      LPort := LParams.Values['Port'];
      if (LPort <> '') and (LPort<> '0') then
        Port := StrToInt(LPort);
    end;
    DriverId           := LParams.Values['DriverId'];
    if DriverId = '' then
      Protocol         := LParams.Values['Protocol'];
    Database           := LParams.Values['Database'];
    User_Name          := LParams.Values['User_Name'];
    Password           := LParams.Values['Password'];
    LoginPrompt        := SameText(LParams.Values['LoginPrompt'],'True');
    OSAuthent          := SameText(LParams.Values['OSAuthent'],'Yes');
    LIsolation         := LParams.Values['Isolation'];

    if LIsolation <> '' then
    begin
      LValue := GetEnumValue(TypeInfo(TFDTxIsolation), LIsolation);
      if LValue <> -1 then
        Isolation := TFDTxIsolation(LValue)
      else
        Isolation := xiReadCommitted;
    end
    else
      Isolation := xiReadCommitted;

    if SameText(LParams.Values['BlobStreamFormat'], 'sfBinary') then
      BlobStreamFormat := sfBinary
    {$IFDEF DELPHI_NEON}
    else if SameText(LParams.Values['BlobStreamFormat'], 'sfJSON') then
      BlobStreamFormat := sfJSON
    {$ENDIF}
    else
      BlobStreamFormat := sfXML; //Default for FireDAC
    UseDelimitedIdents := SameText(LParams.Values['UseDelimitedIdents'],'TRUE');
    EncryptedPassword := LParams.Values['EncryptedPassword'];
  finally
    LParams.Free;
  end;
end;

{ TInstantFireDACConnector }

procedure TInstantFireDACConnector.AfterConnectionChange;
begin
  if (HasConnection) then
    begin
      Connection.Close;
      if Assigned(FOnLogin) then
        Connection.OnLogin := DoLogin;
    end;
end;

procedure TInstantFireDACConnector.AssignLoginOptions;
begin
  inherited;
  if HasConnection then
  begin
    if Assigned(FOnLogin) and not Assigned(Connection.OnLogin) then
      Connection.OnLogin := FOnLogin;
  end;
end;

procedure TInstantFireDACConnector.BeforeConnectionChange;
begin
  inherited;
end;

class function TInstantFireDACConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantFireDACConnectionDef;
end;

function TInstantFireDACConnector.CreateBroker: TInstantBroker;
begin
  CheckConnection;
  Result := nil;

  {$IFDEF SYBASE_SUPPORT}
  if SameText(Connection.DriverName, S_FD_ASAId) then
  begin
    Result := TInstantFireDACSybaseBroker.Create(Self);
    SQLEngine := seSybase;
  end;
  {$ENDIF}

  {$IFDEF MSSQL_SUPPORT}
  if SameText(Connection.DriverName, S_FD_MSSQLId) then
  begin
    Result := TInstantFireDACMSSQLBroker.Create(Self);
    SQLEngine := seMSSQL;
  end;
  {$ENDIF}

  {$IFDEF IBFB_SUPPORT}
  if SameText(Connection.DriverName, S_FD_IBId) then
  begin
    Result := TInstantFireDACIbFbBroker.Create(Self);
    SQLEngine := seInterbase;
  end;
  if SameText(Connection.DriverName, S_FD_FBId) then
  begin
    Result := TInstantFireDACIbFbBroker.Create(Self);
    SQLEngine := seFirebird;
  end;
  {$ENDIF}

  {$IFDEF ORACLE_SUPPORT}
  if SameText(Connection.DriverName, S_FD_OraId) then
  begin
    Result := TInstantFireDACOracleBroker.Create(Self);
    SQLEngine := seOracle;
  end;
  {$ENDIF}

  {$IFDEF PGSQL_SUPPORT}
  if SameText(Connection.DriverName, S_FD_PGId) then
  begin
    Result := TInstantFireDACPgSQLBroker.Create(Self);
    SQLEngine := sePostGres;
  end;
  {$ENDIF}

  {$IFDEF MYSQL_SUPPORT}
  if SameText(Connection.DriverName, S_FD_MySQLId) then
  begin
    Result := TInstantFireDACMySQLBroker.Create(Self);
    SQLEngine := seMySQL;
  end;
  {$ENDIF}

  {$IFDEF SQLITE_SUPPORT}
  if SameText(Connection.DriverName, S_FD_SQLiteId) then
  begin
    Result := TInstantFireDACSQLiteBroker.Create(Self);
    SQLEngine := seSQLLite;
  end;
  {$ENDIF}

  if (Result = nil) then
    raise EInstantError.CreateFmt(SProtocolNotSupported,
     [Connection.DriverName]);
end;

{$IFDEF D22+}
procedure TInstantFireDACConnector.DoLogin(AConnection: TFDCustomConnection;
  AParams: TFDConnectionDefParams);
{$ELSE}
procedure TInstantFireDACConnector.DoLogin(AConnection: TFDCustomConnection;
  const AConnectionDef: IFDStanConnectionDef);
{$ENDIF}
begin
  if (assigned(FOnLogin)) then
    FOnLogin(AConnection, AParams);
end;

function TInstantFireDACConnector.GetBroker: TInstantFireDACBroker;
begin
  Result := inherited Broker as TInstantFireDACBroker;
end;

function TInstantFireDACConnector.GetConnected: Boolean;
begin
  if (HasConnection) then
    Result := Connection.Connected else
    Result := inherited GetConnected;
end;

function TInstantFireDACConnector.GetConnection: TFDConnection;
begin
  Result := inherited Connection as TFDConnection;
end;

function TInstantFireDACConnector.GetDatabaseExists: Boolean;
begin
  AssignLoginOptions;
  try
    Connection.Open;
    Result := True;
    Connection.Close;
  except
// TODO Fixup database detection logic for FireDAC - Exception approach may not work well
//    on E : EZSQLException do
//      if (E.ErrorCode = Broker.DBNotExistsErrorCode) then
//        Result := False
//      else
//        raise;
    Result := False
  end;
end;

procedure TInstantFireDACConnector.InternalBuildDatabase(
  Scheme: TInstantScheme);
begin
  StartTransaction;
  try
    inherited InternalBuildDatabase(Scheme);
    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TInstantFireDACConnector.InternalCommitTransaction;
begin
  CheckConnection;
  if (Connection.InTransaction) then
    Connection.Commit;
end;

procedure TInstantFireDACConnector.InternalCreateDatabase;
begin
  if (Connection.Connected) then
    raise EInstantError.Create(SDatabaseOpen);
  Broker.CreateDatabase;
end;

procedure TInstantFireDACConnector.InternalRollbackTransaction;
begin
  CheckConnection;
  if (Connection.InTransaction) then
    Connection.Rollback;
end;

procedure TInstantFireDACConnector.InternalStartTransaction;
begin
  CheckConnection;
  if (not Connection.InTransaction) then
    Connection.StartTransaction;
end;

function TInstantFireDACConnector.ParamByName(const AName: string): string;
begin
  Result := Connection.Params.Values[AName];
end;

procedure TInstantFireDACConnector.SetConnection(Value: TFDConnection);
begin
  inherited Connection := Value;
end;

procedure TInstantFireDACConnector.SetUseDelimitedIdents(const Value: Boolean);
begin
  FUseDelimitedIdents := Value;
end;

{ TInstantFireDACBroker }

procedure TInstantFireDACBroker.AssignDataSetParams(DataSet: TDataSet; AParams: TParams;
  OnAssignParamValue: TAssignParamValue = nil);
var
  SourceParam : TParam;
  TargetParam : TFDParam;
  i           : Integer;
  TargetParams: TFDParams;
begin
  TargetParams := (DataSet as TFDQuery).Params;
  // Don't call inherited
  if Assigned(OnAssignParamValue) and (AParams.Count = 0) then
  begin
    for i := 0 to Pred(TargetParams.Count) do
    begin
      TargetParam := TargetParams[i];
      SourceParam := AParams.FindParam(TargetParam.Name);
      if not Assigned(SourceParam) then
      begin
        SourceParam := AParams.AddParameter;
        SourceParam.Name := TargetParam.Name;
      end;
      OnAssignParamValue(SourceParam);
    end;
  end;
  for i := 0 to Pred(AParams.Count) do
  begin
    SourceParam := AParams[i];
    TargetParam := TargetParams.FindParam(SourceParam.Name);
    if assigned(TargetParam) then
    begin
      AssignParam(SourceParam, TargetParam);
    end;
  end;
end;

procedure TInstantFireDACBroker.AssignParam(SourceParam: TParam; TargetParam: TFDParam);

{$IFDEF D12+}
function ConvertBlobData(const Bytes: TBytes): RawByteString;
  begin
    SetLength(Result, Length(Bytes));
    if length(Result) > 0 then
      Move(Bytes[0], Result[1], Length(Bytes))
  end;
{$ENDIF}

begin
  case SourceParam.DataType of
    ftBoolean:
      if UseBooleanFields then
        TargetParam.Assign(SourceParam) else
        TargetParam.AsInteger := ord(SourceParam.AsBoolean);
    ftBlob:
    {$IFDEF D12+}
      TargetParam.AsBlob := ConvertBlobData(SourceParam.AsBlob);
    {$ELSE}
      TargetParam.AsBlob := SourceParam.AsBlob;
    {$ENDIF}
    ftDateTime:
      begin
        TargetParam.DataType := ftTimeStamp;
        TargetParam.Value := SourceParam.AsDateTime;
      end;
    ftCurrency:
      begin
        TargetParam.DataType := ftBCD;
        TargetParam.Value := SourceParam.AsCurrency;
      end;
    ftString:
      begin
        TargetParam.DataType := ftString;
        TargetParam.Value := SourceParam.Value;
      end;
    ftWideString:
      begin
        TargetParam.DataType := ftWideString;
        TargetParam.Value := SourceParam.AsWideString;
      end;
    ftMemo, ftFmtMemo:
      begin
        TargetParam.DataType := ftMemo;
        TargetParam.Value := SourceParam.Value;
      end;
    ftWideMemo:
      begin
        TargetParam.DataType := ftWideMemo;
        TargetParam.Value := SourceParam.Value;
      end;
    else
      TargetParam.Assign(SourceParam);
  end;
  
  if (SourceParam.IsNull) then
    begin
      TargetParam.Clear;
      TargetParam.Bound := true;
    end;
end;

function TInstantFireDACBroker.CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := TInstantFireDACCatalog.Create(AScheme,Self);
end;

procedure TInstantFireDACBroker.CreateDatabase;
begin
  InternalCreateDatabase;
end;

function TInstantFireDACBroker.CreateDataSet(const AStatement: string;
  AParams: TParams; OnAssignParamValue: TAssignParamValue): TDataSet;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connector.Connection;
    Query.SQL.Text := AStatement;
    if assigned(AParams) then
      AssignDatasetParams(Query, AParams, OnAssignParamValue);
    Result := Query;
  except
    Query.Free;
    raise;
  end;
end;

function TInstantFireDACBroker.CreateDBBuildCommand(
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

function TInstantFireDACBroker.CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  if UseBooleanFields then
    Result := TInstantSQLResolver.Create(Self, Map) else
    Result := TInstantFireDACResolver.Create(Self, Map);
end;

function TInstantFireDACBroker.DataTypeToColumnType(DataType: TInstantDataType;
  Size: Integer): string;
begin
  Result := InternalDataTypeToColumnType(DataType);
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantFireDACBroker.DBNotExistsErrorCode: Integer;
begin
  Result := InternalDBNotExistsErrorCode;
end;

function TInstantFireDACBroker.Execute(const AStatement: string;
  AParams: TParams; OnAssignParamValue: TAssignParamValue): Integer;
var
  LQuery: TFDQuery;
begin
  LQuery := AcquireDataSet(AStatement, AParams, OnAssignParamValue) as TFDQuery;
  try try
    LQuery.ExecSQL;
    Result := LQuery.RowsAffected;
  except
    on E: Exception do
      raise EInstantError.CreateFmt(SSQLExecuteError,
        [AStatement, GetParamsStr(AParams), E.Message], E);
  end;
  finally
    ReleaseDataSet(LQuery);
  end;
end;

class function TInstantFireDACBroker.GetCatalogClass: TInstantCatalogClass;
begin
  Result := TInstantFireDACCatalog;
end;

function TInstantFireDACBroker.GetConnector: TInstantFireDACConnector;
begin
  Result := inherited Connector as TInstantFireDACConnector;
end;

function TInstantFireDACBroker.GetDatabaseName: string;
begin
  Result := Connector.ParamByName('Database');
end;

function TInstantFireDACBroker.GetDBMSName: string;
begin
  Result := Connector.Connection.DriverName;
end;

function TInstantFireDACBroker.GetSQLDelimiters: string;
var
  LeftCh : char;
  RightCh : char;
begin
  if not Connector.UseDelimitedIdents then
    Result := ''
  else
  begin
    with Connector.Connection do
    begin
      LeftCh  := ConnectionMetaDataIntf.NameQuoteChar[ncDefault, nsLeft];
      RightCh := ConnectionMetaDataIntf.NameQuoteChar[ncDefault, nsRight];
    end;

    if (LeftCh = #0) or (RightCh = #0) or (LeftCh = ' ') or (RightCh = ' ') then
      Result := '' else
      Result := LeftCh + RightCh;
  end;
end;

function TInstantFireDACBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

procedure TInstantFireDACBroker.InternalCreateDatabase;
begin
  raise EInstantError.CreateFmt(SDatabaseCreationNotSupported, [GetDBMSName]);
end;

function TInstantFireDACBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantFireDACQuery.Create(Connector);
end;

function TInstantFireDACBroker.InternalDBNotExistsErrorCode: Integer;
begin
  Result := 0;
end;

{ TInstantFireDACResolver }

function TInstantFireDACResolver.ReadBooleanField(DataSet: TDataSet;
  const FieldName: string): Boolean;
begin
  Result := (DataSet.FieldByName(FieldName).AsInteger <> 0);
end;

{ TInstantFireDACTranslator }

function TInstantFireDACTranslator.TranslateConstant(
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
  end else if (Copy(Constant.Value,1,1) = '[') and (Copy(Constant.Value,length(Constant.Value),1) = ']') then
  begin
    Writer.WriteString(Copy(Constant.Value,2,length(Constant.Value)-2));
    Result := True;
  end else
    Result := inherited TranslateConstant(Constant, Writer);
end;

{ TInstantFireDACQuery }

class function TInstantFireDACQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantFireDACTranslator;
end;

{ TInstantFireDACSybaseBroker }

{$IFDEF SYBASE_SUPPORT}
function TInstantFireDACSybaseBroker.InternalDataTypeToColumnType(
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

function TInstantFireDACSybaseBroker.UseBooleanFields: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{ TInstantFireDACMSSQLBroker }

{$IFDEF MSSQL_SUPPORT}
function TInstantFireDACMSSQLBroker.InternalDataTypeToColumnType(
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

function TInstantFireDACMSSQLBroker.UseBooleanFields: Boolean;
begin
  Result := True;
end;
{$ENDIF}

{ TInstantFireDACIbFbBroker }

{$IFDEF IBFB_SUPPORT}
procedure TInstantFireDACIbFbBroker.InternalCreateDatabase;
var
  OldParams     : string;
  CharacterSet  : string;
  PageSizeStr   : string;
  PageSize      : integer;
const
  DEFAULT_DB_PAGESIZE = 8192;
begin
  // do not call inherited
  with Connector do
    begin
      OldParams := Connection.Params.Text;

      try
        CharacterSet := trim(Connection.Params.Values['CharacterSet']);
        PageSizeStr := trim(Connection.Params.Values['PageSize']);

        if (CharacterSet = '') then
          CharacterSet := 'utf8';

        if (PageSizeStr <> '') then
          begin
            PageSize := StrToIntDef(PageSizeStr,-1);
            if (PageSize <> 1024) and // Deprecated for FB 2.1+
               (PageSize <> 2048) and // Deprecated for FB 2.1+
               (PageSize <> 4096) and
               (PageSize <> 8192) and
               (PageSize <> 16384) then // Available FB 2.0 and later
              raise EInstantError.CreateFmt(SInvalidDatabasePageSize, [PageSizeStr]);
          end else
          begin
            PageSize := DEFAULT_DB_PAGESIZE;
          end;

        Connection.Params.Values['CharacterSet'] := CharacterSet;
        Connection.Params.Values['CreateDatabase'] := 'Yes';
        Connection.Params.Values['SQLDialect'] := '3';
        Connection.Params.Values['PageSize'] := IntToStr(PageSize);
        Connect;
        Disconnect;
      finally
        Connection.Params.Text := OldParams;
      end;
    end;
end;

function TInstantFireDACIbFbBroker.InternalDataTypeToColumnType(DataType: TInstantDataType): string;
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

function TInstantFireDACIbFbBroker.InternalDBNotExistsErrorCode: Integer;
begin
  Result := -902;
end;

function TInstantFireDACIbFbBroker.UseBooleanFields: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{ TInstantFireDACOracleBroker }

{$IFDEF ORACLE_SUPPORT}
function TInstantFireDACOracleBroker.InternalDataTypeToColumnType(
  DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'DECIMAL(14,4)',
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

function TInstantFireDACOracleBroker.UseBooleanFields: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{ TInstantFireDACPgSQLBroker }

{$IFDEF PGSQL_SUPPORT}
function TInstantFireDACPgSQLBroker.InternalDataTypeToColumnType(DataType: TInstantDataType): string;
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

function TInstantFireDACPgSQLBroker.UseBooleanFields: Boolean;
begin
  Result := True;
end;
{$ENDIF}

{$IFDEF MYSQL_SUPPORT}

{ TInstantMySQLGenerator }

function TInstantMySQLGenerator.InternalGenerateDropIndexSQL(Metadata: TInstantIndexMetadata): string;
begin
  Result := Format('ALTER TABLE %s DROP INDEX %s',
   [Metadata.TableMetadata.Name, Metadata.Name]);
end;

{ TInstantFireDACMySQLBroker }

class function TInstantFireDACMySQLBroker.GeneratorClass: TInstantSQLGeneratorClass;
begin
  Result := TInstantMySQLGenerator;
end;

function TInstantFireDACMySQLBroker.InternalDataTypeToColumnType(DataType: TInstantDataType): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'FLOAT',
    'DECIMAL(14,4)',
    'TINYINT(1)',
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

function TInstantFireDACMySQLBroker.InternalDBNotExistsErrorCode: Integer;
begin
  Result := 1049;
end;

function TInstantFireDACMySQLBroker.UseBooleanFields: Boolean;
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

function TInstantDBBuildSQLiteAlterTableSQLCommand.GetSQLStatement(const Index: Integer): string;

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
  { TODO : Some DBMS supports data definition within transactions,
    If FireDAC knows that then the follow code is unnecessary! }
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

{ TInstantFireDACSQLiteBroker }

function TInstantFireDACSQLiteBroker.CreateDBBuildCommand(
  const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  case CommandType of
    ctAddField, ctAlterField, ctDropField:
      Result := TInstantDBBuildSQLiteAlterTableSQLCommand.Create(CommandType, Connector)
    else
      Result := inherited CreateDBBuildCommand(CommandType);
  end;
end;

class function TInstantFireDACSQLiteBroker.GeneratorClass:
  TInstantSQLGeneratorClass;
begin
  Result := TInstantSQLiteGenerator;
end;

function TInstantFireDACSQLiteBroker.InternalDataTypeToColumnType(
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

function TInstantFireDACSQLiteBroker.UseBooleanFields: Boolean;
begin
  Result := True;
end;
{$ENDIF}

initialization
  RegisterClass(TInstantFireDACConnectionDef);
  TInstantFireDACConnector.RegisterClass;

finalization
  TInstantFireDACConnector.UnregisterClass;

end.
