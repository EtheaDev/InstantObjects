(*
 *   InstantObjects
 *   InterBase Express Support
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
 * Carlo Barazzetta, Nando Dessena, Joao Morais
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantIBX;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  System.Classes
  , Data.DB
  , IBX.IBDatabase
  , IBX.IBTable
  , IBX.IBQuery
  , System.SysUtils
  , InstantPersistence
  , InstantBrokers
  , InstantClasses
  , InstantCommand
  , InstantMetadata
  , InstantTypes
  ;

type
  TInstantIBXOption = (ibxUseDelimitedIdents);
  TInstantIBXOptions = set of TInstantIBXOption;

const
  DefaultInstantIBXOptions = [];

type
  TInstantIBXConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FConnectionString: string;
    FOptions: TInstantIBXOptions;
    FParams: string;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    constructor Create(Collection: TCollection); override;
    function Edit: Boolean; override;
  published
    property ConnectionString: string read FConnectionString write FConnectionString;
    property Options: TInstantIBXOptions read FOptions write FOptions;
    property Params: string read FParams write FParams;
  end;

  TInstantIBXConnector = class(TInstantConnectionBasedConnector)
  private
    FTransaction: TIBTransaction;
    FOptions: TInstantIBXOptions;
    FOnLogin: TIBDatabaseLoginEvent;
    function GetConnection: TIBDatabase;
    function GetTransaction: TIBTransaction;
    procedure SetConnection(const Value: TIBDatabase);
  protected
    function CreateBroker: TInstantBroker; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
    procedure InternalCreateDatabase; override;
    procedure AssignLoginOptions; override;
    function GetDatabaseExists: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    property Transaction: TIBTransaction read GetTransaction;
  published
    property Connection: TIBDatabase read GetConnection write SetConnection;
    property Options: TInstantIBXOptions read FOptions write FOptions default DefaultInstantIBXOptions;
    property OnLogin: TIBDatabaseLoginEvent read FOnLogin write FOnLogin;
  end;

  TInstantIBXBroker = class(TInstantSQLBroker)
  private
    function GetDialect: Integer;
    function GetConnector: TInstantIBXConnector;
    function DelimitedIdentsEnabled: Boolean;
  protected
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    function GetSQLDelimiters: string; override;
    function GetSQLQuote: Char; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure AssignDataSetParams(DataSet : TDataSet; AParams: TParams;
      OnAssignParamValue: TAssignParamValue = nil); override;
  public
    function CreateDBBuildCommand(
      const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; override;
    function CreateDataSet(const AStatement: string; AParams: TParams = nil;
      OnAssignParamValue: TAssignParamValue = nil): TDataSet; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams = nil;
      OnAssignParamValue: TAssignParamValue = nil): Integer; override;
    property Connector: TInstantIBXConnector read GetConnector;
    property Dialect: Integer read GetDialect;
  end;

  TInstantIBXResolver = class(TInstantSQLResolver)
  protected
    function ReadBooleanField(DataSet: TDataSet; const FieldName: string;
      out AWasNull: boolean): Boolean; override;
  end;

  TInstantIBXTranslator = class(TInstantRelationalTranslator)
  protected
    function TranslateConstant(Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean; override;
  end;

  TInstantIBXQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

implementation

uses
  Vcl.Controls
  , InstantConsts
  , InstantIBXConnectionDefEdit
  , InstantUtils
  , IBX.IB
  , InstantIBFbCatalog
  , InstantDBBuild
  ;

{ TInstantIBXConnectionDef }

class function TInstantIBXConnectionDef.ConnectionTypeName: string;
begin
  Result := 'IBX';
end;

class function TInstantIBXConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantIBXConnector;
end;

constructor TInstantIBXConnectionDef.Create(Collection: TCollection);
begin
  inherited;
  FOptions := DefaultInstantIBXOptions;
end;

function TInstantIBXConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
var
  Connection: TIBDatabase;
begin
  Connection := TIBDatabase.Create(AOwner);
  try
    Connection.DatabaseName := ExpandDatabaseName(ConnectionString);
    Connection.SQLDialect := 3;
    Connection.Params.Text := Params;
  except
    Connection.Free;
    raise;
  end;
  Result := Connection;
end;

function TInstantIBXConnectionDef.Edit: Boolean;
begin
  with TInstantIBXConnectionDefEditForm.CreateForConnectionDef(nil, Self) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

procedure TInstantIBXConnectionDef.InitConnector(Connector: TInstantConnector);
begin
  inherited;
  (Connector as TInstantIBXConnector).Options := FOptions;
end;

{ TInstantIBXConnector }

procedure TInstantIBXConnector.AssignLoginOptions;
begin
  inherited;
  if HasConnection then
  begin
    if Assigned(FOnLogin) and not Assigned(Connection.OnLogin) then
      Connection.OnLogin := FOnLogin;
  end;
end;

class function TInstantIBXConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantIBXConnectionDef;
end;

constructor TInstantIBXConnector.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := DefaultInstantIBXOptions;
end;

function TInstantIBXConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantIBXBroker.Create(Self);
end;

destructor TInstantIBXConnector.Destroy;
begin
  FTransaction.Free;
  inherited;
end;

function TInstantIBXConnector.GetConnection: TIBDatabase;
begin
  Result := inherited Connection as TIBDatabase;
end;

function TInstantIBXConnector.GetTransaction: TIBTransaction;
begin
  if not Assigned(FTransaction) then
  begin
    CheckConnection;
    FTransaction := TIBTransaction.Create(nil);
    try
      FTransaction.DefaultDatabase := Connection;
      FTransaction.Params.Add('read_committed');
      {$IFDEF D6+}
      // AutoStopAction property from IBX 5.x is broken
      FTransaction.AutoStopAction := saCommit;
      {$ENDIF}
    except
      FreeAndNil(FTransaction);
      raise;
    end
  end;
  Result := FTransaction;
end;

procedure TInstantIBXConnector.InternalBuildDatabase(
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

procedure TInstantIBXConnector.InternalCommitTransaction;
begin
  if Transaction.InTransaction then
    Transaction.Commit;
end;

procedure TInstantIBXConnector.InternalRollbackTransaction;
begin
  if Transaction.InTransaction then
    Transaction.Rollback;
end;

procedure TInstantIBXConnector.InternalStartTransaction;
begin
  if not Transaction.InTransaction then
    Transaction.StartTransaction;
end;

procedure TInstantIBXConnector.SetConnection(const Value: TIBDatabase);
begin
  inherited Connection := Value;
end;

procedure TInstantIBXConnector.InternalCreateDatabase;
const
  CreateDatabaseParam =
   'USER ''%s'' PASSWORD ''%s'' PAGE_SIZE 4096 DEFAULT CHARACTER SET %s';
var
  OldConnectionParams: string;
  CharacterSetName: string;
begin
  inherited;
  OldConnectionParams := Connection.Params.Text;
  CharacterSetName := Connection.Params.Values['lc_ctype'];
  if CharacterSetName = '' then
    CharacterSetName := 'none';
  with Connection.Params do
    Text := Format(CreateDatabaseParam,
     [Values['user_name'], Values['password'], CharacterSetName]);
  try
    try
      Connection.CreateDatabase;
    finally
      Disconnect;
    end;
  finally
    Connection.Params.Text := OldConnectionParams;
  end;
end;

function TInstantIBXConnector.GetDatabaseExists: Boolean;
begin
  AssignLoginOptions;
  try
    Connection.Open;
    try
      Result := True;
    finally
      Connection.Close;
    end;
  except
    on E: EIBInterBaseError do begin
      if (E.SQLCode = -902) and (E.IBErrorCode = 335544344) then
        Result := False
      else
        raise;
    end;
  end;
end;

{ TInstantIBXBroker}

procedure TInstantIBXBroker.AssignDataSetParams(DataSet : TDataSet; AParams: TParams;
  OnAssignParamValue: TAssignParamValue);
var
  I: Integer;
  TargetParams : TParams;
  SourceParam, TargetParam: TParam;
begin
  //don't call inherited!
  TargetParams := TIBQuery(DataSet).Params;
  for I := 0 to Pred(AParams.Count) do
  begin
    SourceParam := AParams[I];
    if Assigned(OnAssignParamValue) then
      OnAssignParamValue(SourceParam);
    TargetParam := TargetParams.FindParam(SourceParam.Name);
    if Assigned(TargetParam) then
      case SourceParam.DataType of
        ftBoolean:
          TargetParam.AsInteger := Integer(SourceParam.AsBoolean);
      else
        TargetParam.Assign(SourceParam);
      end;
  end;
end;

function TInstantIBXBroker.CreateCatalog(
  const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := TInstantIBFbCatalog.Create(AScheme, Self);
end;

function TInstantIBXBroker.CreateDataSet(const AStatement: string;
  AParams: TParams = nil; OnAssignParamValue: TAssignParamValue = nil): TDataSet;
var
  Query: TIBQuery;
begin
  Query := TIBQuery.Create(nil);
  with Query do
  begin
    Database := Connector.Connection;
    Transaction := Connector.Transaction;
    SQL.Text := AStatement;
    if Assigned(AParams) then
      AssignDataSetParams(Query, AParams, OnAssignParamValue);
  end;
  Result := Query;
end;

function TInstantIBXBroker.CreateDBBuildCommand(
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

function TInstantIBXBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantIBXResolver.Create(Self, Map);
end;

function TInstantIBXBroker.DataTypeToColumnType(
  DataType: TInstantDataType; Size: Integer): string;
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
    'DATE',
    'TIME',
    'INTEGER');
begin
  Result := Types[DataType];
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantIBXBroker.DelimitedIdentsEnabled: Boolean;
begin
  Result := ibxUseDelimitedIdents in Connector.Options;
end;

function TInstantIBXBroker.Execute(const AStatement: string;
  AParams: TParams; OnAssignParamValue: TAssignParamValue): Integer;
var
  LQuery: TIBQuery;
begin
  {$if CompilerVersion < 32}Result := 0;{$endif}
  LQuery := AcquireDataSet(AStatement, AParams, OnAssignParamValue) as TIBQuery;
  try try
    LQuery.ExecSQL;
    Result := LQuery.RowsAffected;
  except
    on E: Exception do
    begin
      {$IF DEFINED(DEBUG) OR DEFINED(IO_CONSOLE)}
      raise EInstantError.CreateFmt(SSQLExecuteError,
        [AStatement, GetParamsStr(AParams), E.Message], E);
      {$ELSE}
      raise EInstantError.CreateFmt(SSQLExecuteErrorShort,
        [E.Message], E);
      {$ENDIF}
    end;
  end;
  finally
    ReleaseDataSet(LQuery);
  end;
end;

function TInstantIBXBroker.GetConnector: TInstantIBXConnector;
begin
  Result := inherited Connector as TInstantIBXConnector;
end;

function TInstantIBXBroker.GetDatabaseName: string;
begin
  Result := Connector.Connection.DatabaseName;
end;

function TInstantIBXBroker.GetDBMSName: string;
begin
  Result := 'InterBase';
end;

function TInstantIBXBroker.GetDialect: Integer;
begin
  Result := Connector.Connection.SQLDialect;
end;

function TInstantIBXBroker.GetSQLDelimiters: string;
begin
  if (Dialect = 3) and DelimitedIdentsEnabled() then
    Result := '""'
  else
    Result := inherited GetSQLDelimiters;
end;

function TInstantIBXBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

function TInstantIBXBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantIBXQuery.Create(Connector);
end;

{ TInstantIBXTranslator }

function TInstantIBXTranslator.TranslateConstant(
  Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean;
begin
  if SameText(Constant.Value, InstantTrueString) then
  begin
    Writer.WriteChar('1');
    Result := True;
  end else if SameText(Constant.Value, InstantFalseString) then
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

{ TInstantIBXQuery }

class function TInstantIBXQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantIBXTranslator;
end;

{ TInstantIBXResolver }

function TInstantIBXResolver.ReadBooleanField(DataSet: TDataSet;
  const FieldName: string; out AWasNull: boolean): Boolean;
var
  LField: TField;
begin
  LField := DataSet.FieldByName(FieldName);
  AWasNull := LField.IsNull;
  Result := Boolean(LField.AsInteger);
end;

initialization
  RegisterClass(TInstantIBXConnectionDef);
  TInstantIBXConnector.RegisterClass;

finalization
  TInstantIBXConnector.UnregisterClass;

end.

