(*
 *   InstantObjects
 *   UIB Support
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
 * The Original Code is: InstantObjects UIB Support
 *
 * The Initial Developer of the Original Code is: Andrea Petrelli
 *
 * Contributor(s):
 * Carlo Barazzetta, Nando Dessena, Joao Morais
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantUIB;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Classes, Db, jvuib, jvuibdataset, jvuiblib, SysUtils,
  InstantPersistence, InstantClasses, InstantCommand;

type
  TInstantUIBOption = (uibUseDelimitedIdents);
  TInstantUIBOptions = set of TInstantUIBOption;

const
  DefaultInstantUIBOptions = [];

type
  TInstantUIBConnectionDef = class(TInstantRelationalConnectionDef)
  private
    FLoginPrompt: Boolean;
    FConnectionString: string;
    FOptions: TInstantUIBOptions;
    FParams: string;
  protected
    function CreateDataBase(AOwner: TComponent): TJvUIBDataBase;
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    constructor Create(Collection: TCollection); override;
    function Edit: Boolean; override;
  published
    property ConnectionString: string read FConnectionString write FConnectionString;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt default True;
    property Options: TInstantUIBOptions read FOptions write FOptions;
    property Params: string read FParams write FParams;
  end;

  TInstantUIBConnector = class(TInstantRelationalConnector)
  private
    FDataBase: TJvUIBDataBase;
    FTransaction: TJvUIBTransaction;
    FOptions: TInstantUIBOptions;
    FLoginPrompt: Boolean;
    function GetDataBase: TJvUIBDataBase;
    function GetTransaction: TJvUIBTransaction;
    procedure SetDataBase(const Value: TJvUIBDataBase);
    procedure DataBaseLogin;
  protected
    procedure CheckDataBase;
    function GetConnected: Boolean; override;
    procedure InternalConnect; override;
    procedure InternalDisconnect; override;
    function CreateBroker: TInstantBroker; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
    procedure InternalCreateDatabase; override;
    function GetDatabaseExists: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Transaction: TJvUIBTransaction read GetTransaction;
    function HasDataBase: Boolean;
  published
    property DataBase: TJvUIBDataBase read GetDataBase write SetDataBase;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
    property Options: TInstantUIBOptions read FOptions write FOptions default DefaultInstantUIBOptions;
  end;

  TInstantUIBBroker = class(TInstantSQLBroker)
  private
    function GetSQLDialect: Integer;
    function GetConnector: TInstantUIBConnector;
    function DelimitedIdentsEnabled: Boolean;
  protected
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    function GetSQLDelimiters: string; override;
    function GetSQLQuote: Char; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure AssignDataSetParams(DataSet : TDataSet; AParams: TParams); override;
  public
    function CreateDBBuildCommand(
      const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand; override;
    function CreateDataSet(const AStatement: string; AParams: TParams = nil): TDataSet; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams = nil): Integer; override;
    property Connector: TInstantUIBConnector read GetConnector;
    property SQLDialect: Integer read GetSQLDialect;
  end;

  TInstantUIBResolver = class(TInstantSQLResolver)
  protected
    function ReadBooleanField(DataSet: TDataSet; const FieldName: string): Boolean; override;
  end;

  TInstantUIBTranslator = class(TInstantRelationalTranslator)
  protected
    function TranslateConstant(Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean; override;
  end;

  TInstantUIBQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
    procedure InternalOpen; override;
  end;

implementation

uses
  Controls, InstantConsts, InstantUIBConnectionDefEdit, InstantUtils,
  TypInfo, InstantDBBuild, InstantIBFbCatalog, DbLogDlg;

{ TInstantUIBConnectionDef }

class function TInstantUIBConnectionDef.ConnectionTypeName: string;
begin
  Result := 'UIB';
end;

class function TInstantUIBConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantUIBConnector;
end;

constructor TInstantUIBConnectionDef.Create(Collection: TCollection);
begin
  inherited;
  FLoginPrompt := True;
  FOptions := DefaultInstantUIBOptions;
end;

function TInstantUIBConnectionDef.CreateDataBase(
  AOwner: TComponent): TJvUIBDataBase;
begin
  Result := TJvUIBDataBase.Create(AOwner);
  try
    Result.Params.Text := Params;
    Result.DatabaseName := ConnectionString;
    Result.SQLDialect := 3;
  except
    Result.Free;
    raise;
  end;
end;

function TInstantUIBConnectionDef.Edit: Boolean;
begin
  with TInstantUIBConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

procedure TInstantUIBConnectionDef.InitConnector(Connector: TInstantConnector);
var
  DataBase: TJvUIBDataBase;
begin
  inherited;
  DataBase := CreateDatabase(Connector);
  try
    (Connector as TInstantUIBConnector).DataBase := DataBase;
    (Connector as TInstantUIBConnector).LoginPrompt := LoginPrompt;
    (Connector as TInstantUIBConnector).Options := FOptions;
  except
    DataBase.Free;
    raise;
  end;
end;

{ TInstantUIBConnector }

class function TInstantUIBConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantUIBConnectionDef;
end;

constructor TInstantUIBConnector.Create(AOwner: TComponent);
begin
  inherited;
  FLoginPrompt := True;
  FOptions := DefaultInstantUIBOptions;
end;

function TInstantUIBConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantUIBBroker.Create(Self);
end;

destructor TInstantUIBConnector.Destroy;
begin
  FreeAndNil(FTransaction);
  inherited;
end;

function TInstantUIBConnector.GetDataBase: TJvUIBDataBase;
begin
  if not (csDesigning in ComponentState) then
    CheckDataBase;
  Result := FDataBase;
end;

function TInstantUIBConnector.GetTransaction: TJvUIBTransaction;
begin
  if not Assigned(FTransaction) then
  begin
    CheckDataBase;
    FTransaction := TJvUIBTransaction.Create(nil);
    try
      FTransaction.DataBase := FDatabase;
      FTransaction.AutoStart := True;
      FTransaction.AutoStop := True;
      FTransaction.DefaultAction := etmRollback;
      FTransaction.Options := [tpReadCommitted];
    except
      FTransaction.Free;
      raise;
    end
  end;
  Result := FTransaction;
end;

procedure TInstantUIBConnector.InternalBuildDatabase(
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

procedure TInstantUIBConnector.InternalCommitTransaction;
begin
  Transaction.Commit;
end;

procedure TInstantUIBConnector.InternalRollbackTransaction;
begin
  Transaction.Rollback;
end;

procedure TInstantUIBConnector.InternalStartTransaction;
begin
  if not Transaction.InTransaction then
    Transaction.StartTransaction;
end;

procedure TInstantUIBConnector.SetDataBase(const Value: TJvUIBDataBase);
begin
  if Value <> FDataBase then
  begin
    Disconnect;
    if Assigned(FDataBase) then
      FDataBase.RemoveFreeNotification(Self);
    FDataBase := Value;
    if Assigned(FDataBase) then
      FDataBase.FreeNotification(Self);
  end;
end;

procedure TInstantUIBConnector.InternalCreateDatabase;
begin
  inherited;
  if DataBase.Connected then
    raise EInstantError.Create(SDatabaseOpen);
  try
    DataBase.CreateDatabase(4096);
  finally
    Disconnect;
  end;
end;

function TInstantUIBConnector.GetDatabaseExists: Boolean;
begin
  try
    DataBase.Connected := True;
    try
      Result := True;
    finally
      DataBase.Connected := False;
    end;
  except
    on E: EUIBError do begin
      if (E.SQLCode = -902) and (E.ErrorCode = 24) then
        Result := False
      else
        raise;
    end;
  end;
end;

procedure TInstantUIBConnector.CheckDataBase;
begin
  if not Assigned(FDataBase) then
    raise EInstantError.Create(SUnassignedConnection);
end;

function TInstantUIBConnector.GetConnected: Boolean;
begin
  if HasDataBase then
    Result := DataBase.Connected
  else
    Result := inherited GetConnected;
end;

procedure TInstantUIBConnector.InternalConnect;
begin
  CheckDataBase;
  if FLoginPrompt and not DataBase.Connected then
    DataBaseLogin;
  DataBase.Connected := True;
end;

procedure TInstantUIBConnector.InternalDisconnect;
begin
  if HasDataBase then
    FDataBase.Connected := False;
end;

procedure TInstantUIBConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FDataBase) and (Operation = opRemove) then
    FDataBase := nil;
end;

function TInstantUIBConnector.HasDataBase: Boolean;
begin
  Result := Assigned(FDataBase);
end;

procedure TInstantUIBConnector.DataBaseLogin;
var
  LUserName, LPassWord: string;
begin
  LUserName := DataBase.UserName;
  LPassWord := DataBase.PassWord;
  if LoginDialogEx(FDataBase.DatabaseName, LUserName, LPassWord, False) then
  begin
    FDataBase.UserName := LUserName;
    FDataBase.PassWord := LPassWord;
  end;
end;

{ TInstantUIBBroker}

procedure TInstantUIBBroker.AssignDataSetParams(DataSet: TDataSet; AParams: TParams);
var
  I: Integer;
  BlobContent: string;
  SourceParam: TParam;
  TargetParams: TSQLParams;
begin
  //don't call inherited!
  TargetParams := TJvUIBDataset(DataSet).Params;
  for I := 0 to Pred(AParams.Count) do
  begin
    SourceParam := AParams[I];
    if SourceParam.IsNull then
    begin
      TargetParams.ByNameIsNull[SourceParam.Name] := True;
    end
    else
    begin
      case SourceParam.DataType of
        ftString:
          TargetParams.ByNameAsString[SourceParam.Name] := SourceParam.AsString;
        ftInteger:
          TargetParams.ByNameAsInteger[SourceParam.Name] := SourceParam.AsInteger;
        ftFloat:
          TargetParams.ByNameAsDouble[SourceParam.Name] := SourceParam.AsFloat;
        ftCurrency:
          TargetParams.ByNameAsCurrency[SourceParam.Name] := SourceParam.AsCurrency;
        ftDateTime:
          TargetParams.ByNameAsDateTime[SourceParam.Name] := SourceParam.AsDateTime;
        ftBoolean:
          TargetParams.ByNameAsBoolean[SourceParam.Name] := SourceParam.AsBoolean;
        ftBlob, ftMemo:
        begin
          BlobContent := SourceParam.AsString;
          TJvUIBDataset(DataSet).ParamsSetBlob(SourceParam.Name, BlobContent);
        end;
      else
        raise Exception.Create('Parameter data type not supported: ' +
          GetEnumName(TypeInfo(TFieldType), Ord(SourceParam.DataType)));
      end;
    end;  
  end;
end;

const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'DOUBLE PRECISION',
    'DECIMAL(14,4)',
    'SMALLINT',
    'VARCHAR',
    'BLOB SUB_TYPE 1',
    'TIMESTAMP',
    'BLOB');

function TInstantUIBBroker.DataTypeToColumnType(
  DataType: TInstantDataType; Size: Integer): string;
begin
  Result := Types[DataType];
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantUIBBroker.CreateDataSet(const AStatement: string;
  AParams: TParams): TDataSet;
var
  Query: TJvUIBDataSet;
begin
  Query := TJvUIBDataSet.Create(nil);
  try
    Query.Database := Connector.DataBase;
    Query.FetchBlobs := True;
    Query.OnError := etmStayIn;
    Query.OnClose := etmStayIn;
    Query.SQL.Text := AStatement;
    Query.Transaction := Connector.Transaction;
    Query.UniDirectional := True;
    if Assigned(AParams) then
      AssignDataSetParams(Query, AParams);
    Result := Query;
  except
    Query.Free;
    raise;
  end;
end;

function TInstantUIBBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantUIBResolver.Create(Self, Map);
end;

function TInstantUIBBroker.DelimitedIdentsEnabled: Boolean;
begin
  Result := UIBUseDelimitedIdents in Connector.Options;
end;

function TInstantUIBBroker.Execute(const AStatement: string;
  AParams: TParams): Integer;
var
  DataSet: TJvUIBDataSet;
begin
  DataSet := AcquireDataSet(AStatement, AParams) as TJvUIBDataSet;
  try
    DataSet.Execute;
    Result := DataSet.RowsAffected;
  finally
    ReleaseDataSet(DataSet);
  end;
end;

function TInstantUIBBroker.GetConnector: TInstantUIBConnector;
begin
  Result := inherited Connector as TInstantUIBConnector;
end;

function TInstantUIBBroker.GetDatabaseName: string;
begin
  Result := Connector.DataBase.DatabaseName;
end;

function TInstantUIBBroker.GetDBMSName: string;
begin
  Result := 'InterBase';
end;

function TInstantUIBBroker.GetSQLDialect: Integer;
begin
  Result := Connector.DataBase.SQLDialect;
end;

function TInstantUIBBroker.GetSQLDelimiters: string;
begin
  if (SQLDialect = 3) and DelimitedIdentsEnabled() then
    Result := '""'
  else
    Result := inherited GetSQLDelimiters;
end;

function TInstantUIBBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

function TInstantUIBBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantUIBQuery.Create(Connector);
end;

function TInstantUIBBroker.CreateDBBuildCommand(
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

function TInstantUIBBroker.CreateCatalog(
  const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := TInstantIBFbCatalog.Create(AScheme, Self);
end;

{ TInstantUIBTranslator }

function TInstantUIBTranslator.TranslateConstant(
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
  end else
    Result := inherited TranslateConstant(Constant, Writer);
end;

{ TInstantUIBQuery }

procedure TInstantUIBQuery.InternalOpen;
begin
  inherited;
end;

class function TInstantUIBQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantUIBTranslator;
end;

{ TInstantUIBResolver }

function TInstantUIBResolver.ReadBooleanField(DataSet: TDataSet;
  const FieldName: string): Boolean;
begin
  Result := Boolean(DataSet.FieldByName(FieldName).AsInteger);
end;

initialization
  RegisterClass(TInstantUIBConnectionDef);
  TInstantUIBConnector.RegisterClass;

finalization
  TInstantUIBConnector.UnregisterClass;

end.
