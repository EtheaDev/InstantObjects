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
 * ***** END LICENSE BLOCK ***** *)

unit InstantUIB;

{$I ..\..\Core\InstantDefines.inc}

{$IFDEF D7+}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

interface

uses
  Classes, Db, jvuib, jvuibdataset, jvuiblib, SysUtils, InstantUIBConnection,
  InstantPersistence, InstantClasses, InstantCommand, TypInfo;

type
  TInstantUIBOption = (uibUseDelimitedIdents);
  TInstantUIBOptions = set of TInstantUIBOption;

const
  DefaultInstantUIBOptions = [uibUseDelimitedIdents];

type
  TUIBNetType = (ntLocal, ntTCP, ntNetBEUI, ntSPX);

  TInstantUIBConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FPath: string;
    FServerName: string;
    FNetType: TUIBNetType;
    FOptions: TInstantUIBOptions;
    FParams: string;
    function GetDatabaseName: string;
    function GetServerName: string;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    constructor Create(Collection: TCollection); override;
    function Edit: Boolean; override;
    property DatabaseName: string read GetDatabaseName;
  published
    property Path: string read FPath write FPath;
    property NetType: TUIBNetType read FNetType write FNetType;    
    property ServerName: string read GetServerName write FServerName;
    property Options: TInstantUIBOptions read FOptions write FOptions;
    property Params: string read FParams write FParams;
  end;

  TInstantUIBConnector = class(TInstantConnectionBasedConnector)
  private
    FTransaction: TJvUIBTransaction;
    FOptions: TInstantUIBOptions;
    function GetConnection: TInstantUIBConnection;
    function GetTransaction: TJvUIBTransaction;
    procedure SetConnection(const Value: TInstantUIBConnection);
  protected
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
    property Transaction: TJvUIBTransaction read GetTransaction;
  published
    property Connection: TInstantUIBConnection read GetConnection write SetConnection;
    property Options: TInstantUIBOptions read FOptions write FOptions default DefaultInstantUIBOptions;
  end;

  TInstantUIBBroker= class(TInstantSQLBroker)
  private
    function GetDialect: Integer;
    function GetConnector: TInstantUIBConnector;
    function DelimitedIdentsEnabled: Boolean;
  protected
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    function GetSQLDelimiters: string; override;
    function GetSQLQuote: Char; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure PrepareQuery(DataSet : TDataSet); override;
    procedure UnprepareQuery(DataSet : TDataSet); override;
    function ExecuteQuery(DataSet : TDataSet) : integer; override;
    procedure AssignDataSetParams(DataSet : TDataSet; AParams: TParams); override;
  public
    function CreateDataSet(const AStatement: string; AParams: TParams = nil): TDataSet; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams = nil): Integer; override;
    property Connector: TInstantUIBConnector read GetConnector;
    property Dialect: Integer read GetDialect;
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

procedure Register;

implementation

uses
  Controls, InstantConsts, InstantUIBConnectionDefEdit, InstantUtils,
  IB, IBHeader, IBIntf;

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantUIBConnector]);
end;

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
  FOptions := DefaultInstantUIBOptions;
end;

function TInstantUIBConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
var
  Connection: TInstantUIBConnection;
begin
  Connection := TInstantUIBConnection.Create(AOwner);
  try
    Connection.Database.DatabaseName := DatabaseName;
    Connection.Database.SQLDialect := 3;
    Connection.Database.Params.Text := Params;
  except
    Connection.Free;
    raise;
  end;
  Result := Connection;
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

function TInstantUIBConnectionDef.GetDatabaseName: string;
var
  Fmt: string;
begin
  if NetType = ntLocal then
    Result := Path
  else begin
    case NetType of
      ntTCP: Fmt := '%s:%s';
      ntNetBEUI: Fmt := '\\%s\%s';
      ntSPX: Fmt := '%s@%s';
    end;
    Result := Format(Fmt, [ServerName, Path]);
  end;
end;

function TInstantUIBConnectionDef.GetServerName: string;
begin
  if NetType = ntLocal then
    Result := ''
  else
    Result := FServerName;
end;

procedure TInstantUIBConnectionDef.InitConnector(Connector: TInstantConnector);
begin
  inherited;
  (Connector as TInstantUIBConnector).Options := FOptions;
end;

{ TInstantUIBConnector }

class function TInstantUIBConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantUIBConnectionDef;
end;

constructor TInstantUIBConnector.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := DefaultInstantUIBOptions;
end;

function TInstantUIBConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantUIBBroker.Create(Self);
end;

destructor TInstantUIBConnector.Destroy;
begin
  FTransaction.Free;
  inherited;
end;

function TInstantUIBConnector.GetConnection: TInstantUIBConnection;
begin
  Result := inherited Connection as TInstantUIBConnection;
end;

function TInstantUIBConnector.GetTransaction: TJvUIBTransaction;
begin
  if not Assigned(FTransaction) then
  begin
    CheckConnection;
    FTransaction := TJvUIBTransaction.Create(nil);
    try
      FTransaction.Database := Connection.Database;
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

procedure TInstantUIBConnector.SetConnection(const Value: TInstantUIBConnection);
begin
  inherited Connection := Value;
end;

procedure TInstantUIBConnector.InternalCreateDatabase;
begin
  inherited;
  Connection.Close;
  try
    Connection.Database.CreateDatabase(4096);
  finally
    Connection.Close;
  end;
end;

function TInstantUIBConnector.GetDatabaseExists: Boolean;
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
    Result := False
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
    case SourceParam.DataType of
      ftString:
        TargetParams.ByNameAsString[SourceParam.Name] := SourceParam.AsString;
      ftInteger:
        TargetParams.ByNameAsInteger[SourceParam.Name] := SourceParam.AsInteger;
      ftFloat:
        TargetParams.ByNameAsDouble[SourceParam.Name] := SourceParam.AsFloat;
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

function TInstantUIBBroker.CreateDataSet(const AStatement: string;
  AParams: TParams): TDataSet;
var
  Query: TJvUIBDataSet;
begin
  Query := TJvUIBDataSet.Create(NIl);
  with Query do
  begin
    Database := Connector.Connection.Database;
    FetchBlobs := True;
    OnError := etmStayIn;
    OnClose := etmStayIn;
    SQL.Text := AStatement;
    Transaction := Connector.Transaction;
    UniDirectional := True;
    if Assigned(AParams) then
      AssignDataSetParams(Query, AParams);
  end;
  Result := Query;
end;

function TInstantUIBBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantUIBResolver.Create(Self, Map);
end;

function TInstantUIBBroker.DataTypeToColumnType(
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
    'BLOB');
begin
  Result := Types[DataType];
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantUIBBroker.DelimitedIdentsEnabled: Boolean;
begin
  Result := UIBUseDelimitedIdents in Connector.Options;
end;

function TInstantUIBBroker.Execute(const AStatement: string;
  AParams: TParams): Integer;
begin
  with CreateDataSet(AStatement, AParams) as TJvUIBDataSet do
  try
    Execute;
    Result := RowsAffected;
  finally
    Free;
  end;
end;

function TInstantUIBBroker.ExecuteQuery(DataSet: TDataSet) : integer;
begin
  //don't call inherited!
  with TJvUIBDataSet(DataSet) do
  begin
    Execute;
    Result := RowsAffected;
  end;
end;

function TInstantUIBBroker.GetConnector: TInstantUIBConnector;
begin
  Result := inherited Connector as TInstantUIBConnector;
end;

function TInstantUIBBroker.GetDatabaseName: string;
begin
  Result := Connector.Connection.Database.DatabaseName;
end;

function TInstantUIBBroker.GetDBMSName: string;
begin
  Result := 'InterBase';
end;

function TInstantUIBBroker.GetDialect: Integer;
begin
  Result := Connector.Connection.Database.SQLDialect;
end;

function TInstantUIBBroker.GetSQLDelimiters: string;
begin
  if (Dialect = 3) and DelimitedIdentsEnabled() then
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

procedure TInstantUIBBroker.PrepareQuery(DataSet: TDataSet);
begin
  inherited;
end;

procedure TInstantUIBBroker.UnprepareQuery(DataSet: TDataSet);
begin
  inherited;
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
