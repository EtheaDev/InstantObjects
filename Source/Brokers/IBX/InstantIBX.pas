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
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantIBX;

interface

uses
  Classes, Db, IBDatabase, IBTable, IBQuery, SysUtils, InstantPersistence,
  InstantClasses, InstantCommand;

type
  TIBXNetType = (ntLocal, ntTCP, ntNetBEUI, ntSPX);

  TInstantIBXConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FPath: string;
    FServerName: string;
    FNetType: TIBXNetType;
    function GetDatabaseName: string;
    function GetServerName: string;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
  public
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    function Edit: Boolean; override;
    property DatabaseName: string read GetDatabaseName;
  published
    property Path: string read FPath write FPath;
    property NetType: TIBXNetType read FNetType write FNetType;
    property ServerName: string read GetServerName write FServerName;
  end;

  TInstantIBXConnector = class(TInstantConnectionBasedConnector)
  private
    FTransaction: TIBTransaction;
    function GetConnection: TIBDatabase;
    function GetTransaction: TIBTransaction;
    procedure SetConnection(const Value: TIBDatabase);
  protected
    function CreateBroker: TInstantBroker; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
  public
    destructor Destroy; override;
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    property Transaction: TIBTransaction read GetTransaction;
  published
    property Connection: TIBDatabase read GetConnection write SetConnection;
  end;

  TInstantIBXBroker= class(TInstantSQLBroker)
  private
    function GetDialect: Integer;
    function GetConnector: TInstantIBXConnector;
  protected
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    function GetSQLDelimiters: string; override;
    function GetSQLQuote: Char; override;
    function InternalCreateQuery: TInstantQuery; override;
  public
    function CreateDataSet(const AStatement: string; AParams: TParams = nil): TDataSet; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams = nil): Integer; override;
    property Connector: TInstantIBXConnector read GetConnector;
    property Dialect: Integer read GetDialect;
  end;

  TInstantIBXResolver = class(TInstantSQLResolver)
  protected
    function ReadBooleanField(DataSet: TDataSet; const FieldName: string): Boolean; override;
  end;

  TInstantIBXTranslator = class(TInstantRelationalTranslator)
  protected
    function TranslateConstant(Constant: TInstantIQLConstant; Writer: TInstantIQLWriter): Boolean; override;
  end;

  TInstantIBXQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

procedure Register;

implementation

uses
  Controls, InstantConsts, InstantIBXConnectionDefEdit, InstantUtils;

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantIBXConnector]);
end;

{ TInstantIBXConnectionDef }

class function TInstantIBXConnectionDef.ConnectionTypeName: string;
begin
  Result := 'IBX';
end;

class function TInstantIBXConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantIBXConnector;
end;

function TInstantIBXConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
var
  Connection: TIBDatabase;
begin
  Connection := TIBDatabase.Create(AOwner);
  try
    Connection.DatabaseName := DatabaseName;
    Connection.SQLDialect := 3;
  except
    Connection.Free;
    raise;
  end;
  Result := Connection;
end;

function TInstantIBXConnectionDef.Edit: Boolean;
begin
  with TInstantIBXConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

function TInstantIBXConnectionDef.GetDatabaseName: string;
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

function TInstantIBXConnectionDef.GetServerName: string;
begin
  if NetType = ntLocal then
    Result := ''
  else
    Result := FServerName;
end;

{ TInstantIBXConnector }

class function TInstantIBXConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantIBXConnectionDef;
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
    except
      FTransaction.Free;
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
  Transaction.Commit;
end;

procedure TInstantIBXConnector.InternalRollbackTransaction;
begin
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

{ TInstantIBXBroker}

function TInstantIBXBroker.CreateDataSet(const AStatement: string;
  AParams: TParams): TDataSet;
var
  Query: TIBQuery;

  procedure AssignParams(SourceParams, TargetParams: TParams);
  var
    I: Integer;
    SourceParam, TargetParam: TParam;
  begin
    for I := 0 to Pred(SourceParams.Count) do
    begin
      SourceParam := SourceParams[I];
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

begin
  Query := TIBQuery.Create(nil);
  with Query do
  begin
    Database := Connector.Connection;
    Transaction := Connector.Transaction;
    SQL.Text := AStatement;
    if Assigned(AParams) then
      AssignParams(AParams, Params);
  end;
  Result := Query;
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
    'SMALLINT',
    'VARCHAR',
    'BLOB',
    'TIMESTAMP',
    'BLOB');
begin
  Result := Types[DataType];
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantIBXBroker.Execute(const AStatement: string;
  AParams: TParams): Integer;
begin
  with CreateDataSet(AStatement, AParams) as TIBQuery do
  try
    ExecSQL;
    Result := RowsAffected;
  finally
    Free;
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
  if Dialect >= 3 then
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
  const FieldName: string): Boolean;
begin
  Result := Boolean(DataSet.FieldByName(FieldName).AsInteger);
end;

initialization
  RegisterClass(TInstantIBXConnectionDef);
  TInstantIBXConnector.RegisterClass;

finalization
  TInstantIBXConnector.UnregisterClass;

end.
