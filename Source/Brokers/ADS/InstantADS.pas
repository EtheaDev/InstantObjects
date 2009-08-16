(*
 *   InstantObjects
 *   Advantage Database Server Support
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
 * Carlo Barazzetta: blob streaming in XML format (Part, Parts, References)
 *
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantADS;

interface

uses
  Classes, DB, InstantPersistence, AdsData, AdsCnnct, AdsTable, AdsSet;

type
  TInstantADSConnectionType = (ctLocal, ctRemote, ctInternet);
  TInstantADSConnectionTypes = set of TInstantADSConnectionType;

  TInstantADSConnectionDef = class(TInstantRelationalConnectionDef)
  private
    FAlias: string;
    FConnectionTypes: TInstantADSConnectionTypes;
    FConnectPath: string;
    procedure SetAlias(const Value: string);
    procedure SetConnectPath(const Value: string);
    function GetDatabaseName: string;
  protected
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    property DatabaseName: string read GetDatabaseName;
  published
    property ConnectionTypes: TInstantADSConnectionTypes read FConnectionTypes write FConnectionTypes;
    property Alias: string read FAlias write SetAlias;
    property ConnectPath: string read FConnectPath write SetConnectPath;
  end;

  TInstantADSConnector = class(TInstantRelationalConnector)
  private
    FConnection: TADSConnection;
    function GetConnection: TADSConnection;
    procedure SetConnection(Value: TADSConnection);
  protected
    procedure CheckConnection;
    function CreateBroker: TInstantBroker; override;
    function GetConnected: Boolean; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalConnect; override;
    procedure InternalDisconnect; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    function AllowTransactions: Boolean;
    function HasConnection: Boolean;
  published
    property Connection: TADSConnection read GetConnection write SetConnection;
  end;

  TInstantADSBroker = class(TInstantRelationalBroker)
  private
    function GetConnector: TInstantADSConnector;
  protected
    function CreateResolver(const TableName: string): TInstantResolver; override;
    function InternalCreateQuery: TInstantQuery; override;
  public
    property Connector: TInstantADSConnector read GetConnector;
  end;

  TInstantADSResolver = class(TInstantResolver)
  private
    function GetDataSet: TADSTable;
    function GetBroker: TInstantADSBroker;
  protected
    function CreateDataSet: TDataSet; override;
    function Locate(const AClassName, AObjectId: string): Boolean; override;
  public
    property DataSet: TADSTable read GetDataSet;
    property Broker: TInstantADSBroker read GetBroker;
  end;

  TInstantADSTranslator = class(TInstantRelationalTranslator)
  protected
    function GetDelimiters: string; override;
    function GetQuote: Char; override;
    function IncludeOrderFields: Boolean; override;
  end;

  TInstantADSQuery = class(TInstantRelationalQuery)
  private
    function GetConnector: TInstantADSConnector;
    function GetQuery: TADSQuery;
  private
    FQuery: TADSQuery;
    property Query: TADSQuery read GetQuery;
  protected
    function GetDataSet: TDataSet; override;
    function GetParams: TParams; override;
    function GetStatement: string; override;
    procedure SetParams(Value: TParams); override;
    procedure SetStatement(const Value: string); override;
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  public
    destructor Destroy; override;
    property Connector: TInstantADSConnector read GetConnector;
  end;

procedure Register;

implementation

uses
  InstantADSConnectionDefEdit, InstantClasses, InstantConsts, SysUtils,
  Controls;

const
  SAdminUserName = 'ADSSYS';

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantADSConnector]);
end;

{ TInstantADSConnectionDef }

class function TInstantADSConnectionDef.ConnectionTypeName: string;
begin
  Result := 'ADS';
end;

class function TInstantADSConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantADSConnector;
end;

function TInstantADSConnectionDef.Edit: Boolean;
begin
  with TInstantADSConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

function TInstantADSConnectionDef.GetDatabaseName: string;
const
  LValidChars = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
var
  I: Integer;
begin
  Result := Name;
  for I := 1 to Length(Result) do
    if not (Result[I] in LValidChars) then
      Result[I] := '_';
end;

procedure TInstantADSConnectionDef.InitConnector(
  Connector: TInstantConnector);

  function ServerTypes: TADSServerTypes;
  begin
    Result := [];
    if ctLocal in ConnectionTypes then
      Include(Result, stADS_LOCAL);
    if ctRemote in ConnectionTypes then
      Include(Result, stADS_REMOTE);
    if ctInternet in ConnectionTypes then
      Include(Result, stADS_AIS);
  end;

var
  Connection: TADSConnection;
begin
  Connection := TADSConnection.Create(Connector);
  try
    Connection.Name := DatabaseName;
    if Alias <> '' then
      Connection.AliasName := Alias
    else
      Connection.ConnectPath := ConnectPath;
    Connection.AdsServerTypes := Servertypes;
    (Connector as TInstantADSConnector).Connection := Connection;
  except
    Connection.Free;
    raise;
  end;
end;

procedure TInstantADSConnectionDef.SetAlias(const Value: string);
begin
  if Value <> FAlias then
  begin
    FConnectPath := '';
    FAlias := Value;
  end;
end;

procedure TInstantADSConnectionDef.SetConnectPath(const Value: string);
begin
  if Value <> FConnectPath then
  begin
    FAlias := '';
    FConnectPath := Value;
  end;
end;

{ TInstantADSConnector }

function TInstantADSConnector.AllowTransactions: Boolean;
begin
  Result := not SameText(Connection.UserName, SAdminUserName);
end;

procedure TInstantADSConnector.CheckConnection;
begin
 {oif if not HasConnection then
    raise EInstantError.CreateRes(@SUnassignedConnection);}
end;

class function TInstantADSConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantADSConnectionDef;
end;

function TInstantADSConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantADSBroker.Create(Self);
end;

function TInstantADSConnector.GetConnected: Boolean;
begin
  if HasConnection then
    Result := Connection.IsConnected
  else
    Result := inherited GetConnected;
end;

function TInstantADSConnector.GetConnection: TADSConnection;
begin
  if not (csDesigning in ComponentState) then
    CheckConnection;
  Result := FConnection;
end;

function TInstantADSConnector.GetDatabaseName: string;
begin
  Result := Connection.AliasName;
end;

function TInstantADSConnector.GetDBMSName: string;
begin
  Result := 'ADS';
end;

function TInstantADSConnector.HasConnection: Boolean;
begin
  Result := Assigned(FConnection);
end;

procedure TInstantADSConnector.InternalBuildDatabase(Scheme: TInstantScheme);

  procedure CreateTable(TableMetadata: TInstantTableMetadata);
  const
    FieldTypes: array[TInstantDataType] of TFieldType =
      (ftInteger, ftFloat, ftBCD, ftBoolean, ftString, ftMemo, ftDateTime, ftBlob);
  var
    I: Integer;
    Table: TADSTable;
    IndexName: string;
  begin
    Table := TAdsTable.Create(nil);
    try
      Table.TableName := TableMetadata.Name;
      Table.AdsConnection := Connection;
      with TableMetadata do
      begin
        for I := 0 to Pred(IndexMetadatas.Count) do
          with IndexMetadatas[I] do
          begin
            IndexName := Name;
            if IndexName = '' then
              IndexName := Table.TableName + '_' + 'ID';
            Table.IndexDefs.Add(IndexName, Fields, Options);
          end;
        for I := 0 to Pred(FieldMetadatas.Count) do
          with FieldMetadatas[I] do
            Table.FieldDefs.Add(Name, FieldTypes[DataType], Size,
              foRequired in Options);
      end;
      if Table.Exists then
        Table.DeleteTable;
      Table.CreateTable;
    finally
      Table.Free;
    end;
  end;

var
  I: Integer;
begin
  inherited;
  if not Assigned(Scheme) then
    Exit;
  with Scheme do
    for I := 0 to Pred(TableMetadataCount) do
      CreateTable(TableMetadatas[I]);
end;

procedure TInstantADSConnector.InternalCommitTransaction;
begin
  if AllowTransactions then
    Connection.Commit;
end;

procedure TInstantADSConnector.InternalConnect;
begin
  CheckConnection;
  Connection.Connect;
end;

procedure TInstantADSConnector.InternalDisconnect;
begin
  if HasConnection then
    Connection.Disconnect;
end;

procedure TInstantADSConnector.InternalRollbackTransaction;
begin
  if AllowTransactions then
    Connection.Rollback;
end;

procedure TInstantADSConnector.InternalStartTransaction;
begin
  if AllowTransactions then
    Connection.BeginTransaction;
end;

procedure TInstantADSConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FConnection) and (Operation = opRemove) then
  begin
    Disconnect;
    FConnection := nil;
  end;
end;

procedure TInstantADSConnector.SetConnection(Value: TADSConnection);
begin
  if Value <> FConnection then
  begin
    Disconnect;
    if Assigned(FConnection) then
      FConnection.RemoveFreeNotification(Self);
    FConnection := Value;
    if Assigned(FConnection) then
      FConnection.FreeNotification(Self);
  end;
end;

{ TInstantADSBroker }

function TInstantADSBroker.CreateResolver(
  const TableName: string): TInstantResolver;
begin
  Result := TInstantADSResolver.Create(Self, TableName);
end;

function TInstantADSBroker.GetConnector: TInstantADSConnector;
begin
  Result := inherited Connector as TInstantADSConnector;
end;

function TInstantADSBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantADSQuery.Create(Connector);
end;

{ TInstantADSResolver }

function TInstantADSResolver.CreateDataSet: TDataSet;
begin
  Result:= TAdsTable.Create(nil);
  with TAdsTable(Result) do
  try
    AdsConnection := Broker.Connector.Connection;
    TableName := Self.TableName;
    IndexFieldNames := InstantIndexFieldNames;
  except
    Result.Free;
    raise;
  end;
end;

function TInstantADSResolver.GetBroker: TInstantADSBroker;
begin
  Result := inherited Broker as TInstantADSBroker;
end;

function TInstantADSResolver.GetDataSet: TAdsTable;
begin
  Result := inherited DataSet as TAdsTable;
end;

function TInstantADSResolver.Locate(const AClassName,
  AObjectId: string): Boolean;
begin
  Result := DataSet.FindKey([AClassName, AObjectId]);
end;

{ TInstantADSTranslator }

function TInstantADSTranslator.GetDelimiters: string;
begin
  Result := '""';
end;

function TInstantADSTranslator.GetQuote: Char;
begin
  Result := '''';
end;

function TInstantADSTranslator.IncludeOrderFields: Boolean;
begin
  Result := True;
end;

{ TInstantADSQuery }

destructor TInstantADSQuery.Destroy;
begin
  FQuery.Free;
  inherited;
end;

function TInstantADSQuery.GetConnector: TInstantADSConnector;
begin
  Result := inherited Connector as TInstantADSConnector;
end;

function TInstantADSQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TInstantADSQuery.GetParams: TParams;
begin
  Result := Query.Params;
end;

function TInstantADSQuery.GetQuery: TADSQuery;
begin
  if not Assigned(FQuery) then
  begin
    FQuery := TAdsQuery.Create(nil);
    FQuery.DatabaseName := Connector.Connection.Name;
  end;
  Result := FQuery;
end;

function TInstantADSQuery.GetStatement: string;
begin
  Result := Query.SQL.Text;
end;

procedure TInstantADSQuery.SetParams(Value: TParams);
begin
  Query.Params := Value;
end;

procedure TInstantADSQuery.SetStatement(const Value: string);
begin
  Query.SQL.Text := Value;
end;

class function TInstantADSQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantADSTranslator;
end;

initialization
  RegisterClass(TInstantADSConnectionDef);
  TInstantADSConnector.RegisterClass;

finalization
  TInstantADSConnector.UnregisterClass;

end.
