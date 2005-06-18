(*
 *   InstantObjects
 *   DBISAM Support
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
 * Carlo Barazzetta, Juan J. V. Garcia
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBISAM;

{$IFNDEF VER130}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Classes, DB, DBISAMTb, InstantPersistence, InstantCommand;

type
  TInstantDBISAMConnectionDef = class(TInstantRelationalConnectionDef)
  private
    FDirectory: string;
    FRemoteDatabase: string;
    FRemotePort: Integer;
    FRemoteHost: string;
    FRemoteType: TRemoteType;
    FSessionType: TSessionType;
  protected
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    constructor Create(Collection: TCollection); override;
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property Directory: string read FDirectory write FDirectory;
    property RemoteDatabase: string read FRemoteDatabase write FRemoteDatabase;
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    property RemotePort: Integer read FRemotePort write FRemotePort;
    property RemoteType: TRemoteType read FRemoteType write FRemoteType;
    property SessionType: TSessionType read FSessionType write FSessionType;
  end;

  TInstantDBISAMConnector = class(TInstantRelationalConnector)
  private
    FDatabase: TDBISAMDatabase;
    function GetDatabase: TDBISAMDatabase;
    function GetSession: TDBISAMSession;
    function GetSessionName: string;
    procedure SetDatabase(Value: TDBISAMDatabase);
  protected
    procedure CheckDatabase;
    function CreateBroker: TInstantBroker; override;
    function GetConnected: Boolean; override;
    function GetDatabaseName: string; override;
    function GetDatabaseExists: Boolean; override;
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
    function HasDatabase: Boolean;
    function HasSession: Boolean;
    property Session: TDBISAMSession read GetSession;
    property SessionName: string read GetSessionName;
  published
    property Database: TDBISAMDatabase read GetDatabase write SetDatabase;
  end;

  TInstantDBISAMBroker = class(TInstantRelationalBroker)
  private
    function GetConnector: TInstantDBISAMConnector;
  protected
    function CreateResolver(const TableName: string): TInstantResolver; override;
    function InternalCreateQuery: TInstantQuery; override;
  public
    property Connector: TInstantDBISAMConnector read GetConnector;
  end;

  TInstantDBISAMResolver = class(TInstantResolver)
  private
    function GetDataSet: TDBISAMTable;
    function GetBroker: TInstantDBISAMBroker;
  protected
    function CreateDataSet: TDataSet; override;
    function Locate(const AClassName, AObjectId: string): Boolean; override;
  public
    property DataSet: TDBISAMTable read GetDataSet;
    property Broker: TInstantDBISAMBroker read GetBroker;
  end;

  TInstantDBISAMTranslator = class(TInstantRelationalTranslator)
  protected
    function IncludeOrderFields: Boolean; override;
  end;

  TInstantDBISAMQuery = class(TInstantRelationalQuery)
  private
    function GetConnector: TInstantDBISAMConnector;
    function GetQuery: TDBISAMQuery;
  private
    FQuery: TDBISAMQuery;
    property Query: TDBISAMQuery read GetQuery;
  protected
    function GetDataSet: TDataSet; override;
    function GetParams: TParams; override;
    function GetStatement: string; override;
    procedure SetParams(Value: TParams); override;
    procedure SetRowNumber(Value: Integer); override;
    procedure SetStatement(const Value: string); override;
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  public
    destructor Destroy; override;
    property Connector: TInstantDBISAMConnector read GetConnector;
  end;

procedure Register;

implementation

uses
  InstantDBISAMConnectionDefEdit, InstantClasses, InstantConsts, SysUtils,
  Controls;

resourcestring
  SUnassignedDatabase = 'Unassigned database';

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantDBISAMConnector]);
end;

{ TInstantDBISAMConnectionDef }

class function TInstantDBISAMConnectionDef.ConnectionTypeName: string;
begin
  Result := 'DBISAM';
end;

class function TInstantDBISAMConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantDBISAMConnector;
end;

constructor TInstantDBISAMConnectionDef.Create;
begin
  inherited;
  FRemoteHost := '127.0.0.1';
  FRemotePort := 12001;
end;

function TInstantDBISAMConnectionDef.Edit: Boolean;
begin
  with TInstantDBISAMConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

procedure TInstantDBISAMConnectionDef.InitConnector(
  Connector: TInstantConnector);
var
  Database: TDBISAMDatabase;
  Session: TDBISAMSession;
begin
  Database := TDBISAMDatabase.Create(Connector);
  try
    Database.DatabaseName := Name;
    Database.Directory := Directory;
    Database.RemoteDatabase := RemoteDatabase;
    Session := TDBISAMSession.Create(Connector);
    try
      Session.SessionName := Name;
      Session.RemoteHost := RemoteHost;
      Session.RemotePort := RemotePort;
      Session.RemoteType := RemoteType;
      Session.SessionType := SessionType;
      Database.SessionName := Session.SessionName;
     (Connector as TInstantDBISAMConnector).Database := Database;
    except
      Session.Free;
      raise;
    end;
  except
    Database.Free;
    raise;
  end;
end;

{ TInstantDBISAMConnector }

procedure TInstantDBISAMConnector.CheckDatabase;
begin
  if not HasDatabase then
    raise EInstantError.CreateRes(@SUnassignedDatabase);
end;

class function TInstantDBISAMConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantDBISAMConnectionDef;
end;

function TInstantDBISAMConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantDBISAMBroker.Create(Self);
end;

function TInstantDBISAMConnector.GetConnected: Boolean;
begin
  if HasDatabase then
    Result := Database.Connected
  else
    Result := inherited GetConnected;
end;

function TInstantDBISAMConnector.GetDatabase: TDBISAMDatabase;
begin
  if not (csDesigning in ComponentState) then
    CheckDatabase;
  Result := FDatabase;
end;

function TInstantDBISAMConnector.GetDatabaseExists: Boolean;
var
  SearchRec: TSearchRec;
begin
  if HasSession and (Session.SessionType = stLocal) then
  begin
    Result := FindFirst(IncludeTrailingBackslash(Database.Directory) + '*.*',
      faReadOnly + faArchive, SearchRec) = 0;
    FindClose(SearchRec);
  end else
    Result := inherited GetDatabaseExists;
end;

function TInstantDBISAMConnector.GetDatabaseName: string;
begin
  if HasDatabase then
    Result := Database.DatabaseName
  else
    inherited GetDatabaseName;
end;

function TInstantDBISAMConnector.GetDBMSName: string;
begin
  Result := 'DBISAM';
end;

function TInstantDBISAMConnector.GetSession: TDBISAMSession;
begin
  if HasDatabase then
    Result := Database.Session
  else
    Result := nil;
end;

function TInstantDBISAMConnector.GetSessionName: string;
begin
  if HasSession then
    Result := Session.SessionName
  else
    Result := '';
end;

function TInstantDBISAMConnector.HasDatabase: Boolean;
begin
  Result := Assigned(FDatabase);
end;

function TInstantDBISAMConnector.HasSession: Boolean;
begin
  Result := Assigned(Session);
end;

procedure TInstantDBISAMConnector.InternalBuildDatabase(Scheme: TInstantScheme);

  procedure CreateTable(TableMetadata: TInstantTableMetadata);
  const
    FieldTypes: array[TInstantDataType] of TFieldType = (
      ftInteger, ftFloat, ftCurrency, ftBoolean, ftString, ftMemo, ftDateTime,
      ftBlob);
  var
    I: Integer;
    Table: TDBISAMTable;
    IndexName: string;
  begin
    Table := TDBISAMTable.Create(nil);
    try
      Table.TableName := TableMetadata.Name;
      Table.DatabaseName := DatabaseName;
      Table.SessionName := SessionName;
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

procedure TInstantDBISAMConnector.InternalCommitTransaction;
begin
  Database.Commit;
end;

procedure TInstantDBISAMConnector.InternalConnect;
begin
  CheckDatabase;
  Database.Open;
end;

procedure TInstantDBISAMConnector.InternalDisconnect;
begin
  if HasDatabase then
    Database.Close;
end;

procedure TInstantDBISAMConnector.InternalRollbackTransaction;
begin
  Database.Rollback;
end;

procedure TInstantDBISAMConnector.InternalStartTransaction;
begin
  Database.StartTransaction;
end;

procedure TInstantDBISAMConnector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FDatabase) and (Operation = opRemove) then
  begin
    Disconnect;
    FDatabase := nil;
  end;
end;

procedure TInstantDBISAMConnector.SetDatabase(Value: TDBISAMDatabase);
begin
  if Value <> FDatabase then
  begin
    Disconnect;
    if Assigned(FDatabase) then
      FDatabase.RemoveFreeNotification(Self);
    FDatabase := Value;
    if Assigned(FDatabase) then
      FDatabase.FreeNotification(Self);
  end;
end;

{ TInstantDBISAMBroker }

function TInstantDBISAMBroker.CreateResolver(
  const TableName: string): TInstantResolver;
begin
  Result := TInstantDBISAMResolver.Create(Self, TableName);
end;

function TInstantDBISAMBroker.GetConnector: TInstantDBISAMConnector;
begin
  Result := inherited Connector as TInstantDBISAMConnector;
end;

function TInstantDBISAMBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantDBISAMQuery.Create(Connector);
end;

{ TInstantDBISAMResolver }

function TInstantDBISAMResolver.CreateDataSet: TDataSet;
begin
  Result:= TDBISAMTable.Create(nil);
  with TDBISAMTable(Result) do
  try
    DatabaseName := Broker.Connector.DatabaseName;
    SessionName := Broker.Connector.SessionName;
    TableName := Self.TableName;
    IndexFieldNames := InstantIndexFieldNames;
  except
    Result.Free;
    raise;
  end;
end;

function TInstantDBISAMResolver.GetBroker: TInstantDBISAMBroker;
begin
  Result := inherited Broker as TInstantDBISAMBroker;
end;

function TInstantDBISAMResolver.GetDataSet: TDBISAMTable;
begin
  Result := inherited DataSet as TDBISAMTable;
end;

function TInstantDBISAMResolver.Locate(const AClassName,
  AObjectId: string): Boolean;
begin
  Result := DataSet.FindKey([AClassName, AObjectId]);
end;

{ TInstantDBISAMQuery }

destructor TInstantDBISAMQuery.Destroy;
begin
  FQuery.Free;
  inherited;
end;

function TInstantDBISAMQuery.GetConnector: TInstantDBISAMConnector;
begin
  Result := inherited Connector as TInstantDBISAMConnector;
end;

function TInstantDBISAMQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TInstantDBISAMQuery.GetParams: TParams;
begin
  Result := Query.Params;
end;

function TInstantDBISAMQuery.GetQuery: TDBISAMQuery;
begin
  if not Assigned(FQuery) then
  begin
    FQuery := TDBISAMQuery.Create(nil);
    FQuery.DatabaseName := Connector.DatabaseName;
    FQuery.SessionName := Connector.SessionName;
  end;
  Result := FQuery;
end;

function TInstantDBISAMQuery.GetStatement: string;
begin
  Result := Query.SQL.Text;
end;

procedure TInstantDBISAMQuery.SetParams(Value: TParams);
begin
  Query.Params := Value;
end;

procedure TInstantDBISAMQuery.SetRowNumber(Value: Integer);
begin
  inherited SetRowNumber(Value);

  { Workaround for RecNo issue with DBISAM 3.07 query }
  if RowNumber <> Value then
  begin
    inherited SetRowNumber(Pred(Value));
    Query.Next;
  end;
end;

procedure TInstantDBISAMQuery.SetStatement(const Value: string);
begin
  Query.SQL.Text := Value;
end;

class function TInstantDBISAMQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantDBISAMTranslator;
end;

{ TInstantDBISAMTranslator }

function TInstantDBISAMTranslator.IncludeOrderFields: Boolean;
begin
  Result := True;
end;

initialization
  RegisterClass(TInstantDBISAMConnectionDef);
  TInstantDBISAMConnector.RegisterClass;

finalization
  TInstantDBISAMConnector.UnregisterClass;

end.
