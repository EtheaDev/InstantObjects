(*
 *   InstantObjects
 *   NexusDb Support
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
 * The Original Code is: Bert Moorthaemer
 *
 * The Initial Developer of the Original Code is: Bert Moorthaemer
 *
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 * 
 * ***** END LICENSE BLOCK ***** *)

unit InstantNexusDb;

{$I ../../InstantDefines.inc}
{$I InstantNxDbDefines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, DB, InstantPersistence, InstantCommand,
  nxllTransport, nxsdServerEngine, nxdb, nxsdDataDictionary
  ;

type
  TNexusDbTable = class(TnxTable)
  end;

  TNexusDbQuery = class(TnxQuery)
  protected
    procedure SetRecNo(Value: Integer); override;
  end;

  TInstantNexusDbProtocolType = (ptTCPIP, ptNamedPipes);

  TInstantNexusDbBaseConnectionDef = class(TInstantRelationalConnectionDef)
  private
    FAlias: string;
    FAliasIsPath: Boolean;
  protected
    class function CreateSession(aOwner: TComponent;
      aServerEngine: TnxBaseServerEngine): TnxSession;
    class function CreateDatabase(aOwner: TComponent;
      aSession: TnxSession; const aAlias: string;
      aAliasIsPath: Boolean): TnxDatabase;
  published
    property Alias: string
      read FAlias write FAlias;
    property AliasIsPath: Boolean
      read FAliasIsPath write FAliasIsPath;
  end;

 { SQL based (Remote) --------------------------------------------------------- }

  TInstantNexusDbSQLConnectionDef = class(TInstantNexusDbBaseConnectionDef)
  private
    FPort: Integer;
    FProtocolType: TInstantNexusDbProtocolType;
    FServerName: string;
  protected
    class function CreateServerEngine(aOwner: TComponent; const aServerName:
        string; aTransport: TnxBaseTransport): TnxBaseServerEngine;
    class function CreateTransport(aOwner: TComponent; aProtocolType:
        TInstantNexusDbProtocolType; const aServerName: string; aPort: Integer):
        TnxBaseTransport;
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    constructor Create(Collection: TCollection); override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    function Edit: Boolean; override;
    class procedure LoadAliasList(aProtocolType: TInstantNexusDbProtocolType; const
        aServerName: string; aPort: Integer; aList: TStrings);
    class procedure LoadServerList(aProtocolType: TInstantNexusDbProtocolType;
        aPort: Integer; aList: TStrings);
  published
    property Port: Integer read FPort write FPort default 16000;
    property ProtocolType: TInstantNexusDbProtocolType read FProtocolType write
        FProtocolType default ptTCPIP;
    property ServerName: string read FServerName write FServerName;
  end;

  TInstantNexusDbSQLConnector = class(TInstantRelationalConnector)
  private
    FSession: TnxSession;
    FDatabase: TnxDatabase;

{$IFDEF NX1}
    procedure DatabaseBuildFixup;
    procedure FillFieldMap(aTbl: TNexusDbTable; aList: TStrings);
    function CreateFixedPrimaryIndexDef(aTbl: TNexusDbTable; aDict:
      TnxDataDictionary; var aOldIdx: Integer): TnxIndexDescriptor;
    procedure DoTableFix(aTbl: TNexusDbTable);
{$ENDIF}
    procedure SetSession(Value: TnxSession);
    procedure SetDatabase(Value: TnxDatabase);
  protected
    procedure Notification(aComponent: TComponent; Operation: TOperation);
      override;
    function CreateBroker: TInstantBroker; override;
    function GetConnected: Boolean; override;
    function GetDatabaseExists: Boolean; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    function GetDDLTransactionSupported: Boolean; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalConnect; override;
    procedure InternalDisconnect; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
  published
    property Session: TnxSession
      read FSession write SetSession;
    property Database: TnxDatabase
      read FDatabase write SetDatabase;
  end;

  TInstantNexusDbSQLBroker = class(TInstantSQLBroker)
  private
    function GetConnector: TInstantNexusDbSQLConnector;
  protected
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver;
      override;
    function GetDBMSName: string; override;
    function GetSQLDelimiters: string; override;
    function GetSQLQuote: Char; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure AssignDataSetParams(DataSet: TDataSet; aParams: TParams);
      override;
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog;
        override;
  public
    function CreateDataSet(const aStatement: string;
      aParams: TParams = nil): TDataSet; override;
    function CreateDBBuildCommand(const CommandType: TInstantDBBuildCommandType):
        TInstantDBBuildCommand; override;
    function DataTypeToColumnType(DataType: TInstantDataType;
      Size: Integer): string; override;
    function Execute(const aStatement: string;
      aParams: TParams = nil): Integer; override;
    class function GeneratorClass: TInstantSQLGeneratorClass; override;
    property Connector: TInstantNexusDbSQLConnector
      read GetConnector;
  end;

  TInstantNexusDbSQLResolver = class(TInstantSQLResolver);

  TInstantNexusDbSQLTranslator = class(TInstantRelationalTranslator)
  protected
    function GetDelimiters: string; override;
    function GetQuote: Char; override;
    function IncludeOrderFields: Boolean; override;
  end;

  TInstantNexusDbSQLQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

implementation

uses
  SysUtils,
  Controls,
  Forms,
  InstantConsts,
  InstantUtils,
  InstantNexusDbConnectionDefEdit,
  nxllTypes,
  nxsdTypes,
  nxdbBase,
  nxtwWinsockTransport,
  nxtnNamedPipeTransport,
  nxreRemoteServerEngine,
  InstantNxCatalog, 
  InstantDBBuild;

const
  SUndefined = 'Undefined';

  { common code }

  { TNexusDbQuery }

procedure TNexusDbQuery.SetRecNo(Value: Integer);
begin
  inherited;
  if Value = Succ(RecNo) then
    Next
  else if Value = Pred(RecNo) then
    Prior;
end;

{ TTInstantNexusDbBaseConnectionDef }

class function TInstantNexusDbBaseConnectionDef.CreateSession(aOwner:
  TComponent;
  aServerEngine: TnxBaseServerEngine): TnxSession;
begin
  Result := TnxSession.Create(aOwner);
  with Result do
  try
    ServerEngine := aServerEngine;
    Active := True;
  except
    if Assigned(Result) then
      FreeAndNil(Result);
    raise;
  end;
end;

class function TInstantNexusDbBaseConnectionDef.CreateDatabase(aOwner:
  TComponent;
  aSession: TnxSession; const aAlias: string;
  aAliasIsPath: Boolean): TnxDatabase;
begin
  Result := TnxDatabase.Create(aOwner);
  with Result do
  try
    Session := aSession;
    Timeout := -1;
    if aAliasIsPath then
    begin
      AliasName := '';
      AliasPath := aAlias;
    end
    else
    begin
      AliasPath := '';
      AliasName := aAlias;
    end;
    //      Result.Active := True;
  except
    if Assigned(Result) then
      FreeAndNil(Result);
    raise;
  end;
end;

{ TInstantNexusDbConnectionDef }

constructor TInstantNexusDbSQLConnectionDef.Create(Collection: TCollection);
begin
  inherited;
  FServerName := 'NexusDB@localhost';
  FPort := 16000;
  FProtocolType := ptTCPIP;
end;

{ SQL Based ------------------------------------------------------------------ }

{ TInstantNexusDbSQLConnectionDef }

class function TInstantNexusDbSQLConnectionDef.ConnectionTypeName: string;
begin
  Result := 'NexusDB (Remote/SQL)';
end;

class function TInstantNexusDbSQLConnectionDef.ConnectorClass:
  TInstantConnectorClass;
begin
  Result := TInstantNexusDbSQLConnector;
end;

class function TInstantNexusDbSQLConnectionDef.CreateServerEngine(aOwner:
    TComponent; const aServerName: string; aTransport: TnxBaseTransport):
    TnxBaseServerEngine;
begin
  Result := TnxRemoteServerEngine.Create(aOwner);
  with TnxRemoteServerEngine(Result) do
  try
    Transport := aTransport;
    Active := True;
  except
    if Assigned(Result) then
      FreeAndNil(Result);
    raise;
  end;
end;

class function TInstantNexusDbSQLConnectionDef.CreateTransport(aOwner:
    TComponent; aProtocolType: TInstantNexusDbProtocolType; const aServerName:
    string; aPort: Integer): TnxBaseTransport;
begin
  case aProtocolType of
    ptTCPIP:
      begin
        Result := TnxWinsockTransport.Create(aOwner);
        with TnxWinsockTransport(Result) do
        try
          ServerName := aServerName;
          Port := aPort;
          Active := True;
        except
          if Assigned(Result) then
            FreeAndNil(Result);
          raise;
        end;
      end;

    ptNamedPipes:
      begin
        Result := TnxNamedPipeTransport.Create(aOwner);
        with TnxNamedPipeTransport(Result) do
        try
          ServerName := aServerName;
          Port := aPort;
          Active := True;
        except
          if Assigned(Result) then
            FreeAndNil(Result);
          raise;
        end;
      end;
  end;
end;

function TInstantNexusDbSQLConnectionDef.Edit: Boolean;
begin
  with TInstantNexusDbConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

procedure TInstantNexusDbSQLConnectionDef.InitConnector(Connector:
    TInstantConnector);
var
  SavedCursor: TCursor;
  Transport: TnxBaseTransport;
  ServerEngine: TnxBaseServerEngine;
  Session: TnxSession;
  Database: TnxDatabase;
begin
  SavedCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    Transport := CreateTransport(Connector, ProtocolType, ServerName, Port);
    try
      ServerEngine := CreateServerEngine(Connector, ServerName, Transport);
      try
        Session := CreateSession(Connector, ServerEngine);
        try
          Database := CreateDatabase(Connector, Session, Alias, AliasIsPath);
          try
            TInstantNexusDbSQLConnector(Connector).Session := Session;
            TInstantNexusDbSQLConnector(Connector).Database := Database;
          except
            Database.Free;
            raise;
          end;
        except
          Session.Free;
          raise;
        end;
      except
        ServerEngine.Free;
        raise;
      end;
    except
      Transport.Free;
      raise;
    end;
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

class procedure TInstantNexusDbSQLConnectionDef.LoadAliasList(aProtocolType:
    TInstantNexusDbProtocolType; const aServerName: string; aPort: Integer;
    aList: TStrings);
var
  Transport: TnxBaseTransport;
  ServerEngine: TnxBaseServerEngine;
  Session: TnxSession;
begin
  Transport := CreateTransport(nil, aProtocolType, aServerName, aPort);
  try
    ServerEngine := CreateServerEngine(nil, aServerName, Transport);
    try
      Session := CreateSession(nil, ServerEngine);
      try
        aList.Clear;
        Session.GetAliasNames(aList);
      finally
        Session.Free;
      end;
    finally
      ServerEngine.Free;
    end;
  finally
    Transport.Free;
  end;
end;

class procedure TInstantNexusDbSQLConnectionDef.LoadServerList(aProtocolType:
    TInstantNexusDbProtocolType; aPort: Integer; aList: TStrings);
var
  Transport: TnxBaseTransport;
begin
  Transport := CreateTransport(nil, aProtocolType, '', aPort);
  try
    aList.Clear;
    Transport.GetServerNames(aList, 5000);
  finally
    Transport.Free;
  end;
end;

{ TInstantNexusDbSQLConnector }

procedure TInstantNexusDbSQLConnector.SetSession(Value: TnxSession);
begin
  if Value <> FSession then
  begin
    FSession := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TInstantNexusDbSQLConnector.SetDatabase(Value: TnxDatabase);
begin
  if Value <> FDatabase then
  begin
    FDatabase := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

class function TInstantNexusDbSQLConnector.ConnectionDefClass:
  TInstantConnectionDefClass;
begin
  Result := TInstantNexusDbSQLConnectionDef;
end;

procedure TInstantNexusDbSQLConnector.Notification(
  aComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if (aComponent = Session) then
      Session := nil
    else if (aComponent = Database) then
      Database := nil;
end;

function TInstantNexusDbSQLConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantNexusDbSQLBroker.Create(Self);
end;

{$IFDEF NX1}
{ The CreateFixedPrimaryIndexDef function finds the primary index created in
  SQL and returns its index number in the aOldIdx param. The function returns
  a suitably named TnxIndexDescriptor that is used to replace the SQL
  generated primary index. }

function TInstantNexusDbSQLConnector.CreateFixedPrimaryIndexDef(aTbl:
  TNexusDbTable; aDict: TnxDataDictionary; var aOldIdx: Integer):
  TnxIndexDescriptor;
var
  k: Integer;
  j: Integer;
  lKeyFieldDef: TnxCompKeyDescriptor;
begin
  result := nil;
  aOldIdx := 0;
  try
    for j := 0 to aDict.IndexCount - 1 do
    begin
      if aDict.IndexDescriptor[j].Name = 'key0' then
      begin
        result := TnxIndexDescriptor.CreateStandAlone(0,
          aTbl.TableName + '_ID',
          0,
          TnxCompKeyDescriptor);
        lKeyFieldDef := aDict.IndexDescriptor[j].KeyDescriptor as
          TnxCompKeyDescriptor;
        for k := 0 to lKeyFieldDef.KeyFieldCount - 1 do
        begin
          TnxCompKeyDescriptor(result.KeyDescriptor).Add(
            lKeyFieldDef.KeyFields[k].FieldNumber);
        end; { for }
        aOldIdx := j;
        Break;
      end; { if }
    end; { for }
  except
    result := nil;
  end; { try/except }
end;

procedure TInstantNexusDbSQLConnector.DatabaseBuildFixup;
var
  I: Integer;
  lTblList: TStrings;
  lTbl: TNexusDbTable;
begin
  lTblList := nil;
  lTbl := nil;
  try
    lTblList := TStringList.Create;
    Database.Session.CloseInactiveTables;
    Database.GetTableNames(lTblList);
    lTbl := TNexusDbTable.Create(nil);
    lTbl.Database := Database;
    for I := 0 to lTblList.Count - 1 do
    begin
      lTbl.TableName := lTblList[I];
      DoTableFix(lTbl);
    end; { for }
  finally
    lTbl.Free;
    lTblList.Free;
  end; { try/finally }
end;

procedure TInstantNexusDbSQLConnector.DoTableFix(aTbl: TNexusDbTable);
var
  lOldIdx: Integer;
  lFieldMap: TStrings;
  lCompleted: boolean;
  lStatus: TnxTaskStatus;
  lDict: TnxDataDictionary;
  lTaskInfo: TnxAbstractTaskInfo;
  lIdxDef: TnxIndexDescriptor;
begin
  lDict := nil;
  lFieldMap := TStringList.Create;
  try
    FillFieldMap(aTbl, lFieldMap);
    aTbl.UpdateIndexDefs;
    if aTbl.IndexDefs.Count = 0 then
      Abort;
    aTbl.dsUpdateDataDictionary;
    lDict := TnxDataDictionary.Create;
    lDict.Assign(aTbl.Dictionary);
    lIdxDef := CreateFixedPrimaryIndexDef(aTbl, lDict, lOldIdx);
    if Assigned(lIdxDef) then
    begin
      lDict.RemoveIndex(lOldIdx);
      lIdxDef := lDict.AddIndex(lIdxDef);
      lDict.DefaultIndex := lIdxDef.Number;
      Check(Database.RestructureTable(aTbl.TableName, lDict,
        lFieldMap, lTaskInfo));
      if Assigned(lTaskInfo) then
      begin
        repeat
          Sleep(250);
          lTaskInfo.GetStatus(lCompleted, lStatus);
        until lCompleted;
      end; { if }
      aTbl.FieldDefs.Clear;
      aTbl.IndexDefs.Clear;
    end; { if }
  finally
    lDict.Free;
    lFieldMap.Free;
  end; { try/finally }
end;

procedure TInstantNexusDbSQLConnector.FillFieldMap(aTbl: TNexusDbTable;
  aList: TStrings);
var
  j: Integer;
begin
  aList.Clear;
  for j := 0 to aTbl.FieldDefs.Count - 1 do
  begin
    aList.Add(aTbl.FieldDefs[j].Name);
  end; { for }
end;
{$ENDIF}

function TInstantNexusDbSQLConnector.GetConnected: Boolean;
begin
  Result := Assigned(Database) and Database.Connected;
end;

function TInstantNexusDbSQLConnector.GetDatabaseExists: Boolean;
begin
  Result := False;
  try
    Connect;
    Result := True;
  except
    // eat exceptions
  end;
  Disconnect;
end;

function TInstantNexusDbSQLConnector.GetDatabaseName: string;
begin
  if Assigned(Database) then
    Result := Database.AliasName
  else
    Result := SUndefined;
end;

function TInstantNexusDbSQLConnector.GetDBMSName: string;
begin
  Result := 'NexusDB (Remote/SQL)';
end;

function TInstantNexusDbSQLConnector.GetDDLTransactionSupported: Boolean;
begin
  Result := False;
end;

procedure TInstantNexusDbSQLConnector.InternalBuildDatabase(Scheme:
  TInstantScheme);
begin
  Session.CloseInactiveTables;
  Session.CloseInactiveFolders;
  // No transaction wrapper as DDL actions cannot be rolled back in
  // accordance with advice from NexusDb ng. - SRM 09 Oct 2004
  inherited;
{$IFDEF NX1}
  DatabaseBuildFixup; // Hopefully NexusDb V2 will not need this!
{$ENDIF}
end;

procedure TInstantNexusDbSQLConnector.InternalStartTransaction;
begin
  if not Database.InTransaction then
    Database.StartTransaction;
end;

procedure TInstantNexusDbSQLConnector.InternalCommitTransaction;
begin
  if Database.InTransaction then
    Database.Commit;
end;

procedure TInstantNexusDbSQLConnector.InternalRollbackTransaction;
begin
  if Database.InTransaction then
    Database.Rollback;
end;

procedure TInstantNexusDbSQLConnector.InternalConnect;
begin
  Database.Open;
end;

procedure TInstantNexusDbSQLConnector.InternalDisconnect;
begin
  Database.Close;
end;

{ TInstantNexusDbSQLBroker }

function TInstantNexusDbSQLBroker.GetConnector: TInstantNexusDbSQLConnector;
begin
  Result := inherited Connector as TInstantNexusDbSQLConnector;
end;

function TInstantNexusDbSQLBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantNexusDbSQLResolver.Create(Self, Map);
end;

function TInstantNexusDbSQLBroker.GetDBMSName: string;
begin
  Result := 'NexusDB (SQL)';
end;

function TInstantNexusDbSQLBroker.GetSQLDelimiters: string;
begin
  Result := '""';
end;

function TInstantNexusDbSQLBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

function TInstantNexusDbSQLBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantNexusDbSQLQuery.Create(Connector);
end;

procedure TInstantNexusDbSQLBroker.AssignDataSetParams(DataSet: TDataSet;
  aParams: TParams);
var
  I: Integer;
  TargetParams: TParams;
  SourceParam, TargetParam: TParam;
begin
  TargetParams := TNexusDbQuery(DataSet).Params;
  for I := 0 to Pred(AParams.Count) do
  begin
    SourceParam := AParams[I];
    TargetParam := TargetParams.FindParam(SourceParam.Name);
    if Assigned(TargetParam) then
      TargetParam.Assign(SourceParam);
  end;
end;

function TInstantNexusDbSQLBroker.CreateCatalog(const AScheme: TInstantScheme):
    TInstantCatalog;
begin
  Result := TInstantNxCatalog.Create(AScheme, Self);
end;

function TInstantNexusDbSQLBroker.CreateDataSet(
  const aStatement: string; aParams: TParams = nil): TDataSet;
var
  Query: TNexusDbQuery;
begin
  Query := TNexusDbQuery.Create(nil);
  with Query do
  begin
    Database := Connector.Database;
    SQL.Text := aStatement;
    if Assigned(aParams) then
      AssignDataSetParams(Query, aParams);
  end;
  Result := Query;
end;

function TInstantNexusDbSQLBroker.CreateDBBuildCommand(const CommandType:
    TInstantDBBuildCommandType): TInstantDBBuildCommand;
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

function TInstantNexusDbSQLBroker.DataTypeToColumnType(
  DataType: TInstantDataType; Size: Integer): string;
const
  Types: array[TInstantDataType] of string = (
    'INTEGER',
    'REAL',
    'MONEY',
    'BOOLEAN',
    'VARCHAR',
    'TEXT',
    'DATETIME',
    'BLOB');
begin
  Result := Types[DataType];
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
end;

function TInstantNexusDbSQLBroker.Execute(
  const aStatement: string; aParams: TParams = nil): Integer;
var
  DataSet: TNexusDbQuery;
begin
  Result := 0; 
  if aStatement = '' then
    Exit;

  DataSet := AcquireDataSet(aStatement, aParams) as TNexusDbQuery;
  try
    DataSet.ExecSQL;
    Result := DataSet.RowsAffected;
  finally
    ReleaseDataSet(DataSet);
  end;
end;

class function TInstantNexusDbSQLBroker.GeneratorClass:
    TInstantSQLGeneratorClass;
begin
  Result := TInstantSQLGenerator;
end;

{ TInstantNexusDbSQLTranslator }

function TInstantNexusDbSQLTranslator.GetDelimiters: string;
begin
  Result := '""';
end;

function TInstantNexusDbSQLTranslator.GetQuote: Char;
begin
  Result := '''';
end;

function TInstantNexusDbSQLTranslator.IncludeOrderFields: Boolean;
begin
  Result := True;
end;

{ TInstantNexusDbSQLQuery }

class function TInstantNexusDbSQLQuery.TranslatorClass:
  TInstantRelationalTranslatorClass;
begin
  Result := TInstantNexusDbSQLTranslator;
end;

initialization
  RegisterClass(TInstantNexusDbSQLConnectionDef);
  TInstantNexusDbSQLConnector.RegisterClass;

finalization
  TInstantNexusDbSQLConnector.UnregisterClass;

end.


