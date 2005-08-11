(*
 *   InstantObjects
 *   NexusDB Support
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

unit InstantNexusDB;

{$I ../../InstantDefines.inc}
{$I InstantNexusDBDefines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, DB, InstantPersistence, InstantCommand,
  nxllTransport, nxsdServerEngine, nxdb, nxsdDataDictionary
  ;

type
  TNexusDBTable = class(TnxTable)
  end;

  TNexusDBQuery = class(TnxQuery)
  protected
    procedure SetRecNo(Value: Integer); override;
  end;

  TInstantNexusDBProtocolType = (ptTCPIP, ptNamedPipes);

  TInstantNexusDBBaseConnectionDef = class(TInstantRelationalConnectionDef)
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

  TInstantNexusDBConnectionDef = class(TInstantNexusDBBaseConnectionDef)
  private
    FPort: Integer;
    FProtocolType: TInstantNexusDBProtocolType;
    FServerName: string;
  protected
    class function CreateServerEngine(aOwner: TComponent; const aServerName:
        string; aTransport: TnxBaseTransport): TnxBaseServerEngine;
    class function CreateTransport(aOwner: TComponent; aProtocolType:
        TInstantNexusDBProtocolType; const aServerName: string; aPort: Integer):
        TnxBaseTransport;
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    constructor Create(Collection: TCollection); override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    function Edit: Boolean; override;
    class procedure LoadAliasList(aProtocolType: TInstantNexusDBProtocolType; const
        aServerName: string; aPort: Integer; aList: TStrings);
    class procedure LoadServerList(aProtocolType: TInstantNexusDBProtocolType;
        aPort: Integer; aList: TStrings);
  published
    property Port: Integer read FPort write FPort default 16000;
    property ProtocolType: TInstantNexusDBProtocolType read FProtocolType write
        FProtocolType default ptTCPIP;
    property ServerName: string read FServerName write FServerName;
  end;

  TInstantNexusDBConnector = class(TInstantRelationalConnector)
  private
    FSession: TnxSession;
    FDatabase: TnxDatabase;

{$IFDEF NX1}
    procedure DatabaseBuildFixup;
    procedure FillFieldMap(aTbl: TNexusDBTable; aList: TStrings);
    function CreateFixedPrimaryIndexDef(aTbl: TNexusDBTable; aDict:
      TnxDataDictionary; var aOldIdx: Integer): TnxIndexDescriptor;
    procedure DoTableFix(aTbl: TNexusDBTable);
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

  TInstantNexusDBBroker = class(TInstantSQLBroker)
  private
    function GetConnector: TInstantNexusDBConnector;
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
    property Connector: TInstantNexusDBConnector
      read GetConnector;
  end;

  TInstantNexusDBResolver = class(TInstantSQLResolver)
  end;

  TInstantNexusDBTranslator = class(TInstantRelationalTranslator)
  protected
    function GetDelimiters: string; override;
    function GetQuote: Char; override;
    function IncludeOrderFields: Boolean; override;
  end;

  TInstantNexusDBQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

  TInstantNexusDBSQLGenerator = class(TInstantSQLGenerator)
  protected
    function EmbraceIndex(const IndexName: string): string; virtual;
    function InternalGenerateCreateIndexSQL(Metadata: TInstantIndexMetadata):
        string; override;
    function InternalGenerateDropIndexSQL(Metadata: TInstantIndexMetadata): string;
        override;
  end;

implementation

uses
  SysUtils,
  Controls,
  Forms,
  InstantConsts,
  InstantUtils,
  InstantNexusDBConnectionDefEdit,
  nxllTypes,
  nxsdTypes,
  nxdbBase,
  nxtwWinsockTransport,
  nxtnNamedPipeTransport,
  nxreRemoteServerEngine,
  InstantNexusDBCatalog, 
  InstantDBBuild;

const
  SUndefined = 'Undefined';

  { common code }

  { TNexusDBQuery }

procedure TNexusDBQuery.SetRecNo(Value: Integer);
begin
  inherited;
  if Value = Succ(RecNo) then
    Next
  else if Value = Pred(RecNo) then
    Prior;
end;

{ TTInstantNexusDBBaseConnectionDef }

class function TInstantNexusDBBaseConnectionDef.CreateSession(aOwner:
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

class function TInstantNexusDBBaseConnectionDef.CreateDatabase(aOwner:
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

{ TInstantNexusDBConnectionDef }

constructor TInstantNexusDBConnectionDef.Create(Collection: TCollection);
begin
  inherited;
  FServerName := 'NexusDB@localhost';
  FPort := 16000;
  FProtocolType := ptTCPIP;
end;

{ SQL Based ------------------------------------------------------------------ }

{ TInstantNexusDBSQLConnectionDef }

class function TInstantNexusDBConnectionDef.ConnectionTypeName: string;
begin
  Result := 'NexusDB (Remote/SQL)';
end;

class function TInstantNexusDBConnectionDef.ConnectorClass:
  TInstantConnectorClass;
begin
  Result := TInstantNexusDBConnector;
end;

class function TInstantNexusDBConnectionDef.CreateServerEngine(aOwner:
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

class function TInstantNexusDBConnectionDef.CreateTransport(aOwner:
    TComponent; aProtocolType: TInstantNexusDBProtocolType; const aServerName:
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

function TInstantNexusDBConnectionDef.Edit: Boolean;
begin
  with TInstantNexusDBConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

procedure TInstantNexusDBConnectionDef.InitConnector(Connector:
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
            TInstantNexusDBConnector(Connector).Session := Session;
            TInstantNexusDBConnector(Connector).Database := Database;
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

class procedure TInstantNexusDBConnectionDef.LoadAliasList(aProtocolType:
    TInstantNexusDBProtocolType; const aServerName: string; aPort: Integer;
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

class procedure TInstantNexusDBConnectionDef.LoadServerList(aProtocolType:
    TInstantNexusDBProtocolType; aPort: Integer; aList: TStrings);
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

{ TInstantNexusDBSQLConnector }

procedure TInstantNexusDBConnector.SetSession(Value: TnxSession);
begin
  if Value <> FSession then
  begin
    FSession := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TInstantNexusDBConnector.SetDatabase(Value: TnxDatabase);
begin
  if Value <> FDatabase then
  begin
    FDatabase := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

class function TInstantNexusDBConnector.ConnectionDefClass:
  TInstantConnectionDefClass;
begin
  Result := TInstantNexusDBConnectionDef;
end;

procedure TInstantNexusDBConnector.Notification(
  aComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if (aComponent = Session) then
      Session := nil
    else if (aComponent = Database) then
      Database := nil;
end;

function TInstantNexusDBConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantNexusDBBroker.Create(Self);
end;

{$IFDEF NX1}
{ The CreateFixedPrimaryIndexDef function finds the primary index created in
  SQL and returns its index number in the aOldIdx param. The function returns
  a suitably named TnxIndexDescriptor that is used to replace the SQL
  generated primary index. }

function TInstantNexusDBConnector.CreateFixedPrimaryIndexDef(aTbl:
  TNexusDBTable; aDict: TnxDataDictionary; var aOldIdx: Integer):
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

procedure TInstantNexusDBConnector.DatabaseBuildFixup;
var
  I: Integer;
  lTblList: TStrings;
  lTbl: TNexusDBTable;
begin
  lTblList := nil;
  lTbl := nil;
  try
    lTblList := TStringList.Create;
    Database.Session.CloseInactiveTables;
    Database.GetTableNames(lTblList);
    lTbl := TNexusDBTable.Create(nil);
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

procedure TInstantNexusDBConnector.DoTableFix(aTbl: TNexusDBTable);
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

procedure TInstantNexusDBConnector.FillFieldMap(aTbl: TNexusDBTable;
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

function TInstantNexusDBConnector.GetConnected: Boolean;
begin
  Result := Assigned(Database) and Database.Connected;
end;

function TInstantNexusDBConnector.GetDatabaseExists: Boolean;
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

function TInstantNexusDBConnector.GetDatabaseName: string;
begin
  if Assigned(Database) then
    Result := Database.AliasName
  else
    Result := SUndefined;
end;

function TInstantNexusDBConnector.GetDBMSName: string;
begin
  Result := 'NexusDB (Remote/SQL)';
end;

function TInstantNexusDBConnector.GetDDLTransactionSupported: Boolean;
begin
  Result := False;
end;

procedure TInstantNexusDBConnector.InternalBuildDatabase(Scheme:
  TInstantScheme);
begin
  Session.CloseInactiveTables;
  Session.CloseInactiveFolders;
  // No transaction wrapper as DDL actions cannot be rolled back in
  // accordance with advice from NexusDB ng. - SRM 09 Oct 2004
  inherited;
{$IFDEF NX1}
  DatabaseBuildFixup; // Hopefully NexusDB V2 will not need this!
{$ENDIF}
end;

procedure TInstantNexusDBConnector.InternalStartTransaction;
begin
  if not Database.InTransaction then
    Database.StartTransaction;
end;

procedure TInstantNexusDBConnector.InternalCommitTransaction;
begin
  if Database.InTransaction then
    Database.Commit;
end;

procedure TInstantNexusDBConnector.InternalRollbackTransaction;
begin
  if Database.InTransaction then
    Database.Rollback;
end;

procedure TInstantNexusDBConnector.InternalConnect;
begin
  Database.Open;
end;

procedure TInstantNexusDBConnector.InternalDisconnect;
begin
  Database.Close;
end;

{ TInstantNexusDBSQLBroker }

function TInstantNexusDBBroker.GetConnector: TInstantNexusDBConnector;
begin
  Result := inherited Connector as TInstantNexusDBConnector;
end;

function TInstantNexusDBBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantNexusDBResolver.Create(Self, Map);
end;

function TInstantNexusDBBroker.GetDBMSName: string;
begin
  Result := 'NexusDB (SQL)';
end;

function TInstantNexusDBBroker.GetSQLDelimiters: string;
begin
  Result := '""';
end;

function TInstantNexusDBBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

function TInstantNexusDBBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantNexusDBQuery.Create(Connector);
end;

procedure TInstantNexusDBBroker.AssignDataSetParams(DataSet: TDataSet;
  aParams: TParams);
var
  I: Integer;
  TargetParams: TParams;
  SourceParam, TargetParam: TParam;
begin
  TargetParams := TNexusDBQuery(DataSet).Params;
  for I := 0 to Pred(AParams.Count) do
  begin
    SourceParam := AParams[I];
    TargetParam := TargetParams.FindParam(SourceParam.Name);
    if Assigned(TargetParam) then
      TargetParam.Assign(SourceParam);
  end;
end;

function TInstantNexusDBBroker.CreateCatalog(const AScheme: TInstantScheme):
    TInstantCatalog;
begin
  Result := TInstantNexusDBCatalog.Create(AScheme, Self);
end;

function TInstantNexusDBBroker.CreateDataSet(
  const aStatement: string; aParams: TParams = nil): TDataSet;
var
  Query: TNexusDBQuery;
begin
  Query := TNexusDBQuery.Create(nil);
  with Query do
  begin
    Database := Connector.Database;
    SQL.Text := aStatement;
    if Assigned(aParams) then
      AssignDataSetParams(Query, aParams);
  end;
  Result := Query;
end;

function TInstantNexusDBBroker.CreateDBBuildCommand(const CommandType:
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

function TInstantNexusDBBroker.DataTypeToColumnType(
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

function TInstantNexusDBBroker.Execute(
  const aStatement: string; aParams: TParams = nil): Integer;
var
  DataSet: TNexusDBQuery;
begin
  Result := 0; 
  if aStatement = '' then
    Exit;

  DataSet := AcquireDataSet(aStatement, aParams) as TNexusDBQuery;
  try
    DataSet.ExecSQL;
    Result := DataSet.RowsAffected;
  finally
    ReleaseDataSet(DataSet);
  end;
end;

class function TInstantNexusDBBroker.GeneratorClass:
    TInstantSQLGeneratorClass;
begin
  Result := TInstantNexusDBSQLGenerator;
end;

{ TInstantNexusDBSQLTranslator }

function TInstantNexusDBTranslator.GetDelimiters: string;
begin
  Result := '""';
end;

function TInstantNexusDBTranslator.GetQuote: Char;
begin
  Result := '''';
end;

function TInstantNexusDBTranslator.IncludeOrderFields: Boolean;
begin
  Result := True;
end;

{ TInstantNexusDBSQLQuery }

class function TInstantNexusDBQuery.TranslatorClass:
  TInstantRelationalTranslatorClass;
begin
  Result := TInstantNexusDBTranslator;
end;

function TInstantNexusDBSQLGenerator.EmbraceIndex(const IndexName: string):
    string;
begin
  Result := InstantEmbrace(IndexName, Delimiters);
end;

function TInstantNexusDBSQLGenerator.InternalGenerateCreateIndexSQL(Metadata:
    TInstantIndexMetadata): string;
var
  Modifier, Columns, TableName: string;
begin
  if ixUnique in Metadata.Options then
    Modifier := 'UNIQUE '
  else
    Modifier := '';
  if ixDescending in Metadata.Options then
    Modifier := Modifier + 'DESCENDING ';
  Columns := BuildFieldList(Metadata.Fields);
  TableName := Metadata.TableMetadata.Name;
  Result := Format('CREATE %sINDEX %s ON %s (%s)',
    [Modifier, EmbraceIndex(Metadata.Name), EmbraceTable(TableName), Columns]);
end;

function TInstantNexusDBSQLGenerator.InternalGenerateDropIndexSQL(Metadata:
    TInstantIndexMetadata): string;
begin
  Result := Format('DROP INDEX %s.%s',
          [EmbraceTable(Metadata.TableMetadata.Name),
           EmbraceIndex(Metadata.Name)]);
end;

initialization
  RegisterClass(TInstantNexusDBConnectionDef);
  TInstantNexusDBConnector.RegisterClass;

finalization
  TInstantNexusDBConnector.UnregisterClass;

end.


