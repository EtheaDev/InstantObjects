(*
 *   InstantObjects(tm) NexusDb SQL Broker Support - Broker
 *   NexusDbSQL Support
 *
 *   Copyright (c) Seleqt
 *   Copyright (c) Carlo Wolter - cwolter@tecnimex.it
 *   Copyright (c) Steven Mitchell - srmitch@tpg.com.au
 *

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations under the License.

The Original Code is a combination of InstantObject NexusDb Broker and
Seleqt InstantObjects InterBase Express Broker.

Portions created by Steven Mitchell are Copyright (C) Steven Mitchell.
Portions created by Carlo Wolter are Copyright (C) Carlo Wolter.
Portions created by Seleqt are Copyright (C) Seleqt.
All Rights Reserved.

 * Contributor(s):
 * Carlo Barazzetta and Nando Dessena

=====================================================================
 Limited warranty and disclaimer of warranty
=====================================================================
This software and accompanying written materials are provided
"as is" without warranty of any kind. Further, the author does
not warrant, guarantee, or take any representations regarding
the use, or the results of use, of the software or written
materials in terms of correctness, accuracy, reliability,
currentness or otherwise. The entire risk as to the results
and performance of the software is assumed by you.
Neither the author nor anyone else who has been involved in
the creation, production or delivery of this product shall be
liable for any direct, indirect, consequential or incidental
damages (including damages for loss of business profits, business
interruption, loss of business information and the like) arising
out of the use or inability to use the product even if the author
has been advised of the possibility of such damages.
By using the InstantObject NexusDb SQL Broker component you acknowledge
that you have read this limited warranty, understand it,
and agree to be bound by its' terms and conditions.
=====================================================================
 *)

unit InstantNexusDbSQL;

{$I ..\..\Core\InstantDefines.inc}

{$IFDEF D7+}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, Db, SysUtils, InstantPersistence, InstantClasses, InstantCommand,
  nxdb, nxsdServerEngine, nxreRemoteServerEngine, nxllComponent,
  nxllTransport, nxptBasePooledTransport, nxtwWinsockTransport,
  nxsdDataDictionary
  //, CSIntf
  ;

type
  TNexusDbTable = class(TnxTable)
  end;

  TNexusDbQuery = class(TnxQuery)
  protected
    procedure SetRecNo(Value: Integer); override;
  end;

  TInstantNexusDbSQLConnectionDef = class(TInstantRelationalConnectionDef)
  private
    FAliasName:   string;
    FAliasIsPath: boolean;
    FServerName:  string;
  protected
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    procedure LoadAliasList(FALiasList : TStrings);
    procedure LoadServerList(FServerList : TStrings);
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property AliasName: string read FAliasName write FAliasName;
    property AliasIsPath: boolean read FAliasIsPath write FAliasIsPath;
    property ServerName: string read FServerName write FServerName;
  end;

  TInstantNexusDbSQLConnector = class(TInstantRelationalConnector)
  private
    FDatabase:  TnxDatabase;
    procedure DatabaseBuildFixup;
    procedure FillFieldMap(aTbl: TNexusDbTable; aList: TStrings);
    function CreateFixedPrimaryIndexDef(aTbl: TNexusDbTable;
      aDict: TnxDataDictionary; var aOldIdx: Integer): TnxIndexDescriptor;
    procedure DoTableFix(aTbl: TNexusDbTable);
  protected
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
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
  published
    property Database: TnxDatabase read FDatabase write FDatabase;
  end;

  TInstantNexusDbSQLBroker= class(TInstantSQLBroker)
  private
    function GetConnector: TInstantNexusDbSQLConnector;
  protected
    function CreateResolver(Map: TInstantAttributeMap): TInstantSQLResolver; override;
    function GetDBMSName: string; override;
    function GetSQLDelimiters: String; override;      // SRM - 20 Jan 2005
    function GetSQLQuote: Char; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure AssignDataSetParams(DataSet : TDataSet; AParams: TParams); override;
  public
    function CreateDataSet(const AStatement: string; AParams: TParams = nil): TDataSet; override;
    function DataTypeToColumnType(DataType: TInstantDataType; Size: Integer): string; override;
    function Execute(const AStatement: string; AParams: TParams = nil): Integer; override;
    property Connector: TInstantNexusDbSQLConnector read GetConnector;
  end;

  TInstantNexusDbSQLResolver = class(TInstantSQLResolver);

  TInstantNexusDbSQLTranslator = class(TInstantRelationalTranslator)
  protected
    function GetDelimiters: String; override;         // SRM - 20 Jan 2005
    function GetQuote: Char; override;                // SRM - 29 Dec 2004
    function IncludeOrderFields: Boolean; override;   // SRM - 29 Dec 2004
  end;

  TInstantNexusDbSQLQuery = class(TInstantSQLQuery)
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

implementation

uses
  Controls, InstantConsts, InstantNexusDbSQLConnectionDefEdit,
  InstantUtils, nxdbBase, nxsdTypes;

const
  SUndefined = 'Undefined';

procedure TInstantNexusDbSQLConnectionDef.LoadAliasList(FALiasList : TStrings);
var
  Transport:  TnxWinsockTransport;
  Engine:     TnxRemoteServerEngine;
  Session:    TnxSession;
begin
  if ServerName = '' then
    ServerName  := 'NexusDb@localhost';
  Session := nil;
  Engine := nil;
  Transport := nil;
  try
    Transport := TnxWinsockTransport.Create(nil);     // Setup transport
    Transport.ServerNameRuntime := ServerName;
    FAliasList.Clear;
    try
      Transport.Active := True;
      Engine := TnxRemoteServerEngine.Create(nil);      // Setup engine
      Engine.Transport := Transport;
      Engine.Active := True;
      Session := TnxSession.Create(nil);                // Setup session
      Session.ServerEngine := Engine;
      Session.Active := True;
      Session.GetAliasNames(FAliasList);
  ///    CodeSite.SendStringList('Alias list',AliasList);
    except
      //ignore connections problems
      on EnxTransportException do ;
    end;
  finally
    Session.Free;
    Engine.Free;
    Transport.Free;
  end;
end;

procedure TInstantNexusDbSQLConnectionDef.LoadServerList(FServerList : TStrings);
var
  Transport:  TnxWinsockTransport;
begin
  Transport := nil;
  try
    Transport := TnxWinsockTransport.Create(nil);     // Setup transport
    Transport.GetServerNames(FServerList, 5000);
  finally
    Transport.Free;
  end;
end;


class function TInstantNexusDbSQLConnectionDef.ConnectionTypeName: string;
begin
  Result := 'NexusDb (Remote/SQL)';   // SRM - 29 Dec 2004
end;

class function TInstantNexusDbSQLConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantNexusDbSQLConnector;
end;

function TInstantNexusDbSQLConnectionDef.Edit: Boolean;
begin
  with TInstantNexusDbSQLConnectionDefEditForm.Create(nil) do begin
    try
      LoadData(Self);
      Result := ShowModal = mrOk;
      if Result then begin
        SaveData(Self);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TInstantNexusDbSQLConnectionDef.InitConnector(Connector: TInstantConnector);
{ TODO: Create and return connection instance }
var
  Transport:  TnxWinsockTransport;
  Engine:     TnxRemoteServerEngine;
  Session:    TnxSession;
  Database:   TnxDatabase;
begin
  //CodeSite.EnterMethod('TInstantNexusDbSQLConnectionDef.InitConnector');
  inherited;
  Transport := TnxWinsockTransport.Create(Connector);     // Setup transport
  Transport.ServerName := ServerName;
  Transport.Active := True;
  Engine := TnxRemoteServerEngine.Create(Connector);      // Setup engine
  Engine.Transport := Transport;
  Engine.Active := True;
  Session := TnxSession.Create(Connector);                // Setup session.
  Session.DisplayName := 'nxInitSession';
  Session.ServerEngine := Engine;
  Session.Active := True;
  Database := TnxDatabase.Create(Connector);              // Setup database
  Database.DisplayName := 'nxInitDatabase';
  Database.Session := Session;
  try
    Database.Timeout := -1;
    Database.Active := False;
    if AliasIsPath then begin
      Database.AliasName := '';
      Database.AliasPath := AliasName;                    // Path
      end
    else begin
      Database.AliasName := AliasName;                    // Alias
      Database.AliasPath := '';
    end;
    Database.Active := True;
    (Connector as TInstantNexusDbSQLConnector).Database := Database;
  except
    Database.Free;
    raise;
  end;
  //CodeSite.ExitMethod('TInstantNexusDbSQLConnectionDef.InitConnector');
end;

{ TInstantNexusDbSQLConnector }

class function TInstantNexusDbSQLConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantNexusDbSQLConnectionDef;
end;

function TInstantNexusDbSQLConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantNexusDbSQLBroker.Create(Self);
end;

function TInstantNexusDbSQLConnector.GetConnected: Boolean;
begin
  Result := Assigned(Database) and Database.Connected;
end;

function TInstantNexusDbSQLConnector.GetDatabaseName: string;
begin
  if Assigned(Database) then          // SRM - 29 Dec 2004
    Result := Database.AliasName
  else                                // SRM - 29 Dec 2004
    Result := SUndefined;             // SRM - 29 Dec 2004
end;

function TInstantNexusDbSQLConnector.GetDBMSName: string;
begin
  Result := 'NexusDb (Remote/SQL)';   // SRM - 29 Dec 2004
end;

procedure TInstantNexusDbSQLConnector.FillFieldMap(aTbl: TNexusDbTable;
  aList: TStrings);
var
  j: Integer;
begin
  aList.Clear;
  for j := 0 to aTbl.FieldDefs.Count - 1 do begin
    aList.Add(aTbl.FieldDefs[j].Name);
  end;    { for }
end;

{ The CreateFixedPrimaryIndexDef function finds the primary index created in 
  SQL and returns its index number in the aOldIdx param. The function returns
  a suitably named TnxIndexDescriptor that is used to replace the SQL
  generated primary index. }
function TInstantNexusDbSQLConnector.CreateFixedPrimaryIndexDef(
  aTbl: TNexusDbTable; aDict: TnxDataDictionary; var aOldIdx: Integer):
  TnxIndexDescriptor;
var
  k: Integer;
  j: Integer;
  lKeyFieldDef: TnxCompKeyDescriptor;
begin
  result := nil;
  aOldIdx := 0;
  try
    for j := 0 to aDict.IndexCount - 1 do begin
      //CodeSite.SendFmtMsg('aDict.IndexDescriptor[%d].Name: %s',
      //        [j, aTbl.IndexDefs[j].Name]);
      if aDict.IndexDescriptor[j].Name = 'key0' then begin
        result := TnxIndexDescriptor.CreateStandAlone(0,
                        aTbl.TableName + '_ID',
                        0,
                        TnxCompKeyDescriptor);
        lKeyFieldDef := aDict.IndexDescriptor[j].KeyDescriptor as
                 TnxCompKeyDescriptor;
        for k := 0 to lKeyFieldDef.KeyFieldCount - 1 do begin
          TnxCompKeyDescriptor(result.KeyDescriptor).Add(
                        lKeyFieldDef.KeyFields[k].FieldNumber);
        end;    { for }
        aOldIdx := j;
        Break;
      end;    { if }
    end;    { for }
  except
  	result := nil;
  end;  { try/except }
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
      Exit;
    aTbl.dsUpdateDataDictionary;
    lDict := TnxDataDictionary.Create;
    lDict.Assign(aTbl.Dictionary);
    lIdxDef := CreateFixedPrimaryIndexDef(aTbl, lDict, lOldIdx);
    if Assigned(lIdxDef) then begin
      lDict.RemoveIndex(lOldIdx);
      lIdxDef := lDict.AddIndex(lIdxDef);
      lDict.DefaultIndex := lIdxDef.Number;
      Check(Database.RestructureTable(aTbl.TableName, lDict,
              lFieldMap, lTaskInfo));
      if Assigned(lTaskInfo) then begin
        repeat
          Sleep(250);
          lTaskInfo.GetStatus(lCompleted, lStatus);
        until lCompleted;
      end;    { if }
      aTbl.FieldDefs.Clear;
      aTbl.IndexDefs.Clear;
    end;    { if }
  finally
  	lDict.Free;
    lFieldMap.Free;
  end;  { try/finally }
end;

procedure TInstantNexusDbSQLConnector.DatabaseBuildFixup;
var
  I: Integer;
  lTblList: TStrings;
  lTbl: TNexusDbTable;
begin
  //CodeSite.EnterMethod('TInstantNexusDbSQLConnector.DatabaseBuildFixup');
  lTblList := nil;
  lTbl := nil;
  try
    lTblList := TStringList.Create;
    Database.Session.CloseInactiveTables;
    Database.GetTableNames(lTblList);
    lTbl := TNexusDbTable.Create(nil);
    lTbl.Database := Database;
    for I := 0 to lTblList.Count - 1 do begin
      //CodeSite.SendFmtMsg('lTbl[%d].TableName: %s', [I, lTbl.TableName]);
      lTbl.TableName := lTblList[I];
      DoTableFix(lTbl);
    end;    { for }
  finally
    lTbl.Free;
  	lTblList.Free;
  end;  { try/finally }
  //CodeSite.ExitMethod('TInstantNexusDbSQLConnector.DatabaseBuildFixup');
end;

procedure TInstantNexusDbSQLConnector.InternalBuildDatabase(Scheme: TInstantScheme);
begin
  // Deleted transaction wrapper as DDL actions cannot be rolled back in 
  // accordance with advice from NexusDb ng. - SRM 09 Oct 2004
  inherited;
  DatabaseBuildFixup;     // Hopefully NexusDb V2 will not need this!
end;

procedure TInstantNexusDbSQLConnector.InternalCommitTransaction;
begin
  if Database.InTransaction then      // SRM - 29 Dec 2004
    Database.Commit;
end;

procedure TInstantNexusDbSQLConnector.InternalConnect;
begin
  //CodeSite.EnterMethod('TInstantNexusDbSQLConnector.InternalConnect');
  //CodeSite.SendFmtMsg('Database.DisplayName: %s', [Database.DisplayName]);
  Database.Open;
  //CodeSite.ExitMethod('TInstantNexusDbSQLConnector.InternalConnect');
end;

procedure TInstantNexusDbSQLConnector.InternalDisconnect;
begin
  //CodeSite.EnterMethod('TInstantNexusDbSQLConnector.InternalDisconnect');
  //CodeSite.SendFmtMsg('Database.DisplayName: %s', [Database.DisplayName]);
  Database.Close;
  //CodeSite.ExitMethod('TInstantNexusDbSQLConnector.InternalDisconnect');
end;

procedure TInstantNexusDbSQLConnector.InternalRollbackTransaction;
begin
  if Database.InTransaction then      // SRM - 29 Dec 2004
    Database.Rollback;
end;

procedure TInstantNexusDbSQLConnector.InternalStartTransaction;
begin
  if not Database.InTransaction then  // SRM - 29 Dec 2004
    Database.StartTransaction;
end;

{ TInstantNexusDbSQLBroker}

procedure TInstantNexusDbSQLBroker.AssignDataSetParams(DataSet : TDataSet; AParams: TParams);
var
  I: Integer;
  TargetParams : TParams;
  SourceParam, TargetParam: TParam;
begin
  //don't call inherited!
  TargetParams := TNexusDbQuery(DataSet).Params;
  for I := 0 to Pred(AParams.Count) do
  begin
    SourceParam := AParams[I];
    TargetParam := TargetParams.FindParam(SourceParam.Name);
    if Assigned(TargetParam) then
      TargetParam.Assign(SourceParam);
  end;
end;

function TInstantNexusDbSQLBroker.CreateDataSet(const AStatement: string;
  AParams: TParams): TDataSet;
var
  Query: TNexusDbQuery;
begin
  //CodeSite.EnterMethod('TInstantNexusDbSQLBroker.CreateDataSet');
  //CodeSite.SendFmtMsg('SQL Statement: %s', [#13 + AStatement]);
  Query := TNexusDbQuery.Create(nil);
  with Query do
  begin
    Database := Connector.Database;
    SQL.Text := AStatement;
    if Assigned(AParams) then
      AssignDataSetParams(Query, AParams);
  end;
  Result := Query;
  //CodeSite.ExitMethod('TInstantNexusDbSQLBroker.CreateDataSet');
end;  

function TInstantNexusDbSQLBroker.CreateResolver(
  Map: TInstantAttributeMap): TInstantSQLResolver;
begin
  Result := TInstantNexusDbSQLResolver.Create(Self, Map);
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
  //CodeSite.EnterMethod('TInstantNexusDbSQLBroker.DataTypeToColumnType');
  Result := Types[DataType];
  if (DataType = dtString) and (Size > 0) then
    Result := Result + InstantEmbrace(IntToStr(Size), '()');
  //CodeSite.ExitMethod('TInstantNexusDbSQLBroker.DataTypeToColumnType');
end;

function TInstantNexusDbSQLBroker.Execute(const AStatement: string;
  AParams: TParams): Integer;
var                               // SRM - 11 Feb 2005
  DataSet: TNexusDbQuery;         // SRM - 11 Feb 2005
begin
  //CodeSite.SendFmtMsg('SQL Statement: %s', [#13 + AStatement]);
  // SRM - 11 Feb 2005: begin
  DataSet := AcquireDataSet(AStatement, AParams) as TNexusDbQuery;
  try
    DataSet.ExecSQL;
    Result := DataSet.RowsAffected;
  finally
    ReleaseDataSet(DataSet);
  end;
  // SRM - 11 Feb 2005: end
end;

function TInstantNexusDbSQLBroker.GetConnector: TInstantNexusDbSQLConnector;
begin
  Result := inherited Connector as TInstantNexusDbSQLConnector;
end;

function TInstantNexusDbSQLBroker.GetDBMSName: string;
begin
  Result := 'NexusDb (Remote/SQL)';   // SRM - 29 Dec 2004
end;

// SRM - 20 Jan 2005: begin
function TInstantNexusDbSQLBroker.GetSQLDelimiters: String;
begin
  Result := '""';                     
end;
// SRM - 20 Jan 2005: end

function TInstantNexusDbSQLBroker.GetSQLQuote: Char;
begin
  Result := '''';
end;

function TInstantNexusDbSQLBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantNexusDbSQLQuery.Create(Connector);
end;

{ TInstantNexusDbSQLQuery }

class function TInstantNexusDbSQLQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantNexusDbSQLTranslator;
end;

{ TNexusDbQuery }

procedure TNexusDbQuery.SetRecNo(Value: Integer);
begin
  inherited;
  if Value = Succ(RecNo) then
    Next
  else if Value = Pred(RecNo) then
    Prior;
end;

{ TInstantNexusDbSQLTranslator }

// SRM - 20 Jan 2005: begin
function TInstantNexusDbSQLTranslator.GetDelimiters: String;
begin
  Result := '""';
end;
// SRM - 20 Jan 2005: end

// SRM - 29 Dec 2004: begin
function TInstantNexusDbSQLTranslator.GetQuote: Char;
begin
  Result := '''';
end;

function TInstantNexusDbSQLTranslator.IncludeOrderFields: Boolean;
begin
  Result := True;                     
end;
// SRM - 29 Dec 2004: end

initialization
  RegisterClass(TInstantNexusDbSQLConnectionDef);
  TInstantNexusDbSQLConnector.RegisterClass;

finalization
  TInstantNexusDbSQLConnector.UnregisterClass;

end.
