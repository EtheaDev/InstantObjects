(*
 *   InstantObjects(tm) NexusDb Broker Support - Broker
 *
 *   Copyright (c) Seleqt
 *   Copyright (c) Carlo Wolter - cwolter@tecnimex.it
 *

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations under the License.

The Original Code is InstantObject NexusDb Broker.

The Initial Developer of the Original Code is Carlo Wolter.
Portions created by Seleqt are Copyright (C) Seleqt.
All Rights Reserved.

Contributor(s):
Carlo Barazzetta:
  ServerName support

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
By using the InstantObject NexusDb Broker component you acknowledge
that you have read this limited warranty, understand it,
and agree to be bound by its' terms and conditions.
=====================================================================

 * Contributor(s):
 * Carlo Barazzetta: blob streaming in XML format (Part, Parts, References)
 *
 *)

unit InstantNexusDb;

interface

uses Classes, DB, InstantPersistence, InstantCommand,
  nxdb, nxsdServerEngine, nxreRemoteServerEngine, nxllComponent, nxllTransport, nxptBasePooledTransport, nxtwWinsockTransport
///  , CSIntf
    ;

type
  TNexusDbTable = class(TnxTable)
  end;

  TNexusDbQuery = class(TnxQuery)
  protected
    procedure SetRecNo(Value: Integer); override;
  end;

  TInstantNexusDbConnectionDef = class(TInstantRelationalConnectionDef)
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

  TInstantNexusDbConnector = class(TInstantRelationalConnector)
  private
    FDatabase:  TnxDatabase;
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

  TInstantNexusDbBroker = class(TInstantRelationalBroker)
  private
    function GetConnector: TInstantNexusDbConnector;
  protected
    function CreateResolver(const TableName: string): TInstantResolver; override;
    function InternalCreateQuery: TInstantQuery; override;
  public
    property Connector: TInstantNexusDbConnector read GetConnector;
  end;

  TInstantNexusDbResolver = class(TInstantResolver)
  private
    function GetBroker: TInstantNexusDbBroker;
    function GetDataSet: TNexusDbTable;
  protected
    function CreateDataSet: TDataSet; override;
    function Locate(const AClassName, AObjectId: string): Boolean; override;
  public
    property Broker: TInstantNexusDbBroker read GetBroker;
    property DataSet: TNexusDbTable read GetDataSet;
  end;

  TInstantNexusDbTranslator = class(TInstantRelationalTranslator)
  protected
    function GetDelimiters: string; override;
    function GetWildcard: string; override;
    function GetQuote: Char; override;
    function IncludeOrderFields: Boolean; override;
  end;

  TInstantNexusDbQuery = class(TInstantRelationalQuery)
  private
    FQuery: TNexusDbQuery;
    function GetQuery: TNexusDbQuery;
    function GetConnector: TInstantNexusDbConnector;
  protected
    function GetDataSet: TDataSet; override;
    function GetParams: TParams; override;
    function GetStatement: string; override;
    function IsSequenced: Boolean; override;
    procedure SetParams(Value: TParams); override;
    procedure SetStatement(const Value: string); override;
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
    property Query: TNexusDbQuery read GetQuery;
  public
    destructor Destroy; override;
    property Connector: TInstantNexusDbConnector read GetConnector;
  end;

procedure Register;

implementation

uses
  InstantConsts, Controls, InstantNexusDbConnectionDefEdit;

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantNexusDbConnector]);
end;

{ TInstantNexusDbConnectionDef }

procedure TInstantNexusDbConnectionDef.LoadAliasList(FALiasList : TStrings);
var
  Transport:  TnxWinsockTransport;
  Engine:     TnxRemoteServerEngine;
  Session:    TnxSession;
begin
///  CodeSite.SendMsg('create connection def');
  if ServerName = '' then
    ServerName  := 'NexusDb@localhost';
  Session := nil;
  Engine := nil;
  Transport := nil;
  try
    Transport := TnxWinsockTransport.Create(nil);     // Setup transport
    Transport.ServerNameRuntime := ServerName;
    FAliasList.Clear;
    Try
      Transport.Active := True;
      Engine := TnxRemoteServerEngine.Create(nil);      // Setup engine
      Engine.Transport := Transport;
      Engine.Active := True;
      Session := TnxSession.Create(nil);                // Setup session
      Session.ServerEngine := Engine;
      Session.Active := True;
      Session.GetAliasNames(FAliasList);
  ///    CodeSite.SendStringList('Alias list',AliasList);
    Except
      //ignore connections problems
      on EnxTransportException do ;
    End;
  finally
    Session.Free;
    Engine.Free;
    Transport.Free;
  end;
end;

procedure TInstantNexusDbConnectionDef.LoadServerList(FServerList : TStrings);
var
  Transport:  TnxWinsockTransport;
begin
///  CodeSite.SendMsg('create connection def');
  Transport := nil;
  try
    Transport := TnxWinsockTransport.Create(nil);     // Setup transport
    Transport.GetServerNames(FServerList, 5000);
  finally
    Transport.Free;
  end;
end;


class function TInstantNexusDbConnectionDef.ConnectionTypeName: string;
begin
  Result := 'NexusDb';
end;

class function TInstantNexusDbConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantNexusDbConnector;
end;

function TInstantNexusDbConnectionDef.Edit: Boolean;
begin
///  CodeSite.SendMsg('edit connectiondef');
  with TInstantNexusDbConnectionDefEditForm.Create(nil) do begin
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

procedure TInstantNexusDbConnectionDef.InitConnector(Connector: TInstantConnector);   { TODO: Create and return connection instance }
var
  Transport:  TnxWinsockTransport;
  Engine:     TnxRemoteServerEngine;
  Session:    TnxSession;
  Database:   TnxDatabase;
begin
  inherited;
///  CodeSite.SendMsg('init connector for alias '+AliasName);
  Transport := TnxWinsockTransport.Create(Connector);     // Setup transport
  Transport.ServerNameRuntime := ServerName;
  Transport.Active := True;
  Engine := TnxRemoteServerEngine.Create(Connector);      // Setup engine
  Engine.Transport := Transport;
  Engine.Active := True;
  Session := TnxSession.Create(Connector);                // Setup session
  Session.ServerEngine := Engine;
  Session.Active := True;
  Database := TnxDatabase.Create(Connector);              // Setup database
  Database.Session := Session;
  try
    Database.Timeout := -1;   // 0;
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
    (Connector as TInstantNexusDbConnector).Database := Database;
  except
    Database.Free;
    raise;
  end;
end;

{ TInstantNexusDbConnector }

class function TInstantNexusDbConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantNexusDbConnectionDef;
end;

function TInstantNexusDbConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantNexusDbBroker.Create(Self);
end;

function TInstantNexusDbConnector.GetConnected: Boolean;
begin
  Result := Assigned(Database) and Database.Connected;
end;

function TInstantNexusDbConnector.GetDatabaseName: string;
begin
///  CodeSite.SendMsg('get database name '+Database.AliasName);
  Result := Database.AliasName;
end;

function TInstantNexusDbConnector.GetDBMSName: string;
begin
///  CodeSite.SendMsg('get DBMS name');
  Result := 'NexusDb';
end;

procedure TInstantNexusDbConnector.InternalBuildDatabase(Scheme: TInstantScheme);

  procedure CreateTable(TableMetadata: TInstantTableMetadata);
  const
    FieldTypes: array[TInstantDataType] of TFieldType = (ftInteger, ftFloat, ftBoolean, ftString, ftMemo, ftDateTime, ftBlob);
  var
    I: Integer;
    Table: TNexusDbTable;
    IndexName: string;
  begin
    Table := TNexusDbTable.Create(nil);
    try
      Table.TableName := TableMetadata.Name;
///      CodeSite.SendMsg('creating table '+Table.TableName);
      Table.Database := Database;
      with TableMetadata do begin
        for I := 0 to Pred(IndexMetadatas.Count) do
          with IndexMetadatas[I] do begin
            IndexName := Name;
            if IndexName = '' then
              IndexName := Table.TableName + '_' + 'ID';
            Table.IndexDefs.Add(IndexName, Fields, Options);
          end;
        for I := 0 to Pred(FieldMetadatas.Count) do
          with FieldMetadatas[I] do
            Table.FieldDefs.Add(Name, FieldTypes[DataType], Size, foRequired in Options);
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
///  CodeSite.SendMsg('internal build db');
  if not Assigned(Scheme) then
    Exit;
  Scheme.BlobStreamFormat := BlobStreamFormat; //CB
  with Scheme do
    for I := 0 to Pred(TableMetadataCount) do
      CreateTable(TableMetadatas[I]);
end;

procedure TInstantNexusDbConnector.InternalCommitTransaction;
begin
  Database.Commit;
end;

procedure TInstantNexusDbConnector.InternalConnect;
begin
  Database.Open;
end;

procedure TInstantNexusDbConnector.InternalDisconnect;
begin
  Database.Close;
end;

procedure TInstantNexusDbConnector.InternalRollbackTransaction;
begin
  Database.Rollback;
end;

procedure TInstantNexusDbConnector.InternalStartTransaction;
begin
  Database.StartTransaction;
end;

{ TInstantNexusDbBroker }

function TInstantNexusDbBroker.CreateResolver(const TableName: string): TInstantResolver;
begin
  Result := TInstantNexusDbResolver.Create(Self, TableName);
end;

function TInstantNexusDbBroker.GetConnector: TInstantNexusDbConnector;
begin
  Result := inherited Connector as TInstantNexusDbConnector;
end;

function TInstantNexusDbBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantNexusDbQuery.Create(Connector);
end;

{ TInstantNexusDbResolver }

function TInstantNexusDbResolver.CreateDataSet: TDataSet;
begin
  Result := TNexusDbTable.Create(nil);
  with TNexusDbTable(Result) do
    try
      Database := Broker.Connector.Database;
      TableName := Self.TableName;
      IndexFieldNames := InstantIndexFieldNames;
    except
      Free;
      raise;
    end;
end;

function TInstantNexusDbResolver.GetBroker: TInstantNexusDbBroker;
begin
  Result := inherited Broker as TInstantNexusDbBroker;
end;

function TInstantNexusDbResolver.GetDataSet: TNexusDbTable;
begin
  Result := inherited DataSet as TNexusDbTable;
end;

function TInstantNexusDbResolver.Locate(const AClassName, AObjectId: string): Boolean;
begin
  Result := DataSet.FindKey([AClassName, AObjectId]);
end;

{ TInstantNexusDbQuery }

destructor TInstantNexusDbQuery.Destroy;
begin
  FQuery.Free;
  inherited;
end;

function TInstantNexusDbQuery.GetConnector: TInstantNexusDbConnector;
begin
  Result := inherited Connector as TInstantNexusDbConnector;
end;

function TInstantNexusDbQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TInstantNexusDbQuery.GetParams: TParams;
begin
  Result := Query.Params;
end;

function TInstantNexusDbQuery.GetQuery: TNexusDbQuery;
begin
  if not Assigned(FQuery) then begin
    FQuery := TNexusDbQuery.Create(nil);
    FQuery.Name := Connector.DatabaseName + 'Query';
    FQuery.Database := Connector.Database;
  end;
  Result := FQuery;
end;

function TInstantNexusDbQuery.GetStatement: string;
begin
  Result := Query.SQL.Text;
end;

function TInstantNexusDbQuery.IsSequenced: Boolean;
begin
  Result := Query.IsSequenced;
end;

procedure TInstantNexusDbQuery.SetParams(Value: TParams);
begin
  Query.Params := Value;
end;

procedure TInstantNexusDbQuery.SetStatement(const Value: string);
begin
  Query.SQL.Text := Value;
end;

class function TInstantNexusDbQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantNexusDbTranslator;
end;

{ TInstantNexusDbTranslator }

function TInstantNexusDbTranslator.GetDelimiters: string; // Returns two-char string of field-surrounding delimiters, eg. '[]'
begin
///  CodeSite.SendMsg('  Delimiter '+(inherited GetDelimiters));   // gives nothing now --> change to [] ?
  Result := inherited GetDelimiters;    // []
end;

function TInstantNexusDbTranslator.GetWildcard: string;   // Returns wildcard string, eg. '*'
begin
///  CodeSite.SendMsg('  Wildcard '+(inherited GetWildcard));      // gives % now - ok
  Result := inherited GetWildcard;      // %
end;

function TInstantNexusDbTranslator.GetQuote: Char;
begin
///  CodeSite.SendMsg('  Quote '+(inherited GetQuote));            // gives '' now - ok
  Result := '''';
end;

function TInstantNexusDbTranslator.IncludeOrderFields: Boolean;
begin
  Result := True;
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

initialization
  RegisterClass(TInstantNexusDbConnectionDef);
  TInstantNexusDbConnector.RegisterClass;

finalization
  TInstantNexusDbConnector.UnregisterClass;

end.
