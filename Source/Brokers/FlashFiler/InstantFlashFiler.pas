(*
 *   InstantObjects
 *   TurboPower FlashFiler Support
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
 * ***** END LICENSE BLOCK ***** *)

unit InstantFlashFiler;

interface

uses
  Classes, DB, InstantPersistence, InstantCommand, FFDB, FFDB_Enh;

type
  TFlashFilerTable = class(TffTableEnh)
  end;

  TFlashFilerQuery = class(TffQueryEnh)
  protected
    procedure SetRecNo(Value: Integer); override;
  end;

  TInstantFlashFilerConnectionDef = class(TInstantRelationalConnectionDef)
  private
    FAliasName: string;
  protected
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property AliasName: string read FAliasName write FAliasName;
  end;

  TInstantFlashFilerConnector = class(TInstantRelationalConnector)
  private
    FDatabase: TffDatabase;
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
    property Database: TffDatabase read FDatabase write FDatabase;
  end;

  TInstantFlashFilerBroker = class(TInstantRelationalBroker)
  private
    function GetConnector: TInstantFlashFilerConnector;
  protected
    function CreateResolver(const TableName: string): TInstantResolver; override;
    function InternalCreateQuery: TInstantQuery; override;
  public
    property Connector: TInstantFlashFilerConnector read GetConnector;
  end;

  TInstantFlashFilerResolver = class(TInstantResolver)
  private
    function GetBroker: TInstantFlashFilerBroker;
    function GetDataSet: TFlashFilerTable;
  protected
    function CreateDataSet: TDataSet; override;
    function Locate(const AClassName, AObjectId: string): Boolean; override;
  public
    property Broker: TInstantFlashFilerBroker read GetBroker;
    property DataSet: TFlashFilerTable read GetDataSet;
  end;

  TInstantFlashFilerTranslator = class(TInstantRelationalTranslator)
  protected
    function GetQuote: Char; override;
    function IncludeOrderFields: Boolean; override;
  end;

  TInstantFlashFilerQuery = class(TInstantRelationalQuery)
  private
    FQuery: TFlashFilerQuery;
    function GetQuery: TFlashFilerQuery;
    function GetConnector: TInstantFlashFilerConnector;
  protected
    function GetDataSet: TDataSet; override;
    function GetParams: TParams; override;
    function GetStatement: string; override;
    function IsSequenced: Boolean; override;
    procedure SetParams(Value: TParams); override;
    procedure SetStatement(const Value: string); override;
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
    property Query: TFlashFilerQuery read GetQuery;
  public
    destructor Destroy; override;
    property Connector: TInstantFlashFilerConnector read GetConnector;
  end;

procedure Register;

implementation

uses
  InstantConsts, InstantFlashFilerConnectionDefEdit, Controls;

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantFlashFilerConnector]);
end;

{ TInstantFlashFilerConnectionDef }

class function TInstantFlashFilerConnectionDef.ConnectionTypeName: string;
begin
  Result := 'FlashFiler';
end;

class function TInstantFlashFilerConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantFlashFilerConnector;
end;

function TInstantFlashFilerConnectionDef.Edit: Boolean;
begin
  with TInstantFlashFilerConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

procedure TInstantFlashFilerConnectionDef.InitConnector(
  Connector: TInstantConnector);
var
  Database: TffDatabase;
begin
  inherited;
  Database := TffDatabase.Create(Connector);
  try
    Database.Timeout := 0;
    Database.AutoDatabaseName := True;
    Database.AliasName := AliasName;
    (Connector as TInstantFlashFilerConnector).Database := Database;
  except
    Database.Free;
    raise;
  end;
end;

{ TInstantFlashFilerConnector }

class function TInstantFlashFilerConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantFlashFilerConnectionDef;
end;

function TInstantFlashFilerConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantFlashFilerBroker.Create(Self);
end;

function TInstantFlashFilerConnector.GetConnected: Boolean;
begin
  Result := Assigned(Database) and Database.Connected;
end;

function TInstantFlashFilerConnector.GetDatabaseName: string;
begin
  Result := Database.DatabaseName;
end;

function TInstantFlashFilerConnector.GetDBMSName: string;
begin
  Result := 'FlashFiler';
end;

procedure TInstantFlashFilerConnector.InternalBuildDatabase(Scheme: TInstantScheme);

  procedure CreateTable(TableMetadata: TInstantTableMetadata);
  const
    FieldTypes: array[TInstantDataType] of TFieldType =
      (ftInteger, ftFloat, ftBCD, ftBoolean, ftString, ftMemo, ftDateTime, ftBlob);
  var
    I: Integer;
    Table: TFlashFilerTable;
    IndexName: string;
  begin
    Table := TFlashFilerTable.Create(nil);
    try
      Table.TableName := TableMetadata.Name;
      Table.SessionName := Database.SessionName;
      Table.DatabaseName := DatabaseName;
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

procedure TInstantFlashFilerConnector.InternalCommitTransaction;
begin
  Database.Commit;
end;

procedure TInstantFlashFilerConnector.InternalConnect;
begin
  Database.Open;
end;

procedure TInstantFlashFilerConnector.InternalDisconnect;
begin
  Database.Close;
end;

procedure TInstantFlashFilerConnector.InternalRollbackTransaction;
begin
  Database.Rollback;
end;

procedure TInstantFlashFilerConnector.InternalStartTransaction;
begin
  Database.StartTransaction;
end;

{ TInstantFlashFilerBroker }

function TInstantFlashFilerBroker.CreateResolver(
  const TableName: string): TInstantResolver;
begin
  Result := TInstantFlashFilerResolver.Create(Self, TableName);
end;

function TInstantFlashFilerBroker.GetConnector: TInstantFlashFilerConnector;
begin
  Result := inherited Connector as TInstantFlashFilerConnector;
end;

function TInstantFlashFilerBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantFlashFilerQuery.Create(Connector);
end;

{ TInstantFlashFilerResolver }

function TInstantFlashFilerResolver.CreateDataSet: TDataSet;
begin
  Result := TFlashFilerTable.Create(nil);
  with TFlashFilerTable(Result) do
  try
    SessionName := Broker.Connector.Database.SessionName;
    DatabaseName := Broker.Connector.DatabaseName;
    TableName := Self.TableName;
    IndexFieldNames := InstantIndexFieldNames;
  except
    Free;
    raise;
  end;
end;

function TInstantFlashFilerResolver.GetBroker: TInstantFlashFilerBroker;
begin
  Result := inherited Broker as TInstantFlashFilerBroker;
end;

function TInstantFlashFilerResolver.GetDataSet: TFlashFilerTable;
begin
  Result := inherited DataSet as TFlashFilerTable;
end;

function TInstantFlashFilerResolver.Locate(const AClassName,
  AObjectId: string): Boolean;
begin
  Result := DataSet.FindKey([AClassName, AObjectId]);
end;

{ TInstantFlashFilerQuery }

destructor TInstantFlashFilerQuery.Destroy;
begin
  FQuery.Free;
  inherited;
end;

function TInstantFlashFilerQuery.GetConnector: TInstantFlashFilerConnector;
begin
  Result := inherited Connector as TInstantFlashFilerConnector;
end;

function TInstantFlashFilerQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TInstantFlashFilerQuery.GetParams: TParams;
begin
  Result := Query.Params;
end;

function TInstantFlashFilerQuery.GetQuery: TFlashFilerQuery;
begin
  if not Assigned(FQuery) then
  begin
    FQuery := TFlashFilerQuery.Create(nil);
    FQuery.Name := Connector.DatabaseName + 'Query';
    FQuery.SessionName := Connector.Database.SessionName;
    FQuery.DatabaseName := Connector.DatabaseName;
  end;
  Result := FQuery;
end;

function TInstantFlashFilerQuery.GetStatement: string;
begin
  Result := Query.SQL.Text;
end;

function TInstantFlashFilerQuery.IsSequenced: Boolean;
begin
  Result := Query.IsSequenced;
end;

procedure TInstantFlashFilerQuery.SetParams(Value: TParams);
begin
  Query.Params := Value;
end;

procedure TInstantFlashFilerQuery.SetStatement(const Value: string);
begin
  Query.SQL.Text := Value;
end;

class function TInstantFlashFilerQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantFlashFilerTranslator;
end;

{ TInstantFlashFilerTranslator }

function TInstantFlashFilerTranslator.GetQuote: Char;
begin
  Result := '''';
end;

function TInstantFlashFilerTranslator.IncludeOrderFields: Boolean;
begin
  Result := True;
end;

{ TFlashFilerQuery }

procedure TFlashFilerQuery.SetRecNo(Value: Integer);
begin
  inherited;
  if Value = Succ(RecNo) then
    Next
  else if Value = Pred(RecNo) then
    Prior;
end;

initialization
  RegisterClass(TInstantFlashFilerConnectionDef);
  TInstantFlashFilerConnector.RegisterClass;

finalization
  TInstantFlashFilerConnector.UnregisterClass;

end.
