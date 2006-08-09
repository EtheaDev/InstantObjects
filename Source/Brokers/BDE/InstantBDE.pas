(*
 *   InstantObjects
 *   BDE Support
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
 * Carlo Barazzetta, Nando Dessena, Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantBDE;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

{$IFDEF D6+}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Classes, Db, DBTables, SysUtils, InstantPersistence, InstantCommand,
  InstantBrokers, InstantMetadata, InstantTypes, InstantConsts;

type
  TInstantBDEDriverType = (dtUnknown, dtStandard, dtInterBase, dtMSAccess,
    dtMSSQLServer, dtOracle, dtDB2);

  TInstantBDEConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FAliasName: string;
    FDriverName: string;
    FParameters: string;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property AliasName: string read FAliasName write FAliasName;
    property DriverName: string read FDriverName write FDriverName;
    property Parameters: string read FParameters write FParameters;
  end;

  TInstantBDEConnector = class(TInstantConnectionBasedConnector)
  private
    FOnLogin: TDatabaseLoginEvent;
    function GetConnection: TDatabase;
    procedure SetConnection(const Value: TDatabase);
    function GetDriverType: TInstantBDEDriverType;
  protected
    procedure AssignLoginOptions; override;//CB
    function CreateBroker: TInstantBroker; override;
    function GetDatabaseExists: Boolean; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    function GetDDLTransactionSupported: Boolean; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    property DriverType: TInstantBDEDriverType read GetDriverType;
  published
    property Connection: TDatabase read GetConnection write SetConnection;
    property OnLogin: TDatabaseLoginEvent read FOnLogin write FOnLogin; //CB
  end;

  TInstantBDEBroker = class(TInstantNavigationalBroker)
  private
    function GetConnector: TInstantBDEConnector;
  protected
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog;
        override;
    function CreateResolver(const TableName: string): TInstantResolver; override;
    function InternalCreateQuery: TInstantQuery; override;
  public
    function CreateDBBuildCommand(const CommandType: TInstantDBBuildCommandType):
        TInstantDBBuildCommand; override;
    property Connector: TInstantBDEConnector read GetConnector;
  end;

  TInstantBDEResolver = class(TInstantResolver)
  private
    function GetBroker: TInstantBDEBroker;
    function GetDataSet: TTable;
  protected
    function CreateDataSet: TDataSet; override;
    function CreateNavigationalLinkResolver(const ATableName: string):
        TInstantNavigationalLinkResolver; override;
    function Locate(const AClassName, AObjectId: string): Boolean; override;
    function TranslateError(AObject: TInstantObject;
      E: Exception): Exception; override;
  public
    function FormatTableName(const ATableName: string): string; virtual;
    property Broker: TInstantBDEBroker read GetBroker;
    property DataSet: TTable read GetDataSet;
  end;

  TInstantBDEQuery = class;

  TInstantBDETranslator = class(TInstantRelationalTranslator)
  private
    function GetQuery: TInstantBDEQuery;
  protected
    function GetDelimiters: string; override;
    function GetQuote: Char; override;
    function GetWildcard: string; override;
    function IncludeOrderFields: Boolean; override;
    function TranslateFunction(AFunction: TInstantIQLFunction; Writer: TInstantIQLWriter): Boolean; override;
    function TranslateFunctionName(const FunctionName: string; Writer: TInstantIQLWriter): Boolean; override;
  public
    property Query: TInstantBDEQuery read GetQuery;
  end;

  TInstantBDEQuery = class(TInstantNavigationalQuery)
  private
    FQuery: TQuery;
    function GetQuery: TQuery;
    function GetConnector: TInstantBDEConnector;
  protected
    function GetDataSet: TDataSet; override;
    function GetParams: TParams; override;
    function GetStatement: string; override;
    function IsSequenced: Boolean; override;
    procedure SetParams(Value: TParams); override;
    procedure SetStatement(const Value: string); override;
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
    property Query: TQuery read GetQuery;
  public
    destructor Destroy; override;
    property Connector: TInstantBDEConnector read GetConnector;
  end;

  // Base class for all steps that work by executing one or more commands
  // (that is, a script) each.
  TInstantDBBuildBDECommand = class(TInstantDBBuildCommand)
  private
    function GetConnector: TInstantBDEConnector;
    function GetBroker: TInstantBDEBroker;
  protected
    // Returns The number of statements that compound this script. The
    // predefined implementation returns 1.
    function GetCommandCount: Integer; virtual;
    // Returns the nth command that is part of this script. Valid values
    // are in the range 0 to Pred(GetCommandCount). The default
    // implementation, which should always be called through inherited at the
    // beginning of the overridden version, just returns '', or raises an
    // exception if Index is not in the allowed range.
    function GetCommand(const Index: Integer): string; virtual;
  public
    property Connector: TInstantBDEConnector read GetConnector;
    property Broker: TInstantBDEBroker read GetBroker;
  end;

  TInstantDBBuildBDEAddTableCommand = class(TInstantDBBuildBDECommand)
  private
    function GetTableMetadata: TInstantTableMetadata;
  protected
    procedure InternalExecute; override;
  public
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

  TInstantDBBuildBDEDropTableCommand = class(TInstantDBBuildBDECommand)
  private
    function GetTableMetadata: TInstantTableMetadata;
  protected
    procedure InternalExecute; override;
  public
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

  TInstantDBBuildBDEAddIndexCommand = class(TInstantDBBuildBDECommand)
  private
    function GetIndexMetadata: TInstantIndexMetadata;
  protected
    procedure InternalExecute; override;
  public
    property IndexMetadata: TInstantIndexMetadata read GetIndexMetadata;
  end;

  TInstantDBBuildBDEDropIndexCommand = class(TInstantDBBuildBDECommand)
  private
    function GetIndexMetadata: TInstantIndexMetadata;
  protected
    procedure InternalExecute; override;
  public
    property IndexMetadata: TInstantIndexMetadata read GetIndexMetadata;
  end;

type
  TInstantBDELinkResolver = class(TInstantNavigationalLinkResolver)
  private
    function GetBroker: TInstantBDEBroker;
    function GetDataSet: TTable;
    function GetResolver: TInstantBDEResolver;
  protected
    function CreateDataSet: TDataSet; override;
    procedure SetDatasetParentRange(const AParentClass, AParentId: string);
        override;
  public
    constructor Create(AResolver: TInstantNavigationalResolver; const ATableName:
        string);
    property Broker: TInstantBDEBroker read GetBroker;
    property DataSet: TTable read GetDataSet;
    property Resolver: TInstantBDEResolver read GetResolver;
  end;

procedure Register;
implementation

uses
  Bde, Controls, InstantBDEConnectionDefEdit, InstantBDECatalog;

resourcestring
  SCommandIndexOutOfBounds = 'Command index out of bounds.';

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantBDEConnector]);
end;

{ TInstantBDEConnectionDef }

class function TInstantBDEConnectionDef.ConnectionTypeName: string;
begin
  Result := 'BDE';
end;

class function TInstantBDEConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantBDEConnector;
end;

function TInstantBDEConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
var
  Connection: TDatabase;
begin
  Connection := TDatabase.Create(AOwner);
  try
    Connection.DatabaseName := Name;
    if AliasName <> '' then
      Connection.AliasName := AliasName
    else
      Connection.DriverName := DriverName;
    Connection.Params.Text := Parameters;
    Connection.TransIsolation := tiDirtyRead;
  except
    Connection.Free;
    raise;
  end;
  Result := Connection;
end;

function TInstantBDEConnectionDef.Edit: Boolean;
begin
  with TInstantBDEConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

{ TInstantBDEConnector }

procedure TInstantBDEConnector.AssignLoginOptions;
begin
  inherited;
  if HasConnection then
  begin
    if Assigned(FOnLogin) and not Assigned(Connection.OnLogin) then
      Connection.OnLogin := FOnLogin;
  end;
end;

class function TInstantBDEConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantBDEConnectionDef;
end;

function TInstantBDEConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantBDEBroker.Create(Self);
end;

function TInstantBDEConnector.GetConnection: TDatabase;
begin
  Result := inherited Connection as TDatabase
end;

function TInstantBDEConnector.GetDatabaseExists: Boolean;
var
  SearchRec: TSearchRec;
  Path: string;
  Params: TStringList;
begin
  if DriverType = dtStandard then
  begin
    if Connection.AliasName = '' then
      Path := Connection.Params.Values['PATH']
    else begin
      Params := TStringList.Create;
      try
        Session.GetAliasParams(Connection.AliasName, Params);
        Path := Params.Values['PATH'];
      finally
        Params.Free;
      end;
    end;
    Path := IncludeTrailingBackslash(Path);
    Result := FindFirst(Path + '*.*', faReadOnly + faArchive, SearchRec) = 0;
    FindClose(SearchRec);
  end else
    Result := inherited GetDatabaseExists;
end;

function TInstantBDEConnector.GetDatabaseName: string;
begin
  with Connection do
    if AliasName = '' then
    begin
      Result := Params.Values['PATH'];
      if Result = '' then
        Result := DatabaseName;
    end else
      Result := AliasName;
end;

function TInstantBDEConnector.GetDBMSName: string;
begin
  if HasConnection then
    if Connection.Drivername <> '' then
      Result := Connection.DriverName
    else if Connection.AliasName <> '' then
      Result := Session.GetAliasDriverName(Connection.AliasName)
    else
      Result := ''
  else
    Result := '';
end;

function TInstantBDEConnector.GetDDLTransactionSupported: Boolean;
begin
  Result := False;
end;

function TInstantBDEConnector.GetDriverType: TInstantBDEDriverType;
begin
  if HasConnection then
  begin
    if SameText(DBMSName, 'STANDARD') then
      Result := dtStandard
    else if SameText(DBMSName, 'INTRBASE') then
      Result := dtInterBase
    else if SameText(DBMSName, 'MSACCESS') then
      Result := dtMSAccess
    else if SameText(DBMSName, 'MSSQL') or
      SameText(DBMSName, 'SQL Server') then
      Result := dtMSSQLServer
    else if SameText(DBMSName, 'ORACLE') then
      Result := dtOracle
    else if SameText(DBMSName, 'DB2') or
      SameText(DBMSName, 'IBM DB2 ODBC DRIVER') then
      Result := dtDB2
    else
      Result := dtUnknown;
  end else
    Result := dtUnknown;
end;

procedure TInstantBDEConnector.InternalBuildDatabase(Scheme: TInstantScheme);

  procedure CreateTable(TableMetadata: TInstantTableMetadata);
  const
    FieldTypes: array[TInstantDataType] of TFieldType =
      (ftInteger, ftFloat, ftBCD, ftBoolean, ftString, ftMemo, ftDateTime, ftBlob);
  var
    I: Integer;
    Table: TTable;
    IndexName: string;
  begin
    Table := TTable.Create(nil);
    try
      Table.TableName := TableMetadata.Name;
      Table.DatabaseName := Connection.DatabaseName;
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

procedure TInstantBDEConnector.InternalCommitTransaction;
begin
  Connection.Commit;
end;

procedure TInstantBDEConnector.InternalRollbackTransaction;
begin
  Connection.Rollback;
end;

procedure TInstantBDEConnector.InternalStartTransaction;
begin
  Connection.StartTransaction;
end;

procedure TInstantBDEConnector.SetConnection(const Value: TDatabase);
begin
  inherited Connection := Value;
end;

function TInstantBDEBroker.CreateCatalog(const AScheme: TInstantScheme):
    TInstantCatalog;
begin
  Result := TInstantBDECatalog.Create(AScheme, Self);
end;

function TInstantBDEBroker.CreateDBBuildCommand(const CommandType:
    TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  if CommandType = ctAddTable then
    Result := TInstantDBBuildBDEAddTableCommand.Create(CommandType, Connector)
  else if CommandType = ctDropTable then
    Result := TInstantDBBuildBDEDropTableCommand.Create(CommandType, Connector)
  else if CommandType = ctAddIndex then
    Result := TInstantDBBuildBDEAddIndexCommand.Create(CommandType, Connector)
  else if CommandType = ctDropIndex then
    Result := TInstantDBBuildBDEDropIndexCommand.Create(CommandType, Connector)
  else
    Result := inherited CreateDBBuildCommand(CommandType);
end;

{ TInstantBDEBroker }

function TInstantBDEBroker.CreateResolver(
  const TableName: string): TInstantResolver;
begin
  Result := TInstantBDEResolver.Create(Self, TableName);
end;

function TInstantBDEBroker.GetConnector: TInstantBDEConnector;
begin
  Result := inherited Connector as TInstantBDEConnector;
end;

function TInstantBDEBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantBDEQuery.Create(Connector);
end;

{ TInstantBDEResolver }

function TInstantBDEResolver.CreateDataSet: TDataSet;
begin
  Result:= TTable.Create(nil);
  with TTable(Result) do
  try
    DatabaseName := Broker.Connector.Connection.DatabaseName;
    TableName := FormatTableName(Self.TableName);
    CacheBlobs := False;
    UpdateMode := upWhereKeyOnly;
    IndexFieldNames := InstantIndexFieldNames;
  except
    Result.Free;
    raise;
  end;
end;

function TInstantBDEResolver.CreateNavigationalLinkResolver(const ATableName:
    string): TInstantNavigationalLinkResolver;
begin
  Result := TInstantBDELinkResolver.Create(Self, ATableName);
end;

function TInstantBDEResolver.FormatTableName(const ATableName: string): string;
begin
  if Broker.Connector.DriverType = dtOracle then
    Result := UpperCase(ATableName)
  else
    Result := ATableName;
end;

function TInstantBDEResolver.GetBroker: TInstantBDEBroker;
begin
  Result := inherited Broker as TInstantBDEBroker;
end;

function TInstantBDEResolver.GetDataSet: TTable;
begin
  Result := inherited DataSet as TTable;
end;

function TInstantBDEResolver.Locate(const AClassName, AObjectId: string):
    Boolean;
begin
  Result := DataSet.FindKey([AClassName, AObjectId]);
end;

function TInstantBDEResolver.TranslateError(
  AObject: TInstantObject; E: Exception): Exception;
var
  Error: TDBError;
begin
  Result := nil;
  if E is EDBEngineError then
    with EDBEngineError(E) do
      if (ErrorCount > 0) then
      begin
        Error := Errors[Pred(ErrorCount)];
        if (Error.Category = ERRCAT_INTEGRITY) and
          (Error.SubCode = ERRCODE_KEYVIOL) then
          Result := KeyViolation(AObject, AObject.Id, E)
      end;
end;

{ TInstantBDEQuery }

destructor TInstantBDEQuery.Destroy;
begin
  FreeAndNil(FQuery);
  inherited;
end;

function TInstantBDEQuery.GetConnector: TInstantBDEConnector;
begin
  Result := inherited Connector as TInstantBDEConnector;
end;

function TInstantBDEQuery.GetDataSet: TDataSet;
begin
  Result := Query;
end;

function TInstantBDEQuery.GetParams: TParams;
begin
  Result := Query.Params;
end;

function TInstantBDEQuery.GetQuery: TQuery;
begin
  if not Assigned(FQuery) then
  begin
    FQuery := TQuery.Create(nil);
    if Connector.HasConnection then
      FQuery.DatabaseName := (Connector.Connection as TDatabase).DatabaseName;
  end;
  Result := FQuery;
end;

function TInstantBDEQuery.GetStatement: string;
begin
  Result := Query.SQL.Text;
end;

function TInstantBDEQuery.IsSequenced: Boolean;
begin
  Result := Query.IsSequenced;
end;

procedure TInstantBDEQuery.SetParams(Value: TParams);
begin
  Query.Params := Value;
end;

procedure TInstantBDEQuery.SetStatement(const Value: string);
begin
  Query.SQL.Text := Value;
end;

class function TInstantBDEQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantBDETranslator;
end;

{ TInstantBDETranslator }

function TInstantBDETranslator.GetDelimiters: string;
begin
  case Query.Connector.DriverType of
    dtStandard, dtDB2:
      Result := '""';
    dtMSAccess, dtMSSQLServer:
      Result := '[]';
  else
    Result := inherited GetDelimiters;
  end;
end;

function TInstantBDETranslator.GetQuery: TInstantBDEQuery;
begin
  Result := inherited Query as TInstantBDEQuery;
end;

function TInstantBDETranslator.GetQuote: Char;
begin
  if Query.Connector.DriverType in [dtMSSQLServer, dtOracle, dtDB2] then
    Result := ''''
  else
    Result := inherited GetQuote;
end;

function TInstantBDETranslator.GetWildcard: string;
begin
  if Query.Connector.DriverType = dtMSAccess then
    Result := '*'
  else
    Result := inherited GetWildcard;
end;

function TInstantBDETranslator.IncludeOrderFields: Boolean;
begin
  Result := Query.Connector.DriverType = dtStandard;
end;

function TInstantBDETranslator.TranslateFunction(
  AFunction: TInstantIQLFunction; Writer: TInstantIQLWriter): Boolean;
begin
  if (Query.Connector.DriverType = dtStandard) and
    SameText(AFunction.FunctionName, 'SUBSTRING') then
    with AFunction do
    begin
      Writer.WriteString(FunctionName);
      Writer.WriteChar('(');
      if Assigned(Parameters) then
      begin
        Parameters.Expression.Write(Writer);
      end;
      if Assigned(Parameters.NextParameters) then
      begin
        Writer.WriteString(' FROM ');
        Parameters.NextParameters.Expression.Write(Writer);
      end;
      if Assigned(Parameters.NextParameters.NextParameters) then
      begin
        Writer.WriteString(' FOR ');
        Parameters.NextParameters.NextParameters.Expression.Write(Writer);
      end;
      Writer.WriteChar(')');
      Result := True;
    end
  else
    Result := inherited TranslateFunction(AFunction, Writer);
end;

function TInstantBDETranslator.TranslateFunctionName(
  const FunctionName: string; Writer: TInstantIQLWriter): Boolean;
begin
  Result := True;
  case Query.Connector.DriverType of
    dtMSAccess:
      if SameText(FunctionName, 'UPPER') then
        Writer.WriteString('UCASE')
      else if SameText(FunctionName, 'LOWER') then
        Writer.WriteString('LCASE')
      else if SameText(FunctionName, 'SUBSTRING') then
        Writer.WriteString('MID')
      else
        Result := False;
    dtOracle:
      if SameText(FunctionName, 'SUBSTRING') then
        Writer.WriteString('SUBSTR')
      else
        Result := False;
  else
    Result := False;
  end;
end;

{ TInstantDBBuildBDECommand }

function TInstantDBBuildBDECommand.GetBroker: TInstantBDEBroker;
begin
  Result := Connector.Broker as TInstantBDEBroker;
end;

function TInstantDBBuildBDECommand.GetConnector: TInstantBDEConnector;
begin
  Result := inherited Connector as TInstantBDEConnector;
end;

function TInstantDBBuildBDECommand.GetCommand(const Index: Integer): string;
begin
  if (Index < 0) or (Index >= GetCommandCount) then
    raise EInstantDBBuildError.CreateFmt(SCommandIndexOutOfBounds,
      [Index]);
  Result := '';
end;

function TInstantDBBuildBDECommand.GetCommandCount: Integer;
begin
  Result := 1;
end;

function TInstantDBBuildBDEAddTableCommand.GetTableMetadata:
    TInstantTableMetadata;
begin
  Result := NewMetadata as TInstantTableMetadata;
end;

procedure TInstantDBBuildBDEAddTableCommand.InternalExecute;
const
  FieldTypes: array[TInstantDataType] of TFieldType =
    (ftInteger, ftFloat, ftCurrency, ftBoolean, ftString, ftMemo, ftDateTime, ftBlob);
var
  I: Integer;
  Table: TTable;
  IndexName: string;
begin
  Table := TTable.Create(nil);
  try
    Table.TableName := TableMetadata.Name;
    Table.DatabaseName := Connector.Connection.DatabaseName;
    with TableMetadata do
    begin
      for I := 0 to Pred(FieldMetadatas.Count) do
        with FieldMetadatas[I] do
          if FieldTypes[DataType] in [ftString, ftMemo, ftBlob] then
            Table.FieldDefs.Add(Name, FieldTypes[DataType], Size,
                    foRequired in Options)
          else
            Table.FieldDefs.Add(Name, FieldTypes[DataType], 0,
                    foRequired in Options);

      Table.IndexDefs.Clear;
      for I := 0 to Pred(IndexMetadatas.Count) do
        // Only add the primary index here
        if ixPrimary in IndexMetadatas[I].Options then
        begin
          IndexName := IndexMetadatas[I].Name;
          if IndexName = '' then
            IndexName := Table.TableName + '_' + 'ID';
          with Table.IndexDefs.AddIndexDef do
          begin
            Name := IndexName;
            Fields := IndexMetadatas[I].Fields;
            Options := IndexMetadatas[I].Options;
          end;
      end;
    end;
    Table.CreateTable;
  finally
    Table.Free;
  end;
end;

function TInstantDBBuildBDEDropTableCommand.GetTableMetadata:
    TInstantTableMetadata;
begin
  Result := OldMetadata as TInstantTableMetadata;
end;

procedure TInstantDBBuildBDEDropTableCommand.InternalExecute;
var
  Table: TTable;
begin
  Table := TTable.Create(nil);
  try
    Table.TableName := TableMetadata.Name;
    Table.DatabaseName := Connector.Connection.DatabaseName;
    Table.Close;
    Table.DeleteTable;
  finally
    Table.Free;
  end;
end;

{ TInstantDBBuildAddIndexSQLCommand }

function TInstantDBBuildBDEAddIndexCommand.GetIndexMetadata:
    TInstantIndexMetadata;
begin
  Result := NewMetadata as TInstantIndexMetadata;
end;

procedure TInstantDBBuildBDEAddIndexCommand.InternalExecute;
var
  Table: TTable;
begin
  Table := TTable.Create(nil);
  try
    Table.TableName := IndexMetadata.TableMetadata.Name;
    Table.DatabaseName := Connector.Connection.DatabaseName;
    with IndexMetadata do
    begin
      if not (ixPrimary in Options) then
      begin
        if (Connector.DriverType = dtStandard) and (Pos(';', Fields) = 0) then
          // Name of a non-composite index must the be field name for Paradox.
          Name := Fields;  
        Table.AddIndex(Name, Fields, Options);
      end;
    end;
  finally
    Table.Free;
  end;
end;

{ TInstantDBBuildAddIndexSQLCommand }

function TInstantDBBuildBDEDropIndexCommand.GetIndexMetadata:
    TInstantIndexMetadata;
begin
  Result := OldMetadata as TInstantIndexMetadata;
end;

procedure TInstantDBBuildBDEDropIndexCommand.InternalExecute;
var
  Table: TTable;

  function RemoveTableNamePrefix(const AName, ATableName: String): String;
  begin
    Result := AName;
    if Pos(ATableName, Result) = 1 then
      Result := Copy(Result, 1 + Length(ATableName), Length(Result));
  end;
begin
  Table := TTable.Create(nil);
  try
    Table.TableName := IndexMetadata.TableMetadata.Name;
    Table.DatabaseName := Connector.Connection.DatabaseName;
    Table.Exclusive := True;
    Table.Open;
    with IndexMetadata do
      if not (ixPrimary in Options) then
        Table.DeleteIndex(
                RemoveTableNamePrefix(Name, IndexMetadata.TableMetadata.Name));
    Table.Close;
  finally
    Table.Free;
  end;
end;

constructor TInstantBDELinkResolver.Create(AResolver:
    TInstantNavigationalResolver; const ATableName: string);
begin
  inherited Create(AResolver, ATableName);
end;

function TInstantBDELinkResolver.CreateDataSet: TDataSet;
begin
  Result:= TTable.Create(nil);
  with TTable(Result) do
  try
    DatabaseName := Broker.Connector.Connection.DatabaseName;
    TableName := Resolver.FormatTableName(Self.TableName);
    IndexFieldNames := InstantParentClassFieldName + ';' +
        InstantParentIdFieldName;
    CacheBlobs := False;
    UpdateMode := upWhereKeyOnly;
  except
    Result.Free;
    raise;
  end;
end;

function TInstantBDELinkResolver.GetBroker: TInstantBDEBroker;
begin
  Result := inherited Broker as TInstantBDEBroker;
end;

function TInstantBDELinkResolver.GetDataSet: TTable;
begin
  Result := inherited DataSet as TTable;
end;

function TInstantBDELinkResolver.GetResolver: TInstantBDEResolver;
begin
  Result := inherited Resolver as TInstantBDEResolver;
end;

procedure TInstantBDELinkResolver.SetDatasetParentRange(const
    AParentClass, AParentId: string);
begin
  Dataset.SetRange([AParentClass, AParentId], [AParentClass, AParentId]);
end;


initialization
  RegisterClass(TInstantBDEConnectionDef);
  TInstantBDEConnector.RegisterClass;

finalization
  TInstantBDEConnector.UnregisterClass;

end.
