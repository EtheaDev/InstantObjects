(*
 *   InstantObjects
 *   Custom Broker Template
 *
 *   Instructions for creating custom brokers:
 *
 *   1. Implement the code as specified in all TODO items.
 *   2. Design the ConnectionDef edit form.
 *   3. Build and install the package in the IDE.
 *
 *)

unit InstantXXX;

interface

uses
  Classes, DB, InstantPersistence, InstantCommand;

type
  TInstantXXXConnectionDef = class(TInstantConnectionBasedConnectionDef)
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  end;

  TInstantXXXConnector = class(TInstantRelationalConnector)
  protected
    function CreateBroker: TInstantBroker; override;
    function GetDatabaseName: string; override;
    function GetDBMSName: string; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
  end;

  TInstantXXXBroker = class(TInstantRelationalBroker)
  protected
    function CreateResolver(const TableName: string): TInstantResolver; override;
  end;

  TInstantXXXResolver = class(TInstantResolver)
  protected
    function CreateDataSet: TDataSet; override;
    function Locate(const AClassName, AObjectId: string): Boolean; override;
  end;

  TInstantXXXTranslator = class(TInstantRelationalTranslator)
  protected
    function GetDelimiters: string; override;
    function GetQuote: Char; override;
    function GetWildcard: string; override;
    function TranslateFunction(AFunction: TInstantIQLFunction; Writer: TInstantIQLWriter): Boolean; override;
    function TranslateFunctionName(const FunctionName: string; Writer: TInstantIQLWriter): Boolean; override;
  end;

  TInstantXXXQuery = class(TInstantRelationalQuery)
  protected
    function GetDataSet: TDataSet; override;
    function GetStatement: string; override;
    procedure SetStatement(const Value: string); override;
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
  end;

procedure Register;

implementation

uses
  InstantXXXConnectionDefEdit, Controls;

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantXXXConnector]);
end;

{ TInstantXXXConnectionDef }

class function TInstantXXXConnectionDef.ConnectionTypeName: string;
begin
  Result := 'XXX';
end;

class function TInstantXXXConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantXXXConnector;
end;

function TInstantXXXConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
begin
  { TODO: Create and return connection instance }
  Result := nil;
end;

function TInstantXXXConnectionDef.Edit: Boolean;
begin
  with TInstantXXXConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

{ TInstantXXXBroker }

function TInstantXXXBroker.CreateResolver(
  const TableName: string): TInstantResolver;
begin
  Result := TInstantXXXResolver.Create(Self, TableName);
end;

{ TInstantXXXResolver }

function TInstantXXXResolver.CreateDataSet: TDataSet;
begin
  { TODO: Create and return dataset instance }
  Result := nil;
end;

function TInstantXXXResolver.Locate(const AClassName,
  AObjectId: string): Boolean;
begin
  { TODO: Locate row in dataset }
  Result := False;
end;

{ TInstantXXXConnector }

class function TInstantXXXConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantXXXConnectionDef;
end;

function TInstantXXXConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantXXXBroker.Create(Self);
end;

function TInstantXXXConnector.GetDatabaseName: string;
begin
  { TODO: Return name of database }
  Result := '';
end;

function TInstantXXXConnector.GetDBMSName: string;
begin
  { TODO: Return name of DBMS }
  Result := '';
end;

procedure TInstantXXXConnector.InternalBuildDatabase(Scheme: TInstantScheme);
begin
  Scheme.BlobStreamFormat := BlobStreamFormat; //CB  
  { TODO: Build database from Scheme }
end;

procedure TInstantXXXConnector.InternalCommitTransaction;
begin
  { TODO: Commit transaction for Connection }
end;

function TInstantXXXConnector.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantXXXQuery.Create(Self);
end;

procedure TInstantXXXConnector.InternalRollbackTransaction;
begin
  { TODO: Roll back transaction for Connection }
end;

procedure TInstantXXXConnector.InternalStartTransaction;
begin
  { TODO: Start transaction for Connection }
end;

{ TInstantXXXQuery }

function TInstantXXXQuery.GetDataSet: TDataSet;
begin
  { TODO: Return wrapped query component }
  Result := nil;
end;

function TInstantXXXQuery.GetStatement: string;
begin
  { TODO: Return SQL statement from wrapped query component }
end;

procedure TInstantXXXQuery.SetStatement(const Value: string);
begin
  { TODO: Set SQL statement of wrapped query component }
end;

class function TInstantXXXQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantXXXTranslator;
end;

{ TInstantXXXTranslator }

function TInstantXXXTranslator.GetDelimiters: string;
begin
  { TODO: Return two-char string of field-surrounding delimiters, eg. '[]' }
  Result := inherited GetDelimiters;
end;

function TInstantXXXTranslator.GetQuote: Char;
begin
  { TODO: Return quote char to be used around strings, eg. '"' }
  Result := inherited GetQuote;
end;

function TInstantXXXTranslator.GetWildcard: string;
begin
  { TODO: Return wildcard string, eg. '*' }
  Result := inherited GetWildcard;
end;

function TInstantXXXTranslator.TranslateFunction(
  AFunction: TInstantIQLFunction; Writer: TInstantIQLWriter): Boolean;
begin
  { TODO: Write translated function with supplied writer }
  Result := inherited TranslateFunction(AFunction, Writer);
end;

function TInstantXXXTranslator.TranslateFunctionName(
  const FunctionName: string; Writer: TInstantIQLWriter): Boolean;
begin
  { TODO: Write translated function name with supplied writer }
  Result := False;
end;

initialization
  RegisterClass(TInstantXXXConnectionDef);
  TInstantXXXConnector.RegisterClass;

finalization
  TInstantXXXConnector.UnregisterClass;

end.
