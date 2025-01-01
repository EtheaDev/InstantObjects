(*
 *   InstantObjects
 *   Database build classes
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
 * The Original Code is: InstantObjects Database builder
 *
 * The Initial Developer of the Original Code is: Nando Dessena
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBBuild;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  System.SysUtils
  , System.Classes
  , System.Contnrs
  , InstantClasses
  , InstantPersistence
  , InstantConsts
  , InstantTypes
  , InstantBrokers
  , InstantMetadata
  ;
  
type
  TInstantDBBuildCommandSequence = class;

  TInstantDBBuildCommandNotifyEvent = procedure (const Sender: TObject;
    const ACommand: TInstantDBBuildCommand) of object;

  TInstantDBBuildCommandErrorEvent = procedure (const Sender: TObject;
    const ACommand: TInstantDBBuildCommand; const Error: Exception;
    var RaiseError: Boolean) of object;

  // Builds a TInstantDBBuildCommandSequence. Abstract.
  // Works closely with the connector and the broker. The command
  // sequence, once built, can be executed through the connector.
  TInstantCustomDBBuilder = class(TComponent)
  private
    FConnector: TInstantConnector;
    FCommandSequence: TInstantDBBuildCommandSequence;
    FOnWarning: TInstantWarningEvent;
    function GetAfterCommandExecute: TInstantDBBuildCommandNotifyEvent;
    function GetAfterCommandSequenceExecute: TNotifyEvent;
    function GetBeforeCommandExecute: TInstantDBBuildCommandNotifyEvent;
    function GetBeforeCommandSequenceExecute: TNotifyEvent;
    procedure SetAfterCommandExecute(
      const Value: TInstantDBBuildCommandNotifyEvent);
    procedure SetAfterCommandSequenceExecute(const Value: TNotifyEvent);
    procedure SetBeforeCommandExecute(
      const Value: TInstantDBBuildCommandNotifyEvent);
    procedure SetBeforeCommandSequenceExecute(const Value: TNotifyEvent);
    function GetCommandExecuteError: TInstantDBBuildCommandErrorEvent;
    procedure SetCommandExecuteError(
      const Value: TInstantDBBuildCommandErrorEvent);
    procedure DoWarning(const WarningText: string);
  protected
    function GetConnector: TInstantConnector; virtual;
    procedure InternalBuildCommandSequence; virtual; abstract;
    procedure SetConnector(const Value: TInstantConnector); virtual;
    // Fired after a successful execution of the entire command sequence.
    property AfterCommandSequenceExecute: TNotifyEvent
      read GetAfterCommandSequenceExecute write SetAfterCommandSequenceExecute;
    // Fired after a successful execution of a command of the sequence.
    property AfterCommandExecute: TInstantDBBuildCommandNotifyEvent
      read GetAfterCommandExecute write SetAfterCommandExecute;
    // Fired when the Execute method of the command sequence is called.
    property BeforeCommandSequenceExecute: TNotifyEvent
      read GetBeforeCommandSequenceExecute write SetBeforeCommandSequenceExecute;
    // Fired before executing a command of the sequence. Can be used to
    // programmatically enable/disable the execution of the step.
    property BeforeCommandExecute: TInstantDBBuildCommandNotifyEvent
      read GetBeforeCommandExecute write SetBeforeCommandExecute;
    // Fired in case of an error during execution of a command. Can be used to
    // customize the error handling behaviour, which by default is to stop
    // the sequence execution, raise an exception and rollback the changes
    // (rollbacking only works if the database back-end supports transactional
    // data definition, not a very commonly available feature).
    property OnCommandExecuteError: TInstantDBBuildCommandErrorEvent
      read GetCommandExecuteError write SetCommandExecuteError;
    procedure SourceSchemeWarningHandler(const Sender: TObject;
      const AWarningText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // A reference to the command sequence.
    // You should call BuildCommandSequence before accessing this property.
    property CommandSequence: TInstantDBBuildCommandSequence
      read FCommandSequence;
    // (Re)builds the command sequence. After calling this method,
    // the CommandSequence property contains the sequence of commands.
    procedure BuildCommandSequence;
    // Used for component reference notifications.
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  published
    // Reference to a Connector that points to the target database for
    // evolution. Default is InstantDefaultConnector.
    property Connector: TInstantConnector read GetConnector write SetConnector;
    property OnWarning: TInstantWarningEvent read FOnWarning write FOnWarning;
  end;

  // Base class for database builders and evolvers. Abstract.
  TInstantCustomDBEvolver = class(TInstantCustomDBBuilder)
  private
    FTargetModel: TInstantModel;
  protected
    function GetTargetModel: TInstantModel; virtual;
    procedure SetTargetModel(const Value: TInstantModel); virtual;
    procedure AppendAddFieldCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const FieldMetadata: TInstantFieldMetadata); virtual;
    procedure AppendAddIndexCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const IndexMetadata: TInstantIndexMetadata); virtual;
    procedure AppendAddTableCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const TableMetadata: TInstantTableMetadata); virtual;
    procedure AppendAlterFieldCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const SourceFieldMetadata,
      TargetFieldMetadata: TInstantFieldMetadata); virtual;
    procedure AppendAlterIndexCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const SourceIndexMetadata,
      TargetIndexMetadata: TInstantIndexMetadata); virtual;
    procedure AppendDropFieldCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const FieldMetadata: TInstantFieldMetadata); virtual;
    procedure AppendDropIndexCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const IndexMetadata: TInstantIndexMetadata); virtual;
    function AppendDropTableCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const TableMetadata: TInstantTableMetadata): TInstantDBBuildCommand; virtual;
  public
    // The reference Model. Default is InstantModel.
    property TargetModel: TInstantModel read GetTargetModel write SetTargetModel;
  published
    property AfterCommandSequenceExecute;
    property AfterCommandExecute;
    property BeforeCommandSequenceExecute;
    property BeforeCommandExecute;
    property OnCommandExecuteError;
  end;

  // Builds a TInstantDBBuildCommandSequence that destroys a database structure
  // and recreates it according to a specified Model. It represents the
  // "classic" InstantObjects database building strategy.
  TInstantDBBuilder = class(TInstantCustomDBEvolver)
  private
    // Adds to CommandSequence the steps needed to rebuild the database.
    procedure GenerateCommandSequence(const CommandSequence: TInstantDBBuildCommandSequence);
  protected
    procedure InternalBuildCommandSequence; override;
    function AppendDropTableCommand(
      const CommandSequence: TInstantDBBuildCommandSequence;
      const TableMetadata: TInstantTableMetadata): TInstantDBBuildCommand; override;
  end;

  // A sequence of commands used to build a database.
  TInstantDBBuildCommandSequence = class(TComponent)
  private
    FCommands: TObjectList;
    FConnector: TInstantConnector;
    FAfterExecute: TNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FAfterCommandExecute: TInstantDBBuildCommandNotifyEvent;
    FBeforeCommandExecute: TInstantDBBuildCommandNotifyEvent;
    FTargetScheme: TInstantScheme;
    FSourceScheme: TInstantScheme;
    FCommandExecuteError: TInstantDBBuildCommandErrorEvent;
    function GetConnector: TInstantConnector;
    procedure SetConnector(const Value: TInstantConnector);
    procedure DoAfterCommandExecute(const ACommand: TInstantDBBuildCommand);
    procedure DoBeforeCommandExecute(const ACommand: TInstantDBBuildCommand);
    procedure DoAfterExecute;
    procedure DoBeforeExecute;
    procedure DoCommandExecuteError(const ACommand: TInstantDBBuildCommand;
      const Error: Exception; var RaiseError: Boolean);
    procedure DoExecute;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TInstantDBBuildCommand;
    procedure SetSourceScheme(const Value: TInstantScheme);
    procedure SetTargetScheme(const Value: TInstantScheme);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Fired after a successful execution of the entire sequence.
    property AfterExecute: TNotifyEvent
      read FAfterExecute write FAfterExecute;
    // Fired after a successful execution of a command of the sequence.
    property AfterCommandExecute: TInstantDBBuildCommandNotifyEvent
      read FAfterCommandExecute write FAfterCommandExecute;
    // Fired when the Execute method is called.
    property BeforeExecute: TNotifyEvent
      read FBeforeExecute write FBeforeExecute;
    // Fired before executing a command of the sequence. Can be used to
    // programmatically enable/disable the execution of the step.
    property BeforeCommandExecute: TInstantDBBuildCommandNotifyEvent
      read FBeforeCommandExecute write FBeforeCommandExecute;
    // Connector against which to execute the sequence of commands. Default is
    // InstantDefaultConnector.
    property Connector: TInstantConnector read GetConnector write SetConnector;
    // Number of commands in the sequence.
    property Count: Integer read GetCount;
    // Random access to the commands.
    property Items[const Index: Integer]: TInstantDBBuildCommand read GetItem; default;
    // Fired in case of an error during execution of a command. Can be used to
    // customize the error handling behaviour, which by default is to stop
    // the sequence execution, raise an exception and rollback the changes
    // (rollbacking only works if the database back-end supports transactional
    // data definition, not a very commonly available feature).
    property OnCommandExecuteError: TInstantDBBuildCommandErrorEvent
      read FCommandExecuteError write FCommandExecuteError;
    // The TInstantMetadata objects referenced by the commands reside in
    // SourceScheme (OldMetadata) and TargetScheme (NewMetadata). See
    // TInstantDBEvolver for a usage example of these properties.
    property SourceScheme: TInstantScheme read FSourceScheme write SetSourceScheme;
    property TargetScheme: TInstantScheme read FTargetScheme write SetTargetScheme;
    // Deletes all the commands in the sequence.
    procedure Clear;
    // Adds a command to the tail of the sequence.
    procedure Append(const ACommand: TInstantDBBuildCommand);
    // Executes all the commands in the sequence.
    procedure Execute;
    // Moves a command up or down in the sequence. Pass a positive value to
    // move the chosen item Extent positions down the sequence, and a negative
    // value to move it up.
    procedure MoveItem(const AItem: TInstantDBBuildCommand; const Extent: Integer);
    // Used for component reference notifications.
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  end;

  // Base class for all steps that work by executing one or more SQL statements
  // (that is, a script) each.
  TInstantDBBuildSQLCommand = class(TInstantDBBuildCommand)
  private
    function GetConnector: TInstantRelationalConnector;
    function GetBroker: TInstantSQLBroker;
  protected
    function GetDescription: string; override;
    // Returns The number of statements that compound this script. The
    // predefined implementation returns 1.
    function GetSQLStatementCount: Integer; virtual;
    // Returns the nth SQL statement that is part of this script. Valid values
    // are in the range 0 to Pred(GetSQLStatementCount). The default
    // implementation, which should always be called through inherited at the
    // beginning of the overridden version, just returns '', or raises an
    // exception if Index is not in the allowed range.
    function GetSQLStatement(const Index: Integer): string; virtual;
    // Executes the Nth statement. Handles transactions internally.
    procedure ExecuteSQLStatement(const Index: Integer);
    // Executes all statements.
    procedure InternalExecute; override;
  public
    property Connector: TInstantRelationalConnector read GetConnector;
    property Broker: TInstantSQLBroker read GetBroker;
  end;

  // Adds a table using a SQL CREATE TABLE statement.
  TInstantDBBuildAddTableSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    function GetTableMetadata: TInstantTableMetadata;
  protected
    function GetSQLStatement(const Index: Integer): string; override;
  public
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

  // Drops a table using a SQL DROP TABLE statement.
  TInstantDBBuildDropTableSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    function GetTableMetadata: TInstantTableMetadata;
  protected
    function GetSQLStatement(const Index: Integer): string; override;
  public
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

  // Adds a field using a SQL ALTER TABLE ADD statement.
  TInstantDBBuildAddFieldSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    function GetFieldMetadata: TInstantFieldMetadata;
  protected
    function GetSQLStatement(const Index: Integer): string; override;
  public
    property FieldMetadata: TInstantFieldMetadata read GetFieldMetadata;
  end;

  // Drops a field using a SQL ALTER TABLE DROP statement.
  TInstantDBBuildDropFieldSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    function GetFieldMetadata: TInstantFieldMetadata;
  protected
    function GetSQLStatement(const Index: Integer): string; override;
  public
    property FieldMetadata: TInstantFieldMetadata read GetFieldMetadata;
  end;

  // Alters a field using a SQL ALTER TABLE ALTER COLUMN statement.
  TInstantDBBuildAlterFieldSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    function GetNewFieldMetadata: TInstantFieldMetadata;
    function GetOldFieldMetadata: TInstantFieldMetadata;
  protected
    function GetSQLStatement(const Index: Integer): string; override;
  public
    property OldFieldMetadata: TInstantFieldMetadata read GetOldFieldMetadata;
    property NewFieldMetadata: TInstantFieldMetadata read GetNewFieldMetadata;
  end;

  // Adds an index using a SQL CREATE INDEX statement.
  TInstantDBBuildAddIndexSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    function GetIndexMetadata: TInstantIndexMetadata;
  protected
    function GetSQLStatement(const Index: Integer): string; override;
  public
    property IndexMetadata: TInstantIndexMetadata read GetIndexMetadata;
  end;

  // Drops an index using a SQL DROP INDEX statement.
  TInstantDBBuildDropIndexSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    function GetIndexMetadata: TInstantIndexMetadata;
  protected
    function GetSQLStatement(const Index: Integer): string; override;
  public
    property IndexMetadata: TInstantIndexMetadata read GetIndexMetadata;
  end;

  // Alters an index using a couple of SQL DROP INDEX and CREATE INDEX
  // statements.
  TInstantDBBuildAlterIndexSQLCommand = class(TInstantDBBuildSQLCommand)
  private
    function GetOldIndexMetadata: TInstantIndexMetadata;
    function GetNewIndexMetadata: TInstantIndexMetadata;
  protected
    function GetSQLStatement(const Index: Integer): string; override;
    function GetSQLStatementCount: Integer; override;
  public
    property OldIndexMetadata: TInstantIndexMetadata read GetOldIndexMetadata;
    property NewIndexMetadata: TInstantIndexMetadata read GetNewIndexMetadata;
  end;

  // Alters a field using a sequence of 6 instructions:
  // 1. adds a temporary new field of the new type.
  // 2. copies the values from the old field to the new field.
  // 3. drops the old field.
  // 4. adds a field with the old name and the new type.
  // 5. copies back the values from the new temp field to the field with
  //    the old name.
  // 6. drops the temp field.
  // This class should be used for those database that don't support the
  // SQL ALTER TABLE ALTER COLUMN statement.
  TInstantDBBuildAlterFieldGenericSQLCommand = class(
    TInstantDBBuildAlterFieldSQLCommand)
  private
    FTempFieldMetadata: TInstantFieldMetadata;
  protected
    function GetDescription: string; override;
    function GetSQLStatement(const Index: Integer): string; override;
    function GetSQLStatementCount: Integer; override;
    function InternalExecuteHandleError(const E: Exception): Boolean; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  Data.DB
  ;
  
const
  STmpFieldSuffix = '_XYZ_';

{ TInstantCustomDBBuilder }

procedure TInstantCustomDBBuilder.BuildCommandSequence;
begin
  InternalBuildCommandSequence;
end;

constructor TInstantCustomDBBuilder.Create(AOwner: TComponent);
begin
  inherited;
  FCommandSequence := TInstantDBBuildCommandSequence.Create(Self);
end;

destructor TInstantCustomDBBuilder.Destroy;
begin
  Connector := nil;
  FreeAndNil(FCommandSequence);
  inherited;
end;

function TInstantCustomDBBuilder.GetAfterCommandExecute: TInstantDBBuildCommandNotifyEvent;
begin
  Result := FCommandSequence.AfterCommandExecute;
end;

function TInstantCustomDBBuilder.GetAfterCommandSequenceExecute: TNotifyEvent;
begin
  Result := FCommandSequence.AfterExecute;
end;

function TInstantCustomDBBuilder.GetBeforeCommandExecute: TInstantDBBuildCommandNotifyEvent;
begin
  Result := FCommandSequence.BeforeCommandExecute;
end;

function TInstantCustomDBBuilder.GetBeforeCommandSequenceExecute: TNotifyEvent;
begin
  Result := FCommandSequence.BeforeExecute;
end;

function TInstantCustomDBBuilder.GetCommandExecuteError: TInstantDBBuildCommandErrorEvent;
begin
  Result := FCommandSequence.OnCommandExecuteError;
end;

function TInstantCustomDBBuilder.GetConnector: TInstantConnector;
begin
  if Assigned(FConnector) then
    Result := FConnector
  else
    {$IFNDEF MARS_FIREDAC}
    Result := InstantDefaultConnector;
    {$ELSE}
    raise Exception.Create(SDefaultConnectorNotAvailable);
    {$ENDIF}
end;

procedure TInstantCustomDBBuilder.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FConnector then
      FConnector := nil;
end;

procedure TInstantCustomDBBuilder.SetAfterCommandExecute(
  const Value: TInstantDBBuildCommandNotifyEvent);
begin
  FCommandSequence.AfterCommandExecute := Value;
end;

procedure TInstantCustomDBBuilder.SetAfterCommandSequenceExecute(
  const Value: TNotifyEvent);
begin
  FCommandSequence.AfterExecute := Value;
end;

procedure TInstantCustomDBBuilder.SetBeforeCommandExecute(
  const Value: TInstantDBBuildCommandNotifyEvent);
begin
  FCommandSequence.BeforeCommandExecute := Value;
end;

procedure TInstantCustomDBBuilder.SetBeforeCommandSequenceExecute(
  const Value: TNotifyEvent);
begin
  FCommandSequence.BeforeExecute := Value;
end;

procedure TInstantCustomDBBuilder.SetCommandExecuteError(
  const Value: TInstantDBBuildCommandErrorEvent);
begin
  FCommandSequence.OnCommandExecuteError := Value;
end;

procedure TInstantCustomDBBuilder.SetConnector(const Value: TInstantConnector);
begin
  if Value <> FConnector then
  begin
    if Assigned(FConnector) then
      FConnector.RemoveFreeNotification(Self);
    FConnector := Value;
    if Assigned(FConnector) then
      FConnector.FreeNotification(Self);
    FCommandSequence.Connector := FConnector;
  end;
end;

procedure TInstantCustomDBBuilder.SourceSchemeWarningHandler(
  const Sender: TObject; const AWarningText: string);
begin
  DoWarning(AWarningText);
end;

procedure TInstantCustomDBBuilder.DoWarning(const WarningText: string);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Self, WarningText);
end;

{ TInstantCustomDBEvolver }

function TInstantCustomDBEvolver.GetTargetModel: TInstantModel;
begin
  if Assigned(FTargetModel) then
    Result := FTargetModel
  else
    Result := InstantModel;
end;

procedure TInstantCustomDBEvolver.SetTargetModel(const Value: TInstantModel);
begin
  FTargetModel := Value;
end;

procedure TInstantCustomDBEvolver.AppendAddFieldCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const FieldMetadata: TInstantFieldMetadata);
var
  Command: TInstantDBBuildCommand;
begin
  Command := Connector.Broker.CreateDBBuildCommand(ctAddField);
  Command.NewMetadata := FieldMetadata;
  CommandSequence.Append(Command);
end;

procedure TInstantCustomDBEvolver.AppendAlterFieldCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const SourceFieldMetadata, TargetFieldMetadata: TInstantFieldMetadata);
var
  Command: TInstantDBBuildCommand;
begin
  Command := Connector.Broker.CreateDBBuildCommand(ctAlterField);
  Command.OldMetadata := SourceFieldMetadata;
  Command.NewMetadata := TargetFieldMetadata;
  CommandSequence.Append(Command);
end;

procedure TInstantCustomDBEvolver.AppendDropFieldCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const FieldMetadata: TInstantFieldMetadata);
var
  Command: TInstantDBBuildCommand;
begin
  Command := Connector.Broker.CreateDBBuildCommand(ctDropField);
  Command.OldMetadata := FieldMetadata;
  CommandSequence.Append(Command);
end;

procedure TInstantCustomDBEvolver.AppendAddIndexCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const IndexMetadata: TInstantIndexMetadata);
var
  Command: TInstantDBBuildCommand;
begin
  Command := Connector.Broker.CreateDBBuildCommand(ctAddIndex);
  Command.NewMetadata := IndexMetadata;
  CommandSequence.Append(Command);
end;

procedure TInstantCustomDBEvolver.AppendAlterIndexCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const SourceIndexMetadata, TargetIndexMetadata: TInstantIndexMetadata);
var
  Command: TInstantDBBuildCommand;
begin
  Command := Connector.Broker.CreateDBBuildCommand(ctAlterIndex);
  Command.OldMetadata := SourceIndexMetadata;
  Command.NewMetadata := TargetIndexMetadata;
  CommandSequence.Append(Command);
end;

procedure TInstantCustomDBEvolver.AppendDropIndexCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const IndexMetadata: TInstantIndexMetadata);
var
  Command: TInstantDBBuildCommand;
begin
  Command := Connector.Broker.CreateDBBuildCommand(ctDropIndex);
  Command.OldMetadata := IndexMetadata;
  CommandSequence.Append(Command);
end;

procedure TInstantCustomDBEvolver.AppendAddTableCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const TableMetadata: TInstantTableMetadata);
var
  Command: TInstantDBBuildCommand;
begin
  Command := Connector.Broker.CreateDBBuildCommand(ctAddTable);
  Command.NewMetadata := TableMetadata;
  CommandSequence.Append(Command);
end;

function TInstantCustomDBEvolver.AppendDropTableCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const TableMetadata: TInstantTableMetadata): TInstantDBBuildCommand;
begin
  Result := Connector.Broker.CreateDBBuildCommand(ctDropTable);
  Result.OldMetadata := TableMetadata;
  CommandSequence.Append(Result);
end;

{ TInstantDBBuilder }

function TInstantDBBuilder.AppendDropTableCommand(
  const CommandSequence: TInstantDBBuildCommandSequence;
  const TableMetadata: TInstantTableMetadata): TInstantDBBuildCommand;
begin
  Result := inherited AppendDropTableCommand(CommandSequence, TableMetadata);
  // This DB builder always drops tables by design, so we should better override
  // the default setting of False for Enabled.
  Result.Enabled := True;
end;

procedure TInstantDBBuilder.GenerateCommandSequence(
  const CommandSequence: TInstantDBBuildCommandSequence);
var
  iTable, iIndex: Integer;
  SourceTableMetadata, TargetTableMetadata: TInstantTableMetadata;
  TargetIndexMetadata: TInstantIndexMetadata;
begin
  // Recreate tables.
  if (cfReadTableInfo in CommandSequence.SourceScheme.Catalog.Features) and
    (cfReadTableInfo in CommandSequence.TargetScheme.Catalog.Features) then
  begin
    for iTable := 0 to CommandSequence.TargetScheme.TableMetadataCount - 1 do
    begin
      TargetTableMetadata := CommandSequence.TargetScheme.TableMetadatas[iTable];
      { TODO : This only works for case-insensitive object names! }
      SourceTableMetadata :=
        CommandSequence.SourceScheme.FindTableMetadata(AnsiUpperCase(TargetTableMetadata.Name));
      if Assigned(SourceTableMetadata) then
        AppendDropTableCommand(CommandSequence, TargetTableMetadata);
      AppendAddTableCommand(CommandSequence, TargetTableMetadata);
    // Recreate indexes
    if (cfReadIndexInfo in CommandSequence.SourceScheme.Catalog.Features) and
      (cfReadIndexInfo in CommandSequence.TargetScheme.Catalog.Features) then
    begin
      for iIndex := 0 to TargetTableMetadata.IndexMetadataCount - 1 do
      begin
        TargetIndexMetadata := TargetTableMetadata.IndexMetadatas[iIndex];
        if not (ixPrimary in TargetIndexMetadata.Options) then
          AppendAddIndexCommand(CommandSequence, TargetIndexMetadata);
        end;
      end;
    end;
  end;
end;

procedure TInstantDBBuilder.InternalBuildCommandSequence;
begin
  CommandSequence.Clear;
  CommandSequence.SourceScheme := Connector.Broker.ReadDatabaseScheme(SourceSchemeWarningHandler);
  CommandSequence.TargetScheme := Connector.CreateScheme(TargetModel);
  GenerateCommandSequence(CommandSequence);
end;

{ TInstantDBBuildCommandSequence }

procedure TInstantDBBuildCommandSequence.Append(
  const ACommand: TInstantDBBuildCommand);
begin
  FCommands.Add(ACommand);
end;

procedure TInstantDBBuildCommandSequence.Clear;
begin
  FCommands.Clear;
end;

constructor TInstantDBBuildCommandSequence.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommands := TObjectList.Create(True);
end;

destructor TInstantDBBuildCommandSequence.Destroy;
begin
  FreeAndNil(FCommands);
  FreeAndNil(FSourceScheme);
  FreeAndNil(FTargetScheme);
  inherited;
end;

procedure TInstantDBBuildCommandSequence.DoAfterCommandExecute(
  const ACommand: TInstantDBBuildCommand);
begin
  if Assigned(FAfterCommandExecute) then
    FAfterCommandExecute(Self, ACommand);
end;

procedure TInstantDBBuildCommandSequence.DoAfterExecute;
begin
  if Assigned(FAfterExecute) then
    FAfterExecute(Self);
end;

procedure TInstantDBBuildCommandSequence.DoBeforeCommandExecute(
  const ACommand: TInstantDBBuildCommand);
begin
  if Assigned(FBeforeCommandExecute) then
    FBeforeCommandExecute(Self, ACommand);
end;

procedure TInstantDBBuildCommandSequence.DoBeforeExecute;
begin
  if Assigned(FBeforeExecute) then
    FBeforeExecute(Self);
end;

procedure TInstantDBBuildCommandSequence.DoCommandExecuteError(
  const ACommand: TInstantDBBuildCommand; const Error: Exception;
  var RaiseError: Boolean);
begin

end;

procedure TInstantDBBuildCommandSequence.DoExecute;
var
  I: Integer;
  CurrentCommand: TInstantDBBuildCommand;
  RaiseError: Boolean;
begin
  for I := 0 to FCommands.Count - 1 do
  begin
    CurrentCommand := FCommands[I] as TInstantDBBuildCommand;
    DoBeforeCommandExecute(CurrentCommand);
    try
      if CurrentCommand.Enabled then
        CurrentCommand.Execute;
    except
      on E: Exception do
      begin
        RaiseError := True;
        DoCommandExecuteError(CurrentCommand, E, RaiseError);
        if RaiseError then
          raise;
      end;
    end;
    DoAfterCommandExecute(CurrentCommand);
  end;
end;

procedure TInstantDBBuildCommandSequence.Execute;
var
  LWasConnected: Boolean;
begin
  LWasConnected := Connector.Connected;
  if not LWasConnected then
    Connector.Connect;
  DoBeforeExecute;
  try
    DoExecute;
    DoAfterExecute;
  finally
    if not LWasConnected then
      Connector.Disconnect;
  end;
end;

function TInstantDBBuildCommandSequence.GetConnector: TInstantConnector;
begin
  if Assigned(FConnector) then
    Result := FConnector
  else
    {$IFNDEF MARS_FIREDAC}
    Result := InstantDefaultConnector;
    {$ELSE}
    raise Exception.Create(SDefaultConnectorNotAvailable);
    {$ENDIF}
end;

function TInstantDBBuildCommandSequence.GetCount: Integer;
begin
  Result := FCommands.Count;
end;

function TInstantDBBuildCommandSequence.GetItem(
  const Index: Integer): TInstantDBBuildCommand;
begin
  Result := FCommands[Index] as TInstantDBBuildCommand;
end;

procedure TInstantDBBuildCommandSequence.MoveItem(
  const AItem: TInstantDBBuildCommand; const Extent: Integer);
var
  i: Integer;
begin
  for i := 0 to FCommands.Count - 1 do
  begin
    if FCommands[i] = AItem then
    begin
      FCommands.Move(i, i + Extent);
      Break;
    end;
  end;
end;

procedure TInstantDBBuildCommandSequence.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FConnector then
      FConnector := nil;
end;

procedure TInstantDBBuildCommandSequence.SetConnector(
  const Value: TInstantConnector);
begin
  if Value <> FConnector then
  begin
    if Assigned(FConnector) then
      FConnector.RemoveFreeNotification(Self);
    FConnector := Value;
    if Assigned(FConnector) then
      FConnector.FreeNotification(Self);
  end;
end;

procedure TInstantDBBuildCommandSequence.SetSourceScheme(
  const Value: TInstantScheme);
begin
  if FSourceScheme <> Value then
  begin
    FreeAndNil(FSourceScheme);
    FSourceScheme := Value;
  end;
end;

procedure TInstantDBBuildCommandSequence.SetTargetScheme(
  const Value: TInstantScheme);
begin
  if FTargetScheme <> Value then
  begin
    FreeAndNil(FTargetScheme);
    FTargetScheme := Value;
  end;
end;

{ TInstantDBBuildSQLCommand }

procedure TInstantDBBuildSQLCommand.ExecuteSQLStatement(const Index: Integer);
begin
  if Connector.DDLTransactionSupported then
    Connector.StartTransaction;
  try
    Broker.Execute(GetSQLStatement(Index));
    if Connector.DDLTransactionSupported then
      Connector.CommitTransaction;
  except
    if Connector.DDLTransactionSupported then
      Connector.RollbackTransaction;
    raise;
  end;
end;

function TInstantDBBuildSQLCommand.GetBroker: TInstantSQLBroker;
begin
  Result := Connector.Broker as TInstantSQLBroker;
end;

function TInstantDBBuildSQLCommand.GetConnector: TInstantRelationalConnector;
begin
  Result := inherited Connector as TInstantRelationalConnector;
end;

function TInstantDBBuildSQLCommand.GetDescription: string;
var
  iStatement: Integer;
begin
  Result := '';
  for iStatement := 0 to Pred(GetSQLStatementCount) do
  begin
    if Result <> '' then
      Result := Result + '; ';
    Result := Result + GetSQLStatement(iStatement);
  end;
end;

function TInstantDBBuildSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  if (Index < 0) or (Index >= GetSQLStatementCount) then
    raise EInstantDBBuildError.CreateFmt(SSQLStatementIndexOutOfBounds,
      [Index]);
  Result := '';
end;

function TInstantDBBuildSQLCommand.GetSQLStatementCount: Integer;
begin
  Result := 1;
end;

procedure TInstantDBBuildSQLCommand.InternalExecute;
var
  IStatement: Integer;
begin
  for IStatement := 0 to Pred(GetSQLStatementCount) do
    ExecuteSQLStatement(IStatement);
end;

{ TInstantDBBuildAddTableSQLCommand }

function TInstantDBBuildAddTableSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  Result := inherited GetSQLStatement(Index);
  Result := Broker.Generator.GenerateCreateTableSQL(TableMetadata);
end;

function TInstantDBBuildAddTableSQLCommand.GetTableMetadata: TInstantTableMetadata;
begin
  Result := NewMetadata as TInstantTableMetadata;
end;

{ TInstantDBBuildAddFieldSQLCommand }

function TInstantDBBuildAddFieldSQLCommand.GetFieldMetadata: TInstantFieldMetadata;
begin
  Result := NewMetadata as TInstantFieldMetadata;
end;

function TInstantDBBuildAddFieldSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  Result := inherited GetSQLStatement(Index);
  Result := Broker.Generator.GenerateAddFieldSQL(FieldMetadata);
end;

{ TInstantDBBuildDropFieldSQLCommand }

function TInstantDBBuildDropFieldSQLCommand.GetFieldMetadata: TInstantFieldMetadata;
begin
  Result := OldMetadata as TInstantFieldMetadata;
end;

function TInstantDBBuildDropFieldSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  Result := inherited GetSQLStatement(Index);
  Result := Broker.Generator.GenerateDropFieldSQL(FieldMetadata);
end;

{ TInstantDBBuildDropTableSQLCommand }

function TInstantDBBuildDropTableSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  Result := inherited GetSQLStatement(Index);
  Result := Broker.Generator.GenerateDropTableSQL(TableMetadata);
end;

function TInstantDBBuildDropTableSQLCommand.GetTableMetadata: TInstantTableMetadata;
begin
  Result := OldMetadata as TInstantTableMetadata;
end;

{ TInstantDBBuildAlterFieldSQLCommand }

function TInstantDBBuildAlterFieldSQLCommand.GetNewFieldMetadata: TInstantFieldMetadata;
begin
  Result := NewMetadata as TInstantFieldMetadata;
end;

function TInstantDBBuildAlterFieldSQLCommand.GetOldFieldMetadata: TInstantFieldMetadata;
begin
  Result := OldMetadata as TInstantFieldMetadata;
end;

function TInstantDBBuildAlterFieldSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  Result := inherited GetSQLStatement(Index);
  Result := Broker.Generator.GenerateAlterFieldSQL(OldFieldMetadata, NewFieldMetadata);
end;

{ TInstantDBBuildAddIndexSQLCommand }

function TInstantDBBuildAddIndexSQLCommand.GetIndexMetadata: TInstantIndexMetadata;
begin
  Result := NewMetadata as TInstantIndexMetadata;
end;

function TInstantDBBuildAddIndexSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  Result := inherited GetSQLStatement(Index);
  Result := Broker.Generator.GenerateCreateIndexSQL(IndexMetadata);
end;

{ TInstantDBBuildDropIndexSQLCommand }

function TInstantDBBuildDropIndexSQLCommand.GetIndexMetadata: TInstantIndexMetadata;
begin
  Result := OldMetadata as TInstantIndexMetadata;
end;

function TInstantDBBuildDropIndexSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  Result := inherited GetSQLStatement(Index);
  Result := Broker.Generator.GenerateDropIndexSQL(IndexMetadata);
end;

{ TInstantDBBuildAlterIndexSQLCommand }

function TInstantDBBuildAlterIndexSQLCommand.GetOldIndexMetadata: TInstantIndexMetadata;
begin
  Result := OldMetadata as TInstantIndexMetadata;
end;

function TInstantDBBuildAlterIndexSQLCommand.GetNewIndexMetadata: TInstantIndexMetadata;
begin
  Result := NewMetadata as TInstantIndexMetadata;
end;

function TInstantDBBuildAlterIndexSQLCommand.GetSQLStatement(
  const Index: Integer): string;
begin
  Result := inherited GetSQLStatement(Index);
  if Index = 0 then
    Result := Broker.Generator.GenerateDropIndexSQL(OldIndexMetadata)
  else
    Result := Broker.Generator.GenerateCreateIndexSQL(NewIndexMetadata);
end;

function TInstantDBBuildAlterIndexSQLCommand.GetSQLStatementCount: Integer;
begin
  Result := 2;
end;

{ TInstantDBBuildAlterFieldSQLCommand }

destructor TInstantDBBuildAlterFieldGenericSQLCommand.Destroy;
begin
  FTempFieldMetadata.Free;
  inherited;
end;

function TInstantDBBuildAlterFieldGenericSQLCommand.GetDescription: string;
begin
  Result := Format('ALTER TABLE %s evolve column %s - multi-statement SQL.',
    [NewFieldMetadata.TableMetadata.Name, NewFieldMetadata.Name]);
end;

function TInstantDBBuildAlterFieldGenericSQLCommand.GetSQLStatement(
  const Index: Integer): string;

  function CreateTempFieldMetadata(FieldMetadata: TInstantFieldMetadata):
    TInstantFieldMetadata;
  begin
    Result := TInstantFieldMetadata.Create(FieldMetadata.Collection);
    Result.Assign(FieldMetadata);
    Result.Name := FieldMetadata.Name + STmpFieldSuffix;
  end;

begin
  Result := inherited GetSQLStatement(Index);
  FTempFieldMetadata := CreateTempFieldMetadata(NewFieldMetadata);
  with Broker.Generator do
    case Index of
      0: Result := GenerateAddFieldSQL(FTempFieldMetadata);
      1: Result := GenerateUpdateFieldCopySQL(OldFieldMetadata, FTempFieldMetadata);
      2: Result := GenerateDropFieldSQL(OldFieldMetadata);
      3: Result := GenerateAddFieldSQL(NewFieldMetadata);
      4: Result := GenerateUpdateFieldCopySQL(FTempFieldMetadata, NewFieldMetadata);
      5: Result := GenerateDropFieldSQL(FTempFieldMetadata);
    end;
end;

function TInstantDBBuildAlterFieldGenericSQLCommand.GetSQLStatementCount: Integer;
begin
  Result := 6;
end;

function TInstantDBBuildAlterFieldGenericSQLCommand.InternalExecuteHandleError(
  const E: Exception): Boolean;
begin
  // Try not to leave the temp field around.
  if Assigned(FTempFieldMetadata) then
    Broker.Execute(Broker.Generator.GenerateDropFieldSQL(FTempFieldMetadata));
  Result := False;
end;

end.
