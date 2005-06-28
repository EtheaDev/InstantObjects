(*
 *   InstantObjects
 *   Database evolution classes
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
 * The Original Code is: InstantObjects Database evolver
 *
 * The Initial Developer of the Original Code is: Nando Dessena
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBEvolution;

interface

uses
  Classes, InstantPersistence, InstantDBBuild;

type
  // Builds a TInstantDBBuildCommandSequence that upgrades a database schema to
  // the current (or a given) model.
  TInstantDBEvolver = class(TInstantCustomDBEvolver)
  private
    // Adds to CommandSequence the steps needed to evolve the database.
    procedure GenerateSchemeDiff(const CommandSequence: TInstantDBBuildCommandSequence);
  protected
    // (Re)builds the database evolution sequence. After calling this method,
    // the EvolutionSequence property contains the sequence of steps needed to
    // evolve the database.
    procedure InternalBuildCommandSequence; override;
  published
    property AfterCommandSequenceExecute;
    property AfterCommandExecute;
    property BeforeCommandSequenceExecute;
    property BeforeCommandExecute;
    property OnCommandExecuteError;
  end;

implementation

uses
  SysUtils, DB;
  
{ TInstantDBEvolver }

procedure TInstantDBEvolver.InternalBuildCommandSequence;
begin
  CommandSequence.Clear;
  CommandSequence.SourceScheme := Connector.Broker.ReadDatabaseScheme;
  CommandSequence.TargetScheme := Connector.CreateScheme(TargetModel);
  GenerateSchemeDiff(CommandSequence);
end;

{for each table in the schema
  if the table exists in the db
    for each field in the schema.table
      if the field exists in the db
        if the datatype is different
          generate an AlterColumn command
      else
        generate an AddColumn command
    for each field in the db.table
      if the field does not exist in the schema
        generate a DropColumn command
  else
    generate an AddTable command
for each table in the db
  if the table does not exist in the schema
    generate a DropTable command

And a similar algo for the indices. When we have foreign keys, we'll have
to sort the generated list according to references, or we could use a
strategy of disabling/dropping all the constraints at the beginning and
recreate them later.}
procedure TInstantDBEvolver.GenerateSchemeDiff(const CommandSequence: TInstantDBBuildCommandSequence);
var
  iTable, iField, iIndex: Integer;
  SourceTableMetadata, TargetTableMetadata: TInstantTableMetadata;
  SourceFieldMetadata, TargetFieldMetadata: TInstantFieldMetadata;
  SourceIndexMetadata, TargetIndexMetadata: TInstantIndexMetadata;

  procedure AppendAddFieldCommand(const FieldMetadata: TInstantFieldMetadata);
  var
    Command: TInstantDBBuildCommand;
  begin
    Command := Connector.Broker.CreateDBBuildCommand(ctAddField);
    Command.NewMetadata := FieldMetadata;
    CommandSequence.Append(Command);
  end;

  procedure AppendAlterFieldCommand(const SourceFieldMetadata,
    TargetFieldMetadata: TInstantFieldMetadata);
  var
    Command: TInstantDBBuildCommand;
  begin
    Command := Connector.Broker.CreateDBBuildCommand(ctAlterField);
    Command.OldMetadata := SourceFieldMetadata;
    Command.NewMetadata := TargetFieldMetadata;
    CommandSequence.Append(Command);
  end;

  procedure AppendDropFieldCommand(const FieldMetadata: TInstantFieldMetadata);
  var
    Command: TInstantDBBuildCommand;
  begin
    Command := Connector.Broker.CreateDBBuildCommand(ctDropField);
    Command.OldMetadata := FieldMetadata;
    CommandSequence.Append(Command);
  end;

  procedure AppendAddIndexCommand(const IndexMetadata: TInstantIndexMetadata);
  var
    Command: TInstantDBBuildCommand;
  begin
    Command := Connector.Broker.CreateDBBuildCommand(ctAddIndex);
    Command.NewMetadata := IndexMetadata;
    CommandSequence.Append(Command);
  end;

  procedure AppendAlterIndexCommand(const SourceIndexMetadata,
    TargetIndexMetadata: TInstantIndexMetadata);
  var
    Command: TInstantDBBuildCommand;
  begin
    Command := Connector.Broker.CreateDBBuildCommand(ctDropIndex);
    Command.OldMetadata := SourceIndexMetadata;
    // Enabled normally defaults to False for drop operations, but in this
    // particular case it is more convenient to have it set to True, since the
    // index is not really being dropped - it is being altered by recreating it.
    Command.Enabled := True;
    CommandSequence.Append(Command);
    Command := Connector.Broker.CreateDBBuildCommand(ctAddIndex);
    Command.NewMetadata := TargetIndexMetadata;
    CommandSequence.Append(Command);
  end;

  procedure AppendDropIndexCommand(const IndexMetadata: TInstantIndexMetadata);
  var
    Command: TInstantDBBuildCommand;
  begin
    Command := Connector.Broker.CreateDBBuildCommand(ctDropIndex);
    Command.OldMetadata := IndexMetadata;
    CommandSequence.Append(Command);
  end;

  procedure AppendAddTableCommand(const TableMetadata: TInstantTableMetadata);
  var
    Command: TInstantDBBuildCommand;
  begin
    Command := Connector.Broker.CreateDBBuildCommand(ctAddTable);
    Command.NewMetadata := TableMetadata;
    CommandSequence.Append(Command);
  end;

  procedure AppendDropTableCommand(const TableMetadata: TInstantTableMetadata);
  var
    Command: TInstantDBBuildCommand;
  begin
    Command := Connector.Broker.CreateDBBuildCommand(ctDropTable);
    Command.OldMetadata := TableMetadata;
    CommandSequence.Append(Command);
  end;

begin
  // Upgrade tables.
  for iTable := 0 to CommandSequence.TargetScheme.TableMetadataCount - 1 do
  begin
    TargetTableMetadata := CommandSequence.TargetScheme.TableMetadatas[iTable];
    { TODO : This only works for case-insensitive object names! }
    SourceTableMetadata :=
      CommandSequence.SourceScheme.FindTableMetadata(AnsiUpperCase(TargetTableMetadata.Name));
    if Assigned(SourceTableMetadata) then
    begin
      // Add missing fields and alter modified fields
      for iField := 0 to TargetTableMetadata.FieldMetadataCount - 1 do
      begin
        TargetFieldMetadata := TargetTableMetadata.FieldMetadatas[iField];
        { TODO : This only works for case-insensitive object names! }
        SourceFieldMetadata := SourceTableMetadata.FindFieldMetadata(AnsiUpperCase(TargetFieldMetadata.Name));
        if Assigned(SourceFieldMetadata) then
        begin
          if (TargetFieldMetadata.DataType <> SourceFieldMetadata.DataType) or
             (TargetFieldMetadata.Size > SourceFieldMetadata.Size) then
            AppendAlterFieldCommand(SourceFieldMetadata, TargetFieldMetadata);
        end
        else
          AppendAddFieldCommand(TargetFieldMetadata);
      end;
      // Add missing indexes and recreate modified indexes
      for iIndex := 0 to TargetTableMetadata.IndexMetadataCount - 1 do
      begin
        TargetIndexMetadata := TargetTableMetadata.IndexMetadatas[iIndex];
        if not (ixPrimary in TargetIndexMetadata.Options) then
        begin
          { TODO : This only works for case-insensitive object names! }
          SourceIndexMetadata := SourceTableMetadata.FindIndexMetadata(AnsiUpperCase(TargetIndexMetadata.Name));
          if Assigned(SourceIndexMetadata) then
          begin
            if not SourceIndexMetadata.Equals(TargetIndexMetadata) then
              AppendAlterIndexCommand(SourceIndexMetadata, TargetIndexMetadata);
          end
          else
            AppendAddIndexCommand(TargetIndexMetadata);
        end;
      end;
      // Drop deleted indexes
      for iIndex := 0 to SourceTableMetadata.IndexMetadataCount - 1 do
      begin
        SourceIndexMetadata := SourceTableMetadata.IndexMetadatas[iIndex];
        if not (ixPrimary in SourceIndexMetadata.Options) then
        begin
          { TODO : This only works for case-insensitive object names! }
          TargetIndexMetadata := TargetTableMetadata.FindIndexMetadata(AnsiUpperCase(SourceIndexMetadata.Name));
          if not Assigned(TargetIndexMetadata) then
            AppendDropIndexCommand(SourceIndexMetadata);
        end;
      end;
      // Drop deleted fields
      for iField := 0 to SourceTableMetadata.FieldMetadataCount - 1 do
      begin
        SourceFieldMetadata := SourceTableMetadata.FieldMetadatas[iField];
        { TODO : This only works for case-insensitive object names! }
        TargetFieldMetadata := TargetTableMetadata.FindFieldMetadata(AnsiUpperCase(SourceFieldMetadata.Name));
        if not Assigned(TargetFieldMetadata) then
          AppendDropFieldCommand(SourceFieldMetadata);
      end;
    end
    else
      AppendAddTableCommand(TargetTableMetadata);
  end;
  // Drop deleted tables.
  for iTable := 0 to CommandSequence.SourceScheme.TableMetadataCount - 1 do
  begin
    SourceTableMetadata := CommandSequence.SourceScheme.TableMetadatas[iTable];
    TargetTableMetadata :=
      CommandSequence.TargetScheme.FindTableMetadata(SourceTableMetadata.Name);
    if not Assigned(TargetTableMetadata) then
      AppendDropTableCommand(SourceTableMetadata);
  end;
end;

end.
