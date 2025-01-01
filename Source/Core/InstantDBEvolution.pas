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

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  System.Classes
  , InstantPersistence
  , InstantDBBuild
  , InstantMetadata
  ;

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
  end;

implementation

uses
  System.SysUtils
  , Data.DB
  , InstantTypes
  ;
  
{ TInstantDBEvolver }

procedure TInstantDBEvolver.InternalBuildCommandSequence;
begin
  CommandSequence.Clear;
  CommandSequence.SourceScheme := Connector.Broker.ReadDatabaseScheme(SourceSchemeWarningHandler);
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

  function ReadTableInfoSupported: Boolean;
  begin
    Result := (cfReadTableInfo in CommandSequence.SourceScheme.Catalog.Features) and
      (cfReadTableInfo in CommandSequence.TargetScheme.Catalog.Features);
  end;

  function ReadColumnInfoSupported: Boolean;
  begin
    Result := (cfReadColumnInfo in CommandSequence.SourceScheme.Catalog.Features) and
      (cfReadColumnInfo in CommandSequence.TargetScheme.Catalog.Features);
  end;

  function ReadIndexInfoSupported: Boolean;
  begin
    Result := (cfReadIndexInfo in CommandSequence.SourceScheme.Catalog.Features) and
      (cfReadIndexInfo in CommandSequence.TargetScheme.Catalog.Features);
  end;

begin
  if ReadTableInfoSupported then
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
        if ReadColumnInfoSupported then
        begin
          // Add missing fields and alter modified fields
          for iField := 0 to TargetTableMetadata.FieldMetadataCount - 1 do
          begin
            TargetFieldMetadata := TargetTableMetadata.FieldMetadatas[iField];
            { TODO : This only works for case-insensitive object names! }
            SourceFieldMetadata := SourceTableMetadata.FindFieldMetadata(AnsiUpperCase(TargetFieldMetadata.Name));
            if Assigned(SourceFieldMetadata) then
            begin
              if not SourceFieldMetadata.Equals(TargetFieldMetadata) then
                AppendAlterFieldCommand(CommandSequence, SourceFieldMetadata,
                  TargetFieldMetadata);
            end
            else
              AppendAddFieldCommand(CommandSequence, TargetFieldMetadata);
          end;
        end;
        if ReadIndexInfoSupported then
        begin
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
                  AppendAlterIndexCommand(CommandSequence, SourceIndexMetadata,
                    TargetIndexMetadata);
              end
              else
                AppendAddIndexCommand(CommandSequence, TargetIndexMetadata);
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
                AppendDropIndexCommand(CommandSequence, SourceIndexMetadata);
            end;
          end;
        end;
        if ReadColumnInfoSupported then
        begin
          // Drop deleted fields
          for iField := 0 to SourceTableMetadata.FieldMetadataCount - 1 do
          begin
            SourceFieldMetadata := SourceTableMetadata.FieldMetadatas[iField];
            { TODO : This only works for case-insensitive object names! }
            TargetFieldMetadata := TargetTableMetadata.FindFieldMetadata(AnsiUpperCase(SourceFieldMetadata.Name));
            if not Assigned(TargetFieldMetadata) then
              AppendDropFieldCommand(CommandSequence, SourceFieldMetadata);
          end;
        end;
      end
      else
        AppendAddTableCommand(CommandSequence, TargetTableMetadata);
    end;
    // Drop deleted tables.
    for iTable := 0 to CommandSequence.SourceScheme.TableMetadataCount - 1 do
    begin
      SourceTableMetadata := CommandSequence.SourceScheme.TableMetadatas[iTable];
      TargetTableMetadata :=
        CommandSequence.TargetScheme.FindTableMetadata(SourceTableMetadata.Name);
      if not Assigned(TargetTableMetadata) then
        AppendDropTableCommand(CommandSequence, SourceTableMetadata);
    end;
  end;
end;

end.
