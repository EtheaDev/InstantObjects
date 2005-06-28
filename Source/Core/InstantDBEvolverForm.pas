(*
 *   InstantObjects
 *   Database evolution Form
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
 * The Original Code is: InstantObjects database evolver form
 *
 * The Initial Developer of the Original Code is: Nando Dessena
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBEvolverForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, InstantPersistence, ComCtrls, InstantDBBuild,
  InstantDBEvolution, InstantPresentation, ActnList;

type
  TInstantDBEvolverForm = class(TForm)
    ShowSequenceButton: TButton;
    SequenceListView: TListView;
    EvolveButton: TButton;
    MoveCommandUpButton: TButton;
    MoveCommandDownButton: TButton;
    EvolutionLogMemo: TMemo;
    Label1: TLabel;
    DBEvolver: TInstantDBEvolver;
    EnableAllButton: TButton;
    DisableAllButton: TButton;
    ActionList: TActionList;
    ShowSequenceAction: TAction;
    EvolveAction: TAction;
    MoveCommandUpAction: TAction;
    MoveCommandDownAction: TAction;
    EnableAllCommandsAction: TAction;
    DisableAllCommandsAction: TAction;
    procedure ShowSequenceButtonClick(Sender: TObject);
    procedure DBEvolverBeforeCommandExecute(const Sender: TObject;
      const ACommand: TInstantDBBuildCommand);
    procedure DBEvolverCommandExecuteError(const Sender: TObject;
      const ACommand: TInstantDBBuildCommand; const Error: Exception;
      var RaiseError: Boolean);
    procedure ShowSequenceActionExecute(Sender: TObject);
    procedure EvolveActionExecute(Sender: TObject);
    procedure EvolveActionUpdate(Sender: TObject);
    procedure MoveCommandUpActionExecute(Sender: TObject);
    procedure MoveCommandDownActionExecute(Sender: TObject);
    procedure EnableAllCommandsActionExecute(Sender: TObject);
    procedure DisableAllCommandsActionExecute(Sender: TObject);
    procedure MoveCommandUpActionUpdate(Sender: TObject);
    procedure MoveCommandDownActionUpdate(Sender: TObject);
    procedure EnableAllCommandsActionUpdate(Sender: TObject);
    procedure DisableAllCommandsActionUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SequenceToScreen;
    procedure ScreenToSequence;
    procedure Log(const ALogStr: string);
    function GetConnector: TInstantConnector;
    procedure SetConnector(const Value: TInstantConnector);
    function ConfirmDlg(const Text: string): Boolean;
    function GetTargetModel: TInstantModel;
    procedure SetTargetModel(const Value: TInstantModel);
  public
    // Assign a connector before calling the Execute method, otherwise the
    // default connector is used.
    property Connector: TInstantConnector read GetConnector write SetConnector;
    // Assign a target model before calling the Execute method, otherwise the
    // default model is used.
    property TargetModel: TInstantModel read GetTargetModel write SetTargetModel;
    // Shows the form modally.
    procedure Execute;
  end;

implementation

{$R *.dfm}

procedure TInstantDBEvolverForm.ShowSequenceButtonClick(Sender: TObject);
begin
  DBEvolver.BuildCommandSequence;
  SequenceToScreen;
end;

procedure TInstantDBEvolverForm.SequenceToScreen;
var
  i: Integer;
begin
  SequenceListView.Clear;
  for i := 0 to DBEvolver.CommandSequence.Count - 1 do
  begin
    with SequenceListView.Items.Add do begin
      Caption := DBEvolver.CommandSequence[i].Description;
      Checked := DBEvolver.CommandSequence[i].Enabled;
      Data := DBEvolver.CommandSequence[i];
    end;
  end;
end;

procedure TInstantDBEvolverForm.ScreenToSequence;
var
  i: Integer;
begin
  for i := 0 to SequenceListView.Items.Count - 1 do
    TInstantDBBuildCommand(SequenceListView.Items[i].Data).Enabled :=
      SequenceListView.Items[i].Checked;
end;

procedure TInstantDBEvolverForm.Log(const ALogStr: string);
begin
  EvolutionLogMemo.Lines.Add(ALogStr);
end;

procedure TInstantDBEvolverForm.DBEvolverBeforeCommandExecute(
  const Sender: TObject; const ACommand: TInstantDBBuildCommand);
begin
  if ACommand.Enabled then
    Log('Executing: ' + ACommand.Description)
  else
    Log('Skipping: ' + ACommand.Description);
end;

procedure TInstantDBEvolverForm.DBEvolverCommandExecuteError(
  const Sender: TObject; const ACommand: TInstantDBBuildCommand;
  const Error: Exception; var RaiseError: Boolean);
begin
  Log('Error: ' + Error.Message);
end;

procedure TInstantDBEvolverForm.Execute;
begin
  ShowModal;
end;

function TInstantDBEvolverForm.GetConnector: TInstantConnector;
begin
  Result := DBEvolver.Connector;
end;

procedure TInstantDBEvolverForm.SetConnector(const Value: TInstantConnector);
begin
  DBEvolver.Connector := Value;
end;

function TInstantDBEvolverForm.ConfirmDlg(const Text: string): Boolean;
begin
  Result := MessageDlg(Text, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TInstantDBEvolverForm.ShowSequenceActionExecute(Sender: TObject);
var
  OldScreenCursor: TCursor;
begin
  OldScreenCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    DBEvolver.BuildCommandSequence;
    SequenceToScreen;
  finally
    Screen.Cursor := OldScreenCursor;
  end;
end;

procedure TInstantDBEvolverForm.EvolveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := DBEvolver.CommandSequence.Count > 0;
end;

procedure TInstantDBEvolverForm.EvolveActionExecute(Sender: TObject);
begin
  if ConfirmDlg('Evolve database?') then
  begin
    ScreenToSequence;
    EvolutionLogMemo.Lines.Clear;
    DBEvolver.CommandSequence.Execute;
    ShowMessage('Database evolved without errors.');
    ShowSequenceAction.Execute;
  end;
end;

procedure TInstantDBEvolverForm.MoveCommandUpActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(SequenceListView.Selected) and
    (SequenceListView.Selected.Index > 0);
end;

procedure TInstantDBEvolverForm.MoveCommandUpActionExecute(Sender: TObject);
begin
  ScreenToSequence;
  DBEvolver.CommandSequence.MoveItem(
    TInstantDBBuildCommand(SequenceListView.Selected.Data), -1);
  SequenceToScreen;
end;

procedure TInstantDBEvolverForm.MoveCommandDownActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(SequenceListView.Selected) and
    (SequenceListView.Selected.Index < Pred(SequenceListView.Items.Count));
end;

procedure TInstantDBEvolverForm.MoveCommandDownActionExecute(
  Sender: TObject);
begin
  ScreenToSequence;
  DBEvolver.CommandSequence.MoveItem(
    TInstantDBBuildCommand(SequenceListView.Selected.Data), 1);
  SequenceToScreen;
end;

procedure TInstantDBEvolverForm.EnableAllCommandsActionUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := SequenceListView.Items.Count > 0;
end;

procedure TInstantDBEvolverForm.EnableAllCommandsActionExecute(
  Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(SequenceListView.Items.Count) do
    SequenceListView.Items[i].Checked := True;
  ScreenToSequence;
end;

procedure TInstantDBEvolverForm.DisableAllCommandsActionUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := SequenceListView.Items.Count > 0;
end;

procedure TInstantDBEvolverForm.DisableAllCommandsActionExecute(
  Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(SequenceListView.Items.Count) do
    SequenceListView.Items[i].Checked := False;
  ScreenToSequence;
end;

function TInstantDBEvolverForm.GetTargetModel: TInstantModel;
begin
  Result := DBEvolver.TargetModel;
end;

procedure TInstantDBEvolverForm.SetTargetModel(const Value: TInstantModel);
begin
  DBEvolver.TargetModel := Value;
end;

procedure TInstantDBEvolverForm.FormCreate(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

end.
