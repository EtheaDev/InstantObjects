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

unit InstantCustomDBEvolverFormUnit;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  System.SysUtils
  , System.Classes
  , WinApi.Windows
  , WinApi.Messages
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ComCtrls
  , Vcl.ImgList
  , Vcl.Menus
  , Vcl.ActnList
  , Vcl.ExtCtrls
  , Vcl.StdActns
  , System.Actions
  , InstantPersistence
  , InstantDBBuild
  , InstantDBEvolution
  , InstantPresentation
  , InstantMetadata
  ;

type
  TInstantCustomDBEvolverForm = class(TForm)
    ShowSequenceButton: TButton;
    SequenceListView: TListView;
    EvolveButton: TButton;
    MoveCommandUpButton: TButton;
    MoveCommandDownButton: TButton;
    EvolutionLogMemo: TMemo;
    EvolutionLogLabel: TLabel;
    EnableAllButton: TButton;
    DisableAllButton: TButton;
    CloseButton: TButton;
    ActionList: TActionList;
    ShowSequenceAction: TAction;
    BuildAction: TAction;
    MoveCommandUpAction: TAction;
    MoveCommandDownAction: TAction;
    EnableAllCommandsAction: TAction;
    DisableAllCommandsAction: TAction;
    CloseAction: TAction;
    procedure ShowSequenceButtonClick(Sender: TObject);
    procedure ShowSequenceActionExecute(Sender: TObject);
    procedure BuildActionExecute(Sender: TObject);
    procedure BuildActionUpdate(Sender: TObject);
    procedure MoveCommandUpActionExecute(Sender: TObject);
    procedure MoveCommandDownActionExecute(Sender: TObject);
    procedure EnableAllCommandsActionExecute(Sender: TObject);
    procedure DisableAllCommandsActionExecute(Sender: TObject);
    procedure MoveCommandUpActionUpdate(Sender: TObject);
    procedure MoveCommandDownActionUpdate(Sender: TObject);
    procedure EnableAllCommandsActionUpdate(Sender: TObject);
    procedure DisableAllCommandsActionUpdate(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAfterBuild: TInstantConnectorEvent;
    procedure SequenceToScreen;
    procedure ScreenToSequence;
    procedure Log(const ALogStr: string);
    function GetConnector: TInstantConnector;
    procedure SetConnector(const Value: TInstantConnector);
    function GetTargetModel: TInstantModel;
    procedure SetTargetModel(const Value: TInstantModel);
  protected
    function GetCustomDBEvolver: TInstantCustomDBEvolver; virtual; abstract;
    function ConfirmDlg(const Text: string): Boolean;
    procedure CustomDBEvolverBeforeCommandExecute(const Sender: TObject;
      const ACommand: TInstantDBBuildCommand); virtual;
    procedure CustomDBEvolverAfterCommandExecute(const Sender: TObject;
      const ACommand: TInstantDBBuildCommand); virtual;
    procedure CustomDBEvolverCommandExecuteError(const Sender: TObject;
      const ACommand: TInstantDBBuildCommand; const Error: Exception;
      var RaiseError: Boolean); virtual;
    procedure CustomDBEvolverBeforeCommandSequenceExecute(Sender: TObject); virtual;
    procedure CustomDBEvolverAfterCommandSequenceExecute(Sender: TObject); virtual;
    procedure CustomDBEvolverWarning(const Sender: TObject; const AWarningText: string); virtual;
    procedure BeforeBuildCommandSequence; virtual;
  public
    // Assign a connector before calling the Execute method, otherwise the
    // default connector is used.
    property Connector: TInstantConnector read GetConnector write SetConnector;
    // Assign a target model before calling the Execute method, otherwise the
    // default model is used.
    property TargetModel: TInstantModel read GetTargetModel write SetTargetModel;
    // Shows the form modally.
    procedure Execute;
    // Fired right after executing the command sequence.
    property AfterBuild: TInstantConnectorEvent read FAfterBuild write FAfterBuild;
  end;

implementation

{$R *.dfm}

uses
  System.UITypes;

procedure TInstantCustomDBEvolverForm.ShowSequenceButtonClick(Sender: TObject);
begin
  GetCustomDBEvolver.BuildCommandSequence;
  SequenceToScreen;
end;

procedure TInstantCustomDBEvolverForm.SequenceToScreen;
var
  i: Integer;
begin
  SequenceListView.Items.Clear;
  for i := 0 to GetCustomDBEvolver.CommandSequence.Count - 1 do
  begin
    with SequenceListView.Items.Add do begin
      Caption := GetCustomDBEvolver.CommandSequence[i].Description;
      Checked := GetCustomDBEvolver.CommandSequence[i].Enabled;
      Data := GetCustomDBEvolver.CommandSequence[i];
    end;
  end;
end;

procedure TInstantCustomDBEvolverForm.ScreenToSequence;
var
  i: Integer;
begin
  for i := 0 to SequenceListView.Items.Count - 1 do
    TInstantDBBuildCommand(SequenceListView.Items[i].Data).Enabled :=
      SequenceListView.Items[i].Checked;
end;

procedure TInstantCustomDBEvolverForm.Log(const ALogStr: string);
begin
  EvolutionLogMemo.Lines.Add(ALogStr);
end;

procedure TInstantCustomDBEvolverForm.CustomDBEvolverBeforeCommandExecute(
  const Sender: TObject; const ACommand: TInstantDBBuildCommand);
begin
  if ACommand.Enabled then
    Log('Executing: ' + ACommand.Description)
  else
    Log('Skipping: ' + ACommand.Description);
end;

procedure TInstantCustomDBEvolverForm.CustomDBEvolverAfterCommandExecute(
  const Sender: TObject; const ACommand: TInstantDBBuildCommand);
begin
end;

procedure TInstantCustomDBEvolverForm.CustomDBEvolverCommandExecuteError(
  const Sender: TObject; const ACommand: TInstantDBBuildCommand;
  const Error: Exception; var RaiseError: Boolean);
begin
  Log('Error: ' + Error.Message);
end;

procedure TInstantCustomDBEvolverForm.CustomDBEvolverBeforeCommandSequenceExecute(
  Sender: TObject);
begin
end;

procedure TInstantCustomDBEvolverForm.CustomDBEvolverAfterCommandSequenceExecute(
  Sender: TObject);
begin
  Connector.Connect;
  try
    if Assigned(FAfterBuild) then
      FAfterBuild(Self, Connector);
  finally
    Connector.Disconnect;
  end;
end;

procedure TInstantCustomDBEvolverForm.Execute;
begin
  ShowModal;
end;

function TInstantCustomDBEvolverForm.GetConnector: TInstantConnector;
begin
  Result := GetCustomDBEvolver.Connector;
end;

procedure TInstantCustomDBEvolverForm.SetConnector(const Value: TInstantConnector);
begin
  GetCustomDBEvolver.Connector := Value;
end;

function TInstantCustomDBEvolverForm.ConfirmDlg(const Text: string): Boolean;
begin
  Result := MessageDlg(Text, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TInstantCustomDBEvolverForm.ShowSequenceActionExecute(Sender: TObject);
var
  OldScreenCursor: TCursor;
begin
  OldScreenCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    EvolutionLogMemo.Lines.Clear;
    BeforeBuildCommandSequence;
    GetCustomDBEvolver.BuildCommandSequence;
    SequenceToScreen;
  finally
    Screen.Cursor := OldScreenCursor;
  end;
end;

procedure TInstantCustomDBEvolverForm.BuildActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GetCustomDBEvolver.CommandSequence.Count > 0;
end;

procedure TInstantCustomDBEvolverForm.BuildActionExecute(Sender: TObject);
begin
  ScreenToSequence;
  EvolutionLogMemo.Lines.Clear;
  GetCustomDBEvolver.CommandSequence.Execute;
  ShowSequenceAction.Execute;
end;

procedure TInstantCustomDBEvolverForm.MoveCommandUpActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(SequenceListView.Selected) and
    (SequenceListView.Selected.Index > 0);
end;

procedure TInstantCustomDBEvolverForm.MoveCommandUpActionExecute(Sender: TObject);
begin
  ScreenToSequence;
  GetCustomDBEvolver.CommandSequence.MoveItem(
    TInstantDBBuildCommand(SequenceListView.Selected.Data), -1);
  SequenceToScreen;
end;

procedure TInstantCustomDBEvolverForm.MoveCommandDownActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(SequenceListView.Selected) and
    (SequenceListView.Selected.Index < Pred(SequenceListView.Items.Count));
end;

procedure TInstantCustomDBEvolverForm.MoveCommandDownActionExecute(
  Sender: TObject);
begin
  ScreenToSequence;
  GetCustomDBEvolver.CommandSequence.MoveItem(
    TInstantDBBuildCommand(SequenceListView.Selected.Data), 1);
  SequenceToScreen;
end;

procedure TInstantCustomDBEvolverForm.EnableAllCommandsActionUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := SequenceListView.Items.Count > 0;
end;

procedure TInstantCustomDBEvolverForm.EnableAllCommandsActionExecute(
  Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(SequenceListView.Items.Count) do
    SequenceListView.Items[i].Checked := True;
  ScreenToSequence;
end;

procedure TInstantCustomDBEvolverForm.DisableAllCommandsActionUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := SequenceListView.Items.Count > 0;
end;

procedure TInstantCustomDBEvolverForm.DisableAllCommandsActionExecute(
  Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(SequenceListView.Items.Count) do
    SequenceListView.Items[i].Checked := False;
  ScreenToSequence;
end;

function TInstantCustomDBEvolverForm.GetTargetModel: TInstantModel;
begin
  Result := GetCustomDBEvolver.TargetModel;
end;

procedure TInstantCustomDBEvolverForm.SetTargetModel(const Value: TInstantModel);
begin
  GetCustomDBEvolver.TargetModel := Value;
end;

procedure TInstantCustomDBEvolverForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  GetCustomDBEvolver.BeforeCommandExecute := CustomDBEvolverBeforeCommandExecute;
  GetCustomDBEvolver.AfterCommandExecute := CustomDBEvolverAfterCommandExecute;
  GetCustomDBEvolver.BeforeCommandSequenceExecute := CustomDBEvolverBeforeCommandSequenceExecute;
  GetCustomDBEvolver.AfterCommandSequenceExecute := CustomDBEvolverAfterCommandSequenceExecute;
  GetCustomDBEvolver.OnCommandExecuteError := CustomDBEvolverCommandExecuteError;
  GetCustomDBEvolver.OnWarning := CustomDBEvolverWarning;
end;

procedure TInstantCustomDBEvolverForm.BeforeBuildCommandSequence;
begin
end;

procedure TInstantCustomDBEvolverForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TInstantCustomDBEvolverForm.CustomDBEvolverWarning(
  const Sender: TObject; const AWarningText: string);
begin
  Log('Warning: ' + AWarningText);
end;

end.
