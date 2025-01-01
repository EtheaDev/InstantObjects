(*
 *   InstantObjects
 *   Dual List Dialog
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
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDualList;

{$I '..\InstantDefines.inc'}

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
  , Vcl.ExtCtrls
  , Vcl.ComCtrls
  , Vcl.ActnList
  , InstantDialog
  , System.Actions
  ;

type
  TInstantDualListForm = class(TInstantDialogForm)
    ClientPanel: TPanel;
    LeftPanel: TPanel;
    LeftView: TListView;
    RightPanel: TPanel;
    RightView: TListView;
    CenterPanel: TPanel;
    RightButton: TButton;
    LeftButton: TButton;
    AllRightButton: TButton;
    AllLeftButton: TButton;
    OkButton: TButton;
    CancelButton: TButton;
    Actions: TActionList;
    RightAction: TAction;
    LeftAction: TAction;
    AllRightAction: TAction;
    AllLeftAction: TAction;
    procedure RightActionExecute(Sender: TObject);
    procedure LeftActionExecute(Sender: TObject);
    procedure AllRightActionExecute(Sender: TObject);
    procedure AllLeftActionExecute(Sender: TObject);
    procedure LeftViewDblClick(Sender: TObject);
    procedure RightViewDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClientPanelResize(Sender: TObject);
  private
    procedure MoveItem(Item: TListItem; View: TListView);
  protected
    procedure LoadList(List: TStrings; ListView: TListView);
    procedure SaveList(List: TStrings; ListView: TListView);
    procedure UpdateActions; override;
  public
    function Execute(Left, Right: TStrings): Boolean;
  end;

implementation

{$R *.dfm}

{ TDualListForm }

procedure TInstantDualListForm.AllLeftActionExecute(Sender: TObject);
begin
  RightView.Items.BeginUpdate;
  LeftView.Items.BeginUpdate;
  try
    with RightView do
      while Items.Count > 0 do
        MoveItem(Items[0], LeftView);
  finally
    RightView.Items.EndUpdate;
    LeftView.Items.EndUpdate;
  end;
end;

procedure TInstantDualListForm.AllRightActionExecute(Sender: TObject);
begin
  RightView.Items.BeginUpdate;
  LeftView.Items.BeginUpdate;
  try
    with LeftView do
      while Items.Count > 0 do
        MoveItem(Items[0], RightView);
  finally
    RightView.Items.EndUpdate;
    LeftView.Items.EndUpdate;
  end;
end;

procedure TInstantDualListForm.ClientPanelResize(Sender: TObject);
begin
  inherited;
  LeftPanel.Width := (ClientPanel.Width div 2) - (CenterPanel.Width div 2);
  LeftView.Column[0].Width := LeftPanel.Width - 25;
  RightView.Column[0].Width := RightPanel.Width - 25;
end;

function TInstantDualListForm.Execute(Left, Right: TStrings): Boolean;
begin
  LoadList(Left, LeftView);
  LoadList(Right, RightView);
  Result := ShowModal = mrOk;
  if Result then
  begin
    SaveList(Left, LeftView);
    SaveList(Right, RightView);
  end;
end;

procedure TInstantDualListForm.LeftActionExecute(Sender: TObject);
begin
  with RightView do
    while Assigned(Selected) do
      MoveItem(Selected, LeftView);
end;

procedure TInstantDualListForm.LeftViewDblClick(Sender: TObject);
begin
  RightAction.Execute;
end;

procedure TInstantDualListForm.LoadList(List: TStrings; ListView: TListView);
var
  I: Integer;
begin
  with ListView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to Pred(List.Count) do
        Items.Add.Caption := List[I];
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TInstantDualListForm.MoveItem(Item: TListItem; View: TListView);
begin
  if Assigned(Item) then
  begin
    with View.Items.Add do
      Caption := Item.Caption;
    Item.Free;
  end;
end;

procedure TInstantDualListForm.RightActionExecute(Sender: TObject);
begin
  with LeftView do
    while Assigned(Selected) do
      MoveItem(Selected, RightView);
end;

procedure TInstantDualListForm.RightViewDblClick(Sender: TObject);
begin
  LeftAction.Execute;
end;

procedure TInstantDualListForm.SaveList(List: TStrings; ListView: TListView);
var
  I: Integer;
begin
  with ListView do
  begin
    List.Clear;
    for I := 0 to Pred(Items.Count) do
      List.Add(Items[I].Caption);
  end;
end;

procedure TInstantDualListForm.UpdateActions;
begin
  inherited;
  RightAction.Enabled := Assigned(LeftView.Selected);
  LeftAction.Enabled := Assigned(RightView.Selected);
  AllRightAction.Enabled := LeftView.Items.Count > 0;
  AllLeftAction.Enabled := RightView.Items.Count > 0;
end;

procedure TInstantDualListForm.FormCreate(Sender: TObject);
begin
  inherited;
  BorderStyle := bsSizeable;

  LeftView.HideSelection := True;
  LeftView.ColumnClick := False;
  LeftView.SortType := stText;

  RightView.HideSelection := True;
  RightView.ColumnClick := False;
  RightView.SortType := stText;
end;

end.
