(*
 *   InstantObjects
 *   Basic Wizard Dialog
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

unit InstantWizard;

{$I '..\InstantDefines.inc'}

interface

uses
  System.SysUtils
  , System.Classes
  , InstantDialog
  , WinApi.Windows
  , WinApi.Messages
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , Vcl.ComCtrls
  ;

type
  TInstantWizardForm = class(TInstantDialogForm)
    BackButton: TButton;
    CancelButton: TButton;
    ClientPanel: TPanel;
    NextButton: TButton;
    StartContinueLabel: TLabel;
    StartInfoLabel: TLabel;
    StartSheet: TTabSheet;
    WizardPageControl: TPageControl;
    HeaderPanel: TPanel;
    HeaderBevel: TBevel;
    HeaderImage: TImage;
    HeaderCaptionLabel: TLabel;
    HeaderHintLabel: TLabel;
    procedure BackButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    function AllowBack: Boolean; virtual;
    function AllowNext: Boolean; virtual;
    procedure GoBack; virtual;
    procedure GoNext; virtual;
    procedure UpdateButtons;
    procedure UpdateCaptions;
    procedure UpdateControls;
  end;

implementation

resourcestring
  SFinishButton = 'Finish';
  SNextButton = 'Next >';

{$R *.dfm}

function TInstantWizardForm.AllowBack: Boolean;
begin
  Result := True;
end;

function TInstantWizardForm.AllowNext: Boolean;
begin
  Result := True;
end;

procedure TInstantWizardForm.BackButtonClick(Sender: TObject);
begin
  GoBack;
end;

procedure TInstantWizardForm.FormCreate(Sender: TObject);
begin
  WizardPageControl.ActivePageIndex := 0;
  UpdateControls;
end;

procedure TInstantWizardForm.GoBack;
begin
  with WizardPageControl do
    if ActivePageIndex > 0 then
    begin
      ActivePageIndex := ActivePageIndex - 1;
      UpdateControls;
      SetFocus;
    end;
end;

procedure TInstantWizardForm.GoNext;
begin
  with WizardPageControl do
    if ActivePageIndex = Pred(PageCount) then
      ModalResult := mrOk
    else begin
      ActivePageIndex := ActivePageIndex + 1;
      UpdateControls;
      SetFocus;
    end;
end;

procedure TInstantWizardForm.NextButtonClick(Sender: TObject);
begin
  GoNext;
end;

procedure TInstantWizardForm.UpdateButtons;
begin
  with WizardPageControl do
  begin
    BackButton.Enabled := AllowBack and (ActivePageIndex > 0);
    NextButton.Enabled := AllowNext;
    if ActivePageIndex = Pred(PageCount) then
      NextButton.Caption := SFinishButton else
      NextButton.Caption := SNextButton;
  end;
end;

procedure TInstantWizardForm.UpdateCaptions;
begin
  with WizardPageControl do
  begin
    if not Assigned(ActivePage) then
      Exit;
    HeaderCaptionLabel.Caption := ActivePage.Caption;
    HeaderHintLabel.Caption := ActivePage.Hint;
  end;
end;

procedure TInstantWizardForm.UpdateControls;
begin
  UpdateButtons;
  UpdateCaptions;
end;

end.
