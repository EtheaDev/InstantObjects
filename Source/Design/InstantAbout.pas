(*
 *   InstantObjects
 *   About Box
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

unit InstantAbout;

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
  , InstantDialog
  ;

type
  TInstantAboutForm = class(TInstantDialogForm)
    CloseButton: TButton;
    TopBevel: TBevel;
    Panel1: TPanel;
    LogoImage: TImage;
    TitleLabel: TLabel;
    VersionLabel: TLabel;
    TMLabel: TLabel;
    ClientPanel: TPanel;
    LicenseBorderPanel: TPanel;
    LicensePanel: TPanel;
    LicenseMemo: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

implementation

uses
  WinApi.ShellAPI
  , InstantUtils
  ;

{$R *.dfm}

procedure TInstantAboutForm.FormCreate(Sender: TObject);
begin
  inherited;
  LicenseMemo.Text :=
    'InstantObjects Object Persistence Framework'+sLineBreak+
    'Version: '+InstantObjectVersion+sLineBreak+
    'Mozilla Public License 2.0 Edition'+sLineBreak+
    'This product is subject to the Mozilla Public License Version 2.0 (the "License");'+sLineBreak+
    'you may not use this product except in compliance with the License.'+sLineBreak+
    'You may obtain a copy of the License at http://www.mozilla.org/MPL/'+sLineBreak+
    'Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.'+sLineBreak+
    'See the License for the specific language governing rights and limitations under the License.'+sLineBreak+
    'The Original Code is: Seleqt InstantObjects. The Initial Developer of the Original Code is: Seleqt'+sLineBreak+
    'Portions created by the Initial Developer are Copyright (C) 2001-2003 the Initial Developer.'+sLineBreak+
    'Portions created by Ethea are Copyright (C) 2006-2024 Ethea S.r.l.'+sLineBreak;
  BorderStyle := bsDialog;
  //Fonts and sizes
  TitleLabel.Font.Size := 16;
  TitleLabel.Font.Style := [fsBold];
end;

end.
