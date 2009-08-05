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

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QExtCtrls, QControls, QStdCtrls, QComCtrls, QForms, QGraphics,
{$ENDIF}
  InstantDialog;

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
  ShellAPI, InstantUtils;

{$R *.dfm}

procedure TInstantAboutForm.FormCreate(Sender: TObject);
var
  S, Suffix: string;
begin
  inherited;
{$IFDEF MSWINDOWS}
  with InstantFileVersion(InstantModuleFileName) do
  begin
    S := Format('Version %d.%d.%d', [Major, Minor, Release]);
    if Release > 0 then
      S := S +'.'+ IntToStr(Release);
    Suffix := InstantFileVersionValue(InstantModuleFileName, 'VersionSuffix');
    if Suffix <> '' then
      S := S + ' ' + Suffix;
  end;
  BorderStyle := bsDialog;
  //Fonts and sizes
  TitleLabel.Font.Size := 16;
  TitleLabel.Font.Style := [fsBold];
{$ENDIF}
{$IFDEF LINUX}
  BorderStyle := fbsDialog;
  //Fonts and sizes
  TitleLabel.Font.Size := 18;
  TitleLabel.Font.Style := [fsBold];
  //Package version for Kylix
  S := Format('Version %d.%d.%d %s', [2,1,1,'MPL']);
{$ENDIF}
  VersionLabel.Caption := S;
end;

end.
