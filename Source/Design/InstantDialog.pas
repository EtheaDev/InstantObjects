(*
 *   InstantObjects
 *   Basic Dialog
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

unit InstantDialog;

{$I '..\InstantDefines.inc'}

interface

uses
  System.SysUtils
  , WinApi.Windows
  , WinApi.Messages
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , System.Classes
  ;

type
  TInstantDialogForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonBevel: TBevel;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  Vcl.Themes, ToolsAPI, BrandingAPI
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND};

procedure TInstantDialogForm.FormCreate(Sender: TObject);
{$IF (CompilerVersion >= 32.0)}
var
  LStyle: TCustomStyleServices;
{$IFEND}
begin
  BorderStyle := bsDialog;
{$IF (CompilerVersion >= 32.0)}
  {$IF (CompilerVersion <= 34.0)}
  if UseThemeFont then
    Self.Font.Assign(GetThemeFont);
  {$IFEND}
  {$IF CompilerVersion > 34.0}
  if TIDEThemeMetrics.Font.Enabled then
    Self.Font.Assign(TIDEThemeMetrics.Font.GetFont);
  {$IFEND}

  if ThemeProperties <> nil then
  begin
    LStyle := ThemeProperties.StyleServices;
    StyleElements := StyleElements - [seClient];
    Color := LStyle.GetSystemColor(clWindow);
    ButtonPanel.StyleElements := ButtonPanel.StyleElements - [seClient];
    ButtonPanel.ParentBackground := False;
    ButtonPanel.Color := LStyle.GetSystemColor(clBtnFace);
    IDEThemeManager.RegisterFormClass(TCustomFormClass(Self.ClassType));
    ThemeProperties.ApplyTheme(Self);
  end;
{$IFEND}
end;

end.

