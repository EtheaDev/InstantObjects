(*
 *   InstantObjects
 *   Basic Edit Form
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

unit InstantEdit;

{$I '..\InstantDefines.inc'}

interface

uses
  SysUtils, Classes, DB,
  Windows, Messages, Graphics, StdCtrls, ExtCtrls, Controls, Forms, Dialogs,
  InstantPresentation;
type
  TInstantEditForm = class(TForm)
    SubjectExposer: TInstantExposer;
    SubjectSource: TDataSource;
    EditPanel: TPanel;
    BottomPanel: TPanel;
    ButtonPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetSubject: TObject;
    procedure SetSubject(const Value: TObject);
  protected
    procedure LoadData; virtual;
    procedure SaveData; virtual;
    procedure SubjectChanged; virtual;
  public
    property Subject: TObject read GetSubject write SetSubject;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Themes, ToolsAPI, BrandingAPI
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND};

{ TInstantEditForm }

procedure TInstantEditForm.CancelButtonClick(Sender: TObject);
begin
  try
    SubjectExposer.Revert;
  except
    ModalResult := mrNone;
    raise;
  end;
end;

function TInstantEditForm.GetSubject: TObject;
begin
  Result := SubjectExposer.Subject;
end;

procedure TInstantEditForm.LoadData;
begin
end;

procedure TInstantEditForm.OkButtonClick(Sender: TObject);
begin
  try
    SaveData;
    SubjectExposer.PostChanges;
  except
    ModalResult := mrNone;
    raise;
  end;
end;

procedure TInstantEditForm.SaveData;
begin
end;

procedure TInstantEditForm.SetSubject(const Value: TObject);
begin
  if Value <> Subject then
  begin
    SubjectExposer.Subject := Value;
    SubjectExposer.Remember;
    SubjectChanged;
  end;
end;

procedure TInstantEditForm.SubjectChanged;
begin
  LoadData;
end;

procedure TInstantEditForm.FormCreate(Sender: TObject);
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
    EditPanel.StyleElements := BottomPanel.StyleElements - [seClient];
    EditPanel.ParentBackground := False;
    EditPanel.Color := LStyle.GetSystemColor(clBtnFace);
    BottomPanel.StyleElements := BottomPanel.StyleElements - [seClient];
    BottomPanel.ParentBackground := False;
    BottomPanel.Color := LStyle.GetSystemColor(clBtnFace);
    IDEThemeManager.RegisterFormClass(TCustomFormClass(Self.ClassType));
    ThemeProperties.ApplyTheme(Self);
  end;
{$IFEND}
end;

end.
