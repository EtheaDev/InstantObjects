(*
 *   InstantObjects
 *   InterBase Connection Editor 
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
 * Carlo Barazzetta, Nando Dessena, Joao Morais
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantIBXConnectionDefEdit;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, InstantIBX;

type
  TInstantIBXConnectionDefEditForm = class(TForm)
    ClientPanel: TPanel;
    ConnectionStringLabel: TLabel;
    ConnectionStringEdit: TEdit;
    ConnectionStringButton: TButton;
    BottomPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    BottomBevel: TBevel;
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    UseDelimitedIdentsCheckBox: TCheckBox;
    LoginPromptCheckBox: TCheckBox;
    ParamsLabel: TLabel;
    ParamsEditor: TMemo;
    IdDataTypeComboBox: TComboBox;
    IdDataTypeLabel: TLabel;
    IdSizeEdit: TEdit;
    IdSizeLabel: TLabel;
    UserNameLabel: TLabel;
    UserNameEdit: TEdit;
    PasswordLabel: TLabel;
    PasswordEdit: TEdit;
    SQLRoleLabel: TLabel;
    SQLRoleEdit: TEdit;
    CharacterSetLabel: TLabel;
    CharacterSetComboBox: TComboBox;
    TestButton: TButton;
    procedure ConnectionStringButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UserNameEditChange(Sender: TObject);
    procedure PasswordEditChange(Sender: TObject);
    procedure SQLRoleEditChange(Sender: TObject);
    procedure CharacterSetComboBoxChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
  private
    FOwnerConnectionDef: TInstantIBXConnectionDef;
    function GetIsValid: Boolean;
  public
    constructor CreateForConnectionDef(AOwner: TComponent;
      AConnectionDef: TInstantIBXConnectionDef);
    procedure LoadData(ConnectionDef: TInstantIBXConnectionDef);
    procedure SaveData(ConnectionDef: TInstantIBXConnectionDef);
    property IsValid: Boolean read GetIsValid;
  end;

implementation

{$R *.DFM}

uses
  IB, InstantPersistence, InstantClasses, InstantTypes, InstantConsts;

{ TInstantIBXConnectionDefEditForm }

procedure TInstantIBXConnectionDefEditForm.ConnectionStringButtonClick(
  Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter :=
      'InterBase Database (*.gdb)|*.gdb|' +
      'FireBird Database (*.fdb)|*.fdb|' +
      'All Files (*.*)|*.*';
    if Execute then
      ConnectionStringEdit.Text := FileName;
  finally
    Free;
  end;
end;

constructor TInstantIBXConnectionDefEditForm.CreateForConnectionDef(
  AOwner: TComponent; AConnectionDef: TInstantIBXConnectionDef);
begin
  inherited Create(AOwner);
  FOwnerConnectionDef := AConnectionDef;
end;

procedure TInstantIBXConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  AssignInstantStreamFormat(StreamFormatComboBox.Items);
  AssignInstantDataTypeStrings(IdDataTypeComboBox.Items);
  IdDataTypeComboBox.ItemIndex := Ord(dtString);
  IdSizeEdit.Text := IntToStr(InstantDefaultFieldSize);
end;

procedure TInstantIBXConnectionDefEditForm.FormShow(Sender: TObject);
begin
  TestButton.Visible := Assigned(FOwnerConnectionDef);
end;

function TInstantIBXConnectionDefEditForm.GetIsValid: Boolean;
begin
  Result := ConnectionStringEdit.Text <> '';
end;

procedure TInstantIBXConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantIBXConnectionDef);
begin
  with ConnectionDef do
  begin
    ConnectionStringEdit.Text := ConnectionString;
    StreamFormatComboBox.ItemIndex := Ord(BlobStreamFormat);
    UseDelimitedIdentsCheckBox.Checked := ibxUseDelimitedIdents in Options;
    LoginPromptCheckBox.Checked := LoginPrompt;
    ParamsEditor.Lines.Text := ConnectionDef.Params;
    IdDataTypeComboBox.ItemIndex := Ord(IdDataType);
    IdSizeEdit.Text := IntToStr(IdSize);
    UserNameEdit.Text := ParamsEditor.Lines.Values['user_name'];
    PasswordEdit.Text := ParamsEditor.Lines.Values['password'];
    SQLRoleEdit.Text := ParamsEditor.Lines.Values['sql_role_name'];
    CharacterSetComboBox.Text := ParamsEditor.Lines.Values['lc_ctype'];
  end;
end;

procedure TInstantIBXConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantIBXConnectionDef);
begin
  with ConnectionDef do
  begin
    ConnectionString := ConnectionStringEdit.Text;
    BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex);
    Options := [];
    if UseDelimitedIdentsCheckBox.Checked then
      Options := Options + [ibxUseDelimitedIdents];
    LoginPrompt := LoginPromptCheckBox.Checked;
    Params := ParamsEditor.Lines.Text;
    IdDataType := TInstantDataType(IdDataTypeComboBox.ItemIndex);
    IdSize := StrToInt(IdSizeEdit.Text);
  end;
end;

procedure TInstantIBXConnectionDefEditForm.UserNameEditChange(
  Sender: TObject);
begin
  ParamsEditor.Lines.Values['user_name'] := UserNameEdit.Text;
end;

procedure TInstantIBXConnectionDefEditForm.PasswordEditChange(
  Sender: TObject);
begin
  ParamsEditor.Lines.Values['password'] := PasswordEdit.Text;
end;

procedure TInstantIBXConnectionDefEditForm.SQLRoleEditChange(
  Sender: TObject);
begin
  ParamsEditor.Lines.Values['sql_role_name'] := SQLRoleEdit.Text;
end;

procedure TInstantIBXConnectionDefEditForm.TestButtonClick(Sender: TObject);
begin
  if Assigned(FOwnerConnectionDef) then
  begin
    SaveData(FOwnerConnectionDef);
    FOwnerConnectionDef.TestConnection;
    ShowMessage(SConnectionSuccess);
  end;
end;

procedure TInstantIBXConnectionDefEditForm.CharacterSetComboBoxChange(
  Sender: TObject);
begin
  ParamsEditor.Lines.Values['lc_ctype'] := CharacterSetComboBox.Text;
end;

end.
