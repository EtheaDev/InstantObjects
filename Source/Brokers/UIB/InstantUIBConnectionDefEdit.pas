(*
 *   InstantObjects
 *   UIB Support
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
 * The Original Code is: InstantObjects UIB Support
 *
 * The Initial Developer of the Original Code is: Andrea Petrelli
 *
 * Contributor(s):
 * Nando Dessena, Joao Morais, Marcos E. Yanes
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantUIBConnectionDefEdit;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, InstantUIB, InstantTypes;

type
  TInstantUIBConnectionDefEditForm = class(TForm)
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
    ParamsLabel: TLabel;
    ParamsEditor: TMemo;
    IdDataTypeComboBox: TComboBox;
    Label1: TLabel;
    IdSizeEdit: TEdit;
    Label2: TLabel;
    LoginPromptCheckBox: TCheckBox;
    UserNameLabel: TLabel;
    PasswordLabel: TLabel;
    SQLRoleLabel: TLabel;
    CharacterSetLabel: TLabel;
    UserNameEdit: TEdit;
    PasswordEdit: TEdit;
    SQLRoleEdit: TEdit;
    CharacterSetComboBox: TComboBox;
    LibraryNameLabel: TLabel;
    LibraryNameComboBox: TComboBox;
    procedure ConnectionStringButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UserNameEditChange(Sender: TObject);
    procedure PasswordEditChange(Sender: TObject);
    procedure SQLRoleEditChange(Sender: TObject);
    procedure CharacterSetComboBoxChange(Sender: TObject);
  private
    procedure AssignCharacterSetItems(AItems: TStrings);
    function GetIsValid: Boolean;
    procedure UpdateControls;
  public
    procedure LoadData(ConnectionDef: TInstantUIBConnectionDef);
    procedure SaveData(ConnectionDef: TInstantUIBConnectionDef);
    property IsValid: Boolean read GetIsValid;
  end;

implementation

{$R *.DFM}

uses
  InstantPersistence, InstantClasses, InstantConsts, uiblib;

{ TInstantUIBConnectionDefEditForm }

procedure TInstantUIBConnectionDefEditForm.AssignCharacterSetItems(
  AItems: TStrings);
var
  I: TCharacterSet;
begin
  for I := low(TCharacterSet) to high(TCharacterSet) do
    AItems.Add(string(CharacterSetStr[I]));
end;

procedure TInstantUIBConnectionDefEditForm.ConnectionStringButtonClick(
  Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter :=
      'Firebird Database (*.fdb)|*.fdb|' +
      'InterBase Database (*.gdb)|*.gdb|' +
      'All Files (*.*)|*.*';
    if Execute then
      ConnectionStringEdit.Text := FileName;
  finally
    Free;
  end;
end;

procedure TInstantUIBConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  AssignInstantStreamFormat(StreamFormatComboBox.Items);
  AssignInstantDataTypeStrings(IdDataTypeComboBox.Items);
  AssignCharacterSetItems(CharacterSetComboBox.Items);
  IdDataTypeComboBox.ItemIndex := Ord(dtString);
  IdSizeEdit.Text := IntToStr(InstantDefaultFieldSize);
  UpdateControls;
end;

function TInstantUIBConnectionDefEditForm.GetIsValid: Boolean;
begin
  Result := ConnectionStringEdit.Text <> '';
end;

procedure TInstantUIBConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantUIBConnectionDef);
begin
  with ConnectionDef do
  begin
    ConnectionStringEdit.Text := ConnectionString;
    StreamFormatComboBox.ItemIndex := Ord(BlobStreamFormat);
    UseDelimitedIdentsCheckBox.Checked := UIBUseDelimitedIdents in Options;
    LoginPromptCheckBox.Checked := LoginPrompt;
    ParamsEditor.Lines.Text := ConnectionDef.Params;
    UserNameEdit.Text := ParamsEditor.Lines.Values['user_name'];
    PasswordEdit.Text := ParamsEditor.Lines.Values['password'];
    SQLRoleEdit.Text := ParamsEditor.Lines.Values['sql_role_name'];
    CharacterSetComboBox.ItemIndex := Ord(CharacterSet);
    LibraryNameComboBox.Text := LibraryName;
    IdDataTypeComboBox.ItemIndex := Ord(IdDataType);
    IdSizeEdit.Text := IntToStr(IdSize);
  end;
  UpdateControls;
end;

procedure TInstantUIBConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantUIBConnectionDef);
begin
  with ConnectionDef do
  begin
    ConnectionString := ConnectionStringEdit.Text;
    BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex);
    Options := [];
    if UseDelimitedIdentsCheckBox.Checked then
      Options := Options + [UIBUseDelimitedIdents];
    LoginPrompt := LoginPromptCheckBox.Checked;
    ConnectionDef.Params := ParamsEditor.Lines.Text;
    CharacterSet := TCharacterSet(CharacterSetComboBox.ItemIndex);
    LibraryName := LibraryNameComboBox.Text;
    IdDataType := TInstantDataType(IdDataTypeComboBox.ItemIndex);
    IdSize := StrToInt(IdSizeEdit.Text);
  end;
end;

procedure TInstantUIBConnectionDefEditForm.UpdateControls;
begin
end;

procedure TInstantUIBConnectionDefEditForm.UserNameEditChange(
  Sender: TObject);
begin
  ParamsEditor.Lines.Values['user_name'] := UserNameEdit.Text;
end;

procedure TInstantUIBConnectionDefEditForm.PasswordEditChange(
  Sender: TObject);
begin
  ParamsEditor.Lines.Values['password'] := PasswordEdit.Text;
end;

procedure TInstantUIBConnectionDefEditForm.SQLRoleEditChange(
  Sender: TObject);
begin
  ParamsEditor.Lines.Values['sql_role_name'] := SQLRoleEdit.Text;
end;

procedure TInstantUIBConnectionDefEditForm.CharacterSetComboBoxChange(
  Sender: TObject);
begin
  ParamsEditor.Lines.Values['lc_ctype'] := CharacterSetComboBox.Text;
end;

end.
