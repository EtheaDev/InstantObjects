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

{$IFDEF LINUX}
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
    procedure ConnectionStringButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetIsValid: Boolean;
  public
    procedure LoadData(ConnectionDef: TInstantIBXConnectionDef);
    procedure SaveData(ConnectionDef: TInstantIBXConnectionDef);
    property IsValid: Boolean read GetIsValid;
  end;

implementation

{$R *.DFM}

uses
  IB, InstantPersistence, InstantClasses, InstantConsts;

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

procedure TInstantIBXConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  AssignInstantStreamFormat(StreamFormatComboBox.Items);
  AssignInstantDataTypeStrings(IdDataTypeComboBox.Items);
  IdDataTypeComboBox.ItemIndex := Ord(dtString);
  IdSizeEdit.Text := IntToStr(InstantDefaultFieldSize);
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

end.

