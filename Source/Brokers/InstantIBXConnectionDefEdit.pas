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
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantIBXConnectionDefEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, InstantIBX;

type
  TInstantIBXConnectionDefEditForm = class(TForm)
    ClientPanel: TPanel;
    ServerLabel: TLabel;
    ProtocolLabel: TLabel;
    DatabaseLabel: TLabel;
    LocalRadioButton: TRadioButton;
    RemoteRadioButton: TRadioButton;
    ServerEdit: TEdit;
    ProtocolEdit: TComboBox;
    DatabaseEdit: TEdit;
    DatabaseButton: TButton;
    BottomPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    BottomBevel: TBevel;
    procedure LocalRemoteChange(Sender: TObject);
    procedure DatabaseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetIsValid: Boolean;
    procedure UpdateControls;
  public
    procedure LoadData(ConnectionDef: TInstantIBXConnectionDef);
    procedure SaveData(ConnectionDef: TInstantIBXConnectionDef);
    property IsValid: Boolean read GetIsValid;
  end;

implementation

{$R *.DFM}

uses
  IB;

{ TInstantIBXConnectionDefEditForm }

procedure TInstantIBXConnectionDefEditForm.DatabaseButtonClick(
  Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter :=
      'InterBase Database (*.gdb)|*.gdb|' +
      'All Files (*.*)|*.*';
    if Execute then
      DatabaseEdit.Text := FileName;
  finally
    Free;
  end;
end;

procedure TInstantIBXConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  UpdateControls;
end;

function TInstantIBXConnectionDefEditForm.GetIsValid: Boolean;
begin
  Result :=
    (LocalRadioButton.Checked or (ServerEdit.Text <> '')) and
    (DatabaseEdit.Text <> '');
end;

procedure TInstantIBXConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantIBXConnectionDef);
begin
  with ConnectionDef do
  begin
    LocalRadioButton.Checked := NetType = ntLocal;
    RemoteRadioButton.Checked := NetType <> ntLocal;
    ProtocolEdit.ItemIndex := Ord(NetType) - 1;
    ServerEdit.Text := ServerName;
    DatabaseEdit.Text := Path;
  end;
end;

procedure TInstantIBXConnectionDefEditForm.LocalRemoteChange(
  Sender: TObject);
begin
  UpdateControls;
end;

procedure TInstantIBXConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantIBXConnectionDef);
begin
  with ConnectionDef do
  begin
    if LocalRadioButton.Checked then
      NetType := ntLocal
    else
      NetType := TIBXNetType(ProtocolEdit.ItemIndex + 1);
    ServerName := ServerEdit.Text;
    Path := DatabaseEdit.Text;
  end;
end;

procedure TInstantIBXConnectionDefEditForm.UpdateControls;
const
  Colors: array[Boolean] of TColor = (clBtnFace, clWindow);
var
  UseRemote: Boolean;
begin
  UseRemote := RemoteRadioButton.Checked;
  ServerLabel.Enabled := UseRemote;
  ServerEdit.Enabled := UseRemote;
  ServerEdit.Color := Colors[UseRemote];
  ProtocolLabel.Enabled := UseRemote;
  ProtocolEdit.Enabled := UseRemote;
  ProtocolEdit.Color := Colors[UseRemote];
  DatabaseButton.Enabled := not UseRemote;
  with ProtocolEdit do
    if ItemIndex = -1 then
      ItemIndex := 0;
end;

end.
