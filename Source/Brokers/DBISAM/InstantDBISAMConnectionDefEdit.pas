(*
 *   InstantObjects
 *   DBISAM Connection Editor
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
 * Carlo Barazzetta, Juan J. V. Garcia
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBISAMConnectionDefEdit;

interface

{$IFNDEF VER130}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

uses
  Forms, StdCtrls, Controls, ExtCtrls, Classes, InstantDBISAM;

type
  TInstantDBISAMConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    OkButton: TButton;
    LocalRadioButton: TRadioButton;
    RemoteRadioButton: TRadioButton;
    HostLabel: TLabel;
    HostEdit: TEdit;
    NetworkLabel: TLabel;
    NetworkEdit: TComboBox;
    DatabaseEdit: TEdit;
    DirectoryButton: TButton;
    DatabaseLabel: TLabel;
    PortEdit: TEdit;
    PortLabel: TLabel;
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure DirectoryButtonClick(Sender: TObject);
    procedure LocalRemoteChange(Sender: TObject);
  private
    function GetIsValid: Boolean;
  protected
    procedure UpdateActions; override;
    procedure UpdateControls;
  public
    procedure LoadData(ConnectionDef: TInstantDBISAMConnectionDef);
    procedure SaveData(ConnectionDef: TInstantDBISAMConnectionDef);
    property IsValid: Boolean read GetIsValid;
  end;

implementation

{$R *.DFM}

uses
  DBISAMTb, Graphics, SysUtils, FileCtrl, InstantClasses, InstantPersistence;

{ TInstantDBISAMConnectionDefEditForm }

procedure TInstantDBISAMConnectionDefEditForm.DirectoryButtonClick(
  Sender: TObject);
var
  Dir: string;
begin
  Dir := DatabaseEdit.Text;
  if SelectDirectory('Select Directory', '', Dir) then
    DatabaseEdit.Text := Dir;
end;

procedure TInstantDBISAMConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  AssignInstantStreamFormat(StreamFormatComboBox.Items); //CB
  UpdateControls;
end;

function TInstantDBISAMConnectionDefEditForm.GetIsValid: Boolean;
begin
  Result :=
    (LocalRadioButton.Checked or (HostEdit.Text <> '')) and
    (DatabaseEdit.Text <> '');
end;

procedure TInstantDBISAMConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantDBISAMConnectionDef);
begin
  with ConnectionDef do
  begin
    LocalRadioButton.Checked := SessionType = stLocal;
    RemoteRadioButton.Checked := SessionType = stRemote;
    NetworkEdit.ItemIndex := Ord(RemoteType);
    HostEdit.Text := RemoteHost;
    PortEdit.Text := IntToStr(RemotePort);
    if SessionType = stLocal then
      DatabaseEdit.Text := Directory
    else
      DatabaseEdit.Text := RemoteDatabase;
    StreamFormatComboBox.ItemIndex := Ord(BlobStreamFormat); //CB
  end;
end;

procedure TInstantDBISAMConnectionDefEditForm.LocalRemoteChange(
  Sender: TObject);
begin
  UpdateControls;
end;

procedure TInstantDBISAMConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantDBISAMConnectionDef);
begin
  with ConnectionDef do
  begin
    if LocalRadioButton.Checked then
      SessionType := stLocal
    else
      SessionType := stRemote;
    RemoteType := TRemoteType(NetworkEdit.ItemIndex);
    RemoteHost := HostEdit.Text;
    RemotePort := StrToIntDef(PortEdit.Text, 0);
    if SessionType = stLocal then
      Directory := DatabaseEdit.Text
    else
      RemoteDatabase := DatabaseEdit.Text;
    BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex); //CB
  end;
end;

procedure TInstantDBISAMConnectionDefEditForm.UpdateActions;
begin
  inherited;
  OkButton.Enabled := IsValid;
end;

procedure TInstantDBISAMConnectionDefEditForm.UpdateControls;
const
  Colors: array[Boolean] of TColor = (clBtnFace, clWindow);
var
  UseRemote: Boolean;
begin
  UseRemote := RemoteRadioButton.Checked;
  HostLabel.Enabled := UseRemote;
  HostEdit.Enabled := UseRemote;
  HostEdit.Color := Colors[UseRemote];
  PortEdit.Enabled := UseRemote;
  PortLabel.Enabled := UseRemote;
  PortEdit.Color := Colors[UseRemote];
  NetworkLabel.Enabled := UseRemote;
  NetworkEdit.Enabled := UseRemote;
  NetworkEdit.Color := Colors[UseRemote];
  with NetworkEdit do
    if ItemIndex = -1 then
      ItemIndex := 0;
  DirectoryButton.Enabled := not UseRemote;
end;

end.
