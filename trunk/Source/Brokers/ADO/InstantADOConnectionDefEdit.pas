(*
 *   InstantObjects
 *   ADO Connection Editor
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
 * Carlo Barazzetta: blob streaming in XML format (Part, Parts, References)
 * Carlo Barazzetta: Currency and LoginPrompt support
 * ***** END LICENSE BLOCK ***** *)

unit InstantADOConnectionDefEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, InstantADO;

type
  TInstantADOConnectionDefEditForm = class(TForm)
    ClientPanel: TPanel;
    DataLinkRadioButton: TRadioButton;
    DataLinkEdit: TEdit;
    DataLinkButton: TButton;
    ConnectionStringRadioButton: TRadioButton;
    ConnectionStringEdit: TEdit;
    ConnectionStringButton: TButton;
    BottomPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    BottomBevel: TBevel;
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    LoginPromptCheckBox: TCheckBox;
    procedure ConnectionStringButtonClick(Sender: TObject);
    procedure DataLinkButtonClick(Sender: TObject);
    procedure DataChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetIsValid: Boolean;
    procedure UpdateControls;
  public
    procedure LoadData(ConnectionDef: TInstantADOConnectionDef);
    procedure SaveData(ConnectionDef: TInstantADOConnectionDef);
    property IsValid: Boolean read GetIsValid;
  end;

implementation

{$R *.DFM}

uses
  ADODB, InstantPersistence, InstantClasses;

{ TInstantADOConnDefEditForm }

procedure TInstantADOConnectionDefEditForm.ConnectionStringButtonClick(
  Sender: TObject);
begin
  with ConnectionStringEdit do
    Text := PromptDataSource(Handle, Text);
end;

procedure TInstantADOConnectionDefEditForm.DataChanged(Sender: TObject);
begin
  UpdateControls;
end;

procedure TInstantADOConnectionDefEditForm.DataLinkButtonClick(Sender: TObject);
begin
  with DataLinkEdit do
    Text := PromptDataLinkFile(Handle, Text);
end;

procedure TInstantADOConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  AssignInstantStreamFormat(StreamFormatComboBox.Items); //CB
  UpdateControls;
end;

function TInstantADOConnectionDefEditForm.GetIsValid: Boolean;
begin
  Result :=
    (DataLinkRadioButton.Checked and
    (DataLinkEdit.Text <> '')) or
    (ConnectionStringRadioButton.Checked and
    (ConnectionStringEdit.Text <> ''));
end;

procedure TInstantADOConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantADOConnectionDef);
begin
  with ConnectionDef do
  begin
    if LinkFileName = '' then
    begin
      ConnectionStringRadioButton.Checked := True;
      ConnectionStringEdit.Text := ConnectionString;
    end else
    begin
      DataLinkRadioButton.Checked := True;
      DataLinkEdit.Text := LinkFileName;
    end;
    //CB
    StreamFormatComboBox.ItemIndex := Ord(BlobStreamFormat);
    LoginPromptCheckBox.Checked := LoginPrompt;
  end;
end;

procedure TInstantADOConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantADOConnectionDef);
begin
  with ConnectionDef do
  begin
    if DataLinkRadioButton.Checked then
      ConnectionString := 'FILE NAME=' + DataLinkEdit.Text
    else
      ConnectionString := ConnectionStringEdit.Text;
    //CB
    BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex);
    LoginPrompt := LoginPromptCheckBox.Checked;
  end;
end;

procedure TInstantADOConnectionDefEditForm.UpdateControls;
const
  Colors: array[Boolean] of TColor = (clBtnFace, clWindow);
var
  UseDataLink: Boolean;
begin
  OkButton.Enabled := IsValid;
  UseDataLink := DataLinkRadioButton.Checked;
  ConnectionStringEdit.Enabled := not UseDataLink;
  ConnectionStringEdit.Color := Colors[not UseDataLink];
  ConnectionStringButton.Enabled := not UseDataLink;
  DataLinkEdit.Enabled := UseDataLink;
  DataLinkEdit.Color := Colors[UseDataLink];
  DataLinkButton.Enabled := UseDataLink;
end;

end.
