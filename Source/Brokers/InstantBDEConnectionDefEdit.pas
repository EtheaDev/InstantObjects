(*
 *   InstantObjects
 *   BDE Connection Editor
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

unit InstantBDEConnectionDefEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  InstantBDE, StdCtrls, ExtCtrls;

type
  TInstantBDEConnectionDefEditForm = class(TForm)
    ClientPanel: TPanel;
    AliasLabel: TLabel;
    DriverLabel: TLabel;
    ParametersLabel: TLabel;
    AliasComboBox: TComboBox;
    DriverComboBox: TComboBox;
    ParametersEdit: TMemo;
    BottomPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    BottomBevel: TBevel;
    procedure AliasComboBoxChange(Sender: TObject);
    procedure DriverComboBoxChange(Sender: TObject);
    procedure DriverComboBoxDropDown(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AliasComboBoxDropDown(Sender: TObject);
    procedure ParametersEditChange(Sender: TObject);
  private
    function GetIsValid: Boolean;
    procedure LoadAliasNames;
    procedure LoadDriverNames;
    procedure UpdateControls;
  public
    procedure LoadData(ConnectionDef: TInstantBDEConnectionDef);
    procedure SaveData(ConnectionDef: TInstantBDEConnectionDef);
    property IsValid: Boolean read GetIsValid;
  end;

implementation

{$R *.DFM}

uses
  DbTables;

const
  NoAlias = '(None)';

{ TInstantBDEConnectionDefEditForm }

procedure TInstantBDEConnectionDefEditForm.AliasComboBoxChange(
  Sender: TObject);
begin
  LoadAliasNames;
  UpdateControls;
end;

procedure TInstantBDEConnectionDefEditForm.AliasComboBoxDropDown(
  Sender: TObject);
begin
  LoadAliasNames;
  UpdateControls;
end;

procedure TInstantBDEConnectionDefEditForm.DriverComboBoxChange(
  Sender: TObject);
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    Session.GetDriverParams(DriverComboBox.Text, Params);
    ParametersEdit.Text := Params.Text;
  finally
    Params.Free;
  end;
end;

procedure TInstantBDEConnectionDefEditForm.DriverComboBoxDropDown(
  Sender: TObject);
begin
  LoadDriverNames;
end;

procedure TInstantBDEConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  with AliasComboBox do
  begin
    Items.Add(NoAlias);
    ItemIndex := 0;
  end;
end;

function TInstantBDEConnectionDefEditForm.GetIsValid: Boolean;
begin
  Result :=
    (DriverComboBox.Text <> '') and
    (ParametersEdit.Text <> '');
end;

procedure TInstantBDEConnectionDefEditForm.LoadAliasNames;
var
  SaveCursor: TCursor;
begin
  with AliasComboBox do
    if Items.Count = 1 then
    begin
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        Session.GetAliasNames(AliasComboBox.Items);
        AliasComboBox.Items.Insert(0, NoAlias);
        ItemIndex := 0;
      finally
        Screen.Cursor := SaveCursor;
      end;
    end;
end;

procedure TInstantBDEConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantBDEConnectionDef);
begin
  with ConnectionDef do
  begin
    if AliasName <> ''then
    begin
      LoadAliasNames;
      with AliasComboBox do
        ItemIndex := Items.IndexOf(AliasName);
    end else
    begin
      LoadDriverNames;
      with DriverComboBox do
        ItemIndex := Items.IndexOf(DriverName);
    end;
    UpdateControls;
    ParametersEdit.Text := Parameters;
  end;
end;

procedure TInstantBDEConnectionDefEditForm.LoadDriverNames;
begin
  with DriverComboBox do
    if Items.Count = 0 then
      Session.GetDriverNames(Items);
end;

procedure TInstantBDEConnectionDefEditForm.ParametersEditChange(
  Sender: TObject);
begin
  UpdateControls;
end;

procedure TInstantBDEConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantBDEConnectionDef);
var
  Alias: string;
begin
  with ConnectionDef do
  begin
    Alias := AliasComboBox.Text;
    if Alias = NoAlias then
      DriverName := DriverComboBox.Text
    else
      AliasName := Alias;
    Parameters := ParametersEdit.Text;
  end;
end;

procedure TInstantBDEConnectionDefEditForm.UpdateControls;
const
  Colors: array[Boolean] of TColor = (clBtnFace, clWindow);
var
  Alias: string;
  ViaAlias: Boolean;
  Params: TStringList;
begin
  OkButton.Enabled := IsValid;
  Alias := AliasComboBox.Text;
  ViaAlias := (Alias <> NoAlias) and (Alias <> '');
  DriverComboBox.Enabled := not ViaAlias;
  DriverComboBox.Color := Colors[not ViaAlias];
  if ViaAlias then
  begin
    DriverComboBox.Style := csDropDown;
    DriverComboBox.Text := Session.GetAliasDriverName(Alias);
    Params := TStringList.Create;
    try
      Session.GetAliasParams(Alias, Params);
      ParametersEdit.Text := Params.Text;
    finally
      Params.Free;
    end;
  end else
    DriverComboBox.Style := csDropDownList;
end;

end.
