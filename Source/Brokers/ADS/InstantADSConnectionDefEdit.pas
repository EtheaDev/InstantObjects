(*
 *   InstantObjects
 *   Advantage Database Server Connection Editor
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
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantADSConnectionDefEdit;

{$IFNDEF VER130}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

interface

uses
  Forms, StdCtrls, Controls, ExtCtrls, Classes, InstantADS, Buttons;

type
  TInstantADSConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    OkButton: TButton;
    DatabaseEdit: TComboBox;
    DatabaseLabel: TLabel;
    DirectoryButton: TBitBtn;
    DictionaryButton: TBitBtn;
    ConnectionTypeGroupBox: TGroupBox;
    LocalCheckBox: TCheckBox;
    RemoteCheckBox: TCheckBox;
    InternetCheckBox: TCheckBox;
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    procedure DatabaseEditDropDown(Sender: TObject);
    procedure DirectoryButtonClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadAliases;
  public
    procedure LoadData(ConnectionDef: TInstantADSConnectionDef);
    procedure SaveData(ConnectionDef: TInstantADSConnectionDef);
  end;

implementation

{$R *.DFM}

uses
  SysUtils, FileCtrl, Dialogs, IniFiles, InstantPersistence, InstantClasses;

const
  SDictionaryExt = '.add';
  SDictionaryFilter = 'Advantage Database Dictionary Files (*.add)|*.add';

{ TInstantADSConnectionDefEditForm }

procedure TInstantADSConnectionDefEditForm.DatabaseEditDropDown(
  Sender: TObject);
begin
  LoadAliases;
end;

procedure TInstantADSConnectionDefEditForm.DictionaryButtonClick(
  Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Title := 'Select Data Dictionary';
    Filter := SDictionaryFilter;
    if SameText(ExtractFileExt(DatabaseEdit.Text), SDictionaryExt) then
      FileName := DatabaseEdit.Text
    else
      InitialDir := DatabaseEdit.Text;
    if Execute then
      DatabaseEdit.Text := FileName;
  finally
    Free;
  end;
end;

procedure TInstantADSConnectionDefEditForm.DirectoryButtonClick(
  Sender: TObject);
var
  Dir: string;
begin
  Dir := DatabaseEdit.Text;
  if SelectDirectory('Select Directory', '', Dir) then
    DatabaseEdit.Text := Dir;
end;

procedure TInstantADSConnectionDefEditForm.LoadAliases;

  procedure GetAliasList(List: TStrings);
  const
    AdsIni = 'ADS.INI';
  var
    IniFileName: string;
  begin
    IniFileName := ExtractFilePath(Application.ExeName) + AdsIni;
    if not FileExists(IniFileName) then
      IniFileName := AdsIni;
    with TIniFile.Create(IniFileName) do
    try
      ReadSection('Databases', List);
    finally
      Free;
    end;
  end;

var
  Aliases: TStringList;
  I: Integer;
begin
  if DatabaseEdit.Items.Count > 0 then
    Exit;
  Aliases := TStringList.Create;
  try
    GetAliasList(Aliases);
    with DatabaseEdit.Items do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to Pred(Aliases.Count) do
          Add(Aliases[I]);
      finally
        EndUpdate;
      end;
    end;
  finally
    Aliases.Free;
  end;
end;

procedure TInstantADSConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantADSConnectionDef);
begin
  with ConnectionDef do
  begin
    if Alias <> '' then
      DatabaseEdit.Text := Alias
    else
      DatabaseEdit.Text := ConnectPath;
    LocalCheckBox.Checked := ctLocal in ConnectionTypes;
    RemoteCheckBox.Checked := ctRemote in ConnectionTypes;
    InternetCheckBox.Checked := ctInternet in ConnectionTypes; 
    StreamFormatComboBox.ItemIndex := Ord(BlobStreamFormat); //CB
  end;
end;

procedure TInstantADSConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantADSConnectionDef);
var
  ConnTypes: TInstantADSConnectionTypes;
begin
  with ConnectionDef do
  begin
    if DatabaseEdit.ItemIndex >= 0 then
      Alias := DatabaseEdit.Text
    else
      ConnectPath := DatabaseEdit.Text;
    ConnTypes := [];
    if LocalCheckBox.Checked then
      Include(ConnTypes, ctLocal);
    if RemoteCheckBox.Checked then
      Include(ConnTypes, ctRemote);
    if InternetCheckBox.Checked then
      Include(ConnTypes, ctInternet);
    ConnectionTypes := ConnTypes;
    BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex); //CB
  end;
end;

procedure TInstantADSConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  AssignInstantStreamFormat(StreamFormatComboBox.Items); //CB
end;

end.
