(*
 *   InstantObjects
 *   TurboPower FlashFiler Connection Editor
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

unit InstantFlashFilerConnectionDefEdit;

interface

{$IFNDEF VER130}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

uses
  Forms, StdCtrls, Controls, ExtCtrls, Classes, InstantFlashFiler;

type
  TInstantFlashFilerConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    OkButton: TButton;
    AliasLabel: TLabel;
    AliasEdit: TComboBox;
    BrowseButton: TButton;
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    procedure AliasEditDropDown(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure LoadAliasNames;
  public
    procedure LoadData(ConnectionDef: TInstantFlashFilerConnectionDef);
    procedure SaveData(ConnectionDef: TInstantFlashFilerConnectionDef);
  end;

implementation

uses
  FFDB, FileCtrl, InstantPersistence, InstantClasses;

{$R *.DFM}

{ TInstantFlashFilerConnectionDefEditForm }

procedure TInstantFlashFilerConnectionDefEditForm.AliasEditDropDown(
  Sender: TObject);
begin
  LoadAliasNames;
end;

procedure TInstantFlashFilerConnectionDefEditForm.BrowseButtonClick(
  Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('Database Directory', '', Dir) then
    AliasEdit.Text := Dir;
end;

procedure TInstantFlashFilerConnectionDefEditForm.LoadAliasNames;
var
  SaveCursor: TCursor;
begin
  with AliasEdit do
  begin
    if Items.Count <> 0 then
      Exit;
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      ffSession.GetAliasNames(Items);
    finally
      Screen.Cursor := SaveCursor;
    end;
  end;
end;

procedure TInstantFlashFilerConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantFlashFilerConnectionDef);
begin
  AliasEdit.Text := ConnectionDef.AliasName;
  StreamFormatComboBox.ItemIndex := Ord(ConnectionDef.BlobStreamFormat); //CB
end;

procedure TInstantFlashFilerConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantFlashFilerConnectionDef);
begin
  ConnectionDef.AliasName := AliasEdit.Text;
  ConnectionDef.BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex); //CB
end;

procedure TInstantFlashFilerConnectionDefEditForm.FormCreate(
  Sender: TObject);
begin
  AssignInstantStreamFormat(StreamFormatComboBox.Items); //CB
end;

end.
