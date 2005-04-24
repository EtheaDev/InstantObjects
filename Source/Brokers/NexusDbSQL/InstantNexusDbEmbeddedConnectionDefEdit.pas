(*
 *   InstantObjects
 *   NexusDb Support
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
 * The Original Code is: Bert Moorthaemer
 *
 * The Initial Developer of the Original Code is: Bert Moorthaemer
 *
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 * 
 * ***** END LICENSE BLOCK ***** *)

unit InstantNexusDbEmbeddedConnectionDefEdit;

interface

uses
  Forms, StdCtrls, Controls, ExtCtrls, Classes, InstantNexusDbEmbedded,
  ComCtrls, Graphics, Buttons;

type
  TInstantNexusDbEmbeddedConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    OkButton: TButton;
    DatabaseRadGrp: TRadioGroup;
    AliasesLabel: TLabel;
    PathLabel: TLabel;
    PathEdit: TEdit;
    NexusDBLogo: TImage;
    AliasesCbx: TComboBox;
    LoadAliasesButton: TSpeedButton;
    BrowseButton: TSpeedButton;
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    IDGroupBox: TGroupBox;
    lblIdDataType: TLabel;
    IdDataTypeComboBox: TComboBox;
    lblIdSize: TLabel;
    IdSizeEdit: TEdit;
    procedure LoadAliasesButtonClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetAlias: string;
    function GetAliasIsPath: Boolean;

    procedure SetAlias(const Value: string);
    procedure SetAliasIsPath(Value: Boolean);

    procedure LoadAliases(aList: TStrings);

    property Alias: string
      read GetAlias write SetAlias;
    property AliasIsPath: Boolean
      read GetAliasIsPath write SetAliasIsPath;
  protected
    procedure UpdateActions; override;
  public
    procedure LoadData(ConnectionDef: TInstantNexusDbSQLEmbeddedConnectionDef);
    procedure SaveData(ConnectionDef: TInstantNexusDbSQLEmbeddedConnectionDef);
  end;

implementation

{$R *.DFM}

uses
  SysUtils,
  FileCtrl,
  InstantClasses,
  InstantPersistence,
  InstantConsts,
  InstantNexusDbConsts;

{ TInstantNexusDbEmbeddedConnectionDefEditForm }

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.UpdateActions;
begin
  inherited;
  AliasesCbx.Enabled := not AliasIsPath;
  LoadAliasesButton.Enabled := AliasesCbx.Enabled;
  PathEdit.Enabled := AliasIsPath;
  BrowseButton.Enabled := PathEdit.Enabled;
  OkButton.Enabled :=
    not AliasIsPath or (AliasIsPath and DirectoryExists(Alias));
end;

type
  TConnectionDefCast = class(TInstantNexusDbSQLEmbeddedConnectionDef);

function TInstantNexusDbEmbeddedConnectionDefEditForm.GetAlias: string;
begin
  if AliasIsPath then
    Result := PathEdit.Text
  else if AliasesCbx.ItemIndex < 0 then
    Result := ''
  else
    Result := AliasesCbx.Items.Strings[AliasesCbx.ItemIndex];
end;

function TInstantNexusDbEmbeddedConnectionDefEditForm.GetAliasIsPath: Boolean;
begin
  if DatabaseRadGrp.ItemIndex < 0 then
    Result := False
  else
    Result := Boolean(DatabaseRadGrp.ItemIndex);
end;

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.SetAlias(const Value:
  string);
begin
  if Value <> Alias then
  begin
    if AliasIsPath then
      PathEdit.Text := Trim(Value)
    else
      AliasesCbx.ItemIndex := AliasesCbx.Items.IndexOf(Value);
  end;
end;

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.SetAliasIsPath(Value:
  Boolean);
begin
  if Value <> AliasIsPath then
  begin
    Alias := '';
    DatabaseRadGrp.ItemIndex := Ord(Value);
    Alias := '';
  end;
end;

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.LoadAliases(aList:
  TStrings);
begin
  try
    TInstantNexusDbSQLEmbeddedConnectionDef.LoadAliasList(aList);
  except
    // eat it
  end;
end;

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.LoadData(ConnectionDef:
  TInstantNexusDbSQLEmbeddedConnectionDef);
var
  SavedCursor: TCursor;
begin
  SavedCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    with ConnectionDef do
    begin
      Self.LoadAliases(AliasesCbx.Items);

      Self.AliasIsPath := AliasIsPath;
      Self.Alias := Alias;

      // Begin SRM - 14 Mar 2005
      StreamFormatComboBox.ItemIndex := Ord(ConnectionDef.BlobStreamFormat);
      IdDataTypeComboBox.ItemIndex := Ord(ConnectionDef.IdDataType);
      IdSizeEdit.Text := IntToStr(ConnectionDef.IdSize);
      // End SRM - 14 Mar 2005
    end;
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.SaveData(ConnectionDef:
  TInstantNexusDbSQLEmbeddedConnectionDef);
begin
  with ConnectionDef do
  begin
    Alias := Self.Alias;
    AliasIsPath := Self.AliasIsPath;

    // Begin SRM - 14 Mar 2005
    ConnectionDef.BlobStreamFormat :=
      TInstantStreamFormat(StreamFormatComboBox.ItemIndex);
    ConnectionDef.IdDataType := TInstantDataType(IdDataTypeComboBox.ItemIndex);
    ConnectionDef.IdSize := StrToInt(IdSizeEdit.Text);
    // End SRM - 14 Mar 2005
  end;
end;

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.LoadAliasesButtonClick(
  Sender: TObject);
begin
  LoadAliases(AliasesCbx.Items);
  with AliasesCbx, Items do
    ItemIndex := IndexOf(Alias);
end;

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.BrowseButtonClick(
  Sender: TObject);
var
  AliasDir: string;
begin
  AliasDir := Alias;
  if SelectDirectory(SSelectAnAliasPathPlease, '', AliasDir) then
    Alias := AliasDir;
end;

procedure TInstantNexusDbEmbeddedConnectionDefEditForm.FormCreate(Sender:
  TObject);
begin
  // Begin SRM - 14 Mar 2005
  AssignInstantStreamFormat(StreamFormatComboBox.Items);
  AssignInstantDataTypeStrings(IdDataTypeComboBox.Items);
  IdDataTypeComboBox.ItemIndex := Ord(dtString);
  IdSizeEdit.Text := IntToStr(InstantDefaultFieldSize);
  // End SRM - 14 Mar 2005
end;

end.


