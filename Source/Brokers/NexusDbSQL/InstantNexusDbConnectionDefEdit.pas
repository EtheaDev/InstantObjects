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

unit InstantNexusDbConnectionDefEdit;

interface

uses
  Forms, StdCtrls, Controls, ExtCtrls, Classes, InstantNexusDb, ComCtrls,
  Graphics, Buttons;

type
  TInstantNexusDbConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    OkButton: TButton;
    DatabaseRadGrp: TRadioGroup;
    ServersCbx: TComboBox;
    AliasesLabel: TLabel;
    PathLabel: TLabel;
    PathEdit: TEdit;
    ServerLabel: TLabel;
    TransportTypeRadGrp: TRadioGroup;
    PortLabel: TLabel;
    PortEdit: TEdit;
    PortUpDown: TUpDown;
    NexusDBLogo: TImage;
    LoadServersButton: TSpeedButton;
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
    procedure LoadServersButtonClick(Sender: TObject);
    procedure LoadAliasesButtonClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetAlias: string;
    function GetAliasIsPath: Boolean;
    function GetPort: Integer;
    function GetProtocolType: TInstantNexusDbProtocolType;
    function GetServerName: string;

    procedure SetAlias(const Value: string);
    procedure SetAliasIsPath(Value: Boolean);
    procedure SetPort(Value: Integer);
    procedure SetProtocolType(Value: TInstantNexusDbProtocolType);
    procedure SetServerName(const Value: string);

    procedure LoadServers(aList: TStrings);
    procedure LoadAliases(aList: TStrings);

    property Alias: string
      read GetAlias write SetAlias;
    property AliasIsPath: Boolean
      read GetAliasIsPath write SetAliasIsPath;
    property Port: Integer
      read GetPort write SetPort;
    property ProtocolType: TInstantNexusDbProtocolType
      read GetProtocolType write SetProtocolType;
    property ServerName: string
      read GetServerName write SetServerName;
  protected
    procedure UpdateActions; override;
  public
    procedure LoadData(ConnectionDef: TInstantNexusDbSQLConnectionDef);
    procedure SaveData(ConnectionDef: TInstantNexusDbSQLConnectionDef);
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

{ TInstantNexusDbConnectionDefEditForm }

procedure TInstantNexusDbConnectionDefEditForm.UpdateActions;
begin
  inherited;
  LoadServersButton.Enabled := TransportTypeRadGrp.Enabled;
  AliasesCbx.Enabled := not AliasIsPath;
  LoadAliasesButton.Enabled := AliasesCbx.Enabled;
  PathEdit.Enabled := AliasIsPath;
  BrowseButton.Enabled := PathEdit.Enabled;
  OkButton.Enabled :=
    (not AliasIsPath or (AliasIsPath and DirectoryExists(Alias))) and
    (ServerName > '') and
    (Alias > '');
end;

type
  TConnectionDefCast = class(TInstantNexusDbSQLConnectionDef);

function TInstantNexusDbConnectionDefEditForm.GetAlias: string;
begin
  if AliasIsPath then
    Result := PathEdit.Text
  else if AliasesCbx.ItemIndex < 0 then
    Result := ''
  else
    Result := AliasesCbx.Items.Strings[AliasesCbx.ItemIndex];
end;

function TInstantNexusDbConnectionDefEditForm.GetAliasIsPath: Boolean;
begin
  if DatabaseRadGrp.ItemIndex < 0 then
    Result := False
  else
    Result := Boolean(DatabaseRadGrp.ItemIndex);
end;

function TInstantNexusDbConnectionDefEditForm.GetPort: Integer;
begin
  Result := PortUpDown.Position;
end;

function TInstantNexusDbConnectionDefEditForm.GetProtocolType:
  TInstantNexusDbProtocolType;
begin
  if TransportTypeRadGrp.ItemIndex < 0 then
    Result := ptTCPIP
  else
    Result := TInstantNexusDbProtocolType(TransportTypeRadGrp.ItemIndex);
end;

function TInstantNexusDbConnectionDefEditForm.GetServerName: string;
begin
  if ServersCbx.ItemIndex < 0 then
    Result := ''
  else
    Result := ServersCbx.Items.Strings[ServersCbx.ItemIndex];
end;

procedure TInstantNexusDbConnectionDefEditForm.SetAlias(const Value: string);
begin
  if Value <> Alias then
  begin
    if AliasIsPath then
      PathEdit.Text := Trim(Value)
    else
      AliasesCbx.ItemIndex := AliasesCbx.Items.IndexOf(Value);
  end;
end;

procedure TInstantNexusDbConnectionDefEditForm.SetAliasIsPath(Value: Boolean);
begin
  if Value <> AliasIsPath then
  begin
    Alias := '';
    DatabaseRadGrp.ItemIndex := Ord(Value);
    Alias := '';
  end;
end;

procedure TInstantNexusDbConnectionDefEditForm.SetPort(Value: Integer);
begin
  if Value <> PortUpDown.Position then
  begin
    PortUpDown.Position := Value;
    ServersCbx.Items.Clear;
    ServerName := '';
    AliasesCbx.Items.Clear;
    Alias := '';
  end;
end;

procedure TInstantNexusDbConnectionDefEditForm.SetProtocolType(Value:
  TInstantNexusDbProtocolType);
begin
  if Value <> ProtocolType then
  begin
    TransportTypeRadGrp.ItemIndex := Ord(Value);
    ServersCbx.Items.Clear;
    ServerName := '';
    AliasesCbx.Items.Clear;
    Alias := '';
  end;
end;

procedure TInstantNexusDbConnectionDefEditForm.SetServerName(const Value:
  string);
begin
  if Value <> ServerName then
  begin
    ServersCbx.ItemIndex := ServersCbx.Items.IndexOf(Value);
    AliasesCbx.Items.Clear;
    Alias := '';
  end;
end;

procedure TInstantNexusDbConnectionDefEditForm.LoadServers(aList: TStrings);
var
  SavedCursor: TCursor;
begin
  try
    SavedCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      TInstantNexusDbSQLConnectionDef.LoadServerList(
        ProtocolType, Port, aList);
    finally
      Screen.Cursor := SavedCursor;
    end;
  except
    // eat it
  end;
end;

procedure TInstantNexusDbConnectionDefEditForm.LoadAliases(aList: TStrings);
var
  SavedCursor: TCursor;
begin
  try
    SavedCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      TInstantNexusDbSQLConnectionDef.LoadAliasList(
        ProtocolType, ServerName, Port, aList);
    finally
      Screen.Cursor := SavedCursor;
    end;
  except
    // eat it
  end;
end;

procedure TInstantNexusDbConnectionDefEditForm.LoadData(ConnectionDef:
    TInstantNexusDbSQLConnectionDef);
var
  SavedCursor: TCursor;
begin
  SavedCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    with ConnectionDef do
    begin
      Self.ProtocolType := ProtocolType;
      Self.Port := Port;

      Self.LoadServers(ServersCbx.Items);

      Self.ServerName := ServerName;
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

procedure TInstantNexusDbConnectionDefEditForm.SaveData(ConnectionDef:
    TInstantNexusDbSQLConnectionDef);
begin
  with ConnectionDef do
  begin
    ProtocolType := Self.ProtocolType;
    ServerName := Self.ServerName;
    Port := Self.Port;
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

procedure TInstantNexusDbConnectionDefEditForm.LoadServersButtonClick(
  Sender: TObject);
begin
  LoadServers(ServersCbx.Items);
  with ServersCbx, Items do
    ItemIndex := IndexOf(ServerName);
end;

procedure TInstantNexusDbConnectionDefEditForm.LoadAliasesButtonClick(
  Sender: TObject);
begin
  LoadAliases(AliasesCbx.Items);
  with AliasesCbx, Items do
    ItemIndex := IndexOf(Alias);
end;

procedure TInstantNexusDbConnectionDefEditForm.BrowseButtonClick(
  Sender: TObject);
var
  AliasDir: string;
begin
  AliasDir := Alias;
  if SelectDirectory(SSelectAnAliasPathPlease, '', AliasDir) then
    Alias := AliasDir;
end;

procedure TInstantNexusDbConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  // Begin SRM - 14 Mar 2005
  AssignInstantStreamFormat(StreamFormatComboBox.Items);
  AssignInstantDataTypeStrings(IdDataTypeComboBox.Items);
  IdDataTypeComboBox.ItemIndex := Ord(dtString);
  IdSizeEdit.Text := IntToStr(InstantDefaultFieldSize);
  // End SRM - 14 Mar 2005
end;

end.

