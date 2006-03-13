(*
 *   InstantObjects
 *   NexusDB Support
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

unit InstantNexusDBConnectionDefEdit;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}
{$I InstantNexusDBDefines.inc}

interface

uses
  Forms, StdCtrls, Controls, ExtCtrls, Classes, InstantNexusDB, ComCtrls,
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
    function GetProtocolType: TInstantNexusDBProtocolType;
    function GetServerName: string;

    procedure SetAlias(const Value: string);
    procedure SetAliasIsPath(Value: Boolean);
    procedure SetPort(Value: Integer);
    procedure SetProtocolType(Value: TInstantNexusDBProtocolType);
    procedure SetServerName(const Value: string);

    procedure LoadServers(aList: TStrings);
    procedure LoadAliases(aList: TStrings);

    property Alias: string
      read GetAlias write SetAlias;
    property AliasIsPath: Boolean
      read GetAliasIsPath write SetAliasIsPath;
    property Port: Integer
      read GetPort write SetPort;
    property ProtocolType: TInstantNexusDBProtocolType
      read GetProtocolType write SetProtocolType;
    property ServerName: string
      read GetServerName write SetServerName;
  protected
    procedure UpdateActions; override;
  public
    procedure LoadData(ConnectionDef: TInstantNexusDBConnectionDef);
    procedure SaveData(ConnectionDef: TInstantNexusDBConnectionDef);
  end;

implementation

{$R *.DFM}

uses
  SysUtils,
{$IFNDEF LINUX}
  {$IFDEF D6+}
    {$WARN UNIT_PLATFORM OFF}
  {$ENDIF}
    FileCtrl,   // for SelectDirectory()
  {$IFDEF D6+}
    {$WARN UNIT_PLATFORM ON}
  {$ENDIF}
{$ENDIF}
  InstantClasses,
  InstantPersistence,
  InstantConsts,
  InstantNexusDBConsts;

{ TInstantNexusDBConnectionDefEditForm }

procedure TInstantNexusDBConnectionDefEditForm.UpdateActions;
begin
  inherited;
  LoadServersButton.Enabled := TransportTypeRadGrp.Enabled;
  AliasesCbx.Enabled := not AliasIsPath and ServersCbx.Enabled;
  LoadAliasesButton.Enabled := AliasesCbx.Enabled;
  PathEdit.Enabled := AliasIsPath and ServersCbx.Enabled;
  BrowseButton.Enabled := PathEdit.Enabled;
  OkButton.Enabled :=
    (ServerName <> '') and 
    ((not AliasIsPath and (Alias <> '')) or
      (AliasIsPath and DirectoryExists(Alias)));
end;

type
  TConnectionDefCast = class(TInstantNexusDBConnectionDef);

function TInstantNexusDBConnectionDefEditForm.GetAlias: string;
begin
  if AliasIsPath then
    Result := PathEdit.Text
  else if AliasesCbx.ItemIndex < 0 then
    Result := ''
  else
    Result := AliasesCbx.Items.Strings[AliasesCbx.ItemIndex];
end;

function TInstantNexusDBConnectionDefEditForm.GetAliasIsPath: Boolean;
begin
  if DatabaseRadGrp.ItemIndex < 0 then
    Result := False
  else
    Result := Boolean(DatabaseRadGrp.ItemIndex);
end;

function TInstantNexusDBConnectionDefEditForm.GetPort: Integer;
begin
  Result := PortUpDown.Position;
end;

function TInstantNexusDBConnectionDefEditForm.GetProtocolType:
  TInstantNexusDBProtocolType;
begin
  if TransportTypeRadGrp.ItemIndex < 0 then
    Result := ptTCPIP
  else
    Result := TInstantNexusDBProtocolType(TransportTypeRadGrp.ItemIndex);
end;

function TInstantNexusDBConnectionDefEditForm.GetServerName: string;
begin
  if ServersCbx.ItemIndex < 0 then
    Result := ''
  else
    Result := ServersCbx.Items.Strings[ServersCbx.ItemIndex];
end;

procedure TInstantNexusDBConnectionDefEditForm.SetAlias(const Value: string);
begin
  if Value <> Alias then
  begin
    if AliasIsPath then
      PathEdit.Text := Trim(Value)
    else
      AliasesCbx.ItemIndex := AliasesCbx.Items.IndexOf(Value);
  end;
end;

procedure TInstantNexusDBConnectionDefEditForm.SetAliasIsPath(Value: Boolean);
begin
  if Value <> AliasIsPath then
  begin
    Alias := '';
    DatabaseRadGrp.ItemIndex := Ord(Value);
    Alias := '';
  end;
end;

procedure TInstantNexusDBConnectionDefEditForm.SetPort(Value: Integer);
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

procedure TInstantNexusDBConnectionDefEditForm.SetProtocolType(Value:
  TInstantNexusDBProtocolType);
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

procedure TInstantNexusDBConnectionDefEditForm.SetServerName(const Value:
  string);
begin
  if Value <> ServerName then
  begin
    ServersCbx.ItemIndex := ServersCbx.Items.IndexOf(Value);
    AliasesCbx.Items.Clear;
    Alias := '';
  end;
end;

procedure TInstantNexusDBConnectionDefEditForm.LoadServers(aList: TStrings);
var
  SavedCursor: TCursor;
begin
  try
    SavedCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      TInstantNexusDBConnectionDef.LoadServerList(
        ProtocolType, Port, aList);
    finally
      Screen.Cursor := SavedCursor;
    end;
  except
    // eat it
  end;
end;

procedure TInstantNexusDBConnectionDefEditForm.LoadAliases(aList: TStrings);
var
  SavedCursor: TCursor;
begin
  if ServerName = '' then
    Exit;

  try
    SavedCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      TInstantNexusDBConnectionDef.LoadAliasList(
        ProtocolType, ServerName, Port, aList);
    finally
      Screen.Cursor := SavedCursor;
    end;
  except
    // eat it
  end;
end;

procedure TInstantNexusDBConnectionDefEditForm.LoadData(ConnectionDef:
    TInstantNexusDBConnectionDef);
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

      Self.ServerName := ServerName;

      if ServerName <> '' then
      begin
        ServersCbx.Items.Add(ServerName);
        ServersCbx.ItemIndex := ServersCbx.Items.IndexOf(ServerName);

        Self.LoadAliases(AliasesCbx.Items);

        Self.AliasIsPath := AliasIsPath;
        Self.Alias := Alias;
      end
      else
      begin
        ServersCbx.Text := SNone;
        ServersCbx.Enabled := False;
        AliasesCbx.Text := SNone;
      end;

      StreamFormatComboBox.ItemIndex := Ord(ConnectionDef.BlobStreamFormat);
      IdDataTypeComboBox.ItemIndex := Ord(ConnectionDef.IdDataType);
      IdSizeEdit.Text := IntToStr(ConnectionDef.IdSize);
    end;
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

procedure TInstantNexusDBConnectionDefEditForm.SaveData(ConnectionDef:
    TInstantNexusDBConnectionDef);
begin
  with ConnectionDef do
  begin
    ProtocolType := Self.ProtocolType;
    ServerName := Self.ServerName;
    Port := Self.Port;
    Alias := Self.Alias;
    AliasIsPath := Self.AliasIsPath;

    ConnectionDef.BlobStreamFormat :=
      TInstantStreamFormat(StreamFormatComboBox.ItemIndex);
    ConnectionDef.IdDataType := TInstantDataType(IdDataTypeComboBox.ItemIndex);
    ConnectionDef.IdSize := StrToInt(IdSizeEdit.Text);
  end;
end;

procedure TInstantNexusDBConnectionDefEditForm.LoadServersButtonClick(
  Sender: TObject);
begin
  ServersCbx.Clear;
  ServersCbx.Text := SLoadingServers;
  ServersCbx.Refresh;

  LoadServers(ServersCbx.Items);

  AliasesCbx.Clear;
  AliasesCbx.Text := SNone;
  AliasesCbx.Refresh;

  ServersCbx.Enabled := True;
  if (ServerName <> '') then
    ServersCbx.ItemIndex := ServersCbx.Items.IndexOf(ServerName);

  if (ServersCbx.Items.Count > 0) and (ServersCbx.ItemIndex < 0) then
    ServersCbx.Text := SSelectServerFromList
  else
  begin
    ServersCbx.Text := SNone;
    ServersCbx.Enabled := False;
  end;
end;

procedure TInstantNexusDBConnectionDefEditForm.LoadAliasesButtonClick(
  Sender: TObject);
begin
  AliasesCbx.Clear;
  AliasesCbx.Text := SLoadingAliases;
  AliasesCbx.Refresh;

  LoadAliases(AliasesCbx.Items);

  if (Alias <> '') then
    AliasesCbx.ItemIndex := AliasesCbx.Items.IndexOf(Alias);

  if (AliasesCbx.Items.Count > 0) and (AliasesCbx.ItemIndex < 0) then
    AliasesCbx.Text := SSelectAliasFromList
  else
    AliasesCbx.Text := SNone;
end;

procedure TInstantNexusDBConnectionDefEditForm.BrowseButtonClick(
  Sender: TObject);
var
  AliasDir: string;
begin
  AliasDir := Alias;
  if SelectDirectory(SSelectAnAliasPath, '', AliasDir) then
    Alias := AliasDir;
end;

procedure TInstantNexusDBConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  LoadServersButton.Hint := SLoadAvailableNexusDBServers;
  LoadAliasesButton.Hint := SLoadAvailableAliases;
  BrowseButton.Hint := SSelectAnAliasPath;

  AssignInstantStreamFormat(StreamFormatComboBox.Items);
  AssignInstantDataTypeStrings(IdDataTypeComboBox.Items);
  IdDataTypeComboBox.ItemIndex := Ord(dtString);
  IdSizeEdit.Text := IntToStr(InstantDefaultFieldSize);

  {$IFDEF NX1}
  TransportTypeRadGrp.Items.Delete(Pred(TransportTypeRadGrp.Items.Count));
  {$ENDIF}
end;

end.

