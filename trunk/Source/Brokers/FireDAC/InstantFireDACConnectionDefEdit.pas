(*
 *   InstantObjects
 *   Embarcadero FireDAC Support
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
 * The Original Code is: InstantObjects FireDAC Support
 *
 * The Initial Developer of the Original Code is: David Taylor
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Carlo Barazzetta
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantFireDACConnectionDefEdit;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Forms, Dialogs, StdCtrls, Controls, ExtCtrls, SysUtils, Classes,
  InstantFireDAC,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, FireDAC.Stan.Intf;

type
  TInstantFireDACConnectionDefEditForm = class(TForm)
    ClientPanel                : TPanel;
    HostNameLabel              : TLabel;
    PortLabel                  : TLabel;
    ProtocolLabel              : TLabel;
    DatabaseLabel              : TLabel;
    CatalogLabel               : TLabel;
    UserNameLabel              : TLabel;
    PasswordLabel              : TLabel;
    PropertiesLabel            : TLabel;
    StreamFormatLabel          : TLabel;
    IdDataTypeLabel            : TLabel;
    IdSizeLabel                : TLabel;
    HostNameEdit               : TEdit;
    PortEdit                   : TEdit;
    ProtocolComboBox           : TComboBox;
    DatabaseEdit               : TEdit;
    CatalogComboBox            : TComboBox;
    DatabaseButton             : TButton;
    UserNameEdit               : TEdit;
    PasswordEdit               : TEdit;
    LoginPromptCheckBox        : TCheckBox;
    PropertiesEditor           : TMemo;
    StreamFormatComboBox       : TComboBox;
    IdDataTypeComboBox         : TComboBox;
    IdSizeEdit                 : TEdit;
    UseDelimitedIdentsCheckBox : TCheckBox;
    BottomBevel                : TBevel;
    BottomPanel                : TPanel;
    OkButton                   : TButton;
    CancelButton               : TButton;
    procedure FormCreate(Sender: TObject);
    procedure PortEditExit(Sender: TObject);
    procedure DatabaseButtonClick(Sender: TObject);
  private
    procedure UpdateControls;
  public
    procedure LoadData(ConnectionDef: TInstantFireDACConnectionDef);
    procedure SaveData(ConnectionDef: TInstantFireDACConnectionDef);
  end;

implementation

{$R *.dfm}

uses
  InstantPersistence, InstantClasses, InstantTypes;

{ TInstantFireDACConnectionDefEditForm }

procedure TInstantFireDACConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  AssignInstantStreamFormat(StreamFormatComboBox.Items);
  AssignInstantDataTypeStrings(IdDataTypeComboBox.Items);
  AssignFireDACProtocols(ProtocolComboBox.Items);
  UpdateControls;
end;

procedure TInstantFireDACConnectionDefEditForm.PortEditExit(Sender: TObject);
begin
  if Length(PortEdit.Text) > 0 then
    PortEdit.Text := IntToStr(StrToInt(PortEdit.Text));
end;

procedure TInstantFireDACConnectionDefEditForm.DatabaseButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := 'All Files (*.*)|*.*';
    if Execute then
      DatabaseEdit.Text := FileName;
  finally
    Free;
  end;
end;

procedure TInstantFireDACConnectionDefEditForm.LoadData(ConnectionDef: TInstantFireDACConnectionDef);
begin
  HostNameEdit.Text                  := ConnectionDef.HostName;
  PortEdit.Text                      := InttoStr(ConnectionDef.Port);
  ProtocolComboBox.ItemIndex         := ProtocolComboBox.Items.IndexOf(ConnectionDef.Protocol);
  DatabaseEdit.Text                  := ConnectionDef.Database;
  CatalogComboBox.Text               := ConnectionDef.Catalog;
  UserNameEdit.Text                  := ConnectionDef.UserName;
  PasswordEdit.Text                  := ConnectionDef.Password;
  LoginPromptCheckBox.Checked        := ConnectionDef.LoginPrompt;
  PropertiesEditor.Lines.Text        := ConnectionDef.Properties;
  StreamFormatComboBox.ItemIndex     := Ord(ConnectionDef.BlobStreamFormat);
  IdDataTypeComboBox.ItemIndex       := Ord(ConnectionDef.IdDataType);
  IdSizeEdit.Text                    := InttoStr(ConnectionDef.IdSize);
  UseDelimitedIdentsCheckBox.Checked := ConnectionDef.UseDelimitedIdents;
  UpdateControls;
end;

procedure TInstantFireDACConnectionDefEditForm.SaveData(ConnectionDef: TInstantFireDACConnectionDef);
begin
  ConnectionDef.HostName           := HostNameEdit.Text;
  ConnectionDef.Port               := StrToIntDef(PortEdit.Text,0);
  ConnectionDef.Protocol           := ProtocolComboBox.Text;
  ConnectionDef.Database           := DatabaseEdit.Text;
  ConnectionDef.Catalog            := CatalogComboBox.Text;
  ConnectionDef.UserName           := UserNameEdit.Text;
  ConnectionDef.Password           := PasswordEdit.Text;
  ConnectionDef.LoginPrompt        := LoginPromptCheckBox.Checked;
  ConnectionDef.Properties         := PropertiesEditor.Lines.Text;
  ConnectionDef.BlobStreamFormat   := TInstantStreamFormat(StreamFormatComboBox.ItemIndex);
  ConnectionDef.IdDataType         := TInstantDataType(IdDataTypeComboBox.ItemIndex);
  ConnectionDef.IdSize             := StrToInt(IdSizeEdit.Text);
  ConnectionDef.UseDelimitedIdents := UseDelimitedIdentsCheckBox.Checked;
end;

procedure TInstantFireDACConnectionDefEditForm.UpdateControls;
begin
end;

end.
