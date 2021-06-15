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

{$I '..\..\InstantDefines.inc'}

interface

uses
  Forms, Dialogs, StdCtrls, Controls, ExtCtrls, SysUtils, Classes,
  InstantFireDAC, FireDAC.Stan.Option,
  FireDAC.UI.Intf, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, FireDAC.Stan.Intf;

type
  TInstantFireDACConnectionDefEditForm = class(TForm)
    ClientPanel                : TPanel;
    HostNameLabel              : TLabel;
    PortLabel                  : TLabel;
    DatabaseLabel              : TLabel;
    UserNameLabel              : TLabel;
    PasswordLabel              : TLabel;
    PropertiesLabel            : TLabel;
    StreamFormatLabel          : TLabel;
    IdDataTypeLabel            : TLabel;
    IdSizeLabel                : TLabel;
    HostNameEdit               : TEdit;
    PortEdit                   : TEdit;
    DatabaseEdit               : TEdit;
    DatabaseButton             : TButton;
    UserNameEdit               : TEdit;
    PasswordEdit               : TEdit;
    LoginPromptCheckBox        : TCheckBox;
    AdditionalParamsEditor     : TMemo;
    StreamFormatComboBox       : TComboBox;
    IdDataTypeComboBox         : TComboBox;
    UseDelimitedIdentsCheckBox : TCheckBox;
    BottomBevel                : TBevel;
    BottomPanel                : TPanel;
    OkButton                   : TButton;
    CancelButton               : TButton;
    OSAuthCheckBox: TCheckBox;
    TestButton: TButton;
    DriverIdLabel: TLabel;
    DriverIdComboBox: TComboBox;
    UseUnicodeCheckBox: TCheckBox;
    cbIsolation: TComboBox;
    DefaultStatementCacheEdit: TEdit;
    Label1: TLabel;
    WithNoLockCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure DatabaseButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOwnerConnectionDef: TInstantFireDACConnectionDef;
  public
    constructor CreateForConnectionDef(AOwner: TComponent;
      AConnectionDef: TInstantFireDACConnectionDef);
    procedure LoadData(ConnectionDef: TInstantFireDACConnectionDef);
    procedure SaveData(ConnectionDef: TInstantFireDACConnectionDef);
    procedure TestConnection(ConnectionDef: TInstantFireDACConnectionDef);
  end;

implementation

{$R *.dfm}

uses
  InstantPersistence, InstantClasses, InstantTypes, InstantConsts;

{ TInstantFireDACConnectionDefEditForm }

procedure TInstantFireDACConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  AssignInstantStreamFormat(StreamFormatComboBox.Items);
  AssignInstantDataTypeStrings(IdDataTypeComboBox.Items);
  AssignFireDACDriverIds(DriverIdComboBox.Items);
  AssignFireDACIsolation(cbIsolation.Items);
end;

procedure TInstantFireDACConnectionDefEditForm.FormShow(Sender: TObject);
begin
  TestButton.Visible := Assigned(FOwnerConnectionDef);
end;

constructor TInstantFireDACConnectionDefEditForm.CreateForConnectionDef(
  AOwner: TComponent; AConnectionDef: TInstantFireDACConnectionDef);
begin
  inherited Create(AOwner);
  FOwnerConnectionDef := AConnectionDef;
end;

procedure TInstantFireDACConnectionDefEditForm.DatabaseButtonClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter :=
      'Firebird Database (*.fdb)|*.fdb|'+
      'Interbase Database (*.gdb)|*.gdb';
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
  DriverIdComboBox.ItemIndex         := DriverIdComboBox.Items.IndexOf(ConnectionDef.DriverId);
  DatabaseEdit.Text                  := ConnectionDef.Database;
  UserNameEdit.Text                  := ConnectionDef.User_Name;
  PasswordEdit.Text                  := ConnectionDef.Password;
  LoginPromptCheckBox.Checked        := ConnectionDef.LoginPrompt;
  OSAuthCheckBox.Checked             := ConnectionDef.OSAuthent;
  AdditionalParamsEditor.Lines.Text  := ConnectionDef.AdditionalParams;
  StreamFormatComboBox.ItemIndex     := Ord(ConnectionDef.BlobStreamFormat);
  IdDataTypeComboBox.ItemIndex       := Ord(ConnectionDef.IdDataType);
  cbIsolation.ItemIndex              := Ord(ConnectionDef.Isolation);
  UseDelimitedIdentsCheckBox.Checked := ConnectionDef.UseDelimitedIdents;
  UseUnicodeCheckBox.Checked         := ConnectionDef.UseUnicode;
  DefaultStatementCacheEdit.Text     := IntToStr(ConnectionDef.DefaultStatementCacheCapacity);
  WithNoLockCheckBox.Checked         := ConnectionDef.ReadObjectListWithNoLock;
end;

procedure TInstantFireDACConnectionDefEditForm.SaveData(ConnectionDef: TInstantFireDACConnectionDef);
begin
  ConnectionDef.HostName           := HostNameEdit.Text;
  ConnectionDef.Port               := StrToIntDef(PortEdit.Text,0);
  ConnectionDef.DriverId           := DriverIdComboBox.Text;
  ConnectionDef.Database           := DatabaseEdit.Text;
  ConnectionDef.User_Name          := UserNameEdit.Text;
  ConnectionDef.Password           := PasswordEdit.Text;
  ConnectionDef.LoginPrompt        := LoginPromptCheckBox.Checked;
  ConnectionDef.OSAuthent          := OSAuthCheckBox.Checked;
  ConnectionDef.BlobStreamFormat   := TInstantStreamFormat(StreamFormatComboBox.ItemIndex);
  ConnectionDef.IdDataType         := TInstantDataType(IdDataTypeComboBox.ItemIndex);
  ConnectionDef.Isolation          := TFDTxIsolation(cbIsolation.ItemIndex);
  ConnectionDef.UseDelimitedIdents := UseDelimitedIdentsCheckBox.Checked;
  ConnectionDef.UseUnicode         := UseUnicodeCheckBox.Checked;
  ConnectionDef.AdditionalParams   := AdditionalParamsEditor.Lines.Text;
  ConnectionDef.DefaultStatementCacheCapacity := StrToIntDef(DefaultStatementCacheEdit.Text,0);
  ConnectionDef.ReadObjectListWithNoLock := WithNoLockCheckBox.Checked;
end;

procedure TInstantFireDACConnectionDefEditForm.TestButtonClick(Sender: TObject);
begin
  if Assigned(FOwnerConnectionDef) then
  begin
    SaveData(FOwnerConnectionDef);
    Screen.Cursor := crHourGlass;
    try
      FOwnerConnectionDef.TestConnection;
    finally
      Screen.Cursor := crDefault;
    end;
    ShowMessage(SConnectionSuccess);
  end;
end;

procedure TInstantFireDACConnectionDefEditForm.TestConnection(
  ConnectionDef: TInstantFireDACConnectionDef);
var
  LConnector: TInstantConnector;
begin
  SaveData(ConnectionDef);
  LConnector := ConnectionDef.CreateConnector(nil);
  try
    LConnector.Connect;
    ShowMessage(SConnectionSuccess);
  finally
    LConnector.Free;
  end;
end;

end.
