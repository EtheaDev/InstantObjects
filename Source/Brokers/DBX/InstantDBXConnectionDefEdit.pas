(*
 *   InstantObjects
 *   dbExpress Connection Editor
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
 * Carlo Barazzetta, Adrea Petrelli: porting Kylix
 * Carlo Barazzetta: blob streaming in XML format (Part, Parts, References)
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBXConnectionDefEdit;

interface

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

uses
  Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.Controls
  , Vcl.ExtCtrls
  , System.SysUtils
  , System.Classes
  , InstantDBX
  , Data.DB
  , Data.SQLExpr
  ;

type
  TInstantDBXConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    ConnectionNameLabel: TLabel;
    ConnectionNameListBox: TListBox;
    DriverNameEdit: TComboBox;
    DriverNameLabel: TLabel;
    OkButton: TButton;
    ParamsLabel: TLabel;
    ParamsEditor: TMemo;
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    LoginPromptCheckBox: TCheckBox;
    UseUnicodeCheckBox: TCheckBox;
    TestButton: TButton;
    Label1: TLabel;
    DefaultStatementCacheEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure DriverNameEditChange(Sender: TObject);
    procedure ConnectionNameListBoxClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FConnection: TSQLConnection;
    FOwnerConnectionDef: TInstantDBXConnectionDef;
    function GetConnection: TSQLConnection;
    function GetConnectionName: string;
    function GetDriverName: string;
    function GetParams: TStrings;
    procedure SetConnectionName(const Value: string);
    procedure SetDriverName(const Value: string);
    procedure SetParams(const Value: TStrings);
    procedure UpdateConnectionNames;
    procedure UpdateDriverNames;
    procedure UpdateParams;
  protected
    property Connection: TSQLConnection read GetConnection;
  public
    constructor CreateForConnectionDef(AOwner: TComponent;
      AConnectionDef: TInstantDBXConnectionDef);
    procedure LoadData(ConnectionDef: TInstantDBXConnectionDef);
    procedure SaveData(ConnectionDef: TInstantDBXConnectionDef);
    property ConnectionName: string read GetConnectionName write SetConnectionName;
    property DriverName: string read GetDriverName write SetDriverName;
    property Params: TStrings read GetParams write SetParams;
  end;

implementation

{$R *.dfm}

uses
  InstantPersistence, InstantClasses, InstantConsts;
  
const
  SAllDrivers = '[All]';

type
  TSQLConnectionHack = class(TSQLConnection)
  end;

{ TInstantADOConnDefEditForm }

procedure TInstantDBXConnectionDefEditForm.ConnectionNameListBoxClick(
  Sender: TObject);
begin
  UpdateParams;
end;

constructor TInstantDBXConnectionDefEditForm.CreateForConnectionDef(
  AOwner: TComponent; AConnectionDef: TInstantDBXConnectionDef);
begin
  inherited Create(AOwner);
  FOwnerConnectionDef := AConnectionDef;
end;

procedure TInstantDBXConnectionDefEditForm.DriverNameEditChange(
  Sender: TObject);
begin
  UpdateConnectionNames;
end;

procedure TInstantDBXConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  AssignInstantStreamFormat(StreamFormatComboBox.Items); //CB
  BorderStyle := bsDialog;
  UpdateDriverNames;
end;

procedure TInstantDBXConnectionDefEditForm.FormShow(Sender: TObject);
begin
  TestButton.Visible := Assigned(FOwnerConnectionDef);
end;

function TInstantDBXConnectionDefEditForm.GetConnection: TSQLConnection;
begin
  if not Assigned(FConnection) then
  begin
    FConnection := TSQLConnection.Create(Self);
    TSQLConnectionHack(FConnection).SetDesigning(True);
  end;
  Result := FConnection;
end;

function TInstantDBXConnectionDefEditForm.GetConnectionName: string;
begin
  with ConnectionNameListBox do
    if ItemIndex >= 0 then
      Result := Items[ItemIndex]
    else
      Result := '';
end;

function TInstantDBXConnectionDefEditForm.GetDriverName: string;
begin
  Result := DriverNameEdit.Text;
  if SameText(Result, SAllDrivers) then
    Result := '';
end;

function TInstantDBXConnectionDefEditForm.GetParams: TStrings;
begin
  Result := ParamsEditor.Lines;
end;

procedure TInstantDBXConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantDBXConnectionDef);
begin
  DriverName := ConnectionDef.DriverName;
  ConnectionName := ConnectionDef.ConnectionName;
  Params.Text := ConnectionDef.Params;
  //CB
  StreamFormatComboBox.ItemIndex := Ord(ConnectionDef.BlobStreamFormat);
  LoginPromptCheckBox.Checked := ConnectionDef.LoginPrompt;
  UseUnicodeCheckBox.Checked := ConnectionDef.UseUnicode;
  DefaultStatementCacheEdit.Text := IntToStr(ConnectionDef.DefaultStatementCacheCapacity);
end;

procedure TInstantDBXConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantDBXConnectionDef);
begin
  Connection.ConnectionName := ConnectionName;
  ConnectionDef.ConnectionName := ConnectionName;
  ConnectionDef.DriverName := Connection.DriverName;
  ConnectionDef.LibraryName := Connection.LibraryName;
  ConnectionDef.VendorLib := Connection.VendorLib;
  ConnectionDef.GetDriverFunc := Connection.GetDriverFunc;
  ConnectionDef.Params := Params.Text;
  //CB
  ConnectionDef.BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex);
  ConnectionDef.LoginPrompt := LoginPromptCheckBox.Checked;
  ConnectionDef.UseUnicode := UseUnicodeCheckBox.Checked;
  ConnectionDef.DefaultStatementCacheCapacity := StrToIntDef(DefaultStatementCacheEdit.Text,0);
end;

procedure TInstantDBXConnectionDefEditForm.SetConnectionName(
  const Value: string);
begin
  if Value <> ConnectionName then
  begin
    with ConnectionNameListBox do
      ItemIndex := Items.IndexOf(Value);
    ConnectionNameListBoxClick(Self);
  end;
end;

procedure TInstantDBXConnectionDefEditForm.SetDriverName(
  const Value: string);
begin
  if Value <> DriverName then
  begin
    with DriverNameEdit do
      ItemIndex := Items.IndexOf(Value);
    DriverNameEditChange(Self);
  end;
end;

procedure TInstantDBXConnectionDefEditForm.SetParams(const Value: TStrings);
begin
  ParamsEditor.Lines := Value;
end;

procedure TInstantDBXConnectionDefEditForm.TestButtonClick(Sender: TObject);
begin
  if Assigned(FOwnerConnectionDef) then
  begin
    SaveData(FOwnerConnectionDef);
    FOwnerConnectionDef.TestConnection;
    ShowMessage(SConnectionSuccess);
  end;
end;

procedure TInstantDBXConnectionDefEditForm.UpdateConnectionNames;
begin
  with ConnectionNameListBox do
  begin
    Items.BeginUpdate;
    try
      GetConnectionNames(Items, DriverName);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TInstantDBXConnectionDefEditForm.UpdateDriverNames;
begin
  with DriverNameEdit do
  begin
    Items.BeginUpdate;
    try
      GetDriverNames(Items);
      Items.Insert(0, SAllDrivers);
      ItemIndex := 0;
      DriverNameEditChange(Self);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TInstantDBXConnectionDefEditForm.UpdateParams;
var
  i: integer;
begin
  Connection.ConnectionName := ConnectionName;
  Connection.LoadParamsFromIniFile;
  ParamsEditor.Lines.Clear;
  for i := 0 to Connection.Params.Count -1 do
    ParamsEditor.Lines.add (Connection.Params[i]);
end;

end.

