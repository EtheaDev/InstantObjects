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
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBXConnectionDefEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, InstantDBX, DBXpress, DB, Grids, ValEdit, SQLExpr;

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
    ParamsEditor: TValueListEditor;
    ParamsLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure DriverNameEditChange(Sender: TObject);
    procedure ConnectionNameListBoxClick(Sender: TObject);
  private
    FConnection: TSQLConnection;
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
    procedure LoadData(ConnectionDef: TInstantDBXConnectionDef);
    procedure SaveData(ConnectionDef: TInstantDBXConnectionDef);
    property ConnectionName: string read GetConnectionName write SetConnectionName;
    property DriverName: string read GetDriverName write SetDriverName;
    property Params: TStrings read GetParams write SetParams;
  end;

implementation

{$R *.DFM}

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

procedure TInstantDBXConnectionDefEditForm.DriverNameEditChange(
  Sender: TObject);
begin
  UpdateConnectionNames;
end;

procedure TInstantDBXConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  UpdateDriverNames;
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
  Result := ParamsEditor.Strings;
end;

procedure TInstantDBXConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantDBXConnectionDef);
begin
  DriverName := ConnectionDef.DriverName;
  ConnectionName := ConnectionDef.ConnectionName;
  Params := ConnectionDef.Params;
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
  ConnectionDef.Params := Params;
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
  ParamsEditor.Strings := Value;
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
begin
  Connection.ConnectionName := ConnectionName;
  Connection.LoadParamsFromIniFile;
  ParamsEditor.Strings := Connection.Params;
end;

end.

