(*
 *   InstantObjects(tm) NexusDb Broker Support - ConnectionDefEdit
 *
 *   Copyright (c) Seleqt
 *   Copyright (c) Carlo Wolter - cwolter@tecnimex.it
 *

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations under the License.

The Original Code is InstantObject NexusDb Broker.

The Initial Developer of the Original Code is Carlo Wolter.
Portions created by Seleqt are Copyright (C) Seleqt.
All Rights Reserved.

Contributor(s): None at the moment.

=====================================================================
 Limited warranty and disclaimer of warranty
=====================================================================
This software and accompanying written materials are provided
"as is" without warranty of any kind. Further, the author does
not warrant, guarantee, or take any representations regarding
the use, or the results of use, of the software or written
materials in terms of correctness, accuracy, reliability,
currentness or otherwise. The entire risk as to the results
and performance of the software is assumed by you.
Neither the author nor anyone else who has been involved in
the creation, production or delivery of this product shall be
liable for any direct, indirect, consequential or incidental
damages (including damages for loss of business profits, business
interruption, loss of business information and the like) arising
out of the use or inability to use the product even if the author
has been advised of the possibility of such damages.
By using the InstantObject NexusDb Broker component you acknowledge
that you have read this limited warranty, understand it,
and agree to be bound by its' terms and conditions.
=====================================================================

 * Contributor(s):
 * Carlo Barazzetta: blob streaming in XML format (Part, Parts, References)
 *
 *)

unit InstantNexusDbConnectionDefEdit;

interface

{$IFNDEF VER130}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

uses Windows, Forms, StdCtrls, Controls, ExtCtrls, Classes, InstantNexusDb, nxllComponent, nxdb
/// , CSIntf
    ;

type
  TInstantNexusDbConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    ClientPanel: TPanel;
    OkButton: TButton;
    PathLabel: TLabel;
    BrowseButton: TButton;
    AliasLabel: TLabel;
    lbAlias: TListBox;
    rgSelDb: TRadioGroup;
    ePath: TEdit;
    ServerComboBox: TComboBox;
    ServerLabel: TLabel;
    StreamFormatLabel: TLabel;
    StreamFormatComboBox: TComboBox;
    procedure BrowseButtonClick(Sender: TObject);
    procedure ServerComboBoxSelect(Sender: TObject);
    procedure rgSelDbClick(Sender: TObject);
    procedure ServerComboBoxLoadAlias(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ServerComboBoxDropDown(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FConnectionDef : TInstantNexusDbConnectionDef;
    procedure UpdateControls;
  protected
    procedure LoadAliasNames;
  public
    procedure LoadData(ConnectionDef: TInstantNexusDbConnectionDef);
    procedure SaveData(ConnectionDef: TInstantNexusDbConnectionDef);
  end;

implementation

uses FileCtrl, InstantClasses, InstantPersistence;

{$R *.DFM}

{ TInstantNexusDbConnectionDefEditForm }

procedure TInstantNexusDbConnectionDefEditForm.BrowseButtonClick(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('Database Directory', '', Dir) then ePath.Text := Dir;
end;

procedure TInstantNexusDbConnectionDefEditForm.LoadAliasNames;
var
  OldAliasName : string;
begin
  Screen.Cursor := crHourGlass;
  if lbAlias.ItemIndex >= 0 then
    OldAliasName := lbAlias.Items.Strings[lbAlias.ItemIndex];
  Try
    FConnectionDef.ServerName := ServerComboBox.Text;
    FConnectionDef.LoadAliasList(lbAlias.Items);
  Finally
    if OldAliasName <> '' then
      lbAlias.ItemIndex := lbAlias.Items.IndexOf(OldAliasName);
    Screen.Cursor := crDefault;
  End;
end;

procedure TInstantNexusDbConnectionDefEditForm.LoadData(ConnectionDef: TInstantNexusDbConnectionDef);
begin
///  CodeSite.SendMsg('FORM load data');
  FConnectionDef := ConnectionDef;
  if ConnectionDef.AliasIsPath then
  begin
    rgSelDb.ItemIndex := 1;
    ePath.Text := ConnectionDef.AliasName;
  end
  else
  begin
    rgSelDb.ItemIndex := 0;
    ServerComboBox.Text := ConnectionDef.ServerName;
    LoadAliasNames;
    lbAlias.ItemIndex := lbAlias.Items.IndexOf(ConnectionDef.AliasName);
  end;
  StreamFormatComboBox.ItemIndex := Ord(ConnectionDef.BlobStreamFormat); //CB
  UpdateControls;
end;

procedure TInstantNexusDbConnectionDefEditForm.SaveData(ConnectionDef: TInstantNexusDbConnectionDef);
begin
///  CodeSite.SendMsg('FORM save data');
  case rgSelDb.ItemIndex of
    0:  begin
          ConnectionDef.ServerName := ServerComboBox.Text;
          if lbAlias.ItemIndex >= 0 then
            ConnectionDef.AliasName   := lbAlias.Items.Strings[lbAlias.ItemIndex]
          else
            ConnectionDef.AliasName := '';
          ConnectionDef.AliasIsPath := False;       // True Alias
        end;
    1:  begin
          ConnectionDef.AliasName   := ePath.Text;
          ConnectionDef.AliasIsPath := True;        // Path
        end;
  end;
  ConnectionDef.BlobStreamFormat := TInstantStreamFormat(StreamFormatComboBox.ItemIndex); //CB
///  CodeSite.SendBoolean('Alias '+ConnectionDef.AliasName,ConnectionDef.AliasIsPath);
end;

procedure TInstantNexusDbConnectionDefEditForm.ServerComboBoxSelect(
  Sender: TObject);
begin
  LoadAliasNames;
end;

procedure TInstantNexusDbConnectionDefEditForm.rgSelDbClick(
  Sender: TObject);
begin
  UpdateControls;
end;

procedure TInstantNexusDbConnectionDefEditForm.UpdateControls;
begin
  PathLabel.Visible := rgSelDb.ItemIndex = 1;
  ePath.Visible := rgSelDb.ItemIndex = 1;
  BrowseButton.Visible := rgSelDb.ItemIndex = 1;
  AliasLabel.Visible := rgSelDb.ItemIndex = 0;
  lbAlias.Visible := rgSelDb.ItemIndex = 0;
  ServerComboBox.Visible := rgSelDb.ItemIndex = 0;
  ServerLabel.Visible := rgSelDb.ItemIndex = 0;
end;

procedure TInstantNexusDbConnectionDefEditForm.ServerComboBoxLoadAlias(
  Sender: TObject);
begin
  LoadAliasNames;
end;

procedure TInstantNexusDbConnectionDefEditForm.FormShow(Sender: TObject);
begin
  rgSelDb.SetFocus;
end;

procedure TInstantNexusDbConnectionDefEditForm.ServerComboBoxDropDown(
  Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Try
    FConnectionDef.LoadServerList(ServerComboBox.Items);
  Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure TInstantNexusDbConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  AssignInstantStreamFormat(StreamFormatComboBox.Items); //CB
end;

end.
