(*
 *   InstantObjects
 *   JSON Connection Editor
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
 * The Original Code is: Carlo Barazzetta
 *
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Contributor(s):
 * Adrea Petrelli, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantJSONConnectionDefEdit;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

{$IFDEF D6+}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

interface

uses
  System.Classes, InstantJSON,
  Forms
  , Vcl.StdCtrls
  , Vcl.Controls
  , Vcl.ExtCtrls;

type
  TInstantJSONConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    ClientPanel: TPanel;
    RootDirEdit: TEdit;
    FolderButton: TButton;
    JSONLabel: TLabel;
    ButtonsPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    EncodingComboBox: TComboBox;
    Label1: TLabel;
    procedure FolderButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InitJSONEncoding;
  public
    procedure LoadData(ConnectionDef: TInstantJSONConnectionDef);
    procedure SaveData(ConnectionDef: TInstantJSONConnectionDef);
  end;

implementation

{$R *.dfm}

uses
  TypInfo,
  FileCtrl;

{ TInstantJSONConnectionDefEditForm }

procedure TInstantJSONConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantJSONConnectionDef);
begin
  RootDirEdit.Text := ConnectionDef.RootFolder;
  EncodingComboBox.ItemIndex := Ord(ConnectionDef.JSONFileFormat);
end;

procedure TInstantJSONConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantJSONConnectionDef);
begin
  ConnectionDef.RootFolder := RootDirEdit.Text;
  ConnectionDef.JSONFileFormat := TJSONFileFormat(EncodingComboBox.ItemIndex);
end;

procedure TInstantJSONConnectionDefEditForm.FolderButtonClick(
  Sender: TObject);
var
  Dir: string;
begin
  Dir := RootDirEdit.Text;
  if SelectDirectory(JSONLabel.Caption, '', Dir) then
    RootDirEdit.Text := Dir;
end;

procedure TInstantJSONConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  BorderStyle := bsDialog;
  InitJSONEncoding;
end;

procedure TInstantJSONConnectionDefEditForm.InitJSONEncoding;
var
  I: TJSONFileFormat;
begin
  for I := Low(TJSONFileFormat) to High(TJSONFileFormat) do
    EncodingComboBox.Items.Add(GetEnumName(TypeInfo(TJSONFileFormat), Ord(I)));
end;

end.
