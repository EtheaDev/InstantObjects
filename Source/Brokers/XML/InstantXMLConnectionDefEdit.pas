(*
 *   InstantObjects
 *   XML Connection Editor
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

unit InstantXMLConnectionDefEdit;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, InstantXML,
  Forms, StdCtrls, Controls, ExtCtrls;

type
  TInstantXMLConnectionDefEditForm = class(TForm)
    BottomBevel: TBevel;
    BottomPanel: TPanel;
    ClientPanel: TPanel;
    RootDirEdit: TEdit;
    FolderButton: TButton;
    XMLLabel: TLabel;
    ButtonsPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    EncodingComboBox: TComboBox;
    Label1: TLabel;
    procedure FolderButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InitXMLEncoding;
  public
    procedure LoadData(ConnectionDef: TInstantXMLConnectionDef);
    procedure SaveData(ConnectionDef: TInstantXMLConnectionDef);
  end;

implementation

{$R *.dfm}

uses
  TypInfo,
  FileCtrl;

{ TInstantXMLConnectionDefEditForm }

procedure TInstantXMLConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantXMLConnectionDef);
begin
  RootDirEdit.Text := ConnectionDef.RootFolder;
  EncodingComboBox.ItemIndex := Ord(ConnectionDef.XMLFileFormat);
end;

procedure TInstantXMLConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantXMLConnectionDef);
begin
  ConnectionDef.RootFolder := RootDirEdit.Text;
  ConnectionDef.XMLFileFormat := TXMLFileFormat(EncodingComboBox.ItemIndex);
end;

procedure TInstantXMLConnectionDefEditForm.FolderButtonClick(
  Sender: TObject);
var
  Dir: string;
begin
  Dir := RootDirEdit.Text;
  if SelectDirectory(XMLLabel.Caption, '', Dir) then
    RootDirEdit.Text := Dir;
end;

procedure TInstantXMLConnectionDefEditForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  BorderStyle := bsDialog;
  InitXMLEncoding;
end;

procedure TInstantXMLConnectionDefEditForm.InitXMLEncoding;
var
  I: TXMLFileFormat;
begin
  for I := Low(TXMLFileFormat) to High(TXMLFileFormat) do
    EncodingComboBox.Items.Add(GetEnumName(TypeInfo(TXMLFileFormat), Ord(I)));
end;

end.
