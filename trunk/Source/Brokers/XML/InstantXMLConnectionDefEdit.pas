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
 * Carlo Barazzetta, Adrea Petrelli: porting Kylix
 * Carlo Barazzetta: blob streaming in XML format (Part, Parts, References)
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantXMLConnectionDefEdit;

interface

{$IFDEF VER130}{$DEFINE MSWINDOWS}{$ENDIF}

uses
  Classes, InstantXML,
{$IFDEF MSWINDOWS}
  Forms, StdCtrls, Controls, ExtCtrls;
{$ENDIF}
{$IFDEF LINUX}
  QForms, QStdCtrls, QControls, QExtCtrls, QDialogs;
{$ENDIF}

const
  AXMLFileFormatStr : Array[TXMLFileFormat] of string =
    ('xffUtf8', 'xffUtf8BOT', 'xffIso');

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
    cbVersioning: TCheckBox;
    cbEncoding: TComboBox;
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

{$IFDEF MSWINDOWS}
uses
  FileCtrl;
{$ENDIF}

{ TInstantXMLConnectionDefEditForm }

procedure TInstantXMLConnectionDefEditForm.LoadData(
  ConnectionDef: TInstantXMLConnectionDef);
begin
  { TODO: Copy data from ConnectionDef to edit controls }
  RootDirEdit.Text := ConnectionDef.RootFolder;
  cbEncoding.ItemIndex := Ord(ConnectionDef.XMLFileFormat);
  cbVersioning.Checked := ConnectionDef.UseVersioning;
end;

procedure TInstantXMLConnectionDefEditForm.SaveData(
  ConnectionDef: TInstantXMLConnectionDef);
begin
  { TODO: Copy data from edit controls to ConnectionDef }
  ConnectionDef.RootFolder := RootDirEdit.Text;
  ConnectionDef.XMLFileFormat := TXMLFileFormat(cbEncoding.ItemIndex);
  ConnectionDef.UseVersioning := cbVersioning.Checked;
end;

procedure TInstantXMLConnectionDefEditForm.FolderButtonClick(
  Sender: TObject);
var
{$IFDEF MSWINDOWS}
  Dir: string;
{$ENDIF}
{$IFDEF LINUX}
  Dir: WideString;
{$ENDIF}
begin
  Dir := RootDirEdit.Text;
  if SelectDirectory(XMLLabel.Caption, '', Dir) then
    RootDirEdit.Text := Dir;
end;

procedure TInstantXMLConnectionDefEditForm.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  BorderStyle := bsDialog;
{$ENDIF}
{$IFDEF LINUX}
  BorderStyle := fbsDialog;
{$ENDIF}
  InitXMLEncoding;
end;

procedure TInstantXMLConnectionDefEditForm.InitXMLEncoding;
var
  i : TXMLFileFormat;
begin
  for i := Low(TXMLFileFormat) to High(TXMLFileFormat) do
  begin
    cbEncoding.Items.Add(AXMLFileFormatStr[i]);
  end;
end;

end.
