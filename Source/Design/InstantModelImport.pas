(*
 *   InstantObjects
 *   Import Model
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
 * Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

 unit InstantModelImport;

interface

uses
  WinApi.Windows
  , WinApi.Messages
  , System.SysUtils
  , System.Variants
  , System.Classes
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , InstantDialog
  , Vcl.ExtCtrls
  , Vcl.StdCtrls
  , InstantCode
  , InstantClasses
  ;

type
  TInstantModelImportForm = class(TInstantDialogForm)
    ImportButton: TButton;
    ImportModuleCombo: TComboBox;
    Label1: TLabel;
    FileNameEdit: TEdit;
    FileNameButton: TButton;
    Label2: TLabel;
    CancelButton: TButton;
    OpenDialog: TOpenDialog;
    procedure FileNameButtonClick(Sender: TObject);
    procedure ImportModuleComboChange(Sender: TObject);
    procedure FileNameEditChange(Sender: TObject);
  private
    FModel: TInstantCodeModel;
    FSelectedModule: TInstantCodeModule;
    FSelectedFileName: string;
    procedure LoadModules;
    procedure UpdateControls;
    function GetSelectedFileType: TInstantStreamFormat;
  public
    function Execute(AModel: TInstantCodeModel): Boolean;

    property Model: TInstantCodeModel read FModel;

    property SelectedModule: TInstantCodeModule read FSelectedModule;
    property SelectedFileName: string read FSelectedFileName;
    property SelectedFileType: TInstantStreamFormat read GetSelectedFileType;
  end;

var
  InstantModelImportForm: TInstantModelImportForm;

implementation

{$R *.dfm}

{ TInstantImportModelForm }

function TInstantModelImportForm.Execute(AModel: TInstantCodeModel): Boolean;
begin
  FModel := AModel;

  LoadModules;
  UpdateControls;

  Result := ShowModal = mrOK;

  if Result then
  begin
    with ImportModuleCombo do
      FSelectedModule := Items.Objects[ItemIndex] as TInstantCodeModule;
    FSelectedFileName := FileNameEdit.Text;
  end else
  begin
    FSelectedModule := nil;
    FSelectedFileName := '';
  end;
end;

procedure TInstantModelImportForm.FileNameButtonClick(Sender: TObject);
begin
  inherited;

  OpenDialog.FileName := FileNameEdit.Text;
  if OpenDialog.Execute then
    FileNameEdit.Text := OpenDialog.FileName;
end;

procedure TInstantModelImportForm.LoadModules;
var
  I: Integer;
  Module: TInstantCodeModule;
begin
  ImportModuleCombo.Clear;
  for I := 0 to FModel.ModuleCount - 1 do
  begin
    Module := FModel.Modules[I];
    ImportModuleCombo.Items.AddObject(Module.PascalUnitName, Module)
  end;
end;

procedure TInstantModelImportForm.UpdateControls;
begin
  ImportButton.Enabled := (FileNameEdit.Text <> '') and (ImportModuleCombo.ItemIndex <> -1);
end;

function TInstantModelImportForm.GetSelectedFileType: TInstantStreamFormat;
begin
  if CompareText(ExtractFileExt(SelectedFileName), '.mdr') = 0 then
    Result := sfBinary else
    Result := sfXML;
end;

procedure TInstantModelImportForm.ImportModuleComboChange(Sender: TObject);
begin
  inherited;
  UpdateControls;
end;

procedure TInstantModelImportForm.FileNameEditChange(Sender: TObject);
begin
  inherited;
  UpdateControls;
end;

end.
