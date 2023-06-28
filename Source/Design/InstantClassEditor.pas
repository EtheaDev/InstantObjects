(*
 *   InstantObjects
 *   Class Editor
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
 * Carlo Barazzetta, Adrea Petrelli, Steven Mitchell, Nando Dessena,
 * Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantClassEditor;

{$I '..\InstantDefines.inc'}

interface

uses
  SysUtils, Classes, DB, Contnrs, InstantPresentation,
  InstantPersistence, InstantCode, InstantEdit,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Mask, DBCtrls,
  ImgList, ActnList, Menus, InstantAttributeView;

type
  TInstantClassEditorForm = class(TInstantEditForm)
    PageControl: TPageControl;
    ClassSheet: TTabSheet;
    ClassNameLabel: TLabel;
    BaseClassLabel: TLabel;
    UnitLabel: TLabel;
    ClassNameEdit: TDBEdit;
    BaseClassEdit: TDBComboBox;
    UnitEdit: TDBComboBox;
    StorageEdit: TDBEdit;
    AttributeSheet: TTabSheet;
    StorageLabel: TLabel;
    PersistenceComboBox: TDBComboBox;
    PersistenceLabel: TLabel;
    InstantAttributeViewFrame: TInstantAttributeViewFrame;
    procedure FormCreate(Sender: TObject);
    procedure ClassNameEditChange(Sender: TObject);
    procedure SubjectExposerAfterPostField(Sender: TObject; Field: TField);
    procedure PersistenceComboBoxChange(Sender: TObject);
    procedure SubjectExposerTranslate(Sender: TObject; Field: TField;
      var Value: Variant; Write: Boolean);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ClassSheetResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FModel: TInstantCodeModel;
    FTitle: string;
    FIsNew: Boolean;
    function GetSubject: TInstantCodeClass;
    procedure SetModel(const Value: TInstantCodeModel);
    procedure SetSubject(const Value: TInstantCodeClass);
    procedure SetIsNew(const Value: Boolean);
    function GetChangedAttributes: TStringList;
    function GetNewAttributes: TList;
  protected
    function EditAttribute(Attribute: TInstantCodeAttribute;
      Exists: Boolean; const Title: string = ''): Boolean;
    procedure PopulateBaseClasses;
    procedure PopulateUnits;
    procedure UpdateActions; override;
    procedure UpdateCaption;
    procedure UpdateControls;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ChangedAttributes: TStringList read GetChangedAttributes;
    property IsNew: Boolean read FIsNew write SetIsNew;
    property Model: TInstantCodeModel read FModel write SetModel;
    property NewAttributes: TList read GetNewAttributes;
    property Subject: TInstantCodeClass read GetSubject write SetSubject;
  end;

implementation

uses
  InstantAttributeEditor, InstantDesignUtils, InstantConsts, InstantRtti,
  TypInfo, InstantImageUtils, InstantTypes;

{$R *.dfm}

resourcestring
  SConfirmDeleteAttribute = 'Delete attribute ''%s''?';

{ TInstantClassDesigner }

procedure TInstantClassEditorForm.CancelButtonClick(Sender: TObject);
begin
  inherited;
  Subject.AssignAttributes(InstantAttributeViewFrame.BackupAttributes);
end;

procedure TInstantClassEditorForm.ClassNameEditChange(Sender: TObject);
begin
  with ClassNameEdit do
    SubjectExposer.AssignFieldValue(Field, Text);
  UpdateCaption;
  UpdateControls;
end;

constructor TInstantClassEditorForm.Create(AOwner: TComponent);
begin
  inherited;
  InstantAttributeViewFrame.Subject := Subject;
end;

destructor TInstantClassEditorForm.Destroy;
begin
  inherited;
end;

function TInstantClassEditorForm.EditAttribute(
  Attribute: TInstantCodeAttribute; Exists: Boolean;
  const Title: string): Boolean;

  function GetClassStorageName: String;
  begin
    if Attribute.Metadata.ClassMetadata.StorageName <> '' then
      Result := Attribute.Metadata.ClassMetadata.StorageName
    else
      Result := Remove_T_FromClassName(Attribute.Metadata.ClassMetadata.Name);

  end;

begin
  with TInstantAttributeEditorForm.Create(nil) do
  try
    if Title <> '' then
      Caption := Title;
    Model := Self.Model;
    BaseClassStorageName := GetClassStorageName;
    Limited := Exists;
    Subject := Attribute;
    Result := ShowModal = mrOk;
    if Result then
      Attribute.Realize;
  finally
    Free;
  end;
end;

function TInstantClassEditorForm.GetChangedAttributes: TStringList;
begin
  Result := InstantAttributeViewFrame.ChangedAttributes;
end;

function TInstantClassEditorForm.GetNewAttributes: TList;
begin
  Result := InstantAttributeViewFrame.NewAttributes;
end;

function TInstantClassEditorForm.GetSubject: TInstantCodeClass;
begin
  Result := inherited Subject as TInstantCodeClass;
end;

procedure TInstantClassEditorForm.OkButtonClick(Sender: TObject);
var
  I: Integer;
begin
  with Model do
    for I := 0 to Pred(ClassCount) do
      if (Classes[I] <> Subject) and
        SameText(Classes[I].Name, Subject.Name) then
      begin
        ModalResult := mrNone;
        raise Exception.Create('Class name already used');
      end;
  inherited;
end;

procedure TInstantClassEditorForm.PersistenceComboBoxChange(
  Sender: TObject);
begin
  with PersistenceComboBox do
    SubjectExposer.AssignFieldValue(Field, Text);
  UpdateControls;
end;

procedure TInstantClassEditorForm.PopulateBaseClasses;
var
  I: Integer;
  AClass: TInstantCodeClass;
  AClassName: string;
  ClassList: TClassList;
begin
  with BaseClassEdit.Items do
  begin
    BeginUpdate;
    try
      Clear;
      Add(TInstantObject.ClassName);
      ClassList := TClassList.Create;
      try
        InstantGetClasses(ClassList, nil);
        for i := 0 to Pred(ClassList.Count) do
          Add(ClassList[i].ClassName);
      finally
        ClassList.Free;
      end;
      if Assigned(FModel) then
        for I := 0 to Pred(FModel.ClassCount) do
        begin
          AClass := FModel.Classes[I];
          if not AClass.DerivesFrom(Subject) then
          begin
            AClassName := AClass.Name;
            if (AClassName <> '') and (IndexOf(AClassName) = -1) then
              Add(AClassName);
          end;
        end;
      BaseClassEdit.ItemIndex :=
        BaseClassEdit.Items.IndexOf(TInstantObject.ClassName);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TInstantClassEditorForm.PopulateUnits;
var
  I: Integer;
begin
  with UnitEdit.Items do
  begin
    BeginUpdate;
    try
      Clear;
      if Assigned(FModel) then
        for I := 0 to Pred(FModel.ModuleCount) do
          Add(FModel.Modules[I].Name);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TInstantClassEditorForm.SetIsNew(const Value: Boolean);
begin
  if Value <> FIsNew then
  begin
    FIsNew := Value;
    UpdateControls;
  end;
end;

procedure TInstantClassEditorForm.SetModel(const Value: TInstantCodeModel);
begin
  if Value <> FModel then
  begin
    FModel := Value;
    InstantAttributeViewFrame.Model := Value;
    PopulateBaseClasses;
    PopulateUnits;
  end;
end;

procedure TInstantClassEditorForm.SetSubject(const Value: TInstantCodeClass);
begin
  if Value <> Subject then
  begin
    inherited Subject := Value;
    InstantAttributeViewFrame.Subject := Subject;
//    Subject.CloneAttributes(FBackupAttributes);
    Subject.CloneAttributes(InstantAttributeViewFrame.BackupAttributes);
    with PersistenceComboBox do
      ItemIndex := SubjectExposer.GetFieldStrings(Field, Items);
    PopulateBaseClasses;
    UpdateCaption;
    UpdateControls;
  end;
end;

procedure TInstantClassEditorForm.SubjectExposerAfterPostField(
  Sender: TObject; Field: TField);
begin
  if Field.FieldName = 'BaseClassName' then
  begin
//    FreeAndNil(FNameAttribute);
    InstantAttributeViewFrame.PopulateInheritedAttributes;
  end;
end;

procedure TInstantClassEditorForm.SubjectExposerTranslate(Sender: TObject;
  Field: TField; var Value: Variant; Write: Boolean);
var
  Name: string;
begin
  if Field.FieldName = 'Persistence' then
    if Write then
      Value := 'pe' + Value
    else begin
      Name := Value;
      Delete(Name, 1, 2);
      Value := Name;
    end;
end;

procedure TInstantClassEditorForm.UpdateActions;
begin
  inherited;
end;

procedure TInstantClassEditorForm.UpdateCaption;
begin
  if Assigned(Subject) and (ClassNameEdit.Text <> '') then
    Caption := FTitle + ' - ' + ClassNameEdit.Text
  else
    Caption := FTitle;
end;

procedure TInstantClassEditorForm.UpdateControls;
var
  Stored: Boolean;
begin
  OkButton.Enabled := ClassNameEdit.Text <> '';
  Stored := Assigned(Subject) and (Subject.MetadataInfo.Persistence = peStored);
  UnitEdit.Enabled := IsNew;
  EnableControl(StorageEdit, Stored, SubjectSource);
  EnableControl(StorageLabel, Stored, SubjectSource);
end;

procedure TInstantClassEditorForm.FormCreate(Sender: TObject);
begin
  inherited;
  FTitle := Caption;
  PageControl.ActivePage := ClassSheet;
  ActiveControl := ClassNameEdit;
  ClassSheetResize(nil);
end;

procedure TInstantClassEditorForm.FormShow(Sender: TObject);
begin
  inherited;
  if height < 600 then
    height := 600;
end;

procedure TInstantClassEditorForm.ClassSheetResize(Sender: TObject);
begin
  inherited;
  //simulate right-anchor (for Kylix compatibility)
  ClassNameEdit.Width := ClassSheet.Width - 36;
  BaseClassEdit.Width := ClassSheet.Width - 36;
  UnitEdit.Width := ClassSheet.Width - 36;
  StorageEdit.Width := ClassSheet.Width - 163;
end;

end.


