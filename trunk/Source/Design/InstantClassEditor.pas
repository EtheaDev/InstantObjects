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
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantClassEditor;

{$I ..\Core\InstantDefines.inc}

interface

uses
  SysUtils, Classes, DB, Contnrs, InstantPresentation,
  InstantPersistence, InstantCode, InstantEdit,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Mask, DBCtrls,
  ImgList, ActnList, Menus;
{$ENDIF}
{$IFDEF LINUX}
  QActnList, QMenus, QTypes, QImgList, QComCtrls, QControls, QExtCtrls,
  QStdCtrls, QDBCtrls, QMask, QForms;
{$ENDIF}

type
  TInstantClassEditorForm = class(TInstantEditForm)
    AttributeImages: TImageList;
    StateImages: TImageList;
    AttributesMenu: TPopupMenu;
    Actions: TActionList;
    ActionImages: TImageList;
    AttributeNewAction: TAction;
    AttributeDeleteAction: TAction;
    AttributeNewItem: TMenuItem;
    AttributeDeleteItem: TMenuItem;
    AttributeEditAction: TAction;
    AttributeEditItem: TMenuItem;
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
    AttributesSplitter: TSplitter;
    InheritedAttributesPanel: TPanel;
    InheritedAttributesLabel: TLabel;
    InheritedAttributesView: TListView;
    IntroducedAttributesPanel: TPanel;
    IntroducedAttributesView: TListView;
    IntroducedAttributesLabel: TLabel;
    StorageLabel: TLabel;
    PersistenceComboBox: TDBComboBox;
    PersistenceLabel: TLabel;
    procedure AttributeNewActionExecute(Sender: TObject);
    procedure AttributeDeleteActionExecute(Sender: TObject);
    procedure AttributeEditActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IntroducedAttributesViewDblClick(Sender: TObject);
    procedure IntroducedAttributesViewEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure ClassNameEditChange(Sender: TObject);
    procedure SubjectExposerAfterPostField(Sender: TObject; Field: TField);
    procedure PersistenceComboBoxChange(Sender: TObject);
    procedure SubjectExposerTranslate(Sender: TObject; Field: TField;
      var Value: Variant; Write: Boolean);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure AttributesMenuPopup(Sender: TObject);
    procedure ClassSheetResize(Sender: TObject);
  private
    FBackupAttributes: TObjectList;
    FChangedAttributes: TStringList;
    FNewAttributes: TList;
    FModel: TInstantCodeModel;
    FNameAttribute: TInstantCodeAttribute;
    FTitle: string;
    FIsNew: Boolean;
    procedure DeleteAttribute(Attribute: TInstantCodeAttribute);
    procedure FitColumns(View: TListView);
    function GetNameAttribute: TInstantCodeAttribute;
    function GetSubject: TInstantCodeClass;
    procedure LoadAttributeView(View: TListView; AClass: TInstantCodeClass;
      Recursive: Boolean);
    procedure SetModel(const Value: TInstantCodeModel);
    procedure SetSubject(const Value: TInstantCodeClass);
    function GetFocusedAttribute: TInstantCodeAttribute;
    procedure SetIsNew(const Value: Boolean);
  protected
    function AddAttributeToView(View: TListView;
      Attribute: TInstantCodeAttribute): TListItem;
    function EditAttribute(Attribute: TInstantCodeAttribute;
      Exists: Boolean; const Title: string = ''): Boolean;
    procedure PopulateInheritedAttributes;
    procedure PopulateIntroducedAttributes;
    procedure PopulateBaseClasses;
    procedure PopulateUnits;
    procedure UpdateActions; override;
    procedure UpdateCaption;
    procedure UpdateControls;
    property NameAttribute: TInstantCodeAttribute read GetNameAttribute;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ChangedAttributes: TStringList read FChangedAttributes;
    property FocusedAttribute: TInstantCodeAttribute read GetFocusedAttribute;
    property IsNew: Boolean read FIsNew write SetIsNew;
    property Model: TInstantCodeModel read FModel write SetModel;
    property NewAttributes: TList read FNewAttributes;
    property Subject: TInstantCodeClass read GetSubject write SetSubject;
  end;

implementation

uses
  InstantAttributeEditor, InstantDesignUtils, InstantConsts, InstantRtti,
  TypInfo, InstantImageUtils;

{$R *.dfm}

resourcestring
  SConfirmDeleteAttribute = 'Delete attribute ''%s''?';

{ TInstantClassDesigner }

function TInstantClassEditorForm.AddAttributeToView(
  View: TListView; Attribute: TInstantCodeAttribute): TListItem;
begin
  Result := View.Items.Add;
  with Result do
  begin
    with Attribute do
      if (HostClass = Subject) or not Assigned(HostClass) then
        Caption := Name else
        Caption := HostClass.Name + '.' + Name;
    Data := Attribute;
    SubItems.Add(Attribute.AttributeTypeText);
    case Attribute.AttributeType of
      atReference: ImageIndex := 1;
      atPart: ImageIndex := 2;
      atReferences: ImageIndex := 3;
      atParts: ImageIndex := 4;
    else
      ImageIndex := 0;
    end;
    if Attribute.HostClass <> Subject then
      ImageIndex := ImageIndex + 5;
  end;
  FitColumns(View);
end;

procedure TInstantClassEditorForm.AttributeDeleteActionExecute(Sender: TObject);
var
  Attribute: TInstantCodeAttribute;
begin
  with IntroducedAttributesView do
    if Assigned(ItemFocused) then
    begin
      Attribute := ItemFocused.Data;
      if not Confirm(Format(SConfirmDeleteAttribute, [Attribute.Name])) then
        Exit;
      DeleteAttribute(Attribute);
      ItemFocused.Delete;
      if Assigned(ItemFocused) then
        ItemFocused.Selected := True;
      FitColumns(IntroducedAttributesView);
    end;
end;

procedure TInstantClassEditorForm.AttributeEditActionExecute(Sender: TObject);
var
  OldName: string;
  Attribute: TInstantCodeAttribute;
  Exists: Boolean;
begin
  Attribute := FocusedAttribute;
  if not Assigned(Attribute) then
    Exit;
  OldName := Attribute.Name;
  Exists := FNewAttributes.IndexOf(Attribute) = -1;
  if Exists then
    Attribute.DetectMethodTypes;
  if EditAttribute(Attribute, Exists) then
  begin
    if Exists and (FChangedAttributes.IndexOfObject(Attribute) = -1) then
      FChangedAttributes.AddObject(OldName, Attribute);
    PopulateIntroducedAttributes;
  end;
end;

procedure TInstantClassEditorForm.AttributeNewActionExecute(
  Sender: TObject);
var
  Attribute: TInstantCodeAttribute;
  NewItem: TListItem;
begin
  Attribute := Subject.AddAttribute;
  if not EditAttribute(Attribute, False, 'New Attribute') then
    Attribute.Free
  else begin
    FNewAttributes.Add(Attribute);
    with IntroducedAttributesView do
    begin
      Items.BeginUpdate;
      try
        NewItem := AddAttributeToView(IntroducedAttributesView, Attribute);
        NewItem.Focused := True;
        Selected := NewItem;
      finally
        Items.EndUpdate;
      end;
      NewItem.MakeVisible{$IFDEF MSWINDOWS}(False){$ENDIF};
    end;
  end;
end;

procedure TInstantClassEditorForm.AttributesMenuPopup(Sender: TObject);
begin
  UpdateActions;
end;

procedure TInstantClassEditorForm.CancelButtonClick(Sender: TObject);
begin
  inherited;
  Subject.AssignAttributes(FBackupAttributes);
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
  FBackupAttributes := TObjectList.Create;
  FChangedAttributes := TStringList.Create;
  FNewAttributes := TList.Create;
end;

procedure TInstantClassEditorForm.DeleteAttribute(
  Attribute: TInstantCodeAttribute);
var
  Index: Integer;
begin
  Index := FChangedAttributes.IndexOfObject(Attribute);
  if Index <> -1 then
    FChangedAttributes.Delete(Index);
  FNewAttributes.Remove(Attribute);
  Attribute.Delete;
  Attribute.Free;
end;

destructor TInstantClassEditorForm.Destroy;
begin
  FNewAttributes.Free;
  FChangedAttributes.Free;
  FBackupAttributes.Free;
  FNameAttribute.Free;
  inherited;
end;

function TInstantClassEditorForm.EditAttribute(
  Attribute: TInstantCodeAttribute; Exists: Boolean;
  const Title: string): Boolean;
begin
  with TInstantAttributeEditorForm.Create(nil) do
  try
    if Title <> '' then
      Caption := Title;
    Model := Self.Model;
    Subject := Attribute;
    Limited := Exists;
    Result := ShowModal = mrOk;
    if Result then
      Attribute.Realize;
  finally
    Free;
  end;
end;

procedure TInstantClassEditorForm.FitColumns(View: TListView);
begin
  with View do
    if Columns.Count > 0 then
    begin
      with Columns[Pred(Columns.Count)] do
      begin
{$IFDEF MSWINDOWS}
        Width := -1;
        Width := -2;
{$ENDIF}
{$IFDEF LINUX}
        Width := View.Width div 2;
{$ENDIF}
      end;
    end;
end;

function TInstantClassEditorForm.GetFocusedAttribute: TInstantCodeAttribute;
begin
  with IntroducedAttributesView do
    if Assigned(ItemFocused) then
      Result := ItemFocused.Data
    else
      Result := nil;
end;

function TInstantClassEditorForm.GetNameAttribute: TInstantCodeAttribute;
begin
  if not Assigned(FNameAttribute) and
    Subject.DerivesFrom(TInstantObject.ClassName) then
  begin
    FNameAttribute := TInstantCodeAttribute.Create(nil);
    FNameAttribute.Name := TInstantObject.ClassName + '.' +
      InstantIdFieldName;
    FNameAttribute.AttributeTypeName := 'String';
  end;
  Result := FNameAttribute;
end;

function TInstantClassEditorForm.GetSubject: TInstantCodeClass;
begin
  Result := inherited Subject as TInstantCodeClass;
end;

procedure TInstantClassEditorForm.IntroducedAttributesViewDblClick(
  Sender: TObject);
begin
  AttributeEditAction.Execute;
end;

procedure TInstantClassEditorForm.IntroducedAttributesViewEdited(
  Sender: TObject; Item: TListItem; var S: String);
var
  Attribute: TInstantCodeAttribute;
begin
  Attribute := TInstantCodeAttribute(Item.Data);
  if Assigned(Attribute) then
  begin
    Attribute.Name := S;
    S := Attribute.Name;
  end;
end;

procedure TInstantClassEditorForm.LoadAttributeView(View: TListView;
  AClass: TInstantCodeClass; Recursive: Boolean);
var
  FocusedData: Pointer;

  procedure LoadClass(AClass: TInstantCodeClass);
  var
    I: Integer;
    NewItem: TListItem;
    FocusItem: TListItem;
  begin
    FocusItem := nil;
    if Assigned(AClass) then
      with AClass do
      begin
        for I := 0 to Pred(AttributeCount) do
        begin
          NewItem := AddAttributeToView(View, Attributes[I]);
          if NewItem.Data = FocusedData then
            FocusItem := NewItem;
        end;
        if Recursive then
          LoadClass(BaseClass)
        else begin
          if not Assigned(FocusedData) and (View.Items.Count > 0) then
            FocusItem := View.Items[0];
          if Assigned(FocusItem) then
          begin
            FocusItem.Focused := True;
            View.Selected := FocusItem;
          end;
        end;
      end;
  end;

begin
  with View do
  begin
    if Assigned(ItemFocused) then
      FocusedData := ItemFocused.Data else
      FocusedData := nil;
    with Items do
    begin
      BeginUpdate;
      try
        Clear;
        if Recursive and Assigned(NameAttribute) then
          AddAttributeToView(View, NameAttribute);
        LoadClass(AClass);
      finally
        EndUpdate;
{$IFDEF MSWINDOWS}
        with View.Column[1] do
{$ENDIF}
{$IFDEF LINUX}
        with View.Columns.Items[1] do
{$ENDIF}
        begin
          Width := -1;
          Width := -2;
        end;
      end;
    end;
  end;
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
begin
  with BaseClassEdit.Items do
  begin
    BeginUpdate;
    try
      Clear;
      Add(TInstantObject.ClassName);
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
    finally
      EndUpdate;
    end;
  end;
end;

procedure TInstantClassEditorForm.PopulateInheritedAttributes;
begin
  LoadAttributeView(InheritedAttributesView, Subject.BaseClass, True);
end;

procedure TInstantClassEditorForm.PopulateIntroducedAttributes;
begin
  LoadAttributeView(IntroducedAttributesView, Subject, False);
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
    PopulateBaseClasses;
    PopulateUnits;
  end;
end;

procedure TInstantClassEditorForm.SetSubject(const Value: TInstantCodeClass);
begin
  if Value <> Subject then
  begin
    inherited Subject := Value;
    Subject.CloneAttributes(FBackupAttributes);
    with PersistenceComboBox do
      ItemIndex := SubjectExposer.GetFieldStrings(Field, Items);
    PopulateBaseClasses;
    PopulateIntroducedAttributes;
    PopulateInheritedAttributes;
    UpdateCaption;
    UpdateControls;
  end;
end;

procedure TInstantClassEditorForm.SubjectExposerAfterPostField(
  Sender: TObject; Field: TField);
begin
  if Field.FieldName = 'BaseClassName' then
  begin
    FreeAndNil(FNameAttribute);
    PopulateInheritedAttributes;
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
var
  Attribute: TInstantCodeAttribute;
begin
  inherited;
  Attribute := FocusedAttribute;
  AttributeEditAction.Enabled := Assigned(Attribute);
  AttributeDeleteAction.Enabled := Assigned(Attribute);
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
  LoadMultipleImages(AttributeImages, 'IO_CLASSEDITORATTRIBUTEIMAGES', HInstance);
  LoadMultipleImages(StateImages, 'IO_CLASSEDITORSTATEIMAGES', HInstance);
  LoadMultipleImages(ActionImages, 'IO_CLASSEDITORACTIONIMAGES', HInstance);
{$IFDEF MSWINDOWS}
  BorderStyle := bsSizeable;
  IntroducedAttributesView.SmallImages := AttributeImages;
  InheritedAttributesView.SmallImages := AttributeImages;
{$ENDIF}
{$IFDEF LINUX}
  BorderStyle := fbsSizeable;
  IntroducedAttributesView.Images := AttributeImages;
  InheritedAttributesView.Images := AttributeImages;
{$ENDIF}
  FTitle := Caption;
  PageControl.ActivePage := ClassSheet;
  ActiveControl := ClassNameEdit;
  ClassSheetResize(nil);
end;

procedure TInstantClassEditorForm.ClassSheetResize(Sender: TObject);
begin
  inherited;
  //simulate right-anchor (for Kylix compatibility)
  ClassNameEdit.Width := ClassSheet.Width - 33;
  BaseClassEdit.Width := ClassSheet.Width - 33;
  UnitEdit.Width := ClassSheet.Width - 33;
  StorageEdit.Width := ClassSheet.Width - 169;
end;

end.
