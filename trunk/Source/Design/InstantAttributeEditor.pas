(*
 *   InstantObjects
 *   Attribute Editor
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
 * Steven Mitchell: updating for OFExpt in MM7
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantAttributeEditor;

interface

{$IFDEF VER130}{$DEFINE MSWINDOWS}{$ENDIF}

uses
  SysUtils, Classes,
  InstantEdit, DB, InstantCode, TypInfo,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, Mask, ComCtrls, ImgList,
{$ENDIF}
{$IFDEF LINUX}
  QImgList, QStdCtrls, QDBCtrls, QMask, QControls, QComCtrls, QExtCtrls,
{$ENDIF}
  InstantPresentation;

type
  TInstantStringsEvent = procedure(Sender: TObject; Items: TStrings) of object;
  TInstantAttrStringsEvent = procedure(Sender: TObject; const ClassName: String;
    Items: TStrings) of object;

  TInstantAttributeEditorForm = class(TInstantEditForm)
    AccessSheet: TTabSheet;
    DefinitionSheet: TTabSheet;
    DisplayWidthEdit: TDBEdit;
    DisplayWidthLabel: TLabel;
    EditMaskEdit: TDBEdit;
    EdtMaskLabel: TLabel;
    MethodAddCheckBox: TCheckBox;
    MethodClearCheckBox: TCheckBox;
    MethodDeleteCheckBox: TCheckBox;
    MethodIndexOfCheckBox: TCheckBox;
    MethodInsertCheckBox: TCheckBox;
    MethodRemoveCheckBox: TCheckBox;
    MethodsGroupBox: TGroupBox;
    NameEdit: TDBEdit;
    NameLabel: TLabel;
    ObjectClassEdit: TDBComboBox;
    ObjectClassLabel: TLabel;
    OptionDefaultCheckBox: TCheckBox;
    OptionIndexedCheckBox: TCheckBox;
    OptionReadOnlyCheckBox: TCheckBox;
    OptionRequiredCheckBox: TCheckBox;
    OptionsGroupBox: TGroupBox;
    PageControl: TPageControl;
    PresentationSheet: TTabSheet;
    SingularNameEdit: TDBEdit;
    SingularNameLabel: TLabel;
    SizeEdit: TDBEdit;
    SizeLabel: TLabel;
    StorageNameEdit: TDBEdit;
    StorageNameLabel: TLabel;
    TypeEdit: TDBComboBox;
    TypeImages: TImageList;
    TypeLabel: TLabel;
    ValidCharsEdit: TDBEdit;
    ValidCharsLabel: TLabel;
    VisibilityEdit: TDBComboBox;
    VisibilityLabel: TLabel;
    DefaultValueLabel: TLabel;
    DefaultValueEdit: TDBEdit;
    ExternalLinkedNameEdit: TDBComboBox;
    ExternalLinkedNameLabel: TLabel;
    ExternalStoredNameEdit: TDBEdit;
    ExternalStoredNameLabel: TLabel;
    IsExternalEdit: TDBComboBox;
    IsExternalLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure NumericFieldGetText(Sender: TField; var Text: string;
      Display: Boolean);
    procedure ObjectClassEditChange(Sender: TObject);
    procedure ObjectClassEditEnter(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure SubjectExposerInitField(Sender: TObject; Field: TField);
    procedure SubjectExposerTranslate(Sender: TObject; Field: TField;
      var Value: Variant; Write: Boolean);
    procedure TypeEditClick(Sender: TObject);
    procedure IsExternalEditChange(Sender: TObject);
    procedure ExternalStoredNameEditChange(Sender: TObject);
    procedure ExternalLinkedNameEditChange(Sender: TObject);
    procedure ExternalLinkedNameEditEnter(Sender: TObject);
  private
    FInMM: boolean;   // True if in ModelMaker, default is False
    FLimited: Boolean;
    FModel: TInstantCodeModel;
    FOnLoadClasses: TInstantStringsEvent;
    FOnLoadClassAttributes: TInstantAttrStringsEvent;  
    function GetSubject: TInstantCodeAttribute;
    procedure SetSubject(const Value: TInstantCodeAttribute);
    procedure SetLimited(Value: Boolean);
    procedure SetModel(const Value: TInstantCodeModel);
  protected
    procedure LoadClasses;
    procedure LoadClassAttributes;
    procedure LoadData; override;
    procedure LoadEnums(TypeInfo: PTypeInfo; Items: TStrings;
      Values: Pointer);
    procedure LoadTypes;
    procedure LoadVisibilities;
    procedure LoadIsExternal;
    procedure PopulateClasses;
    procedure PopulateClassAttributes;
    procedure SaveData; override;
    procedure SubjectChanged; override;
    procedure UpdateControls;
  public
    property InMM: boolean read FInMM write FInMM;
    property Limited: Boolean read FLimited write SetLimited;
    property Model: TInstantCodeModel read FModel write SetModel;
    property OnLoadClasses: TInstantStringsEvent read FOnLoadClasses write FOnLoadClasses;
    property OnLoadClassAttributes: TInstantAttrStringsEvent read FOnLoadClassAttributes write FOnLoadClassAttributes;
    property Subject: TInstantCodeAttribute read GetSubject write SetSubject;
  end;

implementation

uses
  InstantRtti, InstantPersistence, InstantDesignUtils, InstantImageUtils;

{$R *.dfm}

{ TInstantAttributeEditorForm }

procedure TInstantAttributeEditorForm.FormCreate(Sender: TObject);
begin
  LoadMultipleImages(TypeImages, 'IO_ATTRIBUTEEDITORIMAGES', HInstance);
  PageControl.ActivePage := DefinitionSheet;
  ActiveControl := NameEdit;
  FInMM := False;       
end;

function TInstantAttributeEditorForm.GetSubject: TInstantCodeAttribute;
begin
  Result := inherited Subject as TInstantCodeAttribute;
end;

procedure TInstantAttributeEditorForm.LoadClasses;
var
  I: Integer;
begin
  with ObjectClassEdit do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      if Assigned(FOnLoadClasses) then
        FOnLoadClasses(Self, Items)
      else if Assigned(FModel) then
        for I := 0 to Pred(FModel.ClassCount) do
          Items.Add(FModel.Classes[I].Name);
      if Assigned(Field) then
        ItemIndex := Items.IndexOf(Field.AsString);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TInstantAttributeEditorForm.LoadData;

  procedure LoadOptions;
  begin
    OptionIndexedCheckBox.Checked := Subject.IsIndexed;
    OptionRequiredCheckBox.Checked := Subject.IsRequired;
    OptionReadOnlyCheckBox.Checked := Subject.ReadOnly;
    OptionDefaultCheckBox.Checked := Subject.IsDefault;
  end;

  procedure LoadMethods;
  begin
    MethodAddCheckBox.Checked := Subject.IncludeAddMethod;
    MethodRemoveCheckBox.Checked := Subject.IncludeRemoveMethod;
    MethodInsertCheckBox.Checked := Subject.IncludeInsertMethod;
    MethodDeleteCheckBox.Checked := Subject.IncludeDeleteMethod;
    MethodIndexOfCheckBox.Checked := Subject.IncludeIndexOfMethod;
    MethodClearCheckBox.Checked := Subject.IncludeClearMethod;
  end;

begin
  inherited;
  LoadOptions;
  LoadMethods;
end;

procedure TInstantAttributeEditorForm.LoadEnums(TypeInfo: PTypeInfo;
  Items: TStrings; Values: Pointer);
type
  PByteSet = ^TByteSet;
  TByteSet = set of Byte;
var
  Names: TStringList;
  ByteSet: PByteSet;
  S: string;
  I: Integer;
begin
  ByteSet := Values;
  Names := TStringList.Create;
  try
    InstantGetEnumNames(TypeInfo, Names);
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to Pred(Names.Count) do
      begin
        if not Assigned(ByteSet) or not (I in ByteSet^) then
        begin
          S := Names[I];
          Items.Add(Copy(S, 3, Length(S)));
        end;
      end;
    finally
      Items.EndUpdate;
    end;
  finally
    Names.Free;
  end;
end;

procedure TInstantAttributeEditorForm.LoadIsExternal;
begin
  with IsExternalEdit do
    ItemIndex := SubjectExposer.GetFieldStrings(Field, Items);
end;

procedure TInstantAttributeEditorForm.LoadTypes;
var
  I: Integer;
begin
  with TypeEdit do
  begin
    ItemIndex := SubjectExposer.GetFieldStrings(Field, Items);
    I := Items.IndexOf('Unknown');
    if I <> -1 then
      Items.Delete(I);
  end;
end;

procedure TInstantAttributeEditorForm.LoadVisibilities;
var
  I: Integer;
begin
  with VisibilityEdit do
  begin
    SubjectExposer.GetFieldStrings(Field, Items);
    I := Items.IndexOf('Default');
    if I <> -1 then
      Items.Delete(I);
    if Assigned(Subject) and Subject.IsContainer then
    begin
      I := Items.IndexOf('Published');
      if I <> -1 then
        Items.Delete(I);
    end;
    if Assigned(Field) then
      ItemIndex := Items.IndexOf(Field.AsString);
  end;
end;

procedure TInstantAttributeEditorForm.NameEditChange(Sender: TObject);
begin
  with NameEdit do
    SubjectExposer.AssignFieldValue(Field, Text);
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.NumericFieldGetText(Sender: TField;
  var Text: string; Display: Boolean);
begin
  if Sender.AsInteger = 0 then
    Text := ''
  else
    Text := IntToStr(Sender.AsInteger);
end;

procedure TInstantAttributeEditorForm.ObjectClassEditChange(
  Sender: TObject);
begin
  with ObjectClassEdit do
    SubjectExposer.AssignFieldValue(Field, Text);
  UpdateControls;
  LoadClassAttributes;  
end;

procedure TInstantAttributeEditorForm.ObjectClassEditEnter(
  Sender: TObject);
begin
  PopulateClasses;
end;

procedure TInstantAttributeEditorForm.OkButtonClick(Sender: TObject);
var
  Attribute: TInstantCodeAttribute;
  I: Integer;
begin
  if Assigned(Subject.Owner) then
    with Subject.Owner do
      for I := 0 to Pred(AttributeCount) do
      begin
        Attribute := Attributes[I];
        if (Attribute <> Subject) and SameText(Attribute.Name, Subject.Name) then
        begin
          ModalResult := mrNone;
          raise Exception.Create('Name already used');
        end;
      end;
  if InMM then
    SaveData
  else
    inherited;
end;

procedure TInstantAttributeEditorForm.PopulateClasses;
begin
  if ObjectClassEdit.Items.Count = 0 then
    LoadClasses;
end;

procedure TInstantAttributeEditorForm.SaveData;

  procedure SaveOptions;
  begin
    Subject.IsIndexed := OptionIndexedCheckBox.Checked;
    Subject.IsRequired := OptionRequiredCheckBox.Checked;
    Subject.ReadOnly := OptionReadOnlyCheckBox.Checked;
    Subject.IsDefault := OptionDefaultCheckBox.Checked;
  end;

  procedure SaveMethods;
  begin
    Subject.IncludeAddMethod := MethodAddCheckBox.Checked;
    Subject.IncludeRemoveMethod := MethodRemoveCheckBox.Checked;
    Subject.IncludeInsertMethod := MethodInsertCheckBox.Checked;
    Subject.IncludeDeleteMethod := MethodDeleteCheckBox.Checked;
    Subject.IncludeIndexOfMethod := MethodIndexOfCheckBox.Checked;
    Subject.IncludeClearMethod := MethodClearCheckBox.Checked;
  end;

begin
  inherited;
  SaveOptions;
  SaveMethods;
end;

procedure TInstantAttributeEditorForm.SetLimited(Value: Boolean);
begin
  if Value <> FLimited then
  begin
    FLimited := Value;
    UpdateControls;
  end;
end;

procedure TInstantAttributeEditorForm.SetModel(const Value: TInstantCodeModel);
begin
  if FModel <> Value then
  begin
    FModel := Value;
    LoadClasses;
  end;
end;

procedure TInstantAttributeEditorForm.SetSubject(
  const Value: TInstantCodeAttribute);
begin
  inherited Subject := Value;
end;

procedure TInstantAttributeEditorForm.SubjectChanged;
begin
  inherited;
  LoadTypes;
  LoadVisibilities;
  LoadIsExternal;
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.SubjectExposerInitField(
  Sender: TObject; Field: TField);
begin
  if (Field.FieldName = 'Metadata.Size') or
    (Field.FieldName = 'Metadata.DisplayWidth') then
    Field.OnGetText := NumericFieldGetText;
end;

procedure TInstantAttributeEditorForm.SubjectExposerTranslate(
  Sender: TObject; Field: TField; var Value: Variant; Write: Boolean);

  procedure TranslateEnum(const Prefix: string);
  var
    Name: string;
  begin
    if Write then
      Value := Prefix + Value
    else begin
      Name := Value;
      Delete(Name, 1, Length(Prefix));
      Value := Name;
    end
  end;

begin
  if Field.FieldName = 'AttributeType' then
    TranslateEnum('at')
  else if Field.FieldName = 'Visibility' then
    TranslateEnum('vi')
  else if Field.FieldName = 'IsExternal' then
    TranslateEnum('ce');
end;

procedure TInstantAttributeEditorForm.TypeEditClick(Sender: TObject);
begin
  with TypeEdit do
    SubjectExposer.AssignFieldValue(Field, Text);
  // Controls need to be enabled first to
  // reliably load combo dropdown lists in MM OFExpt
  UpdateControls;
  LoadVisibilities;
  LoadIsExternal;
end;

procedure TInstantAttributeEditorForm.UpdateControls;

  procedure DisableSubControls(Parent: TWinControl; Disable: Boolean);
  var
    I: Integer;
  begin
    with Parent do
      for I := 0 to Pred(ControlCount) do
      begin
        Controls[I].Enabled := not Disable;
        if Controls[I] is TWinControl then
          DisableSubControls(TWinControl(Controls[I]), Disable);
      end;
  end;

  procedure EnableCtrl(Control: TControl; Enable: Boolean);
  begin
    EnableControl(Control, Enable, SubjectSource);
  end;

var
  HasName, HasClass, HasExternalStoredName, HasExternalLinkedName: Boolean;
  IsComplex, IsContainer, CanBeExternal, IsExternal, IsMaskable, IsString, IsValid: Boolean;
begin
  CanBeExternal := Subject.AttributeType in [atPart, atParts, atReferences];
  if not CanBeExternal then
    Subject.IsExternal := ceNo;
  if Subject.IsExternal = ceLinked then
    Subject.ExternalStoredName := '';
  if Subject.IsExternal = ceStored then
    Subject.ExternalLinkedName := '';

  HasName := NameEdit.Text <> '';
  HasClass := ObjectClassEdit.Text <> '';
  IsComplex := Subject.IsComplex;
  IsMaskable := Subject.AttributeType in [atString, atMemo, atFloat, atCurrency, atInteger];
  IsContainer := Subject.IsContainer;
  HasExternalStoredName := ExternalStoredNameEdit.Text <> '';
  HasExternalLinkedName := ExternalLinkedNameEdit.Text <> '';

  IsExternal := Subject.IsExternal <> ceNo;
  IsString := Subject.AttributeType in [atString, atMemo];
  IsValid := HasName and (not IsComplex or HasClass) and
    (not IsExternal or (HasExternalStoredName or HasExternalLinkedName));

  DisableSubControls(DefinitionSheet, Limited);
  DisableSubControls(AccessSheet, Limited);
  if not Limited then
  begin
    EnableCtrl(ObjectClassLabel, IsComplex);
    EnableCtrl(ObjectClassEdit, IsComplex);
    EnableCtrl(SingularNameLabel, IsContainer);
    EnableCtrl(SingularNameEdit, IsContainer);
    EnableCtrl(OptionDefaultCheckBox, IsContainer);
    EnableCtrl(MethodsGroupBox, IsContainer);
    EnableCtrl(MethodAddCheckBox, IsContainer);
    EnableCtrl(MethodClearCheckBox, IsContainer);
    EnableCtrl(MethodDeleteCheckBox, IsContainer);
    EnableCtrl(MethodIndexOfCheckBox, IsContainer);
    EnableCtrl(MethodInsertCheckBox, IsContainer);
    EnableCtrl(MethodRemoveCheckBox, IsContainer);

    EnableCtrl(IsExternalEdit, CanBeExternal);
    EnableCtrl(IsExternalLabel, CanBeExternal);
  end;
  EnableCtrl(StorageNameLabel, not IsExternal);
  EnableCtrl(StorageNameEdit, not IsExternal);

  EnableCtrl(ExternalLinkedNameLabel, IsExternal and (Subject.IsExternal = ceLinked));
  EnableCtrl(ExternalLinkedNameEdit, IsExternal and (Subject.IsExternal = ceLinked));
  EnableCtrl(ExternalStoredNameLabel, IsExternal and (Subject.IsExternal = ceStored));
  EnableCtrl(ExternalStoredNameEdit, IsExternal and (Subject.IsExternal = ceStored));

  EnableCtrl(SizeLabel, IsString);
  EnableCtrl(SizeEdit, IsString);
  EnableCtrl(OptionsGroupBox, True);
  EnableCtrl(OptionIndexedCheckBox, True);
  EnableCtrl(OptionRequiredCheckBox, True);
  EnableCtrl(OkButton, IsValid);
  PresentationSheet.TabVisible := IsMaskable;
end;

procedure TInstantAttributeEditorForm.IsExternalEditChange(
  Sender: TObject);
begin
  with IsExternalEdit do
    SubjectExposer.AssignFieldValue(Field, Text);
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.ExternalStoredNameEditChange(
  Sender: TObject);
begin
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.ExternalLinkedNameEditChange(
  Sender: TObject);
begin
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.LoadClassAttributes;
var
  I:integer;
begin
  with ExternalLinkedNameEdit do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      if Assigned(FOnLoadClassAttributes) then begin
          FOnLoadClassAttributes(Self, ObjectClassEdit.Text, Items);
      end
      else begin
        if Assigned(FModel) then
          if Assigned(FModel.FindClass(ObjectClassEdit.Text)) then
            for I := 0 to Pred(FModel.FindClass(ObjectClassEdit.Text).AttributeCount) do
              Items.Add(FModel.FindClass(ObjectClassEdit.Text).Attributes[i].Name);
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TInstantAttributeEditorForm.PopulateClassAttributes;
begin
  if ExternalLinkedNameEdit.Items.Count = 0 then
    LoadClassAttributes;
end;

procedure TInstantAttributeEditorForm.ExternalLinkedNameEditEnter(
  Sender: TObject);
begin
  PopulateClassAttributes;
end;

end.
