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
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena, Steven Mitchell,
 * David Moorhouse, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantAttributeEditor;

interface

{$I '..\InstantDefines.inc'}

uses
  SysUtils, Classes,
  InstantEdit, DB, InstantCode, TypInfo,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, Mask, ComCtrls, ImgList,
  InstantPresentation, InstantTypes;

type
  TInstantStringsEvent = procedure(Sender: TObject; Items: TStrings;
    PersistentOnly: Boolean) of object;

  TInstantBooleanEvent = procedure (Sender: TObject; const AClassName: String;
    var IsPersistent: Boolean) of object;

  TInstantAttributeValidator = class
  private
    FAttribute: TInstantCodeAttribute;
    FInterface: IInterface;
    FErrorMsg: string;
  protected
    property Attribute: TInstantCodeAttribute read FAttribute;
    property MMInterface: IInterface read FInterface;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;
  public
    constructor Create(AnAttribute: TInstantCodeAttribute; const AInterface: IInterface);
    function Validate: Boolean; virtual;
  end;

  TInstantAttributeEditorForm = class(TInstantEditForm)
    AccessSheet: TTabSheet;
    DefinitionSheet: TTabSheet;
    DisplayWidthEdit: TDBEdit;
    DisplayWidthLabel: TLabel;
    DisplayLabelEdit: TDBEdit;
    DisplayLabelLabel: TLabel;
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
    OptionUniqueCheckBox: TCheckBox;
    OptionPrimaryKeyCheckBox: TCheckBox;
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
    TypeLabel: TLabel;
    ValidCharsEdit: TDBEdit;
    ValidCharsLabel: TLabel;
    VisibilityEdit: TDBComboBox;
    VisibilityLabel: TLabel;
    DefaultValueLabel: TLabel;
    DefaultValueEdit: TDBEdit;
    ExternalStorageNameEdit: TDBEdit;
    ExternalStorageNameLabel: TLabel;
    StorageKindEdit: TDBComboBox;
    StorageKindLabel: TLabel;
    AutoExternalStorageNameCheckBox: TCheckBox;
    OptionUseNullCheckBox: TCheckBox;
    EnumeratedTypeLabel: TLabel;
    EnumeratedTypeEdit: TDBComboBox;
    IndexNameLabel: TLabel;
    IndexNameEdit: TDBEdit;
    OptionIsDescriptionCheckBox: TCheckBox;
    ForeignKeyFieldsLabel: TLabel;
    ForeignKeyFieldsEdit: TDBEdit;
    procedure NameEditKeyPress(Sender: TObject; var Key: Char);
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
    procedure StorageKindEditChange(Sender: TObject);
    procedure ExternalStorageNameEditChange(Sender: TObject);
    procedure AutoExternalStorageNameCheckBoxClick(Sender: TObject);
    procedure StorageNameEditChange(Sender: TObject);
    procedure EnumeratedTypeEditChange(Sender: TObject);
    procedure EnumeratedTypeEditEnter(Sender: TObject);
    procedure OptionIndexedCheckBoxClick(Sender: TObject);
    procedure ForeignKeyFieldsEditChange(Sender: TObject);
  private
    FBaseClassStorageName: string;
    FLimited: Boolean;
    FMMUninitializedNewAttribute: Boolean;
    FModel: TInstantCodeModel;
    FOnIsClassPersistent: TInstantBooleanEvent;
    FOnLoadClasses: TInstantStringsEvent;
    FValidator: TInstantAttributeValidator;
    function GetSubject: TInstantCodeAttribute;
    procedure SetSubject(const Value: TInstantCodeAttribute);
    procedure SetLimited(Value: Boolean);
    procedure SetModel(const Value: TInstantCodeModel);
    procedure SetValidator(const Value: TInstantAttributeValidator);
  protected
    procedure LoadClasses;
    procedure LoadData; override;
// Do not delete
//    procedure LoadEnums(TypeInfo: PTypeInfo; Items: TStrings;
//      Values: Pointer);
    procedure LoadEnum;
    procedure LoadTypes;
    procedure LoadVisibilities;
    procedure LoadStorageKind;
    procedure PopulateClasses;
    procedure SaveData; override;
    procedure SubjectChanged; override;
    procedure UpdateControls;
    procedure ComputeExternalStorageName;
    procedure ComputeForeignKeyFields;
    function IsClassPersistent(const AClassName: String): Boolean;
  public
    property BaseClassStorageName: string read FBaseClassStorageName write
      FBaseClassStorageName;
    property Limited: Boolean read FLimited write SetLimited;
    property Model: TInstantCodeModel read FModel write SetModel;
    property OnIsClassPersistent: TInstantBooleanEvent read FOnIsClassPersistent
        write FOnIsClassPersistent;
    property OnLoadClasses: TInstantStringsEvent read FOnLoadClasses
      write FOnLoadClasses;
    property Subject: TInstantCodeAttribute read GetSubject write SetSubject;
    property Validator: TInstantAttributeValidator read FValidator write SetValidator;
  end;

implementation

uses
  InstantRtti, InstantPersistence, InstantDesignUtils, InstantImageUtils,
  InstantClasses, InstantMetadata;

{$R *.dfm}

resourcestring
  SConfirmZeroSizeStringAttribute = 'Unspecified Size. Most brokers require ' +
    'you to specify a Size for attributes of type String. Are you sure you ' +
    'want to define a String attribute without a Size?';

{ TInstantAttributeEditorForm }

procedure TInstantAttributeEditorForm.FormCreate(Sender: TObject);
begin
  inherited;
  PageControl.ActivePage := DefinitionSheet;
  ActiveControl := NameEdit;
  FValidator := TInstantAttributeValidator.Create(Subject, nil);
end;

function TInstantAttributeEditorForm.GetSubject: TInstantCodeAttribute;
begin
  Result := inherited Subject as TInstantCodeAttribute;
end;

procedure TInstantAttributeEditorForm.LoadClasses;

  function NeedOnlyPersistentClasses: Boolean;
  begin
    Result := SameText(TypeEdit.Text, 'Reference') or
            SameText(TypeEdit.Text, 'References');
  end;

var
  I: Integer;
begin
  ObjectClassEdit.Items.BeginUpdate;
  try
    ObjectClassEdit.Items.Clear;

    if Assigned(FModel) then
    begin
      for I := 0 to Pred(FModel.ClassCount) do
        if not NeedOnlyPersistentClasses or
            (NeedOnlyPersistentClasses and
            IsClassPersistent(FModel.Classes[I].Name)) then
          ObjectClassEdit.Items.Add(FModel.Classes[I].Name);
    end
    else if Assigned(ObjectClassEdit.Field) and
        (ObjectClassEdit.Field.AsString <> '') and
        not FMMUninitializedNewAttribute then
      ObjectClassEdit.Items.Add(ObjectClassEdit.Field.AsString)
    else if Assigned(FOnLoadClasses) then
    begin
      OnLoadClasses(Self, ObjectClassEdit.Items, NeedOnlyPersistentClasses);
      FMMUninitializedNewAttribute := True;
    end;

    if Assigned(ObjectClassEdit.Field) then
      ObjectClassEdit.ItemIndex :=
        ObjectClassEdit.Items.IndexOf(ObjectClassEdit.Field.AsString);
  finally
    ObjectClassEdit.Items.EndUpdate;
  end;
end;

procedure TInstantAttributeEditorForm.LoadData;

  procedure LoadOptions;
  begin
    OptionIndexedCheckBox.Checked := Subject.IsIndexed;
    OptionRequiredCheckBox.Checked := Subject.IsRequired;
    OptionUniqueCheckBox.Checked := Subject.IsUnique;
    OptionPrimaryKeyCheckBox.Checked := Subject.IsPrimaryKey;
    OptionIsDescriptionCheckBox.Checked := Subject.IsDescription;
    OptionReadOnlyCheckBox.Checked := Subject.ReadOnly;
    OptionDefaultCheckBox.Checked := Subject.IsDefault;
    OptionUseNullCheckBox.Checked := Subject.UseNull;
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
  if (assigned(Subject)) then
  begin
    LoadOptions;
    LoadMethods;
  end;
  LoadEnum;
end;

{procedure TInstantAttributeEditorForm.LoadEnums(TypeInfo: PTypeInfo;
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
end; }

procedure TInstantAttributeEditorForm.LoadStorageKind;
begin
  StorageKindEdit.ItemIndex :=
    SubjectExposer.GetFieldStrings(StorageKindEdit.Field, StorageKindEdit.Items);
end;

procedure TInstantAttributeEditorForm.LoadEnum;
var
  I: Integer;
  CodeType: TInstantCodeType;
begin
  EnumeratedTypeEdit.Items.BeginUpdate;
  try
    EnumeratedTypeEdit.Clear;
    if Assigned(FModel) then
    begin
      for I := 0 to Pred(FModel.TypeCount) do
      begin
        CodeType := FModel.Types[I];
        if CodeType is TInstantCodeEnum then
          EnumeratedTypeEdit.Items.AddObject(CodeType.Name, CodeType);
      end;
    end;

    EnumeratedTypeEdit.ItemIndex :=
      EnumeratedTypeEdit.Items.IndexOf(SubjectExposer.FieldByName('Metadata.EnumName').AsString);
  finally
    EnumeratedTypeEdit.Items.EndUpdate;
  end;
end;

procedure TInstantAttributeEditorForm.LoadTypes;

  procedure RestrictForComplexAttr;
  var
    I: Integer;
  begin
    for I := Pred(TypeEdit.Items.Count) downto 0 do
      if not ((TypeEdit.Items[I] = 'Part') or
          (TypeEdit.Items[I] = 'Parts') or
          (IsClassPersistent(ObjectClassEdit.Text) and
          ((TypeEdit.Items[I] = 'Reference') or
          (TypeEdit.Items[I] = 'References')))) then
        TypeEdit.Items.Delete(I);
  end;

var
  I: Integer;
begin
  TypeEdit.Items.BeginUpdate;
  try
    TypeEdit.ItemIndex := SubjectExposer.GetFieldStrings(TypeEdit.Field,
      TypeEdit.Items);
    I := TypeEdit.Items.IndexOf('Unknown');
    if I <> -1 then
      TypeEdit.Items.Delete(I);
    if not Assigned(FModel) and Subject.IsComplex then
      RestrictForComplexAttr;
  finally
    TypeEdit.Items.EndUpdate;
  end;
end;

procedure TInstantAttributeEditorForm.LoadVisibilities;

  function FindVisibility(ACodeClass: TInstantCodeClass;
    AValuePropName: string): TInstantCodeProperty;
  begin
    if ACodeClass <> nil then
    begin
      Result := ACodeClass.FindProperty(AValuePropName);

      if Result = nil then
        Result := FindVisibility(ACodeClass.BaseClass, AValuePropName);
    end else
      Result := nil;
  end;

var
  I: Integer;
  S: String;
  CodeProperty: TInstantCodeProperty;
begin
  VisibilityEdit.Items.BeginUpdate;
  try
    SubjectExposer.GetFieldStrings(VisibilityEdit.Field, VisibilityEdit.Items);
    I := VisibilityEdit.Items.IndexOf('Default');
    if I <> -1 then
      VisibilityEdit.Items.Delete(I);

    if Assigned(Subject) and Subject.IsContainer then
    begin
      I := VisibilityEdit.Items.IndexOf('Published');
      if I <> -1 then
        VisibilityEdit.Items.Delete(I);
    end;

    if Limited then
    begin
      CodeProperty := FindVisibility(Subject.HostClass, Subject.ValuePropName);
      if CodeProperty <> nil then
      begin
        S := GetEnumName(TypeInfo(TInstantCodeVisibility),
          Ord(CodeProperty.Visibility));
        VisibilityEdit.ItemIndex :=
          VisibilityEdit.Items.IndexOf(Copy(S, 3, length(S)));
      end else
        VisibilityEdit.ItemIndex := -1;
    end
    else if Assigned(VisibilityEdit.Field) then
      VisibilityEdit.ItemIndex :=
        VisibilityEdit.Items.IndexOf(VisibilityEdit.Field.AsString);
  finally
    VisibilityEdit.Items.EndUpdate;
  end;
end;

procedure TInstantAttributeEditorForm.NameEditChange(Sender: TObject);
begin
  SubjectExposer.AssignFieldValue(NameEdit.Field, NameEdit.Text);
  UpdateControls;
  ComputeExternalStorageName;
end;

procedure TInstantAttributeEditorForm.NameEditKeyPress(Sender: TObject;
  var Key: Char);
begin
  inherited;
  if Key = ' ' then
    Key := '_';
end;

procedure TInstantAttributeEditorForm.NumericFieldGetText(Sender: TField;
  var Text: string; Display: Boolean);
begin
  if Sender.AsInteger = 0 then
    Text := ''
  else
    Text := IntToStr(Sender.AsInteger);
end;

procedure TInstantAttributeEditorForm.ObjectClassEditChange(Sender: TObject);
begin
  SubjectExposer.AssignFieldValue(ObjectClassEdit.Field, ObjectClassEdit.Text);
  UpdateControls;
  if StorageKindEdit.Items.Count = 0 then
    LoadStorageKind;
end;

procedure TInstantAttributeEditorForm.ObjectClassEditEnter(
  Sender: TObject);
begin
  PopulateClasses;
end;

procedure TInstantAttributeEditorForm.OkButtonClick(Sender: TObject);
begin
  if OKButton.CanFocus then
    OKButton.SetFocus;

  if not FValidator.Validate then
  begin
    ModalResult := mrNone;
    PageControl.ActivePage := DefinitionSheet;
    NameEdit.SetFocus;
    MessageDlg(FValidator.ErrorMsg, mtError, [mbOK], 0);
    Abort;
  end;

  if (Subject.AttributeType = atString) and (Subject.Metadata.Size = 0) then
    if not Confirm(SConfirmZeroSizeStringAttribute) then
    begin
      ModalResult := mrNone;
      PageControl.ActivePage := DefinitionSheet;
      SizeEdit.SetFocus;
      Exit;
    end;

  if ObjectClassEdit.Enabled then
    if not IsClassPersistent(ObjectClassEdit.Field.AsString) and
        (Subject.StorageKind <> skEmbedded) then
      Subject.StorageKind := skEmbedded;

  inherited;
end;

procedure TInstantAttributeEditorForm.PopulateClasses;
begin
  LoadClasses;
end;

// Must update SubjectExposer fields in SaveData so that
// SubjectExposer.PostChanges does not overwrite our changes.
procedure TInstantAttributeEditorForm.SaveData;

  function SetChangedField(const AFieldName: String; ACheckBoxChecked: Boolean):
    Boolean;
  begin
    Result := False;
    if SubjectExposer.FieldByName(AFieldName).AsBoolean <> ACheckBoxChecked then
    begin
      SubjectExposer.FieldByName(AFieldName).AsBoolean := ACheckBoxChecked;
      Result := True;
    end;
  end;

  function SaveOptions: Boolean;
  begin
    Result := False;
    if SetChangedField('IsIndexed', OptionIndexedCheckBox.Checked) then
      Result := True;
    if SetChangedField('IsRequired', OptionRequiredCheckBox.Checked) then
      Result := True;
    if SetChangedField('IsUnique', OptionUniqueCheckBox.Checked) then
      Result := True;
    if SetChangedField('IsPrimaryKey', OptionPrimaryKeyCheckBox.Checked) then
      Result := True;
    if SetChangedField('IsDescription', OptionIsDescriptionCheckBox.Checked) then
      Result := True;
    if SetChangedField('ReadOnly', OptionReadOnlyCheckBox.Checked) then
      Result := True;
    if SetChangedField('IsDefault', OptionDefaultCheckBox.Checked) then
      Result := True;
    if SetChangedField('UseNull', OptionUseNullCheckBox.Checked) then
      Result := True;
  end;

  function SaveMethods: Boolean;
  begin
    Result := False;
    if SetChangedField('IncludeAddMethod', MethodAddCheckBox.Checked) then
      Result := True;
    if SetChangedField('IncludeRemoveMethod', MethodRemoveCheckBox.Checked) then
      Result := True;
    if SetChangedField('IncludeInsertMethod', MethodInsertCheckBox.Checked) then
      Result := True;
    if SetChangedField('IncludeDeleteMethod', MethodDeleteCheckBox.Checked) then
      Result := True;
    if SetChangedField('IncludeIndexOfMethod', MethodIndexOfCheckBox.Checked) then
      Result := True;
    if SetChangedField('IncludeClearMethod', MethodClearCheckBox.Checked) then
      Result := True;
  end;

var
  OptionsChanged: Boolean;
  MethodsChanged: Boolean;
begin
  inherited;
  OptionsChanged := SaveOptions;
  MethodsChanged := SaveMethods;
  if OptionsChanged or MethodsChanged then
    SubjectExposer.Edit;
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

procedure TInstantAttributeEditorForm.SetSubject(const Value: TInstantCodeAttribute);
begin
  inherited Subject := Value;
  if FValidator <> nil then
    FValidator.FAttribute := Value;
end;

procedure TInstantAttributeEditorForm.SubjectChanged;
begin
  inherited;
  LoadTypes;
  LoadVisibilities;
  LoadStorageKind;
  LoadEnum;
  UpdateControls;
  ComputeExternalStorageName;
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
    else
    begin
      Name := Value;
      Delete(Name, 1, Length(Prefix));
      Value := Name;
    end;
  end;

begin
  if Field.FieldName = 'AttributeType' then
    TranslateEnum('at')
  else
    if Field.FieldName = 'Visibility' then
      TranslateEnum('vi')
    else
      if Field.FieldName = 'StorageKind' then
        TranslateEnum('sk');
end;

procedure TInstantAttributeEditorForm.TypeEditClick(Sender: TObject);
begin
  SubjectExposer.AssignFieldValue(TypeEdit.Field, TypeEdit.Text);
  // Controls need to be enabled first to
  // reliably load combo dropdown lists in MM OFExpt
  UpdateControls;
  LoadVisibilities;
  LoadStorageKind;
  ComputeExternalStorageName;
end;

procedure TInstantAttributeEditorForm.UpdateControls;

  procedure DisableSubControls(Parent: TWinControl; Disable: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to Pred(Parent.ControlCount) do
    begin
      if Parent.Controls[I] is TWinControl then
      begin
        //Disable control only if doesn't have focus
        if not TWinControl(Parent.Controls[I]).Focused then
          Parent.Controls[I].Enabled := not Disable;
        DisableSubControls(TWinControl(Parent.Controls[I]), Disable);
      end
      else
        Parent.Controls[I].Enabled := not Disable;
    end;
  end;

  procedure EnableCtrl(Control: TControl; Enable: Boolean);
  begin
    EnableControl(Control, Enable, SubjectSource);
  end;

  function IsObjectClassPersistent: Boolean;
  begin
    Result := False;
    if Assigned(ObjectClassEdit.Field) then
      Result := IsClassPersistent(ObjectClassEdit.Field.AsString);
  end;

var
  HasName, HasClass, HasEnum, IsComplex, IsContainer, IsEnum, CanBeExternal,
    CanHaveForeignKeyFields, CanHaveStorageName, CanHaveExternalStorageName,
    IsMaskable, IsString, IsValid, CanPk: Boolean;
begin
  CanBeExternal := False;
  CanHaveStorageName := True;
  CanHaveExternalStorageName := False;
  CanHaveForeignKeyFields := False;
  IsComplex := False;
  IsMaskable := False;
  IsContainer := False;
  IsString := False;
  IsEnum := False;
  CanPk := False;

  if Assigned(Subject) then
  begin
    CanBeExternal := Subject.CanBeExternal;
    if not CanBeExternal then
      Subject.StorageKind := skEmbedded;
    if Subject.AttributeType = atPart then
      Subject.ExternalStorageName := '';
    IsComplex := Subject.IsComplex;
    IsMaskable := Subject.AttributeType in [atString, atMemo, atFloat,
      atCurrency, atInteger];
    IsContainer := Subject.IsContainer;
    CanHaveStorageName := Subject.CanHaveStorageName;
    CanHaveExternalStorageName := Subject.CanHaveExternalStorageName;
    CanHaveForeignKeyFields := Subject.CanHaveForeignKeyFields;
    IsString := Subject.AttributeType in [atString, atMemo];
    IsEnum := Subject.IsEnum;
    CanPk := Subject.AttributeType in [atInteger, atString, atDateTime, atDate, atTime];
  end;

  HasName := NameEdit.Text <> '';
  HasClass := ObjectClassEdit.Text <> '';
  HasEnum := EnumeratedTypeEdit.Text <> '';

  IsValid := HasName and ((not IsComplex or HasClass)) or ((IsEnum) and (HasEnum));

  DisableSubControls(DefinitionSheet, Limited);
  DisableSubControls(AccessSheet, Limited);
  if not Limited then
  begin
    EnableCtrl(ObjectClassLabel, IsComplex);
    EnableCtrl(ObjectClassEdit, IsComplex);
    EnableCtrl(SingularNameLabel, IsContainer);
    EnableCtrl(SingularNameEdit, IsContainer);
    EnableCtrl(OptionDefaultCheckBox, IsContainer);
    EnableCtrl(OptionUseNullCheckBox, not IsContainer);
    EnableCtrl(MethodsGroupBox, IsContainer);
    EnableCtrl(MethodAddCheckBox, IsContainer);
    EnableCtrl(MethodClearCheckBox, IsContainer);
    EnableCtrl(MethodDeleteCheckBox, IsContainer);
    EnableCtrl(MethodIndexOfCheckBox, IsContainer);
    EnableCtrl(MethodInsertCheckBox, IsContainer);
    EnableCtrl(MethodRemoveCheckBox, IsContainer);

    EnableCtrl(StorageKindEdit, CanBeExternal and IsObjectClassPersistent);
    EnableCtrl(StorageKindLabel, CanBeExternal and IsObjectClassPersistent);
    EnableCtrl(EnumeratedTypeLabel, IsEnum);
    EnableCtrl(EnumeratedTypeEdit, IsEnum);
  end;
  EnableCtrl(StorageNameLabel, CanHaveStorageName);
  EnableCtrl(StorageNameEdit, CanHaveStorageName);

  EnableCtrl(ForeignKeyFieldsLabel, CanHaveForeignKeyFields);
  EnableCtrl(ForeignKeyFieldsEdit, CanHaveForeignKeyFields);

  EnableCtrl(ExternalStorageNameLabel, CanHaveExternalStorageName);
  EnableCtrl(ExternalStorageNameEdit, CanHaveExternalStorageName);
  EnableCtrl(AutoExternalStorageNameCheckBox, CanHaveExternalStorageName);

  EnableCtrl(SizeLabel, IsString);
  EnableCtrl(SizeEdit, IsString);
  EnableCtrl(OptionsGroupBox, True);
  EnableCtrl(OptionIndexedCheckBox, True);
  EnableCtrl(OptionRequiredCheckBox, True);
  EnableCtrl(OptionUniqueCheckBox, True);
  EnableCtrl(OptionPrimaryKeyCheckBox, CanPk);
  EnableCtrl(OptionIsDescriptionCheckBox, IsString);
  EnableCtrl(OptionUseNullCheckBox, not IsContainer);
  EnableCtrl(IndexNameLabel, OptionIndexedCheckBox.Checked);
  EnableCtrl(IndexNameEdit, OptionIndexedCheckBox.Checked);
  EnableCtrl(OkButton, IsValid);
  PresentationSheet.TabVisible := IsMaskable;
end;

procedure TInstantAttributeEditorForm.StorageKindEditChange(Sender: TObject);
begin
  if StorageKindEdit.Text <> '' then
    SubjectExposer.AssignFieldValue(StorageKindEdit.Field,
      StorageKindEdit.Text);
  UpdateControls;
  ComputeExternalStorageName;
end;

procedure TInstantAttributeEditorForm.EnumeratedTypeEditChange(Sender: TObject);
begin
  inherited;
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.EnumeratedTypeEditEnter(Sender: TObject);
begin
  inherited;
  LoadEnum;
end;

procedure TInstantAttributeEditorForm.ExternalStorageNameEditChange(Sender:
  TObject);
begin
  if ExternalStorageNameEdit.Text <> '' then
    SubjectExposer.AssignFieldValue(ExternalStorageNameEdit.Field,
      ExternalStorageNameEdit.Text);
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.ForeignKeyFieldsEditChange(
  Sender: TObject);
begin
  inherited;
  if ForeignKeyFieldsEdit.Text <> '' then
    SubjectExposer.AssignFieldValue(ForeignKeyFieldsEdit.Field,
      ForeignKeyFieldsEdit.Text);
  UpdateControls;
end;


procedure
  TInstantAttributeEditorForm.AutoExternalStorageNameCheckBoxClick(Sender:
  TObject);
begin
  if AutoExternalStorageNameCheckBox.Checked then
    ComputeExternalStorageName
  else
    ExternalStorageNameEdit.Text := '';
end;

procedure TInstantAttributeEditorForm.ComputeForeignKeyFields;
begin
  if ForeignKeyFieldsEdit.Enabled then
  begin
    if (Subject.ForeignKeyFields <> '') then
      ForeignKeyFieldsEdit.Text := Subject.ForeignKeyFields;
  end
  else
    ForeignKeyFieldsEdit.Text := '';
end;

procedure TInstantAttributeEditorForm.ComputeExternalStorageName;

  function GetStorageName: string;
  begin
    if StorageNameEdit.Text <> '' then
      Result := StorageNameEdit.Text
    else
      Result := NameEdit.Text;
  end;

begin
  if ExternalStorageNameEdit.Enabled then
  begin
    if (Subject.ExternalStorageName <> '') and
      not AutoExternalStorageNameCheckBox.Checked then
      ExternalStorageNameEdit.Text := Subject.ExternalStorageName
    else
      if (ExternalStorageNameEdit.Text = '') or
        AutoExternalStorageNameCheckBox.Checked then
        ExternalStorageNameEdit.Text :=
          Format('%s_%s', [BaseClassStorageName, GetStorageName()]);
  end
  else
    ExternalStorageNameEdit.Text := '';
end;

function TInstantAttributeEditorForm.IsClassPersistent(
  const AClassName: String): Boolean;
var
  CM: TInstantClassMetadata;
begin
  Result := False;

  if Assigned(FOnIsClassPersistent) then
    OnIsClassPersistent(Self, AClassName, Result)
  else
  begin
    CM := InstantGetClassMetadata(AClassName);
    if Assigned(CM) then
      Result := CM.IsStored;
  end;
end;

procedure TInstantAttributeEditorForm.OptionIndexedCheckBoxClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.StorageNameEditChange(Sender: TObject);
begin
  inherited;
  if Assigned(StorageNameEdit.DataSource) then
    SubjectExposer.AssignFieldValue(StorageNameEdit.Field, StorageNameEdit.Text);
  UpdateControls;
end;

procedure TInstantAttributeEditorForm.SetValidator(const Value: TInstantAttributeValidator);
begin
  if FValidator <> Value then begin
    if FValidator <> nil then
      FreeAndNil(FValidator);
    FValidator := Value;
  end;
end;

{ TInstantAttributeValidator }

constructor TInstantAttributeValidator.Create(
  AnAttribute: TInstantCodeAttribute; const AInterface: IInterface);
begin
  inherited Create;
  FAttribute := AnAttribute;
  FInterface := AInterface;
end;

function TInstantAttributeValidator.Validate: Boolean;
var
  TempAttribute: TInstantCodeAttribute;
  CodeClass: TInstantCodeClass;
  I: Integer;

  procedure CheckChildClass(CurrentClass: TInstantCodeClass);
  var
    J,K: Integer;
  begin
    if CurrentClass = nil then
      Exit;
    for J := 0 to Pred(CurrentClass.SubClassCount) do
    begin
      for K := 0 to Pred(CurrentClass.SubClasses[J].PropertyCount) do
      begin
        if not Result then
          Break;
        if SameText(CurrentClass.SubClasses[J].Properties[K].Name, FAttribute.Name) then
        begin
          ErrorMsg := Format('Attribute "%s" exists in descendant class "%s"',
            [FAttribute.Name, CurrentClass.SubClasses[J].Name]);
          Result := False;  
        end;
      end;
      for K := 0 to Pred(CurrentClass.SubClasses[J].MethodCount) do
      begin
        if not Result then
          Break;
        if SameText(CurrentClass.SubClasses[J].Methods[K].Name, FAttribute.Name) then
        begin
          ErrorMsg := Format('Attribute "%s" exists as a method in descendant class "%s"',
            [FAttribute.Name, CurrentClass.SubClasses[J].Name]);
          Result := False;
        end;
      end;
      CheckChildClass(CurrentClass.SubClasses[J]);
    end;
  end;

begin
  Result := True;
  ErrorMsg := '';

  if not Assigned(FAttribute) and not Assigned(FAttribute.Owner)
    and not Assigned(FAttribute.Owner.Owner) then
    begin
      ErrorMsg := 'Cannot validate attribute';
      Result := False;
      Exit;
    end;
  // check that the same attribute name is not used in an ancestor class
  CodeClass := FAttribute.Owner.Owner.BaseClass;
  while (Result) and (CodeClass <> nil) do
  begin
    for I := 0 to Pred(CodeClass.PropertyCount) do
    begin
      if not Result then
        Break;
      if SameText(CodeClass.Properties[I].Name, FAttribute.Name) then
        begin
          ErrorMsg := Format('Attribute "%s" exists in ancestor class "%s"',
            [FAttribute.Name, CodeClass.Name]);
          Result := False;
        end;
    end;
    for I := 0 to Pred(CodeClass.MethodCount) do
    begin
      if not Result then
        Break;
      if SameText(CodeClass.Methods[I].Name, FAttribute.Name) then
      begin
        ErrorMsg := Format('Attribute "%s" exists as a method in ancestor class "%s"',
          [FAttribute.Name, CodeClass.Name]);
        Result := False;  
      end;
    end;
    CodeClass := CodeClass.BaseClass;
  end;
  // check that the same attribute name is not used in any child class
  if Result then
    CheckChildClass(FAttribute.Owner.Owner);

  if Result and Assigned(FAttribute.Owner) then
    for I := 0 to Pred(FAttribute.Owner.AttributeCount) do
    begin
      if not Result then
        Break;
      TempAttribute := FAttribute.Owner.Attributes[I];
      if (TempAttribute <> FAttribute) and SameText(TempAttribute.Name, FAttribute.Name) then
      begin
        ErrorMsg := 'Attribute Name already used';
        Result := False;
      end;
    end;
end;


end.

