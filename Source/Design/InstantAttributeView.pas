(*
 *   InstantObjects
 *   Attribute View Frame
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
 * David Moorhouse, Carlo Barazzetta, Adrea Petrelli, Steven Mitchell,
 * Nando Dessena, David Taylor, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantAttributeView;

{$I '..\InstantDefines.inc'}

interface

uses
  System.SysUtils
  , System.Classes
  , Data.DB
  , System.Contnrs
  , InstantPresentation
  , InstantPersistence
  , InstantCode
  , InstantEdit
  , WinApi.Windows
  , WinApi.Messages
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ComCtrls
  , Vcl.ExtCtrls
  , Vcl.Mask
  , Vcl.DBCtrls
  , Vcl.ImgList
  , Vcl.ActnList
  , Vcl.Menus
  , Vcl.Buttons
  , System.Actions
  , System.ImageList
  ;

type
  TInstantAttributeViewFrame = class(TFrame)
    SubjectSource: TDataSource;
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
    AttributesSplitter: TSplitter;
    InheritedAttributesPanel: TPanel;
    InheritedAttributesLabel: TLabel;
    InheritedAttributesView: TListView;
    IntroducedAttributesPanel: TPanel;
    IntroducedAttributesView: TListView;
    IntroducedAttributesLabel: TLabel;
    procedure AttributeNewActionExecute(Sender: TObject);
    procedure AttributeDeleteActionExecute(Sender: TObject);
    procedure AttributeEditActionExecute(Sender: TObject);
    procedure IntroducedAttributesViewDblClick(Sender: TObject);
    procedure IntroducedAttributesViewEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure SubjectExposerAfterPostField(Sender: TObject; Field: TField);
    procedure AttributesMenuPopup(Sender: TObject);
  private
    FSubject: TInstantCodeClass;
    FBackupAttributes: TObjectList;
    FChangedAttributes: TStringList;
    FNewAttributes: TList;
    FModel: TInstantCodeModel;
    FNameAttribute: TInstantCodeAttribute;
    FWasAccepted: Boolean;
    FOldSubjectName: string;
    procedure DeleteAttribute(Attribute: TInstantCodeAttribute);
    procedure FitColumns(View: TListView);
    function GetNameAttribute: TInstantCodeAttribute;
    procedure LoadAttributeView(View: TListView; AClass: TInstantCodeClass;
      Recursive: Boolean);
    procedure SetModel(const Value: TInstantCodeModel);
    procedure SetSubject(const Value: TInstantCodeClass);
    function GetFocusedAttribute: TInstantCodeAttribute;
  protected
    property FocusedAttribute: TInstantCodeAttribute read GetFocusedAttribute;
    property NameAttribute: TInstantCodeAttribute read GetNameAttribute;
    function AddAttributeToView(View: TListView;
      Attribute: TInstantCodeAttribute): TListItem;
    function EditAttribute(Attribute: TInstantCodeAttribute;
      Exists: Boolean; const Title: string = ''): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure PopulateInheritedAttributes;
    procedure PopulateIntroducedAttributes;
    procedure RestoreAttributes;
    procedure UpdateActions;
    procedure RestoreLayout;
    procedure StoreLayout;
    property BackupAttributes: TObjectList read FBackupAttributes;
    property ChangedAttributes: TStringList read FChangedAttributes;
    property Model: TInstantCodeModel read FModel write SetModel;
    property NewAttributes: TList read FNewAttributes;
    property Subject: TInstantCodeClass read FSubject write SetSubject;

    property WasAccepted: Boolean read FWasAccepted;
    property OldSubjectName: string read FOldSubjectName;
  end;

implementation

uses
  InstantAttributeEditor, InstantDesignUtils, InstantConsts, InstantRtti,
  System.TypInfo
  , InstantImageUtils
  , InstantTypes
  , System.Win.Registry
  ;

{$R *.dfm}

resourcestring
  SConfirmDeleteAttribute = 'Delete attribute ''%s''?';

{ TInstantAttributeViewFrame }

function TInstantAttributeViewFrame.AddAttributeToView(View: TListView;
  Attribute: TInstantCodeAttribute): TListItem;
begin
  Result := View.Items.Add;
  with Result do
  begin
    with Attribute do
      if (HostClass = Subject) or not Assigned(HostClass) then
        Caption := Name else
        Caption := HostClass.Name + '.' + Name;
    Data := Attribute;
    //Add Attribute Type
    SubItems.Add(Attribute.AttributeTypeText);
    //Add StorageName or ExternalStorageName
    if Attribute.CanBeExternal and not Attribute.CanHaveStorageName then
      SubItems.Add(Attribute.ExternalStorageName)
    else
      SubItems.Add(Attribute.StorageName);
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

procedure TInstantAttributeViewFrame.AttributeDeleteActionExecute(
  Sender: TObject);
var
  Attribute: TInstantCodeAttribute;
begin
  FWasAccepted := False;
  FOldSubjectName := Subject.Name;

  with IntroducedAttributesView do
    if Assigned(ItemFocused) then
    begin
      Attribute := ItemFocused.Data;
      if not Confirm(Format(SConfirmDeleteAttribute, [Attribute.Name])) then
        Exit;

      FWasAccepted := True;

      DeleteAttribute(Attribute);
      ItemFocused.Delete;
      if Assigned(ItemFocused) then
        ItemFocused.Selected := True;
      FitColumns(IntroducedAttributesView);
    end;
end;

procedure TInstantAttributeViewFrame.AttributeEditActionExecute(Sender: TObject);
var
  OldName: string;
  Attribute: TInstantCodeAttribute;
  Exists: Boolean;
begin
  FWasAccepted := False;
  FOldSubjectName := Subject.Name;

  Attribute := FocusedAttribute;
  if not Assigned(Attribute) then
    Exit;
  OldName := Attribute.Name;
  Exists := FNewAttributes.IndexOf(Attribute) = -1;
  if Exists then
    Attribute.DetectMethodTypes;
  if EditAttribute(Attribute, Exists) then
  begin
    FWasAccepted := True;

    if Exists and (FChangedAttributes.IndexOfObject(Attribute) = -1) then
      FChangedAttributes.AddObject(OldName, Attribute);
    PopulateIntroducedAttributes;
  end;
end;

procedure TInstantAttributeViewFrame.AttributeNewActionExecute(Sender: TObject);
var
  Attribute: TInstantCodeAttribute;
  NewItem: TListItem;
begin
  FWasAccepted := False;
  FOldSubjectName := Subject.Name;

  Attribute := Subject.AddAttribute;
  if not EditAttribute(Attribute, False, 'New Attribute') then
    Attribute.Free
  else begin
    FWasAccepted := True;

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
      NewItem.MakeVisible(False);
    end;
  end;
end;

procedure TInstantAttributeViewFrame.AttributesMenuPopup(Sender: TObject);
begin
  UpdateActions;
end;

procedure TInstantAttributeViewFrame.Clear;
begin
  InheritedAttributesView.Clear;
  IntroducedAttributesView.Clear;
  FChangedAttributes.Clear;
  FNewAttributes.Clear;
end;

constructor TInstantAttributeViewFrame.Create(AOwner: TComponent);
begin
  inherited;
  FBackupAttributes := TObjectList.Create;
  FChangedAttributes := TStringList.Create;
  FNewAttributes := TList.Create;
  LoadMultipleImages(AttributeImages, 'IO_CLASSEDITORATTRIBUTEIMAGES', HInstance);
  IntroducedAttributesView.SmallImages := AttributeImages;
  InheritedAttributesView.SmallImages := AttributeImages;
end;

procedure TInstantAttributeViewFrame.DeleteAttribute(
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

destructor TInstantAttributeViewFrame.Destroy;
begin
  FNewAttributes.Free;
  FChangedAttributes.Free;
  FBackupAttributes.Free;
  FNameAttribute.Free;
  inherited;
end;

function TInstantAttributeViewFrame.EditAttribute(Attribute: TInstantCodeAttribute;
  Exists: Boolean; const Title: string): Boolean;

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

procedure TInstantAttributeViewFrame.FitColumns(View: TListView);
var
  i : integer;
begin
  //adjust Columns size to window width
  for i := View.Columns.Count-1 downto 0 do
  begin
    View.Columns[i].AutoSize := True;
  end;
end;

function TInstantAttributeViewFrame.GetFocusedAttribute: TInstantCodeAttribute;
begin
  with IntroducedAttributesView do
    if Assigned(ItemFocused) then
      Result := ItemFocused.Data
    else
      Result := nil;
end;

function TInstantAttributeViewFrame.GetNameAttribute: TInstantCodeAttribute;
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

procedure TInstantAttributeViewFrame.IntroducedAttributesViewDblClick(
  Sender: TObject);
begin
  AttributeEditAction.Execute;
end;

procedure TInstantAttributeViewFrame.IntroducedAttributesViewEdited(
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

procedure TInstantAttributeViewFrame.LoadAttributeView(View: TListView;
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
        FitColumns(View);
      end;
    end;
  end;
end;

procedure TInstantAttributeViewFrame.PopulateInheritedAttributes;
begin
  if (assigned(Subject)) and (assigned(Subject.BaseClass)) then
    LoadAttributeView(InheritedAttributesView, Subject.BaseClass, True);
end;

procedure TInstantAttributeViewFrame.PopulateIntroducedAttributes;
begin
  if (assigned(Subject)) then
    LoadAttributeView(IntroducedAttributesView, Subject, False);
end;

procedure TInstantAttributeViewFrame.RestoreLayout;
begin
  with TRegistry.Create do try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey('Software\InstantObjects.org\Layout', False) then begin
      if not ReadBool('Default') then Exit;
      InheritedAttributesPanel.Height := ReadInteger('Splitter');
    end;
  finally
    Free;
  end;
end;

procedure TInstantAttributeViewFrame.SetModel(const Value: TInstantCodeModel);
begin
  if Value <> FModel then
  begin
    FModel := Value;
  end;
end;

procedure TInstantAttributeViewFrame.SetSubject(const Value: TInstantCodeClass);
begin
  if Value <> Subject then
  begin
    FSubject := Value;
    if Subject <> nil then
      Subject.CloneAttributes(FBackupAttributes);
    PopulateIntroducedAttributes;
    PopulateInheritedAttributes;
  end;
  UpdateActions;
end;

procedure TInstantAttributeViewFrame.StoreLayout;
begin
  with TRegistry.Create do try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey('Software\InstantObjects.org\Layout', True) then begin
      WriteInteger('Splitter', InheritedAttributesPanel.Height);
    end;
  finally
    Free;
  end;
end;

procedure TInstantAttributeViewFrame.SubjectExposerAfterPostField(
  Sender: TObject; Field: TField);
begin
  if Field.FieldName = 'BaseClassName' then
  begin
    FreeAndNil(FNameAttribute);
    PopulateInheritedAttributes;
  end;
end;

procedure TInstantAttributeViewFrame.UpdateActions;
var
  Attribute: TInstantCodeAttribute;
begin
  inherited;
  Attribute := FocusedAttribute;
  AttributeNewItem.Enabled := Assigned(Subject);
  AttributeEditAction.Enabled := Assigned(Attribute);
  AttributeDeleteAction.Enabled := Assigned(Attribute);
end;

procedure TInstantAttributeViewFrame.RestoreAttributes;
begin
  Subject.AssignAttributes(FBackupAttributes);
end;


end.
