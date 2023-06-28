(*
 *   InstantObjects
 *   Model Explorer
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
 * Brian Andersen, David Moorhouse, David Taylor
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantModelExplorer;

{$I '..\InstantDefines.inc'}

interface

uses
  SysUtils, Classes, IniFiles,
  Windows, Messages, ExtCtrls, ComCtrls, ImgList, DockForm,
  ToolWin, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ActnList,
  InstantCode, InstantAttributeView, System.Actions, System.ImageList,
  Vcl.Samples.Spin;

type
  TInstantModelStyle = (msInheritance, msRelations);

  TInstantGotoSourceEvent = procedure(Sender: TObject; const FileName: string;
    Pos: TInstantCodePos) of object;

  TInstantModelError = class(TObject)
  private
    FFileName: string;
    FPosition: TInstantCodePos;
    FText: string;
  public
    property FileName: string read FFileName write FFileName;
    property Position: TInstantCodePos read FPosition write FPosition;
    property Text: string read FText write FText;
  end;

  TModelTreeView = class(TTreeView)
  private
    FOnNodeDblClick: TNotifyEvent;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
  protected
    procedure DoNodeDblClick(Node: TTreeNode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnNodeDblClick: TNotifyEvent read FOnNodeDblClick write FOnNodeDblClick;
  end;

  TInstantModelExplorerForm = class(TDockableForm)
    AboutAction: TAction;
    AboutItem: TMenuItem;
    ActionImages: TImageList;
    Actions: TActionList;
    AttributeImages: TImageList;
    BuildDatabaseAction: TAction;
    BuildDatabaseButton: TToolButton;
    CollapseAllAction: TAction;
    CollapseAllItem: TMenuItem;
    DeleteClassAction: TAction;
    DeleteClassItem: TMenuItem;
    EditClassAction: TAction;
    EditClassItem: TMenuItem;
    ExpandAllAction: TAction;
    ExpandAllItem: TMenuItem;
    ExportModelAction: TAction;
    ExportModelItem: TMenuItem;
    ModelImages: TImageList;
    ModelPanel: TPanel;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    NewClassAction: TAction;
    NewClassItem: TMenuItem;
    RefreshAction: TAction;
    RefreshItem: TMenuItem;
    SelectUnitsAction: TAction;
    SelectUnitsButton: TToolButton;
    ToolBar: TToolBar;
    ToolSep1: TToolButton;
    TreeMenu: TPopupMenu;
    ViewButton: TToolButton;
    ViewInheritanceAction: TAction;
    ViewRelationsAction: TAction;
    ViewSourceAction: TAction;
    ViewSourceItem: TMenuItem;
    ImportModelItem: TMenuItem;
    ImportModelAction: TAction;
    AttributePanel: TPanel;
    AttributeSplitter: TSplitter;
    AttributeCaptionPanel: TPanel;
    AttributeCaptionLabel: TLabel;
    ToolSep2: TToolButton;
    ViewAttributeButton: TToolButton;
    ViewAttributesAction: TAction;
    ViewAttributes: TMenuItem;
    InstantAttributeViewFrame: TInstantAttributeViewFrame;
    TopPanel: TPanel;
    cbEnableModelUpdate: TCheckBox;
    edInterval: TSpinEdit;
    procedure AboutActionExecute(Sender: TObject);
    procedure BuildDatabaseActionExecute(Sender: TObject);
    procedure CollapseAllActionExecute(Sender: TObject);
    procedure DeleteClassActionExecute(Sender: TObject);
    procedure EditClassActionExecute(Sender: TObject);
    procedure ExpandAllActionExecute(Sender: TObject);
    procedure ExportModelActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ModelViewChange(Sender: TObject; Node: TTreeNode);
    procedure ModelViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ModelViewNodeDblClick(Sender: TObject);
    procedure NewClassActionExecute(Sender: TObject);
    procedure RefreshActionExecute(Sender: TObject);
    procedure SelectUnitsActionExecute(Sender: TObject);
    procedure TreeMenuPopup(Sender: TObject);
    procedure ViewAttributesActionExecute(Sender: TObject);
    procedure ViewInheritanceActionExecute(Sender: TObject);
    procedure ViewRelationsActionExecute(Sender: TObject);
    procedure ViewSourceActionExecute(Sender: TObject);
    procedure ImportModelActionExecute(Sender: TObject);
    procedure InstantAttributeViewFrameAttributeNewActionExecute(
      Sender: TObject);
    procedure InstantAttributeViewFrameAttributeDeleteActionExecute(
      Sender: TObject);
    procedure InstantAttributeViewFrameAttributeEditActionExecute(
      Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbEnableModelUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edIntervalChange(Sender: TObject);
  private
    FError: TInstantModelError;
    FModel: TInstantCodeModel;
    FModelView: TModelTreeView;
//    FAttributeFrame: TInstantAttributeViewFrame;
    FSelectedNode: TTreeNode;
    FStyle: TInstantModelStyle;
    FOnApplyClass: TInstantCodeClassApplyEvent;
    FOnGotoSource: TInstantGotoSourceEvent;
    FOnLoadModel: TInstantCodeModelEvent;
    function GetFocusedClass: TInstantCodeClass;
    function GetSelectedNode: TTreeNode;
    procedure SetError(E: Exception);
    procedure SetStyle(const Value: TInstantModelStyle);
    procedure ViewClassAttributes(AClass: TInstantCodeClass);
    procedure SetAttributePanelVisible(Visible: Boolean);
    procedure RestoreLayout;
    procedure StoreLayout;
    procedure RealignForm;
    procedure UpdateModelExpertTimer;
    function GetModelView: TModelTreeView;
  protected
    procedure ApplyClass(AClass: TInstantCodeClass;
      ChangeType: TInstantCodeChangeType; OldName: string = '';
      ChangedAttributes: TStringList = nil; NewAttributes: TList = nil);
    procedure ApplyClassFromView(AView: TInstantAttributeViewFrame);
    function ClassFromNode(Node: TTreeNode): TInstantCodeClass;
    procedure DoApplyClass(AClass: TInstantCodeClass;
      ChangeInfo: TInstantCodeClassChangeInfo);
    function EditClass(AClass: TInstantCodeClass; New: Boolean): Boolean;
    procedure GotoNodeSource(Node: TTreeNode);
    procedure GotoSource(const FileName: string; Pos: TInstantCodePos);
    procedure LoadModel;
    procedure UpdateActions; override;
    procedure RefreshAttributeView;
    property SelectedNode: TTreeNode read GetSelectedNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CompileTo(FileName: string);
    procedure Refresh;
    procedure UpdateModel;
    property FocusedClass: TInstantCodeClass read GetFocusedClass;
    property Model: TInstantCodeModel read FModel;
    property ModelView: TModelTreeView read GetModelView;
    property Style: TInstantModelStyle read FStyle write SetStyle;
    property OnApplyClass: TInstantCodeClassApplyEvent read FOnApplyClass write FOnApplyClass;
    property OnGotoSource: TInstantGotoSourceEvent read FOnGotoSource write FOnGotoSource;
    property OnLoadModel: TInstantCodeModelEvent read FOnLoadModel write FOnLoadModel;
  end;

var
  ModelExplorer: TInstantModelExplorerForm;

implementation

uses
  TypInfo, InstantClassEditor, InstantClasses, DeskUtil,
  InstantModelExpert,
  InstantDesignUtils, InstantPersistence, InstantDesignHook, InstantAbout,
  InstantImageUtils, InstantMetadata, InstantModelImport, Registry,
  Vcl.Themes, ToolsAPI, BrandingAPI
  {$IF (CompilerVersion >= 32.0)}, IDETheme.Utils{$IFEND};

resourcestring
  SDeleteClass = 'Delete Class ''%s''?';
  SModelExt = '.xml';
  SModelFilter = 'XML files (*.xml)|*.xml';

{$R *.dfm}

{ TModelTreeView }

constructor TModelTreeView.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TModelTreeView.DoNodeDblClick(Node: TTreeNode);
begin
  if Assigned(FOnNodeDblClick) then
    FOnNodeDblClick(Node);
end;

procedure TModelTreeView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
    if htOnLabel in GetHitTestInfoAt(XPos, YPos) then
      DoNodeDblClick(GetNodeAt(XPos, YPos))
    else
      inherited;
end;

{ TInstantModelExplorerForm }

procedure TInstantModelExplorerForm.AboutActionExecute(Sender: TObject);
begin
  with TInstantAboutForm.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TInstantModelExplorerForm.ApplyClass(AClass: TInstantCodeClass;
  ChangeType: TInstantCodeChangeType; OldName: string;
  ChangedAttributes: TStringList; NewAttributes: TList);
var
  ChangeInfo: TInstantCodeClassChangeInfo;
begin
  ChangeInfo := TInstantCodeClassChangeInfo.Create(AClass, ChangeType,
    OldName, ChangedAttributes, NewAttributes);
  try
    DoApplyClass(AClass, ChangeInfo);
  finally
    ChangeInfo.Free;
  end;
end;

procedure TInstantModelExplorerForm.ApplyClassFromView(
  AView: TInstantAttributeViewFrame);
begin
  if InstantAttributeViewFrame.WasAccepted then
    ApplyClass(FocusedClass, ctEdit, InstantAttributeViewFrame.OldSubjectName,
      InstantAttributeViewFrame.ChangedAttributes,
      InstantAttributeViewFrame.NewAttributes);
end;

procedure TInstantModelExplorerForm.BuildDatabaseActionExecute(
  Sender: TObject);
begin
  if Assigned(ModelExpert) then
    ModelExpert.BuildDatabase(Model);
end;

procedure TInstantModelExplorerForm.UpdateModelExpertTimer;
begin
  if Assigned(ModelExpert) then
  begin
    ModelExpert.TimerDisabled := not cbEnableModelUpdate.Checked;
    ModelExpert.TimerInterval := edInterval.Value * 1000;
  end;
end;

procedure TInstantModelExplorerForm.cbEnableModelUpdateClick(Sender: TObject);
begin
  UpdateModelExpertTimer;
  if cbEnableModelUpdate.Checked then
    ModelExpert.UpdateModel;
end;

function TInstantModelExplorerForm.ClassFromNode(
  Node: TTreeNode): TInstantCodeClass;
var
  AObject: TObject;
begin
  if Assigned(Node) then
  begin
    AObject := TObject(Node.Data);
    if AObject is TInstantCodeClass then
    begin
      Result := TInstantCodeClass(AObject);
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TInstantModelExplorerForm.CollapseAllActionExecute(
  Sender: TObject);
begin
  if Assigned(SelectedNode) then
    SelectedNode.Collapse(True);
end;

procedure TInstantModelExplorerForm.CompileTo(FileName: string);
begin
  FModel.SaveToResFile(FileName);
end;

constructor TInstantModelExplorerForm.Create(AOwner: TComponent);
begin
  inherited;
  Font.Assign(Screen.IconFont);

  LoadMultipleImages(ActionImages, 'IO_MODELEXPLORERACTIONIMAGES', HInstance);
  LoadMultipleImages(ModelImages, 'IO_MODELEXPLORERMODELIMAGES', HInstance);
  LoadMultipleImages(AttributeImages, 'IO_MODELEXPLORERATTRIBUTEIMAGES', HInstance);

//  FAttributeFrame := TInstantAttributeViewFrame.Create(Self);
//  with FAttributeFrame do
//  begin
//    Parent := AttributePanel;
//    Align := alClient;
//  end;

  FModel := TInstantCodeModel.Create;
  DesignModel := @FModel;
  DeskSection := 'InstantModelExplorer';
  AutoSave := True;
  ModelExplorer := Self;
  SetAttributePanelVisible(True);
  RestoreLayout;
  Refresh;
end;

procedure TInstantModelExplorerForm.DeleteClassActionExecute(Sender: TObject);
var
  AClass: TInstantCodeClass;
begin
  AClass := FocusedClass;
  if Assigned(AClass) and Confirm(Format(SDeleteClass, [AClass.Name])) then
  begin
    ApplyClass(AClass, ctDelete);
    AClass.Free;
    UpdateModel;
  end;
end;

destructor TInstantModelExplorerForm.Destroy;
begin
  StoreLayout;
  ModelExplorer := nil;
  FModel.Free;
  FError.Free;
  inherited;
end;

procedure TInstantModelExplorerForm.DoApplyClass(AClass: TInstantCodeClass;
  ChangeInfo: TInstantCodeClassChangeInfo);
begin
  if Assigned(FOnApplyClass) then
    FOnApplyClass(Self, AClass, ChangeInfo);
end;

procedure TInstantModelExplorerForm.edIntervalChange(Sender: TObject);
begin
  UpdateModelExpertTimer;
end;

function TInstantModelExplorerForm.EditClass(AClass: TInstantCodeClass;
  New: Boolean): Boolean;
const
  ChangeTypes: array[Boolean] of TInstantCodeChangeType = (ctEdit, ctNew);
var
  OldName: string;
  EditorForm: TInstantClassEditorForm;
begin
  OldName := AClass.Name;
  EditorForm := TInstantClassEditorForm.Create(nil);
  try
    EditorForm.IsNew := New;
    EditorForm.Model := Self.Model;
    EditorForm.Subject := AClass;
    Result := EditorForm.ShowModal = mrOk;
    if Result then
      ApplyClass(AClass, ChangeTypes[New], OldName,
//        FAttributeFrame.ChangedAttributes,
//        FAttributeFrame.NewAttributes);
        EditorForm.ChangedAttributes,
        EditorForm.NewAttributes);
  finally
    EditorForm.Free;
  end;
end;

procedure TInstantModelExplorerForm.EditClassActionExecute(Sender: TObject);
begin
  if EditClass(FocusedClass, False) then
    Refresh;
end;

procedure TInstantModelExplorerForm.ExpandAllActionExecute(
  Sender: TObject);
begin
  if Assigned(SelectedNode) then
    SelectedNode.Expand(True);
end;

procedure TInstantModelExplorerForm.ImportModelActionExecute(Sender: TObject);
var
  ClassIndex, AttributeIndex: Integer;
  ImportModule: TInstantCodeModule;
  ImportFileName: string;
  ImportFileType: TInstantStreamFormat;
  ImportModel: TInstantModel;
  ImportClassMetadata: TInstantClassMetadata;
  ImportBaseClassName: string;
  ImportAttributeMetadata: TInstantAttributeMetadata;
  NewClasses: TInstantCodeClassList;
  NewClass: TInstantCodeClass;
  NewAttribute: TInstantCodeAttribute;
begin
  with TInstantModelImportForm.Create(nil) do
    try
      if Execute(FModel) then
      begin
        ImportModule := SelectedModule;
        ImportFileName := SelectedFileName;
        ImportFileType := SelectedFileType;
      end else
        Exit;
    finally
      Free;
    end;

  ImportModel := TInstantModel.Create;
  try
    if ImportFileType = sfBinary then
      ImportModel.LoadFromResFile(ImportFileName) else
      ImportModel.LoadFromFile(ImportFileName);

    ModelView.Items.BeginUpdate;
    try
      NewClasses := TInstantCodeClassList.Create;
      try
        for ClassIndex := 0 to ImportModel.ClassMetadatas.Count - 1 do
        begin
          ImportClassMetadata := ImportModel.ClassMetadatas[ClassIndex];

          NewClass := ImportModule.InterfaceSection.AddClass;
          ImportBaseClassName := ImportClassMetadata.ParentName;
          if ImportBaseClassName = '' then
            ImportBaseClassName := TInstantObject.ClassName;
          NewClass.BaseClassName := ImportBaseClassName;
          NewClass.Name := ImportClassMetadata.Name;
          NewClass.Metadata.Assign(ImportClassMetadata);

          for AttributeIndex := 0 to ImportClassMetadata.AttributeMetadatas.Count - 1 do
          begin
            ImportAttributeMetadata := ImportClassMetadata.AttributeMetadatas[AttributeIndex];

            NewAttribute := NewClass.AddAttribute;
            NewAttribute.IsIndexed := ImportAttributeMetadata.IsIndexed;
            NewAttribute.IsRequired := ImportAttributeMetadata.IsRequired;
            NewAttribute.IsUnique := ImportAttributeMetadata.IsUnique;
            NewAttribute.IsDefault := ImportAttributeMetadata.IsDefault;
            NewAttribute.AttributeType := ImportAttributeMetadata.AttributeType;
            NewAttribute.AttributeTypeName := ImportAttributeMetadata.AttributeTypeName;
            NewAttribute.Name := ImportAttributeMetadata.FieldName;
            NewAttribute.StorageKind := ImportAttributeMetadata.StorageKind;
            NewAttribute.StorageName := ImportAttributeMetadata.StorageName;
            NewAttribute.ObjectClassName := ImportAttributeMetadata.ObjectClassName;
            NewAttribute.Realize;
          end;
          NewClasses.Add(NewClass)
        end;

        // Classes needs to be sorted with base classes first or else the code
        // generation might not be done correct.
        NewClasses.SortByBaseClass;

        for ClassIndex := 0 to NewClasses.Count - 1 do
          ApplyClass(NewClasses[ClassIndex], ctNew, '');
      finally
        NewClasses.Free;
      end;
    finally
      ModelView.Items.EndUpdate;
    end;
  finally
    ImportModel.Free;
  end;

  Refresh;
end;

procedure TInstantModelExplorerForm.InstantAttributeViewFrameAttributeDeleteActionExecute(
  Sender: TObject);
begin
  InstantAttributeViewFrame.AttributeDeleteActionExecute(Sender);
  ApplyClassFromView(InstantAttributeViewFrame);
end;

procedure TInstantModelExplorerForm.InstantAttributeViewFrameAttributeEditActionExecute(
  Sender: TObject);
begin
  InstantAttributeViewFrame.AttributeEditActionExecute(Sender);
  ApplyClassFromView(InstantAttributeViewFrame);
end;

procedure TInstantModelExplorerForm.InstantAttributeViewFrameAttributeNewActionExecute(
  Sender: TObject);
//var
//  AClass: TInstantCodeClass;
//  OldName: string;
begin
//  OldName := AClass.Name;
  InstantAttributeViewFrame.AttributeNewActionExecute(Sender);
  ApplyClassFromView(InstantAttributeViewFrame);
end;

procedure TInstantModelExplorerForm.ExportModelActionExecute(
  Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    Filter := SModelFilter;
    DefaultExt := SModelExt;
    if Execute then
      DesignModel.Model.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TInstantModelExplorerForm.RealignForm;
begin
  AttributePanel.Height := Height div 2;
  InstantAttributeViewFrame.InheritedAttributesPanel.height :=
    AttributePanel.Height div 2 - AttributeCaptionPanel.Height;
end;

procedure TInstantModelExplorerForm.FormCreate(Sender: TObject);
{$IF (CompilerVersion >= 32.0)}
var
  LStyle: TCustomStyleServices;
{$IFEND}
begin
  inherited;
{$IF (CompilerVersion >= 32.0)}
  {$IF (CompilerVersion <= 34.0)}
  if UseThemeFont then
    Self.Font.Assign(GetThemeFont);
  {$IFEND}
  {$IF CompilerVersion > 34.0}
  if TIDEThemeMetrics.Font.Enabled then
    Self.Font.Assign(TIDEThemeMetrics.Font.GetFont);
  {$IFEND}

  if ThemeProperties <> nil then
  begin
    LStyle := ThemeProperties.StyleServices;
    StyleElements := StyleElements - [seClient];
    Color := LStyle.GetSystemColor(clWindow);
    TopPanel.StyleElements := TopPanel.StyleElements - [seClient];
    TopPanel.ParentBackground := False;
    TopPanel.Color := LStyle.GetSystemColor(clBtnFace);
    ModelView.StyleElements := FModelView.StyleElements - [seClient];
    //ModelView.ParentBackground := False;
    ModelView.Color := LStyle.GetSystemColor(clWindow);
    IDEThemeManager.RegisterFormClass(TInstantModelExplorerForm);
    ThemeProperties.ApplyTheme(Self);
  end;
{$IFEND}
end;

procedure TInstantModelExplorerForm.FormResize(Sender: TObject);
begin
  RealignForm;
end;

procedure TInstantModelExplorerForm.FormShow(Sender: TObject);
begin
  if height < 600 then
    height := 600;
  ModelView.Repaint; // Avoid wrong icon for first node
  RealignForm;
end;

function TInstantModelExplorerForm.GetFocusedClass: TInstantCodeClass;
begin
  Result := ClassFromNode(SelectedNode);
end;

function TInstantModelExplorerForm.GetModelView: TModelTreeView;
begin
  if not Assigned(FModelView) then
  begin
    FModelView := TModelTreeView.Create(Self);
    with FModelView do
    begin
      Align := alClient;
      Parent := Self;
      Images := ModelImages;
      PopupMenu := TreeMenu;
      ReadOnly := True;
      HideSelection := False;
      RightClickSelect := True;
      OnChange := ModelViewChange;
      OnNodeDblClick := ModelViewNodeDblClick;
      OnGetImageIndex := ModelViewGetImageIndex;
    end;
  end;
  Result := FModelView;
end;


function TInstantModelExplorerForm.GetSelectedNode: TTreeNode;
begin
  if Assigned(FSelectedNode) then
    Result := FSelectedNode
  else
    Result := ModelView.Selected;
end;

procedure TInstantModelExplorerForm.GotoNodeSource(Node: TTreeNode);
var
  AObject: TObject;
begin
  AObject := TObject(Node.Data);
  if AObject is TInstantCodeClass then
    with TInstantCodeClass(AObject) do
      GotoSource(Module.FileName, StartPos)
  else if AObject is TInstantModelError then
    GotoSource(FError.FileName, FError.Position);
end;

procedure TInstantModelExplorerForm.GotoSource(const FileName: string;
  Pos: TInstantCodePos);
begin
  if Assigned(FOnGotoSource) then
    FOnGotoSource(Self, FileName, Pos);
end;

procedure TInstantModelExplorerForm.LoadModel;
var
  NewModel: TInstantCodeModel;
begin
  NewModel := TInstantCodeModel.Create;
  try
    if Assigned(FOnLoadModel) then
      FOnLoadModel(Self, NewModel);
    SetError(nil);
    NewModel.AssignComponents(FModel);
    FModel.Free;
    FModel := NewModel;
    InstantAttributeViewFrame.Model := FModel;
  except
    on E: Exception do
    begin
      NewModel.Free;
      SetError(E);
    end;
  end;
end;

procedure TInstantModelExplorerForm.ModelViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  FSelectedNode := nil;
  RefreshAttributeView;
end;

procedure TInstantModelExplorerForm.ModelViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
var
  AObject: TObject;
  AClass: TInstantCodeClass;
begin
  if Assigned(Node) then
  begin
    AObject := Node.Data;
    if AObject is TInstantCodeClass then
    begin
      AClass := TInstantCodeClass(AObject);
      if AClass.IsStored then
        Node.ImageIndex := 2
      else if AClass.DerivesFrom(TInstantObject.ClassName) then
        Node.ImageIndex := 1
      else
        Node.ImageIndex := 0;
      Node.SelectedIndex := Node.ImageIndex;
    end else if AObject is TInstantModelError then
    begin
      Node.ImageIndex := 3;
      Node.SelectedIndex := 3;
    end;
  end;
end;

procedure TInstantModelExplorerForm.ModelViewNodeDblClick(Sender: TObject);
begin
  GotoNodeSource(Sender as TTreeNode);
end;

procedure TInstantModelExplorerForm.NewClassActionExecute(Sender: TObject);
var
  Module: TInstantCodeModule;
  BaseClass: TInstantCodeClass;
  BaseClassName: string;
  NewClass: TInstantCodeClass;
begin
  BaseClass := FocusedClass;
  if Assigned(BaseClass) then
  begin
    Module := BaseClass.Module;
    BaseClassName := BaseClass.Name;
  end else
  begin
    if Model.ModuleCount < 0 then
    begin
      MessageDlg('No model units have been selected', mtInformation, [mbOk], 0);
      Exit;
    end;
    Module := Model.Modules[0];
    BaseClassName := TInstantObject.ClassName;
  end;
  NewClass := Module.InterfaceSection.AddClass;
  NewClass.BaseClassName := BaseClassName;
  if Assigned(BaseClass) then
    NewClass.Persistence := BaseClass.Persistence;
  if EditClass(NewClass, True) then
  begin
    if NewClass.Module.Name <> Module.Name then
    begin
      Module.RemoveType(NewClass);
      NewClass.Module.InsertType(NewClass);
    end;
    UpdateModel;
  end
  else
    NewClass.Free
end;

procedure TInstantModelExplorerForm.Refresh;
begin
  LoadModel;
  UpdateModel;
  if Assigned(DesignModel) then
    DesignModel.Changed;
end;

procedure TInstantModelExplorerForm.RefreshActionExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TInstantModelExplorerForm.SelectUnitsActionExecute(Sender: TObject);
begin
  ModelExpert.SelectUnits;
end;

procedure TInstantModelExplorerForm.SetAttributePanelVisible(Visible: Boolean);
begin
  AttributePanel.Visible := Visible;
  AttributeSplitter.Visible := Visible;
  ViewAttributeButton.Down := Visible;
  RefreshAttributeView;
end;

procedure TInstantModelExplorerForm.SetError(E: Exception);
begin
  FreeAndNil(FError);
  if Assigned(E) then
  begin
    FError := TInstantModelError.Create;
    FError.Text := E.Message;
    if E is EInstantCodeError then
      with EInstantCodeError(E) do
      begin
        FError.FileName := FileName;
        FError.Position := Position;
      end;
  end;
end;

procedure TInstantModelExplorerForm.SetStyle(const Value: TInstantModelStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    UpdateModel;
  end;
end;

procedure TInstantModelExplorerForm.TreeMenuPopup(Sender: TObject);
begin
  FSelectedNode := ModelView.Selected;
end;

procedure TInstantModelExplorerForm.UpdateActions;
var
  AtClass, HasProject, HasModel, HasClasses: Boolean;
begin
  inherited;
  AtClass := Assigned(FocusedClass);
  HasProject := Assigned(ModelExpert) and Assigned(ModelExpert.ActiveProject);
  HasModel := Model.ModuleCount > 0;
  HasClasses := Model.ClassCount > 0;
  SelectUnitsAction.Enabled := HasProject;
  BuildDatabaseAction.Enabled := HasClasses;
  NewClassAction.Enabled := HasModel;
  ImportModelAction.Enabled := HasModel;
  ExportModelAction.Enabled := HasModel;
  EditClassAction.Enabled := AtClass;
  ViewSourceAction.Enabled := AtClass;
  DeleteClassAction.Enabled := AtClass;
  ExpandAllAction.Enabled := AtClass;
  CollapseAllAction.Enabled := AtClass;
end;

procedure TInstantModelExplorerForm.RefreshAttributeView;
  begin
    if (AttributePanel.Visible) then
      ViewClassAttributes(FocusedClass);
  end;

procedure TInstantModelExplorerForm.UpdateModel;

  function FindClassNode(Parent: TTreeNode;
    Text: string; AClass: TInstantCodeClass): TTreeNode;
  var
    Node: TTreeNode;
    Found: Boolean;
  begin
    Result := nil;
    Found := False;
    if Assigned(Parent) then
      Node := Parent.GetFirstChild else
      Node := ModelView.Items.GetFirstNode;
    while Assigned(Node) do
    begin
      case Style of
        msInheritance:
          Found := (Node.Text = Text) or (Node.Data = AClass);
        msRelations:
          Found := (Node.Text = Text) and (Node.Data = AClass);
      end;
      if Found then
      begin
        Result := Node;
        Break;
      end;
      Node := Node.GetNextSibling;
    end;
  end;

  procedure RemoveInvalidNodes(Parent: TTreeNode; ValidNodes: TList);
  var
    Node, CurrentNode: TTreeNode;
  begin
    if Assigned(Parent) then
      Node := Parent.GetFirstChild else
      Node := ModelView.Items.GetFirstNode;
    while Assigned(Node) do
    begin
      CurrentNode := Node;
      Node := Node.GetNextSibling;
      if ValidNodes.IndexOf(CurrentNode) = -1 then
        CurrentNode.Free;
    end;
  end;

  function AddClass(Node: TTreeNode; AClass: TInstantCodeClass;
    Text: string; var Level: Integer): TTreeNode;
  var
    I: Integer;
    SubNodes: TList;
    SubItems: TStringList;
    Prop: TInstantCodeProperty;
  begin
    Inc(Level);
    try
      if Text = '' then
        Text := AClass.Name else
        Text := Text + ': ' + AClass.Name;
      Result := FindClassNode(Node, Text, AClass);
      if Assigned(Result) then
      begin
        Result.Text := Text;
        Result.Data := AClass
      end else
        Result := ModelView.Items.AddChildObject(Node, Text, AClass);
      SubItems := TStringList.Create;
      SubNodes := TList.Create;
      with AClass do
      try
        case Style of
          msInheritance:
            for I := 0 to Pred(SubClassCount) do
              SubItems.AddObject('', SubClasses[I]);
          msRelations:
            if {IsStored and} (Level < 2) then
              for I := 0 to Pred(PropertyCount) do
              begin
                Prop := Properties[I];
                with Prop do
                  if TypeValue is TInstantCodeClass then
                    SubItems.AddObject(Name, TypeValue);
              end;
        end;
        for I := 0 to Pred(SubItems.Count) do
          SubNodes.Add(AddClass(Result,
            TInstantCodeClass(SubItems.Objects[I]), SubItems[I], Level));
        RemoveInvalidNodes(Result, SubNodes);
      finally
        Result.AlphaSort;
        SubNodes.Free;
        SubItems.Free;
      end;
    finally
      Dec(Level);
    end;
  end;

var
  AClass: TInstantCodeClass;
  Nodes: TList;
  FirstNode: TTreeNode;
  I: Integer;
  Level: Integer;
begin
  if Assigned(ModelExpert) then
  begin
    edInterval.Value := ModelExpert.TimerInterval div 1000;
    cbEnableModelUpdate.Checked := not ModelExpert.TimerDisabled;
  end;
  //FAttributeFrame.Clear;
  InstantAttributeViewFrame.Clear;

  Level := 0;
  FSelectedNode := nil;
  ModelView.Items.BeginUpdate;
  try
    if Assigned(FError) then
    begin
      ModelView.Items.Clear;
      ModelView.ShowRoot := False;
      ModelView.Items.AddObject(nil, FError.Text, FError)
    end else
    begin
      Nodes := TList.Create;
      try
        ModelView.ShowRoot := True;
        for I := 0 to Pred(Model.ClassCount) do
        begin
          AClass := Model.Classes[I];
          if (Style = msRelations) or not Assigned(AClass.BaseClass) then
            Nodes.Add(AddClass(nil, AClass, '', Level));
        end;
        ModelView.AlphaSort;
        RemoveInvalidNodes(nil, Nodes);
        FirstNode := ModelView.Items.GetFirstNode;
        if Assigned(FirstNode) and (FirstNode.GetNextSibling = nil) then
          FirstNode.Expand(False);
      finally
        Nodes.Free;
      end;
    end;
  finally;
    ModelView.Items.EndUpdate;
    ModelView.Repaint;
  end;
  RefreshAttributeView;
end;

procedure TInstantModelExplorerForm.ViewInheritanceActionExecute(
  Sender: TObject);
begin
  Style := msInheritance;
  ViewButton.Action := ViewRelationsAction;
end;

procedure TInstantModelExplorerForm.ViewRelationsActionExecute(
  Sender: TObject);
begin
  Style := msRelations;
  ViewButton.Action := ViewInheritanceAction;
end;

procedure TInstantModelExplorerForm.ViewSourceActionExecute(
  Sender: TObject);
begin
  GotoNodeSource(SelectedNode);
end;

procedure TInstantModelExplorerForm.ViewAttributesActionExecute(Sender: TObject);
begin
  SetAttributePanelVisible(not AttributePanel.Visible);
end;

procedure TInstantModelExplorerForm.ViewClassAttributes(AClass: TInstantCodeClass);
begin
//  FAttributeFrame.Subject := AClass;
  InstantAttributeViewFrame.Subject := AClass;
end;

procedure TInstantModelExplorerForm.RestoreLayout;
begin
  try
    with TRegistry.Create do try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\InstantObjects.org\Layout\ClassAttributes', False) then begin
        SetAttributePanelVisible(ReadBool('ShowAttributes'));
        AttributePanel.Height := ReadInteger('AttributePanelHeight');
//        FAttributeFrame.InheritedAttributesPanel.Height := ReadInteger('InheritedAttributeHeight');
        InstantAttributeViewFrame.InheritedAttributesPanel.Height := ReadInteger('InheritedAttributeHeight');
      end;
    finally
      Free;
    end;
  except
  // silently swallow exception
  end;
end;

procedure TInstantModelExplorerForm.StoreLayout;
begin
  with TRegistry.Create do try
    RootKey := HKEY_CURRENT_USER;
    if OpenKey('Software\InstantObjects.org\Layout\ClassAttributes', True) then begin
      WriteBool('ShowAttributes', ViewAttributeButton.Down);
      WriteInteger('AttributePanelHeight', AttributePanel.Height);
//      WriteInteger('InheritedAttributeHeight', FAttributeFrame.InheritedAttributesPanel.Height);
      WriteInteger('InheritedAttributeHeight', InstantAttributeViewFrame.InheritedAttributesPanel.Height);
    end;
  finally
    Free;
  end;
end;


initialization
  ModelExplorer := nil;
  RegisterFieldAddress('InstantModelExplorer', @ModelExplorer);
  RegisterDesktopFormClass(TInstantModelExplorerForm,
    'InstantModelExplorer', 'InstantModelExplorer');

finalization
  UnregisterFieldAddress(@ModelExplorer);
  ModelExplorer := nil;

end.
