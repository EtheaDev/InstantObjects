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
 * Carlo Barazzetta, Adrea Petrelli: porting Kylix
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantModelExplorer;

interface

uses
  SysUtils, Classes, IniFiles,
{$IFDEF MSWINDOWS}
  Windows, Messages, ExtCtrls, ComCtrls, ImgList, DockForm,
  ToolWin, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ActnList,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QActnList, QMenus, QTypes, QImgList, QComCtrls, QControls, QExtCtrls,
{$ENDIF}
  InstantCode;

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
{$IFDEF MSWINDOWS}
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
{$ENDIF}
  protected
    procedure DoNodeDblClick(Node: TTreeNode); virtual;
  published
    property OnNodeDblClick: TNotifyEvent read FOnNodeDblClick write FOnNodeDblClick;
  end;

{$IFDEF LINUX}
  TDockableForm = class(TForm);
{$ENDIF}

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
    procedure ViewInheritanceActionExecute(Sender: TObject);
    procedure ViewRelationsActionExecute(Sender: TObject);
    procedure ViewSourceActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FError: TInstantModelError;
    FModel: TInstantCodeModel;
    FModelView: TModelTreeView;
    FSelectedNode: TTreeNode;
    FStyle: TInstantModelStyle;
    FOnApplyClass: TInstantCodeClassApplyEvent;
    FOnGotoSource: TInstantGotoSourceEvent;
    FOnLoadModel: TInstantCodeModelEvent;
    function GetFocusedClass: TInstantCodeClass;
    function GetSelectedNode: TTreeNode;
    procedure SetError(E: Exception);
    procedure SetStyle(const Value: TInstantModelStyle);
  protected
    procedure ApplyClass(AClass: TInstantCodeClass;
      ChangeType: TInstantCodeChangeType; OldName: string = '';
      ChangedAttributes: TStringList = nil; NewAttributes: TList = nil);
    function ClassFromNode(Node: TTreeNode): TInstantCodeClass;
    procedure DoApplyClass(AClass: TInstantCodeClass;
      ChangeInfo: TInstantCodeClassChangeInfo);
    function EditClass(AClass: TInstantCodeClass; New: Boolean): Boolean;
    procedure GotoNodeSource(Node: TTreeNode);
    procedure GotoSource(const FileName: string; Pos: TInstantCodePos);
    procedure LoadModel;
    procedure UpdateActions; override;
    property SelectedNode: TTreeNode read GetSelectedNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CompileTo(FileName: string);
{$IFDEF MSWINDOWS}
    procedure LoadWindowState(Desktop: TMemIniFile); override;
    procedure SaveWindowState(Desktop: TMemIniFile; IsProject: Boolean); override;
{$ENDIF}
    procedure Refresh;
    procedure UpdateModel;
    property FocusedClass: TInstantCodeClass read GetFocusedClass;
    property Model: TInstantCodeModel read FModel;
    property ModelView: TModelTreeView read FModelView;
    property Style: TInstantModelStyle read FStyle write SetStyle;
    property OnApplyClass: TInstantCodeClassApplyEvent read FOnApplyClass write FOnApplyClass;
    property OnGotoSource: TInstantGotoSourceEvent read FOnGotoSource write FOnGotoSource;
    property OnLoadModel: TInstantCodeModelEvent read FOnLoadModel write FOnLoadModel;
  end;

var
  ModelExplorer: TInstantModelExplorerForm;

implementation

uses
{$IFDEF LINUX}
  QDialogs,
{$ENDIF}
  TypInfo, InstantClassEditor, InstantClasses, DeskUtil, InstantModelExpert,
  InstantDesignUtils, InstantPersistence, InstantDesignHook, InstantAbout,
  InstantImageUtils;

resourcestring
  SDeleteClass = 'Delete Class ''%s''?';
  SModelExt = '.xml';
  SModelFilter = 'XML files (*.xml)|*.xml';

{$R *.dfm}

{ TModelTreeView }

procedure TModelTreeView.DoNodeDblClick(Node: TTreeNode);
begin
  if Assigned(FOnNodeDblClick) then
    FOnNodeDblClick(Node);
end;

{$IFDEF MSWINDOWS}
procedure TModelTreeView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
    if htOnLabel in GetHitTestInfoAt(XPos, YPos) then
      DoNodeDblClick(GetNodeAt(XPos, YPos))
    else
      inherited;
end;
{$ENDIF}

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

procedure TInstantModelExplorerForm.BuildDatabaseActionExecute(
  Sender: TObject);
begin
  if Assigned(ModelExpert) then
    ModelExpert.BuildDatabase(Model);
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
  FModelView := TModelTreeView.Create(Self);
  with FModelView do
  begin
    Align := alClient;
    Parent := Self;
    Images := ModelImages;
    PopupMenu := TreeMenu;
    ReadOnly := True;
{$IFDEF MSWINDOWS}
    RightClickSelect := True;
{$ENDIF}
    OnChange := ModelViewChange;
    OnNodeDblClick := ModelViewNodeDblClick;    
    OnGetImageIndex := ModelViewGetImageIndex;
  end;
  FModel := TInstantCodeModel.Create;
  DesignModel := @FModel;
{$IFDEF MSWINDOWS}
  DeskSection := 'InstantModelExplorer';
  AutoSave := True;
{$ENDIF}
  ModelExplorer := Self;
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

function TInstantModelExplorerForm.EditClass(AClass: TInstantCodeClass;
  New: Boolean): Boolean;
const
  ChangeTypes: array[Boolean] of TInstantCodeChangeType = (ctEdit, ctNew);
var
  OldName: string;
begin
  OldName := AClass.Name;
  with TInstantClassEditorForm.Create(nil) do
  try
    IsNew := New;
    Model := Self.Model;
    Subject := AClass;
    Result := ShowModal = mrOk;
    if Result then
      ApplyClass(AClass, ChangeTypes[New], OldName, ChangedAttributes,
        NewAttributes);
  finally
    Free;
  end;
end;

procedure TInstantModelExplorerForm.EditClassActionExecute(Sender: TObject);
begin
  if EditClass(FocusedClass, False) then
    UpdateModel;
end;

procedure TInstantModelExplorerForm.ExpandAllActionExecute(
  Sender: TObject);
begin
  if Assigned(SelectedNode) then
    SelectedNode.Expand(True);
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

procedure TInstantModelExplorerForm.FormShow(Sender: TObject);
begin
  FModelView.Repaint; // Avoid wrong icon for first node
end;

function TInstantModelExplorerForm.GetFocusedClass: TInstantCodeClass;
begin
  Result := ClassFromNode(SelectedNode);
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
  except
    on E: Exception do
    begin
      NewModel.Free;
      SetError(E);
    end;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TInstantModelExplorerForm.LoadWindowState(Desktop: TMemIniFile);
begin
  inherited;
end;

procedure TInstantModelExplorerForm.SaveWindowState(Desktop: TMemIniFile;
  IsProject: Boolean);
begin
  inherited;
end;
{$ENDIF}

procedure TInstantModelExplorerForm.ModelViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  FSelectedNode := nil;
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
    UpdateModel
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
  ExportModelAction.Enabled := HasModel;
  EditClassAction.Enabled := AtClass;
  ViewSourceAction.Enabled := AtClass;
  DeleteClassAction.Enabled := AtClass;
  ExpandAllAction.Enabled := AtClass;
  CollapseAllAction.Enabled := AtClass;
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
    Text: string): TTreeNode;
  const
    Level: Integer = 0;
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
            TInstantCodeClass(SubItems.Objects[I]), SubItems[I]));
        RemoveInvalidNodes(Result, SubNodes);
      finally
{$IFDEF MSWINDOWS}
        Result.AlphaSort;
{$ENDIF}
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
begin
  FSelectedNode := nil;
  ModelView.Items.BeginUpdate;
  try
    if Assigned(FError) then
    begin
      ModelView.Items.Clear;
{$IFDEF MSWINDOWS}
      ModelView.ShowRoot := False;
{$ENDIF}
      ModelView.Items.AddObject(nil, FError.Text, FError)
    end else
    begin
      Nodes := TList.Create;
      try
{$IFDEF MSWINDOWS}
        ModelView.ShowRoot := True;
{$ENDIF}
        for I := 0 to Pred(Model.ClassCount) do
        begin
          AClass := Model.Classes[I];
          if (Style = msRelations) or not Assigned(AClass.BaseClass) then
            Nodes.Add(AddClass(nil, AClass, ''));
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

procedure TInstantModelExplorerForm.FormCreate(Sender: TObject);
begin
  LoadMultipleImages(ActionImages,'IO_MODELEXPLORERACTIONIMAGES');
  LoadMultipleImages(ModelImages,'IO_MODELEXPLORERMODELIMAGES');
  LoadMultipleImages(AttributeImages,'IO_MODELEXPLORERATTRIBUTEIMAGES');
end;

initialization
  ModelExplorer := nil;
  RegisterFieldAddress('InstantModelExplorer', @ModelExplorer);
{$IFDEF MSWINDOWS}
  RegisterDesktopFormClass(TInstantModelExplorerForm,
    'InstantModelExplorer', 'InstantModelExplorer');
{$ENDIF}

finalization
  UnregisterFieldAddress(@ModelExplorer);
  ModelExplorer := nil;

end.
