(*
 *   InstantObjects
 *   Object Explorer
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
 * Joao Morais
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantExplorer;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, ComCtrls, ExtCtrls, DbGrids, ImgList,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QComCtrls, QExtCtrls, QDBGrids, QImgList,
{$ENDIF}
  Classes, Db, InstantPersistence, InstantPresentation;

type
  TInstantExplorer = class;

  TInstantExplorerNodeType = (ntProperty, ntObject, ntContainer);
  TInstantExplorerNodeTypes = set of TInstantExplorerNodeType;

  TInstantExplorerNodeData = class(TObject)
  private
    FNodeType: TInstantExplorerNodeType;
    FName: string;
    FInstance: TObject;
    FValue: string;
  protected
    function GetCaption: string; virtual;
    function GetImageIndex: Integer; virtual;
  public
    constructor Create(ANodeType: TInstantExplorerNodeType; AName: string;
      AInstance: TObject; AValue: string);
    destructor Destroy; override;
    property Caption: string read GetCaption;
    property ImageIndex: Integer read GetImageIndex;
    property Instance: TObject read FInstance;
    property Name: string read FName;
    property NodeType: TInstantExplorerNodeType read FNodeType;
    property Value: string read FValue;
  end;

  TInstantExplorerNodeEvent = procedure(Sender: TInstantExplorer;
    Node: TTreeNode) of object;
  TInstantExplorerIncludeNodeEvent = procedure(Sender: TInstantExplorer;
    NodeData: TInstantExplorerNodeData; var Include: Boolean) of object;
  TInstantExplorerCreateNodeEvent = procedure(Sender: TInstantExplorer;
    Nodes: TTreeNodes; Parent: TTreeNode;
    NodeData: TInstantExplorerNodeData; var Node: TTreeNode) of object;
  TInstantExplorerCreateNodeDataEvent = procedure(Sender: TInstantExplorer;
    NodeType: TInstantExplorerNodeType; const Name: string; AObject: TObject;
    const Value: string; var NodeData: TInstantExplorerNodeData) of object;
  TInstantExplorerGetNodeTextEvent = procedure(Sender: TInstantExplorer;
    NodeData: TInstantExplorerNodeData; var Text: string) of object;
    
  TInstantExplorerLayout = (loTreeOnly, loDetailOnly, loVertical, loHorizontal);

  TInstantExplorer = class(TCustomControl)
  private
    FAutoAdjust: Boolean;
    FContentEditor: TDBGrid;
    FContentView: TPanel;
    FDetailPanel: TPanel;
    FDetailView: TPanel;
    FLayout: TInstantExplorerLayout;
    FNodeTypes: TInstantExplorerNodeTypes;
    FNotifier: TInstantObjectNotifier;
    FObjectEditor: TControl;
    FObjectExposer: TInstantExposer;
    FObjectSource: TDataSource;
    FObjectView: TPanel;
    FRootObject: TObject;
    FShowRoot: Boolean;
    FSplitter: TSplitter;
    FTreePanel: TPanel;
    FTreeView: TTreeView;
    FOnChangeNode: TInstantExplorerNodeEvent;
    FOnCreateNode: TInstantExplorerCreateNodeEvent;
    FOnCreateNodeData: TInstantExplorerCreateNodeDataEvent;
    FOnGetImageIndex: TInstantExplorerNodeEvent;
    FOnGetNodeText: TInstantExplorerGetNodeTextEvent;
    FOnIncludeNode: TInstantExplorerIncludeNodeEvent;
    function AddNode(NodeType: TInstantExplorerNodeType; Parent: TTreeNode;
      Name: string; AObject: TObject; Value: string = ''): TTreeNode;
    procedure ArrangeControls;
    procedure AssignRootObject(Value: TObject);
    procedure CreateContentView;
    procedure CreateObjectView;
    procedure CreateDetailPanel;
    procedure CreateDetailViews;
    procedure CreateObjectExposer;
    function CreatePanel(AOwner: TComponent): TPanel;
    procedure CreateSplitter;
    procedure CreateTreePanel;
    procedure DestroyObjectEditor;
    procedure ExpandNode(Node: TTreeNode);
    function GetCurrentObject: TObject;
    function GetImages: TCustomImageList;
    procedure LoadContainerNode(Node: TTreeNode; Container: TInstantContainer);
    procedure LoadNode(Node: TTreeNode; LoadChildren: Boolean);
    procedure LoadObjectNode(Node: TTreeNode; Instance: TObject;
      var ChildCount: Integer; LoadChildren: Boolean);
    function NodeIsLoaded(Node: TTreeNode): Boolean;
    procedure ObjectExposerAfterPost(Sender: TDataSet);
    procedure ResizeControls;
    procedure SetAutoAdjust(const Value: Boolean);
    procedure SetLayout(const Value: TInstantExplorerLayout);
    procedure SetShowRoot(const Value: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
  protected
    procedure ChangeNode(Node: TTreeNode); virtual;
    function CreateContentEditor(AOwner: TComponent;
      DataSource: TDataSource): TDBGrid; virtual;
    function CreateExposer: TInstantExposer; virtual;
    function CreateNode(Nodes: TTreeNodes; Parent: TTreeNode;
      NodeData: TInstantExplorerNodeData): TTreeNode; virtual;
    function CreateNodeData(NodeType: TInstantExplorerNodeType;
      const Name: string; AObject: TObject;
      const Value: string): TInstantExplorerNodeData; virtual;
    function CreateObjectEditor(AOwner: TComponent;
      DataSource: TDataSource): TControl; virtual;
    function CreateTreeView(AOwner: TComponent): TTreeView; virtual;
    procedure GetImageIndex(Node: TTreeNode); virtual;
    function GetNodeText(NodeData: TInstantExplorerNodeData): string; virtual;
    function IncludeNode(NodeData: TInstantExplorerNodeData): Boolean; virtual;
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetDetailView(const Value: TPanel); virtual;
    procedure SetImages(const Value: TCustomImageList); virtual;
    procedure SetNodeTypes(const Value: TInstantExplorerNodeTypes); virtual;
    procedure SetRootObject(const Value: TObject); virtual;
    procedure UpdateDetails;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    procedure RefreshNode(Node: TTreeNode);
    property ContentView: TPanel read FContentView;
    property CurrentObject: TObject read GetCurrentObject;
    property DetailPanel: TPanel read FDetailPanel;
    property DetailView: TPanel read FDetailView write SetDetailView;
    property ObjectExposer: TInstantExposer read FObjectExposer;
    property ObjectSource: TDataSource read FObjectSource;
    property ObjectView: TPanel read FObjectView;
    property RootObject: TObject read FRootObject write SetRootObject;
    property Splitter: TSplitter read FSplitter;
    property TreePanel: TPanel read FTreePanel;
    property TreeView: TTreeView read FTreeView;
  published
    property Align;
    property Anchors;
    property AutoAdjust: Boolean read FAutoAdjust write SetAutoAdjust default True;
    property Constraints;
    property Images: TCustomImageList read GetImages write SetImages;
    property Layout: TInstantExplorerLayout read FLayout write SetLayout;
    property NodeTypes: TInstantExplorerNodeTypes read FNodeTypes write SetNodeTypes default [ntObject, ntContainer];
    property PopupMenu;
    property ShowHint;
    property ShowRoot: Boolean read FShowRoot write SetShowRoot default True;
    property Visible;
{$IFDEF MSWINDOWS}
    property OnCanResize;
{$ENDIF}
    property OnChangeNode: TInstantExplorerNodeEvent read FOnChangeNode write FOnChangeNode;
    property OnCreateNode: TInstantExplorerCreateNodeEvent read FOnCreateNode write FOnCreateNode;
    property OnCreateNodeData: TInstantExplorerCreateNodeDataEvent read FOnCreateNodeData write FOnCreateNodeData;
    property OnClick;
    property OnDblClick;
    property OnGetImageIndex: TInstantExplorerNodeEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetNodeText: TInstantExplorerGetNodeTextEvent read FOnGetNodeText write FOnGetNodeText;
    property OnIncludeNode: TInstantExplorerIncludeNodeEvent read FOnIncludeNode write FOnIncludeNode;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

procedure InstantExploreObject(AObject: TObject);

implementation

uses
  SysUtils, InstantClasses, InstantRtti, TypInfo,
{$IFDEF MSWINDOWS}
  Graphics, StdCtrls, DbCtrls, Windows;
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QStdCtrls, QDbCtrls;
{$ENDIF}

const
  NotLoaded = Pointer(-1);

procedure InstantExploreObject(AObject: TObject);
var
  Form: TForm;
begin
  Form := TForm.Create(nil);
  try
{$IFDEF MSWINDOWS}
    Form.BorderStyle := bsSizeable;
{$ENDIF}
{$IFDEF LINUX}
    Form.BorderStyle := fbsSizeable;
{$ENDIF}
    Form.Position := poScreenCenter;
    Form.Caption := 'Object Explorer';
    Form.Height := Screen.Height div 10 * 6;
    Form.Width := Screen.Width div 10 * 4;
    with TInstantExplorer.Create(Form) do
    begin
      Layout := loVertical;
      Align := alClient;
      Parent := Form;
      RootObject := AObject;
    end;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

{ TInstantExplorerNodeData }

constructor TInstantExplorerNodeData.Create(ANodeType: TInstantExplorerNodeType;
  AName: string; AInstance: TObject; AValue: string);
begin
  FNodeType := ANodeType;
  FName := AName;
  FInstance := AInstance;
  if FInstance is TInstantObject then
    TInstantObject(FInstance).AddRef;
  FValue := AValue;
end;

destructor TInstantExplorerNodeData.Destroy;
begin
  if FInstance is TInstantObject then
    TInstantObject(FInstance).Free;
  inherited;
end;

function TInstantExplorerNodeData.GetCaption: string;
begin
  Result := FName;
  if Assigned(FInstance) then
  begin
    if Result <> '' then
      Result := Result + ': ';
    Result := Result + FInstance.ClassName;
    if FInstance is TInstantObject then
      with TInstantObject(FInstance) do
      begin
        if Id <> '' then
          Result := Result + '(''' + Id + ''')';
        if (Caption <> '') and (Caption <> Id) then
          Result := Result + ', ''' + Caption + '''';
      end
    else if FInstance is TInstantContainer then
      with TInstantContainer(FInstance) do
        Result := Result + '(' + RequiredClass.ClassName + ')'
    else if FInstance is TInstantAttribute then
      //
    else if FInstance is TInstantStreamable then
      with TInstantStreamable(FInstance) do
        if Name <> '' then
          Result := Result + '(''' + Name + ''')';
  end;
  if Value <> '' then
    Result := Result + ' = ' + Value;
end;

function TInstantExplorerNodeData.GetImageIndex: Integer;
begin
  Result := Ord(NodeType);
end;

{ TInstantExplorer }

function TInstantExplorer.AddNode(NodeType: TInstantExplorerNodeType;
  Parent: TTreeNode; Name: string; AObject: TObject; Value: string): TTreeNode;
var
  NodeData: TInstantExplorerNodeData;
begin
  NodeData := CreateNodeData(NodeType, Name, AObject, Value);
  if Assigned(NodeData) then
  begin
    if IncludeNode(NodeData) then
      Result := CreateNode(TreeView.Items, Parent, NodeData)
    else
      Result := nil;
    if Assigned(Result) then
      LoadNode(Result, False)
    else
      NodeData.Free;
  end else
    Result := nil;
end;

procedure TInstantExplorer.ArrangeControls;
begin
  TreePanel.Parent := nil;
  Splitter.Parent := nil;
  DetailPanel.Parent := nil;
  try
    TreePanel.Align := alNone;
    Splitter.Align := alNone;
    DetailPanel.Align := alNone;
    TreePanel.Visible := FLayout <> loDetailOnly;
    DetailPanel.Visible := FLayout <> loTreeOnly;
    Splitter.Visible := TreePanel.Visible and DetailPanel.Visible;
    case FLayout of
      loTreeOnly:
        TreePanel.Align := alClient;
      loDetailOnly:
        DetailPanel.Align := alClient;
      loHorizontal:
        begin
          TreePanel.Left := 0;
          TreePanel.Top := 0;
          TreePanel.Width := Width div 2;
          TreePanel.Align := alLeft;
          Splitter.Align := alLeft;
          Splitter.Left := TreePanel.Width + 1;
          Splitter.Top := 0;
          Splitter.Width := 3;
          DetailPanel.Left := Splitter.Left + Splitter.Width + 1;
          DetailPanel.Top := 0;
          DetailPanel.Align := alClient;
        end;
      loVertical:
        begin
          TreePanel.Left := 0;
          TreePanel.Top := 0;
          TreePanel.Height := Height div 2;
          TreePanel.Align := alTop;
          Splitter.Align := alTop;
          Splitter.Left := 0;
          Splitter.Top := TreePanel.Height + 1;
          Splitter.Height := 3;
          DetailPanel.Left := 0;
          DetailPanel.Top := Splitter.Top + Splitter.Height + 1;
          DetailPanel.Align := alClient;
        end;
    end;
  finally
    TreePanel.Parent := Self;
    Splitter.Parent := Self;
    DetailPanel.Parent := Self;
  end;
end;

procedure TInstantExplorer.AssignRootObject(Value: TObject);
var
  Node: TTreeNode;
  ItemIndex: Integer;
  SaveOnChange: TTVChangedEvent;
  ChildCount: Integer;

  procedure FreeTreeViewNodeData(ATreeNodes: TTreeNodes);
  var
    Obj: TObject;
  begin
    with ATreeNodes do
    begin
      Node := GetFirstNode;
      while Node <> nil do
      begin
        // Don't test 'Assigned(Node.Data)' because value 
        // of 'Node.Data' could be -1 ('NotLoaded')
        if Integer(Node.Data) > 0 then
        begin
          Obj := TObject(Node.Data);
          FreeAndNil(Obj);
        end;
        Node := Node.GetNext;
      end;
    end;
  end;
begin
  ItemIndex := 0;
  with TreeView do
  begin
    SaveOnChange := OnChange;
    OnChange := nil;
    Items.BeginUpdate;
    try
      if Assigned(FRootObject) then
        FreeTreeViewNodeData(Items);

      if Assigned(Selected) then
        ItemIndex := Selected.AbsoluteIndex;
      Items.Clear;
      FRootObject := Value;
      if Assigned(FRootObject) then
      begin
        if Self.ShowRoot then
        begin
          Node := AddNode(ntObject, nil, '', FRootObject);
          if Assigned(Node) then
            Node.Expand(False);
        end else
        begin
          ChildCount := 0;
          LoadObjectNode(nil, FRootObject, ChildCount, True);
        end;
      end;
    finally
      Items.EndUpdate;
      OnChange := SaveOnChange;
      if Items.Count > ItemIndex then
        Selected := Items[ItemIndex]
      else begin
        Selected := nil;
        TreeViewChange(TreeView, nil);
      end;
    end;
  end;
end;

procedure TInstantExplorer.ChangeNode(Node: TTreeNode);
begin
  if Assigned(FOnChangeNode) then
    FOnChangeNode(Self, Node);
end;

procedure TInstantExplorer.Clear;
begin
  RootObject := nil;
end;

constructor TInstantExplorer.Create(AOwner: TComponent);
begin
  inherited;
  FAutoAdjust := True;
  FLayout := loTreeOnly;
  FNodeTypes := [ntObject, ntContainer];
  FShowRoot := True;
  Height := 200;
  Width := 200;
  CreateObjectExposer;
  CreateTreePanel;
  CreateSplitter;
  CreateDetailPanel;
  ArrangeControls;
  FNotifier := TInstantObjectNotifier.Create;
end;

function TInstantExplorer.CreateContentEditor(AOwner: TComponent;
  DataSource: TDataSource): TDBGrid;
var
  Grid: TDBGrid;
begin
  Grid := TDBGrid.Create(AOwner);
  Grid.DataSource := DataSource;
  Grid.BorderStyle := bsNone;
  Result := Grid;
end;

procedure TInstantExplorer.CreateContentView;
begin
  FContentView := CreatePanel(Self);
  FContentEditor := CreateContentEditor(FContentView, ObjectSource);
  if Assigned(FContentEditor) then
    with FContentEditor do
    begin
      Align := alClient;
      Parent := FContentView;
      Show;
    end;
end;

procedure TInstantExplorer.CreateDetailPanel;
begin
  FDetailPanel := CreatePanel(Self);
  with FDetailPanel do
  begin
    BorderStyle := bsSingle;
    Height := Self.Height div 2;
    Align := alClient;
  end;
  CreateDetailViews;
end;

procedure TInstantExplorer.CreateDetailViews;
begin
  CreateObjectView;
  CreateContentView;
end;

function TInstantExplorer.CreateExposer: TInstantExposer;
begin
  Result := TInstantExposer.Create(Self);
  with Result do
  try
    Options := Options - [eoAutoApply];
    FieldOptions := [];
  except
    Free;
    raise;
  end;
end;

function TInstantExplorer.CreateNode(Nodes: TTreeNodes; Parent: TTreeNode;
  NodeData: TInstantExplorerNodeData): TTreeNode;
begin
  if Assigned(FOnCreateNode) then
  begin
    Result := nil;
    FOnCreateNode(Self, Nodes, Parent, NodeData, Result);
  end else
    Result := Nodes.AddChildObject(Parent, GetNodeText(NodeData), NodeData);
end;

function TInstantExplorer.CreateNodeData(
  NodeType: TInstantExplorerNodeType; const Name: string; AObject: TObject;
  const Value: string): TInstantExplorerNodeData;
begin
  if Assigned(FOnCreateNodeData) then
  begin
    Result := nil;
    FOnCreateNodeData(Self, NodeType, Name, AObject, Value, Result);
  end else
    Result := TInstantExplorerNodeData.Create(NodeType, Name, AObject, Value);
end;

function TInstantExplorer.CreateObjectEditor(AOwner: TComponent;
  DataSource: TDataSource): TControl;

  procedure CreateEditControl(AParent: TWinControl; var ATop: Integer;
    PropInfo: PPropInfo; ADataSource: TDataSource);
  var
    Edit: TDBEdit;
  begin
    Edit := TDBEdit.Create(AParent);
    with Edit do
    begin
      Left := 108;
      Top := ATop;
      Anchors := [akLeft, akTop, akRight];
      Constraints.MinWidth := 8;
      Width := AParent.Width - Left - 8;
      Anchors := [akLeft, akTop, akRight];
      Parent := AParent;
      DataField := PropInfo.Name;
      DataSource := ADataSource;
      if not Assigned(PropInfo.SetProc) then
      begin
        ReadOnly := True;
        Color := clBtnFace;
      end;
    end;
    with TLabel.Create(AParent) do
    begin
      Left := 8;
      Top := ATop + 3;
      Parent := AParent;
      Caption := PropInfo.Name;
      FocusControl := Edit;
    end;
    Inc(ATop, Edit.Height);
  end;

var
  I, Top: Integer;
  AObject: TObject;
  Editor: TScrollBox;
begin
  Editor := TScrollBox.Create(AOwner);
  with Editor do
  begin
    BorderStyle := bsNone;
    Align := alClient;
  end;
  AObject := ObjectExposer.CurrentObject;
  Top := 4;
  with TInstantProperties.Create(AObject) do
  try
    for I := 0 to Pred(Count) do
      if Types[I] <> tkClass then
      begin
        Inc(Top, 4);
        CreateEditControl(Editor, Top, PropInfos[I], DataSource);
      end;
  finally
    Free;
  end;
  Editor.Parent := ObjectView;
  Result := Editor;
end;

procedure TInstantExplorer.CreateObjectExposer;
begin
  FObjectExposer := CreateExposer;
  if Assigned(FObjectExposer) then
    FObjectExposer.AfterPost := ObjectExposerAfterPost;
  FObjectSource := TDataSource.Create(Self);
  FObjectSource.DataSet := FObjectExposer;
end;

procedure TInstantExplorer.CreateObjectView;
begin
  FObjectView := CreatePanel(Self);
end;

function TInstantExplorer.CreatePanel(AOwner: TComponent): TPanel;
begin
  Result := TPanel.Create(AOwner);
  Result.BevelInner := bvNone;
  Result.BevelOuter := bvNone;
end;

procedure TInstantExplorer.CreateSplitter;
begin
  FSplitter := TSplitter.Create(Self);
  with FSplitter do
  begin
    Top := FTreePanel.Top + FTreePanel.Height + 1;
    Align := alTop;
  end;
end;

procedure TInstantExplorer.CreateTreePanel;
begin
  FTreePanel := CreatePanel(Self);
  with FTreePanel do
  begin
    Align := alTop;
    FTreeView := CreateTreeView(FTreePanel);
    with FTreeView do
    begin
      Height := Self.Height div 2;
{$IFDEF MSWINDOWS}
      HideSelection := False;
{$ENDIF}
      OnChange := TreeViewChange;
      OnExpanding := TreeViewExpanding;
      OnGetImageIndex := TreeViewGetImageIndex;
      Align := alClient;
      Parent := FTreePanel;
    end;
    Parent := Self;
  end;
end;

function TInstantExplorer.CreateTreeView(AOwner: TComponent): TTreeView;
begin
  Result := TTreeView.Create(AOwner);
  Result.ReadOnly := True;
{$IFDEF MSWINDOWS}
  Result.ShowRoot := ShowRoot;
{$ENDIF}
end;

destructor TInstantExplorer.Destroy;
begin
  inherited;
  FNotifier.Free;
end;

procedure TInstantExplorer.DestroyObjectEditor;
begin
  FreeAndNil(FObjectEditor);
end;

procedure TInstantExplorer.ExpandNode(Node: TTreeNode);
begin
  if not NodeIsLoaded(Node) then
    LoadNode(Node, True);
end;

function TInstantExplorer.GetCurrentObject: TObject;
begin
  with TreeView do
    if Assigned(Selected) and Assigned(Selected.Data) then
      Result := TInstantExplorerNodeData(Selected.Data).Instance
    else
      Result := nil;
end;

procedure TInstantExplorer.GetImageIndex(Node: TTreeNode);
begin
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, Node);
end;

function TInstantExplorer.GetImages: TCustomImageList;
begin
  Result := TreeView.Images;
end;

function TInstantExplorer.GetNodeText(
  NodeData: TInstantExplorerNodeData): string;
begin
  Result := NodeData.Caption;
  if Assigned(FOnGetNodeText) then
    FOnGetNodeText(Self, NodeData, Result);
end;

function TInstantExplorer.IncludeNode(
  NodeData: TInstantExplorerNodeData): Boolean;
begin
  if Assigned(FOnIncludeNode) then
    FOnIncludeNode(Self, NodeData, Result)
  else
    Result := True;
end;

procedure TInstantExplorer.LoadContainerNode(Node: TTreeNode;
  Container: TInstantContainer);
var
  I: Integer;
begin
  for I := 0 to Pred(Container.Count) do
    AddNode(ntObject, Node, IntToStr(I), Container[I]);
end;

procedure TInstantExplorer.Loaded;
begin
  inherited;
  ArrangeControls;
end;

procedure TInstantExplorer.LoadNode(Node: TTreeNode; LoadChildren: Boolean);
var
  Instance: TObject;
  ChildCount: Integer;
  NodeData: TInstantExplorerNodeData;
begin
  if not Assigned(Node) or not Assigned(Node.Data) then
    Exit;
  NodeData := TInstantExplorerNodeData(Node.Data);
  Node.DeleteChildren;
  Instance := NodeData.Instance;
  ChildCount := 0;
  if Instance is TInstantContainer then
    LoadContainerNode(Node, TInstantContainer(Instance))
  else
    LoadObjectNode(Node, Instance, ChildCount, LoadChildren);
  if not LoadChildren and (ChildCount > 0) then
    Node.Owner.AddChildObject(Node, '', NotLoaded);
end;

procedure TInstantExplorer.LoadObjectNode(Node: TTreeNode; Instance: TObject;
  var ChildCount: Integer; LoadChildren: Boolean);

  procedure AddContainerNodes(Node: TTreeNode; Instance: TInstantObject;
    var ChildCount: Integer; LoadChildren: Boolean);
  var
    I: Integer;
    Map: TInstantAttributeMap;
    Attribute: TInstantAttributeMetadata;
    Container: TInstantContainer;
  begin
    Map := Instance.Metadata.MemberMap;
    if not Assigned(Map) then
      Exit;
    for I := 0 to Pred(Map.Count) do
    begin
      Attribute := TInstantAttributeMetadata(Map[I]);
      if Attribute.Category = acContainer then
      begin
        Inc(ChildCount);
        if LoadChildren then
        begin
          Container := Instance.ContainerByName(Attribute.Name);
          AddNode(ntContainer, Node, Container.Name, Container);
        end;
      end;
    end;
  end;

var
  I: Integer;
  NodeType: TInstantExplorerNodeType;
  SubObject: TObject;
  Value: string;
begin
  with TInstantProperties.Create(Instance) do
  try
    for I := 0 to Pred(Count) do
    begin
      if Types[I] = tkClass then
      begin
        NodeType := ntObject;
        SubObject := TObject(Integer(Values[I]));
      end else
      begin
        NodeType := ntProperty;
        SubObject := nil;
      end;
      if NodeType in NodeTypes then
      begin
        Inc(ChildCount);
        if LoadChildren then
        begin
          if Assigned(SubObject) then
            Value := ''
          else
            Value := Texts[I];
          AddNode(NodeType, Node, Names[I], SubObject, Value)
        end;
      end;
    end;
  finally
    Free;
  end;
  if (Instance is TInstantObject) and (ntContainer in NodeTypes) then
    AddContainerNodes(Node, TInstantObject(Instance), ChildCount,
      LoadChildren);
end;

function TInstantExplorer.NodeIsLoaded(Node: TTreeNode): Boolean;
begin
  if Node.HasChildren then
  begin
    Node := Node.GetFirstChild;
    Result := not Assigned(Node) or (Node.Data <> NotLoaded);
  end else
    Result := True;
end;

procedure TInstantExplorer.ObjectExposerAfterPost(Sender: TDataSet);
begin
  RefreshNode(TreeView.Selected);
end;

procedure TInstantExplorer.Refresh;
begin
  AssignRootObject(FRootObject);
end;

procedure TInstantExplorer.RefreshNode(Node: TTreeNode);
var
  IsExpanded: Boolean;
begin
  if not Assigned(Node) then
    Exit;
  IsExpanded := Node.Expanded;
  LoadNode(Node, IsExpanded);
  if IsExpanded then
    Node.Expand(False);
end;

procedure TInstantExplorer.Resize;
begin
  inherited;
  if AutoAdjust then
    ResizeControls;
end;

procedure TInstantExplorer.ResizeControls;
begin
  case Layout of
    loHorizontal:
      TreePanel.Width := Width div 2;
    loVertical:
      TreePanel.Height := Height div 2;
  end;
end;

procedure TInstantExplorer.SetAutoAdjust(const Value: Boolean);
begin
  if Value <> AutoAdjust then
  begin
    FAutoAdjust := Value;
    if FAutoAdjust then
      ResizeControls;
  end;
end;

procedure TInstantExplorer.SetDetailView(const Value: TPanel);
begin
  if Value <> FDetailView then
  begin
    if Assigned(FDetailView) then
    begin
      FDetailView.Hide;
      FDetailView.Parent := nil;
    end;
    FDetailView := Value;
    if Assigned(FDetailView) then
    begin
      FDetailView.Align := alClient;
      FDetailView.Parent := DetailPanel;
      FDetailView.Show;
    end;
  end;
end;

procedure TInstantExplorer.SetImages(const Value: TCustomImageList);
begin
  TreeView.Images := Value;
end;

procedure TInstantExplorer.SetLayout(const Value: TInstantExplorerLayout);
begin
  if Value <> Layout then
  begin
    FLayout := Value;
    UpdateDetails;
    ArrangeControls;
  end;
end;

procedure TInstantExplorer.SetNodeTypes(
  const Value: TInstantExplorerNodeTypes);
begin
  if Value <> NodeTypes then
  begin
    FNodeTypes := Value;
    Refresh;
  end;
end;

procedure TInstantExplorer.SetRootObject(const Value: TObject);
begin
  if Value <> FRootObject then
    AssignRootObject(Value);
end;

procedure TInstantExplorer.SetShowRoot(const Value: Boolean);
begin
  if Value <> ShowRoot then
  begin
    FShowRoot := Value;
    Refresh;
  end;
end;

procedure TInstantExplorer.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateDetails;
  ChangeNode(Node);
end;

procedure TInstantExplorer.TreeViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  ExpandNode(Node);
end;

procedure TInstantExplorer.TreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if not Assigned(Node) then
    Exit;
  if Assigned(Node.Data) then
  begin
    Node.ImageIndex := TInstantExplorerNodeData(Node.Data).ImageIndex;
    Node.SelectedIndex := Node.ImageIndex;
  end;
  GetImageIndex(Node);
end;

procedure TInstantExplorer.UpdateDetails;
var
  AObject: TObject;
  Container: TInstantContainer;
begin
  ObjectExposer.PostChanges;
  if Layout = loTreeOnly then
  begin
    ObjectExposer.Subject := nil;
    Exit;
  end;
  AObject := CurrentObject;
  if AObject is TInstantContainer then
  begin
    DestroyObjectEditor;
    Container := TInstantContainer(AObject);
    ObjectExposer.Subject := nil;
    ObjectExposer.Mode := amContent;
    ObjectExposer.ContainerName := Container.Name;
    ObjectExposer.Subject := Container.Owner;
    DetailView := ContentView;
  end else if Assigned(AObject) then
  begin
    if AObject.ClassType <> ObjectExposer.ObjectClass then
      DestroyObjectEditor;
    ObjectExposer.Subject := nil;
    ObjectExposer.Mode := amObject;
    ObjectExposer.ContainerName := '';
    ObjectExposer.Subject := AObject;
    if not Assigned(FObjectEditor) then
      FObjectEditor := CreateObjectEditor(Self, ObjectSource);
    DetailView := ObjectView;
  end else
  begin
    ObjectExposer.Subject := nil;
    DetailView := nil;
  end;
end;

end.
