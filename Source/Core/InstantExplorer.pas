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
 * Joao Morais, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantExplorer;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Controls, Forms, ComCtrls, ExtCtrls, DbGrids, ImgList,
  Classes, DB, TypInfo, InstantPersistence, InstantPresentation, DbCtrls;

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
    FLevel: integer;
    procedure FreeCurrentObject;
  protected
    function GetCaption: string; virtual;
    function GetImageIndex: Integer; virtual;
  public
    constructor Create(ANodeType: TInstantExplorerNodeType; const AName: string;
      AInstance: TObject; const AValue: string; const ALevel: integer);
    destructor Destroy; override;
    property Caption: string read GetCaption;
    property ImageIndex: Integer read GetImageIndex;
    property Instance: TObject read FInstance;
    property Name: string read FName;
    property NodeType: TInstantExplorerNodeType read FNodeType;
    property Value: string read FValue;
    property Level: integer read FLevel;
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
  TInstantExplorerGetAllowedValuesEvent = procedure (Sender: TInstantExplorer;
    const AObject: TObject; const APropName: string;
    const AAllowedValues: TStrings) of object;
  TInstantExplorerGetFieldNamesEvent = procedure (Sender: TInstantExplorer;
    const AObject: TObject; const AFieldNames: TStrings) of object;

  TInstantExplorerLayout = (loTreeOnly, loDetailOnly, loVertical, loHorizontal);

  TInstantExplorerDBComboBox = class(TDBComboBox)
  private
    procedure ApplyToRecord;
  protected
    procedure Change; override;
    procedure Click; override;
  end;

  TInstantExplorerDBGrid = class(TDBGrid)
  private
    procedure UpdateLastColumnWidth;
  protected
    procedure Resize; override;
  end;

  TInstantExplorerContentEditor = class(TPanel)
  private
    FGrid: TDBGrid;
    function GetGrid: TDBGrid;
  protected
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource(const AValue: TDataSource); virtual;
    function CreateGrid: TDBGrid; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Grid: TDBGrid read GetGrid;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    procedure Setup;
  end;

  TInstantExplorer = class(TCustomControl)
  private
    FAutoAdjust: Boolean;
    FContentEditor: TInstantExplorerContentEditor;
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
    FOnGetAllowedValues: TInstantExplorerGetAllowedValuesEvent;
    FOnGetFieldNames: TInstantExplorerGetFieldNamesEvent;
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
    procedure GetAllowedValues(const AObject: TObject;
      const APropName: string; const AAllowedValues: TStrings);
    function GetImages: TCustomImageList;
    procedure LoadContainerNode(Node: TTreeNode; Container: TInstantContainer);
    procedure LoadInstantQueryNode(Node: TTreeNode; InstantQuery: TInstantQuery);
    procedure LoadNode(Node: TTreeNode; LoadChildren: Boolean);
    procedure LoadObjectNode(Node: TTreeNode; Instance: TObject;
      var ChildCount: Integer; LoadChildren: Boolean);
    function NodeIsLoaded(Node: TTreeNode): Boolean;
    function NodeDataIsAssigned(Node: TTreeNode): Boolean;
    procedure ObjectExposerAfterDelete(Sender: TDataSet);
    procedure ObjectExposerAfterPost(Sender: TDataSet);
    procedure ResizeControls;
    procedure SetAutoAdjust(const Value: Boolean);
    procedure SetLayout(const Value: TInstantExplorerLayout);
    procedure SetShowRoot(const Value: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeViewNodeDeletion(Sender: TObject; Node: TTreeNode);
    procedure SetCurrentObject(const AValue: TObject);
    function GetContentEditor: TWinControl;
    function CreateFieldList: TStrings;
    function GetBorderStyle: TBorderStyle;
    procedure SetBorderStyle(const Value: TBorderStyle);
  protected
    function AddNode(NodeType: TInstantExplorerNodeType; Parent: TTreeNode;
      Name: string; AObject: TObject; Value: string = ''): TTreeNode; virtual;
    procedure ChangeNode(Node: TTreeNode); virtual;
    function CreateContentEditor(AOwner: TComponent;
      DataSource: TDataSource): TInstantExplorerContentEditor; virtual;
    function CreateExposer: TInstantExposer; virtual;
    function CreateNode(Nodes: TTreeNodes; Parent: TTreeNode;
      NodeData: TInstantExplorerNodeData): TTreeNode; virtual;
    function CreateNodeData(NodeType: TInstantExplorerNodeType;
      const Name: string; AObject: TObject;
      const Value: string; const Level: integer): TInstantExplorerNodeData; virtual;
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
    procedure UpdateDetails(ForceRefresh: boolean = False);
    function GetAttributesCount(Instance: TInstantObject): integer; virtual;
    function GetAttribute(Instance: TInstantObject; I: integer): TObject; virtual;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  public
    procedure SetupContentEditor; virtual;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    procedure RefreshNode(Node: TTreeNode);
    property ContentEditor: TWinControl read GetContentEditor;
    property ContentView: TPanel read FContentView;
    property CurrentObject: TObject read GetCurrentObject write SetCurrentObject;
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
    property OnCanResize;
    property OnChangeNode: TInstantExplorerNodeEvent read FOnChangeNode write FOnChangeNode;
    property OnCreateNode: TInstantExplorerCreateNodeEvent read FOnCreateNode write FOnCreateNode;
    property OnCreateNodeData: TInstantExplorerCreateNodeDataEvent read FOnCreateNodeData write FOnCreateNodeData;
    property OnClick;
    property OnDblClick;
    property OnGetAllowedValues: TInstantExplorerGetAllowedValuesEvent
      read FOnGetAllowedValues write FOnGetAllowedValues;
    property OnGetFieldNames: TInstantExplorerGetFieldNamesEvent
      read FOnGetFieldNames write FOnGetFieldNames;
    property OnGetImageIndex: TInstantExplorerNodeEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetNodeText: TInstantExplorerGetNodeTextEvent read FOnGetNodeText write FOnGetNodeText;
    property OnIncludeNode: TInstantExplorerIncludeNodeEvent read FOnIncludeNode write FOnIncludeNode;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsSingle;
  end;

procedure InstantExploreObject(AObject: TObject);

implementation

uses
  SysUtils, Messages, Math,
  InstantClasses, InstantRtti, InstantMetadata, InstantTypes,
  Graphics, StdCtrls, Windows;

const
  NotLoaded = Pointer(-1);

  MAX_DROPDOWN_ITEMS = 10;

procedure InstantExploreObject(AObject: TObject);
var
  Form: TForm;
begin
  Form := TForm.Create(nil);
  try
    Form.Font.Assign(Screen.IconFont);
    Form.BorderStyle := bsSizeable;
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

// Converts CamelCaps to words with spaces among them.
function Beautify(const APropName: string): string;
var
  LCharIndex: Integer;
  LLastWasSpace: Boolean;
begin
  Result := '';
  LLastWasSpace := False;
  for LCharIndex := 1 to Length(APropName) do
  begin
    if (APropName[LCharIndex] = AnsiUpperCase(APropName[LCharIndex])[1]) and not LLastWasSpace then
    begin
      Result := Result + ' ';
      LLastWasSpace := True;
    end
    else
      LLastWasSpace := False;
    Result := Result + APropName[LCharIndex];
  end;
end;


{ TInstantExplorerNodeData }

constructor TInstantExplorerNodeData.Create(ANodeType: TInstantExplorerNodeType;
  const AName: string; AInstance: TObject; const AValue: string; const ALevel: integer);
begin
  FNodeType := ANodeType;
  FName := AName;
  FLevel := ALevel;
  FInstance := AInstance;
  if (FNodeType = ntObject) and (FInstance is TInstantObject) then
    TInstantObject(FInstance).AddRef;
  FValue := AValue;
end;

procedure TInstantExplorerNodeData.FreeCurrentObject;
begin
  if (FNodeType = ntObject) and (FInstance is TInstantObject) then
  begin
    TInstantObject(FInstance).Free;
    FInstance := nil;
  end;
end;

destructor TInstantExplorerNodeData.Destroy;
begin
  FreeCurrentObject;
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
    else if FInstance is TInstantQuery then
      with TInstantQuery(FInstance) do
        Result := Result + '(' + ObjectClassName + ')'
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
  LLevel: integer;
begin
  if Assigned(Parent) then
    LLevel := Parent.Level
  else
    LLevel := 0;
  NodeData := CreateNodeData(NodeType, Name, AObject, Value, LLevel);
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
  LCurrentObject: TObject;
  SaveOnChange: TTVChangedEvent;
  ChildCount: Integer;
begin
  LCurrentObject := nil;
  with TreeView do
  begin
    SaveOnChange := OnChange;
    OnChange := nil;
    Items.BeginUpdate;
    try
      if Assigned(Selected) then
        LCurrentObject := TinstantExplorerNodeData(Selected.Data).Instance;
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
      if LCurrentObject <> nil then
        CurrentObject := LCurrentObject;
    finally
      Items.EndUpdate;
      OnChange := SaveOnChange;
      if Items.Count > 0 then
      begin
        Selected := Items[0];
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

procedure TInstantExplorer.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  DestroyObjectEditor;
  UpdateDetails(False);
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
  DataSource: TDataSource): TInstantExplorerContentEditor;
begin
  Result := TInstantExplorerContentEditor.Create(AOwner);
  Result.DataSource := DataSource;
end;

procedure TInstantExplorer.CreateContentView;
begin
  FContentView := CreatePanel(Self);
  FContentView.Parent := DetailPanel;
  FContentView.Visible := False;
  FContentEditor := CreateContentEditor(FContentView, ObjectSource);
  if Assigned(FContentEditor) then
    with FContentEditor do
    begin
      Align := alClient;
      Parent := FContentView;
    end;
end;

procedure TInstantExplorer.CreateDetailPanel;
begin
  FDetailPanel := CreatePanel(Self);
  with FDetailPanel do
  begin
    Parent := Self;
    BorderStyle := bsNone;
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
  const Value: string; const Level: integer): TInstantExplorerNodeData;
begin
  if Assigned(FOnCreateNodeData) then
  begin
    Result := nil;
    FOnCreateNodeData(Self, NodeType, Name, AObject, Value, Result);
  end else
    Result := TInstantExplorerNodeData.Create(NodeType, Name, AObject, Value, Level);
end;

function TInstantExplorer.CreateObjectEditor(AOwner: TComponent;
  DataSource: TDataSource): TControl;
const
  LABEL_WIDTH = 132;
  LABEL_LEFT = 8;
  VERT_SPACING = 4;
  MIN_CONTROL_WIDTH = 24;

  procedure CreateEditControl(AParent: TWinControl; var ATop: Integer;
    PropInfo: PPropInfo; ADataSource: TDataSource);
  var
    LControl: TWinControl;
    LLabel: TLabel;
    LAllowedValues: TStrings;
  begin
    LLabel := TLabel.Create(AParent);
    LLabel.Left := LABEL_LEFT;
    LLabel.Caption := Beautify(InstantGetPropName(PropInfo))+':';
    LLabel.Parent := AParent;
    LLabel.Top := ATop + 4;
    LLabel.Alignment := taRightJustify;
    LLabel.AutoSize := False;
    LLabel.Width := LABEL_WIDTH - 4;

    case DataSource.DataSet.FieldByName(InstantGetPropName(PropInfo)).DataType of
      ftMemo:
      begin
        LControl := TDBMemo.Create(AParent);
        with TDBMemo(LControl) do
        begin
          Left := LABEL_LEFT + LABEL_WIDTH;
          Anchors := [akLeft, akTop, akRight];
          Parent := AParent;
          Constraints.MinWidth := MIN_CONTROL_WIDTH;
          Width := AParent.Width - Left - 8;
          Top := ATop;
          DataField := InstantGetPropName(PropInfo);
          DataSource := ADataSource;
          if not Assigned(PropInfo.SetProc) then
          begin
            ReadOnly := True;
            Color := clBtnFace;
          end;
        end;
      end;
      ftBoolean:
      begin
        LControl := TDBCheckBox.Create(AParent);
        with TDBCheckBox(LControl) do
        begin
          Left := LABEL_LEFT + LABEL_WIDTH;
          Parent := AParent;
          Width := 16;
          Top := ATop;
          DataField := InstantGetPropName(PropInfo);
          DataSource := ADataSource;
          if not Assigned(PropInfo.SetProc) then
          begin
            ReadOnly := True;
            Color := clBtnFace;
          end;
        end;
      end
      else
      begin
        LAllowedValues := TStringList.Create;
        try
          GetAllowedValues(ObjectExposer.CurrentObject, InstantGetPropName(PropInfo), LAllowedValues);
          if LAllowedValues.Count > 0 then
          begin
            LControl := TInstantExplorerDBComboBox.Create(AParent);
            with TInstantExplorerDBComboBox(LControl) do
            begin
              Left := LABEL_LEFT + LABEL_WIDTH;
              Anchors := [akLeft, akTop, akRight];
              Parent := AParent;
              Constraints.MinWidth := MIN_CONTROL_WIDTH;
              Width := AParent.Width - Left - 8;
              Top := ATop;
              Anchors := [akLeft, akTop, akRight];
              Items := LAllowedValues;
              Style := csDropDownList;
              DataField := InstantGetPropName(PropInfo);
              DataSource := ADataSource;
              if not Assigned(PropInfo.SetProc) then
              begin
                ReadOnly := True;
                Color := clBtnFace;
              end;
            end;
          end
          else
          begin
            LControl := TDBEdit.Create(AParent);
            with TDBEdit(LControl) do
            begin
              Left := LABEL_LEFT + LABEL_WIDTH;
              Anchors := [akLeft, akTop, akRight];
              Parent := AParent;
              Constraints.MinWidth := MIN_CONTROL_WIDTH;
              Width := AParent.Width - Left - 8;
              Top := ATop;
              DataField := InstantGetPropName(PropInfo);
              DataSource := ADataSource;
              if not Assigned(PropInfo.SetProc) then
              begin
                ReadOnly := True;
                Color := clBtnFace;
              end;
            end;
          end;
        finally
          FreeAndNil(LAllowedValues);
        end;
      end;
    end;
    LLabel.FocusControl := LControl;

    Inc(ATop, LControl.Height);
  end;

var
  I, Top: Integer;
  AObject: TObject;
  Editor: TScrollBox;
  LProperties: TInstantProperties;
  LFieldNames: TStrings;
  LFieldIndex: Integer;
begin
  Editor := TScrollBox.Create(AOwner);
  with Editor do
  begin
    BorderStyle := bsNone;
    Align := alClient;
    Parent := ObjectView;
  end;
  AObject := ObjectExposer.CurrentObject;
  Top := VERT_SPACING;
  LProperties := TInstantProperties.Create(AObject);
  try
    LFieldNames := CreateFieldList;
    try
      for LFieldIndex := 0 to LFieldNames.Count - 1 do
      begin
        for I := 0 to Pred(LProperties.Count) do
        begin
          if (LProperties.Types[I] <> tkClass) and (LProperties.Names[I] = LFieldNames[LFieldIndex]) then
          begin
            Inc(Top, VERT_SPACING);
            CreateEditControl(Editor, Top, LProperties.PropInfos[I], DataSource);
          end;
        end;
      end;
    finally
      LFieldNames.Free;
    end;
  finally
    LProperties.Free;
  end;
  Result := Editor;
end;

procedure TInstantExplorer.CreateObjectExposer;
begin
  FObjectExposer := CreateExposer;
  FObjectExposer.AfterPost := ObjectExposerAfterPost;
  FObjectExposer.AfterDelete := ObjectExposerAfterDelete;
  FObjectSource := TDataSource.Create(Self);
  FObjectSource.DataSet := FObjectExposer;
end;

procedure TInstantExplorer.CreateObjectView;
begin
  FObjectView := CreatePanel(Self);
  FObjectView.Parent := DetailPanel;
  FObjectView.Visible := False;
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
    Parent := Self;
    AutoSnap := False;
    MinSize := 30;
    Top := FTreePanel.Top + FTreePanel.Height + 1;
    Align := alTop;
  end;
end;

procedure TInstantExplorer.CreateTreePanel;
begin
  FTreePanel := CreatePanel(Self);
  with FTreePanel do
  begin
    Parent := Self;
    Align := alTop;
    FTreeView := CreateTreeView(FTreePanel);
    with FTreeView do
    begin
      Height := Self.Height div 2;
      HideSelection := False;
      OnChange := TreeViewChange;
      OnExpanding := TreeViewExpanding;
      OnGetImageIndex := TreeViewGetImageIndex;
      Align := alClient;
      Parent := FTreePanel;
      BorderStyle := bsSingle;
    end;
  end;
end;

function TInstantExplorer.CreateTreeView(AOwner: TComponent): TTreeView;
begin
  Result := TTreeView.Create(AOwner);
  Result.ReadOnly := True;
  Result.ShowRoot := ShowRoot;
  Result.OnDeletion := TreeViewNodeDeletion;
end;

destructor TInstantExplorer.Destroy;
begin
  inherited;
  FNotifier.Free;
end;

procedure TInstantExplorer.DestroyObjectEditor;
begin
  if Assigned(ObjectExposer.Subject) then
    ObjectExposer.Subject := nil;
  FreeAndNil(FObjectEditor);
end;

procedure TInstantExplorer.ExpandNode(Node: TTreeNode);
begin
  if not NodeIsLoaded(Node) then
    LoadNode(Node, True);
end;

procedure TInstantExplorer.GetAllowedValues(const AObject: TObject;
  const APropName: string; const AAllowedValues: TStrings);
begin
  if Assigned(FOnGetAllowedValues) then
    FOnGetAllowedValues(Self, AObject, APropName, AAllowedValues);
end;

function TInstantExplorer.GetContentEditor: TWinControl;
begin
  Result := FContentEditor;
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
  if Assigned(FOnGetImageIndex) and Assigned(Node) then
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
  Result := True;
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

procedure TInstantExplorer.LoadInstantQueryNode(Node: TTreeNode;
  InstantQuery: TInstantQuery);
var
  I: Integer;
begin
  for I := 0 to Pred(InstantQuery.ObjectCount) do
    AddNode(ntObject, Node, IntToStr(I), InstantQuery.Objects[I]);
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
  if not NodeDataIsAssigned(Node) then
    Exit;
  NodeData := TInstantExplorerNodeData(Node.Data);
  Node.DeleteChildren;
  Instance := NodeData.Instance;
  ChildCount := 0;
  if Instance is TInstantContainer then
    LoadContainerNode(Node, TInstantContainer(Instance))
  else if Instance is TInstantQuery then
    LoadInstantQueryNode(Node, TInstantQuery(Instance))
  else
    LoadObjectNode(Node, Instance, ChildCount, LoadChildren);
  if not LoadChildren and (ChildCount > 0) then
    Node.Owner.AddChildObject(Node, '', NotLoaded);
end;

function TInstantExplorer.GetAttributesCount(Instance: TInstantObject): integer;
begin
  Result := Instance.Metadata.MemberMap.Count;
end;

function TInstantExplorer.GetBorderStyle: TBorderStyle;
begin
  if Assigned(FTreeView) then
    Result := FTreeView.BorderStyle
  else
    Result := bsSingle;
end;

function TInstantExplorer.GetAttribute(Instance: TInstantObject; I: integer): TObject;
var
  Attribute: TInstantAttributeMetadata;
begin
  Result := nil;
  if I < Instance.Metadata.MemberMap.Count then
  begin
    Attribute := Instance.Metadata.MemberMap[I];
    if Attribute.Category = acContainer then
      Result := Instance.ContainerByName(Attribute.Name)
  end;
end;

procedure TInstantExplorer.LoadObjectNode(Node: TTreeNode; Instance: TObject;
  var ChildCount: Integer; LoadChildren: Boolean);

  procedure AddContainerNodes(Node: TTreeNode; Instance: TInstantObject;
    var ChildCount: Integer; LoadChildren: Boolean);
  var
    I: Integer;
    Map: TInstantAttributeMap;
    Attribute: TObject;
    Container: TInstantContainer;
    InstantQuery: TInstantQuery;
  begin
    Map := Instance.Metadata.MemberMap;
    if not Assigned(Map) then
      Exit;
    for I := 0 to Pred(GetAttributesCount(Instance)) do
    begin
      Attribute := GetAttribute(Instance, I);
      if Attribute is TInstantContainer then
      begin
        Inc(ChildCount);
        Container := TInstantContainer(Attribute);
        if LoadChildren then
          AddNode(ntContainer, Node, Container.Name, Container);
      end
      else if Attribute is TInstantQuery then
      begin
        Inc(ChildCount);
        InstantQuery := TInstantQuery(Attribute);
        if LoadChildren then
        begin
          AddNode(ntContainer, Node, InstantQuery.ObjectClassName, InstantQuery);
        end;
      end;
    end;
  end;

var
  I: Integer;
  NodeType: TInstantExplorerNodeType;
  SubObject: TObject;
  Value: string;
  LProperties: TInstantProperties;
begin
  LProperties := TInstantProperties.Create(Instance);
  try
    for I := 0 to Pred(LProperties.Count) do
    begin
      if LProperties.Types[I] = tkClass then
      begin
        NodeType := ntObject;
        SubObject := TObject(NativeInt(LProperties.Values[I]));
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
            Value := LProperties.Texts[I];
          AddNode(NodeType, Node, LProperties.Names[I], SubObject, Value)
        end;
      end;
    end;
  finally
    LProperties.Free;
  end;
  if (Instance is TInstantObject) and (ntContainer in NodeTypes) then
    AddContainerNodes(Node, TInstantObject(Instance), ChildCount,
      LoadChildren);
end;

procedure TInstantExplorer.TreeViewNodeDeletion(Sender: TObject; Node: TTreeNode);
var
  Obj: TObject;
begin
  // Don't test 'Assigned(Node.Data)' because value
  // of 'Node.Data' could be -1 ('NotLoaded')
  if NodeDataIsAssigned(Node) then
  begin
    Obj := TObject(Node.Data);
    Node.Data := NotLoaded;
    FreeAndNil(Obj);
  end;
end;

function TInstantExplorer.NodeIsLoaded(Node: TTreeNode): Boolean;
begin
  if Node.HasChildren then
  begin
    Node := Node.GetFirstChild;
    Result := not Assigned(Node) or (Node.Data <> NotLoaded);
  end
  else
    Result := True;
end;

function TInstantExplorer.NodeDataIsAssigned(Node: TTreeNode): Boolean;
begin
  Result := Assigned(Node) and (Integer(Node.Data) <> -1);
end;

procedure TInstantExplorer.ObjectExposerAfterPost(Sender: TDataSet);
begin
  Refresh;
end;

procedure TInstantExplorer.ObjectExposerAfterDelete(Sender: TDataSet);
begin
  Refresh;
end;

procedure TInstantExplorer.Refresh;
begin
  AssignRootObject(FRootObject);
end;

procedure TInstantExplorer.RefreshNode(Node: TTreeNode);
var
  IsExpanded: Boolean;
  NodeData: TInstantExplorerNodeData;
begin
  if not Assigned(Node) then
    Exit;
  IsExpanded := Node.Expanded;
  if NodeDataIsAssigned(Node) then
  begin
    NodeData := TInstantExplorerNodeData(Node.Data);
    if (NodeData.Instance is TInstantReferences) and (TInstantReferences(NodeData.Instance).isVirtual) then
      UpdateDetails(True);
  end;
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

procedure TInstantExplorer.SetBorderStyle(const Value: TBorderStyle);
begin
  if Assigned(FTreeView) then
    FTreeView.BorderStyle := Value;
end;

procedure TInstantExplorer.SetCurrentObject(const AValue: TObject);
var
  LNodeIndex: Integer;
begin
  if (AValue <> nil) and (TreeView.Items.Count > 0) then
  begin
    for LNodeIndex := 0 to TreeView.Items.Count - 1 do
    begin
      if Integer(TreeView.Items[LNodeIndex].Data) > 0 then
        if TInstantExplorerNodeData(TreeView.Items[LNodeIndex].Data).Instance = AValue then
        begin
          TreeView.Selected := TreeView.Items[LNodeIndex];
          TreeViewChange(TreeView, nil);
          Break;
        end;
    end;
  end
  else
  begin
    TreeView.Selected := nil;
    TreeViewChange(TreeView, nil);
  end;
end;

procedure TInstantExplorer.SetDetailView(const Value: TPanel);
begin
  if Value <> FDetailView then
  begin
    if Assigned(FDetailView) then
      FDetailView.Visible := False;
    FDetailView := Value;
    if Assigned(FDetailView) then
    begin
      FDetailView.Align := alClient;
      FDetailView.Visible := True;
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
  if not Assigned(Node) or Node.Deleting or (Node.ItemId = nil) then
    Exit;
  if NodeDataIsAssigned(Node) then
  begin
    Node.ImageIndex := TInstantExplorerNodeData(Node.Data).ImageIndex;
    Node.SelectedIndex := Node.ImageIndex;
    GetImageIndex(Node);
  end;
end;

procedure TInstantExplorer.UpdateDetails(ForceRefresh: boolean = False);
var
  AObject: TObject;
  Container: TInstantContainer;
  InstantQuery: TInstantQuery;
begin
  if not ForceRefresh then
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
    if (ObjectExposer.Subject <> Container.Owner) or (ObjectExposer.ContainerName <> Container.Name) then
    begin
      ObjectExposer.Close;
      ObjectExposer.Subject := nil;
      ObjectExposer.Mode := amContent;
      ObjectExposer.ContainerName := Container.Name;
      ObjectExposer.Subject := Container.Owner;
    end
    else if ForceRefresh then
      Container.Owner.Refresh;

    if DetailView <> ContentView then
    begin
      DetailView := ContentView;
      SetupContentEditor;
    end;
  end
  else if AObject is TInstantQuery then
  begin
    DestroyObjectEditor;
    InstantQuery := TInstantQuery(AObject);
    if (ObjectExposer.Subject <> InstantQuery) or (ObjectExposer.ContainerName <> InstantQuery.ObjectClassName) then
    begin
      ObjectExposer.Subject := nil;
      ObjectExposer.Mode := amContent;
      ObjectExposer.Subject := InstantQuery;
    end;
    if DetailView <> ContentView then
    begin
      DetailView := ContentView;
      SetupContentEditor;
    end;
  end
  else if Assigned(AObject) then
  begin
    //if AObject.ClassType <> ObjectExposer.ObjectClass then
      DestroyObjectEditor;
    ObjectExposer.Subject := nil;
    ObjectExposer.Mode := amObject;
    ObjectExposer.ContainerName := '';
    ObjectExposer.Subject := AObject;
    if not Assigned(FObjectEditor) and Assigned(Parent) then
      FObjectEditor := CreateObjectEditor(Self, ObjectSource);
    DetailView := ObjectView;
  end else
  begin
    ObjectExposer.Subject := nil;
    DetailView := nil;
  end;
  // Reduces a treeview painting problem in D2007 with runtime themes enabled
  // and checkboxes in the object editor.
  Update;
end;

procedure TInstantExplorer.SetupContentEditor;
var
  LColumnIndex: Integer;
  LField: TField;
  LFieldNames: TStrings;
  LFieldIndex: Integer;

begin
  // Arrange columns.
  LFieldNames := CreateFieldList;
  try
    FContentEditor.Grid.Columns.Clear;
    for LFieldIndex := 0 to LFieldNames.Count - 1 do
      with FContentEditor.Grid.Columns.Add do
      begin
        FieldName := LFieldNames[LFieldIndex];
        Width := DefaultWidth;
      end;
  finally
    LFieldNames.Free;
  end;

  for LColumnIndex := 0 to FContentEditor.Grid.Columns.Count - 1 do
  begin
    LField := FContentEditor.Grid.Columns[LColumnIndex].Field;
    // Beautify caption.
    FContentEditor.Grid.Columns[LColumnIndex].Title.Caption := Beautify(LField.DisplayLabel);
    // Add combo box for boolean fields.
    if LField.DataType = ftBoolean then
    begin
      FContentEditor.Grid.Columns[LColumnIndex].PickList.Clear;
      FContentEditor.Grid.Columns[LColumnIndex].PickList.Add(BoolToStr(True, True));
      FContentEditor.Grid.Columns[LColumnIndex].PickList.Add(BoolToStr(False, True));
    end
    else
    // Add picklist for all columns.
    begin
      FContentEditor.Grid.Columns[LColumnIndex].PickList.Clear;
      GetAllowedValues(ObjectExposer.CurrentObject,
        FContentEditor.Grid.Columns[LColumnIndex].FieldName,
        FContentEditor.Grid.Columns[LColumnIndex].PickList);
    end;
    FContentEditor.Grid.Columns[LColumnIndex].DropDownRows :=
      Min(MAX_DROPDOWN_ITEMS, FContentEditor.Grid.Columns[LColumnIndex].PickList.Count);
    // Hide detail and memo columns, useless in standard grids.
    if LField.DataType in [ftBlob, ftMemo, ftDataSet] then
      FContentEditor.Grid.Columns[LColumnIndex].Visible := False;
  end;
  FContentEditor.Setup;
end;

function TInstantExplorer.CreateFieldList: TStrings;
var
  LFieldIndex: Integer;
begin
  Result := TStringList.Create;
  try
    for LFieldIndex := 0 to FObjectExposer.FieldCount - 1 do
      Result.Add(FObjectExposer.Fields[LFieldIndex].FieldName);
    if Assigned(FOnGetFieldNames) then
      FOnGetFieldNames(Self, ObjectExposer.CurrentObject, Result);
  except
    Result.Free;
    raise;
  end;
end;

{ TInstantExplorerDBGrid }

procedure TInstantExplorerDBGrid.Resize;
begin
  inherited;
  UpdateLastColumnWidth;
end;

procedure TInstantExplorerDBGrid.UpdateLastColumnWidth;

  function GetAllColumnsWidth(const AMinus: Integer): Integer;
  var
    LColumnIndex: Integer;
  begin
    Result := 0;
    for LColumnIndex := 0 to Columns.Count - 1 - AMinus do
      Inc(Result, Columns[LColumnIndex].Width);
  end;

begin
  if Columns.Count >= 1 then
    Columns[Columns.Count - 1].Width := ClientWidth - GetAllColumnsWidth(1)
      - IndicatorWidth - GetSystemMetrics(SM_CXVSCROLL);
end;

{ TInstantExplorerContentEditor }

constructor TInstantExplorerContentEditor.Create(AOwner: TComponent);
begin
  inherited;
  BevelInner := bvNone;
  BevelOuter := bvNone;

  FGrid := CreateGrid;
  FGrid.Parent := Self;
  FGrid.Align := alClient;
end;

function TInstantExplorerContentEditor.CreateGrid: TDBGrid;
begin
  Result := TInstantExplorerDBGrid.Create(Self);
end;

function TInstantExplorerContentEditor.GetDataSource: TDataSource;
begin
  Result := FGrid.DataSource;
end;

function TInstantExplorerContentEditor.GetGrid: TDBGrid;
begin
  Result := FGrid;
end;

procedure TInstantExplorerContentEditor.SetDataSource(const AValue: TDataSource);
begin
  FGrid.DataSource := AValue;
end;

procedure TInstantExplorerContentEditor.Setup;
begin
  if FGrid is TInstantExplorerDBGrid then
    TInstantExplorerDBGrid(FGrid).UpdateLastColumnWidth;
end;

{ TInstantExplorerDBComboBox }

procedure TInstantExplorerDBComboBox.ApplyToRecord;
var
  LDataLink: TDataLink;
begin
  // Provides the auto-apply feature.
  LDataLink := TDataLink(Perform(CM_GETDATALINK, 0, 0));
  try
    LDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
end;

procedure TInstantExplorerDBComboBox.Change;
begin
  inherited;
  ApplyToRecord;
end;

procedure TInstantExplorerDBComboBox.Click;
begin
  inherited;
  ApplyToRecord;
end;

end.
