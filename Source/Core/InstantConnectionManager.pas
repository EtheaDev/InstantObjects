(*
 *   InstantObjects
 *   Connection Manager
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
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantConnectionManager;

{$I InstantDefines.inc}

{$IFDEF D7+}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, Menus, ActnList, InstantPersistence;

type
  TInstantConnectionManagerActionType = (atNew, atEdit, atDelete, atRename,
    atConnect, atDisconnect, atBuild);
  TInstantConnectionManagerActionTypes = set of TInstantConnectionManagerActionType;

  TInstantConnectionDefEvent = procedure(Sender: TObject;
    var ConnectionDef: TInstantConnectionDef; var Result: Boolean) of object;
  TInstantConnectorEvent = procedure(Sender: TObject;
    Connector: TInstantConnector) of object;
  TInstantConnectorClassEvent = procedure(Sender: TObject;
    ConnectorClass: TInstantConnectorClass; var Result: Boolean) of object;

  TInstantConnectionManagerForm = class(TForm)
    ActionList: TActionList;
    BuildAction: TAction;
    BuildButton: TButton;
    BuildItem: TMenuItem;
    CloseButton: TButton;
    ConnectAction: TAction;
    ConnectButton: TButton;
    ConnectItem: TMenuItem;
    ConnectionImages: TImageList;
    ConnectionMenu: TPopupMenu;
    ConnectionView: TListView;
    DeleteAction: TAction;
    DeleteItem: TMenuItem;
    DisconnectAction: TAction;
    DisconnectItem: TMenuItem;
    EditAction: TAction;
    EditItem: TMenuItem;
    N1: TMenuItem;
    NewMenu: TMenuItem;
    RenameAction: TAction;
    RenameItem: TMenuItem;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ConnectionViewEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure BuildActionExecute(Sender: TObject);
    procedure ConnectActionExecute(Sender: TObject);
    procedure ConnectionViewDblClick(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure DisconnectActionExecute(Sender: TObject);
    procedure EditActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure RenameActionExecute(Sender: TObject);
  private
    FConnectionDefs: TInstantConnectionDefs;
    FFileName: string;
    FModel: TInstantModel;
    FOnBuild: TInstantConnectionDefEvent;
    FOnConnect: TInstantConnectionDefEvent;
    FOnDisconnect: TInstantConnectionDefEvent;
    FOnEdit: TInstantConnectionDefEvent;
    FOnIsConnected: TInstantConnectionDefEvent;
    FOnPrepare: TInstantConnectorEvent;
    FOnSupportConnector: TInstantConnectorClassEvent;
    function ConfirmDlg(const Text: string): Boolean;
    function GetConnectionDefs: TInstantConnectionDefs;
    function GetCurrentConnectionDef: TInstantConnectionDef;
    function GetDefsFileName: string;
    function GetVisibleActions: TInstantConnectionManagerActionTypes;
    procedure SetCurrentConnectionDef(Value: TInstantConnectionDef);
    procedure SetFileName(const Value: string);
    procedure SetVisibleActions(Value: TInstantConnectionManagerActionTypes);
    procedure SetOnSupportConnector(Value: TInstantConnectorClassEvent);
    procedure UpdateMenu;
    property DefsFileName: string read GetDefsFileName;
  protected
    procedure Build(ConnectionDef: TInstantConnectionDef);
    procedure Connect(ConnectionDef: TInstantConnectionDef);
    procedure Disconnect(ConnectionDef: TInstantConnectionDef);
    function Edit(ConnectionDef: TInstantConnectionDef): Boolean;
    function DoBuild(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
    function DoConnect(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
    function DoDisconnect(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
    function DoEdit(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
    procedure DoPrepare(Connector: TInstantConnector); virtual;
    function IsConnected(ConnectionDef: TInstantConnectionDef): Boolean;
    procedure LoadConnectionDefs;
    procedure PopulateConnectionDefs;
    procedure SaveConnectionDefs;
    function SupportConnector(ConnectorClass: TInstantConnectorClass): Boolean;
    property ConnectionDefs: TInstantConnectionDefs read GetConnectionDefs;
  public
    destructor Destroy; override;
    property CurrentConnectionDef: TInstantConnectionDef read GetCurrentConnectionDef write SetCurrentConnectionDef;
    property FileName: string read FFileName write SetFileName;
    property Model: TInstantModel read FModel write FModel;
    property VisibleActions: TInstantConnectionManagerActionTypes read GetVisibleActions write SetVisibleActions;
    property OnBuild: TInstantConnectionDefEvent read FOnBuild write FOnBuild;
    property OnConnect: TInstantConnectionDefEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TInstantConnectionDefEvent read FOnDisconnect write FOnDisconnect;
    property OnEdit: TInstantConnectionDefEvent read FOnEdit write FOnEdit;
    property OnIsConnected: TInstantConnectionDefEvent read FOnIsConnected write FOnIsConnected;
    property OnPrepare: TInstantConnectorEvent read FOnPrepare write FOnPrepare;
    property OnSupportConnector: TInstantConnectorClassEvent read FOnSupportConnector write SetOnSupportConnector;
  end;

  TInstantConnectionManager = class(TComponent)
  private
    FCaption: string;
    FForm: TInstantConnectionManagerForm;
    FFileName: string;
    FModel: TInstantModel;
    FOnBuild: TInstantConnectionDefEvent;
    FOnConnect: TInstantConnectionDefEvent;
    FOnDisconnect: TInstantConnectionDefEvent;
    FOnEdit: TInstantConnectionDefEvent;
    FOnIsConnected: TInstantConnectionDefEvent;
    FOnPrepare: TInstantConnectorEvent;
    FOnSupportConnector: TInstantConnectorClassEvent;
    FVisibleActions: TInstantConnectionManagerActionTypes;
    function GetCaption: string;
    function GetCurrentConnectionDef: TInstantConnectionDef;
    function GetForm: TInstantConnectionManagerForm;
    function GetModel: TInstantModel;
    procedure SetCaption(const Value: string);
    procedure SetCurrentConnectionDef(Value: TInstantConnectionDef);
    procedure SetFileName(const Value: string);
    procedure SetModel(Value: TInstantModel);
    procedure SetOnBuild(Value: TInstantConnectionDefEvent);
    procedure SetOnConnect(Value: TInstantConnectionDefEvent);
    procedure SetOnDisconnect(Value: TInstantConnectionDefEvent);
    procedure SetOnEdit(Value: TInstantConnectionDefEvent);
    procedure SetOnIsConnected(Value: TInstantConnectionDefEvent);
    procedure SetOnPrepare(Value: TInstantConnectorEvent);
    procedure SetOnSupportConnector(Value: TInstantConnectorClassEvent);
    procedure SetVisibleActions(Value: TInstantConnectionManagerActionTypes);
  protected
    function HasForm: Boolean;
    property Form: TInstantConnectionManagerForm read GetForm;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
    property CurrentConnectionDef: TInstantConnectionDef read GetCurrentConnectionDef write SetCurrentConnectionDef;
    property Model: TInstantModel read GetModel write SetModel;
  published
    property Caption: string read GetCaption write SetCaption;
    property FileName: string read FFileName write SetFileName;
    property VisibleActions: TInstantConnectionManagerActionTypes read FVisibleActions write SetVisibleActions
      default [atNew, atEdit, atDelete, atRename, atConnect, atDisconnect, atBuild];
    property OnBuild: TInstantConnectionDefEvent read FOnBuild write SetOnBuild;
    property OnConnect: TInstantConnectionDefEvent read FOnConnect write SetOnConnect;
    property OnDisconnect: TInstantConnectionDefEvent read FOnDisconnect write SetOnDisconnect;
    property OnEdit: TInstantConnectionDefEvent read FOnEdit write SetOnEdit;
    property OnIsConnected: TInstantConnectionDefEvent read FOnIsConnected write SetOnIsConnected;
    property OnPrepare: TInstantConnectorEvent read FOnPrepare write SetOnPrepare;
    property OnSupportConnector: TInstantConnectorClassEvent read FOnSupportConnector write SetOnSupportConnector;
  end;

implementation

{$R *.DFM}

uses
  InstantClasses;

{ TInstantConnectionManagerForm }

procedure TInstantConnectionManagerForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);

  procedure EnableAction(Action: TAction; Enable: Boolean);
  begin
    Action.Enabled := Action.Visible and Enable;
  end;

var
  HasItem, Connected: Boolean;
  ConnectionDef: TInstantConnectionDef;
begin
  ConnectionDef := CurrentConnectionDef;
  HasItem := Assigned(ConnectionDef);
  Connected := HasItem and IsConnected(ConnectionDef);
  EnableAction(EditAction, HasItem and not Connected);
  EnableAction(RenameAction, HasItem);
  EnableAction(DeleteAction, HasItem and not Connected);
  EnableAction(BuildAction, HasItem and not Connected);
  EnableAction(ConnectAction, HasItem and not Connected);
  EnableAction(DisconnectAction, HasItem and Connected);
  if Connected then
    ConnectButton.Action := DisconnectAction else
    ConnectButton.Action := ConnectAction;
  ConnectButton.Default := not ConnectionView.IsEditing;
  CloseButton.Cancel := not ConnectionView.IsEditing;
end;

procedure TInstantConnectionManagerForm.Build(ConnectionDef: TInstantConnectionDef);
begin
  try
    if DoBuild(ConnectionDef) then
      ConnectionDef.IsBuilt := True;
  except
    ConnectionDef.IsBuilt := False;
    raise;
  end;
  PopulateConnectionDefs;
end;

procedure TInstantConnectionManagerForm.BuildActionExecute(Sender: TObject);
begin
  Build(CurrentConnectionDef);
end;

function TInstantConnectionManagerForm.ConfirmDlg(
  const Text: string): Boolean;
begin
  Result := MessageDlg(Text, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TInstantConnectionManagerForm.Connect(ConnectionDef: TInstantConnectionDef);
begin
  if Assigned(ConnectionDef) then
    try
      if DoConnect(ConnectionDef) then
      begin
        ConnectionDef.IsBuilt := True;
        ModalResult := mrOk;
      end;
    finally
      PopulateConnectionDefs;
    end;
end;

procedure TInstantConnectionManagerForm.ConnectActionExecute(Sender: TObject);
begin
  Connect(CurrentConnectionDef);
end;

procedure TInstantConnectionManagerForm.ConnectionViewDblClick(Sender: TObject);
begin
  ConnectAction.Execute;
end;

procedure TInstantConnectionManagerForm.ConnectionViewEdited(Sender: TObject;
  Item: TListItem; var S: String);
var
  Def: TInstantConnectionDef;
begin
  Def := Item.Data;
  Def.Name := S;
end;

procedure TInstantConnectionManagerForm.DeleteActionExecute(
  Sender: TObject);
var
  ConnectionDef: TInstantConnectionDef;
begin
  ConnectionDef := CurrentConnectionDef;
  if Assigned(ConnectionDef) and
    ConfirmDlg(Format('Delete connection "%s"?', [ConnectionDef.Name])) then
  begin
    ConnectionDefs.Remove(ConnectionDef);
    PopulateConnectionDefs;
  end;
end;

destructor TInstantConnectionManagerForm.Destroy;
begin
  SaveConnectionDefs;
  inherited;
  FConnectionDefs.Free;
end;

procedure TInstantConnectionManagerForm.Disconnect(ConnectionDef: TInstantConnectionDef);
begin
  if Assigned(ConnectionDef) then
    try
      DoDisconnect(ConnectionDef);
    finally
      PopulateConnectionDefs;
    end;
end;

procedure TInstantConnectionManagerForm.DisconnectActionExecute(Sender: TObject);
begin
  Disconnect(CurrentConnectionDef);
end;

function TInstantConnectionManagerForm.DoBuild(
  ConnectionDef: TInstantConnectionDef): Boolean;
var
  Connector: TInstantConnector;
  SaveCursor: TCursor;
begin
  if Assigned(FOnBuild) then
  begin
    Result := False;
    FOnBuild(Self, ConnectionDef, Result);
    Exit;
  end;
  if not Assigned(ConnectionDef) or not ConfirmDlg(
    Format('Build database via connection "%s"?', [ConnectionDef.Name])) then
  begin
    Result := False;
    Exit;
  end;
  Connector := ConnectionDef.CreateConnector(nil);
  try
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      Application.ProcessMessages;
      Connector.BuildDatabase(Model);
      DoPrepare(Connector);
      Connector.Disconnect;
    finally
      Screen.Cursor := SaveCursor;
    end;
  finally
    Connector.Free;
  end;
  ShowMessage('Database was built successfully');
  Result := True;
end;

function TInstantConnectionManagerForm.DoConnect(
  ConnectionDef: TInstantConnectionDef): Boolean;
begin
  Result := False;
  if Assigned(FOnConnect) then
    FOnConnect(Self, ConnectionDef, Result);
end;

function TInstantConnectionManagerForm.DoDisconnect(
  ConnectionDef: TInstantConnectionDef): Boolean;
begin
  Result := False;
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, ConnectionDef, Result);
end;

function TInstantConnectionManagerForm.DoEdit(
  ConnectionDef: TInstantConnectionDef): Boolean;
begin
  if Assigned(FOnEdit) then
  begin
    Result := False;
    FOnEdit(Self, ConnectionDef, Result);
  end else
    Result := ConnectionDef.Edit;
end;

procedure TInstantConnectionManagerForm.DoPrepare(Connector: TInstantConnector);
begin
  if Assigned(FOnPrepare) then
    FOnPrepare(Self, Connector);
end;

function TInstantConnectionManagerForm.Edit(
  ConnectionDef: TInstantConnectionDef): Boolean;
begin
  Result := DoEdit(ConnectionDef);
  if Result then
    PopulateConnectionDefs;
end;

procedure TInstantConnectionManagerForm.EditActionExecute(
  Sender: TObject);
begin
  Edit(CurrentConnectionDef);
end;

procedure TInstantConnectionManagerForm.FormCreate(Sender: TObject);
begin
  UpdateMenu;
end;

procedure TInstantConnectionManagerForm.FormShow(Sender: TObject);
begin
  with ConnectionView do
    if (Items.Count > 0) and not Assigned(ItemFocused) then
    begin
      Selected := Items[0];
      ItemFocused := Selected;
    end;
end;

function TInstantConnectionManagerForm.GetConnectionDefs: TInstantConnectionDefs;
begin
  if not Assigned(FConnectionDefs) then
    FConnectionDefs := TInstantConnectionDefs.Create;
  Result := FConnectionDefs;
end;

function TInstantConnectionManagerForm.GetCurrentConnectionDef: TInstantConnectionDef;
begin
  with ConnectionView do
    if Assigned(Selected) then
      Result := Selected.Data
    else
      Result := nil;
end;

function TInstantConnectionManagerForm.GetDefsFileName: string;
var
  Path: string;
begin
  if FileName <> '' then
  begin
    Path := ExtractFilePath(FileName);
    if Path = '' then
      Result := ExtractFilePath(Application.ExeName) + FileName
    else
      Result := FileName;
  end else
    Result := '';
end;

function TInstantConnectionManagerForm.GetVisibleActions: TInstantConnectionManagerActionTypes;
begin
  Result := [];
  if NewMenu.Visible then Include(Result, atNew);
  if EditAction.Visible then Include(Result, atEdit);
  if RenameAction.Visible then Include(Result, atRename);
  if DeleteAction.Visible then Include(Result, atDelete);
  if ConnectAction.Visible then Include(Result, atConnect);
  if DisconnectAction.Visible then Include(Result, atDisconnect);
  if BuildAction.Visible then Include(Result, atBuild);
end;

function TInstantConnectionManagerForm.IsConnected(
  ConnectionDef: TInstantConnectionDef): Boolean;
begin
  Result := False;
  if Assigned(FOnIsConnected) then
    FOnIsConnected(Self, ConnectionDef, Result);
end;

procedure TInstantConnectionManagerForm.LoadConnectionDefs;
var
  FileStream: TFileStream;
begin
  if FileExists(DefsFileName) then
  begin
    try
      FileStream := TFileStream.Create(DefsFileName, fmOpenRead);
      try
        InstantReadObjectFromStream(FileStream, ConnectionDefs);
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do
        raise EInstantError.CreateFmt(
          'Error loading connection definitions from %s: %s', [DefsFileName,
            E.Message]);
    end;
  end;
  PopulateConnectionDefs;
end;

procedure TInstantConnectionManagerForm.NewMenuItemClick(Sender: TObject);
var
  ConnectorClass: TInstantConnectorClass;
  ConnectionDef: TInstantConnectionDef;
  Item: TListItem;
begin
  with Sender as TMenuItem do
    ConnectorClass := InstantConnectorClasses[Tag];
  ConnectionDef := ConnectorClass.ConnectionDefClass.Create(ConnectionDefs);
  try
    ConnectionDef.Name := 'New Connection';
    PopulateConnectionDefs;
    Item := ConnectionView.FindData(0, ConnectionDef, True, True);
    if Assigned(Item) then
      Item.EditCaption;
  except
    ConnectionDef.Free;
    raise;
  end;
end;

procedure TInstantConnectionManagerForm.PopulateConnectionDefs;
var
  CurrentDef, Def: TInstantConnectionDef;
  I: Integer;
begin
  with ConnectionView.Items do
  begin
    BeginUpdate;
    try
      CurrentDef := CurrentConnectionDef;
      Clear;
      for I := 0 to Pred(ConnectionDefs.Count) do
      begin
        Def := ConnectionDefs[I];
        if SupportConnector(Def.ConnectorClass) then
          with Add do
          begin
            if not Def.IsBuilt then
              ImageIndex := 0
            else if IsConnected(Def) then
              ImageIndex := 2
            else
              ImageIndex := 1;
            Caption := Def.Name;
            Data := Def;
            SubItems.Add(Def.ConnectionTypeName);
          end;
      end;
      if Assigned(CurrentDef) then
        CurrentConnectionDef := CurrentDef;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TInstantConnectionManagerForm.RenameActionExecute(
  Sender: TObject);
begin
  with ConnectionView do
    if Assigned(Selected) then
      Selected.EditCaption;
end;

procedure TInstantConnectionManagerForm.SaveConnectionDefs;
var
  FileStream: TFileStream;
begin
  if DefsFileName = '' then
    Exit;
  FileStream := TFileStream.Create(DefsFileName, fmCreate);
  try
    InstantWriteObjectToStream(FileStream, ConnectionDefs);
  finally
    FileStream.Free;
  end;
end;

procedure TInstantConnectionManagerForm.SetCurrentConnectionDef(
  Value: TInstantConnectionDef);
var
  Item: TListItem;
begin
  Item := ConnectionView.FindData(0, Value, True, True);
  if Assigned(Item) then
  begin
    Item.Focused := True;
    Item.Selected := True;
  end;
end;

procedure TInstantConnectionManagerForm.SetFileName(const Value: string);
begin
  if Value <> FFileName then
  begin
    FFileName := Value;
    LoadConnectionDefs;
  end;
end;

procedure TInstantConnectionManagerForm.SetOnSupportConnector(
  Value: TInstantConnectorClassEvent);
begin
  if @Value <> @FOnSupportConnector then
  begin
    FOnSupportConnector := Value;
    UpdateMenu;
  end;
end;

procedure TInstantConnectionManagerForm.SetVisibleActions(
  Value: TInstantConnectionManagerActionTypes);
begin
  NewMenu.Visible := atNew in Value;
  EditAction.Visible := atEdit in Value;
  RenameAction.Visible := atRename in Value;
  DeleteAction.Visible := atDelete in Value;
  ConnectAction.Visible := atConnect in Value;
  DisconnectAction.Visible := atDisconnect in Value;
  BuildAction.Visible := atBuild in Value;
end;

function TInstantConnectionManagerForm.SupportConnector(
  ConnectorClass: TInstantConnectorClass): Boolean;
begin
  Result := True;
  if Assigned(FOnSupportConnector) then
    FOnSupportConnector(Self, ConnectorClass, Result);
end;

procedure TInstantConnectionManagerForm.UpdateMenu;
var
  I: Integer;
  ConnectorClass: TInstantConnectorClass;
  ConnectorClassList: TStringList;
  Item: TMenuItem;
begin
  ConnectorClassList := TStringList.Create;
  try
    for I := 0 to Pred(InstantConnectorClasses.Count) do
    begin
      ConnectorClass := InstantConnectorClasses[I];
      if SupportConnector(ConnectorClass) then
        ConnectorClassList.AddObject(
          ConnectorClass.ConnectionDefClass.ConnectionTypeName + ' Connection',
          Pointer(I));
    end;
    ConnectorClassList.Sort;
    NewMenu.Clear;
    for I := 0 to Pred(ConnectorClassList.Count) do
    begin
      Item := TMenuItem.Create(NewMenu);
      Item.Caption := ConnectorClassList[I];
      Item.Tag := Integer(ConnectorClassList.Objects[I]);
      Item.OnClick := NewMenuItemClick;
      NewMenu.Add(Item);
    end;
  finally
    ConnectorClassList.Free;
  end;
end;

{ TInstantConnectionManager }

constructor TInstantConnectionManager.Create(AOwner: TComponent);
begin
  inherited;
  FVisibleActions := [atNew, atEdit, atDelete, atRename, atConnect,
    atDisconnect, atBuild];
end;

function TInstantConnectionManager.Execute: Boolean;
begin
  Result := Form.ShowModal = mrOk;
end;

function TInstantConnectionManager.GetCaption: string;
begin
  if HasForm then
    Result := Form.Caption
  else
    Result := FCaption;
end;

function TInstantConnectionManager.GetCurrentConnectionDef: TInstantConnectionDef;
begin
  if HasForm then
    Result := Form.CurrentConnectionDef
  else
    Result := nil;
end;

function TInstantConnectionManager.GetForm: TInstantConnectionManagerForm;
begin
  if not Assigned(FForm) then
  begin
    FForm := TInstantConnectionManagerForm.Create(Self);
    if FCaption <> '' then
      FForm.Caption := FCaption;
    FForm.Model := Model;
    FForm.OnSupportConnector := FOnSupportConnector;
    FForm.VisibleActions := FVisibleActions;
    FForm.OnBuild := FOnBuild;
    FForm.OnConnect := FOnConnect;
    FForm.OnDisconnect := FOnDisconnect;
    FForm.OnEdit := FOnEdit;
    FForm.OnIsConnected := FOnIsConnected;
    FForm.OnPrepare := FOnPrepare;
    FForm.FileName := FFileName;
  end;
  Result := FForm;
end;

function TInstantConnectionManager.GetModel: TInstantModel;
begin
  Result := FModel;
  if not Assigned(FModel) then
    Result := InstantModel;
end;

function TInstantConnectionManager.HasForm: Boolean;
begin
  Result := Assigned(FForm);
end;

procedure TInstantConnectionManager.SetCaption(const Value: string);
begin
  if Value <> Caption then
  begin
    FCaption := Value;
    if HasForm then
      Form.Caption := FCaption;
  end;
end;

procedure TInstantConnectionManager.SetCurrentConnectionDef(
  Value: TInstantConnectionDef);
begin
  Form.CurrentConnectionDef := Value;
end;

procedure TInstantConnectionManager.SetFileName(const Value: string);
begin
  if Value <> FFileName then
  begin
    FFileName := Value;
    if Assigned(FForm) then
      FForm.FileName := FFileName;
  end;
end;

procedure TInstantConnectionManager.SetModel(Value: TInstantModel);
begin
  if Value <> Model then
  begin
    FModel := Value;
    if Assigned(FForm) then
      FForm.Model := Model;
  end;
end;

procedure TInstantConnectionManager.SetOnBuild(
  Value: TInstantConnectionDefEvent);
begin
  if @Value <> @FOnBuild then
  begin
    FOnBuild := Value;
    if Assigned(FForm) then
      FForm.OnBuild := FOnBuild;
  end;
end;

procedure TInstantConnectionManager.SetOnConnect(
  Value: TInstantConnectionDefEvent);
begin
  if @Value <> @FOnConnect then
  begin
    FOnConnect := Value;
    if Assigned(FForm) then
      FForm.OnConnect := FOnConnect;
  end;
end;

procedure TInstantConnectionManager.SetOnDisconnect(
  Value: TInstantConnectionDefEvent);
begin
  if @Value <> @FOnDisconnect then
  begin
    FOnDisconnect := Value;
    if Assigned(FForm) then
      FForm.OnDisconnect := FOnDisconnect;
  end;
end;

procedure TInstantConnectionManager.SetOnEdit(
  Value: TInstantConnectionDefEvent);
begin
  if @Value <> @FOnEdit then
  begin
    FOnEdit := Value;
    if Assigned(FForm) then
      FForm.OnEdit := FOnEdit;
  end;
end;

procedure TInstantConnectionManager.SetOnIsConnected(
  Value: TInstantConnectionDefEvent);
begin
  if @Value <> @FOnIsConnected then
  begin
    FOnIsConnected := Value;
    if Assigned(FForm) then
      FForm.OnIsConnected := FOnIsConnected;
  end;
end;

procedure TInstantConnectionManager.SetOnPrepare(
  Value: TInstantConnectorEvent);
begin
  if @Value <> @FOnPrepare then
  begin
    FOnPrepare := Value;
    if Assigned(FForm) then
      FForm.OnPrepare := FOnPrepare;
  end;
end;

procedure TInstantConnectionManager.SetOnSupportConnector(
  Value: TInstantConnectorClassEvent);
begin
  if @Value <> @FOnSupportConnector then
  begin
    FOnSupportConnector := Value;
    if Assigned(FForm) then
      FForm.OnSupportConnector := FOnSupportConnector;
  end;
end;

procedure TInstantConnectionManager.SetVisibleActions(
  Value: TInstantConnectionManagerActionTypes);
begin
  if Value <> FVisibleActions then
  begin
    FVisibleActions := Value;
    if Assigned(FForm) then
      FForm.VisibleActions := Value;
  end;
end;

end.
