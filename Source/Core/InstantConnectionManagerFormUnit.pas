(*
 *   InstantObjects
 *   Connection Manager Form
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
 * Carlo Barazzetta, Nando Dessena, David Taylor
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantConnectionManagerFormUnit;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ImgList, Menus, ActnList, ExtCtrls, StdActns,
  System.Actions,
  InstantConnectionManager, InstantClasses, InstantPersistence, InstantMetadata,
  System.ImageList;

type
  TInstantConnectionManagerForm = class(TForm)
    ActionList: TActionList;
    BuildAction: TAction;
    BuildItem: TMenuItem;
    ConnectAction: TAction;
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
    BottomPanel: TPanel;
    BuildButton: TButton;
    ButtonsPanel: TPanel;
    ConnectButton: TButton;
    CloseButton: TButton;
    FileOpenAction: TAction;
    N2: TMenuItem;
    Open1: TMenuItem;
    EvolveAction: TAction;
    EvolveButton: TButton;
    EvolveItem: TMenuItem;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure BuildActionExecute(Sender: TObject);
    procedure ConnectActionExecute(Sender: TObject);
    procedure ConnectionViewDblClick(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure DisconnectActionExecute(Sender: TObject);
    procedure EditActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RenameActionExecute(Sender: TObject);
    procedure ConnectionViewEditedVCL(Sender: TObject; Item: TListItem;
      var S: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FileOpenActionBeforeExecute(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure ConnectActionUpdate(Sender: TObject);
    procedure DisconnectActionUpdate(Sender: TObject);
    procedure EvolveActionExecute(Sender: TObject);
  private
    FModel: TInstantModel;
    FOnBuild: TInstantConnectionDefEvent;
    FOnEvolve: TInstantConnectionDefEvent;
    FOnConnect: TInstantConnectionDefEvent;
    FOnDisconnect: TInstantConnectionDefEvent;
    FOnEdit: TInstantConnectionDefEvent;
    FOnIsConnected: TInstantConnectionDefEvent;
    FOnPrepare: TInstantConnectorEvent;
    FOnSupportConnector: TInstantConnectorClassEvent;
    FConnectionManager: TInstantConnectionManager;
    FOpenDialog: TOpenDialog;
    FTitle: string;
    // Used to detect whether the database builder form has actually built
    // the database.
    FIsLastDatabaseBuilt: Boolean;
    function ConfirmDlg(const Text: string): Boolean;
    function GetCurrentConnectionDef: TInstantConnectionDef;
    function GetVisibleActions: TInstantConnectionManagerActionTypes;
    procedure SetCurrentConnectionDef(Value: TInstantConnectionDef);
    procedure SetFileName(const Value: string);
    procedure SetVisibleActions(Value: TInstantConnectionManagerActionTypes);
    procedure SetOnSupportConnector(Value: TInstantConnectorClassEvent);
    procedure UpdateMenu;
    function GetConnectionDefs: TInstantConnectionDefs;
    function GetFileName: string;
    procedure UpdateCaption;
    procedure NewMenuItemClick(Sender: TObject);
    procedure SetConnectionManager(const Value: TInstantConnectionManager);
    function GetOpenDialog: TOpenDialog;
    procedure AfterDBBuilderBuild(Sender: TObject;
      Connector: TInstantConnector);
  protected
    procedure Build(ConnectionDef: TInstantConnectionDef);
    procedure Connect(ConnectionDef: TInstantConnectionDef);
    procedure Disconnect(ConnectionDef: TInstantConnectionDef);
    function Edit(ConnectionDef: TInstantConnectionDef): Boolean;
    procedure Evolve(ConnectionDef: TInstantConnectionDef);
    function DoConnect(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
    function DoDisconnect(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
    function DoEdit(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
    procedure DoPrepare(Connector: TInstantConnector); virtual;
    function IsConnected(ConnectionDef: TInstantConnectionDef): Boolean;
    procedure PopulateConnectionDefs;
    function SupportConnector(ConnectorClass: TInstantConnectorClass): Boolean;
    property ConnectionDefs: TInstantConnectionDefs read GetConnectionDefs;
    property FileOpenDialog: TOpenDialog read GetOpenDialog;
    function DoBuild(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
    function DoEvolve(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
  public
    function IsManagerConnected: Boolean;
    property CurrentConnectionDef: TInstantConnectionDef read GetCurrentConnectionDef write SetCurrentConnectionDef;
    property FileName: string read GetFileName write SetFileName;
    property Model: TInstantModel read FModel write FModel;
    property VisibleActions: TInstantConnectionManagerActionTypes read GetVisibleActions write SetVisibleActions;
    property OnBuild: TInstantConnectionDefEvent read FOnBuild write FOnBuild;
    property OnEvolve: TInstantConnectionDefEvent read FOnEvolve write FOnEvolve;
    property OnConnect: TInstantConnectionDefEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TInstantConnectionDefEvent read FOnDisconnect write FOnDisconnect;
    property OnEdit: TInstantConnectionDefEvent read FOnEdit write FOnEdit;
    property OnIsConnected: TInstantConnectionDefEvent read FOnIsConnected write FOnIsConnected;
    property OnPrepare: TInstantConnectorEvent read FOnPrepare write FOnPrepare;
    property OnSupportConnector: TInstantConnectorClassEvent read FOnSupportConnector write SetOnSupportConnector;
    property ConnectionManager: TInstantConnectionManager read FConnectionManager write SetConnectionManager;
  end;

implementation

{$R *.dfm}
{$R connectionmanagerimages.res}

uses
  InstantImageUtils, InstantConsts, InstantDBEvolverFormUnit,
  System.UITypes,
  InstantDBBuilderFormUnit;

const
  SNoConnectorsFound = '< No Brokers Installed >';

procedure DefaultConnectionManagerExecutor(ConnectionManager: TInstantConnectionManager);
var
  ConnectionManagerForm: TInstantConnectionManagerForm;
begin
  ConnectionManagerForm := TInstantConnectionManagerForm.Create(nil);
  try
    ConnectionManagerForm.ConnectionManager := ConnectionManager;
    ConnectionManagerForm.ShowModal;
  finally
    ConnectionManagerForm.Free;
  end;
end;

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
  Connected := IsManagerConnected;
  EnableAction(EditAction, HasItem and not Connected);
  EnableAction(RenameAction, HasItem);
  EnableAction(DeleteAction, HasItem and not Connected);
  EnableAction(BuildAction, HasItem and not Connected);
  EnableAction(EvolveAction, HasItem and not Connected);
  EnableAction(ConnectAction, HasItem and not Connected);
  EnableAction(DisconnectAction, HasItem and Connected);
  EnableAction(FileOpenAction, atOpen in VisibleActions);
  if Connected then
    ConnectButton.Action := DisconnectAction else
    ConnectButton.Action := ConnectAction;
  ConnectButton.Default := not ConnectionView.IsEditing;
  CloseButton.Cancel := not ConnectionView.IsEditing;
end;

procedure TInstantConnectionManagerForm.Build(
  ConnectionDef: TInstantConnectionDef);
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

procedure TInstantConnectionManagerForm.Evolve(
  ConnectionDef: TInstantConnectionDef);
begin
  DoEvolve(ConnectionDef);
  PopulateConnectionDefs;
end;

procedure TInstantConnectionManagerForm.BuildActionExecute(Sender: TObject);
begin
  Build(CurrentConnectionDef);
end;

procedure TInstantConnectionManagerForm.EvolveActionExecute(Sender: TObject);
begin
  Evolve(CurrentConnectionDef);
end;

function TInstantConnectionManagerForm.ConfirmDlg(const Text: string): Boolean;
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

procedure TInstantConnectionManagerForm.ConnectionViewEditedVCL(Sender: TObject;
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
    ConfirmDlg(Format(SDeleteConnectionConfirmation, [ConnectionDef.Name])) then
  begin
    ConnectionDefs.Remove(ConnectionDef);
    PopulateConnectionDefs;
  end;
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

procedure TInstantConnectionManagerForm.AfterDBBuilderBuild(Sender: TObject;
  Connector: TInstantConnector);
begin
  FIsLastDatabaseBuilt := True;
  DoPrepare(Connector);
end;

procedure TInstantConnectionManagerForm.DisconnectActionExecute(Sender: TObject);
begin
  Disconnect(CurrentConnectionDef);
end;

function TInstantConnectionManagerForm.DoBuild(
  ConnectionDef: TInstantConnectionDef): Boolean;
var
  Connector: TInstantConnector;
  DBBuilderForm: TInstantDBBuilderForm;
  DBBuilderSupported: Boolean;
  SaveCursor: TCursor;
begin
  if Assigned(FOnBuild) then
  begin
    Result := False;
    FOnBuild(Self, ConnectionDef, Result);
    Exit;
  end;
  if not Assigned(ConnectionDef) then
  begin
    Result := False;
    Exit;
  end;
  Connector := ConnectionDef.CreateConnector(nil);
  try
    DBBuilderSupported := Connector.Broker.IsCatalogSupported;
    // For catalog-less brokers the build is immediate, so it needs
    // confirmation now.
    if not DBBuilderSupported then
    begin
      if not ConfirmDlg(Format(SDatabaseBuildConfirmation, [ConnectionDef.Name])) then
      begin
        Result := False;
        Exit;
      end;
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        Application.ProcessMessages;
        Connector.BuildDatabase(Model);
        DoPrepare(Connector);
        Result := True;
      finally
        Screen.Cursor := SaveCursor;
      end;
    end
    else
    begin
      DBBuilderForm := TInstantDBBuilderForm.Create(nil);
      try
        DBBuilderForm.Connector := Connector;
        DBBuilderForm.TargetModel := Model;
        DBBuilderForm.AfterBuild := AfterDBBuilderBuild;
        FIsLastDatabaseBuilt := False;
        DBBuilderForm.Execute;
        Result := FIsLastDatabaseBuilt;
      finally
        DBBuilderForm.Free;
      end;
    end;
  finally
    Connector.Free;
  end;
end;

function TInstantConnectionManagerForm.DoEvolve(
  ConnectionDef: TInstantConnectionDef): Boolean;
var
  Connector: TInstantConnector;
  DBEvolverForm: TInstantDBEvolverForm;
begin
  if Assigned(FOnEvolve) then
  begin
    Result := False;
    FOnEvolve(Self, ConnectionDef, Result);
    Exit;
  end;
  if not Assigned(ConnectionDef) then
  begin
    Result := False;
    Exit;
  end;
  Connector := ConnectionDef.CreateConnector(nil);
  try
    if not Connector.Broker.IsCatalogSupported then
      raise EInstantError.Create(SDatabaseEvolutionNonSupported);
    DBEvolverForm := TInstantDBEvolverForm.Create(nil);
    try
      DBEvolverForm.Connector := Connector;
      DBEvolverForm.TargetModel := Model;
      DBEvolverForm.Execute;
      Result := True;
    finally
      DBEvolverForm.Free;
    end;
  finally
    Connector.Free;
  end;
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
  Font.Assign(Screen.IconFont);
  LoadMultipleImages(ConnectionImages, 'IO_CONNECTIONMANAGERIMAGES', HInstance);
  BorderStyle := bsSizeable;
  ConnectionView.OnEdited := ConnectionViewEditedVCL;
  ConnectionView.HideSelection := False;
  ConnectionView.SortType := stText;
  ConnectionView.SmallImages := ConnectionImages;
  ConnectionView.Columns[0].Width := 225;
  ConnectionView.Columns[1].Width := 80;
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
  Result := FConnectionManager.ConnectionDefs;
end;

function TInstantConnectionManagerForm.GetCurrentConnectionDef: TInstantConnectionDef;
begin
  with ConnectionView do
    if Assigned(Selected) and Assigned(Selected.Data) then
      Result := Selected.Data
    else
      Result := nil;
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
  if EvolveAction.Visible then Include(Result, atEvolve);
  if FileOpenAction.Visible then Include(Result, atOpen);
end;

function TInstantConnectionManagerForm.IsConnected(
  ConnectionDef: TInstantConnectionDef): Boolean;
begin
  Result := False;
  if Assigned(FOnIsConnected) then
    FOnIsConnected(Self, ConnectionDef, Result)
  else if Assigned(ConnectionManager.OnIsConnected) then
    ConnectionManager.OnIsConnected(ConnectionManager,ConnectionDef,Result);
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
            SubItems.Add(AInstantStreamFormatStr[Def.BlobStreamFormat]);
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
  FConnectionManager.FileName := Value;
  PopulateConnectionDefs;
  UpdateCaption;
end;

procedure TInstantConnectionManagerForm.SetOnSupportConnector(
  Value: TInstantConnectorClassEvent);
begin
  if Addr(Value) <> Addr(FOnSupportConnector) then
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
  EvolveAction.Visible := atEvolve in Value;
  FileOpenAction.Visible := atOpen in Value;
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

    if (ConnectorClassList.Count < 1) then
    begin
      Item := TMenuItem.Create(NewMenu);
      Item.Caption := SNoConnectorsFound;
      NewMenu.Add(Item);
    end;
  finally
    ConnectorClassList.Free;
  end;
end;

function TInstantConnectionManagerForm.GetFileName: string;
begin
  Result := FConnectionManager.FileName;
end;

procedure TInstantConnectionManagerForm.UpdateCaption;
begin
  if FileName <> '' then
    Caption := FTitle + ' - '+ ExtractFileName(FileName)
  else
    Caption := FTitle;
end;

procedure TInstantConnectionManagerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FConnectionManager.SaveConnectionDefs;
end;

procedure TInstantConnectionManagerForm.FileOpenActionBeforeExecute(
  Sender: TObject);
begin
  FileOpenDialog.FileName := FileName;
end;

procedure TInstantConnectionManagerForm.SetConnectionManager(
  const Value: TInstantConnectionManager);
begin
  if FConnectionManager <> Value then
  begin
    if Value.Caption <> '' then
      FTitle := Value.Caption
    else
      FTitle := Caption;
    FConnectionManager := Value;
    Model := Value.Model;
    OnSupportConnector := Value.OnSupportConnector;
    VisibleActions := Value.VisibleActions;
    OnBuild := Value.OnBuild;
    OnEvolve := Value.OnEvolve;
    OnConnect := Value.OnConnect;
    OnDisconnect := Value.OnDisconnect;
    OnEdit := Value.OnEdit;
    OnIsConnected := Value.OnIsConnected;
    OnPrepare := Value.OnPrepare;
    FileName := Value.FileName;
    UpdateCaption;
  end;
end;

function TInstantConnectionManagerForm.GetOpenDialog: TOpenDialog;
begin
  if not Assigned(FOpenDialog) then
  begin
    FOpenDialog := TOpenDialog.Create(self);
    FOpenDialog.Filter := SConnectionDefFilter;
  end;  
  Result := FOpenDialog;
end;

procedure TInstantConnectionManagerForm.FileOpenActionExecute(
  Sender: TObject);
begin
  FileOpenDialog.InitialDir := ExtractFilePath(FileName);
  if FileOpenDialog.Execute then
    FileName := FileOpenDialog.FileName;
end;

function TInstantConnectionManagerForm.IsManagerConnected: Boolean;
begin
  Result := ConnectionManager.IsConnected;
end;

procedure TInstantConnectionManagerForm.ConnectActionUpdate(
  Sender: TObject);
begin
  ConnectAction.Enabled := Assigned(CurrentConnectionDef) and not IsManagerConnected;
end;

procedure TInstantConnectionManagerForm.DisconnectActionUpdate(Sender: TObject);
begin
  DisconnectAction.Enabled := IsManagerConnected;
end;

initialization
  RegisterConnectionManagerExecutor(DefaultConnectionManagerExecutor);

finalization
  RegisterConnectionManagerExecutor(nil);

end.
