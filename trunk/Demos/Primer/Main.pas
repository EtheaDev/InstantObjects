unit Main;

interface

{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, ComCtrls, ToolWin, Grids, DBGrids, ImgList, ActnList,
  StdCtrls, InstantPersistence, BasicView, Stopwatch, InstantConnectionManager;

type
  TMainForm = class(TForm)
    ActionImages: TImageList;
    ActionList: TActionList;
    ConnectionManager: TInstantConnectionManager;
    ConnectionManagerAction: TAction;
    ConnectionManagerButton: TToolButton;
    ConnectionManagerItem: TMenuItem;
    ExitItem: TMenuItem;
    FileMenu: TMenuItem;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    RandomDataAction: TAction;
    RandomDataButton: TToolButton;
    RandomDataItem: TMenuItem;
    SideBar: TListView;
    SideBarImages: TImageList;
    SideBarPanel: TPanel;
    SideBarSplitter: TSplitter;
    SideBarTopSpacer: TBevel;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    WorkClientPanel: TPanel;
    WorkPanel: TPanel;
    WorkTitleLabel: TLabel;
    WorkTitlePanel: TPanel;
    WorkTitleSpacer: TBevel;
    N2: TMenuItem;
    ExportItem: TMenuItem;
    ImportItem: TMenuItem;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure SideBarSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ConnectionManagerActionExecute(Sender: TObject);
    procedure ConnectionManagerConnect(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
    procedure ConnectionManagerDisconnect(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
    procedure ConnectionManagerPrepare(Sender: TObject;
      Connector: TInstantConnector);
    procedure ConnectionManagerIsConnected(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
    procedure RandomDataActionExecute(Sender: TObject);
    procedure StopwatchStep(Sender: TObject);
    procedure StopwatchStop(Sender: TObject);
    procedure SubViewShowStatus(Sender: TObject; Text: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExportItemClick(Sender: TObject);
    procedure ImportItemClick(Sender: TObject);
  private
    FActiveSubView: TBasicViewForm;
    FConnectionDef: TInstantConnectionDef;
    FConnector: TInstantConnector;
    FStopwatch: TStopwatch;
    FSubViewList: TList;
    procedure CreateRandomContacts(Count: Integer);
    procedure CreateSubViews;
    function GetIsConnected: Boolean;
    function GetStatusText: string;
    function GetStopwatch: TStopwatch;
    function GetSubViewList: TList;
    function GetSubViewCount: Integer;
    function GetSubViews(Index: Integer): TBasicViewForm;
    procedure InitSideBar;
    procedure SetActiveSubView(const Value: TBasicViewForm);
    procedure SetStatusText(const Value: string);
    property SubViewList: TList read GetSubViewList;
    function GetConnectionName: string;
  protected
    procedure UpdateActions; override;
    procedure UpdateControls;
    procedure UpdateStatus;
    property StatusText: string read GetStatusText write SetStatusText;
    property Stopwatch: TStopwatch read GetStopwatch;
    property SubViewCount: Integer read GetSubViewCount;
    property SubViews[Index: Integer]: TBasicViewForm read GetSubViews;
  public
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Reset;
    property ActiveSubView: TBasicViewForm read FActiveSubView write SetActiveSubView;
    property Connector: TInstantConnector read FConnector;
    property ConnectionName: string read GetConnectionName;
    property IsConnected: Boolean read GetIsConnected;
  end;

var
  MainForm: TMainForm;

implementation

uses
  Contnrs, Model, Welcome, MainData, RandomData, DemoData, Utility, ContactView,
  PerformanceView, DemoDataRequest, InstantPresentation, InstantClasses,
  HelpView, QueryView,

{ Note: This demo attempts to include brokers for the data access
  layers supported natively by Delphi. To include additional brokers,
  please add the broker unit(s) to the following list. If you have not
  installed all brokers, please remove the missing broker unit(s) from
  the list. }

  {$IFNDEF VER130}
  InstantDBX,
  {$ENDIF}
  InstantADO,
  InstantBDE,
  InstantIBX;

{$R *.DFM}

const
  XMLFilter = 'XML files (*.xml)|*.xml';
  XMLExt = 'xml';
  XMLStartTag = '<Objects>';
  XMLEndTag = '</Objects>';

procedure SaveObjectsToFile(Objects: TList; const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.Write(XMLStartTag, Length(XMLStartTag));
    InstantWriteObjects(Stream, sfXML, Objects);
    Stream.Write(XMLEndTag, Length(XMLEndTag));
  finally
    Stream.Free;
  end;
end;

procedure LoadObjectsFromFile(Objects: TList; const FileName: string);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    Stream.Size := Stream.Size - Length(XMLEndTag);
    Stream.Position := Length(XMLStartTag);
    InstantReadObjects(Stream, sfXML, Objects);
  finally
    Stream.Free;
  end;
end;

procedure RetrieveObjects(AConnector: TInstantConnector; 
  AClass: TInstantObjectClass; AObjects: TList);
var
  I: Integer;
  Obj: TInstantObject;
begin
  with TInstantSelector.Create(nil) do
  try
    Connector := AConnector;
    Command.Text := 'SELECT * FROM ANY ' + AClass.ClassName;
    Open;
    for I := 0 to Pred(ObjectCount) do
    begin
      Obj := Objects[I] as TInstantObject;
      Obj.AddRef;
      AObjects.Add(Obj);
    end;
  finally
    Free;
  end;
end;

procedure StoreObjects(Connector: TInstantConnector; Objects: TList);
var
  I: Integer;
begin
  Connector.StartTransaction;
  try
    for I := 0 to Pred(Objects.Count) do
      TInstantObject(Objects[I]).Store;
    Connector.CommitTransaction;
  except
    Connector.RollbackTransaction;
    raise;
  end;
end;

procedure ExportObjects(AClass: TInstantObjectClass; 
  const FileName: string);
var
  Objects: TObjectList;
begin
  Objects := TObjectList.Create;
  try
    RetrieveObjects(InstantDefaultConnector, AClass, Objects);
    SaveObjectsToFile(Objects, FileName);
  finally
    Objects.Free;
  end; 
end;

procedure ImportObjects(const FileName: string);
var
  Objects: TObjectList;
begin
  Objects := TObjectList.Create;
  try
    LoadObjectsFromFile(Objects, FileName);
    StoreObjects(InstantDefaultConnector, Objects);
  finally
    Objects.Free;
  end;
end;

{ TMainForm }

procedure TMainForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  RandomDataAction.Enabled := IsConnected;
end;

procedure TMainForm.Connect;
var
  I: Integer;
begin
  if not Assigned(FConnector) then
    Exit;
  BeginBusy;
  try
    FConnector.Connect;
    for I := 0 to Pred(SubViewCount) do
      SubViews[I].Connect;
    MainDataModule.Connect;
    UpdateControls;
    UpdateStatus;
  finally
    EndBusy;
  end;
end;

procedure TMainForm.ConnectionManagerActionExecute(Sender: TObject);
begin
  ConnectionManager.Execute;
end;

procedure TMainForm.ConnectionManagerConnect(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Application.ProcessMessages;
  Disconnect;
  FConnector := ConnectionDef.CreateConnector(Self);
  try
    FConnector.IsDefault := True;
    FConnectionDef := ConnectionDef;
    Connect;
    Result := True;
  except
    FConnectionDef := nil;
    FreeAndNil(FConnector);
    raise;
  end;
end;

procedure TMainForm.ConnectionManagerDisconnect(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Disconnect;
  Result := True;
end;

procedure TMainForm.ConnectionManagerIsConnected(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Result := ConnectionDef = FConnectionDef;
end;

procedure TMainForm.ConnectionManagerPrepare(Sender: TObject;
  Connector: TInstantConnector);
var
  DefaultConnector: TInstantConnector;
begin
  DefaultConnector := InstantDefaultConnector;
  Connector.IsDefault := True;
  try
    Connector.StartTransaction;
    try
      CreateCountries;
      CreateCategories;
      Connector.CommitTransaction;
    except
      Connector.RollbackTransaction;
      raise;
    end;
    if Confirm('Create random data?') then
      CreateRandomContacts(50);
  finally
    if Assigned(DefaultConnector) then
      DefaultConnector.IsDefault := True;
  end;
end;

procedure TMainForm.CreateRandomContacts(Count: Integer);
var
  Companies: TObjectList;

  function CreateRandomContact: TContact;
  var
    Company: TCompany;
  begin
    if Random(2) = 0 then
    begin
      if (Random(2) = 0) and (Companies.Count > 10) then
        Company := Companies[Random(Companies.Count)] as TCompany
      else
        Company := nil;
      Result := CreateRandomPerson(Company);
    end else
    begin
      Result := CreateRandomCompany;
      if Random(2) = 0 then
      begin
        if Companies.Count > 50 then
          Companies.Delete(0);
        Result.AddRef;
        Companies.Add(Result);
      end;
    end;
  end;

var
  I, CommitCount: Integer;
begin
  GetAsyncKeyState(VK_ESCAPE);
  if (InstantDefaultConnector is TInstantBDEConnector) and
    (TInstantBDEConnector(InstantDefaultConnector).DriverType = dtStandard) then
    CommitCount := 200
  else
    CommitCount := 1000;
  Randomize;
  Companies := TObjectList.Create;
  Stopwatch.Start(Count);
  try
    InstantDefaultConnector.StartTransaction;
    try
      for I := 0 to Pred(Count) do
      begin
        with CreateRandomContact do
        try
          Store;
        finally
          Free;
        end;
        Stopwatch.Step;
        if GetAsyncKeyState(VK_ESCAPE) <> 0 then
        begin
          Application.ProcessMessages;
          if Confirm('Abort?') then
            Break
          else
            Application.ProcessMessages;
        end;
        if (Succ(I) mod CommitCount) = 0 then
          with InstantDefaultConnector do
          begin
            CommitTransaction;
            StartTransaction;
          end;
      end;
      InstantDefaultConnector.CommitTransaction;
    except
      InstantDefaultConnector.RollbackTransaction;
      raise;
    end;
  finally
    Stopwatch.Stop;
    Companies.Free;
  end;
end;

procedure TMainForm.CreateSubViews;

  procedure CreateSubView(FormClass: TBasicViewFormClass);
  var
    Form: TBasicViewForm;
  begin
    Form := FormClass.Create(Self);
    Form.OnShowStatus := SubViewShowStatus;
    SubViewList.Add(Form);
  end;

begin
  CreateSubView(THelpViewForm);
  CreateSubView(TContactViewForm);
  CreateSubView(TQueryViewForm);
  CreateSubView(TPerformanceViewForm);
  InitSideBar;
end;

destructor TMainForm.Destroy;
begin
  inherited;
  FSubViewList.Free;
  FStopwatch.Free;
end;

procedure TMainForm.Disconnect;
var
  I: Integer;
begin
  BeginBusy;
  try
    for I := 0 to Pred(SubViewCount) do
      SubViews[I].Disconnect;
    MainDataModule.Disconnect;
    if Assigned(FConnector) then
    begin
      FConnector.Disconnect;
      FreeAndNil(FConnector);
    end;
    FConnectionDef := nil;
    UpdateControls;
    UpdateStatus;
  finally
    EndBusy;
  end;
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ExportItemClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    Filter := XMLFilter;
    DefaultExt := XMLExt;
    if Execute then
      ExportObjects(TContact, FileName);
  finally
    Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Disconnect;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ConnectionManager.FileName := ChangeFileExt(Application.ExeName, '.con');
  CreateSubViews;
  UpdateStatus;
end;

function TMainForm.GetConnectionName: string;
begin
  if Assigned(FConnectionDef) then
    Result := FConnectionDef.Name
  else
    Result := 'Not Connected';
end;

function TMainForm.GetIsConnected: Boolean;
begin
  Result := Assigned(FConnector) and FConnector.Connected;
end;

function TMainForm.GetStatusText: string;
begin
  Result := StatusBar.Panels[0].Text;
end;

function TMainForm.GetStopwatch: TStopwatch;
begin
  if not Assigned(FStopwatch) then
  begin
    FStopwatch := TStopwatch.Create;
    FStopwatch.OnStep := StopwatchStep;
    FStopwatch.OnStop := StopwatchStop;
  end;
  Result := FStopwatch;
end;

function TMainForm.GetSubViewCount: Integer;
begin
  Result := SubViewList.Count;
end;

function TMainForm.GetSubViewList: TList;
begin
  if not Assigned(FSubViewList) then
    FSubViewList := TList.Create;
  Result := FSubViewList;
end;

function TMainForm.GetSubViews(Index: Integer): TBasicViewForm;
begin
  Result := SubViewList[Index];
end;

procedure TMainForm.ImportItemClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    Filter := XMLFilter;
    if Execute then
    begin
      ImportObjects(FileName);
      Reset;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.InitSideBar;

  procedure AddShortcut(Form: TForm; Index: Integer);
  begin
    with SideBar.Items.Add do
    begin
      Caption := Form.Caption;
      ImageIndex := Index;
      Data := Form;
    end;
  end;

var
  I: Integer;
begin
  SideBar.Items.Clear;
  for I := 0 to Pred(SubViewCount) do
    AddShortcut(SubViews[I], I);
  SideBar.Selected := SideBar.Items[0];
end;

procedure TMainForm.RandomDataActionExecute(Sender: TObject);
begin
  with TDemoDataRequestForm.Create(nil) do
  try
    Count := 100;
    if ShowModal = mrOk then
    begin
      CreateRandomContacts(Count);
      Reset;
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.Reset;
var
  I: Integer;
begin
  BeginBusy;
  try
    Update;
    for I := 0 to Pred(SubViewCount) do
      SubViews[I].Reset;
  finally
    EndBusy;
  end;
end;

procedure TMainForm.SetActiveSubView(const Value: TBasicViewForm);
begin
  if Value <> FActiveSubView then
  begin
    if Assigned(FActiveSubView) then
      FActiveSubView.Hide;
    FActiveSubView := Value;
    if Assigned(FActiveSubView) then
    begin
      WorkTitleLabel.Caption := FActiveSubView.Caption;
      with FActiveSubView do
      begin
        BorderStyle := bsNone;
        Align := alClient;
        Parent := WorkClientPanel;
        Height := Parent.ClientHeight;
        Width := Parent.ClientWidth;
        Show;
        if CanFocus then
          SetFocus;
      end;
    end;
    StatusText := '';
    UpdateControls;
  end;
end;

procedure TMainForm.SetStatusText(const Value: string);
begin
  StatusBar.Panels[0].Text := Value;
  StatusBar.Update;
end;

procedure TMainForm.SideBarSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(SideBar.Selected) then
    ActiveSubView := SideBar.Selected.Data;
end;

procedure TMainForm.StopwatchStep(Sender: TObject);
begin
  with Stopwatch do
    StatusText := IntToStr(StepPercent) + '%';
end;

procedure TMainForm.StopwatchStop(Sender: TObject);
begin
  with Stopwatch do
    ShowMessage(Format('%d objects processed in %d ms. (%d o/s)',
      [StepIndex, TotalTime, Round(StepIndex / (TotalTime / 1000))]));
  StatusText := '';
end;

procedure TMainForm.SubViewShowStatus(Sender: TObject; Text: string);
begin
  StatusText := Text;
end;

procedure TMainForm.UpdateActions;
{$J+}
const
  FirstTime: Boolean = True;
{$J-}
begin
  inherited;
  if FirstTime then
  begin
    FirstTime := False;
    with TWelcomeForm.Create(nil) do
    try
      ShowModal;
    finally
      Free;
    end;
  end;
  ImportItem.Enabled := IsConnected;
  ExportItem.Enabled := IsConnected;
end;

procedure TMainForm.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to Pred(SubViewCount) do
    SubViews[I].UpdateControls;
end;

procedure TMainForm.UpdateStatus;
const
  Pad = '      ';
begin
  with StatusBar do
  begin
    Panels[1].Text := 'Connection: ' + ConnectionName + Pad;
    Update;
  end;
end;

end.
