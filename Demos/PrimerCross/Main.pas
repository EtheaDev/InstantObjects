unit Main;

interface
{$IFDEF VER130}{$DEFINE MSWINDOWS}{$ENDIF}
{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Menus, ExtCtrls, ComCtrls, ToolWin, Grids, DBGrids, ImgList, ActnList,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls,QTypes,
  QMenus, QExtCtrls, QComCtrls, QGrids, QDBGrids, QImgList, QActnList,
{$ENDIF}
  InstantPersistence, BasicView, Stopwatch,
  InstantConnectionManagerForm, InstantConnectionManager;

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
    N3: TMenuItem;
    ExportModelItem: TMenuItem;
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
    procedure ExportModelItemClick(Sender: TObject);
  private
    FActiveSubView: TBasicViewForm;
    FConnectionDef: TInstantConnectionDef;
    FConnector: TInstantConnector;
    FStopwatch: TStopwatch;
    FSubViewList: TList;
    procedure CreateRandomContacts(Count: Integer; LoadPictures : boolean);
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
    procedure AssignRandomPicture(Male: boolean; InstantBlob : TInstantBlob);
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

{$IFDEF MSWINDOWS}
  {$R PrimerImages.res}
{$ENDIF}

var
  MainForm: TMainForm;

implementation

uses
  Contnrs, Model, Welcome, MainData, RandomData, DemoData, Utility, ContactView, PerformanceView,
{$IFDEF MSWINDOWS}
  HelpView, JPeg,
{$ENDIF}
{$IFDEF LINUX}
  HelpViewK3,
{$ENDIF}
  DemoDataRequest, InstantPresentation, InstantClasses,
  QueryView, InstantImageUtils,

{ Note: This demo attempts to include brokers for the data access
  layers supported natively by Delphi. To include additional brokers,
  please add the broker unit(s) to the following list. If you have not
  installed all brokers, please remove the missing broker unit(s) from
  the list. }

{$IFDEF MSWINDOWS}
  {$IFNDEF VER130}
  InstantDBX,
  {$ENDIF}
  InstantADO, InstantBDE, InstantIBX,
{$ENDIF}
{$IFDEF LINUX}
  InstantDBX,
{$ENDIF}
  InstantXML;

{$R *.dfm}

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
      RandomDataActionExecute(nil);
  finally
    if Assigned(DefaultConnector) then
      DefaultConnector.IsDefault := True;
  end;
end;

procedure TMainForm.CreateRandomContacts(Count: Integer; LoadPictures : boolean);
var
  Companies: TObjectList;
  Gender: TGender;

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
      Result := CreateRandomPerson(Company, Gender);
      if LoadPictures then
        AssignRandomPicture(Gender=gnMale, TPerson(Result)._Picture);
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
{$IFDEF MSWINDOWS}
  GetAsyncKeyState(VK_ESCAPE);
{$ENDIF}
  CommitCount := 200;
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
{$IFDEF MSWINDOWS}
        if GetAsyncKeyState(VK_ESCAPE) <> 0 then
        begin
          Application.ProcessMessages;
          if Confirm('Abort?') then
            Break
          else
            Application.ProcessMessages;
        end;
{$ENDIF}
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
    Form.FormCreate(Form);
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
    InitialDir := ExtractFilePath(Application.ExeName);
    if Execute then
      ExportObjects(TContact, FileName);
  finally
    Free;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Disconnect;
  if FActiveSubView <> nil then
    FActiveSubView.FormHide(FActiveSubView);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
{$IFDEF MSWINDOWS}
  LoadMultipleImages(SideBarImages,'MAINSIDEBARIMAGES',HInstance);
  LoadMultipleImages(ActionImages,'MAINACTIONIMAGES',HInstance);
  with SideBar do
  begin
    BorderWidth := 4;
    HoverTime := 0;
    HotTrackStyles := [htHandPoint];
    IconOptions.Arrangement := iaLeft;
    LargeImages := SideBarImages;
  end;
{$ENDIF}
{$IFDEF LINUX}
  LoadMultipleImages(ActionImages,ExtractFilePath(Application.ExeName)+'MAINACTIONIMAGES.BMP');
  LoadMultipleImages(SideBarImages,ExtractFilePath(Application.ExeName)+'MAINSIDEBARIMAGES.BMP');
  SideBar.Images := SideBarImages;
{$ENDIF}

// To use XML format for ConnectionManager file:
  ConnectionManager.FileFormat := sfXML;
  ConnectionManager.FileName := ChangeFileExt(Application.ExeName, '.xml');

// To use binary format for ConnectionManager file:
//  ConnectionManager.FileFormat := sfBinary;
//  ConnectionManager.FileName := ChangeFileExt(Application.ExeName, '.con');


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

  procedure AddShortcut(Form: TBasicViewForm; Index: Integer);
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
var
  LoadPictures : boolean;
begin
  with TDemoDataRequestForm.Create(nil) do
  try
    Count := 100;
    if ShowModal = mrOk then
    begin
      LoadPictures := PicturesCheckBox.Checked;
      CreateRandomContacts(Count, LoadPictures);
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
    begin
      FActiveSubView.FormHide(FActiveSubView);
    end;  
    FActiveSubView := Value;
    if Assigned(FActiveSubView) then
    begin
      WorkTitleLabel.Caption := FActiveSubView.Caption;
      with FActiveSubView do
      begin
        Parent := WorkClientPanel;
        SetBounds(SideBarSplitter.Left+SideBarSplitter.Width+self.Height-self.ClientHeight,
          WorkTitleSpacer.Top+WorkTitleSpacer.Height,
          WorkClientPanel.Width, WorkClientPanel.Height);
        Align := alClient;
        Height := Parent.ClientHeight;
        Width := Parent.ClientWidth;
        FormShow(FActiveSubView);
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
var
  ms : word;
{$IFDEF LINUX}
  h,m,s : word;
{$ENDIF}
begin
  with Stopwatch do
  begin
{$IFDEF LINUX}
    decodetime(TotalTime,h,m,s,ms);
    ms := ms+s*1000+m*1000*60+h*1000*60*24;
{$ENDIF}
{$IFDEF MSWINDOWS}
    ms := TotalTime;
{$ENDIF}
    ShowMessage(Format('%d objects processed in %d ms. (%d o/s)',
      [StepIndex, ms, Round(StepIndex / (ms / 1000))]));
  end;
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
      ConnectionManagerAction.Execute;
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

procedure TMainForm.AssignRandomPicture(Male : boolean; InstantBlob : TInstantBlob);
const
{$IFDEF MSWINDOWS}
  ARandomExt : Array[0..2] of string = ('.bmp','.jpg','.emf');
{$ENDIF}
{$IFDEF LINUX}
  ARandomExt : Array[0..3] of string = ('.bmp','.jpg','.emf','.png');
{$ENDIF}
var
  Picture: TPicture;
  PictureName : string;
begin
  PictureName := '0'+IntToStr(Random(5)+1)+ARandomExt[Random(High(ARandomExt)+1)];
  if Male then
    PictureName := 'man'+PictureName
  else
    PictureName := 'woman'+PictureName;
  PictureName := ExtractFilePath(Application.ExeName)+'Pictures'+PathDelim+PictureName;
  if FileExists(PictureName) then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(PictureName);
      InstantBlob.AssignPicture(Picture);
    finally
      Picture.Free;
    end;
  end;
end;

procedure TMainForm.ExportModelItemClick(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    Filter := XMLFilter;
    DefaultExt := XMLExt;
    InitialDir := ExtractFilePath(Application.ExeName);
    if Execute then
      InstantModel.SaveToFile(FileName);
  finally
    Free;
  end;
end;

initialization
{$IFDEF MSWINDOWS}
  InstantRegisterGraphicClass(gffJpeg, TJPEGImage);
{$ENDIF}

end.
