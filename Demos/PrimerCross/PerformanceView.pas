unit PerformanceView;

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
  Windows, Messages, Graphics, Controls, Forms, Dialogs, ToolWin, Menus,
  ExtCtrls, StdCtrls, Contnrs, ComCtrls, TeEngine,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QMenus,
  QExtCtrls, QStdCtrls, QComCtrls, QTypes,
{$ENDIF}
  Series, TeeProcs, Chart,
  InstantPersistence, InstantPresentation, InstantClasses, Stopwatch, DB, BasicView,
  Mask;

type
  TMeasureType = (mtStore, mtRetrieve, mtQuery, mtDispose);

  TTestResult = class(TInstantCollectionItem)
  private
    FIsChecked: Boolean;
    FValues: array[TMeasureType] of Double;
    function GetValues(Index: TMeasureType): Double;
    procedure SetValues(Index: TMeasureType; Value: Double);
  protected
    procedure ReadObject(Reader: TInstantReader); override;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    property Values[Index: TMeasureType]: Double read GetValues write SetValues;
  published
    property IsChecked: Boolean read FIsChecked write FIsChecked;
  end;

  TTestResults = class(TInstantCollection)
  private
    function GetItems(Index: Integer): TTestResult;
  public
    constructor Create;
    function Add: TTestResult;
    function AddResult(AResult: TTestResult): Integer;
    property Items[Index: Integer]: TTestResult read GetItems; default;
  end;

  TShowStatusEvent = procedure(Sender: TObject; const Text: string) of object;

  TTest = class(TObject)
  private
{$IFDEF MSWINDOWS}
    FMeasureStart: Cardinal;
{$ENDIF}
{$IFDEF LINUX}
    FMeasureStart: TDateTime;
{$ENDIF}
    FStopwatch: TStopwatch;
    FTestResult: TTestResult;
    FOnShowStatus: TShowStatusEvent;
    function GetStopwatch: TStopwatch;
    function GetTestResult: TTestResult;
    procedure StopwatchStep(Sender: TObject);
    procedure StopwatchStop(Sender: TObject);
    procedure ShowStatus(const Text: string);
  protected
    procedure BeginMeasure;
    procedure EndMeasure(MeasureType: TMeasureType; Count: Integer);
    procedure Run(Retrieve, Query, Dispose : boolean); virtual; abstract;
    property Stopwatch: TStopwatch read GetStopwatch;
    property TestResult: TTestResult read GetTestResult;
  public
    destructor Destroy; override;
    procedure Execute(Retrieve, Query, Dispose : boolean);
    function ExtractResult: TTestResult;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

  TMethod = procedure of object;

  TPersistenceTest = class(TTest)
  private
    FCount: Integer;
    FObjectList: TStringList;
    function GetObjectList: TStringList;
    procedure TestStore;
    procedure TestRetrieve;
    procedure TestDispose;
    procedure TestQuery;
    procedure Operation(Method: TMethod);
  protected
    procedure Run(Retrieve, Query, Dispose : boolean); override;
    property ObjectList: TStringList read GetObjectList;
  public
    destructor Destroy; override;
    property Count: Integer read FCount write FCount;
  end;

  TPerformanceViewForm = class(TBasicViewForm)
    ChartPanel: TPanel;
    ClientPanel: TPanel;
    InfoBevel: TBevel;
    InfoPanel: TPanel;
    ResultListPanel: TPanel;
    ResultPanel: TPanel;
    RunButton: TButton;
    TestResultChart: TChart;
    TestResultDisposeSeries: TBarSeries;
    TestResultListView: TListView;
    TestResultRetrieveSeries: TBarSeries;
    TestResultStoreSeries: TBarSeries;
    TitleLabel: TLabel;
    ConnectionLabel: TLabel;
    InfoMemo: TMemo;
    TestResultMenu: TPopupMenu;
    TestResultDeleteItem: TMenuItem;
    IconImage: TImage;
    TestResultRenameItem: TMenuItem;
    TransactionsCheckBox: TCheckBox;
    NumberLabel: TLabel;
    ObjectsEdit: TMaskEdit;
    TestRetrieveCheckBox: TCheckBox;
    TestDisposeCheckBox: TCheckBox;
    PoolOptionsRadioGroup: TRadioGroup;
    TestQueryCheckBox: TCheckBox;
    Series1: TBarSeries;
    procedure RunButtonClick(Sender: TObject);
    procedure TestResultListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure TestShowStatus(Sender: TObject; const Text: string);
    procedure TestResultDeleteItemClick(Sender: TObject);
    procedure TestResultMenuPopup(Sender: TObject);
    procedure TestResultRenameItemClick(Sender: TObject);
{$IFDEF MSWINDOWS}
    procedure TestResultListViewEditedVCL(Sender: TObject; Item: TListItem; var S: String);
{$ENDIF}
{$IFDEF LINUX}
    procedure TestResultListViewEditedCLX(Sender: TObject; Item: TListItem; var S: WideString);
{$ENDIF}
    procedure TransactionsCheckBoxClick(Sender: TObject);
    procedure PoolOptionsRadioGroupClick(Sender: TObject);
    procedure TestDisposeCheckBoxClick(Sender: TObject);
    procedure TestRetrieveCheckBoxClick(Sender: TObject);
  private
    FTestResults: TTestResults;
    function GetTestResults: TTestResults;
    function GetTestResultsFileName: string;
    procedure SetTitleLabel(TitleLabel : TLabel);
{$IFDEF LINUX}
    procedure EditItemCaption(Item : TListItem);
{$ENDIF}
  protected
    procedure LoadTestResults;
    procedure ShowTestResults;
    procedure SaveTestResults;
    procedure UpdateChart;
    property TestResults: TTestResults read GetTestResults;
    property TestResultsFileName: string read GetTestResultsFileName;
  public
    procedure FormCreate(Sender: TObject); override;
    procedure FormHide(Sender: TObject); override;
    procedure FormShow(Sender: TObject); override;
    destructor Destroy; override;
    procedure UpdateControls; override;
  end;

implementation

{$R *.dfm}

uses
  Model, DemoData, IniFiles, Utility;

{ TTestResult }

function TTestResult.GetValues(Index: TMeasureType): Double;
begin
  Result := FValues[Index];
end;

procedure TTestResult.ReadObject(Reader: TInstantReader);
var
  Index: TMeasureType;
begin
  inherited;
  for Index := Low(Index) to High(Index) do
    Values[Index] := Reader.ReadFloat;
end;

procedure TTestResult.SetValues(Index: TMeasureType; Value: Double);
begin
  FValues[Index] := Value;
end;

procedure TTestResult.WriteObject(Writer: TInstantWriter);
var
  Index: TMeasureType;
begin
  inherited;
  for Index := Low(Index) to High(Index) do
    Writer.WriteFloat(Values[Index]);
end;

{ TTestResults }

function TTestResults.Add: TTestResult;
begin
  Result := inherited Add as TTestResult;
end;

function TTestResults.AddResult(AResult: TTestResult): Integer;
begin
  Result := IndexOf(AResult.Name);
  AResult.Collection := Self;
  if Result = -1 then
    Result := Count
  else begin
    Delete(Result);
    AResult.Index := Result;
  end;
end;

constructor TTestResults.Create;
begin
  inherited Create(TTestResult);
end;

function TTestResults.GetItems(Index: Integer): TTestResult;
begin
  Result := inherited Items[Index] as TTestResult;
end;

{ TTest }

procedure TTest.BeginMeasure;
begin
  FMeasureStart := Stopwatch.ElapsedTime;
end;

destructor TTest.Destroy;
begin
  inherited;
  FStopwatch.Free;
  FTestResult.Free;
end;

procedure TTest.EndMeasure(MeasureType: TMeasureType; Count: Integer);
var
{$IFDEF MSWINDOWS}
  ElapsedTime: Cardinal;
{$ENDIF}
{$IFDEF LINUX}
  ElapsedTime: TDateTime;
{$ENDIF}
begin
  ElapsedTime := Stopwatch.ElapsedTime - FMeasureStart;
{$IFDEF MSWINDOWS}
  TestResult.Values[MeasureType] := Count / (ElapsedTime / 1000);
{$ENDIF}
{$IFDEF LINUX}
  TestResult.Values[MeasureType] := Count / ElapsedTime / (3600 * 24);
{$ENDIF}
end;

procedure TTest.Execute(Retrieve, Query, Dispose : boolean);
begin
  Run(Retrieve,Query,Dispose);
end;

function TTest.ExtractResult: TTestResult;
begin
  Result := FTestResult;
  FTestResult := nil;
end;

function TTest.GetStopwatch: TStopwatch;
begin
  if not Assigned(FStopwatch) then
  begin
    FStopwatch := TStopwatch.Create;
    FStopwatch.OnStep := StopwatchStep;
    FStopwatch.OnStop := StopwatchStop;
  end;
  Result := FStopwatch;
end;

function TTest.GetTestResult: TTestResult;
begin
  if not Assigned(FTestResult) then
    FTestResult := TTestResult.Create(nil);
  Result := FTestResult;
end;

procedure TTest.ShowStatus(const Text: string);
begin
  if Assigned(FOnShowStatus) then
    FOnShowStatus(Self, Text);
end;

procedure TTest.StopwatchStep(Sender: TObject);
begin
  ShowStatus(IntToStr(Stopwatch.StepPercent) + '%');
end;

procedure TTest.StopwatchStop(Sender: TObject);
begin
  ShowStatus('');
end;

{ TPersistenceTest }

destructor TPersistenceTest.Destroy;
begin
  inherited;
  FObjectList.Free;
end;

function TPersistenceTest.GetObjectList: TStringList;
begin
  if not Assigned(FObjectList) then
    FObjectList := TStringList.Create;
  Result := FObjectList;
end;

procedure TPersistenceTest.Operation(Method: TMethod);
begin
  InstantDefaultConnector.StartTransaction;
  try
    if Assigned(Method) then
      Method;
    InstantDefaultConnector.CommitTransaction;
  except
    InstantDefaultConnector.RollbackTransaction;
    raise;
  end;
end;

procedure TPersistenceTest.Run(Retrieve, Query, Dispose : boolean);
begin
  Stopwatch.Start((1+Ord(Retrieve)+Ord(Query)+Ord(Dispose)) * Count);
  try
    //Test always store operations (necessary to test retrieve and dispose)
    Operation(TestStore);
    if Retrieve then
      Operation(TestRetrieve);
    if Query then
      Operation(TestQuery);
    if Dispose then
      Operation(TestDispose);
  finally
    Stopwatch.Stop;
  end;
end;

procedure TPersistenceTest.TestDispose;
var
  I: Integer;
  AObject: TInstantObject;
begin
  BeginMeasure;
  try
    for I := 0 to Pred(ObjectList.Count) do
    begin
      AObject := ObjectList.Objects[I] as TInstantObject;
      if Assigned(AObject) then
      begin
        AObject.Dispose;
        AObject.Free;
        ObjectList.Objects[I] := nil;
      end;
      Stopwatch.Step;
    end;
  finally
    EndMeasure(mtDispose, ObjectList.Count);
  end;
end;

procedure TPersistenceTest.TestRetrieve;
var
  I: Integer;
begin
  BeginMeasure;
  try
    for I := 0 to Pred(ObjectList.Count) do
    begin
      ObjectList.Objects[I] := TCompany.Retrieve(ObjectList[I]);
      Stopwatch.Step;
    end;
  finally
    EndMeasure(mtRetrieve, ObjectList.Count);
  end;
end;

procedure TPersistenceTest.TestQuery;
var
  InstantQuery : TInstantQuery;
  i : integer;
begin
  InstantQuery := InstantDefaultConnector.CreateQuery;
  try
    InstantQuery.Command := 'SELECT * FROM ANY TCompany WHERE Id = :Id';
    InstantQuery.FetchParams(InstantQuery.Command, InstantQuery.Params);
    BeginMeasure;
    try
      for I := 0 to Pred(ObjectList.Count) do
      begin
        InstantQuery.Params.ParamByName('Id').AsString := ObjectList[I];
        Stopwatch.Step;
        InstantQuery.Open;
        InstantQuery.Close;
      end;
    finally
      EndMeasure(mtQuery, ObjectList.Count);
    end;
  finally
    InstantQuery.Free;
  end;
end;

procedure TPersistenceTest.TestStore;
var
  I: Integer;
  AObject: TInstantObject;
begin
  BeginMeasure;
  try
    ObjectList.Clear;
    for I := 0 to Pred(Count) do
    begin
      AObject := CreateRandomCompany;
      try
        AObject.Store;
        ObjectList.Add(AObject.Id);
      finally
        AObject.Free;
      end;
      Stopwatch.Step;
    end;
  finally
    EndMeasure(mtStore, ObjectList.Count);
  end;
end;

{ TPerformanceViewForm }

destructor TPerformanceViewForm.Destroy;
begin
  inherited;
  FTestResults.Free;
end;

procedure TPerformanceViewForm.FormCreate(Sender: TObject);
begin
  Caption := 'Performance';
  InfoMemo.BorderStyle := bsNone;
  SetTitleLabel(TitleLabel);
  SetTitleLabel(ConnectionLabel);

{$IFDEF MSWINDOWS}
  TestResultListView.OnEdited := TestResultListViewEditedVCL;
  TestResultListView.SortType := stText;
{$ENDIF}
{$IFDEF LINUX}
  TestResultListView.OnEdited := TestResultListViewEditedCLX;
  TestResultListView.Columns[0].Width := 180;
{$ENDIF}
end;

procedure TPerformanceViewForm.FormHide(Sender: TObject);
begin
  inherited;
  SaveTestResults;
end;

function TPerformanceViewForm.GetTestResults: TTestResults;
begin
  if not Assigned(FTestResults) then
    FTestResults := TTestResults.Create;
  Result := FTestResults;
end;

function TPerformanceViewForm.GetTestResultsFileName: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.tst');
end;

procedure TPerformanceViewForm.LoadTestResults;
var
  Stream: TStream;
begin
  if not FileExists(TestResultsFileName) then
    Exit;
  Stream := TFileStream.Create(TestResultsFileName, fmOpenRead);
  try
    InstantReadObjectFromStream(Stream, TestResults);
  finally
    Stream.Free;
  end;
end;

procedure TPerformanceViewForm.RunButtonClick(Sender: TObject);
var
  AResult: TTestResult;
begin
  if not Confirm(Format('Run performance test on "%s"?', [ConnectionName])) then
    Exit;
  with TPersistenceTest.Create do
  try
    OnShowStatus := TestShowStatus;
    Count := StrToInt(Trim(ObjectsEdit.text));
    Execute(TestRetrieveCheckBox.Checked, TestQueryCheckBox.Checked, TestDisposeCheckBox.Checked);
    AResult := ExtractResult;
    AResult.Name := ConnectionName;
    AResult.IsChecked := True;
    TestResults.AddResult(AResult);
    ShowTestResults;
  finally
    Free;
  end;
end;

procedure TPerformanceViewForm.SaveTestResults;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(TestResultsFileName, fmCreate);
  try
    InstantWriteObjectToStream(Stream, TestResults);
  finally
    Stream.Free;
  end;
end;

procedure TPerformanceViewForm.ShowTestResults;
var
  I: Integer;
  TestResult: TTestResult;
begin
  with TestResultListView.Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to Pred(TestResults.Count) do
      begin
        TestResult := TestResults[I];
        with Add do
        begin
          Caption := TestResult.Name;
          Data := TestResult;
          Checked := TestResult.IsChecked;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
  UpdateChart;
end;

procedure TPerformanceViewForm.TestResultDeleteItemClick(Sender: TObject);
var
  TestResult: TTestResult;
begin
  with TestResultListView do
    if Assigned(Selected) then
    begin
      TestResult := Selected.Data;
      if Confirm(Format('Delete test results for "%s"?',
        [TestResult.Name])) then
      begin
        Selected.Delete;
        TestResults.Remove(TestResult);
        ShowTestResults;
      end;
    end;
end;

procedure TPerformanceViewForm.TestResultListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Assigned(Item) and Assigned(Item.Data) then
    with Item do
      TTestResult(Data).IsChecked := Checked;
  UpdateChart;
end;

{$IFDEF MSWINDOWS}
procedure TPerformanceViewForm.TestResultListViewEditedVCL(Sender: TObject;
  Item: TListItem; var S: String);
begin
  TTestResult(Item.Data).Name := S;
  UpdateChart;
end;
{$ENDIF}
{$IFDEF LINUX}
procedure TPerformanceViewForm.TestResultListViewEditedCLX(Sender: TObject;
  Item: TListItem; var S: WideString);
begin
  TTestResult(Item.Data).Name := S;
  UpdateChart;
end;

procedure TPerformanceViewForm.EditItemCaption(Item : TListItem);
begin
  Item.Caption := InputBox('Connection Name','Name:',Item.Caption);
end;
{$ENDIF}

procedure TPerformanceViewForm.TestResultMenuPopup(Sender: TObject);
begin
  with TestResultListView do
  begin
    TestResultDeleteItem.Enabled := Assigned(Selected);
    TestResultRenameItem.Enabled := Assigned(Selected);
  end;
end;

procedure TPerformanceViewForm.TestResultRenameItemClick(Sender: TObject);
begin
  with TestResultListView do
{$IFDEF MSWINDOWS}
    if Assigned(Selected) then
      Selected.EditCaption;
{$ENDIF}
{$IFDEF LINUX}
    EditItemCaption(Selected);
{$ENDIF}
end;

procedure TPerformanceViewForm.TestShowStatus(Sender: TObject;
  const Text: string);
begin
  ShowStatus(Text);
end;

procedure TPerformanceViewForm.UpdateChart;
var
  I: Integer;
  MT: TMeasureType;
  TestResult: TTestResult;
begin
  with TestResultChart do
    for I := 0 to Pred(SeriesCount) do
      SeriesList[I].Clear;
  with TestResultListView do
    for I := 0 to Pred(Items.Count) do
      if Items[I].Checked then
      begin
        TestResult := Items[I].Data;
        if Assigned(TestResult) then
          with TestResultChart do
            for MT := Low(MT) to High(MT) do
              SeriesList[Ord(MT)].Add(TestResult.Values[MT], TestResult.Name);
      end;
end;

procedure TPerformanceViewForm.UpdateControls;
begin
  RunButton.Enabled := IsConnected;
  TransactionsCheckBox.Enabled := IsConnected;
  TransactionsCheckBox.Checked := IsConnected and Connector.UseTransactions;
  ObjectsEdit.Enabled := IsConnected;
  TestRetrieveCheckBox.Enabled := IsConnected;
  TestDisposeCheckBox.Enabled := IsConnected;
  if Assigned(Connector) and (Connector.Broker is TInstantSQLBroker) then
  begin
    PoolOptionsRadioGroup.Visible := True;
    PoolOptionsRadioGroup.Enabled := IsConnected;
    if TInstantSQLBroker(Connector.Broker).UsePreparedQuery then
      PoolOptionsRadioGroup.ItemIndex := 1
    else if TInstantSQLBroker(Connector.Broker).StatementCacheCapacity <> 0 then
      PoolOptionsRadioGroup.ItemIndex := 2
    else
      PoolOptionsRadioGroup.ItemIndex := 0;
  end
  else
    PoolOptionsRadioGroup.Visible := False;

  ConnectionLabel.Caption := 'Connection: ' + ConnectionName;
  UpdateChart;
end;

procedure TPerformanceViewForm.TransactionsCheckBoxClick(Sender: TObject);
begin
  if Assigned(Connector) then
    Connector.UseTransactions := TransactionsCheckBox.Checked;
end;

procedure TPerformanceViewForm.PoolOptionsRadioGroupClick(Sender: TObject);
begin
  inherited;
  if Connector.Broker is TInstantSQLBroker then
  case PoolOptionsRadioGroup.ItemIndex of
    0 :
    begin
      TInstantSQLBroker(Connector.Broker).UsePreparedQuery := False;
      TInstantSQLBroker(Connector.Broker).StatementCacheCapacity := 0;
    end;
    1 :
    begin
      TInstantSQLBroker(Connector.Broker).UsePreparedQuery := True;
      TInstantSQLBroker(Connector.Broker).StatementCacheCapacity := 0;
    end;
    2 :
    begin
      TInstantSQLBroker(Connector.Broker).UsePreparedQuery := False;
      TInstantSQLBroker(Connector.Broker).StatementCacheCapacity := -1;
    end;
  end;  
end;

procedure TPerformanceViewForm.SetTitleLabel(TitleLabel: TLabel);
begin
{$IFDEF MSWINDOWS}
  TitleLabel.Font.Size := 10;
{$ENDIF}
{$IFDEF LINUX}
  TitleLabel.Font.Size := 12;
{$ENDIF}
  TitleLabel.Font.Style := [fsBold];
end;

procedure TPerformanceViewForm.TestDisposeCheckBoxClick(Sender: TObject);
begin
  inherited;
  if TestDisposeCheckBox.Checked then
    TestRetrieveCheckBox.Checked := True;
end;

procedure TPerformanceViewForm.TestRetrieveCheckBoxClick(Sender: TObject);
begin
  inherited;
  if not TestRetrieveCheckBox.Checked then
    TestDisposeCheckBox.Checked := False;
end;

procedure TPerformanceViewForm.FormShow(Sender: TObject);
begin
  inherited;
  Try
    LoadTestResults;
    ShowTestResults;
  Except
    On E : EReadError do
    begin
      ShowMessageFmt('Problems "%s" loading test results File %s: it can be in an older format!',
        [E.Message,GetTestResultsFileName]);
      ShowTestResults;
    end
    else
      raise;
  End;
end;

initialization
  RegisterClasses([TTestResults, TTestResult]);

end.
