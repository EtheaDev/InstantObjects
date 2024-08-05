unit guitestrunner;

interface

uses
  Windows, Messages,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  testreport, fpcunit, testregistry,
  ExtCtrls, StdCtrls, Buttons,
  ComCtrls, ImgList, System.ImageList;

type

  TGUITestRunner = class(TForm, ITestListener)
    pnlToolbar: TPanel;
    btnRun: TBitBtn;
    btnClose: TBitBtn;
    pnlCentral: TPanel;
    pnlBottom: TPanel;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    tsTestTree: TTabSheet;
    tsResultsXML: TTabSheet;
    TestTree: TTreeView;
    XMLMemo: TMemo;
    Memo1: TMemo;
    lblSelectedTest: TLabel;
    ImageList1: TImageList;
    pbBar: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure TestTreeChange(Sender: TObject; Node: TTreeNode);
    procedure pbBarPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    failureCounter: Integer;
    errorCounter: Integer;
    testsCounter: Integer;
    barColor: TColor;
    testSuite: TTest;
    procedure BuildTree(rootNode: TTreeNode; aSuite: TTestSuite);
    function FindNode(aTest: TTest): TTreeNode;
    procedure ResetNodeColors;
    procedure PaintNodeError(aNode: TTreeNode);
    procedure PaintNodeFailure(aNode: TTreeNode);
    procedure PaintNodeSuccess(aNode: TTreeNode);
    procedure PaintRunnableSubnodes(aNode: TTreeNode);
    procedure MemoLog(LogEntry: string);
  public
    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
  end;

var
  TestRunner: TGUITestRunner;

implementation

{$R *.dfm}

function GetSubNode(ANode: TTreeNode; Index: integer): TTreeNode;
begin
  Result := aNode.Item[Index];
end;

{ TFGuiTestRunner }

procedure TGUITestRunner.AddError(ATest: TTest; AError: TTestFailure);
var
  ErrorNode, node: TTreeNode;
begin
  ErrorNode := FindNode(ATest);
  if Assigned(ErrorNode) then
  begin
    ErrorNode.DeleteChildren;
    node := TestTree.Items.AddChild(ErrorNode, 'Exception message: ' + AError.ExceptionMessage);
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    node := TestTree.Items.AddChild(ErrorNode, 'Exception class: ' + AError.ExceptionClassName);
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    {node := TestTree.Items.AddChild(ErrorNode, 'Unit name: ' + AError.SourceUnitName);
    node.ImageIndex := 11;
    node.SelectedIndex := 11;
    node := TestTree.Items.AddChild(ErrorNode, 'Method name: ' + AError.MethodName);
    node.ImageIndex := 11;
    node.SelectedIndex := 11;
    node := TestTree.Items.AddChild(ErrorNode, 'Line number: ' + IntToStr(AError.LineNumber));
    node.ImageIndex := 11;
    node.SelectedIndex := 11;}
    PaintNodeError(ErrorNode);
  end;
  Inc(errorCounter);
  barColor := clRed;
  ErrorNode.Expanded := True;
end;

procedure TGUITestRunner.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  FailureNode, node: TTreeNode;
begin
  FailureNode := FindNode(ATest);
  if Assigned(FailureNode) then
  begin
    FailureNode.DeleteChildren;
    node := TestTree.Items.AddChild(FailureNode, 'Message: ' + AFailure.ExceptionMessage);
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    node := TestTree.Items.AddChild(FailureNode, 'Exception: ' + AFailure.ExceptionClassName);
    node.ImageIndex := 4;
    node.SelectedIndex := 4;
    PaintNodeFailure(FailureNode);
  end;
  Inc(failureCounter);
  if errorCounter = 0 then
    barColor := clFuchsia;
  FailureNode.Expanded := True;
end;

procedure TGUITestRunner.BuildTree(rootNode: TTreeNode; aSuite: TTestSuite);
var
  node: TTreeNode;
  i: integer;
begin
  for i := 0 to ASuite.Tests.Count - 1 do
  begin
    node := TestTree.Items.AddChildObject(rootNode, ASuite.Test[i].TestName, ASuite.Test[i]);
    if ASuite.Test[i] is TTestSuite then
      BuildTree(Node, ASuite.Test[i] as TTestSuite);
    node.ImageIndex := 1;
    node.SelectedIndex := 1;
  end;
end;

procedure TGUITestRunner.EndTest(ATest: TTest);
begin
  Inc(testsCounter);
  pbBar.Refresh;
end;

function TGUITestRunner.FindNode(aTest: TTest): TTreeNode;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to TestTree.Items.Count -1 do
    if (TTest(TestTree.Items[i].data) = aTest) then
    begin
      Result :=  TestTree.Items[i];
      Exit;
    end;
end;

procedure TGUITestRunner.ResetNodeColors;
var
  i: integer;
begin
  for i := 0 to TestTree.Items.Count - 1 do
  begin
    TestTree.Items[i].ImageIndex := 1;
    TestTree.Items[i].SelectedIndex := 1;
  end;
end;

procedure TGUITestRunner.StartTest(ATest: TTest);
var
  Node: TTreeNode;
begin
  Node := FindNode(ATest);
  if Assigned(Node) then
    PaintNodeSuccess(Node);
end;

procedure TGUITestRunner.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  barColor := clGray;
  TestTree.Items.Clear;
  BuildTree(TestTree.Items.AddObject(nil, 'All Tests', GetTestRegistry), GetTestRegistry);
  TestTree.Items[0].Expand(True);
  TestTree.Items[0].Selected := True;
  Memo1.Text := '';
end;

procedure TGUITestRunner.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TGUITestRunner.btnRunClick(Sender: TObject);
var
  testResult: TTestResult;
  FStopCrono: TDateTime;
  FStartCrono: TDateTime;
begin
  barcolor := clGreen;
  ResetNodeColors;
  if (TestTree.Selected <> nil) and (TestTree.Selected.Data <> nil) then
  begin
    testSuite := TTest(TestTree.Selected.Data);
    PaintNodeSuccess(TestTree.Selected);
    PaintRunnableSubnodes(TestTree.Selected);
  end
  else
    begin
      testSuite := GetTestRegistry;
      TestTree.Selected := TestTree.Items[0];
      ResetNodeColors;
      PaintRunnableSubnodes(TestTree.Selected);
    end;
  failureCounter := 0;
  errorCounter := 0;
  testsCounter := 0;
  testResult := TTestResult.Create;
  try
    testResult.AddListener(self);

    FStartCrono:=Now;
    testSuite.Run(testResult);
    FStopCrono:=Now;
    MemoLog('Run ' + IntToStr(testResult.RunTests) + ' tests. Time elapsed: ' + FormatDateTime('hh:nn:ss.zzz', FStopCrono - FStartCrono));

    XMLMemo.lines.text := '<TestResults>' + system.sLineBreak +
      TestResultAsXML(testResult) + system.sLineBreak + '</TestResults>';
  finally
    testResult.Free;
  end;
  pbBar.Invalidate;
end;

procedure TGUITestRunner.TestTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if (Sender as TTreeView).Selected <> nil then
  begin
    MemoLog((Sender as TTreeview).Selected.Text);
    lblSelectedTest.Caption := 'Selected: ' + (Sender as TTreeview).Selected.Text;
  end;
end;

procedure TGUITestRunner.pbBarPaint(Sender: TObject);
var
  msg: string;
  alltest: integer;
begin
  with (Sender as TPaintBox) do
  begin
  Canvas.Lock;

    Canvas.Brush.Color := clSilver;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Font.Color := clWhite;

    if Assigned(TestSuite) then
    begin
      alltest := TestSuite.CountTestCases;
      if FailureCounter + ErrorCounter = 0 then
        barColor := clGreen;
      Canvas.Brush.Color := barColor;
      if TestsCounter <> 0 then
      begin
        Canvas.Rectangle(0, 0, round(TestsCounter / alltest * Width), Height);
        Canvas.Font.Color := clWhite;
        msg := 'Runs: ' + IntToStr(TestsCounter);
        if ErrorCounter <> 0 then
          msg := msg + '    Number of test errors: ' + IntToStr(ErrorCounter);
        if (FailureCounter <> 0) then
          msg := msg + '     Number of test failures: ' + IntToStr(FailureCounter);
        Canvas.Textout(10, 10,  msg)
      end;
    end;

  Canvas.UnLock;
  end;
end;

procedure TGUITestRunner.FormShow(Sender: TObject);
begin
  if (ParamStr(1) = '--now') or (ParamStr(1) = '-n') then
    BtnRunClick(Self);
end;

procedure TGUITestRunner.PaintNodeError(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    aNode.ImageIndex := 2;
    aNode.SelectedIndex := 2;
    aNode := aNode.Parent;
    if Assigned(aNode) and (aNode.ImageIndex in [0, 3, 1 ]) then
      PaintNodeError(aNode);
  end;
end;

procedure TGUITestRunner.PaintNodeFailure(aNode: TTreeNode);
begin
  while Assigned(aNode) do
  begin
    if aNode.ImageIndex in [0, 1] then
    begin
      aNode.ImageIndex := 3;
      aNode.SelectedIndex := 3;
    end;
    aNode := aNode.Parent;
    if Assigned(aNode) and (aNode.ImageIndex in [0, 1]) then
      PaintNodeFailure(aNode);
  end;
end;

procedure TGUITestRunner.PaintNodeSuccess(aNode: TTreeNode);
begin
  if Assigned(aNode) then
  begin
    aNode.ImageIndex := 0;
    aNode.SelectedIndex := 0;
  end;
end;

procedure TGUITestRunner.PaintRunnableSubnodes(aNode: TTreeNode);
var
  i: integer;
begin
  if Assigned(aNode) then
  begin
    PaintNodeSuccess(aNode);
    for i := 0 to aNode.Count - 1 do
      if GetSubNode(aNode, i).Count > 0 then
        PaintRunnableSubnodes(GetSubNode(aNode, i));
  end;
end;

procedure TGUITestRunner.MemoLog(LogEntry: string);
begin
  Memo1.Lines.Add(TimeToStr(Now) + ' - ' + LogEntry);
end;

end.
