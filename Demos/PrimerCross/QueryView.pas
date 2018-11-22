unit QueryView;

interface

{$IFDEF LINUX}
{$I '../../Source/InstantDefines.inc'}
{$ELSE}
{$I '..\..\Source\InstantDefines.inc'}
{$ENDIF}

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, Mask,
  Grids, DBGrids, StdCtrls, ExtCtrls, ActnList, Menus, Buttons,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QMask,
  QGrids, QDBGrids, QStdCtrls, QExtCtrls, QActnList, QMenus, QButtons,
{$ENDIF}
  BasicView, Db, InstantPresentation, ComCtrls;

type
  TQueryViewForm = class(TBasicViewForm)
    Actions: TActionList;
    CommandEdit: TMemo;
    CommandLabel: TLabel;
    CommandPanel: TPanel;
    ExecuteAction: TAction;
    ExecuteButton: TButton;
    ResultGrid: TDBGrid;
    ResultPageControl: TPageControl;
    Splitter: TSplitter;
    TestSelector: TInstantSelector;
    TestSource: TDataSource;
    MaxCountEdit: TMaskEdit;
    NumberLabel: TLabel;
    ResultTabSheet: TTabSheet;
    TranslatedQueryTabSheet: TTabSheet;
    TranslatedQueryMemo: TMemo;
    Label1: TLabel;
    LoadModeComboBox: TComboBox;
    StatsTabSheet: TTabSheet;
    StatsMemo: TMemo;
    FetchAllCheckBox: TCheckBox;
    StatementCacheCheckBox: TCheckBox;
    ExamplePanel: TPanel;
    ExampleLabel: TLabel;
    ExampleComboBox: TComboBox;
    procedure ExecuteActionExecute(Sender: TObject);
    procedure ExampleComboBoxClick(Sender: TObject);
    procedure TestSelectorAfterScroll(DataSet: TDataSet);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure TestSelectorAfterClose(DataSet: TDataSet);
  private
    procedure LogStatement(const AString: string);
    procedure UpdateTabSheets;
  protected
    procedure LoadExamples;
  public
    procedure FormCreate(Sender: TObject); override;
    procedure FormShow(Sender: TObject); override;
    procedure Disconnect; override;
    procedure Connect; override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  InstantPersistence, InstantBrokers, InstantConsts, InstantTypes;
  
const
  Examples: array[0..11, 0..1] of string = (
    ('All contacts',
    'SELECT * FROM ANY TContact'),
    ('All companies',
    'SELECT * FROM TCompany'),
    ('All persons',
    'SELECT * FROM TPerson'),
    ('All employees',
    'SELECT * FROM TPerson WHERE Employer.Name <> ""'),
    ('All employers',
    'SELECT DISTINCT Employer FROM TPerson'),
    ('Employees at customers',
    'SELECT * FROM TPerson WHERE Employer.Category.Name = "Customer" ORDER BY Name'),
    ('My friends',
    'SELECT * FROM TPerson WHERE Category.Name = "Friend"'),
    ('Contacts from Alabama',
    'SELECT * FROM ANY TContact WHERE City = "Alabama" ORDER BY Name'),
    ('Corporations ordered descending by city',
    'SELECT * FROM TCompany WHERE Name LIKE "%Corp%" ORDER BY City DESC'),
    ('Employees from same city as their employer',
    'SELECT * FROM TPerson WHERE City = Employer.City'),
    ('Categories of contacts in cities with names starting by A',
    'SELECT DISTINCT * FROM TCategory WHERE EXISTS(SELECT * FROM ANY TContact WHERE City LIKE ''A%'' USING Category)'),
    ('Companies with at least one employee living in San Diego',
    'SELECT * FROM TCompany WHERE EXISTS(SELECT * FROM TPerson WHERE City = ''San Diego'' USING Employer)')
  );

destructor TQueryViewForm.Destroy;
begin
  InstantLogProc := nil;
  inherited;
end;

procedure TQueryViewForm.Disconnect;
begin
  TestSelector.Close;
  inherited;
end;

procedure TQueryViewForm.ExampleComboBoxClick(Sender: TObject);
begin
  with ExampleComboBox do
    if ItemIndex <> -1 then
      CommandEdit.Text := Examples[ItemIndex, 1];
end;

procedure TQueryViewForm.ExecuteActionExecute(Sender: TObject);
var
  LStartTime: Cardinal;
begin
  LStartTime := GetTickCount;
  try
    TestSelector.RequestedLoadMode := TInstantLoadMode(LoadModeComboBox.ItemIndex);
    {$IFDEF IO_STATEMENT_LOGGING}
    TranslatedQueryMemo.Clear;
    {$ENDIF}
    with TestSelector do
    begin
      if Connector.Broker is TInstantSQLBroker then
      begin
        if StatementCacheCheckBox.Checked then
          TInstantSQLBroker(Connector.Broker).StatementCacheCapacity := -1
        else
          TInstantSQLBroker(Connector.Broker).StatementCacheCapacity := 0;
      end;
      Close;
      TestSelector.MaxCount := StrToInt(Trim(MaxCountEdit.Text));
      Command.Text := CommandEdit.Text;
      DisableControls;
      try
        Open;
        if FetchAllCheckBox.Checked then
          while not Eof do
            Next;
      finally
        EnableControls;
      end;
      ResultPageControl.ActivePage := ResultTabSheet;
    end;
  finally
    StatsMemo.Clear;
    StatsMemo.Lines.Add('Requested load mode: ' + LoadModeComboBox.Items[Ord(TestSelector.RequestedLoadMode)]);
    StatsMemo.Lines.Add('Actual load mode: ' + LoadModeComboBox.Items[Ord(TestSelector.ActualLoadMode)]);
    StatsMemo.Lines.Add(Format('Elapsed time: %ns', [(GetTickCount - LStartTime) / 1000.00]));
  end;
end;

procedure TQueryViewForm.FormCreate(Sender: TObject);
begin
  Caption := 'Query';
  LoadExamples;
  ResultPageControl.ActivePage := ResultTabSheet;
end;

procedure TQueryViewForm.LogStatement(const AString: string);
begin
  //Log only first statement
  if TranslatedQueryMemo.Lines.Count = 0 then
    TranslatedQueryMemo.Lines.Text :=
      Copy(AString, Length(InstantLogStatementBefore) + 1, MaxInt);
end;

procedure TQueryViewForm.LoadExamples;
var
  I: Integer;
begin
  with ExampleComboBox.Items do
  begin
    BeginUpdate;
    try
      for I := Low(Examples) to High(Examples) do
        Add(Examples[I, 0]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TQueryViewForm.TestSelectorAfterScroll(DataSet: TDataSet);
begin
  with DataSet do
    ShowStatus(Format('%d/%d', [RecNo, RecordCount]));
end;

procedure TQueryViewForm.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;
  ExecuteAction.Enabled := IsConnected and (CommandEdit.Text <> '') and Visible;
end;

procedure TQueryViewForm.FormShow(Sender: TObject);
begin
  inherited;
  UpdateTabSheets;
end;

procedure TQueryViewForm.UpdateTabSheets;
begin
{$IFDEF IO_STATEMENT_LOGGING}
  TranslatedQueryTabSheet.TabVisible := Assigned(Connector) and (Connector.Broker is TInstantSQLBroker);
  InstantLogProc := LogStatement;
{$ELSE}
  TranslatedQueryTabSheet.TabVisible := False;
  InstantLogProc := nil;
{$ENDIF}
end;

procedure TQueryViewForm.Connect;
begin
  inherited;
  UpdateTabSheets;
end;

procedure TQueryViewForm.TestSelectorAfterClose(DataSet: TDataSet);
begin
  inherited;
{$IFDEF IO_STATEMENT_LOGGING}
  TranslatedQueryMemo.Clear;
{$ENDIF}
end;

end.
