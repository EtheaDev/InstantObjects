unit QueryView;

interface
{$IFDEF VER130}{$DEFINE MSWINDOWS}{$ENDIF}
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, StdCtrls, ExtCtrls, ActnList, Menus, Buttons,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs,
  QGrids, QDBGrids, QStdCtrls, QExtCtrls, QActnList, QMenus, QButtons,
{$ENDIF}
  BasicView, Db, InstantPresentation;

type
  TQueryViewForm = class(TBasicViewForm)
    Actions: TActionList;
    CommandEdit: TMemo;
    CommandLabel: TLabel;
    CommandPanel: TPanel;
    ExampleComboBox: TComboBox;
    ExampleLabel: TLabel;
    ExecuteAction: TAction;
    ExecuteButton: TButton;
    ResultGrid: TDBGrid;
    ResultPanel: TPanel;
    Splitter: TSplitter;
    TestSelector: TInstantSelector;
    TestSource: TDataSource;
    procedure ExecuteActionExecute(Sender: TObject);
    procedure ExampleComboBoxClick(Sender: TObject);
    procedure TestSelectorAfterScroll(DataSet: TDataSet);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
  protected
    procedure LoadExamples;
  public
    procedure FormCreate(Sender: TObject); override;
    procedure Disconnect; override;
  end;

implementation

{$R *.dfm}

const
  Examples: array[0..9, 0..1] of string = (
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
    'SELECT * FROM TPerson WHERE City = Employer.City')
  );

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
begin
  with TestSelector do
  begin
    Close;
    Command.Text := CommandEdit.Text;
    Open;
  end;
end;

procedure TQueryViewForm.FormCreate(Sender: TObject);
begin
  Caption := 'Query';
  LoadExamples;
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

end.
