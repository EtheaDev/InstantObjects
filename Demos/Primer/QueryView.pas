unit QueryView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BasicView, Db, InstantPresentation, Grids, DBGrids, StdCtrls, ExtCtrls,
  ActnList, Menus, Buttons;

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
    procedure FormCreate(Sender: TObject);
    procedure TestSelectorAfterScroll(DataSet: TDataSet);
  protected
    procedure LoadExamples;
    procedure UpdateActions; override;
  public
    procedure Disconnect; override;
  end;

implementation

{$R *.DFM}

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

procedure TQueryViewForm.UpdateActions;
begin
  inherited;
  ExecuteAction.Enabled := IsConnected and (CommandEdit.Text <> '');
end;

end.
