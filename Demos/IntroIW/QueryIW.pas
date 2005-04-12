unit QueryIW;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes,
  IWCompEdit, IWTMSCtrls, IWCompMemo, IWCompListbox, IWCompButton,
  IWCompLabel, IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl,
  IWControl, IWGrids, IWDBGrids, Controls, Forms, IWVCLBaseContainer,
  IWContainer, IWHTMLContainer, IWRegion, DB, InstantPresentation,
  IWDBStdCtrls;

type
  TQueryForm = class(TIWAppForm)
    TestSource: TDataSource;
    TestSelector: TInstantSelector;
    IWRegion1: TIWRegion;
    IWRegion2: TIWRegion;
    IWDBGrid1: TIWDBGrid;
    StaticText1: TIWLabel;
    btnClose: TIWButton;
    btnExecute: TIWButton;
    ExampleLabel: TIWLabel;
    ExampleComboBox: TIWComboBox;
    CommandEdit: TIWMemo;
    IWLabel3: TIWLabel;
    CommandLabel: TIWLabel;
    MaxCountEdit: TTIWAdvEdit;
    IWDBNavigator1: TIWDBNavigator;
    procedure IWAppFormCreate(Sender: TObject);
    procedure TestSelectorAfterScroll(DataSet: TDataSet);
    procedure ExampleComboBoxChange(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  public
    procedure LoadExamples;
  end;

implementation

{$R *.dfm}

const
  Examples: array[0..2, 0..1] of string = (
    ('All contacts',
    'SELECT * FROM ANY TContact'),
    ('All companies',
    'SELECT * FROM TCompany'),
    ('All persons',
    'SELECT * FROM TPerson'){,

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
    'SELECT * FROM TPerson WHERE City = Employer.City')  }
  );


procedure TQueryForm.IWAppFormCreate(Sender: TObject);
begin
  LoadExamples;
end;

procedure TQueryForm.LoadExamples;
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

procedure TQueryForm.TestSelectorAfterScroll(DataSet: TDataSet);
begin
  with DataSet do
    StaticText1.Caption := Format('%d/%d', [RecNo, RecordCount]);
end;

procedure TQueryForm.ExampleComboBoxChange(Sender: TObject);
begin
  with ExampleComboBox do
    if ItemIndex <> -1 then
      CommandEdit.Text := Examples[ItemIndex, 1];
end;

procedure TQueryForm.btnExecuteClick(Sender: TObject);
begin
  with TestSelector do
  begin
    Close;
    TestSelector.MaxCount := StrToInt(Trim(MaxCountEdit.text));
    Command.Text := CommandEdit.Text;
    Open;
  end;

end;

procedure TQueryForm.btnCloseClick(Sender: TObject);
begin
  TestSelector.Close;
  Release;
end;

end.
