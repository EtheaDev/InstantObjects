unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, InstantPresentation, InstantPersistence, Grids, DBGrids,
  ExtCtrls, DBCtrls, Model, DBTables, InstantBDE;

type
  TMainForm = class(TForm)
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    ContactSelector: TInstantSelector;
    ContactsSource: TDataSource;
    AddPersonButton: TButton;
    AddCompanyButton: TButton;
    EditContactButton: TButton;
    InstantBDEConnector1: TInstantBDEConnector;
    Database1: TDatabase;
    ExploreButton: TButton;
    procedure AddPersonButtonClick(Sender: TObject);
    procedure AddCompanyButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
    procedure ExploreButtonClick(Sender: TObject);
  private
    function EditContact(Contact: TContact): Boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses CompanyEdit, ContactEdit, PersonEdit, InstantExplorer;

{$R *.DFM}

{ TMainForm }

function TMainForm.EditContact(Contact: TContact): Boolean;
var
  Form: TContactEditForm;
begin
  if Contact is TCompany then
    Form := TCompanyEditForm.Create(nil)
  else if Contact is TPerson then
    Form := TPersonEditForm.Create(nil)
  else
    Form := TContactEditForm.Create(nil);
  try
    Form.ContactExposer.Subject := Contact;
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

procedure TMainForm.AddPersonButtonClick(Sender: TObject);
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    if EditContact(Person) then
      ContactSelector.AddObject(Person);
  finally
    Person.Free;
  end;
end;

procedure TMainForm.AddCompanyButtonClick(Sender: TObject);
var
  Company: TCompany;
begin
  Company := TCompany.Create;
  try
    if EditContact(Company) then
      ContactSelector.AddObject(Company);
  finally
    Company.Free;
  end;
end;

procedure TMainForm.EditContactButtonClick(Sender: TObject);
begin
  if EditContact(ContactSelector.CurrentObject as TContact) then
    ContactSelector.Refresh;
end;

procedure TMainForm.ExploreButtonClick(Sender: TObject);
begin
  InstantExploreObject(ContactSelector.CurrentObject);
end;

end.
