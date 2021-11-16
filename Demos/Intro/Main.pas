unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, InstantPresentation, InstantPersistence, Grids, DBGrids,
  ExtCtrls, DBCtrls, Model, InstantBrokers, InstantXML;

type
  TMainForm = class(TForm)
    DBNavigator1: TDBNavigator;
    ContactGrid: TDBGrid;
    ContactSelector: TInstantSelector;
    ContactsSource: TDataSource;
    AddPersonButton: TButton;
    AddCompanyButton: TButton;
    EditContactButton: TButton;
    ExploreButton: TButton;
    InstantXMLConnector: TInstantXMLConnector;
    XMLFileAccessor: TXMLFilesAccessor;
    GenerateDataButton: TButton;
    procedure AddPersonButtonClick(Sender: TObject);
    procedure AddCompanyButtonClick(Sender: TObject);
    procedure EditContactButtonClick(Sender: TObject);
    procedure ExploreButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ContactGridDblClick(Sender: TObject);
    procedure GenerateDataButtonClick(Sender: TObject);
  private
    function EditContact(Contact: TContact): Boolean;
    procedure CreateRandomContacts(Count: Integer);
    procedure EditCurrentContact;
    procedure GenerateRandomData(const ANumber: Integer);
  public
  end;

var
  MainForm: TMainForm;

implementation

uses CompanyEdit, ContactEdit, PersonEdit, InstantExplorer,
  RandomData, Contnrs, DemoData;

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
  EditCurrentContact;
end;

procedure TMainForm.EditCurrentContact;
begin
  if EditContact(ContactSelector.CurrentObject as TContact) then
    ContactSelector.Refresh;
end;

procedure TMainForm.ExploreButtonClick(Sender: TObject);
begin
  InstantExploreObject(ContactSelector.CurrentObject);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  XMLFileAccessor.RootFolder := ExtractFilePath(Application.ExeName)+PathDelim+'Database';
  InstantXMLConnector.Connect;
  ContactSelector.Open;
  if ContactSelector.Eof then
  begin
    //First Time: create of Random Data
    GenerateRandomData(30);
  end;
end;

procedure TMainForm.GenerateDataButtonClick(Sender: TObject);
var
  LValue: string;
begin
  LValue := '10';
  if InputQuery('Generate Random Data', 'How many contacts do you want to generate?', LValue) then
    GenerateRandomData(StrToInt(LValue));
end;

procedure TMainForm.GenerateRandomData(const ANumber: Integer);
begin
  ContactSelector.Close;
  try
    CreateRandomContacts(ANumber);
  finally
    ContactSelector.Open;
  end;
end;

procedure TMainForm.ContactGridDblClick(Sender: TObject);
begin
  EditCurrentContact;
end;

procedure TMainForm.CreateRandomContacts(Count: Integer);
var
  I: Integer;
  LCompany: TCompany;
  LPerson: TPerson;
begin
  GetAsyncKeyState(VK_ESCAPE);
  Randomize;
  InstantDefaultConnector.StartTransaction;
  try
    for I := 0 to Pred(Count) do
    begin
      //Create a new person
      LPerson := nil;
      LCompany := nil;
      try
        LPerson := CreateRandomPerson;
        LPerson.Store;
        //Create a new company
        LCompany := CreateRandomCompany(LPerson);
        LCompany.Store;
      finally
        LPerson.Free;
        LCompany.Free;
      end;
    end;
    InstantDefaultConnector.CommitTransaction;
  except
    InstantDefaultConnector.RollbackTransaction;
    raise;
  end;
end;

end.
