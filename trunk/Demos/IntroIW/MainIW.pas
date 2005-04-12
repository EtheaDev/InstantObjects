unit MainIW;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes, UserSessionUnit,
  Controls, IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWControl,
  IWGrids, IWDBGrids, DB, InstantPresentation, InstantPersistence, Model,
  IWDBStdCtrls, IWCompEdit, IWCompLabel, IWWebGrid, IWDBAdvWebGrid,
  IWCompButton, ContactEditIW, PersonEditIW, CompanyEditIW;

type
  TMainForm = class(TIWAppForm)
    ContactSelector: TInstantSelector;
    ContactsSource: TDataSource;
    IWDBNavigator1: TIWDBNavigator;
    AddPersonButton: TIWButton;
    AddCompanyButton: TIWButton;
    EditContactButton: TIWButton;
    QueryButton: TIWButton;
    EndDemoButtom: TIWButton;
    IWDBGrid1: TIWDBGrid;
    procedure EditContactButtonClick(Sender: TObject);
    procedure AddPersonButtonClick(Sender: TObject);
    procedure AddCompanyButtonClick(Sender: TObject);
    procedure IWAppFormRender(Sender: TObject);
    procedure IWAppFormCreate(Sender: TObject);
    procedure QueryButtonClick(Sender: TObject);
    procedure EndDemoButtomClick(Sender: TObject);
    procedure IWDBGrid1Columns0Click(ASender: TObject;
      const AValue: String);
  public
    bAdd: Boolean;
    Form: TContactEditForm;
    xContact: TContact;
    Person: TPerson;
    Company: TCompany;
   procedure EditContact(Contact: TContact);
  end;

implementation

uses ServerController, QueryIW;

{$R *.dfm}

procedure TMainForm.EditContact(Contact: TContact);
begin
  if Contact is TCompany then
    Form := TCompanyEditForm.Create(WebApplication)
  else if Contact is TPerson then
    Form := TPersonEditForm.Create(WebApplication)
  else
    Form := TContactEditForm.Create(WebApplication);

  xContact := Contact;
  Form.ContactExposer.Subject := Contact;
  UserSession.bOk := False;
  Form.IWDBGrid1.Caption := ContactSelector.FieldByName('Name').AsString +
    '''s Phone Numbers';
  Form.Show;
end;


procedure TMainForm.EditContactButtonClick(Sender: TObject);
begin
  bAdd := False;
  EditContact(ContactSelector.CurrentObject as TContact);
end;

procedure TMainForm.AddPersonButtonClick(Sender: TObject);
begin
  Person := TPerson.Create(ContactSelector.Connector);
  bAdd := True;
  EditContact(Person);
end;

procedure TMainForm.AddCompanyButtonClick(Sender: TObject);
begin
  Company := TCompany.Create(ContactSelector.Connector);
  bAdd := True;
  EditContact(Company);
end;

procedure TMainForm.IWAppFormRender(Sender: TObject);
begin
  if xContact = nil then
    exit;
  if bAdd = True then
  begin
    if xContact is TCompany then
      if UserSession.bOk = True then
        ContactSelector.AddObject(Company)
      else
        Company.Free

    else if xContact is TPerson then
      if UserSession.bOk = True then
        ContactSelector.AddObject(Person)
      else
        Person.Free
  end
  else
    ContactSelector.Refresh;
  xContact := nil;
  bAdd := False;
end;

procedure TMainForm.IWAppFormCreate(Sender: TObject);
begin
  xContact := nil;
  bAdd := False;
  ContactSelector.Open;
end;

procedure TMainForm.QueryButtonClick(Sender: TObject);
begin
  with TQueryForm.Create(WebApplication) do
  begin
    TestSelector.Connector := ContactSelector.Connector;
    Show;
  end;
  xContact := nil;
  bAdd := False;
end;

procedure TMainForm.EndDemoButtomClick(Sender: TObject);
begin
  WebApplication.Terminate('End of IntraWeb with InstantObjects (or if you prefer) InstantObjects with IntraWeb Demo!');
end;

procedure TMainForm.IWDBGrid1Columns0Click(ASender: TObject;
  const AValue: String);
begin
  ContactSelector.Locate('Name', AValue, []);
end;

initialization
  TMainForm.SetAsMainForm;

end.
