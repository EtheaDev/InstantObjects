program Intro;

uses
  Forms,
  Model in 'Model.pas',
  Main in 'Main.pas' {MainForm},
  ContactEdit in 'ContactEdit.pas' {ContactEditForm},
  PersonEdit in 'PersonEdit.pas' {PersonEditForm},
  CompanyEdit in 'CompanyEdit.pas' {CompanyEditForm};

{$R *.RES}
{$R *.MDR} {Model}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TContactEditForm, ContactEditForm);
  Application.CreateForm(TPersonEditForm, PersonEditForm);
  Application.CreateForm(TCompanyEditForm, CompanyEditForm);
  Application.Run;
end.
