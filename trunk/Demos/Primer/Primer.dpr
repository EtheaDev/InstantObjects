program Primer;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Model in 'Model.pas',
  ContactView in 'ContactView.pas' {ContactViewForm},
  BasicView in 'BasicView.pas' {BasicViewForm},
  BasicEdit in 'BasicEdit.pas' {BasicEditForm},
  ContactEdit in 'ContactEdit.pas' {ContactEditForm},
  PersonEdit in 'PersonEdit.pas' {PersonEditForm},
  CompanyEdit in 'CompanyEdit.pas' {CompanyEditForm},
  DemoData in 'DemoData.pas',
  ContactFilterEdit in 'ContactFilterEdit.pas' {ContactFilterEditForm},
  MainData in 'MainData.pas' {MainDataModule: TDataModule},
  ContactBrowse in 'ContactBrowse.pas' {ContactBrowseForm},
  CompanyBrowse in 'CompanyBrowse.pas' {CompanyBrowseForm},
  PerformanceView in 'PerformanceView.pas' {PerformanceViewForm},
  Welcome in 'Welcome.pas' {WelcomeForm},
  PersonBrowse in 'PersonBrowse.pas' {PersonBrowseForm},
  BasicBrowse in 'BasicBrowse.pas' {BasicBrowseForm},
  CountryBrowse in 'CountryBrowse.pas' {CountryBrowseForm},
  DemoDataRequest in 'DemoDataRequest.pas' {DemoDataRequestForm},
  ContactSort in 'ContactSort.pas' {ContactSortForm},
  CategoryBrowse in 'CategoryBrowse.pas' {CategoryBrowseForm},
  HelpView in 'HelpView.pas' {HelpViewForm},
  QueryView in 'QueryView.pas' {QueryViewForm},
  RandomData in 'RandomData.pas',
  Stopwatch in 'Stopwatch.pas',
  Utility in 'Utility.pas';

{$R *.RES}
{$R *.MDR} {Model}

begin
  Application.Initialize;
  Application.Title := 'InstantObjects Primer';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainDataModule, MainDataModule);
  Application.Run;
end.
