program IntroIW;

uses
  Forms,
  IWMain,
  ServerController in 'ServerController.pas' {IWServerController: TIWServerController},
  MainIW in 'MainIW.pas' {MainForm: TIWFormModuleBase},
  UserSessionUnit in 'UserSessionUnit.pas' {IWUserSession: TIWUserSessionBase},
  Model in 'Model.pas',
  ContactEditIW in 'ContactEditIW.pas' {ContactEditForm: TIWAppForm},
  PersonEditIW in 'PersonEditIW.pas' {PersonEditForm: TIWAppForm},
  CompanyEditIW in 'CompanyEditIW.pas' {CompanyEditForm: TIWAppForm},
  QueryIW in 'QueryIW.pas' {QueryForm: TIWAppForm};

{$R *.res}
{$R *.MDR} {Model}
begin
  Application.Initialize;
  Application.CreateForm(TformIWMain, formIWMain);
  Application.Run;
end.
