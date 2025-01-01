program PumpManager;

uses
  Forms,
  InstantConnectionManagerFormUnit in '..\..\Source\Core\InstantConnectionManagerFormUnit.pas' {InstantConnectionManagerForm},
  InstantPumpConnectionManagerFormUnit in '..\..\Source\Core\InstantPumpConnectionManagerFormUnit.pas' {InstantPumpConnectionManagerForm},
  UPumpMain in 'UPumpMain.pas' {PumpDemoMain},
  Utility in '..\PrimerCross\Utility.pas',
  Model in '..\PrimerCross\Model\Model.pas';

{$R *.res}
{$R *.mdr} {Model}

begin
  Application.Initialize;
  Application.Title := 'InstantObjects Pump Demo';
  Application.CreateForm(TPumpDemoMain, PumpDemoMain);
  Application.Run;
end.
