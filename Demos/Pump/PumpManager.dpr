program PumpManager;

uses
  Forms,
  InstantPumpConnectionManagerFormUnit in 'InstantPumpConnectionManagerFormUnit.pas' {InstantPumpConnectionManagerForm},
  InstantConnectionManagerFormUnit in '..\..\Source\Core\InstantConnectionManagerFormUnit.pas' {InstantConnectionManagerForm},
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
