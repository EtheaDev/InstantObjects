program Pump;

uses
  Forms,
  UFmPump in 'UFmPump.pas' {FmPump},
  Model in '..\PrimerCross\Model.pas';

{$R *.res}
{$R *.mdr} {Model}

begin
  Application.Initialize;
  Application.CreateForm(TFmPump, FmPump);
  Application.Run;
end.
