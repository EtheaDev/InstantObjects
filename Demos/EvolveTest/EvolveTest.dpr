program EvolveTest;

uses
  Vcl.Forms,
  FMainEvolveTest in 'FMainEvolveTest.pas' {EvolverTestForm},
  Model in 'Model.pas';

{$R *.res}
{$R *.mdr} {Model}

begin
  Application.Initialize;
  Application.CreateForm(TEvolverTestForm, EvolverTestForm);
  Application.Run;
end.
