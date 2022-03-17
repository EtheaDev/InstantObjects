program PrimerFM;

uses
  FMX.Forms,
  Model in '..\..\Source\Model.pas',
  RandomData in '..\..\Source\RandomData.pas',
  DemoData in '..\..\Source\DemoData.pas',
  Main in '..\..\Source\Main.pas' {fmMain};

{$R *.res}
{$R *.mdr} {Model}

begin
  Application.Initialize;
 Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
