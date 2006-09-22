program TestMocks;

uses
  Forms,
  TestMock in 'TestMock.pas',
  TestIMock in 'TestIMock.pas',
  TestNotRefContInterface in 'TestNotRefContInterface.pas',
  guitestrunner {TestRunner},
  fpcunit,
  testregistry,
  testutils,
  testreport;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGUITestRunner, TestRunner);
  Application.Run;
end.
