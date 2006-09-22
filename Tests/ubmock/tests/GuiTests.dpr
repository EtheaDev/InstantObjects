program GuiTests;

uses
  Forms,
  guitestrunner {TestRunner},
  fpcunit,
  testregistry,
  testutils,
  testreport,
  asserttest in 'asserttest.pas',
  fpcunittests in 'fpcunittests.pas',
  suitetest in 'suitetest.pas',
  TestMock in 'TestMock.pas',
  TestIMock in 'TestIMock.pas',
  TestNotRefContInterface in 'TestNotRefContInterface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGUITestRunner, TestRunner);
  Application.Run;
end.
