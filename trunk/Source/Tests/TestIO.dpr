program TestIO;

uses
  Forms,
  TestMockConnector in 'TestMockConnector.pas',
  guitestrunner {TestRunner},
  fpcunit,
  testregistry,
  testutils,
  testreport,
  InstantMock in 'InstantMock.pas',
  TestMockBroker in 'TestMockBroker.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestRunner, TestRunner);
  Application.Run;
end.
