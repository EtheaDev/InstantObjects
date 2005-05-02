program TestIO;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

uses
{$IFDEF FPC}  Interfaces,
{$ENDIF}
  Forms, GuiTestRunner,
  fpcunit,
  testregistry,
  testutils,
  testreport,
  InstantMock in 'InstantMock.pas',
  TestMockConnector in 'TestMockConnector.pas',
  TestMockBroker in 'TestMockBroker.pas',
  TestInstantPersistence in 'TestInstantPersistence.pas',
  TestInstantClasses in 'TestInstantClasses.pas',
  TestInstantRtti in 'TestInstantRtti.pas',
  TestMinimalModel in 'TestMinimalModel.pas',
  TestMinimalModelDb  in 'TestMinimalModelDb.pas',
  TestModelDb in 'TestModelDb.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGUITestRunner, TestRunner);
  Application.Run;
end.

