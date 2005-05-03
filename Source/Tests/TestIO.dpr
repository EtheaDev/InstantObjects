program TestIO;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

uses
{$IFDEF FPC}
  Interfaces,
{$ENDIF}
  Forms,
  GuiTestRunner,
  fpcunit,
  testregistry,
  testutils,
  testreport,
  InstantMock in 'InstantMock.pas',
  TestMockConnector in 'TestMockConnector.pas',
  TestMockBroker in 'TestMockBroker.pas',
  TestInstantMetadata in 'TestInstantMetadata.pas',
  TestModel in 'TestModel.pas',
  TestInstantFieldMetadata in 'TestInstantFieldMetadata.pas',
  TestInstantClassMetadata in 'TestInstantClassMetadata.pas',
  TestInstantAttributeMetadata in 'TestInstantAttributeMetadata.pas',
  TestInstantIndexMetadata in 'TestInstantIndexMetadata.pas',
  TestInstantTableMetadata in 'TestInstantTableMetadata.pas',
  TestInstantScheme in 'TestInstantScheme.pas',
  TestInstantClasses in 'TestInstantClasses.pas',
  TestInstantRtti in 'TestInstantRtti.pas',
  TestMinimalModel in 'TestMinimalModel.pas';

{$R *.res}
{$R *.mdr} {TestModel}

begin
  Application.Initialize;
  Application.CreateForm(TTestRunner, TestRunner);
  Application.Run;
end.

