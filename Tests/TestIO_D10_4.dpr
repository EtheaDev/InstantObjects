program TestIO_D10_4;

{$I '..\Source\InstantDefines.inc'}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

uses
{$IFNDEF D9+}
  FastMM4,
{$ENDIF}
{$IFDEF FPC}
  Interfaces,
{$ENDIF}
  Forms,
  guitestrunner,
  fpcunit,
  testregistry,
  testutils,
  testreport,
  InstantPersistence,
  TestModel in 'TestModel.pas',
  InstantMock in 'InstantMock.pas',
  TestInstantExposer in 'TestInstantExposer.pas';

{$R *.res}
{$R *.mdr} {TestModel}

begin
  Application.Initialize;
  InstantModel.ClassMetadatas.Clear;
  Application.CreateForm(TGUITestRunner, TestRunner);
  //Application.CreateForm(TTestRunner, TestRunner);
  Application.Run;
end.

