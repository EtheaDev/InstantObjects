program TestIO_DUnit_D11;

{$I '..\Source\InstantDefines.inc'}

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}

uses
  SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  MinimalModel in 'MinimalModel.pas',
  TestModel in 'TestModel.pas',
  TestFullModel in 'TestFullModel.pas',
  InstantPersistence,
  InstantMock in 'InstantMock.pas',
  TestMockConnector in 'TestMockConnector.pas',
  TestMockBroker in 'TestMockBroker.pas',
  TestInstantMetadata in 'TestInstantMetadata.pas',
  TestInstantFieldMetadata in 'TestInstantFieldMetadata.pas',
  TestInstantClassMetadata in 'TestInstantClassMetadata.pas',
  TestInstantAttributeMetadata in 'TestInstantAttributeMetadata.pas',
  TestInstantIndexMetadata in 'TestInstantIndexMetadata.pas',
  TestInstantTableMetadata in 'TestInstantTableMetadata.pas',
  TestInstantScheme in 'TestInstantScheme.pas',
  TestInstantClasses in 'TestInstantClasses.pas',
  TestInstantRtti in 'TestInstantRtti.pas',
  TestInstantAttributeMap in 'TestInstantAttributeMap.pas',
  TestInstantAttribute in 'TestInstantAttribute.pas',
  TestInstantNumeric in 'TestInstantNumeric.pas',
  TestInstantInteger in 'TestInstantInteger.pas',
  TestInstantString in 'TestInstantString.pas',
  TestInstantDateTime in 'TestInstantDateTime.pas',
  TestInstantDate in 'TestInstantDate.pas',
  TestInstantTime in 'TestInstantTime.pas',
  TestInstantBoolean in 'TestInstantBoolean.pas',
  TestInstantFloat in 'TestInstantFloat.pas',
  TestInstantCurrency in 'TestInstantCurrency.pas',
  TestInstantBlob in 'TestInstantBlob.pas',
  TestInstantComplex in 'TestInstantComplex.pas',
  TestInstantPart in 'TestInstantPart.pas',
  TestInstantReference in 'TestInstantReference.pas',
  TestInstantObject in 'TestInstantObject.pas',
  TestInstantObjectState in 'TestInstantObjectState.pas',
  TestInstantCache in 'TestInstantCache.pas',
  TestInstantObjectStore in 'TestInstantObjectStore.pas',
  TestInstantParts in 'TestInstantParts.pas',
  TestInstantReferences in 'TestInstantReferences.pas',
  TestInstantCircularReferences in 'TestInstantCircularReferences.pas',
  TestInstantObjectReference in 'TestInstantObjectReference.pas',
  TestXMLBroker in 'TestXMLBroker.pas',
  TestInstantCode in 'TestInstantCode.pas',
  TestInstantSelector in 'TestInstantSelector.pas',
  TestInstantExposer in 'TestInstantExposer.pas';

{$R *.res}
{$R *.mdr} {TestModel}

{$IFNDEF TESTINSIGHT}
var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    TDUnitX.Options.XMLOutputFile := ExtractFilePath(ParamStr(0)) +
      'dunitx-results-Delphi11.xml';
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.

