{
    $Id: frameworktest.dpr,v 1.1 2005/02/11 22:12:05 decko Exp $
    Copyright (c) 2004 by Dean Zobec
        an example of a console test runner of FPCUnit tests.

    See the file COPYING.TXT, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program ConsoleTest;
{$APPTYPE CONSOLE}
uses
  classes,
  SysUtils,
  fpcunit in '..\fpcunit.pas',
  testreport in '..\testreport.pas',
  asserttest,
  suitetest,
  testutils in '..\testutils.pas',
  TestNotRefContInterface in 'TestNotRefContInterface.pas',
  TestIMock in 'TestIMock.pas',
  TestMock in 'TestMock.pas';

Type
  TTestRunner = class(TObject)
  private
    FSuite: TTestSuite;
    FXMLResultsWriter: TXMLResultsWriter;
  protected  
    procedure Run ;
    procedure doTestRun(aTest: TTest); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  end;


constructor TTestRunner.Create;
begin
  inherited Create;
  FXMLResultsWriter := TXMLResultsWriter.Create;
  FSuite := TTestSuite.Create;
  FSuite.TestName := 'Framework test';
  FSuite.AddTestSuiteFromClass(TAssertTest);
  FSuite.AddTest(TSuiteTest.Suite());
end;

destructor TTestRunner.Destroy;
begin
  FXMLResultsWriter.Free;
  FSuite.Free;
end;

procedure TTestRunner.doTestRun(aTest: TTest);
var
  testResult: TTestResult;
begin
  testResult := TTestResult.Create;
  try
    testResult.AddListener(FXMLResultsWriter); 
    FXMLResultsWriter.WriteHeader;   
    aTest.Run(testResult);
    FXMLResultsWriter.WriteResult(testResult);
  finally  
    testResult.Free;
  end;    
end;
  
procedure TTestRunner.Run;
var
  I : Integer;
  S : String;
begin
  if (ParamStr(1) = '-h') or (ParamStr(1) = '--help') then
  begin
    writeln('FPCUnit test runner');
    writeln('Version 0.1');
    writeln('Usage: ');
    writeln('-l or --list to show a list of registered tests');
    writeln('default format is xml, add --format=latex to output the list as latex source');
    writeln('-a or --all to run all the tests and show the results in xml format');
    writeln('The results can be redirected to an xml file,');
    writeln('for example: testrunner.exe --all > results.xml');
    writeln('use --suite MyTestSuiteName to run only the tests in a single test suite class');
  end
  else
    if (ParamStr(1) = '-l') or (ParamStr(1) = '--list') then
    begin
      if ParamStr(2) = '--format=latex' then
        writeln(GetSuiteAsLatex(FSuite))
      else
          writeln(GetSuiteAsXML(FSuite));
    end;

  if (ParamStr(1) = '-a') or (ParamStr(1) = '--all') then
  begin
    doTestRun(FSuite)
  end
  else
    if ParamStr(1) = '--suite' then
    begin
      S := '';
      S := ParamStr(2);
      if S = '' then
        for I := 0 to FSuite.Tests.count - 1 do
          writeln(FSuite[i].TestName)
      else
      for I := 0 to FSuite.Tests.count - 1 do
        if FSuite[i].TestName = S then
        begin
          doTestRun(FSuite.Test[i]);
        end;
    end;
end;

Var
  App : TTestRunner;

begin
  App:=TTestRunner.Create;
  App.Run;
  App.Free;
end.
