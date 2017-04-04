unit TestUnit;

interface
uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TestIO = class(TObject) 
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Atribute to supply parameters.
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);
  end;

implementation

procedure TestIO.Setup;
begin
end;

procedure TestIO.TearDown;
begin
end;

procedure TestIO.Test1;
begin
end;

procedure TestIO.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TestIO);
end.
