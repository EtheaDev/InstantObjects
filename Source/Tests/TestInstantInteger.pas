unit TestInstantInteger;

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantInteger
  TestTInstantInteger = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantInteger: TInstantInteger;
    FOwner: TCompany;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsCurrency;
    procedure TestAsFloat;
    procedure TestAsInteger;
    procedure TestAssign;
    procedure TestAsString;
    procedure TestAsVariant;
    procedure TestReset;
    procedure TestValue;
  end;

implementation

uses SysUtils, testregistry, InstantClasses;

procedure TestTInstantInteger.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TCompany.Create(FConn);
  FInstantInteger := FOwner._NoOfBranches;
  FInstantInteger.Value := 1;
end;

procedure TestTInstantInteger.TearDown;
begin
  FInstantInteger := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantInteger.TestAsCurrency;
var
  vCurr: Currency;
begin
  vCurr := 23.45;
  FInstantInteger.AsCurrency := vCurr;
  AssertEquals(23, FInstantInteger.Value);
  AssertEquals(23.0, FInstantInteger.AsCurrency);
end;

procedure TestTInstantInteger.TestAsFloat;
begin
  FInstantInteger.AsFloat := 89.45;
  AssertEquals(89, FInstantInteger.Value);
  AssertEquals(89.0, FInstantInteger.AsFloat);
end;

procedure TestTInstantInteger.TestAsInteger;
begin
  FInstantInteger.AsInteger := 100;
  AssertEquals(100, FInstantInteger.Value);
  AssertEquals(100, FInstantInteger.AsInteger);
end;

procedure TestTInstantInteger.TestAssign;
var
  vSource: TInstantInteger;
begin
  AssertEquals(1, FInstantInteger.Value);

  vSource := TInstantInteger.Create;
  try
    VSource.Value := 200;
    FInstantInteger.Assign(vSource);
    AssertEquals(200, FInstantInteger.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantInteger.TestAsString;
begin
  FInstantInteger.AsString := '73';
  AssertEquals(73, FInstantInteger.Value);
  AssertEquals('73', FInstantInteger.AsString);
end;

procedure TestTInstantInteger.TestAsVariant;
begin                                           
  FInstantInteger.AsVariant := 15;
  AssertEquals(15, FInstantInteger.Value);
  AssertEquals(15, FInstantInteger.AsVariant);
end;

procedure TestTInstantInteger.TestReset;
begin
  AssertNotNull(FInstantInteger.Metadata);
  // Metadata.DefaultValue is '';
  FInstantInteger.Reset;
  AssertEquals(0, FInstantInteger.Value);

  FInstantInteger.Metadata.DefaultValue := '1000';
  FInstantInteger.Reset;
  AssertEquals(1000, FInstantInteger.Value);

  FInstantInteger.Metadata := nil;
  AssertNull(FInstantInteger.Metadata);
  FInstantInteger.Reset;
  AssertEquals(0, FInstantInteger.Value);
end;

procedure TestTInstantInteger.TestValue;
begin
  AssertEquals(1, FInstantInteger.Value);
  FInstantInteger.Value := 1000;
  AssertEquals(1000, FInstantInteger.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantInteger]);
{$ENDIF}

end.
 