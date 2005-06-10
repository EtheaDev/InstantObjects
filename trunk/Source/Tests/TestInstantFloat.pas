unit TestInstantFloat;

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantFloat
  TestTInstantFloat = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantFloat: TInstantFloat;
    FOwner: TPerson;
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

procedure TestTInstantFloat.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TPerson.Create(FConn);
  FInstantFloat := FOwner._AL_hours;
  FInstantFloat.Value := 1.3;
end;

procedure TestTInstantFloat.TearDown;
begin
  FInstantFloat := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantFloat.TestAsCurrency;
var
  vCurr: Currency;
begin
  vCurr := 23.45;
  FInstantFloat.AsCurrency := vCurr;
  AssertEquals(23.45, FInstantFloat.Value);
  AssertEquals(vCurr, FInstantFloat.AsCurrency);
end;

procedure TestTInstantFloat.TestAsFloat;
begin
  FInstantFloat.AsFloat := 89.45;
  AssertEquals(89.45, FInstantFloat.Value);
  AssertEquals(89.45, FInstantFloat.AsFloat);
end;

procedure TestTInstantFloat.TestAsInteger;
begin
  FInstantFloat.AsInteger := 89;
  AssertEquals(89.0, FInstantFloat.Value);
  AssertEquals(89.0, FInstantFloat.AsInteger);
end;

procedure TestTInstantFloat.TestAssign;
var
  vSource: TInstantFloat;
begin
  AssertEquals(1.3, FInstantFloat.Value);
  vSource := TInstantFloat.Create;
  try
    VSource.Value := 4.3;
    FInstantFloat.Assign(vSource);
    AssertEquals(4.3, FInstantFloat.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantFloat.TestAsString;
begin
  FInstantFloat.AsString := '1' + DecimalSeparator + '3';
  AssertEquals(1.3, FInstantFloat.Value);
  AssertEquals('1' + DecimalSeparator + '3', FInstantFloat.AsString);
end;

procedure TestTInstantFloat.TestAsVariant;
begin                                           
  FInstantFloat.AsVariant := 15.1;
  AssertEquals(15.1, FInstantFloat.Value);
  AssertEquals(15.1, FInstantFloat.AsVariant);
end;

procedure TestTInstantFloat.TestReset;
begin
  AssertNotNull(FInstantFloat.Metadata);
  // Metadata.DefaultValue is '';
  FInstantFloat.Reset;
  AssertEquals(1.3, FInstantFloat.Value);

  FInstantFloat.Metadata.DefaultValue := '15' + DecimalSeparator + '7';
  FInstantFloat.Reset;
  AssertEquals(15.7, FInstantFloat.Value);

  FInstantFloat.Metadata := nil;
  AssertNull(FInstantFloat.Metadata);
  FInstantFloat.Reset;
  AssertEquals(0.0, FInstantFloat.Value);
end;

procedure TestTInstantFloat.TestValue;
begin
  AssertEquals(1.3, FInstantFloat.Value);
  FInstantFloat.Value := 97.2;
  AssertEquals(97.2, FInstantFloat.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantFloat]);
{$ENDIF}

end.
 