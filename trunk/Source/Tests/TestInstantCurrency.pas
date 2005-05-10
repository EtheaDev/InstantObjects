unit TestInstantCurrency;

interface

uses fpcunit, InstantPersistence;

type

  // Test methods for class TInstantCurrency
  TestTInstantCurrency = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FInstantCurrency: TInstantCurrency;
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

procedure TestTInstantCurrency.SetUp;
var
  vCurr: Currency;
begin
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantCurrency;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantCurrency := TInstantCurrency.Create(nil, FAttrMetadata);
  vCurr := 1.3;
  FInstantCurrency.Value := vCurr;
end;

procedure TestTInstantCurrency.TearDown;
begin
  FreeAndNil(FInstantCurrency);
  FreeAndNil(FAttrMetadata);
end;

procedure TestTInstantCurrency.TestAsCurrency;
var
  vCurr: Currency;
begin
  vCurr := 23.45;
  FInstantCurrency.AsCurrency := vCurr;
  AssertEquals(vCurr, FInstantCurrency.Value);
  AssertEquals(vCurr, FInstantCurrency.AsCurrency);
end;

procedure TestTInstantCurrency.TestAsFloat;
begin
  FInstantCurrency.AsFloat := 89.45;
  AssertEquals(89.45, FInstantCurrency.Value);
  AssertEquals(89.45, FInstantCurrency.AsFloat);
end;

procedure TestTInstantCurrency.TestAsInteger;
begin
  FInstantCurrency.AsInteger := 89;
  AssertEquals(89.0, FInstantCurrency.Value);
  AssertEquals(89.0, FInstantCurrency.AsInteger);
end;

procedure TestTInstantCurrency.TestAssign;
var
  vSource: TInstantCurrency;
  vCurr: Currency;
begin
  AssertEquals(1.3, FInstantCurrency.Value);
  vSource := TInstantCurrency.Create;
  try
    vCurr := 4.3;
    VSource.Value := vCurr;
    FInstantCurrency.Assign(vSource);
    AssertEquals(vCurr, FInstantCurrency.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantCurrency.TestAsString;
begin
  FInstantCurrency.AsString := '15' + DecimalSeparator + '7';
  AssertEquals(15.7, FInstantCurrency.Value);
  AssertEquals('15' + DecimalSeparator + '7', FInstantCurrency.AsString);
end;

procedure TestTInstantCurrency.TestAsVariant;
begin                                           
  FInstantCurrency.AsVariant := 15.1;
  AssertEquals(15.1, FInstantCurrency.Value);
  AssertEquals(15.1, FInstantCurrency.AsVariant);
end;

procedure TestTInstantCurrency.TestReset;
begin
  AssertNotNull(FInstantCurrency.Metadata);
  // Metadata.DefaultValue is '';
  FInstantCurrency.Reset;
  AssertEquals(1.3, FInstantCurrency.Value);

  FInstantCurrency.Metadata.DefaultValue := '15' + DecimalSeparator + '7';
  FInstantCurrency.Reset;
  AssertEquals(15.7, FInstantCurrency.Value);

  FInstantCurrency.Metadata := nil;
  AssertNull(FInstantCurrency.Metadata);
  FInstantCurrency.Reset;
  AssertEquals(0.0, FInstantCurrency.Value);
end;

procedure TestTInstantCurrency.TestValue;
var
  vCurr: Currency;
begin
  AssertEquals(1.3, FInstantCurrency.Value);

  vCurr := 123456789012.12345;
  FInstantCurrency.AsCurrency := vCurr;
  AssertEquals(vCurr, FInstantCurrency.Value);
  AssertEquals(vCurr, FInstantCurrency.AsCurrency);
  vCurr := vCurr + 0.0001;
  AssertFalse('Precision limit', vCurr = FInstantCurrency.Value);
  FInstantCurrency.AsCurrency := vCurr;
  vCurr := vCurr + 0.00001;
  AssertEquals('Out of precision limit', vCurr, FInstantCurrency.Value);
  vCurr := -0.0001;
  FInstantCurrency.AsCurrency := vCurr;
  AssertEquals(vCurr, FInstantCurrency.Value);
  AssertFalse(0 = FInstantCurrency.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantCurrency]);
{$ENDIF}

end.
 