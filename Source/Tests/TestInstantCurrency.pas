unit TestInstantCurrency;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantCurrency
  TTestInstantCurrency = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FInstantCurrency: TInstantCurrency;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestReset;
    procedure TestValue;
  end;

implementation

uses SysUtils, testregistry, InstantClasses;

procedure TTestInstantCurrency.SetUp;
begin
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantCurrency;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantCurrency := TInstantCurrency.Create(nil, FAttrMetadata);
  FInstantCurrency.Value := 1.3;
end;

procedure TTestInstantCurrency.TearDown;
begin
  FreeAndNil(FInstantCurrency);
  FreeAndNil(FAttrMetadata);
end;

procedure TTestInstantCurrency.TestAssign;
var
  vSource: TInstantCurrency;
begin
  AssertEquals('Value is incorrect!', 1.3, FInstantCurrency.Value);
  vSource := TInstantCurrency.Create;
  try
    VSource.Value := 4.3;
    FInstantCurrency.Assign(vSource);
    AssertEquals('Value is incorrect!', 4.3, FInstantCurrency.Value);
  finally
    vSource.Free;
  end;
end;

procedure TTestInstantCurrency.TestReset;
begin
  AssertNotNull('Metadata is nil!', FInstantCurrency.Metadata);
  // Metadata.DefaultValue is '';
  FInstantCurrency.Reset;
  AssertEquals('Reset value is incorrect!', 1.3, FInstantCurrency.Value);

  FInstantCurrency.Metadata.DefaultValue := '15' + DecimalSeparator + '7';
  FInstantCurrency.Reset;
  AssertEquals('Reset value is incorrect!', 15.7, FInstantCurrency.Value);

  FInstantCurrency.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantCurrency.Metadata);
  FInstantCurrency.Reset;
  AssertEquals('Reset value is incorrect!', 0.0, FInstantCurrency.Value);
end;

procedure TTestInstantCurrency.TestValue;
var
  c: Currency;
begin
  c := 123456789012.12345;
  FInstantCurrency.AsCurrency := c;
  AssertEquals(c, FInstantCurrency.Value);
  AssertEquals(c, FInstantCurrency.AsCurrency);
  c := c + 0.0001;
  AssertFalse('Precision limit', c = FInstantCurrency.Value);
  FInstantCurrency.AsCurrency := c;
  c := c + 0.00001;
  AssertEquals('Out of precision limit', c, FInstantCurrency.Value);
  c := -0.0001;
  FInstantCurrency.AsCurrency := c;
  AssertEquals(c, FInstantCurrency.Value);
  AssertFalse(0 = FInstantCurrency.Value);
end;

initialization
  // Register any test cases with the test runner
  RegisterTests([TTestInstantCurrency]);

end.
