unit TestInstantCurrency;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantCurrency
  TestTInstantCurrency = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantCurrency: TInstantCurrency;
    FOwner: TInstantObject;
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
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantCurrency;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantCurrency := TInstantCurrency.Create(FOwner, FAttrMetadata);
  FInstantCurrency.Value := 1.3;
end;

procedure TestTInstantCurrency.TearDown;
begin
  FreeAndNil(FInstantCurrency);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantCurrency.TestAsCurrency;
begin
  FInstantCurrency.AsCurrency := 23.45;
  AssertEquals('Set AsCurrency is incorrect!', 23.45, FInstantCurrency.Value);
  AssertEquals('Get AsCurrency is incorrect!', 23.45,
    FInstantCurrency.AsCurrency);
end;

procedure TestTInstantCurrency.TestAsFloat;
begin
  FInstantCurrency.AsFloat := 89.45;
  AssertEquals('Set AsFloat is incorrect!', 89.45, FInstantCurrency.Value);
  AssertEquals('Get AsFloat is incorrect!', 89.45, FInstantCurrency.AsFloat);
end;

procedure TestTInstantCurrency.TestAsInteger;
begin
  FInstantCurrency.AsInteger := 89;
  AssertEquals('Set AsInteger is incorrect!', 89.0, FInstantCurrency.Value);
  AssertEquals('Get AsInteger is incorrect!', 89.0, FInstantCurrency.AsInteger);
end;

procedure TestTInstantCurrency.TestAssign;
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

procedure TestTInstantCurrency.TestAsString;
begin
  FInstantCurrency.AsString := '1.3';
  AssertEquals('Set AsString is incorrect!', 1.3, FInstantCurrency.Value);
  AssertEquals('Get AsString is incorrect!', '1.3', FInstantCurrency.AsString);
end;

procedure TestTInstantCurrency.TestAsVariant;
begin                                           
  FInstantCurrency.AsVariant := 15.1;
  AssertEquals('Set AsVariant is incorrect!', 15.1, FInstantCurrency.Value);
  AssertEquals('Get AsVariant is incorrect!', 15.1, FInstantCurrency.AsVariant);
end;

procedure TestTInstantCurrency.TestReset;
begin
  AssertNotNull('Metadata is nil!', FInstantCurrency.Metadata);
  // Metadata.DefaultValue is '';
  FInstantCurrency.Reset;
  AssertEquals('Reset value is incorrect!', 1.3, FInstantCurrency.Value);

  FInstantCurrency.Metadata.DefaultValue := '15.7';
  FInstantCurrency.Reset;
  AssertEquals('Reset value is incorrect!', 15.7, FInstantCurrency.Value);

  FInstantCurrency.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantCurrency.Metadata);
  FInstantCurrency.Reset;
  AssertEquals('Reset value is incorrect!', 0.0, FInstantCurrency.Value);
end;

procedure TestTInstantCurrency.TestValue;
begin
  AssertEquals('Value is incorrect!', 1.3, FInstantCurrency.Value);
  FInstantCurrency.Value := 97.2;
  AssertEquals('Value is incorrect!', 97.2, FInstantCurrency.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantCurrency]);
{$ENDIF}

end.
 