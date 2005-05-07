unit TestInstantInteger;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantInteger
  TestTInstantInteger = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantInteger: TInstantInteger;
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

procedure TestTInstantInteger.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantInteger;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantInteger := TInstantInteger.Create(FOwner, FAttrMetadata);
  FInstantInteger.Value := 1;
end;

procedure TestTInstantInteger.TearDown;
begin
  FreeAndNil(FInstantInteger);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantInteger.TestAsCurrency;
begin
  FInstantInteger.AsCurrency := 23.45;
  AssertEquals('Set AsCurrency is incorrect!', 23, FInstantInteger.Value);
  AssertEquals('Get AsCurrency is incorrect!', 23.0,
    FInstantInteger.AsCurrency);
end;

procedure TestTInstantInteger.TestAsFloat;
begin
  FInstantInteger.AsFloat := 89.45;
  AssertEquals('Set AsFloat is incorrect!', 89, FInstantInteger.Value);
  AssertEquals('Get AsFloat is incorrect!', 89.0, FInstantInteger.AsFloat);
end;

procedure TestTInstantInteger.TestAsInteger;
begin
  FInstantInteger.AsInteger := 100;
  AssertEquals('Set AsInteger is incorrect!', 100, FInstantInteger.Value);
  AssertEquals('Get AsInteger is incorrect!', 100, FInstantInteger.AsInteger);
end;

procedure TestTInstantInteger.TestAssign;
var
  vSource: TInstantInteger;
begin
  AssertEquals('Integer value is incorrect!', 1, FInstantInteger.Value);

  vSource := TInstantInteger.Create;
  try
    VSource.Value := 200;
    FInstantInteger.Assign(vSource);
    AssertEquals('Integer value is incorrect!', 200, FInstantInteger.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantInteger.TestAsString;
begin
  FInstantInteger.AsString := '73';
  AssertEquals('Set AsString is incorrect!', 73, FInstantInteger.Value);
  AssertEquals('Get AsString is incorrect!', '73', FInstantInteger.AsString);
end;

procedure TestTInstantInteger.TestAsVariant;
begin                                           
  FInstantInteger.AsVariant := 15;
  AssertEquals('Set AsVariant is incorrect!', 15, FInstantInteger.Value);
  AssertEquals('Get AsVariant is incorrect!', 15, FInstantInteger.AsVariant);
end;

procedure TestTInstantInteger.TestReset;
begin
  AssertNotNull('Metadata is nil!', FInstantInteger.Metadata);
  // Metadata.DefaultValue is '';
  FInstantInteger.Reset;
  AssertEquals('Reset value is incorrect!', 0, FInstantInteger.Value);

  FInstantInteger.Metadata.DefaultValue := '1000';
  FInstantInteger.Reset;
  AssertEquals('Reset value is incorrect!', 1000, FInstantInteger.Value);

  FInstantInteger.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantInteger.Metadata);
  FInstantInteger.Reset;
  AssertEquals('Reset value is incorrect!', 0, FInstantInteger.Value);
end;

procedure TestTInstantInteger.TestValue;
begin
  AssertEquals('Value is incorrect!', 1, FInstantInteger.Value);
  FInstantInteger.Value := 1000;
  AssertEquals('Value is incorrect!', 1000, FInstantInteger.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantInteger]);
{$ENDIF}

end.
 