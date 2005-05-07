unit TestInstantFloat;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantFloat
  TestTInstantFloat = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantFloat: TInstantFloat;
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

procedure TestTInstantFloat.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantFloat;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantFloat := TInstantFloat.Create(FOwner, FAttrMetadata);
  FInstantFloat.Value := 1.3;
end;

procedure TestTInstantFloat.TearDown;
begin
  FreeAndNil(FInstantFloat);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantFloat.TestAsCurrency;
begin
  FInstantFloat.AsCurrency := 23.45;
  AssertEquals('Set AsCurrency is incorrect!', 23.45, FInstantFloat.Value);
  AssertEquals('Get AsCurrency is incorrect!', 23.45,
    FInstantFloat.AsCurrency);
end;

procedure TestTInstantFloat.TestAsFloat;
begin
  FInstantFloat.AsFloat := 89.45;
  AssertEquals('Set AsFloat is incorrect!', 89.45, FInstantFloat.Value);
  AssertEquals('Get AsFloat is incorrect!', 89.45, FInstantFloat.AsFloat);
end;

procedure TestTInstantFloat.TestAsInteger;
begin
  FInstantFloat.AsInteger := 89;
  AssertEquals('Set AsInteger is incorrect!', 89.0, FInstantFloat.Value);
  AssertEquals('Get AsInteger is incorrect!', 89.0, FInstantFloat.AsInteger);
end;

procedure TestTInstantFloat.TestAssign;
var
  vSource: TInstantFloat;
begin
  AssertEquals('Value is incorrect!', 1.3, FInstantFloat.Value);
  vSource := TInstantFloat.Create;
  try
    VSource.Value := 4.3;
    FInstantFloat.Assign(vSource);
    AssertEquals('Value is incorrect!', 4.3, FInstantFloat.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantFloat.TestAsString;
begin
  FInstantFloat.AsString := '1.3';
  AssertEquals('Set AsString is incorrect!', 1.3, FInstantFloat.Value);
  AssertEquals('Get AsString is incorrect!', '1.3', FInstantFloat.AsString);
end;

procedure TestTInstantFloat.TestAsVariant;
begin                                           
  FInstantFloat.AsVariant := 15.1;
  AssertEquals('Set AsVariant is incorrect!', 15.1, FInstantFloat.Value);
  AssertEquals('Get AsVariant is incorrect!', 15.1, FInstantFloat.AsVariant);
end;

procedure TestTInstantFloat.TestReset;
begin
  AssertNotNull('Metadata is nil!', FInstantFloat.Metadata);
  // Metadata.DefaultValue is '';
  FInstantFloat.Reset;
  AssertEquals('Reset value is incorrect!', 1.3, FInstantFloat.Value);

  FInstantFloat.Metadata.DefaultValue := '15.7';
  FInstantFloat.Reset;
  AssertEquals('Reset value is incorrect!', 15.7, FInstantFloat.Value);

  FInstantFloat.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantFloat.Metadata);
  FInstantFloat.Reset;
  AssertEquals('Reset value is incorrect!', 0.0, FInstantFloat.Value);
end;

procedure TestTInstantFloat.TestValue;
begin
  AssertEquals('Value is incorrect!', 1.3, FInstantFloat.Value);
  FInstantFloat.Value := 97.2;
  AssertEquals('Value is incorrect!', 97.2, FInstantFloat.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantFloat]);
{$ENDIF}

end.
 