unit TestInstantString;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantString
  TestTInstantString = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantString: TInstantString;
    FOwner: TInstantObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsBoolean;
    procedure TestAsCurrency;
    procedure TestAsDateTime;
    procedure TestAsFloat;
    procedure TestAsInteger;
    procedure TestAsObject;
    procedure TestAssign;
    procedure TestAsString;
    procedure TestAsVariant;
    procedure TestName;
    procedure TestOwner;
    procedure TestReset;
    procedure TestValue;
  end;

implementation

uses SysUtils, testregistry, InstantClasses;

procedure TestTInstantString.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantString;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantString := TInstantString.Create(FOwner, FAttrMetadata);
  FInstantString.Value := 'StringValue';
end;

procedure TestTInstantString.TearDown;
begin
  FreeAndNil(FInstantString);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantString.TestAsBoolean;
begin
  FInstantString.AsBoolean := True;
  AssertEquals('Set AsBoolean is incorrect!', 'True', FInstantString.Value);
  AssertTrue('Get AsBoolean is false!', FInstantString.AsBoolean);

  FInstantString.AsBoolean := False;
  AssertEquals('Set AsBoolean is incorrect!', 'False', FInstantString.Value);
  AssertFalse('Get AsBoolean is true!', FInstantString.AsBoolean);
end;

procedure TestTInstantString.TestAsCurrency;
begin
  FInstantString.AsCurrency := 23.45;
  AssertEquals('Set AsCurrency is incorrect!', '23.45', FInstantString.Value);
  AssertEquals('Get AsCurrency is incorrect!', 23.45,
    FInstantString.AsCurrency);
end;

procedure TestTInstantString.TestAsDateTime;
begin
  FInstantString.AsDateTime := 12.45;
  AssertEquals('Set AsDateTime is incorrect!', DateTimeToStr(12.45),
    FInstantString.Value);
  AssertEquals('Get AsDateTime is incorrect!', 12.45,
    FInstantString.AsDateTime);
end;

procedure TestTInstantString.TestAsFloat;
begin
  FInstantString.AsFloat := 89.45;
  AssertEquals('Set AsFloat is incorrect!', '89.45', FInstantString.Value);
  AssertEquals('Get AsFloat is incorrect!', 89.45, FInstantString.AsFloat);
end;

procedure TestTInstantString.TestAsInteger;
begin
  FInstantString.AsInteger := 100;
  AssertEquals('Set AsInteger is incorrect!', '100', FInstantString.Value);
  AssertEquals('Get AsInteger is incorrect!', 100, FInstantString.AsInteger);
end;

procedure TestTInstantString.TestAsObject;
begin
  try
    FInstantString.AsObject := TInstantObject.Create(FConn);
    Fail('Exception was not thrown for Set AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantString.AsObject;
    Fail('Exception was not thrown for Get AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantString.TestAssign;
var
  vSource: TInstantString;
begin
  AssertEquals('String value is incorrect!', 'StringValue',
    FInstantString.Value);

  vSource := TInstantString.Create;
  try
    VSource.Value := 'DifferentString';
    FInstantString.Assign(vSource);
    AssertEquals('String value is incorrect!', 'DifferentString',
      FInstantString.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantString.TestAsString;
begin
  FInstantString.AsString := 'DifferentString';
  AssertEquals('Set AsString is incorrect!', 'DifferentString',
    FInstantString.Value);
  AssertEquals('Get AsString is incorrect!', 'DifferentString',
    FInstantString.AsString);
end;

procedure TestTInstantString.TestAsVariant;
begin                                           
  FInstantString.AsVariant := 'DifferentString';
  AssertEquals('Set AsVariant is incorrect!', 'DifferentString',
    FInstantString.Value);
  AssertEquals('Get AsVariant is incorrect!', 'DifferentString',
    FInstantString.AsVariant);
end;

procedure TestTInstantString.TestName;
begin
  AssertEquals('Attribute name is incorrect!', 'AttrMetadataName',
    FInstantString.Name);
end;

procedure TestTInstantString.TestOwner;
begin
  AssertSame('Owner is incorrect!', FOwner, FInstantString.Owner);
end;

procedure TestTInstantString.TestReset;
begin
  AssertNotNull('Metadata is nil!', FInstantString.Metadata);
  // Metadata.DefaultValue is '';
  FInstantString.Reset;
  AssertEquals('Reset value is incorrect!', '', FInstantString.Value);

  FInstantString.Metadata.DefaultValue := '1000';
  FInstantString.Reset;
  AssertEquals('Reset value is incorrect!', '1000', FInstantString.Value);

  FInstantString.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantString.Metadata);
  FInstantString.Reset;
  AssertEquals('Reset value is incorrect!', '', FInstantString.Value);
end;

procedure TestTInstantString.TestValue;
begin
  AssertEquals('Value is incorrect!', 'StringValue', FInstantString.Value);
  FInstantString.Value := 'NewValue';
  AssertEquals('Value is incorrect!', 'NewValue', FInstantString.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantString]);
{$ENDIF}

end.
 