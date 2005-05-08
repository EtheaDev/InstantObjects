unit TestInstantBoolean;

interface

uses fpcunit, InstantPersistence, InstantMock;

Type

  // Test methods for class TInstantBoolean
  TestTInstantBoolean = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantBoolean: TInstantBoolean;
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
    procedure TestDisplayText;
    procedure TestIsDefault;
    procedure TestReset;
    procedure TestValue;
  end;

implementation

uses SysUtils, testregistry, InstantClasses, InstantConsts;

procedure TestTInstantBoolean.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantBoolean;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantBoolean := TInstantBoolean.Create(FOwner, FAttrMetadata);
  FInstantBoolean.Value := False;
end;

procedure TestTInstantBoolean.TearDown;
begin
  FreeAndNil(FInstantBoolean);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantBoolean.TestAsBoolean;
begin
  FInstantBoolean.AsBoolean := True;
  AssertEquals('Set AsBoolean is incorrect!', True, FInstantBoolean.Value);
  AssertTrue('Get AsBoolean is false!', FInstantBoolean.AsBoolean);

  FInstantBoolean.AsBoolean := False;
  AssertEquals('Set AsBoolean is incorrect!', False, FInstantBoolean.Value);
  AssertFalse('Get AsBoolean is true!', FInstantBoolean.AsBoolean);
end;

procedure TestTInstantBoolean.TestAsCurrency;
begin
  FInstantBoolean.AsCurrency := 1;
  AssertTrue('Set AsCurrency is incorrect!', FInstantBoolean.Value);
  AssertEquals('Get AsCurrency is incorrect!', 1, FInstantBoolean.AsCurrency);
end;

procedure TestTInstantBoolean.TestAsDateTime;
begin
  try
    FInstantBoolean.AsDateTime := 12.45;
    Fail('Exception was not thrown for Set AsDateTime!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
  else
    raise;
  end;
  try
    FInstantBoolean.AsDateTime;
    Fail('Exception was not thrown for Get AsDateTime!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
  else
    raise;
  end;
end;

procedure TestTInstantBoolean.TestAsFloat;
begin
  FInstantBoolean.AsFloat := 1;
  AssertTrue('Set AsFloat is incorrect!', FInstantBoolean.Value);
  AssertEquals('Get AsFloat is incorrect!', 1, FInstantBoolean.AsFloat);
end;

procedure TestTInstantBoolean.TestAsInteger;
begin
  FInstantBoolean.AsInteger := 1;
  AssertTrue('Set AsInteger is incorrect!', FInstantBoolean.Value);
  AssertEquals('Get AsInteger is incorrect!', 1, FInstantBoolean.AsInteger);
end;

procedure TestTInstantBoolean.TestAsObject;
begin
  try
    FInstantBoolean.AsObject := TInstantObject.Create(FConn);
    Fail('Exception was not thrown for Set AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantBoolean.AsObject;
    Fail('Exception was not thrown for Get AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantBoolean.TestAssign;
var
  vSource: TInstantBoolean;
begin
  AssertEquals('String value is incorrect!', False,
    FInstantBoolean.Value);

  vSource := TInstantBoolean.Create;
  try
    VSource.Value := True;
    FInstantBoolean.Assign(vSource);
    AssertEquals('String value is incorrect!', True,
      FInstantBoolean.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantBoolean.TestAsString;
begin
  FInstantBoolean.AsString := InstantTrueString;
  AssertTrue('Set AsString is incorrect!', FInstantBoolean.Value);
  AssertTrue('Get AsString is incorrect!', SameText(InstantTrueString,
    FInstantBoolean.AsString));
end;

procedure TestTInstantBoolean.TestAsVariant;
begin                                           
  FInstantBoolean.AsVariant := True;
  AssertTrue('Set AsVariant is incorrect!', FInstantBoolean.Value);
  AssertTrue('Get AsVariant is incorrect!', FInstantBoolean.AsVariant);
end;

procedure TestTInstantBoolean.TestDisplayText;
begin
  AssertTrue('DisplayText is incorrect!',
    SameText(InstantFalseString, FInstantBoolean.DisplayText));

  FInstantBoolean.Metadata.EditMask := '!CC';
  AssertEquals('DisplayText is incorrect!', 'se',
  FInstantBoolean.DisplayText);

  FInstantBoolean.Value := True;
  FInstantBoolean.Metadata.EditMask := 'CCC';
  AssertEquals('DisplayText is incorrect!', 'Tru',
    FInstantBoolean.DisplayText);
end;

procedure TestTInstantBoolean.TestIsDefault;
begin
  AssertTrue('Value is default!', FInstantBoolean.IsDefault);

  FInstantBoolean.Value := True;
  AssertFalse('Value is not default!', FInstantBoolean.IsDefault);
end;

procedure TestTInstantBoolean.TestReset;
begin
  AssertNotNull('Metadata is nil!', FInstantBoolean.Metadata);
  // Metadata.DefaultValue is '';
  FInstantBoolean.Reset;
  AssertEquals('Reset value is incorrect!', False, FInstantBoolean.Value);

  FInstantBoolean.Metadata.DefaultValue := InstantTrueString;
  FInstantBoolean.Reset;
  AssertEquals('Reset value is incorrect!', True, FInstantBoolean.Value);

  FInstantBoolean.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantBoolean.Metadata);
  FInstantBoolean.Reset;
  AssertEquals('Reset value is incorrect!', False, FInstantBoolean.Value);
end;

procedure TestTInstantBoolean.TestValue;
begin
  AssertEquals('Value is incorrect!', False, FInstantBoolean.Value);
  FInstantBoolean.Value := True;
  AssertEquals('Value is incorrect!', True, FInstantBoolean.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantBoolean]);
{$ENDIF}

end.
 
