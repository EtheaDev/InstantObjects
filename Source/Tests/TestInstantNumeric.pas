unit TestInstantNumeric;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantNumeric
  TestTInstantNumeric = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantNumeric: TInstantNumeric;
    FOwner: TInstantObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsBoolean;
    procedure TestAsDateTime;
    procedure TestAsObject;
    procedure TestDisplayText;
    procedure TestIsDefault;
  end;

implementation

uses SysUtils, testregistry, InstantClasses;

procedure TestTInstantNumeric.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantInteger;
  FAttrMetadata.Name := 'AttrMetadataName';
  // TInstantNumeric is abstract so use TInstantInteger
  FInstantNumeric := TInstantInteger.Create(FOwner, FAttrMetadata);
end;

procedure TestTInstantNumeric.TearDown;
begin
  FreeAndNil(FInstantNumeric);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantNumeric.TestAsBoolean;
begin
  FInstantNumeric.AsBoolean := True;
  AssertEquals('Set AsBoolean is incorrect!', 1, FInstantNumeric.Value);
  AssertTrue('Get AsBoolean is false!', FInstantNumeric.AsBoolean);

  FInstantNumeric.AsBoolean := False;
  AssertEquals('Set AsBoolean is incorrect!', 0, FInstantNumeric.Value);
  AssertFalse('Get AsBoolean is true!', FInstantNumeric.AsBoolean);
end;

procedure TestTInstantNumeric.TestAsDateTime;
begin
  FInstantNumeric.AsDateTime := 12.45;
  AssertEquals('Set AsDateTime is incorrect!', 12, FInstantNumeric.Value);
  AssertEquals('Get AsDateTime is incorrect!', 12.0,
    FInstantNumeric.AsDateTime);
end;

procedure TestTInstantNumeric.TestAsObject;
begin
  try
    FInstantNumeric.AsObject := TInstantObject.Create(FConn);
    Fail('Exception was not thrown for Set AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantNumeric.AsObject;
    Fail('Exception was not thrown for Get AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantNumeric.TestDisplayText;
begin
  FInstantNumeric.Value := 1;
  AssertEquals('DisplayText is incorrect!', '1', FInstantNumeric.DisplayText);

  FInstantNumeric.Metadata.EditMask := '000';
  AssertEquals('DisplayText is incorrect!', '001', FInstantNumeric.DisplayText);

  FInstantNumeric.Value := 1000;
  FInstantNumeric.Metadata.EditMask := '#' + ThousandSeparator + '000';
  AssertEquals('DisplayText is incorrect!', '1' + ThousandSeparator + '000',
    FInstantNumeric.DisplayText);
end;

procedure TestTInstantNumeric.TestIsDefault;
begin
  AssertTrue('Value is not default!', FInstantNumeric.IsDefault);

  FInstantNumeric.Value := 100;
  AssertFalse('Value is default!', FInstantNumeric.IsDefault);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantNumeric]);
{$ENDIF}

end.
 