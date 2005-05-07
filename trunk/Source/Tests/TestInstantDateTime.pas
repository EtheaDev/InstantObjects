unit TestInstantDateTime;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantDateTime
  TestTInstantDateTime = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantDateTime: TInstantDateTime;
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

uses SysUtils, testregistry, InstantClasses;

procedure TestTInstantDateTime.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantDateTime;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantDateTime := TInstantDateTime.Create(FOwner, FAttrMetadata);
  FInstantDateTime.Value := 100.5;
end;

procedure TestTInstantDateTime.TearDown;
begin
  FreeAndNil(FInstantDateTime);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantDateTime.TestAsBoolean;
begin
  try
    FInstantDateTime.AsBoolean := True;
    Fail('Exception was not thrown for Set AsBoolean!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDateTime.AsBoolean;
    Fail('Exception was not thrown for Get AsBoolean!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDateTime.TestAsCurrency;
begin
  try
    FInstantDateTime.AsCurrency := 20.5;
    Fail('Exception was not thrown for Set AsCurrency!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDateTime.AsCurrency;
    Fail('Exception was not thrown for Get AsCurrency!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDateTime.TestAsDateTime;
begin
  FInstantDateTime.AsDateTime := 12.45;
  AssertEquals('Set AsDateTime is incorrect!', 12.45,
    FInstantDateTime.Value);
  AssertEquals('Get AsDateTime is incorrect!', 12.45,
    FInstantDateTime.AsDateTime);
end;

procedure TestTInstantDateTime.TestAsFloat;
begin
  try
    FInstantDateTime.AsFloat := 20.5;
    Fail('Exception was not thrown for Set AsFloat!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDateTime.AsFloat;
    Fail('Exception was not thrown for Get AsFloat!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDateTime.TestAsInteger;
begin
  try
    FInstantDateTime.AsInteger := 20;
    Fail('Exception was not thrown for Set AsInteger!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDateTime.AsInteger;
    Fail('Exception was not thrown for Get AsInteger!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDateTime.TestAsObject;
begin
  try
    FInstantDateTime.AsObject := TInstantObject.Create(FConn);
    Fail('Exception was not thrown for Set AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDateTime.AsObject;
    Fail('Exception was not thrown for Get AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDateTime.TestAssign;
var
  vSource: TInstantDateTime;
begin
  AssertEquals('String value is incorrect!', 100.5,
    FInstantDateTime.Value);

  vSource := TInstantDateTime.Create;
  try
    VSource.Value := 115.3;
    FInstantDateTime.Assign(vSource);
    AssertEquals('String value is incorrect!', 115.3,
      FInstantDateTime.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantDateTime.TestAsString;
begin
  FInstantDateTime.AsString := DateTimeToStr(14.5);
  AssertEquals('Set AsString is incorrect!', 14.5,
    FInstantDateTime.Value);
  AssertEquals('Get AsString is incorrect!', DateTimeToStr(14.5),
    FInstantDateTime.AsString);
end;

procedure TestTInstantDateTime.TestAsVariant;
begin                                           
  FInstantDateTime.AsVariant := 150.6;
  AssertEquals('Set AsVariant is incorrect!', 150.6,
    FInstantDateTime.Value);
  AssertEquals('Get AsVariant is incorrect!', 150.6,
    FInstantDateTime.AsVariant);
end;

procedure TestTInstantDateTime.TestDisplayText;
var
  vDateTimeStr: string;
begin
  AssertEquals('DisplayText is incorrect!', FInstantDateTime.AsString,
    FInstantDateTime.DisplayText);

  DateTimeToString(vDateTimeStr, 'yyyymmddhhnnsszzz', FInstantDateTime.Value);
  FInstantDateTime.Metadata.EditMask := 'yyyymmddhhnnsszzz';
  AssertEquals('DisplayText is incorrect!', vDateTimeStr,
  FInstantDateTime.DisplayText);

  FInstantDateTime.Value := 113.8;
  DateTimeToString(vDateTimeStr, 'dd mmm yyyy hh:nn:ss ampm',
    FInstantDateTime.Value);
  FInstantDateTime.Metadata.EditMask := 'dd mmm yyyy hh:nn:ss ampm';
  AssertEquals('DisplayText is incorrect!', vDateTimeStr,
    FInstantDateTime.DisplayText);
end;

procedure TestTInstantDateTime.TestIsDefault;
begin
  AssertFalse('Value is default!', FInstantDateTime.IsDefault);

  FInstantDateTime.Value := 0;
  AssertTrue('Value is not default!', FInstantDateTime.IsDefault);
end;

procedure TestTInstantDateTime.TestReset;
var
  vDateTimeStr: string;
begin
  AssertNotNull('Metadata is nil!', FInstantDateTime.Metadata);
  // Metadata.DefaultValue is '';
  FInstantDateTime.Reset;
  AssertEquals('Reset value is incorrect!', 0, FInstantDateTime.Value);

  DateTimeToString(vDateTimeStr, 'yyyymmddhhnnsszzz', 100.5);
  FInstantDateTime.Metadata.DefaultValue := vDateTimeStr;
  FInstantDateTime.Reset;
  AssertEquals('Reset value is incorrect!', 100.5, FInstantDateTime.Value);

  FInstantDateTime.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantDateTime.Metadata);
  FInstantDateTime.Reset;
  AssertEquals('Reset value is incorrect!', 0, FInstantDateTime.Value);
end;

procedure TestTInstantDateTime.TestValue;
begin
  AssertEquals('Value is incorrect!', 100.5, FInstantDateTime.Value);
  FInstantDateTime.Value := 151.3;
  AssertEquals('Value is incorrect!', 151.3, FInstantDateTime.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantDateTime]);
{$ENDIF}

end.
 