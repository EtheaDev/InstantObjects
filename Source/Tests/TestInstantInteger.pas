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
    procedure TestAsBoolean;
    procedure TestAsCurrency;
    procedure TestAsDateTime;
    procedure TestAsFloat;
    procedure TestAsInteger;
    procedure TestAsObject;
    procedure TestAssign;
    procedure TestAsString;
    procedure TestAsVariant;
    procedure TestChange;
    procedure TestCheckHasMetadata;
    procedure TestDisplayText;
    procedure TestIsDefault;
    procedure TestIsIndexed;
    procedure TestIsMandatory;
    procedure TestIsRequired;
    procedure TestMetadata;
    procedure TestName;
    procedure TestOwner;
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

procedure TestTInstantInteger.TestAsBoolean;
begin
  FInstantInteger.AsBoolean := True;
  AssertEquals('Set AsBoolean is incorrect!', 1, FInstantInteger.Value);
  AssertTrue('Get AsBoolean is false!', FInstantInteger.AsBoolean);

  FInstantInteger.AsBoolean := False;
  AssertEquals('Set AsBoolean is incorrect!', 0, FInstantInteger.Value);
  AssertFalse('Get AsBoolean is true!', FInstantInteger.AsBoolean);
end;

procedure TestTInstantInteger.TestAsCurrency;
begin
  FInstantInteger.AsCurrency := 23.45;
  AssertEquals('Set AsCurrency is incorrect!', 23, FInstantInteger.Value);
  AssertEquals('Get AsCurrency is incorrect!', 23.0,
    FInstantInteger.AsCurrency);
end;

procedure TestTInstantInteger.TestAsDateTime;
begin
  FInstantInteger.AsDateTime := 12.45;
  AssertEquals('Set AsDateTime is incorrect!', 12, FInstantInteger.Value);
  AssertEquals('Get AsDateTime is incorrect!', 12.0,
    FInstantInteger.AsDateTime);
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

procedure TestTInstantInteger.TestAsObject;
begin
  try
    FInstantInteger.AsObject := TInstantObject.Create(FConn);
    Fail('Exception was not thrown for Set AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantInteger.AsObject;
    Fail('Exception was not thrown for Get AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
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

procedure TestTInstantInteger.TestChange;
begin
  AssertTrue('IsChanged is false!', FInstantInteger.IsChanged);
  FInstantInteger.UnChanged;
  AssertFalse('IsChanged is true!', FInstantInteger.IsChanged);
  FInstantInteger.Value := FInstantInteger.Value + 1;
  AssertTrue('IsChanged is false!', FInstantInteger.IsChanged);
  FInstantInteger.UnChanged;
  AssertFalse('IsChanged is true!', FInstantInteger.IsChanged);
  FInstantInteger.Changed;
  AssertTrue('IsChanged is false!', FInstantInteger.IsChanged);
end;

procedure TestTInstantInteger.TestCheckHasMetadata;
begin
  try
    FInstantInteger.CheckHasMetadata;
  except
    Fail('CheckHasMetadata failed!');
  end;

  FInstantInteger.Metadata := nil;
  AssertException(EInstantError, FInstantInteger.CheckHasMetadata);
end;

procedure TestTInstantInteger.TestDisplayText;
begin
  AssertEquals('DisplayText is incorrect!', '1', FInstantInteger.DisplayText);

  FInstantInteger.Metadata.EditMask := '000';
  AssertEquals('DisplayText is incorrect!', '001', FInstantInteger.DisplayText);

  FInstantInteger.Value := 1000;
  FInstantInteger.Metadata.EditMask := '#' + ThousandSeparator + '000';
  AssertEquals('DisplayText is incorrect!', '1' + ThousandSeparator + '000',
    FInstantInteger.DisplayText);
end;

procedure TestTInstantInteger.TestIsDefault;
begin
  AssertFalse('Value is default!', FInstantInteger.IsDefault);

  FInstantInteger.Value := 0;
  AssertTrue('Value is not default!', FInstantInteger.IsDefault);
end;

procedure TestTInstantInteger.TestIsIndexed;
begin
  AssertFalse('Attribute is indexed!', FInstantInteger.IsIndexed);

  FInstantInteger.Metadata.IsIndexed := True;
  AssertTrue('Attribute is not indexed!', FInstantInteger.IsIndexed);
end;

procedure TestTInstantInteger.TestIsMandatory;
begin
  AssertFalse('Attribute is Mandatory!', FInstantInteger.IsMandatory);

  FInstantInteger.Metadata.IsIndexed := True;
  AssertTrue('Attribute is not Mandatory!', FInstantInteger.IsMandatory);
  FInstantInteger.Metadata.IsRequired := True;
  AssertTrue('Attribute is not Mandatory!', FInstantInteger.IsMandatory);
  FInstantInteger.Metadata.IsIndexed := False;
  AssertTrue('Attribute is not Mandatory!', FInstantInteger.IsMandatory);
end;

procedure TestTInstantInteger.TestIsRequired;
begin
  AssertFalse('Attribute is required!', FInstantInteger.IsRequired);

  FInstantInteger.Metadata.IsRequired := True;
  AssertTrue('Attribute is not required!', FInstantInteger.IsRequired);
end;

procedure TestTInstantInteger.TestMetadata;
begin
  AssertNotNull('Metadata is nil!', FInstantInteger.Metadata);
  AssertEquals('Metdata name is incorrect!', 'AttrMetadataName',
    FInstantInteger.Metadata.Name);

  FInstantInteger.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantInteger.Metadata);
  FInstantInteger.Reset;

  FInstantInteger.Metadata := FAttrMetadata;
  AssertNotNull('Metadata is nil!', FInstantInteger.Metadata);
  AssertEquals('Metdata name is incorrect!', 'AttrMetadataName',
    FInstantInteger.Metadata.Name);
end;

procedure TestTInstantInteger.TestName;
begin
  AssertEquals('Attribute name is incorrect!', 'AttrMetadataName',
    FInstantInteger.Name);
end;

procedure TestTInstantInteger.TestOwner;
begin
  AssertSame('Owner is incorrect!', FOwner, FInstantInteger.Owner);
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
 