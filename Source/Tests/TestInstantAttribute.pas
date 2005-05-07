unit TestInstantAttribute;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantAttribute
  TestTInstantAttribute = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantAttribute: TInstantAttribute;
    FOwner: TInstantObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestChange;
    procedure TestCheckHasMetadata;
    procedure TestDisplayText;
    procedure TestIsDefault;
    procedure TestIsIndexed;
    procedure TestIsMandatory;
    procedure TestIsRequired;
    procedure TestMetadata;
  end;

implementation

uses SysUtils, testregistry, InstantClasses;

procedure TestTInstantAttribute.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantString;
  FAttrMetadata.Name := 'AttrMetadataName';
  // TInstantAttribute is abstract so use TInstantString
  FInstantAttribute := TInstantString.Create(FOwner, FAttrMetadata);
end;

procedure TestTInstantAttribute.TearDown;
begin
  FreeAndNil(FInstantAttribute);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantAttribute.TestChange;
begin
  AssertFalse('IsChanged is true!', FInstantAttribute.IsChanged);
  FInstantAttribute.Value := 'NewString';
  AssertTrue('IsChanged is false!', FInstantAttribute.IsChanged);
  FInstantAttribute.UnChanged;
  AssertFalse('IsChanged is true!', FInstantAttribute.IsChanged);
  FInstantAttribute.Changed;
  AssertTrue('IsChanged is false!', FInstantAttribute.IsChanged);
end;

procedure TestTInstantAttribute.TestCheckHasMetadata;
begin
  try
    FInstantAttribute.CheckHasMetadata;
  except
    Fail('CheckHasMetadata failed!');
  end;

  FInstantAttribute.Metadata := nil;
  AssertException(EInstantError, FInstantAttribute.CheckHasMetadata);
end;

procedure TestTInstantAttribute.TestDisplayText;
begin
  FInstantAttribute.Value := 'StringValue';
  AssertEquals('DisplayText is incorrect!', 'StringValue',
    FInstantAttribute.DisplayText);

  FInstantAttribute.Metadata.EditMask := '!CCCCCC';
  AssertEquals('DisplayText is incorrect!', 'gValue',
  FInstantAttribute.DisplayText);

  FInstantAttribute.Value := 'NewString';
  FInstantAttribute.Metadata.EditMask := 'CCCCCC';
  AssertEquals('DisplayText is incorrect!', 'NewStr',
    FInstantAttribute.DisplayText);
end;

procedure TestTInstantAttribute.TestIsDefault;
begin
  AssertTrue('Value is not default!', FInstantAttribute.IsDefault);

  FInstantAttribute.Value := 'NewString';
  AssertFalse('Value is default!', FInstantAttribute.IsDefault);
end;

procedure TestTInstantAttribute.TestIsIndexed;
begin
  AssertFalse('Attribute is indexed!', FInstantAttribute.IsIndexed);

  FInstantAttribute.Metadata.IsIndexed := True;
  AssertTrue('Attribute is not indexed!', FInstantAttribute.IsIndexed);
end;

procedure TestTInstantAttribute.TestIsMandatory;
begin
  AssertFalse('Attribute is Mandatory!', FInstantAttribute.IsMandatory);

  FInstantAttribute.Metadata.IsIndexed := True;
  AssertTrue('Attribute is not Mandatory!', FInstantAttribute.IsMandatory);
  FInstantAttribute.Metadata.IsRequired := True;
  AssertTrue('Attribute is not Mandatory!', FInstantAttribute.IsMandatory);
  FInstantAttribute.Metadata.IsIndexed := False;
  AssertTrue('Attribute is not Mandatory!', FInstantAttribute.IsMandatory);
end;

procedure TestTInstantAttribute.TestIsRequired;
begin
  AssertFalse('Attribute is required!', FInstantAttribute.IsRequired);

  FInstantAttribute.Metadata.IsRequired := True;
  AssertTrue('Attribute is not required!', FInstantAttribute.IsRequired);
end;

procedure TestTInstantAttribute.TestMetadata;
begin
  AssertNotNull('Metadata is nil!', FInstantAttribute.Metadata);
  AssertEquals('Metdata name is incorrect!', 'AttrMetadataName',
    FInstantAttribute.Metadata.Name);

  FInstantAttribute.Metadata := nil;
  AssertNull('Metadata is not nil!', FInstantAttribute.Metadata);
  FInstantAttribute.Reset;

  FInstantAttribute.Metadata := FAttrMetadata;
  AssertNotNull('Metadata is nil!', FInstantAttribute.Metadata);
  AssertEquals('Metdata name is incorrect!', 'AttrMetadataName',
    FInstantAttribute.Metadata.Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantAttribute]);
{$ENDIF}

end.
 