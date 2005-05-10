unit TestInstantAttribute;

interface

uses fpcunit, InstantPersistence;

type

  // Test methods for class TInstantAttribute
  TestTInstantAttribute = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FInstantAttribute: TInstantAttribute;
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
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantString;
  FAttrMetadata.Name := 'AttrMetadataName';
  // TInstantAttribute is abstract so use TInstantString
  FInstantAttribute := TInstantString.Create(nil, FAttrMetadata);
end;

procedure TestTInstantAttribute.TearDown;
begin
  FreeAndNil(FInstantAttribute);
  FreeAndNil(FAttrMetadata);
end;

procedure TestTInstantAttribute.TestChange;
begin
  AssertFalse(FInstantAttribute.IsChanged);
  FInstantAttribute.Value := 'NewString';
  AssertTrue(FInstantAttribute.IsChanged);
  FInstantAttribute.UnChanged;
  AssertFalse(FInstantAttribute.IsChanged);
  FInstantAttribute.Changed;
  AssertTrue(FInstantAttribute.IsChanged);
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
  AssertEquals('StringValue', FInstantAttribute.DisplayText);

  FInstantAttribute.Metadata.EditMask := '!CCCCCC';
  AssertEquals('gValue', FInstantAttribute.DisplayText);

  FInstantAttribute.Value := 'NewString';
  FInstantAttribute.Metadata.EditMask := 'CCCCCC';
  AssertEquals('NewStr', FInstantAttribute.DisplayText);
end;

procedure TestTInstantAttribute.TestIsDefault;
begin
  AssertTrue(FInstantAttribute.IsDefault);

  FInstantAttribute.Value := 'NewString';
  AssertFalse(FInstantAttribute.IsDefault);
end;

procedure TestTInstantAttribute.TestIsIndexed;
begin
  AssertFalse(FInstantAttribute.IsIndexed);

  FInstantAttribute.Metadata.IsIndexed := True;
  AssertTrue(FInstantAttribute.IsIndexed);
end;

procedure TestTInstantAttribute.TestIsMandatory;
begin
  AssertFalse(FInstantAttribute.IsMandatory);

  FInstantAttribute.Metadata.IsIndexed := True;
  AssertTrue(FInstantAttribute.IsMandatory);
  FInstantAttribute.Metadata.IsRequired := True;
  AssertTrue(FInstantAttribute.IsMandatory);
  FInstantAttribute.Metadata.IsIndexed := False;
  AssertTrue(FInstantAttribute.IsMandatory);
end;

procedure TestTInstantAttribute.TestIsRequired;
begin
  AssertFalse(FInstantAttribute.IsRequired);

  FInstantAttribute.Metadata.IsRequired := True;
  AssertTrue(FInstantAttribute.IsRequired);
end;

procedure TestTInstantAttribute.TestMetadata;
begin
  AssertNotNull(FInstantAttribute.Metadata);
  AssertEquals('AttrMetadataName', FInstantAttribute.Metadata.Name);

  FInstantAttribute.Metadata := nil;
  AssertNull(FInstantAttribute.Metadata);
  FInstantAttribute.Reset;

  FInstantAttribute.Metadata := FAttrMetadata;
  AssertNotNull(FInstantAttribute.Metadata);
  AssertEquals('AttrMetadataName', FInstantAttribute.Metadata.Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantAttribute]);
{$ENDIF}

end.
 