unit TestInstantComplex;

interface

uses fpcunit, InstantPersistence;

type

  // Test methods for class TInstantComplex
  TestTInstantComplex = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FInstantComplex: TInstantComplex;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAllowOwned;
    procedure TestAttachObject;
    procedure TestConnector;
    procedure TestDetachObject;
    procedure TestRequiredClass;
    procedure TestRequiredClassName;
  end;

implementation

uses SysUtils, testregistry;

procedure TestTInstantComplex.SetUp;
begin
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantComplex;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantComplex := TInstantComplex.Create(nil, FAttrMetadata);
end;

procedure TestTInstantComplex.TearDown;
begin
  FreeAndNil(FInstantComplex);
  FreeAndNil(FAttrMetadata);
end;

procedure TestTInstantComplex.TestAllowOwned;
begin
  AssertFalse(FInstantComplex.AllowOwned);
end;

procedure TestTInstantComplex.TestAttachObject;
begin
  AssertFalse(FInstantComplex.AttachObject(nil));
end;

procedure TestTInstantComplex.TestConnector;
begin
  AssertNotNull(FInstantComplex.Connector);
end;

procedure TestTInstantComplex.TestDetachObject;
begin
  AssertFalse(FInstantComplex.DetachObject(nil));
end;

procedure TestTInstantComplex.TestRequiredClass;
begin
  AssertEquals(TInstantObject, FInstantComplex.RequiredClass);
end;

procedure TestTInstantComplex.TestRequiredClassName;
begin
  AssertEquals('', FInstantComplex.RequiredClassName);
  FAttrMetadata.ObjectClassName := 'TInstantObject';
  AssertEquals('TInstantObject', FInstantComplex.RequiredClassName);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantComplex]);
{$ENDIF}

end.
 