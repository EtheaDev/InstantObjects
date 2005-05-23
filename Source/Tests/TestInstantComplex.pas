unit TestInstantComplex;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantComplex
  TestTInstantComplex = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantComplex: TInstantComplex;
    FOwner: TInstantObject;
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
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantComplex;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantComplex := TInstantComplex.Create(FOwner, FAttrMetadata);
end;

procedure TestTInstantComplex.TearDown;
begin
  FreeAndNil(FInstantComplex);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
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
{$ELSE}
  RegisterTests([TestTInstantComplex]);
{$ENDIF}

end.
 