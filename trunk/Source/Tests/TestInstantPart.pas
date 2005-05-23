unit TestInstantPart;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantPart
  TestTInstantPart = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantPart: TInstantPart;
    FOwner: TInstantObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAllowOwned;
    procedure TestIsChanged;
    procedure TestIsDefault;
    procedure TestHasValue;
    procedure TestUnchanged;
    procedure TestValue_Reset;
  end;

implementation

uses SysUtils, testregistry;

procedure TestTInstantPart.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantPart;
  FAttrMetadata.StorageKind := skExternal;
  FInstantPart := TInstantPart.Create(FOwner, FAttrMetadata);
end;

procedure TestTInstantPart.TearDown;
begin
  FreeAndNil(FInstantPart);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantPart.TestAssign;
var
  vSource: TInstantPart;
  vAttrMetadata: TInstantAttributeMetadata;
  vPart: TInstantObject;
begin
  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  vAttrMetadata.AttributeClass := TInstantPart;
  vSource := TInstantPart.Create(FOwner, vAttrMetadata);
  try
    vPart := TInstantObject.Create(FConn);
    vSource.Value := vPart;
    AssertTrue(vSource.HasValue);

    AssertFalse(FInstantPart.HasValue);
    FInstantPart.Assign(vSource);
    AssertTrue(FInstantPart.HasValue);
    AssertNotSame(vPart, FInstantPart.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantPart.TestAllowOwned;
begin
  AssertTrue(FInstantPart.AllowOwned);
end;

procedure TestTInstantPart.TestIsChanged;
var
  vPart: TInstantObject;
begin
  AssertFalse(FInstantPart.IsChanged);

  vPart := TInstantObject.Create(FConn);
  vPart.Changed;
  FInstantPart.Value := vPart;
  AssertTrue(FInstantPart.IsChanged);
end;

procedure TestTInstantPart.TestIsDefault;
var
  vPart: TInstantObject;
begin
  AssertTrue(FInstantPart.IsDefault);

  vPart := TInstantObject.Create(FConn);
  vPart.Id := 'PartId';
  FInstantPart.Value := vPart;
  AssertFalse(FInstantPart.IsDefault);
end;

procedure TestTInstantPart.TestHasValue;
begin
  AssertFalse(FInstantPart.HasValue);

  FInstantPart.Value := TInstantObject.Create(FConn);
  AssertTrue(FInstantPart.HasValue);
end;

procedure TestTInstantPart.TestUnchanged;
begin
  FInstantPart.Changed;
  AssertTrue(FInstantPart.IsChanged);
  FInstantPart.Unchanged;
  AssertFalse(FInstantPart.IsChanged);
end;

procedure TestTInstantPart.TestValue_Reset;
var
  vFirstObj: TInstantObject;
  vSecondObj: TInstantObject;
begin
  AssertFalse(FInstantPart.HasValue);
  AssertNotNull(FInstantPart.Value);
  AssertTrue(FInstantPart.HasValue);
  vFirstObj := FInstantPart.Value;

  vSecondObj := TInstantObject.Create(FConn);
  vSecondObj.Id := 'PartId';
  FInstantPart.Value := vSecondObj;
  AssertEquals('PartId', FInstantPart.Value.Id);
  AssertNotSame(vFirstObj, FInstantPart.Value);

  FInstantPart.Reset;
  AssertFalse(FInstantPart.HasValue);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantPart]);
{$ENDIF}

end.
 