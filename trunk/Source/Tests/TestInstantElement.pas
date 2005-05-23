unit TestInstantElement;

interface

uses fpcunit, InstantPersistence, InstantMock;

type

  // Test methods for class TInstantElement
  TestTInstantElement = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FConn: TInstantMockConnector;
    FInstantElement: TInstantElement;
    FOwner: TInstantObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAttach_DetachObject;
    procedure TestHasValue;
    procedure TestSaveObjectTo_FromStream;
  end;

implementation

uses SysUtils, Classes, InstantClasses, testregistry;

procedure TestTInstantElement.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;
  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantPart;
  FInstantElement := TInstantPart.Create(FOwner, FAttrMetadata);
end;

procedure TestTInstantElement.TearDown;
begin
  FreeAndNil(FInstantElement);
  FreeAndNil(FAttrMetadata);
  FreeAndNil(FOwner);
  FreeAndNil(FConn);
end;

procedure TestTInstantElement.TestAttach_DetachObject;
var
  vReturnValue: Boolean;
  vObject: TInstantObject;
begin
  vObject := TInstantObject.Create(FConn);
  vObject.Id := 'Object.Id';
  AssertEquals('Object RefCount 1', 1, vObject.RefCount);

  vReturnValue := FInstantElement.AttachObject(vObject);
  AssertTrue('AttachObject', vReturnValue);
  AssertSame(vObject, FInstantElement.Value);
  AssertTrue('FInstantElement HasValue', FInstantElement.HasValue);
  AssertEquals('Value RefCount 1', 1, FInstantElement.Value.RefCount);
  AssertEquals('Object RefCount 2', 1, vObject.RefCount);

  vReturnValue := FInstantElement.DetachObject(vObject);
  AssertTrue('DetachObject', vReturnValue);
  AssertEquals('Object RefCount 3', 0, vObject.RefCount);
end;

procedure TestTInstantElement.TestHasValue;
begin
  AssertFalse(FInstantElement.HasValue);
end;

procedure TestTInstantElement.TestSaveObjectTo_FromStream;
var
  vObject: TInstantObject;
  vReturnValue: Boolean;
  vStream: TStream;
begin
  vObject := TInstantObject.Create(FConn);
  AssertNotNull('Create object', vObject);
  AssertEquals('Object RefCount 1', 1, vObject.RefCount);
  vReturnValue := FInstantElement.AttachObject(vObject);
  AssertTrue('AttachObject', vReturnValue);
  AssertTrue(FInstantElement.HasValue);
  AssertEquals('Value RefCount 1', 1, FInstantElement.Value.RefCount);
  AssertEquals('Object RefCount 2', 1, vObject.RefCount);

  vStream := TInstantStream.Create;
  try
    FInstantElement.SaveObjectToStream(vStream);
    AssertTrue('vStream.Size check', vStream.Size > 0);
    FInstantElement.Value := nil;
    AssertFalse(FInstantElement.HasValue);
    AssertEquals('Object RefCount 3', 0, vObject.RefCount);
    vStream.Position := 0;
    FInstantElement.LoadObjectFromStream(vStream);
    AssertTrue(FInstantElement.HasValue);
    AssertEquals('Value RefCount 2', 1, FInstantElement.Value.RefCount);
    AssertEquals('Object RefCount 4', 0, vObject.RefCount);
    AssertNotSame(vObject, FInstantElement.Value);
  finally
    vStream.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantElement]);
{$ELSE}
  RegisterTests([TestTInstantElement]);
{$ENDIF}

end.
