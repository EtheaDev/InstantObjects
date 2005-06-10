unit TestInstantElement;

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantElement
  TestTInstantElement = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantElement: TInstantElement;
    FOwner: TContact;
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

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantElement := FOwner._Address;
end;

procedure TestTInstantElement.TearDown;
begin
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantElement.TestAttach_DetachObject;
var
  vReturnValue: Boolean;
  vObject: TAddress;
begin
  vObject := TAddress.Create(FConn);
  vObject.Id := 'Object.Id';
  AssertEquals('Object RefCount 1', 1, vObject.RefCount);

  vReturnValue := FInstantElement.AttachObject(vObject);
  AssertTrue('AttachObject', vReturnValue);
  AssertSame(vObject, FInstantElement.Value);
  AssertTrue('FInstantElement HasValue', FInstantElement.HasValue);
  AssertEquals('Value RefCount 1', 1, FInstantElement.Value.RefCount);
  AssertEquals('Object RefCount 2', 1, vObject.RefCount);
  AssertEquals('Value.Id 1', 'Object.Id', FInstantElement.Value.Id);

  vReturnValue := FInstantElement.DetachObject(vObject);
  AssertFalse('DetachObject', vReturnValue);
  AssertEquals('Object RefCount 3', 1, vObject.RefCount);
  AssertEquals('Value.Id 2', '', FInstantElement.Value.Id);
end;

procedure TestTInstantElement.TestHasValue;
begin
  AssertFalse(FInstantElement.HasValue);
end;

procedure TestTInstantElement.TestSaveObjectTo_FromStream;
var
  vObject: TAddress;
  vReturnValue: Boolean;
  vStream: TStream;
begin
  vObject := TAddress.Create(FConn);
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
    vStream.Position := 0;
    FInstantElement.LoadObjectFromStream(vStream);
    AssertTrue(FInstantElement.HasValue);
    AssertEquals('Value RefCount 2', 1, FInstantElement.Value.RefCount);
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
