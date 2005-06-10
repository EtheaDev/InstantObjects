unit TestInstantComplex;

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantComplex
  TestTInstantComplex = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantComplex: TInstantPart;
    FOwner: TContact;
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

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantComplex := FOwner._Address;
end;

procedure TestTInstantComplex.TearDown;
begin
  FInstantComplex := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
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
  AssertEquals(TAddress, FInstantComplex.RequiredClass);
end;

procedure TestTInstantComplex.TestRequiredClassName;
begin
  AssertEquals('TAddress', FInstantComplex.RequiredClassName);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantComplex]);
{$ENDIF}

end.
 