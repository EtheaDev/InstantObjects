unit TestMockBroker;

interface

uses
  Classes, SysUtils,
  InstantPersistence,
  fpcunit,
  testregistry,
  InstantMock,
  UbMockObject,
  Model;

type

  TTestMockBroker = class(TTestCase)
  private
  protected
    FConn: TInstantMockConnector;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestModelMdx;
    procedure TestGetBroker;
    procedure TestBuildDatabase;
    procedure TestStoreAndRetrieveAddress;
    procedure TestStoreAndRetrieveContact;
  end;


implementation


{ TTestMockBroker }

procedure TTestMockBroker.TestModelMdx;
begin
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0),'.mdx'));

  AssertNotNull(InstantModel.ClassMetadatas.Find('TContact'));
end;


procedure TTestMockBroker.TestGetBroker;
var
  brok: TInstantMockBroker;
begin
  brok := (Fconn.Broker as TInstantMockBroker);
  AssertNotNull(brok);
  AssertEquals(brok.ClassType, TInstantMockBroker);
  brok.MockManager.StartSetUp;
  brok.MockManager.EndSetUp;
  Fconn.BuildDatabase(InstantModel);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.TestBuildDatabase;
var
  brok: TInstantMockBroker;
begin
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0),'.mdx'));
  brok := Fconn.Broker as TInstantMockBroker;
  brok.MockManager.StartSetUp;
  brok.MockManager.EndSetUp;
  Fconn.BuildDatabase(InstantModel);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.TestStoreAndRetrieveAddress;
var
  a: TAddress;
  old_id: string;
  brok: TInstantMockBroker;
begin
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0),'.mdx'));
  Fconn.IsDefault := True;
  Fconn.StartTransaction;
  brok := Fconn.Broker as TInstantMockBroker;
  brok.MockManager.StartSetUp;
  a := TAddress.Create;
  try
    a.City := 'Milan';
    a.Store();
    old_id := a.id;
  finally
    a.Free;
  end;
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalStoreObject ' + old_id);
  brok.MockManager.Verify;
  Fconn.CommitTransaction;
  brok.MockManager.StartSetUp;
  a := TAddress.Create;
  try
    a.Retrieve(old_id);
  finally
    a.Free;
  end;
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalRetrieveObject ' + old_id);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.TestStoreAndRetrieveContact;
var
  c: TContact;
  old_id: string;
  brok: TInstantMockBroker;
begin
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0),'.mdx'));

  Fconn.IsDefault := True;
  brok := Fconn.Broker as TInstantMockBroker;
  brok.MockManager.StartSetUp;
  c := TContact.Create;
  try
    c.Name := 'Mike';
    c.Address.City := 'Milan';
    c.Store();
    old_id := c.id;
  finally
    c.Free;
  end;
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalStoreObject ' + old_id);
  brok.MockManager.Verify;
  brok.MockManager.StartSetUp;
  c := TContact.Create;
  try
    c.Retrieve(old_id);
  finally
    c.Free;
  end;
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalRetrieveObject ' + old_id);
  brok.MockManager.Verify;
end;


procedure TTestMockBroker.SetUp;
begin
  inherited;
  FConn := TInstantMockConnector.Create(nil);
end;

procedure TTestMockBroker.TearDown;
begin
  FConn.Free;
  inherited;
end;

initialization
  RegisterTests([TTestMockBroker]);

end.
