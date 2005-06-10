unit TestMockBroker;

interface

uses
  Classes, SysUtils, InstantPersistence, fpcunit, testregistry, InstantMock,
  TestModel;

type
  TTestMockBroker = class(TTestCase)
  private
  protected
    FConn: TInstantMockConnector;
    procedure SetUp; override;
    procedure TearDown; override;
  public
  published
    procedure TestModelFromToFile;
    procedure TestGetBroker;
    procedure TestBuildDatabase;
    procedure TestModelFromToResFile;
    procedure TestStoreAndRetrieveAddress;
  end;

  TTestMockRelationalBroker = class(TTestCase)
  private
  protected
    FConn: TInstantMockConnector;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetBroker;
    procedure TestStoreAndRetrieveContact;
    procedure TestParts;
  end;

implementation

procedure TTestMockBroker.TestModelFromToFile;
var
  vReturnValue: TInstantClassMetadata;
begin
  AssertTrue('ClassMetadatas.Count', InstantModel.ClassMetadatas.Count > 0);

  // This ensures that the exported file is synchronised
  // with the current model resource file.
  InstantModel.SaveToFile(ChangeFileExt(ParamStr(0), '.mdx'));
  InstantModel.ClassMetadatas.Clear;

  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0), '.mdx'));
  vReturnValue := InstantModel.ClassMetadatas.Find('TCategory');
  AssertNotNull(vReturnValue);

  InstantModel.ClassMetadatas.Remove(vReturnValue);
  AssertNull('TCategory was found!',
    InstantModel.ClassMetadatas.Find('TCategory'));
  InstantModel.SaveToFile(ChangeFileExt(ParamStr(0), '.mdxt'));

  InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0), '.mdxt'));
  AssertNotNull(InstantModel.ClassMetadatas.Find('TContact'));
  AssertNull(InstantModel.ClassMetadatas.Find('TCategory'));
end;

procedure TTestMockBroker.TestModelFromToResFile;
var
  vReturnValue: TInstantClassMetadata;
begin
  vReturnValue := InstantModel.ClassMetadatas.Find('TCategory');
  AssertNotNull(vReturnValue);

  InstantModel.ClassMetadatas.Remove(vReturnValue);
  AssertNull(InstantModel.ClassMetadatas.Find('TCategory'));

  InstantModel.SaveToResFile(ChangeFileExt(ParamStr(0), '.mdrt'));
  InstantModel.ClassMetadatas.Clear;

  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdrt'));
  AssertNotNull(InstantModel.ClassMetadatas.Find('TContact'));
  AssertNull(InstantModel.ClassMetadatas.Find('TCategory'));
end;

procedure TTestMockBroker.TestGetBroker;
var
  brok: TInstantMockBroker;
begin
  brok := (FConn.Broker as TInstantMockBroker);
  AssertNotNull(brok);
  AssertEquals(brok.ClassType, TInstantMockBroker);
  brok.MockManager.StartSetUp;
  brok.MockManager.EndSetUp;
  FConn.BuildDatabase(InstantModel);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.TestBuildDatabase;
var
  brok: TInstantMockBroker;
begin
  brok := FConn.Broker as TInstantMockBroker;
  brok.MockManager.StartSetUp;
  brok.MockManager.EndSetUp;
  FConn.BuildDatabase(InstantModel);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.TestStoreAndRetrieveAddress;
var
  a: TAddress;
  old_id: string;
  brok: TInstantMockBroker;
begin
  FConn.IsDefault := True;
  FConn.StartTransaction;
  brok := FConn.Broker as TInstantMockBroker;
  brok.MockManager.StartSetUp;
  a := TAddress.Create;
  try
    a.City := 'Milan';
    a.Store();
    old_id := a.id;
  finally
    FreeAndNil(a);
  end;
  AssertNull(a);
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalStoreObject ' + old_id);
  brok.MockManager.Verify;
  FConn.CommitTransaction;
  brok.MockManager.StartSetUp;
  a := TAddress.Retrieve(old_id);
  try
    AssertEquals(old_id, a.Id);
  finally
    FreeAndNil(a);
  end;
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalRetrieveObject ' + old_id);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
end;

procedure TTestMockBroker.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

{ TTestMockRelationalBroker }

procedure TTestMockRelationalBroker.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockCRBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
end;

procedure TTestMockRelationalBroker.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TTestMockRelationalBroker.TestGetBroker;
var
  brok: TInstantMockCRBroker;
begin
  brok := (FConn.Broker as TInstantMockCRBroker);
  AssertNotNull(brok);
  AssertEquals(brok.ClassType, TInstantMockCRBroker);
  brok.MockManager.StartSetUp;
  brok.MockManager.EndSetUp;
  Fconn.BuildDatabase(InstantModel);
  brok.MockManager.Verify;
end;

procedure TTestMockRelationalBroker.TestParts;
var
  c: TContact;
  t: TPhone;
begin
  FConn.IsDefault := True;
  c := TContact.Create;
  try
    AssertNotNull(c._Phones);
    AssertEquals(0, c.PhoneCount);
    t := TPhone.Create;
    t.Name := 'Home';
    t.Number := '012 12345678';
    c.AddPhone(t);
    AssertEquals(1, c.PhoneCount);
  finally
    c.Free;
  end;
end;

procedure TTestMockRelationalBroker.TestStoreAndRetrieveContact;
var
  c: TContact;
  old_id: string;
  brok: TInstantMockCRBroker;
  t: TPhone;
begin
  FConn.IsDefault := True;
  brok := FConn.Broker as TInstantMockCRBroker;
  brok.MockManager.StartSetUp;
  c := TContact.Create;
  try
    c.Name := 'Mike';
    c.Address.City := 'Milan';
    t := TPhone.Create;
    t.Name := 'Home';
    t.Number := '012 12345678';
    c.AddPhone(t);
    AssertEquals(1, c.PhoneCount);
    t := TPhone.Create;
    t.Name := 'Office';
    t.Number := '012 23456781';
    c.AddPhone(t);
    AssertEquals(2, c.PhoneCount);
    c.Store();
    old_id := c.id;
  finally
    FreeAndNil(c);
  end;
  AssertNull(c);
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalStoreObject caFail ' + old_id);
  brok.MockManager.Verify;
  brok.MockManager.StartSetUp;
  brok.MockManager.AddExpectation('InternalRetrieveObject caFail ' + old_id);
  brok.MockManager.EndSetUp;
  c := TContact.Retrieve(old_id);
  try
    AssertEquals(old_id, c.Id);
    AssertNotNull(c.Address);
    AssertEquals(0, c.PhoneCount); //mock brocker cannot collect part and parts
  finally
    FreeAndNil(c);
  end;
  brok.MockManager.Verify;
end;

initialization
{$IFNDEF CURR_TESTS}
  RegisterTests([TTestMockBroker, TTestMockRelationalBroker]);
{$ENDIF}
end.