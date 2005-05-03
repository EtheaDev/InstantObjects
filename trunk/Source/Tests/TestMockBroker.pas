unit TestMockBroker;

interface

uses
  Classes, SysUtils, InstantPersistence, fpcunit, testregistry, InstantMock,
  TestModel;

type
  TTestMockBroker = class(TTestCase)
  private
    FClassCount: Integer;
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
    FClassCount: Integer;
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
  if FClassCount > 0 then
  begin
    // This ensures that the exported file is synchronised
    // with the current model resource file.
    InstantModel.SaveToFile(ChangeFileExt(ParamStr(0), '.mdx'));
    InstantModel.ClassMetadatas.Clear;
  end;

  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0), '.mdx'));
  vReturnValue := InstantModel.ClassMetadatas.Find('TCategory');
  AssertNotNull(vReturnValue);
  if FClassCount = 0 then
    FClassCount := InstantModel.ClassMetadatas.Count;

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
  if FClassCount > 0 then
    InstantModel.ClassMetadatas.Clear;

  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
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
  AssertTrue(FClassCount > 0);

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
    FreeAndNil(a);
  end;
  AssertNull(a);
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalStoreObject ' + old_id);
  brok.MockManager.Verify;
  Fconn.CommitTransaction;
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
  inherited;
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if FClassCount > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  FClassCount := InstantModel.ClassMetadatas.Count;
end;

procedure TTestMockBroker.TearDown;
begin
  FConn.Free;
  inherited;
end;

{ TTestMockRelationalBroker }

procedure TTestMockRelationalBroker.SetUp;
begin
  inherited;
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockCRBroker;

  if FClassCount > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  FClassCount := InstantModel.ClassMetadatas.Count;
end;

procedure TTestMockRelationalBroker.TearDown;
begin
  inherited;
  FConn.Free;
end;

procedure TTestMockRelationalBroker.TestGetBroker;
var
  brok: TInstantMockCRBroker;
begin
  brok := (Fconn.Broker as TInstantMockCRBroker);
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
  AssertTrue(FClassCount > 0);

  Fconn.IsDefault := True;
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
  AssertTrue(FClassCount > 0);

  Fconn.IsDefault := True;
  brok := Fconn.Broker as TInstantMockCRBroker;
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