unit TestMockConnector;

interface

uses
  Classes, SysUtils,
  InstantPersistence,
  fpcunit,
  testregistry,
  InstantMock,
  UbMockObject;

type
  TTestMockConnector = class(TTestCase)
  published
    procedure TestBuildDatabase;
    procedure TestConnectDisconnect;
    procedure TestTransaction;
    procedure TestDefault;
  end;

implementation

{ TTestMockConnector }

procedure TTestMockConnector.TestBuildDatabase;
var
  conn: TInstantMockConnector;
begin
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0),'.mdx'));
  conn := TInstantMockConnector.Create(nil);
  try
    AssertNotNull(conn);
    conn.MockManager.AddExpectation('InternalConnect');
    conn.MockManager.AddExpectation('InternalCreateScheme');
    conn.MockManager.AddExpectation('CreateBroker TInstantMockBroker');
    conn.MockManager.AddExpectation('InternalDisconnect');
    conn.MockManager.EndSetUp;
    AssertEquals(4, conn.MockManager.UncoveredExpectations);
    conn.BrokerClass := TInstantMockBroker;
    conn.BuildDatabase(InstantModel);
    conn.MockManager.Verify;
  finally
    conn.Free;
  end;
end;

procedure TTestMockConnector.TestConnectDisconnect;
var
  conn: TInstantMockConnector;
begin
  conn := TInstantMockConnector.Create(nil);
  try
    conn.BrokerClass := TInstantMockBroker;
    conn.BuildDatabase(InstantModel);
    conn.MockManager.StartSetUp;
    conn.MockManager.AddExpectation('InternalConnect');
    conn.MockManager.AddExpectation('InternalDisconnect');
    conn.MockManager.EndSetUp;
    conn.Connect;
    conn.Disconnect;
    conn.MockManager.Verify;
  finally
    conn.Free;
  end;
end;

procedure TTestMockConnector.TestTransaction;
var
  conn: TInstantMockConnector;
begin
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0),'.mdx'));
  conn := TInstantMockConnector.Create(nil);
  try
    conn.BrokerClass := TInstantMockBroker;
    conn.BuildDatabase(InstantModel);
    conn.Connect;
    conn.MockManager.StartSetUp; //reset expectations
    conn.MockManager.AddExpectation('InternalStartTransaction');
    conn.MockManager.AddExpectation('InternalCommitTransaction');
    conn.MockManager.EndSetUp;
    conn.StartTransaction;
    AssertTrue(conn.InTransaction);
    conn.CommitTransaction;
    AssertFalse(conn.InTransaction);
    conn.MockManager.Verify;
    conn.Disconnect;
  finally
    conn.Free;
  end;
end;

procedure TTestMockConnector.TestDefault;
var
  conn: TInstantMockConnector;
begin
  conn := TInstantMockConnector.Create(nil);
  try
    conn.IsDefault := True;
    AssertSame(InstantDefaultConnector, conn);
  finally
    conn.Free;
  end;
end;

initialization
  RegisterTests([TTestMockConnector]);

end.
