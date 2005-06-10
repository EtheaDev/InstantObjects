unit TestMockConnector;

interface

uses
  Classes, SysUtils,
  InstantPersistence,
  fpcunit,
  testregistry,
  InstantMock;

type
  TTestMockConnector = class(TTestCase)
  private
    FConn: TInstantMockConnector;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBuildDatabase;
    procedure TestConnectDisconnect;
    procedure TestTransaction;
    procedure TestDefault;
  end;

implementation

procedure TTestMockConnector.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
end;

procedure TTestMockConnector.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

{ TTestMockConnector }

procedure TTestMockConnector.TestBuildDatabase;
begin
  AssertNotNull(FConn);
  FConn.MockManager.AddExpectation('InternalConnect');
  FConn.MockManager.AddExpectation('InternalCreateScheme');
  FConn.MockManager.AddExpectation('CreateBroker TInstantMockBroker');
  FConn.MockManager.AddExpectation('InternalDisconnect');
  FConn.MockManager.EndSetUp;
  AssertEquals(4, FConn.MockManager.UncoveredExpectations);
  FConn.BrokerClass := TInstantMockBroker;
  FConn.BuildDatabase(InstantModel);
  FConn.MockManager.Verify;
end;

procedure TTestMockConnector.TestConnectDisconnect;
begin
  FConn.BuildDatabase(InstantModel);
  FConn.MockManager.StartSetUp;
  FConn.MockManager.AddExpectation('InternalConnect');
  FConn.MockManager.AddExpectation('InternalDisconnect');
  FConn.MockManager.EndSetUp;
  FConn.Connect;
  FConn.Disconnect;
  FConn.MockManager.Verify;
end;

procedure TTestMockConnector.TestTransaction;
begin
  FConn.BrokerClass := TInstantMockBroker;
  FConn.BuildDatabase(InstantModel);
  FConn.Connect;
  FConn.MockManager.StartSetUp; //reset expectations
  FConn.MockManager.AddExpectation('InternalStartTransaction');
  FConn.MockManager.AddExpectation('InternalCommitTransaction');
  FConn.MockManager.EndSetUp;
  FConn.StartTransaction;
  AssertTrue(FConn.InTransaction);
  FConn.CommitTransaction;
  AssertFalse(FConn.InTransaction);
  FConn.MockManager.Verify;
  FConn.Disconnect;
end;

procedure TTestMockConnector.TestDefault;
begin
  FConn.IsDefault := True;
  AssertSame(InstantDefaultConnector, FConn);
end;

initialization
{$IFNDEF CURR_TESTS}
  RegisterTests([TTestMockConnector]);
{$ENDIF}
end.
