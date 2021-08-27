(*
 *   InstantObjects Test Suite
 *   TestMockConnector
 *)

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is: InstantObjects Test Suite/TestMockConnector
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestMockConnector;

interface

uses
  Classes, SysUtils,
  InstantPersistence,
  {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF}
  InstantMock,
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestMockConnector = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
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
  FConn.MockManager.StartSetUp;
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
  FConn.IsDefault := False;
end;

initialization
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TTestMockConnector]);
{$ENDIF}
end.
