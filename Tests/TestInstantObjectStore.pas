(*
 *   InstantObjects Test Suite
 *   TestInstantObjectStore
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
 * The Original Code is: InstantObjects Test Suite/TestInstantObjectStore
 *
 * The Initial Developer of the Original Code is: Steven Mitchell
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * 
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestInstantObjectStore;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantObjectStore
  [TestFixture]
  TestTInstantObjectStore = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantObjectStore: TInstantObjectStore;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestFind_AbandonObjects;
    procedure TestDispose_Refresh_StoreObject_ObjectDestroyed;
  end;

  // Test methods for class TInstantObjectStores
  [TestFixture]
  TestTInstantObjectStores = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantObjectStores: TInstantObjectStores;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAddObjectStore;
    procedure TestFindObjectStore;
  end;

implementation

uses
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, InstantClasses, InstantMetadata, InstantTypes;

procedure TestTInstantObjectStore.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockCRBroker;
  FConn.IsDefault := False;
  FConn.UseUnicode := TestModel.TestUseUnicode;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FInstantObjectStore := FConn.EnsureObjectStore(TPerson);
  AssertNotNull(FInstantObjectStore);
end;

procedure TestTInstantObjectStore.TearDown;
begin
  FInstantObjectStore := nil;
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantObjectStore.TestFind_AbandonObjects;
var
  vObjectId: string;
  vPerson1: TPerson;
  vPerson2: TPerson;
const
  NAME1 = 'AName1';
  NAME1_UNICODE = '网站导航1';
  NAME2 = 'AName2';
  NAME2_UNICODE = '网站导航2';
begin
  vPerson2 := nil;
  AssertEquals(0, FInstantObjectStore.Count);

  vPerson1 := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson1);
    if not FConn.UseUnicode then
      vPerson1.Name := NAME1
    else
      vPerson1.Name := NAME1_UNICODE;
    vPerson1.Store;
    vObjectId := vPerson1.PersistentId;
    AssertEquals(1, FInstantObjectStore.Count);

    vPerson2 := TPerson.Create(FConn);
    AssertNotNull(vPerson2);
    if not FConn.UseUnicode then
      vPerson2.Name := NAME2
    else
      vPerson2.Name := NAME2_UNICODE;
    vPerson2.Store;
    AssertEquals(2, FInstantObjectStore.Count);

    AssertSame(vPerson1, TPerson(FInstantObjectStore.Find(vObjectId)));
    if not FConn.UseUnicode then
      AssertEquals(NAME1, TPerson(FInstantObjectStore.Find(vObjectId)).Name)
    else
      AssertEquals(NAME1_UNICODE, TPerson(FInstantObjectStore.Find(vObjectId)).Name);

    AssertNotNull(vPerson1.Connector);
    AssertNotNull(vPerson2.Connector);
    FInstantObjectStore.AbandonObjects;
    AssertNull(vPerson1.Connector);
    AssertNull(vPerson2.Connector);

    // Remove abandoned objects from cache
    FInstantObjectStore.ObjectDestroyed(vPerson1);
    FInstantObjectStore.ObjectDestroyed(vPerson2);
  finally
    vPerson1.Free;
    vPerson2.Free;
  end;
end;

procedure TestTInstantObjectStore.TestDispose_Refresh_StoreObject_ObjectDestroyed;
var
  vObjectId: string;
  vPerson: TPerson;
  vBrock: TInstantMockCRBroker;
const
  NAME = 'AName';
  NAME_UNICODE = '网站导航';
begin
  AssertEquals(0, FInstantObjectStore.Count);
  vBrock := FConn.Broker as TInstantMockCRBroker;
  vBrock.MockManager.EndSetUp;

  vPerson := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson);
    if not FConn.UseUnicode then
      vPerson.Name := NAME
    else
      vPerson.Name := NAME_UNICODE;
    vPerson.Store;
    vObjectId := vPerson.PersistentId;
    vBrock.MockManager.StartSetUp;
    vBrock.MockManager.AddExpectation('InternalStoreObject caFail ' + vObjectId);
    vBrock.MockManager.Verify;
    AssertEquals(1, FInstantObjectStore.Count);

    vBrock.MockManager.EndSetUp;
    FInstantObjectStore.DisposeObject(vPerson, caFail);
    vBrock.MockManager.StartSetUp;
    vBrock.MockManager.AddExpectation('InternalDisposeObject caFail ' + vObjectId);
    vBrock.MockManager.Verify;
    AssertEquals(0, FInstantObjectStore.Count);
    if not FConn.UseUnicode then
      AssertEquals(NAME, vPerson.Name)
    else
      AssertEquals(NAME_UNICODE, vPerson.Name);
    vBrock.MockManager.EndSetUp;
    FInstantObjectStore.RefreshObject(vPerson);
    vBrock.MockManager.StartSetUp;
    vBrock.MockManager.AddExpectation('InternalRetrieveObject caFail ');
    vBrock.MockManager.Verify;

    vBrock.MockManager.EndSetUp;
    FInstantObjectStore.ObjectDestroyed(vPerson);
    vBrock.MockManager.StartSetUp;
    vBrock.MockManager.Verify;
    AssertEquals(0, FInstantObjectStore.Count);
    if not FConn.UseUnicode then
      AssertEquals(NAME, vPerson.Name)
    else
      AssertEquals(NAME_UNICODE, vPerson.Name);

    FInstantObjectStore.RefreshObject(vPerson);

    vBrock.MockManager.EndSetUp;
    FInstantObjectStore.StoreObject(vPerson, caFail);
    vBrock.MockManager.StartSetUp;
    vBrock.MockManager.AddExpectation('InternalStoreObject caFail ' + vObjectId);
    vBrock.MockManager.Verify;
    AssertEquals(1, FInstantObjectStore.Count);
    if not FConn.UseUnicode then
      AssertEquals(NAME, vPerson.Name)
    else
      AssertEquals(NAME_UNICODE, vPerson.Name);
  finally
    vPerson.Free;
  end;
end;

procedure TestTInstantObjectStores.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FInstantObjectStores := FConn.ObjectStores;
  FConn.EnsureObjectStore(TPerson);
end;

procedure TestTInstantObjectStores.TearDown;
begin
  FInstantObjectStores := nil;
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantObjectStores.TestAddObjectStore;
var
  vInitCnt: Integer;
  vReturnValue: TInstantObjectStore;
begin
  vInitCnt := FInstantObjectStores.Count;
  vReturnValue := FInstantObjectStores.AddObjectStore;
  AssertNotNull(vReturnValue);
  AssertEquals(vInitCnt + 1, FInstantObjectStores.Count);
end;

procedure TestTInstantObjectStores.TestFindObjectStore;
var
  vReturnValue: TInstantObjectStore;
begin
  vReturnValue := FInstantObjectStores.FindObjectStore(TPerson);
  AssertSame(TPerson, vReturnValue.ObjectClass);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantObjectStore,
                 TestTInstantObjectStores]);
{$ENDIF}

end.
