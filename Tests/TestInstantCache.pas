(*
 *   InstantObjects Test Suite
 *   TestInstantCache
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
 * The Original Code is: InstantObjects Test Suite/TestInstantCache
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

unit TestInstantCache;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantCache.
  // TInstantCacheNode and TInstantCacheEnumerator
  // are not tested seperately as they are only used
  // internally by TInstantCache.
  [TestFixture]
  TestTInstantCache = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantCache: TInstantCache;
    procedure InitSalary(aPerson: TInstantObject);
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAdd_Remove;
    procedure TestFind_Clear;
    procedure TestForEach;
  end;

implementation

uses
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, InstantClasses;

procedure TestTInstantCache.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;

  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  FInstantCache := TInstantCache.Create;
end;

procedure TestTInstantCache.TearDown;
begin
  FreeAndNil(FInstantCache);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantCache.TestAdd_Remove;
var
  vPerson: TPerson;
begin
  AssertEquals(0, FInstantCache.Count);

  vPerson := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson);
    vPerson.Name := 'AName';
    vPerson.Store;
    FInstantCache.Add(vPerson);
    AssertEquals(1, FInstantCache.Count);
    AssertEquals('AName', TPerson(FInstantCache.Items[0]).Name);

    // Should not allow duplicate objects in cache
    FInstantCache.Add(vPerson);
    AssertEquals(1, FInstantCache.Count);

    FInstantCache.Remove(vPerson);
    AssertEquals(0, FInstantCache.Count);
  finally
    vPerson.Free;
  end;
end;

procedure TestTInstantCache.TestFind_Clear;
var
  vPerId: string;
  vPerson1: TPerson;
  vPerson2: TPerson;
  vPerson3: TPerson;
  vReturnValue: TPerson;
begin
  vPerson2 := nil;
  vPerson3 := nil;
  AssertEquals(0, FInstantCache.Count);

  vPerson1 := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson1);
    vPerson1.Name := 'AName_1';
    vPerson1.Store;
    FInstantCache.Add(vPerson1);
    AssertEquals(1, FInstantCache.Count);

    vPerson2 := TPerson.Create(FConn);
    AssertNotNull(vPerson2);
    vPerson2.Name := 'AName_2';
    vPerson2.Store;
    vPerId := vPerson2.PersistentId;
    FInstantCache.Add(vPerson2);
    AssertEquals(2, FInstantCache.Count);


    vPerson3 := TPerson.Create(FConn);
    AssertNotNull(vPerson3);
    vPerson3.Name := 'AName_3';
    vPerson3.Store;
    FInstantCache.Add(vPerson3);
    AssertEquals(3, FInstantCache.Count);

    vReturnValue := TPerson(FInstantCache.Find(vPerId));
    AssertEquals('AName_2', vReturnValue.Name);

    FInstantCache.Clear;
    AssertEquals(0, FInstantCache.Count);
  finally
    vPerson1.Free;
    vPerson2.Free;
    vPerson3.Free;
  end;
end;

procedure TestTInstantCache.InitSalary(aPerson: TInstantObject);
begin
  with aPerson as TPerson do
    Salary := 55000;
end;

procedure TestTInstantCache.TestForEach;
var
  vPerson1: TPerson;
  vPerson2: TPerson;
  vPerson3: TPerson;
  i: Integer;
begin
  vPerson2 := nil;
  vPerson3 := nil;
  AssertEquals(0, FInstantCache.Count);

  vPerson1 := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson1);
    vPerson1.Name := 'AName_1';
    vPerson1.Store;
    FInstantCache.Add(vPerson1);
    AssertEquals(1, FInstantCache.Count);

    vPerson2 := TPerson.Create(FConn);
    AssertNotNull(vPerson2);
    vPerson2.Name := 'AName_2';
    vPerson2.Store;
    FInstantCache.Add(vPerson2);
    AssertEquals(2, FInstantCache.Count);


    vPerson3 := TPerson.Create(FConn);
    AssertNotNull(vPerson3);
    vPerson3.Name := 'AName_3';
    vPerson3.Store;
    FInstantCache.Add(vPerson3);
    AssertEquals(3, FInstantCache.Count);

    FInstantCache.ForEach(InitSalary);

    for i := 0 to Pred(FInstantCache.Count) do
      AssertEquals(55000, TPerson(FInstantCache.Items[i]).Salary);

    FInstantCache.Clear;
    AssertEquals(0, FInstantCache.Count);
  finally
    vPerson1.Free;
    vPerson2.Free;
    vPerson3.Free;
  end;
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantCache]);
{$ENDIF}

end.
 