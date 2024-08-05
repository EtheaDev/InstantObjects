(*
 *   InstantObjects Test Suite
 *   TestInstantObjectReference
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
 * The Original Code is: InstantObjects Test Suite/TestInstantObjectReference
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

unit TestInstantObjectReference;

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantMock, InstantPersistence, TestModel,
  DUnitX.TestFramework;

type
  // Test methods for class TInstantObjectReference
  [TestFixture]
  TestTInstantObjectReference = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantObjectReference: TInstantObjectReference;
    FInstantParts: TInstantParts;
    FInstantReferences: TInstantReferences;
    FInstantObject: TCompany;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAssign;
    procedure TestAssignInstance_DestroyInstance;
    procedure TestDereference;
    procedure TestEquals;
    procedure TestEquals_Object;
    procedure TestHasInstance;
    procedure TestHasReference;
    procedure TestIsBroken;
    procedure TestReferenceObject;
    procedure TestReferenceObject_ClassType;
    procedure TestReset;
    procedure TestWrite_ReadAsObject;
  end;

implementation

uses Classes, SysUtils, TypInfo, InstantClasses;

procedure TestTInstantObjectReference.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FInstantObject := TCompany.Create(FConn);
  FInstantReferences := FInstantObject._Employees;
  FInstantParts := FInstantObject._ExternalPhones;
  FInstantObjectReference := TInstantObjectReference.Create;
end;

procedure TestTInstantObjectReference.TearDown;
begin
  FreeAndNil(FInstantObjectReference);
  FreeAndNil(FInstantObject);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantObjectReference.TestAssign;
var
  Source: TPersistent;
begin
  Source := TInstantObjectReference.Create(FInstantObject);
  try
    AssertEquals('ClassName', FInstantObject.ClassName,
        TInstantObjectReference(Source).ObjectClassName);
    AssertEquals('Id', FInstantObject.Id,
        TInstantObjectReference(Source).ObjectId);
    AssertSame('Instance', FInstantObject,
        TInstantObjectReference(Source).Instance);

    FInstantObjectReference.Assign(Source);
    AssertEquals('ClassName 2', FInstantObject.ClassName,
        FInstantObjectReference.ObjectClassName);
    AssertEquals('Id 2', FInstantObject.Id, FInstantObjectReference.ObjectId);
    AssertSame('Instance 2', FInstantObject, FInstantObjectReference.Instance);
  finally
    Source.Free;
  end;
end;

procedure TestTInstantObjectReference.TestAssignInstance_DestroyInstance;
var
  RefCnt: Integer;
begin
  AssertNotNull(FInstantObject);
  RefCnt := FInstantObject.RefCount;
  AssertFalse('OwnsInstance', FInstantObjectReference.OwnsInstance);
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);
  
  FInstantObjectReference.AssignInstance(FInstantObject);
  AssertTrue('HasInstance 2', FInstantObjectReference.HasInstance);
  AssertEquals('RefCount', RefCnt, FInstantObject.RefCount);

  FInstantObjectReference.DestroyInstance;
  AssertEquals('RefCount 2', RefCnt, FInstantObject.RefCount);
  AssertFalse('HasInstance 3', FInstantObjectReference.HasInstance);

  FInstantObjectReference.OwnsInstance := True;
  FInstantObjectReference.AssignInstance(FInstantObject);
  AssertTrue('HasInstance 4', FInstantObjectReference.HasInstance);
  AssertEquals('RefCount 3', Succ(RefCnt), FInstantObject.RefCount);

  FInstantObjectReference.DestroyInstance;
  AssertEquals('RefCount 4', RefCnt, FInstantObject.RefCount);
  AssertFalse('HasInstance 5', FInstantObjectReference.HasInstance);
end;

procedure TestTInstantObjectReference.TestDereference;
var
  vObj: TInstantObject;
  RefCnt: Integer;
begin
  FInstantObject.Id := 'TestId';
  FInstantObject.Name := 'TestCo';
  FInstantObject.Store;
  RefCnt := FInstantObject.RefCount;

  FInstantObjectReference.ReferenceObject(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);
  AssertEquals('RefCount', RefCnt, FInstantObject.RefCount);

  // with ownership
  vObj := FInstantObjectReference.Dereference(FConn, True,
      False);
  AssertEquals('FInstantObject.Id <> vObj.Id', FInstantObject.Id, vObj.Id);
  AssertSame('FInstantObject <> vObj', FInstantObject, vObj);
  AssertEquals('RefCount 2', Succ(RefCnt), FInstantObject.RefCount);
  FInstantObjectReference.DestroyInstance;
  AssertEquals('RefCount 3', RefCnt, FInstantObject.RefCount);

  // without ownership
  vObj := FInstantObjectReference.Dereference(FConn, False,
      False);
  AssertEquals('FInstantObject.Id <> vObj.Id', FInstantObject.Id, vObj.Id);
  AssertSame('FInstantObject <> vObj', FInstantObject, vObj);
  AssertEquals('RefCount 4', RefCnt, FInstantObject.RefCount);
end;

procedure TestTInstantObjectReference.TestEquals;
var
  ReturnValue: Boolean;
begin
  FInstantObjectReference.ReferenceObject(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);

  ReturnValue := FInstantObjectReference.Equals(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('ReturnValue', ReturnValue);

  FInstantObject.Id := 'TestId';
  ReturnValue := FInstantObjectReference.Equals(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertFalse('ReturnValue 2', ReturnValue);
end;

procedure TestTInstantObjectReference.TestEquals_Object;
var
  ReturnValue: Boolean;
begin
  // FInstantObject is not persistent
  FInstantObjectReference.ReferenceObject(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);

  ReturnValue := FInstantObjectReference.Equals(FInstantObject);
  AssertFalse('ReturnValue', ReturnValue);

  FInstantObject.Id := 'TestId';
  FInstantObject.Name := 'TestCo';
  FInstantObject.Store;

  // FInstantObject is persistent
  FInstantObjectReference.ReferenceObject(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);

  ReturnValue := FInstantObjectReference.Equals(FInstantObject);
  AssertTrue('ReturnValue 2', ReturnValue);

  FInstantObject.Id := 'TestId_changed';
  FInstantObject.Store;

  ReturnValue := FInstantObjectReference.Equals(FInstantObject);
  AssertFalse('ReturnValue 3', ReturnValue);

  FreeAndNil(FInstantObject);

  ReturnValue := FInstantObjectReference.Equals(FInstantObject);
  AssertFalse('ReturnValue 4', ReturnValue);

  FInstantObjectReference.Reset;

  ReturnValue := FInstantObjectReference.Equals(FInstantObject);
  AssertTrue('ReturnValue 5', ReturnValue);
end;

procedure TestTInstantObjectReference.TestHasInstance;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FInstantObjectReference.HasInstance;
  AssertFalse('ReturnValue', ReturnValue);

  FInstantObjectReference.AssignInstance(FInstantObject);
  ReturnValue := FInstantObjectReference.HasInstance;
  AssertTrue('ReturnValue 2', ReturnValue);
end;

procedure TestTInstantObjectReference.TestHasReference;
var
  ReturnValue: Boolean;
begin
  ReturnValue := FInstantObjectReference.HasReference;
  AssertFalse('ReturnValue', ReturnValue);

  FInstantObjectReference.ReferenceObject(
      FInstantObject.ClassName, FInstantObject.Id);
  ReturnValue := FInstantObjectReference.HasReference;
  AssertTrue('ReturnValue 2', ReturnValue);
end;

procedure TestTInstantObjectReference.TestIsBroken;
var
  ReturnValue: Boolean;
begin
  FInstantObjectReference.Dereference(FConn, False,
      False);
  ReturnValue := FInstantObjectReference.IsBroken;
  AssertTrue('ReturnValue', ReturnValue);

  FInstantObjectReference.AssignInstance(FInstantObject);
  ReturnValue := FInstantObjectReference.IsBroken;
  AssertFalse('ReturnValue 2', ReturnValue);

  FInstantObjectReference.Reset;
  ReturnValue := FInstantObjectReference.IsBroken;
  AssertFalse('ReturnValue 3', ReturnValue);
end;

procedure TestTInstantObjectReference.TestWrite_ReadAsObject;
var
  vStream: TStream;
  vReader: TInstantReader;
  vWriter: TInstantWriter;
begin
  FInstantObject.Id := 'TestId';
  FInstantObjectReference.AssignInstance(FInstantObject);
  AssertTrue('HasInstance', FInstantObjectReference.HasInstance);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);

  vStream := TMemoryStream.Create;
  try
    AssertEquals(0, vStream.Size);
    vWriter := TInstantWriter.Create(vStream);
    try
      FInstantObjectReference.WriteAsObject(vWriter);
    finally
      vWriter.Free;
    end;
    AssertTrue('vStream.Size', vStream.Size > 0);

    FInstantObjectReference.Reset;
    AssertFalse('HasInstance 2', FInstantObjectReference.HasInstance);
    AssertFalse('HasReference 2', FInstantObjectReference.HasReference);

    vStream.Position := 0;
    vReader := TInstantReader.Create(vStream);
    try
      FInstantObjectReference.ReadAsObject(vReader);
      AssertFalse('HasInstance 3', FInstantObjectReference.HasInstance);
      AssertTrue('HasReference 3', FInstantObjectReference.HasReference);
      AssertEquals('ObjectClassName', 'TCompany',
          FInstantObjectReference.ObjectClassName);
      AssertEquals('ObjectId', 'TestId', FInstantObjectReference.ObjectId);
    finally
      vReader.Free;
    end;
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantObjectReference.TestReferenceObject;
var
  RefCnt: Integer;
  ReturnValue: Boolean;
begin
  FInstantObject.Id := 'TestId';
  RefCnt := FInstantObject.RefCount;

  FInstantObjectReference.ReferenceObject(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);
  AssertEquals('RefCount', RefCnt, FInstantObject.RefCount);

  ReturnValue := FInstantObjectReference.Equals(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('ReturnValue', ReturnValue);
end;

procedure TestTInstantObjectReference.TestReferenceObject_ClassType;
var
  RefCnt: Integer;
  ReturnValue: Boolean;
begin
  FInstantObject.Id := 'TestId';
  RefCnt := FInstantObject.RefCount;

  FInstantObjectReference.ReferenceObject(
      FInstantObject.ClassType, FInstantObject.Id);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);
  AssertEquals('RefCount', RefCnt, FInstantObject.RefCount);

  ReturnValue := FInstantObjectReference.Equals(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('ReturnValue', ReturnValue);
end;

procedure TestTInstantObjectReference.TestReset;
begin
  FInstantObject.Id := 'TestId';
  FInstantObjectReference.AssignInstance(FInstantObject);
  AssertTrue('HasInstance', FInstantObjectReference.HasInstance);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);

  FInstantObjectReference.Reset;
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);
  AssertFalse('HasReference', FInstantObjectReference.HasReference);

  FInstantObjectReference.ReferenceObject(
      FInstantObject.ClassName, FInstantObject.Id);
  AssertTrue('HasReference', FInstantObjectReference.HasReference);
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);

  FInstantObjectReference.Reset;
  AssertFalse('HasInstance', FInstantObjectReference.HasInstance);
  AssertFalse('HasReference', FInstantObjectReference.HasReference);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantObjectReference]);
{$ENDIF}

end.
