(*
 *   InstantObjects Test Suite
 *   TestInstantObject
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
 * The Original Code is: InstantObjects Test Suite/TestInstantObject
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

unit TestInstantObject;

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantObject
  [TestFixture]
  TestTInstantObject = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantObject: TPerson;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestObjectsArrayMethods;
    procedure TestAddRef_Release;
    procedure TestAssign_Clone_ResetAttributes;
    procedure TestAttributeByName;
    procedure TestCanDispose;
    procedure TestCanStore;
    procedure TestChanged_Unchanged_IsChanged;
    procedure TestChangesDisabled_EnableDisableChanges;
    procedure TestCheckId;
    procedure TestClassType;
    procedure TestContainerByName;
    procedure TestEqualsSignature;
    procedure TestEqualsPersistentSignature;
    procedure TestFindAttribute;
    procedure TestFindContainer;
    procedure TestFreeInstance;
    procedure TestGetNamePath;
    procedure TestHasDefaultContainer;
    procedure TestIsAbandoned;
    procedure TestIsDefault;
    procedure TestIsIdChanged;
    // ToDo: procedure TestInvoke;
    procedure TestIsOperationAllowed;
    procedure TestIsOwned;
    procedure TestIsPersistent;
    procedure TestMetadata;
    procedure TestNewInstance;
    procedure TestObjectChangeCount;
    procedure TestObjectClass;
    procedure TestOwner;
    procedure TestOwnerAttribute;
    procedure TestPersistentId;
    procedure TestRefresh_RefreshAll;
    procedure TestStore_Dispose;
    procedure TestUpdateCount;
  end;

implementation

uses SysUtils, Classes, Db, InstantClasses,
  InstantMetadata, InstantTypes;

procedure TestTInstantObject.SetUp;
var
  vInstantClassMetadata: TInstantClassMetadata;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;

  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  vInstantClassMetadata := InstantModel.ClassMetadatas.Find('TPerson');
  vInstantClassMetadata.DefaultContainerName := 'Emails';
  FInstantObject := TPerson.Create(FConn);
  FInstantObject.Name := 'InitPerson';
end;

procedure TestTInstantObject.TearDown;
begin
  FreeAndNil(FInstantObject);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantObject.TestObjectsArrayMethods;
var
  vReturnValue: Integer;
  vObject: TEmail;
begin
  vObject := TEmail.Create(FConn);
  AssertNotNull(vObject);
  vObject.Address := 'Object1';
  vReturnValue := FInstantObject.AddObject(vObject);
  AssertEquals('vReturnValue', 0, vReturnValue);
  AssertEquals('ObjectCount 1', 1, FInstantObject.ObjectCount);

  vObject := TEmail.Create(FConn);
  AssertNotNull(vObject);
  vObject.Address := 'Object2';
  vReturnValue := FInstantObject.AddObject(vObject);
  AssertEquals('vReturnValue', 1, vReturnValue);
  AssertEquals('ObjectCount 2', 2, FInstantObject.ObjectCount);

  AssertEquals('Object Index', 1, FInstantObject.IndexOfObject(vObject));

  vObject := TEmail.Create(FConn);
  AssertNotNull(vObject);
  vObject.Address := 'Object3';
  vReturnValue := FInstantObject.AddObject(vObject);
  AssertEquals('vReturnValue', 2, vReturnValue);
  AssertEquals('ObjectCount 3', 3, FInstantObject.ObjectCount);

  vObject := TEmail.Create(FConn);
  AssertNotNull(vObject);
  vObject.Address := 'Object4';
  FInstantObject.InsertObject(2, vObject);
  AssertEquals('ObjectCount 4', 4, FInstantObject.ObjectCount);

  FInstantObject.DeleteObject(1);
  AssertEquals('ObjectCount 5', 3, FInstantObject.ObjectCount);
  AssertEquals('Address 1', 'Object1',
    TEmail(FInstantObject.Objects[0]).Address);
  AssertEquals('Address 2', 'Object4',
    TEmail(FInstantObject.Objects[1]).Address);
  AssertEquals('Address 3', 'Object3',
    TEmail(FInstantObject.Objects[2]).Address);

  FInstantObject.ClearObjects;
  AssertEquals('ObjectCount 6', 0, FInstantObject.ObjectCount);
end;

procedure TestTInstantObject.TestAddRef_Release;
var
  vReturnValue: Integer;
begin
  AssertEquals('RefCount 1', 1, FInstantObject.RefCount);
  vReturnValue := FInstantObject.AddRef;
  AssertEquals('RefCount 2', 2, vReturnValue);
  vReturnValue := FInstantObject.Release;
  AssertEquals('RefCount 3', 1, vReturnValue);
end;

procedure TestTInstantObject.TestAssign_Clone_ResetAttributes;
var
  vCompany: TCompany;
  vEmail: TEmail;
  vSource, vClone: TPerson;
begin
  vCompany := nil;
  vClone := nil;

  vSource := TPerson.Create(FConn);
  try
    { NOTE: vSource.Category is not assigned here so that
      the leakage, etc. behaviour of an unassigned reference
      attribute can be monitored during assign and clone
      operations. }
    AssertNotNull(vSource);
    vSource.Name := 'NewPerson';
    vEmail := TEmail.Create(FConn);
    vEmail.Address := 'NewPerson@domain.com';
    vSource.AddEmail(vEmail);
    vCompany := TCompany.Create(FConn);
    vCompany.Name := 'Employer Co.';
    vSource.EmployBy(vCompany);

    AssertEquals('InitPerson', FInstantObject.Name);
    FInstantObject.Assign(vSource);
    vSource._Employer.DestroyObject;
    AssertEquals('NewPerson', FInstantObject.Name);
    AssertEquals(1, FInstantObject.EmailCount);
    AssertEquals('NewPerson@domain.com', FInstantObject.Emails[0].Address);
    AssertEquals('Employer Co.', FInstantObject.Employer.Name);

    vClone := TPerson.Clone(FInstantObject, FConn);
    AssertNotNull(vClone);
    AssertNotSame(vClone, FInstantObject);
    AssertEquals('NewPerson', vClone.Name);
    AssertEquals(1, vClone.EmailCount);
    AssertEquals('NewPerson@domain.com', vClone.Emails[0].Address);
    AssertEquals('Employer Co.', vClone.Employer.Name);

    FInstantObject.ResetAttributes;
    AssertEquals('', FInstantObject.Name);
    AssertEquals(0, FInstantObject.EmailCount);
    AssertNull(FInstantObject.Employer);
  finally
    vCompany.Free;
    vSource.Free;
    vClone.Free;
  end;
end;

procedure TestTInstantObject.TestAttributeByName;
var
  vReturnValue: TInstantAttribute;
begin
  vReturnValue := FInstantObject.AttributeByName('Name');
  AssertNotNull('Name', vReturnValue);
  AssertEquals('Name', 'InitPerson', vReturnValue.Value);
  vReturnValue := FInstantObject.AttributeByName('Employer');
  AssertNotNull('Employer', vReturnValue);
  AssertEquals('Employer', 'Employer', vReturnValue.Name);

  try
    FInstantObject.AttributeByName('Dummy');
    Fail('Should never get here!!');
  except
    on E: EInstantError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantObject.TestCanDispose;
begin
  AssertTrue(FInstantObject.CanDispose);
end;

procedure TestTInstantObject.TestCanStore;
begin
  AssertTrue(FInstantObject.CanStore);
end;

procedure TestTInstantObject.TestChanged_Unchanged_IsChanged;
begin
  AssertTrue(FInstantObject.IsChanged);
  FInstantObject.Unchanged;
  AssertFalse(FInstantObject.IsChanged);
end;

procedure TestTInstantObject.TestChangesDisabled_EnableDisableChanges;
begin
  AssertFalse(FInstantObject.ChangesDisabled);
  FInstantObject.DisableChanges;
  AssertTrue(FInstantObject.ChangesDisabled);
  FInstantObject.DisableChanges;
  AssertTrue(FInstantObject.ChangesDisabled);
  FInstantObject.EnableChanges;
  AssertTrue(FInstantObject.ChangesDisabled);
  FInstantObject.EnableChanges;
  AssertFalse(FInstantObject.ChangesDisabled);
end;

procedure TestTInstantObject.TestCheckId;
begin
  FInstantObject.CheckId;
  AssertFalse(FInstantObject.Id = '');
  FInstantObject.Id := 'PersonId';
  FInstantObject.CheckId;
  AssertEquals('PersonId', FInstantObject.Id);
end;

procedure TestTInstantObject.TestClassType;
begin
  Assert.AreEqual(TPerson, FInstantObject.ClassType);
end;

procedure TestTInstantObject.TestContainerByName;
var
  vReturnValue: TInstantContainer;
begin
  // Get DefaultContainer name
  vReturnValue := FInstantObject.ContainerByName('');
  AssertNotNull(vReturnValue);
  AssertEquals('Emails', vReturnValue.Name);

  vReturnValue := FInstantObject.ContainerByName('Emails');
  AssertNotNull(vReturnValue);

  try
    FInstantObject.ContainerByName('Dummy');
    Fail('Should never get here!!');
  except
    on E: EInstantError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantObject.TestEqualsPersistentSignature;
var
  vReturnValue: Boolean;
  vObjectId: string;
  vObjectClassName: string;
begin
  vObjectId := 'PersonID';
  vObjectClassName := 'TPerson';
  FInstantObject.Id := 'PersonID';
  AssertFalse('IsPersistent', FInstantObject.IsPersistent);
  vReturnValue :=
    FInstantObject.EqualsPersistentSignature(vObjectClassName, vObjectId);
  AssertFalse('EqualsPersistentSignature', vReturnValue);

  FInstantObject.Store;
  vReturnValue :=
    FInstantObject.EqualsPersistentSignature(vObjectClassName, vObjectId);
  AssertTrue('EqualsPersistentSignature', vReturnValue);
end;

procedure TestTInstantObject.TestEqualsSignature;
var
  vReturnValue: Boolean;
  vObjectId: string;
  vObjectClassName: string;
begin
  vObjectId := 'PersonID';
  vObjectClassName := 'TPerson';
  FInstantObject.Id := 'PersonID';
  vReturnValue := FInstantObject.EqualsSignature(vObjectClassName, vObjectId);
  Assert.IsTrue(vReturnValue, 'EqualsSignature');
end;

procedure TestTInstantObject.TestFindAttribute;
var
  vReturnValue: TInstantAttribute;
begin
  vReturnValue := FInstantObject.FindAttribute('Name');
  AssertNotNull('Name', vReturnValue);
  AssertEquals('InitPerson', vReturnValue.Value);
  vReturnValue := FInstantObject.FindAttribute('Employer');
  AssertNotNull('Employer', vReturnValue);
  AssertEquals('Employer', 'Employer', vReturnValue.Name);
end;

procedure TestTInstantObject.TestFindContainer;
var
  vReturnValue: TInstantContainer;
begin
  // Get DefaultContainer name
  vReturnValue := FInstantObject.FindContainer('');
  AssertNotNull(vReturnValue);
  AssertEquals('Emails', vReturnValue.Name);

  vReturnValue := FInstantObject.FindContainer('Emails');
  AssertNotNull(vReturnValue);

  vReturnValue := FInstantObject.FindContainer('Dummy');
  AssertNull(vReturnValue);
end;

procedure TestTInstantObject.TestFreeInstance;
begin
  FInstantObject.FreeInstance;
  FInstantObject := nil;
  Assert.IsNull(FInstantObject);
end;

procedure TestTInstantObject.TestGetNamePath;
begin
  FInstantObject.Id := 'PersonID';
  AssertEquals('TPerson.PersonID', FInstantObject.GetNamePath);
end;

procedure TestTInstantObject.TestHasDefaultContainer;
begin
  AssertTrue(FInstantObject.HasDefaultContainer);
end;

procedure TestTInstantObject.TestIsAbandoned;
begin
  AssertFalse(FInstantObject.IsAbandoned);
end;

procedure TestTInstantObject.TestIsDefault;
begin
  AssertFalse(FInstantObject.IsDefault);
  FInstantObject.ResetAttributes;
  FInstantObject.Id := '';
  AssertTrue('After ResetAttributes', FInstantObject.IsDefault);
end;

procedure TestTInstantObject.TestIsIdChanged;
begin
  AssertTrue('Initial', FInstantObject.IsIdChanged);
  FInstantObject.Store;
  AssertFalse('After store', FInstantObject.IsIdChanged);
  FInstantObject.Id := 'PersonId';
  AssertTrue('After change ID', FInstantObject.IsIdChanged);
  FInstantObject.Store;
  AssertFalse('After changed store', FInstantObject.IsIdChanged);
end;

procedure TestTInstantObject.TestIsOperationAllowed;
var
  vOT: TInstantOperationType;
begin
  for vOT := Low(TInstantOperationType) to High(TInstantOperationType) do
    AssertTrue(FInstantObject.IsOperationAllowed(vOT));
end;

procedure TestTInstantObject.TestIsOwned;
begin
  AssertFalse('TPerson', FInstantObject.IsOwned);
end;

procedure TestTInstantObject.TestIsPersistent;
begin
  AssertFalse(FInstantObject.IsPersistent);
end;

procedure TestTInstantObject.TestMetadata;
var
  vReturnValue: TInstantClassMetadata;
begin
  vReturnValue := FInstantObject.Metadata;
  AssertNotNull(vReturnValue);
  AssertEquals('TPerson', FInstantObject.Metadata.Name);
end;

procedure TestTInstantObject.TestNewInstance;
var
  vReturnValue: TObject;
begin
  vReturnValue := FInstantObject.NewInstance;
  try
    AssertNotNull(vReturnValue);
    AssertEquals('InstanceSize', TPerson.InstanceSize, vReturnValue.InstanceSize);
  finally
    vReturnValue.FreeInstance;
  end;
end;

procedure TestTInstantObject.TestObjectChangeCount;
var
  vReturnValue: Integer;
  vObject: TEmail;
begin
  AssertEquals(0, FInstantObject.ObjectChangeCount);

  vObject := TEmail.Create(FConn);
  AssertNotNull(vObject);
  vObject.Address := 'Object1';
  vReturnValue := FInstantObject.AddObject(vObject);
  AssertEquals('vReturnValue', 0, vReturnValue);
  AssertEquals(1, FInstantObject.ObjectChangeCount);
end;

procedure TestTInstantObject.TestObjectClass;
begin
  Assert.AreEqual(TEmail, FInstantObject.ObjectClass);
end;

procedure TestTInstantObject.TestOwner;
begin
  AssertTrue(FInstantObject.Owner = nil);
end;

procedure TestTInstantObject.TestOwnerAttribute;
begin
  AssertTrue(FInstantObject.OwnerAttribute = nil);
end;

procedure TestTInstantObject.TestPersistentId;
begin
  AssertEquals('', FInstantObject.PersistentId);
  FInstantObject.Store;
  AssertTrue(FInstantObject.Id <> '');
end;

procedure TestTInstantObject.TestRefresh_RefreshAll;
var
  vID: String;
  brok: TInstantMockBroker;
begin
  FConn.StartTransaction;
  brok := FConn.Broker as TInstantMockBroker;
  brok.MockManager.EndSetUp;
  FInstantObject.Store;
  vID := FInstantObject.Id;
  brok.MockManager.StartSetUp;
  brok.MockManager.AddExpectation('InternalStoreObject ' + vID);
  brok.MockManager.Verify;
  FConn.CommitTransaction;

  brok.MockManager.EndSetUp;
  FInstantObject.Refresh;
  brok.MockManager.StartSetUp;
  brok.MockManager.AddExpectation('InternalRetrieveObject ' + vID);
  brok.MockManager.Verify;

  brok.MockManager.EndSetUp;
  FInstantObject.RefreshAll(FConn, nil);
  brok.MockManager.StartSetUp;
  brok.MockManager.AddExpectation('InternalRetrieveObject ' + vID);
  brok.MockManager.Verify;
end;

procedure TestTInstantObject.TestStore_Dispose;
var
  vPhone: TPhone;
  vExternalAddress: TExternalAddress;
  vContact: TContact;
  vID, vID1: String;
  brok: TInstantMockBroker;
  vRet: Integer;
begin
  AssertFalse(FInstantObject.IsPersistent);
  FInstantObject.Store;
  AssertTrue(FInstantObject.IsPersistent);

  FInstantObject.Dispose;
  AssertFalse(FInstantObject.IsPersistent);

  vContact := TContact.Create(FConn);
  vExternalAddress := TExternalAddress.Create(FConn);
  try
    FConn.StartTransaction;
    brok := FConn.Broker as TInstantMockBroker;
    brok.MockManager.EndSetUp;
    vContact.Name := 'MyContact';
    vExternalAddress.Name := 'Part External';
    AssertTrue(vExternalAddress.IsChanged);
    vExternalAddress.Store;
    vID1 := vExternalAddress.Id;
    vContact.ExternalAddress := vExternalAddress;
    AssertEquals('vExternalAddress', 1, vExternalAddress.RefCount);
    AssertEquals('vContact.Address', 1, vContact.Address.RefCount);

    vPhone := TPhone.Create(FConn);
    vPhone.Name := 'Home';
    vRet := vContact.AddPhone(vPhone);
    AssertEquals('vRet 1', 0, vRet);
    vPhone := TPhone.Create(FConn);
    vPhone.Name := 'Work';
    vRet := vContact.AddPhone(vPhone);
    AssertEquals('vRet 2', 1, vRet);

    vContact.Store;
    vPhone.Store;
    AssertTrue('vContact.IsPersistent', vContact.IsPersistent);
    AssertFalse('vPhone.IsPersistent', vPhone.IsPersistent);
    AssertTrue('vExternalAddress.IsPersistent', vExternalAddress.IsPersistent);
    vExternalAddress.Name := 'Changed';
    AssertTrue(vExternalAddress.IsChanged);
    AssertTrue(vContact.IsChanged);
    vID := vContact.Id;
    vContact.Store;
    brok.MockManager.StartSetUp;
    brok.MockManager.AddExpectation('InternalStoreObject ' + vID1);
    brok.MockManager.AddExpectation('InternalStoreObject ' + vID);
    brok.MockManager.AddExpectation('InternalStoreObject ' + vID);
    brok.MockManager.Verify;
    FConn.CommitTransaction;

    brok.MockManager.EndSetUp;
    vContact.Phones[0].Number := '1234567';
    AssertTrue(vContact.IsChanged);
    vContact.Store;
    vContact.Dispose;
    brok.MockManager.StartSetUp;
    brok.MockManager.AddExpectation('InternalStoreObject ' + vID);
    brok.MockManager.AddExpectation('InternalDisposeObject ' + vID);
    brok.MockManager.Verify;
  finally
    vContact.Free;
  end;
end;

procedure TestTInstantObject.TestUpdateCount;
begin
  AssertEquals(0, FInstantObject.UpdateCount);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantObject]);
{$ENDIF}

end.
 