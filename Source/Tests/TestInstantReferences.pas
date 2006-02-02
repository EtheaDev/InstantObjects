(*
 *   InstantObjects Test Suite
 *   TestInstantReferences
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
 * The Original Code is: InstantObjects Test Suite/TestInstantReferences
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

unit TestInstantReferences;

interface

uses fpcunit, InstantMock, InstantPersistence, TestModel;

type

  // Use these tests in conjunction with a memory
  // leak test utility.
  TestTInstantReferences_Leak = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantReferences: TInstantReferences;
    FOwner: TCompany;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddEmbeddedObject;
    procedure TestAddExternalObject;

    // A -> <- B -> C
    procedure TestCircularReferences;
    // A -> B {Parts}-> C -> A
    procedure TestCircularReferences1;
    // A -> B {Parts}-> C {Parts}-> D -> A
    procedure TestCircularReferences2;
    // A -> <- B
    // |
    // +-> C
    // then delete C
    procedure TestCircularReferences3;
    // A->B->C->A
    //    |
    //    +->D
    // then delete D
    procedure TestCircularReferences4;
  end;

  TestTInstantEmbReferences = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantReferences: TInstantReferences;
    FOwner: TCompany;
    function RefsEmbeddedCompare(Holder, Obj1, Obj2: TInstantObject): Integer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAssign;
    procedure TestAttachObject;
    procedure TestClear;
    procedure TestDelete;
    procedure TestDestroyObject;
    procedure TestDetachObject;
    procedure TestExchange;
    procedure TestHasItem;
    procedure TestIndexOfInstance;
    procedure TestIndexOf_Insert;
    procedure TestLoadReferencesTo_FromStream;
    procedure TestMove;
    procedure TestRemove;
    procedure TestReset;
    procedure TestSaveObjectsTo_FromStream;
    procedure TestSort;
    procedure TestUnchanged;
  end;

  TestTInstantExtReferences = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantReferences: TInstantReferences;
    FOwner: TCompany;
    function RefsExternalCompare(Holder, Obj1, Obj2: TInstantObject): Integer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAssign;
    procedure TestAttachObject;
    procedure TestClear;
    procedure TestDelete;
    procedure TestDestroyObject;
    procedure TestDetachObject;
    procedure TestExchange;
    procedure TestHasItem;
    procedure TestIndexOfInstance;
    procedure TestIndexOf_Insert;
    procedure TestLoadReferencesTo_FromStream;
    procedure TestMove;
    procedure TestRemove;
    procedure TestReset;
    procedure TestSaveObjectsTo_FromStream;
    procedure TestSort;
    procedure TestUnchanged;
  end;

implementation

uses SysUtils, Classes, testregistry;

procedure TestTInstantReferences_Leak.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TCompany.Create(FConn);
end;

procedure TestTInstantReferences_Leak.TearDown;
begin
  FInstantReferences := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantReferences_Leak.TestAddEmbeddedObject;
var
  vReturnValue: Integer;
  vReference: TPerson;
begin
  FInstantReferences := FOwner._Employees;

  vReference := TPerson.Create(FConn);
  try
    vReturnValue := FInstantReferences.Add(vReference);
    AssertTrue(vReturnValue <> -1);
    AssertEquals('FInstantReferences.Count 1', 1, FInstantReferences.Count);
    AssertEquals('vReference.RefCount 1', 2, vReference.RefCount);

    vReturnValue := FInstantReferences.Remove(vReference);
    AssertTrue(vReturnValue <> -1);
    AssertEquals('FInstantReferences.Count 2', 0, FInstantReferences.Count);
    AssertEquals('vReference.RefCount 2', 1, vReference.RefCount);
  finally
    vReference.Free;
//    AssertException(EAccessViolation, vReference.Free);
  end;
end;

procedure TestTInstantReferences_Leak.TestAddExternalObject;
var
  vReturnValue: Integer;
  vReference: TProject;
begin
  FInstantReferences := FOwner._Projects;

  vReference := TProject.Create(FConn);
  try
    AssertEquals(1, vReference.RefCount);

    vReturnValue := FInstantReferences.Add(vReference);
    AssertTrue(vReturnValue <> -1);
    AssertEquals('FInstantReferences.Count 1', 1, FInstantReferences.Count);
    AssertEquals('vReference.RefCount 1', 2, vReference.RefCount);

    vReturnValue := FInstantReferences.Remove(vReference);
    AssertTrue(vReturnValue <> -1);
    AssertEquals('FInstantReferences.Count 2', 0, FInstantReferences.Count);
    AssertEquals('vReference.RefCount 2', 1, vReference.RefCount);
  finally
    vReference.Free;
//    AssertException(EAccessViolation, vReference.Free);
  end;
end;

// A -> <- B -> C
procedure TestTInstantReferences_Leak.TestCircularReferences;
var
  vPerson1: TPerson;
  vCategory: TCategory;
begin
  FOwner.Name := 'Owner';

  vPerson1 := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson1);
    vPerson1.Name := 'vPerson1';

    vPerson1.EmployBy(FOwner);
    AssertNotNull(vPerson1.Employer);
    AssertEquals('vPerson1.Employer.Name A', 'Owner', vPerson1.Employer.Name);
  finally
    vPerson1.Free;
  end;
  AssertEquals('FOwner.RefCount 1', 2, FOwner.RefCount);
  AssertEquals('FOwner.ReferencedBy.Count 1', 1, FOwner.ReferencedBy.Count);
  AssertEquals('FOwner.EmployeeCount 1', 1, FOwner.EmployeeCount);
  AssertEquals('FOwner.Employees[0].RefCount 1',
          1, FOwner.Employees[0].RefCount);
  AssertEquals('FOwner.Employees[0].ReferencedBy.Count 1',
          1, FOwner.Employees[0].ReferencedBy.Count);

  vCategory := TCategory.Create(FConn);
  try
    AssertNotNull(vCategory);
    vCategory.Name := 'vCategory';

    FOwner.Employees[0].Category := vCategory;
  finally
    vCategory.Free;
  end;
  AssertEquals('FOwner.RefCount 2', 2, FOwner.RefCount);
  AssertEquals('FOwner.ReferencedBy.Count 2', 1, FOwner.ReferencedBy.Count);

  AssertEquals('FOwner.Employees[0].RefCount 2',
          1, FOwner.Employees[0].RefCount);
  AssertEquals('FOwner.Employees[0].ReferencedBy.Count 2',
          1, FOwner.Employees[0].ReferencedBy.Count);

  AssertEquals('FOwner.Employees[0].Category.RefCount 1',
          1, FOwner.Employees[0].Category.RefCount);
  AssertEquals('FOwner.Employees[0].Category.ReferencedBy.Count 1',
          1, FOwner.Employees[0].Category.ReferencedBy.Count);
end;

// A -> B {Parts}-> C -> A
procedure TestTInstantReferences_Leak.TestCircularReferences1;
var
  vPerson: TPerson;
  vProject: TProject;
  vAddress: TExternalAddress;
begin
  vPerson := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson);
    vPerson.Name := 'vPerson1';

    vProject := TProject.Create(FConn);
    try
      AssertNotNull(vProject);
      vProject.Name := 'vProject1';
      vAddress := TExternalAddress.Create(FConn);
      try
        AssertNotNull(vAddress);
        vAddress.Site_Contact := vPerson;
        AssertEquals('vPerson1', vAddress.Site_Contact.Name);
        vProject.AddAddress(vAddress);
      except
        vAddress.Free;
      end;
      vPerson.AddProject(vProject);
    finally
      vProject.Free;
    end;
  AssertEquals('vPerson.RefCount 1', 2, vPerson.RefCount);
  AssertEquals('vPerson.ReferencedBy.Count 1', 1, vPerson.ReferencedBy.Count);
  AssertEquals('vPerson.Projects[0].RefCount', 1, vPerson.Projects[0].RefCount);
  AssertEquals('vPerson.Projects[0].ReferencedBy.Count',
          1, vPerson.Projects[0].ReferencedBy.Count);
  AssertEquals('vPerson.Projects[0].Addresses[0].RefCount',
          1, vPerson.Projects[0].Addresses[0].RefCount);
  AssertEquals('vPerson.Projects[0].Addresses[0].ReferencedBy.Count',
          0, vPerson.Projects[0].Addresses[0].ReferencedBy.Count);
  finally
    vPerson.Free;
  end;
//  AssertEquals('vPerson.RefCount 2', 1, vPerson.RefCount);
//  AssertEquals('vPerson.ReferencedBy.Count 2', 1, vPerson.ReferencedBy.Count);
end;

// A -> B {Parts}-> C {Parts}-> D -> A
procedure TestTInstantReferences_Leak.TestCircularReferences2;
var
  vPerson: TPerson;
  vProject: TProject;
  vSubProject: TProject;
  vAddress: TExternalAddress;
begin
  vPerson := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson);
    vPerson.Name := 'vPerson';

    vProject := TProject.Create(FConn);
    try
      AssertNotNull(vProject);
      vProject.Name := 'vProject';

      vSubProject := TProject.Create(FConn);
      try
        vAddress := TExternalAddress.Create(FConn);
        try
          AssertNotNull(vAddress);
          vAddress.Site_Contact := vPerson;
          AssertEquals('vPerson', vAddress.Site_Contact.Name);
          vSubProject.AddAddress(vAddress);
        except
          vAddress.Free;
          raise;
        end;
        vProject.AddSubProject(vSubProject);
      except
        vSubProject.Free;
        raise;
      end;
      vPerson.AddProject(vProject);
    finally
      vProject.Free;
    end;
    AssertEquals('vPerson.RefCount 1', 2, vPerson.RefCount);
    AssertEquals('vPerson.ReferencedBy.Count 1', 1, vPerson.ReferencedBy.Count);

    AssertEquals('vPerson.Projects[0].RefCount',
            1, vPerson.Projects[0].RefCount);
    AssertEquals('vPerson.Projects[0].ReferencedBy.Count',
            1, vPerson.Projects[0].ReferencedBy.Count);

    AssertEquals('vPerson.Projects[0].SubProjects[0].RefCount',
            1, vPerson.Projects[0].SubProjects[0].RefCount);
    AssertEquals('vPerson.Projects[0].SubProjects[0].ReferencedBy.Count',
            0, vPerson.Projects[0].SubProjects[0].ReferencedBy.Count);

    AssertEquals('vPerson.Projects[0].SubProjects[0].Addresses[0].RefCount',
            1, vPerson.Projects[0].SubProjects[0].Addresses[0].RefCount);
    AssertEquals('vPerson.Projects[0].SubProjects[0].Addresses[0].ReferencedBy.Count',
            0, vPerson.Projects[0].SubProjects[0].Addresses[0].ReferencedBy.Count);
  finally
    vPerson.Free;
  end;
//  AssertEquals('vPerson.RefCount 2', 1, vPerson.RefCount);
//  AssertEquals('vPerson.ReferencedBy.Count 2', 1, vPerson.ReferencedBy.Count);
end;

// A -> <- B
// |
// +-> C
// then delete C
procedure TestTInstantReferences_Leak.TestCircularReferences3;
var
  vPerson1: TPerson;
  vPerson2: TPerson;
begin
  FOwner.Name := 'Owner';

  vPerson1 := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson1);
    vPerson1.Name := 'vPerson1';

    vPerson1.EmployBy(FOwner);
    AssertNotNull(vPerson1.Employer);
    AssertEquals('vPerson1.Employer.Name A', 'Owner', vPerson1.Employer.Name);
  finally
    vPerson1.Free;
  end;
  AssertEquals('FOwner.RefCount 1', 2, FOwner.RefCount);
  AssertEquals('FOwner.ReferencedBy.Count 1', 1, FOwner.ReferencedBy.Count);
  AssertEquals('FOwner.EmployeeCount 1', 1, FOwner.EmployeeCount);
  AssertEquals('FOwner.Employees[0].RefCount 1',
          1, FOwner.Employees[0].RefCount);
  AssertEquals('FOwner.Employees[0].ReferencedBy.Count 1',
          1, FOwner.Employees[0].ReferencedBy.Count);

  vPerson2 := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson2);
    vPerson2.Name := 'vPerson2';

    FOwner.AddEmployee(vPerson2);
    AssertNull(vPerson2.Employer);
  finally
    vPerson2.Free;
  end;
  AssertEquals('FOwner.RefCount 1', 2, FOwner.RefCount);
  AssertEquals('FOwner.ReferencedBy.Count 1', 1, FOwner.ReferencedBy.Count);

  AssertEquals('FOwner.EmployeeCount', 2, FOwner.EmployeeCount);
  AssertEquals('FOwner.Employees[1].RefCount 1',
          1, FOwner.Employees[1].RefCount);
  AssertEquals('FOwner.Employees[1].ReferencedBy.Count 1',
          1, FOwner.Employees[1].ReferencedBy.Count);

  FOwner.DeleteEmployee(1);
  AssertEquals('FOwner.EmployeeCount', 1, FOwner.EmployeeCount);
end;

// A->B->C->A
//    |
//    +->D
// then delete D
procedure TestTInstantReferences_Leak.TestCircularReferences4;
var
  vPerson: TPerson;
  vProject1: TProject;
  vProject2: TProject;
begin
  FOwner.Name := 'Owner';

  vPerson := TPerson.Create(FConn);
  try
    AssertNotNull(vPerson);
    vPerson.Name := 'vPerson';

    vPerson.EmployBy(FOwner);
    AssertNotNull(vPerson.Employer);
    AssertEquals('vPerson.Employer.Name A', 'Owner', vPerson.Employer.Name);
    FOwner.DeleteEmployee(0);

    vProject1 := TProject.Create(FConn);
    try
      AssertNotNull(vProject1);
      vProject1.Name := 'vProject1';
      vProject1.Manager := vPerson;
      FOwner.AddProject(vProject1);
    finally
      vProject1.Free;
    end;

    vProject2 := TProject.Create(FConn);
    try
      AssertNotNull(vProject2);
      vProject2.Name := 'vProject2';
      FOwner.AddProject(vProject2);
    finally
      vProject2.Free;
    end;

    FreeAndNil(FOwner);

    AssertEquals('vPerson.RefCount 1',
            2, vPerson.RefCount);
    AssertEquals('vPerson.ReferencedBy.Count 1',
            1, vPerson.ReferencedBy.Count);

    AssertEquals('vPerson.Employer.RefCount 1',
            1, vPerson.Employer.RefCount);
    AssertEquals('vPerson.Employer.ReferencedBy.Count 1',
            1, vPerson.Employer.ReferencedBy.Count);

    AssertEquals('vPerson.Employer.EmployeeCount 1',
            0, vPerson.Employer.EmployeeCount);

    AssertEquals('vPerson.Employer.ProjectCount 1',
            2, vPerson.Employer.ProjectCount);
    AssertEquals('vPerson.Employer.Projects[0].RefCount 1',
            1, vPerson.Employer.Projects[0].RefCount);
    AssertEquals('vPerson.Employer.Projects[0].ReferencedBy.Count 1',
            1, vPerson.Employer.Projects[0].ReferencedBy.Count);
    AssertEquals('vPerson.Employer.Projects[1].RefCount 1',
            1, vPerson.Employer.Projects[1].RefCount);
    AssertEquals('vPerson.Employer.Projects[1].ReferencedBy.Count 1',
            1, vPerson.Employer.Projects[1].ReferencedBy.Count);

    vPerson.Employer.DeleteProject(1);
    AssertEquals('vPerson.Employer.ProjectCount 1',
            1, vPerson.Employer.ProjectCount);
  finally
    vPerson.Free;
  end;
end;

function TestTInstantEmbReferences.RefsEmbeddedCompare(Holder, Obj1, Obj2:
    TInstantObject): Integer;
var
  vObj1, vObj2: TPerson;
begin
  vObj1 := Obj1 as TPerson;
  vObj2 := Obj2 as TPerson;

  Result := AnsiCompareText(vObj1.Name, vObj2.Name);
end;

procedure TestTInstantEmbReferences.SetUp;
var
  i: Integer;
  Person: TPerson;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TCompany.Create(FConn);
  FInstantReferences := FOwner._Employees;
  for i := 0 to 2 do
  begin
    Person := TPerson.Create(FConn);
    try
      FOwner.AddEmployee(Person);
    finally
      Person.Free;
    end;
  end;
  AssertEquals('Setup FInstantReferences.Count', 3, FInstantReferences.Count);
end;

procedure TestTInstantEmbReferences.TearDown;
begin
  FInstantReferences := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantEmbReferences.TestAdd;
var
  vReturnValue: Integer;
  vPerson: TPerson;
begin
  vPerson := TPerson.Create(FConn);
  try
    vReturnValue := FInstantReferences.Add(vPerson);
    AssertTrue(vReturnValue <> -1);
    AssertEquals(4, FInstantReferences.Count);
  finally
    vPerson.Free;
  end;
end;

procedure TestTInstantEmbReferences.TestAssign;
var
  vSource: TInstantReferences;
  vAttrMetadata: TInstantAttributeMetadata;
begin
  vSource := nil;

  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  try
    vAttrMetadata.AttributeClass := TInstantReferences;
    vSource := TInstantReferences.Create(FOwner, vAttrMetadata);

    AssertTrue(vSource.Count = 0);
    vSource.Assign(FInstantReferences);
    AssertTrue(vSource.Count = 3);
    AssertSame(FInstantReferences.Items[0], vSource.Items[0]);
  finally
    vSource.Free;
    vAttrMetadata.Free;
  end;
end;

procedure TestTInstantEmbReferences.TestAttachObject;
var
  vReturnValue: Boolean;
  vPerson: TPerson;
begin
  vPerson := TPerson.Create(FConn);
  try
    vReturnValue := FInstantReferences.AttachObject(vPerson);
    AssertTrue(vReturnValue);
    AssertEquals(4, FInstantReferences.Count);
  finally
    vPerson.Free;
  end;
end;

procedure TestTInstantEmbReferences.TestClear;
begin
  FInstantReferences.Clear;
  AssertEquals(0, FInstantReferences.Count);
end;

procedure TestTInstantEmbReferences.TestDelete;
begin
  FInstantReferences.Delete(1);
  AssertEquals(2, FInstantReferences.Count);
end;

procedure TestTInstantEmbReferences.TestDestroyObject;
begin
  AssertTrue(FInstantReferences.HasItem(1));

  FInstantReferences.DestroyObject(1);
  AssertFalse(FInstantReferences.HasItem(1));
end;

procedure TestTInstantEmbReferences.TestDetachObject;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantReferences.DetachObject(FInstantReferences.Items[1]);
  AssertTrue(vReturnValue);
  AssertEquals(2, FInstantReferences.Count);
end;

procedure TestTInstantEmbReferences.TestExchange;
begin
  TPerson(FInstantReferences.Items[0]).Name := 'Ref0';
  TPerson(FInstantReferences.Items[1]).Name := 'Ref1';
  TPerson(FInstantReferences.Items[2]).Name := 'Ref2';
  FInstantReferences.Exchange(0, 2);
  AssertEquals('Ref2', TPerson(FInstantReferences.Items[0]).Name);
  AssertEquals('Ref1', TPerson(FInstantReferences.Items[1]).Name);
  AssertEquals('Ref0', TPerson(FInstantReferences.Items[2]).Name);
end;

procedure TestTInstantEmbReferences.TestHasItem;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantReferences.HasItem(1);
  AssertTrue(vReturnValue);

  FInstantReferences.DestroyObject(1);
  vReturnValue := FInstantReferences.HasItem(1);
  AssertFalse(vReturnValue);
end;

procedure TestTInstantEmbReferences.TestIndexOfInstance;
var
  vReturnValue: Integer;
  vInstance: Pointer;
begin
  vInstance := TPerson.Create(FConn);
  try
    FInstantReferences.Insert(1, vInstance);
    vReturnValue := FInstantReferences.IndexOfInstance(vInstance);
    AssertEquals(1, vReturnValue);
  finally
    TObject(vInstance).Free;
  end;
end;

procedure TestTInstantEmbReferences.TestIndexOf_Insert;
var
  vReturnValue: Integer;
  vObject: TInstantObject;
begin
  vObject := TPerson.Create(FConn);
  try
    FInstantReferences.Insert(1, vObject);
    vReturnValue := FInstantReferences.IndexOf(vObject);
    AssertEquals(1, vReturnValue);
  finally
    vObject.Free;
  end;
end;

procedure TestTInstantEmbReferences.TestLoadReferencesTo_FromStream;
var
  vStream: TStream;
begin
  vStream := TMemoryStream.Create;
  try
    AssertEquals(0, vStream.Size);
    FInstantReferences.SaveReferencesToStream(vStream);
    AssertTrue(vStream.Size > 0);
    FInstantReferences.Clear;
    AssertEquals(0, FInstantReferences.Count);

    vStream.Position := 0;
    FInstantReferences.LoadReferencesFromStream(vStream);
    AssertEquals(3, FInstantReferences.Count);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantEmbReferences.TestMove;
var
  vPerson: TPerson;
begin
  TPerson(FInstantReferences.Items[0]).Name := 'Ref0';
  TPerson(FInstantReferences.Items[1]).Name := 'Ref1';
  TPerson(FInstantReferences.Items[2]).Name := 'Ref2';
  vPerson := TPerson.Create(FConn);
  try
    FInstantReferences.Add(vPerson);
  finally
    vPerson.Free;
  end;
  TPerson(FInstantReferences.Items[3]).Name := 'Ref3';
  FInstantReferences.Move(0, 2);
  AssertEquals('Ref1', TPerson(FInstantReferences.Items[0]).Name);
  AssertEquals('Ref2', TPerson(FInstantReferences.Items[1]).Name);
  AssertEquals('Ref0', TPerson(FInstantReferences.Items[2]).Name);
  AssertEquals('Ref3', TPerson(FInstantReferences.Items[3]).Name);
end;

procedure TestTInstantEmbReferences.TestRemove;
var
  vReturnValue: Integer;
begin
  vReturnValue := FInstantReferences.Remove(FInstantReferences.Items[1]);
  AssertEquals(1, vReturnValue);
  AssertEquals(2, FInstantReferences.Count);
end;

procedure TestTInstantEmbReferences.TestReset;
begin
  FInstantReferences.Reset;
  AssertEquals(0, FInstantReferences.Count);
end;

procedure TestTInstantEmbReferences.TestSaveObjectsTo_FromStream;
var
  vStream: TStream;
  vObject: TInstantObject;
begin
  vStream := TMemoryStream.Create;
  try
    AssertEquals(0, vStream.Size);
    FInstantReferences.SaveObjectsToStream(vStream);
    AssertTrue(vStream.Size > 0);
    vObject := FInstantReferences[0];
    AssertEquals('vObject.RefCount 1', 1, vObject.RefCount);
    FInstantReferences.Clear;
    AssertEquals(0, FInstantReferences.Count);

    vStream.Position := 0;
    FInstantReferences.LoadObjectsFromStream(vStream);
    AssertEquals(3, FInstantReferences.Count);
    vObject := FInstantReferences[0];
    AssertEquals('vObject.RefCount 2', 1, vObject.RefCount);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantEmbReferences.TestSort;
var
  vPerson: TPerson;
begin
  TPerson(FInstantReferences.Items[0]).Name := '2 Ref';
  TPerson(FInstantReferences.Items[1]).Name := '0 Ref';
  TPerson(FInstantReferences.Items[2]).Name := '1 Ref';
  vPerson := TPerson.Create(FConn);
  try
    FOwner.AddEmployee(vPerson);
  finally
    vPerson.Free;
  end;
  TPerson(FInstantReferences.Items[3]).Name := '0 Ref';

  FInstantReferences.Sort(RefsEmbeddedCompare);
  AssertEquals('0 Ref', TPerson(FInstantReferences.Items[0]).Name);
  AssertEquals('0 Ref', TPerson(FInstantReferences.Items[1]).Name);
  AssertEquals('1 Ref', TPerson(FInstantReferences.Items[2]).Name);
  AssertEquals('2 Ref', TPerson(FInstantReferences.Items[3]).Name);
end;

procedure TestTInstantEmbReferences.TestUnchanged;
begin
  AssertEquals(3, FInstantReferences.Count);
  AssertTrue(FInstantReferences.IsChanged);

  FInstantReferences.Unchanged;
  AssertFalse(FInstantReferences.IsChanged);

  TPerson(FInstantReferences.Items[1]).Name := 'Ref2';
  AssertFalse(FInstantReferences.IsChanged);

  FInstantReferences.Delete(1);
  AssertTrue(FInstantReferences.IsChanged);
end;

function TestTInstantExtReferences.RefsExternalCompare(Holder, Obj1, Obj2:
    TInstantObject): Integer;
var
  vObj1, vObj2: TProject;
begin
  vObj1 := Obj1 as TProject;
  vObj2 := Obj2 as TProject;

  Result := AnsiCompareText(vObj1.Name, vObj2.Name);
end;

{ TestTInstantExtReferences }

procedure TestTInstantExtReferences.SetUp;
var
  i: Integer;
  Project: TProject;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TCompany.Create(FConn);
  FInstantReferences := FOwner._Projects;
  for i := 0 to 2 do
  begin
    Project := TProject.Create(FConn);
    try
      FOwner.AddProject(Project);
    finally
      Project.Free;
    end;
  end;
  AssertEquals('Setup FInstantReferences.Count', 3, FInstantReferences.Count);
end;

procedure TestTInstantExtReferences.TearDown;
begin
  FInstantReferences := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantExtReferences.TestAdd;
var
  vReturnValue: Integer;
  vProject: TProject;
begin
  vProject := TProject.Create(FConn);
  try
    vReturnValue := FInstantReferences.Add(vProject);
    AssertTrue(vReturnValue <> -1);
    AssertEquals(4, FInstantReferences.Count);
  finally
    vProject.Free;
  end;
end;

procedure TestTInstantExtReferences.TestAssign;
var
  vSource: TInstantReferences;
  vAttrMetadata: TInstantAttributeMetadata;
begin
  vSource := nil;

  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  try
    vAttrMetadata.AttributeClass := TInstantReferences;
    vSource := TInstantReferences.Create(FOwner, vAttrMetadata);

    AssertTrue(vSource.Count = 0);
    vSource.Assign(FInstantReferences);
    AssertTrue(vSource.Count = 3);
    AssertSame(FInstantReferences.Items[0], vSource.Items[0]);
  finally
    vSource.Free;
    vAttrMetadata.Free;
  end;
end;

procedure TestTInstantExtReferences.TestAttachObject;
var
  vReturnValue: Boolean;
  vProject: TProject;
begin
  vProject := TProject.Create(FConn);
  try
    vReturnValue := FInstantReferences.AttachObject(vProject);
    AssertTrue(vReturnValue);
    AssertEquals(4, FInstantReferences.Count);
  finally
    vProject.Free;
  end;
end;

procedure TestTInstantExtReferences.TestClear;
begin
  FInstantReferences.Clear;
  AssertEquals(0, FInstantReferences.Count);
end;

procedure TestTInstantExtReferences.TestDelete;
begin
  FInstantReferences.Delete(1);
  AssertEquals(2, FInstantReferences.Count);
end;

procedure TestTInstantExtReferences.TestDestroyObject;
begin
  AssertTrue(FInstantReferences.HasItem(1));

  FInstantReferences.DestroyObject(1);
  AssertFalse(FInstantReferences.HasItem(1));
end;

procedure TestTInstantExtReferences.TestDetachObject;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantReferences.DetachObject(FInstantReferences.Items[1]);
  AssertTrue(vReturnValue);
  AssertEquals(2, FInstantReferences.Count);
end;

procedure TestTInstantExtReferences.TestExchange;
begin
  TProject(FInstantReferences.Items[0]).Name := 'Ref0';
  TProject(FInstantReferences.Items[1]).Name := 'Ref1';
  TProject(FInstantReferences.Items[2]).Name := 'Ref2';
  FInstantReferences.Exchange(0, 2);
  AssertEquals('Ref2', TProject(FInstantReferences.Items[0]).Name);
  AssertEquals('Ref1', TProject(FInstantReferences.Items[1]).Name);
  AssertEquals('Ref0', TProject(FInstantReferences.Items[2]).Name);
end;

procedure TestTInstantExtReferences.TestHasItem;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantReferences.HasItem(1);
  AssertTrue(vReturnValue);

  FInstantReferences.DestroyObject(1);
  vReturnValue := FInstantReferences.HasItem(1);
  AssertFalse(vReturnValue);
end;

procedure TestTInstantExtReferences.TestIndexOfInstance;
var
  vReturnValue: Integer;
  vInstance: Pointer;
begin
  vInstance := TProject.Create(FConn);
  try
    FInstantReferences.Insert(1, vInstance);
    vReturnValue := FInstantReferences.IndexOfInstance(vInstance);
    AssertEquals(1, vReturnValue);
  finally
    TObject(vInstance).Free;
  end;
end;

procedure TestTInstantExtReferences.TestIndexOf_Insert;
var
  vReturnValue: Integer;
  vObject: TInstantObject;
begin
  vObject := TProject.Create(FConn);
  try
    FInstantReferences.Insert(1, vObject);
    vReturnValue := FInstantReferences.IndexOf(vObject);
    AssertEquals(1, vReturnValue);
  finally
    vObject.Free;
  end;
end;

procedure TestTInstantExtReferences.TestLoadReferencesTo_FromStream;
var
  vStream: TStream;
begin
  vStream := TMemoryStream.Create;
  try
    AssertEquals(0, vStream.Size);
    FInstantReferences.SaveReferencesToStream(vStream);
    AssertTrue(vStream.Size > 0);
    FInstantReferences.Clear;
    AssertEquals(0, FInstantReferences.Count);

    vStream.Position := 0;
    FInstantReferences.LoadReferencesFromStream(vStream);
    AssertEquals(3, FInstantReferences.Count);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantExtReferences.TestMove;
var
  vProject: TProject;
begin
  TProject(FInstantReferences.Items[0]).Name := 'Ref0';
  TProject(FInstantReferences.Items[1]).Name := 'Ref1';
  TProject(FInstantReferences.Items[2]).Name := 'Ref2';
  vProject := TProject.Create(FConn);
  try
    FInstantReferences.Add(vProject);
  finally
    vProject.Free;
  end;
  TProject(FInstantReferences.Items[3]).Name := 'Ref3';
  FInstantReferences.Move(0, 2);
  AssertEquals('Ref1', TProject(FInstantReferences.Items[0]).Name);
  AssertEquals('Ref2', TProject(FInstantReferences.Items[1]).Name);
  AssertEquals('Ref0', TProject(FInstantReferences.Items[2]).Name);
  AssertEquals('Ref3', TProject(FInstantReferences.Items[3]).Name);
end;

procedure TestTInstantExtReferences.TestRemove;
var
  vReturnValue: Integer;
begin
  vReturnValue := FInstantReferences.Remove(FInstantReferences.Items[1]);
  AssertEquals(1, vReturnValue);
  AssertEquals(2, FInstantReferences.Count);
end;

procedure TestTInstantExtReferences.TestReset;
begin
  FInstantReferences.Reset;
  AssertEquals(0, FInstantReferences.Count);
end;

procedure TestTInstantExtReferences.TestSaveObjectsTo_FromStream;
var
  vStream: TStream;
  vObject: TInstantObject;
begin
  vStream := TMemoryStream.Create;
  try
    AssertEquals(0, vStream.Size);
    FInstantReferences.SaveObjectsToStream(vStream);
    AssertTrue(vStream.Size > 0);
    vObject := FInstantReferences[0];
    AssertEquals('vObject.RefCount 1', 1, vObject.RefCount);
    FInstantReferences.Clear;
    AssertEquals(0, FInstantReferences.Count);

    vStream.Position := 0;
    FInstantReferences.LoadObjectsFromStream(vStream);
    AssertEquals(3, FInstantReferences.Count);
    vObject := FInstantReferences[0];
    AssertEquals('vObject.RefCount 2', 1, vObject.RefCount);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantExtReferences.TestSort;
var
  vProject: TProject;
begin
  TProject(FInstantReferences.Items[0]).Name := '2 Ref';
  TProject(FInstantReferences.Items[1]).Name := '0 Ref';
  TProject(FInstantReferences.Items[2]).Name := '1 Ref';
  vProject := TProject.Create(FConn);
  try
    FOwner.AddProject(vProject);
  finally
    vProject.Free;
  end;
  TProject(FInstantReferences.Items[3]).Name := '0 Ref';

  FInstantReferences.Sort(RefsExternalCompare);
  AssertEquals('0 Ref', TProject(FInstantReferences.Items[0]).Name);
  AssertEquals('0 Ref', TProject(FInstantReferences.Items[1]).Name);
  AssertEquals('1 Ref', TProject(FInstantReferences.Items[2]).Name);
  AssertEquals('2 Ref', TProject(FInstantReferences.Items[3]).Name);
end;

procedure TestTInstantExtReferences.TestUnchanged;
begin
  AssertEquals(3, FInstantReferences.Count);
  AssertTrue(FInstantReferences.IsChanged);

  FInstantReferences.Unchanged;
  AssertFalse(FInstantReferences.IsChanged);

  TProject(FInstantReferences.Items[1]).Name := 'Ref2';
  AssertFalse(FInstantReferences.IsChanged);

  FInstantReferences.Delete(1);
  AssertTrue(FInstantReferences.IsChanged);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantExtReferences,
                TestTinstantEmbReferences,
                TestTInstantReferences_Leak]);
{$ENDIF}

end.
 