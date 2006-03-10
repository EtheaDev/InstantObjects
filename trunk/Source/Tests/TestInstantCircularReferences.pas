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
 * The Original Code is: InstantObjects Test Suite/TestInstantCircularReferences
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

unit TestInstantCircularReferences;

interface

uses fpcunit, InstantMock, InstantPersistence, TestModel;

type

  // For leak testing, run these tests in conjunction 
  // with a memory leak test utility.
  
  TestCircularReferences = class(TTestCase)
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
    // + -> C
    // then delete C
    procedure TestCircularReferences3;
    // A -> B -> C -> A
    //      |
    //      + -> D
    // then delete D
    procedure TestCircularReferences4;
    // A -> B -> C -> A
    //      |
    //      + -> D -> E
    // then delete E
    procedure TestCircularReferences5;
    //      +-> E -> F
    //      |
    // A -> B -> C -> A
    // ^    ^    ^
    // +--D-+----+
    procedure TestCircularReferences6;
  end;

implementation

uses SysUtils, Classes, Windows, testregistry;

procedure TestCircularReferences.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TCompany.Create(FConn);
end;

procedure TestCircularReferences.TearDown;
begin
  FInstantReferences := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestCircularReferences.TestAddEmbeddedObject;
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

procedure TestCircularReferences.TestAddExternalObject;
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
procedure TestCircularReferences.TestCircularReferences;
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
procedure TestCircularReferences.TestCircularReferences1;
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
procedure TestCircularReferences.TestCircularReferences2;
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
// + -> C
// then delete C
procedure TestCircularReferences.TestCircularReferences3;
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

// A -> B -> C -> A
//    |
//    + -> D
// then delete D
procedure TestCircularReferences.TestCircularReferences4;
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

// A -> B -> C -> A
//    |
//    + -> D -> E
// then delete E
procedure TestCircularReferences.TestCircularReferences5;
var
  vPerson: TPerson;
  vProject1: TProject;
  vProject2: TProject;
  vPerson2: TPerson;
begin
  FOwner.Name := 'Owner'; // B

  vPerson := TPerson.Create(FConn); // A
  try
    AssertNotNull(vPerson);
    vPerson.Name := 'vPerson';

    vPerson.EmployBy(FOwner);
    AssertNotNull(vPerson.Employer);
    AssertEquals('vPerson.Employer.Name A', 'Owner', vPerson.Employer.Name);
    FOwner.DeleteEmployee(0);

    vProject1 := TProject.Create(FConn); // C
    try
      AssertNotNull(vProject1);
      vProject1.Name := 'vProject1';
      vProject1.Manager := vPerson;
      FOwner.AddProject(vProject1);
    finally
      vProject1.Free;
    end;

    vProject2 := TProject.Create(FConn); // D
    try
      AssertNotNull(vProject2);
      vProject2.Name := 'vProject2';
      vPerson2 := TPerson.Create(FConn); // E
      try
        AssertNotNull(vPerson2);
        vPerson2.Name := 'vPerson2';

        vProject2.Manager := vPerson2;
      finally
        vPerson2.Free;
      end;
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

    AssertEquals('vPerson.Employer.Projects[1].Manager.RefCount 1',
            1, vPerson.Employer.Projects[1].Manager.RefCount);
    AssertEquals('vPerson.Employer.Projects[1].Manager.ReferencedBy.Count 1',
            1, vPerson.Employer.Projects[1].Manager.ReferencedBy.Count);

    vPerson.Employer.Projects[1].Manager := nil;
    AssertEquals('vPerson.Employer.ProjectCount 1',
            2, vPerson.Employer.ProjectCount);
  finally
    vPerson.Free;
  end;
end;

//     +-> E -> F
//     |
//A -> B -> C -> A
//^    ^    ^
//+--D-+----+
//
//where I observed a disconnection between B and C using
//this sequence of assignment:
//
//VA.RefB := VB;
//VB.RefC := VC;
//VB.RefE := VE;
//VC.RefA := VA;
//VD.RefA := VA;
//VD.RefB := VB;
//VD.RefC := VC;
//VE.RefF := VF;
//
//and this sequence of disposing:
//
//VE.Free;
//VB.Free;
//VA.Free;
//VC.Free;
//
//Test here, VD.RefB.RefC (or VD.RefC, I don't remember) is nil
//
//F.Free;
//D.Free;
procedure TestCircularReferences.TestCircularReferences6;
var
  vPerson: TPerson;
  vProject1: TProject;
  vProject2: TProject;
  vPerson2: TPerson;
  vCategory: TCategory;
begin
  vPerson2 := nil;   //E
  vProject1 := nil;  //C
  vProject2 := nil;  //D
  vCategory := nil;  //F

  FOwner.Name := 'Owner'; // B

  vPerson := TPerson.Create(FConn); // A
  try
    AssertNotNull(vPerson);
    vPerson.Name := 'vPerson';

    // A -> B
    vPerson.Employer := FOwner;
    AssertNotNull(vPerson.Employer);
    AssertEquals('vPerson.Employer.Name A', 'Owner', vPerson.Employer.Name);

    vProject1 := TProject.Create(FConn); // C
    AssertNotNull(vProject1);
    vProject1.Name := 'vProject1';
    // B -> C
    FOwner.AddProject(vProject1);

    vPerson2 := TPerson.Create(FConn); // E
    AssertNotNull(vPerson2);
    vPerson2.Name := 'vPerson2';
    // B -> E
    FOwner.AddEmployee(vPerson2);

    // C -> A
    vProject1.Manager := vPerson;
    AssertNotNull(vProject1);

    vProject2 := TProject.Create(FConn); // D
    AssertNotNull(vProject2);
    vProject2.Name := 'vProject2';
    // D -> A
    vProject2.AddParticipant(vPerson);
    // D -> B
    vProject2.Manager := FOwner;
    AssertNotNull(vProject2);
    AssertNotNull(vProject1);
//    // D -> C
//    // If the following line is uncommented  <--------------
//    // an AV will be raised at runtime       <--------------
//    vProject2.AddSubProject(vProject1);

//    vCategory := TCategory.Create(FConn); // F
//    AssertNotNull(vCategory);
//    vCategory.Name := 'vCategory';
//    // E -> F
//    vPerson2.Category := vCategory;

//    AssertEquals('vPerson.RefCount 1',
//            3, vPerson.RefCount);
//    AssertEquals('vPerson.ReferencedBy.Count 1',
//            2, vPerson.ReferencedBy.Count);
//
//    AssertEquals('FOwner.RefCount 1',
//            3, FOwner.RefCount);
//    AssertEquals('FOwner.ReferencedBy.Count 1',
//            2, FOwner.ReferencedBy.Count);
//
//    AssertEquals('FOwner.EmployeeCount 1',
//            1, vPerson.Employer.EmployeeCount);
//
//    AssertEquals('FOwner.ProjectCount 1',
//            1, FOwner.ProjectCount);
//    AssertEquals('vProject1.RefCount 1',
//            3, vProject1.RefCount);
//    AssertEquals('vProject1.ReferencedBy.Count 1',
//            1, vProject1.ReferencedBy.Count);
//
//    AssertEquals('vPerson2.RefCount 1',
//            2, vPerson2.RefCount);
//    AssertEquals('vPerson2.ReferencedBy.Count 1',
//            1, vPerson2.ReferencedBy.Count);
//
//    AssertEquals('vCategory.RefCount 1',
//            2, vCategory.RefCount);
//    AssertEquals('vCategory.ReferencedBy.Count 1',
//            1, vCategory.ReferencedBy.Count);
//
//    AssertEquals('vProject2.RefCount 1',
//            1, vProject2.RefCount);
//    AssertEquals('vProject2.ReferencedBy.Count 1',
//            0, vProject2.ReferencedBy.Count);

  finally
    vPerson2.Free;   //E
    FreeAndNil(FOwner);     //B
    vPerson.Free;    //A
    vProject1.Free;  //C
    vCategory.Free;  //F
    vProject2.Free;  //D
  end;
end;


initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestCircularReferences]);
{$ELSE}
  RegisterTests([TestCircularReferences]);
{$ENDIF}

end.
 