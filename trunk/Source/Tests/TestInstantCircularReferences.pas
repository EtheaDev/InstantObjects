(*
 *   InstantObjects Test Suite
 *   TestInstantCircularReferences
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
 * Joao Morais
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
    // Free order: E, B, A, C, F, D
    procedure TestCircularReferences6;
    // A -> <- B
    // ^       ^
    // +-- C --+
    // Free order: A, B, C
    procedure TestCircularReferences7;
    // A -> B -> A
    // |         ^
    // +--> C ---+
    // Free order: A, B, C
    procedure TestCircularReferences8;
    // A -> A
    procedure TestCircularReferences9;
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
  AssertEquals('FOwner.RefByCount 1', 1, FOwner.RefByCount);
  AssertEquals('FOwner.EmployeeCount 1', 1, FOwner.EmployeeCount);
  AssertEquals('FOwner.Employees[0].RefCount 1',
          1, FOwner.Employees[0].RefCount);
  AssertEquals('FOwner.Employees[0].RefByCount 1',
          1, FOwner.Employees[0].RefByCount);

  vCategory := TCategory.Create(FConn);
  try
    AssertNotNull(vCategory);
    vCategory.Name := 'vCategory';

    FOwner.Employees[0].Category := vCategory;
  finally
    vCategory.Free;
  end;
  AssertEquals('FOwner.RefCount 2', 2, FOwner.RefCount);
  AssertEquals('FOwner.RefByCount 2', 1, FOwner.RefByCount);

  AssertEquals('FOwner.Employees[0].RefCount 2',
          1, FOwner.Employees[0].RefCount);
  AssertEquals('FOwner.Employees[0].RefByCount 2',
          1, FOwner.Employees[0].RefByCount);

  AssertEquals('FOwner.Employees[0].Category.RefCount 1',
          1, FOwner.Employees[0].Category.RefCount);
  AssertEquals('FOwner.Employees[0].Category.RefByCount 1',
          1, FOwner.Employees[0].Category.RefByCount);
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
  AssertEquals('vPerson.RefByCount 1', 1, vPerson.RefByCount);
  AssertEquals('vPerson.Projects[0].RefCount', 1, vPerson.Projects[0].RefCount);
  AssertEquals('vPerson.Projects[0].RefByCount',
          1, vPerson.Projects[0].RefByCount);
  AssertEquals('vPerson.Projects[0].Addresses[0].RefCount',
          1, vPerson.Projects[0].Addresses[0].RefCount);
  AssertEquals('vPerson.Projects[0].Addresses[0].RefByCount',
          0, vPerson.Projects[0].Addresses[0].RefByCount);
  finally
    vPerson.Free;
  end;
//  AssertEquals('vPerson.RefCount 2', 1, vPerson.RefCount);
//  AssertEquals('vPerson.RefByCount 2', 1, vPerson.RefByCount);
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
    AssertEquals('vPerson.RefByCount 1', 1, vPerson.RefByCount);

    AssertEquals('vPerson.Projects[0].RefCount',
            1, vPerson.Projects[0].RefCount);
    AssertEquals('vPerson.Projects[0].RefByCount',
            1, vPerson.Projects[0].RefByCount);

    AssertEquals('vPerson.Projects[0].SubProjects[0].RefCount',
            1, vPerson.Projects[0].SubProjects[0].RefCount);
    AssertEquals('vPerson.Projects[0].SubProjects[0].RefByCount',
            0, vPerson.Projects[0].SubProjects[0].RefByCount);

    AssertEquals('vPerson.Projects[0].SubProjects[0].Addresses[0].RefCount',
            1, vPerson.Projects[0].SubProjects[0].Addresses[0].RefCount);
    AssertEquals('vPerson.Projects[0].SubProjects[0].Addresses[0].RefByCount',
            0, vPerson.Projects[0].SubProjects[0].Addresses[0].RefByCount);
  finally
    vPerson.Free;
  end;
//  AssertEquals('vPerson.RefCount 2', 1, vPerson.RefCount);
//  AssertEquals('vPerson.RefByCount 2', 1, vPerson.RefByCount);
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
  AssertEquals('FOwner.RefByCount 1', 1, FOwner.RefByCount);
  AssertEquals('FOwner.EmployeeCount 1', 1, FOwner.EmployeeCount);
  AssertEquals('FOwner.Employees[0].RefCount 1',
          1, FOwner.Employees[0].RefCount);
  AssertEquals('FOwner.Employees[0].RefByCount 1',
          1, FOwner.Employees[0].RefByCount);

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
  AssertEquals('FOwner.RefByCount 1', 1, FOwner.RefByCount);

  AssertEquals('FOwner.EmployeeCount', 2, FOwner.EmployeeCount);
  AssertEquals('FOwner.Employees[1].RefCount 1',
          1, FOwner.Employees[1].RefCount);
  AssertEquals('FOwner.Employees[1].RefByCount 1',
          1, FOwner.Employees[1].RefByCount);

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
    AssertEquals('vPerson.RefByCount 1',
            1, vPerson.RefByCount);

    AssertEquals('vPerson.Employer.RefCount 1',
            1, vPerson.Employer.RefCount);
    AssertEquals('vPerson.Employer.RefByCount 1',
            1, vPerson.Employer.RefByCount);

    AssertEquals('vPerson.Employer.EmployeeCount 1',
            0, vPerson.Employer.EmployeeCount);

    AssertEquals('vPerson.Employer.ProjectCount 1',
            2, vPerson.Employer.ProjectCount);
    AssertEquals('vPerson.Employer.Projects[0].RefCount 1',
            1, vPerson.Employer.Projects[0].RefCount);
    AssertEquals('vPerson.Employer.Projects[0].RefByCount 1',
            1, vPerson.Employer.Projects[0].RefByCount);
    AssertEquals('vPerson.Employer.Projects[1].RefCount 1',
            1, vPerson.Employer.Projects[1].RefCount);
    AssertEquals('vPerson.Employer.Projects[1].RefByCount 1',
            1, vPerson.Employer.Projects[1].RefByCount);

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
    AssertEquals('vPerson.RefByCount 1',
            1, vPerson.RefByCount);

    AssertEquals('vPerson.Employer.RefCount 1',
            1, vPerson.Employer.RefCount);
    AssertEquals('vPerson.Employer.RefByCount 1',
            1, vPerson.Employer.RefByCount);

    AssertEquals('vPerson.Employer.EmployeeCount 1',
            0, vPerson.Employer.EmployeeCount);

    AssertEquals('vPerson.Employer.ProjectCount 1',
            2, vPerson.Employer.ProjectCount);
    AssertEquals('vPerson.Employer.Projects[0].RefCount 1',
            1, vPerson.Employer.Projects[0].RefCount);
    AssertEquals('vPerson.Employer.Projects[0].RefByCount 1',
            1, vPerson.Employer.Projects[0].RefByCount);
    AssertEquals('vPerson.Employer.Projects[1].RefCount 1',
            1, vPerson.Employer.Projects[1].RefCount);
    AssertEquals('vPerson.Employer.Projects[1].RefByCount 1',
            1, vPerson.Employer.Projects[1].RefByCount);

    AssertEquals('vPerson.Employer.Projects[1].Manager.RefCount 1',
            1, vPerson.Employer.Projects[1].Manager.RefCount);
    AssertEquals('vPerson.Employer.Projects[1].Manager.RefByCount 1',
            1, vPerson.Employer.Projects[1].Manager.RefByCount);

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
  vCompany2: TCompany;
  vPerson2: TPerson;
  vCategory: TCategory;
begin
//  vPerson2 := nil;   //E
//  vProject1 := nil;  //C
  vCompany2 := nil;  //D
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

    vCompany2 := TCompany.Create(FConn); // D
    AssertNotNull(vCompany2);
    vCompany2.Name := 'vCompany2';
    // D -> A
    vCompany2.AddEmployee(vPerson);
    // D -> B
    vCompany2.AddSubsidiary(FOwner);
    AssertNotNull(vCompany2);
    AssertNotNull(vProject1);
    // D -> C
    vCompany2.AddProject(vProject1);

    vCategory := TCategory.Create(FConn); // F
    AssertNotNull(vCategory);
    vCategory.Name := 'vCategory';
    // E -> F
    vPerson2.Category := vCategory;

    AssertEquals('vPerson.RefCount 1',
            3, vPerson.RefCount);
    AssertEquals('vPerson.RefByCount 1',
            2, vPerson.RefByCount);

    AssertEquals('FOwner.RefCount 1',
            3, FOwner.RefCount);
    AssertEquals('FOwner.RefByCount 1',
            2, FOwner.RefByCount);

    AssertEquals('FOwner.EmployeeCount 1',
            1, vPerson.Employer.EmployeeCount);

    AssertEquals('FOwner.ProjectCount 1',
            1, FOwner.ProjectCount);
    AssertEquals('vProject1.RefCount 1',
            3, vProject1.RefCount);
    AssertEquals('vProject1.RefByCount 1',
            2, vProject1.RefByCount);

    AssertEquals('vPerson2.RefCount 1',
            2, vPerson2.RefCount);
    AssertEquals('vPerson2.RefByCount 1',
            1, vPerson2.RefByCount);

    AssertEquals('vCategory.RefCount 1',
            2, vCategory.RefCount);
    AssertEquals('vCategory.RefByCount 1',
            1, vCategory.RefByCount);

    AssertEquals('vCompany2.RefCount 1',
            1, vCompany2.RefCount);
    AssertEquals('vCompany2.RefByCount 1',
            0, vCompany2.RefByCount);

    vPerson2.Free;   //E
    FOwner.Free;     //B
    try
      vPerson.Free;    //A
      vProject1.Free;  //C
  
      AssertEquals('vPerson.RefCount 2',
              2, vPerson.RefCount);
      AssertEquals('vPerson.RefByCount 2',
              2, vPerson.RefByCount);
  
      AssertEquals('FOwner.RefCount 2',
              2, FOwner.RefCount);
      AssertEquals('FOwner.RefByCount 2',
              2, FOwner.RefByCount);
  
      AssertEquals('FOwner.EmployeeCount 2',
              1, vPerson.Employer.EmployeeCount);
  
      AssertEquals('FOwner.ProjectCount 2',
              1, FOwner.ProjectCount);
      AssertEquals('vProject1.RefCount 2',
              2, vProject1.RefCount);
      AssertEquals('vProject1.RefByCount 2',
              2, vProject1.RefByCount);
  
      AssertEquals('vPerson2.RefCount 2',
              1, vPerson2.RefCount);
      AssertEquals('vPerson2.RefByCount 2',
              1, vPerson2.RefByCount);
  
      AssertEquals('vCategory.RefCount 2',
              2, vCategory.RefCount);
      AssertEquals('vCategory.RefByCount 2',
              1, vCategory.RefByCount);
  
      AssertEquals('vCompany2.RefCount 2',
              1, vCompany2.RefCount);
      AssertEquals('vCompany2.RefByCount 2',
              0, vCompany2.RefByCount);
    finally
      FOwner := nil;
    end;
  finally
    vCategory.Free;  //F
    vCompany2.Free;  //D
  end;
end;

// A -> <- B
// ^       ^
// +-- C --+
// Free order: A, B, C
procedure TestCircularReferences.TestCircularReferences7;
var
  vPerson: TPerson;
  vCompany2: TCompany;
begin
  vCompany2 := nil;  //C

  FOwner.Name := 'Owner'; // B

  vPerson := TPerson.Create(FConn); // A
  try
    AssertNotNull(vPerson);
    vPerson.Name := 'vPerson';

    // A -> B
    vPerson.Employer := FOwner;
    AssertNotNull(vPerson.Employer);
    AssertEquals('vPerson.Employer.Name A', 'Owner', vPerson.Employer.Name);
    // B -> A
    FOwner.AddEmployee(vPerson);

    vCompany2 := TCompany.Create(FConn); // C
    AssertNotNull(vCompany2);
    vCompany2.Name := 'vCompany2';
    // C -> A
    vCompany2.AddEmployee(vPerson);
    // C -> B
    vCompany2.AddSubsidiary(FOwner);
    AssertNotNull(vCompany2);

    AssertEquals('vPerson.RefCount 1',
            3, vPerson.RefCount);
    AssertEquals('vPerson.RefByCount 1',
            2, vPerson.RefByCount);

    AssertEquals('FOwner.RefCount 1',
            3, FOwner.RefCount);
    AssertEquals('FOwner.RefByCount 1',
            2, FOwner.RefByCount);

    AssertEquals('FOwner.EmployeeCount 1',
            1, vPerson.Employer.EmployeeCount);

    AssertEquals('vCompany2.RefCount 1',
            1, vCompany2.RefCount);
    AssertEquals('vCompany2.RefByCount 1',
            0, vCompany2.RefByCount);

  finally
    vPerson.Free;        //A
    FreeAndNil(FOwner);  //B
    vCompany2.Free;      //C
  end;
end;

// A -> B -> A
// |         ^
// +--> C ---+
// Free order: A, B, C
procedure TestCircularReferences.TestCircularReferences8;
var
  vPerson1: TPerson;
  vPerson2: TPerson;
begin
  vPerson2 := nil;  // C

  FOwner.Name := 'Employer';  // A

  vPerson1 := TPerson.Create(FConn);  // B
  try
    vPerson1.Name := 'vPerson1';
    // A -> B
    FOwner.AddEmployee(vPerson1);

    vPerson2 := TPerson.Create(FConn);
    vPerson2.Name := 'vPerson2';
    // A -> C
    FOwner.AddEmployee(vPerson2);

    // B -> A
    vPerson1.Employer := FOwner;

    // C -> A
    vPerson2.Employer := FOwner;

    AssertEquals('FOwner.RefCount 1',
            3, FOwner.RefCount);
    AssertEquals('FOwner.RefByCount 1',
            2, FOwner.RefByCount);

    AssertEquals('vPerson1.RefCount 1',
            2, vPerson1.RefCount);
    AssertEquals('vPerson1.RefByCount 1',
            1, vPerson1.RefByCount);

    AssertEquals('vPerson2.RefCount 1',
            2, vPerson2.RefCount);
    AssertEquals('vPerson2.RefByCount 1',
            1, vPerson2.RefByCount);

    FOwner.Free;    // A
    try
      AssertEquals('FOwner.RefCount 2',
              2, FOwner.RefCount);

      AssertEquals('vPerson1.RefCount 2',
              2, vPerson1.RefCount);

      AssertEquals('vPerson2.RefCount 2',
              2, vPerson2.RefCount);

      vPerson1.Free;       // B
      try
        AssertEquals('FOwner.RefCount 3',
                2, FOwner.RefCount);

        AssertEquals('vPerson1.RefCount 3',
                1, vPerson1.RefCount);

        AssertEquals('vPerson2.RefCount 3',
                2, vPerson2.RefCount);

        // This shouldn't raise AV because objects will be checked just after
        // being removed. If you have problem within this test, just
        // uncomment the following Exit call:

        // Exit;

        vPerson2.Free;       // C

        try
          AssertEquals('FOwner.RefCount 4',
                  0, FOwner.RefCount);

          AssertEquals('vPerson1.RefCount 4',
                  0, vPerson1.RefCount);

          AssertEquals('vPerson2.RefCount 4',
                  0, vPerson2.RefCount);
        finally
          vPerson2 := nil;
        end;

      finally
        vPerson1 := nil;
      end;

    finally
      FOwner := nil;
    end;

  finally
    FreeAndNil(FOwner);  // A
    vPerson1.Free;       // B
    vPerson2.Free;       // C
  end;
end;

// A -> A
procedure TestCircularReferences.TestCircularReferences9;
begin
  FOwner.AddSubsidiary(FOwner);
  try
    AssertEquals('FOwner.RefCount 1',
            2, FOwner.RefCount);
    AssertEquals('FOwner.RefByCount 1',
            1, FOwner.RefByCount);

    // This shouldn't raise AV because objects will be checked just after
    // being removed. If you have problem within this test, just
    // uncomment the following Exit call:

    // Exit;

    FOwner.Free;
    try
      AssertEquals('FOwner.RefCount 2',
              0, FOwner.RefCount);
    finally
      FOwner := nil;
    end;

  finally
    FreeAndNil(FOwner);
  end;
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestCircularReferences]);
{$ENDIF}

end.

