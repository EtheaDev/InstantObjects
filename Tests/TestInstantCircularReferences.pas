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

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantMock, InstantPersistence, TestModel,
  DUnitX.TestFramework;

type

  // For leak testing, run these tests in conjunction 
  // with a memory leak test utility.
  [TestFixture]
  TestCircularReferences = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantReferences: TInstantReferences;
    FCompany: TCompany;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
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

    // A {Refs}-> B {Parts}-> C {Parts}-> D {Parts}-> E {Ref}-> F
    //                                      {Parts}-> G {Parts}-> H {Ref}->F
    procedure TestCircularReferences10;
  end;

implementation

uses SysUtils, Classes, Windows;

procedure TestCircularReferences.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FCompany := TCompany.Create(FConn);
end;

procedure TestCircularReferences.TearDown;
begin
  FInstantReferences := nil;
  FreeAndNil(FCompany);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestCircularReferences.TestAddEmbeddedObject;
var
  vReturnValue: Integer;
  vReference: TPerson;
begin
  FInstantReferences := FCompany._Employees;

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
  end;
end;

procedure TestCircularReferences.TestAddExternalObject;
var
  vReturnValue: Integer;
  vReference: TProject;
begin
  FInstantReferences := FCompany._Projects;

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
  end;
end;

// A -> <- B -> C
procedure TestCircularReferences.TestCircularReferences;
var
  vPerson1: TPerson;
  vCategory: TCategory;
begin
  //FCompany is A

  vPerson1 := TPerson.Create(FConn);  // B
  try
    AssertNotNull(vPerson1);
    // B -> A
    vPerson1.Employer := FCompany;
    AssertNotNull(vPerson1.Employer);
    // A -> B
    FCompany.AddEmployee(vPerson1);
    AssertEquals('FCompany.EmployeeCount A', 1, FCompany.EmployeeCount);
  finally
    vPerson1.Free;  // B
  end;
  AssertEquals('FCompany.RefCount 1', 2, FCompany.RefCount);
  AssertEquals('FCompany.RefByCount 1', 1, FCompany.RefByCount);
  AssertEquals('FCompany.EmployeeCount 1', 1, FCompany.EmployeeCount);
  AssertEquals('FCompany.Employees[0].RefCount 1',
          1, FCompany.Employees[0].RefCount);
  AssertEquals('FCompany.Employees[0].RefByCount 1',
          1, FCompany.Employees[0].RefByCount);

  vCategory := TCategory.Create(FConn);  // C
  try
    AssertNotNull(vCategory);
    // B -> C
    FCompany.Employees[0].Category := vCategory;
    AssertNotNull(FCompany.Employees[0].Category);
  finally
    vCategory.Free;  // C
  end;
  AssertEquals('FCompany.RefCount 2', 2, FCompany.RefCount);
  AssertEquals('FCompany.RefByCount 2', 1, FCompany.RefByCount);

  AssertEquals('FCompany.Employees[0].RefCount 2',
          1, FCompany.Employees[0].RefCount);
  AssertEquals('FCompany.Employees[0].RefByCount 2',
          1, FCompany.Employees[0].RefByCount);

  AssertEquals('FCompany.Employees[0].Category.RefCount 1',
          1, FCompany.Employees[0].Category.RefCount);
  AssertEquals('FCompany.Employees[0].Category.RefByCount 1',
          1, FCompany.Employees[0].Category.RefByCount);
end;

// A -> B {Parts}-> C -> A
procedure TestCircularReferences.TestCircularReferences1;
var
  vPerson: TPerson;
  vProject: TProject;
  vAddress: TExternalAddress;
begin
  vPerson := TPerson.Create(FConn);  // A
  try
    AssertNotNull(vPerson);

    vProject := TProject.Create(FConn);  // B
    try
      AssertNotNull(vProject);
      vAddress := TExternalAddress.Create(FConn);  // C
      try
        AssertNotNull(vAddress);
        // C -> A
        vAddress.Site_Contact := vPerson;
        AssertNotNull(vAddress.Site_Contact);
        // B -> C
        vProject.AddAddress(vAddress);
        AssertEquals('vProject.AddressCount 1', 1, vProject.AddressCount);
      except
        vAddress.Free;  // C
      end;
      // A -> B
      vPerson.AddProject(vProject);
      AssertEquals('vPerson.ProjectCount 1', 1, vPerson.ProjectCount);
    finally
      vProject.Free;  // B
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
    vPerson.Free;  // A
  end;
end;

procedure TestCircularReferences.TestCircularReferences10;
var
  vCountry: TCountry;
  vMasterProjectBox: TProjectBox;
  vProjectBox: TProjectBox;
begin
  // Part I: create and store the object that is going to be referenced by
  // two other objects.
  vCountry := TCountry.Create(FConn);
  try
    vCountry.Id := 'SharedCountry';
    vCountry.Store;
  finally
    AssertEquals('vCountry.RefCount', 1, vCountry.RefCount);
    vCountry.Free;
  end;

  // Part II: build the referencing structure.
  vMasterProjectBox := TProjectBox.Create(FConn); // create Category
  try
    vProjectBox := TProjectBox.Create(FConn); // create Element
    try
      vMasterProjectBox.AddRelatedProjectBox(vProjectBox); // add Element to Category

      vProjectBox.Project := TProject.Create(FConn); // add master Form

      vProjectBox.Project.AddSubProject(TProject.Create(FConn)); // add detail Form

      vProjectBox.Project.SubProjects[0].Items.AddItem(TProjectItem.Create(FConn)); // add TriggerLink
      vCountry := TCountry.Retrieve('SharedCountry', False, False, FConn);
      try
        AssertEquals('vCountry.RefCount', 1, vCountry.RefCount);
        vProjectBox.Project.SubProjects[0].Items.Items[0].Country := vCountry; // add reference to Trigger
        AssertEquals('vCountry.RefCount', 2, vCountry.RefCount);
      finally
        vCountry.Free;
        AssertEquals('vCountry.RefCount', 1, vCountry.RefCount);
      end;

      vProjectBox.Project.SubProjects[0].AddSubProject(TProject.Create(FConn)); // add detail-detail Form.

      vProjectBox.Project.SubProjects[0].SubProjects[0].Items.AddItem(TProjectItem.Create(FConn)); // add TriggerLink
      vCountry := TCountry.Retrieve('SharedCountry', False, False, FConn);
      try
        AssertEquals('vCountry.RefCount', 2, vCountry.RefCount);
        vProjectBox.Project.SubProjects[0].SubProjects[0].Items.Items[0].Country := vCountry; // add reference to Trigger
        AssertEquals('vCountry.RefCount', 3, vCountry.RefCount);
      finally
        vCountry.Free;
        AssertEquals('vCountry.RefCount', 2, vCountry.RefCount);
      end;

      vProjectBox.Store;
    finally
      vProjectBox.Free;
    end;
    vMasterProjectBox.Store;
  finally
    vMasterProjectBox.Free;
  end;
end;

// A -> B {Parts}-> C {Parts}-> D -> A
procedure TestCircularReferences.TestCircularReferences2;
var
  vPerson: TPerson;
  vProject: TProject;
  vSubProject: TProject;
  vAddress: TExternalAddress;
begin
  vPerson := TPerson.Create(FConn);  // A
  try
    AssertNotNull(vPerson);

    vProject := TProject.Create(FConn);  // B
    try
      AssertNotNull(vProject);

      vSubProject := TProject.Create(FConn);  // C
      try
        AssertNotNull(vSubProject);
        vAddress := TExternalAddress.Create(FConn);  // D
        try
          AssertNotNull(vAddress);
          // D -> A
          vAddress.Site_Contact := vPerson;
          AssertNotNull(vAddress.Site_Contact);
          // C -> D
          vSubProject.AddAddress(vAddress);
          AssertEquals('vSubProject.AddressCount 1',
                  1, vSubProject.AddressCount);
        except
          vAddress.Free;  // D
          raise;
        end;
        // B -> C
        vProject.AddSubProject(vSubProject);
        AssertEquals('vProject.SubProjectCount 1', 1, vProject.SubProjectCount);
      except
        vSubProject.Free;  // C
        raise;
      end;
      // A -> B
      vPerson.AddProject(vProject);
      AssertEquals('vPerson.ProjectCount 1', 1, vPerson.ProjectCount);
    finally
      vProject.Free;  // B
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
  // FCompany is A

  vPerson1 := TPerson.Create(FConn);  // B
  try
    AssertNotNull(vPerson1);
    // B -> A
    vPerson1.Employer := FCompany;
    AssertNotNull(vPerson1.Employer);
    // A -> B
    FCompany.AddEmployee(vPerson1);
    AssertEquals('FCompany.EmployeeCount 1', 1, FCompany.EmployeeCount);
  finally
    vPerson1.Free;  // B
  end;
  AssertEquals('FCompany.RefCount 1', 2, FCompany.RefCount);
  AssertEquals('FCompany.RefByCount 1', 1, FCompany.RefByCount);
  AssertEquals('FCompany.EmployeeCount 2', 1, FCompany.EmployeeCount);
  AssertEquals('FCompany.Employees[0].RefCount 1',
          1, FCompany.Employees[0].RefCount);
  AssertEquals('FCompany.Employees[0].RefByCount 1',
          1, FCompany.Employees[0].RefByCount);

  vPerson2 := TPerson.Create(FConn);  // C
  try
    AssertNotNull(vPerson2);
    // A -> C
    FCompany.AddEmployee(vPerson2);
    AssertEquals('FCompany.EmployeeCount 3', 2, FCompany.EmployeeCount);
  finally
    vPerson2.Free;  // C
  end;
  AssertEquals('FCompany.RefCount 2', 2, FCompany.RefCount);
  AssertEquals('FCompany.RefByCount 2', 1, FCompany.RefByCount);

  AssertEquals('FCompany.EmployeeCount 4', 2, FCompany.EmployeeCount);
  AssertEquals('FCompany.Employees[1].RefCount 1',
          1, FCompany.Employees[1].RefCount);
  AssertEquals('FCompany.Employees[1].RefByCount 1',
          1, FCompany.Employees[1].RefByCount);

  FCompany.DeleteEmployee(1);
  AssertEquals('FCompany.EmployeeCount 5', 1, FCompany.EmployeeCount);
end;

// A -> B -> C -> A
//      |
//      + -> D
// then delete D
procedure TestCircularReferences.TestCircularReferences4;
var
  vPerson: TPerson;
  vProject1: TProject;
  vProject2: TProject;
begin
  // FCompany is B

  vPerson := TPerson.Create(FConn);  // A
  try
    AssertNotNull(vPerson);
    // A -> B
    vPerson.Employer := FCompany;
    AssertNotNull(vPerson.Employer);

    vProject1 := TProject.Create(FConn);  // C
    try
      AssertNotNull(vProject1);
      // C -> A
      vProject1.Manager := vPerson;
      AssertNotNull(vProject1.Manager);
      // B -> C
      FCompany.AddProject(vProject1);
      AssertEquals('FCompany.ProjectCount 1', 1, FCompany.ProjectCount);
    finally
      vProject1.Free;  // C
    end;

    vProject2 := TProject.Create(FConn);  // D
    try
      AssertNotNull(vProject2);
       // B -> D
      FCompany.AddProject(vProject2);
      AssertEquals('FCompany.ProjectCount 2', 2, FCompany.ProjectCount);
    finally
      vProject2.Free;  // D
    end;

    FreeAndNil(FCompany);  // B

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
    vPerson.Free;  // A
  end;
end;

// A -> B -> C -> A
//      |
//      + -> D -> E
// then delete E
procedure TestCircularReferences.TestCircularReferences5;
var
  vPerson: TPerson;
  vProject1: TProject;
  vProject2: TProject;
  vPerson2: TPerson;
begin
  //FCompany is B

  vPerson := TPerson.Create(FConn); // A
  try
    AssertNotNull(vPerson);
    vPerson.Name := 'vPerson';

    // A -> B
    vPerson.Employer := FCompany;
    AssertNotNull(vPerson.Employer);

    vProject1 := TProject.Create(FConn); // C
    try
      AssertNotNull(vProject1);
      // C -> A
      vProject1.Manager := vPerson;
      AssertNotNull(vProject1.Manager);
      // B -> C
      FCompany.AddProject(vProject1);
      AssertEquals('FCompany.ProjectCount 1', 1, FCompany.ProjectCount);
    finally
      vProject1.Free;  // C
    end;

    vProject2 := TProject.Create(FConn);  // D
    try
      AssertNotNull(vProject2);
      vPerson2 := TPerson.Create(FConn);  // E
      try
        AssertNotNull(vPerson2);
        // D -> E
        vProject2.Manager := vPerson2;
        AssertNotNull(vProject2.Manager);
      finally
        vPerson2.Free;  // E
      end;
      // B -> D
      FCompany.AddProject(vProject2);
      AssertEquals('FCompany.ProjectCount 2', 2, FCompany.ProjectCount);
    finally
      vProject2.Free;  // D
    end;

    FreeAndNil(FCompany);  // B

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
  vCompany2 := nil;  //D
  vCategory := nil;  //F

  vPerson := TPerson.Create(FConn); // A
  try
    AssertNotNull(vPerson);

    // A -> B
    vPerson.Employer := FCompany;
    AssertNotNull(vPerson.Employer);

    vProject1 := TProject.Create(FConn); // C
    AssertNotNull(vProject1);
    // B -> C
    FCompany.AddProject(vProject1);
    AssertEquals('FCompany.ProjectCount 1', 1, FCompany.ProjectCount);

    vPerson2 := TPerson.Create(FConn); // E
    AssertNotNull(vPerson2);
    // B -> E
    FCompany.AddEmployee(vPerson2);
    AssertEquals('FCompany.EmployeeCount 1', 1, FCompany.EmployeeCount);

    // C -> A
    vProject1.Manager := vPerson;

    vCompany2 := TCompany.Create(FConn); // D
    AssertNotNull(vCompany2);
    // D -> A
    vCompany2.AddEmployee(vPerson);
    AssertEquals('vCompany2.EmployeeCount 1', 1, vCompany2.EmployeeCount);
    // D -> B
    vCompany2.AddSubsidiary(FCompany);
    AssertEquals('vCompany2.SubsidiaryCount', 1, vCompany2.SubsidiaryCount);
    // D -> C
    vCompany2.AddProject(vProject1);
    AssertEquals('vCompany2.ProjectCount', 1, vCompany2.ProjectCount);

    vCategory := TCategory.Create(FConn); // F
    AssertNotNull(vCategory);
    // E -> F
    vPerson2.Category := vCategory;
    AssertNotNull(vPerson2.Category);

    AssertEquals('vPerson.RefCount 1',
            3, vPerson.RefCount);
    AssertEquals('vPerson.RefByCount 1',
            2, vPerson.RefByCount);

    AssertEquals('FCompany.RefCount 1',
            3, FCompany.RefCount);
    AssertEquals('FCompany.RefByCount 1',
            2, FCompany.RefByCount);

    AssertEquals('FCompany.EmployeeCount 2',
            1, vPerson.Employer.EmployeeCount);

    AssertEquals('FCompany.ProjectCount 2',
            1, FCompany.ProjectCount);
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
    FCompany.Free;     //B
    try
      vPerson.Free;    //A
      vProject1.Free;  //C
  
      AssertEquals('vPerson.RefCount 2',
              2, vPerson.RefCount);
      AssertEquals('vPerson.RefByCount 2',
              2, vPerson.RefByCount);
  
      AssertEquals('FCompany.RefCount 2',
              2, FCompany.RefCount);
      AssertEquals('FCompany.RefByCount 2',
              2, FCompany.RefByCount);

      AssertEquals('FCompany.EmployeeCount 2',
              1, vPerson.Employer.EmployeeCount);
  
      AssertEquals('FCompany.ProjectCount 2',
              1, FCompany.ProjectCount);
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
      FCompany := nil;
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

  // FCompany is B

  vPerson := TPerson.Create(FConn); // A
  try
    AssertNotNull(vPerson);

    // A -> B
    vPerson.Employer := FCompany;
    AssertNotNull(vPerson.Employer);
    // B -> A
    FCompany.AddEmployee(vPerson);
    AssertEquals('FCompany.EmployeeCount 1', 1, FCompany.EmployeeCount);

    vCompany2 := TCompany.Create(FConn); // C
    AssertNotNull(vCompany2);
    // C -> A
    vCompany2.AddEmployee(vPerson);
    AssertEquals('vCompany2.EmployeeCount 1', 1, vCompany2.EmployeeCount);
    // C -> B
    vCompany2.AddSubsidiary(FCompany);
    AssertEquals('vCompany2.SubsidiaryCount 1', 1, vCompany2.SubsidiaryCount);

    AssertEquals('vPerson.RefCount 1',
            3, vPerson.RefCount);
    AssertEquals('vPerson.RefByCount 1',
            2, vPerson.RefByCount);

    AssertEquals('FCompany.RefCount 1',
            3, FCompany.RefCount);
    AssertEquals('FCompany.RefByCount 1',
            2, FCompany.RefByCount);

    AssertEquals('FCompany.EmployeeCount 2',
            1, vPerson.Employer.EmployeeCount);

    AssertEquals('vCompany2.RefCount 1',
            1, vCompany2.RefCount);
    AssertEquals('vCompany2.RefByCount 1',
            0, vCompany2.RefByCount);

  finally
    vPerson.Free;        //A
    FreeAndNil(FCompany);  //B   Free here is part of test specification
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
  // FCompany is A

  vPerson1 := TPerson.Create(FConn);  // B
  try
    AssertNotNull(vPerson1);
    // A -> B
    FCompany.AddEmployee(vPerson1);
    AssertEquals('FCompany.EmployeeCount 1', 1, FCompany.EmployeeCount);

    vPerson2 := TPerson.Create(FConn);  // C
    AssertNotNull(vPerson2);
    // A -> C
    FCompany.AddEmployee(vPerson2);
    AssertEquals('FCompany.EmployeeCount 2', 2, FCompany.EmployeeCount);

    // B -> A
    vPerson1.Employer := FCompany;
    AssertNotNull(vPerson1.Employer);

    // C -> A
    vPerson2.Employer := FCompany;
    AssertNotNull(vPerson2.Employer);

    AssertEquals('FCompany.RefCount 1',
            3, FCompany.RefCount);
    AssertEquals('FCompany.RefByCount 1',
            2, FCompany.RefByCount);

    AssertEquals('vPerson1.RefCount 1',
            2, vPerson1.RefCount);
    AssertEquals('vPerson1.RefByCount 1',
            1, vPerson1.RefByCount);

    AssertEquals('vPerson2.RefCount 1',
            2, vPerson2.RefCount);
    AssertEquals('vPerson2.RefByCount 1',
            1, vPerson2.RefByCount);

    FCompany.Free;    // A
    try
      AssertEquals('FCompany.RefCount 2',
              2, FCompany.RefCount);
      AssertEquals('FCompany.RefByCount 2',
              2, FCompany.RefByCount);

      AssertEquals('vPerson1.RefCount 2',
              2, vPerson1.RefCount);
      AssertEquals('vPerson1.RefByCount 2',
              1, vPerson1.RefByCount);

      AssertEquals('vPerson2.RefCount 2',
              2, vPerson2.RefCount);
      AssertEquals('vPerson2.RefByCount 2',
              1, vPerson2.RefByCount);

      vPerson1.Free;       // B
      try
        AssertEquals('FCompany.RefCount 3',
                2, FCompany.RefCount);
        AssertEquals('FCompany.RefByCount 3',
                2, FCompany.RefByCount);

        AssertEquals('vPerson1.RefCount 3',
                1, vPerson1.RefCount);
        AssertEquals('vPerson1.RefByCount 3',
                1, vPerson1.RefByCount);

        AssertEquals('vPerson2.RefCount 3',
                2, vPerson2.RefCount);
        AssertEquals('vPerson2.RefByCount 3',
                1, vPerson2.RefByCount);

        vPerson2.Free;       // C
      finally
        vPerson1 := nil;
      end;

    finally
      FCompany := nil;
    end;

  finally
    vPerson1.Free;       // B
  end;
end;

// A -> A
procedure TestCircularReferences.TestCircularReferences9;
begin
  // FCompany is A

  FCompany.AddSubsidiary(FCompany);
  AssertEquals('FCompany.SubsidiaryCount', 1, FCompany.SubsidiaryCount);
  AssertEquals('FCompany.RefCount',
          2, FCompany.RefCount);
  AssertEquals('FCompany.RefByCount',
          1, FCompany.RefByCount);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestCircularReferences]);
{$ENDIF}

end.

