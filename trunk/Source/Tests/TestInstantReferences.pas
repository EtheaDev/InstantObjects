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
//  FInstantParts.Clear;
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


initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([//TestTInstantExtReferences,
                //TestTinstantEmbReferences,
                TestTInstantReferences_Leak]);
{$ELSE}
  RegisterTests([//TestTInstantExtReferences,
                //TestTinstantEmbReferences,
                TestTInstantReferences_Leak
                ]);
{$ENDIF}

end.
 