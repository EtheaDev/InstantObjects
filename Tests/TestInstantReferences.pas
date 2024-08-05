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

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantMock, InstantPersistence, TestModel,
  DUnitX.TestFramework;

type
  [TestFixture]
  TestTInstantEmbReferences = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantReferences: TInstantReferences;
    FOwner: TCompany;
    function RefsEmbeddedCompare(Holder, Obj1, Obj2: TInstantObject): Integer;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAdd;
    procedure TestAddReference;
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

  [TestFixture]
  TestTInstantExtReferences = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantReferences: TInstantReferences;
    FOwner: TCompany;
    function RefsExternalCompare(Holder, Obj1, Obj2: TInstantObject): Integer;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAdd;
    procedure TestAddReference;
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

uses
  SysUtils, Windows, Classes, InstantClasses, InstantMetadata;

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

procedure TestTInstantEmbReferences.TestAddReference;
begin
  assert.WillRaise(
    procedure begin
      FInstantReferences.AddReference('TPerson', 'NewPersonId');
    end,
    EInstantError,
    'Exception was not thrown for InstantReferences.AddReference'
  );
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

procedure TestTInstantExtReferences.TestAddReference;
var
  vReturnValue: Integer;
begin
  FInstantReferences.Unchanged;
  AssertFalse(FInstantReferences.IsChanged);
  vReturnValue := FInstantReferences.AddReference('TProject', 'NewProjectId');
  AssertTrue(vReturnValue <> -1);
  AssertTrue(FInstantReferences.IsChanged);
  AssertEquals(4, FInstantReferences.Count);
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
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantExtReferences,
                TestTinstantEmbReferences]);
{$ENDIF}

end.
 
