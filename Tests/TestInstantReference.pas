(*
 *   InstantObjects Test Suite
 *   TestInstantReference
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
 * The Original Code is: InstantObjects Test Suite/TestInstantReference
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

unit TestInstantReference;

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantReference
  [TestFixture]
  TestTInstantReference = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantReference: TInstantReference;
    FOwner: TContact;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAssign;
    procedure TestAttach_DetachObject;
    procedure TestDestroyObject_HasReference_HasValue;
    procedure TestHasValue;
    procedure TestIsChanged;
    procedure TestLoadObjectFromStream;
    procedure TestObjectClass_ObjectClassName_ObjectId;
    procedure TestReferenceObject_Class;
    procedure TestReferenceObject_ClassName;
    procedure TestReset;
    procedure TestSaveObjectTo_FromStream;
  end;

implementation

uses SysUtils, Classes, InstantClasses, InstantMetadata;

procedure TestTInstantReference.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantReference := FOwner._Category;
  FInstantReference.UnChanged;
end;

procedure TestTInstantReference.TearDown;
begin
  FInstantReference := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantReference.TestAssign;
var
  vSource: TInstantReference;
  vAttrMetadata: TInstantAttributeMetadata;
  vCategory: TCategory;
begin
  vCategory := nil;

  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  vAttrMetadata.AttributeClass := TInstantReference;
  vSource := TInstantReference.Create(FOwner, vAttrMetadata);
  try
    vCategory := TCategory.Create(FConn);
    AssertEquals(0, vCategory.RefByCount);
    FInstantReference.Value := vCategory;
    AssertEquals(1, vCategory.RefByCount);
    AssertTrue('Value HasVal', FInstantReference.HasValue);
    AssertTrue('Value HasReference', FInstantReference.HasReference);

    AssertFalse('vSource HasVal', vSource.HasValue);
    vSource.Assign(FInstantReference);
    AssertEquals(2, vCategory.RefByCount);
    AssertEquals(3, vCategory.RefCount);
    AssertTrue('Assign HasVal', vSource.HasValue);
    AssertTrue('Assign HasReference', vSource.HasReference);
    AssertNotSame(vSource, FInstantReference.Value);
  finally
    vCategory.Free;
    vSource.Free;
    vAttrMetadata.Free;
  end;
end;

procedure TestTInstantReference.TestAttach_DetachObject;
var
  vReturnValue: Boolean;
  vObject: TCategory;
begin
  vObject := TCategory.Create(FConn);
  try
    vObject.Id := 'Object.Id';
    AssertEquals('Object RefCount 1', 1, vObject.RefCount);
  
    vReturnValue := FInstantReference.AttachObject(vObject);
    AssertTrue('AttachObject', vReturnValue);
    AssertSame(vObject, FInstantReference.Value);
    AssertTrue('HasReference 1', FInstantReference.HasReference);
    AssertTrue('HasValue 1', FInstantReference.HasValue);
    AssertEquals('Value RefCount 1', 2, FInstantReference.Value.RefCount);
    AssertEquals('Object RefCount 2', 2, vObject.RefCount);
    AssertEquals('Value.Id 1', 'Object.Id', FInstantReference.Value.Id);

    vReturnValue := FInstantReference.DetachObject(vObject);
    AssertTrue('DetachObject', vReturnValue);
    AssertFalse('HasReference 2', FInstantReference.HasReference);
    AssertFalse('HasValue 2', FInstantReference.HasValue);
  finally
    vObject.Free;
  end;
end;

procedure TestTInstantReference.TestDestroyObject_HasReference_HasValue;
var
  vObj: TCategory;
begin
  AssertTrue('Initial HasRef', FInstantReference.HasReference);
  AssertFalse('Initial HasVal', FInstantReference.HasValue);

  vObj := TCategory.Create(FConn);
  try
    FInstantReference.Value := vObj;
    AssertTrue('Value HasRef', FInstantReference.HasReference);
    AssertTrue('Value HasVal', FInstantReference.HasValue);
    AssertSame(vObj, FInstantReference.Value);

    FInstantReference.DestroyObject;
    AssertFalse('DestroyObject HasVal', FInstantReference.HasValue);
    AssertFalse('DestroyObject HasRef', FInstantReference.HasReference);
  finally
    vObj.Free;
  end;
end;

procedure TestTInstantReference.TestHasValue;
begin
  AssertFalse(FInstantReference.HasValue);
end;

procedure TestTInstantReference.TestIsChanged;
var
  vObject: TCategory;
begin
  AssertFalse('Initial IsChanged', FInstantReference.IsChanged);

  vObject := TCategory.Create(FConn);
  try
    AssertNotNull('Create object is nil', vObject);
    FInstantReference.Value := vObject;
    AssertTrue('IsChanged False after Value assignment', FInstantReference.IsChanged);

    FInstantReference.Unchanged;
    AssertFalse(FInstantReference.IsChanged);
    vObject.Changed;
    AssertFalse('IsChanged True after referenced object changed',
        FInstantReference.IsChanged);
  finally
    vObject.Free;
  end;
end;

procedure TestTInstantReference.TestLoadObjectFromStream;
var
  vObject: TCategory;
  vReturnValue: Boolean;
  vStream: TStream;
begin
  vObject := TCategory.Create(FConn);
  try
    AssertNotNull('Create object', vObject);
    AssertEquals('Object RefCount 1', 1, vObject.RefCount);
    vReturnValue := FInstantReference.AttachObject(vObject);
    AssertTrue('AttachObject', vReturnValue);
    AssertTrue('AttachObject HasVal', FInstantReference.HasValue);
    AssertTrue('AttachObject HasRef', FInstantReference.HasReference);
    AssertEquals('Object RefCount 2', 2, vObject.RefCount);
    vObject.Store;

    vStream := TInstantStream.Create;
    try
      FInstantReference.SaveObjectToStream(vStream);
      AssertTrue('vStream.Size check', vStream.Size > 0);
      FInstantReference.Value := nil;
      AssertEquals('Object RefCount 3', 1, vObject.RefCount);
      AssertFalse('Value HasVal', FInstantReference.HasValue);
      AssertFalse('Value HasRef', FInstantReference.HasReference);

      vStream.Position := 0;
      FInstantReference.LoadObjectFromStream(vStream);
      AssertTrue('LoadObjectFromStream HasRef', FInstantReference.HasReference);
      AssertTrue('LoadObjectFromStream HasVal', FInstantReference.HasValue);
      AssertEquals('LoadObjectFromStream RefCount', 1,
          FInstantReference.Value.RefCount);
      AssertNotSame('Same Object??', vObject, FInstantReference.Value);
      // Not the same object, so free it.
      AssertEquals('Object RefCount 4', 1, vObject.RefCount);
    finally
      vStream.Free;
    end;
  finally
    vObject.Free;
  end;
end;

procedure TestTInstantReference.TestObjectClass_ObjectClassName_ObjectId;
var
  vObject: TCategory;
  vReturnValue: Boolean;
begin
  vObject := TCategory.Create(FConn);
  try
    AssertNotNull('Create object', vObject);
    vObject.Id := 'Object.Id';
    vReturnValue := FInstantReference.AttachObject(vObject);
    AssertTrue('AttachObject', vReturnValue);

    AssertEquals('ObjectClass', TCategory, FInstantReference.ObjectClass);
    AssertEquals('ObjectClassName', 'TCategory', FInstantReference.ObjectClassName);
    AssertEquals('ObjectId', 'Object.Id', FInstantReference.ObjectId);
  finally
    vObject.Free;
  end;
end;

procedure TestTInstantReference.TestReferenceObject_Class;
var
  vObjectId: string;
  vObjectClass: TInstantObjectClass;
begin
  AssertTrue('Initial HasRef', FInstantReference.HasReference);
  AssertFalse('Initial HasVal', FInstantReference.HasValue);

  vObjectId := 'ObjectId';
  vObjectClass := TCategory;
  FInstantReference.ReferenceObject(vObjectClass, vObjectId);
  AssertTrue('Final HasRef', FInstantReference.HasReference);
  AssertFalse('Final HasVal', FInstantReference.HasValue);
end;

procedure TestTInstantReference.TestReferenceObject_ClassName;
var
  vObjectId: string;
  vObjectClassName: string;
begin
  AssertTrue('Initial HasRef', FInstantReference.HasReference);
  AssertFalse('Initial HasVal', FInstantReference.HasValue);

  vObjectId := 'ObjectId';
  vObjectClassName := 'TCategory';
  FInstantReference.ReferenceObject(vObjectClassName, vObjectId);
  AssertTrue('Final HasRef', FInstantReference.HasReference);
  AssertFalse('Final HasVal', FInstantReference.HasValue);
end;

procedure TestTInstantReference.TestReset;
var
  vObj: TCategory;
begin
  AssertTrue('Initial HasRef', FInstantReference.HasReference);
  AssertFalse('Initial HasVal', FInstantReference.HasValue);
  AssertFalse('Initial IsChanged', FInstantReference.IsChanged);
  AssertFalse('Initial IsDefault', FInstantReference.IsDefault);
  FInstantReference.Reset;
  AssertTrue('IsDefault after Reset', FInstantReference.IsDefault);
  AssertTrue('IsChanged True after initial Reset',
      FInstantReference.IsChanged);

  vObj := TCategory.Create(FConn);
  try
    FInstantReference.Value := vObj;
    AssertTrue(FInstantReference.HasReference);
    AssertTrue(FInstantReference.HasValue);
    AssertSame(vObj, FInstantReference.Value);
    AssertTrue('IsChanged 2 is False!', FInstantReference.IsChanged);
    FInstantReference.UnChanged;

    FInstantReference.Reset;
    AssertFalse('Final HasRef', FInstantReference.HasReference);
    AssertFalse('Final HasVal', FInstantReference.HasValue);
    AssertTrue('Final IsChanged', FInstantReference.IsChanged);
  finally
    vObj.Free;
  end;
end;

procedure TestTInstantReference.TestSaveObjectTo_FromStream;
var
  vObject: TCategory;
  vReturnValue: Boolean;
  vStream: TStream;
begin
  vObject := TCategory.Create(FConn);
  try
    AssertNotNull('Create object', vObject);
    AssertEquals('Object RefCount 1', 1, vObject.RefCount);
    vReturnValue := FInstantReference.AttachObject(vObject);
    AssertTrue('AttachObject', vReturnValue);
    AssertTrue('HasValue 1', FInstantReference.HasValue);
    AssertTrue('HasReference 1', FInstantReference.HasReference);
    AssertEquals('Value RefCount 1', 2, FInstantReference.Value.RefCount);
    AssertEquals('Object RefCount 2', 2, vObject.RefCount);

    vStream := TInstantStream.Create;
    try
      FInstantReference.SaveObjectToStream(vStream);
      AssertTrue('vStream.Size check', vStream.Size > 0);
      FInstantReference.Value := nil;
      AssertFalse('HasValue 2', FInstantReference.HasValue);
      AssertFalse('HasReference 2', FInstantReference.HasReference);
      vStream.Position := 0;
      FInstantReference.LoadObjectFromStream(vStream);
      AssertTrue('HasValue 3', FInstantReference.HasValue);
      AssertTrue('HasReference 3', FInstantReference.HasReference);
      AssertEquals('Value RefCount 2', 1, FInstantReference.Value.RefCount);
    finally
      vStream.Free;
    end;
  finally
    vObject.Free;
  end;
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantReference]);
{$ENDIF}

end.
 