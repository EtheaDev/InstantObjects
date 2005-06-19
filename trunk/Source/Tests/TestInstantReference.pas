(*
 *   InstantObjects
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
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestInstantReference;

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantReference
  TestTInstantReference = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantReference: TInstantReference;
    FOwner: TContact;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestDestroyObject_HasReference_HasValue;
    procedure TestLoadObjectFromStream;
    procedure TestObjectClass_ObjectClassName_ObjectId;
    procedure TestReferenceObject;
    procedure TestReferenceObject1;
    procedure TestReset;
  end;

implementation

uses SysUtils, Classes, InstantClasses, testregistry;

procedure TestTInstantReference.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantReference := FOwner._Category;
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
  vPart: TCategory;
begin
  vPart := nil; 

  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  vAttrMetadata.AttributeClass := TInstantReference;
  vSource := TInstantReference.Create(FOwner, vAttrMetadata);
  try
    vPart := TCategory.Create(FConn);
    FInstantReference.Value := vPart;
    AssertTrue('Value HasVal', FInstantReference.HasValue);

    AssertFalse('vSource HasVal', vSource.HasValue);
    vSource.Assign(FInstantReference);
    AssertTrue('Assign HasVal', vSource.HasValue);
    AssertNotSame(vSource, FInstantReference.Value);
  finally
    vPart.Free;
    vSource.Free;
    vAttrMetadata.Free;
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
      AssertTrue('LoadObjectFromStream HasVal', FInstantReference.HasValue);
      AssertTrue('LoadObjectFromStream HasRef', FInstantReference.HasReference);
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

procedure TestTInstantReference.TestReferenceObject;
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

procedure TestTInstantReference.TestReferenceObject1;
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

  vObj := TCategory.Create(FConn);
  try
    FInstantReference.Value := vObj;
    AssertTrue(FInstantReference.HasReference);
    AssertTrue(FInstantReference.HasValue);
    AssertSame(vObj, FInstantReference.Value);

    FInstantReference.Reset;
    AssertFalse('Final HasRef', FInstantReference.HasReference);
    AssertFalse('Final HasVal', FInstantReference.HasValue);
  finally
    vObj.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantReference]);
{$ENDIF}

end.
 