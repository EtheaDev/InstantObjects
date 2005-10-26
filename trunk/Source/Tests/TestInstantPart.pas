(*
 *   InstantObjects Test Suite
 *   TestInstantPart
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
 * The Original Code is: InstantObjects Test Suite/TestInstantPart
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

unit TestInstantPart;

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantPart
  TestTInstantEmbPart = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantPart: TInstantPart;
    FOwner: TContact;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAllowOwned;
    procedure TestAttach_DetachObject;
    procedure TestIsChanged;
    procedure TestIsDefault;
    procedure TestHasValue;
    procedure TestSaveObjectTo_FromStream;
    procedure TestUnchanged;
    procedure TestValue_Reset;
  end;

  TestTInstantExtPart = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantPart: TInstantPart;
    FOwner: TContact;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAllowOwned;
    procedure TestAssign;
    procedure TestAttach_DetachObject;
    procedure TestHasValue;
    procedure TestIsChanged;
    procedure TestIsDefault;
    procedure TestSaveObjectTo_FromStream;
    procedure TestUnchanged;
    procedure TestValue_Reset;
  end;

implementation

uses SysUtils, Classes, InstantClasses, testregistry;

procedure TestTInstantEmbPart.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantPart := FOwner._Address;
end;

procedure TestTInstantEmbPart.TearDown;
begin
  FInstantPart := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantEmbPart.TestAssign;
var
  vSource: TInstantPart;
  vAttrMetadata: TInstantAttributeMetadata;
  vPart: TAddress;
  vCountry: TCountry;
begin
  vSource := nil;

  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  try
    vAttrMetadata.AttributeClass := TInstantPart;
    vSource := TInstantPart.Create(FOwner, vAttrMetadata);
    vPart := TAddress.Create(FConn);
    FInstantPart.Value := vPart;
    AssertTrue('Value HasVal', FInstantPart.HasValue);
    AssertEquals(1, FInstantPart.Value.RefCount);
    AssertEquals(1, vPart.RefCount);

    // Added this to help check for possible memory leakage
    vCountry := TCountry.Create(FConn);
    try
      TAddress(FInstantPart.Value).Country := vCountry;
    finally
      vCountry.Free;
    end;
    AssertEquals(1, TAddress(FInstantPart.Value).Country.RefCount);

    AssertFalse('vSource HasVal', vSource.HasValue);
    AssertNotSame(FInstantPart, vSource);
    vSource.Assign(FInstantPart);
    AssertTrue('vSource HasVal', vSource.HasValue);
    AssertNotSame(FInstantPart.Value, vSource.Value);
    AssertSame(TAddress(FInstantPart.Value).Country,
        TAddress(vSource.Value).Country);
  finally
    vSource.Free;
    vAttrMetadata.Free;
  end;
end;

procedure TestTInstantEmbPart.TestAllowOwned;
begin
  AssertFalse(FInstantPart.AllowOwned);
end;

procedure TestTInstantEmbPart.TestAttach_DetachObject;
var
  vReturnValue: Boolean;
  vObject: TAddress;
begin
  vObject := TAddress.Create(FConn);
  vObject.Id := 'Object.Id';
  AssertEquals('Object RefCount 1', 1, vObject.RefCount);

  vReturnValue := FInstantPart.AttachObject(vObject);
  AssertTrue('AttachObject', vReturnValue);
  AssertSame(vObject, FInstantPart.Value);
  AssertTrue('FInstantElement HasValue', FInstantPart.HasValue);
  AssertEquals('Value RefCount 1', 1, FInstantPart.Value.RefCount);
  AssertEquals('Object RefCount 2', 1, vObject.RefCount);
  AssertEquals('Value.Id 1', 'Object.Id', FInstantPart.Value.Id);

  vReturnValue := FInstantPart.DetachObject(vObject);
  AssertTrue('DetachObject', vReturnValue);
  AssertEquals('Value.Id 2', '', FInstantPart.Value.Id);
end;

procedure TestTInstantEmbPart.TestIsChanged;
var
  vPart: TAddress;
begin
  AssertFalse(FInstantPart.IsChanged);

  vPart := TAddress.Create(FConn);
  vPart.Changed;
  FInstantPart.Value := vPart;
  AssertTrue(FInstantPart.IsChanged);
end;

procedure TestTInstantEmbPart.TestIsDefault;
var
  vPart: TAddress;
begin
  AssertTrue(FInstantPart.IsDefault);

  vPart := TAddress.Create(FConn);
  vPart.Id := 'PartId';
  FInstantPart.Value := vPart;
  AssertFalse(FInstantPart.IsDefault);
end;

procedure TestTInstantEmbPart.TestHasValue;
begin
  AssertFalse(FInstantPart.HasValue);

  FInstantPart.Value := TAddress.Create(FConn);
  AssertTrue(FInstantPart.HasValue);
end;

procedure TestTInstantEmbPart.TestSaveObjectTo_FromStream;
var
  vObject: TAddress;
  vReturnValue: Boolean;
  vStream: TStream;
begin
  vObject := TAddress.Create(FConn);
  AssertNotNull('Create object', vObject);
  AssertEquals('Object RefCount 1', 1, vObject.RefCount);
  vReturnValue := FInstantPart.AttachObject(vObject);
  AssertTrue('AttachObject', vReturnValue);
  AssertTrue(FInstantPart.HasValue);
  AssertEquals('Value RefCount 1', 1, FInstantPart.Value.RefCount);
  AssertEquals('Object RefCount 2', 1, vObject.RefCount);

  vStream := TInstantStream.Create;
  try
    FInstantPart.SaveObjectToStream(vStream);
    AssertTrue('vStream.Size check', vStream.Size > 0);
    FInstantPart.Value := nil;
    AssertFalse(FInstantPart.HasValue);
    vStream.Position := 0;
    FInstantPart.LoadObjectFromStream(vStream);
    AssertTrue(FInstantPart.HasValue);
    AssertEquals('Value RefCount 2', 1, FInstantPart.Value.RefCount);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantEmbPart.TestUnchanged;
begin
  FInstantPart.Changed;
  AssertTrue(FInstantPart.IsChanged);
  FInstantPart.Unchanged;
  AssertFalse(FInstantPart.IsChanged);
end;

procedure TestTInstantEmbPart.TestValue_Reset;
var
  vFirstObj: TInstantObject;
  vSecondObj: TInstantObject;
begin
  AssertFalse('HasValue 1', FInstantPart.HasValue);
  AssertNotNull('AssertNotNull', FInstantPart.Value);
  AssertTrue('HasValue 2', FInstantPart.HasValue);
  vFirstObj := FInstantPart.Value;

  vSecondObj := TAddress.Create(FConn);
  vSecondObj.Id := 'PartId';
  FInstantPart.Value := vSecondObj;
  AssertEquals('Value.Id', 'PartId', FInstantPart.Value.Id);
  AssertNotSame('AssertNotSame', vFirstObj, FInstantPart.Value);

  FInstantPart.Reset;
  AssertFalse('HasValue 3', FInstantPart.HasValue);
end;

procedure TestTInstantExtPart.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantPart := FOwner._PartExternal;
end;

procedure TestTInstantExtPart.TearDown;
begin
  FInstantPart := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantExtPart.TestAllowOwned;
begin
  AssertTrue(FInstantPart.AllowOwned);
end;

procedure TestTInstantExtPart.TestAssign;
var
  vSource: TInstantPart;
  vAttrMetadata: TInstantAttributeMetadata;
  vPart: TPartExternal;
  vCategory: TCategory;
begin
  vSource := nil;

  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  try
    vAttrMetadata.AttributeClass := TInstantPart;
    vSource := TInstantPart.Create(FOwner, vAttrMetadata);
    vPart := TPartExternal.Create(FConn);
    FInstantPart.Value := vPart;
    AssertTrue('Value HasVal', FInstantPart.HasValue);
    AssertEquals(1, FInstantPart.Value.RefCount);
    AssertEquals(1, vPart.RefCount);

    // Added this to help check for possible memory leakage
    vCategory := TCategory.Create(FConn);
    try
      TPartExternal(FInstantPart.Value).Category := vCategory;
    finally
      vCategory.Free;
    end;
    AssertEquals(1, TPartExternal(FInstantPart.Value).Category.RefCount);

    AssertFalse('vSource HasVal', vSource.HasValue);
    AssertNotSame(FInstantPart, vSource);
    vSource.Assign(FInstantPart);
    AssertTrue('vSource HasVal', vSource.HasValue);
    AssertNotSame(FInstantPart.Value, vSource.Value);
    AssertSame(TPartExternal(FInstantPart.Value).Category,
        TPartExternal(vSource.Value).Category);
  finally
    vSource.Free;
    vAttrMetadata.Free;
  end;
end;

procedure TestTInstantExtPart.TestAttach_DetachObject;
var
  vReturnValue: Boolean;
  vObject: TPartExternal;
begin
  vObject := TPartExternal.Create(FConn);
  vObject.Id := 'Object.Id';
  AssertEquals('Object RefCount 1', 1, vObject.RefCount);

  vReturnValue := FInstantPart.AttachObject(vObject);
  AssertTrue('AttachObject', vReturnValue);
  AssertSame(vObject, FInstantPart.Value);
  AssertTrue('FInstantElement HasValue', FInstantPart.HasValue);
  AssertEquals('Value RefCount 1', 1, FInstantPart.Value.RefCount);
  AssertEquals('Object RefCount 2', 1, vObject.RefCount);
  AssertEquals('Value.Id 1', 'Object.Id', FInstantPart.Value.Id);

  vReturnValue := FInstantPart.DetachObject(vObject);
  AssertTrue('DetachObject', vReturnValue);
  AssertEquals('Value.Id 2', '', FInstantPart.Value.Id);
end;

procedure TestTInstantExtPart.TestHasValue;
begin
  AssertFalse(FInstantPart.HasValue);

  FInstantPart.Value := TPartExternal.Create(FConn);
  AssertTrue(FInstantPart.HasValue);
end;

procedure TestTInstantExtPart.TestIsChanged;
var
  vPart: TPartExternal;
begin
  AssertFalse(FInstantPart.IsChanged);

  vPart := TPartExternal.Create(FConn);
  vPart.Changed;
  FInstantPart.Value := vPart;
  AssertTrue(FInstantPart.IsChanged);
end;

procedure TestTInstantExtPart.TestIsDefault;
var
  vPart: TPartExternal;
begin
  AssertTrue(FInstantPart.IsDefault);

  vPart := TPartExternal.Create(FConn);
  vPart.Id := 'PartId';
  FInstantPart.Value := vPart;
  AssertFalse(FInstantPart.IsDefault);
end;

procedure TestTInstantExtPart.TestSaveObjectTo_FromStream;
var
  vObject: TPartExternal;
  vReturnValue: Boolean;
  vStream: TStream;
begin
  vObject := TPartExternal.Create(FConn);
  AssertNotNull('Create object', vObject);
  AssertEquals('Object RefCount 1', 1, vObject.RefCount);
  vReturnValue := FInstantPart.AttachObject(vObject);
  AssertTrue('AttachObject', vReturnValue);
  AssertTrue(FInstantPart.HasValue);
  AssertEquals('Value RefCount 1', 1, FInstantPart.Value.RefCount);
  AssertEquals('Object RefCount 2', 1, vObject.RefCount);

  vStream := TInstantStream.Create;
  try
    FInstantPart.SaveObjectToStream(vStream);
    AssertTrue('vStream.Size check', vStream.Size > 0);
    FInstantPart.Value := nil;
    AssertFalse(FInstantPart.HasValue);
    vStream.Position := 0;
    FInstantPart.LoadObjectFromStream(vStream);
    AssertTrue(FInstantPart.HasValue);
    AssertEquals('Value RefCount 2', 1, FInstantPart.Value.RefCount);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantExtPart.TestUnchanged;
begin
  FInstantPart.Changed;
  AssertTrue(FInstantPart.IsChanged);
  FInstantPart.Unchanged;
  AssertFalse(FInstantPart.IsChanged);
end;

procedure TestTInstantExtPart.TestValue_Reset;
var
  vFirstObj: TInstantObject;
  vSecondObj: TInstantObject;
begin
  AssertFalse('HasValue 1', FInstantPart.HasValue);
  AssertNotNull('AssertNotNull', FInstantPart.Value);
  AssertTrue('HasValue 2', FInstantPart.HasValue);
  vFirstObj := FInstantPart.Value;

  vSecondObj := TPartExternal.Create(FConn);
  vSecondObj.Id := 'PartId';
  FInstantPart.Value := vSecondObj;
  AssertEquals('Value.Id', 'PartId', FInstantPart.Value.Id);
  AssertNotSame('AssertNotSame', vFirstObj, FInstantPart.Value);

  FInstantPart.Reset;
  AssertFalse('HasValue 3', FInstantPart.HasValue);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantEmbPart,
                  TestTInstantExtPart]);
{$ENDIF}

end.
