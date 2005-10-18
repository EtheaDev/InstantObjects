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
  TestTInstantPart = class(TTestCase)
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
    procedure TestIsChanged;
    procedure TestIsDefault;
    procedure TestHasValue;
    procedure TestUnchanged;
    procedure TestValue_Reset;
  end;

implementation

uses SysUtils, testregistry;

procedure TestTInstantPart.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantPart := FOwner._Address;
end;

procedure TestTInstantPart.TearDown;
begin
  FInstantPart := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantPart.TestAssign;
var
  vSource: TInstantPart;
  vAttrMetadata: TInstantAttributeMetadata;
  vPart: TAddress;
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

    AssertFalse('vSource HasVal', vSource.HasValue);
    AssertNotSame(FInstantPart, vSource);
    vSource.Assign(FInstantPart);
  finally
    vSource.Free;
    vAttrMetadata.Free;
  end;
end;

procedure TestTInstantPart.TestAllowOwned;
begin
  AssertFalse(FInstantPart.AllowOwned);
end;

procedure TestTInstantPart.TestIsChanged;
var
  vPart: TAddress;
begin
  AssertFalse(FInstantPart.IsChanged);

  vPart := TAddress.Create(FConn);
  vPart.Changed;
  FInstantPart.Value := vPart;
  AssertTrue(FInstantPart.IsChanged);
end;

procedure TestTInstantPart.TestIsDefault;
var
  vPart: TAddress;
begin
  AssertTrue(FInstantPart.IsDefault);

  vPart := TAddress.Create(FConn);
  vPart.Id := 'PartId';
  FInstantPart.Value := vPart;
  AssertFalse(FInstantPart.IsDefault);
end;

procedure TestTInstantPart.TestHasValue;
begin
  AssertFalse(FInstantPart.HasValue);

  FInstantPart.Value := TAddress.Create(FConn);
  AssertTrue(FInstantPart.HasValue);
end;

procedure TestTInstantPart.TestUnchanged;
begin
  FInstantPart.Changed;
  AssertTrue(FInstantPart.IsChanged);
  FInstantPart.Unchanged;
  AssertFalse(FInstantPart.IsChanged);
end;

procedure TestTInstantPart.TestValue_Reset;
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

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantPart]);
{$ENDIF}

end.
