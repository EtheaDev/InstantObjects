(*
 *   InstantObjects Test Suite
 *   TestInstantParts
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
 * The Original Code is: InstantObjects Test Suite/TestInstantParts
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

unit TestInstantParts;

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantParts
  TestTInstantExtParts = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantParts: TInstantParts;
    FOwner: TContact;
    function PartsExternalCompare(Holder, Obj1, Obj2: TInstantObject): Integer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAssign;
    procedure TestAttachObject;
    procedure TestClear;
    procedure TestDelete;
    procedure TestUnchanged;
    procedure TestDestroyObject;
    procedure TestDetachObject;
    procedure TestExchange;
    procedure TestHasItem;
    procedure TestIndexOf_Insert;
    procedure TestIndexOfInstance;
    procedure TestMove;
    procedure TestRemove;
    procedure TestReset;
    procedure TestSaveObjectsTo_FromStream;
    procedure TestSort;
  end;

  TestTinstantEmbParts = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantParts: TInstantParts;
    FOwner: TContact;
    function PartsEmbeddedCompare(Holder, Obj1, Obj2: TInstantObject): Integer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAssign;
    procedure TestAttachObject;
    procedure TestClear;
    procedure TestDelete;
    procedure TestDetachObject;
    procedure TestExchange;
    procedure TestHasItem_DestroyObject;
    procedure TestIndexOfInstance;
    procedure TestIndexOf_Insert;
    procedure TestMove;
    procedure TestRemove;
    procedure TestReset;
    procedure TestSaveObjectsTo_FromStream;
    procedure TestSort;
    procedure TestUnchanged;
  end;

  TestTInstantParts_Leak = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantParts: TInstantParts;
    FOwner: TContact;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddEmbeddedObject;
    procedure TestAddExternalObject;
  end;

implementation

uses SysUtils, Classes, testregistry;

procedure TestTInstantExtParts.SetUp;
var
  i: Integer;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantParts := FOwner._ExternalPhones;
  for i := 0 to 2 do
    FOwner.AddExternalPart(TExternalPhones.Create(FConn));
  AssertEquals('Setup FInstantParts.Count', 3, FInstantParts.Count);
end;

function TestTInstantExtParts.PartsExternalCompare(Holder, Obj1, Obj2:
    TInstantObject): Integer;
var
  vObj1, vObj2: TExternalPhones;
begin
  vObj1 := Obj1 as TExternalPhones;
  vObj2 := Obj2 as TExternalPhones;

  Result := AnsiCompareText(vObj1.Name, vObj2.Name);
end;

procedure TestTInstantExtParts.TearDown;
begin
  FInstantParts := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantExtParts.TestAdd;
var
  vReturnValue: Integer;
  vExternalPart: TExternalPhones;
begin
  vExternalPart := TExternalPhones.Create(FConn);
  vReturnValue := FInstantParts.Add(vExternalPart);
  AssertTrue(vReturnValue <> -1);
  AssertEquals(4, FInstantParts.Count);
end;

procedure TestTInstantExtParts.TestAssign;
var
  vSource: TInstantParts;
  vAttrMetadata: TInstantAttributeMetadata;
begin
  vSource := nil;

  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  try
    vAttrMetadata.AttributeClass := TInstantParts;
    vAttrMetadata.StorageKind := skExternal;
    vSource := TInstantParts.Create(FOwner, vAttrMetadata);

    AssertTrue(vSource.Count = 0);
    vSource.Assign(FInstantParts);
    AssertTrue(vSource.Count = 3);
    AssertNotSame(FInstantParts.Items[0], vSource.Items[0]);
  finally
    vSource.Free;
    vAttrMetadata.Free;
  end;
end;

procedure TestTInstantExtParts.TestAttachObject;
var
  vReturnValue: Boolean;
  vExternalPart: TExternalPhones;
begin
  vExternalPart := TExternalPhones.Create(FConn);
  vReturnValue := FInstantParts.AttachObject(vExternalPart);
  AssertTrue(vReturnValue);
  AssertEquals(4, FInstantParts.Count);
end;

procedure TestTInstantExtParts.TestClear;
begin
  FInstantParts.Clear;
  AssertEquals(0, FInstantParts.Count);
end;

procedure TestTInstantExtParts.TestDelete;
begin
  FInstantParts.Delete(1);
  AssertEquals(2, FInstantParts.Count);
end;

procedure TestTInstantExtParts.TestDestroyObject;
begin
  AssertTrue(FInstantParts.HasItem(1));

  FInstantParts.DestroyObject(1);
  AssertFalse(FInstantParts.HasItem(1));
end;

procedure TestTInstantExtParts.TestDetachObject;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantParts.DetachObject(FInstantParts.Items[1]);
  AssertTrue(vReturnValue);
  AssertEquals(2, FInstantParts.Count);
end;

procedure TestTInstantExtParts.TestExchange;
begin
  TExternalPhones(FInstantParts.Items[0]).Name := 'Part0';
  TExternalPhones(FInstantParts.Items[1]).Name := 'Part1';
  TExternalPhones(FInstantParts.Items[2]).Name := 'Part2';
  FInstantParts.Exchange(0, 2);
  AssertEquals('Part2', TExternalPhones(FInstantParts.Items[0]).Name);
  AssertEquals('Part1', TExternalPhones(FInstantParts.Items[1]).Name);
  AssertEquals('Part0', TExternalPhones(FInstantParts.Items[2]).Name);
end;

procedure TestTInstantExtParts.TestHasItem;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantParts.HasItem(1);
  AssertTrue(vReturnValue);

  FInstantParts.DestroyObject(1);
  vReturnValue := FInstantParts.HasItem(1);
  AssertFalse(vReturnValue);
end;

procedure TestTInstantExtParts.TestIndexOf_Insert;
var
  vReturnValue: Integer;
  vObject: TInstantObject;
begin
  vObject := TExternalPhones.Create(FConn);
  FInstantParts.Insert(1, vObject);
  vReturnValue := FInstantParts.IndexOf(vObject);
  AssertEquals(1, vReturnValue);
end;

procedure TestTInstantExtParts.TestIndexOfInstance;
var
  vReturnValue: Integer;
  vInstance: Pointer;
begin
  vInstance := TExternalPhones.Create(FConn);
  FInstantParts.Insert(1, vInstance);
  vReturnValue := FInstantParts.IndexOfInstance(vInstance);
  AssertEquals(1, vReturnValue);
end;

procedure TestTInstantExtParts.TestMove;
var
  vExternalPart: TExternalPhones;
begin
  TExternalPhones(FInstantParts.Items[0]).Name := 'Part0';
  TExternalPhones(FInstantParts.Items[1]).Name := 'Part1';
  TExternalPhones(FInstantParts.Items[2]).Name := 'Part2';
  vExternalPart := TExternalPhones.Create(FConn);
  FInstantParts.Add(vExternalPart);
  TExternalPhones(FInstantParts.Items[3]).Name := 'Part3';
  FInstantParts.Move(0, 2);
  AssertEquals('Part1', TExternalPhones(FInstantParts.Items[0]).Name);
  AssertEquals('Part2', TExternalPhones(FInstantParts.Items[1]).Name);
  AssertEquals('Part0', TExternalPhones(FInstantParts.Items[2]).Name);
  AssertEquals('Part3', TExternalPhones(FInstantParts.Items[3]).Name);
end;

procedure TestTInstantExtParts.TestRemove;
var
  vReturnValue: Integer;
begin
  vReturnValue := FInstantParts.Remove(FInstantParts.Items[1]);
  AssertEquals(1, vReturnValue);
  AssertEquals(2, FInstantParts.Count);
end;

procedure TestTInstantExtParts.TestReset;
begin
  FInstantParts.Reset;
  AssertEquals(0, FInstantParts.Count);
end;

procedure TestTInstantExtParts.TestSaveObjectsTo_FromStream;
var
  vStream: TStream;
begin
  vStream := TMemoryStream.Create;
  try
    AssertEquals(0, vStream.Size);
    FInstantParts.SaveObjectsToStream(vStream);
    AssertTrue(vStream.Size > 0);
    FInstantParts.Clear;
    AssertEquals(0, FInstantParts.Count);

    vStream.Position := 0;
    FInstantParts.LoadObjectsFromStream(vStream);
    AssertEquals(3, FInstantParts.Count);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantExtParts.TestSort;
var
  vExternalPart: TExternalPhones;
begin
  TExternalPhones(FInstantParts.Items[0]).Name := '2 Part';
  TExternalPhones(FInstantParts.Items[1]).Name := '0 Part';
  TExternalPhones(FInstantParts.Items[2]).Name := '1 Part';
  vExternalPart := TExternalPhones.Create(FConn);
  FOwner.AddExternalPart(vExternalPart);
  TExternalPhones(FInstantParts.Items[3]).Name := '0 Part';

  FInstantParts.Sort(PartsExternalCompare);
  AssertEquals('0 Part', TExternalPhones(FInstantParts.Items[0]).Name);
  AssertEquals('0 Part', TExternalPhones(FInstantParts.Items[1]).Name);
  AssertEquals('1 Part', TExternalPhones(FInstantParts.Items[2]).Name);
  AssertEquals('2 Part', TExternalPhones(FInstantParts.Items[3]).Name);
end;

procedure TestTInstantExtParts.TestUnchanged;
begin
  AssertEquals(3, FInstantParts.Count);
  AssertTrue(FInstantParts.IsChanged);

  FInstantParts.Unchanged;
  AssertFalse(FInstantParts.IsChanged);

  TExternalPhones(FInstantParts.Items[1]).Name := 'Part2';
  AssertTrue(FInstantParts.IsChanged);
end;

{ TestTinstantEmbParts }

function TestTinstantEmbParts.PartsEmbeddedCompare(Holder, Obj1, Obj2:
    TInstantObject): Integer;
var
  vObj1, vObj2: TPhone;
begin
  vObj1 := Obj1 as TPhone;
  vObj2 := Obj2 as TPhone;

  Result := AnsiCompareText(vObj1.Name, vObj2.Name);
end;

procedure TestTinstantEmbParts.SetUp;
var
  i: Integer;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantParts := FOwner._Phones;
  for i := 0 to 2 do
    FOwner.AddPhone(TPhone.Create(FConn));
  AssertEquals('Setup FInstantParts.Count', 3, FInstantParts.Count);
end;

procedure TestTinstantEmbParts.TearDown;
begin
  FInstantParts := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTinstantEmbParts.TestAdd;
var
  vReturnValue: Integer;
  vPhone: TPhone;
begin
  vPhone := TPhone.Create(FConn);
  vReturnValue := FInstantParts.Add(vPhone);
  AssertTrue(vReturnValue <> -1);
  AssertEquals(4, FInstantParts.Count);
end;

procedure TestTinstantEmbParts.TestAssign;
var
  vSource: TInstantParts;
  vAttrMetadata: TInstantAttributeMetadata;
begin
  vSource := nil;

  vAttrMetadata := TInstantAttributeMetadata.Create(nil);
  try
    vAttrMetadata.AttributeClass := TInstantParts;
    vSource := TInstantParts.Create(FOwner, vAttrMetadata);

    AssertTrue(vSource.Count = 0);
    vSource.Assign(FInstantParts);
    AssertTrue(vSource.Count = 3);
    AssertNotSame(FInstantParts.Items[0], vSource.Items[0]);
  finally
    vSource.Free;
    vAttrMetadata.Free;
  end;
end;

procedure TestTinstantEmbParts.TestAttachObject;
var
  vReturnValue: Boolean;
  vPhone: TPhone;
begin
  vPhone := TPhone.Create(FConn);
  vReturnValue := FInstantParts.AttachObject(vPhone);
  AssertTrue(vReturnValue);
  AssertEquals(4, FInstantParts.Count);
end;

procedure TestTinstantEmbParts.TestClear;
begin
  FInstantParts.Clear;
  AssertEquals(0, FInstantParts.Count);
end;

procedure TestTinstantEmbParts.TestDelete;
begin
  FInstantParts.Delete(1);
  AssertEquals(2, FInstantParts.Count);
end;

procedure TestTinstantEmbParts.TestDetachObject;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantParts.DetachObject(FInstantParts.Items[1]);
  AssertTrue(vReturnValue);
  AssertEquals(2, FInstantParts.Count);
end;

procedure TestTinstantEmbParts.TestExchange;
begin
  TPhone(FInstantParts.Items[0]).Name := 'Part0';
  TPhone(FInstantParts.Items[1]).Name := 'Part1';
  TPhone(FInstantParts.Items[2]).Name := 'Part2';
  FInstantParts.Exchange(0, 2);
  AssertEquals('Part2', TPhone(FInstantParts.Items[0]).Name);
  AssertEquals('Part1', TPhone(FInstantParts.Items[1]).Name);
  AssertEquals('Part0', TPhone(FInstantParts.Items[2]).Name);
end;

procedure TestTinstantEmbParts.TestHasItem_DestroyObject;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantParts.HasItem(1);
  AssertTrue('Initial', vReturnValue);

  // This should do nothing for embedded parts
  FInstantParts.DestroyObject(1);
  vReturnValue := FInstantParts.HasItem(1);
  AssertTrue('After', vReturnValue);
end;

procedure TestTinstantEmbParts.TestIndexOfInstance;
var
  vReturnValue: Integer;
  vInstance: Pointer;
begin
  vInstance := TPhone.Create(FConn);
  FInstantParts.Insert(1, vInstance);
  vReturnValue := FInstantParts.IndexOfInstance(vInstance);
  AssertEquals(1, vReturnValue);
end;

procedure TestTinstantEmbParts.TestIndexOf_Insert;
var
  vReturnValue: Integer;
  vObject: TInstantObject;
begin
  vObject := TPhone.Create(FConn);
  FInstantParts.Insert(1, vObject);
  vReturnValue := FInstantParts.IndexOf(vObject);
  AssertEquals(1, vReturnValue);
end;

procedure TestTinstantEmbParts.TestMove;
var
  vPhone: TPhone;
begin
  TPhone(FInstantParts.Items[0]).Name := 'Part0';
  TPhone(FInstantParts.Items[1]).Name := 'Part1';
  TPhone(FInstantParts.Items[2]).Name := 'Part2';
  vPhone := TPhone.Create(FConn);
  FInstantParts.Add(vPhone);
  TPhone(FInstantParts.Items[3]).Name := 'Part3';
  FInstantParts.Move(0, 2);
  AssertEquals('Part1', TPhone(FInstantParts.Items[0]).Name);
  AssertEquals('Part2', TPhone(FInstantParts.Items[1]).Name);
  AssertEquals('Part0', TPhone(FInstantParts.Items[2]).Name);
  AssertEquals('Part3', TPhone(FInstantParts.Items[3]).Name);
end;

procedure TestTinstantEmbParts.TestRemove;
var
  vReturnValue: Integer;
begin
  vReturnValue := FInstantParts.Remove(FInstantParts.Items[1]);
  AssertEquals(1, vReturnValue);
  AssertEquals(2, FInstantParts.Count);
end;

procedure TestTinstantEmbParts.TestReset;
begin
  FInstantParts.Reset;
  AssertEquals(0, FInstantParts.Count);
end;

procedure TestTinstantEmbParts.TestSaveObjectsTo_FromStream;
var
  vStream: TStream;
begin
  vStream := TMemoryStream.Create;
  try
    AssertEquals(0, vStream.Size);
    FInstantParts.SaveObjectsToStream(vStream);
    AssertTrue(vStream.Size > 0);
    FInstantParts.Clear;
    AssertEquals(0, FInstantParts.Count);

    vStream.Position := 0;
    FInstantParts.LoadObjectsFromStream(vStream);
    AssertEquals(3, FInstantParts.Count);
  finally
    vStream.Free;
  end;
end;

procedure TestTinstantEmbParts.TestSort;
var
  vPhone: TPhone;
begin
  TPhone(FInstantParts.Items[0]).Name := '2 Part';
  TPhone(FInstantParts.Items[1]).Name := '0 Part';
  TPhone(FInstantParts.Items[2]).Name := '1 Part';
  vPhone := TPhone.Create(FConn);
  FInstantParts.Add(vPhone);
  TPhone(FInstantParts.Items[3]).Name := '0 Part';

  FInstantParts.Sort(PartsEmbeddedCompare);
  AssertEquals('0 Part', TPhone(FInstantParts.Items[0]).Name);
  AssertEquals('0 Part', TPhone(FInstantParts.Items[1]).Name);
  AssertEquals('1 Part', TPhone(FInstantParts.Items[2]).Name);
  AssertEquals('2 Part', TPhone(FInstantParts.Items[3]).Name);
end;

procedure TestTinstantEmbParts.TestUnchanged;
begin
  AssertEquals(3, FInstantParts.Count);
  AssertTrue(FInstantParts.IsChanged);

  FInstantParts.Unchanged;
  AssertFalse(FInstantParts.IsChanged);

  TExternalPhones(FInstantParts.Items[1]).Name := 'Part2';
  AssertTrue(FInstantParts.IsChanged);
end;

procedure TestTInstantParts_Leak.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
end;

procedure TestTInstantParts_Leak.TearDown;
begin
  FInstantParts.Clear;
  FInstantParts := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantParts_Leak.TestAddEmbeddedObject;
var
  vReturnValue: Integer;
  vPart: TPhone;
begin
  FInstantParts := FOwner._Phones;

  vPart := TPhone.Create(FConn);
  AssertEquals(1, vPart.RefCount);

  vReturnValue := FInstantParts.Add(vPart);
  AssertTrue(vReturnValue <> -1);
  AssertEquals('FInstantParts.Count 1', 1, FInstantParts.Count);
  AssertEquals('vPart.RefCount 1', 1, vPart.RefCount);

  vReturnValue := FInstantParts.Remove(vPart);
  AssertTrue(vReturnValue <> -1);
  AssertEquals('FInstantParts.Count 2', 0, FInstantParts.Count);
//  AssertException(EAccessViolation, vPart.Free);
end;

procedure TestTInstantParts_Leak.TestAddExternalObject;
var
  vReturnValue: Integer;
  vPart: TExternalPhones;
begin
  FInstantParts := FOwner._ExternalPhones;

  vPart := TExternalPhones.Create(FConn);
  AssertEquals(1, vPart.RefCount);

  vReturnValue := FInstantParts.Add(vPart);
  AssertTrue(vReturnValue <> -1);
  AssertEquals('FInstantParts.Count 1', 1, FInstantParts.Count);
  AssertEquals('vPart.RefCount 1', 1, vPart.RefCount);

  vReturnValue := FInstantParts.Remove(vPart);
  AssertTrue(vReturnValue <> -1);
  AssertEquals('FInstantParts.Count 2', 0, FInstantParts.Count);
//  AssertEquals('vPart.RefCount 2', 1, vPart.RefCount);
//  AssertException(EAccessViolation, vPart.Free);
end;


initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantExtParts,
                TestTinstantEmbParts,
                TestTInstantParts_Leak]);
{$ELSE}
  RegisterTests([
                TestTInstantExtParts,
                TestTinstantEmbParts,
                TestTInstantParts_Leak
                ]);
{$ENDIF}

end.
 