(*
 *   InstantObjects Test Suite
 *   TestInstantPersistence
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
 * The Original Code is: InstantObjects Test Suite/TestInstantPersistence
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * 
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestInstantPersistence;

interface

uses
  Classes, SysUtils,
  InstantPersistence,
  fpcunit,
  testregistry;

type

  { TTestInstantPersistence }

  TTestInstantPersistence = class(TTestCase)
  published
    procedure TestMetadatas;
    procedure TestInstantAttributeMetadatas;
    procedure TestInstantFieldMetadatas;
    procedure TestInstantString;
  end;

implementation


{ TTestInstantPersistence }

procedure TTestInstantPersistence.TestMetadatas;
var
  i: TInstantMetadata;
  c: TInstantMetadatas;
begin
  c := TInstantMetadatas.Create(nil, TInstantMetadata);
  AssertNotNull(c);
  AssertEquals(0, c.Count);
  i := c.add as TInstantMetadata;
  AssertEquals(1, c.Count);
  i.Name := 'pippo';
  AssertTrue(i = c.Find('pippo'));
  AssertTrue(i.Collection = c);
end;

procedure TTestInstantPersistence.TestInstantAttributeMetadatas;
var
  i: TInstantAttributeMetadata;
  c: TInstantAttributeMetadatas;
begin
  c := TInstantAttributeMetadatas.Create(nil);
  AssertNotNull(c);
  AssertEquals(0, c.Count);
  i := c.add;
  AssertEquals(1, c.Count);
  i.Name := 'pippo';
  i.DefaultValue := '1';
  AssertTrue(i = c.Find('pippo'));
  AssertTrue(i.Collection = c);
  AssertEquals('1', i.DefaultValue);
end;

procedure TTestInstantPersistence.TestInstantFieldMetadatas;
var
  i: TInstantFieldMetadata;
  c: TInstantFieldMetadatas;
begin
  c := TInstantFieldMetadatas.Create(nil);
  AssertNotNull(c);
  AssertEquals(0, c.Count);
  i := c.add;
  AssertEquals(1, c.Count);
  i.Name := 'pippo';
  AssertTrue(i = c.Find('pippo'));
  AssertTrue(i.Collection = c);
end;

procedure TTestInstantPersistence.TestInstantString;
var
  c: TInstantString;
begin
  c := TInstantString.Create();
  try
    AssertFalse(c.IsChanged);
    c.AsString := 'goofy';
    AssertTrue(c.IsChanged);
  finally
    c.Free;
  end;
end;


initialization
  RegisterTests([TTestInstantPersistence]);

end.
