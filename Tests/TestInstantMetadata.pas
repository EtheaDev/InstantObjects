(*
 *   InstantObjects Test Suite
 *   TestInstantMetadata
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
 * The Original Code is: InstantObjects Test Suite/TestInstantMetadata
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

unit TestInstantMetadata;

interface

uses SysUtils, {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantMetadata,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantMetadata
  [TestFixture]
  TestTInstantMetadata = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FInstantMetadata: TInstantMetadata;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestCollectionExists;
  end;

  // Test methods for class TInstantMetadatas
  [TestFixture]
  TestTInstantMetadatas = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FInstantMetadatas: TInstantMetadatas;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestFind;
  end;

implementation

procedure TestTInstantMetadata.SetUp;
var
  vCollection: TInstantMetadatas;
begin
  vCollection := TInstantMetadatas.Create(nil, TInstantMetadata);
  FInstantMetadata := TInstantMetadata.Create(vCollection);
end;

procedure TestTInstantMetadata.TearDown;
var
  vCollection: TInstantMetadatas;
begin
  vCollection := FInstantMetadata.Collection;
  FreeAndNil(FInstantMetadata);
  FreeAndNil(vCollection);
end;

procedure TestTInstantMetadata.TestCollectionExists;
begin
  AssertTrue(FInstantMetadata.Collection <> nil);
end;

procedure TestTInstantMetadatas.SetUp;
var
  TestItem: TInstantMetadata;
begin
  FInstantMetadatas := TInstantMetadatas.Create(nil, TInstantMetadata);
  TestItem := TInstantMetadata(FInstantMetadatas.Add);
  TestItem.Name := 'Name';
  TestItem := TInstantMetadata(FInstantMetadatas.Add);
  TestItem.Name := 'Address';
  TestItem := TInstantMetadata(FInstantMetadatas.Add);
  TestItem.Name := 'Country';
end;

procedure TestTInstantMetadatas.TearDown;
begin
  FInstantMetadatas.Free;
  FInstantMetadatas := nil;
end;

procedure TestTInstantMetadatas.TestFind;
var
  vReturnValue: TInstantMetadata;
  vName: string;
begin
  vName := 'Address';
  vReturnValue := FInstantMetadatas.Find(vName);
  AssertEquals(vName, vReturnValue.Name);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantMetadata,
                 TestTInstantMetadatas]);
{$ENDIF}

end.
