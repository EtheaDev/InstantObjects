(*
 *   InstantObjects Test Suite
 *   TestInstantObjectState
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
 * The Original Code is: InstantObjects Test Suite/TestInstantObjectState
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

unit TestInstantObjectState;

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantObjectState
  [TestFixture]
  TestTInstantObjectState = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FInstantObjectState: TInstantObjectState;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAssign;
  end;

implementation

uses SysUtils, Classes, InstantClasses;

procedure TestTInstantObjectState.SetUp;
begin
  FInstantObjectState := TInstantObjectState.Create;
  FInstantObjectState.IsChanged := True;
  FInstantObjectState.PersistentId := 'ObjectStateId';
  FInstantObjectState.UpdateCount := 0;
end;

procedure TestTInstantObjectState.TearDown;
begin
  FreeAndNil(FInstantObjectState);
end;

procedure TestTInstantObjectState.TestAssign;
var
  Source: TPersistent;
begin
  Source := TInstantObjectState.Create;
  try
    with Source as TInstantObjectState do
    begin
      IsChanged := True;
      PersistentId := 'SourceId';
      UpdateCount := 10;
    end;

    FInstantObjectState.Assign(Source);
    AssertTrue(FInstantObjectState.IsChanged);
    AssertEquals('SourceId', FInstantObjectState.PersistentId);
    AssertEquals(10, FInstantObjectState.UpdateCount);
  finally
    Source.Free;
  end;
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantObjectState]);
{$ENDIF}

end.
 