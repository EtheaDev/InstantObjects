(*
 *   InstantObjects Test Suite
 *   TestInstantTableMetadata
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
 * The Original Code is: InstantObjects Test Suite/TestInstantTableMetadata
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

unit TestInstantTableMetadata;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses fpcunit, InstantPersistence, InstantMetadata;

type

  // Test methods for class TInstantTableMetadata
  TestTInstantTableMetadata = class(TTestCase)
  private
    FInstantTableMetadata: TInstantTableMetadata;
    FOwner: TInstantScheme;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestFieldMetadatas;
    procedure TestIndexMetadatas;
    procedure TestScheme;
  end;

  // Test methods for class TInstantTableMetadatas
  TestTInstantTableMetadatas = class(TTestCase)
  private
    FInstantTableMetadatas: TInstantTableMetadatas;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd_ReadItems;
  end;

implementation

uses
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, testregistry;

{ TestTInstantTableMetadata }

procedure TestTInstantTableMetadata.SetUp;
begin
  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TInstantScheme.Create;
  FOwner.Catalog := TInstantModelCatalog.Create(FOwner, InstantModel);
  FInstantTableMetadata := FOwner.FindTableMetadata('Country');
end;

procedure TestTInstantTableMetadata.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FOwner);
end;

procedure TestTInstantTableMetadata.TestFieldMetadatas;
begin
  AssertNotNull(FInstantTableMetadata.FieldMetadatas);
end;

procedure TestTInstantTableMetadata.TestIndexMetadatas;
begin
  AssertNotNull(FInstantTableMetadata.IndexMetadatas);
end;

procedure TestTInstantTableMetadata.TestScheme;
begin
  AssertNotNull(FInstantTableMetadata.Scheme);
  AssertSame(FOwner, FInstantTableMetadata.Scheme);
end;

procedure TestTInstantTableMetadata.TestAssign;
var
  vDest: TInstantTableMetadata;
begin
  vDest := TInstantTableMetadata.Create(nil);
  try
    AssertTrue(VDest.FieldMetadatas.Count = 0);
    AssertTrue(VDest.IndexMetadatas.Count = 0);
    vDest.Assign(FInstantTableMetadata);
    AssertNotNull(VDest.FieldMetadatas);
    AssertTrue(VDest.FieldMetadatas.Count > 0);
    AssertTrue(VDest.FieldMetadatas[0].Equals(FInstantTableMetadata.FieldMetadatas[0]));
    AssertNotNull(VDest.IndexMetadatas);
    AssertTrue(VDest.IndexMetadatas.Count > 0);
    AssertTrue(VDest.IndexMetadatas[0].Equals(FInstantTableMetadata.IndexMetadatas[0]));
  finally
    vDest.Free;
  end;
end;

{ TestTInstantTableMetadatas }

procedure TestTInstantTableMetadatas.SetUp;
begin
  FInstantTableMetadatas := TInstantTableMetadatas.Create(nil);
end;

procedure TestTInstantTableMetadatas.TearDown;
begin
  FreeAndNil(FInstantTableMetadatas);
end;

procedure TestTInstantTableMetadatas.TestAdd_ReadItems;
var
  vReturnValue: TInstantTableMetadata;
begin
  AssertEquals(0, FInstantTableMetadatas.Count);
  vReturnValue := FInstantTableMetadatas.Add;
  AssertNotNull(vReturnValue);
  AssertEquals(1, FInstantTableMetadatas.Count);
  AssertNotNull('Items[0]', FInstantTableMetadatas.Items[0]);
  FInstantTableMetadatas.Remove(vReturnValue);
  AssertEquals(0, FInstantTableMetadatas.Count);
end;



initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantTableMetadata,
                 TestTInstantTableMetadatas]);
{$ENDIF}

end.
 