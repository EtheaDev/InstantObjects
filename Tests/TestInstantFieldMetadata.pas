(*
 *   InstantObjects Test Suite
 *   TestInstantFieldMetadata
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
 * The Original Code is: InstantObjects Test Suite/TestInstantFieldMetadata
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

unit TestInstantFieldMetadata;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantMetadata,
  DUnitX.TestFramework;

type
  // Test methods for class TInstantFieldMetadata
  [TestFixture]
  TestTInstantFieldMetadata = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FOwner: TInstantTableMetadata;
    FCollection: TInstantFieldMetadatas;
    FInstantFieldMetadata: TInstantFieldMetadata;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAssign;
    procedure TestCollection;
    procedure TestDataType;
    procedure TestOptions;
    procedure TestSize;
    procedure TestAlternateDataTypes;
  end;

  // Test methods for class TInstantFieldMetadatas
  [TestFixture]
  TestTInstantFieldMetadatas = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FOwner: TInstantTableMetadata;
    FInstantFieldMetadatas: TInstantFieldMetadatas;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAddFieldMetadata;
    procedure TestAddRemoveItems;
    procedure TestOwner;
  end;

implementation

uses
  System.Classes,
  SysUtils, TypInfo, InstantTypes;

procedure TestTInstantFieldMetadata.SetUp;
begin
  FOwner := TInstantTableMetadata.Create(nil);
  FCollection := TInstantFieldMetadatas.Create(FOwner);
  FInstantFieldMetadata := TInstantFieldMetadata.Create(FCollection);
  FInstantFieldMetadata.DataType := dtInteger;
  FInstantFieldMetadata.Size := 10;
  FInstantFieldMetadata.Options := [foRequired];
  FInstantFieldMetadata.AlternateDataTypes := [dtBoolean]
end;

procedure TestTInstantFieldMetadata.TearDown;
begin
  FInstantFieldMetadata.Free;
  FInstantFieldMetadata := nil;
  FreeAndNil(FCollection);
  FreeAndNil(FOwner);
end;

procedure TestTInstantFieldMetadata.TestCollection;
begin
  AssertNotNull(FInstantFieldMetadata.Collection);
  AssertSame(FCollection, FInstantFieldMetadata.Collection);
end;

procedure TestTInstantFieldMetadata.TestDataType;
var
  vStr: string;
begin
  vStr := GetEnumName(TypeInfo(TInstantDataType),
    Ord(FInstantFieldMetadata.DataType));
  AssertEquals('dtInteger', vStr);
end;

procedure TestTInstantFieldMetadata.TestOptions;
begin
  AssertTrue(FInstantFieldMetadata.Options = [foRequired]);
end;

procedure TestTInstantFieldMetadata.TestSize;
begin
  AssertEquals(10, FInstantFieldMetadata.Size);
end;

procedure TestTInstantFieldMetadata.TestAlternateDataTypes;
begin
  AssertTrue(FInstantFieldMetadata.AlternateDataTypes = [dtBoolean]);
end;

procedure TestTInstantFieldMetadata.TestAssign;
var
  vDest: TInstantFieldMetadata;
  vStr: string;
begin
  vDest := TInstantFieldMetadata.Create(nil);
  try
    vDest.Assign(FInstantFieldMetadata);
    AssertEquals(10, vDest.Size);
    AssertTrue(vDest.Options = [foRequired]);
    vStr := GetEnumName(TypeInfo(TInstantDataType), Ord(vDest.DataType));
    AssertEquals('dtInteger', vStr);
    AssertTrue(vDest.AlternateDataTypes = [dtBoolean]);
  finally
    vDest.Free;
  end;
end;

procedure TestTInstantFieldMetadatas.SetUp;
begin
  FOwner := TInstantTableMetadata.Create(nil);
  FInstantFieldMetadatas := TInstantFieldMetadatas.Create(FOwner);
end;

procedure TestTInstantFieldMetadatas.TearDown;
begin
  FInstantFieldMetadatas.Free;
  FInstantFieldMetadatas := nil;
  FreeAndNil(FOwner);
end;

procedure TestTInstantFieldMetadatas.TestAddRemoveItems;
var
  vReturnValue: TInstantFieldMetadata;
begin
  vReturnValue := FInstantFieldMetadatas.Add;
  AssertNotNull(vReturnValue);
  AssertEquals(1, FInstantFieldMetadatas.Count);
  AssertNotNull('Items[0]', FInstantFieldMetadatas.Items[0]);
  FInstantFieldMetadatas.Remove(vReturnValue);
  AssertEquals(0, FInstantFieldMetadatas.Count);
end;

procedure TestTInstantFieldMetadatas.TestAddFieldMetadata;
var
  vOptions: TInstantFieldOptions;
  vSize: Integer;
  vDataType: TInstantDataType;
  vName: string;
  vInstantFieldMetadata: TInstantFieldMetadata;
  vStr: string;
begin
  vName := 'Code';
  vDataType := dtInteger;
  vSize := 10;
  vOptions := [foRequired];
  FInstantFieldMetadatas.AddFieldMetadata(vName, vDataType, vSize, vOptions);

  AssertEquals(1, FInstantFieldMetadatas.Count);
  vInstantFieldMetadata := FInstantFieldMetadatas.Items[0];
  AssertNotNull(vInstantFieldMetadata);
  AssertEquals(vName, vInstantFieldMetadata.Name);
  AssertEquals(10, vInstantFieldMetadata.Size);
  AssertTrue(foRequired in vInstantFieldMetadata.Options);
  vStr := GetEnumName(TypeInfo(TInstantDataType),
    Ord(vInstantFieldMetadata.DataType));
  AssertEquals('dtInteger', vStr);
end;

procedure TestTInstantFieldMetadatas.TestOwner;
begin
  AssertNotNull(FInstantFieldMetadatas.Owner);
  AssertSame(FOwner, FInstantFieldMetadatas.Owner);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantFieldMetadata,
                 TestTInstantFieldMetadatas]);
{$ENDIF}

end.
