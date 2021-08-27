(*
 *   InstantObjects Test Suite
 *   TestInstantAttributeMap
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
 * The Original Code is: InstantObjects Test Suite/TestInstantAttributeMap
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

unit TestInstantAttributeMap;

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMetadata,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantAttributeMap
  [TestFixture]
  TestTInstantAttributeMap = class(TInstantTestCase)
  private
    FClassMetadata: TInstantClassMetadata;
    FInstantAttributeMap: TInstantAttributeMap;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAdd;
    procedure TestAddUnique;
    procedure TestClassMetadata;
    procedure TestFind;
    procedure TestIndexOf;
    procedure TestInsert;
    procedure TestIsRootMap;
    procedure TestItems;
    procedure TestRemove;
  end;

  // Test methods for class TInstantAttributeMaps
  [TestFixture]
  TestTInstantAttributeMaps = class(TInstantTestCase)
  private
    FClassMetadata: TInstantClassMetadata;
    FInstantAttributeMaps: TInstantAttributeMaps;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAdd;
    procedure TestAddItem;
    procedure TestFind;
    procedure TestFindMap;
    procedure TestEnsureMap;
    procedure TestIndexOf;
    procedure TestInsert;
    procedure TestItems;
    procedure TestRemove;
    procedure TestRootMap;
  end;

implementation

uses SysUtils;

procedure TestTInstantAttributeMap.SetUp;
var
  vItem: TInstantAttributeMetadata;
  i: Integer;
begin
  FClassMetadata := TInstantClassMetadata.Create(nil);
  FClassMetadata.TableName := 'TableName';

  FInstantAttributeMap := TInstantAttributeMap.Create(FClassMetadata);
  // Make it the root map
  FInstantAttributeMap.Name := 'TableName';

  for i := 1 to 3 do
  begin
    vItem := TInstantAttributeMetadata.Create(nil);
    vItem.Name := 'Item' + IntToStr(i);
    FInstantAttributeMap.Add(vItem);
  end;
  AssertEquals('AttributeMap count is incorrect!', 3,
    FInstantAttributeMap.Count);
end;

procedure TestTInstantAttributeMap.TearDown;
var
  i: Integer;
begin
  // FInstantAttributeMap is ultimately a TList descendent
  // and does not free its object contents.
  for i := FInstantAttributeMap.Count - 1 downto 0 do
    FInstantAttributeMap[i].Free;
  FreeAndNil(FInstantAttributeMap);
  FreeAndNil(FClassMetadata);
  InstantModel.ClassMetadatas.Clear;
end;

procedure TestTInstantAttributeMap.TestAdd;
var
  vReturnValue: Integer;
  vItem: TInstantAttributeMetadata;
begin
  vItem := TInstantAttributeMetadata.Create(nil);
  vReturnValue := FInstantAttributeMap.Add(vItem);
  AssertEquals('AttributeMap item index is incorrect!', 3, vReturnValue);
  AssertEquals('AttributeMap count is incorrect!', 4,
    FInstantAttributeMap.Count);
end;

procedure TestTInstantAttributeMap.TestAddUnique;
var
  vReturnValue: Integer;
  vItem: TInstantAttributeMetadata;
begin
  // Add non-unique named item
  vItem := TInstantAttributeMetadata.Create(nil);
  try
    vItem.Name := 'Item1';      // Existing name
    vReturnValue := FInstantAttributeMap.AddUnique(vItem);
    AssertEquals('AddUnique', -1, vReturnValue);
    AssertEquals('AttributeMap count', 3, FInstantAttributeMap.Count);
  finally
    vItem.Free;
  end;

  // Add unique named item
  vItem := TInstantAttributeMetadata.Create(nil);
  vItem.Name := 'NewItem';    // New name
  vReturnValue := FInstantAttributeMap.AddUnique(vItem);
  AssertEquals('AddUnique', 3, vReturnValue);
  AssertEquals('AttributeMap count', 4, FInstantAttributeMap.Count);
end;

procedure TestTInstantAttributeMap.TestClassMetadata;
begin
  AssertSame('ClassMetadata value is incorrect!', FClassMetadata,
    FInstantAttributeMap.ClassMetadata);
end;

procedure TestTInstantAttributeMap.TestFind;
var
  vReturnValue: TInstantAttributeMetadata;
  vName: string;
begin
  // Find with existing name
  vName := 'Item2';
  vReturnValue := FInstantAttributeMap.Find(vName);
  AssertNotNull('Find returned nil!', vReturnValue);
  AssertEquals('AttributeMetadata name is incorrect!', vName,
    vReturnValue.Name);

  // Find with non-existing name
  vReturnValue := FInstantAttributeMap.Find('NotIn');
  AssertNull('Find returned not nil!', vReturnValue);
end;

procedure TestTInstantAttributeMap.TestIndexOf;
var
  vReturnValue: Integer;
  vItem: TInstantAttributeMetadata;
begin
  vItem := FInstantAttributeMap[1];
  AssertNotNull('Find returned nil!', vItem);

  vReturnValue := FInstantAttributeMap.IndexOf(vItem);
  AssertEquals('AttributeMetadata index is incorrect!', 1, vReturnValue);
end;

procedure TestTInstantAttributeMap.TestInsert;
var
  vItem: TInstantAttributeMetadata;
  vIndex: Integer;
begin
  vItem := TInstantAttributeMetadata.Create(nil);

  vIndex := 1;
  FInstantAttributeMap.Insert(vIndex, vItem);
  AssertEquals('AttributeMap count is incorrect!', 4,
    FInstantAttributeMap.Count);

  vIndex := FInstantAttributeMap.IndexOf(vItem);
  AssertEquals('AttributeMetadata index is incorrect!', 1, vIndex);
end;

procedure TestTInstantAttributeMap.TestIsRootMap;
begin
  AssertTrue('FInstantAttributeMap is not the root map!',
    FInstantAttributeMap.IsRootMap);

  FInstantAttributeMap.Name := 'NewName';
  AssertFalse('FInstantAttributeMap is the root map!',
    FInstantAttributeMap.IsRootMap);
end;

procedure TestTInstantAttributeMap.TestItems;
var
  vName: string;
  vItem, vNewItem,
  vReturnValue : TInstantAttributeMetadata;
begin
  vName := 'NewItem';
  vItem := FInstantAttributeMap[1];
  AssertNotNull('Find returned nil!', vItem);
  AssertEquals('Name is incorrect!', 'Item2', vItem.Name);
  vNewItem := TInstantAttributeMetadata.Create(nil);
  AssertNotNull('Find returned nil!', vNewItem);
  vNewItem.Name := vName;

  FInstantAttributeMap[1] := vNewItem;
  AssertEquals('AttributeMap Count is incorrect!', 3,
    FInstantAttributeMap.Count);
  vReturnValue := FInstantAttributeMap.Find(vName);
  AssertNotNull('Find returned nil!', vReturnValue);
  AssertEquals('Name is incorrect!', vName, vReturnValue.Name);

  vItem.Free;  // Cleanup
end;

procedure TestTInstantAttributeMap.TestRemove;
var
  vReturnValue: Integer;
  vItem: TInstantAttributeMetadata;
begin
  vItem := FInstantAttributeMap[1];
  AssertNotNull('Find returned nil!', vItem);

  vReturnValue := FInstantAttributeMap.Remove(vItem);
  AssertEquals('Remove return value is incorrect!', 1, vReturnValue);
  AssertEquals('AttributeMap count is incorrect!', 2,
    FInstantAttributeMap.Count);

  vItem.Free;  // Cleanup
end;

procedure TestTInstantAttributeMaps.SetUp;
var
  vItem: TInstantAttributeMap;
  i: Integer;
begin
  FClassMetadata := TInstantClassMetadata.Create(nil);
  FClassMetadata.TableName := 'Item3';

  FInstantAttributeMaps := TInstantAttributeMaps.Create(FClassMetadata);

  // The root map is named Item3
  for i := 1 to 3 do
  begin
    vItem := FInstantAttributeMaps.Add;
    vItem.Name := 'Item' + IntToStr(i);
  end;
  AssertEquals('AttributeMaps count is incorrect!', 3,
    FInstantAttributeMaps.Count);
end;

procedure TestTInstantAttributeMaps.TearDown;
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to Pred(FInstantAttributeMaps.Count) do
    for j := 0 to Pred(FInstantAttributeMaps[i].Count) do
      FInstantAttributeMaps[i][j].Free;
  FreeAndNil(FInstantAttributeMaps);
  FreeAndNil(FClassMetadata);
end;

procedure TestTInstantAttributeMaps.TestAdd;
var
  vItem: TInstantAttributeMap;
begin
  vItem := FInstantAttributeMaps.Add;
  AssertNotNull('AttributeMaps is nil!', vItem);
  AssertEquals('AttributeMaps count is incorrect!', 4,
    FInstantAttributeMaps.Count);
end;

procedure TestTInstantAttributeMaps.TestAddItem;
var
  vReturnValue: Integer;
  vItem: TInstantAttributeMap;
begin
  vItem := TInstantAttributeMap.Create(nil);
  vReturnValue := FInstantAttributeMaps.Add(vItem);
  AssertEquals('AttributeMaps item index is incorrect!', 3, vReturnValue);
  AssertEquals('AttributeMaps count is incorrect!', 4,
    FInstantAttributeMaps.Count);
end;

procedure TestTInstantAttributeMaps.TestEnsureMap;
var
  vReturnValue: TInstantAttributeMap;
  vName: string;
begin
  vName := 'Item2';   // Existent name
  vReturnValue := FInstantAttributeMaps.EnsureMap(vName);
  AssertNotNull('AttributeMap return value is nil!', vReturnValue);
  AssertEquals('AttributeMap name is incorrect!', vName, vReturnValue.Name);
  AssertEquals('AttributeMaps count is incorrect!', 3,
    FInstantAttributeMaps.Count);

  vName := 'NewName';   // Non-existent name
  //vItem := TInstantAttributeMap.Create(nil);
  vReturnValue := FInstantAttributeMaps.EnsureMap(vName);
  AssertNotNull('AttributeMap return value is nil!', vReturnValue);
  AssertEquals('AttributeMap name is incorrect!', vName, vReturnValue.Name);
  AssertEquals('AttributeMaps count is incorrect!', 4,
    FInstantAttributeMaps.Count);
end;

procedure TestTInstantAttributeMaps.TestFind;
var
  vReturnValue: TInstantAttributeMap;
  vName: string;
begin
  // Find with existing name
  vName := 'Item2';
  vReturnValue := FInstantAttributeMaps.Find(vName);
  AssertNotNull('Find returned nil!', vReturnValue);
  AssertEquals('AttributeMetadata name is incorrect!', vName,
    vReturnValue.Name);

  // Find with non-existing name
  vReturnValue := FInstantAttributeMaps.Find('NotIn');
  AssertNull('Find returned not nil!', vReturnValue);
end;

procedure TestTInstantAttributeMaps.TestFindMap;
var
  vReturnValue: TInstantAttributeMap;
  vAttributeName: string;
  i: Integer;
  j: Integer;
  vItem: TInstantAttributeMetadata;
begin
  // Ensure that an attribute map that has the desired attribute name
  // cannot be found
  vAttributeName := 'AttrItem11';
  vReturnValue := FInstantAttributeMaps.FindMap(vAttributeName);
  AssertNull('Map was found!', vReturnValue);

  // Add some uniquely named attribute metadatas to each attribute map
  for j := 0 to 2 do
  begin
    for i := 0 to 2 do
    begin
      vItem := TInstantAttributeMetadata.Create(nil);
      vItem.Name := 'AttrItem' + IntToStr(j) + IntToStr(i);
      FInstantAttributeMaps[j].Add(vItem);
    end;
    AssertEquals(Format('AttributeMap[%d] count', [j]), 3,
      FInstantAttributeMaps[j].Count);
  end;

  // Now find the attribute map that has the desired attribute name
  vReturnValue := FInstantAttributeMaps.FindMap(vAttributeName);
  AssertNotNull('Map not found!', vReturnValue);
  AssertEquals('Returned name', 'Item2', vReturnValue.Name);
end;

procedure TestTInstantAttributeMaps.TestIndexOf;
var
  vReturnValue: Integer;
  vItem: TInstantAttributeMap;
begin
  vItem := FInstantAttributeMaps[1];
  AssertNotNull('Find returned nil!', vItem);

  vReturnValue := FInstantAttributeMaps.IndexOf(vItem);
  AssertEquals('AttributeMetadata index is incorrect!', 1, vReturnValue);
end;

procedure TestTInstantAttributeMaps.TestInsert;
var
  vItem: TInstantAttributeMap;
  vIndex: Integer;
begin
  vItem := TInstantAttributeMap.Create(nil);

  vIndex := 1;
  FInstantAttributeMaps.Insert(vIndex, vItem);
  AssertEquals('AttributeMaps count is incorrect!', 4,
    FInstantAttributeMaps.Count);

  vIndex := FInstantAttributeMaps.IndexOf(vItem);
  AssertEquals('AttributeMaps index is incorrect!', 1, vIndex);
end;

procedure TestTInstantAttributeMaps.TestItems;
var
  vItem, vNewItem,
  vReturnValue: TInstantAttributeMap;
  vName: string;
begin
  vName := 'NewItem';
  vItem := FInstantAttributeMaps[1];
  AssertNotNull('Find returned nil!', vItem);
  AssertEquals('Name is incorrect!', 'Item2', vItem.Name);
  vNewItem := TInstantAttributeMap.Create(nil);
  AssertNotNull('Find returned nil!', vNewItem);
  vNewItem.Name := vName;

  FInstantAttributeMaps[1] := vNewItem;
  AssertEquals('AttributeMaps Count is incorrect!', 3,
    FInstantAttributeMaps.Count);
  vReturnValue := FInstantAttributeMaps.Find(vName);
  AssertNotNull('Find returned nil!', vReturnValue);
  AssertEquals('Name is incorrect!', vName, vReturnValue.Name);
end;

procedure TestTInstantAttributeMaps.TestRemove;
var
  vReturnValue: Integer;
  vItem: TInstantAttributeMap;
begin
  vItem := FInstantAttributeMaps[1];
  AssertNotNull('Find returned nil!', vItem);

  vReturnValue := FInstantAttributeMaps.Remove(vItem);
  AssertEquals('Remove return value is incorrect!', 1, vReturnValue);
  AssertEquals('AttributeMap count is incorrect!', 2,
    FInstantAttributeMaps.Count);
end;

procedure TestTInstantAttributeMaps.TestRootMap;
var
  vItem: TInstantAttributeMap;
begin
  vItem := FInstantAttributeMaps.RootMap;
  AssertNotNull('Root map was not found!', vItem);
  AssertEquals('Root map value is incorrect!', 'Item3', vItem.Name);

  FInstantAttributeMaps[2].Name := 'NewName';
  vItem := FInstantAttributeMaps.RootMap;
  AssertNull('Root map was found!', vItem);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantAttributeMap,
                 TestTInstantAttributeMaps]);
{$ENDIF}

end.
 