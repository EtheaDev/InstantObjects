(*
 *   InstantObjects Test Suite
 *   TestInstantClassMetadata
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
 * The Original Code is: InstantObjects Test Suite/TestInstantClassMetadata
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

unit TestInstantClassMetadata;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, InstantMetadata,
  DUnitX.TestFramework;

type
  // Test methods for class TInstantClassMetadata
  [TestFixture]
  TestTInstantClassMetadata = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantClassMetadata: TInstantClassMetadata;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAssign;
    procedure TestAttributeMetadatas;
    procedure TestCollection;
    procedure TestFindInstantAttributeMetadata;
    procedure TestIsEmpty;
    procedure TestIsStored;
    procedure TestMemberMap;
    procedure TestParentName;
    procedure TestPersistence;
    procedure TestStorageMaps;
    procedure TestStorageName;
    procedure TestTableName;
  end;

  // Test methods for class TInstantClassMetadatas
  [TestFixture]
  TestTInstantClassMetadatas = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantClassMetadatas: TInstantClassMetadatas;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAdd;
    procedure TestFind;
    procedure TestItems;
  end;

implementation

uses
  System.Classes,
  SysUtils, TypInfo, InstantTypes;

procedure TestTInstantClassMetadata.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  // Load a default ClassMetadata
  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TContact');
end;

procedure TestTInstantClassMetadata.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantClassMetadata.TestAssign;
var
  vDest, vSource: TInstantClassMetadata;
  vStr: string;
begin
  vSource := TInstantClassMetadata.Create(nil);
  vDest := TInstantClassMetadata.Create(nil);
  try
    vSource.DefaultContainerName := 'DefaultContainerName';
    vSource.StorageName := 'StorageName';
    vSource.Persistence := peStored;
    vDest.Assign(vSource);
    AssertEquals('DefaultContainerName', vDest.DefaultContainerName);
    AssertEquals('StorageName', vDest.StorageName);
    vStr := GetEnumName(TypeInfo(TInstantPersistence),
      Ord(vDest.Persistence));
    AssertEquals('peStored', vStr);
  finally
    vSource.Free;
    vDest.Free;
  end;
end;

procedure TestTInstantClassMetadata.TestAttributeMetadatas;
begin
  AssertNotNull(FInstantClassMetadata.AttributeMetadatas);
end;

procedure TestTInstantClassMetadata.TestCollection;
var
  vCnt: Integer;
begin
  // Collection property contains all of the class metadatas in the model
  AssertNotNull(FInstantClassMetadata.Collection);
  vCnt := FInstantClassMetadata.Collection.Count;

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TPhone');
  AssertEquals(vCnt, FInstantClassMetadata.Collection.Count);
end;

procedure TestTInstantClassMetadata.TestFindInstantAttributeMetadata;
begin
  AssertNotNull(FInstantClassMetadata);
end;

procedure TestTInstantClassMetadata.TestIsEmpty;
begin
  AssertFalse(FInstantClassMetadata.IsEmpty);
end;

procedure TestTInstantClassMetadata.TestIsStored;
begin
  AssertTrue(FInstantClassMetadata.IsStored);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TContactFilter');
  AssertFalse(FInstantClassMetadata.IsStored);
end;

procedure TestTInstantClassMetadata.TestMemberMap;
begin
  AssertNotNull(FInstantClassMetadata.MemberMap);
end;

procedure TestTInstantClassMetadata.TestParentName;
begin
  AssertEquals('', FInstantClassMetadata.ParentName);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TPerson');
  AssertEquals('TContact', FInstantClassMetadata.ParentName);
end;

procedure TestTInstantClassMetadata.TestPersistence;
var
  vStr: string;
begin
  vStr := GetEnumName(TypeInfo(TInstantPersistence),
    Ord(FInstantClassMetadata.Persistence));
  AssertEquals('peStored', vStr);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TContactFilter');
  vStr := GetEnumName(TypeInfo(TInstantPersistence),
    Ord(FInstantClassMetadata.Persistence));
  AssertEquals('peEmbedded', vStr);
end;

procedure TestTInstantClassMetadata.TestStorageMaps;
begin
  AssertNotNull(FInstantClassMetadata.StorageMaps);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TContactFilter');
  AssertNull(FInstantClassMetadata.StorageMaps);
end;

procedure TestTInstantClassMetadata.TestStorageName;
begin
  // Test with default class StorageName returns ''.
  AssertEquals('', FInstantClassMetadata.StorageName);

  // Test for User entered non-default class StorageName.
  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TCategory');
  AssertEquals('Categories', FInstantClassMetadata.StorageName);
end;

procedure TestTInstantClassMetadata.TestTableName;
begin
  // Test with default class StorageName (TableName).
  AssertEquals('Contact', FInstantClassMetadata.TableName);

  // Test for User entered non-default class StorageName (TableName).
  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TCategory');
  AssertEquals('Categories', FInstantClassMetadata.TableName);
end;

procedure TestTInstantClassMetadatas.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  
  FInstantClassMetadatas := InstantModel.ClassMetadatas;
end;

procedure TestTInstantClassMetadatas.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantClassMetadatas.TestAdd;
var
  vReturnValue: TInstantClassMetadata;
  vCnt: Integer;
begin
  vCnt := FInstantClassMetadatas.Count;
  vReturnValue := FInstantClassMetadatas.Add;
  AssertNotNull(vReturnValue);
  AssertEquals(vCnt + 1, FInstantClassMetadatas.Count);
  FInstantClassMetadatas.Remove(vReturnValue);
  AssertEquals(vCnt, FInstantClassMetadatas.Count);
end;

procedure TestTInstantClassMetadatas.TestFind;
var
  vReturnValue: TInstantClassMetadata;
  vName: string;
begin
  vName := 'TAddress';
  vReturnValue := FInstantClassMetadatas.Find(vName);
  AssertEquals(vName, vReturnValue.Name);
end;

procedure TestTInstantClassMetadatas.TestItems;
begin
  AssertEquals('TCountry', FInstantClassMetadatas.Items[1].Name);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantClassMetadata,
                 TestTInstantClassMetadatas]);
{$ENDIF}

end.
