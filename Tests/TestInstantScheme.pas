(*
 *   InstantObjects Test Suite
 *   TestInstantScheme
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
 * The Original Code is: InstantObjects Test Suite/TestInstantScheme
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

unit TestInstantScheme;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses fpcunit, InstantPersistence, InstantMetadata;

type

  // Test methods for class TInstantScheme
  TestTInstantScheme = class(TTestCase)
  private
    FInstantScheme: TInstantScheme;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBlobStreamFormat;
    procedure TestFindTableMetadata;
    procedure TestIdDataType;
    procedure TestIdSize;
    procedure TestTableMetadataCount;
    procedure TestTableMetadatas;
  end;

implementation

uses
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, testregistry, InstantClasses, InstantTypes, InstantConsts;

procedure TestTInstantScheme.SetUp;
begin
  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FInstantScheme := TInstantScheme.Create;
  FInstantScheme.Catalog := TInstantModelCatalog.Create(FInstantScheme, InstantModel);
end;

procedure TestTInstantScheme.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FInstantScheme);
end;

procedure TestTInstantScheme.TestBlobStreamFormat;
begin
  AssertTrue('BlobStreamFormat',
    sfBinary = FInstantScheme.BlobStreamFormat);
  FInstantScheme.BlobStreamFormat := sfXML;
  AssertTrue('BlobStreamFormat',
    sfXML = FInstantScheme.BlobStreamFormat);
  {$IFDEF DELPHI_NEON}
  FInstantScheme.BlobStreamFormat := sfJSON;
  AssertTrue('BlobStreamFormat',
    sfJSON = FInstantScheme.BlobStreamFormat);
  {$ENDIF}
end;

procedure TestTInstantScheme.TestFindTableMetadata;
var
  vReturnValue: TInstantTableMetadata;
  vName: string;
begin
  vName := 'Contact';
  vReturnValue := FInstantScheme.FindTableMetadata(vName);
  AssertNotNull('Could not find TableMetadata!', vReturnValue);
  AssertEquals('', vName, vReturnValue.Name);
end;

procedure TestTInstantScheme.TestIdDataType;
begin
  AssertTrue('IdDataType', dtString = FInstantScheme.IdDataType);
  FInstantScheme.IdDataType := dtInteger;
  AssertTrue('IdDataType', dtInteger = FInstantScheme.IdDataType);
end;

procedure TestTInstantScheme.TestIdSize;
begin
  AssertEquals(InstantDefaultFieldSize,
    FInstantScheme.IdSize);
  FInstantScheme.IdSize := 10;
  AssertEquals(10, FInstantScheme.IdSize);
end;

procedure TestTInstantScheme.TestTableMetadataCount;
var
  vCnt: Integer;
begin
  AssertTrue(FInstantScheme.TableMetadataCount > 0);
  // Test number of ClassMetadatas against number of TableMetadatas
  vCnt := FInstantScheme.TableMetadatas[0].Collection.Count;
  AssertEquals(vCnt, FInstantScheme.TableMetadataCount);
end;

procedure TestTInstantScheme.TestTableMetadatas;
var
  vReturnValue: TInstantTableMetadata;
begin
  vReturnValue := FInstantScheme.TableMetadatas[0];
  AssertNotNull(vReturnValue);
  AssertEquals('Country', vReturnValue.Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantScheme]);
{$ENDIF}

end.
 