(*
 *   InstantObjects Test Suite
 *   TestInstantInteger
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
 * The Original Code is: InstantObjects Test Suite/TestInstantInteger
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

unit TestInstantInteger;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantInteger
  [TestFixture]
  TestTInstantInteger = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantInteger: TInstantInteger;
    FOwner: TCompany;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAsCurrency;
    procedure TestAsFloat;
    procedure TestAsInteger;
    procedure TestAssign;
    procedure TestAsString;
    procedure TestAsVariant;
    procedure TestReset;
    procedure TestValue;
  end;

implementation

uses
  System.Classes,
  SysUtils, InstantClasses;

procedure TestTInstantInteger.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TCompany.Create(FConn);
  FInstantInteger := FOwner._NoOfBranches;
  FInstantInteger.Value := 1;
end;

procedure TestTInstantInteger.TearDown;
begin
  FInstantInteger := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantInteger.TestAsCurrency;
var
  vCurr: Currency;
begin
  vCurr := 23.45;
  FInstantInteger.AsCurrency := vCurr;
  AssertEquals(23, FInstantInteger.Value);
  AssertEquals(Currency(23.0), FInstantInteger.AsCurrency);
end;

procedure TestTInstantInteger.TestAsFloat;
begin
  FInstantInteger.AsFloat := 89.45;
  AssertEquals(89, FInstantInteger.Value);
  AssertEquals(89.0, FInstantInteger.AsFloat);
end;

procedure TestTInstantInteger.TestAsInteger;
begin
  FInstantInteger.AsInteger := 100;
  AssertEquals(100, FInstantInteger.Value);
  AssertEquals(100, FInstantInteger.AsInteger);
end;

procedure TestTInstantInteger.TestAssign;
var
  vSource: TInstantInteger;
begin
  AssertEquals(1, FInstantInteger.Value);

  vSource := TInstantInteger.Create;
  try
    VSource.Value := 200;
    FInstantInteger.Assign(vSource);
    AssertEquals(200, FInstantInteger.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantInteger.TestAsString;
begin
  FInstantInteger.AsString := '73';
  AssertEquals(73, FInstantInteger.Value);
  AssertEquals('73', FInstantInteger.AsString);
end;

procedure TestTInstantInteger.TestAsVariant;
begin                                           
  FInstantInteger.AsVariant := 15;
  AssertEquals(15, FInstantInteger.Value);
  AssertEquals(15, FInstantInteger.AsVariant);
end;

procedure TestTInstantInteger.TestReset;
begin
  AssertNotNull(FInstantInteger.Metadata);
  // Metadata.DefaultValue is '';
  FInstantInteger.Reset;
  AssertEquals(0, FInstantInteger.Value);

  FInstantInteger.Metadata.DefaultValue := '1000';
  FInstantInteger.Reset;
  AssertEquals(1000, FInstantInteger.Value);

  FInstantInteger.Metadata := nil;
  AssertNull(FInstantInteger.Metadata);
  FInstantInteger.Reset;
  AssertEquals(0, FInstantInteger.Value);
end;

procedure TestTInstantInteger.TestValue;
begin
  AssertEquals(1, FInstantInteger.Value);
  FInstantInteger.Value := 1000;
  AssertEquals(1000, FInstantInteger.Value);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantInteger]);
{$ENDIF}

end.
 