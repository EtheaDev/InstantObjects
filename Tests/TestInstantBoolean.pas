(*
 *   InstantObjects Test Suite
 *   TestInstantBoolean
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
 * The Original Code is: InstantObjects Test Suite/TestInstantBoolean
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

unit TestInstantBoolean;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

Type

  // Test methods for class TInstantBoolean
  [TestFixture]
  TestTInstantBoolean = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantBoolean: TInstantBoolean;
    FOwner: TPerson;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAsBoolean;
    procedure TestAsCurrency;
    procedure TestAsDateTime;
    procedure TestAsFloat;
    procedure TestAsInteger;
    procedure TestAsObject;
    procedure TestAssign;
    procedure TestAsString;
    procedure TestAsVariant;
    procedure TestDisplayText;
    procedure TestIsDefault;
    procedure TestReset;
    procedure TestValue;
  end;

implementation

uses
  System.Classes,
  SysUtils, InstantClasses, InstantConsts;

procedure TestTInstantBoolean.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TPerson.Create(FConn);
  FInstantBoolean := FOwner._Employed;
  FInstantBoolean.Value := False;
end;

procedure TestTInstantBoolean.TearDown;
begin
  FInstantBoolean := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantBoolean.TestAsBoolean;
begin
  FInstantBoolean.AsBoolean := True;
  AssertEquals(True, FInstantBoolean.Value);
  AssertTrue(FInstantBoolean.AsBoolean);

  FInstantBoolean.AsBoolean := False;
  AssertEquals(False, FInstantBoolean.Value);
  AssertFalse(FInstantBoolean.AsBoolean);
end;

procedure TestTInstantBoolean.TestAsCurrency;
var
  vCurr: Currency;
begin
  vCurr := 1;
  FInstantBoolean.AsCurrency := vCurr;
  AssertTrue(FInstantBoolean.Value);
  AssertEquals(vCurr, FInstantBoolean.AsCurrency);
end;

procedure TestTInstantBoolean.TestAsDateTime;
begin
  Assert.WillRaise(
    procedure
    begin
      FInstantBoolean.AsDateTime := 12.45;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantBoolean.AsDateTime!');

  Assert.WillRaise(
    procedure
    begin
      FInstantBoolean.AsDateTime;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantBoolean.AsDateTime!');
end;

procedure TestTInstantBoolean.TestAsFloat;
begin
  FInstantBoolean.AsFloat := 1;
  AssertTrue(FInstantBoolean.Value);
  AssertEquals(1, FInstantBoolean.AsFloat);
end;

procedure TestTInstantBoolean.TestAsInteger;
begin
  FInstantBoolean.AsInteger := 1;
  AssertTrue(FInstantBoolean.Value);
  AssertEquals(1, FInstantBoolean.AsInteger);
end;

procedure TestTInstantBoolean.TestAsObject;
var
  vObj: TInstantObject;
begin
  vObj := TInstantObject.Create(FConn);
  try
    Assert.WillRaise(
      procedure begin
        FInstantBoolean.AsObject := vObj;
      end,
      EInstantAccessError,
      'Exception was not thrown for Set InstantBoolean.AsObject!'
      );

    Assert.WillRaise(
      procedure begin
        FInstantBoolean.AsObject;
      end,
      EInstantAccessError,
      'Exception was not thrown for Get InstantBoolean.AsObject!'
      );
  finally
    vObj.Free;
  end;
end;

procedure TestTInstantBoolean.TestAssign;
var
  vSource: TInstantBoolean;
begin
  AssertFalse(FInstantBoolean.Value);

  vSource := TInstantBoolean.Create;
  try
    VSource.Value := True;
    FInstantBoolean.Assign(vSource);
    AssertTrue(FInstantBoolean.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantBoolean.TestAsString;
begin
  FInstantBoolean.AsString := InstantTrueString;
  AssertTrue(FInstantBoolean.Value);
  AssertTrue(SameText(InstantTrueString, FInstantBoolean.AsString));
end;

procedure TestTInstantBoolean.TestAsVariant;
begin                                           
  FInstantBoolean.AsVariant := True;
  AssertTrue(FInstantBoolean.Value);
  AssertTrue(FInstantBoolean.AsVariant);
end;

procedure TestTInstantBoolean.TestDisplayText;
begin
  AssertTrue(SameText(InstantFalseString, FInstantBoolean.DisplayText));

  FInstantBoolean.Metadata.EditMask := '!CC';
  AssertEquals('se', FInstantBoolean.DisplayText);

  FInstantBoolean.Value := True;
  FInstantBoolean.Metadata.EditMask := 'CCC';
  AssertEquals('Tru', FInstantBoolean.DisplayText);
end;

procedure TestTInstantBoolean.TestIsDefault;
begin
  AssertTrue(FInstantBoolean.IsDefault);

  FInstantBoolean.Value := True;
  AssertFalse(FInstantBoolean.IsDefault);
end;

procedure TestTInstantBoolean.TestReset;
begin
  AssertNotNull(FInstantBoolean.Metadata);
  // Metadata.DefaultValue is '';
  FInstantBoolean.Reset;
  AssertEquals(False, FInstantBoolean.Value);

  FInstantBoolean.Metadata.DefaultValue := InstantTrueString;
  FInstantBoolean.Reset;
  AssertEquals(True, FInstantBoolean.Value);

  FInstantBoolean.Metadata := nil;
  AssertNull(FInstantBoolean.Metadata);
  FInstantBoolean.Reset;
  AssertEquals(False, FInstantBoolean.Value);
end;

procedure TestTInstantBoolean.TestValue;
begin
  Assert.AreEqual(False, FInstantBoolean.Value);
  FInstantBoolean.Value := True;
  Assert.AreEqual(True, FInstantBoolean.Value);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantBoolean]);
{$ENDIF}

end.
 