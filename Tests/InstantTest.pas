(*
 *   InstantObjects Test Suite
 *   InstantTest
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
 * The Original Code is: InstantObjects Test Suite/InstantMock
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantTest;

interface

uses
  System.SysUtils
  , DUnitX.TestFramework
  , DUnitX.DUnitCompatibility;

Resourcestring
  SCompare = ' expected: <%s> but was: <%s>';
  SExpectedNotSame = 'expected not same';
  SExceptionCompare = 'Exception %s expected but %s was raised';
  SMethodNotFound = 'Method <%s> not found';
  SNoValidInheritance = ' does not inherit from TTestCase';
  SNoValidTests = 'No valid tests found in ';

type
  TRunMethod = procedure of object;

  {$M+}
  {$TYPEINFO ON}
  TInstantTestCase = class(TTestCase)
  public
    procedure AssertTrue(const condition: Boolean; const msg: string = ''); overload;
    procedure AssertTrue(const msg: string; const condition: Boolean); overload;
    procedure AssertFalse(const condition: Boolean; const msg: string = ''); overload;
    procedure AssertFalse(const msg: string; const condition: Boolean); overload;
    procedure AssertEquals(const expected, actual: extended; const msg: string = ''); overload;
    procedure AssertEquals(const expected, actual: extended; const delta: extended; const msg: string = ''); overload;

    procedure AssertEquals(const expected, actual: TDateTime; const msg: string = ''); overload;
    procedure AssertEquals(const expected, actual: Currency; const msg: string = ''); overload;
    procedure AssertEquals(const expected, actual: Currency; const delta: Currency; const msg: string = ''); overload;
    procedure AssertEquals(const msg: string; const expected, actual: Currency); overload;

    procedure AssertEquals(const expected, actual: integer; const msg: string = ''); overload;
    procedure AssertEquals(const msg: string; const expected, actual: integer); overload;

    procedure AssertEquals(const expected, actual: Cardinal; const msg: string = ''); overload;
    procedure AssertEquals(const expected, actual: int64; const msg: string = ''); overload;

    procedure AssertEquals(const msg: string; const expected, actual: UnicodeString); overload;
{$IFNDEF NEXTGEN}
    procedure AssertEquals(const msg: string; const expected, actual: AnsiString); overload;
    procedure AssertEquals(const msg: string; const expected, actual: ShortString); overload;
{$ENDIF}
    procedure AssertEqualsString(const expected, actual: string; const msg: string = '');
{$IFNDEF NEXTGEN}
    procedure AssertEquals(const expected, actual: WideString; const msg: string = ''); overload;
    procedure AssertEqualsWideString(const expected, actual: WideString; const msg: string = '');
{$ENDIF}
    procedure AssertEqualsXML(const expected, actual: UnicodeString; const msg: string = ''); overload;

    procedure AssertEqualsMem(const expected, actual: pointer; const size:longword; const msg : string='');
    procedure AssertEquals(const expected, actual: Boolean; const msg: string = ''); overload;
    procedure AssertEqualsBin(const expected, actual: longword; const msg: string = ''; digits: integer=32);
    procedure AssertEqualsHex(const expected, actual: longword; const msg: string = ''; digits: integer=8);

    procedure AssertNotEquals(const expected, actual: integer; const msg: string = ''); overload;
    procedure AssertNotEquals(const expected, actual: Cardinal; const msg: string = ''); overload;
    procedure AssertNotEquals(const expected, actual: int64; const msg: string = ''); overload;
    procedure AssertNotEquals(const expected: extended; const actual: extended; const delta: extended = 0; const msg: string = ''); overload;
    procedure AssertNotEquals(const expected, actual: string; const msg: string = ''); overload;
    procedure AssertNotEqualsString(const expected, actual: string; const msg: string = '');
{$IFNDEF NEXTGEN}
    procedure AssertNotEquals(const expected, actual: WideString; const msg: string = ''); overload;
    procedure AssertNotEqualsWideString(const expected, actual: WideString; const msg: string = '');
{$ENDIF}
    procedure AssertNotEqualsMem(const expected, actual: pointer; const size:longword; const msg : string='');
    procedure AssertNotEquals(const expected, actual: Boolean; const msg: string = ''); overload;
    procedure AssertNotEqualsBin(const expected, actual: longword; const msg: string = ''; digits: integer=32);
    procedure AssertNotEqualsHex(const expected, actual: longword; const msg: string = ''; digits: integer=8);

    procedure AssertNotNull(const obj :IUnknown; const msg :string = ''); overload;
    procedure AssertNotNull(const msg :string; const obj :TObject); overload;
    procedure AssertNull(const obj: IUnknown; const msg: string = ''); overload;

    procedure AssertSame(const expected, actual: IInterface; const msg: string = ''); overload;
    procedure AssertSame(const expected, actual: TObject; const msg: string = ''); overload;
    procedure AssertSame(const msg: string; const expected, actual: TObject); overload;
    procedure AssertSame(const expected, actual: TClass; const msg: string = ''); overload;

    procedure AssertNotSame(const expected, actual: IInterface; const msg: string = ''); overload;
    procedure AssertNotSame(const expected, actual: TObject; const msg: string = ''); overload;
    procedure AssertNotSame(const msg: string; const expected, actual: TObject); overload;


    procedure AssertNotNull(const obj: TObject; const msg: string = ''); overload;
    procedure AssertNull(const obj: TObject; const msg: string = ''); overload;
    procedure AssertNull(const msg: string; const obj: TObject); overload;

    procedure AssertException(const AMethod: TTestMethod; const AExceptionClass: ExceptClass; const msg :string = ''); overload;

    procedure AssertEquals(const expected, actual: TClass; const msg: string = ''); overload;
    procedure AssertEquals(const msg: string; const expected, actual: TClass); overload;

    procedure AssertInherits(const expected, actual: TClass; const msg: string = ''); overload;
    procedure AssertIs(const AObject :TObject; const AClass: TClass; const msg: string = ''); overload;

    procedure AssertException(const AExceptionClass: ExceptClass; const AMethod: TRunMethod); overload;

    class procedure Fail(const AMessage: string);
  end;

implementation

//Borrowed from DUnit.

function IntToBin(const value, digits: longword): string;
const
  ALL_32_BIT_0 = '00000000000000000000000000000000';
var
  counter: integer;
  pow:     integer;
begin
  Result := ALL_32_BIT_0;
  SetLength(Result, digits);
  pow := 1 shl (digits - 1);
  if value <> 0 then
    for counter := 0 to digits - 1 do
    begin
      if (value and (pow shr counter)) <> 0 then
      begin
      	{$IFDEF NEXTGEN}
        Result.Remove(counter, 1);
        Result.Insert(counter, '1');
	{$ELSE}
	Result[counter+1] := '1';
	{$ENDIF}
      end;
    end;
end;

procedure TInstantTestCase.AssertTrue(const condition: Boolean; const msg: string);
begin
  Assert.IsTrue(condition,msg);
end;

procedure TInstantTestCase.AssertTrue(const msg: string; const condition: Boolean);
begin
  Assert.IsTrue(condition,msg);
end;

procedure TInstantTestCase.AssertFalse(const condition: Boolean; const msg: string);
begin
  Assert.IsFalse(condition,msg);
end;

procedure TInstantTestCase.AssertFalse(const msg: string; const condition: Boolean);
begin
  Assert.IsFalse(condition,msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: extended; const msg: string);
begin
  Assert.AreEqual(expected,actual,0,msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: extended; const delta: extended; const msg: string);
begin
  Assert.AreEqual(expected,actual,delta,msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: TDateTime; const msg: string);
begin
  Assert.AreEqual(expected,actual,msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: Currency; const msg: string);
begin
  Assert.AreEqual(expected,actual,0,msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: Currency; const delta: Currency; const msg: string);
begin
  Assert.AreEqual(expected,actual,delta,msg);
end;

procedure TInstantTestCase.AssertEquals(const msg: string; const expected, actual: Currency);
begin
  Assert.AreEqual(expected,actual,0,msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: integer; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<integer>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertEquals(const msg: string; const expected, actual: integer);
begin
  AssertEquals(expected, actual, msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: Cardinal; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<Cardinal>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: int64; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<Int64>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertEquals(const msg: string; const expected, actual: UnicodeString);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual(expected, actual, msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertEqualsXML(const expected, actual: UnicodeString;
  const msg: string);
var
  LCleanExpected, LCleanActual: string;

  function CleanString(const AValue: string): string;
  begin
    Result := AValue;
    Result := StringReplace(Result, sLineBreak, '', [rfReplaceAll]);
    Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
  end;

begin
  LCleanExpected := CleanString(expected);
  LCleanActual := CleanString(actual);
  AssertEqualsString(LCleanExpected, LCleanActual, msg);
end;

{$IFNDEF NEXTGEN}
procedure TInstantTestCase.AssertEquals(const msg: string; const expected, actual: AnsiString);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<AnsiString>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertEquals(const msg: string; const expected, actual: ShortString);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<ShortString>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;
{$ENDIF}

procedure TInstantTestCase.AssertEqualsString(const expected, actual: string; const msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;

{$IFNDEF NEXTGEN}
procedure TInstantTestCase.AssertEquals(const expected, actual: WideString; const msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;

procedure TInstantTestCase.AssertEqualsWideString(const expected, actual: WideString; const msg: string);
begin
  Assert.AreEqual(expected,actual,true,msg);
end;
{$ENDIF}

procedure TInstantTestCase.AssertEqualsMem(const expected, actual: pointer; const size:longword; const msg : string = '');
begin
  Assert.AreEqualMemory(expected,actual,size,msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: Boolean; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<Boolean>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertEqualsBin(const expected, actual: longword; const msg: string; digits: integer);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual(IntToBin(expected, digits), IntToBin(actual, digits),msg);
{$ELSE}
  Assert.IsTrue(IntToBin(expected, digits) = IntToBin(actual, digits), msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertEqualsHex(const expected, actual: longword; const msg: string; digits: integer);
begin
  Assert.AreEqual(IntToHex(expected, digits), IntToHex(actual, digits),true,msg);
end;

procedure TInstantTestCase.AssertNotEquals(const expected, actual: integer; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<integer>(expected,actual,msg);
{$ELSE}
  Assert.IsFalse(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertNotEquals(const expected, actual: Cardinal; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<Cardinal>(expected,actual,msg);
{$ELSE}
  Assert.IsFalse(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertNotEquals(const expected, actual: int64; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<int64>(expected,actual,msg);
{$ELSE}
  Assert.IsFalse(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertNotEquals(const expected: extended; const actual: extended; const delta: extended; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,delta,msg);
end;

procedure TInstantTestCase.AssertNotEquals(const expected, actual: string; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

procedure TInstantTestCase.AssertNotEqualsString(const expected, actual: string; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

{$IFNDEF NEXTGEN}
procedure TInstantTestCase.AssertNotEquals(const expected, actual: WideString; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;

procedure TInstantTestCase.AssertNotEqualsWideString(const expected, actual: WideString; const msg: string);
begin
  Assert.AreNotEqual(expected,actual,true,msg);
end;
{$ENDIF}

procedure TInstantTestCase.AssertNotEqualsMem(const expected, actual: pointer; const size:longword; const msg:string='');
begin
  Assert.AreNotEqualMemory(expected,actual,size,msg);
end;

procedure TInstantTestCase.AssertNotEquals(const expected, actual: Boolean; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<boolean>(expected,actual,msg);
{$ELSE}
  Assert.IsFalse(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertNotEqualsBin(const expected, actual: longword; const msg: string; digits: integer);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<longword>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertNotEqualsHex(const expected, actual: longword; const msg: string; digits: integer);
begin
  Assert.AreNotEqual(IntToHex(expected, digits), IntToHex(actual, digits),true,msg);
end;

procedure TInstantTestCase.AssertNotNull(const obj :IUnknown; const msg :string);
begin
  Assert.IsNotNull(obj,msg);
end;

procedure TInstantTestCase.AssertNotNull(const msg :string; const obj :TObject);
begin
  Assert.IsNotNull(obj,msg);
end;

procedure TInstantTestCase.AssertNull(const obj: IUnknown; const msg: string);
begin
  Assert.IsNull(obj,msg);
end;

procedure TInstantTestCase.AssertNull(const msg: string; const obj: TObject);
begin
  Assert.IsNull(obj,msg);
end;

procedure TInstantTestCase.AssertSame(const expected, actual: IUnknown; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<IInterface>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertSame(const expected, actual: TObject; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreEqual<TObject>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected = actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertSame(const msg: string; const expected, actual: TObject);
begin
  AssertSame(expected, actual, msg);
end;

procedure TInstantTestCase.AssertSame(const expected, actual: TClass;
  const msg: string);
begin
  Assert.AreEqual(expected, actual, msg);
end;

procedure TInstantTestCase.AssertNotSame(const expected, actual: IUnknown; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<IInterface>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected <> actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertNotSame(const expected, actual: TObject; const msg: string);
begin
{$IFNDEF DELPHI_XE_DOWN}
  Assert.AreNotEqual<TObject>(expected,actual,msg);
{$ELSE}
  Assert.IsTrue(expected <> actual, msg);
{$ENDIF}
end;

procedure TInstantTestCase.AssertNotSame(const msg: string; const expected, actual: TObject);
begin
  AssertNotSame(expected, actual, msg);
end;

procedure TInstantTestCase.AssertNotNull(const obj: TObject; const msg: string);
begin
  Assert.IsNotNull(obj,msg);
end;

procedure TInstantTestCase.AssertNull(const obj: TObject; const msg: string);
begin
  Assert.IsNull(obj,msg);
end;

procedure TInstantTestCase.AssertException(const AMethod: TTestMethod; const AExceptionClass: ExceptClass; const msg :string);
begin
  Assert.WillRaise(AMethod,AExceptionClass,msg);
end;

procedure TInstantTestCase.AssertEquals(const expected, actual: TClass; const msg: string);
begin
  Assert.AreEqual(expected,actual,msg);
end;

procedure TInstantTestCase.AssertEquals(const msg: string; const expected, actual: TClass);
begin
  Assert.AreEqual(expected,actual,msg);
end;

procedure TInstantTestCase.AssertInherits(const expected, actual: TClass; const msg: string);
begin
  Assert.InheritsFrom(expected,actual,msg);
end;

procedure TInstantTestCase.AssertIs(const AObject :TObject; const AClass: TClass; const msg: string);
begin
  Assert.InheritsFrom(AObject.ClassType,AClass,msg);
end;

procedure TInstantTestCase.AssertException(const AExceptionClass: ExceptClass; const AMethod: TRunMethod);
begin
  AssertException(AMethod, AExceptionClass);
end;

class procedure TInstantTestCase.Fail(const AMessage: String);
begin
  Assert.Fail(AMessage);
end;

end.

