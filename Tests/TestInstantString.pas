(*
 *   InstantObjects Test Suite
 *   TestInstantString
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
 * The Original Code is: InstantObjects Test Suite/TestInstantString
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

unit TestInstantString;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantString
  [TestFixture]
  TestTInstantString = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantString: TInstantString;
    FOwner: TContact;
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
    procedure TestName;
    procedure TestOwner;
    procedure TestReset;
    procedure TestValue;
  end;

implementation

uses
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, InstantClasses;

procedure TestTInstantString.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantString := FOwner._Name;
  FInstantString.Value := 'StringValue';
end;

procedure TestTInstantString.TearDown;
begin
  FInstantString := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantString.TestAsBoolean;
begin
  FInstantString.AsBoolean := True;
  AssertEquals('True', FInstantString.Value);
  AssertTrue(FInstantString.AsBoolean);

  FInstantString.AsBoolean := False;
  AssertEquals('False', FInstantString.Value);
  AssertFalse(FInstantString.AsBoolean);
end;

procedure TestTInstantString.TestAsCurrency;
var
  vCurr: Currency;
begin
  vCurr := 23.45;
  FInstantString.AsCurrency := vCurr;
  AssertEquals('23' + {$IFDEF D15+}FormatSettings.{$ENDIF}DecimalSeparator +
    '45', FInstantString.Value);
  AssertEquals(vCurr, FInstantString.AsCurrency);
end;

procedure TestTInstantString.TestAsDateTime;
begin
  FInstantString.AsDateTime := 12.45;
  AssertEquals(DateTimeToStr(12.45), FInstantString.Value);
  AssertEquals(12.45, FInstantString.AsDateTime);
end;

procedure TestTInstantString.TestAsFloat;
begin
  FInstantString.AsFloat := 89.45;
  AssertEquals('89' + {$IFDEF D15+}FormatSettings.{$ENDIF}DecimalSeparator +
    '45', FInstantString.Value);
  AssertEquals(89.45, FInstantString.AsFloat);
end;

procedure TestTInstantString.TestAsInteger;
begin
  FInstantString.AsInteger := 100;
  AssertEquals('100', FInstantString.Value);
  AssertEquals(100, FInstantString.AsInteger);
end;

procedure TestTInstantString.TestAsObject;
var
  vObj: TInstantObject;
begin
  vObj := TInstantObject.Create(FConn);
  try
    Assert.WillRaise(
      procedure begin
        FInstantString.AsObject := vObj;
      end,
      EInstantAccessError,
      'Exception was not thrown for Set InstantString.AsObject!'
      );

    Assert.WillRaise(
      procedure begin
        FInstantString.AsObject;
      end,
      EInstantAccessError,
      'Exception was not thrown for Get InstantString.AsObject!'
      );
  finally
    vObj.Free;
  end;
end;

procedure TestTInstantString.TestAssign;
var
  vSource: TInstantString;
begin
  AssertEquals('StringValue', FInstantString.Value);

  vSource := TInstantString.Create;
  try
    VSource.Value := 'DifferentString';
    FInstantString.Assign(vSource);
    AssertEquals('DifferentString', FInstantString.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantString.TestAsString;
begin
  FInstantString.AsString := 'DifferentString';
  AssertEquals('DifferentString', FInstantString.Value);
  AssertEquals('DifferentString', FInstantString.AsString);
end;

procedure TestTInstantString.TestAsVariant;
begin
  FInstantString.AsVariant := 'DifferentString';
{$IFDEF VER130}
  AssertEquals('DifferentString', VarToStr(FInstantString.Value));
  AssertEquals('DifferentString', VarToStr(FInstantString.AsVariant));
{$ELSE}
  AssertEquals('DifferentString', FInstantString.Value);
  AssertEquals('DifferentString', FInstantString.AsVariant);
{$ENDIF}
end;

procedure TestTInstantString.TestName;
begin
  AssertEquals('Name', FInstantString.Name);
end;

procedure TestTInstantString.TestOwner;
begin
  AssertSame(FOwner, FInstantString.Owner);
end;

procedure TestTInstantString.TestReset;
begin
  AssertNotNull(FInstantString.Metadata);
  // Metadata.DefaultValue is '';
  FInstantString.Reset;
  AssertEquals('', FInstantString.Value);

  FInstantString.Metadata.DefaultValue := '1000';
  FInstantString.Reset;
  AssertEquals('1000', FInstantString.Value);

  FInstantString.Metadata := nil;
  AssertNull(FInstantString.Metadata);
  FInstantString.Reset;
  AssertEquals('', FInstantString.Value);
end;

procedure TestTInstantString.TestValue;
begin
  AssertEquals('StringValue', FInstantString.Value);
  FInstantString.Value := 'NewValue';
  AssertEquals('NewValue', FInstantString.Value);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantString]);
{$ENDIF}

end.
 