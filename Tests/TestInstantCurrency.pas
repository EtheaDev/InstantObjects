(*
 *   InstantObjects Test Suite
 *   TestInstantCurrency
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
 * The Original Code is: InstantObjects Test Suite/TestInstantCurrency
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

unit TestInstantCurrency;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantCurrency
  [TestFixture]
  TestTInstantCurrency = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantCurrency: TInstantCurrency;
    FOwner: TPerson;
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
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, InstantClasses;

procedure TestTInstantCurrency.SetUp;
var
  vCurr: Currency;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TPerson.Create(FConn);
  FInstantCurrency := FOwner._Salary;
  vCurr := 1.3;
  FInstantCurrency.Value := vCurr;
end;

procedure TestTInstantCurrency.TearDown;
begin
  FInstantCurrency := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantCurrency.TestAsCurrency;
var
  vCurr: Currency;
begin
  vCurr := 23.45;
  FInstantCurrency.AsCurrency := vCurr;
  AssertEquals(vCurr, FInstantCurrency.Value);
  AssertEquals(vCurr, FInstantCurrency.AsCurrency);
end;

procedure TestTInstantCurrency.TestAsFloat;
begin
  FInstantCurrency.AsFloat := 89.45;
  AssertEquals(89.45, FInstantCurrency.Value);
  AssertEquals(89.45, FInstantCurrency.AsFloat);
end;

procedure TestTInstantCurrency.TestAsInteger;
begin
  FInstantCurrency.AsInteger := 89;
  AssertEquals(89.0, FInstantCurrency.Value);
  AssertEquals(89.0, FInstantCurrency.AsInteger);
end;

procedure TestTInstantCurrency.TestAssign;
var
  vSource: TInstantCurrency;
  vCurr: Currency;
begin
  AssertEquals(1.3, FInstantCurrency.Value);
  vSource := TInstantCurrency.Create;
  try
    vCurr := 4.3;
    VSource.Value := vCurr;
    FInstantCurrency.Assign(vSource);
    AssertEquals(vCurr, FInstantCurrency.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantCurrency.TestAsString;
begin
  FInstantCurrency.AsString := '15' +
    {$IFDEF D15+}FormatSettings.{$ENDIF}DecimalSeparator + '7';
  AssertEquals(15.7, FInstantCurrency.Value);
  AssertEquals('15' + {$IFDEF D15+}FormatSettings.{$ENDIF}DecimalSeparator +
    '7', FInstantCurrency.AsString);
end;

procedure TestTInstantCurrency.TestAsVariant;
begin                                           
  FInstantCurrency.AsVariant := 15.1;
  AssertEquals(15.1, FInstantCurrency.Value);
  AssertEquals(15.1, FInstantCurrency.AsVariant);
end;

procedure TestTInstantCurrency.TestReset;
begin
  AssertNotNull(FInstantCurrency.Metadata);
  // Metadata.DefaultValue is '';
  FInstantCurrency.Reset;
  AssertEquals(1.3, FInstantCurrency.Value);

  FInstantCurrency.Metadata.DefaultValue := '15' +
    {$IFDEF D15+}FormatSettings.{$ENDIF}DecimalSeparator + '7';
  FInstantCurrency.Reset;
  AssertEquals(15.7, FInstantCurrency.Value);

  FInstantCurrency.Metadata := nil;
  AssertNull(FInstantCurrency.Metadata);
  FInstantCurrency.Reset;
  AssertEquals(0.0, FInstantCurrency.Value);
end;

procedure TestTInstantCurrency.TestValue;
var
  vCurr: Currency;
begin
  AssertEquals(1.3, FInstantCurrency.Value);

  vCurr := 123456789012.12345;
  FInstantCurrency.AsCurrency := vCurr;
  AssertEquals(vCurr, FInstantCurrency.Value);
  AssertEquals(vCurr, FInstantCurrency.AsCurrency);
  vCurr := vCurr + 0.0001;
  AssertFalse('Precision limit', vCurr = FInstantCurrency.Value);
  FInstantCurrency.AsCurrency := vCurr;
  vCurr := vCurr + 0.00001;
  AssertEquals('Out of precision limit', vCurr, FInstantCurrency.Value);
  vCurr := -0.0001;
  FInstantCurrency.AsCurrency := vCurr;
  AssertEquals(vCurr, FInstantCurrency.Value);
  AssertFalse(0 = FInstantCurrency.Value);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantCurrency]);
{$ENDIF}

end.
 