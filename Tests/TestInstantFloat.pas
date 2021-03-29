(*
 *   InstantObjects Test Suite
 *   TestInstantFloat
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
 * The Original Code is: InstantObjects Test Suite/TestInstantFloat
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

unit TestInstantFloat;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantFloat
  TestTInstantFloat = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantFloat: TInstantFloat;
    FOwner: TPerson;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
  SysUtils, testregistry, InstantClasses;

procedure TestTInstantFloat.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TPerson.Create(FConn);
  FInstantFloat := FOwner._AL_hours;
  FInstantFloat.Value := 1.3;
end;

procedure TestTInstantFloat.TearDown;
begin
  FInstantFloat := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantFloat.TestAsCurrency;
var
  vCurr: Currency;
begin
  vCurr := 23.45;
  FInstantFloat.AsCurrency := vCurr;
  AssertEquals(23.45, FInstantFloat.Value);
  AssertEquals(vCurr, FInstantFloat.AsCurrency);
end;

procedure TestTInstantFloat.TestAsFloat;
begin
  FInstantFloat.AsFloat := 89.45;
  AssertEquals(89.45, FInstantFloat.Value);
  AssertEquals(89.45, FInstantFloat.AsFloat);
end;

procedure TestTInstantFloat.TestAsInteger;
begin
  FInstantFloat.AsInteger := 89;
  AssertEquals(89.0, FInstantFloat.Value);
  AssertEquals(89.0, FInstantFloat.AsInteger);
end;

procedure TestTInstantFloat.TestAssign;
var
  vSource: TInstantFloat;
begin
  AssertEquals(1.3, FInstantFloat.Value);
  vSource := TInstantFloat.Create;
  try
    VSource.Value := 4.3;
    FInstantFloat.Assign(vSource);
    AssertEquals(4.3, FInstantFloat.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantFloat.TestAsString;
begin
  FInstantFloat.AsString := '1' +
    {$IFDEF D15+}FormatSettings.{$ENDIF}DecimalSeparator + '3';
  AssertEquals(1.3, FInstantFloat.Value);
  AssertEquals('1' + {$IFDEF D15+}FormatSettings.{$ENDIF}DecimalSeparator +
    '3', FInstantFloat.AsString);
end;

procedure TestTInstantFloat.TestAsVariant;
begin                                           
  FInstantFloat.AsVariant := 15.1;
  AssertEquals(15.1, FInstantFloat.Value);
  AssertEquals(15.1, FInstantFloat.AsVariant);
end;

procedure TestTInstantFloat.TestReset;
begin
  AssertNotNull(FInstantFloat.Metadata);
  // Metadata.DefaultValue is '';
  FInstantFloat.Reset;
  AssertEquals(1.3, FInstantFloat.Value);

  FInstantFloat.Metadata.DefaultValue := '15' +
    {$IFDEF D15+}FormatSettings.{$ENDIF}DecimalSeparator + '7';
  FInstantFloat.Reset;
  AssertEquals(15.7, FInstantFloat.Value);

  FInstantFloat.Metadata := nil;
  AssertNull(FInstantFloat.Metadata);
  FInstantFloat.Reset;
  AssertEquals(0.0, FInstantFloat.Value);
end;

procedure TestTInstantFloat.TestValue;
begin
  AssertEquals(1.3, FInstantFloat.Value);
  FInstantFloat.Value := 97.2;
  AssertEquals(97.2, FInstantFloat.Value);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantFloat]);
{$ENDIF}

end.
 