(*
 *   InstantObjects Test Suite
 *   TestInstantDateTime
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
 * The Original Code is: InstantObjects Test Suite/TestInstantDateTime
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

unit TestInstantDateTime;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantDateTime
  [TestFixture]
  TestTInstantDateTime = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantDateTime: TInstantDateTime;
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
    procedure TestAsDate;
    procedure TestAsTime;
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
  SysUtils, InstantClasses;

procedure TestTInstantDateTime.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TPerson.Create(FConn);
  FInstantDateTime := FOwner._BirthDate;
  FInstantDateTime.Value := 100.5;
end;

procedure TestTInstantDateTime.TearDown;
begin
  FInstantDateTime := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantDateTime.TestAsBoolean;
begin
  Assert.WillRaise(
    procedure begin
      FInstantDateTime.AsBoolean := True;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantDateTime.AsBoolean!'
    );
  Assert.WillRaise(
    procedure begin
      FInstantDateTime.AsBoolean;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantDateTime.AsBoolean!'
    );
end;

procedure TestTInstantDateTime.TestAsCurrency;
begin
  Assert.WillRaise(
    procedure begin
      FInstantDateTime.AsCurrency := 20.5;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantDateTime.AsCurrency!'
    );
  Assert.WillRaise(
    procedure begin
      FInstantDateTime.AsCurrency;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantDateTime.AsCurrency!'
    );
end;

procedure TestTInstantDateTime.TestAsDateTime;
begin
  FInstantDateTime.AsDateTime := 12.45;
  AssertEquals(12.45, FInstantDateTime.Value);
  AssertEquals(12.45, FInstantDateTime.AsDateTime);
end;

procedure TestTInstantDateTime.TestAsDate;
begin
  FInstantDateTime.AsDate := 120.00;
  AssertEquals(120.00, FInstantDateTime.Value);
  AssertEquals(120.00, FInstantDateTime.AsDate);
end;

procedure TestTInstantDateTime.TestAsTime;
begin
  FInstantDateTime.AsTime := 0.45;
  AssertEquals(0.45, FInstantDateTime.Value);
  AssertEquals(0.45, FInstantDateTime.AsTime);
end;

procedure TestTInstantDateTime.TestAsFloat;
begin
  Assert.WillRaise(
    procedure begin
      FInstantDateTime.AsFloat := 20.5;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantDateTime.AsFloat!'
    );

  Assert.WillRaise(
    procedure begin
      FInstantDateTime.AsFloat;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantDateTime.AsFloat!'
    );
end;

procedure TestTInstantDateTime.TestAsInteger;
begin
  Assert.WillRaise(
    procedure begin
      FInstantDateTime.AsInteger := 20;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantDateTime.AsInteger!'
    );

  Assert.WillRaise(
    procedure begin
      FInstantDateTime.AsInteger;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantDateTime.AsInteger!'
    );
end;

procedure TestTInstantDateTime.TestAsObject;
var
  vObj: TInstantObject;
begin
  vObj := TInstantObject.Create(FConn);
  try
    Assert.WillRaise(
      procedure begin
        FInstantDateTime.AsObject := vObj;
      end,
      EInstantAccessError,
      'Exception was not thrown for Set InstantDateTime.AsObject!'
      );

    Assert.WillRaise(
      procedure begin
        FInstantDateTime.AsObject;
      end,
      EInstantAccessError,
      'Exception was not thrown for Get InstantDateTime.AsObject!'
      );
  finally
    vObj.Free;
  end;
end;

procedure TestTInstantDateTime.TestAssign;
var
  vSource: TInstantDateTime;
begin
  AssertEquals(100.5, FInstantDateTime.Value);

  vSource := TInstantDateTime.Create;
  try
    VSource.Value := 115.3;
    FInstantDateTime.Assign(vSource);
    AssertEquals(115.3, FInstantDateTime.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantDateTime.TestAsString;
begin
  FInstantDateTime.AsString := DateTimeToStr(14.5);
  AssertEquals(14.5, FInstantDateTime.Value);
  AssertEquals(DateTimeToStr(14.5), FInstantDateTime.AsString);
end;

procedure TestTInstantDateTime.TestAsVariant;
begin                                           
  FInstantDateTime.AsVariant := 150.6;
  AssertEquals(150.6, FInstantDateTime.Value);
  AssertEquals(150.6, FInstantDateTime.AsVariant);
end;

procedure TestTInstantDateTime.TestDisplayText;
var
  vDateTimeStr: string;
begin
  AssertEquals(FInstantDateTime.AsString, FInstantDateTime.DisplayText);

  DateTimeToString(vDateTimeStr, 'yyyymmddhhnnsszzz', FInstantDateTime.Value);
  FInstantDateTime.Metadata.EditMask := 'yyyymmddhhnnsszzz';
  AssertEquals(vDateTimeStr, FInstantDateTime.DisplayText);

  FInstantDateTime.Value := 113.8;
  DateTimeToString(vDateTimeStr, 'dd mmm yyyy hh:nn:ss ampm',
    FInstantDateTime.Value);
  FInstantDateTime.Metadata.EditMask := 'dd mmm yyyy hh:nn:ss ampm';
  AssertEquals(vDateTimeStr, FInstantDateTime.DisplayText);
end;

procedure TestTInstantDateTime.TestIsDefault;
begin
  AssertFalse(FInstantDateTime.IsDefault);

  FInstantDateTime.Value := 0;
  AssertTrue(FInstantDateTime.IsDefault);
end;

procedure TestTInstantDateTime.TestReset;
var
  vDateTimeStr: string;
begin
  AssertNotNull(FInstantDateTime.Metadata);
  // Metadata.DefaultValue is '';
  FInstantDateTime.Reset;
  AssertEquals(0, FInstantDateTime.Value);

  DateTimeToString(vDateTimeStr, 'yyyymmddhhnnsszzz', 100.5);
  FInstantDateTime.Metadata.DefaultValue := vDateTimeStr;
  FInstantDateTime.Reset;
  AssertEquals(100.5, FInstantDateTime.Value);

  FInstantDateTime.Metadata := nil;
  AssertNull(FInstantDateTime.Metadata);
  FInstantDateTime.Reset;
  AssertEquals(0, FInstantDateTime.Value);
end;

procedure TestTInstantDateTime.TestValue;
begin
  AssertEquals(100.5, FInstantDateTime.Value);
  FInstantDateTime.Value := 151.3;
  AssertEquals(151.3, FInstantDateTime.Value);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantDateTime]);
{$ENDIF}

end.
 