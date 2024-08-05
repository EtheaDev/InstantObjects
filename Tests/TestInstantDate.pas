(*
 *   InstantObjects Test Suite
 *   TestInstantDate
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
 * The Original Code is: InstantObjects Test Suite/TestInstantDate
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

unit TestInstantDate;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantDate
  [TestFixture]
  TestTInstantDate = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    FConn: TInstantMockConnector;
    FInstantDate: TInstantDate;
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
    procedure TestAsDate;
    procedure TestAsDateTime;
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

procedure TestTInstantDate.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TPerson.Create(FConn);
  FInstantDate := FOwner._EmploymentDate;
  FInstantDate.Value := 100;
end;

procedure TestTInstantDate.TearDown;
begin
  FInstantDate := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantDate.TestAsBoolean;
begin
  Assert.WillRaise(
    procedure begin
      FInstantDate.AsBoolean := True;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantDate.AsBoolean!'
    );
  Assert.WillRaise(
    procedure begin
      FInstantDate.AsBoolean;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantDate.AsBoolean!'
    );
end;

procedure TestTInstantDate.TestAsCurrency;
begin
  Assert.WillRaise(
    procedure begin
      FInstantDate.AsCurrency := 20.5;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantDate.AsCurrency!'
    );
  Assert.WillRaise(
    procedure begin
      FInstantDate.AsCurrency;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantDate.AsCurrency!'
    );
end;

procedure TestTInstantDate.TestAsDate;
begin
  FInstantDate.AsDate := 12;
  AssertEquals(12, FInstantDate.Value);
  AssertEquals(12, FInstantDate.AsDate);
end;

procedure TestTInstantDate.TestAsDateTime;
begin
  FInstantDate.AsDateTime := 12;
  AssertEquals(12, FInstantDate.Value);
  AssertEquals(12, FInstantDate.AsDateTime);
end;

procedure TestTInstantDate.TestAsTime;
begin
  Assert.WillRaise(
    procedure
    begin
      FInstantDate.AsTime := 0.12;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set AsTime!');

  Assert.WillRaise(
    procedure
    begin
      FInstantDate.AsTime;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get AsTime!');
end;

procedure TestTInstantDate.TestAsFloat;
begin
  Assert.WillRaise(
    procedure begin
      FInstantDate.AsFloat := 20.5;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantDate.AsFloat!'
    );

  Assert.WillRaise(
    procedure begin
      FInstantDate.AsFloat;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantDate.AsFloat!'
    );
end;

procedure TestTInstantDate.TestAsInteger;
begin
  Assert.WillRaise(
    procedure begin
      FInstantDate.AsInteger := 20;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantDate.AsInteger!'
    );

  Assert.WillRaise(
    procedure begin
      FInstantDate.AsInteger;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantDate.AsInteger!'
    );
end;

procedure TestTInstantDate.TestAsObject;
var
  vObj: TInstantObject;
begin
  vObj := TInstantObject.Create(FConn);
  try
    Assert.WillRaise(
      procedure begin
        FInstantDate.AsObject := vObj;
      end,
      EInstantAccessError,
      'Exception was not thrown for Set InstantDate.AsObject!'
      );

    Assert.WillRaise(
      procedure begin
        FInstantDate.AsObject;
      end,
      EInstantAccessError,
      'Exception was not thrown for Get InstantDate.AsObject!'
      );
  finally
    vObj.Free;
  end;
end;

procedure TestTInstantDate.TestAssign;
var
  vSource: TInstantDate;
begin
  AssertEquals(100, FInstantDate.Value);

  vSource := TInstantDate.Create;
  try
    VSource.Value := 115;
    FInstantDate.Assign(vSource);
    AssertEquals(115, FInstantDate.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantDate.TestAsString;
begin
  FInstantDate.AsString := DateTimeToStr(14.5);
  AssertEquals(14, FInstantDate.Value);
  AssertEquals(DateTimeToStr(14), FInstantDate.AsString);
end;

procedure TestTInstantDate.TestAsVariant;
begin                                           
  FInstantDate.AsVariant := 150.6;
  AssertEquals(150, FInstantDate.Value);
  AssertEquals(150, FInstantDate.AsVariant);
end;

procedure TestTInstantDate.TestDisplayText;
var
  vDateTimeStr: string;
begin
  AssertEquals(FInstantDate.AsString, FInstantDate.DisplayText);

  DateTimeToString(vDateTimeStr, 'yyyymmdd', FInstantDate.Value);
  FInstantDate.Metadata.EditMask := 'yyyymmdd';
  AssertEquals(vDateTimeStr, FInstantDate.DisplayText);

  FInstantDate.Value := 113;
  DateTimeToString(vDateTimeStr, 'dd mmm yyyy',
    FInstantDate.Value);
  FInstantDate.Metadata.EditMask := 'dd mmm yyyy';
  AssertEquals(vDateTimeStr, FInstantDate.DisplayText);
end;

procedure TestTInstantDate.TestIsDefault;
begin
  AssertFalse(FInstantDate.IsDefault);

  FInstantDate.Value := 0;
  AssertTrue(FInstantDate.IsDefault);
end;

procedure TestTInstantDate.TestReset;
var
  vDateTimeStr: string;
begin
  AssertNotNull(FInstantDate.Metadata);
  // Metadata.DefaultValue is '';
  FInstantDate.Reset;
  AssertEquals(0, FInstantDate.Value);

  DateTimeToString(vDateTimeStr, 'yyyymmdd', 100);
  FInstantDate.Metadata.DefaultValue := vDateTimeStr;
  FInstantDate.Reset;
  AssertEquals(100, FInstantDate.Value);

  FInstantDate.Metadata := nil;
  AssertNull(FInstantDate.Metadata);
  FInstantDate.Reset;
  AssertEquals(0, FInstantDate.Value);
end;

procedure TestTInstantDate.TestValue;
begin
  AssertEquals(100, FInstantDate.Value);
  FInstantDate.Value := 151;
  AssertEquals(151, FInstantDate.Value);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantDate]);
{$ENDIF}

end.
 