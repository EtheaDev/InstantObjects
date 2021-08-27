(*
 *   InstantObjects Test Suite
 *   TestInstantTime
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
 * The Original Code is: InstantObjects Test Suite/TestInstantTime
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

unit TestInstantTime;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantTime
  [TestFixture]
  TestTInstantTime = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantTime: TInstantTime;
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
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, InstantClasses;

procedure TestTInstantTime.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TPerson.Create(FConn);
  FInstantTime := FOwner._BirthTime;
  FInstantTime.Value := 0.100;
end;

procedure TestTInstantTime.TearDown;
begin
  FInstantTime := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantTime.TestAsBoolean;
begin
  Assert.WillRaise(
    procedure begin
      FInstantTime.AsBoolean := True;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantTime.AsBoolean!'
    );
  Assert.WillRaise(
    procedure begin
      FInstantTime.AsBoolean;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantTime.AsBoolean!'
    );
end;

procedure TestTInstantTime.TestAsCurrency;
begin
  Assert.WillRaise(
    procedure begin
      FInstantTime.AsCurrency := 20.5;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantTime.AsCurrency!'
    );
  Assert.WillRaise(
    procedure begin
      FInstantTime.AsCurrency;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantTime.AsCurrency!'
    );
end;

procedure TestTInstantTime.TestAsDate;
begin
  Assert.WillRaise(
    procedure begin
      FInstantTime.AsDate := 12;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantTime.AsDate!'
    );
  Assert.WillRaise(
    procedure begin
      FInstantTime.AsDate;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantTime.AsDate!'
    );
end;

procedure TestTInstantTime.TestAsDateTime;
begin
  FInstantTime.AsDateTime := 0.12;
  AssertEquals(0.12, FInstantTime.Value);
  AssertEquals(0.12, FInstantTime.AsDateTime);
  AssertEquals(0.12, FInstantTime.AsTime);
end;

procedure TestTInstantTime.TestAsTime;
begin
  FInstantTime.AsTime := 0.212;
  AssertEquals(0.212, FInstantTime.Value);
  AssertEquals(0.212, FInstantTime.AsDateTime);
  AssertEquals(0.212, FInstantTime.AsTime);
end;

procedure TestTInstantTime.TestAsFloat;
begin
  Assert.WillRaise(
    procedure begin
      FInstantTime.AsFloat := 20.5;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantTime.AsFloat!'
    );

  Assert.WillRaise(
    procedure begin
      FInstantTime.AsFloat;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantTime.AsFloat!'
    );
end;

procedure TestTInstantTime.TestAsInteger;
begin
  Assert.WillRaise(
    procedure begin
      FInstantTime.AsInteger := 20;
    end,
    EInstantAccessError,
    'Exception was not thrown for Set InstantTime.AsInteger!'
    );

  Assert.WillRaise(
    procedure begin
      FInstantTime.AsInteger;
    end,
    EInstantAccessError,
    'Exception was not thrown for Get InstantTime.AsInteger!'
    );
end;

procedure TestTInstantTime.TestAsObject;
var
  vObj: TInstantObject;
begin
  vObj := TInstantObject.Create(FConn);
  try
    Assert.WillRaise(
      procedure begin
        FInstantTime.AsObject := vObj;
      end,
      EInstantAccessError,
      'Exception was not thrown for Set InstantTime.AsObject!'
      );

    Assert.WillRaise(
      procedure begin
        FInstantTime.AsObject;
      end,
      EInstantAccessError,
      'Exception was not thrown for Get InstantTime.AsObject!'
      );
  finally
    vObj.Free;
  end;
end;

procedure TestTInstantTime.TestAssign;
var
  vSource: TInstantTime;
begin
  AssertEquals(0.100, FInstantTime.Value);

  vSource := TInstantTime.Create;
  try
    VSource.Value := 0.115;
    FInstantTime.Assign(vSource);
    AssertEquals(0.115, FInstantTime.Value);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantTime.TestAsString;
begin
  FInstantTime.AsString := TimeToStr(0.145);
  AssertEquals(0.145, FInstantTime.Value);
  AssertEquals(TimeToStr(0.145), FInstantTime.AsString);

  FInstantTime.AsString := TimeToStr(0.345);
  AssertEquals(0.345, FInstantTime.Value);
  AssertEquals(TimeToStr(0.345), FInstantTime.AsString);
end;

procedure TestTInstantTime.TestAsVariant;
begin                                           
  FInstantTime.AsVariant := 0.1506;
  AssertEquals(0.1506, FInstantTime.Value);
  AssertEquals(0.1506, FInstantTime.AsVariant);
end;

procedure TestTInstantTime.TestDisplayText;
var
  vDateTimeStr: string;
begin
  AssertEquals(FInstantTime.AsString, FInstantTime.DisplayText);

  DateTimeToString(vDateTimeStr, 'hhnnss.zzz', FInstantTime.Value);
  FInstantTime.Metadata.EditMask := 'hhnnss.zzz';
  AssertEquals(vDateTimeStr, FInstantTime.DisplayText);

  FInstantTime.Value := 0.113;
  DateTimeToString(vDateTimeStr, 'hhnnss.zzz',
    FInstantTime.Value);
  FInstantTime.Metadata.EditMask := 'hhnnss.zzz';
  AssertEquals(vDateTimeStr, FInstantTime.DisplayText);
end;

procedure TestTInstantTime.TestIsDefault;
begin
  AssertFalse(FInstantTime.IsDefault);

  FInstantTime.Value := 0;
  AssertTrue(FInstantTime.IsDefault);
end;

procedure TestTInstantTime.TestReset;
var
  vDateTimeStr: string;
begin
  AssertNotNull(FInstantTime.Metadata);
  // Metadata.DefaultValue is '';
  FInstantTime.Reset;
  AssertEquals(0, FInstantTime.Value);

  DateTimeToString(vDateTimeStr, 'hhnnsszzz', 0.101);
  FInstantTime.Metadata.DefaultValue := vDateTimeStr;
  FInstantTime.Reset;
  AssertEquals(0.101, FInstantTime.Value);

  FInstantTime.Metadata := nil;
  AssertNull(FInstantTime.Metadata);
  FInstantTime.Reset;
  AssertEquals(0, FInstantTime.Value);
end;

procedure TestTInstantTime.TestValue;
begin
  AssertEquals(0.100, FInstantTime.Value);
  FInstantTime.Value := 0.151;
  AssertEquals(0.151, FInstantTime.Value);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantTime]);
{$ENDIF}

end.
 