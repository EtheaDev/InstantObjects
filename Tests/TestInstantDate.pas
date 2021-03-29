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

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantDate
  TestTInstantDate = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantDate: TInstantDate;
    FOwner: TPerson;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
  SysUtils, testregistry, InstantClasses;

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
  try
    FInstantDate.AsBoolean := True;
    Fail('Exception was not thrown for Set AsBoolean!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDate.AsBoolean;
    Fail('Exception was not thrown for Get AsBoolean!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDate.TestAsCurrency;
begin
  try
    FInstantDate.AsCurrency := 20.5;
    Fail('Exception was not thrown for Set AsCurrency!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDate.AsCurrency;
    Fail('Exception was not thrown for Get AsCurrency!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
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
  try
    FInstantDate.AsTime := 0.12;
    Fail('Exception was not thrown for Set AsTime!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDate.AsTime;
    Fail('Exception was not thrown for Get AsTime!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDate.TestAsFloat;
begin
  try
    FInstantDate.AsFloat := 20.5;
    Fail('Exception was not thrown for Set AsFloat!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDate.AsFloat;
    Fail('Exception was not thrown for Get AsFloat!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDate.TestAsInteger;
begin
  try
    FInstantDate.AsInteger := 20;
    Fail('Exception was not thrown for Set AsInteger!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
  try
    FInstantDate.AsInteger;
    Fail('Exception was not thrown for Get AsInteger!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
  end;
end;

procedure TestTInstantDate.TestAsObject;
var
  vObj: TInstantObject;
begin
  vObj := TInstantObject.Create(FConn);
  try
    try
      FInstantDate.AsObject := vObj;
      Fail('Exception was not thrown for Set AsObject!'); // should never get here
    except
      on E: EInstantAccessError do ; // do nothing as this is expected
      else
        raise;
    end;
  finally
    vObj.Free;
  end;

  try
    FInstantDate.AsObject;
    Fail('Exception was not thrown for Get AsObject!'); // should never get here
  except
    on E: EInstantAccessError do ; // do nothing as this is expected
    else
      raise;
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
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantDate]);
{$ENDIF}

end.
 