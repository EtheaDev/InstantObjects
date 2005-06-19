(*
 *   InstantObjects
 *   TestInstantAttributeMetadata
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
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestInstantAttributeMetadata;

interface

uses SysUtils, fpcunit, InstantMock, InstantPersistence;

type
  TRunMethodCategory = procedure(Category: TInstantAttributeCategory) of object;
  TRunMethodIAClass = procedure(AClass: TInstantAttributeClass) of object;

  // Extended test methods for class TTestCase
  TTestCaseEx = class(TTestCase)
  public
    class procedure AssertException(const AMessage: string;
                                    AExceptionClass: ExceptClass;
                                    ACategory: TInstantAttributeCategory;
                                    AMethod: TRunMethodCategory);
                                    overload;
    class procedure AssertException(AExceptionClass: ExceptClass;
                                    ACategory: TInstantAttributeCategory;
                                    AMethod: TRunMethodCategory);
                                    overload;
    class procedure AssertException(const AMessage: string;
                                    AExceptionClass: ExceptClass;
                                    AClass: TInstantAttributeClass;
                                    AMethod: TRunMethodIAClass);
                                    overload;
    class procedure AssertException(AExceptionClass: ExceptClass;
                                    AClass: TInstantAttributeClass;
                                    AMethod: TRunMethodIAClass);
                                    overload;
    class procedure AssertNoException(const AMessage: string;
                                    AExceptionClass: ExceptClass;
                                    ACategory: TInstantAttributeCategory;
                                    AMethod: TRunMethodCategory);
                                    overload;
    class procedure AssertNoException(AExceptionClass: ExceptClass;
                                    ACategory: TInstantAttributeCategory;
                                    AMethod: TRunMethodCategory);
                                    overload;
    class procedure AssertNoException(const AMessage: string;
                                    AExceptionClass: ExceptClass;
                                    AClass: TInstantAttributeClass;
                                    AMethod: TRunMethodIAClass);
                                    overload;
    class procedure AssertNoException(AExceptionClass: ExceptClass;
                                    AClass: TInstantAttributeClass;
                                    AMethod: TRunMethodIAClass);
                                    overload;
  end;

  // Test methods for class TInstantAttributeMetadata
  TestTInstantAttributeMetadata = class(TTestCaseEx)
  private
    FConn: TInstantMockConnector;
    FInstantAttributeMetadata: TInstantAttributeMetadata;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreateAttribute;
    procedure TestAssign;
    procedure TestCheckAttributeClass;
    procedure TestCheckCategory;
    procedure TestCheckIsIndexed;
    procedure TestIsAttributeClass;
  end;

  // Test methods for class TInstantAttributeMetadatas
  TestTInstantAttributeMetadatas = class(TTestCase)
  private
    FOwner: TInstantClassMetadata;
    FInstantAttributeMetadatas: TInstantAttributeMetadatas;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddRemove;
    procedure TestClear;
    procedure TestFind;
    procedure TestOwner;
  end;

implementation

uses Classes, TypInfo, testregistry, InstantClasses;

class procedure TTestCaseEx.AssertException(const AMessage: string;
AExceptionClass: ExceptClass; ACategory: TInstantAttributeCategory;
AMethod: TRunMethodCategory);
var
  Passed : Boolean;
  ExceptionName: string;
begin
  Passed := False;
  try
    AMethod(ACategory);
  except
    on E: Exception do
    begin
      ExceptionName := E.ClassName;
      if E.ClassType.InheritsFrom(AExceptionClass) then
      begin
        Passed := AExceptionClass.ClassName = E.ClassName;
      end;
    end;
  end;
  AssertTrue(Format(SExceptionCompare,
    [AExceptionClass.ClassName, ExceptionName])+ ': ' + AMessage, Passed);
end;

class procedure TTestCaseEx.AssertException(const AMessage: string;
AExceptionClass: ExceptClass; AClass: TInstantAttributeClass;
AMethod: TRunMethodIAClass);
var
  Passed : Boolean;
  ExceptionName: string;
begin
  Passed := False;
  try
    AMethod(AClass);
  except
    on E: Exception do
    begin
      ExceptionName := E.ClassName;
      if E.ClassType.InheritsFrom(AExceptionClass) then
      begin
        Passed := AExceptionClass.ClassName = E.ClassName;
      end;
    end;
  end;
  AssertTrue(Format(SExceptionCompare,
    [AExceptionClass.ClassName, ExceptionName])+ ': ' + AMessage, Passed);
end;

class procedure TTestCaseEx.AssertException(AExceptionClass: ExceptClass;
  ACategory: TInstantAttributeCategory; AMethod: TRunMethodCategory);
begin
  AssertException('', AExceptionClass, ACategory, AMethod);
end;

class procedure TTestCaseEx.AssertException(AExceptionClass: ExceptClass;
  AClass: TInstantAttributeClass; AMethod: TRunMethodIAClass);
begin
  AssertException('', AExceptionClass, AClass, AMethod);
end;

class procedure TTestCaseEx.AssertNoException(const AMessage: string;
AExceptionClass: ExceptClass; ACategory: TInstantAttributeCategory;
AMethod: TRunMethodCategory);
var
  Passed : Boolean;
begin
  Passed := True;
  try
    AMethod(ACategory);
  except
    Passed := False;
  end;
  AssertTrue(AMessage, Passed);
end;

class procedure TTestCaseEx.AssertNoException(const AMessage: string;
AExceptionClass: ExceptClass; AClass: TInstantAttributeClass;
AMethod: TRunMethodIAClass);
var
  Passed : Boolean;
begin
  Passed := True;
  try
    AMethod(AClass);
  except
    Passed := False;
  end;
  AssertTrue(AMessage, Passed);
end;

class procedure TTestCaseEx.AssertNoException(AExceptionClass: ExceptClass;
  ACategory: TInstantAttributeCategory; AMethod: TRunMethodCategory);
begin
  AssertNoException('', AExceptionClass, ACategory, AMethod);
end;

class procedure TTestCaseEx.AssertNoException(AExceptionClass: ExceptClass;
  AClass: TInstantAttributeClass; AMethod: TRunMethodIAClass);
begin
  AssertNoException('', AExceptionClass, AClass, AMethod);
end;

procedure TestTInstantAttributeMetadata.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  FInstantAttributeMetadata := TInstantAttributeMetadata.Create(nil);
  FInstantAttributeMetadata.AttributeType := atString;
  FInstantAttributeMetadata.DefaultValue := 'Default';
  FInstantAttributeMetadata.DisplayWidth := 10;
  FInstantAttributeMetadata.EditMask := '';
  FInstantAttributeMetadata.IsIndexed := False;
  FInstantAttributeMetadata.IsRequired := True;
  FInstantAttributeMetadata.ObjectClassName := 'ObjectClassName';
  FInstantAttributeMetadata.Size := 40;
  FInstantAttributeMetadata.StorageName := 'StorageName';
  FInstantAttributeMetadata.StorageKind := skEmbedded;
  FInstantAttributeMetadata.ExternalStorageName := 'ExternalStorageName';
  FInstantAttributeMetadata.ValidChars := ['a'..'z'];
end;

procedure TestTInstantAttributeMetadata.TearDown;
begin
  FreeAndNil(FInstantAttributeMetadata);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantAttributeMetadata.TestAssign;
var
  vDest, vSource: TInstantAttributeMetadata;
  vStr: string;
begin
  // Don't use FInstantAttributeMetadata in this method,
  // otherwise it will interfere with TestCase setup.
  vSource := TInstantAttributeMetadata.Create(nil);
  vDest := TInstantAttributeMetadata.Create(nil);
  try
    vSource.AttributeType := atString;
    vSource.DefaultValue := 'Default';
    vSource.DisplayWidth := 10;
    vSource.EditMask := '';
    vSource.IsIndexed := False;
    vSource.IsRequired := True;
    vSource.ObjectClassName := 'ObjectClassName';
    vSource.Size := 40;
    vSource.StorageName := 'StorageName';
    vSource.StorageKind := skEmbedded;
    vSource.ExternalStorageName := 'ExternalStorageName';
    vSource.ValidChars := ['a'..'z'];
    vDest.Assign(vSource);

    vStr := GetEnumName(TypeInfo(TInstantAttributeType),
      Ord(vDest.AttributeType));
    AssertEquals('AttributeType incorrect', 'atString', vStr);
    AssertEquals('DefaultValue incorrect', 'Default', vDest.DefaultValue);
    AssertEquals('DisplayWidth incorrect', 10, vDest.DisplayWidth);
    AssertEquals('EditMask incorrect', '', vDest.EditMask);
    AssertEquals('IsIndexed incorrect', False, vDest.IsIndexed);
    AssertEquals('IsRequired incorrect', True, vDest.IsRequired);
    AssertEquals('ObjectClassName incorrect', 'ObjectClassName',
      vDest.ObjectClassName);
    AssertEquals('Size incorrect', 40, vDest.Size);
    AssertEquals('StorageName incorrect', 'StorageName', vDest.StorageName);
    vStr := GetEnumName(TypeInfo(TInstantStorageKind),
      Ord(vDest.StorageKind));
    AssertEquals('StorageKind incorrect', 'skEmbedded', vStr);
    AssertEquals('ExternalStorageName incorrect', 'ExternalStorageName',
      vDest.ExternalStorageName);
    AssertTrue('ValidChars incorrect', 'i' in vDest.ValidChars);
  finally
    vSource.Free;
    vDest.Free;
  end;
end;

procedure TestTInstantAttributeMetadata.TestCheckAttributeClass;
begin
  AssertNoException(EInstantError, TInstantSimple,
    FInstantAttributeMetadata.CheckAttributeClass);
  AssertException(EInstantError, TInstantNumeric,
    FInstantAttributeMetadata.CheckAttributeClass);
end;

procedure TestTInstantAttributeMetadata.TestCheckCategory;
begin
  AssertNoException(EInstantError, acSimple,
    FInstantAttributeMetadata.CheckCategory);
  AssertException(EInstantError, acContainer,
    FInstantAttributeMetadata.CheckCategory);
end;

procedure TestTInstantAttributeMetadata.TestCheckIsIndexed;
begin
  AssertException(EInstantError, FInstantAttributeMetadata.CheckIsIndexed);
end;

procedure TestTInstantAttributeMetadata.TestCreateAttribute;
var
  vReturnValue: TInstantAttribute;
  vObject: TInstantObject;
begin
  vObject := TInstantObject.Create(FConn);
  try
    vReturnValue := FInstantAttributeMetadata.CreateAttribute(vObject);
    AssertNotNull('vReturnValue', vReturnValue);
    AssertEquals('AsString', 'Default', vReturnValue.AsString);
    AssertNotNull('Metadata ', vReturnValue.Metadata);
    AssertEquals('Classname', 'TInstantString', vReturnValue.ClassName);
    vReturnValue.Free;
  finally
    vObject.Free;
  end;
end;

procedure TestTInstantAttributeMetadata.TestIsAttributeClass;
var
  vReturnValue: Boolean;
begin
  vReturnValue := FInstantAttributeMetadata.IsAttributeClass(TInstantAttribute);
  AssertTrue('IsAttributeClass error for TInstantAttribute!', vReturnValue);
  vReturnValue := FInstantAttributeMetadata.IsAttributeClass(TInstantSimple);
  AssertTrue('IsAttributeClass error for TInstantMetadata!', vReturnValue);
end;

{ TestTInstantAttributeMetadatas }

procedure TestTInstantAttributeMetadatas.SetUp;
begin
  FOwner := TInstantClassMetadata.Create(nil);

  FInstantAttributeMetadatas := TInstantAttributeMetadatas.Create(FOwner);
end;

procedure TestTInstantAttributeMetadatas.TearDown;
begin
  FreeAndNil(FInstantAttributeMetadatas);
  FreeAndNil(FOwner);
end;

procedure TestTInstantAttributeMetadatas.TestAddRemove;
var
  vReturnValue: TInstantAttributeMetadata;
begin
  vReturnValue := FInstantAttributeMetadatas.Add;
  AssertNotNull('vReturnValue is nil!', vReturnValue);
  AssertEquals('Count is incorrect!', 1, FInstantAttributeMetadatas.Count);
  FInstantAttributeMetadatas.Remove(vReturnValue);
  AssertEquals('Count is incorrect!', 0, FInstantAttributeMetadatas.Count);
end;

procedure TestTInstantAttributeMetadatas.TestClear;
var
  i: Integer;
begin
  for i := 0 to 9 do
  begin
    FInstantAttributeMetadatas.Add;
  end;
  AssertEquals('Count is incorrect!', 10, FInstantAttributeMetadatas.Count);
  FInstantAttributeMetadatas.Clear;
  AssertEquals('Count is incorrect!', 0, FInstantAttributeMetadatas.Count);
end;

procedure TestTInstantAttributeMetadatas.TestFind;
var
  vReturnValue: TInstantAttributeMetadata;
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    vReturnValue := FInstantAttributeMetadatas.Add;
    vReturnValue.Name := 'Name' + IntToStr(i);
  end;
  AssertEquals('Count is incorrect!', 3, FInstantAttributeMetadatas.Count);

  vReturnValue := FInstantAttributeMetadatas.Find('Name1');
  AssertNotNull('vReturnValue is nil!', vReturnValue);
  FInstantAttributeMetadatas.Clear;
  AssertEquals('Count is incorrect!', 0, FInstantAttributeMetadatas.Count);
end;

procedure TestTInstantAttributeMetadatas.TestOwner;
var
  vReturnValue: TInstantClassMetadata;
begin
  vReturnValue := FInstantAttributeMetadatas.Owner;
  AssertNotNull('vReturnValue is nil!', vReturnValue);
  AssertSame('Owner is incorrect!', FOwner, vReturnValue);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantAttributeMetadata,
                 TestTInstantAttributeMetadatas]);
{$ENDIF}

end.
