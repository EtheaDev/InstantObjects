(*
 *   InstantObjects Test Suite
 *   TestInstantBlob
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
 * The Original Code is: InstantObjects Test Suite/TestInstantBlob
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

unit TestInstantBlob;

interface

uses {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantPersistence, InstantMock, TestModel,
  DUnitX.TestFramework;

type

  // Test methods for class TInstantBlob
  [TestFixture]
  TestTInstantBlob = class(TInstantTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantBlob: TInstantBlob;
    FOwner: TPerson;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestAssign;
    procedure TestAssignPicture_ToPicture;
    procedure TestAsString;
    procedure TestAsVariant;
    procedure TestClear;
    procedure TestLoadDataFromStream_ToStream;
    procedure TestReadBuffer_WriteBuffer;
    procedure TestReset;
    procedure TestValue;
  end;

implementation

uses SysUtils, Classes, Graphics, InstantClasses;

procedure TestTInstantBlob.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TPerson.Create(FConn);
  FInstantBlob := FOwner._Picture;
end;

procedure TestTInstantBlob.TearDown;
begin
  FInstantBlob := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantBlob.TestAssign;
var
  vSource: TInstantBlob;
  vStream: TStream;
begin

  AssertEquals(0, FInstantBlob.Size);

  vStream := TFileStream.Create(
    ExtractFilePath(ParamStr(0)) + 'TestBlobText.txt', fmOpenRead);
  try
    AssertTrue(vStream.Size > 0);
    vSource := TInstantBlob.Create;
    try
      vSource.LoadDataFromStream(vStream);
      AssertEquals(vStream.Size, vSource.Size);
      FInstantBlob.Assign(vSource);
      AssertTrue(FInstantBlob.Size > 0);
      AssertEquals(vSource.Size, FInstantBlob.Size);
    finally
      vSource.Free;
    end;
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantBlob.TestAssignPicture_ToPicture;
var
  vSource: TPicture;
begin
  vSource := TPicture.Create;
  try
    vSource.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Picture.bmp');
    AssertFalse(vSource.Bitmap.Empty);
    FInstantBlob.AssignPicture(vSource);
    AssertTrue(FInstantBlob.Size > 0);
  finally
    vSource.Free;
  end;

  vSource := TPicture.Create;
  try
    AssertTrue(vSource.Bitmap.Empty);
    FInstantBlob.AssignToPicture(vSource);
    AssertFalse(vSource.Bitmap.Empty);
  finally
    vSource.Free;
  end;
end;

procedure TestTInstantBlob.TestAsString;
begin
  FInstantBlob.AsString := 'NewString';
  AssertEquals('NewString', FInstantBlob.Value);
  AssertEquals('NewString', FInstantBlob.AsString);
end;

procedure TestTInstantBlob.TestAsVariant;
begin
  FInstantBlob.AsVariant := 'NewString';
  {$IFDEF VER130}
  AssertEquals('NewString', VarToStr(FInstantBlob.Value));
  AssertEquals('NewString', VarToStr(FInstantBlob.AsVariant));
  {$ELSE}
  AssertEquals('NewString', FInstantBlob.Value);
  AssertEquals('NewString', FInstantBlob.AsVariant);
  {$ENDIF}
end;

procedure TestTInstantBlob.TestClear;
var
  vStream: TStream;
begin
  vStream := TFileStream.Create(
    ExtractFilePath(ParamStr(0)) + 'TestBlobText.txt', fmOpenRead);
  try
    AssertTrue(vStream.Size > 0);
    FInstantBlob.LoadDataFromStream(vStream);
    AssertEquals(vStream.Size, FInstantBlob.Size);
    FInstantBlob.Clear;
    AssertEquals(0, FInstantBlob.Size);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantBlob.TestLoadDataFromStream_ToStream;
var
  vStream: TStream;
begin
  AssertEquals(0, FInstantBlob.Size);

  vStream := TFileStream.Create(
    ExtractFilePath(ParamStr(0)) + 'TestBlobText.txt', fmOpenRead);
  try
    AssertTrue(vStream.Size > 0);
    FInstantBlob.LoadDataFromStream(vStream);
    AssertEquals(vStream.Size, FInstantBlob.Size);
  finally
    vStream.Free;
  end;

  vStream := TFileStream.Create(
    ExtractFilePath(ParamStr(0)) + 'TestBlobTextOut.txt', fmCreate);
  try
    FInstantBlob.SaveDataToStream(vStream);
    AssertTrue(vStream.Size > 0);
    AssertEquals(vStream.Size, FInstantBlob.Size);
  finally
    vStream.Free;
  end;

end;

procedure TestTInstantBlob.TestReadBuffer_WriteBuffer;
var
  vStream: TStream;
  vReturnValue: Integer;
  vCount: Integer;
  vPosition: Integer;
  vTempStr1: AnsiString;
  vTempStr2: AnsiString;
  vBlobReadStr: string;
  vStreamReadStr: string;
begin
  vCount := 12;
  vPosition := 20;

  vStream := TFileStream.Create(
    ExtractFilePath(ParamStr(0)) + 'TestBlobText.txt', fmOpenRead);
  try
    AssertTrue(vStream.Size > 0);
    FInstantBlob.LoadDataFromStream(vStream);
    AssertEquals(vStream.Size, FInstantBlob.Size);
    vReturnValue := vStream.Seek(vPosition, soFromBeginning);
    AssertEquals(vPosition, vReturnValue);

    SetLength(vTempStr1, vCount);
    vReturnValue := vStream.Read(vTempStr1[1], vCount);
    AssertEquals(vCount, vReturnValue);
    vStreamReadStr := string(vTempStr1);

    SetLength(vTempStr2, vCount);
    vReturnValue := FInstantBlob.ReadBuffer(vTempStr2[1], vPosition, vCount);
    AssertEquals(vCount, vReturnValue);
    vBlobReadStr := string(vTempStr2);
    AssertEquals(string(vStreamReadStr), string(vBlobReadStr));

    vStreamReadStr := UpperCase(vStreamReadStr);
    vBlobReadStr := UpperCase(vBlobReadStr);
    vReturnValue := FInstantBlob.WriteBuffer(vBlobReadStr[1], vPosition,
      vCount);
    AssertEquals(vCount, vReturnValue);
    AssertTrue(FInstantBlob.IsChanged);
    vReturnValue := FInstantBlob.ReadBuffer(vBlobReadStr[1], vPosition,
      vCount);
    AssertEquals(vCount, vReturnValue);
    AssertEquals(vStreamReadStr, vBlobReadStr);
  finally
    vStream.Free;
  end;
end;

procedure TestTInstantBlob.TestReset;
begin
  AssertNotNull(FInstantBlob.Metadata);
  FInstantBlob.Value := 'This is a new value.';
  AssertEquals('This is a new value.', FInstantBlob.Value);

  // Metadata.DefaultValue is '';
  FInstantBlob.Reset;
  AssertEquals('', FInstantBlob.Value);

  FInstantBlob.Metadata.DefaultValue := 'I am reset!';
  FInstantBlob.Reset;
  AssertEquals('I am reset!', FInstantBlob.Value);

  FInstantBlob.Metadata := nil;
  AssertNull(FInstantBlob.Metadata);
  FInstantBlob.Reset;
  AssertEquals('', FInstantBlob.Value);
end;

procedure TestTInstantBlob.TestValue;
begin
  AssertEquals('', FInstantBlob.Value);

  FInstantBlob.Value := 'NewString';
  AssertEquals('NewString', FInstantBlob.Value);
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TestTInstantBlob]);
{$ENDIF}

end.
 