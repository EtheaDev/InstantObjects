unit TestInstantBlob;

interface

uses fpcunit, InstantPersistence;

type

  // Test methods for class TInstantBlob
  TestTInstantBlob = class(TTestCase)
  private
    FAttrMetadata: TInstantAttributeMetadata;
    FInstantBlob: TInstantBlob;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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

uses SysUtils, Classes, Graphics, testregistry, InstantClasses;

procedure TestTInstantBlob.SetUp;
begin
//  FConn := TInstantMockConnector.Create(nil);
//  FConn.BrokerClass := TInstantMockBroker;
//  FOwner := TInstantObject.Create(FConn);
  FAttrMetadata := TInstantAttributeMetadata.Create(nil);
  FAttrMetadata.AttributeClass := TInstantBlob;
  FAttrMetadata.Name := 'AttrMetadataName';
  FInstantBlob := TInstantBlob.Create(nil, FAttrMetadata);
end;

procedure TestTInstantBlob.TearDown;
begin
  FreeAndNil(FInstantBlob);
  FreeAndNil(FAttrMetadata);
//  FreeAndNil(FOwner);
//  FreeAndNil(FConn);
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
  AssertEquals('NewString', FInstantBlob.Value);
  AssertEquals('NewString', FInstantBlob.AsVariant);
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

    SetLength(vStreamReadStr, vCount);
    vReturnValue := vStream.Read(vStreamReadStr[1], vCount);
    AssertEquals(vCount, vReturnValue);

    SetLength(vBlobReadStr, vCount);
    vReturnValue := FInstantBlob.ReadBuffer(vBlobReadStr[1], vPosition,
      vCount);
    AssertEquals(vCount, vReturnValue);
    AssertEquals(vStreamReadStr, vBlobReadStr);

    vStreamReadStr := UpperCase(vStreamReadStr);
    vBlobReadStr := UpperCase(vBlobReadStr);
    vReturnValue := FInstantBlob.WriteBuffer(vBlobReadStr[1], vPosition,
      vCount);
    AssertEquals(vCount, vReturnValue);
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
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantBlob]);
{$ENDIF}

end.
 