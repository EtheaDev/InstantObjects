unit TestInstantScheme;

interface

uses fpcunit, InstantPersistence;

type

  // Test methods for class TInstantRelationalScheme
  TestTInstantRelationalScheme = class(TTestCase)
  private
    FInstantRelationalScheme: TInstantRelationalScheme;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBlobStreamFormat;
    procedure TestFindTableMetadata;
    procedure TestIdDataType;
    procedure TestIdSize;
    procedure TestTableMetadataCount;
    procedure TestTableMetadatas;
  end;

implementation

uses SysUtils, testregistry, InstantClasses, InstantConsts;

procedure TestTInstantRelationalScheme.SetUp;
begin
  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FInstantRelationalScheme := TInstantRelationalScheme.Create(InstantModel);
end;

procedure TestTInstantRelationalScheme.TearDown;
begin
  FInstantRelationalScheme.Free;
  FInstantRelationalScheme := nil;
end;

procedure TestTInstantRelationalScheme.TestBlobStreamFormat;
begin
  AssertTrue('Incorrect BlobStreamFormat!',
    sfBinary = FInstantRelationalScheme.BlobStreamFormat);
  FInstantRelationalScheme.BlobStreamFormat := sfXML;
  AssertTrue('Incorrect BlobStreamFormat!',
    sfXML = FInstantRelationalScheme.BlobStreamFormat);
end;

procedure TestTInstantRelationalScheme.TestFindTableMetadata;
var
  vReturnValue: TInstantTableMetadata;
  vName: string;
begin
  vName := 'Contact';
  vReturnValue := FInstantRelationalScheme.FindTableMetadata(vName);
  AssertNotNull('Could not find TableMetadata!', vReturnValue);
  AssertEquals('', vName, vReturnValue.Name);
end;

procedure TestTInstantRelationalScheme.TestIdDataType;
begin
  AssertTrue('Incorrect IdDataType!',
    dtString = FInstantRelationalScheme.IdDataType);
  FInstantRelationalScheme.IdDataType := dtInteger;
  AssertTrue('Incorrect IdDataType!',
    dtInteger = FInstantRelationalScheme.IdDataType);
end;

procedure TestTInstantRelationalScheme.TestIdSize;
begin
  AssertEquals('Incorrect IDSize!', InstantDefaultFieldSize,
    FInstantRelationalScheme.IdSize);
  FInstantRelationalScheme.IdSize := 10;
  AssertEquals('Incorrect IDSize!', 10, FInstantRelationalScheme.IdSize);
end;

procedure TestTInstantRelationalScheme.TestTableMetadataCount;
begin
  AssertEquals('Incorrect TableMetadataCount!', 8,
    FInstantRelationalScheme.TableMetadataCount);
end;

procedure TestTInstantRelationalScheme.TestTableMetadatas;
var
  vReturnValue: TInstantTableMetadata;
begin
  vReturnValue := FInstantRelationalScheme.TableMetadatas[0];
  AssertNotNull('TableMetadata is nil!', vReturnValue);
  AssertEquals('', 'Address', vReturnValue.Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantRelationalScheme]);
{$ENDIF}

end.
 