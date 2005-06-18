unit TestInstantScheme;

interface

uses fpcunit, InstantPersistence;

type

  // Test methods for class TInstantScheme
  TestTInstantScheme = class(TTestCase)
  private
    FInstantScheme: TInstantScheme;
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

procedure TestTInstantScheme.SetUp;
begin
  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FInstantScheme := TInstantScheme.Create;
  FInstantScheme.Catalog := TInstantModelCatalog.Create(FInstantScheme, InstantModel);
end;

procedure TestTInstantScheme.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FInstantScheme);
end;

procedure TestTInstantScheme.TestBlobStreamFormat;
begin
  AssertTrue('BlobStreamFormat',
    sfBinary = FInstantScheme.BlobStreamFormat);
  FInstantScheme.BlobStreamFormat := sfXML;
  AssertTrue('BlobStreamFormat',
    sfXML = FInstantScheme.BlobStreamFormat);
end;

procedure TestTInstantScheme.TestFindTableMetadata;
var
  vReturnValue: TInstantTableMetadata;
  vName: string;
begin
  vName := 'Contact';
  vReturnValue := FInstantScheme.FindTableMetadata(vName);
  AssertNotNull('Could not find TableMetadata!', vReturnValue);
  AssertEquals('', vName, vReturnValue.Name);
end;

procedure TestTInstantScheme.TestIdDataType;
begin
  AssertTrue('IdDataType', dtString = FInstantScheme.IdDataType);
  FInstantScheme.IdDataType := dtInteger;
  AssertTrue('IdDataType', dtInteger = FInstantScheme.IdDataType);
end;

procedure TestTInstantScheme.TestIdSize;
begin
  AssertEquals(InstantDefaultFieldSize,
    FInstantScheme.IdSize);
  FInstantScheme.IdSize := 10;
  AssertEquals(10, FInstantScheme.IdSize);
end;

procedure TestTInstantScheme.TestTableMetadataCount;
begin
  AssertEquals(8, FInstantScheme.TableMetadataCount);
end;

procedure TestTInstantScheme.TestTableMetadatas;
var
  vReturnValue: TInstantTableMetadata;
begin
  vReturnValue := FInstantScheme.TableMetadatas[0];
  AssertNotNull(vReturnValue);
  AssertEquals('Address', vReturnValue.Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantScheme]);
{$ENDIF}

end.
 