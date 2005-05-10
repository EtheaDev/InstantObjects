unit TestInstantFieldMetadata;

interface

uses fpcunit, InstantPersistence;

type
  // Test methods for class TInstantFieldMetadata
  TestTInstantFieldMetadata = class(TTestCase)
  private
    FOwner: TInstantTableMetadata;
    FCollection: TInstantFieldMetadatas;
    FInstantFieldMetadata: TInstantFieldMetadata;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCollection;
    procedure TestDataType;
    procedure TestExternalTableName;
    procedure TestOptions;
    procedure TestOriginalAttributeType;
    procedure TestSize;
  end;

  // Test methods for class TInstantFieldMetadatas
  TestTInstantFieldMetadatas = class(TTestCase)
  private
    FOwner: TInstantTableMetadata;
    FInstantFieldMetadatas: TInstantFieldMetadatas;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddFieldMetadata;
    procedure TestAddRemoveItems;
    procedure TestOwner;
  end;

implementation

uses SysUtils, TypInfo, testregistry;

procedure TestTInstantFieldMetadata.SetUp;
begin
  FOwner := TInstantTableMetadata.Create(nil);
  FCollection := TInstantFieldMetadatas.Create(FOwner);
  FInstantFieldMetadata := TInstantFieldMetadata.Create(FCollection);
  FInstantFieldMetadata.DataType := dtInteger;
  FInstantFieldMetadata.ExternalTableName := 'ExternalTableName';
  FInstantFieldMetadata.Size := 10;
  FInstantFieldMetadata.Options := [foRequired];
  FInstantFieldMetadata.OriginalAttributeType := atInteger;
end;

procedure TestTInstantFieldMetadata.TearDown;
begin
  FInstantFieldMetadata.Free;
  FInstantFieldMetadata := nil;
  FreeAndNil(FCollection);
  FreeAndNil(FOwner);
end;

procedure TestTInstantFieldMetadata.TestCollection;
begin
  AssertNotNull(FInstantFieldMetadata.Collection);
  AssertSame(FCollection, FInstantFieldMetadata.Collection);
end;

procedure TestTInstantFieldMetadata.TestDataType;
var
  vStr: string;
begin
  vStr := GetEnumName(TypeInfo(TInstantDataType),
    Ord(FInstantFieldMetadata.DataType));
  AssertEquals('dtInteger', vStr);
end;

procedure TestTInstantFieldMetadata.TestExternalTableName;
begin
  AssertEquals('ExternalTableName', FInstantFieldMetadata.ExternalTableName);
end;

procedure TestTInstantFieldMetadata.TestOptions;
begin
  AssertTrue(foRequired in FInstantFieldMetadata.Options);
end;

procedure TestTInstantFieldMetadata.TestOriginalAttributeType;
var
  vStr: string;
begin
  vStr := GetEnumName(TypeInfo(TInstantAttributeType),
    Ord(FInstantFieldMetadata.OriginalAttributeType));
  AssertEquals('atInteger', vStr);
end;

procedure TestTInstantFieldMetadata.TestSize;
begin
  AssertEquals(10, FInstantFieldMetadata.Size);
end;

procedure TestTInstantFieldMetadatas.SetUp;
begin
  FOwner := TInstantTableMetadata.Create(nil);
  FInstantFieldMetadatas := TInstantFieldMetadatas.Create(FOwner);
end;

procedure TestTInstantFieldMetadatas.TearDown;
begin
  FInstantFieldMetadatas.Free;
  FInstantFieldMetadatas := nil;
  FreeAndNil(FOwner);
end;

procedure TestTInstantFieldMetadatas.TestAddRemoveItems;
var
  vReturnValue: TInstantFieldMetadata;
begin
  vReturnValue := FInstantFieldMetadatas.Add;
  AssertNotNull(vReturnValue);
  AssertEquals(1, FInstantFieldMetadatas.Count);
  AssertNotNull('Items[0]', FInstantFieldMetadatas.Items[0]);
  FInstantFieldMetadatas.Remove(vReturnValue);
  AssertEquals(0, FInstantFieldMetadatas.Count);
end;

procedure TestTInstantFieldMetadatas.TestAddFieldMetadata;
var
  vExternalTableName: string;
  vOptions: TInstantFieldOptions;
  vOriginalAttributeType: TInstantAttributeType;
  vSize: Integer;
  vDataType: TInstantDataType;
  vName: string;
  vInstantFieldMetadata: TInstantFieldMetadata;
  vStr: string;
begin
  vName := 'Code';
  vDataType := dtInteger;
  vSize := 10;
  vOriginalAttributeType := atInteger;
  vOptions := [foRequired];
  vExternalTableName := 'ExternalTableName';
  FInstantFieldMetadatas.AddFieldMetadata(vName, vDataType, vSize,
    vOriginalAttributeType, vOptions, vExternalTableName);

  AssertEquals(1, FInstantFieldMetadatas.Count);
  vInstantFieldMetadata := FInstantFieldMetadatas.Items[0];
  AssertNotNull(vInstantFieldMetadata);
  AssertEquals(vName, vInstantFieldMetadata.Name);
  AssertEquals(10, vInstantFieldMetadata.Size);
  vStr := GetEnumName(TypeInfo(TInstantAttributeType),
    Ord(vInstantFieldMetadata.OriginalAttributeType));
  AssertEquals('atInteger', vStr);
  AssertTrue(foRequired in vInstantFieldMetadata.Options);
  vStr := GetEnumName(TypeInfo(TInstantDataType),
    Ord(vInstantFieldMetadata.DataType));
  AssertEquals('dtInteger', vStr);
  AssertEquals('ExternalTableName', vInstantFieldMetadata.ExternalTableName);
end;

procedure TestTInstantFieldMetadatas.TestOwner;
begin
  AssertNotNull(FInstantFieldMetadatas.Owner);
  AssertSame(FOwner, FInstantFieldMetadatas.Owner);
end;



initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantFieldMetadata,
                 TestTInstantFieldMetadatas]);
{$ENDIF}

end.
