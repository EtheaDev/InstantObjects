unit TestInstantTableMetadata;

interface

uses fpcunit, InstantPersistence;

type

  // Test methods for class TInstantTableMetadata
  TestTInstantTableMetadata = class(TTestCase)
  private
    FInstantTableMetadata: TInstantTableMetadata;
    FOwner: TInstantScheme;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFieldMetadatas;
    procedure TestIndexMetadatas;
    procedure TestScheme;
  end;

  // Test methods for class TInstantTableMetadatas
  TestTInstantTableMetadatas = class(TTestCase)
  private
    FInstantTableMetadatas: TInstantTableMetadatas;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd_ReadItems;
  end;

implementation

uses SysUtils, testregistry;

{ TestTInstantTableMetadata }

procedure TestTInstantTableMetadata.SetUp;
begin
  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TInstantRelationalScheme.Create(InstantModel);
  FInstantTableMetadata := FOwner.FindTableMetadata('Address');
end;

procedure TestTInstantTableMetadata.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FOwner);
end;

procedure TestTInstantTableMetadata.TestFieldMetadatas;
begin
  AssertNotNull(FInstantTableMetadata.FieldMetadatas);
end;

procedure TestTInstantTableMetadata.TestIndexMetadatas;
begin
  AssertNotNull(FInstantTableMetadata.IndexMetadatas);
end;

procedure TestTInstantTableMetadata.TestScheme;
begin
  AssertNotNull(FInstantTableMetadata.Scheme);
  AssertSame(FOwner, FInstantTableMetadata.Scheme);
end;

{ TestTInstantTableMetadatas }

procedure TestTInstantTableMetadatas.SetUp;
begin
  FInstantTableMetadatas := TInstantTableMetadatas.Create(nil);
end;

procedure TestTInstantTableMetadatas.TearDown;
begin
  FreeAndNil(FInstantTableMetadatas);
end;

procedure TestTInstantTableMetadatas.TestAdd_ReadItems;
var
  vReturnValue: TInstantTableMetadata;
begin
  AssertEquals(0, FInstantTableMetadatas.Count);
  vReturnValue := FInstantTableMetadatas.Add;
  AssertNotNull(vReturnValue);
  AssertEquals(1, FInstantTableMetadatas.Count);
  AssertNotNull('Items[0]', FInstantTableMetadatas.Items[0]);
  FInstantTableMetadatas.Remove(vReturnValue);
  AssertEquals(0, FInstantTableMetadatas.Count);
end;



initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantTableMetadata,
                 TestTInstantTableMetadatas]);
{$ENDIF}

end.
 