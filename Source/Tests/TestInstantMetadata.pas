unit TestInstantMetadata;

interface

uses SysUtils, fpcunit, InstantPersistence;

type
  // Test methods for class TInstantMetadata
  TestTInstantMetadata = class(TTestCase)
  private
    FInstantMetadata: TInstantMetadata;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCollectionExists;
  end;

  // Test methods for class TInstantMetadatas
  TestTInstantMetadatas = class(TTestCase)
  private
    FInstantMetadatas: TInstantMetadatas;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFind;
  end;

implementation

uses testregistry;

procedure TestTInstantMetadata.SetUp;
var
  vCollection: TInstantMetadatas;
begin
  vCollection := TInstantMetadatas.Create(nil, TInstantMetadata);
  FInstantMetadata := TInstantMetadata.Create(vCollection);
end;

procedure TestTInstantMetadata.TearDown;
var
  vCollection: TInstantMetadatas;
begin
  vCollection := FInstantMetadata.Collection;
  FreeAndNil(FInstantMetadata);
  FreeAndNil(vCollection);
end;

procedure TestTInstantMetadata.TestCollectionExists;
begin
  AssertTrue(FInstantMetadata.Collection <> nil);
end;

procedure TestTInstantMetadatas.SetUp;
var
  TestItem: TInstantMetadata;
begin
  FInstantMetadatas := TInstantMetadatas.Create(nil, TInstantMetadata);
  TestItem := TInstantMetadata(FInstantMetadatas.Add);
  TestItem.Name := 'Name';
  TestItem := TInstantMetadata(FInstantMetadatas.Add);
  TestItem.Name := 'Address';
  TestItem := TInstantMetadata(FInstantMetadatas.Add);
  TestItem.Name := 'Country';
end;

procedure TestTInstantMetadatas.TearDown;
begin
  FInstantMetadatas.Free;
  FInstantMetadatas := nil;
end;

procedure TestTInstantMetadatas.TestFind;
var
  vReturnValue: TInstantMetadata;
  vName: string;
begin
  vName := 'Address';
  vReturnValue := FInstantMetadatas.Find(vName);
  AssertEquals(vName, vReturnValue.Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantMetadata,
                 TestTInstantMetadatas]);
{$ENDIF}

end.
