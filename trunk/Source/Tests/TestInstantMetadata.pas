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
  ACollection: TInstantMetadatas;
begin
  ACollection := TInstantMetadatas.Create(nil, TInstantMetadata);
  FInstantMetadata := TInstantMetadata.Create(ACollection);
end;

procedure TestTInstantMetadata.TearDown;
begin
  FInstantMetadata.Free;
  FInstantMetadata := nil;
end;

procedure TestTInstantMetadata.TestCollectionExists;
begin
  AssertTrue('TInstantMetadata.Collection is not assigned!',
    FInstantMetadata.Collection <> nil);
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
  AssertEquals('TestFind failed!', vName, vReturnValue.Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantMetadata,
                 TestTInstantMetadatas]);
{$ENDIF}

end.
