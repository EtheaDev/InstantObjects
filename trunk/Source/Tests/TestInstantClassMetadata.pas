unit TestInstantClassMetadata;

interface

uses fpcunit, InstantMock, InstantPersistence;

type
  // Test methods for class TInstantClassMetadata
  TestTInstantClassMetadata = class(TTestCase)
  private
    FClassCount: Integer;
    FConn: TInstantMockConnector;
    FInstantClassMetadata: TInstantClassMetadata;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAssign;
    procedure TestAttributeMetadatas;
    procedure TestCollection;
    procedure TestFindInstantAttributeMetadata;
    procedure TestIsEmpty;
    procedure TestIsStored;
    procedure TestMemberMap;
    procedure TestParentName;
    procedure TestPersistence;
    procedure TestStorageMaps;
    procedure TestStorageName;
    procedure TestTableName;
  end;

  // Test methods for class TInstantClassMetadatas
  TestTInstantClassMetadatas = class(TTestCase)
  private
    FInstantClassMetadatas: TInstantClassMetadatas;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestFind;
    procedure TestItems;
  end;

implementation

uses SysUtils, TypInfo, testregistry;

procedure TestTInstantClassMetadata.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if FClassCount > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  // Load a default ClassMetadata
  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TContact');
end;

procedure TestTInstantClassMetadata.TearDown;
begin
  FInstantClassMetadata := nil;
  FConn.Free;
end;

procedure TestTInstantClassMetadata.TestAssign;
var
  vDest, vSource: TInstantClassMetadata;
  vStr: string;
begin
  vSource := TInstantClassMetadata.Create(nil);
  vDest := TInstantClassMetadata.Create(nil);
  try
    vSource.DefaultContainerName := 'DefaultContainerName';
    vSource.StorageName := 'StorageName';
    vSource.Persistence := peStored;
    vDest.Assign(vSource);
    AssertEquals('DefaultContainerName is incorrect!', 'DefaultContainerName',
      vDest.DefaultContainerName);
    AssertEquals('StorageName is incorrect!', 'StorageName',
      vDest.StorageName);
    vStr := GetEnumName(TypeInfo(TInstantPersistence),
      Ord(vDest.Persistence));
    AssertEquals('PersistenceType incorrect', 'peStored', vStr);
  finally
    vSource.Free;
    vDest.Free;
  end;
end;

procedure TestTInstantClassMetadata.TestAttributeMetadatas;
begin
  AssertNotNull('AttributeMetadatas is nil!',
    FInstantClassMetadata.AttributeMetadatas);
end;

procedure TestTInstantClassMetadata.TestCollection;
begin
  // Collection property contains all of the class metadatas in the model
  AssertNotNull('Collection is nil!', FInstantClassMetadata.Collection);
  AssertEquals('The number of model classes is incorrect!', 9,
    FInstantClassMetadata.Collection.Count);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TPhone');
  AssertEquals('The number of model classes is incorrect!', 9,
    FInstantClassMetadata.Collection.Count);
end;

procedure TestTInstantClassMetadata.TestFindInstantAttributeMetadata;
begin
  AssertNotNull(FInstantClassMetadata);
end;

procedure TestTInstantClassMetadata.TestIsEmpty;
begin
  AssertFalse('FInstantClassMetadata is empty!', FInstantClassMetadata.IsEmpty);
end;

procedure TestTInstantClassMetadata.TestIsStored;
begin
  AssertTrue('Incorrect storage flag!', FInstantClassMetadata.IsStored);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TContactFilter');
  AssertFalse('Incorrect storage flag!', FInstantClassMetadata.IsStored);
end;

procedure TestTInstantClassMetadata.TestMemberMap;
begin
  AssertNotNull('MemberMap is nil!', FInstantClassMetadata.MemberMap);
end;

procedure TestTInstantClassMetadata.TestParentName;
begin
  AssertEquals('ParentName incorrect!', '',
    FInstantClassMetadata.ParentName);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TPerson');
  AssertEquals('ParentName incorrect!', 'TContact',
    FInstantClassMetadata.ParentName);
end;

procedure TestTInstantClassMetadata.TestPersistence;
var
  vStr: string;
begin
  vStr := GetEnumName(TypeInfo(TInstantPersistence),
    Ord(FInstantClassMetadata.Persistence));
  AssertEquals('PersistenceType incorrect', 'peStored', vStr);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TContactFilter');
  vStr := GetEnumName(TypeInfo(TInstantPersistence),
    Ord(FInstantClassMetadata.Persistence));
  AssertEquals('PersistenceType incorrect', 'peEmbedded', vStr);
end;

procedure TestTInstantClassMetadata.TestStorageMaps;
begin
  AssertNotNull('StorageMaps is nil!', FInstantClassMetadata.StorageMaps);

  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TContactFilter');
  AssertNull('StorageMaps should be nil!', FInstantClassMetadata.StorageMaps);
end;

procedure TestTInstantClassMetadata.TestStorageName;
begin
  // Test with default class StorageName returns ''.
  AssertEquals('StorageName incorrect!', '',
    FInstantClassMetadata.StorageName);

  // Test for User entered non-default class StorageName.
  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TCategory');
  AssertEquals('StorageName incorrect!', 'Categories',
    FInstantClassMetadata.StorageName);
end;

procedure TestTInstantClassMetadata.TestTableName;
begin
  // Test with default class StorageName (TableName).
  AssertEquals('TableName incorrect!', 'Contact',
    FInstantClassMetadata.TableName);

  // Test for User entered non-default class StorageName (TableName).
  FInstantClassMetadata := InstantModel.ClassMetadatas.Find('TCategory');
  AssertEquals('TableName incorrect!', 'Categories',
    FInstantClassMetadata.TableName);
end;

procedure TestTInstantClassMetadatas.SetUp;
var
  TestItem: TInstantClassMetadata;
begin
  FInstantClassMetadatas := TInstantClassMetadatas.Create(nil);
  TestItem := TInstantClassMetadata(FInstantClassMetadatas.Add);
  TestItem.Name := 'TPerson';
  TestItem := TInstantClassMetadata(FInstantClassMetadatas.Add);
  TestItem.Name := 'TAddress';
  TestItem := TInstantClassMetadata(FInstantClassMetadatas.Add);
  TestItem.Name := 'TCountry';
end;

procedure TestTInstantClassMetadatas.TearDown;
begin
  FInstantClassMetadatas.Free;
  FInstantClassMetadatas := nil;
end;

procedure TestTInstantClassMetadatas.TestAdd;
var
  vReturnValue: TInstantClassMetadata;
begin
  vReturnValue := FInstantClassMetadatas.Add;
  AssertNotNull('vReturnValue is nil!', vReturnValue);
  AssertEquals('Count is incorrect!', 4, FInstantClassMetadatas.Count);
  FInstantClassMetadatas.Remove(vReturnValue);
  AssertEquals('Count is incorrect!', 3, FInstantClassMetadatas.Count);
end;

procedure TestTInstantClassMetadatas.TestFind;
var
  vReturnValue: TInstantClassMetadata;
  vName: string;
begin
  vName := 'TAddress';
  vReturnValue := FInstantClassMetadatas.Find(vName);
  AssertEquals('TestFind failed!', vName, vReturnValue.Name);
end;

procedure TestTInstantClassMetadatas.TestItems;
begin
  AssertEquals('The second item''s name is incorrect!', 'TAddress',
    FInstantClassMetadatas.Items[1].Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantClassMetadata,
                 TestTInstantClassMetadatas]);
{$ENDIF}

end.
