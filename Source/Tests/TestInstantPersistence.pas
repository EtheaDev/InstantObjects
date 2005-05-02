unit TestInstantPersistence;

interface

uses
  Classes, SysUtils,
  InstantPersistence,
  fpcunit,
  testregistry;

type

  { TTestInstantPersistence }

  TTestInstantPersistence = class(TTestCase)
  published
    procedure TestMetadatas;
    procedure TestInstantAttributeMetadatas;
    procedure TestInstantFieldMetadatas;
    procedure TestInstantString;
  end;

implementation


{ TTestInstantPersistence }

procedure TTestInstantPersistence.TestMetadatas;
var
  i: TInstantMetadata;
  c: TInstantMetadatas;
begin
  c := TInstantMetadatas.Create(nil, TInstantMetadata);
  AssertNotNull(c);
  AssertEquals(0, c.Count);
  i := c.add as TInstantMetadata;
  AssertEquals(1, c.Count);
  i.Name := 'pippo';
  AssertTrue(i = c.Find('pippo'));
  AssertTrue(i.Collection = c);
end;

procedure TTestInstantPersistence.TestInstantAttributeMetadatas;
var
  i: TInstantAttributeMetadata;
  c: TInstantAttributeMetadatas;
begin
  c := TInstantAttributeMetadatas.Create(nil);
  AssertNotNull(c);
  AssertEquals(0, c.Count);
  i := c.add;
  AssertEquals(1, c.Count);
  i.Name := 'pippo';
  i.DefaultValue := '1';
  AssertTrue(i = c.Find('pippo'));
  AssertTrue(i.Collection = c);
  AssertEquals('1', i.DefaultValue);
end;

procedure TTestInstantPersistence.TestInstantFieldMetadatas;
var
  i: TInstantFieldMetadata;
  c: TInstantFieldMetadatas;
begin
  c := TInstantFieldMetadatas.Create(nil);
  AssertNotNull(c);
  AssertEquals(0, c.Count);
  i := c.add;
  AssertEquals(1, c.Count);
  i.Name := 'pippo';
  AssertTrue(i = c.Find('pippo'));
  AssertTrue(i.Collection = c);
end;

procedure TTestInstantPersistence.TestInstantString;
var
  c: TInstantString;
begin
  c := TInstantString.Create();
  try
    AssertFalse(c.IsChanged);
    c.AsString := 'goofy';
    AssertTrue(c.IsChanged);
  finally
    c.Free;
  end;
end;


initialization
  RegisterTests([TTestInstantPersistence]);

end.
