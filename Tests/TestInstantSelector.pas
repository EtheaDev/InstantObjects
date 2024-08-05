(*
 *   InstantObjects Test Suite
 *   TestInstantSelector
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
 * The Original Code is: InstantObjects Test Suite/TestMockBroker
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestInstantSelector;

interface

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

uses
  {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantXML, InstantPresentation, DB,
  DUnitX.TestFramework, TestModel;

type
  [TestFixture]
  TTestSelector = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    function CreateCategory(const AId, AName: string;
      const AValue: Integer): TCategory;
    procedure AssignNameField(const Exp: TDataSet);

  protected
    FConn: TInstantXMLConnector;
    FAcc: TXMLFilesAccessor;
    FSelector: TInstantSelector;
    FCategory: TCategory;
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure FieldSetValue;
    procedure TestBookmarks;
    procedure TestInstantObjectRefField;
    procedure TestStoreAndRetrieveCategory;
    procedure TestStoreAndRetrieveContact;
    procedure TestStoreAndRetrievePerson;
    procedure TestStoreAndRetrievePicture;
    procedure TestStoreAndRetrieveContactPhones;
  end;

implementation

uses
  SysUtils, Classes, ShellAPI, InstantPersistence, Graphics;

const
  DEF_CATEG_ID = 'CATEG_ID';
  DEF_NAME = 'AName';
  DEF_NAME_UNICODE = '网站导航';
  DEF_CITY = 'Milan (€)';
  ADDRESS_STREET = 'Street';
  ADDRESS_STREET_UNICODE = '链接';
  DEF_HOME = 'Home';
  DEF_OFFICE = 'Office';
  DEF_NUM_HOME  = '012-123-45678';
  DEF_NUM_OFFICE = '012-234-56781';

{ TTestXMLBroker }

procedure TTestSelector.SetUp;
begin
  FAcc := TXMLFilesAccessor.Create(nil);
  FAcc.RootFolder := ExtractFilePath(ParamStr(0)) + 'XMLTestData';
  ForceDirectories(FAcc.RootFolder);
  FConn := TInstantXMLConnector.Create(nil);
  FConn.Connection := FAcc;
  //FConn.IsDefault := True;
  FConn.UseUnicode := TestModel.TestUseUnicode;
  FSelector := TInstantSelector.Create(nil);
  FSelector.Connector := FConn;
  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  //Create default Category
  FCategory := TCategory.Create(FConn);
  FCategory.Id := 'CAT000';
  FCategory.Name := 'Default Category';
  FCategory.Store;
end;

procedure TTestSelector.TearDown;

  function DelTree(DirName: string): Boolean;
  var
    SHFileOpStruct : TSHFileOpStruct;
  begin
    try
      Fillchar(SHFileOpStruct,Sizeof(SHFileOpStruct),0) ;
      with SHFileOpStruct do begin
        Wnd := 0;
        pFrom := PChar(ExcludeTrailingPathDelimiter(DirName) + #0);
        wFunc := FO_DELETE;
        fFlags := FOF_ALLOWUNDO;
        fFlags := fFlags or FOF_NOCONFIRMATION;
        fFlags := fFlags or FOF_SILENT;
       end;
       Result := (SHFileOperation(SHFileOpStruct) = 0) ;
    except
      Result := False;
    end;
  end;

begin
  inherited;
  FreeAndNil(FCategory);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FSelector);
  FreeAndNil(FConn);
//  DelTree(FAcc.RootFolder);
  FreeAndNil(FAcc);
end;

procedure TTestSelector.TestBookmarks;
var
  LCat1, LCat2, LCat3: TCategory;
  LBookmark: TBookmark;
  LRecordCount: Integer;
  LObjectAdded: Integer;
begin
  FSelector.ObjectClass := TCategory;
  FSelector.Command.Text := 'SELECT * FROM ANY TCategory';
  FSelector.Open;
  LObjectAdded := 0;
  LBookmark := FSelector.GetBookmark;
  FSelector.Last;
  FSelector.GotoBookmark(LBookmark);

  LRecordCount := FSelector.RecordCount;

  LCat1 := CreateCategory('CAT001', 'Category 001', 1);
  FSelector.AddObject(LCat1);
  Inc(LObjectAdded);
  AssertEquals(FSelector.RecordCount, LRecordCount+LObjectAdded);

  LCat2 := CreateCategory('CAT002', 'Category 002', 2);
  FSelector.AddObject(LCat2);
  Inc(LObjectAdded);
  AssertEquals(FSelector.RecordCount, LRecordCount+LObjectAdded);

  LCat3 := CreateCategory('CAT003', 'Category 003', 3);
  FSelector.AddObject(LCat3);
  Inc(LObjectAdded);
  AssertEquals(FSelector.RecordCount, LRecordCount+LObjectAdded);

  FSelector.First;
  AssertEquals(FSelector.RecNo, 1);

  LBookmark := FSelector.GetBookmark;
  FSelector.Last;
  AssertEquals(FSelector.RecNo, LRecordCount+LObjectAdded);

  FSelector.GotoBookmark(LBookmark);
  AssertEquals(FSelector.RecNo, 1);

  FSelector.RemoveObject(LCat1);
  Dec(LObjectAdded);
  AssertEquals(FSelector.RecordCount, LRecordCount+LObjectAdded);

  FSelector.RemoveObject(LCat2);
  Dec(LObjectAdded);
  AssertEquals(FSelector.RecordCount, LRecordCount+LObjectAdded);

  FSelector.RemoveObject(LCat3);
  Dec(LObjectAdded);
  AssertEquals(FSelector.RecordCount, LRecordCount+LObjectAdded);

  FSelector.Close;
  AssertEquals(FSelector.RecordCount, 0);
end;

procedure TTestSelector.TestInstantObjectRefField;
var
  LField: TField;
begin
  FSelector.ObjectClass := TCategory;
  FSelector.Command.Text := 'SELECT * FROM ANY TCategory';
  FSelector.FieldOptions := [foThorough, foObjects];
  FSelector.Open;
  LField := FSelector.FindField('Self');
  AssertNotNull('Field "Self" not found', LField);
  {$IF DEFINED(WINLINUX64) OR DEFINED(USE_LARGEINT_FIELD_FOR_REF)}
  AssertEquals(LField.ClassName, 'TLargeintField');
  {$ELSE}
  AssertEquals(LField.ClassName, 'TIntegerField');
  {$IFEND}
end;

procedure TTestSelector.TestStoreAndRetrieveCategory;
var
  LCategory: TCategory;
  old_id: string;
begin
  FSelector.ObjectClass := TCategory;
  FSelector.Command.Text := 'SELECT * FROM Any TCategory';
  FSelector.Open;
  LCategory := CreateCategory(DEF_CATEG_ID, 'Default Category', 0);
  try
    FSelector.AddObject(LCategory);
    FSelector.Edit;
    //Test Name (Unicode)
    AssignNameField(FSelector);
    FSelector.Post;
    old_id := LCategory.id;
  finally
    FreeAndNil(LCategory);
  end;
  AssertNull(LCategory);
  LCategory := TCategory.Retrieve(old_id, False, False, FConn);
  try
    AssertNotNull('Object not retrieved', LCategory);
    AssertEquals(old_id, LCategory.Id);
    AssertEquals(DEF_CATEG_ID, LCategory.Id);
    if not FConn.UseUnicode then
    begin
      AssertEquals(DEF_NAME, LCategory.Name);
    end
    else
    begin
      AssertEquals(DEF_NAME_UNICODE, LCategory.Name);
    end;
  finally
    FreeAndNil(LCategory);
  end;
end;

procedure TTestSelector.TestStoreAndRetrieveContact;
var
  LContact: TContact;
  old_id: string;
  Field: TField;
begin
  FSelector.ObjectClass := TContact;
  FSelector.Command.Text := 'SELECT * FROM ANY TContact';
  FSelector.Open;
  LContact := TContact.Create(FConn);
  try
    FSelector.AddObject(LContact);
    FSelector.Edit;
    //Test Name (Unicode)
    AssignNameField(FSelector);
    //Test Address.City (Unicode)
    Field := FSelector.FieldByName('Address.City');
    Field.Value := DEF_CITY;
    AssertEquals(DEF_CITY, LContact.Address.City);
    //Test Address.Street (Unicode)
    Field := FSelector.FieldByName('Address.Street');
    if not FConn.UseUnicode then
    begin
      Field.Value := ADDRESS_STREET;
      AssertEquals(ADDRESS_STREET, LContact.Address.Street);
    end
    else
    begin
      Field.Value := ADDRESS_STREET_UNICODE;
      AssertEquals(ADDRESS_STREET_UNICODE, LContact.Address.Street);
    end;

    FSelector.Post;
    if not FConn.UseUnicode then
    begin
      AssertEquals(DEF_NAME, LContact.Name);
      AssertEquals(DEF_CITY, LContact.Address.City);
      AssertEquals(ADDRESS_STREET, LContact.Address.Street);
    end
    else
    begin
      AssertEquals(DEF_NAME_UNICODE, LContact.Name);
      AssertEquals(DEF_CITY, LContact.Address.City);
      AssertEquals(ADDRESS_STREET_UNICODE, LContact.Address.Street);
    end;
    old_id := LContact.id;
  finally
    FreeAndNil(LContact);
  end;
  AssertNull(LContact);
  LContact := TContact.Retrieve(old_id, False, False, FConn);
  try
    AssertNotNull('Object not retrieved', LContact);
    AssertEquals(old_id, LContact.Id);
    if not FConn.UseUnicode then
    begin
      AssertEquals(DEF_NAME, LContact.Name);
      AssertEquals(DEF_CITY, LContact.Address.City);
      AssertEquals(ADDRESS_STREET, LContact.Address.Street);
    end
    else
    begin
      AssertEquals(DEF_NAME_UNICODE, LContact.Name);
      AssertEquals(DEF_CITY, LContact.Address.City);
      AssertEquals(ADDRESS_STREET_UNICODE, LContact.Address.Street);
    end;
    AssertNotNull(LContact.Address);
  finally
    FreeAndNil(LContact);
  end;
end;

procedure TTestSelector.AssignNameField(const Exp: TDataSet);
var
  Field: TField;
begin
  Field := FSelector.FieldByName('Name');
  if not FConn.UseUnicode then
  begin
    Field.Value := DEF_NAME;
    AssertEquals(DEF_NAME, Field.Value);
  end
  else
  begin
    Field.Value := DEF_NAME_UNICODE;
    AssertEquals(DEF_NAME_UNICODE, Field.Value);
  end;
end;

function TTestSelector.CreateCategory(const AId, AName: string;
  const AValue: Integer): TCategory;
begin
  Result := TCategory.Create(FConn);
  Result.Id := AId;
  Result.Name := AName;
  Result.Value := AValue;
end;

procedure TTestSelector.FieldSetValue;
var
  LCategory: TCategory;
begin
  FSelector.ObjectClass := TCategory;
  FSelector.Command.Text := 'SELECT * FROM ANY TCategory';
  FSelector.Open;
  LCategory := TCategory.Create(FConn);
  try
    LCategory.Name := DEF_NAME;
    FSelector.AddObject(LCategory);
    FSelector.Edit;
    //Test Name (Unicode)
    AssignNameField(FSelector);
  finally
    FreeAndNil(LCategory);
  end;
end;

procedure TTestSelector.TestStoreAndRetrieveContactPhones;
var
  LContact: TContact;
  old_id: string;
  DataSetField: TDataSetField;
  Field: TField;
begin
  FSelector.ObjectClass := TContact;
  FSelector.Command.Text := 'SELECT * FROM ANY TContact';
  FSelector.Open;
  LContact := TContact.Create(FConn);
  try
    FSelector.AddObject(LContact);
    FSelector.Edit;
    //Test Name (Unicode)
    AssignNameField(FSelector);
    DataSetField := FSelector.FieldByName('Phones') as TDataSetField;
    DataSetField.NestedDataSet.Append;
    Field := DataSetField.NestedDataSet.FieldByName('Name');
    Field.Value := DEF_HOME;
    Field := DataSetField.NestedDataSet.FieldByName('Number');
    Field.Value := DEF_NUM_HOME;
    DataSetField.NestedDataSet.Post;
    AssertEquals(1, LContact.PhoneCount);
    DataSetField.NestedDataSet.Append;
    DataSetField.NestedDataSet.FieldByName('Name').Value := DEF_OFFICE;
    DataSetField.NestedDataSet.FieldByName('Number').Value := DEF_NUM_OFFICE;
    DataSetField.NestedDataSet.Post;
    AssertEquals(2, LContact.PhoneCount);
    FSelector.FieldByName('MainPhoneNumber').AsString := DEF_NUM_HOME;
    FSelector.Post;
    AssertEquals(2, LContact.PhoneCount);
    AssertEquals(DEF_HOME, LContact.Phones[0].Name);
    AssertEquals(DEF_NUM_HOME, LContact.Phones[0].Number);
    AssertEquals(DEF_OFFICE, LContact.Phones[1].Name);
    AssertEquals(DEF_NUM_OFFICE, LContact.Phones[1].Number);
    old_id := LContact.id;
  finally
    FreeAndNil(LContact);
  end;
  AssertNull(LContact);
  LContact := TContact.Retrieve(old_id, False, False, FConn);
  try
    AssertNotNull('Object not retrieved', LContact);
    AssertEquals(old_id, LContact.Id);
    if not FConn.UseUnicode then
      AssertEquals(DEF_NAME, LContact.Name)
    else
      AssertEquals(DEF_NAME_UNICODE, LContact.Name);
    AssertNotNull(LContact.Address);
    AssertEquals(2, LContact.PhoneCount);
    AssertEquals(DEF_HOME, LContact.Phones[0].Name);
    AssertEquals(DEF_NUM_HOME, LContact.Phones[0].Number);
    AssertEquals(DEF_OFFICE, LContact.Phones[1].Name);
    AssertEquals(DEF_NUM_OFFICE, LContact.Phones[1].Number);
  finally
    FreeAndNil(LContact);
  end;
end;

procedure TTestSelector.TestStoreAndRetrievePerson;
var
  LPerson: TPerson;
  old_id: string;
  Field: TField;
  LBirthDate: TDateTime;
begin
  FSelector.ObjectClass := TPerson;
  FSelector.Command.Text := 'SELECT * FROM ANY TPerson';
  FSelector.Open;
  LPerson := TPerson.Create(FConn);
  try
    FSelector.AddObject(LPerson);
    FSelector.Edit;
    //Test Name (Unicode)
    AssignNameField(FSelector);
    //Test particular Date
    LBirthDate := EncodeDate(1974, 09, 30);
    Field := FSelector.FieldByName('BirthDate');
    Field.Value := LBirthDate;
    AssertEquals(LBirthDate, LPerson.BirthDate);
    FSelector.Post;
    old_id := LPerson.id;
  finally
    FreeAndNil(LPerson);
  end;
  AssertNull(LPerson);
  LPerson := TPerson.Retrieve(old_id, False, False, FConn);
  try
    AssertNotNull('Object not retrieved', LPerson);
    AssertEquals(old_id, LPerson.Id);
    AssertEquals(LBirthDate, LPerson.BirthDate);
  finally
    FreeAndNil(LPerson);
  end;
end;

procedure TTestSelector.TestStoreAndRetrievePicture;
var
  LPerson: TPerson;
  Field: TField;
  BlobContentBefore, BlobContentAfter: string;
begin
  FSelector.ObjectClass := TPerson;
  FSelector.Command.Text := 'SELECT * FROM ANY TPerson';
  FSelector.Open;
  LPerson := TPerson.Create(FConn);
  try
    FSelector.AddObject(LPerson);
    FSelector.Edit;
    Field := FSelector.FieldByName('Name');
    if not FConn.UseUnicode then
    begin
      Field.Value := DEF_NAME;
      AssertEquals(DEF_NAME, LPerson.Name);
    end
    else
    begin
      Field.Value := DEF_NAME_UNICODE;
      AssertEquals(DEF_NAME_UNICODE, LPerson.Name);
    end;
    Field := FSelector.FieldByName('Picture');
    AssertTrue(Field is TBlobField);
    TBlobField(Field).LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Picture.bmp');
    BlobContentBefore := TBlobField(Field).AsString;
    AssertTrue(BlobContentBefore <> '');
    LPerson.Picture := BlobContentBefore;
    BlobContentAfter := TBlobField(Field).AsString;
    AssertTrue(BlobContentBefore = BlobContentAfter);
    FSelector.Post;
  finally
    FreeAndNil(LPerson);
  end;
end;

initialization
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TTestSelector]);
{$ENDIF}
end.

