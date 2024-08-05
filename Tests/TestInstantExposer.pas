(*
 *   InstantObjects Test Suite
 *   TestInstantExposer
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

unit TestInstantExposer;

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
  TTestExposer = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    function CreateCategory(const AId, AName: string;
      const AValue: Integer): TCategory;
    procedure AssignNameField(const Exp: TDataSet);
  protected
    FConn: TInstantXMLConnector;
    FAcc: TXMLFilesAccessor;
    FExp: TInstantExposer;
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

procedure TTestExposer.SetUp;
begin
  FAcc := TXMLFilesAccessor.Create(nil);
  FAcc.RootFolder := ExtractFilePath(ParamStr(0)) + 'XMLTestData';
  ForceDirectories(FAcc.RootFolder);
  FConn := TInstantXMLConnector.Create(nil);
  FConn.Connection := FAcc;
  //FConn.IsDefault := True;
  FConn.UseUnicode := TestModel.TestUseUnicode;
  FExp := TInstantExposer.Create(nil);

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
  //Create default Category
  FCategory := TCategory.Create(FConn);
  FCategory.Id := 'CAT000';
  FCategory.Name := 'Default Category';
  FCategory.Store;
end;

procedure TTestExposer.TearDown;

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
  FreeAndNil(FExp);
  FreeAndNil(FConn);
//  DelTree(FAcc.RootFolder);
  FreeAndNil(FAcc);
end;

procedure TTestExposer.TestBookmarks;
var
  LContact: TContact;
  LPhone1, LPhone2: TPhone;
  LBookmark: TBookmark;
  LRecordCount: Integer;
  LObjectAdded: Integer;
begin
  FExp.ObjectClass := TPhone;
  FExp.Mode := amContent;
  FExp.ContainerName := 'Phones';
  LContact := TContact.Create(FConn);
  try
    LObjectAdded := 0;
    FExp.Subject := LContact;
    LRecordCount := FExp.RecordCount;

    LPhone1 := TPhone.Create(FConn);
    LPhone1.Name := DEF_HOME;
    LPhone1.Number := DEF_NUM_HOME;
    FExp.AddObject(LPhone1);
    Inc(LObjectAdded);
    AssertEquals(FExp.RecordCount, LRecordCount+LObjectAdded);

    LPhone2 := TPhone.Create(FConn);
    LPhone2.Name := DEF_OFFICE;
    LPhone2.Number := DEF_NUM_OFFICE;
    FExp.AddObject(LPhone2);
    Inc(LObjectAdded);
    AssertEquals(FExp.RecordCount, LRecordCount+LObjectAdded);

    FExp.First;
    AssertEquals(FExp.RecNo, 1);

    LBookmark := FExp.GetBookmark;
    FExp.Last;
    AssertEquals(FExp.RecNo, LRecordCount+LObjectAdded);

    FExp.GotoBookmark(LBookmark);
    AssertEquals(FExp.RecNo, 1);

    FExp.RemoveObject(LPhone1);
    Dec(LObjectAdded);
    AssertEquals(FExp.RecordCount, LRecordCount+LObjectAdded);

    FExp.RemoveObject(LPhone2);
    Dec(LObjectAdded);
    AssertEquals(FExp.RecordCount, LRecordCount+LObjectAdded);

    FExp.Close;
    AssertEquals(FExp.RecordCount, 0);
  finally
    LContact.Free;
  end;
end;

procedure TTestExposer.TestInstantObjectRefField;
var
  LField: TField;
  LCategory: TCategory;
begin
  FExp.ObjectClass := TCategory;
  LCategory := CreateCategory(DEF_CATEG_ID, 'Default Category', 0);
  try
    FExp.FieldOptions := [foThorough, foObjects];
    FExp.Mode := amObject;
    FExp.Subject := LCategory;
    LField := FExp.FindField('Self');
    AssertNotNull('Field "Self" not found', LField);
    {$IF DEFINED(WINLINUX64) OR DEFINED(USE_LARGEINT_FIELD_FOR_REF)}
    AssertEquals(LField.ClassName, 'TLargeintField');
    {$ELSE}
    AssertEquals(LField.ClassName, 'TIntegerField');
    {$IFEND}
  finally
    FreeAndNil(LCategory);
  end;
end;

procedure TTestExposer.TestStoreAndRetrieveCategory;
var
  LCategory: TCategory;
  old_id: string;
begin
  FExp.ObjectClass := TCategory;
  LCategory := CreateCategory(DEF_CATEG_ID, 'Default Category', 0);
  try
    FExp.Mode := amObject;
    FExp.Subject := LCategory;
    FExp.Edit;
    //Test Name (Unicode)
    AssignNameField(FExp);
    FExp.Post;
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

procedure TTestExposer.TestStoreAndRetrieveContact;
var
  LContact: TContact;
  old_id: string;
  Field: TField;
begin
  FExp.ObjectClass := TContact;
  LContact := TContact.Create(FConn);
  try
    FExp.Subject := LContact;
    FExp.Edit;
    //Test Name (Unicode)
    AssignNameField(FExp);
    //Test Address.City (Unicode)
    Field := FExp.FieldByName('Address.City');
    Field.Value := DEF_CITY;
    AssertEquals(DEF_CITY, LContact.Address.City);
    //Test Address.Street (Unicode)
    Field := FExp.FieldByName('Address.Street');
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

    FExp.Post;
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

procedure TTestExposer.AssignNameField(const Exp: TDataSet);
var
  Field: TField;
begin
  Field := FExp.FieldByName('Name');
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

function TTestExposer.CreateCategory(const AId, AName: string;
  const AValue: Integer): TCategory;
begin
  Result := TCategory.Create(FConn);
  Result.Id := AId;
  Result.Name := AName;
  Result.Value := AValue;
end;

procedure TTestExposer.FieldSetValue;
var
  LCategory: TCategory;
begin
  FExp.ObjectClass := TCategory;
  LCategory := TCategory.Create(FConn);
  try
    LCategory.Name := DEF_NAME;
    FExp.Subject := LCategory;
    FExp.Edit;
    //Test Name (Unicode)
    AssignNameField(FExp);
  finally
    FreeAndNil(LCategory);
  end;
end;

procedure TTestExposer.TestStoreAndRetrieveContactPhones;
var
  LContact: TContact;
  old_id: string;
  DataSetField: TDataSetField;
  Field: TField;
begin
  FExp.ObjectClass := TContact;
  LContact := TContact.Create(FConn);
  try
    FExp.Subject := LContact;
    FExp.Edit;
    //Test Name (Unicode)
    AssignNameField(FExp);
    DataSetField := FExp.FieldByName('Phones') as TDataSetField;
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
    FExp.FieldByName('MainPhoneNumber').AsString := DEF_NUM_HOME;
    FExp.Post;
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

procedure TTestExposer.TestStoreAndRetrievePerson;
var
  p: TPerson;
  old_id: string;
  Field: TField;
  LBirthDate: TDateTime;
begin
  FExp.ObjectClass := TPerson;
  p := TPerson.Create(FConn);
  try
    FExp.Subject := p;
    FExp.Edit;
    //Test Name (Unicode)
    AssignNameField(FExp);
    //Test particular Date
    LBirthDate := EncodeDate(1974, 09, 30);
    Field := FExp.FieldByName('BirthDate');
    Field.Value := LBirthDate;
    AssertEquals(LBirthDate, p.BirthDate);
    FExp.Post;
    old_id := p.id;
  finally
    FreeAndNil(p);
  end;
  AssertNull(p);
  p := TPerson.Retrieve(old_id, False, False, FConn);
  try
    AssertNotNull('Object not retrieved', p);
    AssertEquals(old_id, p.Id);
    AssertEquals(LBirthDate, p.BirthDate);
  finally
    FreeAndNil(p);
  end;
end;

procedure TTestExposer.TestStoreAndRetrievePicture;
var
  LPerson: TPerson;
  Field: TField;
  BlobContentBefore, BlobContentAfter: string;
begin
  FExp.ObjectClass := TPerson;
  LPerson := TPerson.Create(FConn);
  try
    FExp.Subject := LPerson;
    FExp.Edit;
    Field := FExp.FieldByName('Name');
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
    Field := FExp.FieldByName('Picture');
    AssertTrue(Field is TBlobField);
    TBlobField(Field).LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Picture.bmp');
    BlobContentBefore := TBlobField(Field).AsString;
    AssertTrue(BlobContentBefore <> '');
    LPerson.Picture := BlobContentBefore;
    BlobContentAfter := TBlobField(Field).AsString;
    AssertTrue(BlobContentBefore = BlobContentAfter);
    FExp.Post;
  finally
    FreeAndNil(LPerson);
  end;
end;

initialization
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TTestExposer]);
{$ENDIF}
end.

