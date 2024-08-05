(*
 *   InstantObjects Test Suite
 *   TestMinimalModel
 *
 * ***** BEGIN LICENSE BLOCK *****
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
 * The Original Code is: InstantObjects Test Suite/TestMinimalModel
 *
 * The Initial Developer of the Original Code is: Uberto Barbini
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestFullModel;

interface

uses
  Classes, SysUtils, {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantMock, StrUtils,
  InstantXML, InstantConsts, InstantClasses,
  TestRandomData, TestModel, InstantMetadata,
  DUnitX.TestFramework;

type
  { TTestFullModel }
  [TestFixture]
  TTestFullModel = class({$IFNDEF DUNITX_TESTS}TTestCase{$ELSE}TInstantTestCase{$ENDIF})
  private
    function CompanyFileName(c: TCompany; s: TInstantStreamFormat): string;
    function PersonFileName(p: TPerson; s: TInstantStreamFormat): string;
    function MetaDataFileName(cm: TInstantClassMetadata; s: TInstantStreamFormat): string;
  protected
    FStoragePath: string;
    FConn: TInstantXMLConnector;
    [Setup]
    procedure SetUp; override;
    [Teardown]
    procedure TearDown; override;
  public
  published
    [Test]
    procedure TestInstantObjectToXML;
    procedure TestInstantObjectToXMLMetadata;
    procedure TestStoreObjects;
    procedure TestRetrieveObjects;
    {$IFDEF DELPHI_NEON}
    procedure TestInstantObjectToJSON;
    procedure TestInstantObjectToJSONMetadata;
    {$ENDIF}
  end;

implementation

uses
  InstantPersistence, InstantTypes, TestDemoData;

{ TTestFullModel }

function TTestFullModel.CompanyFileName(c: TCompany;
  s: TInstantStreamFormat): string;
begin
  Result := FStoragePath+'Company\TCompany.'+c.Id+'.1.';
  case s of
    sfBinary: Result := Result + 'bin';
    sfXML: Result := Result + 'xml';
    {$IFDEF DELPHI_NEON}
    sfJSON: Result := Result + 'json';
    {$ENDIF}
  end;
end;

function TTestFullModel.MetaDataFileName(cm: TInstantClassMetadata;
  s: TInstantStreamFormat): string;
begin
  Result := FStoragePath+'MetaData\'+cm.ClassName+'.';
  case s of
    sfBinary: Result := Result + 'bin';
    sfXML: Result := Result + 'xml';
    {$IFDEF DELPHI_NEON}
    sfJSON: Result := Result + 'json';
    {$ENDIF}
  end;
end;

function TTestFullModel.PersonFileName(p: TPerson;
  s: TInstantStreamFormat): string;
begin
  Result := FStoragePath+'Person\TPerson.'+p.Id+'.1.';
  case s of
    sfBinary: Result := Result + 'bin';
    sfXML: Result := Result + 'xml';
    {$IFDEF DELPHI_NEON}
    sfJSON: Result := Result + 'json';
    {$ENDIF}
  end;
end;

procedure TTestFullModel.SetUp;
begin
  FConn := TInstantXMLConnector.Create(nil);
  FConn.Connection := TXMLFilesAccessor.Create(FConn);
  FStoragePath := ExtractFilePath(ParamStr(0))+'XMLTestData'+PathDelim;
  FConn.Connection.RootFolder := FStoragePath;
  ForceDirectories(FConn.Connection.RootFolder);
  ForceDirectories(FConn.Connection.RootFolder+'SampleClass\');
  ForceDirectories(FConn.Connection.RootFolder+'Person\');
  ForceDirectories(FConn.Connection.RootFolder+'Company\');
  ForceDirectories(FConn.Connection.RootFolder+'MetaData\');
  ForceDirectories(FConn.Connection.RootFolder+'Categories\');
  ForceDirectories(FConn.Connection.RootFolder+'Country\');

  TInstantXMLBroker.Create(FConn);
  FConn.Connect;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  CreateCategories(FConn);
  CreateCountries(FConn);
end;

procedure TTestFullModel.TearDown;
begin
  FConn.Disconnect;
  FConn.Free;
  InstantModel.ClassMetadatas.Clear;
end;

procedure TTestFullModel.TestInstantObjectToXML;
var
  ss: TStringStream;
  s: string;
  p: TPerson;
  c: TCompany;
  LExpected, LValue: string;
  LGender: TGender;
begin
  s := '';
  ss := TStringStream.Create(s, TEncoding.UTF8);
  c := CreateRandomCompany(FConn);
  p := CreateRandomPerson(FConn, c, LGender);
  try
    //Save Company to file
    InstantWriteObject(ss, sfXML, c);
    ss.SaveToFile(CompanyFileName(c, sfXML));
    LValue := ss.DataString;

    //Reload Company from file
    ss.LoadFromFile(CompanyFileName(c, sfXML));
    LExpected := ss.DataString;
    AssertEquals('InstantWriteObject(sfXML)', LExpected, LValue);

    //Save Person to file
    ss.Clear;
    InstantWriteObject(ss, sfXML, p);
    ss.SaveToFile(PersonFileName(p, sfXML));
    LValue := ss.DataString;

    //Reload Person from file
    ss.LoadFromFile(PersonFileName(p, sfXML));
    LExpected := ss.DataString;
    AssertEquals('InstantReadObject(sfXML)', LExpected, LValue);
  finally
    p.Free;
    c.Free;
    ss.Free;
  end;
end;

procedure TTestFullModel.TestInstantObjectToXMLMetadata;
var
  LIOMetadata: TInstantClassMetadata;
  LStringStream: TStringStream;
  LFileName: string;
  lClassName: string;
  LExpected, LActual: string;
begin
  lClassName := 'TSampleClass';
  LIOMetadata := InstantFindClassMetadata(LClassName);
  if Assigned(LIOMetadata) then
  begin
    LFileName := MetaDataFileName(LIOMetadata, sfXML);
    //Save Metadata to file
    LStringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      InstantWriteObject(LStringStream, sfXML, LIOMetadata);
      LStringStream.SaveToFile(LFileName);
      LExpected := LStringStream.DataString;
    finally
      LStringStream.Free;
    end;
    //Load Metadata from file
    LStringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      LStringStream.LoadFromFile(LFileName);
      InstantReadObject(LStringStream, sfXML, LIOMetadata);
      LActual := LStringStream.DataString;

      AssertEqualsXML(LExpected, LActual, 'InstantObject Metadata for '+LClassName);
    finally
      LStringStream.Free;
    end;
  end;
end;

{$IFDEF DELPHI_NEON}
procedure TTestFullModel.TestInstantObjectToJSON;
var
  ss: TStringStream;
  s: string;
  p: TPerson;
  c: TCompany;
  LExpected, LValue: string;
  LGender: TGender;
begin
  s := '';
  ss := TStringStream.Create(s, TEncoding.UTF8);
  c := CreateRandomCompany(FConn);
  p := CreateRandomPerson(FConn, c, LGender);
  try
    //Save Company to file
    InstantWriteObject(ss, sfJSON, c);
    ss.SaveToFile(CompanyFileName(c, sfJSON));
    LValue := ss.DataString;

    //Reload Company from file
    ss.LoadFromFile(CompanyFileName(c, sfJSON));
    LExpected := ss.DataString;
    AssertEquals('InstantWriteObject(sfJSON)', LExpected, LValue);

    //Save Person to file
    ss.Clear;
    InstantWriteObject(ss, sfJSON, p);
    ss.SaveToFile(PersonFileName(p, sfJSON));
    LValue := ss.DataString;

    //Reload Person from file
    ss.LoadFromFile(PersonFileName(p, sfJSON));
    LExpected := ss.DataString;
    AssertEquals('InstantReadObject(sfJSON)', LExpected, LValue);
  finally
    p.Free;
    c.Free;
    ss.Free;
  end;
end;

procedure TTestFullModel.TestInstantObjectToJSONMetadata;
var
  LIOMetadata: TInstantClassMetadata;
  LStringStream: TStringStream;
  LClassName: string;
  LBefore: string;
begin
  lClassName := 'TSampleClass';
  LIOMetadata := InstantFindClassMetadata(lClassName);
  if Assigned(LIOMetadata) then
  begin
    LStringStream := TStringStream.Create;
    try
      InstantWriteObject(LStringStream, sfJSON, LIOMetadata);
      LStringStream.SaveToFile(MetaDataFileName(LIOMetadata, sfJSON));
      LBefore := LStringStream.DataString;
    finally
      LStringStream.Free;
    end;
  end;
end;
{$ENDIF}

procedure TTestFullModel.TestStoreObjects;
var
  i: integer;
  c: TCompany;
  p: TPerson;
  LGender: TGender;
  LCompanyFileName, LPersonFileName: string;
begin
  for i := 0 to 1 do
  begin
    p := nil;
    c := CreateRandomCompany(FConn);
    try
      c.Store;
      LCompanyFileName := CompanyFileName(c, sfXML);
      AssertTrue(FileExists(LCompanyFileName));
      p := CreateRandomPerson(FConn, c, LGender);
      p.Store;
      LPersonFileName := PersonFileName(p, sfXML);
      AssertTrue(FileExists(LPersonFileName));
      DeleteFile(LCompanyFileName);
      DeleteFile(LPersonFileName);
    finally
      p.Free;
      c.Free;
    end;
  end;
end;

procedure TTestFullModel.TestRetrieveObjects;
var
  i: integer;
  c: TCompany;
  p: TPerson;
  LCompanyName, LCompanyId: string;
  LPersonName, LPersonId: string;
  LGender: TGender;
begin
  for i := 0 to 1 do
  begin
    c := CreateRandomCompany(FConn);
    try
      LCompanyName := c.Name;
      c.Store;
      LCompanyId := c.Id;
      p := CreateRandomPerson(FConn, c, LGender);
      LPersonName := p.Name;
      p.Store;
      LPersonId := p.Id;
    finally
      FreeAndNil(c);
    end;
    p := TPerson.Retrieve(LPersonId, False, False, FConn);
    try
      AssertEquals(LPersonName, p.Name);
      AssertEquals(LCompanyName, p.Employer.Name);
    finally
      FreeAndNil(p);
    end;
  end;
end;

initialization
  // Register any test cases with the test runner (old version)
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TTestFullModel]);
{$ENDIF}

end.


