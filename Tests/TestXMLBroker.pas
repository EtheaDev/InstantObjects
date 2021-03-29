(*
 *   InstantObjects Test Suite
 *   TestMockBroker
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

unit TestXMLBroker;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  fpcunit, testregistry, InstantXML;

type
  TTestXMLBroker = class(TTestCase)
  private
  protected
    FConn: TInstantXMLConnector;
    FAcc: TXMLFilesAccessor;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStoreAndRetrieveContact;
    procedure TestOrderBy;
  end;

implementation

uses
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, ShellAPI, InstantPersistence, TestModel;

{ TTestXMLBroker }

procedure TTestXMLBroker.SetUp;
begin
  FAcc := TXMLFilesAccessor.Create(nil);
  FAcc.RootFolder := ExtractFilePath(ParamStr(0)) + 'XMLDB';
  ForceDirectories(FAcc.RootFolder);
  FConn := TInstantXMLConnector.Create(nil);
  FConn.Connection := FAcc;
  FConn.UseUnicode := TestModel.TestUseUnicode;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
end;

procedure TTestXMLBroker.TearDown;

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
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
  DelTree(FAcc.RootFolder);
  FreeAndNil(FAcc);
end;

procedure TTestXMLBroker.TestOrderBy;
var
  c1, c2: TContact;
  t: TPhone;
  LQuery: TInstantQuery;
begin
  FConn.IsDefault := True;

  c1 := TContact.Create;
  try
    c1.Name := 'ZZTop';
    c1.Address.City := 'Dallas';
    t := TPhone.Create;
    t.Name := 'Home';
    t.Number := '012 12345678';
    c1.AddPhone(t);
    AssertEquals(1, c1.PhoneCount);
    t := TPhone.Create;
    t.Name := 'Office';
    t.Number := '012 23456781';
    c1.AddPhone(t);
    AssertEquals(2, c1.PhoneCount);
    c1.Store;
  finally
    FreeAndNil(c1);
  end;

  c2 := TContact.Create;
  try
    c2.Name := 'Aaronson';
    c2.Address.City := 'Las Vegas';
    t := TPhone.Create;
    t.Name := 'Home';
    t.Number := '012 12345678';
    c2.AddPhone(t);
    AssertEquals(1, c2.PhoneCount);
    t := TPhone.Create;
    t.Name := 'Office';
    t.Number := '012 23456781';
    c2.AddPhone(t);
    AssertEquals(2, c2.PhoneCount);
    c2.Store;
  finally
    FreeAndNil(c2);
  end;

  LQuery := FConn.CreateQuery;
  try
    LQuery.Command := 'select * from TContact order by Name';
    LQuery.Open;
    AssertEquals(LQuery.ObjectCount, 2);
    AssertEquals((LQuery.Objects[0] as TContact).Name, 'Aaronson');
    LQuery.Close;

    LQuery.Command := 'select * from TContact order by Name desc';
    LQuery.Open;
    AssertEquals(LQuery.ObjectCount, 2);
    AssertEquals((LQuery.Objects[0] as TContact).Name, 'ZZTop');
    LQuery.Close;
  finally
    FreeAndNil(LQuery);
  end;
end;

procedure TTestXMLBroker.TestStoreAndRetrieveContact;
const
  DEF_NAME = 'Mike ''Artù''';
  DEF_NAME_UNICODE = '网站导航';
  DEF_CITY = 'Milan (€)';
var
  c: TContact;
  old_id: string;
  t: TPhone;
begin
  FConn.IsDefault := True;
  c := TContact.Create;
  try
    if FConn.UseUnicode then
      c.Name := DEF_NAME_UNICODE
    else
      c.Name := DEF_NAME;
    c.Address.City := DEF_CITY;
    t := TPhone.Create;
    t.Name := 'Home';
    t.Number := '012 12345678';
    c.AddPhone(t);
    AssertEquals(1, c.PhoneCount);
    t := TPhone.Create;
    t.Name := 'Office';
    t.Number := '012 23456781';
    c.AddPhone(t);
    AssertEquals(2, c.PhoneCount);
    c.Store();
    old_id := c.id;
  finally
    FreeAndNil(c);
  end;
  AssertNull(c);
  c := TContact.Retrieve(old_id);
  try
    AssertNotNull('Object not retrieved', c);
    AssertEquals(old_id, c.Id);
    if FConn.UseUnicode then
      AssertEquals(DEF_NAME_UNICODE, c.Name)
    else
      AssertEquals(DEF_NAME, c.Name);
    AssertEquals(DEF_CITY, c.Address.City);
    AssertNotNull(c.Address);
    AssertEquals(2, c.PhoneCount);
  finally
    FreeAndNil(c);
  end;
end;

initialization
{$IFNDEF CURR_TESTS}
  RegisterTests([TTestXMLBroker]);
{$ENDIF}
end.

