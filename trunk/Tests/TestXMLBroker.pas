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
  end;

implementation

uses
  SysUtils, InstantPersistence, TestModel;

{ TTestXMLBroker }

procedure TTestXMLBroker.SetUp;
begin
  FAcc := TXMLFilesAccessor.Create(nil);
  FAcc.RootFolder := ExtractFilePath(ParamStr(0));
  FConn := TInstantXMLConnector.Create(nil);
  FConn.Connection := FAcc;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
end;

procedure TTestXMLBroker.TearDown;
begin
  inherited;
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
  FreeAndNil(FAcc);
end;

procedure TTestXMLBroker.TestStoreAndRetrieveContact;
const
  DEF_NAME = 'Mike Artù';
  DEF_CITY = 'Milan (€)';
var
  c: TContact;
  old_id: string;
  t: TPhone;
begin
  FConn.IsDefault := True;
  c := TContact.Create;
  try
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
    AssertEquals(c.Name, DEF_NAME);
    AssertEquals(c.Address.City, DEF_CITY);
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

