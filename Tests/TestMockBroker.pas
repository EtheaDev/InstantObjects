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
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestMockBroker;

interface

uses
  Classes, SysUtils, InstantPersistence, {$IFNDEF DUNITX_TESTS}testregistry, fpcunit,{$ELSE}InstantTest,{$ENDIF} InstantMock,
  TestModel,
  DUnitX.TestFramework;

type
  [TestFixture]
  TTestMockBroker = class(TInstantTestCase)
  protected
    FConn: TInstantMockConnector;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    [Test]
    procedure TestModelFromToFile;
    {$IFDEF DELPHI_NEON}
    procedure TestModelFromToFileJSON;
    {$ENDIF}
    procedure TestGetBroker;
    procedure TestBuildDatabase;
    procedure TestModelFromToResFile;
    procedure TestStoreAndRetrieveProject;
  end;

  [TestFixture]
  TTestMockRelationalBroker = class(TInstantTestCase)
  protected
    FConn: TInstantMockConnector;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    procedure TestGetBroker;
    procedure TestParts;
    procedure TestStoreAndRetrieveContact;
  end;

  [TestFixture]
  TTestMockSQLbroker = class(TInstantTestCase)
  protected
    FConn: TInstantMockConnector;
  public
    [Setup]
    procedure SetUp; override;
    [TearDown]
    procedure TearDown; override;
  published
    procedure TestQuery;
  end;

implementation

uses InstantMetadata, InstantClasses;

procedure TTestMockBroker.TestModelFromToFile;
var
  vReturnValue: TInstantClassMetadata;
begin
  AssertTrue('ClassMetadatas.Count', InstantModel.ClassMetadatas.Count > 0);

  // This ensures that the exported file is synchronised
  // with the current model resource file.
  InstantModel.SaveToFile(ChangeFileExt(ParamStr(0), '.mdx'), sfXML);
  InstantModel.ClassMetadatas.Clear;

  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0), '.mdx'), sfXML);
  vReturnValue := InstantModel.ClassMetadatas.Find('TCategory');
  AssertNotNull(vReturnValue);

  InstantModel.ClassMetadatas.Remove(vReturnValue);
  AssertNull('TCategory was found!',
    InstantModel.ClassMetadatas.Find('TCategory'));
  InstantModel.SaveToFile(ChangeFileExt(ParamStr(0), '.mdxt'), sfXML);

  InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0), '.mdxt'), sfXML);
  AssertNotNull(InstantModel.ClassMetadatas.Find('TContact'));
  AssertNull(InstantModel.ClassMetadatas.Find('TCategory'));
end;

{$IFDEF DELPHI_NEON}
procedure TTestMockBroker.TestModelFromToFileJSON;
var
  vReturnValue: TInstantClassMetadata;
begin
  AssertTrue('ClassMetadatas.Count', InstantModel.ClassMetadatas.Count > 0);

  // This ensures that the exported file is synchronised
  // with the current model resource file.
  InstantModel.SaveToFile(ChangeFileExt(ParamStr(0), '.mdj'), sfJSON);
  InstantModel.ClassMetadatas.Clear;

  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0), '.mdj'), sfJSON);
  vReturnValue := InstantModel.ClassMetadatas.Find('TCategory');
  AssertNotNull(vReturnValue);

  InstantModel.ClassMetadatas.Remove(vReturnValue);
  AssertNull('TCategory was found!',
    InstantModel.ClassMetadatas.Find('TCategory'));
  InstantModel.SaveToFile(ChangeFileExt(ParamStr(0), '.mdjt'), sfJSON);

  InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0), '.mdjt'), sfJSON);
  AssertNotNull(InstantModel.ClassMetadatas.Find('TContact'));
  AssertNull(InstantModel.ClassMetadatas.Find('TCategory'));
end;
{$ENDIF}

procedure TTestMockBroker.TestModelFromToResFile;
var
  vReturnValue: TInstantClassMetadata;
begin
  vReturnValue := InstantModel.ClassMetadatas.Find('TCategory');
  AssertNotNull(vReturnValue);

  InstantModel.ClassMetadatas.Remove(vReturnValue);
  AssertNull(InstantModel.ClassMetadatas.Find('TCategory'));

  InstantModel.SaveToResFile(ChangeFileExt(ParamStr(0), '.mdrt'));
  InstantModel.ClassMetadatas.Clear;

  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdrt'));
  AssertNotNull(InstantModel.ClassMetadatas.Find('TContact'));
  AssertNull(InstantModel.ClassMetadatas.Find('TCategory'));
end;

procedure TTestMockBroker.TestGetBroker;
var
  brok: TInstantMockBroker;
begin
  brok := (FConn.Broker as TInstantMockBroker);
  AssertNotNull(brok);
  AssertEquals(brok.ClassType, TInstantMockBroker);
  brok.MockManager.StartSetUp;
  brok.MockManager.EndSetUp;
  FConn.BuildDatabase(InstantModel);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.TestBuildDatabase;
var
  brok: TInstantMockBroker;
begin
  brok := FConn.Broker as TInstantMockBroker;
  brok.MockManager.StartSetUp;
  brok.MockManager.EndSetUp;
  FConn.BuildDatabase(InstantModel);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.TestStoreAndRetrieveProject;
var
  a: TProject;
  old_id: string;
  brok: TInstantMockBroker;
begin
  //FConn.IsDefault := True;
  FConn.StartTransaction;
  brok := FConn.Broker as TInstantMockBroker;
  brok.MockManager.StartSetUp;
  a := TProject.Create(FConn);
  try
    a.Name := 'Bongo';
    a.Store;
    old_id := a.id;
  finally
    FreeAndNil(a);
  end;
  AssertNull(a);
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalStoreObject ' + old_id);
  brok.MockManager.Verify;
  FConn.CommitTransaction;
  brok.MockManager.StartSetUp;
  a := TProject.Retrieve(old_id, False, False, FConn);
  try
    AssertEquals(old_id, a.Id);
  finally
    FreeAndNil(a);
  end;
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalRetrieveObject ' + old_id);
  brok.MockManager.Verify;
end;

procedure TTestMockBroker.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
end;

procedure TTestMockBroker.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

{ TTestMockRelationalBroker }

procedure TTestMockRelationalBroker.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockCRBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
end;

procedure TTestMockRelationalBroker.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TTestMockRelationalBroker.TestGetBroker;
var
  brok: TInstantMockCRBroker;
begin
  brok := (FConn.Broker as TInstantMockCRBroker);
  AssertNotNull(brok);
  AssertEquals(brok.ClassType, TInstantMockCRBroker);
  brok.MockManager.StartSetUp;
  brok.MockManager.EndSetUp;
  Fconn.BuildDatabase(InstantModel);
  brok.MockManager.Verify;
end;

procedure TTestMockRelationalBroker.TestParts;
var
  c: TContact;
  t: TPhone;
begin
  //FConn.IsDefault := True;
  c := TContact.Create(FConn);
  try
    AssertNotNull(c._Phones);
    AssertEquals(0, c.PhoneCount);
    t := TPhone.Create(FConn);
    t.Name := 'Home';
    t.Number := '012 12345678';
    c.AddPhone(t);
    AssertEquals(1, c.PhoneCount);
  finally
    c.Free;
  end;
end;

procedure TTestMockRelationalBroker.TestStoreAndRetrieveContact;
var
  c: TContact;
  old_id: string;
  brok: TInstantMockCRBroker;
  t: TPhone;
begin
  //FConn.IsDefault := True;
  brok := FConn.Broker as TInstantMockCRBroker;
  brok.MockManager.StartSetUp;
  c := TContact.Create(FConn);
  try
    c.Name := 'Mike';
    c.Address.City := 'Milan';
    t := TPhone.Create(FConn);
    t.Name := 'Home';
    t.Number := '012 12345678';
    c.AddPhone(t);
    AssertEquals(1, c.PhoneCount);
    t := TPhone.Create(FConn);
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
  brok.MockManager.EndSetUp;
  brok.MockManager.AddExpectation('InternalStoreObject caFail ' + old_id);
  brok.MockManager.Verify;
  brok.MockManager.StartSetUp;
  brok.MockManager.AddExpectation('InternalRetrieveObject caFail ' + old_id);
  brok.MockManager.EndSetUp;
  c := TContact.Retrieve(old_id, False, False, FConn);
  try
    AssertEquals(old_id, c.Id);
    AssertNotNull(c.Address);
    AssertEquals(0, c.PhoneCount); //mock brocker cannot collect part and parts
  finally
    FreeAndNil(c);
  end;
  brok.MockManager.Verify;
end;

{ TTestMockSQLbroker }

procedure TTestMockSQLbroker.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockSQLBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));
end;

procedure TTestMockSQLbroker.TearDown;
begin
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TTestMockSQLbroker.TestQuery;
var
  LQuery: TInstantQuery;
begin
  LQuery := FConn.CreateQuery;
  try
    LQuery.Command := 'select * from tcountry';
    AssertTrue(LQuery.ObjectClass = TCountry);
    AssertTrue(LQuery.ObjectClassName = 'TCountry');
  finally
    LQuery.Free;
  end;
end;

initialization
{$IFNDEF DUNITX_TESTS}
  RegisterTests([TTestMockBroker, TTestMockRelationalBroker, TTestMockSQLbroker]);
{$ENDIF}
end.
