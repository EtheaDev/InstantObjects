(*
 *   InstantObjects
 *   TestContactDb
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
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Uberto Barbini
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestContactDb;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  InstantUIBConnection, InstantUIB;

type

  { TestMinimalModelUIB }

  TTestContactModelUIB=class(TTestCase)
  private
    _ApplicationPath : string;
    _Connection: TInstantUIBConnection;
    _Connector: TInstantUIBConnector;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAll;
    procedure TestConnected;
    procedure TestStoring;
  end; 

implementation

uses
  Model;

procedure TTestContactModelUIB.SetUp;
begin
  InstantModel.LoadFromFile(ChangeFileExt(ParamStr(0),'.mdx'));
  //Connect to database
  _Connection := nil;
  _Connector := nil;
  _Connection := TInstantUIBConnection.Create(nil);
  _Connection.Database.DatabaseName :=  ChangeFileExt(ParamStr(0),'.FDB');
  _Connection.Database.UserName := 'SYSDBA';
  _Connection.Database.Password := 'a';
  _Connector := TInstantUIBConnector.Create(nil);
  _Connector.Connection := _Connection;
  _Connector.LoginPrompt := False;
  _Connector.IsDefault := True;
  _Connector.Connect;
end;

procedure TTestContactModelUIB.TearDown;
begin
  if assigned(_Connector) and _Connector.Connected then
    _Connector.Disconnect;
  FreeAndNil(_Connector); //libera anche il connector
//  FreeAndNil(_Connection); //libera anche il connector
end;

procedure TTestContactModelUIB.TestAll;
var
  c: TContact;
  t: TPhone;
  SimpleClass : TSimpleClass;
  Id : string;
  i : integer;
  s: string;
begin
  for i := 0 to 10 do
  begin
    c := TContact.Create;
    try
      c.Name := 'Mike';
      c.Address.City := 'Milan';
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
      AssertEquals(old_id, c.Id);
      AssertNotNull(c.Address);
      AssertEquals(2, c.PhoneCount);
    finally
      FreeAndNil(c);
    end;

  
// Storing Object.
    SimpleClass := TSimpleClass.Create;
    Try
      AssertEquals('', SimpleClass.Id);
      SimpleClass.StringProperty := IntToStr(Random(MaxInt));
      s := SimpleClass.StringProperty;
      SimpleClass.Store;
      Id := SimpleClass.Id;
    Finally
      SimpleClass.Free;
    End;

// Retrieving and changing Object.
    SimpleClass := TSimpleClass.Retrieve(Id);
    Try
      AssertEquals(s, SimpleClass.StringProperty);
      SimpleClass.StringProperty := IntToStr(Random(MaxInt));
      s := SimpleClass.StringProperty;
      SimpleClass.Store;
    Finally
      SimpleClass.Free;
    End;

// Retrieving and deleting Object.
    SimpleClass := TSimpleClass.Retrieve(Id);
    Try
      AssertEquals(s, SimpleClass.StringProperty);
      SimpleClass.Dispose;
    Finally
      SimpleClass.Free;
    End;


// Trying to retrive deleted object
    SimpleClass := nil; 
    SimpleClass := TSimpleClass.Retrieve(Id);
    AssertNull(SimpleClass);

  end;
end;

procedure TTestContactModelUIB.TestConnected;
begin
  AssertTrue(_Connector.Connected);
  _Connector.Disconnect;
  AssertFalse(_Connector.Connected);
end;

procedure TTestContactModelUIB.TestStoring;
var
  Id: string;
  i: integer;
  SimpleClass: TSimpleClass;
begin
  Id := '';
  for i := 0 to 10 do
  begin
    SimpleClass := TSimpleClass.Create;
    try
      SimpleClass.StringProperty := IntToStr(Random(MaxInt));
      SimpleClass.Store;
      AssertFalse(Id = SimpleClass.Id); //different from previous one
      Id := SimpleClass.Id;
    finally
      SimpleClass.Free;
    end;
  end;
end;

initialization

  RegisterTest(TTestContactModelUIB);
end.

