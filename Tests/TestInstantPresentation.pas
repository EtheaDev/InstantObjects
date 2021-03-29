(*
 *   InstantObjects Test Suite
 *   TestInstantPresentation
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
 * The Original Code is: InstantObjects Test Suite/TestInstantPresentation
 *
 * The Initial Developer of the Original Code is: Nando Dessena
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestInstantPresentation;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses fpcunit, InstantPresentation, InstantMock, TestModel;

type

  // Test methods for class TInstantExposer
  TestTInstantExposer = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantExposer: TInstantExposer;
    FPerson: TPerson;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFieldGetValue;
    procedure TestFieldSetValue;
    procedure TestEditPost;
  end;

implementation

uses
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, testregistry, InstantPersistence;

{ TestTInstantExposer }

procedure TestTInstantExposer.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FPerson := TPerson.Create(FConn);
  FPerson.Name := 'John Doe';
  FInstantExposer := TInstantExposer.Create(nil);
  FInstantExposer.Subject := FPerson;
end;

procedure TestTInstantExposer.TearDown;
begin
  FreeAndNil(FInstantExposer);
  FreeAndNil(FPerson);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantExposer.TestFieldGetValue;
begin
  AssertEquals('John Doe', FInstantExposer.FieldByName('Name').AsString);
end;

procedure TestTInstantExposer.TestFieldSetValue;
begin
  FInstantExposer.FieldByName('Name').AsString := 'Jane Doe';
  AssertEquals('Jane Doe', FInstantExposer.FieldByName('Name').AsString);
end;

procedure TestTInstantExposer.TestEditPost;
begin
  FInstantExposer.Edit;
  FInstantExposer.FieldByName('Name').AsString := 'Jane Doe';
  AssertEquals('Jane Doe', FInstantExposer.FieldByName('Name').AsString);
  AssertEquals('Jane Doe', FPerson.Name);
  FInstantExposer.Post;
  AssertEquals('Jane Doe', FInstantExposer.FieldByName('Name').AsString);
  AssertEquals('Jane Doe', FPerson.Name);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantExposer]);
{$ENDIF}

end.
