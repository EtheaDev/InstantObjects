(*
 *   InstantObjects Test Suite
 *   TestInstantComplex
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
 * The Original Code is: InstantObjects Test Suite/TestInstantComplex
 *
 * The Initial Developer of the Original Code is: Steven Mitchell
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * 
 *
 * ***** END LICENSE BLOCK ***** *)

unit TestInstantComplex;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

interface

uses fpcunit, InstantPersistence, InstantMock, TestModel;

type

  // Test methods for class TInstantComplex
  TestTInstantComplex = class(TTestCase)
  private
    FConn: TInstantMockConnector;
    FInstantComplex: TInstantPart;
    FOwner: TContact;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAllowOwned;
    procedure TestAttachObject;
    procedure TestConnector;
    procedure TestDetachObject;
    procedure TestRequiredClass;
    procedure TestRequiredClassName;
  end;

implementation

uses
  {$IFDEF D17+}
  System.Classes,
  {$ENDIF}
  SysUtils, testregistry;

procedure TestTInstantComplex.SetUp;
begin
  FConn := TInstantMockConnector.Create(nil);
  FConn.BrokerClass := TInstantMockBroker;

  if InstantModel.ClassMetadatas.Count > 0 then
    InstantModel.ClassMetadatas.Clear;
  InstantModel.LoadFromResFile(ChangeFileExt(ParamStr(0), '.mdr'));

  FOwner := TContact.Create(FConn);
  FInstantComplex := FOwner._Address;
end;

procedure TestTInstantComplex.TearDown;
begin
  FInstantComplex := nil;
  FreeAndNil(FOwner);
  InstantModel.ClassMetadatas.Clear;
  FreeAndNil(FConn);
end;

procedure TestTInstantComplex.TestAllowOwned;
begin
  AssertFalse(FInstantComplex.AllowOwned);
end;

procedure TestTInstantComplex.TestAttachObject;
begin
  AssertFalse(FInstantComplex.AttachObject(nil));
end;

procedure TestTInstantComplex.TestConnector;
begin
  AssertNotNull(FInstantComplex.Connector);
end;

procedure TestTInstantComplex.TestDetachObject;
begin
  AssertFalse(FInstantComplex.DetachObject(nil));
end;

procedure TestTInstantComplex.TestRequiredClass;
begin
  AssertEquals(TAddress, FInstantComplex.RequiredClass);
end;

procedure TestTInstantComplex.TestRequiredClassName;
begin
  AssertEquals('TAddress', FInstantComplex.RequiredClassName);
end;

initialization
  // Register any test cases with the test runner
{$IFNDEF CURR_TESTS}
  RegisterTests([TestTInstantComplex]);
{$ENDIF}

end.
 