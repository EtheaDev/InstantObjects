(*
 *   InstantObjects
 *   Object Foundry Expert
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
 * The Original Code is: Seleqt InstantObjects/Object Foundry Expert
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit OFUtils;

{$I ObjectFoundry.inc}

interface

uses
  MMToolsAPI, MMIOAPI, MMEngineDefs, OFDefs, InstantCode;

function ClassAsV9ClassBase(AClass: IMMClassBase): IMMV9ClassBase;
function CodeModelAsV9CodeModel(ACodeModel: IMMCodeModel): IMMV9CodeModel;
function GetRootClass: IMMClassBase;
function IsPersistentClass(AClass: IMMClassBase): Boolean;
function MemberAsAttribute(Member: IMMMember): IMMIOAttribute;
function MemberAsProperty(Member: IMMMember): IMMProperty;
function MemberAsField(Member: IMMMember): IMMField;
function MemberAsMethod(Member: IMMMember): IMMMethod;
function AttributeAsProperty(Attribute: IMMIOAttribute): IMMProperty;
function MemberAsAttributeProperty(Member: IMMMember): IMMProperty;
function SameCode(const Code1, Code2: string): Boolean;

function MMVisibilityToInstantCodeVisibility(const Value: TVisibility):
    TInstantCodeVisibility;

function InstantCodeVisibilityToMMVisibility(const Value:
    TInstantCodeVisibility): TVisibility;

implementation

uses
  Windows, SysUtils;

function ClassAsV9ClassBase(AClass: IMMClassBase): IMMV9ClassBase;
begin
  if not Assigned(AClass) or not AClass.Valid or
    (AClass.QueryInterface(IMMV9ClassBase, Result) <> S_OK) then
    Result := nil;
end;

function CodeModelAsV9CodeModel(ACodeModel: IMMCodeModel): IMMV9CodeModel;
begin
  if ACodeModel.QueryInterface(IMMV9CodeModel, Result) <> S_OK then
    Result := nil;
end;

function GetRootClass: IMMClassBase;
var
  CodeModel: IMMV9CodeModel;
begin
  CodeModel := CodeModelAsV9CodeModel(MMToolServices.CodeModel);
  if Assigned(CodeModel) then
    Result := CodeModel.ClassPersistencyRoot
  else
    Result := nil;
end;

function IsPersistentClass(AClass: IMMClassBase): Boolean;
var
  Root: IMMClassBase;
begin
  Root := GetRootClass;
  Result := Assigned(Root) and Assigned(AClass) and AClass.IsClass(Root);
end;

function MemberAsAttribute(Member: IMMMember): IMMIOAttribute;
begin
  if not Assigned(Member) or not Member.Valid or
    (Member.QueryInterface(IMMIOAttribute, Result) <> S_OK) then
    Result := nil;
end;

function MemberAsProperty(Member: IMMMember): IMMProperty;
begin
  if not Assigned(Member) or not Member.Valid or
    (Member.QueryInterface(IMMProperty, Result) <> S_OK) then
    Result := nil;
end;

function MemberAsField(Member: IMMMember): IMMField;
begin
  if not Assigned(Member) or not Member.Valid or
    (Member.QueryInterface(IMMField, Result) <> S_OK) then
    Result := nil;
end;

function MemberAsMethod(Member: IMMMember): IMMMethod;
begin
  if not Assigned(Member) or not Member.Valid or
    (Member.QueryInterface(IMMMember, Result) <> S_OK) then
    Result := nil;
end;

function AttributeAsProperty(Attribute: IMMIOAttribute): IMMProperty;
begin
  if not Assigned(Attribute) or
    (Attribute.QueryInterface(IMMProperty, Result) <> S_OK) then
    Result := nil;
end;

function MemberAsAttributeProperty(Member: IMMMember): IMMProperty;
var
  Attribute: IMMIOAttribute;
begin
  Attribute := MemberAsAttribute(Member);
  if Assigned(Attribute) then
    Result := AttributeAsProperty(Attribute)
  else
    Result := nil;
end;

function SameCode(const Code1, Code2: string): Boolean;
begin
  Result := SameText(Trim(Code1), Trim(Code2));
end;

function MMVisibilityToInstantCodeVisibility(const Value: TVisibility):
    TInstantCodeVisibility;
const
  Map: array[TVisibility] of TInstantCodeVisibility =
    (viDefault, viPrivate, viPrivate, viProtected, viProtected, 
     viPublic, viPublished, viPublished);
begin
  Result := Map[Value];
end;

function InstantCodeVisibilityToMMVisibility(const Value:
    TInstantCodeVisibility): TVisibility;
const
  Map: array[TInstantCodeVisibility] of TVisibility =
    (scDefault, scPrivate, scProtected, scPublic, scPublished);
begin
  Result := Map[Value];
end;

end.
