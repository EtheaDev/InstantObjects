{
    $Id: testutils.pas 229 2024-07-24 13:16:29Z cbarazzetta $
    Copyright (c) 2004 by Dean Zobec

    Port to Delphi of the JUnit framework.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit testutils;

{$I '..\InstantDefines.inc'}

interface

uses
  Classes, SysUtils;

type
  {$M+}
  TNoRefCountObject = class(TObject, IInterface)
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end; 
  {$M-}   
  
procedure FreeObjects(List: TList);  
procedure GetMethodList( AObject: TObject; AList: TStrings ); overload;
procedure GetMethodList( AClass: TClass; AList: TStrings ); overload;

implementation

uses
  TestFramework;

function TNoRefCountObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then Result := 0
    else Result := HRESULT($80004002);
end;

function TNoRefCountObject._AddRef: Integer;stdcall;
begin
  Result := -1;
end;

function TNoRefCountObject._Release: Integer;stdcall;
begin
  Result := -1;
end;

// been to the dentist and suffered a lot
// Hack Alert! see objpas.inc
//  Get a list of published methods for a given class or object
procedure GetMethodList( AObject: TObject; AList: TStrings );
begin
  GetMethodList( AObject.ClassType, AList );
end;

procedure GetMethodList(AClass: TClass; AList: TStrings);
var
  MethodEnumerator: TMethodEnumerator;
  MethodName : string;
  MethodCode : Pointer;
  i : integer;
begin
  MethodEnumerator := TMethodEnumerator.create(AClass);
  try
    AList.Clear;
    for i := 0 to MethodEnumerator.MethodCount - 1 do
    begin
      MethodName := MethodEnumerator.NameOfMethod[i];
      MethodCode := AClass.MethodAddress(MethodName);
      AList.AddObject(MethodName, MethodCode);
    end;
  finally
    MethodEnumerator.Free;
  end;
end;

procedure FreeObjects(List: TList);
var
  i: integer;
begin
  for i:= 0 to List.Count - 1 do
    TObject(List.Items[i]).Free;
end;

end.


