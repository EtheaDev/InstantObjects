{
    $Id: testutils.pas,v 1.1 2005/02/11 22:11:58 decko Exp $
    Copyright (c) 2004 by Dean Zobec

    Port to Delphi of the JUnit framework.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit testutils;

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
type
  PPointer = ^Pointer;
  PMethodRec = ^TMethodRec;
  TMethodRec = packed record
    Size: Word;
    Code: Pointer;
    Name: ShortString;
  end;
var
  MethodTable: PChar;
  vmt: TClass;
  MethodRec: PMethodRec;
  Count: Word;
  idx, i: integer;
begin
  vmt := AClass;
  AList.Clear;
  while vmt <> nil do
  begin
    MethodTable := PChar(Pointer(PChar(vmt) + vmtMethodTable)^);
    if MethodTable <> nil then
    begin
      Move(MethodTable^, Count, 2);
      MethodRec := PMethodRec(MethodTable + 2);
      for i := 0 to Count - 1 do
      begin
        idx := AList.IndexOf(MethodRec^.Name);
        if (idx <> - 1) then
	//found overridden method so delete it
	  aList.Delete(idx);
        AList.AddObject( MethodRec^.Name, TObject(MethodRec^.Code));
        MethodRec := PMethodRec(PChar(MethodRec) + MethodRec^.Size);
      end;
    end;
    vmt := vmt.ClassParent;
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

 
