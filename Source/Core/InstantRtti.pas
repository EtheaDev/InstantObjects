(*
 *   InstantObjects
 *   RTTI Interface
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
 * Carlo Barazzetta, Adrea Petrelli: porting Kylix
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantRtti;

interface

{$I InstantDefines.inc}

{$IFDEF D7+}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

uses
  Classes, TypInfo;

type
  TInstantProperties = class(TObject)
  private
    FInstance: TObject;
    FClass: TClass;
    FPropCount: Integer;
    FPropList: PPropList;
    procedure CreatePropList(TypeInfo: PTypeInfo);
    procedure DestroyPropList;
    function GetCount: Integer;
    function GetPropInfos(Index: Integer): PPropInfo;
    function GetNames(Index: Integer): string;
    function GetTexts(Index: Integer): string;
    function GetTypes(Index: Integer): TTypeKind;
    function GetValues(Index: Integer): Variant;
  public
    constructor Create(AInstance: TObject); overload;
    constructor Create(AClass: TClass; AInstance: TObject = nil); overload;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetNames;
    property PropInfos[Index: Integer]: PPropInfo read GetPropInfos;
    property Texts[Index: Integer]: string read GetTexts;
    property Types[Index: Integer]: TTypeKind read GetTypes;
    property Values[Index: Integer]: Variant read GetValues;
  end;

procedure InstantGetEnumNames(TypeInfo: PTypeInfo; Names: TStrings;
  PrefixLen: Integer = 0);
function InstantGetProperty(AObject: TObject; PropPath: string): Variant;
function InstantGetPropInfo(AClass: TClass; PropPath: string;
  PInstance: Pointer = nil): PPropInfo;
procedure InstantSetProperty(AObject: TObject; PropPath: string; Value: Variant);
function InstantIsDefaultPropertyValue(Instance: TObject;
  PropInfo: PPropInfo): Boolean;

implementation

uses
{$IFDEF MSWINDOWS}
  Controls,
{$ENDIF}
{$IFDEF LINUX}
  InstantClasses, //only for TDate and TTime declaration
  QControls,
{$ENDIF}
  {$IFDEF D6+}Variants,{$ENDIF}SysUtils;

function AccessProperty(AObject: TObject; PropPath: string;
  Value: Variant): Variant;
var
  PropInfo: PPropInfo;
begin
  if Assigned(AObject) then
  begin
    if SameText(PropPath, 'Self') then
    begin
      Result := Integer(AObject);
      Exit;
    end;
    PropInfo := InstantGetPropInfo(AObject.ClassType, PropPath, @AObject);
    if not Assigned(AObject) then
      VarClear(Result)
    else if Assigned(PropInfo) then
    begin
      if (Value <> Null) and Assigned(PropInfo.SetProc) then
      begin
        case PropInfo^.PropType^^.Kind of
          tkClass:
            SetObjectProp(AObject, PropInfo, TObject(Integer(Value)));
          tkEnumeration:
            begin
              {$IFDEF D6+}
              if VarIsStr(Value) and (VarToStr(Value) = '') then
              {$ELSE}
              if Value = '' then
              {$ENDIF}
                Value := 0;
              SetPropValue(AObject, PropInfo^.Name, Value);
            end;
          tkSet:
            if VarToStr(Value) = '' then
              SetPropValue(AObject, PropInfo^.Name, '[]')
            else
              SetPropValue(AObject, PropInfo^.Name, Value);
        else
          SetPropValue(AObject, PropInfo^.Name, Value);
        end;
      end;
      Result := GetPropValue(AObject, PropInfo^.Name);
    end else
      Result := Null;
  end else
    VarClear(Result);
end;

procedure InstantGetEnumNames(TypeInfo: PTypeInfo; Names: TStrings;
  PrefixLen: Integer);
var
  TypeData: PTypeData;
  I: Integer;
  S: string;
begin
  TypeData := GetTypeData(TypeInfo);
  Names.BeginUpdate;
  try
    Names.Clear;
    for I := TypeData^.MinValue to TypeData^.MaxValue do
    begin
      S := GetEnumName(TypeInfo, I);
      Delete(S, 1, PrefixLen);
      Names.Add(S);
    end;
  finally
    Names.EndUpdate;
  end;
end;

function InstantGetProperty(AObject: TObject; PropPath: string): Variant;
begin
  Result := AccessProperty(AObject, PropPath, Null);
end;

function InstantGetPropInfo(AClass: TClass; PropPath: string;
  PInstance: Pointer): PPropInfo;
var
  FirstDot: Integer;
  PropName: string;
  PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
  if Assigned(AClass) then
  begin
    FirstDot := Pos('.', PropPath);
    if FirstDot = 0 then
      Result := GetPropInfo(AClass, PropPath)
    else begin
      PropName := Copy(PropPath, 1, FirstDot - 1);
      System.Delete(PropPath, 1, FirstDot);
      PropInfo := GetPropInfo(AClass, PropName);
      if Assigned(PropInfo) and (PropInfo^.PropType^.Kind = tkClass) then
      begin
        if Assigned(PInstance) and Assigned(TObject(PInstance^)) then
          TObject(PInstance^) := GetObjectProp(TObject(PInstance^), PropInfo);
        TypeData := GetTypeData(PropInfo^.PropType^);
        if Assigned(TypeData) then
          Result := InstantGetPropInfo(TypeData.ClassType, PropPath, PInstance)
        else
          Result := nil;
      end else
        Result := nil;
    end;
  end else
    Result := nil;
end;

procedure InstantSetProperty(AObject: TObject; PropPath: string;
  Value: Variant);
begin
  AccessProperty(AObject, PropPath, Value);
end;

function InstantIsDefaultPropertyValue(Instance: TObject;
  PropInfo: PPropInfo): Boolean;

  function IsDefaultOrdProp: Boolean;
  var
    Value: Longint;
    Default: Longint;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    Default := PropInfo.Default;
    Result := (Default <> Longint($80000000)) and (Value = Default);
  end;

  function IsDefaultFloatProp: Boolean;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    Result := Value = 0;;
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    Result := Value = 0;
  end;

  {$IFDEF VER140}
  function IsDefaultStrProp: Boolean;
  var
    Value: WideString;
  begin
    Value := GetWideStrProp(Instance, PropInfo);
    Result := Value = '';
  end;
  {$ELSE}
  function IsDefaultStrProp: Boolean;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
    Result := Value = '';
  end;
  {$ENDIF}

  function IsDefaultVariantProp: Boolean;
  var
    Value: Variant;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    {$IFDEF VER140}
    Result := VarIsClear(Value);
    {$ELSE}
    Result := VarIsEmpty(Value);
    {$ENDIF}
  end;

begin
  case PropInfo.PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet:
      Result := IsDefaultOrdProp;
    tkFloat:
      Result := IsDefaultFloatProp;
    tkString, tkLString, tkWString:
      Result := IsDefaultStrProp;
    tkVariant:
      Result := IsDefaultVariantProp;
    tkInt64:
      Result := IsDefaultInt64Prop;
  else
    Result := False;
  end;
end;

{ TInstantProperties }

constructor TInstantProperties.Create(AInstance: TObject);
begin
  FInstance := AInstance;
  if Assigned(FInstance) then
    CreatePropList(FInstance.ClassInfo);
end;

constructor TInstantProperties.Create(AClass: TClass; AInstance: TObject);
begin
  FClass := AClass;
  FInstance := AInstance;
  if Assigned(FClass) then
    CreatePropList(FClass.ClassInfo);
end;

procedure TInstantProperties.CreatePropList(TypeInfo: PTypeInfo);
const
  TypeKinds = [tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInt64, tkDynArray];
begin
  DestroyPropList;
  if Assigned(TypeInfo) then
  begin
    FPropCount := GetPropList(TypeInfo, TypeKinds, nil);
    if FPropCount > 0 then
    begin
      GetMem(FPropList, FPropCount * SizeOf(Pointer));
      try
        GetPropList(TypeInfo, TypeKinds, FPropList);
      except
        FreeMem(FPropList, FPropCount * SizeOf(Pointer));
        raise;
      end;
    end;
  end;
end;

destructor TInstantProperties.Destroy;
begin
  inherited;
  DestroyPropList;
end;

procedure TInstantProperties.DestroyPropList;
begin
  if Assigned(FPropList) then
  begin
    FreeMem(FPropList, FPropCount * SizeOf(Pointer));
    FPropList := nil;
    FPropCount := 0;
  end;
end;

function TInstantProperties.GetCount: Integer;
begin
  Result := FPropCount;
end;

function TInstantProperties.GetNames(Index: Integer): string;
begin
  Result := PropInfos[Index]^.Name;
end;

function TInstantProperties.GetPropInfos(Index: Integer): PPropInfo;
begin
  if (Index < 0) or (Index >= FPropCount) then
    raise Exception.CreateFmt('%s: Index out of range', [ClassName]);
  Result := FPropList^[Index];
end;

function TInstantProperties.GetTexts(Index: Integer): string;
var
  Value: Variant;
  Time: TDateTime;
begin
  if not Assigned(FInstance) then
    raise Exception.CreateFmt('%s: Instance unassigned', [ClassName]);
  case Types[Index] of
    tkString, tkLString, tkWString:
      Result := '''' + GetStrProp(FInstance, PropInfos[Index]) + '''';
  else
    Value := Values[Index];
    case VarType(Value) of
      VarDate:
        begin
          Time := VarToDateTime(Value);
          if Time = 0 then
            Result := ''
          else if Time < 1 then
            Result := TimeToStr(Time)
          else
            Result := DateTimeToStr(Time);
        end;
    else
      Result := VarToStr(Value);
    end;
  end;
end;

function TInstantProperties.GetTypes(Index: Integer): TTypeKind;
begin
  Result := PropInfos[Index]^.PropType^.Kind;
end;

function TInstantProperties.GetValues(Index: Integer): Variant;
var
  PropInfo: PPropInfo;
  Value: Double;
begin
  if not Assigned(FInstance) then
  begin
    Result := Unassigned;
    Exit;
  end;
  PropInfo := PropInfos[Index];
  if PropInfo^.PropType^^.Kind = tkFloat then
  begin
    Value := GetFloatProp(FInstance, PropInfo);
    if (PropInfo.PropType^ = TypeInfo(TDateTime))
      or (PropInfo.PropType^ = TypeInfo(TDate))
      or (PropInfo.PropType^ = TypeInfo(TTime)) then
      Result := VarFromDateTime(Value)
    else
      Result := Value;
  end else
    Result := GetPropValue(FInstance, Names[Index]);
end;

end.
