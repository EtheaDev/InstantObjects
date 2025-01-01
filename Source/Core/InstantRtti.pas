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
 * Carlo Barazzetta, Adrea Petrelli, Uberto Barbini, Nando Dessena,
 * Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantRtti;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  System.Classes
  , System.TypInfo
  ;

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

function GetTypeInfo(PropInfo: PPropInfo) : PTypeInfo;
procedure InstantGetEnumNames(TypeInfo: PTypeInfo; Names: TStrings;
  PrefixLen: Integer = 0);
function InstantGetProperty(AObject: TObject; PropPath: string): Variant;
function InstantGetPropInfo(AClass: TClass; PropPath: string;
  PInstance: Pointer = nil): PPropInfo;
procedure InstantSetProperty(AObject: TObject; PropPath: string; Value: Variant);
function InstantIsDefaultPropertyValue(Instance: TObject;
  PropInfo: PPropInfo): Boolean;
function InstantGetPropName(PropInfo: PPropInfo): string; inline;

implementation

uses
  System.Variants
  , System.SysUtils
  , InstantTypes;

function GetTypeInfo(PropInfo: PPropInfo) : PTypeInfo;
begin
  Result := PropInfo^.PropType^;
end;

function AccessProperty(AObject: TObject; PropPath: string;
  Value: Variant): Variant;
var
  PropInfo: PPropInfo;
// DVT Backed out change to fix ME breakage issue
//PreferStrings: Boolean;
begin
  if Assigned(AObject) then
  begin
    if SameText(PropPath, 'Self') then
    begin
      Result := TIORefValueType(AObject);
      Exit;
    end;
    PropInfo := InstantGetPropInfo(AObject.ClassType, PropPath, @AObject);
    if not Assigned(AObject) then
      VarClear(Result)
    else if Assigned(PropInfo) then
    begin
      if not VarIsNull(Value) and Assigned(PropInfo.SetProc) then
      begin
        case GetTypeInfo(PropInfo)^.Kind of
          tkClass:
            SetObjectProp(AObject, PropInfo, TObject(TIORefValueType(Value)));
          tkEnumeration:
            begin
              if VarIsStr(Value) and (VarToStr(Value) = '') then
                Value := 0;
              SetPropValue(AObject, InstantGetPropName(PropInfo), Value);
            end;
          tkSet:
            if VarToStr(Value) = '' then
              SetPropValue(AObject, InstantGetPropName(PropInfo), '[]')
            else
              SetPropValue(AObject, InstantGetPropName(PropInfo), Value);
        else
          SetPropValue(AObject, InstantGetPropName(PropInfo), Value);
        end;
      end;
    // DVT Backed out change to fix ME breakage issue
    //PreferStrings := GetTypeInfo(PropInfo)^.Kind <> tkEnumeration;
    //Result := GetPropValue(AObject, InstantGetPropName(PropInfo), PreferStrings);
      Result := GetPropValue(AObject, InstantGetPropName(PropInfo));
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
        TypeData := GetTypeData(GetTypeInfo(PropInfo));
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
    Result := Value = 0;
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    Result := Value = 0;
  end;

  function IsDefaultStrProp: Boolean;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
    Result := Value = '';
  end;

  function IsDefaultVariantProp: Boolean;
  var
    Value: Variant;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    Result := VarIsEmpty(Value);
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

function InstantGetPropName(PropInfo: PPropInfo): string;
begin
  Result := GetPropName(PropInfo);
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
    tkUString,
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
  Result := InstantGetPropName(PropInfos[Index]);
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
  CurrencyValue : Currency;
begin
  if not Assigned(FInstance) then
  begin
    Result := Unassigned;
    Exit;
  end;
  PropInfo := PropInfos[Index];
  if GetTypeInfo(PropInfo)^.Kind = tkFloat then
  begin
    if GetTypeData(GetTypeInfo(PropInfo)).FloatType = ftCurr then
    begin
      CurrencyValue := GetFloatProp(FInstance, PropInfo);
      Result := CurrencyValue;
    end else
    begin
      Value := GetFloatProp(FInstance, PropInfo);
      if (GetTypeInfo(PropInfo) = TypeInfo(TDateTime))
//        or (PropInfo.PropType^ = TypeInfo(TDate))
//        or (PropInfo.PropType^ = TypeInfo(TTime))
        then
        Result := VarFromDateTime(Value)
      else
        Result := Value;
    end
  end else
    Result := GetPropValue(FInstance, Names[Index]);
end;

end.
