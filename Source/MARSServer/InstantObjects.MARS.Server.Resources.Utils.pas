(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.Resources.Utils
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
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Contributor(s):
 * Carlo Barazzetta, Nicola Tambascia
 *
 * ***** END LICENSE BLOCK ***** *)
unit InstantObjects.MARS.Server.Resources.Utils;

interface

uses
  System.JSON;

type
  TSNJSONObject = class helper for TJSONObject
  private
    function GetS(const AName: string): string;
    function GetD(const AName: string): Double;
    function GetI(const AName: string): Integer;
    function GetO(const AName: string): TJSONObject;
  public
    property S[const AName: string]: string read GetS;
    property I[const AName: string]: Integer read GetI;
    property D[const AName: string]: Double read GetD;
    property O[const AName: string]: TJSONObject read GetO;

    function ValueExists(const AName: string): Boolean;

    // Copies all requested values to properties of ADestination. Returns True if
    // at least one property was changed.
    function CopyValuesToObject(const ADestination: TObject; const AValueNames: TArray<string>): Boolean;
    procedure CopyValuesFromObject(const ASource: TObject; const AValueNames: TArray<string>);
  end;

function GetAppVersion: string;

function DistanceBetweenTwoCoordinates(aLat1, aLon1, aLat2, aLon2: Double): Double;

function ExpandObjectProperties(const AString: string; const AObject: TObject; const ALanguage: string): string;

function IsUserNameWellFormed(const AUserName: string): Boolean;
function IsEmailAddressWellFormed(const AEmailAddress: string): Boolean;
function IsPhoneNumberWellFormed(const APhoneNumber: string): Boolean;
function IsMasterPasswordWellFormed(const AMasterPassword: string): Boolean;
function IsSlavePasswordWellFormed(const ASlavePassword: string): Boolean;
function IsIPWellFormed(const AValue: string): Boolean;
function DateTimeToYYYYMMDD( VarData : Variant ) : string;

implementation

uses
  System.SysUtils
  , System.Variants
  , System.Types
  , System.TypInfo
  , System.Math
  , System.Rtti
  , System.StrUtils
  , System.RegularExpressions
  , System.DateUtils
  , Winapi.Windows
  , Delphi.Mocks.Helpers
  ;

function GetAppVersion: string;
var
  LInfoSize, LValueSize: DWord;
  LInfoPt, LValuePt: Pointer;
begin
  LInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), LValueSize);
  if LInfoSize > 0 then
  begin
    GetMem(LInfoPt, LInfoSize);
    try
      GetFileVersionInfo(PChar(ParamStr(0)), 0, LInfoSize, LInfoPt);
      VerQueryValue(LInfoPt, '\', LValuePt, LValueSize);
      with TVSFixedFileInfo(LValuePt^) do
      begin
        Result := IntToStr(HiWord(dwFileVersionMS)) + '.' +
          IntToStr(LoWord(dwFileVersionMS)) + '.' +
          IntToStr(HiWord(dwFileVersionLS)) + '.' +
          IntToStr(LoWord(dwFileVersionLS));
      end;
    finally
      FreeMem(LInfoPt);
    end;
  end;
end;

function DistanceBetweenTwoCoordinates (aLat1, aLon1, aLat2, aLon2: double): double;
const
  R     = 6371;       // km
  toRad = 0.0174533;  // pi/180
var
  dLat, dLon, temp, apert: double;
begin
  dLat  := (aLat2-aLat1) * toRad;
  dLon  := (aLon2-aLon1) * toRad;
  aLat1 := aLat1 * toRad;
  aLat2 := aLat2 * toRad;

  temp  := (sin(dLat/2) * sin(dLat/2)) + (sin(dLon/2) * sin(dLon/2)) * (cos(aLat1) * cos(aLat2));
  apert := 2 * (arctan2(sqrt(temp), sqrt(1-temp)));
  Result := R * apert;
end;

function ExpandObjectProperties(const AString: string; const AObject: TObject; const ALanguage: string): string;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LProperty: TRttiProperty;
  LValue: TValue;

  function FirstLowercase(const AString: string): string;
  begin
    Result := AString;
    if Result <> '' then
      Result[1] := LowerCase(Result[1])[1];
  end;

  function Replace(const AString, AName, AValue: string): string;
  begin
    Result := AString.Replace('{' + FirstLowercase(AName) + '}', AValue);
  end;

  function GetFormatSettings: TFormatSettings;
  begin
    Result := TFormatSettings.Create(ALanguage);
  end;

begin
  Assert(Assigned(AObject));
  Result := AString;

  if AString <> '' then
  begin
    LContext := TRttiContext.Create;
    try
      LType := LContext.GetType(AObject.ClassInfo);
      for LProperty in LType.GetProperties do
      begin
        LValue := LProperty.GetValue(AObject);
        if SameText(LProperty.PropertyType.Name, 'Boolean') then
          Result := Replace(Result, LProperty.Name, IfThen(LValue.AsType<Boolean>, '1', '0'))
        else if LProperty.PropertyType.TypeKind = tkEnumeration then
          Result := Replace(Result, LProperty.Name, LValue.AsOrdinal.ToString)
        else if LProperty.PropertyType.TypeKind = tkInteger then
          Result := Replace(Result, LProperty.Name, LValue.AsInteger.ToString)
        else if LProperty.PropertyType.TypeKind = tkInt64 then
          Result := Replace(Result, LProperty.Name, LValue.AsInt64.ToString)
        else if MatchText(LProperty.PropertyType.Name, ['TDate', 'TDateTime']) then
        begin
          if LValue.AsType<TDateTime> <> 0 then
            Result := Replace(Result, LProperty.Name, DateToStr(LValue.AsType<TDateTime>, GetFormatSettings))
          else
            Result := Replace(Result, LProperty.Name, '');
        end
        else
          Result := Replace(Result, LProperty.Name, LValue.ToString);
      end;
    finally
      LContext.Free;
    end;
  end;
end;

function IsEmailAddressWellFormed(const AEmailAddress: string): Boolean;
begin
  Result := TRegEx.IsMatch(AEmailAddress, '^[^@]+@[^@]+\.[^@]+$');
end;

function IsUserNameWellFormed(const AUserName: string): Boolean;
begin
  Result := TRegEx.IsMatch(AUserName, '^[a-zA-Z0-9\-_.@£$&_]+$');
end;

function IsPhoneNumberWellFormed(const APhoneNumber: string): Boolean;
begin
  Result := TRegEx.IsMatch(APhoneNumber, '^\+(?:[0-9] ?){6,14}[0-9]$');
end;

function CountChars(const AValue: string; AnyOf: array of Char): Integer;
var
  I: Integer;
  A, C: Char;
  Max: Integer;
begin
  Max := AValue.Length;
  Result := 0;
  I := 0;
  while I <= Max do
  begin
    A := AValue[I];
    for C in AnyOf do
    begin
      if A = C then
        Inc(Result);
    end;
    Inc(I);
  end;
end;

function IsMasterPasswordWellFormed(const AMasterPassword: string): Boolean;
const
  CAPS: array of Char  = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
  LOWERCASE: array of Char  = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];
  NUMBERS: array of Char  = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
//  SYMBOLS: array of Char  = [',', '.', ';', ':', '!', '?', '_', '-', '+', '*', '/'];
var
  LCaps, LLowerCase, LNumbers, LSymbols: integer;
begin
  LCaps := CountChars(AMasterPassword, CAPS);
  LLowerCase := CountChars(AMasterPassword, LOWERCASE);
  LNumbers := CountChars(AMasterPassword, NUMBERS);
  LSymbols := AMasterPassword.Length - LCaps - LLowerCase - LNumbers;
  Result := (AMasterPassword.Length >= 12)
    and (LCaps > 0)
    and (LLowerCase > 0)
    and (LNumbers > 0)
    and (LSymbols > 0)
end;

function IsSlavePasswordWellFormed(const ASlavePassword: string): Boolean;
begin
  // Anything goes, for now, as slave passwords may come from other services.
  Result := True;
end;

function IsIPWellFormed(const AValue: string): Boolean;
var
  LIPBytes: TArray<string>;

  function CheckIPByte(const AIpByte: string): Boolean;
  var
    LIPByte: Integer;
  begin
    LIPByte := StrToIntDef(AIpByte, -1);
    Result := (LIPByte >= 0) and (LIPByte <= 255);
  end;

begin
  LIPBytes := TArray<string>(SplitString(AValue, '.'));
  Result := (Length(LIPBytes) = 4)
    and CheckIPByte(LIPBytes[0])
    and CheckIPByte(LIPBytes[1])
    and CheckIPByte(LIPBytes[2])
    and CheckIPByte(LIPBytes[3]);
end;

function DateTimeToYYYYMMDD( VarData : Variant ) : string;
begin
  if VarToStr(VarData)='' then
    Result := StringOfChar(' ',8)
  else
    Result := FormatDateTime( 'YYYYMMDD', VarToDateTime(VarData) );
end;

var
  _JSONFormatSettings: TFormatSettings;

{ TSNJSONObject }

procedure TSNJSONObject.CopyValuesFromObject(const ASource: TObject;
  const AValueNames: TArray<string>);
var
  LValueName: string;
  LContext: TRttiContext;
  LType: TRttiType;
  LProperty: TRttiProperty;
  LValue: TValue;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(ASource.ClassInfo);
    for LValueName in AValueNames do
    begin
      LProperty := LType.GetProperty(LValueName);
      if Assigned(LProperty) then
      begin
        LValue := LProperty.GetValue(ASource);
        if SameText(LProperty.PropertyType.Name, 'Boolean') then
          AddPair(LValueName, TJSONNUmber.Create(IfThen(LValue.AsType<Boolean>, 1, 0)))
        else if LProperty.PropertyType.TypeKind = tkEnumeration then
          AddPair(LValueName, TJSONNumber.Create(LValue.AsOrdinal))
        else if LProperty.PropertyType.TypeKind = tkInteger then
          AddPair(LValueName, TJSONNumber.Create(LValue.AsInteger))
        else if LProperty.PropertyType.TypeKind = tkInt64 then
          AddPair(LValueName, TJSONNumber.Create(LValue.AsInt64))
        else if MatchText(LProperty.PropertyType.Name, ['TDate', 'TDateTime']) then
        begin
          if LValue.AsType<TDateTime> <> 0 then
            AddPair(LValueName, DateToISO8601(LValue.AsType<TDateTime>))
        end
        else
          AddPair(LValueName, LValue.AsString);
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function TSNJSONObject.CopyValuesToObject(const ADestination: TObject;
  const AValueNames: TArray<string>): Boolean;
var
  LValueName, LValueStr: string;
  LContext: TRttiContext;
  LType: TRttiType;
  LProperty: TRttiProperty;
  LValue: TValue;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(ADestination.ClassInfo);
    Result := False;
    for LValueName in AValueNames do
    begin
      if FindValue(LValueName) <> nil then
      begin
        LProperty := LType.GetProperty(LValueName);
        LValueStr := S[LValueName];
        if SameText(LProperty.PropertyType.Name, 'Boolean') then
          LValue := LValueStr = '1'
        else if LProperty.PropertyType.TypeKind = tkEnumeration then
          LValue := StrToInt(LValueStr)
        else if LProperty.PropertyType.TypeKind = tkInteger then
          LValue := StrToInt(LValueStr)
        else if LProperty.PropertyType.TypeKind = tkInt64 then
          LValue := StrToInt64(LValueStr)
        else if MatchText(LProperty.PropertyType.Name, ['TDate', 'TDateTime']) then
        begin
          if LValueStr <> '' then
            LValue := ISO8601ToDate(LValueStr)
          else
            Exit;
        end
        else
          LValue := LValueStr;
        if not LProperty.GetValue(ADestination).Equals(LValue) then
        begin
          // Enums have different sizes (byte, word, int), so we must make a proper
          // TValue out of the integer value before calling SetValue.
          if (LProperty.PropertyType.TypeKind = tkEnumeration) and
            not (SameText(LProperty.PropertyType.Name, 'Boolean')) then
            TValue.Make(LValue.AsInteger, LProperty.PropertyType.Handle, LValue);
          LProperty.SetValue(ADestination, LValue);
          Result := True;
        end;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function TSNJSONObject.GetD(const AName: string): Double;
begin
  Result := StrToFloatDef(S[AName], 0, _JSONFormatSettings);
end;

function TSNJSONObject.GetI(const AName: string): Integer;
begin
  Result := StrToIntDef(S[AName], 0);
end;

function TSNJSONObject.GetO(const AName: string): TJSONObject;
begin
  if not TryGetValue<TJSONObject>(AName, Result) then
    Result := nil;
end;

function TSNJSONObject.GetS(const AName: string): string;
begin
  if not TryGetValue<string>(AName, Result) then
    Result := '';
end;

function TSNJSONObject.ValueExists(const AName: string): Boolean;
begin
  Result := Assigned(FindValue(AName));
end;

initialization
  _JSONFormatSettings := TFormatSettings.Create;
  _JSONFormatSettings.DecimalSeparator := '.';

end.
