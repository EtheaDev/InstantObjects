(*
 *   InstantObjects
 *   Utilities
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
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena, Uberto Barbini,
 * Brian Andersen, David Taylor
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantUtils;

{$I '..\InstantDefines.inc'}

interface

uses
  Classes, InstantClasses, SysUtils;

type
  TInstantCompareOption = (coCaseInsensitive, coPartial);
  TInstantCompareOptions = set of TInstantCompareOption;

  TInstantVersion = record
    Major, Minor, Release, Build: Word;
  end;

function InstantAllocMem(Size: Cardinal): Pointer;
procedure InstantFreeMem(P: Pointer);
function InstantCompareObjects(Obj1, Obj2: TObject; PropName: string;
  Options: TInstantCompareOptions): Integer; overload;
function InstantCompareObjects(Obj1, Obj2: TObject; PropNames: TStrings;
  Options: TInstantCompareOptions): Integer; overload;
function InstantCompareValues(V1, V2: Variant;
  Options: TInstantCompareOptions): Integer;
function InstantCompareText(const S1, S2: string; IgnoreCase: Boolean): Integer;
function InstantConstArrayToVariant(AValues : array of const) : Variant;
function InstantDateTimeToStr(DateTime: TDateTime): string;
function InstantDateToStr(DateTime: TDateTime): string;
function InstantTimeToStr(ADate: TDateTime): string;
function InstantDateTimeToJStr(ADate: TDateTime): string;
function InstantEmbrace(const S, Delimiters: string): string;
function InstantFileAge(const FileName: string; out FileDateTime: TDateTime): boolean;
function InstantFileVersionValue(const FileName, ValueName: string): string;
function InstantFileVersion(const FileName: string): TInstantVersion;
function InstantFileVersionStr(const FileName: string): string;
function InstantGenerateId: string;
function InstantIsIdentifier(Str: string): Boolean;
function InstantIsNumeric(const Str: string): Boolean;
function InstantIsQuoted(const Str: string; Quote: Char): Boolean;
function InstantMatchChars(C1, C2: Char; IgnoreCase: Boolean): Boolean;
function InstantMatchObject(Obj: TObject; PropName: string;
  const KeyValue: Variant; Options: TInstantCompareOptions): Boolean; overload;
function InstantMatchObject(Obj: TObject; PropNames: TStrings;
  const KeyValues: Variant; Options: TInstantCompareOptions): Boolean; overload;
function InstantModuleFileName: string;
function InstantPartStr(Str: string; Index: Integer; Delimiter: Char): string;
function InstantQuote(const S: string; Quote: Char): string;
function InstantRightPos(const SubStr, Str: string; IgnoreCase: Boolean = False): Integer;
function InstantSameText(const S1, S2: string; IgnoreCase: Boolean): Boolean;
function InstantStrToDate(const Str: string): TDateTime;
function InstantStrToDateTime(const Str: string): TDateTime;
procedure InstantStrToList(const Str: string; List: TStrings;
  Delimiters: TChars);
function InstantStrToTime(const Str: string): TDateTime;
function InstantUnquote(const Str: string; Quote: Char): string;
function InstantStrArrayToString(const StrArray: array of string; Delimiter: Char): string;

function DateOf(const AValue: TDateTime): TDateTime;
function TimeOf(const AValue: TDateTime): TDateTime;

function InstantCharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function InstantCharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
function InstantSmartConcat(const AArray: array of string; const ADelimiter: string = ' - '): string;

implementation

uses
  Windows, ActiveX, ComObj,
  Variants, InstantConsts, InstantRtti,
  DateUtils;


{$IFDEF IO_MEM_OVERRUN_CHECK}
const
  IO_MEM_SIGNATURE = Longint($BCFEEFCB);

type
  PInstantAllocGuard = ^TInstantAllocGuard;
  TInstantAllocGuard = packed array[0..3] of Longint;

  PInstantAllocHeader = ^TInstantAllocHeader;
  TInstantAllocHeader = packed record
    Signature : Longint;
    GuardPtr  : PInstantAllocGuard;
    UserData  : record end;
  end;

function InstantAllocMem(Size: Cardinal): Pointer;
var
  Header: PInstantAllocHeader;
  GuardPtr: PAnsiChar;
  I: integer;
begin
  if (Size > 0) then
  begin
    // Pad the allocation block with a header and guard area
    Header := AllocMem(sizeof(TInstantAllocHeader) + Size + sizeof(TInstantAllocGuard));
    Result := @Header.UserData;

    // Initialize the block header signature and guard pointer
    Header.Signature := IO_MEM_SIGNATURE;
    GuardPtr := PAnsiChar(@Header.UserData);
    inc(GuardPtr,Size);
    Header.GuardPtr := PInstantAllocGuard(GuardPtr);

    // Initialize the guard area with a known signature
    for I := low(Header.GuardPtr^) to high(Header.GuardPtr^) do
      Header.GuardPtr^[I] := IO_MEM_SIGNATURE;
  end else
  begin
    Result := nil;
  end;
end;

procedure InstantFreeMem(P: Pointer);
var
  BlockPtr: PAnsiChar;
  Header: PInstantAllocHeader;
  I: integer;
begin
  if Assigned(P) then
  begin
    BlockPtr := PAnsiChar(P);
    dec(BlockPtr, sizeof(TInstantAllocHeader));
    Header := PInstantAllocHeader(BlockPtr);

    // Ensure the header signature is intact
    if (Header.Signature <> IO_MEM_SIGNATURE) then
      raise EInvalidPointer.Create('InstantFreeMem - header signature is invalid');

    // Ensure the guard pointer is not null
    if (Header.GuardPtr = nil) then
      raise EInvalidPointer.Create('InstantFreeMem - header guard pointer is invalid');

    // Ensure the block guard area has not been modified
    for I := low(Header.GuardPtr^) to high(Header.GuardPtr^) do
      if (Header.GuardPtr^[I] <> IO_MEM_SIGNATURE) then
        raise EInvalidPointer.Create('InstantFreeMem - memory overrun corruption detected');

    FreeMem(BlockPtr);
  end;
end;
{$ELSE}  // IO_MEM_OVERRUN_CHECK disabled

function InstantAllocMem(Size: Cardinal): Pointer;
begin
  Result := AllocMem(Size);
end;

procedure InstantFreeMem(P: Pointer);
begin
  FreeMem(P);
end;
{$ENDIF}

function InstantCompareObjects(Obj1, Obj2: TObject; PropName: string;
  Options: TInstantCompareOptions): Integer;
var
  V1, V2: Variant;
begin
  V1 := InstantGetProperty(Obj1, PropName);
  V2 := InstantGetProperty(Obj2, PropName);
  Result := InstantCompareValues(V1, V2, Options);
end;

function InstantCompareObjects(Obj1, Obj2: TObject; PropNames: TStrings;
  Options: TInstantCompareOptions): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Assigned(PropNames) then
    for I := 0 to Pred(PropNames.Count) do
    begin
      Result := InstantCompareObjects(Obj1, Obj2, PropNames[I], Options);
      if Result <> 0 then
        Break;
    end;
end;

function InstantCompareValues(V1, V2: Variant;
  Options: TInstantCompareOptions): Integer;

  function IsStrVar(V: Variant): Boolean;
  begin
    Result := (VarType(V) = varOleStr) or (VarType(V) = varString);
  end;

  function IsBlankVar(V: Variant): Boolean;
  begin
    Result := VarType(V) in [VarEmpty, varNull];
  end;

  function CompareStrings(S1, S2: Variant): Integer;
  var
    S: string;
  begin
    if IsBlankVar(S1) then
      S1 := '';
    if IsBlankVar(S2) then
      S2 := '';
    if coPartial in Options then
      S := Copy(S1, 1,  Length(VarToStr(S2)))
    else
      S := S1;
    if coCaseInsensitive in Options then
      Result := AnsiCompareText(S, S2)
    else
      Result := AnsiCompareStr(S, S2);
  end;

  function CompareNumbers(N1, N2: Variant): Integer;
  begin
    if IsBlankVar(N1) then
      N1 := 0;
    if IsBlankVar(N2) then
      N2 := 0;
    if N1 > N2 then
      Result := 1
    else if N1 < N2 then
      Result := -1
    else
      Result := 0;
  end;

  function CompareValues(V1, V2: Variant): Integer;
  begin
    case VarType(V1) of
      varInteger, varByte,
      varSingle, varDouble, varCurrency,
      varDate:
        Result := CompareNumbers(V1, V2);
      varString, varOleStr:
        Result := CompareStrings(V1, V2);
      varUString:
        Result := CompareStrings(V1, V2);
    else
      Result := 0;
    end
  end;

begin
  if (VarType(V1) = VarType(V2)) then
    Result := CompareValues(V1, V2)
  else if IsStrVar(V1) and IsStrVar(V2) then
    Result := CompareStrings(V1, V2)
  else if IsBlankVar(V1) then
    Result := CompareValues(V2, V1)
  else if IsBlankVar(V2) then
    Result := CompareValues(V1, V2)
  else
    Result := 0;
end;

function InstantCompareText(const S1, S2: string; IgnoreCase: Boolean): Integer;
begin
  if IgnoreCase then
    Result := CompareText(S1, S2)
  else
    Result := CompareStr(S1, S2);
end;

function InstantConstArrayToVariant(AValues : array of const) : Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([Low(AValues), High(AValues)], varVariant);
  for I := Low(AValues) to High(AValues) do
  begin
    with AValues[I] do begin
      case VType of
        vtInteger: Result[I] := VInteger;
        vtBoolean: Result[I] := VBoolean;
        vtChar: Result[I] := VChar;
        vtExtended: Result[I] := VExtended^;
        vtString: Result[I] := VString^;
        vtPointer: Result[I] := Integer(VPointer);
        vtPChar: Result[I] := AnsiString(VPChar);
        vtAnsiString: Result[I] := string(VAnsiString);
        vtCurrency: Result[I] := VCurrency^;
        vtVariant: Result[I] := VVariant^;
      else
        raise Exception.Create(SInvalidDataType)
      end;
    end;
  end;
end;

function InstantDateTimeToStr(DateTime: TDateTime): string;
begin
  Result := FormatDateTime(InstantDateTimeFormat, DateTime)
end;

function InstantDateToStr(DateTime: TDateTime): string;
begin
  Result := FormatDateTime(InstantDateFormat, DateTime)
end;

function InstantTimeToStr(ADate: TDateTime): string;
begin
  Result := FormatDateTime(InstantTimeFormat, ADate)
end;

function InstantDateTimeToJStr(ADate: TDateTime): string;
const
  SDateFormat: string = '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%.3dZ';
  SOffsetFormat: string = '%s%s%.02d:%.02d';
  Neg: array[Boolean] of string = ('+', '-');
var
  y, mo, d, h, mi, se, ms: Word;
begin
  DecodeDate(ADate, y, mo, d);
  DecodeTime(ADate, h, mi, se, ms);
  Result := Format(SDateFormat, [y, mo, d, h, mi, se, ms]);
end;

function InstantEmbrace(const S, Delimiters: string): string;

  function LeftDelimiter: string;
  begin
    if Length(Delimiters) > 0 then
      Result := Delimiters[1]
    else
      Result := '';
  end;

  function RightDelimiter: string;
  begin
    if Length(Delimiters) = 2 then
      Result := Delimiters[2]
    else
      Result := LeftDelimiter
  end;

begin
  Result := LeftDelimiter + S + RightDelimiter;
end;

function InstantFileAge(const FileName: string; out FileDateTime: TDateTime): boolean;
begin
  Result := FileAge(FileName, FileDateTime);
end;

function InstantFileVersionStr(const FileName: string): string;
begin
  with InstantFileVersion(FileName) do
    Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

function InstantFileVersionValue(const FileName, ValueName: string): string;
type
  TLongInt = packed record
    LoWord, HiWord: Word;
  end;
  PTLongInt = ^TLongInt;
var
  Info, Value: Pointer;
  InfoSize, ValueSize, Wnd: DWORD;
  ValuePath, CharSet: string;
begin
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(Info, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, Info) then
      begin
        if VerQueryValue(Info, '\VarFileInfo\Translation', Value,
          ValueSize) then
        begin
          CharSet := Format('%.4x%.4x', [PTLongInt(Value)^.LoWord,
            PTLongInt(Value)^.HiWord]);
          ValuePath := Format('\StringFileInfo\%s\%s', [CharSet, ValueName]);
          if VerQueryValue(Info, PChar(ValuePath), Value, ValueSize) then
          begin
            SetLength(Result, ValueSize);
            StrLCopy(PChar(Result), Value, ValueSize);
            Exit;
          end;
        end;
      end;
    finally
      FreeMem(Info);
    end;
  end;
  Result := '';
end;

function InstantFileVersion(const FileName: string): TInstantVersion;
var
  InfoSize, VerSize, Wnd: DWORD;
  VerBuf: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) and
        VerQueryValue(VerBuf, '\', Pointer(VerValue), VerSize) then
        with VerValue^ do
        begin
          Result.Major := dwFileVersionMS shr 16;
          Result.Minor := dwFileVersionMS and $FFFF;
          Result.Release := dwFileVersionLS shr 16;
          Result.Build := dwFileVersionLS and $FFFF;
        end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

function InstantGenerateId: string;
var
  I: Integer;
  Id: array[0..15] of Byte;
begin
  OleCheck(CoCreateGuid(TGUID(Id)));
  Result := '';
  for I := 0 to 15 do
    Result := Result + IntToHex(Id[I], 2);
end;

function InstantIsIdentifier(Str: string): Boolean;
begin
  Result := IsValidIdent(Str);
end;

function InstantIsNumeric(const Str: string): Boolean;
var
  Code: Integer;
  D: Double;
begin
  Val(Str, D, Code);
  if D = D then;
  Result := Code = 0;
end;

function InstantIsQuoted(const Str: string; Quote: Char): Boolean;
var
  S: string;
  L: Integer;
begin
  S := Trim(Str);
  L := Length(S);
  Result := (L > 0) and (S[1] = Quote) and (S[L] = Quote);
end;

function InstantMatchChars(C1, C2: Char; IgnoreCase: Boolean): Boolean;
begin
  if IgnoreCase then
    Result := LowerCase(C1) = LowerCase(C2)
  else
    Result := C1 = C2;
end;

function InstantMatchObject(Obj: TObject; PropName: string;
  const KeyValue: Variant; Options: TInstantCompareOptions): Boolean;
var
  PropValue: Variant;
begin
  PropValue := InstantGetProperty(Obj, PropName);
  Result := InstantCompareValues(PropValue, KeyValue, Options) = 0;
end;

function InstantMatchObject(Obj: TObject; PropNames: TStrings;
  const KeyValues: Variant; Options: TInstantCompareOptions): Boolean;
var
  I: Integer;
  KeyValue: Variant;
begin
  Result := True;
  for I := 0 to Pred(PropNames.Count) do
  begin
    if (PropNames.Count = 1) and not VarIsArray(KeyValues) then
      KeyValue := KeyValues else
      KeyValue := KeyValues[I];
    if not InstantMatchObject(Obj, PropNames[I], KeyValue, Options) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function InstantModuleFileName: string;
var
  Filename: array[0..MAX_PATH] of Char;
begin
  GetModuleFileName(HInstance, Filename, MAX_PATH);
  Result := FileName;
end;

function InstantPartStr(Str: string; Index: Integer; Delimiter: Char): string;
var
  P: Integer;
begin
  while (Index > 1) and (Str <> '') do
  begin
    P := Pos(Delimiter, Str);
    if P = 0 then
      Str := ''
    else
      Delete(Str, 1, P);
    Dec(Index);
  end;
  P := Pos(Delimiter, Str);
  if P <> 0 then
    Delete(Str, P, Length(Str) - P + 1);
  Result := Str;
end;

function InstantQuote(const S: string; Quote: Char): string;
begin
  Result := AnsiQuotedStr(S, Quote);
end;

function InstantRightPos(const SubStr, Str: string; IgnoreCase: Boolean): Integer;
var
  I, J: Integer;
begin
  J := Length(SubStr);
  if J > 0 then
    for I := Length(Str) downto 1 do
    begin
      if InstantMatchChars(SubStr[J], Str[I], IgnoreCase) then
      begin
        Dec(J);
        if J = 0 then
        begin
          Result := I;
          Exit;
        end;
      end else
        J := Length(SubStr);
    end;
  Result := 0;
end;

function InstantSameText(const S1, S2: string; IgnoreCase: Boolean): Boolean;
begin
  if IgnoreCase then
    Result := SameText(S1, S2)
  else
    Result := S1 = S2;
end;

function InstantStrToDate(const Str: string): TDateTime;
var
  Y, M, D: Word;
begin
  Y := StrToIntDef(Copy(Str, 1, 4), 0);
  M := StrToIntDef(Copy(Str, 5, 2), 0);
  D := StrToIntDef(Copy(Str, 7, 2), 0);
  Result := EncodeDate(Y, M, D);
end;

function InstantStrToDateTime(const Str: string): TDateTime;
begin
  case Length(Str) of
    Length(InstantDateFormat):
      Result := InstantStrToDate(Str);
    Length(InstantTimeFormat):
      Result := InstantStrToTime(Str);
    Length(InstantDateTimeFormat):
      Result := InstantStrToDate(Copy(Str, 1, Length(InstantDateFormat))) +
        InstantStrToTime(Copy(Str, Length(InstantDateFormat) + 1,
        Length(InstantTimeFormat)));
  else
    raise EInstantConversionError.CreateFmt(SInvalidDateTime, [Str]);
  end;
end;

procedure InstantStrToList(const Str: string; List: TStrings;
  Delimiters: TChars);

  function ExtractPart(var Pos: Integer): string;
  var
    I: Integer;
  begin
    I := Pos;
    while (I <= Length(Str)) and not (InstantCharInSet(Str[I], Delimiters)) do
      Inc(I);
    Result := Copy(Str, Pos, I - Pos);
    if I <= Length(Str) then
      Inc(I);
    Pos := I;
  end;

var
  Pos: Integer;
begin
  List.BeginUpdate;
  try
    Pos := 1;
    while Pos <= Length(Str) do
      List.Add(Trim(ExtractPart(Pos)));
  finally
    List.EndUpdate;
  end;
end;

function InstantStrToTime(const Str: string): TDateTime;
var
  H, M, S, Z: Word;
begin
  H := StrToIntDef(Copy(Str, 1, 2), 0);
  M := StrToIntDef(Copy(Str, 3, 2), 0);
  S := StrToIntDef(Copy(Str, 5, 2), 0);
  Z := StrToIntDef(Copy(Str, 7, 3), 0);
  Result := EncodeTime(H, M, S, Z);
end;

function InstantUnquote(const Str: string; Quote: Char): string;
var
  S: PChar;
begin
  S := PChar(Str);
  if (Length(Str) > 0) and (Str[1] <> Quote) then
    Result := Str
  else
    Result := AnsiExtractQuotedStr(S, Quote);
end;

function InstantStrArrayToString(const StrArray: array of string; Delimiter: Char): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(StrArray) to High(StrArray) do
    if Result = '' then
      Result := StrArray[I]
    else
      Result := Result + Delimiter + StrArray[I];
end;

function DateOf(const AValue: TDateTime): TDateTime;
begin
  Result := Trunc(AValue);
end;

function TimeOf(const AValue: TDateTime): TDateTime;
begin
  Result := Frac(AValue);
end;

function InstantCharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := CharInSet(C, CharSet);
end;

function InstantCharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := CharInSet(C, CharSet);
end;

function InstantSmartConcat(const AArray: array of string; const ADelimiter: string = ' - '): string;
var
  LValue: string;
begin
  Result := '';
  for LValue in AArray do
  begin
    if LValue <> '' then
    begin
      if Result <> '' then
        Result := Result + ADelimiter;
      Result := Result + LValue;
    end;
  end;
end;

end.
