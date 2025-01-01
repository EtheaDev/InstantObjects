(*
 *   InstantObjects
 *   Text Reader/Writer
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
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantTextFiler;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  System.Classes
  , InstantClasses
  , System.Character
  ;

type
  PInstantTextPos = ^TInstantTextPos;
  TInstantTextPos = record
    Column, Line: Integer;
    Offset: Int64; // In characters, not bytes.
  end;

  EInstantTextPosError = class(EInstantError)
  private
    FPosition: TInstantTextPos;
  public
    constructor Create(const Msg: string; APosition: TInstantTextPos);
    property Position: TInstantTextPos read FPosition;
  end;

  TInstantTextFiler = class(TObject)
  private
    FFreeStream: Boolean;
    FPosition: TInstantTextPos;
    FStream: TStream;
    function GetPosition: TInstantTextPos;
    procedure SetPosition(const Value: TInstantTextPos);
  protected
    procedure AdvancePosition(Ch: Char);
    procedure DescendPosition(Ch: Char);
    function GetBof: Boolean; virtual;
    function GetEof: Boolean; virtual;
    function GetStreamPos: Int64; virtual;
    procedure Initialize; virtual;
    function IsSpace(Ch: Char): Boolean; virtual;
    function IsText(Ch: Char): Boolean; virtual;
    procedure SetStreamPos(Value: Int64); virtual;
    property StreamPos: Int64 read GetStreamPos write SetStreamPos;
  public
    constructor Create(AStream: TStream; FreeStream: Boolean = False); overload;
    constructor Create(AText: string); overload;
    destructor Destroy; override;
    procedure Reset; virtual;
    property Bof: Boolean read GetBof;
    property Eof: Boolean read GetEof;
    property Position: TInstantTextPos read GetPosition write SetPosition;
  end;

  TInstantTextReader = class(TInstantTextFiler)
  private
    FConstAware: Boolean;
    FTokenPos: TInstantTextPos;
  protected
    function GetBof: Boolean; override;
    function GetEof: Boolean; override;
    procedure Initialize; override;
    function IsIdentPrefix(Ch: Char): Boolean;
    function IsIdentChar(Ch: Char; AllowDots: boolean): Boolean;
    function IsNumericPrefix(Ch: Char): Boolean;
    function IsStringDelimiter(Ch: Char): Boolean;
  public
    function AtSpace: Boolean;
    function BackChar: Char;
    function GotoToken(const Token: string; Skip: Boolean;
      IgnoreCase: Boolean = True): Boolean;
    function NextChar: Char;
    function NextToken: string;
    function ReadChar: Char; virtual;
    function ReadMatching(const Str: string): Boolean;
    function ReadNext(const Str: string; StopBefore: Boolean = False): string;
    function ReadNumeric: string; virtual;
    function ReadString: string; virtual;
    function ReadIdent(AllowDots: boolean): string;
    function ReadToken: string;
    function SkipSpace: Boolean;
    procedure UnreadToken;
    property ConstAware: Boolean read FConstAware write FConstAware;
  end;

  TInstantTextWriter = class(TInstantTextFiler)
  public
    procedure WriteChar(Ch: Char); virtual;
    procedure WriteString(const Str: string);
  end;

procedure AdvanceTextPos(var Pos: TInstantTextPos; Ch: Char);
procedure DescendTextPos(var Pos: TInstantTextPos; Ch: Char);

implementation

uses
  System.SysUtils
  , InstantUtils
  , InstantConsts
  ;

procedure AdvanceTextPos(var Pos: TInstantTextPos; Ch: Char);
begin
  case Ch of
    #13: Pos.Column := 1;
    #10: Inc(Pos.Line);
  else
    Inc(Pos.Column);
  end;
  Inc(Pos.Offset);
end;

procedure DescendTextPos(var Pos: TInstantTextPos; Ch: Char);
begin
  case Ch of
    #13: Pos.Column := 0; { Unknown }
    #10: Dec(Pos.Line);
  else
    Dec(Pos.Column);
  end;
  Dec(Pos.Offset);
end;

{ EInstantTextPosError }

constructor EInstantTextPosError.Create(const Msg: string;
  APosition: TInstantTextPos);
begin
  inherited Create(Msg);
  FPosition := APosition;
end;

{ TInstantTextFiler }

procedure TInstantTextFiler.AdvancePosition(Ch: Char);
begin
  AdvanceTextPos(FPosition, Ch);
end;

constructor TInstantTextFiler.Create(AStream: TStream; FreeStream: Boolean);
begin
  if not Assigned(AStream) then
    raise EInstantError.Create(SUnassignedStream);
  FStream := AStream;
  FFreeStream := FreeStream;
  Initialize;
  Reset;
end;

constructor TInstantTextFiler.Create(AText: string);
begin
  Create(TInstantStringStream.Create(AText), True);
end;

procedure TInstantTextFiler.DescendPosition(Ch: Char);
begin
  DescendTextPos(FPosition, Ch);
end;

destructor TInstantTextFiler.Destroy;
begin
  inherited;
  if FFreeStream then
    FStream.Free;
end;

function TInstantTextFiler.GetBof: Boolean;
begin
  Result := FStream.Size = 0;
end;

function TInstantTextFiler.GetEof: Boolean;
begin
  Result := FStream.Size = 0;
end;

function TInstantTextFiler.GetPosition: TInstantTextPos;
begin
  FPosition.Offset := StreamPos div SizeOf(Char);
  Result := FPosition;
end;

function TInstantTextFiler.GetStreamPos: Int64;
begin
  Result := FStream.Position;
end;

procedure TInstantTextFiler.Initialize;
begin
end;

function TInstantTextFiler.IsSpace(Ch: Char): Boolean;
begin
  Result := InstantCharInSet(Ch, [' ', #9, #10, #13]);
end;

function TInstantTextFiler.IsText(Ch: Char): Boolean;
begin
  Result := ((Ch >= 'a') and (Ch <= 'z'))
    or ((Ch >= 'A') and (Ch <= 'Z'))
    or ((Ch >= '0') and (Ch <= '9'))
    or (InstantCharInSet(Ch, ['#', '_']));
end;

procedure TInstantTextFiler.Reset;
begin
  FStream.Position := 0;
  with FPosition do
  begin
    Column := 1;
    Line := 1;
    Offset := 0;
  end;
end;

procedure TInstantTextFiler.SetPosition(const Value: TInstantTextPos);
begin
  FPosition := Value;
  StreamPos := FPosition.Offset * SizeOf(Char);
end;

procedure TInstantTextFiler.SetStreamPos(Value: Int64);
begin
  FStream.Position := Value;
end;

{ TInstantTextReader }

function TInstantTextReader.AtSpace: Boolean;
begin
  Result := IsSpace(NextChar);
end;

function TInstantTextReader.BackChar: Char;
begin
  if FStream.Position > 0 then
    FStream.Position := FStream.Position - SizeOf(Char);
  FStream.Read(Result, SizeOf(Char));
  FStream.Position := FStream.Position - SizeOf(Char);
  DescendPosition(Result);
end;

function TInstantTextReader.GetBof: Boolean;
begin
  Result := FStream.Position = 0;
end;

function TInstantTextReader.GetEof: Boolean;
begin
  Result := FStream.Position = FStream.Size;
end;

function TInstantTextReader.GotoToken(const Token: string; Skip,
  IgnoreCase: Boolean): Boolean;
begin
  while not Eof do
  begin
    if InstantSameText(ReadToken, Token, IgnoreCase) then
    begin
      if not Skip then
        UnreadToken;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TInstantTextReader.Initialize;
begin
  inherited;
  ConstAware := True;
end;

function TInstantTextReader.IsIdentPrefix(Ch: Char): Boolean;
begin
  Result := Ch.IsLetter or  (Ch = '_');
end;

function TInstantTextReader.IsIdentChar(Ch: Char; AllowDots: boolean): Boolean;
begin
  Result := Ch.IsLetterOrDigit or (Ch = '_') or (AllowDots and (Ch = '.'));
end;

function TInstantTextReader.IsNumericPrefix(Ch: Char): Boolean;
begin
  Result := ConstAware and
    (((Ch >= '0') and (Ch <= '9')) or (Ch = '.'));
end;

function TInstantTextReader.IsStringDelimiter(Ch: Char): Boolean;
begin
  Result := ConstAware and (InstantCharInSet(Ch, ['''', '"']));
end;

function TInstantTextReader.NextChar: Char;
var
  SavePos: Int64;
begin
  SavePos := FStream.Position;
  try
    FStream.Read(Result, SizeOf(Char));
  finally
    FStream.Position := SavePos;
  end;
end;

function TInstantTextReader.NextToken: string;
begin
  Result := ReadToken;
  UnreadToken;
end;

function TInstantTextReader.ReadChar: Char;
begin
  FStream.Read(Result, SizeOf(Char));
  AdvancePosition(Result);
end;

function TInstantTextReader.ReadMatching(const Str: string): Boolean;
var
  SavePos: TInstantTextPos;
  I: Integer;
begin
  SavePos := Position;
  I := 1;
  while not Eof do
  begin
    if I > Length(Str) then
    begin
      Result := True;
      Exit;
    end;
    if ReadChar = Str[I] then
      Inc(I)
    else
      Break;
  end;
  Position := SavePos;
  Result := False;
end;

function TInstantTextReader.ReadNext(const Str: string;
  StopBefore: Boolean): string;
var
  Ch: Char;
  I: Integer;
  SavePos, StartPos: TInstantTextPos;
begin
  Result := '';
  I := 1;
  StartPos := Position;
  while not Eof and (I <= Length(Str)) do
  begin
    SavePos := Position;
    Ch := ReadChar;
    if IsStringDelimiter(Ch) then
    begin
      Position := SavePos;
      Result := Result + ReadString;
    end else
    begin
      Result := Result + Ch;
      if InstantMatchChars(Ch, Str[I], True) then
      begin
        if I = 1 then
          StartPos := SavePos;
        Inc(I)
      end else
        I := 1;
    end;
  end;
  if (I > Length(Str)) and StopBefore then
    Position := StartPos;
end;

function TInstantTextReader.ReadNumeric: string;
var
  SavePos: TInstantTextPos;
  Ch: Char;
begin
  Result := '';
  SavePos := Position;
  SkipSpace;
  Ch := ReadChar;
  if IsNumericPrefix(Ch) then
  begin
    Result := Ch;
    while not Eof and InstantIsNumeric(Result + NextChar) do
      Result := Result + ReadChar;
  end;
  if Result = '' then
    Position := SavePos;
end;

function TInstantTextReader.ReadString: string;
var
  Ch, Delimiter: Char;
  SavePos: TInstantTextPos;
begin
  Result := '';
  SavePos := Position;
  SkipSpace;
  Ch := ReadChar;
  if IsStringDelimiter(Ch) then
  begin
    Delimiter := Ch;
    while not Eof do
    begin
      Result := Result + Ch;
      Ch := ReadChar;
      if Ch = Delimiter then
      begin
        Result := Result + Ch;
        if NextChar = Delimiter then
          Ch := ReadChar
        else
          Break;
      end;
    end;
  end else
    Position := SavePos;
end;

function TInstantTextReader.ReadIdent(AllowDots: boolean): string;
var
  Ch: Char;
  SavePos: TInstantTextPos;
begin
  Result := '';
  if Eof then
    Exit;
  FTokenPos := Position;
  Ch := ReadChar;
  while IsSpace(Ch) do
  begin
    FTokenPos := Position;
    if Eof then
      Exit;
    Ch := ReadChar;
  end;
  if (not IsIdentPrefix(Ch)) then
  begin
    Position := FTokenPos;
    Exit;
  end;
  repeat
    Result := Result + Ch;
    if Eof then
      Exit;
    SavePos := Position;
    Ch := ReadChar;
  until not IsIdentChar(Ch,AllowDots);
  Position := SavePos;
end;

function TInstantTextReader.ReadToken: string;
var
  Ch: Char;
  SavePos: TInstantTextPos;
begin
  Result := '';
  if Eof then
    Exit;
  FTokenPos := Position;
  Ch := ReadChar;
  while IsSpace(Ch) do
  begin
    FTokenPos := Position;
    if Eof then
      Exit;
    Ch := ReadChar;
  end;
  if IsStringDelimiter(Ch) then
  begin
    Position := FTokenPos;
    Result := ReadString;
    Exit;
  end;
  if IsNumericPrefix(Ch) then
  begin
    Position := FTokenPos;
    Result := ReadNumeric;
    if Result <> '' then
      Exit;
  end;
  while IsText(Ch) do
  begin
    Result := Result + Ch;
    if Eof then
      Exit;
    SavePos := Position;
    Ch := ReadChar;
  end;
  if Result = '' then
    Result := Ch
  else
    Position := SavePos;
end;

function TInstantTextReader.SkipSpace: Boolean;
begin
  while not Eof do
    if not IsSpace(ReadChar) then
    begin
      BackChar;
      Break;
    end;
  Result := Eof;
end;

procedure TInstantTextReader.UnreadToken;
begin
  Position := FTokenPos;
end;

{ TInstantTextWriter }

procedure TInstantTextWriter.WriteChar(Ch: Char);
begin
  FStream.Write(Ch, SizeOf(Ch));
  AdvancePosition(Ch);
end;

procedure TInstantTextWriter.WriteString(const Str: string);
var
  I: Integer;
begin
  for I := 1 to Length(Str) do
    WriteChar(Str[I]);
end;

end.
