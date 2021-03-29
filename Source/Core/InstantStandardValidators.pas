(*
 *   InstantObjects
 *   Validation framework - standard validators.
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
 * Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantStandardValidators;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Contnrs, InstantClasses, InstantMetadata, InstantValidation;

type
  // Base class for classes that handle the pieces of a ValidCharsString string.
  TInstantCharSetPieceValidator = class
  protected
    function GetValidCharsAsString: string; virtual;
  public
    function IsValidChar(const AChar: Char): Boolean; virtual; abstract;
    property ValidCharsAsString: string read GetValidCharsAsString;
  end;

  // Validates a character against a set of distinct characters stored in a string.
  TInstantCharsPieceValidator = class(TInstantCharSetPieceValidator)
  private
    FValidChars: string;
  protected
    function GetValidCharsAsString: string; override;
  public
    procedure AfterConstruction; override;
    procedure AddChar(const AChar: Char);
    procedure RemoveLastChar;
    function IsValidChar(const AChar: Char): Boolean; override;
  end;

  // Validates a character against a range of characters.
  TInstantCharRangePieceValidator = class(TInstantCharSetPieceValidator)
  private
    FValidFrom: Char;
    FValidTo: Char;
  protected
    function GetValidCharsAsString: string; override;
  public
    procedure SetValidChars(const AValidFrom, AValidTo: Char);
    function IsValidChar(const AChar: Char): Boolean; override;
  end;

  // Splits a ValidCharsString string into pieces that hands each to a
  // different internal instance of TInstantCharSetPieceValidator.
  TInstantCharSetValidator = class(TInstantValidator)
  private
    FPieces: TObjectList;
    procedure CreatePieceValidators(const AValidChars: string);
    function IsValidChar(const AChar: Char;
      out AValidationErrorText: string): Boolean;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    class function CreateValidator(
      const AMetadata: TInstantAttributeMetadata): TInstantValidator; override;
    function IsValid(const AAttribute: TInstantAbstractAttribute;
      const AValue: string; out AValidationErrorText: string): Boolean;
      override;
  end;

implementation

uses
  Classes, SysUtils, InstantConsts;

{ TInstantCharSetValidator }

procedure TInstantCharSetValidator.AfterConstruction;
begin
  inherited;
  FPieces := TObjectList.Create;
end;

procedure TInstantCharSetValidator.CreatePieceValidators(
  const AValidChars: string);
var
  I: Integer;
  LCharsValidator: TInstantCharsPieceValidator;
  LCharRangeValidator: TInstantCharRangePieceValidator;
  LChar: Char;
begin
  LCharsValidator := TInstantCharsPieceValidator.Create;
  FPieces.Add(LCharsValidator);
  I := 1;
  while I <= Length(AValidChars) do
  begin
    LChar := AValidChars[I];
    // a..b means a range from a to b.
    if (LChar = '.') and (I > 1) and (I < Length(AValidChars) - 1) and (AValidChars[Succ(I)] = '.') then
    begin
      LCharRangeValidator := TInstantCharRangePieceValidator.Create;
      FPieces.Add(LCharRangeValidator);
      LCharRangeValidator.SetValidChars(AValidChars[Pred(I)],
        AValidChars[I + 2]);
      // no need for the chars validator to take care of the first char in
      // my range.
      LCharsValidator.RemoveLastChar;
      // skip the second dot and the range end char.
      Inc(I, 2);
    end
    // everything else is a simple match.
    else
      LCharsValidator.AddChar(LChar);
    Inc(I);
  end;
end;

class function TInstantCharSetValidator.CreateValidator(
  const AMetadata: TInstantAttributeMetadata): TInstantValidator;
begin
  if Assigned(AMetadata) and (AMetadata.ValidCharsString <> '') then
  begin
    Result := Create;
    try
      Result.Metadata := AMetadata;
      TInstantCharSetValidator(Result).CreatePieceValidators(AMetadata.ValidCharsString);
    except
      FreeAndNil(Result);
      raise;
    end;
  end
  else
    Result := nil;
end;

destructor TInstantCharSetValidator.Destroy;
begin
  FreeAndNil(FPieces);
  inherited;
end;

function TInstantCharSetValidator.IsValid(
  const AAttribute: TInstantAbstractAttribute; const AValue: string;
  out AValidationErrorText: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(AValue) do
  begin
    if not IsValidChar(AValue[I], AValidationErrorText) then
    begin
      Result := False;
      Break;
    end;  
  end;
  if not Result then
    AValidationErrorText := Format(SInvalidAttributeValue, [AValue,
      AAttribute.ClassName, Metadata.Name]) + sLineBreak +
      AValidationErrorText;
end;

function TInstantCharSetValidator.IsValidChar(
  const AChar: Char; out AValidationErrorText: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  AValidationErrorText := '';
  for I := 0 to FPieces.Count - 1 do
  begin
    if TInstantCharSetPieceValidator(FPieces[I]).IsValidChar(AChar) then
    begin
      Result := True;
      Break;
    end;
  end;
  if not Result then
    AValidationErrorText := Format(SInvalidChar, [AChar, Ord(AChar), Metadata.ValidCharsString]);
end;

{ TInstantCharSetPieceValidator }

function TInstantCharSetPieceValidator.GetValidCharsAsString: string;
begin
  Result := '';
end;

{ TInstantCharsPieceValidator }

procedure TInstantCharsPieceValidator.AddChar(const AChar: Char);
begin
  if Pos(AChar, FValidChars) = 0 then
    FValidChars := FValidChars + AChar;
end;

procedure TInstantCharsPieceValidator.AfterConstruction;
begin
  inherited;
  // This is for backward compatibility, as IO has always allowed these
  // characters in a string or memo when ValidCharsString is not empty.
  // I don't think this is strictly correct, especially for string fields.
  FValidChars := #8#10#13;
end;

function TInstantCharsPieceValidator.GetValidCharsAsString: string;
begin
  Result := FValidChars;
end;

function TInstantCharsPieceValidator.IsValidChar(const AChar: Char): Boolean;
begin
  Result := Pos(AChar, FValidChars) > 0;
end;

procedure TInstantCharsPieceValidator.RemoveLastChar;
begin
  if FValidChars <> '' then
    Delete(FValidChars, Length(FValidChars) , 1);
end;

{ TInstantCharRangePieceValidator }

function TInstantCharRangePieceValidator.GetValidCharsAsString: string;
begin
  Result := FValidFrom + '..' + FValidTo;
end;

function TInstantCharRangePieceValidator.IsValidChar(const AChar: Char): Boolean;
begin
  Result := (AChar >= FValidFrom) and (AChar <= FValidTo);
end;

procedure TInstantCharRangePieceValidator.SetValidChars(const AValidFrom,
  AValidTo: Char);
begin
  if AValidFrom > AValidTo then
  begin
    FValidFrom := AValidTo;
    FValidTo := AValidFrom;
  end
  else
  begin
    FValidFrom := AValidFrom;
    FValidTo := AValidTo;
  end;
end;

initialization
  InstantValidatorFactory.AddValidatorClass(TInstantCharSetValidator);

finalization
  InstantValidatorFactory.RemoveValidatorClass(TInstantCharSetValidator);

end.

