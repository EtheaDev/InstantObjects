(*
 *   InstantObjects
 *   Validation framework
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

unit InstantValidation;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  Contnrs, InstantClasses, InstantMetadata, InstantPersistence;

type
  TInstantValidatorListValidationMode = (vmAll, vmAny);

  // A validator that is a list of validators (composite).
  // Shouldn't be registered as it's used directly by the
  // factory as a wrapper for multiple validators and also as a null object,
  // for when no validators apply. It is also a base class for other
  // validators.
  TInstantValidatorList = class(TInstantValidator)
  private
    FValidators: TObjectList;
    FConcatMode: TInstantValidatorListValidationMode;
    function AllValid(const AAttribute: TInstantAbstractAttribute;
      const AValue: string; out AValidationErrorText: string): Boolean;
    function AnyValid(const AAttribute: TInstantAbstractAttribute;
      const AValue: string; out AValidationErrorText: string): Boolean;
  public
    function IsValid(const AAttribute: TInstantAbstractAttribute;
      const AValue: string; out AValidationErrorText: string): Boolean;
      override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    // Adds a validator to the inner list.
    procedure Add(const AValidator: TInstantValidator);
    // Decides whether validators in lists should all pass for a value to be
    // accepted (vmAll) or if any one of them suffices (vmAny).
    property ConcatMode: TInstantValidatorListValidationMode
      read FConcatMode write FConcatMode default vmAll;
  end;

  // Creates validators based on metadata contents.
  TInstantValidatorFactory = class
  private
    FClasses: TClassList;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure AddValidatorClass(const AValidatorClass: TInstantValidatorClass);
    procedure RemoveValidatorClass(const AValidatorClass: TInstantValidatorClass);

    { TODO : Extend this for class-level validation? }
    function CreateValidator(const AMetadata: TInstantAttributeMetadata): TInstantValidator;
  end;

function InstantValidatorFactory: TInstantValidatorFactory;

implementation

uses
  Classes,
  System.Types,
 SysUtils;

var
  _InstantValidatorFactory: TInstantValidatorFactory;

function InstantValidatorFactory: TInstantValidatorFactory;
begin
  if not Assigned(_InstantValidatorFactory) then
    _InstantValidatorFactory := TInstantValidatorFactory.Create;
  Result := _InstantValidatorFactory;
end;

{ TInstantValidatorFactory }

procedure TInstantValidatorFactory.AddValidatorClass(
  const AValidatorClass: TInstantValidatorClass);
begin
  FClasses.Add(AValidatorClass);
end;

procedure TInstantValidatorFactory.AfterConstruction;
begin
  inherited;
  FClasses := TClassList.Create;
end;

function TInstantValidatorFactory.CreateValidator(
  const AMetadata: TInstantAttributeMetadata): TInstantValidator;
var
  I: Integer;
begin
  Result := TInstantValidatorList.Create;
  try
    for I := 0 to FClasses.Count - 1 do
      TInstantValidatorList(Result).Add(
        TInstantValidatorClass(FClasses[I]).CreateValidator(AMetadata));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

destructor TInstantValidatorFactory.Destroy;
begin
  FreeAndNil(FClasses);
  inherited;
end;

procedure TInstantValidatorFactory.RemoveValidatorClass(
  const AValidatorClass: TInstantValidatorClass);
begin
  FClasses.Remove(AValidatorClass);
end;

{ TInstantValidatorList }

procedure TInstantValidatorList.Add(const AValidator: TInstantValidator);
begin
  // It is valid for this method to receive nil and don't do anything.
  if Assigned(AValidator) then
    FValidators.Add(AValidator);
end;

procedure TInstantValidatorList.AfterConstruction;
begin
  inherited;
  FValidators := TObjectList.Create;
  FConcatMode := vmAll;
end;

destructor TInstantValidatorList.Destroy;
begin
  FreeAndNil(FValidators);
  inherited;
end;

function TInstantValidatorList.IsValid(
  const AAttribute: TInstantAbstractAttribute; const AValue: string;
  out AValidationErrorText: string): Boolean;
begin
  if FConcatMode = vmAll then
    Result := AllValid(AAttribute, AValue, AValidationErrorText)
  else
    Result := AnyValid(AAttribute, AValue, AValidationErrorText);
end;

function TInstantValidatorList.AllValid(
  const AAttribute: TInstantAbstractAttribute; const AValue: string;
  out AValidationErrorText: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  AValidationErrorText := '';
  for I := 0 to FValidators.Count - 1 do
  begin
    if not TInstantValidator(FValidators[I]).IsValid(AAttribute, AValue, AValidationErrorText) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TInstantValidatorList.AnyValid(
  const AAttribute: TInstantAbstractAttribute; const AValue: string;
  out AValidationErrorText: string): Boolean;
var
  I: Integer;
  LErrorText: string;
begin
  Result := False;
  AValidationErrorText := '';
  for I := 0 to FValidators.Count - 1 do
  begin
    if TInstantValidator(FValidators[I]).IsValid(AAttribute, AValue, LErrorText) then
    begin
      Result := True;
      Break;
    end
    else
    begin
      if AValidationErrorText = '' then
        AValidationErrorText := LErrorText
      else
        AValidationErrorText := AValidationErrorText + sLineBreak + LErrorText;        
    end;
  end;
end;

initialization

finalization
  FreeAndNil(_InstantValidatorFactory);

end.

