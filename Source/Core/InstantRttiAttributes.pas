(*
 *   InstantObjects
 *   Delphi 2010 Custom Attributes framework
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
 * The Original Code is: Brian Andersen
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantRttiAttributes;

interface

uses
  InstantPersistence
  , System.Rtti
  ;

type
  TInstantRttiAttribute = class(TCustomAttribute)
  public
    procedure Change(AObject: TInstantObject; AMember: TRttiMember); virtual; abstract;
  end;

  TInstantRttiAttributeClass = class of TInstantRttiAttribute;

  TInstantRttiFieldAttribute = class(TInstantRttiAttribute)
  protected
    procedure InternalChange(AObject: TInstantObject; AField: TRttiField); virtual; abstract;
  public
    procedure Change(AObject: TInstantObject; AMember: TRttiMember); override;
  end;

  TInstantRttiMethodAttribute = class(TInstantRttiAttribute)
  protected
    procedure InternalChange(AObject: TInstantObject; AMethod: TRttiMethod); virtual; abstract;
  public
    procedure Change(AObject: TInstantObject; AMember: TRttiMember); override;
  end;

  TInstantRttiPropertyAttribute = class(TInstantRttiAttribute)
  protected
    procedure InternalChange(AObject: TInstantObject; AProperty: TRttiProperty); virtual; abstract;
  public
    procedure Change(AObject: TInstantObject; AMember: TRttiMember); override;
  end;


implementation

{ TInstantRttiFieldAttribute }

procedure TInstantRttiFieldAttribute.Change(AObject: TInstantObject;
  AMember: TRttiMember);
begin
  InternalChange(AObject, AMember as TRttiField);
end;

{ TInstantRttiMethodAttribute }

procedure TInstantRttiMethodAttribute.Change(AObject: TInstantObject;
  AMember: TRttiMember);
begin
  InternalChange(AObject, AMember as TRttiMethod);
end;

{ TInstantRttiPropertyAttribute }

procedure TInstantRttiPropertyAttribute.Change(AObject: TInstantObject;
  AMember: TRttiMember);
begin
  InternalChange(AObject, AMember as TRttiProperty);
end;

end.