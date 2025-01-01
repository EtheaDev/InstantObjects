(*
 *   InstantObjects
 *   Embarcadero FireDAC Support
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
 * The Original Code is: InstantObjects FireDAC Support
 *
 * The Initial Developer of the Original Code is: David Taylor
 *
 * Portions created by the Initial Developer are Copyright (C) 2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Carlo Barazzetta
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantFireDACReg;

interface

procedure Register;

implementation

uses
  System.Classes
  , InstantFireDAC
  ;

procedure Register;
begin
  RegisterComponents('InstantObjects', [TInstantFireDACConnector]);
end;

end.

