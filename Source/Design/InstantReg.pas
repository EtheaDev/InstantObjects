(*
 *   InstantObjects
 *   Component Registration
 *
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
 * Nando Dessena, Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantReg;

{$I '..\InstantDefines.inc'}

interface

procedure Register;

implementation

{$R '..\iodesimages.res'}

{$R IOCompsSplash.res}

uses
  Classes, Graphics, InstantConsts, InstantPersistence, InstantPresentation,
  InstantExplorer, InstantConnectionManager, InstantConnectionManagerFormUnit,
  InstantPump, InstantDBEvolution, InstantDBBuild
  , ToolsAPI
  ;

procedure RegisterWithSplashScreen;
var
  Bmp: TBitmap;
begin
  // Register IO Splash Icon on Delphi Splash Screen
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName(HInstance, 'IOCOMPSSPLASH');

  try
    SplashScreenServices.AddPluginBitmap(SSplashScreenTitle,
            Bmp.Handle, False, '', '');
  finally
    Bmp.Free;
  end;

end;

procedure Register;
begin
  RegisterWithSplashScreen;

  RegisterComponents(InstantPaletteName, [
    TInstantSelector,
    TInstantExposer,
    TInstantExplorer,
    TInstantConnectionManager,
    TInstantPump,
    TInstantDBEvolver,
    TInstantDBBuilder
  ]);
end;

end.
