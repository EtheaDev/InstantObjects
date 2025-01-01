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
  System.Classes
  {$IF (CompilerVersion >= 27.0)}, BrandingAPI{$IFEND}
  , Vcl.Graphics
  , InstantConsts
  , InstantPersistence
  , InstantPresentation
  , InstantExplorer
  , InstantConnectionManager
  , InstantConnectionManagerFormUnit
  , InstantPump
  , InstantDBEvolution
  , InstantDBBuild
  , ToolsAPI
  , Vcl.Imaging.PngImage
  , Winapi.Windows
  , System.SysUtils
  , InstantUtils
  ;

const
  {$IF (CompilerVersion >= 35.0)}
  ABOUT_RES_NAME = 'IOSPLASH48PNG';
  SPLASH_RES_NAME = 'IOSPLASH48PNG';
  {$ELSE}
  ABOUT_RES_NAME = 'IOSPLASH24BMP';
  SPLASH_RES_NAME = 'IOSPLASH24BMP';
  {$IFEND}
  RsAboutTitle = 'Ethea InstantObjects';
  RsAboutDescription = 'Ethea - InstantObjects - https://github.com/EtheaDev/InstantObjects/' + sLineBreak +
    'Pupular OOP-OPF Library and components for Delphi';
  RsAboutLicense = 'Mozilla 2.0 (Free/Opensource)';
var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer;

{$IF (CompilerVersion >= 35.0)}
function CreateBitmapFromPngRes(const AResName: string): Vcl.Graphics.TBitmap;
var
  LPngImage: TPngImage;
  LResStream: TResourceStream;
begin
  LPngImage := nil;
  try
    Result := Vcl.Graphics.TBitmap.Create;
    LPngImage := TPngImage.Create;
    LResStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
    try
      LPngImage.LoadFromStream(LResStream);
      Result.Assign(LPngImage);
    finally
      LResStream.Free;
    end;
  finally
    LPngImage.Free;
  end;
end;

procedure RegisterAboutBox;
var
  LBitmap: Vcl.Graphics.TBitmap;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  LBitmap := CreateBitmapFromPngRes(ABOUT_RES_NAME);
  try
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(
      RsAboutTitle+' '+InstantObjectVersion,
      RsAboutDescription, LBitmap.Handle, False, RsAboutLicense);
  finally
    LBitmap.Free;
  end;
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  LBitmap: Vcl.Graphics.TBitmap;
begin
  LBitmap := CreateBitmapFromPngRes(SPLASH_RES_NAME);
  try
    SplashScreenServices.AddPluginBitmap(
      RsAboutTitle+' '+InstantObjectVersion,
      LBitmap.Handle, False, RsAboutLicense, '');
  finally
    LBitmap.Free;
  end;
end;
{$ELSE}
procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), ABOUT_RES_NAME);
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(RsAboutTitle+' '+InstantObjectVersion,
    RsAboutDescription, ProductImage, False, RsAboutLicense);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  ProductImage: HBITMAP;
begin
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), SPLASH_RES_NAME);
  SplashScreenServices.AddPluginBitmap(RsAboutTitle, ProductImage,
    False, RsAboutLicense);
end;
{$IFEND}

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

initialization
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

end.
