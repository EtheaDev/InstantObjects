(*
 *   InstantObjects
 *   Design Resources
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
 * Carlo Barazzetta, Adrea Petrelli: porting Kylix
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDesignResources;

interface

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, ImgList,
{$ENDIF}
{$IFDEF LINUX}
  QMenus, QControls, QExtCtrls, QForms, QImgList, QGraphics,
{$ENDIF}
  Classes;

type
  TInstantDesignResourceModule = class(TDataModule)
    ToolImages: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  InstantImageUtils;

procedure TInstantDesignResourceModule.DataModuleCreate(Sender: TObject);
begin
  LoadMultipleImages(ToolImages, 'IO_DESIGNRESOURCEIMAGES', HInstance);
end;

end.
