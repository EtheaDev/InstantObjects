(*
 *   InstantObjects
 *   Unit Selection Dialog
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
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantUnitSelect;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, ImgList, ActnList,
{$ENDIF}
{$IFDEF LINUX}
  QImgList, QActnList, QControls, QComCtrls, QStdCtrls, QExtCtrls,
{$ENDIF}
  InstantDualList, InstantImageUtils;

type
  TInstantUnitSelectForm = class(TInstantDualListForm)
    ListImages: TImageList;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TInstantUnitSelectForm.FormCreate(Sender: TObject);
begin
  inherited;
  ListImages.Clear;
  LoadMultipleImages(ListImages, 'IO_UNITSELECTIMAGES', HInstance);
{$IFDEF MSWINDOWS}
  LeftView.SmallImages := ListImages;
  RightView.SmallImages := ListImages;
{$ENDIF}
{$IFDEF LINUX}
  LeftView.Images := ListImages;
  RightView.Images := ListImages;
{$ENDIF}
end;

end.
