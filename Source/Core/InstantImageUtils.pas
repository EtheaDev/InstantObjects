(*
 *   InstantObjects
 *   InstantImageUtils
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
 * The Original Code is: Carlo Barazzetta
 *
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Contributor(s):
 * Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantImageUtils;

interface

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

uses
  SysUtils,
  Graphics, Controls, ImgList,
  Classes;

// Loads a bitmap resource and slices it into a number of images which are
// added to ImageList. If ResourceHandle is passed, then the resource ResourceName
// is loaded from the specified module handle, otherwise ResourceName is
// interpreted as a file name containing the bitmap to load.
procedure LoadMultipleImages(ImageList: TImageList; const ResourceName: string;
  const ResourceHandle: LongWord = 0);

implementation

procedure LoadMultipleImages(ImageList: TImageList; const ResourceName: string;
  const ResourceHandle: LongWord = 0);
var
  Picture: TPicture;
  X, Y: Integer;
  IWidth, IHeight: Integer;
  NewBitmap: TBitmap;
  SubDivideX, SubDivideY: Boolean;
  DivideX, DivideY: Integer;
begin
  ImageList.Clear;
  Picture := TPicture.Create;
  try
    if ResourceHandle = 0 then
      Picture.Bitmap.LoadFromFile(ResourceName)
    else
      Picture.Bitmap.LoadFromResourceName(ResourceHandle, ResourceName);
    IWidth := ImageList.Width;
    IHeight := ImageList.Height;
    if Picture.Graphic is TBitmap then
    begin
      SubDivideX := (Picture.Graphic.Width > IWidth) and
        (Picture.Graphic.Width mod IWidth = 0);

      SubDivideY := (Picture.Graphic.Height > IHeight) and
        (Picture.Graphic.Height mod IHeight = 0);

      if SubDivideX then
        DivideX := Picture.Graphic.Width div IWidth
      else
        DivideX := 1;

      if SubDivideY then
        DivideY := Picture.Graphic.Height div IHeight
      else
        DivideY := 1;

      for Y := 0 to DivideY - 1 do
      begin
        for X := 0 to DivideX - 1 do
        begin
          NewBitmap := TBitmap.Create;
          try
            NewBitmap.Height := IHeight;
            NewBitmap.Width := IWidth;
            NewBitmap.Canvas.CopyRect(Rect(0, 0, NewBitmap.Width,
            NewBitmap.Height), Picture.Bitmap.Canvas,
              Rect(X * IWidth, Y * IHeight, (X + 1) * IWidth,
              (Y + 1) * IHeight));
            ImageList.AddMasked(NewBitmap, NewBitmap.TransparentColor);
          finally
            FreeAndNil(NewBitmap);
          end;
        end;
      end;
    end
    else
      raise EInvalidGraphic.CreateFmt('Error loading images %s',[ResourceName]);
  finally
    Picture.Free;
  end;
end;

end.
