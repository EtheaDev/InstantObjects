(*
 *   InstantObjects
 *   Design Utilities
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
 * Carlo Barazzetta, Adrea Petrelli, Steven Mitchell, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDesignUtils;

{$I '..\InstantDefines.inc'}

interface

uses
  Data.DB
  , Vcl.Controls
  , Vcl.DbCtrls
  , Vcl.Graphics
  , Vcl.Forms
  , Vcl.Dialogs
  ;

procedure Busy(Enable: Boolean);
function Confirm(const Msg: string): Boolean;
procedure EnableControl(Control: TControl; Enable: Boolean; Source: TDataSource);
function ShortenPath(const Path: string; Canvas: TCanvas; MaxLen: Integer): string;
function Remove_T_FromClassName(const AClassName: string): string;

implementation

uses
  System.SysUtils
  , System.TypInfo
  ;

var
  __SaveCursor: TCursor = 0;
  __BusyCount: Integer = 0;

procedure Busy(Enable: Boolean);
begin
  if Enable then
  begin
    if __BusyCount = 0 then
    begin
      __SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
    end;
    Inc(__BusyCount);
  end else if __BusyCount > 0 then
  begin
    Dec(__BusyCount);
    if __BusyCount = 0 then
      Screen.Cursor := __SaveCursor;
  end;
end;

function Confirm(const Msg: string): Boolean;
begin
  Result := MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure EnableControl(Control: TControl; Enable: Boolean;
  Source: TDataSource);
const
  EditColors: array[Boolean] of TColor = (clBtnFace, clWhite);
var
  NewSource: TDataSource;
begin
  Control.Enabled := Enable;
  if Assigned(Source) then
  begin
    if Enable then
      NewSource := Source
    else
      NewSource := nil;
    if IsPublishedProp(Control, 'DataSource') then
      SetObjectProp(Control, 'DataSource', NewSource)
  end;
  if Control is TDBEdit then
    TDBEdit(Control).Color := EditColors[Enable]
  else if Control is TDBComboBox then
    TDBComboBox(Control).Color := EditColors[Enable];
end;

procedure CutFirstDirectory(var S: string);
var
  Root: Boolean;
  P: Integer;
begin
  if S = '\' then
    S := ''
  else
  begin
    if S[1] = '\' then
    begin
      Root := True;
      Delete(S, 1, 1);
    end
    else
      Root := False;
    if S[1] = '.' then
      Delete(S, 1, 4);
    P := AnsiPos('\',S);
    if P <> 0 then
    begin
      Delete(S, 1, P);
      S := '...\' + S;
    end
    else
      S := '';
    if Root then
      S := '\' + S;
  end;
end;

function ShortenPath(const Path: string; Canvas: TCanvas;
  MaxLen: Integer): string;
var
  Drive: string;
  Dir: string;
  Name: string;
begin
  Result := Path;
  Dir := ExtractFilePath(Result);
  Name := ExtractFileName(Result);

  if (Length(Dir) >= 2) and (Dir[2] = ':') then
  begin
    Drive := Copy(Dir, 1, 2);
    Delete(Dir, 1, 2);
  end
  else
    Drive := '';
  while ((Dir <> '') or (Drive <> '')) and (Canvas.TextWidth(Result) > MaxLen) do
  begin
    if Dir = '\...\' then
    begin
      Drive := '';
      Dir := '...\';
    end
    else if Dir = '' then
      Drive := ''
    else
      CutFirstDirectory(Dir);
    Result := Drive + Dir + Name;
  end;
end;

function Remove_T_FromClassName(const AClassName: string): string;
begin
  Result := AClassName;
  // Remove the 'T' from classname
  if (Length(Result) > 1) and (Result[1] = 'T') then
    Delete(Result, 1, 1);
end;

end.
