unit Utility;

interface

{$I '..\..\Source\InstantDefines.inc'}

procedure BeginBusy;
procedure EndBusy;
function Confirm(const Text: string): Boolean;

implementation

uses
  Forms, Dialogs, Controls,
  System.UITypes,
  SysUtils;

var
  BusySaveCursor: TCursor;
  BusyCount: Integer;

procedure BeginBusy;
begin
  if BusyCount = 0 then
  begin
    BusySaveCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
  end;
  Inc(BusyCount);
end;

procedure EndBusy;
begin
  if BusyCount > 0 then
  begin
    Dec(BusyCount);
    if BusyCount = 0 then
      Screen.Cursor := BusySaveCursor;
  end;
end;

function Confirm(const Text: string): Boolean;
begin
  Result := MessageDlg(Text, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

end.
