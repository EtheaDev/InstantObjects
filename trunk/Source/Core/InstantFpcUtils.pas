unit InstantFpcUtils;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RtlConsts, Db;

procedure OleError(ErrorCode: HResult);
procedure OleCheck(Result: HResult);

function FormatMaskText(const EditMask: string; const Value: string): string;

implementation

uses
  typinfo, variants, maskedit;
  
  

function OleResult(Res: HResult): Boolean;
begin
  Result := Res and $80000000 = 0;
end;

{ Raise EOleSysError exception from an error code }

procedure OleError(ErrorCode: HResult);
begin
//  raise EOleSysError.Create('', ErrorCode, 0);
  raise Exception.CreateFmt('OleError %d',[ErrorCode]);
end;

{ Raise EOleSysError exception if result code indicates an error }

procedure OleCheck(Result: HResult);
begin
  if not OleResult(Result) then OleError(Result);
end;


function FormatMaskText(const EditMask: string; const Value: string): string;
begin
  Result := maskedit.FormatMaskText(EditMask, Value);
end;

end.

