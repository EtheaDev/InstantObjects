{*********************************************************}
{* FlashFiler: FFDBEnh.PAS 2.02                          *}
{* Copyright (c) TurboPower Software Co 1996-2001        *}
{* All rights reserved.                                  *}
{*********************************************************}
{* FlashFiler: TffTableEnh - provides RecNo support      *}
{*********************************************************}

{$I ffdefine.inc}

unit ffdb_enh;

interface
uses
  db,
  ffdb,
  classes,
  sysutils,
  windows,
  ffllbase;

type
  {The purpose of this table is to provide RecNo support. This support
   will allow grids the capability of displaying proportional
   scrollbars.}

  TffTableEnh = class(TffTable)
    protected
      teRecNo : TffWord32;
      function GetRecNo: Integer; override;
      function GetRecord(aBuffer  : PChar;
                         aGetMode : TGetMode;
                         aDoCheck : boolean): TGetResult; override;
      procedure InternalFirst; override;
      procedure InternalLast; override;
      procedure InternalOpen; override;
      procedure InternalSetToRecord(aBuffer: PChar); override;
      function btResetRange(aCursorID : TffCursorID;
                            SwallowSeqAccessError : Boolean) : Boolean; override;
    protected
    public
      function GetCurrentRecord(aBuffer : PChar) : boolean; override;
      function IsSequenced : boolean; override;
  end;

  TffQueryEnh = class(TffQuery)
    protected
      teRecNo : TffWord32;
      function GetRecNo: Integer; override;
      function GetRecord(aBuffer  : PChar;
                         aGetMode : TGetMode;
                         aDoCheck : boolean): TGetResult; override;
      procedure InternalFirst; override;
      procedure InternalLast; override;
      procedure InternalOpen; override;
      procedure InternalSetToRecord(aBuffer: PChar); override;
    protected
    public
      function GetCurrentRecord(aBuffer : PChar) : boolean; override;
      function IsSequenced : boolean; override;
  end;


implementation

type
  PDataSetRecInfo = ^TDataSetRecInfo;
  TDataSetRecInfo = packed record
    riBookmarkFlag : TBookmarkFlag;
    riRecNo : TffWord32;
  end;

{*** TffTableEnh ****************************************************}
function TffTableEnh.GetCurrentRecord(aBuffer: PChar): boolean;
begin
  Result := inherited GetCurrentRecord(aBuffer);

  if Result then
    teRecNo := PDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riRecNo;
end;
{--------}
function TffTableEnh.GetRecNo: Integer;
var
  Buffer : PChar;
begin
  Buffer := ActiveBuffer;
  Result := PDataSetRecInfo(Buffer + dsRecInfoOfs)^.riRecNo;
end;
{--------}
function TffTableEnh.GetRecord(aBuffer: PChar; aGetMode: TGetMode;
  aDoCheck: boolean): TGetResult;
begin
  Result := inherited GetRecord(aBuffer, aGetMode, aDoCheck);

  case aGetMode of
    gmNext  : if Result = grOK then Inc(teRecNo);
    gmPrior : if Result = grOK then Dec(teRecNo);
  end;

  if Result = grOK then
    with PDataSetRecInfo(aBuffer + dsRecInfoOfs)^ do begin
      riBookmarkFlag := bfCurrent;
      riRecNo := teRecNo;
    end;
end;
{--------}
procedure TffTableEnh.InternalFirst;
begin
  teRecNo := 0;

  inherited InternalFirst;
end;
{--------}
procedure TffTableEnh.InternalLast;
begin
  teRecNo := GetRecordCount + 1;

  inherited InternalLast;
end;
{--------}
procedure TffTableEnh.InternalOpen;
begin
  inherited InternalOpen;

  teRecNo := 0;
end;
{--------}
procedure TffTableEnh.InternalSetToRecord(aBuffer: PChar);
begin
  inherited InternalSetToRecord(aBuffer);

  teRecNo := PDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riRecNo;
end;
{--------}
function TffTableEnh.IsSequenced: boolean;
begin
  Result := True;
end;
{--------}
function TffTableEnh.btResetRange(aCursorID : TffCursorID;
                      SwallowSeqAccessError : Boolean) : Boolean;
begin
  Result := inherited btResetRange(aCursorID, SwallowSeqAccessError);

  if Result then
    teRecNo := 0;
end;
{********************************************************************}

{ TffQueryEnh }

function TffQueryEnh.GetCurrentRecord(aBuffer: PChar): boolean;
begin
  Result := inherited GetCurrentRecord(aBuffer);

  if Result then
    teRecNo := PDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riRecNo;
end;

function TffQueryEnh.GetRecNo: Integer;
var
  Buffer : PChar;
begin
  Buffer := ActiveBuffer;
  Result := PDataSetRecInfo(Buffer + dsRecInfoOfs)^.riRecNo;
end;

function TffQueryEnh.GetRecord(aBuffer: PChar; aGetMode: TGetMode;
  aDoCheck: boolean): TGetResult;
begin
  Result := inherited GetRecord(aBuffer, aGetMode, aDoCheck);

  case aGetMode of
    gmNext  : if Result = grOK then Inc(teRecNo);
    gmPrior : if Result = grOK then Dec(teRecNo);
  end;

  if Result = grOK then
    with PDataSetRecInfo(aBuffer + dsRecInfoOfs)^ do begin
      riBookmarkFlag := bfCurrent;
      riRecNo := teRecNo;
    end;
end;

procedure TffQueryEnh.InternalFirst;
begin
  teRecNo := 0;

  inherited InternalFirst;
end;

procedure TffQueryEnh.InternalLast;
begin
  teRecNo := GetRecordCount + 1;

  inherited InternalLast;
end;

procedure TffQueryEnh.InternalOpen;
begin
  inherited InternalOpen;

  teRecNo := 0;
end;

procedure TffQueryEnh.InternalSetToRecord(aBuffer: PChar);
begin
  inherited InternalSetToRecord(aBuffer);

  teRecNo := PDataSetRecInfo(aBuffer + dsRecInfoOfs)^.riRecNo;
end;

function TffQueryEnh.IsSequenced: boolean;
begin
  Result := True;
end;

end.

