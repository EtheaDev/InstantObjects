unit OFCritic;

interface

uses Windows, SysUtils, Classes, MMEngineDefs, MMToolsApi, MMCriticsBase;

type
  TObjectFoundryCritic = class(TMMDesignCritic, IUnknown, IMMDesignCritic)
  private
    FHeadline: string;
    FDescription: string;
    function GetRootID: Integer;
  public
    constructor Create;
    procedure Edit; override; safecall;
    function GetAuthor: WideString; override; safecall;
    function GetCriticName: WideString; override; safecall;
    function GetDescription: WideString; override; safecall;
    function GetHeadLine: WideString; override; safecall;
    procedure MsgDoubleClicked(const M: IMMMessage; var Handled: Boolean); override; safecall;
    procedure Refresh; override; safecall;
  end;

implementation

uses
  Dialogs, OFDefs;

const
  RequiredClassNameChars: array[Boolean] of Char = ('T', 'I');

constructor TObjectFoundryCritic.Create;
begin
  inherited Create;
  Category := 'ObjectFoundry';
end;

procedure TObjectFoundryCritic.Edit;
begin
end;

function TObjectFoundryCritic.GetAuthor: WideString;
begin
  Result := 'Seleqt Software';
end;

function TObjectFoundryCritic.GetCriticName: WideString;
begin
  Result := 'ObjectFoundry Critic';
end;

function TObjectFoundryCritic.GetDescription: WideString;
begin
  Result := FDescription;
end;

function TObjectFoundryCritic.GetHeadLine: WideString;
begin
  Result := FHeadline;
end;

procedure TObjectFoundryCritic.MsgDoubleClicked(const M: IMMMessage; var Handled:
    Boolean);
var
  Ref: IOFReference;
  ER: IOFEntityReference;
  C: IMMClassBase;
  R: IUnknown;
  RootID: Integer;
begin
  Handled := True;
  RootID := GetRootID;
  if RootID < 0 then
    Exit;
  Assert(Assigned(M));
  Ref := M.Reference;
  C := nil;
  if Ref.QueryInterface(IOFEntityReference, ER) = S_OK then
  begin
    R := ER.EntityRef;
    if Assigned(R) and (R.QueryInterface(IMMClassBase, C) = S_OK) then
      (C as IMMV9ClassBase).SetPersistency(cpAutoDetect);
  end;
end;

procedure TObjectFoundryCritic.Refresh;
var
  I: Integer;
  CM: IMMCodeModel;
  C: IMMClassBase;
  CName: string;
  Msg: IMMMessage;
  RootID: Integer;
begin
  CM := MMToolServices.CodeModel;
  MMToolServices.MessageServer.BeginUpdate;
  try
    MMToolServices.MessageServer.DeleteOwner(CriticID);
    RootID := GetRootID;
    if RootID < 0 then
    begin
      Msg := MMToolServices.MessageServer.CreateMessage(CriticID, MMCriticsContainer);
      Msg.HeadLine := 'Persistency Root class TInstantObject not found';
      Msg.Priority := Priority;
      Msg.Category := Category;
      Msg := nil;
      Exit;
    end;
    for I := 0 to CM.ClassCount - 1 do
    begin
      C := CM.Classes[I];
      if not (Assigned(C) and C.Valid) then Continue;
      CName := C.Name;
      if C.IsClass(RootID) <> (C as IMMV9ClassBase).IsPersistent then
      begin
        Msg := MMToolServices.MessageServer.CreateMessage(CriticID, MMCriticsContainer);
        Msg.HeadLine := Format('Persistency "%s" not according to framework conventions', [CName]);
        Msg.Priority := Priority;
        Msg.Category := Category;
        Msg.ReferToEntity(C);
        Msg := nil;
      end;
    end;
  finally
    MMToolServices.MessageServer.EndUpdate;
  end;
end;

function TObjectFoundryCritic.GetRootID: Integer;
var
  C: IMMClassBase;
begin
  C :=(MMToolServices.CodeModel as IMMV9CodeModel).ClassPersistencyRoot;
  if Assigned(C) and C.Valid and SameText(C.Name, 'TInstantObject') then
    Result := C.Id
  else
    Result := -1;
end;

end.
