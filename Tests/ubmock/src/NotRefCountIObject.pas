unit NotRefCountIObject;

interface

uses
  Classes, SysUtils;

type

  IFreeable = interface
     function GetObject: TObject;
  end;

  TNotRefCount = class(TInterfacedObject, IFreeable)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function GetObject: TObject;
  end;

  procedure FreeAndNilIntf(var AFreeable: IFreeable);

implementation

procedure FreeAndNilIntf(var AFreeable: IFreeable);
var
  o: TObject;
begin
  o := AFreeable.GetObject;
  AFreeable := nil;
  o.Free;
end;


{ TNotRefCountIObject }

function TNotRefCount._AddRef: Integer;
begin
  Result := -1; //nothing to do
end;

function TNotRefCount._Release: Integer;
begin
  Result := -1; //nothing to do
end;

function TNotRefCount.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := -1; //E_NOINTERFACE;
end;


function TNotRefCount.GetObject: TObject;
begin
  Result := Self;
end;

end.
