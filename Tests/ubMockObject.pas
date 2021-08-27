unit UbMockObject;

interface

uses
  Classes
  , SysUtils
  , NotRefCountIObject
  , DUnitX.TestFramework;

type

  IUbMockObject = interface
    procedure Verify;
    procedure StartSetUp;
    procedure EndSetUp;
    function UncoveredExpectations: integer;
  end;

  TUbMockObject = class(TNotRefCount, IUbMockObject)
  protected

  public
    FSetUpMode: Boolean;
    FSetUpList: TStringList;
    FCallsList: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddExpectation(const ASignatureCall: string);
    property SetUpMode: Boolean read FSetUpMode;

    procedure Verify;
    procedure StartSetUp;
    procedure EndSetUp;
    function UncoveredExpectations: integer;
  end;

implementation

uses
  InstantTest;

{ TUbMockObject }

procedure TUbMockObject.AddExpectation(const ASignatureCall: string);
begin
  if SetUpMode then
    FSetUpList.Add(ASignatureCall)
  else
    FCallsList.Add(ASignatureCall);
end;

function TUbMockObject.UncoveredExpectations: integer;
begin
  Result := FSetUpList.Count - FCallsList.Count;
end;

constructor TUbMockObject.Create;
begin
  FSetUpList := TStringList.Create;
  FCallsList := TStringList.Create;
  FSetUpMode := True;
end;

destructor TUbMockObject.Destroy;
begin
  FSetUpList.Free;
  FCallsList.Free;

  inherited;
end;

procedure TUbMockObject.EndSetUp;
begin
  FSetUpMode := False;
  FCallsList.Clear;
end;

procedure TUbMockObject.StartSetUp;
begin
  FSetUpMode := True;
  FSetUpList.Clear;
end;

procedure TUbMockObject.Verify;
var
  i: integer;
  s1, s2: string;
begin
  Assert.IsTrue(FSetUpList.Count = FCallsList.Count, 'Wrong Expectation number!');

  for i := 0 to FSetUpList.Count - 1 do
  begin
    s1 := FSetUpList[i];
    s2 := FCallsList[i];
    Assert.IsTrue(s1 = s2);
  end;

  FSetUpList.Clear;
  FCallsList.Clear;
end;

end.
