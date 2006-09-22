unit TestIMock;

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  UBMockObject;

type

  TObjectToMock = class(TObject, IUbMockObject)
  private
    FCCC: string;
    FMockManager: TUbMockObject;
    procedure SetCCC(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    function AAA: boolean;
    function BBB(int: integer): string;
    property CCC: string read FCCC write SetCCC;
    property MockManager: TUbMockObject read FMockManager implements IUbMockObject;
  end;

  TTestIMock = class(TTestCase)
  protected
    _MockObj: TObjectToMock;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetUp;
    procedure TestMethods;
    procedure TestFailParams;
  end;


implementation

{ TObjectToMock }


function TObjectToMock.AAA: boolean;
begin
  result := True;
  FMockManager.AddExpectation('AAA');
end;

function TObjectToMock.BBB(int: integer): string;
begin
  result := IntToStr(int);
  FMockManager.AddExpectation('BBB'); //ignore the param
end;

constructor TObjectToMock.Create;
begin
  FMockManager := TUbMockObject.Create;
end;

destructor TObjectToMock.Destroy;
begin
  FMockManager.Free;
  inherited;
end;

procedure TObjectToMock.SetCCC(const Value: string);
begin
  FCCC := Value;
  FMockManager.AddExpectation('SetCCC := ' + Value); //check also the value
end;


{ TTestIMock }

procedure TTestIMock.SetUp;
begin
  inherited;
  _MockObj := TObjectToMock.Create;
end;

procedure TTestIMock.TearDown;
begin
  _MockObj.Free;
  inherited;
end;


procedure TTestIMock.TestFailParams;
begin
  _MockObj.BBB(1);
  _MockObj.BBB(2);
  _MockObj.CCC := '??';
  _MockObj.MockManager.EndSetUp;
  _MockObj.BBB(1);
  _MockObj.BBB(3); //
  _MockObj.CCC := '!!!';
  AssertEquals(0,_MockObj.MockManager.UncoveredExpectations);
  AssertException(EAssertionFailedError, _MockObj.MockManager.Verify);
end;

procedure TTestIMock.TestMethods;
begin
  _MockObj.AAA;
  _MockObj.BBB(2);
  _MockObj.AAA;
  _MockObj.CCC := 'test';
  _MockObj.MockManager.EndSetUp;
  AssertEquals(4,_MockObj.MockManager.UncoveredExpectations);
  _MockObj.AAA;
  _MockObj.BBB(2);
  _MockObj.AAA;
  _MockObj.CCC := 'test';
  AssertEquals(0,_MockObj.MockManager.UncoveredExpectations);
  _MockObj.MockManager.Verify;
end;

procedure TTestIMock.TestSetUp;
begin
  AssertNotNull(_MockObj);
end;

initialization
  RegisterTests([TTestIMock]);
end.
