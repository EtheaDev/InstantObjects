unit Stopwatch;

interface

uses
  Classes, Controls;

type
  TStopwatch = class(TObject)
  private
    FCursor: TCursor;
    FStartTime, FStopTime: Cardinal;
    FStepIndex, FStepCount: Integer;
    FOnStart: TNotifyEvent;
    FOnStep: TNotifyEvent;
    FOnStop: TNotifyEvent;
    function GetElapsedTime: Cardinal;
    function GetStepPercent: Integer;
    function GetTotalTime: Cardinal;
  public
    procedure Reset;
    procedure Start(AStepCount: Integer);
    procedure Step;
    procedure Stop;
    property ElapsedTime: Cardinal read GetElapsedTime;
    property StepCount: Integer read FStepCount;
    property StepIndex: Integer read FStepIndex;
    property StepPercent: Integer read GetStepPercent;
    property TotalTime: Cardinal read GetTotalTime;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStep: TNotifyEvent read FOnStep write FOnStep;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation

uses
  Windows, Forms, Contnrs;

{ TStopwatch }

function TStopwatch.GetElapsedTime: Cardinal;
begin
  Result := GetTickCount - FStartTime;
end;

function TStopwatch.GetStepPercent: Integer;
begin
  Result := StepIndex * 100 div StepCount;
end;

function TStopwatch.GetTotalTime: Cardinal;
begin     
  Result := FStopTime - FStartTime;
end;

procedure TStopwatch.Reset;
begin
  FStartTime := 0;
  FStopTime := 0;
  FStepIndex := 0;
  FStepCount := 0;
end;

procedure TStopwatch.Start(AStepCount: Integer);
begin
  Application.ProcessMessages;
  Reset;
  FCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  FStepCount := AStepCount;
  if Assigned(FOnStart) then
    FOnStart(Self);
  FStartTime := GetTickCount;
end;

procedure TStopwatch.Step;
begin
  Inc(FStepIndex);
  if Assigned(FOnStep) then
    FOnStep(Self);
end;

procedure TStopwatch.Stop;
begin
  FStopTime := GetTickCount;
  Screen.Cursor := FCursor;
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

end.
