unit Stopwatch;

interface

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Controls,
{$ENDIF}
{$IFDEF LINUX}
  QControls,
{$ENDIF}
  Classes;

type
  TStopwatch = class(TObject)
  private
    FCursor: TCursor;
    FStepIndex, FStepCount: Integer;
    FOnStart: TNotifyEvent;
    FOnStep: TNotifyEvent;
    FOnStop: TNotifyEvent;
{$IFDEF MSWINDOWS}
    FStartTime, FStopTime: Cardinal;
    function GetElapsedTime: Cardinal;
    function GetTotalTime: Cardinal;
{$ENDIF}
{$IFDEF LINUX}
    FStartTime, FStopTime: TDateTime;
    function GetElapsedTime: TDateTime;
    function GetTotalTime: TDateTime;
{$ENDIF}
    function GetStepPercent: Integer;
  public
    procedure Reset;
    procedure Start(AStepCount: Integer);
    procedure Step;
    procedure Stop;
{$IFDEF MSWINDOWS}
    property ElapsedTime: Cardinal read GetElapsedTime;
    property TotalTime: Cardinal read GetTotalTime;
{$ENDIF}
{$IFDEF LINUX}
    property ElapsedTime: TDateTime read GetElapsedTime;
    property TotalTime: TDateTime read GetTotalTime;
{$ENDIF}
    property StepCount: Integer read FStepCount;
    property StepIndex: Integer read FStepIndex;
    property StepPercent: Integer read GetStepPercent;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStep: TNotifyEvent read FOnStep write FOnStep;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, Forms,
{$ENDIF}
{$IFDEF LINUX}
  QForms,
{$ENDIF}
  Contnrs;


{ TStopwatch }

{$IFDEF MSWINDOWS}
function TStopwatch.GetElapsedTime: Cardinal;
begin
  Result := GetTickCount - FStartTime;
end;

function TStopwatch.GetTotalTime: Cardinal;
begin
  Result := FStopTime - FStartTime;
end;
{$ENDIF}
{$IFDEF LINUX}
function TStopwatch.GetElapsedTime: TDateTime;
begin
  Result := Now - FStartTime;
end;

function TStopwatch.GetTotalTime: TDateTime;
begin
  Result := FStopTime - FStartTime;
end;
{$ENDIF}

function TStopwatch.GetStepPercent: Integer;
begin
  Result := StepIndex * 100 div StepCount;
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
{$IFDEF MSWINDOWS}
  FStartTime := GetTickCount;
{$ENDIF}
{$IFDEF LINUX}
  FStartTime := Now;
{$ENDIF}
end;

procedure TStopwatch.Step;
begin
  Inc(FStepIndex);
  if Assigned(FOnStep) then
    FOnStep(Self);
end;

procedure TStopwatch.Stop;
begin
{$IFDEF MSWINDOWS}
  FStopTime := GetTickCount;
{$ENDIF}
{$IFDEF LINUX}
  FStopTime := Now;
{$ENDIF}
  Screen.Cursor := FCursor;
  if Assigned(FOnStop) then
    FOnStop(Self);
end;

end.
