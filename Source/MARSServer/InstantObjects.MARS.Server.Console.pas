(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.Console
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
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Contributor(s):
 * Carlo Barazzetta, Nicola Tambascia
 *
 * ***** END LICENSE BLOCK ***** *)
unit InstantObjects.MARS.Server.Console;

{$I MARS.inc}

interface

uses
  System.SysUtils
  , System.Types
  , IdHTTPWebBrokerBridge
  , IdSchedulerOfThreadPool
  , Web.WebReq
  , Web.WebBroker
  , IdContext
  ;

type
  TDummyIndyServer = class
  public
    procedure ParseAuthenticationHandler(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean); virtual;
  end;

procedure RunServer();

implementation

uses
  System.Rtti
  , System.Classes
  , MARS.Utils.Parameters
  , MARS.http.Server.Indy
  , InstantObjects.MARS.Server.Consts
  , Server.Ignition
  ;

var
  FServer: TMARShttpServerIndy;

procedure StartServer;
begin
  if not (FServer.Active) then
  begin
    Writeln(Format(sStartingServer, [FServer.Engine.Port, FServer.Engine.PortSSL]));
    FServer.Active := True;
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StopServer;
begin
  if FServer.Active  then
  begin
    Writeln(sStoppingServer);
    FServer.Active := False;
    Writeln(sServerStopped);
  end
  else
    Writeln(sServerNotRunning);
  Write(cArrow);
end;

procedure SetPort(const APort: string; IsSSL: Boolean);
var
  LPort: Integer;
  LWasActive: Boolean;
begin
  LPort := StrToIntDef(APort, -1);
  if LPort = -1 then
  begin
    Writeln('Port should be an integer number. Try again.');
    Exit;
  end;

  LWasActive := FServer.Active;
  if LWasActive  then
    StopServer;
  if not IsSSL then
    FServer.Engine.Port := LPort
  else
    FServer.Engine.PortSSL := LPort;
  if LWasActive then
    StartServer;
  Writeln(Format(sPortSet, [IntToStr(TServerEngine.Default.Port)]));
  Write(cArrow);
end;

procedure WriteEngineConfig;
var
  LValues: TStringList;
begin
  Writeln(sEngineParams);
  LValues := TStringList.Create;
  try
    LValues.Text := TServerEngine.Default.Parameters.AsStrings.text;
    LValues.Sort;
    Writeln(LValues.Text);
  finally
    LValues.Free;
  end;
end;

procedure WriteCommands;
begin
  Writeln(sCommands);
  Write(cArrow);
end;

procedure WriteStatus;
begin
  Writeln(sIndyVersion + FServer.SessionList.Version);
  Writeln(sActive + BoolToStr(FServer.Active, True));
  Writeln(sPort + IntToStr(FServer.Engine.Port));
  Writeln(sPortSSL + IntToStr(FServer.Engine.PortSSL));
  Write(cArrow);
end;

procedure SetupThreadScheduler(const AServer: TIdHTTPWebBrokerBridge);
var
  LScheduler: TIdSchedulerOfThreadPool;
begin
  LScheduler := TIdSchedulerOfThreadPool.Create(AServer);
  try
    LScheduler.PoolSize := TServerEngine.Default.ThreadPoolSize;
    AServer.Scheduler := LScheduler;
    AServer.MaxConnections := LScheduler.PoolSize;
  except
    AServer.Scheduler.DisposeOf;
    AServer.Scheduler := nil;
    raise;
  end;
end;

procedure RunServer();
var
  LResponse: string;
begin
  WriteEngineConfig;
  WriteCommands;

  // http server implementation
  FServer := TMARShttpServerIndy.Create(TServerEngine.Default);
  try
    while True do
    begin
      Readln(LResponse);
      LResponse := LowerCase(LResponse);
      if sametext(LResponse, cCommandStart) then
        StartServer
      else if sametext(LResponse, cCommandStatus) then
        WriteStatus
      else if sametext(LResponse, cCommandStop) then
        StopServer
{$ifdef DelphiXE3_UP}
      else if LResponse.StartsWith(cCommandSetPort, True) then
        SetPort(LResponse.Split([' '])[2], False)
      else if LResponse.StartsWith(cCommandSetSSLPort, True) then
        SetPort(LResponse.Split([' '])[2], True)
{$else}
      else if AnsiStartsText(cCommandSetPort, LResponse) then
        SetPort(Copy(LResponse, Length(cCommandSetPort)+1, MAXINT), False)
      else if AnsiStartsText(cCommandSetSSLPort, LResponse) then
        SetPort(Copy(LResponse, Length(cCommandSetPort)+1, MAXINT), True)
{$endif}

      else if sametext(LResponse, cCommandHelp) then
        WriteCommands
      else if sametext(LResponse, cCommandExit) then
        if FServer.Active then
        begin
          StopServer;
          break
        end
        else
          break
      else
      begin
        Writeln(sInvalidCommand);
        Write(cArrow);
      end;
    end;
  finally
    FServer.Free;
  end;

end;

{ TDummyIndyServer }

procedure TDummyIndyServer.ParseAuthenticationHandler(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
  // Allow JWT Bearer authentication's scheme
  if SameText(AAuthType, 'Bearer') then
    VHandled := True;
end;

end.
