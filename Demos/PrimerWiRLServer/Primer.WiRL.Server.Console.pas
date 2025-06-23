unit Primer.WiRL.Server.Console;

interface

uses
  System.SysUtils
  , System.Classes
  , System.SyncObjs
  , Primer.WiRL.Server.Listener
  , Primer.WiRL.Server.Filters.Logger
  ;

type
  IRunnable = interface
    ['{B632E38C-96C5-4D0B-BD8E-162BFA616473}']
    procedure Execute;
  end;

  TConsole = class(TInterfacedObject, ILogger, IRunnable)
  private
    { IRunnable }
    procedure Execute;

    { ILogger }
    procedure Log(const AMessage: string);
  public
    class procedure Run; static;
  end;

implementation

{ TConsole }

procedure TConsole.Execute;
var
  LListener: TListener;
begin
  RegisterLogger(Self);

  LListener := TListener.Singleton;
  try
    LListener.Start;

    Writeln(LListener.Name);
    Writeln('_____          _              _   _____ _     _           _                  _    _ _______ _ ');
    Writeln('_   _|        | |            | | |  _  | |   (_)         | |                | |  | (_) ___ \ |');
    Writeln(' | | _ __  ___| |_ __ _ _ __ | |_| | | | |__  _  ___  ___| |_ ___   ______  | |  | |_| |_/ / |');
    Writeln(' | || ''_ \/ __| __/ _'' | ''_ \| __| | | | ''_ \| |/ _ \/ __| __/ __| |______| | |/\| | |    /| |');
    Writeln('_| || | | \__ \ || (_| | | | | |_\ \_/ / |_) | |  __/ (__| |_\__ \          \  /\  / | |\ \| |');
    Writeln('\___/_| |_|___/\__\__,_|_| |_|\__|\___/|_.__/| |\___|\___|\__|___/           \/  \/|_\_| \_|_|');
    Writeln('                                            _/ |                                              ');
    Writeln('                                           |__/                                               ');
    Writeln(LListener.DisplayName);
    Writeln(Format('http://localhost:%d/%s/%s',
      [LListener.Port, LListener.AppPath, LListener.BasePath]));
    Writeln('Press [ENTER] to exit');
    Writeln('');
    Readln;
    LListener.Stop;
  finally
    LListener.Free;
  end;
  UnregisterLogger(Self);
end;

procedure TConsole.Log(const AMessage: string);
begin
   TMonitor.Enter(Self);
   try
      Writeln(AMessage);
   finally
     TMonitor.Exit(Self);
   end;
end;

class procedure TConsole.Run;
var
  LRunnable: IRunnable;
begin
  try
    LRunnable := TConsole.Create;
    LRunnable.Execute;
  except
    on E: Exception do
      Writeln('Error: ' + E.Message);
  end;
end;

end.
