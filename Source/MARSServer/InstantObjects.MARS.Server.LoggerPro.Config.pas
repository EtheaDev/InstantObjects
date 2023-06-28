(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.LoggerPro.Config
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
unit InstantObjects.MARS.Server.LoggerPro.Config;

interface

uses
  LoggerPro;

function Log: ILogWriter;

implementation

uses
  WinAPI.Windows,
  System.SysUtils,
  System.IOUtils,
  LoggerPro.FileAppender;

var
  _Log: ILogWriter;

function Log: ILogWriter;
begin
  Result := _Log;
end;

function GetFileNameFormat: string;
var
  lLogFileNameFormat: string;
begin
  lLogFileNameFormat := TLoggerProFileAppender.DEFAULT_FILENAME_FORMAT;
  // '%s.%2.2d.%s.log';
  var lClientName: string := GetEnvironmentVariable('CLIENTNAME');
  var lComputerName: string := GetEnvironmentVariable('COMPUTERNAME');
  if not lClientName.IsEmpty then
  begin
    Exit('LOG_' + lClientName + '_' + lLogFileNameFormat);
  end;
  if not lComputerName.IsEmpty then
  begin
    Exit('LOG_' + lComputerName + '_' + lLogFileNameFormat);
  end;
  Result := 'LOG_' + GetProcessId(HInstance).ToString + '_' + lLogFileNameFormat;
end;

initialization
{ The TLoggerProFileAppender has its defaults defined as follows:
  DEFAULT_LOG_FORMAT = '%0:s [TID %1:-8d][%2:-10s] %3:s [%4:s]';
  DEFAULT_MAX_BACKUP_FILE_COUNT = 5;
  DEFAULT_MAX_FILE_SIZE_KB = 1000;
  You can override these dafaults passing parameters to the constructor.
  Here's some configuration examples:
  @longcode(#
  // Creates log in the same exe folder without PID in the filename
  _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
  [TFileAppenderOption.LogsInTheSameFolder])]);
  // Creates log in the AppData/Roaming with PID in the filename
  _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
  [TFileAppenderOption.IncludePID])]);
  // Creates log in the same folder with PID in the filename
  _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
  [TFileAppenderOption.IncludePID])]);
  #)
}
// Creates log in the ..\..\ folder without PID in the filename
var lLogsFolder: string := TPath.Combine(TDirectory.GetParent(GetModuleName(HInstance)), 'logs');
_Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 2048, lLogsFolder {'.\logs'}, [], GetFileNameFormat())]);
// Create logs in the exe' same folder
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5)]);
// Creates log in the AppData/Roaming with PID in the filename
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
// [TFileAppenderOption.IncludePID])]);
// Creates log in the same folder with PID in the filename
// _Log := BuildLogWriter([TLoggerProFileAppender.Create(10, 5,
// [TFileAppenderOption.IncludePID])]);
end.
