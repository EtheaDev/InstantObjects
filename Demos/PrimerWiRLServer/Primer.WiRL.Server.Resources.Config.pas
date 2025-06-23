(*
 *   InstantObject with WiRL REST Library
 *   Server.Resources.Config
 *)

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 2.0
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
unit Primer.WiRL.Server.Resources.Config;

interface

uses
  //Delphi
  System.SysUtils
  //WiRL
  , WiRL.Core.Attributes
  , WiRL.Core.JSON
  , WiRL.Core.Registry
  , WiRL.http.Accept.MediaType
  , WiRL.http.Server
  , WiRL.http.Client.Interfaces
  //InstantObjects Connection Context
  , InstantObjects.WiRL.Data
  //Primer Demo
  , Primer.WiRL.Server.Listener
  ;

type
  TWiRLServerConfig = class
  private
    FApplicationName: string;
    FApplicationVersion: string;
    FConfigFileName: string;
    FCopyRight: string;
    FServiceKeyParam: string;
    FEnvironmentParam: string;
    FDBConnectionMsg: string;
  public
    Constructor Create(
      const AConfigFileName, ASKeyParam, AEnvParam: string);
    procedure TestLogin;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property Version: string read FApplicationVersion write FApplicationVersion;
    property FileName: string read FConfigFileName write FConfigFileName;
    property ServiceKeyParam: string read FServiceKeyParam write FServiceKeyParam;
    property EnvironmentParam: string read FEnvironmentParam write FEnvironmentParam;
    property CopyRight: string read FCopyRight write FCopyRight;
    property DBConnectionMsg: string read FDBConnectionMsg write FDBConnectionMsg;
  end;

  [Path('/config')]
  TConfigResource = class
  protected
  public
    [Context] Request: IWiRLRequest;
    [Context] InstantObject: TWiRLInstantObjects;
    [Context] WiRLServer: TWiRLServer;
    [GET]
    [Produces(TMediaType.APPLICATION_JSON)]
    function Configuration([QueryParam]TestLogin: Boolean): TWiRLServerConfig;
  end;

implementation

uses
  InstantObjects.WiRL.Server.Consts
  , InstantObjects.WiRL.Server.Exceptions
  , FireDAC.Phys.MSSQL
  , Winapi.Windows
  ;

{ TConfigResource }

function TConfigResource.Configuration(TestLogin: Boolean): TWiRLServerConfig;
var
  LConfigFileName: string;
begin
  LConfigFileName := TListener.Singleton.ConfigFileName;

  Result := TWiRLServerConfig.Create(LConfigFileName,
    'ServiceKey', //Request.Headers.Values[PARAM_SERVICEKEY],
    'Test' //Request.Headers.Values[PARAM_ENVIRONMENT]
    );

  if TestLogin then
  begin
    try
      InstantObject.ConnectToDatabase;
      Result.FDBConnectionMsg := CONNECT_OK;
    except
      on E: EMSSQLNativeException do
      begin
        Result.FDBConnectionMsg := Format(CONNECT_KO, [E.Message]);
      end
      else
        raise;
    end;
  end;
end;

{ TWiRLServerConfig }

constructor TWiRLServerConfig.Create(
  const AConfigFileName, ASKeyParam, AEnvParam: string);
var
  fn: string;
  LMajorVersion, LMinorVersion, LRelease, LBuild: Integer;

  procedure GetVerInfo( const FileName : string;
    var MajorVersion, MinorVersion, Release, Build : integer);
  type
    cArray   = Array[1..$3FFF] of Char;
    TLangInf = Array[1..2]     of Word;      // Language and charset identifiers

  var
    InfoSize, Wnd: DWORD;
    VerBuf: Pointer;
    FI: PVSFixedFileInfo;
    VerSize: DWORD;

  begin
    MajorVersion := 0;
    MinorVersion := 0;
    Release := 0;
    Build := 0;

    InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
    if InfoSize > 0 then
    begin
      GetMem(VerBuf, InfoSize);
      try
        if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        begin
          if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          begin
            MajorVersion := HIWORD(FI.dwFileVersionMS);
            MinorVersion := LOWORD(FI.dwFileVersionMS);
            Release := HIWORD(FI.dwFileVersionLS);
            Build := LOWORD(FI.dwFileVersionLS);
          end;
        end;
      finally
        FreeMem(VerBuf);
      end;
    end;
  end;

begin
  inherited Create;
  fn := GetModuleName(HInstance);
  FApplicationName := TListener.Singleton.Name;
  GetVerInfo(fn, LMajorVersion, LMinorVersion, LRelease, LBuild);
  FApplicationVersion := Format('%d.%d.%d',[LMajorVersion, LMinorVersion, LRelease]);
  FCopyRight := TListener.Singleton.Copyright;
  FConfigFileName := AConfigFileName;
  FServiceKeyParam := ASKeyParam;
  FEnvironmentParam := AEnvParam;
  FDBConnectionMsg := NOT_CONNECTED;
end;

procedure TWiRLServerConfig.TestLogin;
begin
  ;
end;

initialization
  TWiRLResourceRegistry.Instance.RegisterResource<TConfigResource>;

end.
