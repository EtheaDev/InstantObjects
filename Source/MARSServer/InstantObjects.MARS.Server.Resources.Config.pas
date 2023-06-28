(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.Resources.Config
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
unit InstantObjects.MARS.Server.Resources.Config;

interface

uses
  //Delphi
  System.SysUtils
  //MARS
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.Registry
  , MARS.Core.Engine
  , MARS.Core.RequestAndResponse.Interfaces
  //InstantObjects.MARS
  , InstantObjects.MARS.Data
  ;

type
  TMARSServerConfig = class
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
    property ApplicationName: string read FApplicationName;
    property Version: string read FApplicationVersion;
    property FileName: string read FConfigFileName;
    property ServiceKeyParam: string read FServiceKeyParam;
    property EnvironmentParam: string read FEnvironmentParam;
    property CopyRight: string read FCopyRight;
    property DBConnectionMsg: string read FDBConnectionMsg;
  end;

  [Path('/config'), Produces(TMediaType.APPLICATION_JSON)]
  TConfigResource = class
    [Context] Request: IMARSRequest;
    [Context] InstantObject: TMARSInstantObjects;
  public
    [GET]
    function Configuration([QueryParam]TestLogin: Boolean): TMARSServerConfig;
  end;

implementation

uses
  InstantObjects.MARS.Server.Consts
  , InstantObjects.MARS.Server.Exceptions
  , FireDAC.Phys.MSSQL
  , Server.Ignition //This unit is specific for the Server Project
  , Winapi.Windows
  ;

{ TConfigResource }

function TConfigResource.Configuration(TestLogin: Boolean): TMARSServerConfig;
var
  LConfigFileName: string;
begin
  LConfigFileName := ChangeFileExt(GetModuleName(HInstance), '.ini');
  if not FileExists(LConfigFileName) then
    raise EMARSServerException.CreateError(
      http_500_InternalServerError,
      Format('Config file "%s" not found!', [LConfigFileName]));

  Result := TMARSServerConfig.Create(LConfigFileName,
    Request.GetHeaderParamValue(PARAM_SERVICEKEY),
    Request.GetHeaderParamValue(PARAM_ENVIRONMENT)
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

{ TMARSServerConfig }

constructor TMARSServerConfig.Create(
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
  FApplicationName := REST_API_NAME;
  GetVerInfo(fn, LMajorVersion, LMinorVersion, LRelease, LBuild);
  FApplicationVersion := Format('%d.%d.%d',[LMajorVersion, LMinorVersion, LRelease]);
  FCopyRight := REST_API_COPYRIGHT;
  FConfigFileName := AConfigFileName;
  FServiceKeyParam := ASKeyParam;
  FEnvironmentParam := AEnvParam;
  FDBConnectionMsg := NOT_CONNECTED;
end;

procedure TMARSServerConfig.TestLogin;
begin
  ;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TConfigResource>;

end.
