unit Primer.WiRL.Server.Listener;

interface

uses
  System.Classes,
  System.SysUtils,
  WiRL.http.Server
  ;

type
  TListener = class(TObject)
  private
    FServer: TWiRLServer;
    FEngineName: string;
    FPort: Integer;
    FName: string;
    FDisplayName: string;
    FCopyright: string;
    FAppPath: string;
    FBasePath: string;
    FConnectionFileName: string;
    FConfigFileName: string;
    function GetActive: Boolean;
    procedure SetPort(const Value: Integer);
    procedure LoadConfiguration;
    class function GetSingleton: TListener; static;
  public
    class property Singleton: TListener read GetSingleton;
    property ConfigFileName: string read FConfigFileName;
    property Name: string read FName;
    property EngineName: string read FEngineName;
    property Copyright: string read FCopyright;
    property AppPath: string read FAppPath;
    property BasePath: string read FBasePath;
    property DisplayName: string read FDisplayName;
    property ConnectionFileName: string read FConnectionFileName;
    property Server: TWiRLServer read FServer;
    property Port: Integer read FPort write SetPort;

    procedure Start;
    procedure Stop;
    property Active: Boolean read GetActive;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  WiRL.Configuration.Core,
  WiRL.Configuration.Compression,
  WiRL.Configuration.Auth,
  WiRL.Core.Classes,
  WiRL.Engine.Core,
  WiRL.Engine.REST,
  WiRL.Core.Application,
  WiRL.Core.MessageBody.Default,
  WiRL.Data.MessageBody.Default,
  WiRL.http.Server.Indy,
  WiRL.Core.JSON,
  WiRL.Rtti.Utils,
  WiRL.Data.FireDAC,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  JOSE.Core.JWA,
  System.IniFiles,
  //Neon support
  System.TypInfo,
  WiRL.Configuration.Neon,
  Neon.Core.Types,
  //JWT support
  WiRL.Configuration.JWT,
  Primer.WiRL.Server.Claims
  ;

const
  ENGINE_NAME = 'PrimerAPI';
  APP_NAME = 'WiRL Primer Demo';
  API_PATH = 'rest';
  BASE_PATH = '/primer';
  DISPLAY_NAME = 'WIRL - Primer Multiplatform Rest Server';
  REST_API_COPYRIGHT = 'CopyRight (c) Ethea S.r.l.';
  JWT_SECRET = 'DemoSuperSecretMustBeAtLeast256bit';

var
  _Listener: TListener;


{ TListener }

class function TListener.GetSingleton: TListener;
begin
  if not Assigned(_Listener) then
    _Listener := TListener.Create;
  Result := _Listener;
end;

constructor TListener.Create;
begin
  LoadConfiguration;
end;

destructor TListener.Destroy;
begin
  Stop;
  inherited;
  _Listener := nil;
end;

function TListener.GetActive: Boolean;
begin
  Result := Assigned(FServer);
end;

procedure TListener.LoadConfiguration;
var
  LIniFile: TIniFile;
  LConnectionNames, LConnectionParams: TStringList;
  LAppPath, LConnectionName: string;
begin
  LAppPath := ExtractFilePath(GetModuleName(MainInstance));
  FConfigFileName := LAppPath+'Primer.Wirl.Config.ini';
  LIniFile := TIniFile.Create(FConfigFileName);
  try
    FName := LIniFile.ReadString('Application', 'AppName', APP_NAME);
    FEngineName := LIniFile.ReadString('Application', 'EngineName', ENGINE_NAME);
    FDisplayName := LIniFile.Readstring('Application', 'DisplayName', DISPLAY_NAME);
    FCopyright := LIniFile.Readstring('Application', 'Copyright', REST_API_COPYRIGHT);
    FConnectionFileName := LIniFile.Readstring('Application', 'ConnectionFileName', 'FDConnections.ini');

    FPort := LIniFile.ReadInteger(EngineName, 'Port', 8080);
    FAppPath := LIniFile.Readstring(EngineName, 'APIPath', API_PATH);
    FBasePath := LIniFile.Readstring(EngineName, 'BasePath', BASE_PATH);
  finally
    LIniFile.Free;
  end;

  LIniFile := TIniFile.Create(LAppPath+FConnectionFileName);
  try
    //Add Configurations to FDManager
    LConnectionNames := nil;
    LConnectionParams := nil;
    try
      LConnectionNames := TStringList.Create;
      LConnectionParams := TStringList.Create;
      LIniFile.ReadSections(LConnectionNames);
      for LConnectionName in LConnectionNames do
      begin
        LIniFile.ReadSectionValues(LConnectionName, LConnectionParams);
        FDManager.AddConnectionDef(LConnectionName, LConnectionParams.Values['DriverID'], LConnectionParams);
      end;
    finally
      LConnectionNames.Free;
      LConnectionParams.Free;
    end;
  finally
    LIniFile.Free;
  end;
end;

procedure TListener.SetPort(const Value: Integer);
begin
  if Active then
    raise Exception.Create('Stop before change port');
  FPort := Value;
end;

procedure TListener.Start;
begin
  if Assigned(FServer) then
    Exit;

  FServer := TWiRLServer.Create(nil);

  // Server configuration
  FServer
    .SetPort(FPort)
    // Engine configuration
    .AddEngine<TWiRLRESTEngine>(FAppPath)
      .SetEngineName(FEngineName)

      // Application configuration
      .AddApplication(FBasePath)
        .SetAppName(FName)
        .SetFilters('*')
        .SetResources('*')

      // Compression PlugIn
      .Plugin.Configure<IWiRLConfigurationCompression>
        .SetMinSize(300)
        .SetMediaTypes('application/xml,application/json,text/plain')
        .ApplyConfig

      // Neon plugin configuration for generic Classes (No InstantObects)
      .Plugin.Configure<IWiRLConfigurationNeon>
        .SetUseUTCDate(True)
        .SetVisibility([mvPublic, mvPublished])
        .SetMemberCase(TNeonCase.PascalCase)
        .ApplyConfig

      // Auth configuration (App plugin configuration)
      .Plugin.Configure<IWiRLConfigurationAuth>
        .SetTokenType(TAuthTokenType.JWT)
        .SetTokenLocation(TAuthTokenLocation.Bearer)
        .ApplyConfig

      // JWT configuration (App plugin configuration)
      .Plugin.Configure<IWiRLConfigurationJWT>
        .SetClaimClass(TPrimerServerClaims)
        .SetAlgorithm(TJOSEAlgorithmId.HS256)
        .SetSecret(TEncoding.UTF8.GetBytes(JWT_SECRET))

  ;

  if not FServer.Active then
    FServer.Active := True;
end;

procedure TListener.Stop;
begin
  if not Assigned(FServer) then
    Exit;

  FServer.Active := False;
  FreeAndNil(FServer);
end;

end.
