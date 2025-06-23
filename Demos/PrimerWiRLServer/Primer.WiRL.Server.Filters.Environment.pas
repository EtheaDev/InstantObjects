{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2019 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Primer.WiRL.Server.Filters.Environment;

interface

uses
  System.SysUtils, System.Classes,

  WiRL.Core.Registry,
  WiRL.http.Filters,
  WiRL.http.Request,
  WiRL.Core.Attributes,
  WiRL.Core.Exceptions,
  WiRL.Core.Auth.Context,
  WiRL.Core.Application,
  WiRL.http.Accept.MediaType;

type
  /// <summary>
  ///   Environment Filter. This filter check if the Request contains
  ///   the same Environment and ServiceKey header params
  ///   used during Authentication process
  /// </summary>
  TRequestEnvironmentFilter = class(TInterfacedObject, IWiRLContainerRequestFilter)
  protected
    // Injects the auth context into the "Auth" object
    [Context] Auth: TWiRLAuthContext;
    procedure Filter(ARequestContext: TWiRLContainerRequestContext);
  end;

implementation

uses
  Server.Claims
  , InstantObjects.WiRL.Server.Consts
  , InstantObjects.WiRL.Server.Exceptions
  ;

{ TRequestEnvironmentFilter }

procedure TRequestEnvironmentFilter.Filter(ARequestContext: TWiRLContainerRequestContext);
begin
  if Assigned(Auth) and (Auth.Verified) then
  begin
    {$IFNDEF BYPASS_TOKEN}
    var LClaims := Auth.Subject as TPrimerServerClaims;
    var LTokenEnvironment := LClaims.Environment;
    var LTokenServiceKey := LClaims.ServiceKey;
    {$ELSE}
    var LTokenEnvironment := 'test';
    var LTokenServiceKey := 'frontend';
    {$ENDIF}
    var LEnvironment := ARequestContext.Request.Headers[PARAM_ENVIRONMENT];
    var LServiceKey := ARequestContext.Request.Headers[PARAM_SERVICEKEY];

    //check that the Request environment param is the same stored in Auth Token
    if not SameText(LTokenEnvironment,LEnvironment) then
      raise EWiRLServerException.CreateError(http_406_NotAcceptable,
        Format('%s header param "%s" not equal to token.environment "%s".',
         [PARAM_ENVIRONMENT, LEnvironment, LTokenEnvironment]));
    (*
    //check that the Request servicekey param is the same stored in Auth Token
    if not SameText(LTokenServiceKey,LServiceKey) then
      raise EWiRLServerException.CreateError(http_406_NotAcceptable,
        Format('%s header param "%s" not equal to token.servicekey "%s".',
         [PARAM_SERVICEKEY, LServiceKey, LTokenServiceKey]));
    *)
  end;
end;

initialization

TWiRLFilterRegistry.Instance.RegisterFilter<TRequestEnvironmentFilter>();

end.
