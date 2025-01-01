(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.Resources.Token Example
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
unit Primer.MARS.Server.Resources.Token;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  MARS.Core.Registry,
  MARS.Core.Attributes,
  MARS.Core.MediaType,
  MARS.Core.JSON,
  MARS.Core.MessageBodyWriters,
  MARS.Core.MessageBodyReaders,
  MARS.Core.Token.Resource,
  MARS.Core.RequestAndResponse.Interfaces,
  InstantObjects.MARS.Data,
  InstantPersistence,
  InstantObjects.MARS.Server.Consts,
  Model;

const
  CLAIM_ENVIRONMENT   = 'ENVIRONMENT';
  CLAIM_PROFILE_ID    = 'PROFILE_ID';
  CLAIM_LANGUAGE_ID   = 'LANGUAGE_ID';
  CLAIM_LAST_NAME     = 'LAST_NAME';
  CLAIM_FIRST_NAME    = 'FIRST_NAME';
  CLAIM_EMAIL_ADDRESS = 'EMAIL_ADDRESS';
type
  [Path('token'), Produces(TMediaType.APPLICATION_JSON)]
  TJWTTokenResource = class(TMARSTokenResource)
  private
  protected
    [Context] InstantObject: TMARSInstantObjects;
    [Context] Request: IMARSRequest;
    function Authenticate(const AUserName, APassword: string): Boolean; override;
    procedure BeforeLogin(const AUserName, APassword: string); override;
    procedure AfterLogin(const AUserName, APassword: string); override;
    procedure BeforeLogout(); override;
    procedure AfterLogout(); override;
  public
  end;

implementation

uses
  System.StrUtils,
  System.Hash,
  System.JSON,
  System.TypInfo,
  System.Variants,
  MARS.Core.Response,
  MARS.Core.Exceptions,
  InstantObjects.MARS.Server.Exceptions,
  InstantFireDAC,
  InstantClasses,
  InstantMetaData,
  InstantObjects.MARS.Server.Resources.Base;

{ TJWTTokenResource }

procedure TJWTTokenResource.AfterLogin(const AUserName,
  APassword: string);
begin
  inherited;
end;

procedure TJWTTokenResource.AfterLogout;
begin
  inherited;

end;

function TJWTTokenResource.Authenticate(const AUserName,
  APassword: string): Boolean;
var
  LUser: TUser;
  LPasswordHash: string;
begin
  InstantObject.ConnectToDatabase;
  LUser := TUser.Retrieve(AUserName, false, false, InstantObject.Connector);
  try
    if LUser = nil then
    begin
        raise EMARSServerException.CreateError(http_400_Bad_Request,
         WRONG_AUTH,
         'UserName/Password');
    end
    else
    begin
      LPasswordHash := THashMD5.GetHashString(APassword);
      if (LPasswordHash <> LUser.Password) and (APassword <> LUser.Password) then
        raise EMARSServerException.CreateError(http_406_NotAcceptable,
         WRONG_AUTH,
         'UserName/Password');
    end;
    Result := True;

    if Result then
    begin
      Token.UserName := AUserName;
      if LUser.Administrator and LUser.System then
        Token.Roles := TArray<string>.Create('reader', 'standard', 'admin', 'system')
      else if LUser.Administrator then
        Token.Roles := TArray<string>.Create('reader', 'standard', 'admin')
      else if LUser.System then
        Token.Roles := TArray<string>.Create('reader', 'standard', 'system')
      else if LUser.Profile.Id = 'READONLY' then
        Token.Roles := TArray<string>.Create('reader')
      else
        Token.Roles := TArray<string>.Create('reader', 'standard');
      //Al token aggiunto l'environment che verrà verificato ad ogni chiamata
      Token.Claims.Values[CLAIM_ENVIRONMENT] := Request.GetHeaderParamValue(PARAM_ENVIRONMENT);
      Token.Claims.Values[CLAIM_PROFILE_ID] := LUser.Profile.Id;
      if LUser.Language <> '' then
        Token.Claims.Values[CLAIM_LANGUAGE_ID] := LUser.Language
      else
        Token.Claims.Values[CLAIM_LANGUAGE_ID] := 'IT';
      Token.Claims.Values[CLAIM_LAST_NAME] := LUser.LastName;
      Token.Claims.Values[CLAIM_FIRST_NAME] := LUser.FirstName;
      Token.Claims.Values[CLAIM_EMAIL_ADDRESS] := LUser.Email;
    end;
  finally
    LUser.Free;
  end;
end;

procedure TJWTTokenResource.BeforeLogin(const AUserName,
  APassword: string);
begin
  inherited;
end;

procedure TJWTTokenResource.BeforeLogout;
begin
  inherited;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TJWTTokenResource>;

end.
