(*
 *   InstantObjects
 *   Remote JSON Support
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
 * The Original Code is: Carlo Barazzetta
 *
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Contributor(s):
 * Carlo Barazzetta
 *
 * ***** END LICENSE BLOCK ***** *)
unit InstantRemoteJSON;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.Classes
  , System.SysUtils
  , Data.DB
  , InstantPersistence
  , InstantBrokers
  , InstantCommand
  , InstantMetadata
  , InstantTypes
  , InstantClasses
  , InstantJSON
  , REST.Client
  , REST.Authenticator.Basic
  , REST.Authenticator.OAuth;

type
  EInstantJSONRestClientError = Exception;

  { TInstantJSONRestClient }
  TInstantJSONRestClient = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FAuthToken: TOAuth2Authenticator;
    FBearerToken: string;
    procedure POST_AccessToken(const AUserName, APassword: string);
    function GET_InstantObject(const AClass, AId: string): TInstantObject;
    function POST_InstantObject(const AObject: TInstantObject): TInstantObject;
    function PUT_InstantObject(const AObject: TInstantObject): TInstantObject;
    function DELETE_InstantObject(const AObject: TInstantObject): TInstantObject;

    procedure RaiseError(const AOperation, AErrorMsg: string);
    procedure RaiseErrorFmt(const AOperation, AErrorMsg: string;
      const Args: array of const);
    procedure SetRestToken(const AToken: string);
  protected
    procedure ResetRestClient;
  public
    destructor Destroy;
    constructor Create(const ABaseURL: string;
      const AServiceKey, AEnvironment: string; AConnectTimeOut: Integer = 600000);
    procedure Login(const AUserName, APassword: string);
    function RetrieveInstantObjectList(const AClassName: string;
      const Where: string = ''; const OrderBy: string = ''): TInstantObjectList<TInstantObject>;
    function RetrieveInstantObject(const AClassName, AId: string): TInstantObject;
    function AddIntantObject(const AObject: TInstantObject): TInstantObject;
    procedure DeleteInstantObject(const AObject: TInstantObject);
    function UpdateInstantObject(const AObject: TInstantObject): TInstantObject;
  end;

implementation

uses
  System.StrUtils
  , REST.Types
  , System.JSON;

{ TInstantJSONRestClient }

procedure TInstantJSONRestClient.DeleteInstantObject(
  const AObject: TInstantObject);
begin
  ResetRestClient;
  try
    DELETE_InstantObject(AObject);
  except
    On E: Exception do
      RaiseError('DELETE_InstantObject', E.Message);
  end;
end;

function TInstantJSONRestClient.DELETE_InstantObject(
  const AObject: TInstantObject): TInstantObject;
begin
  ResetRestClient;
  try
    POST_InstantObject(AObject);
  except
    On E: Exception do
      RaiseError('POST_InstantObject', E.Message);
  end;
end;

function TInstantJSONRestClient.UpdateInstantObject(
  const AObject: TInstantObject): TInstantObject;
begin
  ResetRestClient;
  try
    PUT_InstantObject(AObject);
  except
    On E: Exception do
      RaiseError('PUT_InstantObject', E.Message);
  end;
end;

destructor TInstantJSONRestClient.Destroy;
begin
  FreeAndNil(FRESTClient);
  FreeAndNil(FRESTResponse);
  FreeAndNil(FRESTRequest);
  FreeAndNil(FAuthToken);
  inherited;
end;

function TInstantJSONRestClient.GET_InstantObject(const AClass,
  AId: string): TInstantObject;
begin
  ResetRestClient;
  Result := nil;
end;

procedure TInstantJSONRestClient.ResetRestClient;
begin
  FRESTClient.ResetToDefaults;
  FRESTRequest.ResetToDefaults;
  FRESTResponse.ResetToDefaults;
end;

function TInstantJSONRestClient.RetrieveInstantObject(const AClassName,
  AId: string): TInstantObject;
begin

end;

function TInstantJSONRestClient.RetrieveInstantObjectList(const AClassName,
  Where, OrderBy: string): TInstantObjectList<TInstantObject>;
begin

end;

function TInstantJSONRestClient.AddIntantObject(
  const AObject: TInstantObject): TInstantObject;
begin

end;

constructor TInstantJSONRestClient.Create(const ABaseURL: string;
  const AServiceKey, AEnvironment: string; AConnectTimeOut: Integer = 600000);
begin
  FRESTClient := TRESTClient.Create('');

  FRESTResponse := TRESTResponse.Create(nil);

  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.Client := FRESTClient;

  FAuthToken := TOAuth2Authenticator.Create(nil);
  FAuthToken.TokenType := TOAuth2TokenType.ttBEARER;

  FRESTClient.ResetToDefaults;
  FRESTClient.BaseURL := ABaseURL;
  FRESTClient.ReadTimeout := AConnectTimeOut;
  FRESTClient.ConnectTimeout := AConnectTimeOut;

  FRESTResponse.ResetToDefaults;

  FRESTRequest.ResetToDefaults;
  FRESTRequest.AssignedValues := [TCustomRESTRequest.TAssignedValue.rvReadTimeout];
  FRESTRequest.ReadTimeout := AConnectTimeOut;

  //Config RESTRequest
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.SynchronizedEvents := False;
  FRESTRequest.Params.Clear;
  FRESTRequest.Params.AddHeader('serviceKey', AServiceKey);
  FRESTRequest.Params.AddHeader('environment', AEnvironment);

  //Config Token
  FRESTClient.Authenticator := FAuthToken;
  if FBearerToken <> '' then
  begin
    FAuthToken.AccessToken := FBearerToken;
    FRESTRequest.Params.AddHeader('authorization', 'Bearer '+FBearerToken);
  end;
end;

procedure TInstantJSONRestClient.Login(const AUserName, APassword: string);
begin
  try
    POST_AccessToken(AuserName, APassword);
  Except
    On E: Exception do
      RaiseError('POST_AccessToken', E.Message);
  end;
end;

procedure TInstantJSONRestClient.POST_AccessToken(const AUserName, APassword: string);
begin
  ResetRestClient;

  FRESTRequest.Method := TRESTRequestMethod.rmPOST;
  FRESTRequest.Resource := 'token';

  FRESTRequest.AddParameter('username', AUserName, pkREQUESTBODY);
  FRESTRequest.AddParameter('password', APassword, pkREQUESTBODY);

  FRESTRequest.Execute;

  if Assigned(FRESTResponse.JSONValue) then
  begin
    var LJSONObject := FRESTResponse.JSONValue as TJSONObject;
    if not LJSONObject.TryGetValue<string>('Token', FBearerToken) then
      FBearerToken := '';
  end
  else
    FBearerToken := '';
end;

function TInstantJSONRestClient.POST_InstantObject(
  const AObject: TInstantObject): TInstantObject;
begin
  FRESTRequest.Method := TRESTRequestMethod.rmPOST;

  var LJSONObj : TJsonValue;
  //LJSONObj := TJsonObject.ParseJSONValue(ABody);
  FRESTRequest.Body.Add(LJSONObj as TJSONObject);

  FRESTRequest.Resource := '/frontend/search/AnagMaster';
  FRESTRequest.Execute;

  //Result := FRESTResponse.Content;
end;

function TInstantJSONRestClient.PUT_InstantObject(
  const AObject: TInstantObject): TInstantObject;
begin

end;

procedure TInstantJSONRestClient.RaiseError(const AOperation, AErrorMsg: string);
begin
  raise EInstantJSONRestClientError.CreateFmt(
    'Error calling "%s": "%s"', [AOperation, AErrorMsg]);
end;

procedure TInstantJSONRestClient.RaiseErrorFmt(const AOperation,
  AErrorMsg: string; const Args: array of const);
begin
  RaiseError(AOperation, Format(AErrorMsg, Args));
end;

procedure TInstantJSONRestClient.SetRestToken(const AToken: string);
begin
  //InitRest;
end;

end.

