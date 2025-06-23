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
 * Carlo Barazzetta
 *
 * ***** END LICENSE BLOCK ***** *)
unit Primer.WiRL.Server.Resources.User;

interface

uses
  //Delphi
  System.Classes
  , System.SysUtils
  , Generics.Collections
  , System.JSON
  //WiRL
  , WiRL.Core.JSON
  , WiRL.Core.Registry
  , WiRL.Core.Attributes
  , WiRL.http.Accept.MediaType
  , WiRL.Core.Auth.Resource
  , WiRL.Core.Auth.Context
  , WiRL.http.Request
  //WiRL InstantObjects Server and Data
  , InstantObjects.WiRL.Server.Consts
  , InstantObjects.WiRL.Server.Resources
  , InstantObjects.WiRL.Data
  // Only if you want to use a custom (claims) class
  , Server.Claims
  //InstantObjects
  , InstantPersistence
  //Model Units
  , Model
  ;

type
  TDetailsInfo = class
  private
    FClaims: TPrimerServerClaims;
    FMessage: string;
  public
    constructor Create(const AMessage: string; AClaims: TPrimerServerClaims);
    property Message: string read FMessage write FMessage;
    property Claims: TPrimerServerClaims read FClaims write FClaims;
  end;

  TChangePassword = record
    OldPassword: string;
    NewPassword: string;
  end;

  // *********************************************************************
  // Inherit (only one) Auth resource from the base class you want to use:
  // *********************************************************************

  [Path('basic_auth')]
  TBasicAuthResource = class(TWiRLAuthBasicResource)
  private
    // Injects the custom claims into "Subject" field
    [Context] Subject: TPrimerServerClaims;
    // Injects InstantObject Connection
    [Context] InstantObject: TWiRLInstantObjects;
    // Injects http Request
    [Context] Request: TWiRLRequest;
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  end;

  [Path('form_auth')]
  TFormAuthResource = class(TWiRLAuthFormResource)
  private
    // Injects the custom claims into "Subject" field
    [Context] Subject: TPrimerServerClaims;
    // Injects InstantObject Connection
    [Context] InstantObject: TWiRLInstantObjects;
    // Injects http Request
    [Context] Request: TWiRLRequest;
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  end;

  [Path('body_auth')]
  TBodyAuthResource = class(TWiRLAuthBodyResource)
  private
    // Injects the custom claims into "Subject" field
    [Context] Subject: TPrimerServerClaims;
    // Injects InstantObject Connection
    [Context] InstantObject: TWiRLInstantObjects;
    // Injects http Request
    [Context] Request: TWiRLRequest;
  protected
    function Authenticate(const AUserName, APassword: string): TWiRLAuthResult; override;
  end;

  [Path('TUser')]
  TUserResource = class(TInstantObjectResource)
  protected
    // Injects the auth context into the "Auth" object
    [Context] Auth: TWiRLAuthContext;
    // Injects the custom claims into "Subject" object
    [Context] Subject: TPrimerServerClaims;

    //function to "filter" accepted ClassName for requests
    procedure AcceptedClassName(const AClassName: string;
      var AAccepted: boolean); override;
  public
    [POST, Path('/ChangePassword')
    , Consumes(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('standard'){$ENDIF}]
    function UserChangePassword(
      [FormParam('oldpassword')] const AOldPassword: string;
      [FormParam('newpassword')] const ANewPassword: string): string;

    [POST, Path('/{AId}')
    , Consumes(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('system'){$ENDIF}
    ]
    function PostUser([PathParam] const AId: string;
      [BodyParam] AUser: TUser): TUser;

    [PUT, Path('/{AId}')
    , Consumes(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('system'){$ENDIF}
    ]
    function PutUser([PathParam] const AId: string;
      [BodyParam] AUser: TUser): TUser;

    [DELETE, Path('/{AId}')
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('system'){$ENDIF}
    ]
    function DeleteUser([PathParam] AId: string): TUser;

    [GET, Path('/')
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('system'){$ENDIF}
    ]
    function RetrieveUsersList(
      [QueryParam] Where: string = '';
      [QueryParam] OrderBy: string = '';
      [QueryParam] ChangedFrom: string = '';
      [QueryParam] ChangedTo: string = ''): TInstantObjectList<TInstantObject>;

    [GET, Path('/{AId}')
    , Produces(TMediaType.APPLICATION_JSON)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('standard'){$ENDIF}
    ]
    function RetrieveUser([PathParam] AId: string): TUser;

  end;

function CheckPassword(const APassword: string; out EMsg: string): boolean;

implementation

uses
  //Delphi
  System.StrUtils
  , System.DateUtils
  , System.TypInfo
  , System.Variants
  , System.RegularExpressions
  , System.Hash
  //WiRL
  , WiRL.Core.Exceptions
  , WiRL.Data.FireDAC
  //WiRL Server
  , InstantObjects.WiRL.Server.Exceptions
  , InstantObjects.WiRL.Server.Resources.Utils
  //InstantObject
  , InstantFireDAC
  , InstantClasses
  , InstantMetaData
  ;


//Global auth procedure
function UserAuthenticate(
  const Subject: TPrimerServerClaims;
  const InstantObject: TWiRLInstantObjects;
  const Request: TWiRLRequest;
  const AUserName, APassword: string): TWiRLAuthResult;
var
  LUser: TUser;
  LPasswordHash: string;
begin
  InstantObject.ConnectToDatabase;
  //Check UserName and Password
  LUser := TUser.Retrieve(AUserName, false, false, InstantObject.Connector);
  try
    if LUser = nil then
    begin
        raise EWiRLServerException.CreateError(http_400_Bad_Request,
         WRONG_AUTH,
         'UserName/Password');
    end
    else
    begin
      LPasswordHash := THashMD5.GetHashString(APassword);
      if (LPasswordHash <> LUser.Password) and (APassword <> LUser.Password) then
        raise EWiRLServerException.CreateError(http_406_NotAcceptable,
         WRONG_AUTH,
         'UserName/Password');
    end;

    //UserName and Password Passed!
    Result.Success := True;
    Subject.UserID := AUserName;

    //Set Roles based on User Object
    if LUser.System then
      Result.Roles := TArray<string>.Create('reader', 'standard', 'admin', 'system')
    else if LUser.Administrator then
      Result.Roles := TArray<string>.Create('reader', 'standard', 'admin')
    else if LUser.System then
      Result.Roles := TArray<string>.Create('reader', 'standard', 'system')
    else if LUser.Profile.Id = 'READONLY' then
      Result.Roles := TArray<string>.Create('reader')
    else
      Result.Roles := TArray<string>.Create('reader', 'standard');

    // Here you can set all custom claims of the JWT
    // Claims from User
    Subject.ProfileId := LUser.Profile.Id;
    if LUser.Language <> '' then
      Subject.LanguageId := LUser.Language
    else
      Subject.LanguageId := 'en';
    Subject.LastName := LUser.LastName;
    Subject.FirstName := LUser.FirstName;
    Subject.EmailAddress := LUser.Email;

    //Claims from Request
    Subject.Environment := Request.Headers[PARAM_ENVIRONMENT];
    Subject.ServiceKey := Request.Headers[PARAM_SERVICEKEY];
  finally
    LUser.Free;
  end;

  //Set Expiration of Token
  Subject.Expiration := IncSecond(Now(), 30);
end;


{ TBasicAuthResource }

function TBasicAuthResource.Authenticate(const AUserName, APassword: string): TWiRLAuthResult;
begin
  Result := UserAuthenticate(Subject, InstantObject, Request, AUserName, APassword);
end;

{ TFormAuthResource }

function TFormAuthResource.Authenticate(const AUserName, APassword: string): TWiRLAuthResult;
begin
  Result := UserAuthenticate(Subject, InstantObject, Request, AUserName, APassword);
end;

{ TBodyAuthResource }

function TBodyAuthResource.Authenticate(const AUserName, APassword: string): TWiRLAuthResult;
begin
  Result := UserAuthenticate(Subject, InstantObject, Request, AUserName, APassword);
end;

function CheckPassword(const APassword: string; out EMsg: string): boolean;

  procedure AddErrMsg(const AErrMsg: string);
  begin
    if EMsg = '' then
      EMsg := AErrMsg
    else
      EMsg := EMsg+sLineBreak+AErrMsg
  end;
begin

  EMsg := '';

  if Length(APassword) < 8 then
    AddErrMsg(PWD_MIN_8_CHARS);

  if not TRegEx.IsMatch(APassword, '[a-z]') then
    AddErrMsg(PWD_MUST_CONTAIN_LOWER_CHAR);

  if not TRegEx.IsMatch(APassword, '[A-Z]') then
    AddErrMsg(PWD_MUST_CONTAIN_UPPER_CHAR);

  if not TRegEx.IsMatch(APassword, '\d') then
    AddErrMsg(PWD_MUST_CONTAIN_NUM_CHAR);

  if not TRegEx.IsMatch(APassword, '[!,#,%,&,*,@]') then
    AddErrMsg(PWD_MUST_CONTAIN_SPECIAL_CHAR);

  Result := EMsg = '';
end;

{ TUserResource }

procedure TUserResource.AcceptedClassName(const AClassName: string;
  var AAccepted: boolean);
begin
  if SameText(AClassName, 'TUser') then
    AAccepted := True
  else
    inherited AcceptedClassName(AClassName, AAccepted);
end;

function TUserResource.DeleteUser(AId: string): TUser;
begin
  Result := inherited Delete('TUser', AId) as TUser;
end;

function TUserResource.PostUser(const AId: string;
  AUser: TUser): TUser;
begin
  AUser.Password := THashMD5.GetHashString(AUser.Password);
  Result := inherited Post(AId, AUser) as TUser;
  //Hide real password
  Result.Password := '**********';
end;

function TUserResource.PutUser(const AId: string;
  AUser: TUser): TUser;
begin
  AUser.Password := THashMD5.GetHashString(AUser.Password);
  Result := inherited Put(AId, AUser) as TUser;
  //Hide real password
  Result.Password := '**********';
end;

function TUserResource.RetrieveUser(AId: string): TUser;
begin
  Result := inherited RetrieveInstantObject('TUser', AId) as TUser;
  //Hide real password
  Result.Password := '**********';
end;

function TUserResource.RetrieveUsersList(Where, OrderBy, ChangedFrom,
  ChangedTo: string): TInstantObjectList<TInstantObject>;
var
  O: TInstantObject;
begin
  Result := inherited RetrieveInstantObjectList(
    'TUser', Where, OrderBy, ChangedFrom, ChangedTo);
  for O in Result do
    (O as TUser).Password := '**********';
end;

function TUserResource.UserChangePassword(const AOldPassword,
  ANewPassword: string): string;
var
  LUserName: string;
  LUser: TUser;
  LOKPassword: Boolean;
  LErrMsg: string;
begin
  LUserName := (Auth.Subject as TPrimerServerClaims).UserID;
  LUser := RetrieveInstantObject('TUser', LUserName) as TUser;
  Assert(Assigned(LUser));

  if AOldPassword = ANewPassword then
    raise EWiRLServerException.CreateError(
      http_406_NotAcceptable,
      PWD_NOT_CHANGED, 'Password');

  LOKPassword := CheckPassword(ANewPassword, LErrMsg);
  if not LOKPassword then
    raise EWiRLServerException.CreateError(
      http_406_NotAcceptable,
      LErrMsg, 'Password');

  if LUser.Password = THashMD5.GetHashString(AOldPassword) then
  begin
    LUser.Password := THashMD5.GetHashString(ANewPassword);
  end
  else
  begin
    raise EWiRLServerException.CreateError(
      http_406_NotAcceptable,
      AUTH_FAILED, 'Password');
  end;

  LUser.Store;
  LUser.Free;

  Result := PWD_CHANGED;
end;

{ TDetailsInfo }

constructor TDetailsInfo.Create(const AMessage: string; AClaims: TPrimerServerClaims);
begin
  FMessage := AMessage;
  FClaims := AClaims;
end;

initialization
  //User resource
  TWiRLResourceRegistry.Instance.RegisterResource<TUserResource>;

  // Auth resources
  TWiRLResourceRegistry.Instance.RegisterResource<TBasicAuthResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TFormAuthResource>;
  TWiRLResourceRegistry.Instance.RegisterResource<TBodyAuthResource>;

end.
