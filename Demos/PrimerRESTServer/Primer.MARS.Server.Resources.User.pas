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
unit Primer.MARS.Server.Resources.User;

interface

uses
  //Delphi
  Classes
  , SysUtils
  , Generics.Collections
  , System.JSON
  //MARS
  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.JSON
  , MARS.Core.MessageBodyWriters
  , MARS.Core.MessageBodyReaders
  //MARS Server
  , InstantObjects.MARS.Server.Consts
  , InstantObjects.MARS.Server.Resources
  //InstantObjects + MARS
  , InstantObjects.MARS.Data
  //InstantObjects
  , InstantPersistence
  //Model Units
  , Model
;

type


  TChangePassword = record
    OldPassword: string;
    NewPassword: string;
  end;

  [Path('/TUser')
    , Produces(TMediaType.APPLICATION_JSON)
    ]
  TUserResource = class(TInstantObjectResource)
  protected
    //function to "filter" accepted ClassName for requests
    procedure AcceptedClassName(const AClassName: string;
      var AAccepted: boolean); override;
  public

    [POST, Path('ChangePassword')
    , Consumes(TMediaType.APPLICATION_JSON)
    , Produces(TMediaType.TEXT_PLAIN)
    {$IFNDEF BYPASS_TOKEN}, RolesAllowed('standard'){$ENDIF}
    ]
 //   function UserChangePassword([BodyParam] AChangePassword: TChangePassword): TUser;

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
  , System.TypInfo
  , System.Variants
  , System.RegularExpressions
  , System.Hash
  //MARS
  , MARS.Core.Exceptions
  , MARS.Data.FireDAC
  //MARS Server
  , InstantObjects.MARS.Server.Exceptions
  , InstantObjects.MARS.Server.Resources.Utils
  //InstantObject
  , InstantFireDAC
  , InstantClasses
  , InstantMetaData
  ;


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
  LUser: TUser;
  LOKPassword: Boolean;
  LErrMsg: string;
begin
  LUser := RetrieveInstantObject('TUser', Token.UserName) as TUser;
  Assert(Assigned(LUser));

  if AOldPassword = ANewPassword then
    raise EMARSServerException.CreateError(
      http_406_NotAcceptable,
      PWD_NOT_CHANGED, 'Password');

  LOKPassword := CheckPassword(ANewPassword, LErrMsg);
  if not LOKPassword then
    raise EMARSServerException.CreateError(
      http_406_NotAcceptable,
      LErrMsg, 'Password');

  if LUser.Password = THashMD5.GetHashString(AOldPassword) then
  begin
    LUser.Password := THashMD5.GetHashString(ANewPassword);
  end
  else
  begin
    raise EMARSServerException.CreateError(
      http_406_NotAcceptable,
      AUTH_FAILED, 'Password');
  end;

  LUser.Store;
  LUser.Free;

  Result := PWD_CHANGED;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TUserResource>;

end.
