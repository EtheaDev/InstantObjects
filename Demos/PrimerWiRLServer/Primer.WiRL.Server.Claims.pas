{******************************************************************************}
{                                                                              }
{       WiRL: RESTful Library for Delphi                                       }
{                                                                              }
{       Copyright (c) 2015-2021 WiRL Team                                      }
{                                                                              }
{       https://github.com/delphi-blocks/WiRL                                  }
{                                                                              }
{******************************************************************************}
unit Primer.WiRL.Server.Claims;

interface

uses
  System.Classes,
  WiRL.Core.JSON,
  WiRL.Core.Auth.Context;

const
  CLAIM_ENVIRONMENT   = 'ENVIRONMENT';
  CLAIM_PROFILE_ID    = 'PROFILE_ID';
  CLAIM_LANGUAGE_ID   = 'LANGUAGE_ID';
  CLAIM_LAST_NAME     = 'LAST_NAME';
  CLAIM_FIRST_NAME    = 'FIRST_NAME';
  CLAIM_EMAIL_ADDRESS = 'EMAIL_ADDRESS';
  CLAIM_SERVICE_KEY   = 'SERVICEKEY';
type
  // Custom Claims Class for Primer Demo
  TPrimerServerClaims = class(TWiRLSubject)
  private
    function GetEnvironment: string;
    procedure SetEnvironment(const Value: string);
    function GetProfileId: string;
    procedure SetProfileId(const Value: string);
    function GetLanguageId: string;
    procedure SetLanguageId(const Value: string);
    function GetLastName: string;
    procedure SetLastName(const Value: string);
    function GetFirstName: string;
    procedure SetFirstName(const Value: string);
    function GetEmailAddress: string;
    procedure SetEmailAddress(const Value: string);
    function GetServiceKey: string;
    procedure SetServiceKey(const Value: string);
  public
    //From Current User
    property ProfileId: string read GetProfileId write SetProfileId;
    property LanguageId: string read GetLanguageId write SetLanguageId;
    property LastName: string read GetLastName write SetLastName;
    property FirstName: string read GetFirstName write SetFirstName;
    property EmailAddress: string read GetEmailAddress write SetEmailAddress;

    //From Request
    property Environment: string read GetEnvironment write SetEnvironment;
    property ServiceKey: string read GetServiceKey write SetServiceKey;
  end;

implementation

uses
  JOSE.Types.JSON,
  JOSE.Core.Base;

{ TPrimerServerClaims }

function TPrimerServerClaims.GetEnvironment: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_ENVIRONMENT, FJSON).AsString;
end;

procedure TPrimerServerClaims.SetEnvironment(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_ENVIRONMENT, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_ENVIRONMENT, Value, FJSON);
end;

function TPrimerServerClaims.GetProfileId: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_PROFILE_ID, FJSON).AsString;
end;

procedure TPrimerServerClaims.SetProfileId(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_PROFILE_ID, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_PROFILE_ID, Value, FJSON);
end;

function TPrimerServerClaims.GetLanguageId: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_LANGUAGE_ID, FJSON).AsString;
end;

procedure TPrimerServerClaims.SetLanguageId(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_LANGUAGE_ID, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_LANGUAGE_ID, Value, FJSON);
end;

function TPrimerServerClaims.GetLastName: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_LAST_NAME, FJSON).AsString;
end;

procedure TPrimerServerClaims.SetLastName(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_LAST_NAME, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_LAST_NAME, Value, FJSON);
end;

function TPrimerServerClaims.GetFirstName: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_FIRST_NAME, FJSON).AsString;
end;

procedure TPrimerServerClaims.SetFirstName(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_FIRST_NAME, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_FIRST_NAME, Value, FJSON);
end;

function TPrimerServerClaims.GetEmailAddress: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_EMAIL_ADDRESS, FJSON).AsString;
end;

procedure TPrimerServerClaims.SetEmailAddress(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_EMAIL_ADDRESS, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_EMAIL_ADDRESS, Value, FJSON);
end;

function TPrimerServerClaims.GetServiceKey: string;
begin
  Result := TJSONUtils.GetJSONValue(CLAIM_SERVICE_KEY, FJSON).AsString;
end;

procedure TPrimerServerClaims.SetServiceKey(const Value: string);
begin
  if Value = '' then
    TJSONUtils.RemoveJSONNode(CLAIM_SERVICE_KEY, FJSON)
  else
    TJSONUtils.SetJSONValueFrom<string>(CLAIM_SERVICE_KEY, Value, FJSON);
end;

end.
