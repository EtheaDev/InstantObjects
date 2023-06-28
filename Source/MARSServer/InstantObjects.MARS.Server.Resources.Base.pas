(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server.Resources.Base
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
unit InstantObjects.MARS.Server.Resources.Base;

interface

uses
  //Delphi
  System.Classes
  , System.SysUtils
  , System.JSON
  , System.Rtti
  , System.Threading
  , Generics.Collections
  , Web.HTTPApp

  //InstantObjects
  , InstantPersistence
  , InstantBrokers

  //MARS
  , MARS.Core.Registry
  , MARS.Core.Attributes
  , MARS.Core.MediaType
  , MARS.Core.URL
  , MARS.Core.Token
  , MARS.Core.Activation.Interfaces
  , MARS.Core.RequestAndResponse.Interfaces
  , MARS.Core.Exceptions
  , MARS.Core.Response

  //MARS InstantObject
  , InstantObjects.MARS.Data

  //MARS Metadata / Swagger
  , MARS.Metadata
  , MARS.Metadata.InjectionService
  , MARS.Metadata.Attributes
  , MARS.Metadata.JSON
  , MARS.dmustache
  , MARS.dmustache.InjectionService
  , MARS.OpenAPI.v3
  , MARS.OpenAPI.v3.Utils
  ;

type
  // Base class for all MARS Server resources. Provides basic services and utilities,
  // error handling, response formatting.
  TBaseResource = class abstract
  private
    FJSONFormatSettings: TFormatSettings;
    FRequestBody: TJSONObject;
    FResponseBody: TJSONObject;
    FTransientObjects: TObjectList<TObject>;
    function GetRequestBody: TJSONObject;
    function GetResponseBody: TJSONObject;
  protected
    [Context] URL: TMARSURL;
    [Context] Request: IMARSRequest;
    [Context] Response: IMARSResponse;
    [Context] Token: TMARSToken;
    [Context] Activation: IMARSActivation;
    property RequestBody: TJSONObject read GetRequestBody;
    property ResponseBody: TJSONObject read GetResponseBody;

    procedure Finalize_GDI; stdcall;
    procedure Initialize_GDI; stdcall;

    function ResponseBodyHasReturnCode: Boolean;
    function GetPayloadAsJSONObject: TJSONObject;

    function GetClientIPAddress: string;

    // Returns True if the special debug_output header field is set to any
    // non-empty value AND AllowDebugOutput is True in the config file.
    function DebugOutput: Boolean;

    function GetRequestAsJSONObject: TJSONObject;

    // Checks that a field named AFieldName exists in RequestBody and contains a
    // non-empty value. If so, returns True and sets AValue to the string value.
    // Otherwise returns False and calls SetUnacceptableResponse and
    // FillErrorResponse to fill ResponseBody.
    // Used for required field validation in JSON objects passed as the request
    // body. The caller can Exit right after calling this method if it returns False,
    // as the response is automatically set up.
    procedure CheckRequiredField(const AFieldName: string; out AValue: string); overload;
    procedure CheckRequiredObjectField(const AFieldName: string; out AValue: TJSONObject);
    procedure CheckRequiredField(const ARequest: TWebRequest; const AFieldName: string;
      out AValue: string); overload;
    // Like CheckRequiredField, but also checks that the value is a valid number.
    procedure CheckRequiredNumericField(const AFieldName: string; out AValue: Double);
    // Like CheckRequiredField, but also checks that the value is a valid integer number.
    procedure CheckRequiredIntegerField(const AFieldName: string; out AValue: Integer);
    procedure CheckRequiredFields(const AFieldNames: TArray<string>);
    // Same as CheckRequiredField, but gets the value as an argument.
    // Useful for path or form params.
    procedure CheckRequiredString(const AFieldName, AValue: string);
    // Checks that AValue is the same length of AMaxLength or shorter. If so, returns True.
    // Otherwise returns False and calls SetUnacceptableResponse and
    // FillErrorResponse to fill ResponseBody.
    // Used for string field length validation in JSON objects passed as the request
    // body. The caller can Exit right after calling this method if it returns False,
    // as the response is automatically set up.
    procedure CheckStringMaxLength(const AFieldName, AValue: string; const AMaxLength: Integer);

    // Check ip format of the string
    procedure CheckIPAddress(const AFieldName, AValue: string);

    function AddTransientObject<T: class>(const AObject: T): T;

    // Checks that the request is valid according to the Server/RequestIntervalSecs
    // parameter, which specifies the number of seconds before which a request cannot
    // be sent again from the same IP address. Returns False if the same request
    // (same URL) comes from an IP address before enough seconds have passed, in
    // which case it also resets the timer.
    // Otherwise returns True and writes an object with the current timestamp
    // to look for next time.
    //function IsValidRequestFromIPAddress: Boolean;

    //InstantObject Access
    function GetInstantObjectClass(const AClassName: string): TInstantObjectClass;

    function Post(const AId: string;
      AJSONBodyObject: TInstantObject;
      const ACheckIfExists: boolean = True): TInstantObject; virtual;

    function Put(const AId: string;
      AJSONBodyObject: TInstantObject): TInstantObject; virtual;

    function Delete(AClassName: string;
      AId: string): TInstantObject; virtual;

    //function to "filter" accepted ClassName for requests
    procedure AcceptedClassName(const AClassName: string;
      var AAccepted: boolean); virtual;

    //Content-Type from filename
    function GetContentTypeFromFileName(const AFileName: string): string;

    //Base Params Check
    function CheckClassName(const AClassName: string): TInstantObjectClass;
    procedure CheckEmptyJSONBody(
      const AJSONBodyObject: TInstantObject);
    procedure CheckObjectAlreadyExists(
      const AJSONBodyObject: TInstantObject);
    procedure CheckObjectDontExists(const AId: string;
      const AJSONBodyObject: TInstantObject);
    procedure CheckIdDontMatchBody(const APathParamId: string;
      const AJSONBodyObject: TInstantObject);
  public
    [Context] InstantObject: TMARSInstantObjects;

    [OPTIONS, Path('{*}')]
    [Produces(TMediaType.TEXT_HTML)]
    function CORSPreflight: TJSONObject;

    procedure AfterConstruction; override;
    destructor Destroy; override;

    function RetrieveInstantObjectReferenceList([PathParam] AClassName: string;
      [QueryParam] Where: string = '';
      [QueryParam] OrderBy: string = '';
      [QueryParam] ChangedFrom: string = '';
      [QueryParam] ChangedTo: string = '';
      [QueryParam] PageCount: integer = 0;
      [QueryParam] RecordCount: integer = 0): TList<TInstantObjectReference>; virtual;

    function GetInstantObjectReferenceCount([PathParam] AClassName: string;
      [QueryParam] Where: string = '';
      [QueryParam] OrderBy: string = '';
      [QueryParam] ChangedFrom: string = '';
      [QueryParam] ChangedTo: string = ''): Integer; virtual;

    function RetrieveInstantObject([PathParam] AClassName: string;
      [PathParam] AId: string): TInstantObject; virtual;

    function RetrieveInstantObjectList([PathParam] AClassName: string;
      [QueryParam] Where: string = '';
      [QueryParam] OrderBy: string = '';
      [QueryParam] ChangedFrom: string = '';
      [QueryParam] ChangedTo: string = '';
      [QueryParam] PageCount: integer = 0;
      [QueryParam] RecordCount: integer = 0): TInstantObjectList<TInstantObject>; virtual;
  end;

  [Path('/swagger'), Produces(TMediaType.APPLICATION_JSON), Produces(TMediaType.APPLICATION_YAML)
    , MetaVisible(False)
  ]
  TOpenAPIResource = class(TBaseResource)
    [GET, Path('/openapi')]
    function OpenAPI([Context] AOpenAPI: TOpenAPI): TOpenAPI;

    [GET, Path('/info'), IsReference]
    function Info([Context] AOpenAPI: TOpenAPI): TInfo;
  end;

implementation

uses
  //Delphi
  System.DateUtils
  , System.StrUtils
  , System.IOUtils
  //InstantObject
  , InstantQueryBuilder
  //MARS Server units
  , InstantObjects.MARS.Server.Resources.Utils
  , InstantObjects.MARS.Server.Consts
  , InstantObjects.MARS.Server.Exceptions
  //InstantObjects FireDAC Broker
  , InstantFireDAC
  , WinAPI.GDIPObj
  , WinAPI.GDIPApi
  ;

{ TSNResource }

procedure TBaseResource.Initialize_GDI; stdcall;
begin
  //Initialize GDI+
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs := False;
  StartupInput.GdiplusVersion := 1;
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;

procedure TBaseResource.Finalize_GDI; stdcall;
begin
  GdiplusShutdown(gdiplusToken);
end;


function TBaseResource.AddTransientObject<T>(const AObject: T): T;
begin
  if Assigned(AObject) then
    FTransientObjects.Add(AObject);
  Result := AObject;
end;

procedure TBaseResource.AfterConstruction;
begin
  inherited;
  FJSONFormatSettings := TFormatSettings.Create;
  FJSONFormatSettings.DecimalSeparator := '.';
  FTransientObjects := TObjectList<TObject>.Create;
end;

procedure TBaseResource.AcceptedClassName(
  const AClassName: string;
  var AAccepted: boolean);
begin
  AAccepted := True;
  //By default TISUser, TISProfile are not accepted
  AAccepted := AAccepted and not
    (SameText(AClassName, 'TISUser') or
    SameText(AClassName, 'TISProfile'));
end;

function TBaseResource.CheckClassName(const AClassName: string): TInstantObjectClass;
var
  AAccepted: Boolean;
  AClass: TClass;
begin
  if AClassName = '' then
    raise EMARSServerException.CreateError(http_404_NotFound,
      EMPTY_CLASSNAME);
  AAccepted := False;
  AcceptedClassName(AClassName, AAccepted);
  if not AAccepted then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(WRONG_CLASSNAME,[AClassName]));
  AClass := GetClass(AClassName);
  if Assigned(AClass) and AClass.InheritsFrom(TInstantObject) then
    Result := TInstantObjectClass(AClass)
  else
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(WRONG_CLASSNAME,[AClassName]));
end;

procedure TBaseResource.CheckEmptyJSONBody(
  const AJSONBodyObject: TInstantObject);
begin
  //Check if AObject (from JSON Body) is empty
  if not Assigned(AJSONBodyObject) then
    raise EMARSServerException.CreateError(http_400_Bad_Request,
      MISSED_OBJECT_IN_BODY)
  else
    CheckClassName(AJSONBodyObject.ClassName);
end;

procedure TBaseResource.CheckObjectAlreadyExists(
  const AJSONBodyObject: TInstantObject);
begin
  //Check if AJSONBodyObject (from JSON Body) already exists into Database
  if Assigned(AJSONBodyObject) and AJSONBodyObject.IsPersistent then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
       Format(OBJECT_ALREADY_EXISTS,
        [AJSONBodyObject.Id, AJSONBodyObject.Caption]));
end;

procedure TBaseResource.CheckObjectDontExists(const AId: string;
  const AJSONBodyObject: TInstantObject);
begin
  //Check if AJSONBodyObject (from JSON Body) don't exists into Database
  if not Assigned(AJSONBodyObject) or not AJSONBodyObject.IsPersistent then
    raise EMARSServerException.CreateError(http_404_NotFound,
      Format(OBJECT_OF_TYPE_NOT_FOUND, [AId, AJSONBodyObject.ClassName]))
end;

procedure TBaseResource.CheckIdDontMatchBody(const APathParamId: string;
  const AJSONBodyObject: TInstantObject);
begin
  //Check if AJSONBodyObject.Id (from JSON Body) dont match with Id (from PathParam)
  CheckEmptyJSONBody(AJSONBodyObject);
  if (APathParamId <> '') and
     (AJSONBodyObject.Id <> AJSONBodyObject.ReformatID(APathParamId)) then
    raise EMARSServerException.CreateError(http_404_NotFound,
      Format(ID_DONT_MATCH_JSON,
        [APathParamId, AJSONBodyObject.Id]));
end;

procedure TBaseResource.CheckIPAddress(const AFieldName, AValue: string);
begin
  if not IsIPWellFormed(AValue) then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(IP_ADDR_NOT_WELLFORMED, [AFieldName]), AFieldName);
end;

procedure TBaseResource.CheckRequiredField(const AFieldName: string; out AValue: string);
begin
  AValue := RequestBody.S[AFieldName];
  CheckRequiredString(AFieldName, AValue);
end;

procedure TBaseResource.CheckRequiredField(const ARequest: TWebRequest;
  const AFieldName: string; out AValue: string);
begin
  AValue := ARequest.QueryFields.Values[AFieldName];
  if AValue = '' then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(FIELD_NOT_SPECIFIED, [AFieldName]), AFieldName);
end;

procedure TBaseResource.CheckRequiredFields(const AFieldNames: TArray<string>);
var
  AFieldName: string;
  LDummy: string;
begin
  for AFieldName in AFieldNames do
    CheckRequiredField(AFieldName, LDummy);
end;

procedure TBaseResource.CheckRequiredIntegerField(const AFieldName: string;
  out AValue: Integer);
var
  LValue: string;
begin
  LValue := RequestBody.GetValue(AFieldName).AsType<string>;
  if (LValue = '') or not TryStrToInt(LValue, AValue) then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(WRONG_INTEGER_FIELD, [AFieldName]), AFieldName);
end;

procedure TBaseResource.CheckRequiredNumericField(const AFieldName: string; out AValue: Double);
var
  LValue: string;
begin
  LValue := RequestBody.S[AFieldName];
  if (LValue = '') or not TryStrToFloat(LValue, AValue, FJSONFormatSettings) then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(WRONG_INTEGER_FIELD, [AFieldName]), AFieldName);
end;

procedure TBaseResource.CheckRequiredObjectField(const AFieldName: string;
  out AValue: TJSONObject);
begin
  AValue := RequestBody.O[AFieldName];
  if not Assigned(AValue) then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(FIELD_NOT_SPECIFIED, [AFieldName]), AFieldName);
end;

procedure TBaseResource.CheckRequiredString(const AFieldName, AValue: string);
begin
  if AValue = '' then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(FIELD_NOT_SPECIFIED, [AFieldName]), AFieldName);
end;

procedure TBaseResource.CheckStringMaxLength(const AFieldName, AValue: string;
  const AMaxLength: Integer);
begin
  if AValue.Length > AMaxLength then
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(FIELD_VALUE_TOO_LONG, [AFieldName, AMaxLength]));
end;

function TBaseResource.DebugOutput: Boolean;
begin
  Result := (Request.GetFormParamValue('debug_output') <> '');
end;

function TBaseResource.Delete(AClassName, AId: string): TInstantObject;
begin
  CheckClassName(AClassName);
  Result := RetrieveInstantObject(AClassName, AId);
  CheckObjectDontExists(AId, Result);
  Result.Dispose;
  Result := nil;
end;

destructor TBaseResource.Destroy;
begin
  FreeAndNil(FRequestBody);
  //FreeAndNil(FResponseBody);
  FreeAndNil(FTransientObjects);
  inherited;
end;

function TBaseResource.GetResponseBody: TJSONObject;
begin
  if not Assigned(FResponseBody) then
    FResponseBody := TJSONObject.Create;
  Result := FResponseBody;
end;

function TBaseResource.Post(const AId: string;
  AJSONBodyObject: TInstantObject;
  const ACheckIfExists: boolean = True): TInstantObject;
begin
  CheckEmptyJSONBody(AJSONBodyObject);
  if ACheckIfExists then
    CheckObjectAlreadyExists(AJSONBodyObject);
  CheckIdDontMatchBody(AId, AJSONBodyObject);
  //Save Object
  StoreAllInstantObjects(AJSONBodyObject, True);
  Result := AJSONBodyObject;
end;

function TBaseResource.Put(const AId: string;
  AJSONBodyObject: TInstantObject): TInstantObject;
var
  LObjectToUpdate: TInstantObject;
begin
  CheckEmptyJSONBody(AJSONBodyObject);
  if AJSONBodyObject.Id <> AId then
  begin
    //Request to change also Id of Object
    LObjectToUpdate := RetrieveInstantObject(AJSONBodyObject.ClassName, AId);
    if Assigned(LObjectToUpdate) then
      LObjectToUpdate.Assign(AJSONBodyObject);
    //Save Object
    StoreAllInstantObjects(LObjectToUpdate, True);
    Result := LObjectToUpdate;
  end
  else
  begin
    CheckObjectDontExists(AId, AJSONBodyObject);
    //Save Object
    StoreAllInstantObjects(AJSONBodyObject, True);
    Result := AJSONBodyObject;
  end;
end;

function TBaseResource.GetClientIPAddress: string;
begin
  Result := string(Request.GetFormParamValue('X-Real-IP'));
end;

function TBaseResource.GetContentTypeFromFileName(
  const AFileName: string): string;
var
  LFileExt: string;
begin
  LFileExt := ExtractFileExt(AFileName);
  if SameText(LFileExt,'.jpg') or SameText(LFileExt,'.jpeg') then
    Result := 'image/jpeg'
  else if SameText(LFileExt,'.png' ) then
    Result := 'image/png'
  else if SameText(LFileExt,'.pdf' ) then
    Result := 'application/pdf'
  else if SameText(LFileExt,'.htm' ) then
    Result := 'text/html'
  else if SameText(LFileExt,'.html') then
    Result := 'text/html'
  else if SameText(LFileExt,'.js'  ) then
    Result := 'application/javascript'
  else if SameText(LFileExt,'.css' ) then
    Result := 'text/css'
  else if SameText(LFileExt,'.txt' ) then
    Result := 'text/plain'
  else if SameText(LFileExt,'.svg' ) then
    Result := 'image/svg+xml'
  else
    Result := 'text/plain';
end;

function TBaseResource.GetInstantObjectClass(
  const AClassName: string): TInstantObjectClass;
var
  LClass: TInstantObjectClass;
begin
  LClass := CheckClassName(AClassName);
  if not Assigned(LClass) then
      raise EMARSServerException.CreateError(http_400_Bad_Request,
       Format(INVALID_CLASS, [AClassName]), 'ClassName')
  else if LClass.InheritsFrom(TInstantObject) then
    Result := TInstantObjectClass(LClass)
  else
    raise EMARSServerException.CreateError(http_406_NotAcceptable,
      Format(INCOMPATIBLE_IOCLASS, [AClassName]), 'ClassName');
end;

function TBaseResource.GetPayloadAsJSONObject: TJSONObject;
begin
  Result := TJSONObject.ParseJSONValue(Request.Content) as TJSONObject;
end;

function TBaseResource.GetRequestAsJSONObject: TJSONObject;
begin
  Result := TJSONObject.ParseJSONValue(Request.Content) as TJSONObject;
  if Result = nil then
  begin
    Result := TJSONObject.Create;
    Result.AddPair('request', Request.Content);
  end;
end;

function TBaseResource.GetRequestBody: TJSONObject;
begin
  if not Assigned(FRequestBody) then
  begin
    FRequestBody := TJSONObject.ParseJSONValue(Request.Content) as TJSONObject;
    if FRequestBody = nil then
    begin
      FRequestBody := TJSONObject.Create;
      FRequestBody.AddPair('request', Request.Content);
    end;
  end;
  Result := FRequestBody;
end;

function TBaseResource.ResponseBodyHasReturnCode: Boolean;
begin
  Result := ResponseBody.S['errorCode'] <> '';
end;

function TBaseResource.RetrieveInstantObjectList(AClassName, Where,
  OrderBy, ChangedFrom, ChangedTo: string;
  PageCount, RecordCount : integer): TInstantObjectList<TInstantObject>;
var
  I: Integer;
  LInstantObject: TInstantObject;
  LInstantQueryBuilder: TInstantQueryBuilder;
  LClass: TInstantObjectClass;
begin
  LClass := CheckClassName(AClassName);
  LInstantQueryBuilder := TInstantQueryBuilder.Create(InstantObject.Connector);
  try
    LInstantQueryBuilder.UpdateCommand(LClass, Where, OrderBy,
      ChangedFrom, ChangedTo, PageCount, RecordCount);
    LInstantQueryBuilder.Query.Open;

    Result := TInstantObjectList<TInstantObject>.Create(True);
    Result.OwnsObjects := True;
    for I := 0 to LInstantQueryBuilder.Query.ObjectCount -1 do
    begin
      LInstantObject := TInstantObject(LInstantQueryBuilder.Query.Objects[I]);
      Result.Add(LInstantObject);
      LInstantObject.AddRef;
    end;
    //Per dire a MARS di eliminare l'oggetto alla fine della request
    Activation.AddToContext(LInstantQueryBuilder);
  except
    LInstantQueryBuilder.Free;
    raise;
  end;
end;

function TBaseResource.RetrieveInstantObjectReferenceList(AClassName, Where,
  OrderBy, ChangedFrom, ChangedTo: string;
  PageCount, RecordCount : integer): TList<TInstantObjectReference>;
var
  LInstantQueryBuilder: TInstantQueryBuilder;
  LClass: TInstantObjectClass;
  LOrderBy: string;
begin
  LClass := CheckClassName(AClassName);
  LInstantQueryBuilder := TInstantQueryBuilder.Create(InstantObject.Connector);
  try
    if OrderBy <> '' then
      LOrderBy := OrderBy
    else
      LOrderBy := 'Id';

    LInstantQueryBuilder.UpdateCommand(
      LClass, Where, LOrderBy, ChangedFrom, ChangedTo,
      PageCount, RecordCount);

    LInstantQueryBuilder.Query.Open;
    Result := TList<TInstantObjectReference>.Create;
    LInstantQueryBuilder.Query.GetObjectReferenceList(Result);
    //Per dire a MARS di eliminare l'oggetto alla fine della request
    Activation.AddToContext(LInstantQueryBuilder);
  except
    LInstantQueryBuilder.Free;
    raise;
  end;
end;

function TBaseResource.GetInstantObjectReferenceCount(AClassName, Where,
  OrderBy, ChangedFrom, ChangedTo: string): Integer;
var
  LInstantQueryBuilder: TInstantQueryBuilder;
  LClass: TInstantObjectClass;
begin
  LClass := CheckClassName(AClassName);
  LInstantQueryBuilder := TInstantQueryBuilder.Create(InstantObject.Connector);
  try
    LInstantQueryBuilder.UpdateCommand(
      LClass, Where, OrderBy, ChangedFrom, ChangedTo, 0, 0);

    LInstantQueryBuilder.Query.Open;
    Result := LInstantQueryBuilder.Query.ObjectCount;
    //Per dire a MARS di eliminare l'oggetto alla fine della request
    Activation.AddToContext(LInstantQueryBuilder);
  except
    LInstantQueryBuilder.Free;
    raise;
  end;
end;

function TBaseResource.RetrieveInstantObject(AClassName, AId: string): TInstantObject;
var
  LIOClass: TInstantObjectClass;
begin
  LIOClass := GetInstantObjectClass(AClassName);
  if Assigned(LIOClass) then
  begin
    Result := LIOClass.Retrieve(AId, False, False, InstantObject.Connector);
    if not Assigned(Result) then
      raise EMARSServerException.CreateError(http_404_NotFound,
        Format(OBJECT_NOT_FOUND, [AClassName, AId]), 'ClassName+Id');
  end
  else
    raise EMARSServerException.CreateError(http_404_NotFound,
      Format(CLASS_NOT_FOUND,[AClassName]), 'ClassName');
end;

(*
function TBaseResource.IsValidRequestFromIPAddress: Boolean;
var
  LRequest: TRequestFromIPAddress;
  LId: string;
begin
  LId := URL.URL + ':' + GetClientIPAddress;
  LRequest := AddTransientObject<TRequestFromIPAddress>(PersistenceManager.GetObject<TRequestFromIPAddress>(LId));
  if Assigned(LRequest) then
  begin
    Result := LRequest.IsValid;
    LRequest.LastRequestTimestamp := Now;
    LRequest.Counter := LRequest.Counter + 1;
    PersistenceManager.PutObject<TRequestFromIPAddress>(LRequest);
  end
  else
  begin
    LRequest := AddTransientObject<TRequestFromIPAddress>(TRequestFromIPAddress.Create);
    LRequest.Id := LId;
    LRequest.Counter := 1;
    LRequest.LastRequestTimestamp := Now;
    Result := True;
    PersistenceManager.PutObject<TRequestFromIPAddress>(LRequest, SecsPerDay);
  end;
end;
*)

function TBaseResource.CORSPreflight: TJSONObject;
var
  LAccessControlRequestMethod: string;
begin
  Result := TJSONObject.Create;
  LAccessControlRequestMethod := string(Request.GetFormParamValue('Access-Control-Request-Method'));
  if LAccessControlRequestMethod <> '' then
  begin
    Response.SetHeader('Access-Control-Allow-Methods','GET, POST, PUT, DELETE');
    Response.SetHeader('Access-Control-Allow-Headers','service_key, debug_output, accept, token_test_mode, auth_token, X-Real-IP');
  end;
end;

{ TOpenAPIResource }

function TOpenAPIResource.Info(AOpenAPI: TOpenAPI): TInfo;
begin
  Result := AOpenAPI.info;
end;

function TOpenAPIResource.OpenAPI(AOpenAPI: TOpenAPI): TOpenAPI;
begin
  Result := AOpenAPI;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  TMARSResourceRegistry.Instance.RegisterResource<TOpenAPIResource>;

end.

