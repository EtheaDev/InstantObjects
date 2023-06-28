(*
 *   InstantObject with MARS Curiosity REST Library
 *   Server Exceptions
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
unit InstantObjects.MARS.Server.Exceptions;

interface

uses
  //Delphi
  System.Classes
  , Generics.Collections
  , MARS.Core.Exceptions
  ;

type
  THttpStatusCode = (
      http_200_OK
    , http_201_Created
    , http_400_Bad_Request
    , http_403_Forbidden
    , http_401_Unauthorized
    , http_404_NotFound
    , http_406_NotAcceptable
    , http_500_InternalServerError
    );

const
  HttpStatusValue: array[THttpStatusCode] of integer = (
      200 //http_200_OK
    , 201 //http_201_Created
    , 400 //http_400_Bad_Request
    , 403 //http_403_Forbidden
    , 401 //http_401_Unauthorized
    , 404 //http_404_NotFound
    , 406 //http_406_NotAcceptable
    , 500 //http_500_InternalServerError
    );

  HttpStatusContent: array[THttpStatusCode] of string = (
      'OK' //http_OK_Created = 201
    , 'Created' //http_OK_Created = 201
    , 'Bad Request' //http_400_Bad_Request = 400
    , 'Forbidden' //http_403_Forbidden = 403
    , 'Unauthorized' //http_401_Unauthorized = 401
    , 'Not Found' //http_404_NotFound = 404
    , 'Not Acceptable' //http_406_NotAcceptable = 406
    , 'Internal Server Error' //http_500_InternalServerError
    );

type
  EMARSServerException = class(EMARSHttpException)
  private
    ErrorCode: THttpStatusCode;
    FieldName: string;
    Description: string;
    Content: string;
  public
    class function ComposeErrorJSONContent(
      const AErrorCode: THttpStatusCode;
      const AErrorMsg: string;
      const AFieldName: string = ''): string; overload;
    class function ComposeErrorJSONContent(
      const AStatusCode: Integer;
      const AErrorMsg: string;
      const AFieldName: string = ''): string; overload;

    constructor CreateError(const AErrorCode: THttpStatusCode;
      const ADescription: string;
      const AFieldName: string = '');
    function GetErrorJSONContent: string;
  end;

implementation

uses
  //Delphi
  System.StrUtils
  , System.SysUtils
  //MARS
  , MARS.Core.JSON
  ;


{ EMARSServerException }

constructor EMARSServerException.CreateError(const AErrorCode: THttpStatusCode;
  const ADescription: string; const AFieldName: string = '');
begin
  inherited Create(ADescription, HttpStatusValue[AErrorCode]);
  ErrorCode := AErrorCode;
  Description := ADescription;
  FieldName := AFieldName;
  Content := HttpStatusContent[AErrorCode];
end;

function EMARSServerException.GetErrorJSONContent: string;
begin
  Result := ComposeErrorJSONContent(ErrorCode, self.Description, self.FieldName);
end;

class function EMARSServerException.ComposeErrorJSONContent(
  const AStatusCode: Integer; const AErrorMsg: string;
  const AFieldName: string = ''): string;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    LJSONObject.AddPair('errorCode', IntToStr(AStatusCode));
    if AFieldName <> '' then
      LJSONObject.AddPair('attributeName', AFieldName);
    LJSONObject.AddPair('errorDescription', AErrorMsg);
    Result := LJSONObject.ToString;
  finally
    LJSONObject.Free;
  end;
end;

class function EMARSServerException.ComposeErrorJSONContent(
  const AErrorCode: THttpStatusCode;
  const AErrorMsg: string;
  const AFieldName: string = ''): string;
begin
  Result := ComposeErrorJSONContent(HttpStatusValue[AErrorCode],
    AErrorMsg, AFieldName);
end;

end.

