(*
 *   InstantObject with MARS Curiosity REST Library
 *   Primer MARS Server Ignition
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
 * Carlo Barazzetta, Nicola Tambascia
 *
 * ***** END LICENSE BLOCK ***** *)
unit Server.Ignition;

{$I MARS.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.ZLib,
  System.StrUtils,
  MARS.Core.Engine,
  MARS.Core.Engine.Interfaces
  ;

const
  ENGINE_NAME = 'PrimerApi';
  APP_NAME = 'PrimerApp';
  APP_API_PATH = 'rest';
  APP_BASE_PATH = 'primer';
  REST_API_NAME = 'Primer API';
  REST_API_COPYRIGHT = 'Copyright © - Ethea S.r.l.';

type
  TServerEngine=class
  private
    class var FEngine: IMARSEngine;
    class var FAvailableConnectionDefs: TArray<string>;
  public
    class constructor CreateEngine;
    class destructor DestroyEngine;
    class property Default: IMARSEngine read FEngine;
  end;


implementation

uses
  System.Hash,
  FireDAC.Comp.Client,
  FireDAC.Stan.Option,
  MARS.Core.Activation,
  MARS.Core.Activation.Interfaces,
  MARS.Core.Application,
  MARS.Core.Utils,
  MARS.Utils.Parameters.IniFile,
  MARS.Core.URL,
  MARS.Core.RequestAndResponse.Interfaces,
  MARS.Core.MessageBodyWriter,
  MARS.Core.MessageBodyWriters,
  MARS.Core.MessageBodyReaders,
  MARS.Data.MessageBodyWriters,
  MARS.Core.MediaType,
  MARS.Data.FireDAC,
  {$IFDEF MSWINDOWS}
  MARS.mORMotJWT.Token,
  {$ELSE}
  MARS.JOSEJWT.Token,
  {$ENDIF}
  MARS.Metadata,
  MARS.Metadata.JSON,
  MARS.YAML.ReadersAndWriters,
  MARS.Metadata.Engine.Resource,
  MARS.Metadata.ReadersAndWriters,
  MARS.OpenAPI.v3.InjectionService,
  InstantObjects.MARS.Data,
  InstantObjects.Neon.MessageBodyReaders,
  InstantObjects.Neon.MessageBodyWriters,
  InstantObjects.MARS.InjectionService,
  InstantObjects.MARS.Server.Resources.Base,
  InstantObjects.MARS.Server.Resources.Config,
  InstantObjects.MARS.Server.Exceptions,
  InstantObjects.MARS.Server.Consts,
  InstantObjects.MARS.Server.Resources.Utils,
  {$IFDEF LOGGERPRO}
  InstantObjects.MARS.Server.LoggerPro.Config,
  {$ENDIF}
  Primer.MARS.Server.Resources.Token,
  Primer.MARS.Server.Resources.User;

{ TServerEngine }
class constructor TServerEngine.CreateEngine;
{$IFNDEF BYPASS_TOKEN}
var
  LServiceKeyId: string;
  LServiceKeyPassed: Boolean;
  LHashBackoffice, LHashFrontend: string;
{$ENDIF}
begin
  FEngine := TMARSEngine.Create(ENGINE_NAME);
  try
    // Default port to 8080
    FEngine.Parameters.Values['Port'] := 8080;
    FEngine.SetBasePath('/'+APP_API_PATH);
    // Engine configuration
    FEngine.Parameters.LoadFromIniFile;
    // Application configuration
    FEngine.AddApplication(APP_NAME, APP_BASE_PATH, [
      'InstantObjects.MARS.Server.Resources.*',
      'Primer.MARS.Server.Resources.*',
      'MARS.Metadata.*'
      ]);

{$IFDEF MARS_FIREDAC}
    FAvailableConnectionDefs := TMARSFireDAC.LoadConnectionDefs(FEngine.Parameters, 'FireDAC');

    TMARSFireDAC.AfterCreateConnection :=
      procedure (const AConnection: TFDConnection; const AActivation: IMARSActivation)
      begin
        AConnection.TxOptions.Isolation :=
          FEngine.Parameters
          .ByNameTextEnum<TFDTxIsolation>(
            'FireDAC.' + AConnection.ConnectionDefName + '.TxOptions.Isolation'
            , TFDTxIsolation.xiReadCommitted);
      end;

{$ENDIF}
{$REGION 'BeforeHandleRequest'}
    FEngine.BeforeHandleRequest :=
      function (const AEngine: IMARSEngine;
        const AURL: TMARSURL; const ARequest: IMARSRequest; const AResponse: IMARSResponse;
        var Handled: Boolean): Boolean
      begin
        Result := True;
        // skip favicon requests (browser)
        if SameText(AURL.Document, 'favicon.ico') then
        begin
          Result := False;
          Handled := True;
        end;
        // Handle CORS and PreFlight
        if SameText(ARequest.Method, 'OPTIONS') then
        begin
          Handled := True;
          Result := False;
        end;
      end;
{$ENDREGION}
{$REGION 'Global BeforeInvoke handler'}
    // to execute something before each activation
    TMARSActivation.RegisterBeforeInvoke(
      procedure (const AActivation: IMARSActivation; out AIsAllowed: Boolean)
      var
        LRequestSwagger: Boolean;
        LEnvironment, LQueryParams: string;
        {$IFNDEF BYPASS_TOKEN}
        LTokenEnvironment: string;
        {$ENDIF}
      begin
        //Method invoked on every call before any operation
        //Perform access (service_key) and environment (environment) checks.
        LRequestSwagger := Pos('swagger/', AActivation.Request.RawPath) > 0;
        LEnvironment := string(AActivation.Request.GetHeaderParamValue(PARAM_ENVIRONMENT));
        {$IFDEF DEBUG}
        if LEnvironment = '' then
          LEnvironment := 'test';
        {$ENDIF}

        LQueryParams := AActivation.Request.GetQueryString;
        {$IFDEF LOGGERPRO}
        if LQueryParams <> '' then
          Log.Info('Request [Environment: %s] [Method: %s] [Url: %s]',
                  [LEnvironment,
                   AActivation.Request.Method,
                   AActivation.Request.RawPath+'?'+LQueryParams], '')
        else
          Log.Info('Request [Environment: %s] [Method: %s] [Url: %s]',
                  [LEnvironment,
                   AActivation.Request.Method,
                   AActivation.Request.RawPath], '');
        {$ENDIF}

        if (SameText(AActivation.Request.method, 'PUT')) or
          (SameText(AActivation.Request.method, 'POST')) then
        begin
          {$IFDEF LOGGERPRO}
          Log.Info('Body:%s%s',
                  [
                    sLineBreak,
                    AActivation.Request.Content
                  ], '');
          {$ENDIF}
        end;
        if LEnvironment <> '' then
        begin
          {$IFNDEF BYPASS_TOKEN}
          if AActivation.HasToken and AActivation.Token.IsVerified then
          begin
            LTokenEnvironment := AActivation.Token.Claims.Values[CLAIM_ENVIRONMENT].ToString;
            {$IFDEF LOGGERPRO}
            Log.Info('User [UserName: %s]',
                [
                  AActivation.Token.UserName
                ], '');
            {$ENDIF}
            //check that the Request environment param is the same stored in Auth Token
            if not SameText(LTokenEnvironment,LEnvironment) then
              raise EMARSServerException.CreateError(http_406_NotAcceptable,
                Format('%s header param %s not equal to token.environment %s.',
                 [PARAM_ENVIRONMENT, LEnvironment, LTokenEnvironment]));
            (*
              //check that the Request servicekey param is the same stored in Auth Token
              if not SameText(LTokenServiceKey,LServiceKey) then
                raise EWiRLServerException.CreateError(http_406_NotAcceptable,
                  Format('%s header param %s not equal to token.servicekey %s.',
                   [PARAM_SERVICEKEY, LServiceKey, LTokenServiceKey]));
            *)
          end;
          {$ENDIF}
        end
        else if not LRequestSwagger then
        begin
          raise EMARSServerException.CreateError(
              http_406_NotAcceptable,
              Format('Missing %s Header param.',[PARAM_ENVIRONMENT]));
        end;
        {$IFNDEF BYPASS_TOKEN}
        //Check for ServiceKey
        LServiceKeyId := string(AActivation.Request.GetHeaderParamValue(PARAM_SERVICEKEY));
        if LServiceKeyId <> '' then
        begin
          //In dev or UAT environment accept also clear "backoffice" and "frontend" string
          //In Prod environment accept only hash MD5 of "backofficeYYYYMMDD" or "frontendYYYYMMDD" string
          LHashBackoffice := THashMD5.GetHashString('backoffice'+DateTimeToYYYYMMDD(Date()));
          LHashFrontend := THashMD5.GetHashString('frontend'+DateTimeToYYYYMMDD(Date()));
          (*
          LServiceKeyPassed :=
            (SameText(LServiceKeyId,LHashBackoffice) or SameText(LServiceKeyId,LHashFrontend)) or
            (SameText(LEnvironment,'test') and
             (SameText(LServiceKeyId,'backoffice') or SameText(LServiceKeyId,'frontend')));
          if not LServiceKeyPassed then
          begin
            raise EMARSServerException.CreateError(http_406_NotAcceptable,
              Format('Invalid %s header param for service [%s] %s.',
              [PARAM_SERVICEKEY, AActivation.Request.Method, AActivation.Request.RawPath]));
          end;
          *)
        end
        else if not LRequestSwagger then
        begin
          raise EMARSServerException.CreateError(http_406_NotAcceptable,
            Format('Missing %s header param.', [PARAM_SERVICEKEY]));
        end;
        {$ENDIF}
      end
    );
{$ENDREGION}
{$REGION 'Global AfterInvoke handler'}
    // After Invoke
    TMARSActivation.RegisterAfterInvoke(
      procedure (const AActivation: IMARSActivation)
      var
        LOutputStream: TBytesStream;
      begin
        //Metodo invocato dopo l'invocazione di qualsiasi richiesta
        AActivation.Response.SetHeader('language','it-IT');

        // Compression
        if FEngine.Parameters.ByName('Compression.Enabled').AsBoolean and
        ContainsText(AActivation.Request.GetHeaderParamValue('Accept-Encoding'), 'gzip')  then
        begin
          LOutputStream := TBytesStream.Create(nil);
          try
            ZipStream(AActivation.Response.ContentStream, LOutputStream, 15 + 16);
            AActivation.Response.ContentStream.Free;
            AActivation.Response.ContentStream := LOutputStream;
            AActivation.Response.ContentEncoding := 'gzip';
          except
            LOutputStream.Free;
            raise;
          end;
        end;
      end
    );
{$ENDREGION}
{$REGION 'Global InvokeError handler'}
    // to execute something on error
    TMARSActivation.RegisterInvokeError(
      procedure (const AActivation: IMARSActivation; const AException: Exception; var AHandled: Boolean)
      begin
        begin
          AActivation.Response.ContentType := TMediaType.APPLICATION_JSON;
          if AException is EMARSServerException then
            AActivation.Response.Content := EMARSServerException(AException).GetErrorJSONContent
          else
            AActivation.Response.Content := EMARSServerException.ComposeErrorJSONContent(
              AActivation.Response.StatusCode, AException.Message);
          {$IFDEF LOGGERPRO}
          Log.Error('Request Error (an exception will be raised) [STATUS: %d][BODY: %s]',
                [
                  AActivation.Response.StatusCode,
                  AException.Message
                ], '');
          {$ENDIF}
          AHandled := True;
        end;
      end
    );
{$ENDREGION}
  except
    on E : Exception do
    begin
      {$IFDEF LOGGERPRO}
      Log.Error('Error (an exception will be raised) [Message: %s]',
            [
              E.Message
            ], '');
      {$ENDIF}
      raise;
    end;
  end;
end;

class destructor TServerEngine.DestroyEngine;
begin
{$IFDEF IO_NO_DEFAULT_CONNECTOR}
  TMARSFireDAC.CloseConnectionDefs(FAvailableConnectionDefs);
{$ENDIF}
end;
end.
