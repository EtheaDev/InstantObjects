(*
 *   InstantObject with MARS Curiosity REST Library
 *   MARS Data
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
unit InstantObjects.MARS.Data;

{$I MARS.inc}

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Types
  , MARS.Core.Injection
  , MARS.Core.Injection.Interfaces
  , MARS.Core.Injection.Types
  , MARS.Core.Activation.Interfaces
  , InstantObjects.MARS.Server.Exceptions
  , InstantPersistence
  , InstantFireDAC
;

type
  TMARSInstantObjects = class
  private
    FConnectionName: string;
    FInstantFireDACConnector: TInstantFireDACConnector;
    function GetInstantFireDACConnector: TInstantFireDACConnector;
  public
    constructor Create(AActivation: IMARSActivation);
    procedure ConnectToDatabase;
    property ConnectionName: string read FConnectionName;
    property Connector: TInstantFireDACConnector read GetInstantFireDACConnector;
  end;

implementation

uses
  MARS.Rtti.Utils
  , MARS.Data.FireDAC
  , InstantClasses
  , InstantObjects.MARS.InjectionService
  , FireDAC.Comp.Client
;

{ TMARSInstantObjects }

procedure TMARSInstantObjects.ConnectToDatabase;
begin
  Assert(Assigned(Connector));
  try
    Connector.Connect;
  except
    on E: Exception do
      raise EMARSServerException.CreateError(
        http_401_Unauthorized,
        Format('Connection to database "%s" failed with error: %s',
          [FConnectionName, E.Message]));
  end;
end;

constructor TMARSInstantObjects.Create(AActivation: IMARSActivation);
var
  LFDConnection: TFDConnection;
begin
  FConnectionName := AActivation.Request.GetHeaderParamValue('environment');
  if FConnectionName = '' then
    FConnectionName := 'test';
  LFDConnection := TMARSFireDAC.CreateConnectionByDefName(FConnectionName);
  if Assigned(LFDConnection) then
  begin
    FInstantFireDACConnector := TInstantFireDACConnector.Create(nil);
    try
      FInstantFireDACConnector.BlobStreamFormat := sfXML;
      FInstantFireDACConnector.Connection := LFDConnection;
      FInstantFireDACConnector.loginPrompt := False;
      //Per dire a MARS di eliminare l'oggetto alla fine della request
      AActivation.AddToContext(FInstantFireDACConnector);
    except
      LFDConnection.Free;
      FInstantFireDACConnector.Free;
      raise;
    end;
    //Per dire a MARS di eliminare l'oggetto alla fine della request
    AActivation.AddToContext(LFDConnection);
  end
  else
  begin
    raise EMARSServerException.CreateError(
        http_406_NotAcceptable,
        Format('Connection for %s environment not configured',[FConnectionName]));
  end;
end;

function TMARSInstantObjects.GetInstantFireDACConnector: TInstantFireDACConnector;
begin
  Result := FInstantFireDACConnector;
end;

end.
