(*
 *   InstantObject with MARS Curiosity REST Library
 *   MessageBodyReaders
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
unit InstantObjects.Neon.MessageBodyReaders;

interface

uses
  System.Classes
  , System.SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyReader
  , MARS.Core.Activation.Interfaces
  , MARS.Core.RequestAndResponse.Interfaces
  , InstantPersistence
  , InstantObjects.MARS.Data
  ;

type
  [Consumes(TMediaType.APPLICATION_JSON)]
  TInstantObjectReader = class(TInterfacedObject, IMessageBodyReader)
  private
  public
    [Context] Request: IMARSRequest;
    //[Context] InstantObject: TMARSInstantObjects;
    function ReadFrom(
      const AInputData: TBytes;
      const ADestination: TRttiObject; const AMediaType: TMediaType;
      const AActivation: IMARSActivation
    ): TValue; virtual;
  end;

implementation

uses
  System.TypInfo
  , System.JSON
  , System.Generics.Collections
  , MARS.Core.JSON
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  , MARS.Core.MessageBodyReaders
  , Neon.Core.Types
  , Neon.Core.Persistence.JSON
  , Neon.Core.Persistence
  , InstantClasses
  , InstantFireDAC
  , Instant.Neon.Serializers
  ;

{ TInstantObjectReader }

function TInstantObjectReader.ReadFrom(const AInputData: TBytes;
  const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation): TValue;
var
  LMARSInstantObjects: TMARSInstantObjects;
  LConnector: TInstantConnector;
  LInstantObjectClass: TInstantObjectClass;
  LInstantObject: TInstantObject;
  LJSONObject: TJSONObject;
  LClassName, LIdPropName, LIdAltPropName, LId: string;
  LJSONValue: TJSONValue;
  LReader: TNeonDeserializerJSON;
begin
  Result := TValue.Empty;
  LJSONObject := TJSONValueReader.ReadJSONValue(AInputData, ADestination, AMediaType,
   AActivation).AsType<TJSONObject>;
  if not Assigned(LJSONObject) then
    Exit;
  try
    //Retrieve ClassName and Id from JSON
    LJSonValue := LJSONObject.GetValue(IO_SER_CLASSNAME);
    try
      if Assigned(LJSonValue) then
        LClassName := LJSonValue.Value
      else if ADestination is TRttiParameter then
      begin
        //Retrieve ClassName from Resource
        LClassName := TRttiParameter(ADestination).ParamType.Name;
      end;
      if TInstantCustomSerializer.RetrieveInstantObjectClass(LClassName,
        LIdPropName, LIdAltPropName, LInstantObjectClass) then
      begin
        LJSONValue := LJSONObject.GetValue(LIdPropName);
        if Assigned(LJSONValue) then
          LId := LJSONValue.Value;
        if (LClassName <> '') then
        begin
           LMARSInstantObjects := TMARSInstantObjects.Create(AActivation);
           LConnector := LMARSInstantObjects.Connector;
          try
            LInstantObject := TInstantCustomSerializer.RetrieveOrCreateInstantObject(
                LClassName, LId, LConnector);

            if Assigned(LInstantObject) then
            begin
              LJSONValue := TJSONObject.ParseJSONValue(TEncoding.ANSI.GetString(AInputData));
              LReader := TNeonDeserializerJSON.Create(InstantNeonSerializerConfig);
              try
                LReader.JSONToObject(LInstantObject, LJSONValue);
                if LReader.Errors.Count > 0 then
                  raise EInstantError.CreateFmt('Errors on reading JSON object: %s', [LReader.Errors.Text]);
              finally
                LReader.Free;
              end;
              Result := LInstantObject;
            end;
            //Per dire a MARS di eliminare l'oggetto alla fine della request
            AActivation.AddToContext(LMARSInstantObjects);
          except
            if Assigned(LMARSInstantObjects) then
              LMARSInstantObjects.Free;
            raise;
          end;
        end;
      end
      else
        raise Exception.CreateFmt('Class %s is not an InstantObject Class', [LClassName]);
    finally
      LJSonValue.Free;
    end
  finally
    LJSONObject.Free;
  end;
end;

procedure RegisterReaders;
begin
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TInstantObject>(TInstantObjectReader);
end;


initialization
  RegisterReaders;

end.
