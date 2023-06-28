(*
 *   InstantObject with MARS Curiosity REST Library
 *   MessageBodyWriters
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
unit InstantObjects.Neon.MessageBodyWriters;

interface

uses
  Classes, SysUtils, Rtti

  , MARS.Core.Attributes
  , MARS.Core.Declarations
  , MARS.Core.MediaType
  , MARS.Core.MessageBodyWriter
  , MARS.Core.Activation.Interfaces
  ;

type

  [Produces(TMediaType.APPLICATION_JSON)]
  TInstantObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TInstantObjectListWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Produces(TMediaType.APPLICATION_JSON)]
  TInstantObjectReferenceListWriter = class(TInterfacedObject, IMessageBodyWriter)
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

implementation

uses
  //Delphi
  System.TypInfo
  , System.JSON
  , System.Generics.Collections
  //MARS
  , MARS.Core.JSON
  , MARS.Core.Utils
  , MARS.Rtti.Utils
  //NEON
  , Neon.Core.Types
  , Neon.Core.Persistence.JSON
  , Neon.Core.Persistence
  //InstantObject + NEON
  , Instant.Neon.Serializers
  //InstantObject
  , InstantPersistence
  , InstantClasses
  ;


{ TInstantObjectWriter }

procedure TInstantObjectWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LInstantObject: TInstantObject;
  LWriter: TNeonSerializerJSON;
  LJSONValue: TJSONValue;
begin
  LInstantObject := AValue.AsObject as TInstantObject;
  if Assigned(LInstantObject) then
  begin
    LWriter := TNeonSerializerJSON.Create(InstantNeonSerializerConfig);
    try
      //For an Empty Object returns {}
      if Assigned(LInstantObject) then
        LJSONValue := LWriter.ValueToJSON(AValue)
      else
        LJSonValue := TJSONObject.Create;
      try
        TNeon.PrintToStream(LJSONValue, AOutputStream,
          {$IFDEF DEBUG}True{$ELSE}False{$ENDIF}); //Pretty Print only in Debug
        if LWriter.Errors.Count > 0 then
          raise EInstantError.CreateFmt('Errors on writing JSON object: %s', [LWriter.Errors.Text]);
      finally
        LJSONValue.Free;
      end;
    finally
      LWriter.Free;
    end;
  end;
end;

{ TInstantObjectListWriter }

procedure TInstantObjectListWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LInstantObjectList: TInstantObjectList<TInstantObject>;
begin
  LInstantObjectList := AValue.AsObject as TInstantObjectList<TInstantObject>;
  if Assigned(LInstantObjectList) then
  begin
    TNeon.ValueToStream(LInstantObjectList, AOutputStream, InstantNeonSerializerConfig);
  end;
end;

procedure RegisterWriters;
begin
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TInstantObject>(TInstantObjectWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TInstantObjectList<TInstantObject>>(TInstantObjectListWriter);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TList<TInstantObjectReference>>(TInstantObjectReferenceListWriter);
end;

{ TInstantObjectReferenceListWriter }

procedure TInstantObjectReferenceListWriter.WriteTo(const AValue: TValue;
  const AMediaType: TMediaType; AOutputStream: TStream;
  const AActivation: IMARSActivation);
var
  LInstantObjectList: TList<TInstantObjectReference>;
begin
  LInstantObjectList := AValue.AsObject as TList<TInstantObjectReference>;
  if Assigned(LInstantObjectList) then
    TNeon.ValueToStream(LInstantObjectList, AOutputStream, InstantNeonSerializerConfig);
end;

initialization
  RegisterWriters;

end.
