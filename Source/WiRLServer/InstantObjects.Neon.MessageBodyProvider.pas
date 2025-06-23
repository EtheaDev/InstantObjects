(*
 *   InstantObject with WiRL Curiosity REST Library
 *   MessageBody Reader and Writer for InstantObjects
 *)
unit InstantObjects.Neon.MessageBodyProvider;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  WiRL.Core.Classes,
  WiRL.Core.Attributes,
  WiRL.Core.Declarations,
  WiRL.http.Core,
  WiRL.Rtti.Utils,
  WiRL.http.Request,
  WiRL.http.Response,
  WiRL.http.Headers,
  WiRL.http.Accept.MediaType,
  WiRL.Core.MessageBodyWriter,
  WiRL.Core.MessageBodyReader,
  WiRL.Core.MessageBody.Classes,
  WiRL.Core.Exceptions,
  InstantPersistence,
  InstantObjects.WiRL.Data
  ;

type
  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  TWiRLInstantObjectsProvider = class(TMessageBodyProvider)
  public
    [Context] InstantObject: TWiRLInstantObjects;

    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

  [Consumes(TMediaType.APPLICATION_JSON)]
  [Produces(TMediaType.APPLICATION_JSON)]
  TWiRLInstantObjectsListProvider = class(TMessageBodyProvider)
  public
    [Context] InstantObject: TWiRLInstantObjects;

    function ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue; override;

    procedure WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream); override;
  end;

implementation

uses
  InstantClasses
  , InstantFireDAC
  , Instant.Neon.Serializers
  , System.JSON
  , Neon.Core.Persistence.JSON
  ;

{ TWiRLInstantObjectsProvider }

function TWiRLInstantObjectsProvider.ReadFrom(AType: TRttiType; AMediaType: TMediaType;
      AHeaders: IWiRLHeaders; AContentStream: TStream): TValue;
var
  LConnector: TInstantConnector;
  LInstantObjectClass: TInstantObjectClass;
  LInstantObject: TInstantObject;
  LJSONObject: TJSONObject;
  LClassName, LIdPropName, LIdAltPropName, LId: string;
  LJSONValue: TJSONValue;
  LReader: TNeonDeserializerJSON;
begin
  Result := TValue.Empty;
  LJSONValue := TJSONObject.ParseJSONValue(
    ContentStreamToString(AMediaType.Charset, AContentStream));
  if LJSONValue is TJSONObject then
    LJSONObject := TJSONObject(LJSONValue)
  else
    Exit;
  LId := '';
  //Retrieve ClassName and Id from JSON
  LJSONValue := LJSONObject.GetValue(IO_SER_CLASSNAME);
  try
    if Assigned(LJSonValue) then
      LClassName := LJSonValue.Value;
    (*
    else if ADestination is TRttiParameter then
    begin
      //Retrieve ClassName from Resource
      LClassName := TRttiParameter(ADestination).ParamType.Name;
    end;
    *)
    if TInstantCustomSerializer.RetrieveInstantObjectClass(LClassName,
      LIdPropName, LIdAltPropName, LInstantObjectClass) then
    begin
      LJSONValue := LJSONObject.GetValue(LIdPropName);
      if Assigned(LJSONValue) then
        LId := LJSONValue.Value;
      if (LId = '') or (LId = '0') then
      begin
        LJSONValue := LJSONObject.GetValue(LIdAltPropName);
        if Assigned(LJSONValue) then
          LId := 'A'+LJSONValue.Value;
      end;
      if (LClassName <> '') then
      begin
         LConnector := InstantObject.Connector;
        LInstantObject := TInstantCustomSerializer.RetrieveOrCreateInstantObject(
            LClassName, LId, LConnector);

        if Assigned(LInstantObject) then
        begin
          //LJSONValue := TJSONObject.ParseJSONValue(TEncoding.ANSI.GetString(AInputData));
          LReader := TNeonDeserializerJSON.Create(InstantNeonSerializerConfig);
          try
            LReader.JSONToObject(LInstantObject, LJSONObject);

            if LReader.Errors.Count > 0 then
              raise EInstantError.CreateFmt('Errors on reading JSON object: %s', [LReader.Errors.Text]);
          finally
            LReader.Free;
          end;
          Result := LInstantObject;
        end;
      end;
    end
    else
      raise Exception.CreateFmt('Class %s is not an InstantObject Class', [LClassName]);
  finally
    LJSonValue.Free;
  end;
end;

procedure TWiRLInstantObjectsProvider.WriteTo(const AValue: TValue; const AAttributes: TAttributeArray;
      AMediaType: TMediaType; AHeaders: IWiRLHeaders; AContentStream: TStream);
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
        TNeon.PrintToStream(LJSONValue, AContentStream,
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

procedure RegisterMessageBodyClasses;
begin
  //Custom MessageBody Reader for TInstantObject class and descendants
  TMessageBodyReaderRegistry.Instance.RegisterReader(TWiRLInstantObjectsProvider, TInstantObject,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );

  //Custom MessageBody Writer for TInstantObject class and descendants
  TMessageBodyWriterRegistry.Instance.RegisterWriter(TWiRLInstantObjectsProvider, TInstantObject,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );
  //Custom MessageBody Writer for TInstantObjectList class and descendants
  TMessageBodyWriterRegistry.Instance.RegisterWriter(TWiRLInstantObjectsListProvider, TInstantObjectList<TInstantObject>,
    function (AType: TRttiType; const AAttributes: TAttributeArray; AMediaType: TMediaType): Integer
    begin
      Result := TMessageBodyWriterRegistry.AFFINITY_HIGH;
    end
  );
end;

{ TWiRLInstantObjectsListProvider }

function TWiRLInstantObjectsListProvider.ReadFrom(AType: TRttiType;
  AMediaType: TMediaType; AHeaders: IWiRLHeaders;
  AContentStream: TStream): TValue;
begin
  Result := TValue.Empty;
end;

procedure TWiRLInstantObjectsListProvider.WriteTo(const AValue: TValue;
  const AAttributes: TAttributeArray; AMediaType: TMediaType;
  AHeaders: IWiRLHeaders; AContentStream: TStream);
var
  LInstantObjectList: TInstantObjectList<TInstantObject>;
begin
  LInstantObjectList := AValue.AsObject as TInstantObjectList<TInstantObject>;
  if Assigned(LInstantObjectList) then
  begin
    TNeon.ValueToStream(LInstantObjectList, AContentStream, InstantNeonSerializerConfig);
  end;
end;

initialization
  RegisterMessageBodyClasses;

end.
