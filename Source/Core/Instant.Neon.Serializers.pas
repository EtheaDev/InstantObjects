unit Instant.Neon.Serializers;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.SyncObjs, System.TypInfo,
  System.Generics.Collections, System.JSON,
  InstantPersistence,
  Neon.Core.Types,
  Neon.Core.Attributes,
  Neon.Core.Persistence;

type
  //Base class to retrieve objects (Accessing Connector)
  TInstantCustomSerializer = class(TCustomSerializer)
  public
    class function RetrieveInstantObjectClass(const AClassName: string;
      out AIdPropName: string; out AObjectClass: TInstantObjectClass) : boolean;

    class function RetrieveInstantObject(const AClassName, AId: string;
      const AConnector: TInstantConnector = nil) : TInstantObject; virtual;

    class function RetrieveOrCreateInstantObject(const AClassName, AId: string;
      const AConnector: TInstantConnector = nil) : TInstantObject; virtual;
  end;

  TInstantReferenceSerializer = class(TInstantCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  TInstantReferencesSerializer = class(TInstantCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  TInstantClassMetadatasSerializer = class(TInstantCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  TInstantAttributeMetadatasSerializer = class(TInstantCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  TInstantPartsSerializer = class(TInstantCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  TInstantObjectListSerializer = class(TInstantCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

  TInstantObjectReferenceListSerializer = class(TInstantCustomSerializer)
  protected
    class function GetTargetInfo: PTypeInfo; override;
    class function CanHandle(AType: PTypeInfo): Boolean; override;
  public
    function Serialize(const AValue: TValue; ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue; override;
    function Deserialize(AValue: TJSONValue; const AData: TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue; override;
  end;

function JSONToPersistentObject(AJSONstring: string;
  var AObject: TPersistent): TPersistent;

procedure PersistentObjectToJSON(AObject: TPersistent;
  out AJSONString: string);

procedure PersistentObjectToStream(AObject: TObject; AStream: TStream);

var
  InstantNeonSerializerConfig: INeonConfiguration;

implementation

uses
    Neon.Core.Utils
  , Neon.Core.Persistence.JSON
  , InstantClasses
  , InstantMetadata;

procedure InitSerializerConfigs;
var
  LMembers: TNeonMembersSet;
begin
  LMembers := [TNeonMembers.Standard];
  InstantNeonSerializerConfig := TNeonConfiguration.Default;

  // Case settings
  //InstantNeonSerializerConfig.SetMemberCustomCase(nil);
  //InstantNeonSerializerConfig.SetMemberCase(TNeonCase.CamelCase);
  //InstantNeonSerializerConfig.SetMemberCase(TNeonCase.SnakeCase);
  //InstantNeonSerializerConfig.SetMemberCase(TNeonCase.LowerCase);
  //InstantNeonSerializerConfig.SetMemberCase(TNeonCase.UpperCase);

  // Member type settings
  //LMembers := LMembers + [TNeonMembers.Fields];
  LMembers := LMembers + [TNeonMembers.Properties];
  InstantNeonSerializerConfig.SetMembers(LMembers);

  // F Prefix setting
  //InstantNeonSerializerConfig.SetIgnoreFieldPrefix(True);

  // Use UTC Date
  InstantNeonSerializerConfig.SetUseUTCDate(True);

  // Pretty Printing
  InstantNeonSerializerConfig.SetPrettyPrint(True);

  // Visibility settings for object serializer
  InstantNeonSerializerConfig.SetVisibility([mvPublished]);

  // InstantObjects Serializers
  InstantNeonSerializerConfig.GetSerializers.RegisterSerializer(TInstantReferenceSerializer);
  InstantNeonSerializerConfig.GetSerializers.RegisterSerializer(TInstantReferencesSerializer);
  InstantNeonSerializerConfig.GetSerializers.RegisterSerializer(TInstantPartsSerializer);
  InstantNeonSerializerConfig.GetSerializers.RegisterSerializer(TInstantObjectListSerializer);
  InstantNeonSerializerConfig.GetSerializers.RegisterSerializer(TInstantObjectReferenceListSerializer);
  InstantNeonSerializerConfig.GetSerializers.RegisterSerializer(TInstantAttributeMetadatasSerializer);
  InstantNeonSerializerConfig.GetSerializers.RegisterSerializer(TInstantClassMetadatasSerializer);
end;

function JSONToPersistentObject(AJSONstring: string;
  var AObject: TPersistent): TPersistent;
var
  LJSONObject: TJSONObject;
  LJSonClassName: TJSONValue;
  LReader: TNeonDeserializerJSON;
  LClassName: string;
  LClass: TClass;
  LInstantClass: TInstantObjectClass;
begin
  LJSONObject := TJSONObject.ParseJSONValue(AJSONstring) as TJSONObject;
  if Assigned(LJSONObject) then
  begin
    try
      if not Assigned(AObject) then
      begin
        LJSonClassName := LJSONObject.GetValue(IO_SER_CLASSNAME);
        if Assigned(LJSonClassName) then
        begin
          LClassName := LJSonClassName.Value;
          LClass := GetClass(LClassName);
          if Assigned(LClass) and LClass.InheritsFrom(TInstantObject) then
          begin
            LInstantClass := TInstantObjectClass(LClass);
            AObject := LInstantClass.Create;
          end;
        end
      end;
      if not Assigned(AObject) then
        raise EInstantError.CreateFmt('Cannot read object from JSON string "%s",',
          [AJSONstring]);
        LReader := TNeonDeserializerJSON.Create(InstantNeonSerializerConfig);
      try
        LReader.JSONToObject(AObject, LJSONObject);
        if LReader.Errors.Count > 0 then
          raise EInstantError.CreateFmt(
            'Error converting JSON to Object: "%s"',
            [LReader.Errors.Text]);
        Result := AObject;
      finally
        LReader.Free;
      end;
    finally
      LJSONObject.Free;
    end;
  end
  else
  begin
    FreeAndNil(AObject);
    Result := AObject;
  end;
end;

procedure PersistentObjectToJSON(AObject: TPersistent;
  out AJSONString: string);
var
  LJSON: TJSONValue;
  LWriter: TNeonSerializerJSON;
begin
  LWriter := TNeonSerializerJSON.Create(InstantNeonSerializerConfig);
  try
    LJSON := LWriter.ObjectToJSON(AObject);
    try
      if LWriter.Errors.Count > 0 then
        raise EInstantError.CreateFmt(
          'Error converting Object to JSON: "%s"',
          [LWriter.Errors.Text]);
      AJSONString := LJSON.ToString;
    finally
      LJSON.Free;
    end;
  finally
    LWriter.Free;
  end;
end;

procedure PersistentObjectToStream(AObject: TObject; AStream: TStream);
var
  LJSON: TJSONValue;
begin
  LJSON := TNeon.ObjectToJSON(AObject, InstantNeonSerializerConfig);
  try
    TNeon.PrintToStream(LJSON, AStream, True);
  finally
    LJSON.Free;
  end;
end;

{ TInstantReferenceSerializer }

class function TInstantReferenceSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TInstantReference);
end;

function TInstantReferenceSerializer.Serialize(const AValue: TValue; ANeonObject:
    TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LVal: TInstantReference;
  LObjRef: TInstantObjectReference;
  LJSONValue: TJSONValue;
begin
  LVal := AValue.AsType<TInstantReference>;

  if (not Assigned(LVal)) or (LVal.ObjectId = '') then
    Exit(nil);

  //Serialize the Reference
  LObjRef := LVal.ObjectReference;
  //Serialize an TInstantObjectReference
  LJSONValue := AContext.WriteDataMember(LObjRef);
  Result := LJSONValue;
end;

class function TInstantReferenceSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := AType = TypeInfo(TInstantReference);
end;

function TInstantReferenceSerializer.Deserialize(AValue: TJSONValue; const AData:
  TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
var
  LJSONObject: TJSONObject;
  LJSonValue, LJSonValueId: TJSONValue;
  LClass: TPersistentClass;
  LClassName, LId: String;
  LClassAndIdInJSON: Boolean;
  LInstantObject: TInstantObject;
  LReference: TInstantReference;
  LType: TRttiType;
  LProperty: TRttiProperty;
begin
  LReference := AData.AsObject as TInstantReference;

  if not (AValue is TJSONNull) then
    LJSONObject := AValue as TJSONObject
  else
    LJSONObject := nil;
  if Assigned(LJSONObject) then
  begin
    //Read classname from JSON
    LJSonValue := LJSONObject.GetValue(IO_SER_CLASSNAME);
    if Assigned(LJSonValue) then
    begin
      LClassName := LJSonValue.Value;
      LClassAndIdInJSON := True;
    end
    else
    begin
      LClassName := LReference.RequiredClassName;
      LClassAndIdInJSON := False;
    end;
    //Read Id from JSON
    LJSonValueId := LJSONObject.GetValue(IO_SER_ID);
    if Assigned(LJSonValueId) then
    begin
      LId := LJSonValueId.Value;
    end
    else
    begin
      LClassAndIdInJSON := False;
      LClass := LReference.RequiredClass;
      if LClass.InheritsFrom(TInstantObject) then
      begin
        LJSonValueId := LJSONObject.GetValue(TInstantObjectClass(LClass).GetSerializedIdPropertyName);

        if Assigned(LJSonValueId) then
          LId := LJSonValueId.Value;
      end;
    end;

    if LId <> '' then
    begin
      LInstantObject := RetrieveOrCreateInstantObject(LClassName, LId, LReference.Connector);
      try
        //If the referenced object contains also data member, deserialize the object to update data
        if (LJSONObject.Count > 2) or not LClassAndIdInJSON then
        begin
          //read referenced object data from JSON
          LType := TRttiUtils.Context.GetType(LInstantObject.ClassType);
          AContext.ReadDataMember(LJSONObject, LType, LInstantObject);
        end;
        //Try to call property Setter of the OwnerObject Property corresponding to the OwnerObject Reference
        Assert(Assigned(LReference.Owner));
        LType := TRttiUtils.Context.GetType(LReference.Owner.ClassInfo);
        LProperty := LType.GetProperty(LReference.Name);
        if Assigned(LProperty) and (LProperty.IsWritable) then
          LProperty.SetValue(LReference.Owner, LInstantObject)
        else
          LReference.Value := LInstantObject;
      finally
        LInstantObject.Free;
      end;
      Result := LReference;
    end
    else
    begin
      //The Reference present in JSON don't link to any Object
      //Remove the reference if it's present
      if LReference.HasReference then
        LReference := nil;
      Result := LReference;
    end;
  end
  else
  begin
    //The Reference present in JSON don't link to any Object
    //Remove the reference if it's present
    if LReference.HasReference then
      LReference := nil;
    Result := LReference;
  end;
end;

{ TInstantReferencesSerializer }

class function TInstantReferencesSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TInstantReferences);
end;

function TInstantReferencesSerializer.Serialize(const AValue: TValue; ANeonObject:
    TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LVal: TInstantReferences;
  LJSON: TJSONArray;
  i : integer;
  LObjRef : TInstantObjectReference;
  LJSONValue : TJSONValue;
  LInstantObject: TInstantObject;
begin
  LVal := AValue.AsType<TInstantReferences>;

  if (not Assigned(LVal)) or (LVal.Count = 0) then
    Exit(nil);

  LJSON := TJSONArray.Create;
  try
    for i := 0 to LVal.Count-1 do
    begin
      LObjRef := LVal.RefItems[i];

      //Serialize an TInstantObjectReference
      LInstantObject := RetrieveInstantObject(LObjRef.ObjectClassName, LObjRef.ObjectId,
        LVal.Owner.Connector);
      try
        //Serialize the Object referenced
        LJSONValue := AContext.WriteDataMember(LInstantObject);
        LJSON.AddElement(LJSONValue);
      finally
        LInstantObject.Free;
      end;

    end;
  except
    FreeAndNil(LJSON);
    raise;
  end;
  Result := LJSON;
end;

class function TInstantReferencesSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := AType = TypeInfo(TInstantReferences);
end;

function TInstantReferencesSerializer.Deserialize(AValue: TJSONValue; const AData:
  TValue; ANeonObject: TNeonRttiObject; AContext: IDeserializerContext): TValue;
var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  LJSonValue, LJSonValueId: TJSONValue;
  LClass: TPersistentClass;
  LIndex, I: Integer;
  LClassName, LId: String;
  LClassAndIdInJSON: Boolean;
  LInstantObject: TInstantObject;
  LReferences: TInstantReferences;
  LObjectsFound: TList<TInstantObject>;
  LType: TRttiType;
begin
  LJSONArray := AValue as TJSONArray;
  LReferences := AData.AsObject as TInstantReferences;

  LObjectsFound := TList<TInstantObject>.Create;
  Try
    //Add new Referenced objects
    for I := 0 to LJSONArray.Count - 1 do
    begin
      LJSONObject := LJSONArray.Items[I] as TJSONObject;
      //Read classname from JSON
      LJSonValue := LJSONObject.GetValue(IO_SER_CLASSNAME);
      if Assigned(LJSonValue) then
      begin
        LClassName := LJSonValue.Value;
        LClassAndIdInJSON := True;
      end
      else
      begin
        LClassName := LReferences.RequiredClassName;
        LClassAndIdInJSON := False;
      end;

      //Read Id from JSON
      LJSonValueId := LJSONObject.GetValue(IO_SER_ID);
      if Assigned(LJSonValueId) then
      begin
        LId := LJSonValueId.Value;
      end
      else
      begin
        LClassAndIdInJSON := False;
        LClass := LReferences.RequiredClass;
        if LClass.InheritsFrom(TInstantObject) then
        begin
          LJSonValueId := LJSONObject.GetValue(TInstantObjectClass(LClass).GetSerializedIdPropertyName);

          if Assigned(LJSonValueId) then
            LId := LJSonValueId.Value;
        end;
      end;

      if LId <> '' then
      begin
        LInstantObject := RetrieveOrCreateInstantObject(LClassName, LId, LReferences.Connector);
        try
          //If the referenced object contains also data member, deserialize the object to update data
          if (LJSONObject.Count > 2) or not LClassAndIdInJSON then
          begin
            LType := TRttiUtils.Context.GetType(LInstantObject.ClassType);
            AContext.ReadDataMember(LJSONObject, LType, LInstantObject);
          end;
          LIndex := LReferences.IndexOf(LInstantObject);
          if (LIndex < 0) then
            LReferences.Add(LInstantObject);
          LInstantObject.AddRef;
          LObjectsFound.Add(LInstantObject);
        finally
          LInstantObject.Free;
        end;
      end;
    end;

    //Remove referenced objects not found
    LReferences.Owner.AddRef;
    Try
      for I := LReferences.Count - 1 downto 0 do
      begin
        LInstantObject := LReferences[I];
        if LObjectsFound.IndexOf(LInstantObject) < 0 then
        begin
          LInstantObject.Dispose;
          LReferences.Delete(I);
        end;
      end;
    Finally
      LReferences.Owner.Free;
    End;
  Finally
    for I := 0 to LObjectsFound.Count-1 do
        LObjectsFound[I].Free;

    LObjectsFound.Free;
  End;

  Result := LReferences;
end;

{ TInstantPartsSerializer }

class function TInstantPartsSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := AType = TypeInfo(TInstantParts);
end;

function TInstantPartsSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LParts: TInstantParts;
begin
  LParts:= AData.AsObject as TInstantParts;

  Result := LParts;

end;

class function TInstantPartsSerializer.GetTargetInfo: PTypeInfo;
begin
   Result := TypeInfo(TInstantParts);
end;

function TInstantPartsSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LVal: TInstantParts;
  LJSON: TJSONArray;
  i : integer;
  LInstantObject : TInstantObject;
  LJSONValue : TJSONValue;
begin
  LVal := AValue.AsType<TInstantParts>;

//  if ANeonObject.NeonInclude.Value = IncludeIf.NotEmpty then
    if LVal.Count = 0 then
      Exit(nil);

  LJSON := TJSONArray.Create;
  try
    for i := 0 to LVal.Count-1 do
    begin
      LInstantObject := LVal.Items[i];
      LJSONValue := AContext.WriteDataMember(LInstantObject);
      LJSON.AddElement(LJSONValue );
    end;
  except
    FreeAndNil(LJSON);
    raise;
  end;
  Result := LJSON;
end;

{ TInstantObjectListSerializer }

class function TInstantObjectListSerializer.CanHandle(
  AType: PTypeInfo): Boolean;
begin
  Result := AType = TypeInfo(TInstantObjectList<TInstantObject>);
end;

function TInstantObjectListSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  LClassName, LId: String;
  LInstantObject: TInstantObject;
  LObjectList: TInstantObjectList<TInstantObject>;
  I: Integer;
begin
  LJSONArray := AValue as TJSONArray;
  LObjectList := AData.AsObject as TInstantObjectList<TInstantObject>;

  for I := 0 to LJSONArray.Count - 1 do
  begin
    LJSONObject := LJSONArray.Items[I] as TJSONObject;
    LClassName := LJSONObject.GetValue(IO_SER_CLASSNAME).Value;
    LId := LJSONObject.GetValue(IO_SER_ID).Value;
    LInstantObject := RetrieveInstantObject(LClassName, LId);
    if Assigned(LInstantObject) then
      LObjectList.Add(LInstantObject);
  end;

  Result := LObjectList;
end;

class function TInstantObjectListSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TInstantObjectList<TInstantObject>);
end;

function TInstantObjectListSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LVal: TInstantObjectList<TInstantObject>;
  LJSON: TJSONArray;
  i : integer;
  LInstantObject : TInstantObject;
  LJSONValue : TJSONValue;
begin
  LVal := AValue.AsType<TInstantObjectList<TInstantObject>>;

  LJSON := TJSONArray.Create;
  try
//  if ANeonObject.NeonInclude.Value = IncludeIf.NotEmpty then
    if LVal.Count = 0 then
      Exit(LJSON);

    for i := 0 to LVal.Count-1 do
    begin
      LInstantObject := LVal.Items[i];
      LJSONValue := AContext.WriteDataMember(LInstantObject);
      LJSON.AddElement(LJSONValue);
    end;
  except
    FreeAndNil(LJSON);
    raise;
  end;
  Result := LJSON;
end;

{ TInstantCustomSerializer }

class function TInstantCustomSerializer.RetrieveInstantObject(
  const AClassName, AId: string;
  const AConnector: TInstantConnector = nil): TInstantObject;
var
  LClass: TPersistentClass;
  LInstantClass: TInstantObjectClass;
begin
  LClass := GetClass(AClassName);
  If Assigned(LClass) and LClass.InheritsFrom(TInstantObject) then
  begin
    LInstantClass := TInstantObjectClass(LClass);
    Result := LInstantClass.Retrieve(AId, False, False, AConnector);
    if not Assigned(Result) then
      raise Exception.CreateFmt('Object not found. (ClassName=%s - Id=%s) ', [AClassName, AId]);
  end
  else
    raise Exception.CreateFmt('Class not found. (ClassName=%s ) ', [AClassName]);
end;

class function TInstantCustomSerializer.RetrieveInstantObjectClass(
  const AClassName: string; out AIdPropName: string;
  out AObjectClass: TInstantObjectClass): boolean;
var
  LClass: TPersistentClass;
begin
  LClass := GetClass(AClassName);
  Result := Assigned(LClass) and LClass.InheritsFrom(TInstantObject);
  if Result then
  begin
    AObjectClass := TInstantObjectClass(LClass);
    AIdPropName := AObjectClass.GetSerializedIdPropertyName;
  end
  else
  begin
    AObjectClass := nil;
    AIdPropName := '';
  end;
end;

class function TInstantCustomSerializer.RetrieveOrCreateInstantObject(
  const AClassName, AId: string;
  const AConnector: TInstantConnector = nil) : TInstantObject;
var
  LClass: TPersistentClass;
  LInstantClass: TInstantObjectClass;
begin
  LClass := GetClass(AClassName);
  If Assigned(LClass) and LClass.InheritsFrom(TInstantObject) then
  begin
    LInstantClass := TInstantObjectClass(LClass);
    if AId <> '' then
      Result := LInstantClass.Retrieve(AId, False, False, AConnector)
    else
      Result := nil;

    //if not found, crete the object
    if not Assigned(Result) then
    begin
      Result := LInstantClass.Create(AConnector);

      if AId <> '' then
        Result.Id := AId;
    end;
  end
  else
    raise Exception.CreateFmt('Class not found. (ClassName=%s ) ', [AClassName]);
end;

{ TInstantObjectReferenceListSerializer }

class function TInstantObjectReferenceListSerializer.CanHandle(
  AType: PTypeInfo): Boolean;
begin
  Result := AType = TypeInfo(TList<TInstantObjectReference>);
end;

function TInstantObjectReferenceListSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
begin
  Exit(nil);
end;

class function TInstantObjectReferenceListSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TList<TInstantObjectReference>);
end;

function TInstantObjectReferenceListSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LVal: TList<TInstantObjectReference>;
  LJSON: TJSONArray;
  i : integer;
  LInstantObjectReference : TInstantObjectReference;
  LJSONValue : TJSONValue;
begin
  LVal := AValue.AsType<TList<TInstantObjectReference>>;

  LJSON := TJSONArray.Create;
  try
    if LVal.Count = 0 then
      Exit(LJSON);

    for i := 0 to LVal.Count-1 do
    begin
      LInstantObjectReference := LVal[I];
      LJSONValue := AContext.WriteDataMember(LInstantObjectReference);
      LJSON.AddElement(LJSONValue);
    end;
  except
    FreeAndNil(LJSON);
    raise;
  end;
  Result := LJSON;
end;

{ TInstantAttributeMetadatasSerializer }

class function TInstantAttributeMetadatasSerializer.CanHandle(
  AType: PTypeInfo): Boolean;
begin
  Result := AType = TypeInfo(TInstantAttributeMetadatas);
end;

function TInstantAttributeMetadatasSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  LInstantAttributeMetadatas: TInstantAttributeMetadatas;
  LInstantAttributeMetadata: TInstantAttributeMetadata;
  LType: TRttiType;
  I: Integer;
begin
  LJSONArray := AValue as TJSONArray;
  LInstantAttributeMetadatas := AData.AsObject as TInstantAttributeMetadatas;

  //Add new InstantAttributeMetadata objects
  for I := 0 to LJSONArray.Count - 1 do
  begin
    LJSONObject := LJSONArray.Items[I] as TJSONObject;
    LInstantAttributeMetadata := LInstantAttributeMetadatas.Add;
    LType := TRttiUtils.Context.GetType(LInstantAttributeMetadata);
    AContext.ReadDataMember(LJSONObject, LType, LInstantAttributeMetadata);
  end;
  Result := LInstantAttributeMetadatas;
end;

class function TInstantAttributeMetadatasSerializer.GetTargetInfo: PTypeInfo;
begin
   Result := TypeInfo(TInstantAttributeMetadatas);
end;

function TInstantAttributeMetadatasSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LVal: TInstantAttributeMetadatas;
  LJSON: TJSONArray;
  i : integer;
  LInstantAttributeMetadata: TInstantAttributeMetadata;
  LJSONValue : TJSONValue;
begin
  LVal := AValue.AsType<TInstantAttributeMetadatas>;

//  if ANeonObject.NeonInclude.Value = IncludeIf.NotEmpty then
    if LVal.Count = 0 then
      Exit(nil);

  LJSON := TJSONArray.Create;
  try
    for i := 0 to LVal.Count-1 do
    begin
      LInstantAttributeMetadata := LVal.Items[i];
      LJSONValue := AContext.WriteDataMember(LInstantAttributeMetadata);
      LJSON.AddElement(LJSONValue );
    end;
  except
    FreeAndNil(LJSON);
    raise;
  end;
  Result := LJSON;
end;

{ TInstantClassMetadatasSerializer }

class function TInstantClassMetadatasSerializer.CanHandle(AType: PTypeInfo): Boolean;
begin
  Result := AType = TypeInfo(TInstantClassMetadatas);
end;

function TInstantClassMetadatasSerializer.Deserialize(AValue: TJSONValue;
  const AData: TValue; ANeonObject: TNeonRttiObject;
  AContext: IDeserializerContext): TValue;
var
  LJSONArray: TJSONArray;
  LJSONObject: TJSONObject;
  LInstantClassMetadatas: TInstantClassMetadatas;
  LInstantClassMetadata: TInstantClassMetadata;
  LType: TRttiType;
  I: Integer;
begin
  LJSONArray := AValue as TJSONArray;
  LInstantClassMetadatas := AData.AsObject as TInstantClassMetadatas;

  //Add new InstantClassMetadata objects
  for I := 0 to LJSONArray.Count - 1 do
  begin
    LJSONObject := LJSONArray.Items[I] as TJSONObject;
    LInstantClassMetadata := LInstantClassMetadatas.Add;
    LType := TRttiUtils.Context.GetType(TInstantClassMetadata);
    AContext.ReadDataMember(LJSONObject, LType, LInstantClassMetadata);
  end;
  Result := LInstantClassMetadatas;
end;

class function TInstantClassMetadatasSerializer.GetTargetInfo: PTypeInfo;
begin
  Result := TypeInfo(TInstantClassMetadatas);
end;

function TInstantClassMetadatasSerializer.Serialize(const AValue: TValue;
  ANeonObject: TNeonRttiObject; AContext: ISerializerContext): TJSONValue;
var
  LVal: TInstantClassMetadatas;
  LJSON: TJSONArray;
  i : integer;
  LInstantClassMetadata: TInstantClassMetadata;
  LJSONValue : TJSONValue;
begin
  LVal := AValue.AsType<TInstantClassMetadatas>;

//  if ANeonObject.NeonInclude.Value = IncludeIf.NotEmpty then
    if LVal.Count = 0 then
      Exit(nil);

  LJSON := TJSONArray.Create;
  try
    for i := 0 to LVal.Count-1 do
    begin
      LInstantClassMetadata := LVal.Items[i];
      LJSONValue := AContext.WriteDataMember(LInstantClassMetadata);
      LJSON.AddElement(LJSONValue );
    end;
  except
    FreeAndNil(LJSON);
    raise;
  end;
  Result := LJSON;
end;

initialization
  InitSerializerConfigs;

end.
