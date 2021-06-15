(*
 *   InstantObjects
 *   Metadata Classes
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
 * The Original Code is: Seleqt InstantObjects
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Carlo Barazzetta, Andrea Petrelli, Nando Dessena, Steven Mitchell,
 * Joao Morais, Cesar Coll, Uberto Barbini, David Taylor, Hanedi Salas,
 * Riceball Lee, David Moorhouse, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantMetadata;

{$I '..\InstantDefines.inc'}

interface

uses
  Classes
  , Contnrs
  , Db
  {$IFDEF DELPHI_NEON}
  , Neon.Core.Types
  , Neon.Core.Nullables
  , Neon.Core.Attributes
  {$ENDIF}
  , InstantClasses
  , InstantTypes
  , InstantConsts;

type
  TInstantAttributeMap = class;
  TInstantAttributeMaps = class;
  TInstantAttributeMetadata = class;
  TInstantAttributeMetadatas = class;
  TInstantCatalog = class;
  TInstantClassMetadata = class;
  TInstantClassMetadatas = class;
  TInstantFieldMetadata = class;
  TInstantFieldMetadatas = class;
  TInstantIndexMetadata = class;
  TInstantIndexMetadatas = class;
  TInstantMetadatas = class;
  TInstantScheme = class;
  TInstantTableMetadata = class;
  TInstantTableMetadatas = class;

  TInstantMetadata = class(TInstantCollectionItem)
  private
    function GetCollection: TInstantMetadatas;
    procedure SetCollection(Value: TInstantMetadatas);
      {$IFDEF D6+}reintroduce; {$ENDIF}
  protected
    function InternalEquals(const Other: TInstantMetadata): Boolean; virtual;
  public
    function Equals(const Other: TInstantMetadata): Boolean; {$IFDEF D12+} reintroduce; {$ENDIF} overload;
    property Collection: TInstantMetadatas read GetCollection
      write SetCollection;
  end;

  TInstantClassMetadata = class(TInstantMetadata)
  private
    FAttributeMetadatas: TInstantAttributeMetadatas;
    FDefaultContainerName: string;
    FMemberMap: TInstantAttributeMap;
    FParent: TInstantClassMetadata;
    FParentName: string;
    FPersistence: TInstantPersistence;
    FStorageMap: TInstantAttributeMap;
    FStorageMaps: TInstantAttributeMaps;
    FStorageName: string;
    procedure BuildAttributeMap(Map: TInstantAttributeMap; Complete: Boolean);
    procedure BuildStorageMaps(Maps: TInstantAttributeMaps);
    procedure DestroyAttributeViews;
    function GetAttributeMetadatas: TInstantAttributeMetadatas;
    function GetCollection: TInstantClassMetadatas;
    procedure GetDistinctAttributeMetadatas(Map: TInstantAttributeMap);
    function GetIsEmpty: Boolean;
    function GetIsStored: Boolean;
    function GetMemberMap: TInstantAttributeMap;
    function GetParent: TInstantClassMetadata;
    function GetParentName: string;
    function GetStorageMap: TInstantAttributeMap;
    function GetStorageMaps: TInstantAttributeMaps;
    function GetTableName: string;
    procedure SetCollection(Value: TInstantClassMetadatas);
    procedure SetParent(Value: TInstantClassMetadata);
    procedure SetParentName(const Value: string);
    procedure SetTableName(const Value: string);
  protected
    procedure ClearParent;
    class procedure ConvertToBinary(Converter: TInstantTextToBinaryConverter); override;
    class procedure ConvertToText(Converter: TInstantBinaryToTextConverter); override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //{$IFDEF DELPHI_NEON}[neoninclude]{$ENDIF}
    property AttributeMetadatas: TInstantAttributeMetadatas
      read GetAttributeMetadatas;
    property Collection: TInstantClassMetadatas read GetCollection
      write SetCollection;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsStored: Boolean read GetIsStored;
    property MemberMap: TInstantAttributeMap read GetMemberMap;
    property Parent: TInstantClassMetadata read GetParent write SetParent;
    property StorageMap: TInstantAttributeMap read GetStorageMap;
    property StorageMaps: TInstantAttributeMaps read GetStorageMaps;
    {$IFDEF DELPHI_NEON}[neoninclude]{$ENDIF}
    property TableName: string read GetTableName write SetTableName;
  published
    property DefaultContainerName: string read FDefaultContainerName
      write FDefaultContainerName;
    property ParentName: string read GetParentName write SetParentName;
    property Persistence: TInstantPersistence read FPersistence
      write FPersistence;
    property StorageName: string read FStorageName write FStorageName;
  end;

  TInstantFieldMetadata = class(TInstantMetadata)
  private
    FDataType: TInstantDataType;
    FOptions: TInstantFieldOptions;
    FSize: Integer;
    FAlternateDataTypes: TInstantDataTypes;
    function GetCollection: TInstantFieldMetadatas;
    function GetTableMetadata: TInstantTableMetadata;
  protected
    function InternalEquals(const Other: TInstantMetadata): Boolean; override;
  public
    constructor Create(ACollection: TInstantFieldMetadatas); reintroduce;
    procedure Assign(Source: TPersistent); override;
    // Returns True if one of the data types of Other (Other.DataType and
    // Other.AlternateDataTypes) equals one of the data types of Self.
    function DataTypesEqual(const Other: TInstantFieldMetadata): Boolean;
    property Collection: TInstantFieldMetadatas read GetCollection;
    property DataType: TInstantDataType read FDataType write FDataType;
    // When field metadata is gathered from a database, there might be more
    // TInstantDataType values that apply (for example when the database
    // represents dtBoolean and dtInteger attributes with the same column type).
    // In that case, a datatype is chosen as the value of the DataType
    // property, and the others are put in AlternateDataTypes. The
    // DataTypesEqual method considers both DataType and AlternateDataTypes when
    // deciding upon data type "equality".
    property AlternateDataTypes: TInstantDataTypes
      read FAlternateDataTypes write FAlternateDataTypes;
    property Options: TInstantFieldOptions read FOptions write FOptions;
    property Size: Integer read FSize write FSize;
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

  TInstantIndexMetadata = class(TInstantMetadata)
  private
    FFields: string;
    FOptions: TIndexOptions;
    function GetCollection: TInstantIndexMetadatas;
    function GetTableMetadata: TInstantTableMetadata;
  protected
    function InternalEquals(const Other: TInstantMetadata): Boolean; override;
  public
    constructor Create(ACollection: TInstantMetadatas); reintroduce;
    procedure Assign(Source: TPersistent); override;
    property Collection: TInstantIndexMetadatas read GetCollection;
    // Returns True if the field identified by AFieldMetadata is part of this
    // index.
    function IsFieldIndexed(const AFieldMetadata: TInstantFieldMetadata):
      Boolean;
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  published
    property Fields: string read FFields write FFields;
    property Options: TIndexOptions read FOptions write FOptions;
  end;

  TInstantTableMetadata = class(TInstantMetadata)
  private
    FFieldMetadatas: TInstantFieldMetadatas;
    FIndexMetadatas: TInstantIndexMetadatas;
    function GetFieldMetadatas: TInstantFieldMetadatas;
    function GetIndexMetadatas: TInstantIndexMetadatas;
    function GetScheme: TInstantScheme;
    function GetFieldMetadataCount: Integer;
    function GetIndexMetadataCount: Integer;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Scheme: TInstantScheme read GetScheme;
  published
    property FieldMetadataCount: Integer read GetFieldMetadataCount;
    property FieldMetadatas: TInstantFieldMetadatas read GetFieldMetadatas;
    function FindFieldMetadata(const AName: string): TInstantFieldMetadata;
    function FindIndexMetadata(const AName: string): TInstantIndexMetadata;
    property IndexMetadataCount: Integer read GetIndexMetadataCount;
    property IndexMetadatas: TInstantIndexMetadatas read GetIndexMetadatas;
  end;

  TInstantMetadatas = class(TInstantOwnedCollection)
  private
    function GetItems(Index: Integer): TInstantMetadata;
    procedure SetItems(Index: Integer; const Value: TInstantMetadata);
  public
    function Find(const AName: string): TInstantMetadata;
    property Items[Index: Integer]: TInstantMetadata read GetItems
      write SetItems; default;
  end;

  TInstantClassMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantClassMetadata;
    procedure SetItems(Index: Integer; Value: TInstantClassMetadata);
  protected
    class function CreateInstance(Arg: Pointer = nil): TInstantCollection; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInstantClassMetadata;
    function Find(const AName: string): TInstantClassMetadata;
    property Items[Index: Integer]: TInstantClassMetadata read GetItems
      write SetItems; default;
  end;

  TInstantFieldMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantFieldMetadata;
    procedure SetItems(Index: Integer; Value: TInstantFieldMetadata);
  public
    constructor Create(AOwner: TInstantTableMetadata);
    procedure AddFieldMetadata(const AName: string; ADataType: TInstantDataType;
      ASize: Integer; AOptions: TInstantFieldOptions = []);
    function Add: TInstantFieldMetadata;
    function Find(const AName: string): TInstantFieldMetadata;
    property Items[Index: Integer]: TInstantFieldMetadata read GetItems
      write SetItems; default;
    function Owner: TInstantTableMetadata;
  end;

  TInstantIndexMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantIndexMetadata;
    procedure SetItems(Index: Integer; Value: TInstantIndexMetadata);
  public
    constructor Create(AOwner: TInstantTableMetadata);
    procedure AddIndexMetadata(const AName, AFields: string;
      AOptions: TIndexOptions);
    function Add: TInstantIndexMetadata;
    function Find(const AName: string): TInstantIndexMetadata;
    // Returns True if the field identified by AFieldMetadata is part of a
    // defined index.
    function IsFieldIndexed(const AFieldMetadata: TInstantFieldMetadata):
      Boolean;
    property Items[Index: Integer]: TInstantIndexMetadata read GetItems
      write SetItems; default;
    function Owner: TInstantTableMetadata;
  end;

  TInstantTableMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantTableMetadata;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TInstantTableMetadata;
    function Find(const AName: string): TInstantTableMetadata;
    property Items[Index: Integer]: TInstantTableMetadata read GetItems;
      default;
  end;

  TInstantModel = class(TPersistent)
  private
    FClassMetadatas: TInstantClassMetadatas;
    function GetClassMetadatas: TInstantClassMetadatas;
  protected
    procedure DestroyClassMetadatas;
  public
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string; Format: TInstantStreamFormat = sfXML);
    procedure LoadFromResFile(const FileName: string);
    procedure MergeFromResFile(const FileName: string);
    procedure SaveToFile(const FileName: string; Format: TInstantStreamFormat = sfXML);
    procedure SaveToResFile(const FileName: string);
    {$IFDEF DELPHI_NEON}[neoninclude]{$ENDIF}
    property ClassMetadatas: TInstantClassMetadatas read GetClassMetadatas;
  end;

  TInstantScheme = class(TInstantStreamable)
  private
    FOnWarning: TInstantWarningEvent;
    FCatalog: TInstantCatalog;
    FTableMetadataCollection: TInstantTableMetadatas;
    FBlobStreamFormat: TInstantStreamFormat;
    FIdSize: Integer;
    FIdDataType: TInstantDataType;
    function GetTableMetadataCollection: TInstantTableMetadatas;
    function GetTableMetadatas(Index: Integer): TInstantTableMetadata;
    function GetTableMetadataCount: Integer;
    procedure SetCatalog(const Value: TInstantCatalog);
    procedure CatalogWarningEventHandler(const Sender: TObject;
      const AWarningText: string);
  protected
    procedure DoWarning(const AWarningText: string);
    function AttributeTypeToDataType(
      AttributeType: TInstantAttributeType): TInstantDataType; virtual;
    property TableMetadataCollection: TInstantTableMetadatas
      read GetTableMetadataCollection;
  public
    constructor Create;
    destructor Destroy; override;
    property Catalog: TInstantCatalog read FCatalog write SetCatalog;
    function FindTableMetadata(const AName: string): TInstantTableMetadata;
    property TableMetadataCount: Integer read GetTableMetadataCount;
    property TableMetadatas[Index: Integer]: TInstantTableMetadata
      read GetTableMetadatas;
    property BlobStreamFormat: TInstantStreamFormat read FBlobStreamFormat
      write FBlobStreamFormat default sfBinary;
    property IdDataType: TInstantDataType read FIdDataType write FIdDataType
      default dtString;
    property IdSize: Integer read FIdSize write FIdSize
      default InstantDefaultFieldSize;
    // Triggered when the scheme has something to report about its activity,
    // typically during database building/evolution, which is not a fatal error.
    property OnWarning: TInstantWarningEvent read FOnWarning write FOnWarning;
  end;

  // An object that provides the metadata info used by a TInstantScheme object
  // to build itself. It abstracts the way the information is fetched and its
  // source. It always works with a TInstantScheme. It is usually created
  // together with a TInstantScheme object:
  //   Scheme := TInstantScheme.Create;
  //   Scheme.Catalog := Broker.CreateCatalog(Scheme);
  // and the object ownership is transferred to Scheme, which is then
  // responsible for destroying the catalog object.
  TInstantCatalog = class
  private
    FScheme: TInstantScheme;
    FOnWarning: TInstantWarningEvent;
  protected
    function GetFeatures: TInstantCatalogFeatures; virtual;
    procedure DoWarning(const WarningText: string);
  public
    // Creates an instance and binds it to the specified TInstantScheme object.
    constructor Create(const AScheme: TInstantScheme);
    // A reference to the TInstantScheme object to which the current object is
    // bound, assigned on creation. The TInstantScheme object is responsible for
    // the current object's lifetime.
    property Scheme: TInstantScheme read FScheme;
    // Initializes ATableMetadatas from the catalog.
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
      virtual; abstract;
    // Returns a set of supported features. The predefined implementation
    // says that the catalog support everything; derived classes might not
    // support all features.
    property Features: TInstantCatalogFeatures read GetFeatures;
    // Triggered when the catalog has something to report about its activity,
    // typically during InitTableMetadatas, which is not a fatal error.
    property OnWarning: TInstantWarningEvent
      read FOnWarning write FOnWarning;
  end;

  TInstantCatalogClass = class of TInstantCatalog;

  // A TInstantCatalog that gathers its info from a TInstantModel.
  TInstantModelCatalog = class(TInstantCatalog)
  private
    FModel: TInstantModel;
  public
    // Creates an instance and binds it to the specified TInstantScheme object.
    // AModel is written to the Model property.
    constructor Create(const AScheme: TInstantScheme;
      const AModel: TInstantModel);
    // Initializes ATableMetadatas reading maps from the model.
    procedure InitTableMetadatas(ATableMetadatas: TInstantTableMetadatas);
      override;
    // A reference to the TInstantModel from which the catalog reads metadata
    // info.
    property Model: TInstantModel read FModel;
  end;

  TInstantAttributeMap = class(TInstantNamedList)
  private
    FClassMetadata: TInstantClassMetadata;
    FName: string;
    function GetItems(Index: Integer): TInstantAttributeMetadata;
    procedure SetItems(Index: Integer; Value: TInstantAttributeMetadata);
    function GetIsRootMap: Boolean;
  protected
    function GetName: string; override;
    procedure SetName(const Value: string); override;
  public
    constructor Create(AClassMetadata: TInstantClassMetadata);
    function Add(Item: TInstantAttributeMetadata): Integer;
    function AddUnique(Item: TInstantAttributeMetadata): Integer;
    function Find(const AName: string): TInstantAttributeMetadata;
    function IndexOf(Item: TInstantAttributeMetadata): Integer;
    procedure Insert(Index: Integer; Item: TInstantAttributeMetadata);
    function Remove(Item: TInstantAttributeMetadata): Integer;
    property ClassMetadata: TInstantClassMetadata read FClassMetadata;
    property IsRootMap: Boolean read GetIsRootMap;
    property Items[Index: Integer]: TInstantAttributeMetadata read GetItems
      write SetItems; default;
  end;

  TInstantAttributeMaps = class(TObjectList)
  private
    FClassMetadata: TInstantClassMetadata;
    function GetItems(Index: Integer): TInstantAttributeMap;
    function GetRootMap: TInstantAttributeMap;
    procedure SetItems(Index: Integer; Value: TInstantAttributeMap);
  public
    constructor Create(AClassMetadata: TInstantClassMetadata);
    function Add: TInstantAttributeMap; overload;
    function Add(Item: TInstantAttributeMap): Integer; overload;
    function Find(const AName: string): TInstantAttributeMap;
    function FindMap(const AttributeName: string): TInstantAttributeMap;
    function EnsureMap(const AName: string): TInstantAttributeMap;
    function IndexOf(Item: TInstantAttributeMap): Integer;
    procedure Insert(Index: Integer; Item: TInstantAttributeMap);
    function Remove(Item: TInstantAttributeMap): Integer;
    property ClassMetadata: TInstantClassMetadata read FClassMetadata;
    property RootMap: TInstantAttributeMap read GetRootMap;
    property Items[Index: Integer]: TInstantAttributeMap read GetItems
      write SetItems; default;
  end;

  TInstantValidator = class
  private
    FMetadata: TInstantAttributeMetadata;
  public
    // A reference to the metadata. Should be set by CreateValidator on
    // the created instance.
    property Metadata: TInstantAttributeMetadata read FMetadata write FMetadata;
    // Creates and returns a validator instance suitable for the specified
    // metadata, or nil if the class doesn't apply to the metadata.
    { TODO : extend this to support class-level validation? }
    class function CreateValidator(const AMetadata: TInstantAttributeMetadata):
      TInstantValidator; virtual;
    // Should check the value against the validation settings
    // that are specific for each derived class, and return
    // True if the value is valid for the attribute.
    // Otherwise, this method should return False and set an appropriate
    // error message in AValidationErrorText.
    // When this method is called, AAttribute is guaranteed to be assigned.
    function IsValid(const AAttribute: TInstantAbstractAttribute;
      const AValue: string; out AValidationErrorText: string): Boolean; virtual; abstract;
    // Raises an exception if the proposed value is not valid for
    // the attribute.
    procedure Validate(const AAttribute: TInstantAbstractAttribute; const AValue: string);
  end;

  TInstantValidatorClass = class of TInstantValidator;

  TInstantAttributeMetadata = class(TInstantMetadata)
  private
    FAttributeType: TInstantAttributeType;
    FDefaultValue: string;
    FUseNull: Boolean;
    FDisplayWidth: Integer;
    FDisplayLabel: string;
    FEditMask: string;
    FIsIndexed: Boolean;
    FIsRequired: Boolean;
    FIsLocalized: Boolean;
    FIsUnique: Boolean;
    FIndexName: string;
    FObjectClassName: string;
    FSize: Integer;
    FStorageName: string;
    FValidCharsString: string;
    FStorageKind: TInstantStorageKind;
    FExternalStorageName: string;
    FValidator: TInstantValidator;
    FEnumName: string;
    function GetAttributeClass: TInstantAbstractAttributeClass;
    function GetAttributeClassName: string;
    function GetAttributeTypeName: string;
    function GetCategory: TInstantAttributeCategory;
    function GetClassMetadata: TInstantClassMetadata;
    function GetClassMetadataName: string;
    function GetCollection: TInstantAttributeMetadatas;
    function GetFieldName: string;
    function GetHasValidChars: Boolean;
    function GetHasDisplayLabel: Boolean;
    function GetHasIndexName: Boolean;
    function GetIsDefault: Boolean;
    function GetObjectClass: TInstantAbstractObjectClass;
    function GetObjectClassMetadata: TInstantClassMetadata;
    function GetTableName: string;
    function GetValidCharsString: string;
    procedure SetAttributeClass(const AClass: TInstantAbstractAttributeClass);
    procedure SetAttributeClassName(const Value: string);
    procedure SetAttributeTypeName(const Value: string);
    procedure SetCollection(Value: TInstantAttributeMetadatas);
    procedure SetFieldName(const Value: string);
    procedure SetIsDefault(const Value: Boolean);
    procedure SetValidCharsString(const AValue: string);
    function GetValidator: TInstantValidator;
    // Should be called whenever a validation-related property,
    // such as ValidCharsString, changes.
    procedure FreeValidator;
  protected
    property Validator: TInstantValidator read GetValidator;
  public
    function CreateAttribute(
      AObject: TInstantAbstractObject): TInstantAbstractAttribute;
    procedure Assign(Source: TPersistent); override;
    procedure CheckAttributeClass(AClass: TInstantAbstractAttributeClass);
    procedure CheckCategory(ACategory: TInstantAttributeCategory);
    procedure CheckIsIndexed;
    function IsAttributeClass(AClass: TInstantAbstractAttributeClass): Boolean;
    destructor Destroy; override;
    property AttributeClass: TInstantAbstractAttributeClass 
      read GetAttributeClass  write SetAttributeClass;
    property AttributeClassName: string read GetAttributeClassName
      write SetAttributeClassName;
    property Category: TInstantAttributeCategory read GetCategory;
    property ClassMetadataName: string read GetClassMetadataName;
    property Collection: TInstantAttributeMetadatas read GetCollection
      write SetCollection;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
    property ObjectClass: TInstantAbstractObjectClass read GetObjectClass;
    property ObjectClassMetadata: TInstantClassMetadata
      read GetObjectClassMetadata;
    property FieldName: string read GetFieldName write SetFieldName;
    property HasValidChars: Boolean read GetHasValidChars;
    property HasDisplayLabel: Boolean read GetHasDisplayLabel;
    property HasIndexName: Boolean read GetHasIndexName;
    property TableName: string read GetTableName;
    procedure ValidateAttribute(const AAttribute: TInstantAbstractAttribute;
      const AValue: string);
  published
    property AttributeType: TInstantAttributeType read FAttributeType
      write FAttributeType default atUnknown;
    {$IFDEF DELPHI_NEON}[neonignore]{$ENDIF}
    property AttributeTypeName: string read GetAttributeTypeName
      write SetAttributeTypeName stored False;
    {$IFDEF DELPHI_NEON}[neonignore]{$ENDIF}
    property ClassMetadata: TInstantClassMetadata read GetClassMetadata;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property UseNull: Boolean read FUseNull write FUseNull default False;
    property DisplayWidth: Integer read FDisplayWidth write FDisplayWidth
      default 0;
    property DisplayLabel: string read FDisplayLabel write FDisplayLabel;
    property EditMask: string read FEditMask write FEditMask;
    property ExternalStorageName: string read FExternalStorageName
      write FExternalStorageName;
    property StorageKind: TInstantStorageKind read FStorageKind
      write FStorageKind default skEmbedded;
    property IsIndexed: Boolean read FIsIndexed write FIsIndexed;
    property IsRequired: Boolean read FIsRequired write FIsRequired;
    property IsLocalized: Boolean read FIsLocalized write FIsLocalized default False;
    property IsUnique: Boolean read FIsUnique write FIsUnique;
    property IndexName: string read FIndexName write FIndexName;
    {$IFDEF DELPHI_NEON}[neonignore]{$ENDIF}
    property ObjectClassName: string read FObjectClassName
      write FObjectClassName;
    property Size: Integer read FSize write FSize default 0;
    property StorageName: string read FStorageName write FStorageName;
    property ValidCharsString: string read GetValidCharsString
      write SetValidCharsString;
    {$IFDEF DELPHI_NEON}[neonignore]{$ENDIF}
    property EnumName: string read FEnumName write FEnumName;
  end;

  TInstantAttributeMetadatas = class(TInstantMetadatas)
  private
    function GetItems(Index: Integer): TInstantAttributeMetadata;
    procedure SetItems(Index: Integer; Value: TInstantAttributeMetadata);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TInstantClassMetadata);
    function Add: TInstantAttributeMetadata;
    procedure Clear;
    function Find(const AName: string): TInstantAttributeMetadata;
    procedure Remove(Item: TInstantAttributeMetadata);
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TInstantAttributeMetadata read GetItems
      write SetItems; default;
    function Owner: TInstantClassMetadata;
  end;

implementation

uses
  SysUtils, TypInfo, InstantPersistence, InstantUtils, InstantValidation,
{$IFDEF D17+}
  System.Types,
{$ENDIF}
  InstantStandardValidators {registers the standard validators - do not remove};

const
  AttributeClasses: array[TInstantAttributeType] of TInstantAttributeClass = (
    nil, TInstantInteger, TInstantFloat, TInstantCurrency, TInstantBoolean,
    TInstantString, TInstantDateTime, TInstantBlob, TInstantMemo,
    TInstantGraphic, TInstantPart, TInstantReference, TInstantParts,
    TInstantReferences, TInstantDate, TInstantTime, TInstantEnum);

{ TInstantMetadata }

function TInstantMetadata.Equals(const Other: TInstantMetadata): Boolean;
begin
  Result := InternalEquals(Other);
end;

function TInstantMetadata.GetCollection: TInstantMetadatas;
begin
  Result := inherited Collection as TInstantMetadatas;
end;

function TInstantMetadata.InternalEquals(
  const Other: TInstantMetadata): Boolean;
begin
  { TODO : This only works for case-insensitive object names! }
  Result := SameText(Other.Name, Name);
end;

procedure TInstantMetadata.SetCollection(Value: TInstantMetadatas);
begin
  inherited Collection := Value;
end;

destructor TInstantClassMetadata.Destroy;
begin
  FAttributeMetadatas.Free;
  DestroyAttributeViews;
  inherited;
end;

{ TInstantClassMetadata }

procedure TInstantClassMetadata.Assign(Source: TPersistent);
var
  LSource: TInstantClassMetadata;
begin
  inherited;
  if Source is TInstantClassMetadata then
  begin
    LSource := TInstantClassMetadata(Source);
    FDefaultContainerName := LSource.DefaultContainerName;
    FStorageName := LSource.StorageName;
    FPersistence := LSource.Persistence;
    FParentName := LSource.ParentName;
    AttributeMetadatas.Assign(LSource.AttributeMetadatas);
  end;
end;

procedure TInstantClassMetadata.BuildAttributeMap(Map: TInstantAttributeMap;
  Complete: Boolean);
begin
  GetDistinctAttributeMetadatas(Map);
  if Assigned(Parent) and (Complete or not Parent.IsStored) then
    Parent.BuildAttributeMap(Map, Complete);
end;

procedure TInstantClassMetadata.BuildStorageMaps(Maps: TInstantAttributeMaps);
var
  I: Integer;
  Map: TInstantAttributeMap;
begin
  if IsStored then
  begin
    Map := Maps.EnsureMap(TableName);
    for I := 0 to Pred(StorageMap.Count) do
      Map.AddUnique(StorageMap[I]);
  end;
  if Assigned(Parent) then
    Parent.BuildStorageMaps(Maps)
end;

procedure TInstantClassMetadata.ClearParent;
begin
  FParent := nil;
end;

class procedure TInstantClassMetadata.ConvertToBinary(
  Converter: TInstantTextToBinaryConverter);
begin
  with Converter do
  begin
    ConvertProperties(InstantBuildStartTag(InstantAttributeMetadatasTagName));
    Assert(Processor.ReadTagName = InstantAttributeMetadatasTagName);
    if (Processor.Token = xtAnyTag) and not SameText(Processor.PeekTag,
      InstantBuildEndTag(InstantAttributeMetadatasTagName)) then
      Convert;
    Processor.ReadTag;
  end;
end;

class procedure TInstantClassMetadata.ConvertToText(
  Converter: TInstantBinaryToTextConverter);
begin
  inherited;
  with Converter do
  begin
    Producer.WriteStartTag(InstantAttributeMetadatasTagName);
    if not Reader.EndOfList then
      Convert;
    Producer.WriteEndTag;
  end;
end;

procedure TInstantClassMetadata.DestroyAttributeViews;
begin
  FreeAndNil(FStorageMap);
  FreeAndNil(FStorageMaps);
  FreeAndNil(FMemberMap);
end;

function TInstantClassMetadata.GetAttributeMetadatas:
  TInstantAttributeMetadatas;
begin
  if not Assigned(FAttributeMetadatas) then
    FAttributeMetadatas := TInstantAttributeMetadatas.Create(Self);
  Result := FAttributeMetadatas;
end;

function TInstantClassMetadata.GetCollection: TInstantClassMetadatas;
begin
  Result := inherited Collection as TInstantClassMetadatas;
end;

procedure TInstantClassMetadata.GetDistinctAttributeMetadatas(
  Map: TInstantAttributeMap);
var
  I: Integer;
  AttribMeta: TInstantAttributeMetadata;
begin
  with AttributeMetadatas do
    for I := 0 to Pred(Count) do
    begin
      AttribMeta := Items[I];
      if not Assigned(Map.Find(AttribMeta.Name)) then
        Map.Add(AttribMeta);
    end;
end;

function TInstantClassMetadata.GetIsEmpty: Boolean;
begin
  Result := (Persistence = peEmbedded) and (AttributeMetadatas.Count = 0)
    and (StorageName = '');
end;

function TInstantClassMetadata.GetIsStored: Boolean;
begin
  Result := Persistence = peStored;
end;

function TInstantClassMetadata.GetMemberMap: TInstantAttributeMap;
begin
  if not Assigned(FMemberMap) then
  begin
    FMemberMap := TInstantAttributeMap.Create(Self);
    BuildAttributeMap(FMemberMap, True);
  end;
  Result := FMemberMap;
end;

function TInstantClassMetadata.GetParent: TInstantClassMetadata;
begin
  if not Assigned(FParent) then
    FParent := Collection.Find(FParentName);
  Result := FParent;
end;

function TInstantClassMetadata.GetParentName: string;
begin
  if Assigned(FParent) then
    Result := FParent.Name
  else
    Result := FParentName;
end;

function TInstantClassMetadata.GetStorageMap: TInstantAttributeMap;
begin
  if not Assigned(FStorageMap) and IsStored then
  begin
    FStorageMap := TInstantAttributeMap.Create(Self);
    BuildAttributeMap(FStorageMap, False);
  end;
  Result := FStorageMap;
end;

function TInstantClassMetadata.GetStorageMaps: TInstantAttributeMaps;
begin
  if not Assigned(FStorageMaps) and IsStored then
  begin
    FStorageMaps := TInstantAttributeMaps.Create(Self);
    BuildStorageMaps(FStorageMaps);
  end;
  Result := FStorageMaps;
end;

function TInstantClassMetadata.GetTableName: string;
begin
  if FStorageName = '' then
    Result := InstantClassNameToName(Name)
  else
    Result := FStorageName;
end;

procedure TInstantClassMetadata.ReadObject(Reader: TInstantReader);
begin
  inherited;
  if not Reader.EndOfList then
    Reader.ReadObject(AttributeMetadatas);
end;

procedure TInstantClassMetadata.SetCollection(Value: TInstantClassMetadatas);
begin
  if Value <> Collection then
  begin
    ClearParent;
    inherited Collection := Value;
  end;
end;

procedure TInstantClassMetadata.SetParent(Value: TInstantClassMetadata);
begin
  if Value <> FParent then
  begin
    FParent := Value;
    FParentName := '';
  end;
end;

procedure TInstantClassMetadata.SetParentName(const Value: string);
begin
  if Value <> ParentName then
  begin
    ClearParent;
    FParentName := Value;
  end;
end;

procedure TInstantClassMetadata.SetTableName(const Value: string);
begin
  if InstantNameToClassName(Value) = Name then
    StorageName := ''
  else
    StorageName := Value;
end;

procedure TInstantClassMetadata.WriteObject(Writer: TInstantWriter);
begin
  inherited;
  if AttributeMetadatas.Count > 0 then
    Writer.WriteObject(AttributeMetadatas);
end;

constructor TInstantFieldMetadata.Create(ACollection: TInstantFieldMetadatas);
begin
  inherited Create(ACollection);
end;

{ TInstantFieldMetadata }

procedure TInstantFieldMetadata.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantFieldMetadata then
    with TInstantFieldMetadata(Source) do
    begin
      Self.FDataType := FDataType;
      Self.FAlternateDataTypes := FAlternateDataTypes;
      Self.FOptions := FOptions;
      Self.FSize := FSize;
    end;
end;

function TInstantFieldMetadata.DataTypesEqual(
  const Other: TInstantFieldMetadata): Boolean;
begin
  Result := (DataType = Other.DataType) or
    (DataType in Other.AlternateDataTypes) or
    (Other.DataType in AlternateDataTypes);
end;

function TInstantFieldMetadata.GetCollection: TInstantFieldMetadatas;
begin
  Result := inherited Collection as TInstantFieldMetadatas;
end;

function TInstantFieldMetadata.GetTableMetadata: TInstantTableMetadata;
begin
  Result := Collection.Owner;
end;

function TInstantFieldMetadata.InternalEquals(
  const Other: TInstantMetadata): Boolean;
begin
  Result := inherited InternalEquals(Other);
  if Result then
    Result := (Other is TInstantFieldMetadata) and
      (DataTypesEqual(TInstantFieldMetadata(Other)));
  if DataType = dtString then
    Result := Result and (Size = TInstantFieldMetadata(Other).Size);
end;

constructor TInstantIndexMetadata.Create(ACollection: TInstantMetadatas);
begin
  inherited Create(ACollection);
end;

{ TInstantIndexMetadata }

procedure TInstantIndexMetadata.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantIndexMetadata then
    with TInstantIndexMetadata(Source) do
    begin
      Self.FFields := FFields;
      Self.FOptions := FOptions;
    end;
end;

function TInstantIndexMetadata.GetCollection: TInstantIndexMetadatas;
begin
  Result := inherited Collection as TInstantIndexMetadatas;
end;

function TInstantIndexMetadata.GetTableMetadata: TInstantTableMetadata;
begin
  Result := Collection.Owner;
end;

function TInstantIndexMetadata.InternalEquals(
  const Other: TInstantMetadata): Boolean;
begin
  Result := inherited InternalEquals(Other);
  if Result then
    Result := (Other is TInstantIndexMetadata) and
      (TInstantIndexMetadata(Other).Options = Options) and
      { TODO : This only works for case-insensitive field names! }
      SameText(TInstantIndexMetadata(Other).Fields, Fields);
end;

function TInstantIndexMetadata.IsFieldIndexed(
  const AFieldMetadata: TInstantFieldMetadata): Boolean;
var
  I: Integer;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    InstantStrToList(Fields, List, [';']);
    Result := False;
    for I := 0 to Pred(List.Count) do
    begin
      { TODO : This only works for case-insensitive field names! }
      Result := SameText(List[I], AFieldMetadata.Name);
      if Result then
        Break;
    end;
  finally
    List.Free;
  end;
end;

destructor TInstantTableMetadata.Destroy;
begin
  FFieldMetadatas.Free;
  FIndexMetadatas.Free;
  inherited;
end;

{ TInstantTableMetadata }

procedure TInstantTableMetadata.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantTableMetadata then
    with TInstantTableMetadata(Source) do
    begin
      Self.FieldMetadatas.Assign(FieldMetadatas);
      Self.IndexMetadatas.Assign(IndexMetadatas);
    end;
end;

function TInstantTableMetadata.FindFieldMetadata(
  const AName: string): TInstantFieldMetadata;
begin
  Result := FieldMetadatas.Find(AName);
end;

function TInstantTableMetadata.FindIndexMetadata(
  const AName: string): TInstantIndexMetadata;
begin
  Result := IndexMetadatas.Find(AName);
end;

function TInstantTableMetadata.GetFieldMetadataCount: Integer;
begin
  Result := FieldMetadatas.Count;
end;

function TInstantTableMetadata.GetFieldMetadatas: TInstantFieldMetadatas;
begin
  if not Assigned(FFieldMetadatas) then
    FFieldMetadatas := TInstantFieldMetadatas.Create(Self);
  Result := FFieldMetadatas;
end;

function TInstantTableMetadata.GetIndexMetadataCount: Integer;
begin
  Result := IndexMetadatas.Count;
end;

function TInstantTableMetadata.GetIndexMetadatas: TInstantIndexMetadatas;
begin
  if not Assigned(FIndexMetadatas) then
    FIndexMetadatas := TInstantIndexMetadatas.Create(Self);
  Result := FIndexMetadatas;
end;

function TInstantTableMetadata.GetScheme: TInstantScheme;
begin
  if Assigned(Collection) and (Collection.Owner is TInstantScheme) then
    Result := TInstantScheme(Collection.Owner)
  else
    Result := nil;
end;

{ TInstantMetadatas }

function TInstantMetadatas.Find(const AName: string): TInstantMetadata;
begin
  Result := TInstantMetadata(inherited Find(AName));
end;

function TInstantMetadatas.GetItems(Index: Integer): TInstantMetadata;
begin
  Result := inherited Items[Index] as TInstantMetadata;
end;

procedure TInstantMetadatas.SetItems(Index: Integer;
  const Value: TInstantMetadata);
begin
  inherited Items[Index] := Value;
end;

constructor TInstantClassMetadatas.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TInstantClassMetadata);
end;

{ TInstantClassMetadatas }

function TInstantClassMetadatas.Add: TInstantClassMetadata;
begin
  Result := TInstantClassMetadata(inherited Add);
end;

class function TInstantClassMetadatas.CreateInstance(
  Arg: Pointer): TInstantCollection;
begin
  Result := Create(Arg);
end;

function TInstantClassMetadatas.Find(
  const AName: string): TInstantClassMetadata;
begin
  Result := TInstantClassMetadata(inherited Find(AName));
end;

function TInstantClassMetadatas.GetItems(Index: Integer): TInstantClassMetadata;
begin
  Result := TInstantClassMetadata(inherited Items[Index]);
end;

procedure TInstantClassMetadatas.SetItems(Index: Integer;
  Value: TInstantClassMetadata);
begin
  inherited Items[Index] := Value;
end;

constructor TInstantFieldMetadatas.Create(AOwner: TInstantTableMetadata);
begin
  inherited Create(AOwner, TInstantFieldMetadata);
end;

{ TInstantFieldMetadatas }

function TInstantFieldMetadatas.Add: TInstantFieldMetadata;
begin
  Result := TInstantFieldMetadata(inherited Add);
end;

procedure TInstantFieldMetadatas.AddFieldMetadata(const AName: string;
  ADataType: TInstantDataType; ASize: Integer;
  AOptions: TInstantFieldOptions = []);
begin
  with Add do
  begin
    Name := AName;
    DataType := ADataType;
    Size := ASize;
    Options := AOptions;
  end;
end;

function TInstantFieldMetadatas.Find(
  const AName: string): TInstantFieldMetadata;
begin
  Result := inherited Find(AName) as TInstantFieldMetadata;
end;

function TInstantFieldMetadatas.GetItems(Index: Integer): TInstantFieldMetadata;
begin
  Result := TInstantFieldMetadata(inherited Items[Index]);
end;

function TInstantFieldMetadatas.Owner: TInstantTableMetadata;
begin
  Result := inherited Owner as TInstantTableMetadata;
end;

procedure TInstantFieldMetadatas.SetItems(Index: Integer;
  Value: TInstantFieldMetadata);
begin
  inherited Items[Index] := Value;
end;

constructor TInstantIndexMetadatas.Create(AOwner: TInstantTableMetadata);
begin
  inherited Create(AOwner, TInstantIndexMetadata);
end;

{ TInstantIndexMetadatas }

function TInstantIndexMetadatas.Add: TInstantIndexMetadata;
begin
  Result := TInstantIndexMetadata(inherited Add);
end;

procedure TInstantIndexMetadatas.AddIndexMetadata(const AName, AFields: string;
  AOptions: TIndexOptions);
begin
  with Add do
  begin
    Name := AName;
    Fields := AFields;
    Options := AOptions;
  end;
end;

function TInstantIndexMetadatas.Find(
  const AName: string): TInstantIndexMetadata;
begin
  Result := inherited Find(AName) as TInstantIndexMetadata;
end;

function TInstantIndexMetadatas.GetItems(Index: Integer): TInstantIndexMetadata;
begin
  Result := TInstantIndexMetadata(inherited Items[Index]);
end;

function TInstantIndexMetadatas.IsFieldIndexed(
  const AFieldMetadata: TInstantFieldMetadata): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I].IsFieldIndexed(AFieldMetadata);
    if Result then
      Break;
  end;
end;

function TInstantIndexMetadatas.Owner: TInstantTableMetadata;
begin
  Result := inherited Owner as TInstantTableMetadata;
end;

procedure TInstantIndexMetadatas.SetItems(Index: Integer;
  Value: TInstantIndexMetadata);
begin
  inherited Items[Index] := Value
end;

constructor TInstantTableMetadatas.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TInstantTableMetadata);
end;

{ TInstantTableMetadatas }

function TInstantTableMetadatas.Add: TInstantTableMetadata;
begin
  Result := inherited Add as TInstantTableMetadata;
end;

function TInstantTableMetadatas.Find(
  const AName: string): TInstantTableMetadata;
begin
  Result := inherited Find(AName) as TInstantTableMetadata;
end;

function TInstantTableMetadatas.GetItems(Index: Integer): TInstantTableMetadata;
begin
  Result := inherited Items[Index] as TInstantTableMetadata;
end;

{ TInstantCatalog }

constructor TInstantCatalog.Create(const AScheme: TInstantScheme);
begin
  inherited Create;
  FScheme := AScheme;
end;

procedure TInstantCatalog.DoWarning(const WarningText: string);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Self, WarningText);
end;

function TInstantCatalog.GetFeatures: TInstantCatalogFeatures;
begin
  Result := [cfReadTableInfo, cfReadColumnInfo, cfReadIndexInfo];
end;

{ TInstantModelCatalog }

constructor TInstantModelCatalog.Create(const AScheme: TInstantScheme;
  const AModel: TInstantModel);
begin
  inherited Create(AScheme);
  FModel := AModel;
end;

procedure TInstantModelCatalog.InitTableMetadatas(ATableMetadatas:
  TInstantTableMetadatas);
var
  Maps: TInstantAttributeMaps;
  IndexName: string;
  I: Integer;

  procedure AddMap(Map: TInstantAttributeMap);
  var
    I: Integer;
    TableMetadata: TInstantTableMetadata;
    AttributeMetadata: TInstantAttributeMetadata;
    Options: TInstantFieldOptions;

    // Adds a table metadata definition matching AttributeMetadata, which
    // must have StorageKind = skExternal.
    procedure AddExternalTableMetadata;
    var
      TableMetadata: TInstantTableMetadata;
    begin
      TableMetadata := ATableMetadatas.Add;
      with TableMetadata do
      begin
        Name := AttributeMetadata.ExternalStorageName;
        // The structure of an external table is fixed.
        FieldMetadatas.AddFieldMetadata(InstantIdFieldName, Scheme.IdDataType,
          Scheme.IdSize, [foRequired, foIndexed]);
        FieldMetadatas.AddFieldMetadata(InstantParentClassFieldName, dtString,
          InstantDefaultFieldSize);
        FieldMetadatas.AddFieldMetadata(InstantParentIdFieldName,
          Scheme.IdDataType, Scheme.IdSize);
        FieldMetadatas.AddFieldMetadata(InstantChildClassFieldName, dtString,
          InstantDefaultFieldSize);
        FieldMetadatas.AddFieldMetadata(InstantChildIdFieldName,
          Scheme.IdDataType, Scheme.IdSize);
        FieldMetadatas.AddFieldMetadata(InstantSequenceNoFieldName, dtInteger,
          InstantDefaultFieldSize);
        IndexMetadatas.AddIndexMetadata('', InstantIdFieldName,
          [ixPrimary, ixUnique]);
        // ToDo: Temporary linking table indices
        IndexMetadatas.AddIndexMetadata(Name + '_PL',
          InstantParentClassFieldName + ';' + InstantParentIdFieldName, []);
//        IndexMetadatas.AddIndexMetadata(Name + '_CL',
//          InstantChildClassFieldName + ';' + InstantChildIdFieldName, []);
      end;
    end;

  begin
    TableMetadata := ATableMetadatas.Add;
    with TableMetadata do
    begin
      Name := Map.Name;

      // Class + Id + UpdateCount.
      FieldMetadatas.AddFieldMetadata(InstantClassFieldName, dtString,
        InstantDefaultFieldSize, [foRequired, foIndexed]);
      FieldMetadatas.AddFieldMetadata(InstantIdFieldName, Scheme.IdDataType,
        Scheme.IdSize, [foRequired, foIndexed]);
      FieldMetadatas.AddFieldMetadata(InstantUpdateCountFieldName, dtInteger,
        0);
      IndexMetadatas.AddIndexMetadata('', InstantIndexFieldNames,
        [ixPrimary, ixUnique]);

      // Other.
      for I := 0 to Pred(Map.Count) do
      begin
        Options := [];
        AttributeMetadata := Map[I];
        if AttributeMetadata.AttributeType = atReference then
        begin
          FieldMetadatas.AddFieldMetadata(
            AttributeMetadata.FieldName + InstantClassFieldName,
            Scheme.AttributeTypeToDataType(atString), InstantDefaultFieldSize);
          FieldMetadatas.AddFieldMetadata(
            AttributeMetadata.FieldName + InstantIdFieldName,
            Scheme.IdDataType, Scheme.IdSize);
        end
        else if AttributeMetadata.AttributeType = atPart then
        begin
          if AttributeMetadata.StorageKind = skEmbedded then
            FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName,
              Scheme.AttributeTypeToDataType(AttributeMetadata.AttributeType),
              AttributeMetadata.Size)
          else if AttributeMetadata.StorageKind = skExternal then
          begin
            FieldMetadatas.AddFieldMetadata(
              AttributeMetadata.FieldName + InstantClassFieldName,
              Scheme.AttributeTypeToDataType(atString),
              InstantDefaultFieldSize);
            FieldMetadatas.AddFieldMetadata(
              AttributeMetadata.FieldName + InstantIdFieldName,
              Scheme.IdDataType, Scheme.IdSize);
          end;
        end
        else if AttributeMetadata.AttributeType in [atParts, atReferences] then
        begin
          if AttributeMetadata.StorageKind = skEmbedded then
            FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName,
              Scheme.AttributeTypeToDataType(AttributeMetadata.AttributeType),
              AttributeMetadata.Size)
          else if AttributeMetadata.StorageKind = skExternal then
            AddExternalTableMetadata;
        end
        else
        begin
          if AttributeMetadata.IsIndexed then
          begin
            if AttributeMetadata.HasIndexName then
              IndexName := AttributeMetadata.IndexName else
              IndexName := Map.Name + AttributeMetadata.FieldName;

            if AttributeMetadata.IsUnique then
            begin
              IndexMetadatas.AddIndexMetadata(IndexName,
                AttributeMetadata.FieldName, [ixUnique]);
              Options := Options + [foIndexed, foUnique];
            end else
            begin
              IndexMetadatas.AddIndexMetadata(IndexName,
                AttributeMetadata.FieldName, []);
              Options := Options + [foIndexed];
            end;
          end
          else if AttributeMetadata.IsRequired then
          begin
            Options := Options + [foRequired];
          end;
          if AttributeMetadata.IsLocalized then
          begin
            Options := Options + [foMultiLanguage];
          end;
          FieldMetadatas.AddFieldMetadata(AttributeMetadata.FieldName,
            Scheme.AttributeTypeToDataType(AttributeMetadata.AttributeType),
            AttributeMetadata.Size, Options);
        end;
      end;
    end;
  end;

begin
  Maps := InstantCreateStorageMaps(Model.ClassMetadatas);
  try
    if Assigned(Maps) then
    begin
      for I := 0 to Pred(Maps.Count) do
        AddMap(Maps[I]);
    end;
  finally
    Maps.Free;
  end;
end;

{ TInstantModel }

destructor TInstantModel.Destroy;
begin
  DestroyClassMetadatas;
  inherited;
end;

procedure TInstantModel.DestroyClassMetadatas;
begin
  FreeAndNil(FClassMetadatas);
end;

function TInstantModel.GetClassMetadatas: TInstantClassMetadatas;
begin
  if not Assigned(FClassMetadatas) then
    FClassMetadatas := TInstantClassMetadatas.Create(nil);
  Result := FClassMetadatas;
end;

procedure TInstantModel.LoadFromFile(const FileName: string; Format: TInstantStreamFormat = sfXML);
var
  Stream: TInstantFileStream;
begin
  Stream := TInstantFileStream.Create(FileName, fmOpenRead);
  try
    {$IFDEF DELPHI_NEON}
    if Format = sfJSON then
      InstantReadObject(Stream, Format, Self)
    else
    {$ENDIF}
      InstantReadObject(Stream, Format, ClassMetadatas);
  finally
    Stream.Free;
  end;
end;

procedure TInstantModel.LoadFromResFile(const FileName: string);
var
  Stream: TInstantFileStream;
begin
  Stream := TInstantFileStream.Create(FileName, fmOpenRead);
  try
    Stream.ReadResourceFileHeader;
    Stream.ReadObjectRes(ClassMetadatas);
  finally
    Stream.Free;
  end;
end;

procedure TInstantModel.MergeFromResFile(const FileName: string);
var
  LModel: TInstantModel;
  I: Integer;
  LClassMetadata: TInstantClassMetadata;
begin
  LModel := TInstantModel.Create;
  try
    LModel.LoadFromResFile(FileName);
    for I := 0 to LModel.ClassMetadatas.Count - 1 do
    begin
      LClassMetadata := ClassMetadatas.Find(LModel.ClassMetadatas[I].Name);
      if Assigned(LClassMetadata) then
        LClassMetadata.Assign(LModel.ClassMetadatas[I])
      else
      begin
        LClassMetadata := ClassMetadatas.Add;
        try
          LClassMetadata.Assign(LModel.ClassMetadatas[I]);
        except
          FreeAndNil(LClassMetadata);
          raise;
        end;
      end;
    end;
  finally
    LModel.Free;
  end;
end;

procedure TInstantModel.SaveToFile(const FileName: string; Format: TInstantStreamFormat = sfXML);
var
  Stream: TInstantFileStream;
begin
  Stream := TInstantFileStream.Create(FileName, fmCreate);
  try
    {$IFDEF DELPHI_NEON}
    if Format = sfJSON then
      InstantWriteObject(Stream, Format, Self)
    else
    {$ENDIF}
      InstantWriteObject(Stream, Format, ClassMetadatas);
  finally
    Stream.Free;
  end;
end;

procedure TInstantModel.SaveToResFile(const FileName: string);
var
  Stream: TInstantFileStream;
begin
  Stream := TInstantFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteResourceFileHeader;
    Stream.WriteObjectRes(InstantModelResourceName, ClassMetadatas);
    Stream.AlignStream;
  finally
    Stream.Free;
  end;
end;

constructor TInstantScheme.Create;
begin
  inherited Create;
  FBlobStreamFormat := sfBinary;
  FIdDataType := dtString;
  FIdSize := InstantDefaultFieldSize; 
end;

destructor TInstantScheme.Destroy;
begin
  FCatalog.Free;
  FTableMetadataCollection.Free;
  inherited;
end;

{ TInstantScheme }

function TInstantScheme.AttributeTypeToDataType(
  AttributeType: TInstantAttributeType): TInstantDataType;
begin
  Result := InstantAttributeTypeToDataType(AttributeType, BlobStreamFormat);
end;

procedure TInstantScheme.CatalogWarningEventHandler(const Sender: TObject;
  const AWarningText: string);
begin
  DoWarning(AWarningText);
end;

procedure TInstantScheme.DoWarning(const AWarningText: string);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Self, AWarningText);
end;

function TInstantScheme.FindTableMetadata(
  const AName: string): TInstantTableMetadata;
begin
  Result := TableMetadataCollection.Find(AName);
end;

function TInstantScheme.GetTableMetadataCollection: TInstantTableMetadatas;
begin
  if not Assigned(FTableMetadataCollection) then
    FTableMetadataCollection := TInstantTableMetadatas.Create(Self);
  Result := FTableMetadataCollection;
end;

function TInstantScheme.GetTableMetadataCount: Integer;
begin
  Result := TableMetadataCollection.Count;
end;

function TInstantScheme.GetTableMetadatas(
  Index: Integer): TInstantTableMetadata;
begin
  Result := TableMetadataCollection[Index];
end;

procedure TInstantScheme.SetCatalog(const Value: TInstantCatalog);
begin
  FreeAndNil(FCatalog);
  FCatalog := Value;
  if Assigned(FCatalog) then
  begin
    FCatalog.OnWarning := CatalogWarningEventHandler;
    FCatalog.InitTableMetadatas(TableMetadataCollection);
  end;
end;

constructor TInstantAttributeMap.Create(
  AClassMetadata: TInstantClassMetadata);
begin
  inherited Create;
  FClassMetadata := AClassMetadata;
end;

{ TInstantAttributeMap }

function TInstantAttributeMap.Add(Item: TInstantAttributeMetadata): Integer;
begin
  Result := inherited Add(Item);
end;

function TInstantAttributeMap.AddUnique(
  Item: TInstantAttributeMetadata): Integer;
begin
  if not Assigned(Find(Item.Name)) then
    Result := Add(Item)
  else
    Result := -1;
end;

function TInstantAttributeMap.Find(
  const AName: string): TInstantAttributeMetadata;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I];
    if SameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

function TInstantAttributeMap.GetIsRootMap: Boolean;
begin
  Result := Assigned(ClassMetadata) and (Name = ClassMetadata.TableName);
end;

function TInstantAttributeMap.GetItems(
  Index: Integer): TInstantAttributeMetadata;
begin
  Result := inherited Items[Index];
end;

function TInstantAttributeMap.GetName: string;
begin
  Result := FName;
end;

function TInstantAttributeMap.IndexOf(Item: TInstantAttributeMetadata): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TInstantAttributeMap.Insert(Index: Integer;
  Item: TInstantAttributeMetadata);
begin
  inherited Insert(Index, Item);
end;

function TInstantAttributeMap.Remove(Item: TInstantAttributeMetadata): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TInstantAttributeMap.SetItems(Index: Integer;
  Value: TInstantAttributeMetadata);
begin
  inherited Items[Index] := Value;
end;

procedure TInstantAttributeMap.SetName(const Value: string);
begin
  FName := Value;
end;

constructor TInstantAttributeMaps.Create(
  AClassMetadata: TInstantClassMetadata);
begin
  inherited Create;
  FClassMetadata := AClassMetadata;
end;

{ TInstantAttributeMaps }

function TInstantAttributeMaps.Add: TInstantAttributeMap;
begin
  Result := TInstantAttributeMap.Create(ClassMetadata);
  try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TInstantAttributeMaps.Add(Item: TInstantAttributeMap): Integer;
begin
  Result := inherited Add(Item)
end;

function TInstantAttributeMaps.EnsureMap(
  const AName: string): TInstantAttributeMap;
begin
  Result := Find(AName);
  if not Assigned(Result) then
  begin
    Result := Add;
    Result.Name := AName;
  end
end;

function TInstantAttributeMaps.Find(const AName: string): TInstantAttributeMap;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I];
    if SameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

function TInstantAttributeMaps.FindMap(
  const AttributeName: string): TInstantAttributeMap;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I];
    if Assigned(Result.Find(AttributeName)) then
      Exit;
  end;
  Result := nil;
end;

function TInstantAttributeMaps.GetItems(Index: Integer): TInstantAttributeMap;
begin
  Result := inherited Items[Index] as TInstantAttributeMap;
end;

function TInstantAttributeMaps.GetRootMap: TInstantAttributeMap;
var
  I: Integer;
begin
  if Assigned(ClassMetadata) then
    for I := 0 to Pred(Count) do
    begin
      Result := Items[I];
      if Result.Name = ClassMetadata.TableName then
        Exit;
    end;
  Result := nil
end;

function TInstantAttributeMaps.IndexOf(Item: TInstantAttributeMap): Integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TInstantAttributeMaps.Insert(Index: Integer;
  Item: TInstantAttributeMap);
begin
  inherited Insert(Index, Item);
end;

function TInstantAttributeMaps.Remove(Item: TInstantAttributeMap): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TInstantAttributeMaps.SetItems(Index: Integer;
  Value: TInstantAttributeMap);
begin
  inherited Items[Index] := Value;
end;

{ TInstantAttributeMetadata }

procedure TInstantAttributeMetadata.Assign(Source: TPersistent);
var
  LSource: TInstantAttributeMetadata;
begin
  inherited;
  if Source is TInstantAttributeMetadata then
  begin
    LSource := TInstantAttributeMetadata(Source);
    FAttributeType := LSource.AttributeType;
    FDefaultValue := LSource.DefaultValue;
    FDisplayWidth := LSource.DisplayWidth;
    FDisplayLabel := LSource.DisplayLabel;
    FEditMask := LSource.EditMask;
    FIsIndexed := LSource.IsIndexed;
    FIsRequired := LSource.IsRequired;
    FIsLocalized := LSource.IsLocalized;
    FIsUnique := LSource.IsUnique;
    FUseNull := LSource.UseNull;
    FIndexName := LSource.IndexName;
    FObjectClassName := LSource.ObjectClassName;
    FSize := LSource.Size;
    FStorageName := LSource.StorageName;
    FStorageKind := LSource.StorageKind;
    FExternalStorageName := LSource.ExternalStorageName;
    FValidCharsString := LSource.ValidCharsString;
    FEnumName := LSource.EnumName;
  end;
end;

procedure TInstantAttributeMetadata.CheckAttributeClass(AClass:
    TInstantAbstractAttributeClass);
begin
  if Assigned(AClass) and not IsAttributeClass(AClass) then
    raise EInstantError.CreateFmt(SUnexpectedAttributeClass,
      [AttributeClassName, Name, ClassMetadataName, AClass.ClassName]);
end;

procedure TInstantAttributeMetadata.CheckCategory(
  ACategory: TInstantAttributeCategory);

  function CategoryName(Cat: TInstantAttributeCategory): string;
  begin
    Result := GetEnumName(TypeInfo(TInstantAttributeCategory), Ord(Cat));
  end;

begin
  if Category <> ACategory then
    raise EInstantError.CreateFmt(SUnexpectedAttributeCategory,
      [CategoryName(Category), AttributeClassName, Name,
      ClassMetadataName, CategoryName(ACategory)]);
end;

procedure TInstantAttributeMetadata.CheckIsIndexed;
begin
  if not IsIndexed then
    raise EInstantError.CreateFmt(SAttributeNotIndexed,
      [AttributeClassName, Name, ClassMetadataName]);
end;

function TInstantAttributeMetadata.CreateAttribute(AObject:
    TInstantAbstractObject): TInstantAbstractAttribute;
var
  AClass: TInstantAbstractAttributeClass;
begin
  AClass := AttributeClass;
  if not Assigned(AClass) then
    raise EInstantError.CreateFmt(SUnsupportedType, [AttributeTypeName]);
  Result := AClass.Create(TInstantObject(AObject), Self);
end;

destructor TInstantAttributeMetadata.Destroy;
begin
  FreeAndNil(FValidator);
  inherited;
end;

procedure TInstantAttributeMetadata.FreeValidator;
begin
  FreeAndNil(FValidator);
end;

function TInstantAttributeMetadata.GetAttributeClass:
    TInstantAbstractAttributeClass;
begin
  Result := AttributeClasses[AttributeType];
end;

function TInstantAttributeMetadata.GetAttributeClassName: string;
begin
  if Assigned(AttributeClass) then
    Result := AttributeClass.ClassName
  else
    Result := '';
end;

function TInstantAttributeMetadata.GetAttributeTypeName: string;
begin
  Result := GetEnumName(TypeInfo(TInstantAttributeType),
    Ord(AttributeType));
  Result := Copy(Result, 3, Length(Result) - 2);
end;

function TInstantAttributeMetadata.GetCategory: TInstantAttributeCategory;
begin
  if Assigned(AttributeClass) then
  begin
    if AttributeClass.InheritsFrom(TInstantElement) then
      Result := acElement
    else if AttributeClass.InheritsFrom(TInstantContainer) then
      Result := acContainer
    else
      Result := acSimple;
  end else
    Result := acUnknown;
end;

function TInstantAttributeMetadata.GetClassMetadata: TInstantClassMetadata;
begin
  if Assigned(Collection) then
    Result := Collection.Owner
  else
    Result := nil;
end;

function TInstantAttributeMetadata.GetClassMetadataName: string;
begin
  if Assigned(ClassMetadata) then
    Result := ClassMetadata.Name
  else
    Result := SUnassigned;
end;

function TInstantAttributeMetadata.GetCollection: TInstantAttributeMetadatas;
begin
  Result := inherited Collection as TInstantAttributeMetadatas;
end;

function TInstantAttributeMetadata.GetFieldName: string;
begin
  if FStorageName = '' then
    Result := Name
  else
    Result := FStorageName;
end;

function TInstantAttributeMetadata.GetHasValidChars: Boolean;
begin
  Result := FValidCharsString <> '';
end;

function TInstantAttributeMetadata.GetHasDisplayLabel: Boolean;
begin
  Result := FDisplayLabel <> '';
end;

function TInstantAttributeMetadata.GetHasIndexName: Boolean;
begin
  Result := FIndexName <> '';
end;

function TInstantAttributeMetadata.GetIsDefault: Boolean;
begin
  Result := Assigned(ClassMetadata) and
    (ClassMetadata.DefaultContainerName <> '') and
    (ClassMetadata.DefaultContainerName = Name);
end;

function TInstantAttributeMetadata.GetObjectClass: TInstantAbstractObjectClass;
begin
  if ObjectClassName = '' then
    Result := TInstantObject
  else
    Result := InstantFindClass(ObjectClassName);
end;

function TInstantAttributeMetadata.GetObjectClassMetadata:
  TInstantClassMetadata;
begin
  Result := InstantGetClassMetadata(ObjectClassName);
end;

function TInstantAttributeMetadata.GetTableName: string;
begin
  if Assigned(ClassMetadata) then
    Result := ClassMetadata.TableName
  else
    Result := '';
end;

function TInstantAttributeMetadata.GetValidator: TInstantValidator;
begin
  if not Assigned(FValidator) then
    FValidator := InstantValidatorFactory.CreateValidator(Self);
  Result := FValidator;
end;

function TInstantAttributeMetadata.GetValidCharsString: string;
begin
  Result := FValidCharsString;
end;

function TInstantAttributeMetadata.IsAttributeClass(AClass:
    TInstantAbstractAttributeClass): Boolean;
begin
  Result := Assigned(AttributeClass) and AttributeClass.InheritsFrom(AClass);
end;

procedure TInstantAttributeMetadata.SetAttributeClass(const AClass:
    TInstantAbstractAttributeClass);
var
  AttribType: TInstantAttributeType;
begin
  for AttribType := Low(AttribType) to High(AttribType) do
    if AttributeClasses[AttribType] = AClass then
    begin
      AttributeType := AttribType;
      Exit;
    end;
  AttributeType := atUnknown;
end;

procedure TInstantAttributeMetadata.SetAttributeClassName(const Value: string);
var
  AttribType: TInstantAttributeType;
  AClass: TInstantAttributeClass;
begin
  for AttribType := Low(AttribType) to High(AttribType) do
  begin
    AClass := AttributeClasses[AttribType];
    if Assigned(AClass) and SameText(AClass.ClassName, Value) then
    begin
      AttributeType := AttribType;
      Exit;
    end;
  end;
  AttributeType := atUnknown;
end;

procedure TInstantAttributeMetadata.SetAttributeTypeName(const Value: string);
var
  I: Integer;
begin
  if Value = '' then
    Exit;
  I := GetEnumValue(TypeInfo(TInstantAttributeType), 'at' + Value);
  if I <> -1 then
    AttributeType := TInstantAttributeType(I)
  else
    raise EInstantError.CreateFmt(SUnsupportedType, [Value]);
end;

procedure TInstantAttributeMetadata.SetCollection(
  Value: TInstantAttributeMetadatas);
begin
  inherited Collection := Value;
end;

procedure TInstantAttributeMetadata.SetFieldName(const Value: string);
begin
  if Value = Name then
    FStorageName := ''
  else
    FStorageName := Value;
end;

procedure TInstantAttributeMetadata.SetIsDefault(const Value: Boolean);
begin
  if (Value <> IsDefault) and Assigned(ClassMetadata) then
  begin
    if Value then
      ClassMetadata.DefaultContainerName := Name
    else
      ClassMetadata.DefaultContainerName := '';
  end;
end;

procedure TInstantAttributeMetadata.SetValidCharsString(const AValue: string);
begin
  if AValue <> FValidCharsString then
  begin
    FreeValidator;
    FValidCharsString := AValue;
  end;
end;

procedure TInstantAttributeMetadata.ValidateAttribute(
  const AAttribute: TInstantAbstractAttribute; const AValue: string);
begin
  Validator.Validate(AAttribute, AValue);
end;

constructor TInstantAttributeMetadatas.Create(AOwner: TInstantClassMetadata);
begin
  inherited Create(AOwner, TInstantAttributeMetadata);
end;

{ TInstantAttributeMetadatas }

function TInstantAttributeMetadatas.Add: TInstantAttributeMetadata;
begin
  Result := TInstantAttributeMetadata(inherited Add);
  Changed;
end;

procedure TInstantAttributeMetadatas.Assign(Source: TPersistent);
var
  LSource: TInstantAttributeMetadatas;
  I: Integer;
  LAttribMetadata: TInstantAttributeMetadata;
begin
  inherited;
  if Source is TInstantAttributeMetadatas then
  begin
    LSource := TInstantAttributeMetadatas(Source);
    Clear;
    for I := 0 to LSource.Count - 1 do
    begin
      LAttribMetadata := Add;
      try
        LAttribMetadata.Assign(LSource[I]);
      except
        FreeAndNil(LAttribMetadata);
        raise;
      end;
    end;
  end;
end;

procedure TInstantAttributeMetadatas.Changed;
begin
  if Owner <> nil then
    Owner.DestroyAttributeViews;
end;

procedure TInstantAttributeMetadatas.Clear;
begin
  inherited Clear;
  Changed;
end;

function TInstantAttributeMetadatas.Find(
  const AName: string): TInstantAttributeMetadata;
begin
  Result := TInstantAttributeMetadata(inherited Find(AName));
end;

function TInstantAttributeMetadatas.GetItems(
  Index: Integer): TInstantAttributeMetadata;
begin
  Result := TInstantAttributeMetadata(inherited Items[Index]);
end;

function TInstantAttributeMetadatas.Owner: TInstantClassMetadata;
begin
  Result := inherited Owner as TInstantClassMetadata;
end;

procedure TInstantAttributeMetadatas.Remove(Item: TInstantAttributeMetadata);
begin
  inherited Remove(Item);
  Changed;
end;

procedure TInstantAttributeMetadatas.SetItems(Index: Integer;
  Value: TInstantAttributeMetadata);
begin
  inherited Items[Index] := Value;
end;

{ TInstantValidator }

class function TInstantValidator.CreateValidator(
  const AMetadata: TInstantAttributeMetadata): TInstantValidator;
begin
  Result := Create;
  Result.Metadata := AMetadata;
end;

procedure TInstantValidator.Validate(
  const AAttribute: TInstantAbstractAttribute; const AValue: string);
var
  LValidationErrorText: string;
begin
  if not IsValid(AAttribute, AValue, LValidationErrorText) then
    raise EInstantValidationError.Create(LValidationErrorText);
end;

end.

