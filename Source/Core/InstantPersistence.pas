(*
 *   InstantObjects
 *   Core Classes
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

unit InstantPersistence;

{$I '..\InstantDefines.inc'}

interface

uses
  {$IFNDEF IO_CONSOLE}Graphics,{$ENDIF}
  Classes, {$IFNDEF NEXTGEN}Contnrs,{$ENDIF} SysUtils, DB, InstantClasses, InstantCommand, InstantConsts,
  InstantMetadata, InstantTypes
  {$IFDEF DELPHI_NEON}
  , Neon.Core.Types
  , Neon.Core.Nullables
  , Neon.Core.Attributes
  {$ENDIF}
  {$IFDEF D17+}
  , System.Generics.Collections
  {$ENDIF}
  , AnsiStrings;

const
  IO_SER_CLASSNAME = 'ClassName';
  IO_SER_ID = 'Id';

type
  TInstantAttribute = class;
  TInstantAttributeClass = class of TInstantAttribute;
  TInstantBroker = class;
  TInstantComplex = class;
  TInstantConnectionDef = class;
  TInstantConnectionDefClass = class of TInstantConnectionDef;
  TInstantConnector = class;
  TInstantConnectorClass = class of TInstantConnector;
  TInstantContainer = class;
  TInstantObject = class;
  TInstantObjectClass = class of TInstantObject;
  TInstantObjectNotifiers = class;
  TInstantObjectReferenceList = class;
  TInstantObjectStore = class;
  TInstantObjectStores = class;
  TInstantQuery = class;

  TInstantAttributeChangeEvent = procedure(Sender: TInstantObject; Attribute:
    TInstantAttribute) of object;
  TInstantIdChangeEvent = procedure(Sender: TInstantObject;
    const AIdValue: string) of object;
  TInstantConnectorEvent = procedure(Sender: TObject;
    Connector: TInstantConnector) of object;
  TInstantContentChangeEvent = procedure(Sender: TInstantObject;
    Container: TInstantContainer; ChangeType: TInstantContentChangeType;
    Index: Integer; AObject: TInstantObject) of object;
  TInstantErrorEvent = procedure(Sender: TObject;
    OperationType: TInstantOperationType; E: Exception;
    var Action: TInstantErrorAction) of object;
  TInstantInvokableMethod = function(Params: TParams): string of object;
  TInstantNotifyEvent = procedure(Sender: TInstantObject) of object;
  TInstantObjectNotifyEvent = procedure(Sender: TInstantObject;
    Notification: TInstantObjectNotification) of object;
  TInstantObjectProc = procedure(AObject: TInstantOBject) of object;
  TInstantProgressEvent = procedure(Sender: TObject; Count: Integer;
    var Continue: Boolean) of object;
  TInstantSortCompare = function(Holder, Obj1, Obj2: TInstantObject):
    Integer of object;
  TInstantStringFunc = function(const S: string): string of object;
  TInstantSchemeEvent = procedure(Sender: TObject; Scheme: TInstantScheme)
    of object;
  TInstantGenerateIdEvent = procedure(Sender: TObject;
    const AObject: TInstantObject; var Id: string) of object;
  TInstantUpdateOperation = procedure(ConflictAction: TInstantConflictAction)
    of object;

  TInstantAttributeProc = reference to procedure(
    const AAttribute: TInstantAttribute;
    const AParentContainer: TInstantContainer;
    const AItemIndex: Integer);

  TInstantObjectReference = class(TInstantStreamable)
  private
    FInstance: TInstantObject;
    FObjectClassName: string;
    FObjectId: string;
    FOwner: TInstantComplex;
    FOwnsInstance: Boolean;
    FObjectData: TInstantAbstractObjectData;
    procedure ClearReference;
    procedure DoAssignInstance(AInstance: TInstantObject; AOwnsInstance: Boolean);
    function GetInstance: TInstantObject;
    function GetObjectClass: TInstantObjectClass;
    function GetObjectClassName: string;
    function GetObjectId: string;
    function GetSignature: string;
    procedure SetInstance(Value: TInstantObject);
    procedure SetOwnsInstance(const Value: Boolean);
  protected
    class function CreateInstance(Arg: Pointer = nil): TInstantStreamable; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    constructor Create(AInstance: TInstantObject = nil; AOwnsInstance: Boolean = False;
      AOwner: TInstantComplex = nil);
    constructor Clone(Source: TInstantObjectReference; AOwnsInstance: Boolean;
      AOwner: TInstantComplex = nil);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignInstance(AInstance: TInstantObject);
    function Dereference(Connector: TInstantConnector = nil;
      AOwnsInstance: Boolean = True; Retry: Boolean = False): TInstantObject;
    procedure DestroyInstance;
    function Equals(const AObjectClassName, AObjectId: string): Boolean; {$IFDEF D12+}reintroduce;{$ENDIF} overload;
    function Equals(AObject: TInstantObject): Boolean; {$IFDEF D12+}reintroduce;{$ENDIF} overload;
    function HasInstance: Boolean;
    function HasReference: Boolean;
    function IsBroken: Boolean;
    procedure ReadAsObject(Reader: TInstantReader); virtual;
    procedure Reset;
    procedure ReferenceObject(const AObjectClassName, AObjectId: string;
      const AObjectData: TInstantAbstractObjectData = nil); overload;
    procedure ReferenceObject(const AObjectClass: TInstantObjectClass;
      const AObjectId: string; const AObjectData: TInstantAbstractObjectData = nil); overload;
    // Retrieves the referenced object from the internal data object, if available.
    procedure RetrieveObjectFromObjectData(const AConnector: TInstantConnector);
    procedure WriteAsObject(Writer: TInstantWriter); virtual;
    property Instance: TInstantObject read GetInstance write SetInstance;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty(IO_SER_CLASSNAME)]{$ENDIF}
    property ObjectClassName: string read GetObjectClassName;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty(IO_SER_ID)]{$ENDIF}
    property ObjectId: string read GetObjectId;
    property OwnsInstance: Boolean read FOwnsInstance write SetOwnsInstance;
    property Signature: string read GetSignature;
  end;

  EInstantAccessError = class(EInstantError)
  end;

  TInstantAttribute = class(TInstantAbstractAttribute)
  private
    FIsChanged: Boolean;
    function GetIsIndexed: Boolean;
    function GetIsRequired: Boolean;
    function GetIsUnique: Boolean;
    function GetMetadata: TInstantAttributeMetadata;
    function GetName: string;
    function GetValue: Variant;
    procedure SetMetadata(Value: TInstantAttributeMetadata);
    procedure SetValue(AValue: Variant);
  protected
    function AccessError(const TypeName: string): EInstantAccessError;
    class function AttributeType: TInstantAttributeType; virtual; abstract;
    function ConversionError(E: Exception): EInstantConversionError;
    function GetAsBoolean: Boolean; virtual;
    function GetAsCurrency: Currency; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsDate: TDateTime; virtual;
    function GetAsTime: TDateTime; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsObject: TInstantObject; virtual;
    function GetAsString: string; virtual;
    function GetAsVariant: Variant; virtual;
    function GetDisplayText: string; virtual;
    function GetIsChanged: Boolean; virtual;
    function GetIsDefault: Boolean; virtual;
    function GetIsNull: Boolean; virtual;
    function GetIsMandatory: Boolean; virtual;
    function GetOwner: TInstantObject; reintroduce; virtual;
    procedure Initialize; override;
    procedure ReadName(Reader: TInstantReader);
    procedure SetAsBoolean(AValue: Boolean); virtual;
    procedure SetAsCurrency(AValue: Currency); virtual;
    procedure SetAsDateTime(AValue: TDateTime); virtual;
    procedure SetAsDate(AValue: TDateTime); virtual;
    procedure SetAsTime(AValue: TDateTime); virtual;
    procedure SetAsFloat(AValue: Double); virtual;
    procedure SetAsInteger(AValue: Integer); virtual;
    procedure SetAsObject(AValue: TInstantObject); virtual;
    procedure SetAsString(const AValue: string); virtual;
    procedure SetAsVariant(AValue: Variant); virtual;
    procedure SetIsChanged(Value: Boolean); virtual;
    procedure SetOwner(AOwner: TInstantObject); virtual;
    procedure WriteName(Writer: TInstantWriter);
    procedure Validate(const AValue: string); virtual;
  public
    constructor Create(AOwner: TInstantAbstractObject = nil; AMetadata:
      TInstantCollectionItem = nil); override;
    procedure Changed;
    procedure CheckHasMetadata;
    procedure Reset; virtual;
    procedure Unchanged; virtual;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsObject: TInstantObject read GetAsObject write SetAsObject;
    property AsString: string read GetAsString write SetAsstring;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property DisplayText: string read GetDisplayText;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property IsDefault: Boolean read GetIsDefault;
    property IsIndexed: Boolean read GetIsIndexed;
    property IsMandatory: Boolean read GetIsMandatory;
    property IsRequired: Boolean read GetIsRequired;
    property IsUnique: Boolean read GetIsUnique;
    property IsNull: Boolean read GetIsNull;
    property Name: string read GetName;
    property Metadata: TInstantAttributeMetadata read GetMetadata write SetMetadata;
    property Owner: TInstantObject read GetOwner;
    property Value: Variant read GetValue write SetValue;
  end;

  PInstantAttribute = ^TInstantAttribute;

  TInstantSimple = class(TInstantAttribute)
  protected
    function InvalidValueError(AValue: Variant; E: Exception): EInstantError;
  end;

  TInstantNumeric = class(TInstantSimple)
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetDisplayText: string; override;
    function GetIsDefault: Boolean; override;
    function GetIsNull: Boolean; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
  end;

  TInstantInteger = class(TInstantNumeric)
  private
    FValue: Integer;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue: Integer; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: Integer); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: Integer read GetValue write SetValue;
  end;

  TInstantFloat = class(TInstantNumeric)
  private
    FValue: Double;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue: Double; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: Double); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: Double read GetValue write SetValue;
  end;

  TInstantCurrency = class(TInstantNumeric)
  private
    FValue: Currency;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetDisplayText: string; override;
    function GetIsDefault: Boolean; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue: Currency; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: Currency); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: Currency read GetValue write SetValue;
  end;

  TInstantBoolean = class(TInstantSimple)
  private
    FValue: Boolean;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetIsDefault: Boolean; override;
    function GetIsNull: Boolean; override;
    function GetValue: Boolean; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: Boolean); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: Boolean read GetValue write SetValue;
  end;

  TInstantString = class(TInstantSimple)
  private
    FValue: string;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsBoolean: Boolean; override;
    function GetAsCurrency: Currency; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetValue: string; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsCurrency(AValue: Currency); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Integer); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(const AValue: string); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: string read GetValue write SetValue;
  end;

  TInstantCustomDateTime = class(TInstantSimple)
  private
    FValue: TDateTime;
    function DefaultValue: TDateTime;
  protected
    function GetAsDateTime: TDateTime; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDisplayText: string; override;
    function GetIsDefault: Boolean; override;
    function GetIsNull: Boolean; override;
    function GetValue: TDateTime; virtual;
    procedure Initialize; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    procedure SetValue(AValue: TDateTime); virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Value: TDateTime read GetValue write SetValue;
  end;

  TInstantDateTime = class(TInstantCustomDateTime)
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsDate: TDateTime; override;
    function GetAsTime: TDateTime; override;
    procedure SetAsDate(AValue: TDateTime); override;
    procedure SetAsTime(AValue: TDateTime); override;
 end;

  TInstantDate = class(TInstantCustomDateTime)
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsDate: TDateTime; override;
    procedure SetValue(AValue: TDateTime); override;
    procedure SetAsDate(AValue: TDateTime); override;
  end;

  TInstantTime = class(TInstantCustomDateTime)
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAsString: string; override;
    function GetAsTime: TDateTime; override;
    procedure SetValue(AValue: TDateTime); override;
    procedure SetAsTime(AValue: TDateTime); override;
  end;

  TInstantBlob = class(TInstantSimple)
  private
    FStream: TMemoryStream;
    function GetSize: Integer;
    function GetStream: TMemoryStream;
    function UseUnicode: boolean;
    property Stream: TMemoryStream read GetStream;
  protected
    function GetBytes: TInstantBytes;
    procedure SetBytes(const AValue: TInstantBytes);
    function GetValue: string; virtual;
    procedure SetValue(const AValue: string); virtual;
    class function AttributeType: TInstantAttributeType; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure Initialize; override;
    function Read(var Buffer; Position, Count: Integer): Integer; virtual;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsVariant(AValue: Variant); override;
    function Write(const Buffer; Position, Count: Integer): Integer; virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetIsNull: Boolean; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$IFDEF INSTANTOBJECTS_FMX}
    procedure AssignPicture(Source: TImage);
    procedure AssignToPicture(Dest: TImage);
{$ELSE}
    {$IFNDEF IO_CONSOLE}
    procedure AssignPicture(Source: TPicture);
    procedure AssignToPicture(Dest: TPicture);
    {$ENDIF}
{$ENDIF}
    procedure Clear;
    procedure LoadDataFromStream(AStream: TStream);
    function ReadBuffer(var Buffer; Position, Count: Integer): Integer; virtual;
    procedure Reset; override;
    procedure SaveDataToStream(AStream: TStream);
    function WriteBuffer(const Buffer; Position, Count: Integer): Integer; virtual;
    property Bytes: TInstantBytes read GetBytes write SetBytes;
    property Size: Integer read GetSize;
  published
    property Value: string read GetValue write SetValue;
  end;

  TInstantMemo = class(TInstantBlob)
  protected
    class function AttributeType: TInstantAttributeType; override;
    procedure SetValue(const AValue: string); override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure WriteObject(Writer: TInstantWriter); override;
  end;

  TInstantGraphic = class(TInstantBlob)
  protected
    class function AttributeType: TInstantAttributeType; override;
  end;

  TInstantComplex = class(TInstantAttribute)
  private
    function GetRequiredClass: TInstantObjectClass;
    function GetRequiredClassName: string;
  protected
    function GetAllowOwned: Boolean; virtual;
    function GetConnector: TInstantConnector; virtual;
    procedure ValidateObject(AObject: TInstantObject); virtual;
    procedure ValidateObjectClassType(AClass: TInstantObjectClass);
  public
    function AttachObject(AObject: TInstantObject): Boolean; virtual;
    property Connector: TInstantConnector read GetConnector;
    function DetachObject(AObject: TInstantObject): Boolean; virtual;
    property AllowOwned: Boolean read GetAllowOwned;
    property RequiredClass: TInstantObjectClass read GetRequiredClass;
    property RequiredClassName: string read GetRequiredClassName;
  end;

  TInstantElement = class(TInstantComplex)
  protected
    function CreateObject: TInstantObject; virtual;
    function GetAsObject: TInstantObject; override;
    function GetValue: TInstantObject; virtual; abstract;
    procedure SetAsObject(AValue: TInstantObject); override;
    procedure SetValue(AValue: TInstantObject); virtual;
    function GetIsNull: Boolean; override;
  public
    function AttachObject(AObject: TInstantObject): Boolean; override;
    function DetachObject(AObject: TInstantObject): Boolean; override;
    function HasValue: Boolean; virtual;
    procedure LoadObjectFromStream(AStream: TStream); virtual;
    procedure SaveObjectToStream(AStream: TStream); virtual;
  published
    {$IFDEF DELPHI_NEON}[NeonIgnore]{$ENDIF}
    property Value: TInstantObject read GetValue write SetValue;
  end;

  TInstantPart = class(TInstantElement)
  private
    FValue: TInstantObject;
    procedure ClearOwnerContext(AObject: TInstantObject);
    procedure DestroyObject;
    procedure SetOwnerContext(AObject: TInstantObject);
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetIsChanged: Boolean; override;
    function GetIsDefault: Boolean; override;
    function GetValue: TInstantObject; override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetValue(AValue: TInstantObject); override;
    procedure ValidateObject(AObject: TInstantObject); override;
    procedure WriteObject(Writer: TInstantWriter); override;
    function GetAllowOwned: Boolean; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function HasValue: Boolean; override;
    procedure Reset; override;
    procedure Unchanged; override;
  end;

  TInstantReference = class(TInstantElement)
  private
    FAllowOwned: Boolean;
    FConnector: TInstantConnector;
    FObjectReference: TInstantObjectReference;
    procedure DestroyObjectReference;
    function GetInstance: TInstantObject;
    function GetObjectClass: TInstantObjectClass;
    function GetObjectClassName: string;
    function GetObjectId: string;
    function GetObjectReference: TInstantObjectReference;
    function SameInstance(AObject: TInstantObject): Boolean; overload;
    function SameInstance(const AObjectClassName,
      AObjectId: string): Boolean; overload;
    function SameInstance(AObjectClass: TInstantObjectClass;
      AObjectId: string): Boolean; overload;
    procedure SetInstance(AValue: TInstantObject);
  protected
    class function AttributeType: TInstantAttributeType; override;
    function CreateObject: TInstantObject; override;
    function GetAllowOwned: Boolean; override;
    function GetConnector: TInstantConnector; override;
    function GetIsBroken: Boolean; virtual;
    function GetIsDefault: Boolean; override;
    function GetValue: TInstantObject; override;
    procedure ReadObject(Reader: TInstantReader); override;
    function RetrieveObject: TInstantObject; virtual;
    procedure SetAllowOwned(Value: Boolean); virtual;
    procedure SetConnector(Value: TInstantConnector);
    procedure SetValue(AValue: TInstantObject); override;
    procedure ValidateObject(AObject: TInstantObject); override;
    procedure WriteObject(Writer: TInstantWriter); override;
    property Instance: TInstantObject read GetInstance write SetInstance;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DestroyObject;
    function HasReference: Boolean;
    function HasValue: Boolean; override;
    procedure LoadObjectFromStream(AStream: TStream); override;
    procedure ReferenceObject(AObjectClass: TInstantObjectClass;
      const AObjectId: string); overload;
    procedure ReferenceObject(const AObjectClassName, AObjectId: string); overload;
    procedure Reset; override;
    property ObjectReference: TInstantObjectReference read GetObjectReference;
    property AllowOwned write SetAllowOwned;
    property Connector write SetConnector;
    property IsBroken: Boolean read GetIsBroken;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty(IO_SER_CLASSNAME)]{$ENDIF}
    property ObjectClassName: string read GetObjectClassName;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty(IO_SER_ID)]{$ENDIF}
    property ObjectId: string read GetObjectId;
  end;

  TInstantContainer = class(TInstantComplex)
  private
    function GetItems(Index: Integer): TInstantObject;
    function GetChangeCount: Integer;
    procedure SetItems(Index: Integer; AValue: TInstantObject);
    procedure QuickSort(L, R: Integer; Compare: TInstantSortCompare);
  protected
    procedure AfterContentChange(ChangeType: TInstantContentChangeType;
      Index: Integer; AObject: TInstantObject);
    procedure BeforeContentChange(ChangeType: TInstantContentChangeType;
      Index: Integer; AObject: TInstantObject);
    procedure CheckRange(Index: Integer);
    function GetCount: Integer; virtual; abstract;
    function GetInstances(Index: Integer): TInstantObject; virtual;
    function GetIsDefault: Boolean; override;
    function InternalAdd(AObject: TInstantObject): Integer; virtual; abstract;
    function InternalAddReference(const AObjectClassName, AObjectId: string):
        Integer; virtual; abstract;
    procedure InternalClear; virtual; abstract;
    procedure InternalDelete(Index: Integer); virtual; abstract;
    function InternalGetItems(Index: Integer): TInstantObject; virtual; abstract;
    function InternalIndexOf(AObject: TInstantObject): Integer; virtual; abstract;
    function InternalIndexOfInstance(Instance: Pointer): Integer; virtual; abstract;
    procedure InternalExchange(Index1, Index2: Integer); virtual; abstract;
    procedure InternalInsert(Index: Integer; AObject: TInstantObject); virtual; abstract;
    procedure InternalMove(CurIndex, NewIndex: Integer); virtual; abstract;
    procedure InternalSetItems(Index: Integer; AValue: TInstantObject); virtual; abstract;
    procedure ValidateObject(AObject: TInstantObject); override;
    property Instances[Index: Integer]: TInstantObject read GetInstances;
  public
    function Add(AObject: TInstantObject): Integer;
    function AddReference(const AObjectClassName, AObjectId: string): Integer;
    function AttachObject(AObject: TInstantObject): Boolean; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    function DetachObject(AObject: TInstantObject): Boolean; override;
    procedure Exchange(Index1, Index2: Integer);
    function HasItem(Index: Integer): Boolean;
    function IndexOf(AObject: TInstantObject): Integer;
    function IndexOfInstance(Instance: Pointer): Integer;
    procedure Insert(Index: Integer; AObject: TInstantObject);
    procedure LoadObjectsFromStream(AStream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(AObject: TInstantObject): Integer;
    procedure Reset; override;
    procedure SaveObjectsToStream(AStream: TStream);
    procedure Sort(Compare: TInstantSortCompare);
    function isVirtual: boolean;
    function isExternal: boolean;
    function isExmbedded: Boolean;
    property ChangeCount: Integer read GetChangeCount;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantObject read GetItems write SetItems; default;
  end;

  TInstantParts = class(TInstantContainer)
  private
    FAllowOwned: Boolean;
    FObjectReferenceList: TObjectList;
    FObjectList: TObjectList;
    function GetObjectList: TObjectList;
    procedure ClearOwnerContext(AObject: TInstantObject);
    procedure SetOwnerContext(AObject: TInstantObject);
    property ObjectList: TObjectList read GetObjectList;
    function CreateObjectReference(AObject: TInstantObject): TInstantObjectReference;
    function GetObjectReferenceList: TObjectList;
    function GetObjectReferences(Index: Integer): TInstantObjectReference;
    procedure SetObjectReferences(Index: Integer; Value: TInstantObjectReference);
    property ObjectReferenceList: TObjectList read GetObjectReferenceList;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAllowOwned: Boolean; override;
    function GetCount: Integer; override;
    function GetIsChanged: Boolean; override;
    function GetInstances(Index: Integer): TInstantObject; override;
    function InternalAdd(AObject: TInstantObject): Integer; override;
    function InternalAddReference(const AObjectClassName, AObjectId: string):
        Integer; override;
    procedure InternalClear; override;
    procedure InternalDelete(Index: Integer); override;
    function InternalGetItems(Index: Integer): TInstantObject; override;
    function InternalIndexOf(AObject: TInstantObject): Integer; override;
    function InternalIndexOfInstance(Instance: Pointer): Integer; override;
    procedure InternalExchange(Index1, Index2: Integer); override;
    procedure InternalInsert(Index: Integer; AObject: TInstantObject); override;
    procedure InternalMove(CurIndex, NewIndex: Integer); override;
    procedure InternalSetItems(Index: Integer; AValue: TInstantObject); override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAllowOwned(Value: Boolean); virtual;
    procedure ValidateObject(AObject: TInstantObject); override;
    procedure WriteObject(Writer: TInstantWriter); override;
    property ObjectReferences[Index: Integer]: TInstantObjectReference read GetObjectReferences write SetObjectReferences;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Unchanged; override;
    procedure DestroyObject(Index: Integer); overload;
    function DestroyObject(AObject: TInstantObject): Boolean; overload;
    property AllowOwned write SetAllowOwned;
  end;

  TInstantReferences = class(TInstantContainer)
  private
    FAllowOwned: Boolean;
    FConnector: TInstantConnector;
    FObjectReferenceList: TInstantObjectReferenceList;
    function GetObjectReferenceList: TInstantObjectReferenceList;
    function GetRefItems(Index: Integer): TInstantObjectReference;
  protected
    class function AttributeType: TInstantAttributeType; override;
    function GetAllowOwned: Boolean; override;
    function GetConnector: TInstantConnector; override;
    function GetCount: Integer; override;
    function GetInstances(Index: Integer): TInstantObject; override;
    function HasInstance(Index: Integer): boolean;
    function InternalAdd(AObject: TInstantObject): Integer; override;
    function InternalAddReference(const AObjectClassName, AObjectId: string):
        Integer; override;
    procedure InternalClear; override;
    procedure InternalDelete(Index: Integer); override;
    function InternalGetItems(Index: Integer): TInstantObject; override;
    function InternalIndexOf(AObject: TInstantObject): Integer; override;
    function InternalIndexOfInstance(Instance: Pointer): Integer; override;
    procedure InternalExchange(Index1, Index2: Integer); override;
    procedure InternalInsert(Index: Integer; AObject: TInstantObject); override;
    procedure InternalMove(CurIndex, NewIndex: Integer); override;
    procedure InternalSetItems(Index: Integer; AValue: TInstantObject); override;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure SetAllowOwned(Value: Boolean); virtual;
    procedure ValidateObject(AObject: TInstantObject); override;
    procedure WriteObject(Writer: TInstantWriter); override;
    property ObjectReferenceList: TInstantObjectReferenceList read
        GetObjectReferenceList;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DestroyObject(Index: Integer); overload;
    function DestroyObject(AObject: TInstantObject): Boolean; overload;
    procedure LoadObjectsFromStream(AStream: TStream); override;
    procedure LoadReferencesFromStream(AStream: TStream);
    procedure SaveReferencesToStream(AStream: TStream);
    property AllowOwned write SetAllowOwned;
    property Connector write FConnector;
    property RefItems[Index: Integer]: TInstantObjectReference read GetRefItems;
  end;

  TInstantEnum = class(TInstantInteger)
  end;

  TInstantObjectState = class(TPersistent)
  private
    FIsChanged: Boolean;
    FPersistentId: string;
    FUpdateCount: Integer;
  public
    procedure Assign(Source: TPersistent); override;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
    property PersistentId: string read FPersistentId write FPersistentId;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
  end;

  TInstantObject = class(TInstantAbstractObject, IUnknown)
  private
    FDefaultContainer: TInstantContainer;
    FChangeCount: Integer;
    FChangesDisabledCount: Integer;
    FId: string;
    FInUpdate: Boolean;
    FObjectStore: TInstantObjectStore;
    FOwner: TInstantObject;
    FOwnerAttribute: TInstantComplex;
    FRefBy: TObjectList;
    FRefCount: Integer;
    FSavedState: TInstantObjectState;
    FSaveStateLevel: Integer;
    FState: TInstantObjectState;
    FOnAfterContentChange: TInstantContentChangeEvent;
    FOnAttributeChanged: TInstantAttributeChangeEvent;
    FOnIdChanged: TInstantIdChangeEvent;
    FOnBeforeContentChange: TInstantContentChangeEvent;
    FOnChange: TInstantNotifyEvent;
    FOnError: TInstantErrorEvent;
    FOnBeforeDispose: TInstantNotifyEvent;
    procedure ClearOwnerContext;
    procedure CreateAttributes;
    procedure DestroyAttributes;
    procedure DestroyInternalFields;
    procedure DoAfterContentChange(Container: TInstantContainer;
      ChangeType: TInstantContentChangeType; Index: Integer; AObject: TInstantObject);
    procedure DoAfterCreate;
    procedure DoAfterDispose;
    procedure DoAfterRefresh;
    procedure DoAfterRetrieve;
    procedure DoAfterStore;
    procedure DoBeforeContentChange(Container: TInstantContainer;
      ChangeType: TInstantContentChangeType; Index: Integer; AObject: TInstantObject);
    procedure DoBeforeDispose;
    procedure DoBeforeRefresh;
    procedure DoBeforeStore;
    procedure DoChange;
    procedure DoDispose(ConflictAction: TInstantConflictAction);
    procedure DoHandleError(OperationType: TInstantOperationType; E: Exception;
      var Action: TInstantErrorAction);
    procedure DoOwnedObjectChanged(AObject: TInstantObject);
    procedure DoOwnerChanged(OldOwner: TInstantObject; OldOwnerAttribute: TInstantComplex);
    procedure DoRefresh;
    function DoRelease: Integer;
    procedure DoStore(ConflictAction: TInstantConflictAction);
    procedure DoUnchange;
    function FindDefaultContainer: TInstantContainer;
{$IFNDEF IO_NO_CIRCULAR_REFERENCE_CHECK}
    procedure FreeCircularReferences;
{$ENDIF}
    function GetClassId: string;
    function GetDefaultContainer: TInstantContainer;
    function GetHasDefaultContainer: Boolean;
    function GetIsAbandoned: Boolean;
    function GetIsChanged: Boolean;
    function GetIsDefault: Boolean;
    function GetIsIdChanged: Boolean;
    function GetIsOwned: Boolean;
    function GetIsPersistent: Boolean;
    function GetObjectChangeCount: Integer;
    function GetObjectCount: Integer;
    function GetObjects(Index: Integer): TInstantObject;
    function GetObjectStore: TInstantObjectStore;
    function GetPersistentId: string;
    function GetRefBy(Index: Integer): TInstantComplex;
    function GetRefByCount: Integer;
    function GetRefByList: TObjectList;
    function GetSavedState: TInstantObjectState;
    function GetState: TInstantObjectState;
    function GetUpdateCount: Integer;
    procedure Init;
    procedure Finit;
    function GetConnector: TInstantConnector;
    procedure PerformUpdate(Operation: TInstantUpdateOperation; OperationType: TInstantOperationType;
      ConflictAction: TInstantConflictAction);
    procedure ReadAttributes(Reader: TInstantReader);
    procedure SetIsChanged(Value: Boolean);
    procedure SetObjects(Index: Integer; Value: TInstantObject);
    procedure SetOwnerContext(AOwner: TInstantObject; Attribute: TInstantComplex);
    procedure SetPersistentId(const Value: string);
    procedure SetUpdateCount(Value: Integer);
    procedure WriteAttributes(Writer: TInstantWriter);
    property ObjectStore: TInstantObjectStore read GetObjectStore;
    property SavedState: TInstantObjectState read GetSavedState;
    property State: TInstantObjectState read GetState;
  protected
    procedure DoAttributeChanged(Attribute: TInstantAttribute); virtual;
    procedure Abandon;
    procedure AfterAddRef; virtual;
    procedure AfterAssign; virtual;
    procedure AfterContentChange(Container: TInstantContainer; ChangeType: TInstantContentChangeType;
      Index: Integer; AObject: TInstantObject); virtual;
    procedure AfterCreate; virtual;
    procedure AfterCreateAttributes; virtual;
    procedure AfterDispose; virtual;
    procedure AfterRefresh; virtual;
    procedure AfterRetrieve; virtual;
    procedure AfterStore; virtual;
    procedure ApplyState; virtual;
    function AttributeAddress(const AttributeName: string): PInstantAttribute; virtual;
    procedure AttributeChanged(Attribute: TInstantAttribute); virtual;
    procedure BeforeAssign; virtual;
    procedure BeforeContentChange(Container: TInstantContainer; ChangeType: TInstantContentChangeType;
      Index: Integer; AObject: TInstantObject); virtual;
    procedure BeforeCreateAttributes; virtual;
    procedure BeforeDispose; virtual;
    procedure BeforeRelease; virtual;
    procedure BeforeRefresh; virtual;
    procedure BeforeStore; virtual;
    class procedure ConvertToBinary(Converter: TInstantTextToBinaryConverter); override;
    class procedure ConvertToText(Converter: TInstantBinaryToTextConverter); override;
    class function CreateInstance(Arg: Pointer = nil): TInstantStreamable; override;
    procedure Destruct; virtual;
    procedure DisposeOwnedObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction);
    procedure Finalize; virtual;
    function GenerateId: string; virtual;
    function GetCaption: string; virtual;
    function GetId: string; virtual;
    function GetObjectClass: TInstantAbstractObjectClass; virtual;
    function GetOwner: TPersistent; override;
    procedure HandleError(OperationType: TInstantOperationType; E: Exception;
      var Action: TInstantErrorAction); virtual;
    procedure Initialize; virtual;
    procedure OwnedObjectChanged(AObject: TInstantObject); virtual;
    procedure OwnerChanged(OldOwner: TInstantObject; OldOwnerAttribute: TInstantComplex); virtual;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure ReadObject(Reader: TInstantReader); override;
    procedure RestoreState; virtual;
    procedure SaveState; virtual;
    procedure SetConnector(AConnector: TInstantConnector); virtual;
    procedure SetId(const Value: string); virtual;
    function VerifyOperation(OperationType: TInstantOperationType): TInstantVerificationResult; virtual;
    procedure WriteObject(Writer: TInstantWriter); override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    property DefaultContainer: TInstantContainer read GetDefaultContainer;
    property RefByList: TObjectList read GetRefByList;
  public
    class function ReformatID(const Id : string) : string; virtual;
    constructor Clone(Source: TInstantObject;
      AConnector: TInstantConnector = nil); overload; virtual;
    constructor Create(AConnector: TInstantConnector = nil); virtual;
    constructor Retrieve(const AObjectId: string; CreateIfMissing: Boolean = False;
        ARefresh: Boolean = False; AConnector: TComponent = nil;
        const AObjectData: TInstantAbstractObjectData = nil); override;
    function AddObject(AObject: TInstantObject): Integer; overload;
    function AddRef: Integer;
    procedure Assign(Source: TPersistent); override;
    function AttributeByName(const AttributeName: string): TInstantAttribute;
    function CanDispose: Boolean;
    function CanStore: Boolean;
    procedure Changed; virtual;
    function ChangesDisabled: Boolean;
    procedure CheckId;
    {$IFDEF FPC}class{$ENDIF} function ClassType: TInstantObjectClass;
    procedure ClearObjects;
    function Clone(AConnector: TInstantConnector = nil): TInstantObject; overload;
    function ContainerByName(const ContainerName: string): TInstantContainer;
    procedure DeleteObject(Index: Integer);
    procedure DisableChanges;
    procedure Dispose(ConflictAction: TInstantConflictAction = caFail);
    procedure EnableChanges;
    function EqualsSignature(const AObjectClassName, AObjectId: string): Boolean;
    function EqualsPersistentSignature(const AObjectClassName,
      AObjectId: string): Boolean;
    function FindAttribute(const AttributeName: string): TInstantAttribute;
    function FindContainer(const ContainerName: string): TInstantContainer;
    procedure FreeInstance; override;
    function GetNamePath: string; override;
    function IndexOfObject(AObject: TInstantObject): Integer;
    procedure InsertObject(Index: Integer; AObject: TInstantObject);
    function IsOperationAllowed(OperationType: TInstantOperationType): Boolean;
    function Invoke(const MethodName: string; Params: TParams): string;
    class function Metadata: TInstantClassMetadata; virtual;
    class function NewInstance: TObject; override;
    procedure Refresh;
    class procedure RefreshAll(AConnector: TInstantConnector = nil;
      Progress: TInstantProgressEvent = nil);
    function Release: Integer;
    function RemoveObject(AObject: TInstantObject): Integer;
    procedure ResetAttributes;
    procedure Store(ConflictAction: TInstantConflictAction = caFail);
    procedure Unchanged;
    procedure GetDetailsStatementValues(var FromClause,
      SequenceNoFieldName, OrderByClause: string); virtual;
    procedure ForEachAttribute(Proc: TInstantAttributeProc); virtual;
    property Caption: string read GetCaption;
    class function GetSerializedIdPropertyName: string; virtual;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty(IO_SER_CLASSNAME)]{$ENDIF}
    property ClassId: string read GetClassId;
    property Connector: TInstantConnector read GetConnector;
    property HasDefaultContainer: Boolean read GetHasDefaultContainer;
    property IsChanged: Boolean read GetIsChanged write SetIsChanged;
    property IsDefault: Boolean read GetIsDefault;
    property IsIdChanged: Boolean read GetIsIdChanged;
    property IsAbandoned: Boolean read GetIsAbandoned;
    property IsOwned: Boolean read GetIsOwned;
    property IsPersistent: Boolean read GetIsPersistent;
    property ObjectChangeCount: Integer read GetObjectChangeCount;
    property ObjectClass: TInstantAbstractObjectClass read GetObjectClass;
    property ObjectCount: Integer read GetObjectCount;
    {$IFDEF DELPHI_NEON}[NeonInclude, NeonProperty(IO_SER_ID)]{$ENDIF}
    property Id: string read GetId write SetId;
    property InUpdate: Boolean read FInUpdate write FInUpdate;
    property Objects[Index: Integer]: TInstantObject read GetObjects
      write SetObjects;
    property Owner: TInstantObject read FOwner;
    property OwnerAttribute: TInstantComplex read FOwnerAttribute;
    property PersistentId: string read GetPersistentId;
    property RefCount: Integer read FRefCount;
    property RefBy[Index: Integer]: TInstantComplex read GetRefBy;
    property RefByCount: Integer read GetRefByCount;
    property UpdateCount: Integer read GetUpdateCount;
    property OnBeforeDispose: TInstantNotifyEvent
      read FOnBeforeDispose write FOnBeforeDispose;
    property OnAfterContentChange: TInstantContentChangeEvent
      read FOnAfterContentChange write FOnAfterContentChange;
    property OnAttributeChanged: TInstantAttributeChangeEvent
      read FOnAttributeChanged write FOnAttributeChanged;
    property OnBeforeContentChange: TInstantContentChangeEvent
      read FOnBeforeContentChange write FOnBeforeContentChange;
    property OnChange: TInstantNotifyEvent read FOnChange write FOnChange;
    property OnError: TInstantErrorEvent read FOnError write FOnError;
    property OnIdChanged: TInstantIdChangeEvent read FOnIdChanged write FOnIdChanged;
  end;

  TInstantCacheNode = class(TObject)
  private
    FColor: TInstantCacheNodeColor;
    FItem: TInstantObject;
    FLeft: TInstantCacheNode;
    FParent: TInstantCacheNode;
    FRight: TInstantCacheNode;
    function GetSibling: TInstantCacheNode;
  protected
    function IsRed: Boolean;
    function IsBlack: Boolean;
    property Left: TInstantCacheNode read FLeft write FLeft;
    property Right: TInstantCacheNode read FRight write FRight;
    property Item: TInstantObject read FItem write FItem;
    property Color: TInstantCacheNodeColor read FColor write FColor;
    property Parent: TInstantCacheNode read FParent write FParent;
  public
    constructor Create(AItem: TInstantObject; ALeaf,
      AParent: TInstantCacheNode; AColor: TInstantCacheNodeColor);
    function IsLeftChild: Boolean;
    function IsRightChild: Boolean;
    property Sibling: TInstantCacheNode read GetSibling;
  end;

  TInstantCache = class(TObject)
  private
    FCount: Integer;
    FLeaf: TInstantCacheNode;
    FRoot: TInstantCacheNode;
    function GetItems(Index: Integer): TInstantObject;
  protected
    function CompareIds(const Id1, Id2: string): Integer;
    function CompareObjects(Object1, Object2: TInstantObject): Integer;
    procedure DeleteFixup(ANode: TInstantCacheNode);
    function FindNode(const AObjectId: string): TInstantCacheNode;
    procedure LeftRotate(ANode: TInstantCacheNode);
    function Pred(ANode: TInstantCacheNode): TInstantCacheNode;
    procedure Rebalance(ANode: TInstantCacheNode);
    procedure RightRotate(ANode: TInstantCacheNode);
    function Succ(ANode: TInstantCacheNode): TInstantCacheNode;
    property Leaf: TInstantCacheNode read FLeaf;
    property Root: TInstantCacheNode read FRoot write FRoot;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AObject: TInstantObject);
    procedure Clear;
    function Find(const AObjectId: string): TInstantObject;
    procedure ForEach(Proc: TInstantObjectProc);
    procedure Remove(AObject: TInstantObject);
    property Count: Integer read FCount;
    property Items[Index: Integer]: TInstantObject read GetItems;
  end;

  TInstantCacheEnumerator = class(TObject)
  private
    FCache: TInstantCache;
    FNode: TInstantCacheNode;
  protected
    property Node: TInstantCacheNode read FNode write FNode;
    property Cache: TInstantCache read FCache;
  public
    constructor Create(ACache: TInstantCache);
    function HasNext: Boolean;
    function Next: TInstantCacheNode;
    procedure Reset;
  end;

  TInstantObjectStore = class(TInstantCollectionItem)
  private
    FObjectClass: TInstantObjectClass;
    FConnector: TInstantConnector;
    FCache: TInstantCache;
    procedure AddToCache(AObject: TInstantObject);
    procedure CheckBroker(ABroker: TInstantBroker);
    function GetBroker: TInstantBroker;
    function GetCount: Integer;
    function GetItems(Index: Integer): TInstantObject;
    procedure RemoveFromCache(AObject: TInstantObject);
  protected
    procedure AbandonObject(AObject: TInstantObject);
    function GetName: string; override;
    property Broker: TInstantBroker read GetBroker;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AbandonObjects;
    procedure DisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction);
    function Find(const AObjectId: string): TInstantObject;
    procedure ObjectDestroyed(AObject: TInstantObject);
    procedure RefreshObject(AObject: TInstantObject);
    function RetrieveObject(const AObjectId: string; const AObject: TInstantObject;
      const AObjectData: TInstantAbstractObjectData = nil): Boolean;
    procedure StoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction);
    property Connector: TInstantConnector read FConnector write FConnector;
    property ObjectClass: TInstantObjectClass read FObjectClass
      write FObjectClass;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantObject read GetItems;
  end;

  TInstantObjectStores = class(TInstantCollection)
  private
    function GetItems(Index: Integer): TInstantObjectStore;
    procedure SetItems(Index: Integer; Value: TInstantObjectStore);
  public
    constructor Create;
    function AddObjectStore: TInstantObjectStore;
    function FindObjectStore(AClass: TInstantObjectClass): TInstantObjectStore;
    property Items[Index: Integer]: TInstantObjectStore read GetItems
      write SetItems; default;
  end;

  EInstantDBBuildError = class(EInstantError);

  // Abstract; represents a step of the database build sequence.
  TInstantDBBuildCommand = class(TObject)
  private
    FEnabled: Boolean;
    FCommandType: TInstantDBBuildCommandType;
    FOldMetadata: TInstantMetadata;
    FNewMetadata: TInstantMetadata;
    FConnector: TInstantConnector;
    function GetConnector: TInstantConnector;
  protected
    // Executes the database build step.
    procedure InternalExecute; virtual; abstract;
    // Computes and returns the step's description.
    function GetDescription: string; virtual;
    // Called if an exception occurs in InternalExecute. It should do
    // whatever is needed to handle the error and return True if the exception
    // needs to be trapped, otherwise it's re-raised. The default implementation
    // just returns False.
    function InternalExecuteHandleError(const E: Exception): Boolean; virtual;
  public
    constructor Create(const ACommandType: TInstantDBBuildCommandType;
      const AConnector: TInstantConnector = nil);
    // Type of the step, usually set on creation.
    property CommandType: TInstantDBBuildCommandType read FCommandType;
    // A description of what the step will consist of.
    property Description: string read GetDescription;
    destructor Destroy; override;
    property OldMetadata: TInstantMetadata read FOldMetadata write FOldMetadata;
    property NewMetadata: TInstantMetadata read FNewMetadata write FNewMetadata;
    property Connector: TInstantConnector read GetConnector;
    // Executes the database build step.
    procedure Execute;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;
  TInstantDBBuildCommandClass = class of TInstantDBBuildCommand;

  // An unsupported step. Raises an exception when executed.
  TInstantUnsupportedDBBuildCommand = class(TInstantDBBuildCommand)
  protected
    procedure InternalExecute; override;
  end;

  TInstantQuery = class(TComponent)
  private
    FCommand: string;
    FConnector: TInstantConnector;
    FMaxCount: Integer;
    FRequestedLoadMode: TInstantLoadMode;
    FActualLoadMode: TInstantLoadMode;
    function GetConnector: TInstantConnector;
    function GetObjectCount: Integer;
    function GetObjects(Index: Integer): TObject;
    procedure SetActive(Value: Boolean);
    procedure SetCommand(const Value: string);
  protected
    function GetActive: Boolean; virtual;
    function GetObjectClass: TClass; virtual;
    function GetObjectClassName: string; virtual;
    function GetParams: TParams; virtual;
    function InternalAddObject(AObject: TObject): Integer; virtual;
    procedure InternalClose; virtual;
    procedure InternalFetchParams(AParams: TParams); virtual;
    function InternalGetObjectCount: Integer; virtual;
    function InternalGetObjects(Index: Integer): TObject; virtual;
    function InternalIndexOfObject(AObject: TObject): Integer; virtual;
    procedure InternalInsertObject(Index: Integer; AObject: TObject); virtual;
    procedure InternalOpen; virtual;
    procedure InternalRefreshObjects; virtual;
    procedure InternalReleaseObject(AObject: TObject); virtual;
    function InternalRemoveObject(AObject: TObject): Integer; virtual;
    function ObjectFetched(Index: Integer): Boolean; virtual;
    procedure SetParams(Value: TParams); virtual;
    procedure TranslateCommand; virtual;
    function InternalGetObjectReferenceId(Index: Integer) : string; virtual;
    procedure SetActualLoadMode(const AValue: TInstantLoadMode);
  public
    constructor Create(AConnector: TInstantConnector); reintroduce; virtual;
    function AddObject(AObject: TObject): Integer;
    procedure ApplyChanges;
    procedure Close;
    procedure FetchParams(const ACommandText: string; AParams: TParams);
    function HasChangedObject: Boolean;
    function HasObject(AObject: TObject): Boolean;
    function IndexOfObject(AObject: TObject): Integer;
    procedure InsertObject(Index: Integer; AObject: TObject);
    procedure Open;
    procedure RefreshObjects;
    procedure ReleaseObject(AObject: TObject);
    function RemoveObject(AObject: TObject): Integer;
    property Active: Boolean read GetActive write SetActive;
    property Command: string read FCommand write SetCommand;
    property Connector: TInstantConnector read GetConnector;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property ObjectClass: TClass read GetObjectClass;
    property ObjectClassName: string read GetObjectClassName;
    property ObjectCount: Integer read GetObjectCount;
    property ObjectReferenceId[Index: Integer]: string read InternalGetObjectReferenceId;
    property Objects[Index: Integer]: TObject read GetObjects;
    property Params: TParams read GetParams write SetParams;
    property RequestedLoadMode: TInstantLoadMode
      read FRequestedLoadMode write FRequestedLoadMode default lmKeysFirst;
    property ActualLoadMode: TInstantLoadMode read FActualLoadMode;
    {$IFDEF D17+}
    procedure GetObjectList(const AList: TList<TInstantObject>);
    {$ENDIF}
  end;

  EInstantConflict = class(EInstantError)
  end;

  EInstantKeyViolation = class(EInstantConflict)
  end;

  TInstantObjectNotifier = class(TObject)
  private
    FDisableCount: Integer;
    FIncludeOwned: Boolean;
    FObjectClass: TInstantObjectClass;
    FOwner: TInstantObjectNotifiers;
    FOnObjectChanged: TInstantNotifyEvent;
    FOnObjectCreated: TInstantNotifyEvent;
    FOnObjectDisposed: TInstantNotifyEvent;
    FOnObjectRefreshed: TInstantNotifyEvent;
    FOnObjectRetrieved: TInstantNotifyEvent;
    FOnObjectStored: TInstantNotifyEvent;
    FOnNotify: TInstantObjectNotifyEvent;
    procedure DoObjectChanged(AObject: TInstantObject);
    procedure DoObjectCreated(AObject: TInstantObject);
    procedure DoObjectDisposed(AObject: TInstantObject);
    procedure DoObjectRefreshed(AObject: TInstantObject);
    procedure DoObjectRetrieved(AObject: TInstantObject);
    procedure DoObjectStored(AObject: TInstantObject);
    function GetObjectClass: TInstantObjectClass;
    function GetOwner: TInstantObjectNotifiers;
  protected
    function IncludeObject(AObject: TInstantObject): Boolean; virtual;
    procedure ObjectChanged(AObject: TInstantObject); virtual;
    procedure ObjectCreated(AObject: TInstantObject); virtual;
    procedure ObjectDisposed(AObject: TInstantObject); virtual;
    procedure ObjectRefreshed(AObject: TInstantObject); virtual;
    procedure ObjectRetrieved(AObject: TInstantObject); virtual;
    procedure ObjectStored(AObject: TInstantObject); virtual;
  public
    constructor Create(AObjectClass: TInstantObjectClass = nil;
      IncludeOwned: Boolean = False; AOwner: TInstantObjectNotifiers = nil);
    destructor Destroy; override;
    procedure Disable;
    function Disabled: Boolean;
    procedure Enable;
    procedure Notify(AObject: TInstantObject;
      Notification: TInstantObjectNotification);
    property IncludeOwned: Boolean read FIncludeOwned write FIncludeOwned;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
    property Owner: TInstantObjectNotifiers read GetOwner;
    property OnObjectChanged: TInstantNotifyEvent read FOnObjectChanged
      write FOnObjectChanged;
    property OnObjectCreated: TInstantNotifyEvent read FOnObjectCreated
      write FOnObjectCreated;
    property OnObjectDisposed: TInstantNotifyEvent read FOnObjectDisposed
      write FOnObjectDisposed;
    property OnObjectRefreshed: TInstantNotifyEvent read FOnObjectRefreshed
      write FOnObjectRefreshed;
    property OnObjectRetrieved: TInstantNotifyEvent read FOnObjectRetrieved
      write FOnObjectRetrieved;
    property OnObjectStored: TInstantNotifyEvent read FOnObjectStored
      write FOnObjectStored;
    property OnNotify: TInstantObjectNotifyEvent read FOnNotify write FOnNotify;
  end;

  TInstantObjectNotifiers = class(TObject)
  private
    FNotifyingCount: Integer;
    FItemList: TList;
    FDisableCount: Integer;
    FObjectClass: TInstantObjectClass;
    procedure BeginNotify;
    procedure EndNotify;
    function GetCount: Integer;
    function GetIsNotifying: Boolean;
    function GetItemList: TList;
    function GetItems(Index: Integer): TInstantObjectNotifier;
    function GetObjectClass: TInstantObjectClass;
  protected
    function AcceptItem(Item: TInstantObjectNotifier): Boolean; virtual;
    property ItemList: TList read GetItemList;
    property IsNotifying: Boolean read GetIsNotifying;
  public
    constructor Create(AObjectClass: TInstantObjectClass = nil);
    destructor Destroy; override;
    function Add(Item: TInstantObjectNotifier): Integer;
    procedure Disable;
    function Disabled: Boolean;
    procedure Enable;
    procedure Extract(Item: TInstantObjectNotifier);
    procedure Notify(AObject: TInstantObject;
      Notification: TInstantObjectNotification);
    function Remove(Item: TInstantObjectNotifier): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantObjectNotifier read GetItems;
      default;
    property ObjectClass: TInstantObjectClass read GetObjectClass;
  end;

  TInstantProgressLoop = class(TObject)
  private
    FLow, FHigh, FStep: Integer;
    FOnProgress: TInstantProgressEvent;
  public
    constructor Create(ALow, AHigh: Integer;
      ProgressEvent: TInstantProgressEvent);
    function Done: Boolean;
    procedure Next;
    procedure Progress(Sender: TObject; Count: Integer; var Continue: Boolean);
    property Step: Integer read FStep;
  end;

  TInstantObjectReferenceList = class(TObject)
  private
    FConnector: TInstantConnector;
    FList: TObjectList;
    FRefOwner: TInstantComplex;
    FRefOwnsInstance: Boolean;
    function CreateObjectReference(AObject: TInstantObject):
        TInstantObjectReference;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItems(Index: Integer): TInstantObject;
    function GetRefItems(Index: Integer): TInstantObjectReference;
    procedure SetCapacity(Value: Integer);
    procedure SetItems(Index: Integer; const Value: TInstantObject);
  public
    constructor Create(ARefOwnsInstance: Boolean;
      AConnector: TInstantConnector = nil; ARefOwner: TInstantComplex = nil);
    destructor Destroy; override;
    function Add: TInstantObjectReference; overload;
    function Add(Item: TInstantObject): Integer; overload;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function IndexOf(Item: TInstantObject; NeedInstance: Boolean = False): Integer;
    function IndexOfInstance(Item: TInstantObject): Integer;
    function IndexOfReference(AObjectReference: TInstantObjectReference): Integer;
    procedure Insert(Index: Integer; Item: TInstantObject);
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TInstantObject): Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantObject read GetItems write SetItems; default;
    property RefItems[Index: Integer]: TInstantObjectReference read GetRefItems;
  end;

  TInstantBroker = class(TInstantStreamable)
  private
    FConnector: TInstantConnector;
    function GetConnector: TInstantConnector;
  protected
    // Creates an instance of TInstantCatalog suited for the broker and
    // back-end database engine. Must be overridden in derived classes that
    // intend to support catalog-based functionality, like database structure
    // evolution. The predefined implementation just returns nil. Call
    // IsCatalogSupported to know if CreateCatalog will return an instance of
    // the catalog object or nil.
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; virtual;
    class function GetCatalogClass: TInstantCatalogClass; virtual;
    function GetDatabaseName: string; virtual;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); virtual;
    function InternalCreateQuery: TInstantQuery; virtual;
    function InternalDisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; virtual; abstract;
    function InternalRetrieveObject(AObject: TInstantObject;
      const AObjectId: string; ConflictAction: TInstantConflictAction;
      const AObjectData: TInstantAbstractObjectData = nil): Boolean; virtual; abstract;
    function InternalStoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean; virtual; abstract;
  public
    constructor Create(AConnector: TInstantConnector); virtual;
    destructor Destroy; override;
    // Legacy database building code, to be removed when all brokers will have
    // catalogs.
    procedure BuildDatabase(Scheme: TInstantScheme);
    // Creates a database build command object that can perform the build
    // operation represented by CommandType. The predefined implementation
    // creates an instance of TInstantUnsupportedDBBuildCommand, which just 
    // raises an exception when executed. Derived classes should create and 
    // return instances of specific TInstantDBBuildCommand and fall back to 
    // inherited for unsupported values of CommandType.
    function CreateDBBuildCommand(const CommandType: TInstantDBBuildCommandType)
      : TInstantDBBuildCommand; virtual;
    function CreateQuery: TInstantQuery;
    function DisposeObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean;
    // Creates and returns a TInstantScheme object that represents the current
    // database scheme (which may differ from the model-derived scheme).
    // If the broker doesn't have a catalog, calling this method will raise
    // an exception. Call IsCatalogSupported if you need to know in advance
    // whether you can safely call ReadDatabaseScheme or not.
    function ReadDatabaseScheme(
      const AWarningEventHandler: TInstantWarningEvent): TInstantScheme;
        virtual;
    // Returns True if the broker supports creating a valid catalog. If this
    // method returns False, it means that calling CreateCatalog will yield nil,
    // and calling ReadDatabaseSchema will raise an exception.
    function IsCatalogSupported: Boolean;
    function RetrieveObject(AObject: TInstantObject; const AObjectId: string;
      ConflictAction: TInstantConflictAction;
      const AObjectData: TInstantAbstractObjectData = nil): Boolean;
    procedure SetObjectUpdateCount(AObject: TInstantObject; Value: Integer);
    function StoreObject(AObject: TInstantObject;
      ConflictAction: TInstantConflictAction): Boolean;
    property Connector: TInstantConnector read GetConnector;
    property DatabaseName: string read GetDatabaseName;
  end;

  TInstantConnector = class(TComponent)
  private
    FBroker: TInstantBroker;
    FClientList: TList;
    FObjectStores: TInstantObjectStores;
    FTransactionLevel: Integer;
    FTransactedObjectList: TList;
    FUseTransactions: Boolean;
    FBeforeBuildDatabase: TInstantSchemeEvent;
    FBlobStreamFormat: TInstantStreamFormat;
    FOnGenerateId: TInstantGenerateIdEvent;
    FIdSize: Integer;
    FIdDataType: TInstantDataType;
    FUseUnicode: Boolean;
    FDefaultStatementCacheCapacity: integer;
    procedure AbandonObjects;
    procedure ApplyTransactedObjectStates;
    procedure ClearTransactedObjects;
    procedure DoBeforeBuildDatabase(Scheme: TInstantScheme);
    function GetBroker: TInstantBroker;
    function GetClient(Index: Integer): TObject;
    function GetClientCount: Integer;
    function GetClientList: TList;
    function GetInTransaction: Boolean;
    function GetInUse: Boolean;
    function GetIsDefault: Boolean;
    function GetObjectCount: Integer;
    function GetObjects(Index: Integer): TInstantObject;
    function GetObjectStores: TInstantObjectStores;
    function GetTransactedObjectCount: Integer;
    function GetTransactedObjectList: TList;
    function GetTransactedObjects(Index: Integer): TInstantObject;
    procedure RestoreTransactedObjectStates;
    procedure SetConnected(Value: Boolean);
    procedure SetIsDefault(const Value: Boolean);
    function GetUseUnicode: Boolean;
    procedure SetUseUnicode(const Value: Boolean);
    property TransactedObjectList: TList read GetTransactedObjectList;
  protected
    function AddTransactedObject(AObject: TInstantObject): Integer;
    function CreateBroker: TInstantBroker; virtual; abstract;
    function GetConnected: Boolean; virtual;
    function GetDatabaseExists: Boolean; virtual;
    function GetDatabaseName: string; virtual;
    function GetDDLTransactionSupported: Boolean; virtual;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); virtual;
    procedure InternalConnect; virtual; abstract;
    procedure InternalCommitTransaction; virtual;
    procedure InternalCreateDatabase; virtual;
    function InternalCreateQuery: TInstantQuery; virtual;
    function InternalCreateScheme(Model: TInstantModel): TInstantScheme; virtual; abstract;
    procedure InternalDisconnect; virtual; abstract;
    function InternalGenerateId(const AObject: TInstantObject = nil): string; virtual;
    procedure InternalRollbackTransaction; virtual;
    procedure InternalStartTransaction; virtual;
    function RemoveTransactedObject(AObject: TInstantObject): Integer;
    property ClientList: TList read GetClientList;
    property TransactedObjectCount: Integer read GetTransactedObjectCount;
    property TransactedObjects[Index: Integer]: TInstantObject
      read GetTransactedObjects;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildDatabase(Model: TInstantModel = nil); overload;
    procedure BuildDatabase(AClasses: array of TInstantObjectClass); overload;
    procedure CommitTransaction;
    procedure Connect;
    class function ConnectionDefClass: TInstantConnectionDefClass; virtual; abstract;
    procedure CreateDatabase;
    function CreateScheme(Model: TInstantModel = nil): TInstantScheme;
    function CreateQuery: TInstantQuery;
    procedure Disconnect;
    function EnsureObjectStore(AClass: TInstantObjectClass):
      TInstantObjectStore;
    function GenerateId(const AObject: TInstantObject = nil): string;
    class procedure RegisterClass;
    procedure RegisterClient(Client: TObject);
    procedure RollbackTransaction;
    procedure StartTransaction;
    class procedure UnregisterClass;
    procedure UnregisterClient(Client: TObject);
    property Broker: TInstantBroker read GetBroker;
    property ClientCount: Integer read GetClientCount;
    property Clients[Index: Integer]: TObject read GetClient;
    property DatabaseExists: Boolean read GetDatabaseExists;
    property DatabaseName: string read GetDatabaseName;
    property DDLTransactionSupported: Boolean read GetDDLTransactionSupported;
    property InTransaction: Boolean read GetInTransaction;
    property InUse: Boolean read GetInUse;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[Index: Integer]: TInstantObject read GetObjects;
    property ObjectStores: TInstantObjectStores read GetObjectStores;
  published
    property Connected: Boolean read GetConnected write SetConnected stored False;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault default False;
    property UseTransactions: Boolean read FUseTransactions write FUseTransactions default True;
    property UseUnicode: Boolean read GetUseUnicode write SetUseUnicode default False;
    property BeforeBuildDatabase: TInstantSchemeEvent read FBeforeBuildDatabase write FBeforeBuildDatabase;
    property BlobStreamFormat: TInstantStreamFormat read FBlobStreamFormat write FBlobStreamFormat default sfBinary;
    property OnGenerateId: TInstantGenerateIdEvent read FOnGenerateId write FOnGenerateId;
    property IdDataType: TInstantDataType read FIdDataType write FIdDataType default dtString;
    property IdSize: Integer read FIdSize write FIdSize default InstantDefaultFieldSize;
    property DefaultStatementCacheCapacity: Integer read FDefaultStatementCacheCapacity
      write FDefaultStatementCacheCapacity default 0;
  end;

  TInstantConnectionDef = class(TInstantCollectionItem)
  private
    FIsBuilt: Boolean;
    FBlobStreamFormat: TInstantStreamFormat;
    FIdSize: Integer;
    FIdDataType: TInstantDataType;
    FUseUnicode: Boolean;
    FDefaultStatementCacheCapacity: integer;
  protected
    function GetCaption: string; virtual;
    procedure InitConnector(Connector: TInstantConnector); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure TestConnection;
    class function ConnectionTypeName: string; virtual; abstract;
    class function ConnectorClass: TInstantConnectorClass; virtual; abstract;
    function CreateConnector(AOwner: TComponent): TInstantConnector;
    function Edit: Boolean; virtual; abstract;
    property Caption: string read GetCaption;
  published
    property IsBuilt: Boolean read FIsBuilt write FIsBuilt;
    property BlobStreamFormat: TInstantStreamFormat read FBlobStreamFormat
      write FBlobStreamFormat default sfBinary;
    property IdDataType: TInstantDataType read FIdDataType write FIdDataType
      default dtString;
    property IdSize: Integer read FIdSize write FIdSize
      default InstantDefaultFieldSize;
    property UseUnicode: Boolean read FUseUnicode write FUseUnicode
      default False;
    property DefaultStatementCacheCapacity: Integer read FDefaultStatementCacheCapacity
      write FDefaultStatementCacheCapacity default 0;
  end;

  TInstantConnectionDefs = class(TInstantCollection)
  private
    function GetItems(Index: Integer): TInstantConnectionDef;
    procedure SetItems(Index: Integer; const Value: TInstantConnectionDef);
  public
    constructor Create;
    property Items[Index: Integer]: TInstantConnectionDef read GetItems
      write SetItems; default;
  end;

  {$IFDEF D17+}
  TInstantObjectListEnumerator = record
  private
    FIndex: Integer;
    FList: TObjectList<TInstantObject>;
  public
    constructor Create(AList: TObjectList<TInstantObject>);
    function GetCurrent: TInstantObject;
    function MoveNext: Boolean;
    property Current: TInstantObject read GetCurrent;
  end;

  TInstantObjectList<T : TInstantObject> = class(TObjectList<TInstantObject>)
  public
    function GetEnumerator: TInstantObjectListEnumerator;
  end;
  {$ENDIF}

procedure AssignInstantStreamFormat(Strings: TStrings);
function InstantAttributeTypeToDataType(AttributeType: TInstantAttributeType;
  BlobStreamFormat: TInstantStreamFormat = sfBinary): TInstantDataType;
function InstantConnectorClasses: TList;
procedure InstantCheckConnection(Connection: TCustomConnection);
function InstantCheckConnector(var Connector: TInstantConnector): TInstantConnector;
function InstantClassNameToName(const ClassName: string): string;
function InstantCreateStorageMaps(Metadatas: TInstantClassMetadatas = nil): TInstantAttributeMaps;
{$IFNDEF MARS_FIREDAC}
function InstantDefaultConnector: TInstantConnector;
{$ENDIF}
procedure InstantDisableNotifiers;
procedure InstantEnableNotifiers;
function InstantFindAttribute(const Path: string; AObject: TInstantObject; RaiseExceptions: Boolean = True): TInstantAttribute;
function InstantFindClass(const ClassName: string): TInstantObjectClass;
function InstantFindClassMetadata(const ClassName: string): TInstantClassMetadata;
function InstantGetClass(const ClassName: string): TInstantObjectClass;
procedure InstantGetClasses(Classes: TList; BaseClass: TInstantObjectClass = nil);
function InstantGetClassMetadata(const ClassName: string): TInstantClassMetadata;
function InstantIsValidObjectId(const AObjectId: string): Boolean;
function InstantModel: TInstantModel;
function InstantNameToClassName(const Name: string): string;
procedure InstantRefreshObjects(Classes: array of TInstantObjectClass;
  Progress: TInstantProgressEvent = nil; Connector: TInstantConnector = nil);
procedure InstantRegisterClass(AClass: TInstantObjectClass);
procedure InstantRegisterClasses(AClasses: array of TInstantObjectClass);
procedure InstantUnregisterClass(AClass: TInstantObjectClass);
procedure InstantUnregisterClasses(AClasses: array of TInstantObjectClass);

function InstantResolveGraphicFileType(AStream: TStream ): TInstantGraphicFileFormat;
function InstantGetAttributeValue(const Attribute: TInstantAttribute): Variant;
{$IFNDEF IO_CONSOLE}
procedure InstantRegisterGraphicClass(InstantGraphicFileFormat : TInstantGraphicFileFormat;
  AGraphicClass: TGraphicClass);
function InstantGraphicFileFormatToGraphicClass(InstantGraphicFileFormat : TInstantGraphicFileFormat) : TGraphicClass;
{$ENDIF}

// Converts a TInstantDataType to a type that can be used to create TField and
// TParam instances. Raises an exception for unsupported datatypes (at the time
// of writing, all datatypes are supported).
function InstantDataTypeToFieldType(const InstantDataType: TInstantDataType;
  UseUnicode: Boolean): TFieldType;

const
  InstantDataTypeStrings: array[TInstantDataType] of string =
    ('Integer', 'Float', 'Currency', 'Boolean', 'String', 'Memo', 'DateTime',
     'Blob', 'Date', 'Time', 'Integer');

procedure AssignInstantDataTypeStrings(Strings: TStrings);

const
  InstantClassPrefix: string = 'T';
  InstantAttributePrefix: string = '_';

implementation

uses
  Windows,
  TypInfo,
{$IFDEF D6+}
  MaskUtils,
  Variants,
  DateUtils,
{$ELSE}
  Mask,
{$ENDIF}
{$IFDEF D14+}
  RTTI, InstantRttiAttributes,
{$ENDIF}
{$IFDEF D17+}
  System.Types,
{$ENDIF}
  InstantUtils, InstantDesignHook, InstantCode;

var
  ConnectorClasses: TList;
  ClassList: TList;
{$IFNDEF IO_CONSOLE}
  GraphicClassList: array[TInstantGraphicFileFormat] OF TGraphicClass;
{$ENDIF}
  RuntimeModel: TInstantModel;
  ObjectNotifiers: TInstantObjectNotifiers;
  DefaultConnector: TInstantConnector;

{ Global routines }

procedure AssignInstantStreamFormat(Strings: TStrings);
var
  i: TInstantStreamFormat;
begin
  if Assigned(Strings) then
  begin
    Strings.Clear;
    for i := Low(TInstantStreamFormat) to High(TInstantStreamFormat) do
    begin
      Strings.Add(AInstantStreamFormatStr[i]);
    end;
  end;
end;

procedure AssignInstantDataTypeStrings(Strings: TStrings);
var
  i: TInstantDataType;
begin
  if Assigned(Strings) then
  begin
    Strings.Clear;
    for i := Low(TInstantDataType) to High(TInstantDataType) do
    begin
      Strings.Add(InstantDataTypeStrings[i]);
    end;
  end;
end;

function InstantAttributeTypeToDataType(AttributeType: TInstantAttributeType;
  BlobStreamFormat: TInstantStreamFormat = sfBinary): TInstantDataType;
const
  DataTypesBinary: array[TInstantAttributeType] of TInstantDataType = (
    dtString,     //atUnknown
    dtInteger,    //atInteger
    dtFloat,      //atFloat
    dtCurrency,   //atCurrency
    dtBoolean,    //atBoolean
    dtString,     //atString
    dtDateTime,   //atDateTime
    dtBlob,       //atBlob
    dtMemo,       //atMemo
    dtBlob,       //atGraphic
    dtBlob,       //atPart
    dtString,     //atReference
    dtBlob,       //atParts
    dtBlob,       //atReferences
    dtDate,       //atDate
    dtTime,       //atTime
    dtEnum);      //atEnum

  DataTypesXML: array[TInstantAttributeType] of TInstantDataType = (
    dtString,     //atUnknown
    dtInteger,    //atInteger
    dtFloat,      //atFloat
    dtCurrency,   //atCurrency
    dtBoolean,    //atBoolean
    dtString,     //atString
    dtDateTime,   //atDateTime
    dtBlob,       //atBlob
    dtMemo,       //atMemo
    dtBlob,       //atGraphic
    dtMemo,       //atPart
    dtString,     //atReference
    dtMemo,       //atParts
    dtMemo,       //atReferences
    dtDate,       //atDate
    dtTime,       //atTime
    dtEnum);      //atEnum
begin
  if BlobStreamFormat = sfBinary then
    Result := DataTypesBinary[AttributeType]
  else
    Result := DataTypesXML[AttributeType];
end;

function InstantDataTypeToFieldType(const InstantDataType: TInstantDataType;
  UseUnicode: Boolean): TFieldType;
begin
  case InstantDataType of
    dtInteger: Result := ftInteger;
    dtFloat: Result := ftFloat;
    dtCurrency: Result := ftCurrency;
    dtBoolean: Result := ftBoolean;
    dtString:
    begin
      if UseUnicode then
        Result := ftWideString
      else
        Result := ftString;
    end;
    dtMemo:
    begin
      if UseUnicode then
        Result := ftWideMemo
      else
        Result := ftMemo;
    end;
    dtDateTime: Result := ftDateTime;
    dtDate: Result := ftDate;
    dtTime: Result := ftTime;
    dtBlob: Result := ftBlob;
  else
    raise EInstantError.CreateFmt(SUnsupportedDataType,
      [GetEnumName(TypeInfo(TInstantDataType), Ord(InstantDataType))]);
  end;
end;

function InstantConnectorClasses: TList;
begin
  Result := ConnectorClasses;
end;

procedure InstantCheckConnection(Connection: TCustomConnection);
begin
  if not Assigned(Connection) then
    raise EInstantError.Create(SUnassignedConnection);
end;

function InstantCheckConnector(
  var Connector: TInstantConnector): TInstantConnector;
begin
  if Assigned(Connector) then
  begin
    Result := Connector;
    Exit;
  end;
  Connector := DefaultConnector;
  if not Assigned(Connector) then
    raise EInstantError.Create(SUnassignedConnector);
  Result := Connector;
end;

function InstantClassNameToName(const ClassName: string): string;
var
  PrefixLen: Integer;
begin
  PrefixLen := Length(InstantClassPrefix);
  if SameText(Copy(ClassName, 1, PrefixLen), InstantClassPrefix) then
    Result := Copy(ClassName, PrefixLen + 1, Length(ClassName) - PrefixLen)
  else
    Result := ClassName;
end;

function InstantCreateStorageMaps(
  Metadatas: TInstantClassMetadatas): TInstantAttributeMaps;
var
  I, J, K: Integer;
  SourceMap, TargetMap: TInstantAttributeMap;
begin
  if not Assigned(Metadatas) then
    Metadatas := InstantModel.ClassMetadatas;
  if not Assigned(Metadatas) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TInstantAttributeMaps.Create(nil);
  try
    for I := 0 to Pred(Metadatas.Count) do
    begin
      with Metadatas[I] do
        if IsStored then
          for J := 0 to Pred(StorageMaps.Count) do
          begin
            SourceMap := StorageMaps[J];
            TargetMap := Result.EnsureMap(SourceMap.Name);
            for K := 0 to Pred(SourceMap.Count) do
              TargetMap.AddUnique(SourceMap[K]);
          end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{$IFNDEF MARS_FIREDAC}
function InstantDefaultConnector: TInstantConnector;
begin
  Result := DefaultConnector;
end;
{$ENDIF}

procedure InstantDisableNotifiers;
begin
  ObjectNotifiers.Disable;
end;

procedure InstantEnableNotifiers;
begin
  ObjectNotifiers.Enable;
end;

function InstantFindAttribute(const Path: string; AObject: TInstantObject;
  RaiseExceptions: Boolean = True): TInstantAttribute;
var
  I: Integer;
  AttribName: string;
begin
  Result := nil;
  if (Path = '') or not Assigned(AObject) then
    Exit;
  I := 1;
  AttribName := InstantPartStr(Path, I, InstantDot);
  while (AttribName <> '') and Assigned(AObject) do
  begin
    if RaiseExceptions then
      Result := AObject.AttributeByName(AttribName)
    else
      Result := AObject.FindAttribute(AttribName);
    if not Assigned(Result) then
      Exit;
    Inc(I);
    AttribName := InstantPartStr(Path, I, InstantDot);
    if (AttribName <> '') and (Result is TInstantElement) then
      AObject := TInstantElement(Result).Value;
  end;
end;

function InstantFindClass(const ClassName: string): TInstantObjectClass;
begin
  Result := InstantGetClass(Classname);
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SClassNotRegistered, [ClassName]);
end;

function InstantFindClassMetadata(
  const ClassName: string): TInstantClassMetadata;
begin
  Result := InstantGetClassMetadata(ClassName);
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SClassNotRegistered, [ClassName]);
end;

function InstantGetClass(const ClassName: string): TInstantObjectClass;
var
  ObjectClass: TClass;
begin
  ObjectClass := GetClass(ClassName);
  if Assigned(ObjectClass) and ObjectClass.InheritsFrom(TInstantObject) then
    Result := TInstantObjectClass(ObjectClass)
  else
    Result := nil;
end;

procedure InstantGetClasses(Classes: TList; BaseClass: TInstantObjectClass);
var
  I: Integer;
begin
  if not Assigned(Classes) then
    Exit;
  if not Assigned(BaseClass) then
    BaseClass := TInstantObject;
  for I := 0 to Pred(ClassList.Count) do
    if TInstantObjectClass(ClassList[I]).InheritsFrom(BaseClass) then
      Classes.Add(ClassList[I]);
end;

function InstantGetClassMetadata(
  const ClassName: string): TInstantClassMetadata;
var
  CodeClass: TInstantCodeClass;
begin
  if Assigned(DesignModel) then
  begin
    CodeClass := DesignModel^.FindClass(ClassName);
    if Assigned(CodeClass) then
      Result := CodeClass.MetadataInfo.Metadata
    else
      Result := nil;
  end else
    Result := InstantModel.ClassMetadatas.Find(ClassName);
end;

function InstantIsValidObjectId(const AObjectId: string): Boolean;
begin
  Result := Trim(AObjectId) <> '';
end;

function InstantModel: TInstantModel;
begin
  if not Assigned(RuntimeModel) then
    RuntimeModel := TInstantModel.Create;
  Result := RuntimeModel;
end;

function InstantNameToClassName(const Name: string): string;
begin
  Result := InstantClassPrefix + Name;
end;

procedure InstantRefreshObjects(Classes: array of TInstantObjectClass;
  Progress: TInstantProgressEvent; Connector: TInstantConnector);
begin
  with TInstantProgressLoop.Create(Low(Classes), High(Classes), Progress) do
  try
    while not Done do
    begin
      Classes[Step].RefreshAll(Connector, Progress);
      Next;
    end;
  finally
    Free;
  end;
end;

procedure InstantRegisterClass(AClass: TInstantObjectClass);
begin
  Classes.RegisterClass(AClass);
  if not Assigned(ClassList) then
    ClassList := TList.Create;
  if ClassList.IndexOf(AClass) = -1 then
    ClassList.Add(AClass);
end;

procedure InstantRegisterClasses(AClasses: array of TInstantObjectClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    InstantRegisterClass(AClasses[I]);
end;

procedure InstantUnregisterClass(AClass: TInstantObjectClass);
begin
  Classes.UnregisterClass(AClass);
  ClassList.Remove(AClass);
end;

procedure InstantUnregisterClasses(AClasses: array of TInstantObjectClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    InstantUnRegisterClass(AClasses[I]);
end;

function InstantResolveGraphicFileType(AStream: TStream): TInstantGraphicFileFormat;
const
  SizeOfGraphicHeader = 8;
  MinimumBytesToRead = 10;
var
  P: array [0..MinimumBytesToRead - 1] of Byte;
  StreamLength: Longint;
  BytesRetrieved: Integer;
begin
  Result := gffUnknown;
  if not Assigned(AStream) then
    Exit;
  StreamLength := AStream.Size;
  AStream.Position := 0;
  FillChar(P, SizeOf(P), 0);
  BytesRetrieved := AStream.Read(P[0], SizeOf(P));
  AStream.Position := 0;
  if BytesRetrieved < MinimumBytesToRead then
    Exit;
  // bitmap format
  if (P[0] = 66) and (P[1] = 77) then
    Result := gffBmp
  // tiff format
  else if ((P[0] = 73) and (P[1] = 73) and (P[2] = 42) and (P[3] = 0))
   or ((P[0] = 77) and (P[1] = 77) and (P[2] = 42) and (P[3] = 0)) then
    Result := gffTiff
  // jpg format
  else if (P[6] = 74) and (P[7] = 70) and (P[8] = 73) and (P[9] = 70)
   or (P[6] = 69) and (P[7] = 120) and (P[8] = 105) and (P[9] = 102) then
    Result := gffJpeg
  // png format
  else if (P[0] = 137 ) and (P[1] = 80) and (P[2] = 78) and (P[3] = 71)
   and (P[4] = 13) and (P[5] = 10) and (P[6] = 26) and (P[7] = 10) then
    Result := gffPng
  // dcx format
  else if (P[0] = 177) and (P[1] = 104) and (P[2] = 222) and (P[3] = 58) then
    Result := gffDcx
  // pcx format
  else if p[0] = 10 then
    Result := gffPcx
  // emf format
  else if ((P[0] = 215) and (P[1] = 205) and (P[2] = 198) and (P[3] = 154))
   or ((P[0] = 1) and (P[1] = 0) and (P[2] = 0) and (P[3] = 0)) then
    Result := gffEmf
  // gif format
  else if (P[0] = $47) and (P[1] = $49) and (P[2] = $46) then
    Result := gffGif
  // Ico format
  else if (P[0] = 00) and (P[1] = 00) and (P[2] = 01) and (P[3] = 00) then
    Result := gffIco
  // bitmap format with TGraphicHeader header
  else if (P[0] = 01) and (P[1] = 00) and (P[2] = 00) and (P[3] = 01)
   and (PLongint(@p[4])^ = StreamLength - SizeOfGraphicHeader) then
  begin
    Result := gffBmp;
    AStream.Position := SizeOfGraphicHeader;
  end;
end;

{$IFNDEF IO_CONSOLE}
procedure InstantRegisterGraphicClass(InstantGraphicFileFormat : TInstantGraphicFileFormat;
  AGraphicClass: TGraphicClass);
begin
  GraphicClassList[InstantGraphicFileFormat] := AGraphicClass;
end;

function InstantGraphicFileFormatToGraphicClass(InstantGraphicFileFormat : TInstantGraphicFileFormat) : TGraphicClass;
begin
  Result := GraphicClassList[InstantGraphicFileFormat];
end;
{$ENDIF}

function InstantGetAttributeValue(const Attribute: TInstantAttribute): Variant;
begin
  if Attribute is TInstantString then
    Result := TInstantString(Attribute).Value
  else if Attribute is TInstantInteger then
    Result := TInstantInteger(Attribute).Value
  else if Attribute is TInstantFloat then
    Result := TInstantFloat(Attribute).Value
  else if Attribute is TInstantCurrency then
    Result := TInstantCurrency(Attribute).Value
  else if Attribute is TInstantBoolean then
    Result := TInstantBoolean(Attribute).Value
  else if Attribute is TInstantCustomDateTime then
    Result := TInstantCustomDateTime(Attribute).Value
  else
    Result := Attribute.Value;
end;

{ TInstantObjectReference }

procedure TInstantObjectReference.Assign(Source: TPersistent);
var
  LSource: TInstantObjectReference;
begin
  inherited;
  if Source is TInstantObjectReference then
  begin
    LSource := TInstantObjectReference(Source);
    FObjectClassName := LSource.FObjectClassName;
    FObjectId := LSource.FObjectId;
    Instance := LSource.Instance;
    FObjectData := LSource.FObjectData;
  end;
end;

procedure TInstantObjectReference.AssignInstance(AInstance: TInstantObject);
begin
  DoAssignInstance(AInstance, OwnsInstance);
end;

procedure TInstantObjectReference.ClearReference;
begin
  FObjectClassName := '';
  FObjectId := '';
  FreeAndNil(FObjectData);
end;

constructor TInstantObjectReference.Clone(Source: TInstantObjectReference;
  AOwnsInstance: Boolean; AOwner: TInstantComplex);
begin
  Create;
  FOwner := AOwner;
  Assign(Source);
  OwnsInstance := AOwnsInstance;
end;

constructor TInstantObjectReference.Create(AInstance: TInstantObject;
  AOwnsInstance: Boolean; AOwner: TInstantComplex);
begin
  inherited Create;
  FOwner := AOwner;
  DoAssignInstance(AInstance, AOwnsInstance);
end;

class function TInstantObjectReference.CreateInstance(
  Arg: Pointer): TInstantStreamable;
begin
  Result := Create(Arg);
end;

function TInstantObjectReference.Dereference(Connector: TInstantConnector;
  AOwnsInstance: Boolean; Retry: Boolean): TInstantObject;
var
  Obj: TInstantObject;
begin
  if not Assigned(Instance) or (IsBroken and Retry) then
  begin
    InstantCheckConnector(Connector);
    if HasReference then
    begin
      Obj := ObjectClass.Retrieve(ObjectId, False, False, Connector, FObjectData);
      DoAssignInstance(Obj, AOwnsInstance);
    end;
    if Assigned(FInstance) then
      FInstance.Release
    else
      {$IFDEF WIN64}
      Int64(FInstance) := -1;
      {$ELSE}
      Integer(FInstance) := -1;
      {$ENDIF}
    end;
  Result := Instance;
end;

destructor TInstantObjectReference.Destroy;
begin
  FreeAndNil(FObjectData);
  DestroyInstance;
  inherited;
end;

function TInstantObjectReference.Equals(const AObjectClassName,
  AObjectId: string): Boolean;
begin
  Result := SameText(AObjectClassName, FObjectClassName) and
    (AObjectId = FObjectId);
end;

procedure TInstantObjectReference.DestroyInstance;
begin
  if HasInstance and FOwnsInstance then
  begin
    if Assigned(FOwner) then
      FInstance.RefByList.Remove(FOwner);
    FInstance.Free;
  end;
  FInstance := nil;
end;

procedure TInstantObjectReference.DoAssignInstance(AInstance: TInstantObject;
  AOwnsInstance: Boolean);
begin
  if FInstance <> AInstance then
  begin
    DestroyInstance;
    FInstance := AInstance;
    FOwnsInstance := False;
  end;
  OwnsInstance := AOwnsInstance;
end;

function TInstantObjectReference.Equals(AObject: TInstantObject): Boolean;
begin
  if Assigned(AObject) then
    if Assigned(Instance) then
      Result := Instance = AObject
    else
      Result := AObject.EqualsPersistentSignature(ObjectClassName, ObjectId)
  else
    Result := not HasReference;
end;

function TInstantObjectReference.GetInstance: TInstantObject;
begin
  {$IFDEF WIN64}
  if (Int64(FInstance) = -1) or (Int64(FInstance) = 0) then
  {$ELSE}
  if (Integer(FInstance) = -1) or (Integer(FInstance) = 0) then
  {$ENDIF}
    Result := nil
  else
    Result := FInstance;
end;

function TInstantObjectReference.GetObjectClass: TInstantObjectClass;
begin
  Result := InstantFindClass(ObjectClassName);
end;

function TInstantObjectReference.GetObjectClassName: string;
begin
  if Assigned(Instance) then
    Result := Instance.ClassName
  else
    Result := FObjectClassName;
end;

function TInstantObjectReference.GetObjectId: string;
begin
  if Assigned(Instance) then
    Result := Instance.Id
  else
    Result := FObjectId;
end;

function TInstantObjectReference.GetSignature: string;
begin
  if HasReference then
    Result := Format('%s.%s', [ObjectClassName, ObjectId])
  else
    Result := '';
end;

function TInstantObjectReference.HasInstance: Boolean;
begin
  Result := Assigned(Instance);
end;

function TInstantObjectReference.HasReference: Boolean;
begin
  Result := ObjectClassName <> '';
end;

function TInstantObjectReference.IsBroken: Boolean;
begin
  Result := Integer(FInstance) = -1;
end;

procedure TInstantObjectReference.ReadAsObject(Reader: TInstantReader);
var
  AObjectClassName, AObjectId: string;
begin
  Reset;
  with Reader do
  begin
    AObjectClassName := ReadStr;
    AObjectId := ReadStr;
    ReferenceObject(AObjectClassName, AObjectId);
    ReadListEnd;
    ReadListEnd;
  end;
end;

procedure TInstantObjectReference.ReadObject(Reader: TInstantReader);
begin
  inherited;
  DestroyInstance;
  Reader.ReadStr;
  Reader.ReadStr;
  FObjectClassName := Reader.ReadStr;
  FObjectId := Reader.ReadStr;
  FObjectData := nil;
end;

procedure TInstantObjectReference.ReferenceObject(const AObjectClassName,
  AObjectId: string; const AObjectData: TInstantAbstractObjectData = nil);
begin
  if not Equals(AObjectClassName, AObjectId) then
  begin
    FreeAndNil(FObjectData);
    DestroyInstance;
    FObjectClassName := AObjectClassName;
    FObjectId := AObjectId;
    FObjectData := AObjectData;
  end;
end;

procedure TInstantObjectReference.ReferenceObject(
  const AObjectClass: TInstantObjectClass; const AObjectId: string;
  const AObjectData: TInstantAbstractObjectData = nil);
begin
  ReferenceObject(AObjectClass.ClassName, AObjectId, AObjectData);
end;

procedure TInstantObjectReference.RetrieveObjectFromObjectData(
  const AConnector: TInstantConnector);
var
  LObject: TInstantObject;
begin
  Assert(Assigned(FObjectData));

  LObject := ObjectClass.Retrieve(ObjectId, False, False, AConnector, FObjectData);
  DoAssignInstance(LObject, True);
  if Assigned(FInstance) then
    FInstance.Release
  else
    {$IFDEF WIN64}
    Int64(FInstance) := -1;
    {$ELSE}
    Integer(FInstance) := -1;
    {$ENDIF}
end;

procedure TInstantObjectReference.Reset;
begin
  DestroyInstance;
  ClearReference;
end;

procedure TInstantObjectReference.SetInstance(Value: TInstantObject);
begin
  if not Equals(Value) then
    ClearReference;
  DoAssignInstance(Value, OwnsInstance);
end;

procedure TInstantObjectReference.SetOwnsInstance(const Value: Boolean);
begin
  if Assigned(Instance) then
    if Value and not OwnsInstance then
    begin
      FInstance.AddRef;
      if Assigned(FOwner) then
        FInstance.RefByList.Add(FOwner);
    end
    else if not Value and OwnsInstance then
    begin
      FInstance.Release;
      if Assigned(FOwner) then
        FInstance.RefByList.Remove(FOwner);
    end;
  FOwnsInstance := Value;
end;

procedure TInstantObjectReference.WriteAsObject(Writer: TInstantWriter);
begin
  with Writer do
  begin
    WriteUTF8Str(ObjectClassName);
    WriteUTF8Str(ObjectId);
    WriteListEnd;
    WriteListEnd;
  end;
end;

procedure TInstantObjectReference.WriteObject(Writer: TInstantWriter);
begin
  inherited;
  Writer.WriteStr('');
  Writer.WriteStr('');
  Writer.WriteUTF8Str(ObjectClassName);
  Writer.WriteUTF8Str(ObjectId);
end;

{ TInstantAttribute }

function TInstantAttribute.AccessError(
  const TypeName: string): EInstantAccessError;
begin
  Result := EInstantAccessError.CreateFmt(SAccessError,
    [ClassName, Name, TypeName]);
end;

procedure TInstantAttribute.Changed;
begin
  IsChanged := True;
end;

procedure TInstantAttribute.CheckHasMetadata;
begin
  if not Assigned(Metadata) then
    raise EInstantError.CreateFmt(SUnassignedAttributeMetadata,
      [ClassName, Name]);
end;

function TInstantAttribute.ConversionError(
  E: Exception): EInstantConversionError;
begin
  Result := EInstantConversionError.CreateFmt(SAttributeConversionError,
    [ClassName, Name, E.Message], E);
end;

constructor TInstantAttribute.Create(AOwner: TInstantAbstractObject = nil;
  AMetadata: TInstantCollectionItem = nil);
begin
  Assert((AOwner = nil) or (AOwner is TInstantObject));
  Assert((AMetadata = nil) or (AMetadata is TInstantAttributeMetadata));
  inherited Create(AOwner, AMetadata);
end;

function TInstantAttribute.GetAsBoolean: Boolean;
begin
  raise AccessError('Boolean');
end;

function TInstantAttribute.GetAsCurrency: Currency;
begin
  Result := AsFloat;
end;

function TInstantAttribute.GetAsDateTime: TDateTime;
begin
  raise AccessError('DateTime');
end;

function TInstantAttribute.GetAsDate: TDateTime;
begin
  raise AccessError('Date');
end;

function TInstantAttribute.GetAsTime: TDateTime;
begin
  raise AccessError('Time');
end;

function TInstantAttribute.GetAsFloat: Double;
begin
  raise AccessError('Float');
end;

function TInstantAttribute.GetAsInteger: Integer;
begin
  raise AccessError('Integer');
end;

function TInstantAttribute.GetAsObject: TInstantObject;
begin
  raise AccessError('Object');
end;

function TInstantAttribute.GetAsString: string;
begin
  raise AccessError('string');
end;

function TInstantAttribute.GetAsVariant: Variant;
begin
  raise AccessError('Variant');
end;

function TInstantAttribute.GetDisplayText: string;
begin
  if Assigned(Metadata) and (Metadata.EditMask <> '') then
    Result := FormatMaskText(Metadata.EditMask, AsString)
  else
    Result := AsString;
end;

function TInstantAttribute.GetIsChanged: Boolean;
begin
  Result := FIsChanged;
end;

function TInstantAttribute.GetIsDefault: Boolean;
var
  DefaultStr, ValueStr: string;
  L: Integer;
begin
  L := Length(AsString);
  if Assigned(Metadata) then
  begin
    Result := Length(Metadata.DefaultValue) = L;
    if Result then
    begin
      DefaultStr := Metadata.DefaultValue;
      ValueStr := AsString;
      // Fixed Range Check Error when one or both strings are ''.
      //Result := CompareMem(@DefaultStr[1], @ValueStr[1], L);
      Result := CompareStr(DefaultStr, ValueStr) = 0;
    end;
  end
  else
    Result := L = 0;
end;

function TInstantAttribute.GetIsIndexed: Boolean;
begin
  Result := Assigned(Metadata) and Metadata.IsIndexed;
end;

function TInstantAttribute.GetIsMandatory: Boolean;
begin
  Result := IsRequired or IsIndexed;
end;

function TInstantAttribute.GetIsNull: Boolean;
begin
  Result := False;
  if Assigned(Metadata) then
    Result := Metadata.UseNull and (AsString = '');
end;

function TInstantAttribute.GetIsRequired: Boolean;
begin
  Result := Assigned(Metadata) and Metadata.IsRequired;
end;

function TInstantAttribute.GetIsUnique: Boolean;
begin
  Result := Assigned(Metadata) and Metadata.IsUnique;
end;

function TInstantAttribute.GetMetadata: TInstantAttributeMetadata;
begin
  Result := inherited GetMetadata as TInstantAttributeMetadata;
end;

function TInstantAttribute.GetName: string;
begin
  if Assigned(Metadata) then
    Result := Metadata.Name
  else
    Result := '';
end;

function TInstantAttribute.GetOwner: TInstantObject;
begin
  Result := inherited GetOwner as TInstantObject;
end;

function TInstantAttribute.GetValue: Variant;
begin
  Result := AsVariant;
end;

procedure TInstantAttribute.Initialize;

{$IFDEF D14+}
  procedure InitializeRttiAttributes;

    procedure InvokeRttiAttribute(RttiMember: TRttiMember);
    var
      CustomAttribute: TCustomAttribute;
    begin
      for CustomAttribute in RttiMember.GetAttributes do
        if CustomAttribute is TInstantRttiAttribute then
          TInstantRttiAttribute(CustomAttribute).Change(Self.GetOwner, RttiMember);
    end;

  var
    RttiContext: TRttiContext;
    RttiType: TRttiType;
    RttiField: TRttiField;
    RttiMethod: TRttiMethod;
    RttiProperty: TRttiProperty;
  begin
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(Self.ClassType);

      for RttiField in RttiType.GetFields do
        InvokeRttiAttribute(RttiField);

      for RttiMethod in RttiType.GetMethods do
        InvokeRttiAttribute(RttiMethod);

      for RttiProperty in RttiType.GetProperties do
        InvokeRttiAttribute(RttiProperty);
    finally
      RttiContext.Free
    end;
  end;
{$ENDIF}

begin
{$IFDEF D14+}
  InitializeRttiAttributes;
{$ENDIF}
end;

procedure TInstantAttribute.ReadName(Reader: TInstantReader);
begin
  Reader.ReadStr;
end;

procedure TInstantAttribute.Reset;
begin
end;

procedure TInstantAttribute.SetAsBoolean(AValue: Boolean);
begin
  raise AccessError('Boolean');
end;

procedure TInstantAttribute.SetAsCurrency(AValue: Currency);
begin
  AsFloat := AValue;
end;

procedure TInstantAttribute.SetAsDateTime(AValue: TDateTime);
begin
  raise AccessError('DateTime');
end;

procedure TInstantAttribute.SetAsDate(AValue: TDateTime);
begin
  raise AccessError('Date');
end;

procedure TInstantAttribute.SetAsTime(AValue: TDateTime);
begin
  raise AccessError('Time');
end;

procedure TInstantAttribute.SetAsFloat(AValue: Double);
begin
  raise AccessError('Float');
end;

procedure TInstantAttribute.SetAsInteger(AValue: Integer);
begin
  raise AccessError('Integer');
end;

procedure TInstantAttribute.SetAsObject(AValue: TInstantObject);
begin
  raise AccessError('Object');
end;

procedure TInstantAttribute.SetAsString(const AValue: string);
begin
  raise AccessError('string');
end;

procedure TInstantAttribute.SetAsVariant(AValue: Variant);
begin
  raise AccessError('Variant');
end;

procedure TInstantAttribute.SetIsChanged(Value: Boolean);
begin
  if Assigned(Owner) and Value then
    if Owner.ChangesDisabled then
      Exit
    else
      Owner.DoAttributeChanged(Self);
  FIsChanged := Value;
end;

procedure TInstantAttribute.SetMetadata(Value: TInstantAttributeMetadata);
begin
  if Assigned(Value) and (Value.AttributeType <> AttributeType) then
    raise EInstantError.CreateFmt(SMetadataMismatch, [ClassName, Name]);
  inherited SetMetadata(Value);
end;

procedure TInstantAttribute.SetOwner(AOwner: TInstantObject);
begin
  inherited SetOwner(AOwner);
end;

procedure TInstantAttribute.SetValue(AValue: Variant);
begin
  AsVariant := AValue;
end;

procedure TInstantAttribute.Unchanged;
begin
  IsChanged := False;
end;

procedure TInstantAttribute.Validate(const AValue: string);
begin
  if Assigned(Metadata) then
    Metadata.ValidateAttribute(Self, AValue);
end;

procedure TInstantAttribute.WriteName(Writer: TInstantWriter);
begin
  Writer.WriteUTF8Str(Name);
end;

{ TInstantSimple }

function TInstantSimple.InvalidValueError(AValue: Variant;
  E: Exception): EInstantError;
begin
  Result := EInstantError.CreateFmt(SInvalidAttributeValue,
    [VarToStr(AValue), ClassName, Name], E);
end;

{ TInstantNumeric }

function TInstantNumeric.GetAsBoolean: Boolean;
begin
  Result := AsInteger <> 0;
end;

function TInstantNumeric.GetAsDateTime: TDateTime;
begin
  Result := AsFloat;
end;

function TInstantNumeric.GetDisplayText: string;
begin
  if Assigned(Metadata) and (Metadata.EditMask <> '') then
    Result := FormatFloat(Metadata.EditMask, AsFloat)
  else
    Result := inherited GetDisplayText;
end;

function TInstantNumeric.GetIsDefault: Boolean;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    Result := inherited GetIsDefault
  else
    Result := AsFloat = 0;
end;

function TInstantNumeric.GetIsNull: Boolean;
begin
  if Assigned(Metadata) and (Metadata.UseNull) then
    Result := AsFloat = 0
  else
    Result := inherited GetIsNull;
end;

procedure TInstantNumeric.SetAsBoolean(AValue: Boolean);
begin
  AsInteger := Integer(AValue);
end;

procedure TInstantNumeric.SetAsDateTime(AValue: TDateTime);
begin
  AsFloat := AValue;
end;

{ TInstantInteger }

procedure TInstantInteger.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantInteger then
    Value := TInstantInteger(Source).Value;
end;

class function TInstantInteger.AttributeType: TInstantAttributeType;
begin
  Result := atInteger;
end;

function TInstantInteger.GetAsFloat: Double;
begin
  Result := Value;
end;

function TInstantInteger.GetAsInteger: Integer;
begin
  Result := Value;
end;

function TInstantInteger.GetAsString: string;
begin
  Result := IntToStr(Value);
end;

function TInstantInteger.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantInteger.GetValue: Integer;
begin
  Result := FValue;
end;

procedure TInstantInteger.Initialize;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    try
      FValue := StrToInt(Metadata.DefaultValue);
    except
      on E: Exception do
        raise ConversionError(E);
    end
  else
    FValue := 0;
end;

procedure TInstantInteger.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadInteger;
end;

procedure TInstantInteger.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := 0;
  Changed;
end;

procedure TInstantInteger.SetAsFloat(AValue: Double);
begin
  Value := Round(AValue);
end;

procedure TInstantInteger.SetAsInteger(AValue: Integer);
begin
  Value := AValue;
end;

procedure TInstantInteger.SetAsString(const AValue: string);
begin
  try
    Value := StrToInt(AValue);
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

procedure TInstantInteger.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantInteger.SetValue(AValue: Integer);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantInteger.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteInteger(Value);
end;

{ TInstantFloat }

procedure TInstantFloat.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantFloat then
    Value := TInstantFloat(Source).Value;
end;

class function TInstantFloat.AttributeType: TInstantAttributeType;
begin
  Result := atFloat;
end;

function TInstantFloat.GetAsFloat: Double;
begin
  Result := Value;
end;

function TInstantFloat.GetAsInteger: Integer;
begin
  Result := Round(Value);
end;

function TInstantFloat.GetAsString: string;
begin
  Result := FloatToStr(Value);
end;

function TInstantFloat.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantFloat.GetValue: Double;
begin
  Result := FValue;
end;

procedure TInstantFloat.Initialize;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    try
      FValue := StrToFloat(Metadata.DefaultValue);
    except
      on E: Exception do
        raise ConversionError(E);
    end;
end;

procedure TInstantFloat.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadFloat;
end;

procedure TInstantFloat.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := 0;
  Changed;
end;

procedure TInstantFloat.SetAsFloat(AValue: Double);
begin
  Value := AValue;
end;

procedure TInstantFloat.SetAsInteger(AValue: Integer);
begin
  Value := AValue;
end;

procedure TInstantFloat.SetAsString(const AValue: string);
begin
  try
    Value := StrToFloat(AValue)
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

procedure TInstantFloat.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantFloat.SetValue(AValue: Double);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantFloat.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteFloat(Value);
end;

{ TInstantCurrency }

procedure TInstantCurrency.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCurrency then
    Value := TInstantCurrency(Source).Value;
end;

class function TInstantCurrency.AttributeType: TInstantAttributeType;
begin
  Result := atCurrency;
end;

function TInstantCurrency.GetAsBoolean: Boolean;
begin
  Result := Value <> 0;
end;

function TInstantCurrency.GetAsDateTime: TDateTime;
begin
  Result := Value;
end;

function TInstantCurrency.GetAsFloat: Double;
begin
  Result := Value;
end;

function TInstantCurrency.GetAsInteger: Integer;
begin
  Result := Round(Value);
end;

function TInstantCurrency.GetAsString: string;
begin
  Result := CurrToStr(Value);
end;

function TInstantCurrency.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantCurrency.GetDisplayText: string;
begin
  if Assigned(Metadata) and (Metadata.EditMask <> '') then
    Result := FormatCurr(Metadata.EditMask, Value)
  else
    Result := inherited GetDisplayText;
end;

function TInstantCurrency.GetIsDefault: Boolean;
begin
  if Assigned(Metadata) and (Metadata.Defaultvalue <> '') then
    Result := inherited GetIsDefault
  else
    Result := Value = 0;
end;

function TInstantCurrency.GetValue: Currency;
begin
  Result := FValue;
end;

procedure TInstantCurrency.Initialize;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    try
      FValue := StrToCurr(Metadata.DefaultValue);
    except
      on E: Exception do
        raise ConversionError(E);
    end;
end;

procedure TInstantCurrency.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadCurrency;
end;

procedure TInstantCurrency.Reset;
begin
  inherited;
  if Assigned(Metadata) then
    Initialize
  else
    FValue := 0;
  Changed;
end;

procedure TInstantCurrency.SetAsBoolean(AValue: Boolean);
begin
  inherited;
  Value := Ord(AValue);
end;

procedure TInstantCurrency.SetAsDateTime(AValue: TDateTime);
begin
  inherited;
  Value := AValue;
end;

procedure TInstantCurrency.SetAsFloat(AValue: Double);
begin
  Value := AValue;
end;

procedure TInstantCurrency.SetAsInteger(AValue: Integer);
begin
  Value := AValue;
end;

procedure TInstantCurrency.SetAsString(const AValue: string);
begin
  try
    Value := StrToCurr(AValue)
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

procedure TInstantCurrency.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantCurrency.SetValue(AValue: Currency);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantCurrency.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteCurrency(Value);
end;

{ TInstantBoolean }

procedure TInstantBoolean.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantBoolean then
    Value := TInstantBoolean(Source).Value;
end;

class function TInstantBoolean.AttributeType: TInstantAttributeType;
begin
  Result := atBoolean;
end;

function TInstantBoolean.GetAsBoolean: Boolean;
begin
  Result := Value;
end;

function TInstantBoolean.GetAsFloat: Double;
begin
  Result := AsInteger;
end;

function TInstantBoolean.GetAsInteger: Integer;
begin
  Result := Integer(Value);
end;

function TInstantBoolean.GetAsString: string;
begin
  if FValue then
    Result := STrueString
  else
    Result := SFalseString;
end;

function TInstantBoolean.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantBoolean.GetIsDefault: Boolean;
begin
  if Assigned(Metadata) then
    Result := Value = (SameText(Metadata.DefaultValue, InstantTrueString))
  else
    Result := False;
end;

function TInstantBoolean.GetIsNull: Boolean;
begin
  if Assigned(Metadata) and Metadata.UseNull then
    Result := AsBoolean = False
  else
    Result := inherited GetIsNull;
end;

function TInstantBoolean.GetValue: Boolean;
begin
  Result := FValue;
end;

procedure TInstantBoolean.Initialize;
begin
  if Assigned(Metadata) then
    try
      FValue := SameText(Metadata.DefaultValue, InstantTrueString);
    except
      on E: Exception do
        raise ConversionError(E);
    end;
end;

procedure TInstantBoolean.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadBoolean
end;

procedure TInstantBoolean.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := False;
  Changed;
end;

procedure TInstantBoolean.SetAsBoolean(AValue: Boolean);
begin
  Value := AValue;
end;

procedure TInstantBoolean.SetAsFloat(AValue: Double);
begin
  AsInteger := Round(AValue);
end;

procedure TInstantBoolean.SetAsInteger(AValue: Integer);
begin
  Value := Boolean(AValue);
end;

procedure TInstantBoolean.SetAsString(const AValue: string);
begin
  if SameText(AValue, STrueString) then
    Value := True
  else if SameText(AValue, SFalseString) then
    Value := False
  else
    raise ConversionError(nil);
end;

procedure TInstantBoolean.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantBoolean.SetValue(AValue: Boolean);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantBoolean.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteBoolean(Value);
end;

{ TInstantString }

procedure TInstantString.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantString then
    Value := TInstantString(Source).Value;
end;

class function TInstantString.AttributeType: TInstantAttributeType;
begin
  Result := atString;
end;

function TInstantString.GetAsBoolean: Boolean;
begin
  if SameText(Value, STrueString) then
    Result := True
  else if SameText(Value, SFalseString) then
    Result := False
  else
    raise ConversionError(nil);
end;

function TInstantString.GetAsCurrency: Currency;
begin
  Result := AsFloat;
end;

function TInstantString.GetAsDateTime: TDateTime;
begin
  try
    Result := StrToDateTime(Value);
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

function TInstantString.GetAsFloat: Double;
begin
  try
    Result := StrToFloat(Value);
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

function TInstantString.GetAsInteger: Integer;
begin
  try
    Result := StrToInt(Value);
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

function TInstantString.GetAsString: string;
begin
  Result := Value;
end;

function TInstantString.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantString.GetValue: string;
begin
  Result:= FValue;
end;

procedure TInstantString.Initialize;
begin
  if Assigned(Metadata) then
    FValue := Metadata.DefaultValue;
end;

procedure TInstantString.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadString;
end;

procedure TInstantString.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := '';
  Changed;
end;

procedure TInstantString.SetAsBoolean(AValue: Boolean);
begin
  if AValue then
    Value := STrueString
  else
    Value := SFalseString;
end;

procedure TInstantString.SetAsCurrency(AValue: Currency);
begin
  AsFloat := AValue;
end;

procedure TInstantString.SetAsDateTime(AValue: TDateTime);
begin
  Value := DateTimeToStr(AValue);
end;

procedure TInstantString.SetAsFloat(AValue: Double);
begin
  Value := FloatToStr(AValue);
end;

procedure TInstantString.SetAsInteger(AValue: Integer);
begin
  Value := IntToStr(AValue);
end;

procedure TInstantString.SetAsString(const AValue: string);
begin
  Value := AValue;
end;

procedure TInstantString.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantString.SetValue(const AValue: string);
begin
  Validate(AValue);
  if AValue <> FValue then
  begin
    FValue:= AValue;
    Changed;
  end;
end;

procedure TInstantString.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteString(Value);
end;

{ TInstantDateTime }

procedure TInstantCustomDateTime.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCustomDateTime then
    Value := TInstantCustomDateTime(Source).Value
end;

function TInstantCustomDateTime.DefaultValue: TDateTime;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    if SameText(Metadata.DefaultValue, InstantNowString) then
      Result := Now
    else if SameText(Metadata.DefaultValue, InstantDateString) then
      Result := Date
    else if SameText(Metadata.DefaultValue, InstantTimeString) then
      Result := Time
    else
      try
        Result := InstantStrToDateTime(Metadata.DefaultValue);
      except
        on E: Exception do
          raise ConversionError(E);
      end
  else
    Result := 0;
end;

function TInstantCustomDateTime.GetAsDateTime: TDateTime;
begin
  Result := Value;
end;

function TInstantCustomDateTime.GetAsString: string;
begin
  Result := DateTimeToStr(Value);
end;

function TInstantCustomDateTime.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantCustomDateTime.GetDisplayText: string;
begin
  if AsDateTime = 0 then
    Result := ''
  else begin
    if Assigned(Metadata) and (Metadata.EditMask <> '') then
      DateTimeToString(Result, Metadata.EditMask, AsDateTime)
    else
      Result := inherited GetDisplayText;
  end
end;

function TInstantCustomDateTime.GetIsDefault: Boolean;
begin
  Result := Value = DefaultValue;
end;

function TInstantCustomDateTime.GetIsNull: Boolean;
begin
  if Assigned(Metadata) and Metadata.UseNull then
    Result := AsDateTime = 0
  else
    Result := inherited GetIsNull;
end;

function TInstantCustomDateTime.GetValue: TDateTime;
begin
  Result := FValue;
end;

procedure TInstantCustomDateTime.Initialize;
begin
  FValue := DefaultValue;
end;

procedure TInstantCustomDateTime.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadDate;
end;

procedure TInstantCustomDateTime.Reset;
begin
  if Assigned(Metadata) then
    Initialize
  else
    FValue := 0;
  Changed;
end;

procedure TInstantCustomDateTime.SetAsDateTime(AValue: TDateTime);
begin
  Value := AValue;
end;

procedure TInstantCustomDateTime.SetAsString(const AValue: string);
begin
  try
    Value := StrToDateTime(AValue)
  except
    on E: Exception do
      raise ConversionError(E);
  end;
end;

procedure TInstantCustomDateTime.SetAsVariant(AValue: Variant);
begin
  try
    Value := AValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantCustomDateTime.SetValue(AValue: TDateTime);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    Changed;
  end;
end;

procedure TInstantCustomDateTime.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteDate(Value);
end;

{ TInstantDateTime }

class function TInstantDateTime.AttributeType: TInstantAttributeType;
begin
  Result := atDateTime;
end;

function TInstantDateTime.GetAsDate: TDateTime;
begin
  Result := DateOf(Value);
end;

function TInstantDateTime.GetAsTime: TDateTime;
begin
  Result := TimeOf(Value);
end;

procedure TInstantDateTime.SetAsDate(AValue: TDateTime);
begin
  Value := DateOf(AValue);
end;

procedure TInstantDateTime.SetAsTime(AValue: TDateTime);
begin
  Value := TimeOf(AValue);
end;

{ TInstantBlob }

procedure TInstantBlob.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantBlob then
    LoadDataFromStream(TInstantBlob(Source).Stream)
end;

class function TInstantBlob.AttributeType: TInstantAttributeType;
begin
  Result := atBlob;
end;

procedure TInstantBlob.Clear;
begin
  Stream.Clear;
  Changed;
end;

destructor TInstantBlob.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TInstantBlob.GetAsString: string;
begin
  Result := string(Value);
end;

function TInstantBlob.GetAsVariant: Variant;
begin
  Result := Value;
end;

function TInstantBlob.GetBytes: TInstantBytes;
begin
  SetLength(Result, Size);
  if Size > 0 then
    Read(Result[0], 0, Size);
end;

function TInstantBlob.GetIsNull: Boolean;
begin
  Result := Size = 0;
end;

function TInstantBlob.GetSize: Integer;
begin
  Result := Stream.Size;
end;

function TInstantBlob.GetStream: TMemoryStream;
begin
  if not Assigned(FStream) then
    FStream := TMemoryStream.Create;
  Result := FStream;
end;

function TInstantBlob.GetValue: string;
var
  LAnsiValue: AnsiString;
  LValue: String;
begin
  if Size > 0 then
  begin
    if not UseUnicode then
    begin
      SetLength(LAnsiValue, Size div SizeOf(AnsiChar));
      Read(LAnsiValue[1], 0, Size);
      Result := string(LAnsiValue);
    end
    else
    begin
      SetLength(LValue, Size div SizeOf(Char));
      Read(LValue[1], 0, Size);
      Result := LValue;
    end;
  end
  else
    Result := '';
end;

procedure TInstantBlob.Initialize;
begin
  if Assigned(Metadata) then
    if (Metadata.Defaultvalue <> '') then
    begin
      try
        AsString := Metadata.Defaultvalue;
      except
        on E: Exception do
          raise ConversionError(E);
      end;
    end
    else
      Stream.Clear;
end;

procedure TInstantBlob.LoadDataFromStream(AStream: TStream);
begin
  if not Assigned(AStream) then
    Exit;
  Stream.LoadFromStream(AStream);
  Changed;
end;

function TInstantBlob.Read(var Buffer; Position, Count: Integer): Integer;
begin
  Stream.Position := Position;
  Result := Stream.Read(Buffer, Count);
end;

function TInstantBlob.ReadBuffer(var Buffer; Position, Count: Integer): Integer;
begin
  Result := Read(Buffer, Position, Count);
end;

procedure TInstantBlob.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Reader.ReadBinary(LoadDataFromStream);
end;

procedure TInstantBlob.Reset;
begin
  if Assigned(Metadata) then
  begin
    Initialize;
    Changed;
  end else
    Clear;
end;

procedure TInstantBlob.SaveDataToStream(AStream: TStream);
begin
  if Assigned(AStream) then
    Stream.SaveToStream(AStream);
end;

procedure TInstantBlob.SetAsString(const AValue: string);
begin
  Value := AValue;
end;

procedure TInstantBlob.SetAsVariant(AValue: Variant);
var
  LNewValue: string;
begin
  LNewValue := VarToStr(AValue);
  try
    if Value <> LNewValue then
    Value := LNewValue;
  except
    on E: Exception do
      raise InvalidValueError(AValue, E);
  end;
end;

procedure TInstantBlob.SetBytes(const AValue: TInstantBytes);
var
  L: Integer;
begin
  L := length(AValue);
  if L > 0 then
  begin
    Stream.Clear;
    WriteBuffer(AValue[0], 0, L);
    Stream.Size := L;
  end
  else
    Clear;
end;

procedure TInstantBlob.SetValue(const AValue: string);
var
  L: Integer;
  LAnsiValue: AnsiString;
begin
    //Default Blob streaming is not based on UseUnicode setting.
  LAnsiValue := AnsiString(AValue);
  L := Length(LAnsiValue) * SizeOf(AnsiChar);
  if L > 0 then
  begin
    Stream.Clear;
    WriteBuffer(LAnsiValue[1], 0, L);
    Stream.Size := L;
  end
  else
    Clear;
end;

function TInstantBlob.UseUnicode: boolean;
begin
  if Assigned(Owner) and Assigned(Owner.Connector) then
    Result := Owner.Connector.UseUnicode
  {$IFNDEF MARS_FIREDAC}
  else if InstantDefaultConnector <> nil then
    Result := InstantDefaultConnector.UseUnicode
  {$ENDIF}
  else
    Result := False;
end;

function TInstantBlob.Write(const Buffer; Position, Count: Integer): Integer;
var
  LValue: AnsiString;

  function CompareBuffers: Boolean;
  var
    I: Integer;
    B: {$IFDEF D12+}Byte{$ELSE}Char{$ENDIF};
  begin
    Stream.Position := Position;
    for I := 0 to Pred(Count) do
    begin
      Result := (Stream.Read(B, 1) = 1) and (B = {$IFDEF D12+}PByte{$ELSE}PChar{$ENDIF}(@Buffer)[I]);
      if not Result then
        Exit;
    end;
    Result := True;
  end;

begin
  SetLength(LValue, Count + 1);
{$IFDEF D18+}
  AnsiStrings.StrLCopy(PAnsiChar(LValue), PAnsiChar(@Buffer), Count);
{$ELSE}
  StrLCopy(PAnsiChar(LValue), PAnsiChar(@Buffer), Count);
{$ENDIF}
  Validate(string(LValue));

  if not CompareBuffers then
  begin
    Stream.Position := Position;
    Result := Stream.Write(Buffer, Count);
  end
  else
    Result := 0;
end;

function TInstantBlob.WriteBuffer(const Buffer; Position,
  Count: Integer): Integer;
begin
  Result := Write(Buffer, Position, Count);
  IsChanged := Result > 0;
end;

procedure TInstantBlob.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteBinary(SaveDataToStream);
end;

{$IFDEF INSTANTOBJECTS_FMX}
procedure TInstantBlob.AssignPicture(Source: TImage);
begin
  if Assigned(Source.Bitmap) then
  begin
    Stream.Clear;
    Source.Bitmap.SaveToStream(Stream);
  end
  else
    Clear;
  Changed;
end;

procedure TInstantBlob.AssignToPicture(Dest: TImage);
var
  Image : TImage;
  InstantGraphicFileFormat : TInstantGraphicFileFormat;
begin
  if Stream.Size > 0 then
  begin
    Stream.Position := 0;
    InstantGraphicFileFormat := InstantResolveGraphicFileType(Stream);
    if InstantGraphicFileFormat = gffUnknown then
      raise EInstantError.Create(SUnsupportedGraphicStream);
    Image := TImage.Create(nil);
    Try
      Image.Bitmap.LoadFromStream(Stream);
      Dest.Assign(Image);
    Finally
      Image.Free;
    End;
  end
  else if Assigned(Dest.Bitmap) then
    Dest.Bitmap := nil;
end;
{$ELSE}
{$IFNDEF IO_CONSOLE}
procedure TInstantBlob.AssignPicture(Source: TPicture);
begin
  if Assigned(Source.Graphic) then
  begin
    Stream.Clear;
    Source.Graphic.SaveToStream(Stream);
  end
  else
    Clear;
  Changed;
end;

procedure TInstantBlob.AssignToPicture(Dest: TPicture);
var
  Image : TGraphic;
  GraphicClass : TGraphicClass;
  InstantGraphicFileFormat : TInstantGraphicFileFormat;
begin
  if Stream.Size > 0 then
  begin
    Stream.Position := 0;
    InstantGraphicFileFormat := InstantResolveGraphicFileType(Stream);
    if InstantGraphicFileFormat = gffUnknown then
      raise EInstantError.Create(SUnsupportedGraphicStream);
    GraphicClass := InstantGraphicFileFormatToGraphicClass(InstantGraphicFileFormat);
    if not Assigned(GraphicClass) then
      raise EInstantError.Create(SUnsupportedGraphicClass);
    Image := GraphicClass.Create;
    Try
      Image.LoadFromStream(Stream);
      Dest.Assign(Image);
    Finally
      Image.Free;
    End;
  end
  else if Assigned(Dest.Graphic) then
    Dest.Graphic := nil;
end;
{$ENDIF}
{$ENDIF}

procedure TInstantBlob.AssignTo(Dest: TPersistent);
begin
{$IFDEF INSTANTOBJECTS_FMX}
  if Dest is TImage then
    AssignToPicture(TImage(Dest))
  else
{$ELSE}
  {$IFNDEF IO_CONSOLE}
  if Dest is TPicture then
    AssignToPicture(TPicture(Dest))
  else
  {$ENDIF}
{$ENDIF}
    inherited;
end;

{ TInstantMemo }

class function TInstantMemo.AttributeType: TInstantAttributeType;
begin
  Result := atMemo;
end;

procedure TInstantMemo.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Value := Reader.ReadString;
end;

procedure TInstantMemo.SetValue(const AValue: string);
var
  L: Integer;
  LAnsiValue: AnsiString;
  LValue: String;
begin
  //Don't call inherited: Text streaming is different by UseUnicode
  if not UseUnicode then
  begin
    LAnsiValue := AnsiString(AValue);
    L := Length(LAnsiValue) * SizeOf(AnsiChar);
  end
  else
  begin
    LValue := AValue;
    L := Length(LValue) * SizeOf(Char);
  end;
  if L > 0 then
  begin
    Stream.Clear;
    if not UseUnicode then
      WriteBuffer(LAnsiValue[1], 0, L)
    else
      WriteBuffer(LValue[1], 0, L);
    Stream.Size := L;
  end
  else
    Clear;
end;

procedure TInstantMemo.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteString(Value);
end;

{ TInstantComplex }

function TInstantComplex.AttachObject(AObject: TInstantObject): Boolean;
begin
  Result := False;
end;

function TInstantComplex.DetachObject(AObject: TInstantObject): Boolean;
begin
  Result := False;
end;

function TInstantComplex.GetAllowOwned: Boolean;
begin
  Result := False;
end;

function TInstantComplex.GetConnector: TInstantConnector;
begin
  if Assigned(Owner) then
    Result := Owner.Connector
  else
    Result := nil;
  InstantCheckConnector(Result);
end;

function TInstantComplex.GetRequiredClass: TInstantObjectClass;
begin
  if Assigned(Metadata) then
    Result := TInstantObjectClass(Metadata.ObjectClass)
  else
    Result := TInstantObject;
end;

function TInstantComplex.GetRequiredClassName: string;
begin
  if Assigned(Metadata) then
    Result := Metadata.ObjectClassName
  else
    Result := TInstantObject.ClassName;
end;

procedure TInstantComplex.ValidateObject(AObject: TInstantObject);
begin
  if Assigned(AObject) then
  begin
    ValidateObjectClassType(AObject.ClassType);
    if Assigned(Owner) and (AObject.Connector <> Connector) then
      raise EInstantValidationError.CreateFmt(SInvalidConnector,
        [AObject.ClassName, AObject.Id, ClassName, Name]);
  end;
end;

procedure TInstantComplex.ValidateObjectClassType(
  AClass: TInstantObjectClass);
begin
  if not (AClass.InheritsFrom(RequiredClass)) then
    raise EInstantValidationError.CreateFmt(SInvalidObjectClass,
      [AClass.ClassName, ClassName, Name, RequiredClass.ClassName]);
end;

{ TInstantElement }

function TInstantElement.AttachObject(AObject: TInstantObject): Boolean;
begin
  Value  := AObject;
  Result := Value = AObject;
end;

function TInstantElement.CreateObject: TInstantObject;
begin
  Result := RequiredClass.Create(Connector) as TInstantObject;
end;

function TInstantElement.DetachObject(AObject: TInstantObject): Boolean;
begin
  Result := Value = AObject;
  if Result then
    Value := nil;
end;

function TInstantElement.GetAsObject: TInstantObject;
begin
  Result := Value;
end;

function TInstantElement.GetIsNull: Boolean;
begin
  if Assigned(Metadata) and Metadata.UseNull then
    Result := not HasValue
  else
    Result := inherited GetIsNull;
end;

function TInstantElement.HasValue: Boolean;
begin
  Result := False;
end;

procedure TInstantElement.LoadObjectFromStream(AStream: TStream);
var
  Obj: TPersistent;
  MemoryStream : TMemoryStream;
begin
  if not Assigned(AStream) then
    Exit;

  Obj := nil;
  if Connector.BlobStreamFormat = sfBinary then
    Obj := InstantReadObjectFromStream(AStream, nil, nil, Connector)
  else if Connector.BlobStreamFormat = sfXML then
  begin
    MemoryStream := TMemoryStream.Create;
    try
      InstantObjectTextToBinary(AStream, MemoryStream);
      MemoryStream.Position := 0;
      Obj := InstantReadObjectFromStream(MemoryStream, nil, nil, Connector);
    finally
      MemoryStream.Free;
    end;
  end;

  if Assigned(Obj) then
  begin
    try
      Value := TInstantObject(Obj) as TInstantObject
    except
      Obj.Free;
      raise;
    end;
  end;
end;

procedure TInstantElement.SaveObjectToStream(AStream: TStream);
var
  MemoryStream : TMemoryStream;
begin
  if not (Assigned(AStream) and Assigned(Value)) then
    Exit;

  if Connector.BlobStreamFormat = sfBinary then
    InstantWriteObjectToStream(AStream, Value)
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      InstantWriteObjectToStream(MemoryStream, Value);
      MemoryStream.Position := 0;
      InstantObjectBinaryToText(MemoryStream, AStream);
    finally
      MemoryStream.Free;
    end;
  end;
end;

procedure TInstantElement.SetAsObject(AValue: TInstantObject);
begin
  Value := AValue as TInstantObject;
end;

procedure TInstantElement.SetValue(AValue: TInstantObject);
begin
  ValidateObject(AValue);
end;

{ TInstantPart }

procedure TInstantPart.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantPart then
    with TInstantPart(Source) do
      if Assigned(FValue) then
        Self.Value := FValue.Clone(Self.Connector)
      else
        Self.Value := nil;
end;

class function TInstantPart.AttributeType: TInstantAttributeType;
begin
  Result := atPart;
end;

procedure TInstantPart.ClearOwnerContext(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.ClearOwnerContext;
end;

destructor TInstantPart.Destroy;
begin
  DestroyObject;
  inherited;
end;

procedure TInstantPart.DestroyObject;
begin
  FreeAndNil(FValue);
end;

function TInstantPart.GetAllowOwned: Boolean;
begin
  if Metadata.StorageKind = skExternal then
    Result := True
  else
    Result := inherited GetAllowOwned();
end;

function TInstantPart.GetIsChanged: Boolean;
begin
  Result := inherited GetIsChanged or (Assigned(FValue) and FValue.IsChanged);
end;

function TInstantPart.GetIsDefault: Boolean;
begin
  if Assigned(FValue) then
    Result := (FValue.ClassType = RequiredClass) and FValue.IsDefault
  else
    Result := True;
end;

function TInstantPart.GetValue: TInstantObject;
begin
  if not Assigned(FValue) then
  begin
    FValue := CreateObject;
    SetOwnerContext(FValue);
  end;
  Result := FValue;
end;

function TInstantPart.HasValue: Boolean;
begin
  Result := Assigned(FValue);
end;

procedure TInstantPart.ReadObject(Reader: TInstantReader);
var
  Obj: TPersistent;
begin
  ReadName(Reader);
  Reader.ReadIdent;
  Obj := Reader.ReadObject(nil, Connector);
  try
    Value := Obj as TInstantObject;
  except
    Obj.Free;
    raise;
  end;
end;

procedure TInstantPart.Reset;
begin
  if not IsDefault then
  begin
    DestroyObject;
    Changed;
  end;
end;

procedure TInstantPart.SetOwnerContext(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.SetOwnerContext(Owner, Self)
end;

procedure TInstantPart.SetValue(AValue: TInstantObject);
begin
  if AValue <> FValue then
  begin
    inherited;
    ClearOwnerContext(FValue);
    DestroyObject;
    FValue := AValue;
    SetOwnerContext(FValue);
    Changed;
  end;
end;

procedure TInstantPart.Unchanged;
begin
  inherited;
  if HasValue then
    Value.Unchanged;
end;

procedure TInstantPart.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if Assigned(AObject) then
  begin
    if Metadata.StorageKind = skEmbedded then
      if AObject.IsPersistent then
        raise EInstantValidationError.CreateFmt(SPersistentObjectNotAllowed,
          [AObject.ClassName, AObject.Id]);
    if Assigned(Owner) and (AObject = Owner) then
      raise EInstantValidationError.CreateFmt(SOwnershipRecursion,
        [AObject.ClassName, AObject.Id]);
  end;
end;

procedure TInstantPart.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteIdent('');
  Writer.WriteObject(Value);
end;

{ TInstantReference }

procedure TInstantReference.Assign(Source: TPersistent);
begin
  if Source is TInstantReference then
  begin
    with TInstantReference(Source) do
    begin
      // cross-connector object assignment must be supported for InstantPump.
      if (Self.FConnector <> FConnector) then
      begin
        if Assigned(Value) then
          Self.Value := Value.Clone(Self.Connector)
        else
          Self.Value := nil;  
      end
      else
        Self.Value := Value;
    end;
  end;
end;

class function TInstantReference.AttributeType: TInstantAttributeType;
begin
  Result := atReference;
end;

function TInstantReference.CreateObject: TInstantObject;
begin
  if Assigned(Metadata) and (Metadata.DefaultValue <> '') then
    Result := RequiredClass.Retrieve(Metadata.Defaultvalue, False, False,
      Connector) as TInstantObject
  else
    Result := nil;
end;

destructor TInstantReference.Destroy;
begin
  DestroyObjectReference;
  inherited;
end;

procedure TInstantReference.DestroyObject;
begin
  ObjectReference.DestroyInstance;
end;

procedure TInstantReference.DestroyObjectReference;
begin
  FreeAndNil(FObjectReference);
end;

function TInstantReference.GetAllowOwned: Boolean;
begin
  Result := FAllowOwned;
end;

function TInstantReference.GetConnector: TInstantConnector;
begin
  Result := FConnector;
  if not Assigned(Result) then
    Result := inherited GetConnector;
end;

function TInstantReference.GetInstance: TInstantObject;
begin
  Result := ObjectReference.Instance;
end;

function TInstantReference.GetIsBroken: Boolean;
begin
  Result := ObjectReference.IsBroken;
end;

function TInstantReference.GetIsDefault: Boolean;
var
  DefaultObjectId: string;
begin
  if Assigned(Metadata) then
  begin
    DefaultObjectId := Metadata.DefaultValue;
    if DefaultObjectId = '' then
      Result := ObjectClassName = ''
    else
      Result := (ObjectClass = RequiredClass) and
        (ObjectId = DefaultObjectId)
  end else
    Result := ObjectClassName = '';
end;

function TInstantReference.GetObjectClass: TInstantObjectClass;
begin
  Result := ObjectReference.ObjectClass;
end;

function TInstantReference.GetObjectClassName: string;
begin
  Result := ObjectReference.ObjectClassName;
end;

function TInstantReference.GetObjectId: string;
begin
  Result := ObjectReference.ObjectId;
end;

function TInstantReference.GetObjectReference: TInstantObjectReference;
begin
  if not Assigned(FObjectReference) then
    FObjectReference := TInstantObjectReference.Create(nil, True, Self);
  Result := FObjectReference;
end;

function TInstantReference.GetValue: TInstantObject;
var
  Obj: TInstantObject;
begin
  if not Assigned(Instance) then
  begin
    if HasReference then
    begin
      if not IsBroken then
        RetrieveObject;
    end else
    begin
      Obj := CreateObject;
      if Assigned(Obj) then
      begin
        ObjectReference.AssignInstance(Obj);
        Obj.Release;
      end;
    end;
  end;
  Result := Instance;
end;

function TInstantReference.HasReference: Boolean;
begin
  Result := ObjectReference.HasReference;
end;

function TInstantReference.HasValue: Boolean;
begin
  Result := ObjectReference.HasInstance;
end;

procedure TInstantReference.LoadObjectFromStream(AStream: TStream);
begin
  inherited;
  if Assigned(Value) then
    Value.Release;
end;

procedure TInstantReference.ReadObject(Reader: TInstantReader);
begin
  ReadName(Reader);
  Reader.ReadIdent;
  ObjectReference.ReadAsObject(Reader);
  Changed;
end;

procedure TInstantReference.ReferenceObject(AObjectClass: TInstantObjectClass;
  const AObjectId: string);
begin
  if SameInstance(AObjectClass, AObjectId) then
    Exit;
  if Assigned(AObjectClass) then
  begin
    ValidateObjectClassType(AObjectClass);
    ObjectReference.ReferenceObject(AObjectClass, AObjectId);
    Changed;
  end else
    Reset;
end;

procedure TInstantReference.ReferenceObject(const AObjectClassName,
  AObjectId: string);
var
  AObjectClass: TInstantObjectClass;
begin
  if SameInstance(AObjectClassName, AObjectId) then
    Exit;
  if AObjectClassName = '' then
    AObjectClass := nil
  else
    AObjectClass := InstantFindClass(AObjectClassName);
  ReferenceObject(AObjectClass, AObjectId);
end;

procedure TInstantReference.Reset;
begin
  if not IsDefault then
  begin
    DestroyObjectReference;
    Changed;
  end;
end;

function TInstantReference.RetrieveObject: TInstantObject;
begin
  Result := ObjectReference.Dereference(Connector);
end;

function TInstantReference.SameInstance(AObject: TInstantObject): Boolean;
begin
  if Assigned(AObject) then
    Result := ObjectReference.Equals(AObject)
  else
    Result := not ObjectReference.HasReference;
end;

function TInstantReference.SameInstance(const AObjectClassName,
  AObjectId: string): Boolean;
begin
  Result := ObjectReference.Equals(AObjectClassName, AObjectId);
end;

function TInstantReference.SameInstance(AObjectClass: TInstantObjectClass;
  AObjectId: string): Boolean;
begin
  if Assigned(AObjectClass) then
    Result := SameInstance(AObjectClass.ClassName, AObjectId)
  else
    Result := SameInstance(nil)
end;

procedure TInstantReference.SetAllowOwned(Value: Boolean);
begin
  FAllowOwned := Value;
end;

procedure TInstantReference.SetConnector(Value: TInstantConnector);
begin
  if Value = FConnector then
    Exit;
  Reset;
  FConnector := Value;
end;

procedure TInstantReference.SetInstance(AValue: TInstantObject);
begin
  ObjectReference.Instance := AValue;
end;

procedure TInstantReference.SetValue(AValue: TInstantObject);
begin
  if SameInstance(AValue) then
    Exit;
  inherited;
  Instance := AValue;
  Changed;
end;

procedure TInstantReference.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if Assigned(AObject) then
    AObject.CheckId;
end;

procedure TInstantReference.WriteObject(Writer: TInstantWriter);
begin
  WriteName(Writer);
  Writer.WriteIdent('');
  ObjectReference.WriteAsObject(Writer);
end;

{ TInstantContainer }

function TInstantContainer.Add(AObject: TInstantObject): Integer;
begin
  ValidateObject(AObject);
  BeforeContentChange(ctAdd, -1, AObject);
  Result := InternalAdd(AObject);
  AfterContentChange(ctAdd, Result, AObject);
end;

function TInstantContainer.AddReference(const AObjectClassName, AObjectId:
    string): Integer;
begin
  if Assigned(Metadata) and (Metadata.StorageKind = skEmbedded) then
    raise EInstantError.CreateFmt(SUnsupportedAttributeOperation,
        ['AddReference', ClassName, Name, 'StorageKind = skEmbedded']);
  if not InstantFindClass(AObjectClassName).InheritsFrom(RequiredClass) then
    raise EInstantValidationError.CreateFmt(SInvalidObjectClass,
        [AObjectClassName, ClassName, Name, RequiredClass.ClassName]);

  BeforeContentChange(ctAddRef, -1, nil);
  Result := InternalAddReference(AObjectClassName, AObjectId);
  AfterContentChange(ctAddRef, Result, nil);
end;

procedure TInstantContainer.AfterContentChange(
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
  Changed;
  if Assigned(Owner) then
    Owner.DoAfterContentChange(Self, ChangeType, Index, AObject);
end;

function TInstantContainer.AttachObject(AObject: TInstantObject): Boolean;
begin
  Result := Add(AObject) <> -1;
end;

procedure TInstantContainer.BeforeContentChange(
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
  if Assigned(Owner) then
    Owner.DoBeforeContentChange(Self, ChangeType, Index, AObject);
end;

procedure TInstantContainer.CheckRange(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EInstantRangeError.CreateFmt(SIndexOutOfBounds, [Index]);
end;

procedure TInstantContainer.Clear;
begin
  Owner.AddRef;
  try
    BeforeContentChange(ctClear, -1, nil);
    InternalClear;
    AfterContentChange(ctClear, -1, nil);
  finally
    Owner.Release;
  end;
end;

procedure TInstantContainer.Delete(Index: Integer);
var
  Obj: TInstantObject;
begin
  CheckRange(Index);
  Obj := Instances[Index];
  if Assigned(Obj) then
    Obj.AddRef;
  try
    BeforeContentChange(ctRemove, Index, Obj);
    InternalDelete(Index);
    AfterContentChange(ctRemove, Index, Obj);
  finally
    Obj.Free;
  end;
end;

function TInstantContainer.DetachObject(AObject: TInstantObject): Boolean;
begin
  Result := Remove(AObject) <> -1;
end;

procedure TInstantContainer.Exchange(Index1, Index2: Integer);
begin
  CheckRange(Index1);
  CheckRange(Index2);
  InternalExchange(Index1, Index2);
  Changed;
end;

function TInstantContainer.GetChangeCount: Integer;
var
  I: Integer;
  Obj: TInstantObject;
begin
  Result := 0;
  for I := 0 to Pred(Count) do
  begin
    Obj := Items[I];
    if Assigned(Obj) and Obj.IsChanged then
      Inc(Result);
  end;
end;

function TInstantContainer.GetInstances(Index: Integer): TInstantObject;
begin
  Result := Items[Index];
end;

function TInstantContainer.GetIsDefault: Boolean;
begin
  Result := Count = 0;
end;

function TInstantContainer.GetItems(
  Index: Integer): TInstantObject;
begin
  CheckRange(Index);
  Result := InternalGetItems(Index);
end;

function TInstantContainer.HasItem(Index: Integer): Boolean;
begin
  Result := Assigned(Instances[Index]);
end;

function TInstantContainer.IndexOf(AObject: TInstantObject): Integer;
begin
  Result := InternalIndexOf(AObject);
end;

function TInstantContainer.IndexOfInstance(Instance: Pointer): Integer;
begin
  Result := InternalIndexOfInstance(Instance);
end;

procedure TInstantContainer.Insert(Index: Integer;
  AObject: TInstantObject);
begin
  if Index <> 0 then
    CheckRange(Index);
  ValidateObject(AObject);
  BeforeContentChange(ctAdd, Index, AObject);
  InternalInsert(Index, AObject);
  AfterContentChange(ctAdd, Index, AObject);
end;

function TInstantContainer.isExmbedded: Boolean;
begin
  Result := Assigned(Metadata) and (Metadata.StorageKind = skEmbedded);
end;

function TInstantContainer.isExternal: boolean;
begin
  Result := Assigned(Metadata) and (Metadata.StorageKind=skExternal);
end;

function TInstantContainer.isVirtual: boolean;
begin
  Result := Assigned(Metadata) and (Metadata.StorageKind = skVirtual);
end;

procedure TInstantContainer.LoadObjectsFromStream(AStream: TStream);
var
  Obj: TPersistent;
  MemoryStream : TMemoryStream;
begin
  if not Assigned(AStream) then
    Exit;
  Clear;
  if Connector.BlobStreamFormat = sfBinary then
  begin
    while AStream.Position < AStream.Size do
    begin
      Obj := InstantReadObjectFromStream(AStream, nil, nil, Connector);
      try
        Add(Obj as TInstantObject);
      except
        Obj.Free;
        raise;
      end;
    end;
  end
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      // After reading the last object, the XML stream may still contain a few
      // bytes for the final line break (in case the XML cose is beautified),
      // so we go ahead and read another object ony if there's more bytes in the
      // buffer.
      while AStream.Position < AStream.Size - (Length(sLineBreak) * SizeOf(Char)) do
      begin
        MemoryStream.Clear;
        InstantObjectTextToBinary(AStream, MemoryStream);
        MemoryStream.Position := 0;
        Obj := InstantReadObjectFromStream(MemoryStream, nil, nil, Connector);
        try
          Add(Obj as TInstantObject);
        except
          Obj.Free;
          raise;
        end;
      end;
    finally
      MemoryStream.Free;
    end;
  end;
  Changed;
end;

procedure TInstantContainer.Move(CurIndex, NewIndex: Integer);
begin
  CheckRange(CurIndex);
  CheckRange(NewIndex);
  InternalMove(CurIndex, NewIndex);
  Changed;
end;

procedure TInstantContainer.QuickSort(L, R: Integer;
  Compare: TInstantSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(Owner, Items[I], Items[P]) < 0 do
        Inc(I);
      while Compare(Owner, Items[J], Items[P]) > 0 do
        Dec(J);
      if I <= J then
      begin
        Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

function TInstantContainer.Remove(AObject: TInstantObject): Integer;
begin
  Result := IndexOf(AObject);
  if Result <> -1 then
    Delete(Result);
end;

procedure TInstantContainer.Reset;
begin
  Clear;
end;

procedure TInstantContainer.SaveObjectsToStream(AStream: TStream);
var
  I, P: Integer;
  MemoryStream : TMemoryStream;
begin
  if not Assigned(AStream) then
    Exit;
  if Connector.BlobStreamFormat = sfBinary then
  begin
    for I := 0 to Pred(Count) do
      InstantWriteObjectToStream(AStream, Items[I]);
  end
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      for I := 0 to Pred(Count) do
      begin
        P := MemoryStream.Position;
        InstantWriteObjectToStream(MemoryStream, Items[I]);
        MemoryStream.Position := P;
        InstantObjectBinaryToText(MemoryStream, AStream);
      end;
    finally
      MemoryStream.Free;
    end;
  end;
end;

procedure TInstantContainer.SetItems(Index: Integer;
  AValue: TInstantObject);
var
  Obj: TInstantObject;
begin
  CheckRange(Index);
  ValidateObject(AValue);
  Obj := Instances[Index];
  if Assigned(Obj) then
    Obj.AddRef;
  try
    BeforeContentChange(ctReplace, Index, AValue);
    InternalSetItems(Index, AValue);
    AfterContentChange(ctReplace, Index, AValue);
  finally
    Obj.Free;
  end;
end;

procedure TInstantContainer.Sort(Compare: TInstantSortCompare);
begin
  if Count > 1 then
    QuickSort(0, Pred(Count), Compare);
end;

procedure TInstantContainer.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if not Assigned(AObject) then
    raise EInstantError.CreateFmt(SUnassignedObjectInAttribute,
      [ClassName, Name]);
end;

{ TInstantParts }

procedure TInstantParts.Assign(Source: TPersistent);
var
  I: Integer;
  Obj: TInstantObject;
begin
  inherited;
  if Source is TInstantParts then
  begin
    Clear;
    with TInstantParts(Source) do
    begin
      for I := 0 to Pred(Count) do
      begin
        Obj := Items[I].Clone(Self.Connector);
        try
          Self.Add(Obj);
        except
          Obj.Free;
          raise;
        end;
      end;
    end;
  end;
end;

class function TInstantParts.AttributeType: TInstantAttributeType;
begin
  Result := atParts;
end;

procedure TInstantParts.ClearOwnerContext(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.ClearOwnerContext;
end;

function TInstantParts.CreateObjectReference(
  AObject: TInstantObject): TInstantObjectReference;
begin
  Result := TInstantObjectReference.Create(AObject, True);
end;

destructor TInstantParts.Destroy;
begin
  if Metadata.StorageKind = skEmbedded then
    FObjectList.Free
  else
    FObjectReferenceList.Free;
  inherited;
end;

procedure TInstantParts.DestroyObject(Index: Integer);
begin
  if Metadata.StorageKind <> skEmbedded then
    ObjectReferences[Index].DestroyInstance;
end;

function TInstantParts.DestroyObject(AObject: TInstantObject): Boolean;
var
  Index: Integer;
begin
  if Metadata.StorageKind <> skEmbedded then
  begin
    Index := IndexOf(AObject);
    Result := Index >= 0;
    if Result then
      ObjectReferences[Index].DestroyInstance;
  end else
    Result := False;
end;

function TInstantParts.GetAllowOwned: Boolean;
begin
  Result := FAllowOwned;
end;

function TInstantParts.GetCount: Integer;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := ObjectList.Count
  else
    Result := ObjectReferenceList.Count;
end;

function TInstantParts.GetInstances(Index: Integer): TInstantObject;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := inherited GetInstances(Index)
  else
    Result := ObjectReferences[Index].Instance;
end;

function TInstantParts.GetIsChanged: Boolean;
var
  I: Integer;
begin
  Result := inherited GetIsChanged;
  if not Result then
    for I := 0 to Pred(Count) do
    begin
      Result := Items[I].IsChanged;
      if Result then
        Exit;
    end;
end;

function TInstantParts.GetObjectList: TObjectList;
begin
  if not Assigned(FObjectList) then
    FObjectList := TObjectList.Create;
  Result := FObjectList;
end;

function TInstantParts.GetObjectReferenceList: TObjectList;
begin
  if not Assigned(FObjectReferenceList) then
    FObjectReferenceList := TObjectList.Create;
  Result := FObjectReferenceList;
end;

function TInstantParts.GetObjectReferences(
  Index: Integer): TInstantObjectReference;
begin
  Result := TInstantObjectReference(ObjectReferenceList[Index]);
end;

function TInstantParts.InternalAdd(AObject: TInstantObject): Integer;
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    Result := ObjectList.Add(AObject);
    SetOwnerContext(AObject);
  end
  else
  begin
    Ref := CreateObjectReference(AObject);
    try
      Result := ObjectReferenceList.Add(Ref);
      AObject.Free;
      SetOwnerContext(AObject);
    except
      Ref.Free;
      raise;
    end;
  end;
end;

function TInstantParts.InternalAddReference(const AObjectClassName, AObjectId:
    string): Integer;
var
  Ref: TInstantObjectReference;
begin
  Result := -1;

  if Metadata.StorageKind = skExternal then
  begin
    Ref := CreateObjectReference(nil);
    try
      Ref.ReferenceObject(AObjectClassName, AObjectId);
      Result := ObjectReferenceList.Add(Ref);
    except
      Ref.Free;
    end;
  end;
end;

procedure TInstantParts.InternalClear;
var
  I: Integer;
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    for I := 0 to Pred(ObjectList.Count) do
      ClearOwnerContext(ObjectList[I] as TInstantObject);
    ObjectList.Clear;
  end
  else
  begin
    for I := 0 to Pred(ObjectReferenceList.Count) do
      if Assigned(ObjectReferences[I].Instance) then
        ClearOwnerContext(ObjectReferences[I].Instance);
    ObjectReferenceList.Clear;
  end;
end;

procedure TInstantParts.InternalDelete(Index: Integer);
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    ClearOwnerContext(ObjectList[Index] as TInstantObject);
    ObjectList.Delete(Index)
  end
  else
  begin
    Ref := ObjectReferences[Index];
    if assigned(Ref.Instance) then
      ClearOwnerContext(Ref.Instance);
    ObjectReferenceList.Delete(Index);
  end;
end;

procedure TInstantParts.InternalExchange(Index1, Index2: Integer);
begin
  if Metadata.StorageKind = skEmbedded then
    ObjectList.Exchange(Index1, Index2)
  else
    ObjectReferenceList.Exchange(Index1, Index2);
end;

function TInstantParts.InternalGetItems(Index: Integer): TInstantObject;
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := ObjectList[Index] as TInstantObject
  else
  begin
    Ref := ObjectReferences[Index];
    if not Assigned(Ref.Instance) then
      Result := Ref.Dereference(Connector)
    else
      Result := Ref.Instance;
    if not Result.IsOwned then
      SetOwnerContext(Result);
  end;
end;

function TInstantParts.InternalIndexOf(AObject: TInstantObject): Integer;
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := ObjectList.IndexOf(AObject)
  else
  begin
    for Result := 0 to Pred(Count) do
    begin
      Ref := ObjectReferences[Result];
      if Ref.Equals(AObject) then
        Exit;
    end;
    Result := -1;
  end;
end;

function TInstantParts.InternalIndexOfInstance(Instance: Pointer): Integer;
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
    Result := InternalIndexOf(TInstantObject(Instance))
  else
  begin
    for Result := 0 to Pred(Count) do
    begin
      Ref := ObjectReferences[Result];
      if Ref.Instance = Instance then
        Exit;
    end;
    Result := -1;
  end;
end;

procedure TInstantParts.InternalInsert(Index: Integer; AObject: TInstantObject);
var
  Ref: TInstantObjectReference;
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    ObjectList.Insert(Index, AObject);
    SetOwnerContext(AObject);
  end
  else
  begin
    Ref := CreateObjectReference(AObject);
    try
      ObjectReferenceList.Insert(Index, Ref);
      AObject.Free;
      SetOwnerContext(AObject);
    except
      Ref.Free;
      raise;
    end;
  end;
end;

procedure TInstantParts.InternalMove(CurIndex, NewIndex: Integer);
begin
  if Metadata.StorageKind = skEmbedded then
    ObjectList.Move(CurIndex, NewIndex)
  else
    ObjectReferenceList.Move(CurIndex, NewIndex);
end;

procedure TInstantParts.InternalSetItems(Index: Integer;
  AValue: TInstantObject);
begin
  if Metadata.StorageKind = skEmbedded then
  begin
    ClearOwnerContext(ObjectList[Index] as TInstantObject);
    ObjectList[Index] := AValue;
    SetOwnerContext(AValue);
  end
  else
    ObjectReferences[Index].Instance := AValue;
end;

procedure TInstantParts.ReadObject(Reader: TInstantReader);
var
  Obj: TPersistent;
begin
  Clear;
  ReadName(Reader);
  Reader.ReadValue;
  while not Reader.EndOfList do
  begin
    Obj := Reader.ReadObject(nil, Connector);
    try
      Add(Obj as TInstantObject);
    except
      Obj.Free;
      raise;
    end;
  end;
  Reader.ReadListEnd;
end;

procedure TInstantParts.SetAllowOwned(Value: Boolean);
begin
  FAllowOwned := Value;
end;

procedure TInstantParts.SetObjectReferences(Index: Integer;
  Value: TInstantObjectReference);
begin
  ObjectReferenceList[Index] := Value;
end;

procedure TInstantParts.SetOwnerContext(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.SetOwnerContext(Owner, Self);
end;

procedure TInstantParts.Unchanged;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Pred(Count) do
    Items[I].Unchanged;
end;

procedure TInstantParts.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if Metadata.StorageKind = skEmbedded then
    if AObject.IsPersistent then
      raise EInstantValidationError.CreateFmt(SPersistentObjectNotAllowed,
        [AObject.ClassName, AObject.Id]);
  if Assigned(Owner) and (AObject = Owner) then
    raise EInstantError.CreateFmt(SOwnershipRecursion,
      [AObject.ClassName, AObject.Id]);
end;

procedure TInstantParts.WriteObject(Writer: TInstantWriter);
var
  I: Integer;
begin
  WriteName(Writer);
  Writer.WriteValue(vaCollection);
  for I := 0 to Pred(Count) do
    Writer.WriteObject(Items[I]);
  Writer.WriteListEnd;
end;

{ TInstantReferences }

procedure TInstantReferences.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;
  if Source is TInstantReferences then
  begin
    Clear;
    with TInstantReferences(Source) do
    begin
      for I := 0 to Pred(Count) do
      begin
        // cross-connector object assignment must be supported for InstantPump.
        if Connector <> Self.Connector then
          Self.Add(Items[I].Clone(Self.Connector))
        else
          Self.Add(Items[I]);
      end;
    end;
  end;
end;

class function TInstantReferences.AttributeType: TInstantAttributeType;
begin
  Result := atReferences;
end;

destructor TInstantReferences.Destroy;
begin
  FObjectReferenceList.Free;
  inherited;
end;

procedure TInstantReferences.DestroyObject(Index: Integer);
begin
  ObjectReferenceList.RefItems[Index].DestroyInstance;
end;

function TInstantReferences.DestroyObject(AObject: TInstantObject): Boolean;
var
  Index: Integer;
begin
  Index := ObjectReferenceList.IndexOfInstance(AObject);
  Result := Index >= 0;
  if Result then
    ObjectReferenceList.RefItems[Index].DestroyInstance;
end;

function TInstantReferences.GetAllowOwned: Boolean;
begin
  Result := FAllowOwned;
end;

function TInstantReferences.GetConnector: TInstantConnector;
begin
  if Assigned(FConnector) then
    Result := FConnector
  else
    Result := inherited GetConnector;
end;

function TInstantReferences.GetCount: Integer;
begin
  Result := ObjectReferenceList.Count;
end;

function TInstantReferences.GetInstances(Index: Integer): TInstantObject;
begin
  Result := RefItems[Index].Instance;
end;

function TInstantReferences.GetObjectReferenceList: TInstantObjectReferenceList;
begin
  if not Assigned(FObjectReferenceList) then
    FObjectReferenceList := TInstantObjectReferenceList.Create(
            True, Connector, Self);
  Result := FObjectReferenceList;
end;

function TInstantReferences.GetRefItems(Index: Integer): TInstantObjectReference;
begin
  Result := ObjectReferenceList.RefItems[Index];
end;

function TInstantReferences.HasInstance(Index: Integer): boolean;
begin
  Result := ObjectReferenceList.RefItems[Index].HasInstance;
end;

function TInstantReferences.InternalAdd(AObject: TInstantObject): Integer;
begin
  Result := ObjectReferenceList.Add(AObject);
end;

function TInstantReferences.InternalAddReference(const AObjectClassName,
    AObjectId: string): Integer;
var
  Ref: TInstantObjectReference;
begin
  Result := -1;

  if Metadata.StorageKind in [skExternal, skVirtual] then
  begin
    Ref := ObjectReferenceList.Add;
    try
      Ref.ReferenceObject(AObjectClassName, AObjectId);
      Result := ObjectReferenceList.IndexOfReference(Ref);
    except
      Ref.Free;
    end;
  end;
end;

procedure TInstantReferences.InternalClear;
begin
  ObjectReferenceList.Clear;
end;

procedure TInstantReferences.InternalDelete(Index: Integer);
begin
  ObjectReferenceList.Delete(Index);
end;

procedure TInstantReferences.InternalExchange(Index1, Index2: Integer);
begin
  ObjectReferenceList.Exchange(Index1, Index2);
end;

function TInstantReferences.InternalGetItems(Index: Integer): TInstantObject;
begin
  Result := ObjectReferenceList[Index];
end;

function TInstantReferences.InternalIndexOf(
  AObject: TInstantObject): Integer;
begin
  Result := ObjectReferenceList.IndexOf(AObject);
end;

function TInstantReferences.InternalIndexOfInstance(
  Instance: Pointer): Integer;
begin
  Result := ObjectReferenceList.IndexOf(TInstantObject(Instance));
end;

procedure TInstantReferences.InternalInsert(Index: Integer;
  AObject: TInstantObject);
begin
  ObjectReferenceList.Insert(Index, AObject);
end;

procedure TInstantReferences.InternalMove(CurIndex, NewIndex: Integer);
begin
  ObjectReferenceList.Move(CurIndex, NewIndex);
end;

procedure TInstantReferences.InternalSetItems(Index: Integer;
  AValue: TInstantObject);
begin
  ObjectReferenceList[Index] := AValue;
end;

procedure TInstantReferences.LoadObjectsFromStream(AStream: TStream);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Pred(Count) do
    if HasItem(I) then
      Items[I].Release;
end;

procedure TInstantReferences.LoadReferencesFromStream(AStream: TStream);
var
  Obj: TPersistent;
  ObjReference : TInstantObjectReference;
  Processor : TInstantXMLProcessor;
  ObjectEnd, ObjClassName, ObjId : string;
  XMLReferencesTag : string;
begin
  if not Assigned(AStream) then
    Exit;
  Clear;

  if Connector.BlobStreamFormat = sfBinary then
  begin
    while AStream.Position < AStream.Size do
    begin
      Obj := InstantReadObjectFromStream(AStream);
      try
        ObjReference := ObjectReferenceList.Add;
        try
          ObjReference.ReferenceObject(
                  TInstantObjectReference(Obj).ObjectClassName,
                  TInstantObjectReference(Obj).ObjectId);
        except
          ObjectReferenceList.Delete(Pred(Count));
          raise;
        end;
      finally
        Obj.Free;
      end;
    end;
  end
  else if Connector.BlobStreamFormat = sfXML then
  begin
    XMLReferencesTag := Self.ClassName;
    ObjectEnd := InstantBuildEndTag(XMLReferencesTag);
    Processor := TInstantXMLProcessor.Create(AStream);
    try
      //Skip XMLReferencesTag
      if not SameText(Processor.ReadTagName, XMLReferencesTag) then
        exit;
      while not SameText(Processor.PeekTag, ObjectEnd) do
      begin
        ObjClassName := Processor.ReadTagName; //Tag = classname
        ObjId := Processor.ReadData; //Data = ObjectId
        Processor.ReadTag; //closing tag
        ObjReference := ObjectReferenceList.Add;
        try
          ObjReference.ReferenceObject(ObjClassName, ObjId);
        except
          ObjectReferenceList.Delete(Pred(Count));
          raise;
        end;
      end;
    finally
      Processor.Free;
    end
  end;
  Changed;
end;

procedure TInstantReferences.ReadObject(Reader: TInstantReader);
var
  Ref: TInstantObjectReference;
begin
  Clear;
  ReadName(Reader);
  Reader.ReadValue;
  while not Reader.EndOfList do
  begin
    Ref := ObjectReferenceList.Add;
    try
      Ref.ReadAsObject(Reader);
    except
      ObjectReferenceList.Delete(Pred(Count));
      raise;
    end;
  end;
  Reader.ReadListEnd;
end;

procedure TInstantReferences.SaveReferencesToStream(AStream: TStream);
var
  I: Integer;
  InstantXMLProducer : TInstantXMLProducer;
  XMLReferencesTag : string;
begin
  if not Assigned(AStream) then
    Exit;

  if Connector.BlobStreamFormat = sfBinary then
  begin
    for I := 0 to Pred(Count) do
      InstantWriteObjectToStream(AStream, ObjectReferenceList.RefItems[I]);
  end
  else
  begin
    if Count = 0 then exit;
    InstantXMLProducer := TInstantXMLProducer.Create(AStream);
    Try
      XMLReferencesTag := Self.ClassName;
      InstantXMLProducer.WriteStartTag(XMLReferencesTag);
      InstantXMLProducer.WriteData(sLineBreak);
      for I := 0 to Pred(Count) do
      begin
        InstantXMLProducer.WriteStartTag(
                ObjectReferenceList.RefItems[I].ObjectClassName);
        InstantXMLProducer.WriteEscapedData(ObjectReferenceList.RefItems[I].ObjectId);
        InstantXMLProducer.WriteEndTag;
      end;
      InstantXMLProducer.WriteEndTag;
      InstantXMLProducer.WriteData(sLineBreak);
    Finally
      InstantXMLProducer.Free;
    End;
  end;
end;

procedure TInstantReferences.SetAllowOwned(Value: Boolean);
begin
  FAllowOwned := Value;
end;

procedure TInstantReferences.ValidateObject(AObject: TInstantObject);
begin
  inherited;
  if Assigned(AObject) then
    AObject.CheckId;
end;

procedure TInstantReferences.WriteObject(Writer: TInstantWriter);
var
  I: Integer;
begin
  WriteName(Writer);
  Writer.WriteValue(vaCollection);
  for I := 0 to Pred(Count) do
    ObjectReferenceList.RefItems[I].WriteAsObject(Writer);
  Writer.WriteListEnd;
end;

{ TInstantObjectState }

procedure TInstantObjectState.Assign(Source: TPersistent);
begin
  if Source is TInstantObjectState then
    with TInstantObjectState(Source) do
    begin
      Self.FIsChanged := FIsChanged;
      Self.FPersistentId := FPersistentId;
      Self.FUpdateCount := FUpdateCount;
    end
  else
    inherited;
end;

{ TInstantObject }

procedure TInstantObject.Abandon;
begin
  SetConnector(nil);
end;

function TInstantObject.AddObject(AObject: TInstantObject): Integer;
begin
  Result := DefaultContainer.Add(AObject);
end;

function TInstantObject.AddRef: Integer;
begin
  Inc(FRefCount);
  Result:= FRefCount;
  AfterAddRef;
end;

procedure TInstantObject.AfterAddRef;
begin
end;

procedure TInstantObject.AfterAssign;
begin
end;

procedure TInstantObject.AfterContentChange(Container: TInstantContainer;
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
end;

procedure TInstantObject.AfterCreate;
begin
end;

procedure TInstantObject.AfterCreateAttributes;
begin
end;

procedure TInstantObject.AfterDispose;
begin
end;

procedure TInstantObject.AfterRefresh;
begin
end;

procedure TInstantObject.AfterRetrieve;
begin
end;

procedure TInstantObject.AfterStore;
begin
end;

procedure TInstantObject.ApplyState;
begin
  if FSaveStateLevel = 0 then
    Exit;
  if FSaveStateLevel = 1 then
    try
      SavedState.Assign(State);
    except
      on E: Exception do
        raise EInstantError.CreateFmt(SFatalError, [ClassName, Id, E.Message]);
    end;
  Dec(FSaveStateLevel);
end;

procedure TInstantObject.Assign(Source: TPersistent);
var
  I: Integer;
  Obj: TInstantObject;
  Map: TInstantAttributeMap;
  AttribName: string;
  Attrib, SourceAttrib: TInstantAttribute;
begin
  BeforeAssign;
  inherited;
  if Source is ClassType then
  begin
    Obj := TInstantObject(Source);
    Map := Obj.Metadata.MemberMap;
    if Assigned(Map) then
      with Map do
        for I := 0 to Pred(Count) do
        begin
          AttribName := Items[I].Name;
          Attrib := FindAttribute(AttribName);
          if Assigned(Attrib) then
          begin
            SourceAttrib := Obj.AttributeByName(AttribName);
            Attrib.Assign(SourceAttrib);
          end;
        end;
  end;
  AfterAssign;
end;

function TInstantObject.AttributeAddress(
  const AttributeName: string): PInstantAttribute;
begin
  Result := FieldAddress(InstantAttributePrefix + AttributeName);
end;

function TInstantObject.AttributeByName(
  const AttributeName: string): TInstantAttribute;
begin
  Result := FindAttribute(AttributeName);
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SAttributeNotFound, [AttributeName,
      ClassName]);
end;

procedure TInstantObject.AttributeChanged(Attribute: TInstantAttribute);
begin
end;

procedure TInstantObject.BeforeAssign;
begin
end;

procedure TInstantObject.BeforeContentChange(Container: TInstantContainer;
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
end;

procedure TInstantObject.BeforeCreateAttributes;
begin
end;

procedure TInstantObject.BeforeDispose;
begin
end;

procedure TInstantObject.BeforeRefresh;
begin
end;

procedure TInstantObject.BeforeRelease;
begin
end;

procedure TInstantObject.BeforeStore;
begin
end;

function TInstantObject.CanDispose: Boolean;
begin
  if IsOwned then
    Result := Owner.CanDispose
  else
    Result := CanStore;
end;

function TInstantObject.CanStore: Boolean;
begin
  Result := True;
end;

procedure TInstantObject.Changed;
begin
  DoChange;
end;

function TInstantObject.ChangesDisabled: Boolean;
begin
  Result := FChangesDisabledCount <> 0;
end;

procedure TInstantObject.CheckId;
begin
  if not InstantIsValidObjectId(Id) then
  begin
    if FId = '' then
      FId := GenerateId;
    if not InstantIsValidObjectId(Id) then
      raise EInstantError.CreateFmt(SInvalidObjectId, [Id, ClassName]);
  end;
end;

{$IFDEF FPC}class{$ENDIF} function TInstantObject.ClassType: TInstantObjectClass;
begin
  Result := TInstantObjectClass(inherited ClassType);
end;

procedure TInstantObject.ClearObjects;
begin
  DefaultContainer.Clear;
end;

procedure TInstantObject.ClearOwnerContext;
begin
  SetOwnerContext(nil, nil);
end;

constructor TInstantObject.Clone(Source: TInstantObject;
  AConnector: TInstantConnector);
begin
  Create(AConnector);
  if Assigned(Source) then
  begin
    Assign(Source);
    if not Source.IsOwned then
      Id := Source.Id;
  end;
end;

function TInstantObject.Clone(
  AConnector: TInstantConnector = nil): TInstantObject;
begin
  if not Assigned(AConnector) then
    AConnector := Connector;
  Result := ClassType.Clone(Self, AConnector);
end;

function TInstantObject.ContainerByName(
  const ContainerName: string): TInstantContainer;
begin
  Result := FindContainer(ContainerName);
  if not Assigned(Result) then
    if ContainerName = '' then
      raise EInstantError.CreateFmt(SDefaultContainerNotFound,
        [ClassName])
    else
      raise EInstantError.CreateFmt(SContainerNotFound, [ContainerName,
        ClassName]);
end;

class procedure TInstantObject.ConvertToBinary(Converter: TInstantTextToBinaryConverter);

  procedure ConvertAttribute(AttributeMetadata: TInstantAttributeMetadata);
  var
    Count: Integer;
    LValue, AttributeName, AttributeEnd, Bin, Hex: string;
  begin
    with Converter do
    begin
      AttributeName := Processor.ReadTagName;
      Writer.WriteUTF8Str(AttributeName);
      case AttributeMetadata.AttributeType of
        atInteger:
          Writer.WriteInteger(StrToInt(Processor.ReadData));
        atFloat:
          Writer.WriteFloat(StrToFloat(Processor.ReadData));
        atCurrency:
          Writer.WriteCurrency(StrToCurr(Processor.ReadData));
        atBoolean:
          Writer.WriteBoolean(SameText(Processor.ReadData, InstantTrueString));
        atString, atMemo:
          Writer.WriteString(Processor.ReadData);
        atDateTime, atDate, atTime:
          Writer.WriteDate(InstantStrToDateTime(Processor.ReadData));
        atBlob, atGraphic:
          begin
            Hex := Processor.ReadData;
            Count := Length(Hex) div 2;
            SetLength(Bin, Count);
            HexToBin(PChar(Hex), PChar(Bin), Count);
            Writer.WriteValue(vaBinary);
            Writer.Write(Count, SizeOf(Count));
            Writer.Write(Bin[1], Count);
          end;
        atPart, atReference:
          begin
            Writer.WriteIdent('');
            Convert;
          end;
        atReferences, atParts:
          begin
            Writer.WriteValue(vaCollection);
            AttributeEnd := InstantBuildEndTag(AttributeName);
            while not SameText(Processor.PeekTag, AttributeEnd) do
              Convert;
            Writer.WriteListEnd;
          end;
        atEnum:
          begin
            LValue := Processor.ReadData;
            Writer.WriteInteger(StrToInt(LValue));
          end;
      end;
      Processor.ReadTag;
    end;
  end;

  procedure ConvertAttributes;
  var
    AttribMeta: TInstantAttributeMetadata;
    Map: TInstantAttributeMap;
    TagName, ObjectEnd: string;
  begin
    with Converter do
    begin
      ObjectEnd := InstantBuildEndTag(ObjectClassName);
      while not SameText(Processor.PeekTag, ObjectEnd) do
      begin
        InstantCheckClass(ObjectClass, TInstantObject);
        Map := TInstantObjectClass(ObjectClass).Metadata.MemberMap;
        if Assigned(Map) then
        begin
          TagName := Processor.PeekTagName;
          AttribMeta := Map.Find(TagName);
          if Assigned(AttribMeta) then
            ConvertAttribute(AttribMeta)
          else
            Processor.Skip;
        end else
          Processor.Skip;
      end;
      Writer.WriteListEnd;
    end;
  end;

  procedure ConvertId;
  begin
    with Converter do
    begin
      if Processor.PeekTagName = InstantIdFieldName then
      begin
        Processor.ReadTag;
        if (Processor.Token = xtAnyTag) and SameText(Processor.ReadTagName,
          InstantBuildEndTag(InstantIdFieldName)) then
          Writer.WriteStr('')
        else begin
          Writer.WriteUTF8Str(Processor.ReadData);
          Processor.ReadTag;
        end;
      end else
        Writer.WriteStr('');
    end;
  end;

begin
  ConvertId;
  ConvertAttributes;
end;

class procedure TInstantObject.ConvertToText(
  Converter: TInstantBinaryToTextConverter);

  procedure ConvertValue;
  var
    Count: Integer;
    Bin, Hex: string;
    LNextValueType: TValueType;
  begin
    with Converter do
    begin
      LNextValueType := Reader.NextValue;
      case LNextValueType of
        vaInt8, vaInt16, vaInt32:
          Producer.WriteData(IntToStr(Reader.ReadInteger));
        vaExtended:
          Producer.WriteData(FloatToStr(Reader.ReadFloat));
        vaDate:
          Producer.WriteData(InstantDateTimeToStr(Reader.ReadDate));
        vaString, vaLString, vaUTF8String, vaWString:
          Producer.WriteEscapedData(Reader.ReadString);
        vaCurrency:
          Producer.WriteEscapedData(CurrToStr(Reader.ReadCurrency));
        vaIdent:
          begin
            Reader.ReadIdent;
            Convert;
          end;
        vaFalse:
          begin
            Reader.ReadValue;
            Producer.WriteData(InstantFalseString);
          end;
        vaTrue:
          begin
            Reader.ReadValue;
            Producer.WriteData(InstantTrueString);
          end;
        vaBinary:
          begin
            Reader.ReadValue;
            Reader.Read(Count, SizeOf(Integer));
            SetLength(Bin, Count);
            SetLength(Hex, 2 * Count);
            Reader.Read(Bin[1], Count);
            BinToHex(PChar(Bin), PChar(Hex), Count);
            Producer.WriteData(Hex);
          end;
        vaCollection:
          begin
            Reader.ReadValue;
            while not Reader.EndOfList do
              Convert;
            Reader.ReadListEnd;
          end;
      else
        raise EInstantStreamError.CreateFmt(SInvalidValueType,
          [GetEnumName(TypeInfo(TValueType), Ord(LNextValueType))]);
      end;
    end;
  end;

  procedure ConvertAttributes;
  begin
    with Converter do
    begin
      while not Reader.EndOfList do
      begin
        Producer.WriteStartTag(Reader.ReadStr);
        ConvertValue;
        Producer.WriteEndTag;
      end;
      Reader.ReadListEnd;
    end;
  end;

  procedure ConvertId;
  var
    ObjectId: string;
  begin
    with Converter do
    begin
      ObjectId := Reader.ReadStr;
      if ObjectId = '' then
        Exit;
      Producer.WriteStartTag(InstantIdFieldName);
      Producer.WriteEscapedData(ObjectId);
      Producer.WriteEndTag;
    end;
  end;

begin
  ConvertId;
  ConvertAttributes;
end;

constructor TInstantObject.Create(AConnector: TInstantConnector = nil);
begin
  inherited Create;
  InstantCheckConnector(AConnector);
  SetConnector(AConnector);
  Init;
  DoAfterCreate;
end;

procedure TInstantObject.CreateAttributes;
var
  I: Integer;
  AttribMeta: TInstantAttributeMetadata;
  AttribPtr: PInstantAttribute;
begin
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
    begin
      AttribMeta := Items[I];
      AttribPtr := AttributeAddress(AttribMeta.Name);
      if Assigned(AttribPtr) then
        if Assigned(AttribPtr^) then
        begin
          if not Assigned(AttribPtr^.Metadata) then
            AttribPtr^.Metadata := AttribMeta;
          if not Assigned(AttribPtr^.Owner) then
            AttribPtr^.SetOwner(Self);
        end else
          AttribPtr^ := AttribMeta.CreateAttribute(Self) as TInstantAttribute;
    end;
end;

class function TInstantObject.CreateInstance(
  Arg: Pointer): TInstantStreamable;
begin
  if Assigned(Arg) and not (TObject(Arg) is TInstantConnector) then
    raise EInstantError.CreateFmt(SInvalidArgument, [ClassName,
      TInstantConnector.ClassName]);
  Result := Create(Arg);
end;

procedure TInstantObject.DeleteObject(Index: Integer);
begin
  DefaultContainer.Delete(Index);
end;

procedure TInstantObject.DestroyAttributes;
var
  I: Integer;
  Attrib: TInstantAttribute;
begin
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
    begin
      Attrib := FindAttribute(Items[I].Name);
      if Assigned(Attrib) and (Attrib.Owner = Self) then
        FreeAndNil(Attrib);
    end;
end;

procedure TInstantObject.DestroyInternalFields;
begin
  FreeAndNil(FRefBy);
  FreeAndNil(FState);
  FreeAndNil(FSavedState);
end;

procedure TInstantObject.Destruct;
begin
end;

procedure TInstantObject.DisableChanges;
begin
  Inc(FChangesDisabledCount);
end;

procedure TInstantObject.Dispose(ConflictAction: TInstantConflictAction);
begin
  PerformUpdate(DoDispose, otDispose, ConflictAction);
end;

procedure TInstantObject.DisposeOwnedObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction);
begin
  if not Assigned(AObject) or (AObject.Owner <> Self) then
    Exit;
  if AObject.OwnerAttribute.DetachObject(AObject) and IsPersistent then
    Store(ConflictAction);
end;

procedure TInstantObject.DoAfterContentChange(Container: TInstantContainer;
  ChangeType: TInstantContentChangeType; Index: Integer;
  AObject: TInstantObject);
begin
  if ChangesDisabled then
    Exit;
  AfterContentChange(Container, ChangeType, Index, AObject);
  if Assigned(FOnAfterContentChange) then
    FOnAfterContentChange(Self, Container, ChangeType, Index, AObject);
end;

procedure TInstantObject.DoAfterCreate;
begin
  AfterCreate;
  ObjectNotifiers.Notify(Self, onCreated);
end;

procedure TInstantObject.DoAfterDispose;
begin
  AfterDispose;
  ObjectNotifiers.Notify(Self, onDisposed);
end;

procedure TInstantObject.DoAfterRefresh;
begin
  AfterRefresh;
  ObjectNotifiers.Notify(Self, onRefreshed);
end;

procedure TInstantObject.DoAfterRetrieve;
begin
  AfterRetrieve;
  ObjectNotifiers.Notify(Self, onRetrieved);
end;

procedure TInstantObject.DoAfterStore;
begin
  AfterStore;
  ObjectNotifiers.Notify(Self, onStored);
end;

procedure TInstantObject.DoAttributeChanged(Attribute: TInstantAttribute);
begin
  if ChangesDisabled then
    Exit;
  AttributeChanged(Attribute);
  if Assigned(FOnAttributeChanged) then
    FOnAttributeChanged(Self, Attribute);
  if not ((Attribute is TInstantContainer) and TInstantContainer(Attribute).isVirtual) then
    Changed;
end;

procedure TInstantObject.DoBeforeContentChange(
  Container: TInstantContainer; ChangeType: TInstantContentChangeType;
  Index: Integer; AObject: TInstantObject);
begin
  if ChangesDisabled then
    Exit;
  BeforeContentChange(Container, ChangeType, Index, AObject);
  if Assigned(FOnBeforeContentChange) then
    FOnBeforeContentChange(Self, Container, ChangeType, Index, AObject);
end;

procedure TInstantObject.DoBeforeDispose;
begin
  BeforeDispose;
  if Assigned(FOnBeforeDispose) then
    FOnBeforeDispose(Self);
end;

procedure TInstantObject.DoBeforeRefresh;
begin
  BeforeRefresh;
end;

procedure TInstantObject.DoBeforeStore;
begin
  BeforeStore;
end;

procedure TInstantObject.DoChange;
begin
  if ChangesDisabled then
    Exit;
  State.IsChanged := True;
  Inc(FChangeCount);
  if IsOwned then
    Owner.DoOwnedObjectChanged(Self);
  if Assigned(FOnChange) then
    FOnChange(Self);
  ObjectNotifiers.Notify(Self, onChanged);
end;

procedure TInstantObject.DoDispose(ConflictAction: TInstantConflictAction);
begin
  if not IsPersistent or IsAbandoned then
    Exit;
  case VerifyOperation(otDispose) of
    vrCancel:
      Exit;
    vrAbort:
      Abort;
    vrError:
      raise EInstantError.CreateFmt(SDeniedDispose, [ClassName, Id]);
  end;
  DoBeforeDispose;
  if IsOwned then
    Owner.DisposeOwnedObject(Self, ConflictAction)
  else
  begin
    DisableChanges;
    try
      ObjectStore.DisposeObject(Self, ConflictAction);
    finally
      EnableChanges;
    end;
  end;
  DoAfterDispose;
end;

procedure TInstantObject.DoHandleError(OperationType: TInstantOperationType;
  E: Exception; var Action: TInstantErrorAction);
begin
  if Assigned(FOnError) then
    FOnError(Self, OperationType, E, Action)
  else
    HandleError(OperationType, E, Action);
end;

procedure TInstantObject.DoOwnedObjectChanged(AObject: TInstantObject);
begin
  if ChangesDisabled then
    Exit;
  Changed;
  OwnedObjectChanged(AObject);
end;

procedure TInstantObject.DoOwnerChanged(OldOwner: TInstantObject;
  OldOwnerAttribute: TInstantComplex);
begin
  OwnerChanged(OldOwner, OldOwnerAttribute);
end;

procedure TInstantObject.DoRefresh;
begin
  if not IsPersistent or IsAbandoned then
    Exit;
  case VerifyOperation(otRefresh) of
    vrCancel:
      Exit;
    vrAbort:
      Abort;
    vrError:
      raise EInstantError.CreateFmt(SDeniedRefresh, [ClassName, Id]);
  end;
  DoBeforeRefresh;
  DisableChanges;
  try
    ObjectStore.RefreshObject(Self);
  finally
    EnableChanges;
  end;
  Unchanged;
  DoAfterRefresh;
end;

function TInstantObject.DoRelease: Integer;
begin
  if FRefCount > 0 then
  begin
    BeforeRelease;
    Dec(FRefCount);
  end;
  Result:= FRefCount;
end;

procedure TInstantObject.DoStore(ConflictAction: TInstantConflictAction);
begin
  if (IsPersistent and not IsChanged) or IsAbandoned then
    Exit;
  case VerifyOperation(otStore) of
    vrCancel:
       Exit;
     vrAbort:
       Abort;
     vrError:
       raise EInstantError.CreateFmt(SDeniedStore, [ClassName, Id]);
  end;
  DoBeforeStore;
  if not IsOwned then
  begin
    CheckId;
    DisableChanges;
    try
      ObjectStore.StoreObject(Self, ConflictAction);
    finally
      EnableChanges;
    end;
    Unchanged;
    FChangeCount := 0;
  end;
  DoAfterStore;
end;

procedure TInstantObject.DoUnchange;
var
  I: Integer;
begin
  State.IsChanged := False;
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
      AttributeByName(Items[I].Name).Unchanged;
end;

procedure TInstantObject.EnableChanges;
begin
  if FChangesDisabledCount > 0 then
    Dec(FChangesDisabledCount);
end;

function TInstantObject.EqualsPersistentSignature(const AObjectClassName,
  AObjectId: string): Boolean;
begin
  Result := IsPersistent and (AObjectClassName = ClassName) and
    (AObjectId = PersistentId);
end;

function TInstantObject.EqualsSignature(const AObjectClassName,
  AObjectId: string): Boolean;
begin
  Result := (AObjectClassName = ClassName) and (AObjectId = Id);
end;

procedure TInstantObject.Finalize;
begin
end;

function TInstantObject.FindAttribute(
  const AttributeName: string): TInstantAttribute;
var
  AttribPtr: PInstantAttribute;
begin
  AttribPtr := AttributeAddress(AttributeName);
  if Assigned(AttribPtr) then
    Result := AttribPtr^
  else
    Result := nil;
end;

function TInstantObject.FindContainer(
  const ContainerName: string): TInstantContainer;
begin
  if ContainerName = '' then
    Result := FindDefaultContainer
  else
    Result := FindAttribute(ContainerName) as TInstantContainer;
end;

function TInstantObject.FindDefaultContainer: TInstantContainer;

  function FindDefaultContainerForClass(AClass: TInstantObjectClass): TInstantContainer;
  var
    ContainerAttributeName: String;
  begin
    ContainerAttributeName := AClass.Metadata.DefaultContainerName;
    if ContainerAttributeName = '' then
      Result := nil
    else
      Result := AttributeByName(ContainerAttributeName) as TInstantContainer;
  end;

var
  AClass: TInstantObjectClass;
begin
  AClass := ClassType;
  while AClass <> TInstantObject do
  begin
    Result := FindDefaultContainerForClass(AClass);
    if Assigned(Result) then
      Exit;
    AClass := TInstantObjectClass(AClass.Classparent);
  end;
  Result := nil;
end;

procedure TInstantObject.Finit;
begin
  try
    Finalize;
    if not IsPersistent then
      Destruct;
  except
    on E: Exception do
      if (E is EInstantError) or (E is EAbort) then
        raise
      else
        raise EInstantError.CreateFmt(SFinalizationFailed,
          [ClassName, Id, E.Message], E);
  end;
  if not IsAbandoned then
  begin
    if Connector.InTransaction then
      Connector.RemoveTransactedObject(Self);
    if IsPersistent then
      ObjectStore.ObjectDestroyed(Self);
  end;
  DestroyAttributes;
  DestroyInternalFields;
end;

procedure TInstantObject.ForEachAttribute(Proc: TInstantAttributeProc);
var
  LAttribute: TInstantAttribute;
  I, J: Integer;
  LMetadata: TInstantClassMetadata;
  LAttributeMetaData: TInstantAttributeMetadata;
  LContainer: TInstantContainer;
  LElement: TInstantElement;
  LObject: TInstantObject;
begin
  if not Assigned(Proc) then
    Exit;
  LMetadata := Metadata;
  //Ciclo su tutti gli attributi dell'oggetto
  for I := 0 to LMetadata.AttributeMetadatas.Count - 1 do
  begin
    LAttributeMetaData := LMetadata.AttributeMetadatas.Items[I];
    LAttribute := Self.FindAttribute(LAttributeMetaData.Name);
    if Assigned(LAttribute) then
    begin
      if LAttribute is TInstantContainer then
      begin
        LContainer := TInstantContainer(LAttribute);
        Proc(LAttribute, LContainer, 0);
        for J := 0 to LContainer.Count -1 do
        begin
          LObject := LContainer.GetItems(J);
          if Assigned(LObject) then
          begin
            LObject.ForEachAttribute(
              procedure(const AAttribute: TInstantAttribute;
                const AParentContainer: TInstantContainer;
                const AItemIndex: Integer)
              begin
                Proc(AAttribute, LContainer, J+1);
              end);
          end;
        end;
      end
      else
      begin
        Proc(LAttribute, nil, 0);
        if LAttribute is TInstantElement then
        begin
          LElement := TInstantElement(LAttribute);
          LObject := LElement.Value;
          if Assigned(LObject) then
            LObject.ForEachAttribute(
              procedure(const AAttribute: TInstantAttribute;
                const AParentContainer: TInstantContainer;
                const AItemIndex: Integer)
              begin
                Proc(AAttribute, AParentContainer, AItemIndex);
              end);
        end;
      end;
    end;
  end;
end;

{$IFNDEF IO_NO_CIRCULAR_REFERENCE_CHECK}
procedure TInstantObject.FreeCircularReferences;
var
  CheckedObjects: TObjectList;

  function IsInsideCircularReference(const AItem: TInstantComplex): Boolean;

    function CanUnassign(const AOwner: TInstantObject): Boolean;
    var
      CurrentItemOwner: TInstantObject;
      I: Integer;
    begin
      Result := True;
      for I := 0 to Pred(AOwner.RefByCount) do
        if AOwner.FRefBy[I] is TInstantComplex then
        begin
          CurrentItemOwner := TInstantComplex(AOwner.FRefBy[I]).Owner;
          Result := TInstantComplex(AOwner.FRefBy[0]).Owner = CurrentItemOwner;
          if not Result and Assigned(CurrentItemOwner) and
           (CurrentItemOwner.RefByCount = 1) then
            Result := (CurrentItemOwner.RefCount = 1) or
             ((CurrentItemOwner.RefCount = 2) and (CurrentItemOwner = Self));
          if not Result then
            Exit;
        end;
    end;

  var
    ItemOwner: TInstantObject;
    I: Integer;
  begin
    Result := Assigned(AItem) and Assigned(AItem.Owner);
    if not Result then
      Exit;
    ItemOwner := AItem.Owner;
    Result := (ItemOwner = Self) or
     (CheckedObjects.IndexOf(ItemOwner) > -1) or
     IsInsideCircularReference(ItemOwner.OwnerAttribute);
    if not Result and Assigned(ItemOwner.FRefBy) and CanUnassign(ItemOwner) then
    begin
      CheckedObjects.Add(ItemOwner);
      for I := 0 to Pred(ItemOwner.RefByCount) do
        if ItemOwner.FRefBy[I] is TInstantComplex then
        begin
          Result := (ItemOwner.RefCount = ItemOwner.RefByCount) and
           IsInsideCircularReference(TInstantComplex(ItemOwner.FRefBy[I]));
          if Result then
            Exit;
        end;
    end;
  end;

var
  I: Integer;
begin
  if RefByCount = RefCount - 1 then
  begin
    CheckedObjects := TObjectList.Create(False);
    try
      for I := Pred(RefByCount) downto 0 do
      begin
        if (FRefBy[I] is TInstantComplex) and
         IsInsideCircularReference(TInstantComplex(FRefBy[I])) then
        begin
          case TInstantComplex(FRefBy[I]).AttributeType of
            atReference:
              TInstantReference(FRefBy[I]).ObjectReference.DestroyInstance;
            atReferences:
              TInstantReferences(FRefBy[I]).DestroyObject(Self);
          end;
        end;
        CheckedObjects.Clear;
      end;
    finally
      CheckedObjects.Free;
    end;
  end;
end;
{$ENDIF}

procedure TInstantObject.FreeInstance;
begin
{$IFNDEF IO_NO_CIRCULAR_REFERENCE_CHECK}
  FreeCircularReferences;
{$ENDIF}
  DoRelease;
  if FRefCount = 0 then
    try
      Finit;
    finally
      inherited;
    end;
end;

function TInstantObject.GenerateId: string;
begin
  if IsAbandoned then
    Result := InstantGenerateId
  else
    Result := Connector.GenerateId(Self);
end;

function TInstantObject.GetCaption: string;
begin
  Result := Id;
end;

function TInstantObject.GetClassId: string;
begin
  Result := ClassName;
end;

function TInstantObject.GetDefaultContainer: TInstantContainer;
begin
  if not Assigned(FDefaultContainer) then
    FDefaultContainer := FindDefaultContainer;
  Result := FDefaultContainer;
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SDefaultContainerNotSpecified, [ClassName]);
end;

procedure TInstantObject.GetDetailsStatementValues(
  var FromClause, SequenceNoFieldName, OrderByClause: string);
begin
  //Default values for virtual containers details query
  SequenceNoFieldName := InstantUpdateCountFieldName;
  OrderByClause := InstantUpdateCountFieldName;
end;

function TInstantObject.GetHasDefaultContainer: Boolean;
begin
  Result := FindDefaultContainer <> nil;
end;

function TInstantObject.GetId: string;
begin
  Result := FId;
end;

class function TInstantObject.GetSerializedIdPropertyName: string;
begin
  Result := IO_SER_ID;
end;

function TInstantObject.GetIsAbandoned: Boolean;
begin
  Result := not Assigned(Connector);
end;

function TInstantObject.GetIsChanged: Boolean;
begin
  Result := State.IsChanged;
end;

function TInstantObject.GetIsDefault: Boolean;
var
  I: Integer;
begin
  Result := Id = '';
  if Result then
    with Metadata.MemberMap do
      for I := 0 to Pred(Count) do
      begin
        Result := AttributeByName(Items[I].Name).IsDefault;
        if not Result then
          Break;
      end;
end;

function TInstantObject.GetIsIdChanged: Boolean;
begin
  Result := Id <> PersistentId;
end;

function TInstantObject.GetIsOwned: Boolean;
begin
  Result := Assigned(FOwner);
end;

function TInstantObject.GetIsPersistent: Boolean;
begin
  Result := PersistentId <> '';
end;

function TInstantObject.GetNamePath: string;
begin
  Result := Format('%s.%s', [ClassName, Id]);
end;

function TInstantObject.GetObjectChangeCount: Integer;
begin
  Result := DefaultContainer.ChangeCount;
end;

function TInstantObject.GetObjectClass: TInstantAbstractObjectClass;
begin
  Result := DefaultContainer.RequiredClass;
end;

function TInstantObject.GetObjectCount: Integer;
begin
  Result := DefaultContainer.Count;
end;

function TInstantObject.GetObjects(Index: Integer): TInstantObject;
begin
  Result := DefaultContainer.Items[Index];
end;

function TInstantObject.GetObjectStore: TInstantObjectStore;
begin
  if not Assigned(FObjectStore) and not IsAbandoned then
    FObjectStore := Connector.EnsureObjectStore(ClassType);
  Result := FObjectStore;
end;

function TInstantObject.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TInstantObject.GetPersistentId: string;
begin
  Result := State.PersistentId;
end;

function TInstantObject.GetRefBy(Index: Integer): TInstantComplex;
begin
  Result := RefByList[Index] as TInstantComplex;
end;

function TInstantObject.GetRefByCount: Integer;
begin
  if Assigned(FRefBy) then
    Result := FRefBy.Count
  else
    Result := 0;
end;

function TInstantObject.GetRefByList: TObjectList;
begin
  if not Assigned(FRefBy) then
    FRefBy := TObjectList.Create(False);
  Result := FRefBy;
end;

function TInstantObject.GetSavedState: TInstantObjectState;
begin
  if not Assigned(FSavedState) then
    FSavedState := TInstantObjectState.Create;
  Result := FSavedState;
end;

function TInstantObject.GetState: TInstantObjectState;
begin
  if not Assigned(FState) then
    FState := TInstantObjectState.Create;
  Result := FState;
end;

function TInstantObject.GetUpdateCount: Integer;
begin
  Result := State.UpdateCount;
end;

procedure TInstantObject.HandleError(OperationType: TInstantOperationType;
  E: Exception; var Action: TInstantErrorAction);
begin
end;

function TInstantObject.IndexOfObject(AObject: TInstantObject): Integer;
begin
  Result := DefaultContainer.IndexOf(AObject);
end;

procedure TInstantObject.Init;
begin
  FRefCount := 1;
  BeforeCreateAttributes;
  CreateAttributes;
  AfterCreateAttributes;
  DisableChanges;
  try
    try
      Initialize;
    except
      on E: Exception do
        if (E is EInstantError) or (E is EAbort) then
          raise
        else
          raise EInstantError.CreateFmt(SInitializationFailed,
            [ClassName, E.Message], E);
    end;
  finally
    EnableChanges;
  end;
end;

procedure TInstantObject.Initialize;
begin
end;

procedure TInstantObject.InsertObject(Index: Integer; AObject: TInstantObject);
begin
  DefaultContainer.Insert(Index, AObject);
end;

function TInstantObject.Invoke(const MethodName: string;
  Params: TParams): string;
var
  Method: TInstantInvokableMethod;
  MethodAddr: Pointer;
begin
  MethodAddr := MethodAddress(MethodName);
  if Assigned(MethodAddr) then
  begin
    TMethod(Method).Code := MethodAddr;
    TMethod(Method).Data := Self;
    Result := Method(Params);
  end else
    Result := Format(SMethodNotFound, [MethodName, ClassName]);
end;

function TInstantObject.IsOperationAllowed(
  OperationType: TInstantOperationType): Boolean;
begin
  Result := VerifyOperation(OperationType) = vrOk;
end;

class function TInstantObject.Metadata: TInstantClassMetadata;
begin
  Result := InstantModel.ClassMetadatas.Find(ClassName);
  if not Assigned(Result) then
  begin
    Result := InstantModel.ClassMetadatas.Add;
    with Result do
    try
      Name := Self.ClassName;
    except
      InstantModel.ClassMetadatas.Remove(Result);
      raise;
    end;
  end;
end;

class function TInstantObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
end;

procedure TInstantObject.OwnedObjectChanged(AObject: TInstantObject);
begin
end;

procedure TInstantObject.OwnerChanged(OldOwner: TInstantObject;
  OldOwnerAttribute: TInstantComplex);
begin
end;

procedure TInstantObject.PerformUpdate(Operation: TInstantUpdateOperation;
 OperationType: TInstantOperationType; ConflictAction: TInstantConflictAction);
var
  Action: TInstantErrorAction;
  Transact: Boolean;
begin
  Transact := not Connector.InTransaction;
  if Transact then
    Connector.StartTransaction;
  SaveState;
  try
    Operation(ConflictAction);
    if Transact then
      Connector.CommitTransaction;
    ApplyState;
  except
    on E: Exception do
    begin
      RestoreState;
      if Transact then
        Connector.RollbackTransaction;
      if E is EAbort then
        raise
      else begin
        Action := eaError;
        DoHandleError(OperationType, E, Action);
        case Action of
          eaRetry:
            PerformUpdate(Operation, OperationType, ConflictAction);
          eaIgnore:
            PerformUpdate(Operation, OperationType, caFail);
          eaRevert:
            Refresh;
          eaCancel:
            ;
        else
          raise;
        end;
      end;
    end;
  end;
end;

function TInstantObject.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TInstantObject.ReadAttributes(Reader: TInstantReader);
var
  AttributeName: string;
  Attribute: TInstantAttribute;
  Pos: Integer;
begin
  with Reader do
  begin
    while not EndOfList do
    begin
      Pos := Position;
      AttributeName := ReadStr;
      Attribute := FindAttribute(AttributeName);
      if Assigned(Attribute) then
      begin
        Position := Pos;
        Attribute.ReadObject(Reader);
      end else
        Reader.SkipValue;
    end;
    ReadListEnd;
  end;
end;

procedure TInstantObject.ReadObject(Reader: TInstantReader);
begin
  DisableChanges;
  try
    Id := Reader.ReadStr;
    ReadAttributes(Reader);
  finally
    EnableChanges;
  end;
end;

class function TInstantObject.ReformatID(const Id: string): string;
begin
  Result := Id;
end;

procedure TInstantObject.Refresh;
var
  Action: TInstantErrorAction;
begin
  if IsOwned then
    Owner.Refresh
  else
    try
      SaveState;
      DoRefresh;
      ApplyState;
    except
      on E: Exception do
      begin
        RestoreState;
        if E is EAbort then
          raise
        else begin
          Action := eaError;
          DoHandleError(otRefresh, E, Action);
          case Action of
            eaRetry, eaRevert:
              Refresh;
            eaCancel, eaIgnore:
              ;
          else
            raise;
          end;
        end;
      end;
    end;
end;

class procedure TInstantObject.RefreshAll(AConnector: TInstantConnector;
  Progress: TInstantProgressEvent);
var
  I: Integer;
  Continue: Boolean;
  Store: TInstantObjectStore;
  Enumerator: TInstantCacheEnumerator;
  Node: TInstantCacheNode;
begin
  InstantCheckConnector(AConnector);
  Store := AConnector.EnsureObjectStore(Self);
  Enumerator := TInstantCacheEnumerator.Create(Store.FCache);
  try
    Continue := True;
    I := 0;
    Node := Enumerator.Next;
    while Assigned(Node) do
    begin
      Node.Item.Refresh;
      if Assigned(Progress) then
      begin
        Inc(I);
        Progress(Store, (I + 1) * 100 div Store.Count, Continue);
        if not Continue then
          Break;
      end;
      Node := Enumerator.Next;
    end;
  finally
    Enumerator.Free;
  end;
end;

function TInstantObject.Release: Integer;
begin
  Result := DoRelease;
end;

function TInstantObject.RemoveObject(AObject: TInstantObject): Integer;
begin
  Result := DefaultContainer.Remove(AObject);
end;

procedure TInstantObject.ResetAttributes;
var
  I: Integer;
begin
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
      AttributeByName(Items[I].Name).Reset;
end;

procedure TInstantObject.RestoreState;
var
  vInCache: Boolean;
begin
  if FSaveStateLevel = 0 then
    Exit;
  if (FSaveStateLevel = 1) and not IsAbandoned then
    try
      vInCache := ObjectStore.Find(Self.PersistentId) = Self;
      if vInCache then
      begin
        ObjectStore.RemoveFromCache(Self);
      end;
      State.Assign(SavedState);
      if vInCache and IsPersistent then
        ObjectStore.AddToCache(Self);
    except
      on E: Exception do
        raise EInstantError.CreateFmt(SFatalError, [ClassName, Id, E.Message]);
    end;
  Dec(FSaveStateLevel);
end;

{$O-}
constructor TInstantObject.Retrieve(const AObjectId: string; CreateIfMissing:
  Boolean = False; ARefresh: Boolean = False; AConnector: TComponent = nil;
  const AObjectData: TInstantAbstractObjectData = nil);

  procedure RetrieveDenied;
  begin
    raise EInstantError.CreateFmt(SDeniedRetrieve, [ClassName, AObjectId]);
  end;

var
  Instance: TInstantObject;
  Exists: Boolean;
  VerificationResult: TInstantVerificationResult;
begin
  inherited Create;

  InstantCheckConnector(TInstantConnector(AConnector));
  SetConnector(TInstantConnector(AConnector));

  Instance := ObjectStore.Find(AObjectId);
  if Assigned(Instance) then
  begin
    inherited FreeInstance;
    Self := Instance;
    AddRef;
    if ARefresh then
      Refresh;
  end
  else
  begin
    Init;
    Exists := ObjectStore.RetrieveObject(AObjectId, Self, AObjectData);
    VerificationResult := VerifyOperation(otRetrieve);
    case VerificationResult of
      vrAbort:
        Abort;
      vrError:
        RetrieveDenied;
    end;
    if Exists and (VerificationResult = vrOk) then
      DoAfterRetrieve
    else if CreateIfMissing and (VerificationResult = vrOk) then
    begin
      Id := AObjectId;
      DoAfterCreate;
    end else
    begin
      Destroy;
      Self := nil;
      {$IFNDEF WIN64}
      asm
        MOV     [EBP - $09], EAX // Avoid calling AfterConstruction
        POP     dword ptr fs:[$00000000]
        ADD     ESP, $0C
      end;
      {$ENDIF}
    end;
  end;
end;
{$O+}

function TInstantObject.GetConnector: TInstantConnector;
begin
  Result := inherited GetConnector as TInstantConnector;
end;

procedure TInstantObject.SaveState;
begin
  if State.PersistentId = '' then
    Exit;
  if FSaveStateLevel = 0 then
    try
      SavedState.Assign(State);
      if not IsAbandoned and Connector.InTransaction then
      begin
        Connector.AddTransactedObject(Self);
        Inc(FSaveStateLevel);
      end;
    except
      on E: Exception do
        raise EInstantError.CreateFmt(SFatalError, [ClassName, Id, E.Message]);
    end;
  Inc(FSaveStateLevel);
end;

procedure TInstantObject.SetConnector(AConnector: TInstantConnector);
begin
  inherited SetConnector(AConnector);
  FObjectStore := nil;
end;

procedure TInstantObject.SetId(const Value: string);
begin
  if Value <> FId then
  begin
    FId := Value;
    if Assigned(FOnIdChanged) then
      FOnIdChanged(Self, FId);
    Changed;
  end;
end;

procedure TInstantObject.SetIsChanged(Value: Boolean);
begin
  if Value <> IsChanged then
    if Value then
      Changed
    else
      Unchanged;
end;

procedure TInstantObject.SetObjects(Index: Integer; Value: TInstantObject);
begin
  DefaultContainer.Items[Index] := Value;
end;

procedure TInstantObject.SetOwnerContext(AOwner: TInstantObject;
  Attribute: TInstantComplex);
var
  OldOwner: TInstantObject;
  OldOwnerAttribute: TInstantComplex;
begin
  if (AOwner <> FOwner) or (Attribute <> FOwnerAttribute) then
  begin
    OldOwner := FOwner;
    OldOwnerAttribute := FOwnerAttribute;
    FOwner := AOwner;
    FOwnerAttribute := Attribute;
    DoOwnerChanged(OldOwner, OldOwnerAttribute);
  end;
end;

procedure TInstantObject.SetPersistentId(const Value: string);
begin
  State.PersistentId := Value;
  if Value = '' then
    Exit;
  DisableChanges;
  try
    Id := PersistentId;
  finally
    EnableChanges;
  end;
end;

procedure TInstantObject.SetUpdateCount(Value: Integer);
begin
  State.UpdateCount := Value;
end;

procedure TInstantObject.Store(ConflictAction: TInstantConflictAction);
begin
  PerformUpdate(DoStore, otStore, ConflictAction);
end;

procedure TInstantObject.Unchanged;
begin
  DoUnchange;
end;

function TInstantObject.VerifyOperation(
  OperationType: TInstantOperationType): TInstantVerificationResult;
begin
  Result := vrOk;
end;

procedure TInstantObject.WriteAttributes(Writer: TInstantWriter);
var
  I: Integer;
  Attrib: TInstantAttribute;
begin
  with Metadata.MemberMap do
    for I := 0 to Pred(Count) do
    begin
      Attrib := AttributeByName(Items[I].Name);
      if Attrib.IsMandatory or not Attrib.IsDefault then
        Attrib.WriteObject(Writer);
    end;
  Writer.WriteListEnd;
end;

procedure TInstantObject.WriteObject(Writer: TInstantWriter);
begin
  Writer.WriteUTF8Str(Id);
  WriteAttributes(Writer);
end;

function TInstantObject._AddRef: Integer;
begin
  Result := AddRef;
end;

function TInstantObject._Release: Integer;
begin
{$IFNDEF IO_NO_CIRCULAR_REFERENCE_CHECK}
  FreeCircularReferences;
{$ENDIF}
  Result := DoRelease;
  if FRefCount = 0 then
    try
      Finit;
    finally
      inherited FreeInstance;
    end;
end;

{ TInstantCacheNode }

constructor TInstantCacheNode.Create(AItem: TInstantObject; ALeaf,
  AParent: TInstantCacheNode; AColor: TInstantCacheNodeColor);
begin
  inherited Create;
  FItem := AItem;
  FParent := AParent;
  FLeft := ALeaf;
  FRight := ALeaf;
  Color := AColor;
end;

function TInstantCacheNode.GetSibling: TInstantCacheNode;
begin
  if IsLeftChild then
    Result := Parent.Right
  else
    Result := Parent.Left;
end;

function TInstantCacheNode.IsBlack: Boolean;
begin
  Result := Color = ncBlack;
end;

function TInstantCacheNode.IsLeftChild: Boolean;
begin
  Result := Assigned(Parent) and (Parent.Left = Self);
end;

function TInstantCacheNode.IsRed: Boolean;
begin
  Result := Color = ncRed;
end;

function TInstantCacheNode.IsRightChild: Boolean;
begin
  Result := Assigned(Parent) and (Parent.Right = Self);
end;

{ TInstantCache }

procedure TInstantCache.Add(AObject: TInstantObject);
var
  Node: TInstantCacheNode;
  ParentNode: TInstantCacheNode;
  Compare: Integer;
begin
  ParentNode := Leaf;
  Node := Root;
  while Node <> Leaf do
  begin
    ParentNode := Node;
    Compare := CompareObjects(AObject, Node.Item);
    if Compare < 0 then
      Node := Node.Left
    else if Compare > 0 then
      Node := Node.Right
    else
      Exit;
  end;

  Node := TInstantCacheNode.Create(AObject, Leaf, ParentNode, ncRed);
  try
    if ParentNode = Leaf then
      Root := Node
    else if CompareObjects(AObject, ParentNode.Item) < 0 then
      ParentNode.Left := Node
    else
      ParentNode.Right := Node;
  except
    Node.Free;
    raise;
  end;

  Rebalance(Node);
  Root.Color := ncBlack;
  Inc(FCount);
end;

procedure TInstantCache.Clear;

  procedure ClearBranch(Node: TInstantCacheNode);
  begin
    if Node <> Leaf then
    begin
      ClearBranch(Node.Left);
      ClearBranch(Node.Right);
      Dec(FCount);
      Node.Free;
    end;
  end;

begin
  ClearBranch(Root);
  Root := Leaf;
end;

function TInstantCache.CompareIds(const Id1, Id2: string): Integer;
begin
  Result := CompareStr(Id1, Id2);
end;

function TInstantCache.CompareObjects(Object1,
  Object2: TInstantObject): Integer;
begin
  Result := CompareIds(Object1.PersistentId, Object2.PersistentId);
end;

constructor TInstantCache.Create;
begin
  inherited Create;
  FLeaf := TInstantCacheNode.Create(nil, nil, nil, ncBlack);
  FRoot := FLeaf;
end;

procedure TInstantCache.DeleteFixup(ANode: TInstantCacheNode);
var
  Temp: TInstantCacheNode;
begin
  while (ANode <> Root) and ANode.IsBlack do
  begin
    if ANode.IsLeftChild then
    begin
      Temp := ANode.Sibling;
      if Temp.IsRed then
      begin
        Temp.Color := ncBlack;
        ANode.Parent.Color := ncRed;
        LeftRotate(ANode.Parent);
        Temp := ANode.Parent.Right;
      end;
      if Temp.Left.IsBlack and Temp.Right.IsBlack then
      begin
        Temp.Color := ncRed;
        ANode := ANode.Parent;
      end else
      begin
        if Temp.Right.IsBlack then
        begin
          Temp.Left.Color := ncBlack;
          Temp.Color := ncRed;
          RightRotate(Temp);
          Temp := ANode.Parent.Right;
        end;
        Temp.Color := ANode.Parent.Color;
        ANode.Parent.Color := ncBlack;
        Temp.Right.Color := ncBlack;
        LeftRotate(ANode.Parent);
        ANode := Root;
      end;
    end else
    begin
      Temp := ANode.Sibling;
      if Temp.IsRed then
      begin
        Temp.Color := ncBlack;
        ANode.Parent.Color := ncRed;
        RightRotate(ANode.Parent);
        Temp := ANode.Parent.Left;
      end;
      if Temp.Right.IsBlack and Temp.Left.IsBlack then
      begin
        Temp.Color := ncRed;
        ANode := ANode.Parent;
      end else
      begin
        if Temp.Left.IsBlack then
        begin
          Temp.Right.Color := ncBlack;
          Temp.Color := ncRed;
          LeftRotate(Temp);
          Temp := ANode.Parent.Left;
        end;
        Temp.Color := ANode.Parent.Color;
        ANode.Parent.Color := ncBlack;
        Temp.Left.Color := ncBlack;
        RightRotate(ANode.Parent);
        ANode := Root;
      end;
    end;
  end;
  ANode.Color := ncBlack;
end;

destructor TInstantCache.Destroy;
begin
  Clear;
  Leaf.Free;
  inherited;
end;

function TInstantCache.Find(const AObjectId: string): TInstantObject;
begin
  Result := FindNode(AObjectId).Item;
end;

function TInstantCache.FindNode(const AObjectId: string): TInstantCacheNode;
var
  Compare: Integer;
begin
  Result := Root;
  while Result <> Leaf do
  begin
    Compare := CompareIds(AObjectId, Result.Item.PersistentId);
    if Compare < 0 then
      Result := Result.Left
    else if Compare > 0 then
      Result := Result.Right
    else
      Exit;
  end;
end;

procedure TInstantCache.ForEach(Proc: TInstantObjectProc);
var
  Enumerator: TInstantCacheEnumerator;
  Node: TInstantCacheNode;
begin
  if not Assigned(Proc) then
    Exit;
  Enumerator := TInstantCacheEnumerator.Create(Self);
  try
    Node := Enumerator.Next;
    while Assigned(Node) do
    begin
      Proc(Node.Item);
      Node := Enumerator.Next;
    end;
  finally
    Enumerator.Free;
  end;
end;

function TInstantCache.GetItems(Index: Integer): TInstantObject;
var
  I: Integer;
  Enumerator: TInstantCacheEnumerator;
  Node: TInstantCacheNode;
begin
  if Index >= Count then
    raise EInstantError.CreateFmt(SIndexOutOfBounds, [Index]);
  Enumerator := TInstantCacheEnumerator.Create(Self);
  try
    I := 0;
    Node := Enumerator.Next;
    while I < Index do
    begin
      Node := Enumerator.Next;
      Inc(I);
    end;
    Result := Node.Item;
  finally
    Enumerator.Free;
  end;
end;

procedure TInstantCache.LeftRotate(ANode: TInstantCacheNode);
var
  Child: TInstantCacheNode;
begin
  Child := ANode.Right;

  ANode.Right := Child.Left;
  if Child.Left <> Leaf then
    Child.Left.Parent := ANode;

  Child.Parent := ANode.Parent;
  if ANode.Parent = Leaf then // ANode is root
    Root := Child
  else if ANode.IsLeftChild then
    ANode.Parent.Left := Child
  else
    ANode.Parent.Right := Child;

  Child.Left := ANode;
  ANode.Parent := Child;
end;

function TInstantCache.Pred(ANode: TInstantCacheNode): TInstantCacheNode;
begin
  if ANode.Left <> Leaf then
  begin
    Result := ANode.Left;
    while Result.Right <> Leaf do
      Result := Result.Right;
  end else
  begin
    while (ANode <> Root) and ANode.IsLeftChild do
      ANode := ANode.Parent;
    if ANode = Root then
      Result := Leaf
    else
      Result := ANode.Parent;
  end;
end;

procedure TInstantCache.Rebalance(ANode: TInstantCacheNode);
var
  Node: TInstantCacheNode;
  ParentSibling: TInstantCacheNode;
begin
  Node := ANode;
  while (Node <> Root) and Node.Parent.IsRed do
  begin
    if Node.Parent.IsLeftChild then
    begin
      ParentSibling := Node.Parent.Sibling;
      if ParentSibling.IsRed then
      begin
        Node.Parent.Color := ncBlack;
        ParentSibling.Color := ncBlack;
        Node.Parent.Parent.Color := ncRed;
        Node := Node.Parent.Parent;
      end else
      begin
        if Node.Parent.Right = Node then
        begin
          Node := Node.Parent;
          LeftRotate(Node);
        end;
        Node.Parent.Color := ncBlack;
        Node.Parent.Parent.Color := ncRed;
        RightRotate(Node.Parent.Parent);
      end;
    end else
    begin
      ParentSibling := Node.Parent.Sibling;
      if ParentSibling.IsRed then
      begin
        Node.Parent.Color := ncBlack;
        ParentSibling.Color := ncBlack;
        Node.Parent.Parent.Color := ncRed;
        Node := Node.Parent.Parent;
      end else
      begin
        if Node.Parent.Left = Node then
        begin
          Node := Node.Parent;
          RightRotate(Node);
        end;
        Node.Parent.Color := ncBlack;
        Node.Parent.Parent.Color := ncRed;
        LeftRotate(Node.Parent.Parent);
      end;
    end;
  end;
end;

procedure TInstantCache.Remove(AObject: TInstantObject);
var
  ANode, Delete, Child: TInstantCacheNode;
begin
  ANode := FindNode(AObject.PersistentId);
  if ANode = Leaf then
    Exit;

  if (ANode.Left = Leaf) or (ANode.Right = Leaf) then
    Delete := ANode
  else
    Delete := Succ(ANode);

  if Delete.Left <> Leaf then
    Child := Delete.Left
  else
    Child := Delete.Right;

  Child.Parent := Delete.Parent;
  if Delete.Parent = Leaf then
    Root := Child
  else if Delete = Delete.Parent.Left then
    Delete.Parent.Left := Child
  else
    Delete.Parent.Right := Child;

  if Delete <> ANode then
    ANode.Item := Delete.Item;

  if Delete.IsBlack then
    DeleteFixup(Child);

  Delete.Free;
  Dec(FCount);
end;

procedure TInstantCache.RightRotate(ANode: TInstantCacheNode);
var
  Child: TInstantCacheNode;
begin
  Child := ANode.Left;

  ANode.Left := Child.Right;
  if Child.Right <> Leaf then
    Child.Right.Parent := ANode;

  Child.Parent := ANode.Parent;
  if ANode.Parent = Leaf then // ANode is root.
    Root := Child
  else if ANode.IsLeftChild then
    ANode.Parent.Left := Child
  else
    ANode.Parent.Right := Child;

  Child.Right := ANode;
  ANode.Parent := Child;
end;

function TInstantCache.Succ(ANode: TInstantCacheNode): TInstantCacheNode;
begin
  if ANode.Right <> Leaf then
  begin
    Result := ANode.Right;
    while Result.Left <> Leaf do
      Result := Result.Left;
  end else
  begin
    while (ANode <> Root) and (ANode.IsRightChild) do
      ANode := ANode.Parent;
    if ANode = Root then
      Result := Leaf
    else
      Result := ANode.Parent;
  end;
end;

{ TInstantCacheEnumerator }

constructor TInstantCacheEnumerator.Create(ACache: TInstantCache);
begin
  inherited Create;
  FCache := ACache;
  FNode := Cache.Leaf;
end;

function TInstantCacheEnumerator.HasNext: Boolean;
begin
  if Node = nil then
    Result := False
  else if Node <> Cache.Leaf then
    Result := Cache.Succ(Node) <> Cache.Leaf
  else
    Result := not (Cache.Count = 0)
end;

function TInstantCacheEnumerator.Next: TInstantCacheNode;
begin
  if Node = nil then
  begin
    Result := nil;
    Exit;
  end;

  if Node = Cache.Leaf then
  begin
    Node := Cache.Root;
    while (Node <> Cache.Leaf) and (Node.Left <> Cache.Leaf) do
      Node := Node.Left;
  end else
    Node := Cache.Succ(Node);

  if Node <> Cache.Leaf then
    Result := Node
  else begin
    Result := nil;
    Node := nil; //Finished
  end;
end;

procedure TInstantCacheEnumerator.Reset;
begin
  Node := Cache.Leaf;
end;

{ TInstantObjectStore }

procedure TInstantObjectStore.AbandonObject(AObject: TInstantObject);
begin
  if Assigned(AObject) then
    AObject.Abandon;
end;

procedure TInstantObjectStore.AbandonObjects;
begin
  FCache.ForEach(AbandonObject);
end;

procedure TInstantObjectStore.AddToCache(AObject: TInstantObject);
begin
  FCache.Add(AObject);
  if Assigned(Connector) and (Count = 1) then
    Connector.RegisterClient(Self);
end;

procedure TInstantObjectStore.CheckBroker(ABroker: TInstantBroker);
begin
  if not Assigned(ABroker) then
    raise EInstantError.Create(SUnassignedBroker);
end;

constructor TInstantObjectStore.Create(Collection: TCollection);
begin
  inherited;
  FCache := TInstantCache.Create;
end;

destructor TInstantObjectStore.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TInstantObjectStore.DisposeObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction);
begin
  if not (Assigned(AObject) and AObject.Metadata.IsStored) then
    Exit;
  CheckBroker(Broker);
  try
    Broker.DisposeObject(AObject, ConflictAction);
    RemoveFromCache(AObject);
    AObject.SetPersistentId('');
    AObject.SetUpdateCount(0);
  except
    on E: Exception do
      if (E is EInstantError) or (E is EAbort) then
        raise
      else
        raise EInstantError.CreateFmt(SErrorDisposingObject,
          [AObject.ClassName, AObject.Id, E.Message], E);
  end;
end;

function TInstantObjectStore.Find(const AObjectId: string): TInstantObject;
begin
  Result := FCache.Find(AObjectId);
end;

function TInstantObjectStore.GetBroker: TInstantBroker;
begin
  if Assigned(Connector) then
    Result := Connector.Broker
  else
    Result := nil;
end;

function TInstantObjectStore.GetCount: Integer;
begin
  Result := FCache.Count;
end;

function TInstantObjectStore.GetItems(Index: Integer): TInstantObject;
begin
  Result := FCache.Items[Index];
end;

function TInstantObjectStore.GetName: string;
begin
  if Assigned(Connector) and Assigned(ObjectClass) then
    Result := Connector.Name + ObjectClass.ClassName
  else
    Result := '';
end;

procedure TInstantObjectStore.ObjectDestroyed(AObject: TInstantObject);
begin
  if Assigned(AObject) and AObject.IsPersistent then
    RemoveFromCache(AObject);
end;

procedure TInstantObjectStore.RefreshObject(AObject: TInstantObject);
begin
  if not (Assigned(AObject) and AObject.Metadata.IsStored) then
    Exit;
  CheckBroker(Broker);
  try
    if Broker.RetrieveObject(AObject, AObject.PersistentId, caFail) then
      AObject.SetPersistentId(AObject.PersistentId)
    else begin
      RemoveFromCache(AObject);
      AObject.ResetAttributes;
      AObject.SetPersistentId('');
      AObject.SetUpdateCount(0);
    end;
  except
    on E: Exception do
      if (E is EInstantError) or (E is EAbort) then
        raise
      else
        raise EInstantError.CreateFmt(SErrorRefreshingObject,
          [AObject.ClassName, AObject.Id, E.Message], E);
  end;
end;

procedure TInstantObjectStore.RemoveFromCache(AObject: TInstantObject);
begin
  FCache.Remove(AObject);
  if Assigned(Connector) and (Count = 0) then
    Connector.UnregisterClient(Self);
end;

function TInstantObjectStore.RetrieveObject(const AObjectId: string;
  const AObject: TInstantObject;
  const AObjectData: TInstantAbstractObjectData = nil): Boolean;
var
  LObjectId: string;
begin
  Result := Assigned(AObject) and AObject.Metadata.IsStored;
  if not Result then
    Exit;

  CheckBroker(Broker);
  try
    AObject.DisableChanges;
    try
      LObjectId := AObject.ReformatID(AObjectId);
      Result := Broker.RetrieveObject(AObject, LObjectId, caFail, AObjectData);
      if Result then
        AObject.SetPersistentId(LObjectId)
      else begin
        AObject.SetPersistentId('');
        AObject.SetUpdateCount(0);
      end
    finally
      AObject.EnableChanges;
    end;
    if Result then
      AddToCache(AObject);
  except
    on E: Exception do
      if (E is EInstantError) or (E is EAbort) then
        raise
      else
        raise EInstantError.CreateFmt(SErrorRetrievingObject,
          [AObject.ClassName, LObjectId, E.Message], E);
  end;
end;

procedure TInstantObjectStore.StoreObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction);
var
  MustAdd: Boolean;
begin
  if not (Assigned(AObject) and AObject.Metadata.IsStored) then
    Exit;
  CheckBroker(Broker);
  AObject.InUpdate := True;
  try
    try
      if Broker.StoreObject(AObject, ConflictAction) then
      begin
        if AObject.IsPersistent then
        begin
          MustAdd := AObject.Id <> AObject.PersistentId;
          if MustAdd then
            RemoveFromCache(AObject);
        end else
          MustAdd := True;
        AObject.SetPersistentId(AObject.Id);
        if MustAdd then
          AddToCache(AObject);
      end;
    except
      on E: Exception do
        if (E is EInstantError) or (E is EAbort) then
          raise
        else
          raise EInstantError.CreateFmt(SErrorStoringObject,
            [AObject.ClassName, AObject.Id, E.Message], E);
    end;
  finally
    AObject.InUpdate := False;
  end;
end;

{ TInstantObjectStores }

function TInstantObjectStores.AddObjectStore: TInstantObjectStore;
begin
  Result := TInstantObjectStore(inherited Add)
end;

constructor TInstantObjectStores.Create;
begin
  inherited Create(TInstantObjectStore);
end;

function TInstantObjectStores.FindObjectStore(
  AClass: TInstantObjectClass): TInstantObjectStore;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := Items[I];
    if Result.ObjectClass = AClass then
      Exit;
  end;
  Result := nil;
end;

function TInstantObjectStores.GetItems(Index: Integer): TInstantObjectStore;
begin
  Result := TInstantObjectStore(inherited Items[Index]);
end;

procedure TInstantObjectStores.SetItems(Index: Integer;
  Value: TInstantObjectStore);
begin
  Items[Index] := Value;
end;

{ TInstantQuery }

function TInstantQuery.AddObject(AObject: TObject): Integer;
begin
  Result := InternalAddObject(AObject);
end;

procedure TInstantQuery.ApplyChanges;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    if ObjectFetched(I) and (Objects[I] is TInstantObject) then
      TInstantObject(Objects[I]).Store;
end;

procedure TInstantQuery.Close;
begin
  InternalClose;
end;

constructor TInstantQuery.Create(AConnector: TInstantConnector);
begin
  inherited Create(nil);
  FConnector := AConnector;
end;

procedure TInstantQuery.FetchParams(const ACommandText: string;
  AParams: TParams);
begin
  InternalFetchParams(AParams);
end;

function TInstantQuery.GetActive: Boolean;
begin
  Result := False;
end;

function TInstantQuery.GetConnector: TInstantConnector;
begin
  Result := FConnector;
end;

function TInstantQuery.GetObjectClass: TClass;
begin
  Result := FindClass(ObjectClassName);
end;

function TInstantQuery.GetObjectClassName: string;
begin
  with TInstantQueryCommand.Create(nil) do
  try
    Text := Self.Command;
    Result := ResultClassName;
  finally
    Free;
  end;
end;

function TInstantQuery.GetObjectCount: Integer;
begin
  Result := InternalGetObjectCount;
  if (MaxCount <> 0) and (Result > MaxCount) then
    Result := MaxCount;
end;

{$IFDEF D17+}
procedure TInstantQuery.GetObjectList(const AList: TList<TInstantObject>);
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    AList.Add(Objects[I] as TInstantObject);
end;
{$ENDIF}

function TInstantQuery.GetObjects(Index: Integer): TObject;
begin
  Result := InternalGetObjects(Index);
  if not Assigned(Result) then
    raise EInstantAccessError.CreateFmt(SErrorRetrievingObject,
      [ObjectClassName, InternalGetObjectReferenceId(Index),
      SObjectNotAvailable]);
end;

function TInstantQuery.GetParams: TParams;
begin
  Result := nil;
end;

function TInstantQuery.HasChangedObject: Boolean;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    if ObjectFetched(I) and (Objects[I] is TInstantObject) then
    begin
      Result := TInstantObject(Objects[I]).IsChanged;
      if Result then
        Exit;
    end;
  Result := False;
end;

function TInstantQuery.HasObject(AObject: TObject): Boolean;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    if ObjectFetched(I) then
    begin
      Result := Objects[I] = AObject;
      if Result then
        Exit;
    end;
  Result := False;
end;

function TInstantQuery.IndexOfObject(AObject: TObject): Integer;
begin
  Result := InternalIndexOfObject(AObject);
end;

procedure TInstantQuery.InsertObject(Index: Integer; AObject: TObject);
begin
  InternalInsertObject(Index, AObject);
end;

function TInstantQuery.InternalAddObject(AObject: TObject): Integer;
begin
  Result := -1;
end;

procedure TInstantQuery.InternalClose;
begin
end;

procedure TInstantQuery.InternalFetchParams(AParams: TParams);

  procedure FetchParamsForObject(AObject: TInstantIQLObject);
  var
    I: Integer;
    Obj: TInstantIQLObject;
    ParamName: string;
  begin
    if Assigned(AObject) then
      for I := 0 to Pred(AObject.ObjectCount) do
      begin
        Obj := AObject.Objects[I];
        if Obj is TInstantIQLParam then
        begin
          ParamName := TInstantIQLParam(Obj).ParamName;
          if not Assigned(AParams.FindParam(ParamName)) then
            AParams.CreateParam(ftUnknown,ParamName, ptInput)
        end;
        FetchParamsForObject(Obj);
      end;
  end;

var
  ACommand: TInstantIQLCommand;
begin
  if Assigned(AParams) then
  begin
    ACommand := TInstantQueryCommand.Create(nil);
    try
      ACommand.Text := Self.Command;
      FetchParamsForObject(ACommand);
    finally
      ACommand.Free;
    end;
  end;
end;

function TInstantQuery.InternalGetObjectCount: Integer;
begin
  Result := 0;
end;

function TInstantQuery.InternalGetObjectReferenceId(Index: Integer): string;
begin
  Result := 'Query.Object[' + IntToStr(Index) + ']';
end;

function TInstantQuery.InternalGetObjects(Index: Integer): TObject;
begin
  Result := nil;
end;

function TInstantQuery.InternalIndexOfObject(AObject: TObject): Integer;
begin
  Result := -1;
end;

procedure TInstantQuery.InternalInsertObject(Index: Integer;
  AObject: TObject);
begin
end;

procedure TInstantQuery.InternalOpen;
begin
end;

procedure TInstantQuery.InternalRefreshObjects;
begin
end;

procedure TInstantQuery.InternalReleaseObject(AObject: TObject);
begin
end;

function TInstantQuery.InternalRemoveObject(AObject: TObject): Integer;
begin
  Result := -1;
end;

function TInstantQuery.ObjectFetched(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TInstantQuery.Open;
begin
  if Command <> '' then
    TranslateCommand;
  InternalOpen;
end;

procedure TInstantQuery.RefreshObjects;
begin
  InternalRefreshObjects;
end;

procedure TInstantQuery.ReleaseObject(AObject: TObject);
begin
  InternalReleaseObject(AObject);
end;

function TInstantQuery.RemoveObject(AObject: TObject): Integer;
begin
  Result := InternalRemoveObject(AObject);
end;

procedure TInstantQuery.SetActive(Value: Boolean);
begin
  if Value <> Active then
    if Value then
      Open
    else
      Close;
end;

procedure TInstantQuery.SetActualLoadMode(const AValue: TInstantLoadMode);
begin
  FActualLoadMode := AValue;
end;

procedure TInstantQuery.SetCommand(const Value: string);
begin
  if Value <> FCommand then
  begin
    FCommand := Value;
    TranslateCommand;
  end;
end;

procedure TInstantQuery.SetParams(Value: TParams);
begin
end;

procedure TInstantQuery.TranslateCommand;
begin
end;

{ TInstantObjectNotifier }

constructor TInstantObjectNotifier.Create(AObjectClass: TInstantObjectClass;
  IncludeOwned: Boolean; AOwner: TInstantObjectNotifiers);
begin
  FObjectClass := AObjectClass;
  FIncludeOwned := IncludeOwned;
  FOwner := AOwner;
  Owner.Add(Self);
end;

destructor TInstantObjectNotifier.Destroy;
begin
  Owner.Extract(Self);
  inherited;
end;

procedure TInstantObjectNotifier.Disable;
begin
  Inc(FDisableCount);
end;

function TInstantObjectNotifier.Disabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TInstantObjectNotifier.DoObjectChanged(AObject: TInstantObject);
begin
  if Assigned(FOnObjectChanged) then
    FOnObjectChanged(AObject);
  ObjectChanged(AObject);
end;

procedure TInstantObjectNotifier.DoObjectCreated(AObject: TInstantObject);
begin
  if Assigned(FOnObjectCreated) then
    FOnObjectCreated(AObject);
  ObjectCreated(AObject);
end;

procedure TInstantObjectNotifier.DoObjectDisposed(AObject: TInstantObject);
begin
  if Assigned(FOnObjectDisposed) then
    FOnObjectDisposed(AObject);
  ObjectDisposed(AObject);
end;

procedure TInstantObjectNotifier.DoObjectRefreshed(AObject: TInstantObject);
begin
  if Assigned(FOnObjectRefreshed) then
    FOnObjectRefreshed(AObject);
  ObjectRefreshed(AObject);
end;

procedure TInstantObjectNotifier.DoObjectRetrieved(AObject: TInstantObject);
begin
  if Assigned(FOnObjectRetrieved) then
    FOnObjectRetrieved(AObject);
  ObjectRetrieved(AObject);
end;

procedure TInstantObjectNotifier.DoObjectStored(AObject: TInstantObject);
begin
  if Assigned(FOnObjectStored) then
    FOnObjectStored(AObject);
  ObjectStored(AObject);
end;

procedure TInstantObjectNotifier.Enable;
begin
  if Disabled then
    Dec(FDisableCount);
end;

function TInstantObjectNotifier.GetObjectClass: TInstantObjectClass;
begin
  if Assigned(FObjectClass) then
    Result := FObjectClass
  else
    Result := TInstantObject;
end;

function TInstantObjectNotifier.GetOwner: TInstantObjectNotifiers;
begin
  if Assigned(FOwner) then
    Result := FOwner
  else
    Result := ObjectNotifiers;
end;

function TInstantObjectNotifier.IncludeObject(AObject: TInstantObject): Boolean;
begin
  Result := not AObject.IsOwned or (AObject.IsOwned = IncludeOwned);
end;

procedure TInstantObjectNotifier.Notify(AObject: TInstantObject;
  Notification: TInstantObjectNotification);
begin
  if Disabled or not (AObject is ObjectClass) or not IncludeObject(AObject) then
    Exit;
  if Assigned(FOnNotify) then
    FOnNotify(AObject, Notification);
  case Notification of
    onCreated:
      DoObjectCreated(AObject);
    onChanged:
      DoObjectChanged(AObject);
    onDisposed:
      DoObjectDisposed(AObject);
    onRefreshed:
      DoObjectRefreshed(AObject);
    onRetrieved:
      DoObjectRetrieved(AObject);
    onStored:
      DoObjectStored(AObject);
  end;
end;

procedure TInstantObjectNotifier.ObjectChanged(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectCreated(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectDisposed(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectRefreshed(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectRetrieved(AObject: TInstantObject);
begin
end;

procedure TInstantObjectNotifier.ObjectStored(AObject: TInstantObject);
begin
end;

{ TInstantObjectNotifiers }

function TInstantObjectNotifiers.AcceptItem(
  Item: TInstantObjectNotifier): Boolean;
begin
  Result := True;
end;

function TInstantObjectNotifiers.Add(
  Item: TInstantObjectNotifier): Integer;
begin
  if AcceptItem(Item) then
    Result := ItemList.Add(Item)
  else
    Result := -1;
end;

procedure TInstantObjectNotifiers.BeginNotify;
begin
  Inc(FNotifyingCount);
end;

constructor TInstantObjectNotifiers.Create(AObjectClass: TInstantObjectClass);
begin
  inherited Create;
  FObjectClass := AObjectClass;
end;

destructor TInstantObjectNotifiers.Destroy;
begin
  FItemList.Free;
  inherited;
end;

procedure TInstantObjectNotifiers.Disable;
begin
  Inc(FDisableCount);
end;

function TInstantObjectNotifiers.Disabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TInstantObjectNotifiers.Enable;
begin
  if Disabled then
    Dec(FDisableCount);
end;

procedure TInstantObjectNotifiers.EndNotify;
begin
  if IsNotifying then
  begin
    Dec(FNotifyingCount);
    if not IsNotifying then
      ItemList.Pack;
  end;
end;

procedure TInstantObjectNotifiers.Extract(Item: TInstantObjectNotifier);
var
  Index: Integer;
begin
  if IsNotifying then
  begin
    Index := ItemList.IndexOf(Item);
    if Index <> -1 then
      ItemList.Insert(Index, nil);
  end;
  ItemList.Extract(Item);
end;

function TInstantObjectNotifiers.GetCount: Integer;
begin
  Result := ItemList.Count;
end;

function TInstantObjectNotifiers.GetIsNotifying: Boolean;
begin
  Result := FNotifyingCount <> 0;
end;

function TInstantObjectNotifiers.GetItemList: TList;
begin
  if not Assigned(FItemList) then
    FItemList := TObjectList.Create;
  Result := FItemList;
end;

function TInstantObjectNotifiers.GetItems(
  Index: Integer): TInstantObjectNotifier;
begin
  Result := ItemList[Index];
end;

function TInstantObjectNotifiers.GetObjectClass: TInstantObjectClass;
begin
  if Assigned(FObjectClass) then
    Result := FObjectClass
  else
    Result := TInstantObject;
end;

procedure TInstantObjectNotifiers.Notify(AObject: TInstantObject;
  Notification: TInstantObjectNotification);
var
  I: Integer;
begin
  if Disabled or not (AObject is ObjectClass) then
    Exit;
  BeginNotify;
  try
    for I := 0 to Pred(Count) do
      if Assigned(Items[I]) then
        Items[I].Notify(AObject, Notification);
  finally
    EndNotify;
  end;
end;

function TInstantObjectNotifiers.Remove(Item: TInstantObjectNotifier): Integer;
begin
  if IsNotifying then
  begin
    Result := ItemList.IndexOf(Item);
    if Result <> -1 then
      ItemList[Result] := nil;
  end else
    Result := ItemList.Remove(Item);
end;

{ TInstantProgressLoop }

constructor TInstantProgressLoop.Create(ALow, AHigh: Integer;
  ProgressEvent: TInstantProgressEvent);
begin
  FLow := ALow;
  FHigh := AHigh;
  FStep := FLow;
  FOnProgress := ProgressEvent;
end;

function TInstantProgressLoop.Done: Boolean;
begin
  Result := FStep > FHigh;
end;

procedure TInstantProgressLoop.Next;
begin
  Inc(FStep);
end;

procedure TInstantProgressLoop.Progress(Sender: TObject; Count: Integer;
  var Continue: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Count div (FHigh - FStep + 1), Continue);
end;


procedure LoadClassMetadatas;

  function HasModelResource(AInstance: Cardinal): Boolean;
  begin
    Result := (AInstance <> 0) and (FindResource(AInstance,
      PChar(InstantModelResourceName), RT_RCDATA) <> 0);
  end;

  procedure LoadModelFromResource(AInstance: Cardinal);
  begin
    with TInstantResourceStream.Create(AInstance, InstantModelResourceName,
      RT_RCDATA) do
    try
      ReadObject(InstantModel.ClassMetadatas);
    finally
      Free;
    end;
  end;

var
  Instance: Cardinal;
{$IFNDEF FPC}
  LibModule: PLibModule;
{$ENDIF}
begin
  if HasModelResource(HInstance)then
    LoadModelFromResource(HInstance)
  else if HasModelResource(MainInstance) then
    LoadModelFromResource(MainInstance)
  else begin
{$IFNDEF FPC}
    LibModule := LibModuleList;
    while LibModule <> nil do
    begin
      Instance := LibModule.Instance;
      if (Instance <> HInstance) and (Instance <> MainInstance) and
        HasModelResource(Instance) then
      begin
        LoadModelFromResource(Instance);
        Exit;
      end;
      LibModule := LibModule.Next;
    end;
{$ENDIF}
  end;
end;

{ TInstantGraphic }

class function TInstantGraphic.AttributeType: TInstantAttributeType;
begin
  Result := atGraphic;
end;

{ TInstantDBBuildCommand }

constructor TInstantDBBuildCommand.Create(
  const ACommandType: TInstantDBBuildCommandType;
  const AConnector: TInstantConnector = nil);
const
  // Default values for the Enabled property
  // depending on the CommandType.
  InstantDBBuildCommandEnabledDefaults: array[TInstantDBBuildCommandType] of Boolean =
    (True, False, True, True, True, True, True, False);
begin
  inherited Create;
  FCommandType := ACommandType;
  FConnector := AConnector;
  FEnabled := InstantDBBuildCommandEnabledDefaults[FCommandType];
end;

destructor TInstantDBBuildCommand.Destroy;
begin
  inherited;
end;

procedure TInstantDBBuildCommand.Execute;
begin
  try
    InternalExecute;
  except
    on E: Exception do
      if not InternalExecuteHandleError(E) then
        raise;
  end;
end;

function TInstantDBBuildCommand.GetConnector: TInstantConnector;
begin
  if Assigned(FConnector) then
    Result := FConnector
  else
  {$IFNDEF MARS_FIREDAC}
    Result := InstantDefaultConnector;
  {$ELSE}
    Result := nil;
  {$ENDIF}
end;

function TInstantDBBuildCommand.GetDescription: string;
begin
  Result := ClassName + ' [' + GetEnumName(TypeInfo(TInstantDBBuildCommandType), Ord(FCommandType));
  if Assigned(OldMetadata) then
    Result := Result + ', ' + OldMetadata.Name;
  if Assigned(NewMetadata) then
    Result := Result + ', ' + NewMetadata.Name;
  Result := Result + ']';
end;

function TInstantDBBuildCommand.InternalExecuteHandleError(
  const E: Exception): Boolean;
begin
  Result := False;
end;

{ TInstantUnsupportedDBBuildCommand }

procedure TInstantUnsupportedDBBuildCommand.InternalExecute;
begin
  raise EInstantDBBuildError.CreateFmt(SCannotBuildDB, [Description]);
end;

{ TObjectReferenceList }

constructor TInstantObjectReferenceList.Create(ARefOwnsInstance: Boolean; AConnector:
    TInstantConnector; ARefOwner: TInstantComplex);
begin
  inherited Create;
  FList := TObjectList.Create;
  FRefOwnsInstance := ARefOwnsInstance;
  FRefOwner := ARefOwner;
  FConnector := AConnector;
end;

destructor TInstantObjectReferenceList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TInstantObjectReferenceList.Add: TInstantObjectReference;
begin
  Result := CreateObjectReference(nil);
  try
    FList.Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TInstantObjectReferenceList.Add(Item: TInstantObject): Integer;
var
  Ref: TInstantObjectReference;
begin
  Ref := CreateObjectReference(Item);
  try
    Result := FList.Add(Ref);
  except
    Ref.Free;
    raise;
  end;
end;

procedure TInstantObjectReferenceList.Clear;
begin
  FList.Clear;
end;

function TInstantObjectReferenceList.CreateObjectReference(AObject: TInstantObject):
    TInstantObjectReference;
begin
  Result := TInstantObjectReference.Create(nil, FRefOwnsInstance, FRefOwner);
  if Assigned(AObject) then
  begin
    if FRefOwnsInstance then
      Result.Instance := AObject
    else
      Result.ReferenceObject(AObject.ClassName, AObject.Id);
  end;
end;

procedure TInstantObjectReferenceList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TInstantObjectReferenceList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TInstantObjectReferenceList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TInstantObjectReferenceList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TInstantObjectReferenceList.GetItems(Index: Integer): TInstantObject;
var
  Ref: TInstantObjectReference;
begin
  Ref := FList.Items[Index] as TInstantObjectReference;
  Result := Ref.Dereference(FConnector, FRefOwnsInstance);
end;

function TInstantObjectReferenceList.GetRefItems(Index: Integer):
    TInstantObjectReference;
begin
  Result := FList.Items[Index] as TInstantObjectReference;
end;

function TInstantObjectReferenceList.IndexOf(Item: TInstantObject;
  NeedInstance: Boolean = False): Integer;
var
  Ref: TInstantObjectReference;
begin
  for Result := 0 to Pred(Count) do
  begin
    Ref := RefItems[Result];
    if Ref.Equals(Item) and (not NeedInstance or Ref.HasInstance) then
      Exit;
  end;
  Result := -1;
end;

function TInstantObjectReferenceList.IndexOfInstance(Item: TInstantObject): Integer;
begin
  Result := IndexOf(Item, True);
end;

function TInstantObjectReferenceList.IndexOfReference(AObjectReference:
    TInstantObjectReference): Integer;
begin
  Result := FList.IndexOf(AObjectReference);
end;

procedure TInstantObjectReferenceList.Insert(Index: Integer; Item: TInstantObject);
var
  Ref: TInstantObjectReference;
begin
  Ref := CreateObjectReference(Item);
  try
    FList.Insert(Index, Ref);
  except
    Ref.Free;
    raise;
  end;
end;

procedure TInstantObjectReferenceList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

function TInstantObjectReferenceList.Remove(Item: TInstantObject): Integer;
begin
  Result := IndexOf(Item);
  if Result > -1 then
    FList.Delete(Result);
end;

procedure TInstantObjectReferenceList.SetCapacity(Value: Integer);
begin
  FList.Capacity := Value;
end;

procedure TInstantObjectReferenceList.SetItems(Index: Integer; const Value:
    TInstantObject);
var
  Ref: TInstantObjectReference;
begin
  Ref := CreateObjectReference(Value);
  try
    FList.Items[Index] := Ref;
  except
    Ref.Free;
    raise;
  end;
end;


{ TInstantBroker }

constructor TInstantBroker.Create(AConnector: TInstantConnector);
begin
  inherited Create;
  FConnector := AConnector;
end;

destructor TInstantBroker.Destroy;
begin
  inherited;
end;

procedure TInstantBroker.BuildDatabase(Scheme: TInstantScheme);
begin
  InternalBuildDatabase(Scheme);
end;

function TInstantBroker.CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog;
begin
  Result := nil;
end;

function TInstantBroker.CreateDBBuildCommand(
  const CommandType: TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  Result := TInstantUnsupportedDBBuildCommand.Create(CommandType,
    Connector);
end;

function TInstantBroker.CreateQuery: TInstantQuery;
begin
  Result := InternalCreateQuery;
end;

function TInstantBroker.DisposeObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := InternalDisposeObject(AObject, ConflictAction);
end;

class function TInstantBroker.GetCatalogClass: TInstantCatalogClass;
begin
  Result := nil;
end;

function TInstantBroker.GetConnector: TInstantConnector;
begin
  Result := FConnector;
  if not Assigned(Result) then
    raise EInstantError.Create(SUnassignedConnector);
end;

function TInstantBroker.GetDatabaseName: string;
begin
  Result := '';
end;

procedure TInstantBroker.InternalBuildDatabase(Scheme: TInstantScheme);
begin
end;

function TInstantBroker.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantQuery.Create(Connector);
end;

function TInstantBroker.IsCatalogSupported: Boolean;
var
  LCatalog: TInstantCatalog;
begin
  LCatalog := CreateCatalog(nil);
  try
    Result := Assigned(LCatalog);
  finally
    LCatalog.Free;
  end;
end;

function TInstantBroker.ReadDatabaseScheme(
  const AWarningEventHandler: TInstantWarningEvent): TInstantScheme;
begin
  Result := TInstantScheme.Create;
  try
    Result.OnWarning := AWarningEventHandler;
    Result.Catalog := CreateCatalog(Result);
    if Result.Catalog = nil then
      raise Exception.CreateFmt(SUndefinedCatalog, [ClassName]);
  except
    Result.Free;
    raise;
  end;
end;

function TInstantBroker.RetrieveObject(AObject: TInstantObject;
  const AObjectId: string; ConflictAction: TInstantConflictAction;
  const AObjectData: TInstantAbstractObjectData = nil): Boolean;
begin
  Result := InternalRetrieveObject(AObject, AObjectId, ConflictAction, AObjectData);
end;

procedure TInstantBroker.SetObjectUpdateCount(AObject: TInstantObject;
  Value: Integer);
begin
  if Assigned(AObject) then
    AObject.SetUpdateCount(Value);
end;

function TInstantBroker.StoreObject(AObject: TInstantObject;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := InternalStoreObject(AObject, ConflictAction);
end;

constructor TInstantConnector.Create(AOwner: TComponent);
begin
  inherited;
  FUseTransactions := True;
  FUseUnicode := False;
  FIdDataType := dtString;
  FIdSize := InstantDefaultFieldSize;
end;

destructor TInstantConnector.Destroy;
begin
  AbandonObjects;
  IsDefault := False;
  FClientList.Free;
  FBroker.Free;
  FObjectStores.Free;
  FTransactedObjectList.Free;
  inherited;
end;

{ TInstantConnector }

procedure TInstantConnector.AbandonObjects;
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to Pred(ClientCount) do
  begin
    Obj := Clients[I];
    if Obj is TInstantObjectStore then
      TInstantObjectStore(Obj).AbandonObjects;
  end;
end;

function TInstantConnector.AddTransactedObject(
  AObject: TInstantObject): Integer;
begin
  if Assigned(AObject) then
    Result := TransactedObjectList.Add(AObject)
  else
    Result := -1;
end;

procedure TInstantConnector.ApplyTransactedObjectStates;
var
  I: Integer;
begin
  for I := 0 to Pred(TransactedObjectCount) do
    TransactedObjects[I].ApplyState;
end;

procedure TInstantConnector.BuildDatabase(
  AClasses: array of TInstantObjectClass);
var
  I: Integer;
  Model: TInstantModel;
begin
  Model := TInstantModel.Create;
  try
    for I := Low(AClasses) to High(AClasses) do
      if Assigned(AClasses[I]) then
        Model.ClassMetadatas.Add.Assign(AClasses[I].Metadata);
    BuildDatabase(Model);
  finally
    Model.Free;
  end;
end;

procedure TInstantConnector.BuildDatabase(Model: TInstantModel);
var
  Scheme: TInstantScheme;
begin
  CreateDatabase;
  Connect;
  try
    if Model = nil then
      Model := InstantModel;
    Scheme := CreateScheme(Model);
    try
      DoBeforeBuildDatabase(Scheme);
      InternalBuildDatabase(Scheme);
    finally
      Scheme.Free;
    end;
  finally
    Disconnect;
  end;
end;

procedure TInstantConnector.ClearTransactedObjects;
begin
  TransactedObjectList.Clear;
end;

procedure TInstantConnector.CommitTransaction;
begin
  if not InTransaction then
    Exit;
  Dec(FTransactionLevel);
  if FTransactionLevel = 0 then
    try
      InternalCommitTransaction;
      ApplyTransactedObjectStates;
    finally
      ClearTransactedObjects;
    end;
end;

procedure TInstantConnector.Connect;
begin
  InternalConnect;
end;

procedure TInstantConnector.CreateDatabase;
begin
  if Connected or DatabaseExists then
    Exit;
  InternalCreateDatabase;
end;

function TInstantConnector.CreateQuery: TInstantQuery;
begin
  Result := InternalCreateQuery;
  if not Assigned(Result) then
    raise EInstantError.Create(SCapabilityNotSuppported);
end;

function TInstantConnector.CreateScheme(Model: TInstantModel): TInstantScheme;
begin
  Result := InternalCreateScheme(Model);
end;

procedure TInstantConnector.Disconnect;
begin
  InternalDisconnect;
end;

procedure TInstantConnector.DoBeforeBuildDatabase(Scheme: TInstantScheme);
begin
  if Assigned(FBeforeBuildDatabase) then
    FBeforeBuildDatabase(Self, Scheme);
end;

function TInstantConnector.EnsureObjectStore(
  AClass: TInstantObjectClass): TInstantObjectStore;
begin
  Result := ObjectStores.FindObjectStore(AClass);
  if not Assigned(Result) then
  begin
    Result := ObjectStores.AddObjectStore;
    with Result do
    begin
      Connector := Self;
      ObjectClass := AClass;
    end;
  end;
end;

function TInstantConnector.GenerateId(const AObject: TInstantObject = nil): string;
begin
  Result := InternalGenerateId(AObject);
end;

function TInstantConnector.GetBroker: TInstantBroker;
begin
  if not Assigned(FBroker) then
    FBroker := CreateBroker;
  Result := FBroker;
end;

function TInstantConnector.GetClient(Index: Integer): TObject;
begin
  Result := ClientList[Index];
end;

function TInstantConnector.GetClientCount: Integer;
begin
  if (assigned(FClientList)) then
    Result := FClientList.Count
  else
    Result := 0;
end;

function TInstantConnector.GetClientList: TList;
begin
  if not Assigned(FClientList) then
    FClientList := TList.Create;
  Result := FClientList;
end;

function TInstantConnector.GetConnected: Boolean;
begin
  Result := False;
end;

function TInstantConnector.GetDatabaseExists: Boolean;
begin
  Result := True;
end;

function TInstantConnector.GetDatabaseName: string;
begin
  Result := Broker.DatabaseName;
end;

function TInstantConnector.GetDDLTransactionSupported: Boolean;
begin
  Result := True;
end;

function TInstantConnector.GetInTransaction: Boolean;
begin
  Result := FTransactionLevel > 0;
end;

function TInstantConnector.GetInUse: Boolean;
begin
  Result := ClientCount > 0;
end;

function TInstantConnector.GetIsDefault: Boolean;
begin
  Result := Self = DefaultConnector;
end;

function TInstantConnector.GetObjectCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(ObjectStores.Count) do
    Inc(Result, ObjectStores[I].Count);
end;

function TInstantConnector.GetObjects(Index: Integer): TInstantObject;
var
  I, EndIndex, Count: Integer;
  ObjectStore: TInstantObjectStore;
begin
  EndIndex := -1;
  for I := 0 to Pred(ObjectStores.Count) do
  begin
    ObjectStore := ObjectStores[I];
    Count := ObjectStore.Count;
    EndIndex := EndIndex + Count;
    if Index <= EndIndex then
    begin
      Result := ObjectStore.Items[Pred(Count - (EndIndex - Index))];
      Exit;
    end;
  end;
  raise EInstantError.CreateFmt(SIndexOutOfBounds, [Index]);
end;

function TInstantConnector.GetObjectStores: TInstantObjectStores;
begin
  if not Assigned(FObjectStores) then
    FObjectStores := TInstantObjectStores.Create;
  Result := FObjectStores;
end;

function TInstantConnector.GetTransactedObjectCount: Integer;
begin
  Result := TransactedObjectList.Count;
end;

function TInstantConnector.GetTransactedObjectList: TList;
begin
  if not Assigned(FTransactedObjectList) then
    FTransactedObjectList := TList.Create;
  Result := FTransactedObjectList;
end;

function TInstantConnector.GetTransactedObjects(Index: Integer): TInstantObject;
begin
  Result := TransactedObjectList[Index];
end;

function TInstantConnector.GetUseUnicode: Boolean;
begin
  Result := FUseUnicode;
end;

procedure TInstantConnector.InternalBuildDatabase(Scheme: TInstantScheme);
begin
  if Assigned(Scheme) then
  begin
    Scheme.BlobStreamFormat := BlobStreamFormat;
    Scheme.IdDataType := IdDataType;
    Scheme.IdSize := IdSize;
  end;
  Broker.BuildDatabase(Scheme);
end;

procedure TInstantConnector.InternalCommitTransaction;
begin
end;

procedure TInstantConnector.InternalCreateDatabase;
begin
end;

function TInstantConnector.InternalCreateQuery: TInstantQuery;
begin
  Result := Broker.CreateQuery;
end;

function TInstantConnector.InternalGenerateId(const AObject: TInstantObject = nil): string;
begin
  if Assigned(FOnGenerateId) then
  begin
    Result := '';
    FOnGenerateId(Self, AObject, Result);
  end
  else
    Result := InstantGenerateId;
end;

procedure TInstantConnector.InternalRollbackTransaction;
begin
end;

procedure TInstantConnector.InternalStartTransaction;
begin
end;

class procedure TInstantConnector.RegisterClass;
begin
  if not Assigned(ConnectorClasses) then
    ConnectorClasses := TList.Create;
  ConnectorClasses.Add(Self);
end;

procedure TInstantConnector.RegisterClient(Client: TObject);
begin
  if not Assigned(Client) then
    Exit;
  ClientList.Add(Client);
end;

function TInstantConnector.RemoveTransactedObject(
  AObject: TInstantObject): Integer;
begin
  if Assigned(AObject) then
    Result := TransactedObjectList.Remove(AObject)
  else
    Result := -1
end;

procedure TInstantConnector.RestoreTransactedObjectStates;
var
  I: Integer;
begin
  for I := 0 to Pred(TransactedObjectCount) do
    TransactedObjects[I].RestoreState;
end;

procedure TInstantConnector.RollbackTransaction;
begin
  if not InTransaction then
    Exit;
  Dec(FTransactionLevel);
  if FTransactionLevel = 0 then
    try
      InternalRollbackTransaction;
      RestoreTransactedObjectStates;
    finally
      ClearTransactedObjects;
    end;
end;

procedure TInstantConnector.SetConnected(Value: Boolean);
begin
  if Value then
    Connect
  else
    Disconnect;
end;

procedure TInstantConnector.SetIsDefault(const Value: Boolean);
begin
  if Value <> IsDefault then
    if Value then
      DefaultConnector := Self
    else
      DefaultConnector := nil;
end;

procedure TInstantConnector.SetUseUnicode(const Value: Boolean);
begin
  FUseUnicode := Value;
end;

procedure TInstantConnector.StartTransaction;
begin
  if not UseTransactions then
    Exit;
  Inc(FTransactionLevel);
  if FTransactionLevel = 1 then
    InternalStartTransaction;
end;

class procedure TInstantConnector.UnregisterClass;
begin
  ConnectorClasses.Remove(Self);
end;

procedure TInstantConnector.UnregisterClient(Client: TObject);
begin
  if not Assigned(Client) then
    Exit;
  ClientList.Remove(Client);
end;

{ TInstantConnectionDef }

constructor TInstantConnectionDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FBlobStreamFormat := sfBinary;
  FIdDataType := dtString;
  FIdSize := InstantDefaultFieldSize;
  FUseUnicode := False;
  FDefaultStatementCacheCapacity := 0;
end;

function TInstantConnectionDef.CreateConnector(AOwner: TComponent): TInstantConnector;
begin
  Result := ConnectorClass.Create(AOwner);
  try
    InitConnector(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TInstantConnectionDef.GetCaption: string;
var
  Connector: TInstantConnector;
begin
  Connector := CreateConnector(nil);
  try
    if Assigned(Connector) then
      Result := Connector.DatabaseName
    else
      Result := ''
  finally
    Connector.Free;
  end;
end;

procedure TInstantConnectionDef.InitConnector(Connector: TInstantConnector);
begin
  Connector.BlobStreamFormat := BlobStreamFormat;
  Connector.IdDataType := IdDataType;
  Connector.IdSize := IdSize;
  Connector.UseUnicode := UseUnicode;
  Connector.DefaultStatementCacheCapacity := DefaultStatementCacheCapacity;
end;

procedure TInstantConnectionDef.TestConnection;
var
  LConnector: TInstantConnector;
begin
  LConnector := CreateConnector(nil);
  try
    LConnector.Connect;
    LConnector.Disconnect;
  finally
    LConnector.Free;
  end;
end;

{ TInstantConnectionDefs }

constructor TInstantConnectionDefs.Create;
begin
  inherited Create(TInstantConnectionDef);
end;

function TInstantConnectionDefs.GetItems(Index: Integer): TInstantConnectionDef;
begin
  Result := inherited Items[Index] as TInstantConnectionDef;
end;

procedure TInstantConnectionDefs.SetItems(Index: Integer;
  const Value: TInstantConnectionDef);
begin
  inherited Items[Index] := Value;
end;

{ TInstantDate }

class function TInstantDate.AttributeType: TInstantAttributeType;
begin
  Result := atDate;
end;

function TInstantDate.GetAsDate: TDateTime;
begin
  Result := Value;
end;

procedure TInstantDate.SetAsDate(AValue: TDateTime);
begin
  Value := DateOf(AValue);
end;

procedure TInstantDate.SetValue(AValue: TDateTime);
begin
  if AValue <> FValue then
  begin
    FValue := DateOf(AValue);
    Changed;
  end;
end;

{ TInstantTime }

class function TInstantTime.AttributeType: TInstantAttributeType;
begin
  Result := atTime;
end;

function TInstantTime.GetAsTime: TDateTime;
begin
  Result := Value;
end;

function TInstantTime.GetAsString: string;
begin
  Result := TimeToStr(Value);
end;

procedure TInstantTime.SetAsTime(AValue: TDateTime);
begin
  Value := TimeOf(AValue);
end;

procedure TInstantTime.SetValue(AValue: TDateTime);
begin
  if AValue <> FValue then
  begin
    FValue := TimeOf(AValue);
    Changed;
  end;
end;


{ TInstantObjectListEnumerator }
{$IFDEF D17+}
constructor TInstantObjectListEnumerator.Create(AList: TObjectList<TInstantObject>);
begin
  FIndex := -1;
  FList := AList;
end;

function TInstantObjectListEnumerator.GetCurrent;
begin
  Result := FList[FIndex];
end;

function TInstantObjectListEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TInstantObjectList<T> }

function TInstantObjectList<T>.GetEnumerator: TInstantObjectListEnumerator;
begin
  Result := TInstantObjectListEnumerator.Create(Self);
end;
{$ENDIF}

initialization
  RegisterClasses([TInstantClassMetadatas, TInstantClassMetadata,
    TInstantAttributeMetadatas, TInstantAttributeMetadata,
    TInstantObjectReference, TInstantConnectionDefs, TInstantConnectionDef]);
{$IFNDEF IO_CONSOLE}
  GraphicClassList[gffIco] := Graphics.TIcon;
  GraphicClassList[gffBmp] := Graphics.TBitmap;
  {$IFNDEF FPC}
    GraphicClassList[gffEmf] := Graphics.TMetaFile;
  {$ENDIF}
{$ENDIF}
  LoadClassMetadatas;
  ObjectNotifiers := TInstantObjectNotifiers.Create;

finalization
  ClassList.Free;
  ConnectorClasses.Free;
  RuntimeModel.Free;
  ObjectNotifiers.Free;

end.

