(*
 *   InstantObjects
 *   ObjectPascal Code Parser/Producer
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
 * Uberto Barbini, Joao Morais, Riceball Lee, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantCode;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  System.Types, Classes, Contnrs, SysUtils,
  InstantPersistence, InstantClasses, InstantMetadata, InstantTextFiler,
  InstantTypes, TypInfo;

type
  TInstantCodePos = TInstantTextPos;

  TInstantCodeModuleType = (mtProgram, mtUnit, mtLibrary);
  TInstantCodeScope = (scInterface, scImplementation);

  TInstantCodeProject = class;
  TInstantCodeModule = class;
  TInstantCodeReader = class;
  TInstantCodeWriter = class;

  TInstantCodeErrorSeverity = (esWarning, esFatal);

  EInstantCodeError = class(Exception)
  private
    FFileName: string;
    FPosition: TInstantCodePos;
    FReader: TInstantCodeReader;
    FSeverity: TInstantCodeErrorSeverity;
  public
    constructor Create(const Msg: string; AReader: TInstantCodeReader);
    property FileName: string read FFileName write FFileName;
    property Position: TInstantCodePos read FPosition;
    property Severity: TInstantCodeErrorSeverity read FSeverity;
  end;

  TInstantCodeObject = class;
  TInstantCodeObjectClass = class of TInstantCodeObject;

  TInstantCodeFilterEvent = procedure(Sender: TInstantCodeObject;
    var Include: Boolean; Arg: Pointer) of object;

  TInstantCodeObject = class(TPersistent)
  private
    FEndPos: TInstantCodePos;
    FLockCount: Integer;
    FName: string;
    FObjectList: TObjectList;
    FOwner: TInstantCodeObject;
    FStartPos: TInstantCodePos;
    function GetAsString: string;
    function GetIsFiled: Boolean;
    function GetObjectCount: Integer;
    function GetObjectList: TList;
    function GetObjects(Index: Integer): TInstantCodeObject;
    function GetProject: TInstantCodeProject;
    procedure SetAsString(const Value: string);
    property ObjectList: TList read GetObjectList;
  protected
    function AddObject(AObject: TInstantCodeObject): Integer;
    procedure DeleteObject(AObject: TInstantCodeObject);
    procedure ExtractObject(AObject: TInstantCodeObject);
    function FindNearest(List: TList; Instance: TInstantCodeObject;
      out Prior, Next: TObject;
      Filter: TInstantCodeFilterEvent = nil; Arg: Pointer = nil;
      Compare: TStringListSortCompare = nil): Boolean;
    function GetIsEmpty: Boolean; virtual;
    function GetModule: TInstantCodeModule; virtual;
    function GetName: string; virtual;
    procedure InsertObject(Index: Integer; AObject: TInstantCodeObject);
    class function InternalAtIdentifier(Reader: TInstantCodeReader): Boolean; virtual;
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; virtual;
    procedure InternalChanged; virtual;
    procedure InternalClear; virtual;
    procedure InternalRead(Reader: TInstantCodeReader); virtual;
    procedure InternalWrite(Writer: TInstantCodeWriter); virtual;
    class function IsIdentifier(const Str: string): Boolean; virtual;
    procedure ReadObjects(Reader: TInstantCodeReader; Classes: array of TInstantCodeObjectClass);
    procedure SetName(const Value: string); virtual;
    procedure SortByName(List: TList);
    procedure WriteToString(Writer: TInstantCodeWriter); virtual;
    procedure WriteObjects(Writer: TInstantCodeWriter);
  public
    constructor Create(AOwner: TInstantCodeObject); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function AtIdentifier(Reader: TInstantCodeReader): Boolean;
    class function AtInstance(Reader: TInstantCodeReader): Boolean; overload;
    class function AtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; overload;
    procedure Changed;
    procedure Clear;
    procedure Delete; virtual;
    function Find(Name: string): TInstantCodeObject;
    function FindFirst(AClass: TInstantCodeObjectClass): TInstantCodeObject;
    function FindLast(AClass: TInstantCodeObjectClass): TInstantCodeObject;
    procedure Fixup; virtual;
    class function Identifier: string; virtual;
    function IsLocked: Boolean;
    procedure Lock;
    procedure Move(Origin, Delta: TInstantCodePos; MoveOrigin: Boolean); virtual;
    procedure Organize; virtual;
    procedure Read(Reader: TInstantCodeReader);
    procedure ReadRes(Stream: TInstantStream); virtual;
    procedure Reset; virtual;
    procedure Sort; virtual;
    procedure Write(Writer: TInstantCodeWriter);
    procedure Unlock;
    property AsString: string read GetAsString write SetAsString;
    property EndPos: TInstantCodePos read FEndPos write FEndPos;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFiled: Boolean read GetIsFiled;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[Index: Integer]: TInstantCodeObject read GetObjects;
    property Owner: TInstantCodeObject read FOwner;
    property Project: TInstantCodeProject read GetProject;
    property StartPos: TInstantCodePos read FStartPos write FStartPos;
  published
    property Module: TInstantCodeModule read GetModule;
    property Name: string read GetName write SetName;
  end;

  TInstantCodeObjectLink = class(TObject)
  private
    FInstance: TInstantCodeObject;
    FName: string;
    FOwner: TInstantCodeObject;
    function GetInstance: TInstantCodeObject;
    function GetModule: TInstantCodeModule;
    function GetName: string;
    procedure SetInstance(Value: TInstantCodeObject);
    procedure SetName(const Value: string);
  protected
    procedure AssignInstance(Value: TInstantCodeObject); virtual;
    procedure Attach; virtual;
    procedure Detach; virtual;
    function FindInstance: TInstantCodeObject; virtual;
  public
    constructor Create(AOwner: TInstantCodeObject);
    procedure Resolve;
    property Instance: TInstantCodeObject read GetInstance write SetInstance;
    property Module: TInstantCodeModule read GetModule;
    property Name: string read GetName write SetName;
    property Owner: TInstantCodeObject read FOwner;
  end;

  TInstantCodeText = class(TInstantCodeObject)
  private
    FLines: TStringList;
    function GetText: string;
    procedure SetText(Value: string);
  protected
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    property Lines: TStringList read FLines write FLines;
    property Text: string read GetText write SetText;
  end;

  TInstantCodeBody = class(TInstantCodeText)
  end;

  TInstantCodeUses = class(TInstantCodeObject)
  protected
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  end;

  TInstantCodeTypeClass = class of TInstantCodeType;

  TInstantCodeType = class(TInstantCodeObject)
  protected
    function GetIsLinkable: Boolean; virtual;
    class function InternalAtInstance(Reader: TInstantCodeReader;
      out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    property IsLinkable: Boolean read GetIsLinkable;
  end;

  TInstantCodeRecord = class(TInstantCodeType)
  protected
    class function InternalAtIdentifier(Reader: TInstantCodeReader): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    class function Identifier: string; override;
  end;

  TInstantCodeTypeDef = class(TInstantCodeType)
  private
    FTypeDef: string;
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    class function Identifier: string; override;
  end;

  TInstantCodeEnum = class(TInstantCodeType)
  private
    FItems: TStringList;
    function GetCount: Integer;
    function GetItems(Index: Integer): string;
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    class function Identifier: string; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: string read GetItems; default;
  end;

  TInstantCodeSet = class(TInstantCodeType)
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    class function Identifier: string; override;
  end;

  TInstantCodeProcType = class(TInstantCodeType)
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
    class function IsIdentifier(const Str: string): Boolean; override;
  public
    class function Identifier: string; override;
  end;

  TInstantCodeUnknownType = class(TInstantCodeType)
  protected
    class function InternalAtIdentifier(Reader: TInstantCodeReader): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
  end;

  TInstantCodeTypeLink = class(TInstantCodeObjectLink)
  private
    function GetInstance: TInstantCodeType;
    procedure SetInstance(Value: TInstantCodeType);
  protected
    function FindInstance: TInstantCodeObject; override;
  public
    property Instance: TInstantCodeType read GetInstance write SetInstance;
  end;

  TInstantCodeSymbol = class(TInstantCodeObject)
  private
    FTypeLink: TInstantCodeTypeLink;
    function GetTypeName: string;
    function GetTypeValue: TInstantCodeType;
    procedure SetTypeName(const Value: string);
    procedure SetTypeValue(const Value: TInstantCodeType);
  protected
    class function InternalAtInstance(Reader: TInstantCodeReader;
      out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TypeValue: TInstantCodeType read GetTypeValue write SetTypeValue;
    property TypeName: string read GetTypeName write SetTypeName;
  end;

  TInstantCodeVar = class(TInstantCodeSymbol)
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  end;

  TInstantCodeConst = class(TInstantCodeSymbol)
  private
    FValue: string;
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    property Value: string read FValue write FValue;
  end;

  TInstantCodeResString = class(TInstantCodeConst)
  end;

  TInstantCodeParameterOption = (poConst, poVar, poOut);
  TInstantCodeParameterOptions = set of TInstantCodeParameterOption;

  TInstantCodeParameters = class;

  TInstantCodeParameter = class(TInstantCodeSymbol)
  private
    FOptions: TInstantCodeParameterOptions;
    function GetOwner: TInstantCodeParameters; reintroduce;
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    procedure Assign(Source: TPersistent); override;
    property Options: TInstantCodeParameterOptions read FOptions write FOptions;
    property Owner: TInstantCodeParameters read GetOwner;
  end;

  TInstantCodeParameters = class(TInstantCodeObject)
  protected
    function GetCount: Integer;
    function GetItems(Index: Integer): TInstantCodeParameter;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    function Add: TInstantCodeParameter;
    function AddParameter(const AName, ATypeName: string;
      AOptions: TInstantCodeParameterOptions): TInstantCodeParameter;
    procedure Assign(Source: TPersistent); override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantCodeParameter read GetItems; default;
  end;

  TInstantCodeVisibility = (viDefault, viPrivate, viProtected, viPublic, viPublished);
  TInstantCodeVisibilities = set of TInstantCodeVisibility;

  TInstantCodeClass = class;
  TInstantCodeMembers = class;
  TInstantCodeMemberClass = class of TInstantCodeMember;

  TInstantCodeMember = class(TInstantCodeObject)
  private
    FVisibility: TInstantCodeVisibility;
    function GetOwner: TInstantCodeMembers; reintroduce;
    function GetHostClass: TInstantCodeClass;
  protected
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property HostClass: TInstantCodeClass read GetHostClass;
    property Owner: TInstantCodeMembers read GetOwner;
    property Visibility: TInstantCodeVisibility read FVisibility write FVisibility;
  end;

  TInstantCodeMembers = class(TInstantCodeObject)
  private
    function GetOwner: TInstantCodeClass; reintroduce;
  protected
    function GetCount: Integer;
    function GetItems(Index: Integer): TInstantCodeMember;
    class function InternalAtInstance(Reader: TInstantCodeReader;
      out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    procedure Sort; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantCodeMember read GetItems; default;
    property Owner: TInstantCodeClass read GetOwner;
  end;

  TInstantCodeField = class(TInstantCodeMember)
  private
    FVariable: TInstantCodeVar;
    function GetTypeName: string;
    procedure SetTypeName(const Value: string);
  protected
    function GetName: string; override;
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    procedure SetName(const Value: string); override;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TypeName: string read GetTypeName write SetTypeName;
  end;

  TInstantCodePrototype = class;
  TInstantCodeProc = class;
  TInstantCodeProcLink = class;

  TInstantCodeMethod = class(TInstantCodeMember)
  private
    FPrototype: TInstantCodePrototype;
    FProcLink: TInstantCodeProcLink;
    function GetProc: TInstantCodeProc;
    function GetProcName: string;
    procedure SetProc(const Value: TInstantCodeProc);
  protected
    function GetName: string; override;
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    procedure SetName(const Value: string); override;
    property ProcName: string read GetProcName;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Delete; override;
    procedure UpdateProcName;
    procedure UpdatePrototype;
    property Proc: TInstantCodeProc read GetProc write SetProc;
    property Prototype: TInstantCodePrototype read FPrototype;
  end;

  TInstantCodeAttribute = class;

  TInstantCodeProperty = class(TInstantCodeMember)
  private
    FDefaultValue: string;
    FGetter: TInstantCodeMethod;
    FGetterName: string;
    FIsArray: Boolean;
    FIsDefault: Boolean;
    FSetter: TInstantCodeMethod;
    FSetterName: string;
    FStored: string;
    FTypeLink: TInstantCodeTypeLink;
    function GetAttributeType: TInstantAttributeType;
    function GetGetter: TInstantCodeMethod;
    function GetGetterName: string;
    function GetReadOnly: Boolean;
    function GetSetter: TInstantCodeMethod;
    function GetSetterName: string;
    function GetTypeName: string;
    function GetTypeValue: TInstantCodeType;
    procedure SetGetter(Value: TInstantCodeMethod);
    procedure SetGetterName(const Value: string);
    procedure SetSetter(Value: TInstantCodeMethod);
    procedure SetSetterName(const Value: string);
    procedure SetTypeName(const Value: string);
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Delete; override;
    function FindAttribute: TInstantCodeAttribute;
    class function Identifier: string; override;
    property AttributeType: TInstantAttributeType read GetAttributeType;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property Getter: TInstantCodeMethod read GetGetter write SetGetter;
    property GetterName: string read GetGetterName write SetGetterName;
    property IsArray: Boolean read FIsArray write FIsArray;
    property IsDefault: Boolean read FIsDefault write FIsDefault;
    property Setter: TInstantCodeMethod read GetSetter write SetSetter;
    property SetterName: string read GetSetterName write SetSetterName;
    property Stored: string read FStored write FStored;
    property TypeName: string read GetTypeName write SetTypeName;
    property TypeValue: TInstantCodeType read GetTypeValue;
  published
    property ReadOnly: Boolean read GetReadOnly;
  end;

  TInstantCodeContainerMethodType = (mtAdd, mtClear, mtDelete, mtIndexOf, mtInsert, mtRemove);
  TInstantCodeContainerMethodTypes = set of TInstantCodeContainerMethodType;

  TInstantCodeContainerMethodFlag = (mfIndex, mfValue, mfResult);
  TInstantCodeContainerMethodFlags = set of TInstantCodeContainerMethodFlag;

  TInstantCodeMemberRttiAttribute = class(TInstantCodeMember)
  protected
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
  end;

  TInstantCodeClassRttiAttribute = class(TInstantCodeType)
  protected
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
  end;

  TInstantCodeAttributeTailor = class(TObject)
  private
    FAddMethod: TInstantCodeMethod;
    FAttribute: TInstantCodeAttribute;
    FClearMethod: TInstantCodeMethod;
    FCountProp: TInstantCodeProperty;
    FDeleteMethod: TInstantCodeMethod;
    FField: TInstantCodeField;
    FIndexOfMethod: TInstantCodeMethod;
    FInsertMethod: TInstantCodeMethod;
    FIsArray: Boolean;
    FMethodList: TList;
    FMethodTypes: TInstantCodeContainerMethodTypes;
    FReadOnly: Boolean;
    FRemoveMethod: TInstantCodeMethod;
    FValueProp: TInstantCodeProperty;
    function CreateMethod(const AName: string;
      Visibility: TInstantCodeVisibility): TInstantCodeMethod; overload;
    function CreateMethod(const AName, Prefix: string;
      Flags: TInstantCodeContainerMethodFlags): TInstantCodeMethod; overload;
    function GetCountGetterCode: string;
    function GetField: TInstantCodeField;
    function GetFieldName: string;
    function GetFieldValueName: string;
    function GetHostClass: TInstantCodeClass;
    function GetMethodByType(MethodType: TInstantCodeContainerMethodType): TInstantCodeMethod;
    function GetMethodCount: Integer;
    function GetMethodList: TList;
    function GetMethods(Index: Integer): TInstantCodeMethod;
    function GetSingularName: string;
    function GetValueGetterCode: string;
    function GetValueSetterCode: string;
    procedure SetIsArray(Value: Boolean);
    procedure SetMethodByType(MethodType: TInstantCodeContainerMethodType;
      Value: TInstantCodeMethod);
    procedure SetMethodTypes(Value: TInstantCodeContainerMethodTypes);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure AddCountProp;
    procedure AddField;
    procedure AddMethods;
    procedure AddValueProp;
    procedure DeleteItem(var Item);
    procedure DeleteMethods;
    procedure Update; virtual;
    property MethodList: TList read GetMethodList;
  public
    constructor Create(AAttribute: TInstantCodeAttribute);
    destructor Destroy; override;
    procedure Apply;
    function CreateMethod(MethodType: TInstantCodeContainerMethodType): TInstantCodeMethod; overload;
    property Attribute: TInstantCodeAttribute read FAttribute;
    property CountGetterCode: string read GetCountGetterCode;
    property Field: TInstantCodeField read GetField;
    property FieldName: string read GetFieldName;
    property FieldValueName: string read GetFieldValueName;
    property HostClass: TInstantCodeClass read GetHostClass;
    property IsArray: Boolean read FIsArray write SetIsArray;
    property MethodByType[MethodType: TInstantCodeContainerMethodType]: TInstantCodeMethod read GetMethodByType write SetMethodByType;
    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TInstantCodeMethod read GetMethods;
    property MethodTypes: TInstantCodeContainerMethodTypes read FMethodTypes write SetMethodTypes;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property SingularName: string read GetSingularName;
    property ValueGetterCode: string read GetValueGetterCode;
    property ValueProp: TInstantCodeProperty read FValueProp;
    property ValueSetterCode: string read GetValueSetterCode;
  end;

  TInstantCodeMetadataInfo = class;

  TInstantCodeAttribute = class(TInstantCodeObject)
  private
    FFreeMetadata: Boolean;
    FMetadata: TInstantAttributeMetadata;
    FSingularName: string;
    FTailor: TInstantCodeAttributeTailor;
    FVisibility: TInstantCodeVisibility;
    function GetAttributeClass: TInstantAbstractAttributeClass;
    function GetAttributeClassName: string;
    function GetAttributeType: TInstantAttributeType;
    function GetAttributeTypeName: string;
    function GetAttributeTypeText: string;
    function GetCountPropName: string;
    function GetFieldName: string;
    function GetHostClass: TInstantCodeClass;
    function GetIncludeAddMethod: Boolean;
    function GetIncludeClearMethod: Boolean;
    function GetIncludeDeleteMethod: Boolean;
    function GetIncludeIndexOfMethod: Boolean;
    function GetIncludeInsertMethod: Boolean;
    function GetIncludeRemoveMethod: Boolean;
    function GetIsComplex: Boolean;
    function GetIsContainer: Boolean;
    function GetIsDescription: Boolean;
    function GetIsEnum: Boolean;
    function GetIsIndexed: Boolean;
    function GetIsRequired: Boolean;
    function GetIsUnique: Boolean;
    function GetIsPrimaryKey: Boolean;
    function GetIndexName: string;
    function GetMetadata: TInstantAttributeMetadata;
    function GetMethodTypes: TInstantCodeContainerMethodTypes;
    function GetObjectClass: TInstantCodeClass;
    function GetObjectClassName: string;
    function GetOwner: TInstantCodeMetadataInfo; reintroduce;
    function GetPropTypeName: string;
    function GetReadOnly: Boolean;
    function GetStorageName: string;
    function GetTailor: TInstantCodeAttributeTailor;
    function GetValuePropName: string;
    procedure SetAttributeClass(Value: TInstantAbstractAttributeClass);
    procedure SetAttributeClassName(const Value: string);
    procedure SetAttributeType(Value: TInstantAttributeType);
    procedure SetAttributeTypeName(const Value: string);
    procedure SetIncludeAddMethod(const Value: Boolean);
    procedure SetIncludeClearMethod(const Value: Boolean);
    procedure SetIncludeDeleteMethod(const Value: Boolean);
    procedure SetIncludeIndexOfMethod(const Value: Boolean);
    procedure SetIncludeInsertMethod(const Value: Boolean);
    procedure SetIncludeRemoveMethod(const Value: Boolean);
    procedure SetIsDescription(const Value: Boolean);
    procedure SetIsIndexed(const Value: Boolean);
    procedure SetIsRequired(const Value: Boolean);
    procedure SetIsUnique(const Value: Boolean);
    procedure SetIsPrimaryKey(const Value: Boolean);
    procedure SetIndexName(const Value: string);
    procedure SetMethodTypes(const Value: TInstantCodeContainerMethodTypes);
    procedure SetObjectClassName(const Value: string);
    procedure SetPropTypeName(const Value: string);
    procedure SetReadOnly(Value: Boolean);
    procedure SetStorageName(const Value: string);
    procedure SetVisibility(Value: TInstantCodeVisibility);
    function GetExternalStorageName: string;
    function GetForeignKeyFields: string;
    function GetStorageKind: TInstantStorageKind;
    procedure SetExternalStorageName(const Value: string);
    procedure SetForeignKeyFields(const Value: string);
    procedure SetStorageKind(const Value: TInstantStorageKind);
    function GetCanHaveStorageName: boolean;
    function GetCanHaveExternalStorageName: boolean;
    function GetCanHaveForeignKeyFields: boolean;
    function GetCanBeExternal: boolean;
    function GetUseNull: Boolean;
    procedure SetUseNull(const Value: Boolean);
    function GetIsLocalized: Boolean;
    procedure SetIsLocalized(const Value: Boolean);
  protected
    function GetIsDefault: Boolean; virtual;
    function GetMethodName(MethodType: TInstantCodeContainerMethodType): string;
    function GetMethodPrefix(MethodType: TInstantCodeContainerMethodType): string;
    function GetSingularName: string; virtual;
    function GetValueGetterCode: string; virtual;
    function GetValueSetterCode: string; virtual;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    procedure SetIsDefault(const Value: Boolean); virtual;
    procedure SetName(const Value: string); override;
    procedure SetSingularName(const Value: string);
    procedure Update;
    property Tailor: TInstantCodeAttributeTailor read GetTailor;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Delete; override;
    procedure DetectMethodTypes;
    function FindMethod(MethodType: TInstantCodeContainerMethodType): TInstantCodeMethod;
    function FindCountProp: TInstantCodeProperty;
    function FindField: TInstantCodeField;
    function FindValueProp: TInstantCodeProperty;
    procedure IncludeMethodTypes(AMethodTypes: TInstantCodeContainerMethodTypes;
      Include: Boolean);
    procedure Realize;
    property AttributeClass: TInstantAbstractAttributeClass read GetAttributeClass write SetAttributeClass;
    property AttributeClassName: string read GetAttributeClassName write SetAttributeClassName;
    property CanHaveStorageName: boolean read GetCanHaveStorageName;
    property CanHaveExternalStorageName: boolean read GetCanHaveExternalStorageName;
    property CanHaveForeignKeyFields: boolean read GetCanHaveForeignKeyFields;
    property CanBeExternal: boolean read GetCanBeExternal;
    property CountPropName: string read GetCountPropName;
    property FieldName: string read GetFieldName;
    property HostClass: TInstantCodeClass read GetHostClass;
    property ObjectClass: TInstantCodeClass read GetObjectClass;
    property ObjectClassName: string read GetObjectClassName write SetObjectClassName;
    property Owner: TInstantCodeMetadataInfo read GetOwner;
    property IsComplex: Boolean read GetIsComplex;
    property IsContainer: Boolean read GetIsContainer;
    property IsEnum: Boolean read GetIsEnum;
    property PropTypeName: string read GetPropTypeName write SetPropTypeName;
    property ValueGetterCode: string read GetValueGetterCode;
    property ValuePropName: string read GetValuePropName;
    property ValueSetterCode: string read GetValueSetterCode;
  published
    property AttributeType: TInstantAttributeType read GetAttributeType write SetAttributeType;
    property AttributeTypeName: string read GetAttributeTypeName write SetAttributeTypeName;
    property AttributeTypeText: string read GetAttributeTypeText;
    property ExternalStorageName: string read GetExternalStorageName write SetExternalStorageName;
    property ForeignKeyFields: string read GetForeignKeyFields write SetForeignKeyFields;
    property IncludeAddMethod: Boolean read GetIncludeAddMethod write SetIncludeAddMethod;
    property IncludeClearMethod: Boolean read GetIncludeClearMethod write SetIncludeClearMethod;
    property IncludeDeleteMethod: Boolean read GetIncludeDeleteMethod write SetIncludeDeleteMethod;
    property IncludeIndexOfMethod: Boolean read GetIncludeIndexOfMethod write SetIncludeIndexOfMethod;
    property IncludeInsertMethod: Boolean read GetIncludeInsertMethod write SetIncludeInsertMethod;
    property IncludeRemoveMethod: Boolean read GetIncludeRemoveMethod write SetIncludeRemoveMethod;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
    property UseNull: Boolean read GetUseNull write SetUseNull;
    property StorageKind: TInstantStorageKind read GetStorageKind write SetStorageKind;
    property IsDescription: Boolean read GetIsDescription write SetIsDescription;
    property IsIndexed: Boolean read GetIsIndexed write SetIsIndexed;
    property IsRequired: Boolean read GetIsRequired write SetIsRequired;
    property IsLocalized: Boolean read GetIsLocalized write SetIsLocalized;
    property IsUnique: Boolean read GetIsUnique write SetIsUnique;
    property IsPrimaryKey: Boolean read GetIsPrimaryKey write SetIsPrimaryKey;
    property IndexName: string read GetIndexName write SetIndexName;
    property Metadata: TInstantAttributeMetadata read GetMetadata;
    property MethodTypes: TInstantCodeContainerMethodTypes read GetMethodTypes write SetMethodTypes;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property SingularName: string read GetSingularName write SetSingularName;
    property StorageName: string read GetStorageName write SetStorageName;
    property Visibility: TInstantCodeVisibility read FVisibility
      write SetVisibility;
  end;

  TInstantCodeClassLink = class(TInstantCodeTypeLink)
  private
    function GetInstance: TInstantCodeClass;
    procedure SetInstance(const Value: TInstantCodeClass);
  protected
    function FindInstance: TInstantCodeObject; override;
  public
    property Instance: TInstantCodeClass read GetInstance write SetInstance;
  end;

  TInstantCodeBaseClassLink = class(TInstantCodeClassLink)
  private
    function GetOwner: TInstantCodeClass;
  protected
    procedure AssignInstance(Value: TInstantCodeObject); override;
    procedure Attach; override;
    procedure Detach; override;
  public
    constructor Create(AOwner: TInstantCodeClass);
    property Owner: TInstantCodeClass read GetOwner;
  end;

  TInstantCodeModifier = class;

  TInstantCodeChangeType = (ctNew, ctEdit, ctDelete);

  TInstantCodeClassChangeInfo = class(TObject)
  private
    FChangedAttributes: TStringList;
    FChangeType: TInstantCodeChangeType;
    FInstance: TInstantCodeClass;
    FModifier: TInstantCodeModifier;
    FNewAttributes: TList;
    FOldName: string;
  public
    constructor Create(AInstance: TInstantCodeClass;
      AChangeType: TInstantCodeChangeType; const AOldName: string;
      AChangedAttributes: TStringList; ANewAttributes: TList);
    function IsChangedAttribute(Attribute: TInstantCodeAttribute; out OldName: string): Boolean;
    function IsDeletedAttribute(Attribute: TInstantCodeAttribute): Boolean;
    function IsNewAttribute(Attribute: TInstantCodeAttribute): Boolean;
    property ChangeType: TInstantCodeChangeType read FChangeType;
    property Modifier: TInstantCodeModifier read FModifier write FModifier;
    property OldName: string read FOldName;
  end;

  TInstantCodeMetadataInfo = class(TInstantCodeObject)
  private
    FAttributes: TList;
    FMetadata: TInstantClassMetadata;
    function GetAttributeCount: Integer;
    function GetAttributes(Index: Integer): TInstantCodeAttribute;
    function GetClassStatement: string;
    function GetIsStored: Boolean;
    function GetMetadata: TInstantClassMetadata;
    function GetOwner: TInstantCodeClass; reintroduce;
    function GetPersistence: TInstantPersistence;
    function GetStorageName: string;
    procedure SetPersistence(Value: TInstantPersistence);
    procedure SetStorageName(const Value: string);
  protected
    function GetIsEmpty: Boolean; override;
    function GetName: string; override;
    class function InternalAtInstance(Reader: TInstantCodeReader;
      out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    procedure RemoveAttribute(Attribute: TInstantCodeAttribute);
    procedure SetName(const Value: string); override;
    property ClassStatement: string read GetClassStatement;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    function AddAttribute: TInstantCodeAttribute;
    procedure Assign(Source: TPersistent); override;
    procedure AssignAttributes(List: TList);
    procedure AttachTo(MetadataInfo: TInstantCodeMetadataInfo);
    procedure CloneAttributes(List: TList);
    function FindAttribute(const Name: string): TInstantCodeAttribute;
    function FindNearestAttributes(Instance: TInstantCodeObject;
      out Prior, Next: TObject): Boolean;
    procedure InsertAttribute(Attribute: TInstantCodeAttribute);
    procedure Organize; override;
    procedure Sort; override;
    property AttributeCount: Integer read GetAttributeCount;
    property Attributes[Index: Integer]: TInstantCodeAttribute read GetAttributes;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsStored: Boolean read GetIsStored;
    property Metadata: TInstantClassMetadata read GetMetadata;
    property Owner: TInstantCodeClass read GetOwner;
  published
    property Persistence: TInstantPersistence read GetPersistence write SetPersistence;
    property StorageName: string read GetStorageName write SetStorageName;
  end;

  TInstantCodeDivision = class(TInstantCodeMembers)
  private
    FVisibility: TInstantCodeVisibility;
  protected
    function AddMember(AClass: TInstantCodeMemberClass): TInstantCodeMember;
    function GetIsEmpty: Boolean; override;
    function GetName: string; override;
    class function InternalAtInstance(Reader: TInstantCodeReader;
      out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    class function NameToVisibility(const Name: string;
      var Visibility: TInstantCodeVisibility): Boolean;
    class function VisibilityToName(Visibility: TInstantCodeVisibility): string;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    function AddField: TInstantCodeField;
    function AddMethod: TInstantCodeMethod;
    function AddProperty: TInstantCodeProperty;
    property Visibility: TInstantCodeVisibility read FVisibility write FVisibility;
  end;

  TInstantCodeClassForward = class(TInstantCodeType)
  protected
    function GetIsLinkable: Boolean; override;
    class function InternalAtInstance(Reader: TInstantCodeReader;
      out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    class function Identifier: string; override;
  end;

  TInstantCodeClass = class(TInstantCodeType)
  private
    FBaseClassLink: TInstantCodeBaseClassLink;
    FDivisions: TList;
    FFields: TList;
    FMembers: TList;
    FMetadataInfo: TInstantCodeMetadataInfo;
    FMethods: TList;
    FProperties: TList;
    FSubClassList: TList;
    function GetAttributeCount: Integer;
    function GetAttributes(Index: Integer): TInstantCodeAttribute;
    function GetBaseClass: TInstantCodeClass;
    function GetBaseClassName: string;
    function GetDivisionCount: Integer;
    function GetDivisions(Index: Integer): TInstantCodeDivision;
    function GetFieldCount: Integer;
    function GetFields(Index: Integer): TInstantCodeField;
    function GetImplementationComment: string;
    function GetImplementationString: string;
    function GetIsStored: Boolean;
    function GetMemberCount: Integer;
    function GetMembers(Index: Integer): TInstantCodeMember;
    function GetMetadata: TInstantClassMetadata;
    function GetMethodCount: Integer;
    function GetMethods(Index: Integer): TInstantCodeMethod;
    function GetPersistence: TInstantPersistence;
    function GetProperties(Index: Integer): TInstantCodeProperty;
    function GetPropertyCount: Integer;
    function GetStorageName: string;
    function GetSubClass(Index: Integer): TInstantCodeClass;
    function GetSubClassCount: Integer;
    function GetSubClassList: TList;
    function GetPascalUnitName: string;
    procedure SetBaseClass(const Value: TInstantCodeClass);
    procedure SetBaseClassName(const Value: string);
    procedure SetPersistence(const Value: TInstantPersistence);
    procedure SetStorageName(const Value: string);
    procedure SetPascalUnitName(const Value: string);
    procedure RemoveDivision(Division: TInstantCodeDivision);
    procedure SetSubClass(Index: Integer; const Value: TInstantCodeClass);
  protected
    function AddDivision(Visibility: TInstantCodeVisibility): TInstantCodeDivision;
    procedure InsertDivision(Division: TInstantCodeDivision);
    procedure InsertInBase;
    procedure InsertMember(Member: TInstantCodeMember);
    procedure InsertSubClass(AClass: TInstantCodeClass);
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    procedure RemoveFromBase;
    procedure RemoveMember(Member: TInstantCodeMember);
    procedure SetName(const Value: string); override;
    procedure VisibilityFilter(Sender: TInstantCodeObject;
      var Include: Boolean; Arg: Pointer);
    procedure AddUses(AUnitNames: array of string; Scope:
      TInstantCodeScope; var Source: string; ChangeInfo:
      TInstantCodeClassChangeInfo);
    property SubClassList: TList read GetSubClassList;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    function AddAttribute: TInstantCodeAttribute;
    function AddField(Visibility: TInstantCodeVisibility): TInstantCodeField;
    function AddMember(AClass: TInstantCodeMemberClass;
      Visibility: TInstantCodeVisibility): TInstantCodeMember;
    function AddMethod(Visibility: TInstantCodeVisibility): TInstantCodeMethod;
    function AddProperty(Visibility: TInstantCodeVisibility): TInstantCodeProperty;
    procedure ApplyToSource(var Source: string; ChangeInfo: TInstantCodeClassChangeInfo);
    procedure AssignAttributes(List: TList);
    procedure CloneAttributes(List: TList);
    function DerivesFrom(AClass: TInstantCodeClass): Boolean; overload;
    function DerivesFrom(AClassName: string): Boolean; overload;
    function FindAttribute(const Name: string): TInstantCodeAttribute;
    function FindDivision(Visibility: TInstantCodeVisibility): TInstantCodeDivision;
    function FindField(const Name: string): TInstantCodeField;
    function FindNearestDivisions(Division: TInstantCodeDivision;
      out Prior, Next: TObject): Boolean;
    function FindNearestFields(Field: TInstantCodeField;
      out Prior, Next: TObject): Boolean;
    function FindNearestMembers(List: TList; Instance: TInstantCodeObject;
      Visibilities: TInstantCodeVisibilities; out Prior, Next: TObject): Boolean;
    function FindNearestMethods(Method: TInstantCodeMethod;
      out Prior, Next: TObject): Boolean;
    function FindNearestProperties(Prop: TInstantCodeProperty;
      out Prior, Next: TObject): Boolean;
    function FindMember(AClass: TInstantCodeMemberClass;
      const Name: string): TInstantCodeMember;
    function FindMethod(const Name: string): TInstantCodeMethod;
    function FindProperty(const Name: string): TInstantCodeProperty;
    procedure Fixup; override;
    class function Identifier: string; override;
    procedure Organize; override;
    procedure RemoveSubClass(AClass: TInstantCodeClass);
    procedure Sort; override;
    property AttributeCount: Integer read GetAttributeCount;
    property Attributes[Index: Integer]: TInstantCodeAttribute read GetAttributes;
    property BaseClass: TInstantCodeClass read GetBaseClass write SetBaseClass;
    property DivisionCount: Integer read GetDivisionCount;
    property Divisions[Index: Integer]: TInstantCodeDivision read GetDivisions;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TInstantCodeField read GetFields;
    property ImplementationComment: string read GetImplementationComment;
    property ImplementationString: string read GetImplementationString;
    property IsStored: Boolean read GetIsStored;
    property MemberCount: Integer read GetMemberCount;
    property Members[Index: Integer]: TInstantCodeMember read GetMembers;
    property Metadata: TInstantClassMetadata read GetMetadata;
    property MetadataInfo: TInstantCodeMetadataInfo read FMetadataInfo;
    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TInstantCodeMethod read GetMethods;
    property Module: TInstantCodeModule read GetModule;
    property Properties[Index: Integer]: TInstantCodeProperty read GetProperties;
    property PropertyCount: Integer read GetPropertyCount;
    property SubClassCount: Integer read GetSubClassCount;
    property SubClasses[Index: Integer]: TInstantCodeClass read GetSubClass write
        SetSubClass;
  published
    property BaseClassName: string read GetBaseClassName write SetBaseClassName;
    property Persistence: TInstantPersistence read GetPersistence write SetPersistence;
    property StorageName: string read GetStorageName write SetStorageName;
    property PascalUnitName: string read GetPascalUnitName write SetPascalUnitName;
  end;

  TInstantCodeClassList = class(TList)
  private
    function GetItem(Index: Integer): TInstantCodeClass;
    procedure SetItem(Index: Integer; const Value: TInstantCodeClass);
  public
    function Add(Item: TInstantCodeClass): Integer;
    procedure Insert(Index: Integer; Item: TInstantCodeClass);

    procedure SortByBaseClass;

    property Items[Index: Integer]: TInstantCodeClass read GetItem write SetItem; default;
  end;

  TInstantCodeClassRef = class(TInstantCodeType)
  protected
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    class function Identifier: string; override;
  end;

  TInstantCodeInterface = class(TInstantCodeType)
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    class function Identifier: string; override;
  end;

  TInstantCodeDispInterface = class(TInstantCodeInterface)
  public
    class function Identifier: string; override;
  end;

  TInstantCodeSection = class;
  TInstantCodeParagraphClass = class of TInstantCodeParagraph;

  TInstantCodeParagraph = class(TInstantCodeObject)
  private
    function GetOwner: TInstantCodeSection; reintroduce;
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    procedure Reset; override;
    property Owner: TInstantCodeSection read GetOwner;
  end;

  TInstantCodeUsesClause = class(TInstantCodeParagraph)
  protected
    function GetCount: Integer;
    function GetItems(Index: Integer): TInstantCodeUses;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    function Add: TInstantCodeUses;
    function Find(const Name: string): TInstantCodeUses;
    class function Identifier: string; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantCodeUses read GetItems; default;
  end;

  TInstantCodeTypes = class(TInstantCodeParagraph)
  protected
    function GetCount: Integer;
    function GetItems(Index: Integer): TInstantCodeType;
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    function AddClass(AClass: TInstantCodeClass = nil): TInstantCodeClass;
    function AddEnum: TInstantCodeEnum;
    class function Identifier: string; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantCodeType read GetItems; default;
  end;

  TInstantCodeSymbols = class(TInstantCodeParagraph)
  protected
    function AddItem: TInstantCodeObject; virtual; abstract;
    function AtItem(Reader: TInstantCodeReader): Boolean;
    procedure InternalRead(Reader: TInstantCodeReader); override;
  end;

  TInstantCodeConsts = class(TInstantCodeSymbols)
  protected
    function AddItem: TInstantCodeObject; override;
    function GetItems(Index: Integer): TInstantCodeConst;
  public
    class function Identifier: string; override;
    property Items[Index: Integer]: TInstantCodeConst read GetItems; default;
  end;

  TInstantCodeVars = class(TInstantCodeSymbols)
  protected
    function AddItem: TInstantCodeObject; override;
    function GetItems(Index: Integer): TInstantCodeVar;
  public
    class function Identifier: string; override;
    property Items[Index: Integer]: TInstantCodeVar read GetItems; default;
  end;

  TInstantCodeResStrings = class(TInstantCodeSymbols)
  protected
    function AddItem: TInstantCodeObject; override;
    function GetItems(Index: Integer): TInstantCodeResString;
  public
    class function Identifier: string; override;
    property Items[Index: Integer]: TInstantCodeResString read GetItems; default;
  end;

  TInstantCodeProcOption = (poClass, poConstructor, poDestructor);
  TInstantCodeProcOptions = set of TInstantCodeProcOption;

  TInstantCodeProcFlag = (pfVirtual, pfDynamic, pfAbstract, pfOverride,
    pfReintroduce, pfOverload, pfStdCall, pfSafeCall, pfForward);
  TInstantCodeProcFlags = set of TInstantCodeProcFlag;

  TInstantCodePrototype = class(TInstantCodeObject)
  private
    FFlags: TInstantCodeProcFlags;
    FOptions: TInstantCodeProcOptions;
    FParameters: TInstantCodeParameters;
    FResultTypeLink: TInstantCodeTypeLink;
    function GetIsFunction: Boolean;
    function GetParameters: TInstantCodeParameters;
    function GetResultType: TInstantCodeType;
    function GetResultTypeName: string;
    procedure SetResultType(const Value: TInstantCodeType);
    procedure SetResultTypeName(const Value: string);
  protected
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    class function IsIdentifier(const Str: string): Boolean; override;
    procedure ReadFlags(Reader: TInstantCodeReader);
    procedure WriteFlags(Writer: TInstantCodeWriter);
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    function AddParameter: TInstantCodeParameter;
    procedure Assign(Source: TPersistent); override;
    procedure Write(Writer: TInstantCodeWriter; Method: TInstantCodeMethod = nil);
    property Flags: TInstantCodeProcFlags read FFlags write FFlags;
    property IsFunction: Boolean read GetIsFunction;
    property Options: TInstantCodeProcOptions read FOptions write FOptions;
    property Parameters: TInstantCodeParameters read GetParameters;
    property ResultType: TInstantCodeType read GetResultType write SetResultType;
    property ResultTypeName: string read GetResultTypeName write SetResultTypeName;
  end;

  TInstantCodeProcs = class;

  TInstantCodeProc = class(TInstantCodeObject)
  private
    FBody: TInstantCodeBody;
    FHeader: TInstantCodePrototype;
    function GetHasBody: Boolean;
    function GetOwner: TInstantCodeProcs; reintroduce;
    function GetParameters: TInstantCodeParameters;
    function GetResultTypeName: string;
    function GetSection: TInstantCodeSection;
    procedure SetResultTypeName(const Value: string);
  protected
    function GetName: string; override;
    class function InternalAtInstance(Reader: TInstantCodeReader;
      out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    procedure ReadBody(Reader: TInstantCodeReader);
    procedure SetName(const Value: string); override;
    procedure WriteBody(Writer: TInstantCodeWriter);
    property HasBody: Boolean read GetHasBody;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Body: TInstantCodeBody read FBody write FBody;
    property Header: TInstantCodePrototype read FHeader;
    property Owner: TInstantCodeProcs read GetOwner;
    property Parameters: TInstantCodeParameters read GetParameters;
    property ResultTypeName: string read GetResultTypeName write SetResultTypeName;
    property Section: TInstantCodeSection read GetSection;
  end;

  TInstantCodeProcLink = class(TInstantCodeObjectLink)
  private
    function GetInstance: TInstantCodeProc;
    procedure SetInstance(Value: TInstantCodeProc);
  protected
    function FindInstance: TInstantCodeObject; override;
  public
    property Instance: TInstantCodeProc read GetInstance write SetInstance;
  end;

  TInstantCodeProcs = class(TInstantCodeParagraph)
  private
    function GetCount: Integer;
  protected
    function GetItems(Index: Integer): TInstantCodeProc;
    class function InternalAtInstance(Reader: TInstantCodeReader; out Name: string): Boolean; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    function Add: TInstantCodeProc;
    function Find(const Name: string): TInstantCodeProc;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInstantCodeProc read GetItems; default;
  end;

  TInstantCodeSectionClass = class of TInstantCodeSection;

  TInstantCodeSection = class(TInstantCodeObject)
  private
    FProcs: TList;
    function GetProcCount: Integer;
    function GetProcs(Index: Integer): TInstantCodeProc;
  protected
    function AtEndOfSection(Reader: TInstantCodeReader): Boolean; virtual;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
    procedure ReadParagraphs(Reader: TInstantCodeReader);
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    function AddClass(AClass: TInstantCodeClass = nil): TInstantCodeClass;
    function AddProc: TInstantCodeProc;
    function FindNearestProcs(Proc: TInstantCodeProc;
      out Prior, Next: TObject): Boolean;
    function FindProc(const Name: string): TInstantCodeProc;
    function FindProcs: TInstantCodeProcs;
    function FindTypes: TInstantCodeTypes;
    function FindUsesClause: TInstantCodeUsesClause;
    procedure InsertProc(Proc: TInstantCodeProc);
    class function NextSectionClass: TInstantCodeSectionClass; virtual;
    procedure RemoveProc(Proc: TInstantCodeProc);
    procedure Reset; override;
    property ProcCount: Integer read GetProcCount;
    property Procs[Index: Integer]: TInstantCodeProc read GetProcs;
  end;

  TInstantCodeInterfaceSection = class(TInstantCodeSection)
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    class function Identifier: string; override;
    class function NextSectionClass: TInstantCodeSectionClass; override;
  end;

  TInstantCodeImplementationSection = class(TInstantCodeSection)
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  public
    class function Identifier: string; override;
    class function NextSectionClass: TInstantCodeSectionClass; override;
  end;

  TInstantCodeInitializationSection = class(TInstantCodeSection)
  public
    class function Identifier: string; override;
    class function NextSectionClass: TInstantCodeSectionClass; override;
  end;

  TInstantCodeFinalizationSection = class(TInstantCodeSection)
  public
    class function Identifier: string; override;
  end;

  TInstantCodeProgramSection = class(TInstantCodeSection)
  protected
    procedure InternalRead(Reader: TInstantCodeReader); override;
  end;

  TInstantCodeClassEvent = procedure(Sender: TObject;
    AClass: TInstantCodeClass) of object;

  TInstantCodeClassApplyEvent = procedure(Sender: TObject;
    AClass: TInstantCodeClass; ChangeInfo: TInstantCodeClassChangeInfo) of object;

  TInstantCodeModule = class(TInstantCodeObject)
  private
    FTypes: TList;
    FClasses: TList;
    FFileName: string;
    FFinalizationSection: TInstantCodeFinalizationSection;
    FInitializationSection: TInstantCodeInitializationSection;
    FInterfaceSection: TInstantCodeInterfaceSection;
    FImplementationSection: TInstantCodeImplementationSection;
    FModuleType: TInstantCodeModuleType;
    FProgramSection: TInstantCodeProgramSection;
    function GetClassCount: Integer;
    function GetClasses(Index: Integer): TInstantCodeClass;
    function GetFinalizationSection: TInstantCodeFinalizationSection;
    function GetImplementationSection: TInstantCodeImplementationSection;
    function GetInitializationSection: TInstantCodeInitializationSection;
    function GetInterfaceSection: TInstantCodeInterfaceSection;
    function GetModuleTypeName: string;
    function GetOwner: TInstantCodeProject; reintroduce;
    function GetProgramSection: TInstantCodeProgramSection;
    function GetTypeCount: Integer;
    function GetTypes(Index: Integer): TInstantCodeType;
    function GetPascalUnitName: string;
    procedure SetModuleTypeName(const Value: string);
  protected
    function GetModule: TInstantCodeModule; override;
    procedure InternalRead(Reader: TInstantCodeReader); override;
    procedure InternalWrite(Writer: TInstantCodeWriter); override;
  public
    constructor Create(AOwner: TInstantCodeObject); override;
    destructor Destroy; override;
    function FindClass(const Name: string): TInstantCodeClass;
    function FindType(const Name: string): TInstantCodeType;
    procedure InsertType(AType: TInstantCodeType);
    procedure RemoveType(AType: TInstantCodeType);
    procedure LoadFromFile(const FileName: string; Scope: TInstantCodeScope);
    procedure LoadFromStream(Stream: TStream; Scope: TInstantCodeScope);
    procedure LoadFromString(const Str: string; Scope: TInstantCodeScope);
    procedure Read(Reader: TInstantCodeReader; Scope: TInstantCodeScope); reintroduce;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property ClassCount: Integer read GetClassCount;
    property Classes[Index: Integer]: TInstantCodeClass read GetClasses;
    property FileName: string read FFileName write FFileName;
    property FinalizationSection: TInstantCodeFinalizationSection read GetFinalizationSection;
    property ImplementationSection: TInstantCodeImplementationSection read GetImplementationSection;
    property InitializationSection: TInstantCodeInitializationSection read GetInitializationSection;
    property InterfaceSection: TInstantCodeInterfaceSection read GetInterfaceSection;
    property ModuleType: TInstantCodeModuleType read FModuleType write FModuleType;
    property ModuleTypeName: string read GetModuleTypeName write SetModuleTypeName;
    property Owner: TInstantCodeProject read GetOwner;
    property ProgramSection: TInstantCodeProgramSection read GetProgramSection;
    property TypeCount: Integer read GetTypeCount;
    property Types[Index: Integer]: TInstantCodeType read GetTypes;
  published
    property PascalUnitName: string read GetPascalUnitName;
  end;

  TInstantCodeProject = class(TInstantCodeObject)
  private
    FClasses: TList;
    FModel: TInstantModel;
    FTypes: TList;
    function GetClassCount: Integer;
    function GetClasses(Index: Integer): TInstantCodeClass;
    function GetModuleCount: Integer;
    function GetModules(Index: Integer): TInstantCodeModule;
    function GetTypeCount: Integer;
    function GetTypes(Index: Integer): TInstantCodeType;
  protected
    procedure InsertType(AType: TInstantCodeType);
    procedure InternalClear; override;
    procedure RemoveType(AType: TInstantCodeType);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function CreateClassMetadata: TInstantClassMetadata;
    function FindClass(const Name: string): TInstantCodeClass;
    function FindModule(const Name: string): TInstantCodeModule;
    function FindType(const Name: string): TInstantCodeType;
    procedure Fixup; override;
    function LoadModule(Stream: TStream; const FileName: string = '';
      Scope: TInstantCodeScope = scInterface): TInstantCodeModule; overload;
    function LoadModule(const Str: string; const FileName: string = '';
      Scope: TInstantCodeScope = scInterface): TInstantCodeModule; overload;
    procedure SaveToResFile(const FileName: string);
    property ClassCount: Integer read GetClassCount;
    property Classes[Index: Integer]: TInstantCodeClass read GetClasses;
    property Model: TInstantModel read FModel;
    property ModuleCount: Integer read GetModuleCount;
    property Modules[Index: Integer]: TInstantCodeModule read GetModules;
    property TypeCount: Integer read GetTypeCount;
    property Types[Index: Integer]: TInstantCodeType read GetTypes;
  end;

  PInstantCodeModel = ^TInstantCodeModel;
  TInstantCodeModel = class(TInstantCodeProject)
  private
    FComponentList: TList;
    function GetComponentList: TList;
  protected
    procedure InternalChanged; override;
    property ComponentList: TList read GetComponentList;
  public
    procedure AssignComponents(Source: TInstantCodeModel);
    procedure AddComponent(Component: TComponent);
    procedure RemoveComponent(Component: TComponent);
  end;

  TInstantCodeModelEvent = procedure(Sender: TObject;
    Model: TInstantCodeModel) of object;

  TInstantCodeWriter = class(TInstantTextWriter)
  private
    FMargin: Integer;
    FOrigin: TInstantCodePos;
    procedure SetOrigin(Value: TInstantCodePos);
  protected
    function GetStreamPos: Int64; override;
    procedure SetStreamPos(Value: Int64); override;
  public
    procedure Indent;
    procedure Unindent;
    procedure Write(Str: string);
    procedure WriteChar(Ch: Char); override;
    procedure WriteFlags(Flags: Pointer; TypeInfo: PTypeInfo;
      const Prefix: string);
    procedure WriteFmt(Fmt: string; Args: array of const);
    procedure WriteLn(Str: string = '');
    procedure WriteLnFmt(Fmt: string; Args: array of const);
    procedure WriteMargin;
    property Origin: TInstantCodePos read FOrigin write SetOrigin;
  end;

  TInstantCodeReader = class(TInstantTextReader)
  private
    FErrorSeverity: TInstantCodeErrorSeverity;
    FIgnoreComments: Boolean;
  protected
    function GetFinished: Boolean;
    procedure Initialize; override;
  public
    function EnterComment: Boolean;
    procedure ErrorExpected(const Expected: string; Str: Boolean = True);
    procedure ErrorMessage(const Msg: string);
    function Find(Text: string): Boolean;
    function ReadChar: Char; override;
    function ReadEndOfStatement(Required: Boolean = False): Boolean;
    function ReadFlags(Flags: Pointer; TypeInfo: PTypeInfo;
      const Prefix: string): Boolean;
    function ReadInteger: Integer;
    function ReadMacro(Macros: array of string): string;
    function ReadRestOfStatement: string;
    function ReadStatement: string;
    function ReadString: string; override;
    function ReadStringValue: string;
    function ReadTextChar: Char;
    function ReadTokenIgnoring(Ignore: string): string;
    procedure ReadTokenMatching(Match: string);
    procedure SkipBlanks;
    property ErrorSeverity: TInstantCodeErrorSeverity read FErrorSeverity write FErrorSeverity;
    property Finished: Boolean read GetFinished;
    property IgnoreComments: Boolean read FIgnoreComments write FIgnoreComments;
  end;

  TInstantCodeInsertMode = (imBefore, imAfter);

  TInstantCodeModifier = class(TObject)
  private
    FBaseProject: TInstantCodeProject;
    FCode: PString;
    FInsertMode: TInstantCodeInsertMode;
    FModule: TInstantCodeModule;
    FProject: TInstantCodeProject;
    FReader: TInstantCodeReader;
    procedure CheckBaseClassAddUsers(AClass: TInstantCodeClass);
    procedure CreateModule;
    procedure CreateReader;
    procedure DestroyReader;
    function GetCursorPos: TInstantCodePos;
    procedure SetCursorPos(const Value: TInstantCodePos);
  protected
    function ClassDerivesFrom(AClass: TInstantCodeClass; const AClassName: string): Boolean;
    function CreateClassList(OldClass, NewClass: TInstantCodeClass): TStringList;
    procedure DoDelete(Len: Integer);
    procedure DoInsert(const Str: string; Wrap: Boolean);
    function GetIndentText(Pos: TInstantCodePos): string;
    function ReadIndentText: string;
    procedure UpdateModule(Origin, Delta: TInstantCodePos; Before: Boolean);
    procedure UpdateReader;
  public
    constructor Create(var ACode: string; ABaseProject: TInstantCodeProject);
    destructor Destroy; override;
    function AddAttribute(AClass: TInstantCodeClass; Template: TInstantCodeAttribute): TInstantCodeAttribute;
    procedure AddClass(AClass: TInstantCodeClass);
    function AddField(AClass: TInstantCodeClass; Template: TInstantCodeField): TInstantCodeField;
    function AddMethod(AClass: TInstantCodeClass; Template: TInstantCodeMethod): TInstantCodeMethod;
    function AddProc(Template: TInstantCodeProc): TInstantCodeProc;
    function AddProperty(AClass: TInstantCodeClass; Template: TInstantCodeProperty): TInstantCodeProperty;
    procedure AddUses(const AUnitNames: array of string; Scope: TInstantCodeScope = scInterface);
    procedure AdjustForUnitNamesInInterface(var AUnitNames: array of string);
    procedure ChangeAttribute(AClass: TInstantCodeClass; Name: string;
      Template: TInstantCodeAttribute);
    procedure ChangeClass(ChangeInfo: TInstantCodeClassChangeInfo; NewClass: TInstantCodeClass);
    procedure ChangeClassBase(AClass: TInstantCodeClass; const NewBase: string);
    procedure ChangeClassMetadata(OldClass, NewClass: TInstantCodeClass);
    procedure ChangeClassName(AClass: TInstantCodeClass; const NewName: string);
    procedure CloseGap;
    procedure DeleteAttribute(Attribute: TInstantCodeAttribute);
    procedure DeleteClass(AClass: TInstantCodeClass);
    procedure DeleteField(Field: TInstantCodeField);
    procedure DeleteFrom(Pos: TInstantCodePos);
    procedure DeleteMember(Member: TInstantCodeMember);
    procedure DeleteMethod(Method: TInstantCodeMethod);
    procedure DeleteObject(AObject: TInstantCodeObject);
    procedure DeleteProc(Proc: TInstantCodeProc);
    procedure DeleteProperty(Prop: TInstantCodeProperty);
    procedure DeleteText(Len: Integer);
    procedure DeleteTo(Pos: TInstantCodePos);
    procedure EraseObject(AObject: TInstantCodeObject);
    function EnterComment(Back: Boolean = False): Boolean;
    function FindText(Text: string; Back: Boolean = False): Boolean;
    procedure InsertMember(AClass: TInstantCodeClass; Member: TInstantCodeMember;
      Prior, Next: TObject);
    procedure InsertNewLine(Count: Integer = 1); overload;
    procedure InsertNewLine(IndentPos: TInstantCodePos); overload;
    procedure InsertObjectText(AObject: TInstantCodeObject; IndentLevel: Integer = 0);
    procedure InsertText(const Str: string; Wrap: Boolean = False);
    procedure MoveCursor(Count: Integer);
    function NextChar: Char;
    function ReadToken: string;
    procedure RemoveUses(const AUnitNames: array of string; Scope:
        TInstantCodeScope = scInterface);
    procedure ReplaceObjectText(AObject: TInstantCodeObject);
    procedure ReplaceText(Len: Integer; const Str: string);
    procedure Skip(Str: string);
    procedure SkipLine;
    procedure SkipSpace;
    procedure SkipText;
    procedure UpdateClassForward(OldClass, NewClass: TInstantCodeClass);
    procedure UpdateClassRegistration(OldClass, NewClass: TInstantCodeClass);
    procedure UpdateUnit;
    property CursorPos: TInstantCodePos read GetCursorPos write SetCursorPos;
    property InsertMode: TInstantCodeInsertMode read FInsertMode write FInsertMode;
    property Module: TInstantCodeModule read FModule;
    property Project: TInstantCodeProject read FProject;
  end;

  TInstantCodeReaderIdle = procedure(Reader: TInstantCodeReader;
    var Continue: Boolean);

function InstantAttributeTypeToPropertyType(
  AttributeType: TInstantAttributeType): string;
function InstantPropertyTypeToAttributeType(
  const PropType: string): TInstantAttributeType;

const
  InstantCodeReaderIdle: TInstantCodeReaderIdle = nil;

implementation

uses
  InstantRtti, InstantConsts, InstantUtils, DB;

type
  TTypeProcessor = class(TObject)
  private
    FReader: TInstantCodeReader;
    FMetadata: TInstantAttributeMetadata;
  protected
    procedure Error(const Msg: string);
    procedure InternalRead; virtual;
    procedure HandleArgument(Index: Integer; const Argument: string); virtual;
  public
    procedure Read(Reader: TInstantCodeReader; Metadata: TInstantAttributeMetadata);
  end;

  TSimpleTypeProcessor = class(TTypeProcessor)
  end;

  TFloatTypeProcessor = class(TSimpleTypeProcessor)
  protected
    procedure HandleArgument(Index: Integer; const Argument: string); override;
  end;

  TCurrencyTypeProcessor = class(TSimpleTypeProcessor)
  protected
    procedure HandleArgument(Index: Integer; const Argument: string); override;
  end;

  TStringTypeProcessor = class(TSimpleTypeProcessor)
  protected
    procedure HandleArgument(Index: Integer; const Argument: string); override;
  end;

  TDateTimeTypeProcessor = class(TSimpleTypeProcessor)
  protected
    procedure HandleArgument(Index: Integer; const Argument: string); override;
  end;

  TComplexTypeProcessor = class(TTypeProcessor)
  protected
    procedure HandleArgument(Index: Integer; const Argument: string); override;
  end;

  TObjectTypeProcessor = class(TComplexTypeProcessor)
  protected
    procedure InternalRead; override;
  end;

  TContainerTypeProcessor = class(TComplexTypeProcessor)
  protected
    procedure InternalRead; override;
  end;

  TPartTypeProcessor = class(TObjectTypeProcessor)
  protected
    procedure InternalRead; override;
  end;

  TEnumTypeProcessor = class(TSimpleTypeProcessor)
  protected
    procedure InternalRead; override;
    procedure HandleArgument(Index: Integer; const Argument: string); override;
  end;

const
  CRLF = #13#10;

  MetadataInfoID = 'IOMETADATA';
  MetaKeyDefault = 'default';
  MetaKeyDescription = 'description';
  MetaKeyUseNull = 'usenull';
  MetaKeyExternal = 'external';
  MetaKeyVirtual = 'virtual';
  MetaKeyForeignKey = 'foreignkeyfields';
  MetaKeyFormat = 'format';
  MetaKeyIndex = 'index';
  MetaKeyRequired = 'required';
  MetaKeyLocalized = 'localized';
  MetaKeyUnique = 'unique';
  MetaKeyPrimaryKey = 'primarykey';
  MetaKeyMask = 'mask';
  MetaKeyStored = 'stored';
  MetaKeyEmbedded = 'embedded';
  MetaKeyValid = 'valid';
  MetaKeyWidth = 'width';
  MetaKeyLabel = 'label';
  ModuleTypeNames: array[TInstantCodeModuleType] of string =
    ('program', 'unit', 'library');

var
  TypeProcessors: TStringList;

function InstantAttributeTypeToPropertyType(
  AttributeType: TInstantAttributeType): string;
begin
  case AttributeType of
    atInteger:
      Result := 'Integer';
    atFloat:
      Result := 'Double';
    atCurrency:
      Result := 'Currency';
    atBoolean:
      Result := 'Boolean';
    atString, atMemo, atBlob, atGraphic:
      Result := 'string';
    atDateTime:
      Result := 'TDateTime';
    atDate:
      Result := 'TDate';
    atTime:
      Result := 'TTime';
  else
    Result := '';
  end;
end;

function InstantPropertyTypeToAttributeType(
  const PropType: string): TInstantAttributeType;
const
  TypeNames: array[TInstantAttributeType] of string = (
    '',
    'Integer',
    'Double',
    'Currency',
    'Boolean',
    'string',
    'TDateTime',
    '',  // atBlob
    '',  // atMemo
    '',  // atGraphic
    '',  // atPart
    '',  // atReference
    '',  // atParts,
    '',  // atReferences
    'TDate',
    'TTime',
    ''); // atEnum
begin
  for Result := Low(Result) to High(Result) do
    if SameText(PropType, TypeNames[Result]) then
      Exit;
  Result := atUnknown;
end;

procedure CreateTypeProcessors;
begin
  TypeProcessors := TStringList.Create;
  with TypeProcessors do
  begin
    AddObject('Boolean', TSimpleTypeProcessor.Create);
    AddObject('Integer', TSimpleTypeProcessor.Create);
    AddObject('Blob', TSimpleTypeProcessor.Create);
    AddObject('Graphic', TSimpleTypeProcessor.Create);
    AddObject('Float', TFloatTypeProcessor.Create);
    AddObject('Currency', TCurrencyTypeProcessor.Create);
    AddObject('String', TStringTypeProcessor.Create);
    AddObject('Memo', TStringTypeProcessor.Create);
    AddObject('DateTime', TDateTimeTypeProcessor.Create);
    AddObject('Date', TDateTimeTypeProcessor.Create);
    AddObject('Time', TDateTimeTypeProcessor.Create);
    AddObject('Part', TPartTypeProcessor.Create);
    AddObject('Reference', TObjectTypeProcessor.Create);
    AddObject('Parts', TContainerTypeProcessor.Create);
    AddObject('References', TContainerTypeProcessor.Create);
    AddObject('Enum', TEnumTypeProcessor.Create);
  end;
end;

procedure DestroyTypeProcessors;
var
  I: Integer;
begin
  with TypeProcessors do
  try
    for I := 0 to Pred(Count) do
      Objects[I].Free;
  finally
    Free;
  end;
end;

function GetTypeProcessor(Name: string): TTypeProcessor;
var
  I: Integer;
begin
  I := TypeProcessors.IndexOf(Name);
  if I = -1 then
    Result := nil
  else
    Result := TypeProcessors.Objects[I] as TTypeProcessor;
end;

procedure MovePos(var Pos: TInstantCodePos; Delta: TInstantCodePos);
begin
  with Pos do
  begin
    Offset := Offset + Delta.Offset;
    Line := Line + Delta.Line;
    Column := Column + Delta.Column;
  end;
end;

{ TTypeProcessor }

procedure TTypeProcessor.Error(const Msg: string);
begin
  FReader.ErrorMessage(Msg);
end;

procedure TTypeProcessor.HandleArgument(Index: Integer; const Argument: string);
begin
  if Index > 0 then
    Error('Too many arguments')
end;

procedure TTypeProcessor.InternalRead;
var
  Index: Integer;
  Token: string;
begin
  Index := 0;
  with FReader do
    if NextChar = '(' then
    begin
      ReadChar;
      while not Finished do
      begin
        Token := ReadToken;
        if Token = ')' then
        begin
          if Index = 0 then
            HandleArgument(1, Token);
          Break;
        end else if Token = ',' then
        begin
          SkipSpace;
          if NextChar = ')' then
            HandleArgument(Index + 1, '');
        end else
        begin
          Inc(Index);
          HandleArgument(Index, Token);
        end;
      end;
    end else
      try
        HandleArgument(0, '');
      except
        ReadToken;
        Error('Expected ''(''');
      end;
end;

procedure TTypeProcessor.Read(Reader: TInstantCodeReader;
  Metadata: TInstantAttributeMetadata);
begin
  FReader := Reader;
  FMetadata := Metadata;
  InternalRead;
end;

{ TFloatTypeProcessor }

procedure TFloatTypeProcessor.HandleArgument(Index: Integer;
  const Argument: string);
begin
  inherited;
end;

{ TCurrencyTypeProcessor }

procedure TCurrencyTypeProcessor.HandleArgument(Index: Integer;
  const Argument: string);
begin
  inherited;
end;

{ TStringTypeProcessor }

procedure TStringTypeProcessor.HandleArgument(Index: Integer;
  const Argument: string);
begin
  case Index of
    1:
      try
        FMetadata.Size := StrToInt(Argument);
      except
        Error('Expected size as integer');
      end;
  else
    inherited;
  end;
end;

{ TDateTimeTypeProcessor }

procedure TDateTimeTypeProcessor.HandleArgument(Index: Integer;
  const Argument: string);
begin
  case Index of
    1: FMetadata.EditMask := Argument
  else
    inherited;
  end;
end;

{ TComplexTypeProcessor }

procedure TComplexTypeProcessor.HandleArgument(Index: Integer;
  const Argument: string);
begin
  case Index of
    0: Abort;
    1:
      if IsValidIdent(Argument) then
        FMetadata.ObjectClassName := Argument
      else
        Error('Expected class name');
  else
    inherited;
  end;
end;

{ TContainerTypeProcessor }

procedure TContainerTypeProcessor.InternalRead;
var
  Token: string;
begin
  inherited;
  with FReader do
  begin
    while not Finished do
    begin
      SkipSpace;
      if NextChar = ';' then
        Break;
      Token := ReadToken;
      if SameText(Token, MetaKeyStored) then
        FMetadata.StorageName := ReadStringValue;
      if SameText(Token, MetaKeyDefault) then
        FMetadata.IsDefault := True;
      if SameText(Token, MetaKeyExternal) then
      begin
        FMetadata.StorageKind := skExternal;
        FMetadata.ExternalStorageName := ReadStringValue;
      end;
      if SameText(Token, MetaKeyVirtual) then
      begin
        FMetadata.StorageKind := skVirtual;
        FMetadata.ExternalStorageName := ReadStringValue;
      end;
    end;
  end;
end;

{ EInstantCodeError }

constructor EInstantCodeError.Create(const Msg: string;
  AReader: TInstantCodeReader);
begin
  inherited Create(Msg);
  FPosition := AReader.Position;
  FSeverity := AReader.ErrorSeverity;
  FReader := AReader;
end;

{ TInstantCodeObject }

function TInstantCodeObject.AddObject(AObject: TInstantCodeObject): Integer;
begin
  if Assigned(AObject) then
  begin
    Result := ObjectList.Add(AObject);
    AObject.FOwner := Self;
    Changed;
  end else
    Result := -1;
end;

procedure TInstantCodeObject.Assign(Source: TPersistent);
begin
  if Source is TInstantCodeObject then
    with TInstantCodeObject(Source) do
    begin
      Self.Name := Name;
      Self.StartPos := StartPos;
      Self.EndPos := EndPos;
    end;
end;

class function TInstantCodeObject.AtIdentifier(
  Reader: TInstantCodeReader): Boolean;
var
  SavePos: TInstantCodePos;
begin
  SavePos := Reader.Position;
  try
    Result := InternalAtIdentifier(Reader);
  finally
    Reader.Position := SavePos;
  end;
end;

class function TInstantCodeObject.AtInstance(
  Reader: TInstantCodeReader): Boolean;
var
  InstanceName: string;
begin
  Result := AtInstance(Reader, InstanceName);
end;

class function TInstantCodeObject.AtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
var
  SavePos: TInstantCodePos;
begin
  SavePos := Reader.Position;
  try
    Result := InternalAtInstance(Reader, Name);
  finally
    Reader.Position := SavePos;
  end;
end;

procedure TInstantCodeObject.Changed;
begin
  InternalChanged;
  if Assigned(FOwner) then
    FOwner.Changed;
end;

procedure TInstantCodeObject.Clear;
begin
  InternalClear;
  ObjectList.Clear;
end;

constructor TInstantCodeObject.Create(AOwner: TInstantCodeObject);
begin
  if Assigned(AOwner) then
    AOwner.AddObject(Self);
end;

procedure TInstantCodeObject.Delete;
begin
end;

procedure TInstantCodeObject.DeleteObject(AObject: TInstantCodeObject);
begin
  if Assigned(AObject) then
  begin
    AObject.Delete;
    AObject.Free;
  end;
end;

destructor TInstantCodeObject.Destroy;
begin
  FObjectList.Free;
  if Assigned(FOwner) then
    FOwner.ExtractObject(Self);
  inherited;
end;

procedure TInstantCodeObject.ExtractObject(AObject: TInstantCodeObject);
begin
  if Assigned(ObjectList.Extract(AObject)) then
    Changed;
end;

function TInstantCodeObject.Find(Name: string): TInstantCodeObject;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
  begin
    Result := Objects[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeObject.FindFirst(
  AClass: TInstantCodeObjectClass): TInstantCodeObject;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
  begin
    Result := Objects[I];
    if Result.ClassType = AClass then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeObject.FindLast(
  AClass: TInstantCodeObjectClass): TInstantCodeObject;
var
  I: Integer;
begin
  for I := Pred(ObjectCount) downto 0 do
  begin
    Result := Objects[I];
    if Result.ClassType = AClass then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeObject.FindNearest(List: TList;
  Instance: TInstantCodeObject; out Prior, Next: TObject;
  Filter: TInstantCodeFilterEvent; Arg: Pointer;
  Compare: TStringListSortCompare): Boolean;
var
  OrderedList: TStringList;
  AObject: TInstantCodeObject;
  I: Integer;
  Include: Boolean;
begin
  OrderedList := TStringList.Create;
  try
    for I := 0 to Pred(List.Count) do
    begin
      AObject := List[I];
      Include := True;
      if Assigned(Filter) then
        Filter(AObject, Include, Arg);
      if Include then
        OrderedList.AddObject(AObject.Name, AObject);
    end;
    if OrderedList.IndexOfObject(Instance) = -1 then
      OrderedList.AddObject(Instance.Name, Instance);
    if Assigned(Compare) then
      OrderedList.CustomSort(Compare)
    else
      OrderedList.Sorted := True;
    I := OrderedList.IndexOfObject(Instance);
    if I > 0 then
      Prior := OrderedList.Objects[Pred(I)] else
      Prior := nil;
    if I < Pred(OrderedList.Count) then
      Next := OrderedList.Objects[Succ(I)] else
      Next := nil;
    Result := Assigned(Prior) or Assigned(Next);
  finally
    OrderedList.Free;
  end;
end;

procedure TInstantCodeObject.Fixup;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    Objects[I].Fixup;
end;

function TInstantCodeObject.GetAsString: string;
var
  Writer: TInstantCodeWriter;
  Stream: TInstantStringStream;
begin
  Stream := TInstantStringStream.Create('');
  try
    Writer := TInstantCodeWriter.Create(Stream);
    try
      WriteToString(Writer);
    finally
      Writer.Free;
    end;
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function TInstantCodeObject.GetIsEmpty: Boolean;
begin
  Result := False;
end;

function TInstantCodeObject.GetIsFiled: Boolean;
begin
  Result := StartPos.Offset <> 0;
end;

function TInstantCodeObject.GetModule: TInstantCodeModule;
begin
  if Assigned(Owner) then
    Result := Owner.Module
  else
    Result := nil;
end;

function TInstantCodeObject.GetName: string;
begin
  Result := FName;
end;

function TInstantCodeObject.GetObjectCount: Integer;
begin
  Result := ObjectList.Count;
end;

function TInstantCodeObject.GetObjectList: TList;
begin
  if not Assigned(FObjectList) then
    FObjectList := TObjectList.Create;
  Result := FObjectList;
end;

function TInstantCodeObject.GetObjects(Index: Integer): TInstantCodeObject;
begin
  Result := TInstantCodeObject(ObjectList[Index]);
end;

function TInstantCodeObject.GetProject: TInstantCodeProject;
begin
  if Assigned(Module) then
    Result := Module.Owner
  else
    Result := nil;
end;

class function TInstantCodeObject.Identifier: string;
begin
  Result := '';
end;

procedure TInstantCodeObject.InsertObject(Index: Integer;
  AObject: TInstantCodeObject);
begin
  if Assigned(AObject) then
  begin
    ObjectList.Insert(Index, AObject);
    AObject.FOwner := Self;
  end;
end;

class function TInstantCodeObject.InternalAtIdentifier(
  Reader: TInstantCodeReader): Boolean;
begin
  Result := IsIdentifier(Reader.ReadToken);
end;

class function TInstantCodeObject.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
begin
  Result := InternalAtIdentifier(Reader);
  Name := '';
end;

procedure TInstantCodeObject.InternalChanged;
begin
end;

procedure TInstantCodeObject.InternalClear;
begin
end;

procedure TInstantCodeObject.InternalRead(Reader: TInstantCodeReader);
begin
end;

procedure TInstantCodeObject.InternalWrite(Writer: TInstantCodeWriter);
begin
end;

class function TInstantCodeObject.IsIdentifier(const Str: string): Boolean;
begin
  Result := SameText(Str, Identifier);
end;

function TInstantCodeObject.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TInstantCodeObject.Lock;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    Objects[I].Lock;
  Inc(FLockCount);
end;

procedure TInstantCodeObject.Move(Origin, Delta: TInstantCodePos;
  MoveOrigin: Boolean);

  procedure DoMove(var Pos: TInstantCodePos);
  begin
    if (Pos.Offset <> 0) and
      (Pos.Offset > Origin.Offset) or
      ((Pos.Offset = Origin.Offset) and MoveOrigin)
    then
      MovePos(Pos, Delta);
  end;

var
  I: Integer;
begin
  if IsLocked then
    Exit;
  DoMove(FStartPos);
  DoMove(FEndPos);
  for I := 0 to Pred(ObjectCount) do
    Objects[I].Move(Origin, Delta, MoveOrigin);
end;

procedure TInstantCodeObject.Organize;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    Objects[I].Organize;
end;

procedure TInstantCodeObject.Read(Reader: TInstantCodeReader);
begin
  FStartPos := Reader.Position;
  InternalRead(Reader);
  FEndPos := Reader.Position;
end;

procedure TInstantCodeObject.ReadObjects(Reader: TInstantCodeReader;
  Classes: array of TInstantCodeObjectClass);
var
  AObject: TInstantCodeObject;
  I: Integer;
  SavePos: TInstantCodePos;
begin
  while not Reader.Finished do
  begin
    SavePos := Reader.Position;
    Reader.SkipSpace;
    AObject := nil;
    for I := Low(Classes) to High(Classes) do
      if Classes[I].AtInstance(Reader) then
      begin
        AObject := Classes[I].Create(Self);
        Break;
      end;
    if Assigned(AObject) then
      AObject.Read(Reader)
    else begin
      Reader.Position := SavePos;
      Break;
    end;
  end;
end;

procedure TInstantCodeObject.ReadRes(Stream: TInstantStream);
begin
end;

procedure TInstantCodeObject.Reset;
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    Objects[I].Reset;
end;

procedure TInstantCodeObject.SetAsString(const Value: string);
var
  Stream: TInstantStringStream;
  Reader: TInstantCodeReader;
begin
  Stream := TInstantStringStream.Create(Value);
  try
    Reader := TInstantCodeReader.Create(Stream);
    try
      Read(Reader);
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TInstantCodeObject.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TInstantCodeObject.Sort;
begin
  SortByName(ObjectList);
end;

function CompareNames(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(
    TInstantCodeObject(Item1).Name,
    TInstantCodeObject(Item2).Name);
end;

procedure TInstantCodeObject.SortByName(List: TList);
begin
  List.Sort(CompareNames);
end;

procedure TInstantCodeObject.Unlock;
var
  I: Integer;
begin
  if IsLocked then
  begin
    for I := 0 to Pred(ObjectCount) do
      Objects[I].Unlock;
    Dec(FLockCount);
  end;
end;

procedure TInstantCodeObject.Write(Writer: TInstantCodeWriter);
begin
  if IsEmpty then
  begin
    FStartPos := Writer.Position;
    FEndPos := Writer.Position;
  end else
  begin
    Writer.WriteMargin;
    FStartPos := Writer.Position;
    InternalWrite(Writer);
    FEndPos := Writer.Position;
  end;
end;

procedure TInstantCodeObject.WriteObjects(Writer: TInstantCodeWriter);
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
    Objects[I].Write(Writer);
end;

procedure TInstantCodeObject.WriteToString(Writer: TInstantCodeWriter);
begin
  Write(Writer);
end;

{ TInstantCodeObjectLink }

procedure TInstantCodeObjectLink.AssignInstance(Value: TInstantCodeObject);
begin
  if Assigned(FInstance) then
    Detach;
  FInstance := Value;
  if Assigned(FInstance) then
    Attach
  else
    FName := '';
end;

procedure TInstantCodeObjectLink.Attach;
begin
end;

constructor TInstantCodeObjectLink.Create(AOwner: TInstantCodeObject);
begin
  FOwner := AOwner;
end;

procedure TInstantCodeObjectLink.Detach;
begin
end;

function TInstantCodeObjectLink.FindInstance: TInstantCodeObject;
begin
  Result := nil;
end;

function TInstantCodeObjectLink.GetInstance: TInstantCodeObject;
begin
  if not Assigned(FInstance) then
    Resolve;
  Result := FInstance;
end;

function TInstantCodeObjectLink.GetModule: TInstantCodeModule;
begin
  Result := Owner.Module;
end;

function TInstantCodeObjectLink.GetName: string;
begin
  if Assigned(FInstance) then
    Result := FInstance.Name
  else
    Result := FName;
end;

procedure TInstantCodeObjectLink.Resolve;
begin
  if FName <> '' then
    Instance := FindInstance;
end;

procedure TInstantCodeObjectLink.SetInstance(Value: TInstantCodeObject);
begin
  if Value <> FInstance then
    AssignInstance(Value);
end;

procedure TInstantCodeObjectLink.SetName(const Value: string);
begin
  if not SameText(Value, Name) then
    Instance := nil;
  FName := Value;
end;

{ TInstantCodeText }

procedure TInstantCodeText.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeText then
    with TInstantCodeText(Source) do
      Self.Lines.Assign(Lines);
end;

constructor TInstantCodeText.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FLines := TStringList.Create;
end;

destructor TInstantCodeText.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TInstantCodeText.GetText: string;
begin
  Result := FLines.Text;
end;

procedure TInstantCodeText.InternalWrite(Writer: TInstantCodeWriter);
var
  I: Integer;
begin
  for I := 0 to Pred(FLines.Count) do
    Writer.WriteLn(FLines[I]);
end;

procedure TInstantCodeText.Reset;
begin
  inherited;
  FLines.Clear;
end;

procedure TInstantCodeText.SetText(Value: string);
begin
  FLines.Text := Value;
end;

{ TInstantCodeUses }

procedure TInstantCodeUses.InternalWrite(Writer: TInstantCodeWriter);
begin
  Writer.Write(Name);
end;

{ TInstantCodeType }

constructor TInstantCodeType.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  if IsLinkable then
    Module.InsertType(Self);
end;

destructor TInstantCodeType.Destroy;
begin
  if IsLinkable then
    Module.RemoveType(Self);
  inherited;
end;

function TInstantCodeType.GetIsLinkable: Boolean;
begin
  Result := Assigned(Owner);
end;

class function TInstantCodeType.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
begin
  Name := Reader.ReadToken;
  Result := (Reader.ReadToken = '=') and InternalAtIdentifier(Reader);
end;

procedure TInstantCodeType.InternalRead(Reader: TInstantCodeReader);
begin
  Name := Reader.ReadToken;
  if Reader.ReadToken <> '=' then
    Reader.ErrorExpected('=');
end;

procedure TInstantCodeType.InternalWrite(Writer: TInstantCodeWriter);
begin
  Writer.Write(Name + ' = ');
end;

{ TInstantCodeRecord }

class function TInstantCodeRecord.Identifier: string;
begin
  Result := 'record';
end;

class function TInstantCodeRecord.InternalAtIdentifier(
  Reader: TInstantCodeReader): Boolean;
var
  Token: string;
begin
  Token := Reader.ReadToken;
  Result := IsIdentifier(Token) or
    (SameText(Token, 'packed') and IsIdentifier(Reader.ReadToken));
end;

procedure TInstantCodeRecord.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  Reader.GotoToken('end', True);
  Reader.ReadEndOfStatement;
end;

{ TInstantCodeTypeDef }

class function TInstantCodeTypeDef.Identifier: string;
begin
  Result := 'type';
end;

procedure TInstantCodeTypeDef.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  FTypeDef := Reader.ReadStatement;
end;

{ TInstantCodeTypeLink }

function TInstantCodeTypeLink.FindInstance: TInstantCodeObject;
begin
  if Assigned(Module) then
    Result := Module.FindType(Name)
  else
    Result := nil;
end;

function TInstantCodeTypeLink.GetInstance: TInstantCodeType;
begin
  Result := inherited Instance as TInstantCodeType;
end;

procedure TInstantCodeTypeLink.SetInstance(Value: TInstantCodeType);
begin
  inherited Instance := Value;
end;

{ TInstantCodeEnum }

constructor TInstantCodeEnum.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor TInstantCodeEnum.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TInstantCodeEnum.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TInstantCodeEnum.GetItems(Index: Integer): string;
begin
  Result := FItems[Index];
end;

class function TInstantCodeEnum.Identifier: string;
begin
  Result := '(';
end;

procedure TInstantCodeEnum.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  Reader.ReadStatement;
end;

procedure TInstantCodeEnum.InternalWrite(Writer: TInstantCodeWriter);
var
  S: string;
  I: Integer;
begin
  if Count > 0 then
  begin
    inherited;
    S := '';
    for I := 0 to Pred(Count) do
    begin
      if I > 0 then
        S := S + ', ';
      S := S + Items[I];
    end;
    Writer.WriteFmt('(%s);', [S]);
  end;
end;

{ TInstantCodeSet }

class function TInstantCodeSet.Identifier: string;
begin
  Result := 'set';
end;

procedure TInstantCodeSet.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  Reader.ReadStatement;
end;

{ TInstantCodeProcType }

class function TInstantCodeProcType.Identifier: string;
begin
  Result := 'procedure';
end;

class function TInstantCodeProcType.IsIdentifier(const Str: string): Boolean;
begin
  Result := SameText(Str, 'procedure') or SameText(Str, 'function');
end;

procedure TInstantCodeProcType.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  Reader.ReadToken;
  Reader.SkipSpace;
  if Reader.ReadChar = '(' then
    Reader.ReadNext(')');
  Reader.ReadStatement;
end;

{ TInstantCodeUnknownType }

class function TInstantCodeUnknownType.InternalAtIdentifier(
  Reader: TInstantCodeReader): Boolean;
begin
  Result := True;
end;

procedure TInstantCodeUnknownType.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  Reader.ReadStatement;
end;

{ TInstantCodeSymbol }

procedure TInstantCodeSymbol.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeSymbol then
    with TInstantCodeSymbol(Source) do
      Self.TypeName := TypeName;
end;

constructor TInstantCodeSymbol.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FTypeLink := TInstantCodeTypeLink.Create(Self);
end;

destructor TInstantCodeSymbol.Destroy;
begin
  FTypeLink.Free;
  inherited;
end;

function TInstantCodeSymbol.GetTypeName: string;
begin
  Result := FTypeLink.Name;
end;

function TInstantCodeSymbol.GetTypeValue: TInstantCodeType;
begin
  Result := FTypeLink.Instance;
end;

class function TInstantCodeSymbol.InternalAtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
begin
  Name := Reader.ReadToken;
  Reader.SkipSpace;
  Result := InstantCharInSet(Reader.ReadChar, [':', ',']);
end;

procedure TInstantCodeSymbol.InternalRead(Reader: TInstantCodeReader);
var
  SavePos: TInstantCodePos;
begin
  Name := Reader.ReadToken;
  SavePos := Reader.Position;
  Reader.SkipBlanks;
  if Reader.ReadToken = ':' then
    TypeName := Reader.ReadToken
  else begin
    TypeName := '';
    Reader.Position := SavePos;
  end;
end;

procedure TInstantCodeSymbol.InternalWrite(Writer: TInstantCodeWriter);
begin
  Writer.WriteFmt('%s: %s', [Name, TypeName]);
end;

procedure TInstantCodeSymbol.SetTypeName(const Value: string);
begin
  FTypeLink.Name := Value;
end;

procedure TInstantCodeSymbol.SetTypeValue(const Value: TInstantCodeType);
begin
  FTypeLink.Instance := Value;
end;

{ TInstantCodeVar }

procedure TInstantCodeVar.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  Reader.ReadStatement;
end;

{ TInstantCodeParameter }

procedure TInstantCodeParameter.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeParameter then
    with TInstantCodeParameter(Source) do
      Self.Options := Options;
end;

function TInstantCodeParameter.GetOwner: TInstantCodeParameters;
begin
  Result := inherited Owner as TInstantCodeParameters;
end;

procedure TInstantCodeParameter.InternalRead(Reader: TInstantCodeReader);
var
  SavePos: TInstantCodePos;
  Token: string;
begin
  while not Reader.Finished do
  begin
    SavePos := Reader.Position;
    Token := Reader.ReadToken;
    if SameText(Token, 'const') then
      Options := Options + [poConst]
    else if SameText(Token, 'var') then
      Options := Options + [poVar]
    else if SameText(Token, 'out') then
      Options := Options + [poOut]
    else begin
      Reader.Position := SavePos;
      Break;
    end;
  end;
  inherited;
end;

procedure TInstantCodeParameter.InternalWrite(Writer: TInstantCodeWriter);
begin
  if poConst in Options then
    Writer.Write('const ')
  else if poVar in Options then
    Writer.Write('var ')
  else if poOut in Options then
    Writer.Write('out ');
  inherited;
end;

{ TInstantCodeMember }

procedure TInstantCodeMember.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeMember then
    with TInstantCodeMember(Source) do
      Self.Visibility := Visibility;
end;

constructor TInstantCodeMember.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  if Assigned(HostClass) then
    HostClass.InsertMember(Self);
  Visibility := viPrivate;
end;

destructor TInstantCodeMember.Destroy;
begin
  if Assigned(HostClass) then
    HostClass.RemoveMember(Self);
  inherited;
end;

function TInstantCodeMember.GetHostClass: TInstantCodeClass;
begin
  if Assigned(Owner) then
    Result := Owner.Owner
  else
    Result := nil;
end;

function TInstantCodeMember.GetOwner: TInstantCodeMembers;
begin
  Result := inherited Owner as TInstantCodeMembers;
end;

class function TInstantCodeMember.InternalAtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
begin
  Result := inherited InternalAtInstance(Reader, Name);
  if Result then
    Name := Reader.ReadToken;
end;

{ TInstantCodeMembers }

function TInstantCodeMembers.GetCount: Integer;
begin
  Result := ObjectCount;
end;

function TInstantCodeMembers.GetItems(Index: Integer): TInstantCodeMember;
begin
  Result := Objects[Index] as TInstantCodeMember;
end;

function TInstantCodeMembers.GetOwner: TInstantCodeClass;
begin
  Result := inherited Owner as TInstantCodeClass;
end;

class function TInstantCodeMembers.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
begin
  Result :=
    TInstantCodeField.AtInstance(Reader) or
    TInstantCodeMethod.AtInstance(Reader) or
    TInstantCodeProperty.AtInstance(Reader) or
    TInstantCodeMemberRttiAttribute.AtInstance(Reader);
end;

procedure TInstantCodeMembers.InternalRead(Reader: TInstantCodeReader);
begin
  ReadObjects(Reader,
    [TInstantCodeField, TInstantCodeMethod, TInstantCodeProperty,
     TInstantCodeMemberRttiAttribute]);
end;

procedure TInstantCodeMembers.InternalWrite(Writer: TInstantCodeWriter);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    if I > 0 then
      Writer.WriteLn;
    Items[I].Write(Writer);
  end;
end;

function CompareMembers(Item1, Item2: Pointer): Integer; overload;
const
  Classes: array[0..2] of TClass =
    (TInstantCodeField, TInstantCodeMethod, TInstantCodeProperty);
var
  Member1, Member2: TInstantCodeMember;
  I, Value1, Value2: Integer;
begin
  Member1 := Item1;
  Member2 := Item2;
  Value1 := 0;
  Value2 := 0;
  for I := Low(Classes) to High(Classes) do
  begin
    if Member1 is Classes[I] then
      Value1 := I;
    if Member2 is Classes[I] then
      Value2 := I;
  end;
  if Value1 < Value2 then
    Result := -1
  else if Value1 > Value2 then
    Result := 1
  else
    Result := CompareText(Member1.Name, Member2.Name);
end;

function CompareMembersList(List: TStringList;
  Index1, Index2: Integer): Integer;
begin
  Result := CompareMembers(List.Objects[Index1], List.Objects[Index2]);
end;

procedure TInstantCodeMembers.Sort;
begin
  ObjectList.Sort(CompareMembers);
end;

{ TInstantCodeField }

procedure TInstantCodeField.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeField then
    with TInstantCodeField(Source) do
      Self.TypeName := TypeName;
end;

constructor TInstantCodeField.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FVariable := TInstantCodeVar.Create(nil);
end;

destructor TInstantCodeField.Destroy;
begin
  FVariable.Free;
  inherited;
end;

function TInstantCodeField.GetName: string;
begin
  Result := FVariable.Name;
end;

function TInstantCodeField.GetTypeName: string;
begin
  Result := FVariable.TypeName;
end;

class function TInstantCodeField.InternalAtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
begin
  Result := TInstantCodeVar.AtInstance(Reader, Name);
end;

procedure TInstantCodeField.InternalRead(Reader: TInstantCodeReader);
begin
  FVariable.Read(Reader);
end;

procedure TInstantCodeField.InternalWrite(Writer: TInstantCodeWriter);
begin
  inherited;
  FVariable.Write(Writer);
  Writer.Write(';');
end;

procedure TInstantCodeField.SetName(const Value: string);
begin
  FVariable.Name := Value;
end;

procedure TInstantCodeField.SetTypeName(const Value: string);
begin
  FVariable.TypeName := Value;
end;

{ TInstantCodeMethod }

procedure TInstantCodeMethod.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeMethod then
    with TInstantCodeMethod(Source) do
      Self.Prototype.Assign(Prototype);
end;

constructor TInstantCodeMethod.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FPrototype := TInstantCodePrototype.Create(nil);
  FProcLink := TInstantCodeProcLink.Create(Self);
end;

procedure TInstantCodeMethod.Delete;
begin
  DeleteObject(Proc);
  Proc := nil;
end;

destructor TInstantCodeMethod.Destroy;
begin
  FProcLink.Free;
  FPrototype.Free;
  inherited;
end;

function TInstantCodeMethod.GetName: string;
begin
  Result := FPrototype.Name;
end;

function TInstantCodeMethod.GetProc: TInstantCodeProc;
begin
  Result := FProcLink.Instance;
end;

function TInstantCodeMethod.GetProcName: string;
begin
  if Assigned(HostClass) then
    Result := HostClass.Name + '.' + Name
  else
    Result := Name;
end;

class function TInstantCodeMethod.InternalAtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
begin
  Result := TInstantCodePrototype.AtInstance(Reader, Name);
end;

procedure TInstantCodeMethod.InternalRead(Reader: TInstantCodeReader);
var
  SavePos: TInstantCodePos;
begin
  FPrototype.Read(Reader);
  Name := FPrototype.Name;
  SavePos := Reader.Position;
  if SameText(Reader.ReadToken, 'message') then
    Reader.ReadStatement
  else
    Reader.Position := SavePos;
end;

procedure TInstantCodeMethod.InternalWrite(Writer: TInstantCodeWriter);
begin
  if Assigned(Proc) then
    Proc.Header.Write(Writer, Self)
  else
    FPrototype.Write(Writer);
end;

procedure TInstantCodeMethod.SetName(const Value: string);
begin
  FPrototype.Name := Value;
  FProcLink.Name := ProcName;
end;

procedure TInstantCodeMethod.SetProc(const Value: TInstantCodeProc);
begin
  FProcLink.Instance := Value;
end;

procedure TInstantCodeMethod.UpdateProcName;
begin
  if Assigned(Proc) then
    Proc.Name := ProcName;
end;

procedure TInstantCodeMethod.UpdatePrototype;
var
  Name: string;
begin
  if not Assigned(Proc) then
    Exit;
  Name := Prototype.Name;
  try
    Prototype.Assign(Proc.Header);
  finally
    Prototype.Name := Name;
  end;
end;

{ TInstantCodeProperty }

procedure TInstantCodeProperty.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeProperty then
    with TInstantCodeProperty(Source) do
    begin
      Self.DefaultValue := DefaultValue;
      Self.GetterName := GetterName;
      Self.SetterName := SetterName;
      Self.IsArray := IsArray;
      Self.IsDefault := IsDefault;
      Self.Stored := Stored;
      Self.TypeName := TypeName;
    end;
end;

constructor TInstantCodeProperty.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FTypeLink := TInstantCodeTypeLink.Create(Self);
end;

procedure TInstantCodeProperty.Delete;
begin
  DeleteObject(FGetter);
  FGetter := nil;
  DeleteObject(FSetter);
  FSetter := nil;
end;

destructor TInstantCodeProperty.Destroy;
begin
  FTypeLink.Free;
  inherited;
end;

function TInstantCodeProperty.FindAttribute: TInstantCodeAttribute;
begin
  if Assigned(HostClass) then
    Result := HostClass.FindAttribute(Name)
  else
    Result := nil;
end;

function TInstantCodeProperty.GetAttributeType: TInstantAttributeType;
var
  Attribute: TInstantCodeAttribute;
begin
  Attribute := FindAttribute;
  if Assigned(Attribute) then
    Result := Attribute.AttributeType
  else begin
    Result := InstantPropertyTypeToAttributeType(TypeName);
    if Result = atUnknown then
      if Assigned(Project.FindClass(TypeName)) then
        Result := atReference
      else
        Result := atString; // Enum
  end;
end;

function TInstantCodeProperty.GetGetter: TInstantCodeMethod;
begin
  if not Assigned(FGetter) and (GetterName <> '') then
    FGetter := HostClass.FindMethod(GetterName);
  Result := FGetter;
end;

function TInstantCodeProperty.GetGetterName: string;
begin
  if Assigned(FGetter) then
    Result := FGetter.Name
  else
    Result := FGetterName;
end;

function TInstantCodeProperty.GetReadOnly: Boolean;
begin
  Result := SetterName = '';
end;

function TInstantCodeProperty.GetSetter: TInstantCodeMethod;
begin
  if not Assigned(FSetter) and not ReadOnly and (SetterName <> '') then
    FSetter := HostClass.FindMethod(SetterName);
  Result := FSetter;
end;

function TInstantCodeProperty.GetSetterName: string;
begin
  if Assigned(FSetter) then
    Result := FSetter.Name
  else
    Result := FSetterName;
end;

function TInstantCodeProperty.GetTypeName: string;
begin
  Result := FTypeLink.Name;
end;

function TInstantCodeProperty.GetTypeValue: TInstantCodeType;
begin
  Result := FTypeLink.Instance;
end;

class function TInstantCodeProperty.Identifier: string;
begin
  Result := 'property';
end;

procedure TInstantCodeProperty.InternalRead(Reader: TInstantCodeReader);
var
  Token: string;
  SavePos: TInstantCodePos;
begin
  Reader.ReadToken;
  Name := Reader.ReadToken;
  Reader.SkipSpace;
  if Reader.NextChar = '[' then
  begin
    IsArray := True;
    Reader.ReadChar;
    Reader.ReadNext(']');
  end;
  if Reader.NextChar = ':' then
  begin
    Reader.ReadToken;
    TypeName := Reader.ReadToken;
  end;
  while not Reader.Finished do
  begin
    Token := LowerCase(Reader.ReadToken);
    if Token = MetaKeyIndex then
      Reader.ReadToken
    else if Token = 'read' then
      GetterName := Reader.ReadToken
    else if Token = 'write' then
      SetterName := Reader.ReadToken
    else if Token = MetaKeyDefault then
      DefaultValue := Reader.ReadRestOfStatement
    else if Token = MetaKeyStored then
      Stored := Reader.ReadToken
    else if Token = ';' then
      Break;
  end;
  SavePos := Reader.Position;
  IsDefault := SameText(Reader.ReadToken, MetaKeyDefault);
  if IsDefault then
    Reader.ReadEndOfStatement(True)
  else
    Reader.Position := SavePos;
end;

procedure TInstantCodeProperty.InternalWrite(Writer: TInstantCodeWriter);
var
  S: string;
begin
  inherited;
  if IsArray then
    S := Name + '[Index: Integer]' else
    S := Name;
  Writer.WriteFmt('%s %s: %s', [Identifier, S, TypeName]);
  if GetterName <> '' then
    Writer.Write(' read ' + GetterName);
  if SetterName <> '' then
    Writer.Write (' write ' + SetterName);
  Writer.Write(';');
end;

procedure TInstantCodeProperty.SetGetter(Value: TInstantCodeMethod);
begin
  FGetter := Value;
  FGetterName := '';
end;

procedure TInstantCodeProperty.SetGetterName(const Value: string);
begin
  FGetterName := Value;
  if Assigned(FGetter) then
    FGetter.Name := Value
end;

procedure TInstantCodeProperty.SetSetter(Value: TInstantCodeMethod);
begin
  FSetter := Value;
  FSetterName := '';
end;

procedure TInstantCodeProperty.SetSetterName(const Value: string);
begin
  FSetterName := Value;
  if Assigned(FSetter) then
    FSetter.Name := Value
end;

procedure TInstantCodeProperty.SetTypeName(const Value: string);
begin
  FTypeLink.Name := Value;
end;

function _RttiAttributeAtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
begin
  Result := Reader.ReadChar = '[';
  if Result then
    Name := Reader.ReadToken
  else
    Name := '';
end;

procedure _RttiAttributeRead(Reader: TInstantCodeReader);
var
  BracketCount: Integer;
  C: Char;
begin
  BracketCount := 0;
  repeat
    C := Reader.ReadChar;
    if C = '[' then
      Inc(BracketCount)
    else if C = ']' then
      Dec(BracketCount);
  until BracketCount = 0;
end;

{ TInstantCodeMemberRttiAttribute }

class function TInstantCodeMemberRttiAttribute.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
begin
  Result := _RttiAttributeAtInstance(Reader, Name);
end;

procedure TInstantCodeMemberRttiAttribute.InternalRead(Reader: TInstantCodeReader);
begin
  _RttiAttributeRead(Reader);
end;

{ TInstantCodeClassRttiAttribute }

class function TInstantCodeClassRttiAttribute.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
begin
  Result := _RttiAttributeAtInstance(Reader, Name);
end;

procedure TInstantCodeClassRttiAttribute.InternalRead(
  Reader: TInstantCodeReader);
begin
  _RttiAttributeRead(Reader);
end;

{ TInstantCodeAttributeTailor }

procedure TInstantCodeAttributeTailor.AddCountProp;
begin
  FCountProp := Attribute.FindCountProp;
  if not Assigned(FCountProp) then
  begin
    FCountProp := HostClass.AddProperty(Attribute.Visibility);
    with FCountProp do
    begin
      Name := Attribute.CountPropName;
      TypeName := 'Integer';
      Getter := CreateMethod('Get' + Name, viPrivate);
      with Getter.Proc do
      begin
        ResultTypeName := TypeName;
        Body.Text := CountGetterCode;
      end;
      Getter.UpdatePrototype;
    end;
  end;
end;

procedure TInstantCodeAttributeTailor.AddField;
begin
  FField := Attribute.FindField;
  if not Assigned(FField) then
  begin
    FField := HostClass.AddField(viDefault);
    with Field do
    begin
      Name := FieldName;
      TypeName := Attribute.AttributeClassName;
    end;
  end;
end;

procedure TInstantCodeAttributeTailor.AddMethods;
var
  MT: TInstantCodeContainerMethodType;
begin
  for MT := Low(MT) to High(MT) do
    if MT in MethodTypes then
    begin
      MethodByType[MT] := CreateMethod(MT);
      MethodList.Add(MethodByType[MT]);
    end;
end;

procedure TInstantCodeAttributeTailor.AddValueProp;
begin
  FValueProp := Attribute.FindValueProp;
  if not Assigned(FValueProp) then
  begin
    FValueProp := HostClass.AddProperty(Attribute.Visibility);
    with FValueProp do
    begin
      Name := Attribute.ValuePropName;
      TypeName := Attribute.PropTypeName;
      IsArray := Self.IsArray;
      Getter := CreateMethod('Get' + Name, viPrivate);
      with Getter.Proc do
      begin
        ResultTypeName := TypeName;
        if IsArray then
          Parameters.AddParameter('Index', 'Integer', []);
        Body.Text := ValueGetterCode;
      end;
      Getter.UpdatePrototype;
      if not Self.ReadOnly then
      begin
        Setter := CreateMethod('Set' + Name, viPrivate);
        with Setter.Proc do
        begin
          if IsArray then
            Parameters.AddParameter('Index', 'Integer', []);
          with Parameters.AddParameter('Value', TypeName, []) do
            if TypeName = 'string' then
              Options := Options + [poConst];
          Body.Text := ValueSetterCode;
        end;
        Setter.UpdatePrototype;
      end;
    end;
  end;
end;

procedure TInstantCodeAttributeTailor.Apply;
begin
  DeleteItem(FField);
  DeleteItem(FValueProp);
  DeleteItem(FCountProp);
  DeleteMethods;
  if Assigned(HostClass) then
  begin
    AddField;
    AddValueProp;
    if IsArray then
    begin
      AddCountProp;
      AddMethods;
    end;
  end else
    AddMethods;
end;

constructor TInstantCodeAttributeTailor.Create(
  AAttribute: TInstantCodeAttribute);
begin
  FMethodTypes := [mtAdd, mtClear, mtDelete, mtIndexOf, mtInsert, mtRemove];
  FAttribute := AAttribute;
end;

function TInstantCodeAttributeTailor.CreateMethod(
  const AName: string; Visibility: TInstantCodeVisibility): TInstantCodeMethod;
var
  Method: TInstantCodeMethod;
  Proc: TInstantCodeProc;
begin
  if Assigned(HostClass) then
  begin
    Method := HostClass.AddMethod(Visibility);
    Proc := HostClass.Module.ImplementationSection.AddProc;
    Proc.Name := HostClass.Name + '.' + AName;
  end else
  begin
    Method := TInstantCodeMethod.Create(nil);
    Proc := TInstantCodeProc.Create(nil);
    Proc.Name := AName;
  end;
  try
    Method.Name := AName;
    Method.Proc := Proc;
    Result := Method;
  except
    Proc.Free;
    Method.Free;
    raise;
  end;
end;

function TInstantCodeAttributeTailor.CreateMethod(const AName, Prefix: string;
  Flags: TInstantCodeContainerMethodFlags): TInstantCodeMethod;
var
  Code: string;

  procedure AddParam(Parameters: TInstantCodeParameters;
    const AName, ATypeName: string);
  begin
    if Parameters.Count = 0 then
      Code := Code + '(' else
      Code := Code + ', ';
    Code := Code + AName;
    with Parameters.Add do
    begin
      Name := AName;
      TypeName := ATypeName;
    end;
  end;

begin
  Result := CreateMethod(AName, Attribute.Visibility);
  with Result do
  begin
    if mfResult in Flags then
    begin
      Proc.ResultTypeName := 'Integer';
      Code := 'Result := ';
    end else
      Code := '';
    Code := Code + FieldName + '.' + Prefix;
    if mfIndex in Flags then
      AddParam(Proc.Parameters, 'Index', 'Integer');
    if mfValue in Flags then
      AddParam(Proc.Parameters, SingularName, Attribute.PropTypeName);
    if Proc.Parameters.Count > 0 then
      Code := Code + ')';
    Code := Code + ';';
    Proc.Body.Text := Code;
    UpdatePrototype;
  end;
end;

function TInstantCodeAttributeTailor.CreateMethod(
  MethodType: TInstantCodeContainerMethodType): TInstantCodeMethod;
var
  Flags: TInstantCodeContainerMethodFlags;
begin
  Flags := [];
  if MethodType in [mtAdd, mtIndexOf, mtRemove, mtInsert] then
    Include(Flags, mfValue);
  if MethodType in [mtAdd, mtIndexOf, mtRemove] then
    Include(Flags, mfResult);
  if MethodType in [mtDelete, mtInsert] then
    Include(Flags, mfIndex);
  Result := CreateMethod(Attribute.GetMethodName(MethodType),
    Attribute.GetMethodPrefix(MethodType), Flags);
end;

procedure TInstantCodeAttributeTailor.DeleteItem(var Item);
var
  AObject: TObject;
begin
  AObject := TObject(Item);
  if AObject is TInstantCodeObject then
    TInstantCodeObject(AObject).Delete;
  FreeAndNil(AObject);
end;

procedure TInstantCodeAttributeTailor.DeleteMethods;
begin
  DeleteItem(FAddMethod);
  FAddMethod := nil;
  DeleteItem(FClearMethod);
  FClearMethod := nil;
  DeleteItem(FDeleteMethod);
  FDeleteMethod := nil;
  DeleteItem(FIndexOfMethod);
  FIndexOfMethod := nil;
  DeleteItem(FInsertMethod);
  FInsertMethod := nil;
  DeleteItem(FRemoveMethod);
  FRemoveMethod := nil;
end;

destructor TInstantCodeAttributeTailor.Destroy;
begin
  FMethodList.Free;
  inherited;
end;

function TInstantCodeAttributeTailor.GetCountGetterCode: string;
begin
  Result := Format('Result := %s.Count;', [FieldName]);
end;

function TInstantCodeAttributeTailor.GetField: TInstantCodeField;
begin
  Result := FField;
end;

function TInstantCodeAttributeTailor.GetFieldName: string;
begin
  Result := Attribute.FieldName;
end;

function TInstantCodeAttributeTailor.GetFieldValueName: string;
begin
  Result := FieldName;
  if IsArray then
    Result := Result + '[Index]'
  else
    Result := Result + '.Value';
end;

function TInstantCodeAttributeTailor.GetHostClass: TInstantCodeClass;
begin
  Result := Attribute.HostClass;
end;

function TInstantCodeAttributeTailor.GetMethodByType(
  MethodType: TInstantCodeContainerMethodType): TInstantCodeMethod;
begin
  case MethodType of
    mtAdd:
      Result := FAddMethod;
    mtClear:
      Result := FClearMethod;
    mtDelete:
      Result := FDeleteMethod;
    mtIndexOf:
      Result := FIndexOfMethod;
    mtInsert:
      Result := FInsertMethod;
    mtRemove:
      Result := FRemoveMethod;
  else
    Result := nil;
  end;
end;

function TInstantCodeAttributeTailor.GetMethodCount: Integer;
begin
  Result := MethodList.Count;
end;

function TInstantCodeAttributeTailor.GetMethodList: TList;
begin
  if not Assigned(FMethodList) then
    FMethodList := TList.Create;
  Result := FMethodList;
end;

function TInstantCodeAttributeTailor.GetMethods(
  Index: Integer): TInstantCodeMethod;
begin
  Result := MethodList[Index];
end;

function TInstantCodeAttributeTailor.GetSingularName: string;
begin
  Result := Attribute.SingularName;
end;

function TInstantCodeAttributeTailor.GetValueGetterCode: string;
begin
  Result := 'Result := ';
  if not Attribute.IsEnum then
  begin
    Result := Result + FieldValueName;
    if Attribute.IsComplex then
      Result := Result + ' as ' + Attribute.PropTypeName;
  end else
    Result := Result + Attribute.Metadata.EnumName + '(' + FieldValueName + ')';

  Result := Result + ';';
end;

function TInstantCodeAttributeTailor.GetValueSetterCode: string;
const
  SetterText: array[Boolean] of string =
    ('Value;', 'Ord(Value)');
begin
  Result := FieldValueName + ' := ' + SetterText[Attribute.IsEnum] + ';';
end;

procedure TInstantCodeAttributeTailor.SetIsArray(Value: Boolean);
begin
  if Value <> FIsArray then
  begin
    FIsArray := Value;
    Update;
  end;
end;

procedure TInstantCodeAttributeTailor.SetMethodByType(
  MethodType: TInstantCodeContainerMethodType; Value: TInstantCodeMethod);
begin
  case MethodType of
    mtAdd:
      FAddMethod := Value;
    mtClear:
      FClearMethod := Value;
    mtDelete:
      FDeleteMethod := Value;
    mtIndexOf:
      FIndexOfMethod := Value;
    mtInsert:
      FInsertMethod := Value;
    mtRemove:
      FRemoveMethod := Value;
  end;
end;

procedure TInstantCodeAttributeTailor.SetMethodTypes(
  Value: TInstantCodeContainerMethodTypes);
begin
  if Value <> FMethodTypes then
  begin
    FMethodTypes := Value;
    Update;
  end;
end;

procedure TInstantCodeAttributeTailor.SetReadOnly(const Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    Update;
  end;
end;

procedure TInstantCodeAttributeTailor.Update;
begin
end;

{ TInstantCodeAttribute }

procedure TInstantCodeAttribute.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeAttribute then
    with TInstantCodeAttribute(Source) do
    begin
      Self.IsDefault := IsDefault;
      Self.IsDescription := IsDescription;
      Self.IsIndexed := IsIndexed;
      Self.IsRequired := IsRequired;
      Self.IsUnique := IsUnique;
      Self.IsPrimaryKey := IsPrimaryKey;
      Self.ReadOnly := ReadOnly;
      Self.SingularName := SingularName;
      Self.Visibility := Visibility;
      Self.Metadata.Assign(Metadata);
    end;
end;

constructor TInstantCodeAttribute.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  if Assigned(Owner) then
    Owner.FAttributes.Add(Self);
  FVisibility := viPublished;
end;

procedure TInstantCodeAttribute.Delete;
begin
  DeleteObject(FindField);
  DeleteObject(FindValueProp);
  DeleteObject(FindCountProp);
end;

destructor TInstantCodeAttribute.Destroy;
begin
  FTailor.Free;
  if Assigned(Owner) then
    Owner.RemoveAttribute(Self);
  if FFreeMetadata then
    FMetadata.Free;
  inherited;
end;

procedure TInstantCodeAttribute.DetectMethodTypes;
var
  MT: TInstantCodeContainerMethodType;
begin
  for MT := Low(MT) to High(MT) do
    if Assigned(FindMethod(MT)) then
      MethodTypes := MethodTypes + [MT]
    else
      MethodTypes := MethodTypes - [MT];
end;

function TInstantCodeAttribute.FindCountProp: TInstantCodeProperty;
begin
  Result := HostClass.FindProperty(CountPropName);
end;

function TInstantCodeAttribute.FindField: TInstantCodeField;
begin
  Result := HostClass.FindField(FieldName);
end;

function TInstantCodeAttribute.FindMethod(
  MethodType: TInstantCodeContainerMethodType): TInstantCodeMethod;
begin
  Result := HostClass.FindMethod(GetMethodName(MethodType));
end;

function TInstantCodeAttribute.FindValueProp: TInstantCodeProperty;
begin
  Result := HostClass.FindProperty(ValuePropName);
end;

function TInstantCodeAttribute.GetAttributeClass:
    TInstantAbstractAttributeClass;
begin
  Result := Metadata.AttributeClass;
end;

function TInstantCodeAttribute.GetAttributeClassName: string;
begin
  Result := Metadata.AttributeClassName;
end;

function TInstantCodeAttribute.GetAttributeType: TInstantAttributeType;
begin
  Result := Metadata.AttributeType;
end;

function TInstantCodeAttribute.GetAttributeTypeName: string;
begin
  Result := Metadata.AttributeTypeName;
end;

function TInstantCodeAttribute.GetAttributeTypeText: string;
var
  Params: string;
begin
  with Metadata do
  begin
    if AttributeType = atUnknown then
    begin
      Result := '';
      Exit;
    end;
    Result := AttributeTypeName;
    if Assigned(AttributeClass) and
      AttributeClass.InheritsFrom(TInstantComplex) then
      Params := ObjectClassName
    else if (AttributeType in [atString, atMemo]) and (Size > 0) then
      Params := IntToStr(Size)
    else if AttributeType = atEnum then
      Params := EnumName
    else
      Exit;
    Result := Result + '(' + Params + ')';
  end
end;

function TInstantCodeAttribute.GetCountPropName: string;
begin
  Result := SingularName + 'Count';
end;

function TInstantCodeAttribute.GetExternalStorageName: string;
begin
  Result := Metadata.ExternalStorageName;
end;

function TInstantCodeAttribute.GetForeignKeyFields: string;
begin
  Result := Metadata.ForeignKeyFields;
end;

function TInstantCodeAttribute.GetFieldName: string;
begin
  Result := InstantAttributePrefix + Name;
end;

function TInstantCodeAttribute.GetHostClass: TInstantCodeClass;
begin
  if Assigned(Owner) then
    Result := Owner.Owner
  else
    Result := nil;
end;

function TInstantCodeAttribute.GetIncludeAddMethod: Boolean;
begin
  Result := mtAdd in MethodTypes;
end;

function TInstantCodeAttribute.GetIncludeClearMethod: Boolean;
begin
  Result := mtClear in MethodTypes;
end;

function TInstantCodeAttribute.GetIncludeDeleteMethod: Boolean;
begin
  Result := mtDelete in MethodTypes;
end;

function TInstantCodeAttribute.GetIncludeIndexOfMethod: Boolean;
begin
  Result := mtIndexOf in MethodTypes;
end;

function TInstantCodeAttribute.GetIncludeInsertMethod: Boolean;
begin
  Result := mtInsert in MethodTypes;
end;

function TInstantCodeAttribute.GetIncludeRemoveMethod: Boolean;
begin
  Result := mtRemove in MethodTypes;
end;

function TInstantCodeAttribute.GetIsComplex: Boolean;
begin
  Result := Assigned(Metadata.AttributeClass) and
    Metadata.AttributeClass.InheritsFrom(TInstantComplex);
end;

function TInstantCodeAttribute.GetIsContainer: Boolean;
begin
  Result := Assigned(Metadata.AttributeClass) and
    Metadata.AttributeClass.InheritsFrom(TInstantContainer);
end;

function TInstantCodeAttribute.GetIsDescription: Boolean;
begin
  Result := Metadata.IsDescription;
end;

function TInstantCodeAttribute.GetIsEnum: Boolean;
begin
  Result := Assigned(Metadata.AttributeClass) and
    Metadata.AttributeClass.InheritsFrom(TInstantEnum);
end;

function TInstantCodeAttribute.GetIsDefault: Boolean;
begin
  Result := Metadata.IsDefault;
end;

function TInstantCodeAttribute.GetStorageKind: TInstantStorageKind;
begin
  Result := Metadata.StorageKind;
end;

function TInstantCodeAttribute.GetIsIndexed: Boolean;
begin
  Result := Metadata.IsIndexed;
end;

function TInstantCodeAttribute.GetIsLocalized: Boolean;
begin
  Result := Metadata.IsLocalized;
end;

function TInstantCodeAttribute.GetIsRequired: Boolean;
begin
  Result := Metadata.IsRequired;
end;

function TInstantCodeAttribute.GetIsUnique: Boolean;
begin
  Result := Metadata.IsUnique;
end;

function TInstantCodeAttribute.GetIsPrimaryKey: Boolean;
begin
  Result := Metadata.IsPrimaryKey;
end;

function TInstantCodeAttribute.GetIndexName: string;
begin
  Result := Metadata.IndexName;
end;

function TInstantCodeAttribute.GetMetadata: TInstantAttributeMetadata;
begin
  if not Assigned(FMetadata) then
  begin
    if Assigned(Owner) then
      FMetadata := Owner.Metadata.AttributeMetadatas.Add
    else begin
      FMetadata := TInstantAttributeMetadata.Create(nil);
      FFreeMetadata := True;
    end;
    FMetadata.AttributeType := atString;
    FMetadata.Name := Name;
  end;
  Result := FMetadata;
end;

function TInstantCodeAttribute.GetMethodName(
  MethodType: TInstantCodeContainerMethodType): string;
begin
  if MethodType = mtClear then
    Result := GetMethodPrefix(MethodType) + Name
  else
    Result := GetMethodPrefix(MethodType) + SingularName;
end;

function TInstantCodeAttribute.GetMethodPrefix(
  MethodType: TInstantCodeContainerMethodType): string;
const
  Prefixes: array[TInstantCodeContainerMethodType] of string =
    ('Add', 'Clear', 'Delete', 'IndexOf', 'Insert', 'Remove');
begin
  Result := Prefixes[MethodType];
end;

function TInstantCodeAttribute.GetMethodTypes: TInstantCodeContainerMethodTypes;
begin
  Result := Tailor.MethodTypes;
end;

function TInstantCodeAttribute.GetObjectClass: TInstantCodeClass;
begin
  Result := Project.FindClass(ObjectClassName);
end;

function TInstantCodeAttribute.GetObjectClassName: string;
begin
  Result := Metadata.ObjectClassName;
end;

function TInstantCodeAttribute.GetOwner: TInstantCodeMetadataInfo;
begin
  Result := inherited Owner as TInstantCodeMetadataInfo;
end;

function TInstantCodeAttribute.GetPropTypeName: string;
begin
  Result := InstantAttributeTypeToPropertyType(AttributeType);
  if Result = '' then
    with Metadata do
      if Assigned(AttributeClass) then
      begin
        if AttributeClass.InheritsFrom(TInstantComplex) then
          Result := ObjectClassName else
        if AttributeClass.InheritsFrom(TInstantEnum) then
          Result := EnumName;
      end;
end;

function TInstantCodeAttribute.GetReadOnly: Boolean;
begin
  Result := Tailor.ReadOnly;
end;

function TInstantCodeAttribute.GetSingularName: string;
const
  Vowels = ['a', 'e', 'i', 'o', 'u'];
  SpChars = ['s', 'x', 'h'];
var
  i: integer;
begin
  if FSingularName <> '' then
    Result := FSingularName
  else begin
    Result := Name;
    //added the Vowels changes.
    i := Length(Result);
    if Copy(Result, i, 1) = 's' then
    begin
      If (i > 3) and (Copy(Result, i - 2, 3) = 'ies') and
        not (InstantCharInSet(Result[i - 3], Vowels)) then
      begin
        Result := Copy(Result, 1, i - 3) + 'y';
      end
      else If (i > 3) and (Copy(Result, i - 1, 2) = 'es') and
        (InstantCharInSet(Result[i - 2], SpChars)) then
      begin
       if (Result[i - 2] = 'h') and not (InstantCharInSet(Result[i - 3], ['c', 's'])) then
       begin
         //not ch or sh
         Result := Copy(Result, 1, i - 1);
       end
       else
        Result := Copy(Result, 1, i - 2);
      end
      else
        Result := Copy(Result, 1, i - 1);
    end
  end;
end;

function TInstantCodeAttribute.GetStorageName: string;
begin
  Result := Metadata.FieldName
end;

function TInstantCodeAttribute.GetTailor: TInstantCodeAttributeTailor;
begin
  if not Assigned(FTailor) then
    FTailor := TInstantCodeAttributeTailor.Create(Self);
  Result := FTailor;
end;

function TInstantCodeAttribute.GetUseNull: Boolean;
begin
  Result := Metadata.UseNull;
end;

function TInstantCodeAttribute.GetValueGetterCode: string;
begin
  Result := Tailor.ValueGetterCode;
end;

function TInstantCodeAttribute.GetValuePropName: string;
begin
  Result := Name;
end;

function TInstantCodeAttribute.GetValueSetterCode: string;
begin
  Result := Tailor.ValueSetterCode;
end;

procedure TInstantCodeAttribute.IncludeMethodTypes(
  AMethodTypes: TInstantCodeContainerMethodTypes; Include: Boolean);
begin
  if Include then
    MethodTypes := MethodTypes + AMethodTypes
  else
    MethodTypes := MethodTypes - AMethodTypes;
end;

procedure TInstantCodeAttribute.InternalRead(Reader: TInstantCodeReader);
var
  TypeName: string;
  TypeProcessor: TTypeProcessor;
  Token: string;
  SavePos: TInstantCodePos;
begin
  Name := Reader.ReadToken;
  SavePos := Reader.Position;
  if Reader.ReadToken <> ':' then
  begin
    Reader.Position := SavePos;
    Reader.ErrorExpected(':');
  end;
  TypeName := Reader.ReadToken;
  TypeProcessor := GetTypeProcessor(TypeName);
  if not Assigned(TypeProcessor) then
    Reader.ErrorMessage('Unknown type: ' + TypeName);
  AttributeTypeName := TypeName;
  TypeProcessor.Read(Reader, Metadata);
  while not Reader.Finished do
  begin
    if Reader.ReadEndOfStatement then
      Break;
    Token := LowerCase(Reader.ReadToken);
    if Token = MetaKeyStored then
      Metadata.StorageName := Reader.ReadStringValue
    else if Token = MetaKeyDefault then
      Metadata.DefaultValue := Reader.ReadStringValue
    else if Token = MetaKeyDescription then
      IsDescription := True
    else if Token = MetaKeyUseNull then
      Metadata.UseNull := True
    else if Token = MetaKeyIndex then
    begin
      MetaData.IndexName := trim(Reader.ReadStringValue);
      IsIndexed := True;
    end
    else if Token = MetaKeyRequired then
      IsRequired := True
    else if Token = MetaKeyLocalized then
      IsLocalized := True
    else if Token = MetaKeyUnique then
      IsUnique := True
    else if Token = MetaKeyPrimaryKey then
      IsPrimaryKey := True
    else if Token = MetaKeyMask then
      Metadata.EditMask := Reader.ReadStringValue
    else if Token = MetaKeyValid then
      Metadata.ValidCharsString := Reader.ReadStringValue
    else if Token = MetaKeyWidth then
      Metadata.DisplayWidth := Reader.ReadInteger
    else if Token = MetaKeyLabel then
      Metadata.Displaylabel := Reader.ReadStringValue
    else
      Reader.ErrorMessage('Unknown parameter: ' + Token);
  end;
end;

procedure TInstantCodeAttribute.InternalWrite(Writer: TInstantCodeWriter);

  procedure WriteStr(const Name, Value: string; const Force: Boolean = False);
  begin
    if (Value <> '') or Force then
    begin
      if Value <> '' then
        Writer.Write(' ' + Name + ' ''' + Value + '''')
      else
        Writer.Write(' ' + Name);
    end;
  end;

  procedure WriteInt(const Name: string; Value: Integer);
  begin
    if Value <> 0 then
      Writer.Write(' ' + Name + ' ' + IntToStr(Value));
  end;

begin
  Writer.WriteFmt('%s: %s', [Name, AttributeTypeText]);
  if Metadata.StorageKind = skExternal then
  begin
    if Metadata.AttributeType = atPart then
      WriteStr(MetaKeyStored, Metadata.StorageName);
    WriteStr(MetaKeyExternal, Metadata.ExternalStorageName, True);
  end
  else if Metadata.StorageKind = skVirtual then
  begin
    WriteStr(MetaKeyVirtual, Metadata.ExternalStorageName, True);
  end
  else if Metadata.StorageKind = skForeignKeys then
  begin
    WriteStr(MetaKeyForeignKey, Metadata.ForeignKeyFields, True);
  end
  else
    WriteStr(MetaKeyStored, Metadata.StorageName);
  WriteStr(MetaKeyDefault, Metadata.DefaultValue);
  WriteStr(MetaKeyMask, Metadata.EditMask);
  if Metadata.HasValidChars then
    WriteStr(MetaKeyValid, Metadata.ValidCharsString);
  WriteInt(MetaKeyWidth, Metadata.DisplayWidth);
  if Metadata.HasDisplayLabel then
    WriteStr(MetaKeyLabel, Metadata.DisplayLabel);
  if IsIndexed then
  begin
    Writer.Write(' ' + MetaKeyIndex);
    if Metadata.HasIndexName then
      Writer.Write(' ''' + Metadata.IndexName + '''');
  end;
  if IsDescription then
    Writer.Write(' ' + MetaKeyDescription);
  if IsUnique then
    Writer.Write(' ' + MetaKeyUnique);
  if IsPrimaryKey then
    Writer.Write(' ' + MetaKeyPrimaryKey);
  if IsRequired then
    Writer.Write(' ' + MetaKeyRequired);
  if IsLocalized then
    Writer.Write(' ' + MetaKeyLocalized);
  if Metadata.UseNull then
    Writer.Write(' ' + MetaKeyUseNull);
  if IsDefault then
    Writer.Write(' ' + MetaKeyDefault);
  Writer.Write(';');
end;

procedure TInstantCodeAttribute.Realize;
begin
  Tailor.Apply;
end;

procedure TInstantCodeAttribute.SetAttributeClass(Value:
    TInstantAbstractAttributeClass);
begin
  Metadata.AttributeClass := Value;
end;

procedure TInstantCodeAttribute.SetAttributeClassName(const Value: string);
begin
  Metadata.AttributeClassName := Value;
end;

procedure TInstantCodeAttribute.SetAttributeType(
  Value: TInstantAttributeType);
var
  WasContainer: Boolean;
begin
  WasContainer := IsContainer;
  Metadata.AttributeType := Value;
  Tailor.IsArray := IsContainer;
  if (IsContainer <> WasContainer) and (Visibility > viProtected) then
  begin
    if IsContainer then
      Visibility := viPublic
    else
      Visibility := viPublished;
  end;
end;

procedure TInstantCodeAttribute.SetAttributeTypeName(const Value: string);
begin
  Metadata.AttributeTypeName := Value;
  AttributeType := AttributeType;
end;

procedure TInstantCodeAttribute.SetExternalStorageName(const Value: string);
begin
  Metadata.ExternalStorageName := Value;
end;

procedure TInstantCodeAttribute.SetForeignKeyFields(const Value: string);
begin
  Metadata.ForeignKeyFields := Value;
end;

procedure TInstantCodeAttribute.SetIncludeAddMethod(const Value: Boolean);
begin
  IncludeMethodTypes([mtAdd], Value);
end;

procedure TInstantCodeAttribute.SetIncludeClearMethod(
  const Value: Boolean);
begin
  IncludeMethodTypes([mtClear], Value);
end;

procedure TInstantCodeAttribute.SetIncludeDeleteMethod(
  const Value: Boolean);
begin
  IncludeMethodTypes([mtDelete], Value);
end;

procedure TInstantCodeAttribute.SetIncludeIndexOfMethod(
  const Value: Boolean);
begin
  IncludeMethodTypes([mtIndexOf], Value);
end;

procedure TInstantCodeAttribute.SetIncludeInsertMethod(
  const Value: Boolean);
begin
  IncludeMethodTypes([mtInsert], Value);
end;

procedure TInstantCodeAttribute.SetIncludeRemoveMethod(
  const Value: Boolean);
begin
  IncludeMethodTypes([mtRemove], Value);
end;

procedure TInstantCodeAttribute.SetIsDefault(const Value: Boolean);
begin
  Metadata.IsDefault := Value;
end;

procedure TInstantCodeAttribute.SetStorageKind(const Value: TInstantStorageKind);
begin
  if Metadata.AttributeType = atReference then
  begin
    if not (Value in [skForeignKeys, skEmbedded]) then
      raise Exception.Create('Error assigning StorageKind for Metadata of Reference: only "skForeignKeys" or "skEmbedded" are accepted.');
  end;
  Metadata.StorageKind := Value;
end;

procedure TInstantCodeAttribute.SetIsDescription(const Value: Boolean);
begin
  Metadata.IsDescription := Value;
end;

procedure TInstantCodeAttribute.SetIsIndexed(const Value: Boolean);
begin
  Metadata.IsIndexed := Value;
end;

procedure TInstantCodeAttribute.SetIsLocalized(const Value: Boolean);
begin
  Metadata.IsLocalized := Value;
end;

procedure TInstantCodeAttribute.SetIsRequired(const Value: Boolean);
begin
  Metadata.IsRequired := Value;
end;

procedure TInstantCodeAttribute.SetIsUnique(const Value: Boolean);
begin
  Metadata.IsUnique := Value;
end;

procedure TInstantCodeAttribute.SetIsPrimaryKey(const Value: Boolean);
begin
  Metadata.IsPrimaryKey := Value;
end;

procedure TInstantCodeAttribute.SetIndexName(const Value: string);
begin
  Metadata.IndexName := Value;
end;

procedure TInstantCodeAttribute.SetMethodTypes(
  const Value: TInstantCodeContainerMethodTypes);
begin
  Tailor.MethodTypes := Value;
end;

procedure TInstantCodeAttribute.SetName(const Value: string);
begin
  if Value <> Name then
  begin
    inherited;
    Metadata.Name := Name;
    Update;
  end;
end;

procedure TInstantCodeAttribute.SetObjectClassName(const Value: string);
begin
  Metadata.ObjectClassName := Value;
end;

procedure TInstantCodeAttribute.SetPropTypeName(const Value: string);
var
  AType: TInstantAttributeType;
begin
  AType := InstantPropertyTypeToAttributeType(Value);
  if AType <> atUnknown then
    AttributeType := AType;
end;

procedure TInstantCodeAttribute.SetReadOnly(Value: Boolean);
begin
  Tailor.ReadOnly := Value;
end;

procedure TInstantCodeAttribute.SetSingularName(const Value: string);
begin
  if Value <> SingularName then
    FSingularName := Value;
end;

procedure TInstantCodeAttribute.SetStorageName(const Value: string);
begin
  Metadata.FieldName := Value;
end;

procedure TInstantCodeAttribute.SetUseNull(const Value: Boolean);
begin
  Metadata.UseNull := Value;
end;

procedure TInstantCodeAttribute.SetVisibility(
  Value: TInstantCodeVisibility);
begin
  FVisibility := Value;
  Update;
end;

procedure TInstantCodeAttribute.Update;
begin
  Tailor.Update;
end;

function TInstantCodeAttribute.GetCanHaveStorageName: boolean;
begin
  Result := (not (StorageKind in [skExternal, skVirtual, skForeignKeys])) or (AttributeType = atPart);
end;

function TInstantCodeAttribute.GetCanHaveExternalStorageName: boolean;
begin
  Result := (StorageKind in [skExternal]);
end;

function TInstantCodeAttribute.GetCanHaveForeignKeyFields: boolean;
begin
  Result := (StorageKind = skForeignKeys) and (Metadata.AttributeType = atReference);
end;

function TInstantCodeAttribute.GetCanBeExternal: boolean;
begin
  Result := AttributeType in [atPart, atParts, atReferences];
end;

{ TInstantCodeClassLink }

function TInstantCodeClassLink.FindInstance: TInstantCodeObject;
begin
  if Assigned(Module) then
    Result := Module.FindClass(Name)
  else
    Result := nil;
end;

function TInstantCodeClassLink.GetInstance: TInstantCodeClass;
begin
  Result := inherited Instance as TInstantCodeClass;
end;

procedure TInstantCodeClassLink.SetInstance(
  const Value: TInstantCodeClass);
begin
  inherited Instance := Value;
end;

{ TInstantCodeBaseClassLink }

procedure TInstantCodeBaseClassLink.AssignInstance(Value: TInstantCodeObject);
begin
  if Value is TInstantCodeClass then
    if TInstantCodeClass(Value).DerivesFrom(Owner) then
      Exit;
  inherited;
end;

procedure TInstantCodeBaseClassLink.Attach;
begin
  Owner.InsertInBase;
end;

constructor TInstantCodeBaseClassLink.Create(AOwner: TInstantCodeClass);
begin
  inherited Create(AOwner);
end;

procedure TInstantCodeBaseClassLink.Detach;
begin
  Owner.RemoveFromBase;
end;

function TInstantCodeBaseClassLink.GetOwner: TInstantCodeClass;
begin
  Result := inherited Owner as TInstantCodeClass;
end;

{ TInstantCodeClassChangeInfo }

constructor TInstantCodeClassChangeInfo.Create(
  AInstance: TInstantCodeClass; AChangeType: TInstantCodeChangeType;
  const AOldName: string; AChangedAttributes: TStringList;
  ANewAttributes: TList);
begin
  FInstance := AInstance;
  FChangeType := AChangeType;
  FOldName := AOldName;
  FChangedAttributes := AChangedAttributes;
  FNewAttributes := ANewAttributes;
end;

function TInstantCodeClassChangeInfo.IsChangedAttribute(
  Attribute: TInstantCodeAttribute; out OldName: string): Boolean;
var
  Index: Integer;
begin
  Index := FChangedAttributes.IndexOfObject(Attribute);
  Result := Index <> -1;
  if Result then
    OldName := FChangedAttributes[Index];
end;

function TInstantCodeClassChangeInfo.IsDeletedAttribute(
  Attribute: TInstantCodeAttribute): Boolean;
var
  Existing: TInstantCodeAttribute;
begin
  Existing := FInstance.FindAttribute(Attribute.Name);
  if Assigned(Existing) then
    Result := IsNewAttribute(Existing)
  else
    Result := FChangedAttributes.IndexOf(Attribute.Name) = -1;
end;

function TInstantCodeClassChangeInfo.IsNewAttribute(
  Attribute: TInstantCodeAttribute): Boolean;
begin
  Result := FNewAttributes.IndexOf(Attribute) <> -1;
end;

{ TInstantCodeMetadataInfo }

function TInstantCodeMetadataInfo.AddAttribute: TInstantCodeAttribute;
begin
  Result := TInstantCodeAttribute.Create(Self);
end;

procedure TInstantCodeMetadataInfo.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeMetadataInfo then
    with TInstantCodeMetadataInfo(Source) do
    begin
      Self.Persistence := Persistence;
      Self.StorageName := StorageName;
      Self.AssignAttributes(FAttributes);
    end;
end;

procedure TInstantCodeMetadataInfo.AssignAttributes(List: TList);
var
  I: Integer;
  Attribute: TInstantCodeAttribute;
begin
  while FAttributes.Count > 0 do
    TInstantCodeAttribute(FAttributes[0]).Free;
  for I := 0 to Pred(List.Count) do
  begin
    Attribute := TInstantCodeAttribute.Create(Self);
    Attribute.Assign(TInstantCodeAttribute(List[I]));
  end;
end;

procedure TInstantCodeMetadataInfo.AttachTo(
  MetadataInfo: TInstantCodeMetadataInfo);
begin
  Metadata.Parent := MetadataInfo.Metadata;
end;

procedure TInstantCodeMetadataInfo.CloneAttributes(List: TList);
var
  I: Integer;
  Attribute: TInstantCodeAttribute;
begin
  List.Clear;
  for I := 0 to Pred(AttributeCount) do
  begin
    Attribute := TInstantCodeAttribute.Create(nil);
    Attribute.Assign(Attributes[I]);
    List.Add(Attribute);
  end;
end;

constructor TInstantCodeMetadataInfo.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FAttributes := TList.Create;
end;

destructor TInstantCodeMetadataInfo.Destroy;
begin
  FAttributes.Free;
  FMetadata.Free;
  inherited;
end;

function TInstantCodeMetadataInfo.FindAttribute(
  const Name: string): TInstantCodeAttribute;
var
  I: Integer;
begin
  for I := 0 to Pred(AttributeCount) do
  begin
    Result := Attributes[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeMetadataInfo.FindNearestAttributes(
  Instance: TInstantCodeObject; out Prior, Next: TObject): Boolean;
begin
  Result := FindNearest(FAttributes, Instance, Prior, Next);
end;

function TInstantCodeMetadataInfo.GetAttributeCount: Integer;
begin
  Result := FAttributes.Count;
end;

function TInstantCodeMetadataInfo.GetAttributes(
  Index: Integer): TInstantCodeAttribute;
begin
  Result := TInstantCodeAttribute(FAttributes[Index]);
end;

function TInstantCodeMetadataInfo.GetClassStatement: string;
begin
  if Persistence = peStored then
  begin
    Result := MetaKeyStored;
    if Metadata.StorageName <> '' then
      Result := Result + ' ''' + StorageName + '''';
    Result := Result + ';';
  end else if AttributeCount = 0 then
    Result := MetaKeyEmbedded + ';'
  else
    Result := '';
end;

function TInstantCodeMetadataInfo.GetIsEmpty: Boolean;
begin
  Result := False;
end;

function TInstantCodeMetadataInfo.GetIsStored: Boolean;
begin
  Result := Metadata.IsStored;
end;

function TInstantCodeMetadataInfo.GetMetadata: TInstantClassMetadata;
begin
  if not Assigned(FMetadata) then
  begin
    if Assigned(Project) then
      FMetadata := Project.CreateClassMetadata
    else
      FMetadata := TInstantClassMetadata.Create(nil);
    if Assigned(Owner) then
      FMetadata.ParentName := Owner.BaseClassName;
    FMetadata.Name := Name;
  end;
  Result := FMetadata;
end;

function TInstantCodeMetadataInfo.GetName: string;
begin
  Result := Metadata.Name;
end;

function TInstantCodeMetadataInfo.GetOwner: TInstantCodeClass;
begin
  Result := inherited Owner as TInstantCodeClass;
end;

function TInstantCodeMetadataInfo.GetPersistence: TInstantPersistence;
begin
  Result := Metadata.Persistence;
end;

function TInstantCodeMetadataInfo.GetStorageName: string;
begin
  if IsStored then
    Result := Metadata.TableName
  else
    Result := '';
end;

procedure TInstantCodeMetadataInfo.InsertAttribute(
  Attribute: TInstantCodeAttribute);
begin
  FAttributes.Add(Attribute);
end;

class function TInstantCodeMetadataInfo.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;

  function FindMetaDataTag: Boolean;
  var
    SavePos: TInstantCodePos;
  begin
    Result := False;
    if Reader.NextChar = '{' then begin
      SavePos := Reader.Position;
      try
        Reader.EnterComment;
        Result := Reader.ReadMatching(MetadataInfoID);
      finally
        Reader.Position := SavePos;
      end;
    end;
  end;

begin
  Result := FindMetaDataTag;
end;

procedure TInstantCodeMetadataInfo.InternalRead(Reader: TInstantCodeReader);

  procedure ReadMetaDataTagSig;
  begin
    Reader.IgnoreComments := False;
    try
      if not Reader.ReadMatching('{' + MetadataInfoID + ' ') then
        Reader.ErrorExpected('''{' + MetadataInfoID + ' '' tag in ' + Name,
                False);
    finally
      Reader.IgnoreComments := True;
    end;
  end;

var
  Token: string;
  SavePos: TInstantCodePos;
  Attribute: TInstantCodeAttribute;
  SaveErrorSeverity: TInstantCodeErrorSeverity;
begin
  ReadMetaDataTagSig;

  Persistence := peEmbedded;
  SaveErrorSeverity := Reader.ErrorSeverity;
  Reader.ErrorSeverity := esFatal;
  try
    while not Reader.Finished do
    begin
      Reader.SkipSpace;
      SavePos := Reader.Position;
      Token := Reader.ReadToken;
      if SameText(Token, MetaKeyEmbedded) then
      begin
        Persistence := peEmbedded;
        Reader.ReadEndOfStatement(True);
      end else if SameText(Token, MetaKeyStored) then
      begin
        Persistence := peStored;
        StorageName := Reader.ReadStringValue;
        Reader.ReadEndOfStatement(True);
      end else if Token = '}' then
        Break
      else begin
        Reader.Position := SavePos;
        Attribute := AddAttribute;
        try
          Attribute.Read(Reader);
        except
          Attribute.Free;
          raise;
        end;
      end;
    end;
  finally
    Reader.ErrorSeverity := SaveErrorSeverity;
  end;
end;

procedure TInstantCodeMetadataInfo.InternalWrite(Writer: TInstantCodeWriter);
var
  S: string;
  Start, I: Integer;
begin
  Writer.Write('{' + MetadataInfoID + ' ');
  try
    S := ClassStatement;
    if S = '' then
    begin
      Attributes[0].Write(Writer);
      Start := 1;
    end else
    begin
      Writer.Write(S);
      Start := 0;
    end;
    if Start < AttributeCount then
    begin
      Writer.WriteLn;
      Writer.Indent;
      try
        for I := Start to Pred(AttributeCount) do
        begin
          if I > Start then
            Writer.WriteLn;
          Attributes[I].Write(Writer);
        end;
      finally
        Writer.Unindent;
      end;
    end;
  finally
    Writer.Write(' }');
  end;
end;

procedure TInstantCodeMetadataInfo.Organize;
begin
  inherited;
  //Sort;
end;

procedure TInstantCodeMetadataInfo.RemoveAttribute(
  Attribute: TInstantCodeAttribute);
begin
  FAttributes.Remove(Attribute);
end;

procedure TInstantCodeMetadataInfo.SetName(const Value: string);
begin
  Metadata.Name := Value;
end;

procedure TInstantCodeMetadataInfo.SetPersistence(Value: TInstantPersistence);
begin
  Metadata.Persistence := Value;
end;

procedure TInstantCodeMetadataInfo.SetStorageName(const Value: string);
begin
  Metadata.TableName := Value;
end;

procedure TInstantCodeMetadataInfo.Sort;
begin
  inherited;
  SortByName(FAttributes);
end;

{ TInstantCodeDivision }

function TInstantCodeDivision.AddField: TInstantCodeField;
begin
  Result := TInstantCodeField(AddMember(TInstantCodeField));
end;

function TInstantCodeDivision.AddMember(
  AClass: TInstantCodeMemberClass): TInstantCodeMember;
begin
  Result := AClass.Create(Self);
  Result.Visibility := Visibility;
end;

function TInstantCodeDivision.AddMethod: TInstantCodeMethod;
begin
  Result := TInstantCodeMethod(AddMember(TInstantCodeMethod));
end;

function TInstantCodeDivision.AddProperty: TInstantCodeProperty;
begin
  Result := TInstantCodeProperty(AddMember(TInstantCodeProperty));
end;

constructor TInstantCodeDivision.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  if Assigned(Owner) then
    Owner.InsertDivision(Self);
end;

destructor TInstantCodeDivision.Destroy;
begin
  if Assigned(Owner) then
    Owner.RemoveDivision(Self);
  inherited;
end;

function TInstantCodeDivision.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TInstantCodeDivision.GetName: string;
begin
  Result := VisibilityToName(Visibility);
end;

class function TInstantCodeDivision.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
var
  SavePos: TInstantCodePos;
  V: TInstantCodeVisibility;
begin
  SavePos := Reader.Position;
  Result := NameToVisibility(Reader.ReadToken, V);
  if not Result then
  begin
    Reader.Position := SavePos;
    Result := inherited InternalAtInstance(Reader, Name);
  end;
end;

procedure TInstantCodeDivision.InternalRead(Reader: TInstantCodeReader);
var
  SavePos: TInstantCodePos;
  I: Integer;
begin
  with Reader do
  begin
    SavePos := Position;
    if not NameToVisibility(ReadToken, FVisibility) then
    begin
      FVisibility := viDefault;
      Position := SavePos;
    end;
  end;
  inherited;
  for I := 0 to Pred(Count) do
    Items[I].Visibility := Visibility;
end;

procedure TInstantCodeDivision.InternalWrite(Writer: TInstantCodeWriter);
begin
  if Visibility <> viDefault then
    Writer.WriteLn(VisibilityToName(Visibility));
  Writer.Indent;
  try
    inherited;
  finally
    Writer.Unindent;
  end;
end;

class function TInstantCodeDivision.NameToVisibility(const Name: string;
  var Visibility: TInstantCodeVisibility): Boolean;
begin
  Visibility := viPrivate;
  while Visibility <= High(Visibility) do
  begin
    Result := SameText(Name, VisibilityToName(Visibility));
    if Result then
      Exit;
    Inc(Visibility);
  end;
  Result := False;
end;

class function TInstantCodeDivision.VisibilityToName(
  Visibility: TInstantCodeVisibility): string;
begin
  Result := LowerCase(GetEnumName(TypeInfo(TInstantCodeVisibility),
    Ord(Visibility)));
  System.Delete(Result, 1, 2);
end;

{ TInstantCodeClassForward }

function TInstantCodeClassForward.GetIsLinkable: Boolean;
begin
  Result := False;
end;

class function TInstantCodeClassForward.Identifier: string;
begin
  Result := 'class';
end;

class function TInstantCodeClassForward.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
begin
  Result := inherited InternalAtInstance(Reader, Name) and
    (Reader.ReadToken = ';');
end;

procedure TInstantCodeClassForward.InternalRead(
  Reader: TInstantCodeReader);
begin
  inherited;
  Reader.ReadStatement;
end;

procedure TInstantCodeClassForward.InternalWrite(
  Writer: TInstantCodeWriter);
begin
  inherited;
  Writer.Write('class;');
end;

{ TInstantCodeClass }

function TInstantCodeClass.AddAttribute: TInstantCodeAttribute;
begin
  Result := MetadataInfo.AddAttribute;
end;

function TInstantCodeClass.AddDivision(
  Visibility: TInstantCodeVisibility): TInstantCodeDivision;
begin
  Result := TInstantCodeDivision.Create(Self);
  Result.Visibility := Visibility;
end;

function TInstantCodeClass.AddField(
  Visibility: TInstantCodeVisibility): TInstantCodeField;
begin
  Result := TInstantCodeField(AddMember(TInstantCodeField, Visibility));
end;

function TInstantCodeClass.AddMember(AClass: TInstantCodeMemberClass;
  Visibility: TInstantCodeVisibility): TInstantCodeMember;
var
  Division: TInstantCodeDivision;
begin
  Division := FindDivision(Visibility);
  if not Assigned(Division) then
    Division := AddDivision(Visibility);
  Result := Division.AddMember(AClass);
end;

function TInstantCodeClass.AddMethod(
  Visibility: TInstantCodeVisibility): TInstantCodeMethod;
begin
  Result := TInstantCodeMethod(AddMember(TInstantCodeMethod, Visibility));
end;

function TInstantCodeClass.AddProperty(
  Visibility: TInstantCodeVisibility): TInstantCodeProperty;
begin
  Result := TInstantCodeProperty(AddMember(TInstantCodeProperty, Visibility));
end;

procedure TInstantCodeClass.AddUses(AUnitNames: array of string;
  Scope: TInstantCodeScope; var Source: string;
  ChangeInfo: TInstantCodeClassChangeInfo);
var
  Modifier: TInstantCodeModifier;
begin
  if Length(AUnitNames) = 0 then
    Exit;

  if Assigned(ChangeInfo.Modifier) then
    Modifier := ChangeInfo.Modifier
  else
    Modifier := TInstantCodeModifier.Create(Source, Self.Project);
  try
    if Scope = scImplementation then
      Modifier.AdjustForUnitNamesInInterface(AUnitNames);
    Modifier.AddUses(AUnitNames, Scope);
    if Scope = scInterface then
      Modifier.RemoveUses(AUnitNames, scImplementation);
  finally
    if not Assigned(ChangeInfo.Modifier) then
      Modifier.Free;
  end;
end;

procedure TInstantCodeClass.ApplyToSource(var Source: string;
  ChangeInfo: TInstantCodeClassChangeInfo);
var
  Modifier: TInstantCodeModifier;
begin
  Organize;
  if Assigned(ChangeInfo.Modifier) then
    Modifier := ChangeInfo.Modifier
  else
    Modifier := TInstantCodeModifier.Create(Source, Self.Project);
  try
    with Modifier do
      case ChangeInfo.ChangeType of
        ctNew:
          AddClass(Self);
        ctEdit:
          ChangeClass(ChangeInfo, Self);
        ctDelete:
          DeleteClass(Self);
      end;
  finally
    if not Assigned(ChangeInfo.Modifier) then
      Modifier.Free;
  end;
  AddUses(['InstantPersistence', 'InstantTypes'],
      scInterface, Source, ChangeInfo);
  AddUses(['InstantMetadata'], scImplementation, Source, ChangeInfo);
end;

procedure TInstantCodeClass.AssignAttributes(List: TList);
begin
  MetadataInfo.AssignAttributes(List);
end;

procedure TInstantCodeClass.CloneAttributes(List: TList);
begin
  MetadataInfo.CloneAttributes(List);
end;

constructor TInstantCodeClass.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FBaseClassLink := TInstantCodeBaseClassLink.Create(Self);
  FMetadataInfo := TInstantCodeMetadataInfo.Create(Self);
  FDivisions := TList.Create;
  FMembers := TList.Create;
  FFields := TList.Create;
  FMethods := TList.Create;
  FProperties := TList.Create;
end;

function TInstantCodeClass.DerivesFrom(AClass: TInstantCodeClass): Boolean;
begin
  if AClass = Self then
    Result := True
  else if Assigned(BaseClass) then
    Result := (BaseClass = AClass) or BaseClass.DerivesFrom(AClass)
  else
    Result := False;
end;

function TInstantCodeClass.DerivesFrom(AClassName: string): Boolean;
begin
  if SameText(Name, AClassName) or SameText(BaseClassName, AClassName) then
    Result := True
  else if Assigned(BaseClass) then
    Result := BaseClass.DerivesFrom(AClassName)
  else
    Result := False;
end;

destructor TInstantCodeClass.Destroy;
var
  I: Integer;
begin
  if SubClassCount > 0 then
    for I := Pred(SubClassCount) to 0 do
        SubClasses[I].BaseClass := BaseClass;
  RemoveFromBase;
  while SubClassCount > 0 do
    SubClasses[0].BaseClass := nil;
  inherited;
  FSubClassList.Free;
  FProperties.Free;
  FMethods.Free;
  FFields.Free;
  FMembers.Free;
  FDivisions.Free;
  FreeAndNil(FBaseClassLink);
end;

function TInstantCodeClass.FindAttribute(
  const Name: string): TInstantCodeAttribute;
begin
  Result := MetadataInfo.FindAttribute(Name);
end;

function TInstantCodeClass.FindDivision(
  Visibility: TInstantCodeVisibility): TInstantCodeDivision;
var
  I: Integer;
begin
  for I := 0 to Pred(DivisionCount) do
  begin
    Result := Divisions[I];
    if Result.Visibility = Visibility then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeClass.FindField(const Name: string): TInstantCodeField;
var
  I: Integer;
begin
  for I := 0 to Pred(FieldCount) do
  begin
    Result := Fields[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeClass.FindMember(AClass: TInstantCodeMemberClass;
  const Name: string): TInstantCodeMember;
var
  I: Integer;
begin
  for I := 0 to Pred(MemberCount) do
  begin
    Result := Members[I];
    if (Result.ClassType = AClass) and SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeClass.FindMethod(const Name: string): TInstantCodeMethod;
var
  I: Integer;
begin
  for I := 0 to Pred(MethodCount) do
  begin
    Result := Methods[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function CompareDivisions(Item1, Item2: Pointer): Integer; overload;
var
  Division1, Division2: TInstantCodeDivision;
begin
  Division1 := Item1;
  Division2 := Item2;
  if Division1.Visibility < Division2.Visibility then
    Result := -1
  else if Division1.Visibility > Division2.Visibility then
    Result := 1
  else
    Result := 0;
end;

function CompareDivisionsList(List: TStringList;
  Index1, Index2: Integer): Integer;
begin
  Result := CompareDivisions(List.Objects[Index1], List.Objects[Index2]);
end;

function TInstantCodeClass.FindNearestDivisions(
  Division: TInstantCodeDivision; out Prior, Next: TObject): Boolean;
begin
  Result := FindNearest(FDivisions, Division, Prior, Next, nil, nil,
    CompareDivisionsList);
end;

function TInstantCodeClass.FindNearestFields(Field: TInstantCodeField;
  out Prior, Next: TObject): Boolean;
begin
  Result := FindNearestMembers(FFields, Field, [Field.Visibility], Prior, Next);
end;

function TInstantCodeClass.FindNearestMembers(List: TList;
  Instance: TInstantCodeObject; Visibilities: TInstantCodeVisibilities;
  out Prior, Next: TObject): Boolean;
begin
  if not Assigned(List) then
    List := FMembers;
  Result := FindNearest(List, Instance, Prior, Next, VisibilityFilter,
    @Visibilities, CompareMembersList);
end;

function TInstantCodeClass.FindNearestMethods(Method: TInstantCodeMethod;
  out Prior, Next: TObject): Boolean;
begin
  Result := FindNearestMembers(FMethods, Method, [Method.Visibility], Prior, Next);
end;

function TInstantCodeClass.FindNearestProperties(Prop: TInstantCodeProperty;
  out Prior, Next: TObject): Boolean;
begin
  Result := FindNearestMembers(FProperties, Prop, [Prop.Visibility], Prior, Next);
end;

function TInstantCodeClass.FindProperty(
  const Name: string): TInstantCodeProperty;
var
  I: Integer;
begin
  for I := 0 to Pred(PropertyCount) do
  begin
    Result := Properties[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

procedure TInstantCodeClass.Fixup;
begin
  inherited;
  FBaseClassLink.Resolve;
end;

function TInstantCodeClass.GetAttributeCount: Integer;
begin
  Result := MetadataInfo.AttributeCount;
end;

function TInstantCodeClass.GetAttributes(
  Index: Integer): TInstantCodeAttribute;
begin
  Result := MetadataInfo.Attributes[Index];
end;

function TInstantCodeClass.GetBaseClass: TInstantCodeClass;
begin
  if Assigned(FBaseClassLink) then
    Result := FBaseClassLink.Instance
  else
    Result := nil;  
end;

function TInstantCodeClass.GetBaseClassName: string;
begin
  Result := FBaseClassLink.Name;
end;

function TInstantCodeClass.GetDivisionCount: Integer;
begin
  Result := FDivisions.Count;
end;

function TInstantCodeClass.GetDivisions(Index: Integer): TInstantCodeDivision;
begin
  Result := FDivisions[Index];
end;

function TInstantCodeClass.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TInstantCodeClass.GetFields(Index: Integer): TInstantCodeField;
begin
  Result := FFields[Index];
end;

function TInstantCodeClass.GetImplementationComment: string;
begin
  Result := Format('{ %s }', [Name]);
end;

function TInstantCodeClass.GetImplementationString: string;
var
  Stream: TInstantStringStream;
  Writer: TInstantCodeWriter;
  Proc: TInstantCodeProc;
  I: Integer;
begin
  Stream := TInstantStringStream.Create('');
  Writer := TInstantCodeWriter.Create(Stream);
  try
    for I := 0 to Pred(MethodCount) do
    begin
      Proc := Methods[I].Proc;
      if Assigned(Proc) then
      begin
        Writer.WriteLn;
        Proc.Write(Writer);
        Writer.WriteLn;
      end;
    end;
    Result := Stream.DataString;
  finally
    Writer.Free;
    Stream.Free;
  end;
end;

function TInstantCodeClass.GetIsStored: Boolean;
begin
  Result := MetadataInfo.IsStored;
end;

function TInstantCodeClass.GetMemberCount: Integer;
begin
  Result := FMembers.Count;
end;

function TInstantCodeClass.GetMembers(Index: Integer): TInstantCodeMember;
begin
  Result := FMembers[Index];
end;

function TInstantCodeClass.GetMetadata: TInstantClassMetadata;
begin
  Result := MetadataInfo.Metadata;
end;

function TInstantCodeClass.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

function TInstantCodeClass.GetMethods(Index: Integer): TInstantCodeMethod;
begin
  Result := FMethods[Index];
end;

function TInstantCodeClass.GetPersistence: TInstantPersistence;
begin
  Result := MetadataInfo.Persistence;
end;

function TInstantCodeClass.GetProperties(Index: Integer): TInstantCodeProperty;
begin
  Result := TInstantCodeProperty(FProperties[Index]);
end;

function TInstantCodeClass.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

function TInstantCodeClass.GetStorageName: string;
begin
  Result := MetadataInfo.StorageName;
end;

function TInstantCodeClass.GetSubClass(Index: Integer): TInstantCodeClass;
begin
  Result := SubClassList[Index];
end;

function TInstantCodeClass.GetSubClassCount: Integer;
begin
  Result := SubClassList.Count;
end;

function TInstantCodeClass.GetSubClassList: TList;
begin
  if not Assigned(FSubClassList) then
    FSubClassList := TList.Create;
  Result := FSubClassList;
end;

function TInstantCodeClass.GetPascalUnitName: string;
begin
  Result := Module.PascalUnitName;
end;

class function TInstantCodeClass.Identifier: string;
begin
  Result := 'class';
end;

procedure TInstantCodeClass.InsertDivision(Division: TInstantCodeDivision);
begin
  FDivisions.Add(Division);
end;

procedure TInstantCodeClass.InsertInBase;
begin
  if Assigned(BaseClass) then
    BaseClass.InsertSubClass(Self);
end;

procedure TInstantCodeClass.InsertMember(Member: TInstantCodeMember);
begin
  FMembers.Add(Member);
  if Member is TInstantCodeField then
    FFields.Add(Member)
  else if Member is TInstantCodeMethod then
    FMethods.Add(Member)
  else if Member is TInstantCodeProperty then
    FProperties.Add(Member);
end;

procedure TInstantCodeClass.InsertSubClass(AClass: TInstantCodeClass);
begin
  with SubClassList do
    if IndexOf(AClass) = -1 then
      Add(AClass);
  AClass.MetadataInfo.AttachTo(MetadataInfo);
end;

class function TInstantCodeClass.InternalAtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
var
  Token: string;
begin
  Result := inherited InternalAtInstance(Reader, Name);
  if not Result then
    Exit;
  Reader.SkipSpace;
  Token := Reader.ReadToken;
  Result := (Token <> ';') { Forward declaration }
    and not SameText(Token, 'of') { Class reference }
end;

procedure TInstantCodeClass.InternalRead(Reader: TInstantCodeReader);
var
  SavePos: TInstantCodePos;
begin
  inherited;
  Reader.ReadTokenMatching('class');
  Reader.SkipBlanks;
  if Reader.NextChar = '(' then
  begin
    Reader.ReadChar;
    BaseClassName := Reader.ReadToken;
    Reader.ReadNext(')');
    Reader.SkipBlanks;
  end;
  if MetadataInfo.AtInstance(Reader) then
  begin
    MetadataInfo.Read(Reader);
    Reader.SkipSpace;
  end;
  if Reader.NextChar = ';' then
  begin
    Reader.ReadChar;
    Exit;
  end;
  while not Reader.Finished do
  begin
    Reader.SkipSpace;
    SavePos := Reader.Position;
    if SameText(Reader.ReadToken, 'end') then
    begin
      Reader.ReadStatement;
      Break;
    end;
    Reader.Position := SavePos;
    if TInstantCodeDivision.AtInstance(Reader) then
      with AddDivision(viDefault) do
        Read(Reader)
    else
      Break;
  end;
end;

procedure TInstantCodeClass.InternalWrite(Writer: TInstantCodeWriter);
var
  I: Integer;
  Division: TInstantCodeDivision;
begin
  inherited;
  Writer.Write('class');
  if BaseClassName <> '' then
    Writer.WriteLnFmt('(%s)', [BaseClassName]) else
    Writer.WriteLn;
  MetadataInfo.Write(Writer);
  if not MetadataInfo.IsEmpty then
    Writer.WriteLn;
  for I := 0 to Pred(DivisionCount) do
  begin
    Division := Divisions[I];
    Division.Write(Writer);
    if not Division.IsEmpty then
      Writer.WriteLn;
  end;
  Writer.Write('end;');
end;

procedure TInstantCodeClass.Organize;
begin
  inherited;
  Sort;
end;

procedure TInstantCodeClass.RemoveDivision(Division: TInstantCodeDivision);
begin
  FDivisions.Remove(Division);
end;

procedure TInstantCodeClass.RemoveFromBase;
begin
  if Assigned(BaseClass) then
    BaseClass.RemoveSubClass(Self);
end;

procedure TInstantCodeClass.RemoveMember(Member: TInstantCodeMember);
begin
  if Member is TInstantCodeField then
    FFields.Remove(Member)
  else if Member is TInstantCodeMethod then
    FMethods.Remove(Member)
  else if Member is TInstantCodeProperty then
    FProperties.Remove(Member);
  FMembers.Remove(Member);
end;

procedure TInstantCodeClass.RemoveSubClass(AClass: TInstantCodeClass);
begin
  SubClassList.Remove(AClass);
end;

procedure TInstantCodeClass.SetBaseClass(const Value: TInstantCodeClass);
begin
  FBaseClassLink.Instance := Value;
end;

procedure TInstantCodeClass.SetBaseClassName(const Value: string);
begin
  FBaseClassLink.Name := Value;
end;

procedure TInstantCodeClass.SetName(const Value: string);
var
  I: Integer;
begin
  inherited;
  MetadataInfo.Name := Name;
  for I := 0 to Pred(MethodCount) do
    Methods[I].UpdateProcName;
end;

procedure TInstantCodeClass.SetPersistence(const Value: TInstantPersistence);
begin
  MetadataInfo.Persistence := Value;
end;

procedure TInstantCodeClass.SetStorageName(const Value: string);
begin
  MetadataInfo.StorageName := Value;
end;

procedure TInstantCodeClass.SetSubClass(Index: Integer; const Value:
    TInstantCodeClass);
begin
  SubClassList[Index] := Value;
end;

procedure TInstantCodeClass.SetPascalUnitName(const Value: string);
var
  NewModule: TInstantCodeModule;
begin
  NewModule := Project.FindModule(Value);
  if Assigned(NewModule) and (NewModule <> Module) then
  begin
    Owner.ExtractObject(Self);
    NewModule.InterfaceSection.AddClass(Self);
  end;
end;

procedure TInstantCodeClass.Sort;
var
  I: Integer;
begin
  FDivisions.Sort(CompareDivisions);
  for I := 0 to Pred(DivisionCount) do
    Divisions[I].Sort;
  SortByName(FMembers);
  SortByName(FFields);
  SortByName(FMethods);
  SortByName(FProperties);
end;

procedure TInstantCodeClass.VisibilityFilter(Sender: TInstantCodeObject;
  var Include: Boolean; Arg: Pointer);
var
  Visibilities: TInstantCodeVisibilities;
begin
  Visibilities := TInstantCodeVisibilities(Arg^);
  Include := (Sender is TInstantCodeMember) and
    (TInstantCodeMember(Sender).Visibility in Visibilities)
end;

{ TInstantCodeClassList }

function CompareCodeClasses(Item1, Item2: Pointer): Integer;
var
  CodeClass1, CodeClass2: TInstantCodeClass;
begin
  CodeClass1 := Item1;
  CodeClass2 := Item2;

  if (CodeClass1.BaseClass = nil) and (CodeClass2.BaseClass <> nil) then
    Result := -1 else
  if (CodeClass2.BaseClass = nil) and (CodeClass1.BaseClass <> nil) then
    Result := 1 else
  if CodeClass1.BaseClass = CodeClass2.BaseClass then
    Result := 0 else
  if CodeClass1.DerivesFrom(CodeClass2) then
    Result := 1 else
  if CodeClass2.DerivesFrom(CodeClass1) then
    Result := -1 else
    // CodeClass1.BaseClass <> CodeClass2.BaseClass
    Result := CompareCodeClasses(CodeClass1.BaseClass, CodeClass2.BaseClass);
end;

function TInstantCodeClassList.Add(Item: TInstantCodeClass): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TInstantCodeClassList.Insert(Index: Integer; Item: TInstantCodeClass);
begin
  inherited Insert(Index, Item);
end;

procedure TInstantCodeClassList.SortByBaseClass;
begin
  Sort(CompareCodeClasses);
end;

function TInstantCodeClassList.GetItem(Index: Integer): TInstantCodeClass;
begin
  Result := inherited Items[Index];
end;

procedure TInstantCodeClassList.SetItem(Index: Integer;
  const Value: TInstantCodeClass);
begin
  inherited Items[Index] := Value;
end;

{ TInstantCodeClassRef }

class function TInstantCodeClassRef.Identifier: string;
begin
  Result := 'class';
end;

class function TInstantCodeClassRef.InternalAtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
begin
  Result := inherited InternalAtInstance(Reader, Name) and
    SameText(Reader.ReadToken, 'of');
end;

procedure TInstantCodeClassRef.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  Reader.ReadStatement;
end;

{ TInstantCodeInterface }

class function TInstantCodeInterface.Identifier: string;
begin
  Result := 'interface';
end;

procedure TInstantCodeInterface.InternalRead(Reader: TInstantCodeReader);
var
  Token: string;
begin
  inherited;
  Reader.ReadToken;
  Token := LowerCase(Reader.ReadToken);
  if Token = '(' then
  begin
    Reader.ReadNext(')');
    Token := LowerCase(Reader.ReadToken);
  end;
  if Token = '[' then
  begin
    Reader.ReadNext(']');
    Token := LowerCase(Reader.ReadToken);
  end;
  while not Reader.Finished do
  begin
    Reader.ReadStatement;
    if Token = 'end' then
      Break;
    Token := LowerCase(Reader.ReadToken);
  end;
end;

{ TInstantCodeDispInterface }

class function TInstantCodeDispInterface.Identifier: string;
begin
  Result := 'dispinterface';
end;

{ TInstantCodeParagraph }

function TInstantCodeParagraph.GetOwner: TInstantCodeSection;
begin
  Result := FOwner as TInstantCodeSection;
end;

procedure TInstantCodeParagraph.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  Reader.ReadToken;
  Reader.SkipSpace;
end;

procedure TInstantCodeParagraph.Reset;
begin
  inherited;
  Clear;
end;

{ TInstantCodeUsesClause }

function TInstantCodeUsesClause.Add: TInstantCodeUses;
begin
  Result := TInstantCodeUses.Create(Self);
end;

function TInstantCodeUsesClause.Find(const Name: string): TInstantCodeUses;
begin
  Result := inherited Find(Name) as TInstantCodeUses;
end;

function TInstantCodeUsesClause.GetCount: Integer;
begin
  Result := ObjectCount;
end;

function TInstantCodeUsesClause.GetItems(Index: Integer): TInstantCodeUses;
begin
  Result := Objects[Index] as TInstantCodeUses;
end;

class function TInstantCodeUsesClause.Identifier: string;
begin
  Result := 'uses';
end;

procedure TInstantCodeUsesClause.InternalRead(Reader: TInstantCodeReader);
var
  Token: string;
  Ident: string;
  BeginPos: TInstantCodePos;
  UsedUnit: TInstantCodeUses;
begin
  inherited;
  while not Reader.Finished and not Reader.ReadEndOfStatement do
  begin
    Reader.SkipSpace;
    BeginPos := Reader.Position;
    Ident := Reader.ReadIdent(True);
    if (Ident = '') then
      Reader.ErrorExpected('unit name', False);
    UsedUnit := Add;
    UsedUnit.Name := Ident;
    UsedUnit.EndPos := Reader.Position;
    UsedUnit.StartPos := BeginPos;
    Token := Reader.ReadToken;
    if SameText(Token, 'in') then
    begin
      Reader.ReadString;
      Token := Reader.ReadToken;
    end;
    if Token = ';' then
      Break;
    if Token <> ',' then
      Reader.ErrorExpected(', or ; ' + Token);
  end;
end;

procedure TInstantCodeUsesClause.InternalWrite(Writer: TInstantCodeWriter);
var
  I: Integer;
begin
  inherited;
  if Count > 0 then
  begin
    Writer.WriteLn('uses');
    Writer.Indent;
    try
      for I := 0 to Pred(Count) do
      begin
        if I > 0 then
          Writer.Write(', ');
        Items[I].Write(Writer);
      end;
      Writer.WriteLn(';');
    finally
      Writer.Unindent;
    end;
  end;
end;

{ TInstantCodeTypes }

function TInstantCodeTypes.AddClass(
  AClass: TInstantCodeClass): TInstantCodeClass;
begin
  if Assigned(AClass) then
  begin
    AddObject(AClass);
    Result := AClass;
  end else
    Result := TInstantCodeClass.Create(Self);
end;

function TInstantCodeTypes.AddEnum: TInstantCodeEnum;
begin
  Result := TInstantCodeEnum.Create(Self);
end;

function TInstantCodeTypes.GetCount: Integer;
begin
  Result := ObjectCount;
end;

function TInstantCodeTypes.GetItems(Index: Integer): TInstantCodeType;
begin
  Result := Objects[Index] as TInstantCodeType;
end;

class function TInstantCodeTypes.Identifier: string;
begin
  Result := 'type';
end;

procedure TInstantCodeTypes.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  ReadObjects(Reader, [TInstantCodeRecord, TInstantCodeClassRef,
    TInstantCodeClassForward, TInstantCodeClass, TInstantCodeInterface,
    TInstantCodeDispInterface, TInstantCodeEnum, TInstantCodeSet,
    TInstantCodeTypeDef, TInstantCodeProcType, TInstantCodeClassRttiAttribute,
    TInstantCodeUnknownType]);
end;

{ TInstantCodeVars }

function TInstantCodeVars.AddItem: TInstantCodeObject;
begin
  Result := TInstantCodeVar.Create(Self);
end;

function TInstantCodeVars.GetItems(Index: Integer): TInstantCodeVar;
begin
  Result := Objects[Index] as TInstantCodeVar;
end;

class function TInstantCodeVars.Identifier: string;
begin
  Result := 'var';
end;

{ TInstantCodeSymbols }

function TInstantCodeSymbols.AtItem(Reader: TInstantCodeReader): Boolean;
var
  SavePos: TInstantCodePos;
begin
  SavePos := Reader.Position;
  try
    Reader.SkipSpace;
    Reader.ReadToken;
    Reader.SkipSpace;
    Result := InstantCharInSet(Reader.NextChar, [':', '=']);
  finally
    Reader.Position := SavePos;
  end;
end;

procedure TInstantCodeSymbols.InternalRead(Reader: TInstantCodeReader);
begin
  inherited;
  while not Reader.Finished and AtItem(Reader) do
  begin
    Reader.SkipSpace;
    AddItem.Read(Reader)
  end;
end;

{ TInstantCodeConsts }

function TInstantCodeConsts.AddItem: TInstantCodeObject;
begin
  Result := TInstantCodeConst.Create(Self);
end;

function TInstantCodeConsts.GetItems(Index: Integer): TInstantCodeConst;
begin
  Result := Objects[Index] as TInstantCodeConst;
end;

class function TInstantCodeConsts.Identifier: string;
begin
  Result := 'const';
end;

{ TInstantCodeConst }

procedure TInstantCodeConst.InternalRead(Reader: TInstantCodeReader);
begin
  Name := Reader.ReadToken;
  Reader.SkipSpace;
  if Reader.NextChar = ':' then
  begin
    { Typed const }
    Reader.ReadChar;
    Reader.ReadNext('=');
  end else
    Reader.ReadTokenMatching('=');
  Reader.ReadStatement;
end;

{ TInstantCodeResStrings }

function TInstantCodeResStrings.AddItem: TInstantCodeObject;
begin
  Result := TInstantCodeResString.Create(Self);
end;

function TInstantCodeResStrings.GetItems(Index: Integer): TInstantCodeResString;
begin
  Result := Objects[Index] as TInstantCodeResString;
end;

class function TInstantCodeResStrings.Identifier: string;
begin
  Result := 'resourcestring';
end;

{ TInstantCodeParameters }

function TInstantCodeParameters.Add: TInstantCodeParameter;
begin
  Result := TInstantCodeParameter.Create(Self);
end;

function TInstantCodeParameters.AddParameter(const AName, ATypeName: string;
  AOptions: TInstantCodeParameterOptions): TInstantCodeParameter;
begin
  Result := Add;
  with Result do
  begin
    Name := AName;
    TypeName := ATypeName;
    Options := AOptions;
  end;
end;

procedure TInstantCodeParameters.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited;
  if Source is TInstantCodeParameters then
    with TInstantCodeParameters(Source) do
    begin
      Self.Clear;
      for I := 0 to Pred(Count) do
        Self.Add.Assign(Items[I]);
    end;
end;

function TInstantCodeParameters.GetCount: Integer;
begin
  Result := ObjectCount;
end;

function TInstantCodeParameters.GetItems(Index: Integer): TInstantCodeParameter;
begin
  Result := Objects[Index] as TInstantCodeParameter;
end;

procedure TInstantCodeParameters.InternalRead(Reader: TInstantCodeReader);
var
  Ch: Char;
begin
  if Reader.NextChar = '(' then
  begin
    Reader.ReadChar;
    while not Reader.Finished do
    begin
      Reader.SkipSpace;
      if Reader.NextChar = ')' then
      begin
        Reader.ReadChar;
        Break;
      end;
      Add.Read(Reader);
      Reader.SkipSpace;
      while not Reader.Finished do
      begin
        Ch := Reader.ReadChar;
        if Ch = ';' then
          Break;
        if Ch = ')' then
          Exit;
      end;
    end;
  end;
end;

procedure TInstantCodeParameters.InternalWrite(Writer: TInstantCodeWriter);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    if I > 0 then
      Writer.Write('; ');
    Items[I].Write(Writer);
  end;
end;

{ TInstantCodePrototype }

function TInstantCodePrototype.AddParameter: TInstantCodeParameter;
begin
  Result := Parameters.Add;
end;

procedure TInstantCodePrototype.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodePrototype then
    with TInstantCodePrototype(Source) do
    begin
      Self.Options := Options;
      Self.Parameters.Assign(Parameters);
      Self.ResultTypeName := ResultTypeName;
    end;
end;

constructor TInstantCodePrototype.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FResultTypeLink := TInstantCodeTypeLink.Create(Self);
end;

destructor TInstantCodePrototype.Destroy;
begin
  FResultTypeLink.Free;
  inherited;
end;

function TInstantCodePrototype.GetIsFunction: Boolean;
begin
  Result := ResultTypeName <> '';
end;

function TInstantCodePrototype.GetParameters: TInstantCodeParameters;
begin
  if not Assigned(FParameters) then
    FParameters := TInstantCodeParameters.Create(Self);
  Result := FParameters;
end;

function TInstantCodePrototype.GetResultType: TInstantCodeType;
begin
  Result := FResultTypeLink.Instance;
end;

function TInstantCodePrototype.GetResultTypeName: string;
begin
  Result := FResultTypeLink.Name;
end;

class function TInstantCodePrototype.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
var
  Token: string;
begin
  Token := Reader.ReadToken;
  if SameText(Token, 'class') then
    Result := IsIdentifier(Reader.ReadToken)
  else
    Result := IsIdentifier(Token);
  if Result then
    Name := Reader.ReadToken;
end;

procedure TInstantCodePrototype.InternalRead(Reader: TInstantCodeReader);
var
  Token: string;
  ProcName: string;
begin
  Token := Reader.ReadToken;
  if SameText(Token, 'class') then
  begin
    Options := Options + [poClass];
    Token := Reader.ReadToken;
  end;
  if SameText(Token, 'constructor') then
    Options := Options + [poConstructor]
  else if SameText(Token, 'destructor') then
    Options := Options + [poDestructor]
  else if not (SameText(Token, 'procedure') or SameText(Token, 'function')) then
    Reader.ErrorExpected('procedure');
  ProcName := Reader.ReadToken;
  if Reader.NextChar = '.' then
    ProcName := ProcName + Reader.ReadChar + Reader.ReadToken;
  Name := ProcName;
  Reader.SkipSpace;
  if Reader.NextChar = '(' then
    Parameters.Read(Reader);
  Reader.SkipSpace;
  if Reader.NextChar = ':' then
  begin
    Reader.ReadChar;
    ResultTypeName := Reader.ReadToken;
  end;
  Reader.ReadStatement;
  ReadFlags(Reader);
end;

procedure TInstantCodePrototype.InternalWrite(Writer: TInstantCodeWriter);
begin
  if poClass in Options then
    Writer.Write('class ');
  if poConstructor in Options then
    Writer.Write('constructor ')
  else if poDestructor in Options then
    Writer.Write('destructor ')
  else if IsFunction then
    Writer.Write('function ')
  else
    Writer.Write('procedure ');
  Writer.Write(Name);
  if Parameters.Count > 0 then
  begin
    Writer.Write('(');
    Parameters.Write(Writer);
    Writer.Write(')');
  end;
  if IsFunction then
    Writer.Write(': ' + ResultTypeName);
  Writer.Write(';');
  WriteFlags(Writer);
end;

class function TInstantCodePrototype.IsIdentifier(const Str: string): Boolean;
const
  Identifiers: array[0..3] of string =
    ('procedure', 'function', 'constructor', 'destructor');
var
  I: Integer;
begin
  for I := Low(Identifiers) to High(Identifiers) do
  begin
    Result := SameText(Str, Identifiers[I]);
    if Result then
      Exit;
  end;
end;

procedure TInstantCodePrototype.ReadFlags(Reader: TInstantCodeReader);
var
  NewFlags: TInstantCodeProcFlags;
begin
  NewFlags := Flags;
  if Reader.ReadFlags(@NewFlags,
    TypeInfo(TInstantCodeProcFlag), 'pf') then
    Flags := NewFlags;
end;

procedure TInstantCodePrototype.SetResultType(const Value: TInstantCodeType);
begin
  FResultTypeLink.Instance := Value;
end;

procedure TInstantCodePrototype.SetResultTypeName(const Value: string);
begin
  FResultTypeLink.Name := Value;
end;

procedure TInstantCodePrototype.Write(Writer: TInstantCodeWriter;
  Method: TInstantCodeMethod);
var
  SaveName: string;
begin
  if Assigned(Method) then
  begin
    SaveName := Name;
    Name := Method.Name;
    try
      inherited Write(Writer);
    finally
      Name := SaveName;
    end;
  end else
    inherited Write(Writer);
end;

procedure TInstantCodePrototype.WriteFlags(Writer: TInstantCodeWriter);
var
  AFlags: TInstantCodeProcFlags;
begin
  AFlags := Flags;
  Writer.WriteFlags(@AFlags, TypeInfo(TInstantCodeProcFlag), 'pf');
end;

{ TInstantCodeProc }

procedure TInstantCodeProc.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TInstantCodeProc then
    with TInstantCodeProc(Source) do
    begin
      Self.Header.Assign(Header);
      Self.Body.Assign(Body);
    end;
end;

constructor TInstantCodeProc.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  if Assigned(Section) then
    Section.InsertProc(Self);
  FHeader := TInstantCodePrototype.Create(nil);
  FBody := TInstantCodeBody.Create(nil);
end;

destructor TInstantCodeProc.Destroy;
begin
  if Assigned(Section) then
    Section.RemoveProc(Self);
  FBody.Free;
  FHeader.Free;
  inherited;
end;

function TInstantCodeProc.GetHasBody: Boolean;
begin
  Result := (Owner.Owner is TInstantCodeImplementationSection)
    and not (pfForward in FHeader.Flags);
end;

function TInstantCodeProc.GetName: string;
begin
  Result := FHeader.Name;
end;

function TInstantCodeProc.GetOwner: TInstantCodeProcs;
begin
  Result := inherited Owner as TInstantCodeProcs; 
end;

function TInstantCodeProc.GetParameters: TInstantCodeParameters;
begin
  Result := FHeader.Parameters;
end;

function TInstantCodeProc.GetResultTypeName: string;
begin
  Result := FHeader.ResultTypeName;
end;

function TInstantCodeProc.GetSection: TInstantCodeSection;
begin
  if Assigned(Owner) then
    Result := Owner.Owner
  else
    Result := nil;
end;

class function TInstantCodeProc.InternalAtInstance(
  Reader: TInstantCodeReader; out Name: string): Boolean;
begin
  Result := TInstantCodePrototype.AtInstance(Reader, Name);
end;

procedure TInstantCodeProc.InternalRead(Reader: TInstantCodeReader);
begin
  FHeader.Read(Reader);
  if HasBody then
    ReadBody(Reader);
end;

procedure TInstantCodeProc.InternalWrite(Writer: TInstantCodeWriter);
begin
  FHeader.Write(Writer);
  if HasBody then
  begin
    Writer.WriteLn;
    WriteBody(Writer);
  end;
end;

procedure TInstantCodeProc.ReadBody(Reader: TInstantCodeReader);

  procedure ReadProc;
  var
    Level: Integer;
    Token: string;
  begin
    Level := 0;
    while not Reader.Finished do
    begin
      Token := Reader.ReadToken;
      if SameText(Token, 'function') or SameText(Token, 'procedure') then
        ReadProc
      else if SameText(Token, 'begin') or SameText(Token, 'try')
        or SameText(Token, 'case') or SameText(Token, 'asm') then
        Inc(Level)
      else if SameText(Token, 'end') then
      begin
        Dec(Level);
        if Level = 0 then
          Break;
      end;
    end;
    Reader.ReadStatement;
  end;

begin
  ReadProc;
end;

procedure TInstantCodeProc.SetName(const Value: string);
begin
  FHeader.Name := Value;
end;

procedure TInstantCodeProc.SetResultTypeName(const Value: string);
begin
  FHeader.ResultTypeName := Value;
end;

procedure TInstantCodeProc.WriteBody(Writer: TInstantCodeWriter);
begin
  Writer.WriteLn('begin');
  Writer.Indent;
  try
    FBody.Write(Writer);
  finally
    Writer.Unindent;
    Writer.Write('end;');
  end;
end;

{ TInstantCodeProcLink }

function TInstantCodeProcLink.FindInstance: TInstantCodeObject;
begin
  if Assigned(Module) then
    Result := Module.ImplementationSection.FindProc(Name)
  else
    Result := nil;
end;

function TInstantCodeProcLink.GetInstance: TInstantCodeProc;
begin
  Result := inherited Instance as TInstantCodeProc;
end;

procedure TInstantCodeProcLink.SetInstance(Value: TInstantCodeProc);
begin
  inherited Instance := Value;
end;

{ TInstantCodeProcs }

function TInstantCodeProcs.Add: TInstantCodeProc;
begin
  Result := TInstantCodeProc.Create(Self);
end;

function TInstantCodeProcs.Find(const Name: string): TInstantCodeProc;
begin
  Result := inherited Find(Name) as TInstantCodeProc; 
end;

function TInstantCodeProcs.GetCount: Integer;
begin
  Result := ObjectCount;
end;

function TInstantCodeProcs.GetItems(Index: Integer): TInstantCodeProc;
begin
  Result := Objects[Index] as TInstantCodeProc;
end;

class function TInstantCodeProcs.InternalAtInstance(Reader: TInstantCodeReader;
  out Name: string): Boolean;
begin
  Result := TInstantCodeProc.AtInstance(Reader, Name);
end;

procedure TInstantCodeProcs.InternalRead(Reader: TInstantCodeReader);
var
  SavePos: TInstantCodePos;
begin
  while not Reader.Finished do
  begin
    SavePos := Reader.Position;
    Reader.SkipSpace;
    if not AtInstance(Reader) then
    begin
      Reader.Position := SavePos;
      Exit;
    end;
    Add.Read(Reader);
  end;
end;

{ TInstantCodeSection }

function TInstantCodeSection.AddClass(
  AClass: TInstantCodeClass): TInstantCodeClass;
var
  Types: TInstantCodeTypes;
begin
  Types := FindTypes;
  if not Assigned(Types) then
    Types := TInstantCodeTypes.Create(Self);
  Result := Types.AddClass(AClass);
end;

function TInstantCodeSection.AddProc: TInstantCodeProc;
var
  Procs: TInstantCodeProcs;
begin
  Procs := FindProcs;
  if not Assigned(Procs) then
    Procs := TInstantCodeProcs.Create(Self);
  Result := Procs.Add;
end;

function TInstantCodeSection.AtEndOfSection(
  Reader: TInstantCodeReader): Boolean;
var
  SavePos: TInstantCodePos;
begin
  Result := (NextSectionClass <> nil) and NextSectionClass.AtInstance(Reader);
  if not Result then
  begin
    SavePos := Reader.Position;
    Result := SameText(Reader.ReadToken, 'end');
    Reader.Position := SavePos;
  end;
end;

constructor TInstantCodeSection.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FProcs := TList.Create;
end;

destructor TInstantCodeSection.Destroy;
begin
  inherited;
  FProcs.Free;
end;

function TInstantCodeSection.FindNearestProcs(Proc: TInstantCodeProc;
  out Prior, Next: TObject): Boolean;
begin
  Result := FindNearest(FProcs, Proc, Prior, Next);
end;

function TInstantCodeSection.FindProc(
  const Name: string): TInstantCodeProc;
var
  Procs: TInstantCodeProcs;
  I: Integer;
begin
  for I := 0 to Pred(ObjectCount) do
  begin
    if Objects[I] is TInstantCodeProcs then
    begin
      Procs := TInstantCodeProcs(Objects[I]);
      Result := Procs.Find(Name);
      if Assigned(Result) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TInstantCodeSection.FindProcs: TInstantCodeProcs;
begin
  Result := TInstantCodeProcs(FindFirst(TInstantCodeProcs));
end;

function TInstantCodeSection.FindTypes: TInstantCodeTypes;
begin
  Result := TInstantCodeTypes(FindFirst(TInstantCodeTypes));
end;

function TInstantCodeSection.FindUsesClause: TInstantCodeUsesClause;
begin
  Result := TInstantCodeUsesClause(FindFirst(TInstantCodeUsesClause));
end;

function TInstantCodeSection.GetProcCount: Integer;
begin
  Result := FProcs.Count;
end;

function TInstantCodeSection.GetProcs(Index: Integer): TInstantCodeProc;
begin
  Result := FProcs[Index];
end;

procedure TInstantCodeSection.InsertProc(Proc: TInstantCodeProc);
begin
  FProcs.Add(Proc);
end;

procedure TInstantCodeSection.InternalRead(Reader: TInstantCodeReader);
begin
  if Reader.Finished then
    Exit;
  Reader.ReadTokenMatching(Identifier);
  ReadParagraphs(Reader);
end;

procedure TInstantCodeSection.InternalWrite(Writer: TInstantCodeWriter);
begin
  Writer.WriteLn(Identifier);
  Writer.WriteLn;
  WriteObjects(Writer);
end;

class function TInstantCodeSection.NextSectionClass: TInstantCodeSectionClass;
begin
  Result := nil;
end;

procedure TInstantCodeSection.ReadParagraphs(Reader: TInstantCodeReader);
begin
  while not Reader.Finished do
  begin
    ReadObjects(Reader, [TInstantCodeUsesClause, TInstantCodeTypes,
      TInstantCodeConsts, TInstantCodeResStrings, TInstantCodeVars,
      TInstantCodeProcs]);
    Reader.SkipSpace;
    if AtEndOfSection(Reader) then
      Break
    else
      Reader.ReadToken;
  end;
end;

procedure TInstantCodeSection.RemoveProc(Proc: TInstantCodeProc);
begin
  FProcs.Remove(Proc);
end;

procedure TInstantCodeSection.Reset;
begin
  Clear;
  inherited;
end;

{ TInstantCodeInterfaceSection }

class function TInstantCodeInterfaceSection.Identifier: string;
begin
  Result := 'interface';
end;

procedure TInstantCodeInterfaceSection.InternalRead(
  Reader: TInstantCodeReader);
begin
  inherited;
end;

class function TInstantCodeInterfaceSection.NextSectionClass: TInstantCodeSectionClass;
begin
  Result := TInstantCodeImplementationSection;
end;

{ TInstantCodeImplementationSection }

class function TInstantCodeImplementationSection.Identifier: string;
begin
  Result := 'implementation';
end;

procedure TInstantCodeImplementationSection.InternalRead(
  Reader: TInstantCodeReader);
begin
  inherited;
end;

class function TInstantCodeImplementationSection.NextSectionClass: TInstantCodeSectionClass;
begin
  Result := TInstantCodeInitializationSection;
end;

{ TInstantCodeInitializationSection }

class function TInstantCodeInitializationSection.Identifier: string;
begin
  Result := 'initialization';
end;

class function TInstantCodeInitializationSection.NextSectionClass: TInstantCodeSectionClass;
begin
  Result := TInstantCodeFinalizationSection;
end;

{ TInstantCodeFinalizationSection }

class function TInstantCodeFinalizationSection.Identifier: string;
begin
  Result := 'finalization';
end;

{ TInstantCodeProgramSection }

procedure TInstantCodeProgramSection.InternalRead(
  Reader: TInstantCodeReader);
begin
  ReadParagraphs(Reader);
end;

{ TInstantCodeWriter }

function TInstantCodeWriter.GetStreamPos: Int64;
begin
  Result := inherited GetStreamPos + (Origin.Offset * SizeOf(Char));
end;

procedure TInstantCodeWriter.Indent;
begin
  Inc(FMargin, 2);
end;

procedure TInstantCodeWriter.SetOrigin(Value: TInstantCodePos);
begin
  FOrigin := Value;
  Position := FOrigin;
end;

procedure TInstantCodeWriter.SetStreamPos(Value: Int64);
begin
  inherited SetStreamPos(Value - (Origin.Offset * SizeOf(Char)));
end;

procedure TInstantCodeWriter.Unindent;
begin
  Dec(FMargin, 2);
end;

procedure TInstantCodeWriter.Write(Str: string);
begin
  WriteString(Str);
end;

procedure TInstantCodeWriter.WriteChar(Ch: Char);
begin
  if Ch <> #10 then
    WriteMargin;
  inherited;
end;

procedure TInstantCodeWriter.WriteFlags(Flags: Pointer; TypeInfo: PTypeInfo;
  const Prefix: string);
type
  PData = ^TData;
  TData = set of Byte;
var
  Data: PData;
  EnumNames: TStringList;
  FlagName: string;
  I: Integer;
begin
  EnumNames := TStringList.Create;
  try
    InstantGetEnumNames(TypeInfo, EnumNames);
    Data := Flags;
    for I := 0 to Pred(EnumNames.Count) do
    begin
      if GetEnumValue(TypeInfo, EnumNames[I]) in Data^ then
      begin
        FlagName := EnumNames[I];
        Delete(FlagName, 1, Length(Prefix));
        Write(' ' + FlagName + ';');
      end;
    end;
  finally
    EnumNames.Free;
  end;
end;

procedure TInstantCodeWriter.WriteFmt(Fmt: string; Args: array of const);
begin
  Write(Format(Fmt, Args));
end;

procedure TInstantCodeWriter.WriteLn(Str: string);
begin
  Write(Str + CRLF);
end;

procedure TInstantCodeWriter.WriteLnFmt(Fmt: string; Args: array of const);
begin
  WriteLn(Format(Fmt, Args));
end;

procedure TInstantCodeWriter.WriteMargin;
begin
  while Position.Column <= FMargin do
    inherited WriteChar(' ');
end;

{ TInstantCodeReader }

function TInstantCodeReader.EnterComment: Boolean;
begin
  Result := NextChar = '{';
  if Result then
  begin
    IgnoreComments := False;
    try
      ReadChar;
    finally
      IgnoreComments := True;
    end;
    SkipSpace;
  end;
end;

procedure TInstantCodeReader.ErrorExpected(const Expected: string;
  Str: Boolean);
var
  S: string;
begin
  if Str then
    S := '''' + Expected + ''''
  else
    S := Expected;
  ErrorMessage('Expected ' + S);
end;

procedure TInstantCodeReader.ErrorMessage(const Msg: string);
begin
  raise EInstantCodeError.Create(Msg, Self);
end;

function TInstantCodeReader.Find(Text: string): Boolean;
var
  SavePos: TInstantCodePos;
begin
  SavePos := Position;
  ReadNext(Text, True);
  Result := not Eof;
  if not Result then
    Position := SavePos;
end;

function TInstantCodeReader.GetFinished: Boolean;
var
  Continue: Boolean;
begin
  Continue := True;
  if Assigned(InstantCodeReaderIdle) then
    InstantCodeReaderIdle(Self, Continue);
  Result := Eof or not Continue;
end;

procedure TInstantCodeReader.Initialize;
begin
  inherited;
  FIgnoreComments := True;
end;

function TInstantCodeReader.ReadChar: Char;
var
  WasConstAware, IsComment: Boolean;
begin
  Result := inherited ReadChar;
  if IgnoreComments then
  begin
    IgnoreComments := False;
    WasConstAware := ConstAware;
    ConstAware := False;
    IsComment := False;
    try
      case Result of
        '{':
          begin
            ReadNext('}');
            IsComment := True;
          end;
        '(':
          if NextChar = '*' then
          begin
            ReadNext('*)');
            IsComment := True;
          end;
        '/':
          if NextChar = '/' then
          begin
              ReadNext(#13);
            IsComment := True;
          end;
      end;
    finally
      ConstAware := WasConstAware;
      IgnoreComments := True;
      if IsComment then
        Result := ReadChar;
    end;
  end;
end;

function TInstantCodeReader.ReadEndOfStatement(Required: Boolean): Boolean;
begin
  SkipSpace;
  Result := NextChar = ';';
  if Result then
    ReadChar
  else if Required then
    ErrorExpected(';');
end;

function TInstantCodeReader.ReadFlags(Flags: Pointer; TypeInfo: PTypeInfo;
  const Prefix: string): Boolean;
type
  PData = ^TData;
  TData = set of Byte;
var
  Data: PData;
  SavePos: TInstantCodePos;
  Token: string;
  EnumNames: TStringList;

  function SetFlag(const FlagName: string): Boolean;
  var
    Index: Integer;
  begin
    Index := EnumNames.IndexOf(Prefix + FlagName);
    Result := Index <> -1;
    if Result then
      Include(Data^, Index);
  end;

begin
  Result := False;
  EnumNames := TStringList.Create;
  try
    InstantGetEnumNames(TypeInfo, EnumNames);
    Data := Flags;
    while not Finished do
    begin
      SavePos := Position;
      Token := ReadToken;
      if SetFlag(Token) then
      begin
        Result := True;
        ReadStatement;
      end else
      begin
        Position := SavePos;
        Break;
      end;
    end;
  finally
    EnumNames.Free;
  end;
end;

function TInstantCodeReader.ReadInteger: Integer;
var
  Token: string;
begin
  Token := ReadToken;
  try
    Result := StrToInt(Token);
  except
    ErrorExpected('integer value', False);
    Result := 0;
  end;
end;

function TInstantCodeReader.ReadMacro(Macros: array of string): string;
var
  SavePos: TInstantCodePos;
  I: Integer;
begin
  SkipSpace;
  SavePos := Position;
  Result := ReadToken;
  I := Low(Macros);
  while I < High(Macros) do
  begin
    if SameText(Macros[I], Result) then
    begin
      Result := Macros[I + 1];
      Exit;
    end;
    Inc(I, 2);
  end;
  Position := SavePos;
  Result := ReadString;
end;

function TInstantCodeReader.ReadRestOfStatement: string;
var
  SavePos: TInstantCodePos;
  Ch: Char;
begin
  Result := '';
  while not Finished do
  begin
    SavePos := Position;
    Ch := ReadChar;
    if Ch = ';' then
    begin
      Position := SavePos;
      Exit;
    end;
    Result := Result + Ch;
  end;
end;

function TInstantCodeReader.ReadStatement: string;
begin
  Result := ReadNext(';');
end;

function TInstantCodeReader.ReadString: string;
var
  SaveIgnoreComments: Boolean;
begin
  SaveIgnoreComments := IgnoreComments;
  IgnoreComments := False;
  try
    Result := inherited ReadString;
  finally
    IgnoreComments := SaveIgnoreComments;
  end;
end;

function TInstantCodeReader.ReadStringValue: string;
begin
  Result := ReadString;
  Result := Copy(Result, 2, Length(Result) - 2);
end;

function TInstantCodeReader.ReadTextChar: Char;
var
  SaveIgnoreComments: Boolean;
begin
  SaveIgnoreComments := IgnoreComments;
  IgnoreComments := False;
  try
    Result := ReadChar;
  finally
    IgnoreComments := SaveIgnoreComments;
  end;
end;

function TInstantCodeReader.ReadTokenIgnoring(Ignore: string): string;
begin
  repeat
    Result := ReadToken;
  until not SameText(Result, Ignore);
end;

{ TInstantCodeModule }

constructor TInstantCodeModule.Create(AOwner: TInstantCodeObject);
begin
  inherited;
  FModuleType := mtUnit;
  FClasses := TList.Create;
  FTypes := TList.Create;
end;

destructor TInstantCodeModule.Destroy;
begin
  inherited;
  FClasses.Free;
  FTypes.Free;
end;

function TInstantCodeModule.FindClass(
  const Name: string): TInstantCodeClass;
var
  I: Integer;
begin
  for I := 0 to Pred(ClassCount) do
  begin
    Result := Classes[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  if Assigned(Owner) then
    Result := Owner.FindClass(Name)
  else
    Result := nil;
end;

function TInstantCodeModule.FindType(const Name: string): TInstantCodeType;
var
  I: Integer;
begin
  for I := 0 to Pred(TypeCount) do
  begin
    Result := Types[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  if Assigned(Owner) then
    Result := Owner.FindType(Name)
  else
    Result := nil;
end;

function TInstantCodeModule.GetClassCount: Integer;
begin
  Result := FClasses.Count
end;

function TInstantCodeModule.GetClasses(Index: Integer): TInstantCodeClass;
begin
  Result := TInstantCodeClass(FClasses[Index]);
end;

function TInstantCodeModule.GetFinalizationSection: TInstantCodeFinalizationSection;
begin
  if not Assigned(FFinalizationSection) then
    FFinalizationSection := TInstantCodeFinalizationSection.Create(Self);
  Result := FFinalizationSection;
end;

function TInstantCodeModule.GetImplementationSection: TInstantCodeImplementationSection;
begin
  if not Assigned(FImplementationSection) then
    FImplementationSection := TInstantCodeImplementationSection.Create(Self);
  Result := FImplementationSection;
end;

function TInstantCodeModule.GetInitializationSection: TInstantCodeInitializationSection;
begin
  if not Assigned(FInitializationSection) then
    FInitializationSection := TInstantCodeInitializationSection.Create(Self);
  Result := FInitializationSection;
end;

function TInstantCodeModule.GetInterfaceSection: TInstantCodeInterfaceSection;
begin
  if not Assigned(FInterfaceSection) then
    FInterfaceSection := TInstantCodeInterfaceSection.Create(Self);
  Result := FInterfaceSection;
end;

function TInstantCodeModule.GetModule: TInstantCodeModule;
begin
  Result := Self;
end;

function TInstantCodeModule.GetModuleTypeName: string;
begin
  Result := ModuleTypeNames[ModuleType];
end;

function TInstantCodeModule.GetOwner: TInstantCodeProject;
begin
  Result := inherited Owner as TInstantCodeProject;
end;

function TInstantCodeModule.GetProgramSection: TInstantCodeProgramSection;
begin
  if not Assigned(FProgramSection) then
    FProgramSection := TInstantCodeProgramSection.Create(Self);
  Result := FProgramSection;
end;

function TInstantCodeModule.GetTypeCount: Integer;
begin
  Result := FTypes.Count;
end;

function TInstantCodeModule.GetTypes(Index: Integer): TInstantCodeType;
begin
  Result := TInstantCodeType(FTypes[Index]);
end;

function TInstantCodeModule.GetPascalUnitName: string;
begin
  Result := ChangeFileExt(ExtractFileName(Name), '');
end;

procedure TInstantCodeModule.InsertType(AType: TInstantCodeType);
begin
  FTypes.Add(AType);
  if AType is TInstantCodeClass then
    FClasses.Add(AType);
  Owner.InsertType(AType);
end;

procedure TInstantCodeModule.InternalRead(Reader: TInstantCodeReader);
begin
  Read(Reader, scInterface);
end;

procedure TInstantCodeModule.InternalWrite(Writer: TInstantCodeWriter);
begin
  Writer.WriteLnFmt('%s %s;', [ModuleTypeName, PascalUnitName]);
  Writer.WriteLn;
  InterfaceSection.Write(Writer);
  ImplementationSection.Write(Writer);
  InitializationSection.Write(Writer);
  FinalizationSection.Write(Writer);
  Writer.WriteLn('end.');
end;

procedure TInstantCodeModule.LoadFromFile(const FileName: string;
  Scope: TInstantCodeScope);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream, Scope);
  finally
    Stream.Free;
  end;
end;

procedure TInstantCodeModule.LoadFromStream(Stream: TStream;
  Scope: TInstantCodeScope);
var
  Reader: TInstantCodeReader;
begin
  Reader := TInstantCodeReader.Create(Stream);
  try
    Reset;
    Read(Reader, Scope);
  finally
    Reader.Free;
  end;
end;

procedure TInstantCodeModule.LoadFromString(const Str: string;
  Scope: TInstantCodeScope);
var
  Stream: TInstantStringStream;
begin
  Stream := TInstantStringStream.Create(Str);
  try
    LoadFromStream(Stream, Scope);
  finally
    Stream.Free;
  end;
end;

procedure TInstantCodeModule.Read(Reader: TInstantCodeReader;
  Scope: TInstantCodeScope);
var
  Token: string;
begin
  try
    Token := Reader.ReadToken;
    if Token <> '' then
    begin
      ModuleTypeName := Token;
      Name := Reader.ReadToken;
      Reader.ReadStatement;
      Reader.SkipSpace;
      case ModuleType of
        mtUnit:
          begin
            InterfaceSection.Read(Reader);
            Reader.SkipSpace;
            if Scope = scImplementation then
            begin
              ImplementationSection.Read(Reader);
              with InitializationSection do
                if AtInstance(Reader) then
                  Read(Reader);
              with FinalizationSection do
                if AtInstance(Reader) then
                  Read(Reader);
            end;
          end;
        mtProgram:
          ProgramSection.Read(Reader);
      end;
    end;
  except
    on E: Exception do
      Reader.ErrorMessage(E.Message);
  end;
end;

procedure TInstantCodeModule.RemoveType(AType: TInstantCodeType);
begin
  if AType is TInstantCodeClass then
    FClasses.Remove(AType);
  FTypes.Remove(AType);
  Owner.RemoveType(AType);
end;

procedure TInstantCodeModule.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TInstantCodeModule.SaveToStream(Stream: TStream);
var
  Writer: TInstantCodeWriter;
begin
  Writer := TInstantCodeWriter.Create(Stream);
  try
    Organize;
    Write(Writer);
  finally
    Writer.Free;
  end;
end;

procedure TInstantCodeModule.SetModuleTypeName(const Value: string);
var
  MT: TInstantCodeModuleType;
begin
  for MT := Low(TInstantCodeModuleType) to High(TInstantCodeModuleType) do
    if SameText(ModuleTypeNames[MT], Value) then
    begin
      FModuleType := MT;
      Exit;
    end;
  raise Exception.Create('Unknown module type: ' + Value);
end;

procedure TInstantCodeReader.ReadTokenMatching(Match: string);
var
  SavePos: TInstantCodePos;
begin
  SkipSpace;
  SavePos := Position;
  if not SameText(ReadToken, Match) then
  begin
    Position := SavePos;
    ErrorExpected(Match);
  end;
end;

procedure TInstantCodeReader.SkipBlanks;
var
  SaveIgnore: Boolean;
begin
  SaveIgnore := IgnoreComments;
  IgnoreComments := False;
  try
    SkipSpace;
  finally
    IgnoreComments := SaveIgnore;
  end;
end;

{ TInstantCodeProject }

constructor TInstantCodeProject.Create;
begin
  inherited Create(nil);
  FModel := TInstantModel.Create;
  FClasses := TList.Create;
  FTypes := TList.Create;
end;

function TInstantCodeProject.CreateClassMetadata: TInstantClassMetadata;
begin
  Result := Model.ClassMetadatas.Add;
end;

destructor TInstantCodeProject.Destroy;
begin
  inherited;
  FModel.Free;
  FClasses.Free;
  FTypes.Free;
end;

function TInstantCodeProject.FindClass(const Name: string): TInstantCodeClass;
var
  I: Integer;
begin
  for I := 0 to Pred(ClassCount) do
  begin
    Result := Classes[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeProject.FindModule(const Name: string): TInstantCodeModule;
var
  I: Integer;
begin
  for I := 0 to Pred(ModuleCount) do
  begin
    Result := Modules[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function TInstantCodeProject.FindType(const Name: string): TInstantCodeType;
var
  I: Integer;
begin
  for I := 0 to Pred(TypeCount) do
  begin
    Result := Types[I];
    if SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

procedure TInstantCodeProject.Fixup;
var
  I: Integer;
  AClass: TInstantCodeClass;
begin
  for I := 0 to Pred(ClassCount) do
  begin
    AClass := Classes[I];
    with AClass do
      if (BaseClassName <> '') and not Assigned(BaseClass) then
        BaseClass := FindClass(BaseClassName);
  end;
end;

function TInstantCodeProject.GetClassCount: Integer;
begin
  Result := FClasses.Count;
end;

function TInstantCodeProject.GetClasses(Index: Integer): TInstantCodeClass;
begin
  Result := FClasses[Index];
end;

function TInstantCodeProject.GetModuleCount: Integer;
begin
  Result := ObjectCount;
end;

function TInstantCodeProject.GetModules(
  Index: Integer): TInstantCodeModule;
begin
  Result := TInstantCodeModule(Objects[Index]);
end;

function TInstantCodeProject.GetTypeCount: Integer;
begin
  Result := FTypes.Count;
end;

function TInstantCodeProject.GetTypes(Index: Integer): TInstantCodeType;
begin
  Result := FTypes[Index];
end;

procedure TInstantCodeProject.InsertType(AType: TInstantCodeType);
begin
  FTypes.Add(AType);
  if AType is TInstantCodeClass then
    FClasses.Add(AType);
end;

procedure TInstantCodeProject.InternalClear;
begin
  inherited;
  FTypes.Clear;
  FClasses.Clear;
end;

function TInstantCodeProject.LoadModule(Stream: TStream;
  const FileName: string; Scope: TInstantCodeScope): TInstantCodeModule;
begin
  Result := TInstantCodeModule.Create(Self);
  try
    try
      Result.FileName := FileName;
      Result.LoadFromStream(Stream, Scope);
      Fixup;
    except
      on E: EInstantCodeError do
        begin
          E.FileName := FileName;
          raise;
        end
      else
        raise;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TInstantCodeProject.LoadModule(const Str, FileName: string;
  Scope: TInstantCodeScope): TInstantCodeModule;
var
  Stream: TInstantStringStream;
begin
  Stream := TInstantStringStream.Create(Str);
  try
    Result := LoadModule(Stream, FileName, Scope);
  finally
    Stream.Free;
  end;
end;

procedure TInstantCodeProject.RemoveType(AType: TInstantCodeType);
begin
  if AType is TInstantCodeClass then
    FClasses.Remove(AType);
  FTypes.Remove(AType);
end;

procedure TInstantCodeProject.SaveToResFile(const FileName: string);
begin
  Model.SaveToResFile(FileName);
end;

{ TInstantCodeModel }

procedure TInstantCodeModel.AddComponent(Component: TComponent);
begin
  ComponentList.Add(Component);
end;

procedure TInstantCodeModel.AssignComponents(Source: TInstantCodeModel);
var
  I: Integer;
begin
  if not Assigned(Source) then
    Exit;
  ComponentList.Clear;
  for I := 0 to Pred(Source.ComponentList.Count) do
    AddComponent(Source.ComponentList[I]);
end;

function TInstantCodeModel.GetComponentList: TList;
begin
  if not Assigned(FComponentList) then
    FComponentList := TList.Create;
  Result := FComponentList;
end;

procedure TInstantCodeModel.InternalChanged;
var
  I: Integer;
  Component: TComponent;
begin
  inherited;
  for I := 0 to Pred(ComponentList.Count) do
  begin
    Component := ComponentList[I];
    if Component is TDataSet then
      TDataSet(Component).FieldDefs.Updated := False;
  end;
end;

procedure TInstantCodeModel.RemoveComponent(Component: TComponent);
begin
  ComponentList.Remove(Component);
end;

{ TInstantCodeModifier }

function TInstantCodeModifier.AddAttribute(AClass: TInstantCodeClass;
  Template: TInstantCodeAttribute): TInstantCodeAttribute;
var
  MT: TInstantCodeContainerMethodType;
begin
  if not Assigned(Template) or not Assigned(Template.Owner) then
  begin
    Result := nil;
    Exit;
  end;
  AddField(AClass, Template.FindField);
  AddProperty(AClass, Template.FindValueProp);
  if Template.IsContainer then
  begin
    AddProperty(AClass, Template.FindCountProp);
    for MT := Low(MT) to High(MT) do
      AddMethod(AClass, Template.FindMethod(MT));
  end;
  Result := AClass.AddAttribute;
  try
    Result.Assign(Template);
  except
    Result.Free;
    raise;
  end;
end;

procedure TInstantCodeModifier.AddClass(AClass: TInstantCodeClass);

  procedure AddInterface;
  var
    Types: TInstantCodeTypes;
    UsesClause: TInstantCodeUsesClause;
    Prefix: string;
    Pos: TInstantCodePos;
  begin
    with Module.InterfaceSection do
    begin
      Types := FindTypes;
      if not Assigned(Types) then
      begin
        Prefix := CRLF + 'type' + CRLF;
        UsesClause := FindUsesClause;
        if Assigned(UsesClause) then
          Pos := UsesClause.EndPos else
          Pos := StartPos;
        InsertMode := imAfter;
      end else
      begin
        Prefix := CRLF;
        Pos := Types.EndPos;
        InsertMode := imBefore;
      end;
    end;
    CursorPos := Pos;
    SkipLine;
    InsertText(Prefix);
    InsertObjectText(AClass, 1);
    InsertNewLine;
  end;

  procedure AddImplementation;
  var
    AObject: TInstantCodeObject;
    Pos: TInstantCodePos;
  begin
    with Module.ImplementationSection do
    begin
      AObject := FindLast(TInstantCodeProcs);
      if not Assigned(AObject) then
        AObject := FindUsesClause;
      if Assigned(AObject) then
        Pos := AObject.EndPos
      else
        Pos := StartPos;
    end;
    InsertMode := imAfter;
    CursorPos := Pos;
    SkipLine;
    with AClass do
      InsertText(CRLF + ImplementationComment + CRLF +
        ImplementationString);
  end;

begin
  AddInterface;
  if AClass.MethodCount > 0 then
    AddImplementation;
  UpdateClassRegistration(nil, AClass);
  UpdateClassForward(nil, AClass);
  CheckBaseClassAddUsers(AClass);
end;

function TInstantCodeModifier.AddField(AClass: TInstantCodeClass;
  Template: TInstantCodeField): TInstantCodeField;
var
  Prior, Next: TObject;
begin
  if not Assigned(Template) then
  begin
    Result := nil;
    Exit;
  end;
  AClass.FindNearestFields(Template, Prior, Next);
  Result := AClass.AddField(Template.Visibility);
  try
    Result.Assign(Template);
    InsertMember(AClass, Result, Prior, Next);
  except
    Result.Free;
    raise;
  end;
end;

function TInstantCodeModifier.AddMethod(AClass: TInstantCodeClass;
  Template: TInstantCodeMethod): TInstantCodeMethod;
var
  Prior, Next: TObject;
  Pos: TInstantCodePos;
begin
  if not Assigned(Template) then
  begin
    Result := nil;
    Exit;
  end;
  AClass.FindNearestMethods(Template, Prior, Next);
  Result := AClass.AddMethod(Template.Visibility);
  try
    Result.Assign(Template);
    InsertMember(AClass, Result, Prior, Next);
  except
    Result.Free;
    raise;
  end;
  Result.Proc := AddProc(Template.Proc);
  if AClass.MethodCount = 1 then
  begin
    { Insert comment before first procedure }
    InsertMode := imBefore;
    CursorPos := Result.Proc.StartPos;
    Pos := CursorPos;
    InsertNewLine(2);
    CursorPos := Pos;
    InsertText(AClass.ImplementationComment);
  end;
end;

function TInstantCodeModifier.AddProc(
  Template: TInstantCodeProc): TInstantCodeProc;

  function DoInsert: TInstantCodeProc;
  begin
    Result := Module.ImplementationSection.AddProc;
    try
      Result.Assign(Template);
      InsertObjectText(Result);
    except
      Result.Free;
      raise;
    end;
  end;

  function InsertBefore(Next: TObject): TInstantCodeProc;
  var
    Pos: TInstantCodePos;
  begin
    InsertMode := imBefore;
    Pos := TInstantCodeObject(Next).StartPos;
    CursorPos := Pos;
    InsertNewLine(2);
    CursorPos := Pos;
    Result := DoInsert;
  end;

  function InsertAfter(Prior: TObject): TInstantCodeProc;
  begin
    InsertMode := imAfter;
    CursorPos := TInstantCodeObject(Prior).EndPos;
    SkipLine;
    InsertNewLine;
    Result := DoInsert;
    InsertNewLine(1);
  end;

  function Append: TInstantCodeProc;
  var
    Pos: TInstantCodePos;
  begin
    InsertMode := imBefore;
    with Module.ImplementationSection do
    begin
      Pos := EndPos;
      CursorPos := Pos;
      InsertNewLine(2);
      CursorPos := Pos;
      Result := DoInsert;
    end;
  end;

var
  Prior, Next: TObject;
begin
  if not Assigned(Template) then
  begin
    Result := nil;
    Exit;
  end;
  Module.ImplementationSection.FindNearestProcs(Template, Prior, Next);
  if Assigned(Next) then
    Result := InsertBefore(Next)
  else if Assigned(Prior) then
    Result := InsertAfter(Prior)
  else
    Result := Append;
end;

function TInstantCodeModifier.AddProperty(AClass: TInstantCodeClass;
  Template: TInstantCodeProperty): TInstantCodeProperty;
var
  Prior, Next: TObject;
begin
  if not Assigned(Template) then
  begin
    Result := nil;
    Exit;
  end;
  AClass.FindNearestProperties(Template, Prior, Next);
  Result := AClass.AddProperty(Template.Visibility);
  try
    Result.Assign(Template);
    InsertMember(AClass, Result, Prior, Next);
  except
    Result.Free;
    raise;
  end;
  Result.Getter := AddMethod(AClass, Template.Getter);
  Result.Setter := AddMethod(AClass, Template.Setter);
end;

procedure TInstantCodeModifier.AddUses(const AUnitNames: array of string; Scope: TInstantCodeScope);

  procedure AddUsesClause(ASection: TInstantCodeSection;
    AUsesClause: TInstantCodeUsesClause);
  var
    sStr: string;
    iNames: Integer;
  begin
    sStr := '';
    for iNames := Low(AUnitNames) to High(AUnitNames) do
    begin
      if AUnitNames[iNames] <> ''  then
        if sStr = '' then
          sStr := '  ' + AUnitNames[iNames]
        else
          sStr := sStr + ', ' + AUnitNames[iNames];
    end;

    if sStr <> '' then
    begin
      InsertMode := imAfter;
      CursorPos := ASection.StartPos;
      SkipLine;
      InsertText(CRLF + 'uses' + CRLF + sStr + ';' + CRLF);
    end;
  end;

  procedure UpdateUsesClause(ASection: TInstantCodeSection;
    AUsesClause: TInstantCodeUsesClause);
  var
    sStr: string;
    iNames: Integer;
  begin
    sStr := '';
    for iNames := Low(AUnitNames) to High(AUnitNames) do
      if (AUnitNames[iNames] <> '') and
          not Assigned(AUsesClause.Find(AUnitNames[iNames])) then
        sStr := sStr + ', ' + AUnitNames[iNames];

    if sStr <> '' then
    begin
      InsertMode := imBefore;
      CursorPos := AUsesClause.EndPos;
      MoveCursor(-1);
      InsertText(sStr, True);
    end;
  end;

var
  Section: TInstantCodeSection;
  UsesClause: TInstantCodeUsesClause;
begin
  if Length(AUnitNames) = 0 then
    Exit;

  if Scope = scInterface then
    Section := Module.InterfaceSection
  else
    Section := Module.ImplementationSection;

  UsesClause := Section.FindUsesClause;
  if Assigned(UsesClause) then
    UpdateUsesClause(Section, UsesClause)
  else
    AddUsesClause(Section, UsesClause);
end;

procedure TInstantCodeModifier.ChangeAttribute(AClass: TInstantCodeClass;
  Name: string; Template: TInstantCodeAttribute);
var
  Attribute: TInstantCodeAttribute;
begin
  Attribute := AClass.FindAttribute(Name);
  if not Assigned(Attribute) then
    Exit;
  Attribute.StorageName := Template.StorageName;
  ReplaceObjectText(Attribute);
end;

procedure TInstantCodeModifier.ChangeClass(
  ChangeInfo: TInstantCodeClassChangeInfo; NewClass: TInstantCodeClass);

  procedure DeleteAttributes(OldClass: TInstantCodeClass);
  var
    I: Integer;
    Attribute: TInstantCodeAttribute;
    List: TList;
  begin
    List := TList.Create;
    try
      for I := 0 to Pred(OldClass.AttributeCount) do
      begin
        Attribute := OldClass.Attributes[I];
        if ChangeInfo.IsDeletedAttribute(Attribute) then
          List.Add(Attribute);
      end;
      for I := 0 to Pred(List.Count) do
        DeleteAttribute(List[I])
    finally
      List.Free;
    end;
  end;

  procedure ChangeAttributes(OldClass, NewClass: TInstantCodeClass);
  var
    I: Integer;
    Attribute: TInstantCodeAttribute;
    OldName: string;
  begin
    for I := 0 to Pred(NewClass.AttributeCount) do
    begin
      Attribute := NewClass.Attributes[I];
      if ChangeInfo.IsChangedAttribute(Attribute, OldName) then
        ChangeAttribute(OldClass, OldName, Attribute);
    end;
  end;

  procedure AddAttributes(OldClass, NewClass: TInstantCodeClass);
  var
    I: Integer;
    Attribute: TInstantCodeAttribute;
  begin
    for I := 0 to Pred(NewClass.AttributeCount) do
    begin
      Attribute := NewClass.Attributes[I];
      if ChangeInfo.IsNewAttribute(Attribute) then
        AddAttribute(OldClass, Attribute);
    end;
  end;

var
  OldClass: TInstantCodeClass;
begin
  OldClass := Module.FindClass(ChangeInfo.OldName);
  if not Assigned(OldClass) then
    Exit;
  if NewClass.Name <> OldClass.Name then
    ChangeClassName(OldClass, NewClass.Name);
  if NewClass.BaseClassName <> OldClass.BaseClassName then
    ChangeClassBase(OldClass, NewClass.BaseClassName);
  DeleteAttributes(OldClass);
  ChangeAttributes(OldClass, NewClass);
  AddAttributes(OldClass, NewClass);
  ChangeClassMetadata(OldClass, NewClass);
  UpdateClassRegistration(OldClass, NewClass);
  UpdateClassForward(OldClass, NewClass);
  CheckBaseClassAddUsers(NewClass);
end;

procedure TInstantCodeModifier.ChangeClassBase(AClass: TInstantCodeClass;
  const NewBase: string);
var
  Pos: TInstantCodePos;
  Token: string;
begin
  CursorPos := AClass.StartPos;
  with FReader do
    while not Finished do
    begin
      Token := ReadToken;
      if SameText(Token, 'class') then
        Break;
      if Token = ';' then
        Exit;
    end;
  InsertMode := imAfter;
  Pos := CursorPos;
  Token := ReadToken;
  if Token = '(' then
  begin
    { Modify base }
    SkipSpace;
    Pos := CursorPos;
    Token := ReadToken;
    CursorPos := Pos;
    if Token = ')' then
      InsertText(NewBase)
    else
      ReplaceText(Length(Token), NewBase);
  end else
  begin
    { Insert base }
    CursorPos := Pos;
    InsertText('(' + NewBase + ')');
  end;
  AClass.BaseClassName := NewBase;
end;

procedure TInstantCodeModifier.ChangeClassMetadata(OldClass,
  NewClass: TInstantCodeClass);
begin
  EraseObject(OldClass.MetadataInfo);
  CloseGap;
  OldClass.MetadataInfo.Assign(NewClass.MetadataInfo);
  if OldClass.MetadataInfo.IsEmpty then
    Exit;
  CursorPos := OldClass.StartPos;
  InsertMode := imAfter;
  FindText(CRLF);
  InsertNewLine(OldClass.StartPos);
  InsertObjectText(OldClass.MetadataInfo, 1);
end;

procedure TInstantCodeModifier.ChangeClassName(AClass: TInstantCodeClass;
  const NewName: string);

  procedure ChangeSubClasses;
  var
    I: Integer;
    SubClass: TInstantCodeClass;
    List: TList;
  begin
    List := TList.Create;
    try
      for I := 0 to Pred(AClass.SubClassCount) do
        List.Add(AClass.SubClasses[I]);
      for I := 0 to Pred(List.Count) do
      begin
        SubClass := List[I];
        if SubClass.Module = AClass.Module then
          ChangeClassBase(SubClass, NewName);
      end;
    finally
      List.Free;
    end;
  end;

  procedure ChangeProcComment;
  var
    I: Integer;
    Pos: TInstantCodePos;
    Proc: TInstantCodeProc;
  begin
    Pos.Offset := 0;
    for I := 0 to Pred(AClass.MethodCount) do
    begin
      Proc := AClass.Methods[I].Proc;
      if Assigned(Proc) and
        ((Pos.Offset = 0) or (Proc.StartPos.Offset < Pos.Offset)) then
        Pos := Proc.StartPos;
    end;
    if Pos.Offset > 0 then
    begin
      CursorPos := Pos;
      if EnterComment(True) then
      begin
        SkipSpace;
        Pos := CursorPos;
        if SameText(ReadToken, AClass.Name) then
        begin
          CursorPos:= Pos;
          ReplaceText(Length(AClass.Name), NewName);
        end;
      end;
    end;
  end;

  procedure ChangeProcNames;
  var
    I, Line: Integer;
    Proc: TInstantCodeProc;
  begin
    for I := 0 to Pred(AClass.MethodCount) do
    begin
      Proc := AClass.Methods[I].Proc;
      if Assigned(Proc) then
      begin
        CursorPos := Proc.StartPos;
        Line := CursorPos.Line;
        if FindText(AClass.Name) and (CursorPos.Line = Line) then
          ReplaceText(Length(AClass.Name), NewName);
      end;
    end;
  end;

begin
  CursorPos := AClass.StartPos;
  InsertMode := imAfter;
  ReplaceText(Length(AClass.Name), NewName);
  ChangeProcComment;
  ChangeProcNames;
  AClass.Name := NewName;
  ChangeSubClasses;
end;

function TInstantCodeModifier.ClassDerivesFrom(AClass: TInstantCodeClass;
  const AClassName: string): Boolean;
var
  BaseClass: TInstantCodeClass;
begin
  Result := AClass.DerivesFrom(AClassName);
  if not Result and Assigned(FBaseProject) then
  begin
    BaseClass := FBaseProject.FindClass(AClass.BaseClassName);
    Result := Assigned(BaseClass) and BaseClass.DerivesFrom(AClassName);
  end;
end;

procedure TInstantCodeModifier.CloseGap;
var
  SavePos, StartPos, Pos: TInstantCodePos;
  Ch: Char;
begin
  with FReader do
  begin
    SavePos := Position;
    while not Bof do
    begin
      Pos := Position;
      Ch := BackChar;
      if (Ch = #10) then
      begin
        StartPos := Pos;
        Position := SavePos;
        while not Finished do
        begin
          Ch := ReadTextChar;
          if (Ch = #13) and (NextChar = #10) then
          begin
            ReadTextChar;

            DeleteFrom(StartPos);
            Exit;
          end else if not IsSpace(Ch) then
            Exit;
        end;
      end else if not IsSpace(Ch) then
      begin
        Position := SavePos;
        Exit;
      end;
    end;
  end;
end;

constructor TInstantCodeModifier.Create(var ACode: string;
  ABaseProject: TInstantCodeProject);
begin
  FCode := @ACode;
  FBaseProject := ABaseProject;
  FProject := TInstantCodeProject.Create;
  CreateModule;
  CreateReader;
end;

function TInstantCodeModifier.CreateClassList(
  OldClass, NewClass: TInstantCodeClass): TStringList;
var
  I: Integer;
  AClass: TInstantCodeClass;
begin
  Result := TStringList.Create;
  try
    if not Assigned(OldClass) and Assigned(NewClass) then
      Result.AddObject(NewClass.Name, NewClass);
    for I := 0 to Pred(Module.ClassCount) do
    begin
      AClass := Module.Classes[I];
      if ClassDerivesFrom(AClass, TInstantObject.ClassName) then
      begin
        if Assigned(OldClass) and (AClass.Name = OldClass.Name) then
        begin
          if Assigned(NewClass) then
            Result.AddObject(NewClass.Name, NewClass)
        end else
          Result.AddObject(AClass.Name, AClass);
      end;
    end;
    Result.Sort;
  except
    Result.Free;
    raise;
  end;
end;

procedure TInstantCodeModifier.CreateModule;
begin
  FModule := FProject.LoadModule(FCode^, '', scImplementation);
end;

procedure TInstantCodeModifier.CreateReader;
var
  Stream: TInstantStringStream;
begin
  Stream := TInstantStringStream.Create(FCode^);
  try
    FReader := TInstantCodeReader.Create(Stream, True);
  except
    Stream.Free;
    raise;
  end;
end;

procedure TInstantCodeModifier.DeleteAttribute(
  Attribute: TInstantCodeAttribute);
var
  MT: TInstantCodeContainerMethodType;
begin
  if not Assigned(Attribute) then
    Exit;
  with Attribute do
  begin
    DeleteField(FindField);
    DeleteProperty(FindValueProp);
    if IsContainer then
    begin
      DeleteProperty(FindCountProp);
      for MT := Low(MT) to High(MT) do
        DeleteMethod(FindMethod(MT));
    end;
  end;
end;

procedure TInstantCodeModifier.DeleteClass(AClass: TInstantCodeClass);

  procedure DeleteInterface(AClass: TInstantCodeClass);
  var
    Types: TInstantCodeTypes;
  begin
    Types := AClass.Owner as TInstantCodeTypes;
    if Types.ObjectCount = 1 then
      DeleteObject(Types)
    else
      DeleteObject(AClass);
    CloseGap;
    CloseGap;
  end;

  procedure RemoveProcComment;
  var
    StartPos, Pos: TInstantCodePos;
  begin
    if EnterComment(True) then
    begin
      Pos := CursorPos;
      MoveCursor(-1);
      StartPos := CursorPos;
      CursorPos := Pos;
      SkipSpace;
      if SameText(ReadToken, AClass.Name) then
      begin
        SkipSpace;
        if ReadToken = '}' then
        begin
          DeleteFrom(StartPos);
          CloseGap;
          CloseGap;
        end;
      end;
    end;
  end;

  procedure DeleteImplementation(AClass: TInstantCodeClass);
  var
    I: Integer;
    Method: TInstantCodeMethod;
  begin
    for I := 0 to Pred(AClass.MethodCount) do
    begin
      Method := AClass.Methods[I];
      DeleteProc(Method.Proc);
      RemoveProcComment;
    end;
  end;

  procedure UpdateSubclassesBaseClass(AClass: TInstantCodeClass);
  var
    I: Integer;
  begin
    for I := 0 to Pred(AClass.SubClassCount) do
    begin
      if AClass.BaseClassName <> '' then
        ChangeClassBase(AClass.SubClasses[I], AClass.BaseClassName)
      else
        ChangeClassBase(AClass.SubClasses[I], 'TInstantObject');
    end;
  end;

var
  OldClass: TInstantCodeClass;
begin
  if not Assigned(AClass) then
    Exit;
  OldClass := Module.FindClass(AClass.Name);
  if not Assigned(OldClass) then
    Exit;
  UpdateSubclassesBaseClass(OldClass);
  UpdateClassRegistration(OldClass, nil);
  UpdateClassForward(OldClass, nil);
  DeleteImplementation(OldClass);
  DeleteInterface(OldClass);
end;

procedure TInstantCodeModifier.DeleteField(Field: TInstantCodeField);
begin
  DeleteMember(Field);
end;

procedure TInstantCodeModifier.DeleteFrom(Pos: TInstantCodePos);
var
  EndPos: TInstantCodePos;
begin
  EndPos := CursorPos;
  CursorPos := Pos;
  DeleteTo(EndPos);
end;

procedure TInstantCodeModifier.DeleteMember(Member: TInstantCodeMember);
var
  Members: TInstantCodeMembers;
begin
  if not Assigned(Member) then
    Exit;
  Members := Member.Owner;
  DeleteObject(Member);
  CloseGap;
  if Members.Count = 0 then
  begin
    DeleteObject(Members);
    CloseGap;
  end;
end;

procedure TInstantCodeModifier.DeleteMethod(Method: TInstantCodeMethod);
var
  StartPos, EndPos: TInstantCodePos;
begin
  if not Assigned(Method) then
    Exit;
  DeleteProc(Method.Proc);
  if (Method.HostClass.MethodCount = 1) and EnterComment(True) then
    with FReader do
    begin
      StartPos := Position;
      while not Finished do
        if ReadChar = '}' then
        begin
          EndPos := Position;
          Position := StartPos;
          if BackChar = '{' then
          begin
            DeleteTo(EndPos);
            CloseGap;
            CloseGap;
            Break;
          end;
        end;
    end;
  DeleteMember(Method);
end;

procedure TInstantCodeModifier.DeleteObject(AObject: TInstantCodeObject);
begin
  EraseObject(AObject);
  AObject.Free;
end;

procedure TInstantCodeModifier.DeleteProc(Proc: TInstantCodeProc);
begin
  if not Assigned(Proc) then
    Exit;
  DeleteObject(Proc);
  CloseGap;
  CloseGap;
end;

procedure TInstantCodeModifier.DeleteProperty(Prop: TInstantCodeProperty);
begin
  if not Assigned(Prop) then
    Exit;
  DeleteMethod(Prop.Getter);
  DeleteMethod(Prop.Setter);
  DeleteMember(Prop);
end;

procedure TInstantCodeModifier.DeleteText(Len: Integer);
begin
  DoDelete(Len);
  UpdateReader;
end;

procedure TInstantCodeModifier.DeleteTo(Pos: TInstantCodePos);
begin
  DeleteText(Pos.Offset - CursorPos.Offset);
end;

destructor TInstantCodeModifier.Destroy;
begin
  FProject.Free;
  FReader.Free;
  inherited;
end;

procedure TInstantCodeModifier.AdjustForUnitNamesInInterface(var AUnitNames:
    array of string);
var
  Section: TInstantCodeSection;
  UsesClause: TInstantCodeUsesClause;
  iNames: Integer;
begin
  if Length(AUnitNames) = 0 then
    Exit;

  Section := Module.InterfaceSection;
  UsesClause := Section.FindUsesClause;
  if Assigned(UsesClause) then
  begin
    { check the uses clause and remove names of found units from array }
    for iNames := Low(AUnitNames) to High(AUnitNames) do
      if (AUnitNames[iNames] <> '') and
          Assigned(UsesClause.Find(AUnitNames[iNames])) then
        AUnitNames[iNames] := '';
  end;
end;

procedure TInstantCodeModifier.RemoveUses(const AUnitNames: array of string;
    Scope: TInstantCodeScope = scInterface);

  function EraseTrailingUsesItem(const AUsesItem: TInstantCodeUses;
    const AUsesClause: TInstantCodeUsesClause): Boolean;
  var
    SavePos: TInstantCodePos;
    Ch: Char;
    Pos: TInstantCodePos;
  begin
    Result := False;
    if not Assigned(AUsesItem) or not Assigned(AUsesClause) then
      Exit;

    with FReader do
    begin
      SavePos := Position;
      Position := AUsesItem.StartPos;
      Pos := Position;
      Ch := BackChar;
      while (Position.Offset > AUsesClause.StartPos.Offset) and
          not IsText(Ch) do
      begin
        if (Ch = ',') then
        begin
          Pos := Position;
          Break;
        end;
        Ch := BackChar;
      end;
      Result := not (Pos.Offset = AUsesItem.StartPos.Offset);
      if Result then
        DeleteTo(AUsesItem.EndPos)
      else
        Position := SavePos;
    end;
  end;

  function EraseFirstUsesItem(const AUsesItem: TInstantCodeUses;
    const AUsesClause: TInstantCodeUsesClause): Boolean;
  var
    SavePos: TInstantCodePos;
    Ch: Char;
    Pos: TInstantCodePos;
  begin
    Result := False;
    if not Assigned(AUsesItem) or not Assigned(AUsesClause) then
      Exit;

    with FReader do
    begin
      SavePos := Position;
      Position := AUsesItem.EndPos;
      Pos := Position;
      Ch := ReadChar;
      while (Position.Offset <= AUsesClause.EndPos.Offset) and
          not IsText(Ch) do
      begin
        if (Ch = ',') then
        begin
          SkipSpace;
          Pos := Position;
          Break;
        end;
        Ch := ReadChar;
      end;
      Result := not (Pos.Offset = AUsesItem.EndPos.Offset);
      if Result then
        DeleteFrom(AUsesItem.StartPos)
      else
        Position := SavePos;
    end;
  end;

  procedure EraseUsesItem(const AUsesItem: TInstantCodeUses;
    AUsesClause: TInstantCodeUsesClause);
  begin
    if not EraseTrailingUsesItem(AUsesItem, AUsesClause) then
      EraseFirstUsesItem(AUsesItem, AUsesClause);
  end;

  procedure EraseUsesClause(AUsesClause: TInstantCodeUsesClause);
  begin
    EraseObject(AUsesClause);
    CloseGap;
    CloseGap;
  end;

var
  Section: TInstantCodeSection;
  UsesClause: TInstantCodeUsesClause;
  iNames: Integer;
  UsesItem: TInstantCodeUses;
begin
  if Length(AUnitNames) = 0 then
    Exit;

  if Scope = scInterface then
    Section := Module.InterfaceSection
  else
    Section := Module.ImplementationSection;

  UsesClause := Section.FindUsesClause;
  if not Assigned(UsesClause) then
    Exit;

  for iNames := Low(AUnitNames) to High(AUnitNames) do
    if (AUnitNames[iNames] <> '') then
    begin
      UsesItem := UsesClause.Find(AUnitNames[iNames]);
      if Assigned(UsesItem) then
      begin
        if UsesClause.ObjectCount > 1 then
          EraseUsesItem(UsesItem, UsesClause)
        else
        begin
          EraseUsesClause(UsesClause);
          Break;
        end;
      end;
    end;
end;

procedure TInstantCodeModifier.CheckBaseClassAddUsers(AClass:
    TInstantCodeClass);
begin
  if Assigned(AClass.BaseClass) and
      (AClass.Module.Name <> AClass.BaseClass.Module.Name) then
    AddUses([AClass.BaseClass.Module.Name]);
end;

procedure TInstantCodeModifier.DestroyReader;
begin
  FreeAndNil(FReader);
end;

procedure TInstantCodeModifier.DoDelete(Len: Integer);
var
  Delta: TInstantCodePos;
begin
  if Len = 0 then
    Exit;
  System.Delete(FCode^, CursorPos.Offset + 1, Len);
  Delta.Line := 0;
  Delta.Column := 0;
  Delta.Offset := -Len;
  UpdateModule(CursorPos, Delta, InsertMode = imBefore);
end;

procedure TInstantCodeModifier.DoInsert(const Str: string; Wrap: Boolean);
var
  S, IndentText: string;
  I, MaxCol: Integer;
  Delta, NewPos: TInstantCodePos;
begin
  if Wrap then
  begin
    MaxCol := 80 - CursorPos.Column;
    IndentText := ReadIndentText;
    S := WrapText(Str, CRLF + IndentText, [' ', ',', #9], MaxCol)
  end else
    S := Str;
  System.Insert(S, FCode^, CursorPos.Offset + 1);
  NewPos := CursorPos;
  for I := 1 to Length(S) do
    AdvanceTextPos(NewPos, S[I]);
  with Delta do
  begin
    Line := 0;
    Column := 0;
    Offset := NewPos.Offset - CursorPos.Offset;
  end;
  UpdateModule(CursorPos, Delta, InsertMode = imBefore);
  UpdateReader;
  CursorPos := NewPos;
end;

function TInstantCodeModifier.EnterComment(Back: Boolean): Boolean;
var
  SavePos, Pos: TInstantCodePos;
  Ch: Char;
begin
  if not Back then
  begin
    Result := FReader.EnterComment;
    Exit;
  end;
  with FReader do
  begin
    SavePos := Position;
    repeat
      Ch := BackChar;
    until not IsSpace(Ch);
    if Ch = '}' then
    begin
      repeat
        Pos := Position;
        Ch := BackChar;
        if Ch = '{' then
        begin
          Position := Pos;
          Result := True;
          Exit;
        end;
      until Bof;
    end;
    Position := SavePos;
    Result := False;
  end;
end;

procedure TInstantCodeModifier.EraseObject(AObject: TInstantCodeObject);
begin
  if not Assigned(AObject) or not AObject.IsFiled then
    Exit;
  CursorPos := AObject.StartPos;
  DeleteTo(AObject.EndPos);
end;

function TInstantCodeModifier.FindText(Text: string; Back: Boolean): Boolean;
var
  SavePos: TInstantCodePos;
  I: Integer;
  Ch: Char;
begin
  if not Back then
    Result := FReader.Find(Text)
  else
    with FReader do
    begin
      I := Length(Text);
      while not Bof do
      begin
        SavePos := Position;
        Ch := BackChar;
        if LowerCase(Ch) = LowerCase(Text[I]) then
        begin
          Dec(I);
          if I = 0 then
          begin
            Result := True;
            Exit;
          end;
        end else
          I := Length(Text);
      end;
      Position := SavePos;
      Result := False;
    end;
end;

function TInstantCodeModifier.GetCursorPos: TInstantCodePos;
begin
  Result := FReader.Position;
end;

function TInstantCodeModifier.GetIndentText(Pos: TInstantCodePos): string;
begin
  if (Pos.Column < 1) or (Pos.Column > 80) then
    Result := ''
  else
    Result := StringOfChar(' ', Pos.Column - 1);
end;

procedure TInstantCodeModifier.InsertMember(AClass: TInstantCodeClass;
  Member: TInstantCodeMember; Prior, Next: TObject);

  procedure DoInsert;
  begin
    InsertObjectText(Member, 2);
  end;

  procedure InsertBefore(AObject: TObject);
  var
    Pos: TInstantCodePos;
  begin
    with TInstantCodeObject(AObject) do
      if IsFiled then
        Pos := StartPos
      else
        Exit;
    InsertMode := imBefore;
    CursorPos := Pos;
    InsertNewLine(CursorPos);
    CursorPos := Pos;
    DoInsert;
  end;

  procedure InsertAfter(AObject: TObject);
  var
    IndentPos: TInstantCodePos;
  begin
    with TInstantCodeObject(AObject) do
    begin
      if not IsFiled then
        Exit;
      IndentPos := StartPos;
      CursorPos := EndPos;
    end;
    InsertMode := imAfter;
    InsertNewLine(IndentPos);
    DoInsert;
  end;

  procedure Append;
  var
    Division: TInstantCodeDivision;
    Prior, Next: TObject;
    Pos: TInstantCodePos;
  begin
    if not (Member.Owner is TInstantCodeDivision) then
      Exit;
    Division := TInstantCodeDivision(Member.Owner);
    if Division.IsFiled then
      Exit;
    AClass.FindNearestDivisions(Division, Prior, Next);
    if Assigned(Next) then
    begin
      InsertMode := imBefore;
      Pos := TInstantCodeObject(Next).StartPos;
      CursorPos := Pos;
      InsertNewLine(AClass.StartPos);
      CursorPos := Pos;
      InsertObjectText(Division, 1);
    end else if Assigned(Prior) then
    begin
      InsertMode := imAfter;
      CursorPos := TInstantCodeObject(Prior).EndPos;
      InsertNewLine(AClass.StartPos);
      InsertObjectText(Division, 1);
    end else
    begin
      InsertMode := imAfter;
      CursorPos := AClass.EndPos;
      FindText('end', True);
      InsertObjectText(Division, 1);
      InsertNewLine(AClass.StartPos);
    end;
  end;

  procedure InsertBestFit;
  var
    Prior, Next: TObject;
  begin
    AClass.FindNearestMembers(nil, Member, [Member.Visibility], Prior, Next);
    if Assigned(Next) then
      InsertBefore(Next)
    else if Assigned(Prior) then
      InsertAfter(Prior)
    else
      Append;
  end;

begin
  if Assigned(Next) then
    InsertBefore(Next)
  else if Assigned(Prior) then
    InsertAfter(Prior)
  else
    InsertBestFit;
end;

procedure TInstantCodeModifier.InsertNewLine(IndentPos: TInstantCodePos);
begin
  InsertText(CRLF + GetIndentText(IndentPos));
end;

procedure TInstantCodeModifier.InsertNewLine(Count: Integer);
begin
  while Count > 0 do
  begin
    InsertText(CRLF);
    Dec(Count);
  end;
end;

procedure TInstantCodeModifier.InsertObjectText(AObject: TInstantCodeObject;
  IndentLevel: Integer);
var
  Stream: TInstantStringStream;
  Writer: TInstantCodeWriter;
begin
  if not Assigned(AObject) then
    Exit;
  Stream := TInstantStringStream.Create('');
  try
    Writer := TInstantCodeWriter.Create(Stream);
    try
      Writer.Origin := CursorPos;
      while IndentLevel > 0 do
      begin
        Writer.Indent;
        Dec(IndentLevel);
      end;
      AObject.Write(Writer);
      AObject.Lock;
      try
        InsertText(Stream.DataString);
      finally
        AObject.Unlock;
      end;
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TInstantCodeModifier.InsertText(const Str: string; Wrap: Boolean);
begin
  DoInsert(Str, Wrap);
end;

procedure TInstantCodeModifier.MoveCursor(Count: Integer);
begin
  if Count > 0 then
    repeat
      FReader.ReadTextChar;
      Dec(Count);
    until Count = 0
  else if Count < 0 then
    repeat
      FReader.BackChar;
      Inc(Count);
    until Count = 0;
end;

function TInstantCodeModifier.NextChar: Char;
begin
  Result := FReader.NextChar;
end;

function TInstantCodeModifier.ReadIndentText: string;
var
  I: Integer;
begin
  Result := '';
  I := CursorPos.Offset;
  while I >= 0 do
  begin
    if FCode^[I] = #13 then
    begin
      Inc(I);
      if FCode^[I] = #10 then
      begin
        Inc(I);
        while InstantCharInSet(FCode^[I], [' ', #9]) do
        begin
          Result := Result + FCode^[I];
          Inc(I);
        end;
      end;
      Break;
    end;
    Dec(I);
  end;
end;

function TInstantCodeModifier.ReadToken: string;
begin
  Result := FReader.ReadToken;
end;

procedure TInstantCodeModifier.ReplaceObjectText(
  AObject: TInstantCodeObject);
begin
  if not Assigned(AObject) or not AObject.IsFiled then
    Exit;
  CursorPos := AObject.StartPos;
  DeleteTo(AObject.EndPos);
  InsertObjectText(AObject);
end;

procedure TInstantCodeModifier.ReplaceText(Len: Integer; const Str: string);
begin
  DoDelete(Len);
  DoInsert(Str, False);
end;

procedure TInstantCodeModifier.SetCursorPos(const Value: TInstantCodePos);
begin
  FReader.Position := Value;
end;

procedure TInstantCodeModifier.Skip(Str: string);
begin
  FReader.ReadNext(Str);
end;

procedure TInstantCodeModifier.SkipLine;
begin
  Skip(CRLF);
end;

procedure TInstantCodeModifier.SkipSpace;
begin
  FReader.SkipSpace;
end;

procedure TInstantCodeModifier.SkipText;
begin
  with FReader do
    while not Finished and not IsSpace(NextChar) do
      ReadTextChar;
end;

procedure TInstantCodeModifier.UpdateClassForward(OldClass,
  NewClass: TInstantCodeClass);

  procedure RemoveForwards(Types: TInstantCodeTypes);
  var
    I: Integer;
    T: TInstantCodeType;
    ClassList: TStringList;
  begin
    ClassList := CreateClassList(nil, nil);
    try
      I := 0;
      while I < Types.Count do
      begin
        T := Types[I];
        if (T is TInstantCodeClassForward) and
          (ClassList.IndexOf(T.Name) <> -1) then
        begin
          DeleteObject(T);
          CloseGap;
          CloseGap;
        end else
          Inc(I);
      end;
    finally
      ClassList.Free;
    end;
  end;

  procedure InsertForwards(Types: TInstantCodeTypes);
  var
    I: Integer;
    ClassList: TStringList;
    ClassForward: TInstantCodeClassForward;
  begin
    ClassList := CreateClassList(OldClass, NewClass);
    try
      if ClassList.Count > 1 then
      begin
        CursorPos := Types.StartPos;
        SkipLine;
        InsertMode := imAfter;
        for I := 0 to Pred(ClassList.Count) do
        begin
          ClassForward := TInstantCodeClassForward.Create(nil);
          ClassForward.Name := ClassList[I];
          Types.InsertObject(I, ClassForward);
          InsertObjectText(ClassForward, 1);
          InsertNewLine;
        end;
        InsertNewLine;
      end;
    finally
      ClassList.Free;
    end;
  end;

var
  Section: TInstantCodeSection;
  Types: TInstantCodeTypes;
begin
  Section := Module.InterfaceSection;
  Types := Section.FindTypes;
  if Assigned(Types) then
  begin
    RemoveForwards(Types);
    InsertForwards(Types);
  end;
end;

procedure TInstantCodeModifier.UpdateClassRegistration(
  OldClass, NewClass: TInstantCodeClass);
const
  RegisterProcName = 'InstantRegisterClasses';

  function RegistrationText: string;
  var
    I: Integer;
    Stream: TInstantStringStream;
    Writer: TInstantCodeWriter;
    ClassList: TStringList;
  begin
    ClassList := CreateClassList(OldClass, NewClass);
    try
      if ClassList.Count > 0 then
      begin
        Stream := TInstantStringStream.Create('');
        Writer := TInstantCodeWriter.Create(Stream);
        try
          Writer.Indent;
          Writer.Write(RegisterProcName + '([' + CRLF);
          Writer.Indent;
          for I := 0 to Pred(ClassList.Count) do
          begin
            if I > 0 then
              Writer.Write(',' + CRLF);
            Writer.Write(ClassList[I]);
          end;
          Writer.Unindent;
          Writer.Write(CRLF + ']);' + CRLF);
          Result := Stream.DataString;
        finally
          Writer.Free;
          Stream.Free;
        end;
      end else
        Result := '';
    finally
      ClassList.Free;
    end;
  end;

var
  Section: TInstantCodeInitializationSection;
  Pos: TInstantCodePos;
begin
  Section := Module.InitializationSection;
  if not Section.IsFiled then
  begin
    CursorPos := Module.ImplementationSection.EndPos;
    InsertMode := imAfter;
    InsertObjectText(Section);
  end;
  CursorPos := Section.StartPos;
  SkipLine;
  if FindText(RegisterProcName) then
  begin
    Pos := CursorPos;
    Skip(';');
    DeleteFrom(Pos);
    CloseGap;
  end;
  InsertText(RegistrationText);
end;

procedure TInstantCodeModifier.UpdateModule(Origin, Delta: TInstantCodePos;
  Before: Boolean);
begin
  FModule.Move(Origin, Delta, Before);
end;

procedure TInstantCodeModifier.UpdateReader;
var
  SavePos: TInstantCodePos;
begin
  SavePos := CursorPos;
  try
    DestroyReader;
    CreateReader;
  finally
    CursorPos := SavePos;
  end;
end;

procedure TInstantCodeModifier.UpdateUnit;
begin
  UpdateClassForward(nil, nil);
  UpdateClassRegistration(nil, nil);
end;

{ TPartTypeProcessor }

procedure TPartTypeProcessor.InternalRead;
var
  Token: string;
begin
  inherited;
  with FReader do
  begin
    while not Finished do
    begin
      SkipSpace;
      if NextChar = ';' then
        Break;
      Token := ReadToken;
      if SameText(Token, MetaKeyExternal) then
      begin
        FMetadata.StorageKind := skExternal;
        FMetadata.ExternalStorageName := '';
        FMetadata.ForeignKeyFields := '';
      end
      else if SameText(Token, MetaKeyVirtual) then
      begin
        FMetadata.StorageKind := skVirtual;
        FMetadata.ExternalStorageName := '';
        FMetadata.ForeignKeyFields := '';
      end
      else if SameText(Token, MetaKeyStored) then
      begin
        FMetadata.ExternalStorageName := '';
        FMetadata.ForeignKeyFields := '';
        FMetadata.StorageName := ReadStringValue;
      end;
    end;
  end;
end;

{ TEnumTypeProcessor }

procedure TEnumTypeProcessor.HandleArgument(Index: Integer;
  const Argument: string);
//var
//  CodeObject: TInstantCodeObject;
begin
  case Index of
    1:
      if IsValidIdent(Argument) then
      begin
// Code disabled: This checks if the Enumerated type exists. But it
//                only works if the type exists in the same unit as the
//                model.
//        Assert(FModule = nil, 'xxx');
//        if FModule <> nil then
//        begin
//          CodeObject := FModule.InterfaceSection.FindTypes.Find(Argument);
//          CodeObject := FModule.FindType(Argument);

//          if not Assigned(CodeObject)  then
//            Error(Format('Identifier not found: %s', [Argument])) else
//          if CodeObject is TInstantCodeEnum then
//          begin
            FMetadata.EnumName := Argument;
//            FMetadata.EnumValues := (CodeObject as TInstantCodeEnum).FItems;
//          end else
//            Error(Format('Enumerated type expected: %s', [Argument]));
//        end else
//          Error(Format('Invalid identifier name: %s', [Argument]));
      end
  else
    inherited;
  end;
end;

procedure TEnumTypeProcessor.InternalRead;
begin
  inherited;

end;

{ TObjectTypeProcessor }

procedure TObjectTypeProcessor.InternalRead;
var
  Token: string;
begin
  inherited;
  with FReader do
  begin
    while not Finished do
    begin
      SkipSpace;
      if NextChar = ';' then
        Break;
      Token := ReadToken;
      if SameText(Token, MetaKeyStored) then
        FMetadata.StorageName := ReadStringValue;
      if SameText(Token, MetaKeyExternal) then
        FMetadata.StorageKind := skExternal;
      if SameText(Token, MetaKeyForeignKey) then
      begin
        FMetadata.StorageKind := skForeignKeys;
        FMetadata.ForeignKeyFields := ReadStringValue;
      end;
    end;
  end;
end;

initialization
  CreateTypeProcessors;

finalization
  DestroyTypeProcessors;

end.
