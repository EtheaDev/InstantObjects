(*
 *   InstantObjects
 *   Presentation Classes
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
 * Carlo Barazzetta, Andrea Petrelli, Nando Dessena, Joao Morais
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantPresentation;

interface

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

uses
  Classes, DB, InstantPersistence, SysUtils, TypInfo, InstantCode, InstantUtils;

type
  TInstantAddClassFieldDefEvent = procedure (const FieldName : string; var BreakProcess : boolean) of object;
  TInstantChangeType = (ctAppearance, ctData);
  TInstantAccessMode = (amObject, amContent);

  TInstantChangeEvent = procedure(Sender: TObject; ChangeType: TInstantChangeType) of object;
  TInstantCompareObjectsEvent = procedure(Sender: TObject; AObject1, AObject2: TObject; var Compare: Integer) of object;
  TInstantLimitObjectsEvent = procedure(Sender: TObject; AObject: TObject; var Accept: Boolean) of object;

  TInstantAccessorClass = class of TInstantAccessor;

  TInstantAccessor = class(TPersistent)
  private
    FCachedClass: TClass;
    FChangesDisabledCount: Integer;
    FContainerName: string;
    FLimited: Boolean;
    FMode: TInstantAccessMode;
    FObjectClass: TClass;
    FObjectClassName: string;
    FSorted: Boolean;
    FSubject: TObject;
    FView: TList;
    FOnChange: TInstantChangeEvent;
    FOnCompare: TInstantCompareObjectsEvent;
    FOnLimit: TInstantLimitObjectsEvent;
    FOnProgress: TInstantProgressEvent;
    procedure DestroyView;
    function GetAltered: Boolean;
    function GetHasSubject: Boolean;
    function GetInContent: Boolean;
    function GetObjectClass: TClass;
    function GetObjectClassName: string;
    function GetObjectCount: Integer;
    function GetObjects(Index: Integer): TObject;
    function GetSubject: TObject;
    function GetTotalCount: Integer;
    function GetView: TList;
    procedure SetContainerName(const Value: string);
    procedure SetLimited(Value: Boolean);
    procedure SetMode(Value: TInstantAccessMode);
    procedure SetObjectClass(Value: TClass);
    procedure SetObjectClassName(const Value: string);
    procedure SetOnCompare(Value: TInstantCompareObjectsEvent);
    procedure SetOnLimit(Value: TInstantLimitObjectsEvent);
    procedure SetOnProgress(const Value: TInstantProgressEvent);
    procedure SetSorted(Value: Boolean);
    property View: TList read GetView;
  protected
    function AddToView(AObject: TObject): Integer;
    function InsertInView(Index: Integer; AObject: TObject): Integer;
    function RemoveFromView(AObject: TObject): Integer;
    procedure Changed(ChangeType: TInstantChangeType); virtual;
    procedure DoLimit(AObject: TObject; var Accept: Boolean);
    procedure DoProgress(Sender: TObject; Count: Integer; var Continue: Boolean);
    function GetConnector: TInstantConnector; virtual;
    function GetMode: TInstantAccessMode; virtual;
    function IncludeObject(AObject: TObject): Boolean;
    function InternalAddObject(AObject: TObject): Integer; virtual;
    procedure InternalApplyChanges; virtual;
    procedure InternalClear; virtual;
    function InternalCreateObject: TObject; virtual;
    function InternalGetObjectClass: TClass; virtual;
    function InternalGetObjectClassName: string; virtual;
    function InternalGetObjectCount: Integer; virtual;
    function InternalGetObjects(Index: Integer): TObject; virtual;
    function InternalIndexOfInstance(Instance: Pointer): Integer; virtual;
    function InternalIndexOfObject(AObject: TObject): Integer; virtual;
    function InternalInsertObject(Index: Integer; AObject: TObject): Integer; virtual;
    procedure InternalRefreshObjects; virtual;
    procedure InternalReleaseObject(AObject: TObject); virtual;
    function InternalRemoveObject(AObject: TObject): Integer; virtual;
    property HasSubject: Boolean read GetHasSubject;
    property InternalObjects[Index: Integer]: TObject read InternalGetObjects;
    property InternalObjectCount: Integer read InternalGetObjectCount;
  public
    constructor Create(ASubject: TObject); virtual;
    destructor Destroy; override;
    function AddObject(AObject: TObject): Integer;
    procedure ApplyChanges;
    procedure ChangedAppearance;
    procedure ChangedClass;
    procedure ChangedData;
    function ChangesDisabled: Boolean;
    procedure Clear;
    function CreateObject: TObject;
    procedure DisableChanges;
    procedure EnableChanges;
    function FindObject(const KeyFields: string; const KeyValues: Variant;
      Options: TInstantCompareOptions): Integer; virtual;
    function IndexOfInstance(Instance: Pointer): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    function InsertObject(Index: Integer; AObject: TObject): Integer;
    procedure Refresh;
    procedure RefreshObjects;
    procedure RefreshView;
    procedure ReleaseObject(AObject: TObject);
    function RemoveObject(AObject: TObject): Integer;
    function RepositionObject(AObject: TObject): Integer;
    procedure Reset;
    class function SubjectClass: TClass; virtual;
    property Altered: Boolean read GetAltered;
    property Connector: TInstantConnector read GetConnector;
    property ContainerName: string read FContainerName write SetContainerName;
    property InContent: Boolean read GetInContent;
    property Limited: Boolean read FLimited write SetLimited;
    property Mode: TInstantAccessMode read GetMode write SetMode;
    property ObjectCount: Integer read GetObjectCount;
    property ObjectClass: TClass read GetObjectClass write SetObjectClass;
    property ObjectClassName: string read GetObjectClassName write SetObjectClassName;
    property Objects[Index: Integer]: TObject read GetObjects;
    property Sorted: Boolean read FSorted write SetSorted;
    property Subject: TObject read GetSubject;
    property TotalCount: Integer read GetTotalCount;
    property OnChange: TInstantChangeEvent read FOnChange write FOnChange;
    property OnCompare: TInstantCompareObjectsEvent read FOnCompare write SetOnCompare;
    property OnLimit: TInstantLimitObjectsEvent read FOnLimit write SetOnLimit;
    property OnProgress: TInstantProgressEvent read FOnProgress write SetOnProgress;
  end;

  TInstantFieldOption = (foObjects, foThorough, foRecurseNesting);
  TInstantFieldOptions = set of TInstantFieldOption;

  PRecInfo = ^TRecInfo;
  TRecInfo = record
    RecordNumber: Longint;
    UpdateStatus: TUpdateStatus;
    BookmarkFlag: TBookmarkFlag;
  end;

  TInstantFieldEvent = procedure(Sender: TObject; Field: TField) of object;
  TInstantCreateObjectEvent = procedure(Sender: TObject; var AObject: TObject) of object;
  TInstantFieldErrorEvent = procedure(Sender: TObject; E: Exception; Field: TField; Value: Variant; Write: Boolean; var Handled: Boolean) of object;
  TInstantIncludeFieldEvent = procedure(Sender: TObject; const FieldName: string; var Include: Boolean) of object;
  TInstantFieldDefEvent = procedure(Sender: TObject; FieldDef: TFieldDef) of object;
  TInstantFieldTranslateEvent = procedure(Sender: TObject; Field: TField; var Value: Variant; Write: Boolean) of object;

  TInstantBookmark = record
    RecNo: Integer;
    Instance: TObject;
  end;

  TInstantExposerOption = (eoAutoApply, eoDeferInsert, eoSyncEdit);
  TInstantExposerOptions = set of TInstantExposerOption;

  TInstantCustomExposer = class(TDataSet)
  private
    FAccessor: TInstantAccessor;
    FBookmarkOfs, FRecInfoOfs: Word;
    FContainerName: string;
    FFieldOptions: TInstantFieldOptions;
    FFilterBuffer: PChar;
    FIsOpen: Boolean;
    FLimited: Boolean;
    FMode: TInstantAccessMode;
    FNewObject: TObject;
    FNotifier: TInstantObjectNotifier;
    FObjectClass: TClass;
    FObjectClassName: string;
    FOptions: TInstantExposerOptions;
    FReadOnly: Boolean;
    FRecordSize, FRecBufSize: Word;
    FRecNo: Integer;
    FRemovedObject: TObject;
    FRevertBuffer: PChar;
    FSorted: Boolean;
    FUndoBuffer: PChar;
    FAfterPostField: TInstantFieldEvent;
    FBeforePostField: TInstantFieldEvent;
    FOnCompare: TInstantCompareObjectsEvent;
    FOnCreateObject: TInstantCreateObjectEvent;
    FOnFieldError: TInstantFieldErrorEvent;
    FOnIncludeField: TInstantIncludeFieldEvent;
    FOnInitField: TInstantFieldEvent;
    FOnInitFieldDef: TInstantFieldDefEvent;
    FOnLimit: TInstantLimitObjectsEvent;
    FOnProgress: TInstantProgressEvent;
    FOnTranslate: TInstantFieldTranslateEvent;
    FOnAddClassFieldDef: TInstantAddClassFieldDefEvent;
    procedure AccessorChanged(Sender: TObject; ChangeType: TInstantChangeType);
    procedure CheckClass(AObject: TObject);
    procedure ClearData(Buffer: PChar);
    procedure ClearRecord(Buffer: PChar);
    function DataFieldsSize: Integer;
    function GetCurrentBuffer: PChar;
    function GetDesignClass: TInstantCodeClass;
    function GetHasCurrentBuffer: Boolean;
    function GetHasSubject: Boolean;
    function GetInContent: Boolean;
    function GetLimited: Boolean;
    function GetMode: TInstantAccessMode;
    function GetObjectClass: TClass;
    function GetObjectClassName: string;
    function GetObjectCount: Integer;
    function GetObjects(Index: Integer): TObject;
    function GetRecInfo(Buffer: PChar): PRecInfo;
    function GetSorted: Boolean;
    function GetTotalCount: Integer;
    function GetOnCompare: TInstantCompareObjectsEvent;
    function GetOnLimit: TInstantLimitObjectsEvent;
    function GetOnProgress: TInstantProgressEvent;
    procedure GotoActiveRecord;
    procedure GotoRecord(ARecNo: Integer);
    function HasObjectClassName: Boolean;
    procedure InitBufferPointers;
    procedure InitFields;
    function IsSelfField(Field: TField): Boolean;
    procedure LoadCurrentObject(Buffer: PChar);
    procedure ObjectChanged(AObject: TInstantObject);
    procedure PutObject(Buffer: PChar; AObject: TObject; Append: Boolean);
    procedure SaveCurrentObject(Buffer: PChar);
    procedure SetContainerName(const Value: string);
    procedure SetFieldOptions(Value: TInstantFieldOptions);
    procedure SetLimited(Value: Boolean);
    procedure SetMode(Value: TInstantAccessMode);
    procedure SetObjectClass(Value: TClass);
    procedure SetObjectClassName(const Value: string);
    procedure SetOnCompare(Value: TInstantCompareObjectsEvent);
    procedure SetOnLimit(Value: TInstantLimitObjectsEvent);
    procedure SetOnProgress(const Value: TInstantProgressEvent);
    procedure SetSorted(Value: Boolean);
  protected
    { IProviderSupport }
    procedure PSGetAttributes(List: TList); override;
    function PSGetTableName: string; override;
    procedure PSReset; override;
  protected
    procedure AddClassFieldDefs(const FieldName: string; AClass: TClass); overload;
    procedure AddClassFieldDefs(const FieldName: string; AClass: TInstantCodeClass); overload;
    function AddFieldDef(const Prefix: string; PropInfo: PPropInfo): TFieldDef; overload;
    function AddFieldDef(const FieldName: string; FieldType: TFieldType;
      FieldSize: Integer; FieldAttribs: TFieldAttributes): TFieldDef; overload;
    function AddNewObject(Buffer: PChar; Append: Boolean): TObject;
    function AllocRecordBuffer: PChar; override;
    procedure AutoDispose(AObject: TObject);
    procedure AutoStore(AObject: TObject);
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure CopyBufferToObject(Buffer: PChar; AObject: TObject);
    procedure CopyObjectToBuffer(AObject: TObject; Buffer: PChar);
    function CreateAccessor: TInstantAccessor; virtual;
    procedure CreateFields; override;
    function CreateNestedDataSet(DataSetField: TDataSetField): TDataSet; override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    procedure DestroyAccessor;
    procedure DoAfterDelete; override;
    procedure DoAfterPostField(Field: TField); virtual;
    procedure DoBeforeCancel; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforePostField(Field: TField); virtual;
    procedure DoBeforeScroll; override;
    procedure DoIncludeField(FieldName: string; var Include: Boolean); virtual;
    procedure DoTranslate(Field: TField; var Value: Variant; Write: Boolean);
    function FindAttributeMetadata(const Path: string): TInstantAttributeMetadata;
    function FindObjectBuffer(AObject: TObject): PChar; overload;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function GetAccessor: TInstantAccessor; virtual;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetCanModify: Boolean; override;
    function GetCurrentObject: TObject; virtual;
    function GetFieldOffset(const Field: TField): Integer;
    function GetIsChanged: Boolean; virtual;
    function GetRecNo: Integer; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: Longint; override;
    function GetRecordSize: Word; override;
    function GetSubject: TObject; virtual; abstract;
    function HandleFieldError(E: Exception; Field: TField; Value: Variant;
      Write: Boolean): Boolean; virtual;
    function HasAccessor: Boolean;
    function HasPersistentObjectClass: Boolean;
    function IncludeField(FieldName: string; Default: Boolean): Boolean;
    procedure InitAccessor(AAccessor: TInstantAccessor);
    procedure InitField(Field: TField); virtual;
    procedure InitFieldDef(FieldDef: TFieldDef); virtual;
    procedure InitRecord(Buffer: PChar); override;
    function InternalAddObject(AObject: TObject): Integer;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCancel; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    function InternalGetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; virtual;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalInsert; override;
    procedure InternalInsertObject(ARecNo: Integer; AObject: TObject); virtual;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure InternalReleaseObject(AObject: TObject); virtual;
    function InternalRemoveObject(AObject: TObject): Integer; virtual;
    procedure InternalReset; virtual;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function IsCalcField(Field: TField): Boolean;
    function IsCursorOpen: Boolean; override;
    function IsNested: Boolean;
    procedure LoadField(Obj: TObject; Field: TField); virtual;
    procedure LoadFieldParams(AObject: TObject; Field: TField); virtual;
    procedure LoadFieldValue(Field: TField; Buffer: Pointer; AObject: TObject);
    procedure LoadRecord(RecNo: Integer; Buffer: PChar);
    function LocateObject(const KeyFields: string; const KeyValues: Variant;
      Options: TInstantCompareOptions): Boolean; overload;
    function LocateObject(AObject: TObject): Boolean; overload;
    procedure PostField(Field: TField);
    function ReadProperty(Field: TField; Instance: TObject): Variant;
    function RefreshObjectBuffer(AObject: TObject): Boolean;
    procedure SaveField(Field: TField); virtual;
    procedure SaveFieldValue(Field: TField; Buffer: Pointer; AObject: TObject);
    procedure SetActive(Value: Boolean); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetRecNo(Value: Integer); override;
    procedure Undo; virtual;
    procedure UpdateCalcFields;
    procedure WriteProperty(Field: TField; Instance: TObject; Value: Variant); virtual;
    function BreakThorough( const FieldName : string ) : boolean; virtual;
    property Accessor: TInstantAccessor read GetAccessor;
    property ContainerName: string read FContainerName write SetContainerName;
    property CurrentBuffer: PChar read GetCurrentBuffer;
    property DesignClass: TInstantCodeClass read GetDesignClass;
    property HasCurrentBuffer: Boolean read GetHasCurrentBuffer;
    property InContent: Boolean read GetInContent;
    property Mode: TInstantAccessMode read GetMode write SetMode default amObject;
    property ObjectClass: TClass read GetObjectClass write SetObjectClass;
    property ObjectClassName: string read GetObjectClassName write SetObjectClassName stored HasObjectClassName;
    property Subject: TObject read GetSubject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddObject(AObject: TObject): Integer;
    procedure ApplyChanges;
    procedure AssignFieldValue(Field: TField; Value: Variant);
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function ChangesDisabled: Boolean;
    procedure Clear;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function CreateObject: TObject; virtual;
    procedure DeleteObject(Index: Integer);
    procedure DisableChanges;
    procedure EnableChanges;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetFieldStrings(Field: TField; Strings: TStrings): Integer;
    function GotoObject(AObject: TObject): Boolean;
    function IndexOfObject(AObject: TObject): Integer;
    procedure InsertObject(AObject: TObject);
    procedure LoadFields;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    procedure PostChanges;
    procedure RefreshCurrentObject;
    procedure RefreshData;
    procedure RefreshDataView;
    procedure Remember;
    procedure ReleaseObject(AObject: TObject);
    function RemoveObject(AObject: TObject): Integer;
    procedure Reset;
    procedure Revert;
    procedure UpdateBookmark(var BM: TInstantBookmark);
    property CurrentObject: TObject read GetCurrentObject;
    property HasSubject: Boolean read GetHasSubject;
    property IsChanged: Boolean read GetIsChanged;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[Index: Integer]: TObject read GetObjects;
    property TotalCount: Integer read GetTotalCount;
  published
    property FieldOptions: TInstantFieldOptions read FFieldOptions write SetFieldOptions default [foThorough];
    property Filtered;
    property Limited: Boolean read GetLimited write SetLimited default False;
    property Options: TInstantExposerOptions read FOptions write FOptions default [eoAutoApply];
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property AfterCancel;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterPost;
    property AfterPostField: TInstantFieldEvent read FAfterPostField write FAfterPostField;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforePost;
    property BeforePostField: TInstantFieldEvent read FBeforePostField write FBeforePostField;
    property BeforeRefresh;
    property BeforeScroll;
    property OnCalcFields;
    property OnCompare: TInstantCompareObjectsEvent read GetOnCompare write SetOnCompare;
    property OnCreateObject: TInstantCreateObjectEvent read FOnCreateObject write FOnCreateObject;
    property OnFieldError: TInstantFieldErrorEvent read FOnFieldError write FOnFieldError;
    property OnFilterRecord;
    property OnIncludeField: TInstantIncludeFieldEvent read FOnIncludeField write FOnIncludeField;
    property OnInitField: TInstantFieldEvent read FOnInitField write FOnInitField;
    property OnInitFieldDef: TInstantFieldDefEvent read FOnInitFieldDef write FOnInitFieldDef;
    property OnLimit: TInstantLimitObjectsEvent read GetOnLimit write SetOnLimit;
    property OnProgress: TInstantProgressEvent read GetOnProgress write SetOnProgress;
    property OnTranslate: TInstantFieldTranslateEvent read FOnTranslate write FOnTranslate;
    property OnAddClassFieldDef : TInstantAddClassFieldDefEvent read FOnAddClassFieldDef write FOnAddClassFieldDef;
  end;

  TInstantExposerLink = class(TDetailDataLink)
  private
    FDetailExposer: TInstantCustomExposer;
    FMasterProperty: string;
    FOnMasterChange: TNotifyEvent;
    FOnMasterDisable: TNotifyEvent;
  protected
    procedure ActiveChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    procedure DoMasterChange;
    procedure DoMasterDisable;
    function GetDetailDataSet: TDataSet; override;
    function GetMasterExposer: TInstantCustomExposer;
    function GetSubject: TObject;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADetailExposer: TInstantCustomExposer);
    property DetailExposer: TInstantCustomExposer read FDetailExposer;
    property MasterExposer: TInstantCustomExposer read GetMasterExposer;
    property MasterProperty: string read FMasterProperty write FMasterProperty;
    property Subject: TObject read GetSubject;
    property OnMasterChange: TNotifyEvent read FOnMasterChange write FOnMasterChange;
    property OnMasterDisable: TNotifyEvent read FOnMasterDisable write FOnMasterDisable;
  end;

  TInstantExposer = class(TInstantCustomExposer)
  private
    FMasterLink: TInstantExposerLink;
    FSubject: TObject;
    function GetMasterLink: TInstantExposerLink;
    function GetMasterProperty: string;
    function GetMasterSource: TDataSource;
    procedure SetMasterProperty(const Value: string);
    procedure SetMasterSource(Value: TDataSource);
    procedure SetSubject(Value: TObject);
    procedure SyncWithParent(Field: TDataSetField);
  protected
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
    function GetIsChanged: Boolean; override;           
    function GetSubject: TObject; override;
    procedure MasterChanged(Sender: TObject);
    procedure SetDataSetField(const Value: TDataSetField); override;
    procedure SubjectChanged; virtual;
    procedure MasterDisabled(Sender: TObject);
    property MasterLink: TInstantExposerLink read GetMasterLink;
  public
    destructor Destroy; override;
    property ObjectClass;
    property Subject write SetSubject;
  published
    property ContainerName;
    property MasterProperty: string read GetMasterProperty write SetMasterProperty;
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property Mode;
    property ObjectClassName;
  end;

  TInstantQueryAccessor = class(TInstantAccessor)
  private
    function GetSubject: TInstantQuery;
  protected
    function GetConnector: TInstantConnector; override;
    function GetMode: TInstantAccessMode; override;
    function InternalAddObject(AObject: TObject): Integer; override;
    procedure InternalApplyChanges; override;
    function InternalGetObjectClassName: string; override;
    function InternalGetObjectCount: Integer; override;
    function InternalGetObjects(Index: Integer): TObject; override;
    function InternalIndexOfObject(AObject: TObject): Integer; override;
    function InternalInsertObject(Index: Integer; AObject: TObject): Integer; override;
    procedure InternalRefreshObjects; override;
    procedure InternalReleaseObject(AObject: TObject); override;
    function InternalRemoveObject(AObject: TObject): Integer; override;
  public
    property Mode read GetMode;
    class function SubjectClass: TClass; override;
    property Subject: TInstantQuery read GetSubject;
  end;

  TInstantSelector = class(TInstantCustomExposer)
  private
    FAutoOpen: Boolean;
    FCommand: TStringList;
    FConnector: TInstantConnector;
    FMaxCount: Integer;
    FParamCheck: Boolean;
    FParams: TParams;
    FQuery: TInstantQuery;
    procedure CommandChanged(Sender: TObject);
    procedure DestroyQuery;
    function GetCommand: TStringList;
    function GetConnector: TInstantConnector;
    function GetParams: TParams;
    function GetQuery: TInstantQuery;
    procedure ReadParamData(Reader: TReader);
    procedure SetCommand(const Value: TStringList);
    procedure SetConnector(const Value: TInstantConnector);
    procedure SetMaxCount(const Value: Integer);
    procedure SetParams(Value: TParams);
    procedure WriteParamData(Writer: TWriter);
  protected
    { IProviderSupport }
    procedure PSEndTransaction(Commit: Boolean); override;
    function PSGetParams: TParams; override;
    function PSInTransaction: Boolean; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    procedure PSStartTransaction; override;
  protected
    function CanAutoOpen: Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    function GetIsChanged: Boolean; override;
    function GetSubject: TObject; override;
    function HasCommand: Boolean;
    function HasConnector: Boolean;
    procedure Loaded; override;
    procedure InternalClose; override;
    procedure InternalOpen; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(Value: Boolean); override;
    procedure UpdateParams; virtual;
    property Query: TInstantQuery read GetQuery;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ObjectClass;
  published
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen default False;
    property Command: TStringList read GetCommand write SetCommand;
    property Connector: TInstantConnector read GetConnector write SetConnector;
    property MaxCount: Integer read FMaxCount write SetMaxCount default 0;
    property ObjectClassName;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property Params: TParams read GetParams write SetParams stored False;
    property AfterClose;
    property AfterOpen;
    property BeforeClose;
    property BeforeOpen;
  end;

  TInstantBlobStream = class(TStream)
  private
    FAttribute: TInstantBlob;
    FIsChanged: Boolean;
    FField: TBlobField;
    FPosition: Integer;
    function GetExposer: TInstantCustomExposer;
  protected
    procedure Changed;
    procedure Truncate;
    property Attribute: TInstantBlob read FAttribute;
    property Exposer: TInstantCustomExposer read GetExposer;
    property IsChanged: Boolean read FIsChanged;
  public
    constructor Create(AField: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property Field: TBlobField read FField;
  end;

  TInstantAttributeMetadataIncludeProc = procedure(AttributeMetadata: TInstantAttributeMetadata;
    var Include: Boolean; var Traverse: Boolean);
  TInstantPropertyIncludeProc = procedure(PropInfo: PPropInfo;
    var Include: Boolean; var Traverse: Boolean);

procedure InstantGetAttributeList(ClassMetadata: TInstantClassMetadata;
  Items: TStrings; IncludeProc: TInstantAttributeMetadataIncludeProc = nil;
  Circular: Boolean = False);
procedure InstantGetPropertyList(AClass: TClass; Items: TStrings;
  IncludeProc: TInstantPropertyIncludeProc = nil;
  Circular: Boolean = False);

function InstantFindAccessorClass(ObjectClass: TClass): TInstantAccessorClass;
function InstantGetAccessorClass(ObjectClass: TClass): TInstantAccessorClass;
procedure InstantRegisterAccessorClass(AClass: TInstantAccessorClass);
procedure InstantUnregisterAccessorClass(AClass: TInstantAccessorClass);

implementation

uses
{$IFDEF VER130}
  Forms,
{$ENDIF}
{$IFDEF LINUX}
  QForms,
{$ENDIF}
  {$IFDEF D6+}
    Variants,
    {$IFNDEF FPC}MaskUtils,{$ENDIF}
     FmtBcd,
  {$ENDIF} InstantClasses,
  InstantConsts, InstantRtti, InstantDesignHook, InstantAccessors,
  DbConsts;

const
  SelfFieldName = 'Self';
  DefaultStringFieldSize = 255;
  DefaultStringDisplayWidth = 12;

var
  AccessorClasses: TList;

function AttributeTypeToFieldType(
  AttributeType: TInstantAttributeType): TFieldType;
const
  FieldTypes: array[TInstantAttributeType] of TFieldType = (
    ftUnknown, ftInteger, ftFloat, ftBCD, ftBoolean, ftString, ftDateTime,
    ftBlob, ftMemo, ftBlob, ftInteger, ftInteger, ftDataSet, ftDataSet);
begin
  Result := FieldTypes[AttributeType];
end;

function GetAttributeMetadata(AClass: TClass;
  Path: string; PInstance: Pointer = nil): TInstantAttributeMetadata; overload;
var
  I: Integer;
  PropName: string;
  PropInfo: PPropInfo;
  TypeData: PTypeData;
  ClassMetadata: TInstantClassMetadata;
begin
  I := LastDelimiter('.', Path);
  if I > 0 then
  begin
    Result := nil;
    PropName := Copy(Path, 1, Pred(I));
    Delete(Path, 1, I);
    PropInfo := InstantGetPropInfo(AClass, PropName, PInstance);
    if Assigned(PropInfo) and (PropInfo^.PropType^.Kind = tkClass) then
    begin
      TypeData := GetTypeData(PropInfo^.PropType^);
      if Assigned(TypeData) then
        AClass := TypeData.ClassType
      else
        Exit;
    end else
      Exit;
  end;
  if AClass.InheritsFrom(TInstantObject) then
  begin
    ClassMetadata := TInstantObjectClass(AClass).Metadata;
    if Assigned(ClassMetadata) then
    begin
      Result := ClassMetadata.MemberMap.Find(Path);
      Exit;
    end;
  end;
  Result := nil;
end;

function GetAttributeMetadata(AClass: TInstantCodeClass;
  Path: string): TInstantAttributeMetadata; overload;

  function FindAttribute(AClass: TInstantCodeClass;
    Name: string): TInstantCodeAttribute;
  begin
    while Assigned(AClass) do
    begin
      Result := AClass.FindAttribute(Name);
      if Assigned(Result) then
        Exit;
      AClass := AClass.BaseClass;
    end;
    Result := nil;
  end;

var
  I: Integer;
  AttributeName: string;
  Attribute: TInstantCodeAttribute;
begin
  I := Pos('.', Path);
  if I > 0 then
  begin
    AttributeName := Copy(Path, 1, Pred(I));
    Delete(Path, 1, I);
    Attribute := FindAttribute(AClass, AttributeName);
    if Assigned(Attribute) then
      Result := GetAttributeMetadata(Attribute.ObjectClass, Path)
    else
      Result := nil;
  end else
  begin
    Attribute := FindAttribute(AClass, Path);
    if Assigned(Attribute) then
      Result := Attribute.Metadata
    else
      Result := nil;
  end;
end;

function InstantFindAccessorClass(ObjectClass: TClass): TInstantAccessorClass;
begin
  Result := InstantGetAccessorClass(ObjectClass);
  if not Assigned(Result) then
    raise EInstantError.CreateFmt(SAccessorClassNotFoundFor,
      [ObjectClass.ClassName]);
end;

function InstantGetAccessorClass(ObjectClass: TClass): TInstantAccessorClass;
var
  I: Integer;
  AClass: TClass;
begin
  AClass := ObjectClass;
  while Assigned(AClass) do
  begin
    for I := 0 to Pred(AccessorClasses.Count) do
    begin
      Result := TInstantAccessorClass(AccessorClasses[I]);
      if Result.SubjectClass = AClass then
        Exit;
    end;
    AClass := AClass.ClassParent;
  end;
  Result := nil;
end;

procedure InstantGetAttributeList(ClassMetadata: TInstantClassMetadata;
  Items: TStrings; IncludeProc: TInstantAttributeMetadataIncludeProc;
  Circular: Boolean);

  procedure AddAttributes(const Prefix: string;
    ClassMetadata: TInstantClassMetadata; Items, Paths: TStrings);
  var
    I, PathIndex: Integer;
    AttributeMetadata: TInstantAttributeMetadata;
    Path: string;
    Include, Traverse: Boolean;
  begin
    if not Assigned(ClassMetadata) then
      Exit;
    with ClassMetadata do
      for I := 0 to Pred(MemberMap.Count) do
      begin
        AttributeMetadata := MemberMap[I];
        Include := True;
        Traverse := AttributeMetadata.Category = acElement;
        if Assigned(IncludeProc) then
          IncludeProc(AttributeMetadata, Include, Traverse);
        if Include then
          Items.AddObject(Prefix + AttributeMetadata.Name, AttributeMetadata);
        if Traverse then
        begin
          Path := ClassMetadata.Name + '.' + AttributeMetadata.Name;
          if Circular or (Paths.IndexOf(Path) = -1) then
          begin
            PathIndex := Paths.Add(Path);
            try
              AddAttributes(Prefix + AttributeMetadata.Name + '.',
                AttributeMetadata.ObjectClassMetadata, Items, Paths);
            finally
              Paths.Delete(PathIndex);
            end;
          end;
        end;
      end;
  end;

var
  Paths: TStringList;
begin
  Paths := TStringList.Create;
  Items.BeginUpdate;
  try
    AddAttributes('', ClassMetadata, Items, Paths);
  finally
    Items.EndUpdate;
    Paths.Free;
  end;
end;

procedure InstantGetPropertyList(AClass: TClass; Items: TStrings;
  IncludeProc: TInstantPropertyIncludeProc; Circular: Boolean);

  procedure AddProperties(const Prefix: string; AClass: TClass;
    Items, Paths: TStrings);
  var
    I, PathIndex: Integer;
    PropInfo: PPropInfo;
    TypeData: PTypeData;
    Path: string;
    Include, Traverse: Boolean;
  begin
    with TInstantProperties.Create(AClass) do
    try
      for I := 0 to Pred(Count) do
      begin
        PropInfo := PropInfos[I];
        Include := True;
        Traverse := PropInfo.PropType^.Kind = tkClass;
        if Assigned(IncludeProc) then
          IncludeProc(PropInfo, Include, Traverse);
        if Include then
          Items.AddObject(Prefix + Names[I], Pointer(PropInfo));
        if Traverse then
        begin
          Path := AClass.ClassName + '.' + PropInfo.Name;
          if Circular or (Paths.IndexOf(Path) = -1) then
          begin
            PathIndex := Paths.Add(Path);
            try
              TypeData := GetTypeData(PropInfo.PropType^);
              AddProperties(Prefix + PropInfo.Name + '.', TypeData.ClassType,
                Items, Paths);
            finally
              Paths.Delete(PathIndex);
            end;
          end;
        end;
      end;
    finally
      Free;
    end;
  end;

var
  Paths: TStringList;
begin
  Paths := TStringList.Create;
  Items.BeginUpdate;
  try
    AddProperties('', AClass, Items, Paths);
  finally
    Items.EndUpdate;
    Paths.Free;
  end;
end;

procedure InstantRegisterAccessorClass(AClass: TInstantAccessorClass);
begin
  if Assigned(AClass) then
    AccessorClasses.Add(AClass);
end;

procedure InstantUnregisterAccessorClass(AClass: TInstantAccessorClass);
begin
  AccessorClasses.Remove(AClass);
end;


{ TInstantAccessor }

function TInstantAccessor.AddObject(AObject: TObject): Integer;
begin
  Result := InternalAddObject(AObject);
  if Altered then
    Result := AddToView(AObject);
  ChangedData;
end;

function TInstantAccessor.AddToView(AObject: TObject): Integer;
begin
  Result := InsertInView(Succ(ObjectCount), AObject);
end;

procedure TInstantAccessor.ApplyChanges;
begin
  InternalApplyChanges;
end;

procedure TInstantAccessor.Changed(ChangeType: TInstantChangeType);
begin
  if not ChangesDisabled and Assigned(FOnChange) then
    FOnChange(Self, ChangeType);
end;

procedure TInstantAccessor.ChangedAppearance;
begin
  Reset;
  Changed(ctAppearance);
end;

procedure TInstantAccessor.ChangedClass;
begin
  FCachedClass := nil;
  ChangedAppearance;
end;

procedure TInstantAccessor.ChangedData;
begin
  Changed(ctData);
end;

function TInstantAccessor.ChangesDisabled: Boolean;
begin
  Result := FChangesDisabledCount > 0;
end;

procedure TInstantAccessor.Clear;
begin
  InternalClear;
  Reset;
end;

constructor TInstantAccessor.Create(ASubject: TObject);
begin
  inherited Create;
  FSubject := ASubject;
end;

function TInstantAccessor.CreateObject: TObject;
begin
  Result := InternalCreateObject;
end;

destructor TInstantAccessor.Destroy;
begin
  DestroyView;
  inherited;
end;

procedure TInstantAccessor.DestroyView;
begin
  FreeAndNil(FView);
end;

procedure TInstantAccessor.DisableChanges;
begin
  Inc(FChangesDisabledCount);
end;

procedure TInstantAccessor.DoLimit(AObject: TObject; var Accept: Boolean);
begin
  if Limited and Assigned(FOnLimit) then
    FOnLimit(Self, AObject, Accept);
end;

procedure TInstantAccessor.DoProgress(Sender: TObject; Count: Integer;
  var Continue: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Count, Continue);
end;

procedure TInstantAccessor.EnableChanges;
begin
  if ChangesDisabled then
    Dec(FChangesDisabledCount);
end;

function TInstantAccessor.FindObject(const KeyFields: string;
  const KeyValues: Variant; Options: TInstantCompareOptions): Integer;
var
  FieldNames: TStringList;
  I: Integer;
begin
  Result := -1;
  if Pos(';', KeyFields) = 0 then
  begin
    for I := 0 to Pred(ObjectCount) do
      if InstantMatchObject(Objects[I], KeyFields, KeyValues, Options) then
      begin
        Result := I;
        Break;
      end;
  end else
  begin
    FieldNames := TStringList.Create;
    try
      InstantStrToList(KeyFields, FieldNames, [';']);
      for I := 0 to Pred(ObjectCount) do
        if InstantMatchObject(Objects[I], FieldNames, KeyValues, Options) then
        begin
          Result := I;
          Break;
        end;
    finally
      FieldNames.Free;
    end;
  end;
end;

function TInstantAccessor.GetAltered: Boolean;
begin
  Result := Limited or Sorted or Assigned(FOnProgress);
end;

function TInstantAccessor.GetConnector: TInstantConnector;
begin
  Result := nil;
end;

function TInstantAccessor.GetHasSubject: Boolean;
begin
  Result := Assigned(FSubject);
end;

function TInstantAccessor.GetInContent: Boolean;
begin
  Result := Mode = amContent;
end;

function TInstantAccessor.GetMode: TInstantAccessMode;
begin
  Result := FMode;
end;

function TInstantAccessor.GetObjectClass: TClass;
begin
  if Assigned(FObjectClass) then
    Result := FObjectClass
  else if not Assigned(DesignModel) and (ObjectClassName <> '') then
  begin
    if not Assigned(FCachedClass) then
    begin
      FCachedClass := GetClass(ObjectClassName);
      if not Assigned(FCachedClass) then
        raise EInstantError.CreateFmt(SClassNotRegistered, [ObjectClassName]);
    end;
    Result := FCachedClass;
  end else
  begin
    if HasSubject and not InContent then
      Result := Subject.ClassType
    else begin
      Result := InternalGetObjectClass;
      if not Assigned(Result) then
        Result := TObject;
    end;
  end;
end;

function TInstantAccessor.GetObjectClassName: string;
begin
  Result := FObjectClassName;
  if Result = '' then
    Result := InternalGetObjectClassName;
end;

function TInstantAccessor.GetObjectCount: Integer;
begin
  if Altered then
    Result := View.Count
  else
    Result := InternalObjectCount;
end;

function TInstantAccessor.GetObjects(Index: Integer): TObject;
begin
  if Altered then
    Result := View[Index]
  else
    Result := InternalObjects[Index];
end;

function TInstantAccessor.GetSubject: TObject;
begin
  if not Assigned(FSubject) then
    raise Exception.Create('Subject unassigned');
  Result := FSubject;
end;

function TInstantAccessor.GetTotalCount: Integer;
begin
  Result := InternalObjectCount;
end;

function TInstantAccessor.GetView: TList;
var
  I: Integer;
  Continue:Boolean;
begin
  if not Assigned(FView) then
  begin
    Continue:=True;
    FView := TList.Create;
    FView.Capacity := InternalObjectCount;
    for I := 0 to Pred(InternalObjectCount) do
    begin
      DoProgress(InternalObjects[I], I+1, Continue);
      if not Continue then Break;
      AddToView(InternalObjects[I]);
    end;
  end;
  Result := FView;
end;

function TInstantAccessor.IncludeObject(AObject: TObject): Boolean;
begin
  Result := True;
  DoLimit(AObject, Result);
end;

function TInstantAccessor.IndexOfInstance(Instance: Pointer): Integer;
begin
  if Altered then
    Result := View.IndexOf(Instance)
  else
    Result := InternalIndexOfInstance(Instance);
end;

function TInstantAccessor.IndexOfObject(AObject: TObject): Integer;
begin
  if Altered then
    Result := View.IndexOf(AObject)
  else
    Result := InternalIndexOfObject(AObject);
end;

function TInstantAccessor.InsertInView(Index: Integer;
  AObject: TObject): Integer;

  function FindIndex: Integer;
  var
    HighIndex, MidIndex, Compare: Integer;
  begin
    Result := 0;
    HighIndex := Pred(ObjectCount);
    while Result <= HighIndex do
    begin
      MidIndex := (Result + HighIndex) div 2;
      FOnCompare(Self, Objects[MidIndex], AObject, Compare);
      if Compare = 0 then
      begin
        Result := MidIndex;
        Break;
      end else if Compare < 0 then
        Result := MidIndex + 1
      else
        HighIndex := MidIndex - 1;
    end;
  end;

begin
  if not (Assigned(AObject) and IncludeObject(AObject)) then
  begin
    Result := -1;
    Exit;
  end;
  if Sorted and Assigned(FOnCompare) then
    Index := FindIndex;
  if Index < View.Count then
  begin
    View.Insert(Index, AObject);
    Result := Index;
  end else
  begin
    View.Add(AObject);
    Result := Pred(View.Count);
  end;
end;

function TInstantAccessor.InsertObject(Index: Integer;
  AObject: TObject): Integer;
begin
  Result := InternalInsertObject(Index, AObject);
  if Altered then
    Result := InsertInView(Index, AObject);
  ChangedData;
end;

function TInstantAccessor.InternalAddObject(AObject: TObject): Integer;
begin
  Result := -1;
end;

procedure TInstantAccessor.InternalApplyChanges;
begin
end;

procedure TInstantAccessor.InternalClear;
begin
end;

function TInstantAccessor.InternalCreateObject: TObject;
begin
  if ObjectClass.InheritsFrom(TInstantObject) then
    Result := TInstantObjectClass(ObjectClass).Create(Connector)
  else
    Result := ObjectClass.Create;
end;

function TInstantAccessor.InternalGetObjectClass: TClass;
begin
  Result := TObject;
end;

function TInstantAccessor.InternalGetObjectClassName: string;
begin
  Result := '';
end;

function TInstantAccessor.InternalGetObjectCount: Integer;
begin
  Result := 1;
end;

function TInstantAccessor.InternalGetObjects(Index: Integer): TObject;
begin
  Result := Subject;
end;

function TInstantAccessor.InternalIndexOfInstance(Instance: Pointer): Integer;
begin
  Result := InternalIndexOfObject(Instance);
end;

function TInstantAccessor.InternalIndexOfObject(AObject: TObject): Integer;
begin
  if AObject = Subject then
    Result := 0
  else
    Result := -1;
end;

function TInstantAccessor.InternalInsertObject(Index: Integer;
  AObject: TObject): Integer;
begin
  Result := -1;
end;

procedure TInstantAccessor.InternalRefreshObjects;
begin
end;

procedure TInstantAccessor.InternalReleaseObject(AObject: TObject);
begin
end;

function TInstantAccessor.InternalRemoveObject(AObject: TObject): Integer;
begin
  Result := -1;
end;

procedure TInstantAccessor.Refresh;
begin
  DestroyView;
  RefreshObjects;
  Changed(ctData);
end;

procedure TInstantAccessor.RefreshObjects;
begin
  InternalRefreshObjects;
end;

procedure TInstantAccessor.RefreshView;
begin
  DestroyView;
  Changed(ctData);
end;

procedure TInstantAccessor.ReleaseObject(AObject: TObject);
begin
  InternalReleaseObject(AObject);
end;

function TInstantAccessor.RemoveFromView(AObject: TObject): Integer;
begin
  Result := View.Remove(AObject);
end;

function TInstantAccessor.RemoveObject(AObject: TObject): Integer;
begin
  Result := InternalRemoveObject(AObject);
  if Altered then
    Result := RemoveFromView(AObject);
  ChangedData;
end;

function TInstantAccessor.RepositionObject(AObject: TObject): Integer;
begin
  if Altered then
  begin
    RemoveFromView(AObject);
    Result := AddToView(AObject);
  end else
    Result := -1;
end;

procedure TInstantAccessor.Reset;
begin
  DestroyView;
end;

procedure TInstantAccessor.SetContainerName(const Value: string);
begin
  if Value <> FContainerName then
  begin
    FContainerName := Value;
    ChangedClass;
  end;
end;

procedure TInstantAccessor.SetLimited(Value: Boolean);
begin
  if Value <> FLimited then
  begin
    FLimited := Value;
    RefreshView;
  end;
end;

procedure TInstantAccessor.SetMode(Value: TInstantAccessMode);
begin
  if Value <> FMode then
  begin
    FMode := Value;
    ChangedClass;
  end;
end;

procedure TInstantAccessor.SetObjectClass(Value: TClass);
begin
  if Value <> ObjectClass then
  begin
    FObjectClass := Value;
    ChangedClass;
  end;
end;

procedure TInstantAccessor.SetObjectClassName(const Value: string);
begin
  if Value <> ObjectClassName then
  begin
    FObjectClassName := Value;
    ChangedClass;
  end;
end;

procedure TInstantAccessor.SetOnCompare(Value: TInstantCompareObjectsEvent);
begin
  if @Value <> @FOnCompare then
  begin
    FOnCompare := Value;
    RefreshView;
  end;
end;

procedure TInstantAccessor.SetOnLimit(Value: TInstantLimitObjectsEvent);
begin
  if @Value <> @FOnLimit then
  begin
    FOnLimit := Value;
    RefreshView;
  end;
end;

procedure TInstantAccessor.SetOnProgress(
  const Value: TInstantProgressEvent);
begin
  if @Value <> @FOnProgress then
  begin
    FOnProgress := Value;
    RefreshView;
  end;
end;

procedure TInstantAccessor.SetSorted(Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted := Value;
    RefreshView;
  end;
end;

class function TInstantAccessor.SubjectClass: TClass;
begin
  Result := TObject;
end;

{ TInstantCustomExposer }

procedure TInstantCustomExposer.AccessorChanged(Sender: TObject;
  ChangeType: TInstantChangeType);
begin
  case ChangeType of
    ctData:
      if Active then
      begin
        Refresh;
        DoAfterScroll;
      end;
    ctAppearance:
      Reset;
  end;
end;

procedure TInstantCustomExposer.AddClassFieldDefs(const FieldName: string;
  AClass: TClass);
{$J+}
const
  Relations: TStringList = nil;
  Level: Integer = 0;
{$J-}

  procedure AddContainerFieldDefs(Prefix: string; AClass: TClass);
  var
    I: Integer;
    Map: TInstantAttributeMap;
  begin
    if not Assigned(AClass) or not AClass.InheritsFrom(TInstantObject) or
      (IsNested and not (foRecurseNesting in FieldOptions)) then
      Exit;
    Map := TInstantObjectClass(AClass).Metadata.MemberMap;
    if Assigned(Map) then
      for I := 0 to Pred(Map.Count) do
        with Map[I] do
          if AttributeType in [atParts, atReferences] then
            AddFieldDef(Prefix + Name, ftDataSet, 0, []);
  end;

  procedure AddObjectFieldDef(AClass: TClass; Prefix: string;
    PropInfo: PPropInfo);
  var
    Relation: string;
    Index: Integer;
  begin
    Relation := AClass.ClassName + '.' + PropInfo.Name;
      if BreakThorough(Prefix + PropInfo.Name) then
        Exit;

    if (Relations.IndexOf(Relation) = -1) or
      IncludeField(Prefix + '.' + PropInfo.Name, False) then
    begin
      Relations.Add(Relation);
      try
        AddFieldDef(Prefix, PropInfo);
      finally
        Index := Relations.IndexOf(Relation);
        if Index <> -1 then
          Relations.Delete(Index);
      end;
    end;
  end;

var
  TypeData: PTypeData;
  PropList: PPropList;
  PropInfo: PPropInfo;
  PropSize: Integer;
  Prefix: string;
  I: Integer;
begin
  if not Assigned(AClass) or (AClass.ClassInfo = nil) then
    Exit;
  if Level = 0 then
    Relations := TStringList.Create;
  Inc(Level);
  try
    TypeData := GetTypeData(AClass.ClassInfo);
    PropSize := TypeData.PropCount * SizeOf(Pointer);
    if FieldName = '' then
      Prefix := ''
    else
      Prefix := FieldName + '.';
    GetMem(PropList, PropSize);
    try
      GetPropInfos(AClass.ClassInfo, PropList);
      for I := 0 to Pred(TypeData.PropCount) do
      begin
        PropInfo := PropList[I];
        if PropInfo.PropType^.Kind = tkClass then
          AddObjectFieldDef(AClass, Prefix, PropInfo)
        else
          AddFieldDef(Prefix, PropInfo);
      end;
    finally
      FreeMem(PropList, PropSize);
    end;
    AddContainerFieldDefs(Prefix, AClass);
  finally
    Dec(Level);
    if Level = 0 then
      FreeAndNil(Relations);
  end;
end;

procedure TInstantCustomExposer.AddClassFieldDefs(const FieldName: string;
  AClass: TInstantCodeClass);
{$J+}
const
  Level: Integer = 0;
  Relations: TStringList = nil;
{$J-}

  procedure AddProperty(const Prefix: string; Prop: TInstantCodeProperty);
  var
    Relation: string;
    I: Integer;
  begin
    if Prop.Visibility = viPublished then
    begin
      if Prop.TypeValue is TInstantCodeClass then
      begin
        if foObjects in FieldOptions then
          AddFieldDef(Prefix + Prop.Name, ftInteger, 0, []);
        if foThorough in FieldOptions then
        begin
          Relation := AClass.Name + '.' + Prop.Name;
          if Relations.IndexOf(Relation) = -1 then
          begin
            Relations.Add(Relation);
            try
              AddClassFieldDefs(Prefix + Prop.Name,
                TInstantCodeClass(Prop.TypeValue));
            finally
              I := Relations.IndexOf(Relation);
              if I <> -1 then
                Relations.Delete(I);
            end;
          end;
        end;
      end else
        AddFieldDef(Prefix + Prop.Name,
          AttributeTypeToFieldType(Prop.AttributeType), 0, []);
    end;
  end;

  procedure AddContainer(const Prefix: string;
    Attribute: TInstantCodeAttribute);
  begin
    if Attribute.IsContainer then
      AddFieldDef(Prefix + Attribute.Name, ftDataSet, 0, []);
  end;

var
  I: Integer;
  Prefix: string;
begin
  if not Assigned(AClass) then
    Exit;
  if Level = 0 then
    Relations := TStringList.Create;
  Inc(Level);
  try
    if FieldName = '' then
      Prefix := ''
    else
    begin
      if BreakThorough(FieldName) then
        Exit;
      Prefix := FieldName + '.';
    end;
    while Assigned(AClass) do
    begin
      for I := 0 to Pred(AClass.PropertyCount) do
        AddProperty(Prefix, AClass.Properties[I]);
      for I := 0 to Pred(AClass.AttributeCount) do
        AddContainer(Prefix, AClass.Attributes[I]);
      AClass := AClass.BaseClass;
    end;
  finally
    Dec(Level);
    if Level = 0 then
      FreeAndNil(Relations);
  end;
end;

function TInstantCustomExposer.AddFieldDef(const FieldName: string;
  FieldType: TFieldType; FieldSize: Integer;
  FieldAttribs: TFieldAttributes): TFieldDef;
begin
  if IncludeField(FieldName, True) and (FieldDefs.IndexOf(FieldName) = -1) then
  begin
    Result := FieldDefs.AddFieldDef;
    Result.FieldNo := FieldDefs.Count;
    with Result do
    begin
      Name := FieldName;
      DataType := FieldType;
      if FieldSize <> 0 then
        Size := FieldSize;
      if DataType = ftBCD then
        Precision := 18;
      Attributes := FieldAttribs;
    end;
    InitFieldDef(Result);
  end else
    Result := nil;
end;

function TInstantCustomExposer.AddFieldDef(const Prefix: string;
  PropInfo: PPropInfo): TFieldDef;

  function FloatFieldType : TFieldType;
  var
    ATypeInfo : PTypeInfo;
  begin
    ATypeInfo := PropInfo.PropType^;
    if GetTypeData(PropInfo^.PropType^).FloatType = ftCurr then
      Result := DB.ftBcd
    else
    if ATypeInfo = TypeInfo(TDateTime) then
      Result := DB.ftDateTime
(*
    else if ATypeInfo = TypeInfo(TDate) then
      Result := DB.ftDate
    else if ATypeInfo = TypeInfo(TTime) then
      Result := DB.ftTime
*)      
    else
      Result := DB.ftFloat;
  end;

  function StringFieldType(const FieldName: string): TFieldType;
  var
    AttributeMetadata: TInstantAttributeMetadata;
  begin
    AttributeMetadata := FindAttributeMetadata(FieldName);
    if Assigned(AttributeMetadata) then
      case AttributeMetadata.AttributeType of
        atBlob:
          Result := ftBlob;
        atMemo:
          Result := ftMemo;
      else
        Result := ftString;
      end
    else
      Result := ftString;
  end;

var
  FieldName: string;
  FieldType: TFieldType;
  FieldSize: Integer;
  FieldAttribs: TFieldAttributes;
  TypeKind: TTypeKind;
  TypeData: PTypeData;
begin
  Result := nil;
  if not Assigned(PropInfo) then
    Exit;
  FieldName := Prefix + PropInfo^.Name;
  FieldSize := 0;
  FieldAttribs := [];
  TypeKind := PropInfo^.PropType^^.Kind;
  if not Assigned(PropInfo.SetProc) then
    Include(FieldAttribs, DB.faReadOnly);
  case TypeKind of
    tkEnumeration:
      if PropInfo^.PropType^^.Name = 'Boolean' then
        FieldType := ftBoolean
      else
        FieldType := ftString;
    tkString, tkLString:
      FieldType := StringFieldType(FieldName);
    tkInteger:
      FieldType := ftInteger;
    tkFloat:
      FieldType := FloatFieldType;
    tkClass:
      FieldType := ftInteger;
    tkSet:
      FieldType := ftString;
  else
    Exit;
  end;
  if (TypeKind <> tkClass) or (foObjects in FieldOptions) then
    Result := AddFieldDef(FieldName, FieldType, FieldSize, FieldAttribs)
  else
    Result := nil;
  if (TypeKind = tkClass) and (foThorough in FieldOptions) then
  begin
    TypeData := GetTypeData(PropInfo^.PropType^);
    AddClassFieldDefs(FieldName, TypeData^.ClassType);
  end;
end;

function TInstantCustomExposer.AddNewObject(Buffer: PChar;
  Append: Boolean): TObject;
begin
  Result := CreateObject;
  if not (eoDeferInsert in Options) then
    PutObject(Buffer, Result, Append)
  else
    CopyObjectToBuffer(Result, Buffer);
end;

function TInstantCustomExposer.AddObject(AObject: TObject): Integer;
begin
  Result := InternalAddObject(AObject);
  if Result <> -1 then
    GotoObject(AObject);
end;

function TInstantCustomExposer.AllocRecordBuffer: PChar;
begin
  Result := StrAlloc(FRecBufSize);
end;

procedure TInstantCustomExposer.ApplyChanges;
begin
  PostChanges;
  Accessor.ApplyChanges;
end;

procedure TInstantCustomExposer.AssignFieldValue(Field: TField; Value: Variant);
var
  I: Integer;
begin
  WriteProperty(Field, CurrentObject, Value);
  for I := 0 to Pred(FieldCount) do
    LoadField(CurrentObject, Fields[I]);
  UpdateCalcFields;
end;

procedure TInstantCustomExposer.AutoDispose(AObject: TObject);
begin
  if (eoAutoApply in Options) and (AObject is TInstantObject) then
    with TInstantObject(AObject) do
      if CanDispose then
        Dispose;
end;

procedure TInstantCustomExposer.AutoStore(AObject: TObject);
begin
  if (eoAutoApply in Options) and (AObject is TInstantObject) then
    with TInstantObject(AObject) do
      if CanStore then
      begin
        Store;
        CopyObjectToBuffer(AObject, ActiveBuffer);
      end;
end;

function TInstantCustomExposer.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  BM: TInstantBookmark;
begin
  BM := TInstantBookmark(Bookmark^);
  UpdateBookmark(BM);
  Result := BM.RecNo > 0;
end;

function TInstantCustomExposer.BreakThorough(const FieldName: string): boolean;
begin
  Result := False;
  if Assigned(FOnAddClassFieldDef) then
    FOnAddClassFieldDef(FieldName, Result);
end;

function TInstantCustomExposer.ChangesDisabled: Boolean;
begin
  Result := Accessor.ChangesDisabled and FNotifier.Disabled;
end;

procedure TInstantCustomExposer.CheckClass(AObject: TObject);
begin
  if not (AObject is ObjectClass) then
    raise Exception.CreateFmt(SUnexpectedClass,
      [AObject.ClassName, ObjectClass.ClassName]);
end;

procedure TInstantCustomExposer.Clear;
begin
  Cancel;
  Accessor.Clear;
  Refresh;
end;

procedure TInstantCustomExposer.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[RecordSize], CalcFieldsSize, 0);
end;

procedure TInstantCustomExposer.ClearData(Buffer: PChar);
begin
  FillChar(Buffer^, RecordSize, 0);
end;

procedure TInstantCustomExposer.ClearRecord(Buffer: PChar);
begin
  ClearData(Buffer);
  with GetRecInfo(Buffer)^ do
  begin
    RecordNumber := -1;
    UpdateStatus := usDeleted;
    BookmarkFlag := bfCurrent;
  end;
end;

function TInstantCustomExposer.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
var
  BM1, BM2: TInstantBookmark;
begin
  if not Assigned(Bookmark1) or not Assigned(Bookmark2) then
  begin
    Result := 0;
    Exit;
  end;
  BM1 := TInstantBookmark(Bookmark1^);
  BM2 := TInstantBookmark(Bookmark2^);
  UpdateBookmark(BM1);
  UpdateBookmark(BM2);
  if BM1.RecNo < BM2.RecNo then
    Result := -1
  else if BM1.RecNo > BM2.RecNo then
    Result := 1
  else
    Result := 0;
end;

procedure TInstantCustomExposer.CopyBufferToObject(Buffer: PChar;
  AObject: TObject);
var
  I, Offset: Integer;
begin
  if not Assigned(AObject) then
    Exit;
  CheckClass(AObject);
  Offset := 0;
  for I := 0 to Pred(FieldCount) do
  begin
    if not IsCalcField(Fields[I]) then
    begin
      SaveFieldValue(Fields[I], @Buffer[Offset], AObject);
      Inc(Offset, Fields[I].DataSize);
    end;
  end;
end;

procedure TInstantCustomExposer.CopyObjectToBuffer(AObject: TObject;
  Buffer: PChar);
var
  I, Offset: Integer;
  BM: TInstantBookmark;
begin
  if not Assigned(AObject) then
  begin
    ClearRecord(Buffer);
    Exit;
  end;
  CheckClass(AObject);
  Offset := 0;
  for I := 0 to Pred(FieldCount) do
  begin
    if not IsCalcField(Fields[I]) then
    begin
      LoadFieldParams(AObject, Fields[I]);
      LoadFieldValue(Fields[I], @Buffer[Offset], AObject);
      Inc(Offset, Fields[I].DataSize);
    end;
  end;
  GetBookmarkData(Buffer, @BM);
  BM.Instance := AObject;
  SetBookmarkData(Buffer, @BM);
  GetCalcFields(Buffer);
end;

constructor TInstantCustomExposer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [eoAutoApply];
  BookmarkSize := SizeOf(TInstantBookmark);
  FIsOpen := False;
  FFieldOptions := [foThorough];
  FRecNo := 0;
  ObjectView := True;
  FNotifier := TInstantObjectNotifier.Create(nil, True);
  FNotifier.OnObjectChanged := ObjectChanged;
  FNotifier.OnObjectRefreshed := ObjectChanged;
  if (csDesigning in ComponentState) and Assigned(DesignModel) then
    DesignModel^.AddComponent(Self);
end;

function TInstantCustomExposer.CreateAccessor: TInstantAccessor;
var
  AccessorClass: TInstantAccessorClass;
begin
  if Assigned(Subject) then
  begin
    AccessorClass := InstantFindAccessorClass(Subject.ClassType);
    Result := AccessorClass.Create(Subject);
  end else
    Result := TInstantAccessor.Create(nil);
  InitAccessor(Result);
end;

function TInstantCustomExposer.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := TInstantBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TInstantCustomExposer.CreateFields;
begin
  inherited;
  InitFields;
end;

function TInstantCustomExposer.CreateNestedDataSet(
  DataSetField: TDataSetField): TDataSet;
begin
  Result := inherited CreateNestedDataSet(DataSetField);
end;

function TInstantCustomExposer.CreateObject: TObject;
begin
  Result := nil;
  if Assigned(FOnCreateObject) then
    FOnCreateObject(Self, Result);
  if not Assigned(Result) then
    Result := Accessor.CreateObject;
end;

procedure TInstantCustomExposer.DataEvent(Event: TDataEvent;
  Info: Integer);
var
  I: Integer;
  DataSet: TDataSet;
begin
  inherited;
  if Event in [deDataSetChange, deDataSetScroll] then
    for I := 0 to Pred(NestedDataSets.Count) do
    begin
      DataSet := NestedDataSets[I];
      if (DataSet is TInstantCustomExposer) and not DataSet.Active then
        TInstantCustomExposer(DataSet).DataEvent(deParentScroll, 0);
    end;
end;

function TInstantCustomExposer.DataFieldsSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(FieldCount) do
    if not IsCalcField(Fields[I]) then
      Inc(Result, Fields[I].DataSize);
end;

procedure TInstantCustomExposer.DeleteObject(Index: Integer);
begin
  RemoveObject(Objects[Index]);
end;

destructor TInstantCustomExposer.Destroy;
begin
  inherited;
  FNotifier.Free;
  DestroyAccessor;
  if (csDesigning in ComponentState) and Assigned(DesignModel) then
    DesignModel^.RemoveComponent(Self);
end;

procedure TInstantCustomExposer.DestroyAccessor;
begin
  FreeAndNil(FAccessor);
end;

procedure TInstantCustomExposer.DisableChanges;
begin
  Accessor.DisableChanges;
  FNotifier.Disable;
end;

procedure TInstantCustomExposer.DoAfterDelete;
begin
  inherited;
  FRemovedObject := nil;
end;

procedure TInstantCustomExposer.DoAfterPostField(Field: TField);
begin
  if Assigned(FAfterPostField) then
    FAfterPostField(Self, Field);
end;

procedure TInstantCustomExposer.DoBeforeCancel;
begin
  inherited;
  Undo;
end;

procedure TInstantCustomExposer.DoBeforeEdit;
begin
  SaveCurrentObject(FUndoBuffer);
  inherited;
end;

procedure TInstantCustomExposer.DoBeforeInsert;
begin
  if not InContent then
    Abort;
  inherited;
end;

procedure TInstantCustomExposer.DoBeforePostField(Field: TField);
begin
  if Assigned(FBeforePostField) then
    FBeforePostField(Self, Field);
end;

procedure TInstantCustomExposer.DoBeforeScroll;
begin
  Undo;
  inherited;
end;

procedure TInstantCustomExposer.DoIncludeField(FieldName: string;
  var Include: Boolean);
begin
  if Assigned(FOnIncludeField) then
    FOnIncludeField(Self, FieldName, Include);
end;

procedure TInstantCustomExposer.DoTranslate(Field: TField;
  var Value: Variant; Write: Boolean);
begin
  if Assigned(FOnTranslate) then
    FOnTranslate(Self, Field, Value, Write);
end;

procedure TInstantCustomExposer.EnableChanges;
begin
  Accessor.EnableChanges;
  FNotifier.Enable;
end;

function TInstantCustomExposer.FindAttributeMetadata(
  const Path: string): TInstantAttributeMetadata;

  function FindMetadata(AClass: TClass): TInstantAttributeMetadata; overload;
  begin
    if Assigned(AClass) and AClass.InheritsFrom(TInstantObject) then
    begin
      Result := GetAttributeMetadata(AClass, Path);
      if not Assigned(Result) then
        Result := FindMetadata(AClass.ClassParent);
    end else
      Result := nil;
  end;

  function FindMetadata(AClass: TInstantCodeClass): TInstantAttributeMetadata; overload;
  begin
    if Assigned(AClass) and AClass.DerivesFrom(TInstantObject.ClassName) then
    begin
      Result := GetAttributeMetadata(AClass, Path);
      if not Assigned(Result) then
        Result := FindMetadata(AClass.BaseClass);
    end else
      Result := nil;
  end;

begin
  if csDesigning in ComponentState then
    Result := FindMetadata(DesignClass)
  else
    Result := FindMetadata(ObjectClass);
end;

function TInstantCustomExposer.FindObjectBuffer(AObject: TObject): PChar;
var
  I: Integer;
  BM: TInstantBookmark;
begin
  for I := 0 to Pred(BufferCount) do
  begin
    Result := Buffers[I];
    GetBookmarkData(Result, @BM);
    if BM.Instance = AObject then
      Exit;
  end;
  Result := nil;
end;

procedure TInstantCustomExposer.FreeRecordBuffer(var Buffer: PChar);
begin
  StrDispose(Buffer);
  Buffer := nil;
end;

function TInstantCustomExposer.GetAccessor: TInstantAccessor;
begin
  if not Assigned(FAccessor) then
    FAccessor := CreateAccessor;
  Result := FAccessor;
end;

procedure TInstantCustomExposer.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(Buffer[FBookmarkOfs], Data^, BookmarkSize);
end;

function TInstantCustomExposer.GetBookmarkFlag(
  Buffer: PChar): TBookmarkFlag;
begin
  Result := GetRecInfo(Buffer).BookmarkFlag;
end;

function TInstantCustomExposer.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify and HasSubject and not ReadOnly;
end;

function TInstantCustomExposer.GetCurrentBuffer: PChar;
begin
  case State of
    dsCalcFields:
      Result := CalcBuffer;
    dsFilter:
      Result := FFilterBuffer;
  else
    Result := ActiveBuffer
  end;
end;

function TInstantCustomExposer.GetCurrentObject: TObject;
var
  BM: TInstantBookmark;
begin
  if not Active then
    Result := nil
  else if State = dsInsert then
  begin
    GetBookmarkData(CurrentBuffer, @BM);
    Result := BM.Instance;
  end else if not InContent then
    Result := Subject
  else if RecNo <= 0 then
    Result := nil
  else
    Result := Objects[Pred(RecNo)];
end;

function TInstantCustomExposer.GetDesignClass: TInstantCodeClass;
begin
  if Assigned(DesignModel) then
    Result := DesignModel^.FindClass(ObjectClassName)
  else
    Result := nil;
end;

function TInstantCustomExposer.GetFieldData(Field: TField;
  Buffer: Pointer): Boolean;
var
  D: TDateTimeRec;
begin
  if Assigned(Buffer) then
    Move(CurrentBuffer[GetFieldOffset(Field)], Buffer^, Field.DataSize);
  // Show null dates as blanks
  if (Field is TDateTimeField) and Assigned(Buffer) then
  begin
    D := TDateTimeRec(Buffer^);
    Result := (D.Date <> 0) and (D.Time <> 0);
  end else
    Result := (State in [dsEdit, dsInsert]) or (RecordCount > 0);
end;

function TInstantCustomExposer.GetFieldOffset(const Field: TField): Integer;
var
  I: Integer;
begin
  if Field.FieldNo < 0 then
    Result := RecordSize + Field.Offset
  else
  begin
    Result := 0;
    for I := 0 to Pred(Field.Index) do
      if not IsCalcField(Fields[I]) then
        Inc(Result, Fields[I].DataSize);
  end;
end;

function TInstantCustomExposer.GetFieldStrings(Field: TField;
  Strings: TStrings): Integer;
var
  PropInfo: PPropInfo;
  Value: Variant;
  I: Integer;
begin
  if Assigned(Field) then
  begin
    PropInfo := InstantGetPropInfo(ObjectClass, Field.FieldName);
    if PropInfo.PropType^.Kind = tkEnumeration then
    begin
      InstantGetEnumNames(PropInfo^.PropType^, Strings);
      for I := 0 to Pred(Strings.Count) do
      begin
        Value := Strings[I];
        DoTranslate(Field, Value, False);
        Strings[I] := Value;
      end;
      Result := Strings.IndexOf(Field.AsString);
      Exit;
    end else
      Strings.Clear;
  end;
  Result := -1;
end;

function TInstantCustomExposer.GetHasCurrentBuffer: Boolean;
begin
  Result := CurrentBuffer <> nil;
end;

function TInstantCustomExposer.GetHasSubject: Boolean;
begin
  Result := Assigned(Subject);
end;

function TInstantCustomExposer.GetInContent: Boolean;
begin
  Result := Accessor.InContent;
end;

function TInstantCustomExposer.GetIsChanged: Boolean;
begin
  Result := False;
end;

function TInstantCustomExposer.GetLimited: Boolean;
begin
  Result := Accessor.Limited;
end;

function TInstantCustomExposer.GetMode: TInstantAccessMode;
begin
  Result := Accessor.Mode;
end;

function TInstantCustomExposer.GetObjectClass: TClass;
begin
  Result := Accessor.ObjectClass;
end;

function TInstantCustomExposer.GetObjectClassName: string;
begin
  Result := Accessor.ObjectClassName;
end;

function TInstantCustomExposer.GetObjectCount: Integer;
begin
  if HasSubject then
    Result := Accessor.ObjectCount
  else
    Result := 0;
end;

function TInstantCustomExposer.GetObjects(Index: Integer): TObject;
begin
  Result := Accessor.Objects[Index];
end;

function TInstantCustomExposer.GetOnCompare: TInstantCompareObjectsEvent;
begin
  if HasAccessor then
    Result := Accessor.OnCompare
  else
    Result := FOnCompare;
end;

function TInstantCustomExposer.GetOnLimit: TInstantLimitObjectsEvent;
begin
  if HasAccessor then
    Result := Accessor.OnLimit
  else
    Result := FOnLimit;
end;

function TInstantCustomExposer.GetOnProgress: TInstantProgressEvent;
begin
  if HasAccessor then
    Result := Accessor.OnProgress
  else
    Result := FOnProgress;
end;

function TInstantCustomExposer.GetRecInfo(Buffer: PChar): PRecInfo;
begin
  Result := PRecInfo(Buffer + FRecInfoOfs);
end;

function TInstantCustomExposer.GetRecNo: Integer;
begin
  if (State <> dsFilter) and IsEmpty then
    Result := 0
  else
    Result := GetRecInfo(CurrentBuffer).RecordNumber;
end;

function TInstantCustomExposer.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin
  if Filtered and Assigned(OnFilterRecord) then
  begin
    Result := grOK;
    Accept := False;
    SaveState := SetTempState(dsFilter);
    try
      while not Accept do
      begin
        Result := InternalGetRecord(Buffer, GetMode, DoCheck);
        if Result <> grOK then
          Break;
        Accept := True;
        FFilterBuffer := Buffer;
        OnFilterRecord(Self, Accept);
        if not Accept and (GetMode = gmCurrent) then
          GetMode := gmPrior; 
      end;
    finally
      RestoreState(SaveState);
    end;
  end else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck);
end;

function TInstantCustomExposer.GetRecordCount: Longint;
begin
  Result := ObjectCount;
end;

function TInstantCustomExposer.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TInstantCustomExposer.GetSorted: Boolean;
begin
  if HasAccessor then
    Result := Accessor.Sorted
  else
    Result := FSorted;
end;

function TInstantCustomExposer.GetTotalCount: Integer;
begin
  Result := Accessor.TotalCount;
end;


procedure TInstantCustomExposer.GotoActiveRecord;
var
  BM: TInstantBookmark;
begin
  GetBookmarkData(ActiveBuffer, @BM);
  if BM.RecNo >= 0 then
    FRecNo := BM.RecNo
  else
    FRecNo := RecordCount;
end;

function TInstantCustomExposer.GotoObject(AObject: TObject): Boolean;
begin
  Result := Active and LocateObject(AObject);
  if not Result then
    FRecNo := 0;
  if Active then
    Refresh;
end;

procedure TInstantCustomExposer.GotoRecord(ARecNo: Integer);
var
  BM: TInstantBookmark;
begin
  if ARecNo <= 0 then
    Exit;
  FRecNo := ARecNo;
  GetBookmarkData(ActiveBuffer, @BM);
  BM.RecNo := FRecNo;
  BM.Instance := Accessor.Objects[Pred(FRecNo)];
  SetBookmarkData(ActiveBuffer, @BM);
  GetRecInfo(ActiveBuffer).RecordNumber := FRecNo;
  Resync([rmExact, rmCenter]);
  DoAfterScroll;
end;

function TInstantCustomExposer.HandleFieldError(E: Exception;
  Field: TField; Value: Variant; Write: Boolean): Boolean;
begin
  Result := False;
  if E is EAbort then
    Exit;
  if Assigned(FOnFieldError) then
    FOnFieldError(Self, E, Field, Value, Write, Result)
  else if Write then
    raise EInstantError.CreateFmt(SFieldWriteError,
      [VarToStr(Value), Field.FieldName, E.Message], E)
  else
    raise EInstantError.CreateFmt(SFieldReadError,
      [Field.FieldName, E.Message], E);
end;

function TInstantCustomExposer.HasAccessor: Boolean;
begin
  Result := Assigned(FAccessor);
end;

function TInstantCustomExposer.HasObjectClassName: Boolean;
begin
  Result := FObjectClassName <> '';
end;

function TInstantCustomExposer.HasPersistentObjectClass: Boolean;
begin
  if csDesigning in ComponentState then
    Result := DesignClass.DerivesFrom(TInstantObject.ClassName)
  else
    Result := ObjectClass.InheritsFrom(TInstantObject);
end;

function TInstantCustomExposer.IncludeField(FieldName: string;
  Default: Boolean): Boolean;
begin
  if DefaultFields or (csDesigning in ComponentState) or (Fields.Count = 0) then
    Result := Default
  else
    Result := Assigned(Fields.FindField(FieldName));
  DoIncludeField(FieldName, Result);
end;

function TInstantCustomExposer.IndexOfObject(AObject: TObject): Integer;
begin
  Result := Accessor.IndexOfObject(AObject);
end;

procedure TInstantCustomExposer.InitAccessor(AAccessor: TInstantAccessor);
begin
  with AAccessor do
  begin
    DisableChanges;
    try
      OnChange := AccessorChanged;
      OnCompare := Self.FOnCompare;
      OnLimit := Self.FOnLimit;
      OnProgress := Self.FOnProgress;
      ObjectClass := Self.FObjectClass;
      ObjectClassName := Self.FObjectClassName;
      ContainerName := Self.FContainerName;
      Mode := Self.FMode;
      Limited := Self.FLimited;
      Sorted := Self.FSorted;
    finally
      EnableChanges;
    end;
  end;
end;

procedure TInstantCustomExposer.InitBufferPointers;
begin
  FRecordSize := DataFieldsSize;
  FRecInfoOfs := FRecordSize + CalcFieldsSize;
  FBookmarkOfs := FRecInfoOfs + SizeOf(TRecInfo);
  FRecBufSize := FBookmarkOfs + BookmarkSize;
end;

procedure TInstantCustomExposer.InitField(Field: TField);
var
  Metadata: TInstantAttributeMetadata;
begin
  Metadata := FindAttributeMetadata(Field.FieldName);
  if Field.DataType = ftString then
  begin
    if Assigned(Metadata) and (Metadata.DisplayWidth <> 0) then
      Field.DisplayWidth := Metadata.DisplayWidth
    else
      Field.DisplayWidth := DefaultStringDisplayWidth;
  end
  else if Field.DataType = ftBCD then
  begin
    (Field as TBCDField).Currency := True;
  end;
  if Assigned(FOnInitField) then
    FOnInitField(Self, Field);
end;

procedure TInstantCustomExposer.InitFieldDef(FieldDef: TFieldDef);
var
  Metadata: TInstantAttributeMetadata;
begin
  if FieldDef.DataType = ftString then
  begin
    Metadata := FindAttributeMetadata(FieldDef.Name);
    if Assigned(Metadata) and (Metadata.Size <> 0) then
      FieldDef.Size := Metadata.Size
    else
      FieldDef.Size := DefaultStringFieldSize;
  end;
  if Assigned(FOnInitFieldDef) then
    FOnInitFieldDef(Self, FieldDef);
end;

procedure TInstantCustomExposer.InitFields;

  function IsRootField(Field: TField): Boolean;
  begin
    Result := Pos('.', Field.FieldName) = 0;
  end;

  function IsSimpleField(Field: TField): Boolean;
  begin
    Result := not (Field.IsBlob or (Field is TObjectField));
  end;

  function FlagInKey(AField: TField): Boolean;
  begin
    Result := Assigned(AField);
    if Result then
      AField.ProviderFlags := AField.ProviderFlags + [pfInKey];
  end;

var
  I: Integer;
  AddKeyFields: Boolean;
  Field: TField;
  Metadata: TInstantAttributeMetadata;
begin
  AddKeyFields := HasPersistentObjectClass;
  if AddKeyFields then
  begin
    AddKeyFields := FlagInKey(FindField(InstantIdPropName));
    AddKeyFields := not (FlagInKey(FindField(InstantClassIdPropName)) and
      AddKeyFields);
  end;
  for I := 0 to Pred(FieldCount) do
  begin
    Field := Fields[I];
    if AddKeyFields and IsRootField(Field) and IsSimpleField(Field) then
    begin
      Metadata := FindAttributeMetadata(Field.FieldName);
      if Assigned(Metadata) and Metadata.IsIndexed then
        FlagInKey(Field);
    end;
    InitField(Field);
  end;
end;

procedure TInstantCustomExposer.InitRecord(Buffer: PChar);
var
  BM: TInstantBookmark;
begin
  ClearData(Buffer);
  inherited;
  with GetRecInfo(Buffer)^ do
  begin
    RecordNumber := -1;
    UpdateStatus := usInserted;
    BookMarkFlag := bfInserted;
  end;
  BM.RecNo := -1;
  BM.Instance := nil;
  SetBookmarkData(Buffer, @BM);
end;

procedure TInstantCustomExposer.InsertObject(AObject: TObject);
begin
  InternalInsertObject(RecNo, AObject);
  GotoObject(AObject);
end;

function TInstantCustomExposer.InternalAddObject(AObject: TObject): Integer;
begin
  Result := Accessor.AddObject(AObject);
  if Result <> -1 then
    FRecNo := Succ(Result);
end;

procedure TInstantCustomExposer.InternalAddRecord(Buffer: Pointer;
  Append: Boolean);
begin
  AddNewObject(Buffer, Append);
  GetRecInfo(Buffer)^.RecordNumber := FRecNo;
end;

procedure TInstantCustomExposer.InternalCancel;
begin
  inherited;
  if not (eoDeferInsert in Options) and (State = dsInsert)
    and Assigned(FNewObject) then
  begin
    GotoActiveRecord;
    InternalRemoveObject(FNewObject);
  end;
  if (State = dsEdit) and (CurrentObject is TInstantObject) then
    TInstantObject(CurrentObject).IsChanged := False;
  if ObjectCount = 0 then
    ClearRecord(ActiveBuffer);
end;

procedure TInstantCustomExposer.InternalClose;
begin
  FIsOpen := False;
  if DefaultFields then
    DestroyFields;
  FreeRecordBuffer(FRevertBuffer);
  FreeRecordBuffer(FUndoBuffer);
end;

procedure TInstantCustomExposer.InternalDelete;
var
  AObject: TObject;
  IsInstantObject: Boolean;
begin
  AObject := CurrentObject;
  IsInstantObject := AObject is TInstantObject;
  if IsInstantObject then
    TInstantObject(AObject).AddRef;
  try
    AutoDispose(AObject);
    InternalRemoveObject(AObject);
  finally
    if IsInstantObject then
      TInstantObject(AObject).Free;
  end;
  ClearRecord(ActiveBuffer);
  if FRecNo > RecordCount then
    FRecNo := RecordCount;
end;

procedure TInstantCustomExposer.InternalFirst;
begin
  FRecNo := 0;
end;

function TInstantCustomExposer.InternalGetRecord(Buffer: PChar;
  GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  case GetMode of
    gmNext:
      Inc(FRecNo);
    gmPrior:
      Dec(FRecNo);
  end;
  if FRecNo < 1 then
  begin
    Result := grBOF;
    FRecNo := 0;
  end else if FRecNo > RecordCount then
  begin
    Result := grEOF;
    if RecordCount = 0 then
      FRecNo := 0
    else
      FRecNo := Succ(RecordCount);
  end else if (State = dsInsert) and (RecordCount = 1) then
    case GetMode of
      gmPrior:
        Result := grBOF;
      gmNext:
        Result := grEOF;
    else
      Result := grOK;
    end
  else
    Result := grOK;
  LoadRecord(FRecNo, Buffer);
end;

procedure TInstantCustomExposer.InternalGotoBookmark(Bookmark: Pointer);
var
  BM: TInstantBookmark;
begin
  BM := TInstantBookmark(Bookmark^);
  UpdateBookmark(BM);
  if (BM.RecNo > 0) and (BM.RecNo <= RecordCount) then
    FRecNo := BM.RecNo;
end;

procedure TInstantCustomExposer.InternalHandleException;
begin
end;

procedure TInstantCustomExposer.InternalInitFieldDefs;

  procedure SortFieldDefs;
  var
    I: Integer;
    FieldDef: TFieldDef;
  begin
    with TStringList.Create do
    try
      while FieldDefs.Count > 0 do
      begin
        FieldDef := FieldDefs[0];
        AddObject(FieldDef.Name, FieldDef);
        FieldDef.Collection := nil;
      end;
      Sorted := True;
      for I := 0 to Pred(Count) do
      begin
        FieldDef := TFieldDef(Objects[I]);
        FieldDef.Collection := FieldDefs;
      end;
    finally
      Free;
    end;
  end;

begin
  FieldDefs.Clear;
  if not (csDesigning in ComponentState) and (FieldCount > 0) then
  begin
    InitFieldDefsFromFields;
    Exit;
  end;
  if foObjects in FieldOptions then
    FieldDefs.Add(SelfFieldName, ftInteger);
  if csDesigning in ComponentState then
    AddClassFieldDefs('', DesignClass)
  else
    AddClassFieldDefs('', ObjectClass);
  SortFieldDefs;
end;

procedure TInstantCustomExposer.InternalInitRecord(Buffer: PChar);
begin
end;

procedure TInstantCustomExposer.InternalInsert;
begin
  inherited;
  FRemovedObject := nil;
  FNewObject := AddNewObject(ActiveBuffer,
    GetBookmarkFlag(ActiveBuffer) = bfEOF);
end;

procedure TInstantCustomExposer.InternalInsertObject(ARecNo: Integer;
  AObject: TObject);
var
  Index: Integer;
begin
  Index := Pred(ARecNo);
  if Index >= 0 then
  begin
    Index := Accessor.InsertObject(Index, AObject);
    if Index <> -1 then
      FRecNo := Succ(Index);
  end;
end;

procedure TInstantCustomExposer.InternalLast;
begin
  FRecNo := RecordCount + 1;
end;

procedure TInstantCustomExposer.InternalOpen;
begin
  FRecNo := 0;
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(True);
  InitBufferPointers;
  FRevertBuffer := AllocRecordBuffer;
  FUndoBuffer := AllocRecordBuffer;
  FIsOpen := True;
end;

procedure TInstantCustomExposer.InternalPost;
var
  AObject: TObject;
  NewPos: Integer;
begin
  AObject := CurrentObject;
  CopyBufferToObject(ActiveBuffer, AObject);
  AutoStore(AObject);
  if (State = dsInsert) and (eoDeferInsert in Options) then
    PutObject(ActiveBuffer, AObject, False);
  NewPos := Accessor.RepositionObject(AObject);
  if NewPos >= 0 then
    FRecNo := Succ(NewPos);
end;

procedure TInstantCustomExposer.InternalRefresh;
begin
  if (ObjectCount = 0) and (BufferCount > 0) then
    ClearRecord(Buffers[0]);
  inherited;
end;

procedure TInstantCustomExposer.InternalReleaseObject(AObject: TObject);
begin
  Accessor.ReleaseObject(AObject);
end;

function TInstantCustomExposer.InternalRemoveObject(AObject: TObject): Integer;
begin
  with Accessor do
  begin
    DisableChanges;
    try
      Result := RemoveObject(AObject);
      FRemovedObject := AObject;
    finally
      EnableChanges;
    end;
  end;
end;

procedure TInstantCustomExposer.InternalReset;
var
  WasActive: Boolean;
begin
  WasActive := Active;
  Close;
  try
    FieldDefs.Clear;
    FieldDefs.Updated := False;
    Accessor.Reset;
  finally
    if WasActive and (HasSubject or IsNested) then
      Open;
  end;
end;

procedure TInstantCustomExposer.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(Buffer + FBookmarkOfs);
end;

function TInstantCustomExposer.IsCalcField(Field: TField): Boolean;
begin
  Result := Assigned(Field) and
    (Field.FieldKind in [fkLookup, fkCalculated]);
end;

function TInstantCustomExposer.IsCursorOpen: Boolean;
begin
  Result := FIsOpen;
end;

function TInstantCustomExposer.IsNested: Boolean;
begin
  Result := Assigned(DataSetField);
end;

function TInstantCustomExposer.IsSelfField(Field: TField): Boolean;
begin
  Result := Field.FieldName = SelfFieldName;
end;

procedure TInstantCustomExposer.LoadCurrentObject(Buffer: PChar);
begin
  CopyBufferToObject(Buffer, CurrentObject);
end;

procedure TInstantCustomExposer.LoadField(Obj: TObject; Field: TField);
begin
  LoadFieldParams(Obj, Field);
  LoadFieldValue(Field, @CurrentBuffer[GetFieldOffset(Field)], Obj);
  DataEvent(deFieldChange, Longint(Field));
end;

procedure TInstantCustomExposer.LoadFieldParams(AObject: TObject;
  Field: TField);
var
  Metadata: TInstantAttributeMetadata;
begin
  if AObject is TInstantObject then
  begin
    Metadata := GetAttributeMetadata(AObject.ClassType, Field.FieldName,
      @AObject);
    if Assigned(Metadata) then
    begin
      if Field.EditMask <> Metadata.EditMask then
        Field.EditMask := Metadata.EditMask;
      if Metadata.ValidChars <> [] then
        Field.ValidChars := Metadata.ValidChars;
    end;
  end;
end;

procedure TInstantCustomExposer.LoadFields;
var
  I: Integer;
begin
  for I := 0 to Pred(Fields.Count) do
    LoadField(CurrentObject, Fields[I]);
  UpdateCalcFields;
end;

procedure TInstantCustomExposer.LoadFieldValue(Field: TField;
  Buffer: Pointer; AObject: TObject);
var
  Value: Variant;
  Empty: Boolean;
  Len: Integer;
  S: string;
  N: Integer;
  F: Double;
  C: Currency;
  Bcd : TBcd;
  D: TDateTimeRec;
  T: TTimeStamp;
  L: WordBool;
begin
  if not Assigned(AObject) or IsCalcField(Field) then
    Exit;
  try
    if IsSelfField(Field) then
      Value := Integer(AObject)
    else
      Value := ReadProperty(Field, AObject);
  except
    on E: Exception do
      if not HandleFieldError(E, Field, Null, False) then
        raise;
  end;
  Empty := VarIsEmpty(Value) or (Value = Null);
  case Field.DataType of
    ftString:
      begin
        FillChar(Buffer^, Field.DataSize, 0);
        if not Empty then
        begin
          S := Value;
          Len := Length(S);
          if Len >= Field.DataSize then
            Len := Pred(Field.DataSize);
          if Len > 0 then
            Move(S[1], Buffer^, Len);
        end;
      end;
    ftInteger:
      begin
        if Empty then
          N := 0 else
          N := Value;
        Move(N, Buffer^, SizeOf(N));
      end;
    ftFloat:
      begin
        if Empty then
          F := 0 else
          F := Value;
        Move(F, Buffer^, SizeOf(F));
      end;
    ftBCD:
      begin
        if Empty then
          C := 0 else
          C := Value;
        CurrToBCD(C,Bcd);
        Move(Bcd, Buffer^, SizeOf(Bcd));
      end;
    ftDate:
      begin
        if Empty or (Value = 0) then
          D.Date := 0 else
          D.Date := Value + DateDelta;
        Move(D, Buffer^, Field.DataSize);
      end;
    ftTime:
      begin
        if Empty or (Value = 0) then
          D.Time := 0 else
          D.Time := Value * MSecsPerDay;
        Move(D, Buffer^, Field.DataSize);
      end;
    ftDateTime:
      begin
        if Empty or (Value = 0) then
          D.DateTime := 0
        else begin
          T := DateTimeToTimeStamp(Value);
          D.DateTime := TimeStampToMSecs(T);
        end;
        Move(D, Buffer^, Field.DataSize);
      end;
    ftBoolean:
      begin
        if Empty then
          L := False else
          L := Value;
        Move(L, Buffer^, SizeOf(L));
      end;
  end;
end;

procedure TInstantCustomExposer.LoadRecord(RecNo: Integer; Buffer: PChar);
var
  BM: TInstantBookmark;
  Obj: TObject;
begin
  if (RecNo <= 0) or (RecNo > RecordCount) then
    ClearRecord(Buffer)
  else begin
    Obj := Objects[Pred(RecNo)];
    CopyObjectToBuffer(Obj, Buffer);
    with GetRecInfo(Buffer)^ do
    begin
      UpdateStatus := usUnmodified;
      BookmarkFlag := bfCurrent;
      RecordNumber := RecNo;
    end;
    BM.RecNo := RecNo;
    BM.Instance := Obj;
    SetBookmarkData(Buffer, @BM);
  end;
end;

function TInstantCustomExposer.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  CompareOptions: TInstantCompareOptions;
begin
  DoBeforeScroll;
  CompareOptions := [];
  if loCaseInsensitive in Options then
    Include(CompareOptions, coCaseInsensitive);
  if loPartialKey in Options then
    Include(CompareOptions, coPartial);
  Result := LocateObject(KeyFields, KeyValues, CompareOptions);
end;

function TInstantCustomExposer.LocateObject(const KeyFields: string;
  const KeyValues: Variant; Options: TInstantCompareOptions): Boolean;
var
  Index: Integer;
begin
  if HasSubject then
  begin
    Index := Accessor.FindObject(KeyFields, KeyValues, Options);
    Result := Index <> -1;
    if Result then
      GotoRecord(Succ(Index));
  end else
    Result := False;
end;

function TInstantCustomExposer.LocateObject(AObject: TObject): Boolean;
var
  Index: Integer;
begin
  Index := IndexOfObject(AObject);
  Result := Index <> -1;
  if Result then
    GotoRecord(Succ(Index));
end;

function TInstantCustomExposer.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  if LocateObject(KeyFields, KeyValues, []) then
    Result := FieldValues[ResultFields]
  else
    Result := Null;
end;

procedure TInstantCustomExposer.ObjectChanged(AObject: TInstantObject);
begin
  RefreshObjectBuffer(AObject);
end;

procedure TInstantCustomExposer.PostChanges;
var
  I: Integer;
  List: TList;
  AObject: TInstantObject;
begin
  List := TList.Create;
  try
    GetDetailDataSets(List);
    for I := 0 to Pred(List.Count) do
      if TDataSet(List[I]) is TInstantCustomExposer then
        TInstantCustomExposer(List[I]).PostChanges;
  finally
    List.Free;
  end;
  if State in [dsEdit, dsInsert] then
    Post
  else if CurrentObject is TInstantObject then
  begin
    AObject := TInstantObject(CurrentObject);
    if AObject.IsChanged then
      AutoStore(AObject);
  end;
end;

procedure TInstantCustomExposer.PostField(Field: TField);
begin
  if IsCalcField(Field) then
    Exit;
  DoBeforePostField(Field);
  SaveField(Field);
  LoadFields;
  DoAfterPostField(Field);
end;

procedure TInstantCustomExposer.PSGetAttributes(List: TList);
begin
end;

function TInstantCustomExposer.PSGetTableName: string;
begin
  Result := ObjectClassName;
end;

procedure TInstantCustomExposer.PSReset;
begin
  Reset;
end;

procedure TInstantCustomExposer.PutObject(Buffer: PChar; AObject: TObject;
  Append: Boolean);
var
  BM: TInstantBookmark;
begin
  DisableChanges;
  try
    UpdateCursorPos;
    if Append  or (RecordCount = 0) or (FRecNo > RecordCount) then
      InternalAddObject(AObject)
    else begin
      GetBookmarkData(Buffer, @BM);
      InternalInsertObject(BM.RecNo, AObject);
    end;
    CopyObjectToBuffer(AObject, Buffer);
  finally
    EnableChanges;
  end;
end;

function TInstantCustomExposer.ReadProperty(Field: TField;
  Instance: TObject): Variant;
begin
  Result := InstantGetProperty(Instance, Field.FieldName);
  DoTranslate(Field, Result, False);
end;

procedure TInstantCustomExposer.RefreshCurrentObject;
begin
  if CurrentObject is TInstantObject then
    TInstantObject(CurrentObject).Refresh;
end;

procedure TInstantCustomExposer.RefreshData;
begin
  Accessor.Refresh;
end;

procedure TInstantCustomExposer.RefreshDataView;
begin
  Accessor.RefreshView;
end;

function TInstantCustomExposer.RefreshObjectBuffer(AObject: TObject): Boolean;
var
  Buffer: PChar;
  Editing: Boolean;
begin
  if not Active then
  begin
    Result := False;
    Exit;
  end;
  if InContent and (AObject = Subject) then
  begin
    Result := State = dsBrowse;
    if Result then
      RefreshData;
  end else
  begin
    Buffer := FindObjectBuffer(AObject);
    if Assigned(Buffer) then
    begin
      Editing := (Buffer = ActiveBuffer) and (State in dsEditModes);
      Result := (State = dsBrowse) or
        (Editing and (eoSyncEdit in Options));
      if Result then
      begin
        CopyObjectToBuffer(AObject, Buffer);
        if Editing then
          SetModified(True);
        DataEvent(deDataSetChange, 0);
      end;
    end else
      Result := False;
  end;
end;

procedure TInstantCustomExposer.ReleaseObject(AObject: TObject);
begin
  InternalReleaseObject(AObject);
end;

procedure TInstantCustomExposer.Remember;
begin
  SaveCurrentObject(FRevertBuffer);
end;

function TInstantCustomExposer.RemoveObject(AObject: TObject): Integer;
begin
  Result := InternalRemoveObject(AObject);
  if Result <> -1 then
    Refresh;
end;

procedure TInstantCustomExposer.Reset;
begin
  DisableControls;
  try
    InternalReset;
  finally
    EnableControls;
  end;
end;

procedure TInstantCustomExposer.Revert;
begin
  LoadCurrentObject(FRevertBuffer);
end;

procedure TInstantCustomExposer.SaveCurrentObject(Buffer: PChar);
begin
  CopyObjectToBuffer(CurrentObject, Buffer);
end;

procedure TInstantCustomExposer.SaveField(Field: TField);
begin
  SaveFieldValue(Field, @CurrentBuffer[GetFieldOffset(Field)], CurrentObject);
end;

procedure TInstantCustomExposer.SaveFieldValue(Field: TField;
  Buffer: Pointer; AObject: TObject);
var
  P: PChar;
  S: string;
  N: Integer;
  F: Double;
  C: Currency;
  Bcd : TBcd;
  D: TDateTimeRec;
  T: TTimeStamp;
  L: WordBool;
  Value: Variant;
begin
  if not Assigned(AObject) or IsSelfField(Field) or IsCalcField(Field) then
    Exit;
  case Field.DataType of
    ftString:
      begin
        P := StrAlloc(Field.DataSize);
        try
          StrCopy(P, Buffer);
          S := P;
          Value := S;
        finally
          StrDispose(P);
        end;
      end;
    ftInteger:
      begin
        Move(Buffer^, N, SizeOf(N));
        Value := N;
      end;
    ftFloat:
      begin
        Move(Buffer^, F, SizeOf(F));
        Value := F;
      end;
    ftBCD:
      begin
        Move(Buffer^, Bcd, SizeOf(Bcd));
        BCDToCurr(Bcd,C);
        Value := C;
      end;
    ftDate:
      begin
        Move(Buffer^, D, Field.DataSize);
        if D.Date = 0 then
          Value := 0
        else
          Value := D.Date - DateDelta;
      end;
    ftTime:
      begin
        Move(Buffer^, D, Field.DataSize);
        Value := D.Time / MSecsPerDay;
      end;
    ftDateTime:
      begin
        Move(Buffer^, D, Field.DataSize);
        if (D.Date = 0) and (D.Time = 0) then
          Value := 0
        else begin
          T := MSecsToTimeStamp(D.DateTime);
          Value := TimeStampToDateTime(T);
        end;
      end;
    ftBoolean:
      begin
        Move(Buffer^, L, SizeOf(L));
        Value := GetEnumName(TypeInfo(Boolean), Integer(L));
      end;
    ftDataSet, ftBlob, ftGraphic, ftMemo:
      Exit;
  end;
  try
    WriteProperty(Field, AObject, Value);
  except
    on E: Exception do
      if not HandleFieldError(E, Field, Value, True) then
        raise;
  end;
end;

procedure TInstantCustomExposer.SetActive(Value: Boolean);
begin
  if Active and not Value then
    Undo;
  inherited;
end;

procedure TInstantCustomExposer.SetBookmarkData(Buffer: PChar;
  Data: Pointer);
begin
  Move(Data^, Buffer[FBookmarkOfs], BookmarkSize);
end;

procedure TInstantCustomExposer.SetBookmarkFlag(Buffer: PChar;
  Value: TBookmarkFlag);
begin
  GetRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TInstantCustomExposer.SetContainerName(const Value: string);
begin
  if Value <> ContainerName then
  begin
    FContainerName := Value;
    if HasAccessor then
      Accessor.ContainerName := FContainerName;
  end;
end;

procedure TInstantCustomExposer.SetFieldData(Field: TField; Buffer: Pointer);
begin
  if Assigned(Buffer) then
    Move(Buffer^, CurrentBuffer[GetFieldOffset(Field)], Field.DataSize)
  else
    FillChar(CurrentBuffer[GetFieldOffset(Field)], Field.DataSize, 0);
  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
  begin
    PostField(Field);
    DataEvent(deFieldChange, Longint(Field));
  end;
end;

procedure TInstantCustomExposer.SetFieldOptions(Value: TInstantFieldOptions);
begin
  if Value <> FFieldOptions then
  begin
    FFieldOptions := Value;
    Reset;
  end;
end;

procedure TInstantCustomExposer.SetFiltered(Value: Boolean);
begin
  if Value <> Filtered then
  begin
    inherited;
    if Filtered then
      First
    else
      Refresh;
  end;
end;

procedure TInstantCustomExposer.SetLimited(Value: Boolean);
begin
  if Value <> Limited then
  begin
    FLimited := Value;
    if HasAccessor then
      Accessor.Limited := FLimited;
  end;
end;

procedure TInstantCustomExposer.SetMode(Value: TInstantAccessMode);
begin
  if Value <> Mode then
  begin
    FMode := Value;
    if HasAccessor then
      Accessor.Mode := FMode;
  end;
end;

procedure TInstantCustomExposer.SetObjectClass(Value: TClass);
begin
  if Value <> ObjectClass then
  begin
    FObjectClass := Value;
    if HasAccessor then
      Accessor.ObjectClass := FObjectClass;
  end;
end;

procedure TInstantCustomExposer.SetObjectClassName(const Value: string);
begin
  if Value <> ObjectClassName then
  begin
    FObjectClassName := Value;
    if HasAccessor then
      Accessor.ObjectClassName := FObjectClassName;
  end;
end;

procedure TInstantCustomExposer.SetOnCompare(
  Value: TInstantCompareObjectsEvent);
begin
  if @Value <> @OnCompare then
  begin
    FOnCompare := Value;
    if HasAccessor then
      Accessor.OnCompare := FOnCompare;
  end;
end;

procedure TInstantCustomExposer.SetOnLimit(Value: TInstantLimitObjectsEvent);
begin
  if @Value <> @OnLimit then
  begin
    FOnLimit := Value;
    if HasAccessor then
      Accessor.OnLimit := FOnLimit;
  end;
end;

procedure TInstantCustomExposer.SetOnProgress(
  const Value: TInstantProgressEvent);
begin
  if @Value <> @OnProgress then
  begin
    FOnProgress := Value;
    if HasAccessor then
      Accessor.OnProgress := FOnProgress;
  end;
end;

procedure TInstantCustomExposer.SetRecNo(Value: Integer);
begin
  if FRecNo <> Value then
  begin
    DoBeforeScroll;
    FRecNo := Value;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TInstantCustomExposer.SetSorted(Value: Boolean);
begin
  if Value <> Sorted then
  begin
    FSorted := Value;
    if HasAccessor then
      Accessor.Sorted := FSorted;
  end;
end;

procedure TInstantCustomExposer.Undo;
var
  I: Integer;
  List: TList;
begin
  if (State = dsEdit) and Modified then
  begin
    List := TList.Create;
    try
      GetDetailDataSets(List);
      for I := 0 to Pred(List.Count) do
        if TDataSet(List[I]) is TInstantCustomExposer then
          TInstantCustomExposer(List[I]).Undo;
    finally
      List.Free;
    end;
    LoadCurrentObject(FUndoBuffer);
  end;
end;

procedure TInstantCustomExposer.UpdateBookmark(var BM: TInstantBookmark);
begin
  if (RecordCount = 0) or (BM.Instance = FRemovedObject) then
  begin
    BM.RecNo := -1;
    BM.Instance := nil;
  end else if (BM.RecNo < 1) or (BM.RecNo > RecordCount)
    or (Objects[Pred(BM.RecNo)] <> BM.Instance) then
    BM.RecNo := Succ(Accessor.IndexOfInstance(BM.Instance));
end;

procedure TInstantCustomExposer.UpdateCalcFields;
begin
  GetCalcFields(ActiveBuffer);
end;

procedure TInstantCustomExposer.WriteProperty(Field: TField;
  Instance: TObject; Value: Variant);
begin
  DoTranslate(Field, Value, True);
  InstantSetProperty(Instance, Field.FieldName, Value);
end;

{ TInstantExposerLink }

procedure TInstantExposerLink.ActiveChanged;
begin
  DoMasterChange;
end;

constructor TInstantExposerLink.Create(ADetailExposer: TInstantCustomExposer);
begin
  inherited Create;
  FDetailExposer := ADetailExposer;
end;

procedure TInstantExposerLink.DataEvent(Event: TDataEvent; Info: Integer);
begin
  inherited;
  if not DetailExposer.Active and (Event = deRecordChange) then
    RecordChanged(nil);
end;

procedure TInstantExposerLink.DoMasterChange;
begin
  if Assigned(FOnMasterChange) then
    FOnMasterChange(Self);
end;

procedure TInstantExposerLink.DoMasterDisable;
begin
  if Assigned(FOnMasterDisable) then
    FOnMasterDisable(Self);
end;

function TInstantExposerLink.GetDetailDataSet: TDataSet;
begin
  Result := FDetailExposer;
end;

function TInstantExposerLink.GetMasterExposer: TInstantCustomExposer;
begin
  if Assigned(DataSet) and not (DataSet is TInstantCustomExposer) then
    raise EInstantError.CreateFmt(SInvalidMasterDataSetClass,
      [DataSet.ClassName, TInstantCustomExposer.ClassName]);
  Result := DataSet as TInstantCustomExposer;
end;

function TInstantExposerLink.GetSubject: TObject;
var
  Value: Variant;
begin
  if Assigned(MasterExposer) then
  begin
    Result := MasterExposer.CurrentObject;
    if Assigned(Result) and (MasterProperty <> '') then
    begin
      Value := InstantGetProperty(Result, MasterProperty);
      if VarType(Value) = VarInteger then
        Result := TObject(Integer(Value))
      else
        raise EInstantError.CreateFmt(SNotPublishedObjectProperty,
          [MasterProperty]);
    end;
  end else
    Result := nil;
end;

procedure TInstantExposerLink.RecordChanged(Field: TField);
begin
  DoMasterChange;
end;

{ TInstantExposer }

procedure TInstantExposer.DataEvent(Event: TDataEvent;
  Info: Integer);
begin
  case Event of
    deParentScroll:
      SyncWithParent(DataSetField);
  end;
  inherited;
end;

destructor TInstantExposer.Destroy;
begin
  FMasterLink.Free;
  inherited;
end;

function TInstantExposer.GetMasterLink: TInstantExposerLink;
begin
  if not Assigned(FMasterLink) then
  begin
    FMasterLink := TInstantExposerLink.Create(Self);
    FMasterLink.OnMasterChange := MasterChanged;
    FMasterLink.OnMasterDisable := MasterDisabled;
  end;
  Result := FMasterLink;
end;

function TInstantExposer.GetMasterProperty: string;
begin
  Result := MasterLink.MasterProperty;
end;

function TInstantExposer.GetMasterSource: TDataSource;
begin
  Result := MasterLink.DataSource;
end;

function TInstantExposer.GetIsChanged: Boolean;
begin
  if HasSubject and (Subject is TInstantObject) then
    Result := TInstantObject(Subject).IsChanged
  else
    Result := False;
end;

function TInstantExposer.GetSubject: TObject;
begin
  Result := FSubject;
end;

procedure TInstantExposer.MasterChanged(Sender: TObject);
begin
  if MasterLink.Active then
  begin
    Subject := MasterLink.Subject;
    Open;
  end
  else
    Close;
end;

procedure TInstantExposer.MasterDisabled(Sender: TObject);
begin
  Subject := nil;
end;

procedure TInstantExposer.SetDataSetField(const Value: TDataSetField);
begin
  if Value <> DataSetField then
    SyncWithParent(Value);
  inherited;
end;

procedure TInstantExposer.SetMasterProperty(const Value: string);
begin
  if MasterProperty <> Value then
  begin
    MasterLink.MasterProperty := Value;
    Reset;
  end;
end;

procedure TInstantExposer.SetMasterSource(Value: TDataSource);
begin
  if MasterSource <> Value then
  begin
    MasterLink.DataSource := Value;
    Reset;
  end;
end;

procedure TInstantExposer.SetSubject(Value: TObject);
begin
  if Value <> Subject then
  begin
    Undo;
    FSubject := Value;
    DestroyAccessor;
    DisableControls;
    try
      SubjectChanged;
    finally
      EnableControls;
    end;
  end;
end;

procedure TInstantExposer.SubjectChanged;
begin
  if Active then
    Reset
  else
    Open;
end;

procedure TInstantExposer.SyncWithParent(Field: TDataSetField);
var
  Master: TInstantCustomExposer;
  PropPath: string;
  AObject: TObject;
  Attr: TInstantAttributeMetadata;
  I: Integer;
begin
  if Assigned(Field) and (Field.DataSet is TInstantCustomExposer) then
  begin
    Master := TInstantCustomExposer(Field.DataSet);
    Mode := amContent;
    Options := Master.Options;
    FieldOptions := Master.FieldOptions;
    AObject := Master.CurrentObject;
    with Field do
    begin
      I := InstantRightPos('.', FieldName);
      if I > 0 then
      begin
        PropPath := Copy(FieldName, 1, I - 1);
        AObject := TObject(Integer(InstantGetProperty(AObject, PropPath)));
        ContainerName := Copy(FieldName, I + 1, Length(FieldName) - I)
      end else
        ContainerName := FieldName;
    end;
    Subject := AObject;
    if not Assigned(AObject) then
    begin
      Attr := GetAttributeMetadata(Master.ObjectClass, Field.FieldName);
      if Assigned(Attr) then
        ObjectClass := Attr.ObjectClass;
    end;
  end;
end;

{ TInstantQueryAccessor }

function TInstantQueryAccessor.GetConnector: TInstantConnector;
begin
  Result := Subject.Connector;
end;

function TInstantQueryAccessor.GetMode: TInstantAccessMode;
begin
  Result := amContent;
end;

function TInstantQueryAccessor.GetSubject: TInstantQuery;
begin
  Result := inherited Subject as TInstantQuery;
end;

function TInstantQueryAccessor.InternalAddObject(
  AObject: TObject): Integer;
begin
  Result := Subject.AddObject(AObject);
end;

procedure TInstantQueryAccessor.InternalApplyChanges;
begin
  Subject.ApplyChanges;
end;

function TInstantQueryAccessor.InternalGetObjectClassName: string;
begin
  Result := Subject.ObjectClassName;
end;

function TInstantQueryAccessor.InternalGetObjectCount: Integer;
begin
  Result := Subject.ObjectCount;
end;

function TInstantQueryAccessor.InternalGetObjects(Index: Integer): TObject;
begin
  Result := Subject.Objects[Index];
end;

function TInstantQueryAccessor.InternalIndexOfObject(
  AObject: TObject): Integer;
begin
  Result := Subject.IndexOfObject(AObject);
end;

function TInstantQueryAccessor.InternalInsertObject(Index: Integer;
  AObject: TObject): Integer;
begin
  Subject.InsertObject(Index, AObject);
  Result := Index;
end;

procedure TInstantQueryAccessor.InternalRefreshObjects;
begin
  Subject.RefreshObjects;
end;

procedure TInstantQueryAccessor.InternalReleaseObject(AObject: TObject);
begin
  Subject.ReleaseObject(AObject);
end;

function TInstantQueryAccessor.InternalRemoveObject(
  AObject: TObject): Integer;
begin
  Result := Subject.RemoveObject(AObject);
end;

class function TInstantQueryAccessor.SubjectClass: TClass;
begin
  Result := TInstantQuery;
end;

{ TInstantSelector }

function TInstantSelector.CanAutoOpen: Boolean;
begin
  Result := FAutoOpen and HasConnector;
end;

procedure TInstantSelector.CommandChanged(Sender: TObject);
begin
  if csReading in ComponentState then
    Exit;
  Query.Command := Command.Text;
  if ParamCheck or (csDesigning in ComponentState) then
    UpdateParams;
  Accessor.ChangedClass;
  Reset;
end;

constructor TInstantSelector.Create(AOwner: TComponent);
begin
  inherited;
  NestedDataSetClass := TInstantExposer;
  ParamCheck := True;
end;

procedure TInstantSelector.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not Params.IsEqual(TInstantSelector(Filer.Ancestor).Params)
    else
      Result := Params.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

destructor TInstantSelector.Destroy;
begin
  inherited;
  DestroyQuery;
  FParams.Free;
  FCommand.Free;
end;

procedure TInstantSelector.DestroyQuery;
begin
  DestroyAccessor;
  FreeAndNil(FQuery);
end;

function TInstantSelector.GetCommand: TStringList;
begin
  if not Assigned(FCommand) then
  begin
    FCommand := TStringList.Create;
    FCommand.OnChange := CommandChanged;
  end;
  Result := FCommand;
end;

function TInstantSelector.GetConnector: TInstantConnector;
begin
  if Assigned(FConnector) then
    Result := FConnector
  else
    Result := InstantDefaultConnector;
end;

function TInstantSelector.GetParams: TParams;
begin
  if not Assigned(FParams) then
    FParams := TParams.Create(Self);
  Result := FParams;
end;

function TInstantSelector.GetQuery: TInstantQuery;
begin
  if not Assigned(FQuery) then
  begin
    if (csDesigning in ComponentState) or not HasConnector then
      FQuery := TInstantQuery.Create(nil)
    else
      FQuery := Connector.CreateQuery;
    FQuery.MaxCount := MaxCount;
    FQuery.Command := Command.Text;
  end;
  Result := FQuery;
end;

function TInstantSelector.GetIsChanged: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred(Query.ObjectCount) do
    if Query.Objects[I] is TInstantObject then
    begin
      Result := TInstantObject(Query.Objects[I]).IsChanged;
      if Result then
        Break;
    end;
  { TODO : Implement check of deleted records after patch bug #1232576 }
end;

function TInstantSelector.GetSubject: TObject;
begin
  Result := Query;
end;

function TInstantSelector.HasCommand: Boolean;
begin
  Result := Command.Count > 0;
end;

function TInstantSelector.HasConnector: Boolean;
begin
  Result := Assigned(Connector);
end;

procedure TInstantSelector.InternalClose;
begin
  inherited;
  DestroyQuery;
end;

procedure TInstantSelector.InternalOpen;
begin
  if Query.ClassType = TInstantQuery then
  begin
    if not HasConnector then
      raise EInstantError.Create(SUnassignedConnector);
    DestroyQuery;
  end;
  Query.Params := Params;
  Query.Open;
  inherited;
end;

procedure TInstantSelector.Loaded;
begin
  inherited;
  if HasCommand then
    CommandChanged(Self);
  if not (csDesigning in ComponentState) and CanAutoOpen then
    Open;
end;

procedure TInstantSelector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Connector) then
  begin
    Close;
    FConnector := nil;
  end;
end;

procedure TInstantSelector.PSEndTransaction(Commit: Boolean);
begin
  if HasConnector then
    if Commit then
      Connector.CommitTransaction
    else
      Connector.RollbackTransaction;
end;

function TInstantSelector.PSGetParams: TParams;
begin
  Result := Params;
end;

function TInstantSelector.PSInTransaction: Boolean;
begin
  Result := HasConnector and Connector.InTransaction;
end;

procedure TInstantSelector.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    Command.Text := CommandText;
end;

procedure TInstantSelector.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Params.Assign(AParams);
  Close;
end;

procedure TInstantSelector.PSStartTransaction;
begin
  if HasConnector then
    Connector.StartTransaction;
end;

procedure TInstantSelector.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(Params);
end;

procedure TInstantSelector.SetActive(Value: Boolean);
var
  WasActive: Boolean;
begin
  WasActive := Active;
  inherited;
  if WasActive and not Active then
    DestroyQuery;
end;

procedure TInstantSelector.SetCommand(const Value: TStringList);
begin
  if Value.Text <> Command.Text then
    Command.Assign(Value);
end;

procedure TInstantSelector.SetConnector(const Value: TInstantConnector);
begin
  if Value <> FConnector then
  begin
    if Assigned(FConnector) then
      FConnector.RemoveFreeNotification(Self);
    FConnector := Value;
    if Assigned(FConnector) then
      FConnector.FreeNotification(Self);
    DestroyQuery;
  end;
end;

procedure TInstantSelector.SetMaxCount(const Value: Integer);
begin
  if Value <> MaxCount then
  begin
    FMaxCount := Value;
    Query.MaxCount := FMaxCount;
  end;
end;

procedure TInstantSelector.SetParams(Value: TParams);
begin
  Params.AssignValues(Value);
end;

procedure TInstantSelector.UpdateParams;
var
  AParams: TParams;
begin
  AParams := TParams.Create(Self);
  try
    Query.FetchParams(Command.Text, AParams);
    AParams.AssignValues(Params);
    Params.Clear;
    Params.Assign(AParams);
  finally
    AParams.Free;
  end;
end;

procedure TInstantSelector.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{ TInstantBlobStream }

procedure TInstantBlobStream.Changed;
begin
  FIsChanged := True;
end;

constructor TInstantBlobStream.Create(AField: TBlobField;
  Mode: TBlobStreamMode);
var
  Attr: TInstantAttribute;
begin
  inherited Create;
  FField := AField;
  if not (Exposer.CurrentObject is TInstantObject) then
    Exit;
  Attr := InstantFindAttribute(AField.FieldName,
    TInstantObject(Exposer.CurrentObject));
  if Attr is TInstantBlob then
    FAttribute := TInstantBlob(Attr);
  if Mode <> bmRead then
  begin
    if Field.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName], Exposer);
    if not (Exposer.State in [dsEdit, dsInsert]) then
      DatabaseError(SNotEditing, Exposer);
  end;
  if Mode = bmWrite then
    Truncate;
end;

destructor TInstantBlobStream.Destroy;
begin
  if IsChanged then
  try
    Field.Modified := True;
    Exposer.DataEvent(deFieldChange, Integer(Field));
  except
{$IFDEF VER130}
    Application.HandleException(Self);
{$ELSE}
    if Assigned(Classes.ApplicationHandleException) then
      Classes.ApplicationHandleException(Self);
{$ENDIF}
  end;
  inherited Destroy;
end;

function TInstantBlobStream.GetExposer: TInstantCustomExposer;
begin
  if Assigned(Field) then
    Result := Field.DataSet as TInstantCustomExposer
  else
    Result := nil;
end;

function TInstantBlobStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Assigned(Attribute) then
  begin
    Result := Attribute.ReadBuffer(Buffer, Position, Count);
    Inc(FPosition, Result);
  end else
    Result := 0;
end;

function TInstantBlobStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(Attribute) then
  begin
    case Origin of
      0: FPosition := Offset;
      1: Inc(FPosition, Offset);
      2: FPosition := Attribute.Size + Offset;
    end;
    if FPosition < 0 then
      FPosition := 0
  end;
  Result := FPosition;
end;

procedure TInstantBlobStream.Truncate;
begin
  if not Assigned(Attribute) then
    Exit;
  Attribute.Clear;
  Changed;
end;

function TInstantBlobStream.Write(const Buffer; Count: Integer): Longint;
begin
  if Assigned(Attribute) then
  begin
    Result := Attribute.WriteBuffer(Buffer, Position, Count);
    Inc(FPosition, Result);
    Changed;
  end else
    Result := 0;
end;

initialization
  AccessorClasses := TList.Create;
  InstantRegisterAccessorClass(TInstantAccessor);
  InstantRegisterAccessorClass(TInstantQueryAccessor);
  InstantRegisterAccessorClass(TInstantObjectAccessor);
  InstantRegisterAccessorClass(TInstantListAccessor);
  InstantRegisterAccessorClass(TInstantCollectionAccessor);

finalization
  AccessorClasses.Free;

end.
