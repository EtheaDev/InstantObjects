(*
 *   InstantObjects
 *   XML Support
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
 * The Original Code is: Carlo Barazzetta
 *
 * The Initial Developer of the Original Code is: Carlo Barazzetta
 *
 * Contributor(s):
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena, Marco Cantù,
 * Steven Mitchell, Andrea Magni
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantXML;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses
  System.Classes
  , Data.DB
  , System.Contnrs
  , System.Types
  , InstantPersistence
  , InstantBrokers
  , InstantCommand
  , InstantMetadata
  , InstantTypes
  , InstantClasses
  ;

const
  XML_UTF8_HEADER = '<?xml version="1.0" encoding="UTF-8"?>';
  XML_ISO_HEADER = '<?xml version="1.0" encoding="ISO-8859-1"?>';
  XML_EXT = 'xml';
  DOT_XML_EXT = '.' + XML_EXT;
  XML_WILDCARD = '*' + DOT_XML_EXT;

type
  TXMLFileFormat = (xffUtf8, xffIso);

  TXMLFileOpenEvent = function(const AObject: TInstantObject;
      const AObjectId, AFileName: string): Boolean of Object;

  TXMLFileSaveEvent = function(const AObject: TInstantObject;
      const AFileName: string): Boolean of Object;

  TXMLFilesAccessor = class(TCustomConnection)
  private
    FConnected: Boolean;
    FRootFolder: string;
    FXMLFileFormat: TXMLFileFormat;
    FOnCustomLoadXMLFile: TXMLFileOpenEvent;
    FOnCustomSaveToXMLFile: TXMLFileSaveEvent;
    function GetRootFolder: string;
    procedure SetRootFolder(const AValue: string);
    function ObjectUpdateCountFromFileName(const AFileName: string): Integer;
  protected
    procedure CreateStorageDir(const AStorageName: string);
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    // Override this method to redirect storage to different folders with a
    // class-level or object-level granularity.
    function LoadInstantObjectFromXmlFile(const AObject: TInstantObject;
      const AObjectId, AFileName: string): Boolean; virtual;
    function SaveInstantObjectToXmlFile(const AObject: TInstantObject;
      const AFileName: string): Boolean; virtual;
    function LocateInstantObjectXmlFile(const AObjectClassName,
      AObjectId, AFileName: string): Boolean; virtual;
    function DeleteInstantObjectXmlFile(const AObject: TInstantObject;
      const AFileName: string): Boolean; virtual;
    // Override InternalReadInstantObject, InternalSaveInstantObject,
    // InternalLocateInstantObject and InternalDeleteInstantObject to redirect
    // storage to media other than the file system.
    function InternalReadInstantObject(const AObject: TInstantObject;
      const AStorageName, AObjectId: string;
      out AObjectUpdateCount: Integer): Boolean; virtual;
    function InternalWriteInstantObject(const AObject: TInstantObject;
      const AStorageName: string; out AObjectUpdateCount: Integer): Boolean;
      virtual;
    function InternalLocateInstantObject(const AStorageName, AObjectClassName,
      AObjectId: string): Boolean; virtual;
    function InternalDeleteInstantObject(const AObject: TInstantObject;
      const AStorageName: string): Boolean; virtual;
  public
    function GetObjectFileName(const AStorageName, AObjectClassName,
      AObjectId: string): string; virtual;
    constructor Create(AOwner: TComponent); override;
    function LocateInstantObject(const AStorageName, AObjectClassName,
      AObjectId: string): Boolean;
    function ReadInstantObject(const AObject: TInstantObject; const AStorageName,
      AObjectId: string; out AObjectUpdateCount: Integer): Boolean;
    function WriteInstantObject(const AObject: TInstantObject;
      const AStorageName: string; out AObjectUpdateCount: Integer): Boolean;
    function DeleteInstantObject(const AObject: TInstantObject;
      const AStorageName: string): Boolean;
    function CheckConflict(AObject: TInstantObject;
      const AStorageName, AObjectId: string): Boolean;
    procedure LoadFileList(const AFileList: TStringList;
      const AStorageNames: TStrings; const AFilterWildCard: string = ''); virtual;
  published
    property RootFolder: string read GetRootFolder write SetRootFolder;
    property XMLFileFormat: TXMLFileFormat read FXMLFileFormat write FXMLFileFormat default xffUtf8;
    property OnCustomLoadXMLFile: TXMLFileOpenEvent read FOnCustomLoadXMLFile write FOnCustomLoadXMLFile;
    property OnCustomSaveToXMLFile: TXMLFileSaveEvent read FOnCustomSaveToXMLFile write FOnCustomSaveToXMLFile;
  end;

  TInstantXMLConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FRootFolder: string;
    FXMLFileFormat: TXMLFileFormat;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property RootFolder: string read FRootFolder write FRootFolder;
    property XMLFileFormat: TXMLFileFormat read FXMLFileFormat write
      FXMLFileFormat default xffUtf8;
  end;

  TInstantXMLConnector = class(TInstantConnectionBasedConnector)
  private
    function GetConnection: TXMLFilesAccessor;
    procedure SetConnection(const Value: TXMLFilesAccessor);
    procedure CheckConnection;
  protected
    procedure SetBlobStreamFormat(const Value: TInstantStreamFormat); override;
    function CreateBroker: TInstantBroker; override;
    function GetDatabaseName: string; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
    constructor Create(AOwner: TComponent); override;
  published
    property Connection: TXMLFilesAccessor read GetConnection write
      SetConnection;
    property UseTransactions default False;
    property LoginPrompt default False;
    property UseUnicode default False;
    property BlobStreamFormat default sfXML;
    property ReadObjectListWithNoLock default true;
  end;

  TInstantXMLResolver = class;

  TInstantXMLBroker = class(TInstantCustomRelationalBroker)
  private
    FResolverList: TObjectList;
    function GetResolverCount: Integer;
    function GetResolverList: TObjectList;
    function GetResolvers(Index: Integer): TInstantXMLResolver;
    property ResolverList: TObjectList read GetResolverList;
    function GetConnector: TInstantXMLConnector;
  protected
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    class function GetCatalogClass: TInstantCatalogClass; override;
    function CreateResolver(const StorageName: string): TInstantXMLResolver;
    function EnsureResolver(Map: TInstantAttributeMap): TInstantCustomResolver; override;
    function FindResolver(const StorageName: string): TInstantXMLResolver;
    property ResolverCount: Integer read GetResolverCount;
    property Resolvers[Index: Integer]: TInstantXMLResolver read GetResolvers;
  public
    destructor Destroy; override;
    function CreateDBBuildCommand(const CommandType:
      TInstantDBBuildCommandType):
      TInstantDBBuildCommand; override;
    property Connector: TInstantXMLConnector read GetConnector;
  end;

  TInstantXMLResolver = class(TInstantCustomResolver)
  private
    FStorageName: string;
    function GetBroker: TInstantXMLBroker;
    function CheckConflict(AObject: TInstantObject; const AObjectId: string;
      ConflictAction: TInstantConflictAction): Boolean;
  protected
    procedure InternalDisposeMap(AObject: TInstantObject; Map:
      TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
        override;
    procedure InternalRetrieveMap(AObject: TInstantObject;
      const AObjectId: string; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo;
      const AObjectData: TInstantAbstractObjectData); override;
    procedure InternalStoreMap(AObject: TInstantObject; Map:
      TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
        override;
    procedure ResetAttributes(AObject: TInstantObject; Map:
      TInstantAttributeMap);
    function Locate(AObject: TInstantObject; const AObjectId: string): Boolean;
      virtual;
    function ReadInstantObject(AObject: TInstantObject; const AObjectId: string;
      out AObjectUpdateCount: Integer): Boolean;
    function WriteInstantObject(AObject: TInstantObject;
      const AObjectId: string; out AObjectUpdateCount: Integer): Boolean;
  public
    constructor Create(ABroker: TInstantCustomRelationalBroker;
      const AStorageName: string);
    property Broker: TInstantXMLBroker read GetBroker;
    property StorageName: string read FStorageName;
  end;

  TInstantXMLTranslator = class(TInstantRelationalTranslator)
  protected
    function TranslateClassRef(ClassRef: TInstantIQLClassRef; Writer:
      TInstantIQLWriter): Boolean; override;
  end;

  TInstantXMLQuery = class(TInstantCustomRelationalQuery)
  private
    FObjectReferenceList: TObjectList;
    FStatement: string;
    FParamsObject: TParams;
    FStorageNames: TStringList;
    FObjectClassNames: TStringList;
    FIQLCommand: TInstantIQLCommand;
    FFilterWildCard: string;
    procedure DestroyObjectReferenceList;
    function GetObjectReferenceCount: Integer;
    function GetObjectReferenceList: TObjectList;
    function GetObjectReferences(Index: Integer): TInstantObjectReference;
    // Creates an object reference for each item in AFileList that represents
    // an object of a class included in FObjectClassNames.
    procedure InitObjectReferences(const AFileList: TStrings);

    procedure SortObjectReferences;
    function GetParamsObject: TParams;
    function GetConnector: TInstantXMLConnector;
    procedure SetStorageNames(const Value: TStringList);
    procedure SetObjectClassNames(const Value: TStringList);
    function SortCompareMethod(Holder, Obj1, Obj2: TInstantObject): Integer;
    procedure CreateIQLCommand;
    procedure QuickSortObjectReferenceList(List: TList; L, R: Integer;
      Compare: TInstantSortCompare);
    procedure SetFilterWildCard(const Value: string);
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
    function GetActive: Boolean; override;
    function GetParams: TParams; override;
    function GetStatement: string; override;
    function InternalAddObject(AObject: TObject): Integer; override;
    procedure InternalClose; override;
    procedure InternalGetInstantObjectRefs(List: TInstantObjectReferenceList);
        override;
    function InternalGetObjectCount: Integer; override;
    function InternalGetObjects(Index: Integer): TObject; override;
    function InternalIndexOfObject(AObject: TObject): Integer; override;
    procedure InternalInsertObject(Index: Integer; AObject: TObject); override;
    procedure InternalOpen; override;
    procedure InternalReleaseObject(AObject: TObject); override;
    function InternalRemoveObject(AObject: TObject): Integer; override;
    procedure SetParams(Value: TParams); override;
    function ObjectFetched(Index: Integer): Boolean; override;
    procedure SetStatement(const Value: string); override;
    function InternalGetObjectReferenceId(Index: Integer) : string; override;
    property ObjectReferenceCount: Integer read GetObjectReferenceCount;
    property ObjectReferenceList: TObjectList read GetObjectReferenceList;
    property ObjectReferences[Index: Integer]: TInstantObjectReference read
      GetObjectReferences;
    property ParamsObject: TParams read GetParamsObject;
  public
    procedure AfterConstruction; override;
    // List of folders from which files should be loaded in InternalOpen.
    property StorageNames: TStringList read FStorageNames write SetStorageNames;
    // Filter objects by WildCard
    property FilterWildCard: string read FFilterWildCard write SetFilterWildCard;
    // Used to filter by class name the files loaded during InternalOpen.
    property ObjectClassNames: TStringList read FObjectClassNames write SetObjectClassNames;
    destructor Destroy; override;
    property Connector: TInstantXMLConnector read GetConnector;
  end;

  // Base class for all XML database build commands.
  TInstantDBBuildXMLCommand = class(TInstantDBBuildCommand)
  private
    function GetConnector: TInstantXMLConnector;
    function GetBroker: TInstantXMLBroker;
  protected
    // Returns The number of statements that compound this script. The
    // predefined implementation returns 1.
    function GetCommandCount: Integer; virtual;
    // Returns the nth command that is part of this script. Valid values
    // are in the range 0 to Pred(GetCommandCount). The default
    // implementation, which should always be called through inherited at the
    // beginning of the overridden version, just returns '', or raises an
    // exception if Index is not in the allowed range.
    function GetCommand(const Index: Integer): string; virtual;
  public
    property Connector: TInstantXMLConnector read GetConnector;
    property Broker: TInstantXMLBroker read GetBroker;
  end;

  TInstantDBBuildXMLAddTableCommand = class(TInstantDBBuildXMLCommand)
  private
    function GetTableMetadata: TInstantTableMetadata;
  protected
    procedure InternalExecute; override;
  public
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

  TInstantDBBuildXMLDropTableCommand = class(TInstantDBBuildXMLCommand)
  private
    function GetTableMetadata: TInstantTableMetadata;
  protected
    procedure InternalExecute; override;
  public
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

procedure GlobalLoadFileList(const Path: string; FileList: TStringList;
  const AFilterWildCard: string = '');
function GetXMLLineBreak: string;

implementation

uses
  System.SysUtils
  , InstantConsts
  , System.TypInfo
  , InstantXMLCatalog
  , InstantUtils
{$IFNDEF INSTANTOBJECTS_FMX}
{$IFNDEF IO_CONSOLE}
  , InstantXMLConnectionDefEdit
  , Vcl.FileCtrl
  , Vcl.Controls
{$ENDIF}
{$ENDIF}
  , WinApi.Windows
  ;

resourcestring
  SCannotCreateDirectory = 'Cannot create directory %s';
  SCommandIndexOutOfBounds = 'Command index out of bounds.';

function GetFileClassName(const FileName: string): string; forward;
function GetFileId(const FileName: string): string; forward;
function GetObjectUpdateCount(const FileName: string): Integer; forward;

procedure GlobalLoadFileList(const Path: string; FileList: TStringList;
  const AFilterWildCard: string = '');
var
  SearchRec: TSearchRec;
  R: Integer;
  PathWithWildCards: string;
begin
  if AFilterWildCard <> '' then
    PathWithWildCards := IncludeTrailingPathDelimiter(Path) + AFilterWildCard
  else
    PathWithWildCards := IncludeTrailingPathDelimiter(Path) + XML_WILDCARD;
  //Find the first file
  R := System.SysUtils.FindFirst(PathWithWildCards, faAnyFile, SearchRec);
  try
    while R = 0 do // file found!
    begin
      FileList.Append(SearchRec.Name); // Add file to list
      R := System.SysUtils.FindNext(SearchRec); // Find next file
    end;
  finally
    System.SysUtils.FindClose(SearchRec);
  end;
end;

function RightPos(const ASubString, AString: string): Integer;
var
  I: Integer;
  SubStringLength: Integer;
begin
  Result := 0;
  SubStringLength := Length(ASubString);
  for I := Length(AString) - Length(ASubString) + 1 downto 1 do
  begin
    if Copy(AString, I, SubStringLength) = ASubString then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function GetFileClassName(const FileName: string): string;
begin
  // File Name: ClassName.Id.UpdateCount.xml
  Result := Copy(FileName, 1, Pos('.', FileName) - 1);
end;

function GetFileId(const FileName: string): string;
var
  P: Integer;
begin
  // File Name: ClassName.Id.UpdateCount.xml
  // Drop ClassName and extension.
  P := Pos('.', FileName);
  Result := Copy(FileName, P + 1, RightPos('.', FileName) - P - 1);
  // Drop UpdateCount.
  Delete(Result, RightPos('.', Result), MaxInt);
end;

function GetObjectUpdateCount(const FileName: string): Integer;
var
  S: string;
  P: Integer;
begin
  // File Name: ClassName.Id.UpdateCount.xml
  // Drop ClassName and extension.
  P := Pos('.', FileName);
  S := Copy(FileName, P + 1, RightPos('.', FileName) - P - 1);
  // Drop Id.
  Delete(S, 1, RightPos('.', S));
  Result := StrToIntDef(S, 0);
end;

function GetXMLLineBreak: string;
begin
  if InstantXMLIndentationSize > 0 then
    Result := sLineBreak
  else
    Result := '';
end;

{ TInstantXMLConnectionDef }

class function TInstantXMLConnectionDef.ConnectionTypeName: string;
begin
  Result := 'XML';
end;

class function TInstantXMLConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantXMLConnector;
end;

function TInstantXMLConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
begin
  Result := TXMLFilesAccessor.Create(AOwner);
  TXMLFilesAccessor(Result).RootFolder := RootFolder;
  TXMLFilesAccessor(Result).FXMLFileFormat := XMLFileFormat;
end;

function TInstantXMLConnectionDef.Edit: Boolean;
begin
{$IFNDEF INSTANTOBJECTS_FMX}
{$IFNDEF IO_CONSOLE}
  with TInstantXMLConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
{$ELSE}
  Result := False;
{$ENDIF}
{$ENDIF}
end;

{ TInstantXMLResolver }

function TInstantXMLResolver.CheckConflict(AObject: TInstantObject;
  const AObjectId: string;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := Broker.Connector.Connection.CheckConflict(AObject, FStorageName,
    AObjectId);

  if Result and (ConflictAction = caFail) then
    raise EInstantConflict.CreateFmt(SUpdateConflict,
      [AObject.ClassName, AObjectId]);
end;

constructor TInstantXMLResolver.Create(ABroker: TInstantCustomRelationalBroker;
  const AStorageName: string);
begin
  inherited Create(ABroker);
  FStorageName := AStorageName;
end;

function TInstantXMLResolver.GetBroker: TInstantXMLBroker;
begin
  Result := inherited Broker as TInstantXMLBroker;
end;

procedure TInstantXMLResolver.InternalDisposeMap(AObject: TInstantObject;
  Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction;
  Info: PInstantOperationInfo);
var
  TransError: Exception;
  AInfo: TInstantOperationInfo;
  // ObjFileName : string;
begin
  if not Assigned(Info) then
  begin
    Info := @AInfo;
    Info.Conflict := False;
  end;
  if Locate(AObject, AObject.PersistentId) then
  begin
    // Delete object file
    try
      Broker.Connector.Connection.DeleteInstantObject(AObject, FStorageName);
      Info.Success := True;
      Info.Conflict := not Info.Success;
    except
      on EAbort do
        raise;
      on E: Exception do
      begin
        //        TransError := TranslateError(AObject, E);
        TransError := nil;
        if Assigned(TransError) then
          raise TransError
        else
          raise;
      end;
    end;
  end
  else if Map.IsRootMap and (ConflictAction = caFail) then
    raise EInstantConflict.CreateFmt(SDisposeConflict,
      [AObject.ClassName, AObject.PersistentId])
end;

procedure TInstantXMLResolver.InternalRetrieveMap(AObject: TInstantObject;
  const AObjectId: string; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo;
  const AObjectData: TInstantAbstractObjectData);
var
  AInfo: TInstantOperationInfo;
  LObjectUpdateCount: Integer;
begin
  // This resolver does not support retrieving from any kind of object data.
  if not Assigned(Info) then
  begin
    Info := @AInfo;
    Info.Conflict := False;
  end;
  //Read object from file
  Info.Success := Locate(AObject, AObjectId) and
    ReadInstantObject(AObject, AObjectId, LObjectUpdateCount);
  Info.Conflict := not Info.Success;
  if Info.Success then
  begin
    if Map.IsRootMap then
    begin
      Broker.SetObjectUpdateCount(AObject, LObjectUpdateCount);
    end;
  end
  else
    ResetAttributes(AObject, Map);
end;

procedure TInstantXMLResolver.InternalStoreMap(AObject: TInstantObject;
  Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction;
  Info: PInstantOperationInfo);
var
  NewId: string;
  AInfo: TInstantOperationInfo;
  TransError: Exception;
  version: Integer;
begin
  { Avoid any interference with storage when performing store operation }
  NewId := AObject.Id;

  if not Assigned(Info) then
  begin
    Info := @AInfo;
    Info.Conflict := False;
  end;

  if AObject.IsPersistent then
  begin
    if Locate(AObject, AObject.PersistentId) then
    begin
      if Map.IsRootMap then
        Info.Conflict := CheckConflict(AObject, AObject.Id, ConflictAction);
    end;
  end;
  try
    if (ConflictAction = caIgnore) or not Info.Conflict then
    begin
      Info.Success := WriteInstantObject(AObject, NewId, Version);
      Info.Conflict := not Info.Success;
      if Map.IsRootMap then
        Broker.SetObjectUpdateCount(AObject, version);
    end
    else
      Info.Success := True;
  except
    on E: Exception do
    begin
      if E is EAbort then
        raise
      else
      begin
        //        TransError := TranslateError(AObject, E);
        TransError := nil;
        if Assigned(TransError) then
          raise TransError
        else
          raise;
      end;
    end;
  end;
end;

function TInstantXMLResolver.Locate(AObject: TInstantObject; const AObjectId: string):
  Boolean;
begin
  if AObject.Metadata.TableName = FStorageName then
    Result := Broker.Connector.Connection.LocateInstantObject(
      FStorageName, AObject.ClassName, AObjectId)
  else
    Result := True;
end;

function TInstantXMLResolver.ReadInstantObject(AObject: TInstantObject;
  const AObjectId: string; out AObjectUpdateCount: Integer): Boolean;
begin
  if AObject.Metadata.TableName = FStorageName then
    Result := Broker.Connector.Connection.ReadInstantObject(AObject,
      FStorageName, AObjectId, AObjectUpdateCount)
  else
  begin
    Result := True;
    AObjectUpdateCount := 1;
  end;
end;

procedure TInstantXMLResolver.ResetAttributes(AObject: TInstantObject;
  Map: TInstantAttributeMap);
begin

end;

function TInstantXMLResolver.WriteInstantObject(AObject: TInstantObject;
  const AObjectId: string; out AObjectUpdateCount: Integer): Boolean;
begin
  if AObject.Metadata.TableName = FStorageName then
    Result := Broker.Connector.Connection.WriteInstantObject(AObject,
      FStorageName, AObjectUpdateCount)
  else
  begin
    Result := True;
    AObjectUpdateCount := 1;
  end;
end;

{ TInstantXMLConnector }

procedure TInstantXMLConnector.CheckConnection;
begin
  if not assigned(Connection) then
    raise EPropertyError.Create(SUnassignedConnection);
end;

class function TInstantXMLConnector.ConnectionDefClass:
  TInstantConnectionDefClass;
begin
  Result := TInstantXMLConnectionDef;
end;

constructor TInstantXMLConnector.Create(AOwner: TComponent);
begin
  inherited;
  LoginPrompt := False;
  UseTransactions := False;
  UseUnicode := False;
  BlobStreamFormat := sfXML;
  ReadObjectListWithNoLock := true;
end;

function TInstantXMLConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantXMLBroker.Create(Self);
end;

function TInstantXMLConnector.GetConnection: TXMLFilesAccessor;
begin
  Result := inherited Connection as TXMLFilesAccessor;
end;

function TInstantXMLConnector.GetDatabaseName: string;
begin
  Result := Connection.RootFolder;
end;

procedure TInstantXMLConnector.InternalBuildDatabase(Scheme: TInstantScheme);
begin
  CheckConnection;
  if not System.SysUtils.DirectoryExists(Connection.RootFolder) and
      not System.SysUtils.ForceDirectories(Connection.RootFolder) then
    raise EInOutError.CreateFmt(SCannotCreateDirectory,
      [Connection.RootFolder]);
end;

procedure TInstantXMLConnector.InternalCommitTransaction;
begin
  { TODO: Commit transaction for Connection }
end;

function TInstantXMLConnector.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantXMLQuery.Create(Self);
end;

procedure TInstantXMLConnector.InternalRollbackTransaction;
begin
  { TODO: Roll back transaction for Connection }
end;

procedure TInstantXMLConnector.InternalStartTransaction;
begin
  { TODO: Start transaction for Connection }
end;

procedure TInstantXMLConnector.SetBlobStreamFormat(
  const Value: TInstantStreamFormat);
begin
  if Value <> sfXML then
    Exit;
  inherited;
end;

procedure TInstantXMLConnector.SetConnection(
  const Value: TXMLFilesAccessor);
begin
  inherited Connection := Value;
end;

{ TInstantXMLBroker }

function TInstantXMLBroker.CreateResolver(
  const StorageName: string): TInstantXMLResolver;
begin
  Result := TInstantXMLResolver.Create(Self, StorageName);
end;

destructor TInstantXMLBroker.Destroy;
begin
  FResolverList.Free;
  inherited;
end;

function TInstantXMLBroker.CreateCatalog(const AScheme: TInstantScheme):
  TInstantCatalog;
begin
  Result := TInstantXMLCatalog.Create(AScheme, Self);
end;

function TInstantXMLBroker.CreateDBBuildCommand(const CommandType:
  TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  if CommandType = ctAddTable then
    Result := TInstantDBBuildXMLAddTableCommand.Create(CommandType, Connector)
  else if CommandType = ctDropTable then
    Result := TInstantDBBuildXMLDropTableCommand.Create(CommandType, Connector)
  else
    Result := inherited CreateDBBuildCommand(CommandType);
end;

function TInstantXMLBroker.EnsureResolver(
  Map: TInstantAttributeMap): TInstantCustomResolver;
var
  StorageName: string;
begin
  StorageName := Map.Name;
  Result := FindResolver(StorageName);
  if not Assigned(Result) then
  begin
    Result := CreateResolver(StorageName);
    ResolverList.Add(Result);
  end;
end;

function TInstantXMLBroker.FindResolver(
  const StorageName: string): TInstantXMLResolver;
var
  I: Integer;
begin
  for I := 0 to Pred(ResolverCount) do
  begin
    Result := Resolvers[I];
    if SameText(StorageName, Result.StorageName) then
      Exit;
  end;
  Result := nil;
end;

class function TInstantXMLBroker.GetCatalogClass: TInstantCatalogClass;
begin
  Result := TInstantXMLCatalog;
end;

function TInstantXMLBroker.GetConnector: TInstantXMLConnector;
begin
  Result := inherited Connector as TInstantXMLConnector;
end;

function TInstantXMLBroker.GetResolverCount: Integer;
begin
  Result := ResolverList.Count;
end;

function TInstantXMLBroker.GetResolverList: TObjectList;
begin
  if not Assigned(FResolverList) then
    FResolverList := TObjectList.Create;
  Result := FResolverList;
end;

function TInstantXMLBroker.GetResolvers(
  Index: Integer): TInstantXMLResolver;
begin
  Result := ResolverList[Index] as TInstantXMLResolver;
end;

{ TInstantXMLQuery }

procedure TInstantXMLQuery.AfterConstruction;
begin
  inherited;
  FStorageNames := TStringList.Create;
  FStorageNames.Sorted := True;
  FStorageNames.Duplicates := dupIgnore;
  FObjectClassNames := TStringList.Create;
  FObjectClassNames.Sorted := True;
  FObjectClassNames.Duplicates := dupIgnore;
  FIQLCommand := TInstantIQLCommand.Create(nil);
end;

destructor TInstantXMLQuery.Destroy;
begin
  DestroyObjectReferenceList;
  FParamsObject.Free;
  FreeAndNil(FStorageNames);
  FreeAndNil(FObjectClassNames);
  FreeAndNil(FIQLCommand);
  inherited;
end;

procedure TInstantXMLQuery.DestroyObjectReferenceList;
begin
  FreeAndNil(FObjectReferenceList);
end;

function TInstantXMLQuery.GetActive: Boolean;
begin
  Result := Assigned(FObjectReferenceList);
end;

function TInstantXMLQuery.GetConnector: TInstantXMLConnector;
begin
  Result := inherited Connector as TInstantXMLConnector;
end;

function TInstantXMLQuery.GetObjectReferenceCount: Integer;
begin
  Result := ObjectReferenceList.Count;
end;

function TInstantXMLQuery.GetObjectReferenceList: TObjectList;
begin
  if not Assigned(FObjectReferenceList) then
    FObjectReferenceList := TObjectList.Create;
  Result := FObjectReferenceList;
end;

function TInstantXMLQuery.GetObjectReferences(
  Index: Integer): TInstantObjectReference;
begin
  Result := ObjectReferenceList[Index] as TInstantObjectReference;
end;

function TInstantXMLQuery.GetParams: TParams;
begin
  Result := ParamsObject;
end;

function TInstantXMLQuery.GetParamsObject: TParams;
begin
  if not Assigned(FParamsObject) then
    FParamsObject := TParams.Create;
  Result := FParamsObject;
end;

function TInstantXMLQuery.GetStatement: string;
begin
  Result := FStatement;
end;

procedure TInstantXMLQuery.InitObjectReferences(const AFileList: TStrings);
var
  I: Integer;

  procedure AddObjectReference(const AFileName: string);
  var
    vClassName, vObjectId: string;
    vObjectReference: TInstantObjectReference;
    vIndex: Integer;
  begin
    vClassName := GetFileClassName(AFileName);
    if FObjectClassNames.Find(vClassName, vIndex) then
    begin
      vObjectId := GetFileId(AFileName);
      vObjectReference := TInstantObjectReference.Create(nil, True);
      try
        vObjectReference.ReferenceObject(vClassName, vObjectId);
        ObjectReferenceList.Add(vObjectReference);
      except
        ObjectReferenceList.Remove(vObjectReference);
        vObjectReference.Free;
        raise;
      end;
    end;
  end;

begin
  for I := 0 to AFileList.Count - 1 do
    AddObjectReference(AFileList[I]);
end;

function TInstantXMLQuery.InternalAddObject(AObject: TObject): Integer;
var
  ObjectRef: TInstantObjectReference;
begin
  ObjectRef := TInstantObjectReference.Create(AObject as TInstantObject, True);
  try
    Result := ObjectReferenceList.Add(ObjectRef);
  except
    ObjectRef.Free;
    raise
  end;
end;

procedure TInstantXMLQuery.InternalClose;
begin
  DestroyObjectReferenceList;
  inherited;
end;

procedure TInstantXMLQuery.InternalGetInstantObjectRefs(List:
    TInstantObjectReferenceList);
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectReferenceCount) do
    if ObjectFetched(I) and (Objects[I] is TInstantObject) then
      List.Add(TInstantObject(Objects[I]));
end;

function TInstantXMLQuery.InternalGetObjectCount: Integer;
begin
  Result := ObjectReferenceCount;
end;

function TInstantXMLQuery.InternalGetObjectReferenceId(
  Index: Integer): string;
begin
  Result := inherited InternalGetObjectReferenceId(Index)+
    '['+TInstantObjectReference(ObjectReferenceList[Index]).ObjectId+']';
end;

function TInstantXMLQuery.InternalGetObjects(Index: Integer): TObject;
begin
  Result := ObjectReferences[Index].Dereference(Connector);
end;

function TInstantXMLQuery.InternalIndexOfObject(AObject: TObject): Integer;
begin
  if AObject is TInstantObject then
    for Result := 0 to Pred(ObjectReferenceCount) do
      if ObjectReferences[Result].Equals(TInstantObject(AObject)) then
        Exit;
  Result := -1;
end;

procedure TInstantXMLQuery.InternalInsertObject(Index: Integer;
  AObject: TObject);
var
  ObjectRef: TInstantObjectReference;
begin
  ObjectRef := TInstantObjectReference.Create(AObject as TInstantObject, True);
  try
    ObjectReferenceList.Insert(Index, ObjectRef);
  except
    ObjectRef.Free;
    raise
  end;
end;

procedure TInstantXMLQuery.InternalOpen;
var
  vFileList: TStringList;
begin
  inherited;
  CreateIQLCommand;

  vFileList := TStringList.Create;
  try
    Connector.Connection.Open;
    Connector.Connection.LoadFileList(vFileList, FStorageNames, FFilterWildCard);
    InitObjectReferences(vFileList);

    if Assigned(FIQLCommand.Order) then
      SortObjectReferences;
  finally
    vFileList.Free;
  end;
end;

procedure TInstantXMLQuery.CreateIQLCommand;
var
  LReader: TInstantIQLReader;
begin
  LReader := TInstantIQLReader.Create(TStringStream.Create(Command, TEncoding.Unicode), True);
  try
    LReader.ReadObject(FIQLCommand);
  finally
    FreeAndNil(LReader);
  end;
end;

procedure TInstantXMLQuery.InternalReleaseObject(AObject: TObject);
var
  Index: Integer;
begin
  Index := IndexOfObject(AObject);
  if Index <> -1 then
    ObjectReferences[Index].DestroyInstance;
end;

function TInstantXMLQuery.InternalRemoveObject(AObject: TObject): Integer;
begin
  Result := IndexOfObject(AObject);
  if Result <> -1 then
    ObjectReferenceList.Delete(Result);
end;

function TInstantXMLQuery.ObjectFetched(Index: Integer): Boolean;
begin
  Result := ObjectReferences[Index].HasInstance;
end;

procedure TInstantXMLQuery.SetFilterWildCard(const Value: string);
begin
  FFilterWildCard := Value;
  if Active then
  begin
    Close;
    Open;
  end;
end;

procedure TInstantXMLQuery.SetObjectClassNames(const Value: TStringList);
begin
  FObjectClassNames.Assign(Value);
end;

procedure TInstantXMLQuery.SetParams(Value: TParams);
begin
  inherited;
  ParamsObject.Assign(Value);
end;

procedure TInstantXMLQuery.SetStatement(const Value: string);
begin
  inherited;
  FStatement := Value;
end;

procedure TInstantXMLQuery.SetStorageNames(const Value: TStringList);
begin
  FStorageNames.Assign(Value);
end;

procedure TInstantXMLQuery.QuickSortObjectReferenceList(List: TList;
  L, R: Integer; Compare: TInstantSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(nil,
          TInstantObjectReference(List[I]).Dereference(Connector),
          TInstantObjectReference(List[P]).Dereference(Connector)) < 0 do
        Inc(I);
      while Compare(nil,
          TInstantObjectReference(List[J]).Dereference(Connector),
          TInstantObjectReference(List[P]).Dereference(Connector)) > 0 do
        Dec(J);
      if I <= J then
      begin
        List.Exchange(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortObjectReferenceList(List, L, J, Compare);
    L := I;
  until I >= R;
end;

procedure TInstantXMLQuery.SortObjectReferences;
begin
  if ObjectReferenceList.Count > 0 then
    QuickSortObjectReferenceList(ObjectReferenceList, 0,
      Pred(ObjectReferenceList.Count), SortCompareMethod);
end;

function TInstantXMLQuery.SortCompareMethod(Holder, Obj1,
  Obj2: TInstantObject): Integer;
var
  LOrder: TInstantIQLOrder;
begin
  Assert(Assigned(FIQLCommand.Order));

  Result := 0;
  LOrder := FIQLCommand.Order;
  Assert(Assigned(LOrder.OrderSpec));
  while Assigned(LOrder) do
  begin
    Result := InstantCompareObjects(Obj1, Obj2,
      LOrder.OrderSpec.Expression.Text, []);
    if LOrder.OrderSpec.OrderDirection = odDesc then
      Result := -Result;
    if Result <> 0 then
      Break;
    LOrder := LOrder.NextOrder;
  end;
end;

class function TInstantXMLQuery.TranslatorClass:
  TInstantRelationalTranslatorClass;
begin
  Result := TInstantXMLTranslator;
end;

{ TXMLFilesAccessor }

function TXMLFilesAccessor.GetRootFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(FRootFolder);
end;

procedure TXMLFilesAccessor.SetRootFolder(const AValue: string);
begin
  if FRootFolder <> AValue then
  begin
    FRootFolder := AValue;
  end;
end;

function TXMLFilesAccessor.SaveInstantObjectToXmlFile(
  const AObject: TInstantObject; const AFileName: string): Boolean;
var
  strstream: TStringStream;
begin
  if Assigned(FOnCustomSaveToXMLFile) then
    Result := FOnCustomSaveToXMLFile(AObject, AFileName)
  else
  begin
    strstream := TStringStream.Create('', TEncoding.UTF8);
    try
      InstantWriteObject(strStream, sfXML, AObject);
      strstream.SaveToFile(AFileName);
      Result := True;
    finally
      strStream.Free;
    end;
  end;
end;

function TXMLFilesAccessor.LoadInstantObjectFromXmlFile(
  const AObject: TInstantObject; const AObjectId, AFileName: string): Boolean;
var
  fileStream: TFileStream;
begin
  Result := False;
  if FileExists(AFileName) then
  begin
    Try
      if Assigned(FOnCustomLoadXMLFile) then
        Result := FOnCustomLoadXMLFile(AObject, AObjectId, AFileName);

      if not Result then
      begin
        fileStream := TFileStream.Create(AFileName, fmShareDenyWrite);
        try
          InstantReadObject(fileStream, sfXML, AObject);
          Result := True;
        finally
          fileStream.Free;
        end;
      end;
    except
      on E: Exception do raise EInOutError.CreateFmt(SErrorLoadingFile, [AFileName, E.Message]);
    end;
  end;
end;

function TXMLFilesAccessor.LocateInstantObjectXmlFile(const AObjectClassName,
  AObjectId, AFileName: string): Boolean;
begin
  Result := FileExists(AFileName);
end;

function TXMLFilesAccessor.DeleteInstantObjectXmlFile(
  const AObject: TInstantObject; const AFileName: string): Boolean;
begin
  Result := System.SysUtils.DeleteFile(AFileName);
end;

function TXMLFilesAccessor.ReadInstantObject(const AObject: TInstantObject;
  const AStorageName, AObjectId: string; out AObjectUpdateCount: Integer): Boolean;
begin
  Result := InternalReadInstantObject(AObject, AStorageName, AObjectId,
    AObjectUpdateCount);
end;

function TXMLFilesAccessor.InternalReadInstantObject(
  const AObject: TInstantObject; const AStorageName, AObjectId: string;
  out AObjectUpdateCount: Integer): Boolean;
var
  LFileName: string;
begin
  LFileName := GetObjectFileName(AStorageName, AObject.ClassName, AObjectId);
  Result := LoadInstantObjectFromXmlFile(AObject, AObjectId, LFileName);
  AObjectUpdateCount := ObjectUpdateCountFromFileName(LFileName);
end;

function TXMLFilesAccessor.WriteInstantObject(const AObject: TInstantObject;
  const AStorageName: string; out AObjectUpdateCount: Integer): Boolean;
begin
  // Get rid of the previous file if the object already existed and has
  // changed Id.
  if (AObject.PersistentId <> '') and AObject.IsIdChanged then
    DeleteInstantObjectXmlFile(AObject,
      GetObjectFileName(AStorageName, AObject.ClassName, AObject.PersistentId));

  Result := InternalWriteInstantObject(AObject, AStorageName,
    AObjectUpdateCount);
end;

function TXMLFilesAccessor.InternalWriteInstantObject(
  const AObject: TInstantObject; const AStorageName: string;
  out AObjectUpdateCount: Integer): Boolean;
var
  LFileName: string;
begin
  LFileName := GetObjectFileName(AStorageName, AObject.ClassName, AObject.Id);
  CreateStorageDir(AStorageName);
  Result := SaveInstantObjectToXmlFile(AObject, LFileName);
  AObjectUpdateCount := ObjectUpdateCountFromFileName(LFileName);
end;

function TXMLFilesAccessor.LocateInstantObject(const AStorageName,
  AObjectClassName, AObjectId: string): Boolean;
begin
  Result := InternalLocateInstantObject(AStorageName, AObjectClassName,
    AObjectId);
end;

function TXMLFilesAccessor.InternalLocateInstantObject(const AStorageName,
  AObjectClassName, AObjectId: string): Boolean;
var
  LFileName: string;
begin
  LFileName := GetObjectFileName(AStorageName, AObjectClassName, AObjectId);
  Result := LocateInstantObjectXmlFile(AObjectClassName, AObjectId, LFileName);
end;

function TXMLFilesAccessor.DeleteInstantObject(const AObject: TInstantObject;
  const AStorageName: string): Boolean;
begin
  Result := InternalDeleteInstantObject(AObject, AStorageName);
end;

function TXMLFilesAccessor.InternalDeleteInstantObject(
  const AObject: TInstantObject; const AStorageName: string): Boolean;
var
  LFileName: string;
begin
  LFileName := GetObjectFileName(AStorageName, AObject.ClassName, AObject.Id);
  Result := DeleteInstantObjectXmlFile(AObject, LFileName);
end;

procedure TXMLFilesAccessor.CreateStorageDir(const AStorageName: string);
var
  LPath: string;
begin
  if not System.SysUtils.DirectoryExists(RootFolder) then
    MkDir(RootFolder);
  LPath := IncludeTrailingBackslash(RootFolder) + AStorageName;
  if not System.SysUtils.DirectoryExists(LPath) then
    MkDir(LPath);
end;

function TXMLFilesAccessor.ObjectUpdateCountFromFileName(
  const AFileName: string): Integer;
begin
  Result := GetObjectUpdateCount(ExtractFileName(AFileName));
end;

constructor TXMLFilesAccessor.Create(AOwner: TComponent);
begin
  inherited;
  FXMLFileFormat := xffUtf8;
end;

function TXMLFilesAccessor.GetObjectFileName(const AStorageName,
  AObjectClassName, AObjectId: string): string;
begin
  Result := RootFolder + AStorageName + PathDelim + AObjectClassName + '.'
    + AObjectId + '.1' + DOT_XML_EXT;
end;

function TXMLFilesAccessor.CheckConflict(AObject: TInstantObject;
  const AStorageName, AObjectId: string): Boolean;
begin
  Result := False; // don't care about updatecount
end;

procedure TXMLFilesAccessor.LoadFileList(const AFileList: TStringList;
  const AStorageNames: TStrings; const AFilterWildCard: string = '');
var
  I: Integer;
begin
  AFileList.Clear;
  for I := 0 to AStorageNames.Count - 1 do
    GlobalLoadFileList(RootFolder + AStorageNames[I], AFileList, AFilterWildCard);
end;

procedure TXMLFilesAccessor.DoConnect;
begin
  if not System.SysUtils.DirectoryExists(RootFolder) then
    MkDir(RootFolder);
  FConnected := True;
end;

procedure TXMLFilesAccessor.DoDisconnect;
begin
  FConnected := False;
end;

function TXMLFilesAccessor.GetConnected: Boolean;
begin
  Result := FConnected;
end;

{ TInstantXMLTranslator }

function TInstantXMLTranslator.TranslateClassRef(
  ClassRef: TInstantIQLClassRef; Writer: TInstantIQLWriter): Boolean;
var
  vInheritedClasses: TList;
  I: Integer;
begin
  Result := inherited TranslateClassRef(ClassRef, Writer);
  if Context.TablePathCount > 0 then
  begin
    (Query as TInstantXMLQuery).StorageNames.Text := Context.TablePaths[0];
    (Query as TInstantXMLQuery).ObjectClassNames.Text := ClassRef.ObjectClassName;
    if ClassRef.Any then
    begin
      // Need to add all inherited classes as well.
      vInheritedClasses := TList.Create;
      try
        InstantGetClasses(vInheritedClasses, InstantFindClass(ClassRef.ObjectClassName));
        for I := 0 to vInheritedClasses.Count - 1 do
        begin
          (Query as TInstantXMLQuery).StorageNames.Add(
             TInstantObjectClass(vInheritedClasses[I]).Metadata.TableName);
          (Query as TInstantXMLQuery).ObjectClassNames.Add(
             TInstantObjectClass(vInheritedClasses[I]).ClassName);
        end;
      finally
        FreeAndNil(vInheritedClasses);
      end;
    end;
  end
  else
  begin
    (Query as TInstantXMLQuery).StorageNames.Clear;
    (Query as TInstantXMLQuery).ObjectClassNames.Clear;
  end;
end;

{ TInstantDBBuildXMLCommand }

function TInstantDBBuildXMLCommand.GetBroker: TInstantXMLBroker;
begin
  Result := Connector.Broker as TInstantXMLBroker;
end;

function TInstantDBBuildXMLCommand.GetCommand(const Index: Integer): string;
begin
  if (Index < 0) or (Index >= GetCommandCount) then
    raise EInstantDBBuildError.CreateFmt(SCommandIndexOutOfBounds,
      [Index]);
  Result := '';
end;

function TInstantDBBuildXMLCommand.GetCommandCount: Integer;
begin
  Result := 1;
end;

function TInstantDBBuildXMLCommand.GetConnector: TInstantXMLConnector;
begin
  Result := inherited Connector as TInstantXMLConnector;
end;

function TInstantDBBuildXMLAddTableCommand.GetTableMetadata:
  TInstantTableMetadata;
begin
  Result := NewMetadata as TInstantTableMetadata;
end;

procedure TInstantDBBuildXMLAddTableCommand.InternalExecute;
var
  vDatabaseName: string;
begin
  Connector.CheckConnection;
  vDatabaseName := Connector.DatabaseName;

  if not System.SysUtils.DirectoryExists(vDatabaseName) and
      not System.SysUtils.ForceDirectories(vDatabaseName) then
    raise EInOutError.CreateFmt(SCannotCreateDirectory, [vDatabaseName]);

  // No need to create the class-specific folders, which will be created
  // when instances are written.
end;

function TInstantDBBuildXMLDropTableCommand.GetTableMetadata:
  TInstantTableMetadata;
begin
  Result := OldMetadata as TInstantTableMetadata;
end;

procedure TInstantDBBuildXMLDropTableCommand.InternalExecute;
var
  vTableName: string;
  sr: TSearchRec;
  vDatabaseName: string;
begin
  Connector.CheckConnection;
  vDatabaseName := Connector.DatabaseName;

  // Delete subFolder for the "storage name"
  vTableName := vDatabaseName + TableMetadata.Name;
  if System.SysUtils.DirectoryExists(vTableName) then
  begin
    if FindFirst(vTableName + '\*.*', faAnyFile, sr) = 0 then
    begin
      repeat
        System.SysUtils.DeleteFile(vTableName + '\' + sr.Name);
      until FindNext(sr) <> 0;
      System.SysUtils.FindClose(sr);
    end;
    RemoveDir(vTableName);
  end;
end;

initialization
  System.Classes.RegisterClass(TInstantXMLConnectionDef);
  TInstantXMLConnector.RegisterClass;

finalization
  TInstantXMLConnector.UnregisterClass;

end.

