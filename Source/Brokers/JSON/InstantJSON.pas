(*
 *   InstantObjects
 *   JSON Support
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
 * Carlo Barazzetta
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantJSON;

{$IFDEF LINUX64}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  System.Classes
  , Data.DB, Contnrs
  , System.Types, InstantPersistence, InstantBrokers, InstantCommand,
  InstantMetadata, InstantTypes, InstantClasses;

const
  JSON_EXT = 'json';
  DOT_JSON_EXT = '.' + JSON_EXT;
  JSON_WILDCARD = '*' + DOT_JSON_EXT;

type
  TJSONFileFormat = (xffUtf8, xffIso);

  TJSONFileOpenEvent = function(const AObject: TInstantObject;
      const AObjectId, AFileName: string): Boolean of Object;

  TJSONFileSaveEvent = function(const AObject: TInstantObject;
      const AFileName: string): Boolean of Object;

  TJSONFilesAccessor = class(TCustomConnection)
  private
    FConnected: Boolean;
    FRootFolder: string;
    FJSONFileFormat: TJSONFileFormat;
    FOnCustomLoadJSONFile: TJSONFileOpenEvent;
    FOnCustomSaveToJSONFile: TJSONFileSaveEvent;
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
    function LoadInstantObjectFromJSONFile(const AObject: TInstantObject;
      const AObjectId, AFileName: string): Boolean; virtual;
    function SaveInstantObjectToJSONFile(const AObject: TInstantObject;
      const AFileName: string): Boolean; virtual;
    function LocateInstantObjectJSONFile(const AObjectClassName,
      AObjectId, AFileName: string): Boolean; virtual;
    function DeleteInstantObjectJSONFile(const AObject: TInstantObject;
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
    property JSONFileFormat: TJSONFileFormat read FJSONFileFormat write FJSONFileFormat default xffUtf8;
    property OnCustomLoadJSONFile: TJSONFileOpenEvent read FOnCustomLoadJSONFile write FOnCustomLoadJSONFile;
    property OnCustomSaveToJSONFile: TJSONFileSaveEvent read FOnCustomSaveToJSONFile write FOnCustomSaveToJSONFile;
  end;

  TInstantJSONConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FRootFolder: string;
    FRemoteURL: string;
    FJSONFileFormat: TJSONFileFormat;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property RootFolder: string read FRootFolder write FRootFolder;
    property RemoteURL: string read FRemoteURL write FRemoteURL;
    property JSONFileFormat: TJSONFileFormat read FJSONFileFormat write
      FJSONFileFormat default xffUtf8;
  end;

  TInstantJSONConnector = class(TInstantConnectionBasedConnector)
  private
    function GetConnection: TJSONFilesAccessor;
    procedure SetConnection(const Value: TJSONFilesAccessor);
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
    property Connection: TJSONFilesAccessor read GetConnection write
      SetConnection;
    property UseTransactions default False;
    property LoginPrompt default False;
    property UseUnicode default False;
    //change InstantDefins.inc to activate $DEFINE DELPHI_NEON if you want JSON support
    //or remove IOJSON_4 from Package Group
    property BlobStreamFormat default sfJSON;
    property ReadObjectListWithNoLock default true;
  end;

  TInstantJSONResolver = class;

  TInstantJSONBroker = class(TInstantCustomRelationalBroker)
  private
    FResolverList: TObjectList;
    function GetResolverCount: Integer;
    function GetResolverList: TObjectList;
    function GetResolvers(Index: Integer): TInstantJSONResolver;
    property ResolverList: TObjectList read GetResolverList;
    function GetConnector: TInstantJSONConnector;
  protected
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog; override;
    class function GetCatalogClass: TInstantCatalogClass; override;
    function CreateResolver(const StorageName: string): TInstantJSONResolver;
    function EnsureResolver(Map: TInstantAttributeMap): TInstantCustomResolver; override;
    function FindResolver(const StorageName: string): TInstantJSONResolver;
    property ResolverCount: Integer read GetResolverCount;
    property Resolvers[Index: Integer]: TInstantJSONResolver read GetResolvers;
  public
    destructor Destroy; override;
    function CreateDBBuildCommand(const CommandType:
      TInstantDBBuildCommandType):
      TInstantDBBuildCommand; override;
    property Connector: TInstantJSONConnector read GetConnector;
  end;

  TInstantJSONResolver = class(TInstantCustomResolver)
  private
    FStorageName: string;
    function GetBroker: TInstantJSONBroker;
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
    property Broker: TInstantJSONBroker read GetBroker;
    property StorageName: string read FStorageName;
  end;

  TInstantJSONTranslator = class(TInstantRelationalTranslator)
  protected
    function TranslateClassRef(ClassRef: TInstantIQLClassRef; Writer:
      TInstantIQLWriter): Boolean; override;
  end;

  TInstantJSONQuery = class(TInstantCustomRelationalQuery)
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
    function GetConnector: TInstantJSONConnector;
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
    property Connector: TInstantJSONConnector read GetConnector;
  end;

  // Base class for all JSON database build commands.
  TInstantDBBuildJSONCommand = class(TInstantDBBuildCommand)
  private
    function GetConnector: TInstantJSONConnector;
    function GetBroker: TInstantJSONBroker;
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
    property Connector: TInstantJSONConnector read GetConnector;
    property Broker: TInstantJSONBroker read GetBroker;
  end;

  TInstantDBBuildJSONAddTableCommand = class(TInstantDBBuildJSONCommand)
  private
    function GetTableMetadata: TInstantTableMetadata;
  protected
    procedure InternalExecute; override;
  public
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

  TInstantDBBuildJSONDropTableCommand = class(TInstantDBBuildJSONCommand)
  private
    function GetTableMetadata: TInstantTableMetadata;
  protected
    procedure InternalExecute; override;
  public
    property TableMetadata: TInstantTableMetadata read GetTableMetadata;
  end;

procedure GlobalLoadFileList(const Path: string; FileList: TStringList;
  const AFilterWildCard: string = '');
function GetJSONLineBreak: string;

implementation

uses
  SysUtils, InstantConsts,
  TypInfo, InstantJSONCatalog, InstantUtils,
{$IFNDEF INSTANTOBJECTS_FMX}
{$IFNDEF IO_CONSOLE}
InstantJSONConnectionDefEdit, FileCtrl
  , Vcl.Controls,
{$ENDIF}
{$ENDIF}
  Windows;

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
    PathWithWildCards := IncludeTrailingPathDelimiter(Path) + JSON_WILDCARD;
  //Find the first file
  R := SysUtils.FindFirst(PathWithWildCards, faAnyFile, SearchRec);
  try
    while R = 0 do // file found!
    begin
      FileList.Append(SearchRec.Name); // Add file to list
      R := SysUtils.FindNext(SearchRec); // Find next file
    end;
  finally
    SysUtils.FindClose(SearchRec);
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
  // File Name: ClassName.Id.UpdateCount.json
  Result := Copy(FileName, 1, Pos('.', FileName) - 1);
end;

function GetFileId(const FileName: string): string;
var
  P: Integer;
begin
  // File Name: ClassName.Id.UpdateCount.json
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
  // File Name: ClassName.Id.UpdateCount.json
  // Drop ClassName and extension.
  P := Pos('.', FileName);
  S := Copy(FileName, P + 1, RightPos('.', FileName) - P - 1);
  // Drop Id.
  Delete(S, 1, RightPos('.', S));
  Result := StrToIntDef(S, 0);
end;

function GetJSONLineBreak: string;
begin
  if InstantJSONIndentationSize > 0 then
    Result := sLineBreak
  else
    Result := '';
end;

{ TInstantJSONConnectionDef }

class function TInstantJSONConnectionDef.ConnectionTypeName: string;
begin
  Result := 'JSON';
end;

class function TInstantJSONConnectionDef.ConnectorClass: TInstantConnectorClass;
begin
  Result := TInstantJSONConnector;
end;

function TInstantJSONConnectionDef.CreateConnection(
  AOwner: TComponent): TCustomConnection;
begin
  Result := TJSONFilesAccessor.Create(AOwner);
  TJSONFilesAccessor(Result).RootFolder := RootFolder;
  TJSONFilesAccessor(Result).FJSONFileFormat := JSONFileFormat;
end;

function TInstantJSONConnectionDef.Edit: Boolean;
begin
{$IFNDEF INSTANTOBJECTS_FMX}
{$IFNDEF IO_CONSOLE}
  with TInstantJSONConnectionDefEditForm.Create(nil) do
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

{ TInstantJSONResolver }

function TInstantJSONResolver.CheckConflict(AObject: TInstantObject;
  const AObjectId: string;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := Broker.Connector.Connection.CheckConflict(AObject, FStorageName,
    AObjectId);

  if Result and (ConflictAction = caFail) then
    raise EInstantConflict.CreateFmt(SUpdateConflict,
      [AObject.ClassName, AObjectId]);
end;

constructor TInstantJSONResolver.Create(ABroker: TInstantCustomRelationalBroker;
  const AStorageName: string);
begin
  inherited Create(ABroker);
  FStorageName := AStorageName;
end;

function TInstantJSONResolver.GetBroker: TInstantJSONBroker;
begin
  Result := inherited Broker as TInstantJSONBroker;
end;

procedure TInstantJSONResolver.InternalDisposeMap(AObject: TInstantObject;
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

procedure TInstantJSONResolver.InternalRetrieveMap(AObject: TInstantObject;
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

procedure TInstantJSONResolver.InternalStoreMap(AObject: TInstantObject;
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

function TInstantJSONResolver.Locate(AObject: TInstantObject; const AObjectId: string):
  Boolean;
begin
  if AObject.Metadata.TableName = FStorageName then
    Result := Broker.Connector.Connection.LocateInstantObject(
      FStorageName, AObject.ClassName, AObjectId)
  else
    Result := True;
end;

function TInstantJSONResolver.ReadInstantObject(AObject: TInstantObject;
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

procedure TInstantJSONResolver.ResetAttributes(AObject: TInstantObject;
  Map: TInstantAttributeMap);
begin

end;

function TInstantJSONResolver.WriteInstantObject(AObject: TInstantObject;
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

{ TInstantJSONConnector }

procedure TInstantJSONConnector.CheckConnection;
begin
  if not assigned(Connection) then
    raise EPropertyError.Create(SUnassignedConnection);
end;

class function TInstantJSONConnector.ConnectionDefClass:
  TInstantConnectionDefClass;
begin
  Result := TInstantJSONConnectionDef;
end;

constructor TInstantJSONConnector.Create(AOwner: TComponent);
begin
  inherited;
  LoginPrompt := False;
  UseTransactions := False;
  UseUnicode := False;
  BlobStreamFormat := sfJSON;
  ReadObjectListWithNoLock := true;
end;

function TInstantJSONConnector.CreateBroker: TInstantBroker;
begin
  Result := TInstantJSONBroker.Create(Self);
end;

function TInstantJSONConnector.GetConnection: TJSONFilesAccessor;
begin
  Result := inherited Connection as TJSONFilesAccessor;
end;

function TInstantJSONConnector.GetDatabaseName: string;
begin
  Result := Connection.RootFolder;
end;

procedure TInstantJSONConnector.InternalBuildDatabase(Scheme: TInstantScheme);
begin
  CheckConnection;
  if not SysUtils.DirectoryExists(Connection.RootFolder) and
      not SysUtils.ForceDirectories(Connection.RootFolder) then
    raise EInOutError.CreateFmt(SCannotCreateDirectory,
      [Connection.RootFolder]);
end;

procedure TInstantJSONConnector.InternalCommitTransaction;
begin
  { TODO: Commit transaction for Connection }
end;

function TInstantJSONConnector.InternalCreateQuery: TInstantQuery;
begin
  Result := TInstantJSONQuery.Create(Self);
end;

procedure TInstantJSONConnector.InternalRollbackTransaction;
begin
  { TODO: Roll back transaction for Connection }
end;

procedure TInstantJSONConnector.InternalStartTransaction;
begin
  { TODO: Start transaction for Connection }
end;

procedure TInstantJSONConnector.SetBlobStreamFormat(
  const Value: TInstantStreamFormat);
begin
  if Value <> sfJSON then
    Exit;
  inherited;
end;

procedure TInstantJSONConnector.SetConnection(
  const Value: TJSONFilesAccessor);
begin
  inherited Connection := Value;
end;

{ TInstantJSONBroker }

function TInstantJSONBroker.CreateResolver(
  const StorageName: string): TInstantJSONResolver;
begin
  Result := TInstantJSONResolver.Create(Self, StorageName);
end;

destructor TInstantJSONBroker.Destroy;
begin
  FResolverList.Free;
  inherited;
end;

function TInstantJSONBroker.CreateCatalog(const AScheme: TInstantScheme):
  TInstantCatalog;
begin
  Result := TInstantJSONCatalog.Create(AScheme, Self);
end;

function TInstantJSONBroker.CreateDBBuildCommand(const CommandType:
  TInstantDBBuildCommandType): TInstantDBBuildCommand;
begin
  if CommandType = ctAddTable then
    Result := TInstantDBBuildJSONAddTableCommand.Create(CommandType, Connector)
  else if CommandType = ctDropTable then
    Result := TInstantDBBuildJSONDropTableCommand.Create(CommandType, Connector)
  else
    Result := inherited CreateDBBuildCommand(CommandType);
end;

function TInstantJSONBroker.EnsureResolver(
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

function TInstantJSONBroker.FindResolver(
  const StorageName: string): TInstantJSONResolver;
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

class function TInstantJSONBroker.GetCatalogClass: TInstantCatalogClass;
begin
  Result := TInstantJSONCatalog;
end;

function TInstantJSONBroker.GetConnector: TInstantJSONConnector;
begin
  Result := inherited Connector as TInstantJSONConnector;
end;

function TInstantJSONBroker.GetResolverCount: Integer;
begin
  Result := ResolverList.Count;
end;

function TInstantJSONBroker.GetResolverList: TObjectList;
begin
  if not Assigned(FResolverList) then
    FResolverList := TObjectList.Create;
  Result := FResolverList;
end;

function TInstantJSONBroker.GetResolvers(
  Index: Integer): TInstantJSONResolver;
begin
  Result := ResolverList[Index] as TInstantJSONResolver;
end;

{ TInstantJSONQuery }

procedure TInstantJSONQuery.AfterConstruction;
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

destructor TInstantJSONQuery.Destroy;
begin
  DestroyObjectReferenceList;
  FParamsObject.Free;
  FreeAndNil(FStorageNames);
  FreeAndNil(FObjectClassNames);
  FreeAndNil(FIQLCommand);
  inherited;
end;

procedure TInstantJSONQuery.DestroyObjectReferenceList;
begin
  FreeAndNil(FObjectReferenceList);
end;

function TInstantJSONQuery.GetActive: Boolean;
begin
  Result := Assigned(FObjectReferenceList);
end;

function TInstantJSONQuery.GetConnector: TInstantJSONConnector;
begin
  Result := inherited Connector as TInstantJSONConnector;
end;

function TInstantJSONQuery.GetObjectReferenceCount: Integer;
begin
  Result := ObjectReferenceList.Count;
end;

function TInstantJSONQuery.GetObjectReferenceList: TObjectList;
begin
  if not Assigned(FObjectReferenceList) then
    FObjectReferenceList := TObjectList.Create;
  Result := FObjectReferenceList;
end;

function TInstantJSONQuery.GetObjectReferences(
  Index: Integer): TInstantObjectReference;
begin
  Result := ObjectReferenceList[Index] as TInstantObjectReference;
end;

function TInstantJSONQuery.GetParams: TParams;
begin
  Result := ParamsObject;
end;

function TInstantJSONQuery.GetParamsObject: TParams;
begin
  if not Assigned(FParamsObject) then
    FParamsObject := TParams.Create;
  Result := FParamsObject;
end;

function TInstantJSONQuery.GetStatement: string;
begin
  Result := FStatement;
end;

procedure TInstantJSONQuery.InitObjectReferences(const AFileList: TStrings);
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

function TInstantJSONQuery.InternalAddObject(AObject: TObject): Integer;
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

procedure TInstantJSONQuery.InternalClose;
begin
  DestroyObjectReferenceList;
  inherited;
end;

procedure TInstantJSONQuery.InternalGetInstantObjectRefs(List:
    TInstantObjectReferenceList);
var
  I: Integer;
begin
  for I := 0 to Pred(ObjectReferenceCount) do
    if ObjectFetched(I) and (Objects[I] is TInstantObject) then
      List.Add(TInstantObject(Objects[I]));
end;

function TInstantJSONQuery.InternalGetObjectCount: Integer;
begin
  Result := ObjectReferenceCount;
end;

function TInstantJSONQuery.InternalGetObjectReferenceId(
  Index: Integer): string;
begin
  Result := inherited InternalGetObjectReferenceId(Index)+
    '['+TInstantObjectReference(ObjectReferenceList[Index]).ObjectId+']';
end;

function TInstantJSONQuery.InternalGetObjects(Index: Integer): TObject;
begin
  Result := ObjectReferences[Index].Dereference(Connector);
end;

function TInstantJSONQuery.InternalIndexOfObject(AObject: TObject): Integer;
begin
  if AObject is TInstantObject then
    for Result := 0 to Pred(ObjectReferenceCount) do
      if ObjectReferences[Result].Equals(TInstantObject(AObject)) then
        Exit;
  Result := -1;
end;

procedure TInstantJSONQuery.InternalInsertObject(Index: Integer;
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

procedure TInstantJSONQuery.InternalOpen;
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

procedure TInstantJSONQuery.CreateIQLCommand;
var
  LReader: TInstantIQLReader;
begin
  LReader := TInstantIQLReader.Create(TStringStream.Create(Command{$IFDEF D12+}, TEncoding.Unicode{$ENDIF}), True);
  try
    LReader.ReadObject(FIQLCommand);
  finally
    FreeAndNil(LReader);
  end;
end;

procedure TInstantJSONQuery.InternalReleaseObject(AObject: TObject);
var
  Index: Integer;
begin
  Index := IndexOfObject(AObject);
  if Index <> -1 then
    ObjectReferences[Index].DestroyInstance;
end;

function TInstantJSONQuery.InternalRemoveObject(AObject: TObject): Integer;
begin
  Result := IndexOfObject(AObject);
  if Result <> -1 then
    ObjectReferenceList.Delete(Result);
end;

function TInstantJSONQuery.ObjectFetched(Index: Integer): Boolean;
begin
  Result := ObjectReferences[Index].HasInstance;
end;

procedure TInstantJSONQuery.SetFilterWildCard(const Value: string);
begin
  FFilterWildCard := Value;
  if Active then
  begin
    Close;
    Open;
  end;
end;

procedure TInstantJSONQuery.SetObjectClassNames(const Value: TStringList);
begin
  FObjectClassNames.Assign(Value);
end;

procedure TInstantJSONQuery.SetParams(Value: TParams);
begin
  inherited;
  ParamsObject.Assign(Value);
end;

procedure TInstantJSONQuery.SetStatement(const Value: string);
begin
  inherited;
  FStatement := Value;
end;

procedure TInstantJSONQuery.SetStorageNames(const Value: TStringList);
begin
  FStorageNames.Assign(Value);
end;

procedure TInstantJSONQuery.QuickSortObjectReferenceList(List: TList;
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

procedure TInstantJSONQuery.SortObjectReferences;
begin
  if ObjectReferenceList.Count > 0 then
    QuickSortObjectReferenceList(ObjectReferenceList, 0,
      Pred(ObjectReferenceList.Count), SortCompareMethod);
end;

function TInstantJSONQuery.SortCompareMethod(Holder, Obj1,
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

class function TInstantJSONQuery.TranslatorClass:
  TInstantRelationalTranslatorClass;
begin
  Result := TInstantJSONTranslator;
end;

{ TJSONFilesAccessor }

function TJSONFilesAccessor.GetRootFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(FRootFolder);
end;

procedure TJSONFilesAccessor.SetRootFolder(const AValue: string);
begin
  if FRootFolder <> AValue then
  begin
    FRootFolder := AValue;
  end;
end;

function TJSONFilesAccessor.SaveInstantObjectToJSONFile(
  const AObject: TInstantObject; const AFileName: string): Boolean;
var
  strstream: TStringStream;
  fileStream: TFileStream;
  DataStr: UTF8String;
begin
  if Assigned(FOnCustomSaveToJSONFile) then
    Result := FOnCustomSaveToJSONFile(AObject, AFileName)
  else
  begin
    strstream := TStringStream.Create('', TEncoding.UTF8);
    try
      InstantWriteObject(strStream, sfJSON, AObject);
      DataStr := UTF8String(strStream.DataString);
    finally
      strStream.Free;
    end;
    fileStream := TFileStream.Create(AFileName, fmCreate);
    try
      Result := fileStream.Write(DataStr[1], Length(DataStr)) <> 0;
    finally
      fileStream.Free;
    end;
  end;
end;

{$IFDEF UNICODE}
function TJSONFilesAccessor.LoadInstantObjectFromJSONFile(
  const AObject: TInstantObject; const AObjectId, AFileName: string): Boolean;
var
  fileStream: TFileStream;
begin
  Result := False;
  if FileExists(AFileName) then
  begin
    Try
      if Assigned(FOnCustomLoadJSONFile) then
        Result := FOnCustomLoadJSONFile(AObject, AObjectId, AFileName);

      if not Result then
      begin
        fileStream := TFileStream.Create(AFileName, fmShareDenyWrite);
        try
          InstantReadObject(fileStream, sfJSON, AObject);
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
{$ELSE}
function TJSONFilesAccessor.LoadInstantObjectFromJSONFile(
  const AObject: TInstantObject; const AObjectId, AFileName: string): Boolean;
var
  fileStream: TFileStream;
  strUtf8: string;
  strstream: TStringStream;
begin
  if Assigned(FOnCustomLoadJSONFile) then
    Result := FOnCustomLoadJSONFile(AObject, AObjectId, AFileName);

  if not Result then
  
  begin
    fileStream := TFileStream.Create(AFileName, fmShareDenyWrite);
    try
      SetLength(strUtf8, fileStream.Size);
      Result := fileStream.Read(strUtf8[1], fileStream.Size) <> 0;
    finally
      fileStream.Free;
    end;
    {$IFDEF D6+}
    if FJSONFileFormat = xffUtf8 then
      strUtf8 := Utf8ToAnsi(strUtf8);
    {$ENDIF}

    strstream := TStringStream.Create(strUtf8);
    try try
      InstantReadObject(strstream, sfJSON, AObject);
    except
      on E: Exception do raise EInOutError.CreateFmt(SErrorLoadingFile, [AFileName, E.Message]); end;
    finally
      strstream.Free;
    end;
  end;
end;
{$ENDIF}

function TJSONFilesAccessor.LocateInstantObjectJSONFile(const AObjectClassName,
  AObjectId, AFileName: string): Boolean;
begin
  Result := FileExists(AFileName);
end;

function TJSONFilesAccessor.DeleteInstantObjectJSONFile(
  const AObject: TInstantObject; const AFileName: string): Boolean;
begin
  Result := SysUtils.DeleteFile(AFileName);
end;

function TJSONFilesAccessor.ReadInstantObject(const AObject: TInstantObject;
  const AStorageName, AObjectId: string; out AObjectUpdateCount: Integer): Boolean;
begin
  Result := InternalReadInstantObject(AObject, AStorageName, AObjectId,
    AObjectUpdateCount);
end;

function TJSONFilesAccessor.InternalReadInstantObject(
  const AObject: TInstantObject; const AStorageName, AObjectId: string;
  out AObjectUpdateCount: Integer): Boolean;
var
  LFileName: string;
begin
  LFileName := GetObjectFileName(AStorageName, AObject.ClassName, AObjectId);
  Result := LoadInstantObjectFromJSONFile(AObject, AObjectId, LFileName);
  AObjectUpdateCount := ObjectUpdateCountFromFileName(LFileName);
end;

function TJSONFilesAccessor.WriteInstantObject(const AObject: TInstantObject;
  const AStorageName: string; out AObjectUpdateCount: Integer): Boolean;
begin
  // Get rid of the previous file if the object already existed and has
  // changed Id.
  if (AObject.PersistentId <> '') and AObject.IsIdChanged then
    DeleteInstantObjectJSONFile(AObject,
      GetObjectFileName(AStorageName, AObject.ClassName, AObject.PersistentId));

  Result := InternalWriteInstantObject(AObject, AStorageName,
    AObjectUpdateCount);
end;

function TJSONFilesAccessor.InternalWriteInstantObject(
  const AObject: TInstantObject; const AStorageName: string;
  out AObjectUpdateCount: Integer): Boolean;
var
  LFileName: string;
begin
  LFileName := GetObjectFileName(AStorageName, AObject.ClassName, AObject.Id);
  CreateStorageDir(AStorageName);
  Result := SaveInstantObjectToJSONFile(AObject, LFileName);
  AObjectUpdateCount := ObjectUpdateCountFromFileName(LFileName);
end;

function TJSONFilesAccessor.LocateInstantObject(const AStorageName,
  AObjectClassName, AObjectId: string): Boolean;
begin
  Result := InternalLocateInstantObject(AStorageName, AObjectClassName,
    AObjectId);
end;

function TJSONFilesAccessor.InternalLocateInstantObject(const AStorageName,
  AObjectClassName, AObjectId: string): Boolean;
var
  LFileName: string;
begin
  LFileName := GetObjectFileName(AStorageName, AObjectClassName, AObjectId);
  Result := LocateInstantObjectJSONFile(AObjectClassName, AObjectId, LFileName);
end;

function TJSONFilesAccessor.DeleteInstantObject(const AObject: TInstantObject;
  const AStorageName: string): Boolean;
begin
  Result := InternalDeleteInstantObject(AObject, AStorageName);
end;

function TJSONFilesAccessor.InternalDeleteInstantObject(
  const AObject: TInstantObject; const AStorageName: string): Boolean;
var
  LFileName: string;
begin
  LFileName := GetObjectFileName(AStorageName, AObject.ClassName, AObject.Id);
  Result := DeleteInstantObjectJSONFile(AObject, LFileName);
end;

procedure TJSONFilesAccessor.CreateStorageDir(const AStorageName: string);
var
  LPath: string;
begin
  if not SysUtils.DirectoryExists(RootFolder) then
    MkDir(RootFolder);
  LPath := IncludeTrailingBackslash(RootFolder) + AStorageName;
  if not SysUtils.DirectoryExists(LPath) then
    MkDir(LPath);
end;

function TJSONFilesAccessor.ObjectUpdateCountFromFileName(
  const AFileName: string): Integer;
begin
  Result := GetObjectUpdateCount(ExtractFileName(AFileName));
end;

constructor TJSONFilesAccessor.Create(AOwner: TComponent);
begin
  inherited;
  FJSONFileFormat := xffUtf8;
end;

function TJSONFilesAccessor.GetObjectFileName(const AStorageName,
  AObjectClassName, AObjectId: string): string;
begin
  Result := RootFolder + AStorageName + PathDelim + AObjectClassName + '.'
    + AObjectId + '.1' + DOT_JSON_EXT;
end;

function TJSONFilesAccessor.CheckConflict(AObject: TInstantObject;
  const AStorageName, AObjectId: string): Boolean;
begin
  Result := False; // don't care about updatecount
end;

procedure TJSONFilesAccessor.LoadFileList(const AFileList: TStringList;
  const AStorageNames: TStrings; const AFilterWildCard: string = '');
var
  I: Integer;
begin
  AFileList.Clear;
  for I := 0 to AStorageNames.Count - 1 do
    GlobalLoadFileList(RootFolder + AStorageNames[I], AFileList, AFilterWildCard);
end;

procedure TJSONFilesAccessor.DoConnect;
begin
  if not SysUtils.DirectoryExists(RootFolder) then
    MkDir(RootFolder);
  FConnected := True;
end;

procedure TJSONFilesAccessor.DoDisconnect;
begin
  FConnected := False;
end;

function TJSONFilesAccessor.GetConnected: Boolean;
begin
  Result := FConnected;
end;

{ TInstantJSONTranslator }

function TInstantJSONTranslator.TranslateClassRef(
  ClassRef: TInstantIQLClassRef; Writer: TInstantIQLWriter): Boolean;
var
  vInheritedClasses: TList;
  I: Integer;
begin
  Result := inherited TranslateClassRef(ClassRef, Writer);
  if Context.TablePathCount > 0 then
  begin
    (Query as TInstantJSONQuery).StorageNames.Text := Context.TablePaths[0];
    (Query as TInstantJSONQuery).ObjectClassNames.Text := ClassRef.ObjectClassName;
    if ClassRef.Any then
    begin
      // Need to add all inherited classes as well.
      vInheritedClasses := TList.Create;
      try
        InstantGetClasses(vInheritedClasses, InstantFindClass(ClassRef.ObjectClassName));
        for I := 0 to vInheritedClasses.Count - 1 do
        begin
          (Query as TInstantJSONQuery).StorageNames.Add(
             TInstantObjectClass(vInheritedClasses[I]).Metadata.TableName);
          (Query as TInstantJSONQuery).ObjectClassNames.Add(
             TInstantObjectClass(vInheritedClasses[I]).ClassName);
        end;
      finally
        FreeAndNil(vInheritedClasses);
      end;
    end;
  end
  else
  begin
    (Query as TInstantJSONQuery).StorageNames.Clear;
    (Query as TInstantJSONQuery).ObjectClassNames.Clear;
  end;
end;

{ TInstantDBBuildJSONCommand }

function TInstantDBBuildJSONCommand.GetBroker: TInstantJSONBroker;
begin
  Result := Connector.Broker as TInstantJSONBroker;
end;

function TInstantDBBuildJSONCommand.GetCommand(const Index: Integer): string;
begin
  if (Index < 0) or (Index >= GetCommandCount) then
    raise EInstantDBBuildError.CreateFmt(SCommandIndexOutOfBounds,
      [Index]);
  Result := '';
end;

function TInstantDBBuildJSONCommand.GetCommandCount: Integer;
begin
  Result := 1;
end;

function TInstantDBBuildJSONCommand.GetConnector: TInstantJSONConnector;
begin
  Result := inherited Connector as TInstantJSONConnector;
end;

function TInstantDBBuildJSONAddTableCommand.GetTableMetadata:
  TInstantTableMetadata;
begin
  Result := NewMetadata as TInstantTableMetadata;
end;

procedure TInstantDBBuildJSONAddTableCommand.InternalExecute;
var
  vDatabaseName: string;
begin
  Connector.CheckConnection;
  vDatabaseName := Connector.DatabaseName;

  if not SysUtils.DirectoryExists(vDatabaseName) and
      not SysUtils.ForceDirectories(vDatabaseName) then
    raise EInOutError.CreateFmt(SCannotCreateDirectory, [vDatabaseName]);

  // No need to create the class-specific folders, which will be created
  // when instances are written.
end;

function TInstantDBBuildJSONDropTableCommand.GetTableMetadata:
  TInstantTableMetadata;
begin
  Result := OldMetadata as TInstantTableMetadata;
end;

procedure TInstantDBBuildJSONDropTableCommand.InternalExecute;
var
  vTableName: string;
  sr: TSearchRec;
  vDatabaseName: string;
begin
  Connector.CheckConnection;
  vDatabaseName := Connector.DatabaseName;

  // Delete subFolder for the "storage name"
  vTableName := vDatabaseName + TableMetadata.Name;
  if SysUtils.DirectoryExists(vTableName) then
  begin
    if FindFirst(vTableName + '\*.*', faAnyFile, sr) = 0 then
    begin
      repeat
        SysUtils.DeleteFile(vTableName + '\' + sr.Name);
      until FindNext(sr) <> 0;
      SysUtils.FindClose(sr);
    end;
    RemoveDir(vTableName);
  end;
end;

initialization
  Classes.RegisterClass(TInstantJSONConnectionDef);
  TInstantJSONConnector.RegisterClass;

finalization
  TInstantJSONConnector.UnregisterClass;

end.

