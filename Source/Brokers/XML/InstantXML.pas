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
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantXML;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}

{$IFDEF D6+}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

interface

uses
  Classes, DB, InstantPersistence, InstantCommand, Contnrs;

const
  XML_UTF8_HEADER = '<?xml version="1.0" encoding="UTF-8"?>';
  XML_ISO_HEADER = '<?xml version="1.0" encoding="ISO-8859-1"?>';
  XML_EXT = 'xml';
  DOT_XML_EXT = '.'+XML_EXT;
  XML_WILDCARD = '*'+DOT_XML_EXT;
  {$IFDEF D5}PathDelim = '\';{$ENDIF}
  
type
  TXMLFileFormat = (xffUtf8, xffUtf8BOT, xffIso);

  { TXMLFilesAccessor }
  TXMLFilesAccessor = class(TCustomConnection)
  private
    FConnected : boolean;
    FRootFolder : string;
    FUseVersioning: Boolean;
    FXMLFileFormat: TXMLFileFormat;
    procedure MkStorageDir(const StorageName : string);
    function GetRootFolder: string;
    procedure SetRootFolder(const Value: string);
    function SaveToFileXML_UTF8(AObject: TInstantObject; const FileName: string) : boolean;
    function LoadFromFileXML_UTF8(AObject: TInstantObject; const FileName: string) : boolean;
    function PlainObjectFileName(const StorageName, ClassName, Id: string): string;
    function NewObjectFileName(const StorageName, ClassName, Id: string): string;
    function DeleteObjectFileName(const StorageName, ClassName, Id: string): string;
    function VersionFromFilename(const longfilename: string): Integer;
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
  public
    constructor Create (AOwner: TComponent); override;
    function LastObjectFileName(const StorageName, ClassName, Id: string) : string;
    function ReadInstantObject(AObject : TInstantObject; const StorageName, AObjectId: string;
      out Version : integer): boolean;
    function WriteInstantObject(AObject : TInstantObject; const StorageName: string;
      out Version : integer): boolean;
    function DeleteInstantObject(AObject : TInstantObject; const StorageName: string) : boolean;
    function Locate(const StorageName, AObjectClassName, AObjectId: string): Boolean;
    function CheckConflict (AObject : TInstantObject; const StorageName, AObjectId: string): Boolean;
    procedure LoadFileList (FFileListAccessor: TStringList; const StorageName: string);
  published
    property RootFolder : string read GetRootFolder write SetRootFolder;
    property UseVersioning: Boolean read FUseVersioning write FUseVersioning default False;
    property XMLFileFormat: TXMLFileFormat read FXMLFileFormat write FXMLFileFormat default xffUtf8;
  end;

  TInstantXMLConnectionDef = class(TInstantConnectionBasedConnectionDef)
  private
    FRootFolder: string;
    FUseVersioning: Boolean;
    FXMLFileFormat: TXMLFileFormat;
  protected
    function CreateConnection(AOwner: TComponent): TCustomConnection; override;
  public
    function Edit: Boolean; override;
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
  published
    property RootFolder : string read FRootFolder write FRootFolder;
    property UseVersioning: Boolean read FUseVersioning write FUseVersioning default False;
    property XMLFileFormat: TXMLFileFormat read FXMLFileFormat write FXMLFileFormat default xffUtf8;
  end;

  TInstantXMLConnector = class(TInstantConnectionBasedConnector)
  private
    function GetConnection: TXMLFilesAccessor;
    procedure SetConnection(const Value: TXMLFilesAccessor);
    procedure CheckConnection;
  protected
    function CreateBroker: TInstantBroker; override;
    function GetDatabaseName: string; override;
    procedure InternalBuildDatabase(Scheme: TInstantScheme); override;
    procedure InternalCommitTransaction; override;
    function InternalCreateQuery: TInstantQuery; override;
    procedure InternalRollbackTransaction; override;
    procedure InternalStartTransaction; override;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
  published
    property Connection: TXMLFilesAccessor read GetConnection write SetConnection;
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
    function CreateCatalog(const AScheme: TInstantScheme): TInstantCatalog;
        override;
    function CreateResolver(const StorageName: string): TInstantXMLResolver;
    function EnsureResolver(Map: TInstantAttributeMap): TInstantCustomResolver; override;
    function FindResolver(const StorageName: string): TInstantXMLResolver;
    property ResolverCount: Integer read GetResolverCount;
    property Resolvers[Index: Integer]: TInstantXMLResolver read GetResolvers;
  public
    destructor Destroy; override;
    function CreateDBBuildCommand(const CommandType: TInstantDBBuildCommandType):
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
    procedure InternalDisposeMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    procedure InternalRetrieveMap(AObject: TInstantObject; const AObjectId: string;
      Map: TInstantAttributeMap; ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    procedure InternalStoreMap(AObject: TInstantObject; Map: TInstantAttributeMap;
      ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo); override;
    procedure ResetAttributes(AObject: TInstantObject; Map: TInstantAttributeMap);
    function Locate(AObject : TObject; const AObjectId: string): Boolean; virtual;
    function ReadInstantObject(AObject : TInstantObject; const AObjectId: string;
      out Version : integer): boolean;
    function WriteInstantObject(AObject : TInstantObject; const AObjectId: string;
      out Version : integer): boolean;
  public
    constructor Create(ABroker: TInstantCustomRelationalBroker;
      const AStorageName: string);
    property Broker: TInstantXMLBroker read GetBroker;
    property StorageName: string read FStorageName;
  end;

  TInstantXMLTranslator = class(TInstantRelationalTranslator)
  protected
    function TranslateClassRef(ClassRef: TInstantIQLClassRef; Writer: TInstantIQLWriter): Boolean; override;
  end;

  TInstantXMLQuery = class(TInstantCustomRelationalQuery)
  private
    FObjectReferenceList: TObjectList;
    FStatement: string;
    FParamsObject: TParams;
    procedure DestroyObjectReferenceList;
    function GetObjectReferenceCount: Integer;
    function GetObjectReferenceList: TObjectList;
    function GetObjectReferences(Index: Integer): TInstantObjectReference;
    procedure InitObjectReferences(FileListAccessor : TStringList);
    function GetParamsObject: TParams;
    function GetConnector: TInstantXMLConnector;
  protected
    class function TranslatorClass: TInstantRelationalTranslatorClass; override;
    function GetActive: Boolean; override;
    function GetParams: TParams; override;
    function GetStatement: string; override;
    function InternalAddObject(AObject: TObject): Integer; override;
    procedure InternalClose; override;
    function InternalGetObjectCount: Integer; override;
    function InternalGetObjects(Index: Integer): TObject; override;
    function InternalIndexOfObject(AObject: TObject): Integer; override;
    procedure InternalInsertObject(Index: Integer; AObject: TObject); override;
    procedure InternalOpen; override;
    procedure InternalRefreshObjects; override;
    procedure InternalReleaseObject(AObject: TObject); override;
    function InternalRemoveObject(AObject: TObject): Integer; override;
    procedure SetParams(Value: TParams); override;
    function ObjectFetched(Index: Integer): Boolean; override;
    procedure SetStatement(const Value: string); override;
    property ObjectReferenceCount: Integer read GetObjectReferenceCount;
    property ObjectReferenceList: TObjectList read GetObjectReferenceList;
    property ObjectReferences[Index: Integer]: TInstantObjectReference read GetObjectReferences;
    property ParamsObject: TParams read GetParamsObject;
  public
    StorageName : string;
    destructor Destroy; override;
    property Connector: TInstantXMLConnector read GetConnector;
  end;

  // Base class for all steps that work by executing one or more commands
  // (that is, a script) each.
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

implementation

uses
  SysUtils, InstantConsts, InstantClasses, TypInfo, InstantXMLCatalog,
  InstantXMLConnectionDefEdit,
{$IFDEF MSWINDOWS}
  {$IFDEF D5}
    FileCtrl,
  {$ENDIF}
  Controls;
{$ENDIF}
{$IFDEF LINUX}
  QControls;
{$ENDIF}

resourcestring
  SCannotCreateDirectory = 'Cannot create directory %s';
  SCommandIndexOutOfBounds = 'Command index out of bounds.';

function GetFileClassName(const FileName : string) : string; forward;
function GetFileId(const FileName : string) : string; forward;
function GetFileVersion(const FileName : string) : Integer; forward;

{$IFDEF D5}
function IncludeTrailingPathDelimiter(const S : string) : string;
begin
  Result := IncludeTrailingBackSlash(S);
end;
{$ENDIF}

procedure GlobalLoadFileList(const Path: string; FileList : TStringList);
var
  SearchRec : TSearchRec;
  R : Integer;
  PathWithWildCards : string;
begin
  FileList.Clear;
  PathWithWildCards := IncludeTrailingPathDelimiter(Path)+XML_WILDCARD;
  //Find the first file
  R := SysUtils.FindFirst( PathWithWildCards, faAnyFile, SearchRec );
  try
    while R = 0 do // file found!
    begin
      FileList.Append( SearchRec.Name ); // Add file to list
      R := SysUtils.FindNext(SearchRec); // Find next file
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

// fill filelist with unique names of files, using the last version
// number or skipping the deleted files (version = 0)
// code badly needs to be optimized - marcoc
procedure GlobalLoadFileListLastVersion(const Path: string; FileList : TStringList);
var
  i, currentVersion, activeVersionPos, newVersion : Integer;
  currentid, shortFileName : string;
begin
  // first load all of them
  GlobalLoadFileList (Path, FileList);

  // now remove version duplicates and deleted documents
  FileList.Sorted := True;
  currentid := '';
  currentVersion := -1;
  activeVersionPos := -1;
  for i := FileList.Count - 1 downto 0 do
  begin
    shortFileName := extractFilename (filelist[i]);
    if currentid = GetFileId (shortFileName) then
    begin
      newVersion := GetFileVersion(shortFileName);
      // if the file is marked as deleted
      if newVersion = 0 then
      begin
        CurrentVersion := NewVersion;
        fileList.Delete(i);
        activeVersionPos := 0;
        if activeVersionPos <> -1 then
          fileList.Delete(activeVersionPos);
      end
      // if it is already marked as deleted, skip it
      else if currentVersion = 0 then
      begin
        fileList.Delete(i);
      end
      // if the file is "newer"
      else
      if currentVersion < newVersion then
      begin
        // delete the other version, as this is newer
        CurrentVersion := GetFileVersion(shortFileName);
        if activeVersionPos <> -1 then
          fileList.Delete(activeVersionPos);
        activeVersionPos := i;
      end
      else
        // delete this version, which is older
        fileList.Delete(i);
    end
    else // we have moved to a new objectid, reset all
    begin
      currentid := GetFileId (shortFileName);
      currentVersion := GetFileVersion(shortFileName);
      activeVersionPos := i;
      // if the first file of this instance is marked as deleted...
      if CurrentVersion = 0 then
        fileList.Delete(i);
    end;
  end;
end;

function GetFileClassName(const FileName : string) : string;
begin
  Result := Copy(FileName,1,pos('.',FileName)-1);
end;

function GetFileId(const FileName : string) : string;
begin
  //File Name: ClassName.Id.UpdateCount.xml
  Result := Copy(FileName,pos('.',FileName)+1,MaxInt);   //Extract ClassName
  Result := Copy(Result,1,pos('.',Result)-1); //Extract UpdateCount
end;

function GetFileVersion(const FileName : string) : Integer;
var
  s: string;
begin
  //File Name: ClassName.Id.UpdateCount.xml
  s := Copy(FileName,pos('.',FileName)+1,MaxInt);   //Extract ClassName
  s := Copy(s,pos('.',s)+1,MaxInt); //Extract Id
  s := Copy(s,1,pos('.',s)-1); //Extract UpdateCount
  Result := StrToIntDef (s, 0);
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
  TXMLFilesAccessor(Result).UseVersioning := UseVersioning;
  TXMLFilesAccessor(Result).FXMLFileFormat := XMLFileFormat;
end;

function TInstantXMLConnectionDef.Edit: Boolean;
begin
  with TInstantXMLConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

{ TInstantXMLResolver }

function TInstantXMLResolver.CheckConflict(AObject: TInstantObject;
  const AObjectId: string;
  ConflictAction: TInstantConflictAction): Boolean;
begin
  Result := Broker.Connector.Connection.CheckConflict(AObject, FStorageName, AObjectId);

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
      Broker.Connector.Connection.DeleteInstantObject (AObject, FStorageName);
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
  end else if Map.IsRootMap and (ConflictAction = caFail) then
    raise EInstantConflict.CreateFmt(SDisposeConflict,
      [AObject.ClassName, AObject.PersistentId])
end;

procedure TInstantXMLResolver.InternalRetrieveMap(AObject: TInstantObject;
  const AObjectId: string; Map: TInstantAttributeMap;
  ConflictAction: TInstantConflictAction; Info: PInstantOperationInfo);
var
  AInfo: TInstantOperationInfo;
  Version: Integer;
begin
  if not Assigned(Info) then
  begin
    Info := @AInfo;
    Info.Conflict := False;
  end;
  //Read object from file
  Info.Success := Locate(AObject, AObjectId) and
    ReadInstantObject(AObject, AObjectId, Version);
  Info.Conflict := not Info.Success;
  if Info.Success then
  begin
    if Map.IsRootMap then
    begin
      Broker.SetObjectUpdateCount(AObject, Version);
    end;
  end else
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
      Info.Success := WriteInstantObject(AObject,NewId,Version);
      Info.Conflict := not Info.Success;
      if Map.IsRootMap then
        Broker.SetObjectUpdateCount(AObject, version);
    end
    else
      Info.Success := False;
  except
    on E: Exception do
    begin
      if E is EAbort then
        raise
      else begin
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

function TInstantXMLResolver.Locate(AObject : TObject; const AObjectId: string): Boolean;
begin
  Result := Broker.Connector.Connection.Locate(
    FStorageName, AObject.ClassName, AObjectId);
end;

function TInstantXMLResolver.ReadInstantObject(AObject : TInstantObject; const AObjectId: string;
  out Version : integer): boolean;
begin
  Result := Broker.Connector.Connection.ReadInstantObject(AObject, FStorageName, AObjectId, Version);
end;

procedure TInstantXMLResolver.ResetAttributes(AObject: TInstantObject;
  Map: TInstantAttributeMap);
begin

end;

function TInstantXMLResolver.WriteInstantObject(AObject: TInstantObject; const AObjectId: string;
  out Version : integer): boolean;
begin
  Result := Broker.Connector.Connection.WriteInstantObject(AObject, FStorageName, Version);
end;

{ TInstantXMLConnector }

procedure TInstantXMLConnector.CheckConnection;
begin
  if not assigned(Connection) then
    raise EPropertyError.Create(SUnassignedConnection);
end;

class function TInstantXMLConnector.ConnectionDefClass: TInstantConnectionDefClass;
begin
  Result := TInstantXMLConnectionDef;
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
var
  i : integer;
  StorageName : string;
begin
  CheckConnection;
  //build RootFolder if not exists
  if not DirectoryExists(Connection.RootFolder) and
    not ForceDirectories(Connection.RootFolder) then
    raise EInOutError.CreateFmt(SCannotCreateDirectory, [Connection.RootFolder]);

  //build SubFolder for each "storage name"
  for i := 0 to Scheme.TableMetadataCount -1 do
  begin
    StorageName := Scheme.TableMetadatas[i].Name;
    if not DirectoryExists(Connection.RootFolder+StorageName) then
      MkDir(Connection.RootFolder+StorageName);
  end;
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

destructor TInstantXMLQuery.Destroy;
begin
  DestroyObjectReferenceList;
  FParamsObject.Free;
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

procedure TInstantXMLQuery.InitObjectReferences(FileListAccessor : TStringList);
var
  i : integer;

  function CreateObjectReference(const FileName : string): TInstantObjectReference;
  var
    ClassName, ObjectId: string;
  begin
    ClassName := GetFileClassName(FileName);
    ObjectId := GetFileId(FileName);
    Result := TInstantObjectReference.Create(nil, True);
    try
      Result.ReferenceObject(ClassName, ObjectId);
    except
      Result.Free;
      raise
    end;
  end;

begin
  for i := 0 to FileListAccessor.Count -1 do
  begin
    ObjectReferenceList.Add(CreateObjectReference(FileListAccessor.Strings[i]));
  end;
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

function TInstantXMLQuery.InternalGetObjectCount: Integer;
begin
  Result := ObjectReferenceCount;
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
  FFileListAccessor : TStringList;
begin
  inherited;
  FFileListAccessor := TStringList.Create;
  try
    Connector.Connection.Open;
    Connector.Connection.LoadFileList (FFileListAccessor, StorageName);
    InitObjectReferences(FFileListAccessor);
  finally
    FFileListAccessor.Free;
  end;
end;

procedure TInstantXMLQuery.InternalRefreshObjects;
var
  I: Integer;
  BusyObjects: TObjectList;
begin
  BusyObjects := TObjectList.Create;
  try
    for I := 0 to Pred(ObjectReferenceCount) do
    begin
      with ObjectReferences[I] do
        if HasInstance and (Instance.RefCount > 1) then
          BusyObjects.Add(Instance);
    end;
    Close;
    Open;
    for I := 0 to Pred(BusyObjects.Count) do
      TInstantObject(BusyObjects[I]).Refresh;
  finally
    BusyObjects.Free;
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

class function TInstantXMLQuery.TranslatorClass: TInstantRelationalTranslatorClass;
begin
  Result := TInstantXMLTranslator;
end;

{ TXMLFilesAccessor }

function TXMLFilesAccessor.GetRootFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(FRootFolder);
end;

procedure TXMLFilesAccessor.SetRootFolder(const Value: string);
begin
  if FRootFolder <> Value then
  begin
    FRootFolder := Value;
  end;
end;

function TXMLFilesAccessor.SaveToFileXML_UTF8(AObject: TInstantObject;
  const FileName: string) : boolean;
var
  strstream: TStringStream;
  fileStream: TFileStream;
  DataStr : string;
begin
  strstream := TStringStream.Create('');
  try
    InstantWriteObject(strStream, sfXML, AObject);
{$IFNDEF VER130}
    if FXMLFileFormat in [xffUtf8, xffUtf8Bot] then
      DataStr := AnsiToUtf8(XML_UTF8_HEADER+strStream.DataString)
    else
      DataStr := XML_ISO_HEADER+strStream.DataString;
{$ELSE}
    DataStr := strStream.DataString;
{$ENDIF}
  finally
    strStream.Free;
  end;
  fileStream := TFileStream.Create(FileName, fmCreate);
  try
    Result := fileStream.Write (DataStr[1], Length (DataStr)) <> 0;
  finally
    fileStream.Free;
  end;
end;

function RemoveXmlDeclaration(const xmlString: string): string;
var
  nPos: Integer;
begin
  nPos := Pos('<?xml', xmlString);
  if nPos > 0 then
    Result := Copy(xmlString, Pos('?>', xmlString) + 2, maxint)
  else
    Result := xmlString;
end;

function TXMLFilesAccessor.LoadFromFileXML_UTF8(AObject: TInstantObject;
  const FileName: string) : boolean;
var
  fileStream: TFileStream;
  strUtf8: string;
  strstream: TStringStream;
begin
  fileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    // if FXMLFileFormat = xffUtf8Bot then
       // check/skip BOT
    SetLength(strUtf8,fileStream.Size);
    Result := fileStream.Read(strUtf8[1],fileStream.Size) <> 0;
    // skip XML HEADER (until the parser is "dumb")
    strUtf8 := RemoveXmlDeclaration (strUtf8);
  finally
    fileStream.Free;
  end;

{$IFNDEF VER130}
  if FXMLFileFormat in [xffUtf8, xffUtf8Bot] then
    strUtf8 := Utf8ToAnsi (strUtf8);
{$ENDIF}

  strstream := TStringStream.Create (strUtf8);
  try
    InstantReadObject(strstream, sfXML, AObject);
  finally
    strstream.Free;
  end;
end;

function TXMLFilesAccessor.ReadInstantObject(AObject : TInstantObject; const StorageName, AObjectId: string;
  out Version : integer): boolean;
var
  filename: string;
begin
  if FUseVersioning then
    filename := LastObjectFileName(StorageName, AObject.ClassName, AObjectId)
  else
    filename := PlainObjectFileName(StorageName, AObject.ClassName, AObjectId);
  Result := LoadFromFileXML_UTF8(AObject, filename);
  Version := VersionFromFilename(filename);
end;

function TXMLFilesAccessor.WriteInstantObject(AObject : TInstantObject; const StorageName: string;
  out Version : integer): boolean;
var
  filename: string;
begin
  MkStorageDir(StorageName);
  if FUseVersioning then
    filename := NewObjectFileName(StorageName,AObject.ClassName,AObject.Id)
  else
    filename := PlainObjectFileName(StorageName,AObject.ClassName,AObject.Id);
  Result := SavetoFileXML_UTF8(AObject,filename);
  Version := VersionFromFilename(filename);
end;

function TXMLFilesAccessor.Locate(const StorageName, AObjectClassName,
  AObjectId: string): Boolean;
var
  filename: string;
begin
  MkStorageDir(StorageName);
  if FUseVersioning then
  begin
    filename := LastObjectFileName(StorageName, AObjectClassName, AObjectId);
    Result := FileExists(filename) and (VersionFromFilename(filename) <> 0);
  end
  else
  begin
    filename := PlainObjectFileName(StorageName, AObjectClassName, AObjectId);
    Result := FileExists(filename);
  end;
end;

procedure TXMLFilesAccessor.MkStorageDir(const StorageName: string);
begin
  if not DirectoryExists(RootFolder+StorageName) then
    MkDir(RootFolder+StorageName);
end;

function TXMLFilesAccessor.LastObjectFileName(const StorageName, ClassName,
  Id: string): string;
var
  sr: TSearchRec;
  lastnum, nCurrent: Integer;
begin
  if FindFirst(
    RootFolder+StorageName+PathDelim+ClassName+'.'+Id+'.*'+DOT_XML_EXT,
    faAnyFile, SR) = 0 then
  try
    Result := RootFolder+StorageName+PathDelim+sr.Name;
    lastnum := VersionFromFileName (sr.Name);
    while FindNext(sr) = 0 do
    begin
      nCurrent := VersionFromFilename(sr.Name);
      // version "zero" means the file has been deleted
      if (nCurrent = 0) then
      begin
        Result := RootFolder+StorageName+PathDelim+sr.Name;
        Break;
      end;
      if nCurrent > lastnum then
      begin
        lastnum := nCurrent;
        Result := RootFolder+StorageName+PathDelim+sr.Name;
      end;
    end;
  finally
    FindClose (sr);
  end
  else
    Result := ''; // not found, new object/file
end;

function TXMLFilesAccessor.VersionFromFilename(const longfilename: string): Integer;
begin
  Result := GetFileVersion(ExtractFileName(longfilename));
end;

function TXMLFilesAccessor.NewObjectFileName(const StorageName, ClassName,
  Id: string): string;
var
  nVersion: Integer;
  filename: string;
begin
  // grab the current "last" version number
  if FUseVersioning then
  begin
    filename := LastObjectFileName(storagename, ClassName, Id);
    if filename <> '' then
    begin
      nVersion := VersionFromFilename (filename);
      inc(nVersion);
    end
    else
      nVersion := 1; // new object, first version
  end
  else
  begin
    filename := PlainObjectFileName(StorageName, ClassName, Id);
    nVersion := 1; // no version always 1
  end;
  Result := RootFolder+StorageName+PathDelim+ClassName+'.'+Id+'.'+IntToStr(nVersion)+DOT_XML_EXT;
end;

function TXMLFilesAccessor.DeleteInstantObject(AObject: TInstantObject;
  const StorageName: string) : boolean;
begin
  if UseVersioning then
  begin
    // save the document once more with 0 in the version name
    Result := SavetoFileXML_UTF8(AObject,
      DeleteObjectFileName(StorageName,AObject.ClassName,AObject.Id));
  end
  else
  begin
    // delete file from disk
    Result := DeleteFile(PlainObjectFileName(StorageName,AObject.ClassName,AObject.Id));
  end;
end;

function TXMLFilesAccessor.DeleteObjectFileName(const StorageName,
  ClassName, Id: string): string;
begin
  // mark deleted file as version 0
  Result := RootFolder+StorageName+PathDelim+ClassName+'.'+Id+'.'+
    IntToStr (0)+DOT_XML_EXT;
end;

constructor TXMLFilesAccessor.Create(AOwner: TComponent);
begin
  inherited;

  // default values for properties
  FUseVersioning := False;
  FXMLFileFormat := xffUtf8;
end;

function TXMLFilesAccessor.PlainObjectFileName(const StorageName,
  ClassName, Id: string): string;
begin
  // ignore versioning
  //FileName: ClassName.Id.UpdateCount.xml
  Result := RootFolder+StorageName+PathDelim+ClassName+'.'+Id+'.1'+DOT_XML_EXT;
end;

function TXMLFilesAccessor.CheckConflict(AObject: TInstantObject;
  const StorageName, AObjectId: string): Boolean;
var
  version: Integer;
begin
  if FUseVersioning then
  begin
    // check version of XML object
    version := GetFileVersion(LastObjectFileName
      (StorageName, AObject.ClassName, AObjectId));
    Result := (version = 0) or (Version <> AObject.UpdateCount);
  end
  else
    Result := False; // don't care about updatecount and versioning
end;

procedure TXMLFilesAccessor.LoadFileList(FFileListAccessor: TStringList; const StorageName: string);
//var
  // xmldom: TGeoXslProcess; - to be finished by marcoc
  //strXPath,  - to be finished by marcoc
  // result: string;
  // posOpenSquare, posCloseSquare: Integer;
begin
  if FUseVersioning then
    GlobalLoadFileListLastVersion (RootFolder + StorageName, FFileListAccessor)
  else
    GlobalLoadFileList (RootFolder + StorageName, FFileListAccessor);

  // tentative XPATH support by marcoc
    {posOpenSquare := Pos ('[', Statement);  - to be finished by marcoc
    posCloseSquare  := Pos (']', Statement);
    strXPath := Copy (Statement, posOpenSquare, posCloseSquare - posOpenSquare + 1);
    strXpath := '/' + GetObjectClassName + strXPath;
    for i := FFileListAccessor.Count - 1 downto 0 do
    begin
      xmldom := TGeoXslProcess.Create;
      try
        xmldom.LoadXmlFile(Connector.Connection.RootFolder+StorageName +
          PathDelim + FFileListAccessor[i]);
        result := xmldom.applyXPathToXml(strXpath);
        if result = '' then
          FFileListAccessor.Delete (i);
      finally
        xmldom.Free;
      end;
    end;  - to be finished by marcoc }

end;

procedure TXMLFilesAccessor.DoConnect;
begin
  //Check rootfolder
  if DirectoryExists(RootFolder) then
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
begin
  Result := inherited TranslateClassRef(ClassRef, Writer);
  if TablePathCount > 0 then
    (Query as TInstantXMLQuery).StorageName := ClassTablePath
  else
    (Query as TInstantXMLQuery).StorageName := '';
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
  vDatabaseName: String;
begin
  Connector.CheckConnection;
  vDatabaseName := Connector.DatabaseName;

  //build RootFolder if not exists
  if not DirectoryExists(vDatabaseName) and
    not ForceDirectories(vDatabaseName) then
    raise EInOutError.CreateFmt(SCannotCreateDirectory, [vDatabaseName]);

  // Create a subFolder for the "storage name"
  if not DirectoryExists(vDatabaseName + TableMetadata.Name) then
    MkDir(vDatabaseName + TableMetadata.Name);
end;

function TInstantDBBuildXMLDropTableCommand.GetTableMetadata:
    TInstantTableMetadata;
begin
  Result := OldMetadata as TInstantTableMetadata;
end;

procedure TInstantDBBuildXMLDropTableCommand.InternalExecute;
var
  vTableName: String;
  sr: TSearchRec;
  vDatabaseName: String;
begin
  Connector.CheckConnection;
  vDatabaseName := Connector.DatabaseName;

  // Delete subFolder for the "storage name"
  vTableName := vDatabaseName + TableMetadata.Name;
  if DirectoryExists(vTableName) then
  begin
    if FindFirst(vTableName + '\*.*', faAnyFile, sr) = 0 then
    begin
      repeat
        DeleteFile(vTableName + '\' + sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    RemoveDir(vTableName);
  end;
end;


initialization
  RegisterClass(TInstantXMLConnectionDef);
  TInstantXMLConnector.RegisterClass;

finalization
  TInstantXMLConnector.UnregisterClass;

end.
