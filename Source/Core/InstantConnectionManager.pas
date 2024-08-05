(*
 *   InstantObjects
 *   Connection Manager Component
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
 * Carlo Barazzetta, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantConnectionManager;

{$IFDEF LINUX64}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  Windows,  
  InstantClasses, InstantPersistence, InstantMetadata;

type
  TInstantConnectionManagerActionType = (atNew, atEdit, atDelete, atRename,
    atConnect, atDisconnect, atBuild, atEvolve, atOpen);
  TInstantConnectionManagerActionTypes = set of TInstantConnectionManagerActionType;

  TInstantConnectionDefEvent = procedure(Sender: TObject;
    var ConnectionDef: TInstantConnectionDef; var Result: Boolean) of object;
  TInstantConnectorClassEvent = procedure(Sender: TObject;
    ConnectorClass: TInstantConnectorClass; var Result: Boolean) of object;

  TInstantConnectionManager = class;
  TInstantConnectionManagerExecutor = procedure (ConnectionManager: TInstantConnectionManager);

  TInstantConnectionManager = class(TComponent)
  private
    FCaption: string;
    FFileName: string;
    FFileFormat: TInstantStreamFormat;
    FModel: TInstantModel;
    FOnBuild: TInstantConnectionDefEvent;
    FOnEvolve: TInstantConnectionDefEvent;
    FOnConnect: TInstantConnectionDefEvent;
    FOnDisconnect: TInstantConnectionDefEvent;
    FOnEdit: TInstantConnectionDefEvent;
    FOnIsConnected: TInstantConnectionDefEvent;
    FOnPrepare: TInstantConnectorEvent;
    FOnSupportConnector: TInstantConnectorClassEvent;
    FVisibleActions: TInstantConnectionManagerActionTypes;
    FConnectionDefs: TInstantConnectionDefs;
    function GetConnectionDefs: TInstantConnectionDefs;
    function GetModel: TInstantModel;
    procedure SetFileFormat(const Value: TInstantStreamFormat);
    procedure SetFileName(const Value: string);
    procedure SetModel(Value: TInstantModel);
    procedure SetOnBuild(Value: TInstantConnectionDefEvent);
    procedure SetOnEvolve(Value: TInstantConnectionDefEvent);
    procedure SetOnConnect(Value: TInstantConnectionDefEvent);
    procedure SetOnDisconnect(Value: TInstantConnectionDefEvent);
    procedure SetOnEdit(Value: TInstantConnectionDefEvent);
    procedure SetOnIsConnected(Value: TInstantConnectionDefEvent);
    procedure SetOnPrepare(Value: TInstantConnectorEvent);
    procedure SetOnSupportConnector(Value: TInstantConnectorClassEvent);
    procedure SetVisibleActions(Value: TInstantConnectionManagerActionTypes);
    procedure SetCaption(const Value: string);
    function GetDefsFileName: string;
  protected
    // Sets FileFormat based on FileName.
    procedure SetFileFormatFromFileName;
    // Called when FileName changes. The default implementation calls
    // SetFileFormatFromFileName and LoadConnectionDefs to auto-load the
    // connection definitions.
    // This method may be overridden to skip auto-loading in cases where
    // more than just the file name is needed in order to call
    // LoadConnectionDefs.
    procedure AfterFileNameChange; virtual;
    // Fully qualified FileName.
    property DefsFileName: string read GetDefsFileName;
    // Creates and returns a stream to read the connectiondefs data from.
    // If there is nothing to read, it should return nil.
    // The caller is responsible for freeing the stream after using it.
    // The default implementation just creates a TFileStream that opens
    // DefsFileName for reading. If DefsFileName does not exist, then it returns
    // nil.
    // An overridden implementation might get the data from an encrypted file or
    // a completely different source.
    function CreateConnectionDefsInputStream(): TStream; virtual;
    // Creates and returns a stream to write the connectiondefs data to.
    // If there is nothing to write to, it should return nil.
    // The caller is responsible for freeing the stream after using it.
    // The default implementation just creates a TFileStream that opens
    // DefsFileName for writing. If DefsFileName is not specified, then it
    // returns nil.
    // An overridden implementation might write the data to an encrypted file
    // or a completely different destination.
    function CreateConnectionDefsOutputStream(): TStream; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadConnectionDefs;
    procedure LoadConnectionDefsFromStream(const AStream: TStream);
    procedure SaveConnectionDefs;
    procedure SaveConnectionDefsToStream(const AStream: TStream);
    procedure ConnectByName(const ConnectionDefName: string);
    procedure Execute;
    function IsConnected: Boolean;
    property Model: TInstantModel read GetModel write SetModel;
    property ConnectionDefs: TInstantConnectionDefs read GetConnectionDefs;
  published
    property Caption: string read FCaption write SetCaption;
    property FileName: string read FFileName write SetFileName;
    property FileFormat : TInstantStreamFormat
      read FFileFormat write SetFileFormat default sfBinary;
    property VisibleActions: TInstantConnectionManagerActionTypes
      read FVisibleActions write SetVisibleActions
      default [atNew, atEdit, atDelete, atRename, atConnect, atDisconnect, atBuild, atEvolve, atOpen];
    property OnBuild: TInstantConnectionDefEvent read FOnBuild write SetOnBuild;
    property OnEvolve: TInstantConnectionDefEvent read FOnEvolve write SetOnEvolve;
    property OnConnect: TInstantConnectionDefEvent read FOnConnect write SetOnConnect;
    property OnDisconnect: TInstantConnectionDefEvent
      read FOnDisconnect write SetOnDisconnect;
    property OnEdit: TInstantConnectionDefEvent read FOnEdit write SetOnEdit;
    property OnIsConnected: TInstantConnectionDefEvent
      read FOnIsConnected write SetOnIsConnected;
    property OnPrepare: TInstantConnectorEvent read FOnPrepare write SetOnPrepare;
    property OnSupportConnector: TInstantConnectorClassEvent
      read FOnSupportConnector write SetOnSupportConnector;
  end;

procedure RegisterConnectionManagerExecutor(ConnectionManagerExecutor: TInstantConnectionManagerExecutor);

implementation

uses
  InstantConsts;

var
  _ConnectionManagerExecutor: TInstantConnectionManagerExecutor;

procedure RegisterConnectionManagerExecutor(ConnectionManagerExecutor: TInstantConnectionManagerExecutor);
begin
  _ConnectionManagerExecutor := ConnectionManagerExecutor;
end;

{ TInstantConnectionManager }

constructor TInstantConnectionManager.Create(AOwner: TComponent);
begin
  inherited;
  FVisibleActions := [atNew, atEdit, atDelete, atRename, atConnect,
    atDisconnect, atBuild, atEvolve, atOpen];
  FFileFormat := sfBinary;
end;

function TInstantConnectionManager.GetModel: TInstantModel;
begin
  Result := FModel;
  if not Assigned(FModel) then
    Result := InstantModel;
end;

procedure TInstantConnectionManager.SetFileName(const Value: string);
begin
  if Value <> FFileName then
  begin
    FFileName := Value;
    AfterFileNameChange;
  end;
end;

procedure TInstantConnectionManager.SetFileFormatFromFileName;
begin
  if SameText(ExtractFileExt(FFileName), '.xml') then
    FFileFormat := sfXML
  else
    FFileFormat := sfBinary;
end;

procedure TInstantConnectionManager.AfterFileNameChange;
begin
  SetFileFormatFromFileName;
  LoadConnectionDefs;
end;

procedure TInstantConnectionManager.SetFileFormat(
  const Value: TInstantStreamFormat);
begin
  FFileFormat := Value;
end;

procedure TInstantConnectionManager.SetModel(Value: TInstantModel);
begin
  FModel := Value;
end;

procedure TInstantConnectionManager.SetOnBuild(
  Value: TInstantConnectionDefEvent);
begin
  FOnBuild := Value;
end;

procedure TInstantConnectionManager.SetOnEvolve(
  Value: TInstantConnectionDefEvent);
begin
  FOnEvolve := Value;
end;

procedure TInstantConnectionManager.SetOnConnect(
  Value: TInstantConnectionDefEvent);
begin
  FOnConnect := Value;
end;

procedure TInstantConnectionManager.SetOnDisconnect(
  Value: TInstantConnectionDefEvent);
begin
  FOnDisconnect := Value;
end;

procedure TInstantConnectionManager.SetOnEdit(
  Value: TInstantConnectionDefEvent);
begin
  FOnEdit := Value;
end;

procedure TInstantConnectionManager.SetOnIsConnected(
  Value: TInstantConnectionDefEvent);
begin
  FOnIsConnected := Value;
end;

procedure TInstantConnectionManager.SetOnPrepare(
  Value: TInstantConnectorEvent);
begin
  FOnPrepare := Value;
end;

procedure TInstantConnectionManager.SetOnSupportConnector(
  Value: TInstantConnectorClassEvent);
begin
  FOnSupportConnector := Value;
end;

procedure TInstantConnectionManager.SetVisibleActions(
  Value: TInstantConnectionManagerActionTypes);
begin
  FVisibleActions := Value;
end;

procedure TInstantConnectionManager.ConnectByName(const ConnectionDefName: string);
var
  Result: Boolean;
  ConnectionDef : TInstantConnectionDef;
begin
  //Load connectiondefs only if connections definitions are not already loaded
  if ConnectionDefs.Count = 0 then
    LoadConnectionDefs;
  ConnectionDef := ConnectionDefs.Find(ConnectionDefName) as TInstantConnectionDef;
  if Assigned(ConnectionDef) then
  begin
    if Assigned(OnConnect) then
      OnConnect(Self, ConnectionDef, Result)
    else
      raise EInstantError.Create(SUnassignedOnConnectError);
  end
  else
    raise EInstantError.CreateFmt(SConnectionDefError, [ConnectionDefName, FileName]);
end;

destructor TInstantConnectionManager.Destroy;
begin
  FConnectionDefs.Free;
  inherited;
end;

function TInstantConnectionManager.GetConnectionDefs: TInstantConnectionDefs;
begin
  if not Assigned(FConnectionDefs) then
    FConnectionDefs := TInstantConnectionDefs.Create;
  Result := FConnectionDefs;
end;

procedure TInstantConnectionManager.LoadConnectionDefs;
var
  InputStream: TStream;
begin
  try
    InputStream := CreateConnectionDefsInputStream();
    try
      if Assigned(InputStream) then
        LoadConnectionDefsFromStream(InputStream);
    finally
      InputStream.Free;
    end;
  except
    on E: Exception do
      raise EInstantError.CreateFmt(SErrorLoadingConnectionDefs,
        [DefsFileName, E.Message]);
  end;
end;

procedure TInstantConnectionManager.LoadConnectionDefsFromStream(
  const AStream: TStream);
begin
  Assert(Assigned(AStream));

  try
    InstantReadObject(AStream, FileFormat, ConnectionDefs);
  except
    on E: Exception do
      raise EInstantError.CreateFmt(SErrorLoadingConnectionDefs,
        [AStream.ClassName, E.Message]);
  end;
end;

procedure TInstantConnectionManager.SaveConnectionDefs;
var
  OutputStream: TStream;
begin
  OutputStream := CreateConnectionDefsOutputStream();
  try
    if Assigned(OutputStream) then
      SaveConnectionDefsToStream(OutputStream);
  finally
    OutputStream.Free;
  end;
end;

procedure TInstantConnectionManager.SaveConnectionDefsToStream(
  const AStream: TStream);
begin
  Assert(Assigned(AStream));
  InstantWriteObject(AStream, FileFormat, ConnectionDefs);
end;

function TInstantConnectionManager.GetDefsFileName: string;
var
  Path: string;
begin
  if FileName <> '' then
  begin
    Path := ExtractFilePath(FileName);
    if Path = '' then
      Result := ExtractFilePath(ParamStr(0)) + FileName
    else
      Result := FileName;
  end
  else
    Result := '';
end;

procedure TInstantConnectionManager.Execute;
begin
  if Assigned(_ConnectionManagerExecutor) then
    _ConnectionManagerExecutor(Self)
  else
    // WARNING:
    // If you want to use the default ConnectionManagerForm (as in previous versions of IO)
    // simply add InstantConnectionManagerFormUnit in an uses section of your application.
    raise EInstantError.Create(SConnectionManagerExecutorNotAssigned);
end;

procedure TInstantConnectionManager.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

function TInstantConnectionManager.IsConnected: Boolean;
var
  i: Integer;
  ConnectionDef: TInstantConnectionDef;
begin
  Result := False;
  if not Assigned(OnIsConnected) then
    Exit;
  for i := 0 to Pred(ConnectionDefs.Count) do
  begin
    ConnectionDef := ConnectionDefs.Items[i];
    OnIsConnected(Self, ConnectionDef, Result);
    if Result then
      Break;
  end;
end;

function TInstantConnectionManager.CreateConnectionDefsInputStream: TStream;
begin
  if FileExists(DefsFileName) then
    Result := TFileStream.Create(DefsFileName, fmOpenRead or fmShareDenyNone)
  else
    Result := nil;
end;

function TInstantConnectionManager.CreateConnectionDefsOutputStream: TStream;
begin
  if DefsFileName <> '' then
    Result := TFileStream.Create(DefsFileName, fmCreate)
  else
    Result := nil;
end;

end.
