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
 * Contributor(s): Carlo Barazzetta
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantConnectionManager;

{$I InstantDefines.inc}

{$IFDEF D7+}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}
{$ENDIF}

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows,  
{$ENDIF}
{$IFDEF LINUX}
  QTypes, 
{$ENDIF}
  InstantClasses, InstantPersistence;

type
  TInstantConnectionManagerActionType = (atNew, atEdit, atDelete, atRename,
    atConnect, atDisconnect, atBuild, atOpen);
  TInstantConnectionManagerActionTypes = set of TInstantConnectionManagerActionType;

  TInstantConnectionDefEvent = procedure(Sender: TObject;
    var ConnectionDef: TInstantConnectionDef; var Result: Boolean) of object;
  TInstantConnectorEvent = procedure(Sender: TObject;
    Connector: TInstantConnector) of object;
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
    procedure SetFileName(const Value: string);
    procedure SetFileFormat(const Value: TInstantStreamFormat);
    procedure SetModel(Value: TInstantModel);
    procedure SetOnBuild(Value: TInstantConnectionDefEvent);
    procedure SetOnConnect(Value: TInstantConnectionDefEvent);
    procedure SetOnDisconnect(Value: TInstantConnectionDefEvent);
    procedure SetOnEdit(Value: TInstantConnectionDefEvent);
    procedure SetOnIsConnected(Value: TInstantConnectionDefEvent);
    procedure SetOnPrepare(Value: TInstantConnectorEvent);
    procedure SetOnSupportConnector(Value: TInstantConnectorClassEvent);
    procedure SetVisibleActions(Value: TInstantConnectionManagerActionTypes);
    procedure SetCaption(const Value: string);
    function GetDefsFileName: string;
    property DefsFileName: string read GetDefsFileName;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadConnectionDefs;
    procedure SaveConnectionDefs;    
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
      default [atNew, atEdit, atDelete, atRename, atConnect, atDisconnect, atBuild, atOpen];
    property OnBuild: TInstantConnectionDefEvent read FOnBuild write SetOnBuild;
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
    atDisconnect, atBuild, atOpen];
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
    if SameText(ExtractFileExt(Value), '.xml') then
      FFileFormat := sfXML
    else
      FFileFormat := sfBinary;
    FFileName := Value;
    LoadConnectionDefs;
  end;
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
  LoadConnectionDefs;
  ConnectionDef := ConnectionDefs.Find(ConnectionDefName) as TInstantConnectionDef;
  if Assigned(ConnectionDef) then
    OnConnect(Self, ConnectionDef, Result)
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
  FileStream: TFileStream;
  MemoryStream: TMemoryStream;
begin
  if FileExists(DefsFileName) then
  begin
    try
      if FileFormat = sfBinary then
      begin
        FileStream := TFileStream.Create(DefsFileName, fmOpenRead);
        try
          InstantReadObjectFromStream(FileStream, ConnectionDefs);
        finally
          FileStream.Free;
        end;
      end
      else
      begin
        MemoryStream := TMemoryStream.Create;
        try
          MemoryStream.LoadFromFile(DefsFileName);
          InstantReadObject(MemoryStream, sfXML, ConnectionDefs);
        finally
          MemoryStream.Free;
        end;
      end;
    except
      on E: Exception do
        raise EInstantError.CreateFmt(
          'Error loading connection definitions from %s: %s',
          [DefsFileName, E.Message]);
    end;
  end;
end;

procedure TInstantConnectionManager.SaveConnectionDefs;
var
  FileStream: TFileStream;
  MemoryStream: TMemoryStream;
begin
  if DefsFileName = '' then
    Exit;
  FileStream := TFileStream.Create(DefsFileName, fmCreate);
  try
    if FileFormat = sfBinary then
      InstantWriteObjectToStream(FileStream, ConnectionDefs)
    else
    begin
      MemoryStream := TMemoryStream.Create;
      try
        InstantWriteObjectToStream(MemoryStream, ConnectionDefs);
        MemoryStream.Position := 0;
        InstantObjectBinaryToText(MemoryStream, FileStream);
      finally
        MemoryStream.Free;
      end;
    end;
  finally
    FileStream.Free;
  end;
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
    // simply add InstantConnectionManagerForm in an uses section of your application.
    raise EInstantError.Create(SConnectionManagerExecutorNotAssigned);
end;

procedure TInstantConnectionManager.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

function TInstantConnectionManager.isConnected: boolean;
var
  i: Integer;
  ConnectionDef: TInstantConnectionDef;
begin
  Result := False;
  if not Assigned(OnIsConnected) then
    Exit;
  for i := 0 to ConnectionDefs.Count - 1 do
  begin
    ConnectionDef := ConnectionDefs.Items[i];
    OnIsConnected(Self, ConnectionDef, Result);
    if Result then
      Break;
  end;
end;

end.
