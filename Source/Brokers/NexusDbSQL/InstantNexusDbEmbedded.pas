(*
 *   InstantObjects
 *   NexusDb Support
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
 * The Original Code is: Bert Moorthaemer
 *
 * The Initial Developer of the Original Code is: Bert Moorthaemer
 *
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 * 
 * ***** END LICENSE BLOCK ***** *)

unit InstantNexusDbEmbedded;

{$I ../../InstantDefines.inc}
{$I InstantNxDbDefines.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, DB, InstantPersistence, InstantCommand, InstantNexusDb;

type
  { SQL based (Embedded) ------------------------------------------------------- }

  TInstantNexusDbSQLEmbeddedConnectionDef = class(
      TInstantNexusDbBaseConnectionDef)
  private
    class procedure CheckServerEngine;
  protected
    procedure InitConnector(Connector: TInstantConnector); override;
  public
    class function ConnectionTypeName: string; override;
    class function ConnectorClass: TInstantConnectorClass; override;
    function Edit: Boolean; override;
    class procedure LoadAliasList(aList: TStrings);
  end;

  TInstantNexusDbSQLEmbConnector = class(TInstantNexusDbSQLConnector)
  protected
    function GetDBMSName: string; override;
  public
    class function ConnectionDefClass: TInstantConnectionDefClass; override;
  end;

implementation

uses
  SysUtils,
  Controls,
  Forms,
  InstantConsts,
  InstantUtils,
  InstantNexusDbEmbeddedConnectionDefEdit,
  nxllTypes,
  nxsdTypes,
{$IFDEF NX1}
  nx1xAllEngines,
{$ELSE}
  nxseAllEngines,
{$ENDIF}
  nxsrServerEngine,
  nxchCommandHandler,
  nxSqlEngine,
  nxdb;

var
  __EmbeddedServer: TnxServerEngine = nil;

function CreateEmbeddedServer: TnxServerEngine;
begin
  Result := TnxServerEngine.Create(nil);
  with Result do
  try
    ServerName := '(Embedded)';
    Options :=
      [seoForceFailSafe,
      seoCloseInactiveFolders,
      seoCloseInactiveTables];
    SqlEngine := TnxSqlEngine.Create(Result);
    Active := True;
    SqlEngine.Active := True;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ SQL based (Embedded) ------------------------------------------------------- }

class procedure TInstantNexusDbSQLEmbeddedConnectionDef.CheckServerEngine;
begin
  if not Assigned(__EmbeddedServer) then
    __EmbeddedServer := CreateEmbeddedServer;
end;

class function TInstantNexusDbSQLEmbeddedConnectionDef.ConnectionTypeName:
  string;
begin
  Result := 'NexusDB (Embedded/SQL)';
end;

class function TInstantNexusDbSQLEmbeddedConnectionDef.ConnectorClass:
  TInstantConnectorClass;
begin
  Result := TInstantNexusDbSQLEmbConnector;
end;

function TInstantNexusDbSQLEmbeddedConnectionDef.Edit: Boolean;
begin
  with TInstantNexusDbEmbeddedConnectionDefEditForm.Create(nil) do
  try
    LoadData(Self);
    Result := ShowModal = mrOk;
    if Result then
      SaveData(Self);
  finally
    Free;
  end;
end;

procedure TInstantNexusDbSQLEmbeddedConnectionDef.InitConnector(Connector:
    TInstantConnector);
var
  Session: TnxSession;
  Database: TnxDatabase;
begin
  CheckServerEngine;
  Session := CreateSession(Connector, __EmbeddedServer);
  try
    Database := CreateDatabase(Connector, Session, Alias, AliasIsPath);
    try
      TInstantNexusDbSQLEmbConnector(Connector).Session := Session;
      TInstantNexusDbSQLEmbConnector(Connector).Database := Database;
    except
      Database.Free;
      raise;
    end;
  except
    Session.Free;
    raise;
  end;
end;

class procedure TInstantNexusDbSQLEmbeddedConnectionDef.LoadAliasList(aList:
    TStrings);
var
  Session: TnxSession;
begin
  CheckServerEngine;
  Session := CreateSession(nil, __EmbeddedServer);
  try
    aList.Clear;
    Session.GetAliasNames(aList);
  finally
    Session.Free;
  end;
end;

function TInstantNexusDbSQLEmbConnector.GetDBMSName: string;
begin
  Result := 'NexusDB (Embedded/SQL)';
end;

class function TInstantNexusDbSQLEmbConnector.ConnectionDefClass:
  TInstantConnectionDefClass;
begin
  Result := TInstantNexusDbSQLEmbeddedConnectionDef;
end;

initialization
  RegisterClass(TInstantNexusDbSQLEmbeddedConnectionDef);
  TInstantNexusDbSQLEmbConnector.RegisterClass;

finalization
  TInstantNexusDbSQLEmbConnector.UnregisterClass;
  if Assigned(__EmbeddedServer) then
    FreeAndNil(__EmbeddedServer);

end.


