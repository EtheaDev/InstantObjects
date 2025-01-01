(*
 *   InstantObjects
 *   ADO Tools
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
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantADOTools;

interface

uses
  Data.Win.ADODB
  , InstantADOJRO
  ;

type
  TInstantADOSyncType = (stSend, stReceive, stSendReceive);
  TInstantADOSyncMode = (smIndirect, smDirect, smInternet);
  TInstantADOReplicaType = (rtNotReplicable, rtDesignMaster, rtFull, rtPartial);
  TInstantADOReplicaVisibility = (rvGlobal, rvLocal, rvAnonymous);
  TInstantADOReplicaUpdatability = (ruReadWrite, ruReadOnly);

procedure InstantADOCompactDatabase(Connection: TADOConnection); overload;
procedure InstantADOCompactDatabase(const FileName: string); overload;
procedure InstantADOCreateReplica(const Source, Target, Description: string;
  ReplicaType: TInstantADOReplicaType; Visibility: TInstantADOReplicaVisibility;
  Priority: Integer; Updatability: TInstantADOReplicaUpdatability);
procedure InstantADOSynchronizeDatabases(const Source, Target: string;
  SyncType: TInstantADOSyncType; SyncMode: TInstantADOSyncMode);

implementation

uses
  WinApi.Windows
  , System.SysUtils
  ;
  
const
  SMSJetProvider = 'Microsoft.Jet.OLEDB.4.0';

procedure InstantADOCompactDatabase(Connection: TADOConnection);
var
  Engine: JetEngine;
  SourceConnStr, TargetConnStr: string;
  SourceFileName, TargetFileName: string;
begin
  Connection.Close;
  SourceConnStr := Connection.ConnectionString;
  SourceFileName := Connection.Properties['Data Source'].Value;
  TargetFileName := ChangeFileExt(SourceFileName, '.$$$');
  Connection.Properties['Data Source'].Value := TargetFileName;
  try
    TargetConnStr := Connection.ConnectionString;
  finally
    Connection.Properties['Data Source'].Value := SourceFileName;
  end;
  Engine := CoJetEngine.Create;
  Engine.CompactDatabase(SourceConnStr, TargetConnStr);
  DeleteFile(SourceFileName);
  RenameFile(TargetFileName, SourceFileName);
end;

procedure InstantADOCompactDatabase(const FileName: string); overload;
var
  Connection: TADOConnection;
begin
  Connection := TADOConnection.Create(nil);
  try
    Connection.ConnectionString := Format('Provider=%s;Data Source=%s',
      [SMSJetProvider, FileName]);
      InstantADOCompactDatabase(Connection);
  finally
    Connection.Free;
  end;
end;

procedure InstantADOCreateReplica(const Source, Target, Description: string;
  ReplicaType: TInstantADOReplicaType; Visibility: TInstantADOReplicaVisibility;
  Priority: Integer; Updatability: TInstantADOReplicaUpdatability);
const
  ReplicaTypes: array[TInstantADOReplicaType] of Cardinal = (
    jrRepTypeNotReplicable, jrRepTypeDesignMaster, jrRepTypeFull,
    jrRepTypePartial);
  Visibilities: array[TInstantADOReplicaVisibility] of Cardinal = (
    jrRepVisibilityGlobal, jrRepVisibilityLocal, jrRepVisibilityAnon);
  Updatabilities: array[TInstantADOReplicaUpdatability] of Cardinal = (
    jrRepUpdFull, jrRepUpdReadOnly);
var
  R: Replica;
begin
  R := CoReplica.Create;
  R._Set_ActiveConnection(Source);
  R.CreateReplica(Target, Description, ReplicaTypes[ReplicaType],
    Visibilities[Visibility], Priority, Updatabilities[Updatability]);
end;

procedure InstantADOSynchronizeDatabases(const Source, Target: string;
  SyncType: TInstantADOSyncType; SyncMode: TInstantADOSyncMode);
const
  SyncTypes: array[TInstantADOSyncType] of Cardinal = (
    jrSyncTypeExport, jrSyncTypeImport, jrSyncTypeImpExp);
  SyncModes: array[TInstantADOSyncMode] of Cardinal = (
    jrSyncModeIndirect, jrSyncModeDirect, jrSyncModeInternet);
var
  R: Replica;
begin
  R := CoReplica.Create;
  R._Set_ActiveConnection(Source);
  R.Synchronize(Target, SyncTypes[SyncType], SyncModes[SyncMode]);
end;

end.

