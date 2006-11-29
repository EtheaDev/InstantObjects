(*
 *   InstantObjects
 *   Object Foundry Expert
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
 * The Original Code is: Seleqt InstantObjects/Object Foundry Expert
 *
 * The Initial Developer of the Original Code is: Seleqt
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Steven Mitchell
 *
 * ***** END LICENSE BLOCK ***** *)

unit OFReg;

{$I ObjectFoundry.inc}

interface

uses
  MMToolsAPI;

procedure InitializeExpert(const Srv: IMMToolServices); stdcall;
procedure FinalizeExpert; stdcall;
function ExpertVersion: LongInt; stdcall;

exports
  InitializeExpert name MMExpertEntryProcName,
  FinalizeExpert name MMExpertExitProcName,
  ExpertVersion name MMExpertVersionProcName;

implementation

uses
  Forms, OFExpert, OFNotify, OFCritic;

procedure InitializeExpert(const Srv: IMMToolServices); stdcall;
begin
  // Copy interface pointer to initialize global var in MMToolsApi.pas
  MMToolServices := Srv;
  // Register the expert
  Srv.AddExpert(TObjectFoundryExpert.Create);
  // Create a project notifier and register it
  // It will work independent - MM will control it's life cycle.
  Srv.AddProjectNotifier(TProjectNotifier.Create);
  // Add an ObjectFoundry design critic which performs some basic naming convention checks
  // It will work independent - MM will control it's life cycle.
  MMToolServices.CriticManager.AddCritic(TObjectFoundryCritic.Create);
  // now sync with parent window, if we omit this, modal dialogs won't be really modal and
  // modeless forms will behave even worse.
  Application.Handle := Srv.GetParentHandle;
end;

procedure FinalizeExpert; stdcall;
begin
  // there's no need to export this function is there's nothing to clean up
  // In this demo expert all the cleaning-up is done by the Expert.Destroyed
end;

function ExpertVersion: LongInt; stdcall;
begin
  // This funciton and it's implementation are mandatory, if this function is not
  // exported or the version mismatches the version in ModelMaker, the expert is not
  // loaded
  Result := MMToolsApiVersion;
end;

end.
