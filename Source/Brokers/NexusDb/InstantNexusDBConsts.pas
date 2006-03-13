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

unit InstantNexusDBConsts;

{$IFDEF LINUX}
{$I '../../InstantDefines.inc'}
{$ELSE}
{$I '..\..\InstantDefines.inc'}
{$ENDIF}
{$I InstantNexusDBDefines.inc}

interface

resourcestring
  SLoadAvailableAliases = 'Load available aliases';
  SLoadAvailableNexusDBServers = 'Load available NexusDB servers';
  SLoadingAliases = 'Loading Aliases ...';
  SLoadingServers = 'Loading Servers ...';
  SNone = '[None]';
  SSelectAliasFromList = '[Select an alias from the list]';
  SSelectAnAliasPath = 'Select an alias path';
  SSelectServerFromList = '[Select a server from the list]';

implementation

end.
