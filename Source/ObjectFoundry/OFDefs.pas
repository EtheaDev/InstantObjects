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

unit OFDefs;

{$I ObjectFoundry.inc}

interface

uses
  MMToolsAPI
  {$IFDEF MM7+}
  , MMDiagramAPI
  {$ENDIF}
  ;

type
{$IFDEF MM7+}
  {$IFDEF MM9}
  IMMUnit = IMMModule;
  IMMUnitManager = IMMModuleManager;
  IMMV9ClassBase = IMMClassifier;
  IMMV9CodeModel = IMMCodeModel;
  IMMClassBase = IMMClassifier;
  {$ELSE}
  IMMV9ClassBase = IMMClassBase;
  IMMV9CodeModel = IMMCodeModel;
  {$ENDIF}
{$ELSE}
  TMMActionData = record
    Caption: WideString; // ModelMaker provides a defaults name based on to the menu item name
    ImageIndex: Integer; // Default = -1; Only used for toolbuttons, ignored for menu items
    Hint: WideString; // Default = ''
    Checked: Boolean; // Default = False
    Enabled: Boolean; // Default = True
    Visible: Boolean; // Default = True
    Updated: Boolean; // Default = False. Setting Updated = True lets MM call GetActionData
                      // before the menu item is being shown
    ShortCut: Word; // Mapped on TShortCut, Default = 0 (none) ShortCut is static and not updated.
  end;
{$ENDIF}

  IOFReference = IMMReference;
  IOFEntityReference = IMMEntityReference;

implementation

end.
