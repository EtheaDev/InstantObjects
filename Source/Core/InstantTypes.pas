(*
 *   InstantObjects
 *   Types
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
 * Carlo Barazzetta, Andrea Petrelli, Nando Dessena, Steven Mitchell,
 * Joao Morais, Cesar Coll, Uberto Barbini, David Taylor, Hanedi Salas,
 * Riceball Lee, David Moorhouse, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantTypes;

{$IFDEF LINUX}
{$I '../InstantDefines.inc'}
{$ELSE}
{$I '..\InstantDefines.inc'}
{$ENDIF}

interface

type
  {$IFNDEF D6+}
  IInterface = interface (IUnknown)
  end;
  {$ENDIF}

  TInstantStorageKind = (skEmbedded, skExternal);
  TInstantAttributeType = (atUnknown, atInteger, atFloat, atCurrency, atBoolean,
    atString, atDateTime, atBlob, atMemo, atGraphic,
    atPart, atReference, atParts, atReferences, atDate, atTime, atEnum);
  TInstantAttributeCategory = (acUnknown, acSimple, acElement, acContainer);

  TInstantGraphicFileFormat = (gffUnknown, gffBmp, gffTiff, gffJpeg, gffPng,
    gffDcx, gffPcx, gffEmf, gffGif, gffIco);

  TInstantPersistence = (peEmbedded, peStored);

  TInstantDataType = (dtInteger, dtFloat, dtCurrency, dtBoolean, dtString,
    dtMemo, dtDateTime, dtBlob, dtDate, dtTime, dtEnum);
  TInstantDataTypes = set of TInstantDataType;
  TInstantFieldOption = (foRequired, foIndexed);
  TInstantFieldOptions = set of TInstantFieldOption;

  TInstantCatalogFeature = (cfReadTableInfo, cfReadColumnInfo, cfReadIndexInfo);
  TInstantCatalogFeatures = set of TInstantCatalogFeature;

  // ToDo: Add ctAddRef to help file.
  TInstantContentChangeType = (ctAdd, ctAddRef, ctRemove, ctReplace, ctClear);

  TInstantOperationType = (otNone, otCreate, otStore, otRetrieve, otRefresh,
    otDispose);
  TInstantErrorAction = (eaRetry, eaIgnore, eaError, eaRevert, eaCancel);
  TInstantVerificationResult = (vrOk, vrCancel, vrAbort, vrError);
  TInstantConflictAction = (caIgnore, caFail);

  TInstantCacheNodeColor = (ncRed, ncBlack);

  TInstantDBBuildCommandType = (ctAddTable, ctDropTable, ctAddField,
    ctAlterField, ctDropField, ctAddIndex, ctAlterIndex, ctDropIndex);

  TInstantObjectNotification = (onChanged, onCreated, onDisposed, onRefreshed,
    onRetrieved, onStored);

  TInstantWarningEvent = procedure (const Sender: TObject;
    const AWarningText: string) of object;

  TTime = type TDateTime;
  TDate = type TDateTime;

implementation

end.
