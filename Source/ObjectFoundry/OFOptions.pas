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

unit OFOptions;

{$I ObjectFoundry.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  InstantEdit, Db, InstantPresentation, StdCtrls, ExtCtrls, ComCtrls, Mask,
  DBCtrls;

type
  TOFOptions = class(TPersistent)
  private
    FProjectFileName: string;
  public
    procedure Load;
    procedure Save;
  published
    property ProjectFileName: string read FProjectFileName write FProjectFileName;
  end;

  TOFOptionsForm = class(TInstantEditForm)
    PageControl: TPageControl;
    ProjectSheet: TTabSheet;
    ProjectFileEdit: TLabel;
    ProjectFileNameEdit: TDBEdit;
    ProjectFileButton: TButton;
  private
    function GetSubject: TOFOptions;
    procedure SetSubject(const Value: TOFOptions);
  public
    property Subject: TOFOptions read GetSubject write SetSubject;
  end;

implementation

uses
  MMToolsAPI;

{$R *.DFM}

{ TOFOptions }

procedure TOFOptions.Load;
begin

end;

procedure TOFOptions.Save;
begin

end;

{ TOFOptionsForm }

function TOFOptionsForm.GetSubject: TOFOptions;
begin
  Result := inherited Subject as TOFOptions;
end;

procedure TOFOptionsForm.SetSubject(const Value: TOFOptions);
begin
  inherited Subject := Value;
end;

end.
