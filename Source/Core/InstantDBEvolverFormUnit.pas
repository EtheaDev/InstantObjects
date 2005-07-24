(*
 *   InstantObjects
 *   Database evolution Form
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
 * The Original Code is: InstantObjects database evolver form
 *
 * The Initial Developer of the Original Code is: Nando Dessena
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBEvolverFormUnit;

{$I ../../InstantDefines.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF D6+} Variants, {$ENDIF}
 Classes, Graphics, Controls, Forms,
  Dialogs, InstantCustomDBEvolverFormUnit, ActnList, InstantDBBuild,
  InstantDBEvolution, StdCtrls, ComCtrls;

type
  TInstantDBEvolverForm = class(TInstantCustomDBEvolverForm)
    DBEvolver: TInstantDBEvolver;
    procedure BuildActionExecute(Sender: TObject);
  private
  protected
    function GetCustomDBEvolver: TInstantCustomDBEvolver; override;
  public
  end;

implementation

{$R *.dfm}

{ TInstantDBEvolverForm }

function TInstantDBEvolverForm.GetCustomDBEvolver: TInstantCustomDBEvolver;
begin
  Result := DBEvolver;
end;

procedure TInstantDBEvolverForm.BuildActionExecute(Sender: TObject);
begin
  if ConfirmDlg('Evolve database?') then
  begin
    inherited;
    ShowMessage('Database evolved without errors.');
  end;
end;

end.
