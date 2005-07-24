(*
 *   InstantObjects
 *   Database builder Form
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
 * The Original Code is: InstantObjects database builder form
 *
 * The Initial Developer of the Original Code is: Nando Dessena
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDBBuilderFormUnit;

{$I ../../InstantDefines.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF D6+} Variants,{$ENDIF}
 Classes, Graphics, Controls, Forms,
  Dialogs, InstantCustomDBEvolverFormUnit, ActnList, InstantDBBuild,
  InstantDBEvolution, StdCtrls, ComCtrls, InstantConsts;


type
  TInstantDBBuilderForm = class(TInstantCustomDBEvolverForm)
    DBBuilder: TInstantDBBuilder;
    procedure DBBuilderBeforeCommandSequenceExecute(Sender: TObject);
    procedure BuildActionExecute(Sender: TObject);
  private
  protected
    function GetCustomDBEvolver: TInstantCustomDBEvolver; override;
  public
  end;

implementation

{$R *.dfm}

{ TInstantDBBuilderForm }

function TInstantDBBuilderForm.GetCustomDBEvolver: TInstantCustomDBEvolver;
begin
  Result := DBBuilder;
end;

procedure TInstantDBBuilderForm.DBBuilderBeforeCommandSequenceExecute(
  Sender: TObject);
begin
  inherited;
  if not Connector.DatabaseExists then
    Connector.CreateDatabase;
end;

procedure TInstantDBBuilderForm.BuildActionExecute(Sender: TObject);
begin
  if ConfirmDlg('Build database?' + sLineBreak + sLineBreak +
    'Warning: if the database already exists, all data in it will be lost!' + sLineBreak +
    'Use the "Evolve" feature to upgrade the structure of an existing database without loosing any data.') then
  begin
    inherited;
    ShowMessage('Database built without errors.');
  end;
end;

end.
