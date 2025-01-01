(*
 *   InstantObjects
 *   Design Tools
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
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantDesignTools;

{$I '..\InstantDefines.inc'}

interface

uses
  System.Classes
  , DesignIntf
  , DesignEditors
  , StrEdit
  , ColnEdit
  , InstantPresentation
  ;

type
  TInstantSelectorEditor = class(TComponentEditor)
  private
    function GetSelector: TInstantSelector;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    property Selector: TInstantSelector read GetSelector;
  end;

  TInstantSelectorCommandProperty = class(TStringListProperty)
  public
    procedure Edit; override;
  end;

  TInstantSelectorParamsProperty = class(TCollectionProperty)
  public
    function GetColOptions: TColOptions; override;
  end;

  TInstantClassNameProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Vcl.Controls
  , Data.DB
  , InstantCommandEditor
  , InstantDesignHook
  ;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TStrings), TInstantSelector,
    'Command', TInstantSelectorCommandProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TInstantSelector,
    'Params', TInstantSelectorParamsProperty);
  RegisterPropertyEditor(TypeInfo(string), TInstantCustomExposer,
    'ObjectClassName', TInstantClassNameProperty);
end;

function EditCommand(var Command: string): Boolean;
begin
  with TInstantCommandEditorForm.Create(nil) do
  try
    if Assigned(DesignModel) then
      Model := DesignModel^;
    CommandText := Command;
    Result := ShowModal = mrOk;
    if Result then
      Command := CommandText;
  finally
    Free;
  end;
end;

{ TInstantSelectorEditor }

procedure TInstantSelectorEditor.ExecuteVerb(Index: Integer);

  procedure CommandEditor;
  var
    Command: string;
  begin
    Command := Selector.Command.Text;
    if EditCommand(Command) then
    begin
      Selector.Command.Text := Command;
      Designer.Modified;
    end;
  end;

begin
  case Index of
    0: CommandEditor;
  end;
end;

function TInstantSelectorEditor.GetSelector: TInstantSelector;
begin
  Result := Component as TInstantSelector;
end;

function TInstantSelectorEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := '&Command Editor...';
  end;
end;

function TInstantSelectorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TInstantSelectorCommandProperty }

procedure TInstantSelectorCommandProperty.Edit;
var
  Command: TStrings;
  S: string;
begin
  Command := TStrings(GetOrdValue);
  S := Command.Text;
  if EditCommand(S) then
  begin
    Command.Text := S;
    Designer.Modified;
  end;
end;

{ TInstantSelectorParamsProperty }

function TInstantSelectorParamsProperty.GetColOptions: TColOptions;
begin
  Result := [];
end;

{ TInstantClassNameProperty }

function TInstantClassNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TInstantClassNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  if Assigned(DesignModel) then
    with DesignModel^ do
      for I := 0 to Pred(ClassCount) do
        Proc(Classes[I].Name);
end;

end.
