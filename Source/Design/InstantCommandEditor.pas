(*
 *   InstantObjects
 *   Command Editor
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
 * Carlo Barazzetta, Adrea Petrelli, Nando Dessena, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantCommandEditor;

{$I '..\InstantDefines.inc'}

interface

uses
  System.SysUtils
  , System.Classes
  , WinApi.Windows
  , WinApi.Messages
  , Vcl.Graphics
  , Vcl.Controls
  , Vcl.Forms
  , Vcl.Dialogs
  , Vcl.StdCtrls
  , Vcl.ExtCtrls
  , InstantCode
  , InstantCommand
  ;

type
  TInstantCommandEditorForm = class(TForm)
    CommandTextEdit: TMemo;
    BottomPanel: TPanel;
    TopPanel: TPanel;
    FromClassLabel: TLabel;
    FromClassEdit: TComboBox;
    AnyCheckBox: TCheckBox;
    AttributeLabel: TLabel;
    AttributeEdit: TComboBox;
    DistinctCheckBox: TCheckBox;
    CommandTextLabel: TLabel;
    ButtonsPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    procedure CommandTextEditChange(Sender: TObject);
    procedure FromClassEditClick(Sender: TObject);
    procedure AttributeEditClick(Sender: TObject);
    procedure AnyCheckBoxClick(Sender: TObject);
    procedure DistinctCheckBoxClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCommand: TInstantIQLCommand;
    FModel: TInstantCodeModel;
    procedure DestroyCommand;
    function GetAny: Boolean;
    function GetAttributeName: string;
    function GetCommand: TInstantIQLCommand;
    function GetCommandText: string;
    function GetDefaultText(AttributeName, ClassName: string): string;
    function GetDistinct: Boolean;
    function GetFromClass: TInstantCodeClass;
    function GetFromClassName: string;
    function GetFromText: string;
    function GetSelectText: string;
    procedure SetAny(const Value: Boolean);
    procedure SetAttributeName(const Value: string);
    procedure SetCommandText(const Value: string);
    procedure SetDistinct(const Value: Boolean);
    procedure SetFromClassName(const Value: string);
    procedure SetModel(Value: TInstantCodeModel);
  protected
    function ChangeArgument(var Text: string; const Keyword, Argument: string;
      Replace: Boolean = True): Boolean;
    procedure CommandChanged;
    function FindArgument(const Text, Keyword: string; out Start, Len: Integer): Boolean;
    procedure PopulateClasses;
    procedure PopulateAttributes;
    procedure Validate;
    property Command: TInstantIQLCommand read GetCommand;
    property FromText: string read GetFromText;
    property SelectText: string read GetSelectText;
  public
    property Any: Boolean read GetAny write SetAny;
    property AttributeName: string read GetAttributeName write SetAttributeName;
    property CommandText: string read GetCommandText write SetCommandText;
    property Distinct: Boolean read GetDistinct write SetDistinct;
    property FromClass: TInstantCodeClass read GetFromClass;
    property FromClassName: string read GetFromClassName write SetFromClassName;
    property Model: TInstantCodeModel read FModel write SetModel;
  end;

implementation

uses
  InstantPersistence, InstantPresentation, InstantMetadata, InstantTypes,
  InstantUtils;

{$R *.dfm}

procedure TInstantCommandEditorForm.AnyCheckBoxClick(Sender: TObject);
begin
  Any := AnyCheckBox.Checked;
end;

procedure TInstantCommandEditorForm.AttributeEditClick(Sender: TObject);
begin
  AttributeName := AttributeEdit.Text;
end;

function TInstantCommandEditorForm.ChangeArgument(var Text: string;
  const Keyword, Argument: string; Replace: Boolean): Boolean;
var
  Start, Len: Integer;
begin
  Result := FindArgument(Text, Keyword, Start, Len);
  if Result then
  begin
    if Replace then
      Delete(Text, Start, Len)
    else
      Insert(' ', Text, Start);
    if Argument = '' then
      Delete(Text, Start, 1)
    else
      Insert(Argument, Text, Start);
  end;
end;

procedure TInstantCommandEditorForm.CommandChanged;
begin
  DestroyCommand;
  with FromClassEdit do
    ItemIndex := Items.IndexOf(FromClassName);
  PopulateAttributes;
  AnyCheckBox.Checked := Any;
  DistinctCheckBox.Checked := Distinct;
end;

procedure TInstantCommandEditorForm.CommandTextEditChange(Sender: TObject);
begin
  CommandChanged;
end;

procedure TInstantCommandEditorForm.DestroyCommand;
begin
  FreeAndNil(FCommand);
end;

procedure TInstantCommandEditorForm.DistinctCheckBoxClick(Sender: TObject);
begin
  Distinct := DistinctCheckBox.Checked;
end;

function TInstantCommandEditorForm.FindArgument(
  const Text, Keyword: string; out Start, Len: Integer): Boolean;

  function IsSpace(Ch: Char): Boolean;
  begin
    Result := InstantCharInSet(Ch, [' ', #9, #10, #13]);
  end;

var
  I: Integer;
begin
  I := Pos(Keyword, UpperCase(Text));
  Result := I > 0;
  if Result then
  begin
    Inc(I, Length(Keyword));
    while (I <= Length(Text)) and (Text[I] = ' ') do
      Inc(I);
    Start := I;
    while (I <= Length(Text)) and not IsSpace(Text[I]) do
      Inc(I);
    Len := I - Start;
  end;
end;

procedure TInstantCommandEditorForm.FromClassEditClick(Sender: TObject);
begin
  FromClassName := FromClassEdit.Text;
  with AttributeEdit do
    if ItemIndex = -1 then
    begin
      ItemIndex := 0;
      AttributeEditClick(AttributeEdit);
    end;
end;

function TInstantCommandEditorForm.GetAny: Boolean;
begin
  Result := Command.Any;
end;

function TInstantCommandEditorForm.GetAttributeName: string;
begin
  Result := Command.Specifier.Text;
  if Result = '*' then
    Result := 'Self';
end;

function TInstantCommandEditorForm.GetCommand: TInstantIQLCommand;
begin
  if not Assigned(FCommand) then
  begin
    FCommand := TInstantIQLCommand.Create(nil);
    try
      FCommand.Text := CommandText;
    except
    end;
  end;
  Result := FCommand;
end;

function TInstantCommandEditorForm.GetCommandText: string;
begin
  Result := CommandTextEdit.Text;
end;

function TInstantCommandEditorForm.GetDefaultText(AttributeName,
  ClassName: string): string;
begin
  if AttributeName = '' then
    AttributeName := AttributeEdit.Text;
  if (AttributeName = '') or (AttributeName = 'Self') then
    AttributeName := '*';
  if ClassName = '' then
    ClassName := FromClassEdit.Text;
  if ClassName = '' then
    Result := ''
  else
    Result := SelectText + ' ' + AttributeName + ' ' + FromText + ' ' +
      ClassName;
end;

function TInstantCommandEditorForm.GetDistinct: Boolean;
begin
  Result := Command.Distinct;
end;

function TInstantCommandEditorForm.GetFromClass: TInstantCodeClass;
begin
  if Assigned(Model) then
    Result := Model.FindClass(FromClassName)
  else
    Result := nil;
end;

function TInstantCommandEditorForm.GetFromClassName: string;
begin
  Result := Command.ObjectClassName;
end;

procedure IncludeAttribute(AttributeMetadata: TInstantAttributeMetadata;
  var Include: Boolean; var Traverse: Boolean);
begin
  Include := AttributeMetadata.Category = acElement;
end;

function TInstantCommandEditorForm.GetFromText: string;
begin
  Result := 'FROM';
  if AnyCheckBox.Checked then
    Result := Result + ' ANY';
end;

function TInstantCommandEditorForm.GetSelectText: string;
begin
  Result := 'SELECT';
  if DistinctCheckBox.Checked then
    Result := Result + ' DISTINCT';
end;

procedure TInstantCommandEditorForm.OkButtonClick(Sender: TObject);
begin
  try
    Validate;
  except
    ModalResult := mrNone;
    raise;
  end;
end;

procedure TInstantCommandEditorForm.PopulateAttributes;
var
  AClass: TInstantCodeClass;
begin
  with AttributeEdit do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      AClass := FromClass;
      if Assigned(AClass) then
      begin
        Items.Add('Self');
        InstantGetAttributeList(AClass.Metadata, Items, IncludeAttribute);
        ItemIndex := Items.IndexOf(AttributeName);
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TInstantCommandEditorForm.PopulateClasses;
var
  I: Integer;
  AClass: TInstantCodeClass;
begin
  with FromClassEdit do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      if Assigned(Model) then
        with Model do
          for I := 0 to Pred(ClassCount) do
          begin
            AClass := Classes[I];
            if AClass.IsStored then
              Items.AddObject(AClass.Name, AClass);
          end;
      ItemIndex := Items.IndexOf(FromClassName);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TInstantCommandEditorForm.SetAny(const Value: Boolean);
var
  Text, Str: string;
begin
  if Value <> Any then
  begin
    if Value then
      Str := 'ANY'
    else
      Str := '';
    Text := CommandText;
    if not ChangeArgument(Text, 'FROM', Str, Str = '') then
      Text := GetDefaultText(AttributeEdit.Text, FromClassEdit.Text);
    CommandText := Text;
  end;
end;

procedure TInstantCommandEditorForm.SetAttributeName(const Value: string);
var
  Text, Str: string;
begin
  Text := CommandText;
  if Value = 'Self' then
    Str := '*'
  else
    Str := Value;
  if not ChangeArgument(Text, SelectText, Str) then
    Text := GetDefaultText(Str, '');
  CommandText := Text;
end;

procedure TInstantCommandEditorForm.SetCommandText(const Value: string);
begin
  if CommandTextEdit.Text <> Value then
  begin
    CommandTextEdit.Text := Value;
    CommandChanged;
  end;
end;

procedure TInstantCommandEditorForm.SetDistinct(const Value: Boolean);
var
  Text, Str: string;
begin
  if Value <> Distinct then
  begin
    if Value then
      Str := 'DISTINCT'
    else
      Str := '';
    Text := CommandText;
    if not ChangeArgument(Text, 'SELECT', Str, Str = '') then
      Text := GetDefaultText(AttributeEdit.Text, FromClassEdit.Text);
    CommandText := Text;
  end;
end;

procedure TInstantCommandEditorForm.SetFromClassName(
  const Value: string);
var
  Text: string;
begin
  Text := CommandText;
  if not ChangeArgument(Text, FromText, Value) then
    Text := GetDefaultText('', Value);
  CommandText := Text;
end;

procedure TInstantCommandEditorForm.SetModel(Value: TInstantCodeModel);
begin
  if Value <> FModel then
  begin
    FModel := Value;
    PopulateClasses;
  end;
end;

procedure TInstantCommandEditorForm.Validate;
begin
  with TInstantIQLCommand.Create(nil) do
  try
    Text := CommandText;
  finally
    Free;
  end;
end;

procedure TInstantCommandEditorForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  BorderStyle := bsDialog;
end;

end.
