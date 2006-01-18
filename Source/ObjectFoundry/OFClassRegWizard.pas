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

unit OFClassRegWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, MMToolsAPI, ComCtrls;

type
  TClassRegWizardForm = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    UnitListView: TListView;
    procedure OkButtonClick(Sender: TObject);
  private
    FUnitList: TInterfaceList;
    function GetCheckedUnitCount: Integer;
    function GetUnitCount: Integer;
    function GetUnitList: TInterfaceList;
    function GetUnits(Index: Integer): IMMUnit;
    property UnitList: TInterfaceList read GetUnitList;
  protected
    procedure GetCheckedUnits(List: TInterfaceList);
    function GetRegistrationCode(AUnit: IMMUnit): string;
    procedure LoadUnitList;
    procedure PopulateUnitList;
    procedure ProcessUnit(AUnit: IMMUnit);
    procedure Run;
    procedure UpdateActions; override;
    property CheckedUnitCount: Integer read GetCheckedUnitCount;
    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: IMMUnit read GetUnits;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCodeStrings = class(TStringList)
  private
    FIndentLevel: Integer;
    function IndentStr: string;
    procedure SetIndentLevel(Value: Integer);
  public
    function Add(const S: string): Integer; override;
    procedure BeginIndent;
    procedure EndIndent;
    function FindFirst(var Index: Integer; const S: string): Boolean;
    function FindLast(var Index: Integer; const S: string): Boolean;
    procedure Insert(Index: Integer; const S: string); override;
    property IndentLevel: Integer read FIndentLevel write SetIndentLevel;
  end;

implementation

{$R *.DFM}

uses
  OFUtils;

const
  SInitName = 'initialization';
  SRegFuncName = 'InstantRegisterClasses';
  
{ TClassRegWizardForm }

constructor TClassRegWizardForm.Create(AOwner: TComponent);
begin
  inherited;
  PopulateUnitList;
end;

function TClassRegWizardForm.GetCheckedUnitCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  with UnitListView do
    for I := 0 to Pred(Items.Count) do
      if Items[I].Checked then
        Inc(Result);
end;

procedure TClassRegWizardForm.GetCheckedUnits(List: TInterfaceList);
var
  I: Integer;
begin
  List.Clear;
  with UnitListView do
    for I := 0 to Pred(Items.Count) do
      if Items[I].Checked then
        List.Add(IMMUnit(Items[I].Data));
end;

function TClassRegWizardForm.GetRegistrationCode(AUnit: IMMUnit): string;
var
  I: Integer;
  AClass: IMMClassBase;
  ClassNames: TStringList;
  Code: TCodeStrings;
  S: string;
begin
  ClassNames := TStringList.Create;
  try
    for I := 0 to Pred(AUnit.ClassCount) do
    begin
      AClass := AUnit.Classes[I];
      if IsPersistentClass(AClass) then
        ClassNames.Add(AClass.Name);
    end;
    if ClassNames.Count > 0 then
    begin
      ClassNames.Sort;
      Code := TCodeStrings.Create;
      try
        Code.BeginIndent;
        Code.Add(SRegFuncName + '([');
        Code.BeginIndent;
        for I := 0 to Pred(ClassNames.Count) do
        begin
          if I < Pred(ClassNames.Count) then
            S := ','
          else
            S := '';
          Code.Add(ClassNames[I] + S);
        end;
        Code.EndIndent;
        Code.Add(']);');
        Result := TrimRight(Code.Text);
      finally
        Code.Free;
      end;
    end else
      Result := '';
  finally
    ClassNames.Free;
  end;
end;

function TClassRegWizardForm.GetUnitCount: Integer;
begin
  Result := UnitList.Count;
end;

function TClassRegWizardForm.GetUnitList: TInterfaceList;
begin
  if not Assigned(FUnitList) then
  begin
    FUnitList := TInterfaceList.Create;
    LoadUnitList;
  end;
  Result := FUnitList;
end;

function TClassRegWizardForm.GetUnits(Index: Integer): IMMUnit;
begin
  Result := UnitList[Index] as IMMUnit;
end;

procedure TClassRegWizardForm.LoadUnitList;

  function UnitHasPersistentClass(AUnit: IMMUnit): Boolean;
  var
    I: Integer;
    ARoot, AClass: IMMClassBase;
  begin
    ARoot := GetRootClass;
    if Assigned(ARoot) then
      for I := 0 to Pred(AUnit.ClassCount) do
      begin
        AClass := AUnit.Classes[I];
        if AClass.IsClass(ARoot) then
        begin
          Result := True;
          Exit;
        end;
      end;
    Result := False;
  end;

var
  UnitManager: IMMUnitManager;
  AUnit: IMMUnit;
  I: Integer;
begin
  UnitList.Clear;
  UnitManager := MMToolServices.UnitManager;
  if Assigned(UnitManager) then
  begin
    for I := 0 to Pred(UnitManager.UnitCount) do
    begin
      AUnit := UnitManager.Units[I];
      if UnitHasPersistentClass(AUnit) then
        UnitList.Add(AUnit);
    end;
  end;
end;

procedure TClassRegWizardForm.OkButtonClick(Sender: TObject);
begin
  Run;
end;

procedure TClassRegWizardForm.PopulateUnitList;
var
  I: Integer;
  AUnit: IMMUnit;
begin
  with UnitListView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to Pred(UnitCount) do
      begin
        AUnit := Units[I];
        with Items.Add do
        begin
          Caption := ExtractFileName(AUnit.RelUnitName);
          Data := Pointer(AUnit);
          Checked := True;
        end;
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TClassRegWizardForm.ProcessUnit(AUnit: IMMUnit);

  procedure RemoveRegFunc(Code: TCodeStrings; var Index: Integer);
  var
    Start: Integer;
  begin
    if Code.FindFirst(Index, SRegFuncName) then
    begin
      Start := Index;
      if not Code.FindFirst(Index, ']);') then
        Exit;
      while Index >= Start do
      begin
        Code.Delete(Index);
        Dec(Index);
      end;
      Index := Start;
    end;
  end;

var
  Code: TCodeStrings;
  Index: Integer;
begin
  Code := TCodeStrings.Create;
  try
    Code.Text := AUnit.Code;
    Index := Pred(Code.Count);
    if not Code.FindLast(Index, SInitName) then
    begin
      if not Code.FindLast(Index, 'end.') then
        Exit;
      Code.Insert(Index, '');
      Code.Insert(Index, SInitName);
    end;
    Inc(Index);
    RemoveRegFunc(Code, Index);
    Code.Insert(Index, GetRegistrationCode(AUnit));
    AUnit.Code := Code.Text;
  finally
    Code.Free;
  end;
end;

procedure TClassRegWizardForm.Run;
var
  I: Integer;
  CheckedUnits: TInterfaceList;
  AUnit: IMMUnit;
begin
  CheckedUnits := TInterfaceList.Create;
  try
    GetCheckedUnits(CheckedUnits);
    for I := 0 to Pred(CheckedUnits.Count) do
    begin
      AUnit := CheckedUnits[I] as IMMUnit;
      ProcessUnit(AUnit);
    end;
  finally
    CheckedUnits.Free;
  end;
end;

procedure TClassRegWizardForm.UpdateActions;
begin
  inherited;
  OkButton.Enabled := CheckedUnitCount > 0;
end;

{ TCodeStrings }

function TCodeStrings.Add(const S: string): Integer;
begin
  Result := inherited Add(IndentStr + S);
end;

procedure TCodeStrings.BeginIndent;
begin
  Inc(FIndentLevel);
end;

procedure TCodeStrings.EndIndent;
begin
  if IndentLevel > 0 then
    Dec(FIndentLevel);
end;

function TCodeStrings.FindFirst(var Index: Integer; const S: string): Boolean;
var
  I: Integer;
  Str: string;
begin
  for I := Index to Pred(Count) do
  begin
    Str := Trim(Strings[I]);
    if SameText(Copy(Str, 1, Length(S)), S) then
    begin
      Index := I;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TCodeStrings.FindLast(var Index: Integer; const S: string): Boolean;
var
  I: Integer;
  Str: string;
begin
  for I := Index downto 0 do
  begin
    Str := Trim(Strings[I]);
    if SameText(Copy(Str, 1, Length(S)), S) then
    begin
      Index := I;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TCodeStrings.IndentStr: string;
begin
  Result := StringOfChar(' ', IndentLevel * 2);
end;

procedure TCodeStrings.Insert(Index: Integer; const S: string);
begin
  inherited Insert(Index, IndentStr + S);
end;

procedure TCodeStrings.SetIndentLevel(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  FIndentLevel := Value;
end;

end.
