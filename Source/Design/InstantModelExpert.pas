(*
 *   InstantObjects
 *   IDE Model Expert for D7+
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
 * Nando Dessena, Steven Mitchell, Brian Andersen, David Taylor
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantModelExpert;

{$I '..\InstantDefines.inc'}

interface

uses
  Classes, ToolsAPI, InstantOTA, Menus, ImgList, ExtCtrls, Forms,
  InstantDesignResources, InstantModelExplorer, InstantCode, 
  InstantConsts, ActnList, System.Actions;

type
  TIOMetaDataCheckState = (mcNeverChecked, mcCheckError, mcCheckCorrect);

  { When the IDE is being shut down, items in the Database-menu are
    destroyed even if they don't belong to the menu. Since we want to
    detach our items in the Database-menu when the expert is removed,
    we need to know if they have already been destroyed by the IDE
    before doing so. TReferencedMenuItem knows about our reference to
    the item and will clear this reference when it is destroyed.
    This ensures that we do not try to free items that are already
    destroyed by the IDE. }

  PReferencedMenuItem = ^TReferencedMenuItem;
  TReferencedMenuItem = class(TMenuItem)
  private
    FReferee: PReferencedMenuItem;
  public
    constructor Create(AOwner: TComponent;
      var AReferee: TReferencedMenuItem); reintroduce;
    destructor Destroy; override;
  end;

  TSourceEnumerator = procedure(const FileName, Source: string) of object;

  TInstantModelExpert = class(TNotifierObject, IOTAWizard)
  private
    FActiveProjectName: string;
    FBuilderItem: TReferencedMenuItem;
    FExplorerItem: TMenuItem;
    FIDEInterface: TInstantOTAIDEInterface;
    FIsChanged: Boolean;
    FMustUpdateAfterCompile: Boolean;
    FResourceModule: TInstantDesignResourceModule;
    FSaveApplicationIdle: TIdleEvent;
    FUpdateDisableCount: Integer;
    FUpdateTimer: TTimer;
    MetaDataCheckState : TIOMetaDataCheckState;
    MetaDataCheckUnits : string;
    FTimerDisabled: Boolean;
    FTimerInterval: Integer;
    procedure ExplorerApplyClass(Sender: TObject; AClass: TInstantCodeClass;
      ChangeInfo: TInstantCodeClassChangeInfo);
    procedure ExplorerGotoSource(Sender: TObject; const FileName: string;
      Pos: TInstantCodePos);
    procedure ExplorerLoadModel(Sender: TObject; Model: TInstantCodeModel);
    function GetActiveProject: IOTAProject;
    function GetAllowContinue: Boolean;
    function GetCurrentSource: string;
    function GetExplorer: TInstantModelExplorerForm;
    function GetIDString: string;
    function GetIsDirty: Boolean;
    function GetName: string;
    function GetState: TWizardState;
    procedure SetIsDirty(const Value: Boolean);
  protected
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure AccessModelUnits(Project: IOTAProject; Units: TStrings;
      Write: Boolean);
    procedure AddToolError(const FileName, Msg: string; Line, Column: Integer);
    procedure AddToolMessage(const FileName, Msg, Prefix: string; Line, Column: Integer);
    procedure AddToolText(const Text: string);
    procedure AttachMenus;
    procedure BuilderItemClick(Sender: TObject);
    procedure CheckProjectChanged;
    procedure CompileProject(Project: IOTAProject);
    procedure CollectModules(Project: IOTAProject; Modules: TInterfaceList;
      Names: TStrings);
    function CreateIDEInterface: TInstantOTAIDEInterface;
    function CreateUpdateTimer: TTimer;
    procedure DetachMenus;
    procedure DetachMenuActionEvent(AName: string);
    procedure EnumSources(Modules: TInterfaceList;
      Enumerator: TSourceEnumerator);
    function FindMenuAction(AName: string; out AAction: TContainedAction): boolean;
    function FindOrCreateMenuAction(AName, ACaption: string;
      AEventHandler: TNotifyEvent; AImageIndex : integer = -1;
      AShortCut: TShortCut = 0): TContainedAction;
    procedure CheckIOMetadataKeyword(const FileName, Source: string);
    procedure ExplorerItemClick(Sender: TObject);
    procedure GetModelModules(Modules: TInterfaceList);
    procedure IDEAfterCompilation(Sender: TObject; const Project: IOTAProject;
        Succeeded: Boolean; IsCodeInsight: Boolean);
    procedure IDEBeforeCompilation(Sender: TObject; Project: IOTAProject;
      IsCodeInsight: Boolean; var Cancel: Boolean);
    procedure IDEFileNotification(Sender: TObject;
      NotifyCode: TOTAFileNotification; const FileName: string;
      var Cancel: Boolean);
    procedure IDEModuleNotification(Sender: TObject; NotifyCode: TModuleNotifyCode;
      const FileName: string);
    function IsProjectUnit(FileName: string): Boolean;
    function IsModelUnit(FileName: string): Boolean;
    procedure ShowExplorer;
    procedure UpdateTimerTick(Sender: TObject);
    procedure UpdateMenuActions;
    property CurrentSource: string read GetCurrentSource;
    property Explorer: TInstantModelExplorerForm read GetExplorer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DisableUpdate;
    procedure EnableUpdate;
    procedure ApplyClass(AClass: TInstantCodeClass;
      ChangeInfo: TInstantCodeClassChangeInfo);
    procedure BuildDatabase(CodeModel: TInstantCodeModel);
    procedure Execute;
    function LoadModel(Model: TInstantCodeModel; Project: IOTAProject = nil;
      CheckTime: TDateTime = 0): Boolean;
    procedure SelectUnits;
    function UpdateEnabled: Boolean;
    procedure UpdateModel;
    procedure UpdateModelUnits;
    property ActiveProject: IOTAProject read GetActiveProject;
    property AllowContinue: Boolean read GetAllowContinue;
    property IsDirty: Boolean read GetIsDirty write SetIsDirty;
    property TimerDisabled: Boolean read FTimerDisabled write FTimerDisabled default True;
    property TimerInterval: Integer read FTimerInterval write FTimerInterval default 10000;
  end;

var
  ModelExpert: TInstantModelExpert;

procedure Register;

implementation

uses
  SysUtils, TypInfo, InstantDesignUtils, InstantUtils, InstantUnitSelect,
  InstantConnectionManager, Dialogs, Graphics;

const
  SIOIdeMenuCategory = 'InstantObjects';
  SBuilderItemCaption = 'InstantObjects Database &Builder...';
  SExplorerItemCaption = 'InstantObjects &Model Explorer';
  SBuilderItemName = 'InstantBuilderItem'; // Do not localize
  SExplorerItemName = 'InstantExplorerItem'; // Do not localize
  SExplorerItemActionName = 'InstantExplorerItemAction'; // Do not localize
  SBuilderItemActionName = 'InstantBuilderItemAction'; // Do not localize
  SResFileExt = '.mdr';

procedure ReaderIdle(Reader: TInstantCodeReader; var Continue: Boolean);
begin
  Application.ProcessMessages;
  Continue := ModelExpert.AllowContinue;
end;

procedure Register;
begin
  ModelExpert := TInstantModelExpert.Create;
  RegisterPackageWizard(ModelExpert);
  InstantCodeReaderIdle := ReaderIdle;
end;

function FindText(const SubStr, Str: string;
  var Pos, Line, Column: Integer): Boolean;
var
  I, J: Integer;
begin
  J := 1;
  if Pos = 0 then
    Inc(Pos);
  if Pos = 1 then
  begin
    Line := 1;
    Column := 1;
  end;
  I := Pos;
  while I <= Length(Str) do
  begin
    case Str[I] of
      #10:begin
            Inc(Line);
            Column := 1;
          end;
    else
      Inc(Column);
    end;
    if UpperCase(Str[I]) = UpperCase(SubStr[J]) then
    begin
      if J = Length(SubStr) then
      begin
        Pos := I - J + 1;
        Result := True;
        Exit;
      end;
      Inc(J);
      Inc(I);
    end else if J = 1 then
      Inc(I)
    else
      J := 1;
  end;
  Result := False;
end;

{ TReferencedMenuItem }

constructor TReferencedMenuItem.Create(AOwner: TComponent;
  var AReferee: TReferencedMenuItem);
begin
  inherited Create(AOwner);
  FReferee := @AReferee;
end;

destructor TReferencedMenuItem.Destroy;
begin
  inherited;
  FReferee^ := nil;
end;

{ TInstantModelExpert}

procedure TInstantModelExpert.AccessModelUnits(Project: IOTAProject;
  Units: TStrings; Write: Boolean);
const

  ModelTag = #10'{$R *' + SResFileExt + '}';
  ResourceTag = #10'{$R *.res}';

  function ListToStr(List: TStrings): string;
  var
    I: Integer;
    S: string;
  begin
    S := '';
    for I := 0 to Pred(List.Count) do
    begin
      Result := Result + S + List[I];
      S := ', ' + sLineBreak + '            ';
    end;
  end;

  function FindModelDef(const Source: string; out ModelDef: string;
    var Line, Column: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 1;
    if FindText(ModelTag, Source, Result, Line, Column) then
    begin
      I := Result + Length(ModelTag);
      while I <= Length(Source) do
      begin
        case Source[I] of
          ' ': Inc(I);
          '{':
            while I < Length(Source) do
            begin
              Inc(I);
              if Source[I] = '}' then
              begin
                ModelDef := Copy(Source, Result, I - Result + 1);
                Exit;
              end;
            end;
        else
          Break;
        end;
      end;
      ModelDef := Copy(Source, Result, Length(ModelTag));
    end else
      Result := 0;
  end;

  function RemoveBrackets(const Str: string): string;
  begin
    Result := Trim(Str);
    if (Length(Result) > 0) and (Result[1] = '{') then
      Delete(Result, 1, 1);
    if (Length(Result) > 0) and (Result[Length(Result)] = '}') then
      Delete(Result, Length(Result), 1);
    Result := Trim(Result);
  end;

  procedure WriteUses(var Source: string; UnitNames: array of string;
    Include: Boolean);
  var
    UsesClause: TInstantCodeUsesClause;
    UsesItem: TInstantCodeUses;
    Found: Boolean;
    I: Integer;
    S: string;
  begin
    with TInstantCodeModifier.Create(Source, nil) do
    try
      if Module.ModuleType = mtProgram then
      begin
        UsesClause := Module.ProgramSection.FindUsesClause;
        if Assigned(UsesClause) and (UsesClause.Count > 0) then
        begin
          Found := False;
          for I := Low(UnitNames) to High(UnitNames) do
          begin
            UsesItem := UsesClause.Find(UnitNames[I]);
            Found := Assigned(UsesItem);
            if Found then
            begin
              if not Include then
              begin
                EraseObject(UsesItem);
                if NextChar = ',' then
                  DeleteText(1);
                CloseGap;
              end else
                Break;
            end;
          end;
          if Include and not Found then
          begin
            CursorPos := UsesClause[0].StartPos;
            InsertMode := imBefore;
            S := '';
            for I := Low(UnitNames) to High(UnitNames) do
              S := S + UnitNames[I] + ','#10'  ';
            InsertText(S);
          end;
        end;
      end;
    finally
      Free;
    end;
  end;

var
  Editor: IOTASourceEditor;
  Source: string;
  Pos, Line, Column, SourceLen: Integer;
  CurModelDef, NewModelDef: string;
begin
  Editor := FIDEInterface.SourceEditor(Project);
  Source := FIDEInterface.ReadEditorSource(Editor);
  Pos := FindModelDef(Source, CurModelDef, Line, Column);
  if Write then
  begin
    SourceLen := Length(Source);
    if Units.Count > 0 then
      NewModelDef := Format('%s {%s}', [ModelTag, ListToStr(Units)])
    else
      NewModelDef := '';
    if CurModelDef = NewModelDef then
      Exit
    else if Pos > 0 then
      Delete(Source, Pos, Length(CurModelDef))
    else if not FindText(ResourceTag, Source, Pos, Line, Column) then
      Exit
    else
      Inc(Pos, Length(ResourceTag));
    Insert(NewModelDef, Source, Pos);
    FIDEInterface.WriteEditorSource(Editor, Source, SourceLen);
  end else if Pos > 0 then
  begin
    Delete(CurModelDef, 1, Length(ModelTag));
    CurModelDef := RemoveBrackets(CurModelDef);
    if CurModelDef = '' then
      AddToolError(Editor.FileName, 'No model units specified', Line, Column);
    InstantStrToList(CurModelDef, Units, [',']);
  end;
end;

procedure TInstantModelExpert.AddToolError(const FileName, Msg: string; Line,
  Column: Integer);
begin
  AddToolMessage(FileName, Msg, 'Error', Line, Column);
end;

procedure TInstantModelExpert.AddToolMessage(const FileName, Msg, Prefix: string;
  Line, Column: Integer);
begin
  FIDEInterface.MessageServices.AddToolMessage(FileName, Msg, Prefix,
    Line, Column);
end;

procedure TInstantModelExpert.AddToolText(const Text: string);
begin
  AddToolMessage('', Text, '', 0, 0);
end;

procedure TInstantModelExpert.ApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
  CheckProjectChanged;
  if Assigned(FSaveApplicationIdle) then
    FSaveApplicationIdle(Sender, Done);
end;

procedure TInstantModelExpert.ApplyClass(AClass: TInstantCodeClass;
  ChangeInfo: TInstantCodeClassChangeInfo);
var
  Source: string;
  Module: IOTAModule;
  Editor: IOTASourceEditor;
  OldLen: Integer;
begin
  Module := FIDEInterface.FindModule(AClass.Module.PascalUnitName);
  if not Assigned(Module) then
    Exit;
  Editor := FIDEInterface.SourceEditor(Module);
  if not Assigned(Editor) then
    Exit;
  Source := FIDEInterface.ReadEditorSource(Editor);
  OldLen := Length(Source);
  AClass.ApplyToSource(Source, ChangeInfo);
  DisableUpdate;
  try
    FIDEInterface.WriteEditorSource(Editor, Source, OldLen);
  finally
    EnableUpdate;
  end;
end;

procedure TInstantModelExpert.AttachMenus;

  function ItemByName(Items: TMenuItem; Name: string): TMenuItem;
  var
    I: Integer;
  begin
    Result := nil;
    if Assigned(Items) then
      for I := 0 to Pred(Items.Count) do
      begin
        if Items[I].Name = Name then
        begin
          Result := Items[I];
          Break;
        end;
      end;
  end;

  procedure CreateBuilderMenuItem;
  begin
    FBuilderItem := TReferencedMenuItem.Create(nil, FBuilderItem);
    FBuilderItem.Name := SBuilderItemName;
    FBuilderItem.Action := FindOrCreateMenuAction(
      SBuilderItemActionName,
      SBuilderItemCaption,
      BuilderItemClick, 1,
      Menus.ShortCut(Word('B'), [ssCtrl, ssShift]));
  end;

var
  MainMenu: TMainMenu;
  Menu, Item: TMenuItem;
begin
  if not Assigned(BorlandIDEServices) then
    Exit;

  MainMenu := (BorlandIDEServices as INTAServices40).MainMenu;

  if not Assigned(MainMenu) then
    Exit;

  { Add 'Model Explorer' to View-menu }
  Menu := ItemByName(MainMenu.Items, 'ViewsMenu');
  if Assigned(Menu) then
  begin
    FExplorerItem := TMenuItem.Create(nil);
    FExplorerItem.Name := SExplorerItemName;
    FExplorerItem.Action := FindOrCreateMenuAction(
      SExplorerItemActionName,
      SExplorerItemCaption,
      ExplorerItemClick, 0,
      Menus.ShortCut(Word('M'), [ssCtrl, ssShift]));

    Item := ItemByName(Menu, 'ViewStructureItem');
    if Assigned(Item) then
      Menu.Insert(Item.MenuIndex + 1, FExplorerItem)
    else
      Menu.Add(FExplorerItem);

  { Add Database InstantObjects Builder to View-menu }
    CreateBuilderMenuItem;
    Item := ItemByName(Menu, 'mnuViewDataExplorer');
    if Assigned(Item) then
      Menu.Insert(Item.MenuIndex + 1, FBuilderItem)
    else
      Menu.Add(FBuilderItem);
  end;

end;

procedure TInstantModelExpert.BuildDatabase(CodeModel: TInstantCodeModel);
var
  Project: IOTAProject;
begin
  Project := ActiveProject;
  if not Assigned(Project) then
    Exit;
  with TInstantConnectionManager.Create(nil) do
  try
    Caption := 'Database Builder';
    Model := CodeModel.Model;
    FileName := ChangeFileExt(Project.FileName, '.xml');
    VisibleActions := [atNew, atEdit, atDelete, atRename, atBuild, atEvolve, atOpen];
    Execute;
  finally
    Free;
  end;
end;

procedure TInstantModelExpert.BuilderItemClick(Sender: TObject);
begin
  BuildDatabase(Explorer.Model);
end;

procedure TInstantModelExpert.CheckProjectChanged;
var
  Project: IOTAProject;
begin
  with FIDEInterface do
    if Assigned(ProjectGroup) then
    begin
      Project := ProjectGroup.ActiveProject;
      if Assigned(Project) and not SameText(Project.FileName,
        FActiveProjectName) then
      begin
        FActiveProjectName := Project.FileName;
        UpdateModel;
      end;
    end else if FActiveProjectName <> '' then
    begin
      UpdateModel;
      FActiveProjectName := '';
    end;

  UpdateMenuActions;
end;

procedure TInstantModelExpert.CollectModules(Project: IOTAProject;
  Modules: TInterfaceList; Names: TStrings);

  function NameInNames(Name: string): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Pred(Names.Count) do
      if SameText(Name, Names[I]) then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

var
  I: Integer;
  ModuleInfo: IOTAModuleInfo;
begin
  for I := 0 to Pred(Project.GetModuleCount) do
  begin
    ModuleInfo := Project.GetModule(I);
    if NameInNames(ModuleInfo.Name) then
      Modules.Add(ModuleInfo.OpenModule);
  end;
end;

procedure TInstantModelExpert.CompileProject(Project: IOTAProject);
var
  Model: TInstantCodeModel;
  ResFileName: string;
  ResFileTime: TDateTime;
begin
  DisableUpdate;
  Model := TInstantCodeModel.Create;
  try
    ResFileName := ChangeFileExt(Project.FileName, SResFileExt);

    if (not InstantFileAge(ResFileName, ResFileTime)) then
      ResFileTime := 0;

    try
      if LoadModel(Model, Project, ResFileTime) then
        Model.SaveToResFile(ResFileName);
    except
      on E: EInstantCodeError do
        begin
          AddToolError(E.FileName, E.Message, E.Position.Line,
            E.Position.Column);
          Abort;
        end
      else
        raise;
    end;
  finally
    Model.Free;
    EnableUpdate;
  end;
end;

constructor TInstantModelExpert.Create;
begin
  //CheckExpiration;
  FTimerInterval := 10000;
  FTimerDisabled := True;
  FResourceModule := TInstantDesignResourceModule.Create(nil);
  FIDEInterface := CreateIDEInterface;
  FUpdateTimer := CreateUpdateTimer;
  AttachMenus;
  FSaveApplicationIdle := Application.OnIdle;
  Application.OnIdle := ApplicationIdle;
  ModelExplorer := Explorer;
end;

function TInstantModelExpert.CreateIDEInterface: TInstantOTAIDEInterface;
begin
  Result := TInstantOTAIDEInterface.Create;
  with Result do
  begin
    OnAfterCompilation := IDEAfterCompilation;
    OnBeforeCompilation := IDEBeforeCompilation;
    OnFileNotification := IDEFileNotification;
    OnModuleNotification := IDEModuleNotification;
  end;
end;

function TInstantModelExpert.CreateUpdateTimer: TTimer;
begin
  Result := TTimer.Create(nil);
  with Result do
  begin
    Enabled := False;
    Interval := FTimerInterval;
    OnTimer := UpdateTimerTick;
  end;
end;

destructor TInstantModelExpert.Destroy;
begin
  Application.OnIdle := FSaveApplicationIdle;
  DetachMenus;
  FUpdateTimer.Free;
  ModelExplorer.Free;
  FIDEInterface.Free;
  FResourceModule.Free;
  inherited;
end;

procedure TInstantModelExpert.DetachMenus;
begin
  { Unhook action event handlers }
  DetachMenuActionEvent(SBuilderItemActionName);
  DetachMenuActionEvent(SExplorerItemActionName);

  { Remove items }
  FBuilderItem.Free;
  FExplorerItem.Free;
end;

// Searches for an IDE action matching the given name and disables its
// OnExecute event handler and sets Visible to False. This disables
// the action and avoids an AV if the expert is unloaded and the
// event handler becomes invalid (e.g. during a package rebuild)
procedure TInstantModelExpert.DetachMenuActionEvent(AName: string);
var
  LAction : TContainedAction;
begin
  if (FindMenuAction(AName, LAction)) then
  begin
    LAction.OnExecute := nil;
    if (LAction is TCustomAction) then
      TCustomAction(LAction).Visible := False;
  end;
end;

procedure TInstantModelExpert.DisableUpdate;
begin
  Inc(FUpdateDisableCount);
end;

procedure TInstantModelExpert.EnableUpdate;
begin
  if FUpdateDisableCount > 0 then
    Dec(FUpdateDisableCount);
end;

procedure TInstantModelExpert.EnumSources(Modules: TInterfaceList;
  Enumerator: TSourceEnumerator);
var
  I: Integer;
  Module: IOTAModule;
  Editor: IOTASourceEditor;
  Source: string;
begin
  if not Assigned(Enumerator) then
    Exit;
  Busy(True);
  try
    for I := 0 to Pred(Modules.Count) do
    begin
      Module := Modules[I] as IOTAModule;
      if Module.GetModuleFileCount = 1 then
      begin
        Editor := FIDEInterface.SourceEditor(Module);
        Source := FIDEInterface.ReadEditorSource(Editor);
        Enumerator(Editor.FileName, Source);
      end;
    end;
  finally
    Busy(False);
  end;
end;

procedure TInstantModelExpert.Execute;
begin
  ShowExplorer;
end;

procedure TInstantModelExpert.ExplorerApplyClass(Sender: TObject;
  AClass: TInstantCodeClass; ChangeInfo: TInstantCodeClassChangeInfo);
begin
  ApplyClass(AClass, ChangeInfo);
end;

procedure TInstantModelExpert.ExplorerGotoSource(Sender: TObject;
  const FileName: string; Pos: TInstantCodePos);
begin
  FIDEInterface.GotoFilePos(FileName, Pos.Line, Pos.Column);
end;

procedure TInstantModelExpert.ExplorerItemClick(Sender: TObject);
begin
  ShowExplorer;
end;

procedure TInstantModelExpert.ExplorerLoadModel(Sender: TObject;
  Model: TInstantCodeModel);
begin
  LoadModel(Model);
end;

// Searches for an IDE action matching the given name
function TInstantModelExpert.FindMenuAction(AName: string;
  out AAction: TContainedAction): boolean;
var
  IdeActionList: TCustomActionList;
  NTAServices: INTAServices;
  I: integer;
begin
  // Get the IDE's action list
  NTAServices := BorlandIDEServices as INTAServices;
  Assert(Assigned(NTAServices));
  IdeActionList := NTAServices.ActionList;
  Assert(Assigned(IdeActionList));

  // Search for and diable IDE action
  AAction := nil;

  for I := 0 to IdeActionList.ActionCount-1 do
  begin
    if (not SameText(IdeActionList.Actions[I].Name, AName)) then
      continue;
    AAction := IdeActionList.Actions[I];
    break;
  end;

  Result := assigned(AAction);
end;

function TInstantModelExpert.FindOrCreateMenuAction(AName, ACaption: string;
  AEventHandler: TNotifyEvent; AImageIndex : integer = -1;
  AShortCut: TShortCut = 0): TContainedAction;
var
  NTAServices: INTAServices;
  IdeMainForm: TCustomForm;
  IdeActionList: TCustomActionList;
  NewAction: TAction;
  ActionImage: TBitmap;
begin
  if (FindMenuAction(AName, Result)) then
  begin
    // Enable action and connect the event handler
    Result.OnExecute := AEventHandler;
    if (Result is TCustomAction) then
    begin
      TCustomAction(Result).Enabled := true;
      TCustomAction(Result).Visible := true;
    end;
  end else
  begin
    // Get the IDE's main form
    Assert(Assigned(Application));
    IdeMainForm := Application.FindComponent('AppBuilder') as TCustomForm;

    // Get the IDE's action list
    NTAServices := (BorlandIDEServices as INTAServices);
    Assert(Assigned(NTAServices));
    IdeActionList := NTAServices.ActionList;
    Assert(Assigned(IdeActionList));

    // Create and initialize the action
    NewAction := TAction.Create(IdeMainForm);
    NewAction.ActionList := IdeActionList;
    NewAction.Name := AName;
    NewAction.Caption := ACaption;
    NewAction.Category := SIOIdeMenuCategory;
    NewAction.ShortCut := AShortCut;
    NewAction.OnExecute := AEventHandler;
    Result := NewAction;

    if (AImageIndex >= 0) and (AImageIndex < FResourceModule.ToolImages.Count) then
    begin
      ActionImage := TBitmap.Create;

      try
        ActionImage.PixelFormat := pf24bit;
        FResourceModule.ToolImages.GetBitmap(AImageIndex,ActionImage);
        Assert(Assigned(ActionImage));
        NewAction.ImageIndex := NTAServices.AddMasked(ActionImage,
          ActionImage.TransparentColor,AName);
      finally
        FreeAndNil(ActionImage);
      end;
    end;
  end;
end;

function TInstantModelExpert.GetActiveProject: IOTAProject;
begin
  with FIDEInterface do
    if Assigned(ProjectGroup) then
      Result := ProjectGroup.ActiveProject
    else
      Result := nil;
end;

function TInstantModelExpert.GetAllowContinue: Boolean;
begin
  Result := not FIsChanged;
end;

function TInstantModelExpert.GetCurrentSource: string;
var
  Editor: IOTASourceEditor;
begin
  with FIDEInterface do
  begin
    Editor := SourceEditor(CurrentModule);
    Result := ReadEditorSource(Editor);
  end;
end;

function TInstantModelExpert.GetExplorer: TInstantModelExplorerForm;
begin
  if not Assigned(ModelExplorer) then
  begin
    ModelExplorer := TInstantModelExplorerForm.Create(nil);
    with ModelExplorer do
    begin
      OnApplyClass := ExplorerApplyClass;
      OnGotoSource := ExplorerGotoSource;
      OnLoadModel := ExplorerLoadModel;
    end;
  end;
  Result := ModelExplorer;
end;

function TInstantModelExpert.GetIDString: string;
begin
  Result := 'Instant.Model.Expert';
end;

function TInstantModelExpert.GetIsDirty: Boolean;
begin
  Result := FUpdateTimer.Enabled;
end;

procedure TInstantModelExpert.GetModelModules(Modules: TInterfaceList);
var
  Project: IOTAProject;
  UnitNames: TStringList;
begin
  Project := ActiveProject;
  UnitNames := TStringList.Create;
  try
    AccessModelUnits(Project, UnitNames, False);
    CollectModules(Project, Modules, UnitNames);
  finally
    UnitNames.Free;
  end;
end;

function TInstantModelExpert.GetName: string;
begin
  Result := 'Instant Model Expert';
end;

function TInstantModelExpert.GetState: TWizardState;
begin
  Result := [];
end;

procedure TInstantModelExpert.IDEAfterCompilation(Sender: TObject; const
    Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean);
begin
  if IsCodeInsight then
    Exit;
  if FMustUpdateAfterCompile then
  begin
    FMustUpdateAfterCompile := False;
    UpdateModel;
  end;
end;

procedure TInstantModelExpert.IDEBeforeCompilation(Sender: TObject;
  Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  if IsCodeInsight then
    Exit;
  FMustUpdateAfterCompile := IsDirty;
  IsDirty := False;
  FIDEInterface.MessageServices.ClearAllMessages;
  try
    CompileProject(Project);
  except
    on E: EAbort do
      begin
        Cancel := True;
        FIDEInterface.ShowMessages;
        FMustUpdateAfterCompile := False;
      end;
    else
      raise;
  end;
end;

procedure TInstantModelExpert.IDEFileNotification(Sender: TObject;
  NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin
  case NotifyCode of
    ofnFileOpened:
      if IsProjectUnit(FileName) then
        MetaDataCheckState := mcNeverChecked;
    ofnFileClosing:
      if IsModelUnit(FileName) then
        IsDirty := True;
  end;
end;

procedure TInstantModelExpert.IDEModuleNotification(Sender: TObject;
  NotifyCode: TModuleNotifyCode; const FileName: string);
begin
  case NotifyCode of
    mncAfterSave,
    mncEditorModified:
      if IsModelUnit(FileName) then
        IsDirty := True;
    mncEditorSelected: ;
  end;
end;

function TInstantModelExpert.IsProjectUnit(FileName: string): Boolean;
begin
  Result := Assigned(ActiveProject) and SameText(ActiveProject.FileName, FileName);
end;

function TInstantModelExpert.IsModelUnit(FileName: string): Boolean;
var
  Project: IOTAProject;
  Units: TStringList;
  I: Integer;
begin
  Result := False;
  Project := ActiveProject;
  if not Assigned(Project) then
    Exit;
  if SameText(Project.FileName, FileName) then
  begin
    Result := True;
    Exit;
  end;
  Units := TStringList.Create;
  try
    AccessModelUnits(Project, Units, False);
    for I := 0 to Pred(Units.Count) do
      if SameText(Units[I], ChangeFileExt(ExtractFileName(FileName), '')) then
      begin
        Result := True;
        Break;
      end;
  finally
    Units.Free;
  end;
end;

function TInstantModelExpert.LoadModel(Model: TInstantCodeModel;
  Project: IOTAProject; CheckTime: TDateTime): Boolean;

  function EditorModified(Module: IOTAModule): Boolean;
  var
    Editor: IOTASourceEditor;
  begin
    Editor := FIDEInterface.SourceEditor(Module);
    Result := Editor.Modified;
  end;

  function FileModified(const FileName: string; Since: TDateTime): Boolean;
  var
    FileTime: TDateTime;
  begin
    if (not InstantFileAge(FileName, FileTime)) then
      Result := False
    else
      Result := (FileTime > Since);
  end;

  function ModuleModified(Module: IOTAModule; Since: TDateTime): Boolean;
  begin
    Result := EditorModified(Module) or FileModified(Module.FileName, Since);
  end;

  function ModulesModified(Modules: TInterfaceList; Since: TDateTime): Boolean;
  var
    I: Integer;
    Module: IOTAModule;
  begin
    for I := 0 to Pred(Modules.Count) do
    begin
      Module := Modules[I] as IOTAModule;
      Result := ModuleModified(Module, Since);
      if Result then
        Exit;
    end;
    Result := False;
  end;

  procedure ReadModel(Modules: TInterfaceList);
  var
    I: Integer;
    Module: IOTAModule;
    Editor: IOTASourceEditor;
    Source: string;
    Stream: TStringStream;
  begin
    for I := 0 to Pred(Modules.Count) do
    begin
      Module := Modules[I] as IOTAModule;
      Editor := FIDEInterface.SourceEditor(Module);

      Source := FIDEInterface.ReadEditorSource(Editor);
      Stream := TStringStream.Create(Source, TEncoding.Unicode);

      try                                    
        Model.LoadModule(Stream, Editor.FileName);
      finally
        Stream.Free;
      end;
    end;
  end;

var
  Units: TStringList;
  Modules: TInterfaceList;
begin
  if not Assigned(Project) then
    Project := ActiveProject;
  Units := TStringList.Create;
  try
    AccessModelUnits(Project, Units, False);
    if Units.Count > 0 then
    begin
      Modules := TInterfaceList.Create;
      try
        CollectModules(Project, Modules, Units);
        if MetaDataCheckState = mcNeverChecked then
        begin
          MetadataCheckUnits := '';
          MetaDataCheckState := mcCheckCorrect;
          EnumSources(Modules, CheckIOMetadataKeyword);
          if MetaDataCheckState = mcCheckError then
            MessageDlg(Format('WARNING: Project %s contains some class metadata without IOMETADATA keyword:'+
              '%s'+sLineBreak+'Please refer to IOMETADATA_keyword.txt in instantobjects\doc folder.',
              [FActiveProjectName, MetadataCheckUnits]), mtWarning, [mbOK], 0);
        end;
        Result := (CheckTime = 0) or
          ModuleModified(Project, CheckTime) or
          ModulesModified(Modules, CheckTime);
        if Result then
          ReadModel(Modules);
      finally
        Modules.Free;
      end;
    end else
      Result := False;
  finally
    Units.Free;
  end;
end;

procedure TInstantModelExpert.SelectUnits;

  procedure GetUnitNames(Project: IOTAProject; Names: TStrings);
  var
    I: Integer;
    Module: IOTAModuleInfo;
  begin
    for I := 0 to Pred(Project.GetModuleCount) do
    begin
      Module := Project.GetModule(I);
      if (Module.FileName <> '') and (Module.ModuleType = 0) then
        Names.Add(Module.Name);
    end;
  end;

  procedure SubtractList(List, Subtract: TStrings);
  var
    I, Index: Integer;
  begin
    for I := 0 to Pred(Subtract.Count) do
    begin
      Index := List.IndexOf(Subtract[I]);
      if Index <> -1 then
        List.Delete(Index);
    end;
  end;

var
  Project: IOTAProject;
  ModelUnits, OtherUnits: TStringList;
begin
  if not Assigned(FIDEInterface.ProjectGroup) then
    Exit;
  ModelUnits := TStringList.Create;
  OtherUnits := TStringList.Create;
  try
    Project := ActiveProject;
    AccessModelUnits(Project, ModelUnits, False);
    GetUnitNames(Project, OtherUnits);
    SubtractList(OtherUnits, ModelUnits);
    with TInstantUnitSelectForm.Create(nil) do
    try
      if Execute(ModelUnits, OtherUnits) then
      begin
        AccessModelUnits(Project, ModelUnits, True);
        UpdateModel;
      end;
    finally
      Free;
    end;
  finally
    ModelUnits.Free;
    OtherUnits.Free;
  end;
end;

procedure TInstantModelExpert.SetIsDirty(const Value: Boolean);
begin
  if not UpdateEnabled then
    Exit;
  FIsChanged := Value;
  with FUpdateTimer do
  begin
    Enabled := False;
    Enabled := Value;
  end;
end;

procedure TInstantModelExpert.ShowExplorer;
begin
  with Explorer do
  begin
    Refresh;
    Show;
    ModelView.SetFocus;
  end;
end;

function TInstantModelExpert.UpdateEnabled: Boolean;
begin
  Result := (FUpdateDisableCount = 0) and
    not FTimerDisabled;
end;

procedure TInstantModelExpert.UpdateModel;
begin
  IsDirty := False;
  DisableUpdate;
  try
    Explorer.Refresh;
  finally
    EnableUpdate;
  end;
end;

procedure TInstantModelExpert.UpdateModelUnits;

  procedure UpdateModelUnit(Module: IOTAModule);
  var
    Editor: IOTASourceEditor;
    Source, OldSource: string;
  begin
    Editor := FIDEInterface.SourceEditor(Module);
    Source := FIDEInterface.ReadEditorSource(Editor);
    OldSource := Source;
    with TInstantCodeModifier.Create(Source, nil) do
    try
      UpdateUnit;
    finally
      Free;
    end;
    if Source <> OldSource then
    begin
      DisableUpdate;
      try
        FIDEInterface.WriteEditorSource(Editor, Source, Length(Source));
      finally
        EnableUpdate;
      end;
    end;
  end;

var
  Modules: TInterfaceList;
  I: Integer;
begin
  Modules := TInterfaceList.Create;
  try
    GetModelModules(Modules);
    for I := 0 to Pred(Modules.Count) do
      UpdateModelUnit(Modules[I] as IOTAModule);
  finally
    Modules.Free;
  end;
end;

procedure TInstantModelExpert.UpdateTimerTick(Sender: TObject);
begin
  if UpdateEnabled then
    UpdateModel;
end;

procedure TInstantModelExpert.UpdateMenuActions;

  procedure EnableMenuItem(const AMenuItem: TMenuItem; AEnable: boolean);
  begin
    if (assigned(AMenuItem)) then
      if (AMenuItem.Action is TCustomAction) then
        TCustomAction(AMenuItem.Action).Enabled := AEnable;
  end;

var
  HaveProject: boolean;
  HaveModel: boolean;
begin
  HaveProject := Assigned(ActiveProject);
  HaveModel := Explorer.Model.ModuleCount > 0;
  EnableMenuItem(FExplorerItem, HaveProject);
  EnableMenuItem(FBuilderItem, HaveModel);
end;

procedure TInstantModelExpert.CheckIOMetadataKeyword(const FileName, Source: string);
begin
  if pos('{ stored', Source) > 0 then
  begin
    MetaDataCheckUnits := MetaDataCheckUnits + sLineBreak+FileName+';';
    MetaDataCheckState := mcCheckError;
  end;
end;

end.
