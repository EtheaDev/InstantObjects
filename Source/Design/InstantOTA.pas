(*
 *   InstantObjects
 *   Borland OTA Interface (Open Tools API) for D7+
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
 * Nando Dessena, Steven Mitchell, Brian Andersen
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantOTA;

interface

{$I '..\InstantDefines.inc'}

uses
  System.Classes
  , ToolsAPI
  , InstantTypes
  , Vcl.Forms
  ;

type
  InstantOTAString = UTF8String;
  PInstantOTAString = ^InstantOTAString;

  TInstantOTAIDEInterface = class;
  TInstantOTAIDENotifier8 = class;
  TInstantOTAIDENotifier5 = class;
  TInstantOTAModuleNotifier = class;
  TInstantOTAEditorNotifier = class;
  TInstantOTAFormNotifier = class;

  TModuleNotifyCode = (mncModuleDeleted, mncModuleRenamed, mncEditorModified,
    mncFormModified, mncEditorSelected, mncFormSelected, mncBeforeSave,
    mncAfterSave, mncFormSaving, mncProjResModified);

  TInstantOTAAfterCompilationEvent = procedure(Sender: TObject;
    const Project: IOTAProject; Succeeded: Boolean;
    IsCodeInsight: Boolean) of object;
  TInstantOTABeforeCompilationEvent = procedure(Sender: TObject;
    Project: IOTAProject; IsCodeInsight: Boolean;
    var Cancel: Boolean) of object;
  TInstantOTAModuleNotificationEvent = procedure(Sender: TObject;
    NotifyCode: TModuleNotifyCode; const FileName: string) of object;
  TInstantOTAFileNotificationEvent = procedure(Sender: TObject;
    NotifyCode: TOTAFileNotification; const FileName: string;
    var Cancel: Boolean) of object;
  TInstantOTAModuleRenamedNotificationEvent = procedure(Sender: TObject;
    const OldName, NewName: string) of object;

  IInstantOTANotifierUninstallation = interface(IInterface)
  ['{D5690321-5365-4BD1-B149-AE1B3A4AE371}']
    procedure UninstallNotifier;
  end;

  TInstantOTAIDEInterface = class(TObject)
  private
    FIDENotifier5: TInstantOTAIDENotifier5;
    function GetOnModuleRenamedNotification:
        TInstantOTAModuleRenamedNotificationEvent;
    procedure SetOnModuleRenamedNotification(const Value:
        TInstantOTAModuleRenamedNotificationEvent);
    function GetOnFileNotification: TInstantOTAFileNotificationEvent;
    function GetOnModuleNotification: TInstantOTAModuleNotificationEvent;
    procedure SetOnFileNotification(
      const Value: TInstantOTAFileNotificationEvent);
    procedure SetOnModuleNotification(
      const Value: TInstantOTAModuleNotificationEvent);
    function GetOnAfterCompilation: TInstantOTAAfterCompilationEvent;
    function GetOnBeforeCompilation: TInstantOTABeforeCompilationEvent;
    function GetEditActions: IOTAEditActions;
    function GetIDENotifier5: TInstantOTAIDENotifier5;
    function GetMessageServices: IOTAMessageServices;
    function GetModuleServices: IOTAModuleServices;
    function GetProjectGroup: IOTAProjectGroup;
    function GetServices: IOTAServices;
    procedure SetOnAfterCompilation(const Value: TInstantOTAAfterCompilationEvent);
    procedure SetOnBeforeCompilation(const Value:
        TInstantOTABeforeCompilationEvent);
  protected
    function FindForm(Name, ClassName: string): TForm;
    property IDENotifier5: TInstantOTAIDENotifier5 read GetIDENotifier5;
  public
    destructor Destroy; override;
    function CurrentModule: IOTAModule;
    function FindModule(const Name: string): IOTAModule;
    procedure GotoFilePos(const FileName: string; Line, Column: Integer);
    function ReadEditorSource(Editor: IOTASourceEditor): string;
    function ReadModuleSource(Module: IOTAModule): string;
    procedure ShowMessages;
    function SourceEditor(Module: IOTAModule): IOTASourceEditor;
    procedure WriteEditorSource(Editor: IOTASourceEditor; const Source: string;
      ReplaceLen: Integer; Undoable: Boolean = False);
    property EditActions: IOTAEditActions read GetEditActions;
    property MessageServices: IOTAMessageServices read GetMessageServices;
    property ModuleServices: IOTAModuleServices read GetModuleServices;
    property ProjectGroup: IOTAProjectGroup read GetProjectGroup;
    property Services: IOTAServices read GetServices;
    property OnAfterCompilation: TInstantOTAAfterCompilationEvent read
        GetOnAfterCompilation write SetOnAfterCompilation;
    property OnBeforeCompilation: TInstantOTABeforeCompilationEvent read
        GetOnBeforeCompilation write SetOnBeforeCompilation;
    property OnFileNotification: TInstantOTAFileNotificationEvent
      read GetOnFileNotification write SetOnFileNotification;
    property OnModuleNotification: TInstantOTAModuleNotificationEvent
      read GetOnModuleNotification write SetOnModuleNotification;
    property OnModuleRenamedNotification: TInstantOTAModuleRenamedNotificationEvent
        read GetOnModuleRenamedNotification write SetOnModuleRenamedNotification;
  end;

  TInstantOTAIDENotifier5 = class(TNotifierObject, IOTANotifier, IOTAIDENotifier50)
  private
    FNotifierIndex: Integer;
    FOwner: TInstantOTAIDEInterface;
    FOnAfterCompilation: TInstantOTAAfterCompilationEvent;
    FOnBeforeCompilation: TInstantOTABeforeCompilationEvent;
    FModuleNotifierList: TList;
    FOnFileNotification: TInstantOTAFileNotificationEvent;
    FOnModuleNotification: TInstantOTAModuleNotificationEvent;
    FOnModuleRenamedNotification: TInstantOTAModuleRenamedNotificationEvent;
    procedure ClearModuleNotifiers;
    function GetModuleNotifierCount: Integer;
    function GetModuleNotifierList: TList;
    function GetModuleNotifiers(Index: Integer): TInstantOTAModuleNotifier;
    function HasNotifierBeenInstalled(const AFileName: string): Boolean;
    procedure InstallModuleNotifier(Module: IOTAModule); overload;
    procedure RemoveModuleNotifiers;
  protected
    procedure DoAfterCompilation(Succeeded, IsCodeInsight: Boolean); overload;
    procedure DoAfterCompilation(Succeeded: Boolean); overload;
    procedure DoBeforeCompilation(const Project: IOTAProject;
      IsCodeInsight: Boolean; var Cancel: Boolean);
    function IsValidModuleFileName(const AFileName: string): Boolean;
    procedure RegisterNotifier; virtual;
    procedure UnregisterNotifier; virtual;
    property ModuleNotifierList: TList read GetModuleNotifierList;
    property NotifierIndex: Integer read FNotifierIndex write FNotifierIndex;
  public
    constructor Create(AOwner: TInstantOTAIDEInterface);
    destructor Destroy; override;
    // IOTAIDENotifier
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    // IOTAIDENotifier50
    procedure AfterCompile(Succeeded, IsCodeInsight: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;

    procedure AddModuleNotifier(Notifier: TInstantOTAModuleNotifier);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure InstallModuleNotifier(const FileName: string); overload;
    procedure ModuleNotification(const FileName: string; NotifyCode:
        TModuleNotifyCode);
    procedure ModuleRenamed(const OldName, NewName: string);
    function RemoveModuleNotifier(Notifier: TInstantOTAModuleNotifier): Integer;
    property ModuleNotifierCount: Integer read GetModuleNotifierCount;
    property ModuleNotifiers[Index: Integer]: TInstantOTAModuleNotifier read
        GetModuleNotifiers;
    property OnAfterCompilation: TInstantOTAAfterCompilationEvent read
        FOnAfterCompilation write FOnAfterCompilation;
    property OnBeforeCompilation: TInstantOTABeforeCompilationEvent read
        FOnBeforeCompilation write FOnBeforeCompilation;
    property OnFileNotification: TInstantOTAFileNotificationEvent read
        FOnFileNotification write FOnFileNotification;
    property OnModuleNotification: TInstantOTAModuleNotificationEvent read
        FOnModuleNotification write FOnModuleNotification;
    property OnModuleRenamedNotification: TInstantOTAModuleRenamedNotificationEvent
        read FOnModuleRenamedNotification write FOnModuleRenamedNotification;
  end;

  TInstantOTAIDENotifier8 = class(TInstantOTAIDENotifier5, IOTANotifier,
      IOTAIDENotifier50, IOTAIDENotifier80)
  protected
    procedure DoAfterCompilation(const Project: IOTAProject; Succeeded,
        IsCodeInsight: Boolean); overload;
  public
    // IOTAIDENotifier80
    procedure AfterCompile(const Project: IOTAProject; Succeeded: Boolean;
        IsCodeInsight: Boolean); overload;
  end;

  TInstantOTAModuleNotifier = class(TNotifierObject, IOTANotifier,
      IOTAModuleNotifier, IInstantOTANotifierUninstallation)
  private
    FFileName: string;
    FEditorNotifierList: TInterfaceList;
    FModuleInterface: IOTAModule;
    FNotifierIndex: Integer;
    FOwner: TInstantOTAIDENotifier5;
    function GetEditorNotifierCount: Integer;
    function GetEditorNotifierList: TInterfaceList;
    procedure InstallEditorNotifiers;
    procedure RemoveEditorNotifiers;
    procedure RemoveSelfFromOwner;
    procedure RemoveSelfNotifier;
  protected
    property EditorNotifierList: TInterfaceList read GetEditorNotifierList;
    property EditorNotifierCount: Integer read GetEditorNotifierCount;
    function GetModuleInterface: IOTAModule;
    property ModuleInterface: IOTAModule read GetModuleInterface;
    // IOTANotifier
    procedure Destroyed;
    // IOTAModuleNotifier
    function CheckOverwrite: Boolean;
    procedure ModuleNotification(const AFileName: string; NotifyCode:
        TModuleNotifyCode);
    procedure ModuleRenamed(const NewName: string);
    // IInstantOTANotifierUninstallation
    procedure UnInstallNotifier;
  public
    constructor Create(AOwner: TInstantOTAIDENotifier5; AModuleInterface:
        IOTAModule);
    destructor Destroy; override;
  end;

  TInstantOTAMessage = class(TInterfacedObject, IOTACustomMessage)
  private
    FColumnNumber: Integer;
    FFileName: string;
    FLineNumber: Integer;
    FMessage: string;
    FToolName: string;
  public
    constructor Create(AToolName, AFileName: string; ALineNumber,
      AColumnNumber: Integer; AMessage: string);
    function GetColumnNumber: Integer;
    function GetFileName: string;
    function GetLineNumber: Integer;
    function GetLineText: string;
    procedure ShowHelp;
    property ToolName: string read FToolName;
  end;

  TInstantOTABaseEditorNotifier = class(TNotifierObject, IOTANotifier,
      IInstantOTANotifierUninstallation)
  private
    FNotifierIndex: Integer;
    FOwner: TInstantOTAModuleNotifier;
  protected
    FFileName: string;
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    // IInstantOTANotifierUninstallation
    procedure UnInstallNotifier;

    procedure RemoveSelfFromOwner;
    procedure RemoveSelfNotifier; virtual; abstract;
  end;

  TInstantOTAEditorNotifier = class(TInstantOTABaseEditorNotifier, IOTANotifier,
      IOTAEditorNotifier, IInstantOTANotifierUninstallation)
  private
    FSourceInterface: IOTASourceEditor;
  protected
    procedure Destroyed;
    procedure Modified;
    // IOTAEditorNotifier
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);

    procedure RemoveSelfNotifier; override;
  public
    constructor Create(AOwner: TInstantOTAModuleNotifier; ASourceEditorInterface:
        IOTASourceEditor);
    destructor Destroy; override;
  end;

  TInstantOTAFormNotifier = class(TInstantOTABaseEditorNotifier, IOTANotifier,
      IOTAFormNotifier, IInstantOTANotifierUninstallation)
  private
    FFormInterface: IOTAFormEditor;
  protected
    procedure Destroyed;
    procedure Modified;
    // IOTAFormNotifier
    procedure FormActivated;
    procedure FormSaving;
    procedure ComponentRenamed(ComponentHandle: TOTAHandle; const OldName, NewName:
        string);

    procedure RemoveSelfNotifier; override;
  public
    constructor Create(AOwner: TInstantOTAModuleNotifier; AFormEditorInterface:
        IOTAFormEditor);
    destructor Destroy; override;
  end;

implementation

uses
  WinApi.Windows
  , System.SysUtils
  , Vcl.Dialogs
  , Vcl.Controls
  ;

type
  // Use the PNoRefCnt type to assign interface instances without invoking
  // reference counting.  Only use this if you have a very good reason.
  PNoRefCnt = Pointer;

const
  InvalidNotifierIndex = -1;

{ TInstantOTAIDEInterface }

function TInstantOTAIDEInterface.CurrentModule: IOTAModule;
begin
  ModuleServices.CurrentModule;
end;

destructor TInstantOTAIDEInterface.Destroy;
begin
  FIDENotifier5.Free;
  inherited;
end;

function TInstantOTAIDEInterface.FindForm(Name, ClassName: string): TForm;
var
  I: Integer;
begin
  for I := 0 to Pred(Screen.FormCount) do
  begin
    Result := Screen.Forms[I];
    if SameText(Result.Name, Name) and
      SameText(Result.ClassName, ClassName) then
        Exit;
  end;
  Result := nil;
end;

function TInstantOTAIDEInterface.FindModule(
  const Name: string): IOTAModule;
var
  Project: IOTAProject;
  I: Integer;
  ModuleInfo: IOTAModuleInfo;
begin
  Project := ProjectGroup.ActiveProject;
  if Assigned(Project) then
    for I := 0 to Pred(Project.GetModuleCount) do
    begin
      ModuleInfo := Project.GetModule(I);
      if SameText(ModuleInfo.Name, Name) then
      begin
        Result := ModuleInfo.OpenModule;
        Exit;
      end;
    end;
  Result := nil;
end;

function TInstantOTAIDEInterface.GetOnAfterCompilation:
    TInstantOTAAfterCompilationEvent;
begin
  Result := IDENotifier5.OnAfterCompilation;
end;

function TInstantOTAIDEInterface.GetOnBeforeCompilation:
    TInstantOTABeforeCompilationEvent;
begin
  Result := IDENotifier5.OnBeforeCompilation;
end;

function TInstantOTAIDEInterface.GetEditActions: IOTAEditActions;
var
  I, J, K: Integer;
  Module: IOTAModule;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
  EditView: IOTAEditView;
begin
  for I := 0 to Pred(ModuleServices.ModuleCount) do
  begin
    Module := ModuleServices.Modules[I];
    for J := 0 to Pred(Module.GetModuleFileCount) do
    begin
      Editor := Module.GetModuleFileEditor(J);
      if Editor.QueryInterface(IOTASourceEditor, SourceEditor) = S_OK then
      begin
        for K := 0 to Pred(SourceEditor.EditViewCount) do
        begin
          EditView := SourceEditor.EditViews[K];
          if EditView.QueryInterface(IOTAEditActions, Result) = S_OK then
            Exit;
        end;
      end;
    end;
  end;
  Result := nil;
end;

function TInstantOTAIDEInterface.GetIDENotifier5: TInstantOTAIDENotifier5;
begin
  if not Assigned(FIDENotifier5) then
    FIDENotifier5 := TInstantOTAIDENotifier8.Create(Self);
  Result := FIDENotifier5;
end;

function TInstantOTAIDEInterface.GetMessageServices: IOTAMessageServices;
begin
  Result := BorlandIDEServices as IOTAMessageServices;
end;

function TInstantOTAIDEInterface.GetModuleServices: IOTAModuleServices;
begin
  Result := Services as IOTAModuleServices;
end;

function TInstantOTAIDEInterface.GetOnFileNotification: TInstantOTAFileNotificationEvent;
begin
  Result := IDENotifier5.OnFileNotification;
end;

function TInstantOTAIDEInterface.GetOnModuleNotification: TInstantOTAModuleNotificationEvent;
begin
  Result := IDENotifier5.OnModuleNotification;
end;

function TInstantOTAIDEInterface.GetOnModuleRenamedNotification:
    TInstantOTAModuleRenamedNotificationEvent;
begin
  Result := IDENotifier5.OnModuleRenamedNotification;
end;

function TInstantOTAIDEInterface.GetProjectGroup: IOTAProjectGroup;
var
  I: Integer;
begin
  with ModuleServices do
    for I := 0 to Pred(ModuleCount) do
      if Modules[I].QueryInterface(IOTAProjectGroup, Result) = S_OK then
        Exit;
  Result := nil;
end;

function TInstantOTAIDEInterface.GetServices: IOTAServices;
begin
  Result := BorlandIDEServices as IOTAServices;
  if not Assigned(Result) then
    raise Exception.Create('Delphi IOTAServices unavailable');
end;

procedure TInstantOTAIDEInterface.GotoFilePos(const FileName: string;
  Line, Column: Integer);
var
  EditView: IOTAEditView;
  EditPos: TOTAEditPos;
  Editor: TWinControl;
begin
  if (BorlandIDEServices as IOTAActionServices).OpenFile(FileName) then
  begin
    EditView := (BorlandIDEServices as IOTAEditorServices).TopView;
    if Assigned(EditView) then
    begin
      with EditView.GetEditWindow.GetForm do
      begin
        Editor := FindComponent('Editor') as TWinControl;
        if Assigned(Editor) and Editor.CanFocus then
          Editor.SetFocus;
      end;
      EditPos.Line := Line;
      EditPos.Col := Column;
      EditView.CursorPos := EditPos; // *must* be set before SetTopLeft
      EditView.SetTopLeft(Line, 1);
    end;
  end;
end;

function TInstantOTAIDEInterface.ReadEditorSource(
  Editor: IOTASourceEditor): string;
var
  Reader: IOTAEditReader;
  Buffer: InstantOTAString;
  BufferLen, ReadLen, Position: Integer;
begin
  if Assigned(Editor) then
  begin
    BufferLen := 30000;
    Reader := Editor.CreateReader;
    Result := '';
    Position := 0;
    repeat
      SetLength(Buffer, BufferLen);
      ReadLen := Reader.GetText(Position, PAnsiChar(Buffer), BufferLen);
      if ReadLen < BufferLen then // ?? What does these two lines do??
        Dec(ReadLen, 2);          // ??
      SetLength(Buffer, ReadLen);
      Result := Result + string(Buffer);
      Inc(Position, ReadLen);
    until ReadLen < BufferLen - 1;
  end else
    Result := '';
//  ShowMessage(Result);
end;

function TInstantOTAIDEInterface.ReadModuleSource(
  Module: IOTAModule): string;
begin
  Result := ReadEditorSource(SourceEditor(Module));
end;

procedure TInstantOTAIDEInterface.SetOnAfterCompilation(const Value:
    TInstantOTAAfterCompilationEvent);
begin
  IDENotifier5.OnAfterCompilation := Value;
end;

procedure TInstantOTAIDEInterface.SetOnBeforeCompilation(const Value:
    TInstantOTABeforeCompilationEvent);
begin
  IDENotifier5.OnBeforeCompilation := Value;
end;

procedure TInstantOTAIDEInterface.SetOnFileNotification(
  const Value: TInstantOTAFileNotificationEvent);
begin
  IDENotifier5.OnFileNotification := Value;
end;

procedure TInstantOTAIDEInterface.SetOnModuleNotification(
  const Value: TInstantOTAModuleNotificationEvent);
begin
  IDENotifier5.OnModuleNotification := Value;
end;

procedure TInstantOTAIDEInterface.SetOnModuleRenamedNotification(const Value:
    TInstantOTAModuleRenamedNotificationEvent);
begin
  IDENotifier5.OnModuleRenamedNotification := Value;
end;

procedure TInstantOTAIDEInterface.ShowMessages;
var
  MessageView: TForm;
begin
  MessageView := FindForm('MessageView', 'TMessageViewForm');
  if Assigned(MessageView) then
    MessageView.Show;
  EditActions.NextError;
end;

function TInstantOTAIDEInterface.SourceEditor(
  Module: IOTAModule): IOTASourceEditor;
var
  I: Integer;
  Editor: IOTAEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;
  for I := 0 to Pred(Module.GetModuleFileCount) do
  begin
    Editor := Module.GetModuleFileEditor(I);
    if Editor.QueryInterface(IOTASourceEditor, Result) = S_OK then
      Break;
  end;
end;

procedure TInstantOTAIDEInterface.WriteEditorSource(
  Editor: IOTASourceEditor; const Source: string; ReplaceLen: Integer;
  Undoable: Boolean);
var
  Writer: IOTAEditWriter;
begin
  if not Assigned(Editor) then
    Exit;
  if Undoable then
    Writer := Editor.CreateUndoableWriter else
    Writer := Editor.CreateWriter;
  Writer.DeleteTo(ReplaceLen);
  Writer.Insert(PAnsiChar(InstantOTAString(Source)));
end;

{ TInstantOTAIDENotifier5 }

procedure TInstantOTAIDENotifier5.AfterCompile(Succeeded, IsCodeInsight:
    Boolean);
begin
  DoAfterCompilation(Succeeded, IsCodeInsight);
end;

procedure TInstantOTAIDENotifier5.AfterCompile(Succeeded: Boolean);
begin
  DoAfterCompilation(Succeeded);
end;

procedure TInstantOTAIDENotifier5.AfterConstruction;
begin
  inherited;
  _AddRef;
end;

procedure TInstantOTAIDENotifier5.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  DoBeforeCompilation(Project, False, Cancel);
end;

procedure TInstantOTAIDENotifier5.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  DoBeforeCompilation(Project, IsCodeInsight, Cancel);
end;

procedure TInstantOTAIDENotifier5.BeforeDestruction;
begin
end;

constructor TInstantOTAIDENotifier5.Create(AOwner: TInstantOTAIDEInterface);
begin
  inherited Create;
  FOwner := AOwner;
  FNotifierIndex := InvalidNotifierIndex;
  RegisterNotifier;
end;

destructor TInstantOTAIDENotifier5.Destroy;
begin
  // Sometimes we never get destroy notifications for DFMs open as text
  // This check prevents us from AVing on shutdown trying to remove the
  // notifiers for these by-now-closed units
  if not Application.Terminated then
  begin
    RemoveModuleNotifiers;
    ClearModuleNotifiers;
  end;

  FreeAndNil(FModuleNotifierList);
  UnregisterNotifier;
  FOwner := nil;
  inherited;
end;

procedure TInstantOTAIDENotifier5.AddModuleNotifier(Notifier:
    TInstantOTAModuleNotifier);
begin
  ModuleNotifierList.Add(Notifier);
end;

procedure TInstantOTAIDENotifier5.ClearModuleNotifiers;
//var
//  I: Integer;
//  UnaccountedModules: string;
begin
  if not Assigned(FModuleNotifierList) then
    Exit;

//  if ModuleNotifierCount > 0 then
//  begin
//    // If module notifiers are still installed at this
//    // stage, it means that some cleanup logic failed.
//    for I := 0 to Pred(ModuleNotifierCount) do
//    begin
//      with ModuleNotifiers[I] do
//        UnaccountedModules := UnaccountedModules + ModuleInterface.FileName + '; ';
//    end;
//    CodeSite.SendFmtMsg('Warning - IdeNotifier has %d unaccounted modules (%s)',
//      [ModuleNotifierCount, UnaccountedModules]);
//  end;
  Assert(ModuleNotifierCount = 0,
      ClassName + ': [ClearModuleNotifiers] Some module notifier cleanup logic failed!');
  FModuleNotifierList.Clear;
end;

procedure TInstantOTAIDENotifier5.DoAfterCompilation(Succeeded, IsCodeInsight: Boolean);
begin
  if Assigned(FOnAfterCompilation) then
    FOnAfterCompilation(Self, nil, Succeeded, IsCodeInsight);
end;

procedure TInstantOTAIDENotifier5.DoAfterCompilation(Succeeded: Boolean);
begin
  if Assigned(FOnAfterCompilation) then
    FOnAfterCompilation(Self, nil, Succeeded, False);
end;

procedure TInstantOTAIDENotifier5.DoBeforeCompilation(
  const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  if Assigned(FOnBeforeCompilation) then
    FOnBeforeCompilation(Self, Project, IsCodeInsight, Cancel);
end;

procedure TInstantOTAIDENotifier5.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  case NotifyCode of
    ofnFileOpening: ;
    ofnFileOpened:
      InstallModuleNotifier(FileName);
    ofnFileClosing: ;
    ofnDefaultDesktopLoad: ;
    ofnDefaultDesktopSave: ;
    ofnProjectDesktopLoad: ;
    ofnProjectDesktopSave: ;
    ofnPackageInstalled: ;
    ofnPackageUninstalled: ;
  end;
  if Assigned(FOnFileNotification) then
    FOnFileNotification(Self, NotifyCode, FileName, Cancel);
end;

function TInstantOTAIDENotifier5.GetModuleNotifierCount: Integer;
begin
  Result := ModuleNotifierList.Count;
end;

function TInstantOTAIDENotifier5.GetModuleNotifierList: TList;
begin
  if not Assigned(FModuleNotifierList) then
    FModuleNotifierList := TList.Create;
  Result := FModuleNotifierList;
end;

function TInstantOTAIDENotifier5.GetModuleNotifiers(Index: Integer):
    TInstantOTAModuleNotifier;
begin
  Result := TInstantOTAModuleNotifier(ModuleNotifierList[Index]);
end;

function TInstantOTAIDENotifier5.HasNotifierBeenInstalled(const AFileName:
    string): Boolean;
var
  ModuleNotifier: TInstantOTAModuleNotifier;
  Module: IOTAModule;
  I: Integer;
begin
  Result := False;
  if not Assigned(FModuleNotifierList) then
    Exit;

  for I := 0 to Pred(ModuleNotifierCount) do
  begin
    ModuleNotifier := ModuleNotifiers[I];
    Assert(Assigned(ModuleNotifier));

    Module := ModuleNotifier.ModuleInterface;
    Assert(Assigned(Module));

    Result := AnsiSameText(Module.FileName, AFileName);
    if Result then
      Break;
  end;
end;

procedure TInstantOTAIDENotifier5.InstallModuleNotifier(const FileName: string);
var
  ModuleInterface: IOTAModule;
  ModuleServices: IOTAModuleServices;
begin
  if not IsValidModuleFileName(FileName) then
    Exit;
  
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  
  ModuleInterface := ModuleServices.FindModule(FileName);
  if not Assigned(ModuleInterface) then
    Exit;
  
  // Check to prevent duplicate installation of module notifiers
  if not HasNotifierBeenInstalled(FileName) then
    InstallModuleNotifier(ModuleInterface);
end;

procedure TInstantOTAIDENotifier5.InstallModuleNotifier(Module: IOTAModule);
var
  ModuleNotifier: TInstantOTAModuleNotifier;
begin
  Assert(Assigned(Module));
  if not IsValidModuleFileName(Module.FileName) then
    Exit;

  ModuleNotifier := TInstantOTAModuleNotifier.Create(Self, Module);
  ModuleNotifierList.Add(ModuleNotifier);
end;

function TInstantOTAIDENotifier5.IsValidModuleFileName(const AFileName:
    string): Boolean;
begin
  // Ignore the default.htm module (the Welcome Page) in Delphi 8+
  Result := not (SameFileName(AFileName, 'default.htm') or
        SameFileName(AFileName, ('bds:/default.htm')));
end;

procedure TInstantOTAIDENotifier5.ModuleNotification(const FileName: string;
    NotifyCode: TModuleNotifyCode);
begin
  if Assigned(FOnModuleNotification) then
    FOnModuleNotification(Self, NotifyCode, FileName);
end;

procedure TInstantOTAIDENotifier5.ModuleRenamed(const OldName, NewName: string);
begin
  if Assigned(FOnModuleRenamedNotification) then
    OnModuleRenamedNotification(Self, OldName, NewName);
end;

procedure TInstantOTAIDENotifier5.RegisterNotifier;
var
  Services: IOTAServices;
begin
  Services := BorlandIDEServices as IOTAServices;
  if Assigned(Services) then
  begin
    NotifierIndex := Services.AddNotifier(Self as IOTAIDENotifier50);
    Assert(NotifierIndex <> InvalidNotifierIndex);
  end;
end;

procedure TInstantOTAIDENotifier5.RemoveModuleNotifiers;
var
  INotifierUninstallation: IInstantOTANotifierUninstallation;
begin
  if Assigned(FModuleNotifierList) then
  begin
    while ModuleNotifierCount > 0 do
    begin
      INotifierUninstallation := ModuleNotifiers[0] as IInstantOTANotifierUninstallation;
      Assert(Assigned(INotifierUninstallation));

      INotifierUninstallation.UninstallNotifier;
    end;
  end;
end;

function TInstantOTAIDENotifier5.RemoveModuleNotifier(Notifier:
    TInstantOTAModuleNotifier): Integer;
begin
  Result := ModuleNotifierList.Remove(Notifier);
end;

procedure TInstantOTAIDENotifier5.UnregisterNotifier;
var
  Services: IOTAServices;
begin
  Services := BorlandIDEServices as IOTAServices;
  if Assigned(Services)  and (NotifierIndex <> InvalidNotifierIndex) then
  begin
    Services.RemoveNotifier(NotifierIndex);
    NotifierIndex := InvalidNotifierIndex;
  end;
end;

{ TInstantOTAModuleNotifier }

constructor TInstantOTAModuleNotifier.Create(AOwner: TInstantOTAIDENotifier5;
    AModuleInterface: IOTAModule);
begin
  inherited Create;
  FFileName := AModuleInterface.FileName;
  Assert(Assigned(AOwner));
  FOwner := AOwner;
  FNotifierIndex := InvalidNotifierIndex;

  // Keep a reference to the associated module
  // without any reference counting.
  PNoRefCnt(FModuleInterface) := PNoRefCnt(AModuleInterface);

  FNotifierIndex := FModuleInterface.AddNotifier(Self);

  InstallEditorNotifiers;
end;

function TInstantOTAModuleNotifier.CheckOverwrite: Boolean;
begin
  Result := True;
end;

destructor TInstantOTAModuleNotifier.Destroy;
begin
  RemoveEditorNotifiers;
  FreeAndNil(FEditorNotifierList);

  RemoveSelfNotifier;
  PNoRefCnt(FModuleInterface) := nil;
  RemoveSelfFromOwner;
  inherited;
end;

procedure TInstantOTAModuleNotifier.Destroyed;
begin
  RemoveSelfNotifier;

  PNoRefCnt(FModuleInterface) := nil;

  RemoveSelfFromOwner;
end;

function TInstantOTAModuleNotifier.GetEditorNotifierCount: Integer;
begin
  Result := EditorNotifierList.Count;
end;

function TInstantOTAModuleNotifier.GetEditorNotifierList: TInterfaceList;
begin
  if not Assigned(FEditorNotifierList) then
    FEditorNotifierList := TInterfaceList.Create;
  Result := FEditorNotifierList;
end;

function TInstantOTAModuleNotifier.GetModuleInterface: IOTAModule;
begin
  Result := FModuleInterface;
end;

procedure TInstantOTAModuleNotifier.InstallEditorNotifiers;
var
  I: Integer;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
  FormEditor: IOTAFormEditor;
  Notifier: IOTANotifier;
begin
  Assert(Assigned(FModuleInterface));

  for I := 0 to FModuleInterface.GetModuleFileCount - 1 do
  begin
    Editor := FModuleInterface.GetModuleFileEditor(I);
    Assert(Assigned(Editor));

    if Trim(Editor.FileName) = '' then
      Continue; // Don't register notifiers for blank file names

    if Supports(Editor, IOTASourceEditor, SourceEditor) then
    begin
      if Assigned(SourceEditor) then
      begin
        Notifier := TInstantOTAEditorNotifier.Create(Self, SourceEditor);
        EditorNotifierList.Add(Notifier);
      end;
    end;

    if Supports(Editor, IOTAFormEditor, FormEditor) then
    begin
      if Assigned(FormEditor) then
      begin
        Notifier := TInstantOTAFormNotifier.Create(Self, FormEditor);
        EditorNotifierList.Add(Notifier);
      end;
    end;
  end;
end;

procedure TInstantOTAModuleNotifier.ModuleNotification(const AFileName: string;
    NotifyCode: TModuleNotifyCode);
begin
  FOwner.ModuleNotification(AFileName, NotifyCode);
end;

procedure TInstantOTAModuleNotifier.ModuleRenamed(const NewName: string);
begin
  FOwner.ModuleRenamed(FFileName, NewName);
end;

procedure TInstantOTAModuleNotifier.RemoveEditorNotifiers;
var
  INotifierUninstallation: IInstantOTANotifierUninstallation;
begin
  while EditorNotifierCount > 0 do
  begin
    INotifierUninstallation := EditorNotifierList[0] as IInstantOTANotifierUninstallation;
    Assert(Assigned(INotifierUninstallation));

    INotifierUninstallation.UninstallNotifier;
  end;
end;

procedure TInstantOTAModuleNotifier.RemoveSelfFromOwner;
var
  RemovalIndex: Integer;
begin
  if Assigned(FOwner) then
  begin
    RemovalIndex := FOwner.ModuleNotifierList.Remove(Self);
    Assert(RemovalIndex <> -1);
    FOwner := nil;
  end;
end;

procedure TInstantOTAModuleNotifier.RemoveSelfNotifier;
begin
  if not Assigned(FModuleInterface) then
    Exit;

  if FNotifierIndex <> InvalidNotifierIndex then
  begin
    FModuleInterface.RemoveNotifier(FNotifierIndex);
    FNotifierIndex := InvalidNotifierIndex;
  end;
end;

procedure TInstantOTAModuleNotifier.UnInstallNotifier;
begin
  RemoveSelfNotifier;
  RemoveEditorNotifiers;
  RemoveSelfFromOwner;
end;

{ TInstantOTAMessage }

constructor TInstantOTAMessage.Create(AToolName, AFileName: string;
  ALineNumber, AColumnNumber: Integer; AMessage: string);
begin
  inherited Create;
  FToolName := AToolName;
  FFileName := AFileName;
  FLineNumber := ALineNumber;
  FColumnNumber := AColumnNumber;
  FMessage := AMessage;
end;

function TInstantOTAMessage.GetColumnNumber: Integer;
begin
  Result := FColumnNumber;
end;

function TInstantOTAMessage.GetFileName: string;
begin
  Result := FFileName;
end;

function TInstantOTAMessage.GetLineNumber: Integer;
begin
  Result := FLineNumber;
end;

function TInstantOTAMessage.GetLineText: string;
begin
  Result := Format('[%s] %s(%d): %s',
    [FToolName, FFileName, FLineNumber, FMessage]);
end;

procedure TInstantOTAMessage.ShowHelp;
begin
end;

procedure TInstantOTAIDENotifier8.AfterCompile(const Project: IOTAProject;
    Succeeded: Boolean; IsCodeInsight: Boolean);
begin
  DoAfterCompilation(Project, Succeeded, IsCodeInsight);
end;

procedure TInstantOTAIDENotifier8.DoAfterCompilation(const Project:
    IOTAProject; Succeeded, IsCodeInsight: Boolean);
begin
  if Assigned(FOnAfterCompilation) then
    FOnAfterCompilation(Self, Project, Succeeded, IsCodeInsight);
end;

constructor TInstantOTAEditorNotifier.Create(AOwner: TInstantOTAModuleNotifier;
    ASourceEditorInterface: IOTASourceEditor);
begin
  inherited Create;
  FNotifierIndex := InvalidNotifierIndex;
  Assert(Assigned(ASourceEditorInterface));
  FFileName := ASourceEditorInterface.FileName;

  Assert(Assigned(AOwner));
  FOwner := AOwner;

  PNoRefCnt(FSourceInterface) := PNoRefCnt(ASourceEditorInterface);

  FNotifierIndex := ASourceEditorInterface.AddNotifier(Self);
end;

destructor TInstantOTAEditorNotifier.Destroy;
begin
  FOwner := nil;
  PNoRefCnt(FSourceInterface) := nil;
  inherited;
end;

procedure TInstantOTAEditorNotifier.Destroyed;
begin
  PNoRefCnt(FSourceInterface) := nil;
end;

procedure TInstantOTAEditorNotifier.Modified;
begin
  FOwner.ModuleNotification(FFileName, mncEditorModified);
end;

procedure TInstantOTAEditorNotifier.RemoveSelfNotifier;
begin
  if not Assigned(FSourceInterface) then
    Exit;
  
  if FNotifierIndex <> InvalidNotifierIndex then
  begin
    FSourceInterface.RemoveNotifier(FNotifierIndex);
    FNotifierIndex := InvalidNotifierIndex;
  end;
end;

procedure TInstantOTAEditorNotifier.ViewActivated(const View: IOTAEditView);
begin
  FOwner.ModuleNotification(FFileName, mncEditorSelected);
end;

procedure TInstantOTAEditorNotifier.ViewNotification(const View: IOTAEditView;
    Operation: TOperation);
begin
end;

constructor TInstantOTAFormNotifier.Create(AOwner: TInstantOTAModuleNotifier;
    AFormEditorInterface: IOTAFormEditor);
begin
  inherited Create;
  FNotifierIndex := InvalidNotifierIndex;
  Assert(Assigned(AFormEditorInterface));
  FFileName := AFormEditorInterface.FileName;

  Assert(Assigned(AOwner));
  FOwner := AOwner;

  PNoRefCnt(FFormInterface) := PNoRefCnt(AFormEditorInterface);

  FNotifierIndex := AFormEditorInterface.AddNotifier(Self);
end;

destructor TInstantOTAFormNotifier.Destroy;
begin
  FOwner := nil;
  PNoRefCnt(FFormInterface) := nil;
  inherited;
end;

procedure TInstantOTAFormNotifier.ComponentRenamed(ComponentHandle: TOTAHandle;
    const OldName, NewName: string);
begin
end;

procedure TInstantOTAFormNotifier.Destroyed;
begin
  PNoRefCnt(FFormInterface) := nil;
end;

procedure TInstantOTAFormNotifier.FormActivated;
begin
  FOwner.ModuleNotification(FFileName, mncFormSelected);
end;

procedure TInstantOTAFormNotifier.FormSaving;
begin
  FOwner.ModuleNotification(FFileName, mncFormSaving);
end;

procedure TInstantOTAFormNotifier.Modified;
begin
  FOwner.ModuleNotification(FFileName, mncFormModified);
end;

procedure TInstantOTAFormNotifier.RemoveSelfNotifier;
begin
  if not Assigned(FFormInterface) then
    Exit;

  if FNotifierIndex <> InvalidNotifierIndex then
  begin
    FFormInterface.RemoveNotifier(FNotifierIndex);
    FNotifierIndex := InvalidNotifierIndex;
  end;
end;

procedure TInstantOTABaseEditorNotifier.AfterSave;
begin
  FOwner.ModuleNotification(FFileName, mncAfterSave);
end;

procedure TInstantOTABaseEditorNotifier.BeforeSave;
begin
  FOwner.ModuleNotification(FFileName, mncBeforeSave);
end;

procedure TInstantOTABaseEditorNotifier.RemoveSelfFromOwner;
var
  RemovalIndex: Integer;
  ISelfNotifier: IOTANotifier;
begin
  if Assigned(FOwner) then
  begin
    ISelfNotifier := Self as IOTANotifier;

    RemovalIndex := FOwner.EditorNotifierList.Remove(ISelfNotifier);
    Assert(RemovalIndex <> -1);
    FOwner := nil;
  end;
end;

procedure TInstantOTABaseEditorNotifier.UnInstallNotifier;
begin
  RemoveSelfNotifier;
  RemoveSelfFromOwner;
end;


end.
