(*
 *   InstantObjects
 *   Borland OTA Interface (Open Tools API)
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
 *
 * ***** END LICENSE BLOCK ***** *)

unit InstantOTA;

interface

{$I InstantDefines.inc}

{$IFDEF D7+}
{$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

uses
  Classes, ToolsAPI, ExptIntf, ToolIntf, EditIntf, Forms;

type
  TInstantOTAIDEInterface = class;
  TInstantOTAIDENotifier5 = class;
  TInstantOTAIDENotifier3 = class;
  TInstantOTAModuleNotifier = class;

  TInstantOTAAfterCompilationEvent = procedure(Sender: TObject;
    Succeeded: Boolean) of object;
  TInstantOTABeforeCompilationEvent = procedure(Sender: TObject;
    Project: IOTAProject; IsCodeInsight: Boolean;
    var Cancel: Boolean) of object;
  TInstantOTAEventNotificationEvent = procedure(Sender: TObject;
    NotifyCode: TEventNotification; var Cancel: Boolean) of object;
  TInstantOTAFileNotificationEvent = procedure(Sender: TObject;
    NotifyCode: TFileNotification; const FileName: string;
    var Cancel: Boolean) of object;
  TInstantOTAModuleNotificationEvent = procedure(Sender: TObject;
    NotifyCode: TNotifyCode; const FileName: string) of object;

  TInstantOTAIDEInterface = class(TObject)
  private
    FIDENotifier3: TInstantOTAIDENotifier3;
    FIDENotifier5: TInstantOTAIDENotifier5;
    function GetAfterCompilation: TInstantOTAAfterCompilationEvent;
    function GetBeforeCompilation: TInstantOTABeforeCompilationEvent;
    function GetEditActions: IOTAEditActions;
    function GetIDENotifier3: TInstantOTAIDENotifier3;
    function GetIDENotifier5: TInstantOTAIDENotifier5;
    function GetMessageServices: IOTAMessageServices;
    function GetModuleServices: IOTAModuleServices;
    function GetOnEventNotification: TInstantOTAEventNotificationEvent;
    function GetOnFileNotification: TInstantOTAFileNotificationEvent;
    function GetOnModuleNotification: TInstantOTAModuleNotificationEvent;
    function GetProjectGroup: IOTAProjectGroup;
    function GetServices3: TIToolServices;
    function GetServices5: IOTAServices;
    procedure SetAfterCompilation(const Value: TInstantOTAAfterCompilationEvent);
    procedure SetBeforeCompilation(const Value: TInstantOTABeforeCompilationEvent);
    procedure SetOnEventNotification(
      const Value: TInstantOTAEventNotificationEvent);
    procedure SetOnFileNotification(
      const Value: TInstantOTAFileNotificationEvent);
    procedure SetOnModuleNotification(
      const Value: TInstantOTAModuleNotificationEvent);
  protected
    function FindForm(Name, ClassName: string): TForm;
    property IDENotifier3: TInstantOTAIDENotifier3 read GetIDENotifier3;
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
    property AfterCompilation: TInstantOTAAfterCompilationEvent read GetAfterCompilation write SetAfterCompilation;
    property BeforeCompilation: TInstantOTABeforeCompilationEvent read GetBeforeCompilation write SetBeforeCompilation;
    property EditActions: IOTAEditActions read GetEditActions;
    property MessageServices: IOTAMessageServices read GetMessageServices;
    property ModuleServices: IOTAModuleServices read GetModuleServices;
    property ProjectGroup: IOTAProjectGroup read GetProjectGroup;
    property Services3: TIToolServices read GetServices3;
    property Services5: IOTAServices read GetServices5;
    property OnEventNotification: TInstantOTAEventNotificationEvent
      read GetOnEventNotification write SetOnEventNotification;
    property OnFileNotification: TInstantOTAFileNotificationEvent
      read GetOnFileNotification write SetOnFileNotification;
    property OnModuleNotification: TInstantOTAModuleNotificationEvent
      read GetOnModuleNotification write SetOnModuleNotification;
  end;

  TInstantOTAIDENotifier5 = class(TNotifierObject, IOTANotifier, IOTAIDENotifier50)
  private
    FNotifierIndex: Integer;
    FOwner: TInstantOTAIDEInterface;
    FAfterCompilation: TInstantOTAAfterCompilationEvent;
    FBeforeCompilation: TInstantOTABeforeCompilationEvent;
  protected
    procedure DoAfterCompilation(Succeeded, IsCodeInsight: Boolean);
    procedure DoBeforeCompilation(const Project: IOTAProject;
      IsCodeInsight: Boolean; var Cancel: Boolean);
  public
    constructor Create(AOwner: TInstantOTAIDEInterface);
    destructor Destroy; override;
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure AfterConstruction; override;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
    procedure BeforeDestruction; override;
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    property AfterCompilation: TInstantOTAAfterCompilationEvent read FAfterCompilation write FAfterCompilation;
    property BeforeCompilation: TInstantOTABeforeCompilationEvent read FBeforeCompilation write FBeforeCompilation;
  end;

  TInstantOTAIDENotifier3 = class(TIAddInNotifier)
  private
    FModuleNotifierList: TList;
    FOwner: TInstantOTAIDEInterface;
    FOnEventNotification: TInstantOTAEventNotificationEvent;
    FOnFileNotification: TInstantOTAFileNotificationEvent;
    FOnModuleNotification: TInstantOTAModuleNotificationEvent;
    function GetModuleNotifierCount: Integer;
    function GetModuleNotifiers(Index: Integer): TInstantOTAModuleNotifier;
    function GetModuleNotifierList: TList;
  protected
    procedure ModuleNotification(FileName: string; NotifyCode: TNotifyCode);
    property ModuleNotifierList: TList read GetModuleNotifierList;
  public
    constructor Create(AOwner: TInstantOTAIDEInterface);
    destructor Destroy; override;
    procedure AddModuleNotifier(Notifier: TInstantOTAModuleNotifier);
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override;
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    function InstallModuleNotifier(FileName: string): TInstantOTAModuleNotifier;
    procedure RemoveModuleNotifier(Notifier: TInstantOTAModuleNotifier);
    property ModuleNotifiers[Index: Integer]: TInstantOTAModuleNotifier
      read GetModuleNotifiers;
    property ModuleNotifierCount: Integer read GetModuleNotifierCount;
    property OnEventNotification: TInstantOTAEventNotificationEvent
      read FOnEventNotification write FOnEventNotification;
    property OnFileNotification: TInstantOTAFileNotificationEvent
      read FOnFileNotification write FOnFileNotification;
    property OnModuleNotification: TInstantOTAModuleNotificationEvent
      read FOnModuleNotification write FOnModuleNotification;
  end;

  TInstantOTAModuleNotifier = class(TIModuleNotifier)
  private
    FFileName: string;
    FModuleInterface: TIModuleInterface;
    FOwner: TInstantOTAIDENotifier3;
  public
    constructor Create(AOwner: TInstantOTAIDENotifier3;
      AModuleInterface: TIModuleInterface; AFileName: string);
    destructor Destroy; override;
    procedure Notify(NotifyCode: TNotifyCode); override;
    {$IFDEF D6+}
    procedure ComponentRenamed(const AComponent: TComponent;
      const OldName, NewName: string); override;
    {$ELSE}
    procedure ComponentRenamed(ComponentHandle: Pointer;
      const OldName, NewName: string); override;
    {$ENDIF}
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

implementation

uses
  Windows, SysUtils, Dialogs, Controls;

{ TInstantOTAIDEInterface }

function TInstantOTAIDEInterface.CurrentModule: IOTAModule;
begin
  ModuleServices.CurrentModule;
end;

destructor TInstantOTAIDEInterface.Destroy;
begin
  MessageServices.ClearAllMessages;
  FIDENotifier3.Free;
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

function TInstantOTAIDEInterface.GetAfterCompilation: TInstantOTAAfterCompilationEvent;
begin
  Result := IDENotifier5.AfterCompilation;
end;

function TInstantOTAIDEInterface.GetBeforeCompilation: TInstantOTABeforeCompilationEvent;
begin
  Result := IDENotifier5.BeforeCompilation;
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

function TInstantOTAIDEInterface.GetIDENotifier3: TInstantOTAIDENotifier3;
begin
  if not Assigned(FIDENotifier3) then
    FIDENotifier3 := TInstantOTAIDENotifier3.Create(Self);
  Result := FIDENotifier3;
end;

function TInstantOTAIDEInterface.GetIDENotifier5: TInstantOTAIDENotifier5;
begin
  if not Assigned(FIDENotifier5) then
    FIDENotifier5 := TInstantOTAIDENotifier5.Create(Self);
  Result := FIDENotifier5;
end;

function TInstantOTAIDEInterface.GetMessageServices: IOTAMessageServices;
begin
  Result := BorlandIDEServices as IOTAMessageServices;
end;

function TInstantOTAIDEInterface.GetModuleServices: IOTAModuleServices;
begin
  Result := Services5 as IOTAModuleServices;
end;

function TInstantOTAIDEInterface.GetOnEventNotification: TInstantOTAEventNotificationEvent;
begin
  Result := IDENotifier3.OnEventNotification;
end;

function TInstantOTAIDEInterface.GetOnFileNotification: TInstantOTAFileNotificationEvent;
begin
  Result := IDENotifier3.OnFileNotification;
end;

function TInstantOTAIDEInterface.GetOnModuleNotification: TInstantOTAModuleNotificationEvent;
begin
  Result := IDENotifier3.OnModuleNotification;
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

function TInstantOTAIDEInterface.GetServices3: TIToolServices;
begin
  Result := ToolServices;
  if not Assigned(Result) then
    raise Exception.Create('Delphi 4 Tool Services unavailable');
end;

function TInstantOTAIDEInterface.GetServices5: IOTAServices;
begin
  Result := BorlandIDEServices as IOTAServices;
  if not Assigned(Result) then
    raise Exception.Create('Delphi 5 Tool Services unavailable');
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
        if Assigned(Editor) then
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
  Buffer: string;
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
      ReadLen := Reader.GetText(Position, PChar(Buffer), BufferLen);
      if ReadLen < BufferLen then
        Dec(ReadLen, 2);
      SetLength(Buffer, ReadLen);
      Result := Result + Buffer;
      Inc(Position, ReadLen);
    until ReadLen < BufferLen - 1;
  end else
    Result := '';
end;

function TInstantOTAIDEInterface.ReadModuleSource(
  Module: IOTAModule): string;
begin
  Result := ReadEditorSource(SourceEditor(Module));
end;

procedure TInstantOTAIDEInterface.SetAfterCompilation(
  const Value: TInstantOTAAfterCompilationEvent);
begin
  IDENotifier5.AfterCompilation := Value;
end;

procedure TInstantOTAIDEInterface.SetBeforeCompilation(
  const Value: TInstantOTABeforeCompilationEvent);
begin
  IDENotifier5.BeforeCompilation := Value;
end;

procedure TInstantOTAIDEInterface.SetOnEventNotification(
  const Value: TInstantOTAEventNotificationEvent);
begin
  IDENotifier3.OnEventNotification := Value;
end;

procedure TInstantOTAIDEInterface.SetOnFileNotification(
  const Value: TInstantOTAFileNotificationEvent);
begin
  IDENotifier3.OnFileNotification := Value;
end;

procedure TInstantOTAIDEInterface.SetOnModuleNotification(
  const Value: TInstantOTAModuleNotificationEvent);
begin
  IDENotifier3.OnModuleNotification := Value;
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
  Writer.Insert(PChar(Source));
end;

{ TInstantOTAIDENotifier5 }

procedure TInstantOTAIDENotifier5.AfterCompile(Succeeded: Boolean);
begin
  DoAfterCompilation(Succeeded, False);
end;

procedure TInstantOTAIDENotifier5.AfterCompile(Succeeded,
  IsCodeInsight: Boolean);
begin
  DoAfterCompilation(Succeeded, IsCodeInsight);
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
var
  Services: IOTAServices;
begin
  inherited Create;
  FOwner := AOwner;
  Services := BorlandIDEServices as IOTAServices;
  if Assigned(Services) then
    FNotifierIndex := Services.AddNotifier(Self as IOTAIDENotifier50);
end;

destructor TInstantOTAIDENotifier5.Destroy;
var
  Services: IOTAServices;
begin
  Services := BorlandIDEServices as IOTAServices;
  if Assigned(Services) then
    Services.RemoveNotifier(FNotifierIndex);
  inherited;
end;

procedure TInstantOTAIDENotifier5.DoAfterCompilation(Succeeded,
  IsCodeInsight: Boolean);
begin
  if Assigned(FAfterCompilation) then
    FAfterCompilation(Self, Succeeded);
end;

procedure TInstantOTAIDENotifier5.DoBeforeCompilation(
  const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  if Assigned(FBeforeCompilation) then
    FBeforeCompilation(Self, Project, IsCodeInsight, Cancel);
end;

procedure TInstantOTAIDENotifier5.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  case NotifyCode of
    ofnFileOpening: ;
    ofnFileOpened: ;
    ofnFileClosing: ;
    ofnDefaultDesktopLoad: ;
    ofnDefaultDesktopSave: ;
    ofnProjectDesktopLoad: ;
    ofnProjectDesktopSave: ;
    ofnPackageInstalled: ;
    ofnPackageUninstalled: ;
  end;
end;

{ TInstantOTAIDENotifier3 }

procedure TInstantOTAIDENotifier3.AddModuleNotifier(
  Notifier: TInstantOTAModuleNotifier);
begin
  ModuleNotifierList.Add(Notifier);
end;

constructor TInstantOTAIDENotifier3.Create(AOwner: TInstantOTAIDEInterface);
begin
  inherited Create;
  FOwner := AOwner;
  if Assigned(ToolServices) then
    ToolServices.AddNotifier(Self);
end;

destructor TInstantOTAIDENotifier3.Destroy;
begin
  if Assigned(FModuleNotifierList) then
  begin
    while ModuleNotifierCount > 0 do
      ModuleNotifiers[0].Free;
    FModuleNotifierList.Free;
  end;
  if Assigned(ToolServices) then
    ToolServices.RemoveNotifier(Self);
  inherited;
end;

procedure TInstantOTAIDENotifier3.EventNotification(
  NotifyCode: TEventNotification; var Cancel: Boolean);
begin
  case NotifyCode of
    enBeforeCompile: ;
    enAfterCompile: ;
  end;
end;

procedure TInstantOTAIDENotifier3.FileNotification(
  NotifyCode: TFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  case NotifyCode of
    fnFileOpening: ;
    fnFileOpened:
      InstallModuleNotifier(FileName);
    fnFileClosing: ;
    fnProjectOpening: ;
    fnProjectOpened: ;
    fnProjectClosing: ;
    fnAddedToProject: ;
    fnRemovedFromProject: ;
    fnDefaultDesktopLoad: ;
    fnDefaultDesktopSave: ;
    fnProjectDesktopLoad: ;
    fnProjectDesktopSave: ;
    fnPackageInstalled: ;
    fnPackageUninstalled: ;
  end;
  if Assigned(FOnFileNotification) then
    FOnFileNotification(Self, NotifyCode, FileName, Cancel);
end;

function TInstantOTAIDENotifier3.GetModuleNotifierCount: Integer;
begin
  Result := ModuleNotifierList.Count;
end;

function TInstantOTAIDENotifier3.GetModuleNotifierList: TList;
begin
  if not Assigned(FModuleNotifierList) then
    FModuleNotifierList := TList.Create;
  Result := FModuleNotifierList;
end;

function TInstantOTAIDENotifier3.GetModuleNotifiers(
  Index: Integer): TInstantOTAModuleNotifier;
begin
  Result := TInstantOTAModuleNotifier(ModuleNotifierList[Index]);
end;

function TInstantOTAIDENotifier3.InstallModuleNotifier(
  FileName: string): TInstantOTAModuleNotifier;
var
  ModuleInterface: TIModuleInterface;
begin
  ModuleInterface := ToolServices.GetModuleInterface(FileName);
  if Assigned(ModuleInterface) then
    Result := TInstantOTAModuleNotifier.Create(Self, ModuleInterface,
      FileName)
  else
    Result := nil;
end;

procedure TInstantOTAIDENotifier3.ModuleNotification(FileName: string;
  NotifyCode: TNotifyCode);
begin
  if Assigned(FOnModuleNotification) then
    FOnModuleNotification(Self, NotifyCode, FileName);
end;

procedure TInstantOTAIDENotifier3.RemoveModuleNotifier(
  Notifier: TInstantOTAModuleNotifier);
begin
  ModuleNotifierList.Remove(Notifier);
end;

{ TInstantOTAModuleNotifier }

{$IFDEF D6+}
procedure TInstantOTAModuleNotifier.ComponentRenamed(
  const AComponent: TComponent; const OldName, NewName: string);
{$ELSE}
procedure TInstantOTAModuleNotifier.ComponentRenamed(
  ComponentHandle: Pointer; const OldName, NewName: string);
{$ENDIF}
begin
end;

constructor TInstantOTAModuleNotifier.Create(AOwner: TInstantOTAIDENotifier3;
  AModuleInterface: TIModuleInterface; AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FOwner := AOwner;
  if Assigned(FOwner) then
    FOwner.AddModuleNotifier(Self);
  FModuleInterface := AModuleInterface;
  if Assigned(FModuleInterface) then
  begin
    FModuleInterface.AddRef;
    FModuleInterface.AddNotifier(Self);
  end;
end;

destructor TInstantOTAModuleNotifier.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.RemoveModuleNotifier(Self);
  if Assigned(FModuleInterface) then
  begin
    FModuleInterface.RemoveNotifier(Self);
    FModuleInterface.Release;
  end;
  inherited;
end;

procedure TInstantOTAModuleNotifier.Notify(NotifyCode: TNotifyCode);
begin
  if Assigned(FOwner) then
    FOwner.ModuleNotification(FFileName, NotifyCode);
  case NotifyCode of
    ncModuleDeleted:
      Free;
    ncModuleRenamed: ;
    ncEditorModified: ;
    ncFormModified: ;
    ncEditorSelected: ;
    ncFormSelected: ;
    ncBeforeSave: ;
    ncAfterSave: ;
    ncFormSaving: ;
    ncProjResModified: ;
  end;
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

end.
