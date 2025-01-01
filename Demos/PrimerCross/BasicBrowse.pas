unit BasicBrowse;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Data.DB,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ExtCtrls,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  Vcl.ImgList,
  Vcl.Menus,
  Vcl.ActnList,
  InstantPresentation;

const
  VK_ESCAPE_KEY = VK_ESCAPE;
  VK_RETURN_KEY = VK_RETURN;

type
  TBasicBrowseForm = class(TForm)
    ActionImages: TImageList;
    ActionList: TActionList;
    BrowseGrid: TDBGrid;
    BrowseGridPanel: TPanel;
    BrowseSource: TDataSource;
    DeleteAction: TAction;
    DeleteButton: TToolButton;
    DeleteItem: TMenuItem;
    EditAction: TAction;
    EditButton: TToolButton;
    EditItem: TMenuItem;
    GridMenu: TPopupMenu;
    NewAction: TAction;
    NewButton: TToolButton;
    NewItem: TMenuItem;
    SearchEdit: TEdit;
    SelectAction: TAction;
    SelectButton: TToolButton;
    SelectItem: TMenuItem;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    ToolSep1: TToolButton;
    ToolSep2: TToolButton;
    SearchButton: TToolButton;
    SearchAction: TAction;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure NewActionExecute(Sender: TObject);
    procedure EditActionExecute(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure BrowseGridDblClick(Sender: TObject);
    procedure SelectActionExecute(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure ToolBarResize(Sender: TObject);
    procedure SearchActionExecute(Sender: TObject);
    procedure BrowseGridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DbGridDrawColumnCellFixW11(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
  private
    FLookupMode: Boolean;
    function GetExposer: TInstantCustomExposer;
    function GetSelected: TObject;
    procedure SetLookupMode(Value: Boolean);
    procedure SetSelected(Value: TObject);
  protected
    function ConfirmDelete: Boolean; virtual;
    function CreateObject: TObject; virtual;
    function Find(const Text: string): Boolean; virtual;
    procedure Search;
    procedure Select; virtual;
    procedure UpdateControls; virtual;
  public
    function Execute: Boolean;
    property Exposer: TInstantCustomExposer read GetExposer;
    property LookupMode: Boolean read FLookupMode write SetLookupMode;
    property Selected: TObject read GetSelected write SetSelected;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Themes,
  BasicEdit,
  Utility,
  InstantPersistence,
  InstantImageUtils;

{ TBasicBrowseForm }

procedure TBasicBrowseForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  HasItem: Boolean;
begin
  HasItem := Assigned(Selected);
  NewAction.Enabled := not LookupMode;
  EditAction.Enabled := HasItem;
  DeleteAction.Enabled := HasItem;
  SelectAction.Enabled := HasItem;
  SearchAction.Enabled := SearchEdit.Text <> '';
end;

procedure TBasicBrowseForm.BrowseGridDblClick(Sender: TObject);
begin
  with SelectAction do
    if Visible then
      Execute;
end;

procedure TBasicBrowseForm.DbGridDrawColumnCellFixW11(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  LDbGrid: TDbGrid;
begin
  LDbGrid := Sender as TDbGrid;
  //Resolve bad painting of selected cell in Windows 11
  if not StyleServices.Enabled or (StyleServices.IsSystemStyle) then
  begin
    if ((gdSelected in State) and (gdFocused in State))
      or ((gdSelected in State) and (dgRowSelect in LDbGrid.Options) and LDbGrid.Focused)
      then
      LDbGrid.Canvas.Brush.Color := clHighlight;
    LDbGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

function TBasicBrowseForm.ConfirmDelete: Boolean;
var
  S: string;
begin
  if Selected is TInstantObject then
    S := '"' + TInstantObject(Selected).Caption + '"'
  else
    S := 'object';
  Result := Confirm(Format('Delete %s?', [S]));
end;

function TBasicBrowseForm.CreateObject: TObject;
begin
  Result := Exposer.CreateObject;
end;

procedure TBasicBrowseForm.DeleteActionExecute(Sender: TObject);
begin
  if ConfirmDelete then
    Exposer.Delete;
end;

procedure TBasicBrowseForm.EditActionExecute(Sender: TObject);
begin
  if dgEditing in BrowseGrid.Options then
    Exposer.Edit
  else
    EditObject(Selected);
end;

function TBasicBrowseForm.Execute: Boolean;
begin
  if Exposer.Active then
    Exposer.First;
  Result := ShowModal = mrOk;
end;

function TBasicBrowseForm.Find(const Text: string): Boolean;
begin
  Result := False;
end;

procedure TBasicBrowseForm.FormCreate(Sender: TObject);
begin
  LoadMultipleImages(ActionImages,'BROWSEACTIONIMAGES',HInstance);
  BorderStyle := bsSingle;
  SelectItem.Default := True;
  UpdateControls;
end;

procedure TBasicBrowseForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if not Visible then
    Exit;
  case Ord(Key) of
    27:
      if LookupMode then
        Close;
    13:
      if LookupMode then
      begin
        if SearchEdit.Focused then
          Search
        else
          Select;
      end;
  else
    Exit;
  end;
  Key := #0;
end;

function TBasicBrowseForm.GetExposer: TInstantCustomExposer;
begin
  Result := BrowseSource.DataSet as TInstantCustomExposer;
end;

function TBasicBrowseForm.GetSelected: TObject;
begin
  Result := Exposer.CurrentObject;
end;

procedure TBasicBrowseForm.NewActionExecute(Sender: TObject);
var
  NewObject: TObject;
begin
  if dgEditing in BrowseGrid.Options then
    Exposer.Insert
  else begin
    NewObject := CreateObject;
    try
      if EditObject(NewObject) and Exposer.Active then
        Exposer.AddObject(NewObject);
    finally
      NewObject.Free;
    end;
  end;
end;

procedure TBasicBrowseForm.Search;
begin
  BeginBusy;
  try
    if Find(SearchEdit.Text) then
      ActiveControl := BrowseGrid
    else
      ActiveControl := SearchEdit;
  finally
    EndBusy;
  end;
end;

procedure TBasicBrowseForm.Select;
begin
  if Assigned(Selected) then
    ModalResult := mrOk;
end;

procedure TBasicBrowseForm.SelectActionExecute(Sender: TObject);
begin
  Select;
end;

procedure TBasicBrowseForm.SetLookupMode(Value: Boolean);
begin
  if Value <> LookupMode then
  begin
    FLookupMode := Value;
    UpdateControls;
  end;
end;

procedure TBasicBrowseForm.SetSelected(Value: TObject);
begin
  Exposer.GotoObject(Value);
end;

procedure TBasicBrowseForm.UpdateControls;
begin
  SearchEdit.Visible := LookupMode;
  ToolSep1.Visible := LookupMode;
  SelectAction.Visible := LookupMode;
  SearchAction.Visible := LookupMode;
  with BrowseGrid do
    if LookupMode then
      Options := Options + [dgRowSelect]
    else
      Options := Options - [dgRowSelect];
  ToolBar.Repaint;
end;

procedure TBasicBrowseForm.ToolBarResize(Sender: TObject);
begin
  SearchEdit.Width := ToolBar.Width - 140;
end;

procedure TBasicBrowseForm.SearchActionExecute(Sender: TObject);
begin
  Search;
end;

procedure TBasicBrowseForm.BrowseGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F5) and (Shift = []) then
    (Sender as TDBGrid).DataSource.DataSet.Refresh;
end;

end.
