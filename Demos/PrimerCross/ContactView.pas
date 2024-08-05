unit ContactView;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  SysUtils, Classes, DB,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Grids, DBGrids, ExtCtrls, Buttons, ShellApi,
  StdCtrls, ActnList, ToolWin, ImgList, Menus,
  System.Actions,
  BasicView, Model, InstantPresentation, InstantExplorer, System.ImageList;

type
  TContactViewForm = class(TBasicViewFrame)
    ActionList: TActionList;
    ActionImages: TImageList;
    ClientPanel: TPanel;
    ContactGrid: TDBGrid;
    ContactGridMenu: TPopupMenu;
    ContactGridPanel: TPanel;
    ContactSelector: TInstantSelector;
    ContactSource: TDataSource;
    BrowseCountryAction: TAction;
    CountriesButton: TToolButton;
    DeleteAction: TAction;
    DeleteButton: TToolButton;
    DeleteItem: TMenuItem;
    EditAction: TAction;
    EditButton: TToolButton;
    EditItem: TMenuItem;
    ExplorerAction: TAction;
    ExplorerButton: TToolButton;
    ExportAction: TAction;
    ExportButton: TToolButton;
    ExportItem: TMenuItem;
    FilterAction: TAction;
    FilterButton: TToolButton;
    FindAction: TAction;
    FindButton: TBitBtn;
    FindEdit: TComboBox;
    FindLabel: TLabel;
    FindPanel: TPanel;
    GridPanel: TPanel;
    HeaderBevel: TBevel;
    HeaderPanel: TPanel;
    IndexTabControl: TTabControl;
    MenuSep1: TMenuItem;
    MenuSep2: TMenuItem;
    NewCompanyAction: TAction;
    NewCompanyButton: TToolButton;
    NewCompanyItem: TMenuItem;
    NewPersonAction: TAction;
    NewPersonButton: TToolButton;
    NewPersonItem: TMenuItem;
    SortAction: TAction;
    SortButton: TToolButton;
    ToolBar: TToolBar;
    ToolSep1: TToolButton;
    ToolSep3: TToolButton;
    ToolSep4: TToolButton;
    ToolSep2: TToolButton;
    ExplorerSplitter: TSplitter;
    ExplorerPanel: TPanel;
    ExplorerImages: TImageList;
    Explorer: TInstantExplorer;
    ToolSep5: TToolButton;
    CategoriesButton: TToolButton;
    ToolButton2: TToolButton;
    BrowseCategoryAction: TAction;
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure BrowseCountryActionExecute(Sender: TObject);
    procedure ContactGridDblClick(Sender: TObject);
    procedure ContactGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure ContactGridPanelResize(Sender: TObject);
    procedure ContactSelectorAfterScroll(DataSet: TDataSet);
    procedure ContactSelectorFilterRecord(DataSet: TDataSet;
      var Accept: Boolean);
    procedure ContactSelectorLimit(Sender, AObject: TObject;
      var Accept: Boolean);
    procedure DeleteActionExecute(Sender: TObject);
    procedure EditActionExecute(Sender: TObject);
    procedure ExplorerActionExecute(Sender: TObject);
    procedure ExportActionExecute(Sender: TObject);
    procedure FilterActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure FindEditClick(Sender: TObject);
    procedure IndexTabControlChange(Sender: TObject);
    procedure NewCompanyActionExecute(Sender: TObject);
    procedure NewPersonActionExecute(Sender: TObject);
    procedure ExplorerGetImageIndex(Sender: TInstantExplorer;
      Node: TTreeNode);
    procedure ExplorerGetNodeText(Sender: TInstantExplorer;
      NodeData: TInstantExplorerNodeData; var Text: String);
    procedure ContactGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SortActionExecute(Sender: TObject);
    procedure ContactSelectorCompare(Sender, AObject1, AObject2: TObject;
      var Compare: Integer);
    procedure BrowseCategoryActionExecute(Sender: TObject);
    procedure ContactSelectorBeforeClose(DataSet: TDataSet);
  private
    FContactFilter: TContactFilter;
    FSortOrder: string;
    function GetContactFilter: TContactFilter;
    function GetExplorerVisible: Boolean;
    function GetIsFiltered: Boolean;
    procedure SetExplorerVisible(const Value: Boolean);
    procedure SetIsFiltered(const Value: Boolean);
  protected
    procedure AdjustContactGridColumns;
    procedure PerformFindQuery;
    procedure PerformQuery(const Where: string);
    procedure PerformTabQuery;
    procedure UpdateExplorer;
    property ContactFilter: TContactFilter read GetContactFilter;
    property ExplorerVisible: Boolean read GetExplorerVisible write SetExplorerVisible;
    property IsFiltered: Boolean read GetIsFiltered write SetIsFiltered;
  public
    procedure FrameCreate(Sender: TObject); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Reset; override;
    procedure FrameHide(Sender: TObject); override;
    procedure FrameShow(Sender: TObject); override;
  end;

implementation

uses
  Utility, BasicEdit, InstantClasses, InstantPersistence,
  CountryBrowse, MainData, ContactSort, InstantUtils, CategoryBrowse,
  InstantImageUtils;

{$R *.dfm}

{ TContactViewForm }

procedure TContactViewForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  Contact: TContact;
begin
  Contact := ContactSelector.CurrentObject as TContact;
  NewCompanyAction.Enabled := IsConnected and Visible;
  NewPersonAction.Enabled := IsConnected and Visible;
  DeleteAction.Enabled := Assigned(Contact) and Visible;
  EditAction.Enabled := Assigned(Contact) and Visible;
  ExportAction.Enabled := Assigned(Contact) and Visible;
  FilterAction.Enabled := IsConnected and Visible;
  SortAction.Enabled := IsConnected and Visible;
  BrowseCountryAction.Enabled := IsConnected and Visible;
  BrowseCategoryAction.Enabled := IsConnected and Visible;
  FindAction.Enabled := (FindEdit.Text <> '') and Visible;
  IndexTabControl.Enabled := IsConnected  and Visible;

  FindButton.Default := FindEdit.Focused;
  FindPanel.Enabled := IsConnected;
end;

procedure TContactViewForm.AdjustContactGridColumns;
var
  I, Gap, Adjust: Integer;
begin
  with ContactGrid do
  begin
    Gap := Width - GetSystemMetrics(SM_CXVSCROLL) - 23;
    for I := 1 to Pred(Columns.Count) do
      Dec(Gap, Columns[I].Width + 1);
    for I := Pred(Columns.Count) downto 1 do
    begin
      Adjust := Gap div I;
      Columns[I].Width := Columns[I].Width + Adjust;
      Dec(Gap, Adjust);
    end;
  end;
end;

procedure TContactViewForm.BrowseCategoryActionExecute(Sender: TObject);
begin
  with TCategoryBrowseForm.Create(nil) do
  try
    Execute;
  finally
    Free;
  end;
end;

procedure TContactViewForm.BrowseCountryActionExecute(Sender: TObject);
begin
  with TCountryBrowseForm.Create(nil) do
  try
    Execute;
  finally
    Free;
  end;
end;

procedure TContactViewForm.Connect;
begin
  inherited;
  PerformTabQuery;
end;

procedure TContactViewForm.ContactGridDblClick(Sender: TObject);
begin
  EditAction.Execute;
end;

procedure TContactViewForm.ContactGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
var
  Contact: TContact;
  Index: Integer;
begin
  inherited DbGridDrawColumnCellFixW11(Sender, Rect, DataCol, Column, State);

  if gdSelected in State then
    with ContactGrid do
      if not Focused then
      begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.Font.Color := clBlack;
      end;
  ContactGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  if Column.FieldName = '' then
  begin
    Contact := ContactSelector.CurrentObject as TContact;
    if Contact is TPerson then
      Index := 0
    else if Contact is TCompany then
      Index := 1
    else
      Index := -1;
    with ContactGrid do
      ActionImages.Draw(Canvas, Rect.Left + 1, Rect.Top + 1, Index);
  end;
end;

procedure TContactViewForm.ContactGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Chr(Key) = 'F') then
    FindEdit.SetFocus
  else if (Key = VK_F5) and (Shift = []) then
    ContactSelector.RefreshData;
end;

procedure TContactViewForm.ContactGridPanelResize(Sender: TObject);
begin
  AdjustContactGridColumns;
end;

procedure TContactViewForm.ContactSelectorAfterScroll(DataSet: TDataSet);
begin
  UpdateExplorer;
  with ContactSelector do
    ShowStatus(Format('%d/%d', [RecNo, RecordCount]));
end;

procedure TContactViewForm.ContactSelectorBeforeClose(DataSet: TDataSet);
begin
  Explorer.Clear;
end;

procedure TContactViewForm.ContactSelectorCompare(Sender, AObject1,
  AObject2: TObject; var Compare: Integer);
begin
  Compare := InstantCompareObjects(AObject1, AObject2, FSortOrder,
    [coCaseInsensitive]);
end;

procedure TContactViewForm.ContactSelectorFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept := ContactFilter.Matches(ContactSelector.CurrentObject as TContact);
end;

procedure TContactViewForm.ContactSelectorLimit(Sender, AObject: TObject;
  var Accept: Boolean);
begin
  Accept := ContactFilter.Matches(AObject as TContact);
end;

procedure TContactViewForm.DeleteActionExecute(Sender: TObject);
var
  Contact: TContact;
begin
  Contact := ContactSelector.CurrentObject as TContact;
  if Assigned(Contact) and Confirm(Format('Delete "%s"?', [Contact.Name])) then
    ContactSelector.Delete;
end;

destructor TContactViewForm.Destroy;
begin
  inherited;
  FContactFilter.Free;
end;

procedure TContactViewForm.Disconnect;
begin
  Explorer.Clear;
  ContactSelector.Close;
  UpdateExplorer;
  inherited;
end;

procedure TContactViewForm.EditActionExecute(Sender: TObject);
begin
  if EditObject(ContactSelector.CurrentObject) then
    ContactSelector.RefreshData;
end;

procedure TContactViewForm.ExplorerActionExecute(Sender: TObject);
begin
  ExplorerVisible := not ExplorerVisible;
end;

procedure TContactViewForm.ExplorerGetImageIndex(Sender: TInstantExplorer;
  Node: TTreeNode);
var
  NodeData: TInstantExplorerNodeData;
begin
  NodeData := Node.Data;
  if NodeData.Instance is TCompany then
    Node.ImageIndex := 3
  else if NodeData.Instance is TPerson then
    Node.ImageIndex := 4
  else if NodeData.Instance is TPhone then
    Node.ImageIndex := 5
  else if NodeData.Instance is TEmail then
    Node.ImageIndex := 6
  else if NodeData.Instance is TAddress then
    Node.ImageIndex := 7
  else
    Exit;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TContactViewForm.ExplorerGetNodeText(Sender: TInstantExplorer;
  NodeData: TInstantExplorerNodeData; var Text: String);
begin
  with NodeData do
  begin
    if Instance is TInstantObject then
    begin
      if Name = '' then
        Text := TInstantObject(Instance).Caption
      else
        Text := Name;
    end else if Instance is TInstantContainer then
      Text := Name;
  end;
end;

procedure TContactViewForm.ExportActionExecute(Sender: TObject);
var
  FileName: string;
  Stream: TFileStream;
  Contact: TContact;
begin
  FileName := ExtractFilePath(Application.ExeName) + 'Export.xml';
  Contact := ContactSelector.CurrentObject as TContact;
  if not Assigned(Contact) then
    Exit;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    InstantWriteObject(Stream, sfXML, Contact);
  finally
    Stream.Free;
  end;
  ShellExecute(Application.Handle, nil, PChar(FileName), nil, nil, SW_SHOW);
end;

procedure TContactViewForm.FilterActionExecute(Sender: TObject);
begin
  IsFiltered := not IsFiltered and EditObject(ContactFilter);
  FilterButton.Down := IsFiltered;
end;

procedure TContactViewForm.FindActionExecute(Sender: TObject);
begin
  PerformFindQuery;
end;

procedure TContactViewForm.FindEditClick(Sender: TObject);
begin
  PerformFindQuery;
end;

procedure TContactViewForm.FrameCreate(Sender: TObject);
begin
  Caption := 'Contacts';
  ContactGrid.OnDrawColumnCell := ContactGridDrawColumnCell;

  LoadMultipleImages(ActionImages,'CONTACTACTIONIMAGES',HInstance);
  LoadMultipleImages(ExplorerImages,'EXPLORERCONTACTIMAGES',HInstance);
  FindButton.Action := FindAction; //to get glyph image

  FSortOrder := 'Name';
  ExplorerPanel.Width := (Sender as TFrame).Width div 3;
  ExplorerVisible := True;

  with IndexTabControl.Tabs do
  begin
    Add('abc');
    Add('def');
    Add('ghi');
    Add('jkl');
    Add('mno');
    Add('pqr');
    Add('stu');
    Add('vw');
    Add('xyz');
    Add('all');
    Add('?');
  end;
end;

procedure TContactViewForm.FrameHide(Sender: TObject);
begin
  inherited;
  ActionList.State := asSuspended;
end;

procedure TContactViewForm.FrameShow(Sender: TObject);
begin
  inherited;
  ActionList.State := asNormal;
  if ExplorerVisible then
    Explorer.Refresh;
end;

function TContactViewForm.GetContactFilter: TContactFilter;
begin
  if not Assigned(FContactFilter) then
  begin
    FContactFilter := TContactFilter.Create;
    FContactFilter.IsDynamic := True;
  end;
  Result := FContactFilter;
end;

function TContactViewForm.GetExplorerVisible: Boolean;
begin
  Result := ExplorerPanel.Visible;
end;

function TContactViewForm.GetIsFiltered: Boolean;
begin
  if ContactFilter.IsDynamic then
    Result := ContactSelector.Filtered
  else
    Result := ContactSelector.Limited;
end;

procedure TContactViewForm.IndexTabControlChange(Sender: TObject);
begin
  PerformTabQuery;
end;

procedure TContactViewForm.NewCompanyActionExecute(Sender: TObject);
var
  Company: TCompany;
begin
  Company := TCompany.Create;
  try
    if EditObject(Company) then
      ContactSelector.AddObject(Company);
  finally
    Company.Free;
  end;
end;

procedure TContactViewForm.NewPersonActionExecute(Sender: TObject);
var
  Person: TPerson;
begin
  Person := TPerson.Create;
  try
    if EditObject(Person) then
      ContactSelector.AddObject(Person);
  finally
    Person.Free;
  end;
end;

procedure TContactViewForm.PerformFindQuery;
begin
  PerformQuery(Format('Name LIKE "%%%s%%"', [FindEdit.Text]));
  if ContactSelector.ObjectCount > 0 then
  begin
    with FindEdit do
      if Items.IndexOf(Text) = -1 then
        Items.Add(Text);
    if ContactGrid.CanFocus then
      ContactGrid.SetFocus;
  end else if FindEdit.CanFocus then
    FindEdit.SetFocus;
  with IndexTabControl do
    TabIndex := Pred(Tabs.Count);
end;

procedure TContactViewForm.PerformQuery(const Where: string);
var
  S: string;
begin
  BeginBusy;
  try
    Explorer.Clear;
    with ContactSelector do
    begin
      Close;
      S := 'SELECT * FROM ANY ' + TContact.ClassName;
      if Where <> '' then
        S := S + ' WHERE ' + Where;
      if not Sorted then
        S := S + ' ORDER BY ' + FSortOrder;
      Command.Text := S;
      Open;
    end;
  finally
    EndBusy;
  end;
end;

procedure TContactViewForm.PerformTabQuery;

  function WhereClause: string;
  var
    I: Integer;
    OrStr: string;
    Letters: string;
  begin
    Result := '';
    with IndexTabControl do
    begin
      Letters := Tabs[TabIndex];
      if Letters = 'all' then
        Exit;
    end;
    OrStr := '';
    for I := 1 to Length(Letters) do
    begin
      Result := Result + OrStr + Format('(Name LIKE "%s%%")',
        [UpperCase(Letters[I])]);
      OrStr := ' OR ';
    end;
  end;

begin
  with IndexTabControl do
    if TabIndex = Pred(Tabs.Count) then
      PerformFindQuery
    else
      PerformQuery(WhereClause);
end;

procedure TContactViewForm.Reset;
begin
  inherited;
  ContactSelector.Reset;
end;

procedure TContactViewForm.SetExplorerVisible(const Value: Boolean);
begin
  if Value then
  begin
    ExplorerPanel.Width := Width div 3;
    ExplorerPanel.Visible := Value;
    ExplorerSplitter.Visible := Value;
    ExplorerSplitter.Left := ExplorerPanel.Left -1;
    ExplorerButton.Down := Value;
    UpdateExplorer;
  end
  else
  begin
    ExplorerPanel.Visible := False;
    ExplorerSplitter.Visible := False;
    ExplorerButton.Down := False;
  end;
end;

procedure TContactViewForm.SetIsFiltered(const Value: Boolean);
begin
  BeginBusy;
  try
    with ContactSelector do
    begin
      if ContactFilter.IsDynamic then
      begin
        Filtered := Value;
        Limited := False;
      end else
      begin
        Filtered := False;
        Limited := Value;
      end;
    end;
  finally
    EndBusy;
  end;
end;

procedure TContactViewForm.SortActionExecute(Sender: TObject);
begin
  with TContactSortForm.Create(nil) do
  try
    ByAttribute := not ContactSelector.Sorted;
    OrderName := FSortOrder;
    if ShowModal = mrOk then
    begin
      Self.Update;
      BeginBusy;
      try
        FSortOrder := OrderName;
        if ByAttribute then
        begin
          ContactSelector.Close;
          ContactSelector.Sorted := False;
          PerformTabQuery
        end else
        begin
          ContactSelector.Sorted := False;
          ContactSelector.Sorted := True;
        end;
      finally
        EndBusy;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TContactViewForm.UpdateExplorer;
begin
  if ExplorerVisible and ContactSelector.Active then
    Explorer.RootObject := ContactSelector.CurrentObject as TContact
  else if Assigned(Explorer.RootObject) then
    Explorer.Clear;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
