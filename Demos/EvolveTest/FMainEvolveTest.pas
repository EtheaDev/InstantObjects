unit FMainEvolveTest;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  System.Actions,
  Data.DB,
  Vcl.DBActns,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ImgList,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  InstantConnectionManager,
  InstantPersistence,
  InstantPresentation;

type
  TEvolverTestForm = class(TForm)
    connectionManager: TInstantConnectionManager;
    ActionList: TActionList;
    acntConnectionManager: TAction;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    ConnectionManager1: TMenuItem;
    MCSelector: TInstantSelector;
    MCDataSource: TDataSource;
    DBGrid1: TDBGrid;
    MCSelectorBlobAttribute: TBlobField;
    MCSelectorBooleanAttribute: TBooleanField;
    MCSelectorCurrencyAttribute: TBCDField;
    MCSelectorDateTimeAttribute: TDateTimeField;
    MCSelectorEmbeddedPartAttributeDescription: TStringField;
    MCSelectorEmbeddedPartsAtttribute: TDataSetField;
    MCSelectorEmbeddedReferencesAtttribute: TDataSetField;
    MCSelectorExternalPartAttributeDescription: TStringField;
    MCSelectorExternalPartsAttribute: TDataSetField;
    MCSelectorExternalReferencesAttributes: TDataSetField;
    MCSelectorFloatAttribute: TFloatField;
    MCSelectorGraphicAttribute: TBlobField;
    MCSelectorIntegerAttribute: TIntegerField;
    MCSelectorMemoAttribute: TMemoField;
    MCSelectorReferenceAttributeDescriptionAtttribute: TStringField;
    MCSelectorStringAttribute: TStringField;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ImageList: TImageList;
    DataSetFirst1: TDataSetFirst;
    DataSetPrior1: TDataSetPrior;
    DataSetNext1: TDataSetNext;
    DataSetLast1: TDataSetLast;
    DataSetInsert1: TDataSetInsert;
    DataSetDelete1: TDataSetDelete;
    DataSetEdit1: TDataSetEdit;
    DatasetPost1: TDataSetPost;
    DataSetCancel1: TDataSetCancel;
    DataSetRefresh1: TDataSetRefresh;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    Edit1: TMenuItem;
    Insert1: TMenuItem;
    Edit2: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Store1: TMenuItem;
    Cancel1: TMenuItem;
    Move1: TMenuItem;
    First1: TMenuItem;
    Prior1: TMenuItem;
    Next1: TMenuItem;
    Last1: TMenuItem;
    procedure acntConnectionManagerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure connectionManagerConnect(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
    procedure connectionManagerDisconnect(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
    procedure connectionManagerIsConnected(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
  private
    FConnectionDef: TInstantConnectionDef;
    FConnector: TInstantConnector;
    procedure OpenData;
    procedure CloseData;
  public
    procedure Connect;
    procedure Disconnect;
  end;

var
  EvolverTestForm: TEvolverTestForm;

implementation

{$R *.dfm}

uses
  InstantDBX,
  InstantADO,
  InstantFireDAC,
  InstantConnectionManagerFormUnit;

var
  BusySaveCursor: TCursor;
  BusyCount: Integer;
  
procedure BeginBusy;
begin
  if BusyCount = 0 then
  begin
    BusySaveCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
  end;
  Inc(BusyCount);
end;

procedure EndBusy;
begin
  if BusyCount > 0 then
  begin
    Dec(BusyCount);
    if BusyCount = 0 then
      Screen.Cursor := BusySaveCursor;
  end;
end;
  
procedure TEvolverTestForm.acntConnectionManagerExecute(Sender: TObject);
begin
  ConnectionManager.Execute;
end;

procedure TEvolverTestForm.FormCreate(Sender: TObject);
begin
  inherited;
  ConnectionManager.FileName := ChangeFileExt(Application.ExeName,'.con');
end;

procedure TEvolverTestForm.connectionManagerConnect(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Application.ProcessMessages;
  Disconnect;
  FConnector := ConnectionDef.CreateConnector(Self);
  try
    FConnector.IsDefault := True;
    FConnectionDef := ConnectionDef;
    Connect;
    Result := True;
  except
    FConnectionDef := nil;
    FreeAndNil(FConnector);
    raise;
  end;
end;

procedure TEvolverTestForm.connectionManagerDisconnect(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Disconnect;
  Result := True;
end;

procedure TEvolverTestForm.connectionManagerIsConnected(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Result := ConnectionDef = FConnectionDef;
end;

procedure TEvolverTestForm.Connect;
begin
  if not Assigned(FConnector) then
    Exit;
  BeginBusy;
  try
    FConnector.Connect;
    OpenData;
  finally
    EndBusy;
  end;
end;

procedure TEvolverTestForm.Disconnect;
begin
  BeginBusy;
  try
    if Assigned(FConnector) then
    begin
      CloseData;
      FConnector.Disconnect;
      FreeAndNil(FConnector);
    end;
    FConnectionDef := nil;
  finally
    EndBusy;
  end;
end;

procedure TEvolverTestForm.CloseData;
begin
  MCSelector.Close;
end;

procedure TEvolverTestForm.OpenData;
begin
  MCSelector.Open;
end;

end.
