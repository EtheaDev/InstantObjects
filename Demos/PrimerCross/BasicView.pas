unit BasicView;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.DBGrids,
  Vcl.Grids,
  InstantPersistence;


type
  TShowStatusEvent = procedure(Sender: TObject; Text: string) of object;

  TBasicViewFrameClass = class of TBasicViewFrame;

  TBasicViewFrame = class(TFrame)
    procedure DbGridDrawColumnCellFixW11(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
  private
    FOnShowStatus: TShowStatusEvent;
    FCaption: TCaption;
    function GetConnectionName: string;
    function GetConnector: TInstantConnector;
    function GetIsConnected: Boolean;
  protected
    procedure ShowStatus(Text: string);
  public
    procedure FrameCreate(Sender: TObject); virtual;
    procedure FrameHide(Sender: TObject); virtual;
    procedure FrameShow(Sender: TObject); virtual;
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Reset; virtual;
    procedure UpdateControls; virtual;
    property ConnectionName: string read GetConnectionName;
    property Connector: TInstantConnector read GetConnector;
    property IsConnected: Boolean read GetIsConnected;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
    property Caption: TCaption read FCaption write FCaption;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Themes,
  Main;

{ TBasicViewFrame }

procedure TBasicViewFrame.DbGridDrawColumnCellFixW11(Sender: TObject;
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

procedure TBasicViewFrame.Connect;
begin
end;

procedure TBasicViewFrame.Disconnect;
begin
end;

procedure TBasicViewFrame.FrameCreate(Sender: TObject);
begin
end;

procedure TBasicViewFrame.FrameHide(Sender: TObject);
begin
  Self.Hide;
end;

procedure TBasicViewFrame.FrameShow(Sender: TObject);
begin
  Self.Show;
end;

function TBasicViewFrame.GetConnectionName: string;
begin
  if Owner is TMainForm then
    Result := TMainForm(Owner).ConnectionName
  else
    Result := '';
end;

function TBasicViewFrame.GetConnector: TInstantConnector;
begin
  if Owner is TMainForm then
    Result := TMainForm(Owner).Connector
  else
    Result := nil;
end;

function TBasicViewFrame.GetIsConnected: Boolean;
begin
  if Owner is TMainForm then
    Result := TMainForm(Owner).IsConnected
  else
    Result := False;
end;

procedure TBasicViewFrame.Reset;
begin
end;

procedure TBasicViewFrame.ShowStatus(Text: string);
begin
  if Assigned(FOnShowStatus) then
    FOnShowStatus(Self, Text);
end;

procedure TBasicViewFrame.UpdateControls;
begin
end;

end.
