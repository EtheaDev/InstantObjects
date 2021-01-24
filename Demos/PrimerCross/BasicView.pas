unit BasicView;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  InstantPersistence;


type
  TShowStatusEvent = procedure(Sender: TObject; Text: string) of object;

  TBasicViewFrameClass = class of TBasicViewFrame;

  TBasicViewFrame = class(TFrame)
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
  Main;

{ TBasicViewForm }

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
