unit BasicView;

interface

{$IFDEF LINUX}
{$I '../../Source/InstantDefines.inc'}
{$ELSE}
{$I '..\..\Source\InstantDefines.inc'}
{$ENDIF}

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
{$ENDIF}
{$IFDEF LINUX}
  QTypes, QGraphics, QControls, QForms, QDialogs,
{$ENDIF}
  InstantPersistence;


type
  TShowStatusEvent = procedure(Sender: TObject; Text: string) of object;

  TBasicViewFormClass = class of TBasicViewForm;

  TBasicViewForm = class(TFrame)
  private
    FOnShowStatus: TShowStatusEvent;
    FCaption: TCaption;
    function GetConnectionName: string;
    function GetConnector: TInstantConnector;
    function GetIsConnected: Boolean;
  protected
    procedure ShowStatus(Text: string);
  public
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormHide(Sender: TObject); virtual;
    procedure FormShow(Sender: TObject); virtual;
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

procedure TBasicViewForm.Connect;
begin
end;

procedure TBasicViewForm.Disconnect;
begin
end;

procedure TBasicViewForm.FormCreate(Sender: TObject);
begin
  ;
end;

procedure TBasicViewForm.FormHide(Sender: TObject);
begin
  Self.Hide;
end;

procedure TBasicViewForm.FormShow(Sender: TObject);
begin
  Self.Show;
end;

function TBasicViewForm.GetConnectionName: string;
begin
  if Owner is TMainForm then
    Result := TMainForm(Owner).ConnectionName
  else
    Result := '';
end;

function TBasicViewForm.GetConnector: TInstantConnector;
begin
  if Owner is TMainForm then
    Result := TMainForm(Owner).Connector
  else
    Result := nil;
end;

function TBasicViewForm.GetIsConnected: Boolean;
begin
  if Owner is TMainForm then
    Result := TMainForm(Owner).IsConnected
  else
    Result := False;
end;

procedure TBasicViewForm.Reset;
begin
end;

procedure TBasicViewForm.ShowStatus(Text: string);
begin
  if Assigned(FOnShowStatus) then
    FOnShowStatus(Self, Text);
end;

procedure TBasicViewForm.UpdateControls;
begin
end;

end.
