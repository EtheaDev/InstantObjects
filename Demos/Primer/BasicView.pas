unit BasicView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  InstantPersistence;

type
  TShowStatusEvent = procedure(Sender: TObject; Text: string) of object;

  TBasicViewFormClass = class of TBasicViewForm;

  TBasicViewForm = class(TForm)
  private
    FOnShowStatus: TShowStatusEvent;
    function GetConnectionName: string;
    function GetConnector: TInstantConnector;
    function GetIsConnected: Boolean;
  protected
    procedure ShowStatus(Text: string);
  public
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Reset; virtual;
    procedure UpdateControls; virtual;
    property ConnectionName: string read GetConnectionName;
    property Connector: TInstantConnector read GetConnector;
    property IsConnected: Boolean read GetIsConnected;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

implementation

{$R *.DFM}

uses
  Main;

{ TBasicViewForm }

procedure TBasicViewForm.Connect;
begin
end;

procedure TBasicViewForm.Disconnect;
begin
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
