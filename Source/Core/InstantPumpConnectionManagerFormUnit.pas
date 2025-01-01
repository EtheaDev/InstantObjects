unit InstantPumpConnectionManagerFormUnit;

interface

uses
  WinApi.Windows,
 Messages
  , System.SysUtils, Variants, System.Classes,
 Graphics
  , Vcl.Controls
  , Vcl.Forms,
  Dialogs
  , Data.DB
  , Vcl.StdCtrls
  , Vcl.ActnList
  , Vcl.Menus, ImgList,
  ExtCtrls, ComCtrls, System.Actions, System.ImageList
  , InstantConnectionManager
  , InstantConnectionManagerFormUnit
  , InstantPersistence
  , InstantPump
  , InstantMetadata;

type
  TInstantPumpConnectionManagerForm = class(TInstantConnectionManagerForm)
    InstantPump: TInstantPump;
    PumpButton: TButton;
    PumpAction: TAction;
    PumpDataItem: TMenuItem;
    procedure PumpButtonClick(Sender: TObject);
    procedure EmptyBeforePumpCheckBoxClick(Sender: TObject);
    procedure PumpActionExecute(Sender: TObject);
    procedure PumpActionUpdate(Sender: TObject);
    procedure InstantPumpBeforePump(Sender: TObject;
      Scheme: TInstantScheme);
    procedure InstantPumpAfterPump(Sender: TObject;
      Scheme: TInstantScheme);
  private
    FOnPump: TInstantConnectionDefEvent;
    function InternalPumpData(ConnectionDef: TInstantConnectionDef) : boolean;
  protected
    function DoPump(ConnectionDef: TInstantConnectionDef): Boolean; virtual;
  public
    property OnPump : TInstantConnectionDefEvent read FOnPump write FOnPump;
  end;

var
  InstantPumpConnectionManagerForm: TInstantPumpConnectionManagerForm;

implementation

{$R *.dfm}

procedure TInstantPumpConnectionManagerForm.PumpButtonClick(Sender: TObject);
begin
  if MessageDlg('Begin pump?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    InstantPump.Pump;//(AModel);
    ShowMessage('Pump finished.');
  end;
end;

procedure TInstantPumpConnectionManagerForm.EmptyBeforePumpCheckBoxClick(Sender: TObject);
begin
(*
  if EmptyBeforePumpCheckBox.Checked then
    InstantPump1.Options := InstantPump1.Options + [poEmptyDestBeforePump]
  else
    InstantPump1.Options := InstantPump1.Options - [poEmptyDestBeforePump];
*)
end;

procedure PumpConnectionManagerExecutor(ConnectionManager: TInstantConnectionManager);
var
  ConnectionManagerForm: TInstantConnectionManagerForm;
begin
  ConnectionManagerForm := TInstantPumpConnectionManagerForm.Create(nil);
  try
    ConnectionManagerForm.ConnectionManager := ConnectionManager;
    ConnectionManagerForm.ShowModal;
  finally
    ConnectionManagerForm.Free;
  end;
end;

function TInstantPumpConnectionManagerForm.DoPump(
  ConnectionDef: TInstantConnectionDef): Boolean;
begin
  if Assigned(FOnPump) then
  begin
    Result := False;
    FOnPump(Self, ConnectionDef, Result);
  end
  else
    Result := InternalPumpData(ConnectionDef);
end;

procedure TInstantPumpConnectionManagerForm.PumpActionExecute(
  Sender: TObject);
begin
  inherited;
  DoPump(CurrentConnectionDef);
end;

procedure TInstantPumpConnectionManagerForm.PumpActionUpdate(Sender: TObject);
begin
  inherited;
  PumpAction.Enabled := Assigned(CurrentConnectionDef) and IsManagerConnected
    and not IsConnected(CurrentConnectionDef);
end;

function TInstantPumpConnectionManagerForm.InternalPumpData(
  ConnectionDef: TInstantConnectionDef) : boolean;
var
  DestConnector : TInstantConnector;
begin
  //Source Connector is the current connected connector
  InstantPump.SourceConnector := InstantDefaultConnector;
  DestConnector := ConnectionDef.CreateConnector(nil);
  try
    DestConnector.Connect;
    InstantPump.DestConnector := DestConnector;
    InstantPump.Pump;
    Result := True;
  finally
    DestConnector.Disconnect;
    DestConnector.Free;
  end;
end;

procedure TInstantPumpConnectionManagerForm.InstantPumpBeforePump(
  Sender: TObject; Scheme: TInstantScheme);
var
  Selection : Integer;
begin
  inherited;
  Selection := MessageDlg(Format('Pump data from %s to %s: click Yes to empty destination',
    [InstantPump.SourceConnector.Name,InstantPump.DestConnector.Name]),
    mtWarning, [mbYes,mbNo,mbAbort],0 );

  if Selection = mrYes then
    InstantPump.Options := InstantPump.Options + [poEmptyDestBeforePump]
  else if Selection = mrNo then
    InstantPump.Options := InstantPump.Options - [poEmptyDestBeforePump]
  else
    Abort;
end;

procedure TInstantPumpConnectionManagerForm.InstantPumpAfterPump(
  Sender: TObject; Scheme: TInstantScheme);
begin
  inherited;
  ShowMessage('Pump completed succesfully!');
end;

initialization
  RegisterConnectionManagerExecutor(PumpConnectionManagerExecutor);

finalization
  RegisterConnectionManagerExecutor(nil);

end.
