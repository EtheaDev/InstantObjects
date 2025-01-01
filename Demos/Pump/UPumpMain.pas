unit UPumpMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  InstantConnectionManager,
  InstantClasses,
  InstantPersistence;

type
  TPumpDemoMain = class(TForm)
    ConnectionManager: TInstantConnectionManager;
    PumpManagerButton: TButton;
    procedure PumpManagerButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ConnectionManagerConnect(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
    procedure ConnectionManagerDisconnect(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
    procedure ConnectionManagerIsConnected(Sender: TObject;
      var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
  private
    FConnectionDef: TInstantConnectionDef;
    FConnector: TInstantConnector;
    procedure Connect;
    procedure Disconnect;
  public
    { Public declarations }
  end;

var
  PumpDemoMain: TPumpDemoMain;

implementation

{$R *.dfm}

uses
  Utility { Note: This demo attempts to include brokers for the data access
  layers supported natively by Delphi. To include additional brokers,
  please add the broker unit(s) to the following list. If you have not
  installed all brokers, please remove the missing broker unit(s) from
  the list. },
  InstantDBX,
  InstantADO,
  InstantXML,
  InstantFireDAC;

procedure TPumpDemoMain.PumpManagerButtonClick(Sender: TObject);
begin
  ConnectionManager.Execute;
end;

procedure TPumpDemoMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;

// To use XML format for ConnectionManager file:
  ConnectionManager.FileFormat := sfXML;
  ConnectionManager.FileName := ChangeFileExt(Application.ExeName, '.xml');

// To use binary format for ConnectionManager file:
//  ConnectionManager.FileFormat := sfBinary;
//  ConnectionManager.FileName := ChangeFileExt(Application.ExeName, '.con');
end;

procedure TPumpDemoMain.ConnectionManagerConnect(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Application.ProcessMessages;
  Disconnect;
  FConnector := ConnectionDef.CreateConnector(Self);
  try
    FConnector.IsDefault := True;
    FConnectionDef := ConnectionDef;
    Connect;
    Result := False;
  except
    FConnectionDef := nil;
    FreeAndNil(FConnector);
    raise;
  end;
end;

procedure TPumpDemoMain.Connect;
begin
  if not Assigned(FConnector) then
    Exit;
  BeginBusy;
  try
    FConnector.Connect;
  finally
    EndBusy;
  end;
end;

procedure TPumpDemoMain.Disconnect;
begin
  BeginBusy;
  try
    if Assigned(FConnector) then
    begin
      FConnector.Disconnect;
      FreeAndNil(FConnector);
    end;
    FConnectionDef := nil;
  finally
    EndBusy;
  end;
end;

procedure TPumpDemoMain.ConnectionManagerDisconnect(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Disconnect;
  Result := True;
end;

procedure TPumpDemoMain.ConnectionManagerIsConnected(Sender: TObject;
  var ConnectionDef: TInstantConnectionDef; var Result: Boolean);
begin
  Result := ConnectionDef = FConnectionDef;
end;

end.
