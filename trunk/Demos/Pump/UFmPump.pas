unit UFmPump;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InstantPersistence, InstantIBX, DB, IBDatabase, StdCtrls,
  InstantPump;

type
  TFmPump = class(TForm)
    InstantIBXConnector1: TInstantIBXConnector;
    InstantIBXConnector2: TInstantIBXConnector;
    IBDatabase1: TIBDatabase;
    IBDatabase2: TIBDatabase;
    EmptyBeforePumpCheckBox: TCheckBox;
    Button1: TButton;
    InstantPump1: TInstantPump;
    procedure Button1Click(Sender: TObject);
    procedure EmptyBeforePumpCheckBoxClick(Sender: TObject);
  private
  public
  end;

var
  FmPump: TFmPump;

implementation

{$R *.dfm}

procedure TFmPump.Button1Click(Sender: TObject);
begin
  if MessageDlg('Begin pump?', mtConfirmation, [mbYes,mbNo], 0) = mrYes then
    InstantPump1.Pump;
end;

procedure TFmPump.EmptyBeforePumpCheckBoxClick(Sender: TObject);
begin
  if EmptyBeforePumpCheckBox.Checked then
    InstantPump1.Options := InstantPump1.Options + [poEmptyDestBeforePump]
  else
    InstantPump1.Options := InstantPump1.Options - [poEmptyDestBeforePump];
end;

end.
