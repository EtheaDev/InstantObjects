unit UFmPump;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InstantPersistence, InstantIBX, DB, IBDatabase, StdCtrls,
  InstantPump;

type
  TFmPump = class(TForm)
    SourceConnector: TInstantIBXConnector;
    DestConnector: TInstantIBXConnector;
    IBDatabase1: TIBDatabase;
    IBDatabase2: TIBDatabase;
    EmptyBeforePumpCheckBox: TCheckBox;
    PumpButton: TButton;
    InstantPump1: TInstantPump;
    Label1: TLabel;
    Label3: TLabel;
    procedure PumpButtonClick(Sender: TObject);
    procedure EmptyBeforePumpCheckBoxClick(Sender: TObject);
  private
  public
  end;

var
  FmPump: TFmPump;

implementation

{$R *.dfm}

procedure TFmPump.PumpButtonClick(Sender: TObject);
begin
  if MessageDlg('Begin pump?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    InstantPump1.Pump;//(AModel);
    ShowMessage('Pump finished.');
  end;
end;

procedure TFmPump.EmptyBeforePumpCheckBoxClick(Sender: TObject);
begin
  if EmptyBeforePumpCheckBox.Checked then
    InstantPump1.Options := InstantPump1.Options + [poEmptyDestBeforePump]
  else
    InstantPump1.Options := InstantPump1.Options - [poEmptyDestBeforePump];
end;

end.
