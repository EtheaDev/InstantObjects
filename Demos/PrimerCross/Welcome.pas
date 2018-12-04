unit Welcome;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  SysUtils,
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ShellApi,
  Classes;

type
  TWelcomeForm = class(TForm)
    BackPanel: TPanel;
    FramePanel: TPanel;
    LogoImage: TImage;
    OkButton: TButton;
    TitleLabel: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

{ TWelcomeForm }

procedure TWelcomeForm.FormCreate(Sender: TObject);
var
  LLib, LPersonality: string;
begin
  Font.Assign(Screen.IconFont);
  TitleLabel.Caption := Application.Title;
  Font.Assign(Screen.IconFont);
  TitleLabel.Font.Color := clWindowText;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Font.Height := -16;
  BorderStyle := bsNone;
  LPersonality := 'Delphi';
  LLib := 'VCL';
  Memo1.Lines.Clear;
  Memo1.Lines.Add('This application demonstrates the main features of InstantObjects by providing a  sample business model implementation and an accompanying user interface.');
  Memo1.Lines.Add(Format('The entire user interface was built using standard %s controls to demonstrate the level of integration with %s.', [LLib, LPersonality]));
  Memo1.Lines.Add('The application can be tested with the database types of your choice by defining connections and building the databases via the Connection Manager.');
  Memo1.Lines.Add('Enjoy with InstantObjects!');
end;

end.
