unit Welcome;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TWelcomeForm = class(TForm)
    BackPanel: TPanel;
    FramePanel: TPanel;
    LogoImage: TImage;
    TitleLabel: TLabel;
    Memo: TMemo;
    DelphiImage: TImage;
    BottomPanel: TPanel;
    OkButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure Loaded; override;
  end;

implementation

{$R *.dfm}

{ TWelcomeForm }

procedure TWelcomeForm.FormCreate(Sender: TObject);
var
  LLib, LPersonality: string;
begin
  LPersonality := 'Delphi';
  LLib := 'VCL';
  Memo.Lines.Clear;
  Memo.Lines.Add('This application demonstrates the main features of InstantObjects by providing a  sample business model implementation and an accompanying user interface.');
  Memo.Lines.Add(Format('The entire user interface was built using standard %s controls to demonstrate the level of integration with %s.', [LLib, LPersonality]));
  Memo.Lines.Add('The application can be tested with the database types of your choice by defining connections and building the databases via the Connection Manager.');
  Memo.Lines.Add('Enjoy with InstantObjects!');
end;

procedure TWelcomeForm.FormShow(Sender: TObject);
begin
  if OkButton.CanFocus then
    OkButton.SetFocus;
end;

procedure TWelcomeForm.Loaded;
begin
  TitleLabel.Caption := Application.Title;
  TitleLabel.Font.Color := clWindowText;
  TitleLabel.Font.Style := [fsBold];
  TitleLabel.Font.Height := -16;

  inherited;
end;

end.
