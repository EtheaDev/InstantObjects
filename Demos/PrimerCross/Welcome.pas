unit Welcome;

interface

{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  ShellApi,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls,
{$ENDIF}
  Classes;

type
  TWelcomeForm = class(TForm)
    BackPanel: TPanel;
    FramePanel: TPanel;
    InfoLabel1: TLabel;
    InfoLabel2: TLabel;
    InfoLabel3: TLabel;
    InfoLabel5: TLabel;
    LogoImage: TImage;
    OkButton: TButton;
    TitleLabel: TLabel;
    WebLinkLabel: TLabel;
    procedure WebLinkLabelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

{ TWelcomeForm }

procedure TWelcomeForm.WebLinkLabelClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(Application.Handle, nil, PChar(WebLinkLabel.Caption), nil, nil,
    SW_SHOWDEFAULT);
{$ENDIF}
end;

procedure TWelcomeForm.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  BorderStyle := bsNone;
  InfoLabel2.Caption := Format(InfoLabel2.Caption,['VCL','Delphi']);
{$ENDIF}
{$IFDEF LINUX}
  BorderStyle := fbsNone;
  InfoLabel2.Caption := Format(InfoLabel2.Caption,['CLX','Kylix']);
{$ENDIF}
end;

end.
