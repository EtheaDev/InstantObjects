unit Welcome;

interface

{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

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
  end;

implementation

{$R *.DFM}

uses
  ShellApi;

{ TWelcomeForm }

procedure TWelcomeForm.WebLinkLabelClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, nil, PChar(WebLinkLabel.Caption), nil, nil,
    SW_SHOWDEFAULT);
end;

end.
