unit HelpView;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.OleCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  BasicView,
  SHDocVw;

type
  THelpViewForm = class(TBasicViewFrame)
    Browser: TWebBrowser;
  public
    procedure FrameCreate(Sender: TObject); override;
  end;

implementation

{$R *.dfm}

procedure THelpViewForm.FrameCreate(Sender: TObject);
begin
  inherited;
  Caption := 'Overview';
  
  Browser.Navigate('file://' + ExtractFilePath(Application.ExeName) +
    'Help\index.htm');
end;

end.
