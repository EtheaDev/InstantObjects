unit HelpView;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BasicView, OleCtrls, SHDocVw, StdCtrls, ExtCtrls, ComCtrls;

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
