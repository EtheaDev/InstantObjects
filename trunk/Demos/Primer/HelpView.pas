unit HelpView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BasicView, OleCtrls, SHDocVw, StdCtrls, ExtCtrls, ComCtrls;

type
  THelpViewForm = class(TBasicViewForm)
    Browser: TWebBrowser;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure THelpViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  Browser.Navigate('file://' + ExtractFilePath(Application.ExeName) +
    'Help\index.htm');
end;

end.
