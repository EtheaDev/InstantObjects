unit HelpView;

interface

{$IFDEF LINUX}
{$I '../../Source/InstantDefines.inc'}
{$ELSE}
{$I '..\..\Source\InstantDefines.inc'}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BasicView, OleCtrls, SHDocVw, StdCtrls, ExtCtrls, ComCtrls;

type
  THelpViewForm = class(TBasicViewForm)
    Browser: TWebBrowser;
  public
    procedure FormCreate(Sender: TObject); override;
  end;

implementation

{$R *.dfm}

procedure THelpViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := 'Overview';
  
  Browser.Navigate('file://' + ExtractFilePath(Application.ExeName) +
    'Help\index.htm');
end;

end.
