unit HelpViewK3;

interface

uses
  SysUtils, Classes, BasicView, QForms, QControls, QComCtrls, QActnList;

type
  THelpViewForm = class(TBasicViewForm)
    TextBrowser: TTextBrowser;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ActionList: TActionList;
    acIndex: TAction;
    procedure acIndexExecute(Sender: TObject);
  public
    procedure FormCreate(Sender: TObject); override;
  end;

implementation

{$R *.dfm}

procedure THelpViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := 'Overview';
  acIndex.Execute;
end;

procedure THelpViewForm.acIndexExecute(Sender: TObject);
begin
  inherited;
  TextBrowser.FileName := 'file://' + ExtractFilePath(Application.ExeName) +
    'Help/content.htm';
end;

end.
