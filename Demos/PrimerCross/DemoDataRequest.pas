unit DemoDataRequest;

interface

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QComCtrls,
{$ENDIF}
  Classes;

type
  TDemoDataRequestForm = class(TForm)
    CountEdit: TEdit;
    AmountLabel: TLabel;
    CancelButton: TButton;
    InfoLabel: TLabel;
    OkButton: TButton;
    procedure CountEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
  public
    property Count: Integer read GetCount write SetCount;
  end;

implementation

{$R *.dfm}

{ TDemoDataRequestForm }

procedure TDemoDataRequestForm.CountEditChange(Sender: TObject);
begin
  OkButton.Enabled := Count > 0;
end;

function TDemoDataRequestForm.GetCount: Integer;
begin
  Result := StrToIntDef(CountEdit.Text, 0);
end;

procedure TDemoDataRequestForm.SetCount(const Value: Integer);
begin
  CountEdit.Text := IntToStr(Count);
end;

procedure TDemoDataRequestForm.FormCreate(Sender: TObject);
begin
  inherited;
{$IFDEF MSWINDOWS}
  BorderStyle := bsDialog;
{$ENDIF}
{$IFDEF LINUX}
  BorderStyle := fbsDialog;
{$ENDIF}
end;

end.
