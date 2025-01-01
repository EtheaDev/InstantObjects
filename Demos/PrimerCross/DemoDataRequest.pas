unit DemoDataRequest;

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
  Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TDemoDataRequestForm = class(TForm)
    CountEdit: TEdit;
    AmountLabel: TLabel;
    CancelButton: TButton;
    InfoLabel: TLabel;
    OkButton: TButton;
    PicturesCheckBox: TCheckBox;
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
  BorderStyle := bsDialog;
end;

end.
