unit DemoDataRequest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TDemoDataRequestForm = class(TForm)
    CountEdit: TEdit;
    AmountLabel: TLabel;
    CancelButton: TButton;
    InfoLabel: TLabel;
    OkButton: TButton;
    procedure CountEditChange(Sender: TObject);
  private
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
  public
    property Count: Integer read GetCount write SetCount;
  end;

implementation

{$R *.DFM}

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

end.
