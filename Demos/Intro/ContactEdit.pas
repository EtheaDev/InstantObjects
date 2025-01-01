unit ContactEdit;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Data.DB,
  Vcl.DBCtrls,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Mask,
  InstantPresentation;

type
  TContactEditForm = class(TForm)
    ContactExposer: TInstantExposer;
    ContactExposerAddressCity: TStringField;
    ContactExposerAddressPostalCode: TStringField;
    ContactExposerAddressStreet: TStringField;
    ContactExposerName: TStringField;
    ContactExposerPhones: TDataSetField;
    Label1: TLabel;
    DBEdit1: TDBEdit;
    ContactSource: TDataSource;
    Label2: TLabel;
    DBEdit2: TDBEdit;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Label4: TLabel;
    DBEdit4: TDBEdit;
    DBGrid1: TDBGrid;
    Label5: TLabel;
    PhonesExposer: TInstantExposer;
    PhonesSource: TDataSource;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ContactEditForm: TContactEditForm;

implementation

{$R *.DFM}

procedure TContactEditForm.OkButtonClick(Sender: TObject);
begin
  ContactExposer.PostChanges;
end;

end.
