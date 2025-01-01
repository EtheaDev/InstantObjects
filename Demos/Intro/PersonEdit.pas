unit PersonEdit;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Data.DB,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Mask,
  Vcl.DBCtrls,
  Vcl.ComCtrls,
  ContactEdit,
  InstantPresentation;

type
  TPersonEditForm = class(TContactEditForm)
    ContactExposerDateOfBirth: TDateTimeField;
    Label6: TLabel;
    DBEdit5: TDBEdit;
    DateTimePicker1: TDateTimePicker;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PersonEditForm: TPersonEditForm;

implementation

{$R *.DFM}

end.
