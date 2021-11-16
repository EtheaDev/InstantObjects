unit PersonEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ContactEdit, Db, InstantPresentation, StdCtrls, Grids, DBGrids, Mask,
  DBCtrls, Vcl.ComCtrls;

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
