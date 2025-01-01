unit CompanyEdit;

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
  ContactEdit,
  InstantPresentation;

type
  TCompanyEditForm = class(TContactEditForm)
    ContactExposerContactPerson: TIntegerField;
    DBLookupComboBox1: TDBLookupComboBox;
    Label6: TLabel;
    PersonsSelector: TInstantSelector;
    PersonsSource: TDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CompanyEditForm: TCompanyEditForm;

implementation

{$R *.DFM}

end.
