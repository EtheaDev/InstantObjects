unit CompanyEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ContactEdit, Db, InstantPresentation, StdCtrls, Grids, DBGrids, Mask,
  DBCtrls;

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
