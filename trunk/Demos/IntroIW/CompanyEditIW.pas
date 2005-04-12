unit CompanyEditIW;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ContactEditIW, UserSessionUnit, DB, InstantPresentation, IWCompButton,
  IWCompLabel, IWCompEdit, IWDBStdCtrls, IWVCLBaseControl, IWBaseControl,
  IWBaseHTMLControl, IWControl, IWWebGrid, IWDBAdvWebGrid, IWDBTMSCtrls,
  IWCompListbox, IWGrids, IWDBGrids;

type
  TCompanyEditForm = class(TContactEditForm)
    PersonsSelector: TInstantSelector;
    PersonsSource: TDataSource;
    IWDBLookupComboBox1: TIWDBLookupComboBox;
    ContactExposerContactPerson: TIntegerField;
    IWLabel5: TIWLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CompanyEditForm: TCompanyEditForm;

implementation

uses ServerController;

{$R *.dfm}

end.
