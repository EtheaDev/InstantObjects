unit PersonEditIW;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ContactEditIW, UserSessionUnit, DB, InstantPresentation, IWCompButton,
  IWCompLabel, IWCompEdit, IWDBStdCtrls, IWVCLBaseControl, IWBaseControl,
  IWBaseHTMLControl, IWControl, IWGrids, IWDBGrids;

type                                                                       
  TPersonEditForm = class(TContactEditForm)
    IWLabel5: TIWLabel;
    ContactExposerDateOfBirth: TDateTimeField;
    IWDBEdit5: TIWDBEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PersonEditForm: TPersonEditForm;

implementation

uses ServerController;

{$R *.dfm}

end.
