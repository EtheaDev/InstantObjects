unit ContactEditIW;

interface

uses
  Classes, SysUtils, IWAppForm, IWApplication, IWColor, IWTypes,
  UserSessionUnit, InstantPresentation,
  IWCompButton, IWCompLabel, IWCompEdit, IWDBStdCtrls, Controls,
  IWVCLBaseControl, IWBaseControl, IWBaseHTMLControl, IWControl,
  DB, IWGrids, IWDBGrids;

type
  TContactEditForm = class(TIWAppForm)
    ContactExposer: TInstantExposer;
    ContactExposerName: TStringField;
    ContactExposerAddressStreet: TStringField;
    ContactExposerAddressCity: TStringField;
    ContactExposerAddressPostalCode: TStringField;
    ContactExposerPhones: TDataSetField;
    ContactSource: TDataSource;
    PhonesExposer: TInstantExposer;
    PhonesSource: TDataSource;
    IWDBEdit1: TIWDBEdit;
    IWDBEdit2: TIWDBEdit;
    IWDBEdit3: TIWDBEdit;
    IWDBEdit4: TIWDBEdit;
    IWLabel1: TIWLabel;
    IWLabel2: TIWLabel;
    IWLabel3: TIWLabel;
    IWLabel4: TIWLabel;
    OkButton: TIWButton;
    CancelButton: TIWButton;
    IWDBNavigator1: TIWDBNavigator;
    IWDBEdit6: TIWDBEdit;
    IWDBEdit7: TIWDBEdit;
    IWLabel6: TIWLabel;
    IWLabel7: TIWLabel;
    IWDBGrid1: TIWDBGrid;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure IWDBGrid1Columns1Click(ASender: TObject;
      const AValue: String);
  public
  end;

implementation

uses ServerController;

{$R *.dfm}


procedure TContactEditForm.OkButtonClick(Sender: TObject);
begin
  UserSession.bOk := True;
  ContactExposer.PostChanges;
  CancelButtonClick(Sender);
end;

procedure TContactEditForm.CancelButtonClick(Sender: TObject);
begin
  Release;
end;

procedure TContactEditForm.IWDBGrid1Columns1Click(ASender: TObject;
  const AValue: String);
begin
  PhonesExposer.Locate('Name', AValue, []);
end;

end.
