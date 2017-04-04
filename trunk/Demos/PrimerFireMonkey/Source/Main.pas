unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts,
  Fmx.Bind.Navigator, InstantPersistence, InstantBrokers, Data.DB,
  InstantXML, InstantPresentation, Data.Bind.Components, Data.Bind.DBScope,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, FMX.Grid, Data.Bind.DBLinks, Fmx.Bind.DBLinks, FMX.Objects, FMX.Edit, FMX.Ani;

type
  TfmMain = class(TForm)
    DBNavigator1: TBindNavigator;
    ContactsSource: TDataSource;
    InstanXMLXConnector1: TInstantXMLConnector;
    ContactSelector: TInstantSelector;
    XMLFilesAccessor1: TXMLFilesAccessor;
    ConnectButton: TButton;
    RecordLabel: TLabel;
    BindScopeDB1: TBindScopeDB;
    StringGrid1: TStringGrid;
    ContactSelectorAddressCity: TStringField;
    ContactSelectorAddressCountryId: TStringField;
    ContactSelectorAddressCountryName: TStringField;
    ContactSelectorAddressState: TStringField;
    ContactSelectorAddressStreet: TMemoField;
    ContactSelectorAddressZip: TStringField;
    ContactSelectorCategoryName: TStringField;
    ContactSelectorCity: TStringField;
    ContactSelectorMainPhoneNumber: TStringField;
    ContactSelectorName: TStringField;
    ContactSelectorPhones: TDataSetField;
    BindingsList: TBindingsList;
    BindDBGridLink1: TBindDBGridLink;
    ImageControl1: TImageControl;
    PersonExposer: TInstantExposer;
    PersonExposerAddressCity: TStringField;
    PersonExposerAddressCountryId: TStringField;
    PersonExposerAddressCountryName: TStringField;
    PersonExposerAddressState: TStringField;
    PersonExposerAddressStreet: TMemoField;
    PersonExposerAddressZip: TStringField;
    PersonExposerBirthDate: TDateField;
    PersonExposerBirthTime: TTimeField;
    PersonExposerCategoryName: TStringField;
    PersonExposerCity: TStringField;
    PersonExposerEmails: TDataSetField;
    PersonExposerEmployerAddressCity: TStringField;
    PersonExposerEmployerAddressCountryId: TStringField;
    PersonExposerEmployerAddressCountryName: TStringField;
    PersonExposerEmployerAddressState: TStringField;
    PersonExposerEmployerAddressStreet: TMemoField;
    PersonExposerEmployerAddressZip: TStringField;
    PersonExposerEmployerCategoryName: TStringField;
    PersonExposerEmployerCity: TStringField;
    PersonExposerEmployerEmployees: TDataSetField;
    PersonExposerEmployerMainPhoneNumber: TStringField;
    PersonExposerEmployerName: TStringField;
    PersonExposerEmployerPhones: TDataSetField;
    PersonExposerMainEmailAddress: TStringField;
    PersonExposerMainPhoneNumber: TStringField;
    PersonExposerName: TStringField;
    PersonExposerPhones: TDataSetField;
    PersonExposerPicture: TBlobField;
    PersonExposerSalary: TBCDField;
    Edit1: TEdit;
    BindScopeDB2: TBindScopeDB;
    PersonSource: TDataSource;
    DBLinkEdit1Name1: TBindDBEditLink;
    DBLinkImageControl1Picture1: TBindDBImageLink;
    PictureOpacityAnimation: TFloatAnimation;
    PictureRotationAngle: TFloatAnimation;
    procedure ConnectButtonClick(Sender: TObject);
    procedure ContactSelectorAfterScroll(DataSet: TDataSet);
    procedure ContactSelectorAfterOpen(DataSet: TDataSet);
  private
    procedure InitConnection;
    procedure UpdateRecordLabel;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.fmx}

uses
  RandomData, DemoData, Model;

{ TfmMain }

procedure TfmMain.ConnectButtonClick(Sender: TObject);
begin
  InitConnection;
  ContactSelector.Open;
end;

procedure TfmMain.ContactSelectorAfterOpen(DataSet: TDataSet);
begin
  UpdateRecordLabel;
end;

procedure TfmMain.ContactSelectorAfterScroll(DataSet: TDataSet);
begin
  if ContactSelector.CurrentObject is TPerson then
  begin
    PersonExposer.Subject := TPerson(ContactSelector.CurrentObject);
    PictureOpacityAnimation.Start;
    PictureRotationAngle.Start;
  end
  else
    PersonExposer.Subject := nil;
  UpdateRecordLabel;
end;

procedure TfmMain.InitConnection;
begin
  XMLFilesAccessor1.RootFolder := ExtractFilePath(ParamStr(0))+
    '..'+PathDelim+'..'+PathDelim+'..'+PathDelim+'..'+PathDelim+'Database';
//  CreateCategories;
//  CreateCountries;
//  CreateRandomContacts(100, True);
end;

procedure TfmMain.UpdateRecordLabel;
begin
  RecordLabel.Text := Format('Record %d of %d',[ContactSelector.RecNo, ContactSelector.RecordCount]);
end;

end.
