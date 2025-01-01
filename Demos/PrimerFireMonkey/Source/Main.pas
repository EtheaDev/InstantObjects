unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,
  System.Bindings.Outputs,
  Data.DB,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.EngExt,
  Data.Bind.DBLinks,
  Data.Bind.Controls,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  Fmx.Bind.Navigator,
  InstantPersistence,
  InstantBrokers,
  InstantXML,
  InstantPresentation,
  Fmx.Bind.DBEngExt,
  Fmx.Bind.Editors,
  FMX.Grid,
  Fmx.Bind.DBLinks,
  FMX.Objects,
  FMX.Edit,
  FMX.Ani,
  FMX.Grid.Style,
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.Controls.Presentation;

type
  TfmMain = class(TForm)
    DBNavigator1: TBindNavigator;
    ContactsSource: TDataSource;
    InstantXMLConnector: TInstantXMLConnector;
    ContactSelector: TInstantSelector;
    XMLFilesAccessor: TXMLFilesAccessor;
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
  RandomData,
  DemoData,
  Model;

{ TfmMain }

procedure TfmMain.ConnectButtonClick(Sender: TObject);
begin
  InitConnection;
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
  XMLFilesAccessor.RootFolder := ExtractFilePath(ParamStr(0))+
    '..\Database';
  InstantXMLConnector.Connected := True;
  InstantXMLConnector.IsDefault := True;
  ContactSelector.Open;
  if ContactSelector.Eof then
  begin
    ContactSelector.Close;
    Try
      CreateCategories;
      CreateCountries;
      CreateRandomContacts(100, True);
    Finally
      ContactSelector.Open;
    End;
  end;
end;

procedure TfmMain.UpdateRecordLabel;
begin
  RecordLabel.Text := Format('Record %d of %d',[ContactSelector.RecNo, ContactSelector.RecordCount]);
end;

end.
