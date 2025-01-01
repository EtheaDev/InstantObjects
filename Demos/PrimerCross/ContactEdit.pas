unit ContactEdit;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.DBCtrls,
  Data.DB,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Mask,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ComCtrls,
  BasicEdit,
  InstantPresentation,
  Model;

type
  TContactEditForm = class(TBasicEditForm)
    CountryEdit: TDBLookupComboBox;
    CountryLabel: TLabel;
    CityEdit: TDBEdit;
    CityLabel: TLabel;
    MidBevel: TBevel;
    NameEdit: TDBEdit;
    NameLabel: TLabel;
    PhonesExposer: TInstantExposer;
    PhonesLabel: TLabel;
    PhonesGrid: TDBGrid;
    PhonesSource: TDataSource;
    StreetLabel: TLabel;
    StateEdit: TDBEdit;
    StateLabel: TLabel;
    StreetEdit: TDBMemo;
    ZipEdit: TDBEdit;
    ZipLabel: TLabel;
    CategoryLabel: TLabel;
    CategoryEdit: TDBLookupComboBox;
    procedure SubjectExposerIncludeField(Sender: TObject;
      const FieldName: String; var Include: Boolean);
  private
    function GetSubject: TContact;
    procedure SetSubject(const Value: TContact);
  protected
    function GetIsValid: Boolean; override;
  public
    class function ObjectClass: TClass; override;
    property Subject: TContact read GetSubject write SetSubject;
  end;

implementation

{$R *.dfm}

uses
  Vcl.Themes,
  MainData;

{ TContactEditForm }

function TContactEditForm.GetIsValid: Boolean;
begin
  Result := NameEdit.Text <> '';
end;

function TContactEditForm.GetSubject: TContact;
begin
  Result := inherited Subject as TContact;
end;

class function TContactEditForm.ObjectClass: TClass;
begin
  Result := TContact;
end;

procedure TContactEditForm.SetSubject(const Value: TContact);
begin
  inherited Subject := Value;
end;

procedure TContactEditForm.SubjectExposerIncludeField(Sender: TObject;
  const FieldName: String; var Include: Boolean);
begin
  if FieldName = 'MainPhoneNumber' then
    Include := False;
end;

initialization
  TContactEditForm.RegisterClass;

end.
