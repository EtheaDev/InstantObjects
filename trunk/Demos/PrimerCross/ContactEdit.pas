unit ContactEdit;

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Mask, DBCtrls, Grids, DBGrids, ComCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QMask, QDBCtrls, QGrids, QDBGrids, QComCtrls,
{$ENDIF}
{$IFDEF EXTERNALSTORAGE}ModelExternal,{$ELSE}Model,{$ENDIF}
  BasicEdit, Db, InstantPresentation;

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
