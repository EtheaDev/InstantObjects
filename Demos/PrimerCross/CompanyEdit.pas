unit CompanyEdit;

interface
{$IFDEF VER130}{$DEFINE MSWINDOWS}{$ENDIF}
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, DBGrids,
  Mask, DBCtrls, ComCtrls, Menus, ImgList, ActnList, Buttons,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls, QGrids, QDBGrids,
  QMask, QDBCtrls, QComCtrls, QMenus, QImgList, QActnList, QButtons,
{$ENDIF}
  ContactEdit, DB, InstantPresentation, Model;

type
  TCompanyEditForm = class(TContactEditForm)
    Actions: TActionList;
    ActionImages: TImageList;
    EmployeeButtonPanel: TPanel;
    EmployeeDeleteItem: TMenuItem;
    EmployeeEditAction: TAction;
    EmployeeEditButton: TBitBtn;
    EmployeeEditItem: TMenuItem;
    EmployeeExposer: TInstantExposer;
    EmployeeGrid: TDBGrid;
    EmployeeGridMenu: TPopupMenu;
    EmployeeGridPanel: TPanel;
    EmployeeLookupButton: TBitBtn;
    EmployeeLookupAction: TAction;
    EmployeeLookupItem: TMenuItem;
    EmployeeNewAction: TAction;
    EmployeeNewButton: TBitBtn;
    EmployeeNewItem: TMenuItem;
    EmployeeRemoveAction: TAction;
    EmployeeRemoveButton: TBitBtn;
    EmployeeRemoveItem: TMenuItem;
    EmployeeSheet: TTabSheet;
    EmployeeSource: TDataSource;
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure EmployeeNewActionExecute(Sender: TObject);
    procedure EmployeeEditActionExecute(Sender: TObject);
    procedure EmployeeLookupActionExecute(Sender: TObject);
    procedure EmployeeRemoveActionExecute(Sender: TObject);
    procedure EmployeeExposerCompare(Sender, AObject1, AObject2: TObject;
      var Compare: Integer);
    procedure FormCreate(Sender: TObject);
  private
    function GetSubject: TCompany;
    procedure SetSubject(const Value: TCompany);
  public
    class function ObjectClass: TClass; override;
    property Subject: TCompany read GetSubject write SetSubject;
  end;

implementation

{$R *.dfm}

uses
  BasicEdit, Utility, PersonBrowse, InstantImageUtils;

{ TCompanyEditForm }

procedure TCompanyEditForm.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  Employee: TPerson;
begin
  Employee := EmployeeExposer.CurrentObject as TPerson;
  EmployeeEditAction.Enabled := Assigned(Employee);
  EmployeeRemoveAction.Enabled := Assigned(Employee);

  Handled := True;
end;

procedure TCompanyEditForm.EmployeeEditActionExecute(Sender: TObject);
var
  Employee: TPerson;
begin
  Employee := EmployeeExposer.CurrentObject as TPerson;
  Employee.AddRef; // In case Employee is removed from Company
  try
    EditObject(Employee);
  finally
    Employee.Free;
  end;
end;

procedure TCompanyEditForm.EmployeeExposerCompare(Sender, AObject1,
  AObject2: TObject; var Compare: Integer);
begin
  Compare := AnsiCompareText(TPerson(AObject1).Name, TPerson(AObject2).Name);
end;

procedure TCompanyEditForm.EmployeeLookupActionExecute(Sender: TObject);
begin
  with TPersonBrowseForm.Create(nil) do
  try
    LookupMode := True;
    Selected := EmployeeExposer.CurrentObject as TPerson;
    if Execute and Assigned(Selected) then
      Selected.EmployBy(Subject);
  finally
    Free;
  end;
end;

procedure TCompanyEditForm.EmployeeNewActionExecute(Sender: TObject);
var
  Employee: TPerson;
begin
  Employee := TPerson.Create;
  try
    if EditObject(Employee) then
    begin
      if not Assigned(Employee.Employer) then
        Employee.EmployBy(Subject);
      SubjectExposer.RefreshCurrentObject;
    end;
  finally
    Employee.Free;
  end;
end;

procedure TCompanyEditForm.EmployeeRemoveActionExecute(Sender: TObject);
var
  Employee: TPerson;
begin
  Employee := EmployeeExposer.CurrentObject as TPerson;
  if Confirm(Format('Remove "%s" from company?', [Employee.Name])) then
    Employee.EmployBy(nil);
end;

function TCompanyEditForm.GetSubject: TCompany;
begin
  Result := inherited Subject as TCompany;
end;

class function TCompanyEditForm.ObjectClass: TClass;
begin
  Result := TCompany;
end;

procedure TCompanyEditForm.SetSubject(const Value: TCompany);
begin
  inherited Subject := Value;
end;

procedure TCompanyEditForm.FormCreate(Sender: TObject);
begin
  inherited;
{$IFDEF MSWINDOWS}
  LoadMultipleImages(ActionImages,'COMPANYEDITACTIONIMAGES',HInstance);
{$ENDIF}
{$IFDEF LINUX}
  LoadMultipleImages(ActionImages,ExtractFilePath(Application.ExeName)+'COMPANYEDITACTIONIMAGES.BMP');
{$ENDIF}

  //getting glyph images
  EmployeeNewButton.Action := EmployeeNewAction;
  EmployeeLookupButton.Action := EmployeeLookupAction;
  EmployeeEditButton.Action := EmployeeEditAction;
  EmployeeRemoveButton.Action := EmployeeRemoveAction;
end;

initialization
  TCompanyEditForm.RegisterClass;

end.
