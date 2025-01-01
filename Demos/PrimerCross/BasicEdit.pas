unit BasicEdit;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Data.DB,
  Vcl.DBGrids,
  Vcl.Grids,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  InstantPresentation;

type
  TBasicEditFormClass = class of TBasicEditForm;

  TBasicEditForm = class(TForm)
    ButtonPanel: TPanel;
    ClientPanel: TPanel;
    PageControl: TPageControl;
    SubjectExposer: TInstantExposer;
    SubjectSource: TDataSource;
    DetailsSheet: TTabSheet;
    AnchorPanel: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DbGridDrawColumnCellFixW11(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
  private
    function GetSubject: TObject;
    procedure SetSubject(const Value: TObject);
  protected
    function GetIsValid: Boolean; virtual;
    procedure UpdateActions; override;
  public
    class function ObjectClass: TClass; virtual; abstract;
    class procedure RegisterClass;
    property IsValid: Boolean read GetIsValid;
    property Subject: TObject read GetSubject write SetSubject;
  end;

function EditObject(AObject: TObject): Boolean;
function FindEditFormClass(ObjectClass: TClass): TBasicEditFormClass;
function GetEditFormClass(ObjectClass: TClass): TBasicEditFormClass;
procedure RegisterEditFormClass(EditFormClass: TBasicEditFormClass);

implementation

{$R *.dfm}

uses
  Vcl.Themes;

var
  EditFormClasses: TList;

function EditObject(AObject: TObject): Boolean;
var
  EditFormClass: TBasicEditFormClass;
begin
  EditFormClass := GetEditFormClass(AObject.ClassType);
  with EditFormClass.Create(nil) do
  try
    Subject := AObject;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

function FindEditFormClass(ObjectClass: TClass): TBasicEditFormClass;
var
  I: Integer;
begin
  while Assigned(ObjectClass) do
  begin
    for I := 0 to Pred(EditFormClasses.Count) do
    begin
      Result := EditFormClasses[I];
      if Result.ObjectClass = ObjectClass then
        Exit;
    end;
    ObjectClass := ObjectClass.ClassParent;
  end;
  Result := nil;
end;

function GetEditFormClass(ObjectClass: TClass): TBasicEditFormClass;
begin
  Result := FindEditFormClass(ObjectClass);
  if not Assigned(Result) then
    raise Exception.Create('No editor registered for: ' + ObjectClass.ClassName);
end;

procedure RegisterEditFormClass(EditFormClass: TBasicEditFormClass);
begin
  EditFormClasses.Add(EditFormClass);
end;

{ TBasicEditForm }

function TBasicEditForm.GetIsValid: Boolean;
begin
  Result := True;
end;

function TBasicEditForm.GetSubject: TObject;
begin
  Result := SubjectExposer.Subject;
end;

procedure TBasicEditForm.OkButtonClick(Sender: TObject);
begin
  try
    SubjectExposer.PostChanges;
  except
    ModalResult := mrNone;
    raise;
  end;
end;

class procedure TBasicEditForm.RegisterClass;
begin
  RegisterEditFormClass(Self);
end;

procedure TBasicEditForm.SetSubject(const Value: TObject);
begin
  SubjectExposer.Subject := Value;
end;

procedure TBasicEditForm.UpdateActions;
begin
  inherited;
  OkButton.Enabled := IsValid;
end;

procedure TBasicEditForm.DbGridDrawColumnCellFixW11(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  LDbGrid: TDbGrid;
begin
  LDbGrid := Sender as TDbGrid;
  //Resolve bad painting of selected cell in Windows 11
  if not StyleServices.Enabled or (StyleServices.IsSystemStyle) then
  begin
    if ((gdSelected in State) and (gdFocused in State))
      or ((gdSelected in State) and (dgRowSelect in LDbGrid.Options) and LDbGrid.Focused)
      then
      LDbGrid.Canvas.Brush.Color := clHighlight;
    LDbGrid.DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end;
end;

procedure TBasicEditForm.FormCreate(Sender: TObject);
begin
  inherited;
  BorderStyle := bsSingle;
end;


initialization
  EditFormClasses := TList.Create;

finalization
  EditFormClasses.Free;

end.
