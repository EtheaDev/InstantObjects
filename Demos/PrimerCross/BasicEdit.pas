unit BasicEdit;

interface
{$IFDEF VER130}{$DEFINE MSWINDOWS}{$ENDIF}
uses
  SysUtils, Classes, DB,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, QStdCtrls, QComCtrls,
{$ENDIF}
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

procedure TBasicEditForm.FormCreate(Sender: TObject);
begin
  inherited;
{$IFDEF MSWINDOWS}
  BorderStyle := bsSingle;
{$ENDIF}
{$IFDEF LINUX}
  BorderStyle := fbsSingle;
{$ENDIF}
end;


initialization
  EditFormClasses := TList.Create;

finalization
  EditFormClasses.Free;

end.
