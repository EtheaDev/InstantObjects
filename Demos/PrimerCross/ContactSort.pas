unit ContactSort;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  InstantPersistence;

type
  TContactSortForm = class(TForm)
    AttributeComboBox: TComboBox;
    AttributeRadioButton: TRadioButton;
    CancelButton: TButton;
    InfoLabel: TLabel;
    OkButton: TButton;
    PropertyComboBox: TComboBox;
    PropertyRadioButton: TRadioButton;
    procedure FormCreate(Sender: TObject);
  private
    function GetByAttribute: Boolean;
    function GetOrderName: string;
    procedure LoadAttributes(AClass: TInstantObjectClass; Items: TStrings);
    procedure LoadProperties(AClass: TClass; Items: TStrings);
    procedure SetByAttribute(const Value: Boolean);
    procedure SetOrderName(const Value: string);
  protected
    procedure UpdateActions; override;
    procedure UpdateControls;
  public
    property ByAttribute: Boolean read GetByAttribute write SetByAttribute;
    property OrderName: string read GetOrderName write SetOrderName;
  end;

implementation

uses
  System.TypInfo,
  Model,
  InstantRtti,
  InstantPresentation,
  InstantMetadata,
  InstantTypes;

{$R *.dfm}

{ TContactSortForm }

procedure TContactSortForm.FormCreate(Sender: TObject);
begin
  Font.Assign(Screen.IconFont);
  BorderStyle := bsDialog;
  LoadAttributes(TContact, AttributeComboBox.Items);
  LoadProperties(TContact, PropertyComboBox.Items);
  UpdateControls;
end;

function TContactSortForm.GetByAttribute: Boolean;
begin
  Result := AttributeRadioButton.Checked;
end;

function TContactSortForm.GetOrderName: string;
begin
  if ByAttribute then
    Result := AttributeComboBox.Text
  else
    Result := PropertyComboBox.Text;
end;

procedure IncludeAttribute(AttributeMetadata: TInstantAttributeMetadata;
  var Include: Boolean; var Traverse: Boolean);
begin
  Include := AttributeMetadata.Category = acSimple;
  Traverse := AttributeMetadata.AttributeClass = TInstantReference;
end;

procedure TContactSortForm.LoadAttributes(AClass: TInstantObjectClass;
  Items: TStrings);
begin
  InstantGetAttributeList(AClass.Metadata, Items, IncludeAttribute);
end;

procedure IncludeProperty(PropInfo: PPropInfo; var Include: Boolean;
  var Traverse: Boolean);
begin
  Include := PropInfo.PropType^.Kind <> tkClass;
end;

procedure TContactSortForm.LoadProperties(AClass: TClass; Items: TStrings);
begin
  InstantGetPropertyList(AClass, Items, IncludeProperty);
end;

procedure TContactSortForm.SetByAttribute(const Value: Boolean);
begin
  if Value then
    AttributeRadioButton.Checked := True
  else
    PropertyRadioButton.Checked := True;
  UpdateControls;
end;

procedure TContactSortForm.SetOrderName(const Value: string);
var
  Combo: TComboBox;
begin
  if ByAttribute then
    Combo := AttributeComboBox
  else
    Combo := PropertyComboBox;
  with Combo do
    ItemIndex := Items.IndexOf(Value);
end;

procedure TContactSortForm.UpdateActions;
begin
  inherited;
  UpdateControls;
end;

procedure TContactSortForm.UpdateControls;

  procedure EnableCombo(Combo: TComboBox; Enable: Boolean);
  const
    Colors: array[Boolean] of TColor = (clBtnFace, clWindow);
  begin
    Combo.Enabled := Enable;
    Combo.Color := Colors[Enable];
  end;

begin
  EnableCombo(AttributeComboBox, AttributeRadioButton.Checked);
  EnableCombo(PropertyComboBox, PropertyRadioButton.Checked);
  OkButton.Enabled :=
    (ByAttribute and (AttributeComboBox.Text <> '')) or
    (not ByAttribute and (PropertyComboBox.Text <> ''));
end;

end.
