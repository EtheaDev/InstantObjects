unit ContactFilterEdit;

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
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.Mask,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  ContactEdit,
  InstantPresentation,
  Model;

type
  TContactFilterEditForm = class(TContactEditForm)
    DynamicCheckBox: TDBCheckBox;
    InfoPanel: TPanel;
    Info1Label: TLabel;
    Info2Label: TLabel;
  protected
    function GetIsValid: Boolean; override;
  public
    class function ObjectClass: TClass; override;
  end;

implementation

{$R *.dfm}

{ TContactFilterEditForm }

function TContactFilterEditForm.GetIsValid: Boolean;
begin
  Result := not TContactFilter(Subject).IsEmpty;
end;

class function TContactFilterEditForm.ObjectClass: TClass;
begin
  Result := TContactFilter;
end;

initialization
  TContactFilterEditForm.RegisterClass;

end.
