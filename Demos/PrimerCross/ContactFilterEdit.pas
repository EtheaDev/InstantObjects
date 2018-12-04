unit ContactFilterEdit;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, DBGrids, Mask, DBCtrls, ExtCtrls, ComCtrls,
  ContactEdit, DB, InstantPresentation, Model;

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
