unit ContactFilterEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ContactEdit, Db, InstantPresentation, StdCtrls, Grids, DBGrids, Mask,
  DBCtrls, ExtCtrls, Model, ComCtrls;

type
  TContactFilterEditForm = class(TContactEditForm)
    InfoPanel: TPanel;
    Info1Label: TLabel;
    Info2Label: TLabel;
    DynamicCheckBox: TDBCheckBox;
  protected
    function GetIsValid: Boolean; override;
  public
    class function ObjectClass: TClass; override;
  end;

implementation

{$R *.DFM}

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
