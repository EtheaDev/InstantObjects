unit CompanyBrowse;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Menus, ImgList, ActnList, Grids, DBGrids, ExtCtrls, ComCtrls, ToolWin,
  ContactBrowse, DB, InstantPresentation, Model;

type
  TCompanyBrowseForm = class(TContactBrowseForm)
  private
    function GetSelected: TCompany;
    procedure SetSelected(const Value: TCompany);
  protected
    class function ObjectClass: TClass; override;
  public
    property Selected: TCompany read GetSelected write SetSelected;
  end;

implementation

{$R *.dfm}

{ TCompanyBrowseForm }

function TCompanyBrowseForm.GetSelected: TCompany;
begin
  Result := inherited Selected as TCompany;
end;

class function TCompanyBrowseForm.ObjectClass: TClass;
begin
  Result := TCompany;
end;

procedure TCompanyBrowseForm.SetSelected(const Value: TCompany);
begin
  inherited Selected := Value;
end;

end.
 