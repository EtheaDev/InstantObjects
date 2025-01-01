unit CompanyBrowse;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Data.DB,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ActnList,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  ContactBrowse,
  InstantPresentation,
  Model;

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
 