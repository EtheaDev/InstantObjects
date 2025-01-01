unit PersonBrowse;

interface

{$I '../../Source/InstantDefines.inc'}

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
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ActnList,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.StdCtrls,
  ContactBrowse,
  InstantPresentation,
  Model;

type
  TPersonBrowseForm = class(TContactBrowseForm)
  private
    function GetSelected: TPerson;
    procedure SetSelected(const Value: TPerson);
  protected
    class function ObjectClass: TClass; override;
  public
    property Selected: TPerson read GetSelected write SetSelected;
  end;

implementation

{$R *.dfm}

{ TPersonBrowseForm }

function TPersonBrowseForm.GetSelected: TPerson;
begin
  Result := inherited Selected as TPerson;
end;

class function TPersonBrowseForm.ObjectClass: TClass;
begin
  Result := TPerson;
end;

procedure TPersonBrowseForm.SetSelected(const Value: TPerson);
begin
  inherited Selected := Value;
end;

end.
