unit ContactFilterEdit;

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, DBGrids, Mask, DBCtrls, ExtCtrls, ComCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QGrids, QDBGrids, QMask, QDBCtrls, QExtCtrls, QComCtrls,
{$ENDIF}
{$IFDEF EXTERNALSTORAGE}ModelExternal,{$ELSE}Model,{$ENDIF}
  ContactEdit, DB, InstantPresentation;

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
