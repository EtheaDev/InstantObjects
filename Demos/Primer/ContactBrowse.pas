unit ContactBrowse;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BasicBrowse, Menus, ImgList, ActnList, Grids, DBGrids, ExtCtrls, ComCtrls,
  ToolWin, Db, InstantPresentation, Model, StdCtrls;

type
  TContactBrowseForm = class(TBasicBrowseForm)
    ContactSelector: TInstantSelector;
  protected
    function CreateObject: TObject; override;
    function Find(const Text: string): Boolean; override;
    class function ObjectClass: TClass; virtual;
  end;

implementation

uses
  Utility;

{$R *.DFM}

{ TContactLookupForm }

function TContactBrowseForm.CreateObject: TObject;
begin
  Result := ObjectClass.Create;
end;

function TContactBrowseForm.Find(const Text: string): Boolean;
var
  S: string;
begin
  S := 'SELECT * FROM ' + ObjectClass.ClassName;
  if Text <> '' then
    S := S + ' WHERE Name LIKE "%' + Text + '%"';
  S := S + ' ORDER BY Name';
  with ContactSelector do
  begin
    Close;
    Command.Text := S;
    Open;
    Result := ObjectCount > 0;
  end;
end;

class function TContactBrowseForm.ObjectClass: TClass;
begin
  Result := TContact;
end;

end.
