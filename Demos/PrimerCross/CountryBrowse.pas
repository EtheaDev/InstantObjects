unit CountryBrowse;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  SysUtils, Classes, BasicBrowse, DB,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ImgList, Menus, ActnList, Grids, DBGrids, ExtCtrls,
  ComCtrls, ToolWin, StdCtrls,
  InstantPresentation, Model;

type
  TCountryBrowseForm = class(TBasicBrowseForm)
  end;

implementation

uses
  MainData;

{$R *.dfm}

end.
