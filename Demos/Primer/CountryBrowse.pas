unit CountryBrowse;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BasicBrowse, ImgList, Menus, ActnList, Grids, DBGrids, ExtCtrls,
  ComCtrls, ToolWin, Db, InstantPresentation, Model, StdCtrls;

type
  TCountryBrowseForm = class(TBasicBrowseForm)
  end;

implementation

uses
  MainData;

{$R *.DFM}

end.
