unit CategoryBrowse;

interface

{$I '..\..\Source\InstantDefines.inc'}

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ImgList, Menus, ActnList, Grids, DBGrids, ExtCtrls, StdCtrls,
  ComCtrls, ToolWin,
  BasicBrowse, InstantPresentation, DB;

type
  TCategoryBrowseForm = class(TBasicBrowseForm)
  end;

implementation

uses
  MainData;

{$R *.dfm}

end.
 
