unit CategoryBrowse;

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ImgList, Menus, ActnList, Grids, DBGrids, ExtCtrls, StdCtrls,
  ComCtrls, ToolWin,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs, QComCtrls,
  QImgList, QMenus, QActnList, QGrids, QDBGrids, QExtCtrls, QStdCtrls,
{$ENDIF}
  BasicBrowse, InstantPresentation, DB;

type
  TCategoryBrowseForm = class(TBasicBrowseForm)
  end;

implementation

uses
  MainData;

{$R *.dfm}

end.
 
