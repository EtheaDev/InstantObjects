unit CountryBrowse;

interface

{$IFDEF LINUX}
{$I '../../Source/InstantDefines.inc'}
{$ELSE}
{$I '..\..\Source\InstantDefines.inc'}
{$ENDIF}

uses
  SysUtils, Classes, BasicBrowse, DB,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ImgList, Menus, ActnList, Grids, DBGrids, ExtCtrls,
  ComCtrls, ToolWin, StdCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs,
  QImgList, QMenus, QActnList, QGrids, QDBGrids, QExtCtrls,
  QComCtrls, QStdCtrls,
{$ENDIF}
  InstantPresentation, Model;

type
  TCountryBrowseForm = class(TBasicBrowseForm)
  end;

implementation

uses
  MainData;

{$R *.dfm}

end.
