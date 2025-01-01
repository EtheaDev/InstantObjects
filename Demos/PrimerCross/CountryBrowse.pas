unit CountryBrowse;

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
  Vcl.ImgList,
  Vcl.Menus,
  Vcl.ActnList,
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.StdCtrls,
  BasicBrowse,
  InstantPresentation,
  Model;

type
  TCountryBrowseForm = class(TBasicBrowseForm)
  end;

implementation

uses
  MainData;

{$R *.dfm}

end.
