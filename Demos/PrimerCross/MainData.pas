unit MainData;

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
{$ENDIF}
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs,
{$ENDIF}
  DB, InstantPresentation;

type
  TMainDataModule = class(TDataModule)
    CategorySelector: TInstantSelector;
    CategorySource: TDataSource;
    CountrySelector: TInstantSelector;
    CountrySource: TDataSource;
    procedure CategorySelectorCompare(Sender, AObject1, AObject2: TObject;
      var Compare: Integer);
    procedure CountrySelectorCompare(Sender, AObject1, AObject2: TObject;
      var Compare: Integer);
  public
    procedure Connect;
    procedure Disconnect;
  end;

var
  MainDataModule: TMainDataModule;

implementation

uses
  Model;

{$R *.dfm}

{ TMainDataModule }

procedure TMainDataModule.CategorySelectorCompare(Sender, AObject1,
  AObject2: TObject; var Compare: Integer);
begin
  Compare := AnsiCompareText(
    TCategory(AObject1).Name,
    TCategory(AObject2).Name);
end;

procedure TMainDataModule.Connect;
begin
  CountrySelector.Open;
  CategorySelector.Open;
end;

procedure TMainDataModule.CountrySelectorCompare(Sender, AObject1,
  AObject2: TObject; var Compare: Integer);
begin
  Compare := AnsiCompareText(
    TCountry(AObject1).Name,
    TCountry(AObject2).Name);
end;

procedure TMainDataModule.Disconnect;
begin
  CountrySelector.Close;
  CategorySelector.Close;
end;

end.
