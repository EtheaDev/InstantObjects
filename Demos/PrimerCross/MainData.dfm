object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 444
  Width = 563
  object CountrySelector: TInstantSelector
    FieldOptions = [foObjects, foThorough]
    Sorted = True
    OnCompare = CountrySelectorCompare
    Command.Strings = (
      'SELECT * FROM TCountry')
    Left = 40
    Top = 24
  end
  object CountrySource: TDataSource
    DataSet = CountrySelector
    Left = 40
    Top = 80
  end
  object CategorySelector: TInstantSelector
    FieldOptions = [foObjects, foThorough]
    Sorted = True
    OnCompare = CategorySelectorCompare
    Command.Strings = (
      'SELECT * FROM TCategory')
    Left = 136
    Top = 24
  end
  object CategorySource: TDataSource
    DataSet = CategorySelector
    Left = 136
    Top = 80
  end
end
