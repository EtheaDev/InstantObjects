inherited ContactBrowseForm: TContactBrowseForm
  Left = 328
  Caption = 'Contacts'
  PixelsPerInch = 96
  TextHeight = 13
  inherited BrowseGridPanel: TPanel
    inherited BrowseGrid: TDBGrid
      Options = [dgTitles, dgIndicator, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
      Columns = <
        item
          Expanded = False
          FieldName = 'Name'
          Width = 214
          Visible = True
        end>
    end
  end
  inherited BrowseSource: TDataSource
    DataSet = ContactSelector
  end
  object ContactSelector: TInstantSelector
    Left = 24
    Top = 122
  end
end
