inherited ContactBrowseForm: TContactBrowseForm
  Caption = 'Contacts'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited BrowseGridPanel: TPanel
    inherited BrowseGrid: TDBGrid
      Options = [dgTitles, dgIndicator, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
      Columns = <
        item
          Expanded = False
          FieldName = 'Name'
          PickList.Strings = ()
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
