inherited ContactViewForm: TContactViewForm
  Width = 627
  Height = 465
  ExplicitWidth = 627
  ExplicitHeight = 465
  object GridPanel: TPanel
    Left = 0
    Top = 0
    Width = 627
    Height = 465
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object HeaderBevel: TBevel
      Left = 0
      Top = 28
      Width = 627
      Height = 5
      Align = alTop
      Shape = bsTopLine
    end
    object HeaderPanel: TPanel
      Left = 0
      Top = 0
      Width = 627
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object FindPanel: TPanel
        Left = 281
        Top = 0
        Width = 346
        Height = 28
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object FindLabel: TLabel
          Left = 2
          Top = 5
          Width = 20
          Height = 13
          Alignment = taRightJustify
          Caption = 'Find'
          FocusControl = FindEdit
        end
        object FindEdit: TComboBox
          Left = 28
          Top = 2
          Width = 145
          Height = 21
          TabOrder = 0
          OnClick = FindEditClick
        end
        object FindButton: TBitBtn
          Left = 177
          Top = 2
          Width = 22
          Height = 21
          Action = FindAction
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          TabStop = False
        end
      end
      object ToolBar: TToolBar
        Left = 0
        Top = 0
        Width = 281
        Height = 28
        Align = alLeft
        BorderWidth = 1
        Images = ActionImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object NewPersonButton: TToolButton
          Left = 0
          Top = 0
          Action = NewPersonAction
        end
        object NewCompanyButton: TToolButton
          Left = 23
          Top = 0
          Action = NewCompanyAction
        end
        object ToolSep1: TToolButton
          Left = 46
          Top = 0
          Width = 8
          ImageIndex = 3
          Style = tbsSeparator
        end
        object DeleteButton: TToolButton
          Left = 54
          Top = 0
          Action = DeleteAction
        end
        object EditButton: TToolButton
          Left = 77
          Top = 0
          Action = EditAction
        end
        object ToolSep2: TToolButton
          Left = 100
          Top = 0
          Width = 8
          Caption = 'ToolSep2'
          ImageIndex = 8
          Style = tbsSeparator
        end
        object ExportButton: TToolButton
          Left = 108
          Top = 0
          Action = ExportAction
        end
        object ToolSep3: TToolButton
          Left = 131
          Top = 0
          Width = 8
          Caption = 'ToolSep3'
          ImageIndex = 1
          Style = tbsSeparator
        end
        object FilterButton: TToolButton
          Left = 139
          Top = 0
          Action = FilterAction
          Style = tbsCheck
        end
        object SortButton: TToolButton
          Left = 162
          Top = 0
          Action = SortAction
        end
        object ToolSep4: TToolButton
          Left = 185
          Top = 0
          Width = 8
          Caption = 'ToolSep4'
          ImageIndex = 7
          Style = tbsSeparator
        end
        object ExplorerButton: TToolButton
          Left = 193
          Top = 0
          Action = ExplorerAction
          Style = tbsCheck
        end
        object ToolSep5: TToolButton
          Left = 216
          Top = 0
          Width = 8
          Caption = 'ToolSep5'
          ImageIndex = 10
          Style = tbsSeparator
        end
        object CountriesButton: TToolButton
          Left = 224
          Top = 0
          Action = BrowseCountryAction
        end
        object CategoriesButton: TToolButton
          Left = 247
          Top = 0
          Action = BrowseCategoryAction
        end
        object ToolButton2: TToolButton
          Left = 270
          Top = 0
          Width = 8
          Caption = 'ToolSep6'
          ImageIndex = 11
          Style = tbsSeparator
        end
      end
    end
    object ClientPanel: TPanel
      Left = 0
      Top = 33
      Width = 627
      Height = 432
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object IndexTabControl: TTabControl
        Left = 0
        Top = 0
        Width = 627
        Height = 432
        Align = alClient
        TabHeight = 18
        TabOrder = 0
        TabWidth = 30
        OnChange = IndexTabControlChange
        object ExplorerSplitter: TSplitter
          Left = 454
          Top = 6
          Width = 4
          Height = 422
          Align = alRight
          AutoSnap = False
          MinSize = 100
          ExplicitLeft = 497
        end
        object ExplorerPanel: TPanel
          Left = 458
          Top = 6
          Width = 165
          Height = 422
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object Explorer: TInstantExplorer
            Left = 0
            Top = 0
            Width = 165
            Height = 422
            Align = alClient
            Images = ExplorerImages
            Layout = loVertical
            NodeTypes = [ntProperty, ntObject, ntContainer]
            OnGetImageIndex = ExplorerGetImageIndex
            OnGetNodeText = ExplorerGetNodeText
          end
        end
        object ContactGridPanel: TPanel
          Left = 4
          Top = 6
          Width = 450
          Height = 422
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          OnResize = ContactGridPanelResize
          object ContactGrid: TDBGrid
            Left = 0
            Top = 0
            Width = 450
            Height = 422
            Align = alClient
            DataSource = ContactSource
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
            ParentFont = False
            PopupMenu = ContactGridMenu
            ReadOnly = True
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Segoe UI'
            TitleFont.Style = []
            OnDrawColumnCell = ContactGridDrawColumnCell
            OnDblClick = ContactGridDblClick
            OnKeyDown = ContactGridKeyDown
            Columns = <
              item
                Expanded = False
                Width = 18
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'Name'
                Width = 130
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'Address.City'
                Title.Caption = 'City'
                Width = 90
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'Address.Country.Name'
                Title.Caption = 'Country'
                Width = 90
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'MainPhoneNumber'
                Title.Caption = 'Phone'
                Width = 70
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'Category.Name'
                Title.Caption = 'Category'
                Width = 60
                Visible = True
              end>
          end
        end
      end
    end
  end
  object ContactSelector: TInstantSelector
    AfterScroll = ContactSelectorAfterScroll
    OnCompare = ContactSelectorCompare
    OnFilterRecord = ContactSelectorFilterRecord
    OnLimit = ContactSelectorLimit
    Command.Strings = (
      '')
    BeforeClose = ContactSelectorBeforeClose
    Left = 16
    Top = 224
  end
  object ContactSource: TDataSource
    DataSet = ContactSelector
    Left = 48
    Top = 224
  end
  object ActionList: TActionList
    Images = ActionImages
    State = asSuspended
    OnUpdate = ActionListUpdate
    Left = 16
    Top = 256
    object NewPersonAction: TAction
      Caption = 'New &Person...'
      Hint = 'New Person'
      ImageIndex = 0
      ShortCut = 24656
      OnExecute = NewPersonActionExecute
    end
    object NewCompanyAction: TAction
      Caption = 'New &Company...'
      Hint = 'New Company'
      ImageIndex = 1
      ShortCut = 24643
      OnExecute = NewCompanyActionExecute
    end
    object DeleteAction: TAction
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 2
      ShortCut = 46
      OnExecute = DeleteActionExecute
    end
    object EditAction: TAction
      Caption = '&Edit'
      Hint = 'Edit'
      ImageIndex = 3
      ShortCut = 13
      OnExecute = EditActionExecute
    end
    object FilterAction: TAction
      Caption = '&Filter...'
      Hint = 'Filter'
      ImageIndex = 4
      OnExecute = FilterActionExecute
    end
    object SortAction: TAction
      Caption = '&Sort...'
      Hint = 'Sort'
      ImageIndex = 5
      OnExecute = SortActionExecute
    end
    object FindAction: TAction
      Hint = 'Find'
      ImageIndex = 6
      OnExecute = FindActionExecute
    end
    object ExplorerAction: TAction
      Caption = '&Explorer'
      Hint = 'Explorer'
      ImageIndex = 7
      OnExecute = ExplorerActionExecute
    end
    object ExportAction: TAction
      Caption = 'Export to &XML'
      Hint = 'Export to XML'
      ImageIndex = 8
      OnExecute = ExportActionExecute
    end
    object BrowseCountryAction: TAction
      Caption = 'Countries'
      Hint = 'Countries'
      ImageIndex = 9
      OnExecute = BrowseCountryActionExecute
    end
    object BrowseCategoryAction: TAction
      Caption = 'Categories'
      Hint = 'Categories'
      ImageIndex = 10
      OnExecute = BrowseCategoryActionExecute
    end
  end
  object ActionImages: TImageList
    Left = 48
    Top = 256
  end
  object ContactGridMenu: TPopupMenu
    Images = ActionImages
    Left = 16
    Top = 288
    object EditItem: TMenuItem
      Action = EditAction
    end
    object DeleteItem: TMenuItem
      Action = DeleteAction
    end
    object MenuSep1: TMenuItem
      Caption = '-'
    end
    object NewPersonItem: TMenuItem
      Action = NewPersonAction
    end
    object NewCompanyItem: TMenuItem
      Action = NewCompanyAction
    end
    object MenuSep2: TMenuItem
      Caption = '-'
    end
    object ExportItem: TMenuItem
      Action = ExportAction
    end
  end
  object ExplorerImages: TImageList
    Left = 496
    Top = 65
  end
end
