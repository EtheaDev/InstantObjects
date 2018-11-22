inherited QueryViewForm: TQueryViewForm
  Width = 772
  Height = 320
  ExplicitWidth = 772
  ExplicitHeight = 320
  object Splitter: TSplitter
    Left = 0
    Top = 182
    Width = 772
    Height = 6
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 147
  end
  object ExamplePanel: TPanel
    Left = 0
    Top = 0
    Width = 772
    Height = 35
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object ExampleLabel: TLabel
      Left = 27
      Top = 16
      Width = 40
      Height = 13
      Caption = 'E&xample'
      FocusControl = ExampleComboBox
    end
    object ExampleComboBox: TComboBox
      Left = 73
      Top = 8
      Width = 666
      Height = 21
      Style = csDropDownList
      DropDownCount = 12
      TabOrder = 0
      OnClick = ExampleComboBoxClick
    end
  end
  object CommandPanel: TPanel
    Left = 0
    Top = 35
    Width = 772
    Height = 147
    Align = alTop
    BevelOuter = bvLowered
    BorderWidth = 30
    TabOrder = 1
    object CommandLabel: TLabel
      Left = 30
      Top = 14
      Width = 47
      Height = 13
      Caption = '&Command'
      FocusControl = CommandEdit
    end
    object NumberLabel: TLabel
      Left = 614
      Top = 126
      Width = 77
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Max Count:'
      FocusControl = MaxCountEdit
    end
    object Label1: TLabel
      Left = 473
      Top = 125
      Width = 26
      Height = 13
      Caption = 'Mode'
      FocusControl = LoadModeComboBox
    end
    object CommandEdit: TMemo
      Left = 31
      Top = 31
      Width = 710
      Height = 85
      Align = alClient
      TabOrder = 0
    end
    object ExecuteButton: TButton
      Left = 30
      Top = 119
      Width = 59
      Height = 25
      Action = ExecuteAction
      TabOrder = 1
    end
    object MaxCountEdit: TMaskEdit
      Left = 695
      Top = 122
      Width = 44
      Height = 21
      EditMask = '#########;1; '
      MaxLength = 9
      TabOrder = 5
      Text = '0        '
    end
    object LoadModeComboBox: TComboBox
      Left = 507
      Top = 122
      Width = 101
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 4
      Text = 'Keys First'
      Items.Strings = (
        'Keys First'
        'Partial Burst'
        'Full Burst')
    end
    object FetchAllCheckBox: TCheckBox
      Left = 111
      Top = 123
      Width = 64
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Fetch All'
      TabOrder = 2
    end
    object StatementCacheCheckBox: TCheckBox
      Left = 199
      Top = 123
      Width = 106
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Statement Cache'
      TabOrder = 3
    end
  end
  object ResultPageControl: TPageControl
    Left = 0
    Top = 188
    Width = 772
    Height = 132
    ActivePage = ResultTabSheet
    Align = alClient
    TabOrder = 2
    object ResultTabSheet: TTabSheet
      Caption = 'Result'
      object ResultGrid: TDBGrid
        Left = 0
        Top = 0
        Width = 764
        Height = 104
        Align = alClient
        DataSource = TestSource
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object TranslatedQueryTabSheet: TTabSheet
      Caption = 'Translated Query'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TranslatedQueryMemo: TMemo
        Left = 0
        Top = 0
        Width = 764
        Height = 104
        Align = alClient
        TabOrder = 0
      end
    end
    object StatsTabSheet: TTabSheet
      Caption = 'Stats'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StatsMemo: TMemo
        Left = 0
        Top = 0
        Width = 764
        Height = 104
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object TestSelector: TInstantSelector
    AfterClose = TestSelectorAfterClose
    Left = 32
    Top = 192
  end
  object TestSource: TDataSource
    DataSet = TestSelector
    Left = 64
    Top = 192
  end
  object Actions: TActionList
    OnUpdate = ActionsUpdate
    Left = 368
    Top = 40
    object ExecuteAction: TAction
      Caption = '&Execute'
      Hint = 'Execute'
      OnExecute = ExecuteActionExecute
    end
  end
end
