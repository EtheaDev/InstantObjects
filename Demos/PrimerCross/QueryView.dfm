inherited QueryViewFrame: TQueryViewFrame
  Width = 745
  Height = 320
  ExplicitWidth = 745
  ExplicitHeight = 320
  object Splitter: TSplitter
    Left = 0
    Top = 182
    Width = 745
    Height = 6
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 147
    ExplicitWidth = 772
  end
  object ExamplePanel: TPanel
    Left = 0
    Top = 0
    Width = 745
    Height = 35
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      745
      35)
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
      Width = 642
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 12
      TabOrder = 0
      OnClick = ExampleComboBoxClick
    end
  end
  object CommandPanel: TPanel
    Left = 0
    Top = 35
    Width = 745
    Height = 147
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      745
      147)
    object CommandLabel: TLabel
      Left = 30
      Top = 14
      Width = 47
      Height = 13
      Caption = '&Command'
      FocusControl = CommandEdit
    end
    object NumberLabel: TLabel
      Left = 589
      Top = 126
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '&Max Count:'
      FocusControl = MaxCountEdit
      ExplicitLeft = 590
    end
    object Label1: TLabel
      Left = 448
      Top = 125
      Width = 26
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Mode'
      FocusControl = LoadModeComboBox
    end
    object CommandEdit: TMemo
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 737
      Height = 112
      Margins.Bottom = 30
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
      Left = 670
      Top = 122
      Width = 44
      Height = 21
      Anchors = [akTop, akRight]
      EditMask = '#########;1; '
      MaxLength = 9
      TabOrder = 5
      Text = '0        '
    end
    object LoadModeComboBox: TComboBox
      Left = 482
      Top = 122
      Width = 101
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemIndex = 0
      TabOrder = 4
      Text = 'Keys First'
      Items.Strings = (
        'Keys First'
        'Partial Burst'
        'Full Burst')
    end
    object FetchAllCheckBox: TCheckBox
      Left = 102
      Top = 123
      Width = 64
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Fetch All'
      TabOrder = 2
    end
    object StatementCacheCheckBox: TCheckBox
      Left = 185
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
    Width = 745
    Height = 132
    ActivePage = ResultTabSheet
    Align = alClient
    TabOrder = 2
    object ResultTabSheet: TTabSheet
      Caption = 'Result'
      object ResultGrid: TDBGrid
        Left = 0
        Top = 0
        Width = 737
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
      object TranslatedQueryMemo: TMemo
        Left = 0
        Top = 0
        Width = 737
        Height = 104
        Align = alClient
        TabOrder = 0
      end
    end
    object StatsTabSheet: TTabSheet
      Caption = 'Stats'
      ImageIndex = 2
      object StatsMemo: TMemo
        Left = 0
        Top = 0
        Width = 737
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
