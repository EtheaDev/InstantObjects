inherited QueryViewForm: TQueryViewForm
  Height = 320
  object Splitter: TSplitter
    Left = 0
    Top = 147
    Width = 425
    Height = 6
    Cursor = crVSplit
    Align = alTop
  end
  object CommandPanel: TPanel
    Left = 0
    Top = 0
    Width = 425
    Height = 147
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 30
    TabOrder = 0
    object CommandLabel: TLabel
      Left = 32
      Top = 16
      Width = 47
      Height = 13
      Caption = '&Command'
      FocusControl = CommandEdit
    end
    object ExampleLabel: TLabel
      Left = 125
      Top = 124
      Width = 40
      Height = 13
      Caption = 'E&xample'
      FocusControl = ExampleComboBox
    end
    object CommandEdit: TMemo
      Left = 30
      Top = 30
      Width = 365
      Height = 87
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object ExecuteButton: TButton
      Left = 30
      Top = 119
      Width = 75
      Height = 25
      Action = ExecuteAction
      TabOrder = 2
    end
    object ExampleComboBox: TComboBox
      Left = 168
      Top = 121
      Width = 221
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnClick = ExampleComboBoxClick
    end
  end
  object ResultPanel: TPanel
    Left = 0
    Top = 153
    Width = 425
    Height = 167
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object ResultGrid: TDBGrid
      Left = 0
      Top = 0
      Width = 425
      Height = 167
      Align = alClient
      DataSource = TestSource
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  object TestSelector: TInstantSelector
    AfterScroll = TestSelectorAfterScroll
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