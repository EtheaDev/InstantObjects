inherited QueryViewForm: TQueryViewForm
  Width = 772
  Height = 320
  object Splitter: TSplitter
    Left = 0
    Top = 147
    Width = 772
    Height = 6
    Cursor = crVSplit
    Align = alTop
  end
  object CommandPanel: TPanel
    Left = 0
    Top = 0
    Width = 772
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
    object CommandEdit: TMemo
      Left = 30
      Top = 30
      Width = 712
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
      Width = 461
      Height = 21
      Style = csDropDownList
      DropDownCount = 11
      ItemHeight = 13
      TabOrder = 1
      OnClick = ExampleComboBoxClick
    end
    object MaxCountEdit: TMaskEdit
      Left = 695
      Top = 122
      Width = 44
      Height = 21
      EditMask = '#########;1; '
      MaxLength = 9
      TabOrder = 3
      Text = '0        '
    end
  end
  object ResultPageControl: TPageControl
    Left = 0
    Top = 153
    Width = 772
    Height = 167
    ActivePage = ResultTabSheet
    Align = alClient
    TabOrder = 1
    object ResultTabSheet: TTabSheet
      Caption = 'Result'
      object ResultGrid: TDBGrid
        Left = 0
        Top = 0
        Width = 764
        Height = 139
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
    object TranslatedQueryTabSheet: TTabSheet
      Caption = 'Translated Query'
      ImageIndex = 1
      object TranslatedQueryMemo: TMemo
        Left = 0
        Top = 0
        Width = 764
        Height = 139
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object TestSelector: TInstantSelector
    AfterScroll = TestSelectorAfterScroll
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
