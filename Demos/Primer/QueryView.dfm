inherited QueryViewForm: TQueryViewForm
  Height = 320
  Caption = 'Query'
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 144
    Width = 417
    Height = 6
    Cursor = crVSplit
    Align = alTop
  end
  object CommandPanel: TPanel
    Left = 0
    Top = 0
    Width = 417
    Height = 144
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object CommandLabel: TLabel
      Left = 8
      Top = 8
      Width = 47
      Height = 13
      Caption = '&Command'
      FocusControl = CommandEdit
    end
    object ExampleLabel: TLabel
      Left = 105
      Top = 124
      Width = 40
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'E&xample'
      FocusControl = ExampleComboBox
    end
    object CommandEdit: TMemo
      Left = 8
      Top = 24
      Width = 401
      Height = 89
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object ExecuteButton: TButton
      Left = 8
      Top = 119
      Width = 75
      Height = 25
      Action = ExecuteAction
      Anchors = [akLeft, akBottom]
      TabOrder = 2
    end
    object ExampleComboBox: TComboBox
      Left = 153
      Top = 121
      Width = 256
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
      ItemHeight = 13
      TabOrder = 1
      OnClick = ExampleComboBoxClick
    end
  end
  object ResultPanel: TPanel
    Left = 0
    Top = 150
    Width = 417
    Height = 143
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object ResultGrid: TDBGrid
      Left = 8
      Top = 0
      Width = 401
      Height = 135
      Anchors = [akLeft, akTop, akRight, akBottom]
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
    Left = 368
    Top = 40
    object ExecuteAction: TAction
      Caption = '&Execute'
      Hint = 'Execute'
      OnExecute = ExecuteActionExecute
    end
  end
end
