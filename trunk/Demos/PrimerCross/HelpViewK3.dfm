inherited HelpViewForm: THelpViewForm
  Width = 486
  VertScrollBar.Range = 37
  Font.Height = 19
  Font.Name = 'adobe-helvetica'
  ParentFont = False
  object TextBrowser: TTextBrowser
    Left = 0
    Top = 37
    Width = 486
    Height = 287
    Align = alClient
    TabOrder = 0
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 486
    Height = 37
    ButtonHeight = 29
    ButtonWidth = 102
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 1
    object ToolButton1: TToolButton
      HelpType = htContext
      Left = 1
      Top = 4
      Height = 29
      Action = acIndex
      Caption = 'Index page'
    end
  end
  object ActionList: TActionList
    Left = 200
    Top = 80
    object acIndex: TAction
      Caption = 'Index page'
      Hint = 'Go to index page'
      OnExecute = acIndexExecute
    end
  end
end
