inherited InstantDualListForm: TInstantDualListForm
  Left = 310
  Caption = 'Dual List'
  ClientHeight = 259
  ClientWidth = 377
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonPanel: TPanel
    Top = 218
    Width = 377
    inherited ButtonBevel: TBevel
      Width = 377
    end
    object OkButton: TButton
      Left = 218
      Top = 10
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 298
      Top = 10
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 377
    Height = 218
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object LeftPanel: TPanel
      Left = 4
      Top = 4
      Width = 165
      Height = 210
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object LeftView: TListView
        Left = 0
        Top = 0
        Width = 165
        Height = 210
        Align = alClient
        Columns = <
          item
            Caption = 'Include'
            Width = -2
            WidthType = (
              -2)
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = LeftViewDblClick
      end
    end
    object RightPanel: TPanel
      Left = 208
      Top = 4
      Width = 165
      Height = 210
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object RightView: TListView
        Left = 0
        Top = 0
        Width = 165
        Height = 210
        Align = alClient
        Columns = <
          item
            Caption = 'Exclude'
            Width = -2
            WidthType = (
              -2)
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = RightViewDblClick
      end
    end
    object CenterPanel: TPanel
      Left = 169
      Top = 4
      Width = 39
      Height = 210
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object RightButton: TButton
        Left = 7
        Top = 24
        Width = 25
        Height = 25
        Action = RightAction
        TabOrder = 0
      end
      object LeftButton: TButton
        Left = 7
        Top = 56
        Width = 25
        Height = 25
        Action = LeftAction
        TabOrder = 1
      end
      object AllRightButton: TButton
        Left = 7
        Top = 88
        Width = 25
        Height = 25
        Action = AllRightAction
        TabOrder = 2
      end
      object AllLeftButton: TButton
        Left = 7
        Top = 120
        Width = 25
        Height = 25
        Action = AllLeftAction
        TabOrder = 3
      end
    end
  end
  object Actions: TActionList
    Left = 5
    Top = 227
    object RightAction: TAction
      Caption = '>'
      OnExecute = RightActionExecute
    end
    object LeftAction: TAction
      Caption = '<'
      OnExecute = LeftActionExecute
    end
    object AllRightAction: TAction
      Caption = '>>'
      OnExecute = AllRightActionExecute
    end
    object AllLeftAction: TAction
      Caption = '<<'
      OnExecute = AllLeftActionExecute
    end
  end
end
