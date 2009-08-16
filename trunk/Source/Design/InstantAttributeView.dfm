object InstantAttributeViewFrame: TInstantAttributeViewFrame
  Left = 0
  Top = 0
  Width = 376
  Height = 188
  TabOrder = 0
  object AttributesSplitter: TSplitter
    Left = 0
    Top = 84
    Width = 376
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    Constraints.MinHeight = 4
  end
  object InheritedAttributesPanel: TPanel
    Left = 0
    Top = 88
    Width = 376
    Height = 100
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 60
    TabOrder = 1
    object InheritedAttributesLabel: TLabel
      Left = 0
      Top = 0
      Width = 376
      Height = 16
      Align = alTop
      AutoSize = False
      Caption = 'Inherited'
    end
    object InheritedAttributesView: TListView
      Left = 0
      Top = 16
      Width = 376
      Height = 84
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 124
        end
        item
          Caption = 'Type'
          Width = 124
        end
        item
          Caption = 'Storage Name'
          Width = 124
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ReadOnly = True
      ParentFont = False
      PopupMenu = AttributesMenu
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object IntroducedAttributesPanel: TPanel
    Left = 0
    Top = 0
    Width = 376
    Height = 84
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 60
    TabOrder = 0
    object IntroducedAttributesLabel: TLabel
      Left = 0
      Top = 0
      Width = 376
      Height = 16
      Align = alTop
      AutoSize = False
      Caption = 'Introduced'
    end
    object IntroducedAttributesView: TListView
      Left = 0
      Top = 16
      Width = 376
      Height = 68
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 124
        end
        item
          Caption = 'Type'
          Width = 124
        end
        item
          Caption = 'Storage Name'
          Width = 124
        end>
      ReadOnly = True
      PopupMenu = AttributesMenu
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = IntroducedAttributesViewDblClick
    end
  end
  object SubjectSource: TDataSource
    Left = 76
    Top = 132
  end
  object AttributeImages: TImageList
    Left = 108
    Top = 132
  end
  object StateImages: TImageList
    Left = 140
    Top = 132
  end
  object AttributesMenu: TPopupMenu
    Images = ActionImages
    OnPopup = AttributesMenuPopup
    Left = 204
    Top = 132
    object AttributeNewItem: TMenuItem
      Action = AttributeNewAction
      ShortCut = 45
    end
    object AttributeDeleteItem: TMenuItem
      Action = AttributeDeleteAction
      ShortCut = 46
    end
    object AttributeEditItem: TMenuItem
      Action = AttributeEditAction
      ShortCut = 32781
    end
  end
  object Actions: TActionList
    Images = ActionImages
    Left = 236
    Top = 132
    object AttributeNewAction: TAction
      Caption = '&New'
      Hint = 'New Attribute'
      ImageIndex = 0
      OnExecute = AttributeNewActionExecute
    end
    object AttributeDeleteAction: TAction
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 1
      OnExecute = AttributeDeleteActionExecute
    end
    object AttributeEditAction: TAction
      Caption = '&Edit'
      Hint = 'Edit'
      ImageIndex = 2
      OnExecute = AttributeEditActionExecute
    end
  end
  object ActionImages: TImageList
    Left = 172
    Top = 132
  end
end
