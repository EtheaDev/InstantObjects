object BasicBrowseForm: TBasicBrowseForm
  Left = 379
  Top = 254
  Width = 257
  Height = 319
  BorderIcons = [biSystemMenu]
  Caption = 'Browse'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 266
    Width = 249
    Height = 19
    Panels = <>
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 249
    Height = 26
    AutoSize = True
    BorderWidth = 1
    EdgeBorders = []
    Flat = True
    Images = ActionImages
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnResize = ToolBarResize
    object NewButton: TToolButton
      Left = 0
      Top = 0
      Action = NewAction
    end
    object EditButton: TToolButton
      Left = 23
      Top = 0
      Action = EditAction
    end
    object DeleteButton: TToolButton
      Left = 46
      Top = 0
      Action = DeleteAction
    end
    object SelectButton: TToolButton
      Left = 69
      Top = 0
      Action = SelectAction
    end
    object ToolSep1: TToolButton
      Left = 92
      Top = 0
      Width = 8
      ImageIndex = 3
      Style = tbsSeparator
    end
    object SearchEdit: TEdit
      Left = 100
      Top = 0
      Width = 69
      Height = 22
      TabOrder = 0
    end
  end
  object BrowseGridPanel: TPanel
    Left = 0
    Top = 26
    Width = 249
    Height = 240
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 2
    object BrowseGrid: TDBGrid
      Left = 1
      Top = 1
      Width = 247
      Height = 238
      Align = alClient
      DataSource = BrowseSource
      Options = [dgEditing, dgTitles, dgIndicator, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
      PopupMenu = GridMenu
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnDblClick = BrowseGridDblClick
    end
  end
  object ActionList: TActionList
    Images = ActionImages
    OnUpdate = ActionListUpdate
    Left = 24
    Top = 58
    object NewAction: TAction
      Caption = '&New'
      Hint = 'New'
      ImageIndex = 0
      ShortCut = 16462
      OnExecute = NewActionExecute
    end
    object EditAction: TAction
      Caption = '&Edit'
      Hint = 'Edit'
      ImageIndex = 1
      ShortCut = 16453
      OnExecute = EditActionExecute
    end
    object DeleteAction: TAction
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 2
      ShortCut = 16452
      OnExecute = DeleteActionExecute
    end
    object SelectAction: TAction
      Caption = '&Select'
      Hint = 'Select'
      ImageIndex = 3
      OnExecute = SelectActionExecute
    end
  end
  object GridMenu: TPopupMenu
    Images = ActionImages
    Left = 24
    Top = 90
    object SelectItem: TMenuItem
      Action = SelectAction
    end
    object NewItem: TMenuItem
      Action = NewAction
    end
    object EditItem: TMenuItem
      Action = EditAction
    end
    object DeleteItem: TMenuItem
      Action = DeleteAction
    end
  end
  object ActionImages: TImageList
    Left = 56
    Top = 58
  end
  object BrowseSource: TDataSource
    Left = 56
    Top = 90
  end
end
