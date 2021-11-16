object BasicBrowseForm: TBasicBrowseForm
  Left = 379
  Top = 254
  BorderIcons = [biSystemMenu]
  Caption = 'Browse'
  ClientHeight = 280
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
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
    Top = 261
    Width = 241
    Height = 19
    Panels = <>
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 241
    Height = 26
    AutoSize = True
    BorderWidth = 1
    Images = ActionImages
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnResize = ToolBarResize
    object SearchEdit: TEdit
      Left = 0
      Top = 0
      Width = 111
      Height = 22
      TabOrder = 0
    end
    object NewButton: TToolButton
      Left = 111
      Top = 0
      Action = NewAction
    end
    object EditButton: TToolButton
      Left = 134
      Top = 0
      Action = EditAction
    end
    object DeleteButton: TToolButton
      Left = 157
      Top = 0
      Action = DeleteAction
    end
    object SelectButton: TToolButton
      Left = 180
      Top = 0
      Action = SelectAction
    end
    object ToolSep1: TToolButton
      Left = 203
      Top = 0
      Width = 8
      ImageIndex = 3
      Style = tbsSeparator
    end
    object SearchButton: TToolButton
      Left = 211
      Top = 0
      Action = SearchAction
    end
    object ToolSep2: TToolButton
      Left = 0
      Top = 0
      Width = 8
      ImageIndex = 4
      Wrap = True
      Style = tbsSeparator
    end
  end
  object BrowseGridPanel: TPanel
    Left = 0
    Top = 26
    Width = 241
    Height = 235
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 2
    object BrowseGrid: TDBGrid
      Left = 1
      Top = 1
      Width = 239
      Height = 233
      Align = alClient
      DataSource = BrowseSource
      Options = [dgEditing, dgTitles, dgIndicator, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
      PopupMenu = GridMenu
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
      OnDrawColumnCell = DbGridDrawColumnCellFixW11
      OnDblClick = BrowseGridDblClick
      OnKeyUp = BrowseGridKeyUp
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
    object SearchAction: TAction
      Caption = '&Search'
      ImageIndex = 4
      OnExecute = SearchActionExecute
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
