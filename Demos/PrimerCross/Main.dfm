object MainForm: TMainForm
  Left = 254
  Top = 225
  Caption = 'InstantObjects Primer'
  ClientHeight = 618
  ClientWidth = 1067
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object SideBarSplitter: TSplitter
    Left = 88
    Top = 26
    Height = 573
    ResizeStyle = rsLine
    ExplicitHeight = 139
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 1063
    Height = 26
    AutoSize = True
    BorderWidth = 1
    Images = ActionImages
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ConnectionManagerButton: TToolButton
      Left = 0
      Top = 0
      Action = ConnectionManagerAction
    end
    object RandomDataButton: TToolButton
      Left = 23
      Top = 0
      Action = RandomDataAction
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 598
    Width = 1063
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Alignment = taRightJustify
        Width = 50
      end>
  end
  object SideBarPanel: TPanel
    Left = 0
    Top = 26
    Width = 88
    Height = 572
    Align = alLeft
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clAppWorkSpace
    TabOrder = 2
    object SideBarTopSpacer: TBevel
      Left = 0
      Top = 0
      Width = 84
      Height = 8
      Align = alTop
      Shape = bsSpacer
    end
    object SideBar: TListView
      Left = 0
      Top = 8
      Width = 84
      Height = 560
      Align = alClient
      BorderStyle = bsNone
      Color = clAppWorkSpace
      Columns = <>
      IconOptions.AutoArrange = True
      ReadOnly = True
      TabOrder = 0
      OnSelectItem = SideBarSelectItem
    end
  end
  object WorkPanel: TPanel
    Left = 91
    Top = 26
    Width = 972
    Height = 572
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object WorkTitleSpacer: TBevel
      Left = 0
      Top = 27
      Width = 976
      Height = 4
      Align = alTop
      Shape = bsSpacer
      ExplicitWidth = 213
    end
    object WorkTitlePanel: TPanel
      Left = 0
      Top = 0
      Width = 972
      Height = 27
      Align = alTop
      BevelInner = bvLowered
      Color = clGray
      TabOrder = 0
      object WorkTitleLabel: TLabel
        Left = 6
        Top = 5
        Width = 22
        Height = 13
        Caption = 'Title'
      end
    end
    object WorkClientPanel: TPanel
      Left = 0
      Top = 31
      Width = 972
      Height = 541
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object SideBarImages: TImageList
    Height = 36
    Width = 36
    Left = 147
    Top = 276
  end
  object MainMenu: TMainMenu
    Images = ActionImages
    Left = 147
    Top = 172
    object FileMenu: TMenuItem
      Caption = '&File'
      object ConnectionManagerItem: TMenuItem
        Action = ConnectionManagerAction
      end
      object RandomDataItem: TMenuItem
        Action = RandomDataAction
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ExportItem: TMenuItem
        Caption = '&Export all Contacts...'
        OnClick = ExportItemClick
      end
      object ImportItem: TMenuItem
        Caption = '&Import Contacts from file...'
        OnClick = ImportItemClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object ExportModelItem: TMenuItem
        Caption = 'Export model...'
        OnClick = ExportModelItemClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitItemClick
      end
    end
  end
  object ActionList: TActionList
    Images = ActionImages
    OnUpdate = ActionListUpdate
    Left = 147
    Top = 220
    object ConnectionManagerAction: TAction
      Caption = '&Connection Manager'
      Hint = 'Connection Manager'
      ImageIndex = 0
      ShortCut = 24643
      OnExecute = ConnectionManagerActionExecute
    end
    object RandomDataAction: TAction
      Caption = 'Create &Random Data...'
      Hint = 'Create Random Data'
      ImageIndex = 1
      OnExecute = RandomDataActionExecute
    end
  end
  object ActionImages: TImageList
    Left = 147
    Top = 332
  end
  object ConnectionManager: TInstantConnectionManager
    FileFormat = sfXML
    OnConnect = ConnectionManagerConnect
    OnDisconnect = ConnectionManagerDisconnect
    OnIsConnected = ConnectionManagerIsConnected
    OnPrepare = ConnectionManagerPrepare
    Left = 147
    Top = 118
  end
end
