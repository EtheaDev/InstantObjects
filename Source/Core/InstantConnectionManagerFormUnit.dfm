object InstantConnectionManagerForm: TInstantConnectionManagerForm
  Left = 396
  Top = 280
  BorderIcons = [biSystemMenu]
  Caption = 'Connection Manager'
  ClientHeight = 278
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000070444000000088FF844444000077888F44444440007
    888FF444444400077888F44F44440007888FFF44444000077888F8F444000007
    888FFF88700000077888F8870000000780000087000000000FFFFF000000000F
    FFFFFFFF000000000FFFFF00000000000000000000000000000000000000FFFF
    0000F8230000E0010000C0000000C0000000C0000000C0010000C0030000C007
    0000C0070000C0070000C0070000C0070000E00F0000F83F0000FFFF0000}
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object ConnectionView: TListView
    Left = 0
    Top = 0
    Width = 431
    Height = 246
    Align = alClient
    Columns = <
      item
        Caption = 'Connection'
        Width = 320
      end
      item
        Caption = 'Type'
        Width = 100
      end>
    PopupMenu = ConnectionMenu
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ConnectionViewDblClick
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 246
    Width = 431
    Height = 32
    Align = alBottom
    TabOrder = 1
    object BuildButton: TButton
      Left = 4
      Top = 4
      Width = 75
      Height = 25
      Action = BuildAction
      TabOrder = 0
    end
    object ButtonsPanel: TPanel
      Left = 269
      Top = 1
      Width = 161
      Height = 30
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object ConnectButton: TButton
        Left = 4
        Top = 3
        Width = 75
        Height = 25
        Action = ConnectAction
        Default = True
        TabOrder = 0
      end
      object CloseButton: TButton
        Left = 82
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Close'
        ModalResult = 2
        TabOrder = 1
      end
    end
    object EvolveButton: TButton
      Left = 82
      Top = 4
      Width = 75
      Height = 25
      Action = EvolveAction
      TabOrder = 2
    end
  end
  object ConnectionImages: TImageList
    Left = 16
    Top = 96
  end
  object ConnectionMenu: TPopupMenu
    Left = 16
    Top = 64
    object NewMenu: TMenuItem
      Caption = '&New'
    end
    object EditItem: TMenuItem
      Action = EditAction
    end
    object RenameItem: TMenuItem
      Action = RenameAction
    end
    object DeleteItem: TMenuItem
      Action = DeleteAction
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object BuildItem: TMenuItem
      Action = BuildAction
    end
    object EvolveItem: TMenuItem
      Action = EvolveAction
    end
    object ConnectItem: TMenuItem
      Action = ConnectAction
    end
    object DisconnectItem: TMenuItem
      Action = DisconnectAction
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Open1: TMenuItem
      Action = FileOpenAction
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 16
    Top = 32
    object EditAction: TAction
      Caption = '&Edit'
      Hint = 'Edit'
      ShortCut = 16453
      OnExecute = EditActionExecute
    end
    object RenameAction: TAction
      Caption = '&Rename'
      Hint = 'Rename'
      ShortCut = 113
      OnExecute = RenameActionExecute
    end
    object DeleteAction: TAction
      Caption = '&Delete'
      Hint = 'Delete'
      ShortCut = 16452
      OnExecute = DeleteActionExecute
    end
    object EvolveAction: TAction
      Caption = 'E&volve'
      Hint = 'Evolve'
      OnExecute = EvolveActionExecute
    end
    object BuildAction: TAction
      Caption = '&Build'
      Hint = 'Build'
      OnExecute = BuildActionExecute
    end
    object ConnectAction: TAction
      Caption = '&Connect'
      Hint = 'Connect'
      OnExecute = ConnectActionExecute
      OnUpdate = ConnectActionUpdate
    end
    object DisconnectAction: TAction
      Caption = '&Disconnect'
      Hint = 'Disconnect'
      OnExecute = DisconnectActionExecute
      OnUpdate = DisconnectActionUpdate
    end
    object FileOpenAction: TAction
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open configuration file'
      ImageIndex = 7
      ShortCut = 16463
      OnExecute = FileOpenActionExecute
    end
  end
end
