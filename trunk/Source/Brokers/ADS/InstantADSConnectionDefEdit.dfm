object InstantADSConnectionDefEditForm: TInstantADSConnectionDefEditForm
  Left = 324
  Top = 263
  BorderStyle = bsDialog
  Caption = 'ADS Connection'
  ClientHeight = 234
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 197
    Width = 362
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 197
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object DatabaseLabel: TLabel
      Left = 16
      Top = 16
      Width = 46
      Height = 13
      Caption = '&Database'
      FocusControl = DatabaseEdit
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 144
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object DatabaseEdit: TComboBox
      Left = 16
      Top = 32
      Width = 280
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnDropDown = DatabaseEditDropDown
    end
    object DirectoryButton: TBitBtn
      Left = 301
      Top = 32
      Width = 21
      Height = 21
      Hint = 'Database Directory'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = DirectoryButtonClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555555555555555555555555555555550000000000555550BFBFBFBFB
        0555550FBFBFBFBF0555550BFBFBFBFB0555550FBFBFBFBF0555550BFBFBFBFB
        0555550FBFBFBFBF055555000000000055555550FBFB05555555555700007555
        5555555555555555555555555555555555555555555555555555}
    end
    object DictionaryButton: TBitBtn
      Left = 325
      Top = 32
      Width = 21
      Height = 21
      Hint = 'Data Dictionary'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = DictionaryButtonClick
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333C33333333333333C0C333333333333C0F0C3333333333C0FFF0C33
        333333CCC0FFF0C333333CCCCC0FFF0C33334CCCCCC0FFF0C333C4CCCCCC0FFF
        0C333C4CCCCCC0FFF0C333C4CCC0CC0F0C33333C4C0CCCC033333333C4CCCCCC
        333333333C4CCCC33333333333C4C33333333333333333333333}
    end
    object ConnectionTypeGroupBox: TGroupBox
      Left = 16
      Top = 64
      Width = 113
      Height = 73
      Caption = 'Connection Type'
      TabOrder = 3
      object LocalCheckBox: TCheckBox
        Left = 8
        Top = 16
        Width = 81
        Height = 17
        Caption = '&Local'
        TabOrder = 0
      end
      object RemoteCheckBox: TCheckBox
        Left = 8
        Top = 32
        Width = 81
        Height = 17
        Caption = '&Remote'
        TabOrder = 1
      end
      object InternetCheckBox: TCheckBox
        Left = 8
        Top = 48
        Width = 81
        Height = 17
        Caption = '&Internet'
        TabOrder = 2
      end
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 160
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 4
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 199
    Width = 362
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 204
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 284
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
