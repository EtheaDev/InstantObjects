object InstantNexusDbSQLConnectionDefEditForm: TInstantNexusDbSQLConnectionDefEditForm
  Left = 257
  Top = 296
  BorderStyle = bsDialog
  Caption = '  NexusDbSQL Connection'
  ClientHeight = 309
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    3333333000333333333333330000000030000000700000000000000300000000
    3333333000333333333333330000000033333333033333333333333300000000
    3333333303333333333333330000000033333300007744473333333300000000
    33330088FF8444447333333300000000333077888F4444444333333300000000
    33307888FF4444444333333300000000333077888F44F4444333333300000000
    33307888FF8444447333333300000000333077888F8844473333333300000000
    33307888FFF887333333333300000000333077888F8870333333333300000000
    33307800000870333333333300000000333000FFF77777777777777700000000
    3330FFFFF7FFFFFFFFFFFFF700000000333300FFF7FF0FF0F0FF0FF700000000
    3333330007FF0F00F0FF0FF7000000003333333337FF0870FF00FFF700000000
    3333333337FF00F0F0FF0FF7000000003333333337FF0FF0F0FF0FF700000000
    3333333337FFFFFFFFFFFFF70000000033333333377777777777777700000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000
    000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000
    000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000
    000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 272
    Width = 393
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 393
    Height = 272
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PathLabel: TLabel
      Left = 16
      Top = 184
      Width = 22
      Height = 13
      Caption = '&Path'
    end
    object AliasLabel: TLabel
      Left = 180
      Top = 4
      Width = 22
      Height = 13
      Caption = '&Alias'
      FocusControl = lbAlias
    end
    object ServerLabel: TLabel
      Left = 12
      Top = 100
      Width = 60
      Height = 13
      Caption = '&Server name'
      FocusControl = ServerComboBox
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 233
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object lblIdDataType: TLabel
      Left = 168
      Top = 233
      Width = 62
      Height = 13
      Caption = 'Id Data Type'
      FocusControl = IdDataTypeComboBox
    end
    object lblIdSize: TLabel
      Left = 288
      Top = 233
      Width = 32
      Height = 13
      Caption = 'Id Size'
      FocusControl = IdDataTypeComboBox
    end
    object BrowseButton: TButton
      Left = 356
      Top = 204
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 4
      OnClick = BrowseButtonClick
    end
    object lbAlias: TListBox
      Left = 180
      Top = 20
      Width = 197
      Height = 161
      ItemHeight = 13
      TabOrder = 2
    end
    object rgSelDb: TRadioGroup
      Left = 12
      Top = 16
      Width = 157
      Height = 73
      Caption = '&Database Selection  '
      ItemIndex = 0
      Items.Strings = (
        'Alias'
        'Path')
      TabOrder = 0
      OnClick = rgSelDbClick
    end
    object ePath: TEdit
      Left = 16
      Top = 204
      Width = 333
      Height = 21
      TabOrder = 3
    end
    object ServerComboBox: TComboBox
      Left = 12
      Top = 116
      Width = 157
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      OnDropDown = ServerComboBoxDropDown
      OnExit = ServerComboBoxLoadAlias
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 249
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 5
    end
    object IdDataTypeComboBox: TComboBox
      Left = 168
      Top = 249
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
    end
    object IdSizeEdit: TEdit
      Left = 288
      Top = 249
      Width = 89
      Height = 21
      TabOrder = 7
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 274
    Width = 393
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 223
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
      Left = 303
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
