object InstantNexusDbConnectionDefEditForm: TInstantNexusDbConnectionDefEditForm
  Left = 265
  Top = 262
  BorderStyle = bsDialog
  Caption = '  NexusDb Connection'
  ClientHeight = 317
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
  PixelsPerInch = 96
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 280
    Width = 393
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 393
    Height = 280
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object AliasLabel: TLabel
      Left = 16
      Top = 184
      Width = 22
      Height = 13
      Caption = '&Path'
    end
    object Label1: TLabel
      Left = 160
      Top = 4
      Width = 22
      Height = 13
      Caption = '&Alias'
      FocusControl = lbAlias
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 233
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object BrowseButton: TButton
      Left = 356
      Top = 204
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 0
      OnClick = BrowseButtonClick
    end
    object lbAlias: TListBox
      Left = 160
      Top = 20
      Width = 217
      Height = 161
      ItemHeight = 13
      TabOrder = 1
    end
    object rgSelDb: TRadioGroup
      Left = 16
      Top = 16
      Width = 133
      Height = 73
      Caption = '  Database Selection  '
      ItemIndex = 0
      Items.Strings = (
        'Alias'
        'Path')
      TabOrder = 2
    end
    object ePath: TEdit
      Left = 16
      Top = 204
      Width = 333
      Height = 21
      TabOrder = 3
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 249
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
    Top = 282
    Width = 393
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      393
      35)
    object OkButton: TButton
      Left = 195
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
