object InstantFlashFilerConnectionDefEditForm: TInstantFlashFilerConnectionDefEditForm
  Left = 324
  Top = 263
  BorderStyle = bsDialog
  Caption = 'FlashFiler Connection'
  ClientHeight = 183
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
    Top = 146
    Width = 362
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 146
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object AliasLabel: TLabel
      Left = 16
      Top = 16
      Width = 49
      Height = 13
      Caption = '&Alias/Path'
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 61
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object AliasEdit: TComboBox
      Left = 16
      Top = 32
      Width = 305
      Height = 21
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnDropDown = AliasEditDropDown
    end
    object BrowseButton: TButton
      Left = 324
      Top = 32
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = BrowseButtonClick
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 77
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 2
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 148
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
