object InstantUIBConnectionDefEditForm: TInstantUIBConnectionDefEditForm
  Left = 320
  Top = 199
  BorderStyle = bsDialog
  Caption = 'UIB Connection'
  ClientHeight = 329
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
    Top = 292
    Width = 362
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 292
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ConnectionStringLabel: TLabel
      Left = 16
      Top = 12
      Width = 84
      Height = 13
      Caption = '&Connection String'
      FocusControl = ConnectionStringEdit
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 248
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object ParamsLabel: TLabel
      Left = 16
      Top = 76
      Width = 95
      Height = 13
      Caption = 'Connection &Settings'
      FocusControl = ParamsEditor
    end
    object Label1: TLabel
      Left = 136
      Top = 248
      Width = 62
      Height = 13
      Caption = 'Id Data Type'
      FocusControl = IdDataTypeComboBox
    end
    object Label2: TLabel
      Left = 256
      Top = 248
      Width = 32
      Height = 13
      Caption = 'Id Size'
      FocusControl = IdDataTypeComboBox
    end
    object ConnectionStringEdit: TEdit
      Left = 16
      Top = 28
      Width = 305
      Height = 21
      TabOrder = 0
    end
    object ConnectionStringButton: TButton
      Left = 321
      Top = 28
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = ConnectionStringButtonClick
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 264
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 5
    end
    object UseDelimitedIdentsCheckBox: TCheckBox
      Left = 16
      Top = 54
      Width = 150
      Height = 17
      Caption = '&Use delimited identifiers'
      TabOrder = 2
    end
    object ParamsEditor: TMemo
      Left = 17
      Top = 96
      Width = 328
      Height = 149
      TabOrder = 4
    end
    object IdDataTypeComboBox: TComboBox
      Left = 136
      Top = 264
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
    end
    object IdSizeEdit: TEdit
      Left = 256
      Top = 264
      Width = 89
      Height = 21
      TabOrder = 7
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 196
      Top = 54
      Width = 150
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 3
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 294
    Width = 362
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      362
      35)
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
