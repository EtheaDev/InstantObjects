object InstantUIBConnectionDefEditForm: TInstantUIBConnectionDefEditForm
  Left = 914
  Top = 281
  BorderStyle = bsDialog
  Caption = 'InterBase Connection'
  ClientHeight = 399
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
    Top = 362
    Width = 362
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 362
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ServerLabel: TLabel
      Left = 16
      Top = 36
      Width = 31
      Height = 13
      Caption = '&Server'
      FocusControl = ServerEdit
    end
    object ProtocolLabel: TLabel
      Left = 235
      Top = 36
      Width = 39
      Height = 13
      Caption = '&Protocol'
      FocusControl = ProtocolEdit
    end
    object DatabaseLabel: TLabel
      Left = 16
      Top = 76
      Width = 46
      Height = 13
      Caption = '&Database'
      FocusControl = DatabaseEdit
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 312
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object ParamsLabel: TLabel
      Left = 16
      Top = 140
      Width = 95
      Height = 13
      Caption = 'Connection &Settings'
      FocusControl = ParamsEditor
    end
    object Label1: TLabel
      Left = 136
      Top = 312
      Width = 62
      Height = 13
      Caption = 'Id Data Type'
      FocusControl = IdDataTypeComboBox
    end
    object Label2: TLabel
      Left = 256
      Top = 312
      Width = 32
      Height = 13
      Caption = 'Id Size'
      FocusControl = IdDataTypeComboBox
    end
    object LocalRadioButton: TRadioButton
      Left = 16
      Top = 16
      Width = 57
      Height = 17
      Caption = '&Local'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = LocalRemoteChange
    end
    object RemoteRadioButton: TRadioButton
      Left = 86
      Top = 16
      Width = 65
      Height = 17
      Caption = '&Remote'
      TabOrder = 1
      OnClick = LocalRemoteChange
    end
    object ServerEdit: TEdit
      Left = 16
      Top = 52
      Width = 209
      Height = 21
      TabOrder = 2
    end
    object ProtocolEdit: TComboBox
      Left = 235
      Top = 52
      Width = 110
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        'TCP/IP'
        'NetBEUI'
        'SPX')
    end
    object DatabaseEdit: TEdit
      Left = 16
      Top = 92
      Width = 305
      Height = 21
      TabOrder = 4
    end
    object DatabaseButton: TButton
      Left = 321
      Top = 92
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = DatabaseButtonClick
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 328
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 9
    end
    object UseDelimitedIdentsCheckBox: TCheckBox
      Left = 16
      Top = 118
      Width = 150
      Height = 17
      Caption = '&Use delimited identifiers'
      TabOrder = 6
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 196
      Top = 118
      Width = 150
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 7
    end
    object ParamsEditor: TMemo
      Left = 17
      Top = 160
      Width = 328
      Height = 149
      TabOrder = 8
    end
    object IdDataTypeComboBox: TComboBox
      Left = 136
      Top = 328
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 10
    end
    object IdSizeEdit: TEdit
      Left = 256
      Top = 328
      Width = 89
      Height = 21
      TabOrder = 11
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 364
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
