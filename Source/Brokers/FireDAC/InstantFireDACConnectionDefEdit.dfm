object InstantFireDACConnectionDefEditForm: TInstantFireDACConnectionDefEditForm
  Left = 214
  Top = 165
  BorderStyle = bsDialog
  Caption = 'FireDAC Connection'
  ClientHeight = 345
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 308
    Width = 372
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 372
    Height = 308
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object HostNameLabel: TLabel
      Left = 7
      Top = 16
      Width = 56
      Height = 13
      Caption = '&Host Name'
      FocusControl = HostNameEdit
    end
    object PortLabel: TLabel
      Left = 143
      Top = 16
      Width = 87
      Height = 13
      Caption = '&Port (0 = default)'
      FocusControl = PortEdit
    end
    object ProtocolLabel: TLabel
      Left = 231
      Top = 16
      Width = 46
      Height = 13
      Caption = '&Protocol:'
      FocusControl = ProtocolComboBox
    end
    object DatabaseLabel: TLabel
      Left = 7
      Top = 56
      Width = 48
      Height = 13
      Caption = '&Database'
      FocusControl = DatabaseEdit
    end
    object CatalogLabel: TLabel
      Left = 231
      Top = 56
      Width = 40
      Height = 13
      Caption = '&Catalog'
      FocusControl = CatalogComboBox
    end
    object UserNameLabel: TLabel
      Left = 7
      Top = 96
      Width = 54
      Height = 13
      Caption = '&User name'
      FocusControl = UserNameEdit
    end
    object PasswordLabel: TLabel
      Left = 159
      Top = 96
      Width = 49
      Height = 13
      Caption = '&Password'
      FocusControl = PasswordEdit
    end
    object PropertiesLabel: TLabel
      Left = 7
      Top = 144
      Width = 55
      Height = 13
      Caption = 'Pr&operties:'
      FocusControl = PropertiesEditor
    end
    object StreamFormatLabel: TLabel
      Left = 207
      Top = 144
      Width = 60
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object IdDataTypeLabel: TLabel
      Left = 207
      Top = 184
      Width = 63
      Height = 13
      Caption = 'Id Data Type'
      FocusControl = IdDataTypeComboBox
    end
    object IdSizeLabel: TLabel
      Left = 207
      Top = 224
      Width = 33
      Height = 13
      Caption = 'Id Size'
      FocusControl = IdSizeEdit
    end
    object HostNameEdit: TEdit
      Left = 7
      Top = 32
      Width = 129
      Height = 21
      TabOrder = 0
    end
    object PortEdit: TEdit
      Left = 143
      Top = 32
      Width = 81
      Height = 21
      TabOrder = 1
      OnExit = PortEditExit
    end
    object ProtocolComboBox: TComboBox
      Left = 231
      Top = 32
      Width = 113
      Height = 21
      Style = csDropDownList
      Sorted = True
      TabOrder = 2
    end
    object DatabaseEdit: TEdit
      Left = 7
      Top = 72
      Width = 193
      Height = 21
      TabOrder = 3
    end
    object CatalogComboBox: TComboBox
      Left = 231
      Top = 72
      Width = 113
      Height = 21
      TabOrder = 5
    end
    object DatabaseButton: TButton
      Left = 205
      Top = 72
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 4
      OnClick = DatabaseButtonClick
    end
    object UserNameEdit: TEdit
      Left = 7
      Top = 112
      Width = 145
      Height = 21
      TabOrder = 6
    end
    object PasswordEdit: TEdit
      Left = 159
      Top = 112
      Width = 89
      Height = 21
      PasswordChar = '*'
      TabOrder = 7
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 263
      Top = 112
      Width = 81
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 8
    end
    object PropertiesEditor: TMemo
      Left = 7
      Top = 160
      Width = 185
      Height = 129
      TabOrder = 9
    end
    object StreamFormatComboBox: TComboBox
      Left = 207
      Top = 160
      Width = 137
      Height = 21
      Style = csDropDownList
      Sorted = True
      TabOrder = 10
    end
    object IdDataTypeComboBox: TComboBox
      Left = 207
      Top = 200
      Width = 137
      Height = 21
      Style = csDropDownList
      TabOrder = 11
    end
    object IdSizeEdit: TEdit
      Left = 207
      Top = 240
      Width = 65
      Height = 21
      TabOrder = 12
    end
    object UseDelimitedIdentsCheckBox: TCheckBox
      Left = 207
      Top = 272
      Width = 137
      Height = 17
      Caption = '&Use delimited identifiers'
      TabOrder = 13
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 310
    Width = 372
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 209
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 289
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
