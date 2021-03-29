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
  OnShow = FormShow
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
      Width = 81
      Height = 13
      Caption = 'Por&t (0=default)'
      FocusControl = PortEdit
    end
    object DatabaseLabel: TLabel
      Left = 7
      Top = 56
      Width = 282
      Height = 13
      Caption = '&Database Name (or filename for Firebird and Interbase)'
      FocusControl = DatabaseEdit
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
      Left = 102
      Top = 96
      Width = 49
      Height = 13
      Caption = '&Password'
      FocusControl = PasswordEdit
    end
    object PropertiesLabel: TLabel
      Left = 7
      Top = 144
      Width = 96
      Height = 13
      Caption = 'Additional Par&ams:'
      FocusControl = AdditionalParamsEditor
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
      Width = 71
      Height = 13
      Caption = 'Isolation level'
    end
    object DriverIdLabel: TLabel
      Left = 231
      Top = 16
      Width = 47
      Height = 13
      Caption = 'D&river ID:'
      FocusControl = DriverIdComboBox
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
    object DatabaseEdit: TEdit
      Left = 7
      Top = 72
      Width = 310
      Height = 21
      TabOrder = 3
    end
    object DatabaseButton: TButton
      Left = 323
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
      Width = 90
      Height = 21
      TabOrder = 5
    end
    object PasswordEdit: TEdit
      Left = 102
      Top = 112
      Width = 89
      Height = 21
      PasswordChar = '*'
      TabOrder = 6
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 209
      Top = 99
      Width = 104
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 7
    end
    object AdditionalParamsEditor: TMemo
      Left = 7
      Top = 160
      Width = 185
      Height = 142
      TabOrder = 9
    end
    object StreamFormatComboBox: TComboBox
      Left = 207
      Top = 160
      Width = 137
      Height = 21
      Style = csDropDownList
      TabOrder = 10
    end
    object IdDataTypeComboBox: TComboBox
      Left = 207
      Top = 198
      Width = 137
      Height = 21
      Style = csDropDownList
      TabOrder = 11
    end
    object OSAuthCheckBox: TCheckBox
      Left = 209
      Top = 121
      Width = 104
      Height = 17
      Caption = '&OS Authent'
      TabOrder = 8
    end
    object DriverIdComboBox: TComboBox
      Left = 231
      Top = 32
      Width = 113
      Height = 21
      Style = csDropDownList
      Sorted = True
      TabOrder = 2
    end
    object UseDelimitedIdentsCheckBox: TCheckBox
      Left = 207
      Top = 285
      Width = 157
      Height = 17
      Caption = 'Use &delimited identifiers'
      TabOrder = 14
    end
    object UseUnicodeCheckBox: TCheckBox
      Left = 207
      Top = 265
      Width = 104
      Height = 17
      Caption = 'Use &Unicode'
      TabOrder = 13
    end
    object cbIsolation: TComboBox
      Left = 207
      Top = 238
      Width = 137
      Height = 21
      Style = csDropDownList
      TabOrder = 12
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
      TabOrder = 1
    end
    object CancelButton: TButton
      Left = 289
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object TestButton: TButton
      Left = 7
      Top = 6
      Width = 106
      Height = 25
      Caption = 'Test connection...'
      TabOrder = 0
      OnClick = TestButtonClick
    end
  end
end
