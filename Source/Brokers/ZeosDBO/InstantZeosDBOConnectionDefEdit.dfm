object InstantZeosDBOConnectionDefEditForm: TInstantZeosDBOConnectionDefEditForm
  Left = 214
  Top = 165
  BorderStyle = bsDialog
  Caption = 'ZeosDBO Connection'
  ClientHeight = 345
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
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
      Left = 16
      Top = 16
      Width = 53
      Height = 13
      Caption = '&Host Name'
      FocusControl = HostNameEdit
    end
    object PortLabel: TLabel
      Left = 176
      Top = 16
      Width = 19
      Height = 13
      Caption = '&Port'
      FocusControl = PortEdit
    end
    object ProtocolLabel: TLabel
      Left = 240
      Top = 16
      Width = 42
      Height = 13
      Caption = '&Protocol:'
      FocusControl = ProtocolComboBox
    end
    object DatabaseLabel: TLabel
      Left = 16
      Top = 56
      Width = 46
      Height = 13
      Caption = '&Database'
      FocusControl = DatabaseEdit
    end
    object CatalogLabel: TLabel
      Left = 240
      Top = 56
      Width = 36
      Height = 13
      Caption = '&Catalog'
      FocusControl = CatalogEdit
    end
    object UserNameLabel: TLabel
      Left = 16
      Top = 96
      Width = 51
      Height = 13
      Caption = '&User name'
      FocusControl = UserNameEdit
    end
    object PasswordLabel: TLabel
      Left = 168
      Top = 96
      Width = 46
      Height = 13
      Caption = '&Password'
      FocusControl = PasswordEdit
    end
    object PropertiesLabel: TLabel
      Left = 16
      Top = 144
      Width = 50
      Height = 13
      Caption = 'Pr&operties:'
      FocusControl = PropertiesEditor
    end
    object StreamFormatLabel: TLabel
      Left = 216
      Top = 144
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object IdDataTypeLabel: TLabel
      Left = 216
      Top = 184
      Width = 62
      Height = 13
      Caption = 'Id Data Type'
      FocusControl = IdDataTypeComboBox
    end
    object IdSizeLabel: TLabel
      Left = 216
      Top = 224
      Width = 32
      Height = 13
      Caption = 'Id Size'
      FocusControl = IdSizeEdit
    end
    object HostNameEdit: TEdit
      Left = 16
      Top = 32
      Width = 153
      Height = 21
      TabOrder = 0
    end
    object PortEdit: TEdit
      Left = 176
      Top = 32
      Width = 57
      Height = 21
      TabOrder = 1
    end
    object ProtocolComboBox: TComboBox
      Left = 240
      Top = 32
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 2
    end
    object DatabaseEdit: TEdit
      Left = 16
      Top = 72
      Width = 193
      Height = 21
      TabOrder = 3
    end
    object CatalogEdit: TEdit
      Left = 240
      Top = 72
      Width = 113
      Height = 21
      TabOrder = 5
    end
    object DatabaseButton: TButton
      Left = 214
      Top = 72
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 4
    end
    object UserNameEdit: TEdit
      Left = 16
      Top = 112
      Width = 145
      Height = 21
      TabOrder = 6
    end
    object PasswordEdit: TEdit
      Left = 168
      Top = 112
      Width = 89
      Height = 21
      PasswordChar = '*'
      TabOrder = 7
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 272
      Top = 112
      Width = 81
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 8
    end
    object PropertiesEditor: TMemo
      Left = 16
      Top = 160
      Width = 185
      Height = 129
      TabOrder = 9
    end
    object StreamFormatComboBox: TComboBox
      Left = 216
      Top = 160
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 10
    end
    object IdDataTypeComboBox: TComboBox
      Left = 216
      Top = 200
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
    end
    object IdSizeEdit: TEdit
      Left = 216
      Top = 240
      Width = 65
      Height = 21
      TabOrder = 12
    end
    object UseDelimitedIdentsCheckBox: TCheckBox
      Left = 216
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
