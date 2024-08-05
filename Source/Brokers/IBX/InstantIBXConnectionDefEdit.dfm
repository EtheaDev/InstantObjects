object InstantIBXConnectionDefEditForm: TInstantIBXConnectionDefEditForm
  Left = 295
  Top = 238
  BorderStyle = bsDialog
  Caption = 'IBX Connection'
  ClientHeight = 334
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 297
    Width = 362
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 297
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ConnectionStringLabel: TLabel
      Left = 16
      Top = 16
      Width = 94
      Height = 13
      Caption = '&Connection String'
      FocusControl = ConnectionStringEdit
    end
    object UserNameLabel: TLabel
      Left = 16
      Top = 80
      Width = 54
      Height = 13
      Caption = 'User &name'
      FocusControl = UserNameEdit
    end
    object PasswordLabel: TLabel
      Left = 16
      Top = 120
      Width = 49
      Height = 13
      Caption = 'Pass&word'
      FocusControl = PasswordEdit
    end
    object SQLRoleLabel: TLabel
      Left = 16
      Top = 160
      Width = 45
      Height = 13
      Caption = 'S&QL Role'
      FocusControl = SQLRoleEdit
    end
    object CharacterSetLabel: TLabel
      Left = 16
      Top = 200
      Width = 67
      Height = 13
      Caption = 'C&haracter set'
      FocusControl = CharacterSetComboBox
    end
    object ParamsLabel: TLabel
      Left = 136
      Top = 80
      Width = 105
      Height = 13
      Caption = 'Connection &Settings'
      FocusControl = ParamsEditor
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 240
      Width = 60
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object IdDataTypeLabel: TLabel
      Left = 136
      Top = 240
      Width = 63
      Height = 13
      Caption = 'Id &Data Type'
      FocusControl = IdDataTypeComboBox
    end
    object IdSizeLabel: TLabel
      Left = 256
      Top = 240
      Width = 33
      Height = 13
      Caption = 'Id Si&ze'
      FocusControl = IdDataTypeComboBox
    end
    object ConnectionStringEdit: TEdit
      Left = 16
      Top = 32
      Width = 301
      Height = 21
      TabOrder = 0
    end
    object ConnectionStringButton: TButton
      Left = 321
      Top = 32
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = ConnectionStringButtonClick
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 256
      Width = 113
      Height = 21
      Style = csDropDownList
      TabOrder = 9
    end
    object UseDelimitedIdentsCheckBox: TCheckBox
      Left = 16
      Top = 56
      Width = 150
      Height = 17
      Caption = '&Use delimited identifiers'
      TabOrder = 2
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 196
      Top = 56
      Width = 150
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 3
    end
    object ParamsEditor: TMemo
      Left = 136
      Top = 96
      Width = 209
      Height = 141
      TabOrder = 8
    end
    object IdDataTypeComboBox: TComboBox
      Left = 136
      Top = 256
      Width = 113
      Height = 21
      Style = csDropDownList
      TabOrder = 10
    end
    object IdSizeEdit: TEdit
      Left = 256
      Top = 256
      Width = 89
      Height = 21
      TabOrder = 11
    end
    object UserNameEdit: TEdit
      Left = 16
      Top = 96
      Width = 113
      Height = 21
      TabOrder = 4
      OnChange = UserNameEditChange
    end
    object PasswordEdit: TEdit
      Left = 16
      Top = 136
      Width = 113
      Height = 21
      TabOrder = 5
      OnChange = PasswordEditChange
    end
    object SQLRoleEdit: TEdit
      Left = 16
      Top = 176
      Width = 113
      Height = 21
      TabOrder = 6
      OnChange = SQLRoleEditChange
    end
    object CharacterSetComboBox: TComboBox
      Left = 16
      Top = 216
      Width = 113
      Height = 21
      TabOrder = 7
      OnChange = CharacterSetComboBoxChange
      Items.Strings = (
        'None'
        'ASCII'
        'BIG_5'
        'CYRL'
        'DOS437'
        'DOS850'
        'DOS852'
        'DOS857'
        'DOS860'
        'DOS861'
        'DOS863'
        'DOS865'
        'EUCJ_0208'
        'GB_2312'
        'ISO8859_1'
        'KSC_5601'
        'NEXT'
        'OCTETS'
        'SJIS_0208'
        'UNICODE_FSS'
        'WIN1250'
        'WIN1251'
        'WIN1252'
        'WIN1253'
        'WIN1254')
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 299
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
    object TestButton: TButton
      Left = 7
      Top = 6
      Width = 106
      Height = 25
      Caption = 'Test connection...'
      TabOrder = 2
      OnClick = TestButtonClick
    end
  end
end
