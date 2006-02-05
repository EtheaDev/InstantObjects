object InstantUIBConnectionDefEditForm: TInstantUIBConnectionDefEditForm
  Left = 324
  Top = 202
  BorderStyle = bsDialog
  Caption = 'UIB Connection'
  ClientHeight = 373
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
    Top = 336
    Width = 362
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 336
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ConnectionStringLabel: TLabel
      Left = 16
      Top = 16
      Width = 84
      Height = 13
      Caption = '&Connection String'
      FocusControl = ConnectionStringEdit
    end
    object UserNameLabel: TLabel
      Left = 16
      Top = 80
      Width = 51
      Height = 13
      Caption = 'User &name'
      FocusControl = UserNameEdit
    end
    object PasswordLabel: TLabel
      Left = 16
      Top = 120
      Width = 46
      Height = 13
      Caption = 'Pass&word'
      FocusControl = PasswordEdit
    end
    object SQLRoleLabel: TLabel
      Left = 16
      Top = 160
      Width = 46
      Height = 13
      Caption = 'S&QL Role'
      FocusControl = SQLRoleEdit
    end
    object CharacterSetLabel: TLabel
      Left = 16
      Top = 200
      Width = 63
      Height = 13
      Caption = 'C&haracter set'
      FocusControl = CharacterSetComboBox
    end
    object LibraryNameLabel: TLabel
      Left = 16
      Top = 240
      Width = 60
      Height = 13
      Caption = 'Li&brary name'
      FocusControl = LibraryNameComboBox
    end
    object ParamsLabel: TLabel
      Left = 136
      Top = 80
      Width = 95
      Height = 13
      Caption = 'Connection &Settings'
      FocusControl = ParamsEditor
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 280
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object Label1: TLabel
      Left = 136
      Top = 280
      Width = 62
      Height = 13
      Caption = 'Id &Data Type'
      FocusControl = IdDataTypeComboBox
    end
    object Label2: TLabel
      Left = 256
      Top = 280
      Width = 32
      Height = 13
      Caption = 'Id Si&ze'
      FocusControl = IdDataTypeComboBox
    end
    object ConnectionStringEdit: TEdit
      Left = 16
      Top = 32
      Width = 305
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
      Top = 296
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 10
    end
    object UseDelimitedIdentsCheckBox: TCheckBox
      Left = 16
      Top = 56
      Width = 150
      Height = 17
      Caption = '&Use delimited identifiers'
      TabOrder = 2
    end
    object ParamsEditor: TMemo
      Left = 136
      Top = 96
      Width = 209
      Height = 181
      TabOrder = 9
    end
    object IdDataTypeComboBox: TComboBox
      Left = 136
      Top = 296
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
    end
    object IdSizeEdit: TEdit
      Left = 256
      Top = 296
      Width = 89
      Height = 21
      TabOrder = 12
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 196
      Top = 56
      Width = 150
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 3
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
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 7
      OnChange = CharacterSetComboBoxChange
    end
    object LibraryNameComboBox: TComboBox
      Left = 16
      Top = 256
      Width = 113
      Height = 21
      ItemHeight = 13
      TabOrder = 8
      OnChange = SQLRoleEditChange
      Items.Strings = (
        'gds32.dll'
        'fbclient.dll')
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 338
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
