object InstantDBXConnectionDefEditForm: TInstantDBXConnectionDefEditForm
  Left = 287
  Top = 34
  Caption = 'dbExpress Connection'
  ClientHeight = 307
  ClientWidth = 383
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
    Top = 270
    Width = 383
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
    ExplicitTop = 271
    ExplicitWidth = 391
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 383
    Height = 270
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object DriverNameLabel: TLabel
      Left = 16
      Top = 16
      Width = 62
      Height = 13
      Caption = '&Driver Name'
      FocusControl = DriverNameEdit
    end
    object ConnectionNameLabel: TLabel
      Left = 16
      Top = 60
      Width = 92
      Height = 13
      Caption = '&Connection Name'
      FocusControl = ConnectionNameListBox
    end
    object ParamsLabel: TLabel
      Left = 168
      Top = 16
      Width = 105
      Height = 13
      Caption = 'Connection &Settings'
      FocusControl = ParamsEditor
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 228
      Width = 60
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object Label1: TLabel
      Left = 168
      Top = 228
      Width = 127
      Height = 13
      Caption = 'Default Statement Cache'
      FocusControl = DefaultStatementCacheEdit
    end
    object DriverNameEdit: TComboBox
      Left = 16
      Top = 32
      Width = 137
      Height = 21
      Style = csDropDownList
      Sorted = True
      TabOrder = 0
      OnChange = DriverNameEditChange
    end
    object ConnectionNameListBox: TListBox
      Left = 16
      Top = 76
      Width = 136
      Height = 108
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnClick = ConnectionNameListBoxClick
    end
    object ParamsEditor: TMemo
      Left = 168
      Top = 32
      Width = 209
      Height = 190
      TabOrder = 4
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 244
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 5
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 16
      Top = 187
      Width = 137
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 2
    end
    object UseUnicodeCheckBox: TCheckBox
      Left = 16
      Top = 205
      Width = 137
      Height = 17
      Caption = '&Use Unicode'
      TabOrder = 3
    end
    object DefaultStatementCacheEdit: TEdit
      Left = 168
      Top = 243
      Width = 81
      Height = 21
      NumbersOnly = True
      TabOrder = 6
      Text = '0'
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 272
    Width = 383
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 222
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 302
      Top = 6
      Width = 75
      Height = 25
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
