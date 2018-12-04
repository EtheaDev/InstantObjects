object InstantDBXConnectionDefEditForm: TInstantDBXConnectionDefEditForm
  Left = 287
  Top = 34
  Caption = 'dbExpress Connection'
  ClientHeight = 303
  ClientWidth = 383
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
    Top = 266
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
    Height = 266
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 391
    ExplicitHeight = 271
    object DriverNameLabel: TLabel
      Left = 16
      Top = 16
      Width = 59
      Height = 13
      Caption = '&Driver Name'
      FocusControl = DriverNameEdit
    end
    object ConnectionNameLabel: TLabel
      Left = 16
      Top = 60
      Width = 85
      Height = 13
      Caption = '&Connection Name'
      FocusControl = ConnectionNameListBox
    end
    object ParamsLabel: TLabel
      Left = 168
      Top = 16
      Width = 95
      Height = 13
      Caption = 'Connection &Settings'
      FocusControl = ParamsEditor
    end
    object StreamFormatLabel: TLabel
      Left = 16
      Top = 228
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
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
      Height = 233
      TabOrder = 4
    end
    object StreamFormatComboBox: TComboBox
      Left = 16
      Top = 244
      Width = 145
      Height = 21
      Style = csDropDownList
      Sorted = True
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
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 268
    Width = 383
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 273
    ExplicitWidth = 391
    object OkButton: TButton
      Left = 233
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 313
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
