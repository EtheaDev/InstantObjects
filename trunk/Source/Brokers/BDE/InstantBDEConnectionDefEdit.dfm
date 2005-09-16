object InstantBDEConnectionDefEditForm: TInstantBDEConnectionDefEditForm
  Left = 89
  Top = 439
  BorderStyle = bsDialog
  Caption = 'BDE Connection'
  ClientHeight = 369
  ClientWidth = 392
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
    Top = 332
    Width = 392
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 392
    Height = 332
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object AliasLabel: TLabel
      Left = 12
      Top = 12
      Width = 22
      Height = 13
      Caption = '&Alias'
      FocusControl = AliasComboBox
    end
    object DriverLabel: TLabel
      Left = 164
      Top = 12
      Width = 28
      Height = 13
      Caption = '&Driver'
      FocusControl = DriverComboBox
    end
    object ParametersLabel: TLabel
      Left = 12
      Top = 60
      Width = 53
      Height = 13
      Caption = '&Parameters'
      FocusControl = ParametersEdit
    end
    object StreamFormatLabel: TLabel
      Left = 12
      Top = 285
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
    end
    object Label1: TLabel
      Left = 148
      Top = 285
      Width = 62
      Height = 13
      Caption = 'Id Data Type'
      FocusControl = IdDataTypeComboBox
    end
    object Label2: TLabel
      Left = 287
      Top = 285
      Width = 32
      Height = 13
      Caption = 'Id Size'
      FocusControl = IdDataTypeComboBox
    end
    object AliasComboBox: TComboBox
      Left = 12
      Top = 28
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = AliasComboBoxChange
      OnDropDown = AliasComboBoxDropDown
    end
    object DriverComboBox: TComboBox
      Left = 164
      Top = 28
      Width = 213
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnChange = DriverComboBoxChange
      OnDropDown = DriverComboBoxDropDown
    end
    object ParametersEdit: TMemo
      Left = 12
      Top = 76
      Width = 365
      Height = 201
      ScrollBars = ssVertical
      TabOrder = 2
      WordWrap = False
      OnChange = ParametersEditChange
    end
    object StreamFormatComboBox: TComboBox
      Left = 12
      Top = 301
      Width = 120
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 4
      OnChange = AliasComboBoxChange
      OnDropDown = AliasComboBoxDropDown
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 272
      Top = 54
      Width = 105
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 3
    end
    object IdDataTypeComboBox: TComboBox
      Left = 148
      Top = 301
      Width = 120
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
    end
    object IdSizeEdit: TEdit
      Left = 287
      Top = 301
      Width = 90
      Height = 21
      TabOrder = 6
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 334
    Width = 392
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 234
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
      Left = 314
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
