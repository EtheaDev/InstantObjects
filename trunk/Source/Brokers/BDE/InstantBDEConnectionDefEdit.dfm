object InstantBDEConnectionDefEditForm: TInstantBDEConnectionDefEditForm
  Left = 207
  Top = 248
  BorderStyle = bsDialog
  Caption = 'BDE Connection'
  ClientHeight = 351
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
    Top = 314
    Width = 392
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 392
    Height = 314
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
      Left = 163
      Top = 12
      Width = 28
      Height = 13
      Caption = '&Driver'
      FocusControl = DriverComboBox
    end
    object ParametersLabel: TLabel
      Left = 12
      Top = 56
      Width = 53
      Height = 13
      Caption = '&Parameters'
      FocusControl = ParametersEdit
    end
    object StreamFormatLabel: TLabel
      Left = 172
      Top = 288
      Width = 53
      Height = 13
      Caption = 'Blob &format'
      FocusControl = StreamFormatComboBox
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
      Left = 163
      Top = 28
      Width = 222
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
      Top = 72
      Width = 365
      Height = 201
      ScrollBars = ssVertical
      TabOrder = 2
      WordWrap = False
      OnChange = ParametersEditChange
    end
    object StreamFormatComboBox: TComboBox
      Left = 232
      Top = 284
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 4
      OnChange = AliasComboBoxChange
      OnDropDown = AliasComboBoxDropDown
    end
    object LoginPromptCheckBox: TCheckBox
      Left = 16
      Top = 286
      Width = 142
      Height = 17
      Caption = '&Login Prompt'
      TabOrder = 3
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 316
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
