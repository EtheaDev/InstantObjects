object InstantBDEConnectionDefEditForm: TInstantBDEConnectionDefEditForm
  Left = 324
  Top = 263
  BorderStyle = bsDialog
  Caption = 'BDE Connection'
  ClientHeight = 183
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
    Top = 146
    Width = 362
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 362
    Height = 146
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object AliasLabel: TLabel
      Left = 16
      Top = 16
      Width = 22
      Height = 13
      Caption = '&Alias'
      FocusControl = AliasComboBox
    end
    object DriverLabel: TLabel
      Left = 171
      Top = 16
      Width = 28
      Height = 13
      Caption = '&Driver'
      FocusControl = DriverComboBox
    end
    object ParametersLabel: TLabel
      Left = 16
      Top = 64
      Width = 53
      Height = 13
      Caption = '&Parameters'
      FocusControl = ParametersEdit
    end
    object AliasComboBox: TComboBox
      Left = 16
      Top = 32
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
      Left = 171
      Top = 32
      Width = 174
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnChange = DriverComboBoxChange
      OnDropDown = DriverComboBoxDropDown
    end
    object ParametersEdit: TMemo
      Left = 16
      Top = 80
      Width = 329
      Height = 49
      ScrollBars = ssVertical
      TabOrder = 2
      WordWrap = False
      OnChange = ParametersEditChange
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 148
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
