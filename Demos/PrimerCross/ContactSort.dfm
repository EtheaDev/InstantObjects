object ContactSortForm: TContactSortForm
  Left = 334
  Top = 264
  Caption = 'Sort'
  ClientHeight = 228
  ClientWidth = 362
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
  object InfoLabel: TLabel
    Left = 16
    Top = 16
    Width = 321
    Height = 33
    AutoSize = False
    Caption = 
      'Contacts can be ordered by a persistent attribute when selected ' +
      'from the database or sorted by a published property afterwards.'
    WordWrap = True
  end
  object AttributeRadioButton: TRadioButton
    Left = 56
    Top = 64
    Width = 153
    Height = 17
    Caption = 'Order by persistent attribute'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object PropertyRadioButton: TRadioButton
    Left = 56
    Top = 128
    Width = 153
    Height = 17
    Caption = 'Sort by published property'
    TabOrder = 1
  end
  object AttributeComboBox: TComboBox
    Left = 72
    Top = 88
    Width = 209
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 2
  end
  object PropertyComboBox: TComboBox
    Left = 72
    Top = 152
    Width = 209
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 3
  end
  object OkButton: TButton
    Left = 192
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object CancelButton: TButton
    Left = 272
    Top = 200
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
