object InstantCommandEditorForm: TInstantCommandEditorForm
  Left = 322
  Top = 268
  BorderStyle = bsDialog
  Caption = 'Command Editor'
  ClientHeight = 248
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object FromClassLabel: TLabel
    Left = 8
    Top = 11
    Width = 51
    Height = 13
    Caption = '&From Class'
    FocusControl = FromClassEdit
  end
  object CommandTextLabel: TLabel
    Left = 8
    Top = 72
    Width = 71
    Height = 13
    Caption = '&Command Text'
    FocusControl = CommandTextEdit
  end
  object AttributeLabel: TLabel
    Left = 8
    Top = 42
    Width = 31
    Height = 13
    Caption = '&Object'
  end
  object CommandTextEdit: TMemo
    Left = 8
    Top = 88
    Width = 377
    Height = 121
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    OnChange = CommandTextEditChange
  end
  object CancelButton: TButton
    Left = 311
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object OkButton: TButton
    Left = 231
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = OkButtonClick
  end
  object FromClassEdit: TComboBox
    Left = 72
    Top = 8
    Width = 177
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnClick = FromClassEditClick
  end
  object AnyCheckBox: TCheckBox
    Left = 256
    Top = 10
    Width = 121
    Height = 17
    Caption = '&Include descendants'
    TabOrder = 1
    OnClick = AnyCheckBoxClick
  end
  object AttributeEdit: TComboBox
    Left = 72
    Top = 40
    Width = 177
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnClick = AttributeEditClick
  end
  object DistinctCheckBox: TCheckBox
    Left = 256
    Top = 42
    Width = 65
    Height = 17
    Caption = '&Distinct'
    TabOrder = 3
    OnClick = DistinctCheckBoxClick
  end
end
