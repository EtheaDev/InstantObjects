object InstantCommandEditorForm: TInstantCommandEditorForm
  Left = 322
  Top = 268
  Width = 401
  Height = 275
  Caption = 'Command Editor'
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
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 393
    Height = 89
    Align = alTop
    TabOrder = 0
    object FromClassLabel: TLabel
      Left = 8
      Top = 11
      Width = 51
      Height = 13
      Caption = '&From Class'
      FocusControl = FromClassEdit
    end
    object AttributeLabel: TLabel
      Left = 8
      Top = 42
      Width = 31
      Height = 13
      Caption = '&Object'
    end
    object CommandTextLabel: TLabel
      Left = 8
      Top = 72
      Width = 71
      Height = 13
      Caption = '&Command Text'
      FocusControl = CommandTextEdit
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
  object BottomPanel: TPanel
    Left = 0
    Top = 208
    Width = 393
    Height = 33
    Align = alBottom
    TabOrder = 2
    object ButtonsPanel: TPanel
      Left = 227
      Top = 1
      Width = 165
      Height = 31
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object OkButton: TButton
        Left = 6
        Top = 4
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = OkButtonClick
      end
      object CancelButton: TButton
        Left = 86
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object CommandTextEdit: TMemo
    Left = 0
    Top = 89
    Width = 393
    Height = 119
    Align = alClient
    TabOrder = 1
    OnChange = CommandTextEditChange
  end
end
