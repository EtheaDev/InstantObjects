object InstantDBXConnectionDefEditForm: TInstantDBXConnectionDefEditForm
  Left = 325
  Top = 290
  BorderStyle = bsDialog
  Caption = 'dbExpress Connection'
  ClientHeight = 243
  ClientWidth = 391
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
    Top = 206
    Width = 391
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 206
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
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
      Top = 64
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
    object DriverNameEdit: TComboBox
      Left = 16
      Top = 32
      Width = 137
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = DriverNameEditChange
    end
    object ConnectionNameListBox: TListBox
      Left = 16
      Top = 80
      Width = 136
      Height = 108
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnClick = ConnectionNameListBoxClick
    end
    object ParamsEditor: TValueListEditor
      Left = 168
      Top = 32
      Width = 209
      Height = 156
      TabOrder = 2
      ColWidths = (
        85
        118)
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 208
    Width = 391
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 233
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
      Left = 313
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
