object InstantIBXConnectionDefEditForm: TInstantIBXConnectionDefEditForm
  Left = 330
  Top = 305
  BorderStyle = bsDialog
  Caption = 'InterBase Connection'
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
    object ServerLabel: TLabel
      Left = 24
      Top = 40
      Width = 31
      Height = 13
      Caption = '&Server'
      FocusControl = ServerEdit
    end
    object ProtocolLabel: TLabel
      Left = 243
      Top = 40
      Width = 39
      Height = 13
      Caption = '&Protocol'
      FocusControl = ProtocolEdit
    end
    object DatabaseLabel: TLabel
      Left = 24
      Top = 88
      Width = 46
      Height = 13
      Caption = '&Database'
      FocusControl = DatabaseEdit
    end
    object LocalRadioButton: TRadioButton
      Left = 12
      Top = 16
      Width = 57
      Height = 17
      Caption = '&Local'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = LocalRemoteChange
    end
    object RemoteRadioButton: TRadioButton
      Left = 76
      Top = 16
      Width = 65
      Height = 17
      Caption = '&Remote'
      TabOrder = 1
      OnClick = LocalRemoteChange
    end
    object ServerEdit: TEdit
      Left = 24
      Top = 56
      Width = 209
      Height = 21
      TabOrder = 2
    end
    object ProtocolEdit: TComboBox
      Left = 243
      Top = 56
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      Items.Strings = (
        'TCP/IP'
        'NetBEUI'
        'SPX')
    end
    object DatabaseEdit: TEdit
      Left = 24
      Top = 104
      Width = 292
      Height = 21
      TabOrder = 4
    end
    object DatabaseButton: TButton
      Left = 319
      Top = 104
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = DatabaseButtonClick
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
