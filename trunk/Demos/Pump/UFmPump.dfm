object FmPump: TFmPump
  Left = 459
  Top = 330
  Width = 469
  Height = 133
  Caption = 'InstantPump Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 442
    Height = 16
    Caption = 'This application uses the model from the Intro demo application.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 24
    Width = 266
    Height = 13
    Caption = 'Change the settings at design time and recompile to use.'
  end
  object EmptyBeforePumpCheckBox: TCheckBox
    Left = 8
    Top = 48
    Width = 193
    Height = 17
    Caption = 'Empty destination before pump'
    TabOrder = 0
    OnClick = EmptyBeforePumpCheckBoxClick
  end
  object PumpButton: TButton
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Pump'
    TabOrder = 1
    OnClick = PumpButtonClick
  end
  object SourceConnector: TInstantIBXConnector
    Connection = IBDatabase1
    Left = 224
    Top = 48
  end
  object DestConnector: TInstantIBXConnector
    BlobStreamFormat = sfXML
    Connection = IBDatabase2
    Left = 368
    Top = 48
  end
  object IBDatabase1: TIBDatabase
    Left = 264
    Top = 48
  end
  object IBDatabase2: TIBDatabase
    Left = 400
    Top = 48
  end
  object InstantPump1: TInstantPump
    SourceConnector = SourceConnector
    DestConnector = DestConnector
    Left = 320
    Top = 24
  end
end
