object FmPump: TFmPump
  Left = 459
  Top = 330
  Width = 469
  Height = 306
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
  object EmptyBeforePumpCheckBox: TCheckBox
    Left = 8
    Top = 8
    Width = 193
    Height = 17
    Caption = 'Empty destination before pump'
    TabOrder = 0
    OnClick = EmptyBeforePumpCheckBoxClick
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Pump'
    TabOrder = 1
    OnClick = Button1Click
  end
  object InstantIBXConnector1: TInstantIBXConnector
    Connection = IBDatabase1
    Left = 48
    Top = 160
  end
  object InstantIBXConnector2: TInstantIBXConnector
    IsDefault = True
    BlobStreamFormat = sfXML
    Connection = IBDatabase2
    Left = 168
    Top = 160
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = 'C:\IBDATA\PRIMER.GDB'
    Params.Strings = (
      'user_name=SYSDBA'
      'password=a')
    LoginPrompt = False
    SQLDialect = 1
    Left = 48
    Top = 208
  end
  object IBDatabase2: TIBDatabase
    Connected = True
    DatabaseName = 'C:\IBDATA\PRIMERXML.FDB'
    Params.Strings = (
      'user_name=SYSDBA'
      'password=a')
    LoginPrompt = False
    Left = 168
    Top = 208
  end
  object InstantPump1: TInstantPump
    SourceConnector = InstantIBXConnector1
    DestConnector = InstantIBXConnector2
    Options = []
    Left = 112
    Top = 104
  end
end
