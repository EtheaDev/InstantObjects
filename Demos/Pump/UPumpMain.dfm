object PumpDemoMain: TPumpDemoMain
  Left = 393
  Top = 262
  BorderStyle = bsSingle
  Caption = 'InstantObjects Pump Demo'
  ClientHeight = 217
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object PumpManagerButton: TButton
    Left = 112
    Top = 136
    Width = 117
    Height = 25
    Caption = 'Pump Manager'
    TabOrder = 0
    OnClick = PumpManagerButtonClick
  end
  object ConnectionManager: TInstantConnectionManager
    FileFormat = sfXML
    OnConnect = ConnectionManagerConnect
    OnDisconnect = ConnectionManagerDisconnect
    OnIsConnected = ConnectionManagerIsConnected
    Left = 156
    Top = 64
  end
end
