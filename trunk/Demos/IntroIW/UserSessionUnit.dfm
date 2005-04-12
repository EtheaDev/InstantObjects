object IWUserSession: TIWUserSession
  OldCreateOrder = False
  OnCreate = IWUserSessionBaseCreate
  Left = 448
  Top = 429
  Height = 374
  Width = 344
  object ADOConnection1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=.\Database\IWDemoDa' +
      'ta.mdb;Persist Security Info=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 64
    Top = 184
  end
  object InstantADOConnector1: TInstantADOConnector
    Connection = ADOConnection1
    Left = 96
    Top = 240
  end
end
