object MainForm: TMainForm
  Left = 232
  Top = 177
  BorderStyle = bsSingle
  Caption = 'Demo'
  ClientHeight = 309
  ClientWidth = 363
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
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 8
    Width = 240
    Height = 25
    DataSource = ContactsSource
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 40
    Width = 345
    Height = 233
    DataSource = ContactsSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Name'
        Width = 97
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Address.City'
        Width = 111
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Phones'
        Visible = True
      end>
  end
  object AddPersonButton: TButton
    Left = 8
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Add Person'
    TabOrder = 2
    OnClick = AddPersonButtonClick
  end
  object AddCompanyButton: TButton
    Left = 88
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Add Company'
    TabOrder = 3
    OnClick = AddCompanyButtonClick
  end
  object EditContactButton: TButton
    Left = 168
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Edit Contact'
    TabOrder = 4
    OnClick = EditContactButtonClick
  end
  object ExploreButton: TButton
    Left = 248
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Explore'
    TabOrder = 5
    OnClick = ExploreButtonClick
  end
  object ContactSelector: TInstantSelector
    AutoOpen = True
    Command.Strings = (
      'SELECT * FROM ANY TContact')
    Connector = InstantBDEConnector1
    Left = 88
    Top = 232
  end
  object ContactsSource: TDataSource
    DataSet = ContactSelector
    Left = 120
    Top = 232
  end
  object InstantBDEConnector1: TInstantBDEConnector
    IsDefault = True
    Connection = Database1
    Left = 56
    Top = 232
  end
  object Database1: TDatabase
    DatabaseName = 'DemoDatabase'
    DriverName = 'STANDARD'
    LoginPrompt = False
    Params.Strings = (
      'PATH=.\Database'
      'DEFAULT DRIVER=PARADOX'
      'ENABLE BCD=FALSE')
    SessionName = 'Default'
    TransIsolation = tiDirtyRead
    Left = 24
    Top = 232
  end
end
