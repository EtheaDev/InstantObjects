object MainForm: TMainForm
  Left = 232
  Top = 177
  Caption = 'InstantObjects simple demo'
  ClientHeight = 311
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    452
    311)
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
  object ContactGrid: TDBGrid
    Left = 8
    Top = 40
    Width = 439
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = ContactsSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDblClick = ContactGridDblClick
    Columns = <
      item
        Expanded = False
        FieldName = 'Name'
        Width = 158
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
        Width = 118
        Visible = True
      end>
  end
  object AddPersonButton: TButton
    Left = 131
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Add Person'
    TabOrder = 2
    OnClick = AddPersonButtonClick
  end
  object AddCompanyButton: TButton
    Left = 211
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Add Company'
    TabOrder = 3
    OnClick = AddCompanyButtonClick
  end
  object EditContactButton: TButton
    Left = 291
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Edit Contact'
    TabOrder = 4
    OnClick = EditContactButtonClick
  end
  object ExploreButton: TButton
    Left = 371
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Explore'
    TabOrder = 5
    OnClick = ExploreButtonClick
  end
  object ContactSelector: TInstantSelector
    Command.Strings = (
      'SELECT * FROM ANY TContact ORDER BY Name')
    Connector = InstantXMLConnector
    Left = 88
    Top = 232
  end
  object ContactsSource: TDataSource
    DataSet = ContactSelector
    Left = 120
    Top = 232
  end
  object InstantXMLConnector: TInstantXMLConnector
    IsDefault = True
    Connection = XMLFileAccessor
    Left = 56
    Top = 232
  end
  object XMLFileAccessor: TXMLFilesAccessor
    RootFolder = '\'
    Left = 24
    Top = 232
  end
end
