inherited CompanyEditForm: TCompanyEditForm
  Caption = 'Company'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel [5]
    Left = 8
    Top = 168
    Width = 71
    Height = 13
    Caption = 'ContactPerson'
  end
  object DBLookupComboBox1: TDBLookupComboBox [10]
    Left = 8
    Top = 184
    Width = 184
    Height = 21
    DataField = 'ContactPerson'
    DataSource = ContactSource
    KeyField = 'Self'
    ListField = 'Name'
    ListSource = PersonsSource
    TabOrder = 4
  end
  inherited DBGrid1: TDBGrid
    TabOrder = 5
  end
  inherited OkButton: TButton
    TabOrder = 6
  end
  inherited CancelButton: TButton
    TabOrder = 7
  end
  inherited ContactExposer: TInstantExposer
    FieldOptions = [foObjects, foThorough]
    ObjectClassName = 'TCompany'
    Top = 224
    object ContactExposerContactPerson: TIntegerField
      FieldName = 'ContactPerson'
    end
  end
  object PersonsSelector: TInstantSelector
    FieldOptions = [foObjects, foThorough]
    AutoOpen = True
    Command.Strings = (
      'SELECT * FROM TPerson')
    Connector = MainForm.InstantXMLConnector
    Left = 136
    Top = 224
  end
  object PersonsSource: TDataSource
    DataSet = PersonsSelector
    Left = 168
    Top = 224
  end
end
