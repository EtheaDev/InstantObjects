inherited CompanyEditForm: TCompanyEditForm
  Height = 268
  Caption = 'Company'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel [5]
    Left = 8
    Top = 168
    Width = 70
    Height = 13
    Caption = 'ContactPerson'
  end
  inherited DBGrid1: TDBGrid
    TabOrder = 5
  end
  inherited OkButton: TButton
    Top = 208
    TabOrder = 6
  end
  inherited CancelButton: TButton
    Top = 208
    TabOrder = 7
  end
  object DBLookupComboBox1: TDBLookupComboBox [13]
    Left = 8
    Top = 184
    Width = 145
    Height = 21
    DataField = 'ContactPerson'
    DataSource = ContactSource
    KeyField = 'Self'
    ListField = 'Name'
    ListSource = PersonsSource
    TabOrder = 4
  end
  inherited ContactExposer: TInstantExposer
    FieldOptions = [foObjects, foThorough]
    ObjectClassName = 'TCompany'
    Top = 208
    object ContactExposerContactPerson: TIntegerField
      FieldName = 'ContactPerson'
    end
  end
  inherited ContactSource: TDataSource
    Top = 208
  end
  inherited PhonesExposer: TInstantExposer
    Top = 208
  end
  inherited PhonesSource: TDataSource
    Top = 208
  end
  object PersonsSelector: TInstantSelector
    FieldOptions = [foObjects, foThorough]
    AutoOpen = True
    Command.Strings = (
      'SELECT * FROM TPerson')
    Left = 136
    Top = 208
  end
  object PersonsSource: TDataSource
    DataSet = PersonsSelector
    Left = 168
    Top = 208
  end
end
