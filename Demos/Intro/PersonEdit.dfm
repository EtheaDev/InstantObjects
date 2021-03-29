inherited PersonEditForm: TPersonEditForm
  Caption = 'Person'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel [5]
    Left = 8
    Top = 168
    Width = 57
    Height = 13
    Caption = 'DateOfBirth'
  end
  inherited DBGrid1: TDBGrid
    TabOrder = 5
  end
  inherited OkButton: TButton
    TabOrder = 6
  end
  object DBEdit5: TDBEdit [12]
    Left = 8
    Top = 184
    Width = 95
    Height = 21
    DataField = 'DateOfBirth'
    DataSource = ContactSource
    TabOrder = 4
  end
  inherited CancelButton: TButton
    TabOrder = 7
  end
  inherited ContactExposer: TInstantExposer
    ObjectClassName = 'TPerson'
    Top = 208
    object ContactExposerDateOfBirth: TDateTimeField
      FieldName = 'DateOfBirth'
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
end
