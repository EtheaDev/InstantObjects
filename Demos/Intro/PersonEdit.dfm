inherited PersonEditForm: TPersonEditForm
  Height = 270
  Caption = 'Person'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel [5]
    Left = 8
    Top = 168
    Width = 55
    Height = 13
    Caption = 'DateOfBirth'
    FocusControl = DBEdit5
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
  object DBEdit5: TDBEdit [13]
    Left = 8
    Top = 184
    Width = 112
    Height = 21
    DataField = 'DateOfBirth'
    DataSource = ContactSource
    TabOrder = 4
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
