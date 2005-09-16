inherited PersonEditForm: TPersonEditForm
  DesignLeft = 351
  DesignTop = 252
  inherited OkButton: TIWButton
    TabOrder = 5
  end
  inherited CancelButton: TIWButton
    TabOrder = 7
  end
  inherited IWDBEdit6: TIWDBEdit
    TabOrder = 8
  end
  inherited IWDBEdit7: TIWDBEdit
    TabOrder = 9
  end
  object IWLabel5: TIWLabel [16]
    Left = 8
    Top = 240
    Width = 93
    Height = 16
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = False
    Alignment = taLeftJustify
    BGColor = clNone
    Font.Color = clNone
    Font.Size = 10
    Font.Style = [fsBold]
    NoWrap = False
    ConvertSpaces = False
    FriendlyName = 'IWLabel1'
    Caption = 'Date of Birth'
    RawText = False
  end
  object IWDBEdit5: TIWDBEdit [17]
    Left = 8
    Top = 256
    Width = 121
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    Alignment = taLeftJustify
    BGColor = clNone
    FocusColor = clNone
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FriendlyName = 'IWDBEdit5'
    MaxLength = 0
    ReadOnly = False
    Required = False
    ScriptEvents = <>
    TabOrder = 4
    AutoEditable = False
    DataField = 'DateOfBirth'
    PasswordPrompt = False
    DataSource = ContactSource
  end
  inherited ContactExposer: TInstantExposer
    ObjectClassName = 'TPerson'
    object ContactExposerDateOfBirth: TDateTimeField
      FieldName = 'DateOfBirth'
    end
  end
end
