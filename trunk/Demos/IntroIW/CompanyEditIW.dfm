inherited CompanyEditForm: TCompanyEditForm
  DesignLeft = 376
  DesignTop = 459
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
  object IWDBLookupComboBox1: TIWDBLookupComboBox [16]
    Left = 8
    Top = 248
    Width = 169
    Height = 21
    Cursor = crAuto
    IW50Hint = False
    ParentShowHint = False
    ShowHint = True
    ZIndex = 0
    RenderSize = True
    BGColor = clNone
    Font.Color = clNone
    Font.Size = 10
    Font.Style = []
    FocusColor = clNone
    AutoHideOnMenuActivation = False
    ItemsHaveValues = False
    NoSelectionText = '-- No Selection --'
    Required = False
    RequireSelection = True
    ScriptEvents = <>
    UseSize = True
    Style = stNormal
    ButtonColor = clBtnFace
    DoSubmitValidation = True
    Editable = True
    NonEditableAsLabel = True
    TabOrder = 4
    AutoEditable = False
    DataField = 'ContactPerson'
    DataSource = ContactSource
    FriendlyName = 'IWDBLookupComboBox1'
    KeyField = 'Self'
    ListField = 'Name'
    ListSource = PersonsSource
    DisableWhenEmpty = True
  end
  object IWLabel5: TIWLabel [17]
    Left = 8
    Top = 230
    Width = 115
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
    Caption = 'Contact Person'
    RawText = False
  end
  inherited ContactExposer: TInstantExposer
    FieldOptions = [foObjects, foThorough]
    ObjectClassName = 'TCompany'
    object ContactExposerContactPerson: TIntegerField
      FieldName = 'ContactPerson'
    end
  end
  object PersonsSelector: TInstantSelector
    FieldOptions = [foObjects, foThorough]
    AutoOpen = True
    Command.Strings = (
      'SELECT * FROM TPerson')
    Connector = IWUserSession.InstantADOConnector1
    Left = 160
    Top = 352
  end
  object PersonsSource: TDataSource
    DataSet = PersonsSelector
    Left = 192
    Top = 352
  end
end
