object ContactEditForm: TContactEditForm
  Left = 220
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Contact'
  ClientHeight = 206
  ClientWidth = 373
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Name'
    FocusControl = DBEdit1
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 69
    Height = 13
    Caption = 'Address.Street'
    FocusControl = DBEdit2
  end
  object Label3: TLabel
    Left = 8
    Top = 88
    Width = 58
    Height = 13
    Caption = 'Address.City'
    FocusControl = DBEdit3
  end
  object Label4: TLabel
    Left = 8
    Top = 128
    Width = 95
    Height = 13
    Caption = 'Address.PostalCode'
    FocusControl = DBEdit4
  end
  object Label5: TLabel
    Left = 200
    Top = 8
    Width = 36
    Height = 13
    Caption = 'Phones'
  end
  object DBEdit1: TDBEdit
    Left = 8
    Top = 24
    Width = 184
    Height = 21
    DataField = 'Name'
    DataSource = ContactSource
    TabOrder = 0
  end
  object DBEdit2: TDBEdit
    Left = 8
    Top = 64
    Width = 184
    Height = 21
    DataField = 'Address.Street'
    DataSource = ContactSource
    TabOrder = 1
  end
  object DBEdit3: TDBEdit
    Left = 8
    Top = 104
    Width = 184
    Height = 21
    DataField = 'Address.City'
    DataSource = ContactSource
    TabOrder = 2
  end
  object DBEdit4: TDBEdit
    Left = 8
    Top = 144
    Width = 52
    Height = 21
    DataField = 'Address.PostalCode'
    DataSource = ContactSource
    TabOrder = 3
  end
  object DBGrid1: TDBGrid
    Left = 200
    Top = 24
    Width = 169
    Height = 137
    DataSource = PhonesSource
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Name'
        Width = 60
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Number'
        Visible = True
      end>
  end
  object OkButton: TButton
    Left = 208
    Top = 176
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 288
    Top = 176
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object ContactExposer: TInstantExposer
    ObjectClassName = 'TContact'
    Left = 8
    Top = 176
    object ContactExposerName: TStringField
      FieldName = 'Name'
      Size = 30
    end
    object ContactExposerAddressStreet: TStringField
      FieldName = 'Address.Street'
      Size = 30
    end
    object ContactExposerAddressCity: TStringField
      FieldName = 'Address.City'
      Size = 30
    end
    object ContactExposerAddressPostalCode: TStringField
      FieldName = 'Address.PostalCode'
      Size = 8
    end
    object ContactExposerPhones: TDataSetField
      FieldName = 'Phones'
      IncludeObjectField = False
    end
  end
  object ContactSource: TDataSource
    DataSet = ContactExposer
    Left = 40
    Top = 176
  end
  object PhonesExposer: TInstantExposer
    ContainerName = 'Phones'
    MasterSource = ContactSource
    Mode = amContent
    ObjectClassName = 'TPhone'
    Left = 72
    Top = 176
  end
  object PhonesSource: TDataSource
    DataSet = PhonesExposer
    Left = 104
    Top = 176
  end
end
