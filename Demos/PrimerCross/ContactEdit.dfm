inherited ContactEditForm: TContactEditForm
  Left = 300
  Top = 265
  ActiveControl = NameEdit
  BorderIcons = [biSystemMenu]
  Caption = 'Contact'
  ClientHeight = 295
  ClientWidth = 508
  ParentFont = True
  OldCreateOrder = True
  ExplicitWidth = 524
  ExplicitHeight = 334
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonPanel: TPanel
    Top = 264
    Left = 341
    Width = 508
    TabOrder = 1
    ExplicitTop = 264
    inherited AnchorPanel: TPanel
      Left = 341
    end
  end
  inherited ClientPanel: TPanel
    Width = 508
    Height = 264
    TabOrder = 0
    ExplicitHeight = 264
    inherited PageControl: TPageControl
      Width = 504
      Height = 260
      ExplicitHeight = 260
      inherited DetailsSheet: TTabSheet
        ExplicitHeight = 250
        object NameLabel: TLabel
          Left = 8
          Top = 8
          Width = 27
          Height = 13
          Caption = '&Name'
          FocusControl = NameEdit
        end
        object StreetLabel: TLabel
          Left = 8
          Top = 48
          Width = 30
          Height = 13
          Caption = '&Street'
          FocusControl = StreetEdit
        end
        object CityLabel: TLabel
          Left = 8
          Top = 104
          Width = 19
          Height = 13
          Caption = '&City'
          FocusControl = CityEdit
        end
        object ZipLabel: TLabel
          Left = 152
          Top = 104
          Width = 14
          Height = 13
          Caption = '&Zip'
          FocusControl = ZipEdit
        end
        object StateLabel: TLabel
          Left = 136
          Top = 144
          Width = 71
          Height = 13
          Caption = 'S&tate/Province'
          FocusControl = StateEdit
        end
        object PhonesLabel: TLabel
          Left = 240
          Top = 8
          Width = 35
          Height = 13
          Caption = '&Phones'
          FocusControl = PhonesGrid
        end
        object CountryLabel: TLabel
          Left = 8
          Top = 144
          Width = 39
          Height = 13
          Caption = 'C&ountry'
          FocusControl = CountryEdit
        end
        object MidBevel: TBevel
          Left = 224
          Top = 8
          Width = 9
          Height = 217
          Shape = bsLeftLine
        end
        object CategoryLabel: TLabel
          Left = 8
          Top = 184
          Width = 45
          Height = 13
          Caption = 'C&ategory'
          FocusControl = CategoryEdit
        end
        object NameEdit: TDBEdit
          Left = 8
          Top = 24
          Width = 201
          Height = 21
          DataField = 'Name'
          DataSource = SubjectSource
          TabOrder = 0
        end
        object StreetEdit: TDBMemo
          Left = 8
          Top = 64
          Width = 201
          Height = 33
          DataField = 'Address.Street'
          DataSource = SubjectSource
          TabOrder = 1
        end
        object CityEdit: TDBEdit
          Left = 8
          Top = 120
          Width = 137
          Height = 21
          DataField = 'Address.City'
          DataSource = SubjectSource
          TabOrder = 2
        end
        object ZipEdit: TDBEdit
          Left = 152
          Top = 120
          Width = 57
          Height = 21
          DataField = 'Address.Zip'
          DataSource = SubjectSource
          TabOrder = 3
        end
        object StateEdit: TDBEdit
          Left = 136
          Top = 160
          Width = 73
          Height = 21
          DataField = 'Address.State'
          DataSource = SubjectSource
          TabOrder = 5
        end
        object PhonesGrid: TDBGrid
          Left = 240
          Top = 24
          Width = 253
          Height = 197
          DataSource = PhonesSource
          Options = [dgEditing, dgTitles, dgColLines, dgCancelOnExit]
          TabOrder = 7
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
          OnDrawColumnCell = DbGridDrawColumnCellFixW11
          Columns = <
            item
              Expanded = False
              FieldName = 'Name'
              Width = 56
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Number'
              Width = 92
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'PhoneType'
              Visible = True
            end>
        end
        object CountryEdit: TDBLookupComboBox
          Left = 8
          Top = 160
          Width = 121
          Height = 21
          DataField = 'Address.Country'
          DataSource = SubjectSource
          DropDownRows = 6
          KeyField = 'Self'
          ListField = 'Name'
          ListSource = MainDataModule.CountrySource
          TabOrder = 4
        end
        object CategoryEdit: TDBLookupComboBox
          Left = 8
          Top = 200
          Width = 121
          Height = 21
          DataField = 'Category'
          DataSource = SubjectSource
          KeyField = 'Self'
          ListField = 'Name'
          ListSource = MainDataModule.CategorySource
          TabOrder = 6
        end
      end
    end
  end
  object PhonesExposer: TInstantExposer [2]
    ContainerName = 'Phones'
    MasterSource = SubjectSource
    Mode = amContent
    ObjectClassName = 'TPhone'
    Left = 66
    Top = 249
  end
  object PhonesSource: TDataSource [3]
    DataSet = PhonesExposer
    Left = 98
    Top = 249
  end
  inherited SubjectExposer: TInstantExposer
    FieldOptions = [foObjects, foThorough]
    OnIncludeField = SubjectExposerIncludeField
    ObjectClassName = 'TContact'
    Top = 249
  end
  inherited SubjectSource: TDataSource
    Top = 249
  end
end
