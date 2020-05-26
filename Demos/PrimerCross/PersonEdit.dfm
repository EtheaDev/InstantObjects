inherited PersonEditForm: TPersonEditForm
  Left = 298
  Top = 244
  Caption = 'Person'
  ClientHeight = 344
  Font.Name = 'Segoe UI'
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000000000000000000000000000F4000000000
    F404404400000000844044040000000008404404000000000844444400000000
    084444440000000000880440000000000008000000000000008FF71000000000
    008771100000000000111110000000000001110000000000000000000000FFFF
    0000FFFF0000FF9F0000F20F0000E0070000F0070000F8070000F8070000F807
    0000FC0F0000FE3F0000FC1F0000FC1F0000FC1F0000FE3F0000FFFF0000}
  OnShow = FormShow
  ExplicitHeight = 383
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonPanel: TPanel
    Top = 313
    ExplicitTop = 313
    ExplicitWidth = 425
    inherited AnchorPanel: TPanel
      Left = 258
      ExplicitLeft = 258
    end
  end
  inherited ClientPanel: TPanel
    Height = 313
    ExplicitWidth = 425
    ExplicitHeight = 313
    inherited PageControl: TPageControl
      Height = 309
      ExplicitWidth = 421
      ExplicitHeight = 309
      inherited DetailsSheet: TTabSheet
        ExplicitLeft = 4
        ExplicitTop = 6
        ExplicitWidth = 413
        ExplicitHeight = 299
        inherited NameLabel: TLabel
          Width = 29
          ExplicitWidth = 29
        end
        inherited StreetLabel: TLabel
          Width = 30
          ExplicitWidth = 30
        end
        inherited CityLabel: TLabel
          Width = 19
          ExplicitWidth = 19
        end
        inherited ZipLabel: TLabel
          Width = 16
          ExplicitWidth = 16
        end
        inherited StateLabel: TLabel
          Width = 73
          ExplicitWidth = 73
        end
        inherited PhonesLabel: TLabel
          Width = 38
          ExplicitWidth = 38
        end
        inherited CountryLabel: TLabel
          Width = 41
          ExplicitWidth = 41
        end
        inherited MidBevel: TBevel
          Height = 290
          ExplicitHeight = 290
        end
        object BirthDateLabel: TLabel [8]
          Left = 136
          Top = 184
          Width = 51
          Height = 13
          Caption = '&Birth Date'
          FocusControl = BirthDateEdit
        end
        object EmployerLabel: TLabel [9]
          Left = 8
          Top = 224
          Width = 47
          Height = 13
          Caption = '&Employer'
          FocusControl = EmployerEdit
        end
        object EmailsLabel: TLabel [10]
          Left = 240
          Top = 104
          Width = 36
          Height = 13
          Caption = 'E-&mails'
          FocusControl = EmailsGrid
        end
        inherited CategoryLabel: TLabel
          Width = 46
          ExplicitWidth = 46
        end
        object SalaryLabel: TLabel [12]
          Left = 8
          Top = 264
          Width = 30
          Height = 13
          Caption = 'Sa&lary'
        end
        object Label1: TLabel [13]
          Left = 136
          Top = 264
          Width = 48
          Height = 13
          Caption = 'BirthTime'
          FocusControl = BirthTimeEdit
        end
        object PicturePanel: TPanel [14]
          Left = 332
          Top = 203
          Width = 76
          Height = 96
          BevelOuter = bvLowered
          Caption = 'Picture'
          TabOrder = 14
          object PictureImage: TImage
            Left = 3
            Top = 2
            Width = 70
            Height = 93
            Stretch = True
          end
        end
        inherited StreetEdit: TDBMemo
          Height = 37
          ExplicitHeight = 37
        end
        inherited PhonesGrid: TDBGrid
          Height = 76
          TabOrder = 11
          TitleFont.Name = 'Segoe UI'
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
              Width = 90
              Visible = True
            end>
        end
        object BirthDateEdit: TDBEdit [21]
          Left = 136
          Top = 200
          Width = 73
          Height = 21
          DataField = 'BirthDate'
          DataSource = SubjectSource
          TabOrder = 7
        end
        object EmployerEdit: TDBEdit [22]
          Left = 8
          Top = 240
          Width = 129
          Height = 21
          DataField = 'Employer.Name'
          DataSource = SubjectSource
          ReadOnly = True
          TabOrder = 8
        end
        object EmailsGrid: TDBGrid [23]
          Left = 240
          Top = 120
          Width = 169
          Height = 76
          DataSource = EmailsSource
          Options = [dgEditing, dgColLines, dgCancelOnExit]
          TabOrder = 12
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Segoe UI'
          TitleFont.Style = []
          Columns = <
            item
              Expanded = False
              FieldName = 'Address'
              Width = 146
              Visible = True
            end>
        end
        object EmployerToolBar: TToolBar [24]
          Left = 140
          Top = 237
          Width = 69
          Height = 23
          Align = alNone
          AutoSize = True
          ButtonHeight = 23
          Caption = 'EmployerToolBar'
          Images = EmployerToolImages
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          object EmployerLookupButton: TToolButton
            Left = 0
            Top = 0
            Hint = 'Lookup'
            Caption = 'EmployerLookupButton'
            ImageIndex = 0
            OnClick = EmployerLookupButtonClick
          end
          object EmployerEditButton: TToolButton
            Left = 23
            Top = 0
            Hint = 'Edit'
            Caption = 'CompanyLookupButton'
            ImageIndex = 1
            OnClick = EmployerEditButtonClick
          end
          object EmployerClearButton: TToolButton
            Left = 46
            Top = 0
            Hint = 'Clear'
            Caption = 'CompanyLookupButton'
            Enabled = False
            ImageIndex = 2
            OnClick = EmployerClearButtonClick
          end
        end
        object PictureButton: TButton [25]
          Left = 240
          Top = 246
          Width = 81
          Height = 25
          Caption = 'Load &Picture...'
          TabOrder = 13
          OnClick = PictureButtonClick
        end
        object SalaryEdit: TDBEdit
          Left = 8
          Top = 280
          Width = 121
          Height = 21
          DataField = 'Salary'
          DataSource = SubjectSource
          TabOrder = 10
        end
        object ClearButton: TButton
          Left = 240
          Top = 274
          Width = 81
          Height = 25
          Caption = '&Clear Picture'
          TabOrder = 15
          OnClick = ClearButtonClick
        end
        object BirthTimeEdit: TDBEdit
          Left = 136
          Top = 280
          Width = 81
          Height = 21
          DataField = 'BirthTime'
          DataSource = SubjectSource
          TabOrder = 16
        end
      end
    end
  end
  inherited PhonesExposer: TInstantExposer
    Top = 317
  end
  inherited PhonesSource: TDataSource
    Top = 317
  end
  inherited SubjectExposer: TInstantExposer
    ObjectClassName = 'TPerson'
    Top = 317
  end
  inherited SubjectSource: TDataSource
    Top = 317
  end
  object EmailsExposer: TInstantExposer
    Options = []
    ContainerName = 'Emails'
    MasterSource = SubjectSource
    Mode = amContent
    ObjectClassName = 'TEmail'
    Left = 130
    Top = 317
  end
  object EmailsSource: TDataSource
    DataSet = EmailsExposer
    Left = 162
    Top = 317
  end
  object EmployerToolImages: TImageList
    Left = 271
    Top = 217
  end
end
