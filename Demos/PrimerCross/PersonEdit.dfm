inherited PersonEditForm: TPersonEditForm
  Left = 298
  Top = 244
  Caption = 'Person'
  ClientHeight = 353
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
  TextHeight = 13
  inherited ButtonPanel: TPanel
    Top = 322
  end
  inherited ClientPanel: TPanel
    Height = 322
    inherited PageControl: TPageControl
      Height = 319
      inherited DetailsSheet: TTabSheet
        inherited MidBevel: TBevel
          Height = 290
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
          Left = 417
          Top = 203
          Width = 76
          Height = 96
          BevelOuter = bvLowered
          Caption = 'Picture'
          TabOrder = 13
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
        end
        object BirthDateEdit: TDBEdit [20]
          Left = 136
          Top = 200
          Width = 73
          Height = 21
          DataField = 'BirthDate'
          DataSource = SubjectSource
          TabOrder = 7
        end
        object EmployerEdit: TDBEdit [21]
          Left = 8
          Top = 240
          Width = 129
          Height = 21
          DataField = 'Employer.Name'
          DataSource = SubjectSource
          ReadOnly = True
          TabOrder = 8
        end
        object EmailsGrid: TDBGrid [22]
          Left = 240
          Top = 120
          Width = 253
          Height = 76
          DataSource = EmailsSource
          Options = [dgEditing, dgColLines, dgCancelOnExit]
          TabOrder = 11
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Segoe UI'
          TitleFont.Style = []
          OnDrawColumnCell = DbGridDrawColumnCellFixW11
          Columns = <
            item
              Expanded = False
              FieldName = 'Address'
              Width = 220
              Visible = True
            end>
        end
        object EmployerToolBar: TToolBar [23]
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
        object PictureButton: TButton [24]
          Left = 330
          Top = 246
          Width = 81
          Height = 25
          Caption = 'Load &Picture...'
          TabOrder = 12
          OnClick = PictureButtonClick
        end
        inherited PhonesGrid: TDBGrid
          Height = 77
          TabOrder = 16
        end
        object SalaryEdit: TDBEdit [27]
          Left = 8
          Top = 280
          Width = 121
          Height = 21
          DataField = 'Salary'
          DataSource = SubjectSource
          TabOrder = 10
        end
        object ClearButton: TButton [28]
          Left = 330
          Top = 274
          Width = 81
          Height = 25
          Caption = '&Clear Picture'
          TabOrder = 14
          OnClick = ClearButtonClick
        end
        object BirthTimeEdit: TDBEdit [29]
          Left = 136
          Top = 280
          Width = 81
          Height = 21
          DataField = 'BirthTime'
          DataSource = SubjectSource
          TabOrder = 15
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
