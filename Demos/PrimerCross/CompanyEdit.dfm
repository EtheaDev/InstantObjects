inherited CompanyEditForm: TCompanyEditForm
  Left = 282
  Top = 250
  Caption = 'Company'
  ClientHeight = 294
  ClientWidth = 534
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000078
    07F000000000080770078000000000700F8F7880000000878F8F77788000008F
    888787777000007F8777888778000077777F7888880000877FFF77788000008F
    FFF787777000007FF7780087780000777800900888000088009B907000000000
    9BB9007778000009B9007778800000090087788000000000000880000000C07F
    0000801F0000C0070000C0030000C0030000C0030000C0030000C0030000C003
    0000C0030000C0030000C0030000C0030000C0070000C01F0000CE7F0000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonPanel: TPanel
    Top = 263
    Width = 534
    inherited AnchorPanel: TPanel
      Left = 367
    end
  end
  inherited ClientPanel: TPanel
    Width = 534
    Height = 263
    inherited PageControl: TPageControl
      Width = 530
      Height = 259
      inherited DetailsSheet: TTabSheet
        TabVisible = True
        inherited ZipLabel: TLabel
          Left = 176
        end
        inherited StateLabel: TLabel
          Left = 160
        end
        inherited PhonesLabel: TLabel
          Left = 264
        end
        inherited MidBevel: TBevel
          Left = 248
        end
        inherited NameEdit: TDBEdit
          Width = 225
        end
        inherited StreetEdit: TDBMemo
          Width = 225
        end
        inherited CityEdit: TDBEdit
          Width = 161
        end
        inherited ZipEdit: TDBEdit
          Left = 176
        end
        inherited StateEdit: TDBEdit
          Left = 160
        end
        inherited PhonesGrid: TDBGrid
          Left = 264
        end
        inherited CountryEdit: TDBLookupComboBox
          Width = 145
        end
        inherited CategoryEdit: TDBLookupComboBox
          Width = 145
        end
      end
      object EmployeeSheet: TTabSheet
        Caption = 'Employees'
        ImageIndex = 1
        object EmployeeGridPanel: TPanel
          Left = 0
          Top = 0
          Width = 427
          Height = 231
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 2
          TabOrder = 0
          object EmployeeGrid: TDBGrid
            Left = 2
            Top = 2
            Width = 423
            Height = 227
            Align = alClient
            DataSource = EmployeeSource
            Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
            PopupMenu = EmployeeGridMenu
            ReadOnly = True
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            Columns = <
              item
                Expanded = False
                FieldName = 'Name'
                Width = 132
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'MainEmailAddress'
                Title.Caption = 'E-mail'
                Width = 104
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'MainPhoneNumber'
                Title.Caption = 'Phone'
                Width = 78
                Visible = True
              end>
          end
        end
        object EmployeeButtonPanel: TPanel
          Left = 427
          Top = 0
          Width = 95
          Height = 231
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object EmployeeNewButton: TBitBtn
            Left = 10
            Top = 8
            Width = 75
            Height = 25
            Action = EmployeeNewAction
            Caption = '&New...'
            TabOrder = 0
          end
          object EmployeeEditButton: TBitBtn
            Left = 10
            Top = 72
            Width = 75
            Height = 25
            Action = EmployeeEditAction
            Caption = '&Edit...'
            TabOrder = 2
          end
          object EmployeeDeleteButton: TBitBtn
            Left = 10
            Top = 136
            Width = 75
            Height = 25
            Action = EmployeeDeleteAction
            Caption = '&Delete'
            TabOrder = 4
          end
          object EmployeeLookupButton: TBitBtn
            Left = 10
            Top = 40
            Width = 75
            Height = 25
            Action = EmployeeLookupAction
            Caption = '&Lookup...'
            TabOrder = 1
          end
          object EmployeeRemoveButton: TBitBtn
            Left = 10
            Top = 104
            Width = 75
            Height = 25
            Action = EmployeeRemoveAction
            Caption = '&Remove'
            TabOrder = 3
          end
        end
      end
    end
  end
  inherited PhonesExposer: TInstantExposer
    Top = 258
  end
  inherited PhonesSource: TDataSource
    Top = 258
  end
  inherited SubjectExposer: TInstantExposer
    ObjectClassName = 'TCompany'
    Top = 258
  end
  inherited SubjectSource: TDataSource
    Top = 258
  end
  object EmployeeExposer: TInstantExposer
    Sorted = True
    OnCompare = EmployeeExposerCompare
    ContainerName = 'Employees'
    MasterSource = SubjectSource
    Mode = amContent
    ObjectClassName = 'TPerson'
    Left = 130
    Top = 258
  end
  object EmployeeSource: TDataSource
    DataSet = EmployeeExposer
    Left = 162
    Top = 258
  end
  object Actions: TActionList
    Images = ActionImages
    OnUpdate = ActionsUpdate
    Left = 358
    Top = 2
    object EmployeeNewAction: TAction
      Caption = '&New...'
      Hint = 'New'
      ImageIndex = 0
      OnExecute = EmployeeNewActionExecute
    end
    object EmployeeLookupAction: TAction
      Caption = '&Lookup...'
      Hint = 'Lookup'
      ImageIndex = 1
      OnExecute = EmployeeLookupActionExecute
    end
    object EmployeeEditAction: TAction
      Caption = '&Edit...'
      Hint = 'Edit'
      ImageIndex = 2
      OnExecute = EmployeeEditActionExecute
    end
    object EmployeeRemoveAction: TAction
      Caption = '&Remove'
      Hint = 'Remove'
      ImageIndex = 3
      OnExecute = EmployeeRemoveActionExecute
    end
    object EmployeeDeleteAction: TAction
      Caption = '&Delete'
      Hint = 'Delete'
      ImageIndex = 4
      OnExecute = EmployeeDeleteActionExecute
    end
  end
  object ActionImages: TImageList
    Left = 390
    Top = 2
  end
  object EmployeeGridMenu: TPopupMenu
    Images = ActionImages
    Left = 422
    Top = 2
    object EmployeeNewItem: TMenuItem
      Action = EmployeeNewAction
    end
    object EmployeeLookupItem: TMenuItem
      Action = EmployeeLookupAction
    end
    object EmployeeEditItem: TMenuItem
      Action = EmployeeEditAction
    end
    object EmployeeRemoveItem: TMenuItem
      Action = EmployeeRemoveAction
    end
    object EmployeeDeleteItem: TMenuItem
      Action = EmployeeDeleteAction
    end
  end
end
