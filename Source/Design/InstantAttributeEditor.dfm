inherited InstantAttributeEditorForm: TInstantAttributeEditorForm
  Left = 78
  Top = 485
  Caption = 'Attribute Editor'
  ClientWidth = 302
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inherited EditPanel: TPanel
    Width = 298
    object PageControl: TPageControl
      Left = 4
      Top = 4
      Width = 290
      Height = 422
      ActivePage = DefinitionSheet
      Align = alClient
      TabOrder = 0
      object DefinitionSheet: TTabSheet
        Caption = 'Definition'
        object NameLabel: TLabel
          Left = 8
          Top = 8
          Width = 32
          Height = 15
          Caption = '&Name'
          FocusControl = NameEdit
        end
        object TypeLabel: TLabel
          Left = 8
          Top = 48
          Width = 24
          Height = 15
          Caption = '&Type'
          FocusControl = TypeEdit
        end
        object ObjectClassLabel: TLabel
          Left = 8
          Top = 88
          Width = 65
          Height = 15
          Caption = 'Object &Class'
          FocusControl = ObjectClassEdit
        end
        object StorageNameLabel: TLabel
          Left = 8
          Top = 176
          Width = 75
          Height = 15
          Caption = '&Storage Name'
          FocusControl = StorageNameEdit
        end
        object SizeLabel: TLabel
          Left = 208
          Top = 48
          Width = 20
          Height = 15
          Caption = 'Si&ze'
          FocusControl = SizeEdit
        end
        object ExternalStorageNameLabel: TLabel
          Left = 8
          Top = 262
          Width = 120
          Height = 15
          Caption = '&External Storage Name'
          FocusControl = ExternalStorageNameEdit
        end
        object StorageKindLabel: TLabel
          Left = 8
          Top = 216
          Width = 67
          Height = 15
          Caption = 'Storage &Kind'
          FocusControl = StorageKindEdit
        end
        object EnumeratedTypeLabel: TLabel
          Left = 7
          Top = 132
          Width = 90
          Height = 15
          Caption = '&Enumerated type'
          FocusControl = EnumeratedTypeEdit
        end
        object ForeignKeyFieldsLabel: TLabel
          Left = 8
          Top = 308
          Width = 95
          Height = 15
          Caption = '&Foreign Key Fields'
          FocusControl = ForeignKeyFieldsEdit
        end
        object NameEdit: TDBEdit
          Left = 7
          Top = 24
          Width = 271
          Height = 23
          DataField = 'Name'
          DataSource = SubjectSource
          TabOrder = 0
          OnChange = NameEditChange
          OnKeyPress = NameEditKeyPress
        end
        object TypeEdit: TDBComboBox
          Left = 7
          Top = 64
          Width = 192
          Height = 23
          Style = csDropDownList
          DataField = 'AttributeType'
          DataSource = SubjectSource
          Sorted = True
          TabOrder = 1
          OnClick = TypeEditClick
        end
        object ObjectClassEdit: TDBComboBox
          Left = 7
          Top = 104
          Width = 271
          Height = 23
          DataField = 'Metadata.ObjectClassName'
          DataSource = SubjectSource
          Sorted = True
          TabOrder = 3
          OnChange = ObjectClassEditChange
          OnEnter = ObjectClassEditEnter
        end
        object StorageNameEdit: TDBEdit
          Left = 7
          Top = 192
          Width = 271
          Height = 23
          DataField = 'StorageName'
          DataSource = SubjectSource
          TabOrder = 5
          OnChange = StorageNameEditChange
        end
        object SizeEdit: TDBEdit
          Left = 208
          Top = 64
          Width = 70
          Height = 23
          DataField = 'Metadata.Size'
          DataSource = SubjectSource
          TabOrder = 2
        end
        object ExternalStorageNameEdit: TDBEdit
          Left = 7
          Top = 279
          Width = 271
          Height = 23
          DataField = 'ExternalStorageName'
          DataSource = SubjectSource
          TabOrder = 8
          OnChange = ExternalStorageNameEditChange
        end
        object StorageKindEdit: TDBComboBox
          Left = 7
          Top = 232
          Width = 271
          Height = 23
          Style = csDropDownList
          DataField = 'StorageKind'
          DataSource = SubjectSource
          Sorted = True
          TabOrder = 6
          OnChange = StorageKindEditChange
        end
        object AutoExternalStorageNameCheckBox: TCheckBox
          Left = 216
          Top = 260
          Width = 62
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Auto'
          TabOrder = 7
          OnClick = AutoExternalStorageNameCheckBoxClick
        end
        object EnumeratedTypeEdit: TDBComboBox
          Left = 7
          Top = 149
          Width = 271
          Height = 23
          DataField = 'Metadata.EnumName'
          DataSource = SubjectSource
          TabOrder = 4
          OnChange = EnumeratedTypeEditChange
          OnEnter = EnumeratedTypeEditEnter
        end
        object ForeignKeyFieldsEdit: TDBEdit
          Left = 7
          Top = 325
          Width = 271
          Height = 23
          DataField = 'ForeignKeyFields'
          DataSource = SubjectSource
          TabOrder = 9
          OnChange = ForeignKeyFieldsEditChange
        end
      end
      object AccessSheet: TTabSheet
        Caption = 'Access'
        ImageIndex = 2
        object VisibilityLabel: TLabel
          Left = 8
          Top = 8
          Width = 44
          Height = 15
          Caption = '&Visibility'
          FocusControl = VisibilityEdit
        end
        object SingularNameLabel: TLabel
          Left = 148
          Top = 8
          Width = 78
          Height = 15
          Caption = '&Singular Name'
          FocusControl = SingularNameEdit
        end
        object IndexNameLabel: TLabel
          Left = 7
          Top = 261
          Width = 64
          Height = 15
          Caption = 'Index Name'
          FocusControl = IndexNameEdit
        end
        object VisibilityEdit: TDBComboBox
          Left = 8
          Top = 24
          Width = 130
          Height = 23
          Style = csDropDownList
          DataField = 'Visibility'
          DataSource = SubjectSource
          TabOrder = 0
        end
        object MethodsGroupBox: TGroupBox
          Left = 144
          Top = 50
          Width = 130
          Height = 205
          Caption = 'Methods'
          TabOrder = 3
          object MethodAddCheckBox: TCheckBox
            Left = 8
            Top = 24
            Width = 73
            Height = 17
            Caption = '&Add'
            TabOrder = 0
          end
          object MethodRemoveCheckBox: TCheckBox
            Left = 8
            Top = 44
            Width = 73
            Height = 17
            Caption = 'Re&move'
            TabOrder = 1
          end
          object MethodInsertCheckBox: TCheckBox
            Left = 8
            Top = 64
            Width = 73
            Height = 17
            Caption = '&Insert'
            TabOrder = 2
          end
          object MethodDeleteCheckBox: TCheckBox
            Left = 8
            Top = 84
            Width = 73
            Height = 17
            Caption = 'D&elete'
            TabOrder = 3
          end
          object MethodIndexOfCheckBox: TCheckBox
            Left = 8
            Top = 104
            Width = 73
            Height = 17
            Caption = 'Index&Of'
            TabOrder = 4
          end
          object MethodClearCheckBox: TCheckBox
            Left = 8
            Top = 124
            Width = 73
            Height = 17
            Caption = '&Clear'
            TabOrder = 5
          end
        end
        object SingularNameEdit: TDBEdit
          Left = 148
          Top = 24
          Width = 130
          Height = 23
          DataField = 'SingularName'
          DataSource = SubjectSource
          TabOrder = 1
        end
        object OptionsGroupBox: TGroupBox
          Left = 8
          Top = 50
          Width = 130
          Height = 205
          Caption = 'Options'
          TabOrder = 2
          object OptionReadOnlyCheckBox: TCheckBox
            Left = 8
            Top = 104
            Width = 86
            Height = 17
            Caption = '&Read only'
            TabOrder = 4
          end
          object OptionDefaultCheckBox: TCheckBox
            Left = 8
            Top = 124
            Width = 86
            Height = 17
            Caption = '&Default'
            TabOrder = 5
          end
          object OptionIndexedCheckBox: TCheckBox
            Left = 8
            Top = 24
            Width = 86
            Height = 17
            Caption = 'Inde&xed'
            TabOrder = 0
            OnClick = OptionIndexedCheckBoxClick
          end
          object OptionRequiredCheckBox: TCheckBox
            Left = 8
            Top = 84
            Width = 86
            Height = 17
            Caption = 'Req&uired'
            TabOrder = 3
          end
          object OptionUniqueCheckBox: TCheckBox
            Left = 8
            Top = 44
            Width = 86
            Height = 17
            Caption = 'Uni&que'
            TabOrder = 1
          end
          object OptionUseNullCheckBox: TCheckBox
            Left = 8
            Top = 144
            Width = 86
            Height = 17
            Caption = 'Use &Null'
            TabOrder = 6
          end
          object OptionPrimaryKeyCheckBox: TCheckBox
            Left = 8
            Top = 64
            Width = 86
            Height = 17
            Caption = 'Primary &Key'
            TabOrder = 2
          end
          object OptionIsDescriptionCheckBox: TCheckBox
            Left = 8
            Top = 164
            Width = 86
            Height = 17
            Caption = '&Description'
            TabOrder = 7
          end
        end
        object IndexNameEdit: TDBEdit
          Left = 7
          Top = 277
          Width = 271
          Height = 23
          DataField = 'IndexName'
          DataSource = SubjectSource
          TabOrder = 4
        end
      end
      object PresentationSheet: TTabSheet
        Caption = 'Presentation'
        ImageIndex = 1
        object EdtMaskLabel: TLabel
          Left = 8
          Top = 8
          Width = 51
          Height = 15
          Caption = 'Edit &Mask'
          FocusControl = EditMaskEdit
        end
        object DisplayWidthLabel: TLabel
          Left = 8
          Top = 100
          Width = 73
          Height = 15
          Caption = 'Display &Width'
          FocusControl = DisplayWidthEdit
        end
        object DisplayLabelLabel: TLabel
          Left = 8
          Top = 146
          Width = 69
          Height = 15
          Caption = 'Display &Label'
          FocusControl = DisplayLabelEdit
        end
        object ValidCharsLabel: TLabel
          Left = 8
          Top = 54
          Width = 84
          Height = 15
          Caption = '&Valid Characters'
          FocusControl = ValidCharsEdit
        end
        object DefaultValueLabel: TLabel
          Left = 8
          Top = 192
          Width = 69
          Height = 15
          Caption = '&Default Value'
          FocusControl = DefaultValueEdit
        end
        object EditMaskEdit: TDBEdit
          Left = 8
          Top = 23
          Width = 268
          Height = 23
          DataField = 'Metadata.EditMask'
          DataSource = SubjectSource
          TabOrder = 0
        end
        object DisplayWidthEdit: TDBEdit
          Left = 8
          Top = 116
          Width = 85
          Height = 23
          DataField = 'Metadata.DisplayWidth'
          DataSource = SubjectSource
          TabOrder = 2
        end
        object DisplayLabelEdit: TDBEdit
          Left = 8
          Top = 162
          Width = 268
          Height = 23
          DataField = 'Metadata.DisplayLabel'
          DataSource = SubjectSource
          TabOrder = 3
        end
        object ValidCharsEdit: TDBEdit
          Left = 8
          Top = 70
          Width = 268
          Height = 23
          DataField = 'Metadata.ValidCharsString'
          DataSource = SubjectSource
          TabOrder = 1
        end
        object DefaultValueEdit: TDBEdit
          Left = 8
          Top = 208
          Width = 268
          Height = 23
          DataField = 'Metadata.DefaultValue'
          DataSource = SubjectSource
          TabOrder = 4
        end
      end
    end
  end
  inherited BottomPanel: TPanel
    Width = 298
    inherited ButtonPanel: TPanel
      Left = 130
      inherited OkButton: TButton
        Left = 1
        Top = 2
        ExplicitLeft = 1
        ExplicitTop = 2
      end
      inherited CancelButton: TButton
        Left = 81
        Top = 2
        ExplicitLeft = 81
        ExplicitTop = 2
      end
    end
  end
  inherited SubjectExposer: TInstantExposer
    OnInitField = SubjectExposerInitField
    OnTranslate = SubjectExposerTranslate
  end
end
