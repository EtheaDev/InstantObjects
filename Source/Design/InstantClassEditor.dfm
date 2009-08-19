inherited InstantClassEditorForm: TInstantClassEditorForm
  Left = 319
  Top = 196
  Caption = 'Class Editor'
  ClientHeight = 365
  ClientWidth = 400
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited EditPanel: TPanel
    Width = 400
    Height = 334
    object PageControl: TPageControl
      Left = 4
      Top = 4
      Width = 392
      Height = 326
      ActivePage = ClassSheet
      Align = alClient
      TabOrder = 0
      object ClassSheet: TTabSheet
        Caption = 'Class'
        OnResize = ClassSheetResize
        object ClassNameLabel: TLabel
          Left = 16
          Top = 16
          Width = 56
          Height = 13
          Caption = 'Class &Name'
          FocusControl = ClassNameEdit
        end
        object BaseClassLabel: TLabel
          Left = 16
          Top = 64
          Width = 52
          Height = 13
          Caption = '&Base Class'
          FocusControl = BaseClassEdit
        end
        object UnitLabel: TLabel
          Left = 16
          Top = 112
          Width = 19
          Height = 13
          Caption = '&Unit'
          FocusControl = UnitEdit
        end
        object StorageLabel: TLabel
          Left = 148
          Top = 160
          Width = 68
          Height = 13
          Caption = '&Storage Name'
          FocusControl = StorageEdit
        end
        object PersistenceLabel: TLabel
          Left = 16
          Top = 160
          Width = 55
          Height = 13
          Caption = '&Persistence'
          FocusControl = PersistenceComboBox
        end
        object ClassNameEdit: TDBEdit
          Left = 16
          Top = 32
          Width = 353
          Height = 21
          DataField = 'Name'
          DataSource = SubjectSource
          TabOrder = 0
          OnChange = ClassNameEditChange
        end
        object BaseClassEdit: TDBComboBox
          Left = 16
          Top = 80
          Width = 353
          Height = 21
          DataField = 'BaseClassName'
          DataSource = SubjectSource
          ItemHeight = 13
          Sorted = True
          TabOrder = 1
        end
        object UnitEdit: TDBComboBox
          Left = 16
          Top = 128
          Width = 353
          Height = 21
          Style = csDropDownList
          DataField = 'PascalUnitName'
          DataSource = SubjectSource
          ItemHeight = 13
          TabOrder = 2
        end
        object StorageEdit: TDBEdit
          Left = 148
          Top = 176
          Width = 217
          Height = 21
          DataField = 'StorageName'
          DataSource = SubjectSource
          TabOrder = 4
        end
        object PersistenceComboBox: TDBComboBox
          Left = 16
          Top = 176
          Width = 121
          Height = 21
          Style = csDropDownList
          DataField = 'Persistence'
          DataSource = SubjectSource
          ItemHeight = 13
          TabOrder = 3
          OnChange = PersistenceComboBoxChange
        end
      end
      object AttributeSheet: TTabSheet
        BorderWidth = 4
        Caption = 'Attributes'
        ImageIndex = 1
        inline InstantAttributeViewFrame: TInstantAttributeViewFrame
          Left = 0
          Top = 0
          Width = 376
          Height = 290
          Align = alClient
          TabOrder = 0
          inherited AttributesSplitter: TSplitter
            Top = 186
          end
          inherited InheritedAttributesPanel: TPanel
            Top = 190
          end
          inherited IntroducedAttributesPanel: TPanel
            Height = 186
            inherited IntroducedAttributesView: TListView
              Height = 170
            end
          end
        end
      end
    end
  end
  inherited BottomPanel: TPanel
    Top = 334
    Width = 400
    inherited ButtonPanel: TPanel
      Left = 240
      inherited OkButton: TButton
        Left = 1
        Top = 6
      end
    end
  end
  inherited SubjectExposer: TInstantExposer
    AfterPostField = SubjectExposerAfterPostField
    OnTranslate = SubjectExposerTranslate
    Top = 268
  end
  inherited SubjectSource: TDataSource
    Top = 268
  end
end
