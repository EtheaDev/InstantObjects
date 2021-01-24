inherited InstantClassEditorForm: TInstantClassEditorForm
  Left = 319
  Top = 196
  Caption = 'Class Editor'
  ClientHeight = 461
  ClientWidth = 384
  Font.Name = 'Segoe UI'
  OldCreateOrder = True
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 400
  ExplicitHeight = 500
  PixelsPerInch = 96
  TextHeight = 13
  inherited EditPanel: TPanel
    Width = 384
    Height = 430
    ExplicitWidth = 384
    ExplicitHeight = 430
    object PageControl: TPageControl
      Left = 4
      Top = 4
      Width = 376
      Height = 422
      ActivePage = ClassSheet
      Align = alClient
      TabOrder = 0
      object ClassSheet: TTabSheet
        Caption = 'Class'
        OnResize = ClassSheetResize
        object ClassNameLabel: TLabel
          Left = 16
          Top = 16
          Width = 58
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
          Width = 22
          Height = 13
          Caption = '&Unit'
          FocusControl = UnitEdit
        end
        object StorageLabel: TLabel
          Left = 148
          Top = 160
          Width = 72
          Height = 13
          Caption = '&Storage Name'
          FocusControl = StorageEdit
        end
        object PersistenceLabel: TLabel
          Left = 16
          Top = 160
          Width = 57
          Height = 13
          Caption = '&Persistence'
          FocusControl = PersistenceComboBox
        end
        object ClassNameEdit: TDBEdit
          Left = 16
          Top = 32
          Width = 347
          Height = 21
          DataField = 'Name'
          DataSource = SubjectSource
          TabOrder = 0
          OnChange = ClassNameEditChange
        end
        object BaseClassEdit: TDBComboBox
          Left = 16
          Top = 80
          Width = 347
          Height = 21
          DataField = 'BaseClassName'
          DataSource = SubjectSource
          Sorted = True
          TabOrder = 1
        end
        object UnitEdit: TDBComboBox
          Left = 16
          Top = 128
          Width = 347
          Height = 21
          Style = csDropDownList
          DataField = 'PascalUnitName'
          DataSource = SubjectSource
          TabOrder = 2
        end
        object StorageEdit: TDBEdit
          Left = 148
          Top = 176
          Width = 215
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
          Width = 360
          Height = 386
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 360
          ExplicitHeight = 386
          inherited AttributesSplitter: TSplitter
            Top = 282
            Width = 360
            ExplicitTop = 186
            ExplicitWidth = 360
          end
          inherited InheritedAttributesPanel: TPanel
            Top = 286
            Width = 360
            ExplicitTop = 286
            ExplicitWidth = 360
            inherited InheritedAttributesLabel: TLabel
              Width = 360
              ExplicitWidth = 360
            end
            inherited InheritedAttributesView: TListView
              Width = 360
              ExplicitWidth = 360
            end
          end
          inherited IntroducedAttributesPanel: TPanel
            Width = 360
            Height = 282
            ExplicitWidth = 360
            ExplicitHeight = 282
            inherited IntroducedAttributesLabel: TLabel
              Width = 360
              ExplicitWidth = 360
            end
            inherited IntroducedAttributesView: TListView
              Width = 360
              Height = 266
              ExplicitWidth = 360
              ExplicitHeight = 266
            end
          end
        end
      end
    end
  end
  inherited BottomPanel: TPanel
    Top = 430
    Width = 384
    ExplicitTop = 430
    ExplicitWidth = 384
    inherited ButtonPanel: TPanel
      Left = 224
      ExplicitLeft = 224
      inherited OkButton: TButton
        Left = -1
        Top = 2
        ExplicitLeft = -1
        ExplicitTop = 2
      end
      inherited CancelButton: TButton
        Left = 79
        Top = 2
        ExplicitLeft = 79
        ExplicitTop = 2
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
