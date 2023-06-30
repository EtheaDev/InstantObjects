object InstantModelExplorerForm: TInstantModelExplorerForm
  Left = 385
  Top = 186
  Width = 418
  Height = 536
  VertScrollBar.Range = 20
  BorderWidth = 4
  Caption = 'InstantObjects Model Explorer'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object AttributeSplitter: TSplitter
    Left = 0
    Top = 242
    Width = 394
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    Constraints.MinHeight = 4
    Visible = False
    ExplicitTop = 255
    ExplicitWidth = 410
  end
  object ModelPanel: TPanel
    Left = 0
    Top = 31
    Width = 394
    Height = 211
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 20
    TabOrder = 0
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 394
    Height = 31
    Align = alTop
    TabOrder = 2
    object cbEnableModelUpdate: TCheckBox
      AlignWithMargins = True
      Left = 213
      Top = 4
      Width = 120
      Height = 23
      Hint = 'Update InstantObject Model every seconds'
      Margins.Right = 60
      Align = alRight
      Alignment = taLeftJustify
      Caption = 'Update every secs:'
      TabOrder = 0
      OnClick = cbEnableModelUpdateClick
    end
    object ToolBar: TToolBar
      Left = 1
      Top = 1
      Width = 209
      Height = 29
      Align = alClient
      BorderWidth = 1
      ButtonHeight = 23
      Images = ActionImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object SelectUnitsButton: TToolButton
        Left = 0
        Top = 0
        Action = SelectUnitsAction
      end
      object BuildDatabaseButton: TToolButton
        Left = 23
        Top = 0
        Action = BuildDatabaseAction
      end
      object ToolSep1: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Caption = 'ToolSep1'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object ViewButton: TToolButton
        Left = 54
        Top = 0
        Action = ViewRelationsAction
      end
      object ToolSep2: TToolButton
        Left = 77
        Top = 0
        Width = 8
        Caption = 'ToolSep2'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object ViewAttributeButton: TToolButton
        Left = 85
        Top = 0
        Action = ViewAttributesAction
        ImageIndex = 12
      end
    end
    object edInterval: TSpinEdit
      AlignWithMargins = True
      Left = 338
      Top = 3
      Width = 40
      Height = 24
      Hint = 'Update InstantObject Model every seconds'
      MaxValue = 100
      MinValue = 1
      TabOrder = 2
      Value = 10
      OnChange = edIntervalChange
    end
  end
  object AttributePanel: TPanel
    Left = 0
    Top = 246
    Width = 394
    Height = 243
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 45
    TabOrder = 1
    Visible = False
    object AttributeCaptionPanel: TPanel
      Left = 0
      Top = 0
      Width = 394
      Height = 25
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 0
      object AttributeCaptionLabel: TLabel
        Left = 12
        Top = 6
        Width = 82
        Height = 15
        Caption = 'Class Attributes'
      end
    end
    inline InstantAttributeViewFrame: TInstantAttributeViewFrame
      Left = 0
      Top = 25
      Width = 394
      Height = 218
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      ExplicitTop = 25
      ExplicitHeight = 218
      inherited AttributesSplitter: TSplitter
        Top = 114
        Width = 410
      end
      inherited InheritedAttributesPanel: TPanel
        Top = 118
        Width = 394
        inherited InheritedAttributesLabel: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 410
        end
        inherited InheritedAttributesView: TListView
          Top = 22
          Width = 398
          Height = 78
          ExplicitTop = 22
          ExplicitWidth = 394
          ExplicitHeight = 78
        end
      end
      inherited IntroducedAttributesPanel: TPanel
        Width = 394
        Height = 114
        inherited IntroducedAttributesLabel: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 410
        end
        inherited IntroducedAttributesView: TListView
          Top = 22
          Width = 394
          Height = 92
        end
      end
      inherited Actions: TActionList
        inherited AttributeNewAction: TAction
          OnExecute = InstantAttributeViewFrameAttributeNewActionExecute
        end
        inherited AttributeDeleteAction: TAction
          OnExecute = InstantAttributeViewFrameAttributeDeleteActionExecute
        end
        inherited AttributeEditAction: TAction
          OnExecute = InstantAttributeViewFrameAttributeEditActionExecute
        end
      end
    end
  end
  object ModelImages: TImageList
    Left = 104
    Top = 40
  end
  object TreeMenu: TPopupMenu
    Images = ActionImages
    OnPopup = TreeMenuPopup
    Left = 8
    Top = 40
    object NewClassItem: TMenuItem
      Action = NewClassAction
    end
    object EditClassItem: TMenuItem
      Action = EditClassAction
    end
    object DeleteClassItem: TMenuItem
      Action = DeleteClassAction
    end
    object ViewSourceItem: TMenuItem
      Action = ViewSourceAction
    end
    object ViewAttributes: TMenuItem
      Action = ViewAttributesAction
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ExpandAllItem: TMenuItem
      Action = ExpandAllAction
    end
    object CollapseAllItem: TMenuItem
      Action = CollapseAllAction
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object RefreshItem: TMenuItem
      Action = RefreshAction
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object ImportModelItem: TMenuItem
      Action = ImportModelAction
    end
    object ExportModelItem: TMenuItem
      Action = ExportModelAction
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object AboutItem: TMenuItem
      Action = AboutAction
    end
  end
  object Actions: TActionList
    Images = ActionImages
    Left = 40
    Top = 40
    object EditClassAction: TAction
      Caption = '&Edit Class'
      Hint = 'Edit Class'
      ImageIndex = 6
      ShortCut = 32781
      OnExecute = EditClassActionExecute
    end
    object RefreshAction: TAction
      Caption = '&Refresh'
      Hint = 'Refresh'
      ImageIndex = 8
      ShortCut = 116
      OnExecute = RefreshActionExecute
    end
    object SelectUnitsAction: TAction
      Caption = 'Select &Units'
      Enabled = False
      Hint = 'Select Units'
      ImageIndex = 0
      OnExecute = SelectUnitsActionExecute
    end
    object BuildDatabaseAction: TAction
      Caption = '&Build InstantObjects Database...'
      Enabled = False
      Hint = 'Build Database'
      ImageIndex = 1
      OnExecute = BuildDatabaseActionExecute
    end
    object ViewInheritanceAction: TAction
      Caption = '&Inheritance'
      Hint = 'View Inheritance'
      ImageIndex = 2
      OnExecute = ViewInheritanceActionExecute
    end
    object ViewRelationsAction: TAction
      Caption = '&Relations'
      Hint = 'View Relations'
      ImageIndex = 3
      OnExecute = ViewRelationsActionExecute
    end
    object NewClassAction: TAction
      Caption = '&New Class...'
      Hint = 'New Class'
      ImageIndex = 4
      ShortCut = 45
      OnExecute = NewClassActionExecute
    end
    object DeleteClassAction: TAction
      Caption = '&Delete Class'
      Hint = 'Delete Class'
      ImageIndex = 9
      ShortCut = 46
      OnExecute = DeleteClassActionExecute
    end
    object ViewSourceAction: TAction
      Caption = 'View &Source'
      Hint = 'View Source'
      ImageIndex = 7
      ShortCut = 13
      OnExecute = ViewSourceActionExecute
    end
    object ExpandAllAction: TAction
      Caption = 'E&xpand All'
      Hint = 'Expand All'
      OnExecute = ExpandAllActionExecute
    end
    object CollapseAllAction: TAction
      Caption = '&Collapse All'
      Hint = 'Collapse All'
      OnExecute = CollapseAllActionExecute
    end
    object ImportModelAction: TAction
      Caption = '&Import Model...'
      Hint = 'Import model'
      ImageIndex = 11
      OnExecute = ImportModelActionExecute
    end
    object ExportModelAction: TAction
      Caption = '&Export Model...'
      Hint = 'Export Model'
      ImageIndex = 10
      OnExecute = ExportModelActionExecute
    end
    object AboutAction: TAction
      Caption = '&About InstantObjects...'
      Hint = 'About InstantObjects'
      OnExecute = AboutActionExecute
    end
    object ViewAttributesAction: TAction
      Caption = 'View Attributes'
      Hint = 'View Class Attributes'
      ImageIndex = 11
      OnExecute = ViewAttributesActionExecute
    end
  end
  object AttributeImages: TImageList
    Left = 136
    Top = 40
  end
  object ActionImages: TImageList
    Left = 72
    Top = 40
  end
end
