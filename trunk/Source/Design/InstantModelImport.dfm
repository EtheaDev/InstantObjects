inherited InstantImportModelForm: TInstantImportModelForm
  Caption = 'Import Model'
  ClientHeight = 120
  ClientWidth = 416
  ExplicitWidth = 424
  ExplicitHeight = 154
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 16
    Top = 19
    Width = 78
    Height = 13
    Caption = 'Import to module'
  end
  object Label2: TLabel [1]
    Left = 16
    Top = 46
    Width = 45
    Height = 13
    Caption = 'File name'
  end
  inherited ButtonPanel: TPanel
    Top = 79
    Width = 416
    ExplicitTop = 79
    ExplicitWidth = 416
    inherited ButtonBevel: TBevel
      Width = 416
      ExplicitWidth = 416
    end
    object ImportButton: TButton
      Left = 246
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Import'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 327
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ImportModuleCombo: TComboBox
    Left = 103
    Top = 16
    Width = 299
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = ImportModuleComboChange
  end
  object FileNameEdit: TEdit
    Left = 103
    Top = 43
    Width = 275
    Height = 21
    TabOrder = 2
    OnChange = FileNameEditChange
  end
  object FileNameButton: TButton
    Left = 381
    Top = 43
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = FileNameButtonClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'Resource Model (*.mdr)|*.mdr|XML Model (*.xml)|*.xml'
    Options = [ofHideReadOnly, ofPathMustExist, ofNoNetworkButton, ofEnableSizing]
    Title = 'Select model'
    Left = 16
    Top = 80
  end
end
