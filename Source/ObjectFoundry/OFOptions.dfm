inherited OFOptionsForm: TOFOptionsForm
  Left = 340
  Top = 259
  ActiveControl = ProjectFileNameEdit
  Caption = 'ObjectFoundry Options'
  ClientHeight = 249
  ClientWidth = 348
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited EditPanel: TPanel
    Width = 348
    Height = 218
    object PageControl: TPageControl
      Left = 4
      Top = 4
      Width = 340
      Height = 210
      ActivePage = ProjectSheet
      Align = alClient
      TabOrder = 0
      object ProjectSheet: TTabSheet
        Caption = 'Project'
        object ProjectFileEdit: TLabel
          Left = 16
          Top = 16
          Width = 52
          Height = 13
          Caption = '&Project File'
        end
        object ProjectFileNameEdit: TDBEdit
          Left = 16
          Top = 32
          Width = 276
          Height = 21
          DataField = 'ProjectFileName'
          DataSource = SubjectSource
          TabOrder = 0
        end
        object ProjectFileButton: TButton
          Left = 296
          Top = 32
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 1
        end
      end
    end
  end
  inherited ButtonPanel: TPanel
    Top = 218
    Width = 348
    Height = 31
    inherited OkButton: TButton
      Left = 189
      Top = 2
    end
    inherited CancelButton: TButton
      Left = 269
      Top = 2
    end
  end
  inherited SubjectExposer: TInstantExposer
    Top = 218
  end
  inherited SubjectSource: TDataSource
    Top = 218
  end
end
