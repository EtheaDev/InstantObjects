inherited InstantWizardForm: TInstantWizardForm
  Caption = 'Wizard'
  OldCreateOrder = True
  ExplicitWidth = 410
  ExplicitHeight = 292
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonPanel: TPanel
    TabOrder = 1
    object BackButton: TButton
      Left = 156
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '< Back'
      TabOrder = 0
      OnClick = BackButtonClick
    end
    object NextButton: TButton
      Left = 236
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Next >'
      Default = True
      TabOrder = 1
      OnClick = NextButtonClick
    end
    object CancelButton: TButton
      Left = 316
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 49
    Width = 394
    Height = 163
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object WizardPageControl: TPageControl
      Left = 0
      Top = 0
      Width = 394
      Height = 163
      ActivePage = StartSheet
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 0
      object StartSheet: TTabSheet
        Hint = 'This guide will...'
        Caption = 'Welcome'
        TabVisible = False
        object StartInfoLabel: TLabel
          Left = 16
          Top = 16
          Width = 160
          Height = 13
          Caption = 'The following steps will help you...'
        end
        object StartContinueLabel: TLabel
          Left = 16
          Top = 40
          Width = 107
          Height = 13
          Caption = 'Click Next to continue.'
        end
      end
    end
  end
  object HeaderPanel: TPanel
    Left = 0
    Top = 0
    Width = 394
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 2
    object HeaderBevel: TBevel
      Left = 0
      Top = 41
      Width = 394
      Height = 8
      Align = alBottom
      Shape = bsBottomLine
      ExplicitWidth = 402
    end
    object HeaderImage: TImage
      Left = 7
      Top = 7
      Width = 32
      Height = 32
    end
    object HeaderCaptionLabel: TLabel
      Left = 48
      Top = 8
      Width = 89
      Height = 13
      Caption = 'Header Caption'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object HeaderHintLabel: TLabel
      Left = 48
      Top = 24
      Width = 60
      Height = 13
      Caption = 'Header Hint.'
    end
  end
end
