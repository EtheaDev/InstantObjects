object InstantDialogForm: TInstantDialogForm
  Left = 309
  Top = 242
  Width = 410
  Height = 292
  BorderIcons = [biSystemMenu]
  Caption = 'Dialog'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonPanel: TPanel
    Left = 0
    Top = 217
    Width = 402
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object ButtonBevel: TBevel
      Left = 0
      Top = 0
      Width = 402
      Height = 8
      Align = alTop
      Shape = bsTopLine
    end
  end
end
