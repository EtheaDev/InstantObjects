object InstantDialogForm: TInstantDialogForm
  Left = 309
  Top = 242
  BorderIcons = [biSystemMenu]
  Caption = 'Dialog'
  ClientHeight = 201
  ClientWidth = 394
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object ButtonPanel: TPanel
    Left = 0
    Top = 160
    Width = 394
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object ButtonBevel: TBevel
      Left = 0
      Top = 0
      Width = 394
      Height = 8
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 402
    end
  end
end
