object InstantEditForm: TInstantEditForm
  Left = 366
  Top = 273
  Width = 297
  Height = 220
  BorderIcons = [biSystemMenu]
  Caption = 'Edit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object EditPanel: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 155
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 155
    Width = 289
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonPanel: TPanel
      Left = 129
      Top = 0
      Width = 160
      Height = 31
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object OkButton: TButton
        Left = 2
        Top = 4
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = OkButtonClick
      end
      object CancelButton: TButton
        Left = 82
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
        OnClick = CancelButtonClick
      end
    end
  end
  object SubjectExposer: TInstantExposer
    Left = 4
    Top = 162
  end
  object SubjectSource: TDataSource
    DataSet = SubjectExposer
    Left = 36
    Top = 162
  end
end
