object InstantEditForm: TInstantEditForm
  Left = 366
  Top = 273
  BorderIcons = [biSystemMenu]
  Caption = 'Edit'
  ClientHeight = 461
  ClientWidth = 404
  Color = clBtnFace
  Constraints.MaxWidth = 500
  Constraints.MinHeight = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object EditPanel: TPanel
    Left = 0
    Top = 0
    Width = 404
    Height = 430
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 430
    Width = 404
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonPanel: TPanel
      Left = 236
      Top = 0
      Width = 168
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
        Left = 83
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
    Left = 20
    Top = 402
  end
  object SubjectSource: TDataSource
    DataSet = SubjectExposer
    Left = 52
    Top = 402
  end
end
