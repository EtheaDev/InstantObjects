object InstantEditForm: TInstantEditForm
  Left = 366
  Top = 273
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit'
  ClientHeight = 193
  ClientWidth = 289
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
    Height = 160
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 160
    Width = 289
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkButton: TButton
      Left = 130
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object CancelButton: TButton
      Left = 210
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = CancelButtonClick
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
