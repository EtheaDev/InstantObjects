object BasicEditForm: TBasicEditForm
  Left = 326
  Top = 270
  Anchors = [akTop, akRight]
  BorderStyle = bsDialog
  Caption = 'Edit'
  ClientHeight = 255
  ClientWidth = 385
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
  object ButtonPanel: TPanel
    Left = 0
    Top = 224
    Width = 385
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object OkButton: TButton
      Left = 228
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OkButtonClick
    end
    object CancelButton: TButton
      Left = 308
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 224
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 1
    object PageControl: TPageControl
      Left = 2
      Top = 2
      Width = 381
      Height = 220
      ActivePage = DetailsSheet
      Align = alClient
      TabOrder = 0
      object DetailsSheet: TTabSheet
        Caption = 'Details'
        TabVisible = False
      end
    end
  end
  object SubjectExposer: TInstantExposer
    Left = 2
    Top = 225
  end
  object SubjectSource: TDataSource
    DataSet = SubjectExposer
    Left = 34
    Top = 225
  end
end
