object ClassRegWizardForm: TClassRegWizardForm
  Left = 359
  Top = 238
  BorderStyle = bsDialog
  Caption = 'ObjectFoundry Class Registration'
  ClientHeight = 225
  ClientWidth = 287
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
  object OkButton: TButton
    Left = 200
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 200
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object UnitListView: TListView
    Left = 8
    Top = 8
    Width = 177
    Height = 209
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'Unit'
      end>
    TabOrder = 2
    ViewStyle = vsReport
  end
end
