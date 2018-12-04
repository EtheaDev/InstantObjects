object DemoDataRequestForm: TDemoDataRequestForm
  Left = 412
  Top = 260
  Caption = 'Create Random Data'
  ClientHeight = 156
  ClientWidth = 297
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object AmountLabel: TLabel
    Left = 40
    Top = 75
    Width = 138
    Height = 13
    Caption = 'Number of contacts to create'
  end
  object InfoLabel: TLabel
    Left = 16
    Top = 16
    Width = 273
    Height = 33
    AutoSize = False
    Caption = 
      'The specified number of random contacts will be created and stor' +
      'ed in the current database.'
    WordWrap = True
  end
  object CountEdit: TEdit
    Left = 184
    Top = 72
    Width = 57
    Height = 21
    TabOrder = 0
    Text = '100'
    OnChange = CountEditChange
  end
  object OkButton: TButton
    Left = 143
    Top = 128
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 223
    Top = 128
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object PicturesCheckBox: TCheckBox
    Left = 40
    Top = 100
    Width = 201
    Height = 17
    Caption = 'Load people'#39's picture from files'
    TabOrder = 1
  end
end
