object InstantXMLConnectionDefEditForm: TInstantXMLConnectionDefEditForm
  Left = 425
  Top = 292
  Caption = 'XML Connection'
  ClientHeight = 133
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object BottomBevel: TBevel
    Left = 0
    Top = 103
    Width = 294
    Height = 2
    Align = alBottom
    Shape = bsBottomLine
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 294
    Height = 103
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object XMLLabel: TLabel
      Left = 8
      Top = 11
      Width = 120
      Height = 13
      Caption = '&XML data root directory'
      FocusControl = RootDirEdit
    end
    object Label1: TLabel
      Left = 8
      Top = 60
      Width = 73
      Height = 13
      Caption = 'XML &encoding'
      FocusControl = RootDirEdit
    end
    object RootDirEdit: TEdit
      Left = 8
      Top = 26
      Width = 251
      Height = 21
      TabOrder = 0
    end
    object FolderButton: TButton
      Left = 260
      Top = 26
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = FolderButtonClick
    end
    object EncodingComboBox: TComboBox
      Left = 8
      Top = 75
      Width = 251
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 105
    Width = 294
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonsPanel: TPanel
      Left = 121
      Top = 0
      Width = 173
      Height = 28
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object OkButton: TButton
        Left = 5
        Top = 2
        Width = 80
        Height = 24
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object CancelButton: TButton
        Left = 88
        Top = 2
        Width = 80
        Height = 24
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
end
