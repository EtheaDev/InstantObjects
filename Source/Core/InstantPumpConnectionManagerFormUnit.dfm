inherited InstantPumpConnectionManagerForm: TInstantPumpConnectionManagerForm
  Width = 449
  Height = 388
  Caption = 'InstantPump Connection Manager'
  PixelsPerInch = 96
  TextHeight = 13
  inherited ConnectionView: TListView
    Width = 441
    Height = 322
  end
  inherited BottomPanel: TPanel
    Top = 322
    Width = 441
    inherited ButtonsPanel: TPanel
      Left = 279
    end
    object PumpButton: TButton
      Left = 160
      Top = 4
      Width = 81
      Height = 25
      Action = PumpAction
      TabOrder = 3
    end
  end
  inherited ConnectionMenu: TPopupMenu
    object PumpDataItem: TMenuItem [7]
      Action = PumpAction
    end
  end
  inherited ActionList: TActionList
    object PumpAction: TAction
      Caption = 'Pump Data'
      Hint = 'Pump Data from connected to selected'
      OnExecute = PumpActionExecute
      OnUpdate = PumpActionUpdate
    end
  end
  object InstantPump: TInstantPump
    BeforePump = InstantPumpBeforePump
    AfterPump = InstantPumpAfterPump
    Left = 160
    Top = 48
  end
end
