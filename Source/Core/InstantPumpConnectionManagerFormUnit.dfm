inherited InstantPumpConnectionManagerForm: TInstantPumpConnectionManagerForm
  Caption = 'InstantPump Connection Manager'
  TextHeight = 13
  inherited ConnectionView: TListView
  end
  inherited BottomPanel: TPanel
    inherited ButtonsPanel: TPanel
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
