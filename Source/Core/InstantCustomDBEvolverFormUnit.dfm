object InstantCustomDBEvolverForm: TInstantCustomDBEvolverForm
  Left = 244
  Top = 191
  Caption = 'InstantCustomDBEvolverForm'
  ClientHeight = 331
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000070444000000088FF844444000077888F44444440007
    888FF444444400077888F44F44440007888FFF44444000077888F8F444000007
    888FFF88700000077888F8870000000780000087000000000FFFFF000000000F
    FFFFFFFF000000000FFFFF00000000000000000000000000000000000000FFFF
    0000F8230000E0010000C0000000C0000000C0000000C0010000C0030000C007
    0000C0070000C0070000C0070000C0070000E00F0000F83F0000FFFF0000}
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    585
    331)
  TextHeight = 13
  object EvolutionLogLabel: TLabel
    Left = 8
    Top = 176
    Width = 69
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Evolution log'
  end
  object ShowSequenceButton: TButton
    Left = 8
    Top = 8
    Width = 145
    Height = 25
    Action = ShowSequenceAction
    TabOrder = 0
  end
  object SequenceListView: TListView
    Left = 8
    Top = 40
    Width = 437
    Height = 129
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'Evolution sequence'
        Width = 400
      end>
    TabOrder = 2
    ViewStyle = vsReport
  end
  object EvolveButton: TButton
    Left = 160
    Top = 8
    Width = 145
    Height = 25
    Action = BuildAction
    TabOrder = 1
  end
  object MoveCommandUpButton: TButton
    Left = 450
    Top = 40
    Width = 129
    Height = 25
    Action = MoveCommandUpAction
    Anchors = [akTop, akRight]
    TabOrder = 3
  end
  object MoveCommandDownButton: TButton
    Left = 450
    Top = 72
    Width = 129
    Height = 25
    Action = MoveCommandDownAction
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object EvolutionLogMemo: TMemo
    Left = 8
    Top = 192
    Width = 571
    Height = 105
    Anchors = [akLeft, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 7
    WordWrap = False
  end
  object EnableAllButton: TButton
    Left = 450
    Top = 112
    Width = 129
    Height = 25
    Action = EnableAllCommandsAction
    Anchors = [akTop, akRight]
    TabOrder = 5
  end
  object DisableAllButton: TButton
    Left = 450
    Top = 144
    Width = 129
    Height = 25
    Action = DisableAllCommandsAction
    Anchors = [akTop, akRight]
    TabOrder = 6
  end
  object CloseButton: TButton
    Left = 502
    Top = 303
    Width = 75
    Height = 25
    Action = CloseAction
    Anchors = [akRight, akBottom]
    Cancel = True
    TabOrder = 8
  end
  object ActionList: TActionList
    Left = 320
    Top = 96
    object ShowSequenceAction: TAction
      Caption = 'Show Evolution Sequence'
      OnExecute = ShowSequenceActionExecute
    end
    object BuildAction: TAction
      Caption = 'Evolve Database'
      OnExecute = BuildActionExecute
      OnUpdate = BuildActionUpdate
    end
    object MoveCommandUpAction: TAction
      Caption = 'Move Command Up'
      OnExecute = MoveCommandUpActionExecute
      OnUpdate = MoveCommandUpActionUpdate
    end
    object MoveCommandDownAction: TAction
      Caption = 'Move Command Down'
      OnExecute = MoveCommandDownActionExecute
      OnUpdate = MoveCommandDownActionUpdate
    end
    object EnableAllCommandsAction: TAction
      Caption = 'Enable All Commands'
      OnExecute = EnableAllCommandsActionExecute
      OnUpdate = EnableAllCommandsActionUpdate
    end
    object DisableAllCommandsAction: TAction
      Caption = 'Disable All Commands'
      OnExecute = DisableAllCommandsActionExecute
      OnUpdate = DisableAllCommandsActionUpdate
    end
    object CloseAction: TAction
      Caption = 'Close'
      OnExecute = CloseActionExecute
    end
  end
end
