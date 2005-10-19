inherited InstantDBBuilderForm: TInstantDBBuilderForm
  Caption = 'Database Builder'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited EvolutionLogLabel: TLabel
    Width = 40
    Caption = 'Build log'
  end
  inherited SequenceListView: TListView
    Columns = <
      item
        Caption = 'Build sequence'
        Width = 400
      end>
  end
  inherited ActionList: TActionList
    inherited ShowSequenceAction: TAction
      Caption = 'Show Build Sequence'
    end
    inherited BuildAction: TAction
      Caption = 'Build Database'
    end
  end
  object DBBuilder: TInstantDBBuilder
    Left = 272
    Top = 96
  end
end
