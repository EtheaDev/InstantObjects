inherited InstantDBBuilderForm: TInstantDBBuilderForm
  Caption = 'Database Builder'
  TextHeight = 13
  inherited EvolutionLogLabel: TLabel
    Width = 46
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
