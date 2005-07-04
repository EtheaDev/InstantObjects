inherited InstantDBBuilderForm: TInstantDBBuilderForm
  Caption = 'Database Builder'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
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
