inherited PerformanceViewForm: TPerformanceViewForm
  Width = 749
  Height = 564
  ExplicitWidth = 749
  ExplicitHeight = 564
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 749
    Height = 564
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWhite
    TabOrder = 0
    object InfoPanel: TPanel
      Left = 0
      Top = 0
      Width = 745
      Height = 121
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      TabOrder = 0
      DesignSize = (
        745
        121)
      object TitleLabel: TLabel
        Left = 16
        Top = 4
        Width = 143
        Height = 15
        Caption = 'Database Performance Test'
      end
      object InfoBevel: TBevel
        Left = 0
        Top = 112
        Width = 745
        Height = 9
        Align = alBottom
        Shape = bsBottomLine
      end
      object ConnectionLabel: TLabel
        Left = 280
        Top = 4
        Width = 149
        Height = 15
        Caption = 'Connection: Not Connected'
      end
      object ObjectsLabel: TLabel
        Left = 16
        Top = 72
        Width = 86
        Height = 15
        Caption = '&Objects to store:'
        FocusControl = ObjectsEdit
      end
      object CacheSizeLabel: TLabel
        Left = 328
        Top = 72
        Width = 116
        Height = 15
        Caption = 'Statement &Cache Size:'
        FocusControl = CacheSizeEdit
      end
      object RunButton: TButton
        Left = 513
        Top = 81
        Width = 105
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Run Now'
        TabOrder = 7
        OnClick = RunButtonClick
      end
      object InfoMemo: TMemo
        Left = 16
        Top = 23
        Width = 713
        Height = 46
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        Lines.Strings = (
          
            'This page allows you to run a performance test on the current co' +
            'nnection. You can measure the speed of store, '
          
            'retrieve query and dispose operations. Test results can be compa' +
            'red to other connections in the chart below.')
        ReadOnly = True
        TabOrder = 0
      end
      object TransactionsCheckBox: TCheckBox
        Left = 207
        Top = 88
        Width = 112
        Height = 17
        Caption = 'Use &Transactions'
        TabOrder = 5
        OnClick = TransactionsCheckBoxClick
      end
      object ObjectsEdit: TEdit
        Left = 16
        Top = 86
        Width = 79
        Height = 21
        NumbersOnly = True
        TabOrder = 1
        Text = '1000'
      end
      object TestRetrieveCheckBox: TCheckBox
        Left = 109
        Top = 70
        Width = 95
        Height = 17
        Caption = 'Test Retrieve'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = TestRetrieveCheckBoxClick
      end
      object TestDisposeCheckBox: TCheckBox
        Left = 109
        Top = 100
        Width = 95
        Height = 17
        Caption = 'Test Dispose'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = TestDisposeCheckBoxClick
      end
      object TestQueryCheckBox: TCheckBox
        Left = 109
        Top = 85
        Width = 95
        Height = 17
        Caption = 'Test Query'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object CacheSizeEdit: TEdit
        Left = 328
        Top = 86
        Width = 113
        Height = 21
        NumbersOnly = True
        TabOrder = 6
        Text = '-1'
      end
      object ClearAllButton: TButton
        Left = 624
        Top = 81
        Width = 105
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Clear all results'
        TabOrder = 8
        OnClick = ClearAllButtonClick
      end
    end
    object ResultPanel: TPanel
      Left = 0
      Top = 121
      Width = 745
      Height = 439
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object ResultListPanel: TPanel
        Left = 0
        Top = 0
        Width = 209
        Height = 439
        Align = alLeft
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 0
        object TestResultListView: TListView
          Left = 4
          Top = 4
          Width = 201
          Height = 431
          Align = alClient
          Checkboxes = True
          Columns = <
            item
              AutoSize = True
              Caption = 'Tested Connections'
            end>
          ColumnClick = False
          PopupMenu = TestResultMenu
          TabOrder = 0
          ViewStyle = vsReport
          OnChange = TestResultListViewChange
        end
      end
      object ChartPanel: TPanel
        Left = 209
        Top = 0
        Width = 536
        Height = 439
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object TestResultChart: TChart
          Left = 0
          Top = 0
          Width = 536
          Height = 439
          LeftWall.Color = clWhite
          Title.Text.Strings = (
            'Results')
          Title.Visible = False
          BottomAxis.LabelStyle = talText
          LeftAxis.AxisValuesFormat = '##0'
          LeftAxis.Title.Caption = 'Objects per second'
          Align = alClient
          BevelOuter = bvNone
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          DefaultCanvas = 'TGDIPlusCanvas'
          ColorPaletteIndex = 13
          object TestResultStoreSeries: TBarSeries
            HoverElement = []
            Marks.Style = smsValue
            SeriesColor = clGreen
            Title = 'Store'
            ValueFormat = '#,##0'
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Bar'
            YValues.Order = loNone
            Left = 120
            Top = 344
          end
          object TestResultRetrieveSeries: TBarSeries
            HoverElement = []
            Marks.Style = smsValue
            SeriesColor = 8454143
            Title = 'Retrieve'
            ValueFormat = '#,##0'
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Bar'
            YValues.Order = loNone
            Left = 120
            Top = 288
          end
          object Series1: TBarSeries
            HoverElement = []
            Marks.Style = smsValue
            SeriesColor = 16744576
            Title = 'Query'
            ValueFormat = '#,##0'
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Bar'
            YValues.Order = loNone
            Left = 120
            Top = 224
          end
          object TestResultDisposeSeries: TBarSeries
            HoverElement = []
            Marks.Style = smsValue
            Title = 'Dispose'
            ValueFormat = '#,##0'
            XValues.Name = 'X'
            XValues.Order = loAscending
            YValues.Name = 'Bar'
            YValues.Order = loNone
            Left = 120
            Top = 160
          end
        end
      end
    end
  end
  object TestResultMenu: TPopupMenu
    OnPopup = TestResultMenuPopup
    Left = 18
    Top = 187
    object TestResultRenameItem: TMenuItem
      Caption = '&Rename'
      Hint = 'Rename'
      ShortCut = 113
      OnClick = TestResultRenameItemClick
    end
    object TestResultDeleteItem: TMenuItem
      Caption = '&Delete'
      Hint = 'Delete'
      ShortCut = 16452
      OnClick = TestResultDeleteItemClick
    end
  end
end
