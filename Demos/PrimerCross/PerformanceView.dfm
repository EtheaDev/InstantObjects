inherited PerformanceViewForm: TPerformanceViewForm
  Width = 749
  Height = 564
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
      object TitleLabel: TLabel
        Left = 40
        Top = 4
        Width = 133
        Height = 13
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
        Width = 132
        Height = 13
        Caption = 'Connection: Not Connected'
      end
      object IconImage: TImage
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        AutoSize = True
        Picture.Data = {
          055449636F6E0000010002001010100000000000280100002600000020201000
          00000000E80200004E0100002800000010000000200000000100040000000000
          C000000000000000000000000000000000000000000000000000BF0000BF0000
          00BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000FF0000FF0000
          00FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000000000000000
          33000000000000333300000000003333FF300000003333F00F3000000333000F
          F0F300000033FFFFFF03000000333FFFFFF0300000033FFFFFF03000000333FF
          FFF77300000033FFF70073000000333F00FF03000000033F8F77000000000333
          8870700000000033087F88000000000000880000FF3F0000FC1F0000F01F0000
          C00F0000800F000080070000C0070000C0030000E0030000E0010000F0010000
          F0030000F8070000F8030000FC830000FFCF0000280000002000000040000000
          0100040000000000800200000000000000000000000000000000000000000000
          0000BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C00080808000
          0000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0000000000
          0000000000000000000000000000000000000000000330000000000000000000
          0000000003333000000000000000000000000003333333000000000000000000
          0000033333388300000000000000000000033333388F83300000000000000000
          033333388FFFF830000000000000000333333887F007F8330000000000000333
          333000000FF007830000000000033333000FFFFFFFFFF0833000000000333300
          FFFFFFF88FFFFF0830000000000333FFFFFFF88FFF88FFF033000000000333FF
          FFF88FFF88FFFFF0830000000000333FF88FFF88FFFF00FF033000000000333F
          FFFF88FFFF00CFFF0830000000000333FF88FFFF0099CCFFF033000000000377
          77FFFF00CFF99CFFF083000000000000777F0099CCF99CCFFF03300000000FFF
          00770FF99CFF99CFFF083000000FF777FF0770F99CCF99CCFFF0330008F77FCF
          77F070FFFCCFFFCCFFF0830008F8CFFFC7F0770FFFFFFFFF77FF03308F0FFF0F
          FF7F070FFFFFFF7007FF03308F0C0000FC7F07F0FFFF7008F03333308F0FFF0F
          FF7F0FF0FF7008FFF033300008F0CF0FC7F07FFFF008FFF77030000008F00F0F
          08F07FFFF8FFF77777000000008FF000FF0077FFF88777887700000080088FFF
          088003FF33388777F88000007800088000083333333087F88000000000000000
          00003333300008800000000000000800000003300000000000000000FFFFE7FF
          FFFF83FFFFFE03FFFFF801FFFFE001FFFF8000FFFE0000FFF800007FE000007F
          C000003FC000003FE000001FE000001FF000000FF000000FF8000007F8000007
          F8000003E0000003C00000018000000180000000000000000000000100000007
          8000001F8000001F8000001F0000001F38E0107FFDF079FFF8F9FFFF}
      end
      object ObjectsLabel: TLabel
        Left = 16
        Top = 72
        Width = 77
        Height = 13
        Caption = '&Objects to store:'
        FocusControl = ObjectsEdit
      end
      object CacheSizeLabel: TLabel
        Left = 328
        Top = 72
        Width = 108
        Height = 13
        Caption = 'Statement &Cache Size:'
        FocusControl = CacheSizeEdit
      end
      object RunButton: TButton
        Left = 480
        Top = 36
        Width = 105
        Height = 25
        Caption = '&Run Now'
        TabOrder = 7
        OnClick = RunButtonClick
      end
      object InfoMemo: TMemo
        Left = 40
        Top = 18
        Width = 437
        Height = 51
        TabStop = False
        Enabled = False
        Lines.Strings = (
          
            'This page allows you to run a performance test on the current co' +
            'nnection.'
          
            'You can measure the speed of store, retrieve query and dispose o' +
            'perations.'
          
            'Test results can be compared to other connections in the chart b' +
            'elow.')
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
      object ObjectsEdit: TMaskEdit
        Left = 16
        Top = 86
        Width = 81
        Height = 21
        EditMask = '#########;1; '
        MaxLength = 9
        TabOrder = 1
        Text = '500      '
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
      object CacheSizeEdit: TMaskEdit
        Left = 328
        Top = 86
        Width = 113
        Height = 21
        EditMask = '#########;1; '
        MaxLength = 9
        TabOrder = 6
        Text = '-1       '
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
          BackWall.Brush.Color = clWhite
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
          object TestResultStoreSeries: TBarSeries
            Marks.ArrowLength = 20
            Marks.Style = smsValue
            Marks.Visible = False
            SeriesColor = clGreen
            Title = 'Store'
            XValues.DateTime = False
            XValues.Name = 'X'
            XValues.Multiplier = 1.000000000000000000
            XValues.Order = loAscending
            YValues.DateTime = False
            YValues.Name = 'Bar'
            YValues.Multiplier = 1.000000000000000000
            YValues.Order = loNone
          end
          object TestResultRetrieveSeries: TBarSeries
            Marks.ArrowLength = 20
            Marks.Visible = False
            SeriesColor = 8454143
            Title = 'Retrieve'
            XValues.DateTime = False
            XValues.Name = 'X'
            XValues.Multiplier = 1.000000000000000000
            XValues.Order = loAscending
            YValues.DateTime = False
            YValues.Name = 'Bar'
            YValues.Multiplier = 1.000000000000000000
            YValues.Order = loNone
          end
          object Series1: TBarSeries
            Marks.ArrowLength = 20
            Marks.Visible = False
            SeriesColor = 16744576
            Title = 'Query'
            XValues.DateTime = False
            XValues.Name = 'X'
            XValues.Multiplier = 1.000000000000000000
            XValues.Order = loAscending
            YValues.DateTime = False
            YValues.Name = 'Bar'
            YValues.Multiplier = 1.000000000000000000
            YValues.Order = loNone
          end
          object TestResultDisposeSeries: TBarSeries
            Marks.ArrowLength = 20
            Marks.Visible = False
            SeriesColor = clRed
            Title = 'Dispose'
            XValues.DateTime = False
            XValues.Name = 'X'
            XValues.Multiplier = 1.000000000000000000
            XValues.Order = loAscending
            YValues.DateTime = False
            YValues.Name = 'Bar'
            YValues.Multiplier = 1.000000000000000000
            YValues.Order = loNone
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
