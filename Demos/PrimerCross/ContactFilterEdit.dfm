inherited ContactFilterEditForm: TContactFilterEditForm
  Caption = 'Contact Filter'
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    00066000000000000006600000000000000F800000000000000F800000000000
    0088860000000000068886600000000068F88866000000068F88888660000068
    FFF888866600000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000FC3F0000FC3F0000FC3F0000FC3F0000FC3F0000F81F
    0000F00F0000E0070000C00300008001000080010000FFFF0000FFFF0000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited ClientPanel: TPanel
    inherited PageControl: TPageControl
      inherited DetailsSheet: TTabSheet
        inherited PhonesLabel: TLabel
          Visible = False
        end
        inherited PhonesGrid: TDBGrid
          TabOrder = 8
          Visible = False
        end
        object DynamicCheckBox: TDBCheckBox
          Left = 143
          Top = 202
          Width = 66
          Height = 17
          Caption = '&Dynamic'
          DataField = 'IsDynamic'
          DataSource = SubjectSource
          TabOrder = 7
          ValueChecked = 'True'
          ValueUnchecked = 'False'
        end
        object InfoPanel: TPanel
          Left = 228
          Top = 6
          Width = 190
          Height = 217
          BevelInner = bvLowered
          Color = clInfoBk
          TabOrder = 9
          object Info1Label: TLabel
            Left = 16
            Top = 16
            Width = 153
            Height = 80
            AutoSize = False
            Caption = 
              'Use the fields to the left to specify values or part of values t' +
              'hat must be matched for objects to be included in the view.'
            WordWrap = True
          end
          object Info2Label: TLabel
            Left = 16
            Top = 100
            Width = 153
            Height = 70
            AutoSize = False
            Caption = 
              'Uncheck Dynamic to apply the filter on all objects at once inste' +
              'ad of dynamically when browsing.'
            WordWrap = True
          end
        end
      end
    end
  end
  inherited SubjectExposer: TInstantExposer
    ObjectClassName = 'TContactFilter'
  end
end
