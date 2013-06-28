inherited DumpFrame: TDumpFrame
  Width = 443
  Height = 270
  VertScrollBar.Range = 123
  Align = alClient
  AutoScroll = False
  object meSQL: TMemo
    Left = 0
    Top = 194
    Width = 443
    Height = 76
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 194
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 463
      Height = 191
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btBackup: TSpeedButton
        Left = 1
        Top = 97
        Width = 82
        Height = 22
        Caption = 'Backup Tables'
        Flat = True
        Transparent = False
        OnClick = btBackupClick
      end
      object btBackupSQL: TSpeedButton
        Left = 84
        Top = 97
        Width = 82
        Height = 22
        Caption = 'Backup SQL'
        Flat = True
        Transparent = False
        OnClick = btBackupSQLClick
      end
      object btRestore: TSpeedButton
        Left = 167
        Top = 97
        Width = 82
        Height = 22
        Caption = 'Restore'
        Flat = True
        Transparent = False
        OnClick = btRestoreClick
      end
      object Panel6: TPanel
        Left = 1
        Top = 1
        Width = 461
        Height = 47
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 5
          Top = 4
          Width = 221
          Height = 13
          Caption = 'Table names to backup (with comma delimiter):'
        end
        object edTbNames: TEdit
          Left = 5
          Top = 20
          Width = 449
          Height = 21
          TabOrder = 0
          Text = 'emp'
        end
      end
      object Panel7: TPanel
        Left = 1
        Top = 49
        Width = 461
        Height = 47
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Left = 5
          Top = 2
          Width = 124
          Height = 13
          Caption = 'SQL statement to backup:'
        end
        object edQuery: TEdit
          Left = 5
          Top = 20
          Width = 448
          Height = 21
          TabOrder = 0
          Text = 'SELECT * FROM dept'
        end
      end
      object pnResult: TPanel
        Left = 1
        Top = 120
        Width = 461
        Height = 70
        BevelOuter = bvNone
        TabOrder = 2
        DesignSize = (
          461
          70)
        object lbTableName: TLabel
          Left = 91
          Top = 6
          Width = 3
          Height = 13
        end
        object Label3: TLabel
          Left = 10
          Top = 6
          Width = 78
          Height = 13
          Caption = 'Current table:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Pitch = fpVariable
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label4: TLabel
          Left = 10
          Top = 25
          Width = 131
          Height = 13
          Caption = 'Table backup progress:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ProgressBar: TProgressBar
          Left = 0
          Top = 46
          Width = 460
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Min = 0
          Max = 100
          TabOrder = 0
        end
      end
      object Panel4: TPanel
        Left = 250
        Top = 97
        Width = 109
        Height = 22
        BevelOuter = bvNone
        TabOrder = 3
        object cbAddDrop: TCheckBox
          Left = 5
          Top = 3
          Width = 101
          Height = 17
          Caption = 'Add drop'
          TabOrder = 0
        end
      end
      object Panel5: TPanel
        Left = 360
        Top = 97
        Width = 102
        Height = 22
        BevelOuter = bvNone
        TabOrder = 4
        object cbQuoteNames: TCheckBox
          Left = 5
          Top = 3
          Width = 92
          Height = 17
          Caption = 'Quote names'
          TabOrder = 0
        end
      end
    end
  end
  object UniDump: TUniDump
    SQL.Strings = (
      '')
    OnBackupProgress = UniDumpBackupProgress
    OnRestoreProgress = UniDumpRestoreProgress
    Connection = UniDACForm.UniConnection
    Left = 14
    Top = 208
  end
end
