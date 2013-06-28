inherited CachedUpdatesFrame: TCachedUpdatesFrame
  Width = 681
  Height = 270
  Align = alClient
  object DBGrid: TDBGrid
    Left = 0
    Top = 102
    Width = 681
    Height = 168
    Align = alClient
    DataSource = DataSource
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnDrawDataCell = DBGridDrawDataCell
  end
  object Panel8: TPanel
    Left = 0
    Top = 0
    Width = 681
    Height = 102
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ToolBar: TPanel
      Left = 1
      Top = 1
      Width = 667
      Height = 100
      BevelOuter = bvNone
      Color = clBlue
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object btOpen: TSpeedButton
        Left = 1
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Open'
        Flat = True
        Transparent = False
        OnClick = btOpenClick
      end
      object btClose: TSpeedButton
        Left = 84
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Close'
        Flat = True
        Transparent = False
        OnClick = btCloseClick
      end
      object RefreshRecord: TSpeedButton
        Left = 408
        Top = 1
        Width = 82
        Height = 22
        Caption = 'RefreshRecord'
        Flat = True
        Transparent = False
        OnClick = RefreshRecordClick
      end
      object DBNavigator: TDBNavigator
        Left = 167
        Top = 1
        Width = 240
        Height = 22
        DataSource = DataSource
        Flat = True
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 1
        Top = 47
        Width = 368
        Height = 52
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Left = 5
          Top = 5
          Width = 48
          Height = 13
          Caption = 'Updates'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Panel3: TPanel
          Left = 20
          Top = 23
          Width = 333
          Height = 24
          BevelOuter = bvNone
          Color = clBlue
          TabOrder = 0
          object btApply: TSpeedButton
            Left = 1
            Top = 1
            Width = 82
            Height = 22
            Caption = 'Apply'
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
            OnClick = btApplyClick
          end
          object btCommit: TSpeedButton
            Left = 84
            Top = 1
            Width = 82
            Height = 22
            Caption = 'Commit'
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
            OnClick = btCommitClick
          end
          object btCancel: TSpeedButton
            Left = 167
            Top = 1
            Width = 82
            Height = 22
            Caption = 'Cancel'
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
            OnClick = btCancelClick
          end
          object btRevertRecord: TSpeedButton
            Left = 250
            Top = 1
            Width = 82
            Height = 22
            Caption = 'Revert'
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
            OnClick = btRevertRecordClick
          end
        end
      end
      object Panel2: TPanel
        Left = 1
        Top = 24
        Width = 317
        Height = 22
        BevelOuter = bvNone
        TabOrder = 2
        object Label4: TLabel
          Left = 220
          Top = 5
          Width = 51
          Height = 13
          Caption = 'Batch Size'
        end
        object cbCachedUpdates: TCheckBox
          Left = 8
          Top = 4
          Width = 97
          Height = 12
          Caption = 'CachedUpdates'
          TabOrder = 0
          OnClick = cbCachedUpdatesClick
        end
        object cbCustomUpdate: TCheckBox
          Left = 113
          Top = 4
          Width = 97
          Height = 14
          Caption = 'Custom Update'
          TabOrder = 1
          OnClick = cbCustomUpdateClick
        end
        object edUpdateBatchSize: TEdit
          Left = 276
          Top = 1
          Width = 37
          Height = 21
          TabOrder = 2
          Text = '1'
        end
      end
      object Panel4: TPanel
        Left = 370
        Top = 47
        Width = 296
        Height = 52
        BevelOuter = bvNone
        TabOrder = 3
        object Label3: TLabel
          Left = 5
          Top = 5
          Width = 74
          Height = 13
          Caption = 'Transactions'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Panel5: TPanel
          Left = 24
          Top = 23
          Width = 250
          Height = 24
          BevelOuter = bvNone
          Color = clBlue
          TabOrder = 0
          object btStartTrans: TSpeedButton
            Left = 1
            Top = 1
            Width = 82
            Height = 22
            Caption = 'Start'
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
            OnClick = btStartTransClick
          end
          object btCommitTrans: TSpeedButton
            Left = 84
            Top = 1
            Width = 82
            Height = 22
            Caption = 'Commit'
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
            OnClick = btCommitTransClick
          end
          object btRollBackTrans: TSpeedButton
            Left = 167
            Top = 1
            Width = 82
            Height = 22
            Caption = 'Rollback'
            Flat = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = False
            OnClick = btRollbackTransClick
          end
        end
      end
      object Panel6: TPanel
        Left = 319
        Top = 24
        Width = 347
        Height = 22
        BevelOuter = bvNone
        TabOrder = 4
        object Label1: TLabel
          Left = 6
          Top = 4
          Width = 64
          Height = 13
          Caption = 'RecordTypes'
        end
        object cbDeleted: TCheckBox
          Left = 284
          Top = 4
          Width = 59
          Height = 17
          Caption = 'Deleted'
          TabOrder = 0
          OnClick = cbDeletedClick
        end
        object cbInserted: TCheckBox
          Left = 222
          Top = 4
          Width = 61
          Height = 17
          Caption = 'Inserted'
          TabOrder = 1
          OnClick = cbInsertedClick
        end
        object cbModified: TCheckBox
          Left = 155
          Top = 4
          Width = 67
          Height = 17
          Caption = 'Modified'
          TabOrder = 2
          OnClick = cbModifiedClick
        end
        object cbUnmodified: TCheckBox
          Left = 77
          Top = 4
          Width = 76
          Height = 17
          Caption = 'Unmodified'
          TabOrder = 3
          OnClick = cbUnmodifiedClick
        end
      end
      object Panel7: TPanel
        Left = 491
        Top = 1
        Width = 175
        Height = 22
        BevelOuter = bvNone
        TabOrder = 5
      end
    end
  end
  object DataSource: TDataSource
    DataSet = UniQuery
    OnStateChange = DataSourceStateChange
    OnDataChange = DataSourceDataChange
    Left = 233
    Top = 50
  end
  object UniQuery: TUniQuery
    SQL.Strings = (
      'SELECT * FROM emp')
    CachedUpdates = True
    OnUpdateError = UniQueryUpdateError
    OnCalcFields = UniQueryCalcFields
    Left = 205
    Top = 48
  end
end
