inherited FilterAndIndexFrame: TFilterAndIndexFrame
  Width = 670
  Height = 270
  Align = alClient
  object DBGrid: TDBGrid
    Left = 166
    Top = 48
    Width = 504
    Height = 222
    Align = alClient
    DataSource = DataSource
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 670
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    Locked = True
    TabOrder = 1
    object ToolBar: TPanel
      Left = 0
      Top = 0
      Width = 651
      Height = 48
      Align = alLeft
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
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
      object Panel5: TPanel
        Left = 388
        Top = 1
        Width = 149
        Height = 22
        BevelOuter = bvNone
        TabOrder = 0
        object cbCalcFields: TCheckBox
          Left = 8
          Top = 0
          Width = 142
          Height = 23
          Caption = 'Calculated/Lookup fields'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 0
          OnClick = cbCalcFieldsClick
        end
      end
      object Panel2: TPanel
        Left = 167
        Top = 24
        Width = 483
        Height = 23
        BevelOuter = bvNone
        TabOrder = 1
        object cbFilter: TCheckBox
          Left = 380
          Top = 0
          Width = 54
          Height = 21
          Caption = 'Filter'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          TabOrder = 0
          OnClick = cbFilterClick
        end
        object edFilter: TEdit
          Left = 6
          Top = 2
          Width = 365
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = 'DEPTNO = 10'
          OnExit = edFilterExit
        end
      end
      object DBNavigator1: TDBNavigator
        Left = 167
        Top = 1
        Width = 220
        Height = 23
        DataSource = DataSource
        Flat = True
        TabOrder = 2
      end
      object Panel1: TPanel
        Left = 538
        Top = 1
        Width = 112
        Height = 22
        BevelOuter = bvNone
        TabOrder = 3
        object cbCacheCalcFields: TCheckBox
          Left = 9
          Top = 0
          Width = 100
          Height = 23
          Caption = 'CacheCalcFields'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 0
          OnClick = cbCacheCalcFieldsClick
        end
      end
      object Panel6: TPanel
        Left = 0
        Top = 24
        Width = 166
        Height = 23
        BevelOuter = bvNone
        TabOrder = 4
        object cbIndex: TCheckBox
          Left = 9
          Top = 3
          Width = 64
          Height = 15
          Caption = 'Index'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          TabOrder = 0
          OnClick = cbIndexClick
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 48
    Width = 166
    Height = 222
    Align = alLeft
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    object Panel8: TPanel
      Left = 0
      Top = 138
      Width = 166
      Height = 84
      Align = alBottom
      BevelOuter = bvNone
      Caption = 'Panel8'
      Color = clBlue
      TabOrder = 0
      object Panel7: TPanel
        Left = 1
        Top = 1
        Width = 164
        Height = 82
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 6
          Top = 3
          Width = 90
          Height = 13
          Caption = 'IndexFieldNames ='
        end
        object lbIndexFieldNames: TLabel
          Left = 6
          Top = 22
          Width = 126
          Height = 54
          AutoSize = False
          Transparent = False
          WordWrap = True
        end
      end
    end
    object lbFields: TListBox
      Left = 0
      Top = 0
      Width = 166
      Height = 138
      Align = alClient
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 1
      OnClick = lbFieldsClick
    end
  end
  object Query: TUniQuery
    Connection = UniDACForm.UniConnection
    SQL.Strings = (
      'SELECT * FROM emp')
    Debug = True
    RefreshOptions = [roAfterInsert, roAfterUpdate]
    Left = 272
    Top = 55
  end
  object DataSource: TDataSource
    DataSet = Query
    Left = 304
    Top = 55
  end
  object Query2: TUniQuery
    Connection = UniDACForm.UniConnection
    SQL.Strings = (
      'SELECT * FROM emp')
    Debug = True
    OnCalcFields = Query2CalcFields
    Left = 272
    Top = 88
    object Query2CALCULATED: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'CALCULATED'
      Calculated = True
    end
    object Query2LOOKUP: TStringField
      FieldKind = fkLookup
      FieldName = 'LOOKUP'
      LookupDataSet = LookupQuery
      LookupKeyFields = 'DEPTNO'
      LookupResultField = 'DNAME'
      KeyFields = 'DEPTNO'
      Lookup = True
    end
    object Query2EMPNO: TIntegerField
      FieldName = 'EMPNO'
      Required = True
    end
    object Query2ENAME: TStringField
      FieldName = 'ENAME'
      Size = 10
    end
    object Query2JOB: TStringField
      FieldName = 'JOB'
      Size = 9
    end
    object Query2MGR: TIntegerField
      FieldName = 'MGR'
    end
    object Query2HIREDATE: TDateTimeField
      FieldName = 'HIREDATE'
    end
    object Query2SAL: TFloatField
      FieldName = 'SAL'
    end
    object Query2COMM: TFloatField
      FieldName = 'COMM'
    end
    object Query2DEPTNO: TIntegerField
      FieldName = 'DEPTNO'
    end
  end
  object LookupQuery: TUniQuery
    Connection = UniDACForm.UniConnection
    SQL.Strings = (
      'SELECT * FROM dept')
    Left = 304
    Top = 88
    object LookupQueryDEPTNO: TIntegerField
      FieldName = 'DEPTNO'
      Required = True
    end
    object LookupQueryDNAME: TStringField
      FieldName = 'DNAME'
      Size = 14
    end
    object LookupQueryLOC: TStringField
      FieldName = 'LOC'
      Size = 13
    end
  end
end
