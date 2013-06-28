inherited UpdateSQLFrame: TUpdateSQLFrame
  Width = 443
  Height = 277
  Align = alClient
  object Splitter1: TSplitter
    Left = 0
    Top = 121
    Width = 443
    Height = 3
    Cursor = crVSplit
    Align = alTop
    Beveled = True
    Color = clBlue
    ParentColor = False
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 150
    Width = 443
    Height = 127
    Align = alClient
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object meSQL: TMemo
    Left = 0
    Top = 49
    Width = 443
    Height = 72
    Align = alTop
    Anchors = []
    Constraints.MinHeight = 50
    ScrollBars = ssVertical
    TabOrder = 1
    OnExit = meSQLExit
  end
  object Panel3: TPanel
    Left = 0
    Top = 124
    Width = 443
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 328
      Height = 26
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btRefreshRecord: TSpeedButton
        Left = 242
        Top = 1
        Width = 85
        Height = 24
        Caption = 'RefreshRecord'
        Flat = True
        Transparent = False
        OnClick = btRefreshRecordClick
      end
      object DBNavigator1: TDBNavigator
        Left = 1
        Top = 1
        Width = 240
        Height = 24
        DataSource = DataSource
        Flat = True
        TabOrder = 0
      end
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 3
    object ToolBar: TPanel
      Left = 0
      Top = 0
      Width = 431
      Height = 49
      Align = alLeft
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btUnPrepare: TSpeedButton
        Left = 259
        Top = 1
        Width = 85
        Height = 22
        Caption = 'UnPrepare'
        Flat = True
        Transparent = False
        OnClick = btUnPrepareClick
      end
      object btPrepare: TSpeedButton
        Left = 173
        Top = 1
        Width = 85
        Height = 22
        Caption = 'Prepare'
        Flat = True
        Transparent = False
        OnClick = btPrepareClick
      end
      object btOpen: TSpeedButton
        Left = 1
        Top = 1
        Width = 85
        Height = 22
        Caption = 'Open'
        Flat = True
        Transparent = False
        OnClick = btOpenClick
      end
      object btExecute: TSpeedButton
        Left = 345
        Top = 1
        Width = 85
        Height = 22
        Caption = 'Execute'
        Flat = True
        Transparent = False
        OnClick = btExecuteClick
      end
      object btClose: TSpeedButton
        Left = 87
        Top = 1
        Width = 85
        Height = 22
        Caption = 'Close'
        Flat = True
        Transparent = False
        OnClick = btCloseClick
      end
      object Panel4: TPanel
        Left = 2
        Top = 24
        Width = 428
        Height = 24
        BevelOuter = bvNone
        TabOrder = 0
        object cbDeleteObject: TCheckBox
          Left = 14
          Top = 5
          Width = 91
          Height = 17
          Caption = 'DeleteObject'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 0
          OnClick = cbObjectClick
        end
        object cbInsertObject: TCheckBox
          Left = 112
          Top = 5
          Width = 89
          Height = 17
          Caption = 'InsertObject'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 1
          OnClick = cbObjectClick
        end
        object cbModifyObject: TCheckBox
          Left = 210
          Top = 5
          Width = 87
          Height = 17
          Caption = 'ModifyObject'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 2
          OnClick = cbObjectClick
        end
        object cbRefreshObject: TCheckBox
          Left = 314
          Top = 5
          Width = 95
          Height = 17
          Caption = 'RefreshObject'
          Color = clBtnFace
          ParentColor = False
          TabOrder = 3
          OnClick = cbObjectClick
        end
      end
    end
  end
  object UniQuery: TUniQuery
    SQL.Strings = (
      'SELECT * FROM dept'
      '')
    Debug = True
    RefreshOptions = [roAfterInsert, roAfterUpdate]
    UpdateObject = UniUpdateSQL
    Left = 24
    Top = 200
  end
  object DataSource: TDataSource
    DataSet = UniQuery
    Left = 24
    Top = 232
  end
  object UniUpdateSQL: TUniUpdateSQL
    InsertSQL.Strings = (
      'INSERT INTO dept'
      '  (deptno, dname, loc)'
      'VALUES'
      '  (:deptno, :dname, :loc)')
    DeleteSQL.Strings = (
      'DELETE FROM dept'
      'WHERE'
      '  deptno = :deptno')
    ModifySQL.Strings = (
      'UPDATE dept'
      'SET'
      '  deptno = :deptno,'
      '  dname = :dname,'
      '  loc = :loc'
      'WHERE'
      '  deptno = :old_deptno')
    RefreshSQL.Strings = (
      'SELECT * FROM dept'
      'WHERE'
      '  deptno = :deptno')
    Left = 56
    Top = 200
  end
  object RefreshQuery: TUniQuery
    SQL.Strings = (
      'SELECT * FROM dept'
      'WHERE'
      '  deptno = :deptno')
    Left = 96
    Top = 256
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'deptno'
      end>
  end
  object ModifyQuery: TUniQuery
    SQL.Strings = (
      'UPDATE dept'
      'SET'
      '  deptno = :deptno,'
      '  dname = :dname,'
      '  loc = :loc'
      'WHERE'
      '  deptno = :old_deptno')
    Left = 96
    Top = 224
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'deptno'
      end
      item
        DataType = ftUnknown
        Name = 'dname'
      end
      item
        DataType = ftUnknown
        Name = 'loc'
      end
      item
        DataType = ftUnknown
        Name = 'old_deptno'
      end>
  end
  object DeleteQuery: TUniQuery
    SQL.Strings = (
      'DELETE FROM dept'
      'WHERE'
      '  deptno = :deptno')
    Left = 96
    Top = 160
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'deptno'
      end>
  end
  object InsertQuery: TUniQuery
    SQL.Strings = (
      'INSERT INTO dept'
      '  (deptno, dname, loc)'
      'VALUES'
      '  (:deptno, :dname, :loc)')
    Left = 96
    Top = 192
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'deptno'
      end
      item
        DataType = ftUnknown
        Name = 'dname'
      end
      item
        DataType = ftUnknown
        Name = 'loc'
      end>
  end
end
