inherited QueryFrame: TQueryFrame
  Width = 675
  Height = 277
  Align = alClient
  object Splitter1: TSplitter
    Left = 0
    Top = 160
    Width = 675
    Height = 1
    Cursor = crVSplit
    Align = alTop
    Color = clBlue
    ParentColor = False
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 187
    Width = 675
    Height = 90
    Align = alClient
    Constraints.MinWidth = 50
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object TPanel
    Left = 0
    Top = 161
    Width = 675
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Panel9: TPanel
      Left = 1
      Top = 1
      Width = 564
      Height = 24
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btRefreshRecord: TSpeedButton
        Left = 232
        Top = 1
        Width = 82
        Height = 22
        Caption = 'RefreshRecord'
        Flat = True
        Transparent = False
        OnClick = btRefreshRecordClick
      end
      object btSaveToXML: TSpeedButton
        Left = 481
        Top = 1
        Width = 82
        Height = 22
        Caption = 'SaveToXML'
        Flat = True
        Transparent = False
        OnClick = btSaveToXMLClick
      end
      object btLockRecord: TSpeedButton
        Left = 315
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Lock'
        Flat = True
        Transparent = False
        OnClick = btLockRecordClick
      end
      object btUnLock: TSpeedButton
        Left = 398
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Unlock'
        Flat = True
        Transparent = False
        OnClick = btUnLockClick
      end
      object DBNavigator: TDBNavigator
        Left = 1
        Top = 1
        Width = 230
        Height = 22
        DataSource = DataSource
        Flat = True
        TabOrder = 0
      end
    end
  end
  object ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 675
    Height = 80
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Panel1: TPanel
      Left = -8
      Top = 0
      Width = 642
      Height = 78
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btClose: TSpeedButton
        Left = 92
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Close'
        Flat = True
        Transparent = False
        OnClick = btCloseClick
      end
      object btOpen: TSpeedButton
        Left = 9
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Open'
        Flat = True
        Transparent = False
        OnClick = btOpenClick
      end
      object btPrepare: TSpeedButton
        Left = 175
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Prepare'
        Flat = True
        Transparent = False
        OnClick = btPrepareClick
      end
      object btUnPrepare: TSpeedButton
        Left = 258
        Top = 1
        Width = 82
        Height = 22
        Caption = 'UnPrepare'
        Flat = True
        Transparent = False
        OnClick = btUnPrepareClick
      end
      object btExecute: TSpeedButton
        Left = 341
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Execute'
        Flat = True
        Transparent = False
        OnClick = btExecuteClick
      end
      object Panel2: TPanel
        Left = 503
        Top = 1
        Width = 138
        Height = 76
        BevelOuter = bvNone
        TabOrder = 0
        object cbRefreshBeforeEdit: TCheckBox
          Left = 11
          Top = 4
          Width = 121
          Height = 17
          Caption = 'Refresh BeforeEdit'
          TabOrder = 0
          OnClick = cbRefreshOptionsClick
        end
        object cbRefreshAfterInsert: TCheckBox
          Left = 11
          Top = 21
          Width = 121
          Height = 17
          Caption = 'Refresh AfterInsert'
          TabOrder = 1
          OnClick = cbRefreshOptionsClick
        end
        object cbRefreshAfterUpdate: TCheckBox
          Left = 11
          Top = 38
          Width = 121
          Height = 17
          Caption = 'Refresh AfterUpdate'
          TabOrder = 2
          OnClick = cbRefreshOptionsClick
        end
        object cbDMLRefresh: TCheckBox
          Left = 11
          Top = 55
          Width = 121
          Height = 17
          Caption = 'DML Refresh'
          TabOrder = 3
          OnClick = cbDMLRefreshClick
        end
      end
      object Panel5: TPanel
        Left = 282
        Top = 51
        Width = 220
        Height = 26
        BevelOuter = bvNone
        TabOrder = 1
        object StaticText1: TLabel
          Left = 6
          Top = 7
          Width = 54
          Height = 13
          Caption = 'FetchRows'
        end
        object edFetchRows: TEdit
          Left = 67
          Top = 2
          Width = 70
          Height = 21
          TabOrder = 0
          OnExit = edFetchRowsExit
        end
        object cbFetchAll: TCheckBox
          Left = 147
          Top = 5
          Width = 58
          Height = 17
          Caption = 'FetchAll'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = cbFetchAllClick
        end
      end
      object Panel6: TPanel
        Left = 9
        Top = 51
        Width = 272
        Height = 26
        BevelOuter = bvNone
        TabOrder = 2
        object Label5: TLabel
          Left = 7
          Top = 8
          Width = 22
          Height = 13
          Caption = 'Filter'
        end
        object edFilter: TEdit
          Left = 37
          Top = 3
          Width = 164
          Height = 21
          TabOrder = 0
          OnExit = edFilterExit
        end
        object cbFiltered: TCheckBox
          Left = 208
          Top = 5
          Width = 58
          Height = 17
          Caption = 'Filtered'
          TabOrder = 1
          OnClick = cbFilteredClick
        end
      end
      object Panel7: TPanel
        Left = 9
        Top = 24
        Width = 272
        Height = 26
        BevelOuter = bvNone
        TabOrder = 3
        object Label2: TLabel
          Left = 5
          Top = 7
          Width = 73
          Height = 13
          Caption = 'Updating Table'
        end
        object edUpdatingTable: TEdit
          Left = 87
          Top = 3
          Width = 179
          Height = 21
          TabOrder = 0
          OnExit = edUpdatingTableExit
        end
      end
      object Panel3: TPanel
        Left = 282
        Top = 24
        Width = 220
        Height = 26
        BevelOuter = bvNone
        TabOrder = 4
        object Label1: TLabel
          Left = 5
          Top = 7
          Width = 53
          Height = 13
          Caption = 'Lock mode'
        end
        object cmbLockMode: TComboBox
          Left = 70
          Top = 3
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 0
          Text = 'None'
          OnChange = cmbLockModeChange
          Items.Strings = (
            'None'
            'Optimistic'
            'Pessimistic')
        end
      end
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 80
    Width = 675
    Height = 80
    Align = alTop
    ScrollBars = ssVertical
    TabOrder = 3
    OnExit = edFetchRowsExit
  end
  object Panel4: TPanel
    Left = 416
    Top = 1
    Width = 78
    Height = 22
    BevelOuter = bvNone
    TabOrder = 4
  end
  object DataSource: TDataSource
    DataSet = UniQuery
    OnStateChange = DataSourceStateChange
    OnDataChange = DataSourceDataChange
    Left = 108
    Top = 105
  end
  object UniQuery: TUniQuery
    Connection = UniDACForm.UniConnection
    SQL.Strings = (
      'SELECT emp.empno, emp.ename, emp.job, dept.deptno, dept.dname'
      'FROM emp, dept'
      'WHERE dept.deptno = emp.deptno')
    Debug = True
    AfterOpen = UniQueryAfterOpen
    Left = 76
    Top = 105
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML (*.xml)|*.xml'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 140
    Top = 105
  end
end
