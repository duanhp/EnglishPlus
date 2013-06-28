inherited LoaderFrame: TLoaderFrame
  Width = 443
  Height = 270
  Align = alClient
  object DBGrid: TDBGrid
    Left = 0
    Top = 62
    Width = 443
    Height = 208
    Align = alClient
    DataSource = DataSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 62
    Align = alTop
    BevelOuter = bvNone
    Color = clBlue
    TabOrder = 0
    DesignSize = (
      443
      62)
    object btOpen: TSpeedButton
      Left = 1
      Top = 1
      Width = 85
      Height = 24
      Caption = 'Open'
      Flat = True
      Transparent = False
      OnClick = btOpenClick
    end
    object btClose: TSpeedButton
      Left = 87
      Top = 1
      Width = 85
      Height = 24
      Caption = 'Close'
      Flat = True
      Transparent = False
      OnClick = btCloseClick
    end
    object btLoad: TSpeedButton
      Left = 173
      Top = 1
      Width = 85
      Height = 24
      Caption = 'Load'
      Flat = True
      Transparent = False
      OnClick = btLoadClick
    end
    object btDeleteAll: TSpeedButton
      Left = 259
      Top = 1
      Width = 85
      Height = 24
      Caption = 'Delete All'
      Flat = True
      Transparent = False
      OnClick = btDeleteAllClick
    end
    object DBNavigator: TDBNavigator
      Left = 345
      Top = 1
      Width = 240
      Height = 24
      DataSource = DataSource
      Flat = True
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 1
      Top = 26
      Width = 584
      Height = 35
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 6
        Top = 14
        Width = 49
        Height = 13
        Caption = 'Load rows'
      end
      object rgEvent: TRadioGroup
        Left = 169
        Top = 2
        Width = 231
        Height = 30
        Columns = 2
        Items.Strings = (
          'GetColumnData'
          'PutData')
        TabOrder = 0
        OnClick = rgEventClick
      end
      object edRows: TEdit
        Left = 66
        Top = 10
        Width = 87
        Height = 21
        TabOrder = 1
        Text = '1000'
      end
    end
    object Panel2: TPanel
      Left = 586
      Top = 0
      Width = 1049
      Height = 62
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object DataSource: TDataSource
    DataSet = Query
    Left = 56
    Top = 109
  end
  object Query: TUniQuery
    Connection = UniDACForm.UniConnection
    SQL.Strings = (
      'SELECT * FROM unidac_loaded')
    Debug = True
    BeforeFetch = QueryBeforeFetch
    AfterFetch = QueryAfterFetch
    AfterOpen = QueryAfterOpen
    BeforeClose = QueryBeforeClose
    AfterRefresh = QueryAfterOpen
    Left = 24
    Top = 109
  end
  object UniLoader: TUniLoader
    Connection = UniDACForm.UniConnection
    TableName = 'unidac_loaded'
    Columns = <
      item
        Name = 'intg'
        FieldType = ftInteger
      end
      item
        Name = 'dbl'
        FieldType = ftFloat
      end
      item
        Name = 'str'
        Size = 50
      end
      item
        Name = 'dat'
        FieldType = ftDateTime
      end>
    OnPutData = PutData
    OnGetColumnData = GetColumnData
    Left = 24
    Top = 142
  end
end
