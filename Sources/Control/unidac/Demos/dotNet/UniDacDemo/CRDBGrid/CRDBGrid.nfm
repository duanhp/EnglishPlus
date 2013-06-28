inherited CRDBGridFrame: TCRDBGridFrame
  Width = 443
  Height = 277
  Align = alClient
  object CRDBGrid: TCRDBGrid
    Left = 0
    Top = 50
    Width = 443
    Height = 227
    OptionsEx = [dgeLocalFilter, dgeLocalSorting]
    Align = alClient
    DataSource = UniDataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NAME'
        Title.Caption = 'PERSON|NAME'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'COUNTRY'
        Title.Caption = 'PERSON|ADDRESS|COUNTRY'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CITY'
        Title.Caption = 'PERSON|ADDRESS|CITY'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'STREET'
        Title.Caption = 'PERSON|ADDRESS|STREET'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BIRTHDATE'
        Title.Caption = 'PERSON|BIRTHDATE'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'JOB'
        Title.Caption = 'JOB|JOB NAME'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'HIREDATE'
        Title.Caption = 'JOB|HIREDATE'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SAL'
        Title.Caption = 'JOB|SAL'
        Width = 63
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'REMARKS'
        Width = 63
        Visible = True
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 414
      Height = 50
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
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
      object Panel3: TPanel
        Left = 1
        Top = 26
        Width = 412
        Height = 23
        BevelOuter = bvNone
        TabOrder = 0
        object chkFiltered: TCheckBox
          Left = 5
          Top = 4
          Width = 65
          Height = 17
          Caption = 'Filtered'
          TabOrder = 0
          OnClick = chkFilteredClick
        end
        object chkFilterBar: TCheckBox
          Left = 77
          Top = 4
          Width = 65
          Height = 17
          Action = actFilterBar
          TabOrder = 1
        end
        object chkSearchBar: TCheckBox
          Left = 153
          Top = 4
          Width = 81
          Height = 17
          Action = actSearchBar
          TabOrder = 2
        end
        object chkRecCount: TCheckBox
          Left = 239
          Top = 4
          Width = 89
          Height = 17
          Caption = 'Record Count'
          TabOrder = 3
          OnClick = chkRecCountClick
        end
        object chkStretch: TCheckBox
          Left = 333
          Top = 4
          Width = 57
          Height = 17
          Caption = 'Stretch'
          TabOrder = 4
          OnClick = chkStretchClick
        end
      end
      object DBNavigator1: TDBNavigator
        Left = 173
        Top = 1
        Width = 240
        Height = 24
        DataSource = UniDataSource
        Flat = True
        TabOrder = 1
      end
    end
  end
  object UniQuery: TUniQuery
    SQL.Strings = (
      'SELECT * FROM crgrid_test')
    Left = 40
    Top = 144
  end
  object UniDataSource: TUniDataSource
    DataSet = UniQuery
    Left = 72
    Top = 144
  end
  object ActionList1: TActionList
    Left = 104
    Top = 144
    object actSearchBar: TAction
      Caption = 'Search Bar'
      OnExecute = actSearchBarExecute
      OnUpdate = actSearchBarUpdate
    end
    object actFilterBar: TAction
      Caption = 'Filter Bar'
      OnExecute = actFilterBarExecute
      OnUpdate = actFilterBarUpdate
    end
  end
end
