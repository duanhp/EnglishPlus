inherited TableFrame: TTableFrame
  Width = 443
  Height = 277
  Align = alClient
  object DBGrid: TDBGrid
    Left = 0
    Top = 72
    Width = 443
    Height = 205
    Align = alClient
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 72
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 652
      Height = 70
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
      object btPrepare: TSpeedButton
        Left = 173
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Prepare'
        Flat = True
        Transparent = False
        OnClick = btPrepareClick
      end
      object btUnPrepare: TSpeedButton
        Left = 259
        Top = 1
        Width = 85
        Height = 24
        Caption = 'UnPrepare'
        Flat = True
        Transparent = False
        OnClick = btUnPrepareClick
      end
      object btExecute: TSpeedButton
        Left = 345
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Execute'
        Flat = True
        Transparent = False
        OnClick = btExecuteClick
      end
      object DBNavigator: TDBNavigator
        Left = 431
        Top = 1
        Width = 220
        Height = 24
        DataSource = DataSource
        Flat = True
        TabOrder = 0
      end
      object Panel5: TPanel
        Left = 1
        Top = 26
        Width = 171
        Height = 43
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 2
          Width = 56
          Height = 13
          Caption = 'Table name'
        end
        object edTableName: TEdit
          Left = 8
          Top = 18
          Width = 156
          Height = 21
          TabOrder = 0
          OnExit = edTableNameExit
        end
      end
      object Panel6: TPanel
        Left = 173
        Top = 26
        Width = 171
        Height = 43
        BevelOuter = bvNone
        TabOrder = 2
        object Label2: TLabel
          Left = 8
          Top = 2
          Width = 45
          Height = 13
          Caption = 'KeyFields'
        end
        object edKeyFields: TEdit
          Left = 8
          Top = 18
          Width = 156
          Height = 21
          TabOrder = 0
          OnExit = edKeyFieldsExit
        end
      end
      object Panel7: TPanel
        Left = 345
        Top = 26
        Width = 148
        Height = 43
        BevelOuter = bvNone
        TabOrder = 3
        object Label3: TLabel
          Left = 8
          Top = 2
          Width = 53
          Height = 13
          Caption = 'OrderFields'
        end
        object edOrderFields: TEdit
          Left = 8
          Top = 18
          Width = 133
          Height = 21
          TabOrder = 0
          OnExit = edOrderFieldsExit
        end
      end
      object Panel8: TPanel
        Left = 494
        Top = 26
        Width = 157
        Height = 43
        BevelOuter = bvNone
        TabOrder = 4
        object Label4: TLabel
          Left = 8
          Top = 2
          Width = 43
          Height = 13
          Caption = 'FilterSQL'
        end
        object edFilterSQL: TEdit
          Left = 8
          Top = 18
          Width = 142
          Height = 21
          TabOrder = 0
          OnExit = edFilterSQLExit
        end
      end
    end
  end
  object DataSource: TDataSource
    DataSet = UniTable
    Left = 399
    Top = 98
  end
  object UniTable: TUniTable
    TableName = 'dept'
    Left = 370
    Top = 95
  end
end
