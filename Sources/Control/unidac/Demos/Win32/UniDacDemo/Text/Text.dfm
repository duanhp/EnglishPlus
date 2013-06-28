inherited TextFrame: TTextFrame
  Width = 443
  Height = 277
  Align = alClient
  object Splitter1: TSplitter
    Left = 0
    Top = 192
    Width = 443
    Height = 2
    Cursor = crVSplit
    Align = alTop
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 26
    Width = 443
    Height = 166
    Align = alTop
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 388
      Height = 24
      BevelOuter = bvNone
      Color = clBlue
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
      object Panel8: TPanel
        Left = 167
        Top = 1
        Width = 220
        Height = 22
        BevelOuter = bvNone
        TabOrder = 0
        object DBNavigator: TDBNavigator
          Left = 0
          Top = 0
          Width = 220
          Height = 22
          DataSource = DataSource
          Flat = True
          TabOrder = 0
        end
      end
    end
  end
  object meComments: TDBMemo
    Left = 0
    Top = 219
    Width = 443
    Height = 58
    Align = alClient
    DataField = 'TextField'
    DataSource = DataSource
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object ToolBar1: TPanel
    Left = 0
    Top = 194
    Width = 443
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Panel2: TPanel
      Left = 1
      Top = 0
      Width = 250
      Height = 24
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btLoad: TSpeedButton
        Left = 1
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Load from file'
        Flat = True
        Transparent = False
        OnClick = btLoadClick
      end
      object btSave: TSpeedButton
        Left = 84
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Save to file'
        Flat = True
        Transparent = False
        OnClick = btSaveClick
      end
      object btClear: TSpeedButton
        Left = 167
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Clear'
        Flat = True
        Transparent = False
        OnClick = btClearClick
      end
    end
  end
  object DataSource: TDataSource
    DataSet = Query
    OnStateChange = DataSourceStateChange
    Left = 375
    Top = 32
  end
  object Query: TUniQuery
    Connection = UniDACForm.UniConnection
    SQL.Strings = (
      'SELECT * FROM unidac_text')
    Left = 343
    Top = 32
  end
  object OpenDialog: TOpenDialog
    Left = 240
    Top = 259
  end
  object SaveDialog: TSaveDialog
    Left = 272
    Top = 259
  end
end
