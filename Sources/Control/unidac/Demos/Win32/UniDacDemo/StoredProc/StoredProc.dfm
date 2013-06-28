inherited StoredProcFrame: TStoredProcFrame
  Width = 443
  Height = 277
  Align = alClient
  object Splitter1: TSplitter
    Left = 0
    Top = 220
    Width = 443
    Height = 3
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 223
    Width = 443
    Height = 54
    Align = alClient
    DataSource = DataSource
    TabOrder = 1
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
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 431
      Height = 25
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
      object btExecute: TSpeedButton
        Left = 173
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Execute'
        Flat = True
        Transparent = False
        OnClick = btExecuteClick
      end
      object btPrepare: TSpeedButton
        Left = 259
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Prepare'
        Flat = True
        Transparent = False
        OnClick = btPrepareClick
      end
      object btUnPrepare: TSpeedButton
        Left = 345
        Top = 1
        Width = 85
        Height = 24
        Caption = 'UnPrepare'
        Flat = True
        Transparent = False
        OnClick = btUnPrepareClick
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 24
      Width = 431
      Height = 26
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 1
      object btParameters: TSpeedButton
        Left = 259
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Parameters'
        Flat = True
        Transparent = False
        OnClick = btParametersClick
      end
      object btPrepareSQL: TSpeedButton
        Left = 345
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Prepare SQL'
        Flat = True
        Transparent = False
        OnClick = btPrepareSQLClick
      end
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 257
        Height = 24
        BevelOuter = bvNone
        TabOrder = 0
        object lbStoredProcName: TLabel
          Left = 2
          Top = 5
          Width = 111
          Height = 13
          Caption = 'Stored procedure name'
        end
        object edStoredProcNames: TComboBox
          Left = 117
          Top = 1
          Width = 136
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = edStoredProcNamesChange
          OnDropDown = edStoredProcNamesDropDown
        end
      end
    end
  end
  object meSQL: TMemo
    Left = 0
    Top = 50
    Width = 443
    Height = 170
    Align = alTop
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object DataSource: TDataSource
    DataSet = UniStoredProc
    Left = 317
    Top = 67
  end
  object UniStoredProc: TUniStoredProc
    Left = 280
    Top = 65
  end
end
