inherited ConnectDialogFrame: TConnectDialogFrame
  Width = 443
  Height = 270
  Align = alClient
  object ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 1
      Top = 0
      Width = 404
      Height = 52
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
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
      object DBNavigator: TDBNavigator
        Left = 173
        Top = 1
        Width = 230
        Height = 22
        DataSource = DataSource
        Flat = True
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 1
        Top = 24
        Width = 402
        Height = 27
        BevelOuter = bvNone
        TabOrder = 1
        object rbInherited: TRadioButton
          Left = 216
          Top = 5
          Width = 113
          Height = 17
          Caption = 'Inherited connect'
          TabOrder = 0
          OnClick = rbInheritedClick
        end
        object rbMy: TRadioButton
          Left = 112
          Top = 5
          Width = 81
          Height = 17
          Caption = 'My connect'
          TabOrder = 1
          OnClick = rbMyClick
        end
        object rbDefault: TRadioButton
          Left = 8
          Top = 5
          Width = 65
          Height = 17
          Caption = 'Default'
          Checked = True
          TabOrder = 2
          TabStop = True
          OnClick = rbDefaultClick
        end
      end
    end
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 57
    Width = 443
    Height = 213
    Align = alClient
    DataSource = DataSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DataSource: TDataSource
    DataSet = UniQuery
    Left = 80
    Top = 56
  end
  object UniQuery: TUniQuery
    Connection = UniDACForm.UniConnection
    SQL.Strings = (
      'SELECT * FROM Dept')
    Left = 48
    Top = 56
  end
end
