inherited MacrosFrame: TMacrosFrame
  Width = 535
  Height = 448
  VertScrollBar.Range = 123
  Align = alClient
  AutoScroll = False
  object meSQL: TMemo
    Left = 0
    Top = 210
    Width = 535
    Height = 127
    Align = alTop
    Lines.Strings = (
      'SELECT ename, job, sal {from}')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel4: TPanel
    Left = 0
    Top = 0
    Width = 535
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 123
      Height = 13
      Caption = 'Macros of UniConnection:'
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 185
    Width = 535
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 23
      Height = 13
      Caption = 'SQL:'
    end
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 362
    Width = 535
    Height = 86
    Align = alClient
    DataSource = DataSource1
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 25
    Width = 535
    Height = 160
    Align = alTop
    DataSource = DataSource2
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 337
    Width = 535
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 5
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 408
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
      object DBNavigator: TDBNavigator
        Left = 167
        Top = 1
        Width = 240
        Height = 22
        DataSource = DataSource1
        Flat = True
        TabOrder = 0
      end
    end
  end
  object UniQuery: TUniQuery
    Debug = True
    Left = 16
    Top = 256
  end
  object DataSource1: TDataSource
    DataSet = UniQuery
    Left = 56
    Top = 256
  end
  object vtMacros: TVirtualTable
    Active = True
    FieldDefs = <
      item
        Name = 'Name'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Value'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'Contidion'
        DataType = ftString
        Size = 50
      end>
    Left = 144
    Data = {
      0300030004004E616D650100140000000000050056616C756501003200000000
      000900436F6E746964696F6E0100320000000000000006000000090000007461
      626C656E616D6503000000656D7000000000090000007461626C656E616D650A
      0000007075626C69632E656D700A000000506F737467726553514C050000006C
      696D6974090000004C494D495420302C35050000004D7953514C050000006C69
      6D6974100000004C494D49542035204F464653455420300A000000506F737467
      726553514C0400000066726F6D1000000046524F4D207B7461626C656E616D65
      7D000000000400000066726F6D1800000046524F4D207B7461626C656E616D65
      7D207B6C696D69747D050000006C696D6974}
  end
  object DataSource2: TDataSource
    DataSet = vtMacros
    Left = 176
  end
end
