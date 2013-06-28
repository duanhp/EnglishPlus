inherited PicturesFrame: TPicturesFrame
  Width = 443
  Height = 277
  Align = alClient
  object Splitter1: TSplitter
    Left = 0
    Top = 119
    Width = 443
    Height = 2
    Cursor = crVSplit
    Align = alTop
    Color = clBtnFace
    ParentColor = False
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 146
    Width = 443
    Height = 131
    Align = alClient
    TabOrder = 0
    object DBImage: TDBImage
      Left = 0
      Top = 0
      Width = 439
      Height = 127
      Align = alClient
      Center = False
      Constraints.MinHeight = 50
      DataField = 'PICTURE'
      DataSource = dsPictures
      TabOrder = 0
    end
  end
  object ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 0
      Top = 1
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
        DataSource = dsPictures
        Flat = True
        TabOrder = 0
      end
    end
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 26
    Width = 443
    Height = 93
    Align = alTop
    Constraints.MinHeight = 50
    DataSource = dsPictures
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete]
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ToolBar1: TPanel
    Left = 0
    Top = 121
    Width = 443
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Panel1: TPanel
      Left = 2
      Top = 0
      Width = 259
      Height = 24
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btLoad: TSpeedButton
        Left = 1
        Top = 1
        Width = 85
        Height = 22
        Caption = 'Load from file ...'
        Flat = True
        Transparent = False
        OnClick = btLoadClick
      end
      object btSave: TSpeedButton
        Left = 87
        Top = 1
        Width = 85
        Height = 22
        Caption = 'Save to file ...'
        Flat = True
        Transparent = False
        OnClick = btSaveClick
      end
      object btClear: TSpeedButton
        Left = 173
        Top = 1
        Width = 85
        Height = 22
        Caption = 'Clear'
        Flat = True
        Transparent = False
        OnClick = btClearClick
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 288
    Top = 115
  end
  object SavePictureDialog: TSavePictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 320
    Top = 116
  end
  object dsPictures: TDataSource
    DataSet = quPictures
    Left = 376
    Top = 16
  end
  object quPictures: TUniQuery
    Connection = UniDACForm.UniConnection
    SQL.Strings = (
      'SELECT * FROM unidac_blob')
    Debug = True
    FetchRows = 1
    Left = 344
    Top = 16
  end
end
