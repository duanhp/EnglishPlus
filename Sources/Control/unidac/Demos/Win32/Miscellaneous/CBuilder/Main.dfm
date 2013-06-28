object fmMain: TfmMain
  Left = 183
  Top = 181
  ActiveControl = meSQL
  Caption = 'Universal Data Access Demo - UniDAC for C++Builder'
  ClientHeight = 418
  ClientWidth = 703
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 111
    Width = 703
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 703
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btOpen: TButton
      Left = 151
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      OnClick = btOpenClick
    end
    object btClose: TButton
      Left = 226
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = btCloseClick
    end
    object btExecute: TButton
      Left = 301
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 2
      OnClick = btExecuteClick
    end
    object DBNavigator: TDBNavigator
      Left = 376
      Top = 0
      Width = 240
      Height = 25
      DataSource = DataSource
      TabOrder = 3
    end
    object btDisconnect: TButton
      Left = 76
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 4
      OnClick = btDisconnectClick
    end
    object btConnect: TButton
      Left = 1
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 5
      OnClick = btConnectClick
    end
  end
  object meSQL: TMemo
    Left = 0
    Top = 24
    Width = 703
    Height = 87
    Align = alTop
    TabOrder = 1
    OnExit = meSQLExit
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 114
    Width = 703
    Height = 304
    Align = alClient
    DataSource = DataSource
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object UniConnection: TUniConnection
    Database = 'test'
    ConnectDialog = UniConnectDialog
    Left = 352
    Top = 32
  end
  object UniConnectDialog: TUniConnectDialog
    DatabaseLabel = 'Database'
    PortLabel = 'Port'
    ProviderLabel = 'Provider'
    SavePassword = True
    Caption = 'Connect'
    UsernameLabel = 'User Name'
    PasswordLabel = 'Password'
    ServerLabel = 'Server'
    ConnectButton = 'Connect'
    CancelButton = 'Cancel'
    Left = 384
    Top = 32
  end
  object UniQuery: TUniQuery
    Connection = UniConnection
    SQL.Strings = (
      'SELECT * FROM EMP')
    Left = 416
    Top = 32
  end
  object DataSource: TDataSource
    DataSet = UniQuery
    Left = 448
    Top = 32
  end
end
