inherited SSLClientFrame: TSSLClientFrame
  Width = 506
  Height = 270
  Align = alClient
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 506
    Height = 267
    Align = alTop
    BevelOuter = bvNone
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 0
    object Panel4: TPanel
      Left = 1
      Top = 3
      Width = 249
      Height = 24
      BevelOuter = bvNone
      Color = 48127
      TabOrder = 0
      object btConnectDB: TSpeedButton
        Left = 1
        Top = 1
        Width = 123
        Height = 22
        Caption = 'Connect DB'
        Flat = True
        Transparent = False
        OnClick = btConnectDBClick
      end
      object btDisconnectDB: TSpeedButton
        Left = 125
        Top = 1
        Width = 123
        Height = 22
        Caption = 'Disconnect DB'
        Enabled = False
        Flat = True
        Transparent = False
        OnClick = btDisconnectDBClick
      end
    end
    object Panel5: TPanel
      Tag = 1
      Left = 1
      Top = 30
      Width = 661
      Height = 233
      BevelOuter = bvNone
      Color = 48127
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object Panel6: TPanel
        Left = 343
        Top = 1
        Width = 317
        Height = 183
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Left = 8
          Top = 3
          Width = 84
          Height = 13
          Caption = 'DB Connection '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label10: TLabel
          Left = 16
          Top = 56
          Width = 32
          Height = 13
          Caption = 'Server'
        end
        object Label11: TLabel
          Left = 16
          Top = 82
          Width = 20
          Height = 13
          Caption = 'Port'
        end
        object Label12: TLabel
          Left = 16
          Top = 108
          Width = 48
          Height = 13
          Caption = 'Username'
        end
        object Label13: TLabel
          Left = 16
          Top = 134
          Width = 46
          Height = 13
          Caption = 'Password'
        end
        object Label14: TLabel
          Left = 16
          Top = 160
          Width = 46
          Height = 13
          Caption = 'Database'
        end
        object Label6: TLabel
          Left = 16
          Top = 30
          Width = 40
          Height = 13
          Caption = 'Provider'
        end
        object edDBHost: TEdit
          Left = 98
          Top = 52
          Width = 200
          Height = 21
          TabOrder = 1
          OnChange = edDBHostChange
        end
        object edDBUserName: TEdit
          Left = 98
          Top = 104
          Width = 200
          Height = 21
          TabOrder = 3
          OnChange = edDBHostChange
        end
        object edDBPassword: TEdit
          Left = 98
          Top = 130
          Width = 200
          Height = 21
          PasswordChar = '*'
          TabOrder = 4
          OnChange = edDBHostChange
        end
        object seDBPort: TSpinEdit
          Left = 98
          Top = 78
          Width = 200
          Height = 22
          MaxValue = 65536
          MinValue = 0
          TabOrder = 2
          Value = 3306
          OnChange = edDBHostChange
        end
        object cbDBDatabase: TComboBox
          Left = 98
          Top = 156
          Width = 200
          Height = 21
          ItemHeight = 13
          TabOrder = 5
          OnChange = cbDBDatabaseChange
          OnDropDown = cbDBDatabaseDropDown
        end
        object cbProvider: TComboBox
          Left = 98
          Top = 26
          Width = 200
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          OnChange = edDBHostChange
        end
      end
      object Panel7: TPanel
        Left = 1
        Top = 185
        Width = 659
        Height = 22
        BevelOuter = bvNone
        TabOrder = 2
        object lbTableName: TLabel
          Left = 10
          Top = 6
          Width = 55
          Height = 13
          Caption = 'Table name'
        end
        object cbTableName: TComboBox
          Left = 98
          Top = 1
          Width = 307
          Height = 21
          DropDownCount = 16
          Enabled = False
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbTableNameChange
          OnDropDown = cbTableNameDropDown
        end
      end
      object Panel9: TPanel
        Left = 1
        Top = 208
        Width = 404
        Height = 24
        BevelOuter = bvNone
        Color = 48127
        TabOrder = 3
        object btOpen: TSpeedButton
          Left = 1
          Top = 1
          Width = 90
          Height = 22
          Caption = 'Open'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btOpenClick
        end
        object btClose: TSpeedButton
          Left = 92
          Top = 1
          Width = 90
          Height = 22
          Caption = 'Close'
          Enabled = False
          Flat = True
          Transparent = False
          OnClick = btCloseClick
        end
        object DBNavigator: TDBNavigator
          Left = 183
          Top = 1
          Width = 220
          Height = 22
          DataSource = DataSource
          Flat = True
          TabOrder = 0
        end
      end
      object Panel8: TPanel
        Left = 405
        Top = 208
        Width = 255
        Height = 24
        BevelOuter = bvNone
        TabOrder = 4
      end
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 341
        Height = 183
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 3
          Width = 89
          Height = 13
          Caption = 'SSL Connection '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label3: TLabel
          Left = 16
          Top = 54
          Width = 65
          Height = 13
          Caption = 'CA certificate'
        end
        object Label4: TLabel
          Left = 16
          Top = 80
          Width = 78
          Height = 13
          Caption = 'Client certificate'
        end
        object Label5: TLabel
          Left = 16
          Top = 106
          Width = 84
          Height = 13
          Caption = 'Client private key'
        end
        object sbCACertName: TSpeedButton
          Left = 301
          Top = 50
          Width = 23
          Height = 22
          Caption = '...'
          Flat = True
          Transparent = False
          OnClick = sbCACertNameClick
        end
        object sbCertName: TSpeedButton
          Left = 301
          Top = 76
          Width = 23
          Height = 22
          Caption = '...'
          Flat = True
          Transparent = False
          OnClick = sbCertNameClick
        end
        object sbKeyName: TSpeedButton
          Left = 301
          Top = 102
          Width = 23
          Height = 22
          Caption = '...'
          Flat = True
          Transparent = False
          OnClick = sbKeyNameClick
        end
        object edCACertName: TEdit
          Left = 101
          Top = 50
          Width = 200
          Height = 21
          TabOrder = 0
          Text = '.\ca-cert.pem'
          OnChange = edDBHostChange
        end
        object edKeyName: TEdit
          Left = 101
          Top = 102
          Width = 200
          Height = 21
          TabOrder = 2
          Text = '.\client.key'
          OnChange = edDBHostChange
        end
        object cbRandomization: TCheckBox
          Left = 184
          Top = 26
          Width = 118
          Height = 17
          Hint = 'Generation random data increase connection reliability'
          Caption = 'Silent randomization'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
        object cbSSL: TCheckBox
          Left = 99
          Top = 26
          Width = 76
          Height = 17
          Caption = 'Use SSL'
          Checked = True
          State = cbChecked
          TabOrder = 3
          OnClick = edDBHostChange
        end
        object edCertName: TEdit
          Left = 101
          Top = 76
          Width = 200
          Height = 21
          TabOrder = 1
          Text = '.\client-cert.pem'
          OnChange = edDBHostChange
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 267
    Width = 506
    Height = 3
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object DBGrid: TDBGrid
      Left = 0
      Top = 0
      Width = 506
      Height = 3
      Align = alClient
      DataSource = DataSource
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
  end
  object UniConnection: TUniConnection
    LoginPrompt = False
    AfterConnect = UniConnectionAfterConnect
    BeforeConnect = UniConnectionBeforeConnect
    AfterDisconnect = UniConnectionAfterConnect
    Left = 120
    Top = 169
  end
  object UniTable: TUniTable
    Connection = UniConnection
    AfterOpen = UniTableAfterOpen
    AfterClose = UniTableAfterClose
    Left = 152
    Top = 169
  end
  object DataSource: TDataSource
    DataSet = UniTable
    Left = 184
    Top = 169
  end
  object CRSSLIOHandler: TCRSSLIOHandler
    Storage = ScCryptoAPIStorage
    CertName = 'clientcert'
    CACertName = 'cacert'
    Left = 88
    Top = 168
  end
  object ScCryptoAPIStorage: TScCryptoAPIStorage
    CertProviderType = ptMemory
    Left = 56
    Top = 168
  end
  object OpenDialog: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 152
  end
end
