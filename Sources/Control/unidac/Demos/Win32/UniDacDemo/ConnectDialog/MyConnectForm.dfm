object fmMyConnect: TfmMyConnect
  Left = 246
  Top = 163
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Connect'
  ClientHeight = 276
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    291
    276)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 8
    Top = 8
    Width = 273
    Height = 222
    Anchors = [akLeft, akTop, akBottom]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 0
    object lbUsername: TLabel
      Left = 9
      Top = 54
      Width = 58
      Height = 13
      Caption = 'Username'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Layout = tlCenter
    end
    object lbPassword: TLabel
      Left = 9
      Top = 88
      Width = 54
      Height = 13
      Caption = 'Password'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Layout = tlCenter
    end
    object lbServer: TLabel
      Left = 9
      Top = 122
      Width = 38
      Height = 13
      Caption = 'Server'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Layout = tlCenter
    end
    object lbPort: TLabel
      Left = 9
      Top = 156
      Width = 24
      Height = 13
      Caption = 'Port'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Layout = tlCenter
    end
    object lbProvider: TLabel
      Left = 9
      Top = 20
      Width = 48
      Height = 13
      Caption = 'Provider'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Layout = tlCenter
    end
    object lbDatabase: TLabel
      Left = 9
      Top = 190
      Width = 54
      Height = 13
      Caption = 'Database'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      Layout = tlCenter
    end
    object edUserName: TEdit
      Left = 111
      Top = 50
      Width = 153
      Height = 21
      AutoSelect = False
      MaxLength = 32767
      TabOrder = 1
    end
    object edPassword: TEdit
      Left = 111
      Top = 84
      Width = 153
      Height = 21
      AutoSelect = False
      MaxLength = 32767
      PasswordChar = '*'
      TabOrder = 2
    end
    object edServer: TComboBox
      Left = 111
      Top = 118
      Width = 153
      Height = 21
      DropDownCount = 10
      ItemHeight = 13
      TabOrder = 3
      OnDropDown = edServerDropDown
    end
    object edProvider: TComboBox
      Left = 111
      Top = 16
      Width = 153
      Height = 19
      Style = csOwnerDrawFixed
      DropDownCount = 10
      ItemHeight = 13
      TabOrder = 0
      OnChange = edProviderChange
    end
    object edDatabase: TComboBox
      Left = 111
      Top = 186
      Width = 153
      Height = 21
      DropDownCount = 10
      ItemHeight = 13
      TabOrder = 4
      OnDropDown = edDatabaseDropDown
    end
    object edPort: TEdit
      Left = 111
      Top = 152
      Width = 153
      Height = 21
      AutoSelect = False
      MaxLength = 32767
      TabOrder = 5
    end
  end
  object btConnect: TBitBtn
    Left = 49
    Top = 241
    Width = 89
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btConnectClick
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object btCancel: TBitBtn
    Left = 145
    Top = 241
    Width = 89
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
