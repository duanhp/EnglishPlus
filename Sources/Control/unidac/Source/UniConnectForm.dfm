object UniConnectForm: TUniConnectForm
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
      Width = 100
      Height = 13
      AutoSize = False
      Caption = 'Username'
      Layout = tlCenter
    end
    object lbPassword: TLabel
      Left = 9
      Top = 88
      Width = 100
      Height = 13
      AutoSize = False
      Caption = 'Password'
      Layout = tlCenter
    end
    object lbServer: TLabel
      Left = 9
      Top = 122
      Width = 100
      Height = 13
      AutoSize = False
      Caption = 'Server'
      Layout = tlCenter
    end
    object lbPort: TLabel
      Left = 9
      Top = 156
      Width = 100
      Height = 13
      AutoSize = False
      Caption = 'Port'
      Layout = tlCenter
    end
    object lbProvider: TLabel
      Left = 9
      Top = 20
      Width = 101
      Height = 13
      AutoSize = False
      Caption = 'Provider'
      Layout = tlCenter
    end
    object lbDatabase: TLabel
      Left = 9
      Top = 190
      Width = 99
      Height = 13
      AutoSize = False
      Caption = 'Database'
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
  object btConnect: TButton
    Left = 52
    Top = 242
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Connect'
    Default = True
    TabOrder = 1
    OnClick = btConnectClick
  end
  object btCancel: TButton
    Left = 148
    Top = 242
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
  end
end
