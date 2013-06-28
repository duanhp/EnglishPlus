object UniSpecificOptionsFrame: TUniSpecificOptionsFrame
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 80
      Height = 13
      Caption = 'Options for provider'
    end
    object edProvider: TComboBox
      Left = 0
      Top = 16
      Width = 153
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = edProviderChange
    end
  end
end
