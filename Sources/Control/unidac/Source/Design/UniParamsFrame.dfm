inherited UniParamsFrame: TUniParamsFrame
  inherited PanelItem: TPanel
    object lbNational: TLabel [5]
      Left = 34
      Top = 148
      Width = 39
      Height = 13
      Caption = 'National'
      FocusControl = cbNational
    end
    inherited bEdValue: TButton
      Top = 79
    end
    object cbNational: TCheckBox
      Left = 16
      Top = 147
      Width = 13
      Height = 17
      TabOrder = 6
      OnClick = cbNullValueClick
    end
  end
end
