inherited UniQueryOptionsFrame: TUniQueryOptionsFrame
  Width = 517
  object Label5: TLabel [0]
    Left = 8
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Lock'
  end
  object lbUpdatingTable: TLabel [1]
    Left = 160
    Top = 8
    Width = 70
    Height = 13
    Caption = 'UpdatingTable'
  end
  object lbKeyFieldsLabel1: TLabel [2]
    Left = 321
    Top = 8
    Width = 45
    Height = 13
    Caption = 'KeyFields'
  end
  inherited pnOptions: TPanel
    Top = 56
    Width = 491
    Height = 161
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 3
  end
  object cbLock1: TComboBox
    Left = 8
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbLock1Change
    Items.Strings = (
      'None'
      'Pessimistic'
      'Optimistic')
  end
  object edUpdatingTable: TEdit
    Left = 160
    Top = 24
    Width = 153
    Height = 21
    TabOrder = 1
    OnExit = edUpdatingTableExit
  end
  object edKeyFields: TEdit
    Left = 321
    Top = 24
    Width = 174
    Height = 21
    TabOrder = 2
    OnExit = edKeyFieldsExit
  end
end
