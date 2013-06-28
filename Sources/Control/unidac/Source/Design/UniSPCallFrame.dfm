inherited UniSPCallFrame: TUniSPCallFrame
  Width = 553
  Height = 328
  inherited Panel1: TPanel
    Width = 553
    Height = 182
    inherited meSQL: TMemo
      Width = 537
      Height = 160
    end
  end
  inherited pnlTop: TPanel
    Width = 553
    inherited btClear: TSpeedButton
      Left = 545
    end
    inherited gbStatementType: TGroupBox
      Width = 538
    end
  end
  inherited pnSQL: TPanel
    Width = 553
    inherited cbStoredProcName: TComboBox
      Width = 401
    end
    inherited btGenerate: TButton
      Left = 424
    end
  end
  inherited pnSQLSP: TPanel
    Width = 553
    object btCreateSQL: TSpeedButton [1]
      Left = 523
      Top = 15
      Width = 21
      Height = 24
      Hint = 'Create SQL'
      Anchors = [akTop, akRight]
      Flat = True
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
        333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
        0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
        07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
        07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
        0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
        33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
        B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
        3BB33773333773333773B333333B3333333B7333333733333337}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = btCreateSQLClick
    end
    inherited cbStoredProcNameSP: TComboBox
      Left = 120
      Width = 345
    end
    object cbAllProcs: TCheckBox
      Left = 480
      Top = 18
      Width = 33
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'All'
      TabOrder = 1
      OnClick = cbAllProcsClick
    end
  end
end
