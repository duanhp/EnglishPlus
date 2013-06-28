inherited UniQueryEditorForm: TUniQueryEditorForm
  Left = 435
  Top = 196
  Width = 540
  Height = 384
  Caption = 'UniQuery Editor'
  Constraints.MinHeight = 384
  Constraints.MinWidth = 540
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    Top = 309
    Width = 532
    inherited imCorner: TImage
      Left = 520
    end
    inherited btOk: TBitBtn
      Left = 368
    end
    inherited btCancel: TBitBtn
      Left = 449
    end
    object btMacros: TButton
      Left = 180
      Top = 8
      Width = 117
      Height = 25
      Caption = 'Connection Macros...'
      TabOrder = 4
      OnClick = btMacrosClick
    end
  end
  inherited PageControl: TPageControl
    Width = 516
    Height = 301
    object shOptions: TTabSheet [5]
      Caption = '&Options'
      ImageIndex = 6
    end
  end
end
