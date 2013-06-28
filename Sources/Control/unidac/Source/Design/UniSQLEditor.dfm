inherited UniSQLEditorForm: TUniSQLEditorForm
  Caption = 'UniSQLEditorForm'
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnPanel: TPanel
    object btMacros: TButton
      Left = 94
      Top = 8
      Width = 117
      Height = 25
      Caption = 'Connection Macros...'
      TabOrder = 3
      OnClick = btMacrosClick
    end
  end
  inherited PageControl: TPageControl
    object shOptions: TTabSheet [3]
      Caption = '&Options'
      ImageIndex = 4
    end
  end
end
