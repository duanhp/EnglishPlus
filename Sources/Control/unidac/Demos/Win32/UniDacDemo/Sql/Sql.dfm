inherited SqlFrame: TSqlFrame
  Width = 443
  Height = 277
  Align = alClient
  object Splitter1: TSplitter
    Left = 0
    Top = 219
    Width = 443
    Height = 5
    Cursor = crVSplit
    Align = alTop
  end
  object meSQL: TMemo
    Left = 0
    Top = 51
    Width = 443
    Height = 168
    TabStop = False
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    OnExit = meSQLExit
  end
  object ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 51
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 345
      Height = 26
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btExecute: TSpeedButton
        Left = 1
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Execute'
        Flat = True
        Transparent = False
        OnClick = btExecuteClick
      end
      object btParType: TSpeedButton
        Left = 87
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Params Type ...'
        Flat = True
        Transparent = False
        OnClick = btParTypeClick
      end
      object btPrepare: TSpeedButton
        Left = 173
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Prepare'
        Flat = True
        Transparent = False
        OnClick = btPrepareClick
      end
      object btUnprepare: TSpeedButton
        Left = 259
        Top = 1
        Width = 85
        Height = 24
        Caption = 'Unprepare'
        Flat = True
        Transparent = False
        OnClick = btUnprepareClick
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 26
      Width = 259
      Height = 25
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 1
      object btCreateProcCall: TSpeedButton
        Left = 1
        Top = 0
        Width = 85
        Height = 24
        Caption = 'CreateProcCall'
        Flat = True
        Transparent = False
        OnClick = btCreateProcCallClick
      end
      object Panel2: TPanel
        Left = 87
        Top = 0
        Width = 171
        Height = 24
        BevelOuter = bvNone
        TabOrder = 0
        object edStoredProcNames: TComboBox
          Left = 8
          Top = 2
          Width = 155
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = edStoredProcNamesChange
          OnDropDown = edStoredProcNamesDropDown
        end
      end
    end
  end
  object meResult: TMemo
    Left = 0
    Top = 224
    Width = 443
    Height = 53
    TabStop = False
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object UniSQL: TUniSQL
    SQL.Strings = (
      'UPDATE emp SET sal = sal + 10 where sal < 2000')
    Left = 225
    Top = 57
  end
end
