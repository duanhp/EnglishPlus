object ParamTypeForm: TParamTypeForm
  Left = 331
  Top = 149
  Width = 350
  Height = 318
  HorzScrollBar.Range = 311
  VertScrollBar.Range = 199
  BorderStyle = bsToolWindow
  Caption = 'Parameters information'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'Tahoma'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 10
    Top = 10
    Width = 322
    Height = 272
    BevelOuter = bvNone
    Color = clBlue
    TabOrder = 0
    object Panel5: TPanel
      Left = 167
      Top = 223
      Width = 154
      Height = 48
      BevelOuter = bvNone
      TabOrder = 0
      object btClose: TButton
        Left = 37
        Top = 11
        Width = 81
        Height = 27
        Caption = 'Close'
        TabOrder = 0
        OnClick = btCloseClick
      end
    end
    object Panel6: TPanel
      Left = 167
      Top = 1
      Width = 154
      Height = 141
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 10
        Top = 5
        Width = 46
        Height = 13
        Caption = 'Data type'
        Transparent = False
      end
      object rgTypes: TRadioGroup
        Left = 5
        Top = 20
        Width = 144
        Height = 113
        Items.Strings = (
          'String'
          'Integer'
          'Float'
          'Date')
        TabOrder = 0
        OnClick = rgTypesClick
      end
    end
    object Panel7: TPanel
      Left = 1
      Top = 1
      Width = 165
      Height = 270
      BevelOuter = bvNone
      TabOrder = 2
      object Label1: TLabel
        Left = 10
        Top = 5
        Width = 53
        Height = 13
        Caption = 'Parameters'
        Transparent = False
      end
      object lbParams: TListBox
        Left = 5
        Top = 20
        Width = 155
        Height = 239
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbParamsClick
      end
    end
    object Panel3: TPanel
      Left = 167
      Top = 170
      Width = 154
      Height = 52
      BevelOuter = bvNone
      TabOrder = 3
      object lbValue: TLabel
        Left = 10
        Top = 5
        Width = 27
        Height = 13
        Caption = 'Value'
        Transparent = False
      end
      object edParamValue: TEdit
        Left = 5
        Top = 20
        Width = 144
        Height = 21
        TabOrder = 0
        OnExit = edParamValueExit
      end
    end
    object Panel2: TPanel
      Left = 167
      Top = 143
      Width = 154
      Height = 26
      BevelOuter = bvNone
      TabOrder = 4
      object Label3: TLabel
        Left = 10
        Top = 5
        Width = 74
        Height = 13
        Caption = 'Parameter type:'
        Transparent = False
      end
      object lbParameterType: TLabel
        Left = 89
        Top = 5
        Width = 3
        Height = 13
        Transparent = False
      end
    end
  end
end
