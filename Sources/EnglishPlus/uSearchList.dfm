object FmSearchList: TFmSearchList
  Left = 0
  Top = 0
  AlphaBlend = True
  AlphaBlendValue = 150
  BorderStyle = bsNone
  Caption = 'FmSearchList'
  ClientHeight = 285
  ClientWidth = 420
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbWordList: TListBox
    Left = 0
    Top = 0
    Width = 420
    Height = 285
    Style = lbOwnerDrawVariable
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -20
    Font.Name = 'Calibri'
    Font.Style = [fsBold]
    ItemHeight = 30
    Items.Strings = (
      'Hello n '#21890#65292#20320#22909#65281
      'Hello'
      'hello'
      'hello')
    ParentFont = False
    TabOrder = 0
    OnDblClick = LbWordListDblClick
    ExplicitTop = -8
  end
end
