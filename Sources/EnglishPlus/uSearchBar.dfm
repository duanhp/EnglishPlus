object FmSearchBar: TFmSearchBar
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'FmSearchBar'
  ClientHeight = 43
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    687
    43)
  PixelsPerInch = 96
  TextHeight = 13
  object EdSearchText: TEdit
    Left = 160
    Top = 10
    Width = 434
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnKeyUp = EdSearchTextKeyUp
  end
  object BtnGo: TButton
    Left = 604
    Top = 8
    Width = 75
    Height = 26
    Anchors = [akTop, akRight]
    Caption = 'GO'
    TabOrder = 1
    OnClick = BtnGoClick
  end
  object Button1: TButton
    Left = 4
    Top = 6
    Width = 73
    Height = 26
    Caption = 'BtnPre'
    TabOrder = 2
  end
  object Button2: TButton
    Left = 81
    Top = 6
    Width = 73
    Height = 26
    Caption = 'BtnNext'
    TabOrder = 3
  end
end
