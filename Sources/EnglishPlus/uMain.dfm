object FmMain: TFmMain
  Left = 183
  Top = 100
  Caption = 'EnglishPlus'
  ClientHeight = 364
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PlCaption: TPanel
    Left = 0
    Top = 0
    Width = 650
    Height = 34
    Align = alTop
    Caption = ' '
    TabOrder = 0
  end
  object PlContent: TPanel
    Left = 0
    Top = 34
    Width = 650
    Height = 330
    Align = alClient
    Caption = ' '
    TabOrder = 1
    object PLSearch: TPanel
      Left = 1
      Top = 1
      Width = 648
      Height = 41
      Align = alTop
      Caption = ' '
      TabOrder = 0
    end
    object WBWdContent: TWebBrowser
      Left = 1
      Top = 42
      Width = 648
      Height = 287
      Align = alClient
      TabOrder = 1
      ExplicitLeft = 2
      ControlData = {
        4C000000F9420000AA1D00000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
end
