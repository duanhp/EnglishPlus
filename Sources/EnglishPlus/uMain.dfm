object Form1: TForm1
  Left = 183
  Top = 100
  Caption = 'Form1'
  ClientHeight = 364
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object PlCaption: TPanel
    Left = 0
    Top = 0
    Width = 650
    Height = 41
    Align = alTop
    Caption = ' '
    TabOrder = 0
  end
  object PlContent: TPanel
    Left = 0
    Top = 41
    Width = 650
    Height = 282
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
      DesignSize = (
        648
        41)
      object SpeedButton1: TSpeedButton
        Left = 14
        Top = 5
        Width = 52
        Height = 22
        Caption = '<-'
      end
      object SpeedButton2: TSpeedButton
        Left = 72
        Top = 5
        Width = 47
        Height = 22
        Caption = '->'
      end
      object SpeedButton3: TSpeedButton
        Left = 592
        Top = 5
        Width = 47
        Height = 22
        Anchors = [akTop, akRight]
        Caption = 'GO'
      end
      object Edit1: TEdit
        Left = 125
        Top = 5
        Width = 464
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'Edit1'
      end
    end
    object WBWdContent: TWebBrowser
      Left = 1
      Top = 42
      Width = 648
      Height = 239
      Align = alClient
      TabOrder = 1
      ExplicitLeft = 2
      ExplicitTop = 37
      ControlData = {
        4C000000F9420000B41800000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
  end
  object PLbtmBar: TPanel
    Left = 0
    Top = 323
    Width = 650
    Height = 41
    Align = alBottom
    Caption = ' '
    TabOrder = 2
  end
end
