object AboutForm: TAboutForm
  Left = 352
  Top = 192
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About FailOver Demo'
  ClientHeight = 343
  ClientWidth = 416
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnMouseMove = FormMouseMove
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 16
    Top = 36
    Width = 156
    Height = 13
    Caption = 'Copyright '#169' 1997-2011 Devart'
  end
  object lbWeb: TLabel
    Left = 16
    Top = 306
    Width = 73
    Height = 13
    Cursor = crHandPoint
    Caption = 'www.devart.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = 13
    Font.Name = 'Tahoma'
    Font.Pitch = fpVariable
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lbWebClick
    OnMouseMove = lbWebMouseMove
  end
  object lbDemo: TLabel
    Left = 16
    Top = 8
    Width = 119
    Height = 20
    Caption = 'FailOver Demo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object OKBtn: TButton
    Left = 319
    Top = 308
    Width = 75
    Height = 24
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object meAbout: TMemo
    Left = 16
    Top = 56
    Width = 385
    Height = 241
    Color = clBtnFace
    Lines.Strings = (
      'This demo program shows how to use DisconnectedMode, Pooling, '
      
        'CachedUpdates and LocalMasterDetail options. Usage of these opti' +
        'ons and '
      
        'usage of the OnConectionLost event are recommended working in un' +
        'stable '
      'and/or slow network.'
      ''
      
        'The '#39'Open'#39' button opens two data sets. They are linked by Master' +
        '/Detail '
      
        'relationship. You can preconnect to database before opening the ' +
        'datasets, but '
      'usage of disconnected mode has no advantages in this case.'
      ''
      
        'The grid above displays the master data set, the grid below disp' +
        'lays the detail '
      
        'data set. The '#39'Kill session'#39' button terminates session on the se' +
        'rver, which '
      
        'causes connection break. This does not work for with all SQL ser' +
        'vers and with '
      
        'all users. The memo at the bottom of the main form keeps track o' +
        'f connection '
      'messages.'
      ''
      
        'See the "Working in unstable and slow networks" topic in help fo' +
        'r more '
      'detailed information.')
    TabOrder = 1
  end
end
