object MainForm: TMainForm
  Left = 200
  Top = 130
  Width = 820
  Height = 480
  Caption = 'FailOver Demo'
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 820
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnRight: TPanel
    Left = 256
    Top = 49
    Width = 556
    Height = 378
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 331
      Width = 556
      Height = 2
      Cursor = crVSplit
      Align = alTop
      Beveled = True
      Color = clBtnFace
      ParentColor = False
      ResizeStyle = rsUpdate
    end
    object pnMiddle: TPanel
      Left = 0
      Top = 0
      Width = 556
      Height = 331
      Align = alTop
      BevelOuter = bvNone
      Constraints.MinHeight = 160
      ParentColor = True
      TabOrder = 0
      object Splitter: TSplitter
        Left = 0
        Top = 153
        Width = 556
        Height = 2
        Cursor = crVSplit
        Align = alTop
        Beveled = True
        Color = clBtnFace
        MinSize = 100
        ParentColor = False
        ResizeStyle = rsUpdate
      end
      object pnDetail: TPanel
        Left = 0
        Top = 155
        Width = 556
        Height = 176
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        object DBGrid2: TDBGrid
          Left = 0
          Top = 27
          Width = 556
          Height = 149
          Align = alClient
          DataSource = DM.dsDetail
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
        end
        object Panel9: TPanel
          Left = 0
          Top = 0
          Width = 556
          Height = 27
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Panel7: TPanel
            Left = 0
            Top = 2
            Width = 242
            Height = 24
            BevelOuter = bvNone
            Color = clBlue
            TabOrder = 0
            object DBNavigator2: TDBNavigator
              Left = 1
              Top = 1
              Width = 240
              Height = 22
              DataSource = DM.dsDetail
              Flat = True
              TabOrder = 0
            end
          end
        end
      end
      object pnMaster: TPanel
        Left = 0
        Top = 0
        Width = 556
        Height = 153
        Align = alTop
        BevelOuter = bvNone
        Constraints.MinHeight = 100
        ParentColor = True
        TabOrder = 1
        object DBGrid1: TDBGrid
          Left = 0
          Top = 1
          Width = 556
          Height = 152
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          DataSource = DM.dsMaster
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Tahoma'
          TitleFont.Style = []
        end
      end
    end
    object pnBottom: TPanel
      Left = 0
      Top = 333
      Width = 556
      Height = 45
      Align = alClient
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 1
      object meLog: TMemo
        Left = 0
        Top = 0
        Width = 556
        Height = 45
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object pnLeft: TPanel
    Left = 0
    Top = 49
    Width = 256
    Height = 378
    Align = alLeft
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object Panel8: TPanel
      Left = 1
      Top = 128
      Width = 243
      Height = 254
      BevelOuter = bvNone
      TabOrder = 0
      object cbPooling: TCheckBox
        Left = 8
        Top = 138
        Width = 150
        Height = 17
        Caption = 'Pooling'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 6
        OnClick = cbPoolingClick
      end
      object pnPooling: TPanel
        Left = 26
        Top = 157
        Width = 215
        Height = 95
        BevelOuter = bvNone
        Color = clBlue
        TabOrder = 7
        object Panel10: TPanel
          Left = 1
          Top = 1
          Width = 213
          Height = 93
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvNone
          TabOrder = 0
          object Label2: TLabel
            Left = 6
            Top = 54
            Width = 90
            Height = 13
            Caption = 'ConnectionLifetime'
          end
          object Label3: TLabel
            Left = 6
            Top = 8
            Width = 61
            Height = 13
            Caption = 'MaxPoolSize'
          end
          object Label4: TLabel
            Left = 6
            Top = 31
            Width = 58
            Height = 13
            Caption = 'MinPoolSize'
          end
          object cbValidate: TCheckBox
            Left = 6
            Top = 77
            Width = 111
            Height = 14
            Alignment = taLeftJustify
            Caption = 'Validate'
            TabOrder = 3
            OnClick = cbValidateClick
          end
          object edMaxPoolSize: TEdit
            Left = 104
            Top = 4
            Width = 103
            Height = 21
            TabOrder = 0
            OnExit = edMaxPoolSizeExit
          end
          object edMinPoolSize: TEdit
            Left = 104
            Top = 27
            Width = 103
            Height = 21
            TabOrder = 1
            OnExit = edMinPoolSizeExit
          end
          object edConnectionLifetime: TEdit
            Left = 104
            Top = 50
            Width = 103
            Height = 21
            TabOrder = 2
            OnExit = edConnectionLifetimeExit
          end
        end
      end
      object cbCachedUpdates: TCheckBox
        Left = 8
        Top = 7
        Width = 150
        Height = 17
        Caption = 'CachedUpdates'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 0
        OnClick = cbCachedUpdatesClick
      end
      object cbFailover: TCheckBox
        Left = 8
        Top = 88
        Width = 73
        Height = 17
        Caption = 'Failover'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 4
        OnClick = cbFailoverClick
      end
      object cbLocalMasterDetail: TCheckBox
        Left = 8
        Top = 27
        Width = 150
        Height = 17
        Caption = 'LocalMasterDetail'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 1
        OnClick = cbLocalMasterDetailClick
      end
      object cbDisconnectedMode: TCheckBox
        Left = 8
        Top = 68
        Width = 150
        Height = 17
        Caption = 'DisconnectedMode'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 3
        OnClick = cbDisconnectedModeClick
      end
      object cbFetchAll: TCheckBox
        Left = 8
        Top = 48
        Width = 97
        Height = 17
        Caption = 'FetchAll'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 2
        OnClick = cbFetchAllClick
      end
      object pnFailover: TPanel
        Left = 26
        Top = 106
        Width = 215
        Height = 28
        BevelOuter = bvNone
        Color = clBlue
        TabOrder = 5
        object Panel16: TPanel
          Left = 1
          Top = 1
          Width = 213
          Height = 26
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvNone
          TabOrder = 0
          object Label7: TLabel
            Left = 5
            Top = 7
            Width = 52
            Height = 13
            Caption = 'RetryMode'
          end
          object coRetryMode: TComboBox
            Left = 69
            Top = 3
            Width = 141
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 0
            Text = 'Default'
            Items.Strings = (
              'Default'
              'Raise'
              'Reconnect'
              'ReconnectExecute')
          end
        end
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 256
      Height = 122
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Panel1: TPanel
        Left = 2
        Top = 1
        Width = 253
        Height = 122
        BevelOuter = bvNone
        Color = clBlue
        TabOrder = 0
        object Panel15: TPanel
          Left = 1
          Top = 1
          Width = 251
          Height = 60
          BevelOuter = bvNone
          TabOrder = 0
          object Label1: TLabel
            Left = 11
            Top = 6
            Width = 48
            Height = 13
            Alignment = taRightJustify
            Caption = 'Updates'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Panel12: TPanel
            Left = 12
            Top = 26
            Width = 229
            Height = 24
            BevelOuter = bvNone
            Color = clBlue
            TabOrder = 0
            object btApply: TSpeedButton
              Left = 1
              Top = 1
              Width = 75
              Height = 22
              Caption = 'Apply'
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clMaroon
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = False
              OnClick = btApplyClick
            end
            object btCancel: TSpeedButton
              Left = 153
              Top = 1
              Width = 75
              Height = 22
              Caption = 'Cancel'
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clMaroon
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = False
              OnClick = btCancelClick
            end
            object btCommit: TSpeedButton
              Left = 77
              Top = 1
              Width = 75
              Height = 22
              Caption = 'Commit'
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clMaroon
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = False
              OnClick = btCommitClick
            end
          end
        end
        object Panel17: TPanel
          Left = 1
          Top = 62
          Width = 251
          Height = 58
          BevelOuter = bvNone
          TabOrder = 1
          object Label6: TLabel
            Left = 11
            Top = 6
            Width = 74
            Height = 13
            Alignment = taRightJustify
            Caption = 'Transactions'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Panel5: TPanel
            Left = 12
            Top = 26
            Width = 229
            Height = 24
            BevelOuter = bvNone
            Color = clBlue
            TabOrder = 0
            object btStartTrans: TSpeedButton
              Left = 1
              Top = 1
              Width = 75
              Height = 22
              Caption = 'Start'
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clNavy
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = False
              OnClick = btStartTransClick
            end
            object btCommitTrans: TSpeedButton
              Left = 77
              Top = 1
              Width = 75
              Height = 22
              Caption = 'Commit'
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clNavy
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = False
              OnClick = btCommitTransClick
            end
            object btRollbackTrans: TSpeedButton
              Left = 153
              Top = 1
              Width = 75
              Height = 22
              Caption = 'Rollback'
              Flat = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clNavy
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = False
              OnClick = btRollbackTransClick
            end
          end
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 427
    Width = 812
    Height = 19
    Panels = <
      item
        Width = 208
      end
      item
        Width = 208
      end
      item
        Width = 208
      end
      item
        Width = 50
      end>
  end
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 812
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 3
    OnMouseMove = pnTopMouseMove
    object lbAbout: TLabel
      Left = 762
      Top = 6
      Width = 34
      Height = 13
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Caption = 'About'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      OnClick = lbAboutClick
      OnMouseMove = lbAboutMouseMove
    end
    object Panel18: TPanel
      Left = 2
      Top = 1
      Width = 418
      Height = 47
      BevelOuter = bvNone
      Color = clBlue
      TabOrder = 0
      object btConnect: TSpeedButton
        Left = 167
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Connect'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        NumGlyphs = 2
        ParentFont = False
        Transparent = False
        OnClick = btConnectClick
      end
      object btDisconnect: TSpeedButton
        Left = 250
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Disconnect'
        Enabled = False
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        NumGlyphs = 2
        ParentFont = False
        Transparent = False
        OnClick = btDisconnectClick
      end
      object btClose: TSpeedButton
        Left = 84
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Close'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
        OnClick = btCloseClick
      end
      object btOpen: TSpeedButton
        Left = 1
        Top = 1
        Width = 82
        Height = 22
        Caption = 'Open'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        Transparent = False
        OnClick = btOpenClick
      end
      object btKillSession: TSpeedButton
        Left = 333
        Top = 1
        Width = 84
        Height = 22
        Caption = 'Kill session'
        Enabled = False
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = False
        OnClick = btKillSessionClick
      end
      object btCreate: TSpeedButton
        Left = 1
        Top = 24
        Width = 82
        Height = 22
        Caption = 'Create'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = False
        OnClick = btCreateDropClick
      end
      object btDrop: TSpeedButton
        Left = 84
        Top = 24
        Width = 82
        Height = 22
        Caption = 'Drop'
        Flat = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Transparent = False
        OnClick = btCreateDropClick
      end
      object DBNavigator1: TDBNavigator
        Left = 167
        Top = 24
        Width = 250
        Height = 22
        DataSource = DM.dsMaster
        Flat = True
        TabOrder = 0
      end
    end
    object cbDebug: TCheckBox
      Left = 685
      Top = 5
      Width = 64
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Debug'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = cbDebugClick
    end
  end
end
