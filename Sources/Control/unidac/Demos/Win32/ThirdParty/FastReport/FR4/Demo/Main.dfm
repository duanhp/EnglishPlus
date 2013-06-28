object Form1: TForm1
  Left = 325
  Top = 211
  Width = 276
  Height = 160
  Caption = 'Fast Report demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 76
    Top = 52
    Width = 121
    Height = 25
    Caption = 'Design report'
    TabOrder = 0
    OnClick = Button1Click
  end
  object BitBtn1: TBitBtn
    Left = 76
    Top = 80
    Width = 121
    Height = 25
    Caption = 'Show report'
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object frxReport1: TfrxReport
    Version = '4.1.3'
    DotMatrixReport = False
    EngineOptions.MaxMemSize = 10000000
    IniFile = '\Software\Fast Reports'
    PreviewOptions.Buttons = [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Default'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 38261.496498298600000000
    ReportOptions.LastChange = 38261.698964479200000000
    ScriptLanguage = 'PascalScript'
    ScriptText.Strings = (
      'begin'
      ''
      'end.')
    Left = 24
    Top = 16
    Datasets = <
      item
        DataSet = frxReport1.UniDACTable1
        DataSetName = 'UniDACTable1'
      end
      item
        DataSet = frxReport1.UniDACQuery1
        DataSetName = 'UniDACQuery1'
      end>
    Variables = <>
    Style = <>
    object Page1: TfrxReportPage
      PaperWidth = 210.000000000000000000
      PaperHeight = 297.000000000000000000
      PaperSize = 9
      LeftMargin = 10.000000000000000000
      RightMargin = 10.000000000000000000
      TopMargin = 10.000000000000000000
      BottomMargin = 10.000000000000000000
      object ReportTitle1: TfrxReportTitle
        Height = 54.015770000000000000
        Top = 18.897650000000000000
        Width = 718.110700000000000000
        object Memo6: TfrxMemoView
          Left = 3.779530000000000000
          Top = 8.818903330000000000
          Width = 714.331170000000000000
          Height = 40.314986670000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -27
          Font.Name = 'Arial'
          Font.Style = [fsBold, fsItalic]
          HAlign = haCenter
          Memo.UTF8 = (
            'Employees Report')
          ParentFont = False
        end
      end
      object MasterData1: TfrxMasterData
        Height = 31.338590000000000000
        Top = 177.637910000000000000
        Width = 718.110700000000000000
        DataSet = frxReport1.UniDACTable1
        DataSetName = 'UniDACTable1'
        RowCount = 0
        object Memo7: TfrxMemoView
          Left = 10.078746670000000000
          Top = 7.559059999999990000
          Width = 99.527623330000000000
          Height = 21.417336670000000000
          Memo.UTF8 = (
            'Departament')
        end
        object Memo8: TfrxMemoView
          Left = 249.448980000000000000
          Top = 7.559059999999990000
          Width = 31.496083330000000000
          Height = 21.417336670000000000
          Memo.UTF8 = (
            'No')
        end
        object Memo9: TfrxMemoView
          Left = 415.748300000000000000
          Top = 7.559059999999990000
          Width = 54.173263330000000000
          Height = 21.417336670000000000
          Memo.UTF8 = (
            'Location')
        end
        object Memo10: TfrxMemoView
          Left = 291.023810000000000000
          Top = 7.559059999999990000
          Width = 79.370130000000000000
          Height = 18.897650000000000000
          DataField = 'DEPTNO'
          DataSet = frxReport1.UniDACTable1
          DataSetName = 'UniDACTable1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsItalic]
          Memo.UTF8 = (
            '[UniDACTable1."DEPTNO"]')
          ParentFont = False
        end
        object Memo11: TfrxMemoView
          Left = 117.165430000000000000
          Top = 7.559059999999990000
          Width = 113.385900000000000000
          Height = 18.897650000000000000
          DataField = 'DNAME'
          DataSet = frxReport1.UniDACTable1
          DataSetName = 'UniDACTable1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsItalic]
          Memo.UTF8 = (
            '[UniDACTable1."DNAME"]')
          ParentFont = False
        end
        object Memo12: TfrxMemoView
          Left = 483.779840000000000000
          Top = 7.559059999999990000
          Width = 105.826840000000000000
          Height = 18.897650000000000000
          DataField = 'LOC'
          DataSet = frxReport1.UniDACTable1
          DataSetName = 'UniDACTable1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsItalic]
          Memo.UTF8 = (
            '[UniDACTable1."LOC"]')
          ParentFont = False
        end
      end
      object PageFooter1: TfrxPageFooter
        Height = 38.897650000000000000
        Top = 498.897960000000000000
        Width = 718.110700000000000000
        object Memo1: TfrxMemoView
          Left = 642.520100000000000000
          Top = 18.897650000000100000
          Width = 75.590600000000000000
          Height = 18.897650000000000000
          HAlign = haRight
          Memo.UTF8 = (
            '[Page#]')
        end
      end
      object DetailData1: TfrxDetailData
        Height = 26.456710000000000000
        Top = 291.023810000000000000
        Width = 718.110700000000000000
        DataSet = frxReport1.UniDACQuery1
        DataSetName = 'UniDACQuery1'
        RowCount = 0
        object Memo5: TfrxMemoView
          Left = 98.267780000000000000
          Top = 3.779530000000020000
          Width = 109.606370000000000000
          Height = 18.897650000000000000
          DataField = 'ENAME'
          DataSet = frxReport1.UniDACQuery1
          DataSetName = 'UniDACQuery1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Memo.UTF8 = (
            '[UniDACQuery1."ENAME"]')
          ParentFont = False
        end
        object Memo17: TfrxMemoView
          Left = 230.551330000000000000
          Top = 3.779530000000020000
          Width = 98.267780000000000000
          Height = 18.897650000000000000
          DataField = 'JOB'
          DataSet = frxReport1.UniDACQuery1
          DataSetName = 'UniDACQuery1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Memo.UTF8 = (
            '[UniDACQuery1."JOB"]')
          ParentFont = False
        end
        object Memo18: TfrxMemoView
          Left = 340.157700000000000000
          Top = 3.779530000000020000
          Width = 98.267780000000000000
          Height = 18.897650000000000000
          DataField = 'MGR'
          DataSet = frxReport1.UniDACQuery1
          DataSetName = 'UniDACQuery1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Memo.UTF8 = (
            '[UniDACQuery1."MGR"]')
          ParentFont = False
        end
        object Memo19: TfrxMemoView
          Left = 453.543600000000000000
          Top = 3.779530000000020000
          Width = 102.047310000000000000
          Height = 18.897650000000000000
          DataField = 'HIREDATE'
          DataSet = frxReport1.UniDACQuery1
          DataSetName = 'UniDACQuery1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Memo.UTF8 = (
            '[UniDACQuery1."HIREDATE"]')
          ParentFont = False
        end
        object Memo20: TfrxMemoView
          Left = 566.929500000000000000
          Top = 3.779530000000020000
          Width = 79.370130000000000000
          Height = 18.897650000000000000
          DataField = 'SAL'
          DataSet = frxReport1.UniDACQuery1
          DataSetName = 'UniDACQuery1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Memo.UTF8 = (
            '[UniDACQuery1."SAL"]')
          ParentFont = False
        end
        object Memo4: TfrxMemoView
          Left = 11.338590000000000000
          Top = 3.779530000000020000
          Width = 79.370130000000000000
          Height = 18.897650000000000000
          DataField = 'EMPNO'
          DataSet = frxReport1.UniDACQuery1
          DataSetName = 'UniDACQuery1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          Memo.UTF8 = (
            '[UniDACQuery1."EMPNO"]')
          ParentFont = False
        end
      end
      object Footer1: TfrxFooter
        Height = 52.913420000000000000
        Top = 340.157700000000000000
        Width = 718.110700000000000000
        object Line2: TfrxLineView
          Top = 11.338590000000000000
          Width = 714.331170000000000000
          Frame.Typ = [ftTop]
          Frame.Width = 2.000000000000000000
        end
        object Memo21: TfrxMemoView
          Left = 11.338590000000000000
          Top = 18.897650000000000000
          Width = 76.850443330000000000
          Height = 21.417336670000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            'Summary')
          ParentFont = False
        end
        object Memo22: TfrxMemoView
          Left = 117.165430000000000000
          Top = 18.897650000000000000
          Width = 113.385900000000000000
          Height = 18.897650000000000000
          DataField = 'DNAME'
          DataSet = frxReport1.UniDACTable1
          DataSetName = 'UniDACTable1'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsItalic]
          Memo.UTF8 = (
            '[UniDACTable1."DNAME"]')
          ParentFont = False
        end
        object Memo23: TfrxMemoView
          Left = 566.929500000000000000
          Top = 18.897650000000000000
          Width = 94.488250000000000000
          Height = 18.897650000000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            '[SUM(<UniDACQuery1."SAL">,DetailData1)]')
          ParentFont = False
        end
      end
      object Header1: TfrxHeader
        Height = 37.795300000000000000
        Top = 230.551330000000000000
        Width = 718.110700000000000000
        object Line1: TfrxLineView
          Width = 718.110700000000000000
          Frame.Typ = [ftTop]
          Frame.Width = 2.000000000000000000
        end
        object Memo2: TfrxMemoView
          Left = 11.338590000000000000
          Top = 7.559059999999990000
          Width = 76.850443330000000000
          Height = 21.417336670000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            'No')
          ParentFont = False
        end
        object Memo3: TfrxMemoView
          Left = 98.267780000000000000
          Top = 7.559059999999990000
          Width = 114.645743330000000000
          Height = 21.417336670000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            'Name')
          ParentFont = False
        end
        object Memo13: TfrxMemoView
          Left = 230.551330000000000000
          Top = 7.559059999999990000
          Width = 99.527623330000000000
          Height = 21.417336670000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            'Job')
          ParentFont = False
        end
        object Memo14: TfrxMemoView
          Left = 340.157700000000000000
          Top = 7.559059999999990000
          Width = 99.527623330000000000
          Height = 21.417336670000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            'Manager')
          ParentFont = False
        end
        object Memo15: TfrxMemoView
          Left = 453.543600000000000000
          Top = 7.559059999999990000
          Width = 99.527623330000000000
          Height = 21.417336670000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            'Hire Date')
          ParentFont = False
        end
        object Memo16: TfrxMemoView
          Left = 566.929500000000000000
          Top = 7.559059999999990000
          Width = 99.527623330000000000
          Height = 21.417336670000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            'Salary')
          ParentFont = False
        end
      end
      object GroupHeader1: TfrxGroupHeader
        Height = 22.677180000000000000
        Top = 132.283550000000000000
        Width = 718.110700000000000000
        Condition = 'True'
      end
      object GroupFooter1: TfrxGroupFooter
        Height = 22.677180000000000000
        Top = 415.748300000000000000
        Width = 718.110700000000000000
        object Memo25: TfrxMemoView
          Left = 566.929500000000000000
          Width = 94.488250000000000000
          Height = 18.897650000000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            '[SUM(<UniDACQuery1."SAL">,DetailData1)]')
          ParentFont = False
        end
        object Memo24: TfrxMemoView
          Left = 11.338590000000000000
          Top = 3.779530000000020000
          Width = 94.488250000000000000
          Height = 18.897650000000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Memo.UTF8 = (
            'Total')
          ParentFont = False
        end
      end
    end
    object DialogPage1: TfrxDialogPage
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Height = 200.000000000000000000
      Left = 265.000000000000000000
      Top = 150.000000000000000000
      Width = 300.000000000000000000
      object UniDACDatabase1: TfrxUniDACDatabase
        LoginPrompt = False
        DatabaseName = 'SCOTT'
        Username = 'scott'
        Password = 'tiger'
        ProviderName = 'Oracle'
        Server = 'ora'
        Connected = False
        pLeft = 0
        pTop = 0
      end
      object UniDACTable1: TfrxUniDACTable
        UserName = 'UniDACTable1'
        CloseDataSource = True
        TableName = 'dept'
        Database = frxReport1.UniDACDatabase1
        pLeft = 0
        pTop = 0
      end
      object UniDACQuery1: TfrxUniDACQuery
        UserName = 'UniDACQuery1'
        CloseDataSource = True
        Master = frxReport1.UniDACTable1
        Params = <>
        SQL.Strings = (
          'select * from emp')
        Database = frxReport1.UniDACDatabase1
        MasterFields = 'DEPTNO=DEPTNO'
        pLeft = 0
        pTop = 0
        Parameters = <>
      end
    end
  end
  object frxDesigner1: TfrxDesigner
    DefaultScriptLanguage = 'PascalScript'
    DefaultFont.Charset = DEFAULT_CHARSET
    DefaultFont.Color = clWindowText
    DefaultFont.Height = -13
    DefaultFont.Name = 'Arial'
    DefaultFont.Style = []
    DefaultLeftMargin = 10.000000000000000000
    DefaultRightMargin = 10.000000000000000000
    DefaultTopMargin = 10.000000000000000000
    DefaultBottomMargin = 10.000000000000000000
    DefaultPaperSize = 9
    DefaultOrientation = poPortrait
    Restrictions = []
    RTLLanguage = False
    Left = 56
    Top = 16
  end
  object frxUniDACComponents1: TfrxUniDACComponents
    Left = 104
    Top = 16
  end
end
