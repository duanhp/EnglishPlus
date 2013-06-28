{$I DacDemo.inc}

unit VTable;

interface

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFNDEF KYLIX}
  Graphics, Controls, Forms, Dialogs, DBCtrls, ExtCtrls, Grids, DBGrids,
  StdCtrls, ToolWin, ComCtrls, Buttons,
  {$IFNDEF VER130}Variants,{$ENDIF}
{$ELSE}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  QDBCtrls, QComCtrls, QExtCtrls, QGrids, QDBGrids, QButtons,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DB, {$IFDEF FPC}MemDataSet, DemoFrame, MemDS, VirtualTable{$ELSE}MemDS{$ENDIF}, VirtualTable, DemoFrame, DemoForm;

type
  TVirtualTableFrame = class(TDemoFrame)
    DBGrid: TDBGrid;
    DBMemo: TDBMemo;
    Panel1: TPanel;
    Splitter: TSplitter;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    VirtualTable: TVirtualTable;
    DataSource: TDataSource;
    Panel2: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    btAutoFill: TSpeedButton;
    btAddField: TSpeedButton;
    btDelField: TSpeedButton;
    btClear: TSpeedButton;
    btSave: TSpeedButton;
    btLoad: TSpeedButton;
    Panel4: TPanel;
    edField: TEdit;
    Label1: TLabel;
    Panel5: TPanel;
    cbFiltered: TCheckBox;
    edFilter: TEdit;
    Label5: TLabel;
    Panel6: TPanel;
    edValue: TEdit;
    Label2: TLabel;
    Panel7: TPanel;
    btLocate: TSpeedButton;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btAutoFillClick(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure DataSourceStateChange(Sender: TObject);
    procedure btAddFieldClick(Sender: TObject);
    procedure btDelFieldClick(Sender: TObject);
    procedure btLocateClick(Sender: TObject);
    procedure edFilterExit(Sender: TObject);
    procedure cbFilteredClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    { Private declarations }
    FFieldNo: integer;
  public
    procedure Initialize; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ENDIF}
{$IFDEF WIN32_64}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}
{$ENDIF}

procedure TVirtualTableFrame.btOpenClick(Sender: TObject);
begin
  VirtualTable.Open;
  if VirtualTable.Fields.Count > 0 then
    edField.Text := VirtualTable.Fields[0].FieldName
  else 
    edField.Text := '';
end;

procedure TVirtualTableFrame.btCloseClick(Sender: TObject);
begin
  VirtualTable.Close;
end;

procedure TVirtualTableFrame.btAutoFillClick(Sender: TObject);
var
  i:integer;
  Field:TField;
begin
  Field:= VirtualTable.FindField('STRING1');
  for i:= 1 to 100 do
    with VirtualTable do begin
      Append;
      FieldByName('NUMBER').AsInteger:= i;
      FieldByName('STRING').AsString:= 'Use Devart Data Access Components !!! (' + IntToStr(i) + ')';
      FieldByName('DATE').AsDateTime:= Date;
      FieldByName('MEMO').AsString:= 'Memo value (' + IntToStr(i) + ')';

      if Field <> nil then
        FieldByName('STRING1').AsString:= 'TVirtualTable';

      Post;
    end;
end;

procedure TVirtualTableFrame.DataSourceStateChange(Sender: TObject);
begin
  (Application.MainForm as TDemoForm).StatusBar.Panels[1].Text:= 'RecordCount:' + IntToStr(VirtualTable.RecordCount);
  (Application.MainForm as TDemoForm).StatusBar.Panels[2].Text:= 'RecordNo:' + IntToStr(VirtualTable.RecNo);
end;

procedure TVirtualTableFrame.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  DataSourceStateChange(nil);
end;

procedure TVirtualTableFrame.btAddFieldClick(Sender: TObject);
begin
  Inc(FFieldNo);
  VirtualTable.AddField('STRING' + IntToStr(FFieldNo), ftString, 30);
  btDelField.Enabled := True;
end;

procedure TVirtualTableFrame.btDelFieldClick(Sender: TObject);
begin
  if FFieldNo > 0 then begin
    VirtualTable.DeleteField('STRING' + IntToStr(FFieldNo));
    Dec(FFieldNo);
  end;
  if FFieldNo = 0 then
    btDelField.Enabled := False;
end;

procedure TVirtualTableFrame.btLocateClick(Sender: TObject);
begin
  VirtualTable.Locate(edField.Text, Variant(edValue.Text), []);
end;

procedure TVirtualTableFrame.Initialize;
begin
  inherited;
  edFilter.Text:= VirtualTable.Filter;
  cbFiltered.Checked:= VirtualTable.Filtered;
  btDelField.Enabled := False;
  FFieldNo := 0;
  if (Application.MainForm as TDemoForm).GetIsXPMan then begin
    Panel2.Color := clBtnFace;
    Panel7.Color := clBtnFace;
  end
  else begin
    Panel2.Color := (Application.MainForm as TDemoForm).ProductColor;
    Panel7.Color := Panel2.Color;
  end
end;

procedure TVirtualTableFrame.edFilterExit(Sender: TObject);
begin
  VirtualTable.Filter:= edFilter.Text;
end;

procedure TVirtualTableFrame.cbFilteredClick(Sender: TObject);
begin
  try
    VirtualTable.Filtered := cbFiltered.Checked;
  finally
    cbFiltered.Checked := VirtualTable.Filtered;
  end;
end;

procedure TVirtualTableFrame.btLoadClick(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'VirtualTable';
  if OpenDialog.Execute then
    VirtualTable.LoadFromFile(OpenDialog.FileName);
end;

procedure TVirtualTableFrame.btSaveClick(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'VirtualTable';
  if SaveDialog.Execute then
    if LowerCase(ExtractFileExt(SaveDialog.FileName)) = '.xml' then
      VirtualTable.SaveToXML(SaveDialog.FileName)
    else
      VirtualTable.SaveToFile(SaveDialog.FileName)
end;

procedure TVirtualTableFrame.btClearClick(Sender: TObject);
begin
  VirtualTable.Clear;
end;

{$IFDEF FPC}
initialization
  {$i VTable.lrs}
{$ENDIF}

end.
