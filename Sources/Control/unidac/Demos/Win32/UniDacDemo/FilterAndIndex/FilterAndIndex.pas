unit FilterAndIndex;

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Grids, DBGrids, StdCtrls, ToolWin, ComCtrls,
{$IFNDEF FPC}
  MemDS,
{$ELSE}
  MemDataSet,
{$ENDIF}
  UniDacVcl, DBAccess, Uni, DB, Buttons, DemoFrame, UniDacDemoForm;

type
  TFilterAndIndexFrame = class(TDemoFrame)
    Query: TUniQuery;
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    Panel4: TPanel;
    ToolBar: TPanel;
    btClose: TSpeedButton;
    btOpen: TSpeedButton;
    DBNavigator1: TDBNavigator;
    Panel3: TPanel;
    Query2: TUniQuery;
    LookupQuery: TUniQuery;
    Query2EMPNO: TIntegerField;
    Query2ENAME: TStringField;
    Query2JOB: TStringField;
    Query2MGR: TIntegerField;
    Query2HIREDATE: TDateTimeField;
    Query2SAL: TFloatField;
    Query2COMM: TFloatField;
    Query2DEPTNO: TIntegerField;
    LookupQueryDEPTNO: TIntegerField;
    LookupQueryDNAME: TStringField;
    LookupQueryLOC: TStringField;
    Query2CALCULATED: TIntegerField;
    Query2LOOKUP: TStringField;
    Label1: TLabel;
    lbIndexFieldNames: TLabel;
    Panel5: TPanel;
    Panel2: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    lbFields: TListBox;
    cbFilter: TCheckBox;
    cbCalcFields: TCheckBox;
    Panel1: TPanel;
    cbCacheCalcFields: TCheckBox;
    edFilter: TEdit;
    Panel6: TPanel;
    cbIndex: TCheckBox;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure Query2CalcFields(DataSet: TDataSet);
    procedure cbFilterClick(Sender: TObject);
    procedure cbCalcFieldsClick(Sender: TObject);
    procedure cbIndexClick(Sender: TObject);
    procedure lbFieldsClick(Sender: TObject);
    procedure cbCacheCalcFieldsClick(Sender: TObject);
    procedure edFilterExit(Sender: TObject);
  private
    FOldActive: boolean;
    FFilterFilterNames: TStringList;
    procedure BeginChange;
    procedure EndChange;
  public
    destructor Destroy; override;
    // Demo management
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;
  end;

var
  FilterAndIndexFrame: TFilterAndIndexFrame;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ENDIF}
{$IFDEF WIN32}
{$R *.dfm}
{$ENDIF}
{$IFDEF WIN64}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

procedure TFilterAndIndexFrame.btOpenClick(Sender: TObject);
var
  i: integer;
begin
  if DataSource.DataSet.Active then
    Exit;
  lbFields.Items.Clear;
  lbFieldsClick(nil);
  cbFilterClick(nil);
  DataSource.DataSet.Open;
  for i := 0 to DataSource.DataSet.FieldCount - 1 do
    lbFields.Items.Add(DataSource.DataSet.Fields[i].FieldName);
end;

procedure TFilterAndIndexFrame.btCloseClick(Sender: TObject);
begin
  DataSource.DataSet.Close;
end;

procedure TFilterAndIndexFrame.Query2CalcFields(DataSet: TDataSet);
begin
  Query2CALCULATED.AsInteger := Query2EMPNO.AsInteger * 2;
end;

procedure TFilterAndIndexFrame.BeginChange;
begin
  FOldActive := DataSource.DataSet.Active;
  if FOldActive then
    btCloseClick(nil);
end;

procedure TFilterAndIndexFrame.EndChange;
begin
  if FOldActive then
    btOpenClick(nil);
end;

procedure TFilterAndIndexFrame.cbFilterClick(Sender: TObject);
begin
  DataSource.DataSet.Filtered := cbFilter.Checked;
  DataSource.DataSet.Filter := edFilter.Text;
end;

procedure TFilterAndIndexFrame.cbCalcFieldsClick(Sender: TObject);
begin
  BeginChange;
  if cbCalcFields.Checked then
    DataSource.DataSet := Query2
  else
    DataSource.DataSet := Query;
  EndChange;
end;

procedure TFilterAndIndexFrame.cbIndexClick(Sender: TObject);
begin
  if cbIndex.Checked then
    TUniQuery(DataSource.DataSet).IndexFieldNames := lbIndexFieldNames.Caption
  else
    TUniQuery(DataSource.DataSet).IndexFieldNames := ''
end;

procedure TFilterAndIndexFrame.lbFieldsClick(Sender: TObject);
var
  i, k: integer;
  s: string;
begin
  i := 0;
  while i < FFilterFilterNames.Count do begin
    k := lbFields.Items.IndexOf(FFilterFilterNames[i]);
    if (k < 0) or not lbFields.Selected[k] then
      FFilterFilterNames.Delete(i)
    else
      Inc(i);
  end;
  for i := 0 to lbFields.Items.Count - 1 do
    if lbFields.Selected[i] and
      (FFilterFilterNames.IndexOf(lbFields.Items[i]) < 0) then
      FFilterFilterNames.Add(lbFields.Items[i]);

  s := '';
  for i := 0 to FFilterFilterNames.Count - 1 do begin
    if s <> '' then
      s := s + ', ';
    s := s + FFilterFilterNames[i];
  end;
  lbIndexFieldNames.Caption := s;

  cbIndexClick(nil);
end;

procedure TFilterAndIndexFrame.cbCacheCalcFieldsClick(Sender: TObject);
begin
  BeginChange;
  TUniQuery(DataSource.DataSet).Options.CacheCalcFields := cbCacheCalcFields.Checked;
  EndChange;
end;

procedure TFilterAndIndexFrame.edFilterExit(Sender: TObject);
begin
  cbFilterClick(nil);
end;

destructor TFilterAndIndexFrame.Destroy;
begin
  FFilterFilterNames.Free;
  inherited;
end;

// Demo management
procedure TFilterAndIndexFrame.Initialize;
begin
  inherited;
  Query.Connection := Connection as TUniConnection;
  Query2.Connection := Connection as TUniConnection;
  FFilterFilterNames := TStringList.Create;
end;

procedure TFilterAndIndexFrame.SetDebug(Value: boolean);
begin
  Query.Debug := Value;
  Query2.Debug := Value;
end;

{$IFDEF FPC}
initialization
  {$i FilterAndIndex.lrs}
{$ENDIF}

end.
