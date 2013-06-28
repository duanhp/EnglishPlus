unit Loader;

interface

uses
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Grids, DBGrids, StdCtrls, ToolWin, ComCtrls, Buttons,
  UniDacVcl, Variants,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DB, UniLoader, UniScript, DBAccess, Uni, Fetch, DALoader, DemoFrame,
  UniDacDemoForm, DAScript, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF};

type
  TLoaderFrame = class(TDemoFrame)
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    ToolBar: TPanel;
    btOpen: TSpeedButton;
    DBNavigator: TDBNavigator;
    btClose: TSpeedButton;
    Query: TUniQuery;
    btLoad: TSpeedButton;
    UniLoader: TUniLoader;
    btDeleteAll: TSpeedButton;
    Panel1: TPanel;
    rgEvent: TRadioGroup;
    edRows: TEdit;
    Label1: TLabel;
    Panel2: TPanel;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure GetColumnData(Sender: TObject; Column: TDAColumn;
      Row: Integer; var Value: Variant; var EOF: Boolean);
    procedure btDeleteAllClick(Sender: TObject);
    procedure QueryAfterOpen(DataSet: TDataSet);
    procedure QueryBeforeClose(DataSet: TDataSet);
    procedure PutData(Sender: TDALoader);
    procedure rgEventClick(Sender: TObject);
    procedure QueryBeforeFetch(DataSet: TCustomDADataSet;
      var Cancel: Boolean);
    procedure QueryAfterFetch(DataSet: TCustomDADataSet);
  private
    { Private declarations }
  public
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;
    destructor Destroy; override;
  end;

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

destructor TLoaderFrame.Destroy;
begin
  inherited;
  FreeAndNil(FetchForm);
end;

procedure TLoaderFrame.btOpenClick(Sender: TObject);
begin
  Query.Open;
end;

procedure TLoaderFrame.btCloseClick(Sender: TObject);
begin
  Query.Close;
end;

procedure TLoaderFrame.btLoadClick(Sender: TObject);
{$IFNDEF LINUX}
var
  Start, Finish: Int64;
{$ENDIF}
begin
{$IFNDEF LINUX}
  Start := GetTickCount;
{$ENDIF}

  UniLoader.Load;  // loading rows

{$IFNDEF LINUX}
  Finish := GetTickCount;
  UniDacForm.StatusBar.Panels[2].Text := 'Time: ' + FloatToStr((Finish - Start) / 1000) + ' sec.';
{$ENDIF}

  if Query.Active then
    Query.Refresh;
end;

procedure TLoaderFrame.GetColumnData(Sender: TObject;
  Column: TDAColumn; Row: Integer; var Value: Variant; var EOF: Boolean);
begin
  case Column.Index of
    0: Value := Row;
    1: Value := Random*100;
    2: Value := 'abc01234567890123456789';
    3: Value := Date;
  else
    Value := Null;
  end;

  EOF := Row > StrToInt(edRows.Text);
end;

procedure TLoaderFrame.PutData(Sender: TDALoader);
var
  Count: integer;
  i: integer;
begin
  Count := StrToInt(edRows.Text);
  for i := 1 to Count do begin
    Sender.PutColumnData(0, i, i);
    Sender.PutColumnData('DBL', i, Random*100);
    //Sender.PutColumnData(1, i, Random*100);
    Sender.PutColumnData(2, i, 'abc01234567890123456789');
    Sender.PutColumnData(3, i, Date);
  end;
end;

procedure TLoaderFrame.btDeleteAllClick(Sender: TObject);
begin
  Query.Connection.ExecSQL('DELETE FROM UniDAC_Loaded', []);
  if Query.Active then
    Query.Refresh;
end;

procedure TLoaderFrame.QueryAfterOpen(DataSet: TDataSet);
begin
  UniDacForm.StatusBar.Panels[1].Text := 'RecordCount: ' + IntToStr(DataSet.RecordCount);
end;

procedure TLoaderFrame.QueryBeforeClose(DataSet: TDataSet);
begin
  UniDacForm.StatusBar.Panels[1].Text := '';
end;

procedure TLoaderFrame.Initialize;
begin
  inherited;
  Query.Connection := Connection as TUniConnection;
  rgEvent.ItemIndex := 1;
  if FetchForm = nil then
    FetchForm := TFetchForm.Create(UniDacForm);
end;

procedure TLoaderFrame.rgEventClick(Sender: TObject);
begin
  if rgEvent.ItemIndex = 0 then begin
    UniLoader.OnGetColumnData := GetColumnData;
    UniLoader.OnPutData := nil;
  end
  else begin
    UniLoader.OnGetColumnData := nil;
    UniLoader.OnPutData := PutData;
  end
end;

procedure TLoaderFrame.QueryBeforeFetch(DataSet: TCustomDADataSet;
  var Cancel: Boolean);
begin
  if DataSet.FetchingAll then begin
    FetchForm.Show;
    Application.ProcessMessages;
    Cancel := not FetchForm.Visible;

    if Cancel then
      UniDacForm.StatusBar.Panels[1].Text := 'RecordCount: ' + IntToStr(DataSet.RecordCount);
  end;
end;

procedure TLoaderFrame.QueryAfterFetch(DataSet: TCustomDADataSet);
begin
  if not DataSet.FetchingAll then begin
    FetchForm.Close;
    Application.ProcessMessages;

    UniDacForm.StatusBar.Panels[1].Text := 'RecordCount: ' + IntToStr(DataSet.RecordCount);
  end;
end;

procedure TLoaderFrame.SetDebug(Value: boolean);
begin
  Query.Debug := Value;
end;

initialization
{$IFDEF FPC}
{$I Loader.lrs}
{$ENDIF}

end.
