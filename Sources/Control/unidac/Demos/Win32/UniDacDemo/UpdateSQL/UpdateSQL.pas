unit UpdateSQL;

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  Classes, SysUtils, Menus, ImgList, StdCtrls, ComCtrls, Buttons, ExtCtrls, Graphics,
  Controls, Forms, Dialogs, DBCtrls, Grids, DBGrids, UniDacVcl, Variants,
{$IFNDEF FPC}
  MemDS,
{$ELSE}
  MemDataSet,
{$ENDIF}
  DB, DBAccess, Uni, DemoFrame;

type
  TUpdateSQLFrame = class(TDemoFrame)
    UniQuery: TUniQuery;
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    ToolBar: TPanel;
    meSQL: TMemo;
    UniUpdateSQL: TUniUpdateSQL;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    btPrepare: TSpeedButton;
    btUnPrepare: TSpeedButton;
    btExecute: TSpeedButton;
    Panel3: TPanel;
    Panel1: TPanel;
    btRefreshRecord: TSpeedButton;
    DBNavigator1: TDBNavigator;
    Splitter1: TSplitter;
    Panel6: TPanel;
    cbDeleteObject: TCheckBox;
    cbInsertObject: TCheckBox;
    cbModifyObject: TCheckBox;
    cbRefreshObject: TCheckBox;
    Panel4: TPanel;
    RefreshQuery: TUniQuery;
    ModifyQuery: TUniQuery;
    DeleteQuery: TUniQuery;
    InsertQuery: TUniQuery;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btPrepareClick(Sender: TObject);
    procedure btUnPrepareClick(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
    procedure meSQLExit(Sender: TObject);
    procedure btRefreshRecordClick(Sender: TObject);
    procedure cbObjectClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowState;
  public
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;
  end;

implementation

uses UniDacDemoForm;

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
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}
{$ENDIF}

procedure TUpdateSQLFrame.ShowState;
var
  St:string;

  procedure AddSt(S:string);
  begin
    if St <> '' then
      St:= St + ', ';
    St:= St + S;
  end;

begin
  St:= '';

  if UniQuery.Prepared then begin
    AddSt('Prepared');

    if UniQuery.IsQuery then
      AddSt('IsQuery');
  end;

  if UniQuery.Active then
    AddSt('Active')
  else
    AddSt('Inactive');

  if UniQuery.Executing then
    AddSt('Executing');

  if UniQuery.Fetching then
    AddSt('Fetching');

  UniDacForm.StatusBar.Panels[2].Text:= St;
end;

procedure TUpdateSQLFrame.meSQLExit(Sender: TObject);
begin
  if Trim(UniQuery.SQL.Text) <> Trim(meSQL.Lines.Text) then
    UniQuery.SQL.Text:= meSQL.Lines.Text;
  ShowState;
end;

procedure TUpdateSQLFrame.btOpenClick(Sender: TObject);
begin
  try
    UniQuery.Open;
  finally
    ShowState;
  end;
end;

procedure TUpdateSQLFrame.btCloseClick(Sender: TObject);
begin
  UniQuery.Close;
  ShowState;
end;

procedure TUpdateSQLFrame.btPrepareClick(Sender: TObject);
begin
  try
    UniQuery.Prepare;
  finally
    ShowState;
  end;
end;

procedure TUpdateSQLFrame.btUnPrepareClick(Sender: TObject);
begin
  UniQuery.UnPrepare;
  ShowState;
end;

procedure TUpdateSQLFrame.btExecuteClick(Sender: TObject);
begin
  try
    UniQuery.Execute;
  finally
    ShowState;
  end;
end;

procedure TUpdateSQLFrame.Initialize;
begin
  UniQuery.Connection := TUniConnection(Connection);
  DeleteQuery.Connection := TUniConnection(Connection);
  InsertQuery.Connection := TUniConnection(Connection);
  ModifyQuery.Connection := TUniConnection(Connection);
  RefreshQuery.Connection := TUniConnection(Connection);
  
  meSQL.Lines.Assign(UniQuery.SQL);
  ShowState;
end;

procedure TUpdateSQLFrame.btRefreshRecordClick(Sender: TObject);
begin
  UniQuery.RefreshRecord;
end;

procedure TUpdateSQLFrame.SetDebug(Value: boolean);
begin
  UniQuery.Debug := Value;
end;

procedure TUpdateSQLFrame.cbObjectClick(Sender: TObject);
  function GetComponent(cbObject: TCheckBox; Component: TComponent): TComponent;
  begin
    if cbObject.Checked then
      Result := Component
    else
      Result := nil;
  end;
begin
  UniUpdateSQL.DeleteObject := GetComponent(cbDeleteObject, DeleteQuery);
  UniUpdateSQL.InsertObject := GetComponent(cbInsertObject, InsertQuery);
  UniUpdateSQL.ModifyObject := GetComponent(cbModifyObject, ModifyQuery);
  UniUpdateSQL.RefreshObject := GetComponent(cbRefreshObject, RefreshQuery);
end;

{$IFDEF FPC}
initialization
  {$i UpdateSQL.lrs}
{$ENDIF}

end.
