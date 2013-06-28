unit Table;

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
  DB, DBAccess, Uni, DemoFrame, UniDacDemoForm;
  
type
  TTableFrame = class(TDemoFrame)
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    btPrepare: TSpeedButton;
    btUnPrepare: TSpeedButton;
    btExecute: TSpeedButton;
    DBNavigator: TDBNavigator;
    UniTable: TUniTable;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    edTableName: TEdit;
    Panel6: TPanel;
    Label2: TLabel;
    edKeyFields: TEdit;
    Panel7: TPanel;
    Label3: TLabel;
    edOrderFields: TEdit;
    Panel8: TPanel;
    Label4: TLabel;
    edFilterSQL: TEdit;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btPrepareClick(Sender: TObject);
    procedure btUnPrepareClick(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
    procedure edTableNameExit(Sender: TObject);
    procedure edKeyFieldsExit(Sender: TObject);
    procedure edOrderFieldsExit(Sender: TObject);
    procedure edFilterSQLExit(Sender: TObject);
  private
    procedure ShowState;
    procedure SetTableProperties;
  public
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;
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
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}
{$ENDIF}

procedure TTableFrame.ShowState;
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

  if UniTable.Prepared then begin
    AddSt('Prepared');

    if UniTable.IsQuery then
      AddSt('IsQuery');

  end;

  if UniTable.Active then
    AddSt('Active')
  else
    AddSt('Inactive');

  UniDacForm.StatusBar.Panels[1].Text:= St;
end;

procedure TTableFrame.SetTableProperties;
begin
  UniTable.TableName := edTableName.Text;
  UniTable.KeyFields := edKeyFields.Text;
  UniTable.FilterSQL := edFilterSQL.Text;
  UniTable.OrderFields := edOrderFields.Text;
end;

procedure TTableFrame.btOpenClick(Sender: TObject);
begin
  try
    SetTableProperties;
    UniTable.Open;
  finally
    edKeyFields.Text:= UniTable.KeyFields;
    ShowState;
  end;
end;

procedure TTableFrame.btCloseClick(Sender: TObject);
begin
  UniTable.Close;
  ShowState;
end;

procedure TTableFrame.btPrepareClick(Sender: TObject);
begin
  try
    SetTableProperties;
    UniTable.Prepare;
  finally
    edKeyFields.Text:= UniTable.KeyFields;  
    ShowState;
  end;
end;

procedure TTableFrame.btUnPrepareClick(Sender: TObject);
begin
  UniTable.UnPrepare;
  ShowState;
end;

procedure TTableFrame.btExecuteClick(Sender: TObject);
begin
  try
    SetTableProperties;
    UniTable.Execute;
  finally
    edKeyFields.Text:= UniTable.KeyFields;  
    ShowState;
  end;
end;

procedure TTableFrame.edTableNameExit(Sender: TObject);
begin
  UniTable.TableName:= edTableName.Text;
  edKeyFields.Text:= UniTable.KeyFields;
  ShowState;
end;

procedure TTableFrame.edKeyFieldsExit(Sender: TObject);
begin
  UniTable.KeyFields:= edKeyFields.Text;
  ShowState;
end;

procedure TTableFrame.edOrderFieldsExit(Sender: TObject);
begin
  UniTable.OrderFields:= edOrderFields.Text;
  edKeyFields.Text:= UniTable.KeyFields;
  ShowState;
end;

procedure TTableFrame.edFilterSQLExit(Sender: TObject);
begin
  try
    UniTable.FilterSQL:= edFilterSQL.Text;
  finally
    edFilterSQL.Text:= UniTable.FilterSQL;
    ShowState;
  end;
end;

procedure TTableFrame.Initialize;
begin
  UniTable.Connection := TUniConnection(Connection);
  
  edTableName.Text := UniTable.TableName;
  edKeyFields.Text := UniTable.KeyFields;
  edOrderFields.Text := UniTable.OrderFields;
  edFilterSQL.Text := UniTable.FilterSQL;
  ShowState;
end;

procedure TTableFrame.SetDebug(Value: boolean);
begin
  UniTable.Debug := Value;
end;

{$IFDEF FPC}
initialization
  {$i Table.lrs}
{$ENDIF}

end.
