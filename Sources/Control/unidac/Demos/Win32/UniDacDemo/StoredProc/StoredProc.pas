unit StoredProc;

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
  DB, MemUtils, DBAccess, Uni, DemoFrame, DAScript, UniScript, ParamType;

type
  TStoredProcFrame = class(TDemoFrame)
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    ToolBar: TPanel;
    btOpen: TSpeedButton;
    btPrepare: TSpeedButton;
    btUnPrepare: TSpeedButton;
    UniStoredProc: TUniStoredProc;
    btClose: TSpeedButton;
    btExecute: TSpeedButton;
    edStoredProcNames: TComboBox;
    lbStoredProcName: TLabel;
    btParameters: TSpeedButton;
    btPrepareSQL: TSpeedButton;
    meSQL: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    procedure btOpenClick(Sender: TObject);
    procedure btPrepareClick(Sender: TObject);
    procedure btUnPrepareClick(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
    procedure btPrepareSQLClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btParametersClick(Sender: TObject);
    procedure edStoredProcNamesDropDown(Sender: TObject);
    procedure edStoredProcNamesChange(Sender: TObject);
  private
    FParamTypeForm: TParamTypeForm;
    procedure ShowState;
  public
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;
    
    destructor Destroy; override;
  end;

var
  fmStoredProc: TStoredProcFrame;

implementation

uses Math, UniDacDemoForm;

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

procedure TStoredProcFrame.ShowState;
var
  St: string;
  i: integer;

  procedure AddSt(S:string);
  begin
    if St <> '' then
      St := St + ', ';
    St := St + S;
  end;

begin
  St := '';

  if UniStoredProc.Prepared then begin
    AddSt('Prepared');

    if UniStoredProc.IsQuery then
      AddSt('IsQuery');

  end;

  if UniStoredProc.Active then
    AddSt('Active')
  else
    AddSt('Inactive');

  UniDacForm.StatusBar.Panels[2].Text := St;

  if UniStoredProc.SQL.Text = '' then
    meSQL.Lines.Text := 'UniStoredProc.SQL is empty.'
  else
    meSQL.Lines.Text := 'UniStoredProc.SQL is:' + #13#10 + UniStoredProc.SQL.Text;
  if UniStoredProc.Params.Count > 0 then begin
    meSQL.Lines.Add('');
    meSQL.Lines.Add('UniStoredProc.Params:');
    for i := 0 to UniStoredProc.Params.Count - 1 do
      meSQL.Lines.Add(UniStoredProc.Params[i].Name + ' = ' + UniStoredProc.Params[i].AsString);
  end;
end;

procedure TStoredProcFrame.btOpenClick(Sender: TObject);
begin
  try
    UniStoredProc.Open;
  finally
    ShowState;
  end;
end;

procedure TStoredProcFrame.btPrepareClick(Sender: TObject);
begin
  try
    UniStoredProc.Prepare;
  finally
    ShowState;
  end;
end;

procedure TStoredProcFrame.btUnPrepareClick(Sender: TObject);
begin
  try
    UniStoredProc.UnPrepare;
  finally
    ShowState;
  end;
end;

procedure TStoredProcFrame.btExecuteClick(Sender: TObject);
begin
  try
    UniStoredProc.Execute;
  finally
    ShowState;
  end;
end;

procedure TStoredProcFrame.btPrepareSQLClick(Sender: TObject);
begin
  try
    UniStoredProc.PrepareSQL;
  finally
    ShowState;
  end;
end;


procedure TStoredProcFrame.btCloseClick(Sender: TObject);
begin
  UniStoredProc.Close;
  ShowState;
end;

procedure TStoredProcFrame.btParametersClick(Sender: TObject);
begin
  FParamTypeForm.ShowModal;
end;

procedure TStoredProcFrame.edStoredProcNamesDropDown(Sender: TObject);
var
  SpName: String;
  List: _TStringList;
begin
  SpName := edStoredProcNames.Text;
  List := _TStringList.Create;
  try
    Connection.GetStoredProcNames(List);
    AssignStrings(List, edStoredProcNames.Items);
  finally
    List.Free;
  end;
  edStoredProcNames.ItemIndex := edStoredProcNames.Items.IndexOf(SpName);
end;

procedure TStoredProcFrame.edStoredProcNamesChange(Sender: TObject);
begin
  try
    UniStoredProc.StoredProcName := edStoredProcNames.Text;
  finally
    ShowState;
  end;
end;

procedure TStoredProcFrame.Initialize;
begin
  UniStoredProc.Connection := TUniConnection(Connection);
  
  FParamTypeForm := TParamTypeForm.Create(nil);
  FParamTypeForm.Params := UniStoredProc.Params;
  edStoredProcNames.Items.Add('sel_from_emp');
  edStoredProcNames.ItemIndex := 0;
  edStoredProcNamesChange(nil)
end;

procedure TStoredProcFrame.SetDebug(Value: boolean);
begin
  UniStoredProc.Debug := Value;
end;

destructor TStoredProcFrame.Destroy;
begin
  fmStoredProc.Free;
  inherited;
end;

{$IFDEF FPC}
initialization
  {$i StoredProc.lrs}
{$ENDIF}

end.
