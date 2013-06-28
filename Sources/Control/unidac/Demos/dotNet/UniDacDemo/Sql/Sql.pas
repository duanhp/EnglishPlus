unit Sql;

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
  DB, MemUtils, DBAccess, Uni, DemoFrame, DAScript, UniScript, ParamType, UniDacDemoForm;

type
  TSqlFrame = class(TDemoFrame)
    meSQL: TMemo;
    UniSQL: TUniSQL;
    ToolBar: TPanel;
    btExecute: TSpeedButton;
    btParType: TSpeedButton;
    btCreateProcCall: TSpeedButton;
    meResult: TMemo;
    Splitter1: TSplitter;
    edStoredProcNames: TComboBox;
    btPrepare: TSpeedButton;
    btUnprepare: TSpeedButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure btExecuteClick(Sender: TObject);
    procedure btParTypeClick(Sender: TObject);
    procedure btCreateProcCallClick(Sender: TObject);
    procedure meSQLExit(Sender: TObject);
    procedure btPrepareClick(Sender: TObject);
    procedure btUnprepareClick(Sender: TObject);
    procedure edStoredProcNamesChange(Sender: TObject);
    procedure edStoredProcNamesDropDown(Sender: TObject);

  private
    FParamTypeForm: TParamTypeForm;
    Procedure ShowState;
  public
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;

    destructor Destroy; override;
  end;

var
  fmSql: TSqlFrame;

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

procedure TSqlFrame.ShowState;
var
  St:string;

  procedure AddSt(S:string);
  begin
    if St <> '' then
      St := St + ', ';
    St := St + S;
  end;

begin
  St := '';

  if UniSQL.Prepared then begin
    AddSt('Prepared');
  end;

  UniDacForm.StatusBar.Panels[1].Text := St;

  meSQL.Lines.Text := UniSQL.SQL.Text;
end;

procedure TSqlFrame.btExecuteClick(Sender: TObject);
var
  s: string;
begin
  if Trim(UniSQL.SQL.Text) <> Trim(meSQL.Lines.Text) then
    UniSQL.SQL.Text := meSQL.Lines.Text;
  UniSQL.Execute;
  s := 'Rows affected: ' + IntToStr(UniSQL.RowsAffected);
  meResult.Lines.Add(s);
end;

procedure TSqlFrame.btParTypeClick(Sender: TObject);
begin
  UniSQL.SQL.Text := meSQL.Lines.Text;
  FParamTypeForm.ShowModal;
end;

procedure TSqlFrame.btCreateProcCallClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  try
    UniSQL.CreateProcCall(edStoredProcNames.Text);
    meResult.Lines.Text := UniSQL.SQL.Text;
    meResult.Lines.Add('Parameters: ');
    for i := 0 to UniSQL.Params.Count - 1 do begin
      s := '  ' + UniSql.Params[i].Name + ': ';;
      case UniSQL.Params[i].DataType of
        ftString: s := s + 'String';
        ftInteger: s := s + 'Integer';
        ftFloat: s := s + 'Float';
        ftDate: s := s + 'Date';
      else
        s := s + '< Other >';
      end;
      case UniSQL.Params[i].ParamType of
        ptUnknown: s := s + ' (Unknown)';
        ptInput: s := s + ' (Input)';
        ptOutput: s := s + ' (Output)';
        ptInputOutput: s := s + ' (InputOutput)';
        ptResult: s := s + ' (Result)';
      end;
      meResult.Lines.Add(s);
    end;
  finally
    meResult.Lines.Add('');
    ShowState;
  end;
end;

procedure TSqlFrame.meSQLExit(Sender: TObject);
begin
  UniSQL.SQL.Text := meSQL.Lines.Text;
end;

procedure TSqlFrame.btPrepareClick(Sender: TObject);
begin
  try
    if Trim(UniSQL.SQL.Text) <> Trim(meSQL.Lines.Text) then
      UniSQL.SQL.Text := meSQL.Lines.Text;
    UniSQL.Prepare;
  finally
    ShowState;
  end;
end;

procedure TSqlFrame.btUnprepareClick(Sender: TObject);
begin
  try
    UniSQL.UnPrepare;
  finally
    ShowState;
  end;
end;

procedure TSqlFrame.edStoredProcNamesChange(Sender: TObject);
begin
  ShowState;
end;

procedure TSqlFrame.edStoredProcNamesDropDown(Sender: TObject);
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

procedure TSqlFrame.Initialize;
begin
  UniSQL.Connection := TUniConnection(Connection);
  
  FParamTypeForm := TParamTypeForm.Create(nil);
  FParamTypeForm.Params := UniSQL.Params;
  meSQL.Lines.Text := UniSQL.SQL.Text;
  edStoredProcNames.Items.Add('sel_from_emp');
  edStoredProcNames.ItemIndex := 0;
  edStoredProcNamesChange(self)
end;

procedure TSqlFrame.SetDebug(Value: boolean);
begin
  UniSQL.Debug := Value;
end;

destructor TSqlFrame.Destroy;
begin
  FParamTypeForm.Free;
  inherited;
end;

{$IFDEF FPC}
initialization
  {$i Sql.lrs}
{$ENDIF}

end.




