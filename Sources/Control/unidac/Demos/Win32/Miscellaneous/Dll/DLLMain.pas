unit DLLMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, MemDS, StdCtrls, ExtCtrls, DBCtrls, DBAccess,
  Uni, UniDacVcl, Buttons,
  OracleUniProvider,
  SQLServerUniProvider,
  InterBaseUniProvider,
  PostgreSQLUniProvider,
  MySQLUniProvider;

type
  TfmDllMain = class(TForm)
    UniQuery: TUniQuery;
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    pnToolBar: TPanel;
    Panel1: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    DBNavigator: TDBNavigator;
    UniConnection: TUniConnection;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure AssignConnection(UniConnection: TUniConnection); cdecl;
procedure ShowForm; cdecl;
procedure HideForms; cdecl;

implementation

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

{$IFNDEF LINUX}
  {$IFNDEF VER130}
  {$IFNDEF VER140}
  {$IFNDEF CLR}
    {$DEFINE XPMAN}
    {$R WindowsXP.res}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF XPMAN}
uses
  UxTheme;
{$ENDIF}

var
  ExternalUniConnection: TUniConnection;
  FormList: TList;
  FormCount: integer;

procedure AssignConnection(UniConnection: TUniConnection); cdecl;
begin
  ExternalUniConnection:= UniConnection;
end;

procedure ShowForm; cdecl;
begin
  with TfmDllMain.Create(Application) do begin
    Inc(FormCount);
    Caption:= IntToStr(FormCount) + '. ' + Caption;
    UniConnection.AssignConnect(ExternalUniConnection);
    UniQuery.Active := True;
    Show;
  end;
end;

procedure HideForms; cdecl;
begin
  while FormList.Count > 0 do begin
    TForm(FormList[0]).Free;
    FormList.Delete(0);
  end;
end;

procedure TfmDllMain.FormShow(Sender: TObject);
begin
  FormList.Add(Self);
end;

procedure TfmDllMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormList.Remove(Self);
  Action := caFree;
end;

procedure TfmDllMain.btOpenClick(Sender: TObject);
begin
  UniQuery.Open;
end;

procedure TfmDllMain.btCloseClick(Sender: TObject);
begin
  UniQuery.Close;
end;

procedure TfmDllMain.FormCreate(Sender: TObject);
{$IFDEF XPMAN}
  procedure UpdateStyle(Control: TWinControl);
  var
    Panel: TPanel;
    i: integer;
  begin
    for i := 0 to Control.ControlCount - 1 do begin
      if Control.Controls[i] is TSpeedButton then
        TSpeedButton(Control.Controls[i]).Flat := False
      else
      if Control.Controls[i] is TDBNavigator then
        TDBNavigator(Control.Controls[i]).Flat := False;
      if Control.Controls[i] is TWinControl then begin
        if (Control.Controls[i] is TPanel) then begin
            Panel := TPanel(Control.Controls[i]);
            Panel.ParentBackground := False;
            Panel.Color := clBtnFace;
          end;
        UpdateStyle(TWinControl(Control.Controls[i]));
      end;
    end;
  end;
{$ENDIF}
begin
{$IFDEF XPMAN}
  if UseThemes then
    UpdateStyle(Self);
{$ENDIF}
end;

initialization
  FormCount:= 0;
  FormList:= TList.Create;
finalization
  HideForms;
  FormList.Free;
end.

