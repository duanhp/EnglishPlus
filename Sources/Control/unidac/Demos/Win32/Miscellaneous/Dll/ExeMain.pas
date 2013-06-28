unit ExeMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, DBGrids, Db, MemDS, DBCtrls, DBAccess, 
  Uni, UniDacVcl, Buttons,
  OracleUniProvider,
  SQLServerUniProvider,
  InterBaseUniProvider,
  PostgreSQLUniProvider,
  MySQLUniProvider;

type
  TfmExeMain = class(TForm)
    UniConnection: TUniConnection;
    UniConnectDialog: TUniConnectDialog;
    pnToolBar: TPanel;
    DBGrid: TDBGrid;
    UniQuery: TUniQuery;
    DataSource: TDataSource;
    Panel1: TPanel;
    btConnect: TSpeedButton;
    btDisconnect: TSpeedButton;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    Panel2: TPanel;
    DBNavigator: TDBNavigator;
    btFreeDLL: TSpeedButton;
    btLoadDLL: TSpeedButton;
    btShowForm: TSpeedButton;
    btHideForms: TSpeedButton;
    procedure btLoadDLLClick(Sender: TObject);
    procedure btFreeDLLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure btDisconnectClick(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btShowFormClick(Sender: TObject);
    procedure btHideFormsClick(Sender: TObject);
  private
    hDLL:HModule;

  public

  end;

  TAssignUniConnection = procedure (UniConnection: TUniConnection); cdecl;
  TShowForm = procedure; cdecl;
  THideForms = procedure; cdecl;

var
  fmExeMain: TfmExeMain;

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

procedure TfmExeMain.btLoadDLLClick(Sender: TObject);
var
  AssignUniConnection: TAssignUniConnection;
begin
  if hDLL = 0 then begin
    hDLL := LoadLibrary('UniDLL.dll');
    if hDLL <> 0 then begin
      @AssignUniConnection := GetProcAddress(hDLL, 'AssignConnection');
      if @AssignUniConnection <> nil then
        AssignUniConnection(UniConnection);
      MessageDlg('DLL is loaded', mtInformation, [mbOk], 0);
    end
    else
      MessageDlg('Can not load DLL', mtError, [mbOk], 0);
  end
  else
    MessageDlg('DLL is already loaded', mtInformation, [mbOk], 0);
end;

procedure TfmExeMain.btShowFormClick(Sender: TObject);
var
  ShowForm: TShowForm;
begin
  if hDLL <> 0 then begin
    @ShowForm := GetProcAddress(hDLL, 'ShowForm');
    if @ShowForm <> nil then
      ShowForm;
    SetFocus;
  end
  else
    MessageDlg('DLL is not loaded', mtError, [mbOk], 0);
end;

procedure TfmExeMain.btHideFormsClick(Sender: TObject);
var
  HideForms: THideForms;
begin
  if hDLL <> 0 then begin
    @HideForms := GetProcAddress(hDLL, 'HideForms');
    if @HideForms <> nil then
      HideForms;
  end
  else
    MessageDlg('DLL is not loaded', mtError, [mbOk], 0);
end;

procedure TfmExeMain.btFreeDLLClick(Sender: TObject);
begin
  if hDLL <> 0 then begin
    FreeLibrary(hDLL);
    hDLL:= 0;
    MessageDlg('DLL is unloaded', mtInformation, [mbOk], 0);
  end
  else
    MessageDlg('DLL is not loaded', mtError, [mbOk], 0);
end;

procedure TfmExeMain.FormCreate(Sender: TObject);
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
  hDLL:= 0;
end;

procedure TfmExeMain.btConnectClick(Sender: TObject);
begin
  UniConnection.Connect;
end;

procedure TfmExeMain.btDisconnectClick(Sender: TObject);
begin
  UniConnection.Disconnect;
end;

procedure TfmExeMain.btOpenClick(Sender: TObject);
begin
  UniQuery.Open;
end;

procedure TfmExeMain.btCloseClick(Sender: TObject);
begin
  UniQuery.Close;
end;

procedure TfmExeMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if hDLL <> 0 then
    FreeLibrary(hDLL);
end;

end.

