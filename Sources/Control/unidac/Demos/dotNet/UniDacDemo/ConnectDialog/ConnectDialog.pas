unit ConnectDialog;

interface

uses
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Grids, DBGrids, StdCtrls, ToolWin, ComCtrls, Buttons, UniDacVcl,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DB, DBAccess, Uni, UniDacDemoForm, DemoFrame, InheritedConnectForm, MyConnectForm,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF};

type
  TConnectDialogFrame = class(TDemoFrame)
    ToolBar: TPanel;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    UniQuery: TUniQuery;
    Panel1: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    DBNavigator: TDBNavigator;
    Panel3: TPanel;
    rbInherited: TRadioButton;
    rbMy: TRadioButton;
    rbDefault: TRadioButton;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure rbDefaultClick(Sender: TObject);
    procedure rbMyClick(Sender: TObject);
    procedure rbInheritedClick(Sender: TObject);
  public
    destructor Destroy; override;
    procedure Initialize; override;
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

procedure TConnectDialogFrame.Initialize;
begin
  UniQuery.Connection := Connection as TUniConnection;
end;

destructor TConnectDialogFrame.Destroy;
begin
  UniDacForm.UniConnectDialog.DialogClass:= '';
  inherited;
end;

procedure TConnectDialogFrame.btOpenClick(Sender: TObject);
begin
  UniQuery.Open;
end;

procedure TConnectDialogFrame.btCloseClick(Sender: TObject);
begin
  UniQuery.Close;
end;

procedure TConnectDialogFrame.rbDefaultClick(Sender: TObject);
begin
  UniDacForm.UniConnectDialog.DialogClass:= '';
end;

procedure TConnectDialogFrame.rbMyClick(Sender: TObject);
begin
  UniDacForm.UniConnectDialog.DialogClass:= 'TfmMyConnect';
end;

procedure TConnectDialogFrame.rbInheritedClick(Sender: TObject);
begin
  UniDacForm.UniConnectDialog.DialogClass:= 'TfmInheritedConnect';
end;

initialization
{$IFDEF FPC}
{$I ConnectDialog.lrs}
{$ENDIF}

end.

