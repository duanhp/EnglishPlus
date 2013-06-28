unit Macros;

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils, Db, Windows, Messages, Graphics, 
  Controls, Forms, Dialogs, Buttons, DBCtrls, ExtCtrls, 
  Grids, DBGrids, StdCtrls, ToolWin, ComCtrls, UniDacVcl,
  DBAccess, Uni, DemoFrame, UniDacDemoForm,
  {$IFNDEF FPC}MemDS, VirtualTable{$ELSE}MemDataSet{$ENDIF};

type
  TMacrosFrame = class(TDemoFrame)
    meSQL: TMemo;
    Panel4: TPanel;
    Label1: TLabel;
    Panel5: TPanel;
    Label2: TLabel;
    DBGrid: TDBGrid;
    UniQuery: TUniQuery;
    DataSource1: TDataSource;
    vtMacros: TVirtualTable;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    Panel2: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    DBNavigator: TDBNavigator;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    destructor Destroy; override;
    // Demo management
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

destructor TMacrosFrame.Destroy;
begin
  UniQuery.Connection.Macros.Clear;

  inherited;
end;

procedure TMacrosFrame.btOpenClick(Sender: TObject);
var
  Name, Value, Condition: string;
begin
  UniQuery.Connection.Macros.Clear;

  vtMacros.First;
  while not vtMacros.Eof do begin
    Name := vtMacros.Fields[0].AsString;
    Value := vtMacros.Fields[1].AsString;
    Condition := vtMacros.Fields[2].AsString;

    UniQuery.Connection.Macros.Add(Name, Value, Condition);

    vtMacros.Next;
  end;

  UniQuery.SQL.Assign(meSQL.Lines);
  UniQuery.Open;
end;

procedure TMacrosFrame.btCloseClick(Sender: TObject);
begin
  UniQuery.Close;
end;

// Demo management
procedure TMacrosFrame.Initialize;
begin
  UniQuery.Connection := Connection as TUniConnection;
end;

initialization
{$IFDEF FPC}
{$I Macros.lrs}
{$ENDIF}
end.
