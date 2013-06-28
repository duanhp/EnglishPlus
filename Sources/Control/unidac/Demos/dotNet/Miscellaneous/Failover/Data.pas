unit Data;

interface

uses
  Classes, SysUtils, WinSock,
{$IFNDEF LINUX}
  Windows, Menus, ImgList, StdCtrls, ComCtrls, Buttons, ExtCtrls, Graphics,
  Controls, Forms,
{$ELSE}
  QMenus, QImgList, QStdCtrls, QComCtrls, QButtons, QExtCtrls, QGraphics,
  QControls, QForms,
{$ENDIF}
  DB, MemData, DBAccess,
  Uni, MemDS, UniDacVcl, DAScript, UniScript,
{$IFNDEF CLR}
  OracleUniProvider,
  SQLServerUniProvider,
  InterBaseUniProvider,
  MySQLUniProvider,
  PostgreSQLUniProvider
{$ELSE}
  Devart.UniDac.Oracle.OracleUniProvider,
  Devart.UniDac.SQLServer.SQLServerUniProvider,
  Devart.UniDac.InterBase.InterBaseUniProvider,
  Devart.UniDac.MySQL.MySQLUniProvider,
  Devart.UniDac.PostgreSQL.PostgreSQLUniProvider
{$ENDIF}
  ;

type
  TDM = class(TDataModule)
    Connection: TUniConnection;
    quDetail: TUniQuery;
    quMaster: TUniQuery;
    dsMaster: TDataSource;
    dsDetail: TDataSource;
    scCreate_MySQL: TUniScript;
    scCreate_SQLServer: TUniScript;
    scCreate_InterBase: TUniScript;
    scCreate_Oracle: TUniScript;
    scDrop: TUniScript;
    scDrop_InterBase: TUniScript;
  private
  public
    procedure KillSession;
    function InTransaction: boolean;
    procedure StartTransaction;
    procedure RollbackTransaction;
    procedure CommitTransaction;
    procedure CreateTables;
    procedure DropTables;
  end;

const
  ProductColor: TColor = clBlue;

var
  DM: TDM;

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

uses
  Main;

{ TDM }

procedure TDM.KillSession;
var
  KillConnection: TUniConnection;
  Query: TUniQuery;
  SQL: TUniSQL;
  ClientId, SPID, Serial: string;
  Sock: longint;
begin
  KillConnection := TUniConnection.Create(nil);
  try
    KillConnection.ProviderName := Connection.ProviderName;
    KillConnection.Server := Connection.Server;
    KillConnection.Username := Connection.Username;
    KillConnection.Password := Connection.Password;
    KillConnection.Database := Connection.Database;
    KillConnection.Port := Connection.Port;
    KillConnection.LoginPrompt := False;

    if Connection.ProviderName = 'InterBase' then begin
      Query := TUniQuery.Create(nil);
      Query.Connection := Connection;
      SQL := TUniSQL.Create(nil);
      SQL.Connection := KillConnection;
      try
        Query.SQL.Text := 'SELECT TMP$ATTACHMENT_ID FROM TMP$ATTACHMENTS WHERE TMP$STATE = ''ACTIVE''';
        Query.Open;
        SQL.SQL.Text := 'UPDATE TMP$ATTACHMENTS SET TMP$STATE = ''SHUTDOWN'' WHERE TMP$ATTACHMENT_ID = :ATTACHMENT_ID';
        SQL.ParamByName('ATTACHMENT_ID').AsInteger := Query.FieldByName('TMP$ATTACHMENT_ID').AsInteger;
        Query.Close;

        SQL.Execute;
      finally
        Query.Free;
        SQL.Free;
      end;
    end
    else
    if Connection.ProviderName = 'Oracle' then begin
      ClientId := IntToStr(GetTickCount);
      Connection.ExecSQL('BEGIN DBMS_SESSION.SET_IDENTIFIER(:a); END;', [ClientId]);
      Query := TUniQuery.Create(nil);
      try
        Query.Connection := Connection;
        Query.SQL.Text := 'SELECT SID, Serial# FROM v$session ' +
          'WHERE Username = :UN AND Client_Identifier = :ID';
        Query.ParamByName('UN').AsString := Connection.Username;
        Query.ParamByName('ID').AsString := ClientId;
        Query.Open;
        if Query.RecordCount <> 1 then
          raise Exception.Create('Session can''t be killed');
        SPID := Query.FieldByName('SID').AsString;
        Serial := Query.FieldByName('Serial#').AsString;

        KillConnection.ExecSQL('ALTER SYSTEM KILL SESSION ''' + SPID + ',' + Serial + '''', []);
      finally
        Query.Free;
      end;
    end
    else
    if Connection.ProviderName = 'MySQL' then begin
      KillConnection.ExecSQL('KILL ' + IntToStr(GetConnectionThreadID(Connection)), []);
      Sleep(500); // wait until session is killed
    end
    else
    if Connection.ProviderName = 'SQL Server' then begin
      Query := TUniQuery.Create(nil);
      try
        Query.Connection := Connection;
        Query.SQL.Text := 'SELECT @@SPID AS ''SPID''';
        Query.Open;
        SPID := Query.FieldByName('SPID').AsString;

        KillConnection.ExecSQL(Format('KILL %s', [SPID]), []);
      finally
        Query.Free;
      end;
    end
    else
    if Connection.ProviderName = 'PostgreSQL' then begin
      Sock := TPostgreSQLUniProvider.GetSocket(Connection);
      closesocket(Sock);
    end;

  finally
    KillConnection.Free;
  end;
end;

function TDM.InTransaction: boolean;
begin
  Result := Connection.InTransaction;
end;

procedure TDM.StartTransaction;
begin
  Connection.StartTransaction;
end;

procedure TDM.CommitTransaction;
begin
  Connection.Commit;
end;

procedure TDM.RollbackTransaction;
begin
  Connection.Rollback;
end;

procedure TDM.CreateTables;
begin
  if Connection.ProviderName = 'Oracle' then
    scCreate_Oracle.Execute
  else
  if Connection.ProviderName = 'InterBase' then
    scCreate_InterBase.Execute
  else
  if Connection.ProviderName = 'SQL Server' then
    scCreate_SQLServer.Execute
  else
  if Connection.ProviderName = 'MySQL' then
    scCreate_MySQL.Execute;
end;

procedure TDM.DropTables;
begin
  if Connection.ProviderName = 'InterBase' then
    scDrop_InterBase.Execute
  else
    scDrop.Execute;
end;

end.
