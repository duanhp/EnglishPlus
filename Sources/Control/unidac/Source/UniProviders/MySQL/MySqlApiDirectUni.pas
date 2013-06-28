
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlApiDirectUni;
{$ENDIF}

interface

uses
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF VER6P}
  Variants, Types,
{$ENDIF}
  SyncObjs, SysUtils, MemUtils, CRAccess, CRVio,
{$IFNDEF UNIDACPRO}
  MyCall, MySqlApi, MySqlType, MySqlBind, MySqlResultSet,
  MySqlSession, MySqlStmt;
{$ELSE}
  MyCallUni, MySqlApiUni, MySqlTypeUni, MySqlBindUni, MySqlResultSetUni,
  MySqlSessionUni, MySqlStmtUni;
{$ENDIF}

type
  TMySQLAPIDirect = class (TMySQLAPI)
  protected
    FQueryBuffer: TBytes;
    FQueryBufferCS: TCriticalSection;

  public
    constructor Create;
    destructor Destroy; override;

    //function mysql_num_rows(pres: PMYSQL_RES): my_ulonglong; override;
    function mysql_num_fields(pres: PMYSQL_RES): longword; override;
    //function mysql_eof(pres: PMYSQL_RES): my_bool; override;
    function mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD; override;
    //function mysql_fetch_fields(pres: PMYSQL_RES): PMYSQL_FIELDS; override;
    //function mysql_row_tell(pres: PMYSQL_RES): PMYSQL_ROWS; override;
    //function mysql_field_tell(pres: PMYSQL_RES): longword; override;

    function mysql_field_count(pmysql: PMYSQL_CON): longword; override;
    function mysql_affected_rows(pmysql: PMYSQL_CON): my_ulonglong; override;
    function mysql_insert_id(pmysql: PMYSQL_CON): my_ulonglong; override;
    function mysql_errno(pmysql: PMYSQL_CON): longword; override;
    function mysql_error(pmysql: PMYSQL_CON): AnsiString; override;
    function mysql_info(pmysql: PMYSQL_CON): AnsiString; override;
    function mysql_thread_id(pmysql: PMYSQL_CON): longword; override;
    function mysql_character_set_name(pmysql: PMYSQL_CON): AnsiString; override;

    function mysql_init(pmysql: PMYSQL_CON): PMYSQL_CON; override;
  {$IFDEF HAVE_OPENSSL}
    function mysql_ssl_set(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: PAnsiChar): longint; override;
  {$ENDIF} // HAVE_OPENSSL
    //function mysql_connect(pmysql: PMYSQL_CON; const host, user, passwd: PChar): PMYSQL_CON; override;
    //function mysql_change_user(pmysql: PMYSQL_CON; const user, passwd, db: PChar): my_bool; override;
    function mysql_real_connect(pmysql: PMYSQL_CON; host, user, passwd, db: PAnsiChar; port: longword; unix_socket: PAnsiChar; clientflag: longword): PMYSQL_CON; override;
    procedure mysql_close(pmysql: PMYSQL_CON); override;
    function mysql_select_db(pmysql: PMYSQL_CON; const db: PAnsiChar): longint; override;
    //function mysql_query(pmysql: PMYSQL_CON; const q: PChar): longint; override;
    //function mysql_send_query(pmysql: PMYSQL_CON; const q: PChar; length: longword): longint; override;
    //function mysql_read_query_result(pmysql: PMYSQL_CON): longint; override;
    function mysql_real_query(pmysql: PMYSQL_CON; const q: AnsiString; _length: longword): longint; overload; override;
    //function mysql_create_db(pmysql: PMYSQL_CON; const DB: PChar): longint; override;
    //function mysql_drop_db(pmysql: PMYSQL_CON; const DB: PChar): longint; override;
    //function mysql_shutdown(pmysql: PMYSQL_CON): longint; override;
    //function mysql_dump_debug_info(pmysql: PMYSQL_CON): longint; override;
    //function mysql_refresh(pmysql: PMYSQL_CON; refresh_options: longword): longint; override;
    function mysql_kill(pmysql: PMYSQL_CON; pid: longword): longint; override;
    function mysql_ping(pmysql: PMYSQL_CON): longint; override;
    //function mysql_stat(pmysql: PMYSQL_CON): string; override;
    function mysql_get_server_info(pmysql: PMYSQL_CON): AnsiString; override;
    function mysql_get_client_info: AnsiString; override;
    function mysql_get_host_info(pmysql: PMYSQL_CON): AnsiString; override;
    //function mysql_get_proto_info(pmysql: PMYSQL_CON): longword; override;
    //function mysql_list_dbs(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES; override;
    //function mysql_list_tables(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES; override;
    //function mysql_list_fields(pmysql: PMYSQL_CON; const table, wild: PChar): PMYSQL_RES; override;
    //function mysql_list_processes(pmysql: PMYSQL_CON): PMYSQL_RES; override;
    //function mysql_store_result(pmysql: PMYSQL_CON): PMYSQL_RES; override;
    function mysql_use_result(pmysql: PMYSQL_CON): PMYSQL_RES; override;
    function mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; var arg: integer): longint; override;
    function mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; const arg: PAnsiChar): longint; override;
    procedure mysql_free_result(pres: PMYSQL_RES); override;
    //function mysql_data_seek: procedure(pres: PMYSQL_RES; offset: my_ulonglong); override;
    //function mysql_row_seek(pres: PMYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET; override;
    //function mysql_field_seek(pres: PMYSQL_RES; offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; override;
    function mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW; override;
    function mysql_fetch_lengths(pres: PMYSQL_RES): PMYSQL_LENGTHS; override;
    //function mysql_fetch_field(pres: PMYSQL_RES): PMYSQL_FIELD; override;
    //function mysql_escape_string(_to: PChar; const from: PChar; from_length: longword): longword; override;
    //function mysql_real_escape_string(pmysql: PMYSQL_CON; _to: PChar; const from: PChar; length: longword): longword; override;
    //function mysql_debug: procedure(const debug: PChar); override;
    //function mysql_odbc_escape_string(pmysql: PMYSQL_CON; _to: PChar; to_length: longword; const from: PChar; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): string; override;
    //myodbc_remove_escape: procedure(pmysql: PMYSQL_CON; name: PChar); override;
    //function mysql_thread_safe: function: longword; override;

    function mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer; override;
    procedure mysql_server_end; override;

  //  function function mysql_reload(pmysql: PMySQL): longint; override;

  // C API Prepared Statements functions
    function mysql_stmt_init(pmysql: PMYSQL_CON): PMYSQL_STMT; override;
    function mysql_stmt_prepare(pstmt: PMYSQL_STMT; q: PAnsiChar; _length: UINT): int; override;
    //function mysql_stmt_execute(pstmt: PMYSQL_STMT): integer; override;
    function mysql_stmt_param_count(pstmt: PMYSQL_STMT): UINT; override;
    //function mysql_stmt_bind_param(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND): my_bool; override;
    //function mysql_stmt_bind_result(pstmt: PMYSQL_STMT; pbnd: PMYSQL_BIND): my_bool; override;
    function mysql_stmt_field_count(pstmt: PMYSQL_STMT): UINT; override;
    function mysql_stmt_close(pstmt: PMYSQL_STMT): my_bool; override;
    function mysql_stmt_free_result(pstmt: PMYSQL_STMT): my_bool; override;
    function mysql_stmt_errno(pstmt: PMYSQL_STMT): UINT; override;
    function mysql_stmt_error(pstmt: PMYSQL_STMT): AnsiString; override;
    //function mysql_commit(pmysql: PMYSQL_CON): my_bool; override;
    //function mysql_rollback(pmysql: PMYSQL_CON): my_bool; override;
    //function mysql_autocommit(pmysql: PMYSQL_CON;  auto_mode: my_bool): my_bool; override;
    function mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer; override;
    function _mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; var bnds: TMYSQL_BINDS; column: UINT; offset: longword): integer; override;
    function mysql_stmt_send_long_data(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool; override;
    function mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; override;
    function mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; override;
    function mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): my_ulonglong; override;
    //function mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer; override;
    function mysql_more_results(pmysql: PMYSQL_CON): my_bool; override;
    function mysql_next_result(pmysql: PMYSQL_CON): int; override;

//    function _mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW; override;

    procedure _mysql_fetch_lengths(pres: PMYSQL_RES; var Lens: TLenArr); override;
    function _mysql_fetch_value_is_null(prow: PMYSQL_ROW; fieldnr: longword): boolean; override;
    function _mysql_fetch_value_ptr(pres: PMYSQL_RES; prow: PMYSQL_ROW; fieldnr: longword): IntPtr; override;
    procedure _mysql_fetch_value_to_buff(prow: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer); override;
    procedure _mysql_fetch_value_to_buff(prow: PMYSQL_ROW; fieldnr: longword; pBuff: IntPtr; Len: integer); override;
    procedure _mysql_fetch_value_to_str(prow: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer); override;
    procedure _mysql_fetch_value_to_str(prow: PMYSQL_ROW; fieldnr: longword; pBuff: IntPtr; Len: integer); override;
    function _mysql_fetch_value_arr(prow: PMYSQL_ROW; fieldnr: longword; out Off: integer; Len: integer): TValueArr; override;
    function _mysql_fetch_value_str(prow: PMYSQL_ROW; fieldnr: longword): AnsiString; override;

    function _mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword; const Unicode: boolean): TMYSQL_FIELD; override;

    function _mysql_stmt_execute(pstmt: PMYSQL_STMT; Params: TParamDescs; const Unicode: boolean): integer; override;
    function _mysql_stmt_fetch(pstmt: PMYSQL_STMT; out row: PMYSQL_ROW): integer; override;
    function _mysql_stmt_bind_result(pstmt: PMYSQL_STMT; const bnds: TMYSQL_BINDS): my_bool; override;
    procedure _mysql_stmt_fetch_lengths(prow: PMYSQL_ROW; var Lens: TLenArr); override;
    function _mysql_stmt_fetch_value_ptr(pstmt: PMYSQL_STMT; fieldnr: longword): IntPtr; override;

    procedure CheckMySQLLib; override;
    procedure SetIOHandler(pmysql: PMYSQL_CON; IOHandler: TCRIOHandler);
    procedure SetHttpOptions(pmysql: PMYSQL_CON; HttpOptions: THttpOptions);
  end;

var
  MyAPIDirect: TMySQLAPIDirect;

implementation

uses
  DAConsts,
{$IFNDEF UNIDACPRO}
  MySqlErrors, MySqlNet;
{$ELSE}
  MySqlErrorsUni, MySqlNetUni;
{$ENDIF}

{ TMySQLAPIDirect }

constructor TMySQLAPIDirect.Create;
begin
  inherited;
  FQueryBufferCS := TCriticalSection.Create;
end;

destructor TMySQLAPIDirect.Destroy;
begin
  FQueryBufferCS.Free;
  inherited;
end;

function TMySQLAPIDirect.mysql_num_fields(pres: PMYSQL_RES): longword;
begin
  Assert(pres <> nil);
  Result := TMySqlResultSet(pres).FieldCount;
end;

function TMySQLAPIDirect.mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD;
begin
  Assert(False);
  Result := nil;
end;

function TMySQLAPIDirect.mysql_field_count(pmysql: PMYSQL_CON): longword;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).fieldCount;
end;

function TMySQLAPIDirect.mysql_affected_rows(pmysql: PMYSQL_CON): my_ulonglong;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).affectedRows;
end;

function TMySQLAPIDirect.mysql_insert_id(pmysql: PMYSQL_CON): my_ulonglong;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).insertId;
end;

function TMySQLAPIDirect.mysql_errno(pmysql: PMYSQL_CON): longword;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).errno;
end;

function TMySQLAPIDirect.mysql_error(pmysql: PMYSQL_CON): AnsiString;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).error;
end;

function TMySQLAPIDirect.mysql_info(pmysql: PMYSQL_CON): AnsiString;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).info;
end;

function TMySQLAPIDirect.mysql_thread_id(pmysql: PMYSQL_CON): longword;
begin
  Assert(pmysql <> nil);
  Result := TMySqlSession(pmysql).threadId;
end;

function TMySQLAPIDirect.mysql_character_set_name(pmysql: PMYSQL_CON): AnsiString;
begin
  Assert(pmysql <> nil);
  raise NotSupportedException.Create;
end;

function TMySQLAPIDirect.mysql_init(pmysql: PMYSQL_CON): PMYSQL_CON;
begin
  Result := TMySqlSession.Create;
end;

{$IFDEF HAVE_OPENSSL}
function TMySQLAPIDirect.mysql_ssl_set(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: PAnsiChar): longint;
var
  mysql: TMySqlSession;
begin
  Result := 0;
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.protocolType := MYSQL_PROTOCOL_SSL;
  mysql.SSL_key := key;
  mysql.SSL_cert := cert;
  mysql.SSL_ca := ca;
  mysql.SSL_capath := capath;
  mysql.SSL_cipher := cipher;
end;
{$ENDIF}

function TMySQLAPIDirect.mysql_real_connect(pmysql: PMYSQL_CON; host, user, passwd, db: PAnsiChar; port: longword; unix_socket: PAnsiChar; clientflag: longword): PMYSQL_CON;
var
  mysql: TMySqlSession;

begin
  Result := pmysql;
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    mysql.Connect(host, user, passwd, db, port, unix_socket, clientflag);
  except
    on E: EMySqlException do begin
      Result := nil;
      TMySqlSession(pmysql).errno := E.ErrorCode;
      TMySqlSession(pmysql).error := AnsiString(E.Message);
    end;
  end;
end;

procedure TMySQLAPIDirect.mysql_close(pmysql: PMYSQL_CON);
var
  mysql: TMySqlSession;
begin
  if pmysql = nil then
    Exit;

  mysql := TMySqlSession(pmysql);
  try
    mysql.Close;
    if mysql.ResultSet <> nil then
      TMySqlResultSet(mysql.ResultSet).Session := nil;
  except
    on E: EMySqlException do begin
      // silent
    end;
  end;

  try
    mysql.Free;
  except
    on E: EMySqlException do begin
      // silent
    end;
  end;
end;

function TMySQLAPIDirect.mysql_select_db(pmysql: PMYSQL_CON; const db: PAnsiChar): longint;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    mysql.SelectDb(db);
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_real_query(pmysql: PMYSQL_CON; const q: AnsiString; _length: longword): longint;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    FQueryBufferCS.Acquire;
    try
      if Integer(_length) > Length(FQueryBuffer) then
        SetLength(FQueryBuffer, _length);
      Encoding.Default.GetBytes(q, 0, _length, FQueryBuffer, 0);
      mysql.SimpleCommand(scQuery, FQueryBuffer, _length, true);
    finally
      FQueryBufferCS.Release;
    end;

    mysql.ReadQueryResult;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_kill(pmysql: PMYSQL_CON; pid: longword): longint;
  procedure int4store(buff: TBytes; _pos, val: integer);
  begin
    buff[_pos] := Byte(val and $FF);
    buff[_pos+1] := Byte((val shr 8) and $FF);
    buff[_pos+2] := Byte((val shr 16) and $FF);
    buff[_pos+3] := Byte(val shr 24);
  end;

var
  buff: TBytes;
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';

  try
    SetLength(buff, 4);
    int4store(buff, 0, pid);
    mysql.SimpleCommand(scProcessKill, buff, 4, boolean(0));
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_ping(pmysql: PMYSQL_CON): longint;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';
  try
    mysql.Ping;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_get_server_info(pmysql: PMYSQL_CON): AnsiString;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';
  try
    Result := mysql.serverVersion;
  except
    on E: EMySqlException do begin
      Result := '';
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_get_client_info: AnsiString;
begin
  Result := MYSQL_SERVER_VERSION;
end;

function TMySQLAPIDirect.mysql_get_host_info(pmysql: PMYSQL_CON): AnsiString;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';
  try
    Result := mysql.hostInfo;
  except
    on E: EMySqlException do begin
      Result := '';
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_use_result(pmysql: PMYSQL_CON): PMYSQL_RES;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  try
    if Length(mysql.fields) = 0 then begin
      Result := nil;
      Exit;
    end;

    if mysql.status <> msGetResult then
      raise EMySqlException.Create(CR_COMMANDS_OUT_OF_SYNC, [], 'mysql.status = ' + IntToStr(Integer(mysql.status)));

    Result := TMySqlResultSet.Create(mysql);
    mysql.ResultSet := Result;
    mysql.fields := nil;
    mysql.status := msUseResult;

  except
    on E: EMySqlException do begin
      Result := nil;
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; var arg: integer): longint;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';
  try
    case option of
      MYSQL_OPT_PROTOCOL:
        mysql.protocolType := MYSQL_PROTOCOL_TYPE(int(arg));
      MYSQL_OPT_NAMED_PIPE:
        mysql.protocolType := MYSQL_PROTOCOL_PIPE;
      MYSQL_OPT_CONNECT_TIMEOUT:
        mysql._connectTimeout := arg;
      MYSQL_OPT_READ_TIMEOUT, MYSQL_OPT_WRITE_TIMEOUT:
        mysql.commandTimeout := arg;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; const arg: PAnsiChar): longint;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';
  try
    case option of
      MYSQL_SET_CHARSET_NAME:
        mysql.charset := arg;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

procedure TMySQLAPIDirect.mysql_free_result(pres: PMYSQL_RES);
var
  res: TMySqlResultSet;
  session: TMySqlSession;
begin
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);

  try
    session := res.Session;
    res.Clear;

    //???
    if session <> nil then begin
      session.FreeQuery;
      session.ResultSet := nil;
    end;
  except
    on E: EMySqlException do;
  end;
  res.Free;
end;

function TMySQLAPIDirect.mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW;
var
  bind: TMySqlBinds;
  res: TMySqlResultSet;
begin
  Result := nil;
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);
  Assert(res.Session <> nil);
  res.Session.errno := 0;
  res.Session.error := '';

  try
    bind := res.Bind;
    if res.Read(bind) then
      Result := bind;
  except
    on E: EMySqlException do begin
      res.Session.errno := E.ErrorCode;
      res.Session.error := AnsiString(E.Message);
      if res.Session.serverStatus and SERVER_MORE_RESULTS_EXISTS = 0 then
        res.Session.status := msReady;
    end;
  end;
end;

function TMySQLAPIDirect.mysql_fetch_lengths(pres: PMYSQL_RES): PMYSQL_LENGTHS;
begin
  Assert(False);
  Result := nil;
end;

function TMySQLAPIDirect.mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer;
begin
  Assert(False);
  Result := 0;
end;

procedure TMySQLAPIDirect.mysql_server_end;
begin
  Assert(False);
end;

// C API Prepared Statements functions
function TMySQLAPIDirect.mysql_stmt_init(pmysql: PMYSQL_CON): PMYSQL_STMT;
begin
  Result := TMySqlStmt.Create(TMySqlSession(pmysql));
end;

function TMySQLAPIDirect.mysql_stmt_prepare(pstmt: PMYSQL_STMT; q: PAnsiChar; _length: UINT): int;
var
  stmt: TMySqlStmt;
begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    FQueryBufferCS.Acquire;
    try
      if Integer(_length) > Length(FQueryBuffer) then
        SetLength(FQueryBuffer, _length);
      Encoding.Default.GetBytes(q, 0, _length, FQueryBuffer, 0);
      stmt.Prepare(FQueryBuffer, _length);
    finally
      FQueryBufferCS.Release;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := AnsiString(E.Message);
    end;
  end;
end;

(*
function TMySQLAPIDirect.mysql_stmt_execute(pstmt: PMYSQL_STMT): integer;
var
  stmt: TMySqlStmt;
begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    stmt.Execute;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := E.Message;
    end;
  end;
end;
*)

function TMySQLAPIDirect.mysql_stmt_param_count(pstmt: PMYSQL_STMT): UINT;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).ParamCount;
end;

(*function TMySQLAPIDirect.mysql_stmt_bind_param(pstmt: PMYSQL_STMT;  bnd: PMYSQL_BIND): my_bool;
var
  stmt: TMySqlStmt;
  bind: TMySqlBinds;
  i: integer;

begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';
  bind := nil;

  try
    try
      bind := TMySqlBinds.Create(stmt.ParamCount);
      for i := 0 to stmt.ParamCount - 1 do begin
        //bind.Binds[i].IsNull := bnd.is_null;
        bind.Binds[i].Offset := bnd.offset;
        bind.Binds[i].Length := bnd.buffer_length;
        bind.Binds[i].Index := i;
        bind.Binds[i]._Type := bnd.buffer_type;

        bnd := PMYSQL_BIND(Integer(bnd) + SizeOf(MYSQL_BIND));
      end;

      stmt.BindParams(bind);
    except
      bind.Free;
      raise;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := E.Message;
    end;
  end;
end;
*)

function TMySQLAPIDirect._mysql_stmt_bind_result(pstmt: PMYSQL_STMT; const bnds: TMYSQL_BINDS): my_bool; 
var
  stmt: TMySqlStmt;
  bind: TMySqlBinds;

begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';
  bind := nil;

  try
    try
      bind := TMySqlBinds.Create(stmt.FieldCount);
      stmt.BindResult(bind);
    except
      bind.Free;
      raise;
    end;
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect.mysql_stmt_field_count(pstmt: PMYSQL_STMT): UINT;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).FieldCount;
end;

function TMySQLAPIDirect.mysql_stmt_close(pstmt: PMYSQL_STMT): my_bool;
begin
  TMySqlStmt(pstmt).Free;
  Result := 0;
end;

function TMySQLAPIDirect.mysql_stmt_free_result(pstmt: PMYSQL_STMT): my_bool;
begin
  TMySqlStmt(pstmt).FreeResult;                      
  Result := 0;
end;

function TMySQLAPIDirect.mysql_stmt_errno(pstmt: PMYSQL_STMT): UINT;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).errno;
end;

function TMySQLAPIDirect.mysql_stmt_error(pstmt: PMYSQL_STMT): AnsiString;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).error;
end;

{function TMySQLAPIDirect.mysql_commit(pmysql: PMYSQL_CON): my_bool;
begin
  Assert(False);
  Result := 0;
end;

function TMySQLAPIDirect.mysql_rollback(pmysql: PMYSQL_CON): my_bool;
begin
  Assert(False);
  Result := 0;
end;

function TMySQLAPIDirect.mysql_autocommit(pmysql: PMYSQL_CON;  auto_mode: my_bool): my_bool;
begin
  Assert(False);
  Result := 0;
end;}

function TMySQLAPIDirect.mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer;
begin
  Assert(False);
  Result := 0;
end;

function TMySQLAPIDirect._mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; var bnds: TMYSQL_BINDS; column: uint; offset: longword): integer;
begin
  Assert(False);
  Result := 0;
end;

function TMySQLAPIDirect.mysql_stmt_send_long_data(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool;
begin
  Assert(False);
  Result := 0;
end;

function TMySQLAPIDirect.mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).Result;
end;

function TMySQLAPIDirect.mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  Assert(False);
  Result := nil;
end;

function TMySQLAPIDirect.mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): my_ulonglong;
begin
  Assert(pstmt <> nil);
  Result := TMySqlStmt(pstmt).AffectedRows;
end;

{function TMySQLAPIDirect.mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer;
begin
  Assert(False);
  Result := 0;
end;}

function TMySQLAPIDirect.mysql_more_results(pmysql: PMYSQL_CON): my_bool;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  if mysql.serverStatus and SERVER_MORE_RESULTS_EXISTS <> 0 then
    Result := 1
  else
    Result := 0;
end;

function TMySQLAPIDirect.mysql_next_result(pmysql: PMYSQL_CON): int;
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.errno := 0;
  mysql.error := '';
  try
    if mysql.status <> msReady then
      raise EMySqlException.Create(CR_COMMANDS_OUT_OF_SYNC, [], 'mysql.status = ' + IntToStr(Integer(mysql.status)));

    mysql.affectedRows := -1;
    if mysql.serverStatus and SERVER_MORE_RESULTS_EXISTS <> 0 then begin
      mysql.ReadQueryResult;
      Result := 0;
    end
    else      
      Result := -1; // No more results
    except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      mysql.errno := E.ErrorCode;
      mysql.error := AnsiString(E.Message);
    end;
  end;
end;

procedure TMySQLAPIDirect._mysql_fetch_lengths(pres: PMYSQL_RES; var Lens: TLenArr);
var
  res: TMySqlResultSet;
  i: integer;
begin
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);

  if Length(Lens) <> res.FieldCount then // Perf optimization
    SetLength(Lens, res.FieldCount);
  for i := 0 to res.FieldCount - 1 do
    Lens[i] := res.Bind.Binds[i].Length;
end;

function TMySQLAPIDirect._mysql_fetch_value_ptr(pres: PMYSQL_RES; prow: PMYSQL_ROW; fieldnr: longword): IntPtr;
var
  Binds: TMySQLBinds;
  bind: TMySqlBind;
  res: TMySqlResultSet;
  _pos: integer;
begin
  Assert(prow <> nil);
  Assert(pres <> nil);

  Result := nil;
  Binds := TMySqlBinds(prow);
  bind := Binds.Binds[fieldnr];
  _pos := bind.Offset;
  if _pos = 0 then
    Exit;

  res := TMySqlResultSet(pres);
  Result := PtrOffset(GetAddrOfPinnedObject(res.GCHandleData), _pos);
end;

procedure TMySQLAPIDirect._mysql_fetch_value_to_buff(prow: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer);
var
  row: TMySqlBinds; 
  bind: TMySqlBind;
  _pos: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  bind := row.Binds[fieldnr];
  _pos := bind.Offset;
  if (_pos = 0) or (Len = 0) then
    Exit;

{$IFDEF CLR}
  Buffer.BlockCopy(row.Buffer, _pos, Buff, Off, Len);
{$ELSE}
  CopyBuffer(@row.Buffer[_pos], @Buff[Off], Len);
{$ENDIF}
end;

procedure TMySQLAPIDirect._mysql_fetch_value_to_buff(prow: PMYSQL_ROW; fieldnr: longword; pBuff: IntPtr; Len: integer);
var
  bind: TMySqlBind;
  row: TMySqlBinds;
  _pos: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  bind := row.Binds[fieldnr];
  _pos := bind.Offset;
  if (_pos = 0) or (Len = 0) then
    Exit;

{$IFDEF CLR}
  Marshal.Copy(row.Buffer, _pos, pBuff, Len);
{$ELSE}
  CopyBuffer(row.Buffer + _pos, pBuff, Len);
{$ENDIF}
end;

procedure TMySQLAPIDirect._mysql_fetch_value_to_str(prow: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer);
var
  row: TMySqlBinds;
  bind: TMySqlBind;
  _pos: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  bind := row.Binds[fieldnr];
  _pos := bind.Offset;
  if (_pos = 0) or (Len = 0) then begin
  {$IFDEF CLR}
    Buff[Off + Len] := 0;
  {$ELSE}
    Buff[Off + Len] := #0;
  {$ENDIF}
    Exit;
  end;

{$IFDEF CLR}
  Buffer.BlockCopy(row.Buffer, _pos, Buff, Off, Len);
  Buff[Off + Len] := 0;
{$ELSE}
  CopyBuffer(@row.Buffer[_pos], @Buff[Off], Len);
  Buff[Off + Len] := #0;
{$ENDIF}
end;

procedure TMySQLAPIDirect._mysql_fetch_value_to_str(prow: PMYSQL_ROW; fieldnr: longword; pBuff: IntPtr; Len: integer);
var
  row: TMySqlBinds; 
  bind: TMySqlBind;
  _pos: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  bind := row.Binds[fieldnr];
  _pos := bind.Offset;
  if (_pos = 0) or (Len = 0) then
    Exit;

{$IFDEF CLR}
  Marshal.Copy(row.Buffer, _pos, pBuff, Len);
{$ELSE}
  CopyBuffer(row.Buffer + _pos, pBuff, Len);
{$ENDIF}
  Marshal.WriteByte(pBuff, Len, 0);
end;

function TMySQLAPIDirect._mysql_fetch_value_is_null(prow: PMYSQL_ROW; fieldnr: longword): boolean;
var
  bind: TMySqlBind;
begin
  Assert(prow <> nil);
  bind := TMySqlBinds(prow).Binds[fieldnr];
  Result := bind.IsNull;
end;

function TMySQLAPIDirect._mysql_fetch_value_arr(prow: PMYSQL_ROW; fieldnr: longword; out Off: integer; Len: integer): TValueArr;
var
  row: TMySqlBinds; 
  bind: TMySqlBind;
  _pos: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  bind := row.Binds[fieldnr];

  _pos := bind.Offset;
{$IFDEF CLR}
  Result := row.Buffer;
{$ELSE}
  Result := @row.Buffer[0];
{$ENDIF}
  Off := _pos;
end;

function TMySQLAPIDirect._mysql_fetch_value_str(prow: PMYSQL_ROW; fieldnr: longword): AnsiString;
var
  row: TMySqlBinds;
  bind: TMySqlBind;
  _pos, Len: integer;
begin
  Assert(prow <> nil);
  row := TMySqlBinds(prow);
  bind := row.Binds[fieldnr];

  _pos := bind.Offset;
  Len := bind.Length;
{$IFDEF CLR}
  Result := Encoding.Default.GetString(row.Buffer, _pos, Len);
{$ELSE}
  Result := Marshal.PtrToStringAnsi(@row.Buffer[_pos], Len);
{$ENDIF}
end;

function TMySQLAPIDirect._mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword; const Unicode: boolean): TMYSQL_FIELD;
var
  res: TMySqlResultSet;
  Field: TMySqlFieldInfo;
  n: integer;
begin
  Assert(pres <> nil);
  res := TMySqlResultSet(pres);
  Assert(fieldnr < longword(res.FieldCount));
  Assert(res.Fields <> nil);
  Assert(fieldnr < longword(Length(res.Fields)));
  Field := res.Fields[fieldnr];

  Result.Name := Field.Name;
  if Field.OrgName <> '' then
    Result.OrgName := Field.OrgName
  else
    Result.OrgName := Field.Name; // Server version 3.23
  Result.Table := Field.Table;
  Result.OrgTable := Field.OrgTable;


//  Result.Flags := Field.Flags;
  Result.Flags := longword(Field.Flags);
  Result._type := Field._type;
  Result.Decimals := Field.Scale;
  Result.CharsetNr := Field.CharsetNr;
  Result.LengthInBytes := CorrectFieldLen(Field.Length);

(*
  c_char VARCHAR(100) CHARACTER SET cp1251 COLLATE cp1251_general_ci
  -----------
           SELECT c_char   Field.Length = 100
  prepared SELECT c_char   Field.Length = 100
           SELECT ''abc''  Field.Length = 9
           SELECT N''abc'' Field.Length = 9
  prepared SELECT ''abc''  Field.Length = 9
*)       

  if Unicode and (Result.Table <> '') and not res.Session.protocol41 then
    case Field._type of
      FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING, MYSQL_TYPE_VARCHAR, FIELD_TYPE_ENUM, FIELD_TYPE_SET:
      begin // to prevent F2084 Internal Error: C6662 in BDS 2005
        n := Result.LengthInBytes * MaxUTF8CharLen;
        Result.LengthInBytes := n;
      end;
      else;
    end;
end;

function TMySQLAPIDirect._mysql_stmt_execute(pstmt: PMYSQL_STMT; Params: TParamDescs; const Unicode: boolean): integer;
var
  stmt: TMySqlStmt;

begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    stmt.Execute(Params, Unicode);
    Result := 0;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := AnsiString(E.Message);
    end;
  end;
end;

function TMySQLAPIDirect._mysql_stmt_fetch(pstmt: PMYSQL_STMT; out row: PMYSQL_ROW): integer;
var
  stmt: TMySqlStmt;
  bind: TMySqlBinds;

begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);
  stmt.errno := 0;
  stmt.error := '';

  try
    Result := stmt.Fetch(bind);
    row := bind;
  except
    on E: EMySqlException do begin
      Result := E.ErrorCode;
      stmt.errno := E.ErrorCode;
      stmt.error := AnsiString(E.Message);
      if stmt.Session.serverStatus and SERVER_MORE_RESULTS_EXISTS = 0 then
        stmt.Session.status := msReady;
    end;
  end;
end;

procedure TMySQLAPIDirect._mysql_stmt_fetch_lengths(prow: PMYSQL_ROW; var Lens: TLenArr);
var
  i: integer;
  Binds: TMySQLBinds;
begin
  Binds := TMySqlBinds(prow);
  if Length(Lens) <> Length(Binds.Binds) then // Perf optimization
    SetLength(Lens, Length(Binds.Binds));
  for i := 0 to Length(Binds.Binds) - 1 do
    Lens[i] := Binds.Binds[i].Length;
end;

function TMySQLAPIDirect._mysql_stmt_fetch_value_ptr(pstmt: PMYSQL_STMT; fieldnr: longword): IntPtr;
var
  Binds: TMySQLBinds;
  _pos: integer;

  stmt: TMySqlStmt;
  bind: TMySqlBind;

begin
  Assert(pstmt <> nil);
  stmt := TMySqlStmt(pstmt);

  Result := nil;
  Binds := stmt.Result.ResultBind;
  bind := Binds.Binds[fieldnr];
  _pos := bind.Offset;
  if _pos = 0 then
    Exit;

  Result := PtrOffset(GetAddrOfPinnedObject(stmt.result.GCHandleData), _pos);
end;

procedure TMySQLAPIDirect.CheckMySQLLib;
begin
  FClientStructVer := cvDirect;
  FClientVer := string(mysql_get_client_info);
end;

procedure TMySQLAPIDirect.SetIOHandler(pmysql: PMYSQL_CON; IOHandler: TCRIOHandler);
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.IOHandler := IOHandler;
end;

procedure TMySQLAPIDirect.SetHttpOptions(pmysql: PMYSQL_CON; HttpOptions: THttpOptions);
var
  mysql: TMySqlSession;
begin
  Assert(pmysql <> nil);
  mysql := TMySqlSession(pmysql);
  mysql.HttpOptions := HttpOptions;
end;

initialization
  MyAPIDirect := TMySQLAPIDirect.Create;

finalization
  MyAPIDirect.Free;

end.
