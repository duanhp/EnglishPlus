
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlApiUni;
{$ENDIF}

interface

uses
{$IFNDEF UNIDACPRO}MyCall{$ELSE}MyCallUni{$ENDIF},
  CRAccess,
{$IFDEF CLR}
  System.Text,
  System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ELSE}
  SyncObjs, Windows,
{$ENDIF}
{$IFDEF VER6P}
  Types,
{$ENDIF}
  SysUtils, Classes, MemUtils;

type
  TLenArr = array of Integer;
  TMyLogEvent = procedure(const Text: string) of object;

  TMySQLAPI = class
  protected
    FClientVer: string;
    FClientStructVer: TClientStructVer;

    Fbnds: TMYSQL_BINDS;
    Fbnds503: TMYSQL_BINDS503;
  {$IFDEF CLR}
    FbndsGC: GCHandle;
  {$ENDIF}
    function PinBinds(const bnds: TMYSQL_BINDS; const column: UINT = 0): IntPtr;
    procedure UnPinBinds;

  public
    // function mysql_num_rows(pres: PMYSQL_RES): my_ulonglong; virtual;
    function mysql_num_fields(pres: PMYSQL_RES): longword; virtual;
    //function mysql_eof(pres: PMYSQL_RES): my_bool; virtual;
    function mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD; virtual;
    //function mysql_fetch_fields(pres: PMYSQL_RES): PMYSQL_FIELDS; virtual;
    //function mysql_row_tell(pres: PMYSQL_RES): PMYSQL_ROWS; virtual;
    //function mysql_field_tell(pres: PMYSQL_RES): longword; virtual;

    function mysql_field_count(pmysql: PMYSQL_CON): longword; virtual;
    function mysql_affected_rows(pmysql: PMYSQL_CON): my_ulonglong; virtual;
    function mysql_insert_id(pmysql: PMYSQL_CON): my_ulonglong; virtual;
    function mysql_errno(pmysql: PMYSQL_CON): longword; virtual;
    function mysql_error(pmysql: PMYSQL_CON): AnsiString; virtual;
    function mysql_info(pmysql: PMYSQL_CON): AnsiString; virtual;
    function mysql_thread_id(pmysql: PMYSQL_CON): longword; virtual;
    function mysql_character_set_name(pmysql: PMYSQL_CON): AnsiString; virtual; 

    function mysql_init(pmysql: PMYSQL_CON): PMYSQL_CON; virtual;
  {$IFDEF HAVE_OPENSSL}
    function mysql_ssl_set(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: PAnsiChar): longint; virtual;
  {$ENDIF} // HAVE_OPENSSL
    //function mysql_connect(pmysql: PMYSQL_CON; const host, user, passwd: PChar): PMYSQL_CON; virtual; 
    //function mysql_change_user(pmysql: PMYSQL_CON; const user, passwd, db: PChar): my_bool; virtual;
    function mysql_real_connect(pmysql: PMYSQL_CON; host, user, passwd, db: PAnsiChar; port: longword; unix_socket: PAnsiChar; clientflag: longword): PMYSQL_CON; virtual;
    procedure mysql_close(pmysql: PMYSQL_CON); virtual; 
    function mysql_select_db(pmysql: PMYSQL_CON; const db: PAnsiChar): longint; virtual; 
    //function mysql_query(pmysql: PMYSQL_CON; const q: PChar): longint; virtual;
    //function mysql_send_query(pmysql: PMYSQL_CON; const q: PChar; length: longword): longint; virtual; 
    //function mysql_read_query_result(pmysql: PMYSQL_CON): longint; virtual;
    function mysql_real_query(pmysql: PMYSQL_CON; const q: AnsiString; length: longword): longint; virtual; 
    //function mysql_create_db(pmysql: PMYSQL_CON; const DB: PChar): longint; virtual;
    //function mysql_drop_db(pmysql: PMYSQL_CON; const DB: PChar): longint; virtual;
    //function mysql_shutdown(pmysql: PMYSQL_CON): longint; virtual; 
    //function mysql_dump_debug_info(pmysql: PMYSQL_CON): longint; virtual; 
    //function mysql_refresh(pmysql: PMYSQL_CON; refresh_options: longword): longint; virtual; 
    function mysql_kill(pmysql: PMYSQL_CON; pid: longword): longint; virtual; 
    function mysql_ping(pmysql: PMYSQL_CON): longint; virtual;
    //function mysql_stat(pmysql: PMYSQL_CON): string; virtual;
    function mysql_get_server_info(pmysql: PMYSQL_CON): AnsiString; virtual;
    function mysql_get_client_info: AnsiString; virtual;
    function mysql_get_host_info(pmysql: PMYSQL_CON): AnsiString; virtual;
    //function mysql_get_proto_info(pmysql: PMYSQL_CON): longword; virtual; 
    //function mysql_list_dbs(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES; virtual; 
    //function mysql_list_tables(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES; virtual; 
    //function mysql_list_fields(pmysql: PMYSQL_CON; const table, wild: PChar): PMYSQL_RES; virtual; 
    //function mysql_list_processes(pmysql: PMYSQL_CON): PMYSQL_RES; virtual; 
    //function mysql_store_result(pmysql: PMYSQL_CON): PMYSQL_RES; virtual;
    function mysql_use_result(pmysql: PMYSQL_CON): PMYSQL_RES; virtual; 
    function mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; var arg: integer): longint; overload; virtual;
    function mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; const arg: PAnsiChar): longint; overload; virtual;
    procedure mysql_free_result(pres: PMYSQL_RES); virtual;
    //function mysql_data_seek: procedure(pres: PMYSQL_RES; offset: my_ulonglong); virtual; 
    //function mysql_row_seek(pres: PMYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET; virtual; 
    //function mysql_field_seek(pres: PMYSQL_RES; offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; virtual; 
    function mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW; virtual; 
    function mysql_fetch_lengths(pres: PMYSQL_RES): PMYSQL_LENGTHS; virtual;
    //function mysql_fetch_field(pres: PMYSQL_RES): PMYSQL_FIELD; virtual;
    //function mysql_escape_string(_to: PChar; const from: PChar; from_length: longword): longword; virtual; 
    //function mysql_real_escape_string(pmysql: PMYSQL_CON; _to: PChar; const from: PChar; length: longword): longword; virtual; 
    //function mysql_debug: procedure(const debug: PChar); virtual; 
    //function mysql_odbc_escape_string(pmysql: PMYSQL_CON; _to: PChar; to_length: longword; const from: PChar; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): string; virtual; 
    //myodbc_remove_escape: procedure(pmysql: PMYSQL_CON; name: PChar); virtual;
    //function mysql_thread_safe: function: longword; virtual; 

    function mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer; virtual;
    procedure mysql_server_end; virtual;

  //  function function mysql_reload(pmysql: PMySQL): longint; virtual;

  // C API Prepared Statements functions
    function mysql_stmt_init(pmysql: PMYSQL_CON): PMYSQL_STMT; virtual;
    function mysql_stmt_prepare(pmysql: PMYSQL_STMT;  query: PAnsiChar;  length: UINT): int; virtual;
    function mysql_stmt_execute(pstmt: PMYSQL_STMT): integer; virtual;
    function mysql_stmt_param_count(pstmt: PMYSQL_STMT): UINT; virtual;
    function mysql_stmt_bind_param(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND): my_bool; virtual;
    function mysql_stmt_bind_result(pstmt: PMYSQL_STMT; pbnd: PMYSQL_BIND): my_bool; virtual;
    function mysql_stmt_field_count(pstmt: PMYSQL_STMT): UINT; virtual;
    function mysql_stmt_close(pstmt: PMYSQL_STMT): my_bool; virtual;
    function mysql_stmt_free_result(pstmt: PMYSQL_STMT): my_bool; virtual;
    function mysql_stmt_errno(pstmt: PMYSQL_STMT): UINT; virtual;
    function mysql_stmt_error(pstmt: PMYSQL_STMT): AnsiString; virtual;
    //function mysql_commit(pmysql: PMYSQL_CON): my_bool; virtual;
    //function mysql_rollback(pmysql: PMYSQL_CON): my_bool; virtual;
    //function mysql_autocommit(pmysql: PMYSQL_CON;  auto_mode: my_bool): my_bool; virtual;
    function mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer; virtual;
    function mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND; column: UINT; offset: longword): integer; virtual;
    function mysql_stmt_send_long_data(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool; virtual;
    function mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; virtual;
    function mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; virtual;
    function mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): my_ulonglong; virtual;
    //function mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer; virtual;
    function mysql_more_results(pmysql: PMYSQL_CON): my_bool; virtual;
    function mysql_next_result(pmysql: PMYSQL_CON): int; virtual;

    function LoadedMySQLLib: boolean; virtual;
    procedure LoadMySQLLib; virtual;
    procedure FreeMySQLLib; virtual;
    procedure CheckMySQLLib; virtual;

    class function CorrectFieldLen(const FieldLen: longword): longword; {$IFDEF CLR} static; {$ENDIF}

    function _mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword; const Unicode: boolean): TMYSQL_FIELD; virtual;
    function _mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW; virtual;
    function _mysql_stmt_fetch(pstmt: PMYSQL_STMT; out row: PMYSQL_ROW): integer; virtual;

    procedure _mysql_fetch_lengths(pres: PMYSQL_RES; var Lens: TLenArr); virtual;
    function _mysql_fetch_value_is_null(row: PMYSQL_ROW; fieldnr: longword): boolean; virtual;
    function _mysql_fetch_value_ptr(pres: PMYSQL_RES; row: PMYSQL_ROW; fieldnr: longword): IntPtr; virtual;
    procedure _mysql_fetch_value_to_buff(row: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer); overload; virtual;
    procedure _mysql_fetch_value_to_buff(row: PMYSQL_ROW; fieldnr: longword; pBuff: IntPtr; Len: integer); overload; virtual;
    procedure _mysql_fetch_value_to_str(row: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer); overload; virtual;
    procedure _mysql_fetch_value_to_str(row: PMYSQL_ROW; fieldnr: longword; pBuff: IntPtr; Len: integer); overload; virtual;
    function _mysql_fetch_value_arr(row: PMYSQL_ROW; fieldnr: longword; out Off: integer; Len: integer): TValueArr; virtual;
    function _mysql_fetch_value_str(row: PMYSQL_ROW; fieldnr: longword): AnsiString; virtual;

    function _mysql_stmt_execute(pstmt: PMYSQL_STMT; Params: TParamDescs; const Unicode: boolean): integer; virtual;
    function _mysql_stmt_bind_param(pstmt: PMYSQL_STMT; const bnds: TMYSQL_BINDS): my_bool; virtual;
    function _mysql_stmt_bind_result(pstmt: PMYSQL_STMT; const bnds: TMYSQL_BINDS): my_bool; virtual;
    function _mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; var bnds: TMYSQL_BINDS; column: UINT; offset: longword): integer; virtual;
    procedure _mysql_stmt_fetch_lengths(prow: PMYSQL_ROW; var Lens: TLenArr); virtual;
    function _mysql_stmt_fetch_value_ptr(pstmt: PMYSQL_STMT; fieldnr: longword): IntPtr; virtual;

    procedure SetTimeout(pmysql: PMYSQL_CON; Timeout: integer);

    property ClientStructVer: TClientStructVer read FClientStructVer;
    property ClientVer: string read FClientVer;

  end;


{$IFDEF LINUX}
  FARPROC = pointer;
{$ENDIF}

  TMySQLAPIClient = class(TMySQLAPI)
  protected
  {$IFNDEF CLR}
    //Client_mysql_num_rows: function(pres: PMYSQL_RES): my_ulonglong;                                      {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_num_fields: function(pres: PMYSQL_RES): longword;                                          {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_eof: function(pres: PMYSQL_RES): my_bool;                                                {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_fetch_field_direct: function(pres: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD;           {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_fetch_fields: function(pres: PMYSQL_RES): PMYSQL_FIELDS;                                 {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_row_tell: function(pres: PMYSQL_RES): PMYSQL_ROWS;                                       {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_field_tell: function(pres: PMYSQL_RES): longword;                                        {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}

    Client_mysql_field_count: function(pmysql: PMYSQL_CON): longword;                                          {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_affected_rows: function(pmysql: PMYSQL_CON): my_ulonglong;                                    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_insert_id: function(pmysql: PMYSQL_CON): my_ulonglong;                                        {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_errno: function(pmysql: PMYSQL_CON): longword;                                                {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_error: function(pmysql: PMYSQL_CON): PAnsiChar;                                                   {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_info: function(pmysql: PMYSQL_CON): PAnsiChar;                                                    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_thread_id: function(pmysql: PMYSQL_CON): longword;                                                {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_character_set_name: function(pmysql: PMYSQL_CON): PAnsiChar;                                      {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}

    Client_mysql_init: function(pmysql: PMYSQL_CON): PMYSQL_CON;                                                   {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
  {$IFDEF HAVE_OPENSSL}
    Client_mysql_ssl_set: function(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: PAnsiChar): longint;           {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
  {$ENDIF} // HAVE_OPENSSL
    //Client_mysql_connect: function(pmysql: PMYSQL_CON; const host, user, passwd: PChar): PMYSQL_CON;             {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_change_user: function(pmysql: PMYSQL_CON; const user, passwd, db: PChar): my_bool;          {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_real_connect: function(pmysql: PMYSQL_CON; host, user, passwd, db: PAnsiChar; port: longword; unix_socket: PAnsiChar; clientflag: longword): PMYSQL_CON; {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_close: procedure(sock: PMYSQL_CON);                                                           {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_select_db: function(pmysql: PMYSQL_CON; const db: PAnsiChar): longint;                            {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_query: function(pmysql: PMYSQL_CON; const q: PChar): longint;                               {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_send_query: function(pmysql: PMYSQL_CON; const q: PChar; length: longword): longint;        {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_read_query_result: function(pmysql: PMYSQL_CON): longint;                                   {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_real_query: function(pmysql: PMYSQL_CON; const q: PAnsiChar; length: longword): longint;          {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_create_db: function(pmysql: PMYSQL_CON; const DB: PChar): longint;                          {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_drop_db: function(pmysql: PMYSQL_CON; const DB: PChar): longint;                            {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_shutdown: function(pmysql: PMYSQL_CON): longint;                                            {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_dump_debug_info: function(pmysql: PMYSQL_CON): longint;                                     {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_refresh: function(pmysql: PMYSQL_CON; refresh_options: longword): longint;                  {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_kill: function(pmysql: PMYSQL_CON; pid: longword): longint;                                   {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_ping: function(pmysql: PMYSQL_CON): longint;                                                  {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_stat: function(pmysql: PMYSQL_CON): PChar;                                                  {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_get_server_info: function(pmysql: PMYSQL_CON): PAnsiChar;                                         {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_get_client_info: function: PAnsiChar;                                                         {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_get_host_info: function(pmysql: PMYSQL_CON): PAnsiChar;                                           {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_get_proto_info: function(pmysql: PMYSQL_CON): longword;                                     {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_list_dbs: function(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES;                      {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_list_tables: function(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES;                   {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_list_fields: function(pmysql: PMYSQL_CON; const table, wild: PChar): PMYSQL_RES;            {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_list_processes: function(pmysql: PMYSQL_CON): PMYSQL_RES;                                   {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_store_result: function(pmysql: PMYSQL_CON): PMYSQL_RES;                                     {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_use_result: function(pmysql: PMYSQL_CON): PMYSQL_RES;                                         {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_options: function(pmysql: PMYSQL_CON; option: integer; var arg: integer): longint;      {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_options2: function(pmysql: PMYSQL_CON; option: integer; const arg: PAnsiChar): longint;     {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_free_result: procedure(pres: PMYSQL_RES);                                               {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_data_seek: procedure(pres: PMYSQL_RES; offset: my_ulonglong);                         {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_row_seek: function(pres: PMYSQL_RES; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET;     {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_field_seek: function(pres: PMYSQL_RES; offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_fetch_row: function(pres: PMYSQL_RES): PMYSQL_ROW;                                      {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_fetch_lengths: function(pres: PMYSQL_RES): PMYSQL_LENGTHS;                              {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_fetch_field: function(pres: PMYSQL_RES): PMYSQL_FIELD;                                  {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_escape_string: function(_to: PChar; const from: PChar; from_length: longword): longword;{$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_real_escape_string: function(pmysql: PMYSQL_CON; _to: PChar; const from: PChar; length: longword): longword;{$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_debug: procedure(const debug: PChar);                                                   {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_odbc_escape_string: function(pmysql: PMYSQL_CON; _to: PChar; to_length: longword; const from: PChar; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): PChar;                                                             {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //myodbc_remove_escape: procedure(pmysql: PMYSQL_CON; name: PChar);                                 {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_thread_safe: function: longword;                                                        {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}

    Client_mysql_server_init: function(argc: integer; argv: PPChar; groups: PPChar): integer;              {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_server_end: procedure;                                                                    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}

  //  function mysql_reload(pmysql: PMySQL): longint;

  // C API Prepared Statements functions
    Client_mysql_stmt_init: function(pmysql: PMYSQL_CON): PMYSQL_STMT;                                     {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_prepare: function(pmysql: PMYSQL_STMT; query: PAnsiChar; length: UINT): int;            {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_execute: function(pstmt: PMYSQL_STMT): integer;                                       {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_param_count: function (pstmt: PMYSQL_STMT): UINT;                                     {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_bind_param: function(pstmt: PMYSQL_STMT;  bnd: PMYSQL_BIND): my_bool;                 {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_bind_result: function(pstmt: PMYSQL_STMT;  pbnd: PMYSQL_BIND): my_bool;                {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_field_count: function(pstmt: PMYSQL_STMT): UINT;                                      {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_close: function(pstmt: PMYSQL_STMT): my_bool;                                         {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_free_result: function(pstmt: PMYSQL_STMT): my_bool;                                         {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_errno: function(pstmt: PMYSQL_STMT): UINT;                                            {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_error: function(pstmt: PMYSQL_STMT): PAnsiChar;                                           {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_commit: function(pmysql: PMYSQL_CON): my_bool;                                            {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_rollback: function(pmysql: PMYSQL_CON): my_bool;                                          {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    //Client_mysql_autocommit: function(pmysql: PMYSQL_CON;  auto_mode: my_bool): my_bool;                   {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_fetch: function(pstmt: PMYSQL_STMT): integer;                                              {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_fetch_column: function(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND; column: uint; offset: longword): integer; {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_send_long_data: function(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool; {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_prepare_result: function(pstmt: PMYSQL_STMT): PMYSQL_RES;                             {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_result_metadata: function(pstmt: PMYSQL_STMT): PMYSQL_RES;                             {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_param_metadata: function(pstmt: PMYSQL_STMT): PMYSQL_RES;                             {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_affected_rows: function(pstmt: PMYSQL_STMT): my_ulonglong;                            {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_stmt_store_result: function(pstmt: PMYSQL_STMT): integer;                                  {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_more_results: function(pmysql: PMYSQL_CON): my_bool;                                      {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    Client_mysql_next_result: function(pmysql: PMYSQL_CON): my_bool;                                       {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
  {$ENDIF}

    FLastError: longword;
  {$IFDEF MSWINDOWS}
    hMySQLLib: HMODULE;
  {$ENDIF}
  {$IFDEF LINUX}
    hMySQLLib: pointer;
  {$ENDIF}
    class function GetLibraryName: string; virtual;
  {$IFNDEF CLR}
    class function GetNotLinkProc: FARPROC; virtual;
  {$ENDIF}

  public
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
    procedure mysql_close(sock: PMYSQL_CON); override;
    function mysql_select_db(pmysql: PMYSQL_CON; const db: PAnsiChar): longint; override;
    //function mysql_query(pmysql: PMYSQL_CON; const q: PChar): longint; override;
    //function mysql_send_query(pmysql: PMYSQL_CON; const q: PChar; length: longword): longint; override;
    //function mysql_read_query_result(pmysql: PMYSQL_CON): longint; override;
    function mysql_real_query(pmysql: PMYSQL_CON; const q: AnsiString; length: longword): longint; override;
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
    function mysql_stmt_prepare(pmysql: PMYSQL_STMT;  query: PAnsiChar;  length: UINT): int; override;
    function mysql_stmt_execute(pstmt: PMYSQL_STMT): integer; override;
    function mysql_stmt_param_count(pstmt: PMYSQL_STMT): UINT; override;
    function mysql_stmt_bind_param(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND): my_bool; override;
    function mysql_stmt_bind_result(pstmt: PMYSQL_STMT; pbnd: PMYSQL_BIND): my_bool; override;
    function mysql_stmt_field_count(pstmt: PMYSQL_STMT): UINT; override;
    function mysql_stmt_close(pstmt: PMYSQL_STMT): my_bool; override;
    function mysql_stmt_free_result(pstmt: PMYSQL_STMT): my_bool; override;
    function mysql_stmt_errno(pstmt: PMYSQL_STMT): UINT; override;
    function mysql_stmt_error(pstmt: PMYSQL_STMT): AnsiString; override;
    //function mysql_commit(pmysql: PMYSQL_CON): my_bool; override;
    //function mysql_rollback(pmysql: PMYSQL_CON): my_bool; override;
    //function mysql_autocommit(pmysql: PMYSQL_CON;  auto_mode: my_bool): my_bool; override;
    function mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer; override;
    function mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND; column: uint; offset: longword): integer; override;
    function mysql_stmt_send_long_data(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool; override;
    function mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; override;
    function mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; override;
    function mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): my_ulonglong; override;
    //function mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer; override;
    function mysql_more_results(pmysql: PMYSQL_CON): my_bool; override;
    function mysql_next_result(pmysql: PMYSQL_CON): int; override;

    function _mysql_stmt_execute(pstmt: PMYSQL_STMT; Params: TParamDescs; const Unicode: boolean): integer; override;

  {$IFDEF CLR}
    procedure _mysql_fetch_lengths(pres: PMYSQL_RES; var Lens: TLenArr); override;
  {$ENDIF}

    function LoadedMySQLLib: boolean; override;
    procedure LoadMySQLLib; override;
    procedure FreeMySQLLib; override;
    procedure CheckMySQLLib; override;
  end;

{$IFDEF EMBLOG}
  TMutex = class(TSynchroObject)
  protected
    FName: ShortString;
    FHandle: THandle;
    FTimeout: DWord;

  public
    constructor Create(const Name: ShortString; const InitialTimeout: Longword = INFINITE);
    destructor Destroy; override;

    procedure Acquire; override;
    procedure Release; override;

    property Timeout: DWord read FTimeout;
    property Name: ShortString read FName;
  end;
{$ENDIF}

{$IFDEF EMBLOG}
  TCreateFile = function(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
    lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
    hTemplateFile: THandle): THandle; stdcall;
  TWriteFile = function(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
    var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
  //TCloseHandle = function(hObject: THandle): BOOL; stdcall;
  TExitProcess = procedure(uExitCode: UINT); stdcall;
{$ENDIF}

  TMySQLAPIEmbedded = class(TMySQLAPIClient)
  protected
    FParams: TStrings;
    FCurrentParams: string;
    FServerInited: boolean;
    FClientsCount: Integer;
  {$IFDEF EMBLOG}
    FDataDirMutex: TMutex;

    FpCreateFile: TCreateFile;
    FpWriteFile: TWriteFile;
    FpExitProcess: TExitProcess;
    //addr_CloseHandle: TCloseHandle;

    FLogHandle: THandle;
    FLogErrorHandle: THandle;
    FListOnLog: TDAList;
    FListOnLogError: TDAList;
  {$ENDIF}

    procedure SetParams(Value: TStrings);
    class function GetLibraryName: string; override;
  {$IFNDEF CLR}
    class function GetNotLinkProc: FARPROC; override;
  {$ENDIF}

  {$IFDEF EMBLOG}
    function CreateFile(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
      lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
      hTemplateFile: THandle): THandle;
    function WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
      var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL;
    procedure ExitProcess(uExitCode: UINT);
  {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

  {$IFDEF CLR}
    //class function mysql_num_rows(pres: PMYSQL_RES): my_ulonglong; override;
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
    procedure mysql_close(sock: PMYSQL_CON); override;
    function mysql_select_db(pmysql: PMYSQL_CON; const db: PAnsiChar): longint; override;
    //function mysql_query(pmysql: PMYSQL_CON; const q: PChar): longint; override;
    //function mysql_send_query(pmysql: PMYSQL_CON; const q: PChar; length: longword): longint; override;
    //function mysql_read_query_result(pmysql: PMYSQL_CON): longint; override;
    function mysql_real_query(pmysql: PMYSQL_CON; const q: AnsiString; length: longword): longint; override;
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
    function mysql_stmt_prepare(pmysql: PMYSQL_STMT;  query: PAnsiChar;  length: UINT): int; override;
    function mysql_stmt_execute(pstmt: PMYSQL_STMT): integer; override;
    function mysql_stmt_param_count(pstmt: PMYSQL_STMT): UINT; override;
    function mysql_stmt_bind_param(pstmt: PMYSQL_STMT;  bnd: PMYSQL_BIND): my_bool; override;
    function mysql_stmt_bind_result(pstmt: PMYSQL_STMT;  pbnd: PMYSQL_BIND): my_bool; override;
    function mysql_stmt_field_count(pstmt: PMYSQL_STMT): UINT; override;
    function mysql_stmt_close(pstmt: PMYSQL_STMT): my_bool; override;
    function mysql_stmt_free_result(pstmt: PMYSQL_STMT): my_bool; override;
    function mysql_stmt_errno(pstmt: PMYSQL_STMT): UINT; override;
    function mysql_stmt_error(pstmt: PMYSQL_STMT): AnsiString; override;
    // function mysql_commit(pmysql: PMYSQL_CON): my_bool; override;
    // function mysql_rollback(pmysql: PMYSQL_CON): my_bool; override;
    // function mysql_autocommit(pmysql: PMYSQL_CON;  auto_mode: my_bool): my_bool; override;
    function mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer; override;
    function mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND; column: uint; offset: longword): integer; override;
    function mysql_stmt_send_long_data(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool; override;
    function mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; override;
    function mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES; override;
    function mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): my_ulonglong; override;
    // function mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer; override;
    function mysql_more_results(pmysql: PMYSQL_CON): my_bool; override;
    function mysql_next_result(pmysql: PMYSQL_CON): int; override;
    procedure _mysql_fetch_lengths(pres: PMYSQL_RES; var Lens: TLenArr); override;
  {$ENDIF}

    procedure LoadMySQLLib; override;
    procedure FreeMySQLLib; override;

    property Params: TStrings read FParams write SetParams; 

    procedure RegisterOnLogEvent(const Event: TMyLogEvent);
    procedure UnRegisterOnLogEvent(const Event: TMyLogEvent);
    procedure RegisterOnLogErrorEvent(const Event: TMyLogEvent);
    procedure UnRegisterOnLogErrorEvent(const Event: TMyLogEvent);
    procedure UnRegisterConnection;
  end;

{$IFNDEF VER6P}
  TVarType = integer;
{$ENDIF}

  function ObjToIntPtr(AValue: TIntPtrObj): IntPtr;
  function IntPtrToObj(AValue: IntPtr): TIntPtrObj;
  function ConvertInternalTypeMySQLFormat(const InternalType: word): TMySqlFieldType;
  function BCDParamDescToStr(ParamDesc: TParamDesc): string;

var
  MyAPIClient: TMySQLAPIClient;
  MyAPIEmbedded: TMySQLAPIEmbedded;
{$IFNDEF CLR}
  MySQLClientLibrary: string;
{$ENDIF}
  MySQLEmbServerInitMessage: string;
  MySQLEmbServerIniting: boolean;
  MySQLEmbDisableEventLog: boolean;

  UnloadEmbLibraryOnDisconnect: boolean = False;

implementation

uses
{$IFDEF CLR}
  System.IO,
{$ENDIF}
{$IFDEF VER6P}
  Variants, FMTBcd, DateUtils,
{$ENDIF}
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$IFNDEF FPC}WinSvc,{$ELSE}{JwaImageHlp, JwaWinNT,}{$ENDIF}
{$ENDIF}
{$IFDEF WIN32}
  {$IFNDEF FPC}TlHelp32, ImageHlp,{$ELSE}{JwaTlHelp32,}{$ENDIF} DB,
{$ENDIF}
  IniFiles, SysConst, DAConsts, MemData,
{$IFNDEF UNIDACPRO}
  MyConsts, MyClasses;
{$ELSE}
  MyConstsUni, MyClassesUni;
{$ENDIF}

{$WARNINGS OFF}
function ConvertInternalTypeMySQLFormat(const InternalType: word): TMySqlFieldType;
begin
  case InternalType of
    dtInt8, dtBoolean:
      Result := FIELD_TYPE_TINY; // TINYINT
    dtInt16:
      Result := FIELD_TYPE_SHORT; // SMALLINT
    dtUInt16, dtInt32, dtUInt32:
      Result := FIELD_TYPE_LONG; // INT
    dtInt64:
      Result := FIELD_TYPE_LONGLONG; // BIGINT

    // Float fields
    dtFloat:
      Result := FIELD_TYPE_DOUBLE; // FLOAT, DOUBLE, DECIMAL

    dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
      Result := FIELD_TYPE_VAR_STRING;

    // String fields
    dtUnknown,
    dtBlob, dtMemo, dtWideMemo,
    dtBytes, dtVarBytes, dtExtVarBytes,
    dtString, dtExtString,
    dtWideString, dtExtWideString:
      Result := FIELD_TYPE_VAR_STRING;

    // DateTime fields
    dtDateTime:
      Result := FIELD_TYPE_DATETIME; // DATETIME, TIMESTAMP14, TIMESTAMP12, TIMESTAMP8, TIMESTAMP6
    dtDate:
      Result := FIELD_TYPE_DATE; // DATE
    dtTime:
      Result := FIELD_TYPE_TIME; // TIME

    else
      Assert(False, Format('Invalid internal field type $%X (%d)', [InternalType, InternalType]));
  end;

end;
{$WARNINGS ON}

{$IFDEF CLR}

function ObjToIntPtr(AValue: TIntPtrObj): IntPtr;
begin
  if AValue = nil then
    Result := nil
  else
    Result := IntPtr(AValue);
end;

function IntPtrToObj(AValue: IntPtr): TIntPtrObj;
begin
  if AValue = nil then
    Result := nil
  else
    Result := TIntPtrObj(AValue);
end;

{$ELSE}
function ObjToIntPtr(AValue: TIntPtrObj): IntPtr;
begin
  Result := IntPtr(AValue);
end;

function IntPtrToObj(AValue: IntPtr): TIntPtrObj;
begin
  Result := TIntPtrObj(AValue);
end;

{$ENDIF}

{$IFDEF CLR}
const
  libmysql = 'libmysql.dll';

type
  PMYSQL_CON_ = IntPtr;
  PMYSQL_RES_ = IntPtr;
  PMYSQL_STMT_ = IntPtr;
  PMYSQL_FIELD_ = IntPtr;
  PMYSQL_ROW_ = IntPtr;
  PMYSQL_LENGTHS_ = IntPtr;

//mysql_num_rows: function(pres: PMYSQL_RES_): my_ulonglong;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_num_fields')]
function Client_mysql_num_fields(pres: PMYSQL_RES_): longword; external;
//mysql_eof(pres: PMYSQL_RES_): my_bool;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_fetch_field_direct')]
function Client_mysql_fetch_field_direct(pres: PMYSQL_RES_; fieldnr: longword): PMYSQL_FIELD_; external;
//mysql_fetch_fields(pres: PMYSQL_RES_): PMYSQL_FIELD_S;
//mysql_row_tell(pres: PMYSQL_RES_): PMYSQL_ROW_S;
//mysql_field_tell(pres: PMYSQL_RES_): longword;

[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_field_count')]
function Client_mysql_field_count(pmysql: PMYSQL_CON_): longword; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_affected_rows')]
function Client_mysql_affected_rows(pmysql: PMYSQL_CON_): my_ulonglong; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_insert_id')]
function Client_mysql_insert_id(pmysql: PMYSQL_CON_): my_ulonglong; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_errno')]
function Client_mysql_errno(pmysql: PMYSQL_CON_): longword; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_error')]
function Client_mysql_error(pmysql: PMYSQL_CON_): string; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_info')]
function Client_mysql_info(pmysql: PMYSQL_CON_): string; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_thread_id')]
function Client_mysql_thread_id(pmysql: PMYSQL_CON_): longword; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_character_set_name')]
function Client_mysql_character_set_name(pmysql: PMYSQL_CON_): string; external;

[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_init')]
function Client_mysql_init(pmysql: PMYSQL_CON_): PMYSQL_CON_; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_ssl_set')]
function Client_mysql_ssl_set(pmysql: PMYSQL_CON_; key, cert, ca, capath, cipher: string): longint; external;
//mysql_connect(pmysql: PMYSQL_CON_; const host, user, passwd: string): PMYSQL_CON_;
//mysql_change_user(pmysql: PMYSQL_CON_; const user, passwd, db: string): my_bool;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_real_connect')]
function Client_mysql_real_connect(pmysql: PMYSQL_CON_; host, user, passwd, db: string; port: longword; unix_socket: string; clientflag: longword): PMYSQL_CON_; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_close')]
procedure Client_mysql_close(sock: PMYSQL_CON_); external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_select_db')]
function Client_mysql_select_db(pmysql: PMYSQL_CON_; const db: string): longint; external;
//mysql_query(pmysql: PMYSQL_CON_; const q: string): longint;
//mysql_send_query(pmysql: PMYSQL_CON_; const q: string; length: longword): longint;
//mysql_read_query_result(pmysql: PMYSQL_CON_): longint;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_real_query')]
function Client_mysql_real_query(pmysql: PMYSQL_CON_; const q: string; length: longword): longint; external;
//mysql_create_db(pmysql: PMYSQL_CON_; const DB: string): longint;
//mysql_drop_db(pmysql: PMYSQL_CON_; const DB: string): longint;
//mysql_shutdown(pmysql: PMYSQL_CON_): longint;
//mysql_dump_debug_info(pmysql: PMYSQL_CON_): longint;
//mysql_refresh(pmysql: PMYSQL_CON_; refresh_options: longword): longint;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_kill')]
function Client_mysql_kill(pmysql: PMYSQL_CON_; pid: longword): longint; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_ping')]
function Client_mysql_ping(pmysql: PMYSQL_CON_): longint; external;
//mysql_stat(pmysql: PMYSQL_CON_): string;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_get_server_info')]
function Client_mysql_get_server_info(pmysql: PMYSQL_CON_): string; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_get_client_info')]
function Client_mysql_get_client_info: string; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_get_host_info')]
function Client_mysql_get_host_info(pmysql: PMYSQL_CON_): string; external;
//mysql_get_proto_info(pmysql: PMYSQL_CON_): longword;
//mysql_list_dbs(pmysql: PMYSQL_CON_; const wild: string): PMYSQL_RES_;
//mysql_list_tables(pmysql: PMYSQL_CON_; const wild: string): PMYSQL_RES_;
//mysql_list_fields(pmysql: PMYSQL_CON_; const table, wild: string): PMYSQL_RES_;
//mysql_list_processes(pmysql: PMYSQL_CON_): PMYSQL_RES_;
//mysql_store_result(pmysql: PMYSQL_CON_): PMYSQL_RES_;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_use_result')]
function Client_mysql_use_result(pmysql: PMYSQL_CON_): PMYSQL_RES_; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_options')]
function Client_mysql_options(pmysql: PMYSQL_CON_; option: integer; var arg: integer): longint; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_options')]
function Client_mysql_options2(pmysql: PMYSQL_CON_; option: integer; const arg: PAnsiChar): longint; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_free_result')]
procedure Client_mysql_free_result(pres: PMYSQL_RES_); external;
//mysql_data_seek: procedure(pres: PMYSQL_RES_; offset: my_ulonglong);
//mysql_row_seek(pres: PMYSQL_RES_; offset: MYSQL_ROW_OFFSET): MYSQL_ROW_OFFSET;
//mysql_field_seek(pres: PMYSQL_RES_; offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET;

[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_fetch_row')]
function Client_mysql_fetch_row(pres: PMYSQL_RES_): PMYSQL_ROW_; external;

[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_fetch_lengths')]
function Client_mysql_fetch_lengths(pres: PMYSQL_RES_): PMYSQL_LENGTHS_; external;
//[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_fetch_field')]
//function Client_mysql_fetch_field(pres: PMYSQL_RES_): PMYSQL_FIELD_; external;
//mysql_escape_string(_to: string; const from: string; from_length: longword): longword;
//[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_real_escape_string')]
//function Client_mysql_real_escape_string(pmysql: PMYSQL_CON_; _to: string; const from: string; length: longword): longword; external;
//mysql_debug: procedure(const debug: string);
//mysql_odbc_escape_string(pmysql: PMYSQL_CON_; _to: string; to_length: longword; const from: string; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): string;
//myodbc_remove_escape: procedure(pmysql: PMYSQL_CON_; name: string);
//mysql_thread_safe: function: longword;

[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_server_init')]
function Client_mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_server_end')]
procedure Client_mysql_server_end; external;

// C API Prepared Statements functions
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_init')]
function Client_mysql_stmt_init(pmysql: PMYSQL_CON_): PMYSQL_STMT_; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_prepare')]
function Client_mysql_stmt_prepare(pmysql: PMYSQL_CON_; query: PAnsiChar; length: UINT): int; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_execute')]
function Client_mysql_stmt_execute(pstmt: PMYSQL_STMT_): integer; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_param_count')]
function Client_mysql_stmt_param_count(pstmt: PMYSQL_STMT_): UINT; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_bind_param')]
function Client_mysql_stmt_bind_param(pstmt: PMYSQL_STMT_;  bnd: PMYSQL_BIND): my_bool; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_bind_result')]
function Client_mysql_stmt_bind_result(pstmt: PMYSQL_STMT_;  pbnd: PMYSQL_BIND): my_bool; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_field_count')]
function Client_mysql_stmt_field_count(pstmt: PMYSQL_STMT_): UINT; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_close')]
function Client_mysql_stmt_close(pstmt: PMYSQL_STMT_): my_bool; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_free_result')]
function Client_mysql_stmt_free_result(pstmt: PMYSQL_STMT_): my_bool; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_errno')]
function Client_mysql_stmt_errno(pstmt: PMYSQL_STMT_): UINT; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_error')]
function Client_mysql_stmt_error(pstmt: PMYSQL_STMT_): PAnsiChar; external;
{[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_commit')]
function Client_mysql_commit(pmysql: PMYSQL_CON_): my_bool; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_rollback')]
function Client_mysql_rollback(pmysql: PMYSQL_CON_): my_bool; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_autocommit')]
function Client_mysql_autocommit(pmysql: PMYSQL_CON_;  auto_mode: my_bool): my_bool; external;}
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_fetch')]
function Client_mysql_fetch(pstmt: PMYSQL_STMT_): integer; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_fetch_column')]
function Client_mysql_fetch_column(pstmt: PMYSQL_STMT_; bnd: PMYSQL_BIND; column: uint; offset: longword): integer; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_send_long_data')]
function Client_mysql_stmt_send_long_data(pstmt: PMYSQL_STMT_;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_result_metadata')]
function Client_mysql_stmt_prepare_result(pstmt: PMYSQL_STMT_): PMYSQL_RES_; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_param_metadata')]
function Client_mysql_stmt_param_metadata(pstmt: PMYSQL_STMT_): PMYSQL_RES_; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_result_metadata')]
function Client_mysql_stmt_result_metadata(pstmt: PMYSQL_STMT_): PMYSQL_RES_; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_affected_rows')]
function Client_mysql_stmt_affected_rows(pstmt: PMYSQL_STMT_): my_ulonglong; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_store_result')]
function Client_mysql_stmt_store_result(pstmt: PMYSQL_STMT_): integer; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_more_results')]
function Client_mysql_more_results(pmysql: PMYSQL_CON_): my_bool; external;
[DllImport(libmysql, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_next_result')]
function Client_mysql_next_result(pmysql: PMYSQL_CON_): my_bool; external;


const
  libmysqld = 'libmysqld.dll';

//mysql_num_rows: function(pres: PMYSQL_RES_): my_ulonglong;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_num_fields')]
function Emb_mysql_num_fields(pres: PMYSQL_RES_): longword; external;
//mysql_eof(pres: PMYSQL_RES_): my_bool;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_fetch_field_direct')]
function Emb_mysql_fetch_field_direct(pres: PMYSQL_RES_; fieldnr: longword): PMYSQL_FIELD_; external;
//mysql_fetch_fields(pres: PMYSQL_RES_): PMYSQL_FIELD_S;
//mysql_row_tell(pres: PMYSQL_RES_): PMYSQL_ROW_S;
//mysql_field_tell(pres: PMYSQL_RES_): longword;

[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_field_count')]
function Emb_mysql_field_count(pmysql: PMYSQL_CON_): longword; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_affected_rows')]
function Emb_mysql_affected_rows(pmysql: PMYSQL_CON_): my_ulonglong; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_insert_id')]
function Emb_mysql_insert_id(pmysql: PMYSQL_CON_): my_ulonglong; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_errno')]
function Emb_mysql_errno(pmysql: PMYSQL_CON_): longword; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_error')]
function Emb_mysql_error(pmysql: PMYSQL_CON_): string; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_info')]
function Emb_mysql_info(pmysql: PMYSQL_CON_): string; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_thread_id')]
function Emb_mysql_thread_id(pmysql: PMYSQL_CON_): longword; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_character_set_name')]
function Emb_mysql_character_set_name(pmysql: PMYSQL_CON_): string; external;

[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_init')]
function Emb_mysql_init(pmysql: PMYSQL_CON_): PMYSQL_CON_; external;
//mysql_connect(pmysql: PMYSQL_CON_; const host, user, passwd: string): PMYSQL_CON_;
//mysql_change_user(pmysql: PMYSQL_CON_; const user, passwd, db: string): my_bool;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_ssl_set')]
function Emb_mysql_ssl_set(pmysql: PMYSQL_CON_; key, cert, ca, capath, cipher: string): longint; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_real_connect')]
function Emb_mysql_real_connect(pmysql: PMYSQL_CON_; host, user, passwd, db: string; port: longword; unix_socket: string; clientflag: longword): PMYSQL_CON_; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_close')]
procedure Emb_mysql_close(sock: PMYSQL_CON_); external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_select_db')]
function Emb_mysql_select_db(pmysql: PMYSQL_CON_; const db: string): longint; external;
//mysql_query(pmysql: PMYSQL_CON_; const q: string): longint;
//mysql_send_query(pmysql: PMYSQL_CON_; const q: string; length: longword): longint;
//mysql_read_query_result(pmysql: PMYSQL_CON_): longint;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_real_query')]
function Emb_mysql_real_query(pmysql: PMYSQL_CON_; const q: string; length: longword): longint; external;
//mysql_create_db(pmysql: PMYSQL_CON_; const DB: string): longint;
//mysql_drop_db(pmysql: PMYSQL_CON_; const DB: string): longint;
//mysql_shutdown(pmysql: PMYSQL_CON_): longint;
//mysql_dump_debug_info(pmysql: PMYSQL_CON_): longint;
//mysql_refresh(pmysql: PMYSQL_CON_; refresh_options: longword): longint;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_kill')]
function Emb_mysql_kill(pmysql: PMYSQL_CON_; pid: longword): longint; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_ping')]
function Emb_mysql_ping(pmysql: PMYSQL_CON_): longint; external;
//mysql_stat(pmysql: PMYSQL_CON_): string;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_get_server_info')]
function Emb_mysql_get_server_info(pmysql: PMYSQL_CON_): string; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_get_client_info')]
function Emb_mysql_get_client_info: string; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_get_host_info')]
function Emb_mysql_get_host_info(pmysql: PMYSQL_CON_): string; external;
//mysql_get_proto_info(pmysql: PMYSQL_CON_): longword;
//mysql_list_dbs(pmysql: PMYSQL_CON_; const wild: string): PMYSQL_RES_;
//mysql_list_tables(pmysql: PMYSQL_CON_; const wild: string): PMYSQL_RES_;
//mysql_list_fields(pmysql: PMYSQL_CON_; const table, wild: string): PMYSQL_RES_;
//mysql_list_processes(pmysql: PMYSQL_CON_): PMYSQL_RES_;
//mysql_store_result(pmysql: PMYSQL_CON_): PMYSQL_RES_;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_use_result')]
function Emb_mysql_use_result(pmysql: PMYSQL_CON_): PMYSQL_RES_; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_options')]
function Emb_mysql_options(pmysql: PMYSQL_CON_; option: integer; var arg: integer): longint; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_options')]
function Emb_mysql_options2(pmysql: PMYSQL_CON_; option: integer; const arg: PAnsiChar): longint; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_free_result')]
procedure Emb_mysql_free_result(pres: PMYSQL_RES_); external;
//mysql_data_seek: procedure(pres: PMYSQL_RES_; offset: my_ulonglong);
//mysql_row_seek(pres: PMYSQL_RES_; offset: Emb_mysql_ROW_OFFSET): Emb_mysql_ROW_OFFSET;
//mysql_field_seek(pres: PMYSQL_RES_; offset: Emb_mysql_FIELD_OFFSET): Emb_mysql_FIELD_OFFSET;

[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_fetch_row')]
function Emb_mysql_fetch_row(pres: PMYSQL_RES_): PMYSQL_ROW_; external;

[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_fetch_lengths')]
function Emb_mysql_fetch_lengths(pres: PMYSQL_RES_): PMYSQL_LENGTHS_; external;
//[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_fetch_field')]
//function Emb_mysql_fetch_field(pres: PMYSQL_RES_): PMYSQL_FIELD_; external;
//mysql_escape_string(_to: string; const from: string; from_length: longword): longword;
//[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_real_escape_string')]
//function Emb_mysql_real_escape_string(pmysql: PMYSQL_CON_; _to: string; const from: string; length: longword): longword; external;
//mysql_debug: procedure(const debug: string);
//mysql_odbc_escape_string(pmysql: PMYSQL_CON_; _to: string; to_length: longword; const from: string; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): string;
//myodbc_remove_escape: procedure(pmysql: PMYSQL_CON_; name: string);
//mysql_thread_safe: function: longword;

[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_server_init')]
function Emb_mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_server_end')]
procedure Emb_mysql_server_end; external;

  // C API Prepared Statements functions
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_prepare')]
function Emb_mysql_stmt_prepare(pmysql: PMYSQL_CON_; query: PAnsiChar; length: UINT): int; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_execute')]
function Emb_mysql_stmt_execute(pstmt: PMYSQL_STMT_): integer; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_param_count')]
function Emb_mysql_stmt_param_count(pstmt: PMYSQL_STMT_): UINT; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_bind_param')]
function Emb_mysql_stmt_bind_param(pstmt: PMYSQL_STMT_;  bnd: PMYSQL_BIND): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_bind_result')]
function Emb_mysql_stmt_bind_result(pstmt: PMYSQL_STMT_; pbnd: PMYSQL_BIND): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_field_count')]
function Emb_mysql_stmt_field_count(pstmt: PMYSQL_STMT_): UINT; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_close')]
function Emb_mysql_stmt_close(pstmt: PMYSQL_STMT_): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_free_result')]
function Emb_mysql_stmt_free_result(pstmt: PMYSQL_STMT_): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_errno')]
function Emb_mysql_stmt_errno(pstmt: PMYSQL_STMT_): UINT; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_error')]
function Emb_mysql_stmt_error(pstmt: PMYSQL_STMT_): PAnsiChar; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_commit')]
function Emb_mysql_commit(pmysql: PMYSQL_CON_): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_rollback')]
function Emb_mysql_rollback(pmysql: PMYSQL_CON_): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_autocommit')]
function Emb_mysql_autocommit(pmysql: PMYSQL_CON_;  auto_mode: my_bool): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_fetch')]
function Emb_mysql_fetch(pstmt: PMYSQL_STMT_): integer; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_fetch_column')]
function Emb_mysql_fetch_column(pstmt: PMYSQL_STMT_; bnd: PMYSQL_BIND; column: uint; offset: longword): integer; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_send_long_data')]
function Emb_mysql_stmt_send_long_data(pstmt: PMYSQL_STMT_;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_result_metadata')]
function Emb_mysql_stmt_prepare_result(pstmt: PMYSQL_STMT_): PMYSQL_RES_; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_param_metadata')]
function Emb_mysql_stmt_param_metadata(pstmt: PMYSQL_STMT_): PMYSQL_RES_; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_affected_rows')]
function Emb_mysql_stmt_affected_rows(pstmt: PMYSQL_STMT_): my_ulonglong; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_stmt_store_result')]
function Emb_mysql_stmt_store_result(pstmt: PMYSQL_STMT_): integer; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_more_results')]
function Emb_mysql_more_results(pmysql: PMYSQL_CON_): my_bool; external;
[DllImport(libmysqld, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'mysql_next_result')]
function Emb_mysql_next_result(pmysql: PMYSQL_CON_): int; external;
{$ENDIF}

{$IFNDEF CLR}
function NotLinkClient: integer;
begin
  raise Exception.Create('MySQL API function is not linked. You should update MySQL client library (' + TMySQLAPIClient.GetLibraryName + ').');
end;

function NotLinkEmbedded: integer;
begin
  raise Exception.Create('MySQL API function is not linked. You should update MySQL Embedded Server library (' + TMySQLAPIEmbedded.GetLibraryName + ').');
end;

function NotLinkDirect: integer;
begin
  raise Exception.Create('MySQL API function is not implemented.');
end;
{$ENDIF}

{ TMySQLAPI }

// function TMySQLAPI.mysql_num_rows(pres: PMYSQL_RES): my_ulonglong;
//function TMySQLAPI.mysql_eof(pres: PMYSQL_RES): my_bool;
function TMySQLAPI.mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_num_fields(pres: PMYSQL_RES): longword;
begin
  raise Exception.Create(SAbstractError);
end;

//function TMySQLAPI.mysql_fetch_fields(pres: PMYSQL_RES): PMYSQL_FIELDS;
//function TMySQLAPI.mysql_row_tell(pres: PMYSQL_RES): PMYSQL_ROWS;
//function TMySQLAPI.mysql_field_tell(pres: PMYSQL_RES): longword;

function TMySQLAPI.mysql_field_count(pmysql: PMYSQL_CON): longword;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_affected_rows(pmysql: PMYSQL_CON): my_ulonglong;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_insert_id(pmysql: PMYSQL_CON): my_ulonglong;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_errno(pmysql: PMYSQL_CON): longword;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_error(pmysql: PMYSQL_CON): AnsiString;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_info(pmysql: PMYSQL_CON): AnsiString;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_thread_id(pmysql: PMYSQL_CON): longword;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_character_set_name(pmysql: PMYSQL_CON): AnsiString;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_init(pmysql: PMYSQL_CON): PMYSQL_CON;
begin
  raise Exception.Create(SAbstractError);
end;

{$IFDEF HAVE_OPENSSL}
function TMySQLAPI.mysql_ssl_set(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: PAnsiChar): longint;
begin
  raise Exception.Create(SAbstractError);
end;
{$ENDIF} // HAVE_OPENSSL
//function TMySQLAPI.mysql_connect(pmysql: PMYSQL_CON; const host, user, passwd: PChar): PMYSQL_CON;
//function TMySQLAPI.mysql_change_user(pmysql: PMYSQL_CON; const user, passwd, db: PChar): my_bool;
function TMySQLAPI.mysql_real_connect(pmysql: PMYSQL_CON; host, user, passwd, db: PAnsiChar; port: longword; unix_socket: PAnsiChar; clientflag: longword): PMYSQL_CON;
begin
  raise Exception.Create(SAbstractError);
end;

procedure TMySQLAPI.mysql_close(pmysql: PMYSQL_CON);
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_select_db(pmysql: PMYSQL_CON; const db: PAnsiChar): longint;
begin
  raise Exception.Create(SAbstractError);
end;

//function TMySQLAPI.mysql_query(pmysql: PMYSQL_CON; const q: PChar): longint;
//function TMySQLAPI.mysql_send_query(pmysql: PMYSQL_CON; const q: PChar; length: longword): longint;
//function TMySQLAPI.mysql_read_query_result(pmysql: PMYSQL_CON): longint;
function TMySQLAPI.mysql_real_query(pmysql: PMYSQL_CON; const q: AnsiString; length: longword): longint;
begin
  raise Exception.Create(SAbstractError);
end;

//function TMySQLAPI.mysql_create_db(pmysql: PMYSQL_CON; const DB: PChar): longint;
//function TMySQLAPI.mysql_drop_db(pmysql: PMYSQL_CON; const DB: PChar): longint;
//function TMySQLAPI.mysql_shutdown(pmysql: PMYSQL_CON): longint;
//function TMySQLAPI.mysql_dump_debug_info(pmysql: PMYSQL_CON): longint;
//function TMySQLAPI.mysql_refresh(pmysql: PMYSQL_CON; refresh_options: longword): longint;
function TMySQLAPI.mysql_kill(pmysql: PMYSQL_CON; pid: longword): longint;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_ping(pmysql: PMYSQL_CON): longint;
begin
  raise Exception.Create(SAbstractError);
end;

//function TMySQLAPI.mysql_stat(pmysql: PMYSQL_CON): PChar;
function TMySQLAPI.mysql_get_server_info(pmysql: PMYSQL_CON): AnsiString;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_get_client_info: AnsiString;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_get_host_info(pmysql: PMYSQL_CON): AnsiString;
begin
  raise Exception.Create(SAbstractError);
end;

//function TMySQLAPI.mysql_get_proto_info(pmysql: PMYSQL_CON): longword;
//function TMySQLAPI.mysql_list_dbs(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES;
//function TMySQLAPI.mysql_list_tables(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES;
//function TMySQLAPI.mysql_list_fields(pmysql: PMYSQL_CON; const table, wild: PChar): PMYSQL_RES;
//function TMySQLAPI.mysql_list_processes(pmysql: PMYSQL_CON): PMYSQL_RES;
//function TMySQLAPI.mysql_store_result(pmysql: PMYSQL_CON): PMYSQL_RES;
function TMySQLAPI.mysql_use_result(pmysql: PMYSQL_CON): PMYSQL_RES;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; var arg: integer): longint;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; const arg: PAnsiChar): longint;
begin
  raise Exception.Create(SAbstractError);
end;

procedure TMySQLAPI.mysql_free_result(pres: PMYSQL_RES);
begin
  raise Exception.Create(SAbstractError);
end;

//function TMySQLAPI.mysql_data_seek: procedure(pres: PMYSQL_RES; offset: my_ulonglong);
//function TMySQLAPI.mysql_row_seek(pres: PMYSQL_RES; offset: TMySQLAPI.mysql_ROW_OFFSET): TMySQLAPI.mysql_ROW_OFFSET;
//function TMySQLAPI.mysql_field_seek(pres: PMYSQL_RES; offset: TMySQLAPI.mysql_FIELD_OFFSET): TMySQLAPI.mysql_FIELD_OFFSET;
function TMySQLAPI.mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_fetch_lengths(pres: PMYSQL_RES): PMYSQL_LENGTHS;
begin
  raise Exception.Create(SAbstractError);
end;

//function TMySQLAPI.mysql_fetch_field(pres: PMYSQL_RES): PMYSQL_FIELD;
//function TMySQLAPI.mysql_escape_string(_to: PChar; const from: PChar; from_length: longword): longword;
//function TMySQLAPI.mysql_real_escape_string(pmysql: PMYSQL_CON; _to: PChar; const from: PChar; length: longword): longword;
//function TMySQLAPI.mysql_debug: procedure(const debug: PChar);
//function TMySQLAPI.mysql_odbc_escape_string(pmysql: PMYSQL_CON; _to: PChar; to_length: longword; const from: PChar; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): PChar;
//myodbc_remove_escape: procedure(pmysql: PMYSQL_CON; name: PChar);
//function TMySQLAPI.mysql_thread_safe: function: longword;

function TMySQLAPI.mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer;
begin
  raise Exception.Create(SAbstractError);
end;

procedure TMySQLAPI.mysql_server_end;
begin
  raise Exception.Create(SAbstractError);
end;

//  function function TMySQLAPI.mysql_reload(pmysql: PMySQL): longint;

// C API Prepared Statements functions
function TMySQLAPI.mysql_stmt_init(pmysql: PMYSQL_CON): PMYSQL_STMT;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_prepare(pmysql: PMYSQL_CON;  query: PAnsiChar;  length: UINT): int;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_execute(pstmt: PMYSQL_STMT): integer;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_param_count(pstmt: PMYSQL_STMT): UINT;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_bind_param(pstmt: PMYSQL_STMT;  bnd: PMYSQL_BIND): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_bind_result(pstmt: PMYSQL_STMT; pbnd: PMYSQL_BIND): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_field_count(pstmt: PMYSQL_STMT): UINT;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_close(pstmt: PMYSQL_STMT): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_free_result(pstmt: PMYSQL_STMT): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_errno(pstmt: PMYSQL_STMT): UINT;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_error(pstmt: PMYSQL_STMT): AnsiString;
begin
  raise Exception.Create(SAbstractError);
end;

{function TMySQLAPI.mysql_commit(pmysql: PMYSQL_CON): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_rollback(pmysql: PMYSQL_CON): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_autocommit(pmysql: PMYSQL_CON;  auto_mode: my_bool): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;}

function TMySQLAPI.mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND; column: uint; offset: longword): integer;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_send_long_data(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): my_ulonglong;
begin
  raise Exception.Create(SAbstractError);
end;

{function TMySQLAPI.mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer;
begin
  raise Exception.Create(SAbstractError);
end;}

function TMySQLAPI.mysql_more_results(pmysql: PMYSQL_CON): my_bool;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.mysql_next_result(pmysql: PMYSQL_CON): int;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.LoadedMySQLLib: boolean;
begin
  Result := True;
end;

procedure TMySQLAPI.LoadMySQLLib;
begin
end;

procedure TMySQLAPI.FreeMySQLLib;
begin
end;

procedure TMySQLAPI.CheckMySQLLib;
var
  Major, Minor, Release: integer;
begin
  FClientVer := string(mysql_get_client_info);

  DecodeVersion(ClientVer, Major, Minor, Release);

  case Major of
    3:
      FClientStructVer := cv3;
    4:
      if Minor = 0 then
        FClientStructVer := cv4
      else
        if Release = 0 then
          FClientStructVer := cv410
        else
          FClientStructVer := cv411;
    5:
      FClientStructVer := cv411;
  else
    raise Exception.Create(SInvalidClientVersion);
  end;
end;

class function TMySQLAPI.CorrectFieldLen(const FieldLen: longword): longword;
const
  dsMaxStringSize = 8192; { Maximum string field size - copied from db.pas}
begin
  Result := FieldLen;
{$IFDEF VER6P}
  if Result > High(Word) {maximum TFieldDesc.Length} then
    Result := High(Word);
{$ELSE}
  if Result > dsMaxStringSize then
    Result := dsMaxStringSize;
{$ENDIF}
end;

{$IFDEF CLR}
function TMySQLAPI._mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword; const Unicode: boolean): TMYSQL_FIELD;
var
  pField: PMYSQL_FIELD;
  p: IntPtr;
  Field3: TMYSQL_FIELD3;
  Field4: TMYSQL_FIELD4;
  Field410: TMYSQL_FIELD410;
  Field411: TMYSQL_FIELD411;

begin
  pField := mysql_fetch_field_direct(pres, fieldnr);
  p := ObjToIntPtr(pField);
  Assert(pField <> nil);

  case ClientStructVer of
    cv3: begin
      Field3 := TMYSQL_FIELD3(Marshal.PtrToStructure(p, TypeOf(TMYSQL_FIELD3)));

      Result.Name := Field3.Name;
      Result.OrgName := Field3.Name;
      Result.Table := Field3.Table;
      Result.OrgTable := '';
      Result.Flags := Field3.Flags;
      Result._type := Field3._type;
      Result.LengthInBytes := CorrectFieldLen(Field3.Length);
      Result.CharsetNr := 0;
      Result.Decimals := Field3.Decimals;
    end;
    cv4: begin
      Field4 := TMYSQL_FIELD4(Marshal.PtrToStructure(p, TypeOf(TMYSQL_FIELD4)));

      Result.Name := Field4.Name;
      Result.OrgName := Field4.Name;
      Result.Table := Field4.Table;
      Result.OrgTable := Marshal.PtrToStringAnsi(Field4.org_table);
      Result.Flags := Field4.Flags;
      Result._type := Field4._type;
      Result.LengthInBytes := CorrectFieldLen(Field4.Length);
      Result.CharsetNr := 0;
      Result.Decimals := Field4.Decimals;
    end;
    cv410: begin
      Field410 := TMYSQL_FIELD410(Marshal.PtrToStructure(p, TypeOf(TMYSQL_FIELD410)));

      Result.Name := Field410.Name;
      Result.OrgName := Field410.org_name;
      Result.Table := Field410.Table;
      Result.OrgTable := Field410.org_table;
      Result.Flags := Field410.Flags;
      Result._type := Field410._type;
      Result.LengthInBytes := CorrectFieldLen(Field410.Length);
      Result.CharsetNr := Field410.charsetnr;
      Result.Decimals := Field410.Decimals;
    end;
    cv411: begin
      Field411 := TMYSQL_FIELD411(Marshal.PtrToStructure(p, TypeOf(TMYSQL_FIELD411)));

      Result.Name := Field411.Name;
      Result.OrgName := Field411.org_name;
      Result.Table := Field411.Table;
      Result.OrgTable := Marshal.PtrToStringAnsi(Field411.org_table);
      Result.Flags := Field411.Flags;
      Result._type := Field411._type;
      Result.LengthInBytes := CorrectFieldLen(Field411.Length);
      Result.CharsetNr := Field411.charsetnr;
      Result.Decimals := Field411.Decimals;
    end;
  else
    raise Exception.Create(SInvalidClientVersion);
  end;
end;
{$ELSE}
function TMySQLAPI._mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword; const Unicode: boolean): TMYSQL_FIELD;
var
  pField: PMYSQL_FIELD;
begin
  pField := mysql_fetch_field_direct(pres, fieldnr);
  Assert(pField <> nil);

  case ClientStructVer of
    cv3: begin
      Result.Name := PMYSQL_FIELD3(pField).Name;
      Result.OrgName := Result.Name;
      Result.Table := PMYSQL_FIELD3(pField).Table;
      Result.OrgTable := '';
      Result.Flags := PMYSQL_FIELD3(pField).Flags;
      Result._type := PMYSQL_FIELD3(pField)._type;
      Result.LengthInBytes := CorrectFieldLen(PMYSQL_FIELD3(pField).Length);
      Result.CharsetNr := 0;
      Result.Decimals := PMYSQL_FIELD3(pField).Decimals;
    end;
    cv4: begin
      Result.Name := PMYSQL_FIELD4(pField).Name;
      Result.OrgName := Result.Name;
      Result.Table := PMYSQL_FIELD4(pField).Table;
      if Self is TMySQLAPIEmbedded then
        Result.OrgTable := '' //Wrong pointer in PMYSQL_FIELD4(pField).(org_table, db, etc ...) = $BAADF00D
      else
        Result.OrgTable := PAnsiChar(PMYSQL_FIELD4(pField).org_table);
      Result.Flags := PMYSQL_FIELD4(pField).Flags;
      Result._type := PMYSQL_FIELD4(pField)._type;
      Result.LengthInBytes := CorrectFieldLen(PMYSQL_FIELD4(pField).Length);
      Result.CharsetNr := 0;
      Result.Decimals := PMYSQL_FIELD4(pField).Decimals;
    end;
    cv410: begin
      Result.Name := PMYSQL_FIELD410(pField).Name;
      Result.OrgName := PMYSQL_FIELD410(pField).org_name;
      if Result.OrgName = '' then
        Result.OrgName := Result.Name;

      Result.Table := PMYSQL_FIELD410(pField).Table;
      Result.OrgTable := PMYSQL_FIELD410(pField).org_table;
      Result.Flags := PMYSQL_FIELD410(pField).Flags;
      Result._type := PMYSQL_FIELD410(pField)._type;
      Result.LengthInBytes := CorrectFieldLen(PMYSQL_FIELD410(pField).Length);
      Result.CharsetNr := PMYSQL_FIELD410(pField).charsetnr;
      Result.Decimals := PMYSQL_FIELD410(pField).Decimals;
    end;
    cv411: begin
      Result.Name := PMYSQL_FIELD411(pField).Name;
      Result.OrgName := PMYSQL_FIELD411(pField).org_name;
      if Result.OrgName = '' then
        Result.OrgName := Result.Name;

      Result.Table := PMYSQL_FIELD411(pField).Table;
      Result.OrgTable := PAnsiChar(PMYSQL_FIELD411(pField).org_table);
      Result.Flags := PMYSQL_FIELD411(pField).Flags;
      Result._type := PMYSQL_FIELD411(pField)._type;
      Result.LengthInBytes := CorrectFieldLen(PMYSQL_FIELD411(pField).Length);
      Result.CharsetNr := PMYSQL_FIELD411(pField).charsetnr;
      Result.Decimals := PMYSQL_FIELD411(pField).Decimals;
    end;
  else
    raise Exception.Create(SInvalidClientVersion);
  end;
end;
{$ENDIF}

function TMySQLAPI._mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW;
begin
  Result := mysql_fetch_row(pres);
end;

function TMySQLAPI._mysql_stmt_fetch(pstmt: PMYSQL_STMT; out row: PMYSQL_ROW): integer;
begin
  raise Exception.Create(SAbstractError);
end;

procedure TMySQLAPI._mysql_fetch_lengths(pres: PMYSQL_RES; var Lens: TLenArr);
var
  l, i: integer;
  p: PMYSQL_LENGTHS;
begin
  l := mysql_num_fields(pres);
  SetLength(Lens, l);
  p := mysql_fetch_lengths(pres);
  for i := 0 to l - 1 do
    Lens[i] := Marshal.ReadInt32(p, i * sizeof(integer));
end;

function TMySQLAPI._mysql_fetch_value_is_null(row: PMYSQL_ROW; fieldnr: longword): boolean;
begin
  Result := Marshal.ReadIntPtr(ObjToIntPtr(row), sizeof(IntPtr) * fieldnr) = nil;
end;

function TMySQLAPI._mysql_fetch_value_ptr(pres: PMYSQL_RES; row: PMYSQL_ROW; fieldnr: longword): IntPtr;
begin
  Result := Marshal.ReadIntPtr(ObjToIntPtr(row), sizeof(IntPtr) * fieldnr);
end;

procedure TMySQLAPI._mysql_fetch_value_to_buff(row: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer);
begin
{$IFDEF CLR}
  Marshal.Copy(Marshal.ReadIntPtr(ObjToIntPtr(row), sizeof(IntPtr) * fieldnr), Buff, Off, Len);
  Buff[Integer(Off) + Integer(Len)] := 0;
  //OFS(Copy(Buff, Off, Len));
{$ELSE}
  _mysql_fetch_value_to_buff(row, fieldnr, @Buff[Off], Len);
  //OFS(Buff + Off, Len);
{$ENDIF}
end;

procedure TMySQLAPI._mysql_fetch_value_to_buff(row: PMYSQL_ROW; fieldnr: longword; pBuff: IntPtr; Len: integer);
var
  ValuePtr: IntPtr;
begin
  ValuePtr := _mysql_fetch_value_ptr(nil, row, fieldnr);
  CopyBuffer(ValuePtr, pBuff, Len);
end;

{$IFDEF CLR}
procedure TMySQLAPI._mysql_fetch_value_to_str(row: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer);
var
  ValuePtr: IntPtr;
begin
  ValuePtr := _mysql_fetch_value_ptr(nil, row, fieldnr);
  Marshal.Copy(ValuePtr, Buff, Off, Len);
  Buff[Off + Len] := 0;
end;

{$ELSE}

procedure TMySQLAPI._mysql_fetch_value_to_str(row: PMYSQL_ROW; fieldnr: longword; Buff: TValueArr; Off: integer; Len: integer);
begin
  _mysql_fetch_value_to_str(row, fieldnr, @Buff[Off], Len);
end;
{$ENDIF}

procedure TMySQLAPI._mysql_fetch_value_to_str(row: PMYSQL_ROW; fieldnr: longword; pBuff: IntPtr; Len: integer);
var
  ValuePtr: IntPtr;
begin
  ValuePtr := _mysql_fetch_value_ptr(nil, row, fieldnr);
  StrLCopy(PAChar(pBuff), ValuePtr, Len);
end;

function TMySQLAPI._mysql_fetch_value_arr(row: PMYSQL_ROW; fieldnr: longword; out Off: integer; Len: integer): TValueArr;
begin
  Off := 0;
{$IFDEF CLR}
  SetLength(Result, Len + 1);
  Marshal.Copy(Marshal.ReadIntPtr(ObjToIntPtr(row), sizeof(IntPtr) * fieldnr), Result, 0, Len + 1);
{$ELSE}
  Result := Marshal.ReadIntPtr(row, sizeof(IntPtr) * fieldnr);
{$ENDIF}
end;

function TMySQLAPI._mysql_fetch_value_str(row: PMYSQL_ROW; fieldnr: longword): AnsiString;
begin
  Result := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(ObjToIntPtr(row), sizeof(IntPtr) * fieldnr));
end;

function TMySQLAPI._mysql_stmt_execute(pstmt: PMYSQL_STMT; Params: TParamDescs; const Unicode: boolean): integer;
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI.PinBinds(const bnds: TMYSQL_BINDS; const column: UINT = 0): IntPtr;
var
  is503: boolean;
  i, Major, Minor, Release: integer;
begin
  DecodeVersion(ClientVer, Major, Minor, Release);
  is503 := (Major >= 5) and ((Minor >= 1) or (Release >= 3));
  if not is503 then
    Fbnds := bnds
  else
  begin
    SetLength(Fbnds503, Length(bnds));
    for i := Low(bnds) to High(bnds) do begin
      Fbnds503[i].length := bnds[i].length;
      Fbnds503[i].is_null := bnds[i].is_null;
      Fbnds503[i].buffer := bnds[i].buffer;
      Fbnds503[i].buffer_type := bnds[i].buffer_type;
      Fbnds503[i].buffer_length := bnds[i].buffer_length;
      Fbnds503[i].row_ptr := bnds[i].inter_buffer;
      Fbnds503[i].offset := bnds[i].offset;
      Fbnds503[i].length_value := bnds[i].internal_length;
      Fbnds503[i].param_number := bnds[i].param_number;
      Fbnds503[i].pack_length := bnds[i].pack_length;
      Fbnds503[i].is_unsigned := bnds[i].is_unsigned;
      Fbnds503[i].long_data_used := bnds[i].long_data_used;
      Fbnds503[i].is_null_value := bnds[i].internal_is_null;
      Fbnds503[i].store_param_func := bnds[i].store_param_func;
      Fbnds503[i].fetch_result := bnds[i].fetch_result;
      Fbnds503[i].skip_result := bnds[i].skip_result;
    end;
  end;
{$IFDEF CLR}
  if bnds = nil then
    Result := nil
  else
    if not is503 then begin
      FbndsGC := GCHandle.Alloc(Fbnds, GCHandleType.Pinned);
      Result := Marshal.UnsafeAddrOfPinnedArrayElement(Fbnds, column);
    end
    else
    begin
      FbndsGC := GCHandle.Alloc(Fbnds503, GCHandleType.Pinned);
      Result := Marshal.UnsafeAddrOfPinnedArrayElement(Fbnds503, column);
    end;
{$ELSE}
  if not is503 then
    Result := @Fbnds[column]
  else
    Result := @Fbnds503[column];
{$ENDIF}
end;

procedure TMySQLAPI.UnPinBinds;
begin
{$IFDEF CLR}
  if IntPtr(FbndsGC) <> nil then
    FbndsGC.Free;
{$ENDIF}
  SetLength(Fbnds, 0);
end;

function TMySQLAPI._mysql_stmt_bind_param(pstmt: PMYSQL_STMT; const bnds: TMYSQL_BINDS): my_bool; 
var
  pbnd: IntPtr;
begin
  pbnd := PinBinds(bnds);
  try
    Result := mysql_stmt_bind_param(pstmt, pbnd);
  finally
    UnPinBinds;
  end;
end;

function TMySQLAPI._mysql_stmt_bind_result(pstmt: PMYSQL_STMT; const bnds: TMYSQL_BINDS): my_bool;
var
  pbnd: IntPtr;
begin
  pbnd := PinBinds(bnds);
  try
    Result := mysql_stmt_bind_result(pstmt, pbnd);
  finally
    UnPinBinds;
  end;
end;

function TMySQLAPI._mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; var bnds: TMYSQL_BINDS; column: UINT; offset: longword): integer;
var
  pbnd: IntPtr;
begin
  pbnd := PinBinds(bnds, column);
  try
    Result := mysql_stmt_fetch_column(pstmt, pbnd, column, offset);
  finally
    UnPinBinds;
  end;
end;

procedure TMySQLAPI._mysql_stmt_fetch_lengths(prow: PMYSQL_ROW; var Lens: TLenArr);
begin
  raise Exception.Create(SAbstractError);
end;

function TMySQLAPI._mysql_stmt_fetch_value_ptr(pstmt: PMYSQL_STMT; fieldnr: longword): IntPtr;
begin
  raise Exception.Create(SAbstractError);
end;

procedure TMySQLAPI.SetTimeout(pmysql: PMYSQL_CON; Timeout: integer);
begin
  if {$IFDEF HAVE_DIRECT} (ClientStructVer <> cvDirect) and {$ENDIF}
     (ClientStructVer <> cv410) and
     (ClientStructVer <> cv411) then
    Exit;

  if Timeout = 0 then
    Timeout := 365*24*3600; // CLIENT_NET_READ_TIMEOUT, mysql.h

  mysql_options(pmysql, MYSQL_OPT_READ_TIMEOUT, Timeout);
  mysql_options(pmysql, MYSQL_OPT_WRITE_TIMEOUT, Timeout);
end;

{ TMySQLAPIClient }

//function TMySQLAPIClient.mysql_num_rows(pres: PMYSQL_RES): my_ulonglong;
function TMySQLAPIClient.mysql_num_fields(pres: PMYSQL_RES): longword;
begin
  Result := Client_mysql_num_fields(ObjToIntPtr(pres));
end;

//function TMySQLAPIClient.mysql_eof(pres: PMYSQL_RES): my_bool;
function TMySQLAPIClient.mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD;
var
  p: IntPtr;
begin
  p := ObjToIntPtr(pres);
  p := Client_mysql_fetch_field_direct(p, fieldnr);
  Result := IntPtrToObj(p);
end;

//function TMySQLAPIClient.mysql_fetch_fields(pres: PMYSQL_RES): PMYSQL_FIELDS;
//function TMySQLAPIClient.mysql_row_tell(pres: PMYSQL_RES): PMYSQL_ROWS;
//function TMySQLAPIClient.mysql_field_tell(pres: PMYSQL_RES): longword;

function TMySQLAPIClient.mysql_field_count(pmysql: PMYSQL_CON): longword;
begin
  Result := Client_mysql_field_count(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_affected_rows(pmysql: PMYSQL_CON): my_ulonglong;
begin
  Result := Client_mysql_affected_rows(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_insert_id(pmysql: PMYSQL_CON): my_ulonglong;
begin
  Result := Client_mysql_insert_id(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_errno(pmysql: PMYSQL_CON): longword;
begin
  Result := Client_mysql_errno(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_error(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Client_mysql_error(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_info(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Client_mysql_info(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_thread_id(pmysql: PMYSQL_CON): longword;
begin
  Result := Client_mysql_thread_id(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_character_set_name(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Client_mysql_character_set_name(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_init(pmysql: PMYSQL_CON): PMYSQL_CON;
begin
  Result := IntPtrToObj(Client_mysql_init(ObjToIntPtr(pmysql)));
end;

{$IFDEF HAVE_OPENSSL}
function TMySQLAPIClient.mysql_ssl_set(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: PAnsiChar): longint;
begin
  Result := Client_mysql_ssl_set(ObjToIntPtr(pmysql), key, cert, ca, capath, cipher);
end;

{$ENDIF} // HAVE_OPENSSL
//function TMySQLAPIClient.mysql_connect(pmysql: PMYSQL_CON; const host, user, passwd: PChar): PMYSQL_CON;
//function TMySQLAPIClient.mysql_change_user(pmysql: PMYSQL_CON; const user, passwd, db: PChar): my_bool;
function TMySQLAPIClient.mysql_real_connect(pmysql: PMYSQL_CON; host, user, passwd, db: PAnsiChar; port: longword; unix_socket: PAnsiChar; clientflag: longword): PMYSQL_CON;
begin
  Result := IntPtrToObj(Client_mysql_real_connect(ObjToIntPtr(pmysql), host, user, passwd, db, port, unix_socket, clientflag));
end;

procedure TMySQLAPIClient.mysql_close(sock: PMYSQL_CON);
begin
  Client_mysql_close(ObjToIntPtr(sock));
end;

function TMySQLAPIClient.mysql_select_db(pmysql: PMYSQL_CON; const db: PAnsiChar): longint;
begin
  Result := Client_mysql_select_db(ObjToIntPtr(pmysql), db);
end;

//function TMySQLAPIClient.mysql_query(pmysql: PMYSQL_CON; const q: PChar): longint;
//function TMySQLAPIClient.mysql_send_query(pmysql: PMYSQL_CON; const q: PChar; length: longword): longint;
//function TMySQLAPIClient.mysql_read_query_result(pmysql: PMYSQL_CON): longint;
function TMySQLAPIClient.mysql_real_query(pmysql: PMYSQL_CON; const q: AnsiString; length: longword): longint;
begin
  Result := Client_mysql_real_query(ObjToIntPtr(pmysql), PAnsiChar(q), length);
end;

//function TMySQLAPIClient.mysql_create_db(pmysql: PMYSQL_CON; const DB: PChar): longint;
//function TMySQLAPIClient.mysql_drop_db(pmysql: PMYSQL_CON; const DB: PChar): longint;
//function TMySQLAPIClient.mysql_shutdown(pmysql: PMYSQL_CON): longint;
//function TMySQLAPIClient.mysql_dump_debug_info(pmysql: PMYSQL_CON): longint;
//function TMySQLAPIClient.mysql_refresh(pmysql: PMYSQL_CON; refresh_options: longword): longint;
function TMySQLAPIClient.mysql_kill(pmysql: PMYSQL_CON; pid: longword): longint;
begin
  Result := Client_mysql_kill(ObjToIntPtr(pmysql), pid);
end;

function TMySQLAPIClient.mysql_ping(pmysql: PMYSQL_CON): longint;
begin
  Result := Client_mysql_ping(ObjToIntPtr(pmysql));
end;

//function TMySQLAPIClient.mysql_stat(pmysql: PMYSQL_CON): PChar;
function TMySQLAPIClient.mysql_get_server_info(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Client_mysql_get_server_info(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_get_client_info: AnsiString;
begin
  Result := Client_mysql_get_client_info;
end;

function TMySQLAPIClient.mysql_get_host_info(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Client_mysql_get_host_info(ObjToIntPtr(pmysql));
end;

//function TMySQLAPIClient.mysql_get_proto_info(pmysql: PMYSQL_CON): longword;
//function TMySQLAPIClient.mysql_list_dbs(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES;
//function TMySQLAPIClient.mysql_list_tables(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES;
//function TMySQLAPIClient.mysql_list_fields(pmysql: PMYSQL_CON; const table, wild: PChar): PMYSQL_RES;
//function TMySQLAPIClient.mysql_list_processes(pmysql: PMYSQL_CON): PMYSQL_RES;
//function TMySQLAPIClient.mysql_store_result(pmysql: PMYSQL_CON): PMYSQL_RES;
function TMySQLAPIClient.mysql_use_result(pmysql: PMYSQL_CON): PMYSQL_RES;
begin
  Result := IntPtrToObj(Client_mysql_use_result(ObjToIntPtr(pmysql)));
end;

function TMySQLAPIClient.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; var arg: integer): longint;
begin
  Result := Client_mysql_options(ObjToIntPtr(pmysql), Integer(option), arg);
end;

function TMySQLAPIClient.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; const arg: PAnsiChar): longint;
begin
  Result := Client_mysql_options2(ObjToIntPtr(pmysql), Integer(option), arg);
end;

procedure TMySQLAPIClient.mysql_free_result(pres: PMYSQL_RES);
begin
  Client_mysql_free_result(ObjToIntPtr(pres));
end;

//function TMySQLAPIClient.mysql_data_seek: procedure(pres: PMYSQL_RES; offset: my_ulonglong);
//function TMySQLAPIClient.mysql_row_seek(pres: PMYSQL_RES; offset: TMySQLAPIClient.mysql_ROW_OFFSET): TMySQLAPIClient.mysql_ROW_OFFSET;
//function TMySQLAPIClient.mysql_field_seek(pres: PMYSQL_RES; offset: TMySQLAPIClient.mysql_FIELD_OFFSET): TMySQLAPIClient.mysql_FIELD_OFFSET;
function TMySQLAPIClient.mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW;
begin
  Result := IntPtrToObj(Client_mysql_fetch_row(ObjToIntPtr(pres)));
end;

function TMySQLAPIClient.mysql_fetch_lengths(pres: PMYSQL_RES): PMYSQL_LENGTHS;
begin
  Result := IntPtrToObj(Client_mysql_fetch_lengths(ObjToIntPtr(pres)));
end;

//function TMySQLAPIClient.mysql_fetch_field(pres: PMYSQL_RES): PMYSQL_FIELD;
//function TMySQLAPIClient.mysql_escape_string(_to: PChar; const from: PChar; from_length: longword): longword;
//function TMySQLAPIClient.mysql_real_escape_string(pmysql: PMYSQL_CON; _to: PChar; const from: PChar; length: longword): longword;
//function TMySQLAPIClient.mysql_debug: procedure(const debug: PChar);
//function TMySQLAPIClient.mysql_odbc_escape_string(pmysql: PMYSQL_CON; _to: PChar; to_length: longword; const from: PChar; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): PChar;
//myodbc_remove_escape: procedure(pmysql: PMYSQL_CON; name: PChar);
//function TMySQLAPIClient.mysql_thread_safe: function: longword;

function TMySQLAPIClient.mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer;
begin
  Result := Client_mysql_server_init(argc, argv, groups);
end;

procedure TMySQLAPIClient.mysql_server_end;
begin
  Client_mysql_server_end;
end;

//  function function TMySQLAPIClient.mysql_reload(pmysql: PMySQL): longint;

// C API Prepared Statements functions
function TMySQLAPIClient.mysql_stmt_init(pmysql: PMYSQL_CON): PMYSQL_STMT;
begin
  Result := Client_mysql_stmt_init(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_stmt_prepare(pmysql: PMYSQL_CON;  query: PAnsiChar;  length: UINT): int;
begin
  Result := Client_mysql_stmt_prepare(ObjToIntPtr(pmysql), query, length);
end;

function TMySQLAPIClient.mysql_stmt_execute(pstmt: PMYSQL_STMT): integer;
begin
  Result := Client_mysql_stmt_execute(ObjToIntPtr(pstmt));
end;

function TMySQLAPIClient.mysql_stmt_param_count(pstmt: PMYSQL_STMT): UINT;
begin
  Result := Client_mysql_stmt_param_count(ObjToIntPtr(pstmt));
end;

function TMySQLAPIClient.mysql_stmt_bind_param(pstmt: PMYSQL_STMT;  bnd: PMYSQL_BIND): my_bool;
begin
  Result := Client_mysql_stmt_bind_param(ObjToIntPtr(pstmt), bnd);
end;

function TMySQLAPIClient.mysql_stmt_bind_result(pstmt: PMYSQL_STMT; pbnd: PMYSQL_BIND): my_bool;
begin
  Result := Client_mysql_stmt_bind_result(ObjToIntPtr(pstmt), pbnd);
end;

function TMySQLAPIClient.mysql_stmt_field_count(pstmt: PMYSQL_STMT): UINT;
begin
  Result := Client_mysql_stmt_field_count(ObjToIntPtr(pstmt));
end;

function TMySQLAPIClient.mysql_stmt_close(pstmt: PMYSQL_STMT): my_bool;
begin
  Result := Client_mysql_stmt_close(ObjToIntPtr(pstmt));
end;

function TMySQLAPIClient.mysql_stmt_free_result(pstmt: PMYSQL_STMT): my_bool;
begin
  Result := Client_mysql_stmt_free_result(ObjToIntPtr(pstmt));
end;

function TMySQLAPIClient.mysql_stmt_errno(pstmt: PMYSQL_STMT): UINT;
begin
  Result := Client_mysql_stmt_errno(ObjToIntPtr(pstmt));
end;

function TMySQLAPIClient.mysql_stmt_error(pstmt: PMYSQL_STMT): AnsiString;
begin
  Result := Client_mysql_stmt_error(ObjToIntPtr(pstmt));
end;

{function TMySQLAPIClient.mysql_commit(pmysql: PMYSQL_CON): my_bool;
begin
  Result := Client_mysql_commit(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_rollback(pmysql: PMYSQL_CON): my_bool;
begin
  Result := Client_mysql_rollback(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_autocommit(pmysql: PMYSQL_CON;  auto_mode: my_bool): my_bool;
begin
  Result := Client_mysql_autocommit(ObjToIntPtr(pmysql), auto_mode);
end;}

function TMySQLAPIClient.mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer;
begin
  Result := Client_mysql_fetch(ObjToIntPtr(pstmt));
end;

function TMySQLAPIClient.mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND; column: uint; offset: longword): integer;
begin
  Result := Client_mysql_fetch_column(ObjToIntPtr(pstmt), bnd, column, offset);
end;

function TMySQLAPIClient.mysql_stmt_send_long_data(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool;
begin
  Result := Client_mysql_stmt_send_long_data(ObjToIntPtr(pstmt), param_number, data, length);
end;

function TMySQLAPIClient.mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  Result := IntPtrToObj(Client_mysql_stmt_result_metadata(ObjToIntPtr(pstmt)));
end;

function TMySQLAPIClient.mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  Result := IntPtrToObj(Client_mysql_stmt_param_metadata(ObjToIntPtr(pstmt)));
end;

function TMySQLAPIClient.mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): my_ulonglong;
begin
  Result := Client_mysql_stmt_affected_rows(ObjToIntPtr(pstmt));
end;

{function TMySQLAPIClient.mysql_stmt_store_result(pstmt: PMYSQL_STMT): integer;
begin
  Result := Client_mysql_stmt_store_result(ObjToIntPtr(pstmt));
end;}

function TMySQLAPIClient.mysql_more_results(pmysql: PMYSQL_CON): my_bool;
begin
  Result := Client_mysql_more_results(ObjToIntPtr(pmysql));
end;

function TMySQLAPIClient.mysql_next_result(pmysql: PMYSQL_CON): int;
begin
  Result := Client_mysql_next_result(ObjToIntPtr(pmysql));
end;

{$IFNDEF CLR}
type
  _TParamDesc = class (TParamDesc);
{$ENDIF}

function BCDParamDescToStr(ParamDesc: TParamDesc): string;
var
  c: currency;
  l: integer;
begin
  if ParamDesc.GetDataType = dtBCD then begin
    c := ParamDesc.Value;
    Result := CurrToStr(c);
  end
  else
    Result := ParamDesc.Value;
  if DecimalSeparator <> '.' then begin
    l := Pos(DecimalSeparator, Result);
    if l <> 0 then
      Result[l] := '.';
  end;
end;


function TMySQLAPIClient._mysql_stmt_execute(pstmt: PMYSQL_STMT; Params: TParamDescs; const Unicode: boolean): integer;
const
  BytesByRef = [dtBlob, dtBytes, dtVarBytes];
  CharsByRef = [dtMemo, dtWideMemo, dtString, dtWideString];
var
  ParamBlock: IntPtr;
  ParamBlockSize: UINT;
{$IFDEF CLR}
  ParamsGC: array of IntPtr; // List of AllocGCHandle(ParamDesc.Value, True)
  ParamsStr: array of IntPtr;
  UTF8Buff: TBytes;
{$ELSE}
  ParamsStr: array of AnsiString;
{$ENDIF}

  procedure FillBindingForParam(ParamDesc: TParamDesc; var pBnd: MYSQL_BIND{$IFDEF CLR}; is_null: IntPtr{$ENDIF});
  var
    InternalType: word;
    ParamVarType: TVarType;
  begin
    pBnd.length := nil;
  {$IFDEF CLR}
    pBnd.is_null := is_null;
    if ParamDesc.GetNull then
      Marshal.WriteByte(pBnd.is_null, 1)
    else
      Marshal.WriteByte(pBnd.is_null, 0);
  {$ELSE}
    pBnd.is_null := @_TParamDesc(ParamDesc).FIsNull;
  {$ENDIF}

    InternalType := ParamDesc.GetDataType;
    pBnd.buffer_type := ConvertInternalTypeMySQLFormat(InternalType);
    pBnd.buffer := IntPtr(ParamBlockSize);

    case InternalType of
      dtInt8, dtBoolean: begin
        pBnd.buffer_length := sizeof(byte);
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;
      dtInt16: begin
        pBnd.buffer_length := sizeof(smallint);
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;
      dtUInt16: begin
        pBnd.buffer_length := sizeof(integer);
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;
      dtInt32: begin
        pBnd.buffer_length := sizeof(integer);
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;
      dtUInt32: begin
        pBnd.buffer_length := sizeof(longword);
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;
      dtInt64: begin
        pBnd.buffer_length := sizeof(Int64);
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;

      // Float fields
      dtFloat: begin
        pBnd.buffer_length := sizeof(double);
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;

      // String fields
      dtUnknown, dtBlob: begin
        pBnd.buffer_length := 0; // Must be setted on SaveParamValue
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;
      dtBytes, dtVarBytes,
      dtString, dtWideString, dtMemo, dtWideMemo: begin
        ParamVarType := VarType(ParamDesc.Value);
        if not Unicode
          or (ParamVarType = varArray + varByte)
        {$IFDEF CLR}
          or (ParamDesc.Value is TBlob)
        {$ELSE}
          or (ParamVarType = varByRef)
        {$ENDIF}
        then
        begin
          pBnd.buffer_length := 0; // Must be setted on SaveParamValue
          Inc(ParamBlockSize, pBnd.buffer_length);
        end
        else
        begin
          pBnd.buffer_length := Length(ParamDesc.Value){in chars} * MaxUTF8CharLen;
          Inc(ParamBlockSize, pBnd.buffer_length);
        end;
      end;

      // DateTime fields
      dtDate, dtTime, dtDateTime: begin
        pBnd.buffer_length := sizeof(MYSQL_TIME);
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;

      dtBCD: begin
        pBnd.buffer_length := sizeof(UInt) + 22{Length(MaxCurrency) = Length('-922337203685477.5808')};
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;

    {$IFDEF VER6P}
    {$IFNDEF FPC}
      dtFMTBCD: begin
        pBnd.buffer_length := sizeof(UInt) + MaxFMTBcdFractionSize + 10;
        Inc(ParamBlockSize, pBnd.buffer_length);
      end;
    {$ENDIF}
    {$ENDIF}

      else
        Assert(False, Format('Invalid internal field type $%X (%d)', [InternalType, InternalType]));
    end;
  end;

  procedure SaveParamValue(ParamDesc: TParamDesc; var pBnd: MYSQL_BIND);
  var
    InternalType: word;
    pValue: IntPtr;
  {$IFDEF VER6P}
    i64: Int64;
  {$ENDIF}
    ParamVarType: TVarType;
    ParamVarPtr: IntPtr;
    Len: UINT;
    Blob: TBlob;
  {$IFDEF CLR}
    mdt: MYSQL_TIME;
  {$ELSE}
    mdt: PMYSQL_TIME;
  {$ENDIF}
    s: AnsiString;
    ws: WideString;

  begin
    InternalType := ParamDesc.GetDataType;
    pBnd.buffer := PtrOffset(ParamBlock, NativeInt(pBnd.buffer));

    if ParamDesc.GetNull then
      Exit;

    ParamVarType := VarType(ParamDesc.Value);

    if Unicode and (pBnd.buffer_length > 0) and (ParamDesc.GetDataType in [dtString, dtWideString, dtMemo, dtWideMemo]) then begin
      ws := ParamDesc.Value;
    {$IFDEF CLR}
      Len := Length(ws) * MaxUTF8CharLen + 1;
      if Integer(Len) > Length(UTF8Buff) then
        SetLength(UTF8Buff, Len);
      Len := Encoding.UTF8.GetBytes(ws, 0, Length(ws), UTF8Buff, 0);
      UTF8Buff[Len] := 0;
      Len := Len + 1{#0};
      Marshal.Copy(UTF8Buff, 0, pBnd.buffer, Len);
    {$ELSE}
      Len := CLRClasses.UnicodeToUtf8(pBnd.buffer, pBnd.buffer_length, PWideChar(ws), Length(ws));
    {$ENDIF}
      pBnd.buffer_length := Len;
    end
    else
    if ParamDesc.GetDataType in CharsByRef + BytesByRef then begin
      // Optimization for input-only parameters, store by ref
      if ParamVarType = varArray + varByte then begin
      {$IFDEF CLR}
        Len := Length(ParamsGC);
        SetLength(ParamsGC, Len + 1);
        ParamsGC[Len] := AllocGCHandle(ParamDesc.Value, True);
        ParamVarPtr := GetAddrOfPinnedObject(ParamsGC[Len]);
        Len := VarArrayHighBound(ParamDesc.Value, 1) - VarArrayLowBound(ParamDesc.Value, 1) + 1;
      {$ELSE}
        ParamVarPtr := TVarData(ParamDesc.Value).VArray.Data;
        Len := TVarData(ParamDesc.Value).VArray.Bounds[0].ElementCount;
      {$ENDIF}
        pBnd.buffer := ParamVarPtr;
        pBnd.buffer_length := Len;
      end
      else
    {$IFDEF CLR}
      if ParamDesc.Value is TBlob then begin
        Assert(ParamDesc.Value <> nil);
        Blob := TBlob(ParamDesc.Value);
    {$ELSE}
      if ParamVarType = varByRef then begin
        Assert(TVarData(ParamDesc.Value).VPointer <> nil);
        // Assert(TObject(TVarData(ParamDesc.Value).VPointer) is TBlob); - trial
        Blob := TVarData(ParamDesc.Value).VPointer;
    {$ENDIF}
        Blob.Defrag;

        pBnd.buffer_length := Blob.Size;
        if IntPtr(Blob.FirstPiece) = nil then
          pBnd.buffer := nil
        else begin
        {$IFDEF HAVE_COMPRESS}
          if Blob is TCompressedBlob then
            TCompressedBlob(Blob).Compressed := False; // may be non-optimal
        {$ENDIF}
          pBnd.buffer := PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader));
        end;
      end
      else
      begin // CharsByRef Input parameter
      {$IFDEF CLR}
        Len := Length(ParamsStr);
        SetLength(ParamsStr, Len + 1);
        ParamsStr[Len] := Marshal.StringToHGlobalAnsi(ParamDesc.Value);
        pBnd.buffer := ParamsStr[Len];
        Len := Length(ParamDesc.Value);
      {$ELSE}
        ParamVarPtr := TVarData(ParamDesc.Value).VPointer;
        pBnd.buffer := ParamVarPtr;
        if (ParamDesc.GetDataType in CharsByRef) then
          if ParamVarPtr <> nil then begin
            if (ParamVarType = varOleStr) {$IFDEF VER12P}or (ParamVarType = varUString){$ENDIF} then begin // WideString
              Len := Length(ParamsStr);
              SetLength(ParamsStr, Len + 1);
              ParamsStr[Len] := AnsiString(ParamDesc.Value);
              pBnd.buffer := PAnsiChar(ParamsStr[Len]);
              Len := Length(ParamDesc.Value);
            end
            else // Pascal string
              Len := StrLen(PAChar(ParamVarPtr))
          end
          else
            Len := 0
        else
          Len := Integer(ParamDesc.GetSize);
      {$ENDIF}
        pBnd.buffer_length := Len;
      end;
    end
    else
    begin
      pValue := pBnd.buffer;
      case InternalType of
        dtBoolean:
          Marshal.WriteByte(pValue, Byte(WordBool(Boolean(ParamDesc.Value)))); // Convert to boolean is useful to bypass Delphi bug
        dtInt8:
          Marshal.WriteByte(pValue, Byte(ParamDesc.Value));
        dtInt16:
          Marshal.WriteInt16(pValue, SmallInt(ParamDesc.Value));
        dtUInt16:
          Marshal.WriteInt32(pValue, Integer(Word(ParamDesc.Value)));
        dtInt32:
          Marshal.WriteInt32(pValue, Integer(ParamDesc.Value));
        dtUInt32:
          Marshal.WriteInt32(pValue, Integer(LongWord(ParamDesc.Value)));
        dtInt64: begin
        {$IFDEF VER6P}
          i64 := ParamDesc.Value; // Explicit Convert!
          Marshal.WriteInt64(pValue, i64);
        {$ELSE}
          if ParamVarType in [$000E, $0014] then
            PInt64(pValue)^ := PInt64(@TVarData(ParamDesc.Value).VInteger)^
          else
            PInt64(pValue)^ := TVarData(ParamDesc.Value).VInteger;
        {$ENDIF}
        end;

        // Float fields
        dtFloat:
          Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(Double(ParamDesc.Value)));

        // DateTime fields
        dtDate, dtTime, dtDateTime: begin
        {$IFDEF CLR}
          mdt := DateTimeToMYSQL_TIME(ParamDesc.Value);
          Marshal.StructureToPtr(TObject(mdt), pValue, False);
        {$ELSE}
          mdt := pValue;
          mdt^ := DateTimeToMYSQL_TIME(ParamDesc.Value);
        {$ENDIF}
        end;

        dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}: begin
          s := AnsiString(BCDParamDescToStr(ParamDesc));
          Len := Length(s);
          CopyBufferAnsi(s, pValue, Len + 1);
          pBnd.buffer_length := Len;
        end
        else
          Assert(False, Format('Invalid internal field type $%X (%d)', [InternalType, InternalType]));
      end;
    end;
  end;

var
  bnd: TMYSQL_BINDS;
  pbnd: IntPtr;
{$IFDEF CLR}
  bndGC: GCHandle;
  is_null: IntPtr;
{$ENDIF}
  i: integer;
  ParamCnt: integer;
  ParamOffset: integer;

begin
  if (Params.Count > 0) and (Params[0].GetParamType = pdResult) then
    ParamOffset := 1
  else
    ParamOffset := 0;
  Assert(mysql_stmt_param_count(pstmt) = longword(Params.Count - ParamOffset));
  
  ParamCnt := integer(mysql_stmt_param_count(pstmt));
  ParamBlock := nil;
{$IFDEF CLR}
  is_null := nil;
{$ENDIF}
  try
    if ParamCnt <> 0 then begin
      SetLength(bnd, ParamCnt);
    {$IFDEF CLR}
      is_null := Marshal.AllocHGlobal(ParamCnt * SizeOf(my_bool));
      bndGC := GCHandle.Alloc(bnd, GCHandleType.Pinned);
      pbnd := Marshal.UnsafeAddrOfPinnedArrayElement(bnd, 0);
    {$ELSE}
      pbnd := @bnd[0];
    {$ENDIF}
      FillChar(pbnd, ParamCnt * SizeOf(bnd[0]), 0);

      ParamBlockSize := 0;
      for i := 0 to ParamCnt - 1 do
        FillBindingForParam(Params[i + ParamOffset], bnd[i]{$IFDEF CLR}, PtrOffset(is_null, i * SizeOf(my_bool)){$ENDIF});

      ParamBlock := Marshal.AllocHGlobal(ParamBlockSize);
      FillChar(ParamBlock, ParamBlockSize, 0);
      for i := 0 to ParamCnt - 1 do
        SaveParamValue(Params[i + ParamOffset], bnd[i]);

      Result := _mysql_stmt_bind_param(pstmt, bnd);
      if Result <> 0 then
        Exit;
    end;
    Result := mysql_stmt_execute(pstmt);
    if Result <> 0 then
      Exit;
  finally
    Marshal.FreeHGlobal(ParamBlock);
  {$IFDEF CLR}
    if IntPtr(bndGC) <> nil then
      bndGC.Free;

    Marshal.FreeHGlobal(is_null);
    for i := Low(ParamsGC) to High(ParamsGC) do
      FreeGCHandle(ParamsGC[i]);
    for i := Low(ParamsStr) to High(ParamsStr) do
      Marshal.FreeCoTaskMem(ParamsStr[i]);
  {$ENDIF}
  end;
end;

{$IFDEF CLR}
procedure TMySQLAPIClient._mysql_fetch_lengths(pres: PMYSQL_RES; var Lens: TLenArr); 
var
  p: IntPtr;
  FieldCount: integer;
begin
  p := ObjToIntPtr(pres);
  FieldCount := Client_mysql_num_fields(p);
  p := Client_mysql_fetch_lengths(p);
  SetLength(Lens, FieldCount);

  Marshal.Copy(p, Lens, 0, FieldCount);
end;
{$ENDIF}

function TMySQLAPIClient.LoadedMySQLLib: boolean;
begin
{$IFDEF MSWINDOWS}
  Result := hMySQLLib > 0;
{$ENDIF}
{$IFDEF LINUX}
  Result := hMySQLLib <> nil;
{$ENDIF}
end;

const
// See file:///C:/mysql/Docs/manual_toc.html#Option_files
{$IFDEF MSWINDOWS}
  CnfFiles: array[0..1] of ShortString = (
    'my.ini',
    'C:\my.cnf');
{$ENDIF}
{$IFDEF LINUX}
  CnfFiles: array[0..3] of ShortString = (
    '/etc/my.cnf',
    '/usr/local/mysql/data/my.cnf',
    '/usr/local/var/my.cnf',
    '~/.my.cnf');
{$ENDIF}

{$IFNDEF VER6P}
function DirectoryExists(const Directory: string): Boolean;
{$IFDEF LINUX}
var
  st: TStatBuf;
begin
  if stat(PChar(Directory), st) = 0 then
    Result := ((st.st_mode and __S_IFDIR) = __S_IFDIR)
  else
    Result := False;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Code: Integer;
begin
{$IFDEF CLR}
  Code := GetFileAttributes(Directory);
{$ELSE}
  Code := GetFileAttributes(PChar(Directory));
{$ENDIF}
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ENDIF}
{$ENDIF}

class function TMySQLAPIClient.GetLibraryName: string;
begin
{$IFNDEF CLR}
  if MySQLClientLibrary <> '' then begin
    Result := MySQLClientLibrary;
    Exit;
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
  Result := 'libmysql.dll';
{$ENDIF}
{$IFDEF LINUX}
  Result := 'libmysqlclient.so';
{$ENDIF}
end;

{$IFNDEF CLR}
class function TMySQLAPIClient.GetNotLinkProc: FARPROC;
begin
  Result := @NotLinkClient;
end;
{$ENDIF}

procedure TMySQLAPIClient.LoadMySQLLib;
{$IFNDEF CLR}
  procedure AssignProc(var Proc: pointer; const Name: string);
  begin
  {$IFDEF MSWINDOWS}
    Proc := GetProcAddress(hMySQLLib, PChar(Name));
  {$ELSE}
    Proc := dlsym(hMySQLLib, PChar(Name));
  {$ENDIF}
    if Proc = nil then
      Proc := GetNotLinkProc;
  end;
{$ENDIF}
var
{$IFDEF LINUX}
  i: integer;
{$ENDIF}
  LibName: string;

begin
  if LoadedMySQLLib then
    Exit;

  LibName := GetLibraryName;

{$IFDEF CLR}
  hMySQLLib := LoadLibrary(LibName);
{$ELSE}
{$IFDEF MSWINDOWS}
  hMySQLLib := LoadLibrary(PChar(LibName));
  FLastError := GetLastError;
{$ENDIF}
{$IFDEF LINUX}
  hMySQLLib := dlopen(PChar(LibName), RTLD_LAZY);

  i := 17;
  while not LoadedMySQLLib and (i >= 7) do begin
    hMySQLLib := dlopen(PChar(LibName + '.' + IntToStr(i)), RTLD_LAZY);
    Dec(i);
  end;
{$ENDIF}
{$ENDIF}

  if not LoadedMySQLLib then
    Exit;

{$IFNDEF CLR}
  //AssignProc(@dll_mysql_num_rows, 'mysql_num_rows');
  AssignProc(@Client_mysql_num_fields, 'mysql_num_fields');
  //AssignProc(@Client_mysql_eof, 'mysql_eof');
  AssignProc(@Client_mysql_fetch_field_direct, 'mysql_fetch_field_direct');
  //AssignProc(@Client_mysql_fetch_fields, 'mysql_fetch_fields');
  //AssignProc(@Client_mysql_row_tell, 'mysql_row_tell');
  //AssignProc(@Client_mysql_field_tell, 'mysql_field_tell');
  AssignProc(@Client_mysql_field_count, 'mysql_field_count');
  AssignProc(@Client_mysql_affected_rows, 'mysql_affected_rows');
  AssignProc(@Client_mysql_insert_id, 'mysql_insert_id');
  AssignProc(@Client_mysql_errno, 'mysql_errno');
  AssignProc(@Client_mysql_error, 'mysql_error');
  AssignProc(@Client_mysql_info, 'mysql_info');
  AssignProc(@Client_mysql_thread_id, 'mysql_thread_id');
  AssignProc(@Client_mysql_character_set_name, 'mysql_character_set_name');
  AssignProc(@Client_mysql_init, 'mysql_init');
{$IFDEF HAVE_OPENSSL}
  AssignProc(@Client_mysql_ssl_set, 'mysql_ssl_set');
{$ENDIF} // HAVE_OPENSSL
  //AssignProc(@Client_mysql_connect, 'mysql_connect');
  //AssignProc(@Client_mysql_change_user, 'mysql_change_user');
  AssignProc(@Client_mysql_real_connect, 'mysql_real_connect');
  AssignProc(@Client_mysql_close, 'mysql_close');
  AssignProc(@Client_mysql_select_db, 'mysql_select_db');
  //AssignProc(@Client_mysql_query, 'mysql_query');
  //AssignProc(@Client_mysql_send_query, 'mysql_send_query');
  //AssignProc(@Client_mysql_read_query_result, 'mysql_read_query_result');
  AssignProc(@Client_mysql_real_query, 'mysql_real_query');
  //AssignProc(@Client_mysql_create_db, 'mysql_create_db');
  //AssignProc(@Client_mysql_drop_db, 'mysql_drop_db');
  //AssignProc(@Client_mysql_shutdown, 'mysql_shutdown');
  //AssignProc(@Client_mysql_dump_debug_info, 'mysql_dump_debug_info');
  //AssignProc(@Client_mysql_refresh, 'mysql_refresh');
  AssignProc(@Client_mysql_kill, 'mysql_kill');
  AssignProc(@Client_mysql_ping, 'mysql_ping');
  //AssignProc(@Client_mysql_stat, 'mysql_stat');
  AssignProc(@Client_mysql_get_server_info, 'mysql_get_server_info');
  AssignProc(@Client_mysql_get_client_info, 'mysql_get_client_info');
  AssignProc(@Client_mysql_get_host_info, 'mysql_get_host_info');
  //AssignProc(@Client_mysql_get_proto_info, 'mysql_get_proto_info');
  //AssignProc(@Client_mysql_list_dbs, 'mysql_list_dbs');
  //AssignProc(@Client_mysql_list_tables, 'mysql_list_tables');
  //AssignProc(@Client_mysql_list_fields, 'mysql_list_fields');
  //AssignProc(@Client_mysql_list_processes, 'mysql_list_processes');
  //AssignProc(@Client_mysql_store_result, 'mysql_store_result');
  AssignProc(@Client_mysql_use_result, 'mysql_use_result');
  AssignProc(@Client_mysql_options, 'mysql_options');
  AssignProc(@Client_mysql_options2, 'mysql_options');
  AssignProc(@Client_mysql_free_result, 'mysql_free_result');
  //AssignProc(@Client_mysql_data_seek, 'mysql_data_seek');
  //AssignProc(@Client_mysql_row_seek, 'mysql_row_seek');
  //AssignProc(@Client_mysql_field_seek, 'mysql_field_seek');
  AssignProc(@Client_mysql_fetch_row, 'mysql_fetch_row');
  AssignProc(@Client_mysql_fetch_lengths, 'mysql_fetch_lengths');
  //AssignProc(@Client_mysql_fetch_field, 'mysql_fetch_field');
  //AssignProc(@Client_mysql_escape_string, 'mysql_escape_string');
  //AssignProc(@Client_mysql_real_escape_string, 'mysql_real_escape_string');
  //AssignProc(@Client_mysql_debug, 'mysql_debug');
  //AssignProc(@Client_mysql_odbc_escape_string, 'mysql_odbc_escape_string');
  //AssignProc(@myodbc_remove_escape, 'myodbc_remove_escape');
  //AssignProc(@Client_mysql_thread_safe, 'mysql_thread_safe');

  AssignProc(@Client_mysql_server_init, 'mysql_server_init');
  AssignProc(@Client_mysql_server_end, 'mysql_server_end');

// C API Prepared Statements functions
  AssignProc(@Client_mysql_stmt_init, 'mysql_stmt_init');
  AssignProc(@Client_mysql_stmt_prepare, 'mysql_stmt_prepare');
  AssignProc(@Client_mysql_stmt_execute, 'mysql_stmt_execute');
  AssignProc(@Client_mysql_stmt_param_count, 'mysql_stmt_param_count');
  AssignProc(@Client_mysql_stmt_bind_param, 'mysql_stmt_bind_param');
  AssignProc(@Client_mysql_stmt_bind_result, 'mysql_stmt_bind_result');
  AssignProc(@Client_mysql_stmt_field_count, 'mysql_stmt_field_count');
  AssignProc(@Client_mysql_stmt_close, 'mysql_stmt_close');
  AssignProc(@Client_mysql_stmt_free_result, 'mysql_stmt_free_result');
  AssignProc(@Client_mysql_stmt_errno, 'mysql_stmt_errno');
  AssignProc(@Client_mysql_stmt_error, 'mysql_stmt_error');
  //AssignProc(@Client_mysql_commit, 'mysql_commit');
  //AssignProc(@Client_mysql_rollback, 'mysql_rollback');
  //AssignProc(@Client_mysql_autocommit, 'mysql_autocommit');
  AssignProc(@Client_mysql_fetch, 'mysql_stmt_fetch');
  AssignProc(@Client_mysql_fetch_column, 'mysql_stmt_fetch_column');
  AssignProc(@Client_mysql_stmt_send_long_data, 'mysql_stmt_send_long_data');
  AssignProc(@Client_mysql_stmt_prepare_result, 'mysql_stmt_prepare_result');
  AssignProc(@Client_mysql_stmt_result_metadata, 'mysql_stmt_result_metadata');
  AssignProc(@Client_mysql_stmt_param_metadata, 'mysql_stmt_param_metadata');
  AssignProc(@Client_mysql_stmt_affected_rows, 'mysql_stmt_affected_rows');
  AssignProc(@Client_mysql_stmt_store_result, 'mysql_stmt_store_result');
  AssignProc(@Client_mysql_more_results, 'mysql_more_results');
  AssignProc(@Client_mysql_next_result, 'mysql_next_result');
{$ENDIF}
end;

procedure TMySQLAPIClient.FreeMySQLLib;
begin
{$IFDEF MSWINDOWS}
  if hMySQLLib <> 0 then begin
    FreeLibrary(hMySQLLib);
    hMySQLLib := 0;
  end;
{$ENDIF}
{$IFDEF LINUX}
  if hMySQLLib <> nil then begin
    dlclose(hMySQLLib);
    hMySQLLib := nil;
  end;
{$ENDIF}

  inherited;
end;

procedure TMySQLAPIClient.CheckMySQLLib;
var
  Error: EOSError;
  Msg: string;
{$IFDEF LINUX}
  s: string;
{$ENDIF}
begin
  LoadMySQLLib;
  if not LoadedMySQLLib then begin
  {$IFDEF MSWINDOWS}
    Msg := 'MySQL client library couldn''t be loaded. Please place ' + GetLibraryName + ' file to system folder (included to PATH) or to the folder with executable unit of main program.';
  {$ENDIF}
  {$IFDEF LINUX}
    Msg := 'MySQL client library couldn''t be loaded. Please place ' + GetLibraryName + ' (' + GetLibraryName +'.X) file to system folder (included to LD_LIBRARY_PATH) or to the folder with executable unit of main program.';
    s := dlerror;
    if s <> '' then
      Msg := Msg + #$D#$A + s;
  {$ENDIF}
    Error := EOSError.Create(Msg);
    Error.ErrorCode := FLastError;
    raise Error;
  end;

  inherited;
end;

destructor TMySQLAPIClient.Destroy;
begin
  if LoadedMySQLLib then
    FreeMySQLLib;

  inherited;
end;

{$IFDEF EMBLOG}
{ TMutex }

constructor TMutex.Create(const Name: ShortString; const InitialTimeout: Longword = INFINITE);
  function PrepareName(const s: ShortString): ShortString;
  var
    i: integer;
  begin
    Result := LowerCase(Trim(s));
    for i := 1 to Length(Result) do
      if Result[i] in ['\', ':', '/'] then
        Result[i] := ' ';
    Assert(Result <> '');
  end;

begin
  inherited Create;

  FName := PrepareName(Name);
  FTimeout := InitialTimeout;

  FHandle := Windows.CreateMutex(nil, False, PChar(string(FName)));
  Win32Check(FHandle <> 0);
end;

destructor TMutex.Destroy;
begin
  CloseHandle(FHandle);
  inherited;
end;

procedure TMutex.Acquire;
var
  Res: integer;
  s: string;

begin
  inherited;

  Res := WaitForSingleObject(FHandle, FTimeout);
  if Res = WAIT_ABANDONED then
    Res := WaitForSingleObject(FHandle, FTimeout);

  try
    Win32Check(Res = WAIT_OBJECT_0);
  except
    on E: EOSError do
    begin
      case Res of
        WAIT_ABANDONED:
          s := 'WAIT_ABANDONED';
        WAIT_TIMEOUT:
          s := 'WAIT_TIMEOUT';
        else
          s := IntToStr(Res);
      end;
      E.Message := string(FName) + '.Acquire = ' + s + #$D#$A + E.Message;
      raise;
    end;
  end;
end;

procedure TMutex.Release;
begin
  Win32Check(ReleaseMutex(FHandle));
  inherited;
end;
{$ENDIF}

{ TMySQLAPIEmbedded }

constructor TMySQLAPIEmbedded.Create;
begin
  inherited;
  FParams := TStringList.Create;
{$IFDEF EMBLOG}
  FListOnLog := TDAList.Create;
  FListOnLogError := TDAList.Create;
{$ENDIF}
end;

destructor TMySQLAPIEmbedded.Destroy;
begin
{$IFDEF EMBLOG}
  FListOnLog.Free;
  FListOnLogError.Free;
{$ENDIF}
  FParams.Free;
  FClientsCount := 0;
  inherited;
end;

procedure TMySQLAPIEmbedded.SetParams(Value: TStrings);
begin
  FParams.Assign(Value);
end;

class function TMySQLAPIEmbedded.GetLibraryName: string;
begin
{$IFNDEF CLR}
  if MySQLClientLibrary <> '' then begin
    Result := MySQLClientLibrary;
    Exit;
  end;
{$ENDIF}
  
{$IFDEF MSWINDOWS}
  Result := 'libmysqld.dll';
{$ENDIF}
{$IFDEF LINUX}
  Result := 'libmysqld.so';
{$ENDIF}
end;

{$IFNDEF CLR}
class function TMySQLAPIEmbedded.GetNotLinkProc: FARPROC;
begin
  Result := @NotLinkEmbedded;
end;
{$ENDIF}

{$IFDEF EMBLOG}
const
  SLogFileName = 'CAD387A7-72D8-4DB1-BA22-FAEE146918E1';
  SLogErrorFileName = 'BFCA0253-65E4-4021-8D35-CD34D2727C1B';

var
  KnownHandles: array of THandle;

procedure AddInitMessage(const s: string);
begin
  if MySQLEmbServerInitMessage = '' then
    MySQLEmbServerInitMessage := s
  else
    MySQLEmbServerInitMessage := MySQLEmbServerInitMessage + #$D#$A + s;
end;

function MyCreateFile(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall;
var
  FullFileName, FileName: AnsiString;
begin
  Result := MyAPIEmbedded.CreateFile(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);

  if not MySQLEmbServerIniting then
    SetLength(KnownHandles, 0)
  else
    if Result <> INVALID_HANDLE_VALUE then begin
      SetLength(KnownHandles, Length(KnownHandles) + 1);
      KnownHandles[Length(KnownHandles) - 1] := Result;
    end
    else
    begin
      FullFileName := lpFileName;
      FileName := LowerCase(ExtractFileName(FullFileName));
      if (Pos(AnsiString('my.ini'), FileName) = 0) and (Pos(AnsiString('my.cnf'), FileName) = 0) then
        AddInitMessage(string(FullFileName) + ' - ' + SysErrorMessage(GetLastError));
    end;
end;

function BufferToStr(const Buffer; nNumberOfBytesToWrite: DWORD): AnsiString;
begin
  SetLength(Result, nNumberOfBytesToWrite);
  if nNumberOfBytesToWrite > 0 then
    CopyMemory(@Result[1], @Buffer, nNumberOfBytesToWrite);
end;

function MyWriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
    var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
var
  Accept, Found: boolean;
  i: integer;
begin
  if MySQLEmbServerIniting then begin
    Accept := (hFile = GetStdHandle(STD_OUTPUT_HANDLE)) or (hFile = GetStdHandle(STD_ERROR_HANDLE));
    if not Accept then begin
      Found := False;
      for i := Low(KnownHandles) to High(KnownHandles) do
        if KnownHandles[i] = hFile then begin
          Found := True;
          Break;
        end;
      Accept := not Found;
    end;
    if Accept then
      AddInitMessage(string(BufferToStr(Buffer, nNumberOfBytesToWrite)));
  end;

  Result := MyAPIEmbedded.WriteFile(hFile, Buffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped);
end;

procedure MyExitProcess(uExitCode: UINT); stdcall;
begin
  MyAPIEmbedded.ExitProcess(uExitCode);
end;

{function MyCloseHandle(hObject: THandle): BOOL; stdcall;
begin
  case hObject
  try
    if Pos(SLogFileName, lpFileName) > 0 then
      Result := FLogHandle
    else
    if Pos(SLogErrorFileName, lpFileName) > 0 then
      Result := FLogErrorHandle
    else
      Result := addr_CloseHandle(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
  except
    Result := False;
  end;
end;}

type
  _IMAGE_IMPORT_DESCRIPTOR = packed record
    case Integer of
      0: (Characteristics: DWORD);
      1: (OriginalFirstThunk: DWORD;
          TimeDateStamp: DWORD;
          ForwarderChain: DWORD;
          Name: DWORD;
          FirstThunk: DWORD)
  end;
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;
  PIMAGE_IMPORT_DESCRIPTOR = ^IMAGE_IMPORT_DESCRIPTOR;
  PFARPROC = ^FARPROC;
  PLONGBOOL = ^LONGBOOL;

procedure ReplaceIATEntryInOneMod(pszCallerModName: PAnsiChar; pfnCurrent: FARPROC; pfnNew: FARPROC; hmodCaller: hModule);
var
  ulSize: ULONG;
  pImportDesc: PIMAGE_IMPORT_DESCRIPTOR;
  pszModName: PAnsiChar;
  pThunk: PDWORD;
  ppfn: PFARPROC;
  ffound: LongBool;
  written: {$IFDEF VER16P}SIZE_T{$ELSE}DWORD{$ENDIF};
begin
  pImportDesc := ImageDirectoryEntryToData(Pointer(hmodCaller), TRUE,IMAGE_DIRECTORY_ENTRY_IMPORT, ulSize);
  if pImportDesc = nil then
    Exit;
  while pImportDesc.Name <> 0 do
  begin
    pszModName := PAnsiChar(hmodCaller + pImportDesc.Name);
    if (lstrcmpiA(pszModName, pszCallerModName) = 0) then
      Break;
    Inc(pImportDesc)
  end;
  if (pImportDesc.Name = 0) then
    Exit;
  pThunk := PDWORD(hmodCaller + pImportDesc.FirstThunk);
  while pThunk^ <> 0 do
  begin
    ppfn := PFARPROC(pThunk);
    fFound := (ppfn^ = pfnCurrent);
    if (fFound) then
    begin
      Windows.VirtualProtectEx(GetCurrentProcess, ppfn, 4, PAGE_EXECUTE_READWRITE, @written);
      Windows.WriteProcessMemory(GetCurrentProcess, ppfn, @pfnNew, SizeOf(pfnNew), Written);
      Exit
    end;
    Inc(pThunk)
  end
end;

procedure InterceptFunctions(const ModuleName, ProcName: string; const pfnNew: IntPtr; out pfnOld: IntPtr);
var
  hSnapShot: THandle;
  me32: MODULEENTRY32;
begin
  pfnOld := GetProcAddress(getModuleHandle(PChar(ModuleName)), PChar(ProcName));
  hSnapShot := CreateToolHelp32SnapShot(TH32CS_SNAPMODULE, GetCurrentProcessId);
  if hSnapshot <> INVALID_HANDLE_VALUE then
    try
      ZeroMemory(@me32, SizeOf(MODULEENTRY32));
      me32.dwSize := SizeOf(MODULEENTRY32);
      Module32First(hSnapShot, me32);
      repeat
        ReplaceIATEntryInOneMod(PAnsiChar(AnsiString(ModuleName)), pfnOld, pfnNew, me32.hModule)
      until not Module32Next(hSnapShot, me32)
    finally
      CloseHandle(hSnapShot)
    end;

  if pfnOld = nil then
    DatabaseError(SCannotIntercept);
end;

procedure UnInterceptFunctions(const ModuleName, ProcName: string; const pfnCurrent: FarProc);
var
  hSnapShot: THandle;
  me32: MODULEENTRY32;
  pfnOld: FarProc;
begin
  pfnOld := GetProcAddress(getModuleHandle(PChar(ModuleName)), PChar(ProcName));
  hSnapShot := CreateToolHelp32SnapShot(TH32CS_SNAPMODULE, GetCurrentProcessId);
  if hSnapshot = INVALID_HANDLE_VALUE then
    Exit;
  try
    ZeroMemory(@me32, SizeOf(MODULEENTRY32));
    me32.dwSize := SizeOf(MODULEENTRY32);
    Module32First(hSnapShot, me32);
    repeat
      ReplaceIATEntryInOneMod(PAnsiChar(AnsiString(ModuleName)), pfnCurrent, pfnOld, me32.hModule)
    until not Module32Next(hSnapShot, me32)
  finally
    CloseHandle(hSnapShot)
  end
end;

function TMySQLAPIEmbedded.CreateFile(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle;
var
  s: AnsiString;
begin
  s := lpFileName; // for Delphi 2007
  if Pos(AnsiString(SLogFileName), s) > 0 then begin
    Result := FpCreateFile(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes or FILE_FLAG_DELETE_ON_CLOSE, hTemplateFile);
    FLogHandle := Result;
  end
  else
  if Pos(AnsiString(SLogErrorFileName), s) > 0 then begin
    Result := FpCreateFile('NUL', dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
    FLogErrorHandle := Result;
  end
  else
    Result := FpCreateFile(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
end;

function TMySQLAPIEmbedded.WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; lpOverlapped: POverlapped): BOOL;
var
  OnLog, OnLogError: TMyLogEvent;
  i: Integer;
begin
  Result := True;
  lpNumberOfBytesWritten := nNumberOfBytesToWrite;
  if hFile = FLogHandle then
    for i := 0 to FListOnLog.Count - 1 do begin
      OnLog := TMyLogEvent(TMethod(FListOnLog[i]^));
      OnLog(string(BufferToStr(Buffer, nNumberOfBytesToWrite)));
    end
  else
  if hFile = FLogErrorHandle then
    for i := 0 to FListOnLogError.Count - 1 do begin
      OnLogError := TMyLogEvent(TMethod(FListOnLogError[i]^));
      OnLogError(string(BufferToStr(Buffer, nNumberOfBytesToWrite)));
    end
  else
  {if (hFile = GetStdHandle(STD_OUTPUT_HANDLE)) and Assigned(FOnStdOut) then
    FOnStdOut(Copy(PChar(@Buffer), 1, nNumberOfBytesToWrite))
  else
  if (hFile = GetStdHandle(STD_ERROR_HANDLE)) and Assigned(FOnStdErr) then
    FOnStdErr(Copy(PChar(@Buffer), 1, nNumberOfBytesToWrite))
  else}
    Result := FpWriteFile(hFile, Buffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped);
end;

procedure TMySQLAPIEmbedded.ExitProcess(uExitCode: UINT);
begin
  if not MySQLEmbServerIniting then
    FpExitProcess(uExitCode)
  else
    raise Exception.Create('');
end;

type
  PMethod = ^TMethod;

procedure TMySQLAPIEmbedded.RegisterOnLogEvent(const Event: TMyLogEvent);
var
  M: PMethod;
begin
  GetMem(M, sizeof(TMethod));
  M^ := TMethod(Event);
  FListOnLog.Add(M);
end;

procedure TMySQLAPIEmbedded.UnRegisterOnLogEvent(const Event: TMyLogEvent);
var
  M: TMethod;
  i: Integer;
begin
  for i := 0 to FListOnLog.Count - 1 do begin
    M := TMethod(FListOnLog[i]^);
    if (M.Code = TMethod(Event).Code) and (M.Data = TMethod(Event).Data) then begin
      FreeMem(FListOnLog[i]);
      FListOnLog.Delete(i);
      Exit;
    end;
  end;
end;

procedure TMySQLAPIEmbedded.RegisterOnLogErrorEvent(const Event: TMyLogEvent);
var
  M: PMethod;
begin
  GetMem(M, sizeof(TMethod));
  M^ := TMethod(Event);
  FListOnLogError.Add(M);
end;

procedure TMySQLAPIEmbedded.UnRegisterOnLogErrorEvent(const Event: TMyLogEvent);
var
  M: TMethod;
  i: Integer;
begin
  for i := 0 to FListOnLogError.Count - 1 do begin
    M := TMethod(FListOnLogError[i]^);
    if (M.Code = TMethod(Event).Code) and (M.Data = TMethod(Event).Data) then begin
      FreeMem(FListOnLogError[i]);
      FListOnLogError.Delete(i);
      Exit;
    end;
  end;
end;

{$ELSE} //EMBLOG
procedure TMySQLAPIEmbedded.RegisterOnLogEvent(const Event: TMyLogEvent);
begin
end;

procedure TMySQLAPIEmbedded.UnRegisterOnLogEvent(const Event: TMyLogEvent);
begin
end;

procedure TMySQLAPIEmbedded.RegisterOnLogErrorEvent(const Event: TMyLogEvent);
begin
end;

procedure TMySQLAPIEmbedded.UnRegisterOnLogErrorEvent(const Event: TMyLogEvent);
begin
end;
{$ENDIF}

procedure TMySQLAPIEmbedded.UnRegisterConnection;
begin
  if FClientsCount > 0 then begin
    Dec(FClientsCount);

    if (FClientsCount = 0) and (IsLibrary or UnloadEmbLibraryOnDisconnect) then //dbxMda
      FreeMySQLLib;
  end;
end;

procedure TMySQLAPIEmbedded.LoadMySQLLib;
var
  server_args, server_groups: IntPtr;
  server_args_len, i: integer;
  AppName: string;
  s: string;
  p: IntPtr;

  b: TBytes;
  Len: integer;

  Failed: boolean;
  NewParams: string;
{$IFDEF EMBLOG}
  paddr: Pointer;
{$ENDIF}

begin
  if FCurrentParams <> FParams.Text then
    if FClientsCount = 0 then
      FreeMySQLLib
    else
      raise Exception.Create(STwoEmbServer);

  if LoadedMySQLLib then begin
    Inc(FClientsCount);
    Exit;
  end;

  // Prepare params
  for i := FParams.Count - 1 downto 0 do begin
    s := Trim(Params[i]);
    if (s = '') or (s[1] = '#') then
      Params.Delete(i);
  end;

  inherited;

  if LoadedMySQLLib then begin
    Inc(FClientsCount);
    try
//      if Params.IndexOfName('--basedir') = -1 then
//        Params.Values['--basedir'] := '.';

      NewParams := FParams.Text;
    {$IFDEF EMBLOG}
      InterceptFunctions('kernel32.dll', 'CreateFileA', @MyCreateFile, paddr);
      FpCreateFile := paddr;
      InterceptFunctions('kernel32.dll', 'WriteFile', @MyWriteFile, paddr);
      FpWriteFile := paddr;
      InterceptFunctions('kernel32.dll', 'ExitProcess', @MyExitProcess, paddr);
      FpExitProcess := paddr;
      {InterceptFunctions('kernel32.dll', 'CloseHandleA', @MyCloseHandle, paddr);
      addr_CloseHandle := paddr;}

      if not MySQLEmbDisableEventLog then begin
        FParams.Add('--log=' + SLogFileName);
        FParams.Add('--log-error=' + SLogErrorFileName);
      end;

      // Check if second instance
      Assert(FDataDirMutex = nil);
      s := FParams.Values['--datadir'];
      if s = '' then begin
        s := FParams.Values['--basedir'];
        if s <> '' then
          s := IncludeTrailingBackslash(s) + 'data';
      end;

      if s <> '' then begin
        s := ExpandFileName(s);
        FDataDirMutex := TMutex.Create(ShortString(s), 2000);
        try
          FDataDirMutex.Acquire;
        except
          on E: EOSError do begin
            raise Exception.Create(STwoEmbServer);
          end;
        end;
      end;
    {$ENDIF}

      MySQLEmbServerInitMessage := '';
      MySQLEmbServerIniting := True;
      server_args := nil;
      server_groups := nil;
      server_args_len := (1 + FParams.Count);
      try
        server_args := Marshal.AllocHGlobal(server_args_len * sizeof(IntPtr));
        for i := 0 to FParams.Count do begin
          if i = 0 then
            s := 'ignored'
          else
            s := FParams[i - 1];

          /// may be optimized for non CLR platforms
          Len := Length(s);
          SetLength(b, Len + 1);
          Encoding.Default.GetBytes(s, 0, Len, b, 0);
          b[Len] := 0;
          p := Marshal.AllocHGlobal(Len + 1);
          Marshal.Copy(b, 0, p, Len + 1);

          Marshal.WriteIntPtr(server_args, i * sizeof(IntPtr), p);
        end;

        AppName := ExtractFileName(ParamStr(0));
        server_groups := Marshal.AllocHGlobal(4 * sizeof(IntPtr));
        Marshal.WriteIntPtr(server_groups, 3 * sizeof(IntPtr), nil);
        for i := 0 to 2 do begin
          case i of
            0: s := 'embedded';
            1: s := 'server';
            2: s := AppName;
          end;

          /// may be optimized for non CLR platforms
          Len := Length(s);
          SetLength(b, Len + 1);
          Encoding.Default.GetBytes(s, 0, Len, b, 0);
          b[Len] := 0;
          p := Marshal.AllocHGlobal(Len + 1);
          Marshal.Copy(b, 0, p, Len + 1);

          Marshal.WriteIntPtr(server_groups, i * sizeof(IntPtr), p);
        end;

        try
          Failed := mysql_server_init(server_args_len, server_args, server_groups) <> 0;
          FServerInited := True;
          s := '';
        except // mysql_server_init failed or ExitProcess intercepted
          on e: Exception do begin
            {inherited FreeMySQLLib;}
            s := e.Message;
            Failed := True;
          end;
        end;

        if Failed then begin
          if s <> '' then
            s := SEServInitErr + #$D#$A + s
          else
            s := SEServInitErr;
          if MySQLEmbServerInitMessage <> '' then
            s := s + #$D#$A + MySQLEmbServerInitMessage;
          raise Exception.Create(s);
        end;

        FCurrentParams := NewParams;
      finally
        MySQLEmbServerIniting := False;

        if server_args <> nil then begin
          for i := 0 to server_args_len - 1 do
            Marshal.FreeHGlobal(Marshal.ReadIntPtr(server_args, i * sizeof(IntPtr)));
          Marshal.FreeHGlobal(server_args);
        end;

        if server_groups <> nil then begin
          for i := 0 to 2 do
            Marshal.FreeHGlobal(Marshal.ReadIntPtr(server_groups, i * sizeof(IntPtr)));
          Marshal.FreeHGlobal(server_groups);
        end;
      end;
    except
      FreeMySQLLib;
      raise;
    end;
  end;
end;

procedure TMySQLAPIEmbedded.FreeMySQLLib;
begin
  if LoadedMySQLLib then begin
    UnRegisterConnection;
    if FClientsCount > 0 then
      Exit;

  {$IFDEF EMBLOG}
    if Assigned(FpCreateFile) then
      UnInterceptFunctions('kernel32.dll', 'CreateFileA', @MyCreateFile);
    FpCreateFile := nil;
    if Assigned(FpWriteFile) then
      UnInterceptFunctions('kernel32.dll', 'WriteFile', @MyWriteFile);
    FpWriteFile := nil;
    if Assigned(FpExitProcess) then
      UnInterceptFunctions('kernel32.dll', 'ExitProcess', @MyExitProcess);
    FpExitProcess := nil;
    {if Assigned(addr_CloseHandle) then
      UnInterceptFunctions('kernel32.dll', 'CloseHandleA', @MyCloseHandle);
    addr_CloseHandle := nil;}
  {$ENDIF}

    if FServerInited then begin
      FServerInited := False;
      mysql_server_end;
    {$IFDEF EMBLOG}
      if FDataDirMutex <> nil then
        FDataDirMutex.Release;
    {$ENDIF}
    end;
  {$IFDEF EMBLOG}
    FDataDirMutex.Free;
    FDataDirMutex := nil;
  {$ENDIF}
    FCurrentParams := '';
  end;

  inherited;
end;

{$IFDEF CLR}
//function TMySQLAPIEmbedded.mysql_num_rows(pres: PMYSQL_RES): my_ulonglong;
function TMySQLAPIEmbedded.mysql_num_fields(pres: PMYSQL_RES): longword;
begin
  Result := Emb_mysql_num_fields(ObjToIntPtr(pres));
end;

//function TMySQLAPIEmbedded.mysql_eof(pres: PMYSQL_RES): my_bool;
function TMySQLAPIEmbedded.mysql_fetch_field_direct(pres: PMYSQL_RES; fieldnr: longword): PMYSQL_FIELD;
begin
  Result := IntPtrToObj(Emb_mysql_fetch_field_direct(ObjToIntPtr(pres), fieldnr));
end;

//function TMySQLAPIEmbedded.mysql_fetch_fields(pres: PMYSQL_RES): PMYSQL_FIELDS;
//function TMySQLAPIEmbedded.mysql_row_tell(pres: PMYSQL_RES): PMYSQL_ROWS;
//function TMySQLAPIEmbedded.mysql_field_tell(pres: PMYSQL_RES): longword;

function TMySQLAPIEmbedded.mysql_field_count(pmysql: PMYSQL_CON): longword;
begin
  Result := Emb_mysql_field_count(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_affected_rows(pmysql: PMYSQL_CON): my_ulonglong;
begin
  Result := Emb_mysql_affected_rows(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_insert_id(pmysql: PMYSQL_CON): my_ulonglong;
begin
  Result := Emb_mysql_insert_id(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_errno(pmysql: PMYSQL_CON): longword;
begin
  Result := Emb_mysql_errno(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_error(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Emb_mysql_error(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_info(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Emb_mysql_info(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_thread_id(pmysql: PMYSQL_CON): longword;
begin
  Result := Emb_mysql_thread_id(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_character_set_name(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Emb_mysql_character_set_name(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_init(pmysql: PMYSQL_CON): PMYSQL_CON;
begin
  Result := IntPtrToObj(Emb_mysql_init(ObjToIntPtr(pmysql)))
end;

{$IFDEF HAVE_OPENSSL}
function TMySQLAPIEmbedded.mysql_ssl_set(pmysql: PMYSQL_CON; const key, cert, ca, capath, cipher: PAnsiChar): longint;

begin
  Result := Emb_mysql_ssl_set(ObjToIntPtr(pmysql), key, cert, ca, capath, cipher);
end;
{$ENDIF} // HAVE_OPENSSL
//function TMySQLAPIEmbedded.mysql_connect(pmysql: PMYSQL_CON; const host, user, passwd: PChar): PMYSQL_CON;
//function TMySQLAPIEmbedded.mysql_change_user(pmysql: PMYSQL_CON; const user, passwd, db: PChar): my_bool;
function TMySQLAPIEmbedded.mysql_real_connect(pmysql: PMYSQL_CON; host, user, passwd, db: PAnsiChar; port: longword; unix_socket: PAnsiChar; clientflag: longword): PMYSQL_CON;
begin
  Result := IntPtrToObj(Emb_mysql_real_connect(ObjToIntPtr(pmysql), host, user, passwd, db, port, unix_socket, clientflag));
end;

procedure TMySQLAPIEmbedded.mysql_close(sock: PMYSQL_CON);
begin
  Emb_mysql_close(ObjToIntPtr(sock));
end;

function TMySQLAPIEmbedded.mysql_select_db(pmysql: PMYSQL_CON; const db: PAnsiChar): longint;
begin
  Result := Emb_mysql_select_db(ObjToIntPtr(pmysql), db);
end;

//function TMySQLAPIEmbedded.mysql_query(pmysql: PMYSQL_CON; const q: PChar): longint;
//function TMySQLAPIEmbedded.mysql_send_query(pmysql: PMYSQL_CON; const q: PChar; length: longword): longint;
//function TMySQLAPIEmbedded.mysql_read_query_result(pmysql: PMYSQL_CON): longint;
function TMySQLAPIEmbedded.mysql_real_query(pmysql: PMYSQL_CON; const q: AnsiString; length: longword): longint;
begin
  Result := Emb_mysql_real_query(ObjToIntPtr(pmysql), q, length);
end;

//function TMySQLAPIEmbedded.mysql_create_db(pmysql: PMYSQL_CON; const DB: PChar): longint;
//function TMySQLAPIEmbedded.mysql_drop_db(pmysql: PMYSQL_CON; const DB: PChar): longint;
//function TMySQLAPIEmbedded.mysql_shutdown(pmysql: PMYSQL_CON): longint;
//function TMySQLAPIEmbedded.mysql_dump_debug_info(pmysql: PMYSQL_CON): longint;
//function TMySQLAPIEmbedded.mysql_refresh(pmysql: PMYSQL_CON; refresh_options: longword): longint;
function TMySQLAPIEmbedded.mysql_kill(pmysql: PMYSQL_CON; pid: longword): longint;
begin
  Result := Emb_mysql_kill(ObjToIntPtr(pmysql), pid);
end;

function TMySQLAPIEmbedded.mysql_ping(pmysql: PMYSQL_CON): longint;
begin
  Result := Emb_mysql_ping(ObjToIntPtr(pmysql));
end;

//function TMySQLAPIEmbedded.mysql_stat(pmysql: PMYSQL_CON): PChar;
function TMySQLAPIEmbedded.mysql_get_server_info(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Emb_mysql_get_server_info(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_get_client_info: AnsiString;
begin
  Result := Emb_mysql_get_client_info;
end;

function TMySQLAPIEmbedded.mysql_get_host_info(pmysql: PMYSQL_CON): AnsiString;
begin
  Result := Emb_mysql_get_host_info(ObjToIntPtr(pmysql));
end;

//function TMySQLAPIEmbedded.mysql_get_proto_info(pmysql: PMYSQL_CON): longword;
//function TMySQLAPIEmbedded.mysql_list_dbs(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES;
//function TMySQLAPIEmbedded.mysql_list_tables(pmysql: PMYSQL_CON; const wild: PChar): PMYSQL_RES;
//function TMySQLAPIEmbedded.mysql_list_fields(pmysql: PMYSQL_CON; const table, wild: PChar): PMYSQL_RES;
//function TMySQLAPIEmbedded.mysql_list_processes(pmysql: PMYSQL_CON): PMYSQL_RES;
//function TMySQLAPIEmbedded.mysql_store_result(pmysql: PMYSQL_CON): PMYSQL_RES;
function TMySQLAPIEmbedded.mysql_use_result(pmysql: PMYSQL_CON): PMYSQL_RES;
begin
  Result := IntPtrToObj(Emb_mysql_use_result(ObjToIntPtr(pmysql)));
end;

function TMySQLAPIEmbedded.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; var arg: integer): longint;
begin
  Result := Emb_mysql_options(ObjToIntPtr(pmysql), Integer(option), arg);
end;

function TMySQLAPIEmbedded.mysql_options(pmysql: PMYSQL_CON; option: TMySqlOption; const arg: PAnsiChar): longint;
begin
  Result := Emb_mysql_options2(ObjToIntPtr(pmysql), Integer(option), arg);
end;

procedure TMySQLAPIEmbedded.mysql_free_result(pres: PMYSQL_RES);
begin
  Emb_mysql_free_result(ObjToIntPtr(pres));
end;

//function TMySQLAPIEmbedded.mysql_data_seek: procedure(pres: PMYSQL_RES; offset: my_ulonglong);
//function TMySQLAPIEmbedded.mysql_row_seek(pres: PMYSQL_RES; offset: TMySQLAPIEmbedded.mysql_ROW_OFFSET): TMySQLAPIEmbedded.mysql_ROW_OFFSET;
//function TMySQLAPIEmbedded.mysql_field_seek(pres: PMYSQL_RES; offset: TMySQLAPIEmbedded.mysql_FIELD_OFFSET): TMySQLAPIEmbedded.mysql_FIELD_OFFSET;
function TMySQLAPIEmbedded.mysql_fetch_row(pres: PMYSQL_RES): PMYSQL_ROW;
begin
  Result := IntPtrToObj(Emb_mysql_fetch_row(ObjToIntPtr(pres)));
end;

function TMySQLAPIEmbedded.mysql_fetch_lengths(pres: PMYSQL_RES): PMYSQL_LENGTHS;
begin
  Result := IntPtrToObj(Emb_mysql_fetch_lengths(ObjToIntPtr(pres)));
end;

//function TMySQLAPIEmbedded.mysql_fetch_field(pres: PMYSQL_RES): PMYSQL_FIELD;
//function TMySQLAPIEmbedded.mysql_escape_string(_to: PChar; const from: PChar; from_length: longword): longword;
//function TMySQLAPIEmbedded.mysql_real_escape_string(pmysql: PMYSQL_CON; _to: PChar; const from: PChar; length: longword): longword;
//function TMySQLAPIEmbedded.mysql_debug: procedure(const debug: PChar);
//function TMySQLAPIEmbedded.mysql_odbc_escape_string(pmysql: PMYSQL_CON; _to: PChar; to_length: longword; const from: PChar; from_length: longword; param: pointer; extend_buffer: extend_buffer_func): PChar;
//myodbc_remove_escape: procedure(pmysql: PMYSQL_CON; name: PChar);
//function TMySQLAPIEmbedded.mysql_thread_safe: function: longword;

function TMySQLAPIEmbedded.mysql_server_init(argc: integer; argv: PPChar; groups: PPChar): integer;
begin
  Result := Emb_mysql_server_init(argc, argv, groups);
end;

procedure TMySQLAPIEmbedded.mysql_server_end;
begin
  Emb_mysql_server_end;
end;

//  function function TMySQLAPIEmbedded.mysql_reload(pmysql: PMySQL): longint;

// C API Prepared Statements functions
function TMySQLAPIEmbedded.mysql_stmt_prepare(pmysql: PMYSQL_STMT;  query: PAnsiChar;  length: UINT): int;
begin
  Result := Emb_mysql_stmt_prepare(ObjToIntPtr(pmysql), query, length);
end;

function TMySQLAPIEmbedded.mysql_stmt_execute(pstmt: PMYSQL_STMT): integer;
begin
  Result := Emb_mysql_stmt_execute(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_stmt_param_count(pstmt: PMYSQL_STMT): UINT;
begin
  Result := Emb_mysql_stmt_param_count(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_stmt_bind_param(pstmt: PMYSQL_STMT;  bnd: PMYSQL_BIND): my_bool;
begin
  Result := Emb_mysql_stmt_bind_param(ObjToIntPtr(pstmt), bnd);
end;

function TMySQLAPIEmbedded.mysql_stmt_bind_result(pstmt: PMYSQL_STMT; pbnd: PMYSQL_BIND): my_bool;
begin
  Result := Emb_mysql_stmt_bind_result(ObjToIntPtr(pstmt), pbnd);
end;

function TMySQLAPIEmbedded.mysql_stmt_field_count(pstmt: PMYSQL_STMT): UINT;
begin
  Result := Emb_mysql_stmt_field_count(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_stmt_close(pstmt: PMYSQL_STMT): my_bool;
begin
  Result := Emb_mysql_stmt_close(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_stmt_free_result(pstmt: PMYSQL_STMT): my_bool;
begin
  Result := Emb_mysql_stmt_free_result(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_stmt_errno(pstmt: PMYSQL_STMT): UINT;
begin
  Result := Emb_mysql_stmt_errno(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_stmt_error(pstmt: PMYSQL_STMT): AnsiString;
begin
  Result := Emb_mysql_stmt_error(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_stmt_fetch(pstmt: PMYSQL_STMT): integer;
begin
  Result := Emb_mysql_fetch(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_stmt_fetch_column(pstmt: PMYSQL_STMT; bnd: PMYSQL_BIND; column: uint; offset: longword): integer;
begin
  Result := Emb_mysql_fetch_column(ObjToIntPtr(pstmt), bnd, column, offset);
end;

function TMySQLAPIEmbedded.mysql_stmt_send_long_data(pstmt: PMYSQL_STMT;  param_number: UINT;  data: PAnsiChar;  length: UINT): my_bool;
begin
  Result := Emb_mysql_stmt_send_long_data(ObjToIntPtr(pstmt), param_number, data, length);
end;

function TMySQLAPIEmbedded.mysql_stmt_result_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  Result := IntPtrToObj(Emb_mysql_stmt_prepare_result(ObjToIntPtr(pstmt)));
end;

function TMySQLAPIEmbedded.mysql_stmt_param_metadata(pstmt: PMYSQL_STMT): PMYSQL_RES;
begin
  Result := IntPtrToObj(Emb_mysql_stmt_param_metadata(ObjToIntPtr(pstmt)));
end;

function TMySQLAPIEmbedded.mysql_stmt_affected_rows(pstmt: PMYSQL_STMT): my_ulonglong;
begin
  Result := Emb_mysql_stmt_affected_rows(ObjToIntPtr(pstmt));
end;

function TMySQLAPIEmbedded.mysql_more_results(pmysql: PMYSQL_CON): my_bool;
begin
  Result := Emb_mysql_more_results(ObjToIntPtr(pmysql));
end;

function TMySQLAPIEmbedded.mysql_next_result(pmysql: PMYSQL_CON): int;
begin
  Result := Emb_mysql_next_result(ObjToIntPtr(pmysql));
end;

procedure TMySQLAPIEmbedded._mysql_fetch_lengths(pres: PMYSQL_RES; var Lens: TLenArr);
var
  p: IntPtr;
  FieldCount: integer;
begin
  p := ObjToIntPtr(pres);
  FieldCount := Emb_mysql_num_fields(p);
  p := Emb_mysql_fetch_lengths(p);
  SetLength(Lens, FieldCount);

  Marshal.Copy(p, Lens, 0, FieldCount);
end;

{$ENDIF}

initialization
  MyAPIClient := TMySQLAPIClient.Create;
  MyAPIEmbedded := TMySQLAPIEmbedded.Create;

finalization
  MyAPIClient.Free;
  MyAPIEmbedded.Free;

end.

