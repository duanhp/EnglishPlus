
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I SQLiteDac.inc}
unit LiteCallUni;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
  MemUtils;

const
  SQLiteDLLName = {$IFDEF MSWINDOWS}'sqlite3.dll'{$ELSE}'libsqlite3.so'{$ENDIF};

{$IFNDEF CLR}
var
  SQLiteDLL: string;
{$ELSE}
const
  SQLiteDLL = SQLiteDLLName;
{$ENDIF}

const
  // Result Codes
  SQLITE_OK         =  0;   // Successful result
  SQLITE_ERROR      =  1;   // SQL error or missing database
  SQLITE_INTERNAL   =  2;   // Internal logic error in SQLite
  SQLITE_PERM       =  3;   // Access permission denied
  SQLITE_ABORT      =  4;   // Callback routine requested an abort
  SQLITE_BUSY       =  5;   // The database file is locked
  SQLITE_LOCKED     =  6;   // A table in the database is locked
  SQLITE_NOMEM      =  7;   // A malloc() failed
  SQLITE_READONLY   =  8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT  =  9;   // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR      = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT    = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND   = 12;   // NOT USED. Table or record not found
  SQLITE_FULL       = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN   = 14;   // Unable to open the database file
  SQLITE_PROTOCOL   = 15;   // NOT USED. Database lock protocol error
  SQLITE_EMPTY      = 16;   // Database is empty
  SQLITE_SCHEMA     = 17;   // The database schema changed
  SQLITE_TOOBIG     = 18;   // String or BLOB exceeds size limit
  SQLITE_CONSTRAINT = 19;   // Abort due to constraint violation
  SQLITE_MISMATCH   = 20;   // Data type mismatch
  SQLITE_MISUSE     = 21;   // Library used incorrectly
  SQLITE_NOLFS      = 22;   // Uses OS features not supported on host
  SQLITE_AUTH       = 23;   // Authorization denied
  SQLITE_FORMAT     = 24;   // Auxiliary database format error
  SQLITE_RANGE      = 25;   // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB     = 26;   // File opened that is not a database file
  SQLITE_ROW        = 100;  // sqlite3_step() has another row ready
  SQLITE_DONE       = 101;  // sqlite3_step() has finished executing

  // Fundamental Datatypes
  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  // Special destructor types
  SQLITE_STATIC: IntPtr    = IntPtr(0);
  SQLITE_TRANSIENT: IntPtr = IntPtr(-1);

  SQLITE_UTF8       = 1;
  SQLITE_UTF16LE    = 2;
  SQLITE_UTF16BE    = 3;
  SQLITE_UTF16      = 4;    // Use native byte order
  SQLITE_ANY        = 5;

type
  Tsqlite3 = IntPtr;
  Tsqlite3_stmt = IntPtr;
  Tsqlite3_context = IntPtr;
  Tsqlite3_value = IntPtr;

  TCallBackLiteCollation = function (
    pUserData: IntPtr;
    StrSize1: Integer; const pStr1: IntPtr;
    StrSize2: Integer; const pStr2: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  TCallBackLiteFunction = procedure (
    Context: Tsqlite3_context;
    ParamCount: Integer;
    pData: IntPtr
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  // functions
  _sqlite3_open = function(
    filename: PAnsiChar;    // Database filename (UTF-8)
    out ppDb: Tsqlite3      // OUT: SQLite db handle
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_open16 = function(
    filename: PWideChar;    // Database filename (UTF-16)
    out ppDb: Tsqlite3      // OUT: SQLite db handle
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_open_v2 = function(
    filename: PAnsiChar;    // Database filename (UTF-8)
    out ppDb: Tsqlite3;     // OUT: SQLite db handle
    flags: Integer;         // Flags
    zVfs: PAnsiChar         // Name of VFS module to use
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_close = function(
    pDb: Tsqlite3
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_errcode = function(
    pDb: Tsqlite3
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_errmsg = function(
    pDb: Tsqlite3
  ): PAnsiChar; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_last_insert_rowid = function(
    pDb: Tsqlite3
  ): Int64; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_changes = function(
    pDb: Tsqlite3
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_prepare_v2 = function(
    db: Tsqlite3;           // Database handle
    zSql: PAnsiChar;        // SQL statement, UTF-8 encoded
    nByte: Integer;         // Maximum length of zSql in bytes.
    out ppStmt: Tsqlite3_stmt;  // OUT: Statement handle
    pzTail: IntPtr          // OUT: Pointer to unused portion of zSql
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_step = function(
    pStmt: Tsqlite3_stmt
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_reset = function(
    pStmt: Tsqlite3_stmt
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_finalize = function(
    pStmt: Tsqlite3_stmt
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_count = function(
    pStmt: Tsqlite3_stmt
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_type = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_name = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_origin_name = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_table_name = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_database_name = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_decltype = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_blob = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): IntPtr; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_bytes = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_double = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): double; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_int = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_int64 = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): int64; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_column_text = function(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_bind_parameter_count = function(
    pStmt: Tsqlite3_stmt
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_bind_blob = function(
    pStmt: Tsqlite3_stmt; Index: Integer; pBlob: IntPtr; Size: Integer; pDestrType: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_bind_double = function(
    pStmt: Tsqlite3_stmt; Index: Integer; Value: Double
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_bind_int = function(
    pStmt: Tsqlite3_stmt; Index: Integer; Value: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_bind_int64 = function(
    pStmt: Tsqlite3_stmt; Index: Integer; Value: Int64
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_bind_null = function(
    pStmt: Tsqlite3_stmt; Index: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_bind_text = function(
    pStmt: Tsqlite3_stmt; Index: Integer; Value: PAnsiChar; Size: Integer; pDestrType: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_blob = procedure(
    pContext: Tsqlite3_context; pBlob: IntPtr; Size: Integer; pDestrType: IntPtr
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_double = procedure(
    pContext: Tsqlite3_context; Value: Double
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_error = procedure(
    pContext: Tsqlite3_context; pMsg: IntPtr; Size: Integer
  ); {$IFNDEF CLR}cdecl;{$ENDIF}
  
  _sqlite3_result_error16 = procedure(
    pContext: Tsqlite3_context; pMsg: IntPtr; Size: Integer
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_error_toobig = procedure(
    pContext: Tsqlite3_context
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_error_nomem = procedure(
    pContext: Tsqlite3_context
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_error_code = procedure(
    pContext: Tsqlite3_context; ErrorCode: Integer
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_int = procedure(
    pContext: Tsqlite3_context; Value: Integer
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_int64 = procedure(
    pContext: Tsqlite3_context; Value: Int64
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_null = procedure(
    pContext: Tsqlite3_context
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_text = procedure(
    pContext: Tsqlite3_context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_text16 = procedure(
    pContext: Tsqlite3_context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_text16le = procedure(
    pContext: Tsqlite3_context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_text16be = procedure(
    pContext: Tsqlite3_context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_value = procedure(
    pContext: Tsqlite3_context; Value: Tsqlite3_value
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_result_zeroblob = procedure(
    pContext: Tsqlite3_context; Size: Integer
  ); {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_value_blob = function(
    pValue: IntPtr
  ): IntPtr; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_value_bytes = function(
    pValue: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_value_bytes16 = function(
    pValue: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_value_double = function(
    pValue: IntPtr
  ): Double; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_value_int = function(
    pValue: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_value_int64 = function(
    pValue: IntPtr
  ): Int64; {$IFNDEF CLR}cdecl;{$ENDIF}


  _sqlite3_value_text = function(
    pValue: IntPtr
  ): IntPtr; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_value_text16 = function(
    pValue: IntPtr
  ): IntPtr; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_value_type = function(
    pValue: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_user_data = function(
    pContext: Tsqlite3_context
  ): IntPtr; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_libversion = function(
  ): PAnsiChar; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_libversion_number = function(
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_create_collation = function(
    pSQLite: Tsqlite3; zName: PAnsiChar; eTextRep: Integer; userData: IntPtr; func: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_create_collation16 = function(
    pSQLite: Tsqlite3; zName: PWideChar; eTextRep: Integer; userData: IntPtr; func: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_create_function = function(
    pSQLite: Tsqlite3; zFunctionName: PAnsiChar; nArg: Integer; eTextRep: Integer; pApp: IntPtr; xFunc: IntPtr; xStep: IntPtr; xFinal: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_create_function16 = function(
    pSQLite: Tsqlite3; zFunctionName: PWideChar; nArg: Integer; eTextRep: Integer; pApp: IntPtr; xFunc: IntPtr; xStep: IntPtr; xFinal: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_enable_shared_cache = function(
    Value: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_busy_timeout = function(
    pSQLite: Tsqlite3; MilliSeconds: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_busy_handler = function(
    pSQLite: Tsqlite3; func: IntPtr; userData: IntPtr
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_key = function(
    pSQLite: Tsqlite3; key: PAnsiChar; size: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

  _sqlite3_rekey = function(
    pSQLite: Tsqlite3; newkey: PAnsiChar; size: Integer
  ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}

var
  sqlite3_open: _sqlite3_open;
  sqlite3_open16: _sqlite3_open16;
  sqlite3_open_v2: _sqlite3_open_v2;
  sqlite3_close: _sqlite3_close;
  sqlite3_errcode: _sqlite3_errcode;
  sqlite3_errmsg: _sqlite3_errmsg;
  sqlite3_last_insert_rowid: _sqlite3_last_insert_rowid;
  sqlite3_changes: _sqlite3_changes;
  sqlite3_prepare_v2: _sqlite3_prepare_v2;
  sqlite3_step: _sqlite3_step;
  sqlite3_reset: _sqlite3_reset;
  sqlite3_finalize: _sqlite3_finalize;
  sqlite3_column_count: _sqlite3_column_count;
  sqlite3_column_type: _sqlite3_column_type;
  sqlite3_column_name: _sqlite3_column_name;
  sqlite3_column_origin_name: _sqlite3_column_origin_name;
  sqlite3_column_table_name: _sqlite3_column_table_name;
  sqlite3_column_database_name: _sqlite3_column_database_name;
  sqlite3_column_decltype: _sqlite3_column_decltype;
  sqlite3_column_blob: _sqlite3_column_blob;
  sqlite3_column_bytes: _sqlite3_column_bytes;
  sqlite3_column_double: _sqlite3_column_double;
  sqlite3_column_int: _sqlite3_column_int;
  sqlite3_column_int64: _sqlite3_column_int64;
  sqlite3_column_text: _sqlite3_column_text;
  sqlite3_bind_parameter_count: _sqlite3_bind_parameter_count;
  sqlite3_bind_blob: _sqlite3_bind_blob;
  sqlite3_bind_double: _sqlite3_bind_double;
  sqlite3_bind_int: _sqlite3_bind_int;
  sqlite3_bind_int64: _sqlite3_bind_int64;
  sqlite3_bind_null: _sqlite3_bind_null;
  sqlite3_bind_text: _sqlite3_bind_text;
  sqlite3_result_blob: _sqlite3_result_blob;
  sqlite3_result_double: _sqlite3_result_double;
  sqlite3_result_error: _sqlite3_result_error;
  sqlite3_result_error16: _sqlite3_result_error16;
  sqlite3_result_error_toobig: _sqlite3_result_error_toobig;
  sqlite3_result_error_nomem: _sqlite3_result_error_nomem;
  sqlite3_result_error_code: _sqlite3_result_error_code;
  sqlite3_result_int: _sqlite3_result_int;
  sqlite3_result_int64: _sqlite3_result_int64;
  sqlite3_result_null: _sqlite3_result_null;
  sqlite3_result_text: _sqlite3_result_text;
  sqlite3_result_text16: _sqlite3_result_text16;
  sqlite3_result_text16le: _sqlite3_result_text16le;
  sqlite3_result_text16be: _sqlite3_result_text16be;
  sqlite3_result_value: _sqlite3_result_value;
  sqlite3_result_zeroblob: _sqlite3_result_zeroblob;
  sqlite3_value_blob: _sqlite3_value_blob;
  sqlite3_value_bytes: _sqlite3_value_bytes;
  sqlite3_value_bytes16: _sqlite3_value_bytes16;
  sqlite3_value_double: _sqlite3_value_double;
  sqlite3_value_int: _sqlite3_value_int;
  sqlite3_value_int64: _sqlite3_value_int64;
  sqlite3_value_text: _sqlite3_value_text;
  sqlite3_value_text16: _sqlite3_value_text16;
  sqlite3_value_type: _sqlite3_value_type;
  sqlite3_user_data: _sqlite3_user_data;
  sqlite3_libversion: _sqlite3_libversion;
  sqlite3_libversion_number: _sqlite3_libversion_number;
  sqlite3_create_collation: _sqlite3_create_collation;
  sqlite3_create_collation16: _sqlite3_create_collation16;
  sqlite3_create_function: _sqlite3_create_function;
  sqlite3_create_function16: _sqlite3_create_function16;
  sqlite3_enable_shared_cache: _sqlite3_enable_shared_cache;
  sqlite3_busy_timeout: _sqlite3_busy_timeout;
  sqlite3_busy_handler: _sqlite3_busy_handler;
  sqlite3_key: _sqlite3_key;
  sqlite3_rekey: _sqlite3_rekey;

function SQLiteInited: boolean;
procedure InitSQLite;
procedure FreeSQLite;
procedure GetLiteErrorCode(pDb: Tsqlite3; var ErrorCode: integer);
procedure GetLiteErrorMsg(pDb: Tsqlite3; var ErrorMsg: _string);
procedure GetPredefinedErrorMsg(ErrorCode: integer; var ErrorMsg: _string);
function IsMetaDataAPIAvailable: boolean;

implementation

{$IFDEF CLR}
uses
{$IFNDEF UNIDACPRO}
  LiteCallCLR;
{$ELSE}
  LiteCallCLRUni;
{$ENDIF}
{$ENDIF}

var
  LiteInited: boolean;
{$IFDEF MSWINDOWS}
  hLiteLib: HMODULE;
{$ENDIF}
{$IFDEF LINUX}
  hLiteLib: IntPtr;
{$ENDIF}
  LockInit: TCriticalSection;

function NotLink: integer;
begin
  raise Exception.Create('SQLite function is not linked');
end;

function NotLinkEncryption: integer;
begin
  raise Exception.Create('SQLite DLL that you are using does not support SQLite database encryption. ' + #13 +
                         'Please download a new DLL or recompile the existing one with encryption support. ');
end;

{$IFNDEF CLR}
function GetProc(const Name: string; NotLinkPtr: IntPtr): IntPtr; overload;
begin
{$IFDEF MSWINDOWS}
  Result := GetProcAddress(hLiteLib, PChar(Name));
{$ENDIF}
{$IFDEF LINUX}
  Result := dlsym(hLiteLib, PChar(Name));
{$ENDIF}
  if Result = nil then
    Result := NotLinkPtr;
end;

function GetProc(const Name: string): IntPtr; overload;
begin
  Result := GetProc(Name, @NotLink);
end;
{$ENDIF}

procedure InitFunctions;
begin
{$IFDEF CLR}
  sqlite3_open := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_open;
  sqlite3_open16 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_open16;
  sqlite3_open_v2 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_open_v2;
  sqlite3_close := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_close;
  sqlite3_errcode := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_errcode;
  sqlite3_errmsg := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_errmsg;
  sqlite3_last_insert_rowid := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_last_insert_rowid;
  sqlite3_changes := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_changes;
  sqlite3_prepare_v2 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_prepare_v2;
  sqlite3_step := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_step;
  sqlite3_reset := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_reset;
  sqlite3_finalize := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_finalize;
  sqlite3_column_count := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_count;
  sqlite3_column_type := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_type;
  sqlite3_column_name := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_name;
  sqlite3_column_origin_name := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_origin_name;
  sqlite3_column_table_name := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_table_name;
  sqlite3_column_database_name := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_database_name;
  sqlite3_column_decltype := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_decltype;
  sqlite3_column_blob := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_blob;
  sqlite3_column_bytes := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_bytes;
  sqlite3_column_double := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_double;
  sqlite3_column_int := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_int;
  sqlite3_column_int64 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_int64;
  sqlite3_column_text := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_column_text;
  sqlite3_bind_parameter_count := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_bind_parameter_count;
  sqlite3_bind_blob := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_bind_blob;
  sqlite3_bind_double := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_bind_double;
  sqlite3_bind_int := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_bind_int;
  sqlite3_bind_int64 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_bind_int64;
  sqlite3_bind_null := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_bind_null;
  sqlite3_bind_text := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_bind_text;
  sqlite3_result_blob := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_blob;
  sqlite3_result_double := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_double;
  sqlite3_result_error := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_error;
  sqlite3_result_error16 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_error16;
  sqlite3_result_error_toobig := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_error_toobig;
  sqlite3_result_error_nomem := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_error_nomem;
  sqlite3_result_error_code := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_error_code;
  sqlite3_result_int := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_int;
  sqlite3_result_int64 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_int64;
  sqlite3_result_null := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_null;
  sqlite3_result_text := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_text;
  sqlite3_result_text16 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_text16;
  sqlite3_result_text16le := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_text16le;
  sqlite3_result_text16be := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_text16be;
  sqlite3_result_value := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_value;
  sqlite3_result_zeroblob := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_result_zeroblob;
  sqlite3_value_blob := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_blob;
  sqlite3_value_bytes := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_bytes;
  sqlite3_value_bytes16 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_bytes16;
  sqlite3_value_double := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_double;
  sqlite3_value_int := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_int;
  sqlite3_value_int64 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_int64;
  sqlite3_value_text := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_text;
  sqlite3_value_text16 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_text16;
  sqlite3_value_type := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_value_type;
  sqlite3_user_data := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_user_data;
  sqlite3_libversion := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_libversion;
  sqlite3_libversion_number := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_libversion_number;
  sqlite3_create_collation := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_create_collation;
  sqlite3_create_collation16 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_create_collation16;
  sqlite3_create_function := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_create_function;
  sqlite3_create_function16 := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_create_function16;
  sqlite3_enable_shared_cache := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_enable_shared_cache;
  sqlite3_busy_timeout := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_busy_timeout;
  sqlite3_busy_handler := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_busy_handler;
  sqlite3_key := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_key;
  sqlite3_rekey := {$IFNDEF UNIDACPRO}LiteCallCLR{$ELSE}LiteCallCLRUni{$ENDIF}.sqlite3_rekey;
{$ELSE}
  sqlite3_open := GetProc('sqlite3_open');
  sqlite3_open16 := GetProc('sqlite3_open16');
  sqlite3_open_v2 := GetProc('sqlite3_open_v2');
  sqlite3_close := GetProc('sqlite3_close');
  sqlite3_errcode := GetProc('sqlite3_errcode');
  sqlite3_errmsg := GetProc('sqlite3_errmsg');
  sqlite3_last_insert_rowid := GetProc('sqlite3_last_insert_rowid');
  sqlite3_changes := GetProc('sqlite3_changes');
  sqlite3_prepare_v2 := GetProc('sqlite3_prepare_v2');
  sqlite3_step := GetProc('sqlite3_step');
  sqlite3_reset := GetProc('sqlite3_reset');
  sqlite3_finalize := GetProc('sqlite3_finalize');
  sqlite3_column_count := GetProc('sqlite3_column_count');
  sqlite3_column_type := GetProc('sqlite3_column_type');
  sqlite3_column_name := GetProc('sqlite3_column_name');
  sqlite3_column_origin_name := GetProc('sqlite3_column_origin_name');
  sqlite3_column_table_name := GetProc('sqlite3_column_table_name');
  sqlite3_column_database_name := GetProc('sqlite3_column_database_name');
  sqlite3_column_decltype := GetProc('sqlite3_column_decltype');
  sqlite3_column_blob := GetProc('sqlite3_column_blob');
  sqlite3_column_bytes := GetProc('sqlite3_column_bytes');
  sqlite3_column_double := GetProc('sqlite3_column_double');
  sqlite3_column_int := GetProc('sqlite3_column_int');
  sqlite3_column_int64 := GetProc('sqlite3_column_int64');
  sqlite3_column_text := GetProc('sqlite3_column_text');
  sqlite3_bind_parameter_count := GetProc('sqlite3_bind_parameter_count');
  sqlite3_bind_blob := GetProc('sqlite3_bind_blob');
  sqlite3_bind_double := GetProc('sqlite3_bind_double');
  sqlite3_bind_int := GetProc('sqlite3_bind_int');
  sqlite3_bind_int64 := GetProc('sqlite3_bind_int64');
  sqlite3_bind_null := GetProc('sqlite3_bind_null');
  sqlite3_bind_text := GetProc('sqlite3_bind_text');
  sqlite3_result_blob := GetProc('sqlite3_result_blob');
  sqlite3_result_double := GetProc('sqlite3_result_double');
  sqlite3_result_error := GetProc('sqlite3_result_error');
  sqlite3_result_error16 := GetProc('sqlite3_result_error16');
  sqlite3_result_error_toobig := GetProc('sqlite3_result_error_toobig');
  sqlite3_result_error_nomem := GetProc('sqlite3_result_error_nomem');
  sqlite3_result_error_code := GetProc('sqlite3_result_error_code');
  sqlite3_result_int := GetProc('sqlite3_result_int');
  sqlite3_result_int64 := GetProc('sqlite3_result_int64');
  sqlite3_result_null := GetProc('sqlite3_result_null');
  sqlite3_result_text := GetProc('sqlite3_result_text');
  sqlite3_result_text16 := GetProc('sqlite3_result_text16');
  sqlite3_result_text16le := GetProc('sqlite3_result_text16le');
  sqlite3_result_text16be := GetProc('sqlite3_result_text16be');
  sqlite3_result_value := GetProc('sqlite3_result_value');
  sqlite3_result_zeroblob := GetProc('sqlite3_result_zeroblob');
  sqlite3_value_blob := GetProc('sqlite3_value_blob');;
  sqlite3_value_bytes := GetProc('sqlite3_value_bytes');
  sqlite3_value_bytes16 := GetProc('sqlite3_value_bytes16');
  sqlite3_value_double := GetProc('sqlite3_value_double');
  sqlite3_value_int := GetProc('sqlite3_value_int');
  sqlite3_value_int64 := GetProc('sqlite3_value_int64');
  sqlite3_value_text := GetProc('sqlite3_value_text');
  sqlite3_value_text16 := GetProc('sqlite3_value_text16');
  sqlite3_value_type := GetProc('sqlite3_value_type');
  sqlite3_user_data := GetProc('sqlite3_user_data');
  sqlite3_libversion := GetProc('sqlite3_libversion');
  sqlite3_libversion_number := GetProc('sqlite3_libversion_number');
  sqlite3_create_collation := GetProc('sqlite3_create_collation');
  sqlite3_create_collation16 := GetProc('sqlite3_create_collation16');
  sqlite3_create_function := GetProc('sqlite3_create_function');
  sqlite3_create_function16 := GetProc('sqlite3_create_function16');
  sqlite3_enable_shared_cache := GetProc('sqlite3_enable_shared_cache');
  sqlite3_busy_timeout := GetProc('sqlite3_busy_timeout');
  sqlite3_busy_handler := GetProc('sqlite3_busy_handler');
  sqlite3_key := GetProc('sqlite3_key', @NotLinkEncryption);
  sqlite3_rekey := GetProc('sqlite3_rekey', @NotLinkEncryption);
{$ENDIF}
end;

function SQLiteInited: boolean;
begin
  Result := LiteInited;
end;

procedure InitSQLite;
begin
  LockInit.Enter;
  try
    if LiteInited then
      exit;

  {$IFNDEF CLR}
    if SQLiteDLL = '' then
      SQLiteDLL := SQLiteDLLName;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    hLiteLib := LoadLibraryEx(PChar(SQLiteDLL), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  {$ENDIF}
  {$IFDEF LINUX}
    hLiteLib := dlopen(PChar(SQLiteDLL), RTLD_LAZY);
  {$ENDIF}

    if NativeUInt(hLiteLib) = 0 then
      raise Exception.Create('Cannot load client DLL: ' + SQLiteDLL);

    InitFunctions;

    LiteInited := True;
  finally
    LockInit.Leave;
  end;
end;

procedure FreeSQLite;
begin
  LockInit.Enter;
  try
    if not LiteInited then
      exit;

    LiteInited := False;

  {$IFDEF MSWINDOWS}
    FreeLibrary(hLiteLib);
    hLiteLib := 0;
  {$ENDIF}
  {$IFDEF LINUX}
    hLiteLib := nil;
  {$ENDIF}

  finally
    LockInit.Leave;
  end;
end;

procedure GetLiteErrorCode(pDb: Tsqlite3; var ErrorCode: integer);
begin
  ErrorCode := sqlite3_errcode(pDb);
end;

procedure GetLiteErrorMsg(pDb: Tsqlite3; var ErrorMsg: _string);
begin
  ErrorMsg := _string(AnsiString(sqlite3_errmsg(pDb)));
end;

procedure GetPredefinedErrorMsg(ErrorCode: integer; var ErrorMsg: _string);
begin
  case ErrorCode of
    SQLITE_OK:         ErrorMsg := 'not an error';
    SQLITE_ERROR:      ErrorMsg := 'SQL error or missing database';
    SQLITE_INTERNAL:   ErrorMsg := 'Internal logic error in SQLite';
    SQLITE_PERM:       ErrorMsg := 'Access permission denied';
    SQLITE_ABORT:      ErrorMsg := 'Callback routine requested an abort';
    SQLITE_BUSY:       ErrorMsg := 'The database file is locked';
    SQLITE_LOCKED:     ErrorMsg := 'A table in the database is locked';
    SQLITE_NOMEM:      ErrorMsg := 'A malloc() failed';
    SQLITE_READONLY:   ErrorMsg := 'Attempt to write a readonly database';
    SQLITE_INTERRUPT:  ErrorMsg := 'Operation terminated by sqlite3_interrupt()';
    SQLITE_IOERR:      ErrorMsg := 'Some kind of disk I/O error occurred';
    SQLITE_CORRUPT:    ErrorMsg := 'The database disk image is malformed';
    SQLITE_NOTFOUND:   ErrorMsg := 'NOT USED. Table or record not found';
    SQLITE_FULL:       ErrorMsg := 'Insertion failed because database is full';
    SQLITE_CANTOPEN:   ErrorMsg := 'Unable to open the database file';
    SQLITE_PROTOCOL:   ErrorMsg := 'NOT USED. Database lock protocol error';
    SQLITE_EMPTY:      ErrorMsg := 'Database is empty ';
    SQLITE_SCHEMA:     ErrorMsg := 'The database schema changed';
    SQLITE_TOOBIG:     ErrorMsg := 'String or BLOB exceeds size limit';
    SQLITE_CONSTRAINT: ErrorMsg := 'Abort due to constraint violation';
    SQLITE_MISMATCH:   ErrorMsg := 'Data type mismatch';
    SQLITE_MISUSE:     ErrorMsg := 'Library used incorrectly';
    SQLITE_NOLFS:      ErrorMsg := 'Uses OS features not supported on host';
    SQLITE_AUTH:       ErrorMsg := 'Authorization denied';
    SQLITE_FORMAT:     ErrorMsg := 'Auxiliary database format error';
    SQLITE_RANGE:      ErrorMsg := '2nd parameter to sqlite3_bind out of range';
    SQLITE_NOTADB:     ErrorMsg := 'File opened that is not a database file';
    SQLITE_ROW:        ErrorMsg := 'sqlite3_step() has another row ready';
    SQLITE_DONE:       ErrorMsg := 'sqlite3_step() has finished executing';
  else
    ErrorMsg := 'Unknown error';
  end;
end;

function IsMetaDataAPIAvailable: boolean;
begin
  Result := (@sqlite3_column_origin_name <> @NotLink) and
            (@sqlite3_column_table_name <> @NotLink) and
            (@sqlite3_column_database_name <> @NotLink);
end;

initialization
  LockInit := TCriticalSection.Create;

{$IFNDEF CLR}
  sqlite3_open := @NotLink;
  sqlite3_open16 := @NotLink;
  sqlite3_open_v2 := @NotLink;
  sqlite3_close := @NotLink;
  sqlite3_errcode := @NotLink;
  sqlite3_errmsg := @NotLink;
  sqlite3_last_insert_rowid := @NotLink;
  sqlite3_changes := @NotLink;
  sqlite3_prepare_v2 := @NotLink;
  sqlite3_step := @NotLink;
  sqlite3_reset := @NotLink;
  sqlite3_finalize := @NotLink;
  sqlite3_column_count := @NotLink;
  sqlite3_column_type := @NotLink;
  sqlite3_column_name := @NotLink;
  sqlite3_column_origin_name := @NotLink;
  sqlite3_column_table_name := @NotLink;
  sqlite3_column_database_name := @NotLink;
  sqlite3_column_decltype := @NotLink;
  sqlite3_column_blob := @NotLink;
  sqlite3_column_bytes := @NotLink;
  sqlite3_column_double := @NotLink;
  sqlite3_column_int := @NotLink;
  sqlite3_column_int64 := @NotLink;
  sqlite3_column_text := @NotLink;
  sqlite3_bind_parameter_count := @NotLink;
  sqlite3_bind_blob := @NotLink;
  sqlite3_bind_double := @NotLink;
  sqlite3_bind_int := @NotLink;
  sqlite3_bind_int64 := @NotLink;
  sqlite3_bind_null := @NotLink;
  sqlite3_bind_text := @NotLink;
  sqlite3_result_blob := @NotLink;
  sqlite3_result_double := @NotLink;
  sqlite3_result_error := @NotLink;
  sqlite3_result_error16 := @NotLink;
  sqlite3_result_error_toobig := @NotLink;
  sqlite3_result_error_nomem := @NotLink;
  sqlite3_result_error_code := @NotLink;
  sqlite3_result_int := @NotLink;
  sqlite3_result_int64 := @NotLink;
  sqlite3_result_null := @NotLink;
  sqlite3_result_text := @NotLink;
  sqlite3_result_text16 := @NotLink;
  sqlite3_result_text16le := @NotLink;
  sqlite3_result_text16be := @NotLink;
  sqlite3_result_value := @NotLink;
  sqlite3_result_zeroblob := @NotLink;
  sqlite3_value_blob := @NotLink;
  sqlite3_value_bytes := @NotLink;
  sqlite3_value_bytes16 := @NotLink;
  sqlite3_value_double := @NotLink;
  sqlite3_value_int := @NotLink;
  sqlite3_value_int64 := @NotLink;
  sqlite3_value_text := @NotLink;
  sqlite3_value_text16 := @NotLink;
  sqlite3_value_type := @NotLink;
  sqlite3_user_data := @NotLink;
  sqlite3_libversion := @NotLink;
  sqlite3_libversion_number := @NotLink;
  sqlite3_create_collation := @NotLink;
  sqlite3_create_collation16 := @NotLink;
  sqlite3_create_function := @NotLink;
  sqlite3_create_function16 := @NotLink;
  sqlite3_enable_shared_cache := @NotLink;
  sqlite3_busy_timeout := @NotLink;
  sqlite3_busy_handler := @NotLink;
  sqlite3_key := @NotLinkEncryption;
  sqlite3_rekey := @NotLinkEncryption;
{$ENDIF}

finalization
{$IFNDEF CLR}
  FreeSQLite;
{$ENDIF}
  LockInit.Free;

end.

