{$IFNDEF UNIDACPRO}
{$I ..\SQLiteDac.inc}
unit Devart.SQLiteDac.LiteCallCLR;
{$ENDIF}

interface

uses
  System.Runtime.InteropServices, System.Text,
  MemUtils, {$IFNDEF UNIDACPRO}LiteCall{$ELSE}LiteCallUni{$ENDIF};


  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_open(
    filename: PAnsiChar;    // Database filename (UTF-8)
    out ppDb: Tsqlite3      // OUT: SQLite db handle
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_open16(
    filename: PWideChar;    // Database filename (UTF-16)
    out ppDb: Tsqlite3      // OUT: SQLite db handle
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_open_v2(
    filename: PAnsiChar;    // Database filename (UTF-8)
    out ppDb: Tsqlite3;     // OUT: SQLite db handle
    flags: Integer;         // Flags
    zVfs: PAnsiChar         // Name of VFS module to use
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_close(
    pDb: Tsqlite3
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_errcode(
    pDb: Tsqlite3
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_errmsg(
    pDb: Tsqlite3
  ): PAnsiChar; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_last_insert_rowid(
    pDb: Tsqlite3
  ): Int64; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_changes(
    pDb: Tsqlite3
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_prepare_v2(
    db: Tsqlite3;           // Database handle
    zSql: PAnsiChar;        // SQL statement, UTF-8 encoded
    nByte: Integer;         // Maximum length of zSql in bytes.
    out ppStmt: Tsqlite3_stmt;  // OUT: Statement handle
    pzTail: IntPtr          // OUT: Pointer to unused portion of zSql
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_step(
    pStmt: Tsqlite3_stmt
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_reset(
    pStmt: Tsqlite3_stmt
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_finalize(
    pStmt: Tsqlite3_stmt
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_count(
    pStmt: Tsqlite3_stmt
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_type(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_name(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_origin_name(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_table_name(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_database_name(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_decltype(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_blob(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): IntPtr; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_bytes(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_double(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): double; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_int(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_int64(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): Int64; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_column_text(
    pStmt: Tsqlite3_stmt; iCol: Integer
  ): PAnsiChar; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_bind_parameter_count(
    pStmt: Tsqlite3_stmt
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_bind_blob(
    pStmt: Tsqlite3_stmt; Index: Integer; pBlob: IntPtr; size: Integer; pDestrType: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_bind_double(
    pStmt: Tsqlite3_stmt; Index: Integer; Value: double
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_bind_int(
    pStmt: Tsqlite3_stmt; Index: Integer; Value: Integer
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_bind_int64(
    pStmt: Tsqlite3_stmt; Index: Integer; Value: Int64
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_bind_null(
    pStmt: Tsqlite3_stmt; Index: Integer
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_bind_text(
    pStmt: Tsqlite3_stmt; Index: Integer; Value: PAnsiChar; size: Integer; pDestrType: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_blob(
    pContext: Tsqlite3_context; pBlob: IntPtr; Size: Integer; pDestrType: IntPtr
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_double(
    pContext: Tsqlite3_context; Value: Double
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_error(
    pContext: Tsqlite3_context; pMsg: IntPtr; Size: Integer
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_error16(
    pContext: Tsqlite3_context; pMsg: IntPtr; Size: Integer
  );external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_error_toobig(
    pContext: Tsqlite3_context
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_error_nomem(
    pContext: Tsqlite3_context
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_error_code(
    pContext: Tsqlite3_context; ErrorCode: Integer
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_int(
    pContext: Tsqlite3_context; Value: Integer
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_int64(
    pContext: Tsqlite3_context; Value: Int64
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_null(
    pContext: Tsqlite3_context
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_text(
    pContext: Tsqlite3_context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_text16(
    pContext: Tsqlite3_context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_text16le(
    pContext: Tsqlite3_context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_text16be(
    pContext: Tsqlite3_context; pStr: IntPtr; Size: Integer; pDestrType: IntPtr
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_value(
    pContext: Tsqlite3_context; Value: Tsqlite3_value
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  procedure sqlite3_result_zeroblob(
    pContext: Tsqlite3_context; Size: Integer
  ); external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_blob (
    pValue: IntPtr
  ): IntPtr; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_bytes (
    pValue: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_bytes16 (
    pValue: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_double (
    pValue: IntPtr
  ): Double; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_int (
    pValue: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_int64 (
    pValue: IntPtr
  ): Int64; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_text (
    pValue: IntPtr
  ): IntPtr; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_text16 (
    pValue: IntPtr
  ): IntPtr; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_value_type (
    pValue: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_user_data (
    pContext: Tsqlite3_context
  ): IntPtr; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_libversion(
  ): PAnsiChar; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_libversion_number(
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_create_collation(
    pSQLite: Tsqlite3; zName: PAnsiChar; eTextRep: Integer; userData: IntPtr; func: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_create_collation16(
    pSQLite: Tsqlite3; zName: PWideChar; eTextRep: Integer; userData: IntPtr; func: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_create_function(
    pSQLite: Tsqlite3; zFunctionName: PAnsiChar; nArg: Integer; eTextRep: Integer; pApp: IntPtr; xFunc: IntPtr; xStep: IntPtr; xFinal: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_create_function16(
    pSQLite: Tsqlite3; zFunctionName: PWideChar; nArg: Integer; eTextRep: Integer; pApp: IntPtr; xFunc: IntPtr; xStep: IntPtr; xFinal: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_enable_shared_cache(
    Value: Integer
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_busy_timeout(
    pSQLite: Tsqlite3; MilliSeconds: Integer
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_busy_handler(
    pSQLite: Tsqlite3; func: IntPtr; userData: IntPtr
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_key(
    pSQLite: Tsqlite3; Key: PAnsiChar; size: Integer
  ): Integer; external;

  [DllImport(SQLiteDLL, CharSet = CharSet.Ansi, SetLastError = True, CallingConvention=CallingConvention.Cdecl)]
  function sqlite3_rekey(
    pSQLite: Tsqlite3; NewKey: PAnsiChar; size: Integer
  ): Integer; external;

implementation

end.
