
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ODBCDac.inc}
unit ODBCCallUni;
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
  MemUtils,
{$IFNDEF UNIDACPRO}
  ODBCError;
{$ELSE}
  ODBCErrorUni;
{$ENDIF}

const
  ODBCDLLName = 'odbc32.dll';

{$IFNDEF CLR}
var
  ODBCDLL: string;
{$ELSE}
const
  ODBCDLL = ODBCDLLName;
{$ENDIF}

type
  TSQLHandle     = IntPtr;
  TSQLHEnv       = IntPtr;
  TSQLHDbc       = IntPtr;
  TSQLHStmt      = IntPtr;
  TSQLHDesc      = IntPtr;

  TSQLDateStruct = packed record
    Year:  smallint;
    Month: word;
    Day:   word;
  end;
  SQL_DATE_STRUCT = TSQLDateStruct;

  TSQLTimeStruct = packed record
    Hour:   word;
    Minute: word;
    Second: word;
  end;
  SQL_TIME_STRUCT = TSQLTimeStruct;

  TSQLTimeStampStruct = packed record
    Year:     smallint;
    Month:    word;
    Day:      word;
    Hour:     word;
    Minute:   word;
    Second:   word;
    Fraction: longword;
  end;
  SQL_TIMESTAMP_STRUCT = TSQLTimeStampStruct;

  TSQLYearMonth = packed record
    Year:  longword;
    Month: longword;
  end;
  SQL_YEAR_MONTH = TSQLYearMonth;

  TSQLDaySecond = packed record
    Day:      longword;
    Hour:     longword;
    Minute:   longword;
    Second:   longword;
    Fraction: longword;
  end;
  SQL_DAY_SECOND = TSQLDaySecond;

  SQLInterval = (
    SQL_IS_DUMMY,
    SQL_IS_YEAR,
    SQL_IS_MONTH,
    SQL_IS_DAY,
    SQL_IS_HOUR,
    SQL_IS_MINUTE,
    SQL_IS_SECOND,
    SQL_IS_YEAR_TO_MONTH,
    SQL_IS_DAY_TO_HOUR,
    SQL_IS_DAY_TO_MINUTE,
    SQL_IS_DAY_TO_SECOND,
    SQL_IS_HOUR_TO_MINUTE,
    SQL_IS_HOUR_TO_SECOND,
    SQL_IS_MINUTE_TO_SECOND);

  TSQLInterval = packed record
    Interval_type: SQLInterval;
    Interval_sign: smallint;
    case SQLInterval of
      SQL_IS_YEAR_TO_MONTH: (YearMonth: TSQLYearMonth);
      SQL_IS_DAY_TO_SECOND: (DaySecond: TSQLDaySecond);
  end;

const
  SQL_MAX_NUMERIC_LEN	= 16;

type
  TSQLNumericStruct = packed record
    Precision: byte;
    Scale: shortint;
    Sign: byte;
  {$IFNDEF CLR}
    case Integer of
      0: (
        Val: array [0 .. SQL_MAX_NUMERIC_LEN - 1] of byte;
      );
      1: (
        ValLow: int64;
        ValHigh: int64;
      );
  {$ELSE}
    ValLow: int64;
    ValHigh: int64;
  {$ENDIF}
  end;

const
  // Length indicator values
  SQL_NULL_DATA    = -1;
  SQL_DATA_AT_EXEC = -2;

  // SQLReturn values from functions
  SQL_SUCCESS           = 0;
  SQL_SUCCESS_WITH_INFO = 1;
  SQL_NO_DATA           = 100;
  SQL_ERROR             = -1;
  SQL_INVALID_HANDLE    = -2;
  SQL_STILL_EXECUTING   = 2;
  SQL_NEED_DATA         = 99;

  //function SQL_LEN_BINARY_ATTR(Length: integer): integer;
  //function SQL_LEN_DATA_AT_EXEC(Length: integer): integer;

const
  SQL_INTERVAL_COLSIZE = 28;

  // Flags for null-terminated string
  SQL_NTS  = -3;
  SQL_NTSL = -3;

  // Maximum message Length
  SQL_MAX_MESSAGE_LENGTH = 512;

  // Date/time Length constants
  SQL_DATE_LEN      = 10;
  SQL_TIME_LEN      = 8;  // Add P+1 if precision is nonzero
  SQL_TIMESTAMP_LEN = 19; // Add P+1 if precision is nonzero

  // Handle type identifiers
  SQL_HANDLE_ENV  = 1;
  SQL_HANDLE_DBC  = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

  // Environment attribute
  SQL_ATTR_OUTPUT_NTS = 10001;

  // Connection attributes
  SQL_ATTR_AUTO_IPD    = 10001;
  SQL_ATTR_METADATA_ID = 10014;

  // For IBM DB2
  SQL_ATTR_LONGDATA_COMPAT = 1253;
  SQL_LD_COMPAT_NO = 0;
  SQL_LD_COMPAT_YES = 1;

  // Statement attributes
  SQL_ATTR_APP_ROW_DESC       = 10010;
  SQL_ATTR_APP_PARAM_DESC     = 10011;
  SQL_ATTR_IMP_ROW_DESC       = 10012;
  SQL_ATTR_IMP_PARAM_DESC     = 10013;
  SQL_ATTR_CURSOR_SCROLLABLE  = -1;
  SQL_ATTR_CURSOR_SENSITIVITY = -2;

  // SQL_ATTR_CURSOR_SCROLLABLE values
  SQL_NONSCROLLABLE = 0;
  SQL_SCROLLABLE    = 1;

  // Identifiers of fields in the SQL descriptor
  SQL_DESC_COUNT                  = 1001;
  SQL_DESC_TYPE                   = 1002;
  SQL_DESC_LENGTH                 = 1003;
  SQL_DESC_OCTET_LENGTH_PTR       = 1004;
  SQL_DESC_PRECISION              = 1005;
  SQL_DESC_SCALE                  = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE               = 1008;
  SQL_DESC_INDICATOR_PTR          = 1009;
  SQL_DESC_DATA_PTR               = 1010;
  SQL_DESC_NAME                   = 1011;
  SQL_DESC_UNNAMED                = 1012;
  SQL_DESC_OCTET_LENGTH           = 1013;
  SQL_DESC_ALLOC_TYPE             = 1099;

  // Identifiers of fields in the diagnostics area
  SQL_DIAG_RETURNCODE            = 1;
  SQL_DIAG_NUMBER                = 2;
  SQL_DIAG_ROW_COUNT             = 3;
  SQL_DIAG_SQLSTATE              = 4;
  SQL_DIAG_NATIVE                = 5;
  SQL_DIAG_MESSAGE_TEXT          = 6;
  SQL_DIAG_DYNAMIC_FUNCTION      = 7;
  SQL_DIAG_CLASS_ORIGIN          = 8;
  SQL_DIAG_SUBCLASS_ORIGIN       = 9;
  SQL_DIAG_CONNECTION_NAME       = 10;
  SQL_DIAG_SERVER_NAME           = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

  // Dynamic function codes
  SQL_DIAG_ALTER_DOMAIN          = 3;
  SQL_DIAG_ALTER_TABLE           = 4;
  SQL_DIAG_CALL                  = 7;
  SQL_DIAG_CREATE_ASSERTION      = 6;
  SQL_DIAG_CREATE_CHARACTER_SET  = 8;
  SQL_DIAG_CREATE_COLLATION      = 10;
  SQL_DIAG_CREATE_DOMAIN         = 23;
  SQL_DIAG_CREATE_INDEX          = -1;
  SQL_DIAG_CREATE_SCHEMA         = 64;
  SQL_DIAG_CREATE_TABLE          = 77;
  SQL_DIAG_CREATE_TRANSLATION    = 79;
  SQL_DIAG_CREATE_VIEW           = 84;
  SQL_DIAG_DELETE_WHERE          = 19;
  SQL_DIAG_DROP_ASSERTION        = 24;
  SQL_DIAG_DROP_CHARACTER_SET    = 25;
  SQL_DIAG_DROP_COLLATION        = 26;
  SQL_DIAG_DROP_DOMAIN           = 27;
  SQL_DIAG_DROP_INDEX            = -2;
  SQL_DIAG_DROP_SCHEMA           = 31;
  SQL_DIAG_DROP_TABLE            = 32;
  SQL_DIAG_DROP_TRANSLATION      = 33;
  SQL_DIAG_DROP_VIEW             = 36;
  SQL_DIAG_DYNAMIC_DELETE_CURSOR = 38;
  SQL_DIAG_DYNAMIC_UPDATE_CURSOR = 81;
  SQL_DIAG_GRANT                 = 48;
  SQL_DIAG_INSERT                = 50;
  SQL_DIAG_REVOKE                = 59;
  SQL_DIAG_SELECT_CURSOR         = 85;
  SQL_DIAG_UNKNOWN_STATEMENT     = 0;
  SQL_DIAG_UPDATE_WHERE          = 82;

  // SQL data type codes
  SQL_UNKNOWN_TYPE = 0;
  SQL_CHAR         = 1;
  SQL_NUMERIC      = 2;
  SQL_DECIMAL      = 3;
  SQL_INTEGER      = 4;
  SQL_SMALLINT     = 5;
  SQL_FLOAT        = 6;
  SQL_REAL         = 7;
  SQL_DOUBLE       = 8;
  SQL_DATETIME     = 9;
  SQL_VARCHAR      = 12;

  // One-parameter shortcuts for date/time data types
  SQL_TYPE_DATE      = 91;
  SQL_TYPE_TIME      = 92;
  SQL_TYPE_TIMESTAMP = 93;

  // Statement attribute values for cursor sensitivity
  SQL_UNSPECIFIED = 0;
  SQL_INSENSITIVE = 1;
  SQL_SENSITIVE   = 2;

  // GetTypeInfo() request for all data types
  SQL_ALL_TYPES = 0;

  // Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData()
  SQL_DEFAULT = 99;

  // SQLGetData() code indicating that the application row descriptor
  // specifies the data type
  SQL_ARD_TYPE = -99;

  // SQL date/time type subcodes
  SQL_CODE_DATE      = 1;
  SQL_CODE_TIME      = 2;
  SQL_CODE_TIMESTAMP = 3;

  // CLI option values
  SQL_FALSE = 0;
  SQL_TRUE  = 1;

  // Values of NULLABLE field in descriptor
  SQL_NO_NULLS = 0;
  SQL_NULLABLE = 1;

  // Value returned by SQLGetTypeInfo() to denote that it is
  // not known whether or not a data type supports null values.
  SQL_NULLABLE_UNKNOWN = 2;

  // Values returned by SQLGetTypeInfo() to show WHERE clause supported
  SQL_PRED_NONE  = 0;
  SQL_PRED_CHAR  = 1;
  SQL_PRED_BASIC = 2;

  // values of UNNAMED field in descriptor
  SQL_NAMED = 0;
  SQL_UNNAMED = 1;

  // values of ALLOC_TYPE field in descriptor
  SQL_DESC_ALLOC_AUTO = 1;
  SQL_DESC_ALLOC_USER = 2;

  // FreeStmt() options
  SQL_CLOSE        = 0;
  SQL_DROP         = 1;
  SQL_UNBIND       = 2;
  SQL_RESET_PARAMS = 3;

  // Codes used for FetchOrientation in SQLFetchScroll(), and in SQLDataSources()
  SQL_FETCH_NEXT  = 1;
  SQL_FETCH_FIRST = 2;

  // Other codes used for FetchOrientation in SQLFetchScroll()
  SQL_FETCH_LAST     = 3;
  SQL_FETCH_PRIOR    = 4;
  SQL_FETCH_ABSOLUTE = 5;
  SQL_FETCH_RELATIVE = 6;

  // SQLEndTran() options
  SQL_COMMIT   = 0;
  SQL_ROLLBACK = 1;

  // Null handles returned by SQLAllocHandle()
  SQL_NULL_HENV  = nil;//TSQLHandle(0);
  SQL_NULL_HDBC  = nil;//TSQLHandle(0);
  SQL_NULL_HSTMT = nil;//TSQLHandle(0);
  SQL_NULL_HDESC = nil;//TSQLHandle(0);

  // Null handle used in place of parent handle when allocating HENV
  SQL_NULL_HANDLE = nil;//TSQLHandle(0);

  // Values that may appear in the Result set of SQLSpecialColumns()
  SQL_SCOPE_CURROW      = 0;
  SQL_SCOPE_TRANSACTION = 1;
  SQL_SCOPE_SESSION     = 2;
  SQL_PC_UNKNOWN        = 0;
  SQL_PC_NON_PSEUDO     = 1;
  SQL_PC_PSEUDO         = 2;

  // Reserved value for the IdentifierType argument of SQLSpecialColumns()
  SQL_ROW_IDENTIFIER = 1;

  // Reserved values for UNIQUE argument of SQLStatistics()
  SQL_INDEX_UNIQUE = 0;
  SQL_INDEX_ALL    = 1;

  // Values that may appear in the Result set of SQLStatistics()
  SQL_INDEX_CLUSTERED = 1;
  SQL_INDEX_HASHED    = 2;
  SQL_INDEX_OTHER     = 3;

  // SQLGetFunctions() values to identify ODBC APIs
  SQL_API_SQLALLOCCONNECT     = 1;
  SQL_API_SQLALLOCENV         = 2;
  SQL_API_SQLALLOCHANDLE      = 1001;
  SQL_API_SQLALLOCSTMT        = 3;
  SQL_API_SQLBINDCOL          = 4;
  SQL_API_SQLBINDPARAM        = 1002;
  SQL_API_SQLCANCEL           = 5;
  SQL_API_SQLCLOSECURSOR      = 1003;
  SQL_API_SQLCOLATTRIBUTE     = 6;
  SQL_API_SQLCOLUMNS          = 40;
  SQL_API_SQLCONNECT          = 7;
  SQL_API_SQLCOPYDESC         = 1004;
  SQL_API_SQLDATASOURCES      = 57;
  SQL_API_SQLDESCRIBECOL      = 8;
  SQL_API_SQLDISCONNECT       = 9;
  SQL_API_SQLENDTRAN          = 1005;
  SQL_API_SQLERROR            = 10;
  SQL_API_SQLEXECDIRECT       = 11;
  SQL_API_SQLEXECUTE          = 12;
  SQL_API_SQLFETCH            = 13;
  SQL_API_SQLFETCHSCROLL      = 1021;
  SQL_API_SQLFREECONNECT      = 14;
  SQL_API_SQLFREEENV          = 15;
  SQL_API_SQLFREEHANDLE       = 1006;
  SQL_API_SQLFREESTMT         = 16;
  SQL_API_SQLGETCONNECTATTR   = 1007;
  SQL_API_SQLGETCONNECTOPTION = 42;
  SQL_API_SQLGETCURSORNAME    = 17;
  SQL_API_SQLGETDATA          = 43;
  SQL_API_SQLGETADSCFIELD     = 1008;
  SQL_API_SQLGETADSCREC       = 1009;
  SQL_API_SQLGETDIAGFIELD     = 1010;
  SQL_API_SQLGETDIAGREC       = 1011;
  SQL_API_SQLGETENVATTR       = 1012;
  SQL_API_SQLGETFUNCTIONS     = 44;
  SQL_API_SQLGETINFO          = 45;
  SQL_API_SQLGETSTMTATTR      = 1014;
  SQL_API_SQLGETSTMTOPTION    = 46;
  SQL_API_SQLGETTYPEINFO      = 47;
  SQL_API_SQLNUMRESULTCOLS    = 18;
  SQL_API_SQLPARAMDATA        = 48;
  SQL_API_SQLPREPARE          = 19;
  SQL_API_SQLPUTDATA          = 49;
  SQL_API_SQLROWCOUNT         = 20;
  SQL_API_SQLSETCONNECTATTR   = 1016;
  SQL_API_SQLSETCONNECTOPTION = 50;
  SQL_API_SQLSETCURSORNAME    = 21;
  SQL_API_SQLSETADSCFIELD     = 1017;
  SQL_API_SQLSETADSCREC       = 1018;
  SQL_API_SQLSETENVATTR       = 1019;
  SQL_API_SQLSETPARAM         = 22;
  SQL_API_SQLSETSTMTATTR      = 1020;
  SQL_API_SQLSETSTMTOPTION    = 51;
  SQL_API_SQLSPECIALCOLUMNS   = 52;
  SQL_API_SQLSTATISTICS       = 53;
  SQL_API_SQLTABLES           = 54;
  SQL_API_SQLTRANSACT         = 23;

  // Information requested by SQLGetInfo()
  SQL_MAX_DRIVER_CONNECTIONS        = 0;
  SQL_MAXIMUM_DRIVER_CONNECTIONS    = SQL_MAX_DRIVER_CONNECTIONS;
  SQL_MAX_CONCURRENT_ACTIVITIES     = 1;
  SQL_MAXIMUM_CONCURRENT_ACTIVITIES = SQL_MAX_CONCURRENT_ACTIVITIES;
  SQL_DATA_SOURCE_NAME              = 2;
  SQL_FETCH_DIRECTION               = 8;
  SQL_SERVER_NAME                   = 13;
  SQL_SEARCH_PATTERN_ESCAPE         = 14;
  SQL_DBMS_NAME                     = 17;
  SQL_DBMS_VER                      = 18;
  SQL_ACCESSIBLE_TABLES             = 19;
  SQL_ACCESSIBLE_PROCEDURES         = 20;
  SQL_CURSOR_COMMIT_BEHAVIOR        = 23;
  SQL_DATA_SOURCE_READ_ONLY         = 25;
  SQL_DEFAULT_TXN_ISOLATION         = 26;
  SQL_IDENTIFIER_CASE               = 28;
  SQL_IDENTIFIER_QUOTE_CHAR         = 29;
  SQL_MAX_COLUMN_NAME_LEN           = 30;
  SQL_MAXIMUM_COLUMN_NAME_LENGTH    = SQL_MAX_COLUMN_NAME_LEN;
  SQL_MAX_CURSOR_NAME_LEN           = 31;
  SQL_MAXIMUM_CURSOR_NAME_LENGTH    = SQL_MAX_CURSOR_NAME_LEN;
  SQL_MAX_SCHEMA_NAME_LEN           = 32;
  SQL_MAXIMUM_SCHEMA_NAME_LENGTH    = SQL_MAX_SCHEMA_NAME_LEN;
  SQL_MAX_CATALOG_NAME_LEN          = 34;
  SQL_MAXIMUM_CATALOG_NAME_LENGTH   = SQL_MAX_CATALOG_NAME_LEN;
  SQL_MAX_TABLE_NAME_LEN            = 35;
  SQL_SCROLL_CONCURRENCY            = 43;
  SQL_TXN_CAPABLE                   = 46;
  SQL_TRANSACTION_CAPABLE           = SQL_TXN_CAPABLE;
  SQL_USER_NAME                     = 47;
  SQL_TXN_ISOLATION_OPTION          = 72;
  SQL_TRANSACTION_ISOLATION_OPTION  = SQL_TXN_ISOLATION_OPTION;
  SQL_INTEGRITY                     = 73;
  SQL_GETDATA_EXTENSIONS            = 81;
  SQL_NULL_COLLATION                = 85;
  SQL_ALTER_TABLE                   = 86;
  SQL_ORDER_BY_COLUMNS_IN_SELECT    = 90;
  SQL_SPECIAL_CHARACTERS            = 94;
  SQL_MAX_COLUMNS_IN_GROUP_BY       = 97;
  SQL_MAXIMUM_COLUMNS_IN_GROUP_BY   = SQL_MAX_COLUMNS_IN_GROUP_BY;
  SQL_MAX_COLUMNS_IN_INDEX          = 98;
  SQL_MAXIMUM_COLUMNS_IN_INDEX      = SQL_MAX_COLUMNS_IN_INDEX;
  SQL_MAX_COLUMNS_IN_ORDER_BY       = 99;
  SQL_MAXIMUM_COLUMNS_IN_ORDER_BY   = SQL_MAX_COLUMNS_IN_ORDER_BY;
  SQL_MAX_COLUMNS_IN_SELECT         = 100;
  SQL_MAXIMUM_COLUMNS_IN_SELECT     = SQL_MAX_COLUMNS_IN_SELECT;
  SQL_MAX_COLUMNS_IN_TABLE          = 101;
  SQL_MAX_INDEX_SIZE                = 102;
  SQL_MAXIMUM_INDEX_SIZE            = SQL_MAX_INDEX_SIZE;
  SQL_MAX_ROW_SIZE                  = 104;
  SQL_MAXIMUM_ROW_SIZE              = SQL_MAX_ROW_SIZE;
  SQL_MAX_STATEMENT_LEN             = 105;
  SQL_MAXIMUM_STATEMENT_LENGTH      = SQL_MAX_STATEMENT_LEN;
  SQL_MAX_TABLES_IN_SELECT          = 106;
  SQL_MAXIMUM_TABLES_IN_SELECT      = SQL_MAX_TABLES_IN_SELECT;
  SQL_MAX_USER_NAME_LEN             = 107;
  SQL_MAXIMUM_USER_NAME_LENGTH      = SQL_MAX_USER_NAME_LEN;
  SQL_OJ_CAPABILITIES               = 115;
  SQL_OUTER_JOIN_CAPABILITIES       = SQL_OJ_CAPABILITIES;
  SQL_XOPEN_CLI_YEAR                = 10000;
  SQL_CURSOR_SENSITIVITY            = 10001;
  SQL_DESCRIBE_PARAMETER            = 10002;
  SQL_CATALOG_NAME                  = 10003;
  SQL_COLLATION_SEQ                 = 10004;
  SQL_MAX_IDENTIFIER_LEN            = 10005;
  SQL_MAXIMUM_IDENTIFIER_LENGTH     = SQL_MAX_IDENTIFIER_LEN;

  // SQL_ALTER_TABLE bitmasks
  SQL_AT_ADD_COLUMN     = $00000001;
  SQL_AT_DROP_COLUMN    = $00000002;
  SQL_AT_ADD_CONSTRAINT = $00000008;

  // The following bitmasks are ODBC extensions
  SQL_AT_COLUMN_SINGLE                  = $00000020;
  SQL_AT_ADD_COLUMN_DEFAULT             = $00000040;
  SQL_AT_ADD_COLUMN_COLLATION           = $00000080;
  SQL_AT_SET_COLUMN_DEFAULT             = $00000100;
  SQL_AT_DROP_COLUMN_DEFAULT            = $00000200;
  SQL_AT_DROP_COLUMN_CASCADE            = $00000400;
  SQL_AT_DROP_COLUMN_RESTRICT           = $00000800;
  SQL_AT_ADD_TABLE_CONSTRAINT           = $00001000;
  SQL_AT_DROP_TABLE_CONSTRAINT_CASCADE  = $00002000;
  SQL_AT_DROP_TABLE_CONSTRAINT_RESTRICT = $00004000;
  SQL_AT_CONSTRAINT_NAME_DEFINITION     = $00008000;
  SQL_AT_CONSTRAINT_INITIALLY_DEFERRED  = $00010000;
  SQL_AT_CONSTRAINT_INITIALLY_IMMEDIATE = $00020000;
  SQL_AT_CONSTRAINT_DEFERRABLE          = $00040000;
  SQL_AT_CONSTRAINT_NON_DEFERRABLE      = $00080000;

  // SQL_ASYNC_MODE values
  SQL_AM_NONE       = 0;
  SQL_AM_CONNECTION = 1;
  SQL_AM_STATEMENT  = 2;

  // SQL_CURSOR_COMMIT_BEHAVIOR values
  SQL_CB_DELETE   = 0;
  SQL_CB_CLOSE    = 1;
  SQL_CB_PRESERVE = 2;

  // SQL_FETCH_DIRECTION bitmasks
  SQL_FD_FETCH_NEXT     = $00000001;
  SQL_FD_FETCH_FIRST    = $00000002;
  SQL_FD_FETCH_LAST     = $00000004;
  SQL_FD_FETCH_PRIOR    = $00000008;
  SQL_FD_FETCH_ABSOLUTE = $00000010;
  SQL_FD_FETCH_RELATIVE = $00000020;

  // SQL_GETDATA_EXTENSIONS bitmasks
  SQL_GD_ANY_COLUMN = $00000001;
  SQL_GD_ANY_ORDER  = $00000002;

  // SQL_IDENTIFIER_CASE values
  SQL_IC_UPPER     = 1;
  SQL_IC_LOWER     = 2;
  SQL_IC_SENSITIVE = 3;
  SQL_IC_MIXED     = 4;

  // SQL_OJ_CAPABILITIES bitmasks
  // NB: this means 'outer join', not what you may be thinking
  SQL_OJ_LEFT               = $00000001;
  SQL_OJ_RIGHT              = $00000002;
  SQL_OJ_FULL               = $00000004;
  SQL_OJ_NESTED             = $00000008;
  SQL_OJ_NOT_ORDERED        = $00000010;
  SQL_OJ_INNER              = $00000020;
  SQL_OJ_ALL_COMPARISON_OPS = $00000040;

  // SQL_SCROLL_CONCURRENCY bitmasks
  SQL_SCCO_READ_ONLY  = $00000001;
  SQL_SCCO_LOCK       = $00000002;
  SQL_SCCO_OPT_ROWVER = $00000004;
  SQL_SCCO_OPT_VALUES = $00000008;

  // SQL_TXN_CAPABLE values
  SQL_TC_NONE       = 0;
  SQL_TC_DML        = 1;
  SQL_TC_ALL        = 2;
  SQL_TC_DDL_COMMIT = 3;
  SQL_TC_DDL_IGNORE = 4;

  // SQL_TXN_ISOLATION_OPTION bitmasks
  SQL_TXN_READ_UNCOMMITTED         = $00000001;
  SQL_TRANSACTION_READ_UNCOMMITTED = SQL_TXN_READ_UNCOMMITTED;
  SQL_TXN_READ_COMMITTED           = $00000002;
  SQL_TRANSACTION_READ_COMMITTED   = SQL_TXN_READ_COMMITTED;
  SQL_TXN_REPEATABLE_READ          = $00000004;
  SQL_TRANSACTION_REPEATABLE_READ  = SQL_TXN_REPEATABLE_READ;
  SQL_TXN_SERIALIZABLE             = $00000008;
  SQL_TRANSACTION_SERIALIZABLE     = SQL_TXN_SERIALIZABLE;

  // SQL_NULL_COLLATION values
  SQL_NC_HIGH = 0;
  SQL_NC_LOW  = 1;

type
  _SQLAllocHandle = function(
    HandleType:       smallint;
    InputHandle:      TSQLHandle;
    var OutputHandle: TSQLHandle
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLBindCol = function(
    StatementHandle: TSQLHStmt;
    ColumnNumber:    word;
    TargetType:      smallint;
    TargetValue:     IntPtr;
    BufferLength:    NativeInt;
    StrLen_or_Ind:   IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLBindParam = function(
    StatementHandle:   TSQLHStmt;
    ParameterNumber:   word;
    ValueType:         smallint;
    ParameterType:     smallint;
    LengthPrecision:   NativeUInt;
    ParameterScale:    smallint;
    ParameterValue:    IntPtr;
    var StrLen_or_Ind: NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLCancel = function(
    StatementHandle: TSQLHStmt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLCloseCursor = function(
    StatementHandle: TSQLHStmt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLColAttributeStr = function(
    StatementHandle:    TSQLHStmt;
    ColumnNumber:       word;
    FieldIdentifier:    word;
    CharacterAttribute: IntPtr;
    BufferLength:       smallint;
    var StringLength:   smallint;
    NumericAttribute:   IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLColAttributeInt = function(
    StatementHandle:      TSQLHStmt;
    ColumnNumber:         word;
    FieldIdentifier:      word;
    CharacterAttribute:   IntPtr;
    BufferLength:         smallint;
    var StringLength:     smallint;
    var NumericAttribute: NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLColumns = function(
    StatementHandle: TSQLHStmt;
    CatalogName:     _PChar;
    NameLength1:     smallint;
    SchemaName:      _PChar;
    NameLength2:     smallint;
    TableName:       _PChar;
    NameLength3:     smallint;
    ColumnName:      _PChar;
    NameLength4:     smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLConnect = function(
    ConnectionHandle: TSQLHDbc;
    ServerName:       _PChar;
    NameLength1:      smallint;
    UserName:         _PChar;
    NameLength2:      smallint;
    Authentication:   _PChar;
    NameLength3:      smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLCopyDesc = function(
    SourceDescHandle: TSQLHDesc;
    TargetDescHandle: TSQLHDesc
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLDataSources = function(
    EnvironmentHandle: TSQLHEnv;
    Direction:         word;
    ServerName:        IntPtr;
    BufferLength1:     smallint;
    var NameLength1:   smallint;
    Description:       IntPtr;
    BufferLength2:     smallint;
    var NameLength2:   smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLDescribeCol = function(
    StatementHandle:   TSQLHStmt;
    ColumnNumber:      word;
    ColumnName:        IntPtr;
    BufferLength:      smallint;
    var NameLength:    smallint;
    var DataType:      smallint;
    var ColumnSize:    NativeUInt;
    var DecimalDigits: smallint;
    var Nullable:      smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLDisconnect = function(
    ConnectionHandle: TSQLHDbc
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLEndTran = function(
    HandleType:     smallint;
    Handle:         TSQLHandle;
    CompletionType: smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLExecDirect = function(
    StatementHandle: TSQLHStmt;
    StatementText:   _PChar;
    TextLength:      integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLExecute = function(
    StatementHandle: TSQLHStmt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLFetch = function(
    StatementHandle: TSQLHStmt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLFetchScroll = function(
    StatementHandle:  TSQLHStmt;
    FetchOrientation: smallint;
    FetchOffset:      NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLFreeHandle = function(
    HandleType: smallint;
    Handle:     TSQLHandle
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetConnectAttrInt = function(
    ConnectionHandle: TSQLHDbc;
    Attribute:        integer;
    var ValuePtr:     integer;
    BufferLength:     integer;
    StringLength:     IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetConnectAttrStr = function(
    ConnectionHandle: TSQLHDbc;
    Attribute:        integer;
    ValuePtr:         _PChar;
    BufferLength:     integer;
    var StringLength: integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetConnectOption = function(
    ConnectionHandle: TSQLHDbc;
    Option:           word;
    Value:            IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetCursorName = function(
    StatementHandle: TSQLHStmt;
    CursorName:      IntPtr;
    BufferLength:    smallint;
    var NameLength:  smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetData = function(
    StatementHandle: TSQLHStmt;
    ColumnNumber:    word;
    TargetType:      smallint;
    TargetValue:     IntPtr;
    BufferLength:    NativeInt;
    var StrLen_or_Ind: NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetDescFieldPtr = function(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    var Value:        IntPtr;
    BufferLength:     integer;
    StringLength:     IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetDescFieldInt = function(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    var Value:        integer;
    BufferLength:     integer;
    StringLength:     IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetDescFieldStr = function(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    Value:            IntPtr;
    BufferLength:     integer;
    var StringLength: integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetDescRec = function(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    Name:             IntPtr;
    BufferLength:     smallint;
    var StringLength: smallint;
    var _Type:        smallint;
    var SubType:      smallint;
    var Length:       NativeInt;
    var Precision:    smallint;
    var Scale:        smallint;
    var Nullable:     smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetDiagField = function(
    HandleType:       smallint;
    Handle:           TSQLHandle;
    RecNumber:        smallint;
    DiagIdentifier:   smallint;
    DiagInfo:         IntPtr;
    BufferLength:     smallint;
    var StringLength: smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetDiagRec = function(
    HandleType:      smallint;
    Handle:          TSQLHandle;
    RecNumber:       smallint;
    SQLstate:        _PChar; // pointer to 5 character buffer
    var NativeError: integer;
    MessageText:     IntPtr;
    BufferLength:    smallint;
    var TextLength:  smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetEnvAttr = function(
    EnvironmentHandle: TSQLHEnv;
    Attribute:         integer;
    Value:             IntPtr;
    BufferLength:      integer;
    var StringLength:  integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetFunctions = function(
    ConnectionHandle: TSQLHDbc;
    FunctionId:       word;
    var Supported:    word
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetInfoInt = function(
    ConnectionHandle: TSQLHDbc;
    InfoType:         word;
    var InfoValue:    NativeInt;
    BufferLength:     smallint;
    StringLengthPtr:  IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetInfoStr = function(
    ConnectionHandle: TSQLHDbc;
    InfoType:         word;
    InfoValuePtr:     IntPtr;
    BufferLength:     smallint;
    var StringLength: smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetStmtAttrPtr = function(
    StatementHandle:  TSQLHStmt;
    Attribute:        integer;
    var Value:        IntPtr;
    BufferLength:     integer;
    StringLength:     IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetStmtOption = function(
    StatementHandle: TSQLHStmt;
    Option:          word;
    Value:           IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetTypeInfo = function(
    StatementHandle: TSQLHStmt;
    DataType:        smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLNumResultCols = function(
    StatementHandle: TSQLHStmt;
    var ColumnCount: smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLParamData = function(
    StatementHandle: TSQLHStmt;
    var Value:       IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLPrepare = function(
    StatementHandle: TSQLHStmt;
    StatementText:   _PChar;
    TextLength:      integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLPutData = function(
    StatementHandle: TSQLHStmt;
    Data:            IntPtr;
    StrLen_or_Ind:   NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLRowCount = function(
    StatementHandle: TSQLHStmt;
    var RowCount:    NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetConnectAttrInt = function(
    ConnectionHandle: TSQLHDbc;
    Attribute:        integer;
    ValuePtr:         integer;
    StringLength:     integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetConnectAttrStr = function(
    ConnectionHandle: TSQLHDbc;
    Attribute:        integer;
    ValuePtr:         _PChar;
    StringLength:     integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetConnectOption = function(
    ConnectionHandle: TSQLHDbc;
    Option:           word;
    Value:            NativeUInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetCursorName = function(
    StatementHandle: TSQLHStmt;
    CursorName:      _PChar;
    NameLength:      smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetDescFieldPtr = function(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    Value:            IntPtr;
    BufferLength:     integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetDescFieldInt = function(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    Value:            integer;
    BufferLength:     integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetDescRec = function(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    _Type:            smallint;
    SubType:          smallint;
    Length:           NativeInt;
    Precision:        smallint;
    Scale:            smallint;
    Data:             IntPtr;
    var StringLength: NativeInt;
    var Indicator:    NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetEnvAttrInt = function(
    EnvironmentHandle: TSQLHEnv;
    Attribute:         integer;
    ValuePtr:          integer;
    StringLength:      integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetParam = function(
    StatementHandle:   TSQLHStmt;
    ParameterNumber:   word;
    ValueType:         smallint;
    ParameterType:     smallint;
    LengthPrecision:   NativeUInt;
    ParameterScale:    smallint;
    ParameterValue:    IntPtr;
    var StrLen_or_Ind: NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetStmtAttrPtr = function(
    StatementHandle: TSQLHStmt;
    Attribute:       integer;
    ValuePtr:        IntPtr;
    StringLength:    integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetStmtAttrInt = function(
    StatementHandle: TSQLHStmt;
    Attribute:       integer;
    ValuePtr:        integer;
    StringLength:    integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetStmtAttrStr = function(
    StatementHandle: TSQLHStmt;
    Attribute:       integer;
    ValuePtr:        _PChar;
    StringLength:    integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetStmtOption = function(
    StatementHandle: TSQLHStmt;
    Option:          word;
    Value:           NativeUInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSpecialColumns = function(
    StatementHandle: TSQLHStmt;
    IdentifierType:  word;
    CatalogName:     _PChar;
    NameLength1:     smallint;
    SchemaName:      _PChar;
    NameLength2:     smallint;
    TableName:       _PChar;
    NameLength3:     smallint;
    Scope:           word;
    Nullable:        word
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLStatistics = function(
    StatementHandle: TSQLHStmt;
    CatalogName:     _PChar;
    NameLength1:     smallint;
    SchemaName:      _PChar;
    NameLength2:     smallint;
    TableName:       _PChar;
    NameLength3:     smallint;
    Unique:          word;
    Reserved:        word
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLTables = function(
    StatementHandle: TSQLHStmt;
    CatalogName:     _PChar;
    NameLength1:     smallint;
    SchemaName:      _PChar;
    NameLength2:     smallint;
    TableName:       _PChar;
    NameLength3:     smallint;
    TableType:       _PChar;
    NameLength4:     smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLTransact = function(
    EnvironmentHandle: TSQLHEnv;
    ConnectionHandle:  TSQLHDbc;
    CompletionType:    word
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  TSQLState = array[0..5] of _Char;

const
  SQL_WCHAR        = -8;
  SQL_WVARCHAR     = -9;
  SQL_WLONGVARCHAR = -10;

  // Generally useful constants
  SQL_SQLSTATE_SIZE  = 5;  // Size of SQLSTATE
  SQL_MAX_DSN_LENGTH = 32; // Maximum data source name size

  SQL_MAX_OPTION_STRING_LENGTH = 256;

  // Return code SQL_NO_DATA_FOUND is the same as SQL_NO_DATA
  SQL_NO_DATA_FOUND = SQL_NO_DATA;

  // An env handle type
  SQL_HANDLE_SENV = 5;

  // Environment attributes
  SQL_ATTR_ODBC_VERSION       = 200;
  SQL_ATTR_CONNECTION_POOLING = 201;
  SQL_ATTR_CP_MATCH           = 202;

  // Values for SQL_ATTR_CONNECTION_POOLING
  SQL_CP_OFF            = 0;
  SQL_CP_ONE_PER_DRIVER = 1;
  SQL_CP_ONE_PER_HENV   = 2;
  SQL_CP_DEFAULT        = SQL_CP_OFF;

  // Values for SQL_ATTR_CP_MATCH
  SQL_CP_STRICT_MATCH  = 0;
  SQL_CP_RELAXED_MATCH = 1;
  SQL_CP_MATCH_DEFAULT = SQL_CP_STRICT_MATCH;

  // Values for SQL_ATTR_ODBC_VERSION
  SQL_OV_ODBC2 = 2;
  SQL_OV_ODBC3 = 3;

  // Connection attributes
  SQL_ACCESS_MODE       = 101;
  SQL_AUTOCOMMIT        = 102;
  SQL_LOGIN_TIMEOUT     = 103;
  SQL_OPT_TRACE         = 104;
  SQL_OPT_TRACEFILE     = 105;
  SQL_TRANSLATE_DLL     = 106;
  SQL_TRANSLATE_OPTION  = 107;
  SQL_TXN_ISOLATION     = 108;
  SQL_CURRENT_QUALIFIER = 109;
  SQL_ODBC_CURSORS      = 110;
  SQL_QUIET_MODE        = 111;
  SQL_PACKET_SIZE       = 112;

  // Connection attributes with new names
  SQL_ATTR_ACCESS_MODE         = SQL_ACCESS_MODE;
  SQL_ATTR_AUTOCOMMIT          = SQL_AUTOCOMMIT;
  SQL_ATTR_CONNECTION_TIMEOUT  = 113;
  SQL_ATTR_CURRENT_CATALOG     = SQL_CURRENT_QUALIFIER;
  SQL_ATTR_DISCONNECT_BEHAVIOR = 114;
  SQL_ATTR_ENLIST_IN_DTC       = 1207;
  SQL_ATTR_ENLIST_IN_XA        = 1208;
  SQL_ATTR_LOGIN_TIMEOUT       = SQL_LOGIN_TIMEOUT;
  SQL_ATTR_ODBC_CURSORS        = SQL_ODBC_CURSORS;
  SQL_ATTR_PACKET_SIZE         = SQL_PACKET_SIZE;
  SQL_ATTR_QUIET_MODE          = SQL_QUIET_MODE;
  SQL_ATTR_TRACE               = SQL_OPT_TRACE;
  SQL_ATTR_TRACEFILE           = SQL_OPT_TRACEFILE;
  SQL_ATTR_TRANSLATE_LIB       = SQL_TRANSLATE_DLL;
  SQL_ATTR_TRANSLATE_OPTION    = SQL_TRANSLATE_OPTION;
  SQL_ATTR_TXN_ISOLATION       = SQL_TXN_ISOLATION;
  SQL_ATTR_CONNECTION_DEAD     = 1209;                   // GetConnectAttr only

  { ODBC Driver Manager sets this connection attribute to an unicode driver
    (which supports SQLConnectW) when the application is an ANSI application
    (which calls SQLConnect, SQLDriverConnect, or SQLBrowseConnect).
    This is SetConnectAttr only and application does not set this attribute
    This attribute was introduced because some unicode driver's some APIs may
    need to behave differently on ANSI or Unicode applications. A unicode
    driver, which has same behavior for both ANSI or Unicode applications,
    should return SQL_ERROR when the driver manager sets this connection
    attribute. When a unicode driver returns SQL_SUCCESS on this attribute,
    the driver manager treates ANSI and Unicode connections differently in
    connection pooling. }
  SQL_ATTR_ANSI_APP = 115;

  // SQL_ACCESS_MODE options
  SQL_MODE_READ_WRITE = 0;
  SQL_MODE_READ_ONLY  = 1;
  SQL_MODE_DEFAULT    = SQL_MODE_READ_WRITE;

  // SQL_AUTOCOMMIT options
  SQL_AUTOCOMMIT_OFF     = 0;
  SQL_AUTOCOMMIT_ON      = 1;
  SQL_AUTOCOMMIT_DEFAULT = SQL_AUTOCOMMIT_ON;

  // SQL_LOGIN_TIMEOUT options
  SQL_LOGIN_TIMEOUT_DEFAULT = 15;

  // SQL_OPT_TRACE options
  SQL_OPT_TRACE_OFF          = 0;
  SQL_OPT_TRACE_ON           = 1;
  SQL_OPT_TRACE_DEFAULT      = SQL_OPT_TRACE_OFF;
  SQL_OPT_TRACE_FILE_DEFAULT = '\\SQL.LOG';

  // SQL_ODBC_CURSORS options
  SQL_CUR_USE_IF_NEEDED = 0;
  SQL_CUR_USE_ODBC      = 1;
  SQL_CUR_USE_DRIVER    = 2;
  SQL_CUR_DEFAULT       = SQL_CUR_USE_DRIVER;

  // Values for SQL_ATTR_DISCONNECT_BEHAVIOR
  SQL_DB_RETURN_TO_POOL = 0;
  SQL_DB_DISCONNECT     = 1;
  SQL_DB_DEFAULT        = SQL_DB_RETURN_TO_POOL;

  // Values for SQL_ATTR_ENLIST_IN_DTC
  SQL_DTC_DONE = 0;

  // Values for SQL_ATTR_CONNECTION_DEAD
  SQL_CD_TRUE  = 1; // Connection is closed/dead
  SQL_CD_FALSE = 0; // Connection is open/available

  // Values for SQL_ATTR_ANSI_APP
  SQL_AA_TRUE  = 1; // the application is an ANSI app
  SQL_AA_FALSE = 0; // the application is a Unicode app

  // Statement attributes
  SQL_QUERY_TIMEOUT   = 0;
  SQL_MAX_ROWS        = 1;
  SQL_NOSCAN          = 2;
  SQL_MAX_LENGTH      = 3;
  SQL_ASYNC_ENABLE    = 4;  // Same as SQL_ATTR_ASYNC_ENABLE
  SQL_BIND_TYPE       = 5;
  SQL_CURSOR_TYPE     = 6;
  SQL_CONCURRENCY     = 7;
  SQL_KEYSET_SIZE     = 8;
  SQL_ROWSET_SIZE     = 9;
  SQL_SIMULATE_CURSOR = 10;
  SQL_RETRIEVE_DATA   = 11;
  SQL_USE_BOOKMARKS   = 12;
  SQL_GET_BOOKMARK    = 13; // GetStmtOption Only
  SQL_ROW_NUMBER      = 14; // GetStmtOption Only

  SQL_ATTR_ASYNC_ENABLE          = 4;
  SQL_ATTR_CONCURRENCY           = SQL_CONCURRENCY;
  SQL_ATTR_CURSOR_TYPE           = SQL_CURSOR_TYPE;
  SQL_ATTR_ENABLE_AUTO_IPD       = 15;
  SQL_ATTR_FETCH_BOOKMARK_PTR    = 16;
  SQL_ATTR_KEYSET_SIZE           = SQL_KEYSET_SIZE;
  SQL_ATTR_MAX_LENGTH            = SQL_MAX_LENGTH;
  SQL_ATTR_MAX_ROWS              = SQL_MAX_ROWS;
  SQL_ATTR_NOSCAN                = SQL_NOSCAN;
  SQL_ATTR_PARAM_BIND_OFFSET_PTR = 17;
  SQL_ATTR_PARAM_BIND_TYPE       = 18;
  SQL_ATTR_PARAM_OPERATION_PTR   = 19;
  SQL_ATTR_PARAM_STATUS_PTR      = 20;
  SQL_ATTR_PARAMS_PROCESSED_PTR  = 21;
  SQL_ATTR_PARAMSET_SIZE         = 22;
  SQL_ATTR_QUERY_TIMEOUT         = SQL_QUERY_TIMEOUT;
  SQL_ATTR_RETRIEVE_DATA         = SQL_RETRIEVE_DATA;
  SQL_ATTR_ROW_BIND_OFFSET_PTR   = 23;
  SQL_ATTR_ROW_BIND_TYPE         = SQL_BIND_TYPE;
  SQL_ATTR_ROW_NUMBER            = SQL_ROW_NUMBER;      // GetStmtAttr
  SQL_ATTR_ROW_OPERATION_PTR     = 24;
  SQL_ATTR_ROW_STATUS_PTR        = 25;
  SQL_ATTR_ROWS_FETCHED_PTR      = 26;
  SQL_ATTR_ROW_ARRAY_SIZE        = 27;
  SQL_ATTR_SIMULATE_CURSOR       = SQL_SIMULATE_CURSOR;
  SQL_ATTR_USE_BOOKMARKS         = SQL_USE_BOOKMARKS;

  // SQLColAttributes defines
  SQL_COLUMN_COUNT          = 0;
  SQL_COLUMN_NAME           = 1;
  SQL_COLUMN_TYPE           = 2;
  SQL_COLUMN_LENGTH         = 3;
  SQL_COLUMN_PRECISION      = 4;
  SQL_COLUMN_SCALE          = 5;
  SQL_COLUMN_DISPLAY_SIZE   = 6;
  SQL_COLUMN_NULLABLE       = 7;
  SQL_COLUMN_UNSIGNED       = 8;
  SQL_COLUMN_MONEY          = 9;
  SQL_COLUMN_UPDATABLE      = 10;
  SQL_COLUMN_AUTO_INCREMENT = 11;
  SQL_COLUMN_CASE_SENSITIVE = 12;
  SQL_COLUMN_SEARCHABLE     = 13;
  SQL_COLUMN_TYPE_NAME      = 14;
  SQL_COLUMN_TABLE_NAME     = 15;
  SQL_COLUMN_OWNER_NAME     = 16;
  SQL_COLUMN_QUALIFIER_NAME = 17;
  SQL_COLUMN_LABEL          = 18;

  // For MSSQL sql_variant C subtype
  SQL_CA_SS_VARIANT_TYPE    = 1215;

  SQL_COLATT_OPT_MAX = SQL_COLUMN_LABEL;
  SQL_COLATT_OPT_MIN = SQL_COLUMN_COUNT;

  // SQLColAttributes subdefines for SQL_COLUMN_UPDATABLE
  SQL_ATTR_READONLY          = 0;
  SQL_ATTR_WRITE             = 1;
  SQL_ATTR_READWRITE_UNKNOWN = 2;

  // SQLColAttributes subdefines for SQL_COLUMN_SEARCHABLE
  // These are also used by SQLGetInfo
  SQL_UNSEARCHABLE    = 0;
  SQL_LIKE_ONLY       = 1;
  SQL_ALL_EXCEPT_LIKE = 2;
  SQL_SEARCHABLE      = 3;
  SQL_PRED_SEARCHABLE = SQL_SEARCHABLE;

  // Special return values for SQLGetData
  SQL_NO_TOTAL =  -4;

  // New defines for SEARCHABLE column in SQLGetTypeInfo
  SQL_COL_PRED_CHAR  = SQL_LIKE_ONLY;
  SQL_COL_PRED_BASIC = SQL_ALL_EXCEPT_LIKE;

  // Whether an attribute is a pointer or not
  SQL_IS_POINTER   = -4;
  SQL_IS_UINTEGER  = -5;
  SQL_IS_INTEGER   = -6;
  SQL_IS_USMALLINT = -7;
  SQL_IS_SMALLINT  = -8;

  // The value of SQL_ATTR_PARAM_BIND_TYPE
  SQL_PARAM_BIND_BY_COLUMN    = 0;
  SQL_PARAM_BIND_TYPE_DEFAULT = SQL_PARAM_BIND_BY_COLUMN;

  // SQL_QUERY_TIMEOUT options
  SQL_QUERY_TIMEOUT_DEFAULT = 0;

  // SQL_MAX_ROWS options
  SQL_MAX_ROWS_DEFAULT = 0;

  // SQL_NOSCAN options
  SQL_NOSCAN_OFF     = 0;       // 1.0 FALSE
  SQL_NOSCAN_ON      = 1;       // 1.0 TRUE
  SQL_NOSCAN_DEFAULT = SQL_NOSCAN_OFF;

  // SQL_MAX_LENGTH options
  SQL_MAX_LENGTH_DEFAULT = 0;

  // Values for SQL_ATTR_ASYNC_ENABLE
  SQL_ASYNC_ENABLE_OFF     = 0;
  SQL_ASYNC_ENABLE_ON      = 1;
  SQL_ASYNC_ENABLE_DEFAULT = SQL_ASYNC_ENABLE_OFF;

  // SQL_BIND_TYPE options
  SQL_BIND_BY_COLUMN    = 0;
  SQL_BIND_TYPE_DEFAULT = SQL_BIND_BY_COLUMN; // Default value

  // SQL_CONCURRENCY options
  SQL_CONCUR_READ_ONLY = 1;
  SQL_CONCUR_LOCK      = 2;
  SQL_CONCUR_ROWVER    = 3;
  SQL_CONCUR_VALUES    = 4;
  SQL_CONCUR_DEFAULT   = SQL_CONCUR_READ_ONLY; // Default value

  // SQL_CURSOR_TYPE options
  SQL_CURSOR_FORWARD_ONLY  = 0;
  SQL_CURSOR_KEYSET_DRIVEN = 1;
  SQL_CURSOR_DYNAMIC       = 2;
  SQL_CURSOR_STATIC        = 3;
  SQL_CURSOR_TYPE_DEFAULT  = SQL_CURSOR_FORWARD_ONLY; // Default value

  // SQL_ROWSET_SIZE options
  SQL_ROWSET_SIZE_DEFAULT = 1;

  // SQL_KEYSET_SIZE options
  SQL_KEYSET_SIZE_DEFAULT = 0;

  // SQL_SIMULATE_CURSOR options
  SQL_SC_NON_UNIQUE = 0;
  SQL_SC_TRY_UNIQUE = 1;
  SQL_SC_UNIQUE     = 2;

  // SQL_RETRIEVE_DATA options
  SQL_RD_OFF     = 0;
  SQL_RD_ON      = 1;
  SQL_RD_DEFAULT = SQL_RD_ON;

  // SQL_USE_BOOKMARKS options
  SQL_UB_OFF     = 0;
  SQL_UB_ON      = 1;
  SQL_UB_DEFAULT = SQL_UB_OFF;

  // New values for SQL_USE_BOOKMARKS attribute
  SQL_UB_FIXED    = SQL_UB_ON;
  SQL_UB_VARIABLE = 2;

  // Extended descriptor field
  SQL_DESC_ARRAY_SIZE                  = 20;
  SQL_DESC_ARRAY_STATUS_PTR            = 21;
  SQL_DESC_AUTO_UNIQUE_VALUE           = SQL_COLUMN_AUTO_INCREMENT;
  SQL_DESC_BASE_COLUMN_NAME            = 22;
  SQL_DESC_BASE_TABLE_NAME             = 23;
  SQL_DESC_BIND_OFFSET_PTR             = 24;
  SQL_DESC_BIND_TYPE                   = 25;
  SQL_DESC_CASE_SENSITIVE              = SQL_COLUMN_CASE_SENSITIVE;
  SQL_DESC_CATALOG_NAME                = SQL_COLUMN_QUALIFIER_NAME;
  SQL_DESC_CONCISE_TYPE                = SQL_COLUMN_TYPE;
  SQL_DESC_DATETIME_INTERVAL_PRECISION = 26;
  SQL_DESC_DISPLAY_SIZE                = SQL_COLUMN_DISPLAY_SIZE;
  SQL_DESC_FIXED_PREC_SCALE            = SQL_COLUMN_MONEY;
  SQL_DESC_LABEL                       = SQL_COLUMN_LABEL;
  SQL_DESC_LITERAL_PREFIX              = 27;
  SQL_DESC_LITERAL_SUFFIX              = 28;
  SQL_DESC_LOCAL_TYPE_NAME             = 29;
  SQL_DESC_MAXIMUM_SCALE               = 30;
  SQL_DESC_MINIMUM_SCALE               = 31;
  SQL_DESC_NUM_PREC_RADIX              = 32;
  SQL_DESC_PARAMETER_TYPE              = 33;
  SQL_DESC_ROWS_PROCESSED_PTR          = 34;
  SQL_DESC_ROWVER                      = 35;
  SQL_DESC_SCHEMA_NAME                 = SQL_COLUMN_OWNER_NAME;
  SQL_DESC_SEARCHABLE                  = SQL_COLUMN_SEARCHABLE;
  SQL_DESC_TYPE_NAME                   = SQL_COLUMN_TYPE_NAME;
  SQL_DESC_TABLE_NAME                  = SQL_COLUMN_TABLE_NAME;
  SQL_DESC_UNSIGNED                    = SQL_COLUMN_UNSIGNED;
  SQL_DESC_UPDATABLE                   = SQL_COLUMN_UPDATABLE;

  // Defines for diagnostics fields
  SQL_DIAG_CURSOR_ROW_COUNT = -1249;
  SQL_DIAG_ROW_NUMBER       = -1248;
  SQL_DIAG_COLUMN_NUMBER    = -1247;

  // SQL extended datatypes
  SQL_DATE          = 9;
  SQL_INTERVAL      = 10;
  SQL_TIME          = 10;
  SQL_TIMESTAMP     = 11;
  SQL_LONGVARCHAR   = -1;
  SQL_BINARY        = -2;
  SQL_VARBINARY     = -3;
  SQL_LONGVARBINARY = -4;
  SQL_BIGINT        = -5;
  SQL_TINYINT       = -6;
  SQL_BIT           = -7;
  SQL_GUID          = -11;

  // MS SQL Server data types
  SQL_VARIANT    = -150;

  // IBM DB2 data types
  SQL_GRAPHIC           = -95;
  SQL_VARGRAPHIC        = -96;
  SQL_LONGVARGRAPHIC    = -97;
  SQL_BLOB              = -98;
  SQL_CLOB              = -99;
  SQL_DBCLOB            = -350;
  SQL_XML               = -370;
  SQL_DATALINK          = -400;
  SQL_USER_DEFINED_TYPE = -450;

  SQL_BLOB_LOCATOR      = 31;
  SQL_CLOB_LOCATOR      = 41;
  SQL_DBCLOB_LOCATOR    = -351;

  // Oracle data types
  SQL_REFCURSOR    = -403;

  // Interval code
  SQL_CODE_YEAR             = 1;
  SQL_CODE_MONTH            = 2;
  SQL_CODE_DAY              = 3;
  SQL_CODE_HOUR             = 4;
  SQL_CODE_MINUTE           = 5;
  SQL_CODE_SECOND           = 6;
  SQL_CODE_YEAR_TO_MONTH    = 7;
  SQL_CODE_DAY_TO_HOUR      = 8;
  SQL_CODE_DAY_TO_MINUTE    = 9;
  SQL_CODE_DAY_TO_SECOND    = 10;
  SQL_CODE_HOUR_TO_MINUTE   = 11;
  SQL_CODE_HOUR_TO_SECOND   = 12;
  SQL_CODE_MINUTE_TO_SECOND = 13;

  SQL_INTERVAL_YEAR             = (100 + SQL_CODE_YEAR);
  SQL_INTERVAL_MONTH            = (100 + SQL_CODE_MONTH);
  SQL_INTERVAL_DAY              = (100 + SQL_CODE_DAY);
  SQL_INTERVAL_HOUR             = (100 + SQL_CODE_HOUR);
  SQL_INTERVAL_MINUTE           = (100 + SQL_CODE_MINUTE);
  SQL_INTERVAL_SECOND           = (100 + SQL_CODE_SECOND);
  SQL_INTERVAL_YEAR_TO_MONTH    = (100 + SQL_CODE_YEAR_TO_MONTH);
  SQL_INTERVAL_DAY_TO_HOUR      = (100 + SQL_CODE_DAY_TO_HOUR);
  SQL_INTERVAL_DAY_TO_MINUTE    = (100 + SQL_CODE_DAY_TO_MINUTE);
  SQL_INTERVAL_DAY_TO_SECOND    = (100 + SQL_CODE_DAY_TO_SECOND);
  SQL_INTERVAL_HOUR_TO_MINUTE   = (100 + SQL_CODE_HOUR_TO_MINUTE);
  SQL_INTERVAL_HOUR_TO_SECOND   = (100 + SQL_CODE_HOUR_TO_SECOND);
  SQL_INTERVAL_MINUTE_TO_SECOND = (100 + SQL_CODE_MINUTE_TO_SECOND);

  SQL_UNICODE             = SQL_WCHAR;
  SQL_UNICODE_VARCHAR     = SQL_WVARCHAR;
  SQL_UNICODE_LONGVARCHAR = SQL_WLONGVARCHAR;
  SQL_UNICODE_CHAR        = SQL_WCHAR;

  // C datatype to SQL datatype mapping SQL types
  SQL_C_DEFAULT                   = 99;
  SQL_SIGNED_OFFSET               = -20;
  SQL_UNSIGNED_OFFSET             = -22;
  SQL_C_CHAR                      = SQL_CHAR;     // CHAR, VARCHAR, DECIMAL, NUMERIC
  SQL_C_WCHAR                     = SQL_WCHAR;
  SQL_C_LONG                      = SQL_INTEGER;  // INTEGER
  SQL_C_SHORT                     = SQL_SMALLINT; // SMALLINT
  SQL_C_FLOAT                     = SQL_REAL;     // REAL
  SQL_C_DOUBLE                    = SQL_DOUBLE;   // FLOAT, DOUBLE
  SQL_C_NUMERIC                   = SQL_NUMERIC;
  SQL_C_DATE                      = SQL_DATE;
  SQL_C_TIME                      = SQL_TIME;
  SQL_C_TIMESTAMP                 = SQL_TIMESTAMP;
  SQL_C_TYPE_DATE                 = SQL_TYPE_DATE;
  SQL_C_TYPE_TIME                 = SQL_TYPE_TIME;
  SQL_C_TYPE_TIMESTAMP            = SQL_TYPE_TIMESTAMP;
  SQL_C_INTERVAL_YEAR             = SQL_INTERVAL_YEAR;
  SQL_C_INTERVAL_MONTH            = SQL_INTERVAL_MONTH;
  SQL_C_INTERVAL_DAY              = SQL_INTERVAL_DAY;
  SQL_C_INTERVAL_HOUR             = SQL_INTERVAL_HOUR;
  SQL_C_INTERVAL_MINUTE           = SQL_INTERVAL_MINUTE;
  SQL_C_INTERVAL_SECOND           = SQL_INTERVAL_SECOND;
  SQL_C_INTERVAL_YEAR_TO_MONTH    = SQL_INTERVAL_YEAR_TO_MONTH;
  SQL_C_INTERVAL_DAY_TO_HOUR      = SQL_INTERVAL_DAY_TO_HOUR;
  SQL_C_INTERVAL_DAY_TO_MINUTE    = SQL_INTERVAL_DAY_TO_MINUTE;
  SQL_C_INTERVAL_DAY_TO_SECOND    = SQL_INTERVAL_DAY_TO_SECOND;
  SQL_C_INTERVAL_HOUR_TO_MINUTE   = SQL_INTERVAL_HOUR_TO_MINUTE;
  SQL_C_INTERVAL_HOUR_TO_SECOND   = SQL_INTERVAL_HOUR_TO_SECOND;
  SQL_C_INTERVAL_MINUTE_TO_SECOND = SQL_INTERVAL_MINUTE_TO_SECOND;
  SQL_C_BINARY                    = SQL_BINARY;
  SQL_C_BIT                       = SQL_BIT;
  SQL_C_SBIGINT                   = (SQL_BIGINT + SQL_SIGNED_OFFSET);    // SIGNED BIGINT
  SQL_C_UBIGINT                   = (SQL_BIGINT + SQL_UNSIGNED_OFFSET);  // UNSIGNED BIGINT
  SQL_C_TINYINT                   = SQL_TINYINT;
  SQL_C_SLONG                     = (SQL_C_LONG + SQL_SIGNED_OFFSET);    // SIGNED INTEGER
  SQL_C_SSHORT                    = (SQL_C_SHORT + SQL_SIGNED_OFFSET);   // SIGNED SMALLINT
  SQL_C_STINYINT                  = (SQL_TINYINT + SQL_SIGNED_OFFSET);   // SIGNED TINYINT
  SQL_C_ULONG                     = (SQL_C_LONG + SQL_UNSIGNED_OFFSET);  // UNSIGNED INTEGER
  SQL_C_USHORT                    = (SQL_C_SHORT + SQL_UNSIGNED_OFFSET); // UNSIGNED SMALLINT
  SQL_C_UTINYINT                  = (SQL_TINYINT + SQL_UNSIGNED_OFFSET); // UNSIGNED TINYINT
  SQL_C_BOOKMARK                  = SQL_C_ULONG;                         // BOOKMARK
  SQL_C_GUID                      = SQL_GUID;

  SQL_TYPE_NULL     = 0;
  SQL_C_VARBOOKMARK = SQL_C_BINARY;

  // IBM DB2 data types
  SQL_C_BLOB_LOCATOR    = SQL_BLOB_LOCATOR;
  SQL_C_CLOB_LOCATOR    = SQL_CLOB_LOCATOR;
  SQL_C_DBCLOB_LOCATOR  = SQL_DBCLOB_LOCATOR;

  // Define for SQL_DIAG_ROW_NUMBER and SQL_DIAG_COLUMN_NUMBER
  SQL_NO_ROW_NUMBER         = -1;
  SQL_NO_COLUMN_NUMBER      = -1;
  SQL_ROW_NUMBER_UNKNOWN    = -2;
  SQL_COLUMN_NUMBER_UNKNOWN = -2;

  // SQLBindParameter extensions
  SQL_DEFAULT_PARAM           = -5;
  SQL_IGNORE                  = -6;
  SQL_COLUMN_IGNORE           = SQL_IGNORE;
  SQL_LEN_DATA_AT_EXEC_OFFSET = -100;

  // Binary Length for driver specific attributes
  SQL_LEN_BINARY_ATTR_OFFSET = -100;

const
  // Defines for SQLBindParameter and
  // SQLProcedureColumns (returned in the Result set)
  SQL_PARAM_TYPE_UNKNOWN = 0;
  SQL_PARAM_INPUT        = 1;
  SQL_PARAM_INPUT_OUTPUT = 2;
  SQL_RESULT_COL         = 3;
  SQL_PARAM_OUTPUT       = 4;
  SQL_RETURN_VALUE       = 5;

  // Defines used by Driver Manager when mapping SQLSetParam to SQLBindParameter
  SQL_PARAM_TYPE_DEFAULT = SQL_PARAM_INPUT_OUTPUT;
  SQL_SETPARAM_VALUE_MAX = -1;

  // SQLGetFunctions: additional values for
  // fFunction to represent functions that
  // are not in the X/Open spec.
  SQL_API_SQLALLOCHANDLESTD   = 73;
  SQL_API_SQLBULKOPERATIONS   = 24;
  SQL_API_SQLBINDPARAMETER    = 72;
  SQL_API_SQLBROWSECONNECT    = 55;
  SQL_API_SQLCOLATTRIBUTES    = 6;
  SQL_API_SQLCOLUMNPRIVILEGES = 56;
  SQL_API_SQLDESCRIBEPARAM    = 58;
  SQL_API_SQLDRIVERCONNECT    = 41;
  SQL_API_SQLDRIVERS          = 71;
  SQL_API_SQLEXTENDEDFETCH    = 59;
  SQL_API_SQLFOREIGNKEYS      = 60;
  SQL_API_SQLMORERESULTS      = 61;
  SQL_API_SQLNATIVESQL        = 62;
  SQL_API_SQLNUMPARAMS        = 63;
  SQL_API_SQLPARAMOPTIONS     = 64;
  SQL_API_SQLPRIMARYKEYS      = 65;
  SQL_API_SQLPROCEDURECOLUMNS = 66;
  SQL_API_SQLPROCEDURES       = 67;
  SQL_API_SQLSETPOS           = 68;
  SQL_API_SQLSETSCROLLOPTIONS = 69;
  SQL_API_SQLTABLEPRIVILEGES  = 70;

  // SQL_API_ALL_FUNCTIONS returns an array
  // of 'booleans' representing whether a
  // function is implemented by the driver.
  //
  // CAUTION: Only functions defined in ODBC
  // version 2.0 and earlier are returned, the
  // new high-range function numbers defined by
  // X/Open break this scheme. See the new
  // method -- SQL_API_ODBC3_ALL_FUNCTIONS
  SQL_API_ALL_FUNCTIONS = 0; // See CAUTION above

  // 2.X drivers export a dummy function with
  // ordinal number SQL_API_LOADBYORDINAL to speed
  // loading under the windows operating system.
  //
  // CAUTION: Loading by ordinal is not supported
  // for 3.0 and above drivers.
  SQL_API_LOADBYORDINAL = 199; // See CAUTION above

  // SQL_API_ODBC3_ALL_FUNCTIONS
  // This returns a bitmap, which allows us to*
  // handle the higher-valued function numbers.
  // Use SQL_FUNC_EXISTS(bitmap,function_number)
  // to determine if the function exists.
  SQL_API_ODBC3_ALL_FUNCTIONS      = 999;
  SQL_API_ODBC3_ALL_FUNCTIONS_SIZE = 250; // Array of 250 words

const
  // Extended definitions for SQLGetInfo
  // Values in ODBC 2.0 that are not
  // in the X/Open spec
  SQL_INFO_FIRST                 = 0;
  SQL_ACTIVE_CONNECTIONS         = 0;  // MAX_DRIVER_CONNECTIONS
  SQL_ACTIVE_STATEMENTS          = 1;  // MAX_CONCURRENT_ACTIVITIES
  SQL_DRIVER_HDBC                = 3;
  SQL_DRIVER_HENV                = 4;
  SQL_DRIVER_HSTMT               = 5;
  SQL_DRIVER_NAME                = 6;
  SQL_DRIVER_VER                 = 7;
  SQL_ODBC_API_CONFORMANCE       = 9;
  SQL_ODBC_VER                   = 10;
  SQL_ROW_UPDATES                = 11;
  SQL_ODBC_SAG_CLI_CONFORMANCE   = 12;
  SQL_ODBC_SQL_CONFORMANCE       = 15;
  SQL_PROCEDURES                 = 21;
  SQL_CONCAT_NULL_BEHAVIOR       = 22;
  SQL_CURSOR_ROLLBACK_BEHAVIOR   = 24;
  SQL_EXPRESSIONS_IN_ORDERBY     = 27;
  SQL_MAX_OWNER_NAME_LEN         = 32; // MAX_SCHEMA_NAME_LEN
  SQL_MAX_PROCEDURE_NAME_LEN     = 33;
  SQL_MAX_QUALIFIER_NAME_LEN     = 34; // MAX_CATALOG_NAME_LEN
  SQL_MULT_RESULT_SETS           = 36;
  SQL_MULTIPLE_ACTIVE_TXN        = 37;
  SQL_OUTER_JOINS                = 38;
  SQL_OWNER_TERM                 = 39;
  SQL_PROCEDURE_TERM             = 40;
  SQL_QUALIFIER_NAME_SEPARATOR   = 41;
  SQL_QUALIFIER_TERM             = 42;
  SQL_SCROLL_OPTIONS             = 44;
  SQL_TABLE_TERM                 = 45;
  SQL_CONVERT_FUNCTIONS          = 48;
  SQL_NUMERIC_FUNCTIONS          = 49;
  SQL_STRING_FUNCTIONS           = 50;
  SQL_SYSTEM_FUNCTIONS           = 51;
  SQL_TIMEDATE_FUNCTIONS         = 52;
  SQL_CONVERT_BIGINT             = 53;
  SQL_CONVERT_BINARY             = 54;
  SQL_CONVERT_BIT                = 55;
  SQL_CONVERT_CHAR               = 56;
  SQL_CONVERT_DATE               = 57;
  SQL_CONVERT_DECIMAL            = 58;
  SQL_CONVERT_DOUBLE             = 59;
  SQL_CONVERT_FLOAT              = 60;
  SQL_CONVERT_INTEGER            = 61;
  SQL_CONVERT_LONGVARCHAR        = 62;
  SQL_CONVERT_NUMERIC            = 63;
  SQL_CONVERT_REAL               = 64;
  SQL_CONVERT_SMALLINT           = 65;
  SQL_CONVERT_TIME               = 66;
  SQL_CONVERT_TIMESTAMP          = 67;
  SQL_CONVERT_TINYINT            = 68;
  SQL_CONVERT_VARBINARY          = 69;
  SQL_CONVERT_VARCHAR            = 70;
  SQL_CONVERT_LONGVARBINARY      = 71;
  SQL_ODBC_SQL_OPT_IEF           = 73; // SQL_INTEGRITY
  SQL_CORRELATION_NAME           = 74;
  SQL_NON_NULLABLE_COLUMNS       = 75;
  SQL_DRIVER_HLIB                = 76;
  SQL_DRIVER_ODBC_VER            = 77;
  SQL_LOCK_TYPES                 = 78;
  SQL_POS_OPERATIONS             = 79;
  SQL_POSITIONED_STATEMENTS      = 80;
  SQL_BOOKMARK_PERSISTENCE       = 82;
  SQL_STATIC_SENSITIVITY         = 83;
  SQL_FILE_USAGE                 = 84;
  SQL_COLUMN_ALIAS               = 87;
  SQL_GROUP_BY                   = 88;
  SQL_KEYWORDS                   = 89;
  SQL_OWNER_USAGE                = 91;
  SQL_QUALIFIER_USAGE            = 92;
  SQL_QUOTED_IDENTIFIER_CASE     = 93;
  SQL_SUBQUERIES                 = 95;
  SQL_UNION                      = 96;
  SQL_MAX_ROW_SIZE_INCLUDES_LONG = 103;
  SQL_MAX_CHAR_LITERAL_LEN       = 108;
  SQL_TIMEDATE_ADD_INTERVALS     = 109;
  SQL_TIMEDATE_DIFF_INTERVALS    = 110;
  SQL_NEED_LONG_DATA_LEN         = 111;
  SQL_MAX_BINARY_LITERAL_LEN     = 112;
  SQL_LIKE_ESCAPE_CLAUSE         = 113;
  SQL_QUALIFIER_LOCATION         = 114;

  // ODBC 3.0 SQLGetInfo values that are not part
  // of the X/Open standard at this time. X/Open
  // standard values are in SQL.h.
  SQL_ACTIVE_ENVIRONMENTS             = 116;
  SQL_ALTER_DOMAIN                    = 117;
  SQL_SQL_CONFORMANCE                 = 118;
  SQL_DATETIME_LITERALS               = 119;
  SQL_ASYNC_MODE                      = 10021; // new X/Open spec
  SQL_BATCH_ROW_COUNT                 = 120;
  SQL_BATCH_SUPPORT                   = 121;
  SQL_CATALOG_LOCATION                = SQL_QUALIFIER_LOCATION;
  SQL_CATALOG_NAME_SEPARATOR          = SQL_QUALIFIER_NAME_SEPARATOR;
  SQL_CATALOG_TERM                    = SQL_QUALIFIER_TERM;
  SQL_CATALOG_USAGE                   = SQL_QUALIFIER_USAGE;
  SQL_CONVERT_WCHAR                   = 122;
  SQL_CONVERT_INTERVAL_DAY_TIME       = 123;
  SQL_CONVERT_INTERVAL_YEAR_MONTH     = 124;
  SQL_CONVERT_WLONGVARCHAR            = 125;
  SQL_CONVERT_WVARCHAR                = 126;
  SQL_CREATE_ASSERTION                = 127;
  SQL_CREATE_CHARACTER_SET            = 128;
  SQL_CREATE_COLLATION                = 129;
  SQL_CREATE_DOMAIN                   = 130;
  SQL_CREATE_SCHEMA                   = 131;
  SQL_CREATE_TABLE                    = 132;
  SQL_CREATE_TRANSLATION              = 133;
  SQL_CREATE_VIEW                     = 134;
  SQL_DRIVER_HDESC                    = 135;
  SQL_DROP_ASSERTION                  = 136;
  SQL_DROP_CHARACTER_SET              = 137;
  SQL_DROP_COLLATION                  = 138;
  SQL_DROP_DOMAIN                     = 139;
  SQL_DROP_SCHEMA                     = 140;
  SQL_DROP_TABLE                      = 141;
  SQL_DROP_TRANSLATION                = 142;
  SQL_DROP_VIEW                       = 143;
  SQL_DYNAMIC_CURSOR_ATTRIBUTES1      = 144;
  SQL_DYNAMIC_CURSOR_ATTRIBUTES2      = 145;
  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 = 146;
  SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 = 147;
  SQL_INDEX_KEYWORDS                  = 148;
  SQL_INFO_SCHEMA_VIEWS               = 149;
  SQL_KEYSET_CURSOR_ATTRIBUTES1       = 150;
  SQL_KEYSET_CURSOR_ATTRIBUTES2       = 151;
  SQL_MAX_ASYNC_CONCURRENT_STATEMENTS = 10022; // new X/Open spec
  SQL_ODBC_INTERFACE_CONFORMANCE      = 152;
  SQL_PARAM_ARRAY_ROW_COUNTS          = 153;
  SQL_PARAM_ARRAY_SELECTS             = 154;
  SQL_SCHEMA_TERM                     = SQL_OWNER_TERM;
  SQL_SCHEMA_USAGE                    = SQL_OWNER_USAGE;
  SQL_SQL92_DATETIME_FUNCTIONS        = 155;
  SQL_SQL92_FOREIGN_KEY_DELETE_RULE   = 156;
  SQL_SQL92_FOREIGN_KEY_UPDATE_RULE   = 157;
  SQL_SQL92_GRANT                     = 158;
  SQL_SQL92_NUMERIC_VALUE_FUNCTIONS   = 159;
  SQL_SQL92_PREDICATES                = 160;
  SQL_SQL92_RELATIONAL_JOIN_OPERATORS = 161;
  SQL_SQL92_REVOKE                    = 162;
  SQL_SQL92_ROW_VALUE_CONSTRUCTOR     = 163;
  SQL_SQL92_STRING_FUNCTIONS          = 164;
  SQL_SQL92_VALUE_EXPRESSIONS         = 165;
  SQL_STANDARD_CLI_CONFORMANCE        = 166;
  SQL_STATIC_CURSOR_ATTRIBUTES1       = 167;
  SQL_STATIC_CURSOR_ATTRIBUTES2       = 168;
  SQL_AGGREGATE_FUNCTIONS             = 169;
  SQL_DDL_INDEX                       = 170;
  SQL_DM_VER                          = 171;
  SQL_INSERT_STATEMENT                = 172;
  SQL_CONVERT_GUID                    = 173;
  SQL_UNION_STATEMENT                 = SQL_UNION;

  SQL_DTC_TRANSITION_COST             = 1750;

  // SQL_CONVERT_* return value bitmasks
  SQL_CVT_CHAR                = $00000001;
  SQL_CVT_NUMERIC             = $00000002;
  SQL_CVT_DECIMAL             = $00000004;
  SQL_CVT_INTEGER             = $00000008;
  SQL_CVT_SMALLINT            = $00000010;
  SQL_CVT_FLOAT               = $00000020;
  SQL_CVT_REAL                = $00000040;
  SQL_CVT_DOUBLE              = $00000080;
  SQL_CVT_VARCHAR             = $00000100;
  SQL_CVT_LONGVARCHAR         = $00000200;
  SQL_CVT_BINARY              = $00000400;
  SQL_CVT_VARBINARY           = $00000800;
  SQL_CVT_BIT                 = $00001000;
  SQL_CVT_TINYINT             = $00002000;
  SQL_CVT_BIGINT              = $00004000;
  SQL_CVT_DATE                = $00008000;
  SQL_CVT_TIME                = $00010000;
  SQL_CVT_TIMESTAMP           = $00020000;
  SQL_CVT_LONGVARBINARY       = $00040000;
  SQL_CVT_INTERVAL_YEAR_MONTH = $00080000;
  SQL_CVT_INTERVAL_DAY_TIME   = $00100000;
  SQL_CVT_WCHAR               = $00200000;
  SQL_CVT_WLONGVARCHAR        = $00400000;
  SQL_CVT_WVARCHAR            = $00800000;
  SQL_CVT_GUID                = $01000000;

  // SQL_CONVERT_FUNCTIONS functions
  SQL_FN_CVT_CONVERT = $00000001;
  SQL_FN_CVT_CAST    = $00000002;

  // SQL_STRING_FUNCTIONS functions
  SQL_FN_STR_CONCAT           = $00000001;
  SQL_FN_STR_INSERT           = $00000002;
  SQL_FN_STR_LEFT             = $00000004;
  SQL_FN_STR_LTRIM            = $00000008;
  SQL_FN_STR_LENGTH           = $00000010;
  SQL_FN_STR_LOCATE           = $00000020;
  SQL_FN_STR_LCASE            = $00000040;
  SQL_FN_STR_REPEAT           = $00000080;
  SQL_FN_STR_REPLACE          = $00000100;
  SQL_FN_STR_RIGHT            = $00000200;
  SQL_FN_STR_RTRIM            = $00000400;
  SQL_FN_STR_SUBSTRING        = $00000800;
  SQL_FN_STR_UCASE            = $00001000;
  SQL_FN_STR_ASCII            = $00002000;
  SQL_FN_STR_CHAR             = $00004000;
  SQL_FN_STR_DIFFERENCE       = $00008000;
  SQL_FN_STR_LOCATE_2         = $00010000;
  SQL_FN_STR_SOUNDEX          = $00020000;
  SQL_FN_STR_SPACE            = $00040000;
  SQL_FN_STR_BIT_LENGTH       = $00080000;
  SQL_FN_STR_CHAR_LENGTH      = $00100000;
  SQL_FN_STR_CHARACTER_LENGTH = $00200000;
  SQL_FN_STR_OCTET_LENGTH     = $00400000;
  SQL_FN_STR_POSITION         = $00800000;

  // SQL_SQL92_STRING_FUNCTIONS
  SQL_SSF_CONVERT       = $00000001;
  SQL_SSF_LOWER         = $00000002;
  SQL_SSF_UPPER         = $00000004;
  SQL_SSF_SUBSTRING     = $00000008;
  SQL_SSF_TRANSLATE     = $00000010;
  SQL_SSF_TRIM_BOTH     = $00000020;
  SQL_SSF_TRIM_LEADING  = $00000040;
  SQL_SSF_TRIM_TRAILING = $00000080;

  // SQL_NUMERIC_FUNCTIONS functions
  SQL_FN_NUM_ABS     = $00000001;
  SQL_FN_NUM_ACOS     = $00000002;
  SQL_FN_NUM_ASIN     = $00000004;
  SQL_FN_NUM_ATAN     = $00000008;
  SQL_FN_NUM_ATAN2    = $00000010;
  SQL_FN_NUM_CEILING  = $00000020;
  SQL_FN_NUM_COS      = $00000040;
  SQL_FN_NUM_COT      = $00000080;
  SQL_FN_NUM_EXP      = $00000100;
  SQL_FN_NUM_FLOOR    = $00000200;
  SQL_FN_NUM_LOG      = $00000400;
  SQL_FN_NUM_MOD      = $00000800;
  SQL_FN_NUM_SIGN     = $00001000;
  SQL_FN_NUM_SIN      = $00002000;
  SQL_FN_NUM_SQRT     = $00004000;
  SQL_FN_NUM_TAN      = $00008000;
  SQL_FN_NUM_PI       = $00010000;
  SQL_FN_NUM_RAND     = $00020000;
  SQL_FN_NUM_DEGREES  = $00040000;
  SQL_FN_NUM_LOG10    = $00080000;
  SQL_FN_NUM_POWER    = $00100000;
  SQL_FN_NUM_RADIANS  = $00200000;
  SQL_FN_NUM_ROUND    = $00400000;
  SQL_FN_NUM_TRUNCATE = $00800000;

  // SQL_SQL92_NUMERIC_VALUE_FUNCTIONS
  SQL_SNVF_BIT_LENGTH       = $00000001;
  SQL_SNVF_CHAR_LENGTH      = $00000002;
  SQL_SNVF_CHARACTER_LENGTH = $00000004;
  SQL_SNVF_EXTRACT          = $00000008;
  SQL_SNVF_OCTET_LENGTH     = $00000010;
  SQL_SNVF_POSITION         = $00000020;

  // SQL_TIMEDATE_FUNCTIONS functions
  SQL_FN_TD_NOW               = $00000001;
  SQL_FN_TD_CURDATE           = $00000002;
  SQL_FN_TD_DAYOFMONTH        = $00000004;
  SQL_FN_TD_DAYOFWEEK         = $00000008;
  SQL_FN_TD_DAYOFYEAR         = $00000010;
  SQL_FN_TD_MONTH             = $00000020;
  SQL_FN_TD_QUARTER           = $00000040;
  SQL_FN_TD_WEEK              = $00000080;
  SQL_FN_TD_YEAR              = $00000100;
  SQL_FN_TD_CURTIME           = $00000200;
  SQL_FN_TD_HOUR              = $00000400;
  SQL_FN_TD_MINUTE            = $00000800;
  SQL_FN_TD_SECOND            = $00001000;
  SQL_FN_TD_TIMESTAMPADD      = $00002000;
  SQL_FN_TD_TIMESTAMPDIFF     = $00004000;
  SQL_FN_TD_DAYNAME           = $00008000;
  SQL_FN_TD_MONTHNAME         = $00010000;
  SQL_FN_TD_CURRENT_DATE      = $00020000;
  SQL_FN_TD_CURRENT_TIME      = $00040000;
  SQL_FN_TD_CURRENT_TIMESTAMP = $00080000;
  SQL_FN_TD_EXTRACT           = $00100000;

  // SQL_SQL92_DATETIME_FUNCTIONS
  SQL_SDF_CURRENT_DATE      = $00000001;
  SQL_SDF_CURRENT_TIME      = $00000002;
  SQL_SDF_CURRENT_TIMESTAMP = $00000004;

  // SQL_SYSTEM_FUNCTIONS functions
  SQL_FN_SYS_USERNAME = $00000001;
  SQL_FN_SYS_DBNAME   = $00000002;
  SQL_FN_SYS_IFNULL   = $00000004;

  // SQL_TIMEDATE_ADD_INTERVALS and SQL_TIMEDATE_DIFF_INTERVALS functions
  SQL_FN_TSI_FRAC_SECOND = $00000001;
  SQL_FN_TSI_SECOND      = $00000002;
  SQL_FN_TSI_MINUTE      = $00000004;
  SQL_FN_TSI_HOUR        = $00000008;
  SQL_FN_TSI_DAY         = $00000010;
  SQL_FN_TSI_WEEK        = $00000020;
  SQL_FN_TSI_MONTH       = $00000040;
  SQL_FN_TSI_QUARTER     = $00000080;
  SQL_FN_TSI_YEAR        = $00000100;

  // Supported SQLFetchScroll FetchOrientation's
  SQL_CA1_NEXT     = $00000001;
  SQL_CA1_ABSOLUTE = $00000002;
  SQL_CA1_RELATIVE = $00000004;
  SQL_CA1_BOOKMARK = $00000008;

  // Supported SQLSetPos LockType's
  SQL_CA1_LOCK_NO_CHANGE = $00000040;
  SQL_CA1_LOCK_EXCLUSIVE = $00000080;
  SQL_CA1_LOCK_UNLOCK    = $00000100;

  // Supported SQLSetPos Operations
  SQL_CA1_POS_POSITION = $00000200;
  SQL_CA1_POS_UPDATE   = $00000400;
  SQL_CA1_POS_DELETE   = $00000800;
  SQL_CA1_POS_REFRESH  = $00001000;

  // Positioned updates and deletes
  SQL_CA1_POSITIONED_UPDATE = $00002000;
  SQL_CA1_POSITIONED_DELETE = $00004000;
  SQL_CA1_SELECT_FOR_UPDATE = $00008000;

  // Supported SQLBulkOperations operations
  SQL_CA1_BULK_ADD                = $00010000;
  SQL_CA1_BULK_UPDATE_BY_BOOKMARK = $00020000;
  SQL_CA1_BULK_DELETE_BY_BOOKMARK = $00040000;
  SQL_CA1_BULK_FETCH_BY_BOOKMARK  = $00080000;

  // Supported values for SQL_ATTR_SCROLL_CONCURRENCY
  SQL_CA2_READ_ONLY_CONCURRENCY  = $00000001;
  SQL_CA2_LOCK_CONCURRENCY       = $00000002;
  SQL_CA2_OPT_ROWVER_CONCURRENCY = $00000004;
  SQL_CA2_OPT_VALUES_CONCURRENCY = $00000008;

  // Sensitivity of the cursor to its own inserts, deletes, and updates
  SQL_CA2_SENSITIVITY_ADDITIONS = $00000010;
  SQL_CA2_SENSITIVITY_DELETIONS = $00000020;
  SQL_CA2_SENSITIVITY_UPDATES   = $00000040;

  // Semantics of SQL_ATTR_MAX_ROWS
  SQL_CA2_MAX_ROWS_SELECT      = $00000080;
  SQL_CA2_MAX_ROWS_INSERT      = $00000100;
  SQL_CA2_MAX_ROWS_DELETE      = $00000200;
  SQL_CA2_MAX_ROWS_UPDATE      = $00000400;
  SQL_CA2_MAX_ROWS_CATALOG     = $00000800;
  SQL_CA2_MAX_ROWS_AFFECTS_ALL = (SQL_CA2_MAX_ROWS_SELECT or
                                  SQL_CA2_MAX_ROWS_INSERT or
                                  SQL_CA2_MAX_ROWS_DELETE or
                                  SQL_CA2_MAX_ROWS_UPDATE or
                                  SQL_CA2_MAX_ROWS_CATALOG);

  // Semantics of SQL_DIAG_CURSOR_ROW_COUNT
  SQL_CA2_CRC_EXACT       = $00001000;
  SQL_CA2_CRC_APPROXIMATE = $00002000;

  // The kinds of positioned statements that can be simulated
  SQL_CA2_SIMULATE_NON_UNIQUE = $00004000;
  SQL_CA2_SIMULATE_TRY_UNIQUE = $00008000;
  SQL_CA2_SIMULATE_UNIQUE     = $00010000;

  // SQL_ODBC_API_CONFORMANCE values
  SQL_OAC_NONE   = $0000;
  SQL_OAC_LEVEL1 = $0001;
  SQL_OAC_LEVEL2 = $0002;

  // SQL_ODBC_SAG_CLI_CONFORMANCE values
  SQL_OSCC_NOT_COMPLIANT = $0000;
  SQL_OSCC_COMPLIANT     = $0001;

  // SQL_ODBC_SQL_CONFORMANCE values
  SQL_OSC_MINIMUM  = $0000;
  SQL_OSC_CORE     = $0001;
  SQL_OSC_EXTENDED = $0002;

  // SQL_CONCAT_NULL_BEHAVIOR values
  SQL_CB_NULL     = $0000;
  SQL_CB_NON_NULL = $0001;

  // SQL_SCROLL_OPTIONS masks
  SQL_SO_FORWARD_ONLY  = $00000001;
  SQL_SO_KEYSET_DRIVEN = $00000002;
  SQL_SO_DYNAMIC       = $00000004;
  SQL_SO_MIXED         = $00000008;
  SQL_SO_STATIC        = $00000010;

  // SQL_FETCH_DIRECTION masks
  SQL_FD_FETCH_RESUME   = $00000040; // SQL_FETCH_RESUME is no longer supported
  SQL_FD_FETCH_BOOKMARK = $00000080;

  // SQL_TXN_ISOLATION_OPTION masks
  SQL_TXN_VERSIONING = $00000010;    // SQL_TXN_VERSIONING is no longer supported

  // SQL_CORRELATION_NAME values
  SQL_CN_NONE      = $0000;
  SQL_CN_DIFFERENT = $0001;
  SQL_CN_ANY       = $0002;

  // SQL_NON_NULLABLE_COLUMNS values
  SQL_NNC_NULL     = $0000;
  SQL_NNC_NON_NULL = $0001;

  // SQL_NULL_COLLATION values
  SQL_NC_START = $0002;
  SQL_NC_END   = $0004;

  // SQL_FILE_USAGE values
  SQL_FILE_NOT_SUPPORTED = $0000;
  SQL_FILE_TABLE         = $0001;
  SQL_FILE_QUALIFIER     = $0002;
  SQL_FILE_CATALOG       = SQL_FILE_QUALIFIER; // ODBC 3.0

  // SQL_GETDATA_EXTENSIONS values
  SQL_GD_BLOCK = $00000004;
  SQL_GD_BOUND = $00000008;

  // SQL_POSITIONED_STATEMENTS masks
  SQL_PS_POSITIONED_DELETE = $00000001;
  SQL_PS_POSITIONED_UPDATE = $00000002;
  SQL_PS_SELECT_FOR_UPDATE = $00000004;

  // SQL_GROUP_BY values
  SQL_GB_NOT_SUPPORTED            = $0000;
  SQL_GB_GROUP_BY_EQUALS_SELECT   = $0001;
  SQL_GB_GROUP_BY_CONTAINS_SELECT = $0002;
  SQL_GB_NO_RELATION              = $0003;
  SQL_GB_COLLATE                  = $0004;

  // SQL_OWNER_USAGE masks
  SQL_OU_DML_STATEMENTS       = $00000001;
  SQL_OU_PROCEDURE_INVOCATION = $00000002;
  SQL_OU_TABLE_DEFINITION     = $00000004;
  SQL_OU_INDEX_DEFINITION     = $00000008;
  SQL_OU_PRIVILEGE_DEFINITION = $00000010;

  // SQL_SCHEMA_USAGE masks
  SQL_SU_DML_STATEMENTS       = SQL_OU_DML_STATEMENTS;
  SQL_SU_PROCEDURE_INVOCATION = SQL_OU_PROCEDURE_INVOCATION;
  SQL_SU_TABLE_DEFINITION     = SQL_OU_TABLE_DEFINITION;
  SQL_SU_INDEX_DEFINITION     = SQL_OU_INDEX_DEFINITION;
  SQL_SU_PRIVILEGE_DEFINITION = SQL_OU_PRIVILEGE_DEFINITION;

  // SQL_QUALIFIER_USAGE masks
  SQL_QU_DML_STATEMENTS       = $00000001;
  SQL_QU_PROCEDURE_INVOCATION = $00000002;
  SQL_QU_TABLE_DEFINITION     = $00000004;
  SQL_QU_INDEX_DEFINITION     = $00000008;
  SQL_QU_PRIVILEGE_DEFINITION = $00000010;

  // SQL_CATALOG_USAGE masks
  SQL_CU_DML_STATEMENTS       = SQL_QU_DML_STATEMENTS;
  SQL_CU_PROCEDURE_INVOCATION = SQL_QU_PROCEDURE_INVOCATION;
  SQL_CU_TABLE_DEFINITION     = SQL_QU_TABLE_DEFINITION;
  SQL_CU_INDEX_DEFINITION     = SQL_QU_INDEX_DEFINITION;
  SQL_CU_PRIVILEGE_DEFINITION = SQL_QU_PRIVILEGE_DEFINITION;

  // SQL_SUBQUERIES masks
  SQL_SQ_COMPARISON            = $00000001;
  SQL_SQ_EXISTS                = $00000002;
  SQL_SQ_IN                    = $00000004;
  SQL_SQ_QUANTIFIED            = $00000008;
  SQL_SQ_CORRELATED_SUBQUERIES = $00000010;

  // SQL_UNION masks
  SQL_U_UNION     = $00000001;
  SQL_U_UNION_ALL = $00000002;

  // SQL_BOOKMARK_PERSISTENCE values
  SQL_BP_CLOSE       = $00000001;
  SQL_BP_DELETE      = $00000002;
  SQL_BP_DROP        = $00000004;
  SQL_BP_TRANSACTION = $00000008;
  SQL_BP_UPDATE      = $00000010;
  SQL_BP_OTHER_HSTMT = $00000020;
  SQL_BP_SCROLL      = $00000040;

  // SQL_STATIC_SENSITIVITY values
  SQL_SS_ADDITIONS = $00000001;
  SQL_SS_DELETIONS = $00000002;
  SQL_SS_UPDATES   = $00000004;

  // SQL_VIEW values
  SQL_CV_CREATE_VIEW  = $00000001;
  SQL_CV_CHECK_OPTION = $00000002;
  SQL_CV_CASCADED     = $00000004;
  SQL_CV_LOCAL        = $00000008;

  // SQL_LOCK_TYPES masks
  SQL_LCK_NO_CHANGE = $00000001;
  SQL_LCK_EXCLUSIVE = $00000002;
  SQL_LCK_UNLOCK    = $00000004;

  // SQL_POS_OPERATIONS masks
  SQL_POS_POSITION = $00000001;
  SQL_POS_REFRESH  = $00000002;
  SQL_POS_UPDATE   = $00000004;
  SQL_POS_DELETE   = $00000008;
  SQL_POS_ADD      = $00000010;

  // SQL_QUALIFIER_LOCATION values
  SQL_QL_START = $0001;
  SQL_QL_END   = $0002;

  // Here start return values for ODBC 3.0 SQLGetInfo
  // SQL_AGGREGATE_FUNCTIONS bitmasks
  SQL_AF_AVG      = $00000001;
  SQL_AF_COUNT    = $00000002;
  SQL_AF_MAX      = $00000004;
  SQL_AF_MIN      = $00000008;
  SQL_AF_SUM      = $00000010;
  SQL_AF_DISTINCT = $00000020;
  SQL_AF_ALL      = $00000040;

  // SQL_SQL_CONFORMANCE bit masks
  SQL_SC_SQL92_ENTRY            = $00000001;
  SQL_SC_FIPS127_2_TRANSITIONAL = $00000002;
  SQL_SC_SQL92_INTERMEDIATE     = $00000004;
  SQL_SC_SQL92_FULL             = $00000008;

  // SQL_DATETIME_LITERALS masks
  SQL_DL_SQL92_DATE                      = $00000001;
  SQL_DL_SQL92_TIME                      = $00000002;
  SQL_DL_SQL92_TIMESTAMP                 = $00000004;
  SQL_DL_SQL92_INTERVAL_YEAR             = $00000008;
  SQL_DL_SQL92_INTERVAL_MONTH            = $00000010;
  SQL_DL_SQL92_INTERVAL_DAY              = $00000020;
  SQL_DL_SQL92_INTERVAL_HOUR             = $00000040;
  SQL_DL_SQL92_INTERVAL_MINUTE           = $00000080;
  SQL_DL_SQL92_INTERVAL_SECOND           = $00000100;
  SQL_DL_SQL92_INTERVAL_YEAR_TO_MONTH    = $00000200;
  SQL_DL_SQL92_INTERVAL_DAY_TO_HOUR      = $00000400;
  SQL_DL_SQL92_INTERVAL_DAY_TO_MINUTE    = $00000800;
  SQL_DL_SQL92_INTERVAL_DAY_TO_SECOND    = $00001000;
  SQL_DL_SQL92_INTERVAL_HOUR_TO_MINUTE   = $00002000;
  SQL_DL_SQL92_INTERVAL_HOUR_TO_SECOND   = $00004000;
  SQL_DL_SQL92_INTERVAL_MINUTE_TO_SECOND = $00008000;

  // SQL_CATALOG_LOCATION values
  SQL_CL_START = SQL_QL_START;
  SQL_CL_END   = SQL_QL_END;

  // Values for SQL_BATCH_ROW_COUNT
  SQL_BRC_PROCEDURES = $0000001;
  SQL_BRC_EXPLICIT   = $0000002;
  SQL_BRC_ROLLED_UP  = $0000004;

  // Bitmasks for SQL_BATCH_SUPPORT
  SQL_BS_SELECT_EXPLICIT    = $00000001;
  SQL_BS_ROW_COUNT_EXPLICIT = $00000002;
  SQL_BS_SELECT_PROC        = $00000004;
  SQL_BS_ROW_COUNT_PROC     = $00000008;

  // Values for SQL_PARAM_ARRAY_ROW_COUNTS getinfo
  SQL_PARC_BATCH    = 1;
  SQL_PARC_NO_BATCH = 2;

  // Values for SQL_PARAM_ARRAY_SELECTS
  SQL_PAS_BATCH     = 1;
  SQL_PAS_NO_BATCH  = 2;
  SQL_PAS_NO_SELECT = 3;

  // Bitmasks for SQL_INDEX_KEYWORDS
  SQL_IK_NONE = $00000000;
  SQL_IK_ASC  = $00000001;
  SQL_IK_DESC = $00000002;
  SQL_IK_ALL  = (SQL_IK_ASC or SQL_IK_DESC);

  // Bitmasks for SQL_INFO_SCHEMA_VIEWS
  SQL_ISV_ASSERTIONS              = $00000001;
  SQL_ISV_CHARACTER_SETS          = $00000002;
  SQL_ISV_CHECK_CONSTRAINTS       = $00000004;
  SQL_ISV_COLLATIONS              = $00000008;
  SQL_ISV_COLUMN_DOMAIN_USAGE     = $00000010;
  SQL_ISV_COLUMN_PRIVILEGES       = $00000020;
  SQL_ISV_COLUMNS                 = $00000040;
  SQL_ISV_CONSTRAINT_COLUMN_USAGE = $00000080;
  SQL_ISV_CONSTRAINT_TABLE_USAGE  = $00000100;
  SQL_ISV_DOMAIN_CONSTRAINTS      = $00000200;
  SQL_ISV_DOMAINS                 = $00000400;
  SQL_ISV_KEY_COLUMN_USAGE        = $00000800;
  SQL_ISV_REFERENTIAL_CONSTRAINTS = $00001000;
  SQL_ISV_SCHEMATA                = $00002000;
  SQL_ISV_SQL_LANGUAGES           = $00004000;
  SQL_ISV_TABLE_CONSTRAINTS       = $00008000;
  SQL_ISV_TABLE_PRIVILEGES        = $00010000;
  SQL_ISV_TABLES                  = $00020000;
  SQL_ISV_TRANSLATIONS            = $00040000;
  SQL_ISV_USAGE_PRIVILEGES        = $00080000;
  SQL_ISV_VIEW_COLUMN_USAGE       = $00100000;
  SQL_ISV_VIEW_TABLE_USAGE        = $00200000;
  SQL_ISV_VIEWS                   = $00400000;

  // Bitmasks for SQL_ALTER_DOMAIN
  SQL_AD_CONSTRAINT_NAME_DEFINITION         = $00000001;
  SQL_AD_ADD_DOMAIN_CONSTRAINT              = $00000002;
  SQL_AD_DROP_DOMAIN_CONSTRAINT             = $00000004;
  SQL_AD_ADD_DOMAIN_DEFAULT                 = $00000008;
  SQL_AD_DROP_DOMAIN_DEFAULT                = $00000010;
  SQL_AD_ADD_CONSTRAINT_INITIALLY_DEFERRED  = $00000020;
  SQL_AD_ADD_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_AD_ADD_CONSTRAINT_DEFERRABLE          = $00000080;
  SQL_AD_ADD_CONSTRAINT_NON_DEFERRABLE      = $00000100;

  // SQL_CREATE_SCHEMA bitmasks
  SQL_CS_CREATE_SCHEMA         = $00000001;
  SQL_CS_AUTHORIZATION         = $00000002;
  SQL_CS_DEFAULT_CHARACTER_SET = $00000004;

  // SQL_CREATE_TRANSLATION bitmasks
  SQL_CTR_CREATE_TRANSLATION = $00000001;

  // SQL_CREATE_ASSERTION bitmasks
  SQL_CA_CREATE_ASSERTION               = $00000001;
  SQL_CA_CONSTRAINT_INITIALLY_DEFERRED  = $00000010;
  SQL_CA_CONSTRAINT_INITIALLY_IMMEDIATE = $00000020;
  SQL_CA_CONSTRAINT_DEFERRABLE          = $00000040;
  SQL_CA_CONSTRAINT_NON_DEFERRABLE      = $00000080;

  // SQL_CREATE_CHARACTER_SET bitmasks
  SQL_CCS_CREATE_CHARACTER_SET = $00000001;
  SQL_CCS_COLLATE_CLAUSE       = $00000002;
  SQL_CCS_LIMITED_COLLATION    = $00000004;

  // SQL_CREATE_COLLATION bitmasks
  SQL_CCOL_CREATE_COLLATION = $00000001;

  // SQL_CREATE_DOMAIN bitmasks
  SQL_CDO_CREATE_DOMAIN                  = $00000001;
  SQL_CDO_DEFAULT                        = $00000002;
  SQL_CDO_CONSTRAINT                     = $00000004;
  SQL_CDO_COLLATION                      = $00000008;
  SQL_CDO_CONSTRAINT_NAME_DEFINITION     = $00000010;
  SQL_CDO_CONSTRAINT_INITIALLY_DEFERRED  = $00000020;
  SQL_CDO_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_CDO_CONSTRAINT_DEFERRABLE          = $00000080;
  SQL_CDO_CONSTRAINT_NON_DEFERRABLE      = $00000100;

  // SQL_CREATE_TABLE bitmasks
  SQL_CT_CREATE_TABLE                   = $00000001;
  SQL_CT_COMMIT_PRESERVE                = $00000002;
  SQL_CT_COMMIT_DELETE                  = $00000004;
  SQL_CT_GLOBAL_TEMPORARY               = $00000008;
  SQL_CT_LOCAL_TEMPORARY                = $00000010;
  SQL_CT_CONSTRAINT_INITIALLY_DEFERRED  = $00000020;
  SQL_CT_CONSTRAINT_INITIALLY_IMMEDIATE = $00000040;
  SQL_CT_CONSTRAINT_DEFERRABLE          = $00000080;
  SQL_CT_CONSTRAINT_NON_DEFERRABLE      = $00000100;
  SQL_CT_COLUMN_CONSTRAINT              = $00000200;
  SQL_CT_COLUMN_DEFAULT                 = $00000400;
  SQL_CT_COLUMN_COLLATION               = $00000800;
  SQL_CT_TABLE_CONSTRAINT               = $00001000;
  SQL_CT_CONSTRAINT_NAME_DEFINITION     = $00002000;

  // SQL_DDL_INDEX bitmasks
  SQL_DI_CREATE_INDEX = $00000001;
  SQL_DI_DROP_INDEX   = $00000002;

  // SQL_DROP_COLLATION bitmasks
  SQL_DC_DROP_COLLATION = $00000001;

  // SQL_DROP_DOMAIN bitmasks
  SQL_DD_DROP_DOMAIN = $00000001;
  SQL_DD_RESTRICT    = $00000002;
  SQL_DD_CASCADE     = $00000004;

  // SQL_DROP_SCHEMA bitmasks
  SQL_DS_DROP_SCHEMA = $00000001;
  SQL_DS_RESTRICT    = $00000002;
  SQL_DS_CASCADE     = $00000004;

  // SQL_DROP_CHARACTER_SET bitmasks
  SQL_DCS_DROP_CHARACTER_SET = $00000001;

  // SQL_DROP_ASSERTION bitmasks
  SQL_DA_DROP_ASSERTION = $00000001;

  // SQL_DROP_TABLE bitmasks
  SQL_DT_DROP_TABLE = $00000001;
  SQL_DT_RESTRICT   = $00000002;
  SQL_DT_CASCADE    = $00000004;

  // SQL_DROP_TRANSLATION bitmasks
  SQL_DTR_DROP_TRANSLATION = $00000001;

  // SQL_DROP_VIEW bitmasks
  SQL_DV_DROP_VIEW = $00000001;
  SQL_DV_RESTRICT  = $00000002;
  SQL_DV_CASCADE   = $00000004;

  // SQL_INSERT_STATEMENT bitmasks
  SQL_IS_INSERT_LITERALS = $00000001;
  SQL_IS_INSERT_SEARCHED = $00000002;
  SQL_IS_SELECT_INTO     = $00000004;

  // SQL_ODBC_INTERFACE_CONFORMANCE values
  SQL_OIC_CORE   = 1;
  SQL_OIC_LEVEL1 = 2;
  SQL_OIC_LEVEL2 = 3;

  // SQL_SQL92_FOREIGN_KEY_DELETE_RULE bitmasks
  SQL_SFKD_CASCADE     = $00000001;
  SQL_SFKD_NO_ACTION   = $00000002;
  SQL_SFKD_SET_DEFAULT = $00000004;
  SQL_SFKD_SET_NULL    = $00000008;

  // SQL_SQL92_FOREIGN_KEY_UPDATE_RULE bitmasks
  SQL_SFKU_CASCADE     = $00000001;
  SQL_SFKU_NO_ACTION   = $00000002;
  SQL_SFKU_SET_DEFAULT = $00000004;
  SQL_SFKU_SET_NULL    = $00000008;

  // SQL_SQL92_GRANT bitmasks
  SQL_SG_USAGE_ON_DOMAIN        = $00000001;
  SQL_SG_USAGE_ON_CHARACTER_SET = $00000002;
  SQL_SG_USAGE_ON_COLLATION     = $00000004;
  SQL_SG_USAGE_ON_TRANSLATION   = $00000008;
  SQL_SG_WITH_GRANT_OPTION      = $00000010;
  SQL_SG_DELETE_TABLE           = $00000020;
  SQL_SG_INSERT_TABLE           = $00000040;
  SQL_SG_INSERT_COLUMN          = $00000080;
  SQL_SG_REFERENCES_TABLE       = $00000100;
  SQL_SG_REFERENCES_COLUMN      = $00000200;
  SQL_SG_SELECT_TABLE           = $00000400;
  SQL_SG_UPDATE_TABLE           = $00000800;
  SQL_SG_UPDATE_COLUMN          = $00001000;

  // SQL_SQL92_PREDICATES bitmasks
  SQL_SP_EXISTS                = $00000001;
  SQL_SP_ISNOTNULL             = $00000002;
  SQL_SP_ISNULL                = $00000004;
  SQL_SP_MATCH_FULL            = $00000008;
  SQL_SP_MATCH_PARTIAL         = $00000010;
  SQL_SP_MATCH_UNIQUE_FULL     = $00000020;
  SQL_SP_MATCH_UNIQUE_PARTIAL  = $00000040;
  SQL_SP_OVERLAPS              = $00000080;
  SQL_SP_UNIQUE                = $00000100;
  SQL_SP_LIKE                  = $00000200;
  SQL_SP_IN                    = $00000400;
  SQL_SP_BETWEEN               = $00000800;
  SQL_SP_COMPARISON            = $00001000;
  SQL_SP_QUANTIFIED_COMPARISON = $00002000;

  // SQL_SQL92_RELATIONAL_JOIN_OPERATORS bitmasks
  SQL_SRJO_CORRESPONDING_CLAUSE = $00000001;
  SQL_SRJO_CROSS_JOIN           = $00000002;
  SQL_SRJO_EXCEPT_JOIN          = $00000004;
  SQL_SRJO_FULL_OUTER_JOIN      = $00000008;
  SQL_SRJO_INNER_JOIN           = $00000010;
  SQL_SRJO_INTERSECT_JOIN       = $00000020;
  SQL_SRJO_LEFT_OUTER_JOIN      = $00000040;
  SQL_SRJO_NATURAL_JOIN         = $00000080;
  SQL_SRJO_RIGHT_OUTER_JOIN     = $00000100;
  SQL_SRJO_UNION_JOIN           = $00000200;

  // SQL_SQL92_REVOKE bitmasks
  SQL_SR_USAGE_ON_DOMAIN        = $00000001;
  SQL_SR_USAGE_ON_CHARACTER_SET = $00000002;
  SQL_SR_USAGE_ON_COLLATION     = $00000004;
  SQL_SR_USAGE_ON_TRANSLATION   = $00000008;
  SQL_SR_GRANT_OPTION_FOR       = $00000010;
  SQL_SR_CASCADE                = $00000020;
  SQL_SR_RESTRICT               = $00000040;
  SQL_SR_DELETE_TABLE           = $00000080;
  SQL_SR_INSERT_TABLE           = $00000100;
  SQL_SR_INSERT_COLUMN          = $00000200;
  SQL_SR_REFERENCES_TABLE       = $00000400;
  SQL_SR_REFERENCES_COLUMN      = $00000800;
  SQL_SR_SELECT_TABLE           = $00001000;
  SQL_SR_UPDATE_TABLE           = $00002000;
  SQL_SR_UPDATE_COLUMN          = $00004000;

  // SQL_SQL92_ROW_VALUE_CONSTRUCTOR bitmasks
  SQL_SRVC_VALUE_EXPRESSION = $00000001;
  SQL_SRVC_NULL             = $00000002;
  SQL_SRVC_DEFAULT          = $00000004;
  SQL_SRVC_ROW_SUBQUERY     = $00000008;

  // SQL_SQL92_VALUE_EXPRESSIONS bitmasks
  SQL_SVE_CASE     = $00000001;
  SQL_SVE_CAST     = $00000002;
  SQL_SVE_COALESCE = $00000004;
  SQL_SVE_NULLIF   = $00000008;

  // SQL_STANDARD_CLI_CONFORMANCE bitmasks
  SQL_SCC_XOPEN_CLI_VERSION1 = $00000001;
  SQL_SCC_ISO92_CLI          = $00000002;

  // SQL_UNION_STATEMENT bitmasks
  SQL_US_UNION     = SQL_U_UNION;
  SQL_US_UNION_ALL = SQL_U_UNION_ALL;

  // SQL_DTC_TRANSITION_COST bitmasks
  SQL_DTC_ENLIST_EXPENSIVE   = $00000001;
  SQL_DTC_UNENLIST_EXPENSIVE = $00000002;

  // additional SQLDataSources fetch directions
  SQL_FETCH_FIRST_USER   = 31;
  SQL_FETCH_FIRST_SYSTEM = 32;

  // Defines for SQLSetPos
  SQL_ENTIRE_ROWSET = 0;

  // Operations in SQLSetPos
  SQL_POSITION = 0; // 1.0 FALSE
  SQL_REFRESH  = 1; // 1.0 TRUE
  SQL_UPDATE   = 2;
  SQL_DELETE   = 3;

  // Operations in SQLBulkOperations
  SQL_ADD                     = 4;
  SQL_SETPOS_MAX_OPTION_VALUE = SQL_ADD;
  SQL_UPDATE_BY_BOOKMARK      = 5;
  SQL_DELETE_BY_BOOKMARK      = 6;
  SQL_FETCH_BY_BOOKMARK       = 7;

  // Lock options in SQLSetPos
  SQL_LOCK_NO_CHANGE = 0; // 1.0 FALSE
  SQL_LOCK_EXCLUSIVE = 1; // 1.0 TRUE
  SQL_LOCK_UNLOCK    = 2;

  SQL_SETPOS_MAX_LOCK_VALUE = SQL_LOCK_UNLOCK;

const
  // Column types and scopes in SQLSpecialColumns.
  SQL_BEST_ROWID = 1;
  SQL_ROWVER     = 2;

  // Defines for SQLSpecialColumns (returned in the Result set
  SQL_PC_NOT_PSEUDO = 1;

  // Defines for SQLStatistics
  SQL_QUICK  = 0;
  SQL_ENSURE = 1;

  // Defines for SQLStatistics (returned in the Result set)
  SQL_TABLE_STAT = 0;

  // Defines for SQLTables
  SQL_ALL_CATALOGS    = '%';
  SQL_ALL_SCHEMAS     = '%';
  SQL_ALL_TABLE_TYPES = '%';

  // Options for SQLDriverConnect
  SQL_DRIVER_NOPROMPT          = 0;
  SQL_DRIVER_COMPLETE          = 1;
  SQL_DRIVER_PROMPT            = 2;
  SQL_DRIVER_COMPLETE_REQUIRED = 3;

type
  _SQLDriverConnect = function(
    ConnectionHandle:     TSQLHDbc;
    WindowHandle:         HWND;
    InConnectionString:   _PChar;
    StringLength1:        smallint;
    OutConnectionString:  IntPtr;
    BufferLength:         smallint;
    var StringLength2Ptr: smallint;
    DriverCompletion:     word
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

const
  // SQLExtendedFetch
  SQL_FETCH_BOOKMARK = 8;

  // SQLExtendedFetch row status element values
  SQL_ROW_SUCCESS           = 0;
  SQL_ROW_DELETED           = 1;
  SQL_ROW_UPDATED           = 2;
  SQL_ROW_NOROW             = 3;
  SQL_ROW_ADDED             = 4;
  SQL_ROW_ERROR             = 5;
  SQL_ROW_SUCCESS_WITH_INFO = 6;
  SQL_ROW_PROCEED           = 0;
  SQL_ROW_IGNORE            = 1;

  // Value for SQL_DESC_ARRAY_STATUS_PTR
  SQL_PARAM_SUCCESS           = 0;
  SQL_PARAM_SUCCESS_WITH_INFO = 6;
  SQL_PARAM_ERROR             = 5;
  SQL_PARAM_UNUSED            = 7;
  SQL_PARAM_DIAG_UNAVAILABLE  = 1;
  SQL_PARAM_PROCEED           = 0;
  SQL_PARAM_IGNORE            = 1;

  // Defines for SQLForeignKeys (UPDATE_RULE and DELETE_RULE)
  SQL_CASCADE     = 0;
  SQL_RESTRICT    = 1;
  SQL_SET_NULL    = 2;
  SQL_NO_ACTION   = 3;
  SQL_SET_DEFAULT = 4;

  // Note that the following are in a different column of SQLForeignKeys than
  // the previous #defines.   These are for DEFERRABILITY.
  SQL_INITIALLY_DEFERRED  = 5;
  SQL_INITIALLY_IMMEDIATE = 6;
  SQL_NOT_DEFERRABLE      = 7;

  // Defines for SQLProcedures (returned in the Result set)
  SQL_PT_UNKNOWN   = 0;
  SQL_PT_PROCEDURE = 1;
  SQL_PT_FUNCTION  = 2;

type
  _SQLBrowseConnect = function(
    HDbc:              TSQLHDbc;
    szConnStrIn:       _PChar;
    cbConnStrIn:       smallint;
    szConnStrOut:      IntPtr;
    cbConnStrOutMax:   smallint;
    var pcbConnStrOut: smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLBulkOperations = function(
    StatementHandle: TSQLHStmt;
    Operation:       smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLColAttributes = function(
    HStmt:       TSQLHStmt;
    icol:        word;
    fDescType:   word;
    rgbDesc:     IntPtr;
    cbDescMax:   smallint;
    var pcbDesc: smallint;
    var pfDesc:  NativeInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLColumnPrivileges = function(
    HStmt:             TSQLHStmt;
    szCatalogName:     _PChar;
    cbCatalogName:     smallint;
    szSchemaName:      _PChar;
    cbSchemaName:      smallint;
    szTableName:       _PChar;
    cbTableName:       smallint;
    szColumnName:      _PChar;
    cbColumnName:      smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLDescribeParam = function(
    HStmt:           TSQLHStmt;
    ipar:            word;
    var pfSQLType:   smallint;
    var pcbParamDef: NativeUInt;
    var pibScale:    smallint;
    var pfNullable:  smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLExtendedFetch = function(
    HStmt:            TSQLHStmt;
    fFetchType:       word;
    irow:             NativeInt;
    var pcrow:        NativeUInt;
    var rgfRowStatus: word
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLForeignKeys = function(
    HStmt:            TSQLHStmt;
    szPkCatalogName:  _PChar;
    cbPkCatalogName:  smallint;
    szPkSchemaName:   _PChar;
    cbPkSchemaName:   smallint;
    szPkTableName:    _PChar;
    cbPkTableName:    smallint;
    szFkCatalogName:  _PChar;
    cbFkCatalogName:  smallint;
    szFkSchemaName:   _PChar;
    cbFkSchemaName:   smallint;
    szFkTableName:    _PChar;
    cbFkTableName:    smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLMoreResults = function(
    HStmt: TSQLHStmt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLNativeSQL = function(
    ConnectionHandle:   TSQLHDbc;
    InStatementText:    _PChar;
    TextLength1:        integer;
    OutStatementText:   IntPtr;
    BufferLength:       integer;
    var TextLength2Ptr: integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLNumParams = function(
    HStmt:     TSQLHStmt;
    var pcpar: smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLParamOptions = function(
    HStmt:     TSQLHStmt;
    crow:      NativeUInt;
    var pirow: NativeUInt
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLPrimaryKeys = function(
    HStmt:         TSQLHStmt;
    szCatalogName: _PChar;
    cbCatalogName: smallint;
    szSchemaName:  _PChar;
    cbSchemaName:  smallint;
    szTableName:   _PChar;
    cbTableName:   smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLProcedureColumns = function(
    HStmt:         TSQLHStmt;
    szCatalogName: _PChar;
    cbCatalogName: smallint;
    szSchemaName:  _PChar;
    cbSchemaName:  smallint;
    szProcName:    _PChar;
    cbProcName:    smallint;
    szColumnName:  _PChar;
    cbColumnName:  smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLProcedures = function(
    HStmt:         TSQLHStmt;
    szCatalogName: _PChar;
    cbCatalogName: smallint;
    szSchemaName:  _PChar;
    cbSchemaName:  smallint;
    szProcName:    _PChar;
    cbProcName:    smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLSetPos = function(
    HStmt:   TSQLHStmt;
    irow:    word;
    fOption: word;
    fLock:   word
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLTablePrivileges = function(
    HStmt:             TSQLHStmt;
    szCatalogName:     _PChar;
    cbCatalogName:     smallint;
    szSchemaName:      _PChar;
    cbSchemaName:      smallint;
    szTableName:       _PChar;
    cbTableName:       smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLDrivers = function(
    EnvironmentHandle:  TSQLHEnv;
    Direction:          word;
    DriverDescription:  IntPtr;
    BufferLength1:      smallint;
    var DescriptionLengthPtr: smallint;
    DriverAttributes:   IntPtr;
    BufferLength2:      smallint;
    var AttributesLengthPtr:  smallint
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLBindParameter = function(
     StatementHandle: TSQLHStmt;
     ParameterNumber: word;
     InputOutputType: smallint;
     ValueType: smallint;
     ParameterType: smallint;
     ColumnSize: NativeUInt;
     DecimalDigits: smallint;
     ParameterValuePtr: IntPtr;
     BufferLength: NativeInt;
     StrLen_or_IndPtr: IntPtr
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

function SQL_LEN_DATA_AT_EXEC(Length: integer): integer;

type
  TODBCCli = class
  protected
  {$IFDEF MSWINDOWS}
    FLib: HMODULE;
  {$ENDIF}
  {$IFDEF LINUX}
    FLib: IntPtr;
  {$ENDIF}
    FLock: TCriticalSection;

    function GetLibName: string; virtual;
  {$IFNDEF CLR}
    function GetProc(const Name: string): IntPtr;
  {$ENDIF}
    procedure InitFunctions; virtual;

  public
    SQLHEnv: TSQLHEnv;

    SQLAllocHandle: _SQLAllocHandle;
    SQLBindCol: _SQLBindCol;
    SQLBindParameter: _SQLBindParameter;
    SQLCloseCursor: _SQLCloseCursor;
    SQLColAttributeInt: _SQLColAttributeInt;
    SQLColAttributeStr: _SQLColAttributeStr;
    SQLColumns: _SQLColumns;
    SQLConnect: _SQLConnect;
    SQLDataSources: _SQLDataSources;
    SQLDescribeCol: _SQLDescribeCol;
    SQLDisconnect: _SQLDisconnect;
    SQLDriverConnect: _SQLDriverConnect;
    SQLDrivers: _SQLDrivers;
    SQLEndTran: _SQLEndTran;
    SQLExecDirect: _SQLExecDirect;
    SQLExecute: _SQLExecute;
    SQLFetch: _SQLFetch;
    SQLFreeHandle: _SQLFreeHandle;
    SQLGetConnectAttrInt: _SQLGetConnectAttrInt;
    SQLGetConnectAttrStr: _SQLGetConnectAttrStr;
    SQLGetData: _SQLGetData;
    SQLGetDescFieldInt: _SQLGetDescFieldInt;
    SQLGetDescFieldStr: _SQLGetDescFieldStr;
    SQLGetDiagRec: _SQLGetDiagRec;
    SQLGetInfoInt: _SQLGetInfoInt;
    SQLGetInfoStr: _SQLGetInfoStr;
    SQLGetStmtAttrPtr: _SQLGetStmtAttrPtr;
    SQLMoreResults: _SQLMoreResults;
    SQLNumResultCols: _SQLNumResultCols;
    SQLParamData: _SQLParamData;
    SQLPrepare: _SQLPrepare;
    SQLProcedureColumns: _SQLProcedureColumns;
    SQLProcedures: _SQLProcedures;
    SQLPutData: _SQLPutData;
    SQLRowCount: _SQLRowCount;
    SQLSetConnectAttrInt: _SQLSetConnectAttrInt;
    SQLSetConnectAttrStr: _SQLSetConnectAttrStr;
    SQLSetDescFieldPtr: _SQLSetDescFieldPtr;
    SQLSetDescFieldInt: _SQLSetDescFieldInt;
    SQLSetEnvAttrInt: _SQLSetEnvAttrInt;
    SQLSetStmtAttrPtr: _SQLSetStmtAttrPtr;
    SQLSetStmtAttrInt: _SQLSetStmtAttrInt;
    SQLSetStmtAttrStr: _SQLSetStmtAttrStr;
    SQLSpecialColumns: _SQLSpecialColumns;
    SQLStatistics: _SQLStatistics;
    SQLTables: _SQLTables;

    constructor Create(Lock: TCriticalSection);
    destructor Destroy; override;

    procedure LoadLib;
    procedure FreeLib;
    function IsLoaded: boolean;

    procedure InitEnvironment;
    procedure FreeEnvironment;
    function IsInited: boolean;

    function GetODBCError(HandleType: smallint; Handle: TSQLHandle; ErrorCode: smallint): EODBCError;
  end;

function GetODBCCli: TODBCCli;

function IsODBCError(ReturnCode: smallint): boolean;

function PtrToXString(P: IntPtr): _string; overload;

implementation

{$IFDEF CLR}
uses
{$IFNDEF UNIDACPRO}
  ODBCCallCLR;
{$ELSE}
  ODBCCallCLRUni;
{$ENDIF}
{$ENDIF}

var
  ODBCCli: TODBCCli;
  LockODBCCli: TCriticalSection;

function SQL_LEN_DATA_AT_EXEC(Length: integer): integer;
begin
  Result := -(Length) + SQL_LEN_DATA_AT_EXEC_OFFSET;
end;

function GetODBCCli: TODBCCli;
begin
  LockODBCCli.Enter;
  try
    if ODBCCli = nil then
      ODBCCli := TODBCCli.Create(LockODBCCli);
  finally
    LockODBCCli.Leave;
  end;

  Result := ODBCCli;
end;

constructor TODBCCli.Create(Lock: TCriticalSection);
begin
  inherited Create;

  FLock := Lock;
end;

destructor TODBCCli.Destroy;
begin
  FreeLib;

  inherited;
end;

procedure TODBCCli.LoadLib;
var
  LibName: string;
begin
  FLock.Enter;
  try
    if Integer(FLib) <> 0 then
      exit;

    LibName := GetLibName;

  {$IFDEF MSWINDOWS}
    FLib := LoadLibraryEx(PChar(LibName), 0, LOAD_WITH_ALTERED_SEARCH_PATH);
  {$ENDIF}
  {$IFDEF LINUX}
    FLib := dlopen(PChar(LibName), RTLD_LAZY);
  {$ENDIF}

    if Integer(FLib) = 0 then
      raise Exception.Create('Cannot load client library: ' + LibName);

    try
      InitFunctions;
    except
      FreeLib;
      raise;
    end;

  finally
    FLock.Leave;
  end;
end;

procedure TODBCCli.FreeLib;
begin
  FLock.Enter;
  try
    if Integer(FLib) = 0 then
      exit;

    FreeEnvironment;

  {$IFDEF MSWINDOWS}
    FreeLibrary(FLib);
    FLib := 0;
  {$ENDIF}
  {$IFDEF LINUX}
    FLib := nil;
  {$ENDIF}

  finally
    FLock.Leave;
  end;
end;

function TODBCCli.IsLoaded: boolean;
begin
  Result := Integer(FLib) <> 0;
end;

function TODBCCli.GetLibName: string;
begin
{$IFNDEF CLR}
  if ODBCDLL = '' then
    ODBCDLL := ODBCDLLName;
{$ENDIF}
  Result := ODBCDLL;
end;

function NotLink: integer;
begin
  raise Exception.Create('CLI function is not linked');
end;

{$IFNDEF CLR}
function TODBCCli.GetProc(const Name: string): IntPtr;
begin
{$IFDEF MSWINDOWS}
  Result := GetProcAddress(FLib, PChar(Name));
{$ENDIF}
{$IFDEF LINUX}
  Result := dlsym(FLib, PChar(Name));
{$ENDIF}
  if Result = nil then
    Result := @NotLink;
end;
{$ENDIF}

procedure TODBCCli.InitFunctions;
begin
{$IFDEF CLR}
  SQLAllocHandle := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLAllocHandle;
  SQLBindCol := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLBindCol;
  SQLBindParameter := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLBindParameter;
  SQLCloseCursor := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLCloseCursor;
  SQLColAttributeInt := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLColAttributeInt;
  SQLColAttributeStr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLColAttributeStr;
  SQLColumns := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLColumns;
  SQLConnect := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLConnect;
  SQLDataSources := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLDataSources;
  SQLDescribeCol := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLDescribeCol;
  SQLDisconnect := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLDisconnect;
  SQLDriverConnect := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLDriverConnect;
  SQLDrivers := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLDrivers;
  SQLEndTran := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLEndTran;
  SQLExecDirect := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLExecDirect;
  SQLExecute := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLExecute;
  SQLFetch := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLFetch;
  SQLFreeHandle := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLFreeHandle;
  SQLGetConnectAttrInt := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetConnectAttrInt;
  SQLGetConnectAttrStr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetConnectAttrStr;
  SQLGetData := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetData;
  SQLGetDescFieldInt := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetDescFieldInt;
  SQLGetDescFieldStr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetDescFieldStr;
  SQLGetDiagRec := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetDiagRec;
  SQLGetInfoInt := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetInfoInt;
  SQLGetInfoStr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetInfoStr;
  SQLGetStmtAttrPtr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLGetStmtAttrPtr;
  SQLMoreResults := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLMoreResults;
  SQLNumResultCols := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLNumResultCols;
  SQLParamData := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLParamData;
  SQLPrepare := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLPrepare;
  SQLProcedureColumns := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLProcedureColumns;
  SQLProcedures := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLProcedures;
  SQLPutData := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLPutData;
  SQLRowCount := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLRowCount;
  SQLSetConnectAttrInt := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSetConnectAttrInt;
  SQLSetConnectAttrStr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSetConnectAttrStr;
  SQLSetDescFieldPtr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSetDescFieldPtr;
  SQLSetDescFieldInt := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSetDescFieldInt;
  SQLSetEnvAttrInt := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSetEnvAttrInt;
  SQLSetStmtAttrPtr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSetStmtAttrPtr;
  SQLSetStmtAttrInt := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSetStmtAttrInt;
  SQLSetStmtAttrStr := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSetStmtAttrStr;
  SQLSpecialColumns := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLSpecialColumns;
  SQLStatistics := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLStatistics;
  SQLTables := {$IFNDEF UNIDACPRO}ODBCCallCLR{$ELSE}ODBCCallCLRUni{$ENDIF}.SQLTables;
{$ELSE}
{$IFDEF IS_UNICODE}
  SQLColAttributeInt := GetProc('SQLColAttributeW');
  SQLColumns := GetProc('SQLColumnsW');
  SQLConnect := GetProc('SQLConnectW');
  SQLDataSources := GetProc('SQLDataSourcesW');
  SQLDescribeCol := GetProc('SQLDescribeColW');
  SQLDriverConnect := GetProc('SQLDriverConnectW');
  SQLDrivers := GetProc('SQLDriversW');
  SQLExecDirect := GetProc('SQLExecDirectW');
  SQLGetConnectAttrInt := GetProc('SQLGetConnectAttrW');
  SQLGetDescFieldInt := GetProc('SQLGetDescFieldW');
  SQLGetDiagRec := GetProc('SQLGetDiagRecW');
  SQLGetInfoInt := GetProc('SQLGetInfoW');
  SQLGetStmtAttrPtr := GetProc('SQLGetStmtAttr');
  SQLPrepare := GetProc('SQLPrepareW');
  SQLProcedureColumns := GetProc('SQLProcedureColumnsW');
  SQLProcedures := GetProc('SQLProceduresW');
  SQLSetConnectAttrInt := GetProc('SQLSetConnectAttrW');
  SQLSetDescFieldPtr := GetProc('SQLSetDescFieldW');
  SQLSetStmtAttrPtr := GetProc('SQLSetStmtAttrW');
  SQLSpecialColumns := GetProc('SQLSpecialColumnsW');
  SQLStatistics := GetProc('SQLStatisticsW');
  SQLTables := GetProc('SQLTablesW');
{$ELSE}
  SQLColAttributeInt := GetProc('SQLColAttribute');
  SQLColumns := GetProc('SQLColumns');
  SQLConnect := GetProc('SQLConnect');
  SQLDataSources := GetProc('SQLDataSources');
  SQLDescribeCol := GetProc('SQLDescribeCol');
  SQLDriverConnect := GetProc('SQLDriverConnect');
  SQLDrivers := GetProc('SQLDrivers');
  SQLExecDirect := GetProc('SQLExecDirect');
  SQLGetConnectAttrInt := GetProc('SQLGetConnectAttr');
  SQLGetDescFieldInt := GetProc('SQLGetDescField');
  SQLGetDiagRec := GetProc('SQLGetDiagRec');
  SQLGetInfoInt := GetProc('SQLGetInfo');
  SQLGetStmtAttrPtr := GetProc('SQLGetStmtAttr');
  SQLPrepare := GetProc('SQLPrepare');
  SQLProcedureColumns := GetProc('SQLProcedureColumns');
  SQLProcedures := GetProc('SQLProcedures');
  SQLSetConnectAttrInt := GetProc('SQLSetConnectAttr');
  SQLSetDescFieldPtr := GetProc('SQLSetDescField');
  SQLSetStmtAttrPtr := GetProc('SQLSetStmtAttr');
  SQLSpecialColumns := GetProc('SQLSpecialColumns');
  SQLStatistics := GetProc('SQLStatistics');
  SQLTables := GetProc('SQLTables');
{$ENDIF}
  SQLAllocHandle := GetProc('SQLAllocHandle');
  SQLBindCol := GetProc('SQLBindCol');
  SQLBindParameter := GetProc('SQLBindParameter');
  SQLCloseCursor := GetProc('SQLCloseCursor');
  SQLColAttributeStr := @SQLColAttributeInt;
  SQLDisconnect := GetProc('SQLDisconnect');
  SQLEndTran := GetProc('SQLEndTran');
  SQLExecute := GetProc('SQLExecute');
  SQLFetch := GetProc('SQLFetch');
  SQLFreeHandle := GetProc('SQLFreeHandle');
  SQLGetConnectAttrStr := @SQLGetConnectAttrInt;
  SQLGetData := GetProc('SQLGetData');
  SQLGetDescFieldStr := @SQLGetDescFieldInt;
  SQLGetInfoStr := @SQLGetInfoInt;
  SQLMoreResults := GetProc('SQLMoreResults');
  SQLNumResultCols := GetProc('SQLNumResultCols');
  SQLParamData := GetProc('SQLParamData');
  SQLPutData := GetProc('SQLPutData');
  SQLRowCount := GetProc('SQLRowCount');
  SQLSetConnectAttrStr := @SQLSetConnectAttrInt;
  SQLSetDescFieldInt := @SQLSetDescFieldPtr;
  SQLSetEnvAttrInt := GetProc('SQLSetEnvAttr');
  SQLSetStmtAttrInt := @SQLSetStmtAttrPtr;
  SQLSetStmtAttrStr := @SQLSetStmtAttrPtr;
{$ENDIF}
end;

procedure TODBCCli.InitEnvironment;

  procedure Error(Res: smallint);
  begin
    raise Exception.CreateFmt('Can''t initialize SQL environment. Error %d', [Res]);
  end;

var
  Res: smallint;
begin
  FLock.Enter;
  try
    if SQLHEnv <> nil then
      exit;

    if Integer(FLib) = 0 then
      LoadLib;

    Res := SQLAllocHandle(SQL_HANDLE_ENV, nil, SQLHEnv);
    if IsODBCError(Res) then
      Error(Res);

    try
      Res := SQLSetEnvAttrInt(SQLHEnv, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, 0);
      if IsODBCError(Res) then
        Error(Res);
    except
      FreeEnvironment;
      raise;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TODBCCli.FreeEnvironment;
begin
  FLock.Enter;
  try
    if SQLHEnv = nil then
      exit;

    SQLFreeHandle(SQL_HANDLE_ENV, SQLHEnv);

    SQLHEnv := nil;
  finally
    FLock.Leave;
  end;
end;

function TODBCCli.IsInited: boolean;
begin
  Result := SQLHEnv <> nil;
end;

function IsODBCError(ReturnCode: smallint): boolean;
begin
  Result := (ReturnCode <> SQL_SUCCESS) and (ReturnCode <> SQL_SUCCESS_WITH_INFO);
end;

function TODBCCli.GetODBCError(HandleType: smallint; Handle: TSQLHandle; ErrorCode: smallint): EODBCError;
const
  BufSize = 8192;
var
  Buf: IntPtr;
  State: TSQLState;
  NativeError: integer;
  Res, Len: smallint;
  Msg: _string;
begin
  State[0] := #0;
  NativeError := 0;
  Buf := Marshal.AllocHGlobal(BufSize * SizeOf(_char));
  try
    Res := SQLGetDiagRec(HandleType, Handle, 1, State, NativeError, Buf, BufSize, Len);
    if not IsODBCError(Res) then
      Msg := PtrToXString(Buf)
    else begin
      Res := SQLGetDiagRec(SQL_HANDLE_ENV, SQLHEnv, 1, State, NativeError, Buf, BufSize, Len);
      if not IsODBCError(Res) then
        Msg := PtrToXString(Buf);
    end;
  finally
    Marshal.FreeHGlobal(Buf);
  end;
  Result := EODBCError.Create(ErrorCode, Msg, State, NativeError);
end;

function PtrToXString(P: IntPtr): _string;
begin
{$IFDEF IS_UNICODE}
  Result := Marshal.PtrToStringUni(P);
{$ELSE}
  Result := Marshal.PtrToStringAnsi(P);
{$ENDIF}
end;

initialization
  LockODBCCli := TCriticalSection.Create;
  ODBCCli := nil;

finalization
{$IFNDEF CLR}
  ODBCCli.Free;
{$ENDIF}
  LockODBCCli.Free;

end.

