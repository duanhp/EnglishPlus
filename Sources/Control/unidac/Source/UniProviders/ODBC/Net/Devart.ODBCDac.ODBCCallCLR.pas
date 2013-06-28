{$IFNDEF UNIDACPRO}
{$I ..\ODBCDac.inc}
unit Devart.ODBCDac.ODBCCallCLR;
{$ENDIF}

interface

uses
  Windows, System.Runtime.InteropServices, System.Text,
  MemUtils, {$IFNDEF UNIDACPRO}ODBCCall{$ELSE}ODBCCallUni{$ENDIF};

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLAllocHandle(
    HandleType:       smallint;
    InputHandle:      TSQLHandle;
    var OutputHandle: TSQLHandle
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLBindCol(
    StatementHandle: TSQLHStmt;
    ColumnNumber:    word;
    TargetType:      smallint;
    TargetValue:     IntPtr;
    BufferLength:    integer;
    StrLen_or_Ind:   IntPtr
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLBindParameter(
     StatementHandle: TSQLHStmt;
     ParameterNumber: word;
     InputOutputType: smallint;
     ValueType: smallint;
     ParameterType: smallint;
     ColumnSize: longword;
     DecimalDigits: smallint;
     ParameterValuePtr: IntPtr;
     BufferLength: integer;
     StrLen_or_IndPtr: IntPtr
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLCloseCursor(
    StatementHandle: TSQLHStmt
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLColAttributeW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLColAttributeInt(
    StatementHandle:      TSQLHStmt;
    ColumnNumber:         word;
    FieldIdentifier:      word;
    CharacterAttribute:   IntPtr;
    BufferLength:         smallint;
    var StringLength:     smallint;
    var NumericAttribute: integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLColAttributeW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLColAttributeStr(
    StatementHandle:    TSQLHStmt;
    ColumnNumber:       word;
    FieldIdentifier:    word;
    CharacterAttribute: IntPtr;
    BufferLength:       smallint;
    var StringLength:   smallint;
    NumericAttribute:   IntPtr
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLColumnsW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLColumns(
    StatementHandle: TSQLHStmt;
    CatalogName:     _PChar;
    NameLength1:     smallint;
    SchemaName:      _PChar;
    NameLength2:     smallint;
    TableName:       _PChar;
    NameLength3:     smallint;
    ColumnName:      _PChar;
    NameLength4:     smallint
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLConnectW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLConnect(
    ConnectionHandle: TSQLHDbc;
    ServerName:       _PChar;
    NameLength1:      smallint;
    UserName:         _PChar;
    NameLength2:      smallint;
    Authentication:   _PChar;
    NameLength3:      smallint
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLDataSourcesW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLDataSources(
    EnvironmentHandle: TSQLHEnv;
    Direction:         word;
    ServerName:        IntPtr;
    BufferLength1:     smallint;
    var NameLength1:   smallint;
    Description:       IntPtr;
    BufferLength2:     smallint;
    var NameLength2:   smallint
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLDescribeColW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLDescribeCol(
    StatementHandle:   TSQLHStmt;
    ColumnNumber:      word;
    ColumnName:        IntPtr;
    BufferLength:      smallint;
    var NameLength:    smallint;
    var DataType:      smallint;
    var ColumnSize:    longword;
    var DecimalDigits: smallint;
    var Nullable:      smallint
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLDisconnect(
    ConnectionHandle: TSQLHDbc
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLDriverConnectW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLDriverConnect(
    ConnectionHandle:     TSQLHDbc;
    WindowHandle:         HWND;
    InConnectionString:   _PChar;
    StringLength1:        smallint;
    OutConnectionString:  IntPtr;
    BufferLength:         smallint;
    var StringLength2Ptr: smallint;
    DriverCompletion:     word
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLDriversW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLDrivers(
    EnvironmentHandle:  TSQLHEnv;
    Direction:          word;
    DriverDescription:  IntPtr;
    BufferLength1:      smallint;
    var DescriptionLengthPtr: smallint;
    DriverAttributes:   IntPtr;
    BufferLength2:      smallint;
    var AttributesLengthPtr:  smallint
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLEndTran(
    HandleType:     smallint;
    Handle:         TSQLHandle;
    CompletionType: smallint
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLExecDirectW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLExecDirect(
    StatementHandle: TSQLHStmt;
    StatementText:   _PChar;
    TextLength:      integer
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLExecute(
    StatementHandle: TSQLHStmt
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLFetch(
    StatementHandle: TSQLHStmt
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLFreeHandle(
    HandleType: smallint;
    Handle:     TSQLHandle
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLGetConnectAttrW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetConnectAttrInt(
    ConnectionHandle: TSQLHDbc;
    Attribute:        integer;
    var ValuePtr:     integer;
    BufferLength:     integer;
    StringLength:     IntPtr
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLGetConnectAttrW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetConnectAttrStr(
    ConnectionHandle: TSQLHDbc;
    Attribute:        integer;
    ValuePtr:         _PChar;
    BufferLength:     integer;
    var StringLength: integer
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetData(
    StatementHandle: TSQLHStmt;
    ColumnNumber:    word;
    TargetType:      smallint;
    TargetValue:     IntPtr;
    BufferLength:    integer;
    var StrLen_or_Ind: integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLGetDescFieldW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetDescFieldInt(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    var Value:        integer;
    BufferLength:     integer;
    StringLength:     IntPtr
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLGetDescFieldW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetDescFieldStr(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    Value:            IntPtr;
    BufferLength:     integer;
    var StringLength: integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLGetDiagRecW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetDiagRec(
    HandleType:      smallint;
    Handle:          TSQLHandle;
    RecNumber:       smallint;
    SQLstate:        _PChar; // pointer to 5 character buffer
    var NativeError: integer;
    MessageText:     IntPtr;
    BufferLength:    smallint;
    var TextLength:  smallint
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLGetInfoW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetInfoInt(
    ConnectionHandle: TSQLHDbc;
    InfoType:         word;
    var InfoValue:    integer;
    BufferLength:     smallint;
    StringLengthPtr:  IntPtr
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLGetInfoW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetInfoStr(
    ConnectionHandle: TSQLHDbc;
    InfoType:         word;
    InfoValuePtr:     IntPtr;
    BufferLength:     smallint;
    var StringLength: smallint
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLGetStmtAttrW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLGetStmtAttrPtr(
    StatementHandle:  TSQLHStmt;
    Attribute:        integer;
    var Value:        IntPtr;
    BufferLength:     integer;
    StringLength:     IntPtr
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLMoreResults(
    HStmt: TSQLHStmt
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLNumResultCols(
    StatementHandle: TSQLHStmt;
    var ColumnCount: smallint
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLParamData(
    StatementHandle: TSQLHStmt;
    var Value:       IntPtr
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLPrepareW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLPrepare(
    StatementHandle: TSQLHStmt;
    StatementText:   _PChar;
    TextLength:      integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLProcedureColumnsW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLProcedureColumns(
    HStmt:         TSQLHStmt;
    szCatalogName: _PChar;
    cbCatalogName: smallint;
    szSchemaName:  _PChar;
    cbSchemaName:  smallint;
    szProcName:    _PChar;
    cbProcName:    smallint;
    szColumnName:  _PChar;
    cbColumnName:  smallint
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLProceduresW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLProcedures(
    HStmt:         TSQLHStmt;
    szCatalogName: _PChar;
    cbCatalogName: smallint;
    szSchemaName:  _PChar;
    cbSchemaName:  smallint;
    szProcName:    _PChar;
    cbProcName:    smallint
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLPutData(
    StatementHandle: TSQLHStmt;
    Data:            IntPtr;
    StrLen_or_Ind:   integer
  ): smallint; external;

  [DllImport(ODBCDLL, CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLRowCount(
    StatementHandle: TSQLHStmt;
    var RowCount:    integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSetConnectAttrW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSetConnectAttrInt(
    ConnectionHandle: TSQLHDbc;
    Attribute:        integer;
    ValuePtr:         integer;
    StringLength:     integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSetConnectAttrW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSetConnectAttrStr(
    ConnectionHandle: TSQLHDbc;
    Attribute:        integer;
    ValuePtr:         _PChar;
    StringLength:     integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSetDescFieldW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSetDescFieldPtr(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    Value:            IntPtr;
    BufferLength:     integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSetDescFieldW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSetDescFieldInt(
    DescriptorHandle: TSQLHDesc;
    RecNumber:        smallint;
    FieldIdentifier:  smallint;
    Value:            integer;
    BufferLength:     integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSetEnvAttr', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSetEnvAttrInt(
    EnvironmentHandle: TSQLHEnv;
    Attribute:         integer;
    ValuePtr:          integer;
    StringLength:      integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSetStmtAttrW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSetStmtAttrPtr(
    StatementHandle: TSQLHStmt;
    Attribute:       integer;
    ValuePtr:        IntPtr;
    StringLength:    integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSetStmtAttrW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSetStmtAttrInt(
    StatementHandle: TSQLHStmt;
    Attribute:       integer;
    ValuePtr:        integer;
    StringLength:    integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSetStmtAttrW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSetStmtAttrStr(
    StatementHandle: TSQLHStmt;
    Attribute:       integer;
    ValuePtr:        _PChar;
    StringLength:    integer
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLSpecialColumnsW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLSpecialColumns(
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
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLStatisticsW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLStatistics(
    StatementHandle: TSQLHStmt;
    CatalogName:     _PChar;
    NameLength1:     smallint;
    SchemaName:      _PChar;
    NameLength2:     smallint;
    TableName:       _PChar;
    NameLength3:     smallint;
    Unique:          word;
    Reserved:        word
  ): smallint; external;

  [DllImport(ODBCDLL, EntryPoint='SQLTablesW', CharSet = CharSet.Unicode, SetLastError = True, CallingConvention=CallingConvention.Stdcall)]
  function SQLTables(
    StatementHandle: TSQLHStmt;
    CatalogName:     _PChar;
    NameLength1:     smallint;
    SchemaName:      _PChar;
    NameLength2:     smallint;
    TableName:       _PChar;
    NameLength3:     smallint;
    TableType:       _PChar;
    NameLength4:     smallint
  ): smallint; external;

implementation

end.
