
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//  MyClasses
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MyClassesUni;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFNDEF LITE}
  DB, DBAccess,
{$ENDIF}
{$IFDEF CLR}
  System.Runtime.InteropServices,
  Borland.Vcl.TypInfo,
  System.Text, System.Globalization,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
  MemData, MemUtils, CRAccess, CRParser, CRVio,
{$IFNDEF UNIDACPRO}
  MyCall, MySqlApi;
{$ELSE}
  MyCallUni, MySqlApiUni;
{$ENDIF}

const
  // properties
  prMySQLBase       = 1000;

  // connection properties
  prConnectionTimeout = prMySQLBase + 1; // Integer
  prFieldsAsString  = prMySQLBase + 5; // boolean
  prNullForZeroDate = prMySQLBase + 6; // boolean
  prCompress        = prMySQLBase + 7; // boolean

  prCharset         = prMySQLBase + 9; // string

  //prMySQLAPI        = prMySQLBase + 10; // MySQLAPIClient, MySQLAPIEmbedded, MySQLAPIDirect
  prProtocol        = prMySQLBase + 11; // boolean

  prEnableBoolean   = prMySQLBase + 14; // boolean
  prCreateConnection = prMySQLBase + 15; // boolean
  prCommandTimeout  = prMySQLBase + 16; // integer
  prOpenNext        = prMySQLBase + 17; // boolean
  prUseUnicode      = prMySQLBase + 18; // boolean
  prEmbParams       = prMySQLBase + 19; // string
{$IFDEF HAVE_OPENSSL}
  prSSL_Chipher     = prMySQLBase + 20; // string
  prSSL_CA          = prMySQLBase + 21; // string
  prSSL_Key         = prMySQLBase + 22; // string
  prSSL_Cert        = prMySQLBase + 23; // string
{$ENDIF}
  //prCompressBlobMode = prMySQLBase + 24; // TCompressBlobMode
  prIsCanOpenNext   = prMySQLBase + 25; // boolean
  prParamPosition   = prMySQLBase + 27; // integer
  prFillParamPosition = prMySQLBase + 28; // boolean
  prUseHandler      = prMySQLBase + 29; // boolean
  prEmbedded        = prMySQLBase + 30; // boolean
  prDirect          = prMySQLBase + 31; // boolean
  prCheckPrecision  = prMySQLBase + 32; // boolean
  prOptimizedBigInt = prMySQLBase + 33; // boolean
  prBinaryAsString  = prMySQLBase + 34; // boolean
  prCheckBackslashes = prMySQLBase + 35; // boolean
  prNeedBackslashes = prMySQLBase + 36; // boolean
  prLock            = prMySQLBase + 37;
  prDelayed         = prMySQLBase + 38;
  prRowsPerQuery    = prMySQLBase + 39;
  prDuplicateKeys   = prMySQLBase + 40;
  prNullForZeroDelphiDate = prMySQLBase + 41; // boolean

  SResultParamName = 'Result';

type
  TReadParamsProc = procedure of object;

  TParamPositions = array of integer;

{ EMyError }
  EMyError = class({$IFDEF LITE}Exception{$ELSE}EDAError{$ENDIF})
  protected
  {$IFDEF LITE}
    FErrorCode: integer;
  {$ELSE}
    FLineNumber: integer;
  {$ENDIF}

  public
    constructor Create(ErrorCode: integer; Msg: _string);
  {$IFNDEF LITE}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}

  {$IFDEF LITE}
    property ErrorCode: integer read FErrorCode;
  {$ELSE}
    property LineNumber: integer read FLineNumber;
  {$ENDIF}
  end;

type
{ TMySQLConnection }
//  TMyErrorProc = procedure (E: {$IFDEF LITE}Exception{$ELSE}EDAError{$ENDIF}; var Fail: boolean) of object;

  TMyProtocol = (mpDefault, mpTCP, mpSocket, mpPipe, mpMemory{$IFDEF HAVE_OPENSSL}, mpSSL{$ENDIF}, mpHttp);

  TMyIOHandler = TCRIOHandler;
  TMySQLConnection = class;
  TGetMySQLConnection = function: TMySQLConnection of object;
  TReturnMySQLConnection = procedure(CRConnection: TCRConnection) of object;
  TExecuteProc = procedure(const NativeSQL: _string) of object;

  TParamInfo = record
    Name: _string;
    Direction: TParamDirection;
    ParamType: string;
    Unsigned: boolean;
    Size: integer;
  end;
  TParamInfos = array of TParamInfo;

  TMySQLConnection = class (TCRConnection)
  protected
    FMySQLAPI: TMySQLAPI;

    FMySQL: PMYSQL_CON;

  { Session }
    FDatabase: string;
    FPort: integer;
    FCompress: boolean;
    FUseUnicode: boolean;
    FCharset: string;
    FConnectionTimeout: integer;
    //FDoError: TMyErrorProc;
    FProtocol: TMyProtocol;
    FOptimizedBigInt: boolean;
    FNullForZeroDelphiDate: boolean;
    FEmbedded: boolean;
    FDirect: boolean;
    FCheckPrecision: boolean;
    FCheckBackslashes: boolean;
    FNeedBackslashes: boolean;

    FAdditional: boolean; // This is additional connection for FetchAl = False mode

    FEmbParams: TStringList;
  {$IFDEF HAVE_OPENSSL}
    FSSL_Chipher: string;
    FSSL_CA: string;
    FSSL_Key: string;
    FSSL_Cert: string;
  {$ENDIF}

  { Server info }
    FServerVer: string;
    FServerPrimaryVer, FServerMinorVer, FServerReleaseVer: integer;

  { Pooling for FetchAll = False }
    FGetMySQLConnection: TGetMySQLConnection;
    FReturnMySQLConnection: TReturnMySQLConnection;

    function GetIsClient41: boolean;
    function GetIsServer41: boolean;

    procedure SetDatabase(const Value: string);
    procedure ExecStatement(const SQL: _string);
    procedure TryToApplyUseUnicode;
    procedure CheckNoBackslashEscapesMode;

    procedure DoError(E: Exception; var Fail: boolean); override;
    procedure SetMySQLAPI;
    procedure SetEmbParamsStr(const EmbParams: string);

    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetTransactionClass: TCRTransactionClass; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    procedure Check(const Status: TIntPtrObj; Component: TObject); overload; virtual;
    procedure Check(const Status: integer; Component: TObject); overload; virtual;
    procedure Check(Component: TObject); overload; virtual;
    procedure MySQLError(Component: TObject);

    procedure CheckStmt(stmt: PMYSQL_STMT; const Status: integer; Component: TObject); virtual;
    procedure MySQLStmtError(stmt: PMYSQL_STMT; Component: TObject);

    procedure Connect(const ConnectString: _string); override;
    procedure Disconnect; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    function CheckIsValid: boolean; override;
//    function SetProc(Prop: integer; const Value: TMethod): boolean; override;

    function GetServerVersion: _string; override;
    function GetServerVersionFull: _string; override;
    function GetClientVersion: _string; override;

    function GetThreadId: longword;

    property IsClient41: boolean read GetIsClient41;
    property IsServer41: boolean read GetIsServer41;
    property ServerPrimaryVer: integer read FServerPrimaryVer;
    property ServerMinorVer: integer read FServerMinorVer;
    property ServerReleaseVer: integer read FServerReleaseVer;

    property MySQLAPI: TMySQLAPI read FMySQLAPI;
    property MySQL: PMYSQL_CON read FMySQL;

    property GetMySQLConnection: TGetMySQLConnection read FGetMySQLConnection write FGetMySQLConnection;
    property ReturnMySQLConnection: TReturnMySQLConnection read FReturnMySQLConnection write FReturnMySQLConnection;

    property Additional: boolean read FAdditional write FAdditional; // This is additional connection for FetchAl = False mode
    property Reconnected: boolean read FReconnected write FReconnected;
  end;

type
{ TMySQLCommand }

  TMySQLCommand = class (TCRCommand)
  protected
    FConnection: TMySQLConnection;
    FCommandTimeout: integer;

    FCursorState: TCursorState;

    FRowsAffected: integer;
    FRowsAffectedPrev: integer; // For SP with out param

    FStoredProc: boolean;
    FCanReadParams: boolean;
    FIsSelectParams: boolean;
    FRequestResultset: boolean; // Indicate current command owner - MyCommand(False) or MyDataSet(True)

    // Params processing (see PlaceParamValues)
    FCachedSQL: _TStringList; // SQL without comments. One line for each parameter
    FParamPositions: TParamPositions;
    FFillParamPosition: boolean;

    FPrepared: boolean;
    Fstmt: PMYSQL_STMT;

    FOpenNext: boolean;
    FOnExecute: TExecuteProc;
    FSQLResult: pMYSQL_RES;

    FBreakExecCS: TCriticalSection;
    FWaitForBreak: boolean;

    procedure Check(const Status: TIntPtrObj); overload; virtual;
    procedure Check(const Status: integer); overload; virtual;
    procedure Check; overload; virtual;
    procedure CheckStmt(const Status: integer); virtual;

    function CalcRowsAffected: integer;

    procedure ParseSQLParam(ParsedSQL: _StringBuilder; Parser: TSQLParser; Params: TParamDescs; LeftQuote, RightQuote: _char; const RenamePrefix: _string); override;

    procedure GetSPParams(const Name: _string; out IsFunc: boolean; out ParamList: _string; out ReturnParam: _string);
    procedure DescribeParams(const ParamList: _string; const IsResult: boolean; var ParamsInfo: TParamInfos; ParamDescs: TParamDescs);

    property Params;
    property Executing;
    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;

    function ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string; override;

    function CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string; override;
    procedure Execute(Iters: integer = 1); override;

    procedure SetConnection(Value: TCRConnection); override;
    procedure SetSQL(const Value: _string); override;

    class function GetTableInfoClass: TTableInfoClass; override;
    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;

    function GetProp(Prop: integer; var Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    procedure BreakExec; override;

    property OnExecute: TExecuteProc read FOnExecute write FOnExecute;
  end;

{ TMySQLRecordset }

  TMySQLFieldDesc = class (TCRFieldDesc)
  private
    FMySQLType: TMySqlFieldType; // Native MySQL datatype
    FIsUnsigned: boolean; // "Sign" flag for MySQL datatype
    FIsPrimaryKey: boolean;

    FIsBinary: boolean; // for string and blob fields
    FIsCurrentTimestamp: boolean;

  public
    property MySQLType: TMySqlFieldType read FMySQLType;
    property IsUnsigned: boolean read FIsUnsigned;
    property IsPrimaryKey: boolean read FIsPrimaryKey;

    property IsBinary: boolean read FIsBinary;
    property IsCurrentTimestamp: boolean read FIsCurrentTimestamp;
  end;

  TMyTableInfo = class(TCRTableInfo)
  protected
    FMaxTimestamp: TDateTime;
    FTableNameFull: _string;
    FTimestampField: TMySQLFieldDesc;
    function GetTableNameFull: _string; override;
    procedure SetTableNameFull(const Value: _string); override;

  public
    property MaxTimestamp: TDateTime read FMaxTimestamp write FMaxTimestamp;
    property TimestampField: TMySQLFieldDesc read FTimestampField;
  end;

  TMySQLInfo = class(TSQLInfo)
  public
    function LeftQuote: _char; override;
    function RightQuote: _char; override;
    function IsQuoted(const Value: _string): boolean; override;
    function Quote(const Value: _string; const LeftQ: _char; const RightQ: _char): _string; override;
    function UnQuote(const Value: _string): _string; override;

    procedure SplitObjectName(const Name: _string; out DataBase: _string; out ObjName: _string); reintroduce;
  end;

  TMyOpenNextState = (
    osNotChecked,
    osMoreResults,     // mysql_next_result = 0   Successful and there are more results
    osNoMoreResults,   // mysql_next_result = -1  Successful and there are no more results
    osError            // mysql_next_result > 0   An error occurred
  );
  TMySQLFetchState = (fsBOF, fsMiddle, fsEof);

  TMySQLRecordset = class (TCRRecordSet)
  private
    FIsClient41: boolean;
    FUseUnicode: boolean;
    FOptimizedBigInt: boolean;
    FNullForZeroDelphiDate: boolean;
    FServerPrimaryVer: integer;
    FServerMinorVer: integer;
    function GetIsClient41: boolean;
    function GetUseUnicode: boolean;
    function GetOptimizedBigInt: boolean;
    function GetNullForZeroDelphiDate: boolean;
    function GetServerPrimaryVer: integer;
    function GetServerMinorVer: integer;
  protected
    FCommand: TMySQLCommand;

    FConnectionSwap: TMySQLConnection; // Used to store main connection on FetchAll = False

    FNullForZeroDate: boolean;
    FFieldsAsString: boolean;
    FEnableBoolean: boolean;
    FBinaryAsString: boolean;

    FFetchState: TMySQLFetchState;
    FFetchBnd: TMYSQL_BINDS; // PreparedClient, PreparedDirect

    FFetchBlock: IntPtr; // PreparedClient
    FFetchBlockSize: integer;
  {$IFDEF CLR}
    FFetchBlockArr: TBytes; // PreparedClient
    FFetchBlockGC: GCHandle; // PreparedClient

    FFetchBuf: TBytes; // Unprepared, PreparedDirect
  {$ENDIF}

    FNulls: TBytes; // for fetch performance optimization
    FValueBuffLen: TLenArr;

    FCreateConnection: boolean;

    // OpenNext
    FOpenNextState: TMyOpenNextState;
    FSQLResultNext: pMYSQL_RES;
    FRowsAffectedNext: integer;
    FIsCanFastClose: boolean;

    FUseHandler: boolean;
    
    procedure Check(const Status: IntPtr); overload; virtual;
    procedure Check(const Status: integer); overload; virtual;
    procedure Check; overload; virtual;

    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;

  { Open / Close }
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

    procedure QuerySwapConnection;
    procedure ReleaseSwapConnection(ForbidQueries: boolean = False);
    procedure FreeResult(const TryGetNextResult, TryToFinishFetch: boolean);
    procedure DrainResults;

  { Data }
    //procedure FreeData; override;

  { Fields }
    procedure InternalInitFields; override;
    function GetIndicatorSize: word; override;

  { Fetch }
    function GetCorrectedDataType(const FieldDesc: TFieldDesc): word;
    procedure AllocFetchBlock; // Bind fields
    function Fetch(FetchBack: boolean = False): boolean; override;
    procedure FreeFetchBlock; // Free Bind struct

    //PreCached FConection properties
    property IsClient41: boolean read GetIsClient41;
    property UseUnicode: boolean read GetUseUnicode;
    property OptimizedBigInt: boolean read GetOptimizedBigInt;
    property NullForZeroDelphiDate: boolean read GetNullForZeroDelphiDate;
    property ServerPrimaryVer: integer read GetServerPrimaryVer;
    property ServerMinorVer: integer read GetServerMinorVer;
  public
    constructor Create; override;

  { Fields }
    function GetFieldDescType: TFieldDescClass; override;
    procedure ExplicitInitFields; override;

  { Open / Close }
    procedure ExecCommand; override;
    procedure Disconnect; override;

  { Fields }
    function GetNull(FieldNo: word; RecBuf: IntPtr): boolean; override;
    procedure SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean); override;
    procedure SetNulls(RecBuf: IntPtr);
    procedure GetFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Dest: IntPtr); override;
    procedure PutFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Source: IntPtr; IsDatabaseValue: boolean = False); override;

    procedure CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;

  { Navigation }
    procedure SetToEnd; override;
    procedure BreakFetch; override;

    function CanDisconnect: boolean; override;

    function RowsReturn: boolean; override;

    function GetSQLObjectIndex(const ObjName: _string; out IsAlias: boolean): integer; overload; // MySQL specific - Search by name and by alias!!!
    function GetSQLObjectIndex(const ObjName: _string): integer; overload; // MySQL specific - Search by name and by alias!!!
    function AddSQLObjectIfNeed(const SQLObjName: _string; const IsView: boolean): integer; // Add new SQLObject, if need. If SQLObject already present then return its index
    function GetProp(Prop: integer; var Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    function IsCaseSensitive: boolean; override;
  end;

  TMySQLTransaction = class (TCRTransaction)
  protected
    function GetConnection: TMySQLConnection;

  public
    constructor Create; override;
    destructor Destroy; override;

  { Transaction control }
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure Savepoint(const Name: _string); override;
    procedure ReleaseSavepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;
  end;

{$IFNDEF LITE}
  TMySQLMetaData = class (TCRMetaData)
  protected
    FRecordSet: TMySQLRecordSet;

    function CreateRecordSet: TCRRecordSet; override;
    function GetTypesForSQL(const ObjectTypes: _string): _string;

    function GetTables(Restrictions: _TStrings): TData; override;
    procedure CopyTablesData(Restrictions: _TStrings); virtual;

    function GetColumns(Restrictions: _TStrings): TData; override;
    procedure CopyColumnsData(Restrictions: _TStrings); virtual;

    function GetProcedures(Restrictions: _TStrings): TData; override;
    function GetProcedureParameters(Restrictions: _TStrings): TData; override;

    function GetIndex(Restrictions: _TStrings; Columns: boolean): TData;
    function GetIndexes(Restrictions: _TStrings): TData; override;
    procedure CopyIndexesData(Restrictions: _TStrings); virtual;

    function GetIndexColumns(Restrictions: _TStrings): TData; override;
    procedure CopyIndexColumnsData(Restrictions: _TStrings); virtual;

    function GetConstraints(Restrictions: _TStrings): TData; override;

    function GetDatabases(Restrictions: _TStrings): TData; override;
    procedure CopyDatabasesData(Restrictions: _TStrings);
  end;
{$ENDIF}

{ TMySQLLoader }

  _TMyDuplicateKeys = (_dkNone, _dkIgnore, _dkReplace);

  TMySQLLoader = class(TCRLoader)
  protected
    FConnection: TMySQLConnection;
    FInsHeader: _string;
    FBuffer: AnsiStringBuilder;
    FRowBuffer: AnsiStringBuilder; // CurrentRow
    FValues: array of Variant;

    FLock: boolean;
    FDelayed: boolean;
    FRowsPerQuery: integer;
    FDebug: boolean;
    //FCommandTimeout: integer;
    FDuplicateKeys: _TMyDuplicateKeys;

    procedure Clear;
    procedure Flush;
    procedure ExecSQL(const SQL: AnsiString);

    procedure CalculateRowBuffer;
    procedure AppendRowBufferToBuffer;

    class function GetRecordSetClass: TCRRecordSetClass; override;
    procedure SetConnection(Value: TCRConnection); override;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Prepare; override;
    procedure Reset; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;
  end;

  function IsLargeDataTypeUsed(const FieldDesc: TFieldDesc): boolean; overload;
  function IsLargeDataTypeUsed(const ParamDesc: TParamDesc): boolean; overload;

  function GenerateTableName(const CatalogName: _string;
    const TableName: _string;
    const DefaultCatalogName: _string): _string; overload;

  function ConvertMySQLTypeToInternalFormat(const MySQLType: TMySqlFieldType;
    const FieldLengthInBytes, Decimals: integer;
    const IsBinary, IsUnsigned, EnableBoolean, OptimizedBigInt, BinaryAsString, Unicode,
    EnableBCD, EnableFMTBCD, CheckPrecision: boolean;
    const CharsetNr: integer;
    var InternalType: word; var Fixed: boolean): boolean;

  function ChangeDecimalSeparator(const Value: Variant): string;

  procedure AppendValueToSQL(
    SQL: AnsiStringBuilder;
    const DataType: word;
    const Value: variant;
    const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, NeedBackslashes: boolean;
    const ServerPrimaryVer: integer;
    const Charset: string); overload;

{$IFNDEF CLR}
  procedure AppendValueToSQL(
    SQL: WideStringBuilder;
    const DataType: word;
    const Value: variant;
    const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, NeedBackslashes: boolean;
    const ServerPrimaryVer: integer;
    const Charset: string); overload;
{$ENDIF}

  procedure AppendValueToSQL(
    SQLA: AnsiStringBuilder;
    SQLW: WideStringBuilder;
    const DataType: word;
    const Value: variant;
    const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, NeedBackslashes: boolean;
    const ServerPrimaryVer: integer;
    const Charset: string); overload;

  function ValueToSQL(
    const DataType: word;
    const Value: variant;
    const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}: boolean;
    const ServerPrimaryVer: integer): AnsiString;

{$IFDEF CLR}
  function EscapeAndQuoteStr(const p: string; Len: integer; const NeedBackslashes: boolean): string;
{$ELSE}
  function EscapeAndQuoteStr(const p: AnsiString; Len: integer): AnsiString; overload;
  function EscapeAndQuoteStr(const p: AnsiString; Len: integer; const NeedBackslashes: boolean; const Charset: string): AnsiString; overload;
  function EscapeAndQuoteWideStr(const p: WideString; Len: integer; const NeedBackslashes: boolean): WideString; overload;
{$ENDIF}


{$IFDEF AUTOTEST}
var
  __ServerExecuteCount: integer;
  __ServerPrepareCount: integer;
  __SwapConnectionCount: integer;
{$ENDIF}

var
  ZeroDate: TDateTime;
  __Strings65535ToMemo: boolean = True;  // remove at 15.08.07
  __UseNewBooleanParamType: boolean = False;
  MySQLInfo: TMySQLInfo;

var
  CurrentProjectOutputDir: string = '';

implementation

uses
{$IFDEF PERF_COUNTER}
  Debug,
{$ENDIF}
{$IFDEF VER6P}
  FMTBcd, DateUtils,
{$ENDIF}
{$IFDEF HAVE_DIRECT}
  {$IFNDEF UNIDACPRO}
    MySqlApiDirect, MySqlSession,
  {$ELSE}
    MySqlApiDirectUni, MySqlSessionUni,
  {$ENDIF}
{$ENDIF}
  SysConst, DAConsts, Math,
{$IFNDEF UNIDACPRO}
  MyParser, MyConsts, MySqlNet;
{$ELSE}
  MyParserUni, MyConstsUni, MySqlNetUni;
{$ENDIF}

{$IFDEF LINUX}
const
  MaxWord = $FFFF;
{$ENDIF}

{$IFDEF CLR}
function EscapeAndQuoteStr(const p: string; Len: integer; const NeedBackslashes: boolean): string;
var
  St: StringBuilder;
  i: integer;
begin
  St := StringBuilder.Create(Len * 2 + 2);
  try
    St.Append('''');
    for i := 1 to Len do
      if not NeedBackslashes then
        case p[i] of
          '''':
            St.Append('''''');
        else
          St.Append(p[i]);
        end
      else
        case p[i] of
          #0:
            St.Append('\0');
          #$A:
            St.Append('\n');
          #$D:
            St.Append('\r');
          '\':
            St.Append('\\');
          '''':
            St.Append('\''');
          '"':
            St.Append('\"');
          #$1A:
            St.Append('\Z');
        else
          St.Append(p[i]);
        end;

    St.Append('''');
    Result := St.ToString;
  finally
    St.Free;
  end;
end;

{$ELSE}
function EscapeAndQuoteStr(const p: PAnsiChar; Len: integer; const NeedBackslashes: boolean; const Charset: string): AnsiString; overload;
  function mysql_real_escape_string(_to, from: PAnsiChar; length: longword): longword;
  var
    to_start, _end: PAnsiChar;
    IsGbkOrEuckr, IsEuckr: boolean;
  begin
    IsEuckr := LowerCase(Charset) = 'euckr';
    IsGbkOrEuckr := (LowerCase(Charset) = 'gbk') or IsEuckr;

    to_start := _to;
    _end := from + length;
    while from <> _end do begin
      if not NeedBackslashes then
        case from^ of
          '''': begin
            _to^ := '''';
            Inc(_to);
            _to^ := '''';
            Inc(_to);
          end;
        else
          begin
            _to^ := from^;
            Inc(_to);
          end;
        end
      else begin
        if SysLocale.FarEast and IsGbkOrEuckr and (from^ in LeadBytes) then begin
          _to^ := '\';
          Inc(_to);
          _to^ := from^;
          Inc(_to);

          if IsEuckr and (from <> _end) then begin
            Inc(from);
            _to^ := from^;
            Inc(_to);
          end;
        end
        else
          case from^ of
            #0: begin
              _to^ := '\';
              Inc(_to);
              _to^ := '0';
              Inc(_to);
            end;
            #$A: begin
              _to^ := '\';
              Inc(_to);
              _to^ := 'n';
              Inc(_to);
            end;
            #$D: begin
              _to^ := '\';
              Inc(_to);
              _to^ := 'r';
              Inc(_to);
            end;
            '\': begin
              _to^ := '\';
              Inc(_to);
              _to^ := '\';
              Inc(_to);
            end;
            '''': begin
              _to^ := '\';
              Inc(_to);
              _to^ := '''';
              Inc(_to);
            end;
            '"': begin
              _to^ := '\';
              Inc(_to);
              _to^ := '"';
              Inc(_to);
            end;
            #$1A: begin
              _to^ := '\';
              Inc(_to);
              _to^ := 'Z';
              Inc(_to);
            end;
          else
            begin
              _to^ := from^;
              Inc(_to);
            end;
          end;
      end;

      Inc(from);
    end;

    _to^ := #0;
    Result := _to - to_start;
  end;
begin
  SetLength(Result, cardinal(Len * 2 + 2));
  Result[1] := '''';
  SetLength(Result,
    mysql_real_escape_string(PAnsiChar(Result) + 1, p, Len) + 2);
  Result[Length(Result)] := '''';
end;

function EscapeAndQuoteStr(const p: AnsiString; Len: integer): AnsiString; overload;
begin
  Result := EscapeAndQuoteStr(p, Len, True, '');
end;

function EscapeAndQuoteStr(const p: AnsiString; Len: integer; const NeedBackslashes: boolean; const Charset: string): AnsiString; overload;
begin
  Result := EscapeAndQuoteStr(PAnsiChar(p), Len, NeedBackslashes, Charset);
end;

function EscapeAndQuoteWideStr(const p: PWideChar; Len: integer{in characters}; const NeedBackslashes: boolean): WideString; overload;
  function mysql_real_escape_string(_to: PWideChar; from: PWideChar; length: longword): longword;
  var
    to_start, _end: PWideChar;
  begin
    to_start := _to;

    _end := from + length;
    while from <> _end do begin
      if not NeedBackslashes then
        case from^ of
          '''': begin
            _to^ := '''';
            Inc(_to);
            _to^ := '''';
            Inc(_to);
          end;
        else
          begin
            _to^ := from^;
            Inc(_to);
          end;
        end
      else
        case from^ of
          #0: begin
            _to^ := '\';
            Inc(_to);
            _to^ := '0';
            Inc(_to);
          end;
          #$A: begin
            _to^ := '\';
            Inc(_to);
            _to^ := 'n';
            Inc(_to);
          end;
          #$D: begin
            _to^ := '\';
            Inc(_to);
            _to^ := 'r';
            Inc(_to);
          end;
          '\': begin
            _to^ := '\';
            Inc(_to);
            _to^ := '\';
            Inc(_to);
          end;
          '''': begin
            _to^ := '\';
            Inc(_to);
            _to^ := '''';
            Inc(_to);
          end;
          '"': begin
            _to^ := '\';
            Inc(_to);
            _to^ := '"';
            Inc(_to);
          end;
          #$1A: begin
            _to^ := '\';
            Inc(_to);
            _to^ := 'Z';
            Inc(_to);
          end;
        else
          begin
            _to^ := from^;
            Inc(_to);
          end;
        end;

      Inc(from);
    end;

    _to^ := #0;
    Result := _to - to_start;
  end;
begin
  SetLength(Result, cardinal(Len * 2 + 2));
  Result[1] := '''';
  SetLength(Result,
    mysql_real_escape_string(PWideChar(Result) + 1, p, Len) + 2);
  Result[Length(Result)] := '''';
end;

function EscapeAndQuoteWideStr(const p: WideString; Len: integer; const NeedBackslashes: boolean): WideString; overload;
begin
  Result := EscapeAndQuoteWideStr(PWideChar(p), Len, NeedBackslashes);
end;
{$ENDIF}

{$IFDEF CLR}
function EscapeAndQuoteVar(const Data: variant; const UseUnicode: boolean; const NeedBackslashes: boolean; const Charset: string): string;
var
  Blob: TBlob;
  s: string;
  b: TBytes;
begin
  if VarType(Data) = varObject then begin
    Blob := TBlob(Data);
    s := Blob.AsString;
    if UseUnicode then
      Result := UTF8Encode(EscapeAndQuoteStr(s, Length(s), NeedBackslashes))
    else
      Result := EscapeAndQuoteStr(s, Length(s), NeedBackslashes);
  end
  else
  if VarType(Data) = varArray + varByte then begin
    b := data;
    s := Encoding.Default.GetString(b);
    Result := EscapeAndQuoteStr(s, Length(s), NeedBackslashes);
    if UseUnicode then
      Result := UTF8Encode(Result)
  end
  else
  begin
    s := Data;
    Result := EscapeAndQuoteStr(s, Length(s), NeedBackslashes);
    if UseUnicode then
      Result := UTF8Encode(Result);
  end;
end;
{$ELSE}
function EscapeAndQuoteVar(const Data: variant; const UseUnicode: boolean{$IFDEF HAVE_COMPRESS}; const SendBlobCompressed: boolean{$ENDIF}; const NeedBackslashes: boolean; const Charset: string): AnsiString;
  procedure SendBlobAsIs(Blob: TBlob);
  begin
    if Blob.Size = 0 then begin
      Result := '''''';
      Exit;
    end;

    Blob.Defrag;
    if not UseUnicode then
      Result := EscapeAndQuoteStr(PAnsiChar(Blob.FirstPiece) + sizeof(TPieceHeader), Blob.FirstPiece.Used, NeedBackslashes, Charset)
    else
      if Blob.IsUnicode then
        Result := UTF8Encode(EscapeAndQuoteWideStr(PWideChar(PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader))), Blob.FirstPiece.Used div 2, NeedBackslashes))
      else
        Result := AnsiToUtf8(string(EscapeAndQuoteStr(PAnsiChar(Blob.FirstPiece) + sizeof(TPieceHeader), Blob.FirstPiece.Used, NeedBackslashes, Charset)));
  end;

var
  pData: PVarData;
  Blob: TBlob;
  s: AnsiString;
  ws: WideString;
{$IFDEF HAVE_COMPRESS}
  SwapBlob: TBlob;
{$ENDIF}
begin
  pData := @Data;
  if pData.VType = varByRef{$IFDEF FPC} or varVariant{$ENDIF} then begin
    Assert(pData.VPointer <> nil);
    // Assert(TObject(pData.VPointer) is TBlob); /// incompatible with trial exe/bpl
    Blob := pData.VPointer;
    Blob.Defrag;

  {$IFDEF HAVE_COMPRESS}
    if SendBlobCompressed then begin
      // (compr.a) Blob is not TCompressedBlob
      //   (compr.a.1) can't compress (Size <= MIN_COMPRESS_LENGTH) -> send "as is"
      //   (compr.a.2) can compress (Size > MIN_COMPRESS_LENGTH)    -> convert to TCompressedBlob(Compressed = False), then (compr.b.2)
      // (compr.b) Blob is TCompressedBlob, but not compressed
      //   (compr.b.1) can't compress (Size <= MIN_COMPRESS_LENGTH) -> send "as is"
      //   (compr.b.2) can compress (Size > MIN_COMPRESS_LENGTH)    -> compress, then send "as is"
      // (compr.c) Blob is TCompressedBlob and compressed           -> send "as is"
      SwapBlob := nil;
      try
        if not (Blob is TCompressedBlob) {compr.a} then
          if Blob.Size <= MIN_COMPRESS_LENGTH {compr.a.1} then begin
            SendBlobAsIs(Blob);
            Exit;
          end
          else {compr.a.2}
          begin
            SwapBlob := Blob;
            Blob := TCompressedBlob.Create(Blob.IsUnicode);
            Blob.Assign(SwapBlob);
          end;
        // {compr.b} or {compr.c}
        TCompressedBlob(Blob).Compressed := True; // Try to compress {compr.b.2 only}
        SendBlobAsIs(Blob);
      finally
        if SwapBlob <> nil then
          Blob.Free; // Free created object
      end;
    end
    else
    begin
      // (uncompr.a) Blob is not TCompressedBlob                    -> send "as is"
      // (uncompr.b) Blob is TCompressedBlob, but not compressed    -> send "as is"
      // (uncompr.c) Blob is TCompressedBlob and compressed         -> uncompress then send "as is"
      if not (Blob is TCompressedBlob) {uncompr.a} or
         not TCompressedBlob(Blob).Compressed {uncompr.b}
      then
        SendBlobAsIs(Blob)
      else
      begin {uncompr.c} // Does not use SendBlobAsIs for performance reason
        if not UseUnicode then
          Result := EscapeAndQuoteStr(Blob.AsAnsiString, Blob.Size, NeedBackslashes, Charset)
        else
          if Blob.IsUnicode then
            Result := UTF8Encode(EscapeAndQuoteWideStr(Blob.AsWideString, Blob.Size div 2, NeedBackslashes))
          else
            Result := AnsiToUTF8(string(EscapeAndQuoteStr(Blob.AsAnsiString, Blob.Size, NeedBackslashes, Charset)));
      end;
    end;
  {$ELSE}
    SendBlobAsIs(Blob);
  {$ENDIF}
  end
  else
  if pData.VType = varArray + varByte then
    Result := EscapeAndQuoteStr(pData.VArray.Data, pData.VArray.Bounds[0].ElementCount, NeedBackslashes, Charset)
  else
  if UseUnicode then begin
    if TVarData(Data).VPointer = nil then
      Result := ''''''
    else
    begin
      if (pData.VType = varOleStr) {$IFDEF VER12P}or (pData.VType = varUString){$ENDIF} then begin
        ws := EscapeAndQuoteWideStr(TVarData(Data).VPointer, StrLenW(TVarData(Data).VPointer), NeedBackslashes);
        Result := UTF8Encode(ws);
      end
      else
      begin
        s := EscapeAndQuoteStr(TVarData(Data).VPointer, StrLen(PAChar(TVarData(Data).VPointer)), NeedBackslashes, Charset);
        Result := AnsiToUTF8(string(s));
      end;
    end;
  end
  else
  begin
    s := AnsiString(Data);
    Result := EscapeAndQuoteStr(s, Length(s), NeedBackslashes, Charset);
  end;
end;
{$ENDIF}

function IsLargeDataTypeUsed(const FieldDesc: TFieldDesc): boolean;
begin
  Result := TMySQLFieldDesc(FieldDesc).FMySQLType in [
    FIELD_TYPE_GEOMETRY,
    FIELD_TYPE_TINY_BLOB, // TINYBLOB, TINYTEXT
    FIELD_TYPE_MEDIUM_BLOB, // MEDIUMBLOB, MEDIUMTEXT
    FIELD_TYPE_LONG_BLOB, // LONGBLOB, LONGTEXT
    FIELD_TYPE_BLOB // BLOB, TEXT
  ];
end;

function IsLargeDataTypeUsed(const ParamDesc: TParamDesc): boolean; 
begin
  case ParamDesc.GetDataType of
    dtBlob, dtMemo, dtWideMemo:
      Result := True;
    else
      Result := False;
  end;
end;

// FetchBlock support
function IsNeedFetchBlock(const DataType: word): boolean; // Return True if field need to fetch into separate buffer
begin
  case DataType of
    dtExtString, dtExtWideString, dtWideString, dtBlob, dtMemo, dtWideMemo,
    dtVarBytes, dtExtVarBytes,
    dtBCD, {$IFDEF VER6P}{$IFNDEF FPC}dtFMTBCD,{$ENDIF}{$ENDIF}
    dtDate, dtTime, dtDateTime:
      Result := True;
    else
      Result := False;
  end;
end;

procedure IncFetchBlockOffset(var FetchBlockOffset: integer; FieldDesc: TFieldDesc);
{$IFOPT C+}
var
  OldFetchBlockOffset: integer;
{$ENDIF}
begin
{$IFOPT C+}
  OldFetchBlockOffset := FetchBlockOffset;
{$ENDIF}
  case FieldDesc.DataType of
    dtVarBytes, dtExtVarBytes, dtExtString:
      Inc(FetchBlockOffset, sizeof(UInt) + FieldDesc.Length + 2);
    dtWideString, dtExtWideString:
      Inc(FetchBlockOffset, sizeof(UInt) + FieldDesc.Length * MaxUTF8CharLen + 3);
    dtMemo, dtWideMemo, dtBlob:
      case TMySQLFieldDesc(FieldDesc).MySQLType of
        FIELD_TYPE_TINY_BLOB: // TINYBLOB, TINYTEXT
          Inc(FetchBlockOffset, $FF + sizeof(UInt));
        FIELD_TYPE_GEOMETRY,
        FIELD_TYPE_MEDIUM_BLOB, // MEDIUMBLOB, MEDIUMTEXT
        FIELD_TYPE_LONG_BLOB, // LONGBLOB, LONGTEXT
        FIELD_TYPE_BLOB, // BLOB, TEXT
        FIELD_TYPE_VAR_STRING,
        FIELD_TYPE_STRING:
          Inc(FetchBlockOffset, sizeof(UInt) + DefaultPieceSize);
      end;
    dtDate, dtTime, dtDateTime:
      Inc(FetchBlockOffset, sizeof(UInt) + sizeof(MYSQL_TIME));
    dtFloat{corrected, see GetCorrectedDataType}, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
      Inc(FetchBlockOffset, sizeof(UInt) + FieldDesc.Length + FieldDesc.Scale + 5);
    else
      Assert(False);
  end;
{$IFOPT C+}
  Assert(FetchBlockOffset - OldFetchBlockOffset > 4, IntToStr(FieldDesc.FieldNo) + ' ' + IntToStr(FieldDesc.DataType));
{$ENDIF}
end;

function GenerateTableName(const CatalogName: _string;
  const TableName: _string;
  const DefaultCatalogName: _string): _string;
begin
  if (CatalogName <> '') and (CatalogName <> DefaultCatalogName) then
    Result := _Format('%s.%s',
      [MySQLInfo.QuoteIfNeed(CatalogName),
       MySQLInfo.QuoteIfNeed(TableName)])
  else
    Result := _Format('%s', [MySQLInfo.QuoteIfNeed(TableName)])
end;

function ConvertMySQLTypeToInternalFormat(const MySQLType: TMySqlFieldType;
  const FieldLengthInBytes, Decimals: integer;
  const IsBinary, IsUnsigned, EnableBoolean, OptimizedBigInt, BinaryAsString, Unicode,
  EnableBCD, EnableFMTBCD, CheckPrecision: boolean;
  const CharsetNr: integer;
  var InternalType: word; var Fixed: boolean): boolean;

var
  Prec: integer;
begin
  Fixed := False;
  Result := True;

  case MySQLType of // Must be sync with TCustomMyDataSet.SetNumberRange
    // Integer fields
    FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL: begin // DECIMAL
      if CheckPrecision then begin
        Prec := FieldLengthInBytes - 1;
        if Decimals > 0 then
          Dec(Prec);
        if EnableBCD and (Prec <= MaxBcdPrecision - MaxBcdScale) and
          (Decimals <= MaxBcdScale)
        then
          InternalType := dtBCD
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        else
        if EnableFMTBCD then
          InternalType := dtFmtBCD
      {$ENDIF}
      {$ENDIF}
        else
          InternalType := dtFloat;
      end
      else begin
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        if EnableFMTBCD then
          InternalType := dtFmtBCD
        else
      {$ENDIF}
      {$ENDIF}
        if EnableBCD then
          InternalType := dtBCD
        else
          InternalType := dtFloat;
      end;
    end;
    FIELD_TYPE_TINY: begin// TINYINT
      if (FieldLengthInBytes = 1) and EnableBoolean then
        InternalType := dtBoolean
      else
        if IsUnsigned then
          InternalType := dtWord
        else
          InternalType := dtInt8;
    end;
    MYSQL_TYPE_BIT:
      //MySQL 5.0
      //  BIT(10)     MYSQL_TYPE_BIT      1
      //  BIT(2)      MYSQL_TYPE_BIT      0
      //  BIT         MYSQL_TYPE_BIT      0
      //  BOOL        FIELD_TYPE_TINY     1
      if (FieldLengthInBytes <= 11) and OptimizedBigInt then
        InternalType := dtInt32
      else
        InternalType := dtInt64;
    FIELD_TYPE_SHORT: // SMALLINT
      if IsUnsigned then
        InternalType := dtUInt16
      else
        InternalType := dtInt16;
    FIELD_TYPE_LONG: // INT
      if IsUnsigned then
        InternalType := dtUInt32
      else
        InternalType := dtInt32;
    FIELD_TYPE_LONGLONG: // BIGINT
      if (FieldLengthInBytes <= 11) and OptimizedBigInt then
        InternalType := dtInt32
      else
        InternalType := dtInt64; // 'Unsigned' flag is not used. For details see manual.html#Column_types
    FIELD_TYPE_INT24: // MEDIUMINT
      InternalType := dtInt32; // 'Unsigned' flag is not used. dtInt32 may contain both signed or unsigned 24-bit values

    // Float fields
    FIELD_TYPE_FLOAT: // FLOAT
      InternalType := dtFloat;
    FIELD_TYPE_DOUBLE: // DOUBLE
      InternalType := dtFloat;

    // String fields
    FIELD_TYPE_VAR_STRING, MYSQL_TYPE_VARCHAR: // CHAR(?), VARCHAR
      if IsBinary and (CharsetNr = 63) and not BinaryAsString then
        InternalType := dtVarBytes
      else
        InternalType := dtString;
    FIELD_TYPE_STRING: // CHAR(?), ENUM, SET
    begin
      if IsBinary and (CharsetNr = 63) and not BinaryAsString then
        InternalType := dtBytes
      else
        InternalType := dtString;
      Fixed := True;
    end;

    // DateTime fields
    FIELD_TYPE_TIMESTAMP: // TIMESTAMP14, TIMESTAMP12, TIMESTAMP8, TIMESTAMP6
      InternalType := dtDateTime;
    FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE: // DATE
      InternalType := dtDate;
    FIELD_TYPE_TIME: // TIME
      InternalType := dtTime;
    FIELD_TYPE_DATETIME: // DATETIME
      InternalType := dtDateTime;
    FIELD_TYPE_YEAR: // YEAR4, YEAR2
      InternalType := dtWord;
    FIELD_TYPE_NULL:
      InternalType := dtString;
    FIELD_TYPE_ENUM:
      InternalType := dtString;
    FIELD_TYPE_SET:
      InternalType := dtString;
    FIELD_TYPE_GEOMETRY,
    FIELD_TYPE_TINY_BLOB, // TINYBLOB, TINYTEXT
    FIELD_TYPE_MEDIUM_BLOB, // MEDIUMBLOB, MEDIUMTEXT
    FIELD_TYPE_LONG_BLOB, // LONGBLOB, LONGTEXT
    FIELD_TYPE_BLOB: // BLOB, TEXT
      begin
        if IsBinary then
          InternalType := dtBlob
        else
          InternalType := dtMemo;

        { Commented for ADO etc compatibility

        if FieldLength <= 255 then // this is a TINYBLOB or TINYTEXT
          if IsBinary then
            InternalType := dtVarBytes
          else
            InternalType := dtString;}
      end;
    else
      Result := False;
  end;

  if Result and Unicode {and not IsBinary }then
    case InternalType of
      dtString:
        InternalType := dtWideString;
      dtMemo:
        InternalType := dtWideMemo;
    end;

  Assert(Result, 'Unknown MySQL datatype (' + IntToStr(MySQLType) + ')');
end;

{ EMyError }

{$IFDEF LITE}
procedure DatabaseError(const Message: string; Component: TComponent = nil);
begin
  if Assigned(Component) and (Component.Name <> '') then
    raise Exception.Create(Format('%s: %s', [Component.Name, Message])) else
    raise Exception.Create(Message);
end;

procedure DatabaseErrorFmt(const Message: string; const Args: array of const;
  Component: TComponent = nil);
begin
  DatabaseError(Format(Message, Args), Component);
end;

constructor EMyError.Create(ErrorCode: integer; Msg: _string);
begin
{$IFDEF CLR}
  inherited Create(Msg);
{$ELSE}
  inherited
  Message := Msg;
{$ENDIF}
  FErrorCode := ErrorCode;
end;

{$ELSE}
constructor EMyError.Create(ErrorCode: integer; Msg: _string);
var
  s: _string;
  i, j: integer;
begin
{$IFDEF CLR}
  inherited;
{$ELSE}
  inherited
  Message := Msg;
{$ENDIF}
  FErrorCode := ErrorCode;
  case ErrorCode of
    ER_PARSE_ERROR: begin
      try
        i := Length(Msg);
        //j := 0;
        while i >= 1 do begin
          if (Msg[i] >= '0') and (Msg[i] <= '9') then begin
            // search Line
            j := i;
            Dec(i);
            while (i >= 1) and (Msg[i] >= '0') and (Msg[i] <= '9') do
              Dec(i);
            s := Copy(Msg, i + 1, j - i);
            FLineNumber := StrToInt(s);
            Break;
          end;
          Dec(i);
        end;
      except
        // Silent exception handling
      end;
    end;
  end;
end;
{$ENDIF}

{$IFNDEF LITE}
function EMyError.IsFatalError: boolean;
begin
  Result :=
    (ErrorCode = CR_CONN_HOST_ERROR) or
    (ErrorCode = CR_SERVER_LOST) or
    (ErrorCode = CR_SERVER_GONE_ERROR) or
    (ErrorCode = ER_SERVER_SHUTDOWN);
end;

function EMyError.IsKeyViolation: boolean;
begin
  Result := (ErrorCode = ER_DUP_ENTRY);
end;
{$ENDIF}

function ChangeDecimalSeparator(const Value: Variant): string;
var
  i: integer;
begin
{$IFDEF VER6P}
  Result := string(Value);
{$ELSE}
  if TVarData(Value).VType = varSingle then
    Result := FloatToStr(TVarData(Value).VSingle)
  else
  if TVarData(Value).VType = varDouble then
    Result := FloatToStr(TVarData(Value).VDouble)
  else
    Result := Value;
{$ENDIF}
  if DecimalSeparator <> '.' then begin
  {$IFDEF CLR}
    i := 1; // Delphi bug
  {$ELSE}
  {$IFDEF VER9P}
    i := 1;
  {$ELSE}
    i := 2;
  {$ENDIF}
  {$ENDIF}
    while i < Length(Result) do begin
      if Result[i] = DecimalSeparator then begin
        Result[i] := '.';
        Break;
      end;
      Inc(i);
    end;
  end;
end;

procedure AppendValueToSQL(
  SQL: AnsiStringBuilder;
  const DataType: word;
  const Value: variant;
  const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, NeedBackslashes: boolean;
  const ServerPrimaryVer: integer;
  const Charset: string);
begin
  AppendValueToSQL(SQL, nil, DataType, Value, IsNull, UseUnicode,
  {$IFDEF HAVE_COMPRESS}SendBlobCompressed, {$ENDIF}
  NeedBackslashes, ServerPrimaryVer, Charset);
end;

{$IFNDEF CLR}
procedure AppendValueToSQL(
  SQL: WideStringBuilder;
  const DataType: word;
  const Value: variant;
  const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, NeedBackslashes: boolean;
  const ServerPrimaryVer: integer;
  const Charset: string);
begin
  AppendValueToSQL(nil, SQL, DataType, Value, IsNull, UseUnicode,
  {$IFDEF HAVE_COMPRESS}SendBlobCompressed, {$ENDIF}
  NeedBackslashes, ServerPrimaryVer, Charset);
end;
{$ENDIF}

procedure AppendValueToSQL(
  SQLA: AnsiStringBuilder;
  SQLW: WideStringBuilder;
  const DataType: word;
  const Value: variant;
  const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, NeedBackslashes: boolean;
  const ServerPrimaryVer: integer;
  const Charset: string);

  procedure Append({$IFNDEF LITE}{$IFDEF VER7P}const{$ENDIF}{$ENDIF} Value: string);
  begin
    if SQLA <> nil then
      SQLA.Append(AnsiString(Value))
    else
      SQLW.Append(Value);
  end;

  procedure AppendA({$IFNDEF LITE}const{$ENDIF} Value: AnsiString);
  begin
    if SQLA <> nil then
      SQLA.Append(Value)
    else
      SQLW.Append(WideString(Value));
  end;

  procedure DivMod(Dividend: integer; Divisor: integer;
    out Result, Remainder: integer);
  begin
    Result := Dividend div Divisor;
    Remainder := Dividend - Result * Divisor;
  end;

var
  SecondsAll: integer;
  HourCount, MinCount, SecCount: integer;
  c: Currency;

{$IFNDEF VER6P}
  pParamData: PVarData;
  i: integer;
{$ENDIF}
{$IFDEF CLR}
  s: string;
{$ENDIF}

begin
  if IsNull then
    Append('NULL')
  else
  if (DataType in [dtDate, dtTime, dtDateTime]) and
  {$IFDEF CLR}
    {$IFDEF VER11P}
      (TDateTime(double(Value)) = ZeroDate)
    {$ELSE}
      (TDateTime(Value) = ZeroDate)
    {$ENDIF}
  {$ELSE}
    (VarToDateTime(Value) = ZeroDate)
  {$ENDIF}
  then
    Append('''0000-00-00 00:00:00''')
  else
  begin
    case DataType of
      dtUnknown, dtBytes, dtVarBytes, dtExtVarBytes, dtBlob: // Unicode not used in any case
        AppendA(EscapeAndQuoteVar(Value, False{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, NeedBackslashes, Charset));
      dtString, dtWideString, dtExtString, dtExtWideString, dtMemo, dtWideMemo:
        AppendA(EscapeAndQuoteVar(Value, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, NeedBackslashes, Charset));
      dtDateTime: begin
        Append('''');
        Append(FormatDateTime('YYYY-MM-DD HH:NN:SS', Value));
        Append('''');
      end;
      dtDate: begin
        Append('''');
        Append(FormatDateTime('YYYY-MM-DD', Value));
        Append('''');
      end;
      dtTime: begin
        SecondsAll := Round(TDateTime(Value) * SecsPerDay);
        if SecondsAll >= 0 then
          Append('''')
        else
        begin
          Append('''-');
          SecondsAll := - SecondsAll;
        end;

        DivMod(SecondsAll, 60{Min/Hour} * 60{Sec/Min}, HourCount, SecondsAll);
        DivMod(SecondsAll, 60{Sec/Min}, MinCount, SecCount);

        Append(IntToStr(HourCount));
        if MinCount >= 10 then
          Append(':')
        else
          Append(':0');
        Append(IntToStr(MinCount));
        if SecCount >= 10 then
          Append(':')
        else
          Append(':0');
        Append(IntToStr(SecCount));
        Append('''');
      end;
      dtFloat, dtCurrency:
        Append(ChangeDecimalSeparator(Value));
    {$IFNDEF VER6P}
      dtInt64:
      begin
        if VarType(Value) in [$000E, $0014] then begin
          pParamData := @TVarData(Value); // Source
          Append(IntToStr(PInt64(@pParamData.VInteger)^))
        end
        else
        begin
          i := Value;
          Append(IntToStr(i));
        end;
      end;
    {$ENDIF}
      dtBCD:
      begin
    {$IFNDEF VER6P}
        if VarType(Value) in [$000E, $0014] then
          c := PInt64(@TVarData(Value).VInteger)^
        else
    {$ENDIF}
        c := Value;
        Append(ChangeDecimalSeparator(CurrToStr(c)));
      end;
    {$IFDEF VER6P}
    {$IFNDEF FPC}
      dtFmtBCD:
      begin
      {$IFDEF CLR}
        s := Value.ToString;
        s := ChangeDecimalSeparator(s);

        if (s[1] = '-') and (s[2] = '.') then begin // Delphi bug
          Append('-0.');
          if SQLA <> nil then
            SQLA.Append(s, 2{from 0}, Length(s) - 2)
          else
            SQLW.Append(s, 2{from 0}, Length(s) - 2);
        end
        else
        begin
          if s[1] = '.' then // Delphi bug
            Append('0');

          Append(s);
        end;
      {$ELSE}
        Append(ChangeDecimalSeparator(Value));
      {$ENDIF}
      end;
    {$ENDIF}
    {$ENDIF}
      dtBoolean: // see MySQL reference manual 10.1.4 Boolean Values
        if (ServerPrimaryVer >= 5) and __UseNewBooleanParamType then begin
          if Boolean(Value) then
            Append('b''1''')
          else
            Append('b''0''');
        end
        else begin
          if Boolean(Value) then
            Append('1')
          else
            Append('0');
        end
      else
      {$IFNDEF VER6P}
        if TVarDataD6(Value).VType = 14 then
          Append(IntToStr(TVarDataD6(Value).VInt64))
        else
      {$ENDIF}
        Append(Value);
    end;
  end;
end;

function ValueToSQL(
  const DataType: word;
  const Value: variant;
  const IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}: boolean;
  const ServerPrimaryVer: integer): AnsiString;
var
  SQL: AnsiStringBuilder;
begin
  SQL := AnsiStringBuilder.Create;
  try
    AppendValueToSQL(SQL, DataType, Value, IsNull, UseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, False, ServerPrimaryVer, '');
    Result := SQL.ToString;
  finally
    SQL.Free;
  end;
end;

{ TMySQLConnection }

constructor TMySQLConnection.Create;
begin
  inherited;
  FConnectionTimeout := 15;

  FMySQL := nil;

{$IFDEF HAVE_DIRECT}
  FDirect := True;
  FMySQLAPI := MyAPIDirect;
{$ELSE}
  FMySQLAPI := MyAPIClient;
{$ENDIF}

  FProtocol := mpDefault;
  FEmbParams := TStringList.Create;
  FCheckBackslashes := False;
  FNeedBackslashes := True;
end;

destructor TMySQLConnection.Destroy;
begin
  FEmbParams.Free;
  inherited;
end;

function TMySQLConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TMySQLCommand;
end;

function TMySQLConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TMySQLTransaction;
end;

procedure TMySQLConnection.SetDatabase(const Value: string);
begin
  if FDatabase <> Value then begin
    FDatabase := Value;
    if FConnected and (Value <> '') then
      Check(FMySQLAPI.mysql_select_db(FMySQL, PAnsiChar(AnsiString(FDatabase))), Component);
  end;
end;

procedure TMySQLConnection.Check(const Status: TIntPtrObj; Component: TObject);
begin
  if Status = nil then
    MySQLError(Component);
end;

procedure TMySQLConnection.Check(const Status: integer; Component: TObject);
begin
  if Status <> 0 then
    MySQLError(Component);
end;

procedure TMySQLConnection.Check(Component: TObject);
begin
  if FMySQLAPI.mysql_errno(FMySQL) <> 0 then
    MySQLError(Component);
end;

procedure TMySQLConnection.MySQLError(Component: TObject);
var
  Fail: boolean;
  Err: EMyError;
  Code: longword;
  MsgBuf: AnsiString;
  Msg: _string;
begin
  Code := FMySQLAPI.mysql_errno(FMySQL);
  MsgBuf := FMySQLAPI.mysql_error(FMySQL);
  if FUseUnicode then
    Msg := UTF8Decode(MsgBuf)
  else
    Msg := _string(MsgBuf);

  Err := EMyError.Create(Code, Msg);
  try
  {$IFNDEF LITE}
    Err.Component := Component;
  {$ENDIF}

    Fail := True;

    if Assigned(OnError) then
      DoError(Err, Fail);
    if Fail then
      raise Err
    else
      Abort;
  finally
    if not Fail then
      Err.Free;
  end;
end;


procedure TMySQLConnection.CheckStmt(stmt: PMYSQL_STMT; const Status: integer; Component: TObject);
begin
  if Status <> 0 then
    MySQLStmtError(stmt, Component);
end;

procedure TMySQLConnection.MySQLStmtError(stmt: PMYSQL_STMT; Component: TObject);
var
  Fail: boolean;
  Err: EMyError;
  Code: longword;
  MsgBuf: AnsiString;
  Msg: _string;
begin
  Code := FMySQLAPI.mysql_stmt_errno(stmt);
  MsgBuf := FMySQLAPI.mysql_stmt_error(stmt);
  if FUseUnicode then
    Msg := UTF8Decode(MsgBuf)
  else
    Msg := _string(MsgBuf);

  Err := EMyError.Create(Code, Msg);
{$IFNDEF LITE}
  Err.Component := Component;
{$ENDIF}
  Fail := True;
  if Assigned(OnError) then
    DoError(Err, Fail);
  if Fail then
    raise Err
  else
    Abort;
end;

procedure TMySQLConnection.TryToApplyUseUnicode;
begin
  if (FServerPrimaryVer >= 4)
    and ((FServerPrimaryVer <> 4) or (FServerMinorVer <> 0)) then begin // Ignore 4.0 and below for ODAC compatibility
    if FUseUnicode then
      ExecStatement('SET NAMES utf8')
    else
      if FCharset <> '' then
        ExecStatement('SET NAMES ' + FCharset);
  end
  else
    FUseUnicode := False; // To prevent errors on decode
end;

procedure TMySQLConnection.CheckNoBackslashEscapesMode;

  function GetSQLMode: string;
  var
    RecordSet: TMySQLRecordSet;
    RecBuf: IntPtr;
    v: variant;
  begin
    RecordSet := TMySQLRecordSet.Create;
    try
      RecordSet.FCommand.FConnection := Self;
      RecordSet.FCommand.SQL := 'SELECT @@session.sql_mode';
      RecordSet.SetProp(prFlatBuffers, False);
      RecordSet.SetProp(prLongStrings, True);
      RecordSet.SetProp(prFetchAll, True);
      RecordSet.Open;

      if RecordSet.RecordCount = 0 then
        Exit;
      RecBuf := nil;
      RecordSet.AllocRecBuf(RecBuf);
      try
        RecordSet.GetNextRecord(RecBuf);
        if not RecordSet.GetNull(1, RecBuf) then begin
          RecordSet.GetFieldAsVariant(1, RecBuf, v);
          Result := v;
        end;
      finally
        if RecBuf <> nil then
          RecordSet.FreeRecBuf(RecBuf);
      end;
    finally
      RecordSet.Free;
    end;
  end;

begin
  if FCheckBackslashes then
    FNeedBackslashes := Pos('NO_BACKSLASH_ESCAPES', UpperCase(GetSQLMode)) <= 0
  else
    FNeedBackslashes := True;
end;

procedure TMySQLConnection.DoError(E: Exception; var Fail: boolean);
var
  Reconnect: boolean;
{$IFNDEF LITE}  
  Reexecute: boolean;
  ConnLostCause: TConnLostCause;
{$ENDIF}
begin
  if not Additional then
    inherited
  else begin
    Reconnect := False;
  {$IFNDEF LITE}
    Reexecute := False;
  {$ENDIF}
    if Assigned(OnError) then
      OnError(E, Fail, Reconnect{$IFNDEF LITE}, Reexecute{$ENDIF}, 0{$IFNDEF LITE}, ConnLostCause{$ENDIF});
  end;
end;

procedure TMySQLConnection.Connect(const ConnectString: _string);
var
  MemoryName: AnsiString;
  Utf8CharsetUsed: boolean;

  procedure SetOptions;
  var
    Pr: MYSQL_PROTOCOL_TYPE;
    PrL: integer;
    p: Integer;
  begin
    // Setting options
    Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_OPT_CONNECT_TIMEOUT, FConnectionTimeout), Component);
    Utf8CharsetUsed := FUseUnicode or (LowerCase(FCharset) = 'utf8');
    if Utf8CharsetUsed then
      Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_SET_CHARSET_NAME, PAnsiChar('utf8')), Component);

  {$IFNDEF LITE}
  {$IFDEF HAVE_DIRECT}
    if FMySQLAPI is TMySQLAPIDirect then
      TMySQLAPIDirect(FMySQLAPI).SetIOHandler(FMySQL, FIOHandler);
  {$ENDIF}
  {$ENDIF}

    // Protocol
  {$IFDEF HAVE_OPENSSL}
    if FProtocol = mpSSL then begin
      FSSL_Key := StringReplace(FSSL_Key, '\', '/', [rfReplaceAll]);
      FSSL_Cert := StringReplace(FSSL_Cert, '\', '/', [rfReplaceAll]);
      FSSL_CA := StringReplace(FSSL_CA, '\', '/', [rfReplaceAll]);

      // Without checking as error in non-direct implementation
      FMySQLAPI.mysql_ssl_set(FMySQL, PAnsiChar(AnsiString(FSSL_Key)),
        PAnsiChar(AnsiString(FSSL_Cert)), PAnsiChar(AnsiString(FSSL_CA)),
        nil, PAnsiChar(AnsiString(FSSL_Chipher)));
    end
    else
  {$ENDIF}
  {$IFNDEF LITE}
    if FProtocol = mpHttp then begin
      if not (FMySQLAPI is TMySQLAPIDirect) then
        DatabaseError('Wrong protocol');
      TMySQLAPIDirect(FMySQLAPI).SetHttpOptions(FMySQL, HttpOptions);
    end
    else
  {$ENDIF}
    if FProtocol <> mpDefault then begin
      if IsClient41 then begin
        Pr := MYSQL_PROTOCOL_DEFAULT;
        case FProtocol of
          mpTCP:
            Pr := MYSQL_PROTOCOL_TCP;
          mpPipe:
            Pr := MYSQL_PROTOCOL_PIPE;
          mpSocket:
            Pr := MYSQL_PROTOCOL_SOCKET;
          mpMemory:
            Pr := MYSQL_PROTOCOL_MEMORY;
        else
          DatabaseError('Wrong protocol');
        end;
        PrL := Integer(Pr);
        Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_OPT_PROTOCOL, PrL), Component);

        if FProtocol = mpMemory then begin
          p := Pos(',', FServer);
          if p > 0 then begin
            MemoryName := AnsiString(Copy(FServer, p + 1, Length(FServer)));
            Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_SHARED_MEMORY_BASE_NAME, PAnsiChar(MemoryName)), Component);
          end;
        end;
      end
      else
        if FProtocol = mpPipe then begin
          PrL := 0;
          Check(FMySQLAPI.mysql_options(FMySQL, MYSQL_OPT_NAMED_PIPE, PrL), Component);
        end;
    end;

  end;

var
  Flags: Cardinal;
  HostName: _string;
  pServer: PAnsiChar;
  pUsername, pPassword: PAnsiChar;
  p: Integer;
begin
  if not FConnected then begin
    FServerVer := '';
    FServerPrimaryVer := 0;
    FServerMinorVer := 0;
    if FMySQLAPI is TMySQLAPIEmbedded then
      TMySQLAPIEmbedded(FMySQLAPI).Params := FEmbParams;
    FMySQLAPI.CheckMySQLLib;
    try
      if FMySQL <> nil then
        Disconnect;

      Assert(FMySQL = nil);

      // Getting FMySQL
      FMySQL := FMySQLAPI.mysql_init(FMySQL);
      Check(FMySQL, Component);
      SetOptions;

      // Other opt
      Flags := CLIENT_FOUND_ROWS;
      if IsClient41 then
        Flags := Flags + CLIENT_MULTI_STATEMENTS;

      if FCompress then
        Flags := Flags + CLIENT_COMPRESS;

      if FServer = '' then
        pServer := 'localhost'#0
      else begin
        p := Pos(',', FServer);
        if p > 0 then begin
          HostName := Copy(FServer, 1, p - 1);
          pServer := PAnsiChar(AnsiString(HostName));
        end
        else
          pServer := PAnsiChar(AnsiString(FServer));
      end;

      if Utf8CharsetUsed then begin
        pUsername := PAnsiChar(AnsiString(Utf8Encode(WideString(FUsername))));
        pPassword := PAnsiChar(AnsiString(Utf8Encode(WideString(FPassword))));
      end
      else begin
        pUsername := PAnsiChar(AnsiString(FUsername));
        pPassword := PAnsiChar(AnsiString(FPassword));
      end;

      Check(FMySQLAPI.mysql_real_connect(FMySQL, pServer, pUsername, pPassword,
        PAnsiChar(AnsiString(FDatabase)), FPort, nil, Flags), Component);
      FConnected := True;
      FNativeConnection := True;
      inherited;

      FServerVer := string(FMySQLAPI.mysql_get_server_info(FMySQL));

      DecodeVersion(FServerVer, FServerPrimaryVer, FServerMinorVer, FServerReleaseVer);
      TryToApplyUseUnicode;
      CheckNoBackslashEscapesMode;
    except
      on EFailOver do;
      else begin
        try
          Disconnect;
        except
        end;
        raise;
      end;
    end;
  end;
end;

procedure TMySQLConnection.Disconnect;
begin
  if (FMySQL <> nil) and FNativeConnection then
    FMySQLAPI.mysql_close(FMySQL);
  FMySQL := nil;
  FConnected := False;
end;

procedure TMySQLConnection.Assign(Source: TCRConnection);
var
  Src: TMySQLConnection;
begin
  inherited;

  Src := TMySQLConnection(Source);
  FConnectionTimeout := Src.FConnectionTimeout;
  FDatabase := Src.FDatabase;
  FPort := Src.FPort;
  FCompress := Src.FCompress;
  FUseUnicode := Src.FUseUnicode;
  FCharset := Src.FCharset;
  FProtocol := Src.FProtocol;
  FMySQLAPI := Src.FMySQLAPI;
  FDirect := Src.FDirect;
  FEmbedded := Src.FEmbedded;
  FOptimizedBigInt := Src.FOptimizedBigInt;
  FNullForZeroDelphiDate := Src.FNullForZeroDelphiDate;
  FCheckPrecision := Src.FCheckPrecision;
end;

procedure TMySQLConnection.AssignConnect(Source: TCRConnection);
var
  Src: TMySQLConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TMySQLConnection(Source);
      Assign(Src);
      FMySQL := Src.FMySQL;
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := True;
      FNativeConnection := False;
    end;
  end;
end;

procedure TMySQLConnection.ExecStatement(const SQL: _string);
var
  FCommand: TMySQLCommand;
begin
  FCommand := TMySQLCommand.Create;
  try
    FCommand.SetConnection(Self);
    FCommand.Component := Component; 
    FCommand.SetSQL(SQL);
    FCommand.FCommandTimeout := FConnectionTimeout;
    FCommand.Execute;
  finally 
    FCommand.Free;
  end;
end;

procedure TMySQLConnection.SetMySQLAPI;
begin
  if FEmbedded then
    FMySQLAPI := MyAPIEmbedded
  else
  {$IFDEF HAVE_DIRECT}
    if FDirect then
      FMySQLAPI := MyAPIDirect
    else
  {$ENDIF}
      FMySQLAPI := MyAPIClient;
end;

procedure TMySQLConnection.SetEmbParamsStr(const EmbParams: string);

  procedure CheckDirParam(const ParamName: string);
  var
    s: string;
  begin
    s := FEmbParams.Values[ParamName];
    if s = '' then
      Exit;

    if (s <> '') and ((s[1] = '/') or (s[1] = '\')) then
      s := Copy(s, 2, MaxInt);

    if (s <> '') and (s[1] = '.') and // relative path
      (ParamName = '--basedir') and
      (CurrentProjectOutputDir <> '')
    then
      s := IncludeTrailingBackslash(CurrentProjectOutputDir) + s;

    s := StringReplace(s, '\', '/', [rfReplaceAll]);
    FEmbParams.Values[ParamName] := s;
  end;

begin
  FEmbParams.Text := Trim(EmbParams);

  CheckDirParam('--basedir');
  CheckDirParam('--datadir');
  CheckDirParam('--character-sets-dir');
  CheckDirParam('--tmpdir');
  CheckDirParam('--log-bin');
  CheckDirParam('--log-bin-index');
end;

function TMySQLConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      SetDatabase(Value);
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prPort:
      FPort := Value;
    prCompress:
      FCompress := Value;
    prUseUnicode: begin
      if FUseUnicode <> Value then begin
        FUseUnicode := Value;
        if FConnected then
          TryToApplyUseUnicode;
      end;
    end;
    prCharset: begin
      if FCharset <> Value then begin
        FCharset := Value;
        if FConnected and not FUseUnicode then
          TryToApplyUseUnicode;
      end;
    end;
    prProtocol:
      FProtocol := TMyProtocol(Integer(Value));
    prEmbParams:
      SetEmbParamsStr(Value);
    prEmbedded: begin
      FEmbedded := Value;
      SetMySQLAPI;
    end;
    prDirect: begin
      FDirect := Value;
      SetMySQLAPI;
    end;
    prOptimizedBigInt:
      FOptimizedBigInt := Value;
    prNullForZeroDelphiDate:
      FNullForZeroDelphiDate := Value;
  {$IFDEF HAVE_OPENSSL}
    prSSL_Chipher:
      FSSL_Chipher := Value;
    prSSL_CA:
      FSSL_CA := Value;
    prSSL_Key:
      FSSL_Key := Value;
    prSSL_Cert:
      FSSL_Cert := Value;
  {$ENDIF}
    prCheckPrecision:
      FCheckPrecision := Value;
    prCheckBackslashes: begin
      if FCheckBackslashes <> Value then begin
        FCheckBackslashes := Value;
        if FConnected then
          CheckNoBackslashEscapesMode;
      end;
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMySQLConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prDatabase:
      Value := FDatabase; // string
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prPort:
      Value := FPort;
    prCompress:
      Value := FCompress;
    prUseUnicode:
      Value := FUseUnicode;
    prLastInsertId:
    {$IFDEF VER6P}
      Value := FMySQLAPI.mysql_insert_id(FMySQL);
    {$ELSE}
    begin
      Value := Unassigned;
      TVarData(Value).VType := $000E;
      PInt64(@TVarData(Value).VInteger)^ := FMySQLAPI.mysql_insert_id(FMySQL);
    end;
    {$ENDIF}
    {prCharset:
      Value := string(FMySQLAPI.mysql_character_set_name(FMySQL)); // string}
    prCheckBackslashes:
      Value := FCheckBackslashes;
    prNeedBackslashes:
      Value := FNeedBackslashes;
    prCharset:
      Value := FCharset;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TMySQLConnection.GetClientVersion: _string;
begin
  if FMySQLAPI.LoadedMySQLLib then
    Result := FMySQLAPI.ClientVer
  else
  begin
    if FMySQLAPI is TMySQLAPIEmbedded then
      TMySQLAPIEmbedded(FMySQLAPI).Params := FEmbParams;
    FMySQLAPI.CheckMySQLLib;
    Result := FMySQLAPI.ClientVer;
  {$IFNDEF CLR}
    FMySQLAPI.FreeMySQLLib;
  {$ENDIF}
  end;
end;

function TMySQLConnection.GetIsClient41: boolean;
begin
  Assert(FMySQLAPI <> nil);
  Result := (FMySQLAPI.ClientStructVer = cv410)
    or (FMySQLAPI.ClientStructVer = cv411)
  {$IFDEF HAVE_DIRECT}
    or (FMySQLAPI.ClientStructVer = cvDirect)
  {$ENDIF}
  ;
end;

function TMySQLConnection.GetIsServer41: boolean;
begin
  Assert(FMySQLAPI <> nil);
  Result := (FServerPrimaryVer >= 5) or
    ((FServerPrimaryVer = 4) and (FServerMinorVer >= 1));
end;

function TMySQLConnection.GetServerVersion: _string;
begin
  Result := IntToStr(FServerPrimaryVer) + '.' + IntToStr(FServerMinorVer) +
    '.' + IntToStr(FServerReleaseVer);
end;

function TMySQLConnection.GetServerVersionFull: _string;
begin
  Result := FServerVer;
end;

function TMySQLConnection.GetThreadId: longword;
begin
  Assert(FMySQLAPI <> nil);
  Result := MySQLAPI.mysql_thread_id(FMySQL);
end;

function TMySQLConnection.CheckIsValid: boolean;
begin
  FIsValid := (FMySQLAPI <> nil) and (FMySQL <> nil) and (FMySQLAPI.mysql_ping(FMySQL) = 0)
    and (FMySQLAPI.mysql_ping(FMySQL) = 0); // on some server versions two pings is needed to detect that connection is killed
  Result := FIsValid;
end;

{ TMySQLCommand }

procedure TMySQLCommand.Check(const Status: TIntPtrObj);
begin
  Assert(FConnection <> nil);
  if Status = nil then
    FConnection.MySQLError(Component);
end;

procedure TMySQLCommand.Check(const Status: integer);
begin
  Assert(FConnection <> nil);
  if Status <> 0 then
    FConnection.MySQLError(Component);
end;

procedure TMySQLCommand.Check;
begin
  Assert(FConnection <> nil);
  FConnection.Check(Component);
end;

procedure TMySQLCommand.CheckStmt(const Status: integer);
begin
  Assert(FConnection <> nil);
  if Status <> 0 then
    FConnection.MySQLStmtError(Fstmt, Component);
end;

const
  SQL_UNKNOWN = 0;
  
constructor TMySQLCommand.Create;
begin
  inherited;
  FCommandTimeout := DefaultCommandTimeout;
  FRequestResultset := False;
  FRowsAffected := -1;
  FCursorState := csInactive;
  FCachedSQL := _TStringList.Create;
  FBreakExecCS := TCriticalSection.Create;
end;

destructor TMySQLCommand.Destroy;
begin
  FCachedSQL.Free;
  FBreakExecCS.Free;
  inherited;
end;

procedure TMySQLCommand.Prepare;
var
  u: AnsiString;
begin
  if GetPrepared then
    Exit;

  if not FConnection.IsClient41 then
    DatabaseError(SPrepareNotSupportedC);

  if not FConnection.IsServer41 then
    DatabaseError(SPrepareNotSupportedS);

  Fstmt := FConnection.FMySQLAPI.mysql_stmt_init(FConnection.FMySQL);
  Check(Fstmt);

  try
    if {$IFDEF HAVE_DIRECT}(FConnection.FMySQLAPI.ClientStructVer = cvDirect) or {$ENDIF}
      FConnection.IsClient41 then begin
      Check(FConnection.FMySQLAPI.mysql_options(FConnection.FMySQL, MYSQL_OPT_READ_TIMEOUT, FCommandTimeout));
      Check(FConnection.FMySQLAPI.mysql_options(FConnection.FMySQL, MYSQL_OPT_WRITE_TIMEOUT, FCommandTimeout));
    end;
    if FConnection.FUseUnicode then
      u := UTF8Encode(FSQL)
    else
      u := AnsiString(FSQL);
    CheckStmt(FConnection.FMySQLAPI.mysql_stmt_prepare(Fstmt, PAnsiChar(u), Length(u)));
  {$IFDEF AUTOTEST}
    Inc(__ServerPrepareCount);
  {$ENDIF}
  except
    if Fstmt <> nil then begin
      FConnection.FMySQLAPI.mysql_stmt_close(Fstmt);
      Fstmt := nil;
    end;
    raise;
  end;
  FPrepared := True;

  inherited;
end;

procedure TMySQLCommand.Unprepare;
begin
  if GetPrepared and (Fstmt <> nil) then begin
    FConnection.FMySQLAPI.mysql_stmt_close(Fstmt);
    Fstmt := nil;
    inherited;
    FPrepared := False;
  end;
end;

function TMySQLCommand.GetPrepared: boolean;
begin
  Result := FPrepared;
end;

{$IFNDEF CLR}
type
  _TParamDesc = class (TParamDesc);
{$ENDIF}

function TMySQLCommand.CalcRowsAffected: integer;
begin
  if not GetPrepared then begin
    Result := FConnection.FMySQLAPI.mysql_affected_rows(FConnection.FMySQL);
    if FConnection.FMySQLAPI.mysql_field_count(FConnection.FMySQL) = 0 then
      FCursorState := csInactive
    else
      FCursorState := csExecuted;
  end
  else
  begin
    Result := FConnection.FMySQLAPI.mysql_stmt_affected_rows(Fstmt);
    if FConnection.FMySQLAPI.mysql_stmt_field_count(Fstmt) = 0 then
      FCursorState := csInactive
    else
      FCursorState := csExecuted;
  end
end;

function TMySQLCommand.ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string;
begin
  if Pos(':', SQL) = 0 then begin
    if Params <> nil then
      Params.Clear;
    Result := SQL; // query without parameters
  end
  else
    Result := inherited ParseSQL(SQL, Params, ReplaceAll, RenamePrefix);
end;

procedure TMySQLCommand.ParseSQLParam(ParsedSQL: _StringBuilder; Parser: TSQLParser; Params: TParamDescs; LeftQuote, RightQuote: _char; const RenamePrefix: _string);
var
  OldParamCount: integer;
  OldLen: integer;
begin
  if Params = nil then begin
    inherited;
    Exit;
  end;

  OldParamCount := Params.Count;
  OldLen := ParsedSQL.Length;

  inherited;

  if Params.Count = OldParamCount + 1 then begin
    Assert(ParsedSQL.Length = OldLen + 1);
    SetProp(prParamPosition, OldLen + 1);
  end;
end;

procedure TMySQLCommand.GetSPParams(const Name: _string; out IsFunc: boolean;
  out ParamList: _string; out ReturnParam: _string);

  type
    TMyParamBlockResult = record
      Params: _string;
      Returns: _string;
    end;

  procedure _GetSPParams(out IsFunc: boolean; out ParamList: _string; out ReturnParam: _string);
  var
    RecordSet: TMySQLRecordSet;

  var
    RecBuf: IntPtr;
    Blob: TBlob;
    v: variant;
    Database, ProcName: _string;
  begin
    IsFunc := False;
    ParamList := '';
    ReturnParam := '';

    if FConnection.FServerPrimaryVer < 5 then
      Exit;

    RecordSet := TMySQLRecordSet.Create;
    try
      RecordSet.FCommand.FConnection := FConnection;

      MySQLInfo.SplitObjectName(Name, Database, ProcName);
      if Database <> '' then
        Database := SQLInfo.NormalizeName(Database, False, True)
      else
        Database := FConnection.FDatabase;
      ProcName := SQLInfo.NormalizeName(ProcName, False, True);

      // Can't use LOWER (5.0.9 bug) RecordSet.FCommand.SQL := 'SELECT type, returns, param_list FROM mysql.proc WHERE (name = ''' + Name + ''') AND (LOWER(db) = LOWER(''' + FDatabase + '''))';
      RecordSet.FCommand.SQL := 'SELECT type, returns, param_list FROM mysql.proc WHERE (name = ''' + ProcName + ''') AND (LOWER(db) = ''' + AnsiLowerCase(Database) + ''')';
      RecordSet.SetProp(prFlatBuffers, False);
      RecordSet.SetProp(prLongStrings, True);
      RecordSet.SetProp(prFetchAll, True);
      RecordSet.Open;

      if RecordSet.RecordCount = 0 then
        Exit;
      //Assert(RecordSet.RecordCount = 1, Name); // CR 18269 M

      RecBuf := nil;
      RecordSet.AllocRecBuf(RecBuf);
      try
        //while True do begin // CR 18269 M
          RecordSet.GetNextRecord(RecBuf);
          if not RecordSet.Eof then begin
            RecordSet.GetFieldAsVariant(1, RecBuf, v);
            IsFunc := UpperCase(v)= 'FUNCTION';

            RecordSet.GetFieldAsVariant(2, RecBuf, v);
            ReturnParam := v;

            if not RecordSet.GetNull(3, RecBuf) then begin
              Blob := RecordSet.GetObject(3, RecBuf) as TBlob;
              if Blob.Size > 0 then
                ParamList := Blob.{$IFDEF IS_UNICODE}AsWideString{$ELSE}AsString{$ENDIF};
            end;
          end;
      finally
        if RecBuf <> nil then
          RecordSet.FreeRecBuf(RecBuf);
      end;

    finally
      RecordSet.Free;
    end;
  end;

  function GetCreateSQL: _string;
  var
    RecordSet: TMySQLRecordSet;
    RecBuf: IntPtr;
    v: variant;
    Database, ProcName: _string;
  begin
    RecordSet := TMySQLRecordSet.Create;
    try
      RecordSet.FCommand.FConnection := FConnection;
      MySQLInfo.SplitObjectName(Name, Database, ProcName);
      if Database <> '' then
        Database := SQLInfo.NormalizeName(Database, False, True)
      else
        Database := FConnection.FDatabase;
      ProcName := SQLInfo.NormalizeName(ProcName, False, True);

      RecordSet.FCommand.SQL := 'SELECT ROUTINE_TYPE FROM INFORMATION_SCHEMA.ROUTINES '+
        'WHERE (ROUTINE_NAME = ''' + ProcName + ''') AND (LOWER(ROUTINE_SCHEMA) = ''' +
        _LowerCase(Database) + ''')';
      RecordSet.SetProp(prFlatBuffers, False);
      RecordSet.SetProp(prLongStrings, True);
      RecordSet.SetProp(prFetchAll, True);
      RecordSet.Open;

      if RecordSet.RecordCount = 0 then
        Exit;
      //Assert(RecordSet.RecordCount = 1, Name); // CR 18269 M

      RecBuf := nil;
      RecordSet.AllocRecBuf(RecBuf);
      try
        //while True do begin // CR 18269 M
          RecordSet.GetNextRecord(RecBuf);
          if not RecordSet.Eof then begin
            RecordSet.GetFieldAsVariant(1, RecBuf, v);
            IsFunc := UpperCase(v)= 'FUNCTION';
          end;
      finally
        if RecBuf <> nil then
          RecordSet.FreeRecBuf(RecBuf);
      end;

    finally
      RecordSet.Free;
    end;

    if IsFunc then
      Result := 'SHOW CREATE FUNCTION ' + Database + '.' + SQLInfo.QuoteIfNeed(ProcName)
    else
      Result := 'SHOW CREATE PROCEDURE ' + Database + '.' + SQLInfo.QuoteIfNeed(ProcName);

    RecordSet := TMySQLRecordSet.Create;
    try
      RecordSet.FCommand.FConnection := FConnection;

      RecordSet.FCommand.SQL := Result;
      RecordSet.SetProp(prFlatBuffers, False);
      RecordSet.SetProp(prLongStrings, True);
      RecordSet.SetProp(prFetchAll, True);
      RecordSet.Open;
      RecordSet.AllocRecBuf(RecBuf);
      try
        //while True do begin // CR 18269 M
        RecordSet.GetNextRecord(RecBuf);
        if not RecordSet.Eof then begin
          if not RecordSet.GetNull(3, RecBuf) then begin
            RecordSet.GetFieldAsVariant(3, RecBuf, v);
            Result := v;
          end;
        end;
      finally
        if RecBuf <> nil then
          RecordSet.FreeRecBuf(RecBuf);
      end;
    finally
      RecordSet.Free;
    end;
  end;

  function GetParamBlock(aProcSrc: _string): TMyParamBlockResult;
  var
    aReturnsBeginPos: integer;

    function GetParamBlockParams: _string;
    var
      i, aBeginPos, aEndPos, aColonCount: integer;
    begin
      Result := '';
      aBeginPos := Pos('(', aProcSrc);
      if aBeginPos > aReturnsBeginPos then
        Exit;
      if aBeginPos = 0 then
        Exit;
      aColonCount := 1;
      aEndPos := 0;
      for i := aBeginPos + 1 to Length(aProcSrc) - 1 do
      begin
        if aProcSrc[i] = '(' then
          Inc(aColonCount)
        else
          if aProcSrc[i] = ')' then
            Dec(aColonCount);
        if aColonCount = 0 then
        begin
          aEndPos := i;
          Break;
        end;
      end;
      if aColonCount <> 0 then Exit;
      Result := Copy(aProcSrc, aBeginPos + 1, (aEndPos - aBeginPos - 1));
    end;

    function GetParamBlockReturns: _string;
    var
      Parser: TMyParser;
      aLexem: _string;
      aCurrToken: Integer;
      aTypeSize: _string;

      function ParserNext(out Lexem: _string): Integer;
      begin
        Result := Parser.GetNext(Lexem);
        aCurrToken := Result;
      end;
    begin
      Result := '';
      Parser := TMyParser.Create(aProcSrc);
      try
        Parser.OmitBlank := True;
        Parser.OmitComment := True;
        Parser.Uppered := True;
        if not Parser.ToLexem('RETURNS') then
          exit;

        aReturnsBeginPos := Parser.CurrPos;

        if Parser.GetNext(aLexem) <> lcIdent then
          Exit;

        Result := aLexem;

        if (Parser.GetNext(aLexem) = lcEnd) or (aLexem <> '(') then
          Exit;

        Result := Result + aLexem;

        Parser.OmitBlank := False;

        aTypeSize := '';
        while (ParserNext(aLexem) <> lcEnd) and (aLexem <> ')') do
        begin
          if aLexem = '(' then
            Exit;

          aTypeSize := aTypeSize + aLexem;
        end;

        if (aCurrToken <> lcEnd) and (aLexem = ')') then
          Result := Result + aTypeSize + ')';

      finally
        Parser.Free;
      end;
    end;

  begin
    aReturnsBeginPos := MaxInt;
    Result.Returns := GetParamBlockReturns;
    Result.Params := GetParamBlockParams;
  end;

begin
  if (FConnection.FServerPrimaryVer = 5) and (FConnection.FServerMinorVer = 0) and
    (FConnection.FServerReleaseVer < 4)
  then
    _GetSPParams(IsFunc, ParamList, ReturnParam)
  else
    with GetParamBlock(GetCreateSQL) do
    begin
      ParamList := Params;
      ReturnParam := Returns;
    end;
end;

procedure TMySQLCommand.DescribeParams(const ParamList: _string; const IsResult: boolean;
  var ParamsInfo: TParamInfos; ParamDescs: TParamDescs);
var
  Parser: TMyParser;
  StLex: _string;
  CodeLexem: integer;
  s, Name: _string;

  Param: TParamDesc;
  ParamType: TParamDirection;
  pii: integer;
  sz, code: Integer;
begin
  Parser := TMyParser.Create(ParamList);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    while True do begin
      CodeLexem := Parser.GetNext(StLex);
      if CodeLexem = lcEnd then
        Break;

      pii := Length(ParamsInfo);
      SetLength(ParamsInfo, pii + 1);

      if ParamDescs <> nil then begin
        Param := TParamDesc.Create;//(Params, ParamType);
        ParamDescs.Add(Param);
      end
      else
        Param := nil;

      // Parameter direction
      if not IsResult then begin
        s := UpperCase(StLex);
        ParamType := pdUnknown;
        if s = 'IN' then
          ParamType := pdInput
        else
        if s = 'OUT' then
          ParamType := pdOutput
        else
        if s = 'INOUT' then
          ParamType := pdInputOutput;

        if ParamType = pdUnknown then
          ParamType := pdInput // as default
        else
          {$IFOPT C+}CodeLexem := {$ENDIF}Parser.GetNext(StLex);

        // Parameter name
        Assert((CodeLexem = lcIdent) or (CodeLexem = lxSET) or (CodeLexem = lxSTATUS), StLex + ' in ' + ParamList + ', Name = ' + Name);

        Name := StLex;

        // Parameter type
        {$IFOPT C+}CodeLexem := {$ENDIF}Parser.GetNext(StLex);
      end
      else
      begin
        ParamType := pdResult;
        Name := SResultParamName;
      end;

      if not IsResult then
        ParamsInfo[pii].Name := Name;
      if Param <> nil then
        Param.SetName(Name);

      ParamsInfo[pii].Direction := ParamType;
      if Param <> nil then
        Param.SetParamType(ParamType);

      // Parameter type
      Assert((CodeLexem = lcIdent) or (CodeLexem = lxSET), StLex + ' in ' + ParamList + ', Name = ' + Name);  // SET may be both identifier and datatype
      s := UpperCase(StLex);
      ParamsInfo[pii].ParamType := s;
      ParamsInfo[pii].Unsigned := False;

      if Param <> nil then begin
        if s = 'BIT' then
          Param.SetDataType(dtBoolean)
        else
        if s = 'TINYINT' then
          Param.SetDataType(dtInt8)
        else
        if s = 'SMALLINT' then
          Param.SetDataType(dtInt16)
        else
        if s = 'MEDIUMINT' then
          Param.SetDataType(dtInt32)
        else
        if (s = 'INT') or (s = 'INTEGER') then
          Param.SetDataType(dtInt32)
        else
        if s = 'BIGINT' then
          Param.SetDataType(dtInt64)
        else
        if s = 'DOUBLE' then
          Param.SetDataType(dtFloat)
        else
        if s = 'FLOAT' then
          Param.SetDataType(dtFloat)
        else
        if (s = 'DECIMAL') or (s = 'NUMERIC') then // DECIMAL(10, 2)?
          Param.SetDataType(dtFloat)
        else
        if (s = 'CHAR') or (s = 'NCHAR') then // (SIZE)?, BINARY?
          Param.SetDataType(dtString)
        else
        if (s = 'VARCHAR') or (s = 'NVARCHAR') then // (SIZE)?, BINARY?
          Param.SetDataType(dtString)
        else
        if s = 'DATE' then
          Param.SetDataType(dtDate)
        else
        if s = 'TIME' then
          Param.SetDataType(dtTime)
        else
        if s = 'TIMESTAMP' then // (14)?
          Param.SetDataType(dtDateTime)
        else
        if s = 'DATETIME' then
          Param.SetDataType(dtDateTime)
        else
        if s = 'YEAR' then // (4)?
          Param.SetDataType(dtWord)
        else
        if (s = 'BOOL') or (s = 'BOOLEAN') then
          Param.SetDataType(dtInt8)
        else
        if s = 'TINYBLOB' then
          Param.SetDataType(dtBlob)
        else
        if s = 'BLOB' then
          Param.SetDataType(dtBlob)
        else
        if s = 'MEDIUMBLOB' then
          Param.SetDataType(dtBlob)
        else
        if s = 'LONGBLOB' then
          Param.SetDataType(dtBlob)
        else
        if s = 'TINYTEXT' then
          Param.SetDataType(dtMemo)
        else
        if s = 'TEXT' then
          Param.SetDataType(dtMemo)
        else
        if s = 'MEDIUMTEXT' then
          Param.SetDataType(dtMemo)
        else
        if s = 'LONGTEXT' then
          Param.SetDataType(dtMemo)
        else
        if s = 'ENUM' then // ('value1','value2','value3')?
          Param.SetDataType(dtString)
        else
        if s = 'SET' then // ('value1','value2','value3')
          Param.SetDataType(dtString)
        else
        if s = 'GEOMETRY' then
          Param.SetDataType(dtBlob)
        else
        if s = 'POINT' then
          Param.SetDataType(dtBlob)
        else
        if s = 'LINESTRING' then
          Param.SetDataType(dtBlob)
        else
        if s = 'POLYGON' then
          Param.SetDataType(dtBlob)
        else
        if s = 'GEOMETRYCOLLECTION' then
          Param.SetDataType(dtBlob)
        else
        if s = 'MULTIPOINT' then
          Param.SetDataType(dtBlob)
        else
        if s = 'MULTILINESTRING' then
          Param.SetDataType(dtBlob)
        else
        if s = 'MULTIPOLYGON' then
          Param.SetDataType(dtBlob)
        else
          raise Exception.Create('Unknown column type "' + s + '" in "' + ParamList + '"');
      end;

      ParamsInfo[pii].Size := -1;
      repeat
        CodeLexem := Parser.GetNext(StLex);
        if StLex = '(' then
          repeat
            CodeLexem := Parser.GetNext(StLex);
            Val(StLex, sz, code);
            if code = 0 then
              ParamsInfo[pii].Size := sz;

            if (Param <> nil) and (ParamsInfo[pii].Size <> -1) then begin
              if (Param.GetDataType = dtString) and (CodeLexem = lcNumber) then
                Param.SetSize(ParamsInfo[pii].Size);
              if (Param.GetDataType = dtInt8) and (CodeLexem = lcNumber) then
                Param.SetDataType(dtBoolean);
            end;

            if ((s = 'DECIMAL') or (s = 'NUMERIC')) and (StLex <> ')') then
              ParamsInfo[pii].ParamType := ParamsInfo[pii].ParamType + '(' + StLex + ')';
          until (CodeLexem = lcEnd) or (StLex = ')');
        if UpperCase(StLex) = 'UNSIGNED' then
          ParamsInfo[Length(ParamsInfo) - 1].Unsigned := True;
      until (CodeLexem = lcEnd) or (StLex = ',');

      if Param <> nil then
        if FConnection.FUseUnicode then
          case Param.GetDataType of
            dtString:
              Param.SetDataType(dtWideString);
            dtMemo:
              Param.SetDataType(dtWideMemo);
          end;
    end;
  finally
    Parser.Free;
  end;
end;

function TMySQLCommand.CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string;

  function GetSelectForParam(const ParamName: _string; const QuotedParamName: _string; ParamInfo: TParamInfo): _string;
  begin
    if (ParamInfo.ParamType = 'TINYINT') or (ParamInfo.ParamType = 'SMALLINT') or
      (ParamInfo.ParamType = 'MEDIUMINT') or (ParamInfo.ParamType = 'INT') or
      (ParamInfo.ParamType = 'INTEGER') or (ParamInfo.ParamType = 'BIGINT') then begin
      Result := _Format('CAST(%s AS', [QuotedParamName]);
      if ParamInfo.Unsigned then
        Result := Result + ' UNSIGNED)'
      else
        Result := Result + ' SIGNED)';
      Result := Result + ' AS ' + _QuotedStr(ParamName, '''');
    end
    else
    if ParamInfo.ParamType = 'YEAR' then
      Result := _Format('CAST(%s AS UNSIGNED) AS %s', [QuotedParamName, _QuotedStr(ParamName, '''')])
    else
    if ParamInfo.ParamType = 'DATE' then
      Result := _Format('CAST(%s AS DATE) AS %s', [QuotedParamName, _QuotedStr(ParamName, '''')])
    else
    if ParamInfo.ParamType = 'TIME' then
      Result := _Format('CAST(%s AS TIME) AS %s', [QuotedParamName, _QuotedStr(ParamName, '''')])
    else
    if ParamInfo.ParamType = 'DATETIME' then
      Result := _Format('CAST(%s AS DATETIME) AS %s', [QuotedParamName, _QuotedStr(ParamName, '''')])
    else
    if (ParamInfo.ParamType = 'TINYBLOB') or (ParamInfo.ParamType = 'BLOB') or
      (ParamInfo.ParamType = 'MEDIUMBLOB') or (ParamInfo.ParamType = 'LONGBLOB') then
      Result := _Format('CAST(%s AS BINARY) AS %s', [QuotedParamName, _QuotedStr(ParamName, '''')])
    else
    if (Copy(ParamInfo.ParamType, 1, 7) = 'DECIMAL') or (Copy(ParamInfo.ParamType, 1, 7) = 'NUMERIC') then
      Result := _Format('CAST(%s AS ' + ParamInfo.ParamType + ') AS %s', [QuotedParamName, _QuotedStr(ParamName, '''')])
    else
      Result := _Format('%s AS %s', [QuotedParamName, _QuotedStr(ParamName, '''')]);
  end;

var
  i, iMin: integer;
  sCall, sParams, sUserParams, sSet, sSelect, sCastSelect, sUserSet: _string;
  ParamName, OriginalParamName: _string;

  IsFunc: boolean;
  ParamList: _string;
  ReturnParam: _string;
  Param: TParamDesc;
  ParamsInfo: TParamInfos;
  pd: TParamDescs;

begin
  if NeedDescribe or (Params.Count > 0) then
    GetSPParams(Name, IsFunc, ParamList, ReturnParam)
  else
    IsFunc := False;

  if NeedDescribe then begin
    Params.Clear;
    pd := Params;
  end
  else
    pd := nil;

  ParamsInfo := nil;
  if NeedDescribe or (Params.Count > 0) then begin
    if IsFunc then
      DescribeParams(ReturnParam, True, ParamsInfo, pd);
    DescribeParams(ParamList, False, ParamsInfo, pd);
    Assert(Params.Count <= Length(ParamsInfo));
  end;

  if IsFunc then begin
    sCall := 'SELECT ' + SQLInfo.NormalizeName(Name, FQuoteNames) + '(';
    iMin := 1;
  end
  else
  begin
    iMin := 0;
    sCall := 'CALL ' + SQLInfo.NormalizeName(Name, FQuoteNames) + '(';
  end;

  sParams := '';
  sUserParams := '';
  sSet := '';
  sUserSet := '';
  sSelect := '';
  sCastSelect := '';
  for i := iMin to Params.Count - 1 do
  begin
    Param := Params[i];

    if i > iMin then begin
      sParams := sParams + ', ';
      sUserParams := sUserParams + ', ';
    end;

    ParamName := Param.GetName;
  {$IFDEF LITE}
    if ParamName = '' then begin
      Param.SetName(ParamsInfo[i].Name);
      ParamName := ParamsInfo[i].Name;
    end;
  {$ENDIF}
    case Param.GetParamType of
    // TODO: Dbx support (empty param names)
      pdUnknown, pdInput: begin
        sParams := sParams + '?';
        sUserParams := sUserParams + ':' + ParamName;
      end;
      pdInputOutput, pdOutput: begin
        OriginalParamName := '@' + ParamName;
        ParamName := '@' + SQLInfo.NormalizeName(ParamName, FQuoteNames);

        if Param.GetParamType = pdInputOutput then begin
          if sSet <> '' then begin
            sSet := sSet + ', ';
            sUserSet := sUserSet + ', ';
          end;
          sSet := sSet + ParamName + ' = ?';
          sUserSet := sUserSet + ParamName + ' = :' + Param.GetName;
        end;

        if sSelect = '' then
          sSelect := ParamName
        else
          sSelect := sSelect + ', ' + ParamName;

        if sCastSelect = '' then
          sCastSelect := GetSelectForParam(OriginalParamName, ParamName, ParamsInfo[i])
        else
          sCastSelect := sCastSelect + ', ' + GetSelectForParam(OriginalParamName, ParamName, ParamsInfo[i]);

        sParams := sParams + ParamName;
        sUserParams := sUserParams + ParamName;
      end;
    end;
  end;
  Result := sCall + sUserParams + ')';
  sCall := sCall + sParams + ')';

  if sSet <> '' then begin
    sCall := 'SET ' + sSet + '; ' + sCall;
    Result := 'SET ' + sUserSet + '; ' + Result;
  end;
  if sSelect <> '' then begin
    sCall := sCall + '; SELECT ' + sCastSelect;
    Result := Result + '; SELECT ' + sCastSelect;
  end;

  FSQL := sCall;
  FUserSQL := Result;
end;

procedure TMySQLCommand.Execute(Iters: integer = 1);
  procedure DoExecute;
  var
    SQL: AnsiStringBuilder;

    procedure PlaceParamValues; // Convert TParamDescs
      procedure FillCachedSQL;
      var
        Parser: TMyParser;
        s: _string;
        StPos, Len: integer;
        b: boolean;
      begin
        Parser := TMyParser.Create(FSQL);
        try
          Parser.OmitBlank := False;
          Parser.OmitComment := True;
          Parser.QuotedString := True;

          StPos := Parser.CurrPos;
          while True do begin
            b := Parser.ToLexem('?');
            if b then
              Len := Parser.CurrPos - StPos - 1
            else
              Len := Parser.CurrPos - StPos;

            s := Copy(FSQL, StPos + 1, Len);
            FCachedSQL.Add(s);

            if not b then
              Break;

            StPos := Parser.CurrPos;
          end;
        finally
          Parser.Free;
        end;
      end;

    var
      IOParamCnt: integer;

      function GetParam(const ParamIdx: integer): TParamDesc;
      var
        i, j: integer;
      begin
        if not FStoredProc then begin
          Result := FParams[ParamIdx];
          Exit;
        end;

        if IOParamCnt = -1 then begin
          IOParamCnt := 0;
          for i := 0 to FParams.Count - 1 do
            if FParams[i].GetParamType = pdInputOutput then
              Inc(IOParamCnt);
        end;

        if ParamIdx < IOParamCnt then begin // Search pdInputOutput param
          j := 0;
          for i := 0 to FParams.Count - 1 do begin
            Result := FParams[i];
            if Result.GetParamType = pdInputOutput then begin
              if j = ParamIdx then
                Exit;
              Inc(j);
            end;
          end;
        end
        else
        begin // Search pdInput param
          j := IOParamCnt;
          for i := 0 to FParams.Count - 1 do begin
            Result := FParams[i];
            if Result.GetParamType = pdInput then begin
              if j = ParamIdx then
                Exit;
              Inc(j);
            end;
          end;
        end;

        Result := nil;
        // CR 19492 Assert(False, IntToStr(ParamIdx) + #$D#$A + FSQL);
      end;

    var
      i, LenPP, Prev: integer;
      Param: TParamDesc;
    {$IFDEF HAVE_COMPRESS}
      SendBlobCompressed: boolean;
    {$ENDIF}
      ParamIdx: integer;
      u: AnsiString;
    begin
    {$IFDEF HAVE_COMPRESS}
      case FCompressBlob of
        cbServer, cbClientServer:
          SendBlobCompressed := True;
        else
          SendBlobCompressed := False;
      end;
    {$ENDIF}

      SQL.Length := 0;
      IOParamCnt := -1;
      LenPP := Length(FParamPositions);
      if LenPP = 0 then begin// dbExpress or ParamCheck = False case
        if (FCachedSQL.Count = 0) then
          FillCachedSQL;

        ParamIdx := 0;
        for i := 0 to FCachedSQL.Count - 1 do begin
          if FConnection.FUseUnicode then
            u := UTF8Encode(FCachedSQL[i])
          else
            u := AnsiString(FCachedSQL[i]);
          SQL.Append(u);
          if i <> FCachedSQL.Count - 1 then begin
            Param := GetParam(ParamIdx);
            if Param <> nil then begin
              if Param.GetParamType = pdResult then begin          // cr-mda 21751
                Inc(ParamIdx);
                Param := GetParam(ParamIdx);
              end;
              if Param <> nil then
                AppendValueToSQL(SQL, Param.GetDataType, Param.Value, Param.GetNull, FConnection.FUseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, FConnection.FNeedBackslashes, FConnection.ServerPrimaryVer, FConnection.FCharset);
            end;
          end;
          Inc(ParamIdx);
        end;
      end
      else
      begin
        Assert(FStoredProc or (LenPP = Params.Count));

        Prev := 0;
        for i := 0 to LenPP - 1 do begin
          if FConnection.FUseUnicode then begin
            u := UTF8Encode(copy(FSQL, Prev + 1, FParamPositions[i] - Prev - 1));
            SQL.Append(u);
          end
          else
            SQL.Append(AnsiString(FSQL), Prev, FParamPositions[i] - Prev - 1);
          Prev := FParamPositions[i];

          Param := GetParam(i);
          if Param <> nil then
            AppendValueToSQL(SQL, Param.GetDataType, Param.Value, Param.GetNull, FConnection.FUseUnicode{$IFDEF HAVE_COMPRESS}, SendBlobCompressed{$ENDIF}, FConnection.FNeedBackslashes, FConnection.ServerPrimaryVer, FConnection.FCharset);
        end;
        SQL.Append(AnsiString(FSQL), Prev, Length(FSQL) - Prev);
      end;
    end;

  var
    SQLResult: pMYSQL_RES;
    i, Cnt: integer;

    u: AnsiString;
    res: shortint;
    OpenNextStatus: integer;
    RowsAffectedNext: integer;

  begin
    Assert(FConnection <> nil);
    Assert(FConnection.FMySQL <> nil);

    if not FOpenNext then begin
      if (FParams.Count = 0) or (Pos('?', FSQL) = 0) then begin// query without parameters
        if Assigned(FOnExecute) then
          FOnExecute(FSQL);
        if FConnection.FUseUnicode then
          u := UTF8Encode(FSQL)
        else
          u := AnsiString(FSQL);
        Check(FConnection.FMySQLAPI.mysql_real_query(FConnection.FMySQL, u, length(u)));
      end
      else // query may contain parameters
      begin
        Cnt := 0;
        for i := 0 to FParams.Count - 1 do
          Inc(Cnt, FParams[i].GetSize);
        SQL := AnsiStringBuilder.Create(Cnt * 2 + Length(FSQL));
        try
        {$IFDEF PERF_COUNTER}
          PerfCounters[5].Start;
        {$ENDIF}
          PlaceParamValues;
        {$IFDEF PERF_COUNTER}
          PerfCounters[5].Stop;
        {$ENDIF}
          if Assigned(FOnExecute) then
            if FConnection.FUseUnicode then
              FOnExecute(UTF8Decode(SQL.ToString))
            else
              FOnExecute(_string(SQL.ToString));

          Check(FConnection.FMySQLAPI.mysql_real_query(
            FConnection.FMySQL, SQL.ToString, SQL.Length));
        finally
          SQL.Free;
        end;
      end;
    end;

    FRowsAffectedPrev := FRowsAffected;
    FRowsAffected := CalcRowsAffected;
    if FRequestResultset then begin
      if not FOpenNext then begin
        Assert(FSQLResult = nil);
        // FConnection.FMySQLAPI.mysql_store_result is too slow vs FConnection.FMySQLAPI.mysql_use_result
        FSQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
        // Check(FSQLResult) does not need because FSQLResult may be nil!
        Check;

        if FConnection.IsClient41 and (FConnection.FMySQLAPI.mysql_more_results(FConnection.FMySQL) <> 0) then begin
          OpenNextStatus := 0;
          RowsAffectedNext := FRowsAffected;
          while (FSQLResult = nil) and (OpenNextStatus = 0) do begin
            OpenNextStatus := ShortInt(FConnection.FMySQLAPI.mysql_next_result(FConnection.FMySQL));
            // OpenNextStatus
            //  0  Successful and there are more results
            // -1  Successful and there are no more results
            // >0  An error occurred
            if OpenNextStatus > 0 then begin
              FCursorState := csInactive;
              Check(OpenNextStatus); // error
            end;

            FRowsAffectedPrev := RowsAffectedNext;
            RowsAffectedNext := CalcRowsAffected;
            FSQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
            Check;
          end;
        end;
      end;
    end
    else
    begin
      SQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
    {$IFDEF HAVE_DIRECT}
      if FConnection.FMySQLAPI is TMySQLAPIDirect then
        TMySqlSession(FConnection.FMySQL).SkipPacket := False;
    {$ENDIF}
      if SQLResult <> nil then
        FConnection.FMySQLAPI.mysql_free_result(SQLResult);

      if FConnection.IsClient41 then begin
        res := ShortInt(FConnection.FMySQLAPI.mysql_next_result(FConnection.FMySQL));
        while res = 0 do begin
          SQLResult := FConnection.FMySQLAPI.mysql_use_result(FConnection.FMySQL);
          if SQLResult <> nil then
            FConnection.FMySQLAPI.mysql_free_result(SQLResult);
          res := ShortInt(FConnection.FMySQLAPI.mysql_next_result(FConnection.FMySQL));
        end;
        if res > 0 then
          Check;
      end;
    end;
  end;

  procedure DoExecutePrepared;
  begin
    CheckStmt(FConnection.FMySQLAPI._mysql_stmt_execute(Fstmt, FParams, FConnection.FUseUnicode));

    FRowsAffectedPrev := FRowsAffected;
    FRowsAffected := CalcRowsAffected;
    if FRequestResultset then begin
      FSQLResult := FConnection.FMySQLAPI.mysql_stmt_result_metadata(Fstmt);
      Check;
    end
    else
      FConnection.FMySQLAPI.mysql_stmt_free_result(Fstmt);
  end;

var
  OldCursorState: TCursorState;

begin
  if (FCursorState <> csInactive) and (FCursorState <> csPrepared) then
    Exit;

  Assert(FConnection <> nil);

  // non-SP statements: nonsense
  //     SP statements: wait for fetch
  FCanReadParams := False;
  FIsSelectParams := False;
  OldCursorState := GetCursorState;
  try
    FConnection.FMySQLAPI.SetTimeout(FConnection.FMySQL, FCommandTimeout);
  {$IFDEF HAVE_DIRECT}
    if FConnection.FMySQLAPI is TMySQLAPIDirect then
      TMySqlSession(FConnection.FMySQL).SkipPacket := FStoredProc;
  {$ENDIF}

    FExecuting := True;
    FWaitForBreak := False;
    SetCursorState(csExecuting);
    try
      if not GetPrepared then
        DoExecute
      else
        DoExecutePrepared;
      // SetCursorState(csExecuted); - setted on CalcRowsAffected
    finally
      if FSQLResult = nil then
        SetCursorState(csInactive);
      FBreakExecCS.Acquire;
      FExecuting := False;
      FBreakExecCS.Release;
    end;
  {$IFDEF AUTOTEST}
    Inc(__ServerExecuteCount);
  {$ENDIF}
  except
    if GetCursorState <> csInactive then  // ODAC: on lost connection
      SetCursorState(OldCursorState);
    if Assigned(FAfterExecute) then
      FAfterExecute(False);
    if FWaitForBreak then
      Abort
    else
      raise;
  end;

  if Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TMySQLCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TMySQLConnection(Value);
  end;
end;

procedure TMySQLCommand.SetSQL(const Value: _string);
begin
  inherited;
  if not FFillParamPosition then
    SetLength(FParamPositions, 0);
  FCachedSQL.Clear;
end;

function TMySQLCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TMySQLCommand.SetCursorState(Value: TCursorState);
begin
  FCursorState := Value;
end;

function TMySQLCommand.GetProp(Prop: integer; var Value: variant): boolean;
var
  ra: integer;
begin
  Result := True;

  case Prop of
    prRowsProcessed: begin
      if FIsSelectParams then begin
        Assert(FStoredProc);
        ra := FRowsAffectedPrev;
      end
      else
        ra := FRowsAffected;
      if ra = -1 then
        Value := 0
      else
        Value := ra;
    end;
    prCanReadParams:
      Value := FCanReadParams;
    prIsSelectParams:
      Value := FIsSelectParams;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TMySQLCommand.SetProp(Prop: integer; const Value: variant): boolean;
var
  l: integer;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommandTimeout := Value;
    prCanReadParams:
      FCanReadParams := Value;
    prIsStoredProc:
      FStoredProc := Value;
    prParamPosition: begin
      l := Length(FParamPositions);
      SetLength(FParamPositions, l + 1);
      FParamPositions[l] := Value;
    end;
    prFillParamPosition: begin
      FFillParamPosition := Value;
      if FFillParamPosition then
        SetLength(FParamPositions, 0);
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TMySQLCommand.BreakExec;
var
  Conn: TMySQLConnection;
  s: AnsiString;
begin
  Conn := nil;
  FBreakExecCS.Acquire;
  try
    if not FExecuting then
      Exit;

    FWaitForBreak := True;
    Conn := TMySQLConnection.Create;
    Conn.Assign(FConnection);
    Conn.Connect('');

    try
      s := AnsiString('KILL ' + IntToStr(FConnection.FMySQLAPI.mysql_thread_id(FConnection.MySQL)));
      Conn.FMySQLAPI.mysql_real_query(Conn.FMySQL, s, length(s)); // may return error if connection already terminated
    except
      // silent
    end;
  finally
    Conn.Free;
    FBreakExecCS.Release;
  end;
end;

class function TMySQLCommand.GetTableInfoClass: TTableInfoClass;
begin
  Result := TMyTableInfo;
end;

class function TMySQLCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TMySQLInfo;
end;

class function TMySQLCommand.GetParserClass: TSQLParserClass;
begin
  Result := TMyParser;
end;

{ TMyTableInfo }

function TMyTableInfo.GetTableNameFull: _string;
begin
  Result := FTableNameFull;
end;

procedure TMyTableInfo.SetTableNameFull(const Value: _string);
begin
  FTableNameFull := Value;
end;

{ TMySQLInfo }

function TMySQLInfo.LeftQuote: _char;
begin
  Result := _char('`');
end;

function TMySQLInfo.RightQuote: _char;
begin
  Result := _char('`');
end;

function TMySQLInfo.IsQuoted(const Value: _string): boolean;
var
  l: integer;
begin
//  Assert(Pos('.', Value) = 0, 'In func IsQuotedName delimited values not allowed'); // If fieldname contains '.'

  l := Length(Value);
  if (l <= 1) then
    Result := False
  else
    Result :=
      ((Value[1] = '`') and (Value[l] = '`')) or
      ((Value[1] = '"') and (Value[l] = '"')) or
      ((Value[1] = '''') and (Value[l] = ''''));
end;

function TMySQLInfo.Quote(const Value: _string; const LeftQ: _char; const RightQ: _char): _string;
begin
  if not IsQuoted(Value) then
    Result := _QuotedStr(Value, '`')
  else
    Result := Value;
end;

function TMySQLInfo.UnQuote(const Value: _string): _string;
begin
  if IsQuoted(Value) then
    Result := _DequotedStr(Value, Value[1])
  else
    Result := Value;
end;

procedure TMySQLInfo.SplitObjectName(const Name: _string; out DataBase: _string; out ObjName: _string);
var
  Info: TExtTableInfo;
begin
  inherited SplitObjectName(Name, Info);
  DataBase := Info.Schema;
  ObjName := Info.Table;
end;

{ TMySQLRecordset }

constructor TMySQLRecordset.Create;
begin
  inherited;
  FNullForZeroDate := True;
  FFetchRows := 25;
  FEnableBoolean := True;
  FBinaryAsString := True;
  FCreateConnection := True;
  FUseHandler := False;
end;

{ Fields }
function TMySQLRecordset.GetFieldDescType: TFieldDescClass;
begin
  Result := TMySQLFieldDesc;
end;

procedure TMySQLRecordset.ExplicitInitFields;
var
  NeedReset: boolean;
begin
  NeedReset := GetCommand.GetCursorState <= csPrepared;
  try
    inherited;
  finally
    if NeedReset then begin
      GetCommand.SetCursorState(csInactive);
      CommandType := ctUnknown;
    end;
  end;
end;

procedure TMySQLRecordset.Check(const Status: IntPtr);
begin
  Assert(FCommand.FConnection <> nil);
  if Status = nil then 
    FCommand.FConnection.MySQLError(Component);
end;

procedure TMySQLRecordset.Check(const Status: integer); 
begin
  Assert(FCommand.FConnection <> nil);
  if Status <> 0 then
    FCommand.FConnection.MySQLError(Component);
end;

procedure TMySQLRecordset.Check;
begin
  Assert(FCommand.FConnection <> nil);
  FCommand.FConnection.Check(Component);
end;

procedure TMySQLRecordset.CreateCommand;
var
  Cmd: TMySQLCommand;
begin
  Cmd := TMySQLCommand.Create;
  Cmd.FRequestResultset := True;
  SetCommand(Cmd);
end;

procedure TMySQLRecordset.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TMySQLCommand(Value);
end;

function TMySQLRecordset.GetIsClient41: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FIsClient41 := FCommand.FConnection.IsClient41;
  Result := FIsClient41;
end;

function TMySQLRecordset.GetUseUnicode: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FUseUnicode := FCommand.FConnection.FUseUnicode;
  Result := FUseUnicode;
end;

function TMySQLRecordset.GetOptimizedBigInt: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FOptimizedBigInt := FCommand.FConnection.FOptimizedBigInt;
  Result := FOptimizedBigInt;
end;

function TMySQLRecordset.GetNullForZeroDelphiDate: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FNullForZeroDelphiDate := FCommand.FConnection.FNullForZeroDelphiDate;
  Result := FNullForZeroDelphiDate;
end;

function TMySQLRecordset.GetServerPrimaryVer: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FServerPrimaryVer := FCommand.FConnection.FServerPrimaryVer;
  Result := FServerPrimaryVer;
end;

function TMySQLRecordset.GetServerMinorVer: integer;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FServerMinorVer := FCommand.FConnection.FServerMinorVer;
  Result := FServerMinorVer;
end;

function TMySQLRecordset.CanDisconnect: boolean;
begin
  Result := inherited CanDisconnect;

  Assert(FCommand <> nil);
  Assert(FCommand.FConnection <> nil);
  if Result and FUseHandler and
    ((not FCommand.FConnection.IsClient41) or (not FCommand.FConnection.IsServer41)) then // See TCustomMyTable.SeparatedHandler
    Result := False;
end;

function TMySQLRecordset.RowsReturn: boolean;
begin
  if CommandType <> ctUnknown then
    Result := inherited RowsReturn
  else                              //we need to know this info even if CommandType is not set(TCustomDADataSet.DoAfterExecute)
    Result := (FCommand.FSQLResult <> nil) or (FCommand.FOpenNext and (FSQLResultNext <> nil));
end;

procedure TMySQLRecordset.ExecCommand;
begin
  try
    FOpenNextState := osNotChecked;
    try
      if not FFetchAll and FCreateConnection then
        QuerySwapConnection;

      inherited;

      if FCommand.FOpenNext and (CommandType = ctUnknown) then begin
        FCommand.FSQLResult := FSQLResultNext;
        FCommand.FRowsAffected := FRowsAffectedNext;
        FSQLResultNext := nil;
      end
      else
        if Prepared then
          FFetchState := fsBOF;

    except
      FreeResult(False {nonsense on error processing}, True);
      ReleaseSwapConnection;
      raise;
    end;

    if FCommand.FSQLResult <> nil then begin
      CommandType := ctCursor;
      FCommand.SetCursorState(csExecuted);
    end
    else
      if not FCommand.FOpenNext then begin
        CommandType := ctStatement;
        FreeResult(True, True);
        if FSQLResultNext <> nil then begin
          FCommand.FSQLResult := FSQLResultNext;
          FCommand.FRowsAffected := FRowsAffectedNext;
          FSQLResultNext := nil;
          CommandType := ctCursor;
        end
        else
          CommandType := ctStatement;
      end;
  finally
    if CommandType <> ctCursor then
      FCommand.SetCursorState(csInactive);
  end;
end;

procedure TMySQLRecordset.Disconnect;
begin
  if not Prepared then
    FreeResult(False, True);
    
  GetIsClient41;
  GetUseUnicode;
  GetOptimizedBigInt;
  GetServerPrimaryVer;
  GetServerMinorVer;
  
  inherited;
end;

procedure TMySQLRecordset.InternalPrepare;
var
  cColumns: UINT;
begin
  if not FFetchAll and FCreateConnection then
    QuerySwapConnection;
    
  try
    inherited;

    Assert(FCommand.Fstmt <> nil);
    cColumns := FCommand.FConnection.MySQLAPI.mysql_stmt_field_count(FCommand.Fstmt);

    if cColumns > 0 then
      CommandType := ctCursor
    else
      CommandType := ctStatement;
    FCommand.FSQLResult := FCommand.FConnection.FMySQLAPI.mysql_stmt_result_metadata(FCommand.Fstmt);
  except
    InternalUnPrepare;
    raise;
  end;
end;

procedure TMySQLRecordset.InternalUnPrepare;
begin
  try
    if (FCommand.FSQLResult <> nil) and (FCommand.Fstmt <> nil {Prepared cannot be used. See TData.UnPrepare for details}) then begin
      //FCommand.FConnection.FMySQLAPI.mysql_free_result(FSQLResult);
      FCommand.FSQLResult := nil;
    end;

    inherited;
  finally
    FCommand.SetCursorState(csInactive);
    CommandType := ctUnknown;
    ReleaseSwapConnection;
  end;
end;

procedure TMySQLRecordset.QuerySwapConnection;
begin
  if FCommand.GetCursorState = csExecuted then
    Exit; // This is a duplicate call from Execute (1. - InternalExecute; 2. - Open)

  if FConnectionSwap <> nil then
    Exit;

  Assert(FConnectionSwap = nil);
  Assert(FCommand.FConnection <> nil);

  FCommand.FConnection.FMySQLAPI.SetTimeout(FCommand.FConnection.FMySQL, FCommand.FCommandTimeout);
  Check(FCommand.FConnection.FMySQLAPI.mysql_ping(FCommand.FConnection.FMySQL));

  FConnectionSwap := FCommand.FConnection;
  FCommand.FConnection := nil;

  if Assigned(FConnectionSwap.FGetMySQLConnection) then begin
    Assert(Assigned(FConnectionSwap.FReturnMySQLConnection));
    FCommand.FConnection := FConnectionSwap.FGetMySQLConnection; // pooling
  end
  else
  begin
    Assert(not Assigned(FConnectionSwap.FReturnMySQLConnection));
    FCommand.FConnection := TMySQLConnection.Create;
  {$IFDEF AUTOTEST}
    Inc(__SwapConnectionCount);
  {$ENDIF}

    FCommand.FConnection.Assign(FConnectionSwap);
  end;

  FCommand.FConnection.Connect('');
  if FConnectionSwap.GetInternalTransaction.GetInTransaction then
    FCommand.FConnection.GetInternalTransaction.StartTransaction;
  FCommand.FConnection.Additional := True;
end;

procedure TMySQLRecordset.ReleaseSwapConnection(ForbidQueries: boolean = False);
begin
  if (FConnectionSwap = nil) or (FCommand = nil) then
    Exit;

  if FCommand.FConnection <> nil then begin
    if FCommand.FConnection.GetInternalTransaction.GetInTransaction and not ForbidQueries then /// CR-DbxMda12849
      FCommand.FConnection.GetInternalTransaction.Commit;
    FCommand.FConnection.Additional := True;
    if Assigned(FConnectionSwap.FReturnMySQLConnection) then begin
      FCommand.FConnection.IsValid := not ForbidQueries; /// CR15836
      FConnectionSwap.FReturnMySQLConnection(FCommand.FConnection);
    end
    else
    begin
      FCommand.FConnection.Disconnect; // Quick destroy FSQLResult without fetching all. WAR: if fetched all records then not optimal
      FreeAndNil(FCommand.FConnection);
    {$IFDEF AUTOTEST}
      Dec(__SwapConnectionCount);
    {$ENDIF}
    end;
  end;
  FCommand.FConnection := FConnectionSwap;
  FConnectionSwap := nil;
end;

procedure TMySQLRecordset.FreeResult(const TryGetNextResult, TryToFinishFetch: boolean);
var
  LastResultsetProcessed: boolean;
  i: integer;
  s: AnsiString;
  row: PMYSQL_ROW;
  ForbidQueries: boolean; /// CR-M14748
  OpenNextStatus: integer;
begin
  {OFS('+TMySQLRecordset.FreeResult (TryGetNextResult = ' + BoolToStr(TryGetNextResult, True) + ', TryToFinishFetch = ' + BoolToStr(TryToFinishFetch, True) + ')');
  OFS('OpenNextState = ' + IntToStr(Integer(FOpenNextState)));
  OFS('CommandType = ' + IntToStr(Integer(CommandType)));
  if FCommand <> nil then
    OFS('Command.CursorState = ' + IntToStr(Integer(FCommand.FCursorState)));}
  try
    // Called with TryGetNextResult = True on
    //   - Executing command with first non-SELECT statement (INSERT;SELECT;SELECT;) - TMySQLRecordset.ExecCommand
    //   - Closing cursor - TMySQLRecordset.InternalClose
    //   - Fetch ending - TMySQLRecordset.Fetch.ProcessRow

    ForbidQueries := False;
    SetLength(FFetchBnd, 0);
    if Prepared then begin
      // skip underfetched rows
      // without KILL optimization
      if CommandType = ctCursor then begin
        // ??? FCommand.FConnection.FMySQLAPI.mysql_stmt_free_result(FCommand.Fstmt);
        i := FCommand.FConnection.FMySQLAPI.mysql_stmt_errno(FCommand.Fstmt);
        repeat
          FCommand.CheckStmt(i);

          if (FCommand.FConnection.FMySQLAPI = MyAPIClient) or
            (FCommand.FConnection.FMySQLAPI = MyAPIEmbedded) then
            i := FCommand.FConnection.FMySQLAPI.mysql_stmt_fetch(FCommand.Fstmt)
          else
            i := FCommand.FConnection.FMySQLAPI._mysql_stmt_fetch(FCommand.Fstmt, row);
        until (i = MYSQL_NO_DATA) or (i = MYSQL_DATA_TRUNCATED);
      end;

      Exit;
    end;

    LastResultsetProcessed := False;
    if (FCommand = nil) or
      (FCommand.FConnection = nil) or
      (FCommand.FConnection.FMySQLAPI = nil) or
      (FCommand.FConnection.FMySQL = nil)
    then begin
      FOpenNextState := osNoMoreResults;
      CommandType := ctUnknown;
      if FCommand <> nil then
        FCommand.FCursorState := csInactive;
    end
    else
    begin
      if FCommand.FSQLResult <> nil then begin
        Assert(FCommand <> nil);
        Assert(FCommand.FConnection <> nil);

        if FFetchAll
          or not FCreateConnection{prCreateConnection}
          // or (FConnectionSwap = nil) // Disconnected mode
          or not FIsCanFastClose then begin
          if TryToFinishFetch then begin
            if FWaitForFetchBreak then begin
              FCommand.Executing := True;
              try
                FCommand.BreakExec;
              finally
                FCommand.Executing := False;
              end;
            end;
            while FCommand.FConnection.FMySQLAPI._mysql_fetch_row(FCommand.FSQLResult) <> nil do;
          end;
        end
        else
        begin
          Assert(FConnectionSwap <> nil);

          // Quick terminate http://devart.com/forums/viewtopic.php?p=9095
          i := FFetchRows;
          while FCommand.FConnection.FMySQLAPI._mysql_fetch_row(FCommand.FSQLResult) <> nil do begin
            Dec(i);
            if i = 0 then begin
              s := AnsiString('KILL ' + IntToStr(FCommand.FConnection.FMySQLAPI.mysql_thread_id(FCommand.FConnection.MySQL)));
              FConnectionSwap.FMySQLAPI.mysql_real_query(FConnectionSwap.FMySQL, s, length(s)); // may return error if connection already terminated
              ForbidQueries := True;
              while FCommand.FConnection.FMySQLAPI._mysql_fetch_row(FCommand.FSQLResult) <> nil do;
              Break;
            end;
          end;
        end;

        if IsClient41 and
          (FCommand.FConnection.FMySQLAPI.mysql_more_results(FCommand.FConnection.MySQL) <> 0) then
          FOpenNextState := osMoreResults
        else
        begin
          FOpenNextState := osNoMoreResults;
          LastResultsetProcessed := True;
          ReleaseSwapConnection(ForbidQueries); //! Before FCommand.FConnection.FMySQLAPI.mysql_free_result to bypass mem leaks
        end;
        FCommand.FConnection.FMySQLAPI.mysql_free_result(FCommand.FSQLResult);
        FCommand.FSQLResult := nil;
      end
      else
        if (FOpenNextState <> osNoMoreResults) and ((FSQLResultNext <> nil) or (FCommand.FSQLResult = nil)) then
          FOpenNextState := osMoreResults;
    end;


    if LastResultsetProcessed or
      not TryGetNextResult or
      (FOpenNextState = osNoMoreResults) or
      not IsClient41
    then
      Exit;

    try
      FRowsAffectedNext := -2;
      while FSQLResultNext = nil do begin
        OpenNextStatus := ShortInt(FCommand.FConnection.FMySQLAPI.mysql_next_result(FCommand.FConnection.FMySQL));
        // OpenNextStatus
        //  0  Successful and there are more results
        // -1  Successful and there are no more results
        // >0  An error occurred

        if OpenNextStatus > 0 then begin
          FOpenNextState := osError;
          FCommand.FCursorState := csInactive;
          CommandType := ctUnknown;
          Check(OpenNextStatus); // error
        end
        else
        if OpenNextStatus = 0 then
          FOpenNextState := osMoreResults
        else // OpenNextStatus < 0
          FOpenNextState := osNoMoreResults;

        if FRowsAffectedNext = -2 then
          FCommand.FRowsAffectedPrev := FCommand.FRowsAffected
        else
          FCommand.FRowsAffectedPrev := FRowsAffectedNext;
        FRowsAffectedNext := FCommand.CalcRowsAffected;
        try
          FSQLResultNext := FCommand.FConnection.FMySQLAPI.mysql_use_result(FCommand.FConnection.FMySQL); // ??? check mysql_field_count
          // Check(FSQLResultNext) does not need because FSQLResultNext may be nil!
          Check;
        except
          if FSQLResultNext <> nil then begin
            FCommand.FConnection.FMySQLAPI.mysql_free_result(FSQLResultNext);
            FSQLResultNext := nil;
          end;
          raise;
        end;
        if FOpenNextState <> osMoreResults then
          Break;
      end;
    finally
      if FOpenNextState <> osMoreResults then
        ReleaseSwapConnection(ForbidQueries);
    end;
  finally
    if (FCommand <> nil)
      and ((FCommand.GetCursorState = csFetching) or (FCommand.GetCursorState = csFetchingAll)) then
      FCommand.SetCursorState(csFetched);

    {OFS('OpenNextState = ' + IntToStr(Integer(FOpenNextState)));
    OFS('CommandType = ' + IntToStr(Integer(CommandType)));
    if FCommand <> nil then
      OFS('Command.CursorState = ' + IntToStr(Integer(FCommand.FCursorState)));
    OFS('-TMySQLRecordset.FreeResult');}
  end;
end;

procedure TMySQLRecordset.DrainResults;
begin
  repeat
    FreeResult(True, True);
    if FCommand <> nil then begin
      FCommand.FSQLResult := FSQLResultNext;
      FCommand.FRowsAffected := FRowsAffectedNext;
    end;
    FSQLResultNext := nil;
  until FOpenNextState <> osMoreResults;
end;

procedure TMySQLRecordset.InternalOpen(DisableInitFields: boolean = False);
begin
  try
    inherited;
  except
    FreeResult(False {nonsense on error processing}, True);
    raise;
  end;
end;

procedure TMySQLRecordset.InternalClose;
var
  i: integer;
  FieldDesc: TMySQLFieldDesc;
begin
  if FFetchAll then
    DrainResults // OpenNext method isn't compatible with FetchAll = True.
  else
    FreeResult(True, True);
  FreeFetchBlock; // CR-M18793
  if FCommand <> nil then
    FCommand.FCursorState := csInactive;

  if not Prepared then
    CommandType := ctUnknown;

  // For refreshQuick
  for i := 0 to Fields.Count - 1 do begin
    FieldDesc := TMySQLFieldDesc(Fields[i]);
    if (FieldDesc.MySQLType = FIELD_TYPE_TIMESTAMP) and (FieldDesc.TableInfo <> nil) then
      TMyTableInfo(FieldDesc.TableInfo).FMaxTimestamp := 0;
  end;

  inherited;
end;

procedure TMySQLRecordset.InternalInitFields;
var
  Parser: TMyParser;
  ShowStatement, ExplainStatement4{CR11007}: boolean;

  function IsFlagSetted(const flags: longword; const Flag: longword): boolean;
  begin
    Result := (flags and Flag) <> 0;
  end;

  function ConvertMYSQL_FIELDToInternalFormat(const pField: TMYSQL_FIELD;
    var InternalType: word; var Fixed: boolean; var FieldLengthInChars: integer): boolean;
    
    function GetFieldLengthInChars(const LengthInBytes: longword; const CharsetNr: integer): longword;
    begin
      if not UseUnicode then
        Result := LengthInBytes
      else
        case InternalType of
          dtWideString, dtExtWideString, dtMemo, dtWideMemo:
          begin
            if ShowStatement or (CharsetNr = 63) then {binary}
              Result := LengthInBytes 
            else
              Result := LengthInBytes div MaxUTF8CharLen;
          end;
          else
            Result := LengthInBytes;
        end;
    end;

  var
    IsUnsigned: boolean;
    IsBinary: boolean;

  begin
    Fixed := False;
    Result := True;

    IsUnsigned := IsFlagSetted(pField.Flags, UNSIGNED_FLAG);
    IsBinary := IsFlagSetted(pField.Flags, BINARY_FLAG);

    // Calc InternalType
    if FFieldsAsString and
      not (pField._type in
        [FIELD_TYPE_GEOMETRY,
        FIELD_TYPE_TINY_BLOB, // TINYBLOB, TINYTEXT (may be BLOB or TEXT!)
        FIELD_TYPE_BLOB, // BLOB, TEXT
        FIELD_TYPE_MEDIUM_BLOB, // MEDIUMBLOB, MEDIUMTEXT
        FIELD_TYPE_LONG_BLOB] // LONGBLOB, LONGTEXT
      ) then
      InternalType := dtString
    else begin
      Assert(FCommand.FConnection <> nil);
      Result := ConvertMySQLTypeToInternalFormat(pField._type, pField.LengthInBytes,
        pField.Decimals, IsBinary, IsUnsigned, FEnableBoolean, OptimizedBigInt, FBinaryAsString, UseUnicode,
        FCommand.EnableBCD or FCommand.FConnection.EnableBCD,
        {$IFDEF VER6P}{$IFNDEF FPC}FCommand.EnableFMTBCD or FCommand.FConnection.EnableFMTBCD{$ELSE}False{$ENDIF}{$ELSE}False{$ENDIF},
        FCommand.FConnection.FCheckPrecision, pField.CharsetNr, InternalType, Fixed);
    end;
    Assert(Result);

    FieldLengthInChars := GetFieldLengthInChars(pField.LengthInBytes, pField.CharsetNr);

    // Correct MySQL problem with IF and LONGMEMO and LONGBLOB fields      ///CR-M20947
    if (pField._type = FIELD_TYPE_VAR_STRING) and (pField.LengthInBytes = 65535)
      and __Strings65535ToMemo and (pField.Table = '') then
      if IsBinary then
        InternalType := dtBlob
      else
        InternalType := dtMemo;

    // Correct MySQL "SHOW ..." bugs
    if ShowStatement then begin
      // Correct MySQL "SHOW PRIVILEGES" bug
      Parser.ToBegin;
      if ((LowerCase(pField.Name) = 'privilege') or (LowerCase(pField.Name) = 'context'))
        and (FieldLengthInChars < 30)
        and (Parser.ToLexem(lxSHOW) <> lcEnd) and (Parser.ToLexem(lxPRIVILEGES) <> lcEnd) then
        FieldLengthInChars := 40;

      Parser.ToBegin;
      if ((LowerCase(pField.Name) = 'state') or (LowerCase(pField.Name) = 'command'))
        and (Parser.ToLexem(lxSHOW) <> lcEnd) and (Parser.ToLexem(lxPROCESSLIST) <> lcEnd) then
        FieldLengthInChars := FieldLengthInChars * 3;

      // Correct MySQL "SHOW SLAVE STATUS" bug
      Parser.ToBegin;
      if (LowerCase(pField.Name) = 'slave_io_state')
        and (FieldLengthInChars < 80)
        and (Parser.ToLexem(lxSLAVE) <> lcEnd) and (Parser.ToLexem(lxSTATUS) <> lcEnd) then
        FieldLengthInChars := 80;

      Parser.ToBegin;
      if ((LowerCase(pField.Name) = 'slave_io_running') or (LowerCase(pField.Name) = 'slave_sql_running'))
        and (FieldLengthInChars < 3)
        and (Parser.ToLexem(lxSLAVE) <> lcEnd) and (Parser.ToLexem(lxSTATUS) <> lcEnd) then
        FieldLengthInChars := FieldLengthInChars * 3;

      Parser.ToBegin;
      if ((LowerCase(pField.Name) = 'last_error') or
          (LowerCase(pField.Name) = 'last_io_error') or
          (LowerCase(pField.Name) = 'last_sql_error'))
        and (FieldLengthInChars < 80)
        and (Parser.ToLexem(lxSHOW) <> lcEnd) and (Parser.ToLexem(lxSLAVE) <> lcEnd) and (Parser.ToLexem(lxSTATUS) <> lcEnd) then
        FieldLengthInChars := 2048;

      if ((ServerPrimaryVer = 4) and (ServerMinorVer = 0)) or
        (ServerPrimaryVer = 5) then begin
        Parser.ToBegin;
        if (LowerCase(pField.Name) = 'create table') and (Parser.ToLexem(lxCREATE) <> lcEnd) then
          FieldLengthInChars := {$IFDEF VER6P}MaxWord{$ELSE}dsMaxStringSize{$ENDIF};
      end;

      if ServerPrimaryVer = 5 then begin
        Parser.ToBegin;
        if (LowerCase(pField.Name) = 'create procedure') and (Parser.ToLexem(lxCREATE) <> lcEnd) then
          FieldLengthInChars := {$IFDEF VER6P}MaxWord{$ELSE}dsMaxStringSize{$ENDIF};

        if ServerMinorVer = 0 then begin
          Parser.ToBegin;
          if Parser.ToLexem(lxENGINES) <> lcEnd then
            FieldLengthInChars := FieldLengthInChars * 3 + 1;
        end;

        if FCommand.FConnection.ServerReleaseVer > 51 then begin
          if LowerCase(pField.Name) = 'user' then
            FieldLengthInChars := 16 //max allowed user name
          else
          if LowerCase(pField.Name) = 'host' then
            FieldLengthInChars := 60;
        end;
      end;
    end;

    if ExplainStatement4 and (LowerCase(pField.Name) = 'type') then
      FieldLengthInChars := {$IFDEF VER6P}MaxWord{$ELSE}dsMaxStringSize{$ENDIF} - 1; // to prevent range check error at Field.Size := LengthInChars + 1;

    if not FLongStrings and (FieldLengthInChars > 256)
      {$IFDEF FPC}or (FieldLengthInChars = 0{StoredFunc result}){$ENDIF}
    then
      if InternalType = dtString then
        InternalType := dtMemo
      else
      if InternalType = dtWideString then
        InternalType := dtWideMemo;

    // FlatBuffers = False support
    if (not FFlatBuffers and (FieldLengthInChars >= FlatBufferLimit))
      {$IFNDEF FPC}or (FieldLengthInChars = 0{StoredFunc result}){$ENDIF}
    then
      case InternalType of
        dtString:
          InternalType := dtExtString;
        dtWideString:
          InternalType := dtExtWideString;
        dtBytes, dtVarBytes:
          InternalType := dtExtVarBytes;
      end;

    Assert(Result);
  end;

  procedure SetFieldSizeParam(Field: TFieldDesc; const LengthInChars, Decimals: integer); // Sets Field Size, Length and Scale properties
  const
  {$IFDEF VER6P}
    SizeForUnknownSizedFields = MaxWord - 5; // For compatibility with TClientDataSet
  {$ELSE}
    SizeForUnknownSizedFields = dsMaxStringSize;
  {$ENDIF}
  begin
    case Field.DataType of
      // Integer fields
      dtBoolean:
        Field.Size := sizeof(WordBool);
      dtInt8:
        Field.Size := sizeof(word);
      dtInt16, dtUInt16:
        Field.Size := sizeof(smallint);
      dtInt32, dtUInt32:
        Field.Size := sizeof(integer);
      dtInt64:
        Field.Size := sizeof(int64);

      // Float fields
      dtFloat:
      begin
        /// ! Changes must be sync with dbxmda (getColumnScale, getColumnPrecision)
        Field.Size := sizeof(double);
        Field.Length := LengthInChars;
        Field.Scale := Decimals;
      end;
      dtBCD:
      begin
        Field.Size := sizeof(Currency);
        Field.Length := LengthInChars;
        Field.Scale := Decimals;
      end;
    {$IFDEF VER6P}
    {$IFNDEF FPC}
      dtFmtBCD:
      begin
        if LengthInChars < SizeOfTBcd then
          Field.Size := SizeOfTBcd
        else
          Field.Size := LengthInChars + 1 {#0}; // To right notation of large NUMERIC values
        Field.Length := LengthInChars;
        Field.Scale := Decimals;
      end;
    {$ENDIF}
    {$ENDIF}

      dtDateTime, dtDate, dtTime:
      begin
        Field.Size := sizeof(TDateTime);
        Field.Length := LengthInChars;
      end;
      dtBytes, dtString:
      begin
        Field.Length := LengthInChars;
        Field.Size := LengthInChars + 1;
      end;
      dtExtString:
      begin
        Field.Length := LengthInChars;
        if Field.Length = 0 then
          Field.Length := SizeForUnknownSizedFields;

        Field.Size := sizeof(pointer) {pointer to StringHeap};
      end;
      dtWideString:
      begin
        Field.Length := LengthInChars;
        Field.Size := (Field.Length + 1) * sizeof(WideChar);
      end;
      dtExtWideString:
      begin
        Field.Length := LengthInChars;
        if Field.Length = 0 then
          Field.Length := SizeForUnknownSizedFields;

        Field.Size := sizeof(pointer) {pointer to StringHeap};
      end;
      dtBlob, dtMemo, dtWideMemo:
      begin
        // Field.Length := GetFieldLength(pField);
        Field.Size := sizeof(TBlob);
      end;
      dtVarBytes:
      begin
        Field.Length := LengthInChars;
        Field.Size := sizeof(word) {Readed bytes} + Field.Length {Data};
      end;
      dtExtVarBytes:
      begin
        Field.Length := LengthInChars;
        Field.Size := sizeof(pointer) {pointer to StringHeap};
      end;
      else
        Assert(False);
    end;
  end;
  
  procedure FillTablesAliases;
  var
    SQLObjName: _string;// Table name
    StLex, Alias: _string;
    CodeLexem: integer;
    Field: TFieldDesc;
    ObjNameFull: _string;
    FieldName: _string;
    NewFieldLen: word;
    HandlerSyntax: boolean;
    i, j: integer;
    b: boolean;
  begin
    Parser.ToBegin;
    HandlerSyntax := FUseHandler and (Parser.ToLexem(lxHANDLER) <> lcEnd);
    if HandlerSyntax or ((not HandlerSyntax) and (Parser.ToLexem(lxSELECT) <> lcEnd)) then begin
      if HandlerSyntax or ((not HandlerSyntax) and (Parser.ToLexem(lxFROM) <> lcEnd)) then
        repeat
          repeat
           CodeLexem := Parser.GetNext(StLex);// Omit blank
          until CodeLexem <> lcBlank;

          // TableName
          b := True;
          SQLObjName := StLex;
          if not HandlerSyntax then
            while True do begin
              CodeLexem := Parser.GetNext(StLex);

              if (Length(StLex) > 0) and ((StLex[1] = ',') or (StLex[1] = ';')) then begin
                b := False;
                Break;
              end;

              if {(Length(StLex) > 0) and (StLex[1] in ['[', ']', '"', '.']) and} (CodeLexem <> 0) and (CodeLexem <> lcBlank) then
                SQLObjName := SQLObjName + StLex
              else
                Break;
            end;

          if b and not HandlerSyntax then begin
            // 'AS' clause
            if Parser.GetNext(Alias) = lxAS then
              Parser.GetNext(Alias)
            else
              Parser.Back;

            // Alias
            if Parser.GetNext(Alias) = lcIdent then
              Parser.GetNext(StLex)
            else begin
              Alias := '';
              Parser.Back;
            end;
          end
          else
            Alias := '';

          SQLObjName := MySQLInfo.NormalizeName(SQLObjName, False, True);

          // Remove Database name (not supported in this version of MySQL)
          ObjNameFull := SQLObjName;
          while True do begin
            i := Pos('.', SQLObjName);
            if i = 0 then
              Break;
            SQLObjName := Copy(SQLObjName, i + 1, 1000);
          end;
          Assert(SQLObjName <> '', 'TableName cannot be empty');

          if Alias = '' then begin
            i := GetSQLObjectIndex(SQLObjName);
            // i may be -1 for queries like "SELECT count(uid) FROM ALL_TYPES"
            if i <> - 1 then begin
              if not _SameText(FTablesInfo[i].TableName, FTablesInfo[i].TableAlias) and
                 _SameText(FTablesInfo[i].TableAlias, SQLObjName) then begin //IsView
                FTablesInfo[i].IsView := True;
                for j := 0 to Fields.Count - 1 do
                  if TCRFieldDesc(Fields[j]).TableInfo = FTablesInfo[i] then
                    Fields[j].ActualName := Fields[j].Name;
              end;

              FTablesInfo[i].TableAlias := '';
            end
            else
            if ServerPrimaryVer >= 5 then
              i := AddSQLObjectIfNeed(SQLObjName, True);
          end
          else begin
            i := AddSQLObjectIfNeed(Alias{MySQL specific}, False);
            FTablesInfo[i].TableName := MySQLInfo.NormalizeName(SQLObjName);
          end;
          // i may be -1 for queries like "SELECT count(uid) FROM ALL_TYPES"
          if i <> - 1 then
            FTablesInfo[i].TableNameFull := MySQLInfo.NormalizeName(ObjNameFull);
        until (StLex <> ',');
    end
    else
    begin
      NewFieldLen := 256;
      Parser.ToBegin; // Correct MySQL "SHOW COLUMNS" bug
      b := (Parser.ToLexem(lxSHOW) <> lcEnd) and (Parser.ToLexem(lxCOLUMNS) <> lcEnd);
      if not b then begin
        Parser.ToBegin; // Correct MySQL "DESC" bug
        b := Parser.ToLexem(lxDESC) <> lcEnd;
      end;

      if b then
        FieldName := 'Type';

      if b then begin
        Field := Fields.FindField(FieldName);
        if Field <> nil then begin
          Field.Length := NewFieldLen;
          if not FLongStrings then
            Field.DataType := dtMemo;
        end;
      end;
    end;
    // correct ObjNameFull
    for i := 0 to FTablesInfo.Count - 1 do
      if FTablesInfo[i].TableNameFull = '' then
        FTablesInfo[i].TableNameFull := FTablesInfo[i].TableName;
  end;

  procedure CreateFieldDescs;
    procedure FillFieldDesc(const pField: TMYSQL_FIELD; const InternalType: word; const LengthInChars: integer; Field: TMySQLFieldDesc);
    var
      Idx: integer;
      pField_Name, pField_OrgName, pField_Table, pField_OrgTable: _string;
    begin
      // SELECT FieldName FieldAlias FROM TableName TableAlias
      // pField.Name = 'FieldAlias'
      // pField.Table = 'TableAlias'

      if UseUnicode then begin
        pField_Name := UTF8Decode(pField.Name);
        pField_OrgName := UTF8Decode(pField.OrgName);
        pField_Table := UTF8Decode(pField.Table);
        pField_OrgTable := UTF8Decode(pField.OrgTable);
      end
      else begin
        pField_Name := _string(pField.Name);
        pField_OrgName := _string(pField.OrgName);
        pField_Table := _string(pField.Table);
        pField_OrgTable := _string(pField.OrgTable);
      end;

      Field.Name := pField_Name;
      // IsView := (pField.OrgTable <> '') and (UpperCase(pField.OrgTable) <> UpperCase(pField.Table));
      Field.ActualName := pField_OrgName;

      if pField_Table <> '' then begin
        Idx := AddSQLObjectIfNeed(pField_Table, False);
        if pField_OrgTable <> '' then
          FTablesInfo[Idx].TableName := MySQLInfo.NormalizeName(pField_OrgTable);
        FTablesInfo[Idx].TableAlias := MySQLInfo.NormalizeName(pField_Table); // pField^.table may contain or object name or alias
        Field.TableInfo := TablesInfo[Idx];
      end;

      // Field.TableName := pField.Table;

      if not (InternalType in [dtBlob, dtMemo, dtWideMemo]) then // Length in symbols. For UTF8 Length(WideString) always smaller then Length(UTF8String)
        Field.Length := LengthInChars;
      Field.FMySQLType := pField._type;

      Field.FIsUnsigned := IsFlagSetted(pField.Flags, UNSIGNED_FLAG);
      Field.SubDataType := dtUnknown;
      Field.DataType := InternalType;
      Field.ReadOnly := {IsFlagSetted(FieldFlags, AUTO_INCREMENT_FLAG) or} (Field.FMySQLType = FIELD_TYPE_NULL){ or (Field.TableName = '') - error on modifying SELECT CAST() ... };
      Field.IsAutoIncrement := IsFlagSetted(pField.Flags, AUTO_INCREMENT_FLAG);
      Field.Required := not Field.IsAutoIncrement and (pField._type <> MYSQL_TYPE_TIMESTAMP)
        and IsFlagSetted(pField.Flags, NOT_NULL_FLAG);
      Field.FIsBinary := IsFlagSetted(pField.Flags, BINARY_FLAG);
      Field.IsKey := IsFlagSetted(pField.Flags, PRI_KEY_FLAG + UNIQUE_KEY_FLAG + MULTIPLE_KEY_FLAG + PART_KEY_FLAG);
      Field.FIsPrimaryKey := IsFlagSetted(pField.Flags, PRI_KEY_FLAG);
      Field.FIsCurrentTimestamp := IsFlagSetted(pField.Flags, TIMESTAMP_FLAG);
      if (pField._type = MYSQL_TYPE_TIMESTAMP) and (Field.TableInfo <> nil) then
        if (TMyTableInfo(Field.TableInfo).FTimestampField = nil) or Field.FIsCurrentTimestamp then
          TMyTableInfo(Field.TableInfo).FTimestampField := Field;

      SetFieldSizeParam(Field, LengthInChars, pField.Decimals); // Sets Field Size, Length and Scale properties
    end;

  (*procedure CorrectFieldDescsTableName;
    var
      i, j: integer;
      Field: TMySQLFieldDesc;
      IsAlias: boolean;
    begin
      for i := 0 to Fields.Count - 1 do begin
        Field := TMySQLFieldDesc(Fields[i]);
        if Field.FieldDescKind = fdkData then begin
          j := GetSQLObjectIndex(Field.TableInfo.TableName, IsAlias);
          if (j <> - 1) and IsAlias then begin// Field.TableName - is alias and we need to correct it
            Field.TableName := FTablesInfo[j].TableName;
            // Field.ActualName := Field.TableName + '.' + Field.Name;
          end;
        end;
      end;
    end;*)
  var
    Field: TMySQLFieldDesc;
    pField: TMYSQL_FIELD;
    i, FieldCount: integer;
    InternalType: word;
    Fixed: boolean;
    FieldLengthInChars: integer;

  begin
    // Optimization
    if FCommand.FPrepared and (Fields.Count > 0) then
      Exit;

    Assert(FCommand.FSQLResult <> nil); // Query does not return cursor
    FieldCount := FCommand.FConnection.FMySQLAPI.mysql_num_fields(FCommand.FSQLResult);

    Assert(FTablesInfo <> nil);
    FTablesInfo.BeginUpdate;
    try
      for i := 0 to FieldCount - 1 do begin
        pField := FCommand.FConnection.FMySQLAPI._mysql_fetch_field_direct(FCommand.FSQLResult, i, UseUnicode);
        (*
        if UseUnicode then begin
          pField.Name := UTF8Decode(pField.Name);
          pField.OrgName := UTF8Decode(pField.OrgName);
          pField.Table := UTF8Decode(pField.Table);
        end;
        *)

        if ConvertMYSQL_FIELDToInternalFormat(pField, InternalType, Fixed, FieldLengthInChars) then begin
          Field := TMySQLFieldDesc.Create;
          try
            Field.FieldNo := i + 1;
            Field.ActualFieldNo := i;
            Field.Fixed := Fixed;

            FillFieldDesc(pField, InternalType, FieldLengthInChars, Field);
            FFields.Add(Field);
          except
            Field.Free;
            raise;
          end;
        end
        else
          Assert(False, 'Unknown MySQL datatype'); // MySQL field list must be equivalent to FieldDesc list
      end;
    finally
      FTablesInfo.EndUpdate;
    end;

    FillTablesAliases;

    // Set correct values for FieldDesc.TableName and correct FieldDesc.ActualName
    // Example:
    //   for 'SELECT FieldName FROM TableName' FieldDesc['FieldName'].TableName = 'TableName'
    //   for 'SELECT FieldName FROM TableName TableAlias' FieldDesc['FieldName'].TableName = 'TableAlias' (incorrect)
    // CorrectFieldDescsTableName;
    SetLength(FNulls, 0);
    // For
    //   SELECT FieldName FieldAlias FROM TableName TableAlias
    // must be
    //   Field.Name = 'FieldAlias'
    //   Field.ActualName = 'FieldAlias'
    //   Field.TableName = 'ALL_TYPES'
  end;

  procedure SetLimit;
  var
    Lexem: _string;
    s: _string;
    i: integer;
    SelectPos: integer;
  begin
    Parser.ToBegin;
    if Parser.ToLexem(lxSELECT) = lcEnd then
      Exit;

    SelectPos := Parser.CurrPos;

    Parser.ToBegin;
    if (Parser.ToLexem(lxSET) <> lcEnd) and (Parser.CurrPos < SelectPos) then
      Exit;
    Parser.ToBegin;

    if Parser.ToLexem(lxLIMIT) <> lcEnd then begin
      if (Parser.GetNext(Lexem) = lcBlank)
        and (Parser.GetNext(Lexem) = lcNumber) then begin
        s := Copy(FCommand.SQL, 1, Parser.PrevPos) + '0' + Copy(FCommand.SQL, Parser.CurrPos + 1, MaxInt);
        FCommand.SQL := s;
      end;
    end
    else
    begin
      Parser.ToBegin;
      i := Parser.ToLexem(lxPROCEDURE);
      if i = lcEnd then begin
        Parser.ToBegin;
        i := Parser.ToLexem(lxFOR);
        if i = lcEnd then begin
          Parser.ToBegin;
          i := Parser.ToLexem(lxLOCK);
        end;
      end;
      if i <> lcEnd then begin
        s := Copy(FCommand.SQL, 1, Parser.PrevPos) + 'LIMIT 0 ' + Copy(FCommand.SQL, Parser.PrevPos  + 1, MaxInt);
        FCommand.SQL := s;
      end
      else begin
        Parser.ToBegin;
        i := Parser.ToLexem(7{';'});
        if SelectPos > Parser.CurrPos then
          Exit; // MRS query

        if i <> lcEnd then begin
          s := Copy(FCommand.SQL, 1, Parser.PrevPos) + ' LIMIT 0 ' + Copy(FCommand.SQL, Parser.PrevPos  + 1, MaxInt);
          FCommand.SQL := s;
        end
        else
          FCommand.SQL := FCommand.SQL + ' LIMIT 0';
      end;
    end;
  end;
  
var
  OldSQL: _string;
  OldCachedSQL: _string; /// CR11914, 12021

begin
  Parser := TMyParser.Create(FCommand.SQL);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;

  // 'SHOW CHARSET'
  // 'SHOW ENGINES'
  // 'SHOW INNODB STATUS'
  // 'SHOW PRIVILEGES' {NonUnicode too - "Create temporary table"}
  // 'SHOW STATUS'
  // 'SHOW TABLE STATUS'
  // 'SHOW VARIABLES'
  Parser.ToBegin;
  ShowStatement := Parser.ToLexem(lxSHOW) <> lcEnd;

  ExplainStatement4 := ServerPrimaryVer <= 4;
  if ExplainStatement4 then begin
    Parser.ToBegin;
    ExplainStatement4 := Parser.ToLexem(lxEXPLAIN) <> lcEnd;
    if not ExplainStatement4 then begin
      Parser.ToBegin;
      ExplainStatement4 := Parser.ToLexem(lxDESCRIBE) <> lcEnd;
    end;
  end;

  try
    FIsCanFastClose := False;
    case CommandType of
      ctUnknown: begin // This is a FieldDefs.Update call
        try
          OldSQL := FCommand.SQL;
          OldCachedSQL := FCommand.FCachedSQL.Text;
          if not FCommand.FStoredProc then begin
            FCommand.SetProp(prDisableParamScan, True);
            try
              SetLimit;
            finally
              FCommand.SetProp(prDisableParamScan, False);
            end;
          end;

          Assert(FCommand.FSQLResult = nil, 'FCommand.FSQLResult must be nil');
          Assert(FSQLResultNext = nil, 'FSQLResultNext must be nil');
          FOpenNextState := osNotChecked;
          FCommand.Execute;

          if FCommand.FSQLResult = nil then begin // Search for first resultset
            FreeResult(True, True);
            FCommand.FSQLResult := FSQLResultNext;
            FCommand.FRowsAffected := FRowsAffectedNext;
            FSQLResultNext := nil;
          end;

          if FCommand.FSQLResult <> nil then // This is SELECT statement (statement with fields)
            CreateFieldDescs;
        finally
          DrainResults;

          FCommand.SQL := OldSQL;
          FCommand.FCachedSQL.Text := OldCachedSQL;
        end;
      end;
      ctCursor: begin
        CreateFieldDescs;

        if FConnectionSwap <> nil then begin
          Parser.ToBegin;
          if Parser.ToLexem(lxSELECT) <> lcEnd then begin
            FIsCanFastClose := not Parser.ToLexem(';');
          end;
        end;
      end;
    end;
    
  finally
    Parser.Free;
  end;
end;

function TMySQLRecordset.GetIndicatorSize: word;
begin
  Result := FieldCount * sizeof(boolean);
end;

function TMySQLRecordset.GetNull(FieldNo: word; RecBuf: IntPtr): boolean;
begin
  Result := boolean(Marshal.ReadByte(RecBuf, DataSize + (FieldNo - 1{numeration from 1}) * sizeof(boolean)));
  if Result then
    Result := GetNullByBlob(FieldNo, RecBuf);
end;

procedure TMySQLRecordset.SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean);
begin
  Marshal.WriteByte(RecBuf, DataSize + (FieldNo - 1{numeration from 1}) * sizeof(boolean), Byte(Value));
end;

procedure TMySQLRecordset.SetNulls(RecBuf: IntPtr);
begin
  Assert(Length(FNulls) = FFields.Count);
  Marshal.Copy(FNulls, 0, PtrOffset(RecBuf, DataSize), FFields.Count);
end;

procedure TMySQLRecordset.GetFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Dest: IntPtr);
var
  Blob: IntPtr;
begin
  case Field.DataType of
    dtBlob, dtMemo, dtWideMemo: begin
      Blob := Marshal.ReadIntPtr(FieldBuf);
      Marshal.WriteIntPtr(Dest, Blob);
    end;
  else
    inherited;
  end;
end;

procedure TMySQLRecordset.PutFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Source: IntPtr; IsDatabaseValue: boolean = False);
var
  Blob: IntPtr;
begin
  case Field.DataType of
    dtBlob, dtMemo, dtWideMemo: begin
      Blob := Marshal.ReadIntPtr(Source);
      Marshal.WriteIntPtr(FieldBuf, Blob);
    end;
  else
    inherited;
  end;
end;

procedure TMySQLRecordset.CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Blob: TBlob;
  FieldDesc: TFieldDesc;
begin
  inherited;

  //++ non-optimal
  if WithBlob and
    (UseUnicode
  {$IFDEF HAVE_COMPRESS}
    or (FCommand.FCompressBlob <> cbNone)
  {$ENDIF}
  ) then
    for i := 0 to FieldCount - 1 do begin
      FieldDesc := Fields[i];
      if FieldDesc.FieldDescKind <> fdkCalculated then
        case FieldDesc.DataType of
          dtBlob, dtMemo, dtWideMemo: begin
            Blob := GetObject(i + 1, RecBuf) as TBlob;
          {$IFDEF HAVE_COMPRESS}
            if FCommand.FCompressBlob <> cbNone then begin
              Blob.Free;
              Blob := TCompressedBlob.Create;
              Blob.RollbackEnabled := True;
              SetObject(i + 1, RecBuf, Blob);
            end;
          {$ENDIF}
            if FieldDesc.DataType = dtWideMemo then
              Blob.IsUnicode := UseUnicode;
          end;
        end;
    end;
end;

function TMySQLRecordset.GetCorrectedDataType(const FieldDesc: TFieldDesc): word;
begin
  Result := FieldDesc.DataType;
  Assert(FieldDesc is TMySQLFieldDesc);
  if (Result = dtFloat) and (
    (TMySQLFieldDesc(FieldDesc).MySQLType = FIELD_TYPE_DECIMAL) or
    (TMySQLFieldDesc(FieldDesc).MySQLType = FIELD_TYPE_NEWDECIMAL)
  ) and
  (ServerPrimaryVer >= 5) then
    Result := dtBCD;
end;

procedure TMySQLRecordset.AllocFetchBlock; // Bind fields
  procedure FillBindingForField(FieldDesc: TFieldDesc; var pBnd: MYSQL_BIND);
  var
    InternalType: word;
    OldFetchBlockSize: integer;
  begin
    pBnd.length := nil;
    //pBnd.is_null := @_TFieldDesc(FieldDesc).FIsNull;

    InternalType := GetCorrectedDataType(FieldDesc);

    pBnd.buffer_type := ConvertInternalTypeMySQLFormat(InternalType);
    pBnd.buffer := IntPtr(FFetchBlockSize);

    if IsNeedFetchBlock(InternalType) then begin
      OldFetchBlockSize := FFetchBlockSize;
      IncFetchBlockOffset(FFetchBlockSize, FieldDesc);
      pBnd.buffer_length := FFetchBlockSize - OldFetchBlockSize - sizeof(UInt);
    end
    else
      case InternalType of
        dtInt8, dtBoolean:
          pBnd.buffer_length := sizeof(byte);
        dtInt16:
          pBnd.buffer_length := sizeof(smallint);
        dtUInt16:
          pBnd.buffer_length := sizeof(word);
        dtInt32:
          pBnd.buffer_length := sizeof(integer);
        dtUInt32:
          pBnd.buffer_length := sizeof(longword);
        dtInt64:
          pBnd.buffer_length := sizeof(Int64);

        // Float fields
        dtFloat:
          pBnd.buffer_length := sizeof(double);
        dtBCD
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        , dtFMTBCD
      {$ENDIF}
      {$ENDIF}:
          pBnd.buffer_length := FieldDesc.Length + 1;

        dtBytes, dtVarBytes:
          pBnd.buffer_length := FieldDesc.Size;

        // String fields
        dtString:
          pBnd.buffer_length := FieldDesc.Size;

        dtExtString:
          pBnd.buffer_length := FieldDesc.Length + 1;

        dtWideString:
          pBnd.buffer_length := FieldDesc.Size * MaxUTF8CharLen;

        dtExtWideString:
          pBnd.buffer_length := (FieldDesc.Length + 1) * MaxUTF8CharLen;

        else
          Assert(False, Format('Invalid internal field type $%X (%d) for field %s', [InternalType, InternalType, FieldDesc.Name]));
      end;
  end;

var
  i: integer;
  FieldCnt: integer;
begin
  FFetchState := fsMiddle;
  // Unprepared or PreparedDirect
  if not Prepared {$IFDEF HAVE_DIRECT}or (FCommand.FConnection.FMySQLAPI = MyAPIDirect){$ENDIF} then begin
  {$IFDEF CLR}
    SetLength(FFetchBuf, RecordSize);
  {$ENDIF}
    Exit;
  end;

  // PreparedClient
  FieldCnt := FFields.Count;
  if FieldCnt <> 0 then begin
    SetLength(FFetchBnd, FieldCnt);
  {$IFDEF CLR}
    FillChar(Marshal.UnsafeAddrOfPinnedArrayElement(FFetchBnd, 0), FieldCnt * SizeOf(FFetchBnd[0]), 0);
  {$ELSE}
    FillChar(@FFetchBnd[0], FieldCnt * SizeOf(FFetchBnd[0]), 0);
  {$ENDIF}

    FFetchBlockSize := 0;
    for i := 0 to FieldCnt - 1 do
      FillBindingForField(FFields[i], FFetchBnd[i]);

  {$IFDEF CLR}
    SetLength(FFetchBlockArr, FFetchBlockSize);
    FFetchBlockGC := GCHandle.Alloc(FFetchBlock, GCHandleType.Pinned);
    FFetchBlock := Marshal.UnsafeAddrOfPinnedArrayElement(FFetchBlockArr, 0);
  {$ELSE}
    FFetchBlock := Marshal.AllocHGlobal(FFetchBlockSize);
  {$ENDIF}

    for i := 0 to FieldCnt - 1 do
      if IsNeedFetchBlock(GetCorrectedDataType(FFields[i])) then begin
        FFetchBnd[i].length := PtrOffset(FFetchBlock, NativeInt(FFetchBnd[i].buffer));
        FFetchBnd[i].buffer := PtrOffset(FFetchBlock, NativeInt(FFetchBnd[i].buffer) + sizeof(UInt));
      end;
  end;
end;

procedure TMySQLRecordset.FreeFetchBlock; // Free Bind struct
begin
  FFetchBlockSize := 0;
{$IFDEF CLR}
  SetLength(FFetchBuf, 0);
  if IntPtr(FFetchBlockGC) <> nil then begin
    FFetchBlockGC.Free;
    // FFetchBlockGC := GCHandle(nil);
  end;
  SetLength(FFetchBlockArr, 0);
{$ELSE}
  Marshal.FreeHGlobal(FFetchBlock);
{$ENDIF}
  FFetchBlock := nil;
end;

{$IFNDEF CLR}
{$IFDEF VER9P}
{$IFNDEF CPUX64}
function SignificantIntDigitsD7(const pIn: PAnsiChar; Digits: Word): Word; pascal;
asm
        PUSH    ESI
        PUSH    EBX
        MOV     ESI, pIn
        MOV     CX,Digits         // start with all digits
        CLD
@@1:    CMP     CX,0              // if 0,
        JE      @@3               // then end
        LODSB                     // load pIn BYTE into AL and
        CMP     AL,0              // if 0
        JNE     @@2
        SUB     CX,1              // then subtract 1 from CX
        CMP     CX,0              // make sure it's not 0
        JE      @@3
        SUB     CX,1              // subtract 1 again and
        JMP     @@1               // start again
@@2:    SHR     AL,4              // check last nibble
        CMP     AL,0              // if 0, then
        JNE     @@3
        SUB     CX,1              // subtract 1 more from CX
@@3:    MOV     Result,CX         // Result is CX
        POP     EBX
        POP     ESI
end;

procedure _CopyOddFractions;
asm
@@1:    CMP     CL,CH            // splitting nibbles:
        JAE     @@4
        MOV     AH,DL            // DL contains last nibble
        ADD     CL,1
        CMP     CL,CH
        JNE     @@2
        MOV     AL,0
        JMP     @@3
@@2:    LODSB
        MOV     DL,AL           // save off nibble
        SHR     AL,4            // get 1st Nibble of new byte
@@3:    AND     AH,15           // get 2nd Nibble of saved byte
        SHL     AH,4            // make 2nd Nibble of saved 1st Nibble of new
        OR      AH,AL           // make 1st Nibble of new 2nd Nibble of new
        MOV     AL,AH
        STOSB
        ADD     CL,1
        JMP     @@1
@@4:
end;

procedure _CopyRestBlank;
asm
@@1:    CMP     CL,1
        JBE     @@2
        MOV     AL,0
        STOSB
        SUB     CL,2
        JMP     @@1
@@2:
end;

procedure _CopyBytes;
asm
@@1:    CMP     CL,0                // set # of bytes to copy
        JBE     @@2                 // so just
        LODSB                       // copy all bytes
        SUB     CL,2
        STOSB                       // from in to out
        CMP     CL,0
        JLE     @@2
        JMP     @@1
@@2:
end;

procedure NormalizeFractionsD7(const pIn: PAnsiChar; InPrec, InScale, OutPrec, outScale: ShortInt; pOut: PAnsiChar); pascal;
asm
  // setup
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    ECX
        PUSH    EDX
        MOV     EDI,pOut
        MOV     ESI,pIn

        MOV     CL,OutPrec
        MOV     CH,OutScale
        CMP     CL,InPrec
        JA      @@6                // if OutPrec > InPrec ...
        CMP     CL,InPrec
        JE      @@4                // if OutPrec = InPrec, move to CheckScale
        MOV     AH,InPrec
        SUB     AH,OutPrec
        MOV     CL,InPrec
// Case where Output precision is less than input: cut it down
@@0:    CMP     AH,0
        JE      @@9
        LODSB
        CMP     AL,0
        JE      @@1
        MOV     [EDI],AL
        ADD     EDI,1
@@1:    SUB     AH,1
        CMP     AH,0
        JE      @@2
        SUB     AH,1
        SUB     CL,2
        JMP     @@0
@@2:    MOV     DL,AL              // save byte to DL: splitting required
        CMP     CH,InScale         // CH contains OutScale
        JBE     @@3
        MOV     CH,InScale         // # of digits to store =
@@3:    ADD     CH,OutPrec         // Min(InScale,OutScale) + OutPrec
        MOV     CL,0               // nothing stored yet
        CALL    _CopyOddFractions;
        MOV     CL,OutScale
        CMP     CL,InScale
        JBE     @@12
        SUB     CL,InScale
        CALL    _CopyRestBlank;
        JMP     @@12
@@4:    ADD     CL,InScale         // case where outputsize = input size, so just copy bytes
@@5:    CALL    _CopyBytes          // Otherwise, copy only Prec bytes
        CMP     CH,InScale
        JE      @@12
        MOV     CL,CH
        CALL    _CopyRestBlank
        JMP     @@12
// case where additional blank nibbles to prefixed to Fractions
@@6:    SUB     CL,InPrec
@@7:    CMP     CL,0
        JE      @@9
        SUB     CL,1
        CMP     CL,0
        JE      @@8
        MOV     AL,0                // add two blank nibbles
        STOSB
        SUB     CL,1
        JMP     @@7
@@8:    LODSB
        MOV     DL,AL                // save copy of input byte
        SHR     AL,4                 // get first nibble
        OR      AL,0
        STOSB
        MOV     AL,DL
        MOV     AH,InPrec
        MOV     OutPrec,AH
        MOV     CL,1               // 1 nibble stored already
        JMP     @@2
//      even # of fractions to copy
@@9:    MOV     CH,OutScale         // CL must be set to scale values to be copies.
        MOV     CL,InPrec
        CMP     CH,InScale
        JAE     @@10
        ADD     CL,CH
        JMP     @@11
@@10:   ADD     CL,InScale
@@11:   CALL    _CopyBytes
        MOV     CL,OutPrec
        CMP     CL,InPrec
        JBE     @@12
        SUB     CL,InPrec
        CALL    _CopyRestBlank;
@@12:   
        POP     EDX
        POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function ReverseNegativeD7(SignSpecialPlaces: Byte): Byte;
begin
  if (SignSpecialPlaces and (1 shl 7)) <> 0 then
    Result := (SignSpecialPlaces and 63)
  else
    Result := (SignSpecialPlaces and 63) or (1 shl 7);
end;

function NormalizeBcdD7(const InBcd: TBcd; var OutBcd: TBcd; const Prec, Scale: Word): Boolean;
var
  PIn, POut: PAnsiChar;
  I: Integer;
  Start, DecDigits: SmallInt;
  Negative: Boolean;
begin
  Result := True;
  if (Word(InBcd.Precision) = Prec) and (Word(InBcd.SignSpecialPlaces and 63) = Scale) then
    OutBcd := InBcd
  else
  begin
    Negative := InBcd.SignSpecialPlaces and (1 shl 7) <> 0;
    DecDigits := InBcd.SignSpecialPlaces and 63;
    OutBcd.Precision := Prec;
    OutBcd.SignSpecialPlaces := Scale;
    PIn := PAnsiChar(@InBcd.Fraction);
    POut := PAnsiChar(@OutBcd.Fraction);
    System.FillChar(POut^, SizeOf(OutBcd.Fraction), 0);
    if (Prec < Word(InBcd.Precision)) and (SignificantIntDigitsD7(pIn, Word(InBcd.Precision)) > Prec) then
      Result := False
    else
    begin
      { Precision is IntegerDigits, Scale is Decimal Digits }
      NormalizeFractionsD7(PIn, SmallInt(InBcd.Precision - DecDigits),
          DecDigits, Prec-(Scale and 63), Scale and 63, pOut);
      if Negative and (OutBcd.SignSpecialPlaces and (1 shl 7) = 0) then
        OutBcd.SignSpecialPlaces := ReverseNegativeD7(OutBcd.SignSpecialPlaces);
    end;
  end;
  { Guarantee unused Nibbles are blank }
  POut := PAnsiChar(@OutBcd.Fraction);
  Start := OutBcd.Precision div 2;
  if (OutBcd.Precision mod 2) = 1 then Inc(Start);
  for I := Start to SizeOf(OutBcd.Fraction) -1 do
    POut[I] := #0;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

function TMySQLRecordset.Fetch(FetchBack: boolean = False): boolean;
var
  pHBlock: PBlockHeader;
  RowsObtained: integer;
  API: TMySQLAPI;
  CorrectDecimalSeparator: boolean;
  UTF8Buff: TBytes;
  wsBuff: TBytes;

  // Convert Utf8 buffer to WideString buffer with or without null terminator.
  // Nearly copied from System.Utf8ToUnicode
  function Utf8ToWs(
    const Dest: TValueArr; DestIdx: Cardinal; MaxDestBytes{w/wo #0}: Cardinal;
    const Source: TValueArr; SourceIdx, SourceBytes: Cardinal;
    const AddNull: boolean): Cardinal{bytes w/wo #0};
  var
    i: Cardinal;
    c: Byte;
    wc: Cardinal;
  begin
    (*OFS('+Utf8ToWs ' +
      'DestIdx = ' + IntToStr(DestIdx) + ', MaxDestBytes = ' + IntToStr(MaxDestBytes) {+ ', Length(Dest) = ' + IntToStr(Length(Dest))} +
      ', SourceIdx = ' + IntToStr(SourceIdx) + ', SourceBytes = ' + IntToStr(SourceBytes));
  {$IFDEF CLR}
    OFS(Copy(Source, SourceIdx, SourceBytes));
  {$ELSE}
    OFS(Source + SourceIdx, SourceBytes);
  {$ENDIF}
    *)

    Assert(Source <> nil);
    Assert(Dest <> nil);
  {$IFDEF CLR}
    Assert(Integer(DestIdx + MaxDestBytes) <= Length(Dest), 'DestIdx = ' + IntToStr(DestIdx) + ', MaxDestBytes = ' + IntToStr(MaxDestBytes) + ', Length(Dest) = ' + IntToStr(Length(Dest)));
  {$ENDIF}

    Result := 0;
    i := SourceIdx;
    while i < SourceIdx + SourceBytes do
    begin
      wc := Cardinal(Source[Integer(i)]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceIdx + SourceBytes then
          raise Exception.Create('incomplete multibyte char');
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[Integer(i)]);
          Inc(i);
          if (c and $C0) <> $80 then
            raise Exception.Create('malformed trail byte or out of range char');
          if i >= SourceIdx + SourceBytes then
            raise Exception.Create('incomplete multibyte char');
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[Integer(i)]);
        Inc(i);
        if ((c and $C0) <> $80) and (c > $80) then
          raise Exception.Create('malformed trail byte');
        wc := (wc shl 6) or (c and $3F);
      end;

      // Assert(Result + 1 < MaxDestBytes, 'Result + 1 >= MaxDestBytes, Result = ' + IntToStr(Result) + ', MaxDestBytes = ' + IntToStr(MaxDestBytes));
      if not (Result + 1 < MaxDestBytes) then
        Break;
    {$IFDEF CLR}
      Dest[Integer(Result + DestIdx)] := Byte(wc);
      Dest[Integer(Result + DestIdx + 1)] := Byte(wc shr 8);
    {$ELSE}
      Cardinal(PtrOffset(Dest, DestIdx + Result)^) := wc;
    {$ENDIF}
      Inc(Result, sizeof(WideChar));
    end;

    if AddNull then begin
      // Assert(Result < MaxDestBytes, 'Result >= MaxDestBytes, Result = ' + IntToStr(Result) + ', MaxDestBytes = ' + IntToStr(MaxDestBytes));
      if Result < MaxDestBytes then
        Marshal.WriteInt16(Dest, Integer(DestIdx + Result), 0)
      else
      begin
        Result := MaxDestBytes - sizeof(WideChar);
        Marshal.WriteInt16(Dest, Integer(DestIdx + Result), 0);
      end;
      Inc(Result, sizeof(WideChar));
    end;
    //OFS('-Utf8ToWs');
  end;

  // Convert Utf8 string and add to StringHeap
  function Utf8ToStringHeap(const Source: TValueArr; Idx, Len: Cardinal): IntPtr;
  var
    wsLen: integer;
  begin
    wsLen := (Integer(Len) + 1) * sizeof(WideChar);
    if wsLen > Length(wsBuff) then
      SetLength(wsBuff, wsLen);
    Len := Utf8ToWs({$IFDEF CLR}wsBuff{$ELSE}@wsBuff[0]{$ENDIF}, 0, wsLen, Source, Idx, Len, True);

    Result := StringHeap.NewBuf(Len);
    Marshal.Copy(wsBuff, 0, Result, Len);
  end;

{$IFDEF HAVE_COMPRESS}
  procedure SetCompressed(FieldIdx: integer; Blob: TBlob);
  begin
    if not (Blob is TCompressedBlob) then
      Exit;

    try
      case FCommand.FCompressBlob of
        cbNone, cbServer:
          TCompressedBlob(Blob).Compressed := False;
        cbClient, cbClientServer:
          TCompressedBlob(Blob).Compressed := True;
      end;
    except
      Blob.Clear;
      FNulls[FieldIdx] := Byte(True);
    end;
  end;
{$ENDIF}

  function CreateBlob(Field: TFieldDesc): TBlob;
  var
    Unicode: boolean;
  begin
    Unicode := ((Field.DataType = dtMemo) or (Field.DataType = dtWideMemo)) and UseUnicode;
  {$IFDEF HAVE_COMPRESS}
    if FCommand.FCompressBlob <> cbNone then
      Result := TCompressedBlob.Create(Unicode)
    else
  {$ENDIF}
      Result := TBlob.Create(Unicode);
    Result.RollbackEnabled := True;
  end;

  function PrepareBlock(const OldpData: IntPtr): IntPtr; // if OldpData = nil then creates new block, otherwise return pointer to next item in block
    procedure InitBlock;
    var
      i, j: integer;
      Ptr: IntPtr;
      Field: TFieldDesc;
    begin
      if not HasComplexFields then
        Exit;

    // Create complex filds
      for i := 0 to FFetchRows - 1 do begin
        Ptr := PtrOffset(pHBlock, sizeof(TBlockHeader) + i * (RecordSize + sizeof(TItemHeader)) + sizeof(TItemHeader));

        for j := 0 to FieldCount - 1 do begin
          Field := Fields[j];
          if Field.FieldDescKind <> fdkCalculated then
            case Field.DataType of
              dtBlob, dtMemo, dtWideMemo, dtVariant, dtExtString, dtExtWideString, dtExtVarBytes:
                 Marshal.WriteIntPtr(Ptr, Field.Offset, nil);
            end;
        end;
      end;
    end;

    procedure ClearBlock;
    var
      i: integer;
      Free: PItemHeader;
    begin
      // Free complex filds
      Free := PtrOffset(pHBlock, sizeof(TBlockHeader));
      for i := 1 to pHBlock.ItemCount do begin
        if HasComplexFields and (Free.Flag <> flFree) then
          FreeComplexFields(PtrOffset(Free, sizeof(TItemHeader)), True);
        Free := PtrOffset(Free, sizeof(TItemHeader) + RecordSize);
      end;
    end;

  var
    NewBlock: boolean;
  begin
    if OldpData = nil then begin // Preparing block
      NewBlock := (IntPtr(BlockMan.FirstBlock) = nil) or (not FUniDirectional);
      if NewBlock then
        BlockMan.AllocBlock(pHBlock, FFetchRows)
      else begin
        pHBlock := BlockMan.FirstBlock;

        // Refresh block: drop values of blobs
        ClearBlock;
      end;
      InitBlock;
      Result := PtrOffset(pHBlock, sizeof(TBlockHeader) + sizeof(TItemHeader));
    end
    else
      Result := PtrOffset(OldpData, RecordSize + sizeof(TItemHeader));
  end;

  // For RefreshQuick
  procedure SaveMaxTimestamp(FieldDesc: TMySQLFieldDesc; const Value: TDateTime);
  begin
    if (FieldDesc.TableInfo <> nil) and (FieldDesc = TMyTableInfo(FieldDesc.TableInfo).TimestampField) then
      if TMyTableInfo(FieldDesc.TableInfo).FMaxTimestamp < Value then
        TMyTableInfo(FieldDesc.TableInfo).FMaxTimestamp := Value;
  end;

  procedure GetDataFromRow(var pData: IntPtr; const row: PMYSQL_ROW);
  var
    Field: TFieldDesc;
    FieldMySQLType: TMySqlFieldType;
    FieldIdx: integer;

    ValueBuff: TValueArr;

    // Write value to ValueBuff
  {$IFDEF CLR}
    procedure WriteByte(Value: Byte);
    begin
      ValueBuff[Field.Offset] := Value;
    end;

    procedure WriteInt16(Value: smallint);
    begin
      ValueBuff[Field.Offset] := Byte(Value);
      ValueBuff[Field.Offset + 1] := Byte(Value shr 8);
    end;

    procedure WriteInt32(Value: integer);
    begin
      ValueBuff[Field.Offset] := Byte(Value);
      ValueBuff[Field.Offset + 1] := Byte(Value shr 8);
      ValueBuff[Field.Offset + 2] := Byte(Value shr 16);
      ValueBuff[Field.Offset + 3] := Byte(Value shr 24);
    end;

    procedure WriteIntPtr(Value: IntPtr);
    begin
      ValueBuff[Field.Offset] := Byte(Integer(Value));
      ValueBuff[Field.Offset + 1] := Byte(Integer(Value) shr 8);
      ValueBuff[Field.Offset + 2] := Byte(Integer(Value) shr 16);
      ValueBuff[Field.Offset + 3] := Byte(Integer(Value) shr 24);
    end;

    procedure WriteInt64(Value: Int64);
    begin
      ValueBuff[Field.Offset] := Byte(Value);
      ValueBuff[Field.Offset + 1] := Byte(Value shr 8);
      ValueBuff[Field.Offset + 2] := Byte(Value shr 16);
      ValueBuff[Field.Offset + 3] := Byte(Value shr 24);
      ValueBuff[Field.Offset + 4] := Byte(Value shr 32);
      ValueBuff[Field.Offset + 5] := Byte(Value shr 40);
      ValueBuff[Field.Offset + 6] := Byte(Value shr 48);
      ValueBuff[Field.Offset + 7] := Byte(Value shr 56);
    end;

  {$ELSE}
    procedure WriteByte(Value: Byte);
    begin
      Marshal.WriteByte(ValueBuff, Field.Offset, Value);
    end;

    procedure WriteInt16(Value: smallint);
    begin
      Marshal.WriteInt16(ValueBuff, Field.Offset, Value);
    end;

    procedure WriteInt32(Value: integer);
    begin
      Marshal.WriteInt32(ValueBuff, Field.Offset, Value);
    end;

    procedure WriteIntPtr(Value: IntPtr);
    begin
      Marshal.WriteIntPtr(ValueBuff, Field.Offset, Value);
    end;

    procedure WriteInt64(Value: Int64);
    begin
      Marshal.WriteInt64(ValueBuff, Field.Offset, Value);
    end;

  {$ENDIF}

    function ReadMaxPossibleLen: integer;
    begin
      Result := FValueBuffLen[FieldIdx];
      if (Result > Field.Length) and (Field.Length > 0 {StoredFunc result}) and not UseUnicode then
        Result := Field.Length;
    end;

    // Must be sync with MyAccess DateTimeFromStr!!!
    function DateTimeFromStr(var Res: TDateTime): boolean;
    var
      ValueArr: TValueArr;
      ValueArrOff, Len: integer;

      function IntAt(const Off, Len: integer): integer; // return decoded integer from string
      var
        i: integer;
      begin
        Result := 0;
        for i := ValueArrOff + Off to ValueArrOff + Off + Len - 1 do
          Result := Result * 10 + (Byte(ValueArr[i]) - $30 {Ord('0')});
      end;

      function Year2: integer;
      begin
        Result := IntAt(0, 2);
        if Result >= 70 then
          Result := 1900 + Result
        else
          Result := 2000 + Result;
      end;

    const
      HoursPerDay = 24;

    var
      i: integer;
      t: TDateTime;
      s: AnsiString;
    begin
      Result := True;
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);

      case TMySQLFieldDesc(Field).MySQLType of
        FIELD_TYPE_TIMESTAMP: begin
          case Field.Length of
            19: // YYYY-MM-DD HH:MM:SS
              Result := TryEncodeDateTime(IntAt(0, 4), IntAt(5, 2), IntAt(8, 2), IntAt(11, 2), IntAt(14, 2), IntAt(17, 2), 0, Res);
            14: // YYYYMMDDHHMMSS
              Result := TryEncodeDateTime(IntAt(0, 4), IntAt(4, 2), IntAt(6, 2), IntAt(8, 2), IntAt(10, 2), IntAt(12, 2), 0, Res);
            12: // YYMMDDHHMMSS
              Result := TryEncodeDateTime(Year2, IntAt(2, 2), IntAt(4, 2), IntAt(6, 2), IntAt(8, 2), IntAt(10, 2), 0, Res);
            10: // YYMMDDHHMM
              Result := TryEncodeDateTime(Year2, IntAt(2, 2), IntAt(4, 2), IntAt(6, 2), IntAt(8, 2), 0, 0, Res);
            8:  // YYYYMMDD
              Result := TryEncodeDate(IntAt(0, 4), IntAt(4, 2), IntAt(6, 2), Res);
            6:  // YYMMDD
              Result := TryEncodeDate(Year2, IntAt(2, 2), IntAt(4, 2), Res);
            4:  // YYMM
              Result := TryEncodeDate(Year2, IntAt(2, 2), 1, Res);
            2:  // YY
              Result := TryEncodeDate(Year2, 1, 1, Res);
            else
              Assert(False, 'Invalid FIELD_TYPE_TIMESTAMP Field.Length (' + IntToStr(Field.Length) + ')');
          end;
          // For RefreshQuick
          SaveMaxTimestamp(TMySQLFieldDesc(Field), Res);
        end;
        FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE: // YYYY-MM-DD
          Result := TryEncodeDate(IntAt(0, 4), IntAt(5, 2), IntAt(8, 2), Res);
        FIELD_TYPE_TIME: // HH:MM:SS
        begin
          i := 0;
          while Byte(ValueArr[i + ValueArrOff]) <> Byte(':') do begin
            if i >= 4 then begin
               s := API._mysql_fetch_value_str(row, FieldIdx);
               DatabaseErrorFmt('Wrong time format. Field %s, value %s', [Field.Name, s]);
            end;
            Inc(i);
          end;
          Res := (IntAt(i + 1, 2) * 60 + IntAt(i + 4, 2)) / SecsPerDay; // MM:SS
          if Byte(ValueArr[ValueArrOff]) = Byte('-') then
            Res := - IntAt(1, i - 1) / HoursPerDay - Res
          else
            Res :=   IntAt(0, i)     / HoursPerDay + Res;
        end;
        FIELD_TYPE_DATETIME: // YYYY-MM-DD HH:MM:SS
        begin
          Result := TryEncodeDate(IntAt(0, 4), IntAt(5, 2), IntAt(8, 2), Res);
          if Result then begin
            Result := TryEncodeTime(IntAt(11, 2), IntAt(14, 2), IntAt(17, 2), 0, t);
            if Res >= 0 then
              Res := Res + t
            else
              Res := Res - t;
          end;
        end;
      end;
    end;

    function DateTimeFromStrPrep(var Res: TDateTime): boolean;
    var
      ValueArr: TValueArr;
      ValueArrOff, Len: integer;
      ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
    begin
      Result := True;
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);

      case FieldMySQLType of
        FIELD_TYPE_TIME: begin
          if Len = 0 then begin /// CR12073
            if FNullForZeroDate then
              Res := Byte(True)// SetNull(i + 1, pRec, True)
            else
              Res := BitConverter.DoubleToInt64Bits(ZeroDate); // PDateTime(pValue)^ := ZeroDate
            Result := True;
          end
          else
          begin
            Assert((Len = 12) or (Len = 8), 'Error DateTimeFromStrPrep function Len = ' + IntToStr(Len));
            ADay := Marshal.ReadInt32(ValueArr, ValueArrOff + 1);
            AHour := Marshal.ReadByte(ValueArr, ValueArrOff + 5);
            AMinute := Marshal.ReadByte(ValueArr, ValueArrOff + 6);
            ASecond := Marshal.ReadByte(ValueArr, ValueArrOff + 7);
            if Len = 12 then
              AMilliSecond := Marshal.ReadInt32(ValueArr, ValueArrOff + 8) div 1000
            else
              AMilliSecond := 0;
            Result := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, Res);
            if Result then
              Res := Res + ADay;
          end;
        end;
        FIELD_TYPE_DATE, FIELD_TYPE_DATETIME, FIELD_TYPE_TIMESTAMP: begin
          case Len of
            4: Result := TryEncodeDate(
              Marshal.ReadInt16(ValueArr, ValueArrOff),
              Marshal.ReadByte(ValueArr, ValueArrOff + 2),
              Marshal.ReadByte(ValueArr, ValueArrOff + 3),
              Res);
            7: Result := TryEncodeDateTime(
              Marshal.ReadInt16(ValueArr, ValueArrOff),
              Marshal.ReadByte(ValueArr, ValueArrOff + 2),
              Marshal.ReadByte(ValueArr, ValueArrOff + 3),
              Marshal.ReadByte(ValueArr, ValueArrOff + 4),
              Marshal.ReadByte(ValueArr, ValueArrOff + 5),
              Marshal.ReadByte(ValueArr, ValueArrOff + 6),
              0,
              Res);
            11: Result := TryEncodeDateTime(
              Marshal.ReadInt16(ValueArr, ValueArrOff),
              Marshal.ReadByte(ValueArr, ValueArrOff + 2),
              Marshal.ReadByte(ValueArr, ValueArrOff + 3),
              Marshal.ReadByte(ValueArr, ValueArrOff + 4),
              Marshal.ReadByte(ValueArr, ValueArrOff + 5),
              Marshal.ReadByte(ValueArr, ValueArrOff + 6),
              Marshal.ReadInt32(ValueArr, ValueArrOff + 7) div 1000,
              Res);
            0: Result := False;
            else
              Assert(False, 'FIELD_TYPE_DATETIME Len = ' + IntToStr(Len));
          end;
          // For RefreshQuick
          SaveMaxTimestamp(TMySQLFieldDesc(Field), Res);
        end;
      end;
    end;

    procedure ConvertFromBit;
    var
      ValueArr: TValueArr;
      ValueArrOff, Len: integer;
      b: boolean;
      i64: Int64;
      i: integer;

    begin
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);

      if Field.DataType = dtBoolean then begin
        b := Marshal.ReadByte(ValueArr, ValueArrOff + Len - 1) <> 0;
        WriteInt16(SmallInt(WordBool(b)));
      end
      else
      begin
        i64 := 0;
        for i := 0 to Len - 1 do
          i64 := (i64 shl 8) + Marshal.ReadByte(ValueArr, ValueArrOff + i);
        WriteInt64(i64);
      end;
    end;

    procedure ConvertToInt;
    var
      ValueArr: TValueArr;
      ValueArrOff, Len: integer;

      function PCharToInt: integer;
      var
        ValueArrOffEnd: integer;

        Sign: boolean;
        i: integer;
        b: byte;
      begin
        ValueArrOffEnd := ValueArrOff + Len;
        Result := 0;

        if Prepared or (FieldMySQLType = MYSQL_TYPE_BIT) then begin
          case Len of
            1: Result := Marshal.ReadByte(ValueArr, ValueArrOff);
            2: Result := Marshal.ReadInt16(ValueArr, ValueArrOff);
            4: Result := Marshal.ReadInt32(ValueArr, ValueArrOff);
            8: Result := Marshal.ReadInt64(ValueArr, ValueArrOff);
            else
              Assert(False);
          end;

          Exit;
        end;

        Sign := Byte(ValueArr[ValueArrOff]) = Byte('-');
        if Sign then
          i := ValueArrOff + 1
        else
          i := ValueArrOff;
        while i < ValueArrOffEnd do begin
        {$IFDEF OVERFLOWCHECKINGON}
          {$OVERFLOWCHECKS OFF}
        {$ENDIF}
          b := Byte(ValueArr[i]) - $30 {Ord('0')};
          Result := Result * 10 + b;
          Inc(i);
        {$IFDEF OVERFLOWCHECKINGON}
          {$OVERFLOWCHECKS ON}
        {$ENDIF}
        end;
        if Sign then
          Result := - Result;
      end;

      function PCharToInt64: int64;
      var
        ValueArrOffEnd: integer;

        Sign: boolean;
        i: integer;
        b: byte;
      begin
        ValueArrOffEnd := ValueArrOff + Len;
        Result := 0;

        if Prepared then begin
          case Len of
            1: Result := Marshal.ReadByte(ValueArr, ValueArrOff);
            2: Result := Marshal.ReadInt16(ValueArr, ValueArrOff);
            4: Result := Marshal.ReadInt32(ValueArr, ValueArrOff);
            8: Result := Marshal.ReadInt64(ValueArr, ValueArrOff);
            else
              Assert(False);
          end;

          Exit;
        end;

        Sign := Byte(ValueArr[ValueArrOff]) = Byte('-');
        if Sign then
          i := ValueArrOff + 1
        else
          i := ValueArrOff;
        while i < ValueArrOffEnd do begin
        {$IFDEF OVERFLOWCHECKINGON}
          {$OVERFLOWCHECKS OFF}
        {$ENDIF}
          b := Byte(ValueArr[i]) - $30 {Ord('0')};
          Result := Result * 10 + b;
          Inc(i);
        {$IFDEF OVERFLOWCHECKINGON}
          {$OVERFLOWCHECKS ON}
        {$ENDIF}
        end;

        if Sign then
        {$IFDEF VER6P}
          Result := - Result;
        {$ELSE}
          asm
            // eax = Low dword from Result
            // edx = High dword from Result
            mov eax, [ebp-$08]
            mov edx, [ebp-$04]

            xor ecx, ecx
            neg eax
            xchg edx, ecx
            sbb edx, ecx

            mov [ebp-$08], eax
            mov [ebp-$04], edx
          end;
        {$ENDIF}
      end;

    var
      w: word;
      i64: int64;

    begin
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);

      case Field.DataType of
        // Signed Integer fields
        dtInt8, dtInt16, dtInt32, dtInt64: begin
          case Field.Size of
            8: WriteInt64(PCharToInt64);
            4: WriteInt32(PCharToInt);
            2: WriteInt16(PCharToInt);
            1: WriteByte(Byte(PCharToInt));
          end;
        end;
        dtBoolean:
          WriteInt16(SmallInt(WordBool(Boolean(PCharToInt)))); // Convert to boolean is useful to bypass Delphi bug
        // UnSigned Integer fields
        dtWord, dtUInt32: begin
          case Field.Size of
            8: begin
              // Do not change! (CB5 Internal Compiler Error: C1093)
              i64 := Int64(PCharToInt64);
              WriteInt64(i64);
              // 8: WriteInt64(Int64(PCharToInt64));
            end;
            4:
              if Field.DataType = dtUInt32 then
                WriteInt32(Integer(PCharToInt64))
              else
                WriteInt32(Integer(PCharToInt));
            2: begin
              w := Word(PCharToInt);
              {if (TMySQLFieldDesc(Field).MySQLType = FIELD_TYPE_YEAR) and (Field.Length = 2) then
                if w >= 70 then
                  w := 1900 + w
                else
                  w := 2000 + w;}

              WriteInt16(smallint(w));
            end;
            1: WriteByte(Byte(PCharToInt));
          end;
        end;
        else
          Assert(False);
      end;
    end;

    procedure ConvertToIntPrep;
    var
      ValueArr: TValueArr;
      ValueArrOff, Len: integer;

      function BinToInt64: int64;
      begin
        Result := 0;
        case Len of
          1: Result := ShortInt{Convert to signed}(Marshal.ReadByte(ValueArr, ValueArrOff));
          2: Result := Marshal.ReadInt16(ValueArr, ValueArrOff);
          4: Result := Marshal.ReadInt32(ValueArr, ValueArrOff);
          8: Result := Marshal.ReadInt64(ValueArr, ValueArrOff);
          else
            Assert(False);
        end;
      end;

      function BinToUInt32: UInt32;
      begin
        Result := 0;
        case Len of
          1: Result := Marshal.ReadByte(ValueArr, ValueArrOff);
          2: Result := UInt16(Marshal.ReadInt16(ValueArr, ValueArrOff));
          4: Result := UInt32(Marshal.ReadInt32(ValueArr, ValueArrOff));
          else
            Assert(False);
        end;
      end;

    var
      i64: int64;

    begin
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);

      case Field.DataType of
        // Signed Integer fields
        dtInt8, dtInt16, dtInt32, dtInt64: begin
          case Field.Size of
            8: WriteInt64(BinToInt64);
            4: WriteInt32(BinToInt64);
            2: WriteInt16(BinToInt64);
            1: WriteByte(Byte(BinToInt64));
          end;
        end;
        dtBoolean:
          WriteInt16(SmallInt(WordBool(Boolean(BinToInt64)))); // Convert to boolean is useful to bypass Delphi bug
        // UnSigned Integer fields
        dtWord, dtUInt32: begin
          case Field.Size of
            8: begin
              // Do not change! (CB5 Internal Compiler Error: C1093)
              i64 := Int64(BinToInt64);
              WriteInt64(i64);
              // 8: WriteInt64(Int64(BinToInt64));
            end;
            4: WriteInt32(Int32(BinToUInt32));
            2: WriteInt16(Int16(BinToUInt32));
            1: WriteByte(BinToUInt32);
          end;
        end;
        else
          Assert(False);
      end;
    end;

    procedure ConvertToFloat; // Nearly copied from TMySqlBind.GetDouble
      function Power10(Exponent: int): Extended;
      begin
        Result := IntPower(10, Exponent);
      end;

      function GetIntAt(const buffer: TValueArr; off: integer; count: integer): Int32;
      var
        _end, i: integer;
        isNegative: boolean;
      begin
        _end := off + count;
        if (byte(buffer[off]) = byte('-')) then begin
          isNegative := true;
          Inc(off);
        end
        else
        begin
          isNegative := false;
          if byte(buffer[off]) = byte('+') then
            Inc(off);
        end;

        Result := 0;
        for i := off to _end - 1 do begin
          if (byte(buffer[off]) < byte('0')) or (byte(buffer[off]) > byte('9')) then
            raise EConvertError.CreateFmt(SInvalidInteger, [{$IFDEF CLR}Encoding.Default.GetString(buffer, off, count){$ELSE}Copy(buffer + off, 1, count){$ENDIF}]);
          result := result * 10 + byte(buffer[i]) - byte('0');
        end;
        if isNegative then
          Result := - Result;
      end;

    const
      partLen = 18;
      multiplerForLen1 = 1e18;

    var
      ValueArr: TValueArr;
      ValueArrOff, Len: integer;
      length, start: integer;

      end1, end2: integer;
      off, e, point: integer;
      isNegative: boolean;
      result1, result2: int64;

      d: extended;
      currVal: Byte;

    begin
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);

      if Prepared and
        ((FieldMySQLType = FIELD_TYPE_FLOAT) or (FieldMySQLType = FIELD_TYPE_DOUBLE)) then begin
        case Len of
          4: begin
          {$IFDEF CLR}
            d := BitConverter.ToSingle(ValueArr, ValueArrOff);
          {$ELSE}
            d := PSingle(@ValueArr[ValueArrOff])^;
          {$ENDIF}
            WriteInt64(BitConverter.DoubleToInt64Bits(d));
          end;
          8: WriteInt64(Marshal.ReadInt64(ValueArr, ValueArrOff));
          else
            Assert(len = 8, 'Float Len = ' + IntToStr(Len));
        end;
        Exit;
      end;

      start := ValueArrOff;

      if Len > partLen then begin
        end1 := start + partLen - 1;
        if Len > partLen * 2 then
          length := partLen * 2
        else
          length := Len;
        end2 := start + length - 1
      end
      else begin
        end1 := start + len - 1;
        end2 := end1;
        length := Len;
      end;

      if byte(ValueArr[start]) = byte('-') then
      begin
        isNegative := true;
        Inc(start);
      end
      else begin
        isNegative := false;
        if byte(ValueArr[start]) = byte('+') then
          Inc(start);
      end;

      point := end2;
      result1 := 0;

      for off := start to end1 do begin
        currVal := byte(ValueArr[off]);
        if (currVal < byte('0')) or (currVal > byte('9')) then begin
          if (point = end2) and (currVal = byte('.')) then
            point := off
          else begin
            if (currVal = byte('e')) or (currVal = byte('E')) then begin
              e := GetIntAt(ValueArr, off + 1, ValueArrOff + len - off - 1);   // getting mantissa
              if isNegative then
                if point = end2 then // if without point
                  d := - result1 * IntPower(10, e)
                else
                  d := - result1 * IntPower(10, e - (off - point - 1))
              else
                if point = end2 then // if without point
                  d := result1 * IntPower(10, e)
                else
                   d := result1 * IntPower(10, e - (off - point - 1));
              WriteInt64(BitConverter.DoubleToInt64Bits(d));
              Exit;
            end
            else
              raise EConvertError.CreateFmt(SInvalidFloat, [{$IFDEF CLR}Encoding.Default.GetString(ValueArr, start, length){$ELSE}Copy(ValueArr + start, 1, length){$ENDIF}]);
          end
        end
          else
            result1 := result1 * 10 + currVal - byte('0');
      end;

      result2 := 0;

      for off := end1 + 1 to end2 do begin
        currVal := byte(ValueArr[off]);
        if (currVal < byte('0')) or (currVal > byte('9')) then begin
          if (point = end2) and (currVal = byte('.')) then
            point := off
          else begin
            if (currVal = byte('e')) or (currVal = byte('E')) then begin
              e := GetIntAt(ValueArr, off + 1, ValueArrOff + len - off - 1);   // getting mantissa
              if isNegative then
                if point = end2 then // if without point
                  d := - (result1 * IntPower(multiplerForLen1, e) + result2 * IntPower(10, e))
                else
                  d := - (result1 * IntPower(multiplerForLen1, e - (off - point - 1)) + result2 * IntPower(10, e - (off - point - 1)))
              else
                if point = end2 then // if without point
                  d := (result1 * IntPower(multiplerForLen1, e) + result2 * IntPower(10, e))
                else
                  d := (result1 * IntPower(multiplerForLen1, e - (off - point - 1)) + result2 * IntPower(10, e - (off - point - 1)));
              WriteInt64(BitConverter.DoubleToInt64Bits(d));
              Exit;
            end
            else
              raise EConvertError.CreateFmt(SInvalidFloat, [{$IFDEF CLR}Encoding.Default.GetString(ValueArr, start, length){$ELSE}Copy(ValueArr + start, 1, length){$ENDIF}]);
          end
        end
          else
            result2 := result2 * int64(10) + currVal - byte('0');
      end;

      if end2 <> end1 then  // long float (length is more than PartLen)
        if result2 = 0 then  // finishes by 00000000
          if point <= end1 then
            d := result1 * IntPower(10, point - start - (end1- start))
          else
            if point = end2 then  // without floating point
              d := result1 * IntPower(10, length - (end1 - start))
            else // (point > end1) and (point < end2)
              d := result1 * IntPower(10, point - end1 - 1)
        else
          d := (result1 * IntPower(10, end2 - end1) + result2) * IntPower(10, - (end2 - point))
      else
          d := result1 * IntPower(10, point - end1);

      if isNegative then
        d := - d;

      WriteInt64(BitConverter.DoubleToInt64Bits(d));
    end;

    procedure ConvertToFloatPrep;
    var
      ValueArr: TValueArr;
      ValueArrOff, Len: integer;
      d: extended;

    begin
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);

      Assert(Prepared);

      case Len of
        4: begin
        {$IFDEF CLR}
          d := BitConverter.ToSingle(ValueArr, ValueArrOff);
        {$ELSE}
          d := PSingle(@ValueArr[ValueArrOff])^;
        {$ENDIF}
          WriteInt64(BitConverter.DoubleToInt64Bits(d));
        end;
        8: WriteInt64(Marshal.ReadInt64(ValueArr, ValueArrOff));
        else
          Assert(len = 8, 'Float Len = ' + IntToStr(Len));
      end;
    end;
  (*
  {$IFDEF CLR}
    procedure ConvertToFloat;
    var
      s: string;

      d: double;
      sb: StringBuilder;

    begin
      if CorrectDecimalSeparator then begin
        s := API._mysql_fetch_value_str(row, FieldIdx);
        sb := StringBuilder.Create(s);
        sb.Replace('.', DecimalSeparator);
        d := StrToFloat(sb.ToString);
      end
      else
        d := StrToFloat(API._mysql_fetch_value_str(row, FieldIdx));
      WriteInt64(BitConverter.DoubleToInt64Bits(d));
    end;

  {$ELSE}
    procedure ConvertToFloat;
    var
      ValueArr: TValueArr;
      ValueArrOff, Len: integer;

      d: Extended;
      i: integer;
    begin
      Len := ReadValueBuffLen;
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);

      if CorrectDecimalSeparator then begin
        i := ValueArrOff;
        while not IsValueFinished(ValueArr, i, ValueArrOff + Len) do begin
          if ValueArr[i] = '.' then
            ValueArr[i] := DecimalSeparator;
          Inc(i);
        end;
      end;

      TextToFloat(ValueArr + ValueArrOff, d, fvExtended);
      WriteInt64(BitConverter.DoubleToInt64Bits(d));
    end;
  {$ENDIF}
  *)

  {$IFDEF CLR}
    procedure ConvertToBCD;
    var
      s: string;

      d: double;
      sb: StringBuilder;
      i64: Int64;

    begin
      if CorrectDecimalSeparator then begin
        s := API._mysql_fetch_value_str(row, FieldIdx);
        sb := StringBuilder.Create(s);
        sb.Replace('.', DecimalSeparator);
        d := StrToFloat(sb.ToString);
      end
      else
        d := StrToFloat(API._mysql_fetch_value_str(row, FieldIdx));
      i64 := Round(d * 10000);
      WriteInt64(i64);
    end;

  {$ELSE}
    procedure ConvertToBCD;
    var
      ValueArr: TValueArr;
      ValueArrOff, ValueArrOffEnd, Len: integer;

      c: Currency;
      i: integer;
    begin
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);
      ValueArrOffEnd := ValueArrOff + Len;

      if CorrectDecimalSeparator then begin
        i := ValueArrOff;
        while i <> ValueArrOffEnd do begin
          if ValueArr[i] = '.' then
            ValueArr[i] := AnsiChar(DecimalSeparator);
          Inc(i);
        end;
      end;

      c := StrToCurr(string(Copy(ValueArr + ValueArrOff, 1, Len)));
      PCurrency(ValueBuff + Field.Offset)^ := c;
    end;
  {$ENDIF}

  {$IFDEF VER6P}
  {$IFDEF CLR}
    procedure ConvertToFmtBCD;
    var
      s: string;

      Bcd: TBcd;
      BcdOut: TBcd;
      sb: StringBuilder;
      off, I: integer;
      FieldLength: integer;
    begin
      if CorrectDecimalSeparator then begin
        s := API._mysql_fetch_value_str(row, FieldIdx);
        sb := StringBuilder.Create(s);
        sb.Replace('.', DecimalSeparator);
        Bcd := StrToBcd(sb.ToString);
      end
      else
        Bcd := StrToBcd(API._mysql_fetch_value_str(row, FieldIdx));
      FieldLength := Field.Length;
    {$IFDEF CLR}
      if FieldLength >= 61 then
        FieldLength := 60;
    {$ENDIF}
      NormalizeBcd(Bcd, BcdOut, FieldLength, Field.Scale);

      // Copied from TBcd.ToBytes
      off := Field.Offset;
      ValueBuff[off] := BcdOut.Precision;
      ValueBuff[off + 1] := BcdOut.SignSpecialPlaces;
      for I := 0 to 31 do
        ValueBuff[off + I + 2] := BcdOut.Fraction[I];
    end;

  {$ELSE}
    procedure ConvertToFmtBCD;
    var
      ValueArr: TValueArr;
      ValueArrOff, ValueArrOffEnd, Len: integer;

      Bcd: TBcd;
      i: integer;
      FieldLength, FieldScale: word;
    begin
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);
      ValueArrOffEnd := ValueArrOff + Len;

      if CorrectDecimalSeparator then begin
        i := ValueArrOff;
        while i <> ValueArrOffEnd do begin
          if ValueArr[i] = '.' then
            ValueArr[i] := AnsiChar(DecimalSeparator);
          Inc(i);
        end;
      end;

      Bcd := StrToBcd(string(Copy(ValueArr + ValueArrOff, 1, Len)));

      FieldLength := Field.Length;
      FieldScale := Field.Scale;
    {$IFDEF VER9P}
    {$IFNDEF CPUX64}
      NormalizeBcdD7(Bcd, PBcd(ValueBuff + Field.Offset)^, FieldLength, FieldScale);
    {$ELSE}
      NormalizeBcd(Bcd, PBcd(ValueBuff + Field.Offset)^, FieldLength, FieldScale);
    {$ENDIF}
    {$ELSE}
      NormalizeBcd(Bcd, PBcd(ValueBuff + Field.Offset)^, FieldLength, FieldScale);
    {$ENDIF}
    end;
  {$ENDIF}
  {$ENDIF}

    procedure ConvertToString;
    var
      Len: integer;
    begin
      Len := ReadMaxPossibleLen;
      if TMySQLFieldDesc(Field).IsBinary and not (Field.DataType in [dtString, dtExtString]) then
        API._mysql_fetch_value_to_buff(row, FieldIdx, ValueBuff, Field.Offset, Len)
      else
        API._mysql_fetch_value_to_str(row, FieldIdx, ValueBuff, Field.Offset, Len);
    end;

    procedure ConvertToExtString;
    var
      ExtStringPtr: IntPtr;
      Len: integer;
    begin
      Assert(not TMySQLFieldDesc(Field).IsBinary or (Field.DataType in [dtString, dtExtString]));
      if boolean(FNulls[Field.FieldNo - 1]) then
        WriteIntPtr(nil)
      else begin
        // For some fields which were taken as results of string functions MySQL server returns
        // data length less than real data length
        // If FlatBuffers=False than the size of allocated memory is determined for each string
        Len := FValueBuffLen[FieldIdx];

        // ExtStringPtr := StringHeap.AllocStr(ValuePtr, False {already trimmed by MySQL}, Len);
        ExtStringPtr := StringHeap.NewBuf(Len + 1);
        API._mysql_fetch_value_to_buff(row, FieldIdx, ExtStringPtr, Len);
        Marshal.WriteByte(ExtStringPtr, Len, Byte(#0));

        WriteIntPtr(ExtStringPtr);
      end;
    end;

    procedure ConvertToWideString;
    var
      Len: integer;
    begin
      Len := ReadMaxPossibleLen;
      if Len + 1 > Length(UTF8Buff) then
        SetLength(UTF8Buff, Len + 1);
      if Len > 0 then begin
        if TMySQLFieldDesc(Field).IsBinary and not (Field.DataType in [dtString, dtExtString]) then
          API._mysql_fetch_value_to_buff(row, FieldIdx, {$IFDEF CLR}UTF8Buff{$ELSE}@UTF8Buff[0]{$ENDIF}, 0, Len)
        else
          API._mysql_fetch_value_to_str(row, FieldIdx, {$IFDEF CLR}UTF8Buff{$ELSE}@UTF8Buff[0]{$ENDIF}, 0, Len);
      end;
      Utf8ToWs({$IFDEF CLR}ValueBuff{$ELSE}@ValueBuff[0]{$ENDIF}, Field.Offset, Field.Size, {$IFDEF CLR}UTF8Buff{$ELSE}@UTF8Buff[0]{$ENDIF}, 0, Len, True);
    end;

    procedure ConvertToExtWideString;
    var
      ExtStringPtr: IntPtr;
      Len: integer;
    begin
      Assert(not TMySQLFieldDesc(Field).IsBinary or (Field.DataType in [dtString, dtExtString, dtWideString, dtExtWideString]), 'Field.DataType = ' + IntToStr(Field.DataType) + #$D#$A + 'Field.Name = ' + Field.Name);
      if boolean(FNulls[Field.FieldNo - 1]) then
        WriteIntPtr(nil)
      else begin
        Len := FValueBuffLen[FieldIdx];
        if Len + 1 > Length(UTF8Buff) then
          SetLength(UTF8Buff, Len + 1);

        if Len > 0 then
          API._mysql_fetch_value_to_buff(row, FieldIdx, {$IFDEF CLR}UTF8Buff{$ELSE}@UTF8Buff[0]{$ENDIF}, 0, Len);
        ExtStringPtr := Utf8ToStringHeap({$IFDEF CLR}UTF8Buff{$ELSE}@UTF8Buff[0]{$ENDIF}, 0, Len);
        WriteIntPtr(ExtStringPtr);
      end;
    end;

    procedure ConvertToBlob;
    var
      Blob: TBlob;
      Len: integer;
      wsLen: integer;
      ValueArr: TValueArr;
      ValueArrOff: integer;
    {$IFDEF CLR}
      ValuePtr: IntPtr;
      ValueArrGC: GCHandle;
    {$ENDIF}

    begin
      Blob := CreateBlob(Field);
      Blob.RollbackEnabled := False;
      { See CR 4246
      if TMySQLFieldDesc(Field).IsBinary then
        Len := ReadValueBuffLen
      else
        Len := StrLen(ValueArr);
      }
      Len := FValueBuffLen[FieldIdx];
      ValueArr := API._mysql_fetch_value_arr(row, FieldIdx, ValueArrOff, Len);
      (*OFS('+API._mysql_fetch_value_arr');
    {$IFDEF CLR}
      OFS(Copy(ValueArr, ValueArrOff, Len));
    {$ELSE}
      OFS(ValueArr + ValueArrOff, Len);
    {$ENDIF}
      OFS('-API._mysql_fetch_value_arr');*)

      if Len > 0 then begin // IsNull already tested
        if not Blob.IsUnicode then begin
        {$IFDEF CLR}
          if Prepared then
            ValuePtr := API._mysql_stmt_fetch_value_ptr(FCommand.Fstmt, FieldIdx)
          else
            ValuePtr := API._mysql_fetch_value_ptr(FCommand.FSQLResult, row, FieldIdx);
          Blob.Write(0, Len, ValuePtr)
        {$ELSE}
          Blob.Write(0, Len, ValueArr + ValueArrOff)
        {$ENDIF}
        end
        else
        begin
          wsLen := (Len + 1) * sizeof(WideChar);
          if wsLen > Length(wsBuff) then
            SetLength(wsBuff, wsLen);

          Len := Utf8ToWs({$IFDEF CLR}wsBuff{$ELSE}@wsBuff[0]{$ENDIF}, 0, wsLen, ValueArr, ValueArrOff, Len, False);
        {$IFDEF CLR}
          ValueArrGC := GCHandle.Alloc(wsBuff, GCHandleType.Pinned);
          try
            ValuePtr := Marshal.UnsafeAddrOfPinnedArrayElement(wsBuff, 0);
            Blob.Write(0, Len, ValuePtr);
          finally
            ValueArrGC.Free;
          end;
        {$ELSE}
          Blob.Write(0, Len, @wsBuff[0]);
        {$ENDIF}
        end;
      {$IFDEF HAVE_COMPRESS}
        SetCompressed(FieldIdx, Blob);
      {$ENDIF}
        Blob.RollbackEnabled := True;
      end;

      WriteIntPtr(Blob.GCHandle);
    end;

    procedure ConvertToVarBytes;
    var
      Len: integer;
    begin
      Len := ReadMaxPossibleLen;
      API._mysql_fetch_value_to_buff(row, FieldIdx, ValueBuff, Field.Offset + sizeof(word), Len);
      WriteInt16(Len);
    end;

    procedure ConvertToExtVarBytes;
    var
      Len: integer;
      HeapBuf: IntPtr;
    begin
      if boolean(FNulls[Field.FieldNo - 1]) then
        WriteIntPtr(nil)
      else begin
        Len := FValueBuffLen[FieldIdx];
        HeapBuf := StringHeap.NewBuf(Len + sizeof(Word));
        API._mysql_fetch_value_to_buff(row, FieldIdx, PtrOffset(HeapBuf, sizeof(word)), Len);
        WriteIntPtr(HeapBuf);
        Marshal.WriteInt16(HeapBuf, SmallInt(Len));
      end;
    end;

  var
    DateValue: TDateTime;
    b: boolean;
    Blob: TBlob;

  begin
    if Prepared then
      API._mysql_stmt_fetch_lengths(row, FValueBuffLen)
    else
      API._mysql_fetch_lengths(FCommand.FSQLResult, FValueBuffLen);

    for FieldIdx := 0 to FieldCount - 1 do begin
      Field := Fields[FieldIdx];
      if Field.FieldDescKind <> fdkData then
        FNulls[FieldIdx] := Byte(True)
      else begin
        FNulls[FieldIdx] := Byte(API._mysql_fetch_value_is_null(row, FieldIdx));
        Assert(Field is TMySQLFieldDesc);
        FieldMySQLType := TMySQLFieldDesc(Field).MySQLType;
      end;
    {$IFDEF CLR}
      ValueBuff := FFetchBuf;
    {$ELSE}
      ValueBuff := pData;
      // Field.Offset
    {$ENDIF}
      if not boolean(FNulls[FieldIdx]) then begin
        case Field.DataType of
          // Signed Integer fields
          dtInt8, dtInt16, dtInt32, dtInt64,
          dtBoolean,
          // UnSigned Integer fields
          dtWord, dtUInt32:
            if FieldMySQLType = MYSQL_TYPE_BIT then
              ConvertFromBit
            else
            if Prepared then
              ConvertToIntPrep
            else
              ConvertToInt;

          // Float fields
          dtFloat:
            if Prepared and
              ((FieldMySQLType = FIELD_TYPE_FLOAT) or (FieldMySQLType = FIELD_TYPE_DOUBLE)) then
              ConvertToFloatPrep
            else
              ConvertToFloat;
          dtBCD:
            ConvertToBCD;
        {$IFDEF VER6P}
        {$IFNDEF FPC}
          dtFmtBCD:
            ConvertToFmtBCD;
        {$ENDIF}
        {$ENDIF}
          dtDateTime, dtDate, dtTime: begin
            if Prepared and (FieldMySQLType <> FIELD_TYPE_NEWDATE) then
              b := DateTimeFromStrPrep(DateValue)
            else
              b := DateTimeFromStr(DateValue);
            if b then begin
              if NullForZeroDelphiDate and (Double(DateValue) = 0) then
                FNulls[FieldIdx]:= Byte(True)// SetNull(i + 1, pRec, True)
              else
                WriteInt64(BitConverter.DoubleToInt64Bits(DateValue)); // PDateTime(pValue)^ := DateValue
            end
            else
              if FNullForZeroDate then
                FNulls[FieldIdx]:= Byte(True)// SetNull(i + 1, pRec, True)
              else
                WriteInt64(BitConverter.DoubleToInt64Bits(ZeroDate)); // PDateTime(pValue)^ := ZeroDate
          end;

          dtBytes, dtString:
            ConvertToString;
          dtExtString:
            ConvertToExtString;
          dtWideString:
            ConvertToWideString;
          dtExtWideString:
            ConvertToExtWideString;
          dtBlob, dtMemo, dtWideMemo:
            ConvertToBlob;
          dtVarBytes:
            ConvertToVarBytes;
          dtExtVarBytes:
            ConvertToExtVarBytes;
          else
            Assert(False);
        end;
      end
      else
      begin
        case Field.DataType of
          dtBlob, dtMemo, dtWideMemo: begin
            Blob := CreateBlob(Field);
            WriteIntPtr(Blob.GCHandle);
          end;
          dtExtString, dtExtWideString, dtExtVarBytes:
            WriteIntPtr(nil);
        end;
      end;
    end;
  {$IFDEF CLR}
    Marshal.Copy(ValueBuff, 0, pData, RecordSize);
  {$ENDIF}
    SetNulls(pData);
  end;

  function ProcessRow(var pData: IntPtr): boolean;
  var
    row: PMYSQL_ROW;

  begin
    row := API._mysql_fetch_row(FCommand.FSQLResult);

    Result := row <> nil;

    // Process Eof or errors
    if not Result then begin
      // Sync with InternalPrepare
      // if not FFetchAll then - now we always call FCommand.FConnection.FMySQLAPI.mysql_use_result
      Check; // For FCommand.FConnection.FMySQLAPI.mysql_use_result may be errors
      FreeResult(True, True);
      Exit;
    end;

    // Preparing BlockMan block
    pData := PrepareBlock(pData);

    // GettingData
    GetDataFromRow(pData, row);
    Inc(RowsObtained);
  end;

  function ProcessRowPreparedClient(var pData: IntPtr): boolean;
    procedure CorrectBindingForField(FieldDesc: TFieldDesc; var pBnd: MYSQL_BIND);
    begin
      Assert(pData <> nil);

      pBnd.is_null := PtrOffset(pData, DataSize + (FieldDesc.FieldNo - 1{numeration from 1}) * sizeof(boolean));

      if not IsNeedFetchBlock(GetCorrectedDataType(FieldDesc)) then begin
        pBnd.buffer := PtrOffset(pData, FieldDesc.Offset);
        pBnd.length := nil;
      end;
    end;

    procedure GetDataFromRowPrepClient;
    var
      Field: TFieldDesc;
      pValue, pFetchBlockValue, pFetchBlockLen: IntPtr;

      procedure SetZeroDate(const FieldIdx: integer);
      begin
        if FNullForZeroDate then
          SetNull(FieldIdx + 1, pData, True)
        else
          Marshal.WriteInt64(PtrOffset(pData, Fields[FieldIdx].Offset), BitConverter.DoubleToInt64Bits(ZeroDate)); // PDateTime(pValue)^ := ZeroDate
      end;

    {$IFDEF CLR}
      procedure ConvertToBCD(DataType: word);
      var
        s: string;
        Len: integer;
        b: TBytes;

        d: double;
        sb: StringBuilder;
        i64: Int64;

      begin
        Len := Marshal.ReadInt32(pFetchBlockLen);
        SetLength(b, Len);
        Marshal.Copy(pFetchBlockValue, b, 0, Len);
        s := Encoding.Default.GetString(b);

        if CorrectDecimalSeparator then begin
          sb := StringBuilder.Create(s);
          sb.Replace('.', DecimalSeparator);
          d := StrToFloat(sb.ToString);
        end
        else
          d := StrToFloat(s);
        case DataType of
          dtFloat: begin
            i64 := BitConverter.DoubleToInt64Bits(d);
            Marshal.WriteInt64(pValue, i64);
          end;
          dtBCD: begin
            i64 := Round(d * 10000);
            Marshal.WriteInt64(pValue, i64);
          end;
          else
            Assert(False);
        end;
      end;

    {$ELSE}
      procedure ConvertToBCD(DataType: word);
      var
        ValueArr: TValueArr;
        Len: integer;

        d: double;
        c: Currency;
        i: integer;
      begin
        Len := Marshal.ReadInt32(pFetchBlockLen);
        ValueArr := pFetchBlockValue;

        if CorrectDecimalSeparator then begin
          i := 0;
          while i <> Len do begin
            if ValueArr[i] = '.' then
              ValueArr[i] := AnsiChar(DecimalSeparator);
            Inc(i);
          end;
        end;

        case DataType of
          dtFloat: begin
            d := StrToFloat(string(Copy(ValueArr, 1, Len)));
            PDouble(pValue)^ := d;
          end;
          dtBCD: begin
            c := StrToCurr(string(Copy(ValueArr, 1, Len)));
            PCurrency(pValue)^ := c;
          end;
          else
            Assert(False);
        end;
      end;
    {$ENDIF}

    {$IFDEF VER6P}
    {$IFDEF CLR}
      procedure ConvertToFmtBCD;
      var
        s: string;
        Len: integer;
        b: TBytes;

        Bcd: TBcd;
        BcdOut: TBcd;
        sb: StringBuilder;
        I: integer;

      begin
        Len := Marshal.ReadInt32(pFetchBlockLen);
        SetLength(b, Len);
        Marshal.Copy(pFetchBlockValue, b, 0, Len);
        s := Encoding.Default.GetString(b);

        if CorrectDecimalSeparator then begin
          sb := StringBuilder.Create(s);
          sb.Replace('.', DecimalSeparator);
          Bcd := StrToBcd(sb.ToString);
        end
        else
          Bcd := StrToBcd(s);

        NormalizeBcd(Bcd, BcdOut, Field.Length, Field.Scale);

        // Copied from TBcd.ToBytes
        Marshal.WriteByte(pValue, 0, BcdOut.Precision);
        Marshal.WriteByte(pValue, 1, BcdOut.SignSpecialPlaces);
        for I := 0 to 31 do
          Marshal.WriteByte(pValue, I + 2, BcdOut.Fraction[I]);
      end;

    {$ELSE}
      procedure ConvertToFmtBCD;
      var
        ValueArr: TValueArr;
        Len: integer;

        Bcd: TBcd;
        i: integer;
        FieldLength, FieldScale: word;
      begin
        Len := Marshal.ReadInt32(pFetchBlockLen);
        ValueArr := pFetchBlockValue;

        if CorrectDecimalSeparator then begin
          i := 0;
          while i <> Len do begin
            if ValueArr[i] = '.' then
              ValueArr[i] := AnsiChar(DecimalSeparator);
            Inc(i);
          end;
        end;

        Bcd := StrToBcd(string(Copy(ValueArr, 1, Len)));

        FieldLength := Field.Length;
        FieldScale := Field.Scale;
      {$IFDEF VER9P}
      {$IFNDEF CPUX64}
        NormalizeBcdD7(Bcd, PBcd(pValue)^, FieldLength, FieldScale);
      {$ELSE}
        NormalizeBcd(Bcd, PBcd(pValue)^, FieldLength, FieldScale);
      {$ENDIF}
      {$ELSE}
        NormalizeBcd(Bcd, PBcd(pValue)^, FieldLength, FieldScale);
      {$ENDIF}
      end;
    {$ENDIF}
    {$ENDIF}

    var
      FieldIdx: integer;
      FetchBlockOffset: integer;
      Blob: TSharedObject;
      pc: IntPtr;
      Len, Len1: UInt;
      wsLen: integer;
      p: cardinal;
      dt: TDateTime;
    {$IFDEF CLR}
      mdt: MYSQL_TIME;
      b: TBytes;
      ValuePtr: IntPtr;
      ValueArrGC: GCHandle;
    {$ELSE}
      mdt: PMYSQL_TIME;
    {$ENDIF}
      ExtStringPtr: IntPtr;
      ExtBytesPtr: IntPtr;
      si: SmallInt;
      DataType: word;

    begin
      FetchBlockOffset := 0;
      for FieldIdx := 0 to FieldCount - 1 do begin
        Field := Fields[FieldIdx];
        pValue := PtrOffset(pData, Field.Offset);
        DataType := Field.DataType; // not corrected!
        if IsNeedFetchBlock(GetCorrectedDataType(Field)) then begin
          pFetchBlockLen := PtrOffset(FFetchBlock, FetchBlockOffset);
          pFetchBlockValue := PtrOffset(pFetchBlockLen, sizeof(UInt));

          case DataType of
            dtWideString:
              if GetNull(FieldIdx + 1, pData) then
                Marshal.WriteIntPtr(pValue, nil)
              else
              begin
                Len := Uint(Marshal.ReadInt32(pFetchBlockLen));

              {$IFDEF CLR}
                SetLength(b, (Field.Length + 1) * SizeOf(WideChar));
                Len := Utf8ToWs(b, 0, Field.Size, FFetchBlockArr, FetchBlockOffset + sizeof(UInt), Len, True);
                Marshal.Copy(b, 0, pValue, Len);
              {$ELSE}
                Utf8ToWs(pValue, 0, Field.Size, pFetchBlockValue, 0, Len, True);
              {$ENDIF}
              end;
            dtExtString, dtExtWideString: begin
              if GetNull(FieldIdx + 1, pData) then
                Marshal.WriteIntPtr(pValue, nil)
              else
              begin
                if DataType = dtExtString then
                  Marshal.WriteIntPtr(pValue, StringHeap.AllocStr(pFetchBlockValue, TrimFixedChar))
                else
                begin
                  Len := Uint(Marshal.ReadInt32(pFetchBlockLen));
                {$IFDEF CLR}
                  ExtStringPtr := Utf8ToStringHeap(FFetchBlockArr, FetchBlockOffset + sizeof(UInt), Len);
                {$ELSE}
                  ExtStringPtr := Utf8ToStringHeap(pFetchBlockValue, 0, Len);
                {$ENDIF}
                  Marshal.WriteIntPtr(pValue, ExtStringPtr);
                end;
              end;
            end;
            dtVarBytes: begin
              if GetNull(FieldIdx + 1, pData) then
                Marshal.WriteInt16(pValue, 0)
              else begin
                Len := Uint(Marshal.ReadInt32(pFetchBlockLen));
                CopyBuffer(pFetchBlockValue, PtrOffset(pValue, sizeof(word)), Len);
                Marshal.WriteInt16(pValue, SmallInt(Len));
              end;
            end;
            dtExtVarBytes: begin
              if GetNull(FieldIdx + 1, pData) then
                Marshal.WriteIntPtr(pValue, nil)
              else begin
                Len := Uint(Marshal.ReadInt32(pFetchBlockLen));
                ExtBytesPtr := StringHeap.NewBuf(Len + sizeof(Word));
                CopyBuffer(pFetchBlockValue, PtrOffset(ExtBytesPtr, sizeof(word)), Len);
                Marshal.WriteInt16(ExtBytesPtr, SmallInt(Len));
                Marshal.WriteIntPtr(pValue, ExtBytesPtr);
              end;
            end;
            dtMemo, dtWideMemo, dtBlob: begin
              Blob := CreateBlob(Field);
              if not GetNull(FieldIdx + 1, pData) then begin
                Len := Uint(Marshal.ReadInt32(pFetchBlockLen));
                if Len > 0 then begin
                  TBlob(Blob).RollbackEnabled := False;
                  pc := pFetchBlockValue;
                  p := 0;

                  while Len > 0 do begin
                    if TBlob(Blob).IsUnicode and (Integer(Len) > Length(UTF8Buff)) then
                      SetLength(UTF8Buff, Len);

                    if Len > UInt(DefaultPieceSize) then
                      Len1 := DefaultPieceSize
                    else
                      Len1 := Len;

                    if TBlob(Blob).IsUnicode then
                    {$IFDEF CLR}
                      Marshal.Copy(pc, UTF8Buff, p, Len1)
                    {$ELSE}
                      CopyBuffer(pc, @UTF8Buff[p], Len1)
                    {$ENDIF}
                    else
                      TBlob(Blob).Write(p, Len1, pc);
                    Inc(p, Len1);
                    Dec(Len, Len1);

                    if Len > 0 then
                      API._mysql_stmt_fetch_column(FCommand.Fstmt, FFetchBnd, FieldIdx, p);
                  end;

                  if TBlob(Blob).IsUnicode then begin
                    Len := Length(UTF8Buff);
                    wsLen := (Len + 1) * sizeof(WideChar);
                    if wsLen > Length(wsBuff) then
                      SetLength(wsBuff, wsLen);

                    Len := Utf8ToWs({$IFDEF CLR}wsBuff{$ELSE}@wsBuff[0]{$ENDIF}, 0, wsLen, {$IFDEF CLR}UTF8Buff{$ELSE}@UTF8Buff[0]{$ENDIF}, 0, Len, False);
                  {$IFDEF CLR}
                    ValueArrGC := GCHandle.Alloc(wsBuff, GCHandleType.Pinned);
                    try
                      ValuePtr := Marshal.UnsafeAddrOfPinnedArrayElement(wsBuff, 0);
                      TBlob(Blob).Write(0, Len, ValuePtr);
                    finally
                      ValueArrGC.Free;
                    end;
                  {$ELSE}
                    TBlob(Blob).Write(0, Len, @wsBuff[0]);
                  {$ENDIF}
                  end;

                {$IFDEF HAVE_COMPRESS}
                  SetCompressed(FieldIdx, Blob as TBlob);
                {$ENDIF}
                  TBlob(Blob).RollbackEnabled := True;
                end;
              end;
              Marshal.WriteIntPtr(pValue, Blob.GCHandle);
            end;
            dtDate, dtTime, dtDateTime:
              if not GetNull(FieldIdx + 1, pData) then begin
              {$IFDEF CLR}
                mdt := MYSQL_TIME(Marshal.PtrToStructure(pFetchBlockValue, TypeOf(MYSQL_TIME)));
              {$ELSE}
                mdt := pFetchBlockValue;
              {$ENDIF}
                case mdt.time_type of
                  MYSQL_TIMESTAMP_NONE,
                  MYSQL_TIMESTAMP_ERROR:
                    SetZeroDate(FieldIdx);
                  MYSQL_TIMESTAMP_DATE: begin
                    dt := EncodeDate(mdt.year, mdt.month, mdt.day);
                    if NullForZeroDelphiDate and (Double(dt) = 0) then
                      SetNull(FieldIdx + 1, pData, True)
                    else
                      Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(dt));
                  end;
                  MYSQL_TIMESTAMP_DATETIME: begin
                    if TryEncodeDateTime(mdt.year, mdt.month, mdt.day, mdt.hour, mdt.minute, mdt.second, mdt.second_part, dt) then begin
                      if NullForZeroDelphiDate and (Double(dt) = 0) then
                        SetNull(FieldIdx + 1, pData, True)
                      else
                        Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(dt));
                    end
                    else
                      SetZeroDate(FieldIdx);
                  end;
                  MYSQL_TIMESTAMP_TIME: begin
                    dt := EncodeTime(mdt.hour, mdt.minute, mdt.second, mdt.second_part);
                    Marshal.WriteInt64(pValue, BitConverter.DoubleToInt64Bits(dt));
                  end;
                end;
              end;
            dtFloat, dtBCD:
              if not GetNull(FieldIdx + 1, pData) then
                ConvertToBCD(DataType);
          {$IFDEF VER6P}
          {$IFNDEF FPC}
            dtFMTBCD:
              if not GetNull(FieldIdx + 1, pData) then
                ConvertToFmtBcd;
          {$ENDIF}
          {$ENDIF}
            else
              Assert(False);
          end;
          IncFetchBlockOffset(FetchBlockOffset, Field);
          Assert(FetchBlockOffset <= FFetchBlockSize);
        end
        else
        begin
          case DataType of
            dtBoolean: begin
              si := Marshal.ReadByte(pValue);
              Marshal.WriteInt16(pValue, SmallInt(WordBool(Boolean(si)))); // Convert to boolean is useful to bypass Delphi bug
            end;
            dtInt8: begin
              si := ShortInt(Marshal.ReadByte(pValue));
              Marshal.WriteInt16(pValue, si); // Convert to boolean is useful to bypass Delphi bug
            end;
          end;
        end;
      end;
    end;

  var
    i: int;
    OldpData: IntPtr;

  begin
    // Preparing BlockMan block
    OldpData := pData;
    Result := False;
    try
      pData := PrepareBlock(pData);

      for i := 0 to FFields.Count - 1 do
        CorrectBindingForField(FFields[i], FFetchBnd[i]);
      FCommand.FConnection.CheckStmt(FCommand.Fstmt, FCommand.FConnection.FMySQLAPI._mysql_stmt_bind_result(FCommand.Fstmt, FFetchBnd), Component);

      i := API.mysql_stmt_fetch(FCommand.Fstmt);
      Result := i <> MYSQL_NO_DATA;
    finally
      if not Result and (OldpData = nil) then
        pData := nil;
    end;

    // Process Eof or errors
    if not Result then begin
      FFetchState := fsEof;
      FreeResult(False {nonsense on prepared statements}, True);
      Exit;
    end
    else
      if i <> MYSQL_DATA_TRUNCATED then
        FCommand.CheckStmt(i);

    // GettingData
    GetDataFromRowPrepClient;
    Inc(RowsObtained);
  end;

  function ProcessRowPreparedClientDirect(var pData: IntPtr): boolean;
  var
    i: int;
    row: PMYSQL_ROW;

  begin
    FCommand.FConnection.CheckStmt(FCommand.Fstmt, FCommand.FConnection.FMySQLAPI._mysql_stmt_bind_result(FCommand.Fstmt, FFetchBnd), Component);

    i := API._mysql_stmt_fetch(FCommand.Fstmt, row);
    Result := (i <> MYSQL_NO_DATA) and (i <> MYSQL_DATA_TRUNCATED);

    // Process Eof or errors
    if not Result then begin
      FFetchState := fsEof;
      FreeResult(False {nonsense on prepared statements}, True);
      Exit;
    end
    else
      FCommand.CheckStmt(i);

    // Preparing BlockMan block
    pData := PrepareBlock(pData);

    // GettingData
    GetDataFromRow(pData, row);
    Inc(RowsObtained);
  end;

  procedure ReadParams;
  var
    i: integer;
    Param: TParamDesc;
    RecBuf, p: IntPtr;
    Field: TFieldDesc;
    Value: Variant;
    Blob: TSharedObject;
    Data: TBytes;
  begin
    AllocRecBuf(IntPtr(RecBuf));
    try
      BlockMan.GetRecord(FirstItem, RecBuf);
      // GetNextRecord(RecBuf);

      for i := 0 to FCommand.Params.Count - 1 do begin
        Param := FCommand.Params[i];
        if not IsLargeDataTypeUsed(Param) then
          case Param.GetParamType of
            pdResult: begin
              GetFieldAsVariant(1, RecBuf, Value);
              Param.Value := Value;
            end;
            pdInputOutput, pdOutput: begin
              Field := FindField('@' + Param.GetName);
              if (Field <> nil) and not IsLargeDataTypeUsed(Param) then begin
                GetFieldAsVariant(Field.FieldNo, RecBuf, Value);
                Param.Value := Value;
              end;
            end;
          end
        else
        if (Param.GetParamType in [pdResult, pdInputOutput, pdOutput]) then begin
          if Param.GetParamType = pdResult then
            Field := Fields[0]
          else
            Field := FindField('@' + Param.GetName);
          case Field.DataType of
            dtBlob, dtMemo, dtWideMemo: begin
              Blob := GetObject(Field.FieldNo, RecBuf);
              TBlob(Param.GetObject).Assign(TBlob(Blob));
            end;
            dtExtString: begin
              p := Marshal.ReadIntPtr(RecBuf, Field.Offset);
              if Param.GetDataType in [dtBlob, dtMemo, dtWideMemo] then begin
                TBlob(Param.GetObject).Clear;
                TBlob(Param.GetObject).AsAnsiString := Marshal.PtrToStringAnsi(p)
              end
              else
                Param.Value := Marshal.PtrToStringAnsi(p);
            end;
            dtExtWideString: begin
              p := Marshal.ReadIntPtr(RecBuf, Field.Offset);
              if Param.GetDataType in [dtBlob, dtMemo, dtWideMemo] then begin
                TBlob(Param.GetObject).Clear;
                TBlob(Param.GetObject).AsWideString := Marshal.PtrToStringUni(p)
              end
              else
                Param.Value := Marshal.PtrToStringUni(p);
            end;
            dtExtVarBytes: begin
              p := Marshal.ReadIntPtr(RecBuf, Field.Offset);
              if Param.GetDataType in [dtBlob, dtMemo, dtWideMemo] then begin
                TBlob(Param.GetObject).Clear;
                TBlob(Param.GetObject).Write(0, word(Marshal.ReadInt16(p)), PtrOffset(p, SizeOf(word)));
              end
              else begin
                SetLength(Data, word(Marshal.ReadInt16(p)));
                Marshal.Copy(PtrOffset(p, SizeOf(word)), Data, 0, Length(Data));
                Param.Value := Data;
              end;
            end;
          else
            raise Exception.Create(SDataTypeNotSupported);
          end;

        end;
      end;
    finally
      FreeRecBuf(RecBuf);
    end;
    FCommand.FCanReadParams := True;
    FCommand.FIsSelectParams := True;
    if Assigned(FCommand.ReadParams) then
      FCommand.ReadParams;
  end;

var
  i: integer;

  pData: IntPtr;
  b, Cancel: boolean;

begin
  if Fields.Count = 0 then begin
    FreeResult(False, True);
    DatabaseError(SNoResultSet, nil);
  end
  else
    if Length(FNulls) = 0 then
      SetLength(FNulls, Fields.Count);

  Result := False;
  if FCommand.GetCursorState in [csFetched, csInactive] then
    Exit;

  if Prepared then
    Result := FFetchState <> fsEof
  else
    Result := FCommand.FSQLResult <> nil;
  if not Result then
    Exit;

  Assert(FCommand.GetCursorState >= csExecuted);
  if FCommand.GetCursorState = csExecuted then
    FCommand.SetCursorState(csFetching);

  DoBeforeFetch(Cancel);
  if Cancel then begin
    Result := False;
    Exit;
  end;

  try
    pHBlock := nil;
    pData := nil;
    RowsObtained := 0;

    try
      if (not Prepared) or (Prepared and (FFetchState = fsBOF)) then
        AllocFetchBlock;

      CorrectDecimalSeparator := DecimalSeparator <> '.';

      API := FCommand.FConnection.FMySQLAPI; // For performance reason
      for i := 0 to FFetchRows - 1 do begin
        if not Prepared then
          b := ProcessRow(pData)
        else
        {$IFDEF HAVE_DIRECT}
          if FCommand.FConnection.FMySQLAPI = MyAPIDirect then
            b := ProcessRowPreparedClientDirect(pData)
          else
        {$ENDIF}
            b := ProcessRowPreparedClient(pData);

        if not b then
          Break;
      end;

      Result := pData <> nil;

      if IntPtr(pHBlock) <> nil then begin
        if Result then
          CreateBlockStruct(pHBlock, RowsObtained, FetchBack)
        else
          BlockMan.FreeBlock(pHBlock);
      end;

      if Result and FCommand.FStoredProc and (RecordCount = 1) and (FCommand.Params.Count > 0) then
        ReadParams;

      if not Result then
        FreeFetchBlock;

    except
      FreeFetchBlock;
      FreeResult(False {nonsense on error processing}, False);
      FEOF := True;
      raise;
    end;

  finally
    DoAfterFetch;
  end;
end;

procedure TMySQLRecordset.SetToEnd;
begin
  FetchAll;
  inherited;
end;

procedure TMySQLRecordset.BreakFetch;
begin
  inherited;

  if (FCommand <> nil)
    and ((FCommand.GetCursorState = csFetching) or (FCommand.GetCursorState = csFetchingAll)) then
    FCommand.SetCursorState(csFetched);
end;

function TMySQLRecordset.GetSQLObjectIndex(const ObjName: _string; out IsAlias: boolean): integer; // MySQL specific - Search by name and by alias!!!
var
  TableName: _string;
  DataBase, Name: _string;
begin
  Result := - 1;
  if FCommand.FConnection = nil then
    Exit;

  MySQLInfo.SplitObjectName(ObjName, DataBase, Name);
  Name := MySQLInfo.NormalizeName(Name, False, True);
  DataBase := MySQLInfo.NormalizeName(DataBase, False, True);
  TableName := GenerateTableName(DataBase, Name, FCommand.FConnection.FDatabase);

  Result := FTablesInfo.IndexByName(TableName);
  if Result <> - 1 then
    IsAlias := False;

  if Result = - 1 then begin
    Result := FTablesInfo.IndexByAlias(TableName);
    if Result <> - 1 then
      IsAlias := True;
  end;
end;

function TMySQLRecordset.GetSQLObjectIndex(const ObjName: _string): integer; // MySQL specific - Search by name and by alias!!!
var
  IsAlias: boolean;  
begin
  Result := GetSQLObjectIndex(ObjName, IsAlias);
end;

function TMySQLRecordset.AddSQLObjectIfNeed(const SQLObjName: _string; const IsView: boolean): integer; // Add new SQLObject, if need. If SQLObject already present then return its index
var
  TableInfo: TMyTableInfo;
  SQLObjectName: _string;
begin
  Assert(FTablesInfo <> nil);
  SQLObjectName := MySQLInfo.NormalizeName(SQLObjName);
  Result := GetSQLObjectIndex(SQLObjectName);
  if Result = - 1 then begin
    TableInfo := FTablesInfo.Add as TMyTableInfo;
    Result := FTablesInfo.IndexOf(TableInfo);
    Assert(Result <> -1);

    TableInfo.TableName := SQLObjectName;
    TableInfo.IsView := IsView;
  end;
end;

function TMySQLRecordset.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prIsCanOpenNext:
      Value := FOpenNextState = osMoreResults;
    prOpenNext:
      Value := FCommand.FOpenNext;
    prRowsFetched:
      Value := FRecordCount; // TODO:
    prEnableBoolean:
      Value := FEnableBoolean;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TMySQLRecordset.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prNullForZeroDate:
      FNullForZeroDate := Value;
    prFieldsAsString:
      FFieldsAsString := Value;
    prEnableBoolean:
      FEnableBoolean := Value;
    prBinaryAsString:
      FBinaryAsString := Value;
    prCreateConnection:
      FCreateConnection := Value;
    prCommandTimeout:
    begin
      Assert(FCommand <> nil);
      FCommand.FCommandTimeout := Value;
    end;
    prOpenNext:
      FCommand.FOpenNext := Value;
    prUseHandler:
      FUseHandler := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;

function TMySQLRecordset.IsCaseSensitive: boolean;
begin
  Result := False;
end;

{ TMySQLTransaction }

constructor TMySQLTransaction.Create;
begin
  inherited Create;
end;

destructor TMySQLTransaction.Destroy;
begin
  inherited;
end;

function TMySQLTransaction.GetConnection: TMySQLConnection;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  if FConnections.Count > 1 then
    raise Exception.Create('multiple transaction'); // upd нормальное сообщениe

  Result := FConnections[0] as TMySQLConnection;
  if (Result = nil) or not Result.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);
end;

procedure TMySQLTransaction.StartTransaction;
var
  Connection: TMySQLConnection;
  s: string;
begin
  if FReadOnly then
    raise Exception.Create(SReadOnlyTransactionNotSupported);

  CheckInactive;
  Connection := GetConnection;

  // default isolation level is REPEATABLE READ
  // but default can be changed by MySQL server configuration or executing SET TRANSACTION
  case FIsolationLevel of
    ilReadCommitted:
      s := 'SET TRANSACTION ISOLATION LEVEL READ COMMITTED';
    ilReadUnCommitted:
      s := 'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED';
    ilRepeatableRead:
      s := 'SET TRANSACTION ISOLATION LEVEL REPEATABLE READ';
    ilIsolated:
      s := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;
  if Connection.GetIsServer41 then  // multistatement execution is supported since MySQL 4.1
    Connection.ExecStatement(s + '; BEGIN')
  else begin
    Connection.ExecStatement(s);
    Connection.ExecStatement('BEGIN');
  end;
  FActive := True;
  FNativeTransaction := True;
end;

procedure TMySQLTransaction.Commit;
begin
  CheckActive;
  if FNativeTransaction then
    GetConnection.ExecStatement('COMMIT');
  FActive := False;
end;

procedure TMySQLTransaction.Rollback;
begin
  CheckActive;
  if FNativeTransaction then
    GetConnection.ExecStatement('ROLLBACK');
  FActive := False;
end;

procedure TMySQLTransaction.Savepoint(const Name: _string);
begin
  CheckActive;
  GetConnection.ExecStatement('SAVEPOINT ' + Name);
end;

procedure TMySQLTransaction.ReleaseSavepoint(const Name: _string);
begin
  CheckActive;
  GetConnection.ExecStatement('RELEASE SAVEPOINT ' + Name);
end;

procedure TMySQLTransaction.RollbackToSavepoint(const Name: _string);
begin
  CheckActive;
  GetConnection.ExecStatement('ROLLBACK TO SAVEPOINT ' + Name);
end;

{$IFNDEF LITE}
{ TMySQLMetaData }

function TMySQLMetaData.CreateRecordSet: TCRRecordSet;
begin
  FRecordSet := TMySQLRecordSet.Create;
  Result := FRecordSet;
end;

function TMySQLMetaData.GetTypesForSQL(const ObjectTypes: _string): _string;
var
  i: integer;
  TypesList: _TStringList;
begin
  TypesList := _TStringList.Create;
  try
    ParseTypes(ObjectTypes, TypesList);
    Result := '';
    for i := 0 to TypesList.Count - 1 do begin
      if TypesList[i] <> '' then begin
        if Result <> '' then
          Result := Result + ', ';
        Result := Result + _QuotedStr(TypesList[i], '''');
      end;
    end;
  finally
    TypesList.Free;
  end;
end;

function TMySQLMetaData.GetTables(Restrictions: _TStrings): TData;
const
  fmtGetTablesSQL = 'SELECT ' +
    'cast('''' as char(1)) TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE ' +
    'FROM information_schema.TABLES ' +
    '%s ORDER BY TABLE_SCHEMA, TABLE_NAME ';
var
  SQL, WhereClause, Schema, TableName, TableTypes, QuotedTypes, Scope: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    if FRecordSet.FCommand.FConnection.FDatabase = '' then begin
      CreateTablesFields;
      FMemData.Open;
    end
    else begin
      SQL := 'SHOW TABLES';
      if Schema <> '' then
        SQL := SQL + ' FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(Schema);
      FRecordSet.Close;
      FRecordSet.SetSQL(SQL);
      FRecordSet.Open;

      CreateTablesFields;
      FMemData.Open;
      CopyTablesData(Restrictions);
      FRecordSet.Close;
      FMemData.SetToBegin;
    end;
    Result := FMemData;
  end
  else begin
    WhereClause := '';
    if Scope <> 'LOCAL' then
      AddWhere(WhereClause, 'TABLE_SCHEMA', Schema)
    else
      WhereClause := 'TABLE_SCHEMA = DATABASE()';
    AddWhere(WhereClause, 'TABLE_NAME', TableName);

    QuotedTypes := GetTypesForSQL(TableTypes);
    if QuotedTypes <> '' then begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      WhereClause := WhereClause + 'TABLE_TYPE IN (' + QuotedTypes + ')';
    end;

    if WhereClause <> '' then
      WhereClause := 'WHERE ' + WhereClause;

    FRecordSet.Close;
    FRecordSet.SetSQL(_Format(fmtGetTablesSQL, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
  end;
end;

procedure TMySQLMetaData.CopyTablesData(Restrictions: _TStrings);
const
  snTABLE_NAME    = 1;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
var
  Schema: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  if Schema <> '' then
    Schema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FRecordSet.FCommand.FConnection.FDatabase;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME], [dnTABLE_NAME]);
    FMemDataHelper.FieldValues[dnSCHEMA] := Schema;
    FMemDataHelper.AppendRecord;
  end;
end;

function TMySQLMetaData.GetColumns(Restrictions: _TStrings): TData;
const
  fmtGetColumnsSQL = 'SELECT ' +
    'cast('''' as char(1)) TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, ' +
    'ORDINAL_POSITION POSITION, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH DATA_LENGTH, ' +
    'NUMERIC_PRECISION DATA_PRECISION, NUMERIC_SCALE DATA_SCALE, ' +
    '(case IS_NULLABLE when ''YES'' then 1 else 0 end) NULLABLE, COLUMN_DEFAULT DEFAULT_VALUE ' +
    'FROM information_schema.COLUMNS ' +
    '%s ORDER BY TABLE_SCHEMA, TABLE_NAME, ORDINAL_POSITION ';
var
  SQL, WhereClause, Schema, TableName, ColumnName: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    if TableName = '' then begin
      raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);
    end
    else begin
      SQL := 'SHOW COLUMNS FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(TableName);
      if Schema <> '' then
        SQL := SQL + ' FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(Schema);
      FRecordSet.Close;
      FRecordSet.SetSQL(SQL);
      FRecordSet.Open;

      CreateColumnsFields;
      FMemData.Open;
      CopyColumnsData(Restrictions);
      FRecordSet.Close;
      FMemData.SetToBegin;
    end;
    Result := FMemData;
  end
  else begin
    WhereClause := '';
    AddWhere(WhereClause, 'TABLE_SCHEMA', Schema);
    AddWhere(WhereClause, 'TABLE_NAME', TableName);
    AddWhere(WhereClause, 'COLUMN_NAME', ColumnName);
    if WhereClause <> '' then
      WhereClause := 'WHERE ' + WhereClause;

    FRecordSet.Close;
    FRecordSet.SetSQL(_Format(fmtGetColumnsSQL, [WhereClause]));
    FRecordSet.Open;
    Result := FRecordSet;
  end;
end;

procedure TMySQLMetaData.CopyColumnsData(Restrictions: _TStrings);
const
  snCOLUMN_NAME   = 1;
  snDATA_TYPE     = 2;
  snNULLABLE      = 3;
  snDEFAULT       = 5;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnCOLUMN_NAME   = 4;
  dnPOSITION      = 5;
  dnDATA_TYPE     = 6;
  dnLENGTH        = 7;
  dnPRECISION     = 8;
  dnSCALE         = 9;
  dnNULLABLE      = 10;
  dnDEFAULT       = 11;

var
  Schema, TableName: _string;
  NullableStr: string;
  FieldNo, Nullable: integer;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  if Schema <> '' then
    Schema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FRecordSet.FCommand.FConnection.FDatabase;
  TableName := FRecordSet.FCommand.SQLInfo.NormalizeName(TableName, False, True);

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  FieldNo := 1;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCOLUMN_NAME, snDATA_TYPE, snDEFAULT],
      [dnCOLUMN_NAME, dnDATA_TYPE, dnDEFAULT]);

    FMemDataHelper.FieldValues[dnSCHEMA] := Schema;
    FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
    FMemDataHelper.FieldValues[dnPOSITION] := FieldNo;
    NullableStr := VarToStr(FRecordSetHelper.FieldValues[snNULLABLE]);
    if (NullableStr = 'Y') or (NullableStr = 'YES') then
      Nullable := 1
    else
      Nullable := 0;
    FMemDataHelper.FieldValues[dnNULLABLE] := Nullable;

    FMemDataHelper.AppendRecord;
    Inc(FieldNo);
  end;
end;

function TMySQLMetaData.GetProcedures(Restrictions: _TStrings): TData;
const
  fmtGetProceduresSQL = 'SELECT ' +
    'cast('''' as char(1)) PROCEDURE_CATALOG, ROUTINE_SCHEMA PROCEDURE_SCHEMA, ' +
    'ROUTINE_NAME PROCEDURE_NAME, ROUTINE_TYPE PROCEDURE_TYPE ' +
    'FROM information_schema.ROUTINES ' +
    '%s ORDER BY ROUTINE_SCHEMA, ROUTINE_NAME ';
var
  WhereClause, Schema, ProcName, ProcTypes, QuotedTypes, Scope: _string;
begin
  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    CreateProceduresFields;
    FMemData.Open;
    Result := FMemData;
    exit;
  end;

  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ProcTypes := Trim(Restrictions.Values['PROCEDURE_TYPE']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'ROUTINE_SCHEMA', Schema)
  else
    WhereClause := 'ROUTINE_SCHEMA = DATABASE()';
  AddWhere(WhereClause, 'ROUTINE_NAME', ProcName);

  QuotedTypes := GetTypesForSQL(ProcTypes);
  if QuotedTypes <> '' then begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'ROUTINE_TYPE IN (' + QuotedTypes + ')';
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.Close;
  FRecordSet.SetSQL(_Format(fmtGetProceduresSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TMySQLMetaData.GetProcedureParameters(Restrictions: _TStrings): TData;
var
  Schema, ProcName: _string;
  IsFunc: boolean;
  ParamList, ReturnParam: _string;
  ParamInfos: TParamInfos;
  i, pos: integer;
  UQSchema, UQProcName: _string;
  Direction: string;

const
  dnCATALOG        = 1;
  dnSCHEMA         = 2;
  dnPROC_NAME      = 3;
  dnPARAM_NAME     = 4;
  dnPOSITION       = 5;
  dnDIRECTION      = 6;
  dnDATA_TYPE      = 7;
  dnDATA_LENGTH    = 8;
  dnDATA_PRECISION = 9;
  dnDATA_SCALE     = 10;

begin
  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    CreateProcedureParametersFields;
    FMemData.Open;
    Result := FMemData;
    exit;
  end;

  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);

  if ProcName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['PROCEDURE_NAME']);

  // TODO: Schema is not supported by GetSPParams
  FRecordSet.FCommand.GetSPParams(ProcName, IsFunc, ParamList, ReturnParam);

  ParamInfos := nil;
  if IsFunc then
    FRecordSet.FCommand.DescribeParams(ReturnParam, True, ParamInfos, nil);
  FRecordSet.FCommand.DescribeParams(ParamList, False, ParamInfos, nil);

  CreateProcedureParametersFields;
  FMemData.Open;

  if Schema <> '' then
    UQSchema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    UQSchema := FRecordSet.FCommand.FConnection.FDatabase;
  UQProcName := FRecordSet.FCommand.SQLInfo.NormalizeName(ProcName, False, True);

  FMemDataHelper.AllocBuffer;
  for i := 0 to High(ParamInfos) do begin
    FMemDataHelper.InitRecord;

    FMemDataHelper.FieldValues[dnSCHEMA] := UQSchema;
    FMemDataHelper.FieldValues[dnPROC_NAME] := UQProcName;
    FMemDataHelper.FieldValues[dnPARAM_NAME] := ParamInfos[i].Name;
    pos := i;
    if not IsFunc then
      Inc(pos);
    FMemDataHelper.FieldValues[dnPOSITION] := pos;

    case ParamInfos[i].Direction of
      pdInput:
        Direction := 'IN';
      pdOutput, pdResult:
        Direction := 'OUT';
      pdInputOutput:
        Direction := 'IN/OUT';
    end;
    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;
    FMemDataHelper.FieldValues[dnDATA_TYPE] := ParamInfos[i].ParamType;
    if ParamInfos[i].Size >= 0 then
      FMemDataHelper.FieldValues[dnDATA_LENGTH] := ParamInfos[i].Size;

    FMemDataHelper.AppendRecord;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TMySQLMetaData.GetIndex(Restrictions: _TStrings; Columns: boolean): TData;
var
  TableName, TableSchema, SQL: _string;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);

  if TableName = '' then begin
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);
  end
  else begin
    SQL := 'SHOW INDEX FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(TableName);
    if TableSchema <> '' then
      SQL := SQL + ' FROM ' + FRecordSet.FCommand.SQLInfo.NormalizeName(TableSchema);
    FRecordSet.Close;
    FRecordSet.SetSQL(SQL);
    FRecordSet.Open;

    if Columns then
      CreateIndexColumnsFields
    else
      CreateIndexesFields;
    FMemData.Open;
    if Columns then
      CopyIndexColumnsData(Restrictions)
    else
      CopyIndexesData(Restrictions);
    FRecordSet.Close;
    FMemData.SetToBegin;
  end;
  Result := FMemData;
end;

function TMySQLMetaData.GetIndexes(Restrictions: _TStrings): TData;
begin
  Result := GetIndex(Restrictions, False);
end;

procedure TMySQLMetaData.CopyIndexesData(Restrictions: _TStrings);
const
  snTABLE_NAME    = 1;
  snNON_UNIQUE    = 2;
  snINDEX_NAME    = 3;

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
var
  Schema, OldIndexName, IndexName: _string;
  Unique: integer;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  if Schema <> '' then
    Schema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FRecordSet.FCommand.FConnection.FDatabase;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  OldIndexName := '';
  while FRecordSetHelper.NextRecord do begin
    IndexName := _VarToStr(FRecordSetHelper.FieldValues[snINDEX_NAME]);
    if IndexName = OldIndexName then
      continue;
    OldIndexName := IndexName;

    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_NAME, snINDEX_NAME],
      [dnTABLE_NAME, dnINDEX_NAME]);

    FMemDataHelper.FieldValues[dnTABLE_SCHEMA] := Schema;
    FMemDataHelper.FieldValues[dnINDEX_SCHEMA] := Schema;

    if FRecordSetHelper.FieldValues[snNON_UNIQUE] = 0 then
      Unique := 1
    else
      Unique := 0;
    FMemDataHelper.FieldValues[dnUNIQUE] := Unique;

    FMemDataHelper.AppendRecord;
  end;
end;

function TMySQLMetaData.GetIndexColumns(Restrictions: _TStrings): TData;
begin
  Result := GetIndex(Restrictions, True);
end;

procedure TMySQLMetaData.CopyIndexColumnsData(Restrictions: _TStrings);
const
  snTABLE_NAME      = 1;
  snINDEX_NAME      = 3;
  snPOSITION        = 4;
  snCOLUMN_NAME     = 5;
  snCOLLATION       = 6;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnINDEX_CATALOG   = 4;
  dnINDEX_SCHEMA    = 5;
  dnINDEX_NAME      = 6;
  dnCOLUMN_NAME     = 7;
  dnPOSITION        = 8;
  dnSORT_ORDER      = 9;
var
  Schema: _string;
  SortOrder: string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  if Schema <> '' then
    Schema := FRecordSet.FCommand.SQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FRecordSet.FCommand.FConnection.FDatabase;

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_NAME, snINDEX_NAME, snCOLUMN_NAME, snPOSITION],
      [dnTABLE_NAME, dnINDEX_NAME, dnCOLUMN_NAME, dnPOSITION]);

    FMemDataHelper.FieldValues[dnTABLE_SCHEMA] := Schema;
    FMemDataHelper.FieldValues[dnINDEX_SCHEMA] := Schema;

    SortOrder := VarToStr(FRecordSetHelper.FieldValues[snCOLLATION]);
    if SortOrder = 'A' then
      SortOrder := 'ASC'
    else
    if SortOrder = 'D' then
      SortOrder := 'DESC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := SortOrder;

    FMemDataHelper.AppendRecord;
  end;
end;

function TMySQLMetaData.GetConstraints(Restrictions: _TStrings): TData;
const
  fmtGetConstraintsSQL = 'SELECT ' +
    'cast('''' as char(1)) TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, CONSTRAINT_NAME, ' +
    'CONSTRAINT_TYPE, cast('''' as char(1)) INDEX_CATALOG, ' +
    'cast('''' as char(1)) INDEX_OWNER, cast('''' as char(1)) INDEX_NAME ' +
    'FROM information_schema.table_constraints ' +
    '%s ORDER BY TABLE_SCHEMA, TABLE_NAME, CONSTRAINT_NAME';
var
  WhereClause, Schema, TableName, ConstraintName, Types, QuotedTypes: _string;
begin
  if FRecordSet.FCommand.FConnection.ServerPrimaryVer < 5 then begin
    CreateConstraintsFields;
    FMemData.Open;
    Result := FMemData;
    exit;
  end;

  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := Trim(Restrictions.Values['CONSTRAINT_TYPE']);

  WhereClause := '';
  AddWhere(WhereClause, 'TABLE_SCHEMA', Schema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'CONSTRAINT_NAME', ConstraintName);

  QuotedTypes := GetTypesForSQL(Types);
  if QuotedTypes <> '' then begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'CONSTRAINT_TYPE IN (' + QuotedTypes + ')';
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.Close;
  FRecordSet.SetSQL(_Format(fmtGetConstraintsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TMySQLMetaData.GetDatabases(Restrictions: _TStrings): TData;
begin
  FRecordSet.Close;
  FRecordSet.SetSQL('SHOW DATABASES');
  FRecordSet.Open;

  CreateDatabasesFields;
  FMemData.Open;
  CopyDatabasesData(Restrictions);
  FRecordSet.Close;
  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TMySQLMetaData.CopyDatabasesData(Restrictions: _TStrings);
const
  snDB_NAME = 1;
  dnDB_NAME = 1;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snDB_NAME], [dnDB_NAME]);
    FMemDataHelper.AppendRecord;
  end;
end;
{$ENDIF}

{ TMySQLLoader }

constructor TMySQLLoader.Create;
begin
  inherited;

  FBuffer := AnsiStringBuilder.Create(16384 {default net_buffer_length value} * 10);
  FRowBuffer := AnsiStringBuilder.Create(1000);
  //FCommandTimeout := 30;;
end;

destructor TMySQLLoader.Destroy;
begin
  FRowBuffer.Free;
  FBuffer.Free;

  inherited;
end;

function TMySQLLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prLock:
      FLock := Value;
    prDelayed:
      FDelayed := Value;
    prRowsPerQuery:
      FRowsPerQuery := Value;
    prDuplicateKeys:
      FDuplicateKeys := _TMyDuplicateKeys(Value);
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TMySQLLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prLock:
      Value := FLock;
    prDelayed:
      Value := FDelayed;
    prRowsPerQuery:
      Value := FRowsPerQuery;
    prDuplicateKeys:
      Value := Variant(FDuplicateKeys);
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

procedure TMySQLLoader.Clear;
begin
  FBuffer.Length := 0;
  FBuffer.Append(AnsiString(FInsHeader));
end;

procedure TMySQLLoader.Reset;
begin
  FInsHeader := '';
  Clear;
  inherited;
end;

procedure TMySQLLoader.Prepare;
var
  ColumnList: _string;
  i: integer;
begin
  inherited;

  ColumnList := '';
  for i := 0 to Columns.Count - 1 do
    if i = 0 then
      ColumnList := Columns[i].Name
    else
      ColumnList := ColumnList + ', ' + Columns[i].Name;

  case FDuplicateKeys of
    _dkNone:
      if FDelayed then
        FInsHeader := 'INSERT DELAYED INTO '
      else
        FInsHeader := 'INSERT INTO ';
    _dkIgnore:
      if FDelayed then
        FInsHeader := 'INSERT DELAYED IGNORE INTO '
      else
        FInsHeader := 'INSERT IGNORE INTO ';
    _dkReplace:
      if FDelayed then
        FInsHeader := 'REPLACE DELAYED INTO '
      else
        FInsHeader := 'REPLACE INTO ';
  end;
  FInsHeader := FInsHeader + FTableName + ' (' + ColumnList + ') VALUES ';
  Clear;

  SetLength(FValues, Columns.Count);

  if FLock then
    ExecSQL(AnsiString('LOCK TABLES ' + FTableName + ' WRITE'));
end;

procedure TMySQLLoader.Flush;
begin
  if (FBuffer.Length = 0) or (FBuffer.Length = Length(FInsHeader)) then
    Exit;

  ExecSQL(FBuffer.ToString);

  Clear;
end;

procedure TMySQLLoader.ExecSQL(const SQL: AnsiString);
var
  ConnectionTimeout: integer;
  v: variant;
  SQLResult: pMYSQL_RES;
begin
  FConnection.GetProp(prConnectionTimeout, v);
  ConnectionTimeout := v;
  //Connection.ConnectionTimeout := CommandTimeout;
  FConnection.MySQLAPI.SetTimeout(FConnection.MySQL, ConnectionTimeout);
  FConnection.Check(FConnection.MySQLAPI.mysql_real_query(FConnection.MySQL, SQL, Length(SQL)), nil);
  SQLResult := FConnection.MySQLAPI.mysql_use_result(FConnection.MySQL);
  if SQLResult <> nil then
    FConnection.MySQLAPI.mysql_free_result(SQLResult);
end;

procedure TMySQLLoader.CalculateRowBuffer;
var
  i: integer;
  v: variant;
  UseUnicode, NeedBackslashes: boolean;
begin
  FConnection.GetProp(prUseUnicode, v);
  UseUnicode := v;
  FConnection.GetProp(prNeedBackslashes, v);
  NeedBackslashes := v;

  FRowBuffer.Length := 0;
  for i := 0 to Length(FValues) - 1 do begin
    if i <> 0 then
      FRowBuffer.Append(', ');

    AppendValueToSQL(FRowBuffer, Columns[i].DataType, FValues[i],
      VarIsEmpty(FValues[i]) or VarIsNull(FValues[i]), UseUnicode,
      {$IFDEF HAVE_COMPRESS}False, {$ENDIF} NeedBackslashes, FConnection.ServerPrimaryVer, FConnection.FCharset);
    FValues[i] := Null;
  end;
end;

procedure TMySQLLoader.AppendRowBufferToBuffer;
begin
  if FBuffer.Length <> Length(FInsHeader) then
    FBuffer.Append(', ');

  FBuffer.Append('(');
  FBuffer.Append(FRowBuffer);
  FBuffer.Append(')');
  Inc(FLoadedRows);
end;

procedure TMySQLLoader.PutColumnData(Col, Row: integer; const Value: variant);
begin
  if FInsHeader = '' then
    Prepare;

  if (FLastRow <> -1) and (Row > FLastRow + 1) then begin
    if Row <> FLastRow + 2 then
      raise Exception.Create('Invalid row number');

    if FRowsPerQuery > 0 then begin
      CalculateRowBuffer;
      AppendRowBufferToBuffer;
      if (Row - 1) mod FRowsPerQuery = 0 then
        Flush;
    end
    else begin
      CalculateRowBuffer;
      if (FRowBuffer.Length + FBuffer.Length + 10 {Length(', ()') + reserve} > 16384 {default net_buffer_length value} * 10{??? perf. opt})
        and (FBuffer.Length <> Length(FInsHeader))
      then
        Flush;
      AppendRowBufferToBuffer;
    end;
  end;

  inherited;


{$IFNDEF VER6P}
  if TVarData(Value).VType = varByRef then begin
    FValues[Col] := Unassigned;
    TVarData(FValues[Col]).VType := varByRef;
    TVarData(FValues[Col]).VPointer := TVarData(Value).VPointer;
  end
  else
{$ENDIF}
  FValues[Col] := Value;
end;

procedure TMySQLLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then begin
    CalculateRowBuffer;
    AppendRowBufferToBuffer;
  end;
  Flush;
end;

procedure TMySQLLoader.Finish;
begin
  if FLock then
    ExecSQL('UNLOCK TABLES');
  Reset;
end;

class function TMySQLLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TMySQLRecordSet;
end;

procedure TMySQLLoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TMySQLConnection(Value);
end;

procedure TMySQLLoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  inherited;

  if FieldDesc.DataType = dtBoolean then
    Column.DataType := dtInteger;
end;

initialization
{$IFDEF VER8P}
  ZeroDate := EncodeDate(100, 1, 1);
{$ELSE}
  ZeroDate := not $A9558; // EncodeDate(1, 1, 1);
{$ENDIF}

{$IFDEF HAVE_COMPRESS_INTERFACE}
{$IFNDEF HAVE_COMPRESS_INTERNAL}
  CompressProc := {$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.compress;
  UnCompressProc := {$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.uncompress;
{$ENDIF}
{$ENDIF}
  MySQLInfo := TMySQLInfo.Create(nil);

finalization
  MySQLInfo.Free;

end.
