
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 2006-2011 Devart. All right reserved.
//  InterBase Classes
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I IbDac.inc}
unit IBCClassesUni;
{$ENDIF}

{$J+}
{$DEFINE _LOCAL_ERROR_HANDLE}  // LOCAL_ERROR_HANDLE
{$IFDEF VER6P}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFNDEF VER6P}
   ActiveX,
{$ENDIF}
{$IFDEF CLR}
  WinUtils,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF VER6P}
  Variants, FMTBcd,
{$ENDIF}
{$IFDEF VER10}
{$IFNDEF CLR}
  WideStrUtils,
{$ENDIF}
{$ENDIF}
  SysUtils, Classes, SyncObjs, MemData, MemUtils, CRParser, CRAccess,
{$IFNDEF UNIDACPRO}
  IBCCall, IBCError;
{$ELSE}
  IBCCallUni, IBCErrorUni;
{$ENDIF}

const
  dtDbKey           = 101;
  dtFixedChar       = 102;
  dtFixedWideChar   = 103;
  dtExtBytes        = 104;

  prGDSBase           = 1000;

  prCharset           = prGDSBase + 1;  // string
  prUseUnicode        = prGDSBase + 2;  // bool
  prRole              = prGDSBase + 3;  // string
  prDBSQLDialect      = prGDSBase + 4;  // integer ReadOnly default 3;
  prSQLDialect        = prGDSBase + 5;  // integer
  prCharLength        = prGDSBase + 6;  // word
  prOptimizedNumerics = prGDSBase + 7;  // bool for DbxIda
  prEnableMemos       = prGDSBase + 8;  // bool
  prAutoClose         = prGDSBase + 9;  // bool
  prCursor            = prGDSBase + 10; // string
  prPlan              = prGDSBase + 11; // string
  prQueryRowsAffected = prGDSBase + 12; // bool
  prParsedSQLType     = prGDSBase + 13; // word
  prDeferredBlobRead  = prGDSBase + 15; // bool
  prStreamedBlobs     = prGDSBase + 16; // bool
  prCacheBlobs        = prGDSBase + 17; // bool
  prComplexArrayFields = prGDSBase + 18; // bool
  prCacheArrays       = prGDSBase + 19; // bool
  prDeferredArrayRead = prGDSBase + 20; // bool
  prBooleanDomainFields = prGDSBase + 21; // bool
  prClientLibrary     = prGDSBase + 22; // string
  prSimpleNumericMap  = prGDSBase + 23; // bool
  prProtocol          = prGDSBase + 24; // TIBCProtocol
  prRowsInserted      = prGDSBase + 26;
  prRowsUpdated       = prGDSBase + 27;
  prRowsDeleted       = prGDSBase + 28;
  prEnableLargeint    = prGDSBase + 29;
  prInsertMode        = prGDSBase + 30;
  prRowsPerBatch      = prGDSBase + 31;
  prSetDomainNames    = prGDSBase + 32;

  LocalBufferLen = 1024;

  IBC_MAX_EVENTS = 15;
  IBC_MAX_EVENT_LENGTH = 64;

type
  TXSQLVARType = (vtGDS, vtGDS7);
  _TIBCProtocol = (_TCP, _NetBEUI, _SPX);
  _TStringArray = array of _string;

  TGDSCommand = class;
  TGDSTransaction = class;
  TGDSConnection = class;
  TIBCBlob = class;

  TGDSDatabaseInfo = class
  private
    FGDSConnection: TGDSConnection;
    FUserNames: TStringList;
    FBackoutCount: TStringList;
    FDeleteCount: TStringList;
    FExpungeCount: TStringList;
    FInsertCount: TStringList;
    FPurgeCount: TStringList;
    FReadIdxCount: TStringList;
    FReadSeqCount: TStringList;
    FUpdateCount: TStringList;
    FActiveTransactions: TStringList;
    procedure CheckConnection;
    procedure GetDatabaseInfo(LocalBuffer: IntPtr; DbInfoCommand: IntPtr);
    function GetDBFileName: string;
    function GetDBSiteName: string;
    function GetDBImplementationNo: integer;
    function GetDBImplementationClass: integer;
    function GetVersion(DatabaseInfoCommand: integer): string;
    function GetUserNames: TStringList;
    function GetBackoutCount: TStringList;
    function GetDeleteCount: TStringList;
    function GetExpungeCount: TStringList;
    function GetInsertCount: TStringList;
    function GetPurgeCount: TStringList;
    function GetReadIdxCount: TStringList;
    function GetReadSeqCount: TStringList;
    function GetUpdateCount: TStringList;
    function GetIsRemoteConnect: boolean;
    function GetActiveTransactions: TStringList;
    function GetStringDatabaseInfo(DatabaseInfoCommand: integer): string;
    function GetBooleanDatabaseInfo(DatabaseInfoCommand: integer): boolean;
    function GetIntDatabaseInfo(DatabaseInfoCommand: integer): integer;
    function GetOperationCounts(DataBaseInfoCommand: integer; FOperation: TStringList): TStringList;

    property Owner: TGDSConnection read FGDSConnection write FGDSConnection;
  public
    constructor Create;
    destructor Destroy; override;
    property Allocation: integer index isc_info_allocation read GetIntDatabaseInfo;
    property BaseLevel: integer index isc_info_base_level read GetIntDatabaseInfo;
    property DBFileName: string read GetDBFileName;
    property DBSiteName: string read GetDBSiteName;
    property IsRemoteConnect: boolean read GetIsRemoteConnect;
    property DBImplementationNo: integer read GetDBImplementationNo;
    property DBImplementationClass: integer read GetDBImplementationClass;
    property NoReserve: integer index isc_info_no_reserve read GetIntDatabaseInfo;
    property ODSMinorVersion: integer index isc_info_ods_minor_version read GetIntDatabaseInfo;
    property ODSMajorVersion: integer index isc_info_ods_version read GetIntDatabaseInfo;
    property PageSize: integer index isc_info_page_size read GetIntDatabaseInfo;
    property Version: string index isc_info_version read GetVersion;
    property CurrentMemory: integer index isc_info_current_memory read GetIntDatabaseInfo;
    property ForcedWrites: integer index isc_info_forced_writes read GetIntDatabaseInfo;
    property MaxMemory: integer index isc_info_max_memory read GetIntDatabaseInfo;
    property NumBuffers: integer index isc_info_num_buffers read GetIntDatabaseInfo;
    property SweepInterval: integer index isc_info_sweep_interval read GetIntDatabaseInfo;
    property UserNames: TStringList read GetUserNames;
    property Fetches: integer index isc_info_fetches read GetIntDatabaseInfo;
    property Marks: integer index isc_info_marks read GetIntDatabaseInfo;
    property Reads: integer index isc_info_reads read GetIntDatabaseInfo;
    property Writes: integer index isc_info_writes read GetIntDatabaseInfo;
    property BackoutCount: TStringList read GetBackoutCount;
    property DeleteCount: TStringList read GetDeleteCount;
    property ExpungeCount: TStringList read GetExpungeCount;
    property InsertCount: TStringList read GetInsertCount;
    property PurgeCount: TStringList read GetPurgeCount;
    property ReadIdxCount: TStringList read GetReadIdxCount;
    property ReadSeqCount: TStringList read GetReadSeqCount;
    property UpdateCount: TStringList read GetUpdateCount;
    property DBSQLDialect: integer index isc_info_db_SQL_dialect read GetIntDatabaseInfo;
    property ReadOnly: Boolean index isc_info_db_read_only read GetBooleanDatabaseInfo;
    property LogFile: integer index isc_info_logfile read GetIntDatabaseInfo;
    property CurLogFileName: string index isc_info_cur_logfile_name read GetStringDatabaseInfo;
    property CurLogPartitionOffset: integer index isc_info_cur_log_part_offset read GetIntDatabaseInfo;
    property NumWALBuffers: integer index isc_info_num_wal_buffers read GetIntDatabaseInfo;
    property WALBufferSize: integer index isc_info_wal_buffer_size read GetIntDatabaseInfo;
    property WALCheckpointLength: integer index isc_info_wal_ckpt_length read GetIntDatabaseInfo;
    property WALCurCheckpointInterval: integer index isc_info_wal_cur_ckpt_interval read GetIntDatabaseInfo;
    property WALPrvCheckpointFilename: string index isc_info_wal_prv_ckpt_fname read GetStringDatabaseInfo;
    property WALPrvCheckpointPartOffset: integer index isc_info_wal_prv_ckpt_poffset read GetIntDatabaseInfo;
    property WALGroupCommitWaitUSecs: integer index isc_info_wal_grpc_wait_usecs read GetIntDatabaseInfo;
    property WALNumIO: integer index isc_info_wal_num_io read GetIntDatabaseInfo;
    property WALAverageIOSize: integer index isc_info_wal_avg_io_size read GetIntDatabaseInfo;
    property WALNumCommits: integer index isc_info_wal_num_commits read GetIntDatabaseInfo;
    property WALAverageGroupCommitSize: integer index isc_info_wal_avg_grpc_size read GetIntDatabaseInfo;
    property AttachmentID: integer index isc_info_attachment_id read GetIntDatabaseInfo;
    property IsEncrypted: boolean index isc_info_db_encrypted read GetBooleanDatabaseInfo;
    property IsEUAActive: boolean index isc_info_db_eua_active read GetBooleanDatabaseInfo;
  { Firebird }
    property FBVersion: string index frb_info_firebird_version read GetVersion;
    property InfoAttCharset: integer index frb_info_att_charset read GetIntDatabaseInfo;
    property InfoDbClass: integer index frb_info_db_class read GetIntDatabaseInfo;
    property InfoOldestTransaction: integer index frb_info_oldest_transaction read GetIntDatabaseInfo;
    property InfoOldestActive: integer index frb_info_oldest_active read GetIntDatabaseInfo;
    property InfoOldestSnapshot: integer index frb_info_oldest_snapshot read GetIntDatabaseInfo;
    property InfoNextTransaction: integer index frb_info_next_transaction read GetIntDatabaseInfo;
    property InfoDbProvider: integer index frb_info_db_provider read GetIntDatabaseInfo;
    property InfoActiveTransactions: TStringList read GetActiveTransactions;
  end;

  TGDSConnection = class (TCRConnection)
  private
    FDatabaseHandle: PISC_DB_HANDLE;
    FDPB: TBytes;
    FDPBLength: integer;
    FStatusVector: TStatusVector;
    FLastError: integer;
    FGDS: TGDS;

    { Parameters }
    FDatabase: string;
    FProtocol: _TIBCProtocol;
    FRole: string;
    FDBSQLDialect: integer;
    FSQLDialect: integer;
    FReadOnly: Boolean;
    FReadOnlyCahed: Boolean;
    FIsFBServerCached: Boolean;
    FIsFBServer: Boolean;
    FOptimizedNumerics: boolean;
    FEnableLargeint: boolean;
    FEnableMemos: boolean;
    FSimpleNumericMap: boolean;

    { Charset options }
    FCharset: string;
    FCharsetId: word;
    FDBCharsetId: word;
    FCharLength: word;
    FDBCharLength: word;
    FQueryCharLength: boolean;
    FUseUnicode: boolean;
    FClientLibrary: string;

    FParams: TStrings;
    FParamsChanged: boolean;
    FDatabaseInfo: TGDSDatabaseInfo;
    FMajorServerVersion: integer;
    FMinorServerVersion: integer;
    FServerVersion: string;
    FServerVersionFull: string;

    procedure ParamsChange(Sender: TObject);
    procedure ParamsChanging(Sender: TObject);
    procedure CheckInactive;
    procedure CheckClientSQLDialect;
    procedure GetDatabaseParameters;
    function GetCharLength(CharsetID: integer): integer;
    procedure FlushProps;

  protected
    procedure CreateDPB;
    procedure DoError(E: Exception; var Fail: boolean); override;

    property InProcessError: boolean read FInProcessError;
    property AutoCommit;
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

    procedure Check(Status: ISC_STATUS);
    procedure IBCError(var StatusVector: TStatusVector; UseCallback: boolean; Component: TObject); virtual;

    procedure Connect(const ConnectString: _string); override;
    procedure Disconnect; override;

    function CreateCommand: TGDSCommand;

  { Connect control }
    procedure AssignConnect(Source: TCRConnection); override;
    function GetLastError: integer;
    procedure SetLastError(Value: integer);
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
    procedure SetUsername(const Value: _string); override;
    procedure SetPassword(const Value: _string); override;
    function CheckIsValid: boolean; override;
    function CanChangeDatabase: boolean; override;

  { InterBase support}
    function GetDatabaseHandle: TISC_DB_HANDLE;
    procedure SetDatabaseHandle(Value: TISC_DB_HANDLE);
    procedure CreateDatabase;
    procedure DropDatabase;

    function IsFBServer: boolean;
    function GetMajorServerVersion: integer;
    function GetMinorServerVersion: integer;

    function GetServerVersion: _string; override;
    function GetServerVersionFull: _string; override;
    function GetClientVersion: _string; override;

    procedure SetParams(const Value: TStrings);
    function GetParams: TStrings;

    property DatabaseInfo: TGDSDatabaseInfo read FDatabaseInfo;
    property GDS: TGDS read FGDS;
  end;

  { TGDSConnections }

  TGDSConnections = class (TDAList)
  private
    function GetItems(Index: integer): TGDSConnection;

  public
    function FindConnection(GDSConnection: TGDSConnection): integer;
    property Items[Index: integer]: TGDSConnection read GetItems; default;
  end;

  { TGDSTransaction }
  TIBCIsolationLevel = (iblSnapshot, iblReadCommitted, iblReadOnlyReadCommitted, iblTableStability, iblReadOnlyTableStability, iblCustom);

  TGDSTransaction = class(TCRTransaction)
  private
    FTransactionHandle: PISC_TR_HANDLE;
    FTPB: TBytes;
    FTPBLength: integer;

    FStatusVector: TStatusVector;
    FLastError: integer;
    FGDS: TGDS;

    { Parameters }
    FParams: TStrings;
    FParamsChanged: boolean;

    //FOnClose: TNotifyEvent; // not used

    procedure ParamsChange(Sender: TObject);
    procedure ParamsChanging(Sender: TObject);
    procedure ExecuteImmediate(const SQL: string);
  protected
    procedure CreateTPB;
    procedure IBCError(var StatusVector: TStatusVector; UseCallback: boolean); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Check(Status: ISC_STATUS);

  { Transaction control }
    procedure Commit; override;
    procedure CommitRetaining; override;
    procedure Rollback; override;
    procedure RollbackRetaining; override;
    procedure StartTransaction; override;
    procedure AssignConnect(Source: TCRTransaction); override;
    procedure Reset; override;
    function CanRestoreAfterFailover: boolean; override;

  { Savepoit control }
    procedure ReleaseSavepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;
    procedure Savepoint(const Name: _string); override;

    procedure AssignTransaction(Source: TGDSTransaction);
    function SetProp(Prop: integer; const Value: variant): boolean; override; // TODO: remove
    function GetProp(Prop: integer; var Value: variant): boolean; override; // TODO: remove

  { Connections list control }
    function GetConnection(Idx: integer): TGDSConnection;

    function GetTransactionHandle: TISC_TR_HANDLE;
    procedure SetTransactionHandle(Value: TISC_TR_HANDLE);

    procedure SetParams(const Value: TStrings);
    function GetParams: TStrings;

    function GetInTransaction: boolean; override;
    //property OnClose: TNotifyEvent read FOnClose write FOnClose;

    property GDS: TGDS read FGDS;
    property NativeTransaction: boolean read FNativeTransaction;
  end;

  TSQLDA = class;

  TSQLVARAccessor = class
  private
    FOwner: TSQLDA;
    FXSQLVAR: IntPtr;
    FXSQLVARType: TXSQLVARType;

    procedure ReadStringValue(Offset: integer; var Value: _string);
    procedure WriteStringValue(Offset: integer; const Value: _string);

    function GetSqlType: Short;
    procedure SetSqlType(const Value: Short);
    function GetSqlScale: Short;
    procedure SetSQLScale(const Value: Short);
    function GetSqlPrecision: Short;
    procedure SetSqlPrecision(const Value: Short);
    function GetSqlSubtype: Short;
    procedure SetSqlSubtype(const Value: Short);
    function GetSqlLen: Word;
    procedure SetSqlLen(const Value: Word);
    function GetSqlData: IntPtr;
    procedure SetSqlData(const Value: IntPtr);
    function GetSqlInd: IntPtr;
    procedure SetSqlInd(const Value: IntPtr);
    function GetSqlName_length: Short;
    procedure SetSqlName_length(const Value: Short);
    function GetSqlName: _string;
    procedure SetSqlName(const Value: _string);
    function GetRelName_Length: Short;
    procedure SetRelName_Length(const Value: Short);
    function GetRelName: _string;
    procedure SetRelName(const Value: _string);
    function GetOwnName_Length: Short;
    procedure SetOwnName_Length(const Value: Short);
    function GetOwnName: _string;
    procedure SetOwnName(const Value: _string);
    function GetAliasName_Length: Short;
    procedure SetAliasName_Length(const Value: Short);
    function GetAliasName: _string;
    procedure SetAliasName(const Value: _string);
    function GetIsNullable: boolean;
  protected
    procedure SetXSQLVAR(XSQLVAR: IntPtr; XSQLVARType: TXSQLVARType);
  public
    constructor Create(AOwner: TSQLDA);

    property sqltype: Short read GetSqlType write SetSqlType;
    property sqlscale: Short read GetSqlScale write SetSQLScale;
    property sqlprecision: Short read GetSqlPrecision write SetSQLPrecision;
    property sqlsubtype: Short read GetSqlSubtype write SetSqlSubtype;
    property sqllen: Word read GetSqlLen write SetSqlLen;
    property sqldata: IntPtr read GetSqlData write SetSqlData;
    property sqlind: IntPtr read GetSqlInd write SetSqlInd;
    property sqlname_length: Short read GetSqlName_length write SetSqlName_length;
    property sqlname: _string read GetSqlName write SetSqlName;
    property relname_length: Short read GetRelName_Length write SetRelName_Length;
    property relname: _string read GetRelName write SetRelName;
    property ownname_length: Short read GetOwnName_Length write SetOwnName_Length;
    property ownname: _string read GetOwnName write SetOwnName;
    property aliasname_length: Short read GetAliasName_Length write SetAliasName_Length;
    property aliasname: _string read GetAliasName write SetAliasName;
    property IsNullable: boolean read GetIsNullable;
  end;

  TSQLDA = class
  private
    FConnection: TGDSConnection;
    FXSQLDA: IntPtr;
    FXSQLVARType: TXSQLVARType;
    FXSQLVARs: array of TSQLVARAccessor;

    function GetVersion: Short;
    procedure SetVersion(const Value: Short);
    function GetSqldaid: String;
    procedure SetSqldaid(const Value: String);
    function GetSqldabc: Long;
    procedure SetSqldabc(const Value: Long);
    function GetSqln: Short;
    procedure SetSqln(const Value: Short);
    function GetSqld: Short;
    procedure SetSqld(const Value: Short);
    function GetSqlvar: IntPtr;
    procedure SetSqlvar(const Value: IntPtr);
    function GetSQLVars(Index: integer): TSQLVARAccessor;
  public
    constructor Create(Connection: TGDSConnection; SQLVARType: TXSQLVARType);
    destructor Destroy; override;

    procedure AllocSQLDA(n: integer);
    procedure FreeSQLDA;

    property Version: Short read GetVersion write SetVersion;
    property SqldaID: String read GetSqldaid write SetSqldaid;
    property Sqldabc: Long read GetSqldabc write SetSqldabc;
    property Sqln: Short read GetSqln write SetSqln;
    property Sqld: Short read GetSqld write SetSqld;
    property SqlVar: IntPtr read GetSqlvar write SetSqlvar;

    property Vars[Index: integer]: TSQLVARAccessor read GetSQLVars; default;
  end;

  TIBCParamDesc = class (TParamDesc)
  private
    FValue: IntPtr;
    FInternalValue: IntPtr;
    FValueSize: integer;
    FInternalSize: integer;
    FIndicator: IntPtr;
    FPrecision: integer;
    FScale: integer;

  protected
    procedure AllocBuffer;
    procedure FreeBuffer;

    property Name;
    property DataType;
    property ParamType;
    property Size;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetDataType(Value: word); override;
    procedure SetSize(Value: integer); override; //Value in bytes

    function ValuePtr: IntPtr;
    procedure SetValuePtr(Buf: IntPtr);

    function InternalValuePtr: IntPtr;

    function IndicatorPtr: IntPtr;
    procedure SyncIndicator;

    function GetAsFloat: double;
    procedure SetAsFloat(Value: double);
    function GetAsAnsiString: AnsiString;
    procedure SetAsAnsiString(const Value: AnsiString);
    function GetAsWideString: WideString;
    procedure SetAsWideString(const Value: WideString);
    function GetAsInteger: integer;
    procedure SetAsInteger(Value: integer);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetAsBoolean: boolean;
    procedure SetAsBoolean(Value: boolean);

    function GetAsSmallInt: SmallInt;
    procedure SetAsSmallInt(Value: SmallInt);
    function GetAsInt64: Int64;
    procedure SetAsInt64(Value: Int64);
    function GetAsCurr: Currency;
    procedure SetAsCurr(Value: Currency);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    function GetAsFmtBCD: TBcd;
    procedure SetAsFmtBCD(const Value: TBcd);
  {$ENDIF}
  {$ENDIF}

    function GetAsVariant: variant;
    procedure SetAsVariant(const Value: variant);
    function GetValue: variant; override;
    procedure SetValue(const Value: variant); override;

    function GetNull: boolean; override;
    procedure SetNull(const Value: boolean); override;

    property Precision: integer read FPrecision write FPrecision;
    property Scale: integer read FScale write FScale;
  end;

  TGDSCommand = class (TCRCommand)
  private
    FSQLType: word;
    FParsedSQLType: word;       //Type determined at parse time used for Insert ... Returning
    FIntegerPrecision: integer;
    FFloatPrecision: integer;
    FQueryRowsAffected: boolean;
    FCacheBlobs: boolean;
    FStreamedBlobs: boolean;
    FCacheArrays: boolean;
    FUseDescribeParams: boolean;
    FRowsInserted: integer;
    FRowsUpdated: integer;
    FRowsDeleted: integer;

    FState: TCursorState;

    FStmtHandle: PISC_STMT_HANDLE;
    FCursor: IntPtr;
    FStatusVector: TStatusVector;

    FGCHandle: IntPtr;

    FInXSQLDA: TSQLDA;
    FOutXSQLDA: TSQLDA;

    function GetIsFBConnection: boolean;
    function GetGCHandle: IntPtr;
    procedure CheckSQLDA(InVarsCount: integer = 0; OutVarsCount: integer = 0);
    function GetPlan: string;
    function GetGDS: TGDS;
  protected
    FConnection: TGDSConnection;
    FTransaction: TGDSTransaction;

    procedure RaiseError(const Msg: string); virtual; // for TRIALCALL

    procedure CheckDatabase;
    procedure CheckTransaction;

    procedure Check(Status: ISC_STATUS);
    procedure CheckActive;

    function GetActive: boolean;
    function GetRowsAffected: integer;

    procedure ReadOutParams;
    procedure DescribeParam(const Name: _string; Param: TIBCParamDesc; SQLVAR: TSQLVARAccessor);
    procedure DescribeParams(Full: boolean = True; InParamsCount: integer = 0);

    property Params;
    property Executing;
    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}

    property GCHandle: IntPtr read GetGCHandle;

  public
    constructor Create; override; //CLR requirement
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;

    procedure InternalExecute;
    function ExecuteNext: boolean;

    procedure Finish;

    function ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string; override;
    function CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string; override;

  { Params }
    function AddParam: TParamDesc; override;
    procedure BindParams;

    function GetParamDescType: TParamDescClass; override;
    function GetParam(Index: integer): TIBCParamDesc;

    procedure BreakExec; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;

    procedure Execute(Iters: integer = 1); override;
    procedure ExecuteBatch(const Statements: _TStringArray);

    procedure SetConnection(Value: TCRConnection); override;
    procedure SetTransaction(Value: TCRTransaction); override;
    function GetSQLType: integer;
    function GetSQLTypeEx: integer;

    function GetStmtHandle: TISC_STMT_HANDLE;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    property GDS: TGDS read GetGDS;
  end;

  TIBCFieldDesc = class(TCRFieldDesc)
  protected
    FCharsetID: integer;
    FSQLSubType: integer;
    FDomainName: _string;
  public
    property DomainName: _string read FDomainName write FDomainName;
    property CharsetID: integer read FCharsetID write FCharsetID;
    property SQLSubType: integer read FSQLSubType write FSQLSubType;
  end;

  TIBCSQLInfo = class(TSQLInfo)
  public
    function IdentCase: TIdentCase; override;
  end;

  TGDSRecordSet = class (TCRRecordSet)
  private

    FAutoClose: boolean;
    FFieldXSQLDA: TSQLDA;

    FFieldsAsString: boolean;
    FComplexArrayFields: boolean;
    FDeferredBlobRead: boolean;
    FDeferredArrayRead: boolean;

    FHasFlatUnicodeFields: boolean; //Recordset has non complex unicode fields that are not stored in String Heap
    FFetchBlock: IntPtr;
    FFetchBlockItemSize: integer;

    FFetchedRows: integer;
    FFetchStart: integer;
    FFetchEnd: integer;
    FGCHandle: IntPtr;

    FIsFBConnection: boolean;
    FMinorServerVersion: integer;
    FMajorServerVersion: integer;
    FUseUnicode: boolean;
    FCharLength: integer;
    FSQLDialect: integer;
    FBooleanDomainFields: boolean;
    FSetDomainNames: boolean;

    function GetIsFBConnection: boolean;
    function GetMinorServerVersion: integer;
    function GetMajorServerVersion: integer;
    function GetUseUnicode: boolean;
    function GetCharLength: integer;
    function GetSQLDialect: integer;
    function GetGCHandle: IntPtr;
    function GetGDS: TGDS;

    procedure AllocFetchBlock;
    procedure FreeFetchBlock;
    function GetComplexFldOffset(FieldDesc: TFieldDesc): integer;
    function InternalFetch: boolean;

  protected
    FCommand: TGDSCommand;
    FConnection: TGDSConnection;

    procedure Check(Status: ISC_STATUS);

    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
  { Open/Close }
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;
    procedure InternalInitFields; override;
    procedure ExecFetch(DisableInitFields: boolean); override;
    function GetIndicatorSize: word; override;
  { Fetch }
    function Fetch(FetchBack: boolean = False): boolean; override;
  { Fields }
    function GetArrayFieldName(ObjectType: TObjectType; ItemIndex: integer): _string; override;

    property GCHandle: IntPtr read GetGCHandle;
    //PreCached FConection properties
    property IsFBConnection: boolean read GetIsFBConnection;
    property MinorServerVersion: integer read GetMinorServerVersion;
    property MajorServerVersion: integer read GetMajorServerVersion;
    property UseUnicode: boolean read GetUseUnicode;
    property CharLength: integer read GetCharLength;
    property SQLDialect: integer read GetSQLDialect;    

  public
    constructor Create; override; //CLR requirement
    destructor Destroy; override;

  { Open/Close }
    function IsFullReopen: boolean; override;  
    procedure Reopen; override;
    procedure SetCommandType;
    procedure ExecCommand; override; // Execute command
    procedure Disconnect; override;

  { Filter/Find/Locate/Sorting }
    function CompareFields(RecBuf1: IntPtr; RecBuf2: IntPtr; SortColumn: TSortColumn): integer; overload; override;

  { Fetch }
    procedure FetchAll; override;
    function RowsReturn: boolean; override;

  { Fields }
    function GetFieldDescType: TFieldDescClass; override;
    procedure InitFields; override;
    procedure SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean); override;
    function GetNull(FieldNo: word; RecBuf: IntPtr): boolean; override;

    procedure GetFieldAsVariant(FieldNo: word; RecBuf: IntPtr; var Value: variant); override;

    class function IsComplexFieldType(DataType: word): boolean; override;
    function IsFetchBlockField(DataType: word): boolean;
  { Records }
    procedure CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure CreateComplexField(RecBuf: IntPtr; FieldIndex: integer; WithBlob: boolean); override;
    procedure FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean); override;

  { Navigation }
    procedure SetToEnd; override;

    procedure SetConnection(Value: TCRConnection); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    property GDS: TGDS read GetGDS;
  end;

{$IFNDEF LITE}

{ TGDSMetaData }

  TGDSMetaData = class (TCRMetaData)
  protected
    function CreateRecordSet: TCRRecordSet; override;

    function InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData; override;
    procedure InternalGetMetaDataKindsList(List: _TStringList); override;
    procedure InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string); override;

    function GetTables(Restrictions: _TStrings): TData; override;
    procedure CopyTablesData(Restrictions: _TStrings); virtual;

    function GetColumns(Restrictions: _TStrings): TData; override;
    procedure CopyColumnsData(Restrictions: _TStrings); virtual;

    function GetProcedures(Restrictions: _TStrings): TData; override;
    procedure CopyProceduresData(Restrictions: _TStrings); virtual;

    function GetProcedureParameters(Restrictions: _TStrings): TData; override;
    procedure CopyProcedureParametersData(Restrictions: _TStrings); virtual;

    function GetIndexes(Restrictions: _TStrings): TData; override;
    procedure CopyIndexesData(Restrictions: _TStrings); virtual;

    function GetIndexColumns(Restrictions: _TStrings): TData; override;
    procedure CopyIndexColumnsData(Restrictions: _TStrings); virtual;

    function GetConstraints(Restrictions: _TStrings): TData; override;
    procedure CopyConstraintsData(Restrictions: _TStrings); virtual;

    function GetRoles(Restrictions: _TStrings): TData; override;

    function GetGenerators(Restrictions: _TStrings): TData;

  {$IFNDEF DBX_METAINFO}
    procedure CreateTablesFields; override;
    procedure CreateColumnsFields; override;
    procedure CreateProceduresFields; override;
    procedure CreateProcedureParametersFields; override;
    procedure CreateIndexesFields; override;
    procedure CreateIndexColumnsFields; override;
    procedure CreateConstraintsFields; override;
  {$ENDIF}
  end;

{ TGDSLoader }

  _TIBCInsertMode = (_imInsert, _imUpdateOrInsert);

  TGDSLoaderColumn = class(TCRLoaderColumn)
  private
    FDataTypeName: _string;
  end;

  TGDSLoader = class (TCRLoader)
  private
    FConnection: TGDSConnection;
    FCommand: TGDSCommand;
    FInsertHeader: _string;
    FGeneratedRows: integer;
    FInsertMode: _TIBCInsertMode;
    FRowsPerBatch: integer;

  protected
    class function GetRecordSetClass: TCRRecordSetClass; override;
    procedure SetConnection(Value: TCRConnection); override;

    procedure GenerateSQL(BatchSize: integer);
    procedure FlushRows;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    class function GetColumnClass: TCRLoaderColumnClass; override;
    procedure Reset; override;
    procedure Prepare; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;
  end;

{ TGDSAlerter }

  TGDSAlerterEventCallback = procedure(const EventName: _string; EventCount: integer) of object;

  TGDSAlerterThread = class;

  TGDSAlerter = class(TCRAlerter)
  private
    FThreads: TList;
    FGCHandle: IntPtr;
    FOnEvent: TGDSAlerterEventCallback;

    function GetGCHandle: IntPtr;

  protected
    procedure DoOnEvent(const EventName: _string; EventCount: integer);
    property GCHandle: IntPtr read GetGCHandle;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SendEvent(const EventName, Message: _string); override;
    procedure Start; override;
    procedure Stop; override;

    property OnEvent: TGDSAlerterEventCallback read FOnEvent write FOnEvent;
  end;

{ TGDSAlerterThread }

  TGDSAlerterThread = class(TThread)
  private
    FAlerter: TGDSAlerter;
    FConnection: TGDSConnection;
    FStatusVector: TStatusVector;
    FEventBuffer: IntPtr;
    FEventBufferLen: Integer;
    FResultBuffer: IntPtr;
    FEventIDPtr: IntPtr;
    FEventGroup: Integer;
    FEventCount: Integer;
    FFirstActivation: Boolean;
    FGCHandle: IntPtr;
    FSignal: TEvent;

    procedure RegisterEvents;
    procedure QueueEvents;
    procedure CancelEvents;
    procedure CheckEvents;
    procedure UpdateResultBuffer(Length: SmallInt; Updated: IntPtr);
    procedure Check(Status: ISC_STATUS);

  protected
    procedure Execute; override;

  public
    constructor Create(Alerter: TGDSAlerter; EventGroup: Integer);
    destructor Destroy; override;

    procedure Terminate;
  end;

{$ENDIF}

  TIBCBlob = class(TCompressedBlob)
  private
    FID: PISC_QUAD;
    FHandle: PISC_BLOB_HANDLE;
    FStatusVector: TStatusVector;
    FDbHandle: PISC_DB_HANDLE;
    FTrHandle: PISC_TR_HANDLE;
    FConnection: TGDSConnection;
    FTransaction: TGDSTransaction;

    FInfoReaded: boolean;
    FBlobLength: Cardinal;
    FNumSegments: Cardinal;
    FMaxSegmentSize: Word;

    FNativeHandle: boolean;
    FCached: boolean;

    FBPB: TBytes;
    FBPBLength: integer;

    FParamsChanged: boolean;

    FStreamed: boolean;
    FSubTypeChanged: boolean;
    FConversionSubTypeChanged: boolean;
    FSubType: integer;
    FCharsetID: integer;
    FConversionSubType: integer;
    FConversionCharsetID: integer;

    function GetID: TISC_QUAD;
    function GetHandle: TISC_BLOB_HANDLE;
    function GetDbHandle: TISC_DB_HANDLE;
    function GetTrHandle: TISC_TR_HANDLE;
    procedure SetID(Value: TISC_QUAD);
    procedure SetHandle(Value: TISC_BLOB_HANDLE);
    procedure SetDbHandle(Value: TISC_DB_HANDLE);
    procedure SetTrHandle(Value: TISC_TR_HANDLE);
    procedure SetConnection(Value: TGDSConnection);
    procedure SetTransaction(Value: TGDSTransaction);
    procedure SetSubType(Value: integer);
    procedure SetCharsetID(Value: integer);
    procedure SetConversionSubType(Value: integer);
    procedure SetConversionCharsetID(Value: integer);
    procedure SetStreamed(Value: boolean);
    procedure SetCached(const Value: boolean);
    function GetGDS: TGDS;

  protected
    FNeedReadBlob: boolean;

    procedure Init(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
    function GetIDPtr: PISC_QUAD;
    procedure Check(Status: ISC_STATUS);
    procedure GetBlobInfo;
    procedure CheckValue; override;
    function GetSize: cardinal; override;
    function GetSizeAnsi: cardinal; override;
    procedure CheckAlloc;
    procedure CheckDatabase;
    procedure CheckTransaction;
    function CharSize: Byte;

    procedure CheckInfo;
    procedure CreateBPB;

  public
    constructor Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE); overload;
    constructor Create(Connection: TGDSConnection; Transaction: TGDSTransaction); overload;
    destructor Destroy; override;

    procedure AllocBlob(Read: boolean = True);
    procedure CloseBlob;
    procedure FreeBlob; override;
    procedure Disconnect; override;

    function IsInit: boolean;
    function LengthBlob: longint;

    procedure ReadBlob;
    procedure WriteBlob;

    function Read(Position, Count: cardinal; Dest: IntPtr): cardinal; override;
    procedure Write(Position, Count: cardinal; Source: IntPtr); override;
    procedure Clear; override;
    procedure Truncate(NewSize: cardinal); override;

    property ID: TISC_QUAD read GetID write SetID;
    property Handle: TISC_BLOB_HANDLE read GetHandle write SetHandle;

    property DbHandle: TISC_DB_HANDLE read GetDbHandle write SetDbHandle;
    property TrHandle: TISC_TR_HANDLE read GetTrHandle write SetTrHandle;
    property Connection: TGDSConnection read FConnection write SetConnection;
    property Transaction: TGDSTransaction read FTransaction write SetTransaction;
    property GDS: TGDS read GetGDS;

    property SubType: integer read FSubType write SetSubType;
    property CharsetID: integer read FCharsetID write SetCharsetID;
    property ConversionSubType: integer read FSubType write SetConversionSubType;
    property ConversionCharsetID: integer read FConversionCharsetID write SetConversionCharsetID;

    property NumSegments: Cardinal read FNumSegments;
    property MaxSegmentSize: Word read FMaxSegmentSize;

    property Streamed: boolean read FStreamed write SetStreamed;
    property Cached: boolean read FCached write SetCached;
  end;

  function Reverse2(Value: word): Word;
  function Reverse4(Value: cardinal): cardinal;

  function XSQLDA_LENGTH(n: Long; XSQLVARType: TXSQLVARType): Long;

  procedure DateTimeToSQLTimeStamp(DateTime: TDateTime; Buf: IntPtr);
  procedure DateTimeToSQLDate(DateTime: TDateTime; Buf: IntPtr);
  procedure DateTimeToSQLTime(DateTime: TDateTime; Buf: IntPtr);
  procedure SQLTimeStampToDateTime(ib_date: IntPtr; Buf: IntPtr);
  procedure SQLDateToDateTime(ib_date: IntPtr; Buf: IntPtr);
  procedure SQLTimeToDateTime(ib_time: IntPtr; Buf: IntPtr);

  //procedure SetStrTerminator(Buf: IntPtr; Size, CharLen: integer);

{BCD converting support}
{$IFDEF VER6P}
  function DBDecimalToBcd(Value: int64; Scale: integer): TBcd;
{$ENDIF}

  function GetFullDatabaseName(const Host: string; Protocol: _TIBCProtocol; const FileName: string): string;
  function GetDBNamePos(const Database: string): integer;
  procedure ParseDatabaseName(const Database: string; var Host: string; var Protocol: _TIBCProtocol;
    var FileName: string);

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  procedure AllocIBDACWnd;
{$ENDIF}
{$ENDIF}

var
  IntegerPrecision: integer = 10;
  FloatPrecision: integer   = 16;
  IBCSQLInfo: TIBCSQLInfo;
  DisableReturningKeyword: boolean = False;

implementation

uses
  DAConsts,
{$IFNDEF UNIDACPRO}
  IBCConsts, IBCParser, IBCArray,
{$ELSE}
  IBCConstsUni, IBCParserUni, IBCArrayUni,
{$ENDIF}
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text, System.Threading,
{$ENDIF}
{$IFNDEF VER6P}
  ComObj,
{$ENDIF}
  Math;

const
  BooleanIntTypes = [dtSmallint, dtInteger, dtInt64];

{$IFDEF MSWINDOWS}
const
  WM_EVENTRECIEVED = WM_USER + 1;
{$ENDIF}

{$IFNDEF LITE}
var
{$IFDEF MSWINDOWS}
  hIBDACWindow: HWND = 0;
{$ENDIF}
  ISCCallbackPtr: IntPtr;
{$IFDEF CLR}
  HISCCallback: GCHandle;
  ISCCallbackCode: array[0..24] of byte =
    ($55,
    $8B, $EC,

    $8B, $45, $10, $50,
    $66, $8B, $45, $0C, $50,
    $8B, $45, $8, $50,

    $B8, $00, $00, $00, $00,
    $FF, $D0,

    $5D,
    $C3);

  ISCCallbackRec: packed record
    Ptr: TISC_Callback;
  end;
  CallbackRecPtr: IntPtr;
{$ENDIF}

type
  TAlertEvent = class
  public
    Name: _string;
    Count: integer;
  end;

{$ENDIF}

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
function WndProc(hWindow: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; {$IFNDEF CLR} stdcall; {$ENDIF}
var
  Alerter: TGDSAlerter;
  Event: TAlertEvent;
begin
  Result := 0;
  try
    case Msg of
      WM_EVENTRECIEVED: begin
        Alerter := TGDSAlerter(GetGCHandleTarget(IntPtr(wParam)));
        Event := TAlertEvent(GetGCHandleTarget(IntPtr(lParam)));
        FreeGCHandle(IntPtr(lParam));
        try
          Alerter.DoOnEvent(Event.Name, Event.Count);
        finally
          Event.Free;
        end;
      end;
    else
      Result := DefWindowProc(hWindow, Msg, wParam, lParam);
    end;
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(nil);
  end;
end;

var
  WndClass: TWndClass = (
    style: 0;
    lpfnWndProc: @WndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: '';
    lpszClassName: 'IBDACUtilWnd'
  );

procedure AllocIBDACWnd;
var
  TempClass: {$IFDEF CLR}TWndClassInfo{$ELSE}TWndClass{$ENDIF};
  ClassRegistered: boolean;
{$IFDEF CLR}
  WndClassPtr: IntPtr;
{$ENDIF}
  WndProcPtr: IntPtr;
begin
  if hIBDACWindow = 0 then begin
    WndClass.hInstance := HInstance;
    ClassRegistered := Windows.GetClassInfo(HInstance, WndClass.lpszClassName,
      TempClass);
  {$IFDEF CLR}
    WndClassPtr := Marshal.AllocHGlobal(SizeOf(TWndClass));
    try
      Marshal.StructureToPtr(WndClass, WndClassPtr, False);
      WndProcPtr :=  Marshal.ReadIntPtr(WndClassPtr, 4);
    finally
      Marshal.FreeHGlobal(WndClassPtr);
    end;
  {$ELSE}
    WndProcPtr := @WndProc;
  {$ENDIF}

    if not ClassRegistered or ({$IFDEF FPC}@{$ENDIF}TempClass.lpfnWndProc <> WndProcPtr) then begin
      if ClassRegistered then
        Windows.UnregisterClass(WndClass.lpszClassName, HInstance);

      hIBDACWindow := Windows.RegisterClass(WndClass);
    end;

    hIBDACWindow := Windows.CreateWindowEx(WS_EX_TOOLWINDOW, 'IBDACUtilWnd',
      '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
    Assert(hIBDACWindow <> 0);
    Windows.SetWindowLong(hIBDACWindow, GWL_WNDPROC, Longint(WndProcPtr));
  end;
end;
{$ENDIF}
{$ENDIF}

{ TGDSDatabaseInfo }

constructor TGDSDatabaseInfo.Create;
begin
  inherited Create;

  FUserNames := TStringList.Create;
  FBackoutCount := nil;
  FDeleteCount := nil;
  FExpungeCount := nil;
  FInsertCount := nil;
  FPurgeCount := nil;
  FReadIdxCount := nil;
  FReadSeqCount := nil;
  FUpdateCount := nil;
  FActiveTransactions := nil;
end;

destructor TGDSDatabaseInfo.Destroy;
begin
  FActiveTransactions.Free;
  FUserNames.Free;
  FBackoutCount.Free;
  FDeleteCount.Free;
  FExpungeCount.Free;
  FInsertCount.Free;
  FPurgeCount.Free;
  FReadIdxCount.Free;
  FReadSeqCount.Free;
  FUpdateCount.Free;
  inherited Destroy;
end;

procedure TGDSDatabaseInfo.CheckConnection;
begin
  if FGDSConnection = nil then
    RaiseError(SConnectionNotDefined);
end;

procedure TGDSDatabaseInfo.GetDatabaseInfo(LocalBuffer: IntPtr; DbInfoCommand: IntPtr);
var
  Res: ISC_STATUS;
begin
  FGDSConnection.GDS.Busy;
  Res := FGDSConnection.GDS.isc_database_info(FGDSConnection.FStatusVector,
    FGDSConnection.FDatabaseHandle, 1, DbInfoCommand, LocalBufferLen, LocalBuffer);
  FGDSConnection.GDS.Release;
  FGDSConnection.Check(Res);
end;

function TGDSDatabaseInfo.GetDBFileName: string;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_db_id);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Marshal.WriteByte(PtrOffset(LocalBuffer, 5 + Marshal.ReadByte(LocalBuffer, 4)), 0);
    Result := string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, 5)));
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetDBSiteName: string;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
  Length: integer;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_db_id);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Length := Marshal.ReadByte(PtrOffset(LocalBuffer, 5 + Integer(Marshal.ReadByte(LocalBuffer, 4))));
    Result := string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, 6 + Marshal.ReadByte(LocalBuffer, 4)), Length));
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetIsRemoteConnect: boolean;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_db_id);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result := Marshal.ReadByte(LocalBuffer, 3) = 4;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetDBImplementationNo: integer;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_implementation);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result := Marshal.ReadByte(LocalBuffer, 3);
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetDBImplementationClass: integer;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_implementation);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result := Marshal.ReadByte(LocalBuffer, 4);
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetVersion(DatabaseInfoCommand: integer): string;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, DatabaseInfoCommand);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    if Marshal.ReadByte(LocalBuffer, 0) = DatabaseInfoCommand then
      Result := string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, 5),
        Marshal.ReadByte(LocalBuffer, 4)))
    else
      Result := '';
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetUserNames: TStringList;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
  i, UserLength: integer;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, isc_info_user_names);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result := FUserNames;
    FUserNames.Clear;
    i := 0;
    while Marshal.ReadByte(LocalBuffer, i) = isc_info_user_names do
    begin
      Inc(i, 3);
      UserLength := Marshal.ReadByte(LocalBuffer, i);
      Inc(i, 1);
      Result.Add(string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, i), UserLength)));
      Inc(i, UserLength);
    end;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetBackoutCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_backout_count, FBackoutCount);
end;

function TGDSDatabaseInfo.GetDeleteCount: TstringList;
begin
  Result := GetOperationCounts(isc_info_delete_count, FDeleteCount);
end;

function TGDSDatabaseInfo.GetExpungeCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_expunge_count, FExpungeCount);
end;

function TGDSDatabaseInfo.GetInsertCount: TstringList;
begin
  Result := GetOperationCounts(isc_info_insert_count, FInsertCount);
end;

function TGDSDatabaseInfo.GetPurgeCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_purge_count, FPurgeCount);
end;

function TGDSDatabaseInfo.GetReadIdxCount: TstringList;
begin
  Result := GetOperationCounts(isc_info_read_idx_count, FReadIdxCount);
end;

function TGDSDatabaseInfo.GetReadSeqCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_read_seq_count, FReadSeqCount);
end;

function TGDSDatabaseInfo.GetUpdateCount: TStringList;
begin
  Result := GetOperationCounts(isc_info_update_count, FUpdateCount);
end;

function TGDSDatabaseInfo.GetStringDatabaseInfo(DatabaseInfoCommand: integer): string;
var
  DbInfoCommand: IntPtr;
  LocalBuffer: IntPtr;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, DatabaseInfoCommand);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Marshal.WriteByte(PtrOffset(LocalBuffer, 4 + Marshal.ReadByte(LocalBuffer, 3)), 0);
    Result := string(Marshal.PtrToStringAnsi(PtrOffset(LocalBuffer, 4),
      Marshal.ReadByte(PtrOffset(LocalBuffer, 4 + Marshal.ReadByte(PtrOffset(LocalBuffer, 3))))));
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DBInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetBooleanDatabaseInfo(DatabaseInfoCommand: integer): boolean;
begin
  if DatabaseInfoCommand = isc_info_db_read_only then
    if (ODSMajorVersion < 10) then begin
      Result := False;
      Exit;
    end;
  Result := Byte(GetIntDatabaseInfo(DatabaseInfoCommand)) <> 0;
end;

function TGDSDatabaseInfo.GetIntDatabaseInfo(DatabaseInfoCommand: integer): integer;
var
  DbInfoCommand: IntPtr;
  LocalBuffer: IntPtr;
begin
  Result := -1;
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, DatabaseInfoCommand);
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    if (DatabaseInfoCommand = isc_info_db_SQL_dialect) and (Marshal.ReadByte(LocalBuffer, 0) <> isc_info_db_SQL_dialect) then
      Result := 1
    else
    if Marshal.ReadByte(LocalBuffer, 0) <> DatabaseInfoCommand then
      Result := 0
    else
    if (DatabaseInfoCommand = isc_info_base_level) then
      Result := Marshal.ReadByte(LocalBuffer, 4)
    else
    case Marshal.ReadInt16(LocalBuffer, 1) of
      0: Result := 0;
      1: Result := Marshal.ReadByte(LocalBuffer, 3);
      2: Result := Marshal.ReadInt16(LocalBuffer, 3);
      4: Result := Marshal.ReadInt32(LocalBuffer, 3);
      8: Result := Integer(Marshal.ReadInt64(LocalBuffer, 3));
      else
        RaiseError('Unsupported length');
    end;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DbInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetActiveTransactions: TStringList;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
  i: integer;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, frb_info_active_transactions);
    if FActiveTransactions = nil then
      FActiveTransactions := TStringList.Create;
    Result := FActiveTransactions;
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    Result.Clear;
    i := 0;
    while Marshal.ReadByte(LocalBuffer, i) = frb_info_active_transactions do begin
      Inc(i, 3);
      Result.Add(IntToStr(Marshal.ReadInt32(LocalBuffer, i)));
      Inc(i, 4);
    end;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DBInfoCommand);
  end;
end;

function TGDSDatabaseInfo.GetOperationCounts(DataBaseInfoCommand: integer; FOperation: TStringList): TStringList;
var
  LocalBuffer: IntPtr;
  DbInfoCommand: IntPtr;
  i, QtdTables, IdTable, QtdOperations: integer;
begin
  CheckConnection;
  LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
  DbInfoCommand := Marshal.AllocHGlobal(SizeOf(Byte));
  try
    Marshal.WriteByte(DbInfoCommand, DataBaseInfoCommand);
    if FOperation = nil then
      FOperation := TStringList.Create;
    Result := FOperation;
    GetDatabaseInfo(LocalBuffer, DbInfoCommand);
    FOperation.Clear;
    QtdTables := Marshal.ReadInt16(LocalBuffer, 1) div 6;
    for i := 0 to QtdTables - 1 do
    begin
      IdTable := Marshal.ReadInt16(LocalBuffer, 3 + (i * 6));
      QtdOperations := Marshal.ReadInt32(LocalBuffer, 5 + (i * 6));
      FOperation.Add(IntToStr(IdTable) + '=' + IntToStr(QtdOperations));
    end;
  finally
    Marshal.FreeHGlobal(LocalBuffer);
    Marshal.FreeHGlobal(DBInfoCommand);
  end;
end;

{ TGDSConnection }

constructor TGDSConnection.Create;
begin
  inherited Create;

  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FDatabaseHandle := Marshal.AllocHGlobal(SizeOf(TISC_DB_HANDLE));
  Marshal.WriteIntPtr(FDatabaseHandle, nil);
  SetLength(FDPB, 0);
  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
  FDatabaseInfo := TGDSDatabaseInfo.Create;
  FDatabaseInfo.Owner := Self;

  FQueryCharLength := True;  
  FSQLDialect := 3;
  FlushProps;
end;

destructor TGDSConnection.Destroy;
begin
  Disconnect;

  FParams.Free;
  FDatabaseInfo.Free;

  SetLength(FDPB, 0);
  Marshal.FreeHGlobal(FDatabaseHandle);
  Marshal.FreeHGlobal(FStatusVector);

  inherited;
end;

procedure TGDSConnection.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    IBCError(FStatusVector, True, Component);
end;

procedure TGDSConnection.IBCError(var StatusVector: TStatusVector; UseCallback: boolean; Component: TObject);
var
  Msg, SqlErrMsg: _string;
  ErrorNumber, ErrorCode: integer;
  Fail: boolean;
begin
  GDS.Busy;
  ErrorCode := GDS.isc_sqlcode(StatusVector);
  GDS.Release;
  GDS.GetIBError(ErrorCode, FUseUnicode, ErrorNumber, StatusVector, Msg, SqlErrMsg);
  FLastError := ErrorCode;
  try
    raise EIBCError.Create(FLastError, ErrorNumber, Msg, SqlErrMsg);
  except
    on E: EIBCError do begin
    {$IFNDEF LITE}
      E.Component := Component;
    {$ENDIF}
      Fail := True;
      if UseCallback then begin
        GDS.AlerterFatalError := GDS.AlerterFatalError and EIBCError.IsFatalError(ErrorNumber);
        try
          DoError(E, Fail);
        finally
          GDS.AlerterFatalError := False;
        end;
      end;
      if Fail then
        raise
      else
        Abort;
    end;
  end;
end;

procedure TGDSConnection.DoError(E: Exception; var Fail: boolean);
begin
  inherited; //for D8
end;

procedure TGDSConnection.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TGDSConnection.ParamsChanging(Sender: TObject);
begin
  CheckInactive;
end;

procedure TGDSConnection.CheckInactive;
begin
  if FConnected then
    RaiseError(SConnectionOpen);
end;

procedure TGDSConnection.CreateDPB;
const
  DPBPrefix = 'isc_dpb_';
  DPBConstantNames: array[1..isc_dpb_last_dpb_constant] of string = (
    'cdd_pathname',
    'allocation',
    'journal',
    'page_size',
    'num_buffers',
    'buffer_length',
    'debug',
    'garbage_collect',
    'verify',
    'sweep',
    'enable_journal',
    'disable_journal',
    'dbkey_scope',
    'number_of_users',
    'trace',
    'no_garbage_collect',
    'damaged',
    'license',
    'sys_user_name',
    'encrypt_key',
    'activate_shadow',
    'sweep_interval',
    'delete_shadow',
    'force_write',
    'begin_log',
    'quit_log',
    'no_reserve',
    'user_name',
    'password',
    'password_enc',
    'sys_user_name_enc',
    'interp',
    'online_dump',
    'old_file_size',
    'old_num_files',
    'old_file',
    'old_start_page',
    'old_start_seqno',
    'old_start_file',
    'drop_walfile',
    'old_dump_id',
    'wal_backup_dir',
    'wal_chkptlen',
    'wal_numbufs',
    'wal_bufsize',
    'wal_grp_cmt_wait',
    'lc_messages',
    'lc_ctype',
    'cache_manager',
    'shutdown',
    'online',
    'shutdown_delay',
    'reserved',
    'overwrite',
    'sec_attach',
    'disable_wal',
    'connect_timeout',
    'dummy_packet_interval',
    'gbak_attach',
    'sql_role_name',
    'set_page_buffers',
    'working_directory',
    'sql_dialect',
    'set_db_readonly',
    'set_db_sql_dialect',
    'gfix_attach',
    'gstat_attach',
    'gbak_ods_version',
    'gbak_ods_minor_version',
    'set_group_commit',
    'gbak_validate',
    'client_interbase_var',
    'admin_option',
    'process_name',
    'instance_name'
  );

  procedure CheckDPBSize (NeededSize: integer; var DPBSize: integer);
  begin
    if DPBSize < NeededSize then begin
      DPBSize := ((NeededSize div 2048) + 1) * 2048;
      SetLength(FDPB, DPBSize);
    end;
  end;

var
  i, j: integer;
  DPBVal: UShort;
  ParamName, ParamValue: string;
  DPBSize: integer;
  vCharset: string;
begin
  DPBSize := 2048; //Start size that must cover common user needs
  SetLength(FDPB, DPBSize);
  FDPBLength := 1;
  FDPB[0] := isc_dpb_version1;
  if FUseUnicode and (UpperCase(FCharset) <> 'UTF8') then
    vCharset := 'UNICODE_FSS'
  else
    vCharset := FCharset;
  try
    CheckDPBSize(FDPBLength + 6 + Length(vCharset) + Length(FPassword) + Length(FUsername), DPBSize);
    if FUsername <> '' then begin
      FDPB[FDPBLength] := isc_dpb_user_name;
      FDPB[FDPBLength + 1] := Length(FUsername);
      Encoding.Default.GetBytes(FUsername, 0, Length(FUsername), FDPB, FDPBLength + 2);
      Inc(FDPBLength, 2 + Length(FUsername));
    end;

    if FPassword <> '' then begin
      FDPB[FDPBLength] := isc_dpb_password;
      FDPB[FDPBLength + 1] := Length(FPassword);
      Encoding.Default.GetBytes(FPassword, 0, Length(FPassword), FDPB, FDPBLength + 2);
      Inc(FDPBLength, 2 + Length(FPassword));
    end;

    if vCharset <> '' then begin
      FDPB[FDPBLength] := isc_dpb_lc_ctype;
      FDPB[FDPBLength + 1] := Length(vCharset);
      Encoding.Default.GetBytes(vCharset, 0, Length(vCharset), FDPB, FDPBLength + 2);
      Inc(FDPBLength, 2 + Length(vCharset));
    end;

    if FRole <> '' then begin
      FDPB[FDPBLength] := isc_dpb_sql_role_name;
      FDPB[FDPBLength + 1] := Length(FRole);
      Encoding.Default.GetBytes(FRole, 0, Length(FRole), FDPB, FDPBLength + 2);
      Inc(FDPBLength, 2 + Length(FRole));
    end;

    for i := 0 to FParams.Count - 1 do begin
      if (Trim(FParams.Names[i]) = '') then
        continue;
      ParamName := LowerCase(FParams.Names[i]);
      ParamValue := Copy(FParams[i], Pos('=', FParams[i]) + 1, Length(FParams[i]));
      if (Pos(DPBPrefix, ParamName) = 1) then
        Delete(ParamName, 1, Length(DPBPrefix));
      DPBVal := 0;
      { Find the parameter }
      for j := 1 to isc_dpb_last_dpb_constant do
        if (ParamName = DPBConstantNames[j]) then begin
          DPBVal := j;
          break;
        end;
      case DPBVal of
        isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
        isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
        isc_dpb_lc_messages, isc_dpb_lc_ctype,
        isc_dpb_sql_role_name, isc_dpb_process_name: begin
          CheckDPBSize(FDPBLength + 2 + Length(ParamValue), DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := Length(ParamValue);
          Encoding.Default.GetBytes(ParamValue, 0, Length(ParamValue), FDPB, FDPBLength + 2);
          Inc(FDPBLength, 2 + Length(ParamValue));
        end;
        isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
        isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify, isc_dpb_sql_dialect:  begin
          CheckDPBSize(FDPBLength + 3, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 1;
          FDPB[FDPBLength + 2] := Byte(StrToInt(ParamValue));
          Inc(FDPBLength, 3);
        end;
        isc_dpb_sweep: begin
          CheckDPBSize(FDPBLength + 3, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 1;
          FDPB[FDPBLength + 2] := isc_dpb_records;
          Inc(FDPBLength, 3);
        end;
        isc_dpb_sweep_interval: begin
          CheckDPBSize(FDPBLength + 6, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 4;
          j := StrToInt(ParamValue);
          FDPB[FDPBLength + 2] := Byte(j);
          FDPB[FDPBLength + 3] := Byte(j shr 8);
          FDPB[FDPBLength + 4] := Byte(j shr 16);
          FDPB[FDPBLength + 5] := Byte(j shr 24);
          Inc(FDPBLength, 6);
        end;
        isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
        isc_dpb_quit_log:
        begin
          CheckDPBSize(FDPBLength + 3, DPBSize);
          FDPB[FDPBLength] := DPBVal;
          FDPB[FDPBLength + 1] := 1;
          FDPB[FDPBLength + 2] := 0;
          Inc(FDPBLength, 3);
        end;
        else begin
          if (DPBVal > 0) and
             (DPBVal <= isc_dpb_last_dpb_constant) then
            RaiseError(Format(SDPBConstantNotSupported, [DPBConstantNames[DPBVal]]))
          else
            RaiseError(Format(SDPBConstantUnknown,[ParamName]));
        end;
      end;
    end;  
  except
    SetLength(FDPB, 0);
    FDPBLength := 0;
    raise;
  end;
end;

function TGDSConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TGDSCommand;
end;

function TGDSConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TGDSTransaction;
end;

procedure TGDSConnection.Connect(const ConnectString: _string);
var
  FullDBName: AnsiString;
  pName: IntPtr;
  Res: ISC_STATUS;
begin
  if not FConnected then begin
    FGDS := GDSList.GetGDS(FClientLibrary);

    if FParamsChanged then begin
      CreateDPB;
      FParamsChanged := False;
    end;

    FullDBName := AnsiString(GetFullDatabaseName(FServer, FProtocol, FDatabase));
    pName := Marshal.StringToHGlobalAnsi(FullDBName);
    try
      try
        GDS.Busy;
        Res := GDS.isc_attach_database(FStatusVector, Length(FullDBName), pName, FDatabaseHandle, FDPBLength, FDPB);
        GDS.Release;
        Check(Res);
      except
        on EFailOver do; //This exception raised after FailOver Reconnect in DoError so DBHandle is valid
        else begin
          //Check(isc_detach_database(FStatusVector, FDatabaseHandle));
          Marshal.WriteIntPtr(FDatabaseHandle, nil);
          raise;
        end;
      end;
    finally
      Marshal.FreeCoTaskMem(pName);
    end;

    inherited;
    FConnected := True;

    if FDisconnectedMode then begin
      //Clean cached values on connection to obtain current values from DB
      FServerVersion := '';
      FServerVersionFull := '';
      FMajorServerVersion := 0;
      FMinorServerVersion := -1;
      FReadOnlyCahed := False;
      FIsFBServerCached := False;
    end;

    CheckClientSQLDialect;
    // get connection parameters
    //if (FCharset <> '') or FQueryCharLength then
    GetDatabaseParameters;

  end;
end;

procedure TGDSConnection.CheckClientSQLDialect;
begin
  FDBSQLDialect := FDatabaseInfo.DBSQLDialect;
  if (FDBSQLDialect < FSQLDialect) then
    FSQLDialect := FDBSQLDialect;
end;

procedure TGDSConnection.Disconnect;
var
  Res: ISC_STATUS;
begin
  if FConnected then begin
    FConnected := False;

    try
      if FNativeConnection and not GDS.AlerterFatalError then begin
        GDS.Busy;
        Res := GDS.isc_detach_database(FStatusVector, FDatabaseHandle);
        GDS.Release;
        Check(Res);
      end;
    finally
      Marshal.WriteIntPtr(FDatabaseHandle, nil);
      FNativeConnection := True;
      if not FDisconnectedMode then
        FlushProps;
    end;
  end;
end;

function TGDSConnection.CreateCommand: TGDSCommand;
begin
  Result := TGDSCommand.Create;
  Result.SetConnection(Self);
  Result.SetTransaction(Self.GetInternalTransaction);
end;

{ Connect control }
procedure TGDSConnection.AssignConnect(Source: TCRConnection);
var
  Src: TGDSConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TGDSConnection(Source);
      FCharset := Src.FCharset;
      FCharsetId := Src.FCharsetId;
      FDBSQLDialect := Src.FDBSQLDialect;
      FUseUnicode := Src.FUseUnicode;
      FClientLibrary := Src.FClientLibrary;

      SetDatabaseHandle(Src.GetDatabaseHandle);
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
    end
    else
      SetDatabaseHandle(nil);
  end;
end;

function TGDSConnection.GetLastError: integer;
begin
  Result := FLastError;
end;

procedure TGDSConnection.SetLastError(Value: integer);
begin
  FLastError := Value;
end;

procedure TGDSConnection.GetDatabaseParameters;
var
  SQL: _string;
  RecordSet: TGDSRecordSet;
  RecBuf: IntPtr;
  v: variant;
  CharsetIdNeeded: boolean;
  vCharset: _string;

begin
  if FConnected then begin
    if FUseUnicode and (UpperCase(FCharset) <> 'UTF8') then
      vCharset := 'UNICODE_FSS'
    else
      vCharset := FCharset;
    CharsetIdNeeded := vCharset <> '';
    RecordSet := TGDSRecordSet.Create;
    try
      RecordSet.SetConnection(Self);
      RecordSet.SetTransaction(FInternalTransaction);
      FInternalTransaction.StartTransaction;
      SQL := 'SELECT CST.RDB$CHARACTER_SET_ID CONN_CH, CST.RDB$BYTES_PER_CHARACTER CONN_CH_LEN,' + #13#10 +
        'DB.RDB$CHARACTER_SET_NAME DB_CH, DB_CST.RDB$BYTES_PER_CHARACTER DB_CH_LEN, ' +
        'DB_CST.RDB$CHARACTER_SET_ID DB_CH_ID' + #13#10 +
        'FROM RDB$CHARACTER_SETS CST, RDB$DATABASE DB, RDB$CHARACTER_SETS DB_CST' + #13#10 +
        'WHERE (DB_CST.RDB$CHARACTER_SET_NAME = DB.RDB$CHARACTER_SET_NAME)';
        if CharsetIdNeeded then
          SQL := SQL + #13#10 + 'AND (CST.RDB$CHARACTER_SET_NAME = UPPER(''' + vCharset + '''))'
        else
          SQL := SQL + #13#10 + 'AND (CST.RDB$CHARACTER_SET_NAME = DB.RDB$CHARACTER_SET_NAME)';
      RecordSet.SetSQL(SQL);
      RecordSet.SetProp(prFlatBuffers, False);
      RecordSet.SetProp(prLongStrings, True);
      RecordSet.Open;
      RecordSet.FetchAll;
      RecordSet.SetToBegin;

      if RecordSet.RecordCount = 0 then
        Exit;
      Assert(RecordSet.RecordCount = 1, 'GetDatabaseParameters');

      RecBuf := nil;
      RecordSet.AllocRecBuf(RecBuf);
      try
        while True do begin
          RecordSet.GetNextRecord(RecBuf);
          if RecordSet.Eof then
            Break;
          if not RecordSet.GetNull(3, RecBuf) then begin
            if FQueryCharLength then begin
              RecordSet.GetFieldAsVariant(2, RecBuf, v); //Connection Charset Bytes
              FCharLength := v;
            end;
            if CharsetIdNeeded then begin
              RecordSet.GetFieldAsVariant(1, RecBuf, v); //Connection Charset ID
              FCharsetId := v
            end;
            RecordSet.GetFieldAsVariant(4, RecBuf, v); //Database Charset Bytes
            FDBCharLength := v;
            //RecordSet.GetFieldAsVariant(3, RecBuf, v); //Database Charset Name
            RecordSet.GetFieldAsVariant(5, RecBuf, v); //Database Charset ID
            FDBCharsetId := v;
          end
          else begin
            FCharsetId := 0;
            FCharLength := 1;
          end;
        end;
      finally
        if RecBuf <> nil then
          RecordSet.FreeRecBuf(RecBuf);
      end;
    finally
      RecordSet.Free;
      FInternalTransaction.Commit;
    end;
  end;
end;

function TGDSConnection.GetCharLength(CharsetID: integer): integer;
begin
  case CharsetID of
    5, 6, 8, 44, 56, 57, 64: Result := 2;
    3: Result := 3;
    4, 59: Result := 4;
  else
    Result := 1;
  end;
end;

procedure TGDSConnection.FlushProps;
begin
  FDBSQLDialect := 3;
  FCharsetId := 0;
  FServerVersion := '';
  FServerVersionFull := '';
  FMajorServerVersion := 0;
  FMinorServerVersion := -1;
  FReadOnlyCahed := False;
  FIsFBServerCached := False;
  if FQueryCharLength then
    FCharLength := 0;
end;

function TGDSConnection.SetProp(Prop: integer; const Value: variant): boolean;
var
  OldCharsetId: word;
  OldCharset: string;
  S: string;
begin
  Result := True;
  case Prop of
    prUseUnicode: begin
      FUseUnicode := Boolean(Value);
      FQueryCharLength :=  True;
      FParamsChanged := True;
    end;
    prRole:
      if FRole <> string(Value) then begin
        FRole := string(Value);
        FParamsChanged := True;
      end;
    prEnableMemos:
      FEnableMemos := Boolean(Value);
    prOptimizedNumerics:
      FOptimizedNumerics := Boolean(Value);
    prEnableLargeint:
      FEnableLargeint := Boolean(Value);
    prSQLDialect:
      FSQLDialect := Integer(Value);
    prCharLength:
      if (FQueryCharLength and (Word(Value) <> 0))
        or (FCharLength <> Word(Value))
      then begin
        FCharLength := Word(Value);
        FQueryCharLength := FCharLength = 0;
        if FCharLength = 0 then
          GetDatabaseParameters;
      end;
    prCharset: begin
      S := Value;
      if FCharset <> S then begin
        OldCharset := FCharset;
        OldCharsetId := FCharsetId;
        FCharset := S;
        FParamsChanged := True;
        try
          GetDatabaseParameters;
        except
          FCharset := OldCharset;
          FCharsetId := OldCharsetId;
          raise;
        end;
      end;
    end;
    prClientLibrary:
      FClientLibrary := Value;
    prSimpleNumericMap:
      FSimpleNumericMap := Value;
    prDatabase:
      FDatabase := Value;
    prProtocol:
      FProtocol := _TIBCProtocol(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prUseUnicode:
      Value := FUseUnicode;
    prRole:
      Value := FRole;
    prCharLength:
      if FQueryCharLength then
        Value := 0
      else
        Value := FCharLength;
    prCharset:
      Value := FCharset;
    prEnableMemos:
      Value := FEnableMemos;
    prSQLDialect:
      Value := FSQLDialect;
    prDBSQLDialect:
      Value := FDBSQLDialect;
    prReadOnly: begin
      if not FReadOnlyCahed then begin
        FReadOnly := FDatabaseInfo.ReadOnly;
        FReadOnlyCahed := True;
      end;
      Value := FReadOnly;
    end;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TGDSConnection.SetUsername(const Value: _string);
begin
  if Value <> FUsername then
    FParamsChanged := True;
  inherited SetUsername(Value);
end;

procedure TGDSConnection.SetPassword(const Value: _string);
begin
  if Value <> FPassword then
    FParamsChanged := True;
  inherited SetPassword(Value);
end;

function TGDSConnection.CheckIsValid: boolean;
begin
  if not FIsValid then
    Result := False
  else begin
    FIsValid := True;
    try
      if FDatabaseInfo.BaseLevel = 0 then
        ;
    except
      FIsValid := False;
    end;
    Result := FIsValid;
  end;
end;

function TGDSConnection.CanChangeDatabase: boolean;
begin
  Result := False; 
end;

{ InterBase support}
function TGDSConnection.GetDatabaseHandle: TISC_DB_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FDatabaseHandle);
end;

procedure TGDSConnection.SetDatabaseHandle(Value: TISC_DB_HANDLE);
begin
  if Value <> GetDatabaseHandle then begin
    if FNativeConnection then
      CheckInactive
    else
      Disconnect;

    Marshal.WriteIntPtr(FDatabaseHandle, Value);
    FNativeConnection := Value = nil;
    FConnected := Value <> nil;

    if (Value <> nil) and (FGDS = nil) then
      FGDS := GDSList.GetGDS(FClientLibrary);
  end;
end;

procedure TGDSConnection.CreateDatabase;
var
  tr_handle: PISC_TR_HANDLE;
  PParams: IntPtr;
  InSQLDA: TSQLDA;
  FullDBName: string;
  Res: ISC_STATUS;
begin
  CheckInactive;
  if FGDS = nil then
    FGDS := GDSList.GetGDS(FClientLibrary);
  FullDBName := GetFullDatabaseName(FServer, FProtocol, FDatabase);
  PParams := Marshal.StringToHGlobalAnsi(AnsiString('CREATE DATABASE ''' + FullDBName + ''' ' + FParams.Text));
  if GDS.Version >= 7 then
    InSQLDA := TSQLDA.Create(Self, vtGDS7)
  else
    InSQLDA := TSQLDA.Create(Self, vtGDS);
  InSQLDA.AllocSQLDA(0);
  tr_handle := Marshal.AllocHGlobal(SizeOf(TISC_TR_HANDLE));
  try
    Marshal.WriteIntPtr(tr_handle, nil);
    GDS.Busy;
    Res := GDS.isc_dsql_execute_immediate(FStatusVector, FDatabaseHandle,
             tr_handle, 0, PParams, FSQLDialect, InSQLDA.FXSQLDA);
    GDS.Release;
    Check(Res);
    FDBSQLDialect := FDatabaseInfo.DBSQLDialect;
    FConnected := True;
  finally
    Marshal.FreeCoTaskMem(PParams);
    InSQLDA.Free;
    Marshal.FreeHGlobal(tr_handle);
  end;
end;

procedure TGDSConnection.DropDatabase;
var
  Res: ISC_STATUS;
begin
  if not FConnected then
    RaiseError(SConnectionClosed);

  try
    GDS.Busy;
    Res := GDS.isc_drop_database(FStatusVector, FDatabaseHandle);
    GDS.Release;
    Check(Res);
  finally
    Marshal.WriteIntPtr(FDatabaseHandle, nil);
    FNativeConnection := True;
    FConnected := False;
  end;
end;

procedure TGDSConnection.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

function TGDSConnection.GetParams: TStrings;
begin
  Result := FParams;
end;

function TGDSConnection.IsFBServer: boolean;
begin
  if not FIsFBServerCached then begin
    FIsFBServer := FDatabaseInfo.FBVersion <> '';
    FIsFBServerCached := True;
  end;
  Result := FIsFBServer;
end;

function TGDSConnection.GetMajorServerVersion: integer;
begin
  if FServerVersion = '' then
    GetServerVersion;

  Result := FMajorServerVersion;
end;

function TGDSConnection.GetMinorServerVersion: integer;
begin
  if FServerVersion = '' then
    GetServerVersion;

  Result := FMinorServerVersion;
end;

function TGDSConnection.GetServerVersion: _string;
var
  DotPos, StartPos, EndPos: integer;
begin
  if FServerVersion = '' then begin
    if FServerVersionFull = '' then
      GetServerVersionFull;

    if Length(FServerVersionFull) >= 7 then begin
      DotPos := Pos('.', FServerVersionFull);
      StartPos := DotPos - 1;
      EndPos := Pos(' ', FServerVersionFull) - 1;
      if EndPos <= 0 then
        EndPos := Length(FServerVersionFull);
      if (StartPos > 1) and (EndPos >= StartPos) then begin
        if AnsiChar(FServerVersionFull[StartPos - 1]) in ['0'..'9'] then
          Dec(StartPos);

        FServerVersion := Copy(FServerVersionFull, StartPos, EndPos - StartPos + 1);

        FMajorServerVersion := StrToInt(Copy(FServerVersionFull, StartPos, DotPos - StartPos));
        FMinorServerVersion := StrToInt(FServerVersionFull[DotPos + 1]);
      end;
    end;
  end;

  Result := FServerVersion;
end;

function TGDSConnection.GetServerVersionFull: _string;
begin
  if FServerVersionFull = '' then begin
    if IsFBServer then
      FServerVersionFull := FDatabaseInfo.FBVersion
    else
      FServerVersionFull := FDatabaseInfo.Version;
  end;
  Result := FServerVersionFull;
end;

function TGDSConnection.GetClientVersion: _string;
begin
  Assert(GDS <> nil);
  Result := GDS.VersionSt;
end;

{TGDSTransaction}

constructor TGDSTransaction.Create;
begin
  inherited Create;

  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FTransactionHandle := Marshal.AllocHGlobal(Sizeof(TISC_TR_HANDLE));
  Marshal.WriteIntPtr(FTransactionHandle, nil);
  SetLength(FTPB, 0);

  FParams := TStringList.Create;
  FParamsChanged := True;
  TStringList(FParams).OnChange := ParamsChange;
  TStringList(FParams).OnChanging := ParamsChanging;
end;

destructor TGDSTransaction.Destroy;
begin
//  CloseTransaction;
  SetTransactionHandle(nil);
  FParams.Free;
  SetLength(FTPB, 0);
  Marshal.FreeHGlobal(FTransactionHandle);
  Marshal.FreeHGlobal(FStatusVector);
  inherited;
end;

procedure TGDSTransaction.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    if FConnections.Count = 1 then
      TGDSConnection(FConnections[0]).IBCError(FStatusVector, True, Component)
    else
      IBCError(FStatusVector, True);
end;

procedure TGDSTransaction.IBCError(var StatusVector: TStatusVector; UseCallback: boolean);
var
  Msg, SqlErrMsg: _string;
  ErrorNumber, ErrorCode: integer;
  Fail: boolean;
begin
  GDS.Busy;
  ErrorCode := GDS.isc_sqlcode(StatusVector);
  GDS.Release;
  GDS.GetIBError(ErrorCode, False, ErrorNumber, StatusVector, Msg, SqlErrMsg);
  FLastError := ErrorCode;
  try
    raise EIBCError.Create(FLastError, ErrorNumber, Msg, SqlErrMsg);
  except
    on E: EIBCError do begin
    {$IFNDEF LITE}
      E.Component := Component;
    {$ENDIF}
      Fail := True;
      if UseCallback then
        if Assigned(OnError) then
          OnError(E, Fail);
      if Fail then
        raise
      else
        Abort;
    end;
  end;
end;

procedure TGDSTransaction.ParamsChange(Sender: TObject);
begin
  FParamsChanged := True;
end;

procedure TGDSTransaction.ParamsChanging(Sender: TObject);
begin
  CheckInActive;                                                    
end;

procedure TGDSTransaction.Commit;
var
  Res: ISC_STATUS;
  ErrorNumber: integer;
  NeedClose: boolean;
begin
  CheckActive;
  if FNativeTransaction then begin
    if not GDS.AlerterFatalError then begin
      GDS.Busy;
      Res := GDS.isc_commit_transaction(FStatusVector, FTransactionHandle);
      GDS.Release;
      if Res > 0 then begin
        ErrorNumber := Marshal.ReadInt32(FStatusVector, 1 * SizeOf(Long));
        NeedClose := EIBCError.IsFatalError(ErrorNumber);
      end
      else
        NeedClose := True;
      if NeedClose then
        SetTransactionHandle(nil);
      Check(Res);
    end
    else
      SetTransactionHandle(nil);
  end
  //else
  //  RaiseError(SCantEndSharedTransaction);
end;

procedure TGDSTransaction.CommitRetaining;
var
  Res: ISC_STATUS;
begin
  CheckActive;
  GDS.Busy;
  Res := GDS.isc_commit_retaining(FStatusVector, FTransactionHandle);
  GDS.Release;
  Check(Res);
end;

procedure TGDSTransaction.Rollback;
var
  Res: ISC_STATUS;
begin
  CheckActive;
  if FNativeTransaction then begin
    if not GDS.AlerterFatalError then begin
      GDS.Busy;
      Res := GDS.isc_rollback_transaction(FStatusVector, FTransactionHandle);
      GDS.Release;
      SetTransactionHandle(nil);
      Check(Res);
    end
    else
      SetTransactionHandle(nil);
  end
  //else
  //  RaiseError(SCantEndSharedTransaction);
end;

procedure TGDSTransaction.RollbackRetaining;
var
  Res: ISC_STATUS;
begin
  CheckActive;
  GDS.Busy;
  Res := GDS.isc_rollback_retaining(FStatusVector, FTransactionHandle);
  GDS.Release;
  Check(Res);
end;

procedure TGDSTransaction.CreateTPB;
const
  TPBPrefix = 'isc_tpb_';
  TPBConstantNames: array[1..isc_tpb_last_tpb_constant] of string = (
    'consistency',
    'concurrency',
    'shared',
    'protected',
    'exclusive',
    'wait',
    'nowait',
    'read',
    'write',
    'lock_read',
    'lock_write',
    'verb_time',
    'commit_time',
    'ignore_limbo',
    'read_committed',
    'autocommit',
    'rec_version',
    'no_rec_version',
    'restart_requests',
    'no_auto_undo',
    'no_savepoint',
    'lock_timeout'
  );

  procedure CheckTPBSize (NeededSize: integer; var TPBSize: integer);
  begin
    if TPBSize < NeededSize then begin
      TPBSize := ((NeededSize div 2048) + 1) * 2048;
      SetLength(FTPB, TPBSize);
    end;
  end;

var
  TPBSize: integer;

  procedure TPBIsolationLevel;
  begin
    case FIsolationLevel of
      ilSnapshot: begin
         CheckTPBSize(FTPBLength + 3, TPBSize);
         FTPB[FTPBLength] := isc_tpb_concurrency;
         FTPB[FTPBLength + 1] := isc_tpb_nowait;
         inc(FTPBLength, 3);
      end;
      ilReadCommitted: begin
         CheckTPBSize(FTPBLength + 4, TPBSize);
         FTPB[FTPBLength] := isc_tpb_read_committed;
         FTPB[FTPBLength + 1] := isc_tpb_rec_version;
         FTPB[FTPBLength + 2] := isc_tpb_nowait;
         inc(FTPBLength, 4);
      end;
      ilIsolated: begin
         CheckTPBSize(FTPBLength + 2, TPBSize);
         FTPB[FTPBLength] := isc_tpb_consistency;
         inc(FTPBLength, 2);
      end;
    else
      RaiseError(SUnsupportedIsolationLevel);
    end;
    if FReadOnly then
      FTPB[FTPBLength - 1] := isc_tpb_read
    else
      FTPB[FTPBLength - 1] := isc_tpb_write;
  end;

var
  i, j: integer;
  TPBVal: UShort;
  ParamName, ParamValue: string;
  Found: boolean;

begin
  TPBSize := 2048; //Start size that must cover common user needs
  SetLength(FTPB, TPBSize);
  FTPBLength := 1;
  FTPB[0] := isc_tpb_version3;

  if FIsolationLevel <> ilCustom then
    TPBIsolationLevel; //Set isolation Level

  try
    for i := 0 to FParams.Count - 1 do begin
      if Pos('=', FParams[i]) > 0 then begin
        ParamName := Trim(LowerCase(FParams.Names[i]));
        ParamValue := Copy(FParams[i], Pos('=', FParams[i]) + 1, Length(FParams[i]));
      end
      else begin
        ParamName := Trim(LowerCase(FParams[i]));
        ParamValue := '';
      end;
      if ParamName = '' then
        continue;
      if (Pos(TPBPrefix, ParamName) = 1) then
        Delete(ParamName, 1, Length(TPBPrefix));
      TPBVal := 0;
      { Find the parameter }
      for j := 1 to isc_tpb_last_tpb_constant do
        if (ParamName = TPBConstantNames[j]) then begin
          TPBVal := j;
          break;
        end;

      Found := False;
      for j := 0 to FTPBLength - 1 do
        if FTPB[j] = TPBVal then begin
          Found := True;
          break;
        end;

      if Found then
        continue;

      case TPBVal of
        isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
        isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
        isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
        isc_tpb_read_committed, isc_tpb_rec_version, isc_tpb_no_rec_version,
        isc_tpb_restart_requests, isc_tpb_no_auto_undo, isc_tpb_no_savepoint: begin
          CheckTPBSize(FTPBLength + 1, TPBSize);
          FTPB[FTPBLength] := TPBVal;
          Inc(FTPBLength)
        end;
        unique_isc_tpb_lock_timeout: begin
          CheckTPBSize(FTPBLength + 6, TPBSize);
          FTPB[FTPBLength] := isc_tpb_lock_timeout;
          FTPB[FTPBLength + 1] := 4;
          j := StrToIntDef(ParamValue, 10);
          FTPB[FTPBLength + 2] := Byte(j);
          FTPB[FTPBLength + 3] := Byte(j shr 8);
          FTPB[FTPBLength + 4] := Byte(j shr 16);
          FTPB[FTPBLength + 5] := Byte(j shr 24);
          Inc(FTPBLength, 6);
        end;
        isc_tpb_lock_read, isc_tpb_lock_write: begin
          CheckTPBSize(FTPBLength + 2 + Length(ParamValue), TPBSize);
          FTPB[FTPBLength] := TPBVal;
          FTPB[FTPBLength + 1] := Length(ParamValue);
          Encoding.Default.GetBytes(ParamValue, 0, Length(ParamValue), FTPB, FTPBLength + 2);
          Inc(FTPBLength, 2 + Length(ParamValue));
        end;
        else begin
          if (TPBVal > 0) and
             (TPBVal <= isc_tpb_last_tpb_constant) then
            RaiseError(Format(STPBConstantNotSupported, [TPBConstantNames[TPBVal]]))
          else
            RaiseError(Format(STPBConstantUnknown, [ParamName]));
        end;
      end;
    end;
  except
    SetLength(FTPB, 0);
    FTPBLength := 0;
    raise;
  end;
end;

procedure TGDSTransaction.StartTransaction;
var
  pTEB: PISC_TEB_ARRAY;
  TPBHandle, PTPB: IntPtr;
  i: integer;
  TEBOffset: integer;
  Res: ISC_STATUS;
begin
  CheckInactive;

  if FConnections.Count = 0 then
    RaiseError(SNoConnectionsInTransaction);

  for i := 0 to FConnections.Count - 1 do
   if FConnections[i] <> nil then begin
     if not FConnections[i].GetConnected then
       RaiseError(SConnClosedOnTrStart);
   end;

  FGDS := TGDSConnection(FConnections[0]).GDS;

  if FParamsChanged then begin
    CreateTPB;
    FParamsChanged := False;
  end;

  pTEB := Marshal.AllocHGlobal(FConnections.Count * SizeOfISC_TEB);
  try
    TPBHandle := AllocGCHandle(FTPB, True);
    PTPB := GetAddrOfPinnedObject(TPBHandle);
    TEBOffset := 0;
    for i := 0 to FConnections.Count - 1 do begin
      Marshal.WriteIntPtr(pTEB, TEBOffset, TGDSConnection(FConnections[i]).FDatabaseHandle);
      Marshal.WriteInt32(pTEB, TEBOffset + OffsetOf_TISC_TEB_tpb_length, FTPBLength);
      Marshal.WriteIntPtr(pTEB, TEBOffset + OffsetOf_TISC_TEB_tpb_address, PTPB);
      Inc(TEBOffset, SizeOfISC_TEB);
    end;
    try
      try
        GDS.Busy;
        Res := GDS.isc_start_multiple(FStatusVector, FTransactionHandle, FConnections.Count, pTEB);
        GDS.Release;
        Check(Res);
      except
        Marshal.WriteIntPtr(FTransactionHandle, nil);
        raise;
      end;
    finally
      FreeGCHandle(TPBHandle);
    end;
  finally
    Marshal.FreeHGlobal(pTEB);
  end;
  FNativeTransaction := True;
end;

procedure TGDSTransaction.Reset;
begin
  SetTransactionHandle(nil);
end;

procedure TGDSTransaction.AssignConnect(Source: TCRTransaction);
begin
  inherited;

  SetTransactionHandle(TGDSTransaction(Source).GetTransactionHandle);
end;

function TGDSTransaction.CanRestoreAfterFailover: boolean;
begin
  Result := FReadOnly and (FIsolationLevel = ilReadCommitted);
end;

{ Savepoint control }
procedure TGDSTransaction.ExecuteImmediate(const SQL: string);
var
  PSQL: IntPtr;
  Conn: TGDSConnection;
  Res: ISC_STATUS;
  i: integer;
begin
  PSQL := Marshal.StringToHGlobalAnsi(AnsiString(SQL));
  try
    for i := 0 to FConnections.Count - 1 do begin
      Conn := TGDSConnection(FConnections[i]);
      GDS.Busy;
      Res := GDS.isc_dsql_execute_immediate(FStatusVector, Conn.FDatabaseHandle,
        FTransactionHandle, 0, PSQL, Conn.FSQLDialect, nil);
      GDS.Release;
      Check(Res);
    end;
  finally
    Marshal.FreeCoTaskMem(PSQL);
  end;
end;

procedure TGDSTransaction.ReleaseSavepoint(const Name: _string);
begin
  CheckActive;
  ExecuteImmediate('RELEASE SAVEPOINT ' + Name);
{ This code unsupported in FB and raises AV when we execute it from
  IB client against FB database

  pString := Marshal.StringToHGlobalAnsi(Name);
  try
    Check(isc_release_savepoint(FStatusVector, FTransactionHandle, pString));
  finally
    Marshal.FreeCoTaskMem(pString);
  end;
}
end;

procedure TGDSTransaction.RollbackToSavepoint(const Name: _string);
begin
  CheckActive;
  ExecuteImmediate('ROLLBACK TO SAVEPOINT ' + Name);
{ This code unsupported in FB and raises AV when we execute it from
  IB client against FB database

  pString := Marshal.StringToHGlobalAnsi(Name);
  try
    Check(isc_rollback_savepoint(FStatusVector, FTransactionHandle, pString, 0));
  finally
    Marshal.FreeCoTaskMem(pString);
  end;
}
end;

procedure TGDSTransaction.Savepoint(const Name: _string);
begin
  CheckActive;
  ExecuteImmediate('SAVEPOINT ' + Name);
{ This code unsupported in FB and raises AV when execute it from
  IB client against FB database

  pString := Marshal.StringToHGlobalAnsi(Name);
  try
    Check(isc_start_savepoint(FStatusVector, FTransactionHandle, pString));
  finally
    Marshal.FreeCoTaskMem(pString);
  end;
}  
end;

{ Transaction control}
procedure TGDSTransaction.AssignTransaction(Source: TGDSTransaction);
begin
  if Source <> Self then begin
    CheckInactive;
    if Source <> nil then begin
      SetTransactionHandle(Source.GetTransactionHandle);
    end
    else
      SetTransactionHandle(nil);
  end;
end;

function TGDSTransaction.GetTransactionHandle: TISC_TR_HANDLE;
begin
  Result := nil;
  if FTransactionHandle <> nil then
    Result := Marshal.ReadIntPtr(FTransactionHandle);
end;

procedure TGDSTransaction.SetTransactionHandle(Value: TISC_TR_HANDLE);
begin
  if Value <> GetTransactionHandle then begin
    Marshal.WriteIntPtr(FTransactionHandle, Value);
    FNativeTransaction := Value = nil;
    if (Value <> nil) and (GDS = nil) then
      FGDS := GDSList.GetGDS('');
  end;
end;

function TGDSTransaction.GetInTransaction: boolean;
begin
  Result := GetTransactionHandle <> nil;
end;

function TGDSTransaction.GetConnection(Idx: integer): TGDSConnection;
begin
  Result := TGDSConnection(FConnections[Idx]);
end;

function TGDSTransaction.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := inherited SetProp(Prop, Value);
end;

function TGDSTransaction.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited GetProp(Prop, Value);
end;

procedure TGDSTransaction.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

function TGDSTransaction.GetParams: TStrings;
begin
  Result := FParams;
end;

{TGDSConnections}

function TGDSConnections.GetItems(Index: integer): TGDSConnection;
begin
  Result := TGDSConnection(inherited Items[Index]);
end;

function TGDSConnections.FindConnection(GDSConnection: TGDSConnection): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if GDSConnection = TGDSConnection(Items[i]) then begin
      Result := i;
      Break;
    end;
end;

{TGDSCommand}

constructor TGDSCommand.Create;
var
  GUID: TGUID;
  UniqCursor: String;
  UniqCursorLen: integer;

  function GUIDToString(const GUID: TGUID): string;
  {$IFDEF CLR}
   var
     SB: StringBuilder;
   begin
     SB := StringBuilder.Create(35);
     try
       Result := UpperCase(SysUtils.GUIDToString(GUID));
       SB.Append('IBC');
       SB.Append(Copy(Result, 1, 8));
       SB.Append(Copy(Result, 10, 4));
       SB.Append(Copy(Result, 15, 4));
       SB.Append(Copy(Result, 20, 4));
       SB.Append(Copy(Result, 25, 12));
       Result := SB.ToString;
     finally
       SB.Free;
     end;
  {$ELSE}
   begin
     SetLength(Result, 33);
     StrLFmt(PChar(Result), 33,'A%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
       [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
       GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
  {$ENDIF}
  end;
begin
  inherited Create;

  FAutoCommit := False;
  FCacheBlobs := True;
  FCacheArrays := True;
  FSQLType := SQL_UNKNOWN;
  FParsedSQLType := SQL_UNKNOWN;
  FIntegerPrecision := IntegerPrecision;
  FFloatPrecision := FloatPrecision;
  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FState := csInactive;
  FStmtHandle := Marshal.AllocHGlobal(Sizeof(TISC_STMT_HANDLE));
  Marshal.WriteIntPtr(FStmtHandle, nil);
  {$IFDEF VER6P}
  CreateGuid(GUID);
  {$ELSE}
  CoCreateGuid(GUID);
  {$ENDIF}
  UniqCursor := GUIDToString(GUID);
  UniqCursorLen := Length(UniqCursor);
  FCursor := Marshal.AllocHGlobal(UniqCursorLen + 1);
  Marshal.WriteByte(FCursor, UniqCursorLen, 0);
  CopyBufferAnsi(AnsiString(UniqCursor), FCursor, UniqCursorLen);
  FQueryRowsAffected := True;
end;

destructor TGDSCommand.Destroy;
begin
  if GetCursorState > csInactive then
    Finish;
  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);
  FInXSQLDA.Free;
  FOutXSQLDA.Free;
  Marshal.FreeHGlobal(FStmtHandle);
  Marshal.FreeHGlobal(FStatusVector);
  Marshal.FreeHGlobal(FCursor);
  inherited;
end;

procedure TGDSCommand.CheckSQLDA(InVarsCount: integer = 0; OutVarsCount: integer = 0);
var
  GDSVersion: currency;
begin
  GDSVersion := GDS.Version;
  if (FInXSQLDA = nil) or
    ((GDSVersion < 7) and (FInXSQLDA.FXSQLVARType = vtGDS7)) or
    ((GDSVersion >= 7) and (FInXSQLDA.FXSQLVARType = vtGDS)) then begin
    FInXSQLDA.Free;
    if GDSVersion >= 7 then
      FInXSQLDA := TSQLDA.Create(FConnection, vtGDS7)
    else
      FInXSQLDA := TSQLDA.Create(FConnection, vtGDS);
  end;
  FInXSQLDA.AllocSQLDA(InVarsCount);

  if (FOutXSQLDA = nil) or
    ((GDSVersion < 7) and (FOutXSQLDA.FXSQLVARType = vtGDS7)) or
    ((GDSVersion >= 7) and (FOutXSQLDA.FXSQLVARType = vtGDS)) then begin
    FOutXSQLDA.Free;
    if GDSVersion >= 7 then
      FOutXSQLDA := TSQLDA.Create(FConnection, vtGDS7)
    else
      FOutXSQLDA := TSQLDA.Create(FConnection, vtGDS);
  end;
  FOutXSQLDA.AllocSQLDA(OutVarsCount);
end;

procedure TGDSCommand.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure TGDSCommand.CheckDatabase;
begin
  if FConnection = nil then
    RaiseError(SConnectionNotDefined);
  if not FConnection.GetConnected then
    RaiseError(SConnectionClosed);
end;

procedure TGDSCommand.CheckTransaction;
begin
  if (FTransaction = nil) then
    RaiseError(STransactionNotAssigned);
  if not FTransaction.GetInTransaction then
    RaiseError(SNotInTransaction);
end;

procedure TGDSCommand.CheckActive;
begin
  if GetCursorState = csInactive then
    RaiseError(SCursorNotOpened);
end;

function TGDSCommand.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

procedure TGDSCommand.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    FConnection.IBCError(FStatusVector, True, Component);
end;

function TGDSCommand.GetActive: boolean;
begin
  Result := FState <> csInactive;
end;

function TGDSCommand.GetRowsAffected: integer;
var
  LocalBuffer, InfoType: IntPtr;
  Res: ISC_STATUS;
begin
  if (not FQueryRowsAffected) or (not GetPrepared or (FSQLType = SQL_DDL) or
    ((FSQLType = SQL_EXEC_PROCEDURE) and (FParsedSQLType <> SQL_INSERT))) then begin
    FRowsInserted := -1;
    FRowsUpdated := -1;
    FRowsDeleted := -1;
    Result := -1;
  end
  else begin
    InfoType := Marshal.AllocHGlobal(SizeOf(Byte));
    Marshal.WriteByte(InfoType, isc_info_sql_records);
    LocalBuffer := Marshal.AllocHGlobal(LocalBufferLen);
    try
      GDS.Busy;
      Res := GDS.isc_dsql_sql_info(FStatusVector, FStmtHandle, 1, InfoType, LocalBufferLen, LocalBuffer);
      GDS.Release;
      Check(Res);

      if (Marshal.ReadByte(LocalBuffer) <> isc_info_sql_records) then
        RaiseError(SSQLInfoError);

      FRowsUpdated := Marshal.ReadInt32(LocalBuffer, 6);
      FRowsDeleted := Marshal.ReadInt32(LocalBuffer, 13);
      FRowsInserted := Marshal.ReadInt32(LocalBuffer, 27);
      Result := FRowsInserted + FRowsUpdated + FRowsDeleted;
    finally
      Marshal.FreeHGlobal(LocalBuffer);
      Marshal.FreeHGlobal(InfoType);
    end;
  end;
end;

procedure TGDSCommand.DescribeParam(const Name: _string; Param: TIBCParamDesc; SQLVAR: TSQLVARAccessor);
var
  DataSize: integer;
  DataType: SmallInt;
  Precision, Scale: integer;
begin
  if Name = '' then //Output parameters
    Param.Name := SQLVAR.GetAliasName
  else
    Param.Name := Name;
  Param.Scale := 0;
  Param.Precision := 0;

  DataSize := SQLVAR.sqllen;
  DataType := SQLVAR.sqltype;

  case DataType of
    SQL_VARYING, SQL_TEXT: begin
      if Byte(SQLVAR.sqlsubtype) = CH_OCTETS then begin
        if DataType = SQL_TEXT then
          Param.DataType := dtBytes
        else
          Param.DataType := dtVarBytes;
        Param.SetSize(DataSize);
      end
      else
        if FConnection.FUseUnicode then begin
          DataSize := DataSize div FConnection.GetCharLength(Byte(SQLVAR.sqlsubtype));
          if DataType = SQL_TEXT then begin
            Param.DataType := dtFixedWideChar;
            Param.SetSize(DataSize);
          end
          else begin
            Param.DataType := dtWideString;
            Param.SetSize(DataSize);
          end;
          SQLVAR.sqllen := DataSize * FConnection.GetCharLength(Byte(SQLVAR.sqlsubtype)); //to set proper out param len if DB Charset is not UTF8
        end
        else begin
          if DataType = SQL_TEXT then
            Param.DataType := dtFixedChar
          else
            Param.DataType := dtString;
          Param.SetSize(DataSize); //size in bytes
        end;
    end;
    SQL_FLOAT,SQL_DOUBLE: begin
      Param.SetDataType(dtFloat);
    end;
    SQL_SHORT, SQL_LONG, SQL_INT64: begin
      case DataType of
        SQL_SHORT: begin
          Precision := IntegerPrecision div 2;
          Param.SetDataType(dtSmallint);
        end;
        SQL_LONG: begin
          Precision := IntegerPrecision;
          Param.SetDataType(dtInteger);
        end;
        else begin
          Precision := IntegerPrecision * 2;
          Param.SetDataType(dtInt64);
        end;
      end;
      Scale := SQLVAR.sqlscale;
      Param.Precision := Precision;
      Param.Scale := ABS(Scale);
      if (Scale <> 0) or (FConnection.FSimpleNumericMap and (SQLVAR.sqlsubtype <> 0)) then begin
        if (Scale >= (-4)) and (FConnection.EnableBCD or FEnableBCD) then begin
          Param.SetDataType(dtBCD);
        end
        else
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        if (FConnection.FSQLDialect <> 1) and (FConnection.EnableFMTBCD or FEnableFMTBCD) then begin
          Param.SetDataType(dtFMTBCD);
        end
        else
      {$ENDIF}
      {$ENDIF}
        begin
          Param.SetDataType(dtFloat);
        end;
      end;
    end;
    SQL_BOOLEAN: begin
      Param.SetDataType(dtBoolean);
    end;
    SQL_TYPE_TIME: begin
      Param.SetDataType(dtTime);
    end;
    SQL_TYPE_DATE: begin
      Param.SetDataType(dtDate);
    end;
    SQL_TIMESTAMP: begin
      Param.SetDataType(dtDateTime);
    end;
    SQL_QUAD, SQL_BLOB: begin
      if FConnection.FEnableMemos and (SQLVAR.sqlsubtype = isc_blob_text) then begin
        if FConnection.FUseUnicode and
          ((FConnection.FDBCharsetId = CH_UNICODE_FSS) or
           (FConnection.FDBCharsetId = CH_UTF8) or
           (FConnection.FDBCharsetId = CH_NONE))
        then
          Param.SetDataType(dtWideMemo)
        else
          Param.SetDataType(dtMemo);
      end
      else
        Param.SetDataType(dtBlob);
    end;
    SQL_ARRAY:
      Param.SetDataType(dtArray);
    else
      RaiseError(SDataTypeNotSupported);
  end;
  if Param.ValuePtr = nil then //Allocate param value
    Param.AllocBuffer;
end;

procedure TGDSCommand.DescribeParams(Full: boolean = True; InParamsCount: integer = 0);
var
  i, OutParamsCount: integer;
  Param: TIBCParamDesc;
begin
  if Full then begin
    CheckSQLDA;

    GDS.Busy;
    try
      Check(GDS.isc_dsql_describe_bind(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA));
      InParamsCount := FInXSQLDA.GetSqld;
      if InParamsCount > FInXSQLDA.GetSqln then begin
        FInXSQLDA.AllocSQLDA(InParamsCount);
        Check(GDS.isc_dsql_describe_bind(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA));
      end;
    finally
      GDS.Release;
    end;

    for i := 0 to InParamsCount - 1 do begin
      if i >= Params.Count then begin
        Param := TIBCParamDesc(AddParam);
        Param.ParamType := pdInput;
      end
      else
        Param := TIBCParamDesc(Params[i]);
      DescribeParam(Param.GetName, Param, FInXSQLDA.Vars[i]);
    end;
  end;

  if FSQLType <> SQL_SELECT then begin
    GDS.Busy;
    try
      Check(GDS.isc_dsql_describe(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FOutXSQLDA.FXSQLDA));
      OutParamsCount := FOutXSQLDA.GetSqld;
      if OutParamsCount > FOutXSQLDA.GetSqln then begin
        FOutXSQLDA.AllocSQLDA(OutParamsCount);
        Check(GDS.isc_dsql_describe(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FOutXSQLDA.FXSQLDA));
      end;
    finally
      GDS.Release;
    end;

    for i := 0 to OutParamsCount - 1 do begin
      if i >= Params.Count - InParamsCount then begin
        Param := TIBCParamDesc(AddParam);
        Param.ParamType := pdOutput;
      end
      else
        Param := TIBCParamDesc(Params[InParamsCount + i]);
      if Full or (Param.GetDataType = dtUnknown) then
        DescribeParam(Param.GetName, Param, FOutXSQLDA.Vars[i]);
    end;
  end
  else
    OutParamsCount := 0;

  if Full then
    for i := Params.Count - 1 downto InParamsCount + OutParamsCount do
      Params[i].Free;
end;

procedure TGDSCommand.InternalExecute;
var
  OldPrepared: boolean;
  i: integer;
  Param: TIBCParamDesc;
  Res: integer;
  OldCursorState: TCursorState;
  Blob: TIBCBlob;
begin
  OldPrepared := GetPrepared;
  if not OldPrepared then
    Prepare;
  try
    BindParams;

    //writing Blob parameters to create valid Blob_ID
    for i := 0 to FParams.Count - 1 do begin
      Param := TIBCParamDesc(FParams[i]);
      if not Param.GetIsBound then
        continue;
      case Param.DataType of
        dtBlob, dtMemo, dtWideMemo: begin
          Blob := TIBCBlob(Param.GetObject);
          Blob.Cached := True;
          Blob.Streamed := FStreamedBlobs;
          if Blob.Cached and not Param.GetNull then begin
          {$IFDEF HAVE_COMPRESS}
            case FCompressBlob of
              cbServer, cbClientServer:
                Blob.Compressed := True;
              else
                Blob.Compressed := False;
            end;
          {$ENDIF}
            if Param.ParamType <= pdInput then
              Blob.WriteBlob;
          end;
        end;
        dtArray:
          if (Param.ParamType <= pdInput) and (not Param.GetNull) and
            TCustomIBCArray(Param.GetObject).Cached
          then
            TCustomIBCArray(Param.GetObject).WriteArray;
      end;
    end;

    //Execute statement
    OldCursorState := GetCursorState;
    SetCursorState(csExecuting);

    GDS.Busy;
    try
      case FSQLType of
        SQL_SELECT:
          Res := GDS.isc_dsql_execute2(FStatusVector, FTransaction.FTransactionHandle,
            FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA, nil);
        SQL_SELECT_FOR_UPD: begin
          Res := GDS.isc_dsql_execute2(FStatusVector, FTransaction.FTransactionHandle,
            FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA, nil);
          Check(GDS.isc_dsql_set_cursor_name(FStatusVector, FStmtHandle, FCursor, 0));
        end;
        SQL_EXEC_PROCEDURE: begin
          Res := GDS.isc_dsql_execute2(FStatusVector, FTransaction.FTransactionHandle,
            FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA, FOutXSQLDA.FXSQLDA);
        end
        else
          Res := GDS.isc_dsql_execute(FStatusVector, FTransaction.FTransactionHandle,
            FStmtHandle, FConnection.FSQLDialect, FInXSQLDA.FXSQLDA);
      end;
    finally
      GDS.Release;
    end;
    
    //Change cursor state
    if Res <> 0 then
      SetCursorState(OldCursorState) // In future thr error code should be checked and cursor unprepared
    else
      SetCursorState(csExecuted);
    Check(Res);
  finally
    if not OldPrepared then
      Finish;
  end;
end;

function TGDSCommand.ExecuteNext: boolean;
var
  Res: ISC_STATUS;
begin
  Result := False;
  if GetCursorState = csExecuting then
    Exit;
  FExecuting := True;
  try
    if GetCursorState <> csExecuted then
      RaiseError(SCursorNotOpened);

      SetCursorState(csExecuting);

    case FSQLType of
      SQL_EXEC_PROCEDURE, SQL_SELECT, SQL_SELECT_FOR_UPD: begin
        GDS.Busy;
        Res := GDS.isc_dsql_fetch(FStatusVector, FStmtHandle, FConnection.FSQLDialect, FOutXSQLDA.FXSQLDA);
        GDS.Release;
        if (Res = 100) then
          Result := False
        else begin
          if Res = 0 then
            SetCursorState(csExecuted)
          else
            SetCursorState(csInactive);
          Check(Res);
          Result := True
        end;
      end
      else
        RaiseError(SCantExecuteNonSelectStatement);
    end;
  except
    FExecuting := False;
    if Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;
  ReadOutParams;
  FExecuting := False;
  if Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TGDSCommand.ReadOutParams;
var
  i: integer;
  OutParamsCount: integer;
  SQLVAR: TSQLVARAccessor;
  Param: TIBCParamDesc;
  Blob: TIBCBlob;
  Arr: TCustomIBCArray;
{$IFDEF VER6P}  
  BcdValue: TBCD;
  bcdscale: integer;
{$IFDEF CLR}
  Temp: TBytes;
{$ENDIF}
{$ENDIF}
  Len: integer;
  Offset: integer;

begin
  if (FOutXSQLDA = nil) or (FOutXSQLDA.FXSQLDA = nil) then
    Exit;

  OutParamsCount := 0;
  for i := 0 to Params.Count - 1 do begin
    if Params[i].GetParamType in [pdOutput] then begin
      Param := TIBCParamDesc(Params[i]);
      SQLVAR := FOutXSQLDA.Vars[OutParamsCount];
      case SQLVAR.sqltype of
        SQL_VARYING, SQL_TEXT: begin
          if (not FConnection.FUseUnicode) or (Byte(SQLVAR.sqlsubtype) = CH_OCTETS) then begin
            if SQLVAR.sqltype = SQL_VARYING then begin
              Len := Marshal.ReadInt16(Param.FValue);
              Marshal.WriteByte(Param.FValue, Len + SizeOf(Short), 0);
              //SetStrTerminator(Param.FValue, Param.FLen + SizeOf(Short), FConnection.FCharLength);
            end;
          end
          else begin
            if SQLVAR.sqltype = SQL_TEXT then begin
              Offset := 0;
              Len := SQLVAR.sqllen;
            end
            else begin
              Offset := 2;
              Len := Marshal.ReadInt16(Param.FInternalValue);
            end;
            if (Param.DataType = dtString) or (Param.DataType = dtFixedChar) then
                Utf8ToAnsi(PtrOffset(Param.InternalValuePtr, Offset), Len,
                  PtrOffset(Param.ValuePtr, Offset), Param.FValueSize - Offset)
            else
              if (Param.DataType = dtWideString) or (Param.DataType = dtFixedWideChar) then
                Utf8ToWS(PtrOffset(Param.InternalValuePtr, Offset), Len,
                  PtrOffset(Param.ValuePtr, Offset), Param.FValueSize - Offset);
          end;
        end;
        SQL_TYPE_DATE:
          SQLDateToDateTime(Param.InternalValuePtr, Param.ValuePtr);
        SQL_TYPE_TIME:
          SQLTimeToDateTime(Param.InternalValuePtr, Param.ValuePtr);
        SQL_TIMESTAMP:
          SQLTimeStampToDateTime(Param.InternalValuePtr, Param.ValuePtr);
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        SQL_INT64: begin
          case Param.GetDataType of
            dtFMTBCD: begin
              bcdscale := (-1 * SQLVAR.GetSqlScale); // refer to comments in PrepareBlock function
              BcdValue := DBDecimalToBcd(Marshal.ReadInt64(Param.InternalValuePtr), BcdScale);
            {$IFDEF CLR}
              Temp := TBcd.ToBytes(BcdValue);
              Marshal.Copy(Temp, 0, Param.FValue, SizeOfTBcd);
            {$ELSE}
              PBcd(Param.FValue)^ := BcdValue;
            {$ENDIF}
            end;
          end;
        end;
      {$ENDIF}
      {$ENDIF}
      end;
      //Read Blob params
      case Param.DataType of
        dtBlob, dtMemo, dtWideMemo: begin
          Blob := TIBCBlob(Param.GetObject);
          Blob.Cached := FCacheBlobs;
          Blob.Streamed := FStreamedBlobs;
          Blob.CloseBlob;
          if Blob.Cached and not Param.GetNull then
            Blob.ReadBlob;
        end;
        dtArray: begin
          Arr := TCustomIBCArray(Param.GetObject);
          Arr.Cached := FCacheArrays;
          if Arr.Cached and not Param.GetNull then
            Arr.ReadArray;
        end;
      end;
      inc(OutParamsCount);
    end;
  end;
end;

procedure TGDSCommand.Finish;
var
  Res: ISC_STATUS;
begin
  Res := 0;
  try
    if (GetStmtHandle <> nil) and FConnection.GetConnected and not GDS.AlerterFatalError then begin
{This code COULD be called for Statements with isc_dsql_set_cursor_name to close open cursors
 but we should ensure that cursor exists or function fails
      if FSQLType = SQL_SELECT_FOR_UPD then
        Check(isc_dsql_free_statement(FStatusVector, FStmtHandle, DSQL_close));
}
      GDS.Busy;
      try
        if GetStmtHandle <> nil then begin
          Res := GDS.isc_dsql_free_statement(FStatusVector, FStmtHandle, DSQL_drop);
          Marshal.WriteIntPtr(FStmtHandle, nil);
        end;
      finally
        GDS.Release;
      end;
    end;
  finally
    Marshal.WriteIntPtr(FStmtHandle, nil);
    FSQLType := SQL_UNKNOWN;
    SetCursorState(csInactive);
  end;
  Check(Res);                 //We should raise exception AFTER reseting FStmtHandle to avoid
                              //infinite loop during fatal error handle closing
end;

function TGDSCommand.GetIsFBConnection: boolean;
begin
  if FConnection <> nil then
    Result := FConnection.IsFBServer
  else
    Result := True;
end;

{ Params }
function TGDSCommand.AddParam: TParamDesc;
begin
  Result := TIBCParamDesc.Create;
  FParams.Add(Result);
end;

class function TGDSCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TIBCSQLInfo;
end;

class function TGDSCommand.GetParserClass: TSQLParserClass;
begin
  Result := TIBCParser;
end;

function TGDSCommand.ParseSQL(const SQL: _string; Params: TParamDescs;
  ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string;
var
  ParsedSQL: _StringBuilder;
  Parser: TIBCParser;
  Code, PrevCode: integer;
  St, St2: _string;
  l: integer;
  ParamName: _string;
  Param: TIBCParamDesc;

  LeftQuote, RightQuote: _char;

  InDDL: boolean;
  InReturning: boolean;
  IsParam: boolean;
begin
  Assert(Params <> nil);

  LeftQuote := '"';
  RightQuote := '"';

  FParsedSQLType := SQL_UNKNOWN; //Reset ParsedSQLType
  InDDL := False;
  InReturning := False;
  Code := 0;
  PrevCode := 0;

  if FScanParams then
    Params.Clear;

  ParsedSQL := _StringBuilder.Create(Length(SQL) + Length(SQL) div 2);
  try
    Parser := TIBCParser.Create(SQL);
    try
      Parser.OmitBlank := False;
      Parser.OmitComment := False;
      Parser.QuotedString := True;
      Parser.Uppered := False;
      Parser.DecSeparator := '.';
      Parser.ToBegin;
      repeat
        repeat
          if not InReturning and ((Code = lxReturning) and not DisableReturningKeyword and GetIsFBConnection and not InDDL) then begin
            InReturning := True;
            ParamName := '';
            //Set ParsedSQLType to obtain right affected record count
            FParsedSQLType := SQL_INSERT;
          end;

          if (Code <> lcBlank) and (Code <> lcComment) then
            PrevCode := Code;

          Code := Parser.GetNext(St);

          if (Code = lxBLOCK) and (PrevCode = lxEXECUTE) then
            FParsedSQLType := SQL_EX_EXECUTE_BLOCK;

          InDDL := InDDL or (Code = lxBEGIN) or (Code = lxCREATE) or (Code = lxDROP) or (Code = lxALTER)
            or (Code = lxDECLARE) or (Code = lxGRANT) or (Code = lxREVOKE) ;

          IsParam := (Code = 3) and not InDDL; //3 - ':'

          if not IsParam then
            ParsedSQL.Append(St);
        until (Code = lcEnd) or IsParam or InReturning;

        if IsParam or (InReturning and FScanParams) then begin
          if IsParam then
            Code := Parser.GetNext(St);

          if (Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst) then begin
            St2 := St;
            l := Length(St2);
            // remove quotes
            if (St2[1] = LeftQuote) and (St2[l] = RightQuote) then
              St2 := Copy(St2, 2, l - 2);
            if InReturning then begin
              if ParamName = '' then
                ParamName := 'RET_' + St2
              else
                ParamName := ParamName + '_' + St2;
            end
            else
              ParamName := St2;

            if not InReturning then begin
              if FScanParams then begin
                Param := TIBCParamDesc.Create;
                Param.SetName(ParamName);
                Params.Add(Param);
              end;

              if ReplaceAll or (Params.FindParam(ParamName) <> nil) then
                ParsedSQL.Append('?')
              else
                ParsedSQL.Append(':' + St);
            end;
          end
          else
          if InReturning and ((St = ',') or (Code = lcEnd)) and (ParamName <> '')
          then begin
            Param := TIBCParamDesc.Create;
            Param.SetName(ParamName);
            Param.ParamType:= pdOutput;
            Params.Add(Param);
            ParamName := '';
          end;
        end;

      until Code = lcEnd;
    finally
      Parser.Free;
    end;

    Result := ParsedSQL.ToString;
  finally
    ParsedSQL.Free;
  end;
end;

procedure TGDSCommand.BindParams;
var
  i: integer;
  InParamsCount, DefaultParamsCount: integer;
  Param: TIBCParamDesc;
  Blob: TIBCBlob;
  Arr: TCustomIBCArray;
  SQLVAR: TSQLVARAccessor;
{$IFDEF CLR}
  Temp: TBytes;
{$ENDIF}
{$IFDEF VER6P}
  BcdValue: TBcd;
{$ENDIF}
  NeedDescribe: boolean;
  utfBuff: TBytes;
  sa: AnsiString;

begin
  InParamsCount := 0;
  DefaultParamsCount := 0;
  NeedDescribe := False;
  SetLength(utfBuff, 0);
  for i := 0 to Params.Count - 1 do begin
    Param := TIBCParamDesc(Params[i]);
    if Param.GetParamType in [pdUnknown, pdInput] then begin
      Inc(InParamsCount);
      if not Param.GetIsBound then
        Inc(DefaultParamsCount);
    end
    else
      NeedDescribe := NeedDescribe or (Param.GetDataType = dtUnknown)
        or ((Param.GetDataType in [dtString, dtFixedChar, dtWideString, dtFixedWideChar, dtBytes, dtVarBytes]) and
        (Param.Size = 0))
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        or ((Param.GetDataType = dtFMTBCD) and (Param.Scale = 0))
      {$ENDIF}
      {$ENDIF};
  end;
  CheckSQLDA(InParamsCount - DefaultParamsCount, Params.Count - InParamsCount);

  { Perfomance improvement }
  if NeedDescribe then
    DescribeParams(False, InParamsCount);

  for i := 0 to Params.Count - 1 do begin
    Param := TIBCParamDesc(Params[i]); //Trial version AS restriction
    if not Param.GetIsBound then
      continue;
    Param.SyncIndicator;
    if i < InParamsCount then
      SQLVAR := FInXSQLDA.Vars[i]
    else
      SQLVAR := FOutXSQLDA.Vars[i - InParamsCount];
    SQLVAR.sqlind := Param.IndicatorPtr;
    SQLVAR.sqlprecision := 0;
    case Param.DataType of
      dtUnknown:
        if Param.GetNull and (Param.ParamType <= pdInput) then
          SQLVAR.sqltype := SQL_TEXT or 1
        else
          raise Exception.Create(Format(SUnknownParamDataType, [Param.GetName]));
      dtString, dtFixedChar: begin
        if (Param.ParamType = pdOutput) and (Param.DataType = dtString) then
          SQLVAR.sqltype := SQL_VARYING or 1
        else
          SQLVAR.sqltype := SQL_TEXT or 1;

        if (Param.ParamType = pdOutput) and (Param.Size = 0) then
          Param.SetSize(SQLVAR.sqllen);

        if FConnection.FUseUnicode then begin
          if Param.InternalValuePtr = Param.FValue then //This will prevent Internal buffer reallocation
            with Param do begin                         //in case of second execution without Size change
              FInternalSize := Size * 3;
              if (ParamType = pdOutput) and (DataType = dtString) then
                FInternalSize := FInternalSize + 2; //Alloc space for VarChar length

              FInternalValue := Marshal.AllocHGlobal(FInternalSize);
              FillChar(FInternalValue, FInternalSize, $00);
            end;
          if Param.ParamType <= pdInput then
            SQLVAR.sqllen := AnsiToUtf8(Param.ValuePtr, Param.FInternalValue, Param.FInternalSize)
          else
            SQLVAR.sqllen := Param.Size * 3;
        end
        else begin
          if Param.ParamType <= pdInput then
            if Param.ValuePtr = nil then
              SQLVAR.sqllen := 0
            else
              SQLVAR.sqllen := StrLen(PAChar(Param.ValuePtr))
          else
            if (Param.Size > 0) then
              SQLVAR.sqllen := Param.Size;
        end;
      end;
      dtBytes, dtVarBytes: begin
        if (Param.DataType = dtVarBytes) then
          SQLVAR.sqltype := SQL_VARYING or 1
        else
          SQLVAR.sqltype := SQL_TEXT or 1;

        if (Param.ParamType = pdOutput) and (Param.Size = 0) then
          Param.SetSize(SQLVAR.sqllen);

        if (Param.ParamType = pdOutput) or (Param.DataType = dtBytes) then begin
          if (Param.Size > 0) then
            SQLVAR.sqllen := Param.Size;
        end
        else
          if Param.ValuePtr = nil then
            SQLVAR.sqllen := 0
          else
            if Param.DataType = dtVarBytes then
              SQLVAR.sqllen := Marshal.ReadInt16(Param.ValuePtr);
      end;
      dtWideString, dtFixedWideChar: begin
        if (Param.ParamType = pdOutput) and (Param.DataType = dtWideString) then
          SQLVAR.sqltype := SQL_VARYING or 1
        else
          SQLVAR.sqltype := SQL_TEXT or 1;

        if (Param.ParamType = pdOutput) and (Param.Size = 0) then
          if FConnection.FCharLength > 1 then
            Param.SetSize((SQLVAR.sqllen div FConnection.FCharLength) + 1)
          else
            Param.SetSize(SQLVAR.sqllen);

        if FConnection.FUseUnicode then begin
          if Param.InternalValuePtr = Param.FValue then //This will prevent Internal buffer reallocation
            with Param do begin                         //in case of second execution without Size change
              FInternalSize := Size * 3;
              if (ParamType = pdOutput) and (DataType = dtWideString) then
                FInternalSize := FInternalSize + 2; //Alloc space for VarChar length

              FInternalValue := Marshal.AllocHGlobal(FInternalSize);
              FillChar(FInternalValue, FInternalSize, $00);
            end;
          if Param.ParamType <= pdInput then
            SQLVAR.sqllen := WsToUtf8(Param.ValuePtr, Param.FInternalValue, Param.FInternalSize)
          else
            SQLVAR.sqllen := Param.Size * 3;
        end
        else begin
          if Param.ParamType <= pdInput then begin
            if Param.ValuePtr = nil then
              SQLVAR.sqllen := 0
            else begin
              if Param.InternalValuePtr = Param.FValue then
                with Param do begin
                  FInternalSize := Size * 6;
                  FInternalValue := Marshal.AllocHGlobal(FInternalSize);
                end;
              sa := AnsiString(Marshal.PtrToStringUni(Param.FValue));
            {$IFDEF CLR}
              Marshal.Copy(TBytes(sa), 0, Param.FInternalValue, Length(sa));
            {$ELSE}
              Move(Pointer(sa)^, Param.FInternalValue^, Length(sa));
            {$ENDIF}
              SQLVAR.sqllen := Length(sa);
            end;
          end
          else
            if (Param.Size > 0) then
              SQLVAR.sqllen := Param.Size;
        end;
      end;
      dtDateTime, dtDate, dtTime: begin
        if (Param.DataType = dtDate) and (FConnection.FDBSQLDialect >= 3) then begin
          SQLVAR.sqltype := SQL_TYPE_DATE or 1;
          SQLVAR.sqllen := SizeOf(ISC_DATE);
          if Param.ParamType <= pdInput then
            DateTimeToSQLDate(Param.GetAsDateTime, Param.InternalValuePtr);
        end
        else
        if (Param.DataType = dtTime) and (FConnection.FDBSQLDialect >= 3) then begin
          SQLVAR.sqltype := SQL_TYPE_TIME or  1;
          SQLVAR.sqllen := SizeOf(ISC_TIME);
          if Param.ParamType <= pdInput then
            DateTimeToSQLTime(Param.GetAsDateTime, Param.InternalValuePtr);
        end
        else begin
          SQLVAR.sqltype := SQL_TIMESTAMP or 1;
          SQLVAR.sqllen := SizeOf(TISC_QUAD);
          if Param.ParamType <= pdInput then
            DateTimeToSQLTimeStamp(Param.GetAsDateTime, Param.InternalValuePtr);
        end;
      end;
      dtSmallint: begin
        SQLVAR.sqltype := SQL_SHORT or 1;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(SmallInt);
      end;
      dtWord: begin
         SQLVAR.sqltype := SQL_SHORT or 1;
         SQLVAR.sqlscale := 0;
         SQLVAR.sqllen := SizeOf(SmallInt);
      end;
      dtInteger: begin
        SQLVAR.sqltype := SQL_LONG or 1;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(Long);
      end;
      dtFloat: begin
        SQLVAR.sqlscale := 0;
        SQLVAR.sqltype := SQL_DOUBLE or 1;
        SQLVAR.sqllen := SizeOf(Double);
      end;
      dtLargeint: begin
        SQLVAR.sqltype := SQL_INT64 or 1;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(Int64);
      end;
      dtCurrency, dtBCD: begin
        SQLVAR.sqltype := SQL_INT64 or 1;
        SQLVAR.sqlscale := -4;
        SQLVAR.sqllen := SizeOf(Int64);
      end;
   {$IFDEF VER6P}
   {$IFNDEF FPC}
      dtFMTBCD: begin
        SQLVAR.sqltype := SQL_INT64 or 1;
        SQLVAR.sqllen := SizeOf(Int64);
        if Param.ParamType <= pdInput then begin
        {$IFDEF CLR}
          SetLength(Temp, SizeOfTBcd);
          Marshal.Copy(Param.FValue, Temp, 0, SizeOfTBcd);
          BcdValue := TBcd.FromBytes(Temp);
          SQLVAR.sqlscale := -1 * BcdScale(BcdValue);
          Temp[1] := Temp[1] and 128; //Drop scale;
          BcdValue := TBcd.FromBytes(Temp);
        {$ELSE}
          BcdValue := PBcd(Param.FValue)^;
          SQLVAR.sqlscale := -1 * Integer(BcdScale(BcdValue));
          BcdValue.SignSpecialPlaces := BcdValue.SignSpecialPlaces and 128;
        {$ENDIF}
          if not Param.GetNull then
            Marshal.WriteInt64(Param.InternalValuePtr, StrToInt64(BcdToStr(BcdValue)));
        end
        else begin
          if Param.Scale = 0 then begin
            Param.Scale := -1 * SQLVAR.sqlscale;
            Param.Precision := SQLVAR.sqlprecision;
          end
          else begin
            SQLVAR.sqlscale := -1 * Param.Scale;
            SQLVAR.sqlprecision := Param.Precision;
          end;
        end;
      end;
  {$ENDIF}
  {$ENDIF}
      dtBoolean: begin
        if GDS.Version < 7 then
          SQLVAR.sqltype := SQL_SHORT or 1
        else
          SQLVAR.sqltype := SQL_BOOLEAN or 1;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(SmallInt);
      end;
      dtBlob, dtMemo, dtWideMemo: begin
        Blob := TIBCBlob(Param.GetObject);
        Blob.Connection := FConnection;
        Blob.Transaction := FTransaction;
        Blob.Streamed := FStreamedBlobs;           //To write blobs in streamed mode even if we read it as segmented
        Param.FInternalValue := Blob.GetIDPtr;
        SQLVAR.sqltype := SQL_BLOB or 1;
        SQLVAR.sqlsubtype := Blob.SubType;
        SQLVAR.sqlscale := 0;
        SQLVAR.sqllen := SizeOf(TISC_QUAD);
      end;
      dtArray: begin
        Arr := TCustomIBCArray(Param.GetObject);
        Arr.Connection := FConnection;
        Arr.Transaction := FTransaction;
        Param.FInternalValue := TIBCArrayUtils.GetArrayIDPtr(Arr);
        SQLVAR.sqltype := SQL_ARRAY or 1;
//        SQLVAR.sqlsubtype := Param.GetAsArray.SubType;
        SQLVAR.sqlscale := Arr.ItemScale;
        SQLVAR.sqllen := SizeOf(TISC_QUAD);
      end;
    end;
    SQLVAR.sqldata := Param.InternalValuePtr;
  end;
end;

function TGDSCommand.GetParamDescType: TParamDescClass;
begin
  Result := TIBCParamDesc;
end;

function TGDSCommand.GetParam(Index: integer): TIBCParamDesc;
begin
  Result := TIBCParamDesc(Params[Index]);
end;

procedure TGDSCommand.BreakExec;
begin
  if Executing and (GetStmtHandle <> nil) then
    Check(GDS.fb_cancel_operation(FStatusVector, FStmtHandle, CANCEL_OPTION));
end;

function TGDSCommand.CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string;

  function NormalizeStrValue(const Value: _string): _string;
  begin
    Result := Trim(Value);
    if (FConnection <> nil) and (FConnection.FSQLDialect = 3) then
      Result := SQLInfo.NormalizeName(Value, FQuoteNames)
    else
      Result := _UpperCase(Result);
  end;

var
  RecordSet: TGDSRecordSet;
  SQL, NormName: _string;
  v: Variant;
  RecBuf: IntPtr;
  Param: TIBCParamDesc;
  i: integer;
  Added, OldUseDescribe: boolean;

begin
  if IsQuery then
    Result := 'SELECT * FROM ' + NormalizeStrValue(Name)
  else
    Result := 'EXECUTE PROCEDURE ' + NormalizeStrValue(Name);
  try
    if NeedDescribe then begin
      ClearParams;
      CheckDatabase;
      CheckTransaction;
      RecordSet := TGDSRecordSet.Create;
      try
        RecordSet.SetConnection(FConnection);
        RecordSet.SetTransaction(FTransaction);
        if SQLInfo.IsQuoted(Trim(Name)) then
          NormName := SQLInfo.UnQuote(Trim(Name))
        else
          NormName := _UpperCase(Trim(Name));

        SQL := 'SELECT P.RDB$PARAMETER_NAME ' + #13#10 +
          'FROM RDB$PROCEDURE_PARAMETERS P' + #13#10 +
          'WHERE P.RDB$PROCEDURE_NAME = ' + _QuotedStr(NormName, '''') + #13#10 +
          '  AND P.RDB$PARAMETER_TYPE = 0 ' + #13#10 +
          'ORDER BY P.RDB$PARAMETER_NUMBER';
        RecordSet.SetSQL(SQL);
        RecordSet.SetProp(prFlatBuffers, False);
        RecordSet.SetProp(prLongStrings, True);
        RecordSet.Open;
        RecordSet.FetchAll;
        RecordSet.SetToBegin;
        FSQL := '';
        if RecordSet.RecordCount > 0 then begin
          RecBuf := nil;
          RecordSet.AllocRecBuf(RecBuf);
          Result := Result + '(';
          FSQL := Result;
          try
            RecordSet.GetNextRecord(RecBuf);
            while not RecordSet.Eof do begin
              RecordSet.GetFieldAsVariant(1, RecBuf, v); //Parameter name
              //Add param to find out Param Name
              Param := TIBCParamDesc(AddParam);
              Param.ParamType := pdInput;
              Param.Name := Trim(v);
              FSQL := FSQL + '?';
              Result := Result + ':' + NormalizeStrValue(Param.Name);
              RecordSet.GetNextRecord(RecBuf);
              if not RecordSet.Eof then
                SQL := ', '
              else
                SQL := ')';
              FSQL := FSQL + SQL;
              Result := Result + SQL;
            end;
          finally
            if RecBuf <> nil then
              RecordSet.FreeRecBuf(RecBuf);
          end;
        end
        else
          FSQL := Result; //Only out paramas or no params
      finally
        RecordSet.Free;
      end;
      FUserSQL := Result;
      OldUseDescribe := FUseDescribeParams;
      FUseDescribeParams := True;
      try
        Prepare; // Desribe In and Out Params
        Unprepare;
      finally
        FUseDescribeParams := OldUseDescribe;
      end;
    end
    else begin
      //Create SQL using params
      //We assume that param order is equal to param order in StoredProcedure
      SQL := Result;
      Added := False;
      for i := 0 to FParams.Count - 1 do begin
        Param := TIBCParamDesc(FParams[i]);
        if (Param.ParamType = pdInput) and Param.GetIsBound then begin
          if not Added then begin
            SQL := SQL + '(';
            Result := Result + '(';
          end
          else begin
            SQL := SQL + ', ';
            Result := Result + ', ';
          end;
          SQL := SQL + '?';
          Result := Result + ':' + NormalizeStrValue(Param.Name);
          Added := True;
        end;
      end;
      if Added then begin
        SQL := SQL + ')';
        Result := Result + ')';
      end;
      FSQL := SQL;
      FUserSQL := Result;
    end;
  finally
    FParsedSQLType := SQL_UNKNOWN;
  end;
end;

procedure TGDSCommand.Prepare;
var
  pSQL: PStr;
  LocalBuffer, InfoType: IntPtr;
  SwapLen: integer;

begin
  CheckDatabase;
  CheckTransaction;
  if GetCursorState <> csInactive then
    Exit;
  FSQLType := SQL_UNKNOWN;
  if Trim(FSQL) = '' then //to avoid server crash
    RaiseError(SEmptySQL);
  try
    GDS.Busy;
    try
      Check(GDS.isc_dsql_alloc_statement2(FStatusVector, FConnection.FDatabaseHandle, FStmtHandle));
      pSQL := StringToPtrGDS(FSQL, FConnection.FUseUnicode);
      try
        Check(GDS.isc_dsql_prepare(FStatusVector, FTransaction.FTransactionHandle, FStmtHandle, 0,
          pSQL, FConnection.FSQLDialect, nil));
      finally
        FreeStringGDS(pSQL, FConnection.FUseUnicode);
      end;
      InfoType := Marshal.AllocHGlobal(SizeOf(Byte));
      Marshal.WriteByte(InfoType, isc_info_sql_stmt_type);
      LocalBuffer := Marshal.AllocHGlobal(8);
      try
        Check(GDS.isc_dsql_sql_info(FStatusVector, FStmtHandle, 1, InfoType, 8, LocalBuffer));
        if (Marshal.ReadByte(LocalBuffer) <> isc_info_sql_stmt_type) then
          RaiseError(SSQLInfoError);
        SwapLen := Marshal.ReadInt16(LocalBuffer, 1);
        case SwapLen of
          2: FSQLType := Marshal.ReadInt16(LocalBuffer, 3);
          4: FSQLType := Marshal.ReadInt32(LocalBuffer, 3);
        end;
      finally
        Marshal.FreeHGlobal(LocalBuffer);
        Marshal.FreeHGlobal(InfoType);
      end;
    finally
      GDS.Release;
    end;
    //Validate statement type
    if (FSQLType = SQL_PUT_SEGMENT) or (FSQLType = SQL_GET_SEGMENT) or
       (FSQLType = SQL_START_TRANS) then
      RaiseError(SWrongSQLType);

    if FUseDescribeParams then
      DescribeParams;

    inherited;

  except
    Finish;
    raise;
  end;
end;

procedure TGDSCommand.Unprepare;
begin
  if GetCursorState = csInactive then
    Exit;
  Finish;

  inherited; 
end;

function TGDSCommand.GetPrepared: boolean;
begin
  Result := GetCursorState >= csPrepared;
end;

procedure TGDSCommand.Execute(Iters: integer = 1);
begin
  if GetCursorState = csExecuting then
    Exit;
  FExecuting := True;
  try
    InternalExecute;
  except
    FExecuting := False;
    if Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;
  ReadOutParams;
  FExecuting := False;
  if Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TGDSCommand.ExecuteBatch(const Statements: _TStringArray);
var
  Status, SqlPtr, RowsAff: IntPtr;
  Count, i: integer;
  Sql: array of PStr;
{$IFDEF CLR}
  SqlPtrs: array of IntPtr;
  Len: integer;
{$ENDIF}
  Res: ISC_STATUS;
begin
  Count := Length(Statements);
  if Count = 0 then
    exit;
  SetLength(Sql, Count);
  Status := Marshal.AllocHGlobal(Count * SizeOf(ISC_STATUS));
{$IFDEF CLR}
  SqlPtr := Marshal.AllocHGlobal(Count * SizeOf(IntPtr));
  SetLength(SqlPtrs, Count);
{$ELSE}
  SqlPtr := Sql;
{$ENDIF}
  RowsAff := Marshal.AllocHGlobal(Count * SizeOf(Longword));
  try
    for i := 0 to Count - 1 do begin
      Sql[i] := StringToPtrGDS(Statements[i], FConnection.FUseUnicode);
    {$IFDEF CLR}
      Len := Length(Sql[i]);
      SqlPtrs[i] := Marshal.AllocHGlobal(Len + 1);
      Marshal.Copy(Sql[i], 0, SqlPtrs[i], Len);
      Marshal.WriteByte(SqlPtrs[i], Len, 0);
      Marshal.WriteIntPtr(SqlPtr, i * SizeOf(IntPtr), SqlPtrs[i]);
    {$ENDIF}
    end;
    GDS.Busy;
    Res := GDS.isc_dsql_batch_execute_immed(Status, FConnection.FDatabaseHandle,
      FTransaction.FTransactionHandle, FConnection.FSQLDialect, Count, SqlPtr,
      RowsAff);
    GDS.Release;
    Check(Res);
  finally
    Marshal.FreeHGlobal(Status);
    Marshal.FreeHGlobal(RowsAff);
    for i := 0 to Count - 1 do begin
      FreeStringGDS(Sql[i], FConnection.FUseUnicode);
    {$IFDEF CLR}
      Marshal.FreeHGlobal(SqlPtrs[i]);
    {$ENDIF}
    end;
  {$IFDEF CLR}
    Marshal.FreeHGlobal(SqlPtr);
  {$ENDIF}
  end;
end;

procedure TGDSCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    if GetActive then
      Finish;

    inherited;

    FConnection := TGDSConnection(Value);
  end;
end;

procedure TGDSCommand.SetTransaction(Value: TCRTransaction);
begin
  if Value <> FTransaction then begin
    if GetActive then
      Finish;

    FTransaction := TGDSTransaction(Value);
  end;
end;

function TGDSCommand.GetSQLType: integer;
begin
  Result := FSQLType;
end;

function TGDSCommand.GetSQLTypeEx: integer;
begin
  if FParsedSQLType = SQL_EX_EXECUTE_BLOCK then
    Result := FParsedSQLType
  else
    Result := FSQLType;
end;

function TGDSCommand.GetStmtHandle: TISC_STMT_HANDLE;
begin
  Result := nil;
  if FStmtHandle <> nil then
    Result := Marshal.ReadIntPtr(FStmtHandle);
end;

function TGDSCommand.GetCursorState: TCursorState; 
begin
  Result := FState;
end;

procedure TGDSCommand.SetCursorState(Value: TCursorState);
begin
  FState := Value;
end;

function TGDSCommand.GetPlan: string;
const
  Buffer_Size = 16384;
var
  InfoType, Buffer: IntPtr;
  PlanLength: integer;
  Res: ISC_STATUS;
begin
  Result := '';
  if GetPrepared and (FSQLType in [SQL_SELECT, SQL_UPDATE, SQL_DELETE,
    SQL_EXEC_PROCEDURE, SQL_SELECT_FOR_UPD]) then begin
    InfoType := Marshal.AllocHGlobal(SizeOf(Byte));
    Buffer := Marshal.AllocHGlobal(Buffer_Size);
    try
      Marshal.WriteByte(InfoType, isc_info_sql_get_plan);
      GDS.Busy;
      Res := GDS.isc_dsql_sql_info(FStatusVector, FStmtHandle, 2, InfoType, Buffer_Size, Buffer);
      GDS.Release;
      Check(Res);
      if Marshal.ReadByte(Buffer) = isc_info_sql_get_plan then begin
        PlanLength := Marshal.ReadInt16(Buffer, 1);
        Result := string(Marshal.PtrToStringAnsi(PtrOffset(Buffer, 3), PlanLength));
        Result := Trim(Result);
      end;  
    finally
      Marshal.FreeHGlobal(InfoType);
      Marshal.FreeHGlobal(Buffer);
    end;
  end;   
end;

function TGDSCommand.GetGDS: TGDS;
begin
  Result := FConnection.FGDS;
end;

function TGDSCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prQueryRowsAffected:
      FQueryRowsAffected := Value;
    prParsedSQLType:
      FParsedSQLType := Value;
    prCacheBlobs:
      FCacheBlobs := Value;
    prStreamedBlobs:
      FStreamedBlobs := Value;
    prCacheArrays:
      FCacheArrays := Value;
    prUseDescribeParams:
      FUseDescribeParams := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSCommand.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prSQLType:
      Value := FSQLType;
    prRowsProcessed: begin
      Value := GetRowsAffected;
      if Value = -1 then
        Value := 0;
    end;
    prQueryRowsAffected:
      Value := FQueryRowsAffected;
    prRowsInserted:
      Value := FRowsInserted;
    prRowsUpdated:
      Value := FRowsUpdated;
    prRowsDeleted:
      Value := FRowsDeleted;
    prParsedSQLType:
      Value := FParsedSQLType;
    prPlan:
      Value := GetPlan;
    prCacheBlobs:
      Value := FCacheBlobs;
    prStreamedBlobs:
      Value := FStreamedBlobs;
    prCacheArrays:
      Value := FCacheArrays;
    prUseDescribeParams:
      Value := FUseDescribeParams;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

{ TSQLDA }

procedure TSQLDA.AllocSQLDA(n: integer);
var
  i: integer;
begin
  if ((FXSQLDA = nil) or (sqln <> n)) and (n >= 0) then begin
    FreeSQLDA;

    FXSQLDA := Marshal.AllocHGlobal(XSQLDA_LENGTH(n, FXSQLVARType));
    FillChar(FXSQLDA, XSQLDA_LENGTH(n, FXSQLVARType), 0);
    if FXSQLVARType = vtGDS then
      Version := SQLDA_VERSION1
    else
      Version := SQLDA_VERSION2;
    SetLength(FXSQLVARs, n);
    for i := 0 to n - 1 do begin
      FXSQLVARs[i] := TSQLVARAccessor.Create(Self);
      if FXSQLVARType = vtGDS then
        FXSQLVARs[i].SetXSQLVAR(PtrOffset(FXSQLDA, XSQLDA_LENGTH(0, FXSQLVARType) +  i * SizeOfXSQLVAR_V1), FXSQLVARType)
      else
        FXSQLVARs[i].SetXSQLVAR(PtrOffset(FXSQLDA, XSQLDA_LENGTH(0, FXSQLVARType) +  i * SizeOfXSQLVAR), FXSQLVARType);
    end;
    sqln := n;
    sqld := n;
  end;
end;

constructor TSQLDA.Create(Connection: TGDSConnection; SQLVARType: TXSQLVARType);
begin
  inherited Create;

  FConnection := Connection;
  FXSQLVARType := SQLVARType;
end;

destructor TSQLDA.Destroy;
begin
  FreeSQLDA;

  inherited;
end;

procedure TSQLDA.FreeSQLDA;
var
  i: integer;
begin
  if FXSQLDA <> nil then begin
    for i := 0 to length(FXSQLVARS) - 1 do
      FXSQLVARS[i].Free;
    SetLength(FXSQLVARs, 0);
    Marshal.FreeHGlobal(FXSQLDA);
  end;
end;

function TSQLDA.GetVersion: Short;
begin
  Result := Marshal.ReadInt16(FXSQLDA);
end;

procedure TSQLDA.SetVersion(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLDA, Value);
end;

function TSQLDA.GetSqldaid: String;
begin
  //unused
end;

procedure TSQLDA.SetSqldaid(const Value: String);
begin
  //unused
end;

function TSQLDA.GetSqldabc: Long;
begin
  Result := Marshal.ReadInt32(FXSQLDA, OffsetOf_XSQLDA_sqldabc);
end;

procedure TSQLDA.SetSqldabc(const Value: Long);
begin
  Marshal.WriteInt32(FXSQLDA, OffsetOf_XSQLDA_sqldabc, Value);
end;

function TSQLDA.GetSqln: Short;
begin
  Result := Marshal.ReadInt16(FXSQLDA, OffsetOf_XSQLDA_sqln);
end;

procedure TSQLDA.SetSqln(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLDA, OffsetOf_XSQLDA_sqln, Value);
end;

function TSQLDA.GetSqld: Short;
begin
  Result := Marshal.ReadInt16(FXSQLDA, OffsetOf_XSQLDA_sqld);
end;

procedure TSQLDA.SetSqld(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLDA, OffsetOf_XSQLDA_sqld, Value);
end;

function TSQLDA.GetSqlvar: IntPtr;
begin
  Result := Marshal.ReadIntPtr(FXSQLDA, OffsetOf_XSQLDA_sqlvar);
end;

procedure TSQLDA.SetSqlvar(const Value: IntPtr);
begin
  Marshal.WriteIntPtr(FXSQLDA, OffsetOf_XSQLDA_sqlvar, Value);
end;

function TSQLDA.GetSQLVars(Index: integer): TSQLVARAccessor;
begin
  Result := FXSQLVars[Index];
end;

{ TSQLVARAccessor }

constructor TSQLVARAccessor.Create(AOwner: TSQLDA);
begin
  inherited Create;

  FOwner := AOwner;
end;

function TSQLVARAccessor.GetAliasName: _string;
begin
  if FXSQLVARType = vtGDS7 then
    ReadStringValue(OffsetOf_XSQLVAR_aliasname, Result)
  else
    ReadStringValue(OffsetOf_XSQLVAR_V1_aliasname, Result);
end;

procedure TSQLVARAccessor.SetAliasName(const Value: _string);
begin
  if FXSQLVARType = vtGDS7 then
    WriteStringValue(OffsetOf_XSQLVAR_aliasname, Value)
  else
    WriteStringValue(OffsetOf_XSQLVAR_V1_aliasname, Value);
end;

function TSQLVARAccessor.GetAliasName_Length: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_aliasname_length)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_aliasname_length);
end;

procedure TSQLVARAccessor.SetAliasName_Length(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_aliasname_length, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_aliasname_length, Value);
end;

function TSQLVARAccessor.GetOwnName: _string;
begin
  if FXSQLVARType = vtGDS7 then
    ReadStringValue(OffsetOf_XSQLVAR_ownname, Result)
  else
    ReadStringValue(OffsetOf_XSQLVAR_V1_ownname, Result);
end;

procedure TSQLVARAccessor.SetOwnName(const Value: _string);
begin
  if FXSQLVARType = vtGDS7 then
    WriteStringValue(OffsetOf_XSQLVAR_ownname, Value)
  else
    WriteStringValue(OffsetOf_XSQLVAR_V1_ownname, Value);
end;

function TSQLVARAccessor.GetOwnName_Length: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_ownname_length)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_ownname_length);
end;

procedure TSQLVARAccessor.SetOwnName_Length(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_ownname_length, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_ownname_length, Value);
end;

function TSQLVARAccessor.GetRelName: _string;
begin
  if FXSQLVARType = vtGDS7 then
    ReadStringValue(OffsetOf_XSQLVAR_relname, Result)
  else
    ReadStringValue(OffsetOf_XSQLVAR_V1_relname, Result);
end;

procedure TSQLVARAccessor.SetRelName(const Value: _string);
begin
  if FXSQLVARType = vtGDS7 then
    WriteStringValue(OffsetOf_XSQLVAR_relname, Value)
  else
    WriteStringValue(OffsetOf_XSQLVAR_V1_relname, Value);
end;

function TSQLVARAccessor.GetRelName_Length: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_relname_length)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_relname_length);
end;

procedure TSQLVARAccessor.SetRelName_Length(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_relname_length, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_relname_length, Value);
end;

function TSQLVARAccessor.GetSqlName: _string;
begin
  if FXSQLVARType = vtGDS7 then
    ReadStringValue(OffsetOf_XSQLVAR_sqlname, Result)
  else
    ReadStringValue(OffsetOf_XSQLVAR_V1_sqlname, Result);
end;

procedure TSQLVARAccessor.SetSqlName(const Value: _string);
begin
  if FXSQLVARType = vtGDS7 then
    WriteStringValue(OffsetOf_XSQLVAR_sqlname, Value)
  else
    WriteStringValue(OffsetOf_XSQLVAR_V1_sqlname, Value);
end;

function TSQLVARAccessor.GetSqlName_length: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlname_length)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlname_length);
end;

procedure TSQLVARAccessor.SetSqlName_length(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlname_length, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlname_length, Value);
end;

function TSQLVARAccessor.GetSqlInd: IntPtr;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_sqlind)
  else
    Result := Marshal.ReadIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlind);
end;

procedure TSQLVARAccessor.SetSqlInd(const Value: IntPtr);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_sqlind, Value)
  else
    Marshal.WriteIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlind, Value);
end;

function TSQLVARAccessor.GetSqlData: IntPtr;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_sqldata)
  else
    Result := Marshal.ReadIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqldata);
end;

procedure TSQLVARAccessor.SetSqlData(const Value: IntPtr);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_sqldata, Value)
  else
    Marshal.WriteIntPtr(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqldata, Value);
end;

function TSQLVARAccessor.GetSqlLen: Word;
begin
  if FXSQLVARType = vtGDS7 then
    Result := word(Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqllen))
  else
    Result := word(Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqllen));
end;

procedure TSQLVARAccessor.SetSqlLen(const Value: Word);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqllen, SmallInt(Value))
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqllen, SmallInt(Value));
end;

function TSQLVARAccessor.GetSqlSubtype: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlsubtype)
  else
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlsubtype);
end;

procedure TSQLVARAccessor.SetSqlSubtype(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlsubtype, Value)
  else
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_V1_sqlsubtype, Value);
end;

function TSQLVARAccessor.GetSqlPrecision: Short;
begin
  if FXSQLVARType = vtGDS7 then
    Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlprecision)
  else
    Result := 0;
end;

procedure TSQLVARAccessor.SetSqlPrecision(const Value: Short);
begin
  if FXSQLVARType = vtGDS7 then
    Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlprecision, Value);
end;

function TSQLVARAccessor.GetSqlScale: Short;
begin
  Result := Marshal.ReadInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlscale);
end;

procedure TSQLVARAccessor.SetSQLScale(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLVAR, OffsetOf_XSQLVAR_sqlscale, Value);
end;

function TSQLVARAccessor.GetSqlType: Short;
begin
  Result := Marshal.ReadInt16(FXSQLVAR) and (not 1);
end;

procedure TSQLVARAccessor.SetSqlType(const Value: Short);
begin
  Marshal.WriteInt16(FXSQLVAR, Value);
end;

function TSQLVARAccessor.GetIsNullable: boolean;
begin
  Result := (Marshal.ReadInt16(FXSQLVAR) and 1) = 1;
end;

procedure TSQLVARAccessor.ReadStringValue(Offset: integer; var Value: _string);
var
  Size: integer;
begin
  Size := Marshal.ReadInt16(FXSQLVAR, Offset - SizeOf(Short));
  Value := PtrToStringGDS(PtrOffset(FXSQLVAR, Offset), Size, FOwner.FConnection.FUseUnicode);
end;

procedure TSQLVARAccessor.WriteStringValue(Offset: integer; const Value: _string);
begin
  Marshal.WriteInt16(FXSQLVAR, Offset - SizeOf(Short), Length(Value));
  CopyBufferAnsi(AnsiString(Value), PtrOffset(FXSQLVAR, Offset), Length(Value));
end;

procedure TSQLVARAccessor.SetXSQLVAR(XSQLVAR: IntPtr; XSQLVARType: TXSQLVARType);
begin
  FXSQLVAR := XSQLVAR;
  FXSQLVARType := XSQLVARType;
end;

{ TGDSParamDesc }

procedure TIBCParamDesc.AllocBuffer;
begin
  FInternalSize := 0;
  case FDataType of
    dtString:
      FValueSize := 2 + FSize + 1;        // 2 byte lenght + 1 terminator byte
    dtFixedChar:
      FValueSize := FSize + 1;            // 1 terminator byte
    dtWideString:
      FValueSize := 2 + (FSize + 1) * 2;
    dtFixedWideChar:
      FValueSize := (FSize + 1) * 2;
    dtBytes:
      FValueSize := FSize + 1;            // 1 terminator byte
    dtVarBytes:
      FValueSize := 2 + FSize + 1;        // 2 byte lenght + 1 terminator byte
    dtSmallInt:
      FValueSize := SizeOf(SmallInt);
    dtInteger:
      FValueSize := sizeof(integer);
    dtDateTime, dtDate, dtTime: begin
      FValueSize := SizeOf(TDateTime);
      FInternalSize := SizeOf(TISC_QUAD);
    end;
    dtFloat:
      FValueSize := SizeOf(Double);
    dtBoolean:
      FValueSize := 2; // as smallint
    dtLargeint:
      FValueSize := SizeOf(Int64);
    dtCurrency, dtBCD:
      FValueSize := SizeOf(Currency);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    dtFMTBCD: begin
      FValueSize := SizeOfTBcd;
      FInternalSize := SizeOf(Int64);
    end;
  {$ENDIF}
  {$ENDIF}
    dtMemo, dtWideMemo, dtBlob, dtArray:
      FValueSize := 0;
    dtUnknown: ; //To avoid exception during AssembleSQL->WriteParam(False) for UpdateSQLs
  else
    Assert(False, SUnknownDataType);
  end;

  if FValueSize > 0 then begin
    FValue := Marshal.AllocHGlobal(FValueSize);
    FillChar(FValue, FValueSize, $00);
  end;

  if FInternalSize > 0 then begin
    FInternalValue := Marshal.AllocHGlobal(FInternalSize);
    FillChar(FInternalValue, FInternalSize, $00);
  end;

  Marshal.WriteInt16(FIndicator, -1);
end;

procedure TIBCParamDesc.FreeBuffer;
begin
  if (FValueSize > 0) and (FValue <> nil) then begin
    Marshal.FreeHGlobal(FValue);
  end;
  FValue := nil;
  if (FInternalSize > 0) and (FInternalValue <> nil) then begin
    Marshal.FreeHGlobal(FInternalValue);
    FInternalSize := 0;
  end;
  FInternalValue := nil; //This value could be set at Blob or Array param binding
end;

function TIBCParamDesc.GetAsAnsiString: AnsiString;
var
  ValPtr, Ptr: IntPtr;
  Len: integer;
begin
  case FDataType of
    dtString:
    {$IFDEF LITE}
      if ParamType <> pdOutput then
        ValPtr := FValue
      else
    {$ENDIF}
        ValPtr := PtrOffset(FValue, 2);
    dtFixedChar:
      ValPtr := FValue;
    else
      ValPtr := nil;
  end;
  if FConvertEOL then begin
    Ptr := Marshal.AllocHGlobal(FSize * 4);
    try
      Len := AddCRString(ValPtr, Ptr, FSize);
      Result := Marshal.PtrToStringAnsi(Ptr, Len);
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end
  else
    Result := Marshal.PtrToStringAnsi(ValPtr);
end;

procedure TIBCParamDesc.SetAsAnsiString(const Value: AnsiString);
var
  SPtr: IntPtr;
  SLen: integer;
begin
  if FDataType <> dtFixedChar then
    SetDataType(dtString);

  SLen := Length(Value);
  if SLen > FSize then
    SLen := FSize;
  if SLen > 0 then begin
    SPtr := Marshal.StringToHGlobalAnsi(Value);
    try
      if FConvertEOL then
        SLen := RemoveCRString(SPtr, FValue, SLen, Length(Value))
      else begin
        CopyBuffer(SPtr, FValue, SLen);
      end;
    finally
      Marshal.FreeCoTaskMem(SPtr);
    end;
  end;
  Marshal.WriteByte(FValue, SLen, 0);
  SetNull(False);
end;

function TIBCParamDesc.GetAsWideString: WideString;
var
  ValPtr: IntPtr;
  Ptr: IntPtr;
  Len: integer;
begin
  case FDataType of
    dtString:
      Result := WideString(GetAsAnsiString);
    dtWideString, dtFixedWideChar: begin
      if FDataType = dtWideString then
      {$IFDEF LITE}
        if ParamType <> pdOutput then
          ValPtr := FValue
        else
      {$ENDIF}
          ValPtr := PtrOffset(FValue, 2)
      else
        ValPtr := FValue;
      if FConvertEOL then begin
        Ptr := Marshal.AllocHGlobal(FSize * 4);
        try
          Len := AddCRUnicode(ValPtr, Ptr, FSize);
          Result := Marshal.PtrToStringUni(Ptr, Len);
        finally
          Marshal.FreeHGlobal(Ptr);
        end;
      end
      else
        Result := Marshal.PtrToStringUni(ValPtr);
    end;
  else
    Result := '';
  end;
end;

procedure TIBCParamDesc.SetAsWideString(const Value: WideString);
var
  SPtr: IntPtr;
  SLen: integer;
begin
  if FDataType <> dtFixedWideChar then
    SetDataType(dtWideString);

  SLen := Length(Value);
  if SLen > FSize then
    SLen := FSize;
  if SLen > 0 then begin
    SPtr := Marshal.StringToHGlobalUni(Value);
    try
      if FConvertEOL then
        SLen := RemoveCRUnicode(SPtr, FValue, SLen, Length(Value))
      else begin
        CopyBuffer(SPtr, FValue, SLen * 2);
      end;
    finally
      Marshal.FreeCoTaskMem(SPtr);
    end;
  end;
  Marshal.WriteInt16(FValue, SLen * 2, 0);
  SetNull(False);
end;

function TIBCParamDesc.GetAsInteger: integer;
begin
  if (not GetNull) and (FDataType = dtInteger) then
    Result := Marshal.ReadInt32(FValue)
  else
    Result := 0;
end;

procedure TIBCParamDesc.SetAsInteger(Value: integer);
begin
  SetDataType(dtInteger);
  Marshal.WriteInt32(FValue, Value);
  SetNull(False);
end;

function TIBCParamDesc.GetAsSmallInt: SmallInt;
begin
  if (not GetNull) and (FDataType = dtSmallInt) then
    Result := Marshal.ReadInt16(FValue)
  else
    Result := 0;
end;

procedure TIBCParamDesc.SetAsSmallInt(Value: SmallInt);
begin
  SetDataType(dtSmallInt);
  Marshal.WriteInt16(FValue, Value);
  SetNull(False);
end;

function TIBCParamDesc.GetAsInt64: Int64;
begin
  if (not GetNull) and (FDataType = dtInt64) then
    Result := Marshal.ReadInt64(FValue)
  else
    Result := 0;
end;

procedure TIBCParamDesc.SetAsInt64(Value: Int64);
begin
  SetDataType(dtInt64);
  Marshal.WriteInt64(FValue, Value);
  SetNull(False);
end;

function TIBCParamDesc.GetAsCurr: Currency;
begin
  if (not GetNull) and ((FDataType = dtCurrency) or (FDataType = dtBCD)) then
   {$IFDEF CLR}
    Result := Borland.Delphi.System.Currency.FromOACurrency(Marshal.ReadInt64(FValue))
   {$ELSE}
    Result := PCurrency(FValue)^
   {$ENDIF}
  else
    Result := 0;
end;

procedure TIBCParamDesc.SetAsCurr(Value: Currency);
begin
  SetDataType(dtCurrency);
  Marshal.WriteInt64(FValue, {$IFNDEF CLR}PInt64(@Value)^{$ELSE}Value.ToOACurrency{$ENDIF});
  SetNull(False);
end;

{$IFDEF VER6P}
{$IFNDEF FPC}
function TIBCParamDesc.GetAsFmtBCD: TBcd;
{$IFDEF CLR}
var
  Temp: TBytes;
{$ENDIF}
begin
  if (not GetNull) and (FDataType = dtFMTBCD) then
{$IFDEF CLR}
    begin
      SetLength(Temp, SizeOfTBcd);
      Marshal.Copy(FValue, Temp, 0, SizeOfTBcd);
      Result := TBcd.FromBytes(Temp);
    end
{$ELSE}
    Result := PBcd(FValue)^
{$ENDIF}
  else
    Result := NullBcd;
end;

procedure TIBCParamDesc.SetAsFmtBCD(const Value: TBcd);
{$IFDEF CLR}
var
  Temp: TBytes;
{$ENDIF}  
begin
  SetDataType(dtFMTBCD);
{$IFDEF CLR}
  Temp := TBcd.ToBytes(Value);
  Marshal.Copy(Temp, 0, FValue, Length(Temp));
{$ELSE}
  PBcd(FValue)^ := Value;
{$ENDIF}
  SetNull(False);
end;
{$ENDIF}
{$ENDIF}

function TIBCParamDesc.GetAsFloat: double;
begin
  if not GetNull and (FDataType = dtFloat) then
    Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FValue))
  else
    Result := 0;
end;

procedure TIBCParamDesc.SetAsFloat(Value: double);
begin
  Marshal.WriteInt64(FValue, BitConverter.DoubleToInt64Bits(Value));
  SetNull(False);
end;

function TIBCParamDesc.GetAsDateTime: TDateTime;
begin
  if not GetNull and (FDataType in [dtDateTime, dtDate, dtTime]) then begin
    Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FValue))
  end
  else
    Result := 0;
end;

procedure TIBCParamDesc.SetAsDateTime(Value: TDateTime);
begin
  // should call SetDataType if FSize is 4 (dtInteger)
  Marshal.WriteInt64(FValue, BitConverter.DoubleToInt64Bits(Value));
  SetNull(False);
end;

function TIBCParamDesc.GetAsBoolean: boolean;
begin
  if not GetNull and (FDataType = dtBoolean) then
    Result := Marshal.ReadInt16(FValue) = 1
  else
    Result := False;
end;

procedure TIBCParamDesc.SetAsBoolean(Value: boolean);
begin
  if Value then
    Marshal.WriteInt16(FValue, 1)
  else
    Marshal.WriteInt16(FValue, 0);
  SetNull(False);
end;

function TIBCParamDesc.GetAsVariant: variant;
var
  Len, i: integer;
begin
  if GetNull then begin
    Result := Null;
    Exit;
  end;

  case FDataType of
    dtBoolean:
      Result := GetAsBoolean;
    dtString, dtFixedChar:
      Result := GetAsAnsiString;
    dtWideString, dtFixedWideChar:
      Result := GetAsWideString;
    dtInteger:
      Result := GetAsInteger;
    dtFloat:
      Result := GetAsFloat;
    dtDateTime, dtDate, dtTime:
      Result := GetAsDateTime;
    dtUnknown:
      Result := Unassigned;
    dtSmallint:
      Result := GetAsSmallInt;
    dtLargeint: begin
    {$IFDEF VER6P}
      Result := GetAsInt64;
    {$ELSE}
      TVarData(Result).VType := VT_DECIMAL;
      Decimal(Result).lo64 := GetAsInt64;
    {$ENDIF}
    end;
    dtCurrency, dtBCD:
      Result := GetAsCurr;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    dtFMTBCD:
      VarFMTBcdCreate(Result, GetAsFmtBCD);
  {$ENDIF}
  {$ENDIF}
    dtBytes: begin
      Result := VarArrayCreate([0, Size - 1], varByte);
      for i := 0 to Size - 1 do begin
        Result[i] := Marshal.ReadByte(FValue, i);
      end;
    end;
    dtVarBytes: begin
      Len := Word(Marshal.ReadInt16(FValue));
      Result := VarArrayCreate([0, Len - 1], varByte);
      for i := 0 to Len - 1 do begin
        Result[i] := Marshal.ReadByte(FValue, 2 + i);
      end;
    end;
    else
      Result := Null;
  end;
end;

procedure TIBCParamDesc.SetAsVariant(const Value: variant);
var
  Len, BufferOffset: integer;
{$IFDEF CLR}
  Buf: TBytes;
  i: integer;
{$ENDIF}  
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    SetNull(True)
  else
  begin
    if FDataType = dtUnknown then
      case VarType(Value) of
        varSmallint, varInteger, varByte {$IFDEF VER6P}, varWord, varLongWord, varShortInt, varInt64{$ENDIF}:
          SetDataType(dtInteger);
        varSingle, varDouble, varCurrency:
          SetDataType(dtFloat);
        varDate:
          SetDataType(dtDateTime);
        varArray:
          SetDataType(dtBytes);
      else
        if VarIsStr(Value) then begin
          SetDataType(dtString);
          SetSize(Length(Value));
        end
        else
          raise EConvertError.Create(SUnknownDataType);
      end;

    case FDataType of
      dtBytes, dtVarBytes: begin
        if FDataType = dtBytes then
          BufferOffset := 0
        else
          BufferOffset := SizeOf(Word);

        if VarIsStr(Value) then begin
          Len := Length(Value);
          if Len > GetSize then
            Len := GetSize;
        {$IFDEF CLR}
          Buf := Encoding.Default.GetBytes(Copy(Value, 1, Len));
          Marshal.Copy(Buf, 0, PtrOffset(FValue, BufferOffset), Length(Buf));
          Marshal.WriteByte(PtrOffset(FValue, BufferOffset), Length(Buf), 0);
        {$ELSE}
          StrPLCopy(PtrOffset(FValue, BufferOffset), Value, Len);
        {$ENDIF}
        end
        else begin
        {$IFDEF CLR}
          Len := VarArrayHighBound(Value, 1) + 1;
          if Len > GetSize then
            Len := GetSize;
          Borland.Delphi.System.SetLength(Buf, Len);
          for i := 0 to High(Buf) do
            Buf[i] := VarArrayGet(Value, i);
          Marshal.Copy(Buf, 0, PtrOffset(FValue, BufferOffset), Length(Buf));
        {$ELSE}
          Len := TVarData(Value).VArray.Bounds[0].ElementCount;
          if Len > GetSize then
            Len := GetSize;
          Move(TVarData(Value).VArray.Data^, (PAnsiChar(FValue) + BufferOffset)^, Len);
        {$ENDIF}
        end;
        if FDataType = dtVarBytes then
          Marshal.WriteInt16(FValue, Len);
        SetNull(Len = 0);
      end;
      dtString, dtFixedChar:
        SetAsAnsiString(AnsiString(Value));
      dtWideString, dtFixedWideChar:
        SetAsWideString(Value);
      dtInteger:
        case VarType(Value) of
          varSmallint, varInteger, varByte, {$IFDEF VER6P}varWord, varLongWord, varShortInt, varInt64,{$ENDIF}
          varSingle, varDouble, varCurrency:
            SetAsInteger(Value);
        else
          if VarIsStr(Value) then
            SetAsInteger(StrToInt(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtDateTime, dtDate, dtTime:
        case VarType(Value) of
          varDate:
            SetAsDateTime(Value);
          varDouble, varSingle, varCurrency:
            SetAsDateTime(Double(Value));
        else
          if VarIsStr(Value) then
            SetAsDateTime(StrToDateTime(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtFloat:
        case VarType(Value) of
          varSmallint, varInteger, varByte, {$IFDEF VER6P}varWord, varLongWord, varShortInt, varInt64,{$ENDIF}
          varSingle, varDouble, varCurrency:
            SetAsFloat(Value);
        else
          if VarIsStr(Value) then
            SetAsFloat(StrToFloat(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtBoolean:
        case VarType(Value) of
          varBoolean:
            SetAsBoolean(Value);
          varSmallint, varInteger, varByte, {$IFDEF VER6P}varWord, varLongWord, varShortInt, varInt64,{$ENDIF}
          varSingle, varDouble, varCurrency:
            SetAsBoolean(Value = 0);
        end;
      dtSmallint:
        case VarType(Value) of
          varSmallint, varInteger, varByte, {$IFDEF VER6P}varWord, varLongWord, varShortInt, varInt64,{$ENDIF}
          varSingle, varDouble, varCurrency:
            SetAsSmallInt(Value);
        else
          if VarIsStr(Value) then
            SetAsSmallInt(StrToInt(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtLargeint:
        case VarType(Value) of
          varSmallint, varInteger, varByte, {$IFDEF VER6P}varWord, varLongWord, varShortInt, varInt64, {$ENDIF}
          varSingle, varDouble, varCurrency:
        {$IFNDEF VER6P}
            SetAsInt64(Integer(Value));
          varDecimal:
            SetAsInt64(Decimal(Value).lo64);
        {$ELSE}
            SetAsInt64(Value);
        {$ENDIF}
        else
          if VarIsStr(Value) then
            SetAsInt64(StrToInt64(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtCurrency, dtBCD:
        case VarType(Value) of
          varSmallint, varInteger, varByte, {$IFDEF VER6P}varWord, varLongWord, varShortInt, varInt64,{$ENDIF}
          varSingle, varDouble, varCurrency:
            SetAsCurr(Value);
        else
          if VarIsStr(Value) then
            SetAsCurr(StrToCurr(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
   {$IFDEF VER6P}
   {$IFNDEF FPC}
      dtFMTBCD:
        case VarType(Value) of
          varSmallint, varInteger, varByte, {$IFDEF VER6P}varWord, varLongWord, varShortInt, varInt64,{$ENDIF}
          varSingle, varDouble, varCurrency:
            SetAsFmtBCD(StrToBcd(FloatToStr(Value))) //Default variant conversion is 18,4 in other cases we lost value
        else
          if VarIsFMTBcd(Value) then
            SetAsFmtBCD(VarToBcd(Value))
          else
          if VarIsStr(Value) then
            SetAsFmtBCD(StrToBcd(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
   {$ENDIF}
   {$ENDIF}
    end;
  end;
end;

function TIBCParamDesc.GetValue: variant;
begin
  Result := GetAsVariant;
end;

procedure TIBCParamDesc.SetValue(const Value: variant);
begin
  SetAsVariant(Value);
end;

procedure TIBCParamDesc.SetDataType(Value: word);
begin
  if Value <> FDataType then begin
    FreeBuffer;

    FDataType := Value;

    AllocBuffer;
  end;
end;

procedure TIBCParamDesc.SetSize(Value: integer);
begin
  case FDataType of
    dtString, dtWideString, dtFixedChar, dtFixedWideChar, dtBytes, dtVarBytes: begin
      if Value <> FSize then begin
        FreeBuffer;
        FSize := Value;
        AllocBuffer;
      end;
    end;
  end;
end;

procedure TIBCParamDesc.SetValuePtr(Buf: IntPtr);
begin
  if GetDataType in [dtBlob, dtMemo, dtWideMemo, dtArray] then
    RaiseError(SParamIsNotStorable);

  case FDataType of
    dtWideString: begin
      StrLCopyW(PWChar(FValue), Buf, GetSize);
    end;
    dtString: begin
      StrLCopy(PAChar(FValue), Buf, GetSize);
    end;
  else
    CopyBuffer(Buf, FValue, GetSize);
  end
end;

function TIBCParamDesc.ValuePtr: IntPtr;
begin
  Result := FValue;
end;

function TIBCParamDesc.InternalValuePtr: IntPtr;
begin
  if FInternalValue = nil then
    Result := FValue
  else
    Result := FInternalValue;
end;

function TIBCParamDesc.GetNull: boolean;
begin
  if Marshal.ReadInt16(FIndicator) = -1 then
    Result := True
  else
    Result := False;
end;

procedure TIBCParamDesc.SetNull(const Value: boolean);
begin
  if Value = True then begin
    Marshal.WriteInt16(FIndicator, -1);
    case FDataType of
      dtString, dtFixedChar:
        Marshal.WriteByte(FValue, 0);
      dtWideString, dtFixedWideChar:
        Marshal.WriteInt16(FValue, 0);
      dtInteger:
        Marshal.WriteInt32(FValue, 0);
      dtFloat:
        Marshal.WriteInt64(FValue, 0);
      dtBoolean:
        Marshal.WriteInt16(FValue, 0);
      dtSmallint:
        Marshal.WriteInt16(FValue, 0);
      dtLargeint:
        Marshal.WriteInt64(FValue, 0);
      dtCurrency, dtBCD:
        Marshal.WriteInt64(FValue, 0);
   {$IFDEF VER6P}
   {$IFNDEF FPC}
      dtFMTBCD:
        FillChar(FValue, FValueSize, 0);
   {$ENDIF}
   {$ENDIF}
      dtDateTime, dtDate, dtTime:
        FillChar(FValue, FValueSize, 0);
      dtBlob, dtMemo, dtWideMemo:
        TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).Clear;
      dtBytes, dtVarBytes:
        Marshal.WriteInt16(FValue, 0);
      dtArray:
        TCustomIBCArray({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).ClearArray;
    end;
  end
  else
    Marshal.WriteInt16(FIndicator, 0);
end;

constructor TIBCParamDesc.Create;
begin
  inherited Create;

  FIndicator := Marshal.AllocHGlobal(SizeOf(SmallInt));
  Marshal.WriteInt16(FIndicator, -1);
end;

destructor TIBCParamDesc.Destroy;
begin
  FreeBuffer;
  Marshal.FreeHGlobal(FIndicator);

  inherited;
end;

function TIBCParamDesc.IndicatorPtr: IntPtr;
begin
  Result := FIndicator;
end;

procedure TIBCParamDesc.SyncIndicator;
var
  Ind: SHORT;
begin
  case FDataType of
    dtBlob, dtMemo, dtWideMemo, dtArray: begin
      if VarIsArray(FData) then
        Exit;
      if {$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF} = nil then
        Ind := -1
      else
        case FDataType of
          dtBlob, dtMemo, dtWideMemo:
            if TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).Size = 0 then
              Ind := -1
            else
              Ind := 0;
          dtArray:
            if TCustomIBCArray({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).IsNull then
              Ind := -1
            else
              Ind := 0;
        else
          Exit;
        end;
    end
  else
    if (FValue = nil) then
      Ind := -1
    else
      Exit;
  end;
  Marshal.WriteInt16(FIndicator, Ind);
end;

{ TIBCSQLInfo }

function TIBCSQLInfo.IdentCase: TIdentCase;
begin
  Result := icUpper;
end;

{ TGDSRecordSet }

constructor TGDSRecordSet.Create;
begin
  inherited Create;

  FFetchRows := 25;
end;

destructor TGDSRecordSet.Destroy;
begin
  Close;

  FreeFetchBlock;
  FFetchedRows := 0;
  FFieldXSQLDA.Free;

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

procedure TGDSRecordSet.CreateCommand;
begin
  SetCommand(TGDSCommand.Create);
end;

procedure TGDSRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TGDSCommand(Value);
  if FCommand <> nil then begin
    FConnection := FCommand.FConnection;
  end;
end;

procedure TGDSRecordSet.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    FConnection.IBCError(FCommand.FStatusVector, True, Component);
end;

{ Open /Close }

function TGDSRecordSet.IsFullReopen: boolean;
begin
  Result := False;
end;

procedure TGDSRecordSet.InternalPrepare;
begin
  FCommand.Prepare;
  SetCommandType;
end;

procedure TGDSRecordSet.SetCommandType;
begin
  case FCommand.FSQLType of
    SQL_UNKNOWN:
      CommandType := ctUnknown;
    SQL_SELECT, SQL_SELECT_FOR_UPD:
      CommandType := ctCursor;
    else
      CommandType := ctStatement;
  end;
end;

procedure TGDSRecordSet.InternalUnPrepare;
begin
  try
    inherited;
  finally
    CommandType := ctUnknown;
  end;
end;

procedure TGDSRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  inherited;
end;

procedure TGDSRecordSet.InternalClose;
var
  Res: integer;
begin
  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  Res := 0;
  if Active and (FCommand.GetStmtHandle <> nil) then
    case FCommand.FSQLType of
      SQL_SELECT, SQL_SELECT_FOR_UPD: begin
        GDS.Busy;
        Res := GDS.isc_dsql_free_statement(FCommand.FStatusVector, FCommand.FStmtHandle, DSQL_close);
        GDS.Release;
      end;
    end;

  inherited;

  FreeFetchBlock;
  if not Prepared then
    InternalUnprepare;
  Check(Res);
end;

procedure TGDSRecordSet.ExecCommand; // Execute command
var
  NeedPrepare: boolean;
begin
  NeedPrepare := (CommandType <> ctCursor) and not Prepared;
  if NeedPrepare then
    FCommand.Prepare;
  try
    inherited;

    SetCommandType;
  finally // for Unprepare on Exception
    if (CommandType <> ctCursor) and NeedPrepare then
      FCommand.Unprepare;
  end;
end;

procedure TGDSRecordSet.ExecFetch(DisableInitFields: boolean);
var
  OldExecuted: boolean;
begin
  FFetchedRows := 0;
  FCommand.Executing := True;
  try
    OldExecuted := (FCommand.GetCursorState = csExecuted);

    if {FCommand.NativeCursor and} not OldExecuted then begin
      if not Prepared then
        InternalPrepare;
      FCommand.BindParams;
      FCommand.InternalExecute;
    end;

    if not RowsReturn then
      RaiseError(SNoRows);

    if not DisableInitFields and
      (not Prepared or (Fields.Count = 0) or NeedInitFieldsOnFetch)
    then
      InitFields;

  except
    on E: Exception do begin
      try
        if Assigned(FAfterExecFetch) then
          FAfterExecFetch(False);
      finally
        FCommand.Executing := False;
      end;
      raise;
    end;
  end;
  try
    if FFetchAll then
      try
        FetchAll;
      except
        Active := True;
        InternalClose;
        Active := False;
        raise;
      end;

    if Assigned(FAfterExecFetch) then
      FAfterExecFetch(True);
  finally
    FCommand.Executing := False;
  end;
end;

procedure TGDSRecordSet.Reopen;
var
  Res: ISC_STATUS;
begin
  FreeData;
  InitData;
  if Assigned(FOnDataChanged) then
    // perform dataset resync to prevent AV if grid is repainted in BeforeFetch/AfterFetch events
    FOnDataChanged;

  if Active and (FCommand.GetStmtHandle <> nil) then begin
    GDS.Busy;
    Res := GDS.isc_dsql_free_statement(FCommand.FStatusVector, FCommand.FStmtHandle, DSQL_close);
    GDS.Release;
    Check(Res);
  end;
  InternalOpen(True);
end;

procedure TGDSRecordSet.Disconnect;
begin
  if (FConnection <> nil) and not FConnection.InProcessError and not GDS.AlerterFatalError then begin
    //Cache connection depenednt information
    GetIsFBConnection;
    GetMinorServerVersion;
    GetMajorServerVersion;
    GetUseUnicode;
    GetCharLength;
    GetSQLDialect;
  end;
  
  inherited;
end;

{ Fields}

function TGDSRecordSet.GetIndicatorSize: word;
begin
  Result := FieldCount * SizeOf(Short);
end;

procedure TGDSRecordSet.InternalInitFields;

  procedure FillTablesAliases;
  var
    Parser: TIBCParser;
    TableName: _string;// Table or view name
    StLex, Alias: _string;
    CodeLexem, BracketCount: integer;
    TableInfo: TCRTableInfo;
  begin
    TablesInfo.BeginUpdate;
    Parser := TIBCParser.Create(FCommand.SQL);
    Parser.OmitBlank := False;
    Parser.OmitComment := True;
    try
      if Parser.ToLexem(lxSELECT) <> lcEnd then begin
        BracketCount := 0;
        repeat
          CodeLexem := Parser.GetNext(StLex);
          if CodeLexem = lcSymbol then begin
            if StLex = '(' then
              Inc(BracketCount)
            else
              if StLex = ')' then
                Dec(BracketCount);
          end;
        until ((CodeLexem = lxFROM) and (BracketCount = 0)) or (CodeLexem = lcEnd);

        if CodeLexem <> lcEnd then
          repeat
            repeat
             CodeLexem := Parser.GetNext(StLex);// Omit blank
            until CodeLexem <> lcBlank;

            // TableName
            TableName := StLex;
            while True do begin
              CodeLexem := Parser.GetNext(StLex);

              if (Length(StLex) > 0) and ((StLex[1] = ',') or (StLex[1] = '(') or (StLex[1] = ')')) then  // InterBase syntax "select * from storedproc(:p1,:p2)"
                Break;

              if (CodeLexem <> lcEnd) and (CodeLexem <> lcBlank) then
                TableName := TableName + StLex
              else
                Break;
            end;

            if CodeLexem = lcBlank then begin
              // 'AS' clause
              if Parser.GetNext(Alias) = lxAS then
                Parser.GetNext(Alias) // Omit blank
              else
                Parser.Back;

              // Alias
              if Parser.GetNext(Alias) = lcIdent then begin
                repeat
                 CodeLexem := Parser.GetNext(StLex);// Omit blank
                until CodeLexem <> lcBlank;
              end
              else begin
                if Alias = ',' then
                  StLex := ','
                else
                  Parser.Back;

                Alias := '';
              end;
            end
            else
              Alias := '';

            Assert(TableName <> '', 'TableName cannot be empty');
            TableName := FCommand.SQLInfo.NormalizeName(TableName);
            TableInfo := TablesInfo.FindByName(TableName);
            if TableInfo = nil then begin
              TableInfo := TablesInfo.FindByName(FCommand.SQLInfo.UnQuote(TableName));
              if TableInfo = nil then
                TableInfo :=  TablesInfo.Add;
              TableInfo.TableName := TableName; //Add/(Replace quoted) table name
              TableInfo.TableAlias := '';
            end;

            if Alias <> '' then
              TableInfo.TableAlias := FCommand.SQLInfo.NormalizeName(Alias);
          until (StLex <> ',');
      end;
    finally
      Parser.Free;
      TablesInfo.EndUpdate;
    end;
  end;

  procedure DescribeDefineFieldDesc(Field: TIBCFieldDesc; SQLVAR: TSQLVARAccessor; FieldNo: integer);
  var
    DataSize: integer;
    DataType: SmallInt;
    Precision, Scale: integer;
    IsNullable: boolean;
    TableInfo: TCRTableInfo;
    FldTableName: _string;
    ArrType: TIBCArrayType;

    procedure GetBlobCharset(Field: TIBCFieldDesc; const FldTableName: _string);
    //This operation could be done by isc_blob_lookup_desc2 or by Query to system table
    var
      TableName, FieldName: PStr;
      BlobDesc: IntPtr;
      Res: ISC_STATUS;
    begin
      TableName := StringToPtrGDS(FldTableName, FConnection.FUseUnicode);
      FieldName := StringToPtrGDS(Field.ActualName, FConnection.FUseUnicode);
      BlobDesc := Marshal.AllocHGlobal(SizeOfBLOB_DESC);
      try
        GDS.Busy;
        Res := GDS.isc_blob_lookup_desc(FCommand.FStatusVector, FConnection.FDatabaseHandle,
          FCommand.FTransaction.FTransactionHandle, TableName, FieldName, BlobDesc, nil);
        GDS.Release;
        if Res <> isc_field_not_defined then begin
          Check(Res);
          Field.CharsetID := Marshal.ReadInt16(BlobDesc, 2);
        end;
      finally
        FreeStringGDS(TableName, FConnection.FUseUnicode);
        FreeStringGDS(FieldName, FConnection.FUseUnicode);
        Marshal.FreeHGlobal(BlobDesc);
      end;
    end;

  begin
    Field.Name := SQLVAR.aliasname;
    Field.ActualName := SQLVAR.sqlname;
    Field.ActualFieldNo := FieldNo;
    Field.ReadOnly := SQLVAR.relname = '';
    Field.CharsetID := 0;
    Field.Length := 0;
    Field.Scale := 0;
    Field.SubDataType := dtUnknown;
    FldTableName := FCommand.SQLInfo.QuoteIfNeed(SQLVAR.relname);

    if FldTableName <> '' then begin
      TableInfo := TablesInfo.FindByName(FldTableName);
      TablesInfo.BeginUpdate;
      try
        if TableInfo = nil then begin
          TableInfo := TablesInfo.Add;
          TableInfo.TableName := FldTableName;
          TableInfo.TableAlias := '';
        end;
      finally
        TablesInfo.EndUpdate;
      end;
      Field.TableInfo := TableInfo;
    end
    else
      Field.TableInfo := nil;
    DataSize := SQLVAR.sqllen;
    DataType := SQLVAR.sqltype;
    IsNullable := SQLVAR.IsNullable;

    case DataType of
      SQL_VARYING, SQL_TEXT: begin
        Field.CharsetID := Byte(SQLVAR.sqlsubtype); //LSB- CharsetID, MSB- CollateID
        if (Field.ActualName = 'DB_KEY') or (Field.ActualName = 'RDB$DB_KEY') then begin
          //DB_KEY field definition
          Field.Name := 'RDB$DB_KEY';       //Full DB_KEY name for correct Update/Insert/Delete SQLs generation
          Field.ActualName := 'RDB$DB_KEY';
        {$IFNDEF FPC}
          Field.DataType := dtBytes;
        {$ELSE}
          Field.DataType := dtString;
        {$ENDIF}
          Field.SubDataType := dtDbKey;
          Field.Size := DataSize + 1;
          Field.Length := DataSize;
          Field.ReadOnly := True;
        end
        else begin
        {$IFNDEF FPC}
          if not FFieldsAsString and (Field.SubDataType <> dtDbKey) and (Field.CharsetID = CH_OCTETS) then begin
            if (DataSize >= FlatBufferLimit) and not FFlatBuffers then begin
               Field.DataType := dtExtVarBytes;
               SQLVAR.sqltype := SQL_VARYING or 1; //Map all long OCTETS fields as VarChar
               Field.Size := SizeOf(IntPtr);
            end
            else begin
              if DataType = SQL_VARYING then begin
                Field.DataType := dtVarBytes;
                Field.Size := DataSize + 1 + SizeOf(Short); //+ Terminator + Size
              end
              else begin
                Field.DataType := dtBytes;
                Field.Size := DataSize + 1;
              end;
            end;
          end
          else
        {$ENDIF}
        {$IFDEF FPC}
          {$DEFINE SHORTSTRINGFIELD}
        {$ENDIF}
        {$IFNDEF VER6P}
          {$DEFINE SHORTSTRINGFIELD}
        {$ENDIF}
            if (FConnection.FUseUnicode) and (Field.CharsetID <> CH_OCTETS) then begin  //None and OCTETS are not converted to UTF8
              //if (Copy(Field.ActualName, 1, 4) <> 'RDB$') or (FConnection.FDBCharsetId = CH_UTF8) then begin
                DataSize := DataSize div FConnection.GetCharLength(Field.CharsetID);  //Size in Chars
                SQLVAR.sqllen := DataSize * FConnection.GetCharLength(Field.CharsetID); //to set proper field length
              //end;
              if ((DataSize <= 255) or FLongStrings)
               {$IFDEF SHORTSTRINGFIELD} and (DataSize < (8192 div SizeOf(WideChar))){$ENDIF} then begin
                if (DataSize >= FlatBufferLimit) and not FFlatBuffers or
                  ((DataSize +  SizeOf(Short)) > ($FFFF div SizeOf(WideChar))) then begin
                  Field.DataType := dtExtWideString;
                  Field.Size := SizeOf(PWideChar);
                end
                else begin
                  Field.DataType := dtWideString;
                  if DataType = SQL_TEXT then
                    Field.Size := (DataSize + 1) * SizeOf(WideChar)//size in bytes
                  else
                    Field.Size := (DataSize + 1) * SizeOf(WideChar) + SizeOf(Short);//size in bytes + space for Len storage
                end;
              end
              else begin
                Field.DataType := dtWideMemo; //dtMemo;
                Field.SubDataType := dtWideString;
                Field.Size := SizeOf(IntPtr);
              end;
            end
            else begin
              if (DataSize <= 255) or FLongStrings
               {$IFDEF SHORTSTRINGFIELD} and (DataSize <= 8192) {$ENDIF} then begin  //Delphi 5 limitation
                if (DataSize >= FlatBufferLimit) and not FFlatBuffers then begin
                  Field.DataType := dtExtString;
                  Field.Size := SizeOf(IntPtr);
                end
                else begin
                  Field.DataType := dtString;
                  if DataType = SQL_TEXT then
                    Field.Size := DataSize + 1
                  else
                    Field.Size := DataSize + 1 + SizeOf(Short);
                end;
              end
              else begin
                Field.DataType := dtMemo;
                Field.SubDataType := dtString;
                Field.Size := SizeOf(IntPtr);
              end;
            end;
          Field.Length := DataSize; // is used to differ from Memo
        end;
      end;
      SQL_FLOAT, SQL_DOUBLE: begin
        if FFieldsAsString then begin
          Field.SubDataType := dtFloat;
          Field.Scale := 0;
          Field.Length := FloatPrecision + 1; //+ Sign ;
          Field.Size := Field.Length + 1;
          if FConnection.FUseUnicode then begin
            Field.DataType := dtWideString;
            SQLVAR.sqllen := (Field.Size - 1) * 3;
            Field.Size := Field.Size * 2;
          end
          else begin
            SQLVAR.sqllen := Field.Size - 1;
            Field.DataType := dtString;
          end;
          SQLVAR.sqltype := SQL_TEXT or 1; //SetUp SQLVAR sqltype to made server convert numbers to strings
        end
        else begin
          Field.DataType := dtFloat;
          Field.Size := SizeOf(Double);
        end;
      end;
      SQL_SHORT, SQL_LONG, SQL_INT64: begin
        case DataType of
          SQL_SHORT: begin
            if FConnection.FSimpleNumericMap then
              Precision := 4
            else
              Precision := IntegerPrecision div 2;
            Field.DataType := dtSmallint;
            Field.Size := SizeOf(Smallint);
          end;
          SQL_LONG: begin
            if FConnection.FSimpleNumericMap then
              Precision := 9
            else
              Precision := IntegerPrecision;
            Field.DataType := dtInteger;
            Field.Size := SizeOf(integer);
          end;
          else begin
            Field.DataType := dtInt64;
            Field.Size := SizeOf(Int64);
            if FConnection.FSimpleNumericMap then
              Precision := 18
            else
              Precision := IntegerPrecision * 2;
          end;
        end;
        Scale := SQLVAR.sqlscale;
      {$IFDEF LITE}
        if Precision < 18 then   // In DbxIda we can not return ftfmtbcd field type.
          Precision := 15;       // Using Precision parameter we make dbExpress represent ftbcd as ftfmtbcd
      {$ENDIF}
        Field.Length := Precision;
      {$IFDEF LITE}
        Field.SQLSubType := SQLVAR.sqlsubtype;
      {$ENDIF}  
        if FFieldsAsString then begin
          Field.SubDataType := Field.DataType;
          Field.Scale := Abs(Scale);
          Field.Length := Field.Length + 1; //+ Sign 
          Field.Size := Field.Length + 1;
          if FConnection.FUseUnicode then begin
            Field.DataType := dtWideString;
            SQLVAR.sqllen := (Field.Size - 1) * 3;
            Field.Size := Field.Size * 2;
          end
          else begin
            SQLVAR.sqllen := Field.Size - 1;
            Field.DataType := dtString;
          end;
          SQLVAR.sqltype := SQL_TEXT or 1; //SetUp SQLVAR sqltype to made server convert numbers to strings
        end
        else begin
          Field.Scale := ABS(Scale);
        {$IFNDEF LITE}
          if (Scale <> 0) or (FConnection.FSimpleNumericMap and (SQLVAR.sqlsubtype <> 0)) then
            if (Scale >= (-4)) and (FConnection.EnableBCD or FCommand.EnableBCD)
              and (not FConnection.FSimpleNumericMap or (Precision <= MaxBcdPrecision - MaxBCDScale))
            then begin
              Field.DataType := dtBCD;
              Field.Size := SizeOf(Currency);
              SQLVAR.sqltype := SQL_INT64 or 1; //SetUp SQLVAR sqltype to made server convert numbers to Currency
              SQLVAR.sqlscale := -4;
              SQLVAR.sqllen := Field.Size;
            end
            else
        {$ENDIF}
          {$IFDEF VER6P}
          {$IFNDEF FPC}
            if not (FConnection.FSQLDialect = 1) and
            {$IFNDEF LITE}
              FConnection.EnableFMTBCD or FCommand.EnableFMTBCD
            {$ELSE}
              ((not FConnection.FOptimizedNumerics and ((SQLVAR.sqlsubtype <> 0) or (Field.DataType = dtInt64) and (not FConnection.FEnableLargeint or (Scale <> 0))))
              or (FConnection.FOptimizedNumerics and (not FConnection.FEnableLargeint or (Scale <> 0)) and FConnection.FEnableFMTBCD and (Field.DataType = dtInt64)))
            {$ENDIF}
            then begin
              Field.DataType := dtFMTBCD;
              Field.Size := SizeOfTBcd;
            end
            else
          {$ENDIF}
          {$ENDIF}
          {$IFDEF LITE}
            if (Scale <> 0) or (Field.DataType = dtInt64) and not FConnection.FEnableLargeint then
          {$ENDIF}
            begin
              Field.DataType := dtFloat;
              Field.Size := SizeOf(Double);
              SQLVAR.sqltype := SQL_DOUBLE or 1; //SetUp SQLVAR sqltype to made server convert numbers to Float
              SQLVAR.sqlscale := 0;
              SQLVAR.sqllen := Field.Size;
            end;
        end;
      end;
      SQL_BOOLEAN: begin
        if FFieldsAsString then begin
          Field.SubDataType := dtBoolean;
          Field.Scale := 0;
          Field.Length := 2;
          Field.Size := 2;          //String boolean representation is '0' or '1'
          if FConnection.FUseUnicode then begin
            Field.DataType := dtWideString;
            SQLVAR.sqllen := (Field.Size - 1) * 3;
            Field.Size := Field.Size * 2;
          end
          else begin
            SQLVAR.sqllen := Field.Size - 1;
            Field.DataType := dtString;
          end;
          SQLVAR.sqltype := SQL_TEXT  or 1; //SetUp SQLVAR sqltype to made server convert booleans to strings
        end
        else begin
          if GDS.Version < 7 then begin
            SQLVAR.sqltype := SQL_SHORT or 1;
            SQLVAR.sqllen := SizeOf(Smallint);
          end;
          Field.DataType := dtBoolean;
          Field.Size := SizeOf(WordBool);
        end;
      end;
      SQL_TYPE_TIME: begin
        if FFieldsAsString then begin
          Field.SubDataType := dtTime;
          Field.Scale := 0;
          Field.Length := 13 + 1;
          Field.Size := 13 + 1; //Time format HH:MM:SS.0000
          if FConnection.FUseUnicode then begin
            Field.DataType := dtWideString;
            SQLVAR.sqllen := (Field.Size - 1) * 3;
            Field.Size := Field.Size * 2;
          end
          else begin
            SQLVAR.sqllen := Field.Size - 1;
            Field.DataType := dtString;
          end;
          SQLVAR.sqltype := SQL_TEXT  or 1; //SetUp SQLVAR sqltype to made server convert time to strings
        end
        else begin
          Field.DataType := dtTime;
          Field.Size := SizeOf(TDateTime);
        end;
      end;
      SQL_TYPE_DATE: begin
        if FFieldsAsString then begin
          Field.SubDataType := dtDate;
          Field.Scale := 0;
          Field.Length := 11 + 1;
          Field.Size := 11 + 1; //Time format YYYY-MM-DD (SQL Dial 3) or YYYY-MMM-DD (SQL Dial 1)
          if FConnection.FUseUnicode then begin
            Field.DataType := dtWideString;
            SQLVAR.sqllen := (Field.Size - 1) * 3;
            Field.Size := Field.Size * 2;
          end
          else begin
            SQLVAR.sqllen := Field.Size - 1;
            Field.DataType := dtString;
          end;
          SQLVAR.sqltype := SQL_TEXT  or 1; //SetUp SQLVAR sqltype to made server convert date to strings
        end
        else begin
          Field.DataType := dtDate;
          Field.Size := SizeOf(TDateTime);
        end;
      end;
      SQL_TIMESTAMP: begin
        if FFieldsAsString then begin
          Field.SubDataType := dtDateTime;
          Field.Scale := 0;
          Field.Length := 0;
          Field.Size := 25 + 1; //Time format YYYY-MM-DD HH:MM:SS.0000 (SQL Dial 3)
                            //or YYYY-MMM-DD HH:MM:SS.0000 (SQL Dial 1)
          if FConnection.FUseUnicode then begin
            Field.DataType := dtWideString;
            SQLVAR.sqllen := (Field.Size - 1) * 3;
            Field.Size := Field.Size * 2;
          end
          else begin
            SQLVAR.sqllen := Field.Size - 1;
            Field.DataType := dtString;
          end;
          SQLVAR.sqltype := SQL_TEXT  or 1; //SetUp SQLVAR sqltype to made server convert datetime to strings
        end
        else begin
          Field.DataType := dtDateTime;
          Field.Size := SizeOf(TDateTime);
        end;
      end;
      SQL_QUAD, SQL_BLOB: begin
        Field.Size := SizeOf(IntPtr);
        Field.SQLSubType := SQLVAR.sqlsubtype;
        if FConnection.FUseUnicode and (FldTableName <> '') then  //We need Blob CharsetID to perform right UTF conversion
          GetBlobCharset(Field, FldTableName);

        if FConnection.FEnableMemos and (Field.SQLSubType = isc_blob_text) then begin
          if FConnection.FUseUnicode and
            ((Field.CharsetId = CH_UNICODE_FSS) or (Field.CharsetId = CH_UTF8)
            or (Field.CharsetId = CH_NONE))
          then
            Field.DataType := dtWideMemo
          else
            Field.DataType := dtMemo;
        end
        else
          Field.DataType := dtBlob;
      end;
      SQL_ARRAY: begin
        Field.DataType := dtArray;
        Field.Size := SizeOf(IntPtr);
        Field.SQLSubType := SQLVAR.sqlsubtype;
        ArrType := TIBCArrayType.Create(FConnection,
          FCommand.FTransaction, FldTableName, Field.ActualName);
        try
          Field.ObjectType := ArrType;
        finally
          ArrType.Free;
        end;
      end
      else
        RaiseError(SDataTypeNotSupported);
    end;
    Field.Fixed := SQLVAR.sqltype = SQL_TEXT;
    Field.Required := not IsNullable and (FldTableName <> '') and (Field.SubDataType <> dtDbKey); // DB_KEY is readonly field
    FHasFlatUnicodeFields := FHasFlatUnicodeFields or (Field.DataType = dtWideString);
  end;

  procedure FillFieldsDomainNames(OnlyForBooleanFields: boolean = False);
  var
    i: integer;
    TableNames: _TStringList;
    Filter, TableName, RelName, FieldName, Domain: _string;
    RecordSet: TGDSRecordSet;
    RecBuf: IntPtr;
    Val: variant;
  begin
    TableNames := _TStringList.Create;
    try
      TableNames.Sorted := True;
      TableNames.Duplicates := dupIgnore;
      for i := 0 to Fields.Count - 1 do begin
        if (Fields[i].ParentField = nil) and (TIBCFieldDesc(Fields[i]).TableInfo <> nil) then
          if not OnlyForBooleanFields or (Fields[i].DataType in BooleanIntTypes) then begin
            TableName := TIBCFieldDesc(Fields[i]).TableInfo.TableName;
            if TableName <> '' then
              TableNames.Add(TableName);
          end;
      end;
      if TableNames.Count = 0 then
        exit;

      Filter := '';
      for i := 0 to TableNames.Count - 1 do begin
        if Filter <> '' then
          Filter := Filter + ', ';
        Filter := Filter + _QuotedStr(TableNames[i], '''');
      end;
    finally
      TableNames.Free;
    end;

    RecordSet := TGDSRecordSet.Create;
    try
      RecordSet.SetConnection(FConnection);
      RecordSet.SetTransaction(FCommand.FTransaction);
      RecordSet.SetSQL(_Format(
        'SELECT RDB$RELATION_NAME, RDB$FIELD_NAME, RDB$FIELD_SOURCE ' +
        'FROM RDB$RELATION_FIELDS ' +
        'WHERE RDB$RELATION_NAME IN (%s)', [Filter]));
      RecordSet.SetProp(prFlatBuffers, False);
      RecordSet.SetProp(prLongStrings, True);
      RecordSet.TrimFixedChar := True;
      RecordSet.Open;
      RecordSet.FetchAll;
      RecordSet.SetToBegin;
      if RecordSet.RecordCount > 0 then begin
        RecordSet.AllocRecBuf(RecBuf);
        try
          RecordSet.GetNextRecord(RecBuf);
          while not RecordSet.Eof do begin
            RecordSet.GetFieldAsVariant(1, RecBuf, Val);
            RelName := _VarToStr(Val);
            RecordSet.GetFieldAsVariant(2, RecBuf, Val);
            FieldName := _VarToStr(Val);
            RecordSet.GetFieldAsVariant(3, RecBuf, Val);
            Domain := _VarToStr(Val);
            for i := 0 to Fields.Count - 1 do begin
              if (Fields[i].ParentField = nil) and (TIBCFieldDesc(Fields[i]).TableInfo <> nil) then
                if not OnlyForBooleanFields or (Fields[i].DataType in BooleanIntTypes) then begin
                  TableName := TIBCFieldDesc(Fields[i]).TableInfo.TableName;
                  if (Fields[i].ActualName = FieldName) and (TableName = RelName) then
                    TIBCFieldDesc(Fields[i]).DomainName := Domain;
                end;
            end;
            RecordSet.GetNextRecord(RecBuf);
          end;
        finally
          Marshal.FreeHGlobal(RecBuf);
        end;
      end;
    finally
      RecordSet.Free;
    end;
  end;

  procedure DetectBooleanIntFields;
  var
    Domain: _string;
    i: integer;
  begin
    if not FSetDomainNames then
      FillFieldsDomainNames(True);

    for i := 0 to Fields.Count - 1 do
      if Fields[i].DataType in BooleanIntTypes then begin
        Domain := _UpperCase(TIBCFieldDesc(Fields[i]).DomainName);
        if Pos('BOOLEAN', Domain) > 0 then begin
          Fields[i].SubDataType := Fields[i].DataType;
          Fields[i].DataType := dtBoolean;
        end;
      end;
  end;

var
  i: integer;
  FieldsCount: integer;
  Field: TIBCFieldDesc;
  OldCursorState: TCursorState;
begin
  inherited;

  OldCursorState := FCommand.GetCursorState;
  try
    if FCommand.GetCursorState = csInactive then
      FCommand.Prepare;
    FCommand.CheckActive;

    if (FFieldXSQLDA = nil) or
      ((GDS.Version < 7) and (FFieldXSQLDA.FXSQLVARType = vtGDS7)) or
      ((GDS.Version >= 7) and (FFieldXSQLDA.FXSQLVARType = vtGDS)) then begin
      FFieldXSQLDA.Free;
      if GDS.Version >= 7 then
        FFieldXSQLDA := TSQLDA.Create(FConnection, vtGDS7)
      else
        FFieldXSQLDA := TSQLDA.Create(FConnection, vtGDS);
      FFieldXSQLDA.AllocSQLDA(0);
    end;

    //Extract felds count and describe them
    GDS.Busy;
    try
      Check(GDS.isc_dsql_describe(FCommand.FStatusVector, FCommand.FStmtHandle, FConnection.FSQLDialect, FFieldXSQLDA.FXSQLDA));
      FieldsCount := FFieldXSQLDA.GetSqld;
      if FieldsCount > FFieldXSQLDA.GetSqln then begin
        FFieldXSQLDA.AllocSQLDA(FieldsCount);
        Check(GDS.isc_dsql_describe(FCommand.FStatusVector, FCommand.FStmtHandle, FConnection.FSQLDialect, FFieldXSQLDA.FXSQLDA));
      end;
    finally
      GDS.Release;
    end;

    TablesInfo.Clear;
    TablesInfo.CaseSensitive := True;

    FHasFlatUnicodeFields := False;
    i := 0;
    while i < FieldsCount do begin
      Field := TIBCFieldDesc(GetFieldDescType.Create);
      DescribeDefineFieldDesc(Field, FFieldXSQLDA.Vars[i], i + 1);
      Field.FieldNo := FFields.Count + 1;
      FFields.Add(Field);

      if (Field.DataType = dtObject) or ((Field.DataType = dtArray) and FComplexArrayFields) then
        InitObjectFields(Field.ObjectType, Field);

      Inc(i);
    end;

    FillTablesAliases;

    if FSetDomainNames then
      FillFieldsDomainNames;

    if FBooleanDomainFields then
      DetectBooleanIntFields;
  finally
    if OldCursorState = csInactive then
      FCommand.Finish
    else
      FCommand.SetCursorState(OldCursorState);
  end;
end;

procedure TGDSRecordSet.InitFields;
var
  i: integer;
  FieldDesc: TFieldDesc;
begin
  inherited;

  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if ((FieldDesc.DataType = dtString) or (FieldDesc.DataType = dtWideString)) and
       (FieldDesc.FieldDescKind = fdkData) and not FieldDesc.Fixed then begin
      FieldDesc.Offset := FieldDesc.Offset + SizeOf(Short); //To omit VARCHAR Length
      if FieldDesc.Size >= SizeOf(Short) then
        FieldDesc.Size := FieldDesc.Size - SizeOf(Short);
    end;
    if (FieldDesc.DataType = dtBoolean) and (FieldDesc.SubDataType in BooleanIntTypes) then
      FieldDesc.Size := 2;
  end;
end;

function TGDSRecordSet.GetNull(FieldNo: word; RecBuf: IntPtr): boolean;
var
  Field: TFieldDesc;
  DBObject: IntPtr; 
begin
  Field := Fields[FieldNo - 1];
  if not Field.HasParent then
    if Field.DataType = dtArray then begin
      DBObject := Marshal.ReadIntPtr(RecBuf, Field.Offset);
      if DBObject <> nil then
        Result := TCustomIBCArray(GetGCHandleTarget(DBObject)).IsNull
      else
        Result := True;
    end
    else begin
      Result := Marshal.ReadInt16(RecBuf, DataSize + (FieldNo - 1) * SizeOf(Short)) = -1
    end
  else
    Result := GetChildFieldIsNull(Field, RecBuf);
  if Result then
    Result := GetNullByBlob(FieldNo, RecBuf);
end;

procedure TGDSRecordSet.SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean);
var
  Field: TFieldDesc;
  Ind: Short;
  Blob: TBlob;
begin
  Field := Fields[FieldNo - 1];
  if not Field.HasParent then begin
    if Value then
      Ind := -1
    else
      Ind := 0;
    Marshal.WriteInt16(RecBuf, DataSize + (FieldNo - 1) * SizeOf(Short), Ind);

    if Value and IsBlobFieldType(Field.DataType) then begin // clear Blob value
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(PtrOffset(RecBuf, Field.Offset))));
      if Blob <> nil then
        Blob.Clear;
    end;
  end
  else
    PutChildField(Field, RecBuf, nil);
end;

{ Records }
procedure TGDSRecordSet.CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean);
begin
  if not HasComplexFields then
    Exit;

  inherited;
end;

procedure TGDSRecordSet.CreateComplexField(RecBuf: IntPtr; FieldIndex: integer; WithBlob: boolean);
var
  Ptr: IntPtr;
  Blob: TBlob;
  Lob: TIBCBlob;
  IBCArray: TCustomIBCArray;
  FieldDesc: TFieldDesc;
begin
  FieldDesc := Fields[FieldIndex];
  if not FieldDesc.HasParent and (FieldDesc.FieldDescKind <> fdkCalculated) and
    (not IsBlobFieldType(FieldDesc.DataType) or WithBlob) then begin
    Ptr := PtrOffset(RecBuf, FieldDesc.Offset);
    case FieldDesc.DataType of
      dtBlob, dtMemo, dtWideMemo: begin
         //This is VarChar or Char field mapped on Memo
        if ((FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtWideString)) then begin
          if WithBlob then begin
            inherited;
            Blob := TBlob(GetGCHandleTarget( Marshal.ReadIntPtr(PtrOffset(RecBuf, FieldDesc.Offset))));
            Blob.IsUnicode := UseUnicode and (TIBCFieldDesc(FieldDesc).CharsetID <> CH_OCTETS); //None and OCTETS are not converted to UTF8
          end;
        end
        else begin
          if (FConnection <> nil) and (FConnection.GetConnected) then
            Lob := TIBCBlob.Create(FConnection, FCommand.FTransaction) //We set Update Transaction on WriteParams/BindParam
          else
            Lob := TIBCBlob.Create(TGDSConnection(nil), TGDSTransaction(nil)); //Set DB and Transaction Handles on BindParams
          Lob.CharsetId := TIBCFieldDesc(FieldDesc).CharsetID;
          if FieldDesc.DataType = dtWideMemo then
            Lob.IsUnicode := True;
          Lob.Cached := FCommand.FCacheBlobs;
          Lob.Streamed := FCommand.FStreamedBlobs;
          Lob.FSubType := TIBCFieldDesc(FieldDesc).SQLSubType;
          Marshal.WriteIntPtr(Ptr, Lob.GCHandle);
        end;
      end;
      dtArray: begin
        if (FConnection <> nil) and (FConnection.GetConnected) then
          IBCArray := TCustomIBCArray.Create(FConnection, FCommand.FTransaction,
            GenerateTableName(FieldDesc), FieldDesc.ActualName)
        else
          IBCArray := TCustomIBCArray.Create(TGDSConnection(nil), TGDSTransaction(nil),
            GenerateTableName(FieldDesc), FieldDesc.ActualName);
        IBCArray.Cached := FCommand.FCacheArrays;
        IBCArray.ArrayType := TIBCArrayType(FieldDesc.ObjectType);
        Marshal.WriteIntPtr(Ptr, IBCArray.GCHandle);
      end
      else
        inherited;
    end;
  end;
end;

procedure TGDSRecordSet.FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  SharedObject: TSharedObject;
  FieldDesc: TFieldDesc;
begin
  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if not FieldDesc.HasParent and (FieldDesc.FieldDescKind <> fdkCalculated) and
      (not(IsBlobFieldType(FieldDesc.DataType)) or WithBlob)
    then
      case FieldDesc.DataType of
        dtArray: begin
          SharedObject := TSharedObject(GetGCHandleTarget( Marshal.ReadIntPtr(PtrOffset(RecBuf, FieldDesc.Offset))));
          SharedObject.Free;
        end;
      end;
  end;
  inherited;
end;

procedure TGDSRecordSet.CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean);
var
  i: integer;
  SrcPtr, DestPtr: IntPtr;
  IBCArraySrc,IBCArrayDest: TCustomIBCArray;
  FieldDesc: TFieldDesc;
begin
  inherited;

  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if not FieldDesc.HasParent and (FieldDesc.FieldDescKind <> fdkCalculated) then begin
      case FieldDesc.DataType of
        dtArray: begin
          SrcPtr := Marshal.ReadIntPtr(PtrOffset(Source, FieldDesc.Offset));
          DestPtr := Marshal.ReadIntPtr(PtrOffset(Dest, FieldDesc.Offset));
          IBCArraySrc := TCustomIBCArray(GetGCHandleTarget(SrcPtr));
          IBCArrayDest := TCustomIBCArray(GetGCHandleTarget(DestPtr));
          IBCArrayDest.Assign(IBCArraySrc);
        end;
      end;
    end;
  end;
end;

function TGDSRecordSet.CompareFields(RecBuf1: IntPtr; RecBuf2: IntPtr; SortColumn: TSortColumn): integer;
var
  IsBlank1, IsBlank2: boolean;
begin
  if not (IsFBConnection and (MajorServerVersion >= 2)) then begin
    IsBlank1 := GetNull(SortColumn.FieldDesc.FieldNo, RecBuf1);
    IsBlank2 := GetNull(SortColumn.FieldDesc.FieldNo, RecBuf2);
    if IsBlank1 and not IsBlank2 then
      if SortColumn.DescendingOrder then
        Result := -1  //InterBase specific NULL order
      else
        Result := 1
    else
    if not IsBlank1 and IsBlank2 then
      if SortColumn.DescendingOrder then
        Result := 1
      else
        Result := -1 //InterBase specific NULL order
    else
    if IsBlank1 and IsBlank2 then
      Result := 0
    else
      Result := inherited CompareFields(RecBuf1, RecBuf2, SortColumn);
  end
  else
    Result := inherited CompareFields(RecBuf1, RecBuf2, SortColumn);
end;

function TGDSRecordSet.GetArrayFieldName(ObjectType: TObjectType; ItemIndex: integer): _string;
begin
  Result :=  '[' + IntToStr(TIBCArrayType(ObjectType).LowBound + ItemIndex) + ']';
end;

function TGDSRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TIBCFieldDesc;
end;

{ Fetch }

class function TGDSRecordSet.IsComplexFieldType(DataType: word): boolean;
begin
  Result := DataType = dtArray;
  if not Result then
    Result := inherited IsComplexFieldType(DataType);
end;

function TGDSRecordSet.IsFetchBlockField(DataType: word): boolean;
begin
  Result := IsComplexFieldType(DataType) or (DataType = dtWideString);
end;

procedure TGDSRecordSet.AllocFetchBlock;
var
  i: integer;
  FieldDesc: TFieldDesc;
begin
  if FFetchBlock <> nil then
    FreeFetchBlock;
  FFetchBlockItemSize := 0;
  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if (FieldDesc.FieldDescKind = fdkData) and
      not FieldDesc.HasParent and IsFetchBlockField(FieldDesc.DataType)
    then
      Inc(FFetchBlockItemSize, GetComplexFldOffset(FieldDesc));
  end;
  if FFetchBlockItemSize > 0 then
    FFetchBlock := Marshal.AllocHGlobal(FFetchBlockItemSize * FFetchRows);
end;

procedure TGDSRecordSet.FreeFetchBlock;
begin
  if FFetchBlock <> nil then
    Marshal.FreeHGlobal(FFetchBlock);
  FFetchBlock := nil;
  FFetchBlockItemSize := 0;
end;

function TGDSRecordSet.GetComplexFldOffset(FieldDesc: TFieldDesc): integer;
begin
  Result := SizeOf(IntPtr);
  if (FieldDesc.DataType = dtExtString) or (FieldDesc.DataType = dtExtVarBytes) or
    (FieldDesc.DataType = dtMemo) and (FieldDesc.SubDataType = dtString) or
    (FieldDesc.DataType = dtExtWideString) or (FieldDesc.DataType = dtWideString) or
    (FieldDesc.DataType = dtWideMemo) and (FieldDesc.SubDataType = dtWideString)
  then begin
    if (FieldDesc.DataType = dtExtString) or (FieldDesc.DataType = dtExtVarBytes) or
      (FieldDesc.DataType = dtMemo)
    then
      if (CharLength > 1) and (FieldDesc.DataType <> dtExtVarBytes) then
        Result := FieldDesc.Length * CharLength + 1 //+ 1 for terminator
      else
        Result := FieldDesc.Length + 1 //+ 1 for terminator
    else
      if (FieldDesc.DataType = dtExtWideString) or (FieldDesc.DataType = dtWideString) or
        (FieldDesc.DataType = dtWideMemo)
      then
        Result := FieldDesc.Length * FConnection.GetCharLength(TIBCFieldDesc(FieldDesc).CharsetID) + 1;
    if not FieldDesc.Fixed then
      Inc(Result, SizeOf(Short)); // SizeOf(Short) - string length storage
  end
  else
  case FieldDesc.DataType of
    dtBlob, dtMemo, dtWideMemo, dtArray:
      Result := SizeOf(TISC_QUAD);
  end;
end;

function TGDSRecordSet.InternalFetch: boolean;
var
  i,j: integer;
  pRec, pInd: IntPtr;
  NeedFetch, Fetched: word;
  Res: ISC_STATUS;
  Block: PBlockHeader;
  TempBlock: PBlockHeader;
  Item: PItemHeader;
  ItemSize: integer;
  NewBlock: boolean;
  BufferPtr: IntPtr;
  TextLen: word;
  ComplexFieldOffset: integer;
  ActualFieldCount: integer;
  ObjPtr: IntPtr;
  IndPtr: IntPtr;
  OldFirstItem: PItemHeader;
  OldLastItem: PItemHeader;
  OldFetchStart: integer;
  OldFetchEnd: integer;
  FieldDesc: TFieldDesc;

  procedure InitBlock;
  var
    i,j: integer;
    Ptr: IntPtr;
    FieldPtr: IntPtr;
    FieldDesc: TFieldDesc;
  begin
  // Create complex filds
    ComplexFieldOffset := 0;
    if not HasComplexFields then
      Exit;
    for i := 0 to FFetchRows - 1 do begin
      Ptr := PtrOffset(Block, SizeOf(TBlockHeader) + i * ItemSize + SizeOf(TItemHeader));
      CreateComplexFields(Ptr, True);
      for j := 0 to FieldCount - 1 do begin
        FieldDesc := Fields[j];
        if not FieldDesc.HasParent and IsFetchBlockField(FieldDesc.DataType) and
          (FieldDesc.FieldDescKind = fdkData)
        then begin
          FieldPtr := PtrOffset(FFetchBlock, ComplexFieldOffset);
          if (FieldDesc.DataType = dtMemo) and (FieldDesc.SubDataType = dtString) then // String as memo
            Marshal.WriteByte(FieldPtr, 0)
          else
          if (FieldDesc.DataType = dtWideMemo) and (FieldDesc.SubDataType = dtWideString) then
            Marshal.WriteInt16(FieldPtr, 0)
          else
          case FieldDesc.DataType of
            dtBlob, dtMemo, dtWideMemo, dtArray:
              Marshal.WriteInt64(FieldPtr, 0);
            dtWideString:
              Marshal.WriteInt32(FieldPtr, 0);
            dtExtString:
              Marshal.WriteIntPtr(FieldPtr, nil);
            dtExtVarBytes:
              Marshal.WriteIntPtr(FieldPtr, nil);
            dtExtWideString:
              Marshal.WriteIntPtr(FieldPtr, nil);
          end;
          Inc(ComplexFieldOffset, GetComplexFldOffset(FieldDesc));
        end;
      end;
    end;
  end;

  procedure PrepareBlock(Fetched: integer);
  var
    i, j: integer;
    Ptr: IntPtr;
    ObjPtr: IntPtr;
    Piece: PPieceHeader;
    Len: integer;
    Source: IntPtr;
    Dest: IntPtr;
    FieldDesc: TFieldDesc;
  {$IFDEF VER6P}
    BcdValue: TBCD;
    bcdscale: integer;
  {$IFDEF CLR}
    Temp: TBytes;
  {$ENDIF}
  {$ENDIF}
    HeapBuf: IntPtr;
    wsLen: integer;
    wsBuff: IntPtr;

    procedure TrimBuffer(Buf: IntPtr; var BufLen: integer);
    var
      p: IntPtr;
    begin
      p := PtrOffset(Buf, BufLen - 1);
      while (BufLen > 0) and (Marshal.ReadByte(p) = Byte(' ')) do begin
        Dec(BufLen);
        p := PtrOffset(p, -1);
      end;
    end;

    function IsProcessFieldType(DataType: word): boolean;
    begin
      case DataType of
        dtDate, dtTime, dtDateTime {$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD {$ENDIF}{$ENDIF}:
          Result := True;
        else
          Result := IsFetchBlockField(DataType);
      end;
    end;

  begin
    ComplexFieldOffset := 0;
    wsBuff := nil;
    wsLen := 0;
    try
      for i := 0 to Fetched - 1 do begin
        Ptr := PtrOffset(Block, SizeOf(TBlockHeader) + i * ItemSize + SizeOf(TItemHeader));
        for j := 0 to FieldCount - 1 do begin
          FieldDesc := Fields[j];
          if not FieldDesc.HasParent and IsProcessFieldType(FieldDesc.DataType) and (FieldDesc.FieldDescKind = fdkData) then begin
            ObjPtr := Marshal.ReadIntPtr(PtrOffset(Ptr, FieldDesc.Offset));
            Source := PtrOffset(FFetchBlock, ComplexFieldOffset);
            if (FieldDesc.DataType = dtMemo) and (FieldDesc.SubDataType = dtString) or // String as memo
              (FieldDesc.DataType = dtWideMemo) and (FieldDesc.SubDataType = dtWideString)
            then begin
              if FieldDesc.SubDataType = dtString then begin
                if not FieldDesc.Fixed then
                  Source := PtrOffset(Source, SizeOf(Short));
                Len := StrLen(PAChar(Source));
                if FieldDesc.Fixed and TrimFixedChar then begin
                  Dest := PtrOffset(Source, Len - 1);
                  while (Marshal.ReadByte(Dest) = Byte(' ')) and (Len > 0) do begin
                    Dest := PtrOffset(Dest, -1 {sizeof(AsciiChar)});
                    Dec(Len);
                  end;
                end;
                if Len > 0 then begin
                  TBlob(GetGCHandleTarget(ObjPtr)).AllocPiece(Piece, Len); // for term
                  TBlob(GetGCHandleTarget(ObjPtr)).AppendPiece(Piece);
                  CopyBuffer(Source, PtrOffset(Piece, SizeOf(TPieceHeader)), Len);
                  Piece.Used := Len;
                end;
              end
              else begin
                if FieldDesc.Fixed then begin
                  Len := FieldDesc.Length * FConnection.GetCharLength(TIBCFieldDesc(FieldDesc).CharsetID);
                end
                else begin
                  Len := Marshal.ReadInt16(Source);
                  Source := PtrOffset(Source, 2);
                end;

                if TrimFixedChar and FieldDesc.Fixed then
                  TrimBuffer(Source, Len);

                if Len > 0 then begin
                  TBlob(GetGCHandleTarget(ObjPtr)).AllocPiece(Piece, FieldDesc.Length * SizeOf(WideChar));
                  TBlob(GetGCHandleTarget(ObjPtr)).AppendPiece(Piece);
                  Dest := PtrOffset(Piece, SizeOf(TPieceHeader));

                  Len := Utf8ToWs(Source, Len, Dest, FieldDesc.Length * SizeOf(WideChar), False);

                  Piece.Used := Len;
                  TBlob(GetGCHandleTarget(ObjPtr)).Compress;
                end;
              end;
            end
            else
            case FieldDesc.DataType of
              dtExtString:
                if FieldDesc.Fixed then
                  Marshal.WriteIntPtr(PtrOffset(Ptr, FieldDesc.Offset),
                    StringHeap.AllocStr(Source, TrimFixedChar))
                else
                  Marshal.WriteIntPtr(PtrOffset(Ptr, FieldDesc.Offset), //Remove len fron VARCHAR
                    StringHeap.AllocStr(PtrOffset(Source, SizeOf(Short)), False));
              dtExtWideString: begin
                if FieldDesc.Fixed then begin
                  Len := FieldDesc.Length * FConnection.GetCharLength(TIBCFieldDesc(FieldDesc).CharsetID);
                  //if ((Copy(FieldDesc.ActualName, 1, 4) <> 'RDB$') or (FConnection.FDBCharsetId = CH_UTF8)) then
                  //  Len := Len * FConnection.GetCharLength(TIBCFieldDesc(FieldDesc).CharsetID);
                end
                else begin
                  Len := Marshal.ReadInt16(Source);
                  Source := PtrOffset(Source, 2);
                end;

                if TrimFixedChar and FieldDesc.Fixed then
                  TrimBuffer(Source, Len);

                if wsLen < Len * SizeOf(WideChar) then begin
                  wsLen := Len * SizeOf(WideChar);
                  if wsBuff <> nil then
                    Marshal.FreeHGlobal(wsBuff);
                  wsBuff := Marshal.AllocHGlobal(wsLen);
                end;

                if Len > 0 then
                  Len := Utf8ToWs(Source, Len, wsBuff, wsLen, False);

                if FieldDesc.Fixed then
                  Len := Min(Len, FieldDesc.Length * SizeOf(WideChar));
                  
                HeapBuf := StringHeap.NewBuf(Len + 2);
                CopyBuffer(wsBuff, HeapBuf, Len);
                Marshal.WriteInt16(HeapBuf, Len, 0);

                Marshal.WriteIntPtr(PtrOffset(Ptr, FieldDesc.Offset), HeapBuf);
              end;
              dtWideString: begin
                if FieldDesc.Fixed then begin
                  Len := FieldDesc.Length * FConnection.GetCharLength(TIBCFieldDesc(FieldDesc).CharsetID);
                end
                else begin
                  Len := Marshal.ReadInt16(Source);
                  Source := PtrOffset(Source, 2);
                end;

                if TrimFixedChar and FieldDesc.Fixed then
                  TrimBuffer(Source, Len);

                Dest := PtrOffset(Ptr, FieldDesc.Offset);
                Len := Utf8ToWs(Source, Len, Dest, FieldDesc.Size, True);
              end;
              dtExtVarBytes: begin
                Dest := StringHeap.NewBuf(Marshal.ReadInt16(Source) + SizeOf(Word));
                Marshal.WriteIntPtr(PtrOffset(Ptr, FieldDesc.Offset), Dest);
                CopyBuffer(Source, Dest, Marshal.ReadInt16(Source) + SizeOf(Word));
              end;
              dtBlob, dtMemo, dtWideMemo: begin
                TIBCBlob(GetGCHandleTarget(ObjPtr)).ID := Marshal.ReadInt64(Source);
              end;
              dtArray:
                TCustomIBCArray(GetGCHandleTarget(ObjPtr)).ArrayID := Marshal.ReadInt64(Source);
              dtDate, dtTime, dtDateTime: begin
                Dest := PtrOffset(Ptr, FieldDesc.Offset);
                case FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo - 1].sqltype of  //We need to know exact SQLTYPE to use right conversion
                  SQL_TYPE_TIME:
                    SQLTimeToDateTime(Dest, Dest);
                  SQL_TYPE_DATE:
                    SQLDateToDateTime(Dest, Dest);
                  SQL_TIMESTAMP:
                    SQLTimeStampToDateTime(Dest, Dest);
                end;
                continue;
              end;
             {$IFDEF VER6P}
             {$IFNDEF FPC}
              dtFMTBCD: begin
                Dest := PtrOffset(Ptr, FieldDesc.Offset);
                bcdscale := (-1 * FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo - 1].GetSqlScale);
                case FFieldXSQLDA.FXSQLVARs[FieldDesc.ActualFieldNo - 1].sqltype of
                  SQL_SHORT:
                    BcdValue := DBDecimalToBcd(Marshal.ReadInt16(Dest), bcdscale);
                  SQL_LONG:
                    BcdValue := DBDecimalToBcd(Marshal.ReadInt32(Dest), bcdscale);
                  else
                    BcdValue := DBDecimalToBcd(Marshal.ReadInt64(Dest), bcdscale);
                end;
              {$IFDEF CLR}
                Temp := TBcd.ToBytes(BcdValue);
                Marshal.Copy(Temp, 0, Dest, SizeOfTBcd);
              {$ELSE}
                PBcd(Dest)^ := BcdValue;
              {$ENDIF}
                continue;
              end;
             {$ENDIF}
             {$ENDIF}
            end;
            Inc(ComplexFieldOffset, GetComplexFldOffset(FieldDesc));
          end;
        end;
      end;
    finally
      if wsBuff <> nil then
        Marshal.FreeHGlobal(wsBuff);
    end;
  end;

  procedure ClearBlock;
  var
    i: integer;
  begin
  // Free complex fields
    if not HasComplexFields then
      Exit;
    for i := 0 to FFetchRows - 1 do
      FreeComplexFields(PtrOffset(Block, SizeOf(TBlockHeader) + i * ItemSize + SizeOf(TItemHeader)), True);
  end;

  procedure SwapBlocks(Block1, Block2: PBlockHeader);
  begin
    Block1.Prev := nil;
    Block1.Next := Block2;
    Block2.Next := nil;
    Block2.Prev := Block1;
  end;

begin
  ItemSize := RecordSize + SizeOf(TItemHeader);
  NewBlock := (IntPtr(BlockMan.FirstBlock) = nil) or not FUniDirectional;
  if NewBlock then begin
    BlockMan.AllocBlock(Block, FFetchRows);

    if HasComplexFields or FHasFlatUnicodeFields then begin
      if IntPtr(FFetchBlock) = nil then
        AllocFetchBlock;
      InitBlock;
    end;
  end
  else begin
    if IntPtr(BlockMan.FirstBlock.Next) <> nil then begin // more then one block
      NewBlock := True; // for redefine data
      if FEOF or FBOF then
        Block := BlockMan.FirstBlock
      else begin
        // BlockMan.FirstBlock.Next always present FFetchStart position
        TempBlock := BlockMan.FirstBlock;
        Block := TempBlock.Next;
        SwapBlocks(Block, TempBlock);
        BlockMan.FirstBlock := Block;
        if (IntPtr(FirstItem) <> nil) and ((IntPtr(LastItem) = nil) or (IntPtr(FirstItem.Block) <> IntPtr(LastItem.Block))) then begin
          FirstItem := PtrOffset(TempBlock, SizeOf(TBlockHeader));
          Inc(FFetchStart, Block.UsedItems);
        end;
      end
    end
    else
      Block := BlockMan.FirstBlock;
    // Refresh block: drop values of blobs
    ClearBlock;
    InitBlock;
  end;

  // remeber first item and last item on case of exception
  OldFirstItem := FirstItem;
  OldLastItem := LastItem;
  OldFetchStart := FFetchStart;
  OldFetchEnd := FFetchEnd;
  try     // For free memory
    pRec := PtrOffset(Block, SizeOf(TBlockHeader) + sizeof(TItemHeader));
    pInd := PtrOffset(pRec, DataSize);

  { DefineData }
    ComplexFieldOffset := 0;
    ActualFieldCount := 0;
    for i := 0 to FieldCount - 1 do begin
      FieldDesc := Fields[i];
      if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then begin  //Do not fetch Child, Calculated or LookUp fields
        // this code was never called before Blob as Memo support
        {if (FieldDesc.DataType in [dtMemo]) and
          (FieldDesc.SubDataType <> dtString) and (FieldDesc.SubDataType <> dtWideString) then begin
          Inc(ComplexFieldOffset, SizeOf(IntPtr));
        end
        else begin}
          if IsFetchBlockField(FieldDesc.DataType) then begin
            BufferPtr := PtrOffset(FFetchBlock, ComplexFieldOffset);
            Inc(ComplexFieldOffset, GetComplexFldOffset(FieldDesc));
          end
          else begin
            if (FieldDesc.DataType = dtString)
               and not FieldDesc.Fixed then
              BufferPtr := PtrOffset(pRec, FieldDesc.Offset - SizeOf(Short)) //To Fetch VARCHAR types with length
            else
              BufferPtr := PtrOffset(pRec, FieldDesc.Offset);
          end;

          IndPtr := PtrOffset(pInd, i * SizeOf(Short));
          //Define data ptr and Ind Ptr to FieldSQLDA
          FFieldXSQLDA.FXSQLVARs[ActualFieldCount].sqlind := IndPtr;
          FFieldXSQLDA.FXSQLVARs[ActualFieldCount].sqldata := BufferPtr;
          Inc(ActualFieldCount);
        //end;
      end;
    end;

    if FNoData and (FFetchedRows > FFetchEnd) then begin
      NeedFetch := FFetchedRows - FFetchEnd;
      if NeedFetch > FFetchRows then
        NeedFetch := FFetchRows;
    end
    else
      NeedFetch := FFetchRows;

    FCommand.CheckActive;
    Fetched := 0;
    for i := 0 to NeedFetch - 1 do begin
      GDS.Busy;
      Res := GDS.isc_dsql_fetch(FCommand.FStatusVector, FCommand.FStmtHandle, FConnection.FSQLDialect, FFieldXSQLDA.FXSQLDA);
      GDS.Release;

      if Res <> 0 then
        if Res = 100 then begin
          FCommand.SetCursorState(csFetched);
          Break;
        end
        else
          try
            Check(Res);
          except
            FCommand.SetCursorState(csFetched);
            raise;
          end
      else begin
        Inc(Fetched);
        Inc(FFetchedRows);

        //Setup SQLVARs with new data offsets
        ActualFieldCount := 0;
        for j := 0 to FieldCount - 1 do begin
          FieldDesc := Fields[j];
          if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then begin
            IndPtr := FFieldXSQLDA.FXSQLVARs[ActualFieldCount].sqlind;
            BufferPtr := FFieldXSQLDA.FXSQLVARs[ActualFieldCount].sqldata;
            //Terminate fetched strings
            case FFieldXSQLDA.FXSQLVARs[ActualFieldCount].sqltype of
              SQL_TEXT, SQL_VARYING: begin
                if FieldDesc.Fixed then
                  TextLen := word(FFieldXSQLDA.FXSQLVARs[ActualFieldCount].sqllen)
                else
                  TextLen := word(Marshal.ReadInt16(BufferPtr) + SizeOf(Short));
                Marshal.WriteByte(BufferPtr, TextLen, 0);
                //SetStrTerminator(BufferPtr, TextLen, FConnection.FCharLength);
              end;
              SQL_FLOAT: begin
              {$IFDEF CLR}
                Marshal.WriteInt64(BufferPtr, BitConverter.DoubleToInt64Bits(BitConverter.ToSingle(BitConverter.GetBytes(Marshal.ReadInt32(BufferPtr)), 0)));
              {$ELSE}
                Double(BufferPtr^) := Single(BufferPtr^);
              {$ENDIF}
              end;
            end;

            if IsFetchBlockField(FieldDesc.DataType) then
              BufferPtr := PtrOffset(BufferPtr, FFetchBlockItemSize)
            else
              BufferPtr := PtrOffset(BufferPtr, ItemSize);

            if not FFieldXSQLDA.FXSQLVARs[ActualFieldCount].IsNullable then
              Marshal.WriteInt16(IndPtr, 0); //NotNull
            IndPtr := PtrOffset(IndPtr, ItemSize);
            FFieldXSQLDA.FXSQLVARs[ActualFieldCount].sqlind := IndPtr;
            FFieldXSQLDA.FXSQLVARs[ActualFieldCount].sqldata := BufferPtr;
            Inc(ActualFieldCount);
          end;
        end;//for j
      end;
    end;//for i

    if FCommand.GetCursorState < csFetching then
      FCommand.SetCursorState(csFetching);

    Inc(FFetchEnd, Fetched);

    Result := Fetched > 0;
    if Result then begin
      if HasFields([dtMemo, dtWideMemo, dtBlob, dtArray, dtExtString, dtExtWideString,
        dtWideString, dtExtVarBytes, dtDate, dtTime, dtDateTime
        {$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}])
      then
        PrepareBlock(Fetched);

      // Prepare complex field
      Item := PtrOffset(Block, SizeOf(TBlockHeader));
      for i := 1 to Fetched do begin
        if HasComplexFields then
          for j := 0 to FieldCount - 1 do begin
            FieldDesc := Fields[j];
            if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then begin
              ObjPtr := Marshal.ReadIntPtr(PtrOffset(Item, SizeOf(TItemHeader) + FieldDesc.Offset));
              case FieldDesc.DataType of
                dtBlob, dtMemo, dtWideMemo: begin
                  if (FieldDesc.SubDataType <> dtString) and (FieldDesc.SubDataType <> dtWideString) then begin
                    TIBCBlob(GetGCHandleTarget(ObjPtr)).FNeedReadBlob := True;
                    if not FDeferredBlobRead and FCommand.FCacheBlobs then begin
                      TIBCBlob(GetGCHandleTarget(ObjPtr)).ReadBlob;
                    {$IFDEF HAVE_COMPRESS}
                      if (FCommand.FCompressBlob = cbClient) or (FCommand.FCompressBlob = cbClientServer) then
                          TIBCBlob(GetGCHandleTarget(ObjPtr)).Compressed := True;
                    {$ENDIF}
                    end;
                  end
                end;
                dtArray:
                  if not FDeferredArrayRead and FCommand.FCacheArrays then
                    TCustomIBCArray(GetGCHandleTarget(ObjPtr)).ReadArray;
              end;
            end;
          end;
        Item := PtrOffset(Item, ItemSize);
      end;

      CreateBlockStruct(Block, Fetched);
      if (HasComplexFields or FHasFlatUnicodeFields) and (FCommand.GetCursorState in [csFetched, csInactive]) then
        FreeFetchBlock;
    end // if Result then begin
    else begin
      if NewBlock then begin
        if HasComplexFields then
          ClearBlock;
        BlockMan.FreeBlock(Block);
      end;

      if HasComplexFields or FHasFlatUnicodeFields then
        FreeFetchBlock;
    end;
  except
    if NewBlock then begin
      if HasComplexFields then
        ClearBlock; // !!! Possible AV on fatal error
      // BlockMan.FirstBlock = nil means that dataset was closed after some
      // fatal error and all blocks are already freed.
      if IntPtr(BlockMan.FirstBlock) <> nil then begin
        BlockMan.FreeBlock(Block);
        // restore first and last items
        FirstItem := OldFirstItem;
        LastItem := OldLastItem;
        if IntPtr(FirstItem) <> nil then
          FirstItem.Prev := nil;
        if IntPtr(LastItem) <> nil then
          LastItem.Next := nil;
        // restore fetch origins
        FFetchStart := OldFetchStart;
        FFetchEnd := OldFetchEnd;
      end;
    end;
    raise;
  end;  // try
end;

function TGDSRecordSet.Fetch(FetchBack: boolean = False): boolean;
var
  Cancel: boolean;
  OldCommandType: TCommandType;
begin
  Result := False;

  if not (FCommand.GetCursorState in [csFetched, csInactive]) then begin
    DoBeforeFetch(Cancel);
    if Cancel then
      Exit;
    try
      try
        Result := InternalFetch;
      except
        FEOF := True;
        raise;
      end;
    finally
      DoAfterFetch;
    end;
  end;

  if (FCommand.GetCursorState = csFetched) and FAutoClose then begin
    OldCommandType := CommandType;
    InternalUnPrepare;
    // We need to save old CommandType to save old FieldDescs on Refresh (for SQL Generator).
    CommandType := OldCommandType;
    Prepared := False;
  end;
end;

procedure TGDSRecordSet.FetchAll;
begin
  if (FCommand.GetCursorState < csFetchingAll) and FCommand.GetActive then begin
    FCommand.SetCursorState(csFetchingAll);
    try
      while Fetch do;
    except
      if FCommand.GetCursorState <> csInactive then
        FCommand.SetCursorState(csFetched);
      raise;
    end;
  end;
end;

function TGDSRecordSet.RowsReturn: boolean;
begin
 if CommandType <> ctUnknown then
    Result := inherited RowsReturn
  else                              //we need to know this info even if CommandType is not set(TCustomDADataSet.DoAfterExecute)
    Result := (FCommand.FSQLType = SQL_SELECT) or (FCommand.FSQLType = SQL_SELECT_FOR_UPD);
end;

{ Navigation }
procedure TGDSRecordSet.SetToEnd;
begin
  FetchAll;

  inherited;
end;

procedure TGDSRecordSet.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TGDSConnection(Value);
end;

function TGDSRecordSet.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

function TGDSRecordSet.GetGDS: TGDS;
begin
  Result := FConnection.FGDS;
end;

function TGDSRecordSet.GetIsFBConnection: boolean;
begin
  if FConnection <> nil then
    FIsFBConnection := FConnection.IsFBServer;
  Result := FIsFBConnection;
end;

function TGDSRecordSet.GetMinorServerVersion: integer;
begin
  if FConnection <> nil then
    FMinorServerVersion := FConnection.GetMinorServerVersion;
  Result := FMinorServerVersion;
end;

function TGDSRecordSet.GetMajorServerVersion: integer;
begin
  if FConnection <> nil then
    FMajorServerVersion := FConnection.GetMajorServerVersion;
  Result := FMajorServerVersion;
end;

function TGDSRecordSet.GetUseUnicode: boolean;
begin
  if FConnection <> nil then
    FUseUnicode := FConnection.FUseUnicode;
  Result := FUseUnicode;
end;

function TGDSRecordSet.GetCharLength: integer;
begin
  if FConnection <> nil then
    FCharLength := FConnection.FCharLength;
  Result := FCharLength;
end;

function TGDSRecordSet.GetSQLDialect: integer;
begin
  if FConnection <> nil then
    FSQLDialect := FConnection.FSQLDialect;
  Result := FSQLDialect;
end;

function TGDSRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prFetchRows: begin
      FreeFetchBlock;
      Result := inherited SetProp(Prop, Value);
    end;
    prAutoClose:
      FAutoClose := Boolean(Value);
    prFieldsAsString:
      FFieldsAsString := Boolean(Value);
    prComplexArrayFields:
      FComplexArrayFields := Boolean(Value);
    prDeferredArrayRead:
      FDeferredArrayRead := Boolean(Value);
    prDeferredBlobRead:
      FDeferredBlobRead := Boolean(Value);
    prBooleanDomainFields:
      FBooleanDomainFields := Boolean(Value);
    prSetDomainNames:
      FSetDomainNames := Boolean(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSRecordSet.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCursor:
      Value := Marshal.PtrToStringAnsi(FCommand.FCursor);
    prRowsFetched:
      Value := FFetchedRows;
    prAutoClose:
      Value := FAutoClose;
    prFieldsAsString:
      Value := FFieldsAsString;
    prComplexArrayFields:
      Value := FComplexArrayFields;
    prDeferredArrayRead:
      Value := FDeferredArrayRead;
    prDeferredBlobRead:
      Value := FDeferredBlobRead;
    prBooleanDomainFields:
      Value := FBooleanDomainFields;
    prSetDomainNames:
      Value := FSetDomainNames;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TGDSRecordSet.GetFieldAsVariant(FieldNo: word; RecBuf: IntPtr;
  var Value: variant);
var
  Field: TFieldDesc;
  FieldData: IntPtr;
  IsBlank, NativeBuffer: boolean;
  ObjPtr: IntPtr;

begin
  if GetNull(FieldNo, RecBuf) then begin
    Value := Null;
    Exit;
  end;

  Value := Unassigned; // Delphi bug
  Field := Fields[FieldNo - 1];

  if not Field.HasParent then begin
    FieldData := PtrOffset(RecBuf, Field.Offset);
    NativeBuffer := True;
  end
  else
    GetChildField(Field, RecBuf, FieldData, IsBlank, NativeBuffer);

  try
    ObjPtr := Marshal.ReadIntPtr(FieldData);
    case Field.DataType of
      dtBlob, dtMemo, dtWideMemo:
//blob SubType is rarely set to right value:   if TIBCBlob(GetGCHandleTarget(ObjPtr)).SubType = isc_blob_text then
        if (Field.SubDataType <> dtString) and (Field.SubDataType <> dtWideString) then
          Value := TBlob(GetGCHandleTarget(ObjPtr)).AsString
        else
          inherited;
      dtArray:
        Value := TCustomIBCArray(GetGCHandleTarget(ObjPtr)).Items;
    else
      inherited;
    end
  finally
    if not NativeBuffer then
      Marshal.FreeHGlobal(FieldData);
  end;
end;

{$IFNDEF LITE}

{ TGDSMetaData }

function TGDSMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TGDSRecordSet.Create;
  Result.SetProp(prAutoClose, True);
  Result.TrimFixedChar := True;
end;

function TGDSMetaData.InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData;
begin
  if MetaDataKind = 'generators' then
    Result := GetGenerators(Restrictions)
  else
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions);
end;

procedure TGDSMetaData.InternalGetMetaDataKindsList(List: _TStringList);
begin
  inherited;

  List.Add('Roles');
  List.Add('Generators');

  List.Sort;
end;

procedure TGDSMetaData.InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string);
begin
  List.Clear;

  if MetaDataKind = 'roles' then begin
    List.Add('ROLE_NAME');
  end
  else
  if MetaDataKind = 'generators' then begin
    List.Add('GENERATOR_NAME');
  end
  else
    inherited;
end;

function TGDSMetaData.GetTables(Restrictions: _TStrings): TData;
const
  fmtGetTablesSQL = 'SELECT RDB$RELATION_NAME, ' +
    'RDB$VIEW_BLR, RDB$SYSTEM_FLAG, RDB$DESCRIPTION ' +
    'FROM RDB$RELATIONS %s ' +
    'ORDER BY RDB$RELATION_NAME';
var
  WhereClause, TableName, TableTypes, Scope, TypeFilter: _string;
  BoolTypes: TBooleanArray;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));
  if Scope <> 'LOCAL' then
    TableTypes := Trim(Restrictions.Values['TABLE_TYPE'])
  else
    TableTypes := 'TABLE, VIEW';

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$RELATION_NAME', TableName);

  BoolTypes := nil;
  if TableTypes <> '' then begin
    BoolTypes := ParseTypes(TableTypes, ['TABLE', 'VIEW', 'SYSTEM TABLE']);

    TypeFilter := '';
    if not BoolTypes[0] then
      TypeFilter := 'NOT (RDB$VIEW_BLR IS NULL)';
    if not BoolTypes[1] then begin
      if TypeFilter <> '' then
        TypeFilter := TypeFilter + ' AND ';
      TypeFilter := TypeFilter + 'RDB$VIEW_BLR IS NULL';
    end;
    if TypeFilter <> '' then
      TypeFilter := TypeFilter + ' AND ';
    TypeFilter := TypeFilter + 'RDB$SYSTEM_FLAG = 0';

    if BoolTypes[2] then begin
      if TypeFilter <> '' then
        TypeFilter := TypeFilter + ' OR ';
      TypeFilter := TypeFilter + 'RDB$SYSTEM_FLAG <> 0';
    end;

    if TypeFilter <> '' then begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND (' + TypeFilter + ')'
      else
        WhereClause := TypeFilter;
    end;
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetTablesSQL, [WhereClause]));
  FRecordSet.Open;
  CopyTablesData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METAINFO}
procedure TGDSMetaData.CreateTablesFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('TABLE_TYPE', dtString, 20);
  AddField('DESCRIPTION', dtMemo);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyTablesData(Restrictions: _TStrings);
const
  snTABLE_NAME    = 1;
  snVIEW_BLR      = 2;
  snSYSTEM_FLAG   = 3;
  snDESCRIPTION   = 4;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
  dnDESCRIPTION   = 5;
var
  TypeName: string;
  Value: variant;
begin
  CreateTablesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snDESCRIPTION], [dnTABLE_NAME, dnDESCRIPTION]);

    if FRecordSetHelper.FieldValues[snSYSTEM_FLAG] = 1 then
      TypeName := 'SYSTEM TABLE'
    else begin
      Value := FRecordSetHelper.FieldValues[snVIEW_BLR];
      if VarIsNull(Value) then
        TypeName := 'TABLE'
      else
        TypeName := 'VIEW';
    end;

    FMemDataHelper.FieldValues[dnTABLE_TYPE] := TypeName;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetColumns(Restrictions: _TStrings): TData;
const
  fmtGetColumnsSQL = 'SELECT RF.RDB$RELATION_NAME, RF.RDB$FIELD_NAME, ' +
    'RF.RDB$FIELD_POSITION, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, ' +
    'F.RDB$FIELD_LENGTH, F.RDB$FIELD_PRECISION, F.RDB$FIELD_SCALE, ' +
    'RF.RDB$NULL_FLAG, ' +
    'CASE WHEN RF.RDB$DEFAULT_VALUE IS NOT NULL THEN RF.RDB$DEFAULT_VALUE ELSE F.RDB$DEFAULT_VALUE END, ' +
    'CASE WHEN RF.RDB$DESCRIPTION IS NOT NULL THEN RF.RDB$DESCRIPTION ELSE F.RDB$DESCRIPTION END ' +
    'FROM RDB$RELATION_FIELDS RF ' +
    'INNER JOIN RDB$FIELDS F ON (RF.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME) %s ' +
    'ORDER BY RF.RDB$RELATION_NAME, RF.RDB$FIELD_POSITION';
var
  WhereClause, TableName, ColumnName: _string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'RF.RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'RF.RDB$FIELD_NAME', ColumnName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  CopyColumnsData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METAINFO}
procedure TGDSMetaData.CreateColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('POSITION', dtInt32);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_SUBTYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  AddField('NULLABLE', dtInt32);
  AddField('DEFAULT_VALUE', dtMemo);
  AddField('DESCRIPTION', dtMemo);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyColumnsData(Restrictions: _TStrings);
const
  snTABLE_NAME    = 1;
  snCOLUMN_NAME   = 2;
  snPOSITION      = 3;
  snTYPE          = 4;
  snSUBTYPE       = 5;
  snLENGTH        = 6;
  snPRECISION     = 7;
  snSCALE         = 8;
  snNULL_FLAG     = 9;
  snDEFAULT_VALUE = 10;
  snDESCRIPTION   = 11;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnCOLUMN_NAME   = 4;
  dnPOSITION      = 5;
  dnTYPE          = 6;
  dnSUBTYPE       = 7;
  dnLENGTH        = 8;
  dnPRECISION     = 9;
  dnSCALE         = 10;
  dnNULLABLE      = 11;
  dnDEFAULT_VALUE = 12;
  dnDESCRIPTION   = 13;
var
  Value: variant;
  Nullable: integer;
begin
  CreateColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_NAME, snCOLUMN_NAME, snPOSITION, snTYPE, snSUBTYPE, snLENGTH, snPRECISION, snSCALE, snDEFAULT_VALUE, snDESCRIPTION],
      [dnTABLE_NAME, dnCOLUMN_NAME, dnPOSITION, dnTYPE, dnSUBTYPE, dnLENGTH, dnPRECISION, dnSCALE, dnDEFAULT_VALUE, dnDESCRIPTION]);

    Value := FRecordSetHelper.FieldValues[snNULL_FLAG];
    if VarIsNull(Value) then
      Nullable := 1
    else
      Nullable := 0;
    FMemDataHelper.FieldValues[dnNULLABLE] := Nullable;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetProcedures(Restrictions: _TStrings): TData;
const
  fmtGetProceduresSQL = 'SELECT RDB$PROCEDURE_NAME, ' +
    'RDB$PROCEDURE_INPUTS, RDB$PROCEDURE_OUTPUTS ' +
    'FROM RDB$PROCEDURES ' +
    '%s ORDER BY RDB$PROCEDURE_NAME';
var
  WhereClause, ProcName: _string;
begin
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$PROCEDURE_NAME', ProcName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetProceduresSQL, [WhereClause]));
  FRecordSet.Open;
  CopyProceduresData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METAINFO}
procedure TGDSMetaData.CreateProceduresFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 0);
  AddField('PROCEDURE_SCHEMA', dtString, 0);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PROCEDURE_TYPE', dtString, 0);
  AddField('IN_PARAMETERS', dtInt32);
  AddField('OUT_PARAMETERS', dtInt32);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyProceduresData(Restrictions: _TStrings);
const
  snPROCEDURE_NAME = 1;
  snIN_PARAMETERS  = 2;
  snOUT_PARAMETERS = 3;

  dnCATALOG        = 1;
  dnSCHEMA         = 2;
  dnPROCEDURE_NAME = 3;
  dnPROCEDURE_TYPE = 4;
  dnIN_PARAMETERS  = 5;
  dnOUT_PARAMETERS = 6;
var
  Value: variant;
begin
  CreateProceduresFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snPROCEDURE_NAME], [dnPROCEDURE_NAME]);

    Value := FRecordSetHelper.FieldValues[snIN_PARAMETERS];
    if VarIsNull(Value) then
      Value := 0;
    FMemDataHelper.FieldValues[dnIN_PARAMETERS] := Value;

    Value := FRecordSetHelper.FieldValues[snOUT_PARAMETERS];
    if VarIsNull(Value) then
      Value := 0;
    FMemDataHelper.FieldValues[dnOUT_PARAMETERS] := Value;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetProcedureParameters(Restrictions: _TStrings): TData;
const
  fmtGetProcedureParametersSQL = 'SELECT P.RDB$PROCEDURE_NAME, P.RDB$PARAMETER_NAME, ' +
    'P.RDB$PARAMETER_NUMBER, P.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE, ' +
    'F.RDB$FIELD_LENGTH, F.RDB$FIELD_PRECISION, F.RDB$FIELD_SCALE ' +
    'FROM RDB$PROCEDURE_PARAMETERS P ' +
    'INNER JOIN RDB$FIELDS F ON (P.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME) ' +
    '%s ORDER BY P.RDB$PROCEDURE_NAME, P.RDB$PARAMETER_TYPE, P.RDB$PARAMETER_NUMBER';
var
  WhereClause, ProcName, ParamName: _string;
begin
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ParamName := Trim(Restrictions.Values['PARAMETER_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'P.RDB$PROCEDURE_NAME', ProcName);
  AddWhere(WhereClause, 'P.RDB$PARAMETER_NAME', ParamName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetProcedureParametersSQL, [WhereClause]));
  FRecordSet.Open;
  CopyProcedureParametersData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METAINFO}
procedure TGDSMetaData.CreateProcedureParametersFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 0);
  AddField('PROCEDURE_SCHEMA', dtString, 0);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PARAMETER_NAME', dtString, 100);
  AddField('POSITION', dtInt32);
  AddField('DIRECTION', dtString, 10);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_SUBTYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyProcedureParametersData(Restrictions: _TStrings);
const
  snPROCEDURE_NAME = 1;
  snPARAMETER_NAME = 2;
  snPOSITION      = 3;
  snDIRECTION     = 4;
  snTYPE          = 5;
  snSUBTYPE       = 6;
  snLENGTH        = 7;
  snPRECISION     = 8;
  snSCALE         = 9;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnPROCEDURE_NAME = 3;
  dnPARAMETER_NAME = 4;
  dnPOSITION      = 5;
  dnDIRECTION     = 6;
  dnTYPE          = 7;
  dnSUBTYPE       = 8;
  dnLENGTH        = 9;
  dnPRECISION     = 10;
  dnSCALE         = 11;
var
  Value: variant;
  Direction: string;
begin
  CreateProcedureParametersFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snPROCEDURE_NAME, snPARAMETER_NAME, snPOSITION, snTYPE, snSUBTYPE, snLENGTH, snPRECISION, snSCALE],
      [dnPROCEDURE_NAME, dnPARAMETER_NAME, dnPOSITION, dnTYPE, dnSUBTYPE, dnLENGTH, dnPRECISION, dnSCALE]);

    Value := FRecordSetHelper.FieldValues[snDIRECTION];
    if Value = 0 then
      Direction := 'IN'
    else
      Direction := 'OUT';
    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetIndexes(Restrictions: _TStrings): TData;
const
  fmtGetIndexesSQL = 'SELECT RDB$RELATION_NAME, RDB$INDEX_NAME, ' +
    'RDB$UNIQUE_FLAG, RDB$INDEX_TYPE ' +
    'FROM RDB$INDICES ' +
    '%s ORDER BY RDB$RELATION_NAME, RDB$INDEX_NAME';
var
  WhereClause, TableName, IndexName: _string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'RDB$INDEX_NAME', IndexName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetIndexesSQL, [WhereClause]));
  FRecordSet.Open;
  CopyIndexesData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METAINFO}
procedure TGDSMetaData.CreateIndexesFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 0);
  AddField('INDEX_SCHEMA', dtString, 0);
  AddField('INDEX_NAME', dtString, 100);
  AddField('UNIQUE', dtInt32);
  AddField('SORT_ORDER', dtString, 4);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyIndexesData(Restrictions: _TStrings);
const
  snTABLE_NAME    = 1;
  snINDEX_NAME    = 2;
  snUNIQUE_FLAG   = 3;
  snINDEX_TYPE    = 4;

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
  dnSORT_ORDER    = 8;
var
  SortOrder: string;
  Unique: integer;
begin
  CreateIndexesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snINDEX_NAME], [dnTABLE_NAME, dnINDEX_NAME]);

    if FRecordSetHelper.FieldValues[snUNIQUE_FLAG] = 1 then
      Unique := 1
    else
      Unique := 0;
    FMemDataHelper.FieldValues[dnUNIQUE] := Unique;

    if FRecordSetHelper.FieldValues[snINDEX_TYPE] = 1 then
      SortOrder := 'DESC'
    else
      SortOrder := 'ASC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := SortOrder;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetIndexColumns(Restrictions: _TStrings): TData;
const
  fmtGetIndexColumnsSQL = 'SELECT I.RDB$RELATION_NAME TABLE_NAME, I.RDB$INDEX_NAME INDEX_NAME, ' +
    'S.RDB$FIELD_NAME COLUMN_NAME, S.RDB$FIELD_POSITION COLUMN_POSITION, ' +
    'I.RDB$INDEX_TYPE ' +
    'FROM RDB$INDICES I ' +
    'INNER JOIN RDB$INDEX_SEGMENTS S ON (I.RDB$INDEX_NAME = S.RDB$INDEX_NAME) ' +
    '%s ORDER BY S.RDB$INDEX_NAME, S.RDB$FIELD_POSITION';
var
  WhereClause, TableName, IndexName, Uniqueness: _string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  Uniqueness := Trim(Restrictions.Values['UNIQUE']);

  WhereClause := '';
  AddWhere(WhereClause, 'I.RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'I.RDB$INDEX_NAME', IndexName);
  if (Uniqueness = '0') or (Uniqueness = '1') then
    AddWhere(WhereClause, 'I.RDB$UNIQUE_FLAG', Uniqueness);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetIndexColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  CopyIndexColumnsData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METAINFO}
procedure TGDSMetaData.CreateIndexColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 0);
  AddField('INDEX_SCHEMA', dtString, 0);
  AddField('INDEX_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('COLUMN_POSITION', dtInt32);
  AddField('SORT_ORDER', dtString, 4);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyIndexColumnsData(Restrictions: _TStrings);
const
  snTABLE_NAME    = 1;
  snINDEX_NAME    = 2;
  snCOLUMN_NAME   = 3;
  snPOSITION      = 4;
  snINDEX_TYPE    = 5;

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnCOLUMN_NAME   = 7;
  dnPOSITION      = 8;
  dnSORT_ORDER    = 9;
var
  SortOrder: string;
begin
  CreateIndexColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snINDEX_NAME, snCOLUMN_NAME, snPOSITION],
      [dnTABLE_NAME, dnINDEX_NAME, dnCOLUMN_NAME, dnPOSITION]);

    if FRecordSetHelper.FieldValues[snINDEX_TYPE] = 1 then
      SortOrder := 'DESC'
    else
      SortOrder := 'ASC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := SortOrder;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetConstraints(Restrictions: _TStrings): TData;
const
  fmtGetConstraintsSQL = 'SELECT RDB$RELATION_NAME, RDB$CONSTRAINT_NAME, ' +
    'RDB$CONSTRAINT_TYPE, RDB$INDEX_NAME ' +
    'FROM RDB$RELATION_CONSTRAINTS ' +
    '%s ORDER BY RDB$RELATION_NAME, RDB$CONSTRAINT_NAME';
var
  WhereClause, TableName, ConstraintName, Types, TypesFilter: _string;
  BoolTypes: TBooleanArray;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := Trim(Restrictions.Values['CONSTRAINT_TYPE']);

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$RELATION_NAME', TableName);
  AddWhere(WhereClause, 'RDB$CONSTRAINT_NAME', ConstraintName);

  BoolTypes := nil;
  if Types <> '' then begin
    BoolTypes := ParseTypes(Types, ['CHECK', 'PRIMARY KEY', 'UNIQUE', 'FOREIGN KEY', 'UNKNOWN']);
    if not (BoolTypes[0] and BoolTypes[1] and BoolTypes[2] and BoolTypes[3] and BoolTypes[4])
    then begin
      TypesFilter := '';
      for i := 0 to High(BoolTypes) - 1 do begin
        if BoolTypes[i] xor BoolTypes[4] then begin
          if TypesFilter <> '' then
            TypesFilter := TypesFilter + ', ';
          case i of
            0: TypesFilter := TypesFilter + '''PCHECK'', ''NOT NULL''';
            1: TypesFilter := TypesFilter + '''PRIMARY KEY''';
            2: TypesFilter := TypesFilter + '''UNIQUE''';
            3: TypesFilter := TypesFilter + '''FOREIGN KEY''';
          end;
        end;
      end;
      if TypesFilter = '' then
        TypesFilter := '0 = 1'
      else begin
        TypesFilter := 'RDB$CONSTRAINT_TYPE IN (' + TypesFilter + ')';
        if BoolTypes[4] then
          TypesFilter := 'NOT ' + TypesFilter;
      end;
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      WhereClause := WhereClause + TypesFilter;
    end;
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetConstraintsSQL, [WhereClause]));
  FRecordSet.Open;
  CopyConstraintsData(Restrictions);
  FRecordSet.Close;
  Result := FMemData;
end;

{$IFNDEF DBX_METAINFO}
procedure TGDSMetaData.CreateConstraintsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 0);
  AddField('TABLE_SCHEMA', dtString, 0);
  AddField('TABLE_NAME', dtString, 100);
  AddField('CONSTRAINT_NAME', dtString, 100);
  AddField('CONSTRAINT_TYPE', dtString, 100);
  AddField('INDEX_CATALOG', dtString, 0);
  AddField('INDEX_SCHEMA', dtString, 0);
  AddField('INDEX_NAME', dtString, 100);
  FMemData.InitFields;
end;
{$ENDIF}

procedure TGDSMetaData.CopyConstraintsData(Restrictions: _TStrings);
const
  snTABLE_NAME      = 1;
  snCONSTRAINT_NAME = 2;
  snCONSTRAINT_TYPE = 3;
  snINDEX_NAME      = 4;

  dnCATALOG         = 1;
  dnSCHEMA          = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCONSTRAINT_TYPE = 5;
  dnINDEX_CATALOG   = 6;
  dnINDEX_SCHEMA    = 7;
  dnINDEX_NAME      = 8;
var
  SourceType, DestType: string;
begin
  CreateConstraintsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_NAME, snCONSTRAINT_NAME, snINDEX_NAME],
      [dnTABLE_NAME, dnCONSTRAINT_NAME, dnINDEX_NAME]);

    SourceType := VarToStr(FRecordSetHelper.FieldValues[snCONSTRAINT_TYPE]);
    if (SourceType = 'PRIMARY KEY') or (SourceType = 'FOREIGN KEY') or
      (SourceType = 'UNIQUE')
    then
      DestType := SourceType
    else
    if (SourceType = 'PCHECK') or (SourceType = 'NOT NULL') then
      DestType := 'CHECK'
    else
      DestType := 'UNKNOWN';
    FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := DestType;

    FMemDataHelper.AppendRecord;
  end;
  FMemData.SetToBegin;
end;

function TGDSMetaData.GetRoles(Restrictions: _TStrings): TData;
const
  GetRolesSQL = 'SELECT RDB$ROLE_NAME ROLE_NAME ' +
    'FROM RDB$ROLES ' +
    '%s ORDER BY RDB$ROLE_NAME';
var
  WhereClause, RoleName: _string;
begin
  RoleName := Trim(Restrictions.Values['ROLE_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'RDB$ROLE_NAME', RoleName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(GetRolesSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TGDSMetaData.GetGenerators(Restrictions: _TStrings): TData;
const
  SQL = 'SELECT RDB$GENERATOR_NAME GENERATOR_NAME ' +
    'FROM RDB$GENERATORS ' +
    '%s ORDER BY RDB$GENERATOR_NAME';
var
  WhereClause, ObjName, Scope: _string;
begin
  ObjName := Trim(Restrictions.Values['GENERATOR_NAME']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';

  if Scope = 'LOCAL' then
    WhereClause := 'RDB$SYSTEM_FLAG = 0';

  AddWhere(WhereClause, 'RDB$GENERATOR_NAME', ObjName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(SQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

{ TGDSLoader }

constructor TGDSLoader.Create;
begin
  inherited;

  FCommand := TGDSCommand.Create;
  FCommand.SetProp(prDisableParamScan, True);
  FInsertMode := _imInsert;
  FRowsPerBatch := 50;
end;

destructor TGDSLoader.Destroy;
begin
  FCommand.Free;

  inherited;
end;

function TGDSLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prInsertMode:
      FInsertMode := _TIBCInsertMode(Value);
    prRowsPerBatch:
      FRowsPerBatch := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TGDSLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prInsertMode:
      Value := Variant(FInsertMode);
    prRowsPerBatch:
      Value := FRowsPerBatch;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

class function TGDSLoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TGDSLoaderColumn;
end;

procedure TGDSLoader.Reset;
var
  i: integer;
  Param: TIBCParamDesc;
begin
  inherited;

  for i := 0 to FCommand.Params.Count - 1 do begin
    Param := TIBCParamDesc(FCommand.Params[i]);
    if Param.DataType in [dtBlob, dtMemo, dtWideMemo] then
      Param.GetObject.Free;
  end;

  FCommand.Params.Clear;
end;

procedure TGDSLoader.Prepare;
var
  i, BatchSize: integer;
  Col: TGDSLoaderColumn;
begin
  inherited;

  if FConnection.IsFBServer and (FConnection.GetMajorServerVersion >= 2) then
    BatchSize := FRowsPerBatch
  else
    BatchSize := 1;

  case FInsertMode of
    _imInsert:
      FInsertHeader := 'INSERT INTO ';
    _imUpdateOrInsert:
      FInsertHeader := 'UPDATE OR INSERT INTO ';
  else
    Assert(False);
  end;
  FInsertHeader := FInsertHeader + FCommand.SQLInfo.NormalizeName(FTableName) + '(';

  for i := 0 to Columns.Count - 1 do begin
    Col := TGDSLoaderColumn(Columns[i]);
    if i > 0 then
      FInsertHeader := FInsertHeader + ',';
    FInsertHeader := FInsertHeader + FCommand.SQLInfo.NormalizeName(Columns[i].Name);

    if BatchSize > 1 then begin
      case Col.DataType of
        dtUnknown, dtString, dtExtString,
        dtWideString, dtExtWideString,
        dtBytes, dtVarBytes, dtExtVarBytes:
          Col.FDataTypeName := 'VARCHAR(' + IntToStr(Col.Size) + ')';
        dtInt8, dtInt16:
          Col.FDataTypeName := 'SMALLINT';
        dtInt32, dtUInt16:
          Col.FDataTypeName := 'INTEGER';
        dtInt64, dtUInt32:
          Col.FDataTypeName := 'BIGINT';
        dtFloat, dtCurrency:
          Col.FDataTypeName := 'DOUBLE PRECISION';
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        dtFMTBCD,
      {$ENDIF}
      {$ENDIF}
        dtBCD:
          Col.FDataTypeName := 'NUMERIC(' + IntToStr(Col.Precision) + ',' + IntToStr(Col.Scale) + ')';
        dtDate:
          Col.FDataTypeName := 'DATE';
        dtTime:
          Col.FDataTypeName := 'TIME';
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        dtSQLTimeStamp,
      {$ENDIF}
      {$ENDIF}
        dtDateTime:
          Col.FDataTypeName := 'TIMESTAMP';
        dtBoolean:
          Col.FDataTypeName := 'BOOLEAN';
        dtBlob, dtMemo, dtWideMemo:
          Col.FDataTypeName := 'BLOB';
      else
        raise Exception.Create(SUnknownDataType);
      end;
    end;
  end;
  FInsertHeader := FInsertHeader + ') VALUES (';

  FCommand.SetConnection(FConnection);
  FCommand.SetTransaction(FTransaction);

  GenerateSQL(BatchSize);
end;

procedure TGDSLoader.GenerateSQL(BatchSize: integer);
const
  MaxSQLLength = 32768;
var
  r, i: integer;
  Col: TGDSLoaderColumn;
  Param: TIBCParamDesc;
  Blob: TIBCBlob;
  ParamName: _string;
  ParamsSB: _StringBuilder;
  InsertSB: _StringBuilder;
begin
  FCommand.Unprepare;
  FGeneratedRows := 0;
  ParamsSB := _StringBuilder.Create(MaxSQLLength);
  InsertSB := _StringBuilder.Create(MaxSQLLength);
  try
    for r := 0 to BatchSize - 1 do begin
      InsertSB.Append(FInsertHeader);
      for i := 0 to Columns.Count - 1 do begin
        Col := TGDSLoaderColumn(Columns[i]);
        if FCommand.Params.Count <= r * Columns.Count + i then begin
          Param := TIBCParamDesc.Create;
          Param.SetDataType(Col.DataType);
          Param.SetParamType(pdInput);
          Param.SetSize(Col.Size);
          FCommand.Params.Add(Param);
          if Col.DataType in [dtBlob, dtMemo, dtWideMemo] then begin
            Blob := TIBCBlob.Create(TGDSConnection(nil), TGDSTransaction(nil));
            if Col.DataType = dtWideMemo then
              Blob.IsUnicode := True;
            Param.SetObject(Blob);
          end;
        end;

        if BatchSize > 1 then begin
          if ParamsSB.Length > 0 then
            ParamsSB.Append(',');
          ParamName := 'P' + IntToStr(r) + '_' + IntToStr(i);
          ParamsSB.Append(ParamName + ' ' + Col.FDataTypeName + ' =?');

          if i > 0 then
            InsertSB.Append(',');
          InsertSB.Append(':' + ParamName);
        end
        else begin
          if i > 0 then
            InsertSB.Append(',?')
          else
            InsertSB.Append('?');
        end;
      end;
      if BatchSize > 1 then
        InsertSB.Append(');')
      else
        InsertSB.Append(')');
      Inc(FGeneratedRows);
      if ParamsSB.Length + InsertSB.Length >= MaxSQLLength then
        break;
    end;

    for i := FCommand.Params.Count - 1 downto FGeneratedRows * Columns.Count do begin
      Param := TIBCParamDesc(FCommand.Params[i]);
      if Param.DataType in [dtBlob, dtMemo, dtWideMemo] then
        Param.GetObject.Free;
      Param.Free;
      FCommand.Params.Delete(i);
    end;

    if BatchSize > 1 then
      FCommand.SetSQL('EXECUTE BLOCK (' + ParamsSB.ToString + ') AS BEGIN ' +
        InsertSB.ToString + ' END')
    else
      FCommand.SetSQL(InsertSB.ToString);
  finally
    ParamsSB.Free;
    InsertSB.Free;
  end;
  FCommand.Prepare;
end;

procedure TGDSLoader.PutColumnData(Col, Row: integer; const Value: variant);
var
  Param: TIBCParamDesc;
  Blob: TIBCBlob;
begin
  if (FLastRow <> -1) and (Row = FLastRow + 2) and
    (FLastRow + 1 - FLoadedRows >= FGeneratedRows)
  then
    FlushRows;

  inherited;

  Param := TIBCParamDesc(FCommand.Params[(Row - 1 - FLoadedRows) * Columns.Count + Col]);
  if Param.DataType in [dtBlob, dtMemo, dtWideMemo] then begin
    if VarIsNull(Value) or VarIsEmpty(Value) then
      Param.SetNull(True)
    else begin
      Blob := TIBCBlob(Param.GetObject);

    {$IFDEF VER12P}
      if VarType(Value) = varArray + varByte then
        Blob.AsBytes := Value
      else
    {$ENDIF}
      if Blob.IsUnicode then
        Blob.AsWideString := Value
      else
        Blob.AsAnsiString := AnsiString(Value);
    end;
  end
  else
    Param.SetValue(Value);
end;

procedure TGDSLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then begin
    if FLastRow + 1 - FLoadedRows <> FGeneratedRows then
      GenerateSQL(FLastRow + 1 - FLoadedRows);
    FlushRows;
  end;
end;

procedure TGDSLoader.FlushRows;
begin
  FCommand.Execute;
  FLoadedRows := FLastRow + 1; 
end;

procedure TGDSLoader.Finish;
begin
  FCommand.Unprepare;
  Reset;

  inherited;
end;

class function TGDSLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TGDSRecordSet;
end;

procedure TGDSLoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TGDSConnection(Value);
end;

{ TGDSAlerter }

constructor TGDSAlerter.Create;
begin
  inherited;

  FThreads := TList.Create;
end;

destructor TGDSAlerter.Destroy;
begin
  Stop;

  FThreads.Free;

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

function TGDSAlerter.SetProp(Prop: integer; const Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited SetProp(Prop, Value);
  //end;
end;

function TGDSAlerter.GetProp(Prop: integer; var Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited GetProp(Prop, Value);
  //end;
end;

procedure TGDSAlerter.SendEvent(const EventName, Message: _string);
var
  Command: TGDSCommand;
begin
  if not TGDSConnection(FConnection).IsFBServer or
    (TGDSConnection(FConnection).GetMajorServerVersion < 2)
  then
    RaiseError(SSendEventNotSupported);

  Command := TGDSConnection(FConnection).CreateCommand;
  try
    Command.SetTransaction(Transaction);
    Command.SetSQL(
      'EXECUTE BLOCK (Name VARCHAR(1000) = :Name) AS'#13#10 +
      'BEGIN POST_EVENT Name; END');

    with Command.Params[0] do begin
      SetParamType(pdInput);
      if TGDSConnection(FConnection).FUseUnicode then begin
        SetDataType(dtWideString);
        SetSize(Length(WideString(EventName)));
      end
      else begin
        SetDataType(dtString);
        SetSize(Length(AnsiString(EventName)));
      end;
      SetValue(EventName);
    end;

    Command.Execute;
  finally
    Command.Free;
  end;
end;

procedure TGDSAlerter.Start;
var
  i, j: Integer;
  Name: string;
  Thread: TGDSAlerterThread;
begin
  if FActive then
    exit;

  Assert(FEventNames.Count > 0);

{$IFDEF MSWINDOWS}
  AllocIBDACWnd;
{$ENDIF}

  for i := FEventNames.Count - 1 downto 0 do begin
    Name := Trim(FEventNames[i]);
    if Name = '' then
      FEventNames.Delete(i)
    else
    if Length(Name) > IBC_MAX_EVENT_LENGTH - 1 then
      RaiseError(Format(SEventNameTooLong, [IBC_MAX_EVENT_LENGTH]))
    else begin
      for j := 0 to i - 1 do
        if AnsiSameText(Name, FEventNames[j]) then begin
          FEventNames.Delete(i);
          Break;
        end;
    end;
  end;

  FActive := True;
  for i := 0 to (FEventNames.Count - 1) div IBC_MAX_EVENTS do begin
    Thread := TGDSAlerterThread.Create(Self, i);
    FThreads.Add(Thread);
  end;
end;

procedure TGDSAlerter.Stop;
var
  i: integer;
  AlerterThread: TGDSAlerterThread;
begin
  if not FActive then
    exit;

  FActive := False;
  for i := 0 to FThreads.Count - 1 do begin
    AlerterThread := TGDSAlerterThread(FThreads[i]);
    AlerterThread.Terminate;
  {$IFNDEF LINUX}
    if {$IFNDEF CLR}GetCurrentThreadId <> AlerterThread.ThreadID{$ELSE}Thread.CurrentThread <> AlerterThread.Handle{$ENDIF} then begin
      AlerterThread.WaitFor;
      AlerterThread.Free;
    end
    else
      AlerterThread.FreeOnTerminate := True; //when FatalError
  {$ELSE}
    AlerterThread.WaitFor;
    AlerterThread.Free;
  {$ENDIF}
  end;
  FThreads.Clear;
end;

procedure TGDSAlerter.DoOnEvent(const EventName: _string; EventCount: Integer);
var
  InhOnEvent: TCRAlerterEventCallback;
begin
  InhOnEvent := inherited OnEvent;
  if Assigned(InhOnEvent) then
    InhOnEvent(EventName, '');

  if Assigned(FOnEvent) then
    FOnEvent(EventName, EventCount);
end;

function TGDSAlerter.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

{ TGDSAlerterThread }

constructor TGDSAlerterThread.Create(Alerter: TGDSAlerter; EventGroup: Integer);
begin
  inherited Create(True);

  FAlerter := Alerter;
  FConnection := TGDSConnection(FAlerter.Connection);
  FEventGroup := EventGroup;
  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_STATUS));
  FEventIDPtr := Marshal.AllocHGlobal(SizeOf(Integer));
  FGCHandle := AllocGCHandle(Self);
  FSignal := TEvent.Create(nil, False, False, '');

  Resume;
end;

destructor TGDSAlerterThread.Destroy;
begin
  CancelEvents;
  Marshal.FreeHGlobal(FEventIDPtr);
  Marshal.FreeHGlobal(FStatusVector);
  FreeGCHandle(FGCHandle);
  FSignal.Free;

  inherited;
end;

procedure TGDSAlerterThread.Terminate;
begin
  inherited;

  CancelEvents;
  FSignal.SetEvent;
end;

procedure TGDSAlerterThread.Check(Status: ISC_STATUS);
begin
  if Status > 0 then
    try
      FConnection.GDS.AlerterFatalError := True;
      FConnection.IBCError(FStatusVector, True, FConnection.Component);
    finally
      FConnection.GDS.AlerterFatalError := False;
    end;
end;

procedure TGDSAlerterThread.RegisterEvents;
var
  i, Index: integer;
  EventNames: array [0 .. IBC_MAX_EVENTS - 1] of AnsiString;
  EBP: array [0 .. IBC_MAX_EVENTS - 1] of IntPtr;
begin
  FEventBuffer := nil;
  FResultBuffer := nil;
  FEventBufferLen := 0;
  FFirstActivation := True;
  FEventCount := (FAlerter.EventNames.Count - (FEventGroup * IBC_MAX_EVENTS));
  if (FEventCount > IBC_MAX_EVENTS) then
    FEventCount := IBC_MAX_EVENTS;

  for i := 0 to IBC_MAX_EVENTS - 1 do
    EBP[i] := nil;
  try
    for i := 0 to FEventCount - 1 do begin
      Index := i + FEventGroup * IBC_MAX_EVENTS;
      EventNames[i] := AnsiString(FAlerter.EventNames[Index]);
      EBP[i] := Marshal.StringToHGlobalAnsi(EventNames[i]);
    end;

    FConnection.GDS.Busy;
    FEventBufferLen := FConnection.GDS.isc_event_block(FEventBuffer, FResultBuffer, FEventCount,
      EBP[0], EBP[1], EBP[2], EBP[3], EBP[4], EBP[5], EBP[6], EBP[7], EBP[8], EBP[9],
      EBP[10], EBP[11], EBP[12], EBP[13], EBP[14]);
    FConnection.GDS.Release;
  finally
  {$IFDEF CLR}
    for i := 0 to IBC_MAX_EVENTS - 1 do
      if EBP[i] <> nil then
        FreeString(EBP[i]);
  {$ENDIF}
  end;
end;

procedure TGDSAlerterThread.QueueEvents;
var
  Res: ISC_STATUS;
begin
  if FEventBuffer = nil then
    RegisterEvents;

  FConnection.GDS.Busy;
  Res := FConnection.GDS.isc_que_events(FStatusVector,
    FConnection.FDatabaseHandle, FEventIDPtr, FEventBufferLen,
    FEventBuffer, ISCCallbackPtr, FGCHandle);
  FConnection.GDS.Release;
  Check(Res);
end;

procedure TGDSAlerterThread.CancelEvents;
begin
  if FEventBuffer = nil then
    exit;

  FConnection.GDS.Busy;
  FConnection.GDS.isc_cancel_events(FStatusVector,
    FConnection.FDatabaseHandle, FEventIDPtr);
  FConnection.GDS.isc_free(FEventBuffer);
  FEventBuffer := nil;
  FConnection.GDS.isc_free(FResultBuffer);
  FResultBuffer := nil;
  FEventBufferLen := 0;
  FConnection.GDS.Release;
end;

procedure TGDSAlerterThread.CheckEvents;
var
  FiredNum: Integer;
  i: Integer;
{$IFDEF MSWINDOWS}
  Event: TAlertEvent;
{$ENDIF}
begin
  FConnection.GDS.Busy;
  FConnection.GDS.isc_event_counts(FStatusVector, FEventBufferLen, FEventBuffer, FResultBuffer);
  FConnection.GDS.Release;

  if not FFirstActivation then begin
    for i := 0 to FEventCount - 1 do begin
      FiredNum := Marshal.ReadInt32(FStatusVector, i * SizeOf(Long));
      if FiredNum > 0 then begin
      {$IFDEF MSWINDOWS}
        Event := TAlertEvent.Create;
        Event.Name := FAlerter.EventNames[(FEventGroup * IBC_MAX_EVENTS) + i];
        Event.Count := FiredNum;
        PostMessage(hIBDACWindow, WM_EVENTRECIEVED, NativeInt(FAlerter.GCHandle),
          NativeInt(AllocGCHandle(Event)));
      {$ELSE}
        FAlerter.DoOnEvent(FAlerter.EventNames[(FEventGroup * IBC_MAX_EVENTS) + i], FiredNum);
      {$ENDIF}
      end;
    end;
  end
  else
    FFirstActivation := False;
end;

procedure TGDSAlerterThread.UpdateResultBuffer(Length: SmallInt; Updated: IntPtr);
begin
  CopyBuffer(Updated, FResultBuffer, Length);
end;

procedure TGDSAlerterThread.Execute;
begin
  try
    QueueEvents;
    while not Terminated do begin
      FSignal.WaitFor($FFFFFFFF);
      if not Terminated then begin
        CheckEvents;
        QueueEvents;
      end;
    end;
  except
  end;
end;

procedure EventCallback(P: IntPtr; Length: SmallInt; Updated: IntPtr); {$IFNDEF CLR} cdecl; {$ENDIF}
var
  Thread: TGDSAlerterThread;
begin
  if (P <> nil) then begin
    Thread := TGDSAlerterThread(GetGCHandleTarget(P));
    Thread.UpdateResultBuffer(Length, Updated);
    Thread.FSignal.SetEvent;
  end;
end;

{$ENDIF}

{ TIBCBlob }

constructor TIBCBlob.Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
begin
  inherited Create;

  Init(DbHandle, TrHandle);
end;

constructor TIBCBlob.Create(Connection: TGDSConnection; Transaction: TGDSTransaction);
begin
  inherited Create;

  FConnection := Connection;
  FTransaction := Transaction;
  if (FConnection <> nil) and (FTransaction <> nil) then
    Init(FConnection.GetDatabaseHandle, FTransaction.GetTransactionHandle)
  else
    Init(nil, nil);
end;

procedure TIBCBlob.Init(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
begin
  FID := Marshal.AllocHGlobal(SizeOf(TISC_QUAD));
  Marshal.WriteInt64(FID, 0);
  FHandle := Marshal.AllocHGlobal(SizeOf(TISC_BLOB_HANDLE));
  Marshal.WriteIntPtr(FHandle, nil);
  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FDbHandle := Marshal.AllocHGlobal(SizeOf(TISC_DB_HANDLE));
  FTrHandle := Marshal.AllocHGlobal(SizeOf(TISC_TR_HANDLE));
  Marshal.WriteIntPtr(FDbHandle, DbHandle);
  Marshal.WriteIntPtr(FTrHandle, TrHandle);

///  PieceSize := 16384;
  FCharsetID := -1;
  FConversionCharsetID := -1;

  FCached := True;
  FParamsChanged := True;
end;

destructor TIBCBlob.Destroy;
begin
  FreeBlob;

  Marshal.FreeHGlobal(FID);
  Marshal.FreeHGlobal(FHandle);
  Marshal.FreeHGlobal(FStatusVector);
  Marshal.FreeHGlobal(FDbHandle);
  Marshal.FreeHGlobal(FTrHandle);

  inherited Destroy;
end;

procedure TIBCBlob.Check(Status: ISC_STATUS);
var
  SqlErrMsg, Msg: _string;
  ErrorNumber, ErrorCode: integer;
begin
  if Status > 0 then begin
    GDS.Busy;
    ErrorCode := GDS.isc_sqlcode(FStatusVector);
    GDS.Release;
    GDS.GetIBError(ErrorCode, False, ErrorNumber, FStatusVector, Msg, SqlErrMsg);
    raise EIBCError.Create(ErrorCode, ErrorNumber, Msg, SqlErrMsg);
  end;
end;

procedure TIBCBlob.CreateBPB;
const
  DEFAULT_BPB_SIZE = 128;

  procedure CheckBPBSize (NeededSize: integer; var BPBSize: integer);
  begin
    if BPBSize < NeededSize then begin
      BPBSize := ((NeededSize div DEFAULT_BPB_SIZE) + 1) * DEFAULT_BPB_SIZE;
      SetLength(FBPB, BPBSize);
    end;
  end;

var
  BPBSize: integer;
begin
  BPBSize := DEFAULT_BPB_SIZE; //Start size that must cover common user needs
  SetLength(FBPB, BPBSize);
  FBPBLength := 0;
  if not(FSubTypeChanged or (FCharsetID >= 0) or FConversionSubTypeChanged or (FConversionCharsetID >= 0) or FStreamed) then
    Exit;
  FBPBLength := 1;
  FBPB[0] := isc_bpb_version1;
  try
    CheckBPBSize(FBPBLength + 19, BPBSize);
    if FSubTypeChanged then begin
      FBPB[FBPBLength] := isc_bpb_source_type;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := FSubType;
      Inc(FBPBLength, 3);
    end;
    if FConversionSubTypeChanged then begin
      FBPB[FBPBLength] := isc_bpb_target_type;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := FConversionSubType;
      Inc(FBPBLength, 3);
    end;
    if FCharsetID >= 0 then begin
      FBPB[FBPBLength] := isc_bpb_source_interp;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := FCharsetID;
      Inc(FBPBLength, 3);
    end;
    if FConversionCharsetID >= 0 then begin
      FBPB[FBPBLength] := isc_bpb_target_interp;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := FConversionCharsetID;
      Inc(FBPBLength, 3);
    end;
    if FStreamed then begin
      FBPB[FBPBLength] := isc_bpb_type;
      FBPB[FBPBLength + 1] := 1;
      FBPB[FBPBLength + 2] := isc_bpb_type_stream;
      Inc(FBPBLength, 3);
    end;
  except
    SetLength(FBPB, 0);
    FBPBLength := 0;
    raise;
  end;
end;

procedure TIBCBlob.AllocBlob(Read: boolean = True);
begin
  if Marshal.ReadIntPtr(FHandle) = nil then begin
    CheckDatabase;
    CheckTransaction;

    if FParamsChanged then begin
      CreateBPB;
      FParamsChanged := False;
    end;

    GDS.Busy;
    try
      if Read then begin
        if Marshal.ReadInt64(FID) <> 0 then
          Check(GDS.isc_open_blob2(FStatusVector, FDbHandle, FTrHandle, FHandle, FID, FBPBLength, FBPB))
        else
          RaiseError(SCantReadEmptyBlobID)
      end
      else begin
        Marshal.WriteInt64(FID, 0);
        Check(GDS.isc_create_blob2(FStatusVector, FDbHandle, FTrHandle, FHandle, FID, FBPBLength, FBPB));
      end;
      FNativeHandle := True;
    finally
      GDS.Release;
    end;
  end;
  if Read and not FInfoReaded then
    GetBlobInfo; //We check if existing blob is streamed and determine its size
end;

procedure TIBCBlob.CloseBlob;
begin
  if Marshal.ReadIntPtr(FHandle) <> nil then begin
    GDS.Busy;
    try
      if FNativeHandle then
        Check(GDS.isc_close_blob(FStatusVector, FHandle));
      Marshal.WriteIntPtr(FHandle, nil);
      FInfoReaded := False;
      FNativeHandle := False;
    finally
      GDS.Release;
    end;
  end;
end;

procedure TIBCBlob.FreeBlob;
begin
  CloseBlob;
  Marshal.WriteInt64(FID, 0);
end;

procedure TIBCBlob.Disconnect;
begin
  FreeBlob;
end;

procedure TIBCBlob.CheckInfo;
begin
  if not FInfoReaded then
    AllocBlob;
end;

procedure TIBCBlob.GetBlobInfo;
var
  BlobItems: IntPtr;
  ItemsSize: integer;
  BlobInfo: IntPtr;
  InfoSize: integer;

  ItemOffset, ItemSize: integer;
  InfoItem: byte;
  Res: ISC_STATUS;

  function GetInfo(Info: IntPtr; OffSet, Size: integer): integer;
  begin
    case Size of
      1:  Result := Marshal.ReadByte(Info, OffSet);
      2:  Result := Marshal.ReadInt16(Info, OffSet);
      4:  Result := Marshal.ReadInt32(Info, OffSet);
    else
      Result := Marshal.ReadInt64(Info, OffSet);
    end;
  end;

begin
  InfoSize := 32 * 1; //SizeOf(Char) CLR fix
  BlobInfo := Marshal.AllocHGlobal(InfoSize);

  ItemsSize := 4 * 1; //SizeOf(Char) CLR fix
  BlobItems := Marshal.AllocHGlobal(ItemsSize);
  try
    Marshal.WriteByte(BlobItems, 0, isc_info_blob_num_segments);
    Marshal.WriteByte(BlobItems, 1, isc_info_blob_max_segment);
    Marshal.WriteByte(BlobItems, 2, isc_info_blob_total_length);
    Marshal.WriteByte(BlobItems, 3, isc_info_blob_type);
    GDS.Busy;
    Res := GDS.isc_blob_info(FStatusVector, FHandle, ItemsSize, BlobItems, InfoSize, BlobInfo);
    GDS.Release;
    Check(Res);

    ItemOffset := 0;
    while (ItemOffset < InfoSize) and (Marshal.ReadByte(BlobInfo, ItemOffset) <> isc_info_end) do begin
      InfoItem := Marshal.ReadByte(BlobInfo, ItemOffset);
      ItemSize := Marshal.ReadInt16(BlobInfo, ItemOffset + 1);
      case InfoItem of
        isc_info_blob_num_segments:
          FNumSegments := GetInfo(BlobInfo, ItemOffset + 3, ItemSize);
        isc_info_blob_max_segment:
          FMaxSegmentSize := GetInfo(BlobInfo, ItemOffset + 3, ItemSize);
        isc_info_blob_total_length:
          FBlobLength := GetInfo(BlobInfo, ItemOffset + 3, ItemSize);
        isc_info_blob_type: begin
          FStreamed := GetInfo(BlobInfo, ItemOffset + 3, ItemSize) = 1; { 0 = segmented, 1 = streamed }
        end;
      end;
      ItemOffset := ItemOffset + 3 + ItemSize;
    end;
  finally
    Marshal.FreeHGlobal(BlobInfo);
    Marshal.FreeHGlobal(BlobItems);
  end;
  FInfoReaded := True;
end;

procedure TIBCBlob.CheckValue;
begin
  if FNeedReadBlob then
    ReadBlob;
end;

function TIBCBlob.GetSize: Cardinal;
begin
  if FNeedReadBlob and FIsUnicode then
    ReadBlob;

  if FNeedReadBlob then
    Result := LengthBlob
  else
    Result := inherited GetSize;
end;

function TIBCBlob.GetSizeAnsi: cardinal;
begin
  Assert(FIsUnicode);
  if FNeedReadBlob then
    ReadBlob;

  Result := inherited GetSizeAnsi;
end;

procedure TIBCBlob.CheckAlloc;
begin
  if Marshal.ReadIntPtr(FHandle) = nil then
    RaiseError(SBlobNotAllocatted);
end;

procedure TIBCBlob.CheckDatabase;
begin
  if FDbHandle = nil then
    RaiseError(SConnectionNotDefined);
end;

procedure TIBCBlob.CheckTransaction;
begin
  if FTrHandle = nil then
    RaiseError(STransactionNotAssigned);
end;

function TIBCBlob.IsInit: boolean;
begin
  //CheckAlloc;
  Result := Marshal.ReadInt64(FID) <> 0;
end;

function TIBCBlob.LengthBlob: longint;
begin
  CheckDatabase;
  CheckTransaction;
  if IsInit then begin
    CheckInfo;
    Result := FBlobLength;
    // reset FNeedReadBlob to avoid multiple LengthBlob calls on empty LOB
    if Result = 0 then
      FNeedReadBlob := False;
  end
  else begin
    Result := 0;
    FNeedReadBlob := False;
  end;
end;

function TIBCBlob.CharSize: Byte;
begin
  Result := 1;
  if FISUnicode then Result := 2;
end;

procedure TIBCBlob.ReadBlob;
var
  WsPiece, Piece: PPieceHeader;
  PActualReadLen: IntPtr;
  PActualPosition: IntPtr;
  Res: integer;
  BlobLength: Integer;
  BuffPtr: IntPtr;
  UsedCount: Integer;
  MaxPieceSize: Integer;
  Len: integer;
begin
  CheckDatabase;
  CheckTransaction;
  FData.Clear;
  if IsInit then begin
    PActualPosition := Marshal.AllocHGlobal(SizeOf(ISC_LONG));
    PActualReadLen := Marshal.AllocHGlobal(SizeOf(Word));
    try
      if GetHandle = nil then
        AllocBlob(True)
      else
        if FStreamed then begin
          GDS.Busy;
          Res := GDS.isc_seek_blob(FStatusVector, FHandle, 0, 0, PActualPosition);
          GDS.Release;
          Check(Res);
        end;

      BlobLength := LengthBlob;
      if (BlobLength > 0) then begin
        GDS.Busy;
        try
          UsedCount := 0;
          if (FMaxSegmentSize < PieceSize) and (FMaxSegmentSize > 0) then
            MaxPieceSize := FMaxSegmentSize
          else
            MaxPieceSize := PieceSize;

          while UsedCount < BlobLength do begin
            if FIsUnicode then
              AllocPiece(Piece, BlobLength)
            else
              if (BlobLength - UsedCount) > MaxPieceSize then
                AllocPiece(Piece, MaxPieceSize)
              else
                AllocPiece(Piece, BlobLength - UsedCount);

            try
              while Piece.Used < Piece.Size do begin
                BuffPtr := PtrOffset(Piece, Sizeof(TPieceHeader) + Piece.Used);
                Res := GDS.isc_get_segment(FStatusVector, FHandle, PActualReadLen, Min(Piece.Size - Piece.Used, $FFFF{MaxWord}), BuffPtr);
                if not ((Res = 0) or (Marshal.ReadInt32(FStatusVector, 1 * SizeOf(Long)) = isc_segment)) then
                  Check(Res);
                Piece.Used := Piece.Used + word(Marshal.ReadInt16(PActualReadLen));
              end;
              UsedCount := UsedCount + Piece.Used;
            except
              FreePiece(Piece);
              raise;
            end;

            AppendPiece(Piece);
          end;
        finally
          GDS.Release;
        end;

        if FIsUnicode then begin //BLOB unicode settings is separate from connection unicode settings
          //Convert UTF8 to UTF16
          Len := UsedCount * SizeOf(WideChar); //if all utf8 chars lesser than 128
          AllocPiece(WsPiece, Len);
          try
            Len := Utf8ToWs(PtrOffset(Piece, Sizeof(TPieceHeader)), UsedCount, PtrOffset(WsPiece, Sizeof(TPieceHeader)), Len, False);
            WsPiece.Used := Len;
            FreePiece(Piece);
            CompressPiece(WsPiece);
          except
            FreePiece(WsPiece);
            raise;
          end;
          AppendPiece(WsPiece);
        end;

        if not FStreamed then
          FCached := True;
      end;
    finally
      Marshal.FreeHGlobal(PActualReadLen);
      Marshal.FreeHGlobal(PActualPosition);
      if FCached or not FStreamed then
        CloseBlob;
    end;
  end;
  FNeedReadBlob := False;
end;

procedure TIBCBlob.WriteBlob;
var
  Piece: PPieceHeader;
  BufLen: Integer;
  WriteSize: word;
  Offset, Chars: integer;
  utfPiece: IntPtr;
begin
  CheckDatabase;
  CheckTransaction;

  CloseBlob;
  AllocBlob(False);
  try
    utfPiece := nil;
    if FIsUnicode then
      utfPiece := Marshal.AllocHGlobal(PieceSize + 1);

    Piece := FirstPiece;
    try
      GDS.Busy;
      try
        while IntPtr(Piece) <> nil do begin
          BufLen := Piece.Used;
          Offset := 0;
          WriteSize := 0;
          while BufLen > 0 do begin
            if FIsUnicode then begin
              if BufLen = 1 then
                break;
              if BufLen div 2 > PieceSize div 3 then
                Chars := PieceSize div 3
              else
                Chars := BufLen div 2;

            {$IFNDEF CLR}
            {$IFNDEF KYLIX}
              WriteSize := CLRClasses.UnicodeToUtf8(PAChar(utfPiece), PieceSize + 1,
                PtrOffset(Piece, Sizeof(TPieceHeader) + Offset), Chars);
              if WriteSize > 0 then
                Dec(WriteSize); // #0

              Check(GDS.isc_put_segment(FStatusVector, FHandle, WriteSize, utfPiece));
            {$ENDIF}
            {$ENDIF}

              WriteSize := Chars * 2;
            end
            else begin
              if BufLen > PieceSize then
                WriteSize := PieceSize
              else
                WriteSize := BufLen;

              Check(GDS.isc_put_segment(FStatusVector, FHandle, WriteSize,
                PtrOffset(Piece, Sizeof(TPieceHeader) + Offset)));
            end;

            Dec(BufLen, WriteSize);
            Inc(Offset, WriteSize);
          end;
          Piece := Piece.Next;
        end;
      finally
        GDS.Release;
      end;
      CheckInfo; //To achieve information about real blob state on server side
                 //Current blobID will be invalid after insert or update operation
      FBlobLength := Size; //temporary blob size fix (info returns first segment length)
    finally
      if FIsUnicode then
        Marshal.FreeHGlobal(utfPiece);
    end;
  finally
    CloseBlob;
  end;
end;

function TIBCBlob.Read(Position, Count: cardinal; Dest: IntPtr): cardinal;
var
  PActualReadLen, PActualPosition: IntPtr;
  ReadOffset: cardinal;
  ReadSize: word;
  Res: integer;

begin
  Result := 0;
  if not Cached then begin
    CheckAlloc;
    CheckDatabase;
    CheckTransaction;
    if IsInit then begin
      if not FStreamed then begin
        ReadBlob;
        FCached := True;
        Result := Read(Position, Count, Dest);
        Exit;
      end;

      PActualReadLen := Marshal.AllocHGlobal(SizeOf(Word));
      PActualPosition := Marshal.AllocHGlobal(SizeOf(ISC_LONG));
      try
        GDS.Busy;
        Res := GDS.isc_seek_blob(FStatusVector, FHandle, 0, Position, PActualPosition);
        GDS.Release;
        Check(Res);

        if Cardinal(Marshal.ReadInt32(PActualPosition)) <> Position then
          RaiseError(SCantSetReadPosition);
        GDS.Busy;
        try
          ReadOffset := 0;
          while Cardinal(Count - ReadOffset) > 0 do begin
            if Cardinal(Count - ReadOffset) > Cardinal(PieceSize) then
              ReadSize := PieceSize
            else
              ReadSize := Cardinal(Count - ReadOffset);
            Res := GDS.isc_get_segment(FStatusVector, FHandle, PActualReadLen, ReadSize, PtrOffset(Dest, ReadOffset));
            ReadOffset := ReadOffset + Word(Marshal.ReadInt16(PActualReadLen));
            if not ((Res = 0) or (Marshal.ReadInt32(FStatusVector, 1 * SizeOf(Long)) = isc_segment)) then
              Check(Res);
          end;
          Result := ReadOffset;
        finally
          GDS.Release;
        end;
      {Unicode support BLOB Streamed read - no UTF16 conversion}
      finally
        Marshal.FreeHGlobal(PActualReadLen);
        Marshal.FreeHGlobal(PActualPosition);
      end;
    end;
  end
  else
    Result := inherited Read(Position, Count, Dest);
end;

procedure TIBCBlob.Write(Position, Count: cardinal; Source: IntPtr);
begin
  Cached := True;

  inherited Write(Position, Count, Source);
end;

procedure TIBCBlob.Clear;
begin
  FNeedReadBlob := False;

  inherited Clear;
end;

procedure TIBCBlob.Truncate(NewSize: cardinal);
begin
  if NewSize = 0 then
    FNeedReadBlob := False
  else
    CheckValue;

  inherited Truncate(NewSize);
end;

function TIBCBlob.GetIDPtr: PISC_QUAD;
begin
  Result := FID;
end;

function TIBCBlob.GetID: TISC_QUAD;
begin
  Result := Marshal.ReadInt64(FID);
end;

procedure TIBCBlob.SetID(Value: TISC_QUAD);
begin
  FreeBlob;
  Marshal.WriteInt64(FID, Value);
end;

function TIBCBlob.GetHandle: TISC_BLOB_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FHandle);
end;

procedure TIBCBlob.SetHandle(Value: TISC_BLOB_HANDLE);
begin
  FreeBlob;
  Marshal.WriteIntPtr(FHandle, Value);
  if Marshal.ReadIntPtr(FHandle) <> nil then
    FNativeHandle := False;
end;

function TIBCBlob.GetDbHandle: TISC_DB_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FDbHandle);
end;

function TIBCBlob.GetTrHandle: TISC_TR_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FTrHandle);
end;

procedure TIBCBlob.SetDbHandle(Value: TISC_DB_HANDLE);
begin
  Marshal.WriteIntPtr(FDbHandle, Value);
end;

procedure TIBCBlob.SetTrHandle(Value: TISC_TR_HANDLE);
begin
  Marshal.WriteIntPtr(FTrHandle, Value);
end;

procedure TIBCBlob.SetConnection(Value: TGDSConnection);
begin
  if Value <> nil then
    SetDbHandle(Value.GetDatabaseHandle);
  FConnection := Value;
end;

procedure TIBCBlob.SetTransaction(Value: TGDSTransaction);
begin
  if Value <> nil then
    SetTrHandle(Value.GetTransactionHandle);
  FTransaction := Value;
end;

procedure TIBCBlob.SetSubType(Value: integer);
begin
  FSubTypeChanged := True;
  FParamsChanged := True;
  FSubType := Value;
end;

procedure TIBCBlob.SetCharsetID(Value: integer);
begin
  FParamsChanged := True;
  FCharsetID := Value;
end;

procedure TIBCBlob.SetConversionSubType(Value: integer);
begin
  FConversionSubTypeChanged := True;
  FParamsChanged := True;
  FConversionSubType := Value;
end;

procedure TIBCBlob.SetConversionCharsetID(Value: integer);
begin
  FParamsChanged := True;
  FConversionCharsetID := Value;
end;

procedure TIBCBlob.SetStreamed(Value: boolean);
begin
  if FStreamed <> Value then begin
    if not Value and (GetHandle <> nil) then
      RaiseError(SCannotChangeStreamed);
    FStreamed := Value;
    FParamsChanged := True;
    FInfoReaded := False;
  end;
end;

procedure TIBCBlob.SetCached(const Value: boolean);
begin
  if FCached <> Value then begin
    if not Value then begin
      if (IntPtr(FirstPiece) <> nil) then   //Blob is already readed
        RaiseError(SCannotDisableBlobCache)
    end
    else
      ReadBlob;
    FCached := Value;
  end;
end;

function TIBCBlob.GetGDS: TGDS;
begin
  if FConnection <> nil then
    Result := FConnection.FGDS
  else
    Result := GDSList.GetGDS('');
end;
//******************************************************************************

function Reverse2(Value: word): Word;
begin
  Result := Swap(Value);
//  Result := Word((byte(Value) shl 8) or byte(Value shr 8));
end;

function Reverse4(Value: cardinal): cardinal;
begin
   Result := cardinal((byte(Value) shl 24) or (byte(Value shr 8) shl 16)
           or (byte(Value shr 16) shl 8) or byte(Value shr 24));
end;

function XSQLDA_LENGTH(n: Long; XSQLVARType: TXSQLVARType): Long;
begin
  if XSQLVARType = vtGDS7 then
    Result := SizeOfXSQLDA + ((n - 1)  * SizeOfXSQLVAR)
  else
    Result := SizeOfXSQLDA_V1 + ((n - 1) * SizeOfXSQLVAR_V1);
end;

{Date conversions}

const
  IBDateDelta = 15018;

procedure DateTimeToSQLTimeStamp(DateTime: TDateTime; Buf: IntPtr);
var
  Date, Time: longint;
begin
  Date := Trunc(DateTime) + IBDateDelta;
  Time := Round(Abs(Frac(DateTime)) * (MSecsPerDay * 10));
  Marshal.WriteInt32(Buf, Date);
  Marshal.WriteInt32(Buf, SizeOf(LongInt), Time);
end;

procedure DateTimeToSQLDate(DateTime: TDateTime; Buf: IntPtr);
var
  Date: longint;
begin
  Date := Trunc(DateTime) + IBDateDelta;
  Marshal.WriteInt32(Buf, Date);
end;

procedure DateTimeToSQLTime(DateTime: TDateTime; Buf: IntPtr);
var
  Time: longint;
begin
  Time := Round(Abs(Frac(DateTime)) * (MSecsPerDay * 10));
  Marshal.WriteInt32(Buf, Time);
end;

procedure SQLTimeStampToDateTime(ib_date: IntPtr; Buf: IntPtr);
var
  Date, Time, DateTime: TDateTime;
begin
  Date := Marshal.ReadInt32(ib_date) - IBDateDelta;
  Time := Marshal.ReadInt32(ib_date, SizeOf(LongInt)) / (MSecsPerDay * 10);
  if Date < 0 then
    DateTime := Date - Time
  else
    DateTime := Date + Time;
  Marshal.WriteInt64(Buf, BitConverter.DoubleToInt64Bits(DateTime));
end;

procedure SQLDateToDateTime(ib_date: IntPtr; Buf: IntPtr);
var
  Date: TDateTime;
begin
  Date := Marshal.ReadInt32(ib_date) - IBDateDelta;
  Marshal.WriteInt64(Buf, BitConverter.DoubleToInt64Bits(Date));
end;

procedure SQLTimeToDateTime(ib_time: IntPtr; Buf: IntPtr);
var
  Time: TDateTime;
begin
  Time := Marshal.ReadInt32(ib_time) / (MSecsPerDay * 10);
  Marshal.WriteInt64(Buf, BitConverter.DoubleToInt64Bits(Time));
end;

{SQL parsing}

{function IsStoredProcCall(SQL: _string): boolean;
var
  Parser: TIBCParser;
  PrevCode, Code: integer;
  StLex: _string;
begin
  Result := False;
  PrevCode := 0;
  Parser := TIBCParser.Create(SQL);
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  try
    repeat
      Code := Parser.GetNext(StLex);
      if (PrevCode = lxEXECUTE) and (Code = lxPROCEDURE) then
        Result := True;
      PrevCode := Code;
    until (Code = lcEND) or Result;
  finally
    Parser.Free;
  end;
end;}

{procedure SetStrTerminator(Buf: IntPtr; Size, CharLen: integer);
begin
  case CharLen of
    0, 1:
      Marshal.WriteByte(Buf, Size, 0);
    2:
      Marshal.WriteInt16(Buf, Size, 0);
    3: begin
      Marshal.WriteInt16(Buf, Size, 0);
      Marshal.WriteByte(Buf, Size + 2, 0);
    end;
  end;
end;}

{$IFDEF VER6P}
function DBDecimalToBcd(Value: int64; Scale: integer): TBcd;
var
  StrVal: string;
  StrLen: integer;
  Negative: boolean;
  bcdstr: string;
  i: integer;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  if Value < 0 then begin
    Value := -Value;
    Negative := True;
  end
  else
    Negative := False;

  StrVal := IntToStr(Value);
  StrLen := Length(StrVal);
  if (StrLen <= Scale) and (Value <> 0) then begin // Value <> 0 - in some cases IB
    SetLength(bcdstr, Scale + 2);                  // returs Value: 0 Scale: 1
  {$IFDEF CLR}
    for i := 1 to Scale + 2 do
      if i = 2 then
        bcdstr[i] := DecimalSeparator[1]
      else
      if i > Scale - StrLen + 2 then
        bcdstr[i] := StrVal[i - (Scale - StrLen + 2)]
      else
        bcdstr[i] := '0';
  {$ELSE}
    for i := 1 to Scale - StrLen + 2 do
      bcdstr[i] := '0';
    bcdstr[2] := DecimalSeparator;
    Move(StrVal[1], bcdstr[Scale - StrLen + 3], StrLen * SizeOf(Char));
  {$ENDIF}
  end
  else
    bcdstr := StrVal;

  if Negative then
    bcdstr := '-' + bcdstr;

  Result := StrToBcd(bcdstr);

  if (StrLen >= Scale) and (Scale > 0) then begin
  {$IFDEF CLR}
    b := TBcd.ToBytes(Result);
    b[1] := Result.SignSpecialPlaces or Scale;
    Result := TBcd.FromBytes(b);
  {$ELSE}
    Result.SignSpecialPlaces := Result.SignSpecialPlaces or Scale;
  {$ENDIF}
  end;
end;
{$ENDIF}

function GetDBNamePos(const Database: string): integer;
begin
  Result := Pos('@', Database);
  if Result > 0 then begin
    Inc(Result);
    Exit;
  end;
  Result := Pos('\\', Database);
  if Result = 1 then begin
    Result := Pos('\', Copy(Database, 3, MaxInt)) + 3;
    Exit;
  end;
  Result := Pos(':', Database);
  if (Result > 0) and (Length(Database) > (Result + 1)) and (Database[Result + 1] <> '\') then
    Inc(Result)
  else
    Result := -1;
end;

function GetFullDatabaseName(const Host: string; Protocol: _TIBCProtocol; const FileName: string): string;
begin
  Result := '';
  if Host <> '' then
    case Protocol of
      _TCP:
        Result := Host + ':';
      _NetBEUI:
        Result := '\\' + Host + '\';
      _SPX:
        Result := Host + '@';
    end;

  Result := Result + FileName;
end;

procedure ParseDatabaseName(const Database: string; var Host: string; var Protocol: _TIBCProtocol;
  var FileName: string);
var
  NamePos: integer;
begin
  NamePos := GetDBNamePos(Database);
  if NamePos > 0 then begin
    case Database[NamePos - 1] of
      '@': begin
        Protocol := _SPX;
        Host := Copy(Database, 1, NamePos - 2);
      end;
      ':': begin
        Protocol := _TCP;
        Host := Copy(Database, 1, NamePos - 2);
      end;
      '\': begin
        Protocol := _NetBEUI;
        Host := Copy(Database, 3, NamePos - 2);
      end;
    end;
    FileName := Copy(Database, NamePos, MaxInt);
  end
  else begin
    Host := '';
    Protocol := _TCP;
    FileName := Database;
  end;
end;

initialization
  IBCSQLInfo := TIBCSQLInfo.Create(nil);

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  //AllocIBDACWnd;
{$ENDIF}

{$IFDEF CLR}
  ISCCallbackPtr := Marshal.AllocHGlobal(Length(ISCCallbackCode));
  Marshal.Copy(TBytes(ISCCallbackCode), 0, ISCCallbackPtr, Length(ISCCallbackCode));
  CallbackRecPtr := Marshal.AllocHGlobal(20);
  try
    ISCCallbackRec.Ptr := EventCallback;
    HISCCallback := GCHandle.Alloc(@ISCCallbackRec.Ptr, GCHandleType.Normal);
    Marshal.StructureToPtr(ISCCallbackRec, CallbackRecPtr, False);
    Marshal.WriteIntPtr(ISCCallbackPtr, 17, Marshal.ReadIntPtr(CallbackRecPtr));
  finally
    Marshal.FreeHGlobal(CallbackRecPtr);
  end;
{$ELSE}
  ISCCallbackPtr := @EventCallback;
{$ENDIF}
{$ENDIF}

finalization
  IBCSQLInfo.Free;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  if hIBDACWindow <> 0 then
    DestroyWindow(hIBDACWindow);
{$ENDIF}

{$IFDEF CLR}
  HISCCallback.Free;
  Marshal.FreeHGlobal(ISCCallbackPtr);
{$ENDIF}
{$ENDIF}

end.

