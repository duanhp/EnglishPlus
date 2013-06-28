
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  Oracle Classes
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}
unit OraClassesUni;
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
{$IFDEF CLR}
  Variants, WinUtils,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF VER6P}
  FMTBcd,
{$IFNDEF FPC}
  SqlTimSt,
{$ENDIF}
{$ENDIF}
  SysUtils, Classes, SyncObjs, MemData, CRAccess, MemUtils, CRParser,
{$IFNDEF UNIDACPRO}
  OraCall, OraError;
{$ELSE}
  OraCallUni, OraErrorUni;
{$ENDIF}

const
  dtRowId         = 100;
  dtOraBlob       = 102;
  dtOraClob       = 103;
  dtBFILE         = 104;
  dtCFILE         = 105;
  dtLabel         = 106;  // MLSLABEL
  dtFixedChar     = 107;
  dtUndefined     = 108;
  dtTimeStamp     = 109;
  dtTimeStampTZ   = 110;
  dtTimeStampLTZ  = 111;
  dtIntervalYM    = 112;
  dtIntervalDS    = 113;
  dtURowId        = 114;
  dtNumber        = 115;
  dtXML           = 116;
  dtFixedWideChar = 117;
  dtBFloat        = 118;
  dtBDouble       = 119;
  dtNString       = 120;
  dtNWideString   = 121;
  dtNClob         = 122;
  dtWideOraClob   = 123;

// obsolete
  dtBLOBLocator = dtOraBlob;
  dtCLOBLocator = dtOraClob;

// Props
  prOCIBase           = 1000;

  prNonBlocking       = prOCIBase + 1;  // bool
  prThreadSafety      = prOCIBase + 2;  // bool
  prAutoClose         = prOCIBase + 3;  // bool
  prErrorOffset       = prOCIBase + 4;  // word
  prDateFormat        = prOCIBase + 6;  // string
  prDeferredLobRead   = prOCIBase + 7;  // bool
  prConnectMode       = prOCIBase + 8;  // enum
  prCharLength        = prOCIBase + 9;  // word
  prCacheLobs         = prOCIBase + 10; // bool
  prEnableIntegers    = prOCIBase + 11; // bool
  prInternalName      = prOCIBase + 12; // string
  prScrollableCursor  = prOCIBase + 13; // bool
  prStoreRowId        = prOCIBase + 14; // bool
  prCharset           = prOCIBase + 15; // word
  prDateLanguage      = prOCIBase + 16; // string
  prTimeStampFormat   = prOCIBase + 17; // string
  prTimeStampTZFormat = prOCIBase + 18; // string
  prRawAsString       = prOCIBase + 19; // bool
  prNumberAsString    = prOCIBase + 20; // bool
  prNumericCharacters = prOCIBase + 21; // string
  prEnableNumbers     = prOCIBase + 22; // bool
  prUseUnicode        = prOCIBase + 23; // bool
  prIntegerPrecision  = prOCIBase + 24; // word;
  prFloatPrecision    = prOCIBase + 25; // word;
  prTemporaryLobUpdate= prOCIBase + 26; // bool
  prDisconnectMode    = prOCIBase + 27; // bool
  prInactiveTimeout   = prOCIBase + 28; // integer
  prResumeTimeout     = prOCIBase + 29; // integer
  prTransactionName   = prOCIBase + 30; // string
  prConnectionTimeOut = prOCIBase + 31; // integer
  prHasObjectFields   = prOCIBase + 32; // bool
  prStatementCache    = prOCIBase + 33; // bool
  prStatementCacheSize= prOCIBase + 34; // integer
  prEnabled           = prOCIBase + 35; // bool
  prTimeout           = prOCIBase + 36; // integer
  prPersistent        = prOCIBase + 37; // bool
  prOperations        = prOCIBase + 38; // set
  prPrefetchRows      = prOCIBase + 39; // integer
  prTransactionResume = prOCIBase + 40; // bool
  prRollbackSegment   = prOCIBase + 41; // string
  prHomeName          = prOCIBase + 42; // string
  prDirect            = prOCIBase + 43; // bool
  prClientIdentifier  = prOCIBase + 44; // string
  prOptimizerMode     = prOCIBase + 45; // TOptimizerMode
  prUseOCI7           = prOCIBase + 46; // bool
  prSchema            = prOCIBase + 47; // string
{$IFNDEF FPC}
  prEnableSQLTimeStamp = prOCIBase + 48; // bool
{$ELSE}
  prTimeStampAsString  = prOCIBase + 48; // bool
{$ENDIF}
  prIntervalAsString  = prOCIBase + 49; // bool
  prSmallintPrecision = prOCIBase + 50; // integer
  prLargeintPrecision = prOCIBase + 51; // integer
  prBCDPrecision      = prOCIBase + 52; // integer
{$IFDEF VER6P}
{$IFNDEF FPC}
  prFmtBCDPrecision   = prOCIBase + 53; // integer
{$ENDIF}
{$ENDIF}
  prCheckParamHasDefault = prOCIBase + 54; // bool
  prUseResultParams      = prOCIBase + 55; // bool
  prUseDefaultDataTypes  = prOCIBase + 56; // bool
  prEnableLargeint       = prOCIBase + 57; // bool
  prUnicodeEnvironment   = prOCIBase + 58; // bool
  prDirectPath           = prOCIBase + 59; // bool
  prEnableWideOraClob    = prOCIBase + 60; // bool
  prAlerterTimeout       = prOCIBase + 61; // bool
  prAlerterInterval      = prOCIBase + 62; // bool
  prAlerterEventType     = prOCIBase + 63; // bool
  prAlerterSelfEvents    = prOCIBase + 64; // bool
  prUnicodeAsNational    = prOCIBase + 65; // bool

  RowIdSize = 18;

  MaxBlobSize: longint = 2147483647;

  MaxTransactionIdLength = 64; // Maximum length for TransactionId and BranchQualifier

type
  TOraCursor = class;
  TOraLob = class;
  TOraFile = class;
  TOraTimeStamp = class;
  TOraInterval = class;
  TOraNumber = class;
  TOraParamDesc = class;
  TOCIConnection = class;
  TOCICommand = class;
{$IFNDEF LITE}
  TOCITransaction = class;
{$ENDIF}

{ OraAccess level }

  TTransactionMode = (tmReadOnly, tmReadWrite, tmReadCommitted, tmSerializable);
  TConnectMode = (cmNormal, cmSysOper, cmSysDBA, cmSysASM);

{ TOraParamDesc }

  TOraParamDesc = class (TParamDesc)
  private
    FValue: IntPtr;
    FValueSize: integer;
    FNeedTruncateString: boolean;
    FActualLengthPtr: IntPtr;
    FDefIndicator: IntPtr;
    FIndicator: IntPtr;
    FTable: boolean;
    FLength: integer;  // Table Length
    FHandle: IntPtr;
    FBindBufferSize: integer;
    FBlobPiece: integer;  // number of piece
    FLen: integer;
    FTableIndicator: boolean;
    FHasDefault: boolean;
    FBufferAllocated: boolean;
    FIsResult: boolean;
    FOwner: TOCICommand;
  {$IFDEF LITE}
    FOriginalDataType: word;
  {$ENDIF}

    function GetActualLength: integer;
    procedure SetActualLength(Value: integer);
    property ActualLength: integer read GetActualLength write SetActualLength;

  protected
    procedure AllocBuffer;
    procedure FreeBuffer;

    procedure CheckRange(Index: integer);
    procedure ValidateParamValue;

    property Name;
    property DataType;
    property SubDataType;
    property ParamType;
    property Size;

    procedure ClearBindData;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetDataType(Value: word); override;
    procedure SetSize(Value: integer); override;
    procedure SetNeedTruncateString(Value: boolean);
    procedure SetTable(Value: boolean);
    procedure SetLength(Value: integer);
    procedure SetHasDefault(Value: boolean);
    procedure SetIsResult(Value: boolean);

  // TEMP for describe
    function GetTable: boolean;
    function GetLength: integer;
    function GetHasDefault: boolean;
    function GetIsResult: boolean;
  {$IFDEF LITE}
    function GetOriginalDataType: word;
  {$ENDIF}

    function GetIndicator(Index: integer): smallint;
    procedure SetIndicator(Index: integer; Value: smallint);

    function ValuePtr: IntPtr;
    function GetValueSize: integer;
    procedure SetValuePtr(Buf: IntPtr);
    function IndicatorPtr: IntPtr;
    procedure SyncIndicator(Connection: TOCIConnection);

    function GetItemAsDateTime(Index: integer): TDateTime;
    procedure SetItemAsDateTime(Index: integer; Value: TDateTime);
    function GetItemAsFloat(Index: integer): double;
    procedure SetItemAsFloat(Index: integer; Value: double);
    function GetItemAsInteger(Index: integer): integer;
    procedure SetItemAsInteger(Index: integer; Value: integer);
    function GetItemAsCurrency(Index: integer): currency;
    procedure SetItemAsCurrency(Index: integer; Value: currency);
  {$IFDEF VER6P}
    function GetItemAsLargeInt(Index: integer): Int64;
    procedure SetItemAsLargeInt(Index: integer; Value: Int64);
  {$IFNDEF FPC}
    procedure GetItemAsBcd(Index: integer; var Value: TBCD);
    procedure SetItemAsBcd(Index: integer; const Value: TBCD);
    procedure GetItemAsSQLTimeStamp(Index: integer; var Value: TSQLTimeStamp);
    procedure SetItemAsSQLTimeStamp(Index: integer; const Value: TSQLTimeStamp);
  {$ENDIF}
  {$ENDIF}
    function GetItemAsAnsiString(Index: integer): AnsiString;
    procedure SetItemAsAnsiString(Index: integer; const Value: AnsiString);
    function GetItemAsWideString(Index: integer): WideString;
    procedure SetItemAsWideString(Index: integer; const Value: WideString);
    function GetItemAsBoolean(Index: integer): boolean;
    procedure SetItemAsBoolean(Index: integer; Value: boolean);
    procedure SetItemAsObject(Index: integer; Value: TSharedObject);
    function GetItemAsObject(Index: integer): TSharedObject;

    function GetItemAsVariant(Index: integer): variant;
    procedure SetItemAsVariant(Index: integer; const Value: variant);

    function GetValue: variant; override;
    procedure SetValue(const Value: variant); override;

    function GetAsBlobRef: TBlob;
    function GetAsCursor: TOraCursor;
    function GetAsOraBlob: TOraLob;
    function GetAsBFile: TOraFile;
    function GetAsTimeStamp: TOraTimeStamp;
    function GetAsInterval: TOraInterval;
    function GetAsNumber: TOraNumber;

    function GetObject: TSharedObject; override;
    procedure SetObject(Value: TSharedObject); override;

    function GetNull: boolean; override;
    procedure SetNull(const Value: boolean); override;
    function GetItemNull(Index: integer): boolean;
    procedure SetItemNull(Index: integer; Value: boolean);
  end;

{ TOCIConnection }

  TRunMethod = procedure of object;
  TEndMethod = procedure(E: Exception) of object;

  TMethodDesc = class
  public
    RunMethod     : TRunMethod;
    EndMethod     : TEndMethod;
  {$IFDEF MSWINDOWS}
    hWindow   :HWND;
  {$ENDIF}
  end;

{$IFDEF LINUX}
  THandle = integer;
{$ENDIF}

  TNlsParamType = (nlsDateLanguage, nlsDateFormat, nlsNumericCharacters, nlsTimeStampFormat,
    nlsTimeStampTZFormat);

  TNlsSessionParam = record
    Name: _string;
    Value: _string;
    IsUserDefined: boolean;
  end;

  TFailoverCallback = procedure (FailoverState: cardinal; FailoverType: cardinal;
    var Retry: boolean) of object;
  TInfoMessageCallback = procedure(Error: EOraError) of object;

  TConnectionType = (ctDefault, ctOCIPooled{$IFDEF MSWINDOWS}{$IFNDEF LITE}, ctMTSPooled{$ENDIF}{$ENDIF});
  TOptimizerMode = (omDefault, omFirstRows1000, omFirstRows100, omFirstRows10, omFirstRows1, omFirstRows, omAllRows, omChoose, omRule);

  TOCIConnection = class (TCRConnection)
  private
    FThreadSafety: boolean;
    FMaxStringSize: word;
    FOCICallStyle: TOCICallStyle;
    FOCICallStyleCommand: TOCICallStyle;
    FLastError: integer;
    FConnectMode: TConnectMode;
    FEnableIntegers: boolean;
    FEnableLargeint: boolean;
    FEnableNumbers: boolean;
    FEnableWideOraClob: boolean;
    FInternalName: _string;
    FCommand: TOCICommand;
    FOracleVersionSt: _string;
    FOracleVersionFull: _string;
    FOracleVersion: word;
    FProxyConnection : TOCIConnection;
    FDisconnectMode: boolean;
    FConnectionTimeout: integer;
    FOCIPoolName: _string;
    FStatementCache: boolean;
    FStatementCacheSize: integer;
    FHomeName: string;
    FDirect: boolean;
    FUnicodeEnv: boolean;
    FConnectionType: TConnectionType;
  {$IFNDEF LITE}
    FClientIdentifier: _string;
    FOptimizerMode: TOptimizerMode;
    FSchema: _string;
    FCachedSchema: _string;
  {$ENDIF}
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    FEnableSQLTimeStamp: boolean;
  {$ELSE}
    FTimeStampAsString: boolean;
  {$ENDIF}
    FIntervalAsString: boolean;
  {$ENDIF}
    FUnicodeAsNational: boolean;

    FSmallintPrecision: integer;
    FIntegerPrecision: integer;
    FLargeIntPrecision: integer;
    FFloatPrecision: integer;
    FBCDPrecision, FBCDScale: integer;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    FFmtBCDPrecision, FFmtBCDScale: integer;
  {$ENDIF}
  {$ENDIF}

  { Charset parameters }
    FCharset: _string;
    FCharsetId: word;
    FCharLength: word;
    FQueryCharLength: boolean;
    FMaxCharLength: word;
    FMinCharLength: word;
    FUseUnicode: boolean;

  { NLS session parameters }
    FNlsParams: array[TNlsParamType] of TNlsSessionParam;

  { OCI73 }
    LDA: PLDA;
    HDA: PHDA;
  { OCI80 }
    hSvcCtx  : pOCISvcCtx;
    hServer  : pOCIServer;
    hSession : pOCISession;
    hOCIError : pOCIError;  // local error handle
    hOCIEnv   : pOCIEnv;
    hOCIAuthInfo : pOCIAuthInfo;
    //hTrans   : pOCITrans;

  {$IFDEF MSWINDOWS}
    hBusy: THandle;
  {$IFNDEF LITE}
    hMTSSvcCtx: pOCISvcCtx;
  {$ENDIF}
  {$ENDIF}

    procedure GetSessionParameters;
    procedure SetNlsParameter(const Name, Value: _string);
    function GetMaxStringSize: word;

  protected
    FOnFailover: TFailoverCallback;
    FOnInfoMessage: TInfoMessageCallback;

    procedure SetStatementCacheSize(Size: integer);
    procedure SetupEnvironment;
    procedure SetupConnection;
  {$IFNDEF LITE}
    procedure SetOptimizerMode;
    procedure SetClientIdentifier;
  {$ENDIF}

    procedure RaiseError(const Msg: string); virtual; // for TRIALCALL
    procedure DoError(E: Exception; var Fail: boolean); override;

  {$IFDEF MSWINDOWS}
  {$IFNDEF LITE}
    procedure MTSCheck(status: sword);
    procedure MTSError(var ErrorCode: sword; UseCallback: boolean);

    procedure Enlist(Transaction: TMTSTransaction); override;
    procedure UnEnlist(Transaction: TMTSTransaction); override;
  {$ENDIF}
  {$ENDIF}

    property AutoCommit;
    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}
    property ConvertEOL: boolean read FConvertEOL;

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetTransactionClass: TCRTransactionClass; override;

    procedure CheckOCI;
    procedure CheckOCI73;
    procedure CheckOCI80;

    procedure Check(Status: sword);
    procedure OraError(FOCICallStyle: TOCICallStyle; ErrorCode: sword; UseCallback: boolean; Component: TObject); virtual;

    procedure SetConnectionType(ConnectionType: TConnectionType);
    procedure Connect(const ConnectString: _string); override;
    procedure Disconnect; override;

    function CreateCommand: TOCICommand;
    function GetCommand: TOCICommand;

    function GetOracleVersion: word;
    function GetServerVersion: _string; override;
    function GetServerVersionFull: _string; override;
    function GetClientVersion: _string; override;

  {$IFNDEF LITE}
    procedure SetCurrentSchema(SchemaName : _string);
    function GetCurrentSchema: _string;
    function GetDefaultSchema: _string;
    function GetCachedSchema: _string;
  {$ENDIF}

    procedure BreakExec;

  { Multi Thread }
    procedure Busy;
    procedure BusyWait;
    procedure Release;
    function RunThread(RunMethod: TRunMethod; EndMethod: TEndMethod): TThread;
    function StopThread(var hThread: TThread{$IFDEF MSWINDOWS}; APeekMessage: boolean = False{$ENDIF}): boolean;

  { OCI73 }
    function GetLDA: PLDA;
    procedure SetLDA(Value: PLDA);

  { OCI80 }
    function GetSvcCtx: pOCISvcCtx;
    procedure SetSvcCtx(Value: pOCISvcCtx);

    procedure ChangePassword(NewPassword: _string);

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    procedure SetNonBlocking(Value: boolean); // nonblocking connection
    function GetOCICallStyle: TOCICallStyle;
    procedure SetOCICallStyle(Value: TOCICallStyle);
    function GetOCICallStyleCommand: TOCICallStyle;
    function GetLastError: integer;
    procedure SetLastError(Value: integer);
    procedure GetTableFields(TableName: _string; Fields: _TStringList);

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    function CheckIsValid: boolean; override;

  {$IFNDEF LITE}
    procedure ReturnToPool; override;
  {$ENDIF}

    property ProxyConnection : TOCIConnection read FProxyConnection write FProxyConnection;
    property OCIEnv: pOCIEnv read hOCIEnv;
    property UnicodeEnv: boolean read FUnicodeEnv;

    property OnFailover: TFailoverCallback read FOnFailover write FOnFailover;
    property OnInfoMessage: TInfoMessageCallback read FOnInfoMessage write FOnInfoMessage;
  end;

{ TOraCursor }

  TOraCursor = class (TCRCursor)
  private
    FCDA: PCDA;
    phOCIStmt: ppOCIStmt;

    hOCIError: pOCIError;  // local error handle
    hOCIEnv: pOCIEnv;
    FUnicodeEnv: boolean;
    FState: TCursorState;
    FOCICallStyle: TOCICallStyle;
    FScrollable: boolean;
    FStatementCache: boolean;
    FPrefetchRows: integer;


    procedure DisablePrefetching;
    function GetCDA: PCDA;
    function GethOCIStmt: pOCIStmt;
    procedure SethOCIStmt(Value: pOCIStmt);
    function GetOCIStmt: pOCIStmt;
    function GetOCIStmtPtr: ppOCIStmt;
    procedure SetOCICallStyle(Value: TOCICallStyle);
    procedure SetPrefetchRows(Value: integer);

    property hOCIStmt: pOCIStmt read GethOCIStmt write SethOCIStmt;

  protected
    procedure Check(Status: sword);
    procedure CheckOCI;
    procedure CheckOCI73;
    procedure CheckOCI80;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AllocCursor(StatementCache: boolean = False);
    procedure FreeCursor;
    procedure Disconnect; override;

    function CanFetch: boolean; override;

    property CDA: PCDA read GetCDA;
    property OCIStmt: pOCIStmt read GetOCIStmt;
    property OCIStmtPtr: ppOCIStmt read GetOCIStmtPtr;
    property State: TCursorState read FState write FState;
    property OCICallStyle: TOCICallStyle read FOCICallStyle write SetOCICallStyle;
    property PrefetchRows: integer read FPrefetchRows write SetPrefetchRows;
  end;

{$IFDEF MSWINDOWS}
{ TOCIChangeNotification }

  TChangeNotifyEventType = (cneNone, cneStartup, cneShutdown, cneShutdownAny,
    cneDropDB, cneDereg, cneObjChange);

  TCustomNotifyChanges = class
  private
    function GetCount: integer;
  protected
    FItems: array of TObject;
    function CreateItem(ChangeDescriptor: IntPtr): TObject; virtual; abstract;
  public
    constructor Create(OCIColl: pOCIColl);
    destructor Destroy; override;
    property Count: integer read GetCount;
  end;

  TNotifyRowChange = class
  private
    FRowId: string;
    FOperations: TChangeNotifyOperations;
  public
    constructor Create(ChangeDescriptor: IntPtr);
    property RowId: string read FRowId;
    property Operations: TChangeNotifyOperations read FOperations;
  end;

  TNotifyRowChanges = class(TCustomNotifyChanges)
  private
    function GetChanges(Index: integer): TNotifyRowChange;
  protected
    function CreateItem(ChangeDescriptor: IntPtr): TObject; override;
  public
    property Changes[Index: integer]: TNotifyRowChange read GetChanges; default;
  end;

  TNotifyTableChange = class
  private
    FTableName: _string;
    FOperations: TChangeNotifyOperations;
    FRowChanges: TNotifyRowChanges;
  public
    constructor Create(ChangeDescriptor: IntPtr);
    destructor Destroy; override;
    property TableName: _string read FTableName;
    property Operations: TChangeNotifyOperations read FOperations;
    property RowChanges: TNotifyRowChanges read FRowChanges;
  end;

  TNotifyTableChanges = class(TCustomNotifyChanges)
  private
    function GetChanges(Index: integer): TNotifyTableChange;
  protected
    function CreateItem(ChangeDescriptor: IntPtr): TObject; override;
  public
    property Changes[Index: integer]: TNotifyTableChange read GetChanges; default;
  end;

  TNotifyChange = class
  private
    FNotifyType: TChangeNotifyEventType;
    FTableChanges: TNotifyTableChanges;
  public
    constructor Create(ChangeDescriptor: IntPtr);
    destructor Destroy; override;
    property NotifyType: TChangeNotifyEventType read FNotifyType;
    property TableChanges: TNotifyTableChanges read FTableChanges;
  end;

  TChangeNotifyCallback = procedure(NotifyType: TChangeNotifyEventType;
    TableChanges: TNotifyTableChanges) of object;

  TOCIChangeNotification = class
  private
    FGCHandle: IntPtr;
    FEnabled: boolean;
    FPersistent: boolean;
    FTimeOut: integer;
    FOperations: TChangeNotifyDMLOperations;
    FOnChange: TChangeNotifyCallback;
    hOCISubscription: pOCISubscription;

    function GetGCHandle: IntPtr;
    procedure SetEnabled(Value: boolean);
    function CallbackChangeNotify(pCtx: IntPtr; pSubscrHp: pOCISubscription;
      pPayload: IntPtr; iPayloadLen: ub4; pDescriptor: IntPtr; iMode: ub4): sword;

  protected
    property GCHandle: IntPtr read GetGCHandle;

  public
    constructor Create;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean;
    function GetProp(Prop: integer; var Value: variant): boolean;

    function GetSubscriptionHandle(Connection: TOCIConnection): pOCISubscription;
    procedure Register(Connection: TOCIConnection);
    procedure Unregister(Connection: TOCIConnection);
    function IsActive: boolean;

    property OnChange: TChangeNotifyCallback read FOnChange write FOnChange;
  end;
{$ENDIF}

{ TOCICommand }

  TOCICommand = class (TCRCommand)
  private
    FCursor: TOraCursor;
    FCursorRef: TOraCursor;
    FOCICallStyle: TOCICallStyle;
    FNonBlocking: boolean;
    FSQLType: word;
    FLastSQLType: word;
    FRowsProcessed: integer;
    FFetchedRows: integer;
    FErrorOffset: word;
    FIterCount: integer;
    FFieldsAsString: boolean;
    FCacheLobs: boolean;
    FStoreRowId: boolean;
    FRowId: string;
    FRawAsString: boolean;
    FNumberAsString: boolean;
    FUseDefaultDataTypes: boolean;

    FSmallintPrecision: integer;
    FIntegerPrecision: integer;
    FLargeIntPrecision: integer;
    FFloatPrecision: integer;
    FBCDPrecision, FBCDScale: integer;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    FFmtBCDPrecision, FFmtBCDScale: integer;
  {$ENDIF}
  {$ENDIF}

    FForceUnprepare: boolean;
    FGCHandle: IntPtr;
    FTemporaryLobUpdate: boolean;
    FStatementCache: boolean;
  {$IFDEF MSWINDOWS}
    FChangeNotification: TOCIChangeNotification;
  {$ENDIF}
    FCheckParamHasDefault: boolean;
    FUseResultParams: boolean;

  { OCI8 }
    hOCIError: pOCIError;  // local error handle

  {$IFDEF MSWINDOWS}
    hBusy: THandle;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    hExecThread: TThread;
  {$ENDIF}
  {$IFDEF WIN32_64}
    hExecuted: TEvent;
  {$ENDIF}

    function GetGCHandle: IntPtr;
    function RemoveCRSymbols(SQLText: _string; var ErrorOffset: integer): _string;

  protected
    FConnection: TOCIConnection;

    procedure DoExecute;
    procedure EndExecute(E: Exception);

    procedure RaiseError(const Msg: string); virtual; // for TRIALCALL

    procedure CheckOCI;
    procedure CheckOCI73;
    procedure CheckOCI80;

    procedure CheckActive;
    procedure CheckInactive;
    procedure CheckSession;

    procedure Check(Status: sword);

    function GetSmallintPrecision: integer;
    function GetIntegerPrecision: integer;
    function GetLargeintPrecision: integer;
    function GetFloatPrecision: integer;
    function GetBCDPrecision: integer;
    function GetBCDScale: integer;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    function GetFmtBCDPrecision: integer;
    function GetFmtBCDScale: integer;
  {$ENDIF}
  {$ENDIF}

  { OCI73 }
    function GetOraType7(DataType: integer; SubDataType: integer{ = 0}): integer;
    function GetFieldDesc7(FieldNo: integer; var Field: TFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
    function InternalFetch7(Rows: word): word;
    function InternalFetchPiece7: integer;
    function DescSP(const objnam: _string; ovrld: pub2; pos: pub2; level: pub2;
      argnam: IntPtr; arnlen: pub2; dtype: pub2; defsup: pub1; mode: pub1;
      dtsiz: pub4; prec: psb2; scale: psb2; radix: pub1;
      spare: pub4; var arrsiz: ub4): sword;
    procedure InitProcParams7(Name: _string; Overload: integer);

  { OCI80 }
    function GetOraType8(DataType: integer; SubDataType: integer): integer;
    function GetFieldDesc8(FieldNo: integer; var Field: TFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
    function InternalFetch8(Rows: word; Orientation: integer; Offset: integer): word;
    function InternalExecuteFetch8(Rows: word): word;    
    function InternalFetchPiece8(Orientation: integer; Offset: integer): integer;
    procedure InitProcParams8(Name: _string; Overload: integer);

    function CallbackInBind(Bind: pOCIBind; Iter: ub4; Index: ub4; var Buf: IntPtr;
      var BufLen: ub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;
    function CallbackOutBind(Bind: pOCIBind; Iter: ub4; Index: ub4; var Buf: IntPtr;
      var BufLen: pub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;

    procedure SetArrayLength(Value: integer);
    function GetActive: boolean;

    property Params;
    property Executing;
    property GCHandle: IntPtr read GetGCHandle;

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;

    function GetOraType(DataType: integer; SubDataType: integer): integer;

    procedure InternalOpen;
    procedure InternalParse;
    procedure InternalPrepare;

    function ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string; override;
    procedure InitProcParams(Name: _string; Overload: integer);
    function CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string; override;

    function NeedBindParam(Param: TOraParamDesc): boolean;
    procedure BindParam(Param: TOraParamDesc);
    function InternalExecute(Mode: integer; Rows: Integer = 0): sword;
    procedure Exec;
    function GetFieldDesc(FieldNo: integer; var Field: TFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
    procedure DefineData(Field: TFieldDesc; Buf: IntPtr; Ind: psb2);
    procedure DefineArrayData(Field: TFieldDesc; Buf: IntPtr; Ind: psb2; BufSkip: integer;
      IndSkip: integer);
    procedure DefinePieceData(Field: TFieldDesc; Buf: IntPtr; Ind: psb2);
    procedure DefineDynamic(Field: TFieldDesc; Owner: IntPtr; Proc: IntPtr; CharsetId: Integer);

    function InternalFetch(Rows: word; Orientation: integer = OCI_FETCH_NEXT; Offset: integer = 0): word;
    function InternalFetchPiece(Orientation: integer = OCI_FETCH_NEXT; Offset: integer = 0): integer;
    procedure InternalCancel;
    procedure InternalClose;
    procedure Finish;

    procedure GetPI(var Handle: pOCIHandle; var Piece: byte; var Buf: IntPtr;
      var Iteration: cardinal; var Index: cardinal; var Mode: TParamDirection);
    procedure SetPI(Handle: pOCIHandle; HType: cardinal; Piece: byte; Buf: IntPtr;
      var BufLen: cardinal; Ind: psb2);

    function NativeCursor: boolean;
    function RowsReturn: boolean;
    procedure CheckRowsReturn;

  { Params }
    function AddParam: TParamDesc; override;
    procedure BindParams;
    //procedure DisconnectParams;
    function GetParam(Index: integer): TOraParamDesc;

    procedure BreakExec; override;
    procedure HardBreak;

    procedure Busy;
    procedure Release;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;

    procedure Execute(Iters: integer = 1); override;

    procedure SetConnection(Value: TCRConnection); override;
    function GetCursor: TCRCursor; override;
    procedure SetCursor(Value: TCRCursor); override;
    function GetNextCursor: TOraCursor;
    procedure SetOCICallStyle(Value: TOCICallStyle);
    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;
    function GetSQLType: integer;
    function GetRowId: string;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

  {$IFDEF MSWINDOWS}
    property ChangeNotification: TOCIChangeNotification read FChangeNotification write FChangeNotification;
  {$ENDIF}
  end;

{ TOCISQLInfo }

  TOCISQLInfo = class(TSQLInfo)
  protected
    function NextCharQuotesNeed(Ch: _Char; IdCase: TIdentCase): boolean; override;
  public
    function IdentCase: TIdentCase; override;

    function NormalizeName(const Value: _string; const LeftQ: _char; const RightQ: _char; QuoteNames: boolean = False; UnQuoteNames: boolean = False): _string; override;
    // for normal C++Builder header
    function NormalizeName(const Value: _string; QuoteNames: boolean = False; UnQuoteNames: boolean = False): _string; override;

    procedure SplitObjectName(const Name: _string; var Info: TExtTableInfo); override;

    procedure ParseTablesInfo(const SQL: _string; TablesInfo: TCRTablesInfo); override;

    class function ParseSPName(FullName: _string; var Name: _string; var Overload: integer): boolean;
    class function GetFinalSPName(Name: _string; Overload: integer): _string;
  end;

  TOCIFieldDesc = class(TCRFieldDesc)
  public
    function IsNational: boolean; override;
  end;

{ TOCIRecordSet }

  TModifyAction = procedure of object;

  TOCIRecordSet = class (TCRRecordSet)
  private
    FAutoClose: boolean;
    FDeferredLobRead: boolean;
    hExecFetchThread: TThread;
    hFetchAllThread: TThread;
    FFetchCursor: TOraCursor;
    FFetchBlock: IntPtr;
    FFetchBlockItemSize: integer;
    FPieceFetch: boolean;
    FFetchItems: IntPtr;  // for callback fetch
    // for backward fetch
    FFetchAbsolute: boolean;
    FFetchStart: integer;
    FFetchEnd: integer;
    FGCHandle: IntPtr;
    FHasConvertedFields: boolean;
  {$IFDEF MSWINDOWS}
    hEvent: TEvent;
  {$ENDIF}
    FStopFetch : boolean;
    FFetching : boolean;
    FHasObjectFields: boolean;
    FTempFilterText: _string;
    FDisableInitFields: boolean;

    //PreCached FConection properties
    FDisconnectedMode: boolean;
    FUseUnicode: boolean;
    FCharLength: integer;

    function HasCursorParams: boolean;
    procedure InitFetchCursor;

    function FetchArray(FetchBack: boolean = False): boolean;
    function FetchPiece(FetchBack: boolean = False): boolean;

    procedure AllocFetchBlock;
    procedure FreeFetchBlock;

    function GetNonBlocking: boolean;
    function GetGCHandle: IntPtr;
    function GetDisconnectedMode: boolean;
    function GetUseUnicode: boolean;
    function GetCharLength: integer;

    function IsConvertedFieldType(DataType: word): boolean;
  protected
    FCommand: TOCICommand;
    FConnection: TOCIConnection;  // for perf

    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;

  { Open/Close }
    function NeedInitFieldsOnPrepare: boolean; override;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;
    procedure InternalInitFields; override;

    procedure ExecFetch(DisableInitFields: boolean); override;

    function GetIndicatorSize: word; override;

    function GetEOF: boolean; override;

  { Filter/Find/Locate/Sorting }
    procedure SetFilterText(Value: _string); override;
          
  { Fetch }
    procedure InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean); override;
    function Fetch(FetchBack: boolean = False): boolean; override;
    function CanFetchBack: boolean; override;

  { Items }
    procedure FreeAllItems;

  { Edit }
    procedure DoExecFetch;
    procedure EndExecFetch(E: Exception);
    procedure DoFetchAll;
    procedure DoFetchAllPulse;
    procedure EndFetchAll(E: Exception);

    function CallbackDefine(Define: pOCIDefine; Iter: cardinal; var Buf: IntPtr;
      var BufLen: pub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;

    property GCHandle: IntPtr read GetGCHandle;
    //PreCached FConection properties
    property DisconnectedMode: boolean read GetDisconnectedMode;
    property UseUnicode: boolean read GetUseUnicode;
    property CharLength: integer read GetCharLength;

  public
    constructor Create; override;
    destructor Destroy; override;

  { Open/Close }
    function IsFullReopen: boolean; override;
    procedure Reopen; override;
    procedure SetCommandType;
    procedure ExecCommand; override; // Execute command
    procedure BreakFetch; override;
    procedure Disconnect; override;    

  { Fetch }
    procedure FetchAll; override;

    function RowsReturn: boolean; override;
  { Fields }
    function GetFieldDescType: TFieldDescClass; override;

    procedure SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean); override;
    function GetNull(FieldNo: word; RecBuf: IntPtr): boolean; override;

    procedure GetDateFromBuf(Buf: IntPtr; Field: TFieldDesc; Date: IntPtr; Format: TDateFormat); override;
    procedure PutDateToBuf(Buf: IntPtr; Field: TFieldDesc; Date: IntPtr; Format: TDateFormat); override;

    class function IsBlobFieldType(DataType: word): boolean; override;
    class function IsComplexFieldType(DataType: word): boolean; override;

    procedure GetFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Dest: IntPtr); override;
    procedure GetFieldAsVariant(FieldNo: word; RecBuf: IntPtr; var Value: variant); override;
    procedure PutFieldAsVariant(FieldNo: word; RecBuf: IntPtr; const Value: variant; IsDatabaseValue: boolean = False); override;

    function FieldListDependsOnParams: boolean; override;

  { Records }
    procedure CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean); override;

    function CompareFieldValue(ValuePtr: IntPtr; const ValueType: integer; FieldDesc: TFieldDesc; RecBuf: IntPtr; const Options: TCompareOptions): integer; override;
    function CompareFields(RecBuf1: IntPtr; RecBuf2: IntPtr; FieldDesc: TFieldDesc; Options: TCompareOptions = []): integer; override;
    procedure SortItems; override;
    procedure FilterUpdated; override;

  { Navigation }
    procedure SetToBegin; override;
    procedure SetToEnd; override;

  { BookMarks }
    procedure GetBookmark(Bookmark: PRecBookmark); override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;
    function GetBlockFetchPos(Block: PBlockHeader): integer;
    function GetItemFetchPos(Item: PItemHeader): integer;

  { Blobs }
    procedure SetConnection(Value: TCRConnection); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
  end;

{ TOraLob }

  TLobType = (ltBlob, ltClob, ltNClob);

  TOraLob = class (TCompressedBlob)
  private
    FConnection: TOCIConnection;
    FSvcCtx: pOCISvcCtx;
    phLobLocator: ppOCILobLocator;
    hOCIEnv: pOCIEnv;
    FUnicodeEnv: boolean;
    FNativeHandle: boolean;
    FCached: boolean;
    FCharsetForm: integer;
    FLobType: TLobType;
    FCharLength: byte;

    function GetOCILobLocator: pOCILobLocator;
    function GethOCILobLocator: pOCIStmt;
    procedure SethOCILobLocator(Value: pOCIStmt);
    procedure SetOCILobLocator(Value: pOCILobLocator);
    function GetOCILobLocatorPtr: ppOCILobLocator;
    procedure SetCached(const Value: boolean);
    procedure SetOCISvcCtx(const Value: pOCISvcCtx);
    procedure SetConnection(const Value: TOCIConnection);


    property hLobLocator: pOCILobLocator read GethOCILobLocator write SethOCILobLocator;

  protected
    FNeedReadLob: boolean;

    procedure Check(Status: sword);
    procedure CheckValue; override;
    function GetSize: Cardinal; override;
    function GetSizeAnsi: Cardinal; override;
    procedure CheckAlloc;
    procedure CheckSession;
    procedure CheckInit;
    procedure CheckCharSetForm;
    function CharSize: Byte; virtual;
    procedure ReadLob(var SharedPiece: PPieceHeader); overload;

  public
    constructor Create(ASvcCtx: pOCISvcCtx); 
    destructor Destroy; override;

    procedure AllocLob; virtual;
    procedure FreeBlob; override;
    procedure FreeLob;
    procedure Disconnect; override;

    procedure Init;
    procedure CreateTemporary(LobType: TLobType);
    procedure FreeTemporary;
    function IsTemporary: LongBool;
    function IsInit: boolean;

    function LengthLob: Cardinal;

    procedure EnableBuffering;
    procedure DisableBuffering;

    procedure ReadLob; overload;
    procedure WriteLob;

    function Read(Position, Count: cardinal; Dest: IntPtr): cardinal; override;
    procedure Write(Position, Count: cardinal; Source: IntPtr); override;
    procedure Clear; override;
    procedure Truncate(NewSize: cardinal); override;

    property OCISvcCtx: pOCISvcCtx read FSvcCtx write SetOCISvcCtx;
    property Connection: TOCIConnection read FConnection write SetConnection;

    property OCILobLocator: pOCILobLocator read GetOCILobLocator write SetOCILobLocator;
    property OCILobLocatorPtr: ppOCILobLocator read GetOCILobLocatorPtr;
    property Cached: boolean read FCached write SetCached;
    property LobType: TLobType read FLobType write FLobType;
  end;

{ TOraFile }

  TOraFile = class (TOraLob)
  private
    FNeedRollback: boolean;
    FRollbackFileDir: _string;
    FRollbackFileName: _string;

    function GetFileDir: _string;
    procedure SetFileDir(const Value: _string);
    function GetFileName: _string;
    procedure SetFileName(const Value: _string);
    procedure SetFileDirAndName(const FileDir, FileName: _string);

  protected
    CanRollback: boolean;

    procedure CheckValue; override;
    function CharSize: Byte; override;

    procedure SaveToRollback; override;
  public
    destructor Destroy; override;

    procedure AllocLob; override;
    procedure FreeBlob; override;

    procedure Open;
    procedure Close;
    procedure EnableRollback;

    procedure Commit; override;
    procedure Cancel; override;

    procedure Refresh;

    function IsOpen: boolean;

    function Exists: boolean;

    property FileDir: _string read GetFileDir write SetFileDir;
    property FileName: _string read GetFileName write SetFileName;
  end;

{ TOraTimeStamp }

  TOraTimeStamp = class (TSharedObject)
  private
    phOCIDateTime: ppOCIDateTime;
    FDescriptorType: cardinal;
    FPrecision: byte;
    FFormat: string;
    FNativeHandle: boolean;
    FIndicator: IntPtr;

    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetTimeZone: string;
    function GethOCIDateTime: pOCIDateTime;
    procedure SethOCIDateTime(Value: pOCIDateTime);
    function GetOCIDateTime: pOCIDateTime;
    procedure SetOCIDateTime(const Value: pOCIDateTime);
    procedure SetDescriptorType(const Value: cardinal);
    function GetOCIDateTimePtr: ppOCIDateTime;

    procedure CheckValid;

    procedure SetFormat(const AFormat: string);

    property hOCIDateTime: pOCIDateTime read GethOCIDateTime write SethOCIDateTime;

  public
    constructor Create(DataType: word);
    destructor Destroy; override;

    procedure AllocDateTime;
    procedure FreeDateTime;
    procedure Disconnect; override;

    procedure AssignTo(Dest: TOraTimeStamp);
    function Compare(Dest: TOraTimeStamp): integer;

    function GetIsNull: boolean;
    procedure SetIsNull(Value: boolean);

    procedure Construct(Year: smallint; Month, Day, Hour, Min, Sec: byte;
      FSec: cardinal; TimeZone: string);

    procedure GetDate(var Year: smallint; var Month, Day: byte);
    procedure SetDate(Year: smallint; Month, Day: byte);

    procedure GetTime(var Hour, Min, Sec: byte; var FSec: cardinal);
    procedure SetTime(Hour, Min, Sec: byte; FSec: cardinal);

    procedure GetTimeZoneOffset(var Hour, Min: shortint);
    procedure SetTimeZoneOffset(TZHour, TZMin: shortint);

    property DescriptorType: cardinal read FDescriptorType write SetDescriptorType;
    property OCIDateTime: pOCIDateTime read GetOCIDateTime write SetOCIDateTime;
    property OCIDateTimePtr: ppOCIDateTime read GetOCIDateTimePtr;
    property Format: string read FFormat write SetFormat;
    property Precision: byte read FPrecision write FPrecision;
    property TimeZone: string read GetTimeZone;
    property AsString: string read GetAsString write SetAsString;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property IsNull: boolean read GetIsNull write SetIsNull;
  end;

  TOraInterval = class (TSharedObject)
  private
    phOCIInterval: ppOCIInterval;
    FDescriptorType: cardinal;
    FNativeHandle: boolean;
    FFracPrecision: byte;
    FLeadPrecision: byte;
    FIndicator: IntPtr;

    procedure Init;
    procedure CheckValid;
    function GetAsString: string;
    function GethOCIInterval: pOCIInterval;
    procedure SethOCIInterval(Value: pOCIInterval);
    function GetOCIInterval: pOCIInterval;
    function GetOCIIntervalPtr: ppOCIInterval;
    procedure SetAsString(const Value: string);
    procedure SetDescriptorType(const Value: cardinal);
    procedure SetOCIInterval(const Value: pOCIInterval);

    property hOCIInterval: pOCIInterval read GethOCIInterval write SethOCIInterval;
  public
    constructor Create(DataType: word);
    destructor Destroy; override;

    procedure AllocInterval;
    procedure FreeInterval;
    procedure Disconnect; override;

    procedure AssignTo(Dest: TOraInterval);
    function Compare(Dest: TOraInterval): integer;

    function GetIsNull: boolean;
    procedure SetIsNull(Value: boolean);

    procedure GetYearMonth(var Year, Month: integer);
    procedure SetYearMonth(Year, Month: integer);

    procedure GetDaySecond(var Day, Hour, Min, Sec, FSec: integer);
    procedure SetDaySecond(Day, Hour, Min, Sec, FSec: integer);

    property DescriptorType: cardinal read FDescriptorType write SetDescriptorType;
    property OCIInterval: pOCIInterval read GetOCIInterval write SetOCIInterval;
    property OCIIntervalPtr: ppOCIInterval read GetOCIIntervalPtr;
    property LeadPrecision: byte read FLeadPrecision write FLeadPrecision;
    property FracPrecision: byte read FFracPrecision write FFracPrecision;
    property AsString: string read GetAsString write SetAsString;
    property IsNull: boolean read GetIsNull write SetIsNull;
  end;

  TOraNumber = class (TSharedObject)
  private
    phOCINumber: pOCINumber;
    FIndicator: IntPtr;
    FNativeHandle: boolean;

    function GetOCINumberPtr: pOCINumber;
    procedure SetOCINumberPtr(Value: pOCINumber);
    function GetOCINumber: OCINumber;
    procedure SetOCINumber(Value: OCINumber);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsInteger: integer;
    procedure SetAsInteger(const Value: integer);
    function GetAsLargeInt: int64;
    procedure SetAsLargeInt(const Value: int64);
    function GetAsFloat: double;
    procedure SetAsFloat(const Value: double);
    function GetIsNull: boolean;
    procedure SetIsNull(Value: boolean);
  {$IFDEF VER6P}
    function GetAsBCD: TBCD;
    procedure SetAsBCD(const Value: TBCD);
  {$ENDIF}

  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignTo(Dest: TOraNumber);
    function Compare(Dest: TOraNumber): integer;

    property OCINumber: OCINumber read GetOCINumber write SetOCINumber;
    property OCINumberPtr: pOCINumber read GetOCINumberPtr write SetOCINumberPtr;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
    property AsLargeInt: Int64 read GetAsLargeInt write SetAsLargeInt;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property IsNull: boolean read GetIsNull write SetIsNull;
  {$IFDEF VER6P}
    property AsBCD: TBCD read GetAsBCD write SetAsBCD;
  {$ENDIF}
  end;

{ TOCITransaction }

  TOraTransactionState = (tsInactive, tsActive, tsPrepared, tsFinished);

{$IFNDEF LITE}
  TTransactionLink = record
    BranchQualifier: TBytes;
    State: TOraTransactionState;
    OCITrans: pOCITrans;
  end;
{$ENDIF}

  TOCITransaction = class (TCRTransaction)
  private
    FLocalTransactionId: string;
  {$IFNDEF LITE}
    FTransactionName: _string;
    FRollbackSegment: _string;
    FTransactionLinks: array of TTransactionLink;
    FInactiveTimeOut: integer;
    FResumeTimeOut: integer;
    FXID: IntPtr;
    FTransactionId: TBytes;
    FResume: boolean;
  {$ENDIF}

  protected
  {$IFNDEF LITE}
    procedure WriteTransactionId;
    procedure WriteBranchQualifier(TransactionLink: TTransactionLink);
    procedure FreeTransaction;
  {$ENDIF}

    function LocalTransactionId(CreateTransaction: boolean = False): string;
    procedure StartTransactionLocal;
    procedure CommitLocal;
    procedure RollbackLocal;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Check(Status: sword);
    procedure OraError(var ErrorCode: sword; UseCallback: boolean);

  {$IFNDEF LITE}
    procedure SetTransactionId(TransactionId: TBytes);
    procedure SetBranchQualifier(Index: integer; BranchQualifier: TBytes);
  {$ENDIF}

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    function DetectInTransaction(CanActivate: boolean): boolean; override;
    procedure AssignConnect(Source: TCRTransaction); override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure Savepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;
  {$IFNDEF LITE}
    procedure Detach;
  {$ENDIF}
  end;

{$IFNDEF LITE}

{ TOCIMetaData }

  TOCIMetaData = class (TCRMetaData)
  protected
    function GetTypesForSQL(const ObjectTypes: _string; AllTypes: array of _string): _string;
    function CreateRecordSet: TCRRecordSet; override;

    function InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData; override;
    procedure InternalGetMetaDataKindsList(List: _TStringList); override;
    procedure InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string); override;
    function GetTables(Restrictions: _TStrings): TData; override;
    function GetColumns(Restrictions: _TStrings): TData; override;
    function GetProcedures(Restrictions: _TStrings): TData; override;
    function GetProcedureParameters(Restrictions: _TStrings): TData; override;
    function GetIndexes(Restrictions: _TStrings): TData; override;
    function GetIndexColumns(Restrictions: _TStrings): TData; override;
    function GetConstraints(Restrictions: _TStrings): TData; override;

    function GetDataTypes(Restrictions: _TStrings): TData; override;
    function GetUsers(Restrictions: _TStrings): TData; override;
    function GetUDTs(Restrictions: _TStrings): TData; override;
    function GetPackages(Restrictions: _TStrings): TData; override;
    function GetSequences(Restrictions: _TStrings): TData;
  end;

{ TOCILoader }

  TOCILoaderColumn = class (TCRLoaderColumn)
  private
    FDateFormat: _string;
    FOffset: integer;
    FColumnType: integer;
    FIsNational: boolean;
    FIsLob: boolean;
  protected
    procedure VerifySize;
  public
    constructor Create; override;

    property DateFormat: _string read FDateFormat write FDateFormat;
    property IsNational: boolean read FIsNational write FIsNational;
    property IsLob: boolean read FIsLob write FIsLob;
  end;

  _TDPErrorAction = (_dpAbort, _dpFail, _dpIgnore);
  _TDPErrorEvent = procedure(E: Exception; Col, Row: integer; var Action: _TDPErrorAction) of object;

  TOCILoader = class(TCRLoader)
  private
    FConnection: TOCIConnection;
    FIsDirectMode: boolean;
    FOnError: _TDPErrorEvent;
    FDML: TOCICommand;

    hDirPathCtx: pOCIDirPathCtx;
    hColumnArray: pOCIDirPathColArray;
    hStream: pOCIDirPathStream;

    FBufNumRows: integer;  // count of Rows in ColumnArray
    FBufNumCols: word;     // count of Cols in ColumnArray
    FRowSize: word;      // size of Row in buffer
    FBuffer: IntPtr;
    FBlobBuffer: TList;

  protected
    class function GetRecordSetClass: TCRRecordSetClass; override;
    procedure SetConnection(Value: TCRConnection); override;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); override;
    function ConverToUnicode(Column: TOCILoaderColumn): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    class function GetColumnClass: TCRLoaderColumnClass; override;
    procedure Prepare; override;
    procedure Reset; override;
    procedure PutColumnData(Col, Row: integer; const Value: variant); override;
    procedure DoLoad; override;
    procedure Finish; override;

    property OnError: _TDPErrorEvent read FOnError write FOnError;
  end;

{ TOCIAlerter }

  _TEventType = (_etAlert, _etPipe);
  TMessageType = (mtNone, mtNumber, mtString, mtDate);

  TOCIAlerterTimeoutCallback = procedure(var Continue: boolean) of object;

  TOCIAlerterListenThread = class;

  TOCIAlerter = class(TCRAlerter)
  private
    FGCHandle: IntPtr;
    FListenConnection: TOCIConnection;
    FWaitCommand: TOCICommand;
    FListenThread: TOCIAlerterListenThread;
    FTimeOut: integer;
    FOnTimeOut: TOCIAlerterTimeoutCallback;
    FInterval: integer;
    FEventType: _TEventType;
    FSelfMessage: _string;
    FRegistered: boolean;
    FAutoCommit: boolean;
    FSelfEvents: boolean;
  {$IFDEF MSWINDOWS}
    FResponseEvent: TEvent;
    FIntervalEvent: TEvent;
  {$ENDIF}

    function GetGCHandle: IntPtr;
    procedure RegisterEvents;
    procedure RemoveEvents;
    procedure SendAlertMessage(const EventName, Message: _string);
    function ProcessWaitResult: boolean;
    procedure ProcessMessage;
    function ProcessError(Status: integer): boolean;
    procedure DoOnEvent(const EventName, Message: _string);
    procedure DoOnTimeout(var Continue: boolean);
    procedure DoOnError(E: Exception);

  protected
    property GCHandle: IntPtr read GetGCHandle;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SendEvent(const EventName, Message: _string); override;
    procedure Start; override;
    procedure Stop; override;

    // pipe methods
    procedure PackMessage(const Item: variant);
    procedure UnpackMessage(var Item: variant);
    function NextItemType: TMessageType;
    procedure SendPipeMessage(const Name: _string);
    procedure PurgePipe;

    property OnTimeOut: TOCIAlerterTimeoutCallback read FOnTimeOut write FOnTimeOut;
  end;

  TOCIAlerterListenThread = class(TThread)
  private
    FAlerter: TOCIAlerter;

  protected
    procedure Execute; override;

  public
    constructor Create(Alerter: TOCIAlerter);
    procedure Terminate;
  end;

{$ENDIF}

{ TOraClassesUtils }

  TOraClassesUtils = class
  public
    class procedure InternalUnPrepare(Obj: TOCIRecordSet);
  end;

var
  SmallintPrecision: integer = 0;
  IntegerPrecision: integer = 9;
  LargeIntPrecision: integer = 0;
  FloatPrecision: integer   = 15;
  BCDPrecision: string      = '14,4';
{$IFNDEF FPC}
  FmtBCDPrecision: string   = '38,38';
{$ENDIF}
  EnableWideOraClob: boolean = True;
  RemoveCRInStringLiterals: boolean = False;
  UseMaxDataSize: boolean = True;
  NumberAsInteger: boolean = False;
  UseOCI7ProcDesc: boolean = False;
  OldTimeStampLTZRepresentation: boolean = False; // Keep old behavior for TIMESTAMP WITH LOCAL TIME ZONE
  OCISQLInfo: TOCISQLInfo;

  function OraDateToDateTime(Buf: IntPtr): TDateTime;
  function OraDateToMSecs(Buf: IntPtr): double;
  procedure DateTimeToOraDate(DateTime: TDateTime; Buf: IntPtr);
  procedure MSecsToOraDate(MSecs: double; Buf: IntPtr);
{$IFDEF VER6P}
{$IFNDEF FPC}
  procedure OraTimeStampToSQLTimeStamp(OraTimeStamp: TOraTimeStamp; var SQLTimeStamp: TSQLTimeStamp);
  procedure SQLTimeStampToOraTimeStamp(OraTimeStamp: TOraTimeStamp; const SQLTimeStamp: TSQLTimeStamp);
{$ENDIF}
{$ENDIF}

  procedure OCIInit;
  procedure OCIFinish;

  function QuotedOCIName(Name: _string): _string;
  function QuotedSQLName(Name: _string): _string;

  procedure ParseConnectString(const ConnectString: _string;
    var Username, Password, Server: _string; var ConnectMode: TConnectMode); 

{$IFDEF MSWINDOWS}
  procedure AllocODACWnd;
{$ENDIF}

implementation

{$IFDEF UNICODE_BUILD}
{$IFNDEF CLR}
{$DEFINE ORANETW}
{$ENDIF}
{$ENDIF}

uses
  {$IFDEF PROF}OraProf, {$ENDIF}
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text, System.Threading,
{$ELSE}
  {$IFDEF VER6P}Variants,{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  ComObj,
{$ENDIF}
{$ENDIF}
{$IFDEF NET}
{$IFNDEF ORANETW}
  {$IFNDEF UNIDACPRO}OraNet{$ELSE}OraNetUni{$ENDIF},
{$ELSE}
  {$IFNDEF UNIDACPRO}OraNetW{$ELSE}OraNetUniW{$ENDIF},
{$ENDIF}
{$ENDIF}
  DAConsts, {$IFNDEF UNIDACPRO}OraConsts{$ELSE}OraConstsUni{$ENDIF},
  {$IFNDEF UNIDACPRO}OraObjects{$ELSE}OraObjectsUni{$ENDIF},
  {$IFNDEF UNIDACPRO}OraParser{$ELSE}OraParserUni{$ENDIF}, Math;

const
  WM_ENDTHREAD       = $400;
  WM_AFTERFETCH      = $404;
  WM_CHANGENOTIFY    = $405;
  WM_ALERTER_EVENT   = $406;
  WM_ALERTER_TIMEOUT = $407;
  WM_ALERTER_STOPED  = $408;

  msTerminate = '__ODAC_Terminate_Event__';
  msBlank = '__ODAC_Blank_Message__';

type
  TArr = array [0..100] of byte;  // DEBUG TEMP
  PArr = ^TArr;
  TArrC = array [0..100] of char;  // DEBUG TEMP
  PArrC = ^TArrC;

{$IFNDEF LINUX}
type
  TExecThread =  class(TThread)
  protected
    FMethodDesc: TMethodDesc;
    FException: Exception;
    FGCHandle: IntPtr;
  public
    constructor Create(MethodDesc: TMethodDesc; CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;
{$ENDIF}

type
  TAlertEvent = class
  public
    Name: _string;
    Message: _string;
  end;

var
{$IFDEF LINUX}
  hLockConnect: TCriticalSection;
{$ENDIF}
  ActiveConnectionsCount: integer;
  hLockConnectCount: TCriticalSection;
{$IFDEF MSWINDOWS}
  hODACWindow: HWND = 0;
{$ENDIF}

  OCICallbackDefinePtr: IntPtr;
  OCICallbackInBindPtr: IntPtr;
  OCICallbackOutBindPtr: IntPtr;
  OCICallbackFailoverPtr: IntPtr;
{$IFDEF MSWINDOWS}
  OCICallbackChangeNotifyPtr: IntPtr;
{$ENDIF}

{$IFDEF CLR}
  HOCICallbackDefine: GCHandle;
  HOCICallbackInBind: GCHandle;
  HOCICallbackOutBind: GCHandle;
  HOCICallbackFailover: GCHandle;
  HOCICallbackChangeNotify: GCHandle;
  HWndProc: GCHandle;
{$ENDIF}

procedure OCIInit;
begin
  {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.InitOCI;
end;

procedure OCIFinish;
begin
  FinishOCI;
end;

function Shift(Value: cardinal): cardinal;
begin
  Result := Value;
  if Result <> 0 then
    if (Result and $FF) = 0 then  // while do
      Result := Result shr 8;
end;

function Reverse2(Value: word): TBytes;
begin
  SetLength(Result, 2);
  Result[0] := byte(Value shr 8);
  Result[1] := byte(Value);
end;

function Reverse4(Value: cardinal): TBytes;
begin
  SetLength(Result, 4);
  Result[0] := byte(Value shr 24);
  Result[1] := byte(Value shr 16);
  Result[2] := byte(Value shr 8);
  Result[3] := byte(Value);
end;

// Converts Count bytes from memory pointed by Bytes to 64 base string. Starting
// digit (6-bit chunk) may be shifted by -4, -2, 0 or 2 bits. Missing bits
// assumed to be zero.
// Bytes are converted in the following way (example for Shift = 0):
// 0 byte   1 byte   hi    lo
// 00000100|00000000|01000001|01000011|...
// ------++ ++++---- --++++++ ------++ +++
//  B(1)   A(0)   B(1)  B(1)   Q(16)
function BytesTo64BaseString(Bytes: TBytes; Count: integer; Shift: integer): string;
const
  Map = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i, RestBits: cardinal;
  CurByte, Digit, NextDigit: byte;
begin
  Result :='';
  RestBits := 2 - Shift;
  NextDigit := $FF;

  for i := 0 to Count - 1 do begin
    CurByte := Bytes[i];

    Digit := CurByte shr RestBits;
    if NextDigit <> $FF then
      Digit := Digit or NextDigit;

    Result := Result + Map[Digit + 1];

    NextDigit := (CurByte and ($FF shr (8 - RestBits))) shl (6 - RestBits);

    if RestBits = 6 then begin
      Result := Result + Map[NextDigit + 1];
      NextDigit := $FF;
      RestBits := 2;
    end
    else
      RestBits := RestBits + 2;
  end;

  if NextDigit <> $FF then
    Result := Result + Map[NextDigit + 1];
end;

function RowId7ToString(RowId: PRowId7): string;
var
  Buf: TBytes;
begin
  Buf := nil;
  if (RowId.rd.rcs4 = 0) then begin // obj num
  // restricted (Oracle 7)
    Result :=
      IntToHex(Shift(RowId.rcs7), 8) + '.' +
      IntToHex(Shift(RowId.rcs8), 4) + '.' +  // use 2 byte
      IntToHex(RowId.rd.rcs5, 4)
  end
  else begin
  // extended (Oracle 8 and higher)
    Buf := Reverse4(RowId.rd.rcs4);
    Result := BytesTo64BaseString(Buf, 4, -4);

    Buf := Reverse2(RowId.rd.rcs5);
    Result := Result + BytesTo64BaseString(Buf, 2, -2);

    Buf := Reverse4(Shift(RowId.rcs7));
    Result := Result + BytesTo64BaseString(Buf, 4, -4);

    Buf := Reverse2(Shift(RowId.rcs8)); // use 3 byte
    Result := Result + BytesTo64BaseString(Buf, 2, -2);
  end;
end;

function RowId8ToString(RowId: PRowId8): string;
var
  Buf: TBytes;
begin
  Buf := nil;
  if (RowId.ridobjnum = 0) then
  // restricted (Oracle 7)
    Result :=
      IntToHex(Shift(RowId.ridblocknum), 8) + '.' +
      IntToHex(Shift(RowId.ridslotnum), 4) + '.' +
      IntToHex(RowId.ridfilenum, 4)
  else begin
  // extended (Oracle 8 and higher)
    Buf := Reverse4(RowId.ridobjnum);
    Result := BytesTo64BaseString(Buf, 4, -4);

    Buf := Reverse2(RowId.ridfilenum);
    Result := Result + BytesTo64BaseString(Buf, 2, -2);

    Buf := Reverse4(RowId.ridblocknum);
    Result := Result + BytesTo64BaseString(Buf, 4, -4);

    Buf := Reverse2(RowId.ridslotnum);
    Result := Result + BytesTo64BaseString(Buf, 2, -2);
  end;
end;

function RowId81ToString(RowIdPtr: PRowId81): string;
var
  Bytes: TBytes;
begin
  if RowIdPtr.ridobjnum = 0 then begin
  // restricted (Oracle 7)
    Result :=
      IntToHex(BitConverter.ToInt32(Reverse4(RowIdPtr.ridblocknum), 0), 8) + '.' +
      IntToHex(BitConverter.ToInt16(Reverse2(RowIdPtr.ridslotnum), 0), 4) + '.' +
      IntToHex(BitConverter.ToInt16(Reverse2(RowIdPtr.ridfilenum), 0), 4)
  end
  else begin
  // extended (Oracle 8 and higher)
    SetLength(Bytes, 4);
    Marshal.Copy(PtrOffset(RowIdPtr, 1{TRowId81.ridobjnum}), Bytes, 0, 4);
    Result := BytesTo64BaseString(Bytes, 4, -4);
    Marshal.Copy(PtrOffset(RowIdPtr, 5{TRowId81.ridfilenum}), Bytes, 0, 2);
    Result := Result + BytesTo64BaseString(Bytes, 2, -2);
    Marshal.Copy(PtrOffset(RowIdPtr, 7{TRowId81.ridblocknum}), Bytes, 0, 4);
    Result := Result + BytesTo64BaseString(Bytes, 4, -4);
    Marshal.Copy(PtrOffset(RowIdPtr, 11{TRowId81.ridslotnum}), Bytes, 0, 2);
    Result := Result + BytesTo64BaseString(Bytes, 2, -2);
  end;
end;

function URowIdToString(RowIdPtr: PRowId81; Length: integer): string;
var
  Bytes: TBytes;
begin
  SetLength(Bytes, Length - 1);
  Marshal.Copy(PtrOffset(RowIdPtr, 1), Bytes, 0, Length - 1);
  Result := '*' + BytesTo64BaseString(Bytes, Length - 1, 0);
end;

{ Data convertion }

function OraDateToDateTime(Buf: IntPtr): TDateTime;
var
  Time: TDateTime;
  OraDate: TBytes;
begin
  SetLength(OraDate, 7);
  Marshal.Copy(Buf, OraDate, 0, 7);
  Result := EncodeDate(Abs((OraDate[0] - 100) * 100 + OraDate[1] - 100),
    OraDate[2], OraDate[3]);
  Time := EncodeTime(OraDate[4] - 1, OraDate[5] - 1, OraDate[6] - 1, 0);
  if Result < 0 then
    Result := Result - Time
  else
    Result := Result + Time;
end;

function OraDateToMSecs(Buf: IntPtr): double;
begin
  Result := TimeStampToMSecs(DateTimeToTimeStamp(OraDateToDateTime(Buf)));//{$IFNDEF CLR}BitConverter.DoubleToInt64Bits{$ENDIF}
end;

procedure DateTimeToOraDate(DateTime: TDateTime; Buf: IntPtr);
var
  Year, Month, Day, Hour, Min, Sec, MSec: word;
  OraDate: TBytes;
begin
  SetLength(OraDate, 8);
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Min, Sec, MSec);
  OraDate[0] := Year div 100 + 100;
  OraDate[1] := Year mod 100 + 100;
  OraDate[2] := Month;
  OraDate[3] := Day;
  OraDate[4] := Hour + 1;
  OraDate[5] := Min + 1;
  OraDate[6] := Sec + 1;
  Marshal.Copy(OraDate, 0, Buf, 7);
end;

procedure MSecsToOraDate(MSecs: double; Buf: IntPtr);
begin
  DateTimeToOraDate(MemUtils.TimeStampToDateTime(MSecsToTimeStamp(Trunc(MSecs))), Buf);
end;

{$IFDEF VER6P}
{$IFNDEF FPC}
procedure OraTimeStampToSQLTimeStamp(OraTimeStamp: TOraTimeStamp; var SQLTimeStamp: TSQLTimeStamp);
var
  Year: SmallInt;
  Month, Day, Hour, Min, Sec: byte;
  FSec: cardinal;
begin
  if OraTimeStamp.IsNull then
    SQLTimeStamp := NullSqlTimeStamp
  else begin
    OraTimeStamp.GetDate(Year, Month, Day);
    SQLTimeStamp.Year := Year;
    SQLTimeStamp.Month := Month;
    SQLTimeStamp.Day := Day;

    OraTimeStamp.GetTime(Hour, Min, Sec, FSec);
    SQLTimeStamp.Hour := Hour;
    SQLTimeStamp.Minute := Min;
    SQLTimeStamp.Second := Sec;
    SQLTimeStamp.Fractions := FSec div 1000000;
  end;
end;

procedure SQLTimeStampToOraTimeStamp(OraTimeStamp: TOraTimeStamp; const SQLTimeStamp: TSQLTimeStamp);
begin
  if (SQLTimeStamp.Year = 0) and (SQLTimeStamp.Month = 0) and (SQLTimeStamp.Day = 0)
    and (SQLTimeStamp.Hour = 0) and (SQLTimeStamp.Minute = 0) and (SQLTimeStamp.Second = 0)
    and (SQLTimeStamp.Fractions = 0)
  then
    OraTimeStamp.IsNull := True
  else
    OraTimeStamp.Construct(SQLTimeStamp.Year, SQLTimeStamp.Month, SQLTimeStamp.Day,
      SQLTimeStamp.Hour, SQLTimeStamp.Minute, SQLTimeStamp.Second, SQLTimeStamp.Fractions * 1000000, '');
end;
{$ENDIF}
{$ENDIF}

procedure GetPrecAndScale(const Value: string; var Precision, Scale: integer);
var
  P: integer;
begin
  P := Pos(',', Value);
  if P > 0 then begin
    Precision := StrToInt(Trim(Copy(Value, 1, P - 1)));
    Scale := StrToInt(Trim(Copy(Value, P + 1, Integer(Length(Value) - P))));
  end
  else begin
    Precision := StrToInt(Trim(Value));
    Scale := 0;
  end;
end;

{ TOCIConnection }

constructor TOCIConnection.Create;
begin
  inherited Create;

  FAutoCommit := True;
  FThreadSafety := True;
  FOCICallStyle := None;
  FEnableIntegers := True;
  FEnableWideOraClob := EnableWideOraClob;
  FCharLength := 1;
  FInternalName := '';

  FSmallintPrecision := SmallintPrecision;
  FIntegerPrecision := IntegerPrecision;
  FLargeIntPrecision := LargeIntPrecision;
  FFloatPrecision := FloatPrecision;
  GetPrecAndScale(BCDPrecision, FBCDPrecision, FBCDScale);
{$IFDEF VER6P}
{$IFNDEF FPC}
  GetPrecAndScale(FmtBCDPrecision, FFmtBCDPrecision, FFmtBCDScale);
{$ENDIF}
{$ENDIF}
  FUnicodeAsNational := OCIUnicodeAsNational;

  FNlsParams[nlsDateLanguage].Name := 'NLS_DATE_LANGUAGE';
  FNlsParams[nlsDateFormat].Name := 'NLS_DATE_FORMAT';
  FNlsParams[nlsTimeStampFormat].Name := 'NLS_TIMESTAMP_FORMAT';
  FNlsParams[nlsTimeStampTZFormat].Name := 'NLS_TIMESTAMP_TZ_FORMAT';
  FNlsParams[nlsNumericCharacters].Name := 'NLS_NUMERIC_CHARACTERS';

{$IFDEF MSWINDOWS}
  hBusy := CreateMutex(nil, False, '');
{$ENDIF}
end;

destructor TOCIConnection.Destroy;
begin
  Disconnect;

  FCommand.Free;

{$IFDEF MSWINDOWS}
  CloseHandle(hBusy);
{$ENDIF}

  inherited;
end;

function TOCIConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TOCICommand;
end;

function TOCIConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TOCITransaction;
end;

procedure TOCIConnection.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure TOCIConnection.DoError(E: Exception; var Fail: boolean);
begin
  inherited; //for D8
end;

procedure TOCIConnection.CheckOCI;
begin
  if not ((FOCICallStyle = OCI73) or (FOCICallStyle = OCI80)) then
    RaiseError(SCheckOCI);
end;

procedure TOCIConnection.CheckOCI73;
begin
  if not (FOCICallStyle = OCI73) then
    RaiseError(SCheckOCI73);
end;

procedure TOCIConnection.CheckOCI80;
begin
  if not (FOCICallStyle = OCI80) then
    RaiseError(SCheckOCI80);
end;

procedure TOCIConnection.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    OraError(FOCICallStyle, Status, True, Component);
end;

procedure TOCIConnection.OraError(FOCICallStyle: TOCICallStyle;
  ErrorCode: sword; UseCallback: boolean; Component: TObject);
const
  MsgLen = 512;
var
  MsgBuf: IntPtr;
  Msg: _string;
  Code: integer;
  Fail, NeedFreeError: boolean;
  Error: EOraError;
begin
  Code := 0;
  if FOCICallStyle = OCI73 then begin
    Code := ErrorCode;
    if Code = -9 then
      Code := -1;
    MsgBuf := Marshal.AllocHGlobal(MsgLen + 1);
    try
      oerhms(LDA, Code, MsgBuf, MsgLen);
      Msg := _string(Marshal.PtrToStringAnsi(MsgBuf));
    finally
      Marshal.FreeHGlobal(MsgBuf);
    end;
    Code := Abs(Code);
  end
  else
  if FOCICallStyle = OCI80 then begin
    Code := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.GetOraError(ErrorCode, FUnicodeEnv, Msg, hOCIError);
  end
  else
    CheckOCI;

  FLastError := Code;

  NeedFreeError := True;
  Error := EOraError.Create(Code, Msg);
  try
  {$IFNDEF LITE}
    Error.Component := Component;
  {$ENDIF}

    if (FOCICallStyle = OCI80) and (ErrorCode = OCI_SUCCESS_WITH_INFO)
      and (Code <> 24344) // except compilation error
    then begin
      if Assigned(FOnInfoMessage) then
        FOnInfoMessage(Error);
      exit;
    end;

    Fail := True;
    if UseCallback then
      DoError(Error, Fail);
    if Fail then
      NeedFreeError := False;
  finally
    if NeedFreeError then
      Error.Free;
  end;

  if Fail then
    raise Error
  else
    Abort;
end;

function OCICallbackFailover(svchp: IntPtr; envhp: IntPtr; fo_ctx: IntPtr; fo_type: ub4; fo_event: ub4): sb4;{$IFNDEF CLR} cdecl; {$ENDIF}
var
  OCIConnection: TOCIConnection;
  Retry: boolean;
  i: TNlsParamType;
begin
  OCIConnection := TOCIConnection(GetGCHandleTarget(fo_ctx));
  Assert(OCIConnection <> nil);
  Result := 0;
  if fo_event = OCI_FO_END then begin
      // apply user NLS settings
    with OCIConnection do begin
      for i := Low(FNlsParams) to High(FNlsParams) do
        if FNlsParams[i].IsUserDefined then
          SetNlsParameter(FNlsParams[i].Name, FNlsParams[i].Value);
    end;
  end;

  if Assigned(OCIConnection.FOnFailover) then begin
    Retry := False;
    OCIConnection.FOnFailover(fo_event, fo_type, Retry);
    if Retry then
      Result := OCI_FO_RETRY;
  end;
end;

procedure TOCIConnection.SetConnectionType(ConnectionType: TConnectionType);
begin
  Assert(not FConnected);
  FConnectionType := ConnectionType;
end;

procedure TOCIConnection.SetStatementCacheSize(Size: integer);
begin
  if (FStatementCache) and (OCI73 in PossibleOCICallStyles) and
   (FOCICallStyle = OCI80) and (OCIVersion > 9200)
  then begin
    CheckOCI80;
    Check(OCIAttrSet2(hSvcCtx, OCI_HTYPE_SVCCTX, Size, 4, OCI_ATTR_STMTCACHESIZE, hOCIError));
  end; //TODO: else Raise Error Statemet caching supported sice version 9.2
end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
procedure TOCIConnection.MTSCheck(Status: sword);
begin
  if Status <> ORAMTSERR_NOERROR then
    MTSError(Status, True);
end;

procedure TOCIConnection.MTSError(var ErrorCode: sword; UseCallback: boolean);
var
  Msg: _string;
  Fail: boolean;
begin
  ErrorCode := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.GetMTSError(ErrorCode, FUnicodeEnv, Msg, hOCIError);

  try
    raise EOraError.Create(ErrorCode, Msg);
  except
    on E: EOraError do begin
      Fail := True;
      if UseCallback then
        DoError(E, Fail);
      if Fail then
        raise
      else
        Abort;
    end;
  end;
end;

procedure TOCIConnection.Enlist(Transaction: TMTSTransaction);
var
  Value: variant;
begin
  Transaction.GetProp(prTransactionReadOnly, Value);
  if Value then
    raise Exception.Create(SReadOnlyNotSupportedWithMTS);

  Transaction.GetProp(prIsolationLevel, Value);
  if TCRIsolationLevel(Value) <> ilReadCommitted then
    raise Exception.Create(SIsolationLevelNotSupportedWithMTS);

  InitMTS;
  Assert(hMTSSvcCtx = nil);

  if FConnectionType = ctMTSPooled then begin
    hMTSSvcCtx := hSvcCtx;
    MTSCheck(OraMTSSvcEnlist(hMTSSvcCtx, hOCIError, Transaction.MTSTransaction, ORAMTS_ENFLG_DEFAULT));
  end
  else begin
    MTSCheck(OraMTSEnlCtxGet(PAnsiChar(AnsiString(FUserName)), PAnsiChar(AnsiString(FPassword)),
      PAnsiChar(AnsiString(FServer)), hSvcCtx, hOCIError, OCI_DEFAULT, hMTSSvcCtx));
    MTSCheck(OraMTSJoinTxn(hMTSSvcCtx, Transaction.MTSTransaction));
  end;
end;

procedure TOCIConnection.UnEnlist(Transaction: TMTSTransaction);
begin
  if hMTSSvcCtx = nil then
    exit;

  if FConnectionType = ctMTSPooled then
    MTSCheck(OraMTSSvcEnlist(hMTSSvcCtx, hOCIError, nil, ORAMTS_ENFLG_DEFAULT))
  else begin
    MTSCheck(OraMTSJoinTxn(hMTSSvcCtx, nil));
    MTSCheck(OraMTSEnlCtxRel(hMTSSvcCtx));
  end;

  hMTSSvcCtx := nil;
end;
{$ENDIF}
{$ENDIF}

procedure TOCIConnection.SetupEnvironment;
begin
//This function checks if current call layer settings are differs from session's
//and reset call layer if it is necessary
{$IFNDEF LITE}
  if (hOCIEnv <> nil) and (OCIUnicode <> IsUnicodeEnv(hOCIEnv, hOCIError)) then
    FreeOCI;
{$ELSE}
  if OCIUnicode <> FUseUnicode and FUnicodeEnv then
    FreeOCI;
  if not OCIInited then
    OCIUnicode := FUseUnicode and FUnicodeEnv;
{$ENDIF}
{$IFDEF NET}
  if (FDirect <> (PossibleOCICallStyles = [OCI80])) then begin
    FreeOCI;
    if FDirect then begin
      FOCICallStyle := OCI80;
      InitNet;
    end
    else begin
      FreeNet;
    end;
  end;
{$ENDIF}
  if not FDirect and (FHomeName <> {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OracleHomeName) then begin
    FreeOCI;
    {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OracleHomeName := FHomeName;

  end;
end;

procedure TOCIConnection.SetupConnection;
begin
  hLockConnectCount.Enter;
  try
    if (ActiveConnectionsCount > 0) then begin
      if not FDirect and (FHomeName <> {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OracleHomeName) then
        RaiseError(SHomeNameDiffers);
      if (FDirect <> (PossibleOCICallStyles = [OCI80])) then
        RaiseError(SDirectDiffers);
    {$IFNDEF LITE}
      if (hOCIEnv <> nil) and (OCIUnicode <> IsUnicodeEnv(hOCIEnv, hOCIError)) then
        RaiseError(SOCIUnicodeDiffers);
    {$ELSE}
      if OCIUnicode <> FUseUnicode and FUnicodeEnv then
        RaiseError(SOCIUnicodeDiffers);
    {$ENDIF}
    end;
    SetupEnvironment;

    Inc(ActiveConnectionsCount);
  finally
    hLockConnectCount.Leave;
  end;

  FOracleVersionFull := '';
  FOracleVersionSt := '';
  FOracleVersion := 0;

  if not OCIInited then
    OCIInit;

  if (FOCICallStyle = None) or not (FOCICallStyle in PossibleOCICallStyles) then
    FOCICallStyle := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCICallStyle;

  if FProxyConnection <> nil then
    CheckOCI80;

  FOCICallStyleCommand := FOCICallStyle;
end;

procedure TOCIConnection.Connect(const ConnectString: _string);
var
  Credt: ub4;
  Mode: ub4;
  i: TNlsParamType;
  Failover: TOCIFoCbkStruct;
  Handle: IntPtr;
  CharsetId: Integer; //ub2
  Res, Size: integer;
  p: IntPtr;


  function GetOraConnectMode(MTS: boolean): integer;
  begin
  {$IFDEF MSWINDOWS}
  {$IFNDEF LITE}
    if MTS then
      case FConnectMode of
        cmSYSOPER:
          Result := ORAMTS_CFLG_SYSOPRLOGN;
        cmSYSDBA:
          Result := ORAMTS_CFLG_SYSDBALOGN;
      else
        Result := OCI_DEFAULT;
      end
    else
  {$ENDIF}
  {$ENDIF}
      case FConnectMode of
        cmSYSOPER:
          Result := OCI_SYSOPER;
        cmSYSDBA:
          Result := OCI_SYSDBA;
        cmSysASM:
          Result := OCI_SYSASM;
      else
        Result := OCI_DEFAULT;
      end;
  end;


  procedure ConnectDefaultOCI80;
  begin
    hOCIEnv := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv;
  {$IFDEF LOCAL_ERROR_HANDLE}
    if hOCIError = nil then
      Check(OCIHandleAlloc(hOCIEnv, hOCIError, OCI_HTYPE_ERROR, 0, nil));
  {$ELSE}
    hOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError;
  {$ENDIF}
    FUnicodeEnv := IsUnicodeEnv(hOCIEnv, hOCIError);
    if FUnicodeEnv and not FUseUnicode then
      RaiseError(SUseUnicodeRequired);

    if hSvcCtx = nil then
      Check(OCIHandleAlloc(hOCIEnv, hSvcCtx, OCI_HTYPE_SVCCTX, 0, nil));
    //For child proxy session we don't need Server handle
    if (FProxyConnection = nil) and (hServer = nil) then
      Check(OCIHandleAlloc(hOCIEnv, hServer, OCI_HTYPE_SERVER, 0, nil));
    if hSession = nil then
      Check(OCIHandleAlloc(hOCIEnv, hSession, OCI_HTYPE_SESSION, 0, nil));

  {$IFNDEF LITE}
    if (FClientIdentifier <> '') and (OCIVersion >= 9000) then begin
      p := StringToHGlobalOCI(FClientIdentifier, Size, FUnicodeEnv);
      Res := OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size,
        OCI_ATTR_CLIENT_IDENTIFIER, hOCIError);
      FreeStringOCI(p, FUnicodeEnv);
      Check(Res);
    end;
  {$ENDIF}

    try
      if FProxyConnection = nil then begin
      {$IFDEF LINUX}
        hLockConnect.Enter;
        try
      {$ENDIF}
          p := StringToHGlobalOCI(FServer, Size, FUnicodeEnv);
          Res := OCIServerAttach(hServer, hOCIError, p, Size, OCI_DEFAULT);
          FreeStringOCI(p, FUnicodeEnv);
          Check(Res);
      {$IFDEF LINUX}
        finally
          hLockConnect.Leave;
        end;
      {$ENDIF}
      end;

      if (OCI73 in PossibleOCICallStyles) and (FProxyConnection = nil) then begin
        p := StringToHGlobalOCI(FInternalName, Size, FUnicodeEnv);
        Res := OCIAttrSet1(hServer, OCI_HTYPE_SERVER, p, 0, OCI_ATTR_INTERNAL_NAME, hOCIError);
        FreeStringOCI(p, FUnicodeEnv);
        Check(Res);
      end;
      try
      // Set the server context in the service context
        if FProxyConnection = nil then
          Check(OCIAttrSet1(hSvcCtx, OCI_HTYPE_SVCCTX, hServer, 0, OCI_ATTR_SERVER, hOCIError))
        else
          Check(OCIAttrSet1(hSvcCtx, OCI_HTYPE_SVCCTX, FProxyConnection.hServer, 0, OCI_ATTR_SERVER, hOCIError));

        if FProxyConnection <> nil then begin
          p := StringToHGlobalOCI(FUsername, Size, FUnicodeEnv);
          Res := OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size, OCI_ATTR_USERNAME, hOCIError);
          FreeStringOCI(p, FUnicodeEnv);
          Check(Res);

          if FPassword <> '' then begin
            p := StringToHGlobalOCI(FPassword, Size, FUnicodeEnv);
            Res := OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size, OCI_ATTR_PASSWORD, hOCIError);
            FreeStringOCI(p, FUnicodeEnv);
            Check(Res);
          end;

          Check(OCIAttrSet1(hSession, OCI_HTYPE_SESSION, FProxyConnection.hSession, 0, OCI_ATTR_PROXY_CREDENTIALS, hOCIError));
          Credt := OCI_CRED_PROXY;
        end
        else
          if FUsername <> '' then begin
          // Set username and password attribute in user session handle
            p := StringToHGlobalOCI(FUsername, Size, FUnicodeEnv);
            Res := OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size, OCI_ATTR_USERNAME, hOCIError);
            FreeStringOCI(p, FUnicodeEnv);
            Check(Res);

            p := StringToHGlobalOCI(FPassword, Size, FUnicodeEnv);
            Res := OCIAttrSet1(hSession, OCI_HTYPE_SESSION, p, Size, OCI_ATTR_PASSWORD, hOCIError);
            FreeStringOCI(p, FUnicodeEnv);
            Check(Res);

            Credt := OCI_CRED_RDBMS;
          end
          else
            Credt := OCI_CRED_EXT;

        Mode := GetOraConnectMode(False);

        ///Version > 9200
        if (FStatementCache) and (OCI73 in PossibleOCICallStyles) and
         (FOCICallStyle = OCI80) and (OCIVersion > 9200)
        then
          Mode := Mode or OCI_STMT_CACHE;

        if PossibleOCICallStyles = [OCI80] then begin
          Check(OCIAttrSet2(hSvcCtx, OCI_HTYPE_SVCCTX, FConnectionTimeout, 0, OCI_ATTR_CONNECTION_TIMEOUT, hOCIError));
          if FUseUnicode then begin
            CharsetId := Integer(OCI_UTF16ID);
            Check(OCIAttrSet2(hSvcCtx, OCI_HTYPE_SVCCTX, CharsetId, 0, OCI_ATTR_CHARSET_ID, hOCIError));
          end
          else
            if FCharset <> '' then begin
              p := StringToHGlobalOCI(FCharset, Size, FUnicodeEnv);
              Res := OCIAttrSet1(hSvcCtx, OCI_HTYPE_SVCCTX, p, 0, OCI_ATTR_CHARSET, hOCIError);
              FreeStringOCI(p, FUnicodeEnv);
              Check(Res);
            end;
        end;

      {$IFDEF LINUX}
        hLockConnect.Enter;
        try
      {$ENDIF}
          Check(OCISessionBegin(hSvcCtx, hOCIError, hSession, Credt, Mode));
      {$IFDEF LINUX}
        finally
          hLockConnect.Leave;
        end;
      {$ENDIF}

      // Set the user session in the service context
        Check(OCIAttrSet1(hSvcCtx, OCI_HTYPE_SVCCTX, hSession, 0, OCI_ATTR_SESSION, hOCIError));

        if OCI73 in PossibleOCICallStyles then
          if GetOracleVersion < 8000 then
            FOCICallStyleCommand := OCI73;
      except
        if hServer <> nil then
          Check(OCIServerDetach(hServer, hOCIError, OCI_DEFAULT));
        raise;
      end;
    except
      on EFailOver do;
      else begin
        if hSession <> nil then
          OCIHandleFree(hSession, OCI_HTYPE_SESSION);
        if hServer <> nil then
          OCIHandleFree(hServer, OCI_HTYPE_SERVER);
        if hSvcCtx <> nil then
          OCIHandleFree(hSvcCtx, OCI_HTYPE_SVCCTX);
      {$IFDEF LOCAL_ERROR_HANDLE}
        if hOCIError <> nil then
          OCIHandleFree(hOCIError, OCI_HTYPE_ERROR);
      {$ENDIF}
        hSession := nil;
        hServer := nil;
        hSvcCtx := nil;
        hOCIError := nil;
        hOCIEnv := nil;
      end;
      raise;
    end;
  end;

  procedure ConnectOCIPooled;
  var
    retTagInfo: IntPtr;
    retTagInfo_len: Cardinal;
    found: LongBool;
  begin
    CheckOCI80;
    FOCIPoolName := ConnectString;

    hOCIEnv := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv;
  {$IFDEF LOCAL_ERROR_HANDLE}
    if hOCIError = nil then
      Check(OCIHandleAlloc(hOCIEnv, hOCIError, OCI_HTYPE_ERROR, 0, nil));
  {$ELSE}
    hOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError;
  {$ENDIF}
    FUnicodeEnv := IsUnicodeEnv(hOCIEnv, hOCIError);
    if FUnicodeEnv and not FUseUnicode then
      RaiseError(SUseUnicodeRequired);

    if hOCIAuthInfo = nil then
      Check(OCIHandleAlloc(hOCIEnv, hOCIAuthInfo, OCI_HTYPE_AUTHINFO, 0, nil));

    try
      p := StringToHGlobalOCI(FUserName, Size, FUnicodeEnv);
      Res := OCIAttrSet1(hOCIAuthInfo, OCI_HTYPE_AUTHINFO, p, Size, OCI_ATTR_USERNAME, hOCIError);
      FreeStringOCI(p, FUnicodeEnv);
      Check(Res);

      p := StringToHGlobalOCI(FPassword, Size, FUnicodeEnv);
      Res := OCIAttrSet1(hOCIAuthInfo, OCI_HTYPE_AUTHINFO, p, Size, OCI_ATTR_PASSWORD, hOCIError);
      FreeStringOCI(p, FUnicodeEnv);
      Check(Res);

      Mode := OCI_SESSGET_SPOOL;
      if (FStatementCache) and (OCI73 in PossibleOCICallStyles) and
        (FOCICallStyle = OCI80) and (OCIVersion > 9200)
      then
        Mode := Mode or OCI_SESSGET_STMTCACHE;

      p := StringToHGlobalOCI(FOCIPoolName, Size, FUnicodeEnv);
      Res := OCISessionGet(hOCIEnv, hOCIError, hSvcCtx, hOCIAuthInfo, p, Size, nil, 0, RetTagInfo,
        retTagInfo_len, found, OCI_SESSGET_SPOOL);
      FreeStringOCI(p, FUnicodeEnv);
      Check(Res);
    except
      if hOCIAuthInfo <> nil then
        Check(OCIHandleFree(hOCIAuthInfo, OCI_HTYPE_AUTHINFO));

      raise;  
    end;
  end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  procedure ConnectMTSPooled;
  begin
    InitMTS;

    MTSCheck(OraMTSSvcGet(PAnsiChar(AnsiString(FUserName)), PAnsiChar(AnsiString(FPassword)),
      PAnsiChar(AnsiString(FServer)), hSvcCtx, hOCIEnv, ORAMTS_CFLG_NOIMPLICIT));

  {$IFDEF LOCAL_ERROR_HANDLE}
    if hOCIError = nil then
      Check(OCIHandleAlloc(hOCIEnv, hOCIError, OCI_HTYPE_ERROR, 0, nil));
  {$ELSE}
    hOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError;
  {$ENDIF}
    FUnicodeEnv := IsUnicodeEnv(hOCIEnv, hOCIError);
    if FUnicodeEnv and not FUseUnicode then
      RaiseError(SUseUnicodeRequired);
  end;
{$ENDIF}
{$ENDIF}

begin
  if not FConnected then begin
    try
      hOCIEnv := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv;

      SetupConnection;

      if FOCICallStyle = OCI73 then begin
      // Allocate LDA, HDA
      {$IFDEF CLR}
        if IntPtr(LDA) = nil then
          LDA := Marshal.AllocHGlobal(CDA_SIZE);
        if HDA = nil then
          HDA := Marshal.AllocHGlobal(HDA_SIZE);
      {$ELSE}
        if LDA = nil then
          New(LDA);
        if HDA = nil then
          New(HDA);
      {$ENDIF}
        FillChar(HDA, HDA_SIZE, 0);

        try
          Check(olog(LDA, HDA, PAnsiChar(AnsiString(FUsername)), -1,
            PAnsiChar(AnsiString(FPassword)), -1,
            PAnsiChar(AnsiString(FServer)), -1, OCI_LM_DEF));

          if FThreadSafety and not {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.GetThreadSafety then
            {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.SetThreadSafety(True);
        except
        {$IFNDEF CLR}
          Dispose(HDA);
          Dispose(LDA);
        {$ELSE}
          Marshal.FreeHGlobal(HDA);
          Marshal.FreeHGlobal(LDA);
        {$ENDIF}
          LDA := nil;
          HDA := nil;
          raise;
        end;
      end
      else
      if FOCICallStyle = OCI80 then begin
        case FConnectionType of
          ctDefault:
            ConnectDefaultOCI80;
          ctOCIPooled:
            ConnectOCIPooled;
        {$IFDEF MSWINDOWS}
        {$IFNDEF LITE}
          ctMTSPooled:
            ConnectMTSPooled;
        {$ENDIF}
        {$ENDIF}
        end;
      end
      else
        CheckOCI;

      FConnected := True;
    except
      InterlockedDecrement(ActiveConnectionsCount);
      raise;
    end;

    SetStatementCacheSize(FStatementCacheSize);

    // FailOver callback for child ProxyConnection
    // implemented using base ProxyConnection failover events
    if (FProxyConnection = nil) and (OCI73 in PossibleOCICallStyles) and
      (FOCICallStyle = OCI80) and (FConnectionType = ctDefault) and
      (GetOracleVersion >= 8000)
    then begin
      Failover.fo_ctx := AllocGCHandle(Self, False);
      Failover.callback_function := OCICallbackFailoverPtr;
      Handle := AllocGCHandle({$IFNDEF CLR}@{$ENDIF}Failover, True);
      try
        Check(OCIAttrSet1(hServer, OCI_HTYPE_SERVER, GetAddrOfPinnedObject(Handle), 0, OCI_ATTR_FOCBK, hOCIError));
      finally
        FreeGCHandle(Handle);
      end;
    end;

    // apply user NLS settings
    for i := Low(FNlsParams) to High(FNlsParams) do
      if FNlsParams[i].IsUserDefined then
        SetNlsParameter(FNlsParams[i].Name, FNlsParams[i].Value);

    // get session parameters if necessary
    if (FCharset <> '') or FQueryCharLength then
      GetSessionParameters;

  {$IFNDEF LITE}
    SetOptimizerMode;
    if (FClientIdentifier <> '') and (OCIVersion < 9000) then
      SetClientIdentifier;
    if FSchema <> '' then
      SetCurrentSchema(FSchema);
  {$ENDIF}

    inherited;      
  end;
end;


procedure TOCIConnection.Disconnect;
var
  i: TNlsParamType;
begin
  if FConnected then begin
    try
      if FOCICallStyle = OCI73 then begin
        FConnected := False;
        try
          if FNativeConnection then
            Check(ologof(LDA));
        finally
        // Free LDA, HDA
        {$IFDEF CLR}
          if HDA <> nil then
            Marshal.FreeHGlobal(HDA);
          if (IntPtr(LDA) <> nil) and FNativeConnection then
            Marshal.FreeHGlobal(LDA);
        {$ELSE}
          if HDA <> nil then
            Dispose(HDA);
          if (IntPtr(LDA) <> nil) and FNativeConnection then
            Dispose(LDA);
        {$ENDIF}
          HDA := nil;
          LDA := nil;
        end;
      end
      else
      if FOCICallStyle = OCI80 then begin
        FConnected := False;
        try
          case FConnectionType of
            ctDefault: begin
              if FNativeConnection then begin
              //We should leave ObjectTypes in DisconnectMode to access Objects
              //Types will be destroyed only when there are no more references on them
                if ObjectTypes <> nil then
                  ObjectTypes.ClearTypes(hSvcCtx, FDisconnectMode);
                if hOCIError <> nil then
                  Check(OCISessionEnd(hSvcCtx, hOCIError, hSession, OCI_DEFAULT));
                if hServer <> nil then
                  Check(OCIServerDetach(hServer, hOCIError, OCI_DEFAULT));
              end;
            end;
            ctOCIPooled: begin
              Check(OCISessionRelease(hSvcCtx, hOCIError, nil, 0, OCI_DEFAULT));
              hSvcCtx := nil;
            end;
          {$IFDEF MSWINDOWS}
          {$IFNDEF LITE}
            ctMTSPooled: begin
              MTSCheck(OraMTSSvcRel(hSvcCtx));
              hSvcCtx := nil;
            end;
          {$ENDIF}
          {$ENDIF}
          end;
        finally
        // Free handles
          if hOCIAuthInfo <> nil then
            OCIHandleFree(hOCIAuthInfo, OCI_HTYPE_AUTHINFO);
          if hSession <> nil then
            OCIHandleFree(hSession, OCI_HTYPE_SESSION);
          if hServer <> nil then
            OCIHandleFree(hServer, OCI_HTYPE_SERVER);
          if (hSvcCtx <> nil) and FNativeConnection then
            OCIHandleFree(hSvcCtx, OCI_HTYPE_SVCCTX);
        {$IFDEF LOCAL_ERROR_HANDLE}
          if hOCIError <> nil then
            OCIHandleFree(hOCIError, OCI_HTYPE_ERROR);
        {$ENDIF}
          hOCIAuthInfo := nil;
          hSession := nil;
          hServer := nil;
          hSvcCtx := nil;
          hOCIError := nil;
          hOCIEnv := nil;

          if (IntPtr(LDA) <> nil) and FNativeConnection then begin  // For convert to LDA
          {$IFDEF CLR}
            Marshal.FreeHGlobal(LDA);
          {$ELSE}
            Dispose(LDA);
          {$ENDIF}
            LDA := nil;
          end;
        end;
      end
      else
        CheckOCI;

    finally
      FNativeConnection := True;
      FOracleVersionFull := '';
      FOracleVersionSt := '';
      FOracleVersion := 0;
    {$IFNDEF LITE}
      FCachedSchema := '';
    {$ENDIF}

      // reset session parameters
      FMaxStringSize := 0;
      FCharsetId := 0;
      if FQueryCharLength then
        FCharLength := 0;
      for i := Low(FNlsParams) to High(FNlsParams) do
        if not FNlsParams[i].IsUserDefined then
          FNlsParams[i].Value := '';
      InterlockedDecrement(ActiveConnectionsCount);
    end;
  end;
end;

function TOCIConnection.CreateCommand: TOCICommand;
begin
  Result := TOCICommand.Create;
  Result.SetConnection(Self);
  Result.SetProp(prAutoCommit, False); // stataments that are executed with this command don't need to be commited
end;

function TOCIConnection.GetCommand: TOCICommand;
begin
  if FCommand = nil then
    FCommand := CreateCommand;

  Result := FCommand;
end;

function TOCIConnection.GetOracleVersion: word;
begin
  if FOracleVersion = 0 then
    FOracleVersion := VersionStrToWord(GetServerVersion);
  Result := FOracleVersion;
end;

function TOCIConnection.GetServerVersion: _string;

  // Extracts Oracle version number from string returned by OCIServerVersion.
  function GetVersionNumber(RawVersion: _string): _string;
  var
    Ind, Start: integer;
  begin
    Result := '';
    Ind := Pos('.', RawVersion);
    if Ind > 1 then begin
      Start := Ind - 1;
      if (Start > 1) and (RawVersion[Start - 1] >= '0') and (RawVersion[Start - 1] <= '9') then //10.0
        Start := Start - 1;
      Ind := Pos(' ', Copy(RawVersion, Start, Length(RawVersion) - (Start - 1)) );
      if Ind > 0 then
        Result := Copy(RawVersion, Start, Ind - 1);
    end;
  end;

begin
  if FOracleVersionSt = '' then begin
    if FOracleVersionFull = '' then
      GetServerVersionFull;
    if FOracleVersionSt = '' then
      FOracleVersionSt := GetVersionNumber(FOracleVersionFull);
  end;

  Result := FOracleVersionSt;
end;

function TOCIConnection.GetServerVersionFull: _string;
var
  VersionFull: IntPtr;
  BufSize: integer;
  Command: TOCICommand;
begin
  if FOracleVersionFull = '' then begin
    if not {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCILite then begin
      try
        if (FOCICallStyle = OCI73) or not (OCI73 in PossibleOCICallStyles) then begin
          Command := GetCommand;

          Command.SetSQL(
            'begin SELECT Product, Version INTO :Product, :Version ' +
            'FROM SYS.PRODUCT_COMPONENT_VERSION ' +
            'WHERE Upper(Product) LIKE ''%ORACLE%''; end;');

          with Command.Params[0] do begin
            SetDataType(dtString);
            SetParamType(pdOutput);
            SetSize(65);
          end;

          with Command.Params[1] do begin
            SetDataType(dtString);
            SetParamType(pdOutput);
            SetSize(65);
          end;

          Command.Execute;

          FOracleVersionSt := Command.Params[1].GetValue;
          FOracleVersionFull := Command.Params[0].GetValue + FOracleVersionSt;
        end
        else begin
          BufSize := 255 * SizeOfCharOCI(FUnicodeEnv);
          VersionFull := Marshal.AllocHGlobal(BufSize);
          try
            Check(OCIServerVersion(hSvcCtx, hOCIError, VersionFull,
              BufSize, OCI_HTYPE_SVCCTX)); // ORA-0 on Lite
            FOracleVersionFull := PtrToStringOCI(VersionFull, FUnicodeEnv);
          finally
            Marshal.FreeHGlobal(VersionFull);
          end;
        end;
      except
        on E: EOraError do
          if E.ErrorCode <> 6550 then
            raise;
      end;
    end
    else
      FOracleVersionFull := OCIVersionSt + ' Lite';
  end;

  Result := FOracleVersionFull;
end;

function TOCIConnection.GetClientVersion: _string;
begin
  Result := OCIVersionSt;
end;

{$IFNDEF LITE}
procedure TOCIConnection.SetCurrentSchema(SchemaName: _string);
var
  Command: TOCICommand;
begin
  if {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCILite then
    raise Exception.Create(SOLiteSchemaNotSupported);
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);
  if SchemaName = '' then
    SchemaName := GetDefaultSchema;

  Command := GetCommand;
  Command.SetSQL('ALTER SESSION SET CURRENT_SCHEMA = ' + SchemaName);
  try
    Command.Execute;
  except
    on E: EOraError do
      if E.ErrorCode = 1435 then
        raise EOraError.Create(E.ErrorCode,'Schema does not exist'+#$A, Self)
      else
        raise;
  end;
  FSchema := SchemaName;
  FCachedSchema := '';
end;

function TOCIConnection.GetCurrentSchema: _string;
var
  Command: TOCICommand;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if (GetOracleVersion < 8100) or ({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCILite) then
    if FSchema = '' then
      Result := GetDefaultSchema
    else
      Result := FSchema
  else begin
    Command := GetCommand;
    Command.SetSQL(
      'begin' +
      '  :Result := SYS_CONTEXT (''USERENV'', ''CURRENT_SCHEMA'');' +
      'end;');
    with Command.Params[0] do begin
      SetDataType(dtString);
      SetParamType(pdOutput);
      SetSize(50);
    end;
    Command.Execute;
    Result := OCISQLInfo.QuoteIfNeed(Command.Params[0].GetValue);
  end;
end;

function TOCIConnection.GetDefaultSchema: _string;
begin
  if (FConnectMode in [cmSysOper, cmSysDBA]) then
    Result := 'SYS'
  else
    Result := OCISQLInfo.NormalizeName(FUserName, False, True);
end;

function TOCIConnection.GetCachedSchema: _string;
begin
  if FCachedSchema = '' then
    if FSchema <> '' then
      FCachedSchema := OCISQLInfo.NormalizeName(FSchema, False, True)
    else
      FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;
{$ENDIF}

procedure TOCIConnection.GetTableFields(TableName: _string; Fields: _TStringList);
var
  Handle: IntPtr;
  hDescribe: pOCIDescribe;
  b1: Integer; //byte
  hParam, hColList, hCol: pOCIParam;
  NumCols: ub2;
  i, Len, BufSize: Integer;
  ValueInt: Integer;
  Ptr, ValuePtr, StrPtr: IntPtr;
  ObjType: byte;
  pName, DbLink: _string;
begin
  Fields.Clear;
  if FOCICallStyleCommand = OCI73 then
    Exit
  else
  if FOCICallStyleCommand = OCI80 then begin
    if not (OCI73 in PossibleOCICallStyles)
      or (GetOracleVersion = 8050)
    then
      Exit;
  end
  else
    CheckOCI;

  Check(OCIHandleAlloc(hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
  try
    b1 := 1;
    Check(OCIAttrSet2(hDescribe, OCI_HTYPE_DESCRIBE, b1, 0, OCI_ATTR_DESC_PUBLIC, hOCIError));
    Ptr := Marshal.AllocHGlobal(sizeof(integer));
    try
      DbLink := '';
      while True do begin
        Handle := StringToHGlobalOCI(TableName, BufSize, FUnicodeEnv);
        try
          try
            Check(OCIDescribeAny(GetSvcCtx, hOCIError, Handle, BufSize,
              OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, hDescribe));
          except
            on EOraError do // OCIDescribeAny fails on Oracle 8 if TableName contains db link
              Exit;
          end;
        finally
          FreeStringOCI(Handle, FUnicodeEnv);
        end;

        ValuePtr := OrdinalToPtr(hParam);
        try
          Check(OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil, OCI_ATTR_PARAM, hOCIError));
        finally
          PtrToOrdinal(ValuePtr, hParam);
        end;

        Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PTYPE, hOCIError));
        ObjType := Byte(ValueInt);

        case ObjType of
          OCI_PTYPE_TABLE, OCI_PTYPE_VIEW: begin
          end;
          OCI_PTYPE_SYN: begin
          // Describe synonyms
            {if d2 > 0 then
              TableName := '.' + Copy(TableName, d2 + 1, Length(TableName))
            else}
              TableName := '';
          
            StrPtr := nil;
            ValuePtr := OrdinalToPtr(StrPtr);
            try
              Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr,  OCI_ATTR_NAME, hOCIError));
            finally
              PtrToOrdinal(ValuePtr, StrPtr);
            end;
            Len := Cardinal(Marshal.ReadInt32(Ptr));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FUnicodeEnv));
            TableName := pName + TableName;

            StrPtr := nil;
            ValuePtr := OrdinalToPtr(StrPtr);
            try
              Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_SCHEMA_NAME, hOCIError));
            finally
              PtrToOrdinal(ValuePtr, StrPtr);
            end;
            Len := Cardinal(Marshal.ReadInt32(Ptr));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FUnicodeEnv));
            if pName <> '' then  // can be NULL for synonyms with db link
              TableName := pName + '.' + TableName;

            StrPtr := nil;
            ValuePtr := OrdinalToPtr(StrPtr);
            try
              Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_LINK, hOCIError));
            finally
              PtrToOrdinal(ValuePtr, StrPtr);
            end;
            Len := Cardinal(Marshal.ReadInt32(Ptr));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FUnicodeEnv));
            if pName <> '' then begin
              if DbLink <> '' then
                Exit; // Cannot use two db links
              DbLink := '@' + pName;
            end;
            TableName := TableName + DbLink;

            Continue;
          end;
        else
          RaiseOraError(4043, _Format(SObjectNotExist, [TableName]));
        end;

        Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_NUM_COLS, hOCIError));
        NumCols := ub2(ValueInt);

        ValuePtr := OrdinalToPtr(hColList);
        try
          Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil, OCI_ATTR_LIST_COLUMNS, hOCIError));
        finally
          PtrToOrdinal(ValuePtr, hColList);
        end;

        for i := 1 to NumCols do begin
          Check(OCIParamGet(hColList, OCI_DTYPE_PARAM, hOCIError, hCol, i));  // hProc
          
          try
            StrPtr := nil;
            ValuePtr := OrdinalToPtr(StrPtr);
            try
              Check(OCIAttrGet1(hCol, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_NAME, hOCIError));
            finally
              PtrToOrdinal(ValuePtr, StrPtr);
            end;
            Len := Marshal.ReadInt32(Ptr);
            Fields.Add(PtrToStringOCI(StrPtr, Len, FUnicodeEnv));
          finally
            // Free memory after OCIParamGet
            OCIDescriptorFree(hCol, OCI_DTYPE_PARAM);
          end;
        end;

        break;
      end;
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  finally
    Check(OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE));
  end;
end;

procedure TOCIConnection.GetSessionParameters;
var
  Command: TOCICommand;
  SQLText: _string;
  Param: TOraParamDesc;
  i: integer;
  CharsetIdNeeded: boolean;
  Value: variant;
begin
  if FConnected and not OCILite then begin
    Command := GetCommand;

    FCharsetId := 0;
    CharsetIdNeeded := (FCharset <> '') and (GetOracleVersion >= 8000);

    SQLText :=
      'declare' +
      '  cursor NlsParamsCursor is' +
      '    SELECT * FROM nls_session_parameters;' +
      'begin' +
         // AL32UTF8 can has one char length = 4 bytes
      '  SELECT Nvl(Lengthb(Chr(16777216)), Nvl(Lengthb(Chr(65536)), Nvl(Lengthb(Chr(256)), 1))), Nvl(Lengthb(Chr(1)), 1)' +
      '    INTO :MaxCharLength, :MinCharLength FROM dual;' +
      '  for NlsRecord in NlsParamsCursor loop' +
      '    if NlsRecord.parameter = ''NLS_DATE_LANGUAGE'' then' +
      '      :NlsDateLanguage := NlsRecord.value;' +
      '    elsif NlsRecord.parameter = ''NLS_DATE_FORMAT'' then' +
      '      :NlsDateFormat := NlsRecord.value;'+
      '    elsif NlsRecord.parameter = ''NLS_NUMERIC_CHARACTERS'' then' +
      '      :NlsNumericCharacters := NlsRecord.value;';
    if GetOracleVersion >= 8000 then
      SQLText := SQLText +
        '    elsif NlsRecord.parameter = ''NLS_TIMESTAMP_FORMAT'' then' +
        '      :NlsTimeStampFormat := NlsRecord.value;' +
        '    elsif NlsRecord.parameter = ''NLS_TIMESTAMP_TZ_FORMAT'' then' +
        '      :NlsTimeStampTZFormat := NlsRecord.value;';
    SQLText := SQLText +
      '    end if;' +
      '  end loop;';
    if CharsetIdNeeded then begin
      SQLText := SQLText +  '  SELECT NLS_CHARSET_ID';
      if GetOracleVersion >= 9000 then
        SQLText := SQLText + '(cast(:Charset as Varchar2(50)))'
      else
        SQLText := SQLText + '(:Charset)';
      SQLText := SQLText + ' INTO :CharsetId FROM dual;';
    end;  
    SQLText := SQLText + 'end;';

    Command.SetSQL(SQLText);

    // setup params
    for i := 0 to Command.Params.Count - 1 do begin
      Param := TOraParamDesc(FCommand.Params[i]);
      Param.SetDataType(dtString);
      Param.SetParamType(pdOutput);
      Param.SetSize(40);
    end;
    Command.GetParam(0).SetDataType(dtInteger);
    Command.GetParam(1).SetDataType(dtInteger);

    if CharsetIdNeeded then begin
      Command.GetParam(7).SetParamType(pdInput);
      Command.GetParam(7).SetValue(FCharset);

      Command.GetParam(8).SetDataType(dtInteger);
    end;

    try
      // execute
      Command.Execute;
    except
      // hide exceptions
    end;

    Value := FCommand.GetParam(0).GetValue;
    if VarIsNull(Value) then
      FMaxCharLength := 1
    else
      FMaxCharLength := Value;
    Value := FCommand.GetParam(1).GetValue;
    if VarIsNull(Value) then
      FMinCharLength := 1
    else
      FMinCharLength := Value;
    // get param values
    if FQueryCharLength then
      FCharLength := FMaxCharLength;

    FNlsParams[nlsDateLanguage].Value := _VarToStr(Command.GetParam(2).GetValue);
    FNlsParams[nlsDateFormat].Value := _VarToStr(Command.GetParam(3).GetValue);
    FNlsParams[nlsNumericCharacters].Value := _VarToStr(Command.GetParam(4).GetValue);

    if GetOracleVersion >= 8000 then begin
      FNlsParams[nlsTimeStampFormat].Value := _VarToStr(Command.GetParam(5).GetValue);
      FNlsParams[nlsTimeStampTZFormat].Value := _VarToStr(Command.GetParam(6).GetValue);
    end;

    if CharsetIdNeeded then begin
      Value := Command.GetParam(8).GetValue;
      if VarIsNull(Value) then
        RaiseError('Cannot find charset ' + FCharset);
      FCharsetId := Value;
    end;
  end;
end;

procedure TOCIConnection.SetNlsParameter(const Name, Value: _string);
var
  Command: TOCICommand;
begin
  if FConnected then begin
    Command := GetCommand;

    Command.SetSQL('ALTER SESSION SET '+ Name + '=' + '''' + Value + '''');
    Command.Execute;
  end;
end;

function TOCIConnection.GetMaxStringSize: word;
var
  Version: string;
  OracleVersionSt: IntPtr;
begin
  if (FMaxStringSize = 0) and FConnected then begin
    FMaxStringSize := 2000;

    if FOCICallStyle <> OCI73 then begin
      if OCI73 in PossibleOCICallStyles then begin
        if GetOracleVersion div 1000 <> 7 then
          FMaxStringSize := 4000;
      end
      else begin
        OracleVersionSt := Marshal.AllocHGlobal(8);
        try
          OCIServerVersion(hSvcCtx, hOCIError, OracleVersionSt, 8, OCI_HTYPE_SVCCTX);  // ORA-0 on Lite
          Version := string(Marshal.PtrToStringAnsi(OracleVersionSt));
        finally
          Marshal.FreeHGlobal(OracleVersionSt);
        end;
        // OCIServerVersion does not query server for it's version under NET
        if Pos('7', Version) <> 7 then
          FMaxStringSize := 4000;
      end;
    end;
  end;

  Result := FMaxStringSize;
end;

procedure TOCIConnection.BreakExec;
begin
  if FConnected then
    if FOCICallStyle = OCI73 then begin
      Check(obreak(LDA));
    //  if Res <> BREAKED then
    end
    else
    if FOCICallStyle = OCI80 then begin
      Check(OCIBreak(hSvcCtx, hOCIError));
    end
    else
      CheckOCI;
end;

procedure TOCIConnection.ChangePassword(NewPassword: _string);
var
  hSvcCtx: pOCISvcCtx;
  hServer: pOCIServer;
  hSession: pOCISession;
  CharsetId: Integer;
  Res, BufSize, Size, Size2, Size3: integer;
  p, p2, p3: IntPtr;
begin
  if not FConnected then 
    SetupConnection;

  if FConnected then begin
    hSvcCtx := Self.hSvcCtx;
    hServer := Self.hServer;
    hSession := Self.hSession;
  end
  else begin
    // Allocate handles
    hOCIEnv := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv;
    hOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError;
    FUnicodeEnv := IsUnicodeEnv(hOCIEnv, hOCIError);

    Check(OCIHandleAlloc(hOCIEnv, hSvcCtx, OCI_HTYPE_SVCCTX, 0, nil));
    Check(OCIHandleAlloc(hOCIEnv, hServer, OCI_HTYPE_SERVER, 0, nil));
    Check(OCIHandleAlloc(hOCIEnv, hSession, OCI_HTYPE_SESSION, 0, nil));
  end;

  try
    if not FConnected then begin
      p := StringToHGlobalOCI(FServer, BufSize, FUnicodeEnv);
      Res := OCIServerAttach(hServer, hOCIError, p, BufSize, OCI_DEFAULT);
      FreeStringOCI(p, FUnicodeEnv);
      Check(Res);
    end;

    try
      if not FConnected then begin
        Check(OCIAttrSet1(hSvcCtx, OCI_HTYPE_SVCCTX, hServer, 0, OCI_ATTR_SERVER, hOCIError));
        Check(OCIAttrSet1(hSvcCtx, OCI_HTYPE_SVCCTX, hSession, 0, OCI_ATTR_SESSION, hOCIError));

        if PossibleOCICallStyles = [OCI80] then begin
          Check(OCIAttrSet2(hSvcCtx, OCI_HTYPE_SVCCTX, FConnectionTimeout, 0, OCI_ATTR_CONNECTION_TIMEOUT, hOCIError));
          if FUseUnicode then begin
            CharsetId := Integer(OCI_UTF16ID);
            Check(OCIAttrSet2(hSvcCtx, OCI_HTYPE_SVCCTX, CharsetId, 0, OCI_ATTR_CHARSET_ID, hOCIError));
          end
          else
            if FCharset <> '' then begin
              p := StringToHGlobalOCI(FCharset, Size, FUnicodeEnv);
              Res := OCIAttrSet1(hSvcCtx, OCI_HTYPE_SVCCTX, p, 0, OCI_ATTR_CHARSET, hOCIError);
              FreeStringOCI(p, FUnicodeEnv);
              Check(Res);
            end;
        end;
      end;

      p := StringToHGlobalOCI(FUsername, BufSize, FUnicodeEnv);
      p2 := StringToHGlobalOCI(FPassword, Size2, FUnicodeEnv);
      p3 := StringToHGlobalOCI(NewPassword, Size3, FUnicodeEnv);
      Res := OCIPasswordChange(hSvcCtx, hOCIError, p, BufSize, p2, Size2, p3, Size3, OCI_AUTH);
      FreeStringOCI(p, FUnicodeEnv);
      FreeStringOCI(p2, FUnicodeEnv);
      FreeStringOCI(p3, FUnicodeEnv);
      Check(Res);

      if not FConnected then
        Check(OCISessionEnd(hSvcCtx, hOCIError, hSession, OCI_DEFAULT));
    finally
      if not FConnected then
        Check(OCIServerDetach(hServer, hOCIError, OCI_DEFAULT));
    end;
  finally
    if not FConnected then begin
      hOCIError := nil;
      OCIHandleFree(hSession, OCI_HTYPE_SESSION);
      OCIHandleFree(hServer, OCI_HTYPE_SERVER);
      OCIHandleFree(hSvcCtx, OCI_HTYPE_SVCCTX);
      hOCIEnv := nil;
    end;
  end;
end;

function TOCIConnection.GetLDA: PLDA;
begin
  if FOCICallStyle = OCI73 then
    Result := LDA
  else
  if FOCICallStyle = OCI80 then begin
    if IntPtr(LDA) = nil then
    {$IFDEF CLR}
      LDA := Marshal.AllocHGlobal(CDA_SIZE);
    {$ELSE}
      New(LDA);
    {$ENDIF}

 // BUG with OCI 8.1.5 raise access violation on first time when uses OCI 8.0.x initialization
    Check(OCISvcCtxToLda(hSvcCtx, hOCIError, LDA));

    Result := LDA;
  end
  else begin
    CheckOCI;
    Result := nil;
  end;
//  Check(OCIAttrGet(hSvcCtx, OCI_HTYPE_SVCCTX, @Value, nil, OCI_ATTR_IN_V8_MODE, hOCIError));
end;

procedure TOCIConnection.SetLDA(Value: PLDA);
begin
  if IntPtr(Value) <> IntPtr(LDA) then begin
    Disconnect;

    LDA := Value;

    FNativeConnection := IntPtr(LDA) = nil;
    FConnected := IntPtr(LDA) <> nil;

    if (IntPtr(LDA) <> nil) and not OCIInited then
      OCIInit;
    FOCICallStyleCommand := FOCICallStyle;

    if FOCICallStyle = OCI80 then begin
      Check(OCILdaToSvcCtx(hSvcCtx, hOCIError, LDA));
    end;
  end;
end;

function TOCIConnection.GetSvcCtx: pOCISvcCtx;
begin
  CheckOCI80;
  Result := hSvcCtx;
end;

procedure TOCIConnection.SetSvcCtx(Value: pOCISvcCtx);
var
  ValuePtr: IntPtr;
begin
  CheckOCI80;

  if Value <> hSvcCtx then begin
    Disconnect;

    hSvcCtx := Value;

    FNativeConnection := hSvcCtx = nil;
    FConnected := hSvcCtx <> nil;

    if (hSvcCtx <> nil) and not OCIInited then
      OCIInit;
    FOCICallStyleCommand := FOCICallStyle;
    hOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError;
    if Value <> nil then begin
      ValuePtr := OrdinalToPtr(hOCIEnv);
      try
        Check(OCIAttrGet1(Value, OCI_HTYPE_SVCCTX, ValuePtr, nil, OCI_ATTR_ENV, hOCIError));
      finally
        PtrToOrdinal(ValuePtr, hOCIEnv);
      end;
      FUnicodeEnv := IsUnicodeEnv(hOCIEnv, hOCIError);
    end;
  end;
end;

procedure TOCIConnection.Assign(Source: TCRConnection);
var
  Src: TOCIConnection;
begin
  inherited;

  Src := TOCIConnection(Source);
  FThreadSafety := Src.FThreadSafety;
  FConnectMode := Src.FConnectMode;
  FEnableIntegers := Src.FEnableIntegers;
  FEnableLargeint := Src.FEnableLargeint;
  FEnableNumbers := Src.FEnableNumbers;
  FEnableWideOraClob := Src.FEnableWideOraClob;
  FInternalName := Src.FInternalName;
  FConnectionTimeout := Src.FConnectionTimeout;
  FStatementCache := Src.FStatementCache;
  FStatementCacheSize := Src.FStatementCacheSize;
  FHomeName := Src.FHomeName;
  FDirect := Src.FDirect;
  FConnectionType := Src.FConnectionType;
{$IFNDEF LITE}
  FClientIdentifier := Src.FClientIdentifier;
  FOptimizerMode := Src.FOptimizerMode;
  FSchema := Src.FSchema;
{$ENDIF}
{$IFDEF VER6P}
{$IFNDEF FPC}
  FEnableSQLTimeStamp := Src.FEnableSQLTimeStamp;
{$ELSE}
  FTimeStampAsString := Src.FTimeStampAsString;
{$ENDIF}
  FIntervalAsString := Src.FIntervalAsString;
{$ENDIF}
  FSmallintPrecision := Src.FSmallintPrecision;
  FIntegerPrecision := Src.FIntegerPrecision;
  FLargeIntPrecision := Src.FLargeIntPrecision;
  FFloatPrecision := Src.FFloatPrecision;
  FBCDPrecision := Src.FBCDPrecision;
  FBCDScale := Src.FBCDScale;
{$IFDEF VER6P}
{$IFNDEF FPC}
  FFmtBCDPrecision := Src.FFmtBCDPrecision;
  FFmtBCDScale := Src.FFmtBCDScale;
{$ENDIF}
{$ENDIF}
  FCharset := Src.FCharset;
  FCharLength := Src.FCharLength;
  FQueryCharLength := Src.FQueryCharLength;
  FUseUnicode := Src.FUseUnicode;
  FNlsParams := Src.FNlsParams;
  FUnicodeAsNational := Src.FUnicodeAsNational;
end;

procedure TOCIConnection.AssignConnect(Source: TCRConnection);
var
  Src: TOCIConnection;
begin
  if Source <> Self then begin
    Disconnect;

    // it is necessary to setup environment and init OCI
    // for target connection that is located in the DLL
    SetupEnvironment;

    if not OCIInited then
      OCIInit;

    if Source <> nil then begin
      Src := TOCIConnection(Source);
      FOCICallStyle := Src.FOCICallStyle;
      if FOCICallStyle = OCI73 then begin
        SetLDA(Src.GetLDA)
      end
      else
      if FOCICallStyle = OCI80 then begin
        SetSvcCtx(Src.GetSvcCtx);
        Self.hOCIError := Src.hOCIError;
      end
      else
        CheckOCI;

      Assign(Src);

    {$IFNDEF LITE}
      FCachedSchema := Src.FCachedSchema;
    {$ENDIF}
      FCharsetId := Src.FCharsetId;
      FMaxStringSize := Src.FMaxStringSize;

      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
    end
    else begin
      SetLDA(nil);
      SetSvcCtx(nil);
      Self.hOCIError := nil;
    end;
  end;
end;

{ Multi Thread }

procedure TOCIConnection.Busy;
begin
{$IFDEF MSWINDOWS}
  if FThreadSafety then begin
    case WaitForSingleObject(hBusy, 0) of
      WAIT_OBJECT_0:
        Exit;
      WAIT_TIMEOUT:
        raise EOraError.Create(OCI_CONNECTION_BUSY,
          'ORA-03127a');
    else
      Assert(False);
    end;
  end;
{$ENDIF}
end;

procedure TOCIConnection.BusyWait;
begin
{$IFDEF MSWINDOWS}
  if FThreadSafety then begin
    case WaitForSingleObject(hBusy, INFINITE) of
      WAIT_OBJECT_0:
        Exit;
      WAIT_TIMEOUT:
        raise EOraError.Create(OCI_CONNECTION_BUSY,
          'ORA-03127a');
      WAIT_ABANDONED: begin
        Exit;
        Abort;
      end;
    else
      Assert(False);
    end;
  end;
{$ENDIF}
end;

procedure TOCIConnection.Release;
begin
{$IFDEF MSWINDOWS}
  if FThreadSafety then
    ReleaseMutex(hBusy);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
function WndProc(hWindow: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; {$IFNDEF CLR} stdcall; {$ENDIF}
var
  ExecThread: TExecThread;
  Recordset: TOCIRecordSet;
  ChangeNotification: TOCIChangeNotification;
  NotifyChange: TNotifyChange;
{$IFNDEF LITE}
  Alerter: TOCIAlerter;
  Event: TAlertEvent;
{$IFDEF CLR}
  Continue: boolean;
{$ENDIF}
  E: Exception;
{$ENDIF}
begin
  Result := 0;
  try
    case Msg of
      WM_CREATE:
        Result := 0;
      WM_ENDTHREAD: begin
        ExecThread := TExecThread(GetGCHandleTarget(IntPtr(wParam)));
        try
          if Assigned(ExecThread.FMethodDesc.EndMethod) then
            ExecThread.FMethodDesc.EndMethod(ExecThread.FException);
          if ExecThread.FException <> nil then
            if ExecThread.FException is EOraError then
              raise EOraError.Create(EOraError(ExecThread.FException).ErrorCode, ExecThread.FException.Message)
            else
              raise Exception.Create(ExecThread.FException.Message);
        finally
          ExecThread.Terminate;
          ExecThread.WaitFor;
          ExecThread.Free;
        end;
      end;
      WM_AFTERFETCH: begin
        RecordSet := TOCIRecordSet(GetGCHandleTarget(IntPtr(wParam)));
        Recordset.OnAfterFetch();
      end;
      WM_CHANGENOTIFY: begin
        ChangeNotification := TOCIChangeNotification(GetGCHandleTarget(IntPtr(wParam)));
        NotifyChange := TNotifyChange(GetGCHandleTarget(IntPtr(lParam)));
        FreeGCHandle(IntPtr(lParam));
        try
          ChangeNotification.FOnChange(NotifyChange.NotifyType, NotifyChange.TableChanges);
        finally
          NotifyChange.Free;
        end;
      end;
    {$IFNDEF LITE}
      WM_ALERTER_EVENT: begin
        Alerter := TOCIAlerter(GetGCHandleTarget(IntPtr(wParam)));
        Event := TAlertEvent(GetGCHandleTarget(IntPtr(lParam)));
        FreeGCHandle(IntPtr(lParam));
        try
          Alerter.DoOnEvent(Event.Name, Event.Message);
        finally
          Event.Free;
          Alerter.FResponseEvent.SetEvent;
        end;
      end;
      WM_ALERTER_TIMEOUT: begin
        Alerter := TOCIAlerter(GetGCHandleTarget(IntPtr(wParam)));
        try
        {$IFNDEF CLR}
          Alerter.DoOnTimeOut(Boolean(Pointer(lParam)^));
        {$ELSE}
          Continue := False;
          Alerter.DoOnTimeOut(Continue);
        {$ENDIF}
        finally
          Alerter.FResponseEvent.SetEvent;
        end;
      end;
      WM_ALERTER_STOPED: begin
        Alerter := TOCIAlerter(GetGCHandleTarget(IntPtr(wParam)));
        Alerter.Stop;

        if lParam <> 0 then begin
          E := Exception(GetGCHandleTarget(IntPtr(lParam)));
          FreeGCHandle(IntPtr(lParam));
          try
            Alerter.DoOnError(E);
          finally
            E.Free;
          end;
        end;
      end;
    {$ENDIF}
    else
      Result := DefWindowProc(hWindow, Msg, wParam, lParam);
    end;
  except
  {$IFDEF LITE}
    on E: Exception do
      MessageBox(HInstance, PChar(E.Message), 'Error', MB_OK);
  {$ELSE}
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(nil);
  {$ENDIF}
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
    lpszClassName: 'ODACUtilWnd'
  );

procedure AllocODACWnd;
var
  TempClass: {$IFDEF CLR}TWndClassInfo{$ELSE}TWndClass{$ENDIF};
  ClassRegistered: boolean;
{$IFDEF CLR}
  WndClassPtr: IntPtr;
{$ENDIF}
  WndProcPtr: IntPtr;
begin
  if hODACWindow = 0 then begin
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
    WndProcPtr := @{$IFNDEF UNIDACPRO}OraClasses{$ELSE}OraClassesUni{$ENDIF}.WndProc;
  {$ENDIF}

    if not ClassRegistered or ({$IFDEF FPC}@{$ENDIF}TempClass.lpfnWndProc <> WndProcPtr) then begin
      if ClassRegistered then
        Windows.UnregisterClass(WndClass.lpszClassName, HInstance);

      hODACWindow := Windows.RegisterClass(WndClass);
    end;

    hODACWindow := Windows.CreateWindowEx(WS_EX_TOOLWINDOW, 'ODACUtilWnd',
      '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
    Assert(hODACWindow > 0);
    Windows.SetWindowLong(hODACWindow, GWL_WNDPROC, Longint(WndProcPtr));
  end;
end;

constructor TExecThread.Create(MethodDesc: TMethodDesc; CreateSuspended: Boolean);
begin
  inherited Create(True);

  FMethodDesc := MethodDesc;
  if not CreateSuspended then
    Resume;
end;

destructor TExecThread.Destroy;
begin
  FMethodDesc.Free;
  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);
{$IFDEF VER6P}
{$IFNDEF CLR}
  FException.Free;
{$ENDIF}
{$ENDIF}

  inherited;
end;

procedure TExecThread.Execute;
begin
  FGCHandle := AllocGCHandle(Self, False);
  try
    TRunMethod(FMethodDesc.RunMethod);
    FException := nil;
    PostMessage(FMethodDesc.hWindow, WM_ENDTHREAD, DWORD(Integer(FGCHandle)), 0);
  except
    on E: Exception do begin
  {$IFDEF CLR}
      if E.ClassName <> 'ThreadAbortException' then
  {$ENDIF}
      begin
        FException := E;
        PostMessage(FMethodDesc.hWindow, WM_ENDTHREAD, DWORD(Integer(FGCHandle)), 0);
      {$IFDEF VER6P}
      {$IFNDEF CLR}
        AcquireExceptionObject;
      {$ENDIF}
      {$ELSE}
        while not Terminated do; // don't free exception until processed
      {$ENDIF}
      end;
    end;
  end;
end;

{$ENDIF}

function TOCIConnection.RunThread(RunMethod: TRunMethod; EndMethod: TEndMethod): TThread;
{$IFDEF MSWINDOWS}
var
  MethodDesc: TMethodDesc;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if hODACWindow = 0 then
    AllocODACWnd;

  MethodDesc := TMethodDesc.Create;

  MethodDesc.RunMethod := RunMethod;
  MethodDesc.EndMethod := EndMethod;
  MethodDesc.hWindow := hODACWindow;

  Result := TExecThread.Create(MethodDesc, False);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TOCIConnection.StopThread(var hThread: TThread{$IFDEF MSWINDOWS}; APeekMessage: boolean = False{$ENDIF}): boolean;
{$IFDEF WIN32_64}
var
  AMsg: TMSG;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}

{$IFDEF CLR}
  hThread := nil;
{$ELSE}
  Result := TerminateThread(hThread.Handle, 0);
  if Result = True then begin
    CloseHandle(hThread.Handle);
    hThread := nil;
  end;
  // PeekMessage is needed to avoid AV when TOCICommand or TOCIRecordSet is
  // destroying and thread is active.
  if APeekMessage then
    PeekMessage(AMsg, hODACWindow, WM_ENDTHREAD, WM_ENDTHREAD, PM_REMOVE);
{$ENDIF}
{$ENDIF}
  Result := True;
end;

procedure TOCIConnection.SetNonBlocking(Value: boolean);
begin
  if FConnected then begin
    if (FOCICallStyle = OCI73) or (FOCICallStyle = OCI80) then begin
      if Value then
        Check(onbset(GetLDA))
      else
        Check(onbclr(GetLDA));
    end
   {else
    if FOCICallStyle = OCI80 then begin
      Check(OCIAttrSet(hSvcCtx, OCI_HTYPE_SVCCTX, @Temp, 0, OCI_ATTR_NONBLOCKING_MODE, hOCIError));
      Check(OCIAttrGet(hSvcCtx, OCI_HTYPE_SVCCTX, @Temp, nil, OCI_ATTR_NONBLOCKING_MODE, hOCIError));
    end}
    else
      CheckOCI;
  end;
end;

function TOCIConnection.GetOCICallStyle: TOCICallStyle;
begin
  Result := FOCICallStyle;
end;

procedure TOCIConnection.SetOCICallStyle(Value: TOCICallStyle);
begin
  if Value <> FOCICallStyle then begin
    Disconnect;
    FOCICallStyle := Value;
  end;
end;

function TOCIConnection.GetOCICallStyleCommand: TOCICallStyle;
begin
  Result := FOCICallStyleCommand;
end;

function TOCIConnection.GetLastError: integer;
begin
  Result := FLastError;
end;

procedure TOCIConnection.SetLastError(Value: integer);
begin
  FLastError := Value;
end;

{ OCI73 }

{procedure TOCIConnection.SetAutoCommit(Value: boolean);
begin
  CheckOCI73;

  if Connected then
    if Value then
      Check(ocon(LDA))
    else
      Check(ocof(LDA));
end;}

function TOCIConnection.SetProp(Prop: integer; const Value: variant): boolean;
var
  S: _string;
  OldCharset: _string;
  OldCharsetId: word;

  procedure SetNlsParam(ParamType: TNlsParamType);
  begin
    S := Value;
    if FNlsParams[ParamType].Value <> S then begin
      FNlsParams[ParamType].Value := S;
      FNlsParams[ParamType].IsUserDefined := S <> '';
      if FNlsParams[ParamType].IsUserDefined then
        SetNlsParameter(FNlsParams[ParamType].Name, FNlsParams[ParamType].Value);
    end;
  end;

begin
  Result := True;
  case Prop of
    prThreadSafety: begin
      FThreadSafety := Boolean(Value);
    {$IFNDEF CLR}
    {$IFNDEF FPC}
      IsMultiThread := FThreadSafety;  // switch multithread memory managemant
    {$ENDIF}
    {$ENDIF}
    end;
    prConnectMode:
      FConnectMode := TConnectMode(Value);
    prEnableIntegers:
      FEnableIntegers := Boolean(Value);
    prEnableLargeint:
      FEnableLargeint := Boolean(Value);
    prEnableNumbers:
      FEnableNumbers := Boolean(Value);
    prEnableWideOraClob:
      FEnableWideOraClob := Boolean(Value);
    prInternalName:
      FInternalName := Value;
    prCharLength:
      if (FQueryCharLength and (Word(Value) <> 0))
        or (FCharLength <> Word(Value))
      then begin
        FCharLength := Word(Value);
        FMaxStringSize := 0;
        FQueryCharLength := FCharLength = 0;
        if FCharLength = 0 then
          GetSessionParameters;
      end;
    prCharset: begin
      S := Value;
      if FCharset <> S then begin
        OldCharset := FCharset;
        OldCharsetId := FCharsetId;
        FCharset := S;
        try
          GetSessionParameters;
        except
          FCharset := OldCharset;
          FCharsetId := OldCharsetId;
          raise;
        end;
      end;
    end;
    prUseUnicode:
      FUseUnicode := Boolean(Value);
    prDateLanguage:
      SetNlsParam(nlsDateLanguage);
    prDateFormat:
      SetNlsParam(nlsDateFormat);
    prTimeStampFormat:
      SetNlsParam(nlsTimeStampFormat);
    prTimeStampTZFormat:
      SetNlsParam(nlsTimeStampTZFormat);
    prNumericCharacters:
      SetNlsParam(nlsNumericCharacters);
    prDisconnectMode:
      FDisconnectMode := Boolean(Value);
    prConnectionTimeOut:
      FConnectionTimeout := Value;
    prStatementCache:
      FStatementCache := Boolean(Value);
    prStatementCacheSize: begin
      FStatementCacheSize := Value;
      if GetConnected then
        SetStatementCacheSize(FStatementCacheSize);
    end;
    prDirect: begin
      FDirect := Value;
      if FDirect then
        SetOCICallStyle(OCI80);
    end;
    prHomeName:
      FHomeName := Value;
    prUnicodeEnvironment:
      FUnicodeEnv := Value;
  {$IFNDEF LITE}
    prClientIdentifier: begin
      if Value <> FClientIdentifier then begin
        FClientIdentifier := Value;
        if GetConnected then
          SetClientIdentifier;
      end;
    end;
    prOptimizerMode: begin
      if TOptimizerMode(Value) <> FOptimizerMode then begin
        FOptimizerMode := TOptimizerMode(Value);
        if GetConnected then
          SetOptimizerMode;
      end;
    end;
    prSchema: begin
      if Value <> FSchema then begin
        FSchema := Value;
        if GetConnected then
          SetCurrentSchema(FSchema);
      end;
    end;
  {$ENDIF}
    prUseOCI7:
      if not FDirect then begin
        if Boolean(Value) then
          SetOCICallStyle(OCI73)
        else
          SetOCICallStyle({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCICallStyle);
      end;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    prEnableSQLTimeStamp:
      FEnableSQLTimeStamp := Value;
  {$ELSE}
    prTimeStampAsString:
      FTimeStampAsString := Value;
  {$ENDIF}
    prIntervalAsString:
      FIntervalAsString := Value;
  {$ENDIF}
    prSmallintPrecision:
      FSmallintPrecision := Value;
    prIntegerPrecision:
      FIntegerPrecision := Value;
    prFloatPrecision:
      FFloatPrecision := Value;
    prLargeintPrecision:
      FLargeIntPrecision := Value;
    prBCDPrecision:
      GetPrecAndScale(Value, FBCDPrecision, FBCDScale);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    prFmtBCDPrecision:
      GetPrecAndScale(Value, FFmtBCDPrecision, FFmtBCDScale);
  {$ENDIF}
  {$ENDIF}
    prUnicodeAsNational:
      FUnicodeAsNational := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCIConnection.GetProp(Prop: integer; var Value: variant): boolean;

  procedure GetNlsParam(ParamType: TNlsParamType);
  begin
    if FNlsParams[ParamType].Value = '' then
      GetSessionParameters;
    Value := FNlsParams[ParamType].Value;
  end;

begin
  Result := True;
  case Prop of
    prMaxStringSize:
      Value := GetMaxStringSize;
    prCharLength:
      if FQueryCharLength then
        Value := 0
      else
        Value := FCharLength;
    prCharset:
      Value := FCharset;
    prUseUnicode:
      Value := FUseUnicode;
    prDateFormat:
      GetNlsParam(nlsDateFormat);
    prDateLanguage:
      GetNlsParam(nlsDateLanguage);
    prTimeStampFormat:
      GetNlsParam(nlsTimeStampFormat);
    prTimeStampTZFormat:
      GetNlsParam(nlsTimeStampTZFormat);
    prNumericCharacters:
      GetNlsParam(nlsNumericCharacters);
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TOCIConnection.CheckIsValid: boolean;
begin
  if not FIsValid then
    Result := False
  else begin
    FIsValid := True;
    try
      if (PossibleOCICallStyles <> [OCI80]) and
        (OCIVersion >= 10200) and (FOracleVersion >= 10200)
      then
        Check(OCIPing(GetSvcCtx, hOCIError, OCI_DEFAULT))
      else
        FInternalTransaction.Commit;
    except
      FIsValid := False;
    end;
    Result := FIsValid;
  end;
end;

{$IFNDEF LITE}
procedure TOCIConnection.ReturnToPool;
begin
  Assert(FPool <> nil);
  FOnFailover := nil;
  inherited;
end;
{$ENDIF}

{$IFNDEF LITE}
procedure TOCIConnection.SetOptimizerMode;
var
  s: _string;
  Mode: TOptimizerMode;
  Command: TOCICommand;
begin
  if (FOptimizerMode <> omDefault) then begin
    Mode := FOptimizerMode;
    if (GetOracleVersion < 9000) and (Mode in [omFirstRows1000..omFirstRows1]) then
      Mode := omFirstRows;
    case Mode of
      omFirstRows1000:    s := 'FIRST_ROWS_1000';
      omFirstRows100:     s := 'FIRST_ROWS_100';
      omFirstRows10:      s := 'FIRST_ROWS_10';
      omFirstRows1:       s := 'FIRST_ROWS_1';
      omFirstRows:        s := 'FIRST_ROWS';
      omAllRows:          s := 'ALL_ROWS';
      omChoose:           s := 'CHOOSE';
      omRule:             s := 'RULE';
    end;

    Command := GetCommand;
    Command.SetSQL('ALTER SESSION SET OPTIMIZER_MODE = ' + s);
    Command.Execute;
  end;
end;

// used to set ClientIdentifier on active connection
procedure TOCIConnection.SetClientIdentifier;
var
  s: _string;
  Command: TOCICommand;
begin
  s := TrimLeft(FClientIdentifier);
  if (Length(s) > 0) and (s[1] = ':') then
    RaiseError(SClientIdentifierFirstCharShouldNotBeColon);

  if GetOracleVersion < 9000 then
    Exit;

  Command := GetCommand;
  Command.SetSQL('BEGIN DBMS_SESSION.SET_IDENTIFIER(:a); END;');
  Command.Params[0].SetDataType(dtString);
  Command.Params[0].SetParamType(pdInput);
  Command.Params[0].SetSize(Length(FClientIdentifier));
  Command.Params[0].Value := FClientIdentifier;
  try
    Command.Execute;
  except
    on E: EOraError do begin
      if EOraError(E).ErrorCode = 6550 then
        raise EOraError.Create( EOraError(E).ErrorCode,
          'Please execute GRANT EXECUTE ON DBMS_SESSION TO ' +
          FUsername + ' as SYS' + LineSeparator + LineSeparator + EOraError(E).Message);
      raise;
    end;
  end;
end;
{$ENDIF}

{ TOraCursor }

constructor TOraCursor.Create;
begin
  inherited Create;

  FState := csInactive;
  FOCICallStyle := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCICallStyle;
  phOCIStmt := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(phOCIStmt, nil);
end;

destructor TOraCursor.Destroy;
begin
  FreeCursor;
  Marshal.FreeHGlobal(phOCIStmt);

  inherited;
end;

procedure TOraCursor.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    {$IFDEF CLR}Devart.Odac.{$ENDIF}{$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.
    {$IFNDEF FPC}OraError{$ELSE}DoOraError{$ENDIF}(Status, FUnicodeEnv, hOCIError);
end;

procedure TOraCursor.CheckOCI;
begin
  if not ((FOCICallStyle = OCI73) or (FOCICallStyle = OCI80)) then
    RaiseError(SCheckOCI);
end;

procedure TOraCursor.CheckOCI73;
begin
  if not (FOCICallStyle = OCI73) then
    RaiseError(SCheckOCI73);
end;

procedure TOraCursor.CheckOCI80;
begin
  if not (FOCICallStyle = OCI80) then
    RaiseError(SCheckOCI80);
end;

procedure TOraCursor.AllocCursor(StatementCache: boolean = False);
var
  OCIStmt: pOCIStmt;
begin
  if FOCICallStyle = None then
    FOCICallStyle := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCICallStyle;

  if FOCICallStyle = OCI73 then begin
    if IntPtr(FCDA) = nil then begin
  {$IFDEF CLR}
    FCDA := Marshal.AllocHGlobal(CDA_SIZE);
  {$ELSE}
      GetMem(FCDA, CDA_SIZE);
  {$ENDIF}
    FillChar(FCDA, CDA_SIZE, 0);
    end;
  end
  else
  if FOCICallStyle = OCI80 then begin
    if hOCIStmt = nil then begin
      if Self.hOCIError = nil then
        Self.hOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError;
      if hOCIEnv = nil then begin
        hOCIEnv := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv;
        FUnicodeEnv := IsUnicodeEnv(hOCIEnv, hOCIError);
      end;

      FStatementCache := StatementCache;
      if not FStatementCache then begin
        Check(OCIHandleAlloc(hOCIEnv, OCIStmt, OCI_HTYPE_STMT, 0, nil));
        hOCIStmt := OCIStmt;
      end;
    end;
  end
  else
    CheckOCI;
end;

procedure TOraCursor.FreeCursor;
begin
  if IntPtr(FCDA) <> nil then begin
    CheckOCI73;
  {$IFDEF CLR}
    Marshal.FreeHGlobal(FCDA);
  {$ELSE}
    FreeMem(FCDA);
  {$ENDIF}
    FCDA := nil;
  end;

  if hOCIStmt <> nil then begin
    CheckOCI80;
    if FStatementCache then
      Check(OCIStmtRelease(hOCIStmt, hOCIError, nil, 0, OCI_DEFAULT))
    else
    if (hOCIEnv <> nil) or (PossibleOCICallStyles = [OCI80]) then
      Check(OCIHandleFree(hOCIStmt, OCI_HTYPE_STMT));
    hOCIStmt := nil;
    hOCIError := nil;
    hOCIEnv := nil;
  end;
end;

procedure TOraCursor.Disconnect;
begin
  FreeCursor;
end;

function TOraCursor.CanFetch: boolean;
begin
  Result := (State >= csExecuted) and (State < csFetched);
end;

procedure TOraCursor.DisablePrefetching;
begin
  // Turn off prefetching
  SetPrefetchRows(0);
end;

function TOraCursor.GetCDA: PCDA;
begin
  CheckOCI73;

  if IntPtr(FCDA) = nil then
    AllocCursor;

  Result := FCDA;
end;

function TOraCursor.GethOCIStmt: pOCIStmt;
begin
  Result := Marshal.ReadIntPtr(phOCIStmt);
end;

procedure TOraCursor.SethOCIStmt(Value: pOCIStmt);
begin
  Marshal.WriteIntPtr(phOCIStmt, Value);

  if (FPrefetchRows = 0) or (OCIVersion < 9000) then //disable prefetching for Oracle < 9
    DisablePrefetching
  else  
    SetPrefetchRows(FPrefetchRows);
end;

function TOraCursor.GetOCIStmt: pOCIStmt;
begin
  CheckOCI80;

  if hOCIStmt = nil then
    AllocCursor;

  Result := hOCIStmt;
end;

function TOraCursor.GetOCIStmtPtr: ppOCIStmt;
begin
  CheckOCI80;

  if hOCIStmt = nil then
    AllocCursor;

  Result := phOCIStmt;
end;

procedure TOraCursor.SetOCICallStyle(Value: TOCICallStyle);
begin
  if Value <> FOCICallStyle then begin
    FreeCursor;
    FOCICallStyle := Value;
  end;
end;

procedure TOraCursor.SetPrefetchRows(Value: integer);
var
  Buf: Integer;
begin
  if FOCICallStyle = OCI80 then
    if hOCIStmt <> nil then
      if OCIVersion div 10 <> 816 then begin
        Buf := Value;
        Check(OCIAttrSet2(hOCIStmt, OCI_HTYPE_STMT, Buf, 0, OCI_ATTR_PREFETCH_ROWS, hOCIError));
        if Buf = 0 then
          Check(OCIAttrSet2(hOCIStmt, OCI_HTYPE_STMT, Buf, 0, OCI_ATTR_PREFETCH_MEMORY, hOCIError)); // Disable prefetching
      end;
end;

{ TParam }

constructor TOraParamDesc.Create;
begin
  inherited Create;

  FActualLengthPtr := Marshal.AllocHGlobal(sizeof(integer));
  Marshal.WriteInt32(FActualLengthPtr, 0);
  FDefIndicator := Marshal.AllocHGlobal(2);
  Marshal.WriteInt16(FDefIndicator, -1);
  FIndicator := FDefIndicator;
  FLength := 1;
end;

destructor TOraParamDesc.Destroy;
begin
  FreeBuffer;
  Marshal.FreeHGlobal(FDefIndicator);
  Marshal.FreeHGlobal(FActualLengthPtr);

  inherited;
end;

{ Memory management }

function TOraParamDesc.GetActualLength: integer;
begin
  Result := Marshal.ReadInt32(FActualLengthPtr);
end;

procedure TOraParamDesc.SetActualLength(Value: integer);
begin
  Marshal.WriteInt32(FActualLengthPtr, Value);
end;

procedure TOraParamDesc.AllocBuffer;

  function CreateLob(LobDataType: integer): TOraLob;
  begin
    Result := TOraLob.Create(nil);
    if LobDataType in [dtBlob, dtOraBlob] then
      Result.LobType := ltBlob
    else if LobDataType in [dtOraClob] then
      Result.LobType := ltClob
    else if LobDataType in [dtWideOraClob, dtNClob] then
      Result.LobType := ltNClob;
  end;

var
  Obj: TSharedObject;
  i: integer;
begin
  if FBufferAllocated or (FDataType = dtUnknown) then
    exit;

  case FDataType of
    dtString, dtFixedChar:
      if FSize >= 1 then
        FValueSize := FSize + 1
      else
        FValueSize := 2;
    dtWideString, dtFixedWideChar:
      if FSize >= 1 then
        FValueSize := (FSize + 1) * SizeOf(WideChar)
      else
        FValueSize := 4;
    dtInteger, dtSmallint:
      FValueSize := sizeof(Integer);
    dtFloat:
      FValueSize := sizeof(Double);
    dtDateTime:
      FValueSize := 7;
    dtBoolean:
      FValueSize := 4; // as int
    dtCursor:
      FValueSize := 0;
    dtBlob,dtMemo,dtWideMemo,dtOraBlob,dtOraClob,dtBFile,dtCFile:
      if FLength > 1 then
        FValueSize := SizeOf(pOCILobLocator)
      else
        FValueSize := 0;
    dtObject,dtXML,dtArray,dtTable:
      FValueSize := 0;
    dtReference:
      FValueSize := 0;
    dtLabel:
      FValueSize := 0;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF},
    dtIntervalYM, dtIntervalDS:
      if FLength > 1 then
        FValueSize := SizeOf(pOCIInterval)
      else
        FValueSize := 0;
    dtBytes, dtVarBytes:
      FValueSize := FSize + SizeOf(Word);
    dtNumber, dtBCD{$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
      if FLength > 1 then
        FValueSize := OCI_NUMBER_SIZE
      else
        FValueSize := 0;
  else
    Assert(False, SUnknownDataType);
  end;

  if FValueSize > 0 then begin
    FValue := Marshal.AllocHGlobal(FValueSize * FLength);
    FillChar(FValue, FValueSize * FLength, $00);
  end;
  
  if (FLength > 1) and (FDataType in [dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
    dtIntervalYM, dtIntervalDS, dtNumber, dtBCD,
    {$IFDEF VER6P}dtLargeint, {$IFNDEF FPC}dtFMTBCD, dtSQLTimeStamp, {$ENDIF}{$ENDIF}
    dtBlob, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob])
  then
    FData := VarArrayCreate([1, FLength], varVariant);

  if FLength > 1 then begin
    FIndicator := Marshal.AllocHGlobal(sizeof(sb2)*FLength);
    FillChar(FIndicator, sizeof(sb2)*FLength, $FF);
    FTableIndicator := True;
  end
  else
    Marshal.WriteInt16(FDefIndicator, -1);

  FBufferAllocated := True;

  if FDataType in [dtBlob, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob] then  begin
    if FLength > 1 then
      for i := 1 to FLength do begin
        Obj := CreateLob(FDataType);
        SetItemAsObject(i, Obj);
      end
    else begin
      Obj := CreateLob(FDataType);
      try
        SetObject(Obj);
      finally
        Obj.Release;
      end;
    end;
  end
  else
  if FDataType in [dtBCD{$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD, dtSQLTimeStamp{$ENDIF}{$ENDIF}] then begin
    if FLength > 1 then
      for i := 1 to FLength do begin
      {$IFDEF VER6P}
      {$IFNDEF FPC}
        if FDataType = dtSQLTimeStamp then
          Obj := TOraTimeStamp.Create(FDataType)
        else
      {$ENDIF}
      {$ENDIF}
          Obj := TOraNumber.Create;
        SetItemAsObject(i, Obj);
      end
    else begin
    {$IFDEF VER6P}
    {$IFNDEF FPC}
      if FDataType = dtSQLTimeStamp then
        Obj := TOraTimeStamp.Create(FDataType)
      else
    {$ENDIF}
    {$ENDIF}
        Obj := TOraNumber.Create;
      try
        SetObject(Obj);
      finally
        Obj.Release;
      end;
    end;
  end;
end;

procedure TOraParamDesc.FreeBuffer;

  procedure FreeItems;
  var
    i: integer;
    Obj: TSharedObject;
  begin
      for i := 1 to VarArrayHighBound(FData, 1) do begin
      {$IFDEF CLR}
        Obj :=TSharedObject(FData[i]);
      {$ELSE}
        Obj :=TVarData(FData[i]).VPointer;
      {$ENDIF}
        if Obj <> nil then begin
          Assert(Obj is TSharedObject);
          Obj.Free;
        end;
      end;
    FData := Unassigned;
  end;

begin
  if DataType in [dtBCD{$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD, dtSQLTimeStamp{$ENDIF}{$ENDIF}] then
    if FLength > 1 then
      FreeItems;

  if (FValueSize > 0) and (FValue <> nil) then begin
    Marshal.FreeHGlobal(FValue);
    FValue := nil;
  end
  else
  if (FLength > 1) and
     (FDataType in [dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS, dtNumber,
                    dtBlob, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob])
  then
    FreeItems;

  if FTableIndicator then begin
    Marshal.FreeHGlobal(FIndicator);
    FTableIndicator := False;
  end;
  FIndicator := FDefIndicator;

  if GetObject <> nil then begin
    GetObject.Free;
  {$IFNDEF FPC}
    FData := Unassigned;
  {$ELSE}
    FillByte(FData, SizeOf(TVarRec), 0);
  {$ENDIF}
  end;

  ClearBindData;

  FBufferAllocated := False;
end;

procedure TOraParamDesc.ClearBindData;
begin
  FHandle := nil;
  FBindBufferSize := 0;
end;

function TOraParamDesc.ValuePtr: IntPtr;
begin
  Result := FValue;
end;

function TOraParamDesc.GetValueSize: integer;
begin
  Result := FValueSize;
end;

procedure TOraParamDesc.SetValuePtr(Buf: IntPtr);
begin
  if GetDataType in [dtOraBlob, dtOraClob, dtCursor, dtBFile, dtCFile,
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
    dtIntervalYM, dtIntervalDS, dtNumber, dtBCD
    {$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD, dtSQLTimeStamp{$ENDIF}{$ENDIF}]
  then
    RaiseError(SParamIsNotStorable);

  case GetDataType of
    dtString, dtFixedChar, dtMemo: begin
      StrLCopy(PAChar(FValue), Buf, FValueSize - 1);
      FLen := StrLen(PAChar(Buf));
      if FLen > FValueSize - 1 then
        FLen := FValueSize - 1;
    end;
    dtWideString, dtWideMemo: begin
      StrLCopyW(FValue, Buf, (FValueSize - 2) div 2);
      FLen := StrLenW(Buf);
      if FLen * 2  > FValueSize - 2 then
        FLen := (FValueSize - 2) div 2;
    end;
  else
    CopyBuffer(Buf, FValue, FValueSize);
  end;
end;

function TOraParamDesc.IndicatorPtr: IntPtr;
begin
  Result := FIndicator;
end;

procedure TOraParamDesc.SetDataType(Value: word);
begin
  if Value <> FDataType then begin
    FreeBuffer;

    if (Value in [dtString, dtFixedChar, dtMemo]) and
      (FOwner <> nil) and (FOwner.FConnection <> nil) and (FOwner.FConnection.FUnicodeEnv)
    then begin
    {$IFDEF LITE}
      FOriginalDataType := Value;
    {$ENDIF}
      case Value of
        dtString:
          FDataType := dtWideString;
        dtFixedChar:
          FDataType := dtFixedWideChar;
        dtMemo:
          FDataType := dtWideMemo;
      end;
    end
    else
      FDataType := Value;
  end;
end;

procedure TOraParamDesc.SetSize(Value: integer);
begin
  if Value <> FSize then begin
    case FDataType of
      dtString, dtFixedChar, dtWideString, dtFixedWideChar, dtBytes, dtVarBytes:
        FreeBuffer;
    end;

    FSize := Value;
  end;
end;

procedure TOraParamDesc.SetNeedTruncateString(Value: boolean);
begin
  FNeedTruncateString := Value;
end;

procedure TOraParamDesc.CheckRange(Index: integer);
begin
  if (1 > Index) or (Index > FLength) then
    raise ERangeError.Create(SInvalidIndex);
end;

procedure TOraParamDesc.ValidateParamValue;
begin
  // Oracle 8.0.5 and lower doesn't support AL16UTF16 encoding
  if OCIVersion <= 8170 then
    if FDataType in [dtWideString, dtFixedWideChar] then
        SetItemAsAnsiString(1, GetItemAsWideString(1));
end;

procedure TOraParamDesc.SyncIndicator(Connection: TOCIConnection);
var
  Ind: sb2;
  Index: integer;
  ts: TOraTimeStamp;
  int: TOraInterval;
  num: TOraNumber;
  lob: TOraLob;
begin
  if FLength > 1 then begin
    case FDataType of
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF},
      dtIntervalYM, dtIntervalDS,
      dtNumber, dtBCD, {$IFDEF VER6P}dtLargeint, {$IFNDEF FPC}dtFMTBCD, {$ENDIF}{$ENDIF}
      dtBlob, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob:
        for Index := 1 to FLength do
          case FDataType of
            dtBlob, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob: begin
              lob := TOraLob(GetItemAsObject(Index));
              if lob <> nil then begin
                lob.SetConnection(Connection);
                CopyBuffer(lob.OCILobLocatorPtr, PtrOffset(FValue, (Index - 1) * FValueSize), FValueSize);
                if lob.Size <> 0 then
                  SetIndicator(Index, 0)
                else
                  SetIndicator(Index, -1);
              end
              else
                SetIndicator(Index, -1);
            end;
            dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}: begin
              ts := TOraTimeStamp(GetItemAsObject(Index));
              if ts <> nil then begin
                Marshal.WriteInt32(FValue, (Index - 1) * FValueSize, Integer(ts.OCIDateTime));
              {$IFDEF VER6P}
              {$IFNDEF FPC}
                if FDataType = dtSQLTimeStamp then
                  ts.OCIDateTime := PtrOffset(FValue, (Index - 1) * FValueSize);
              {$ENDIF}
              {$ENDIF}
                SetIndicator(Index, sb2(Marshal.ReadInt16(ts.FIndicator)));
              end
              else
                SetIndicator(Index, -1);
            end;
            dtIntervalYM, dtIntervalDS: begin
              int := TOraInterval(GetItemAsObject(Index));
              if int <> nil then begin
                Marshal.WriteInt32(FValue, (Index - 1) * FValueSize, Integer(int.OCIInterval));
                SetIndicator(Index, sb2(Marshal.ReadInt16(int.FIndicator)));
              end
              else
                SetIndicator(Index, -1);
            end;
            dtNumber, dtBCD{$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}: begin
              num := TOraNumber(GetItemAsObject(Index));
              if num <> nil then begin
                CopyBuffer(num.OCINumberPtr, PtrOffset(FValue, (Index - 1) * FValueSize), FValueSize);
                if FDataType <> dtNumber then
                // FValue is freed on FreeBuffer.
                // For dtNumber param OCINumber buffer must exists after FreeBuffer
                // because OraNumber is used in TOraParam.ParamObject.
                  num.OCINumberPtr := PtrOffset(FValue, (Index - 1) * FValueSize);
                SetIndicator(Index, sb2(Marshal.ReadInt16(num.FIndicator)));
              end
              else
                SetIndicator(Index, -1);
            end;
          end;
    end;
  end
  else begin
    case FDataType of
      dtBlob,dtMemo,dtWideMemo,
      dtOraBlob,dtOraClob,
      dtBFile,dtCFile,
      dtObject,dtXML,dtReference,dtArray,dtTable,dtCursor,
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS,
      dtNumber, dtBCD{$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD, dtSQLTimeStamp{$ENDIF}{$ENDIF}: begin
        if VarIsArray(FData) then
          Exit;
        if {$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF} = nil then
          Ind := -1
        else
          case FDataType of
            dtBlob,dtMemo,dtWideMemo,dtOraBlob,dtOraClob:
              if TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).Size = 0 then
                Ind := -1
              else
                Ind := 0;
            dtBFile,dtCFile:
              if (TOraFile({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).FileDir = '') and (TOraFile({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).FileName = '') then
                Ind := -1
              else
                Ind := 0;
            dtObject,dtXML,dtReference,dtArray,dtTable:
              if TOraObject({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).Instance = nil then
                Ind := -1
              else
                Ind := 0;
            dtCursor:
              Ind := 0;
          else
          { $IFDEF LINUX
            Ind := 0;  // Kylix 1 anti warning
          $ENDIF}
            Exit;
          end;
      end
    else
      if (FValue = nil) then
        Ind := -1
      else
        Exit;
    end;

    SetIndicator(1, Ind)
  end;
end;

function TOraParamDesc.GetIndicator(Index: integer): smallint;
begin
  case FDataType of
    dtCursor:
      Result := 0; // strict for binding
    else begin
      CheckRange(Index);
      Result := Marshal.ReadInt16(FIndicator, (Index-1)*sizeof(sb2));
    end;
  end;
end;

procedure TOraParamDesc.SetIndicator(Index: integer; Value: smallint);
begin
  CheckRange(Index);
  Marshal.WriteInt16(FIndicator, (Index-1)*sizeof(sb2), Value);
end;

procedure TOraParamDesc.SetTable(Value: boolean);
begin
  FTable := Value;
end;

procedure TOraParamDesc.SetLength(Value: integer);
begin
  if Value <> FLength then begin
    FreeBuffer;

    FLength := Value;
  end;
end;

procedure TOraParamDesc.SetHasDefault(Value: boolean);
begin
  FHasDefault := Value;
end;

procedure TOraParamDesc.SetIsResult(Value: boolean);
begin
  FIsResult := Value;
end;

function TOraParamDesc.GetTable: boolean;
begin
  Result := FTable;
end;

function TOraParamDesc.GetLength: integer;
begin
  Result := FLength;
end;

function TOraParamDesc.GetHasDefault: boolean;
begin
  Result := FHasDefault;
end;

function TOraParamDesc.GetIsResult: boolean;
begin
  Result := FIsResult;
end;

{$IFDEF LITE}
function TOraParamDesc.GetOriginalDataType: word;
begin
  Result := FOriginalDataType;
end;
{$ENDIF}

function TOraParamDesc.GetItemAsDateTime(Index: integer): TDateTime;
var
  VPtr: IntPtr;
begin
  if not GetItemNull(Index) and (FDataType = dtDateTime) then begin
    VPtr := PtrOffset(FValue, (Index - 1) * FValueSize);
    Result := OraDateToDateTime(VPtr);
  end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsDateTime(Index: integer; Value: TDateTime);
var
  VPtr: IntPtr;
begin
  CheckRange(Index);
  FDataType := dtDateTime;
  AllocBuffer;
  VPtr := PtrOffset(FValue, (Index - 1) * FValueSize);
  DateTimeToOraDate(Value, VPtr);
  SetItemNull(Index, False);
end;

function TOraParamDesc.GetItemAsFloat(Index: integer): double;
begin
  if not GetItemNull(Index) and (FDataType = dtFloat) then begin
    Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FValue, (Index - 1)*FValueSize));
  end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsFloat(Index: integer; Value: double);
begin
  CheckRange(Index);
  FDataType := dtFloat;
  AllocBuffer;

  Marshal.WriteInt64(FValue, (Index - 1)*FValueSize, BitConverter.DoubleToInt64Bits(Value));
  SetItemNull(Index, False);
end;

function TOraParamDesc.GetItemAsInteger(Index: integer): integer;
begin
  if not GetItemNull(Index) and (FDataType = dtInteger) then
    Result := Marshal.ReadInt32(FValue, (Index - 1)*FValueSize)
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsInteger(Index: integer; Value: integer);
begin
  CheckRange(Index);
  FDataType := dtInteger;
  AllocBuffer;

  Marshal.WriteInt32(FValue, (Index - 1)*FValueSize, Value);
  SetItemNull(Index, False);
end;

function TOraParamDesc.GetItemAsCurrency(Index: integer): currency;
begin
  if not GetItemNull(Index) and (FDataType = dtBCD) then begin
    if GetLength > 1 then
      Result := StrToCurr(TOraNumber(GetItemAsObject(Index)).AsString)
    else
      Result := StrToCurr(GetAsNumber.AsString);
  end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsCurrency(Index: integer; Value: currency);
var
  OraNumber: TOraNumber;
begin
  CheckRange(Index);
  FDataType := dtBCD;
  AllocBuffer;

  if GetLength > 1 then
    OraNumber := TOraNumber(GetItemAsObject(Index))
  else
    OraNumber := GetAsNumber;

  Assert(OraNumber <> nil);
  OraNumber.AsString := CurrToStr(Value);
end;

{$IFDEF VER6P}
function TOraParamDesc.GetItemAsLargeInt(Index: integer): Int64;
begin
  if not GetItemNull(Index) and (FDataType = dtLargeint) then begin
    if GetLength > 1 then
      Result := TOraNumber(GetItemAsObject(Index)).AsLargeInt
    else
      Result := GetAsNumber.AsLargeInt;
  end
  else
    Result := 0;
end;

procedure TOraParamDesc.SetItemAsLargeInt(Index: integer; Value: Int64);
var
  OraNumber: TOraNumber;
begin
  CheckRange(Index);
  FDataType := dtLargeint;
  AllocBuffer;

  if GetLength > 1 then
    OraNumber := TOraNumber(GetItemAsObject(Index))
  else
    OraNumber := GetAsNumber;

  Assert(OraNumber <> nil);
  OraNumber.AsLargeInt := Value;
end;

{$IFNDEF FPC}
procedure TOraParamDesc.GetItemAsBcd(Index: integer; var Value: TBCD);
begin
  if not GetItemNull(Index) and (FDataType = dtFMTBCD) then begin
    if GetLength > 1 then
      Value := TOraNumber(GetItemAsObject(Index)).AsBCD
    else
      Value := GetAsNumber.AsBCD;
  end
  else
    Value := NullBcd;
end;

procedure TOraParamDesc.SetItemAsBcd(Index: integer; const Value: TBCD);
var
  OraNumber: TOraNumber;
begin
  CheckRange(Index);
  FDataType := dtFMTBCD;
  AllocBuffer;

  if GetLength > 1 then
    OraNumber := TOraNumber(GetItemAsObject(Index))
  else
    OraNumber := GetAsNumber;

  Assert(OraNumber <> nil);
  OraNumber.AsBCD := Value;
end;

procedure TOraParamDesc.GetItemAsSQLTimeStamp(Index: integer; var Value: TSQLTimeStamp);
var
  OraTS: TOraTimeStamp;
begin
  if not GetItemNull(Index) and (FDataType = dtSQLTimeStamp) then begin
    if GetLength > 1 then
      OraTS := TOraTimeStamp(GetItemAsObject(Index))
    else
      OraTS := GetAsTimeStamp;
    OraTimeStampToSQLTimeStamp(OraTS, Value);
  end
  else
    Value := NullSqlTimeStamp;
end;

procedure TOraParamDesc.SetItemAsSQLTimeStamp(Index: integer; const Value: TSQLTimeStamp);
var
  OraTS: TOraTimeStamp;
begin
  CheckRange(Index);
  FDataType := dtSQLTimeStamp;
  AllocBuffer;

  if GetLength > 1 then
    OraTS := TOraTimeStamp(GetItemAsObject(Index))
  else
    OraTS := GetAsTimeStamp;

  Assert(OraTS <> nil);
  SQLTimeStampToOraTimeStamp(OraTS, Value);
end;
{$ENDIF}
{$ENDIF}

function TOraParamDesc.GetItemAsAnsiString(Index: integer): AnsiString;
var
  VPtr: PAChar;
  Ptr: IntPtr;
  Len: integer;
begin
  if GetItemNull(Index) then begin
    Result := '';
    exit;
  end;

  case FDataType of
    dtWideString, dtFixedWideChar:
      Result := AnsiString(GetItemAsWideString(Index));
    dtString, dtFixedChar: begin
      VPtr := PAChar(PtrOffset(FValue, (Index - 1) * FValueSize));
      if FConvertEOL then begin
        Len := StrLen(VPtr);
        Ptr := Marshal.AllocHGlobal(Len * 2);
        try
          Len := AddCRString(VPtr, Ptr, Len);
          Result := Marshal.PtrToStringAnsi(Ptr, Len);
        finally
          Marshal.FreeHGlobal(Ptr);
        end;
      end
      else
        Result := Marshal.PtrToStringAnsi(VPtr);
    end;
  else
    Result := '';
  end;
end;

procedure TOraParamDesc.SetItemAsAnsiString(Index: integer; const Value: AnsiString);
var
  VPtr: IntPtr;
  SPtr: IntPtr;
  SLen: integer;
begin
  CheckRange(Index);
  if FDataType <> dtFixedChar then
    FDataType := dtString;

  SLen := Length(Value);
  if not FNeedTruncateString then
    if SLen > FSize then
      SetSize(SLen);
  AllocBuffer;

  VPtr := PtrOffset(FValue, (Index - 1) * FValueSize);
  if Value <> '' then begin
    if FNeedTruncateString then
      if SLen > FValueSize - 1 then  // null terminator
        SLen := FValueSize - 1; // truncate value
    SPtr := Marshal.StringToHGlobalAnsi(Value);
    try
      if FConvertEOL then
        SLen := RemoveCRString(SPtr, VPtr, SLen, Length(Value))
      else begin
        CopyBuffer(SPtr, VPtr, SLen);
      end;
    finally
      FreeString(SPtr);
    end;
    if (FLength = 1) or (SLen > FLen) then
      FLen := SLen;
    Marshal.WriteByte(VPtr, SLen, 0);

    SetItemNull(Index, False);
  end
  else
    SetItemNull(Index, True);
end;

function TOraParamDesc.GetItemAsWideString(Index: integer): WideString;
var
  VPtr, Ptr: IntPtr;
  Len : integer;
begin
  case FDataType of
    dtString, dtFixedChar:
      Result := WideString(GetItemAsAnsiString(Index));
    dtWideString, dtFixedWideChar: begin
      VPtr := PtrOffset(FValue, (Index - 1) * FValueSize);
      if FConvertEOL then begin
        Len := StrLenW(VPtr);
        Ptr := Marshal.AllocHGlobal(Len * 4);
        try
          Len := AddCRUnicode(VPtr, Ptr, Len);
          Result := Marshal.PtrToStringUni(Ptr, Len);
        finally
          Marshal.FreeHGlobal(Ptr);
        end;
      end
      else
        Result := Marshal.PtrToStringUni(VPtr);
    end
  else
    Result := '';
  end;
end;

procedure TOraParamDesc.SetItemAsWideString(Index: integer; const Value: WideString);
var
  VPtr: IntPtr;
  SPtr: IntPtr;
  SLen: integer;
begin
  CheckRange(Index);
  if FDataType <> dtFixedWideChar then
    FDataType := dtWideString;

  SLen := Length(Value);
  if not FNeedTruncateString then
    if SLen > FSize then
      SetSize(SLen);
  AllocBuffer;

  VPtr := PtrOffset(FValue, (Index - 1) * FValueSize);
  if Value <> '' then begin
    if FNeedTruncateString then
      if SLen * 2 > FValueSize - 2 then  // null terminator
        SLen := (FValueSize - 2) div 2; // truncate value
    SPtr := Marshal.StringToHGlobalUni(Value);
    try
      if FConvertEOL then
        SLen := RemoveCRUnicode(SPtr, VPtr, SLen, Length(Value))
      else
        CopyBuffer(SPtr, VPtr, SLen * 2);
    finally
      FreeString(SPtr);
    end;
    if (FLength = 1) or (SLen > FLen) then
      FLen := SLen;
    Marshal.WriteInt16(VPtr, SLen * 2, 0);

    SetItemNull(Index, False);
  end
  else
    SetItemNull(Index, True);
end;

function TOraParamDesc.GetItemAsBoolean(Index: integer): boolean;
begin
  if not GetItemNull(Index) and (FDataType = dtBoolean) then begin
    Result := Boolean(Marshal.ReadInt32(FValue, (Index - 1)*FValueSize));
  end
  else
    Result := False;
end;

procedure TOraParamDesc.SetItemAsBoolean(Index: integer; Value: boolean);
begin
  CheckRange(Index);
  FDataType := dtBoolean;
  AllocBuffer;
  Marshal.WriteInt32(FValue, (Index - 1) * FValueSize, Integer(Value));
  SetItemNull(Index, False);
end;

{$IFNDEF VER6P}
  {$DEFINE INTVARREF}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE INTVARREF}
{$ENDIF}

procedure TOraParamDesc.SetItemAsObject(Index: integer; Value: TSharedObject);
{$IFNDEF CLR}
var
  ItemValue: Variant;
{$ENDIF}
begin
  AllocBuffer;
{$IFDEF CLR}
  FData[Index] := Variant(Value);
{$ELSE}
  ItemValue := FData[Index];
{$IFNDEF INTVARREF}
  TVarData(ItemValue).VType := varByRef;
  TVarData(ItemValue).VPointer := Value;
{$ELSE}
  ItemValue := Integer(Value);
{$ENDIF}
  FData[Index] := ItemValue;
{$ENDIF}
end;

function TOraParamDesc.GetItemAsObject(Index: integer): TSharedObject;
var
  ItemData: Variant;
begin
  Assert(VarIsArray(FData));
  ItemData := FData[Index];
{$IFDEF CLR}
  Assert((ItemData = nil) or (ItemData is TSharedObject));
  Result := TSharedObject(ItemData);
{$ELSE}
  if VarIsNull(ItemData) or VarIsEmpty(ItemData) then
    Result := nil
  else begin
  {$IFNDEF INTVARREF}
    Assert(TVarData(ItemData).VType = varByRef);
    Result := TVarData(ItemData).VPointer;
  {$ELSE}
    Result := TSharedObject(Integer(ItemData));
  {$ENDIF}
  end;
{$ENDIF}
end;

function TOraParamDesc.GetItemAsVariant(Index: integer): variant;
var
  i, Len: word;
{$IFDEF VER6P}
{$IFNDEF FPC}
  Bcd: TBcd;
  TS: TSQLTimeStamp;
{$ENDIF}
{$ENDIF}
begin
  CheckRange(Index);

  if GetItemNull(Index) then begin
    if FDataType = dtString then
      Result := ''
    else
      Result := Null;
    Exit;
  end;

  case FDataType of
    dtBoolean:
      Result := GetItemAsBoolean(Index);
    dtString, dtFixedChar:
      Result := GetItemAsAnsiString(Index);
    dtWideString, dtFixedWideChar:
      Result := GetItemAsWideString(Index);
    dtInteger:
      Result := GetItemAsInteger(Index);
    dtFloat:
      Result := GetItemAsFloat(Index);
    dtBCD:
      Result := GetItemAsCurrency(Index);
  {$IFDEF VER6P}
    dtLargeint:
      Result := GetItemAsLargeInt(Index);
  {$IFNDEF FPC}
    dtFMTBCD: begin
      GetItemAsBcd(Index, Bcd);
      VarFMTBcdCreate(Result, Bcd);
    end;
    dtSQLTimeStamp: begin
      GetItemAsSQLTimeStamp(Index, TS);
      VarSQLTimeStampCreate(Result, TS);
    end;
  {$ENDIF}
  {$ENDIF}
    dtDateTime:
      Result := GetItemAsDateTime(Index);
    dtBlob,dtMemo,dtWideMemo:
      if TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).IsUnicode then
        Result := TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).AsWideString
      else
        Result := TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).AsString;
    dtUnknown:
      Result := Unassigned;
    dtBytes,dtVarBytes: begin
      Len := Word(Marshal.ReadInt16(FValue, (Index - 1) * FValueSize));
      Result := VarArrayCreate([0, Len - 1], varByte);
      for i := 0 to Len - 1 do begin
        Result[i] := Marshal.ReadByte(FValue, (Index - 1) * FValueSize + i + 2);
      end;
    end;
    else
      Result := Null;
  end;
end;

procedure TOraParamDesc.SetItemAsVariant(Index: integer; const Value: variant);
var
  Len: integer;
  sa: AnsiString;
  sw: WideString;
{$IFDEF CLR}
  i, lb: integer;
  Buf: TBytes;
  Handle: IntPtr;
{$ENDIF}
{$IFNDEF INTVARREF}
  ItemObject: TSharedObject;
{$ENDIF}
{$IFDEF VER6P}
  i64: int64;
{$ENDIF}
begin
  CheckRange(Index);

  if VarIsNull(Value) or VarIsEmpty(Value) then
    SetItemNull(Index, True)
  else begin
    if FDataType = dtUnknown then
      case VarType(Value) of
        varSmallint,varInteger,varByte{$IFDEF VER6P},varWord,varShortInt{$ENDIF}:
          SetDataType(dtInteger);
      {$IFDEF VER6P}
        varLongWord,varInt64:
          SetDataType(dtLargeint);
      {$ENDIF}
        varSingle,varDouble,varCurrency:
          SetDataType(dtFloat);
        varDate:
          SetDataType(dtDateTime);
        varString: begin
          SetDataType(dtString);
          sa := AnsiString(Value);
          SetSize(Length(sa));
        end;
      else
        if VarIsStr(Value) then begin
          SetDataType(dtWideString);
          sw := WideString(Value);
          SetSize(Length(sw));
        end
        else
          raise EConvertError.Create(SUnknownDataType);
      end;

    AllocBuffer;
    case FDataType of
      dtBlob, dtOraBlob, dtOraClob, dtWideOraClob, dtNClob: begin
        case VarType(Value) of
        {$IFNDEF INTVARREF}
          {$IFDEF CLR}varObject{$ELSE}varByRef{$ENDIF} : begin
          {$IFDEF CLR}
            Assert(Value is TSharedObject);
            ItemObject :=TSharedObject(Value);
          {$ELSE}
            Assert(TVarData(Value).VType = varByRef);
            ItemObject :=TVarData(Value).VPointer;
          {$ENDIF}
            SetItemAsObject(Index, ItemObject);
          end;
        {$ELSE}
          varInteger:
            SetItemAsObject(Index, TSharedObject(Integer(Value)));
        {$ENDIF}
          varString:
            TOraLob(GetItemAsObject(Index)).AsAnsiString := Value;
        {$IFNDEF CLR}varOleStr{$ELSE}varChar{$ENDIF}:
            TOraLob(GetItemAsObject(Index)).AsWideString := Value;
          varByte + varArray: begin
          {$IFDEF CLR}
            lb := VarArrayLowBound(Value, 1);
            Len := VarArrayHighBound(Value, 1) - lb + 1;
            Borland.Delphi.System.SetLength(Buf, Len);
            for i := 0 to Len - 1 do
              Buf[i] := Value[i + lb];
            Handle := AllocGCHandle(Buf, True);
            try
              TOraLob(GetItemAsObject(Index)).Write(0, Len, GetAddrOfPinnedObject(Handle));
            finally
              FreeGCHandle(Handle);
            end;
          {$ELSE}
            TOraLob(GetItemAsObject(Index)).Write(0, TVarData(Value).VArray.Bounds[0].ElementCount, TVarData(Value).VArray.Data);
          {$ENDIF}
          end;
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      end;
      dtBytes, dtVarBytes: begin
        if VarIsStr(Value) then begin
          Len := Length(Value);
          if Len > GetSize then
            Len := GetSize;
        {$IFDEF CLR}
          Buf := Encoding.Default.GetBytes(Copy(Value, 1, Len));
          Marshal.Copy(Buf, 0, PtrOffset(FValue, (Index - 1) * FValueSize + SizeOf(Word)), Length(Buf));
          Marshal.WriteByte(FValue, (Index - 1) * FValueSize + SizeOf(Word) + Length(Buf), 0);
        {$ELSE}
          StrPLCopy(PAnsiChar(FValue) + (Index - 1) * FValueSize + SizeOf(Word), AnsiString(Value), Len);
        {$ENDIF}
        end
        else begin
        {$IFDEF CLR}
          // VarArrayLowBound(Value, 1) can differ from 0
          lb := VarArrayLowBound(Value, 1);
          Len := VarArrayHighBound(Value, 1) - lb + 1;
          if Len > GetSize then
            Len := GetSize;
          Borland.Delphi.System.SetLength(Buf, Len);
          for i := 0 to Len - 1 do
            Buf[i] := Value[i + lb];
          Marshal.Copy(Buf, 0, PtrOffset(FValue, (Index - 1) * FValueSize + SizeOf(Word)), Length(Buf));
        {$ELSE}
          Len := TVarData(Value).VArray.Bounds[0].ElementCount;
          if Len > GetSize then
            Len := GetSize;
          Move(TVarData(Value).VArray.Data^, (PAnsiChar(FValue) + (Index - 1) * FValueSize + SizeOf(Word))^, Len);
        {$ENDIF}
        end;
        Marshal.WriteInt16(FValue, (Index - 1) * FValueSize, Len);
        SetItemNull(Index, Len = 0);
      end;
      dtString, dtFixedChar:
        SetItemAsAnsiString(Index, AnsiString(Value));
      dtWideString, dtFixedWideChar:
        SetItemAsWideString(Index, WideString(Value));
      dtInteger, dtSmallint:
        case VarType(Value) of
          varSmallint,varInteger,varByte,{$IFDEF VER6P}varWord,varLongWord,varShortInt,varInt64,{$ENDIF}
          varSingle,varDouble,varCurrency:
            SetItemAsInteger(Index, Value);
        {$IFNDEF VER6P}
          $E: // VT_DECIMAL - this type of variant is returned by TLargeintField.
            SetItemAsInteger(Index, PInt64(@TVarData(Value).VInteger)^);
        {$ENDIF}
        else
          if VarIsStr(Value) then
            SetItemAsInteger(Index, StrToInt(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtFloat:
        case VarType(Value) of
          varSmallint,varInteger,varByte,{$IFDEF VER6P}varWord,varLongWord,varShortInt,varInt64,{$ENDIF}
          varSingle,varDouble,varCurrency:
            SetItemAsFloat(Index, Value);
        {$IFNDEF VER6P}
          $E: // VT_DECIMAL
            SetItemAsFloat(Index, PInt64(@TVarData(Value).VInteger)^);
        {$ENDIF}
        else
          if VarIsStr(Value) then
            SetItemAsFloat(Index, StrToFloat(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtBCD:
        case VarType(Value) of
          varSmallint,varInteger,varByte,{$IFDEF VER6P}varWord,varLongWord,varShortInt,varInt64,{$ENDIF}
          varSingle,varDouble,varCurrency:
            SetItemAsCurrency(Index, Value);
        {$IFNDEF VER6P}
          $E: // VT_DECIMAL
            SetItemAsCurrency(Index, PInt64(@TVarData(Value).VInteger)^);
        {$ENDIF}
        else
          if VarIsStr(Value) then
            SetItemAsCurrency(Index, StrToCurr(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
    {$IFDEF VER6P}
      dtLargeint:
        case VarType(Value) of
          varSmallint,varInteger,varByte,{$IFDEF VER6P}varWord,varLongWord,varShortInt,varInt64,{$ENDIF}
          varSingle,varDouble,varCurrency:
            SetItemAsLargeInt(Index, Value);
        else
          if VarIsStr(Value) then
            SetItemAsLargeInt(Index, StrToInt64(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
    {$IFNDEF FPC}
      dtFMTBCD:
        // VarToBcd works correctly only for VarFMTBcd
        case VarType(Value) of
          varSmallint, varInteger, varByte, varWord, varShortInt:
            SetItemAsBcd(Index, IntegerToBcd(Value));
          varLongWord, varInt64: begin
            i64 := Value;
            SetItemAsBcd(Index, StrToBcd(IntToStr(i64)));
          end;
          varSingle, varDouble, varCurrency:
            SetItemAsBcd(Index, DoubleToBcd(Value));
        else
          if VarIsStr(Value) then
            SetItemAsBcd(Index, StrToBcd(Value))
          else
          if VarType(Value) = VarFMTBcd then
            SetItemAsBcd(Index, VarToBcd(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtSQLTimeStamp:
        SetItemAsSQLTimeStamp(Index, VarToSQLTimeStamp(Value));
    {$ENDIF}
    {$ENDIF}
      dtDateTime:
        case VarType(Value) of
          varDate:
            SetItemAsDateTime(Index, Value);
          varDouble, varSingle, varCurrency:
            SetItemAsDateTime(Index, Double(Value));
        else
          if VarIsStr(Value) then
            SetItemAsDateTime(Index, StrToDateTime(Value))
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      dtBoolean:
        case VarType(Value) of
          varBoolean:
            SetItemAsBoolean(Index, Value);
          varSmallint,varInteger,varByte,{$IFDEF VER6P}varWord,varLongWord,varShortInt,varInt64,{$ENDIF}
          varSingle,varDouble,varCurrency:
            SetItemAsBoolean(Index, Value = 0);
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS, dtNumber:
        case VarType(Value) of
        {$IFNDEF INTVARREF}
          {$IFDEF CLR}varObject{$ELSE}varByRef{$ENDIF} : begin
          {$IFDEF CLR}
            Assert(Value is TSharedObject);
            ItemObject :=TSharedObject(Value);
          {$ELSE}
            Assert(TVarData(Value).VType = varByRef);
            ItemObject :=TVarData(Value).VPointer;
          {$ENDIF}
            SetItemAsObject(Index, ItemObject);
          end;
        {$ELSE}
          varInteger:
            SetItemAsObject(Index, TSharedObject(Integer(Value)));
        {$ENDIF}
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
    else
      raise EConvertError.Create(SCannotConvertType);
    end;
  end;
end;

function TOraParamDesc.GetValue: variant;
var
  i: integer;
begin
  if FLength > 1 then begin
    Result := VarArrayCreate([1, FLength], varVariant);
    for i := 1 to FLength do
      Result[i] := GetItemAsVariant(i);
  end
  else
    Result := GetItemAsVariant(1);
end;

procedure TOraParamDesc.SetValue(const Value: variant);
var
  i: integer;
begin
  if (FDataType in [dtBytes, dtVarBytes]) and
    ((VarType(Value) = varArray or varByte) or VarIsStr(Value))
  then
    SetItemAsVariant(1, Value)
  else
  if VarIsArray(Value) then begin
    FLen := 0; 
    SetLength(VarArrayHighBound(Value, 1));
    for i := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
      SetItemAsVariant(i, Value[i])
  end
  else
    SetItemAsVariant(1, Value);
end;

function TOraParamDesc.GetObject: TSharedObject;
begin
{$IFDEF CLR}
  if FData is TSharedObject then
    Result := TSharedObject(FData)
{$ELSE}
  if VarType(FData) = varByRef{$IFDEF FPC} or varVariant{$ENDIF} then
    Result := TSharedObject(TVarData(FData).VPointer)
{$ENDIF}
  else
    Result := nil;
end;

procedure TOraParamDesc.SetObject(Value: TSharedObject);
begin
  if Value = GetObject then
    Exit;

  FreeBuffer;

{$IFDEF CLR}
  FData := Variant(Value);
{$ELSE}
  TVarData(FData).VType := varByRef{$IFDEF FPC} or varVariant{$ENDIF};
  TVarData(FData).VPointer := Value;
{$ENDIF}

  if Value <> nil then begin
    Value.AddRef;
    case FDataType of
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}:
        FIndicator := TOraTimeStamp(Value).FIndicator;
      dtIntervalYM, dtIntervalDS:
        FIndicator := TOraInterval(Value).FIndicator;
      dtNumber, dtBCD{$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
        FIndicator := TOraNumber(Value).FIndicator;
    end;
  end;

  FBufferAllocated := True;
end;

function TOraParamDesc.GetAsBlobRef: TBlob;
begin
  Result := TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF});
end;

function TOraParamDesc.GetAsCursor: TOraCursor;
begin
  Result := TOraCursor({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF});
end;

function TOraParamDesc.GetAsOraBlob: TOraLob;
begin
  Result := TOraLob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF});
end;

function TOraParamDesc.GetAsBFile: TOraFile;
begin
  Result := TOraFile({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF});
end;

function TOraParamDesc.GetAsTimeStamp: TOraTimeStamp;
begin
  Result := TOraTimeStamp({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF});
end;

function TOraParamDesc.GetAsInterval: TOraInterval;
begin
  Result := TOraInterval({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF});
end;

function TOraParamDesc.GetAsNumber: TOraNumber;
begin
  Result := TOraNumber({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF});
end;

function TOraParamDesc.GetNull: boolean;
begin
  Result := GetItemNull(1);
end;

procedure TOraParamDesc.SetNull(const Value: boolean);
begin
  SetItemNull(1, Value);
end;

function TOraParamDesc.GetItemNull(Index: integer): boolean;
begin
  Result := GetIndicator(Index) < 0;
end;

procedure TOraParamDesc.SetItemNull(Index: integer; Value: boolean);
var
  VPtr: IntPtr;
begin
  AllocBuffer;

  if Value then begin
    SetIndicator(Index, -1);
    case FDataType of
      dtString, dtFixedChar:
        Marshal.WriteByte(FValue, (Index - 1) * FValueSize, 0);
      dtWideString, dtFixedWideChar:
        Marshal.WriteInt16(FValue, (Index - 1) * FValueSize, 0);
      dtInteger:
        Marshal.WriteInt32(FValue, (Index - 1) * FValueSize, 0);
      dtFloat:
        Marshal.WriteInt64(FValue, (Index - 1) * FValueSize, 0);
      dtDateTime: begin
        VPtr := PtrOffset(FValue, (Index - 1) * FValueSize);
        FillChar(VPtr, FValueSize, 0);
      end;
      dtBlob,dtMemo,dtWideMemo,dtOraBlob,dtOraClob:
        TBlob({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).Clear;
      dtBytes, dtVarBytes:
        Marshal.WriteInt16(FValue, (Index - 1)*FValueSize, 0);
      dtObject,dtXML,dtReference,dtArray,dtTable:
        TOraObject({$IFDEF CLR}FData{$ELSE}TVarData(FData).VPointer{$ENDIF}).IsNull := True;
    end;
  end
  else
    SetIndicator(Index, 0);
end;

{$IFDEF MSWINDOWS}
{ TCustomNotifyChanges }

constructor TCustomNotifyChanges.Create(OCIColl: pOCIColl);
var
  Count, i, Exists: integer;
  Ind, Elem: IntPtr;
begin
  inherited Create;

  Check(OCICollSize(hOCIEnv, hOCIError, OCIColl, Count));

  SetLength(FItems, Count);

  for i := 0 to Count - 1 do begin
    Check(OCICollGetElem(hOCIEnv, hOCIError, OCIColl, i, Exists, Elem, Ind));

    FItems[i] := CreateItem(Marshal.ReadIntPtr(Elem));
  end;
end;

destructor TCustomNotifyChanges.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FItems) - 1 do
    FItems[i].Free;

  inherited Destroy;
end;

function TCustomNotifyChanges.GetCount: integer;
begin
  Result := Length(FItems);
end;

{ TNotifyRowChange }

constructor TNotifyRowChange.Create(ChangeDescriptor: IntPtr);
var
  Len, Flags: integer;
  Ptr, ValuePtr, StrPtr: IntPtr;
begin
  inherited Create;
  
  StrPtr := nil;
  ValuePtr := OrdinalToPtr(StrPtr);
  try
    Ptr := OrdinalToPtr(Len);
    try
      Check(OCIAttrGet1(ChangeDescriptor, OCI_DTYPE_ROW_CHDES, ValuePtr, Ptr,
        OCI_ATTR_CHDES_ROW_ROWID, hOCIError));
    finally
      PtrToOrdinal(Ptr, Len);
    end;
  finally
    PtrToOrdinal(ValuePtr, StrPtr);
  end;
  FRowId := PtrToStringOCI(StrPtr, Len, OCIUnicode);

  Check(OCIAttrGet2(ChangeDescriptor, OCI_DTYPE_ROW_CHDES, Flags, nil,
    OCI_ATTR_CHDES_ROW_OPFLAGS, hOCIError));

  FOperations := [];
  if Flags and OCI_OPCODE_INSERT <> 0 then
    Include(FOperations, cnoInsert);
  if Flags and OCI_OPCODE_UPDATE <> 0 then
    Include(FOperations, cnoUpdate);
  if Flags and OCI_OPCODE_DELETE <> 0 then
    Include(FOperations, cnoDelete);
end;

{ TNotifyRowChanges }

function TNotifyRowChanges.CreateItem(ChangeDescriptor: IntPtr): TObject;
begin
  Result := TNotifyRowChange.Create(ChangeDescriptor);
end;

function TNotifyRowChanges.GetChanges(Index: integer): TNotifyRowChange;
begin
  Result := TNotifyRowChange(FItems[Index]);
end;

{ TNotifyTableChange }

constructor TNotifyTableChange.Create(ChangeDescriptor: IntPtr);
var
  Len, Flags, OCIRowChanges: integer;
  Ptr, ValuePtr, StrPtr: IntPtr;
begin
  inherited Create;
  
  StrPtr := nil;
  ValuePtr := OrdinalToPtr(StrPtr);
  try
    Ptr := OrdinalToPtr(Len);
    try
      Check(OCIAttrGet1(ChangeDescriptor, OCI_DTYPE_TABLE_CHDES, ValuePtr, Ptr,
        OCI_ATTR_CHDES_TABLE_NAME, hOCIError));
    finally
      PtrToOrdinal(Ptr, Len);
    end;
  finally
    PtrToOrdinal(ValuePtr, StrPtr);
  end;
  FTableName := PtrToStringOCI(StrPtr, Len, OCIUnicode);

  Check(OCIAttrGet2(ChangeDescriptor, OCI_DTYPE_TABLE_CHDES, Flags, nil,
    OCI_ATTR_CHDES_TABLE_OPFLAGS, hOCIError));

  FOperations := [];
  if Flags and OCI_OPCODE_INSERT <> 0 then
    Include(FOperations, cnoInsert);
  if Flags and OCI_OPCODE_UPDATE <> 0 then
    Include(FOperations, cnoUpdate);
  if Flags and OCI_OPCODE_DELETE <> 0 then
    Include(FOperations, cnoDelete);
  if Flags and OCI_OPCODE_ALLROWS <> 0 then
    Include(FOperations, cnoAllRows);
  if Flags and OCI_OPCODE_ALTER <> 0 then
    Include(FOperations, cnoAlter);
  if Flags and OCI_OPCODE_DROP <> 0 then
    Include(FOperations, cnoDrop);

  if not (cnoAllRows in FOperations) then begin
    Check(OCIAttrGet2(ChangeDescriptor, OCI_DTYPE_TABLE_CHDES, OCIRowChanges, nil,
      OCI_ATTR_CHDES_TABLE_ROW_CHANGES, hOCIError));

    FRowChanges := TNotifyRowChanges.Create(pOCIColl(OCIRowChanges));
  end
  else
    FRowChanges := nil;
end;

destructor TNotifyTableChange.Destroy;
begin
  FRowChanges.Free;

  inherited Destroy;
end;

{ TNotifyTableChanges }

function TNotifyTableChanges.CreateItem(ChangeDescriptor: IntPtr): TObject;
begin
  Result := TNotifyTableChange.Create(ChangeDescriptor);
end;

function TNotifyTableChanges.GetChanges(Index: integer): TNotifyTableChange;
begin
  Result := TNotifyTableChange(FItems[Index]);
end;

{ TNotifyChange }

constructor TNotifyChange.Create(ChangeDescriptor: IntPtr);
var
  OCINotifyType, OCITableChanges: integer;
begin
  inherited Create;

  // Get the Notification Type
  Check(OCIAttrGet2(ChangeDescriptor, OCI_DTYPE_CHDES, OCINotifyType, nil,
    OCI_ATTR_CHDES_NFYTYPE, hOCIError));
  case OCINotifyType of
    OCI_EVENT_NONE:
      FNotifyType := cneNone;
    OCI_EVENT_STARTUP:
      FNotifyType := cneStartup;
    OCI_EVENT_SHUTDOWN:
      FNotifyType := cneShutdown;
    OCI_EVENT_SHUTDOWN_ANY:
      FNotifyType := cneShutdownAny;
    OCI_EVENT_DROP_DB:
      FNotifyType := cneDropDB;
    OCI_EVENT_DEREG:
      FNotifyType := cneDereg;
    OCI_EVENT_OBJCHANGE:
      FNotifyType := cneObjChange;
  else
    Assert(False);
    FNotifyType := cneNone;
  end;

  if OCINotifyType = OCI_EVENT_OBJCHANGE then begin
    Check(OCIAttrGet2(ChangeDescriptor, OCI_DTYPE_CHDES, OCITableChanges, nil,
      OCI_ATTR_CHDES_TABLE_CHANGES, hOCIError));

    FTableChanges := TNotifyTableChanges.Create(pOCIColl(OCITableChanges));
  end
  else
    FTableChanges := nil;
end;

destructor TNotifyChange.Destroy;
begin
  FTableChanges.Free;

  inherited Destroy;
end;

{ TOCIChangeNotification }

procedure TOCIChangeNotification.Register(Connection: TOCIConnection);
var
  hOCISubscr: pOCISubscription;
  AttrValue: integer;
begin
  if PossibleOCICallStyles = [OCI80] then
    RaiseError(SChangeNotifyNotSupportedWithDirect);

  if OCIVersion < 10200 then
    RaiseError(SChangeNotifyOCIVersion);

  if Connection.GetOracleVersion < 10200 then
    RaiseError(SChangeNotifyServerVersion);

  Check(OCIHandleAlloc(hOCIEnv, hOCISubscr, OCI_HTYPE_SUBSCRIPTION, 0, nil));
  try
    AttrValue := OCI_SUBSCR_NAMESPACE_DBCHANGE;
    Check(OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, SizeOf(ub4),
      OCI_ATTR_SUBSCR_NAMESPACE, hOCIError));

    Check(OCIAttrSet1(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, OCICallbackChangeNotifyPtr,  0,
      OCI_ATTR_SUBSCR_CALLBACK, hOCIError));

    AttrValue := 1;
    Check(OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, SizeOf(ub4),
      OCI_ATTR_CHNF_ROWIDS, hOCIError));

    if FOperations = [] then
      raise Exception.Create(SChangeNotifyOperationsCannotBeEmpty);
    AttrValue := 0;
    if FOperations <> [cnoInsert, cnoUpdate, cnoDelete] then begin // default
      if cnoInsert in FOperations then
        AttrValue := OCI_OPCODE_INSERT;
      if cnoUpdate in FOperations then
        AttrValue := AttrValue or OCI_OPCODE_UPDATE;
      if cnoUpdate in FOperations then
        AttrValue := AttrValue or OCI_OPCODE_DELETE;
    end;
    Check(OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, 0,
      OCI_ATTR_CHNF_OPERATIONS, hOCIError));

    Check(OCIAttrSet1(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, GCHandle, 0,
      OCI_ATTR_SUBSCR_CTX, hOCIError));

    Check(OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, FTimeout, 0,
      OCI_ATTR_SUBSCR_TIMEOUT, hOCIError));

    AttrValue := 0;
    if FPersistent then
      AttrValue := OCI_SUBSCR_QOS_RELIABLE;
    Check(OCIAttrSet2(hOCISubscr, OCI_HTYPE_SUBSCRIPTION, AttrValue, 0,
      OCI_ATTR_SUBSCR_QOSFLAGS, hOCIError));

    if hODACWindow = 0 then
      AllocODACWnd;

    Check(OCISubscriptionRegister(Connection.GetSvcCtx, hOCISubscr, 1,
      hOCIError, OCI_DEFAULT));

  except
    OCIHandleFree(hOCISubscr, OCI_HTYPE_SUBSCRIPTION);
    raise;
  end;

  hOCISubscription := hOCISubscr;
end;

procedure TOCIChangeNotification.Unregister(Connection: TOCIConnection);
begin
  if hOCISubscription <> nil then begin
    OCISubscriptionUnRegister(Connection.GetSvcCtx, hOCISubscription,
      hOCIError, OCI_DEFAULT);

    OCIHandleFree(hOCISubscription, OCI_HTYPE_SUBSCRIPTION);

    hOCISubscription := nil;
  end;
end;

function TOCIChangeNotification.GetSubscriptionHandle(Connection: TOCIConnection): pOCISubscription;
begin
  if hOCISubscription = nil then
    Register(Connection);
  Result := hOCISubscription;
end;

function TOCIChangeNotification.IsActive: boolean;
begin
  Result := hOCISubscription <> nil;
end;

function TOCIChangeNotification.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

procedure TOCIChangeNotification.SetEnabled(Value: boolean);
begin
  if hOCISubscription <> nil then begin
    if FEnabled and not Value then
      Check(OCISubscriptionDisable(hOCISubscription, hOCIError, OCI_DEFAULT));
    if not FEnabled and Value then
      Check(OCISubscriptionEnable(hOCISubscription, hOCIError, OCI_DEFAULT));
  end;
  FEnabled := Value;
end;

function OCICallbackChangeNotify(pCtx: IntPtr; pSubscrHp: pOCISubscription;
  pPayload: IntPtr; iPayloadLen: ub4; pDescriptor: IntPtr; iMode: ub4): sword;{$IFNDEF CLR} cdecl; {$ENDIF}
begin
  Result := TOCIChangeNotification(GetGCHandleTarget(pCtx)).CallbackChangeNotify(pCtx,
    pSubscrHp, pPayload, iPayloadLen, pDescriptor, iMode);
end;

function TOCIChangeNotification.CallbackChangeNotify(pCtx: IntPtr; pSubscrHp: pOCISubscription;
  pPayload: IntPtr; iPayloadLen: ub4; pDescriptor: IntPtr; iMode: ub4): sword;
var
  NotifyChange: TNotifyChange;
begin
  NotifyChange := TNotifyChange.Create(pDescriptor);

  if NotifyChange.NotifyType = cneDereg then begin
    OCIHandleFree(hOCISubscription, OCI_HTYPE_SUBSCRIPTION);
    hOCISubscription := nil;
  end;

  if Assigned(FOnChange) then
    PostMessage(hODACWindow, WM_CHANGENOTIFY, Integer(GCHandle),
      Integer(AllocGCHandle(NotifyChange)));

  Result := OCI_SUCCESS;
end;

constructor TOCIChangeNotification.Create;
begin
  inherited;

  FEnabled := True;
  FOperations := [cnoInsert, cnoUpdate, cnoDelete];
end;

destructor TOCIChangeNotification.Destroy;
begin
  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

function TOCIChangeNotification.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prEnabled:
      SetEnabled(Boolean(Value));
    prTimeout:
      FTimeout := Value;
    prPersistent:
      FPersistent := Boolean(Value);
    prOperations:
      FOperations := TChangeNotifyDMLOperations({$IFNDEF FPC}Byte{$ELSE}Integer{$ENDIF}(Value));
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TOCIChangeNotification.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;
{$ENDIF}

{ TOCICommand }

constructor TOCICommand.Create;
begin
  inherited Create;

  FCursor := TOraCursor.Create;
  FCursorRef := FCursor;
  SetCursorState(csInactive);
  FAutoCommit := True;
  FSQLType := SQL_UNKNOWN;
  FOCICallStyle := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCICallStyle;
  FIterCount := 1;
  FCacheLobs := True;
  FCheckParamHasDefault := True;

  FSmallintPrecision := -1;
  FIntegerPrecision := -1;
  FLargeIntPrecision := -1;
  FFloatPrecision := -1;
  FBCDPrecision := -1;
  FBCDScale := 0;
{$IFDEF VER6P}
{$IFNDEF FPC}
  FFmtBCDPrecision := -1;
  FFmtBCDScale := 0;
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  FRawAsString := True;
{$ENDIF}

{$IFDEF MSWINDOWS}
  hBusy := CreateMutex(nil, False, '');
{$ENDIF}
{$IFDEF WIN32_64}
  hExecuted := TEvent.Create(nil, True, True, '');
{$ENDIF}
end;

destructor TOCICommand.Destroy;
begin
{$IFDEF MSWINDOWS}
  if hExecThread <> nil then begin
    if FConnection <> nil then
      FConnection.StopThread(hExecThread, True);
  end
  else
{$ENDIF}
    if GetCursorState > csInactive then
      Finish;

{$IFDEF MSWINDOWS}
  CloseHandle(hBusy);
{$ENDIF}
{$IFDEF WIN32_64}
  hExecuted.Free;
{$ENDIF}

  Assert(FCursorRef <> nil);

  if (FCursorRef <> FCursor) then
    FCursorRef.Free;

  FCursor.Free;

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

procedure TOCICommand.RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure TOCICommand.CheckOCI;
begin
  if not ((FOCICallStyle = OCI73) or (FOCICallStyle = OCI80)) then
    RaiseError(SCheckOCI);
end;

procedure TOCICommand.CheckOCI73;
begin
  if not (FOCICallStyle = OCI73) then
    RaiseError(SCheckOCI73);
end;

procedure TOCICommand.CheckOCI80;
begin
  if not (FOCICallStyle = OCI80) then
    RaiseError(SCheckOCI80);
end;

procedure TOCICommand.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    FConnection.OraError(FOCICallStyle, Status, not (FNonBlocking and (FExecuting
      or (GetCursorState in [csFetching, csFetchingAll]))), Component)
end;

procedure TOCICommand.CheckSession;
begin
  if FConnection = nil then
    RaiseError(SSessionNotDefined);
  if not FConnection.GetConnected then
    RaiseError(SSessionNotConnected);
end;

procedure TOCICommand.CheckActive;
begin
  if GetCursorState = csInactive then
    RaiseError(SCursorNotOpened);
end;

procedure TOCICommand.CheckInactive;
begin
  if GetCursorState <> csInactive then
    RaiseError(SCursorOpened);
end;

function TOCICommand.GetSmallintPrecision: integer;
begin
  if FSmallintPrecision <> -1 then
    Result := FSmallintPrecision
  else
    Result := FConnection.FSmallintPrecision;
end;

function TOCICommand.GetIntegerPrecision: integer;
begin
  if FIntegerPrecision <> -1 then
    Result := FIntegerPrecision
  else
    Result := FConnection.FIntegerPrecision;
end;

function TOCICommand.GetLargeintPrecision: integer;
const
  DefaultLargeIntPrecision = 18;
begin
  if FLargeintPrecision <> -1 then
    Result := FLargeintPrecision
  else
    Result := FConnection.FLargeintPrecision;

  if (Result = 0) and FConnection.FEnableLargeint then
    Result := DefaultLargeIntPrecision;
end;

function TOCICommand.GetFloatPrecision: integer;
begin
  if FFloatPrecision <> -1 then
    Result := FFloatPrecision
  else
    Result := FConnection.FFloatPrecision;
end;

function TOCICommand.GetBCDPrecision: integer;
begin
  if FBCDPrecision <> -1 then
    Result := FBCDPrecision
  else
    Result := FConnection.FBCDPrecision;
end;

function TOCICommand.GetBCDScale: integer;
begin
  if FBCDPrecision <> -1 then
    Result := FBCDScale
  else
    Result := FConnection.FBCDScale;
end;

{$IFDEF VER6P}
{$IFNDEF FPC}
function TOCICommand.GetFmtBCDPrecision: integer;
begin
  if FFmtBCDPrecision <> -1 then
    Result := FFmtBCDPrecision
  else
    Result := FConnection.FFmtBCDPrecision;
end;

function TOCICommand.GetFmtBCDScale: integer;
begin
  if FFmtBCDPrecision <> -1 then
    Result := FFmtBCDScale
  else
    Result := FConnection.FFmtBCDScale;
end;
{$ENDIF}
{$ENDIF}

function TOCICommand.GetOraType7(DataType: integer; SubDataType: integer): integer;
var
  OraType: integer;
begin
  case DataType of
    dtBoolean:
      OraType := SQLT_INT;
    dtString:
      OraType := STRING_TYPE;  // return with terminator
    dtInteger:
      OraType := INTEGER_TYPE;
    dtFloat:
      OraType := FLOAT_TYPE;
    dtDateTime:
      OraType := DATE_TYPE;
    dtFixedChar:
      OraType := CHARZ_TYPE;
    dtMemo:
      if (SubDataType = dtString) or (SubDataType = dtNString) then
        OraType := STRING_TYPE  // return with terminator
      else
        OraType := LONG_TYPE;
    dtBlob:
      OraType := LONGRAW_TYPE;
    dtRowId:
      OraType := ROWID_TYPE;
    dtCursor:
      OraType := CURSOR_TYPE;
    dtExtString:
      OraType := STRING_TYPE;
    dtVarBytes:
      OraType := RAW_TYPE;
    dtExtVarBytes:
      OraType := RAW_TYPE;
    dtLabel:
      OraType := SQLT_OSL;
  else
    RaiseError(SDataTypeNotSupported);
    OraType := 0;
  end;
  Result := OraType;
end;

function TOCICommand.GetOraType8(DataType: integer; SubDataType: integer): integer;
var
  OraType: integer;
begin
  case DataType of
    dtString:
      OraType := SQLT_STR;  // return with terminator
    dtWideString:
      OraType := SQLT_STR;
    dtInteger, dtSmallint:
      OraType := SQLT_INT;
    dtFloat:
      if ((SubDataType = dtBDouble) or (SubDataType = dtBFloat)) and (OCIVersion >= 10000) and (FConnection.GetOracleVersion >= 10000) then
        OraType := SQLT_BDOUBLE
      else
        OraType := SQLT_FLT;
    dtDateTime:
      OraType := SQLT_DAT;
    dtFixedChar:
      OraType := SQLT_AVC; // CHARZ (null terminated char)
    dtFixedWideChar:
      OraType := SQLT_AVC;
    dtMemo, dtWideMemo:
      if (SubDataType = dtString) or (SubDataType = dtWideString) or
         (SubDataType = dtNString) or (SubDataType = dtNWideString) then
        OraType := SQLT_STR  // return with terminator
      else
        OraType := SQLT_LNG;
    dtVarBytes:
      OraType := SQLT_BIN;
    dtBlob:
      OraType := SQLT_LBI;
    dtOraBlob:
      OraType := SQLT_BLOB;
    dtOraClob, dtWideOraClob:
      OraType := SQLT_CLOB;
    dtBFILE:
      OraType := SQLT_BFILE;
    dtCFILE:
      OraType := SQLT_CFILE;
    dtRowId:
      OraType := SQLT_RDD;
    dtCursor:
      OraType := SQLT_RSET;
    dtObject:
      OraType := SQLT_NTY;
    dtXML: begin
      if OCIVersion < 9200 then
        RaiseError(SDataTypeNotSupported);
      OraType := SQLT_NTY;
    end;
    dtReference:
      OraType := SQLT_REF;
    dtArray:
      OraType := SQLT_NTY;
    dtTable:
      OraType := SQLT_NTY;
    dtBoolean:
      OraType := SQLT_INT;
    dtLabel:
      OraType := SQLT_OSL;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF},
    dtIntervalYM, dtIntervalDS: begin
      if OCIVersion < 9000 then
        RaiseError(SDataTypeNotSupported);
      OraType := 0; // anti warning
      case DataType of
        dtTimeStamp{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}:
          OraType := SQLT_TIMESTAMP;
        dtTimeStampTZ:
          OraType := SQLT_TIMESTAMP_TZ;
        dtTimeStampLTZ:
          if not OldTimeStampLTZRepresentation then
            OraType := SQLT_TIMESTAMP
          else
            OraType := SQLT_TIMESTAMP_LTZ;
        dtIntervalYM:
          OraType := SQLT_INTERVAL_YM;
        dtIntervalDS:
          OraType := SQLT_INTERVAL_DS;
      end;
    end;
    dtUndefined:
      OraType := SQLT_CHR;
    dtExtString:
      OraType := SQLT_STR;
    dtExtWideString:
      OraType := SQLT_STR;
    dtExtVarBytes:
      OraType := SQLT_BIN;
    dtNumber, dtLargeint, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}: begin
      OraType := SQLT_VNU;
    end;
  else
    RaiseError(SDataTypeNotSupported);
    OraType := 0;
  end;
  Result := OraType;
end;

function TOCICommand.GetOraType(DataType: integer; SubDataType: integer): integer;
begin
  if FOCICallStyle = OCI73 then begin
    Result := GetOraType7(DataType, SubDataType);
  end
  else
  if FOCICallStyle = OCI80 then begin
    Result := GetOraType8(DataType, SubDataType);
  end
  else begin
    Result := 0;
    CheckOCI;
  end;
end;

procedure TOCICommand.InternalOpen;
var
  Res: sword;
begin
  CheckOCI73;
  CheckSession;

  Busy;
  try // For Busy
    FConnection.BusyWait;
      Res := oopen(FCursorRef.CDA, FConnection.GetLDA, nil, -1, -1, nil, -1);
    FConnection.Release;

    Check(Res);

    SetCursorState(csOpen);
  finally
    Release;
  end;
end;

function TOCICommand.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

/// RemoveCRSymbols is used to remove #13 symbol from string. It is needed for
/// pl/sql code to be executed without runtime errors. If ErrorOffset <> -1 is
/// Result may contain invalid value but ErrorOffset is recalculated to
/// reflect error correctly.
function TOCICommand.RemoveCRSymbols(SQLText: _string;
  var ErrorOffset: integer): _string;
const
  WrappedConstL: _string = 'wrapped';
  WrappedConstU: _string = 'WRAPPED';
var
  i, j, starti, Len: integer;
  InlineComment: boolean;
  NestedComment: boolean;
  StringLiteral: boolean;
  AlternativeQuoting: boolean;
  QuoteDelimiter: _char;

  IsWrapped: boolean;
  WrappedIndex: integer;
begin
  Result := '';

  if SQLText = '' then
    Exit;

  Len := Length(SQLText);
  Result := '';
  j := 1;
  i := 1;
  starti := 1;
  IsWrapped := False;
  WrappedIndex := 1;
  InlineComment := False;
  NestedComment := False;
  StringLiteral := False;
  AlternativeQuoting := False;
  QuoteDelimiter := #0;

  while i < Len do begin
    if (ErrorOffset <> -1) and (ErrorOffset = j - 1) then begin
      ErrorOffset := i - 1;
      Exit;
    end;
    case SQLText[i] of
      #13:
        if not StringLiteral or RemoveCRInStringLiterals then begin
          Result := Result + Copy(SQLText, starti, i - starti) + #10;
          if SQLText[i + 1] = #10 then
            starti := i + 2
          else begin
            starti := i + 1;
            j := j + 1;
          end;
          i := i + 1;
          Continue;
        end;
      #10:
        InlineComment := False;
      '-':
        if not (NestedComment or StringLiteral) and (SQLText[i + 1] = '-') then
          InlineComment := True;
      '/':
        if not (InlineComment or StringLiteral) and (SQLText[i + 1] = '*') then
          NestedComment := True;
      '''': begin
        // to avoid bug if comment contains single apostrophe
        if not (InlineComment or NestedComment or IsWrapped) then begin
          if not StringLiteral then begin
            if (i > 1) and ((SQLText[i - 1] = 'q') or (SQLText[i - 1] = 'Q')) then begin
              AlternativeQuoting := True;
              QuoteDelimiter := SQLText[i + 1];
              case QuoteDelimiter of
                '[': QuoteDelimiter := ']';
                '{': QuoteDelimiter := '}';
                '<': QuoteDelimiter := '>';
                '(': QuoteDelimiter := ')';
              end;
              j := j + 1;
              i := i + 1;
            end
            else
              AlternativeQuoting := False;
            StringLiteral := True;
          end
          else begin
            if AlternativeQuoting then begin
              if (i > 1) and (SQLText[i - 1] = QuoteDelimiter) then
                StringLiteral := False;
            end
            else
              StringLiteral := False;
          end;
        end;
      end;
      '*':
        if not (InlineComment or StringLiteral) and NestedComment
          and (SQLText[i + 1] = '/')
        then begin
          NestedComment := False;
        /// on case of such code as "select /*all fields*/* from dept"
          j := j + 1;
          i := i + 1;
        end;
    end;
    //to avoid bug with processing wrapped packages
    if not (IsWrapped or NestedComment or StringLiteral) then begin
      if ((SQLText[i] = WrappedConstU[WrappedIndex]) or (SQLText[i] = WrappedConstL[WrappedIndex]))
        and (i > 1) and ((WrappedIndex > 1) or (SQLText[i - 1] = #10) or (SQLText[i - 1] = ' ')
        or (SQLText[i - 1] = '/'))
      then
        inc(WrappedIndex)
      else
        WrappedIndex := 1;

      if (WrappedIndex = 8) then begin
        if (i < Len) and ((SQLText[i + 1] = #13) or (SQLText[i + 1] = #10)
          or (SQLText[i + 1] = ' ') or (SQLText[i + 1] = '/'))
        then
          IsWrapped := True;
        WrappedIndex := 1;
      end;
    end
    else
      WrappedIndex := 1;

    j := j + 1;
    i := i + 1;
  end;
  Result := Result + Copy(SQLText, starti, i - starti + 1);
  if (ErrorOffset <> -1) and (ErrorOffset = j - 1) then begin
    ErrorOffset := i - 1;
  end;
end;

procedure TOCICommand.InternalParse;
var
  Res: sword;
  SQLText: _string;
begin
  CheckOCI73;
  CheckActive;

// for PL-SQL
  Res := -1;
  SQLText := RemoveCRSymbols(TrimRight(FSQL), Res);

  Busy;
  try   // For Busy
    FConnection.BusyWait;
      Res := oparse(FCursorRef.CDA, PAnsiChar(AnsiString(SQLText)), Length(SQLText) + 1, //-1,
        OCI_PARSE_NODEFER, OCI_LANG_V7);  // DEFER ???
    FConnection.Release;

    // check for compilation errors
    if (Res = OCI_SUCCESS) and (FCursorRef.CDA.wrn and 33 = 33) then
      FConnection.FLastError := 24344;

    Check(Res);

    FSQLType := FCursorRef.CDA.ft;
    FLastSQLtype := FSQLType;

    SetCursorState(csParsed);
  finally
    FErrorOffset := FCursorRef.CDA.peo;
    Release;
  end;
end;

procedure TOCICommand.InternalPrepare;
var
  SQLText: _string;
  Res: sword;
  StmtType: word;
  StmtTypeInt32: Integer;
  OCIStmt: pOCIStmt;
{$IFDEF MSWINDOWS}
  OCISubscr: pOCISubscription;
{$ENDIF}
  i, Size: integer;
  p: IntPtr;
begin
  CheckOCI80;
  CheckSession;

  for i := 0 to Params.Count - 1 do
    TOraParamDesc(Params[i]).ClearBindData;

// for PL-SQL
  Res := -1;
  SQLText := RemoveCRSymbols(TrimRight(FSQL), Res);
  if SQLText = '' then
    SQLText := ' ';

  if FConnection.FUnicodeEnv and (OCIVersion >= 10200) and
    (OCIVersion < 11000) and (SQLText[Length(SQLText)] = 'n')
  then
    SQLText := SQLText + ' ';

  Busy;
  try   // For Busy
    FConnection.BusyWait;

    if FConnection.FStatementCache and FStatementCache then begin
      if PossibleOCICallStyles = [OCI80] then
        RaiseError(SStmtCacheNotSupportedWithDirect);
      p := StringToHGlobalOCI(SQLText, Size, FConnection.FUnicodeEnv);
      Res := OCIStmtPrepare2(FConnection.hSvcCtx, OCIStmt, hOCIError, p, Size,
        nil, 0, OCI_NTV_SYNTAX, OCI_DEFAULT);
      FreeStringOCI(p, FConnection.FUnicodeEnv);
      FCursorRef.hOCIStmt := OCIStmt;
    end
    else begin
      p := StringToHGlobalOCI(SQLText, Size, FConnection.FUnicodeEnv);
      Res := OCIStmtPrepare(FCursorRef.OCIStmt, hOCIError, p, Size, OCI_NTV_SYNTAX, OCI_DEFAULT);
      FreeStringOCI(p, FConnection.FUnicodeEnv);
    end;
    FConnection.Release;

    Check(Res);

    Check(OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, StmtTypeInt32, nil, OCI_ATTR_STMT_TYPE, hOCIError));
    StmtType := Word(StmtTypeInt32);

    case StmtType of
      OCI_STMT_SELECT:
        FSQLType := SQL_SELECT;
      OCI_STMT_UPDATE:
        FSQLType := SQL_UPDATE;
      OCI_STMT_DELETE:
        FSQLType := SQL_DELETE;
      OCI_STMT_INSERT:
        FSQLType := SQL_INSERT;
      OCI_STMT_BEGIN, OCI_STMT_DECLARE:
        FSQLType := SQL_PLSQL;
      14,15: // explain 805 - 14, 816 - 15
        FSQLType:= SQL_EXPLAIN;
    else
      FSQLType := SQL_UNKNOWN;
    end;
    FLastSQLType := FSQLType;

  {$IFDEF MSWINDOWS}
    if (FChangeNotification <> nil) and (FChangeNotification.FEnabled) then begin
      OCISubscr := FChangeNotification.GetSubscriptionHandle(FConnection);

      Check(OCIAttrSet1(FCursorRef.OCIStmt, OCI_HTYPE_STMT, OCISubscr, 0,
        OCI_ATTR_CHNF_REGHANDLE, hOCIError));
    end;
  {$ENDIF}

    SetCursorState(csPrepared);
  finally
    Release;
  end;
end;

procedure TOCICommand.Prepare;
begin
  CheckSession;
  if GetCursorState <> csInactive then
    Exit;
//  CheckInactive;

  FOCICallStyle := FConnection.FOCICallStyleCommand;  // see InitProcParams and SetCursor too
  hOCIError := FConnection.hOCIError;
  FCursorRef.SetOCICallStyle(FOCICallStyle);

  FRowsProcessed := 0;
  FErrorOffset := 0;

  FCursorRef.hOCIEnv := FConnection.hOCIEnv;
  FCursorRef.hOCIError := FConnection.hOCIError;
  FCursorRef.FUnicodeEnv := FConnection.FUnicodeEnv;
  FCursorRef.AllocCursor(FConnection.FStatementCache);
  try
    if FOCICallStyle = OCI73 then begin
      InternalOpen;
      try
        InternalParse;
      except
        InternalClose;
        raise;
      end;
    end
    else
    if FOCICallStyle = OCI80 then begin
      InternalPrepare;
    end
    else
      CheckOCI;
  except
    FCursorRef.FreeCursor;
    raise;
  end;

  inherited;
end;

function TOCICommand.NeedBindParam(Param: TOraParamDesc): boolean;
begin
  Result := Param.GetIsBound or (FCheckParamHasDefault and not Param.FHasDefault) or
    (Param.ParamType <> pdInput);
end;

function OCICallbackInBind(ictxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
  var bufp: IntPtr; var alen: ub4; var piece: ub1; var indp: IntPtr): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}forward;

function OCICallbackOutBind(octxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
  var bufp: IntPtr; var alen: pub4; var piece: ub1; var indp: IntPtr; var rcodep: pub2): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}forward;

procedure TOCICommand.BindParam(Param: TOraParamDesc);
var
  NullBuf: IntPtr;
  OraType: word;
  hBind: pOCIBind;
  ValuePtr: IntPtr;
  LenPtr: IntPtr;
  StrPtr: IntPtr;
  BufferSize: integer;
  BufferSkip: integer;
  ParamName: _string;
  Obj: TOraObject;
  CharsetId: Integer; //sb2
  CharsetForm : Integer;
  MaxDataSize: Integer;
  Res, StrSize: integer;
begin
  if not NeedBindParam(Param) then
    exit;

  Param.ValidateParamValue;

  Param.AllocBuffer;

  if Param.DataType = dtUnknown then
    if Param.GetNull then
      OraType := VARCHAR2_TYPE  // WAR
    else
      raise Exception.Create(Format(SUnknownParamDataType, [Param.GetName]))
  else
    OraType := GetOraType(Param.DataType, Param.SubDataType);

  Param.SyncIndicator(FConnection);

  ParamName :=  Param.Name;
  BufferSize := Param.FValueSize;
  BufferSkip := BufferSize;
  ValuePtr := Param.ValuePtr;
  LenPtr := nil;

  if not Param.FTable then begin
    case Param.DataType of
      dtBytes,dtVarBytes: begin
        LenPtr := ValuePtr;
        BufferSize := 2000; // for RAW datatype out params
        ValuePtr := PtrOffset(ValuePtr, SizeOf(Word));
      end;
      dtString,dtFixedChar: begin
        if ((Param.ParamType = pdInput) or (FSQLType = SQL_SELECT) and
          (Param.GetParamType in [pdUnknown, pdInputOutput])) and (Param.FLen <> 0)
        then begin
          BufferSize := Param.FLen + 1;
          Marshal.WriteByte(ValuePtr, BufferSize - 1, 0); // anti ORA-01480
        end;
        if (PossibleOCICallStyles = [OCI80]) then begin
          if (not FConnection.FUseUnicode) and (FConnection.FCharLength > 1) and (BufferSize > FConnection.GetMaxStringSize) then begin
            BufferSize := FConnection.GetMaxStringSize;
            Marshal.WriteByte(ValuePtr, BufferSize - 1, 0);
          end;
        end
        else begin
          if (not FConnection.FUseUnicode) and (FConnection.FCharLength > 1) and (BufferSize > FConnection.GetMaxStringSize + 1) then begin
            BufferSize := FConnection.GetMaxStringSize + 1;
            Marshal.WriteByte(ValuePtr, BufferSize - 1, 0);
          end
        end;
      end;
      dtWideString, dtFixedWideChar: begin
        if ((Param.ParamType = pdInput) or (FSQLType = SQL_SELECT) and
          (Param.GetParamType in [pdUnknown, pdInputOutput])) and (Param.FLen <> 0)
        then begin
           BufferSize := Param.FLen * 2+ 2;
           Marshal.WriteInt16(ValuePtr, BufferSize - 2, 0);
        end;
        if (PossibleOCICallStyles = [OCI80]) then begin
          if (not FConnection.FUseUnicode) and (FConnection.FCharLength > 1) and (BufferSize > FConnection.GetMaxStringSize * 2) then begin
            BufferSize := FConnection.GetMaxStringSize * 2;
            BufferSize := BufferSize - BufferSize mod 2 - 1;
            Marshal.WriteInt16(ValuePtr, BufferSize - 1, 0);
          end;
        end
        else begin
          if (not FConnection.FUseUnicode) and (FConnection.FCharLength > 1) and (BufferSize > FConnection.GetMaxStringSize * 2 + 2) then begin
            BufferSize := FConnection.GetMaxStringSize * 2 + 2;
            BufferSize := BufferSize - BufferSize mod 2;
            Marshal.WriteInt16(ValuePtr, BufferSize - 2, 0);
          end;
        end
      end;
      dtBlob,dtMemo,dtWideMemo:
        if Param.ParamType = pdInput then begin
          BufferSize := Param.GetAsBlobRef.Size; // anti ORA-01026
          if BufferSize = 0 then
            Inc(BufferSize);
        end
        else begin
          BufferSize := MaxBlobSize;
          if Param.ParamType in [pdOutput, pdResult] then
            Param.GetAsBlobRef.Clear;  // prevent write OUT data
        end;
    end;
  end
  else
    case Param.DataType of
      dtString,dtFixedChar: begin
        if (PossibleOCICallStyles = [OCI80]) then begin
          if BufferSize > FConnection.GetMaxStringSize + 1 then begin
            BufferSize := FConnection.GetMaxStringSize + 1;
            Marshal.WriteByte(ValuePtr, BufferSize - 1, 0);// anti ORA-01480
          end
        end
        else begin
          if BufferSize > FConnection.GetMaxStringSize then begin
            BufferSize := FConnection.GetMaxStringSize;
            Marshal.WriteByte(ValuePtr, BufferSize - 1, 0);// anti ORA-01480
          end
        end
      end;
    end;

  NullBuf := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(NullBuf, nil);
  try
    if FOCICallStyle = OCI73 then begin
      case Param.DataType of
        dtCursor: begin
          Assert(Param.GetAsCursor <> nil);
          Param.GetAsCursor.FOCICallStyle := FOCICallStyle;
          Param.GetAsCursor.AllocCursor;
          ValuePtr := Param.GetAsCursor.CDA;
        end;
        dtUnknown:  begin
          ValuePtr := NullBuf;
          BufferSize := 1;
        end;
      end;

      if not Param.FTable then begin
        if not(Param.DataType in [dtBlob,dtMemo]) then
          if LenPtr <> nil then
            Check(obndra(FCursorRef.CDA, PAnsiChar(AnsiString(ParamName)), -1, ValuePtr, BufferSize,
              OraType, -1, Param.IndicatorPtr, LenPtr, nil, 0, nil, nil, -1, -1))
          else
            Check(obndrv(FCursorRef.CDA, PAnsiChar(AnsiString(ParamName)), -1, ValuePtr, BufferSize,
              OraType, -1, Param.IndicatorPtr, nil, -1, -1))
        else
        { Piecewise }
          Check(obindps(FCursorRef.CDA, 0, PAnsiChar(AnsiString(ParamName)), -1, nil, MaxBlobSize,
            OraType, 0, Param.IndicatorPtr, nil, nil, 0, 0, 0, 0, 0, nil, nil, 0, 0));

        Param.ActualLength := 1;
      end
      else begin
      { Table }
        Param.ActualLength := Param.FLength;
        Check(obindps(FCursorRef.CDA, 1, PAnsiChar(AnsiString(ParamName)), -1, ValuePtr, BufferSize,
          OraType, 0, Param.IndicatorPtr, nil, nil, BufferSize, sizeof(sb2),
          0, 0, Param.FLength, Param.FActualLengthPtr, nil, 0, 0));
      end;
    end
    else
    if FOCICallStyle = OCI80 then begin
      case Param.DataType of
        dtCursor: begin
          Assert(Param.GetAsCursor <> nil);
          Param.GetAsCursor.FOCICallStyle := FOCICallStyle;
          Param.GetAsCursor.hOCIEnv := FConnection.hOCIEnv;
          Param.GetAsCursor.hOCIError := FConnection.hOCIError;
          Param.GetAsCursor.FUnicodeEnv := FConnection.FUnicodeEnv;
          Param.GetAsCursor.AllocCursor;
          ValuePtr := Param.GetAsCursor.OCIStmtPtr;
        end;
        dtOraBlob,dtOraClob,dtNClob: begin
          if Param.GetLength <= 1 then begin
            Param.GetAsOraBlob.SetConnection(FConnection);
            Param.GetAsOraBlob.FCharLength := FConnection.FCharLength;
            Param.GetAsOraBlob.AllocLob;
            ValuePtr := Param.GetAsOraBlob.OCILobLocatorPtr;
          end;  
        end;
        dtBFile,dtCFile: begin
          Param.GetAsBFile.SetConnection(FConnection);
          Param.GetAsBFile.AllocLob;
          ValuePtr := Param.GetAsBFile.OCILobLocatorPtr;
        end;
        dtObject,dtXML,dtArray,dtTable: begin
          Obj := TOraObject(Param.GetObject);
          Obj.OCIError := FConnection.hOCIError;
          Obj.AllocObject(FConnection.GetSvcCtx);
          Obj.WriteLobs;
          ValuePtr := nil;
        end;
        dtReference: begin
          ValuePtr := nil;
          //ValuePtr := Param.AsRef.OCIRefPtr;
          //RefPtr := nil;
          //ValuePtr := @RefPtr;//Param.AsRef.OCIRefPtr;
        end;
        dtNumber, dtBCD{$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}: begin
          if Param.GetLength <= 1 then begin
            BufferSize := OCI_NUMBER_SIZE;
            ValuePtr := Param.GetAsNumber.OCINumberPtr;
          end;  
        end;
        dtUnknown: begin
          ValuePtr := NullBuf;
          BufferSize := 1;
        end;
        else
          if OCIVersion >= 9000 then begin
            case Param.DataType of
              dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}:
                if Param.GetLength <= 1 then
                  with Param.GetAsTimeStamp do begin
                    BufferSize := sizeof(IntPtr);
                    case Param.DataType of
                      dtTimeStamp{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}:
                        DescriptorType := OCI_DTYPE_TIMESTAMP;
                      dtTimeStampTZ:
                        DescriptorType := OCI_DTYPE_TIMESTAMP_TZ;
                      dtTimeStampLTZ:
                        if not OldTimeStampLTZRepresentation then
                          DescriptorType := OCI_DTYPE_TIMESTAMP
                        else
                          DescriptorType := OCI_DTYPE_TIMESTAMP_LTZ;
                    end;
                    AllocDateTime;
                    ValuePtr := GetOCIDateTimePtr;
                  end;
              dtIntervalYM, dtIntervalDS:
                if Param.GetLength <= 1 then
                  with Param.GetAsInterval do begin
                    BufferSize := sizeof(IntPtr);
                    if Param.DataType = dtIntervalYM then
                      DescriptorType := OCI_DTYPE_INTERVAL_YM
                    else
                      DescriptorType := OCI_DTYPE_INTERVAL_DS;
                    AllocInterval;
                    ValuePtr := GetOCIIntervalPtr;
                  end;
            end;
          end;
      end;


      if (Param.FHandle <> nil) and (Param.FBindBufferSize = BufferSize) then
        Exit;

      if not Param.FTable then begin
        if FIterCount = 1 then begin
          if not (Param.DataType in [dtBlob,dtMemo,dtWideMemo]) then begin
            StrPtr := StringToHGlobalOCI(ParamName, StrSize, FConnection.FUnicodeEnv);
            Res := OCIBindByName(FCursorRef.OCIStmt, hBind, hOCIError, StrPtr, StrSize,
              ValuePtr, BufferSize, OraType,
              Param.IndicatorPtr, LenPtr, nil, 0, nil, OCI_DEFAULT);
            FreeStringOCI(StrPtr, FConnection.FUnicodeEnv);
            Check(Res);

            case Param.DataType of
              dtObject,dtArray,dtTable:
                Check(OCIBindObject(hBind, hOCIError, TOraObject(Param.GetObject).ObjectType.TDO,
                  TOraObject(Param.GetObject).InstancePtr, nil, TOraObject(Param.GetObject).IndicatorPtr, nil));
              dtXML: begin
                // if XML is NULL then Instance and Indicator must be nil (for OUT parameters especially)
                if TOraObject(Param.GetObject).IsNull then
                  TOraObject(Param.GetObject).FreeObject;

                Check(OCIBindObject(hBind, hOCIError, TOraObject(Param.GetObject).ObjectType.TDO,
                  TOraObject(Param.GetObject).InstancePtr, nil, TOraObject(Param.GetObject).IndicatorPtr, nil));
              end;
              dtReference:
                Check(OCIBindObject(hBind, hOCIError, TOraObject(Param.GetObject).ObjectType.TDO,
                  IntPtr(TOraRef(Param.GetObject).OCIRefPtr), nil, nil, nil));
            end;
          end
          else begin
          { Piecewise }
            {if Param.FParamType = pdInput then begin
              BufferSize := Param.GetAsBlobRef.Size; // anti ORA-01026
              if BufferSize = 0 then
                Inc(BufferSize);
            end
            else begin
              BufferSize := MaxBlobSize;
              if Param.FParamType = pdOutput then
                Param.GetAsBlobRef.Clear;  // prevent write OUT data
            end;}

            StrPtr := StringToHGlobalOCI(ParamName, StrSize, FConnection.FUnicodeEnv);
            Res := OCIBindByName(FCursorRef.OCIStmt, hBind, hOCIError, StrPtr, StrSize,
              nil, BufferSize, OraType, Param.IndicatorPtr,
              nil, nil, 0, nil, OCI_DATA_AT_EXEC);
            FreeStringOCI(StrPtr, FConnection.FUnicodeEnv);
            Check(Res);

          // Dynamic blob
          {$IFDEF CLR}
            if PossibleOCICallStyles = [OCI80] then
              Check(OCIBindDynamic(hBind, hOCIError, GCHandle, IntPtr(HOCICallbackInBind), GCHandle, IntPtr(HOCICallbackOutBind)))
            else
          {$ENDIF}
              Check(OCIBindDynamic(hBind, hOCIError, GCHandle, OCICallbackInBindPtr, GCHandle, OCICallbackOutBindPtr));
          end;
          Param.ActualLength := 1;
        end
        else begin
        // DML Array
          if FIterCount > Param.FLength then
            RaiseError(Format(SArrayParam, [ParamName, FIterCount]));

          StrPtr := StringToHGlobalOCI(ParamName, StrSize, FConnection.FUnicodeEnv);
          Res := OCIBindByName(FCursorRef.OCIStmt, hBind, hOCIError, StrPtr, StrSize,
            ValuePtr, BufferSize, OraType,
            Param.IndicatorPtr, LenPtr, nil, 0, nil, OCI_DEFAULT);
          FreeStringOCI(StrPtr, FConnection.FUnicodeEnv);
          Check(Res);

          Check(OCIBindArrayOfStruct(hBind, hOCIError, BufferSkip, sizeof(sb2),
            BufferSkip, 0))
        end;
        Param.FHandle := hBind;
        Param.FBindBufferSize := BufferSize;
      end
      else begin
      { Table }
      // PL/SQL table
        Param.ActualLength := Param.FLength;
        StrPtr := StringToHGlobalOCI(ParamName, StrSize, FConnection.FUnicodeEnv);
        Res := OCIBindByName(FCursorRef.OCIStmt, hBind, hOCIError, StrPtr, StrSize,
          ValuePtr, BufferSize, OraType,
          Param.IndicatorPtr, nil, nil, Param.FLength, Param.FActualLengthPtr, OCI_DEFAULT);
        FreeStringOCI(StrPtr, FConnection.FUnicodeEnv);
        Check(Res);

        Check(OCIBindArrayOfStruct(hBind, hOCIError, BufferSkip, sizeof(sb2),
          0, 0));

        Param.FHandle := hBind;
        Param.FBindBufferSize := BufferSize;
      end;

      case Param.DataType of
        dtString, dtFixedChar:
          CharsetId := Integer(FConnection.FCharsetId);
        dtWideString, dtFixedWideChar:
          CharsetId := Integer(OCI_UTF16ID);
        dtBlob,dtMemo,dtWideMemo:
          if Param.GetAsBlobRef.IsUnicode then
            CharsetId := Integer(OCI_UTF16ID)
          else
            CharsetId := Integer(FConnection.FCharsetId);
      else
        CharsetId := 0;
      end;

      if Param.GetNational or
         ((CharsetId = OCI_UTF16ID) and FConnection.FUnicodeAsNational and (OCIVersion >= 9000)) then
      begin
        CharsetForm := SQLCS_NCHAR;
        Check(OCIAttrSet2(hBind, OCI_HTYPE_BIND, CharsetForm, 0, OCI_ATTR_CHARSET_FORM, hOCIError));
      end;
      if Integer(CharsetId) > 0 then
        Check(OCIAttrSet2(hBind, OCI_HTYPE_BIND, CharsetId, 0, OCI_ATTR_CHARSET_ID, hOCIError));
      if UseMaxDataSize and not (PossibleOCICallStyles = [OCI80]) and
        (Param.DataType in [dtString, dtFixedChar, dtWideString, dtFixedWideChar])
      then begin
        MaxDataSize := Param.FValueSize - 1;
        if Param.DataType in [dtWideString, dtFixedWideChar] then
          MaxDataSize := MaxDataSize div 2;
        if (Param.ParamType = pdInput) and not Param.FTable then begin //(((Param.ParamType = pdUnknown) and (Param.FLen > 0)) or
                                                                       // Unknown param is binded as Input/OutPut
                                                                       // cr8800
          MaxDataSize := FConnection.GetMaxStringSize;
          if Param.FLen * 3 < MaxDataSize then
            MaxDataSize := Param.FLen * 3;

          if (Param.DataType in [dtWideString, dtFixedWideChar]) then begin
            if (MaxDataSize < 4) then
             MaxDataSize := 4;
          end
          else
            if MaxDataSize < 2 then
              MaxDataSize := 2;
        end;
        Check(OCIAttrSet2(hBind, OCI_HTYPE_BIND, MaxDataSize, 0, OCI_ATTR_MAXDATA_SIZE, hOCIError));
        if (OCIVersion >= 9000) and (FConnection.GetOracleVersion >= 9200) and (Param.ParamType = pdInput) and (Param.FLen > 0) and
           ((FConnection.GetOracleVersion < 9206) or (FConnection.GetOracleVersion > 10000)) then begin //Oracle 9.2.0.6 Patchset Bug
          MaxDataSize := Integer(Param.FLen);
          Check(OCIAttrSet2(hBind, OCI_HTYPE_BIND, MaxDataSize, 0, OCI_ATTR_MAXCHAR_SIZE, hOCIError));
        end;
      end;
    end
    else
      CheckOCI;
  finally
    Marshal.FreeHGlobal(NullBuf);
  end;
end;

function TOCICommand.InternalExecute(Mode: integer; Rows: Integer = 0): sword;
var
  OldCursorState: TCursorState;
  Res: sword;
  Res1: sword;
  Iters:ub4;
  i: integer;
  ValueInt: Integer;
  OCIStmt: pOCIStmt;
begin
  CheckActive;

  Busy;
  OldCursorState := GetCursorState;
  SetCursorState(csExecuting);
  Res := OCI_SUCCESS;

  OCIStmt := nil;
  try // For Busy
    try
      if FOCICallStyle = OCI73 then begin
        if Mode = OCI_DEFAULT then begin
          FConnection.BusyWait;
            Res := oexn(FCursorRef.CDA, FIterCount, 0);
          FConnection.Release;

          if (Res <> OCI_SUCCESS) and
             (Res <> OCI_STILL_IS_PIECE) and
             (Res <> OCI_STILL_IS_PIECE1) and
             (Res <> OCI_BLOCKED)
          then begin
            FErrorOffset := FCursorRef.CDA.peo;
            Check(Res);
          end;

          if Res = OCI_SUCCESS then
            FRowsProcessed := FCursorRef.CDA.rpc;
        end;
      end
      else
      if FOCICallStyle = OCI80 then begin
        if FSQLType = SQL_SELECT then begin
          Iters := Rows
        end
        else
          Iters := FIterCount;

        OCIStmt := FCursorRef.OCIStmt;
        FConnection.BusyWait;
          Res := OCIStmtExecute(FConnection.GetSvcCtx, OCIStmt, hOCIError, Iters, 0, nil, nil, Mode);
        FConnection.Release;

        if Mode <> OCI_DESCRIBE_ONLY then begin
          Check(OCIAttrGet2(OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, OCI_ATTR_ROW_COUNT, hOCIError));
          FRowsProcessed := Integer(ValueInt);
        end;

        if FSQLType = SQL_UNKNOWN then begin
          Check(OCIAttrGet2(OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, OCI_ATTR_SQLFNCODE, hOCIError));
          FSQLType := Word(ValueInt);
          FLastSQLType := FSQLType;
        end;

        if (Res <> OCI_SUCCESS) and
           (Res <> OCI_NEED_DATA) and
           ((FSQLType <> SQL_SELECT) or (Res <> OCI_NO_DATA))
        then begin
          Res1 := GetOraError(Res, FConnection.FUnicodeEnv);
          if Res1 <> Abs(OCI_BLOCKED) then
            Check(Res)
          else
            Res := Res1;
        end;
      end
      else
        CheckOCI;
    except
      if GetCursorState <> csInactive then  // on lost connection
        SetCursorState(OldCursorState);
      raise;
    end;

    if Mode <> OCI_DESCRIBE_ONLY then begin
      SetCursorState(csExecuted);

      for i := 0 to FParams.Count - 1 do
        if TOraParamDesc(FParams[i]).DataType = dtCursor then
          TOraParamDesc(FParams[i]).GetAsCursor.State := csExecuted;
    end
    else
      SetCursorState(OldCursorState);

  finally
    if (FOCICallStyle = OCI80) and (OCIVersion >= 8050) and
      (GetCursorState <> csInactive) // on lost connection
    then begin
      if OCIVersion >= 8100 then
        Check(OCIAttrGet2(OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, OCI_ATTR_PARSE_ERROR_OFFSET, hOCIError))
      else
        Check(OCIAttrGet2(OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, $80, hOCIError));
      FErrorOffset := Word(ValueInt);
    end;
    Release;
  end;
  FFetchedRows := 0;
  Result := Res;
end;

procedure TOCICommand.Exec;
const
  BufSize = 100;
var
  i, j: integer;
  Res: sword;
  PieceStatus: ub1;
  Blob: TBlob;
  Piece: PPieceHeader;
  Ptr: IntPtr;
  BufLen: ub4;
  Iteration: ub4;
  Index: ub4;
  NeedData: integer;
  IsData: integer;
  hBind: pOCIHandle;
  Buf: IntPtr;
  Param: TOraParamDesc;
  Mode: TParamDirection;
  CommitOnSuccess: boolean;
  ItemObject: TSharedObject;
  OraLob: TOraLob;
  LobType: TLobType;

  procedure Warning(Msg: string);
  begin
  {$IFDEF WIN32_64}
    MessageBox(0{HInstance}, PChar(Msg), 'Warning', MB_OK or MB_ICONWARNING);
  {$ENDIF}
  end;

  procedure CreateLobTemporary(OraLob: TOraLob);
  begin
    OraLob.SetConnection(FConnection);
    {$IFDEF HAVE_COMPRESS}
    if (Param.DataType = dtOraBlob) and (FCompressBlob in [cbServer, cbClientServer]) then
      OraLob.Compressed := True
    else
      OraLob.Compressed := False;
    {$ENDIF}
    if Param.DataType = dtOraBlob then
      LobType := ltBlob
    else if Param.GetNational then
      LobType := ltNClob
    else
      LobType := ltClob;
    OraLob.CreateTemporary(LobType);
    OraLob.WriteLob;
  end;

begin
  CommitOnSuccess := (FOCICallStyle = OCI80) and
    (FSQLType <> SQL_SELECT) and
    FAutoCommit and
    FConnection.AutoCommit;

  if FTemporaryLobUpdate then
    for i := 0 to FParams.Count - 1 do begin
      Param := TOraParamDesc(FParams[i]);
      if (Param.DataType in [dtOraBlob,dtOraClob]) and (Param.ParamType in [pdUnknown, pdInput, pdInputOutput]) then
        // standard parameter
        if Param.GetLength <= 1 then begin
          if not Param.GetNull then begin
            OraLob := Param.GetAsOraBlob;
            CreateLobTemporary(OraLob);
          end;
        end
        // DML array
        else begin
          for Index := 1 to Param.GetLength do begin
            OraLob := TOraLob(Param.GetItemAsObject(Index));
            if not OraLob.IsEmpty then
              CreateLobTemporary(OraLob);
          end;
        end;  
    end
  else
  if CommitOnSuccess then
    for i := 0 to FParams.Count - 1 do
      if TOraParamDesc(FParams[i]).DataType in [dtOraBlob, dtOraClob] then begin
        CommitOnSuccess := False;
        break;
      end;

  if (FSQLType = SQL_SELECT) and (FCursorRef <> nil) and (FCursorRef.FScrollable) then begin
    Res := InternalExecute(OCI_SCROLLABLE_CURSOR)
  end
  else
    if CommitOnSuccess then
      Res := InternalExecute(OCI_COMMIT_ON_SUCCESS)
    else
      Res := InternalExecute(OCI_DEFAULT);

  // remember ROWID value for updates without explicit prepare
  if FStoreRowId then
    FRowId := GetRowId
  else
    FRowId := '';

  // Reading/Writing Piecewise data
    if Res <> OCI_SUCCESS then begin
      Buf := Marshal.AllocHGlobal(BufSize + 1);
      try
        if FOCICallStyle = OCI80 then begin
          GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);
          repeat
            for i := 0 to FParams.Count - 1 do
              if TOraParamDesc(FParams[i]).FHandle = hBind then
                break;
            Assert(i < FParams.Count);

            Blob := TOraParamDesc(FParams[i]).GetAsBlobRef;

            if Mode = pdInput then begin
              Piece := Blob.FirstPiece;
              if IntPtr(Piece) <> nil then
                repeat
                  BufLen := Piece.Used;
                  if IntPtr(Piece.Next) = nil then
                    PieceStatus := OCI_LAST_PIECE;

                  SetPI(hBind, OCI_HTYPE_BIND, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, nil);

                  Res := InternalExecute(OCI_DEFAULT);

                  GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

                  Piece := Piece.Next;
                until (Res = OCI_SUCCESS) or (PieceStatus = OCI_FIRST_PIECE) or {WAR} (PieceStatus = OCI_ONE_PIECE)
              else begin
                BufLen := 0;
                SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, Buf, BufLen, nil);
                Res := InternalExecute(OCI_DEFAULT);
                GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);
              end;
            end
            else
            if Mode = pdOutput then begin
              Blob.Clear;

              repeat
                BufLen := Blob.PieceSize;
                Blob.AllocPiece(Piece, BufLen);

                SetPI(hBind, OCI_HTYPE_BIND, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, nil);

                Res := InternalExecute(OCI_DEFAULT);

                Piece.Used := BufLen;
                if BufLen < Cardinal(Blob.PieceSize) then
                  Blob.ReallocPiece(Piece, BufLen);
                Blob.AppendPiece(Piece);

                GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

              until (Res = OCI_SUCCESS) or (PieceStatus = OCI_FIRST_PIECE) or (PieceStatus = OCI_ONE_PIECE);
            end;
          until Res = OCI_SUCCESS;
        end
        else
        if FOCICallStyle = OCI73 then begin
          NeedData := OCI_STILL_IS_PIECE1;
          IsData := OCI_STILL_IS_PIECE;

          // IN params
          for i := 0 to FParams.Count - 1 do begin
            Param := TOraParamDesc(FParams[i]);
            if (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) and
              (Param.ParamType in [pdInput, pdInputOutput]) and not Param.GetNull// and (FOCICallStyle = OCI73) or {for OCI73}
              //((GetParam(i).ParamType in [pdInput, pdInputOutput]) and{or} not GetParam(i).IsNull) and (FOCICallStyle = OCI80))    {for OCI80}
            then begin
              if Res <> NeedData then begin
                Warning(STooManyInputParams);
                break;
              end;

              GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

              Blob := Param.GetAsBlobRef;
              Assert(Blob <> nil);

              Piece := Blob.FirstPiece;
              if IntPtr(Piece) <> nil then
                repeat
                  BufLen := Piece.Used;
                  if IntPtr(Piece.Next) = nil then
                    PieceStatus := OCI_LAST_PIECE;

                  SetPI(hBind, OCI_HTYPE_BIND, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, nil);

                  Res := InternalExecute(OCI_DEFAULT);

                  GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

                  Piece := Piece.Next;
                until (Res = OCI_SUCCESS) or (PieceStatus = OCI_FIRST_PIECE) or {WAR} (PieceStatus = OCI_ONE_PIECE)
              else begin
                BufLen := 0;
                SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, Buf, BufLen, nil);
                Res := InternalExecute(OCI_DEFAULT);
              end;
            end;
          end;

          if (Res = NeedData) then begin
            Warning(SNotEnoughInputParams);
            while Res = NeedData do begin
              BufLen := 0;
              SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, Buf, BufLen, nil);
              Res := InternalExecute(OCI_DEFAULT);
            end;
          end;

          // OUT params
          for i := 0 to FParams.Count - 1 do begin
            Param := TOraParamDesc(FParams[i]);
            if (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) and
              (Param.ParamType in [pdOutput, pdInputOutput, pdResult])
            then begin
              if Res <> IsData then begin
                Warning(STooNanyOutputParams);
                break;
              end;

              GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

              Blob := Param.GetAsBlobRef;
              Assert(Blob <> nil);

              Blob.Clear;

              repeat
                BufLen := Blob.PieceSize;
                Blob.AllocPiece(Piece, BufLen);

                SetPI(hBind, OCI_HTYPE_BIND, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, nil);

                Res := InternalExecute(OCI_DEFAULT);

                Piece.Used := BufLen;
                if BufLen < Cardinal(Blob.PieceSize) then
                  Blob.ReallocPiece(Piece, BufLen);
                Blob.AppendPiece(Piece);

                GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode);

              until (Res = OCI_SUCCESS) or (PieceStatus = OCI_FIRST_PIECE) or (PieceStatus = OCI_ONE_PIECE);
            end;
          end;

          if Res = IsData then begin
            Warning(SNotEnoughOutputParams);
            while Res = IsData do begin
              BufLen := BufSize;
              GetPI(hBind, PieceStatus, Ptr, Iteration, Index, Mode); // ???
              SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, Buf, BufLen, nil);
              Res := InternalExecute(OCI_DEFAULT);
            end;
          end;

          // OCI73 bug - damage data of last piece
          BufLen := 0;
          SetPI(hBind, OCI_HTYPE_BIND, OCI_LAST_PIECE, nil, BufLen, nil);
        end
        else begin
          CheckOCI;
        end;
      finally
        Marshal.FreeHGlobal(Buf);
      end;
    end;

// Reading/Writing LOBs, Updating DML Array params
  if (Res = OCI_SUCCESS) and (FOCICallStyle = OCI80) and (FRowsProcessed > 0) then
    for i := 0 to FParams.Count - 1 do begin
      Param := TOraParamDesc(FParams[i]);
      if (Param.FLength > 1) and (Param.ParamType in [pdUnknown, pdOutput, pdInputOutput, pdResult]) then begin
        for j := 1 to Param.FLength do begin
          case Param.DataType of
            dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}: begin
              ItemObject := Param.GetItemAsObject(j);
              if ItemObject <> nil then
                Marshal.WriteInt16(TOraTimeStamp(ItemObject).FIndicator, Marshal.ReadInt16(Param.FIndicator, (j - 1) * sizeof(sb2)));
            end;
            dtIntervalYM, dtIntervalDS: begin
              ItemObject := Param.GetItemAsObject(j);
              if ItemObject <> nil then
                Marshal.WriteInt16(TOraInterval(ItemObject).FIndicator, Marshal.ReadInt16(Param.FIndicator, (j - 1) * sizeof(sb2)));
            end;
            dtNumber, dtBCD{$IFDEF VER6P}, dtLargeint{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}: begin
              ItemObject := Param.GetItemAsObject(j);
              if ItemObject <> nil then
                Marshal.WriteInt16(TOraNumber(ItemObject).FIndicator, Marshal.ReadInt16(Param.FIndicator, (j - 1) * sizeof(sb2)));
            end;
          end;
        end;
      end
      else
        if Param.DataType in [dtOraBlob,dtOraClob] then begin
          Param.GetAsOraBlob.Cached := FCacheLobs or (Param.GetParamType = pdInput);
          if Param.GetAsOraBlob.Cached then begin
            if Param.ParamType in [pdInput] then begin
              if not FTemporaryLobUpdate and (not Param.GetNull) then begin
              {$IFDEF HAVE_COMPRESS}
                if (Param.DataType = dtOraBlob) and (FCompressBlob in [cbServer, cbClientServer]) then
                  Param.GetAsOraBlob.Compressed := True
                else
                  Param.GetAsOraBlob.Compressed := False;
              {$ENDIF}
                Param.GetAsOraBlob.WriteLob;
              end;
            end
            else if (Param.ParamType in [pdOutput, pdResult]) or
                     FTemporaryLobUpdate and (Param.ParamType in [pdInputOutput])
            then begin
              if not Param.GetNull then begin
                Param.GetAsOraBlob.ReadLob;
              {$IFDEF HAVE_COMPRESS}
                if (Param.DataType = dtOraBlob) and (FCompressBlob in [cbClient, cbClientServer]) then
                  Param.GetAsOraBlob.Compressed := True
                else
                  Param.GetAsOraBlob.Compressed := False;
              {$ENDIF}
              end
              else begin
                Param.GetAsOraBlob.FreeBlob;
                Param.GetAsOraBlob.Clear;
              end;
            end
            else
              RaiseError(SNeedParamType);
          end;
        end
        else
          if Param.DataType = dtXML then
            if Param.ParamType in [pdOutput, pdInputOutput, pdResult] then
              TOraXML(Param.GetObject).ReadXML;
    end;

// Adjust table params FActualLength
  {
  for i := 0 to FParams.Count - 1 do begin
    if Param.FTable then
      if Param.FActualLength < Param.FLength then
        FillChar(Param.FIndicator,
  }

// AutoCommit
  if (Res = OCI_SUCCESS) and
     (FSQLType <> SQL_SELECT) and
     FAutoCommit and
     FConnection.AutoCommit and
     not CommitOnSuccess
  then
    FConnection.GetInternalTransaction.Commit;
end;

function TOCICommand.CallbackInBind(Bind: pOCIBind; Iter: ub4; Index: ub4;
  var Buf: IntPtr; var BufLen: ub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;
var
  i: word;
  Param: TOraParamDesc;
  Blob: TBlob;
  Piece: PPieceHeader;
begin
  for i := 0 to FParams.Count - 1 do
    if TOraParamDesc(FParams[i]).FHandle = Bind then
      break;
  Assert(i < FParams.Count);
  Param := TOraParamDesc(FParams[i]);
  Blob := Param.GetAsBlobRef;

  Piece := Blob.FirstPiece;
  if PieceStatus = OCI_FIRST_PIECE then
    Param.FBlobPiece := 0
  else begin
    i := 0;
    while (IntPtr(Piece) <> nil) and (i < Param.FBlobPiece) do begin
      Piece := Piece.Next;
      Inc(i);
    end;
  end;

  if IntPtr(Piece) <> nil then begin
    Buf := PieceData(Piece);
    BufLen := Piece.Used;
    Ind := Param.IndicatorPtr;
    if IntPtr(Piece.Next) = nil then
      PieceStatus := OCI_LAST_PIECE
    else
      if Param.FBlobPiece = 0 then
        PieceStatus := OCI_FIRST_PIECE
      else
        PieceStatus := OCI_NEXT_PIECE;
    Inc(Param.FBlobPiece);
  end
  else begin
    Buf := nil;
    BufLen := 0;
    Ind := nil;
    PieceStatus := OCI_LAST_PIECE;
  end;

  Result := OCI_CONTINUE;
end;

function TOCICommand.CallbackOutBind(Bind: pOCIBind; Iter: ub4; Index: ub4;
  var Buf: IntPtr; var BufLen: pub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;
var
  i: integer;
  Blob: TBlob;
  Piece: PPieceHeader;
  Len: cardinal;
begin
  for i := 0 to FParams.Count - 1 do
    if TOraParamDesc(FParams[i]).FHandle = Bind then
      break;
  Assert(i < FParams.Count);

  Blob := TOraParamDesc(FParams[i]).GetAsBlobRef;

  if PieceStatus = OCI_ONE_PIECE then
    Blob.Clear;

  Len := Blob.PieceSize;
  Blob.AllocPiece(Piece, Len);
  Blob.AppendPiece(Piece);

  Buf := PieceData(Piece);

  Piece.Used := Len;
  BufLen := PieceUsedPtr(Piece);

  Ind := TOraParamDesc(FParams[Index]).IndicatorPtr;

  PieceStatus := OCI_NEXT_PIECE;

  Result := OCI_CONTINUE;
end;

function OCICallbackInBind(ictxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
  var bufp: IntPtr; var alen: ub4; var piece: ub1; var indp: IntPtr): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}
begin
  Result := TOCICommand(GetGCHandleTarget(ictxp)).CallbackInBind(bindp, iter, index, bufp, alen,
    piece, indp);
end;

function OCICallbackOutBind(octxp: IntPtr; bindp: pOCIBind; iter: ub4; index: ub4;
  var bufp: IntPtr; var alen: pub4; var piece: ub1; var indp: IntPtr; var rcodep: pub2): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}
begin
  Result := TOCICommand(GetGCHandleTarget(octxp)).CallbackOutBind(bindp, iter, index, bufp, alen,
    piece, indp);
  rcodep := nil;
end;

function TOCICommand.GetFieldDesc7(FieldNo: integer; var Field: TFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
const
  MaxFieldNameSize = 50;
var
  dbsize: sb4;
  dbtype: sb2;
  cbuf: AnsiString;
  Ptr: IntPtr;
  cbufl: sb4;
  dsize: sb4;
  prec: sb2;
  scale: sb2;
  nullok: sb2;
  Res: sword;
  Len: Integer;
  Str: string;
begin
  cbufl := MaxFieldNameSize;

  Ptr := Marshal.AllocHGlobal(MaxFieldNameSize + 1);
  try
    Res := odescr(FCursorRef.CDA, FieldNo, dbsize, dbtype, Ptr,
      cbufl, dsize, prec, scale, nullok);
    if Res = 0 then
      cbuf := Marshal.PtrToStringAnsi(Ptr, cbufl);
  finally
    Marshal.FreeHGlobal(Ptr);
  end;

  if Res = 0 then begin
    Field.Name := string(cbuf);
    Field.ActualFieldNo := FieldNo;
    Field.Length := 0;
    Field.Scale := 0;
    Field.SubDataType := dtUnknown;

    case dbtype of
      VARCHAR2_TYPE,CHAR_TYPE: begin
        if (dbsize <= 255) or LongString then begin  // for IDSBase compatibility
          if (dbsize >= FlatBufferLimit) and not FlatBuffer then begin
            Field.DataType := dtExtString;
            Field.Size := sizeof(IntPtr);
          end
          else begin
            Field.DataType := dtString;
            Field.Size := dbsize + 1;  // for terminator
          end;
        end
        else begin
          Field.DataType := dtMemo;
          Field.SubDataType := dtString;
          Field.Size := sizeof(IntPtr);
        end;
        Field.Length := dbsize; // !is used to differ from Memo
        Field.Fixed := dbtype = CHAR_TYPE;
      end;
      NUMBER_TYPE:
        if FFieldsAsString or FNumberAsString then begin
          Field.DataType := dtString;
          Field.SubDataType := dtNumber;
          if prec > 0 then
            Field.Length := prec
          else
            Field.Length := 38;
          Field.Scale := Abs(scale);
          Field.Size := Field.Length + 3;
        end
        else begin
          Field.Length := prec;
          Field.Scale := Abs(scale);
          if not FConnection.FEnableIntegers or (Abs(scale) > 0) or (prec > GetIntegerPrecision)
            or (prec = 0)
          then begin
            Field.DataType := dtFloat;
            Field.Size := sizeof(Double);
          end
          else begin
            Field.DataType := dtInteger;
            Field.Size := sizeof(Integer);
          end;
        end;
      DATE_TYPE: begin
        if FFieldsAsString then begin
          Str := UpperCase(FConnection.FNlsParams[nlsDateFormat].Value);
          Len := Length(Str);
          if Pos('MONTH', Str) > 0 then
            Len := Len + 7;
          if Pos('DAY', Str) > 0 then
            Len := Len + 9;
          Field.DataType := dtString;
          Field.Size := Len + 1;
          Field.Length := Len;
        end
        else begin
          Field.DataType := dtDateTime;
          Field.Size := 7;
        end;
      end;
      ROWID_TYPE: begin
        Field.DataType := dtString; //dtRowId;  //WAR
        Field.SubDataType := dtRowId;
        Field.Size := RowIdSize + 1; // for terminator //dbsize;
        Field.Length := RowIdSize;
      end;
      RAW_TYPE:
        if FFieldsAsString or FRawAsString then begin
          Field.DataType := dtString;
          Field.Size := dbsize*2 + 1;  // for terminator and heximal represent
          Field.Length := dbsize*2;
        end
        else begin
          Field.DataType := dtVarBytes;
          Field.Size := dbsize + SizeOf(Word);
          Field.Length := dbsize;
        end;
      LONG_TYPE: begin
        Field.DataType := dtMemo;
        Field.Size := sizeof(IntPtr);
      end;
      LONGRAW_TYPE: begin
        Field.DataType := dtBlob;
        Field.Size := sizeof(IntPtr);
      end;
      SQLT_OSL: begin
        Field.DataType := dtLabel;
        Field.Size := dbSize;
      end;
    else
      RaiseError(SDataTypeNotSupported);
      Field.DataType := dtUnknown;
    end;

    Field.Required := nullok = 0;
    Result := True;
  end
  else begin
    if Res <> OCI_VAR_NOT_IN_LIST then
      Check(Res);
    Result := False;
  end;
end;

function TOCICommand.GetFieldDesc8(FieldNo: integer; var Field: TFieldDesc; LongString: boolean; FlatBuffer: boolean): boolean;
var
  hParam: pOCIParam;
  Len: integer;
  Name: _string;
  Res: sword;
  OraType: sb2;
  DataSize: sb2;
  Prec: sb2;
  Scale, OldScale: sb1;
  IsNull: ub1;
  TypeName: _string;
  SchemaName: _string;
  Count: integer;
  ValueInt: Integer;
  Ptr, ValuePtr, StrPtr: IntPtr;
  NationalCharset : boolean;
  CharUsed: boolean;

  procedure GetNlsFieldDesc(NlsType: TNlsParamType);
  var
    Str: _string;
  begin
    if FConnection.FNlsParams[NlsType].Value = '' then
      FConnection.GetSessionParameters;

    Str := _UpperCase(FConnection.FNlsParams[NlsType].Value);
    Len := Length(Str);
    if Pos('MONTH', Str) > 0 then
      Len := Len + 7;
    if Pos('DAY', Str) > 0 then
      Len := Len + 9;
    if (NlsType in [nlsTimeStampFormat, nlsTimeStampTZFormat]) and (Pos('SSXFF', Str) > 0) then
      Len := Len + 4;
    if FConnection.FUseUnicode then begin
      Field.DataType := dtWideString;
      Field.Size := (Len + 1) * sizeof(WideChar);
    end
    else begin
      Field.DataType := dtString;
      Field.Size := Len + 1;
    end;
    Field.Length := Len;
  end;

begin
  Result := False;

  Ptr := Marshal.AllocHGlobal(sizeof(integer));
  try
    Check(OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, ValueInt, nil,
      OCI_ATTR_PARAM_COUNT, hOCIError));
    Count := Integer(ValueInt);

    if FieldNo > Count then
      Exit;

    hParam := nil;
    Res := OCIParamGet(FCursorRef.OCIStmt, OCI_HTYPE_STMT, hOCIError, hParam, FieldNo);

    (*if hParam = nil {Res = OCI_NO_DATA} then  //WAR  ORACLE BAG
      Exit;*)

    try
      Check(Res);

      StrPtr := nil;
      ValuePtr := OrdinalToPtr(StrPtr);
      try
        Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_NAME, hOCIError));
      finally
        PtrToOrdinal(ValuePtr, StrPtr);
      end;
      Len := Marshal.ReadInt32(Ptr);
      Name := PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv);

      Field.Name := Name;
      Field.ActualFieldNo := FieldNo;
      Field.Length := 0;
      Field.Scale := 0;
      Field.SubDataType := dtUnknown;

      Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_TYPE, hOCIError));
      OraType := sb2(ValueInt);

      Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_SIZE, hOCIError));
      DataSize := sb2(ValueInt);

      if OraType in [SQLT_NTY,SQLT_REF] then begin
        StrPtr := nil;
        ValuePtr := OrdinalToPtr(StrPtr);
        try
          Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_TYPE_NAME, hOCIError));
        finally
          PtrToOrdinal(ValuePtr, StrPtr);
        end;
        Len := Marshal.ReadInt32(Ptr);
        TypeName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv));

        StrPtr := nil;
        ValuePtr := OrdinalToPtr(StrPtr);
        try
          Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_SCHEMA_NAME, hOCIError));
        finally
          PtrToOrdinal(ValuePtr, StrPtr);
        end;
        Len := Marshal.ReadInt32(Ptr);
        SchemaName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv));

        if ObjectTypes <> nil then
          Field.ObjectType := ObjectTypes.FindType(FConnection.GetSvcCtx, QuotedOCIName(SchemaName + '.' + TypeName))
        else
          Field.ObjectType := nil;

        if Field.ObjectType = nil then begin
          Field.ObjectType := TOraType.Create(FConnection, QuotedOCIName(SchemaName + '.' + TypeName));
          Field.ObjectType.Release;
        end;
      end;

      case OraType of
        SQLT_CHR,SQLT_AFC: begin
          Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, hOCIError));
          NationalCharset := ub1(ValueInt) = SQLCS_NCHAR;
          Field.Length := DataSize; // is used to differ from Memo
          if OCIVersion >= 9000 then begin
            Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHAR_USED, hOCIError));
            CharUsed := ub1(ValueInt) = 1;
            // if filed size is defined in characters (not bytes) then FieldSize = Size * Length of cgaracter
            if CharUsed then
              if FConnection.FUseUnicode {$IFDEF LITE}or (FConnection.FCharLength = 1){$ENDIF} then begin
                Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHAR_SIZE, hOCIError));
                Field.Length := sb2(ValueInt);
              end
              // for NVARCAHR and FCharLength = 1 - each char has size 2 bytes
              else if NationalCharset and (FConnection.FCharLength = 1) then begin
                Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHAR_SIZE, hOCIError));
                Field.Length := sb2(ValueInt) * 2;
              end
              // in non Unicode mode and CharLength > 1 size of field = length in chars * size of char in bytes
              else if FConnection.FCharLength > 1 then begin
                Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHAR_SIZE, hOCIError));
                Field.Length := sb2(ValueInt) * FConnection.FCharLength;
              end;
          end;
          if FConnection.ConvertEOL then
            Field.Length := Field.Length * 3 div 2; // data can grow on ConvertEOL

          Field.Fixed := OraType = SQLT_AFC;

          if FConnection.FUseUnicode then begin
            if (Field.Length <= 255) or LongString  then begin  // for IDSBase compatibility
              if (Field.Length >= FlatBufferLimit) and not FlatBuffer then begin
                Field.DataType := dtExtWideString;
                Field.Size := sizeof(IntPtr);
              end
              else begin
                Field.DataType := dtWideString;
                Field.Size := (Field.Length + 1) * 2;  // for terminator
              end;
            end
            else begin
              Field.DataType := dtWideMemo;
              Field.SubDataType := dtWideString;
              Field.Size := sizeof(IntPtr);
            end;
            if NationalCharset then
              Field.SubDataType := dtNWideString;
          end
          else begin
            if (Field.Length <= 255) or LongString  then begin  // for IDSBase compatibility
              if (Field.Length >= FlatBufferLimit) and not FlatBuffer then begin
                Field.DataType := dtExtString;
                Field.Size := sizeof(IntPtr);
              end
              else begin
                Field.DataType := dtString;
                Field.Size := Field.Length + 1;  // for terminator
              end;
            end
            else begin
              Field.DataType := dtMemo;
              Field.SubDataType := dtString;
              Field.Size := sizeof(IntPtr);
            end;
            if NationalCharset then
              Field.SubDataType := dtNString;
          end;
        end;
        SQLT_NUM: begin
          Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PRECISION, hOCIError));
          Prec := sb2(ValueInt);
          Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_SCALE, hOCIError));
          Scale := sb1(ValueInt);
          Field.Length := Prec;
          if FFieldsAsString or FNumberAsString then begin
            if FConnection.FUseUnicode then
              Field.DataType := dtWideString
            else
              Field.DataType := dtString;
            Field.SubDataType := dtNumber;
            Field.Scale := Abs(Scale);
            if Prec > 0 then begin
              if Scale = -127 then begin //FLOAT
                Field.Length := Ceil(Prec * 0.30103);
                Field.Size := 255; // string in fixed point format
              end
              else begin
                Field.Length := Prec + 2;
                Field.Size := Field.Length + 1;
              end
            end
            else begin
              // max scale = 130
              Field.Length := 132;
              Field.Size := Field.Length + 1;
            end;
            if FConnection.FUseUnicode then
              Field.Size := Field.Size * 2;
          end
          else begin
            OldScale := 0;
            if (Scale = -127) and (Prec > 0) then //FLOAT
              Field.Length := Ceil(Prec * 0.30103)
            else begin   //NUMBER
              if Prec = 0 then
                Prec := 38;
              if Scale = - 127 then begin
                if not NumberAsInteger then
                  OldScale := Scale;
                Scale := 0
              end;
              Field.Length := Prec;
            end;
            Field.Scale := Abs(Scale);
            if OldScale > 0 then
              Scale := OldScale;
            if (FUseDefaultDataTypes or FConnection.FEnableIntegers) and
              (Prec <= GetSmallintPrecision) and (Scale = 0)
            then begin
              Field.DataType := dtSmallint;
              Field.Size := sizeof(Smallint);
            end
            else
            if (FUseDefaultDataTypes or FConnection.FEnableIntegers) and
              (Prec <= GetIntegerPrecision) and (Scale = 0)
            then begin
              Field.DataType := dtInteger;
              Field.Size := sizeof(Integer);
            end
            else
            if (FUseDefaultDataTypes or FConnection.FEnableIntegers) and
              (Prec <= GetLargeIntPrecision) and (Scale = 0)
            then begin
              Field.DataType := dtLargeint;
              Field.Size := sizeof(Int64);
            end
            else
            if not FUseDefaultDataTypes and (FConnection.EnableBCD or FEnableBCD) and
              (Prec <= GetBCDPrecision) and (Scale >= 0) and (Scale <= GetBCDScale)
            then begin
              Field.DataType := dtBCD;
              Field.Size := sizeof(Currency);
              if Field.Length > MaxBcdPrecision then
                Field.Length := MaxBcdPrecision;
              if Field.Scale > MaxBcdScale then
                Field.Scale := MaxBcdScale;
            end
            else
            if Field.Length <= GetFloatPrecision then begin
              Field.DataType := dtFloat;
              Field.Size := sizeof(Double);
            end
          {$IFDEF VER6P}
          {$IFNDEF FPC}
            else
            if not FUseDefaultDataTypes and (FConnection.EnableFMTBCD or FEnableFMTBCD) and
              (Prec <= GetFmtBCDPrecision) and (Scale >= 0) and (Scale <= GetFmtBCDScale)
            then begin
              Field.DataType := dtFMTBCD;
              Field.Size := SizeOfTBcd;
              if Field.Length > MaxFMTBcdDigits then
                Field.Length := MaxFMTBcdDigits;
              if Field.Scale > Field.Length then // if length was reduced
                Field.Scale := Field.Length;
            end
          {$ENDIF}
          {$ENDIF}
            else
            if not FUseDefaultDataTypes and FConnection.FEnableNumbers then begin
              Field.DataType := dtNumber;
              Field.Size := sizeof(IntPtr);
            end
            else begin
              Field.DataType := dtFloat;
              Field.Size := sizeof(Double);
            end
          end;
        end;
        SQLT_DAT: begin
          if FFieldsAsString then
            GetNlsFieldDesc(nlsDateFormat)
          else begin
            Field.DataType := dtDateTime;
            Field.Size := 7;
          end;
        end;
        SQLT_RID: begin // ROWID for Oracle 7
          Field.DataType := dtString;
          Field.SubDataType := dtRowId;
          Field.Length := RowIdSize;
          Field.Size := RowIdSize + 1;

          if FConnection.FUseUnicode then begin
            Field.DataType := dtWideString;
            Field.Size := Field.Size * 2;
          end
        end;
        SQLT_RDD: begin // ROWID is bound as string
          Field.DataType := dtString;
          if DataSize > SizeOf(IntPtr) then begin // UROWID has its actual datasize
            Field.SubDataType := dtURowId;
            Field.Length := Ceil(DataSize * 1.34); // for correct conversion into 64 base string
          end
          else begin
            Field.SubDataType := dtRowId;
            Field.Length := RowIdSize;
          end;
          Field.Size := Field.Length + 1; // for terminator

          if FConnection.FUseUnicode then begin
            Field.DataType := dtWideString;
            Field.Size := Field.Size * 2;
          end
        end;
        SQLT_BIN:
          if FFieldsAsString or FRawAsString then begin
            if FConnection.FUseUnicode then begin
              Field.DataType := dtWideString;
              Field.Size := DataSize * 4 + 2;  // for terminator and heximal represent
              Field.Length := DataSize * 4;
            end
            else begin
              Field.DataType := dtString;
              Field.Size := DataSize * 2 + 1;  // for terminator and heximal represent
              Field.Length := DataSize * 2;
            end;
          end
          else begin
            if (DataSize >= FlatBufferLimit) and not FlatBuffer then begin
              Field.DataType := dtExtVarBytes;
              Field.Size := SizeOf(IntPtr);
            end
            else begin
              Field.DataType := dtVarBytes;
              Field.Size := DataSize + SizeOf(Word);
            end;
            Field.Length := DataSize;
          end;
        SQLT_LNG: begin
          if FConnection.FUseUnicode then
            Field.DataType := dtWideMemo
          else
            Field.DataType := dtMemo;
          Field.Size := sizeof(IntPtr);
        end;
        SQLT_LBI: begin
          Field.DataType := dtBlob;
          Field.Size := sizeof(IntPtr);
        end;
        SQLT_CLOB: begin
          if FConnection.FUseUnicode and FConnection.FEnableWideOraClob then
            Field.DataType := dtWideOraClob
          else
            Field.DataType := dtOraClob;
          Field.Size := sizeof(IntPtr);

            Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, hOCIError));
            NationalCharset := ub1(ValueInt) = SQLCS_NCHAR;
          if NationalCharSet then
            Field.SubDataType := dtNCLOB;
        end;
        SQLT_BLOB: begin
          Field.DataType := dtOraBlob;
          Field.Size := sizeof(IntPtr);
        end;
        SQLT_BFILEE: begin
          Field.DataType := dtBFILE;
          Field.Size := sizeof(IntPtr);
        end;
        SQLT_CFILEE: begin
          Field.DataType := dtCFILE;
          Field.Size := sizeof(IntPtr);
        end;
        SQLT_RSET: begin
          Field.DataType := dtCursor;
          Field.Size := sizeof(IntPtr);
        end;
        SQLT_NTY: begin
          if (OCIVersion < 9200) and (TOraType(Field.ObjectType).DataType = dtXML) then
            RaiseError(SDataTypeNotSupported);
          Field.DataType := TOraType(Field.ObjectType).DataType;
          Field.Size := sizeof(IntPtr);
        end;
        SQLT_REF: begin
          if (OCIVersion < 9200) and (TOraType(Field.ObjectType).DataType = dtXML) then
            RaiseError(SDataTypeNotSupported);
          Field.DataType := dtReference;
          Field.Size := sizeof(IntPtr);
        end;
        SQLT_OSL: begin
          Field.DataType := dtLabel;
          Field.Size := DataSize;
        end;
        SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ, SQLT_INTERVAL_YM,
        SQLT_INTERVAL_DS:
        begin
          if OCIVersion < 9000 then
            RaiseError(SDataTypeNotSupported);

          if FFieldsAsString
          {$IFDEF VER6P}
            or FConnection.FIntervalAsString and (OraType in [SQLT_INTERVAL_YM, SQLT_INTERVAL_DS])
          {$IFDEF FPC}
            or FConnection.FTimeStampAsString and (OraType in [SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ])
          {$ENDIF}
          {$ENDIF}
          then
            case OraType of
              SQLT_TIMESTAMP, SQLT_TIMESTAMP_LTZ:
                GetNlsFieldDesc(nlsTimeStampFormat);
              SQLT_TIMESTAMP_TZ:
                GetNlsFieldDesc(nlsTimeStampTZFormat);
              SQLT_INTERVAL_YM, SQLT_INTERVAL_DS: begin
                if FConnection.FUseUnicode then
                  Field.DataType := dtWideString
                else
                  Field.DataType := dtString;
                Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PRECISION, hOCIError));
                Prec := sb2(ValueInt);
                if OraType = SQLT_INTERVAL_YM then
                  Field.Length := Prec + 4 // interval year to month form is +Prec-MM
                else begin
                  Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_SCALE, hOCIError));
                  Scale := sb1(ValueInt);
                  Field.Length := Prec + Scale + 11; // interval day to second form is +Prec HH:MM:SS.Scale
                end;
                Field.Size := Field.Length + 1;
                if FConnection.FUseUnicode then
                  Field.Size := Field.Size * 2;

                if OraType = SQLT_INTERVAL_YM then
                  Field.SubDataType := dtIntervalYM
                else
                  Field.SubDataType := dtIntervalDS;
              end;
            end
          else begin
            Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_FSPRECISION, hOCIError));
            Field.Length := Word(ValueInt);
            case OraType of
              SQLT_INTERVAL_YM:
                Field.DataType := dtIntervalYM;
              SQLT_INTERVAL_DS:
                Field.DataType := dtIntervalDS;
            else
            {$IFDEF VER6P}
            {$IFNDEF FPC}
              if FConnection.FEnableSQLTimeStamp then
                Field.DataType := dtSQLTimeStamp
              else
            {$ENDIF}
            {$ENDIF}
              case OraType of
                SQLT_TIMESTAMP:
                  Field.DataType := dtTimeStamp;
                SQLT_TIMESTAMP_TZ:
                  Field.DataType := dtTimeStampTZ;
                SQLT_TIMESTAMP_LTZ:
                  Field.DataType := dtTimeStampLTZ;
              end;
            end;
          {$IFDEF VER6P}
          {$IFNDEF FPC}
            if Field.DataType = dtSQLTimeStamp then
              Field.Size := sizeof(TSQLTimeStamp)
            else
          {$ENDIF}
          {$ENDIF}
              Field.Size := sizeof(IntPtr);
          end;
        end;
        SQLT_UND: begin
          Field.DataType := dtUndefined;
          Field.Size := DataSize;
        end;
        SQLT_IBFLOAT: begin
          Field.DataType := dtFloat;
          Field.SubDataType := dtBFloat;
          Field.Size := sizeof(Double);
          Field.Length := 38;
          Field.Scale := 127;
        end;
        SQLT_IBDOUBLE: begin
          Field.DataType := dtFloat;
          Field.SubDataType := dtBDouble;
          Field.Size := sizeof(Double);
          Field.Length := 38;
          Field.Scale := 127;
        end;
        else
          RaiseError(SDataTypeNotSupported);
      end;

      Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_IS_NULL, hOCIError));
      IsNull := Byte(ValueInt);

    finally
      // free memory after OCIParamGet
      OCIDescriptorFree(hParam, OCI_DTYPE_PARAM);
    end;
      
  finally
    Marshal.FreeHGlobal(Ptr);
  end;

  Field.Required := (IsNull = 0) and (Field.Name <> 'ROWID');
  Result := True;
end;

function TOCICommand.GetFieldDesc(FieldNo: integer; var Field: TFieldDesc;
  LongString: boolean; FlatBuffer: boolean): boolean;
begin
  if FOCICallStyle = OCI73 then begin
    Result := GetFieldDesc7(FieldNo, Field, LongString, FlatBuffer);
  end
  else
  if FOCICallStyle = OCI80 then begin
    Result := GetFieldDesc8(FieldNo, Field, LongString, FlatBuffer);
  end
  else begin
    Result := False;
    CheckOCI;
  end;
end;

procedure TOCICommand.DefineData(Field: TFieldDesc; Buf: IntPtr; Ind: psb2);
var
  OraType: word;
  hDefine: pOCIDefine;
  ValuePtr: IntPtr;
  IndPtr: IntPtr;
  LenPtr: pub2;
  Size: longword;
  CharsetId : Integer; //sb2
  CharsetForm : Integer;
begin
  OraType := GetOraType(Field.DataType, Field.SubDataType);

  if (Field.DataType = dtExtString) or (Field.SubDataType = dtString) or
     (Field.SubDataType = dtNString) then
    Size := Field.Length + 1
  else
  if (Field.DataType = dtExtWideString) or (Field.SubDataType = dtWideString) or
     (Field.SubDataType = dtNWideString) then
    Size := Field.Length * 2 + 2
  else
    case Field.DataType of
      dtExtVarBytes:
        Size := Field.Length + SizeOf(Word);
      dtNumber, dtLargeint, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
        Size := OCI_NUMBER_SIZE;
    else
      Size := Field.Size;
    end;

  if FOCICallStyle = OCI73 then begin
    case Field.DataType of
      dtVarBytes, dtExtVarBytes: begin
        LenPtr := Buf;
        Buf := PtrOffset(Buf, 2);
      end
    else
      LenPtr := nil;
    end;
    Check(odefin(FCursorRef.CDA, Field.ActualFieldNo, Buf, Size, OraType, -1, Ind, nil,
      -1, -1, LenPtr, pub2(0)));
  end
  else
  if FOCICallStyle = OCI80 then begin
    hDefine := nil;
    LenPtr := nil;
    case Field.DataType of
      dtObject,dtXML,dtReference,dtArray,dtTable:
        ValuePtr := nil;
      dtVarBytes, dtExtVarBytes: begin
        LenPtr := Buf;
        ValuePtr := PtrOffset(Buf, 2);
      end
    else
      ValuePtr := Buf;
    end;

    if Field.DataType = dtXML then
      IndPtr := nil
    else
      IndPtr := Ind;

    Check(OCIDefineByPos(FCursorRef.OCIStmt, hDefine, hOCIError, Field.ActualFieldNo, ValuePtr, Size, OraType,
      IndPtr, LenPtr, nil, OCI_DEFAULT));

    CharsetId := 0;
    if (Field.DataType in [dtString, dtExtString]) or
      (Field.DataType in [dtBlob,dtMemo]) and
      ((Field.SubDataType = dtString) or (Field.SubDataType = dtNString)) then
      CharsetId := Integer(FConnection.FCharsetId);
    if (Field.DataType in [dtWideString, dtExtWideString]) or
      (Field.DataType in [dtBlob,dtWideMemo]) and
      ((Field.SubDataType = dtWideString) or (Field.SubDataType = dtNWideString)) then
      CharsetId := Integer(OCI_UTF16ID);

    if ((Field.SubDataType = dtNString) or (Field.SubDataType = dtNWideString)) then begin
      CharsetForm := SQLCS_NCHAR;
      Check(OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, CharsetForm, 0, OCI_ATTR_CHARSET_FORM, hOCIError));
    end;

    if CharsetId > 0 then
      Check(OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, CharsetId, 0, OCI_ATTR_CHARSET_ID, hOCIError));

    case Field.DataType of
      dtObject,dtXML,dtArray,dtTable:
        Check(OCIDefineObject(hDefine, hOCIError, TOraType(Field.ObjectType).TDO,
          Buf, nil, IntPtr(Ind), nil));
      dtReference:
        Check(OCIDefineObject(hDefine, hOCIError, nil, Buf, nil, nil , nil));
    end;

    Field.Handle := hDefine;
  end
  else
    CheckOCI;
end;

procedure TOCICommand.DefineArrayData(Field: TFieldDesc; Buf: IntPtr;
  Ind: psb2; BufSkip: integer; IndSkip: integer);
var
  OraType: word;
  hDefine: pOCIDefine;
  ValuePtr: IntPtr;
  IndPtr: IntPtr;
  LenPtr: pub2;
  LenSkip: integer;
  Size: longword;
  CharsetId: Integer;//sb2
  CharsetForm : Integer;
begin
  OraType := GetOraType(Field.DataType, Field.SubDataType);

  if (Field.DataType = dtExtString) or (Field.SubDataType = dtString) or
     (Field.SubDataType = dtNString) then  // in future use DataSize
    Size := Field.Length + 1
  else
  if (Field.DataType = dtExtWideString) or (Field.SubDataType = dtWideString) or
     (Field.SubDataType = dtNWideString) then  // in future use DataSize
    Size := Field.Length *2 + 2
  else
    case Field.DataType of
      dtExtVarBytes:
        Size := Field.Length + SizeOf(Word);
      dtNumber, dtLargeint, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
        Size := OCI_NUMBER_SIZE;
    else
      Size := Field.Size;
    end;

  if FOCICallStyle = OCI73 then begin
    case Field.DataType of
      dtVarBytes, dtExtVarBytes: begin
        LenPtr := Buf;
        Buf := PtrOffset(Buf, 2);
        LenSkip := BufSkip;
      end
    else
      LenPtr := nil;
      LenSkip := 0;
    end;
    Check(odefinps(FCursorRef.CDA, 1, Field.ActualFieldNo, Buf, Size, OraType, 0, Ind, nil,
      0, 0, LenPtr, pub2(0), BufSkip, IndSkip, LenSkip, 0));
  end
  else
  if FOCICallStyle = OCI80 then begin
    hDefine := nil;
    LenPtr := nil;
    LenSkip := 0;
    case Field.DataType of
      dtObject,dtXML,dtReference,dtArray,dtTable:
        ValuePtr := nil;
      dtVarBytes,dtExtVarBytes: begin
        LenPtr := Buf;
        LenSkip := BufSkip;
        ValuePtr := PtrOffset(Buf, 2);
      end;
    else
      ValuePtr := Buf;
    end;

    if Field.DataType = dtXML then
      IndPtr := nil
    else
      IndPtr := Ind;

    Check(OCIDefineByPos(FCursorRef.OCIStmt, hDefine, hOCIError, Field.ActualFieldNo,
      ValuePtr, Size, OraType, IndPtr, LenPtr, nil, OCI_DEFAULT));

    CharsetId := 0;
    if (Field.DataType in [dtString, dtExtString]) or
      (Field.DataType in [dtBlob,dtMemo]) and
      ((Field.SubDataType = dtString) or (Field.SubDataType = dtString)) then
      CharsetId := Integer(FConnection.FCharsetId);
    if (Field.DataType in [dtWideString, dtExtWideString]) or
      (Field.DataType in [dtBlob,dtWideMemo]) and
      ((Field.SubDataType = dtWideString) or (Field.SubDataType = dtNWideString)) then
      CharsetId := Integer(OCI_UTF16ID);

    if ((Field.SubDataType = dtNString) or (Field.SubDataType = dtNWideString)) then begin
      CharsetForm := SQLCS_NCHAR;
      Check(OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, CharsetForm, 0, OCI_ATTR_CHARSET_FORM, hOCIError));
    end;

    if CharsetId > 0 then
      Check(OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, CharsetId, 0, OCI_ATTR_CHARSET_ID, hOCIError));

    case Field.DataType of
      dtObject,dtXML,dtArray,dtTable:
        Check(OCIDefineObject(hDefine, hOCIError, TOraType(Field.ObjectType).TDO,
          Buf, nil, IntPtr(Ind), nil));
      dtReference :
        Check(OCIDefineObject(hDefine, hOCIError, nil, Buf, nil, nil , nil));
    end;

    Check(OCIDefineArrayOfStruct(hDefine, hOCIError, BufSkip, IndSkip, LenSkip, 0));

    Field.Handle := hDefine;
  end
  else
    CheckOCI;
end;

procedure TOCICommand.DefinePieceData(Field: TFieldDesc; Buf: IntPtr; Ind: psb2);
var
  OraType: word;
  hDefine: pOCIDefine;
begin
  OraType := GetOraType(Field.DataType, Field.SubDataType);

  if FOCICallStyle = OCI73 then begin
    Check(odefinps(FCursorRef.CDA, 0, Field.ActualFieldNo, Buf, MaxBlobSize, OraType, 0,
      Ind, nil, 0, 0, pub2(0), pub2(0), 0, 0, 0, 0));
  end
  else
  if FOCICallStyle = OCI80 then begin
    hDefine := nil;
    Check(OCIDefineByPos(FCursorRef.OCIStmt, hDefine, hOCIError, Field.ActualFieldNo, Buf, MaxBlobSize, OraType,
      Ind, nil, nil, OCI_DYNAMIC_FETCH));

    Field.Handle := hDefine;
  end
  else
    CheckOCI;
end;

procedure TOCICommand.DefineDynamic(Field: TFieldDesc; {Buf: IntPtr; Ind: psb2;}
  Owner: IntPtr; Proc: IntPtr; CharsetId: integer);
var
  OraType: word;
  hDefine: pOCIDefine;
begin
  CheckOCI80;

  OraType := GetOraType(Field.DataType, Field.SubDataType);

  hDefine := nil;
  Check(OCIDefineByPos(FCursorRef.OCIStmt, hDefine, hOCIError, Field.ActualFieldNo,
    nil, MaxBlobSize, OraType, nil, nil, nil, OCI_DYNAMIC_FETCH));

  Check(OCIDefineDynamic(hDefine, hOCIError, Owner, Proc));

  if CharsetId > 0 then
    Check(OCIAttrSet2(hDefine, OCI_HTYPE_DEFINE, CharsetId, 0, OCI_ATTR_CHARSET_ID, hOCIError));

  Field.Handle := hDefine;
end;

function TOCICommand.InternalFetch7(Rows: word): word;
var
  Res: sword;
begin
//  In PtrCDA^.rpc  problem with cursor ???
  FConnection.BusyWait;
    Res := ofen(FCursorRef.CDA, Rows);
  FConnection.Release;

  if FCursorRef.CDA.rpc > Cardinal(FFetchedRows) then
    Result := FCursorRef.CDA.rpc - Cardinal(FFetchedRows)
  else begin
    Result := 0;
    Res := OCI_NO_DATA_FOUND;
  end;
  FFetchedRows := FCursorRef.CDA.rpc;

  if Res <> 0 then begin
    if Res = OCI_NO_DATA_FOUND then begin
      InternalCancel;
    end
    else
      Check(Res);
  end;
end;

function TOCICommand.InternalFetch8(Rows: word; Orientation: integer; Offset: integer): word;
var
  Res: sword;
  RowsProc: integer;
  ValueInt: Integer;
begin
  FConnection.BusyWait;
    if Orientation = OCI_FETCH_NEXT then
      Res := OCIStmtFetch(FCursorRef.OCIStmt, hOCIError, Rows, OCI_FETCH_NEXT, OCI_DEFAULT)
    else
      Res := OCIStmtFetch2(FCursorRef.OCIStmt, hOCIError, Rows, Orientation, Offset, OCI_DEFAULT);
  FConnection.Release;

  Check(OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, OCI_ATTR_ROW_COUNT, hOCIError));
  RowsProc := Integer(ValueInt);
  if RowsProc > FFetchedRows then
    Result := RowsProc - FFetchedRows
  else
    Result := 0; // bug with 'select level from dual connect by level<=1000' under Direct mode
  FFetchedRows := RowsProc;

  if Res = OCI_NO_DATA then begin
    if not FCursorRef.FScrollable then
      InternalCancel
  end
  else
    try
      Check(Res);
    except
      InternalCancel;
      raise;
    end;
end;

function TOCICommand.InternalExecuteFetch8(Rows: word): word;
var
  Res: sword;
  ValueInt: Integer;
  RowsProc: Integer;
begin
  Res := InternalExecute(OCI_DEFAULT, Rows);

  if Res <> 0 then begin
    Check(OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, OCI_ATTR_ROW_COUNT, hOCIError));
    RowsProc := Integer(ValueInt);
    Result := RowsProc - FFetchedRows;
    FFetchedRows := RowsProc;

    if Res = OCI_NO_DATA then
      if not FCursorRef.FScrollable then
        InternalCancel;
  end
  else begin
    Check(OCIAttrGet2(FCursorRef.OCIStmt, OCI_HTYPE_STMT, ValueInt, nil, OCI_ATTR_ROW_COUNT, hOCIError));
    FFetchedRows := Integer(ValueInt);
    Result := Rows;
  end;
end;

function TOCICommand.InternalFetch(Rows: word; Orientation: integer = OCI_FETCH_NEXT; Offset: integer = 0): word;
var
  FetchingAll: boolean;
begin
  FetchingAll := GetCursorState = csFetchingAll;
  CheckActive;

  if FOCICallStyle = OCI73 then begin
    Result := InternalFetch7(Rows);
  end
  else
  if FOCICallStyle = OCI80 then begin
    if (GetCursorState < csExecuting) then begin
      FetchingAll := GetCursorState = csExecuteFetchAll;
      Result := InternalExecuteFetch8(Rows);
    end
    else
      Result:= InternalFetch8(Rows, Orientation, Offset);
  end
  else begin
    Result := 0;
    CheckOCI;
  end;

  if GetCursorState < csFetching then
    if FetchingAll then
      SetCursorState(csFetchingAll)
    else
      SetCursorState(csFetching);
end;

// For piecewise
function TOCICommand.InternalFetchPiece7: integer;
var
  Res: sword;
begin
  FConnection.BusyWait;
    Res := ofen(FCursorRef.CDA, 1);
  FConnection.Release;

  if Res <> 0 then begin
    if Res = OCI_NO_DATA_FOUND then begin
      InternalCancel;
    end
    else
      if Res <> OCI_STILL_IS_PIECE then
        Check(Res);
  end;

  Result := Res;
end;

function TOCICommand.InternalFetchPiece8(Orientation: integer; Offset: integer): integer;
var
  Res: sword;
begin
  FConnection.BusyWait;
    if Orientation = OCI_FETCH_NEXT then
      Res := OCIStmtFetch(FCursorRef.OCIStmt, hOCIError, 1, OCI_FETCH_NEXT, OCI_DEFAULT)
    else
      Res := OCIStmtFetch2(FCursorRef.OCIStmt, hOCIError, 1, Orientation, Offset, OCI_DEFAULT);
  FConnection.Release;

  if Res <> 0 then begin
    if Res = OCI_NO_DATA then // OCI_SUCCESS_WITH_INFO
      if FCursorRef.FScrollable then
      else
        InternalCancel
    else
      if Res <> OCI_NEED_DATA then
        Check(Res);
  end;

  Result := Res;
end;

function TOCICommand.InternalFetchPiece(Orientation: integer = OCI_FETCH_NEXT; Offset: integer = 0): integer;
begin
  CheckActive;

  if FOCICallStyle = OCI73 then begin
    Result := InternalFetchPiece7;
  end
  else
  if FOCICallStyle = OCI80 then begin
    Result := InternalFetchPiece8(Orientation, Offset);
  end
  else begin
    Result := 0;
    CheckOCI;
  end;

  if GetCursorState < csFetching then
    SetCursorState(csFetching);
end;

procedure TOCICommand.InternalCancel;
begin
  CheckActive;
  FConnection.BusyWait;
  try
    if FOCICallStyle = OCI73 then begin
      Check(ocan(FCursorRef.CDA));
    end
    else
    if FOCICallStyle = OCI80 then begin
      //Check(OCIStmtFetch(Cursor.OCIStmt, hOCIError, 0, OCI_FETCH_NEXT, OCI_DEFAULT));
    end
    else
      CheckOCI;
  finally
    FConnection.Release;
    SetCursorState(csFetched);
  end;
end;

procedure TOCICommand.InternalClose;
begin
  CheckOCI73;
  CheckActive;
  Busy;
  try // For Busy
    FConnection.BusyWait;
    try
      Check(oclose(FCursorRef.CDA));
    finally
      FConnection.Release;
      SetCursorState(csInactive);
    end;
  finally
    Release;
  end;
end;

procedure TOCICommand.Finish;
begin
  CheckActive;

  try
    if FOCICallStyle = OCI73 then begin
      try
        InternalClose;
      finally
        FCursorRef.FreeCursor;
      end;
    end
    else
    if FOCICallStyle = OCI80 then begin
      Busy;
      try
        FConnection.BusyWait;
        try
        // Free statement handle
          FCursorRef.hOCIEnv := FConnection.hOCIEnv;
          FCursorRef.hOCIError := FConnection.hOCIError;
          FCursorRef.FUnicodeEnv := FConnection.FUnicodeEnv;
          FCursorRef.FreeCursor;
        finally
          FConnection.Release;
        end;
      finally
        Release;
      end;
    end
    else
      CheckOCI;
  finally
    SetCursorState(csInactive);
    FSQLType := SQL_UNKNOWN;
  end;
end;

procedure TOCICommand.Unprepare;
begin
  Finish;

  inherited;
end;

function TOCICommand.NativeCursor: boolean;
begin
  Result := FCursorRef = FCursor;
end;

function TOCICommand.RowsReturn: boolean;
begin
  Result := (FSQLType = SQL_SELECT) or
    (FCursorRef <> FCursor) and (FCursorRef.State >= csExecuted);
end;

// Check return SQL statement rows
procedure TOCICommand.CheckRowsReturn;
begin
  if not RowsReturn then
    RaiseError(SNotRows);
end;

procedure TOCICommand.GetPI(var Handle: pOCIHandle; var Piece: byte;
  var Buf: IntPtr; var Iteration: cardinal; var Index: cardinal; var Mode: TParamDirection);
var
  Res: sword;
  HType: ub4;
  IOMode: ub1;
  hIteration, hIndex, hPiece: IntPtr;
  hHType: IntPtr;
  hIOMode: IntPtr;
begin
  if FOCICallStyle = OCI73 then begin
    FConnection.BusyWait;
    Res := ogetpi(FCursorRef.CDA, Piece, Buf, Iteration, Index);
    FConnection.Release;

    Check(Res);
    Handle := nil;
    HType := 0;
    Mode := pdUnknown;
  end
  else
  if FOCICallStyle = OCI80 then begin
    hIteration := OrdinalToPtr(Iteration);
    hIndex := OrdinalToPtr(Index);
    hPiece := OrdinalToPtr(Piece);
    hHType := AllocOrdinal(HType);
    hIOMode := AllocOrdinal(IOMode);
    try
      FConnection.BusyWait;
        Res := OCIStmtGetPieceInfo(FCursorRef.OCIStmt, hOCIError, Handle, hHType,
          hIOMode, hIteration, hIndex, hPiece);
      FConnection.Release;
    finally
      PtrToOrdinal(hIteration, Iteration);
      PtrToOrdinal(hIndex, Index);
      PtrToOrdinal(hPiece, Piece);
      PtrToOrdinal(hHType, HType);
      PtrToOrdinal(hIOMode, IOMode);
    end;

    Check(Res);
    case IOMode of
      OCI_PARAM_IN:
        Mode := pdInput;
      OCI_PARAM_OUT:
        Mode := pdOutput;
    else
      Mode := pdUnknown;
    end;
  end
  else
    CheckOCI;
end;

procedure TOCICommand.SetPI(Handle: pOCIHandle; HType: cardinal; Piece: byte;
  Buf: IntPtr; var BufLen: cardinal; Ind: psb2);
var
  Res: sword;
  HGlobal: IntPtr;
begin
  if FOCICallStyle = OCI73 then begin
    FConnection.BusyWait;
    Res := osetpi(FCursorRef.CDA, Piece, Buf, BufLen);
    FConnection.Release;

    Check(Res);
  end
  else
  if FOCICallStyle = OCI80 then begin
    HGlobal := OrdinalToPtr(BufLen);
    try
      FConnection.BusyWait;
        Res := OCIStmtSetPieceInfo(Handle, HType, hOCIError, Buf, HGlobal, Piece, Ind, nil);
      FConnection.Release;
    finally
      PtrToOrdinal(HGlobal, BufLen);
    end;

    Check(Res);
  end
  else
    CheckOCI;
end;

/// DescSP performs low-level call of the SYS.DBMS_DESCRIBE.DESCRIBE_PROCEDURE
/// to retrieve procedure params. Should be used in InitProcParams7 instead of
/// odessp in Direct mode.
{$IFNDEF VER7P}
{$O-}
{$ENDIF}
function TOCICommand.DescSP(const objnam: _string; ovrld: pub2; pos: pub2; level: pub2;
  argnam: IntPtr; arnlen: pub2; dtype: pub2; defsup: pub1; mode: pub1;
  dtsiz: pub4; prec: psb2; scale: psb2; radix: pub1;
  spare: pub4; var arrsiz: ub4): sword;
var
  hOCIStmt: pOCIStmt;
  SQLText: _string;
  hBind: pOCIBind;
  i, Size: integer;
  Indicator: IntPtr;
  p, PObjName, PArrSize: IntPtr;
  //ValuePtr: Integer;
begin
  {Result := OCIAttrGet2(FConnection.GetSvcCtx, OCI_HTYPE_SVCCTX, ValuePtr, nil, OCI_ATTR_ENV, hOCIError);
  if Result <> OCI_SUCCESS then
    Exit;}

/// alloc statement handle
  Result := OCIHandleAlloc(FConnection.hOCIEnv, hOCIStmt, OCI_HTYPE_STMT, 0, nil);
  if Result <> OCI_SUCCESS then
    Exit;

/// prepare
  SQLText :=
    'begin' +
    ' sys.dbms_describe.describe_procedure(:object_name, null, null, :overload,' +
    ' :position, :level, :argument, :datatype, :default, :in_out, :length,' +
    ' :precision, :scale, :radix, :spare);' +
    'end;';
  p := StringToHGlobalOCI(SQLText, Size, FConnection.FUnicodeEnv);
  Result := OCIStmtPrepare(hOCIStmt, hOCIError, p, Size, OCI_NTV_SYNTAX, OCI_DEFAULT);
  FreeStringOCI(p, FConnection.FUnicodeEnv);
  if Result <> OCI_SUCCESS then
    Exit;

/// bind object_name param
  PObjName := Marshal.StringToHGlobalAnsi(AnsiString(objnam));
  try
    Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 1, PObjName, Length(objnam) + 1,
      SQLT_STR, nil, nil, nil, 0, nil, OCI_DEFAULT);
    if Result <> OCI_SUCCESS then
      Exit;

  /// bind overload param
    PArrSize := OrdinalToPtr(arrsiz);
    try
      Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 2, ovrld,
        sizeof(ub2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);

      if Result <> OCI_SUCCESS then
        Exit;
      Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub2), 0, 0, 0);
      if Result <> OCI_SUCCESS then
        Exit;

    /// bind position param
      Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 3, pos,
        sizeof(ub2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
      if Result <> OCI_SUCCESS then
        Exit;
      Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub2), 0, 0, 0);
      if Result <> OCI_SUCCESS then
        Exit;

    /// bind level param
      Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 4, level,
        sizeof(ub2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
      if Result <> OCI_SUCCESS then
        Exit;
      Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub2), 0, 0, 0);
      if Result <> OCI_SUCCESS then
        Exit;

    /// bind argument param
      for i := 0 to Marshal.ReadInt32(PArrSize) - 1 do
        Marshal.WriteInt16(arnlen, sizeof(ub2) * i + 30);
    /// indicator is needed to allow NULL arument name values (i.e Result)
      Indicator := Marshal.AllocHGlobal(2 * Marshal.ReadInt32(PArrSize));
      try
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 5, argnam,
          30, SQLT_CHR, Indicator, arnlen, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, 30, sizeof(sb2), sizeof(ub2), 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind datatype param
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 6, dtype,
          sizeof(ub2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub2), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind default param
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 7, defsup,
          sizeof(ub1), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub1), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind in_out param
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 8, mode,
          sizeof(ub1), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub1), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind length param
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 9, dtsiz,
          sizeof(ub4), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub4), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind precision param
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 10, prec,
          sizeof(sb2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(sb2), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind scale param
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 11, scale,
          sizeof(sb2), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(sb2), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind radix param
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 12, radix,
          sizeof(ub1), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub1), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// bind spare param
        Result := OCIBindByPos(hOCIStmt, hBind, hOCIError, 13, spare,
          sizeof(ub4), SQLT_INT, nil, nil, nil, arrsiz, PArrSize, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;
        Result := OCIBindArrayOfStruct(hBind, hOCIError, sizeof(ub4), 0, 0, 0);
        if Result <> OCI_SUCCESS then
          Exit;

      /// execute statement
        Result := OCIStmtExecute(FConnection.hSvcCtx, hOCIStmt, hOCIError, 1, 0, nil, nil, OCI_DEFAULT);
        if Result <> OCI_SUCCESS then
          Exit;

      /// free statement handle
        Result := OCIHandleFree(hOCIStmt, OCI_HTYPE_STMT);
      finally
        Marshal.FreeHGlobal(Indicator);
      end;
    finally
      PtrToOrdinal(PArrSize, arrsiz);
    end;
  finally
    FreeString(PObjName);
  end;
end;


procedure TOCICommand.InitProcParams7(Name: _string; Overload: integer);
const
  MaxParams = 150;
var
  ovrld: IntPtr;
  pos: IntPtr;
  level: IntPtr;
  argnm: IntPtr;
  arnlen: IntPtr;
  dtype: IntPtr;
  defsup: IntPtr;
  mode: IntPtr;
  dtsize: IntPtr;
  prec: IntPtr;
  scale: IntPtr;
  radix: IntPtr;
  spare: IntPtr;
  arrsize: ub4;
  i: word;
  Param: TOraParamDesc;
  Table: boolean;
  TableLength, OraType: integer;
  Res: sword;
//  Msg: array [0..512] of char;
  ParamName: _string;
  ParamIndex: integer;
  ParamOverload: integer;
begin
  arrsize := MaxParams;

  ovrld := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  pos := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  level := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  argnm := Marshal.AllocHGlobal( (MaxParams + 1) * 30);
  arnlen := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  dtype := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  defsup := Marshal.AllocHGlobal( MaxParams + 1 );
  mode := Marshal.AllocHGlobal( MaxParams + 1 );
  dtsize := Marshal.AllocHGlobal( (MaxParams + 1) * 4);
  prec := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  scale := Marshal.AllocHGlobal( (MaxParams + 1) * 2);
  radix := Marshal.AllocHGlobal( MaxParams + 1 );
  spare := Marshal.AllocHGlobal( (MaxParams + 1) * 4);
  try
    if OCI73 in PossibleOCICallStyles then
      Res := odessp(FConnection.GetLDA, PAnsiChar(AnsiString(Name)), -1, nil, 0, nil, 0, ovrld,
        pos, level, argnm, arnlen, dtype, defsup, mode, dtsize, prec,
        scale, radix, spare, arrsize)
    else
      Res := DescSP(Name, ovrld,
        pos, level, argnm, arnlen, dtype, defsup, mode, dtsize,
        prec, scale, radix, spare, arrsize);

    if Res <> 0 then begin  // for using with OCI8 and UseOCI7ProcDesc = True
      if OCI73 in PossibleOCICallStyles then
        FConnection.OraError(OCI73, Res, True, Component)
      else
        FConnection.OraError(OCI80, Res, True, Component);
  //      oerhms(FConnection.GetLDA, Res, Msg, 512);
  //      raise EOraError.Create(Abs(Res), Msg);
    end;

    Assert(arrsize < MaxParams);

    FParams.Clear;

    Param := nil;
    Table := False;
    TableLength := 0;

    // Overload 0 and 1 is equal
    if Overload = 0 then
      Overload := 1;

    i := 0;
    while i < arrsize do begin
      ParamOverload := Marshal.ReadInt16(ovrld, i * 2);
      // Overload 0 and 1 is equal
      if ParamOverload = 0 then
        ParamOverload := 1;
      if (Marshal.ReadInt16(dtype, i * 2) <> UNKNOWN_TYPE) and (ParamOverload = Overload) then begin
        if Marshal.ReadInt16(level, i * 2) = 0 then begin
          Param := TOraParamDesc(AddParam);

          if Marshal.ReadInt16(pos, i * 2) = 0 then begin
            Param.Name := 'RESULT';

            Param.ParamType := pdResult;
          end
          else begin
            ParamName := _string(Marshal.PtrToStringAnsi(PtrOffset(argnm, i * 30), Marshal.ReadInt16(arnlen, i * 2)));
            ParamName := StringReplace(ParamName, ' ' , '_', [rfReplaceAll]);
            if FParams.FindParam(ParamName) <> nil then begin
              ParamIndex := 1;
              while FParams.FindParam(ParamName + '_' + IntToStr(ParamIndex)) <> nil do
                Inc(ParamIndex);
              ParamName := ParamName + '_' + IntToStr(ParamIndex);
            end;
            Param.Name := ParamName;

            case Marshal.ReadByte(mode, i) of
              0: Param.ParamType := pdInput;
              1: Param.ParamType := pdOutput;
              2: Param.ParamType := pdInputOutput;
            end;

            Param.SetHasDefault(Boolean(Marshal.ReadByte(defsup, i)));
          end;
        end;

        Assert(Param <> nil);

        if (Marshal.ReadInt16(level, i * 2) = 0) or Table then begin
          OraType := Marshal.ReadInt16(dtype, i * 2);
          case OraType of
            VARCHAR2_TYPE: begin
            {$IFNDEF LITE}
              if FConnection.FUseUnicode then
                Param.DataType := dtWideString
              else
            {$ENDIF}
                Param.DataType := dtString;
              if Marshal.ReadInt32(dtsize, i * 4) > 0 then
                Param.Size := Marshal.ReadInt32(dtsize, i * 4);
            end;
            CHAR_TYPE: begin
            {$IFNDEF LITE}
              if FConnection.FUseUnicode then
                Param.DataType := dtFixedWideChar
              else
            {$ENDIF}
                Param.DataType := dtFixedChar;
              if Marshal.ReadInt32(dtsize, i * 4) > 0 then
                Param.Size := Marshal.ReadInt32(dtsize, i * 4);
            end;
            NUMBER_TYPE:
              if not FConnection.FEnableIntegers or (Abs(Marshal.ReadInt16(scale, i * 2)) > 0)
                or (Marshal.ReadInt16(prec, i * 2) > GetIntegerPrecision) or (Marshal.ReadInt16(prec, i * 2) = 0)
              then begin
                Param.DataType := dtFloat;
              end
              else
                Param.DataType := dtInteger;
            INTEGER_TYPE:
              Param.DataType := dtInteger;
            DATE_TYPE:
              Param.DataType := dtDateTime;
            ROWID_TYPE: begin
              Param.DataType := dtString;
              Param.Size := RowIdSize + 1;
            end;
            RAW_TYPE: begin
              Param.DataType := dtVarBytes;
              if Marshal.ReadInt32(dtsize, i * 4) > 0 then
                Param.Size := Marshal.ReadInt32(dtsize, i * 4);
            end;
            LONG_TYPE:
            {$IFNDEF LITE}
              if FConnection.FUseUnicode then
                Param.DataType := dtWideMemo
              else
            {$ENDIF}
                Param.DataType := dtMemo;
            LONGRAW_TYPE:
              Param.DataType := dtBlob;
            CURSOR_TYPE: begin
              Param.DataType := dtCursor;
              //Param.FParamType := pdOutput
            end;
            SQLT_TAB: begin
              Table := True;
              TableLength := 1;
            end;
            SQLT_BOL:
              Param.DataType := dtBoolean;
            SQLT_REC: begin
              //Rec := True;
            end;
          // Oracle8 types
            SQLT_CLOB:
              Param.DataType := dtOraClob;
            SQLT_BLOB:
              Param.DataType := dtOraBlob;
            SQLT_BFILEE:
              Param.DataType := dtBFILE;
            SQLT_CFILEE:
              Param.DataType := dtCFILE;
            SQLT_RSET:
              Param.DataType := dtCursor;
            SQLT_NTY:
              Param.DataType := dtObject;//TOraType(Field.ObjectType).DataType;
            SQLT_REF:
              Param.DataType := dtReference;
            SQLT_NCO:
              Param.DataType := dtTable;
            SQLT_VARRAY:
              Param.DataType := dtArray;
          // Oracle9 types
            SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ: begin
            {$IFDEF VER6P}
            {$IFNDEF FPC}
              if FConnection.FEnableSQLTimeStamp then
                Param.DataType := dtSQLTimeStamp
            {$ELSE}
              if FConnection.FTimeStampAsString then
                Param.DataType := dtDateTime
            {$ENDIF}
              else
            {$ENDIF}
                case OraType of
                  SQLT_TIMESTAMP:
                    Param.DataType := dtTimeStamp;
                  SQLT_TIMESTAMP_TZ:
                    Param.DataType := dtTimeStampTZ;
                  SQLT_TIMESTAMP_LTZ:
                    Param.DataType := dtTimeStampLTZ;
                end;
            end;
            SQLT_INTERVAL_YM:
              Param.DataType := dtIntervalYM;
            SQLT_INTERVAL_DS:
              Param.DataType := dtIntervalDS;
          else
            RaiseError(SUnsupportedDataType + ' [' + IntToStr(Marshal.ReadInt16(dtype, i * 2)) + ']')
          end;
        end;

        if Marshal.ReadInt16(level, i * 2) > 0 then
          if Table then begin
          // for PL/SQL table
            Param.FTable := Table;
            Param.FLength := TableLength;
            Table := False;
            TableLength := 0;
          end;
        if (Param.ValuePtr = nil) and not Table then
          Param.AllocBuffer;
      end;
      Inc(i);
    end;
  finally
    Marshal.FreeHGlobal(argnm);
    Marshal.FreeHGlobal(ovrld);
    Marshal.FreeHGlobal(pos);
    Marshal.FreeHGlobal(level);
    Marshal.FreeHGlobal(arnlen);
    Marshal.FreeHGlobal(dtype);
    Marshal.FreeHGlobal(defsup);
    Marshal.FreeHGlobal(mode);
    Marshal.FreeHGlobal(dtsize);
    Marshal.FreeHGlobal(prec);
    Marshal.FreeHGlobal(scale);
    Marshal.FreeHGlobal(radix);
    Marshal.FreeHGlobal(spare);
  end;
end;

procedure TOCICommand.InitProcParams8(Name: _string; Overload: integer);

  function DescribeSynonym(const SynonymName: _string): _string;
  var
    Handle: IntPtr;
    hDescribe: pOCIDescribe;
    b1: Integer; //byte
    hParam: pOCIParam;
    Len: Integer;
    Ptr, ValuePtr, StrPtr: IntPtr;
    ValueInt: Integer;
    ObjType: byte;
    Res, Size: integer;
  begin
    Result := SynonymName;
    Check(OCIHandleAlloc(hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
    try
      b1 := 1;
      Check(OCIAttrSet2(hDescribe, OCI_HTYPE_DESCRIBE, b1, 0, OCI_ATTR_DESC_PUBLIC, hOCIError));
      Ptr := Marshal.AllocHGlobal(sizeof(integer));
      try
        while True do begin
          Handle := StringToHGlobalOCI(Result, Size, FConnection.FUnicodeEnv);
          Res := OCIDescribeAny(FConnection.GetSvcCtx, hOCIError, Handle, Size,
            OCI_OTYPE_NAME, OCI_DEFAULT, OCI_PTYPE_UNK, hDescribe);
          FreeStringOCI(Handle, FConnection.FUnicodeEnv);
          Check(Res);

          ValuePtr := OrdinalToPtr(hParam);
          try
            Check(OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil,
              OCI_ATTR_PARAM, hOCIError));
          finally
            PtrToOrdinal(ValuePtr, hParam);
          end;

          Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil,
            OCI_ATTR_PTYPE, hOCIError));
          ObjType := Byte(ValueInt);

          case ObjType of
            OCI_PTYPE_TYPE:
              Break;
            OCI_PTYPE_SYN: begin
              StrPtr := nil;
              ValuePtr := OrdinalToPtr(StrPtr);
              try
                Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_NAME, hOCIError));
              finally
                PtrToOrdinal(ValuePtr, StrPtr);
              end;
              Len := Cardinal(Marshal.ReadInt32(Ptr));
              Result := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv));

              StrPtr := nil;
              ValuePtr := OrdinalToPtr(StrPtr);
              try
                Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_SCHEMA_NAME, hOCIError));
              finally
                PtrToOrdinal(ValuePtr, StrPtr);
              end;
              Len := Cardinal(Marshal.ReadInt32(Ptr));
              if Len > 0 then
                Result := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv)) + '.' + Result;
            end
          else
            RaiseOraError(4043, _Format(SObjectNotExist, [Name]));
          end;
        end;
      finally
        Marshal.FreeHGlobal(Ptr);
      end;
    finally
      Check(OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE));
    end;
  end;

var
  Res: sword;
  i, BufSize: integer;
  Param: TOraParamDesc;
  hDescribe: pOCIDescribe;
  hParam: pOCIParam;
  hArgList: pOCIParam;
  hArgList1: pOCIParam; // arguments of level 1
  hArg: pOCIParam;
  hProcList: pOCIParam;
  ArgCount: word;
  pName: _string;
  Len: cardinal;
  OraType: word;
  Size: word;
  Scale: shortint;
  Prec: word; // ??? byte
  Mode: byte;
  ProcCount: word;
  ListType: byte;
  lOverload: word;
  d1,d2: integer;
  St,St1: _string;
  ObjType: byte;
  b1: Integer; //byte
  iOffset: integer;
  Handle: IntPtr;
  ValuePtr, StrPtr: IntPtr;
  ValueInt: Integer;
  PLen: IntPtr;
  TypeName, SchemaName: _string;
  ObjectType: TOraType;
  OraObject: TOraObject;
  IsEqual: boolean;
  ParamName: _string;
  ParamIndex: Integer;
begin
  Check(OCIHandleAlloc(FConnection.hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
  Check(OCIAttrSet2(hDescribe, OCI_HTYPE_DESCRIBE, b1, 0, OCI_ATTR_DESC_PUBLIC, hOCIError));
  try
    if Overload = 1 then
      Overload := 0;
    lOverload := 0;

    PLen  := Marshal.AllocHGlobal(sizeof(Integer));
    try
      while True do begin
        d1 := Pos('.', Name);
        if d1 > 0 then begin
          d2 := Pos('.', Copy(Name, d1 + 1, Length(Name)));
          if d2 > 0 then
            inc(d2, d1);
        end
        else
          d2 :=0;

      { Get the describe handle }
        if d2 > 0 then
          St := Copy(Name, 1, d2 - 1)
        else
          St := Name;

        Handle := StringToHGlobalOCI(St, BufSize, FConnection.FUnicodeEnv);
        Res := OCIDescribeAny(FConnection.GetSvcCtx, hOCIError, Handle, BufSize,
          OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, hDescribe);
        FreeStringOCI(Handle, FConnection.FUnicodeEnv);

        if (Res <> 0) and (d1 > 0) and (d2 = 0) then begin
        // for <package>.<proc>
          St := Copy(Name, 1, d1 - 1);
          Handle := StringToHGlobalOCI(St, BufSize, FConnection.FUnicodeEnv);
          Res := OCIDescribeAny(FConnection.GetSvcCtx, hOCIError, Handle, BufSize,
            OCI_OTYPE_NAME, 0, OCI_PTYPE_UNK, hDescribe);
          FreeStringOCI(Handle, FConnection.FUnicodeEnv);
          Check(Res);
        end
        else
          Check(Res);

        if d2 = 0 then
          d2 := d1;

        ValuePtr := OrdinalToPtr(hParam);
        try  
          Check(OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil, OCI_ATTR_PARAM, hOCIError));
        finally
          PtrToOrdinal(ValuePtr, hParam);
        end;

        Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil,
          OCI_ATTR_PTYPE, hOCIError));
        ObjType := Byte(ValueInt);

        case ObjType of
          OCI_PTYPE_PROC, OCI_PTYPE_FUNC: begin

          end;
          OCI_PTYPE_PKG, OCI_PTYPE_TYPE : begin
          // Describe package proc
          // Describe object methods

          { Get the parameter handle }
            ValuePtr := OrdinalToPtr(hParam);
            try
              Check(OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil,
                OCI_ATTR_PARAM, hOCIError));
            finally
              PtrToOrdinal(hParam, hParam);
            end;

            if ObjType = OCI_PTYPE_PKG then begin
            { Get the subprogram list }
              ValuePtr := OrdinalToPtr(hProcList);
              try
                Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil,
                  OCI_ATTR_LIST_SUBPROGRAMS, hOCIError));
              finally
                PtrToOrdinal(ValuePtr, hProcList);
              end;

              iOffset := 0;
            end
            else begin
            { Get the method list }
              ValuePtr := OrdinalToPtr(hProcList);
              try
                Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil,
                  OCI_ATTR_LIST_TYPE_METHODS, hOCIError));
              finally
                PtrToOrdinal(ValuePtr, hProcList);
              end;

              iOffset := 1;
            end;

          { Get the number of procs }
            Check(OCIAttrGet2(hProcList, OCI_DTYPE_PARAM, ValueInt, nil,
              OCI_ATTR_NUM_PARAMS, hOCIError));
            ProcCount := Word(ValueInt);

            St := Copy(Name, d2 + 1, Length(Name));
            i := iOffset;
            lOverload := 0;
            while i < ProcCount + iOffset do begin
            { Get the parameter handle }
              Check(OCIParamGet(hProcList, OCI_DTYPE_PARAM, hOCIError, hParam, i));  // hProc

              try
                StrPtr := nil;
                ValuePtr := OrdinalToPtr(StrPtr);
                try
                  Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, PLen, OCI_ATTR_NAME, hOCIError));
                finally
                  PtrToOrdinal(ValuePtr, StrPtr);
                end;
                Len := Cardinal(Marshal.ReadInt32(PLen));
                St1 := PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv);

                if OCISQLInfo.IsQuoted(St) then
                  IsEqual := OCISQLInfo.UnQuote(St) = St1
                else
                  IsEqual := _UpperCase(St) = St1;

                if IsEqual then begin
                  if Overload > 0 then
                    Inc(lOverload);
                  if lOverload = Overload then
                    break;
                end;

              finally
                // free memory after OCIParamGet
                OCIDescriptorFree(hParam, OCI_DTYPE_PARAM);
              end;

              Inc(i);
            end;

            if i = ProcCount + iOffset then
              RaiseOraError(4043, _Format(SObjectNotExist, [Name]));
          end;
          OCI_PTYPE_SYN: begin
          // Describe synonyms
            if d2 > 0 then
              Name := '.' + Copy(Name, d2 + 1, Length(Name))
            else
              Name := '';

            StrPtr := nil;
            ValuePtr := OrdinalToPtr(StrPtr);
            try
              Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, PLen,
                OCI_ATTR_NAME, hOCIError));
            finally
              PtrToOrdinal(ValuePtr, StrPtr);
            end;
            Len := Cardinal(Marshal.ReadInt32(PLen));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv));
            Name := pName + Name;

            StrPtr := nil;
            ValuePtr := OrdinalToPtr(StrPtr);
            try
              Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, PLen,
                OCI_ATTR_SCHEMA_NAME, hOCIError));
            finally
              PtrToOrdinal(ValuePtr, StrPtr);
            end;
            Len := Cardinal(Marshal.ReadInt32(PLen));
            pName := OCISQLInfo.QuoteIfNeed(PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv));
            Name := pName + '.' + Name;

            Continue;
          end;
          //OCI_PTYPE_SCHEMA:
        else
          RaiseOraError(4043, _Format(SObjectNotExist, [St]));
        end;

      { Get the arg list }
        ValuePtr := OrdinalToPtr(hArgList);
        try
          Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil,
            OCI_ATTR_LIST_ARGUMENTS, hOCIError));
        finally
          PtrToOrdinal(ValuePtr, hArgList);
        end;

      { Get the number of arguments }
        Check(OCIAttrGet2(hArgList, OCI_DTYPE_PARAM, ValueInt, nil,
          OCI_ATTR_NUM_PARAMS, hOCIError));
        ArgCount := Word(ValueInt);

        ListType := 0;
        if (OCIVersion < 8100) then begin
          if (ObjType = OCI_PTYPE_FUNC)
          /// Test param at zero position for package subprogram. It won't return
          /// error in case of function.
            or ((ObjType = OCI_PTYPE_PKG)
            and (OCIParamGet(hArgList, OCI_DTYPE_PARAM, hOCIError, hArg, 0) = 0))
          then
            ListType := OCI_LTYPE_ARG_FUNC
        end
        else begin
        /// Get the type of subproram for 8.1.5 and above. In 8.0.x client attribute
        /// OCI_ATTR_LTYPE is not supported.
          Check(OCIAttrGet2(hArgList, OCI_DTYPE_PARAM, ValueInt, nil,
            OCI_ATTR_LTYPE, hOCIError));
          ListType := Byte(ValueInt);
        end;

        FParams.Clear;

        Param := nil;
        hArgList1 := nil;

        if ListType in [OCI_LTYPE_ARG_FUNC, OCI_LTYPE_TYPE_ARG_FUNC] then begin // IsFunc
          i := 0;
          if ListType in [OCI_LTYPE_ARG_FUNC] then
            Dec(ArgCount);
        end
        else
          // OCI_LTYPE_ARG_PROC, OCI_LTYPE_TYPE_ARG_PROC
          i := 1;

        while i <= ArgCount do begin
          if hArgList1 = nil then
            Check(OCIParamGet(hArgList, OCI_DTYPE_PARAM, hOCIError, hArg, i))
          else
            Check(OCIParamGet(hArgList1, OCI_DTYPE_PARAM, hOCIError, hArg, 1));

          try
            Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil,
              OCI_ATTR_PTYPE, hOCIError));
            ObjType := Byte(ValueInt);

            //Check(OCIAttrGet(hArg, OCI_DTYPE_PARAM, @Level, nil, OCI_ATTR_LEVEL, hOCIError));
            Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_TYPE, hOCIError));
            OraType := Word(ValueInt);

            if (OraType <> SQLT_UNK) and (lOverload = Overload) then begin
              if hArgList1 = nil then begin
                if i > 0 then begin
                  StrPtr := nil;
                  ValuePtr := OrdinalToPtr(StrPtr);
                  try
                    Check(OCIAttrGet1(hArg, OCI_DTYPE_PARAM, ValuePtr, PLen, OCI_ATTR_NAME, hOCIError));
                  finally
                    PtrToOrdinal(ValuePtr, StrPtr);
                  end;
                  Len := Cardinal(Marshal.ReadInt32(PLen));
                  ParamName := PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv);
                  ParamName := StringReplace(ParamName, ' ' , '_', [rfReplaceAll]);
                  if FParams.FindParam(ParamName) <> nil then begin
                    ParamIndex := 1;
                    while FParams.FindParam(ParamName + '_' + IntToStr(ParamIndex)) <> nil do
                      Inc(ParamIndex);
                    ParamName := ParamName + '_' + IntToStr(ParamIndex);
                  end;
                end;
                Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_IOMODE, hOCIError));
                Mode := Byte(ValueInt);

                Param := TOraParamDesc(AddParam);
                if i > 0 then begin
                  Param.Name := ParamName;

                  case Mode of
                    0: Param.ParamType := pdInput;
                    1: Param.ParamType := pdOutput;
                    2: Param.ParamType := pdInputOutput;
                  end;

                  Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_HAS_DEFAULT, hOCIError));
                  Param.SetHasDefault(Boolean(ValueInt));
                end
                else begin
                  Param.Name := 'RESULT';
                  Param.ParamType := pdResult;
                end;
              end;

              Assert(Param <> nil);

              Size := 0;
              Scale := 0;
              Prec := 0;
              if //(OraType <> SQLT_NTY) and
                 (ObjType = OCI_PTYPE_ARG) then begin  // for Self param of methods
                Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_SIZE, hOCIError));
                Size := Word(ValueInt);
                Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_SCALE, hOCIError));
                Scale := sb1(ValueInt);
                Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PRECISION, hOCIError));
                Prec := Word(ValueInt);
              end;
              // Use default value OCI_ATTR_HAS_DEFAULT

              case OraType of
                SQLT_CHR: begin
                  if FConnection.FUseUnicode then
                    Param.DataType := dtWideString
                  else
                    Param.DataType := dtString;
                  if Size > 0 then
                    Param.Size := Size;
                  Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, hOCIError));
                  Param.SetNational(ub1(ValueInt) = SQLCS_NCHAR);
                end;
                SQLT_AFC: begin
                  if FConnection.FUseUnicode then
                    Param.DataType := dtFixedWideChar
                  else
                    Param.DataType := dtFixedChar;
                  if Size > 0 then
                    Param.Size := Size;
                  Check(OCIAttrGet2(hArg, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_CHARSET_FORM, hOCIError));
                  Param.SetNational(ub1(ValueInt) = SQLCS_NCHAR);
                end;
                SQLT_NUM: begin
                  if Prec = 0 then begin
                    Prec := 38;
                    if Scale = 0 then // NUMBER
                      Scale := 127;
                  end;
                  if FConnection.FEnableIntegers and (Prec <= GetSmallintPrecision) and (Scale = 0)
                  then
                    Param.DataType := dtSmallint
                  else
                  if FConnection.FEnableIntegers and (Prec <= GetIntegerPrecision) and (Scale = 0)
                  then
                    Param.DataType := dtInteger
                  else
                  if FConnection.FEnableIntegers and (Prec <= GetLargeintPrecision) and (Scale = 0)
                  then
                    Param.DataType := dtLargeint
                  else
                  if Prec <= GetFloatPrecision then
                    Param.DataType := dtFloat
                  else
                  if (FConnection.EnableBCD or FEnableBCD) and (Prec <= GetBCDPrecision) and
                    (Scale >= 0) and (Scale <= GetBCDScale)
                  then
                    Param.DataType := dtBCD
                {$IFDEF VER6P}
                {$IFNDEF FPC}
                  else
                  if (FConnection.EnableFMTBCD or FEnableFMTBCD) and (Prec <= GetFmtBCDPrecision) and
                   (Scale >= 0) and (Scale <= GetFmtBCDScale)
                  then
                    Param.DataType := dtFMTBCD
                {$ENDIF}
                {$ENDIF}
                  else
                  if FConnection.FEnableNumbers then
                    Param.DataType := dtNumber
                  else
                    Param.DataType := dtFloat;
                end;
                INTEGER_TYPE:
                  Param.DataType := dtInteger;
                SQLT_IBFLOAT: begin
                  Param.DataType := dtFloat;
                  Param.SubDataType := dtBFloat;
                end;
                SQLT_IBDOUBLE: begin
                  Param.DataType := dtFloat;
                  Param.SubDataType := dtBDouble;
                end;
                SQLT_DAT:
                  Param.DataType := dtDateTime;
                SQLT_RID,SQLT_RDD : begin
                {$IFNDEF LITE}
                  if FConnection.FUseUnicode then
                    Param.DataType := dtWideString // dtRowId;
                  else
                {$ENDIF}
                    Param.DataType := dtString; // dtRowId;
                  Param.Size := RowIdSize + 1; // for terminator
                end;
                SQLT_BIN: begin
                  Param.DataType := dtVarBytes;
                  if Size > 0 then
                    Param.Size := Size;
                end;
                SQLT_LNG:
                {$IFNDEF LITE}
                  if FConnection.FUseUnicode then
                    Param.DataType := dtWideMemo
                  else
                {$ENDIF}
                    Param.DataType := dtMemo;
                SQLT_LBI:
                  Param.DataType := dtBlob;
                SQLT_CUR: begin  // SQLT_RSET
                  Param.DataType := dtCursor;
                  //Param.ParamType := pdOutput;
                end;
                SQLT_TAB: begin
                  ValuePtr := OrdinalToPtr(hArgList1);
                  try
                    Check(OCIAttrGet1(hArg, OCI_DTYPE_PARAM, ValuePtr, nil, OCI_ATTR_LIST_ARGUMENTS, hOCIError));
                  finally
                    PtrToOrdinal(ValuePtr, hArgList1);
                  end;
                  Param.FTable := True;
                  Param.FLength := 1;
                  Continue;
                end;
                SQLT_BOL:
                  Param.DataType := dtBoolean;
                SQLT_REC: begin
                  //Rec:= True;
                end;
              // Oracle8 types
                SQLT_CLOB:
                  Param.DataType := dtOraClob;
                SQLT_BLOB:
                  Param.DataType := dtOraBlob;
                SQLT_BFILEE:
                  Param.DataType := dtBFILE;
                SQLT_CFILEE:
                  Param.DataType := dtCFILE;
                SQLT_NTY: begin
                  StrPtr := nil;
                  ValuePtr := OrdinalToPtr(StrPtr);
                  try
                    Check(OCIAttrGet1(hArg, OCI_DTYPE_PARAM, ValuePtr, PLen, OCI_ATTR_TYPE_NAME, hOCIError));
                  finally
                    PtrToOrdinal(ValuePtr, StrPtr);
                  end;
                  Len := Marshal.ReadInt32(PLen);
                  TypeName := PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv);

                  StrPtr := nil;
                  ValuePtr := OrdinalToPtr(StrPtr);
                  try
                    Check(OCIAttrGet1(hArg, OCI_DTYPE_PARAM, ValuePtr, PLen, OCI_ATTR_SCHEMA_NAME, hOCIError));
                  finally
                    PtrToOrdinal(ValuePtr, StrPtr);
                  end;
                  Len := Marshal.ReadInt32(PLen);
                  SchemaName := PtrToStringOCI(StrPtr, Len, FConnection.FUnicodeEnv);

                  if SchemaName <> 'PUBLIC' then // Oracle 10
                    TypeName := SchemaName + '.' + TypeName;

                  TypeName := DescribeSynonym(TypeName);
                  ObjectType := nil;
                  if ObjectTypes <> nil then
                    ObjectType := TOraType(ObjectTypes.FindType(FConnection.GetSvcCtx, TypeName));
                  if ObjectType = nil then
                    ObjectType := TOraType.Create(FConnection, TypeName)
                  else
                    ObjectType.AddRef;

                  try
                    if ObjectType.DataType = dtXml then begin
                      Param.DataType := dtXml;
                      OraObject := TOraXML.Create(ObjectType);
                      try
                        Param.SetObject(OraObject); // increments ref count to 2
                      finally
                        OraObject.Free;             // decrements ref count to 1
                      end;
                    end
                    else begin
                      Param.DataType := dtObject;//TOraType(Field.ObjectType).DataType;
                      OraObject := TOraObject.Create(ObjectType);
                      try
                        Param.SetObject(OraObject); // increments ref count to 2
                      finally
                        OraObject.Free;             // decrements ref count to 1
                      end;
                    end;
                  finally
                    ObjectType.Release;
                  end;
                end;
                SQLT_REF:
                  Param.DataType := dtReference;
                SQLT_NCO:
                  Param.DataType := dtTable;
                SQLT_VARRAY:
                  Param.DataType := dtArray;
              // Oracle9 types
                SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ: begin
                {$IFDEF VER6P}
                {$IFNDEF FPC}
                  if FConnection.FEnableSQLTimeStamp then
                    Param.DataType := dtSQLTimeStamp
                {$ELSE}
                  if FConnection.FTimeStampAsString then
                    Param.DataType := dtDateTime
                {$ENDIF}
                  else
                {$ENDIF}
                    case OraType of
                      SQLT_TIMESTAMP:
                        Param.DataType := dtTimeStamp;
                      SQLT_TIMESTAMP_TZ:
                        Param.DataType := dtTimeStampTZ;
                      SQLT_TIMESTAMP_LTZ:
                        Param.DataType := dtTimeStampLTZ;
                    end;
                end;
                SQLT_INTERVAL_YM:
                  Param.DataType := dtIntervalYM;
                SQLT_INTERVAL_DS:
                  Param.DataType := dtIntervalDS;
              else
                RaiseError(SUnsupportedDataType + ' [' + IntToStr(OraType) + ']')
              end;
              if Param.ValuePtr = nil then
                Param.AllocBuffer;
            end;

            hArgList1 := nil;
            Inc(i);

          finally
            // free memory after OCIParamGet
            OCIDescriptorFree(hArg, OCI_DTYPE_PARAM);
          end;

        end;

        break;
      end;
    finally
      Marshal.FreeHGlobal(PLen);
    end;

  finally
    Check(OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE));
  end;
end;

procedure TOCICommand.InitProcParams(Name: _string; Overload: integer);
begin
  CheckSession;
  FOCICallStyle := FConnection.FOCICallStyleCommand;
  hOCIError := FConnection.hOCIError;

  if FOCICallStyle = OCI73 then begin
    InitProcParams7(Name, Overload);
  end
  else
  if FOCICallStyle = OCI80 then begin
    if UseOCI7ProcDesc or
      not (OCI73 in PossibleOCICallStyles)
    /// Oracle Server 8.0.5 has a bug with OCI80 call style after recompiling.
      or (FConnection.GetOracleVersion = 8050)
    {/// Oracle 8.0.5 has a bug with OCI80 call style??? - Not detected.
    /// Oracle 8.0.4 has bug with OCI73 call style (cannot describe more than 10
    /// parameters).

      or (OCIVersionSt < '8.1')
      and (StrLComp(PChar(OCIVersionSt), '8.0.4', 5) <> 0)}
    then
      InitProcParams7(Name, Overload)
    else
      InitProcParams8(Name, Overload);
  end
  else
    CheckOCI;
end;

procedure TOCICommand.DoExecute;
var
  OldPrepared: boolean;
begin
  OldPrepared := GetPrepared;
  if not OldPrepared then
    Prepare;
  try
    if FSQLType <> SQL_EXPLAIN then  // avoid binding with EXPLAIN PLAN
      BindParams;
    Exec;
  finally
    if (not OldPrepared or FForceUnprepare) and (GetCursorState <> csInactive) then
      Finish;
    FForceUnprepare := False;

    FIterCount := 1;
  end;
end;

procedure TOCICommand.Execute(Iters: integer);
var
  E: Exception;
begin
  if GetCursorState = csExecuting then
    Exit;

  FIterCount := Iters;

  FExecuting := True;
  if FNonBlocking then begin
    if not FConnection.FThreadSafety then begin
      E := Exception.Create(SNeedThreadSafety);
      EndExecute(E);
      raise E;
    end;

  {$IFDEF MSWINDOWS}
    Assert(hExecThread = nil);
    hExecThread := FConnection.RunThread(DoExecute, EndExecute);
  {$ENDIF}
  end
  else begin
    try
      DoExecute;
    except
      on E: Exception do begin
        EndExecute(E);
        raise;
      end;
    end;
    EndExecute(nil);
  end;
end;

procedure TOCICommand.BreakExec;
begin
  if GetCursorState <> csInactive then
    FConnection.BreakExec;
end;

procedure TOCICommand.HardBreak;
{$IFDEF MSWINDOWS}
var
  E: Exception;
  Res: DWORD;
{$IFDEF CLR}
  ReturnValue: LongWord;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if GetCursorState <> csInactive {= csExecuting} then begin
    FConnection.BreakExec;

  {$IFDEF CLR}
    Res := 0;
    if hExecThread.WaitFor(3000, ReturnValue) then
      Res := WAIT_TIMEOUT;
  {$ELSE}
    Res := WaitForSingleObject(hExecThread.Handle, 3000);
  {$ENDIF}

    case Res of  // timeout 3 sec
      WAIT_TIMEOUT: begin
        if hExecThread <> nil then
          FConnection.StopThread(hExecThread);
        E := Exception.Create('HardBreak');
        try
          if GetCursorState = csExecuting then
            Finish;
        finally
          EndExecute(E);
          E.Free;
        end;
      end;
    end;
  end;
{$ENDIF}
end;

procedure TOCICommand.EndExecute(E: Exception);
var
  Fail: boolean;
begin
{$IFDEF MSWINDOWS}
  if hExecThread <> nil then begin
//    FConnection.StopThread(hExecThread);
    hExecThread := nil;
  end
  else
    if FNonBlocking then Exit;
{$ENDIF}

  FExecuting := False;  // Here for reexecute in AfterExecute

  if Assigned(FAfterExecute) then
    FAfterExecute(E = nil);

  if FNonBlocking and (E is EOraError) then begin
    Fail := True;
    FConnection.DoError(EOraError(E), Fail);
    if not Fail then
      Abort;
  end;
end;

{ Params }

function TOCICommand.AddParam: TParamDesc;
var
  Param: TOraParamDesc;
begin
  Param := TOraParamDesc.Create;
  Param.FOwner := Self;
  Result := Param;
  FParams.Add(Result);
end;

class function TOCICommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TOCISQLInfo;
end;

class function TOCICommand.GetParserClass: TSQLParserClass;
begin
  Result := TOraParser;
end;

function TOCICommand.ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string;
var
  Param: TOraParamDesc;
  Parser: TOraParser;
  Code: integer;
  St: _string;
  AllParams: _TStringList;
begin
  if RenamePrefix <> '' then
    Result := inherited ParseSQL(SQL, Params, ReplaceAll, RenamePrefix)
  else
    Result := SQL;

  if not FScanParams or (Params = nil) then
    exit;

  Params.Clear;
  Parser := TOraParser.Create(SQL);
  Parser.DecSeparator := '.';
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  AllParams := _TStringList.Create;
  AllParams.Sorted := True;
  try
    Parser.ToBegin;
    repeat
      Code := Parser.GetNext(St);
      if (Code = lxCREATE) or (Code = lxTRIGGER) then
        Code := lcEnd;
      while (Code <> lcEnd) and (Code <> 3) do begin // ':'
        Code := Parser.GetNext(St);
        if Code = lxTRIGGER then
          Code := lcEnd;
      end;
      if Code = 3 then begin 
        Code := Parser.GetNext(St);
        if (Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst) then begin // PL/SQL reserved words is allowed
          if AllParams.IndexOf(St) < 0 then begin
            Param := TOraParamDesc.Create;
            Param.FOwner := Self;
            Param.SetName(St);
            Params.Add(Param);
            AllParams.Add(St)
          end;
        end;
      end;
    until Code = lcEnd;
  finally
    AllParams.Free;
    Parser.Free;
  end;
end;

procedure TOCICommand.BindParams;
var
  i: integer;
begin
  FConnection.BusyWait;
  try
    for i := 0 to FParams.Count - 1 do
      BindParam(TOraParamDesc(FParams[i]));
  finally
    FConnection.Release;
  end;

  SetCursorState(csBound);
end;

{procedure TOCICommand.DisconnectParams;
var
  Param: TOraParamDesc;
  i: integer;
begin
  for i := 0 to FParams.Count - 1 do begin
    Param := TOraParamDesc(FParams[i]);
    case Param.FDataType of
      dtOraBlob,dtOraClob:
        Param.GetAsOraBlob.FreeLob;
      dtObject,dtArray:
        TOraObject(Param.GetObject).FreeObject;
    end;
  end;
end;}

function TOCICommand.GetParam(Index: integer): TOraParamDesc;
begin
  Result := TOraParamDesc(FParams[Index]);
end;

{ Strored Proc }

function TOCICommand.CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string;
const
  STab = '  ';
var
  SPName: _string;
  SPOverload: integer;
  PassByName: boolean;
  St, DeclSt, OutSt: _string;
  i: integer;
  Added: boolean;
  ResParamDesc, ParamDesc: TOraParamDesc;

  function PrepareVariable(AParamDesc: TOraParamDesc): _string;
  begin
    case AParamDesc.DataType of
      dtBoolean: begin
        DeclSt := DeclSt + STab + 'v_' + AParamDesc.Name + ' boolean';
        if AParamDesc.ParamType in [pdUnknown, pdInput, pdInputOutput] then begin
          DeclSt := DeclSt + ' := sys.DIUTIL.INT_TO_BOOL(:' + AParamDesc.GetName + ');' + LineSeparator;
        end else
          DeclSt := DeclSt + ';' + LineSeparator;
        if AParamDesc.ParamType in [pdUnknown, pdOutput, pdInputOutput, pdResult] then begin
          OutSt := OutSt + STab + ':' + AParamDesc.Name + ' := sys.DIUTIL.BOOL_TO_INT(v_' + AParamDesc.GetName + ');' + LineSeparator;
        end;
        Result := 'v_' + AParamDesc.Name;
      end;
      else
        Result := ':' + AParamDesc.Name;
    end;
    if (AParamDesc.ParamType <> pdResult) and PassByName then
      Result := AParamDesc.Name + ' => ' + Result;
  end;

  function FindResultParam: TOraParamDesc;
  var
    i: integer;
  begin
    Result := nil;

    for i := 0 to FParams.Count - 1 do
      if (FParams[i] <> nil) then
        if (TOraParamDesc(FParams[i]).ParamType = pdResult) or TOraParamDesc(FParams[i]).FIsResult
        then begin
          Result := TOraParamDesc(FParams[i]);
          break;
        end;
  end;

begin
  TOCISQLInfo.ParseSPName(Name, SPName, SPOverload);

  if NeedDescribe then
    InitProcParams(SPName, SPOverload);

  ResParamDesc := FindResultParam;

  PassByName := False;
  Added := False;
  for i := FParams.Count - 1 downto 0 do begin
    ParamDesc := TOraParamDesc(FParams[i]);
    if not NeedBindParam(ParamDesc) then begin
      if Added then begin
        PassByName := True;
        break;
      end;
    end
    else
      if ParamDesc <> ResParamDesc then
        Added := True;
  end;

  St := 'begin' + LineSeparator;
  St := St + STab;

  DeclSt := '';
  OutSt := '';

  if ResParamDesc <> nil then begin
    St := St + PrepareVariable(ResParamDesc) + ' := ';
  end;
  St := St + SPName;

  Added := False;
  for i := 0 to FParams.Count - 1 do begin
    ParamDesc := TOraParamDesc(FParams[i]);
    if (ParamDesc <> ResParamDesc) and NeedBindParam(ParamDesc) then begin
      if not Added then
        St := St + '('
      else
        St := St + ', ';
      St := St + PrepareVariable(ParamDesc);
      Added := True;
    end;
  end;

  if Added then
    St := St + ')';

  St := St + ';' + LineSeparator;
  if DeclSt <> '' then
    St := 'declare' + LineSeparator + DeclSt + St;
  if OutSt <> '' then
    St := St + OutSt;
  St := St + 'end;';

  if (ResParamDesc <> nil) and
    (ResParamDesc.ParamType = pdResult) and not FUseResultParams
  then begin
    ResParamDesc.ParamType := pdOutput;  // represent as OUT
    ResParamDesc.FIsResult := True;
  end;

  Result := St;
  FSQL := St;
  FUserSQL := Result;
end;

procedure TOCICommand.Busy;
begin
{$IFDEF MSWINDOWS}
  if FNonBlocking then
    WaitForSingleObject(hBusy, INFINITE);
{$ENDIF}
end;

procedure TOCICommand.Release;
begin
{$IFDEF MSWINDOWS}
  if FNonBlocking then
    ReleaseMutex(hBusy);
{$ENDIF}
end;

procedure TOCICommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
  {$IFDEF MSWINDOWS}
    if hExecThread <> nil then
      FConnection.StopThread(hExecThread, True);
  {$ENDIF}
    if GetActive then
      Finish;

    inherited;

    FConnection := TOCIConnection(Value);
  end;
end;

function TOCICommand.GetCursorState: TCursorState;
begin
//  WAR Blocked
  Result := FCursorRef.State;
end;

procedure TOCICommand.SetCursorState(Value: TCursorState);
begin
  if FNonBlocking then begin
    Busy;
    FCursorRef.State := Value;
    Release;
  end
  else
    FCursorRef.State := Value;
end;

function TOCICommand.GetCursor: TCRCursor;
begin
  Result := FCursorRef;
end;

procedure TOCICommand.SetCursor(Value: TCRCursor);
begin
  if Value <> FCursorRef then begin
    if (FCursorRef <> nil) and (FCursorRef <> FCursor) then
      FCursorRef.Release;

    if (Value <> nil) and (Value <> FCursor) then begin
      FCursorRef := Value as TOraCursor;
      FCursorRef.AddRef;
      FOCICallStyle := FCursorRef.FOCICallStyle;
      if FConnection <> nil then
        hOCIError := FConnection.hOCIError
      else
        hOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError; // global hOCIError

      FFetchedRows := 0;
    end
    else
      FCursorRef := FCursor;
  end;
end;

function TOCICommand.GetNextCursor: TOraCursor;
var
  i: integer;
  Param: TOraParamDesc;
  Cursor: TOraCursor;
  Found: boolean;
begin
  Found := False;
  for i := 0 to FParams.Count - 1 do begin
    Param := TOraParamDesc(FParams[i]);
    if Param.GetDataType = dtCursor then begin
      Cursor := Param.GetAsCursor;
      if not Found then begin
        if Cursor = FCursorRef then
          Found := True;
      end
      else
      if Cursor.CanFetch then begin
        Result := Cursor;
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TOCICommand.GetRowId: string;
type
  TBuf = array [0..35] of byte;
  PBuf = ^TBuf;
var
  hRowId: pOCIRowid;
  Ptr: IntPtr;
  RowId81Ptr: pRowId81;
  Res: sword;
  PLen: IntPtr;
  Len: ub2;
begin
  if GetCursorState = csInactive then begin
    Result := FRowId;
    Exit;
  end;

  PLen := Marshal.AllocHGlobal(sizeof(Integer));
  try
    if FOCICallStyle = OCI73 then begin
      Result := RowId7ToString({$IFNDEF CLR}@{$ENDIF}FCursorRef.CDA.rid)
    end
    else
    if FOCICallStyle = OCI80 then begin
      Result := '';
      Check(OCIDescriptorAlloc(FConnection.hOCIEnv, Ptr, OCI_DTYPE_ROWID, 0, nil));
      Res := OCIAttrGet1(FCursorRef.OCIStmt, OCI_HTYPE_STMT, Ptr, PLen, OCI_ATTR_ROWID, hOCIError);
      hRowId := Ptr;
      try
        if Res <> OCI_NO_DATA then begin
          Check(Res);
          if OCIVersion >= 9000 then begin
            Len := 4000;
            Ptr := Marshal.AllocHGlobal(Len);
            try
              Check(OCIRowidToChar(hRowId, Ptr, Len, hOCIError));
              Result := string(Marshal.PtrToStringAnsi(Ptr, Len));
            finally
              Marshal.FreeHGlobal(Ptr);
            end;
          end
          else
          if OCIVersion >= 8100 then begin
            RowId81Ptr := pOCIRowid81(IntPtr(hRowId)).RowId;
            if IntPtr(RowId81Ptr) <> nil then begin
              if RowId81Ptr.filler = 2 then begin// UROWID
                Result := URowIdToString(RowId81Ptr, hRowId.RowId.ridfilenum)
              end
              else
                Result := RowId81ToString(RowId81Ptr);
            end;
          end
          else
            Result := RowId8ToString({$IFNDEF CLR}@{$ENDIF}hRowId.RowId);
        end;
      finally
        if not IsLibrary then
        /// We are forced to disable OCIDescriptorFree call in library because of
        /// strange bug on second DML execute with it under Windows XP,
        /// Windows 98 SE. Now memory will leak. :(
          Check(OCIDescriptorFree(hRowId, OCI_DTYPE_ROWID));
      end;
    end
    else
      CheckOCI;
  finally
    Marshal.FreeHGlobal(PLen);
  end;
end;

function TOCICommand.GetSQLType: integer;
begin
  Result := FLastSQLType;
end;

function TOCICommand.GetActive: boolean;
begin
  Result := GetCursorState <> csInactive;
end;

function TOCICommand.GetPrepared: boolean;
begin
  Result := GetCursorState >= csPrepared;
end;

procedure TOCICommand.SetOCICallStyle(Value: TOCICallStyle);
begin
  if Value <> FOCICallStyle then begin
    if GetActive then
      Finish;

    FOCICallStyle := Value;
    FCursorRef.FOCICallStyle := FOCICallStyle;
  end;
end;

procedure TOCICommand.SetArrayLength(Value: integer);
var
  i : integer;
begin
  for i := 0 to FParams.Count - 1 do
    TOraParamDesc(FParams[i]).FLength := Value;
end;

function TOCICommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    {$IFDEF MSWINDOWS}
    prNonBlocking:
      FNonBlocking := Boolean(Value);
    {$ENDIF}
    {$IFDEF LINUX}
    prNonBlocking:
      FNonBlocking := False;  // doesn't support non blocking
    {$ENDIF}
    prFieldsAsString:
      FFieldsAsString := Boolean(Value);
    prCacheLobs:
      FCacheLobs := Boolean(Value);
    prScrollableCursor:
      FCursorRef.FScrollable := Boolean(Value);
    prStoreRowId:
      FStoreRowId := Boolean(Value);
    prRawAsString:
      FRawAsString := Boolean(Value);
    prNumberAsString:
      FNumberAsString := Boolean(Value);
    prSmallintPrecision:
      FSmallintPrecision := Value;
    prIntegerPrecision:
      FIntegerPrecision := Value;
    prFloatPrecision:
      FFloatPrecision := Value;
    prLargeintPrecision:
      FLargeIntPrecision := Value;
    prBCDPrecision:
      GetPrecAndScale(Value, FBCDPrecision, FBCDScale);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    prFmtBCDPrecision:
      GetPrecAndScale(Value, FFmtBCDPrecision, FFmtBCDScale);
  {$ENDIF}
  {$ENDIF}
    prTemporaryLobUpdate:
      FTemporaryLobUpdate := Boolean(Value);
    prStatementCache:
      FStatementCache := Boolean(Value);
    prPrefetchRows:
      FCursor.FPrefetchRows := Value;
    prCheckParamHasDefault:
      FCheckParamHasDefault := Value;
    prUseResultParams:
      FUseResultParams := Value;
    prUseDefaultDataTypes:
      FUseDefaultDataTypes := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCICommand.GetProp(Prop: integer; var Value: variant): boolean;
var
  Buf: integer;
begin
  Result := True;
  case Prop of
    prRowsProcessed:
      if FFetchedRows > 0 then
        Value := FFetchedRows
      else
        Value := FRowsProcessed;
    prSQLType:
      Value := FSQLType;
    prErrorOffset: begin
      Buf := FErrorOffset;
      RemoveCRSymbols(TrimRight(FSQL), Buf);
      Value := Buf;
    end;
    prStoreRowId:
      Value := FStoreRowId;
    prTemporaryLobUpdate:
      Value := FTemporaryLobUpdate;
    prFieldsAsString:
      Value := FFieldsAsString;
    prCacheLobs:
      Value := FCacheLobs;
    prRawAsString:
      Value := FRawAsString;
    prNumberAsString:
      Value := FNumberAsString;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

{ TOCISQLInfo }

function TOCISQLInfo.NextCharQuotesNeed(Ch: _Char; IdCase: TIdentCase): boolean;
begin
  case IdCase of
    icUpper:
      case Ch of
        'A'..'Z', '_', '0'..'9', '$', '#': Result := False;
      else
        Result := True;
      end;
    icLower:
      case Ch of
        'a'..'z', '_', '0'..'9', '$', '#': Result := False;
      else
        Result := True;
      end;
  else
    case Ch of
      'a'..'z', 'A'..'Z', '_', '0'..'9', '$', '#': Result := False;
    else
      Result := True;
    end;
  end;
end;

function TOCISQLInfo.IdentCase: TIdentCase;
begin
  Result := icUpper;
end;

function TOCISQLInfo.NormalizeName(const Value: _string; const LeftQ: _char; const RightQ: _char; QuoteNames: boolean = False; UnQuoteNames: boolean = False): _string;
var
  i,j: integer;
begin
  if IsQuoted(Value) then begin
    i := Pos(RightQ + '.', Value);
    j := Pos('@' + LeftQ, Value);
  end
  else begin
    i := Pos('.', Value) - 1;
    j := Pos('@', Value) - 1;
  end;
  if (i > 0) then
    Result := NormalizeName(Copy(Value, 1, i), LeftQ, RightQ, QuoteNames, UnQuoteNames) +
      '.' + NormalizeName(Copy(Value, i + 2, Length(Value) - i + 1), LeftQ, RightQ, QuoteNames, UnQuoteNames)
  else
  if (j > 0) then
    Result := NormalizeName(Copy(Value, 1, j), LeftQ, RightQ, QuoteNames, UnQuoteNames) +
      '@' + NormalizeName(Copy(Value, j + 2, Length(Value) - j + 1), LeftQ, RightQ, QuoteNames, UnQuoteNames)
  else begin
    if not IsQuoted(Value) and not QuoteNames then
      Result := _UpperCase(Value)
    else
      Result := Value;

    if not UnQuoteNames and (QuoteNames or QuotesNeeded(Result)) then
      Result := Quote(Result, LeftQ, RightQ)
    else
      Result := UnQuote(Result)
  end;
end;

function TOCISQLInfo.NormalizeName(const Value: _string; QuoteNames: boolean = False; UnQuoteNames: boolean = False): _string;
begin
  Result := NormalizeName(Value, LeftQuote, RightQuote, QuoteNames, UnQuoteNames);
end;

procedure TOCISQLInfo.SplitObjectName(const Name: _string; var Info: TExtTableInfo);
var
  i, p, Len: integer;
  InQuote: boolean;
begin
  Info.Table := '';
  Info.Schema := '';
  Info.Catalog := '';
  Info.DBLink := '';

  Len := Length(Name);
  p := 1;
  InQuote := False;

  for i := 1 to Len + 1 do begin
    if (i <= Len) and (Name[i] = '"') then
      InQuote := not InQuote;

    if (i = Len + 1) or (not InQuote and (Name[i] = '.')) then begin
      if p > 1 then
        Info.Schema := Info.Table;
      Info.Table := Copy(Name, p, i - p);
      p := i + 1;
    end;

    if (i <= Len) and not InQuote and (Name[i] = '@') then begin
      if p > 1 then
        Info.Schema := Info.Table;
      Info.Table := Copy(Name, p, i - p);
      Info.DBLink := Copy(Name, i + 1, MaxInt);
      break;
    end;
  end;
end;

procedure TOCISQLInfo.ParseTablesInfo(const SQL: _string; TablesInfo: TCRTablesInfo);
var
  Parser: TOraParser;
  StLex, Name, Alias: _string;
  CodeLexem: integer;
  PriorLexem: integer;
  PriorStLex: _string;
  BracketCount: integer;
  TableInfo: TCRTableInfo;
begin
  TablesInfo.Clear;

  Parser := TOraParser.Create(SQL);
  TablesInfo.BeginUpdate;
  Parser.OmitBlank := True;
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
          CodeLexem := Parser.GetNext(StLex);
          if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) then begin
            Name := StLex;
            Parser.GetNext(StLex);
            if StLex = '.' then begin
              Name := Name + StLex;
              CodeLexem := Parser.GetNext(StLex);
              if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) then
                Name := Name + StLex
              else
                break;
            end
            else
              Parser.Back;

            CodeLexem := Parser.GetNext(StLex);

          /// check for remote server
            if StLex = '@' then begin
              CodeLexem := Parser.GetNext(StLex);
              if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) then begin
                Name := Name + '@';
                repeat
                  PriorLexem := CodeLexem;
                  PriorStLex := StLex;
                  Name := Name + StLex;
                  CodeLexem := Parser.GetNext(StLex);
                  if (StLex = '.') and
                    ((PriorLexem = lcIdent) or (PriorLexem >= lxSQLFirst))
                  then
                    continue;
                  if (PriorStLex = '.') and
                    ((CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst))
                  then
                    continue;
                  break;
                until False;
              end;
            end;

            if CodeLexem = lxPARTITION then begin
              CodeLexem := Parser.GetNext(StLex);
              if StLex = '(' then begin
                if not Parser.ToLexem(')') then
                  break;
                CodeLexem := Parser.GetNext(StLex);
              end;
            end;

            if (CodeLexem = lcIdent) or (CodeLexem >= lxSQLFirst) and
              (CodeLexem <> lxJOIN) and (CodeLexem <> lxINNER) and
              (CodeLexem <> lxLEFT) and (CodeLexem <> lxRIGHT) and
              (CodeLexem <> lxFULL) and
              not Parser.IsClauseLexem(CodeLexem)
            then begin
              Alias := StLex;
              Parser.GetNext(StLex);
            end
            else
              Alias := '';

            TableInfo := TablesInfo.Add;
            TableInfo.TableName := NormalizeName(Name);
            TableInfo.TableAlias := NormalizeName(Alias);
          end
          else // skip subquerys
          if StLex = '(' then begin
            BracketCount := 0;
            while (Parser.GetNext(StLex) <> lcEnd) and ((StLex <> ')') or (BracketCount > 0)) do
              if StLex = '(' then
                inc(BracketCount)
              else
              if StLex = ')' then
                dec(BracketCount);
            if StLex = ')' then
              Parser.GetNext(StLex);
            if StLex <> ',' then
              Parser.GetNext(StLex);
          end
          else
            break;
        until (StLex <> ',');
    end;
  finally
    TablesInfo.EndUpdate;
    Parser.Free;
  end;
end;

class function TOCISQLInfo.ParseSPName(FullName: _string; var Name: _string; var Overload: integer): boolean;
var
  i: integer;
  str_index: integer;
begin
  Result := False;

  str_index := 0;
  for i := Length(FullName) downto 1 do begin
    if (FullName[i] = '"') or (FullName[i] = '.') then begin
      break;
    end;
    if FullName[i] = ':' then begin
      if str_index = 0 then begin
        Result := True;
        str_index := i;
      end
      // if more then one ":"
      else begin
        Result := False;
        break;
      end;
    end;
  end;

  if Result then begin
    if TryStrToInt(Trim(copy(FullName, str_index + 1, MaxInt)), Overload) then
      Name := Trim(copy(FullName, 1, str_index - 1))
    else begin
      Result := False;
      Name := FullName;
      Overload := 0;
    end;
  end
  else begin
    Name := FullName;
    Overload := 0;
  end;
end;

class function TOCISQLInfo.GetFinalSPName(Name: _string; Overload: integer): _string;
var
  SPOverload: integer;
begin
  if (Name <> '') and (Overload > 0) then begin
    if ParseSPName(Name, Result, SPOverload) then
      Result := Result + ':' + IntToStr(Overload)
    else
      Result := Name + ':' + IntToStr(Overload);
  end
  else
    Result := Name;
end;

{ TOCIFieldDesc }

function TOCIFieldDesc.IsNational: boolean;
begin
  Result := (SubDataType = dtNString) or (SubDataType = dtNWideString) or
    (SubDataType = dtNClob);
end;

{ TOCIRecordSet }

constructor TOCIRecordSet.Create;
begin
  inherited Create;

  FRequireEmptyStrToNull := True;

  FFetchRows := 25;
{$IFDEF MSWINDOWS}
  hEvent := TEvent.Create(nil, True, True, '');
{$ENDIF}
  FHasObjectFields := False;
end;

destructor TOCIRecordSet.Destroy;
begin
  Close;

{$IFDEF MSWINDOWS}
  if hExecFetchThread <> nil then begin
    FConnection.StopThread(hExecFetchThread, True);
  end;
  if hFetchAllThread <> nil then begin
    FConnection.StopThread(hFetchAllThread, True);
  end;
  hEvent.Free;
{$ENDIF}

  FreeFetchBlock;

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

procedure TOCIRecordSet.CreateCommand;
begin
  SetCommand(TOCICommand.Create);
end;

procedure TOCIRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TOCICommand(Value);
  if FCommand <> nil then begin
    FConnection := FCommand.FConnection;
  end;
end;

function TOCIRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TOCIFieldDesc;
end;

{ Open /Close }

procedure TOCIRecordSet.InternalPrepare;
begin
  if FCommand.NativeCursor then
    FCommand.Prepare;

  InitFetchCursor;

  SetCommandType;
end;

procedure TOCIRecordSet.SetCommandType;
begin
  case FCommand.FSQLType of
    SQL_UNKNOWN:
      CommandType := ctUnknown;
    SQL_SELECT:
      CommandType := ctCursor;
  else
    if FCommand.RowsReturn or (FCommand.FSQLType = SQL_PLSQL) and (FFetchCursor <> nil) then
      CommandType := ctCursor
    else
      CommandType := ctStatement;
  end;
end;

function TOCIRecordSet.CanFetchBack: boolean;
begin
  Result := (FCommand.FCursorRef <> nil) and FCommand.FCursorRef.FScrollable;
end;

function TOCIRecordSet.NeedInitFieldsOnPrepare: boolean;
begin
  Result := inherited NeedInitFieldsOnPrepare or HasCursorParams;
end;

procedure TOCIRecordSet.InternalUnPrepare;
begin
  if not FCommand.NativeCursor then begin
    try
      if FCommand.GetCursorState <> csInactive then
        FCommand.Finish;
    finally
      FCommand.SetCursor(nil);
      FFetchCursor.FreeCursor; // ignore ref count
    end;
  end;

  if FCommand.GetCursorState <> csInactive then
    FCommand.Finish;

  // CommandType & FCommand.FSQLType should be reset in any case
  // (even if FCommand.GetCursorState = csInactive)
  CommandType := ctUnknown;
  FCommand.FSQLType := SQL_UNKNOWN;
end;

procedure TOCIRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  inherited;
end;

procedure TOCIRecordSet.InternalClose;
{$IFDEF MSWINDOWS}
var
  AMsg: TMSG;
{$ENDIF}
begin
  if (hExecFetchThread <> nil) or (hFetchAllThread <> nil) then begin
    if FCommand.Executing then // doesn't break if not Executing
      FCommand.BreakExec;
  {$IFDEF CLR}
    hEvent.WaitFor(INFINITE);
    hEvent.WaitFor(INFINITE);
  {$ENDIF}
  {$IFDEF WIN32_64}
    // Additional WaitForSingleObject is set to wait for event to pulse on case
    // if hExecFetchThread is in FetchArray or in FetchPiece function. This was
    // made to ensure that this thread will wait for for final SetEvent in
    // DoExecFetch procedure.
    WaitForSingleObject(THandle(hEvent.Handle), INFINITE);
    WaitForSingleObject(THandle(hEvent.Handle), INFINITE);
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    while hExecFetchThread <> nil do begin
      if PeekMessage(AMsg, hODACWindow, 0, 0, PM_REMOVE) then
        DispatchMessage(AMsg);
    end;
  {$ENDIF}
  end;

  if GetNonBlocking and (hFetchAllThread <> nil) then begin
    FStopFetch := True;
    while FFetching do sleep(0);
  {$IFDEF MSWINDOWS}
    if PeekMessage(AMsg, hODACWindow, WM_ENDTHREAD, WM_ENDTHREAD, PM_REMOVE) then
      DispatchMessage(AMsg);
  {$ENDIF}
  end;

  if not FCommand.NativeCursor then begin
    if FCommand.GetCursorState <> csInactive then
      FCommand.Finish;
    FFetchCursor.FreeCursor; // ignore ref count
    FCommand.SetCursor(nil);
  end;

  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  inherited;

  FreeFetchBlock;
  if not Prepared then
    InternalUnprepare;
end;

procedure TOCIRecordSet.DoExecFetch;
var
  OldExecuted: boolean;
  CanExecuteFetch: boolean;
  i: integer;
begin
{$IFDEF MSWINDOWS}
  PulseEvent(THandle(hEvent.Handle));
{$ENDIF}
  try
    CanExecuteFetch := (FCommand.GetCursorState = csPrepared) and (FCommand.FSQLType = SQL_SELECT) and
      (FCommand.FOCICallStyle = OCI80) and not (PossibleOCICallStyles = [OCI80]);
    OldExecuted := (FCommand.GetCursorState = csExecuted);

    if FCommand.NativeCursor and not OldExecuted then begin
      if not Prepared then
        InternalPrepare;
      FCommand.BindParams;
      if not CanExecuteFetch then
        FCommand.Exec;     //if DataSet is prepared(desribeonly) then execute and fetch can be combined
    end;

    if not FDisableInitFields then begin
      InitFields;
    end;

    if not RowsReturn then
      RaiseError(SNotRows);

    if CanFetchBack then begin
      CheckOCI90;
      FPieceFetch := True;
      BlockMan.DefaultItemCount := FFetchRows;
    end
    else begin
      FPieceFetch := False;
      if FCommand.FOCICallStyle = OCI73 then // piece fetch is used with OCI7 only
        for i := 0 to FFields.Count - 1 do
          if (FFields[i].DataType in [dtBlob,dtMemo]) and (FFields[i].Length = 0)
          then begin
            FPieceFetch := True;
            break;
          end;
    end;

    FCommand.SetCursor(FFetchCursor);

    if GetNonBlocking then begin
      Fetch;
      CurrentItem := FirstItem;
      FBOF := IntPtr(FirstItem) = nil;
    end;
  finally
{$IFDEF MSWINDOWS}
    hEvent.SetEvent;
{$ENDIF}
  end;
end;

procedure TOCIRecordSet.EndExecFetch(E: Exception);
var
  Fail: boolean;
begin
{$IFDEF MSWINDOWS}
  if hExecFetchThread <> nil then begin
//    FConnection.StopThread(hExecFetchThread);
    hExecFetchThread := nil;
  end
  else
    if (FCommand = nil) or GetNonBlocking then Exit;
{$ENDIF}

  try
    if Assigned(FAfterExecFetch) then //moved here to start exactly after execute
      FAfterExecFetch(E = nil);

    if (E = nil) and
      (FFetchAll or (GetNonBlocking and (IndexFields.Count > 0)))
    then
      if FCommand.GetCursorState < csFetched then
        FetchAll
      else
        if GetNonBlocking and (IndexFields.Count > 0) then
          SortItems;
    if (E = nil) and GetNonBlocking and (FTempFilterText <> '') then begin
      inherited SetFilterText(FTempFilterText);
      FTempFilterText := '';
      inherited FilterUpdated;
      if Assigned(FOnDataChanged) then
        FOnDataChanged;
    end;

    if FFetchAll and GetNonBlocking and (FCommand.GetCursorState <> csFetchingAll) then
      StopWait; //Reset screen cursor
  finally
    FCommand.Executing := False;
  end;

  if FCommand.FNonBlocking and (E is EOraError) then begin
    Fail := True;
    FConnection.DoError(EOraError(E), Fail);
    if not Fail then
      Abort;
  end;
end;

procedure TOCIRecordSet.ExecCommand; // Execute command
var
  NeedPrepare: boolean;
begin
  NeedPrepare := (CommandType <> ctCursor) and not Prepared;

  if NeedPrepare then
    FCommand.Prepare;

  try
    inherited;

    InitFetchCursor;

    SetCommandType;

    FCommand.FForceUnprepare := FCommand.FNonBlocking and (CommandType <> ctCursor);
  finally // for Unprepare on Exception
    if not FCommand.FNonBlocking and (CommandType <> ctCursor) and NeedPrepare
      and FCommand.GetPrepared then //Disconnect mode collision (Unprepare occurs on Disconnect (AfterExecute))
      FCommand.Unprepare;
  end;
end;

procedure TOCIRecordSet.BreakFetch;
begin
  FStopFetch := True;
end;

procedure TOCIRecordSet.ExecFetch(DisableInitFields: boolean);
begin
  FCommand.Executing := True;
  if FCommand.FNonBlocking then begin
    if not FConnection.FThreadSafety then
      RaiseError(SNeedThreadSafety);

    // InitFields here for valid DataSet.CreateFieldDefs
    if not DisableInitFields then begin
      InitFields;
      SetCommandType;
    end;
    FilterFunc := nil;
    inherited SetFilterText('');
  {$IFDEF MSWINDOWS}
    StringHeap.ThreadSafety := True;
    hEvent.ResetEvent;
    FDisableInitFields := True;
    hExecFetchThread := FConnection.RunThread(DoExecFetch, EndExecFetch);
  {$ENDIF}
  end
  else begin
    StringHeap.ThreadSafety := False;
    try
      FDisableInitFields := DisableInitFields;
      DoExecFetch;
    except
      on E: Exception do begin
        EndExecFetch(E);
        raise;
      end;
    end;
    EndExecFetch(nil);
  end;
end;

function TOCIRecordSet.IsFullReopen: boolean;
begin
  Result := not FCommand.NativeCursor;
end;

procedure TOCIRecordSet.Reopen;
{$IFDEF MSWINDOWS}
var
  Msg: TMsg;
{$ENDIF}
begin
  if FCommand.NativeCursor then begin
    //FCommand.BindParams;  // for reallocated Param

  {$IFDEF MSWINDOWS}
    if GetNonBlocking then begin
      while (hExecFetchThread <> nil) do
        PeekMessage(Msg, hODACWindow, 0, 0, PM_REMOVE);

      while (GetNonBlocking) and (hFetchAllThread <> nil) do begin
        FStopFetch := True;
        PeekMessage(Msg, hODACWindow, 0, 0, PM_REMOVE);
      end;
    end;
  {$ENDIF}

    FreeData;
    InitData;
    if Assigned(FOnDataChanged) then
      // perform dataset resync to prevent AV if grid is repainted in BeforeFetch/AfterFetch events
      FOnDataChanged;

    InternalOpen(True);
  end
  else begin
    if FCommand.FSQLType = SQL_UNKNOWN then
      RaiseError(SReopenNotAllowed);

    Close;
    Open;
  end;
end;

procedure TOCIRecordSet.Disconnect;
begin
  GetDisconnectedMode;
  GetUseUnicode;
  GetCharLength;

  inherited;
end;

{ Fields}

function TOCIRecordSet.GetIndicatorSize: word;
begin
  Result := FieldCount*sizeof(sb2);
end;

function TOCIRecordSet.HasCursorParams: boolean;
var
  i: integer;
begin
  Result := false;

  for i := 0 to FCommand.GetParamCount - 1 do
    if FCommand.GetParam(i).DataType = dtCursor then begin
      Result := true;
      exit;
    end;
end;

procedure TOCIRecordSet.InitFetchCursor;
var
  i: integer;
  Cur: TOraCursor;
  State: integer;
begin
  FFetchCursor := FCommand.FCursorRef;

  if FCommand.NativeCursor and (FCommand.FSQLType = SQL_PLSQL) then begin
    FFetchCursor := nil;

    for i := 0 to FCommand.GetParamCount - 1 do
      if FCommand.GetParam(i).DataType = dtCursor then begin
        Cur := FCommand.GetParam(i).GetAsCursor;

        // first cursor that executed (will return rows)
        if (Cur <> nil) and (Cur.hOCIStmt <> nil) then begin
          if OCIVersion >= 9100 then // supported starting with 9.1
            Check(OCIAttrGet2(Cur.OCIStmt, OCI_HTYPE_STMT, State, nil, OCI_ATTR_STMT_STATE, FCommand.hOCIError))
          else
            State := OCI_STMT_STATE_EXECUTED;
          if State = OCI_STMT_STATE_EXECUTED then begin
            FFetchCursor := Cur;
            break;
          end;
        end;
      end;

    // redefine command type - depend on cursor: canbe executed or not
    if FFetchCursor = nil then
      FCommandType := ctStatement
    else
      FCommandType := ctCursor;
  end;
end;

procedure TOCIRecordSet.InternalInitFields;
var
  i: integer;
  Field: TFieldDesc;
  OldCursorState: TCursorState;
  OldCursor: TOraCursor;
  DescribeExecute: boolean;
begin
  inherited;

  OldCursorState := FCommand.GetCursorState;
  OldCursor := FCommand.FCursorRef;
  DescribeExecute := False;

  try
    if FCommand.NativeCursor then begin
      if FCommand.GetCursorState = csInactive then
        FCommand.Prepare;

      InitFetchCursor;

      if FCommand.FSQLType = SQL_SELECT then begin
        if (FCommand.FOCICallStyle = OCI80) and (FCommand.GetCursorState < csExecuted) then begin
          if FCommand.GetCursorState < csBound then  // to correct describe type of expression
            FCommand.BindParams;
          FCommand.InternalExecute(OCI_DESCRIBE_ONLY);
        end;
      end
      else if FCommand.FSQLType = SQL_PLSQL then begin
          if FCommand.GetCursorState < csBound then
            FCommand.BindParams;

          if FCommand.GetCursorState < csExecuted then begin
            FCommand.Exec;   // need for describe
            DescribeExecute := True;
            InitFetchCursor; // exit if all returned cursors are not in executed state
          end;

          if CommandType <> ctCursor then
            Exit;

          FCommand.SetCursor(FFetchCursor);
        end
        else
          Exit;
    end
    else begin
      InitFetchCursor;

      FCommand.CheckActive;
    end;

    FHasConvertedFields := False;

    FConnection.BusyWait;
    try
      i := 1;
      while True do begin
        Field := GetFieldDescType.Create;
        if FCommand.GetFieldDesc(i, Field, FLongStrings, FFlatBuffers) then begin
          Field.ActualName := Field.Name;
          Field.FieldNo := FFields.Count + 1;
          FHasConvertedFields := FHasConvertedFields or IsConvertedFieldType(Field.DataType);
          FFields.Add(Field);
          Inc(i);

          if FConnection.FDisconnectMode then
            if Field.DataType in [dtObject, dtXML, dtReference, dtArray, dtTable] then
              Field.ReadOnly := True;
          if Field.DataType in [dtObject,dtArray] then
            InitObjectFields(Field.ObjectType, Field);
        end
        else begin
          Field.Free;
          break;
        end;
      end;
    finally
      FConnection.Release;
    end;
  {$IFNDEF LITE}
    FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, TablesInfo);
    TablesInfo.CaseSensitive := True;
  {$ENDIF}
    FHasObjectFields := HasFields([dtObject, dtXML, dtReference, dtArray, dtTable]);
  finally
    if not FCommand.NativeCursor then
      if FCommand.FCursorRef <> OldCursor then
        FCommand.SetCursor(OldCursor);

    if DescribeExecute then
      for i := 0 to FCommand.Params.Count - 1 do
        if FCommand.GetParam(i).DataType = dtCursor then
          FCommand.GetParam(i).GetAsCursor.FreeCursor;

    if (OldCursorState = csInactive) and (FCommand.GetCursorState <> csInactive) then
      FCommand.Finish
    else
      FCommand.SetCursorState(OldCursorState);
  end;
end;

procedure TOCIRecordSet.GetDateFromBuf(Buf: IntPtr; Field: TFieldDesc; Date: IntPtr; Format: TDateFormat);
var
  DateTime: TDateTime;
begin
  if Field.HasParent then begin
    inherited;
    exit;
  end;

  case Format of
    dfMSecs: begin
      try
        Marshal.WriteInt64(Date, BitConverter.DoubleToInt64Bits(OraDateToMSecs(Buf)));
      except
        on E: EConvertError do
          // 1 day - to prevent error on converting to TDateTime
          Marshal.WriteInt64(Date, BitConverter.DoubleToInt64Bits(MSecsPerDay));
      end;
    end;
    dfDateTime: begin
      try
        DateTime := OraDateToDateTime(Buf);
        Marshal.WriteInt64(Date, BitConverter.DoubleToInt64Bits(DateTime));
      except
        on E: EConvertError do
          Marshal.WriteInt64(Date, 0);
      end;
    end;
  else
    // for calc fields
    inherited;
  end;
end;

procedure TOCIRecordSet.PutDateToBuf(Buf: IntPtr; Field: TFieldDesc; Date: IntPtr; Format: TDateFormat);
begin
  if Field.HasParent then begin
    inherited;
    exit;
  end;

  case Format of
    dfMSecs:
      MSecsToOraDate(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Date)), Buf);
    dfDateTime:
      DateTimeToOraDate(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Date)), Buf);
  else
    inherited;
  end;
end;

function TOCIRecordSet.GetNull(FieldNo: word; RecBuf: IntPtr): boolean;
var
  Field: TFieldDesc;
  Buf: IntPtr;
  OraObject: TOraObject;
begin
  Field := Fields[FieldNo - 1];
  if not Field.HasParent then begin
      case Field.DataType of
        dtObject, dtArray, dtTable: begin
          // indicator in record buffer is not synchronized with object state
          OraObject := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
          Result := OraObject.IsNull;
        end;
      else
        Result :=
          Marshal.ReadInt16(RecBuf, DataSize + (FieldNo - 1) * sizeof(sb2)) = -1;
      end;

      if not Result then begin
        case Field.DataType of
          dtDateTime: begin
            Buf := PtrOffset(RecBuf, Field.Offset);
            Result :=
          {$IFDEF CLR}
              (Marshal.ReadByte(Buf, 0) <= 100) or
          {$ELSE}
              ((Marshal.ReadByte(Buf, 0) = 100) and (Marshal.ReadInt32(Buf, 1) = 100)) or // only exclude year zero
          {$ENDIF}
              (Marshal.ReadInt16(Buf, 2) = 0) or
              (Marshal.ReadByte(Buf, 4) < 1) or (Marshal.ReadByte(Buf, 5) < 1) or (Marshal.ReadByte(Buf, 6) < 1) or
              (Marshal.ReadByte(Buf, 4) > 24) or (Marshal.ReadByte(Buf, 5) > 60) or
              (Marshal.ReadByte(Buf, 6) > 60);
          end;
        end;
      end;
    //end;
  end
  else
    Result := GetChildFieldIsNull(Field, RecBuf);
  if Result then
    Result := GetNullByBlob(FieldNo, RecBuf);
end;

procedure TOCIRecordSet.SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean);
var
  Field: TFieldDesc;
  Blob: TBlob;
  Ind: OCIInd;
  OraTimeStamp: TOraTimeStamp;
  OraInterval: TOraInterval;
  OraNumber: TOraNumber;
  OraObject: TOraObject;
  ObjPtr: IntPtr;
begin
  Field := Fields[FieldNo - 1];
  if not Field.HasParent then begin
    if Value then
      Ind := -1
    else
      Ind := 0;

    Marshal.WriteInt16(
      RecBuf, DataSize + (FieldNo - 1) * SizeOf(sb2), Ind);

    if Value and IsBlobFieldType(Field.DataType) then begin // clear Blob value
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
      if Blob <> nil then
        Blob.Clear;
    end;

    case Field.DataType of
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
        OraTimeStamp := TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
        if OraTimeStamp <> nil then
          Marshal.WriteInt16(OraTimeStamp.FIndicator, Ind);
      end;
      dtIntervalYM, dtIntervalDS: begin
        OraInterval := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
        if OraInterval <> nil then
          Marshal.WriteInt16(OraInterval.FIndicator, Ind);
      end;
      dtNumber: begin
        ObjPtr := Marshal.ReadIntPtr(RecBuf, Field.Offset);
        OraNumber := TOraNumber(GetGCHandleTarget(ObjPtr));
        if OraNumber <> nil then
          Marshal.WriteInt16(OraNumber.FIndicator, Ind);
      end;
      dtObject, dtArray, dtTable: begin
        OraObject := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
        if OraObject <> nil then
          OraObject.IsNull := Value;
      end;
    end;
  end
  else
    PutChildField(Field, RecBuf, nil);
end;

class function TOCIRecordSet.IsBlobFieldType(DataType: word): boolean;
begin
  case DataType of
    dtBlob,dtMemo,dtWideMemo,dtOraBlob,dtOraClob,dtWideOraClob,dtBFile,dtCFile:
      Result := True;
  else
    Result := False;
  end;
end;

class function TOCIRecordSet.IsComplexFieldType(DataType: word): boolean;
begin
  case DataType of
    dtCursor, dtObject, dtXML, dtReference, dtArray, dtTable, dtTimeStamp, dtTimeStampTZ,
    dtTimeStampLTZ, dtIntervalYM, dtIntervalDS, dtExtString, dtExtWideString, dtExtVarBytes,
    dtNumber:
      Result := True;
  else
    Result := IsBlobFieldType(DataType);
  end;
end;

function TOCIRecordSet.IsConvertedFieldType(DataType: word): boolean;
begin
  case DataType of
    dtLargeint, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD, dtSQLTimeStamp{$ENDIF}{$ENDIF}:
      Result := True;
  else
    Result := False;  
  end;
end;

{ Records }

procedure TOCIRecordSet.CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Ptr: IntPtr;
  Lob: TOraLob;
  Blob: TBlob;
  BFile: TOraFile;
  Cursor: TOraCursor;
  OraObject: TOraObject;
  OraRef: TOraRef;
  OraArray: TOraArray;
  OraNestTable: TOraNestTable;
  OraTimeStamp: TOraTimeStamp;
  FieldDesc: TFieldDesc;
begin
{ TODO : FConnection could be nil }

  if not HasComplexFields then
    Exit;

  inherited;

  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if not FieldDesc.HasParent and
      (not(IsBlobFieldType(FieldDesc.DataType)) or WithBlob)
    then begin
      Ptr := PtrOffset(RecBuf, FieldDesc.Offset);
      case FieldDesc.DataType of
        dtBlob, dtMemo, dtWideMemo:
          if WithBlob then begin
            Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, FieldDesc.Offset)));
            if (FCommand.FOCICallStyle = OCI80) and
              ((FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtWideString))
            then
              Blob.IsUnicode := UseUnicode;
          end;
        dtOraBlob,dtOraClob,dtWideOraClob: begin
          Lob := TOraLob.Create(FConnection.GetSvcCtx);
          if FieldDesc.DataType <> dtOraBlob then begin
            Lob.IsUnicode := UseUnicode;
            Lob.LobType := ltClob;
            Lob.FCharLength := CharLength;
          end
          else
            Lob.LobType := ltBlob;
          Lob.AllocLob;
          Lob.Cached := FCommand.FCacheLobs;
          Lob.EnableRollback;
          Marshal.WriteIntPtr(Ptr, Lob.GCHandle);
        end;
        dtBFile,dtCFile: begin
          if (FConnection <> nil) and FConnection.GetConnected then
            BFile := TOraFile.Create(FConnection.GetSvcCtx)
          else
            BFile := TOraFile.Create(nil);          
          BFile.AllocLob;
          BFile.Cached := FCommand.FCacheLobs;
          BFile.EnableRollback;
          Marshal.WriteIntPtr(Ptr, BFile.GCHandle);
        end;
        dtCursor: begin
          Cursor := TOraCursor.Create;
          Cursor.FOCICallStyle := FCommand.FOCICallStyle;
          Marshal.WriteIntPtr(Ptr, Cursor.GCHandle);
        end;
        dtXML: begin
          if (FConnection = nil) or not FConnection.GetConnected then
            RaiseError('Unsupported Disconnected mode type');
          if not TOraType(FieldDesc.ObjectType).Valid then
            TOraType(FieldDesc.ObjectType).Describe(FConnection.GetSvcCtx);

          OraObject := TOraXML.Create(TOraType(FieldDesc.ObjectType));
          if ((OCIVersion > 9203) and (OCIVersion < 9206)) then    //Bug with memory
            OraObject.AllocObject(FConnection.GetSvcCtx);            //leak with 9204 client
          OraObject.OCIError := hOCIError;
          Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
        end;
        dtObject: begin
          if (FConnection = nil) or not FConnection.GetConnected then
            RaiseError('Unsupported Disconnected mode type');
          if not TOraType(FieldDesc.ObjectType).Valid then
            TOraType(FieldDesc.ObjectType).Describe(FConnection.GetSvcCtx);

          OraObject := TOraObject.Create(TOraType(FieldDesc.ObjectType));
          OraObject.OCIError := hOCIError;
          //OraObject.AllocObject(FConnection.GetSvcCtx); // ??? Optim?
          OraObject.NativeInstance := True;
          Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
        end;
        dtReference: begin
          if (FConnection = nil) or not FConnection.GetConnected then
            RaiseError('Unsupported Disconnected mode type');
          if not TOraType(FieldDesc.ObjectType).Valid then
            TOraType(FieldDesc.ObjectType).Describe(FConnection.GetSvcCtx);

          OraRef := TOraRef.Create(TOraType(FieldDesc.ObjectType));
          OraRef.OCIError := hOCIError;
          Marshal.WriteIntPtr(Ptr, OraRef.GCHandle);
        end;
        dtArray: begin
          if (FConnection = nil) or not FConnection.GetConnected then
            RaiseError('Unsupported Disconnected mode type');
          if not TOraType(FieldDesc.ObjectType).Valid then
            TOraType(FieldDesc.ObjectType).Describe(FConnection.GetSvcCtx);

          OraArray := TOraArray.Create(TOraType(FieldDesc.ObjectType));
          OraArray.OCIError := hOCIError;
          OraArray.AllocObject(FConnection.GetSvcCtx); // ??? Optim?
          Marshal.WriteIntPtr(Ptr, OraArray.GCHandle);
        end;
        dtTable: begin
          if (FConnection = nil) or not FConnection.GetConnected then
            RaiseError('Unsupported Disconnected mode type');
          if not TOraType(FieldDesc.ObjectType).Valid then
            TOraType(FieldDesc.ObjectType).Describe(FConnection.GetSvcCtx);

          OraNestTable := TOraNestTable.Create(TOraType(FieldDesc.ObjectType));
          OraNestTable.OCIError := hOCIError;
          OraNestTable.AllocObject(FConnection.GetSvcCtx); // ??? Optim?
          Marshal.WriteIntPtr(Ptr, OraNestTable.GCHandle);
        end;
        dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
          OraTimeStamp := TOraTimeStamp.Create(FieldDesc.DataType);
          OraTimeStamp.Precision := FieldDesc.Length;
          Marshal.WriteIntPtr(Ptr, OraTimeStamp.GCHandle);
        end;
        dtIntervalYM, dtIntervalDS: begin
          Marshal.WriteIntPtr(Ptr, TOraInterval.Create(FieldDesc.DataType).GCHandle);
        end;
        dtNumber:
          Marshal.WriteIntPtr(Ptr, TOraNumber.Create.GCHandle);
      end;
    end;
  end;
end;

procedure TOCIRecordSet.FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  SharedObject: TSharedObject;
  FieldDesc: TFieldDesc;
begin
  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if not FieldDesc.HasParent and
      (not(IsBlobFieldType(FieldDesc.DataType)) or WithBlob)
    then
      case FieldDesc.DataType of
        dtOraBlob,dtOraClob,dtWideOraClob,dtBFile,dtCFile,
        dtCursor,
        dtObject,dtXML,dtReference,dtArray,dtTable,
        dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS,
        dtNumber: begin
          SharedObject := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, FieldDesc.Offset)));
          SharedObject.Free;
        end;
      end;
  end;

  inherited;
end;

procedure TOCIRecordSet.CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean);
var
  i: integer;
  SrcPtr: IntPtr;
  DestPtr: IntPtr;
  OraObjectSrc,OraObjectDest: TOraObject;
  FieldDesc: TFieldDesc;
begin
  inherited;

  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if not FieldDesc.HasParent and
      (not(IsBlobFieldType(FieldDesc.DataType)) or WithBlob)
      and IsComplexFieldType(FieldDesc.DataType)
    then begin
      SrcPtr := Marshal.ReadIntPtr(Source, FieldDesc.Offset);
      DestPtr := Marshal.ReadIntPtr(Dest, FieldDesc.Offset);
      case FieldDesc.DataType of
        dtObject,dtXML,dtReference,dtArray,dtTable: begin
          OraObjectSrc := TOraObject(GetGCHandleTarget(SrcPtr));
          OraObjectDest := TOraObject(GetGCHandleTarget(DestPtr));
          OraObjectDest.Assign(OraObjectSrc);
          if DisconnectedMode then
            OraObjectDest.CacheObject;
        end;
        dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
          TOraTimeStamp(GetGCHandleTarget(SrcPtr)).AssignTo(TOraTimeStamp(GetGCHandleTarget(DestPtr)));
        dtIntervalYM, dtIntervalDS:
          TOraInterval(GetGCHandleTarget(SrcPtr)).AssignTo(TOraInterval(GetGCHandleTarget(DestPtr)));
        dtNumber:
          TOraNumber(GetGCHandleTarget(SrcPtr)).AssignTo(TOraNumber(GetGCHandleTarget(DestPtr)));
      end;
    end;
  end;
end;

function TOCIRecordSet.CompareFieldValue(ValuePtr: IntPtr; const ValueType: integer; FieldDesc: TFieldDesc; RecBuf: IntPtr; const Options: TCompareOptions): integer;
var
  FieldBuf: IntPtr;
  IsBlank, NativeBuffer: boolean;
  OraNumber: TOraNumber;
  OraTimeStamp: TOraTimeStamp;
  OraInterval: TOraInterval;
begin
  if FieldDesc.ParentField = nil then begin
    FieldBuf := PtrOffset(RecBuf, FieldDesc.Offset);
    NativeBuffer := True;
  end
  else
    GetChildField(FieldDesc, RecBuf, FieldBuf, IsBlank, NativeBuffer);

  Result := 0;
  try
    case FieldDesc.DataType of
      dtNumber:
        if ValueType in [dtFloat, dtInt32, dtString, dtLargeint] then begin
          OraNumber := TOraNumber.Create;
          try
            case ValueType of
              dtFloat:
                OraNumber.AsFloat := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
              dtInt32:
                OraNumber.AsInteger := Marshal.ReadInt32(ValuePtr);
              dtString:
                OraNumber.AsString := string(Marshal.PtrToStringAnsi(ValuePtr));
              dtLargeint:
                OraNumber.AsLargeInt := Marshal.ReadInt64(ValuePtr);
            end;
            Result := OraNumber.Compare(TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf))));
          finally
            OraNumber.Free;
          end;
        end
        else
        if ValueType = dtNumber then begin
          OraNumber := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
          Result := OraNumber.Compare(TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf))));
        end
        else
          Assert(False);
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: // TODO: SQLTimeStamp support
        if ValueType in [dtDateTime, dtString] then begin
          OraTimeStamp := TOraTimeStamp.Create(FieldDesc.DataType);
          try
            case ValueType of
              dtDateTime:
                OraTimeStamp.AsDateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
              dtString:
                OraTimeStamp.AsString := string(Marshal.PtrToStringAnsi(ValuePtr));
            end;
            Result := OraTimeStamp.Compare(TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf))));
          finally
            OraTimeStamp.Free;
          end;
        end
        else
        if ValueType in [dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ] then begin
          OraTimeStamp := TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
          Result := OraTimeStamp.Compare(TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf))));
        end
        else
          Assert(False);
      dtIntervalYM, dtIntervalDS:
        if ValueType = dtString then begin
          OraInterval := TOraInterval.Create(FieldDesc.DataType);
          try
            OraInterval.AsString := string(Marshal.PtrToStringAnsi(ValuePtr));
            Result := OraInterval.Compare(TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf))));
          finally
            OraInterval.Free;
          end;
        end
        else
        if ValueType in [dtIntervalYM, dtIntervalDS] then begin
          OraInterval := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
          Result := OraInterval.Compare(TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf))));
        end
        else
          Assert(False);
    else
      Result := inherited CompareFieldValue(ValuePtr, ValueType, FieldDesc, RecBuf, Options);
    end;
  finally
    if not NativeBuffer then
      Marshal.FreeHGlobal(FieldBuf);
  end;
end;

function TOCIRecordSet.CompareFields(RecBuf1: IntPtr; RecBuf2: IntPtr; FieldDesc: TFieldDesc; Options: TCompareOptions = []): integer;
var
  FieldBuf: IntPtr;
  FieldBufStatic: IntPtr;
  IsBlank1, IsBlank2: boolean;
  DataType: word;
  ValueBuf: IntPtr;
begin
  case FieldDesc.DataType of
    dtNumber,
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ,
    dtIntervalYM, dtIntervalDS: begin
    end;
  else
    Result := inherited CompareFields(RecBuf1, RecBuf2, FieldDesc, Options);
    Exit;
  end;

  FieldBufStatic := nil;
  ValueBuf := nil;
  try
    if FieldDesc.ParentField = nil then begin
      FieldBuf := PtrOffset(RecBuf1, FieldDesc.Offset);
      IsBlank1 := GetNull(FieldDesc.FieldNo, RecBuf1);
    end
    else begin
    // support objects
      FieldBufStatic := Marshal.AllocHGlobal(4001);
      FieldBuf := FieldBufStatic;
      GetField(FieldDesc.FieldNo, RecBuf1, FieldBuf, IsBlank1);  // GetChildField
    end;

    IsBlank2 := GetNull(FieldDesc.FieldNo, RecBuf2);
    if IsBlank1 and not IsBlank2 then
      Result := -1
    else
    if not IsBlank1 and IsBlank2 then
      Result := 1
    else
    if IsBlank1 and IsBlank2 then
      Result := 0
    else begin
      DataType := FieldDesc.DataType;
      Result := CompareFieldValue(FieldBuf, DataType, FieldDesc,
        RecBuf2, Options);
    end;
  finally
    if FieldBufStatic <> nil then
      Marshal.FreeHGlobal(FieldBufStatic);
    if ValueBuf <> nil then
      Marshal.FreeHGlobal(ValueBuf);
  end;
end;

function TOCIRecordSet.GetEOF: boolean;
begin
  if (IntPtr(CurrentItem )= nil) or (IntPtr(CurrentItem.Next) = nil) then begin
    if hExecFetchThread = nil then
    {$IFDEF CLR}
      hEvent.WaitFor(INFINITE)
    {$ENDIF}
    {$IFDEF WIN32_64}
      WaitForSingleObject(THandle(hEvent.Handle), INFINITE)
    {$ENDIF}
    else begin
      Result := True;
      Exit;
    end;
  end;

  Result := inherited GetEOF;
end;

procedure TOCIRecordSet.SortItems;
begin
  // SortItems can be called during the command is executing or fetching
  if not GetNonBlocking or (FCommand.GetCursorState = csFetched) or
    ((FCommand.GetCursorState = csInactive) and not FCommand.Executing) // for AutoClose
  then
    inherited;
end;

procedure TOCIRecordSet.SetFilterText(Value: _string);
begin
  if not GetNonBlocking or (FCommand.GetCursorState = csFetched) or
    ((FCommand.GetCursorState = csInactive) and not FCommand.Executing) // for AutoClose
  then
    inherited
  else
    FTempFilterText := Value;
end;

procedure TOCIRecordSet.FilterUpdated;
begin
  // FilterUpdated can be called during the command is executing or fetching
  if not GetNonBlocking or (FCommand.GetCursorState = csFetched) or
    ((FCommand.GetCursorState = csInactive) and not FCommand.Executing) // for AutoClose
  then
    inherited;
end;

{ Fetch }

procedure TOCIRecordSet.AllocFetchBlock;
var
  i: integer;
  RefFieldCount : integer;
  FieldDesc: TFieldDesc;
begin
  if FFetchBlock <> nil then
    FreeFetchBlock;
  RefFieldCount := 0;
  FFetchBlockItemSize := 0;
  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if not FieldDesc.HasParent and (IsComplexFieldType(FieldDesc.DataType) or (IsConvertedFieldType(FieldDesc.DataType)))  then
      if (FieldDesc.DataType = dtExtString) or (FieldDesc.DataType = dtMemo) and
        ((FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtNString))
      then // String as memo
        Inc(FFetchBlockItemSize, FieldDesc.Length + 1) // for terminator
      else
      if (FieldDesc.DataType = dtExtWideString) or (FieldDesc.DataType = dtWideMemo) and
        ((FieldDesc.SubDataType = dtWideString) or (FieldDesc.SubDataType = dtNWideString))
      then // WideString
        Inc(FFetchBlockItemSize, FieldDesc.Length * 2 + 2) // for terminator
      else
        case FieldDesc.DataType of
          dtExtVarBytes:
            Inc(FFetchBlockItemSize, FieldDesc.Length + SizeOf(Word));
          dtNumber, dtLargeint, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
            Inc(FFetchBlockItemSize, OCI_NUMBER_SIZE);
          dtXML:
            Inc(FFetchBlockItemSize, 2 * sizeof(IntPtr));
          dtReference: begin
            Inc(FFetchBlockItemSize, sizeof(IntPtr));
            Inc(RefFieldCount);
          end;
        else
          Inc(FFetchBlockItemSize, sizeof(IntPtr));
        end;
  end;
  if FFetchBlockItemSize > 0 then
    FFetchBlock := Marshal.AllocHGlobal(FFetchBlockItemSize*FFetchRows + RefFieldCount * FFetchRows * sizeof(sb2));
end;

procedure TOCIRecordSet.FreeFetchBlock;
begin
  if FFetchBlock <> nil then
    Marshal.FreeHGlobal(FFetchBlock);
  FFetchBlock := nil;
  FFetchBlockItemSize := 0;
end;

function TOCIRecordSet.CallbackDefine(Define: pOCIDefine; Iter: cardinal; var Buf: IntPtr;
  var BufLen: pub4; var PieceStatus: ub1; var Ind: IntPtr): sb4;
var
  i: word;
  Blob: TBlob;
  Piece: PPieceHeader;
  Len: cardinal;
begin
  i := 0;
  while (i < FieldCount) and (Fields[i].Handle <> Define) do
    Inc(i);

  Assert(i < FieldCount);

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(FFetchItems, 
    (RecordSize + sizeof(TItemHeader)) * Integer(Iter) + sizeof(TItemHeader) + Fields[i].Offset)));

  Len := Blob.PieceSize;
  Blob.AllocPiece(Piece, Len);
  Blob.AppendPiece(Piece);

  Buf := PieceData(Piece);
  Piece.Used := Len;
  BufLen := PieceUsedPtr(Piece);

  Ind := PtrOffset(FFetchItems,
    (RecordSize + sizeof(TItemHeader)) * Integer(Iter) + sizeof(TItemHeader) + DataSize + i * sizeof(sb2));

  if (OCIVersion >= 8160) and (OCIVersion < 9000) then begin
  // WAR fix Oracle bug that increments IntPtr to indicator; occurs on 8.1.6,8.1.7
    Ind := PtrOffset(Ind, - Integer(Iter) * sizeof(sb2));
  end;

  PieceStatus := OCI_NEXT_PIECE;

  Result := OCI_CONTINUE;
end;

function OCICallbackDefine(octxp: IntPtr; defnp: pOCIDefine; iter: ub4; var bufp: IntPtr;
  var alenp: pub4; var piece: ub1; var indp: IntPtr; var rcodep: pub2): sb4; {$IFNDEF CLR} cdecl; {$ENDIF}
begin
  Result := TOCIRecordSet(GetGCHandleTarget(octxp)).CallbackDefine(defnp, iter, bufp,
    alenp, piece, indp);
  rcodep := nil;
end;

function TOCIRecordSet.Fetch(FetchBack: boolean = False): boolean;
var
  Cancel: boolean;
  OldCommandType: TCommandType;
{$IFDEF MSWINDOWS}
  InThread: boolean;
{$ENDIF}
begin
  Result := False;

  if not (FCommand.GetCursorState in [csFetched, csInactive]) then begin
  {$IFDEF CLR}
    InThread := (hFetchAllThread <> nil) and (hFetchAllThread.Handle = Thread.CurrentThread)
      or (hExecFetchThread <> nil) and (hExecFetchThread.Handle = Thread.CurrentThread);
  {$ENDIF}
  {$IFDEF WIN32_64}
    InThread := (hFetchAllThread <> nil) and (hFetchAllThread.ThreadID = GetCurrentThreadId)
      or (hExecFetchThread <> nil) and (hExecFetchThread.ThreadID = GetCurrentThreadId);
  {$ENDIF}

    Cancel := False;
    if Assigned(OnBeforeFetch) then begin
      OnBeforeFetch(Cancel);
      if Cancel then begin
        if FAutoClose then
          FCommand.InternalCancel
        else
          // reset cursor state for FetchAll
          if (FCommand.GetCursorState = csFetchingAll) then
            FCommand.SetCursorState(csFetching);
      end;
    end;

    if not Cancel then
      try
        try
          if FPieceFetch or {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCILite then  // Lite don't supports array fetch
            Result := FetchPiece(FetchBack)
          else
            Result := FetchArray(FetchBack);
        except
          FEOF := True;
          raise;
        end;
      finally
        if Assigned(OnAfterFetch) then
        {$IFDEF MSWINDOWS}
          if InThread then begin
            if not FFetchAll or (FCommand.GetCursorState = csFetched) then
              PostMessage(hODACWindow, WM_AFTERFETCH, Integer(GCHandle), 0);
          end
          else
        {$ENDIF}
            OnAfterFetch();
      end;
  end;

  if (FCommand.GetCursorState = csFetched) and FAutoClose then begin
    OldCommandType := CommandType;
    InternalUnPrepare;
    // We need to save old CommandType to save old FieldDescs on Refresh.
    // This is need for SQL Generator (old FieldDescs have TableInfo field set)
    CommandType := OldCommandType;
    Prepared := False;
  end;
end;

procedure TOCIRecordSet.FreeAllItems;
var
  Item: PItemHeader;
begin
  while IntPtr(FirstItem) <> nil do begin
    Item := FirstItem;
    FirstItem := Item.Next;
    if Item.Flag = flUsed then begin
      if FPieceFetch then
        BlockMan.FreeItem(Item);
    end;
  end;
  LastItem := nil;
end;

procedure TOCIRecordSet.InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean);
begin
  if Filtered and not (FFetchAll or (GetNonBlocking and (IndexFields.Count > 0))) then begin
    if FetchBack then
      InitFetchedItems(PtrOffset(Block, sizeof(TBlockHeader) + (RowsObtained - 1) * (sizeof(TItemHeader) + RecordSize)), FNoData, FetchBack)
    else
      InitFetchedItems(PtrOffset(Block, sizeof(TBlockHeader)), FNoData, FetchBack);
  end
  else
    if not (FetchBack or FNoData) then
      Inc(FRecordCount, RowsObtained);

{$IFDEF MSWINDOWS}
  if GetNonBlocking and FFetchAll then // WAR
    PulseEvent(THandle(hEvent.Handle));
{$ENDIF}
end;

function TOCIRecordSet.FetchArray(FetchBack: boolean = False): boolean;
var
  i,j: integer;
  pRec, pInd: IntPtr;
  Fetched: word;
  Block: PBlockHeader;
  TempBlock: PBlockHeader;
  Item: IntPtr;
  ItemHeader: PItemHeader {$IFNDEF CLR}absolute Item{$ENDIF};
  ItemSize: integer;
  NewBlock: boolean;
  BufferPtr: IntPtr;
  ComplexFieldOffset: integer;
  RefFieldCount: integer;
  BufSkip: integer;
  IndSkip: integer;
  ObjPtr: IntPtr;
  OraFile: TOraFile;
  OraRef: TOraRef;
  IndPtr: IntPtr;
  OldFirstItem: PItemHeader;
  OldLastItem: PItemHeader;
  OldFetchStart: integer;
  OldFetchEnd: integer;
  CharsetID: word;
  FieldDesc: TFieldDesc;
  SharedPiece: PPieceHeader;

  procedure InitBlock;
  var
    i,j: integer;
    Ptr: IntPtr;
    ObjPtr: IntPtr;
    FieldPtr: IntPtr;
    FieldDesc: TFieldDesc;
  {$IFDEF VER6P}
    OraTimeStamp: TOraTimeStamp;
  {$ENDIF}
  begin
  // Create complex filds
    ComplexFieldOffset := 0;
    if not (HasComplexFields or FHasConvertedFields) then
      Exit;
    for i := 0 to FFetchRows - 1 do begin
      Ptr := PtrOffset(Block, sizeof(TBlockHeader) + i * ItemSize + sizeof(TItemHeader));
      CreateComplexFields(Ptr, True);
      for j := 0 to FieldCount - 1 do begin
        FieldDesc := Fields[j];
        if not FieldDesc.HasParent and (IsComplexFieldType(FieldDesc.DataType) or IsConvertedFieldType(FieldDesc.DataType)) then begin
          ObjPtr := Marshal.ReadIntPtr(Ptr, FieldDesc.Offset);
          FieldPtr := PtrOffset(FFetchBlock, ComplexFieldOffset);
          case FieldDesc.DataType of
            dtCursor: begin
              TOraCursor(GetGCHandleTarget(ObjPtr)).State := csExecuted;
              Marshal.WriteIntPtr(FieldPtr, TOraCursor(GetGCHandleTarget(ObjPtr)).OCIStmt);
            end;
            dtOraBlob,dtOraClob,dtWideOraClob,dtBFile,dtCFile:
              Marshal.WriteIntPtr(FieldPtr, TOraLob(GetGCHandleTarget(ObjPtr)).OCILobLocator);
            dtObject,dtArray,dtTable:
              Marshal.WriteIntPtr(FieldPtr, TOraObject(GetGCHandleTarget(ObjPtr)).Instance);
            dtXML:begin
              Marshal.WriteIntPtr(FieldPtr, TOraXML(GetGCHandleTarget(ObjPtr)).Instance);
              Marshal.WriteIntPtr(FieldPtr, sizeof(IntPtr), nil);
            end;
            dtReference:
              Marshal.WriteIntPtr(FieldPtr, TOraRef(GetGCHandleTarget(ObjPtr)).OCIRef);
            dtMemo:
              if (FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtNString) then // String as memo
                Marshal.WriteByte(FieldPtr, 0);
            dtWideMemo:
              if (FieldDesc.SubDataType = dtWideString) or (FieldDesc.SubDataType = dtNWideString) then // WideString as memo
                Marshal.WriteInt16(FieldPtr, 0);
            dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
              Marshal.WriteIntPtr(FieldPtr, TOraTimeStamp(GetGCHandleTarget(ObjPtr)).OCIDateTime);
          {$IFDEF VER6P}
          {$IFNDEF FPC}
            dtSQLTimeStamp: begin
              OraTimeStamp := TOraTimeStamp.Create(FieldDesc.DataType);
              OraTimeStamp.Precision := FieldDesc.Length;
              Marshal.WriteIntPtr(Ptr, FieldDesc.Offset, OraTimeStamp.GCHandle);
              Marshal.WriteIntPtr(FieldPtr, OraTimeStamp.OCIDateTime);
            end;
          {$ENDIF}
          {$ENDIF}
            dtIntervalYM, dtIntervalDS:
              Marshal.WriteIntPtr(FieldPtr, TOraInterval(GetGCHandleTarget(ObjPtr)).OCIInterval);
            dtExtString:
              Marshal.WriteIntPtr(FieldPtr, nil);
            dtExtWideString:
              Marshal.WriteIntPtr(FieldPtr, nil);
            dtExtVarBytes:
              Marshal.WriteIntPtr(FieldPtr, nil);
          end;
          if (FieldDesc.DataType = dtExtString) or (FieldDesc.DataType = dtMemo) and
             ((FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtNString))
          then // String as memo
            Inc(ComplexFieldOffset, FieldDesc.Length + 1) // for terminator
          else
          if (FieldDesc.DataType = dtExtWideString) or (FieldDesc.DataType = dtWideMemo) and
             ((FieldDesc.SubDataType = dtWideString) or (FieldDesc.SubDataType = dtNWideString))
          then // String as memo
            Inc(ComplexFieldOffset, FieldDesc.Length * 2 + 2) // for terminator
          else
            case FieldDesc.DataType of
              dtExtVarBytes:
                Inc(ComplexFieldOffset, FieldDesc.Length + SizeOf(Word));
              dtNumber, dtLargeint, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
                Inc(ComplexFieldOffset, OCI_NUMBER_SIZE);
              dtXML:
                Inc(ComplexFieldOffset, 2 * sizeof(IntPtr));
            else
              Inc(ComplexFieldOffset, sizeof(IntPtr));
            end;
        end;
      end;
    end;
  end;

  procedure PrepareBlock(Fetched: integer);
  var
    i,j: integer;
    Ptr: IntPtr;
    ObjPtr: IntPtr;
    Piece: PPieceHeader;
    Len: integer;
    Source: IntPtr;
    Dest: IntPtr;
    FieldDesc: TFieldDesc;
    OraNumber: TOraNumber;
    Curr: currency;
    i64: int64;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    BcdValue: TBCD;
    OraTS: TOraTimeStamp;
    SQLTS: TSQLTimeStamp;
  {$IFDEF CLR}
    Temp: TBytes;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  begin
    ComplexFieldOffset := 0;
    if not (HasComplexFields or FHasConvertedFields) then
      Exit;

    if FHasConvertedFields then
      OraNumber := TOraNumber.Create
    else
      OraNumber := nil;
    try
      for i := 0 to Fetched - 1 do begin
        RefFieldCount := 0;
        Ptr := PtrOffset(Block, sizeof(TBlockHeader) + i*ItemSize + sizeof(TItemHeader));
        for j := 0 to FieldCount - 1 do begin
          FieldDesc := Fields[j];
          if not FieldDesc.HasParent and (IsComplexFieldType(FieldDesc.DataType) or IsConvertedFieldType(FieldDesc.DataType)) then begin
            ObjPtr := Marshal.ReadIntPtr(Ptr, FieldDesc.Offset);
            Source := PtrOffset(FFetchBlock, ComplexFieldOffset);
            case FieldDesc.DataType of
              dtReference: begin
                with TOraRef(GetGCHandleTarget(ObjPtr)) do begin
                  Indicator := PtrOffset(Ptr, DataSize + j * SizeOf(SB2));
                  Marshal.WriteInt16(Indicator, Marshal.ReadInt16(FFetchBlock, FFetchRows * FFetchBlockItemSize +
                                                      RefFieldCount * FFetchRows * SizeOf(SB2)), i * SizeOf(SB2));
                  OCIRef := Marshal.ReadIntPtr(IntPtr(Source));
                end;
                Inc(RefFieldCount)
              end;
              dtMemo:
                if (FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtNString) then begin // String as memo
                  Len := StrLen(PAChar(PtrOffset(FFetchBlock, ComplexFieldOffset)));
                  if Len > 0 then begin
                    TBlob(GetGCHandleTarget(ObjPtr)).AllocPiece(Piece, Len); // for term
                    TBlob(GetGCHandleTarget(ObjPtr)).AppendPiece(Piece);
                    CopyBuffer(Source, PtrOffset(TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece, sizeof(TPieceHeader)), Len);
                    TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece.Used := Len;
                  end;
                end;
              dtWideMemo:
                if (FieldDesc.SubDataType = dtWideString) or (FieldDesc.SubDataType = dtNWideString) then begin // String as memo
                  Len := StrLenW(PWChar(PtrOffset(FFetchBlock, ComplexFieldOffset))) * sizeof(WideChar);
                  if Len > 0 then begin
                    TBlob(GetGCHandleTarget(ObjPtr)).AllocPiece(Piece, Len); // for term
                    TBlob(GetGCHandleTarget(ObjPtr)).AppendPiece(Piece);
                    CopyBuffer(Source, PtrOffset(TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece, sizeof(TPieceHeader)), Len);
                    TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece.Used := Len;
                  end;
                end;
              dtExtString:
                Marshal.WriteIntPtr(Ptr, FieldDesc.Offset,
                  StringHeap.AllocStr(Source, FieldDesc.Fixed and TrimFixedChar));
              dtExtWideString:
                Marshal.WriteIntPtr(Ptr, FieldDesc.Offset,
                  StringHeap.AllocWideStr(Source, FieldDesc.Fixed and TrimFixedChar));
              dtExtVarBytes: begin
                Dest := StringHeap.NewBuf(Marshal.ReadInt16(Source) + SizeOf(Word));
                Marshal.WriteIntPtr(Ptr, FieldDesc.Offset, Dest);
                CopyBuffer(Source, Dest, Marshal.ReadInt16(Source) + SizeOf(Word));
              end;
              dtNumber:
                CopyBuffer(Source, TOraNumber(GetGCHandleTarget(ObjPtr)).OCINumberPtr, OCI_NUMBER_SIZE);
              dtLargeint: begin
                OraNumber.OCINumberPtr := Source;
                Marshal.WriteInt16(OraNumber.FIndicator, Marshal.ReadInt16(Ptr, DataSize + j * sizeof(sb2)));
                Marshal.WriteInt64(Ptr, FieldDesc.Offset, OraNumber.AsLargeInt);
              end;
              dtBCD: begin
                OraNumber.OCINumberPtr := Source;
                Marshal.WriteInt16(OraNumber.FIndicator, Marshal.ReadInt16(Ptr, DataSize + j * sizeof(sb2)));
                if OraNumber.IsNull then
                  Curr := 0
                else
                  Curr := StrToCurr(OraNumber.AsString);
              {$IFDEF CLR}
                i64 := Decimal.ToOACurrency(Curr);
              {$ELSE}
                i64 := PInt64(@Curr)^;
              {$ENDIF}
                Marshal.WriteInt64(Ptr, FieldDesc.Offset, i64);
              end;
            {$IFDEF VER6P}
            {$IFNDEF FPC}
              dtFMTBCD: begin
                OraNumber.OCINumberPtr := Source;
                Marshal.WriteInt16(OraNumber.FIndicator, Marshal.ReadInt16(Ptr, DataSize + j * sizeof(sb2)));
                BcdValue := OraNumber.AsBCD;
              {$IFDEF CLR}
                Temp := TBcd.ToBytes(BcdValue);
                Marshal.Copy(Temp, 0, PtrOffset(Ptr, FieldDesc.Offset), SizeOfTBcd);
              {$ELSE}
                PBcd(PtrOffset(Ptr, FieldDesc.Offset))^ := BcdValue;
              {$ENDIF}
              end;
              dtSQLTimeStamp: begin
                OraTS := TOraTimeStamp(GetGCHandleTarget(ObjPtr));
                Marshal.WriteInt16(OraTS.FIndicator, Marshal.ReadInt16(Ptr, DataSize + j * sizeof(sb2)));
                OraTimeStampToSQLTimeStamp(OraTS, SQLTS);
                OraTS.Free;
              {$IFDEF CLR}
                Temp := TSQLTimeStamp.ToBytes(SQLTS);
                Marshal.Copy(Temp, 0, PtrOffset(Ptr, FieldDesc.Offset), SizeOf(TSQLTimeStamp));
              {$ELSE}
                PSQLTimeStamp(PtrOffset(Ptr, FieldDesc.Offset))^ := SQLTS;
              {$ENDIF}
              end;
            {$ENDIF}
            {$ENDIF}
              dtObject:
                TOraObject(GetGCHandleTarget(ObjPtr)).Instance := Marshal.ReadIntPtr(Source);
              dtXML: begin
                TOraXML(GetGCHandleTarget(ObjPtr)).Instance := Marshal.ReadIntPtr(Source);
                TOraXML(GetGCHandleTarget(ObjPtr)).Indicator := Marshal.ReadIntPtr(Source, sizeof(IntPtr));
                TOraXML(GetGCHandleTarget(ObjPtr)).ReadXML;
              end;
            end;
            if (FieldDesc.DataType = dtExtString) or (FieldDesc.DataType = dtMemo) and
               ((FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtNString))
            then // String as memo
              Inc(ComplexFieldOffset, FieldDesc.Length + 1) // for terminator
            else
            if (FieldDesc.DataType = dtExtWideString) or (FieldDesc.DataType = dtWideMemo) and
               ((FieldDesc.SubDataType = dtWideString) or (FieldDesc.SubDataType = dtNWideString))
            then
              Inc(ComplexFieldOffset, FieldDesc.Length *2 + 2) // for terminator
            else
              case FieldDesc.DataType of
                dtExtVarBytes:
                  Inc(ComplexFieldOffset, FieldDesc.Length + SizeOf(Word));
                dtNumber, dtLargeint, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
                  Inc(ComplexFieldOffset, OCI_NUMBER_SIZE);
                dtXML:
                  Inc(ComplexFieldOffset, 2 * sizeof(IntPtr));
              else
                Inc(ComplexFieldOffset, sizeof(IntPtr));
              end;
          end;
        end;
      end;
    finally
      OraNumber.Free;
    end;
  end;

  procedure ClearBlockUnused;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
  var
    i, j: integer;
    Ptr, ObjPtr: IntPtr;
    FieldDesc: TFieldDesc;
    Obj: TSharedObject;
  {$ENDIF}
  {$ENDIF}
  begin
    // Free OraTimeStamps
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    if FHasConvertedFields then
      for i := Fetched to FFetchRows - 1 do begin
        Ptr := PtrOffset(Block, sizeof(TBlockHeader) + i * ItemSize + sizeof(TItemHeader));
        for j := 0 to FieldCount - 1 do begin
          FieldDesc := Fields[j];
          if not FieldDesc.HasParent then
            case FieldDesc.DataType of
              dtSQLTimeStamp: begin
                ObjPtr := Marshal.ReadIntPtr(Ptr, FieldDesc.Offset);
                Obj := TOraTimeStamp(GetGCHandleTarget(ObjPtr));
                Obj.Free;
              end;
            end;
        end;
      end;
  {$ENDIF}
  {$ENDIF}
  end;

  procedure ClearBlock;
  var
    i: integer;
  begin
  // Free complex filds
    if not HasComplexFields then
      Exit;
    for i := 0 to FFetchRows - 1 do
      FreeComplexFields(PtrOffset(Block, sizeof(TBlockHeader) + i*ItemSize + sizeof(TItemHeader)), True);
  end;

  procedure SwapBlocks(Block1, Block2: PBlockHeader);
  begin
    Block1.Prev := nil;
    Block1.Next := Block2;
    Block2.Next := nil;
    Block2.Prev := Block1;
  end;

begin
  Result := False;

  if FetchBack and (FFetchStart = 0) and not FEOF or not FetchBack and
    (FFetchEnd = FCommand.FFetchedRows) and FNoData and not FBOF
  then
    Exit;

  FCommand.Busy; // Ok !!!
  // for close in time fetch all
  if FCommand.GetCursorState < csBound then begin // For Non Blocking
    FCommand.Release;
    Exit;
  end;

  try // For Busy
    ItemSize := RecordSize + sizeof(TItemHeader);
    NewBlock := (IntPtr(BlockMan.FirstBlock) = nil) or not (FUniDirectional or CanFetchBack and
      ((IntPtr(BlockMan.FirstBlock.Next) <> nil) or FBOF or FEOF));
    if NewBlock then begin
      BlockMan.AllocBlock(Block, FFetchRows);
      if FetchBack then begin
        SwapBlocks(Block.Next, Block);
        BlockMan.FirstBlock := Block.Prev;
      end;

      if HasComplexFields or FHasConvertedFields then begin
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
          if FetchBack then begin
            // if more then one block used
            if not ((IntPtr(FirstItem) <> nil) and (IntPtr(LastItem) <> nil) and (FirstItem.Block = LastItem.Block)) then begin
              SwapBlocks(Block, TempBlock);
              BlockMan.FirstBlock := Block;
              if IntPtr(LastItem) <> nil then begin
                LastItem := PtrOffset(Block, sizeof(TBlockHeader) + (Block.UsedItems - 1) * ItemSize);
                Dec(FFetchEnd, TempBlock.UsedItems);
              end;
              Block := TempBlock;
            end;
          end
          else begin
            SwapBlocks(Block, TempBlock);
            BlockMan.FirstBlock := Block;
            if (IntPtr(FirstItem) <> nil) and ((IntPtr(LastItem) = nil) or (IntPtr(FirstItem.Block) <> IntPtr(LastItem.Block))) then begin
              FirstItem := PtrOffset(TempBlock, sizeof(TBlockHeader));
              Inc(FFetchStart, Block.UsedItems);
            end;
          end;
        end
      end
      else
        Block := BlockMan.FirstBlock;
    // Refresh block: drop values of blobs
      ClearBlock;
      InitBlock;
    end;

    if CanFetchBack then begin
      if FBOF then begin
        FFetchStart := FFetchEnd;
        LastItem := nil;
      end;
      if FEOF then begin
        FFetchEnd := FFetchStart;
        FirstItem := nil;
      end;
    end;

    // remeber first item and last item on case of exception
    OldFirstItem := FirstItem;
    OldLastItem := LastItem;
    OldFetchStart := FFetchStart;
    OldFetchEnd := FFetchEnd;
    try     // For free memory
      if NewBlock then begin
        pRec := PtrOffset(Block, sizeof(TBlockHeader) + sizeof(TItemHeader));
        pInd := PtrOffset(pRec, DataSize);

        FConnection.BusyWait;
        try  // For FConnection.Busy
        { DefineData }
          ComplexFieldOffset := 0;
          RefFieldCount := 0;
          for i := 0 to FieldCount - 1 do begin
            FieldDesc := Fields[i];
            if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then
              if (FieldDesc.DataType in [dtBlob,dtMemo,dtWideMemo]) and
                ((FieldDesc.SubDataType <> dtString) and (FieldDesc.SubDataType <> dtWideString) and
                (FieldDesc.SubDataType <> dtNString) and (FieldDesc.SubDataType <> dtNWideString))
              then begin
                ObjPtr := Marshal.ReadIntPtr(pRec, FieldDesc.Offset);
                if TBlob(GetGCHandleTarget(ObjPtr)).IsUnicode then
                  CharsetId := OCI_UTF16ID
                else
                  CharsetId := FConnection.FCharsetId;
              {$IFDEF CLR}
                if PossibleOCICallStyles = [OCI80] then
                  FCommand.DefineDynamic(FieldDesc, GCHandle, IntPtr(HOCICallbackDefine), CharsetId)
                else
              {$ENDIF}
                  FCommand.DefineDynamic(FieldDesc, GCHandle, OCICallbackDefinePtr, CharsetId);
                Inc(ComplexFieldOffset, sizeof(IntPtr));
              end
              else begin
                if IsComplexFieldType(FieldDesc.DataType) or IsConvertedFieldType(FieldDesc.DataType) then begin
                  BufferPtr := PtrOffset(FFetchBlock, ComplexFieldOffset);
                  BufSkip := FFetchBlockItemSize;
                  if (FieldDesc.DataType = dtExtString) or (FieldDesc.DataType = dtMemo) and
                     ((FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtNString))
                  then // String as memo
                    Inc(ComplexFieldOffset, FieldDesc.Length + 1) // for termimator
                  else
                  if (FieldDesc.DataType = dtExtWideString) or (FieldDesc.DataType = dtWideMemo) and
                     ((FieldDesc.SubDataType = dtWideString) or (FieldDesc.SubDataType = dtNWideString))
                  then
                    Inc(ComplexFieldOffset, FieldDesc.Length * 2 + 2) // for termimator
                  else
                    case FieldDesc.DataType of
                      dtExtVarBytes:
                        Inc(ComplexFieldOffset, FieldDesc.Length + SizeOf(Word));
                      dtNumber, dtLargeint, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
                        Inc(ComplexFieldOffset, OCI_NUMBER_SIZE);
                      dtXML:
                        Inc(ComplexFieldOffset, 2 * sizeof(IntPtr));
                    else
                      Inc(ComplexFieldOffset, sizeof(IntPtr));
                    end;
                end
                else begin
                  BufferPtr := PtrOffset(pRec, FieldDesc.Offset);
                  BufSkip := ItemSize;
                end;

                if FieldDesc.DataType in [dtObject,dtArray,dtTable] then begin
                  Marshal.WriteInt16(pInd, i * sizeof(sb2), 0);
                  IndPtr := nil; // WAR Oracle bug - indicator skip is ignored for Objects
                  IndSkip := ItemSize;
                end
                else
                if FieldDesc.DataType = dtReference then begin
                  IndPtr := PtrOffset(FFetchBlock, FFetchRows * FFetchBlockItemSize + RefFieldCount * FFetchRows * SizeOf(SB2));
                  IndSkip := SizeOf(SB2); // WAR Oracle bug - indicator skip is ignored for REFs
                  inc(RefFieldCount);
                end
                else
                if FieldDesc.DataType = dtXML then begin
                  IndPtr := PtrOffset(BufferPtr, sizeof(IntPtr));
                  IndSkip := FFetchBlockItemSize;
                end
                else begin
                  IndPtr := PtrOffset(pInd, i * sizeof(sb2));
                  IndSkip := ItemSize;
                end;

                FCommand.DefineArrayData(FieldDesc, BufferPtr,
                  IndPtr, BufSkip, IndSkip);
              end;
          end;
        finally
          FConnection.Release;
        end;
      end;

      FFetchItems := PtrOffset(Block, sizeof(TBlockHeader));

      if FetchBack then begin
        if FFetchStart < FFetchRows then
          Fetched := FFetchStart
        else
          Fetched := FFetchRows;
        Fetched := FCommand.InternalFetch(Fetched, OCI_FETCH_ABSOLUTE, FFetchStart - Fetched + 1);
        Dec(FFetchStart, Fetched);
      end
      else begin
        if FNoData and (FCommand.FFetchedRows > FFetchEnd) then begin
          Fetched := FCommand.FFetchedRows - FFetchEnd;
          if Fetched > FFetchRows then
            Fetched := FFetchRows;
        end
        else
          Fetched := FFetchRows;
        if FFetchAbsolute then
          Fetched := FCommand.InternalFetch(Fetched, OCI_FETCH_ABSOLUTE, FFetchEnd + 1)
        else
          Fetched := FCommand.InternalFetch(Fetched);
        Inc(FFetchEnd, Fetched);
      end;

      FFetchAbsolute := FetchBack;

      Result := Fetched > 0;
      if Result then begin
        if HasFields([dtMemo,dtWideMemo,dtReference,dtExtString,dtExtWideString,
          dtExtVarBytes, dtNumber, dtLargeint, dtBCD, dtXML, dtObject
          {$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD, dtSQLTimeStamp{$ENDIF}{$ENDIF}])
        then
          PrepareBlock(Fetched);

        // Prepare comlex field
        Item := PtrOffset(Block, sizeof(TBlockHeader));
      {$IFDEF CLR}
        ItemHeader := Item;
      {$ENDIF}
        SharedPiece := nil;
        for i := 1 to Fetched do begin
        // Prepare comlex field
          if HasComplexFields then
            for j := 0 to FieldCount - 1 do begin
              FieldDesc := Fields[j];
              if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then begin
                ObjPtr := Marshal.ReadIntPtr(Item, sizeof(TItemHeader) + FieldDesc.Offset);
                case FieldDesc.DataType of
                  dtBlob,dtMemo,dtWideMemo:
                    if (FieldDesc.SubDataType <> dtString) and (FieldDesc.SubDataType <> dtWideString) and
                       (FieldDesc.SubDataType <> dtNString) and (FieldDesc.SubDataType <> dtNWideString) then
                      TBlob(GetGCHandleTarget(ObjPtr)).Compress;
                  dtOraBlob,dtOraClob,dtWideOraClob: begin
                    TOraLob(GetGCHandleTarget(ObjPtr)).FNeedReadLob := True;
                    if not FDeferredLobRead and FCommand.FCacheLobs then begin
                      if IntPtr(SharedPiece) = nil then
                        TBlob.AllocPiece(SharedPiece, DefaultPieceSize);
                      TOraLob(GetGCHandleTarget(ObjPtr)).ReadLob(SharedPiece);
                    {$IFDEF HAVE_COMPRESS}
                      if (FieldDesc.DataType = dtOraBlob) and
                        (FCommand.FCompressBlob in [cbClient, cbClientServer])
                      then
                        TOraLob(GetGCHandleTarget(ObjPtr)).Compressed := True
                      else
                        TOraLob(GetGCHandleTarget(ObjPtr)).Compressed := False;
                    {$ENDIF}
                    end;
                    // sync indicator ???
                    //if TOraLob(GetGCHandleTarget(ObjPtr)).Size = 0 then
                    //  psb2(IntPtr(PChar(Item) + sizeof(TItemHeader) + DataSize + j * sizeof(sb2)))^ := -1;
                  end;
                  dtBFile,dtCFile: begin
                    TOraFile(GetGCHandleTarget(ObjPtr)).FNeedReadLob := True;
                    if not FDeferredLobRead and FCommand.FCacheLobs then begin
                      OraFile := TOraFile(GetGCHandleTarget(ObjPtr));
                      if OraFile.Exists then begin
                        OraFile.Open;
                        try
                          OraFile.ReadLob;
                        finally
                          OraFile.Close;
                        end;
                      end;
                    end;
                  end;
                  dtObject: begin
                    if FConnection.FDisconnectMode then
                      TOraObject(GetGCHandleTarget(ObjPtr)).CacheObject;
                  end;
                  dtArray: begin
                    if FConnection.FDisconnectMode then
                      TOraArray(GetGCHandleTarget(ObjPtr)).CacheObject;
                  end;
                  dtTable: begin
                    if FConnection.FDisconnectMode then
                      TOraNestTable(GetGCHandleTarget(ObjPtr)).CacheObject;
                  end;
                  dtXML: begin
                    if FConnection.FDisconnectMode then
                      TOraXML(GetGCHandleTarget(ObjPtr)).CacheObject;
                  end;
                  dtReference: begin
                    OraRef := TOraRef(GetGCHandleTarget(ObjPtr));
                    if not OraRef.IsNull then
                      OraRef.Pin;
                    if FConnection.FDisconnectMode then
                      TOraRef(GetGCHandleTarget(ObjPtr)).CacheObject;
                  end;
                  dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
                    Marshal.WriteInt16(
                      TOraTimeStamp(GetGCHandleTarget(ObjPtr)).FIndicator,
                      Marshal.ReadInt16(Item, sizeof(TItemHeader) + DataSize + j * sizeof(sb2))
                    );
                  dtIntervalYM, dtIntervalDS:
                    Marshal.WriteInt16(
                      TOraInterval(GetGCHandleTarget(ObjPtr)).FIndicator,
                      Marshal.ReadInt16(Item, sizeof(TItemHeader) + DataSize + j * sizeof(sb2))
                    );
                  dtNumber:
                    Marshal.WriteInt16(
                      TOraNumber(GetGCHandleTarget(ObjPtr)).FIndicator,
                      Marshal.ReadInt16(Item, sizeof(TItemHeader) + DataSize + j * sizeof(sb2))
                    );
                end;
              end;
            end;
          Item := PtrOffset(Item, ItemSize);
        {$IFDEF CLR}
          ItemHeader := Item;
        {$ENDIF}
        end;
        if IntPtr(SharedPiece) <> nil then
          Marshal.FreeHGlobal(SharedPiece);

        CreateBlockStruct(Block, Fetched, FetchBack);

        if (HasComplexFields or FHasConvertedFields) and (FCommand.GetCursorState in [csFetched, csInactive]) then begin
          FreeFetchBlock;
          ClearBlockUnused;
        end;
      end // if Result then begin
      else begin
        if not CanFetchBack then begin
          if HasComplexFields or FHasConvertedFields then begin
            FreeFetchBlock;
            ClearBlockUnused;
          end;

          if NewBlock then begin
            if HasComplexFields then
              ClearBlock;
            BlockMan.FreeBlock(Block);
          end;
        end else
          FirstItem.Prev := nil;
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
  finally
    FCommand.Release;
  end;
end;

var
  GlobalBuf: IntPtr = nil; // static as odefinps writes data to it

function TOCIRecordSet.FetchPiece(FetchBack: boolean = False): boolean;
var
  i: word;
  pRec, pInd: IntPtr;
  Res: integer;
  Item: PItemHeader;
  PieceStatus: ub1;
  Blob: TBlob;
  Piece: PPieceHeader;
  Ptr: IntPtr;
  ATemp: IntPtr;
  BufLen: ub4;
  Iteration: ub4;
  Index: ub4;
  NeedData: integer;
  NoData: integer;
  hDefine: pOCIHandle;
  BufferPtr: IntPtr;
  OraFile: TOraFile;
  OraRef: TOraRef;
  ObjPtr: IntPtr;
  Mode: TParamDirection;
  IndPtr: IntPtr;
  CharsetId: word;
  FieldDesc: TFieldDesc;
begin
  Result := False;

  FCommand.Busy;
  if FCommand.GetCursorState < csBound then begin   // For Non Blocking
    FCommand.Release;
    Exit;
  end;

  try  // For Busy
    if FUniDirectional and (IntPtr(FirstItem) <> nil) then begin
      Item := FirstItem;
      FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True);

      FirstItem := nil;
      LastItem := nil;
      CurrentItem := nil;
    end
    else begin
      if CanFetchBack and (IntPtr(BlockMan.FirstFree) = nil) and (IntPtr(BlockMan.FirstBlock) <> nil) then begin
        if FetchBack then begin
          if IntPtr(LastItem) <> nil then begin
            Item := LastItem.Prev;
            BlockMan.FreeItem(LastItem);
            LastItem := Item;
            if IntPtr(Item) <> nil then
              Item.Next := nil;
            Dec(FFetchEnd);
          end
        end
        else begin
          if IntPtr(FirstItem) <> nil then begin
            Item := FirstItem.Next;
            BlockMan.FreeItem(FirstItem);
            FirstItem := Item;
            if IntPtr(Item) <> nil then
              Item.Prev := nil;
            Inc(FFetchStart);
          end;
        end;
      end;
      BlockMan.AllocItem(Item);
    end;

    if CanFetchBack then begin
      if FBOF then begin
        FFetchStart := FFetchEnd;
        LastItem := nil;
      end;
      if FEOF then begin
        FFetchEnd := FFetchStart;
        FirstItem := nil;
      end;
    end;

    CreateComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True);

    try     // For free memory
      pRec := PtrOffset(Item, sizeof(TItemHeader));
      pInd := PtrOffset(pRec, DataSize);
      BufLen := MaxBlobSize;
      FConnection.BusyWait;
      try  // For FConnection.Busy
           // Picewise fetch as one operation

      { DefineData } // OPTIM - Once
        for i := 0 to FieldCount - 1 do begin
          FieldDesc := Fields[i];
          if not FieldDesc.HasParent and (FieldDesc.FieldDescKind = fdkData) then begin
            BufferPtr := PtrOffset(pRec, FieldDesc.Offset);

            if FieldDesc.DataType = dtXML then begin
              ObjPtr := Marshal.ReadIntPtr(BufferPtr);
              IndPtr :=TOraObject(GetGCHandleTarget(ObjPtr)).IndicatorPtr;
              Marshal.WriteIntPtr(IndPtr, nil);
            end
            else
              IndPtr := PtrOffset(pInd, i * sizeof(sb2));

            case FieldDesc.DataType of
              dtCursor: begin
                ObjPtr := Marshal.ReadIntPtr(BufferPtr);
                TOraCursor(GetGCHandleTarget(ObjPtr)).State := csExecuted;
                BufferPtr := TOraCursor(GetGCHandleTarget(ObjPtr)).OCIStmtPtr;
              end;
              dtOraBlob,dtOraClob,dtWideOraClob,dtBFile,dtCFile:
                BufferPtr := TOraLob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).OCILobLocatorPtr;
              dtObject,dtXML,dtArray,dtTable:
                BufferPtr := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).InstancePtr;
              dtReference:
                BufferPtr := TOraRef(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).OCIRefPtr;
              dtMemo:
                if (FieldDesc.SubDataType = dtString) or (FieldDesc.SubDataType = dtNString) then begin
                  TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).AllocPiece(Piece, FieldDesc.Length + 1); // for term
                  TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).AppendPiece(Piece);
                  BufferPtr := PtrOffset(Piece, sizeof(TPieceHeader));
                end;
              dtWideMemo:
                if (FieldDesc.SubDataType = dtWideString) or (FieldDesc.SubDataType = dtNWideString) then begin
                  TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).AllocPiece(Piece, FieldDesc.Length * 2 + sizeof(WideChar)); // for term
                  TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).AppendPiece(Piece);
                  BufferPtr := PtrOffset(Piece, sizeof(TPieceHeader));
                end;
              dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
                BufferPtr := TOraTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).OCIDateTimePtr;
              dtIntervalYM, dtIntervalDS:
                BufferPtr := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(BufferPtr))).OCIIntervalPtr;
              dtExtString: begin
                ObjPtr := StringHeap.NewBuf(FieldDesc.Length + 1);
                Marshal.WriteIntPtr(BufferPtr, ObjPtr);
                BufferPtr := ObjPtr;
              end;
              dtExtWideString: begin
                ObjPtr := StringHeap.NewBuf(FieldDesc.Length *2 + 2);
                Marshal.WriteIntPtr(BufferPtr, ObjPtr);
                BufferPtr := ObjPtr;
              end;
              dtExtVarBytes: begin
                ObjPtr := StringHeap.NewBuf(FieldDesc.Length + SizeOf(Word));
                Marshal.WriteIntPtr(BufferPtr, ObjPtr);
                BufferPtr := ObjPtr;
              end;
              dtNumber: begin
                ObjPtr := Marshal.ReadIntPtr(BufferPtr);
                BufferPtr := TOraNumber(GetGCHandleTarget(ObjPtr)).OCINumberPtr;
              end;
            end;

            if (FieldDesc.DataType in [dtBlob,dtMemo,dtWideMemo]) and
              ((FieldDesc.SubDataType <> dtString) and (FieldDesc.SubDataType <> dtWideString) and
              (FieldDesc.SubDataType <> dtNString) and (FieldDesc.SubDataType <> dtNWideString))
            then
              if FCommand.FOCICallStyle = OCI73 then
                FCommand.DefinePieceData(FieldDesc, nil, IndPtr)
              else begin
                ObjPtr := Marshal.ReadIntPtr(BufferPtr);
                if TBlob(GetGCHandleTarget(ObjPtr)).IsUnicode then
                  CharsetId := OCI_UTF16ID
                else
                  CharsetId := FConnection.FCharsetId;
                FCommand.DefineDynamic(FieldDesc, GCHandle, OCICallbackDefinePtr, CharsetId);
              end
            else
              FCommand.DefineData(FieldDesc, BufferPtr, IndPtr);
          end;
        end;

        FFetchItems := Item;

        if FetchBack then
          Res := FCommand.InternalFetchPiece(OCI_FETCH_ABSOLUTE, FFetchStart)
        else begin
          if FFetchAbsolute then
            Res := FCommand.InternalFetchPiece(OCI_FETCH_ABSOLUTE, FFetchEnd + 1)
          else
            Res := FCommand.InternalFetchPiece;
        end;

        FFetchAbsolute := FetchBack;

        if FCommand.FOCICallStyle = OCI73 then begin
          NeedData := OCI_STILL_IS_PIECE;
          NoData := OCI_NO_DATA_FOUND;
        end
        else
        if FCommand.FOCICallStyle = OCI80 then begin
          NeedData := OCI_NEED_DATA;
          NoData := OCI_NO_DATA;
        end
        else begin
          FConnection.CheckOCI;
          NeedData := 0;
          NoData := 0;
        end;

        if Res = NeedData then begin
          if GlobalBuf = nil then
            GlobalBuf := Marshal.AllocHGlobal(sizeof(IntPtr));
          Marshal.WriteIntPtr(GlobalBuf, nil);
          for i := 0 to FieldCount - 1 do begin
            FieldDesc := Fields[i];
            if FieldDesc.FieldDescKind <> fdkData then
              continue;
            if (FieldDesc.DataType = dtBlob) or
              (FieldDesc.DataType = dtMemo) and (FieldDesc.SubDataType <> dtString) and (FieldDesc.SubDataType <> dtNString) or
              (FieldDesc.DataType = dtWideMemo) and (FieldDesc.SubDataType <> dtWideString) and (FieldDesc.SubDataType <> dtNWideString)
            then begin
              if Res <> NeedData then
                Assert(False);            // DEBUG

              FCommand.GetPI(hDefine, PieceStatus, Ptr, Iteration, Index, Mode);

              Blob := nil; // anti warning
              if (PieceStatus = OCI_FIRST_PIECE) or
                 (PieceStatus = OCI_ONE_PIECE)
              then begin
                ObjPtr := Marshal.ReadIntPtr(Item, sizeof(TItemHeader) + FieldDesc.Offset);
                Blob := TBlob(GetGCHandleTarget(ObjPtr));
              end
              else
                Assert(False); // DEBUG

              repeat
                BufLen := Blob.PieceSize;
                Blob.AllocPiece(Piece, BufLen);

                FCommand.SetPI(hDefine, OCI_HTYPE_DEFINE, PieceStatus, PtrOffset(Piece, Sizeof(TPieceHeader)),
                  BufLen, PtrOffset(pInd, i * sizeof(sb2)));

                Res := FCommand.InternalFetchPiece;

                if BufLen > 0 then begin
                  Piece.Used := BufLen;
                  if BufLen < Cardinal(Blob.PieceSize) then
                    Blob.ReallocPiece(Piece, BufLen);
                  Blob.AppendPiece(Piece);
                end
                else
                  Blob.FreePiece(Piece);

                FCommand.GetPI(hDefine, PieceStatus, Ptr, Iteration, Index, Mode);

                if PieceStatus = OCI_FIRST_PIECE then begin
                  BufLen := sizeof(integer);
                  FCommand.SetPI(hDefine, OCI_HTYPE_DEFINE, PieceStatus, GlobalBuf, BufLen, nil);  // ???
                end;
              until (Res = 0) or (Res = NoData) or (PieceStatus = OCI_FIRST_PIECE);
            end
            else
            if FieldDesc.DataType = dtExtString then begin
              ObjPtr := PtrOffset(Item, sizeof(TItemHeader) + FieldDesc.Offset);
              Ptr := Marshal.ReadIntPtr(ObjPtr);
              Marshal.WriteIntPtr(ObjPtr, StringHeap.ReAllocStr(Ptr, FieldDesc.Fixed and TrimFixedChar));
            end
            else
            if FieldDesc.DataType = dtExtWideString then begin
              ObjPtr := PtrOffset(Item, sizeof(TItemHeader) + FieldDesc.Offset);
              Ptr := Marshal.ReadIntPtr(ObjPtr);
              Marshal.WriteIntPtr(ObjPtr, StringHeap.ReAllocStr(Ptr, FieldDesc.Fixed and TrimFixedChar));
            end
            else
            if FieldDesc.DataType = dtExtVarBytes then begin
              Ptr := PtrOffset(Item, sizeof(TItemHeader) + FieldDesc.Offset);
              ATemp := StringHeap.NewBuf(Marshal.ReadInt16(Marshal.ReadIntPtr(Ptr)) + SizeOf(Word));
              CopyBuffer(Marshal.ReadIntPtr(Ptr), ATemp, Marshal.ReadInt16(Marshal.ReadIntPtr(Ptr)) + SizeOf(Word));
              StringHeap.DisposeBuf(Marshal.ReadIntPtr(Ptr));
              Marshal.WriteIntPtr(Ptr, ATemp);
            end;
          end;
          Marshal.WriteInt32(GlobalBuf, $FFFFFF); // DEBUG
        end // if Res = NeedData then begin
        else
          if (FCommand.FOCICallStyle = OCI73) and (Res <> NoData) then
            Assert(False); // DEBUG
      finally
        FConnection.Release;
      end;

      Result := Res = 0;

      if Result then begin
        // Prepare comlex field
        if HasComplexFields then
          for i := 0 to FieldCount - 1 do begin
            FieldDesc := Fields[i];
            if FieldDesc.FieldDescKind <> fdkData then
              continue;
            ObjPtr := Marshal.ReadIntPtr(Item, sizeof(TItemHeader) + Fields[i].Offset);
            case Fields[i].DataType of
              dtBlob,dtMemo:
                if (Fields[i].SubDataType = dtString) or (Fields[i].SubDataType = dtNString) then
                  TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece.Used :=
                    StrLen(PAChar(PtrOffset(TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece, sizeof(TPieceHeader))))
                else
                  TBlob(GetGCHandleTarget(ObjPtr)).Compress;
              dtWideMemo:
                if (Fields[i].SubDataType = dtWideString) or (Fields[i].SubDataType = dtNWideString) then
                  TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece.Used :=
                    StrLenW(PWChar(PtrOffset(TBlob(GetGCHandleTarget(ObjPtr)).FirstPiece, sizeof(TPieceHeader)))) * sizeof(WideChar)
                else
                  TBlob(GetGCHandleTarget(ObjPtr)).Compress;
              dtOraBlob,dtOraClob,dtWideOraClob: begin
                TOraLob(GetGCHandleTarget(ObjPtr)).FNeedReadLob := True;
                if not FDeferredLobRead and FCommand.FCacheLobs then
                  TOraLob(GetGCHandleTarget(ObjPtr)).ReadLob;
              {$IFDEF HAVE_COMPRESS}
                if (Fields[i].DataType = dtOraBlob) and
                  (FCommand.FCompressBlob in [cbClient, cbClientServer])
                then
                  TOraLob(GetGCHandleTarget(ObjPtr)).Compressed := True
                else
                  TOraLob(GetGCHandleTarget(ObjPtr)).Compressed := False;
              {$ENDIF}
              end;
              dtBFile,dtCFile: begin
                TOraFile(GetGCHandleTarget(ObjPtr)).FNeedReadLob := True;
                if not FDeferredLobRead and FCommand.FCacheLobs then begin
                  OraFile := TOraFile(GetGCHandleTarget(ObjPtr));
                  if OraFile.Exists then begin
                    OraFile.Open;
                    try
                      OraFile.ReadLob;
                    finally
                      OraFile.Close;
                    end;
                  end;
                end;
              end;
              dtReference: begin
                OraRef := TOraRef(GetGCHandleTarget(ObjPtr));
                if OraRef.OCIRef <> nil then
                  OraRef.Pin;
              end;
              dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
                Marshal.WriteInt16(
                  TOraTimeStamp(GetGCHandleTarget(ObjPtr)).FIndicator,
                  Marshal.ReadInt16(Item, sizeof(TItemHeader) + DataSize + i * sizeof(sb2))
                );
              dtIntervalYM, dtIntervalDS:
                Marshal.WriteInt16(
                  TOraInterval(GetGCHandleTarget(ObjPtr)).FIndicator,
                  Marshal.ReadInt16(Item, sizeof(TItemHeader) + DataSize + i * sizeof(sb2))
                );
              dtNumber:
                Marshal.WriteInt16(
                  TOraNumber(GetGCHandleTarget(ObjPtr)).FIndicator,
                  Marshal.ReadInt16(Item, sizeof(TItemHeader) + DataSize + i * sizeof(sb2))
                );
              dtXML:
                TOraXML(GetGCHandleTarget(ObjPtr)).ReadXML;
            end;
          end;

        // Create Items
        if FetchBack then begin
          if IntPtr(LastItem) = nil then
            LastItem := Item;

          Item.Prev := nil;
          Item.Next := FirstItem;

          if IntPtr(FirstItem) <> nil then begin
            FirstItem.Prev := Item;
            Item.Order := FirstItem.Order - 1;
          end
          else
            Item.Order := FRecordCount;

          FirstItem := Item;
          Dec(FFetchStart);
        end
        else begin
          if IntPtr(FirstItem) = nil then
            FirstItem := Item;

          Item.Prev := LastItem;
          Item.Next := nil;

          if IntPtr(LastItem) <> nil then begin
            LastItem.Next := Item;
            Item.Order := LastItem.Order + 1;
          end
          else
            Item.Order := 1;

          LastItem := Item;

          Inc(FRecordCount);
          Inc(FFetchEnd);
        end;
        Item.Flag := flUsed;
        Item.Rollback := nil;
        Item.Status := isUnmodified;
        Item.UpdateResult := urNone;
        Item.FilterResult := fsNotChecked;

        UpdateCachedBuffer(Item, Item);
        // update fetched rows for correct TDataSet.RecordCount calculation
        FCommand.FFetchedRows := RecordCount;

      {$IFDEF MSWINDOWS}
        if GetNonBlocking and FFetchAll then
          PulseEvent(THandle(hEvent.Handle));
      {$ENDIF}
      end // if Result then begin
      else begin
        FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True);
        BlockMan.FreeItem(Item);
      end;
    except
      FreeComplexFields(PtrOffset(Item, sizeof(TItemHeader)), True); // !!! Possible AV on fatal error
      // BlockMan.FirstBlock = nil means that dataset was closed after some
      // fatal error and all blocks are already freed.
      if IntPtr(BlockMan.FirstBlock) <> nil then
        BlockMan.FreeItem(Item);
      raise;
    end;
  finally
    FCommand.Release;
  end;
end;

procedure TOCIRecordSet.DoFetchAll;
begin
  try
    FStopFetch := False;
    FFetching := True;
    try
      if CanFetchBack then begin
        SetToEnd;
        Fetch(True);
      end
      else
        while True do begin
          if FStopFetch then begin
            FCommand.InternalCancel;
            break;
          end;
          if not Fetch then
            break;
        end;
    finally
      FFetching := False;
    end;
  except
    // earlier we used to restore old cursor state but it does not lead to
    // any positive results
    FCommand.SetCursorState(csInactive);
    raise;
  end;
end;

procedure TOCIRecordSet.DoFetchAllPulse;
begin
{$IFDEF MSWINDOWS}
  PulseEvent(THandle(hEvent.Handle));
  try
{$ENDIF}
    DoFetchAll;
{$IFDEF MSWINDOWS}
  finally
    hEvent.SetEvent;
  end;
{$ENDIF}
end;

procedure TOCIRecordSet.EndFetchAll(E: Exception);
var
  Fail: boolean;
begin
{$IFDEF MSWINDOWS}
  if hFetchAllThread <> nil then begin
//    FConnection.StopThread(hFetchAllThread);
    hFetchAllThread := nil;
  end
  else
    if (FCommand = nil) or GetNonBlocking then Exit;
{$ENDIF}
  if GetNonBlocking and (IndexFields.Count > 0) then
    SortItems;

  if Assigned(FAfterFetchAll) then
    FAfterFetchAll(E = nil);

  if FCommand.FNonBlocking and (E is EOraError) then begin
    Fail := True;
    FConnection.DoError(EOraError(E), Fail);
    if not Fail then
      Abort;
  end;
end;

procedure TOCIRecordSet.FetchAll;
begin
  if (FCommand.GetCursorState < csFetchingAll) and FCommand.GetActive then begin
    if (FCommand.GetCursorState < csExecuting) then
      FCommand.SetCursorState(csExecuteFetchAll)
    else
      FCommand.SetCursorState(csFetchingAll);  

    FStopFetch := False;

    if FCommand.FNonBlocking then begin
      if not FConnection.FThreadSafety then
        raise Exception.Create(SNeedThreadSafety);
    {$IFDEF MSWINDOWS}
      hEvent.ResetEvent;
      hFetchAllThread := FConnection.RunThread(DoFetchAllPulse, EndFetchAll);
    {$ENDIF}
    end
    else
      DoFetchAll;
  end;
end;

function TOCIRecordSet.RowsReturn: boolean;
begin
  if CommandType <> ctUnknown then
    Result := inherited RowsReturn
  else                              //we need to know this info even if CommandType is not set(TCustomDADataSet.DoAfterExecute)
    Result := FCommand.RowsReturn;
end;
{ Edit }

{ Navigation }

procedure TOCIRecordSet.SetToBegin;
begin
  inherited;

  if CanFetchBack and (FFetchStart <> 0) then begin
    FFetchAbsolute := True;
//    FirstItem := nil;
    FreeAllItems;
    FFetchEnd := 0;
  end;
end;

procedure TOCIRecordSet.SetToEnd;
begin
  StartWait;
  try
    if GetNonBlocking and FFetchAll then begin
      while FCommand.GetCursorState = csFetchingAll do sleep(0);
    end
    else
      if FCommand.GetCursorState < csFetched then
        if CanFetchBack then begin
          if not ((FFetchEnd = FCommand.FFetchedRows) and FNoData) then begin
            FCommand.InternalFetch(1, OCI_FETCH_LAST);
            FNoData := True;
  //          LastItem := nil;
            FreeAllItems;
            FFetchStart := FCommand.FFetchedRows;
          end;
        end
        else begin
          FetchAll;
          while FCommand.GetCursorState = csFetchingAll do sleep(0);
        end;
  finally
    StopWait;
  end;

  inherited;
end;

procedure TOCIRecordSet.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TOCIConnection(Value);
end;

function TOCIRecordSet.GetNonBlocking: boolean;
begin
  Result := FCommand.FNonBlocking;
end;

function TOCIRecordSet.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

function TOCIRecordSet.GetDisconnectedMode: boolean;
begin
  if FConnection <> nil then
    FDisconnectedMode := FConnection.DisconnectedMode;
  Result := FDisconnectedMode;
end;

function TOCIRecordSet.GetUseUnicode: boolean;
begin
  if FConnection <> nil then
    FUseUnicode := FConnection.FUseUnicode;
  Result := FUseUnicode;
end;

function TOCIRecordSet.GetCharLength: integer;
begin
  if FConnection <> nil then
    FCharLength := FConnection.FCharLength;
  Result := FCharLength;
end;

function TOCIRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prFetchRows: begin
      FreeFetchBlock;
      Result := inherited SetProp(Prop, Value);
    end;
    prAutoClose:
      FAutoClose := Boolean(Value);
    prDeferredLobRead:
      FDeferredLobRead := Boolean(Value);
    prFieldsAsString:
      FCommand.FFieldsAsString := Boolean(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCIRecordSet.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prHasObjectFields:
      Value := FHasObjectFields;
    prRowsFetched:
      Value := FCommand.FFetchedRows;
    prAutoClose:
      Value := FAutoClose;
    prDeferredLobRead:
      Value := FDeferredLobRead;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

{ TLOBLocator }

constructor TOraLob.Create(ASvcCtx: pOCISvcCtx);
begin
  inherited Create;

  phLobLocator := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(phLobLocator, nil);
  OCISvcCtx := ASvcCtx;
  FCached := True;
  FCharsetForm := 0;
end;

destructor TOraLob.Destroy;
begin
  FreeLob;
  Marshal.FreeHGlobal(phLobLocator);

  inherited;
end;

procedure TOraLob.AllocLob;
var
  LOBLocator: pOCILOBLocator;
begin
  if hLOBLocator = nil then begin
    if not OCIInited then
      OCIInit;

    LOBLocator := hLOBLocator;
    Check(OCIDescriptorAlloc(hOCIEnv, LOBLocator, OCI_DTYPE_LOB, 0, nil));
    hLOBLocator := LOBLocator;
    FNativeHandle := True;
  end;
end;

procedure TOraLob.FreeLob;
begin
  FreeBlob;
end;

procedure TOraLob.Disconnect;
begin
  FreeBlob;
end;

procedure TOraLob.FreeBlob;
begin
  if IntPtr(hLOBLocator) <> nil then
    if FNativeHandle then
      if IsTemporary then
        FreeTemporary;
  if IntPtr(hLOBLocator) <> nil then begin
    if FNativeHandle then
      Check(OCIDescriptorFree(hLOBLocator, OCI_DTYPE_LOB));
    hLOBLocator := nil;
    FCharsetForm := 0;
    FNativeHandle := False;
  end;
end;

procedure TOraLob.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    {$IFDEF CLR}Devart.Odac.{$ENDIF}{$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.
    {$IFNDEF FPC}OraError{$ELSE}DoOraError{$ENDIF}(Status, FUnicodeEnv, hOCIError);
end;

procedure TOraLob.CheckValue;
begin
  if FNeedReadLob then
    ReadLob;
end;

function TOraLob.GetSize: Cardinal;
begin
  if FNeedReadLob and (FCharLength > 1) and not FIsUnicode then
    ReadLob; // for multibyte charset LengthLob * CharSize is incorrect

  if FNeedReadLob then
    Result := LengthLob * CharSize
  else
    Result := inherited GetSize;
end;

function TOraLob.GetSizeAnsi: Cardinal;
begin
  Assert(FIsUnicode);
  if FNeedReadLob then begin
    if {$IFNDEF CLR}not SysLocale.FarEast{$ELSE}(LeadBytes = []){$ENDIF} then begin
      Result := LengthLob;
      Exit;
    end;
    ReadLob;
  end;

  Result := inherited GetSizeAnsi;
end;

procedure TOraLob.CheckAlloc;
begin
  if hLOBLocator = nil then
    RaiseError(SLobNotAllocatted);
end;

procedure TOraLob.CheckSession;
begin
  if FSvcCtx = nil then
    RaiseError(SSvcCtxNotDefined);
end;

procedure TOraLob.CheckInit;
begin
  if not IsInit then
    RaiseError(SLobNotInited);
end;

procedure TOraLob.Init;
var
  Value: Integer;
begin
  Value := 0;
  Check(OCIAttrSet2(hLOBLocator, OCI_DTYPE_LOB, Value, 0, OCI_ATTR_LOBEMPTY, hOCIError));
end;

procedure TOraLob.CreateTemporary(LobType: TLobType);
var
  OCIlobType: byte;

  // OCILobCreateTemporary can not create NCLOB locator
  procedure PLSQLCreateTemporary;
  var
    Command: TOCICommand;
    TempLob: TOraLob;
  begin
    Command := TOCICommand.Create;
    try
      Command.SetConnection(FConnection);
      Command.SetProp(prAutoCommit, False);      
      Command.SetSQL('begin dbms_lob.createtemporary(:TempClob, False); end;');
      with Command.Params[0] as TOraParamDesc do begin
        SetDataType(dtOraClob);
        SetParamType(pdInput);
        SetNational(True);
        TempLob := TOraLob.Create(nil);
        SetObject(TempLob);
        TempLob.Release;
      end;
      Command.Execute();

      OCILobLocator := TempLob.hLobLocator;
      TempLob.FNativeHandle := False;
      FNativeHandle := True;
    finally
      Command.Free;
    end;
  end;

begin
  CheckOCI81;
  FreeLob;

  OCIlobType := Ord(lobType) + 1;

  if (LobType = ltNClob) and (FConnection <> nil) then
    PLSQLCreateTemporary
  else begin
    AllocLob;
    Check(OCILobCreateTemporary(FSvcCtx, hOCIError, hLOBLocator, OCI_DEFAULT, OCI_DEFAULT, OCIlobType,
      0, OCI_DURATION_SESSION));
  end;

  FLobType := LobType;
  if (OCIlobType = OCI_TEMP_CLOB) or (OCIlobType = OCI_TEMP_BLOB) then
    FCharsetForm := SQLCS_IMPLICIT
  else
    FCharsetForm := SQLCS_NCHAR;
end;

procedure TOraLob.FreeTemporary;
begin
  if IsTemporary and IsInit then
    Check(OCILobFreeTemporary(FSvcCtx, hOCIError, hLOBLocator));
  FreeLob;
end;

function TOraLob.IsTemporary: LongBool;
begin
  Check(OCILobIsTemporary(hOCIEnv, hOCIError, hLOBLocator, Result));
end;

function TOraLob.IsInit: boolean;
var
  Res: tbool;
begin
  CheckAlloc;
  Check(OCILobLocatorIsInit(hOCIEnv, hOCIError, hLOBLOcator, Res));
  Result := Res <> 0;
end;

function TOraLob.LengthLob: Cardinal;
var
  Res: Cardinal;
begin
  CheckAlloc;
  CheckSession;
  if IsInit then begin
    Check(OCILobGetLength(FSvcCtx, hOCIError, hLOBLocator, Res));
    Result := Res;
    // reset FNeedReadLob to avoid multiple LengthLob calls on empty LOB
    if Result = 0 then
      FNeedReadLob := False;
  end
  else begin
    Result := 0;
    FNeedReadLob := False;
  end;
end;

procedure TOraLob.CheckCharSetForm;
var
  CharsetForm: ub1;
begin
  if FCharsetForm = 0 then begin
    Check(OCILobCharSetForm(Self.hOCIEnv, hOCIError, hLOBLocator, CharsetForm));
    FCharsetForm := CharsetForm;
    if CharsetForm = SQLCS_NCHAR then
      FLobType := ltNClob;
  end;
end;

function TOraLob.CharSize: Byte;
begin
  Result := 1;
  if FISUnicode then Result := 2;
end;

procedure TOraLob.ReadLob;
var
  Piece: PPieceHeader;
begin
  Piece := nil;
  ReadLob(Piece);
end;

procedure TOraLob.ReadLob(var SharedPiece: PPieceHeader);
var
  Piece, Piece2: PPieceHeader;
  BufLen: cardinal;
  Buflen_Bytes: Int64;
  Buflen_Chars: Int64;
  Res: integer;
  CharsetId: word;
  StartBufLen: longint;
  PieceState: Byte;
begin
  CheckAlloc;
  CheckSession;

  FData.Clear;
  PieceState := OCI_FIRST_PIECE;
  if IsInit then begin
    StartBufLen := 0;
    CheckCharSetForm;
    if IntPtr(SharedPiece) = nil then
      AllocPiece(Piece, PieceSize)
    else
      Piece := SharedPiece;
    repeat
      try
        BufLen := StartBufLen;

        if FIsUnicode then
          CharsetId := OCI_UTF16ID
        else
          CharsetId := 0;

        if (OCIVersion >= 10000) then begin
          Buflen_Bytes := BufLen;
          Buflen_Chars := BufLen;
          Res := OCILobRead2(FSvcCtx, hOCIError, hLOBLocator, Buflen_Bytes, Buflen_Chars, 1,
            PtrOffset(Piece, Sizeof(TPieceHeader)), PieceSize, PieceState, nil, nil, CharsetId, FCharsetForm);
          PieceState := OCI_NEXT_PIECE;
          Piece.Used :=  Buflen_Bytes;
          BufLen := Buflen_Bytes;
        end
        else begin
          Res := OCILobRead(FSvcCtx, hOCIError, hLOBLocator, BufLen, 1,
            PtrOffset(Piece, Sizeof(TPieceHeader)), PieceSize, nil, nil, CharsetId, FCharsetForm);
          Piece.Used := BufLen * CharSize;
        end;

        if Res <> OCI_NEED_DATA then
          Check(Res);
      except
        if IntPtr(Piece) <> IntPtr(SharedPiece) then
          FreePiece(Piece);
        raise;
      end;

      if BufLen = 0 then begin  // 804 server and any another client
        if IntPtr(Piece) <> IntPtr(SharedPiece) then
          FreePiece(Piece);
        FNeedReadLob := False;
        Exit;
      end;

      if Piece = SharedPiece then begin
        if Piece.Used < Piece.Size div 2 then begin
          AllocPiece(Piece2, Piece.Used);
          CopyBuffer(PtrOffset(Piece, SizeOf(TPieceHeader)),
            PtrOffset(Piece2, SizeOf(TPieceHeader)), Piece.Used);
          Piece2.Used := Piece.Used;
          Piece := Piece2;
        end
        else
          SharedPiece := nil;
      end
      else
        if Piece.Used < Piece.Size div 2 then
          CompressPiece(Piece);

      AppendPiece(Piece);

      if Res = OCI_SUCCESS then
        break
      else
        AllocPiece(Piece, PieceSize);
    until False;
  end;
  FNeedReadLob := False;
  FCached := True;
end;

procedure TOraLob.WriteLob;
var
  Piece: PPieceHeader;
  BufLen: cardinal;
  Offset: cardinal;
  CharsetId: word;
  CharCount: ub4;
  EndChar: byte;
begin
  CheckAlloc;
  CheckSession;
  CheckInit;
  CheckCharSetForm;

  Check(OCILobTrim(FSvcCtx, hOCIError, hLOBLocator, 0));

  Piece := FirstPiece;
  Offset := 1;
  while IntPtr(Piece) <> nil do begin
    BufLen := Piece.Used;

    if (PossibleOCICallStyles = [OCI80]) and (FLobType <> ltBlob) and not FIsUnicode then begin
      EndChar := Marshal.ReadByte(IntPtr(Piece), Sizeof(TPieceHeader) + BufLen - 1);
      if EndChar = 0 then
        Dec(BufLen);
    end;

    if FIsUnicode then begin
      CharsetId := OCI_UTF16ID;
      CharCount := BufLen div CharSize;
    end
    else begin
      if (FLobType <> ltBlob) and FUnicodeEnv then
        raise Exception.Create(SClobMustBeUnicode);
      CharsetId := 0;
      CharCount := BufLen;
    end;
    Check(OCILobWrite(FSvcCtx, hOCIError, hLOBLocator, CharCount, Offset,
      PtrOffset(Piece, Sizeof(TPieceHeader)), BufLen, OCI_ONE_PIECE, nil, nil, CharsetId, FCharsetForm));

    Piece := Piece.Next;
    Inc(Offset, CharCount);
  end;
end;

function TOraLob.Read(Position, Count: cardinal; Dest: IntPtr): cardinal;
var
  BytesRead: cardinal;
  BytesRead64: Int64;
  CharsRead: Int64;
  Res: integer;
  CharsetId: word;
begin
  Result := 0;
  if not Cached then begin
    CheckAlloc;
    CheckSession;
    if IsInit then begin
      BytesRead := Count; // to avoid OCI_NEED_DATA error
      CheckCharSetForm;

      if BytesRead = 0 then
        BytesRead := LengthLob;

      if Count = 0 then
        Count := Size;

      if FIsUnicode then
        CharsetId := OCI_UTF16ID
      else
        CharsetId := 0;

      if (OCIVersion >= 10000) then begin
        BytesRead64 := BytesRead;
        CharsRead := BytesRead;

        Res := OCILobRead2(FSvcCtx, hOCIError, hLOBLocator, BytesRead64, CharsRead, Position + 1,
          Dest, Count, OCI_FIRST_PIECE, nil, nil, CharsetId, FCharsetForm);
        BytesRead := CharsRead;
        Result := BytesRead64;
      end else begin
        Res := OCILobRead(FSvcCtx, hOCIError, hLOBLocator, BytesRead, Position + 1,
          Dest, Count, nil, nil, CharsetId, FCharsetForm);
        Result := BytesRead * CharSize;
      end;

      if Res <> OCI_NEED_DATA then
        Check(Res);
    end;
  end
  else
    Result := inherited Read(Position, Count, Dest);
end;

procedure TOraLob.Write(Position, Count: cardinal; Source: IntPtr);
begin
  Cached := True;

  inherited Write(Position, Count, Source);
end;

procedure TOraLob.Clear;
begin
  FNeedReadLob := False;
  
  inherited Clear;
end;

procedure TOraLob.Truncate(NewSize: cardinal);
begin
  if NewSize = 0 then
    FNeedReadLob := False
  else
    CheckValue;

  inherited Truncate(NewSize);
end;

{procedure TLOBLocator.TrimLob(Size:longint);
begin
  CheckAlloc;

  EnableBuffering;
  Check(OCILobTrim(FConnection.GetSvcCtx, hOCIError, hLOBLOcator, Size));
  DisableBuffering;
end;}

procedure TOraLob.EnableBuffering;
begin
  CheckAlloc;
  CheckSession;

  Check(OCILobEnableBuffering(FSvcCtx, hOCIError, hLOBLOcator));
end;

procedure TOraLob.DisableBuffering;
begin
  CheckAlloc;
  CheckSession;

  Check(OCILobDisableBuffering(FSvcCtx, hOCIError, hLOBLOcator));
end;

function TOraLob.GethOCILobLocator: pOCIStmt;
begin
  Result := Marshal.ReadIntPtr(phLobLocator);
end;

procedure TOraLob.SethOCILobLocator(Value: pOCIStmt);
begin
  Marshal.WriteIntPtr(phLobLocator, Value);
end;

function TOraLob.GetOCILobLocator: pOCILobLocator;
begin
  if hLobLocator = nil then
    AllocLob;

  Result := hLobLocator;
end;

procedure TOraLob.SetOCILobLocator(Value: pOCILobLocator);
begin
  FreeLob;
  hLobLocator := Value;
  if IntPtr(hLobLocator) <> nil then
    FNativeHandle := False;
end;

function TOraLob.GetOCILobLocatorPtr: ppOCILobLocator;
begin
  if hLobLocator = nil then
    AllocLob;

  Result := phLobLocator;
end;

procedure TOraLob.SetCached(const Value: boolean);
begin
  if FCached <> Value then begin
    if not Value and (IntPtr(FirstPiece) <> nil) then begin
      FData.Clear;
      FNeedReadLob := True;
    end;
    FCached := Value;
  end;
end;

procedure TOraLob.SetOCISvcCtx(const Value: pOCISvcCtx);
var
  ValuePtr: IntPtr;
begin
  FConnection := nil;
  FSvcCtx := Value;
  FUnicodeEnv := False;
  if IntPtr(FSvcCtx) <> nil then begin
    ValuePtr := OrdinalToPtr(hOCIEnv);
    try
      Check(OCIAttrGet1(FSvcCtx, OCI_HTYPE_SVCCTX, ValuePtr, nil, OCI_ATTR_ENV, hOCIError));
    finally
      PtrToOrdinal(ValuePtr, hOCIEnv);
    end;
    FUnicodeEnv := IsUnicodeEnv(hOCIEnv, hOCIError);
  end;
end;

procedure TOraLob.SetConnection(const Value: TOCIConnection);
begin
  if Value <> nil then
    SetOCISvcCtx(Value.hSvcCtx);
  FConnection := Value;
end;

procedure TOCIRecordSet.GetFieldData(Field: TFieldDesc; FieldBuf: IntPtr; Dest: IntPtr);
begin
  case Field.DataType of
    dtFixedChar:
      if NeedConvertEOL then
        AddCRString(FieldBuf, Dest, Field.Size)
      else
        CopyBuffer(FieldBuf, Dest, Field.Size);
    dtFixedWideChar:
      if NeedConvertEOL then
        AddCRUnicode(FieldBuf, Dest, Field.Size div 2)
      else
        CopyBuffer(FieldBuf, Dest, Field.Size);
  else
    inherited GetFieldData(Field, FieldBuf, Dest);
  end;
end;

procedure TOCIRecordSet.GetFieldAsVariant(FieldNo: word; RecBuf: IntPtr;
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
      dtOraClob: begin
        Value := TBlob(GetGCHandleTarget(ObjPtr)).AsString;
      end;
      dtWideOraClob:
        Value := TBlob(GetGCHandleTarget(ObjPtr)).AsWideString;
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
        Value := TOraTimeStamp(GetGCHandleTarget(ObjPtr)).AsDateTime;
      dtIntervalYM, dtIntervalDS:
        Value := TOraInterval(GetGCHandleTarget(ObjPtr)).AsString;
      dtNumber:
        Value := TOraNumber(GetGCHandleTarget(ObjPtr)).AsFloat;
    else
      inherited;
    end
  finally
    if not NativeBuffer then
      Marshal.FreeHGlobal(FieldData);
  end;
end;

procedure TOCIRecordSet.PutFieldAsVariant(FieldNo: word; RecBuf: IntPtr; const Value: variant; IsDatabaseValue: boolean = False);
var
  Field: TFieldDesc;
  FieldData: IntPtr;
  IsBlank, NativeBuffer: boolean;
  ObjPtr: IntPtr;
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then begin
    SetNull(FieldNo, RecBuf, True);
    Exit;
  end;

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
      dtNumber: begin
        case VarType(Value) of
          varSmallint,varInteger,varByte:
            TOraNumber(GetGCHandleTarget(ObjPtr)).AsInteger := Value;
          varSingle,varDouble{$IFDEF WIN32_64},varCurrency{$ENDIF}:
            TOraNumber(GetGCHandleTarget(ObjPtr)).AsString := Value;
        else
          if VarIsStr(Value) then
            TOraNumber(GetGCHandleTarget(ObjPtr)).AsString := Value
          else
            raise EConvertError.Create(SCannotConvertType);
        end;
      end;
    else
      inherited;
    end
  finally
    if not NativeBuffer then
      Marshal.FreeHGlobal(FieldData);
  end;
end;

function TOCIRecordSet.FieldListDependsOnParams: boolean;
begin
  Result := (FCommand <> nil) and (FCommand.FSQLType = SQL_PLSQL) and HasCursorParams;
end;

procedure TOCIRecordSet.GetBookmark(Bookmark: PRecBookmark);
var
  ItemSize: integer;
begin
  if CanFetchBack then
//    with Bookmark^ do
    begin
      Bookmark.Item := CurrentItem;
      if IntPtr(CurrentItem) <> nil then begin
        if FPieceFetch then
          Bookmark.Order := GetItemFetchPos(CurrentItem)
        else begin
          Bookmark.RefreshIteration := GetBlockFetchPos(CurrentItem.Block);
          ItemSize := RecordSize + sizeof(TItemHeader);
          Bookmark.Order := (PtrSubstract(CurrentItem, CurrentItem.Block) - SizeOf(TBlockHeader)) div ItemSize;
        end;
      end;
    end
  else
    inherited;
end;

procedure TOCIRecordSet.SetToBookmark(Bookmark: PRecBookmark);
var
  AFetchStart: integer;
  i: integer;

  function SetCurrentItem(Block: PBlockHeader; ABlockFetchStart: integer): boolean;
  var
    ItemSize: integer;
  begin
    if IntPtr(Block) <> nil then
      Result := (AFetchStart >= ABlockFetchStart) and (AFetchStart < ABlockFetchStart + Block.UsedItems)
    else
      Result := False;
    if Result then begin
      ItemSize := RecordSize + sizeof(TItemHeader);
      CurrentItem := PtrOffset(Block, SizeOf(TBlockHeader) +
        (AFetchStart - ABlockFetchStart) * ItemSize);
      Assert(CurrentItem.Flag = flUsed);
    end
  end;

begin
  if CanFetchBack then
//    with Bookmark^ do
      if IntPtr(Bookmark.Item) <> nil then begin
        FBOF := False;
        FEOF := False;
        if FPieceFetch then begin
          if (Bookmark.Item.Flag = flUsed) and (Bookmark.Order = GetItemFetchPos(Bookmark.Item)) then
            CurrentItem := Bookmark.Item
          else begin
            if (Bookmark.Order >= FFetchStart - BlockMan.DefaultItemCount div 2) and
              (Bookmark.Order < FFetchEnd + BlockMan.DefaultItemCount div 2)
            then begin
              if Bookmark.Order < FFetchStart then begin
                for i := FFetchStart + 1 downto Bookmark.Order do
                  Fetch(True);
                CurrentItem := FirstItem;
              end
              else
              if Bookmark.Order > FFetchEnd then begin
                for i := FFetchEnd to Bookmark.Order do
                  Fetch;
                CurrentItem := LastItem;
              end
            end
            else begin
              FreeAllItems;
              FFetchAbsolute := True;
              FFetchStart := AFetchStart;
              FFetchEnd := AFetchStart;
              FirstItem := nil;
              LastItem := nil;
              Fetch;
              CurrentItem := FirstItem;
            end
          end
        end
        else begin
          if (Bookmark.Item.Flag = flUsed) and (Bookmark.RefreshIteration = GetBlockFetchPos(Bookmark.Item.Block)) then
            CurrentItem := Bookmark.Item
          else begin
            AFetchStart := Bookmark.RefreshIteration + Bookmark.Order;
            if (AFetchStart >= FFetchStart - FFetchRows) and (AFetchStart < FFetchEnd + FFetchRows) then begin
              if AFetchStart < FFetchStart then
                Fetch(True)
              else
              if AFetchStart > FFetchEnd then
                Fetch;

              if not SetCurrentItem(BlockMan.FirstBlock, FFetchEnd - BlockMan.FirstBlock.UsedItems) then
                SetCurrentItem(BlockMan.FirstBlock.Next, FFetchStart);
            end
            else begin
              FreeAllItems;
              FFetchAbsolute := True;
              FFetchStart := AFetchStart;
              FFetchEnd := AFetchStart;
              FirstItem := nil;
              LastItem := nil;
              Fetch;
              CurrentItem := FirstItem;
            end;
            if IntPtr(FirstItem) <> nil then
              FirstItem.Prev := nil;  // remove cycle link
            if IntPtr(LastItem) <> nil then
              LastItem.Next := nil;
          end;
        end;
      end
      else
        CurrentItem := nil
  else
    inherited;
end;

function TOCIRecordSet.GetBlockFetchPos(Block: PBlockHeader): integer;
begin
  if Block = BlockMan.FirstBlock then
    Result := FFetchEnd - BlockMan.FirstBlock.UsedItems
  else
    Result := FFetchStart
end;

function TOCIRecordSet.GetItemFetchPos(Item: PItemHeader): integer;
begin
  Result := FFetchStart;
  while (IntPtr(Item) <> nil) and (IntPtr(Item) <> IntPtr(FirstItem)) do begin
    Item := Item.Prev;
    Inc(Result);
  end;
end;

{ TOraFile }

destructor TOraFile.Destroy;
begin
{  if IsOpen then
    Close;}

  inherited;
end;

procedure TOraFile.AllocLob;
var
  LOBLocator: pOCILOBLocator;
begin
  if hLOBLocator = nil then begin
    if not OCIInited then
      OCIInit;

    LOBLocator := hLOBLocator;
    Check(OCIDescriptorAlloc(hOCIEnv, LOBLocator, OCI_DTYPE_FILE, 0, nil));
    hLOBLocator := LOBLocator;
    FNativeHandle := True;
  end;
end;

procedure TOraFile.FreeBlob;
begin
  if IntPtr(hLOBLocator) <> nil then begin
    if FNativeHandle then
      Check(OCIDescriptorFree(hLOBLocator, OCI_DTYPE_FILE));
    hLOBLocator := nil;
    FNativeHandle := False;
  end;
end;

procedure TOraFile.Open;
begin
  CheckAlloc;
  CheckSession;

  if IsInit then
    Check(OCILobFileOpen(FSvcCtx, hOCIError, hLobLocator, OCI_FILE_READONLY));
end;

procedure TOraFile.Close;
begin
  CheckAlloc;
  CheckSession;

  if IsInit then
    Check(OCILobFileClose(FSvcCtx, hOCIError, hLobLocator));
end;

procedure TOraFile.EnableRollback;
begin
  FNeedRollback := True;
end;

procedure TOraFile.SaveToRollback;
begin
  FRollbackFileDir := GetFileDir;
  FRollbackFileName := GetFileName;

  CanRollback := True;
end;

procedure TOraFile.Commit;
begin
  CanRollback := False;
  FNeedRollback := False;
end;

procedure TOraFile.Cancel;
begin
  if CanRollback then begin
    FileName := FRollbackFileName;
    FileDir := FRollbackFileDir;
    if Cached then
      Refresh
    else begin
      Clear;
      FNeedReadLob := True;
    end;
  end;
  CanRollback := False;
  FNeedRollback := False;
end;

procedure TOraFile.Refresh;
begin
  CheckAlloc;
  CheckSession;

  FData.Clear;

  if Exists then begin
    Open;
    try
      ReadLob;
    finally
      Close;
    end;
  end;
end;

function TOraFile.IsOpen: boolean;
var
  Res: tbool;
begin
  Result := False;
  if (IntPtr(hLobLocator) <> nil) and IsInit then begin
    CheckSession;
    Check(OCILobFileIsOpen(FSvcCtx, hOCIError, hLobLocator, Res));
    Result := Res <> 0;
  end;
end;

function TOraFile.Exists: boolean;
var
  Val: tbool;
  Res: sword;
begin
  Result := False;
  if (IntPtr(hLobLocator) <> nil) and IsInit then begin
    CheckSession;

    if FileName = '' then
      Exit;

    Res := OCILobFileExists(FSvcCtx, hOCIError, hLobLocator, Val);
    if Res = OCI_SUCCESS then
      Result := Val <> 0
    else
      if GetOraError(Res, FUnicodeEnv) <> 22285 then  // dir or file not exist
        {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.
        {$IFNDEF FPC}OraError{$ELSE}DoOraError{$ENDIF}(Res, FUnicodeEnv);
  end;
end;

function TOraFile.GetFileDir: _string;
var
  Len: ub2;
  hLen, Handle: IntPtr;
begin
  if (hLobLocator <> nil) and IsInit then begin
    Len := 255 * SizeOfCharOCI(FUnicodeEnv);
    Handle := Marshal.AllocHGlobal(Len);
    hLen := OrdinalToPtr(Len);
    try
      Check(OCILobFileGetName(hOCIEnv, hOCIError, hLobLocator, Handle, hLen, nil, nil));
    finally
      PtrToOrdinal(hLen, Len);
      Result := PtrToStringOCI(Handle, Len, FUnicodeEnv);
      Marshal.FreeHGlobal(Handle);
    end;
    if Result = #0 then
      Result := '';
  end
  else
    Result := '';
end;

procedure TOraFile.SetFileDir(const Value: _string);
begin
  SetFileDirAndName(_UpperCase(Value), FileName);
end;

function TOraFile.GetFileName: _string;
var
  Len: ub2;
  hLen: IntPtr;
  Handle: IntPtr;
begin
  if (IntPtr(hLobLocator) <> nil) and IsInit then begin
    Len := 255 * SizeOfCharOCI(FUnicodeEnv);
    Handle := Marshal.AllocHGlobal(Len);
    hLen := OrdinalToPtr(Len);
    try
      Check(OCILobFileGetName(hOCIEnv, hOCIError, hLobLocator, nil, nil, Handle, hLen));
    finally
      PtrToOrdinal(hLen, Len);
      Result := PtrToStringOCI(Handle, Len, FUnicodeEnv);
      Marshal.FreeHGlobal(Handle);
    end;
    if Result = #0 then
      Result := '';
  end
  else
    Result := '';
end;

procedure TOraFile.SetFileName(const Value: _string);
begin
  SetFileDirAndName(FileDir, Value);
end;

procedure TOraFile.SetFileDirAndName(const FileDir, FileName: _string);
var
  DSize, NSize, Res: integer;
  p1, p2: IntPtr;
begin
  CheckAlloc;

  if FNeedRollback and not CanRollback then
    SaveToRollback;

  p1 := StringToHGlobalOCI(FileDir, DSize, FUnicodeEnv);
  if DSize = 0 then
    DSize := SizeOfCharOCI(FUnicodeEnv);
  p2 := StringToHGlobalOCI(FileName, NSize, FUnicodeEnv);
  if NSize = 0 then
    NSize := SizeOfCharOCI(FUnicodeEnv);

  Res := OCILobFileSetName(hOCIEnv, hOCIError, phLobLocator, p1, DSize, p2, NSize);
  FreeStringOCI(p1, FUnicodeEnv);
  FreeStringOCI(p2, FUnicodeEnv);
  Check(Res);
end;

procedure TOraFile.CheckValue;
begin
  if FNeedReadLob and Exists then begin
    Open;
    try
      ReadLob;
    finally
      Close;
    end;
  end;
end;

function TOraFile.CharSize: Byte;
begin
  Result := 1;
end;

{ TOraTimeStamp }

constructor TOraTimeStamp.Create(DataType: word);
begin
  inherited Create;

  phOCIDateTime := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(phOCIDateTime, nil);
  FIndicator := Marshal.AllocHGlobal(sizeof(OCIInd));
  Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
  FPrecision := 6;
  hOCIDateTime := nil;

  case DataType of
    dtTimeStamp{$IFDEF VER6P}{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}{$ENDIF}:
      FDescriptorType := OCI_DTYPE_TIMESTAMP;
    dtTimeStampTZ:
      FDescriptorType := OCI_DTYPE_TIMESTAMP_TZ;
    dtTimeStampLTZ:
      if not OldTimeStampLTZRepresentation then
        FDescriptorType := OCI_DTYPE_TIMESTAMP
      else
        FDescriptorType := OCI_DTYPE_TIMESTAMP_LTZ;
  else
    Assert(False);
  end;
end;

destructor TOraTimeStamp.Destroy;
begin
  FreeDateTime;
  Marshal.FreeHGlobal(phOCIDateTime);
  Marshal.FreeHGlobal(FIndicator);

  inherited;
end;

procedure TOraTimeStamp.AllocDateTime;
var
  OCIDateTime: pOCIDateTime;
begin
  if hOCIDateTime = nil then begin
    if not OCIInited then
      OCIInit;

    OCIDateTime := hOCIDateTime;
    Check(OCIDescriptorAlloc({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, OCIDateTime, FDescriptorType, 0, nil));
    hOCIDateTime := OCIDateTime;
    FNativeHandle := True;
  end;
end;

procedure TOraTimeStamp.FreeDateTime;
begin
  if IntPtr(hOCIDateTime) <> nil then begin
    if FNativeHandle then
      Check(OCIDescriptorFree(hOCIDateTime, FDescriptorType));
    hOCIDateTime := nil;
    Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
    FNativeHandle := False;
  end;
end;

procedure TOraTimeStamp.Disconnect;
begin
  FreeDateTime;
end;

function TOraTimeStamp.GetIsNull: boolean;
var
  Indicator: OCIInd;
begin
  Indicator := Marshal.ReadInt16(FIndicator);
  Result := Indicator = OCI_IND_NULL;
end;

procedure TOraTimeStamp.SetIsNull(Value: boolean);
begin
  if Value then
    Marshal.WriteInt16(FIndicator, OCI_IND_NULL)
  else
    Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
end;

procedure TOraTimeStamp.AssignTo(Dest: TOraTimeStamp);
begin
  Dest.DescriptorType := DescriptorType;
  Dest.AllocDateTime;
  Dest.Format := Format;
  Dest.Precision := Precision;
  Marshal.WriteInt16(Dest.FIndicator, Marshal.ReadInt16(FIndicator));
  if not IsNull then
    Check(OCIDateTimeAssign({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIDateTime, Dest.OCIDateTime));
end;

function TOraTimeStamp.Compare(Dest: TOraTimeStamp): integer;
begin
  Check(OCIDateTimeCompare({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIDateTime, Dest.OCIDateTime, Result));
end;

procedure TOraTimeStamp.SetDescriptorType(const Value: cardinal);
begin
  if FDescriptorType <> Value then begin
    FreeDateTime;
    FDescriptorType := Value;
  end;
end;

function TOraTimeStamp.GethOCIDateTime: pOCIDateTime;
begin
  Result := Marshal.ReadIntPtr(phOCIDateTime);
end;

procedure TOraTimeStamp.SethOCIDateTime(Value: pOCIDateTime);
begin
  Marshal.WriteIntPtr(phOCIDateTime, Value);
end;

function TOraTimeStamp.GetOCIDateTime: pOCIDateTime;
begin
  AllocDateTime;
  Result := hOCIDateTime;
end;

procedure TOraTimeStamp.SetOCIDateTime(const Value: pOCIDateTime);
begin
  FreeDateTime;
  hOCIDateTime := Value;
  if IntPtr(Value) <> nil then begin
    FNativeHandle := False;
    IsNull := False;
  end;
end;

function TOraTimeStamp.GetOCIDateTimePtr: ppOCIDateTime;
begin
  AllocDateTime;
  Result := phOCIDateTime;
end;

procedure TOraTimeStamp.Construct(Year: smallint; Month, Day, Hour, Min,
  Sec: byte; FSec: cardinal; TimeZone: string);
var
  Res, BufSize: integer;
  p: IntPtr;
begin
  if TimeZone <> '' then
    p := StringToHGlobalOCI(TimeZone, BufSize, OCIUnicode)
  else begin
    p := nil; // PChar('') means UTC
    BufSize := 0;
  end;

  Res := OCIDateTimeConstruct({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIDateTime, Year, Month,
    Day, Hour, Min, Sec, FSec, p, BufSize);
  FreeStringOCI(p, OCIUnicode);
  Check(Res);
  Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
end;

function TOraTimeStamp.GetAsString: string;
var
  BufSize: ub4;
  FmtSize: integer;
  Ptr, FmtPtr: IntPtr;
begin
  if IsNull then begin // to avoid error on OCI function call
    Result := '';
    Exit;
  end;

  if FFormat = '' then
    SetFormat('');

  BufSize := 255 * SizeOfCharOCI(OCIUnicode);
  Ptr := Marshal.AllocHGlobal(BufSize);
  FmtPtr := StringToHGlobalOCI(FFormat, FmtSize, OCIUnicode);
  try
    try
      Check(OCIDateTimeToText({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError,
        OCIDateTime, FmtPtr, FmtSize, Precision, nil, 0, BufSize, Ptr));
      Result := PtrToStringOCI(Ptr, BufSize, OCIUnicode);
    except
      // Timestamp value is corrupted by a trigger and RETURNING INTO
      on E: EOraError do
        if E.ErrorCode = 1877 then
          Result := ''
        else
          raise;
      on E: EConvertError do // for Direct option
        Result := '';
    end;
  finally
    Marshal.FreeHGlobal(Ptr);
    FreeStringOCI(FmtPtr, OCIUnicode);
  end;
end;

procedure TOraTimeStamp.SetAsString(const Value: string);
var
  hTmp: pOCIDateTime;
  Buf: pOCIDateTime;
  p1, p2: IntPtr;
  VSize, FSize: integer;
begin
  // We need to backup timestamp value because OCIDateTimeFromText may corrupt
  // it if input string is wrong format.
  if Value = '' then begin
    Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
    Exit;
  end;

  if FFormat = '' then
    SetFormat('');

  hTmp := hOCIDateTime;
  hOCIDateTime := nil;
  p1 := StringToHGlobalOCI(Value, VSize, OCIUnicode);
  p2 := StringToHGlobalOCI(FFormat, FSize, OCIUnicode);
  try
    try
      Check(OCIDateTimeFromText({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError,
        p1, VSize, p2, FSize, nil, 0, OCIDateTime));
    except
      // restore valid timestamp value
      Buf := hTmp;
      hTmp := hOCIDateTime;
      hOCIDateTime := Buf;
      raise;
    end;
    Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
  finally
    FreeStringOCI(p1, OCIUnicode);
    FreeStringOCI(p2, OCIUnicode);
    if hTmp <> nil then
      OCIDescriptorFree(hTmp, FDescriptorType);
  end;
end;

procedure TOraTimeStamp.GetDate(var Year: smallint; var Month, Day: byte);
begin
  Check(OCIDateTimeGetDate({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIDateTime, Year, Month, Day));
end;

procedure TOraTimeStamp.SetDate(Year: smallint; Month, Day: byte);
var
  Hour, Min, Sec: byte;
  FSec: cardinal;
  TZ: string;
begin
  GetTime(Hour, Min, Sec, FSec);

  if DescriptorType <> OCI_DTYPE_TIMESTAMP then
    // suppress exception raised when timezone is absent
    try
      TZ := TimeZone;
    except
      on EOraError do;
    end;

  Construct(Year, Month, Day, Hour, Min, Sec, FSec, TZ);
end;

procedure TOraTimeStamp.GetTime(var Hour, Min, Sec: byte; var FSec: cardinal);
begin
  Check(OCIDateTimeGetTime({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIDateTime, Hour, Min, Sec, FSec));
end;

procedure TOraTimeStamp.SetTime(Hour, Min, Sec: byte; FSec: cardinal);
var
  Year: smallint;
  Month, Day: byte;
  TZ: string;
begin
  GetDate(Year, Month, Day);
  if DescriptorType <> OCI_DTYPE_TIMESTAMP then
    TZ := TimeZone;
  Construct(Year, Month, Day, Hour, Min, Sec, FSec, TZ);
end;

procedure TOraTimeStamp.GetTimeZoneOffset(var Hour, Min: shortint);
begin
  Check(OCIDateTimeGetTimeZoneOffset({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIDateTime, Hour, Min));
end;

procedure TOraTimeStamp.SetTimeZoneOffset(TZHour, TZMin: shortint);
var
  Year: smallint;
  Month, Day, Hour, Min, Sec: byte;
  FSec: cardinal;
  TZ: string;
begin
  GetDate(Year, Month, Day);
  GetTime(Hour, Min, Sec, FSec);

  TZ := '';

  if TZHour >= 0 then
    TZ := TZ + '+';
  TZ := TZ + IntToStr(TZHour) + ':';

  if tzMin >= 0 then
    TZ := TZ + '+';

  TZ := TZ + IntToStr(TZMin);

  Construct(Year, Month, Day, Hour, Min, Sec, FSec, TZ);

  CheckValid;
end;

function TOraTimeStamp.GetTimeZone: string;
var
  BufSize: cardinal;
  Ptr: IntPtr;
begin
  BufSize := 255;
  Ptr := Marshal.AllocHGlobal(BufSize);
  try
    Check(OCIDateTimeGetTimeZoneName({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIDateTime,
      Ptr, BufSize));
    Result := string(Marshal.PtrToStringAnsi(Ptr, BufSize));
  finally
    Marshal.FreeHGlobal(Ptr);
  end;
end;

procedure TOraTimeStamp.CheckValid;
var
  Valid: cardinal;
begin
  Assert(hOCIDateTime <> nil);
  Valid := 0;
  Check(OCIDateTimeCheck({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, hOCIDateTime, Valid));
  if Valid <> 0 then
    raise Exception.Create(SInvalidTimeStamp);
end;

procedure TOraTimeStamp.SetFormat(const AFormat: string);
var
  P: integer;
  Str: string;
  Buf: integer;
  Temp: string;
begin
  if AFormat = '' then begin
    FFormat := '';
    P := 1;
    Str := ShortDateFormat;
    while (P <= Length(Str)) {(P <> nil) and (Str[P] <> #0)} do begin
      case Str[P] of
        'd','D': begin
          Inc(P);
          if (Str[P] = 'd') or (Str[P] = 'D') then begin
            Inc(P);
            if (Str[P] = 'd') or (Str[P] = 'D') then begin
              Inc(P);
              if (Str[P] = 'd') or (Str[P] = 'D') then begin
                Inc(P);
                FFormat := FFormat + 'fmDAYfm';
              end else
                FFormat := FFormat + 'DY';
            end else
              FFormat := FFormat + 'DD';
          end else
            FFormat := FFormat + 'fmDDfm';
        end;
        'm','M': begin
          Inc(P);
          if (Str[P] = 'm') or (Str[P] = 'M') then begin
            Inc(P);
            if (Str[P] = 'm') or (Str[P] = 'M') then begin
              Inc(P);
              if (Str[P] = 'm') or (Str[P] = 'M') then begin
                Inc(P);
                FFormat := FFormat + 'MONTH';
              end else
                FFormat := FFormat + 'MON';
            end else
              FFormat := FFormat + 'MM';
          end else
            FFormat := FFormat + 'fmMMfm';
        end;
        'g','G': begin
          Inc(P);
          FFormat := FFormat + 'E';
        end;
        '''','"': begin
          Buf := P;
          Inc(P);
          P := Pos(Str[Buf], Copy(Str, P, Length(Str) - P + 1));
          if P <> 0 then begin
            Inc(P, Buf + 1);
            Str[Buf] := '"';
            Str[P - 1] := '"';
            FFormat := FFormat + Copy(Str, Buf, P - Buf)
          end else begin
            P := Length(Str) + 1;
            FFormat := FFormat + Copy(Str, Buf, P - Buf) + '"';
          end;
        end;
      else
        FFormat := FFormat + UpperCase(Str[P]);
        Inc(P);
      end;
    end;
    FFormat := FFormat + ' ';
    Str := LongTimeFormat;
    P := 1;
    while (P <= Length(Str)) {(P <> nil) and (Str[P] <> #0)} do begin
      case Str[P] of
        't','T': begin
          Inc(P);
          while (P <= Length(Str)) and ((Str[P] = 't') or (Str[P] = 'T')) do
            Inc(P);
          FFormat := FFormat + 'AM';
        end;
        'h','H': begin
          Inc(P);
          if (AnsiPos('AMPM', LongTimeFormat) = 0) and (AnsiPos('t', LongTimeFormat) = 0) then
            Temp := '24'
          else
            Temp := '12';
          if (Str[P] = 'h') or (Str[P] = 'H') then begin
            Inc(P);
            FFormat := FFormat + 'HH' + Temp;
          end else
            FFormat := FFormat + 'fmHH'+ Temp +'fm';
        end;
        'n','N','m','M': begin
          Inc(P);
          if (Str[P] = 'n') or (Str[P] = 'N') or (Str[P] = 'm') or (Str[P] = 'M') then begin
            Inc(P);
            FFormat := FFormat + 'MI';
          end else
            FFormat := FFormat + 'fmMIfm';
        end;
        's','S': begin
          Inc(P);
          if AnsiPos('Z', UpperCase(Str)) = 0 then begin
            Temp := DecimalSeparator + 'FF'; // + IntToStr(Precision); does not work on Oracle 9.0.1
          end else
            Temp := '';
          if (Str[P] = 's') or (Str[P] = 'S') then begin
            Inc(P);
            FFormat := FFormat + 'SS' + Temp;
          end else
            FFormat := FFormat + 'fmSSfm' + Temp;
        end;
        'z','Z': begin
          FFormat := FFormat + 'FF';
          Inc(P);
          while (P <= Length(Str)) and ((Str[P] = 'z') or (Str[P] = 'Z')) do
            Inc(P);
        end;
        '''','"': begin
          Buf := P;
          Inc(P);
          P := Pos(Str[Buf], Copy(Str, P, Length(Str) - P + 1));
          if P <> 0 then begin
            Inc(P, Buf + 1);
            Str[Buf] := '"';
            Str[P - 1] := '"';
            FFormat := FFormat + Copy(Str, Buf, P - Buf)
          end else begin
            P := Length(Str) + 1;
            FFormat := FFormat + Copy(Str, Buf, P - Buf) + '"';
          end;
        end;
        ':': begin // LongTimeFormat always contains ":" instead of TimeSeparator (Delphi bug)
          Inc(P);
          FFormat := FFormat + TimeSeparator;
        end;
      else
        if AnsiCompareText('AMPM', Copy(Str, P, 4)) = 0 then begin
          FFormat := FFormat + 'AM';
          Inc(P, 4);
        end
        else begin
          FFormat := FFormat + UpperCase(Str[P]);
          Inc(P);
        end;
      end;
    end;
    if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then
      FFormat := FFormat + ' TZH:TZM';
  end
  else
    FFormat := AFormat;
end;

function TOraTimeStamp.GetAsDateTime: TDateTime;
var
  Year: smallint;
  Month, Day, Hour, Min, Sec: byte;
  FSec: cardinal;
  Date, Time: TDateTime;
  i: integer;
begin
  if IsNull then begin // to avoid error on OCI function call
    Result := 0;
    Exit;
  end;

  GetDate(Year, Month, Day);
  GetTime(Hour, Min, Sec, FSec);
  Date := EncodeDate(Year, Month, Day);
  for i := 3 to 9 - 1 do
    FSec := FSec div 10;
  Time := EncodeTime(Hour, Min, Sec, FSec);
  if Date < 0 then
    Result := Date - Time
  else
    Result := Date + Time;
end;

procedure TOraTimeStamp.SetAsDateTime(Value: TDateTime);
var
  Year, Month, Day, Hour, Min, Sec, FSec: word;
begin
  DecodeDate(Value, Year, Month, Day);
  DecodeTime(Value, Hour, Min, Sec, FSec);
  Construct(Year, Month, Day, Hour, Min, Sec, 0, '');
end;

{ TOraInterval }

constructor TOraInterval.Create(DataType: word);
begin
  inherited Create;

  phOCIInterval := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(phOCIInterval, nil);
  FIndicator := Marshal.AllocHGlobal(sizeof(OCIInd));
  Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
  hOCIInterval := nil;
  FLeadPrecision := 2;
  FFracPrecision := 6;

  case DataType of
    dtIntervalDS:
      FDescriptorType := OCI_DTYPE_INTERVAL_DS;
    dtIntervalYM:
      FDescriptorType := OCI_DTYPE_INTERVAL_YM;
  else
    Assert(False);
  end;
end;

destructor TOraInterval.Destroy;
begin
  FreeInterval;
  Marshal.FreeHGlobal(FIndicator);
  Marshal.FreeHGlobal(phOCIInterval);

  inherited;
end;

procedure TOraInterval.AllocInterval;
var
 Interval: pOCIInterval;
begin
  if hOCIInterval = nil then begin
    if not OCIInited then
      OCIInit;

    Interval := nil;
    Check(OCIDescriptorAlloc({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, Interval, FDescriptorType, 0, nil));
    hOCIInterval := Interval;
    FNativeHandle := True;
  end;
end;

procedure TOraInterval.FreeInterval;
begin
  if hOCIInterval <> nil then begin
    if FNativeHandle then
      Check(OCIDescriptorFree(hOCIInterval, FDescriptorType));
    hOCIInterval := nil;
    FNativeHandle := False;
    Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
  end;
end;

procedure TOraInterval.Disconnect;
begin
  FreeInterval;
end;

procedure TOraInterval.AssignTo(Dest: TOraInterval);
begin
  Dest.DescriptorType := DescriptorType;
  Dest.AllocInterval;
  Dest.LeadPrecision := LeadPrecision;
  Dest.FracPrecision := FracPrecision;
  Marshal.WriteInt16(Dest.FIndicator, Marshal.ReadInt16(FIndicator));
  if not IsNull then
    Check(OCIIntervalAssign({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIInterval, Dest.OCIInterval));
end;

function TOraInterval.Compare(Dest: TOraInterval): integer;
begin
  Check(OCIIntervalCompare({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIInterval, Dest.OCIInterval, Result));
end;

function TOraInterval.GetIsNull: boolean;
var
  Indicator: OCIInd;
begin
  Indicator := Marshal.ReadInt16(FIndicator);
  Result := Indicator = OCI_IND_NULL;
end;

procedure TOraInterval.SetIsNull(Value: boolean);
begin
  if Value then
    Marshal.WriteInt16(FIndicator, OCI_IND_NULL)
  else
    Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
end;


procedure TOraInterval.SetDescriptorType(const Value: cardinal);
begin
  if FDescriptorType <> Value then begin
    FreeInterval;
    FDescriptorType := Value;
  end;
end;

function TOraInterval.GethOCIInterval: pOCIInterval;
begin
  Result := Marshal.ReadIntPtr(phOCIInterval);
end;

procedure TOraInterval.SethOCIInterval(Value: pOCIInterval);
begin
  Marshal.WriteIntPtr(phOCIInterval, Value);
end;

function TOraInterval.GetOCIInterval: pOCIInterval;
begin
  AllocInterval;
  Result := hOCIInterval;
end;

procedure TOraInterval.SetOCIInterval(const Value: pOCIInterval);
begin
  FreeInterval;
  hOCIInterval := Value;
  if Value <> nil then begin
    FNativeHandle := False;
    IsNull := False;
  end;
end;

function TOraInterval.GetOCIIntervalPtr: ppOCIInterval;
begin
  AllocInterval;
  Result := phOCIInterval;
end;

procedure TOraInterval.CheckValid;
var
  Valid: cardinal;
begin
  Assert(hOCIInterval <> nil);
  Valid := 0;
  Check(OCIIntervalCheck({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, hOCIInterval, Valid));
  if Valid <> 0 then
    raise Exception.Create(SInvalidInterval);
end;

function TOraInterval.GetAsString: string;
var
  Ptr: IntPtr;
  BufSize: size_t;
begin
  if IsNull then begin // to avoid error on OCI function call
    Result := '';
    Exit;
  end;

  BufSize := 255 * SizeOfCharOCI(OCIUnicode);
  Ptr := Marshal.AllocHGlobal(BufSize);
  try
    Check(OCIIntervalToText({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIInterval, LeadPrecision,
      FracPrecision, Ptr, BufSize, BufSize));
    Result := PtrToStringOCI(Ptr, BufSize, OCIUnicode);
  finally
    Marshal.FreeHGlobal(Ptr);
  end;
end;

procedure TOraInterval.SetAsString(const Value: string);
var
  p: IntPtr;
  Res, Size: integer;
begin
  if Value = '' then begin
    Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
    Exit;
  end;

  p := StringToHGlobalOCI(Value, Size, OCIUnicode);
  Res := OCIIntervalFromText({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError,
    p, Size, OCIInterval);
  FreeStringOCI(p, OCIUnicode);
  Check(Res);

  CheckValid;
  Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
end;

procedure TOraInterval.GetDaySecond(var Day, Hour, Min, Sec, FSec: integer);
begin
  Check(OCIIntervalGetDaySecond({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, Day, Hour, Min, Sec,
    FSec, OCIInterval));
end;

procedure TOraInterval.Init;
var
  i: int64;
  pNum: IntPtr;
begin
  // Need to correctly initialize OCIInterval before SetDaySecond or SetYearMonth call.
  i := 0;
  pNum := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
  try
    Check(OCINumberFromInt(hOCIError, i, SizeOf(i), OCI_NUMBER_UNSIGNED, pNum));
    Check(OCIIntervalFromNumber({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, OCIInterval, pNum));
  finally
    Marshal.FreeHGlobal(pNum);
  end;
end;

procedure TOraInterval.SetDaySecond(Day, Hour, Min, Sec, FSec: integer);
begin
  Init;
  Check(OCIIntervalSetDaySecond({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, Day, Hour, Min, Sec, FSec,
    OCIInterval));
  Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
end;

procedure TOraInterval.GetYearMonth(var Year, Month: integer);
begin
  Check(OCIIntervalGetYearMonth({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, Year, Month, OCIInterval));
end;

procedure TOraInterval.SetYearMonth(Year, Month: integer);
begin
  Init;
  Check(OCIIntervalSetYearMonth({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIEnv, hOCIError, Year, Month, OCIInterval));
  Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
end;

{ TOraNumber }

constructor TOraNumber.Create;
begin
  inherited Create;

  phOCINumber := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
  FillChar(phOCINumber, OCI_NUMBER_SIZE, 0);
  FIndicator := Marshal.AllocHGlobal(sizeof(OCIInd));
  IsNull := True;
  FNativeHandle := True;
end;

destructor TOraNumber.Destroy;
begin
  Marshal.FreeHGlobal(FIndicator);
  if FNativeHandle then //To support SetOCINumberPtr assignation
    Marshal.FreeHGlobal(phOCINumber);

  inherited;
end;

procedure TOraNumber.AssignTo(Dest: TOraNumber);
begin
  Dest.IsNull := IsNull;
  if not IsNull then
    Check(OCINumberAssign(hOCIError, OCINumberPtr, Dest.OCINumberPtr));
end;

function TOraNumber.Compare(Dest: TOraNumber): integer;
begin
  Check(OCINumberCmp ( hOCIError, OCINumberPtr, Dest.OCINumberPtr, Result));
end;

function TOraNumber.GetOCINumberPtr: pOCINumber;
begin
  Result := phOCINumber;
end;

procedure TOraNumber.SetOCINumberPtr(Value: pOCINumber);
begin
  if Value <> phOCINumber then begin
    if FNativeHandle then
      Marshal.FreeHGlobal(phOCINumber);
    FNativeHandle := False;
    phOCINumber := Value;
    IsNull := False;
  end;
end;

const
  MAX_NUMBER_TEXT_SIZE = 64;
  SNlsParams = 'NLS_NUMERIC_CHARACTERS=''%s%s''';

function TOraNumber.GetOCINumber: OCINumber;
begin
{$IFDEF CLR}
  Marshal.Copy(phOCINumber, TBytes(Result.OCINumberPart), 0, OCI_NUMBER_SIZE);
{$ELSE}
  Move(phOCINumber^, Result.OCINumberPart[0], OCI_NUMBER_SIZE);
{$ENDIF}
end;

procedure TOraNumber.SetOCINumber(Value: OCINumber);
begin
{$IFDEF CLR}
  Marshal.Copy(TBytes(Value.OCINumberPart), 0, phOCINumber, OCI_NUMBER_SIZE);
{$ELSE}
  Move(Value.OCINumberPart[0], phOCINumber^, OCI_NUMBER_SIZE);
{$ENDIF}
  IsNull := False;
end;

function TOraNumber.GetAsString: string;
var
  BufSize: cardinal;
  NlsParams: string;
  Ptr, pF, pN: IntPtr;
  FSize, NSize: integer;
begin
  Result := '';
  if IsNull then
    Exit;

  if OCIVersion < 8100 then begin
    Result := FormatFloat('0.#', GetAsFloat);
    Exit;
  end;

  NlsParams := Format(SNlsParams, [DecimalSeparator, ThousandSeparator]);

  BufSize := MAX_NUMBER_TEXT_SIZE * SizeOfCharOCI(OCIUnicode);
  Ptr := Marshal.AllocHGlobal(BufSize);
  pF := StringToHGlobalOCI('TM9', FSize, OCIUnicode);
  pN := StringToHGlobalOCI(NlsParams, NSize, OCIUnicode);
  try
    Check(OCINumberToText(hOCIError, OCINumberPtr, pF, FSize, pN, NSize, BufSize, Ptr));
    Result := PtrToStringOCI(Ptr, BufSize, OCIUnicode);
  finally
    Marshal.FreeHGlobal(Ptr);
    FreeStringOCI(pF, OCIUnicode);
    FreeStringOCI(pN, OCIUnicode);
  end;

  if Length(Result) > 0 then begin
    if (Result[1] = DecimalSeparator) then
      Result := '0' + Result
    else
      if (Result[1] = '-') and (Result[2] = DecimalSeparator) then
        Result := '-0' + Copy(Result, 2, MaxInt)
  end;
end;

procedure TOraNumber.SetAsString(const Value: string);
var
  FormatStr: string;
  NlsParams: string;

  function GetFormatString: string;
  const
    SScientificFormat = '9D99999999999999999999999999999999999999999EEEE';
  var
    i: integer;
    Signed: boolean;
    FractionalPart: boolean;
    HasDigits: boolean;
  begin
    if Length(Value) > 0 then begin
      SetLength(Result, Length(Value));

      Signed := (Value[1] = '+') or (Value[1] = '-');
      FractionalPart := False;
      HasDigits := False;

      if Signed then begin
        Result[1] := 'S';
        i := 2;
      end
      else
        i := 1;

      while i <= Length(Value) do begin
        if (Value[i] = DecimalSeparator) and not FractionalPart then begin
          Result[i] := 'D';
          FractionalPart := True;
        end
        else
        if (Value[i] = ThousandSeparator) and HasDigits and not FractionalPart then
          Result[i] := 'G'
        else
        if (Value[i] = 'e') or (Value[i] = 'E') then begin
          if Signed then
            Result := 'S' + SScientificFormat
          else
            Result := SScientificFormat;
          Exit;
        end
        else begin
          Result[i] := '9';
          HasDigits := True;
        end;
        Inc(i);
      end;
      if Length(Result) >= MAX_NUMBER_TEXT_SIZE then
        if Signed then
          SetLength(Result, MAX_NUMBER_TEXT_SIZE)
        else
          SetLength(Result, MAX_NUMBER_TEXT_SIZE - 1);
    end
    else
      Result := '';
  end;

var
  pV, pF, pN: IntPtr;
  Res, VSize, FSize, NSize: integer;
begin
  if Value = '' then begin
    Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
    Exit;
  end;
  FillChar(OCINumberPtr, sizeof(OCI_NUMBER_SIZE), 0);
  FormatStr := GetFormatString;
  NlsParams := Format(SNlsParams, [DecimalSeparator, ThousandSeparator]);

  pV := StringToHGlobalOCI(Value, VSize, OCIUnicode);
  pF := StringToHGlobalOCI(FormatStr, FSize, OCIUnicode);
  pN := StringToHGlobalOCI(NlsParams, NSize, OCIUnicode);
  Res := OCINumberFromText(hOCIError, pV, VSize, pF, FSize, pN, NSize, OCINumberPtr);
  FreeStringOCI(pV, OCIUnicode);
  FreeStringOCI(pF, OCIUnicode);
  FreeStringOCI(pN, OCIUnicode);
  Check(Res);

  IsNull := False;
end;

function TOraNumber.GetAsInteger: integer;
begin
  Result := Integer(GetAsLargeInt);
end;

procedure TOraNumber.SetAsInteger(const Value: integer);
begin
  SetAsLargeInt(Value);
end;

function TOraNumber.GetAsLargeInt: int64;
begin
  if IsNull then
    Result := 0
  else
    Check(OCINumberToInt(hOCIError, OCINumberPtr, sizeof(int64), OCI_NUMBER_SIGNED, Result));
end;

procedure TOraNumber.SetAsLargeInt(const Value: int64);
var
  Val: int64;
begin
  Val := Value;
  Check(OCINumberFromInt(hOCIError, Val, sizeof(int64), OCI_NUMBER_SIGNED, OCINumberPtr));
  IsNull := False;
end;

function TOraNumber.GetAsFloat: double;
begin
  if IsNull then
    Result := 0
  else
    Check(OCINumberToReal(hOCIError, OCINumberPtr, sizeof(double), Result));
end;

procedure TOraNumber.SetAsFloat(const Value: double);
var
  Val: double;
begin
  Val := Value;
  Check(OCINumberFromReal(hOCIError, Val, sizeof(double), OCINumberPtr));
  IsNull := False;
end;

{$IFDEF VER6P}
function TOraNumber.GetAsBCD: TBCD;
{$IFDEF CLR}
var
  Val: TBytes;
{$ENDIF}
begin
  if IsNull then
    Result := NullBcd
  else begin
  {$IFDEF CLR}
    SetLength(Val, SizeOfTBCD);
    OCINumberToBCD(OCINumberPtr, Val);
    Result := TBcd.FromBytes(Val);
  {$ELSE}
    OCINumberToBCD(OCINumberPtr, Result);
  {$ENDIF}
  end;
end;

procedure TOraNumber.SetAsBCD(const Value: TBCD);
// OCINumberFromBCD causes problems in OCI
(*{$IFDEF NET}
var
  Val: TBytes;
{$ENDIF}*)
begin
//{$IFNDEF NET}
  AsString := BcdToStr(Value);
(*{$ELSE}
{$IFDEF CLR}
  Val := TBcd.ToBytes(Value);
{$ELSE}
  SetLength(Val, SizeOfTBCD);
  Move(Value, Val[0], SizeOfTBCD);
{$ENDIF}
  Check(OCINumberFromBCD(hOCIError, Val, SizeOfTBCD, OCINumberPtr));
{$ENDIF}*)
  IsNull := False;
end;
{$ENDIF}

function TOraNumber.GetIsNull: boolean;
begin
  Result := Marshal.ReadInt16(FIndicator) = OCI_IND_NULL;
end;

procedure TOraNumber.SetIsNull(Value: boolean);
begin
  if Value then
    Marshal.WriteInt16(FIndicator, OCI_IND_NULL)
  else
    Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
end;

{TOCITransaction}

constructor TOCITransaction.Create;
begin
  inherited;
end;

destructor TOCITransaction.Destroy;
begin
{$IFNDEF LITE}
  if FXID <> nil then
    Marshal.FreeHGlobal(FXID);
{$ENDIF}

  inherited;
end;

procedure TOCITransaction.Check(Status: sword);
var
  Connection: TOCIConnection;
begin
  if Status <> OCI_SUCCESS then
    if FConnections.Count = 1 then begin
      Connection := TOCIConnection(FConnections[0]);
      Connection.OraError(Connection.FOCICallStyle, Status, True, Component);
    end
    else
      OraError(Status, True);
end;

procedure TOCITransaction.OraError(var ErrorCode: sword; UseCallback: boolean);
var
  Msg: _string;
  Code: sword;
  Fail: boolean;
begin
  Code := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.GetOraError(ErrorCode, OCIUnicode, Msg, hOCIError);
  if (ErrorCode = OCI_SUCCESS_WITH_INFO) and (Code <> 24344) then  // except compilation error
    Exit;
  ErrorCode := Code;

  try
    raise EOraError.Create(ErrorCode, Msg);
  except
    on E: EOraError do begin
      Fail := True;
      if UseCallback then
        if Assigned(FOnError) then
          FOnError(E, Fail);
      if Fail then
        raise
      else
        Abort;
    end;
  end;
end;

function TOCITransaction.SetProp(Prop: integer; const Value: variant): boolean;
begin
{$IFNDEF LITE}
  Result := True;
  case Prop of
    prInactiveTimeout:
      FInactiveTimeout := Value;
    prResumeTimeout:
      FResumeTimeout := Value;
    prTransactionName:
      FTransactionName := Value;
    prTransactionResume:
      FResume := Value;
    prRollbackSegment:
      FRollbackSegment := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
{$ELSE}
  Result := inherited SetProp(Prop, Value);
{$ENDIF}
end;

function TOCITransaction.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited SetProp(Prop, Value);
end;

{$IFNDEF LITE}
procedure TOCITransaction.SetTransactionId(TransactionId: TBytes);
begin
  FTransactionId := Copy(TransactionId, 0, Length(TransactionId));
end;

procedure TOCITransaction.SetBranchQualifier(Index: integer; BranchQualifier: TBytes);
begin
  if Length(FTransactionLinks) <= Index then
    SetLength(FTransactionLinks, Index + 1);
  FTransactionLinks[Index].BranchQualifier := Copy(BranchQualifier, 0, Length(BranchQualifier));
end;

procedure TOCITransaction.WriteTransactionId;
var
  i, GtridLen: integer;
begin
  GtridLen := Length(FTransactionId);
  if GtridLen > MaxTransactionIdLength then
    raise Exception.CreateFmt(STransactionIdTooLong, ['TransactionId']);

  if FXID = nil then
    FXID := Marshal.AllocHGlobal(XID_SIZE);

  Marshal.WriteInt32(FXID, 0, 0);        // FormatID
  Marshal.WriteInt32(FXID, 4, GtridLen); // Gtrid_length
  for i := 0 to GtridLen - 1 do
    Marshal.WriteByte(FXID, 12 + i, FTransactionId[i]);
end;

procedure TOCITransaction.WriteBranchQualifier(TransactionLink: TTransactionLink);
var
  BranchQualifier: TBytes;
  i, GtridLen, BqualLen: integer;
begin
  BranchQualifier := TransactionLink.BranchQualifier;
  BqualLen := Length(BranchQualifier);
  if BqualLen > MaxTransactionIdLength then
    raise Exception.CreateFmt(STransactionIdTooLong, ['BranchQualifier']);

  Marshal.WriteInt32(FXID, 8, BqualLen); // Bqual_length
  GtridLen := Length(FTransactionId);
  for i := 0 to BqualLen - 1 do
    Marshal.WriteByte(FXID, 12 + GtridLen + i, BranchQualifier[i]);
end;

procedure TOCITransaction.FreeTransaction;
var
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do
    if FTransactionLinks[i].State = tsFinished then begin
      Check(OCIAttrSet1(TOCIConnection(FConnections[i]).GetSvcCtx, OCI_HTYPE_SVCCTX,
        nil, 0, OCI_ATTR_TRANS, hOCIError));
      Check(OCIHandleFree(FTransactionLinks[i].OCITrans, OCI_HTYPE_TRANS));
      FTransactionLinks[i].State := tsInactive;
    end;
  FActive := False;
end;
{$ENDIF}

procedure TOCITransaction.StartTransactionLocal;
{$IFNDEF LITE}
var
  S: _string;
  Connection: TOCIConnection;
  Command: TOCICommand;
{$ENDIF}
begin
  CommitLocal;

{$IFNDEF LITE}
  Connection := TOCIConnection(FConnections[0]);
  Command := Connection.GetCommand;

  if FRollbackSegment <> '' then begin
    S := 'SET TRANSACTION USE ROLLBACK SEGMENT '+ FRollbackSegment;
    if FTransactionName <> '' then
      S := S + ' NAME ''' + FTransactionName + '''';
    Command.SetSQL('begin ' + S + '; end;');
    Command.Execute;
  end;

  if FReadOnly then begin
    S := 'SET TRANSACTION READ ONLY';
    if FTransactionName <> '' then
      S := S + ' NAME ''' + FTransactionName + '''';
    Command.SetSQL('begin ' + S + '; end;');
    Command.Execute;
  end
  else
  if FIsolationLevel = ilSnapshot then begin
    S := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
    if FTransactionName <> '' then
      S := S + ' NAME ''' + FTransactionName + '''';
    Command.SetSQL('begin ' + S + '; end;');
    Command.Execute;
  end
  else
  if not (FIsolationLevel = ilReadCommitted) then
    RaiseError(SUnsupportedIsolationLevel);

  if (FIsolationLevel = ilReadCommitted) and (FRollbackSegment = '') and
    (FTransactionName <> '')
  then begin
    Command.SetSQL('begin SET TRANSACTION NAME ''' + FTransactionName + '''; end;');
    Command.Execute;
  end;
{$ENDIF}

  FLocalTransactionId := LocalTransactionId(True); //Server side transaction starts here
  FActive := True;
  FNativeTransaction := True;
end;

procedure TOCITransaction.CommitLocal;
var
  Connection: TOCIConnection;
begin
  Connection := TOCIConnection(FConnections[0]);
  Connection.BusyWait;
  try
    FLocalTransactionId := '';
    FActive := False;
    if FNativeTransaction then begin
      if Connection.FOCICallStyle = OCI73 then begin
        Check(ocom(Connection.LDA));
      end
      else
      if Connection.FOCICallStyle = OCI80 then begin
        Check(OCITransCommit(Connection.hSvcCtx, Connection.hOCIError, OCI_DEFAULT));
      end
      else
        CheckOCI;
    end;
    FNativeTransaction := True;
  finally
    Connection.Release;
  end;
end;

procedure TOCITransaction.RollbackLocal;
var
  Connection: TOCIConnection;
begin
  Connection := TOCIConnection(FConnections[0]);
  FLocalTransactionId := '';
  FActive := False;
  if FNativeTransaction then begin
    if Connection.FOCICallStyle = OCI73 then begin
      Check(orol(Connection.LDA));
    end
    else
    if Connection.FOCICallStyle = OCI80 then begin
      Check(OCITransRollback(Connection.hSvcCtx, Connection.hOCIError, OCI_DEFAULT));
    end
    else
      CheckOCI;
  end;
  FNativeTransaction := True;
end;

procedure TOCITransaction.Savepoint(const Name: _string);
var
  Connection: TOCIConnection;
  Command: TOCICommand;
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    Connection := TOCIConnection(FConnections[i]);
    Command := Connection.GetCommand;
    Command.SetSQL('SAVEPOINT ' + Name);
    Command.Execute;
  end;
end;

procedure TOCITransaction.RollbackToSavepoint(const Name: _string);
var
  Connection: TOCIConnection;
  Command: TOCICommand;
  i: integer;
begin
  for i := 0 to FConnections.Count - 1 do begin
    Connection := TOCIConnection(FConnections[i]);
    Command := Connection.GetCommand;
    Command.SetSQL('ROLLBACK TO SAVEPOINT ' + Name);
    Command.Execute;
  end;
end;

function TOCITransaction.DetectInTransaction(CanActivate: boolean): boolean;
var
  NewID: string;
begin
  if not FActive then begin
    if CanActivate then begin
      FLocalTransactionId := LocalTransactionId;
      FActive := FLocalTransactionId <> '';
    end;
  end
  else
  if FLocalTransactionId <> '' then begin
    NewID := LocalTransactionId;
    FActive := NewID = FLocalTransactionId;
    if not FActive then begin
      // transaction was implicitly ended (by server-side logic)
      FLocalTransactionId := '';
    end;
  end;
  Result := FActive;
end;

procedure TOCITransaction.AssignConnect(Source: TCRTransaction);
begin
  inherited;

  FLocalTransactionId := TOCITransaction(Source).FLocalTransactionId;
end;

function TOCITransaction.LocalTransactionId(CreateTransaction: boolean = False): string;
var
  SqlLine: _string;
  Connection: TOCIConnection;
  Command: TOCICommand;
begin
  //we couldn't use here SQL couse of recursive calls in AfterExecute in DisconnectedMode
  Connection := TOCIConnection(FConnections[0]);
  // Connection.BusyWait - fix bug with multi thread applications
  Connection.BusyWait;
  try
    Command := Connection.GetCommand;
    SqlLine := '  :result := sys.dbms_transaction.local_transaction_id';
    if CreateTransaction then
      SqlLine := SqlLine + '(true)'; //call local_transaction_id(true) - to create transaction if there is no any
    SqlLine := SqlLine + '; ';
    Command.SetSQL('begin' + SqlLine + 'end;');
    with TOraParamDesc(Command.GetParam(0)) do begin
      SetDataType(dtString);
      SetParamType(pdOutput);
      SetSize(4000);
    end;
    Command.Execute;

    Result := string(TOraParamDesc(Command.GetParam(0)).GetItemAsAnsiString(1));
  finally
    Connection.Release;
  end;
end;

procedure TOCITransaction.StartTransaction;
{$IFNDEF LITE}
var
  i, Size, Res: integer;
  TransactionFlags: integer;
  Connection: TOCIConnection;
  p: IntPtr;
{$ENDIF}
begin
{$IFNDEF LITE}
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  if (FConnections.Count > 1) and (FTransactionName = '') and (FTransactionId = nil) then
    raise Exception.Create(SCannotStartTransactionWithoutId);

  for i := 0 to FConnections.Count - 1 do
    if not FConnections[i].GetConnected then
      raise Exception.Create(SConnectionInTransactionNotActive);

  if (FConnections.Count = 1) and (FTransactionId = nil) then begin
{$ENDIF}
    StartTransactionLocal;
{$IFNDEF LITE}
    exit;
  end;

  CheckInactive;

  if (OCICallStyle = OCI80) and not (OCI73 in PossibleOCICallStyles) then
    raise Exception.Create(STransactionNotSupportedWithDirect);

  CheckOCI80;

  SetLength(FTransactionLinks, FConnections.Count);

  if FTransactionId <> nil then
    WriteTransactionId;

  if FReadOnly then begin
    TransactionFlags := OCI_TRANS_READONLY;
  end
  else
    case FIsolationLevel of
      ilReadCommitted:
        TransactionFlags := OCI_TRANS_READWRITE;
      ilSnapshot:
        TransactionFlags := OCI_TRANS_SERIALIZABLE;
    else
      raise Exception.Create(SUnsupportedIsolationLevel);
    end;

  FActive := True;
  FLocalTransactionId := '';
  try
    for i := 0 to FConnections.Count - 1 do begin
      Connection := TOCIConnection(FConnections[i]);
      with FTransactionLinks[i] do begin
        Check(OCIHandleAlloc(hOCIEnv, OCITrans, OCI_HTYPE_TRANS, 0, nil));
        try
          if FTransactionId <> nil then begin
            WriteBranchQualifier(FTransactionLinks[i]);
            Check(OCIAttrSet1(OCITrans, OCI_HTYPE_TRANS,
              FXID, XID_SIZE, OCI_ATTR_XID, hOCIError));
          end
          else
            if FTransactionName <> '' then begin
              p := StringToHGlobalOCI(FTransactionName, Size, OCIUnicode);
              Res := OCIAttrSet1(OCITrans, OCI_HTYPE_TRANS, p, Size, OCI_ATTR_TRANS_NAME, hOCIError);
              FreeStringOCI(p, OCIUnicode);
              Check(Res);
            end;

            Check(OCIAttrSet1(Connection.GetSvcCtx, OCI_HTYPE_SVCCTX,
              OCITrans, 0, OCI_ATTR_TRANS, hOCIError));

          if FResume then
            Check(OCITransStart(Connection.GetSvcCtx, hOCIError, FResumeTimeOut,
              TransactionFlags or OCI_TRANS_RESUME))
          else
            Check(OCITransStart(Connection.GetSvcCtx, hOCIError, FInactiveTimeOut,
              TransactionFlags or OCI_TRANS_NEW));
        except
          Check(OCIAttrSet1(Connection.GetSvcCtx, OCI_HTYPE_SVCCTX,
            nil, 0, OCI_ATTR_TRANS, hOCIError));

          Check(OCIHandleFree(OCITrans, OCI_HTYPE_TRANS));
          raise;
        end;
        State := tsActive;
      end;
    end;
  except // detach or rollback successfully started branches
    if FResume then
      Detach
    else
      Rollback;
    raise;
  end;
{$ENDIF}
end;

procedure TOCITransaction.Commit;
{$IFNDEF LITE}
var
  i, Status: integer;
{$ENDIF}
begin
{$IFNDEF LITE}
  if (FConnections.Count = 1) and (FTransactionId = nil) then begin
{$ENDIF}
    CommitLocal;
{$IFNDEF LITE}
    exit;
  end;

  CheckActive;

  if FConnections.Count > 1 then begin
    for i := 0 to FConnections.Count - 1 do
      if FTransactionLinks[i].State = tsActive then begin
        Status := OCITransPrepare(TOCIConnection(FConnections[i]).GetSvcCtx, hOCIError, OCI_DEFAULT);
        case Status of
          OCI_SUCCESS:
            FTransactionLinks[i].State := tsPrepared;
          OCI_SUCCESS_WITH_INFO:
            FTransactionLinks[i].State := tsFinished;
          else begin
            RollBack;
            Check(Status);
          end;
        end;
      end;
    for i := 0 to FConnections.Count - 1 do
      if FTransactionLinks[i].State = tsPrepared then begin
        Check(OCITransCommit(TOCIConnection(FConnections[i]).GetSvcCtx, hOCIError, OCI_TRANS_TWOPHASE));
        FTransactionLinks[i].State := tsFinished;
      end;
  end
  else
    if FTransactionLinks[0].State = tsActive then begin
      Check(OCITransCommit(TOCIConnection(FConnections[0]).GetSvcCtx, hOCIError, OCI_DEFAULT));
      FTransactionLinks[0].State := tsFinished;
    end;

  FreeTransaction;
{$ENDIF}
end;

procedure TOCITransaction.Rollback;
{$IFNDEF LITE}
var
  i: integer;
{$ENDIF}
begin
{$IFNDEF LITE}
  if (FConnections.Count = 1) and (FTransactionId = nil) then begin
{$ENDIF}
    RollbackLocal;
{$IFNDEF LITE}
    exit;
  end;

  CheckActive;

  for i := 0 to FConnections.Count - 1 do
    if FTransactionLinks[i].State in [tsActive, tsPrepared] then begin
      Check(OCITransRollback(TOCIConnection(FConnections[i]).GetSvcCtx, hOCIError, OCI_DEFAULT));
      FTransactionLinks[i].State := tsFinished;
    end;

  FreeTransaction;
{$ENDIF}
end;

{$IFNDEF LITE}
procedure TOCITransaction.Detach;
var
  i: integer;
begin
  CheckActive;

  for i := 0 to FConnections.Count - 1 do
    if FTransactionLinks[i].State = tsActive then begin
      Check(OCITransDetach(TOCIConnection(FConnections[i]).GetSvcCtx, hOCIError, OCI_DEFAULT));
      FTransactionLinks[i].State := tsFinished;
    end;
  FreeTransaction;
end;
{$ENDIF}

{$IFNDEF LITE}

{ TOCIMetaData }

function TOCIMetaData.GetTypesForSQL(const ObjectTypes: _string; AllTypes: array of _string): _string;
var
  i: integer;
  Res: TBooleanArray;
begin
  Res := ParseTypes(ObjectTypes, AllTypes);
  Result := '';
  for i := 0 to High(AllTypes) do
    if Res[i] then begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + '''' + AllTypes[i] + '''';
    end;
  if Result = '' then
    Result := '''''';
end;

function TOCIMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TOCIRecordSet.Create;
  Result.GetCommand.SetProp(prIntegerPrecision, 38);
  Result.GetCommand.SetProp(prUseDefaultDataTypes, True);
end;

function TOCIMetaData.InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData;
begin
  if MetaDataKind = 'sequences' then
    Result := GetSequences(Restrictions)
  else
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions);
end;

procedure TOCIMetaData.InternalGetMetaDataKindsList(List: _TStringList);
begin
  inherited;

  List.Add('DataTypes');
  List.Add('Users');
  List.Add('UserDefinedTypes');
  List.Add('Packages');
  List.Add('Sequences');

  List.Sort;
end;

procedure TOCIMetaData.InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string);
begin
  List.Clear;

  if MetaDataKind = 'procedures' then begin
    List.Add('PROCEDURE_CATALOG');
    List.Add('PROCEDURE_SCHEMA');
    List.Add('PROCEDURE_PACKAGE');
    List.Add('PROCEDURE_NAME');
    List.Add('PROCEDURE_TYPE');
  end
  else
  if MetaDataKind = 'procedureparameters' then begin
    List.Add('PROCEDURE_CATALOG');
    List.Add('PROCEDURE_SCHEMA');
    List.Add('PROCEDURE_PACKAGE');
    List.Add('PROCEDURE_NAME');
    List.Add('PARAMETER_NAME');
  end
  else
  if MetaDataKind = 'packages' then begin
    List.Add('PACKAGE_CATALOG');
    List.Add('PACKAGE_SCHEMA');
    List.Add('PACKAGE_NAME');
  end
  else
  if MetaDataKind = 'users' then begin
    List.Add('USER_NAME');
  end
  else
  if MetaDataKind = 'userdefinedtypes' then begin
    List.Add('TYPE_CATALOG');
    List.Add('TYPE_SCHEMA');
    List.Add('TYPE_NAME');
  end
  else
  if MetaDataKind = 'sequences' then begin
    List.Add('SEQUENCE_CATALOG');
    List.Add('SEQUENCE_SCHEMA');
    List.Add('SEQUENCE_NAME');
  end
  else
    inherited;
end;

function TOCIMetaData.GetTables(Restrictions: _TStrings): TData;
const
  fmtGetTablesSQL = 'SELECT ' +
    ''''' TABLE_CATALOG, OWNER TABLE_SCHEMA, OBJECT_NAME TABLE_NAME, ' +
    'OBJECT_TYPE TABLE_TYPE, CREATED, LAST_DDL_TIME ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE IN (%s) ORDER BY OWNER, OBJECT_NAME ';
var
  WhereClause, Schema, TableName, TableTypes, QuotedTypes, Scope: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema);
  AddWhere(WhereClause, 'OBJECT_NAME', TableName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  QuotedTypes := GetTypesForSQL(TableTypes, ['TABLE', 'VIEW', 'SYNONYM']);

  FRecordSet.SetSQL(_Format(fmtGetTablesSQL, [WhereClause, QuotedTypes]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetColumns(Restrictions: _TStrings): TData;
const
  fmtGetColumnsSQL = 'SELECT ' +
    ''''' TABLE_CATALOG, OWNER TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, COLUMN_ID POSITION, ' +
    'DATA_TYPE, DATA_LENGTH, DATA_PRECISION, DATA_SCALE, ' +
    'DECODE(NULLABLE, ''Y'', 1, 0) NULLABLE, DATA_TYPE_MOD, DATA_TYPE_OWNER ' +
    'FROM SYS.ALL_TAB_COLUMNS %s ORDER BY OWNER, TABLE_NAME, COLUMN_ID ';
var
  WhereClause, Schema, TableName, ColumnName: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'OWNER', Schema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'COLUMN_NAME', ColumnName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetProcedures(Restrictions: _TStrings): TData;
const
  fmtGetProceduresSQL = 'SELECT ' +
    ''''' PROCEDURE_CATALOG, OWNER PROCEDURE_SCHEMA, OBJECT_NAME PROCEDURE_NAME, ' +
    'OBJECT_TYPE PROCEDURE_TYPE, '''' PROCEDURE_PACKAGE, CREATED, LAST_DDL_TIME, STATUS, NULL OVERLOAD ' +
    'FROM SYS.ALL_OBJECTS WHERE %s OBJECT_TYPE IN (%s) ';
  fmtGetPackageProceduresSQL = 'SELECT ' +
    ''''' PROCEDURE_CATALOG, OWNER PROCEDURE_SCHEMA, ' +
    'OBJECT_NAME PROCEDURE_NAME, ' +
    'DECODE(POSITION, 0, ''FUNCTION'', ''PROCEDURE'') PROCEDURE_TYPE, ' +
    'PACKAGE_NAME PROCEDURE_PACKAGE, ' +
    'TO_DATE(NULL) CREATED, TO_DATE(NULL) LAST_DDL_TIME, '''' STATUS, ' +
    'OVERLOAD ' +
    'FROM ( ' +
    'SELECT OWNER, PACKAGE_NAME, OBJECT_NAME, OVERLOAD, MIN(POSITION) POSITION ' +
    'FROM SYS.ALL_ARGUMENTS WHERE %s DATA_LEVEL = 0 ' +
    'GROUP BY OWNER, PACKAGE_NAME, OBJECT_NAME, OVERLOAD ' +
    ') %s';
var
  WhereClause, Schema, Package, ProcName, ProcTypes, QuotedTypes, Str, SQL, Scope: _string;
  BoolTypes: TBooleanArray;
begin
  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  Package := Trim(Restrictions.Values['PROCEDURE_PACKAGE']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ProcTypes := Trim(Restrictions.Values['PROCEDURE_TYPE']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema);

  AddWhere(WhereClause, 'OBJECT_NAME', ProcName);

  BoolTypes := nil;

  if (Package = '') or (Package = '""') then begin
    Str := WhereClause;
    if Str <> '' then
      Str := Str + ' AND ';

    QuotedTypes := GetTypesForSQL(ProcTypes, ['PROCEDURE', 'FUNCTION']);

    SQL := _Format(fmtGetProceduresSQL, [Str, QuotedTypes]);
  end;

  if (Package = '') or (Package <> '""') then begin
    if Package <> '' then
      AddWhere(WhereClause, 'PACKAGE_NAME', Package)
    else begin
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      WhereClause := WhereClause + 'PACKAGE_NAME is not NULL';
    end;
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';

    BoolTypes := ParseTypes(ProcTypes, ['PROCEDURE', 'FUNCTION']);
    if BoolTypes[0] or BoolTypes[1] then begin
      if not (BoolTypes[0] and BoolTypes[1]) then begin
        if BoolTypes[0] then
          Str := '1'
        else
          Str := '0';
        QuotedTypes := 'WHERE POSITION = ' + Str;
      end
      else
        QuotedTypes := '';
    end
    else
      QuotedTypes := 'WHERE 1 = 0';

    if SQL <> '' then
      SQL := SQL + LineSeparator + 'UNION ALL' + LineSeparator;
    SQL := SQL + _Format(fmtGetPackageProceduresSQL, [WhereClause, QuotedTypes]);
  end;

  SQL := SQL + LineSeparator + 'ORDER BY PROCEDURE_SCHEMA, PROCEDURE_PACKAGE, PROCEDURE_NAME';

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetProcedureParameters(Restrictions: _TStrings): TData;
const
  fmtGetProcedureParametersSQL = 'SELECT ' +
    ''''' PROCEDURE_CATALOG, OWNER PROCEDURE_SCHEMA, OBJECT_NAME PROCEDURE_NAME, ' +
    'ARGUMENT_NAME PARAMETER_NAME, POSITION, IN_OUT DIRECTION, ' +
    'DATA_TYPE, DATA_LENGTH, DATA_PRECISION, DATA_SCALE, ' +
    'TYPE_OWNER, TYPE_NAME, TYPE_SUBNAME, TYPE_LINK ' +
    'FROM SYS.ALL_ARGUMENTS ' +
    'WHERE %s AND DATA_LEVEL = 0 AND (ARGUMENT_NAME IS NOT NULL OR POSITION = 0) ' +
    'ORDER BY OWNER, OBJECT_NAME, OVERLOAD, POSITION';
var
  WhereClause, Schema, Package, ProcName, ParamName: _string;
begin
  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  Package := Trim(Restrictions.Values['PROCEDURE_PACKAGE']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ParamName := Trim(Restrictions.Values['PARAMETER_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'OWNER', Schema);
  AddWhere(WhereClause, 'PACKAGE_NAME', Package, True);
  AddWhere(WhereClause, 'OBJECT_NAME', ProcName);
  AddWhere(WhereClause, 'ARGUMENT_NAME', ParamName);

  FRecordSet.SetSQL(_Format(fmtGetProcedureParametersSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetIndexes(Restrictions: _TStrings): TData;
const
  fmtGetIndexesSQL =
    'SELECT '''' TABLE_CATALOG, ' +
    '       I.TABLE_OWNER TABLE_SCHEMA, ' +
    '       I.TABLE_NAME, ' +
    '       '''' INDEX_CATALOG, ' +
    '       I.OWNER INDEX_SCHEMA, ' +
    '       I.INDEX_NAME, ' +
    '       DECODE(I.UNIQUENESS, ''UNIQUE'', 1, 0) "UNIQUE", ' +
    '       I.INDEX_TYPE, ' +
    '       O.CREATED, ' +
    '       O.LAST_DDL_TIME, ' +
    '       I.STATUS ' +
    'FROM (select TABLE_OWNER, ' +
    '             TABLE_NAME, ' +
    '             OWNER, ' +
    '             INDEX_NAME, ' +
    '             UNIQUENESS, ' +
    '             INDEX_TYPE, ' +
    '             STATUS ' +
    '      from ALL_INDEXES ' +
    '      where %s) I, ' +
    '      (select OBJECT_NAME, ' +
    '              OWNER,' +
    '              CREATED, ' +
    '              LAST_DDL_TIME ' +
    '       from SYS.ALL_OBJECTS ' +
    '       where OBJECT_TYPE = ''INDEX'') O ' +
    'WHERE I.INDEX_NAME = O.OBJECT_NAME AND ' +
    '      I.OWNER = O.OWNER ' +
    'ORDER BY I.OWNER,  ' +
    '         I.TABLE_OWNER, ' +
    '         I.TABLE_NAME, ' +
    '         I.INDEX_NAME';
var
  WhereClause, IndexSchema, TableName, TableSchema, IndexName: _string;
  SQL: _string;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexSchema := Trim(Restrictions.Values['INDEX_SCHEMA']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'OWNER', IndexSchema);
  AddWhere(WhereClause, 'TABLE_OWNER', TableSchema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'INDEX_NAME', IndexName);

  SQL := _Format(fmtGetIndexesSQL, [WhereClause]);
  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  
  Result := FRecordSet;
end;

function TOCIMetaData.GetIndexColumns(Restrictions: _TStrings): TData;
const
  fmtGetIndexColumnsSQL = 'SELECT ' +
    ''''' TABLE_CATALOG, TABLE_OWNER TABLE_SCHEMA, TABLE_NAME, ' +
    ''''' INDEX_CATALOG, INDEX_OWNER INDEX_SCHEMA, INDEX_NAME, ' +
    'COLUMN_NAME, COLUMN_POSITION, %s DESCENDING ' +
    'FROM SYS.ALL_IND_COLUMNS IC ' +
    '%s ORDER BY INDEX_OWNER, TABLE_OWNER, TABLE_NAME, INDEX_NAME, COLUMN_POSITION';

  // Index for Primary Key must be first, but for Oracle 805 it is impossible
  fmtSQLWithIndexTypeOld = 'SELECT ' +
    ''''' TABLE_CATALOG, IC.TABLE_OWNER TABLE_SCHEMA, IC.TABLE_NAME, ' +
    ''''' INDEX_CATALOG, IC.INDEX_OWNER INDEX_SCHEMA, IC.INDEX_NAME, ' +
    'IC.COLUMN_NAME, IC.COLUMN_POSITION, %s DESCENDING ' +
    'FROM SYS.ALL_IND_COLUMNS IC, SYS.ALL_INDEXES I ' +
    'WHERE %s ' +
    '      I.OWNER = IC.INDEX_OWNER AND I.INDEX_NAME = IC.INDEX_NAME ' +
    'ORDER BY IC.INDEX_OWNER, IC.TABLE_OWNER, IC.TABLE_NAME, IC.INDEX_NAME, IC.COLUMN_POSITION';

  // Index for Primary Key must be first
  fmtSQLWithIndexTypeForORA9 = 'SELECT ' +
    ''''' TABLE_CATALOG, IC.TABLE_OWNER TABLE_SCHEMA, IC.TABLE_NAME, ' +
    ''''' INDEX_CATALOG, IC.INDEX_OWNER INDEX_SCHEMA, IC.INDEX_NAME, ' +
    'IC.COLUMN_NAME, IC.COLUMN_POSITION, %s DESCENDING ' +
    'FROM SYS.ALL_IND_COLUMNS IC, SYS.ALL_INDEXES I, SYS.ALL_CONSTRAINTS C ' +
    'WHERE %s ' +
    '      I.OWNER = IC.INDEX_OWNER AND I.INDEX_NAME = IC.INDEX_NAME AND ' +
    '      C.TABLE_NAME (+) = IC.TABLE_NAME AND C.INDEX_NAME (+) = IC.INDEX_NAME AND C.OWNER (+)= IC.TABLE_OWNER ' +
    'ORDER BY DECODE(C.CONSTRAINT_TYPE, ''P'', 1, ''U'', 2, 3), IC.INDEX_OWNER, IC.TABLE_OWNER, IC.TABLE_NAME, IC.INDEX_NAME, IC.COLUMN_POSITION';

  DescOld = '0';
  DescForORA9 = 'DECODE(IC.DESCEND, ''ASC'', 0, 1)';
var
  WhereClause, IndexSchema, TableName, TableSchema, IndexName, Uniqueness, SQL,
  Desc: _string;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexSchema := Trim(Restrictions.Values['INDEX_SCHEMA']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  Uniqueness := Trim(Restrictions.Values['UNIQUE']);

  WhereClause := '';
  AddWhere(WhereClause, 'IC.INDEX_OWNER', IndexSchema);
  AddWhere(WhereClause, 'IC.TABLE_OWNER', TableSchema);
  AddWhere(WhereClause, 'IC.TABLE_NAME', TableName);
  AddWhere(WhereClause, 'IC.INDEX_NAME', IndexName);

  if TOCIRecordSet(FRecordSet).FConnection.GetOracleVersion < 9000 then
    Desc := DescOld
  else
    Desc := DescForORA9;

  if (Uniqueness = '0') or (Uniqueness = '1') then begin
    if Uniqueness = '0' then
      Uniqueness := 'NONUNIQUE'
    else
      Uniqueness := 'UNIQUE';
    AddWhere(WhereClause, 'I.UNIQUENESS', Uniqueness);
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    if TOCIConnection(FRecordSet.GetCommand.GetConnection).GetOracleVersion < 9000 then
      SQL := _Format(fmtSQLWithIndexTypeOld, [Desc, WhereClause])
    else
      SQL := _Format(fmtSQLWithIndexTypeForORA9, [Desc, WhereClause]);
  end
  else begin
    if WhereClause <> '' then
      WhereClause := 'WHERE ' + WhereClause;
    SQL := _Format(fmtGetIndexColumnsSQL, [Desc, WhereClause]);
  end;

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;

  Result := FRecordSet;
end;

function TOCIMetaData.GetConstraints(Restrictions: _TStrings): TData;
const
  fmtGetConstraintsSQLStart = 'SELECT ' +
    ''''' TABLE_CATALOG, OWNER TABLE_SCHEMA, TABLE_NAME, CONSTRAINT_NAME, ' +
    'DECODE(CONSTRAINT_TYPE, ''C'', ''CHECK'', ''P'', ''PRIMARY KEY'', ''U'', ''UNIQUE'', ' +
    '''R'', ''FOREIGN KEY'', ''UNKNOWN'') CONSTRAINT_TYPE, ';

  fmtGetConstraintsSQLforORA9 = ''''' INDEX_CATALOG, INDEX_OWNER INDEX_SCHEMA, INDEX_NAME, ';
  
  fmtGetConstraintsSQLEnd = 'R_OWNER REFERRED_TABLE_OWNER, R_CONSTRAINT_NAME REFERRED_CONSTRAINT_NAME, ' +
    'DECODE(STATUS, ''ENABLED'', 1, 0) ENABLED ' +
    'FROM SYS.ALL_CONSTRAINTS ' +
    '%s ORDER BY OWNER, TABLE_NAME, CONSTRAINT_NAME';
var
  WhereClause, Schema, TableName, ConstraintName, Types, TypesFilter: _string;
  fmtGetConstraintsSQL: string;
  BoolTypes: TBooleanArray;
  i: integer;
  SQL: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := Trim(Restrictions.Values['CONSTRAINT_TYPE']);

  WhereClause := '';
  AddWhere(WhereClause, 'OWNER', Schema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'CONSTRAINT_NAME', ConstraintName);

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
            0: TypesFilter := TypesFilter + '''C''';
            1: TypesFilter := TypesFilter + '''P''';
            2: TypesFilter := TypesFilter + '''U''';
            3: TypesFilter := TypesFilter + '''R''';
          end;
        end;
      end;
      if TypesFilter = '' then
        TypesFilter := '0 = 1'
      else begin
        TypesFilter := 'CONSTRAINT_TYPE IN (' + TypesFilter + ')';
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

  if TOCIConnection(FRecordSet.GetCommand.GetConnection).GetOracleVersion >= 9000 then
    fmtGetConstraintsSQL := fmtGetConstraintsSQLStart + fmtGetConstraintsSQLforORA9 + fmtGetConstraintsSQLEnd
  else
    fmtGetConstraintsSQL := fmtGetConstraintsSQLStart + fmtGetConstraintsSQLEnd;

  SQL := _Format(fmtGetConstraintsSQL, [WhereClause]);
  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetDataTypes(Restrictions: _TStrings): TData;
const
  fmtGetDataTypesSQL = 'SELECT TYPE_NAME FROM SYS.ALL_TYPES ' +
  'WHERE OWNER IS NULL ' +
  'ORDER BY TYPE_NAME';
begin
  FRecordSet.SetSQL(fmtGetDataTypesSQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetUsers(Restrictions: _TStrings): TData;
const
  fmtGetUsersSQL = 'SELECT ' +
    'USERNAME USER_NAME, CREATED ' +
    'FROM SYS.ALL_USERS ' +
    '%s ORDER BY USERNAME';
var
  WhereClause, UserName: _string;
begin
  UserName := Trim(Restrictions.Values['USER_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'USERNAME', UserName);
  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetUsersSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetPackages(Restrictions: _TStrings): TData;
const
  fmtGetPackagesSQL = 'SELECT ' +
    ''''' PACKAGE_CATALOG, OWNER PACKAGE_SCHEMA, OBJECT_NAME PACKAGE_NAME, ' +
    'CREATED, LAST_DDL_TIME, STATUS ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE = ''PACKAGE'' ORDER BY OWNER, OBJECT_NAME';
var
  WhereClause, Schema, PackageName: _string;
begin
  Schema := Trim(Restrictions.Values['PACKAGE_SCHEMA']);
  PackageName := Trim(Restrictions.Values['PACKAGE_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'OWNER', Schema);
  AddWhere(WhereClause, 'OBJECT_NAME', PackageName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  FRecordSet.SetSQL(_Format(fmtGetPackagesSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetUDTs(Restrictions: _TStrings): TData;
const
  fmtGetUDTsSQL = 'SELECT ' +
    ''''' TYPE_CATALOG, OWNER TYPE_SCHEMA, OBJECT_NAME TYPE_NAME, ' +
    'CREATED, LAST_DDL_TIME, STATUS ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE = ''TYPE'' ORDER BY OWNER, OBJECT_NAME';
var
  WhereClause, Schema, TypeName: _string;
begin
  Schema := Trim(Restrictions.Values['TYPE_SCHEMA']);
  TypeName := Trim(Restrictions.Values['TYPE_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'OWNER', Schema);
  AddWhere(WhereClause, 'OBJECT_NAME', TypeName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  FRecordSet.SetSQL(_Format(fmtGetUDTsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TOCIMetaData.GetSequences(Restrictions: _TStrings): TData;
const
  fmtGetSequencesSQL = 'SELECT ' +
    ''''' SEQUENCE_CATALOG, OWNER SEQUENCE_SCHEMA, OBJECT_NAME SEQUENCE_NAME, ' +
    'CREATED, LAST_DDL_TIME ' +
    'FROM SYS.ALL_OBJECTS ' +
    'WHERE %s OBJECT_TYPE IN (''SEQUENCE'') ORDER BY OWNER, OBJECT_NAME ';
var
  WhereClause, Schema, ObjectName, Scope: _string;
begin
  Schema := Trim(Restrictions.Values['SEQUENCE_SCHEMA']);
  ObjectName := Trim(Restrictions.Values['SEQUENCE_NAME']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'OWNER', Schema)
  else
    AddWhere(WhereClause, 'OWNER', TOCIRecordSet(FRecordSet).FConnection.GetCachedSchema);
  AddWhere(WhereClause, 'OBJECT_NAME', ObjectName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  FRecordSet.SetSQL(_Format(fmtGetSequencesSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

{ TOCILoader }

const
  DPBufferSize = $10000*2; // 64K*2

// RESET, [[GET_RECORD, FIELD_SET]+, DO_CONVERT, DO_LOAD]+, RESET

constructor TOCILoaderColumn.Create;
begin
  inherited;
  FIsNational := false;
  FIsLob := false;
end;

procedure TOCILoaderColumn.VerifySize;
begin
  case DataType of
    dtString, dtWideString, dtFixedChar, dtFixedWideChar, dtMemo, dtWideMemo, dtGuid {$IFDEF VER6P}, dtTimeStamp{$ENDIF}: begin
      if Size = 0 then
        Size := 20;
    end;
    dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32: begin
      if Size < sizeof(Integer) then
        Size := sizeof(Integer);
    end;
    dtFloat, dtCurrency, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}: begin
      if Size < sizeof(Double) then
        Size := sizeof(Double);
    end;
    dtDate, dtTime, dtDateTime: begin
      if Size < 7 then
        Size := 7;
    end;
    dtBlob, dtOraBlob: begin
      if Size < 4 then
        Size := 4;
    end;
    dtOraClob, dtNClob, dtWideOraClob: begin
      if Size < 4 then
        Size := 4;
    end;
  else begin
      if Size = 0 then
        Size := 20;
    end;
  end;
end;

constructor TOCILoader.Create;
begin
  inherited;

  FDML := TOCICommand.Create;
  FDML.SetProp(prAutoCommit, True);
  FDML.SetProp(prTemporaryLobUpdate, True);
  hDirPathCtx := nil;
  hColumnArray := nil;
  hStream := nil;
  FBuffer := nil;
  FBlobBuffer := nil;
  FIsDirectMode := True;
  FSkipReadOnlyFieldDescs := False;
end;

destructor TOCILoader.Destroy;
begin
  FDML.Free;

  inherited;
end;

function TOCILoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirectPath:
      FIsDirectMode := Value;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TOCILoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirectPath:
      Value := FIsDirectMode;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

procedure TOCILoader.Prepare;
var
  StrTableName: _string;
  SchemaName: _string;
  i: integer;
  ColumnCount: integer; //sb2
  hColumnList: pOCIParam;
  hColumn: pOCIParam;
  ParamType: byte;
  D1: integer; //byte;
  D2: integer; //word;
  BufSize: Integer;
  Res: sword;
  Ptr, ValuePtr: IntPtr;
  ValueInt: Integer;
  ValSize: integer;
  St: _string;
  ColumnSize: integer;
  CharsetID: integer;
begin
  inherited;

  if FIsDirectMode then begin
  // Direct path
    if PossibleOCICallStyles = [OCI80] then
      RaiseError(SDirectPathLoadingNotSupportedWithDirect);
    CheckOCI81;
    FConnection.CheckOCI80;

    if hDirPathCtx = nil then begin
      Res := OCIHandleAlloc(hOCIEnv, pOCIHandle(hDirPathCtx), OCI_HTYPE_DIRPATH_CTX, 0, nil); // raises OCI_NO_DATA if OCIThreaded
      if Res <> OCI_SUCCESS then begin
        hDirPathCtx := nil;
        Check(Res);
      end;
    end;

    i := Pos('.', TableName);
    if i > 0 then begin
      StrTableName := _UpperCase(Copy(TableName, i + 1, Length(TableName)));
      SchemaName := _UpperCase(Copy(TableName, 1, i - 1));
    end
    else begin
      StrTableName := _UpperCase(TableName);
      SchemaName := FConnection.GetCachedSchema;
    end;

    Ptr := StringToHGlobalOCI(SchemaName, ValSize, OCIUnicode);
    Res := OCIAttrSet1(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, Ptr, ValSize,
      OCI_ATTR_SCHEMA_NAME, hOCIError);
    FreeStringOCI(Ptr, OCIUnicode);
    Check(Res);

    Ptr := StringToHGlobalOCI(StrTableName, ValSize, OCIUnicode);
    Res := OCIAttrSet1(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, Ptr, ValSize,
      OCI_ATTR_NAME, hOCIError);
    FreeStringOCI(Ptr, OCIUnicode);
    Check(Res);

  // Columns
    ColumnCount := Columns.Count;
    Check(OCIAttrSet2(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, ColumnCount,
      0, OCI_ATTR_NUM_COLS, hOCIError));

    ValuePtr := OrdinalToPtr(hColumnList);
    try
      Check(OCIAttrGet1(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, ValuePtr, nil,
        OCI_ATTR_LIST_COLUMNS, hOCIError));
    finally
      PtrToOrdinal(ValuePtr, hColumnList);
    end;

    Check(OCIAttrGet2(hColumnList, OCI_DTYPE_PARAM, ValueInt, nil,
      OCI_ATTR_PTYPE, hOCIError));
    ParamType := Byte(ValueInt);

    Assert(ParamType = OCI_PTYPE_LIST);

    FRowSize := 0;
    for i := 0 to ColumnCount - 1 do begin
      Check(OCIParamGet(hColumnList, OCI_DTYPE_PARAM, hOCIError, hColumn, i + 1));

      try
        Ptr := StringToHGlobalOCI(Columns[i].Name, ValSize, OCIUnicode);
        Res := OCIAttrSet1(hColumn, OCI_DTYPE_PARAM, Ptr, ValSize,
          OCI_ATTR_NAME, hOCIError);
        FreeStringOCI(Ptr, OCIUnicode);
        Check(Res);

        with TOCILoaderColumn(Columns[i]) do begin
          ColumnSize := DataSize;
          case DataType of
            dtInt8, dtInt16, dtInt32: begin
              FColumnType := SQLT_INT;
              ColumnSize := SizeOf(Integer);
            end;
            dtUInt32, dtUInt16: begin
              FColumnType := SQLT_UIN;
              ColumnSize := SizeOf(Integer);
            end;
            dtFloat, dtCurrency, dtInt64, dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBcd{$ENDIF}{$ENDIF}, dtNumber: begin
              FColumnType := SQLT_FLT;
              ColumnSize := SizeOf(Double);
            end;
            dtDate, dtTime, dtDateTime, dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
              FColumnType := SQLT_DAT;
              ColumnSize := 8;
            end;
            dtBlob, dtOraBlob: begin
              FColumnType := SQLT_CHR;
              ColumnSize := 4;
              FIsLob := true;
            end;
            dtOraClob, dtNClob, dtWideOraClob: begin
              FColumnType := SQLT_CHR;
              ColumnSize := 4;
              FIsLob := true;
            end;
          else
            FColumnType := SQLT_CHR;
            if ColumnSize = 0 then
              ColumnSize := 20;
          end;

          Check(OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, FColumnType,
            0, OCI_ATTR_DATA_TYPE, hOCIError));

          if ConverToUnicode(TOCILoaderColumn(Columns[i])) then begin
            CharsetID := OCI_UTF16ID;
            Check(OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, CharsetID,
              0, OCI_ATTR_CHARSET_ID, hOCIError));
          end;

          ValueInt := ColumnSize;
          Check(OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, ValueInt,
            0, OCI_ATTR_DATA_SIZE, hOCIError));

          if Precision > 0 then begin
            D1 := Integer(Precision);
            Check(OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, D1,
              0, OCI_ATTR_PRECISION, hOCIError));
          end;

          if Scale > 0 then begin
            D1 := Integer(Scale);
            Check(OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, D1,
              0, OCI_ATTR_SCALE, hOCIError));
          end;

        // date format
          if FDateFormat <> '' then begin
            Ptr := StringToHGlobalOCI(FDateFormat, ValSize, OCIUnicode);
            Res := OCIAttrSet1(hColumn, OCI_DTYPE_PARAM, Ptr, ValSize,
              OCI_ATTR_DATEFORMAT, hOCIError);
            FreeStringOCI(Ptr, OCIUnicode);
            Check(Res);
          end;

          if Scale > 0 then begin
          // Set char set for float fields
            D2 := 1; // US7ASCII
            Check(OCIAttrSet2(hColumn, OCI_DTYPE_PARAM, D2, 0,
              OCI_ATTR_CHARSET_ID, hOCIError));
          end;

          FOffset := FRowSize;
          Inc(FRowSize, ColumnSize);
        end;

      finally
        // free memory for OCIParamGet
        OCIDescriptorFree(hColumn, OCI_DTYPE_PARAM);
      end;

    end;

    BufSize := Integer(DPBufferSize);
    Check(OCIAttrSet2(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX, BufSize,
        0, OCI_ATTR_BUF_SIZE, hOCIError));
    try
      Check(OCIDirPathPrepare(hDirPathCtx, FConnection.GetSvcCtx, hOCIError));
    except
      Check(OCIHandleFree(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX));
      hDirPathCtx := nil;
      raise;
    end;

    if hColumnArray = nil then
      Check(OCIHandleAlloc(hDirPathCtx, pOCIHandle(hColumnArray), OCI_HTYPE_DIRPATH_COLUMN_ARRAY, 0, nil));
    if hStream = nil then
      Check(OCIHandleAlloc(hDirPathCtx, pOCIHandle(hStream), OCI_HTYPE_DIRPATH_STREAM, 0, nil));

    Check(OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY, ValueInt, nil,
      OCI_ATTR_NUM_ROWS, hOCIError));
    FBufNumRows := Integer(ValueInt);

    Check(OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY, ValueInt, nil,
      OCI_ATTR_NUM_COLS, hOCIError));
    FBufNumCols := Word(ValueInt);

    FBuffer := Marshal.AllocHGlobal(FBufNumRows * FRowSize);
    FBlobBuffer := TList.Create;

    Reset;
  end
  else begin
  // DML
    FDML.SetConnection(FConnection);
    St := 'INSERT INTO ' + TableName + '(';
    for i := 0 to Columns.Count - 1 do begin
      if i > 0 then
        St := St + ', ';
      St := St + '"' + Columns[i].Name + '"'
    end;
    St := St + ') VALUES (';
    for i := 0 to Columns.Count - 1 do begin
      if i > 0 then
        St := St + ', ';
      St := St + ':"' + Columns[i].Name + '"';
    end;
    St := St + ')';

    FDML.SetSQL(St);

  // Set params type
    FBufNumRows := 100;
    for i := 0 to Columns.Count - 1 do begin
      FDML.Params[i].SetDataType(Columns[i].DataType);
      if Columns[i].DataType in [dtBlob, dtOraBlob, dtOraClob, dtNClob, dtWideOraClob] then
        FDML.Params[i].SetSize(0)
      else
        FDML.Params[i].SetSize(TOCILoaderColumn(Columns[i]).Size);
      TOraParamDesc(FDML.Params[i]).SetLength(FBufNumRows);
    end;

    FDML.Prepare;
  end;
end;

procedure TOCILoader.Finish;
var
  i: integer;
begin
  if FIsDirectMode then begin
  // Direct
    Reset;

    if hDirPathCtx <> nil then begin
      Check(OCIDirPathFinish(hDirPathCtx, hOCIError));

      Check(OCIHandleFree(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY));
      Check(OCIHandleFree(hStream, OCI_HTYPE_DIRPATH_STREAM));
      Check(OCIHandleFree(hDirPathCtx, OCI_HTYPE_DIRPATH_CTX));

      hColumnArray := nil;
      hStream := nil;
      hDirPathCtx := nil;
    end;

    Marshal.FreeHGlobal(FBuffer);
    FBuffer := nil;
    if FBlobBuffer <> nil then begin
      for i := 0 to FBlobBuffer.Count - 1 do
        Marshal.FreeHGlobal(IntPtr(FBlobBuffer.Items[i]));
      FBlobBuffer.Free;
      FBlobBuffer := nil;
    end;
  end
  else begin
  // DML
    FDML.UnPrepare;
  end;

  inherited;
end;

procedure TOCILoader.Reset;
var
  i: Integer;
begin
  inherited;

  for i := 0 to FColumns.Count - 1 do
    TOCILoaderColumn(FColumns[i]).VerifySize;

  if hColumnArray <> nil then
    Check(OCIDirPathColArrayReset(hColumnArray, hOCIError));
  if hStream <> nil then
    Check(OCIDirPathStreamReset(hStream, hOCIError));
end;

procedure TOCILoader.PutColumnData(Col: integer; Row: integer; const Value: variant);
var
{$IFDEF CLR}
  i, lb: integer;
{$ENDIF}
  SrcPtr, Ptr: IntPtr;
  Size: integer;
  Column: TOCILoaderColumn;
  Flag: byte;
  AStr: AnsiString;
  WStr: WideString;
  OldDecimalSeparator: {$IFDEF CLR}string{$ELSE}Char{$ENDIF};
  NumericCharacters: variant;
  ObjRef: TSharedObject;

  procedure Error;
  begin
    raise EConvertError.Create('Cannot convert data for ' + Column.Name + ' column');
  end;
begin
  if Row > FLoadedRows + FBufNumRows then
    DoLoad;

  inherited;

  if Row <= FLoadedRows then
    RaiseError('Invalid row number');

  if FIsDirectMode then begin
  // Direct
    Column := TOCILoaderColumn(Columns[Col]);
    Ptr := PtrOffset(FBuffer, (Row - FLoadedRows - 1) * FRowSize + Column.FOffset);
    Flag := OCI_DIRPATH_COL_COMPLETE;
    Size := Column.Size;

    if VarIsNull(Value) or VarIsEmpty(Value) then
      Flag := OCI_DIRPATH_COL_NULL
    else if Column.FIsLob then begin
      if VarIsStr(Value) then begin
        if ConverToUnicode(Column) then begin
          WStr := Value;
          Size := Length(WStr) * 2;
          SrcPtr := Marshal.StringToHGlobalUni(WStr);
        end
        else begin
          AStr := Value;
          Size := Length(AStr);
          SrcPtr := Marshal.StringToHGlobalAnsi(AStr);
        end;

        try
          Ptr := Marshal.AllocHGlobal(Size);
          CopyBuffer(SrcPtr, Ptr, Size);
        finally
          FreeString(SrcPtr);
        end;
      end
      else if VarType(Value) = varByte + varArray then begin
      {$IFDEF CLR}
        lb := VarArrayLowBound(Value, 1);
        Size := VarArrayHighBound(Value, 1) - lb + 1;
        Ptr := Marshal.AllocHGlobal(Size);
        for i := lb to Size - 1 do
          Marshal.WriteByte(Ptr, i, Value[i + lb]);
      {$ELSE}
        Size := TVarData(Value).VArray.Bounds[0].ElementCount;
        SrcPtr := TVarData(Value).VArray.Data;
        Ptr := Marshal.AllocHGlobal(Size);
        CopyBuffer(SrcPtr, Ptr, Size);
     {$ENDIF}
      end
      else if VarType(Value) = {$IFDEF CLR}varObject{$ELSE}varByRef{$ENDIF} then begin
     {$IFDEF CLR}
        Assert(Value is TSharedObject);
        ObjRef :=TSharedObject(Value);
     {$ELSE}
        ObjRef := TVarData(Value).VPointer;
     {$ENDIF}
        Size := TOraLob(ObjRef).Size;
        Ptr := Marshal.AllocHGlobal(Size);
        TOraLob(ObjRef).Read(0, Size, Ptr);
      end
      else
        Error;

      FBlobBuffer.Add(Ptr);
    end
    else
      case Column.FColumnType of
        SQLT_CHR: begin
          case VarType(Value) of
            varInteger,varByte,varSmallint{$IFDEF VER6P},varShortInt,varWord,varLongWord{$ENDIF}:
              if ConverToUnicode(Column) then
                WStr := IntToStr(Value)
              else
                AStr := IntToStr(Value);
            varDouble,varSingle,varCurrency: begin
              OldDecimalSeparator := DecimalSeparator;
              FConnection.GetProp(prNumericCharacters, NumericCharacters);
              DecimalSeparator := VarToStr(NumericCharacters)[1];
              try
                {if (Column.Precision > 0) and (Column.Scale > 0) then
                  Str := FloatToStrF(Value, ffFixed, Column.Precision, Column.Scale)
                else
                  Str := FloatToStr(Value);
                }
                if ConverToUnicode(Column) then
                  WStr := FloatToStr(Value)
                else
                  AStr := FloatToStr(Value);
              finally
                DecimalSeparator := OldDecimalSeparator;
              end;
            end;
            varDate:
              if Column.DateFormat <> '' then
                if ConverToUnicode(Column) then
                  WStr := FormatDateTime(Column.DateFormat, Value)
                else
                  AStr := FormatDateTime(Column.DateFormat, Value)
              else
                if ConverToUnicode(Column) then
                  WStr := DateToStr(Value)
                else
                  AStr := DateToStr(Value);
          else
            if VarIsStr(Value) then
              if ConverToUnicode(Column) then
                WStr := Value
              else
                AStr := Value
            else
              Error;
          end;

          if ConverToUnicode(Column) then begin
            Size := Length(WStr);
            if Size > Column.Size then
              Size := Column.Size;
            Size := Size * 2;
            SrcPtr := Marshal.StringToHGlobalUni(WStr);
          end
          else begin
            Size := Length(AStr);
            if Size > Column.Size then
              Size := Column.Size;
            SrcPtr := Marshal.StringToHGlobalAnsi(AnsiString(AStr));
          end;

          try
            CopyBuffer(SrcPtr, Ptr, Size);
          finally
            FreeString(SrcPtr);
          end;
        end;
        SQLT_INT, SQLT_UIN:
          case VarType(Value) of
            varInteger,varDouble,varCurrency,varSingle,varByte,varSmallint{$IFDEF VER6P},varShortInt,varWord,varLongWord{$ENDIF}:
              Marshal.WriteInt32(Ptr, Value);
          else
            if VarIsStr(Value) then
              Marshal.WriteInt32(Ptr, StrToInt(Value))
            else
              Error;
          end;
        SQLT_FLT:
          case VarType(Value) of
            varByte, varShortInt, varSmallint, varWord, varInteger, {$IFDEF VER6P}varLongWord,{$ENDIF} varInt64,
            varSingle, varDouble, varCurrency:
              Marshal.WriteInt64(Ptr, BitConverter.DoubleToInt64Bits(Value));
          else
            if VarIsStr(Value) then
              Marshal.WriteInt64(Ptr, BitConverter.DoubleToInt64Bits(StrToFloat(Value)))
            else
              Error;
          end;
        SQLT_DAT:
          case VarType(Value) of
            varDate:
              DateTimeToOraDate(Value, Ptr);
          else
            Error;
          end;
      else
        Error;
      end;

    Check(OCIDirPathColArrayEntrySet(hColumnArray, hOCIError, Row - FLoadedRows - 1,
      Col, Ptr, Size, Flag));
  end
  else begin
  // DML
    TOraParamDesc(FDML.Params[Col]).SetItemAsVariant((Row - 1) mod FBufNumRows + 1, Value);
  end;
end;

procedure TOCILoader.DoLoad;
var
  Res: sword;
  ArrRowCount: integer; // count of rows in array
  RowOffset: integer;
  PieceRows: integer; // count of rows in piece
  RowCount: integer;
  Action: _TDPErrorAction;
  ColCount: word;
  ValueInt: Integer;
begin
  if FIsDirectMode then begin
  /// Loading via Oracle direct path interface
    RowOffset := 0;
    ColCount := 0;
    ArrRowCount := FLastRow - FLoadedRows + 1;
    Action := _dpFail;
    repeat
      Res := OCIDirPathColArrayToStream(hColumnArray, hDirPathCtx, hStream,
        hOCIError, ArrRowCount, RowOffset);

      Check(OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY, ValueInt, nil,
        OCI_ATTR_ROW_COUNT, hOCIError));
      RowCount := Integer(ValueInt);

      if OCIVersion >= 8170 then // Difference of 8.1.7
        PieceRows := RowCount
      else
        PieceRows := RowCount - RowOffset;

      if Res <> OCI_SUCCESS then begin
      /// Stream buffer is not large enough to contain all of the column array
      /// data (OCI_CONTINUE), partial column was encountered (OCI_NEED_DATA)
      /// or convert error ocured (OCI_ERROR).
        if OCIVersion >= 8170 then // Difference of 8.1.7
          Inc(RowOffset, RowCount)
        else
          RowOffset := RowCount;

        if Res = OCI_ERROR then begin
        /// process convert error
          try
            Check(OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
              ValueInt, nil, OCI_ATTR_COL_COUNT, hOCIError));
            ColCount := Word(ValueInt);
            Check(Res);
          except
            on E: Exception do begin
              Action := _dpFail;
              if Assigned(FOnError) then
                FOnError(E, ColCount, FLoadedRows + RowCount, Action);
              case Action of
                _dpFail:
                  raise;
                _dpAbort:
                  Break;
              end;
            end;
          end;
        /// Skip row which caused error
          Inc(PieceRows);
          Inc(RowOffset);
        end;
      end;

      Res := OCIDirPathLoadStream(hDirPathCtx, hStream, hOCIError);

      if Res = OCI_ERROR then begin
      /// process load error
        ColCount := 0;
        try
          Check(OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
            ValueInt, nil, OCI_ATTR_ROW_COUNT, hOCIError));
          RowCount := Integer(ValueInt);
          Check(OCIAttrGet2(hColumnArray, OCI_HTYPE_DIRPATH_COLUMN_ARRAY,
            ValueInt, nil, OCI_ATTR_COL_COUNT, hOCIError));
          ColCount := Word(ValueInt);
          Check(Res);
        except
          on E: Exception do begin
            Action := _dpFail;
            if Assigned(FOnError) then
              FOnError(E, ColCount, FLoadedRows + RowCount, Action);
            case Action of
              _dpFail:
                raise;
              _dpAbort:
                Break;
            end;
          end;
        end;
      end;
    /// reset stream
      Check(OCIDirPathStreamReset(hStream, hOCIError));
      Inc(FLoadedRows, PieceRows);
    until FLoadedRows >= FLastRow + 1;

    if Action <> _dpAbort then
      Assert(FLoadedRows = FLastRow + 1);

  /// reset column array
    Check(OCIDirPathColArrayReset(hColumnArray, hOCIError));
    if Action = _dpAbort then
      Abort;
  end
  else begin
  // Loading via DML array
    ArrRowCount := FLastRow - FLoadedRows + 1;
    FDML.Execute(ArrRowCount);
    Inc(FLoadedRows, ArrRowCount);
  end;
end;

class function TOCILoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TOCILoaderColumn;
end;

class function TOCILoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TOCIRecordSet;
end;

procedure TOCILoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TOCIConnection(Value);
end;

procedure TOCILoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  inherited;

  with TOCILoaderColumn(Column) do begin
    case FieldDesc.DataType of
      dtNumber: begin
        Size := 0;
        Precision := FieldDesc.Length;
        Scale := FieldDesc.Scale;
      end;
      dtTime: begin
        FDateFormat := 'hh24:mi:ss';
      end;
      dtDate: begin
        FDateFormat := 'dd.mm.yyyy';
      end;
      dtDateTime,dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
        FDateFormat := 'dd.mm.yyyy hh24:mi:ss';
      end;
    end;

    FIsNational := TOCIFieldDesc(FieldDesc).IsNational;
  end;
end;

function TOCILoader.ConverToUnicode(Column: TOCILoaderColumn): boolean;
begin
  Result := False;

  // Loader allows to use Unicode for Oracle 11 client and higher
  {
  Result := (Column.DataType in [dtString, dtWideString,
                                 dtFixedChar, dtFixedWideChar,
                                 dtOraClob, dtNClob, dtWideOraClob]) and
            (FConnection.FUseUnicode or Column.IsNational);
  }
end;

{ TOCIAlerter }

constructor TOCIAlerter.Create;
begin
  inherited;

  FListenConnection := TOCIConnection.Create;
  FWaitCommand := FListenConnection.CreateCommand;
{$IFDEF MSWINDOWS}
  FResponseEvent := TEvent.Create(nil, True, False, '');
  FIntervalEvent := TEvent.Create(nil, True, False, '');
{$ENDIF}

  FTimeOut := -1;
  FInterval := 0;
  FEventType := _etAlert;
  FSelfMessage := msBlank;
  FAutoCommit := True;
  FSelfEvents := True;
end;

destructor TOCIAlerter.Destroy;
begin
  Stop;

  FWaitCommand.Free;
  FListenConnection.Free;
{$IFDEF MSWINDOWS}
  FResponseEvent.Free;
  FIntervalEvent.Free;
{$ENDIF}

  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);

  inherited;
end;

function TOCIAlerter.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      FAutoCommit := Value;
    prAlerterTimeout:
      FTimeOut := Value;
    prAlerterInterval:
      FInterval := Value;
    prAlerterEventType:
      FEventType := _TEventType(Value);
    prAlerterSelfEvents:
      FSelfEvents := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOCIAlerter.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prAutoCommit:
      Value := FAutoCommit;
    prAlerterTimeout:
      Value := FTimeOut;
    prAlerterInterval:
      Value := FInterval;
    prAlerterEventType:
      Value := Variant(FEventType);
    prAlerterSelfEvents:
      Value := FSelfEvents;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TOCIAlerter.SendEvent(const EventName, Message: _string);
begin
  case FEventType of
    _etAlert: begin
      SendAlertMessage(EventName, Message);
   end;
    _etPipe: begin
      PackMessage(Message);
      SendPipeMessage(EventName);
    end;
  end;
end;

procedure TOCIAlerter.Start;
var
  Name: _string;
  i: integer;
  SQL: _string;
begin
  if FActive then
    exit;

  Assert(FEventNames.Count > 0);

{$IFDEF MSWINDOWS}
  AllocODACWnd;
{$ENDIF}

  FListenConnection.Assign(TOCIConnection(FConnection));
  FListenConnection.Connect('');

  if (FEventType = _etAlert) and not FRegistered then
    RegisterEvents;

  case FEventType of
    _etAlert: begin
      SQL := '';
      if OCIVersion div 10 = 805 then begin  // fix Oracle 8.0.5 bug that unregister events, need to check server version
        for i := 0 to FEventNames.Count - 1 do begin
          Name := Trim(FEventNames[i]);
          SQL := SQL + '  DBMS_ALERT.Register(''' + Name + ''');' + LineSeparator;
        end;
      end;

      FWaitCommand.SetSQL('begin ' + SQL + 'DBMS_ALERT.WaitAny(:Name, :Message, :Status, :TimeOut); end;');
      with FWaitCommand.Params[0] do begin
        SetParamType(pdOutput);
        SetSize(TOCIConnection(FConnection).GetMaxStringSize);
        if TOCIConnection(FConnection).FUseUnicode then
          SetDataType(dtWideString)
        else
          SetDataType(dtString);
      end;
      with FWaitCommand.Params[1] do begin
        SetParamType(pdOutput);
        SetSize(TOCIConnection(FConnection).GetMaxStringSize);
        if TOCIConnection(FConnection).FUseUnicode then
          SetDataType(dtWideString)
        else
          SetDataType(dtString);
      end;
      with FWaitCommand.Params[2] do begin
        SetParamType(pdOutput);
        SetDataType(dtInteger);
      end;
      with FWaitCommand.Params[3] do begin
        SetParamType(pdInput);
        SetDataType(dtInteger);
        if FTimeOut >= 0 then
          Value := FTimeOut
        else
          Value := 86400000; // 1000 days
      end;
    end;
    _etPipe: begin
      FWaitCommand.SetSQL('begin :Status := DBMS_PIPE.Receive_Message(:Name, :TimeOut); end;');

      with FWaitCommand.Params[0] do begin
        SetParamType(pdOutput);
        SetDataType(dtInteger);
      end;

      // get first pipe from list
      Name := FEventNames[0];

      with FWaitCommand.Params[1] do begin
        SetParamType(pdInput);
        if TOCIConnection(FConnection).FUseUnicode then begin
          SetDataType(dtWideString);
          SetSize(Length(WideString(Name)));
        end
        else begin
          SetDataType(dtString);
          SetSize(Length(AnsiString(Name)));
        end;
        Value := Name;
      end;
      with FWaitCommand.Params[2] do begin
        SetParamType(pdInput);
        SetDataType(dtInteger);
        if FTimeOut >= 0 then
          Value := FTimeOut
        else
          Value := 86400000; // 1000 days
      end;
    end;
  end;

  FActive := True;
  try
    FWaitCommand.Prepare;

    FListenThread := TOCIAlerterListenThread.Create(Self);
  except
    FActive := False;
    raise;
  end;
end;

procedure TOCIAlerter.Stop;
begin
  if FActive then begin
    FListenThread.Terminate;
    FListenThread.WaitFor;
    FreeAndNil(FListenThread);

    FActive := False;

    try
      FWaitCommand.UnPrepare;
      if FEventType = _etAlert then
        RemoveEvents;
      FListenConnection.Disconnect;
    except
    end;
  end;
end;

procedure TOCIAlerter.PackMessage(const Item: variant);
var
  Command: TOCICommand;
begin
  if (FEventType = _etPipe) and (Item <> Null) then begin
    Command := TOCIConnection(FConnection).GetCommand;

    Command.SetSQL('begin DBMS_PIPE.Pack_Message(:Item); end;');

    with Command.Params[0] do begin
      SetParamType(pdInput);
      SetDataType(dtUnknown);
      Value := Item;
    end;

    Command.Execute;
  end;
end;

procedure TOCIAlerter.UnpackMessage(var Item: variant);
var
  Command: TOCICommand;
  DataType: word;
  Size: integer;
begin
  Size := 0;
  case NextItemType of
    mtNone: begin
      Item := Null;
      Exit;
    end;
    mtNumber:
      DataType := dtFloat;
    mtString: begin
      if TOCIConnection(FListenConnection).FUseUnicode then
        DataType := dtWideString
      else
        DataType := dtString;
      Size := TOCIConnection(FListenConnection).GetMaxStringSize;
    end;
    mtDate:
      DataType := dtDateTime;
  else
    DataType := dtString;
  end;

  Command := TOCIConnection(FListenConnection).GetCommand;

  Command.SetSQL('begin DBMS_PIPE.Unpack_Message(:Item); end;');

  with Command.Params[0] do begin
    SetParamType(pdOutput);
    SetDataType(DataType);
    SetSize(Size);
  end;

  Command.Execute;

  Item := Command.Params[0].GetValue;
end;

function TOCIAlerter.NextItemType: TMessageType;
var
  Command: TOCICommand;
  Res: integer;
begin
  Command := TOCIConnection(FListenConnection).GetCommand;

  Command.SetSQL('begin :Result := DBMS_PIPE.Next_Item_Type; end;');

  with Command.Params[0] do begin
    SetParamType(pdOutput);
    SetDataType(dtInteger);
  end;

  Command.Execute;

  Res := Command.Params[0].GetValue;
  case Res of
    0: Result := mtNone;
    6: Result := mtNumber;
    9: Result := mtString;
    12: Result := mtDate;
  else
    Result := mtNone;
  end;
end;

procedure TOCIAlerter.SendPipeMessage(const Name: _string);
var
  Command: TOCICommand;
  Res: integer;
  AName: _string;
begin
  Assert(FEventNames.Count > 0);

  AName := Name;
  if AName = '' then
    AName := FEventNames[0];

  Command := TOCIConnection(FConnection).GetCommand;

  Command.SetSQL('begin :Result := DBMS_PIPE.Send_Message(:Name); end;');

  with Command.Params[0] do begin
    SetParamType(pdOutput);
    SetDataType(dtInteger);
  end;
  with Command.Params[1] do begin
    SetParamType(pdInput);
    if TOCIConnection(FConnection).FUseUnicode then begin
      SetDataType(dtWideString);
      SetSize(Length(WideString(AName)));
    end
    else begin
      SetDataType(dtString);
      SetSize(Length(AnsiString(AName)));
    end;
    Value := AName;
  end;

  Command.Execute;

  Res := Command.Params[0].GetValue;
  if Res <> 0 then
    raise Exception.Create(SAlerterSendFailed);
end;

procedure TOCIAlerter.PurgePipe;
var
  Command: TOCICommand;
  AName: _string;
begin
  Assert(FEventNames.Count > 0);

  AName := FEventNames[0];

  Command := TOCIConnection(FConnection).GetCommand;

  Command.SetSQL('begin :Result := DBMS_PIPE.Purge(:Name); end;');

  with Command.Params[0] do begin
    SetParamType(pdOutput);
    SetDataType(dtInteger);
  end;
  with Command.Params[1] do begin
    SetParamType(pdInput);
    if TOCIConnection(FConnection).FUseUnicode then begin
      SetDataType(dtWideString);
      SetSize(Length(WideString(AName)));
    end
    else begin
      SetDataType(dtString);
      SetSize(Length(AnsiString(AName)));
    end;
    Value := AName;
  end;

  Command.Execute;
end;

function TOCIAlerter.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;

procedure TOCIAlerter.RegisterEvents;
var
  Name: _string;
  i: integer;
  SQL: _string;
  Command: TOCICommand;
begin
  Assert(FEventNames.Count > 0);

  if FEventType = _etAlert then begin
    SQL := '';
    for i := 0 to FEventNames.Count - 1 do begin
      Name := Trim(FEventNames[i]);
      SQL := SQL + '  DBMS_ALERT.Register(''' + Name + ''');' + LineSeparator;
    end;

    Command := TOCIConnection(FListenConnection).GetCommand;
    Command.SetSQL('begin' + LineSeparator + SQL + 'end;');
    try
      Command.Execute;
    except
      on E: EOraError do begin
        if E.ErrorCode = 6550 then
          raise EOraError.Create(E.ErrorCode,
            'Please execute GRANT EXECUTE ON DBMS_ALERT TO ' +
            FConnection.GetUsername + ' as SYS' + LineSeparator + LineSeparator + E.Message);
        raise;
      end;
    end;
  end;
  FRegistered := True;
end;

procedure TOCIAlerter.RemoveEvents;
var
  Name: _string;
  i: integer;
  SQL: _string;
  Command: TOCICommand;
begin
  Assert(FEventNames.Count > 0);

  FRegistered := False;
  if FEventType = _etAlert then begin
    SQL := '';
    for i := 0 to FEventNames.Count - 1 do begin
      Name := Trim(FEventNames[i]);
      SQL := SQL + '  DBMS_ALERT.Remove(''' + Name + ''');' + LineSeparator;
    end;

    Command := TOCIConnection(FListenConnection).GetCommand;
    Command.SetSQL('begin' + LineSeparator + SQL + 'end;');
    Command.Execute;
  end;
end;

procedure TOCIAlerter.SendAlertMessage(const EventName, Message: _string);
var
  Command: TOCICommand;
begin
  if not FSelfEvents then
    FSelfMessage := Message;
  Command := TOCIConnection(FConnection).GetCommand;
  Command.SetSQL('begin DBMS_ALERT.Signal(:Name, :Message); end;');

  with Command.Params[0] do begin
    SetParamType(pdInput);
    if TOCIConnection(FConnection).FUseUnicode then begin
      SetDataType(dtWideString);
      SetSize(Length(WideString(EventName)));
    end
    else begin
      SetDataType(dtString);
      SetSize(Length(AnsiString(EventName)));
    end;
    Value := EventName;
  end;

  with Command.Params[1] do begin
    SetParamType(pdInput);
    if TOCIConnection(FConnection).FUseUnicode then begin
      SetDataType(dtWideString);
      SetSize(Length(WideString(Message)));
    end
    else begin
      SetDataType(dtString);
      SetSize(Length(AnsiString(Message)));
    end;
    Value := Message;
  end;

  Command.Execute;

  if FAutoCommit and TOCIConnection(FConnection).AutoCommit then
    FConnection.GetInternalTransaction.Commit;
end;

function TOCIAlerter.ProcessWaitResult: boolean;
var
  Status: integer;
begin
  Status := FWaitCommand.Params.ParamByName('Status').GetValue;
  if Status = 0 then begin
    ProcessMessage;
    Result := True;
  end
  else
    Result := ProcessError(Status);
end;

procedure TOCIAlerter.ProcessMessage;
var
  Event: TAlertEvent;
  Item: variant;
begin
  Event := TAlertEvent.Create;
  try
    case FEventType of
      _etAlert: begin
        Event.Name := _VarToStr(FWaitCommand.Params.ParamByName('Name').GetValue);
        Event.Message := _VarToStr(FWaitCommand.Params.ParamByName('Message').GetValue);
      end;
      _etPipe: begin
        Event.Name := FEventNames[0];
        UnpackMessage(Item);
        Event.Message := _VarToStr(Item);
      end;
    end;

    if Event.Message = FSelfMessage then begin
      FSelfMessage := msBlank;
      Event.Free;
    end
    else
    if Event.Message = msTerminate then begin
      Event.Free;
    end
    else begin
    {$IFDEF MSWINDOWS}
      FResponseEvent.ResetEvent;
      PostMessage(hODACWindow, WM_ALERTER_EVENT, Integer(GCHandle),
        Integer(AllocGCHandle(Event)));
      FResponseEvent.WaitFor(INFINITE);
    {$ELSE}
      try
        DoOnEvent(Event.Name, Event.Message);
      finally
        Event.Free;
      end;  
    {$ENDIF}
    end;

  except
    Event.Free;
    raise;
  end;
end;

function TOCIAlerter.ProcessError(Status: integer): boolean;
begin
  if Status = 1 then begin
    if FTimeOut >= 0 then begin
      Result := False;
    {$IFDEF MSWINDOWS}
      FResponseEvent.ResetEvent;
      PostMessage(hODACWindow, WM_ALERTER_TIMEOUT, Integer(GCHandle),
        {$IFNDEF CLR}Integer(@Result){$ELSE}0{$ENDIF});
      FResponseEvent.WaitFor(INFINITE);
    {$ELSE}
      DoOnTimeout(Result);
    {$ENDIF}
    end
    else
      Result := True;
  end
  else
    raise Exception.CreateFmt(SAlerterReceiveFailed, [Status]);
end;

procedure TOCIAlerter.DoOnEvent(const EventName, Message: _string);
begin
  if Assigned(OnEvent) then
    OnEvent(EventName, Message);
end;

procedure TOCIAlerter.DoOnTimeout(var Continue: boolean);
begin
  if Assigned(OnTimeout) then
    OnTimeout(Continue);
end;

procedure TOCIAlerter.DoOnError(E: Exception);
begin
  if Assigned(OnError) then
    OnError(E);
end;

{ TOCIAlerterListenThread }

constructor TOCIAlerterListenThread.Create(Alerter: TOCIAlerter);
begin
  inherited Create(True);

  FAlerter := Alerter;
  Resume;
end;

procedure TOCIAlerterListenThread.Terminate;
var
  Event: _string;
begin
  inherited Terminate;

{$IFDEF MSWINDOWS}
  FAlerter.FResponseEvent.SetEvent;
  FAlerter.FIntervalEvent.SetEvent;
{$ENDIF}

  Event := FAlerter.EventNames[0];

  {if csDestroying in FSession.ComponentState then begin
    // force stopping on application exit
    if FWaitCommand.Executing then
      FWaitCommand.BreakExec;
  end
  else}
  FAlerter.SendEvent(Event, msTerminate);
  if (FAlerter.FEventType = _etAlert) and
    not (FAlerter.FAutoCommit and TOCIConnection(FAlerter.Connection).AutoCommit)
  then
    FAlerter.Connection.GetInternalTransaction.Commit;

{$IFDEF WIN32_64}
  if WaitForSingleObject(Handle, 1000) = WAIT_TIMEOUT	then
    FAlerter.FListenConnection.BreakExec;
{$ENDIF}
end;

procedure TOCIAlerterListenThread.Execute;
var
  Continue: boolean;
begin
  try
    repeat
      FAlerter.FWaitCommand.Execute;

      if not Terminated then begin
        Continue := FAlerter.ProcessWaitResult;

      {$IFDEF MSWINDOWS}
        if not Continue and not Terminated then begin
          if FAlerter.FInterval > 0 then begin
            FAlerter.FIntervalEvent.ResetEvent;
            FAlerter.FIntervalEvent.WaitFor(FAlerter.FInterval * 1000);
            Continue := not Terminated;
          end
          else
            PostMessage(hODACWindow, WM_ALERTER_STOPED, Integer(FAlerter.GCHandle), 0);
        end;
      {$ENDIF}
      end
      else
        Continue := False;

    until not Continue;
  except
  {$IFDEF MSWINDOWS}
    on E: Exception do begin
      if not Terminated then begin
      {$IFDEF VER6P}
      {$IFNDEF CLR}
        AcquireExceptionObject;
      {$ENDIF}
        PostMessage(hODACWindow, WM_ALERTER_STOPED, Integer(FAlerter.GCHandle),
          Integer(AllocGCHandle(E)));
      {$ELSE}
        PostMessage(hODACWindow, WM_ALERTER_STOPED, Integer(FAlerter.GCHandle), 0);
      {$ENDIF}
      end;
    end;
  {$ENDIF}
  end;
end;

{$ENDIF}

{ TOraClassesUtils }

class procedure TOraClassesUtils.InternalUnPrepare(Obj: TOCIRecordSet);
begin
  Obj.InternalUnPrepare;
end;

{$IFDEF CLR}
function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;
var
  L: Integer;
  Buffer: StringBuilder;
begin
  Buffer := StringBuilder.Create(1024);
  L := GetLocaleInfo(Locale, LocaleType, Buffer, Buffer.Capacity);
  if L > 0 then Result := Buffer.ToString else Result := Default;
end;
{$ENDIF}

{$IFDEF FPC}
function GetLocaleStr(Locale, LocaleType: Integer; const Default: string): string;
{$IFDEF MSWINDOWS}
var
  L: Integer;
  Buffer: array[0..255] of Char;
begin
  L := GetLocaleInfo(Locale, LocaleType, Buffer, SizeOf(Buffer));
  if L > 0 then SetString(Result, Buffer, L - 1) else Result := Default;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := Default;
end;
{$ENDIF}
{$ENDIF}

function QuotedOCIName(Name: _string): _string;
var
  i: integer;
  SchemaName: _string;
begin
  i := Pos('.', Name);
  if i > 0 then begin
    SchemaName := Copy(Name, 1, i - 1);
    Result := '"' + SchemaName + '".' + QuotedOCIName(Copy(Name, i + 1, Length(Name)));
  end
  else begin
    Result := '"' + Name + '"';
  end;
end;

function QuotedSQLName(Name: _string): _string;
var
  i: integer;
  SchemaName: _string;
  function IsQuoted(Name: _string): boolean;
  begin
    Result := (Length(Name) > 1) and (Name[1] = '"') and (Name[Length(Name)] = '"');
  end;
begin
  i := Pos('.', Name);
  if i > 0 then begin
    SchemaName := Trim(Copy(Name, 1, i - 1));
    if not IsQuoted(SchemaName) then
      SchemaName := '"' + _UpperCase(SchemaName) + '"';
    Result := SchemaName + '.' + QuotedSQLName(Copy(Name, i + 1, Length(Name)));
  end
  else begin
    Result := Trim(Name);
    if not IsQuoted(Result) then
      Result := '"' + _UpperCase(Result) + '"';
  end;
end;

procedure ParseConnectString(const ConnectString: _string;
  var Username, Password, Server: _string; var ConnectMode: TConnectMode);
var
  i, p, Mode: integer;
  s: _string;
  c: _char;
  Quoted: boolean;
begin
  Username := '';
  Password := '';
  Server := '';
  ConnectMode := cmNormal;

  // 1. bypass blanks
  // 2. go to the end of name
  // 3. go to delimiter ('/', '@') and detect next mode

  Mode := 0;
  i := 1;
  while True do begin
    // 1.
    while (i <= Length(ConnectString)) and (ConnectString[i] = ' ') do
      Inc(i);
    if i > Length(ConnectString) then
      exit;

    // 2.
    p := i;
    Quoted := ConnectString[i] = '"';
    if Quoted then begin
      Inc(i);
      while i <= Length(ConnectString) do begin
        c := ConnectString[i];
        Inc(i);
        if c = '"' then
          break;
      end;
    end
    else begin
      while i <= Length(ConnectString) do begin
        case ConnectString[i] of
          ' ', '"':
            break;
          '/', '@':
            if Mode < 2 then
              // Server can contain any characters except space and "
              break;
        end;
        Inc(i);
      end
    end;

    s := Copy(ConnectString, p, i - p);
    case Mode of
      0:
        Username := s;
      1:
        Password := OCISQLInfo.UnQuote(s); // Password is case-insensitive; Direct mode doen't support quotes
      2:
        Server := OCISQLInfo.UnQuote(s); // Quoted server name is not supported by OCI
      3: begin
        s := _UpperCase(s);
        if s = 'SYSDBA' then
          ConnectMode := cmSysDBA
        else
        if s = 'SYSOPER' then
          ConnectMode := cmSysOper
        else
        if s = 'SYSASM' then
          ConnectMode := cmSysASM
        else
          RaiseError(SInvalidConnectString);
      end;
    end;

    // 3.
    p := i;
    while (i <= Length(ConnectString)) and (ConnectString[i] = ' ') do
      Inc(i);
    if i > Length(ConnectString) then
      exit;
    if (Mode < 1) and (ConnectString[i] = '/') then
      Mode := 1
    else
    if (Mode < 2) and (ConnectString[i] = '@') then
      Mode := 2
    else
    // at least one space must be before and after AS
    if (Mode < 3) and (i > p) and (_UpperCase(Copy(ConnectString, i, 3)) = 'AS ') then
      Mode := 3
    else
      exit;
    if Mode = 3 then
      Inc(i, 3)
    else
      Inc(i);
  end;
end;

{$IFDEF CLR}
var
  OCICallbackDefineCode : array[0..51] of byte =
    ($55, $8B, $EC, $51,
    $8B, $45, $24, $50,
    $8B, $45, $20, $50,
    $8B, $45, $1C, $50,
    $8B, $45, $18, $50,
    $8B, $45, $14, $50,
    $8B, $45, $10, $50,
    $8B, $45, $C, $50,
    $8B, $45, $8, $50,

    $B8, $00, $00, $00, $00,
    $FF, $D0,
    $89, $45, $FC,
    $8B, $45, $FC,
    $59,
    $5D,
    $C3);

  OCICallbackInBindCode : array[0..51] of byte =
    ($55,
    $8B, $EC,
    $51,
    $8B, $45, $24, $50,
    $8B, $45, $20, $50,
    $8B, $45, $1C, $50,
    $8B, $45, $18, $50,
    $8B, $45, $14, $50,
    $8B, $45, $10, $50,
    $8B, $45, $C, $50,
    $8B, $45, $8, $50,

    $B8, $00, $00, $00, $00,
    $FF, $D0,
    $89, $45, $FC,
    $8B, $45, $FC,
    $59,
    $5D,
    $C3);

  OCICallbackOutBindCode : array[0..55] of byte =
    ($55,
    $8B, $EC,
    $51,
    $8B, $45, $28, $50,
    $8B, $45, $24, $50,
    $8B, $45, $20, $50,
    $8B, $45, $1C, $50,
    $8B, $45, $18, $50,
    $8B, $45, $14, $50,
    $8B, $45, $10, $50,
    $8B, $45, $C, $50,
    $8B, $45, $8, $50,

    $B8, $00, $00, $00, $00,
    $FF, $D0,

    $89, $45, $FC,
    $8B, $45, $FC,

    $59,
    $5D,
    $C3);

  OCICallbackFailoverCode : array[0..39] of byte =
    ($55,
    $8B, $EC,
    $51,
    $8B, $45, $18, $50,
    $8B, $45, $14, $50,
    $8B, $45, $10, $50,
    $8B, $45, $C, $50,
    $8B, $45, $8, $50,

    $B8, $00, $00, $00, $00,
    $FF, $D0,

    $89, $45, $FC,
    $8B, $45, $FC,

    $59,
    $5D,
    $C3);

  OCISubscriptionNotifyCode : array[0..43] of byte =
    ($55,
    $8B, $EC,
    $51,
    $8B, $45, $1C, $50,
    $8B, $45, $18, $50,
    $8B, $45, $14, $50,
    $8B, $45, $10, $50,
    $8B, $45, $C, $50,
    $8B, $45, $8, $50,
    $B8, $00, $00, $00, $00,
    $FF, $D0,
    $89, $45, $FC,
    $8B, $45, $FC,
    $59,
    $5D,
    $C3);


var
  DefineRec: packed record
    Ptr: TOCICallbackDefine;
  end;
  InBindRec: packed record
    Ptr: TOCICallbackInBind;
  end;
  OutBindRec: packed record
    Ptr: TOCICallbackOutBind;
  end;
  FailoverRec: packed record
    Ptr: TOCICallbackFailover;
  end;
  ChangeNotifyRec: packed record
    Ptr: TOCISubscriptionNotify;
  end;
  CallbackRecPtr: IntPtr;
{$ENDIF}

initialization
{$IFDEF LINUX}
  hLockConnect := TCriticalSection.Create;
{$ENDIF}
  ActiveConnectionsCount := 0;
  hLockConnectCount := TCriticalSection.Create;
  OCISQLInfo := TOCISQLInfo.Create(nil);

{$IFDEF CLR}
  OCICallbackDefinePtr := Marshal.AllocHGlobal(Length(OCICallbackDefineCode));
  Marshal.Copy(TBytes(OCICallbackDefineCode), 0, OCICallbackDefinePtr, Length(OCICallbackDefineCode));

  OCICallbackInBindPtr := Marshal.AllocHGlobal(Length(OCICallbackInBindCode));
  Marshal.Copy(TBytes(OCICallbackInBindCode), 0, OCICallbackInBindPtr, Length(OCICallbackInBindCode));

  OCICallbackOutBindPtr := Marshal.AllocHGlobal(Length(OCICallbackOutBindCode));
  Marshal.Copy(TBytes(OCICallbackOutBindCode), 0, OCICallbackOutBindPtr, Length(OCICallbackOutBindCode));

  OCICallbackFailoverPtr := Marshal.AllocHGlobal(Length(OCICallbackFailoverCode));
  Marshal.Copy(TBytes(OCICallbackFailoverCode), 0, OCICallbackFailoverPtr, Length(OCICallbackFailoverCode));

  OCICallbackChangeNotifyPtr := Marshal.AllocHGlobal(Length(OCISubscriptionNotifyCode));
  Marshal.Copy(TBytes(OCISubscriptionNotifyCode), 0, OCICallbackChangeNotifyPtr, Length(OCISubscriptionNotifyCode));

  CallbackRecPtr := Marshal.AllocHGlobal(20);
  try
    DefineRec.Ptr := OCICallbackDefine;
    HOCICallbackDefine := GCHandle.Alloc(@DefineRec.Ptr, GCHandleType.Normal);
    Marshal.StructureToPtr(DefineRec, CallbackRecPtr, False);
    Marshal.WriteIntPtr(OCICallbackDefinePtr, 37, Marshal.ReadIntPtr(CallbackRecPtr));

    InBindRec.Ptr := OCICallbackInBind;
    HOCICallbackInBind := GCHandle.Alloc(@InBindRec.Ptr, GCHandleType.Normal);
    Marshal.StructureToPtr(InBindRec, CallbackRecPtr, False);
    Marshal.WriteIntPtr(OCICallbackInBindPtr, 37, Marshal.ReadIntPtr(CallbackRecPtr));

    OutBindRec.Ptr := OCICallbackOutBind;
    HOCICallbackOutBind := GCHandle.Alloc(@OutBindRec.Ptr, GCHandleType.Normal);
    Marshal.StructureToPtr(OutBindRec, CallbackRecPtr, False);
    Marshal.WriteIntPtr(OCICallbackOutBindPtr, 41, Marshal.ReadIntPtr(CallbackRecPtr));

    FailoverRec.Ptr := OCICallbackFailover;
    HOCICallbackFailover := GCHandle.Alloc(@FailoverRec.Ptr, GCHandleType.Normal);
    Marshal.StructureToPtr(FailoverRec, CallbackRecPtr, False);
    Marshal.WriteIntPtr(OCICallbackFailoverPtr, 25, Marshal.ReadIntPtr(CallbackRecPtr));

    ChangeNotifyRec.Ptr := OCICallbackChangeNotify;
    HOCICallbackChangeNotify := GCHandle.Alloc(@ChangeNotifyRec.Ptr, GCHandleType.Normal);
    Marshal.StructureToPtr(ChangeNotifyRec, CallbackRecPtr, False);
    Marshal.WriteIntPtr(OCICallbackChangeNotifyPtr, 29, Marshal.ReadIntPtr(CallbackRecPtr));

    HWndProc := GCHandle.Alloc(@WndClass.lpfnWndProc, GCHandleType.Normal);
  finally
    Marshal.FreeHGlobal(CallbackRecPtr);
  end;
{$ELSE}
  OCICallbackDefinePtr := @OCICallbackDefine;
  OCICallbackInBindPtr := @OCICallbackInBind;
  OCICallbackOutBindPtr := @OCICallbackOutBind;
  OCICallbackFailoverPtr := @OCICallbackFailover;
{$IFDEF MSWINDOWS}
  OCICallbackChangeNotifyPtr := @OCICallbackChangeNotify;
{$ENDIF}
{$ENDIF}

finalization
  if GlobalBuf <> nil then
    Marshal.FreeHGlobal(GlobalBuf);
{$IFDEF LINUX}
  hLockConnect.Free;
{$ENDIF}
  hLockConnectCount.Free;
  OCISQLInfo.Free;
{$IFDEF MSWINDOWS}
  if hODACWindow <> 0 then
    DestroyWindow(hODACWindow);
{$ENDIF}

{$IFDEF CLR}
  HOCICallbackDefine.Free;
  HOCICallbackInBind.Free;
  HOCICallbackOutBind.Free;
  HOCICallbackFailover.Free;
  HOCICallbackChangeNotify.Free;
  HWndProc.Free;
  Marshal.FreeHGlobal(OCICallbackDefinePtr);
  Marshal.FreeHGlobal(OCICallbackInBindPtr);
  Marshal.FreeHGlobal(OCICallbackOutBindPtr);
  Marshal.FreeHGlobal(OCICallbackFailoverPtr);
  Marshal.FreeHGlobal(OCICallbackChangeNotifyPtr);
{$ENDIF}

end.
