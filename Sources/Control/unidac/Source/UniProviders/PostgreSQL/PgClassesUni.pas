
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgClassesUni;

{$ENDIF}

interface

uses  
{$IFDEF MSWINDOWS}
  Windows, Messages, Registry,
{$ENDIF}
  Classes, SysUtils, Types, Variants, Math, SyncObjs, FMTBcd,
{$IFNDEF FPC}
  SqlTimSt,
{$ENDIF}
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text, WinUtils,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRParser, MemUtils, MemData, CRAccess, CRVio,
{$IFNDEF UNIDACPRO}
  PgCall, PgSQLNet, PgSQLProtocol, PgError;
{$ELSE}
  PgCallUni, PgSQLNetUni, PgSQLProtocolUni, PgErrorUni;
{$ENDIF}

const
  dtVoid = 100;
  dtPgName = 101;
  dtPgChar = 102;
  dtFixedChar = 103;
  dtNumeric = 105;
  dtPgSingle = 106;
  dtOID = 107;
  dtPgLargeObject = 108;

  dtPgDate = 109;
  dtPgTime = 110;
  dtPgTimeStamp = 111;
  dtPgInterval = 112;
  dtPgTimeStampTZ = 113;
  dtPgTimeTZ = 114;

  dtPgPoint    = 115;
  dtPgLSeg     = 116;
  dtPgBox      = 117;
  dtPgPath     = 118;
  dtPgPolygon  = 119;
  dtPgCircle   = 120;


  prPgSQLBase = 1000;

  prSchema             = prPgSQLBase + 1;
  prUseUnicode         = prPgSQLBase + 2;
  prCharset            = prPgSQLBase + 3;
  prConnectionTimeout  = prPgSQLBase + 4;
  prEnablePgTimeStamps = prPgSQLBase + 5;
  prIntervalAsString   = prPgSQLBase + 6;
  prEnableGeometrics   = prPgSQLBase + 7;
  prEnableComposites   = prPgSQLBase + 8;
  prSimpleQueryExecute = prPgSQLBase + 9;
  prOIDAsInt           = prPgSQLBase + 10;
  prCacheBlobs         = prPgSQLBase + 11;
  prDeferredBlobRead   = prPgSQLBase + 12;
  prCommandTimeout     = prPgSQLBase + 13;
  prCursorAsString     = prPgSQLBase + 14;
  prUnknownAsString    = prPgSQLBase + 15;
  prAutoClose          = prPgSQLBase + 16;
  prProtocolVersion    = prPgSQLBase + 17;
  prSSL_Mode           = prPgSQLBase + 18;
  prSSL_CACert         = prPgSQLBase + 19;
  prSSL_Cert           = prPgSQLBase + 20;
  prSSL_Key            = prPgSQLBase + 21;
  prSSL_CipherList     = prPgSQLBase + 22;
  prFieldsAsText       = prPgSQLBase + 23;
  prBufferSize         = prPgSQLBase + 25;
  prTextMode           = prPgSQLBase + 26;
  prUseParamTypes      = prPgSQLBase + 27;
  prCursorWithHold     = prPgSQLBase + 28;
  prApplicationName    = prPgSQLBase + 29;

  MaxNumericWords = 250;

{$HPPEMIT 'struct TParamPlaceHolder;'}
{$HPPEMIT 'struct TEventInfo;'}
{$HPPEMIT 'class TPgSQLParamDesc;'}

type
  TPgSQLTypes = class;
  TPgSQLCommand = class;
  TPgSQLRecordSet = class;
  TPgCursor = class;
  TPgSQLFieldDesc = class;
{$IFNDEF LITE}
  TPgSQLNotificationsHandler = class;
  TPgSQLNotificationsThread = class;
{$ENDIF}

  TProtocolVersion = (pv20, pv30);

  TPgSQLNotificationEvent = procedure (const Name: _string; const PID: integer; const Message: _string) of object;

  TSSLMode = (smDisable, smRequire, smPrefer, smAllow);

  TPgSQLConnection = class(TCRConnection)
  private
    FDatabase: string;
    FPort: Integer;
    FProtocolVersion: TProtocolVersion;
    FConnectionTimeout: integer;
    FApplicationName: string;
    FSSL_Mode: TSSLMode;
    FSSL_CACert: string;
    FSSL_Cert: string;
    FSSL_Key: string;
    FSSL_CipherList: string;
    FUseUnicode: boolean;
    FCharset: string;
    FSchema: _string;
    FCachedSchema: _string;
    FEnablePgTimeStamps: boolean;
    FIntervalAsString: boolean;
    FEnableGeometrics: boolean;
    FEnableComposites: boolean;

    FProtocol: TPgSQLProtocol;
    FCommand: TPgSQLCommand;
    FTypes: TPgSQLTypes;
    FFetchConnection: TPgSQLConnection;
  {$IFNDEF LITE}
    FAuxConnection: TPgSQLConnection;
    FNotificationsHandler: TPgSQLNotificationsHandler;
  {$ENDIF}
    FInTransaction: boolean;
    FIntegerDateTimes: boolean;
    FFetchConCount: integer;

    FOnNotice: TPgProtocolNoticeEvent;
    FOnNotification: TPgSQLNotificationEvent;

    function DecodeError(E: EPgError): EPgError;
    procedure ProcessInternalException(E: EPgError; Component: TObject);

    procedure PrepareFetchConnection;
    procedure ReleaseFetchConnection;
  {$IFNDEF LITE}
    procedure CheckAuxConnection;
  {$ENDIF}
    procedure SetClientEncoding;
    procedure GetIntegerDateTimes;

    procedure DoOnNotice(Errors: TPgErrors);
    procedure DoOnNotification(const Name: AnsiString; const PID: integer; const Message: AnsiString);

  protected
    property EnableBCD;
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetTransactionClass: TCRTransactionClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    function CheckIsValid: boolean; override;
  {$IFNDEF LITE}
    procedure ReturnToPool; override;
  {$ENDIF}

    procedure Connect(const ConnectString: _string); override;
    procedure Disconnect; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function GetServerVersion: _string; override;
    function GetServerVersionFull: _string; override;
    function GetClientVersion: _string; override;
    function GetMajorServerVersion: integer;
    function GetMinorServerVersion: integer;
    function VersionIsEqualOrHigher(MajorVer, MinorVer: integer): boolean;

    function GetProcessID: integer;
    function GetProtocolVersion: TProtocolVersion;
    function GetProtocol: TPgSQLProtocol;
    function GetTypes: TPgSQLTypes;
    function GetCurrentSchema: _string;
    procedure SetCurrentSchema(Value: _string);
    function GetCachedSchema: _string;
    function GetInTransaction: boolean;
  {$IFNDEF LITE}
    function GetNotificationsHandler: TPgSQLNotificationsHandler;
  {$ENDIF}

    function CanChangeDatabase: boolean; override;

    function CreateCommand: TPgSQLCommand;
    function GetCommand: TPgSQLCommand;
    procedure ExecCommand(const SQL: _string);

    procedure BreakExec;

    property OnNotice: TPgProtocolNoticeEvent read FOnNotice write FOnNotice;
    property OnNotification: TPgSQLNotificationEvent read FOnNotification write FOnNotification;
  end;

  TPgSQLTransaction = class(TCRTransaction)
  public
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: _string); override;
    procedure ReleaseSavepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;
  end;

  TPgSQLTypes = class
  private
    FConnection: TPgSQLConnection;
  {$IFNDEF LITE}
    FUnknownTypes: TThreadList;
    FRowTypes: TThreadList;
  {$ENDIF}

    procedure GetPgSQLTypeAttr(DataType: Integer; TypeModifier: Integer;
      var Length, Scale: integer);
  {$IFNDEF LITE}
    function IsUnknownType(TypeCode: integer): boolean;
    function FindRowType(TypeCode: integer): TObjectType; overload;
    function FindRowType(const TypeName: _string): TObjectType; overload;
  {$ENDIF}

  public
    constructor Create(Connection: TPgSQLConnection);
    destructor Destroy; override;

    procedure DetectDataType(TypeOID, TypeModifier: integer;
      var DataType, SubDataType: word; var Length, Scale, Size: integer;
      var Fixed: boolean; var ObjectType: TObjectType;
      LongStrings, FlatBuffers, OIDAsInt, EnableBCD, EnableFMTBCD,
      CursorAsString, UnknownAsString, FieldsAsText: boolean);
    function GetInternalType(TypeCode: integer): word; overload;
    function GetInternalType(TypeCode: integer; var ObjectType: TObjectType): word; overload;
    function DataTypeToOID(DataType, SubDataType: word): integer;
  {$IFNDEF LITE}
    function GetRowType(TypeCode: integer): TObjectType; overload;
    function GetRowType(const TypeName: _string): TObjectType; overload;
  {$ENDIF}
  end;

  TPgSQLParamDesc = class(TParamDesc)
  private
  {$IFDEF LITE}
    FTypeOID: integer;
  {$ENDIF}
  public
    function GetAsCursor: TPgCursor;
    procedure SetAsCursor(Value: TPgCursor);
  end;

  TPgSQLInfo = class(TSQLInfo)
  protected
    procedure ParseExtColumnName(Parser: TSQLParser; var Code: integer; var Str: _string); override;

  public
    function IdentCase: TIdentCase; override;
  end;

  TPgSQLFieldDesc = class(TCRFieldDesc)
  private
    FIsTextMode: boolean;
    FTableOID: integer;
    FTableCol: smallint;
  public
    property TableOID: integer read FTableOID write FTableOID;
    property TableCol: smallint read FTableCol write FTableCol;
  end;

  TPgCursor = class(TCRCursor)
  private
    FStmtHandle: TPgSQLStatement;

    procedure CreateStatement;
    procedure FreeStatement;
  protected
    FState: TCursorState;

    procedure SetState(Value: TCursorState); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    
    function CanFetch: boolean; override;
    property State: TCursorState read FState write SetState;
  end;

  TParamPlaceHolder = record
    Position: integer;
    EndPosition: integer;
    ParamRef: TPgSQLParamDesc;
  end;

  TParamPlaceHolders = array of TParamPlaceHolder;

  _TStringArray = array of _string;

  TProcParamsInfo = record
    RetTypeOID: integer;
    IsReturnSet: boolean;
    TypeOIDs: TIntegerDynArray;
    Modes, Names: _TStringArray;
  end;

  TPgSQLCommand = class(TCRCommand)
  private
    FConnection: TPgSQLConnection;
    FUsedConnection: TPgSQLConnection;
    FRecordSet: TPgSQLRecordSet;
    FCommandTimeout: integer;
    FOIDAsInt: boolean;
    FFieldsAsText: boolean;
    FUseParamTypes: boolean;

    FPlaceHolders: TParamPlaceHolders;
    FPlaceHoldersCount: integer;

    FCursor: TPgCursor;
    FCursorRef: TPgCursor;
    FNextCursorRef: TPgCursor;
    FNativePreparation: boolean;

    FParsedStmtType: TParsedStmtType;

    FForceSimpleProtocol: boolean;

    FLastInsertOID: Int64;
    FRowsAffected: integer;

    FInParamsCount: integer;
    FInParamRefs: array of TPgSQLParamDesc;
    FOutParamsCount: integer;

    function GetFormatCode(TypeOID: integer; IsField: boolean): integer;

    function UseSimpleProtocol: boolean;
    function GetUsedConnection: TPgSQLConnection;
    procedure CheckConnection;

    function GetFinalSQL: _string;

    procedure MakeSPParam(const Name: _string; Oid: integer; Direction: _char);
    procedure GetProcParamsInfo(const Schema, Name: _string; Overload: integer;
      var ParamsInfo: TProcParamsInfo);

    function NativeCursor: boolean;
    function OutParamsReturn: boolean;
    function RowsReturn: boolean;
    function GetParsedStmtType: TParsedStmtType;

  protected
    // simple (Text) protocol
    function GetFinalSQLWithParamValues: AnsiString; // UTF8

    // binary protocol
    procedure CallbackBindParamValue(Dest: TPgSQLNet; ParamNo: integer; const ItemDesc: TPgSQLItemDesc);
    procedure PerformPrepare(const SQL: AnsiString; ParsedSQLType: TParsedStmtType; const ParamTypes: TIntegerDynArray);
    procedure PerformUnprepare;
    procedure PerformBindExecute;
    procedure PerformClosePortal;

    procedure InternalExecute;
    procedure DescribeFields;
    procedure DescribeParams;
    procedure ReadOutParams;
    procedure SplitParams;
    procedure ResetParams;

    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;

    function GetParamDescType: TParamDescClass; override;

    procedure ClearPlaceHolders;
    procedure AddPlaceHolder(Position, EndPosition: integer; ParamRef: TPgSQLParamDesc);
    function AddParam: TParamDesc; override;
    function GetParam(Index: integer): TPgSQLParamDesc;
    function ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string; override;
    procedure InitProcParams(const Name: _string; Overload: integer);
    function CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string; override;

    function GetPrepared: boolean; override;
    procedure Prepare; override;
    procedure Unprepare; override;
    procedure Execute(Iters: integer = 1); override;

    procedure SetConnection(Value: TCRConnection); override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    function GetCursor: TCRCursor; override;
    procedure SetCursor(Value: TCRCursor); override;
    function GetFirstCursor(): TPgCursor;
    function GetNextCursor(): TPgCursor;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
    procedure BreakExec; override;
  end;

  TCreateFieldObjectFunc = function(DataType: word): TSharedObject of Object;

  TPgSQLRecordSet = class(TCRRecordSet)
  private
    FCommand: TPgSQLCommand;
    FFetchCursor: TPgCursor;
    FCreateFieldObjectFunc: TCreateFieldObjectFunc;
    FTopFields: array of integer;
    FDescribeExecute: boolean;
    FNeedExtFieldsInfo: boolean;
    FRowsFetched: integer;

    FCacheBlobs: boolean;
    FDeferredBlobRead: boolean;
    FCursorAsString: boolean;
    FUnknownAsString: boolean;
    FCursorWithHold: boolean;

    procedure SetCommandType;
    procedure ReceiveFetchBuffer(Source: TPgSQLNet; Size: integer; FetchBlock: IntPtr;
      Row, Col: integer);
    function InternalFetch(FetchBack: boolean; var NoData: boolean): boolean;
    procedure GetExtFieldsInfo;

  protected
    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
    function UsedConnection: TPgSQLConnection;

    function InternalGetObject(FieldNo: word; RecBuf: IntPtr): TSharedObject;

    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InitFetchCursor;

    function NeedInitFieldsOnFetch: boolean; override;
    procedure InternalInitFields; override;
    procedure ExecFetch(DisableInitFields: boolean); override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

    function Fetch(FetchBack: boolean; var NoData: boolean): boolean; reintroduce; overload;
    function Fetch(FetchBack: boolean = False): boolean; overload; override;

    property IsFetchAll: boolean read FFetchAll;

  public
    constructor Create; override;
    destructor Destroy; override;

  { Fetch }
    procedure FetchAll; override;
    function CanDisconnect: boolean; override;

  { Open/Close }
    procedure ExecCommand; override;
    function IsFullReopen: boolean; override;
    procedure Reopen; override;

  { Fields }
    function GetFieldDescType: TFieldDescClass; override;
    procedure ClearFields; override;
    class function IsBlobFieldType(DataType: word): boolean; override;
    class function IsComplexFieldType(DataType: word): boolean; override;

  { Records }
    procedure CreateComplexField(RecBuf: IntPtr; FieldIndex: integer; WithBlob: boolean); override;
    class procedure InternalCreateComplexField(RecBuf: IntPtr; Connection: TPgSQLConnection;
      WithBlob: boolean; Offset: integer; DataType: word; ObjectType: TObjectType;
      CreateFieldObjectFunc: TCreateFieldObjectFunc; CacheBlobs: boolean);
    procedure FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
  {$IFNDEF LITE}
    procedure CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean); override;
    function CompareFieldValue(ValuePtr: IntPtr; const ValueType: integer; FieldDesc: TFieldDesc; RecBuf: IntPtr; const Options: TCompareOptions): integer; override;
    function CompareFields(RecBuf1: IntPtr; RecBuf2: IntPtr; FieldDesc: TFieldDesc; Options: TCompareOptions = []): integer; override;
  {$ENDIF}

  { Navigation }
    procedure SetToEnd; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    property OnCreateFieldObject: TCreateFieldObjectFunc read FCreateFieldObjectFunc write FCreateFieldObjectFunc;
  end;

{ TPgSQLMetaData }

  TPgSQLMetaData = class (TCRMetaData)
  protected
    function GetConnection: TPgSQLConnection;
    function CreateRecordSet: TCRRecordSet; override;

    function InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData; override;
    procedure InternalGetMetaDataKindsList(List: _TStringList); override;
    procedure InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string); override;

    function GetTables(Restrictions: _TStrings): TData; override;
    function GetColumns(Restrictions: _TStrings): TData; override;
    function GetProcedures(Restrictions: _TStrings): TData; override;
  {$IFNDEF LITE}
    procedure CreateProcedureParametersFields; override;
    function GetProcedureParameters(Restrictions: _TStrings): TData; override;
  {$ENDIF}
    function GetIndexes(Restrictions: _TStrings): TData; override;
    function GetIndexColumns(Restrictions: _TStrings): TData; override;
    function GetConstraints(Restrictions: _TStrings): TData; override;

    function GetDataTypes(Restrictions: _TStrings): TData; override;
    function GetDatabases(Restrictions: _TStrings): TData; override;
    function GetUsers(Restrictions: _TStrings): TData; override;
    function GetSequences(Restrictions: _TStrings): TData;
    function GetSchemas(Restrictions: _TStrings): TData;
  end;

{ TPgBufferConverter }

  TPgBufferConverter = class
  public
    { Writing }
  {$IFNDEF FPC}
    class procedure VarToFmtBcd(const Value: variant; var Bcd: TBcd);
  {$ENDIF}
    class procedure VarToPgDate(const Value: variant;
      var PgDate: TSharedObject; var NeedFree: boolean);
    class procedure VarToPgTime(const Value: variant; WithTimeZone: boolean;
      var PgTime: TSharedObject; var NeedFree: boolean);
    class procedure VarToPgTimeStamp(const Value: variant;
      var PgTimeStamp: TSharedObject; var NeedFree: boolean);
    class procedure VarToPgInterval(const Value: variant;
      var PgInterval: TSharedObject; var NeedFree: boolean);
    class procedure VarToOID(const Value: variant; var OID: integer);
  {$IFNDEF LITE}
    class procedure VarToPgGeometric(const Value: variant; DataType: word;
      var PgGeometric: TSharedObject; var NeedFree: boolean);
    class procedure VarToPgRow(const Value: variant; ObjectType: TObjectType;
      var PgRow: TDBObject; var NeedFree: boolean);
  {$ENDIF}
    class procedure VarToCursor(const Value: variant; var CursorName: _string);

    { Reading }
    class procedure ReadString(Source: TPgSQLNet; Size: integer; Dest: IntPtr; Trim: boolean);
    class procedure ReadWideString(Source: TPgSQLNet; Size: integer; Dest: IntPtr; Trim: boolean);
    class procedure ReadMemo(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadWideMemo(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadPgCursor(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
      Connection: TPgSQLConnection);
    class procedure ReadGuid(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
  end;

{ TPgBinaryConverter }

  TPgNumeric = record
    NDigits: integer;
    Weight: integer;
    Sign: integer;
    DScale: integer;
    Digits: array [0 .. MaxNumericWords - 1] of word;
    StartPos: integer;
  end;

  TPgBinaryConverter = class (TPgBufferConverter)
  private
    class procedure ReadSingle(Source: TPgSQLNet; Dest: IntPtr);
    class procedure ReadNumeric(Source: TPgSQLNet; Dest: IntPtr);

    class procedure StringToNumeric(const Value: string; var Numeric: TPgNumeric);
    class procedure DoubleToNumeric(Value: double; var Numeric: TPgNumeric);
    class procedure StripNumeric(var Numeric: TPgNumeric);

  public
    class procedure WriteValue(Value: Variant; Dest: TPgSQLNet;
      DataType, SubDataType: word; ObjectType: TObjectType; Connection: TPgSQLConnection);
    class procedure WriteSmallInt(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteInteger(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteBigInt(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteDouble(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteSingle(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteNumeric(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteCurrency(Value: Variant; Dest: TPgSQLNet;
      Connection: TPgSQLConnection);
    class procedure WriteBoolean(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteString(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteWideString(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteOID(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteDate(Value: Variant; Dest: TPgSQLNet);
    class procedure WriteTime(Value: Variant; Dest: TPgSQLNet;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure WriteTimeStamp(Value: Variant; Dest: TPgSQLNet;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure WriteInterval(Value: Variant; Dest: TPgSQLNet;
      IntegerDateTimes: boolean);
    class procedure WriteGuid(Value: Variant; Dest: TPgSQLNet);
  {$IFNDEF LITE}
    class procedure WritePgGeometric(Value: Variant; Dest: TPgSQLNet;
      DataType: word);
    class procedure WritePgRow(Value: Variant; Dest: TPgSQLNet;
      ObjectType: TObjectType; Connection: TPgSQLConnection);
  {$ENDIF}

    class procedure ReadValue(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
      DataType, SubDataType: word; Trim: boolean; Connection: TPgSQLConnection);
    class procedure ReadInt16(Source: TPgSQLNet; Dest: IntPtr);
    class procedure ReadInt32(Source: TPgSQLNet; Dest: IntPtr);
    class procedure ReadInt64(Source: TPgSQLNet; Dest: IntPtr);
    class procedure ReadDouble(Source: TPgSQLNet; Dest: IntPtr; SubDataType: word);
    class procedure ReadBCD(Source: TPgSQLNet; Dest: IntPtr);
  {$IFNDEF FPC}
    class procedure ReadFMTBCD(Source: TPgSQLNet; Dest: IntPtr);
  {$ENDIF}
    class procedure ReadCurrency(Source: TPgSQLNet; Dest: IntPtr; Connection: TPgSQLConnection);
    class procedure ReadBoolean(Source: TPgSQLNet; Dest: IntPtr);
    class procedure ReadBlob(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadPgLargeObject(Source: TPgSQLNet; Dest: IntPtr);
    class procedure ReadDate(Source: TPgSQLNet; Dest: IntPtr);
    class procedure ReadTime(Source: TPgSQLNet; Dest: IntPtr;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure ReadDateTime(Source: TPgSQLNet; Dest: IntPtr;
      IntegerDateTimes, WithTimeZone: boolean);
  {$IFNDEF LITE}
    class procedure ReadPgDate(Source: TPgSQLNet; Dest: IntPtr);
    class procedure ReadPgTime(Source: TPgSQLNet; Dest: IntPtr;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure ReadPgTimeStamp(Source: TPgSQLNet; Dest: IntPtr;
      IntegerDateTimes, WithTimeZone: boolean);
    class procedure ReadPgInterval(Source: TPgSQLNet; Dest: IntPtr;
      IntegerDateTimes: boolean);
    class procedure ReadPgGeometric(Source: TPgSQLNet; Size: integer;
      Dest: IntPtr; DataType: word);
    class procedure ReadPgRow(Source: TPgSQLNet; Dest: IntPtr;
      Trim: boolean; Connection: TPgSQLConnection);
  {$ENDIF}
  end;

{ TPgTextConverter }

  TPgTextConverter = class (TPgBufferConverter)
  public
    class function GetInt64(Source: TPgSQLNet; Size: integer): Int64;
    class function EscapeString(const Value: AnsiString;
      StringQuote, ByteaQuote, LoaderQuote: boolean): AnsiString;

    class function ValueToText(const Value: variant; DataType: word;
      UseUnicode: boolean; Quote: boolean = True; LoaderQuote: boolean = False): AnsiString; // UTF8
    class function BooleanToText(const Value: variant): AnsiString;
    class function BlobToText(const Value: variant; UseUnicode: boolean): AnsiString;
    class function DateToText(const Value: variant): AnsiString;
    class function TimeToText(const Value: variant): AnsiString;
    class function TimeStampToText(const Value: variant): AnsiString;
    class function IntervalToText(const Value: variant): AnsiString;
    class function LargeObjectToText(const Value: variant): AnsiString;
    class function GUIDToText(const Value: variant): AnsiString;
  {$IFNDEF LITE}
    class function PgGeometricToText(const Value: variant; DataType: word): AnsiString;
    class function PgRowToText(const Value: variant): AnsiString;
  {$ENDIF}
    class function CursorToText(const Value: variant; UseUnicode: boolean): AnsiString;

    class procedure ReadValue(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
      DataType, SubDataType: word; Trim: boolean; Connection: TPgSQLConnection);
    class procedure ReadInt16(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadInt32(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadInt64(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadDouble(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadBCD(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
  {$IFNDEF FPC}
    class procedure ReadFMTBCD(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
  {$ENDIF}
    class procedure ReadCurrency(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadBoolean(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadBlob(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadPgLargeObject(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadDate(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadTime(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadDateTime(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
  {$IFNDEF LITE}
    class procedure ReadPgDate(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadPgTime(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
      WithTimeZone: boolean);
    class procedure ReadPgTimeStamp(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
      WithTimeZone: boolean);
    class procedure ReadPgInterval(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadPgGeometric(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
    class procedure ReadPgRow(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
  {$ENDIF}
  end;

{$IFNDEF LITE}

{ TPgSQLAlerter }

  TPgSQLAlerter = class(TCRAlerter)
  private
    FOnEvent: TPgSQLNotificationEvent;

  protected
    procedure DoOnEvent(const EventName: _string; const PID: integer; const Message: _string);

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SendEvent(const EventName, Message: _string); override;
    procedure Start; override;
    procedure Stop; override;

    property OnEvent: TPgSQLNotificationEvent read FOnEvent write FOnEvent;
  end;

{ TPgSQLNotificationsHandler }

  TEventInfo = record
    EventName: _string;
    Callbacks: array of TPgSQLNotificationEvent;
  end;

  TPgSQLNotification = class
  public
    PID: integer;
    Name: _string;
    Message: _string;
  end;

  TPgSQLNotificationsHandler = class
  private
    FBaseConnection: TPgSQLConnection;
    FConnection: TPgSQLConnection;
    FEvents: array of TEventInfo;
    FThread: TPgSQLNotificationsThread;
{$IFDEF MSWINDOWS}
    FGCHandle: IntPtr;
{$ENDIF}
    FLockConnection: TCriticalSection;
    FConnectionIsLocked: boolean;
    FAllowLockEvent: TEvent;

    function EventIndex(const Name: _string): integer;
    procedure Start;
    procedure Stop;
    procedure Listen(const Name: _string);
    procedure Unlisten(const Name: _string);
    procedure LockConnection;
{$IFDEF MSWINDOWS}
    function GetGCHandle: IntPtr;
{$ENDIF}
    procedure ProcessNotification(const Name: _string; const PID: integer; const Message: _string);
    procedure DoOnNotification(NotificationPID: Integer; const NotificationName: _string; const NotificationMsg: _string);
    
{$IFDEF MSWINDOWS}
    property GCHandle: IntPtr read GetGCHandle;
{$ENDIF}

  public
    constructor Create(BaseConnection: TPgSQLConnection);
    destructor Destroy; override;

    procedure RegisterEvents(Events: _TStrings; Callback: TPgSQLNotificationEvent);
    procedure UnregisterEvents(Events: _TStrings; CallbackObject: TObject);
  end;

  TPgSQLNotificationsThread = class (TThread)
  private
    FHandler: TPgSQLNotificationsHandler;
  protected
    procedure Execute; override;
  end;

{ TPgSQLLoaderColumn }

  TPgSQLLoaderColumn = class(TCRLoaderColumn)
  private
    FSubDataType: integer;
    FRowType: TObjectType;
    FRowTypeName: _string;

  public
    property SubDataType: integer read FSubDataType write FSubDataType;
    property RowTypeName: _string read FRowTypeName write FRowTypeName;
  end;

{ TPgSQLLoader }

  TPgSQLLoader = class (TCRLoader)
  private
    FConnection: TPgSQLConnection;
    FCommand: TPgSQLCommand;
    FRowValues: array of variant;
    FPrepared: boolean;
    FBlockOpened: boolean;
    FBufferSize: integer;
    FTextMode: boolean;
    FIsTextMode: boolean;
    FNet: TPgSQLNet;

  protected
    class function GetRecordSetClass: TCRRecordSetClass; override;
    procedure SetConnection(Value: TCRConnection); override;
    procedure FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc); override;

    procedure LoadRow;
    procedure WriteHeader;
    procedure BeginDataBlock;
    procedure EndDataBlock;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    class function GetColumnClass: TCRLoaderColumnClass; override;
    procedure Reset; override;
    procedure Prepare; override;
    procedure DoLoad; override;
    procedure Finish; override;
    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); override;
  end;

{$ENDIF}

var
  PgSQLInfo: TPgSQLInfo;


implementation

uses
  DAConsts,
{$IFNDEF UNIDACPRO}
  PgConsts, PgParser, PgObjects;
{$ELSE}
  PgConstsUni, PgParserUni, PgObjectsUni;
{$ENDIF}

const
  DACProductName = 'PgDAC';



{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
var
  hUtilWindow: HWND;
{$ENDIF}
{$ENDIF}

function CheckTypeCorrespondence(SourceType: Word; DestType: Word): boolean;
begin
  case SourceType of
    dtMemo, dtWideMemo:
      Result := (DestType = SourceType) or
        (DestType in [dtBlob, dtMemo, dtWideMemo, dtString, dtWideString]);
    dtBlob:
      Result := DestType in [dtBlob, dtMemo, dtWideMemo];
    dtPgDate, dtPgTime, dtPgTimeStamp,
    dtPgInterval,
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle,
    dtObject:
      Result := DestType = SourceType;
  else
    Result := True;
  end;
end;

{ TPgBufferConverter}

const
  MaxDecimalWords = 8;
  DecimalArray : array [0 .. MaxDecimalWords - 1] of double = (
    1,
    10000,
    100000000,
    1000000000000,
    10000000000000000,
    100000000000000000000.0,
    1000000000000000000000000.0,
    10000000000000000000000000000.0);

  DecimalArrayInt64 : array [0 .. 3] of Int64 = (
    10000,
    100000000,
    1000000000000,
    10000000000000000);

  NUMERIC_POS = $0000;
  NUMERIC_NEG = $4000;
  DEC_DIGITS  = 4;

{$IFNDEF FPC}
class procedure TPgBufferConverter.VarToFmtBcd(const Value: variant; var Bcd: TBcd);
var
  i64: int64;
begin
  case VarType(Value) of
    varSmallint, varInteger, varByte, varWord, varShortInt:
      Bcd := IntegerToBcd(Value);
    varLongWord, varInt64: begin
      i64 := Value;
      Bcd := StrToBcd(IntToStr(i64));
    end;
    varSingle, varDouble, varCurrency:
      Bcd := DoubleToBcd(Value);
  else
    if VarIsStr(Value) then
      Bcd := StrToBcd(Value)
    else
    if VarType(Value) = VarFMTBcd then
      Bcd := VarToBcd(Value)
    else
      raise EConvertError.Create(SCannotConvertType);
  end;
end;
{$ENDIF}

class procedure TPgBufferConverter.VarToPgDate(const Value: variant;
  var PgDate: TSharedObject; var NeedFree: boolean);
var
  Days: integer;
  Date: TDateTime;
  Obj: TObject;
  ts: TCustomPgTimeStamp;
begin
  PgDate := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varDate, varDouble: begin
        ts := TPgDate.Create;
        PgDate := ts;
        NeedFree := True;
        Date := Value;
        TPgDate.FromDateTime(Date, Days);
        ts.Days := Days;
      end;
      {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TCustomPgTimeStamp) then
          raise Exception.Create(SCannotConvertType);
        PgDate := TCustomPgTimeStamp(Obj);
      end;
    else
    {$IFNDEF FPC}
      if VarType(Value) = VarSQLTimeStamp then begin
        ts := TPgDate.Create;
        PgDate := ts;
        NeedFree := True;
        TPgDate.FromSQLTimeStamp(VarToSqlTimeStamp(Value), Days);
        ts.Days := Days;
      end
      else
    {$ENDIF}
      if VarIsStr(Value) then begin
        ts := TPgDate.Create;
        PgDate := ts;
        NeedFree := True;
        TPgDate.FromString(VarToStr(Value), Days, False);
        ts.Days := Days;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgDate.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToPgTime(const Value: variant; WithTimeZone: boolean;
  var PgTime: TSharedObject; var NeedFree: boolean);
var
  Ticks: int64;
  TimeZoneOffset: integer;
  Date: TDateTime;
  Obj: TObject;
  ts: TPgTime;
begin
  PgTime := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varDate, varDouble: begin
        ts := TPgTime.Create;
        PgTime := ts;
        NeedFree := True;
        Date := Value;
        TPgTime.FromDateTime(Date, Ticks);
        ts.Ticks := Ticks;
      {$IFDEF MSWINDOWS}
        if WithTimeZone then
          ts.TimeZoneOffset := GetUtcOffset(Now);
      {$ENDIF}
      end;
      {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TCustomPgTimeStamp) then
          raise Exception.Create(SCannotConvertType);
        PgTime := TCustomPgTimeStamp(Obj);
      end;
    else
    {$IFNDEF FPC}
      {$IFDEF MSWINDOWS}
      if VarType(Value) = VarSQLTimeStamp then begin
        ts := TPgTime.Create;
        PgTime := ts;
        NeedFree := True;
        TPgTime.FromSQLTimeStamp(VarToSqlTimeStamp(Value), Ticks);
        ts.Ticks := Ticks;
        if WithTimeZone then
          ts.TimeZoneOffset := GetUtcOffset(Now);
      end
      else
      {$ENDIF}
    {$ENDIF}
      if VarIsStr(Value) then begin
        ts := TPgTime.Create;
        PgTime := ts;
        NeedFree := True;
        TPgTime.FromString(VarToStr(Value), Ticks, TimeZoneOffset, False);
        ts.Ticks := Ticks;
        ts.TimeZoneOffset := TimeZoneOffset;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgTime.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToPgTimeStamp(const Value: variant;
  var PgTimeStamp: TSharedObject; var NeedFree: boolean);
var
  Days: integer;
  Ticks: int64;
  Date: TDateTime;
  Obj: TObject;
  ts: TCustomPgTimeStamp;
begin
  PgTimeStamp := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      varDate, varDouble: begin
        ts := TPgTimeStamp.Create;
        PgTimeStamp := ts;
        NeedFree := True;
        Date := Value;
        TPgTimeStamp.FromDateTime(Date, Days, Ticks);
        ts.Days := Days;
        ts.Ticks := Ticks;
      end;
      {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TCustomPgTimeStamp) then
          raise Exception.Create(SCannotConvertType);
        PgTimeStamp := TCustomPgTimeStamp(Obj);
      end;
    else
    {$IFNDEF FPC}
      if VarType(Value) = VarSQLTimeStamp then begin
        ts := TPgTimeStamp.Create;
        PgTimeStamp := ts;
        NeedFree := True;
        TPgTimeStamp.FromSQLTimeStamp(VarToSqlTimeStamp(Value), Days, Ticks);
        ts.Days := Days;
        ts.Ticks := Ticks;
      end
      else
    {$ENDIF}
      if VarIsStr(Value) then begin
        ts := TPgTimeStamp.Create;
        PgTimeStamp := ts;
        NeedFree := True;
        TPgTimeStamp.FromString(VarToStr(Value), Days, Ticks, False);
        ts.Days := Days;
        ts.Ticks := Ticks;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgTimeStamp.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToPgInterval(const Value: variant;
  var PgInterval: TSharedObject; var NeedFree: boolean);
var
  Months, Days: integer;
  Seconds: double;
  Obj: TObject;
  Int: TPgInterval;
begin
  PgInterval := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TPgInterval) then
          raise Exception.Create(SCannotConvertType);
        PgInterval := TPgInterval(Obj);
      end;
    else
      if VarIsStr(Value) then begin
        Int := TPgInterval.Create;
        PgInterval := Int;
        NeedFree := True;
        TPgInterval.FromString(VarToStr(Value), Months, Days, Seconds);
        Int.MonthsFull := Months;
        Int.Days := Days;
        Int.SecondsFull := Seconds;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgInterval.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToOID(const Value: variant; var OID: integer);
var
  Obj: TObject;
  Lob: TPgSQLLargeObject;
begin
  case VarType(Value) of
    {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
    {$IFDEF CLR}
      Obj := Value;
    {$ELSE}
      Obj := TObject(TVarData(Value).VPointer);
    {$ENDIF}
      if not IsClass(Obj, TPgSQLLargeObject) then
        raise Exception.Create(SCannotConvertType);
      Lob := TPgSQLLargeObject(Obj);
      OID := Lob.OID;
    end;
  else
    OID := Value;
  end;
end;

{$IFNDEF LITE}
class procedure TPgBufferConverter.VarToPgGeometric(const Value: variant; DataType: word;
  var PgGeometric: TSharedObject; var NeedFree: boolean);
var
  Obj: TObject;
begin
  PgGeometric := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TPgGeometric) then
          raise Exception.Create(SCannotConvertType);

        PgGeometric := TPgGeometric(Obj);
      end;
    else
      if VarIsStr(Value) then begin
        case DataType of
          dtPgPoint:
            PgGeometric := TPgPoint.Create;
          dtPgLSeg:
            PgGeometric := TPgLSeg.Create;
          dtPgBox:
            PgGeometric := TPgBox.Create;
          dtPgPath:
            PgGeometric := TPgPath.Create;
          dtPgPolygon:
            PgGeometric := TPgPolygon.Create;
          dtPgCircle:
            PgGeometric := TPgCircle.Create;
        else
          Assert(False);
        end;
        NeedFree := True;
        TPgGeometric(PgGeometric).AsString := Value;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgGeometric.Free;
    raise;
  end;
end;

class procedure TPgBufferConverter.VarToPgRow(const Value: variant; ObjectType: TObjectType;
  var PgRow: TDBObject; var NeedFree: boolean);
var
  Obj: TObject;
begin
  PgRow := nil;
  NeedFree := False;
  try
    case VarType(Value) of
      {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
      {$IFDEF CLR}
        Obj := Value;
      {$ELSE}
        Obj := TObject(TVarData(Value).VPointer);
      {$ENDIF}
        if not IsClass(Obj, TPgRow) then
          raise Exception.Create(SCannotConvertType);

        PgRow := TPgRow(Obj);
        if (ObjectType <> nil) and (PgRow.ObjectType <> ObjectType) then
          raise Exception.Create(SCannotConvertType);
      end;
    else
      if VarIsStr(Value) then begin
        Assert(ObjectType <> nil);
        PgRow := TPgRow.Create(TPgRowType(ObjectType));
        NeedFree := True;
        TPgRow(PgRow).AsString := Value;
      end
      else
        raise Exception.Create(SCannotConvertType);
    end;
  except
    if NeedFree then
      PgRow.Free;
    raise;
  end;
end;
{$ENDIF NDEF LITE}

class procedure TPgBufferConverter.VarToCursor(const Value: variant; var CursorName: _string);
var
  Obj: TObject;
  Cursor: TPgRefCursor;
begin
  case VarType(Value) of
    {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
    {$IFDEF CLR}
      Obj := Value;
    {$ELSE}
      Obj := TObject(TVarData(Value).VPointer);
    {$ENDIF}
      if not IsClass(Obj, TPgRefCursor) then
        raise Exception.Create(SCannotConvertType);
      Cursor := TPgRefCursor(Obj);
      CursorName := Cursor.CursorName;
    end;
  else
    CursorName := _VarToStr(Value);
  end;
end;

class procedure TPgBufferConverter.ReadString(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
  Trim: boolean);
var
  EndPtr: IntPtr;
begin
  Source.ReadString(Dest, Size);
  if Trim then begin
    EndPtr := PtrOffset(Dest, Size - sizeof(AnsiChar));
    while (PtrCompare(EndPtr, Dest) >= 0) and
    {$IFDEF CLR}
      (Marshal.ReadByte(EndPtr) = Byte(' '))
    {$ELSE}
      (PByte(EndPtr)^ = Byte(' '))
    {$ENDIF}
    do
      EndPtr := PtrOffset(EndPtr, - sizeof(AnsiChar));
    Marshal.WriteByte(EndPtr, 1, 0);
  end;
end;

class procedure TPgBufferConverter.ReadWideString(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
  Trim: boolean);
var
  EndPtr: IntPtr;
begin
  Size := Source.ReadWideString(Dest, Size);
  if Trim then begin
    EndPtr := PtrOffset(Dest, Size - sizeof(WideChar));
    while (PtrCompare(EndPtr, Dest) >= 0) and
    {$IFDEF CLR}
      (Marshal.ReadInt16(EndPtr) = Int16(WideChar(' ')))
    {$ELSE}
      (PWord(EndPtr)^ = Word(WideChar(' ')))
    {$ENDIF}
    do
      EndPtr := PtrOffset(EndPtr, - sizeof(WideChar));
    Marshal.WriteInt16(EndPtr, sizeof(WideChar), 0);
  end;
end;

class procedure TPgBufferConverter.ReadMemo(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
begin
  if Size = 0 then
    Exit;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Blob.AllocPiece(Piece, Size);
  try
    BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
    Source.ReadString(BlobData, Size, False);
    Piece.Used := Size;
    Blob.AppendPiece(Piece);
  except
    Blob.FreePiece(Piece);
  end;
end;

class procedure TPgBufferConverter.ReadWideMemo(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
begin
  if Size = 0 then
    Exit;
    
  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Blob.AllocPiece(Piece, Size * 2);
  try
    BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
    Piece.Used := Source.ReadWideString(BlobData, Size, False);
    Blob.CompressPiece(Piece);
    Blob.AppendPiece(Piece);
  except
    Blob.FreePiece(Piece);
  end;
end;

class procedure TPgBufferConverter.ReadPgCursor(Source: TPgSQLNet; Size: integer;
  Dest: IntPtr; Connection: TPgSQLConnection);
var
  Cursor: TPgRefCursor;
  CursorName: _string;
begin
  Cursor := TPgRefCursor(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  if Connection.FUseUnicode then
    CursorName := Utf8Decode(Source.ReadString(Size))
  else
    CursorName := _string(Source.ReadString(Size));
  Cursor.Assign(Connection, CursorName);
end;

class procedure TPgBufferConverter.ReadGuid(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
begin
  Source.ReadString(Dest, Size);
end;

{ TPgBinaryConverter }

class procedure TPgBinaryConverter.WriteValue(Value: Variant; Dest: TPgSQLNet;
  DataType, SubDataType: word; ObjectType: TObjectType; Connection: TPgSQLConnection);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) or (DataType = dtCursor) then
    Dest.WriteInt32(-1)
  else begin
    // PgLoader support
    case DataType of
      dtNumeric: begin
        DataType := dtFloat;
        SubDataType := dtNumeric;
      end;
      dtPgTimeTZ: begin
        DataType := dtTime;
        SubDataType := dtPgTimeTZ;
      end;
      dtPgTimeStampTZ: begin
        DataType := dtDateTime;
        SubDataType := dtPgTimeStampTZ;
      end;
    end;

    Dest.EnterSizeBlock;
    try
      case DataType of
        dtSmallInt:
          WriteSmallInt(Value, Dest);
        dtInteger:
          if SubDataType = dtOID then
            WriteOID(Value, Dest)
          else
            WriteInteger(Value, Dest);
        dtLargeInt:
          WriteBigInt(Value, Dest);
        dtFloat, dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}:
          case SubDataType of
            dtPgSingle:
              WriteSingle(Value, Dest);
            dtNumeric:
              WriteNumeric(Value, Dest);
          {$IFDEF LITE}
            dtLargeint:
              WriteBigInt(Value, Dest);
          {$ENDIF}
          else
            WriteDouble(Value, Dest);
          end;
        dtCurrency:
          WriteCurrency(Value, Dest, Connection);
        dtBoolean:
          WriteBoolean(Value, Dest);
        dtString, dtExtString, dtMemo, dtBlob:
          WriteString(Value, Dest);
        dtWideString, dtExtWideString, dtWideMemo:
          WriteWideString(Value, Dest);
        dtDate, dtPgDate:
          WriteDate(Value, Dest);
        dtTime, dtPgTime:
          WriteTime(Value, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeTZ);
        dtDateTime, dtPgTimeStamp:
          WriteTimeStamp(Value, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeStampTZ);
        dtPgInterval:
          WriteInterval(Value, Dest, Connection.FIntegerDateTimes);
        dtPgLargeObject:
          WriteOID(Value, Dest);
        dtGuid:
          WriteGuid(Value, Dest);
      {$IFNDEF LITE}
        dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
          WritePgGeometric(Value, Dest, DataType);
        dtObject:
          WritePgRow(Value, Dest, ObjectType, Connection);
      {$ENDIF}
      end;
    finally
      Dest.LeaveSizeBlock;
    end;
  end;
end;

class procedure TPgBinaryConverter.WriteSmallInt(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteInt16(Value);
end;

class procedure TPgBinaryConverter.WriteInteger(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteInt32(Value);
end;

class procedure TPgBinaryConverter.WriteBigInt(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteInt64(Value);
end;

class procedure TPgBinaryConverter.WriteDouble(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteDouble(Value);
end;

class procedure TPgBinaryConverter.WriteSingle(Value: Variant; Dest: TPgSQLNet);
begin
  Dest.WriteSingle(Value);
end;

class procedure TPgBinaryConverter.StringToNumeric(const Value: string; var Numeric: TPgNumeric);
var
  p: integer;

  procedure InvalidString;
  begin
    raise EConvertError.CreateFmt('Invalid NUMERIC string: ''%s''', [Value]);
  end;

  procedure CheckLength;
  begin
    if p > Length(Value) then
      InvalidString;
  end;

  function IsDigit(c: char): boolean;
  begin
    Result := (c >= '0') and (c <= '9');
  end;

const
  MaxDigits = 1000;
var
  HaveDp: boolean;
  Digits: array [0 .. MaxDigits + 8 - 1] of byte;
  i, j, DWeight, DScale, DDigits, Exp, Offset, NDigits: integer;
begin
  p := 1;
  while (p <= Length(Value)) and (Value[p] = ' ') do
    Inc(p);

  CheckLength;
  Numeric.Sign := NUMERIC_POS;
  case Value[p] of
    '+':
      Inc(p);
    '-': begin
      Numeric.Sign := NUMERIC_NEG;
      Inc(p);
    end;
  end;

  CheckLength;
  HaveDp := False;
  if (Value[p] = '.') or (Value[p] = {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator) then begin
    HaveDp := True;
    Inc(p);
  end;

  CheckLength;
  if not IsDigit(Value[p]) then
    InvalidString;

  Digits[0] := 0;
  Digits[1] := 0;
  Digits[2] := 0;
  Digits[3] := 0;
  i := DEC_DIGITS;
  DWeight := -1;
  DScale := 0;

  while p <= Length(Value) do begin
    if IsDigit(Value[p]) then begin
      Digits[i] := Ord(Value[p]) - Ord('0');
      Inc(i);
      Inc(p);
      if HaveDp then
        Inc(DScale)
      else
        Inc(DWeight);
    end
    else
    if (Value[p] = '.') or (Value[p] = {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator) then begin
      if HaveDp then
        InvalidString;
      HaveDp := True;
      Inc(p);
    end
    else
      break;
  end;

  DDigits := i - DEC_DIGITS;
  Digits[i] := 0;
  Digits[i + 1] := 0;
  Digits[i + 2] := 0;

  if (p <= Length(Value)) and ((Value[p] = 'e') or (Value[p] = 'E')) then begin
    Inc(p);
    i := p;
    if (i <= Length(Value)) and ((Value[i] = '+') or (Value[i] = '-')) then
      Inc(i);
    while (i <= Length(Value)) and IsDigit(Value[i]) do
      Inc(i);
    if i = p then
      InvalidString;
    exp := StrToInt(Copy(Value, p, i - p));
    p := i;
    DWeight := DWeight + exp;
    DScale := DScale - exp;
    if DScale < 0 then
      DScale := 0;
  end;

  while p <= Length(Value) do begin
    if Value[p] <> ' ' then
      InvalidString;
    Inc(p);
  end;

  if DWeight >= 0 then
    Numeric.Weight := (DWeight + 1 + DEC_DIGITS - 1) div DEC_DIGITS - 1
  else
    Numeric.Weight := -((-DWeight - 1) div DEC_DIGITS + 1);

  Offset := (Numeric.Weight + 1) * DEC_DIGITS - (DWeight + 1);
  NDigits := (DDigits + Offset + DEC_DIGITS - 1) div DEC_DIGITS;
  Numeric.NDigits := NDigits;
  Numeric.DScale := DScale;
  Numeric.StartPos := 0;

  i := DEC_DIGITS - Offset;
  j := 0;
  while NDigits > 0 do begin
    Numeric.Digits[j] := ((Digits[i] * 10 + Digits[i + 1]) * 10 +
      Digits[i + 2]) * 10 + Digits[i + 3];
    i := i + DEC_DIGITS;
    Inc(j);
    Dec(NDigits);
  end;
end;

class procedure TPgBinaryConverter.DoubleToNumeric(Value: double; var Numeric: TPgNumeric);
var
  IntPart, FracPart, res: double;
  w: word;
  IntCount, FracCount, i: integer;
begin
  if Value = 0 then begin
    Numeric.NDigits := 0;
    Numeric.Weight := 0;
    Numeric.Sign := NUMERIC_POS;
    Numeric.DScale := 0;
    exit;
  end;

  if Value < 0 then begin
    Value := -Value;
    Numeric.Sign := NUMERIC_NEG;
  end
  else
    Numeric.Sign := NUMERIC_POS;

  IntPart := {$IFNDEF CLR}System.{$ENDIF}Int(Value);
  FracPart := Value - IntPart;

  IntCount := 0;
  while IntPart > 0 do begin
    res := IntPart / 10000;
    IntPart := {$IFNDEF CLR}System.{$ENDIF}Int(res);
    res := res - IntPart;
    w := Round(res * 10000);
    Numeric.Digits[IntCount] := w;
    Inc(IntCount);
  end;

  for i := 0 to (IntCount div 2) - 1 do begin
    w := Numeric.Digits[i];
    Numeric.Digits[i] := Numeric.Digits[IntCount - i - 1];
    Numeric.Digits[IntCount - i - 1] := w;
  end;

  FracCount := 0;
  while FracPart > 0 do begin
    if IntCount + FracCount > MaxNumericWords then
      break;

    FracPart := FracPart * 10000;
    res := {$IFNDEF CLR}System.{$ENDIF}Int(FracPart);
    FracPart := FracPart - res;
    w := Trunc(res);
    Numeric.Digits[IntCount + FracCount] := w;
    Inc(FracCount);
  end;

  Numeric.NDigits := IntCount + FracCount;
  Numeric.Weight := IntCount - 1;
  Numeric.DScale := 0;
  Numeric.StartPos := 0;
end;

class procedure TPgBinaryConverter.StripNumeric(var Numeric: TPgNumeric);
var
  p, c: integer;
begin
  p := Numeric.StartPos;
  while (p < Numeric.StartPos + Numeric.NDigits) and (Numeric.Digits[p] = 0) do
    Inc(p);

  c := p - Numeric.StartPos;
  Numeric.Weight := Numeric.Weight - c;
  Numeric.NDigits := Numeric.NDigits - c;
  Numeric.StartPos := p;

  p := Numeric.StartPos + Numeric.NDigits - 1;
  while (p >= Numeric.StartPos) and (Numeric.Digits[p] = 0) do
    Dec(p);

  Numeric.NDigits := p - Numeric.StartPos + 1;

  if Numeric.NDigits = 0 then begin
    Numeric.Sign := NUMERIC_POS;
    Numeric.Weight := 0;
  end;
end;

class procedure TPgBinaryConverter.WriteNumeric(Value: Variant; Dest: TPgSQLNet);
var
  Numeric: TPgNumeric;
  d: double;
  i: integer;
  i64: int64;
begin
  case VarType(Value) of
    varSingle, varDouble: begin
      d := Value;
      DoubleToNumeric(d, Numeric);
    end;
    varSmallint,varInteger,varByte,varWord,varShortInt:
      StringToNumeric(IntToStr(Integer(Value)), Numeric);
    varLongWord, varInt64: begin
      i64 := Value;
      StringToNumeric(IntToStr(i64), Numeric);
    end;
    varCurrency:
      StringToNumeric(CurrToStr(Value), Numeric);
  else
    if VarIsStr(Value) then
      StringToNumeric(Value, Numeric)
  {$IFNDEF FPC}
    else
    if VarType(Value) = VarFMTBcd then
      StringToNumeric(BcdToStr(VarToBcd(Value)), Numeric)
  {$ENDIF}
    else
      raise Exception.Create(SCannotConvertType);
  end;

  StripNumeric(Numeric);

  Dest.WriteWord(Numeric.NDigits);
  Dest.WriteInt16(Numeric.Weight);
  Dest.WriteWord(Numeric.Sign);
  Dest.WriteWord(Numeric.DScale);

  for i := Numeric.StartPos to Numeric.StartPos + Numeric.NDigits - 1 do
    Dest.WriteWord(Numeric.Digits[i]);
end;

class procedure TPgBinaryConverter.WriteCurrency(Value: Variant; Dest: TPgSQLNet;
  Connection: TPgSQLConnection);
var
  c: Currency;
begin
  c := Value;
  if Connection.VersionIsEqualOrHigher(8,3) then
    Dest.WriteInt64(Round(c * 100))
  else
    Dest.WriteInt32(Round(c * 100));
end;

class procedure TPgBinaryConverter.WriteBoolean(Value: Variant; Dest: TPgSQLNet);
var
  b: boolean;
begin
  b := Value;

  Dest.WriteByte(Byte(b));
end;

class procedure TPgBinaryConverter.WriteString(Value: Variant; Dest: TPgSQLNet);
var
  s: AnsiString;
  Obj: TObject;
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
{$IFNDEF CLR}
  lb, hb: integer;
{$ENDIF}
begin
  case VarType(Value) of
    {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
    {$IFDEF CLR}
      Obj := Value;
    {$ELSE}
      Obj := TObject(TVarData(Value).VPointer);
    {$ENDIF}
      if not IsClass(Obj, TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      if Blob.IsUnicode then begin
        s := Blob.AsAnsiString;
        Dest.WriteBytes(TValueArr(s), 0, Length(s));
        exit;
      end;
      Piece := Blob.FirstPiece;
      while IntPtr(Piece) <> nil do begin
        BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
        Dest.WriteBytes(BlobData, 0, Piece.Used);
        Piece := Piece.Next;
      end;
    end;
  {$IFNDEF CLR}
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        Dest.WriteBytes(TValueArr(BlobData), 0, hb - lb + 1);
      finally
        VarArrayUnlock(Value);
      end;
    end;
  {$ENDIF}
  else
    if VarIsStr(Value) then
      s := AnsiString(Value)
    else
      s := AnsiString(VarToStr(Value));

    Dest.WriteBytes(TValueArr(s), 0, Length(s));
  end;
end;

class procedure TPgBinaryConverter.WriteWideString(Value: Variant; Dest: TPgSQLNet);
var
  s: AnsiString;
  Obj: TObject;
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
  Utf8Buf: TBytes;
  Count: integer;
{$IFNDEF CLR}
  MaxLen: integer;
  lb, hb: integer;
{$ELSE}
  Chars: array of char;
{$ENDIF}
begin
  case VarType(Value) of
    {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
    {$IFDEF CLR}
      Obj := Value;
    {$ELSE}
      Obj := TObject(TVarData(Value).VPointer);
    {$ENDIF}
      if not IsClass(Obj, TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      if not Blob.IsUnicode then begin
        s := UTF8Encode(Blob.AsWideString);
        Dest.WriteBytes(TValueArr(s), 0, Length(s));
        exit;
      end;
      Piece := Blob.FirstPiece;
      while IntPtr(Piece) <> nil do begin
        BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
      {$IFNDEF CLR}
        MaxLen := Piece.Used div 2 * 3 + 1;
        if Length(Utf8Buf) < MaxLen then
          SetLength(Utf8Buf, MaxLen);
        Count := CLRClasses.UnicodeToUtf8(TValueArr(Utf8Buf), MaxLen, BlobData, Piece.Used div 2);
      {$ELSE}
        SetLength(Chars, Piece.Used div 2);
        Marshal.Copy(BlobData, Chars, 0, Piece.Used div 2);
        Utf8Buf := Encoding.UTF8.GetBytes(String(Chars));
        Count := Length(Utf8Buf);
      {$ENDIF}
        Dest.WriteBytes(TValueArr(Utf8Buf), 0, Count - 1);
        Piece := Piece.Next;
      end;
    end;
  {$IFNDEF CLR}
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        Dest.WriteBytes(TValueArr(BlobData), 0, hb - lb + 1);
      finally
        VarArrayUnlock(Value);
      end;
    end;
  {$ENDIF}
  else
    if VarIsStr(Value) then
      s := UTF8Encode(Value)
    else
      s := UTF8Encode(VarToStr(Value));

    Dest.WriteBytes(TValueArr(s), 0, Length(s));
  end;
end;

class procedure TPgBinaryConverter.WriteOID(Value: Variant; Dest: TPgSQLNet);
var
  OID: integer;
begin
  VarToOID(Value, OID);
  Dest.WriteInt32(OID);
end;

class procedure TPgBinaryConverter.WriteDate(Value: Variant; Dest: TPgSQLNet);
var
  Obj: TSharedObject;
  ts: TCustomPgTimeStamp;
  NeedFree: boolean;
begin
  VarToPgDate(Value, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);
    Dest.WriteInt32(ts.Days);
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WriteTime(Value: Variant; Dest: TPgSQLNet;
  IntegerDateTimes, WithTimeZone: boolean);
var
  Obj: TSharedObject;
  ts: TCustomPgTimeStamp;
  NeedFree: boolean;
begin
  VarToPgTime(Value, WithTimeZone, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);

    if IntegerDateTimes then
      Dest.WriteInt64(ts.Ticks)
    else
      Dest.WriteDouble(ts.Ticks / McSecsPerSec);

    if WithTimeZone then
      Dest.WriteInt32(-ts.TimeZoneOffset);
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WriteTimeStamp(Value: Variant; Dest: TPgSQLNet;
  IntegerDateTimes, WithTimeZone: boolean);
var
  Obj: TSharedObject;
  ts: TCustomPgTimeStamp;
  NeedFree: boolean;
  Days: integer;
  Ticks: int64;
begin
  VarToPgTimeStamp(Value, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);
    Days := ts.Days;
    Ticks := ts.Ticks;

  {$IFDEF MSWINDOWS}
    if WithTimeZone and (Ticks <> High(Int64)) and (Ticks <> Low(Int64)) then
      LocalTimeToUTCTime(Days, Ticks);
  {$ENDIF}

    if IntegerDateTimes then begin
      if (Ticks = High(Int64)) or (Ticks = Low(Int64)) then
        Dest.WriteInt64(Ticks)
      else
        Dest.WriteInt64(Days * McSecsPerDay + Ticks)
    end
    else begin
      if Ticks = High(Int64) then
        Dest.WriteDouble(Infinity)
      else
      if Ticks = Low(Int64) then
        Dest.WriteDouble(-Infinity)
      else
        Dest.WriteDouble(Days * (SecsPerDay + 0.0) + Ticks / McSecsPerSec);
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WriteInterval(Value: Variant; Dest: TPgSQLNet;
  IntegerDateTimes: boolean);
var
  Obj: TSharedObject;
  Int: TPgInterval;
  NeedFree: boolean;
begin
  VarToPgInterval(Value, Obj, NeedFree);
  try
    Int := TPgInterval(Obj);

    if IntegerDateTimes then
      Dest.WriteInt64(Round(Int.SecondsFull * McSecsPerSec))
    else
      Dest.WriteDouble(Int.SecondsFull);

    Dest.WriteInt32(Int.Days);
    Dest.WriteInt32(Int.MonthsFull);
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WriteGuid(Value: Variant; Dest: TPgSQLNet);
var
  s: AnsiString;
begin
  s := AnsiString(Value);
  Dest.WriteBytes(TValueArr(s), 0, Length(s));
end;

{$IFNDEF LITE}
class procedure TPgBinaryConverter.WritePgGeometric(Value: Variant; Dest: TPgSQLNet;
  DataType: word);

  procedure WritePoint(Point: TPgPoint);
  begin
    Dest.WriteDouble(Point.X);
    Dest.WriteDouble(Point.Y); 
  end;

var
  i: integer;
  Obj: TSharedObject;
  PgGeometric: TPgGeometric;
  NeedFree: boolean;
begin
  VarToPgGeometric(Value, DataType, Obj, NeedFree);
  try
    PgGeometric := TPgGeometric(Obj);

    case DataType of
      dtPgPoint:
        WritePoint(TPgPoint(PgGeometric));
      dtPgLSeg:
        with TPgLSeg(PgGeometric) do begin
          WritePoint(StartPoint);
          WritePoint(EndPoint);
        end;
      dtPgBox:
        with TPgBox(PgGeometric) do begin
          WritePoint(LowerLeft);
          WritePoint(UpperRight);
        end;
      dtPgPath:
        with TPgPath(PgGeometric) do begin
          if IsClosedPath then
            i := 1
          else
            i := 0;
          Dest.WriteByte(i);
          Dest.WriteInt32(Count);
          for i := 0 to Count - 1 do
            WritePoint(Points[i]);
        end;
      dtPgPolygon:
        with TPgPolygon(PgGeometric) do begin
          Dest.WriteInt32(Count);
          for i := 0 to Count - 1 do
            WritePoint(Points[i]);
        end;
      dtPgCircle:
        with TPgCircle(PgGeometric) do begin
          WritePoint(Center);
          Dest.WriteDouble(Radius);
        end;
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class procedure TPgBinaryConverter.WritePgRow(Value: Variant; Dest: TPgSQLNet;
  ObjectType: TObjectType; Connection: TPgSQLConnection);
var
  Obj: TDBObject;
  Row: TPgRow;
  NeedFree: boolean;
  i: integer;
  Attr: TPgAttribute;
  AttrValue: variant;
begin
  VarToPgRow(Value, ObjectType, Obj, NeedFree);
  try
    Row := TPgRow(Obj);
    Dest.WriteInt32(ObjectType.AttributeCount);
    for i := 0 to ObjectType.AttributeCount - 1 do begin
      Attr := TPgAttribute(ObjectType.Attributes[i]);

      Dest.WriteInt32(Attr.TypeOID);

      AttrValue := Row.AttrValueEx(Attr.Name, True);
      WriteValue(AttrValue, Dest, Attr.DataType, Attr.SubDataType, Attr.ObjectType,
        Connection);
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;
{$ENDIF NDEF LITE}

class procedure TPgBinaryConverter.ReadValue(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
  DataType, SubDataType: word; Trim: boolean; Connection: TPgSQLConnection);
begin
  case DataType of
    dtSmallint:
      ReadInt16(Source, Dest);
    dtInteger:
      ReadInt32(Source, Dest);
    dtInt64:
      ReadInt64(Source, Dest);
    dtBoolean:
      ReadBoolean(Source, Dest);
    dtFloat:
      ReadDouble(Source, Dest, SubDataType);
    dtBCD:
      ReadBCD(Source, Dest);
  {$IFNDEF FPC}
    dtFMTBCD:
      ReadFMTBCD(Source, Dest);
  {$ENDIF}
    dtCurrency:
      ReadCurrency(Source, Dest, Connection);
    dtString, dtExtString:
      ReadString(Source, Size, Dest, Trim);
    dtWideString, dtExtWideString:
      ReadWideString(Source, Size, Dest, Trim);
    dtDate:
      ReadDate(Source, Dest);
    dtTime:
      ReadTime(Source, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeTZ);
    dtDateTime:
      ReadDateTime(Source, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeStampTZ);
  {$IFNDEF LITE}
    dtPgDate:
      ReadPgDate(Source, Dest);
    dtPgTime:
      ReadPgTime(Source, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeTZ);
    dtPgTimeStamp:
      ReadPgTimeStamp(Source, Dest, Connection.FIntegerDateTimes, SubDataType = dtPgTimeStampTZ);
    dtPgInterval:
      ReadPgInterval(Source, Dest, Connection.FIntegerDateTimes);
  {$ENDIF}
    dtMemo:
      ReadMemo(Source, Size, Dest);
    dtWideMemo:
      ReadWideMemo(Source, Size, Dest);
    dtBlob:
      ReadBlob(Source, Size, Dest);
    dtPgLargeObject:
      ReadPgLargeObject(Source, Dest);
    dtGuid:
      ReadGuid(Source, Size, Dest);
  {$IFNDEF LITE}
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
      ReadPgGeometric(Source, Size, Dest, DataType);
    dtObject:
      ReadPgRow(Source, Dest, Trim, Connection);
    dtCursor:
      ReadPgCursor(Source, Size, Dest, Connection);
  {$ENDIF}
  end;
end;

class procedure TPgBinaryConverter.ReadInt16(Source: TPgSQLNet; Dest: IntPtr);
begin
  Marshal.WriteInt16(Dest, Source.ReadInt16);
end;

class procedure TPgBinaryConverter.ReadInt32(Source: TPgSQLNet; Dest: IntPtr);
begin
  Marshal.WriteInt32(Dest, Source.ReadInt32);
end;

class procedure TPgBinaryConverter.ReadInt64(Source: TPgSQLNet; Dest: IntPtr);
begin
  Marshal.WriteInt64(Dest, Source.ReadInt64);
end;

class procedure TPgBinaryConverter.ReadDouble(Source: TPgSQLNet; Dest: IntPtr; SubDataType: word);
begin
  case SubDataType of
    dtNumeric:
      ReadNumeric(Source, Dest);
    dtPgSingle:
      ReadSingle(Source, Dest);
  {$IFDEF LITE}
    dtLargeint:
      Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Source.ReadInt64));
  {$ENDIF}
  else
    Marshal.WriteByte(Dest, 7, Source.ReadByte);
    Marshal.WriteByte(Dest, 6, Source.ReadByte);
    Marshal.WriteByte(Dest, 5, Source.ReadByte);
    Marshal.WriteByte(Dest, 4, Source.ReadByte);
    Marshal.WriteByte(Dest, 3, Source.ReadByte);
    Marshal.WriteByte(Dest, 2, Source.ReadByte);
    Marshal.WriteByte(Dest, 1, Source.ReadByte);
    Marshal.WriteByte(Dest, 0, Source.ReadByte);
  end;
end;

class procedure TPgBinaryConverter.ReadBCD(Source: TPgSQLNet; Dest: IntPtr);
var
  i, c, a: integer;
  WordCount: integer;
  Weight, Sign: integer;
  Value: int64;
  w: word;
begin
  WordCount := Source.ReadWord;
  Weight := Source.ReadInt16;
  Sign := Source.ReadWord;
  Source.ReadWord; // read scale

  Value := 0;
  if WordCount > 0 then begin
    i := 0;
    while i <= Weight do begin
      w := Source.ReadWord;
      if Weight - i <= High(DecimalArrayInt64) then begin
        if Weight - i = High(DecimalArrayInt64) then
          w := w mod 100;
        Value := Value + w * DecimalArrayInt64[Weight - i];
      end;
      Inc(i);
      if i = WordCount then
        break;
    end;

    if i < WordCount then begin
      c := i;
      if Weight < 0 then
        a := -Weight
      else
        a := 1;

      for i := a to WordCount - Weight - 1 do begin
        w := Source.ReadWord;
        if i = 1 then
          Value := Value + w;
        Inc(c);
        if c = WordCount then
          break;
      end;
    end;

    if Sign > 0 then
      Value := -Value;
  end;
  Marshal.WriteInt64(Dest, Value)
end;

{$IFNDEF FPC}
class procedure TPgBinaryConverter.ReadFMTBCD(Source: TPgSQLNet; Dest: IntPtr);
var
  i, c, a, Prec, IntDigits: integer;
  WordCount: integer;
  Weight, Sign: integer;
{$IFNDEF CLR}
  Value: TBcd;
{$ELSE}
  Value: TBytes;
{$ENDIF}
  w: word;

  procedure SaveDigit(Val: byte);
  var
    p: integer;
  begin
    p := Prec div 2;
    if not Odd(Prec) then
      Val := Val shl 4;

    {$IFNDEF CLR}
      Value.Fraction[p] := Value.Fraction[p] or Val;
    {$ELSE}
      Value[p + 2] := Value[p + 2] or Val;
    {$ENDIF}
    Inc(Prec);
  end;

  procedure Save2Digits(Val: byte; RemoveZero: boolean);
  var
    hd, ld: byte;
  begin
    hd := Val div 10;
    ld := Val mod 10;

    if not (RemoveZero and (Prec = 0) and (hd = 0)) then
      SaveDigit(hd);

    SaveDigit(ld);
  end;

  procedure Save4Digits(Val: word; RemoveZero: boolean);
  var
    hd, ld: byte;
  begin
    hd := Val div 100;
    ld := Val mod 100;

    if not (RemoveZero and (Prec = 0) and (hd = 0)) then
      Save2Digits(hd, RemoveZero);

    Save2Digits(ld, RemoveZero);
  end;

  procedure RemoveTrailZeros;
  var
    p: integer;
    Val: byte;
  begin
    while Prec > IntDigits do begin
      p := (Prec - 1) div 2;
      Val := Value{$IFNDEF CLR}.Fraction[p]{$ELSE}[p + 2]{$ENDIF};
      if Odd(Prec) then
        Val := Val shr 4
      else
        Val := Val and $0F;

      if Val = 0 then
        Dec(Prec)
      else
        break;
    end;
  end;

begin
  WordCount := Source.ReadWord;
  Weight := Source.ReadInt16;
  Sign := Source.ReadWord;
  Source.ReadWord; // read scale

{$IFNDEF CLR}
  System.FillChar(Value, SizeOfTBcd, 0);
{$ELSE}
  SetLength(Value, SizeOfTBcd);
{$ENDIF}
  Prec := 0;
  IntDigits := 0;
  if WordCount > 0 then begin
    i := 0;
    while i <= Weight do begin
      w := Source.ReadWord;
      if Weight - i < 16 then
        Save4Digits(w, True);
      Inc(i);
      if i = WordCount then begin
        Inc(Prec, (Weight - i + 1) * 4);
        break;
      end;
    end;

    IntDigits := Prec;

    if i < WordCount then begin
      c := i;
      if Weight < 0 then
        a := -Weight
      else
        a := 1;

      Prec := Prec + (a - 1) * 4;

      for i := a to WordCount - Weight - 1 do begin
        w := Source.ReadWord;
        if Prec <= 63 then
          Save4Digits(w, False);
        Inc(c);
        if c = WordCount then
          break;
      end;

      RemoveTrailZeros;
    end;
  end
  else begin
    Prec := 1;
    IntDigits := 1;
  end;

{$IFNDEF CLR}
  Value.Precision := Prec;
  Value.SignSpecialPlaces := Prec - IntDigits;
  if Sign > 0 then
    Value.SignSpecialPlaces := Value.SignSpecialPlaces or $80;
  PBcd(Dest)^ := Value;
{$ELSE}
  Value[0] := Prec;
  Value[1] := Prec - IntDigits;
  if Sign > 0 then
    Value[1] := Value[1] or $80;
  Marshal.Copy(Value, 0, Dest, SizeOfTBcd);
{$ENDIF}
end;
{$ENDIF}

class procedure TPgBinaryConverter.ReadCurrency(Source: TPgSQLNet; Dest: IntPtr;
  Connection: TPgSQLConnection);
var
  d: double;
begin
  if Connection.VersionIsEqualOrHigher(8,3) then
    d := Source.ReadInt64 / 100
  else
    d := Source.ReadInt32 / 100;
  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(d));
end;

class procedure TPgBinaryConverter.ReadSingle(Source: TPgSQLNet; Dest: IntPtr);
var
  i: integer;
  d: double;
begin
  i := Source.ReadInt32;
{$IFNDEF CLR}
  d := PSingle(@i)^;
{$ELSE}
  d := BitConverter.ToSingle(BitConverter.GetBytes(i), 0);
{$ENDIF}

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(d));
end;

class procedure TPgBinaryConverter.ReadNumeric(Source: TPgSQLNet; Dest: IntPtr);
var
  i, c, a: integer;
  WordCount: integer;
  Weight, Sign: integer;
  Value: double;
  w: word;
begin
  WordCount := Source.ReadWord;
  Weight := Source.ReadInt16;
  Sign := Source.ReadWord;
  Source.ReadWord; // read scale

  Value := 0;
  if WordCount > 0 then begin
    i := 0;
    while i <= Weight do begin
      w := Source.ReadWord;
      Value := Value + w * DecimalArray[Weight - i];
      Inc(i);
      if i = WordCount then
        break;
    end;

    if i < WordCount then begin
      c := i;
      if Weight < 0 then
        a := -Weight
      else
        a := 1;

      for i := a to WordCount - Weight - 1 do begin
        w := Source.ReadWord;
        if i < MaxDecimalWords then
          Value := Value + w / DecimalArray[i];
        Inc(c);
        if c = WordCount then
          break;
      end;
    end;

    if Sign > 0 then
      Value := -Value;
  end;
  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Value));
end;

class procedure TPgBinaryConverter.ReadBoolean(Source: TPgSQLNet; Dest: IntPtr);
begin
  Marshal.WriteInt16(Dest, Source.ReadByte);
end;

class procedure TPgBinaryConverter.ReadBlob(Source: TPgSQLNet; Size: integer;
  Dest: IntPtr);
var
  Blob: TBlob;
  BlobData: IntPtr;
  Piece: PPieceHeader;
begin
  if Size = 0 then
    Exit;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Blob.AllocPiece(Piece, Size);
  try
    BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
    Source.ReadBytes(BlobData, 0, Size);
    Piece.Used := Size;
    Blob.AppendPiece(Piece);
  except
    Blob.FreePiece(Piece);
  end;
end;

class procedure TPgBinaryConverter.ReadPgLargeObject(Source: TPgSQLNet; Dest: IntPtr);
var
  Obj: TPgSQLLargeObject;
begin
  Obj := TPgSQLLargeObject(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Obj.OID := Source.ReadInt32;
  TPgObjectsUtils.LargeObjectResetOldOID(Obj);
end;

class procedure TPgBinaryConverter.ReadDate(Source: TPgSQLNet; Dest: IntPtr);
var
  Days: integer;
  Date: TDateTime;
begin
  Days := Source.ReadInt32;
  Date := PostgresBaseDate + Days;
  if Date < MinDateTime then
    Date := MinDateTime;

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

class procedure TPgBinaryConverter.ReadTime(Source: TPgSQLNet; Dest: IntPtr;
  IntegerDateTimes, WithTimeZone: boolean);
var
  Seconds: double;
  Date: TDateTime;
begin
  if IntegerDateTimes then
    Seconds := Source.ReadInt64 / 1E6
  else
    Seconds := Source.ReadDouble;

  Date := Seconds / SecsPerDay;

  if WithTimeZone then
    Source.ReadInt32; // time zone (seconds)

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

class procedure TPgBinaryConverter.ReadDateTime(Source: TPgSQLNet; Dest: IntPtr;
  IntegerDateTimes, WithTimeZone: boolean);
var
  t: Int64;
  Seconds: double;
  Date: TDateTime;
begin
  if IntegerDateTimes then begin
    t := Source.ReadInt64;
    if t = High(Int64) then
      Seconds := Infinity
    else
    if t = Low(Int64) then
      Seconds := -Infinity
    else
      Seconds := t / 1000000;
  end
  else
    Seconds := Source.ReadDouble;

  if Seconds = Infinity then
    Date := MaxDateTime
  else
  if Seconds = -Infinity then
    Date := MinDateTime
  else begin
    Date := AddTimeSpan(PostgresBaseDate, Seconds / SecsPerDay);
    if Date < MinDateTime then
      Date := MinDateTime
  {$IFDEF MSWINDOWS}
    else
    if WithTimeZone then begin
      Date := UTCTimeToLocalTime(Date);
    end;
  {$ENDIF}
  end;

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

{$IFNDEF LITE}
class procedure TPgBinaryConverter.ReadPgDate(Source: TPgSQLNet; Dest: IntPtr);
var
  d: TPgDate;
begin
  d := TPgDate(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  d.Days := Source.ReadInt32;
end;

class procedure TPgBinaryConverter.ReadPgTimeStamp(Source: TPgSQLNet; Dest: IntPtr;
  IntegerDateTimes, WithTimeZone: boolean);
var
  t: Int64;
  Seconds: double;
  ts: TPgTimeStamp;
  Days: integer;
  Ticks: int64;
begin
  ts := TPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));

  if IntegerDateTimes then begin
    t := Source.ReadInt64;
    if (t = High(Int64)) or (t = Low(Int64)) then
      ts.Ticks := t
    else begin
      ts.Ticks := t mod McSecsPerDay;
      if ts.Ticks < 0 then
        ts.Ticks := ts.Ticks + McSecsPerDay;
      ts.Days := (t - ts.Ticks) div McSecsPerDay;
    end;
  end
  else begin
    Seconds := Source.ReadDouble;
    if Seconds = Infinity then
      ts.Ticks := High(Int64)
    else
    if Seconds = -Infinity then
      ts.Ticks := Low(Int64)
    else begin
      ts.Days := Floor(Seconds / SecsPerDay);
      ts.Ticks := Round((Seconds - ts.Days * (SecsPerDay + 0.0)) * McSecsPerSec);
    end;
  end;
  if WithTimeZone then begin
    ts.HasTimeZone := True;
  {$IFDEF MSWINDOWS}
    if (ts.Ticks <> High(Int64)) and (ts.Ticks <> Low(Int64)) then begin
      Days := ts.Days;
      Ticks := ts.Ticks;
      UTCTimeToLocalTime(Days, Ticks);
      ts.Days := Days;
      ts.Ticks := Ticks;
    end;
  {$ENDIF}
  end;
end;

class procedure TPgBinaryConverter.ReadPgTime(Source: TPgSQLNet; Dest: IntPtr;
  IntegerDateTimes, WithTimeZone: boolean);
var
  Seconds: double;
  ts: TPgTime;
begin
  ts := TPgTime(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));

  if IntegerDateTimes then begin
    ts.Ticks := Source.ReadInt64;
  end
  else begin
    Seconds := Source.ReadDouble;
    ts.Ticks := Round(Seconds * McSecsPerSec);
  end;
  if WithTimeZone then begin
    ts.HasTimeZone := True;
    ts.TimeZoneOffset := -Source.ReadInt32;
  end;
end;

class procedure TPgBinaryConverter.ReadPgInterval(Source: TPgSQLNet; Dest: IntPtr;
  IntegerDateTimes: boolean);
var
  Interval: TPgInterval;
begin
  Interval := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  if IntegerDateTimes then
    Interval.SecondsFull := Source.ReadInt64 / McSecsPerSec
  else
    Interval.SecondsFull := Source.ReadDouble;

  Interval.Days := Source.ReadInt32;
  Interval.MonthsFull := Source.ReadInt32;
end;

class procedure TPgBinaryConverter.ReadPgGeometric(Source: TPgSQLNet; Size: integer;
  Dest: IntPtr; DataType: word);

  procedure ReadPoint(Point: TPgPoint);
  begin
    Point.X := Source.ReadDouble;
    Point.Y := Source.ReadDouble;
  end;

var
  i: integer;
  PgGeometric: TPgGeometric;
begin
  PgGeometric := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  case DataType of
    dtPgPoint:
      ReadPoint(TPgPoint(PgGeometric));
    dtPgLSeg:
      with TPgLSeg(PgGeometric) do begin
        ReadPoint(StartPoint);
        ReadPoint(EndPoint);
      end;
    dtPgBox:
      with TPgBox(PgGeometric) do begin
        ReadPoint(LowerLeft);
        ReadPoint(UpperRight);
      end;
    dtPgPath:
      with TPgPath(PgGeometric) do begin
        i := Source.ReadByte;
        IsClosedPath := i <> 0;
        Count := Source.ReadInt32;
        for i := 0 to Count - 1 do
          ReadPoint(Points[i]);
      end;
    dtPgPolygon:
      with TPgPath(PgGeometric) do begin
        Count := Source.ReadInt32;
        for i := 0 to Count - 1 do
          ReadPoint(Points[i]);
      end;
    dtPgCircle:
      with TPgCircle(PgGeometric) do begin
        ReadPoint(Center);
        Radius := Source.ReadDouble;
      end;
  end;
end;

class procedure TPgBinaryConverter.ReadPgRow(Source: TPgSQLNet; Dest: IntPtr;
  Trim: boolean; Connection: TPgSQLConnection);
var
  Row: TPgRow;
  RowType: TPgRowType;
  Buffer, IndPtr, FieldBuf, ReadBuf, StrBuf: IntPtr;
  Count, i, Size: integer;
  Attr: TPgAttribute;
begin
  Row := TPgRow(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  RowType := Row.RowType;
  Buffer := Row.Buffer;
  IndPtr := PtrOffset(Buffer, RowType.Size);

  Count := Source.ReadInt32;
  for i := 0 to Count - 1 do begin
    Attr := TPgAttribute(RowType.Attributes[i]);

    Source.ReadInt32; // attr type OID
    Size := Source.ReadInt32;
    if Size >= 0 then begin
      FieldBuf := PtrOffset(Buffer, Attr.Offset);
      case Attr.DataType of
        dtExtString: begin
          StrBuf := Marshal.AllocHGlobal(Size + 1);
          ReadBuf := StrBuf;
        end;
        dtExtWideString: begin
          StrBuf := Marshal.AllocHGlobal((Size + 1) * 2);
          ReadBuf := StrBuf;
        end;
      else
        StrBuf := nil;
        ReadBuf := FieldBuf;
      end;
      ReadValue(Source, Size, ReadBuf, Attr.DataType, Attr.SubDataType, Trim, Connection);
      if StrBuf <> nil then
        Marshal.WriteIntPtr(FieldBuf, StrBuf);
      Marshal.WriteByte(IndPtr, i, 0);
    end
    else
      Marshal.WriteByte(IndPtr, i, Byte(-1));
  end;
end;
{$ENDIF NDEF LITE}

{ TPgTextConverter }

class function TPgTextConverter.EscapeString(const Value: AnsiString;
  StringQuote, ByteaQuote, LoaderQuote: boolean): AnsiString;
var
  i, Start: integer;
  sb: AnsiStringBuilder;
  c: AnsiChar;
begin
  if not (StringQuote or ByteaQuote or LoaderQuote) then begin
    Result := Value;
    exit;
  end;

  sb := AnsiStringBuilder.Create(Length(Value) * 2);
  try
    if StringQuote then
      sb.Append('''');
    Start := 1;
    for i := 1 to Length(Value) do begin
      c := Value[i];
      if (Ord(c) < 32) or (ByteaQuote and (Ord(c) > 126))
        or (c = '\') or StringQuote and (c = '''')
      then begin
        sb.Append(Value, Start - 1, i - Start);
        if c = '\' then begin
          if ByteaQuote and (StringQuote or LoaderQuote) then
            sb.Append('\\\\')
          else
            sb.Append('\\');
        end
        else
        if c = '''' then
          sb.Append('''''')
        else begin
          if ByteaQuote and (StringQuote or LoaderQuote) then
            sb.Append('\\')
          else
            sb.Append('\');
          sb.Append(AnsiChar((Ord(c) shr 6) and 3 + 48));
          sb.Append(AnsiChar((Ord(c) shr 3) and 7 + 48));
          sb.Append(AnsiChar(Ord(c) and 7 + 48));
        end;
        Start := i + 1;
      end;
    end;
    sb.Append(Value, Start - 1, Length(Value) - Start + 1);
    if StringQuote then
      sb.Append('''');
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class procedure TPgTextConverter.ReadValue(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
  DataType, SubDataType: word; Trim: boolean; Connection: TPgSQLConnection);
begin
  case DataType of
    dtSmallint:
      ReadInt16(Source, Size, Dest);
    dtInteger:
      ReadInt32(Source, Size, Dest);
    dtInt64:
      ReadInt64(Source, Size, Dest);
    dtBoolean:
      ReadBoolean(Source, Size, Dest);
    dtFloat:
      ReadDouble(Source, Size, Dest);
    dtBCD:
      ReadBCD(Source, Size, Dest);
  {$IFNDEF FPC}
    dtFMTBCD:
      ReadFMTBCD(Source, Size, Dest);
  {$ENDIF}
    dtCurrency:
      ReadCurrency(Source, Size, Dest);
    dtString, dtExtString:
      ReadString(Source, Size, Dest, Trim);
    dtWideString, dtExtWideString:
      ReadWideString(Source, Size, Dest, Trim);
    dtDate:
      ReadDate(Source, Size, Dest);
    dtTime:
      ReadTime(Source, Size, Dest);
    dtDateTime:
      ReadDateTime(Source, Size, Dest);
  {$IFNDEF LITE}
    dtPgDate:
      ReadPgDate(Source, Size, Dest);
    dtPgTime:
      ReadPgTime(Source, Size, Dest, SubDataType = dtPgTimeTZ);
    dtPgTimeStamp:
      ReadPgTimeStamp(Source, Size, Dest, SubDataType = dtPgTimeStampTZ);
    dtPgInterval:
      ReadPgInterval(Source, Size, Dest);
  {$ENDIF}
    dtMemo:
      ReadMemo(Source, Size, Dest);
    dtWideMemo:
      ReadWideMemo(Source, Size, Dest);
    dtBlob:
      ReadBlob(Source, Size, Dest);
    dtPgLargeObject:
      ReadPgLargeObject(Source, Size, Dest);
    dtGuid:
      ReadGuid(Source, Size, Dest);
  {$IFNDEF LITE}
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
      ReadPgGeometric(Source, Size, Dest);
    dtObject:
      ReadPgRow(Source, Size, Dest);
    dtCursor:
      ReadPgCursor(Source, Size, Dest, Connection);
  {$ENDIF}
  end;
end;

class function TPgTextConverter.GetInt64(Source: TPgSQLNet; Size: integer): Int64;
var
  s: string;
begin
  s := string(Source.ReadString(Size));
  Result := StrToInt64(s);
end;

class procedure TPgTextConverter.ReadInt16(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
begin
  Marshal.WriteInt16(Dest, GetInt64(Source, Size));
end;

class procedure TPgTextConverter.ReadInt32(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
begin
  Marshal.WriteInt32(Dest, GetInt64(Source, Size));
end;

class procedure TPgTextConverter.ReadInt64(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
begin
  Marshal.WriteInt64(Dest, GetInt64(Source, Size));
end;

class procedure TPgTextConverter.ReadDouble(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  Value: Double;
  s: string;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
  s := string(Source.ReadString(Size));
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
  Value := StrToFloat(s, FmtSet);
{$ELSE}
  if DecimalSeparator <> '.' then
    s := StringReplace(s, '.', DecimalSeparator, []);
  Value := StrToFloat(s);
{$ENDIF}

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Value));
end;

class procedure TPgTextConverter.ReadBCD(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  s: string;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
  Value: currency;
  i64: int64;
begin
  s := string(Source.ReadString(Size));
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
  Value := StrToCurr(s, FmtSet);
{$ELSE}
  if DecimalSeparator <> '.' then
    s := StringReplace(s, '.', DecimalSeparator, []);
  Value := StrToCurr(s);
{$ENDIF}
{$IFDEF CLR}
  i64 := Decimal.ToOACurrency(Value);
{$ELSE}
  i64 := PInt64(@Value)^;
{$ENDIF}
  Marshal.WriteInt64(Dest, i64);
end;

{$IFNDEF FPC}
class procedure TPgTextConverter.ReadFMTBCD(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  s: string;
  Value: TBcd;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  s := string(Source.ReadString(Size));
  if {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
    s := StringReplace(s, '.', {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator, []);
  Value := StrToBcd(s);
{$IFDEF CLR}
  b := TBcd.ToBytes(Value);
  Marshal.Copy(b, 0, Dest, SizeOfTBcd);
{$ELSE}
  PBcd(Dest)^ := Value;
{$ENDIF}
end;
{$ENDIF}

class procedure TPgTextConverter.ReadCurrency(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  c: AnsiChar;
  i: integer;
  IsNegative: boolean;
  Value: integer;
begin
  Value := 0;
  IsNegative := False;
  for i := 1 to Size do begin
    c := Source.ReadChar;
    case c of
      '-':
        IsNegative := True;
      '+', '$', ',', '.':;
      '0'..'9':
        Value := Value * 10 + Ord(c) - Ord('0');
    end;
  end;
  if IsNegative then
    Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(- Value / 100))
  else
    Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Value / 100))
end;

class procedure TPgTextConverter.ReadBoolean(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  s: string;
  b: boolean;
begin
  s := string(Source.ReadString(Size));
  b := (s = 't');
  Marshal.WriteInt16(Dest, Smallint(b));
end;

class procedure TPgTextConverter.ReadBlob(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  Blob: TBlob;
  BlobData: IntPtr;
  Piece: PPieceHeader;
  ReadPos: integer;
  WritePos: integer;
begin
  if Size = 0 then
    Exit;

  Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Blob.AllocPiece(Piece, Size);
  try
    BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));

    Source.ReadBytes(BlobData, 0, Size);

    ReadPos := 0;
    WritePos := 0;
    while ReadPos < Size do begin
      if Marshal.ReadByte(BlobData, ReadPos) = Ord('\') then begin
        if Marshal.ReadByte(BlobData, ReadPos + 1) = Ord('\') then begin
          Marshal.WriteByte(BlobData, WritePos, Ord('\'));
          Inc(ReadPos);
        end  
        else begin
          Marshal.WriteByte(BlobData, WritePos,
            (Marshal.ReadByte(BlobData, ReadPos + 1) - 48) * 64 +
            (Marshal.ReadByte(BlobData, ReadPos + 2) - 48) * 8 +
            Marshal.ReadByte(BlobData, ReadPos + 3) - 48);
          Inc(ReadPos, 3);
        end;
      end
      else
      if ReadPos <> WritePos then
        Marshal.WriteByte(BlobData, WritePos, Marshal.ReadByte(BlobData, ReadPos));
      Inc(ReadPos);
      Inc(WritePos);
    end;

    Piece.Used := WritePos;
    Blob.CompressPiece(Piece);
    Blob.AppendPiece(Piece);
  except
    Blob.FreePiece(Piece);
  end;  
end;

class procedure TPgTextConverter.ReadPgLargeObject(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  Obj: TPgSQLLargeObject;
begin
  Obj := TPgSQLLargeObject(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  Obj.OID := GetInt64(Source, Size);
  TPgObjectsUtils.LargeObjectResetOldOID(Obj);
end;

class procedure TPgTextConverter.ReadDate(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  s: string;
  Days: integer;
  Date: TDateTime;
begin
  s := string(Source.ReadString(Size));
  TPgDate.FromString(s, Days, True);
  Date := TPgDate.ToDateTime(Days);

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

class procedure TPgTextConverter.ReadTime(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  s: string;
  Ticks: int64;
  TZOffset: integer;
  Date: TDateTime;
begin
  s := string(Source.ReadString(Size));
  TPgTime.FromString(s, Ticks, TZOffset, True);
  Date := TPgTime.ToDateTime(Ticks);

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

class procedure TPgTextConverter.ReadDateTime(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  s: string;
  Days: integer;
  Ticks: int64;
  Date: TDateTime;
begin
  s := string(Source.ReadString(Size));
  TPgTimeStamp.FromString(s, Days, Ticks, True);
  Date := TPgTimeStamp.ToDateTime(Days, Ticks);

  Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
end;

{$IFNDEF LITE}
class procedure TPgTextConverter.ReadPgDate(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  d: TPgDate;
  s: string;
  Days: integer;
begin
  d := TPgDate(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := string(Source.ReadString(Size));
  TPgDate.FromString(s, Days, True);
  d.Days := Days;
end;

class procedure TPgTextConverter.ReadPgTime(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
  WithTimeZone: boolean);
var
  ts: TPgTime;
  s: string;
  tz: integer;
  Ticks: int64;
begin
  ts := TPgTime(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := string(Source.ReadString(Size));
  TPgTime.FromString(s, Ticks, tz, True);
  ts.Ticks := Ticks;
  if WithTimeZone then begin
    ts.HasTimeZone := True;
    ts.TimeZoneOffset := tz;
  end;
end;

class procedure TPgTextConverter.ReadPgTimeStamp(Source: TPgSQLNet; Size: integer; Dest: IntPtr;
  WithTimeZone: boolean);
var
  ts: TPgTimeStamp;
  s: string;
  Days: integer;
  Ticks: int64;
begin
  ts := TPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := string(Source.ReadString(Size));
  TPgTimeStamp.FromString(s, Days, Ticks, True);
  ts.Days := Days;
  ts.Ticks := Ticks;

  if WithTimeZone then
    ts.HasTimeZone := True;
end;

class procedure TPgTextConverter.ReadPgInterval(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  Interval: TPgInterval;
  s: string;
  Months, Days: integer;
  Seconds: double;
begin
  s := string(Source.ReadString(Size));
  Interval := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  TPgInterval.FromString(s, Months, Days, Seconds);
  Interval.MonthsFull := Months;
  Interval.Days := Days;
  Interval.SecondsFull := Seconds;
end;

class procedure TPgTextConverter.ReadPgGeometric(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  Obj: TPgGeometric;
  s: string;
begin
  Obj := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := string(Source.ReadString(Size));
  Obj.AsString := s;
end;

class procedure TPgTextConverter.ReadPgRow(Source: TPgSQLNet; Size: integer; Dest: IntPtr);
var
  Obj: TPgRow;
  s: string;
begin
  Obj := TPgRow(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  s := string(Source.ReadString(Size));
  Obj.AsString := s;
end;
{$ENDIF}

class function TPgTextConverter.ValueToText(const Value: variant; DataType: word;
  UseUnicode: boolean; Quote: boolean = True; LoaderQuote: boolean = False): AnsiString; // UTF8

  procedure ReplaceSeparator;
  var
    p: integer;
  begin
    if {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then begin
      p := Pos(AnsiString({$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator), Result);
      if p > 0 then
        Result[p] := '.';
    end;
  end;

var
  i64: int64;
{$IFNDEF FPC}
  bcd: TBcd;
{$ENDIF}
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
{$ENDIF}
  case DataType of
    dtSmallint: begin
      Result := AnsiString(IntToStr(Smallint(Value)));
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtInteger: begin
      Result := AnsiString(IntToStr(Integer(Value)));
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtLargeInt: begin
      i64 := Value;
      Result := AnsiString(IntToStr(i64));
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtFloat: begin
      Result := AnsiString(FloatToStr(Extended(Value){$IFDEF VER7P}, FmtSet{$ENDIF}));
    {$IFNDEF VER7P}
      ReplaceSeparator;
    {$ENDIF}
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
    dtCurrency: begin
      Result := AnsiString(FormatFloat('#################.##', Extended(Value){$IFDEF VER7P}, FmtSet{$ENDIF}));
    {$IFNDEF VER7P}
      ReplaceSeparator;
    {$ENDIF}
    end;
    dtBCD: begin
      Result := AnsiString(CurrToStr(Currency(Value){$IFDEF VER7P}, FmtSet{$ENDIF}));
    {$IFNDEF VER7P}
      ReplaceSeparator;
    {$ENDIF}
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
  {$IFNDEF FPC}
    dtFMTBCD: begin
      VarToFmtBcd(Value, Bcd);
      Result := AnsiString(BcdToStr(Bcd));
      ReplaceSeparator;
    {$IFNDEF LITE}
      Quote := False;
    {$ENDIF}
    end;
  {$ENDIF}
    dtBoolean:
      Result := BooleanToText(Value);
    dtUnknown, dtString, dtWideString, dtExtString, dtExtWideString:
      if UseUnicode then
        Result := UTF8Encode(WideString(Value))
      else
        Result := AnsiString(Value);
    dtBlob, dtMemo, dtWideMemo:
      Result := BlobToText(Value, (DataType <> dtBlob) and UseUnicode);
    dtDate, dtPgDate:
      Result := DateToText(Value);
    dtTime, dtPgTime, dtPgTimeTZ:
      Result := TimeToText(Value);
    dtDateTime, dtPgTimeStamp, dtPgTimeStampTZ:
      Result := TimeStampToText(Value);
    dtPgLargeObject: begin
      Result := LargeObjectToText(Value);
      Quote := False;
    end;
    dtGuid:
      Result := GUIDToText(Value);
  {$IFNDEF LITE}
    dtPgInterval:
      Result := IntervalToText(Value);
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
      Result := PgGeometricToText(Value, DataType);
    dtObject:
      Result := PgRowToText(Value);
  {$ENDIF}
    dtCursor:
      Result := CursorToText(Value, UseUnicode);
  else
    Assert(False);
  end;

  Result := EscapeString(Result, Quote, DataType = dtBlob, LoaderQuote);
end;

class function TPgTextConverter.BooleanToText(const Value: variant): AnsiString;
var
  b: boolean;
begin
  b := Value;
  if b then
    Result := 't'
  else
    Result := 'f';
end;

class function TPgTextConverter.BlobToText(const Value: variant; UseUnicode: boolean): AnsiString;
var
  Obj: TObject;
  Blob: TBlob;
  ws: WideString;
{$IFNDEF CLR}
  BlobData: IntPtr;
  lb, hb: integer;
{$ENDIF}
begin
  case VarType(Value) of
    {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
    {$IFDEF CLR}
      Obj := Value;
    {$ELSE}
      Obj := TObject(TVarData(Value).VPointer);
    {$ENDIF}
      if not IsClass(Obj, TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      if UseUnicode then begin
        ws := Blob.AsWideString;
        Result := UTF8Encode(ws);
      end
      else
        Result := Blob.AsAnsiString;
    end;
  {$IFNDEF CLR}
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        SetLength(Result, hb - lb + 1);
        Move(BlobData^, IntPtr(Result)^, hb - lb + 1);
      finally
        VarArrayUnlock(Value);
      end;
    end;
  {$ENDIF}
  else
    if VarIsStr(Value) then begin
      if UseUnicode then
        Result := UTF8Encode(Value)
      else
        Result := AnsiString(Value);
    end
    else
      raise Exception.Create(SCannotConvertType);
  end;
end;

class function TPgTextConverter.DateToText(const Value: variant): AnsiString;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  ts: TCustomPgTimeStamp;
begin
  VarToPgDate(Value, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);
    Result := AnsiString(TPgDate.ConvertToString(ts.Days, 'YYYY-MM-DD'));
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class function TPgTextConverter.TimeToText(const Value: variant): AnsiString;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  ts: TCustomPgTimeStamp;
begin
  VarToPgTime(Value, True, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);
    Result := AnsiString(TPgTime.ConvertToString(ts.Ticks, True, ts.TimeZoneOffset, 'HH:NN:SS.ZZZZZZ'));
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class function TPgTextConverter.TimeStampToText(const Value: variant): AnsiString;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  ts: TCustomPgTimeStamp;
  tz: integer;
begin
  VarToPgTimeStamp(Value, Obj, NeedFree);
  try
    ts := TCustomPgTimeStamp(Obj);

    if ts.Ticks = High(Int64) then
      Result := 'infinity'
    else
    if ts.Ticks = Low(Int64) then
      Result := '-infinity'
    else begin
    {$IFDEF MSWINDOWS}
      tz := GetUTCOffset(TPgTimeStamp.ToDateTime(ts.Days, ts.Ticks));
    {$ELSE}
      tz := 0;
    {$ENDIF}
      Result := AnsiString(TPgTimeStamp.ConvertToString(ts.Days, ts.Ticks, True, tz,
        'YYYY-MM-DD HH:NN:SS.ZZZZZZ'));
    end;
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class function TPgTextConverter.IntervalToText(const Value: variant): AnsiString;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  Int: TPgInterval;
begin
  VarToPgInterval(Value, Obj, NeedFree);
  try
    Int := TPgInterval(Obj);
    Result := AnsiString(TPgInterval.ConvertToString(Int.MonthsFull, Int.Days, Int.SecondsFull));
  finally
    if NeedFree then
      Obj.Free;
  end;
end;

class function TPgTextConverter.LargeObjectToText(const Value: variant): AnsiString;
var
  OID: integer;
begin
  VarToOID(Value, OID);
  Result := AnsiString(IntToStr(OID));
end;

class function TPgTextConverter.GUIDToText(const Value: variant): AnsiString;
begin
  Result := AnsiString(Value);
end;

{$IFNDEF LITE}
class function TPgTextConverter.PgGeometricToText(const Value: variant; DataType: word): AnsiString;
var
  Obj: TSharedObject;
  NeedFree: boolean;
  PgGeometric: TPgGeometric;
begin
  VarToPgGeometric(Value, DataType, Obj, NeedFree);
  PgGeometric := TPgGeometric(Obj);
  Result := AnsiString(PgGeometric.AsString);
end;

class function TPgTextConverter.PgRowToText(const Value: variant): AnsiString;
var
  Obj: TDBObject;
  NeedFree: boolean;
  PgRow: TPgRow;
begin
  VarToPgRow(Value, nil, Obj, NeedFree);
  Assert(not NeedFree);
  PgRow := TPgRow(Obj);
  Result := AnsiString(PgRow.AsString);
end;
{$ENDIF}

class function TPgTextConverter.CursorToText(const Value: variant; UseUnicode: boolean): AnsiString;
var
  CursorName: _string;
begin
  VarToCursor(Value, CursorName);
  if UseUnicode then
    Result := UTF8Encode(CursorName)
  else
    Result := AnsiString(CursorName);
end;

{ TPgSQLConnection }

constructor TPgSQLConnection.Create;
begin
  inherited;

  FProtocolVersion := pv30;
  FTypes := TPgSQLTypes.Create(Self);
end;

destructor TPgSQLConnection.Destroy;
begin
  Disconnect;

  FCommand.Free;
  FTypes.Free;
  FProtocol.Free;
  FFetchConnection.Free;
{$IFNDEF LITE}
  FAuxConnection.Free;
  FNotificationsHandler.Free;
{$ENDIF}

  inherited;
end;

function TPgSQLConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prPort:
      FPort := Value;
    prUseUnicode:
      if Value <> FUseUnicode then begin
        FUseUnicode := Value;
        if GetConnected then
          SetClientEncoding;
      end;
    prCharset:
      if Value <> FCharset then begin
        FCharset := Trim(Value);
        if GetConnected then
          SetClientEncoding;
      end;
    prSchema: begin
      if Value <> FSchema then begin
        if GetConnected then
          SetCurrentSchema(Value);
        FSchema := Value;
      end;
    end;
    prProtocolVersion:
      FProtocolVersion := TProtocolVersion(Value);
    prEnablePgTimeStamps:
      FEnablePgTimeStamps := Value;
    prIntervalAsString:
      FIntervalAsString := Value;
    prEnableGeometrics:
      FEnableGeometrics := Value;
    prEnableComposites:
      FEnableComposites := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prSSL_Mode:
      FSSL_Mode := TSSLMode(Integer(Value));
    prSSL_CACert:
      FSSL_CACert := Value;
    prSSL_Cert:
      FSSL_Cert := Value;
    prSSL_Key:
      FSSL_Key := Value;
    prSSL_CipherList:
      FSSL_CipherList := Value;
    prApplicationName:
      FApplicationName := Value;  
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TPgSQLConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prSchema:
      Value := FSchema;
    prUseUnicode:
      Value := FUseUnicode;
    prProtocolVersion:
      Value := Variant(FProtocolVersion);
    prEnablePgTimeStamps:
      Value := FEnablePgTimeStamps;
    prIntervalAsString:
      Value := FIntervalAsString;
    prEnableGeometrics:
      Value := FEnableGeometrics;
    prEnableComposites:
      Value := FEnableComposites;
  else
    Result := inherited GetProp(Prop, Value);
  end
end;

function TPgSQLConnection.CheckIsValid: boolean;
begin
  if not FIsValid then
    Result := False
  else begin
    try
      FProtocol.Ping;
    except
      FIsValid := False;
    end;
    Result := FIsValid;
  end;
end;

{$IFNDEF LITE}
procedure TPgSQLConnection.ReturnToPool;
begin
  inherited;

  FOnNotice := nil;
  FOnNotification := nil;
end;
{$ENDIF}

procedure TPgSQLConnection.Connect(const ConnectString: _string);

  procedure InternalConnect(ProtocolClass: TPgSQLProtocolClass;
    SuppressError: boolean);
  var
    Port: integer;
    Database: AnsiString;
    UserName: AnsiString;
    Password: AnsiString;
    ApplicationName: AnsiString;
  begin
    FProtocol := ProtocolClass.Create;
    try
      FProtocol.SetSSLOptions(_TSSLMode(FSSL_Mode),
        {$IFNDEF LITE}FIOHandler{$ELSE}nil{$ENDIF},
        FSSL_CACert, FSSL_Cert, FSSL_Key, FSSL_CipherList);

      Port := FPort;
      if Port = 0 then
        Port := PG_DEFAULT_PORT;

      if FUseUnicode then begin
        Database := UTF8Encode(FDatabase);
        UserName := UTF8Encode(FUserName);
        Password := UTF8Encode(FPassword);
        ApplicationName := UTF8Encode(FApplicationName);
      end
      else begin
        Database := AnsiString(FDatabase);
        UserName := AnsiString(FUserName);
        Password := AnsiString(FPassword);
        ApplicationName := AnsiString(FApplicationName);
      end;
      FProtocol.Connect(AnsiString(FServer), Port, FConnectionTimeout,
        Database, UserName, Password, ApplicationName);
    except
      on E: Exception do begin
        FProtocol.Free;
        FProtocol := nil;
{$IFNDEF CLR}
        E.Message := SCannotConnect + #13 + E.Message;
{$ENDIF}        
        if E is EPgError then begin
          if not SuppressError then
            ProcessInternalException(EPgError(E), Component);
        end
        else
          raise;
      end;
    end;
  end;

begin
  if FConnected then
    Exit;


  try
    case FProtocolVersion of
      pv20:
        InternalConnect(TPgSQLProtocol20, False);
      pv30:
        InternalConnect(TPgSQLProtocol30, False);
    else
      Assert(False);
    end;

    FConnected := True;
    FNativeConnection := True;

    FProtocol.OnNotice := DoOnNotice;
    FProtocol.OnNotification := DoOnNotification;

    SetClientEncoding;
    GetIntegerDateTimes;

    if FSchema <> '' then
      SetCurrentSchema(FSchema);

    inherited;

  except
    on EFailOver do;
    else begin
      try
        Disconnect;
      except
      end;
      raise;
    end
  end;
end;

procedure TPgSQLConnection.Disconnect;
begin
  if not FConnected then
    exit;

  FCachedSchema := '';
  FConnected := False;

  if FNativeConnection then
    try
      FProtocol.Disconnect;
    finally
      FProtocol.Free;
      FProtocol := nil;
    end;

  if FFetchConnection <> nil then
    FFetchConnection.Disconnect;
{$IFNDEF LITE}
  if FAuxConnection <> nil then
    FAuxConnection.Disconnect;
{$ENDIF}

  inherited;
end;

procedure TPgSQLConnection.Assign(Source: TCRConnection);
var
  Src: TPgSQLConnection;
begin
  inherited;

  Src := TPgSQLConnection(Source);
  FDatabase := Src.FDatabase;
  FPort := Src.FPort;
  FProtocolVersion := Src.FProtocolVersion;
  FConnectionTimeout := Src.FConnectionTimeout;
  FSSL_Mode := Src.FSSL_Mode;
  FSSL_CACert := Src.FSSL_CACert;
  FSSL_Cert := Src.FSSL_Cert;
  FSSL_Key := Src.FSSL_Key;
  FSSL_CipherList := Src.FSSL_CipherList;
  FUseUnicode := Src.FUseUnicode;
  FCharset := Src.FCharset;
  FSchema := Src.FSchema;
  FEnablePgTimeStamps := Src.FEnablePgTimeStamps;
  FIntervalAsString := Src.FIntervalAsString;
  FEnableGeometrics := Src.FEnableGeometrics;
  FEnableComposites := Src.FEnableComposites;
end;

procedure TPgSQLConnection.AssignConnect(Source: TCRConnection);
var
  Src: TPgSQLConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TPgSQLConnection(Source);
      Assign(Src);
      FProtocol := Src.FProtocol;
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := True;
      FNativeConnection := False;
    end;
  end;
end;

function TPgSQLConnection.GetServerVersion: _string;
begin
  Result := _string(FProtocol.ServerVersion);
end;

function TPgSQLConnection.GetServerVersionFull: _string;
begin
  Result := _string(FProtocol.ServerVersionFull);
end;

function TPgSQLConnection.GetClientVersion: _string;
begin
  Result := '8.0 Direct';
end;

function TPgSQLConnection.GetMajorServerVersion: integer;
begin
  if GetConnected then
    Result := FProtocol.MajorServerVersion
  else
    Result := 0;  
end;

function TPgSQLConnection.GetMinorServerVersion: integer;
begin
  if GetConnected then
    Result := FProtocol.MinorServerVersion
  else
    Result := 0;  
end;

function TPgSQLConnection.VersionIsEqualOrHigher(MajorVer, MinorVer: integer): boolean;
begin
  Result := (GetMajorServerVersion > MajorVer) or
    ((GetMajorServerVersion = MajorVer) and (GetMinorServerVersion >= MinorVer));
end;    

function TPgSQLConnection.GetProcessID: integer;
begin
  Assert(FProtocol <> nil);
  Result := FProtocol.GetBackendPID;
end;

function TPgSQLConnection.GetProtocolVersion: TProtocolVersion;
begin
  Result := FProtocolVersion;
end;

function TPgSQLConnection.GetProtocol: TPgSQLProtocol;
begin
  Result := FProtocol;
end;

function TPgSQLConnection.GetTypes: TPgSQLTypes;
begin
  Result := FTypes;
end;

function TPgSQLConnection.CanChangeDatabase: boolean;
begin
  Result := False; 
end;

function TPgSQLConnection.CreateCommand: TPgSQLCommand;
begin
  Result := TPgSQLCommand.Create;
  Result.SetConnection(Self);
  Result.SetProp(prSimpleQueryExecute, True);
end;

function TPgSQLConnection.GetCommand: TPgSQLCommand;
begin
  if FCommand = nil then
    FCommand := CreateCommand;

  Result := FCommand;
end;

procedure TPgSQLConnection.ExecCommand(const SQL: _string);
var
  Command: TPgSQLCommand;
begin
  Command := GetCommand;
  Command.SetSQL(SQL);
  Command.Execute();
end;

procedure TPgSQLConnection.BreakExec;
begin
  if FProtocol <> nil then
    FProtocol.RequestCancel;
end;

function TPgSQLConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TPgSQLCommand;
end;

function TPgSQLConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TPgSQLTransaction;
end;

function TPgSQLConnection.DecodeError(E: EPgError): EPgError;
var
  Msg, DetailMsg, Hint, CallStack, FileName, ProcedureName: _string;
begin
  if FUseUnicode then begin
    Msg := UTF8Decode(AnsiString(E.Message));
    DetailMsg := UTF8Decode(AnsiString(E.DetailMsg));
    Hint := UTF8Decode(AnsiString(E.Hint));
    CallStack := UTF8Decode(AnsiString(E.CallStack));
    FileName := UTF8Decode(AnsiString(E.FileName));
    ProcedureName := UTF8Decode(AnsiString(E.ProcedureName));
  end
  else begin
    Msg := _string(E.Message);
    DetailMsg := _string(E.DetailMsg);
    Hint := _string(E.Hint);
    CallStack := _string(E.CallStack);
    FileName := _string(E.FileName);
    ProcedureName := _string(E.ProcedureName);
  end;

  Result := EPgError.Create(E.Severity, E.ErrorCode, Msg, DetailMsg, Hint, CallStack,
    FileName, ProcedureName, E.Position, E.LineNumber);
end;

procedure TPgSQLConnection.ProcessInternalException(E: EPgError; Component: TObject);
var
  PgError: EPgError;
  Fail: boolean;
begin
  PgError := DecodeError(E);
{$IFNDEF LITE}
  PgError.Component := Component;
{$ENDIF}
  try
    E := PgError;
    Fail := True;
    DoError(E, Fail);
    if Fail then
      PgError := nil; // don't free
  finally
    PgError.Free;
  end;

  if Fail then
    raise E
  else
    Abort;
end;

procedure TPgSQLConnection.PrepareFetchConnection;
begin
  if FFetchConnection = nil then
    FFetchConnection := TPgSQLConnection.Create;

  if not FFetchConnection.FConnected then begin
    FFetchConnection.Assign(Self);
    FFetchConnection.Connect('');
  end;

  Inc(FFetchConCount);
  if FFetchConCount = 1 then
    FFetchConnection.GetInternalTransaction.StartTransaction;
end;

procedure TPgSQLConnection.ReleaseFetchConnection;
begin
  Dec(FFetchConCount);
  if FFetchConCount = 0 then
    FFetchConnection.GetInternalTransaction.Rollback;
end;

{$IFNDEF LITE}
procedure TPgSQLConnection.CheckAuxConnection;
begin
  if FAuxConnection = nil then
    FAuxConnection := TPgSQLConnection.Create;

  if not FAuxConnection.FConnected then begin
    FAuxConnection.Assign(Self);
    FAuxConnection.Connect('');
  end;
end;
{$ENDIF}

procedure TPgSQLConnection.SetClientEncoding;
var
  SQL: _string;
  DefaultEnc: AnsiString;
begin
  SQL := '';
  DefaultEnc := AnsiUpperCase(FProtocol.GetServerParameter('client_encoding'));
  if FUseUnicode then begin
    if VersionIsEqualOrHigher(8, 1) then begin
      if DefaultEnc <> 'UTF8' then
        SQL := 'SET client_encoding=''UTF8''';
    end
    else begin
      if DefaultEnc <> 'UNICODE' then
        SQL := 'SET client_encoding=''UNICODE''';
    end;
  end
  else begin
    if (FCharset <> '') and (AnsiUpperCase(AnsiString(FCharset)) <> DefaultEnc) then
      SQL := _Format('SET client_encoding=''%s''', [FCharset]);
  end;

  if SQL <> '' then
    ExecCommand(SQL);
end;

procedure TPgSQLConnection.GetIntegerDateTimes;
var
  RecordSet: TPgSQLRecordSet;
  RecBuf: IntPtr;
  v: variant;
begin
  if not VersionIsEqualOrHigher(8,0) then begin
    FIntegerDateTimes := False;
    exit;
  end;

  RecordSet := TPgSQLRecordSet.Create;
  try
    RecordSet.SetConnection(Self);
    RecordSet.SetProp(prSimpleQueryExecute, True);
    RecordSet.SetSQL('show integer_datetimes');
    RecordSet.Open;

    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        FIntegerDateTimes := False
      else begin
        RecordSet.GetFieldAsVariant(1, RecBuf, v);
        FIntegerDateTimes := AnsiLowerCase(VarToStr(v)) = 'on';
      end;
    finally
      RecordSet.FreeRecBuf(RecBuf);
    end;
    RecordSet.Close;
  finally
    RecordSet.Free;
  end;
end;

function TPgSQLConnection.GetCurrentSchema: _string;
var
  Command: TPgSQLCommand;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  Command := GetCommand;
  Command.SQL := 'SELECT CURRENT_SCHEMA()';
  with Command.AddParam do
    SetParamType(pdOutput);
  Command.Execute();
  Result := Command.Params[0].Value;
end;

function TPgSQLConnection.GetCachedSchema: _string;
begin
  if FCachedSchema = '' then
    if FSchema <> '' then
      FCachedSchema := PgSQLInfo.UnQuote(FSchema)
    else
      FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;

procedure TPgSQLConnection.SetCurrentSchema(Value: _string);
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if Value = '' then
    Value := 'public';

  ExecCommand('SET search_path TO ' + PgSQLInfo.QuoteIfNeed(Value));
  FSchema := Value;
  FCachedSchema := '';
end;

function TPgSQLConnection.GetInTransaction: boolean;
begin
  Result := FInTransaction;
end;

{$IFNDEF LITE}
function TPgSQLConnection.GetNotificationsHandler: TPgSQLNotificationsHandler;
begin
  if FNotificationsHandler = nil then
    FNotificationsHandler := TPgSQLNotificationsHandler.Create(Self);

  Result := FNotificationsHandler;
end;
{$ENDIF}

procedure TPgSQLConnection.DoOnNotice(Errors: TPgErrors);
var
  NewErrors: TPgErrors;
  i: integer;
begin
  if Assigned(FOnNotice) then begin
    NewErrors := TPgErrors.Create;
    try
      for i := 0 to Errors.Count - 1 do
        NewErrors.Add(DecodeError(Errors[i]));
      FOnNotice(NewErrors);
    finally
      NewErrors.Free;
    end;
  end;
end;

procedure TPgSQLConnection.DoOnNotification(const Name: AnsiString; const PID: integer; const Message: AnsiString);
var
  DecName: _string;
  DecMessage: _string;
begin
  if Assigned(FOnNotification) then begin
    if FUseUnicode then begin
      DecName := UTF8Decode(Name);
      DecMessage := UTF8Decode(Message);
    end  
    else begin
      DecName := _string(Name);
      DecMessage := _string(Message);
    end;
    FOnNotification(DecName, PID, DecMessage);
  end;
end;

{ TPgSQLTransaction }

procedure TPgSQLTransaction.StartTransaction;
var
  CommandText: string;
  Connection: TPgSQLConnection;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  Connection := TPgSQLConnection(FConnections[0]);
  
  if not Connection.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);

  CommandText := 'BEGIN; SET TRANSACTION ISOLATION LEVEL ';

  case FIsolationLevel of
    ilReadCommitted:
      CommandText := CommandText + 'READ COMMITTED';
    ilSnapshot:
      CommandText := CommandText + 'SERIALIZABLE';
    ilRepeatableRead:
      CommandText := CommandText + 'REPEATABLE READ';
    ilReadUncommitted:
      CommandText := CommandText + 'READ UNCOMMITTED';
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;

  if FReadOnly then
    CommandText := CommandText + ' READ ONLY';

  Connection.ExecCommand(CommandText);

  FActive := True;
  FNativeTransaction := True;
  Connection.FInTransaction := True;
end;

procedure TPgSQLTransaction.Commit;
var
  Connection: TPgSQLConnection;
begin
  CheckActive;

  Connection := TPgSQLConnection(FConnections[0]);
  if FNativeTransaction then
    Connection.ExecCommand('COMMIT');

  FActive := False;   
  Connection.FInTransaction := False;
end;

procedure TPgSQLTransaction.Rollback;
var
  Connection: TPgSQLConnection;
begin
  CheckActive;

  Connection := TPgSQLConnection(FConnections[0]);
  if FNativeTransaction then
    Connection.ExecCommand('ROLLBACK');

  FActive := False;
  Connection.FInTransaction := False;
end;

procedure TPgSQLTransaction.Savepoint(const Name: _string);
var
  Connection: TPgSQLConnection;
begin
  CheckActive;

  Connection := TPgSQLConnection(FConnections[0]);
  Connection.ExecCommand('SAVEPOINT ' + Name);
end;

procedure TPgSQLTransaction.ReleaseSavepoint(const Name: _string);
var
  Connection: TPgSQLConnection;
begin
  CheckActive;

  Connection := TPgSQLConnection(FConnections[0]);
  Connection.ExecCommand('RELEASE SAVEPOINT ' + Name);
end;

procedure TPgSQLTransaction.RollbackToSavepoint(const Name: _string);
var
  Connection: TPgSQLConnection;
begin
  CheckActive;

  Connection := TPgSQLConnection(FConnections[0]);
  Connection.ExecCommand('ROLLBACK TO SAVEPOINT ' + Name);
end;

{ TPgSQLTypes }

constructor TPgSQLTypes.Create(Connection: TPgSQLConnection);
begin
  inherited Create;

  FConnection := Connection;
{$IFNDEF LITE}
  FUnknownTypes := TThreadList.Create;
  FRowTypes := TThreadList.Create;
{$ENDIF}
end;

destructor TPgSQLTypes.Destroy;
{$IFNDEF LITE}
var
  i: integer;
  List: TList;
{$ENDIF}
begin
{$IFNDEF LITE}
  FUnknownTypes.Free;
  List := FRowTypes.LockList;
  for i := 0 to List.Count - 1 do
    TPgRowType(List[i]).Free;
  FRowTypes.UnlockList;
  FRowTypes.Free;
{$ENDIF}

  inherited;
end;

procedure TPgSQLTypes.DetectDataType(TypeOID, TypeModifier: integer;
  var DataType, SubDataType: word; var Length, Scale, Size: integer;
  var Fixed: boolean; var ObjectType: TObjectType;
  LongStrings, FlatBuffers, OIDAsInt, EnableBCD, EnableFMTBCD,
  CursorAsString, UnknownAsString, FieldsAsText: boolean);
const
  dsMaxStringSize = 8192;
begin
  DataType := GetInternalType(TypeOID, ObjectType);
  SubDataType := 0;
  Fixed := False;

  GetPgSQLTypeAttr(DataType, TypeModifier, Length, Scale);

  case DataType of
    dtFixedChar: begin
      Fixed := True;
      DataType := dtString;
    end;
    dtPgName: begin
      DataType := dtString;
      Length := 63;
    end;
    dtPgChar: begin
      DataType := dtString;
      Length := 1;
    end;
    dtVoid: begin
      DataType := dtString;
      SubDataType := dtVoid;
    end;
    dtGuid:
      Length := 38;
  else
    if FieldsAsText then begin
      SubDataType := DataType;
      DataType := dtString;
    end
    else begin
      case DataType of
        dtUnknown: begin
          SubDataType := DataType;
          DataType := dtString;
        end;
        dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
          if not FConnection.FEnableGeometrics then begin
            SubDataType := DataType;
            DataType := dtString;
          end;
        dtPgInterval:
          if FConnection.FIntervalAsString then begin
            SubDataType := DataType;
            DataType := dtString;
          end;
        dtCursor:
          if CursorAsString then begin
            SubDataType := DataType;
            DataType := dtString;
            Length := 63;
          end;
      end;
    end;
  end;

  case DataType of
    dtString: begin
      if (Length = 0) and (SubDataType <> dtVoid) then begin
        if UnknownAsString then
          Length := dsMaxStringSize - 1 // TODO: trim on fetch
        else
          DataType := dtMemo;
      end;
      if DataType <> dtMemo then begin
        if (Length < 255) or (LongStrings and (Length <= High(Word))) then begin
          if (Length >= FlatBufferLimit) and not FlatBuffers then
            DataType := dtExtString;
        end
        else begin
          DataType := dtMemo;
          SubDataType := dtString;
        end;
      end;
      if FConnection.FUseUnicode then
        case DataType of
          dtString:
            DataType := dtWideString;
          dtExtString:
            DataType := dtExtWideString;
          dtMemo: begin
            DataType := dtWideMemo;
            if SubDataType = 0 then
              SubDataType := dtWideString;
          end;
        end;
    end;
    dtPgSingle: begin
      DataType := dtFloat;
      SubDataType := dtPgSingle;
    end;
    dtNumeric: begin
    {$IFDEF LITE}
      if Length = 0 then begin
        Length := 32;
        Scale := 8;
      end;
    {$ENDIF}
    {$IFNDEF FPC}
      if EnableFMTBCD or FConnection.EnableFMTBCD then begin
        DataType := dtFMTBCD;
        if Length > MaxFMTBcdDigits then
          Length := MaxFMTBcdDigits;
        if Scale > Length then // if length was reduced
          Scale := Length;
      end
      else
    {$ENDIF}
      if EnableBCD or FConnection.EnableBCD then begin
        DataType := dtBCD;
        if Length > MaxBcdPrecision then
          Length := MaxBcdPrecision;
        if Scale > MaxBcdScale then
          Scale := MaxBcdScale;
      end
      else
        DataType := dtFloat;
      SubDataType := dtNumeric;
    end;
    dtPgDate:
      if not FConnection.FEnablePgTimeStamps then
        DataType := dtDate;
    dtPgTime:
      if not FConnection.FEnablePgTimeStamps then
        DataType := dtTime;
    dtPgTimeTZ: begin
      if FConnection.FEnablePgTimeStamps then
        DataType := dtPgTime
      else
        DataType := dtTime;
      SubDataType := dtPgTimeTZ;
    end;
    dtPgTimeStamp:
      if not FConnection.FEnablePgTimeStamps then
        DataType := dtDateTime;
    dtPgTimeStampTZ: begin
      if FConnection.FEnablePgTimeStamps then
        DataType := dtPgTimeStamp
      else
        DataType := dtDateTime;
      SubDataType := dtPgTimeStampTZ;
    end;
    dtOID: begin
      if OIDAsInt then
        DataType := dtInteger
      else
        DataType := dtPgLargeObject;
      SubDataType := dtOID;
    end;
    dtMemo:
      if FConnection.FUseUnicode then
        DataType := dtWideMemo;
  {$IFDEF LITE}
    dtLargeint:
      if not (EnableFMTBCD or FConnection.EnableFMTBCD) then begin
        DataType := dtFloat;
        SubDataType := dtLargeint;
      end;
  {$ENDIF}
  end;

  case DataType of
    dtString:
      Size := Length + 1;
    dtWideString:
      Size := (Length + 1) * 2;
    dtSmallInt:
      Size := SizeOf(SmallInt);
    dtInteger:
      Size := SizeOf(Integer);
    dtLargeInt:
      Size := SizeOf(Int64);
    dtBoolean:
      Size := sizeOf(WordBool);
    dtFloat, dtCurrency, dtBCD:
      Size := SizeOf(Double);
  {$IFNDEF FPC}
    dtFMTBCD:
      Size := SizeOf(TBcd);
  {$ENDIF}
    dtDate, dtTime, dtDateTime:
      Size := SizeOf(TDateTime);
    dtGuid:
      Size := Length + 1;
  else
    Size := SizeOf(IntPtr);
  end;
end;

function TPgSQLTypes.GetInternalType(TypeCode: integer): word;
var
  ObjectType: TObjectType;
begin
  Result := GetInternalType(TypeCode, ObjectType);
end;

function TPgSQLTypes.GetInternalType(TypeCode: integer; var ObjectType: TObjectType): word;
begin
  ObjectType := nil;
  case TypeCode of
    SQL_VOID:
      Result := dtVoid;
    SQL_CHAR:
      Result := dtFixedChar;
    SQL_VARCHAR:
      Result := dtString;
    SQL_NAME:
      Result := dtPgName;
    SQL_CHARACTER:
      Result := dtPgChar;
    SQL_SMALLINT:
      Result := dtSmallInt;
    SQL_INT:
      Result := dtInteger;
    SQL_BIGINT:
      Result := dtLargeInt;
    SQL_REAL:
      Result := dtPgSingle;
    SQL_DOUBLE:
      Result := dtFloat;
    SQL_NUMERIC:
      Result := dtNumeric;
    SQL_MONEY:
      Result := dtCurrency;
    SQL_TEXT:
      Result := dtMemo;
    SQL_BYTEA:
      Result := dtBlob;
    SQL_DATE:
      Result := dtPgDate;
    SQL_TIME:
      Result := dtPgTime;
    SQL_TIMETZ:
      Result := dtPgTimeTZ;
    SQL_TIMESTAMP:
      Result := dtPgTimeStamp;
    SQL_TIMESTAMPTZ:
      Result := dtPgTimeStampTZ;
    SQL_INTERVAL:
      Result := dtPgInterval;
    SQL_OID:
      Result := dtOID;
    SQL_BOOLEAN:
      Result := dtBoolean;
    SQL_POINT:
      Result := dtPgPoint;
    SQL_LSEG:
      Result := dtPgLSeg;
    SQL_BOX:
      Result := dtPgBox;
    SQL_PATH:
      Result := dtPgPath;
    SQL_POLYGON:
      Result := dtPgPolygon;
    SQL_CIRCLE:
      Result := dtPgCircle;
    SQL_REFCURSOR:
      Result := dtCursor;
    SQL_UUID:
      Result := dtGuid;
  else
  {$IFNDEF LITE}
    if FConnection.FEnableComposites then begin
      case TypeCode of
        PG_TYPE_RELTYPE_OID, PG_ATTRIBUTE_RELTYPE_OID, PG_PROC_RELTYPE_OID, PG_CLASS_RELTYPE_OID: ;
      else
        if (TypeCode < 10000) or IsUnknownType(TypeCode) then begin
          Result := dtUnknown;
          exit;
        end;
      end;
      ObjectType := GetRowType(TypeCode);
      if ObjectType <> nil then
        Result := dtObject
      else begin
        FUnknownTypes.Add({$IFNDEF CLR}Pointer{$ELSE}System.Object{$ENDIF}(TypeCode));
        Result := dtUnknown;
      end;
    end
    else
  {$ENDIF}
      Result := dtUnknown;
  end;
end;

function TPgSQLTypes.DataTypeToOID(DataType, SubDataType: word): integer;
begin
  case DataType of
    dtSmallInt, dtInt8:
      Result := SQL_SMALLINT;
    dtInteger:
      if SubDataType = dtOID then
        Result := SQL_OID
      else
        Result := SQL_INT;
    dtWord:
      Result := SQL_INT;
    dtLargeInt, dtLongword:
      Result := SQL_BIGINT;
    dtFloat:
      Result := SQL_DOUBLE;
    dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}, dtNumeric:
      Result := SQL_NUMERIC;
    dtCurrency:
      Result := SQL_MONEY;
    dtBoolean:
      Result := SQL_BOOLEAN;
    dtMemo, dtWideMemo:
      Result := SQL_TEXT;
    dtBlob:
      Result := SQL_BYTEA;
    dtDate, dtPgDate:
      Result := SQL_DATE;
    dtTime, dtPgTime:
      if SubDataType = dtPgTimeTZ then
        Result := SQL_TIMETZ
      else
        Result := SQL_TIME;
    dtPgTimeTZ:
      Result := SQL_TIMETZ;
    dtDateTime, dtPgTimeStamp:
      if SubDataType = dtPgTimeStampTZ then
        Result := SQL_TIMESTAMPTZ
      else
        Result := SQL_TIMESTAMP;
    dtPgTimeStampTZ:
      Result := SQL_TIMESTAMPTZ;
    dtPgInterval:
      Result := SQL_INTERVAL;
    dtPgPoint:
      Result := SQL_POINT;
    dtPgLSeg:
      Result := SQL_LSEG;
    dtPgBox:
      Result := SQL_BOX;
    dtPgPath:
      Result := SQL_PATH;
    dtPgPolygon:
      Result := SQL_POLYGON;
    dtPgCircle:
      Result := SQL_CIRCLE;
    dtCursor:
      Result := SQL_REFCURSOR;
    dtPgLargeObject:
      Result := SQL_OID;
    dtGuid:
      Result := SQL_UUID;  
  else
    Result := SQL_VARCHAR;
  end;
end;

procedure TPgSQLTypes.GetPgSQLTypeAttr(DataType: Integer; TypeModifier: Integer;
  var Length, Scale: Integer);
begin
  Length := 0;
  Scale := 0;

  case DataType of
    dtString, dtFixedChar:
      if TypeModifier <> -1 then
        Length := TypeModifier - 4;
    dtNumeric:
      if TypeModifier <> -1 then begin
        Length := (TypeModifier - 4) shr 16 and $ffff;
        Scale := (TypeModifier - 4) and $ffff;
      end;
  end;
end;

{$IFNDEF LITE}
function TPgSQLTypes.IsUnknownType(TypeCode: integer): boolean;
var
  i: integer;
  List: TList;
begin
  Result := False;

  List := FUnknownTypes.LockList;
  try
    for i := 0 to List.Count - 1 do
      if Integer(List.Items[i]) = TypeCode then begin
        Result := True;
        exit;
      end;
  finally
    FUnknownTypes.UnlockList;
  end;
end;

function TPgSQLTypes.FindRowType(TypeCode: integer): TObjectType;
var
  i: integer;
  List: TList;
  t: TPgRowType;
begin
  Result := nil;

  List := FRowTypes.LockList;
  try
    for i := 0 to List.Count - 1 do begin
      t := TPgRowType(List.Items[i]);
      if t.TypeOID = TypeCode then begin
        Result := t;
        exit;
      end;
    end;
  finally
    FRowTypes.UnlockList;
  end;
end;

function TPgSQLTypes.FindRowType(const TypeName: _string): TObjectType;
var
  i: integer;
  List: TList;
  t: TPgRowType;
begin
  Result := nil;

  List := FRowTypes.LockList;
  try
    for i := 0 to List.Count - 1 do begin
      t := TPgRowType(List.Items[i]);
      if t.Name = TypeName then begin
        Result := t;
        exit;
      end;
    end;
  finally
    FRowTypes.UnlockList;
  end;
end;

function TPgSQLTypes.GetRowType(TypeCode: integer): TObjectType;
var
  Con: TPgSQLConnection;
  RowType: TPgRowType;
begin
  Result := FindRowType(TypeCode);

  if Result = nil then begin
    if FConnection.FProtocol.ProtocolState = psIsReadyForQuery then
      Con := FConnection
    else begin
      FConnection.CheckAuxConnection;
      Con := FConnection.FAuxConnection;
    end;

    RowType := TPgRowType.Create;
    try
      if RowType.Describe(Con, TypeCode) then begin
        FRowTypes.Add(RowType);
        Result := RowType;
      end
      else
        RowType.Free;
    except
      RowType.Free;
      raise;
    end;
  end;
end;

function TPgSQLTypes.GetRowType(const TypeName: _string): TObjectType;
var
  Con: TPgSQLConnection;
  RowType: TPgRowType;
  p: integer;
  Schema, Name, NormName: _string;
begin
  p := Pos('.', TypeName);
  if p > 0 then begin
    Schema := PgSQLInfo.NormalizeName(Copy(TypeName, 1, p - 1), True);
    Name := PgSQLInfo.NormalizeName(Copy(TypeName, p + 1, MaxInt), True);
  end
  else begin
    Schema := '"' + FConnection.GetCachedSchema + '"';
    Name := PgSQLInfo.NormalizeName(TypeName, True);
  end;
  NormName := Schema + '.' + Name;
  Result := FindRowType(NormName);

  if Result = nil then begin
    if FConnection.FProtocol.ProtocolState = psIsReadyForQuery then
      Con := FConnection
    else begin
      FConnection.CheckAuxConnection;
      Con := FConnection.FAuxConnection;
    end;

    RowType := TPgRowType.Create;
    try
      if RowType.Describe(Con, TypeName) then begin
        FRowTypes.Add(RowType);
        Result := RowType;
      end
      else
        RowType.Free;
    except
      RowType.Free;
      raise;
    end;
  end;
end;
{$ENDIF NDEF LITE}

{ TPgSQLParamDesc }

function TPgSQLParamDesc.GetAsCursor: TPgCursor;
begin
  Result := GetObject as TPgCursor;
end;

procedure TPgSQLParamDesc.SetAsCursor(Value: TPgCursor);
begin
  SetObject(Value);
end;

{ TPgSQLInfo }

procedure TPgSQLInfo.ParseExtColumnName(Parser: TSQLParser; var Code: integer; var Str: _string);
begin
  inherited;

  // type convertion
  if Str = ':' then begin
    Code := Parser.GetNext(Str);
    if Str = ':' then begin
      Parser.GetNext(Str);
      Code := Parser.GetNext(Str);
    end
    else
      Parser.Back;
  end;
end;

function TPgSQLInfo.IdentCase: TIdentCase;
begin
  Result := icLower;
end;

{ TPgCursor }

constructor TPgCursor.Create;
begin
  inherited;
end;

destructor TPgCursor.Destroy;
begin
  FStmtHandle.Free;

  inherited;
end;

function TPgCursor.CanFetch: boolean;
begin
  Result := (FState > csInactive) and (FState < csFetched);
end;

procedure TPgCursor.SetState(Value: TCursorState);
begin
  FState := Value;
end;

procedure TPgCursor.CreateStatement;
begin
  FStmtHandle := TPgSQLStatement.Create;
end;

procedure TPgCursor.FreeStatement;
begin
  FStmtHandle.Free;
  FStmtHandle := nil;
end;

{ TPgSQLCommand }

constructor TPgSQLCommand.Create;
begin
  inherited;

  FCursor := TPgCursor.Create;
  FCursorRef := FCursor;
end;

destructor TPgSQLCommand.Destroy;
begin
  FCursor.Free;

  inherited;
end;

class function TPgSQLCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TPgSQLInfo;
end;

class function TPgSQLCommand.GetParserClass: TSQLParserClass;
begin
  Result := TPgParser;
end;

function TPgSQLCommand.GetParamDescType: TParamDescClass;
begin
  Result := TPgSQLParamDesc;
end;

function TPgSQLCommand.AddParam: TParamDesc;
begin
  Result := GetParamDescType.Create;
  FParams.Add(Result);
end;

function TPgSQLCommand.GetParam(Index: integer): TPgSQLParamDesc;
begin
  Result := TPgSQLParamDesc(Params[Index]);
end;

function TPgSQLCommand.ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string;
var
  ParsedSQL: _StringBuilder;
  Parser: TPgParser;
  Code, p: integer;
  St, TempSt: _string;
  l: integer;
  ParamName: _string;
  ParamIndex, ParamCount: integer;
  Param: TPgSQLParamDesc;

  LeftQuote, RightQuote: _char;
  IsParam, IsFirstLexem: boolean;

  function IndexOfParam(Name: _string): integer;
  var
    i: integer;
  begin
    Result := -1;
    for i := 0 to Params.Count - 1 do
    if (Params.Items[i] <> nil) then
      if _SameText(TParamDesc(Params[i]).GetName, Name) then begin
        Result := i;
        break;
      end;
  end;
  
begin
  Assert(Params <> nil);

  ClearPlaceHolders;
  FParsedStmtType := stUnknown; 

  if FScanParams then
    Params.Clear;

  ParsedSQL := _StringBuilder.Create(Length(SQL) + Length(SQL) div 2);
  try
    Parser := TPgParser.Create(SQL);
    try
      Parser.OmitBlank := False;
      Parser.OmitComment := True;
      Parser.QuotedString := True;
      Parser.DecSeparator := '.';
      Parser.ToBegin;

      LeftQuote := '"';
      RightQuote := '"';
      ParamCount := 0;
      IsFirstLexem := True;

      repeat
        IsParam := False;
        repeat
          Code := Parser.GetNext(St);

          if IsFirstLexem then begin
            if Code = lxSELECT then
              FParsedStmtType := stSelect;

            if (Code <> lcBlank) and (Code <> lcComment) then
              IsFirstLexem := False;
          end;

          if (Code = 3) then begin     // PostgreSQL conversion (::) workaround
            TempSt := St;
            Code := Parser.GetNext(St);
            if (Code = lcIdent) or (Code = lcNumber) or (Code >= lxSQLFirst) then
              IsParam := True
            else
              St := TempSt + St;
          end;

          if not IsParam then
            ParsedSQL.Append(St);
        until (Code = lcEnd) or IsParam;

        if IsParam then begin
          ParamName := St;

          l := Length(ParamName);
          if (ParamName[1] = LeftQuote) and (ParamName[l] = RightQuote) then
            ParamName := Copy(ParamName, 2, l - 2);

          ParamIndex := IndexOfParam(ParamName);

          Param := nil;
          if ParamIndex > -1 then
            Param := GetParam(ParamIndex);

          if FScanParams and (ParamIndex = - 1) then begin
            Param := TPgSQLParamDesc.Create;
            Param.SetName(ParamName);
            ParamIndex := Params.Add(Param);
          end;

          if ReplaceAll or (ParamIndex >= 0) then begin
            p := ParsedSQL.Length + 1;
            if ParamIndex = -1 then
              ParamIndex := ParamCount;
            ParsedSQL.Append('$' + IntToStr(ParamIndex + 1));
            AddPlaceHolder(p, ParsedSQL.Length + 1, Param);
            Inc(ParamCount);
          end
          else
            ParsedSQL.Append(':' + St);
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

procedure TPgSQLCommand.MakeSPParam(const Name: _string; Oid: integer; Direction: _char);
var
  Param: TParamDesc;
  DataType, SubDataType: word;
  Length, Scale, Size: integer;
  Fixed: boolean;
  ObjectType: TObjectType;
begin
  Param := AddParam;
  Param.SetName(Name);

  case Direction of
    'i': Param.SetParamType(pdInput);
    'o': Param.SetParamType(pdOutput);
    'b': Param.SetParamType(pdInputOutput);
    'r': Param.SetParamType(pdResult);
  else
    Assert(False);
  end;

  FUsedConnection.FTypes.DetectDataType(Oid, -1, DataType, SubDataType,
    Length, Scale, Size, Fixed, ObjectType,
    True, True, FOIDAsInt, EnableBCD, {$IFNDEF FPC}EnableFMTBCD{$ELSE}False{$ENDIF},
    False, True, False);

  Param.SetDataType(DataType);
  Param.SetSubDataType(SubDataType);
{$IFDEF LITE}
  Param.SetSize(Size);
{$ENDIF}
end;

procedure TPgSQLCommand.GetProcParamsInfo(const Schema, Name: _string; Overload: integer;
  var ParamsInfo: TProcParamsInfo);

  function Split(const s: _string; delim: _char): _TStringArray;
  var
    i, p, c: integer;
  begin
    SetLength(Result, Length(s));
    if Length(s) = 0 then
      exit;
    c := 0;
    p := 1;
    for i := 1 to Length(s) do begin
      if s[i] = delim then begin
        Result[c] := Copy(s, p, i - p);
        Inc(c);
        p := i + 1;
      end;
    end;
    Result[c] := Copy(s, p, MaxInt);
    Inc(c);
    SetLength(Result, c);
  end;

const
  SQL = 'SELECT p.prorettype, p.proretset, p.proargtypes, p.proallargtypes, p.proargmodes, p.proargnames ' +
    'FROM pg_proc p ' +
    '  INNER JOIN pg_namespace n ON n.oid = p.pronamespace ' +
    'WHERE n.nspname = ''%s'' AND p.proname = ''%s'' ' +
    'OFFSET %d LIMIT 1';

var
  RecordSet: TPgSQLRecordSet;
  i: integer;
  RecBuf: IntPtr;
  v: variant;
  s: _string;
  stypes: _TStringArray;
begin
  stypes := nil;
  RecordSet := TPgSQLRecordSet.Create;
  try
    RecordSet.SetProp(prOIDAsInt, True);
    RecordSet.SetConnection(FUsedConnection);
    RecordSet.SetProp(prOIDAsInt, True);
    RecordSet.SetSQL(_Format(SQL, [Schema, Name, Overload - 1]));
    RecordSet.Open;

    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        raise Exception.CreateFmt(SStoredProcNotFound, [Schema, Name]);

      RecordSet.GetFieldAsVariant(1, RecBuf, v);
      ParamsInfo.RetTypeOid := v;

      RecordSet.GetFieldAsVariant(2, RecBuf, v);
      ParamsInfo.IsReturnSet := v;

      if not RecordSet.GetNull(4, RecBuf) then begin
        RecordSet.GetFieldAsVariant(4, RecBuf, v);
        s := v;
        s := Copy(s, 2, Length(s) - 2);
        stypes := Split(s, ',');
      end
      else begin
        RecordSet.GetFieldAsVariant(3, RecBuf, v);
        s := v;
        stypes := Split(s, ' ');
      end;
      SetLength(ParamsInfo.TypeOIDs, Length(stypes));
      for i := 0 to Length(stypes) - 1 do
        ParamsInfo.TypeOIDs[i] := StrToInt(stypes[i]);

      if not RecordSet.GetNull(5, RecBuf) then begin
        RecordSet.GetFieldAsVariant(5, RecBuf, v);
        s := v;
        s := Copy(s, 2, Length(s) - 2);
        ParamsInfo.Modes := Split(s, ',');
      end;

      if not RecordSet.GetNull(6, RecBuf) then begin
        RecordSet.GetFieldAsVariant(6, RecBuf, v);
        s := v;
        s := Copy(s, 2, Length(s) - 2);
        ParamsInfo.Names := Split(s, ',');
      end;

    finally
      RecordSet.FreeRecBuf(RecBuf);
    end;
  finally
    RecordSet.Free;
  end;
end;

procedure TPgSQLCommand.InitProcParams(const Name: _string; Overload: integer);
var
  ProcName, Schema: _string;
  ParamIndex: Integer;
  m, p, i: integer;
  ParamsInfo: TProcParamsInfo;
  HasOutParams: boolean;
  s, ParamName: _string;
  Mode: _char;
begin
  m := 0;
  p := Length(Name) + 1;
  for i := Length(Name) downto 0 do begin
    if (i = 0) or (Name[i] = '.') then begin
      s := Copy(Name, i + 1, p - i - 1);
      p := i;
      case m of
        0:
          ProcName := s;
        1: begin
          Schema := s;
          break;
        end;
      end;
      Inc(m);
    end;
  end;
  if Schema <> '' then
    Schema := PgSQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := FUsedConnection.GetCachedSchema;
  ProcName := PgSQLInfo.NormalizeName(ProcName, False, True);

  GetProcParamsInfo(Schema, ProcName, Overload, ParamsInfo);

  FParams.Clear;

  if not ParamsInfo.IsReturnSet and (ParamsInfo.RetTypeOid <> SQL_VOID) then begin
    HasOutParams := False;
    for i := 0 to Length(ParamsInfo.Modes) - 1 do
      if ParamsInfo.Modes[i] <> 'i' then begin
        HasOutParams := True;
        break;
      end;

    if not HasOutParams then
      MakeSPParam('result', ParamsInfo.RetTypeOid, 'r');
  end;

  for i := 0 to Length(ParamsInfo.TypeOIDs) - 1 do begin
    if i < Length(ParamsInfo.Names) then begin
      ParamName := ParamsInfo.Names[i];
      ParamName := PgSQLInfo.UnQuote(ParamName);
      if ParamName <> '' then
        ParamName := StringReplace(ParamName, ' ' , '_', [rfReplaceAll])
      else
        ParamName := IntToStr(i + 1);
    end
    else
      ParamName := IntToStr(i + 1);
    if FParams.FindParam(ParamName) <> nil then begin
      ParamIndex := 1;
      while FParams.FindParam(ParamName + '_' + IntToStr(ParamIndex)) <> nil do
        Inc(ParamIndex);
      ParamName := ParamName + '_' + IntToStr(ParamIndex);
    end;

    if i < Length(ParamsInfo.Modes) then begin
      s := ParamsInfo.Modes[i];
      if s <> '' then
        Mode := s[1]
      else
        Mode := 'i';
    end
    else
      Mode := 'i';

    if ParamsInfo.IsReturnSet and (Mode = 'b') then
      Mode := 'i';

    if not ParamsInfo.IsReturnSet or (Mode = 'i') then
      MakeSPParam(ParamName, ParamsInfo.TypeOIDs[i], Mode);
  end;
end;

function TPgSQLCommand.CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string;
var
  ProcName, s, si: _string;
  Overload, OverloadPos, i, j, p, OutCount: integer;
  Added: boolean;
begin
  Overload := 1;
  ProcName := Name;
  OverloadPos := Pos(':', Name);
  if OverloadPos > 0 then begin
    try
      Overload := StrToInt(Trim(Copy(Name, OverloadPos + 1, Length(Name))));
      ProcName := Copy(Name, 1, OverloadPos - 1);
    except
    end;
    if Overload = 0 then
      Overload := 1;
  end;

  if NeedDescribe then
    InitProcParams(ProcName, Overload);

  OutCount := 0;
  for i := 0 to FParams.Count - 1 do begin
    if FParams[i].GetParamType > pdInput then begin
      Inc(OutCount);
      if OutCount > 1 then
        break;
    end;
  end;

  if OutCount = 1 then
    s := 'SELECT '
  else
    s := 'SELECT * FROM ';
  s := s + ProcName + '(';
  si := s;
  Added := False;
  j := 1;
  ClearPlaceHolders;
  for i := 0 to FParams.Count - 1 do begin
    if FParams[i].GetParamType in [pdInput, pdInputOutput] then begin
      if Added then begin
        s := s + ', ';
        si := si + ', ';
      end;
      s := s + ':' + FParams[i].GetName;
      p := Length(si) + 1;
      si := si + '$' + IntToStr(j);
      AddPlaceHolder(p, Length(si) + 1, TPgSQLParamDesc(FParams[i]));
      Inc(j);
      Added := True;
    end;
  end;

  s := s + ')';
  si := si + ')';

  FSQL := si;
  FUserSQL := s;
  FParsedStmtType := stSelect;
  Result := s;
end;

procedure TPgSQLCommand.Prepare;
var
  i: integer;
  SQL: AnsiString;
  ParamTypes: TIntegerDynArray;
begin
  if (GetCursorState > csPrepared) or
     (GetCursorState > csInactive) and (FCursorRef.FStmtHandle <> nil) and FCursorRef.FStmtHandle.Prepared
  then
    Exit;

  FUsedConnection := GetUsedConnection;

  CheckConnection;
  if NativeCursor and (Trim(FSQL) = '') then
    raise Exception.Create(SEmptySQLStatement);
  if not NativeCursor and not FUsedConnection.GetInTransaction then
    raise Exception.Create(SRefCursorNeedTransaction);

  SplitParams;
  FCursorRef.CreateStatement;
  try
    if not UseSimpleProtocol then begin
      if FUsedConnection.FUseUnicode then
        SQL := UTF8Encode(GetFinalSQL)
      else
        SQL := AnsiString(GetFinalSQL);

      if FUseParamTypes and NativeCursor then begin
        SetLength(ParamTypes, FInParamsCount);
        for i := 0 to FInParamsCount - 1 do
          ParamTypes[i] := FUsedConnection.FTypes.DataTypeToOID(
            FInParamRefs[i].GetDataType, FInParamRefs[i].GetSubDataType);
      end
      else
        ParamTypes := nil;

      try
        PerformPrepare(SQL, GetParsedStmtType, ParamTypes);
      except
        on E: EPgError do begin
          if FUsedConnection = FConnection.FFetchConnection then
          begin
            FUsedConnection := FConnection;
            FConnection.ReleaseFetchConnection;
          end;

          FUsedConnection.ProcessInternalException(E, Component);
        end;
      end;

      DescribeParams;

      DescribeFields;
    end;

    SetCursorState(csPrepared);
  except
    if (FUsedConnection.FProtocol <> nil) and not UseSimpleProtocol then
      PerformUnprepare;
    FCursorRef.FreeStatement;
    raise;
  end;
end;

procedure TPgSQLCommand.Unprepare;
begin
  if not NativeCursor and not FNativePreparation then
    Exit;

  try
    ResetParams;

    if GetPrepared then begin
      PerformClosePortal;
      if FUsedConnection.FProtocolVersion <> pv20 then
        PerformUnprepare;
      FCursorRef.FreeStatement;

      if FUsedConnection = FConnection.FFetchConnection then begin
        FUsedConnection := FConnection;
        FConnection.ReleaseFetchConnection;
      end;
    end;
  finally
    SetCursorState(csInactive);
  end;
end;

function TPgSQLCommand.GetPrepared: boolean;
begin
  Result := GetCursorState >= csPrepared;
end;

procedure TPgSQLCommand.Execute(Iters: integer);
begin
  if GetCursorState > csPrepared then
    Exit;

  try
    InternalExecute;
  except
    if (FRecordSet = nil) and Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;
  if (FRecordSet = nil) and Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TPgSQLCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TPgSQLConnection(Value);
    FUsedConnection := FConnection;
  end;
end;

function TPgSQLCommand.GetCursorState: TCursorState;
begin
  Result := FCursorRef.State;
end;

procedure TPgSQLCommand.SetCursorState(Value: TCursorState);
begin
  FCursorRef.State := Value;
end;

function TPgSQLCommand.GetCursor: TCRCursor;
begin
  Result := FCursorRef;
end;

procedure TPgSQLCommand.SetCursor(Value: TCRCursor);
begin
  if FCursorRef <> Value then begin
    if (FCursorRef <> FCursor) then
      FCursorRef.Release;

    if Value <> nil then begin
      FCursorRef := TPgCursor(Value);
      FCursorRef.AddRef;

      FNativePreparation := FCursorRef.State < csPrepared;
    end
    else
      FCursorRef := FCursor;
  end;
end;

function TPgSQLCommand.GetFirstCursor: TPgCursor;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Params.Count - 1 do
    if Params[i].GetParamType >= pdOutput then
      if Params[i].GetDataType = dtCursor then
        if Result = nil then
          Result := TPgSQLParamDesc(Params[i]).GetAsCursor
        else begin
          FNextCursorRef := TPgSQLParamDesc(Params[i]).GetAsCursor;
          exit;
        end;
  FNextCursorRef := nil;
end;

function TPgSQLCommand.GetNextCursor: TPgCursor;
var
  i: integer;
  Param: TPgSQLParamDesc;
  Cursor: TPgCursor;
  Found: boolean;
begin
  Result := FNextCursorRef;
  
  if Result <> nil then begin
    Found := False;
    for i := 0 to FParams.Count - 1 do begin
      Param := TPgSQLParamDesc(FParams[i]);
      if Params[i].GetParamType >= pdOutput then
        if Param.GetDataType = dtCursor then begin
          Cursor := Param.GetAsCursor;
          if not Found then begin
            if Cursor = FNextCursorRef then
              Found := True;
          end
          else
          if Cursor.CanFetch then begin
            FNextCursorRef := Cursor;
            exit;
          end;
        end;
      end;
    end;
  FNextCursorRef := nil;
end;

function TPgSQLCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prSimpleQueryExecute:
      FForceSimpleProtocol := Value;
    prCommandTimeout:
      FCommandTimeout := Value;
    prOIDAsInt:
      FOIDAsInt := Value;
    prUseParamTypes:
      FUseParamTypes := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TPgSQLCommand.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prLastInsertId:
      Value := FLastInsertOID;
    prRowsProcessed:
      Value := FRowsAffected;
    prCommandTimeout:
      Value := FCommandTimeout;
    prUseParamTypes:
      Value := FUseParamTypes;
  else
    Result := inherited GetProp(Prop, Value);    
  end;    
end;

function TPgSQLCommand.GetFormatCode(TypeOID: integer; IsField: boolean): integer;
begin
  if (IsField and FFieldsAsText) or UseSimpleProtocol then
    Result := 0
  else
    case FUsedConnection.FTypes.GetInternalType(TypeOID) of
      dtUnknown, dtString, dtPgName, dtPgChar, dtVoid, dtGuid:
        Result := 0;
      dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
        Result := Byte(FUsedConnection.FEnableGeometrics);
      dtPgInterval:
        Result := Byte(not FUsedConnection.FIntervalAsString);
      dtObject:
        Result := Byte(FUsedConnection.FEnableComposites);
    else
      Result := 1; //Byte(not FUsedConnection.FTypes.IsTextOnly(TypeOid));
    end;
end;

function TPgSQLCommand.UseSimpleProtocol: boolean;
begin
  Result := FForceSimpleProtocol or
  (FUsedConnection <> nil) and (FUsedConnection.FProtocolVersion = pv20) or
  (FRecordSet <> nil) and FRecordSet.FCursorWithHold and not FRecordSet.IsFetchAll;
end;

function TPgSQLCommand.GetUsedConnection: TPgSQLConnection;
begin
  if (GetParsedStmtType = stSelect) and
     (FRecordSet <> nil) and
      not FRecordSet.IsFetchAll and
      not FRecordSet.FCursorWithHold and
      not FConnection.GetInTransaction
  then begin
    FConnection.PrepareFetchConnection;
    Result := FConnection.FFetchConnection;
  end
  else
    Result := FConnection;
end;

procedure TPgSQLCommand.CheckConnection;
begin
  if FUsedConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
end;

procedure TPgSQLCommand.ClearPlaceHolders();
begin
  FPlaceHoldersCount := 0;
end;

procedure TPgSQLCommand.AddPlaceHolder(Position, EndPosition: integer; ParamRef: TPgSQLParamDesc);
begin
  if FPlaceHoldersCount >= Length(FPlaceHolders) then
    SetLength(FPlaceHolders, Length(FPlaceHolders) + 25);

  FPlaceHolders[FPlaceHoldersCount].Position := Position;
  FPlaceHolders[FPlaceHoldersCount].EndPosition := EndPosition;
  FPlaceHolders[FPlaceHoldersCount].ParamRef := ParamRef;

  Inc(FPlaceHoldersCount);
end;

function TPgSQLCommand.GetFinalSQL: _string;
begin
  if not NativeCursor and (FCursorRef is TPgRefCursor) then
    Result := PgSQLInfo.NormalizeName(TPgRefCursor(FCursorRef).CursorName, True)
  else
    Result := FSQL;
end;

function TPgSQLCommand.GetFinalSQLWithParamValues: AnsiString; // UTF8
var
  FinalSQL: _string;
  s: _string;
  sa: AnsiString;
  i: integer;
  Param: TPgSQLParamDesc;
  ValuesSQL: AnsiStringBuilder;
  PrevPos: integer;
  CurrPos: integer;
begin
  FinalSQL := GetFinalSQL;
  if UseSimpleProtocol and NativeCursor and (FPlaceHoldersCount > 0) then begin
    ValuesSQL := AnsiStringBuilder.Create(Length(FinalSQL) * 4);
    try
      PrevPos := 1;

      for i := 0 to FPlaceHoldersCount - 1 do begin
        CurrPos := FPlaceHolders[i].Position;
        Param := FPlaceHolders[i].ParamRef;
        if Param = nil then begin
          Assert(FPlaceHoldersCount = FInParamsCount);
          Param := TPgSQLParamDesc(Params[i]);
        end;

        s := Copy(FinalSQL, PrevPos, CurrPos - PrevPos);
        if FUsedConnection.FUseUnicode then
          sa := UTF8Encode(s)
        else
          sa := AnsiString(s);

        ValuesSQL.Append(sa);
        if Param.GetNull or (Param.GetDataType = dtCursor) then
          ValuesSQL.Append('null')
        else
          ValuesSQL.Append(TPgTextConverter.ValueToText(Param.Value, Param.GetDataType, FUsedConnection.FUseUnicode));

        PrevPos := FPlaceHolders[i].EndPosition;
      end;

      s := Copy(FinalSQL, PrevPos, MaxInt);
      if FUsedConnection.FUseUnicode then
        sa := UTF8Encode(s)
      else
        sa := AnsiString(s);
      ValuesSQL.Append(sa);

      Result := ValuesSQL.ToString;
    finally
      ValuesSQL.Free;
    end;
  end
  else
    if FUsedConnection.FUseUnicode then
      Result := UTF8Encode(FinalSQL)
    else
      Result := AnsiString(FinalSQL);
end;

procedure TPgSQLCommand.CallbackBindParamValue(Dest: TPgSQLNet; ParamNo: integer; const ItemDesc: TPgSQLItemDesc);
var
  Param: TPgSQLParamDesc;
  DataType, SubDataType: word;
  Length, Scale, Size: integer;
  Fixed: boolean;
  ObjectType: TObjectType;
begin
  Param := FInParamRefs[ParamNo];
  Assert(Param <> nil);

  if Param.GetNull then
    TPgBinaryConverter.WriteValue(Null, Dest, 0, 0, nil, FUsedConnection)
  else begin
    FUsedConnection.FTypes.DetectDataType(ItemDesc.TypeOid, ItemDesc.TypeModifier,
      DataType, SubDataType, Length, Scale, Size, Fixed, ObjectType,
      True, True, True, False, False, False, True, False);
    TPgBinaryConverter.WriteValue(Param.Value, Dest, DataType, SubDataType, ObjectType,
      FUsedConnection);
  end;
end;

procedure TPgSQLCommand.PerformPrepare(const SQL: AnsiString; ParsedSQLType: TParsedStmtType; const ParamTypes: TIntegerDynArray);
begin
  FUsedConnection.FProtocol.PrepareStmt(FCursorRef.FStmtHandle, SQL, ParsedSQLType, ParamTypes);
end;

procedure TPgSQLCommand.PerformUnprepare;
begin
  FUsedConnection.FProtocol.UnPrepareStmt(FCursorRef.FStmtHandle);
end;

procedure TPgSQLCommand.PerformBindExecute;
begin
  FUsedConnection.FProtocol.BindExecutePreparedStmt(FCursorRef.FStmtHandle, CallbackBindParamValue);
end;

procedure TPgSQLCommand.PerformClosePortal;
begin
  if GetCursorState > csPrepared then begin
    FUsedConnection.FProtocol.CloseStmt(FCursorRef.FStmtHandle);
    SetCursorState(csPrepared);
  end;
end;

procedure TPgSQLCommand.InternalExecute;
var
  i: integer;
  Param: TPgSQLParamDesc;
  LargeObject: TPgSQLLargeObject;
  NeedPrepare: boolean;
  OldCursorState: TCursorState;

begin
  NeedPrepare := not GetPrepared;
  if NeedPrepare then begin
    Prepare;
    FCursorRef.FStmtHandle.AutoUnprepare := True;
  end
  else begin
    if not FCursorRef.FStmtHandle.Prepared and not UseSimpleProtocol then begin
      Unprepare;
      Prepare;
    end;
    if FCursorRef.FStmtHandle.Prepared and UseSimpleProtocol then begin
      Unprepare;
      Prepare;
    end;
  end;    

  OldCursorState := GetCursorState;
  try
    for i := 0 to FParams.Count - 1 do begin
      Param := TPgSQLParamDesc(FParams[i]);
      case Param.GetDataType of
        dtPgLargeObject: begin
          LargeObject := Param.GetObject as TPgSQLLargeObject;
          if LargeObject.Cached then begin
            LargeObject.Connection := FUsedConnection;
            LargeObject.WriteBlob;
          end;
        end;
      {$IFDEF LITE}
        dtBlob:
          if not FOIDAsInt and (Param.FTypeOID = SQL_OID) then begin
            LargeObject := TPgSQLLargeObject.Create(FUsedConnection);
            LargeObject.SetData(TBlob(Param.GetObject).GetData);
            TBlob(Param.GetObject).Free;
            Param.SetObject(LargeObject);
            LargeObject.CreateObject;
            LargeObject.WriteBlob;
          end;
      {$ENDIF}
      end;
    end;

    FUsedConnection.FProtocol.SetTimeout(FCommandTimeout);

    if UseSimpleProtocol then
      FUsedConnection.FProtocol.ExecuteStmt(FCursorRef.FStmtHandle, GetFinalSQLWithParamValues, GetParsedStmtType)
    else 
      PerformBindExecute;

    SetCursorState(csExecuted);

    if OutParamsReturn then
      ReadOutParams;

    if FRecordSet = nil then
      PerformClosePortal;

    // when we execute insert statement with returnin this information
    // is be available after fetch
    FLastInsertOID := FUsedConnection.FProtocol.LastInsertOID(FCursorRef.FStmtHandle);
    FRowsAffected := FUsedConnection.FProtocol.RowsAffected(FCursorRef.FStmtHandle);
  except
    on E: Exception do begin
      SetCursorState(OldCursorState);
      FRowsAffected := 0;

      if E is EPgError then
        FUsedConnection.ProcessInternalException(EPgError(E), Component)
      else
        raise;
    end;
  end;

  if NeedPrepare then
    UnPrepare;
end;

procedure TPgSQLCommand.DescribeFields;
var
  i: integer;
  OutParams: TPgSQLItemDescs;
begin
  FUsedConnection.FProtocol.DescribeFields(FCursorRef.FStmtHandle, OutParams);
  for i := 0 to Length(OutParams) - 1 do
    OutParams[i].FormatCode := GetFormatCode(OutParams[i].TypeOid, True);
end;

procedure TPgSQLCommand.DescribeParams;
var
  i: integer;
  InParams: TPgSQLItemDescs;
begin
  if NativeCursor then begin
    FUsedConnection.FProtocol.DescribeParams(FCursorRef.FStmtHandle, InParams);

  {$IFNDEF LITE}
    if Length(InParams) <> FInParamsCount then
      raise Exception.Create(SInvalidParams);
  {$ENDIF}

    for i := 0 to Length(InParams) - 1 do
      InParams[i].FormatCode := GetFormatCode(InParams[i].TypeOid, False);
  {$IFDEF LITE}
    for i := 0 to Length(InParams) - 1 do
      FInParamRefs[i].FTypeOID := InParams[i].TypeOid;
  {$ENDIF}
  end;
end;

procedure TPgSQLCommand.ReadOutParams;
var
  RecordSet: TPgSQLRecordSet;
  i, n: integer;
  Param: TPgSQLParamDesc;
  Field: TFieldDesc;
  RecBuf: IntPtr;
  Value: Variant;
  SourceObj: TSharedObject;
  Blob: TBlob;
begin
  RecordSet := TPgSQLRecordSet.Create;
  try
    RecordSet.SetProp(prFetchRows, 1);
    RecordSet.SetProp(prFlatBuffers, False);
    RecordSet.SetProp(prOidAsInt, True);
    RecordSet.SetProp(prCursorAsString, True);

    RecordSet.SetConnection(FUsedConnection);
    RecordSet.GetCommand.SetCursor(FCursor);
    RecordSet.Open;
    RecordSet.AllocRecBuf(IntPtr(RecBuf));
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        Exit;

      n := 0;
      for i := 0 to FParams.Count - 1 do begin
        Param := TPgSQLParamDesc(FParams[i]);
        if Param.GetParamType in [pdOutput, pdInputOutput, pdResult] then begin
          if n >= RecordSet.FieldCount then
            break;
          repeat
            Field := RecordSet.Fields[n];
            Inc(n);
          until Field.ParentField = nil;

          if not CheckTypeCorrespondence(Param.GetDataType, Field.DataType) then
            raise Exception.CreateFmt(SInvalidOutParamDataType, [Param.GetName]);

          if RecordSet.GetNull(Field.ActualFieldNo, RecBuf) then
            Param.SetNull(True)
          else begin
            Param.SetNull(False);
            if RecordSet.IsComplexFieldType(Field.DataType) and
              not (Field.DataType in [dtExtString, dtExtWideString])
            then begin
              SourceObj := RecordSet.InternalGetObject(Field.ActualFieldNo, RecBuf);
              case Field.DataType of
                dtBlob, dtMemo, dtWideMemo: begin
                  if Param.GetDataType = dtString then
                    Param.Value := TBlob(SourceObj).AsAnsiString
                  else
                  if Param.GetDataType = dtWideString then
                    Param.Value := TBlob(SourceObj).AsWideString
                  else begin
                    Blob := Param.GetObject as TBlob;
                    // cannot be used in UniDAC
                    {Blob.GetData.Clear;
                    Blob.IsUnicode := TBlob(SourceObj).IsUnicode;
                    Blob.SetData(TBlob(SourceObj).GetData);}
                    Blob.Assign(TBlob(SourceObj));
                  end;
                end;
              {$IFNDEF LITE}
                dtPgDate, dtPgTime, dtPgTimeStamp:
                  TCustomPgTimeStamp(Param.GetObject).Assign(TCustomPgTimeStamp(SourceObj));
                dtPgInterval:
                  TPgInterval(Param.GetObject).Assign(TPgInterval(SourceObj));
                dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
                  TPgGeometric(Param.GetObject).Assign(TPgGeometric(SourceObj));
                dtObject: begin
                  TPgRow(Param.GetObject).RowType := TPgRow(SourceObj).RowType;
                  TPgRow(Param.GetObject).Assign(TPgRow(SourceObj));
                end;
              {$ENDIF}
              end
            end
            else begin
              RecordSet.GetFieldAsVariant(Field.ActualFieldNo, RecBuf, Value);
              case Param.GetDataType of
                dtPgLargeObject:
                  TPgSQLLargeObject(Param.GetObject).OID := Value;
                dtCursor:
                  TPgRefCursor(Param.GetObject).Assign(FUsedConnection, Value);
              else
                Param.Value := Value;
              end;
            end;
          end;
        end;
      end;
    finally
      Marshal.FreeHGlobal(RecBuf);
      RecordSet.Close;
    end;
  finally
    RecordSet.Free;
  end;
end;

procedure TPgSQLCommand.SplitParams;
var
  i: integer;
  Index: integer;
begin
  FInParamsCount := 0;
  FOutParamsCount := 0;

  for i := 0 to FParams.Count - 1 do begin
    if FParams[i].GetParamType in [pdUnknown, pdInput, pdInputOutput] then
      Inc(FInParamsCount);
    if FParams[i].GetParamType in [pdInputOutput, pdOutput, pdResult] then
      Inc(FOutParamsCount);
  end;

  if not UseSimpleProtocol then begin
    SetLength(FInParamRefs, FInParamsCount);

    Index := 0;
    for i := 0 to FParams.Count - 1 do
      if FParams[i].GetParamType in [pdUnknown, pdInput, pdInputOutput] then begin
        FInParamRefs[Index] := TPgSQLParamDesc(FParams[i]);
        Inc(Index);
      end;
  end;
end;

procedure TPgSQLCommand.ResetParams;
begin
  FInParamsCount := 0;
  FOutParamsCount := 0;

  SetLength(FInParamRefs, 0);
end;

function TPgSQLCommand.NativeCursor: boolean;
begin
  Result := FCursor = FCursorRef;
end;

function TPgSQLCommand.OutParamsReturn: boolean;
begin
  CheckConnection;
  
  Result := False;
  if GetPrepared then
    Result := NativeCursor and
      (FUsedConnection.FProtocol.RowsReturn(FCursorRef.FStmtHandle) and
      ((FOutParamsCount > 0) or FUsedConnection.FProtocol.IsVoidFunc(FCursorRef.FStmtHandle)
        or (FRecordSet = nil) or (FRecordSet.FDescribeExecute)));
end;

function TPgSQLCommand.RowsReturn: boolean;
begin
  CheckConnection;

  Result := False;
  if GetPrepared then
    Result := FUsedConnection.FProtocol.RowsReturn(FCursorRef.FStmtHandle) and
      ((FRecordSet <> nil) and FRecordSet.FDescribeExecute or not OutParamsReturn);
end;

function TPgSQLCommand.GetParsedStmtType: TParsedStmtType;
begin
  if not NativeCursor then
    Result := stCursor
  else
    Result := FParsedStmtType;
end;

procedure TPgSQLCommand.BreakExec;
begin
  if (GetCursorState <> csInactive) and (FUsedConnection <> nil) then
    FUsedConnection.BreakExec;
end;

{ TPgSQLRecordSet }

constructor TPgSQLRecordSet.Create;
begin
  inherited;

  FFetchAll := True;
  FFetchRows := 25;
end;

destructor TPgSQLRecordSet.Destroy;
begin
  Close;

  inherited;
end;

procedure TPgSQLRecordSet.FetchAll;
var
  NoData: boolean;
begin
  if (FCommand.GetCursorState >= csExecuted) and (FCommand.GetCursorState < csFetchingAll) then begin
    FCommand.SetCursorState(csFetchingAll);
    try
      while True do begin
        repeat
          Fetch(False, NoData);
        until NoData;

        if UsedConnection.GetConnected and
          UsedConnection.FProtocol.HasMoreResultSets
        then begin
          FreeData;
          InitData;
          FEOF := False;
          InitFields;
        end
        else
          break;
      end;
    finally
      if FCommand.GetCursorState <> csInactive then
        FCommand.SetCursorState(csFetched);
    end;
  end;
end;

function TPgSQLRecordSet.CanDisconnect: boolean;
begin
  Result := not FNeedExtFieldsInfo and inherited CanDisconnect;
end;

function TPgSQLRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TPgSQLFieldDesc;
end;

procedure TPgSQLRecordSet.ExecCommand;
var
  NeedPrepare: boolean;
begin
  try
    NeedPrepare := not Prepared;
    if NeedPrepare then begin
      InternalPrepare;
      FFetchCursor.FStmtHandle.AutoUnprepare := True;
    end;

    try
      FFetchCursor.FStmtHandle.FetchAll := FFetchAll;
      FFetchCursor.FStmtHandle.WithHold := FCursorWithHold;
      if FFetchAll then
        FFetchCursor.FStmtHandle.FetchRows := 0
      else
        FFetchCursor.FStmtHandle.FetchRows := FFetchRows;

      if (FCommand.GetParsedStmtType = stSelect) and not FCursorWithHold and
        not FFetchAll and not UsedConnection.GetInTransaction
      then
        raise Exception.Create(SFetchAllFalseNeedTransaction);

      inherited;

      if FCommand.UseSimpleProtocol then // no fields on prepare
        SetCommandType;

    except
      if NeedPrepare then
        InternalUnprepare;
      raise;
    end;
    if CommandType <> ctCursor then
      if NeedPrepare then
        InternalUnprepare
      else
        FCommand.PerformClosePortal;

  except
    if not FDescribeExecute and Assigned(FCommand.AfterExecute) then
      FCommand.AfterExecute(False);
    raise;
  end;
  if not FDescribeExecute and Assigned(FCommand.AfterExecute) then
    FCommand.AfterExecute(True);
end;

function TPgSQLRecordSet.IsFullReopen: boolean;
begin
  Result := not FCommand.NativeCursor;
end;

procedure TPgSQLRecordSet.Reopen;
begin
  if FCommand.NativeCursor then begin
    FCommand.PerformClosePortal;

    FreeData;
    InitData;
    if Assigned(FOnDataChanged) then
      // perform dataset resync to prevent AV if grid is repainted in BeforeFetch/AfterFetch events
      FOnDataChanged;

    InternalOpen(True);
  end
  else begin
    Close;
    Open;
  end;
end;

procedure TPgSQLRecordSet.CreateComplexField(RecBuf: IntPtr; FieldIndex: integer; WithBlob: boolean);
var
  FieldDesc: TFieldDesc;
begin
  FieldDesc := Fields[FieldIndex];
  InternalCreateComplexField(RecBuf, UsedConnection, WithBlob, FieldDesc.Offset,
    FieldDesc.DataType, FieldDesc.ObjectType, FCreateFieldObjectFunc, FCacheBlobs);
end;

class procedure TPgSQLRecordSet.InternalCreateComplexField(RecBuf: IntPtr; Connection: TPgSQLConnection;
  WithBlob: boolean; Offset: integer; DataType: word; ObjectType: TObjectType;
  CreateFieldObjectFunc: TCreateFieldObjectFunc; CacheBlobs: boolean);
var
  Ptr: IntPtr;
  Obj: TSharedObject;
begin
  Obj := nil;
  Ptr := PtrOffset(RecBuf, Offset);

  if Assigned(CreateFieldObjectFunc) then
    Obj := CreateFieldObjectFunc(DataType);


  case DataType of
    // copied from MemData
    dtBlob, dtMemo, dtWideMemo:
      if WithBlob then begin
        Obj := TCompressedBlob.Create; // TODO: UniDAC require TCompressedBlob
        if DataType = dtWideMemo then
          TBlob(Obj).IsUnicode := True;
        // RollBack is always on for LOB fields. Otherwise modification
        // that cannot be canceled is possible.
        TBlob(Obj).EnableRollback;
      end;
    dtExtString, dtExtWideString, dtExtVarBytes:
      Marshal.WriteIntPtr(Ptr, nil);
    //
    dtPgLargeObject: begin
      if Obj = nil then                       // UniDac and dbExpress
        Obj := TPgSQLLargeObject.Create(Connection);

      TPgSQLLargeObject(Obj).Cached := CacheBlobs;
    end;
  {$IFNDEF LITE}
    dtPgDate:
      Obj := TPgDate.Create;
    dtPgTime:
      Obj := TPgTime.Create;
    dtPgTimeStamp:
      Obj := TPgTimeStamp.Create;
    dtPgInterval:
      Obj := TPgInterval.Create;
    dtPgPoint:
      Obj := TPgPoint.Create;
    dtPgLSeg:
      Obj := TPgLSeg.Create;
    dtPgBox:
      Obj := TPgBox.Create;
    dtPgPath:
      Obj := TPgPath.Create;
    dtPgPolygon:
      Obj := TPgPolygon.Create;
    dtPgCircle:
      Obj := TPgCircle.Create;
    dtObject:
      Obj := TPgRow.Create(TPgRowType(ObjectType));
  {$ENDIF}
    dtCursor:
      Obj := TPgRefCursor.Create;
  else
    inherited;
  end;
  if Obj <> nil then
    Marshal.WriteIntPtr(Ptr, Obj.GCHandle);
end;

procedure TPgSQLRecordSet.FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Obj: TSharedObject;
  FieldDesc: TFieldDesc;
begin
  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if not FieldDesc.HasParent then
      case FieldDesc.DataType of
        dtPgLargeObject,
        dtPgDate, dtPgTime, dtPgTimeStamp, dtPgInterval,
        dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle,
        dtCursor, dtObject: begin
          Obj := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, FieldDesc.Offset)));
          Obj.Free;
        end;
      end;
  end;

  inherited;
end;

{$IFNDEF LITE}
procedure TPgSQLRecordSet.CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean);
var
  i: integer;
  SrcPtr, DestPtr: IntPtr;
  FieldDesc: TFieldDesc;
begin
  inherited;

  for i := 0 to FieldCount - 1 do begin
    FieldDesc := Fields[i];
    if not FieldDesc.HasParent then begin
      SrcPtr := Marshal.ReadIntPtr(Source, FieldDesc.Offset);
      DestPtr := Marshal.ReadIntPtr(Dest, FieldDesc.Offset);
      case FieldDesc.DataType of
        dtPgDate, dtPgTime, dtPgTimeStamp:
          TCustomPgTimeStamp(GetGCHandleTarget(DestPtr)).Assign(TCustomPgTimeStamp(GetGCHandleTarget(SrcPtr)));
        dtPgInterval:
          TPgInterval(GetGCHandleTarget(DestPtr)).Assign(TPgInterval(GetGCHandleTarget(SrcPtr)));
        dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
          TPgGeometric(GetGCHandleTarget(DestPtr)).Assign(TPgGeometric(GetGCHandleTarget(SrcPtr)));
        dtObject:
          TPgRow(GetGCHandleTarget(DestPtr)).Assign(TPgRow(GetGCHandleTarget(SrcPtr)));
      end;
    end;
  end;
end;

function TPgSQLRecordSet.CompareFieldValue(ValuePtr: IntPtr; const ValueType: integer; FieldDesc: TFieldDesc; RecBuf: IntPtr; const Options: TCompareOptions): integer;
var
  FieldBuf: IntPtr;
  FieldBufStatic: IntPtr;
  IsBlank: boolean;
  v: variant;
  Obj: TSharedObject;
  NeedFree: boolean;
  ts1, ts2: TCustomPgTimeStamp;
  int1, int2: TPgInterval;
  Geom1, Geom2: TPgGeometric;
begin
  FieldBufStatic := nil;
  Obj := nil;
  NeedFree := False;
  try
    if FieldDesc.ParentField = nil then
      FieldBuf := PtrOffset(RecBuf, FieldDesc.Offset)
    else begin
    // support objects
      FieldBufStatic :=  Marshal.AllocHGlobal(4000);
      FieldBuf := FieldBufStatic;
      GetField(FieldDesc.FieldNo, RecBuf, FieldBuf, IsBlank);  // GetChildField
    end;

    case FieldDesc.DataType of
      dtPgDate, dtPgTime, dtPgTimeStamp: begin
        ts2 := TCustomPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf)));
        ts1 := nil;
        v := Unassigned;
        case ValueType of
          dtDateTime, dtDate, dtTime:
            v := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValuePtr));
          dtString:
            v := Marshal.PtrToStringAnsi(ValuePtr);
          dtWideString:
            v := Marshal.PtrToStringUni(ValuePtr);
          dtPgDate, dtPgTime, dtPgTimeStamp:
            ts1 := TCustomPgTimeStamp(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        else
          Assert(False);
        end;
        if ts1 = nil then begin
          case FieldDesc.DataType of
            dtPgDate:
              TPgBufferConverter.VarToPgDate(v, Obj, NeedFree);
            dtPgTime:
              TPgBufferConverter.VarToPgTime(v, ts2.HasTimeZone, Obj, NeedFree);
            dtPgTimeStamp:
              TPgBufferConverter.VarToPgTimeStamp(v, Obj, NeedFree);
          end;
          ts1 := TCustomPgTimeStamp(Obj);
        end;
        Result := ts1.Compare(ts2);
      end;
      dtPgInterval: begin
        int2 := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf)));
        int1 := nil;
        v := Unassigned;
        case ValueType of
          dtString:
            v := Marshal.PtrToStringAnsi(ValuePtr);
          dtWideString:
            v := Marshal.PtrToStringUni(ValuePtr);
          dtPgInterval:
            int1 := TPgInterval(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        else
          Assert(False);
        end;
        if int1 = nil then begin
          TPgBufferConverter.VarToPgInterval(v, Obj, NeedFree);
          int1 := TPgInterval(Obj);
        end;
        Result := int1.Compare(int2);
      end;
      dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle: begin
        Geom2 := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf)));
        Geom1 := nil;
        v := Unassigned;
        case ValueType of
          dtString:
            v := Marshal.PtrToStringAnsi(ValuePtr);
          dtWideString:
            v := Marshal.PtrToStringUni(ValuePtr);
          dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
            Geom1 := TPgGeometric(GetGCHandleTarget(Marshal.ReadIntPtr(ValuePtr)));
        else
          Assert(False);
        end;
        if Geom1 = nil then begin
          TPgBufferConverter.VarToPgGeometric(v, FieldDesc.DataType, Obj, NeedFree);
          Geom1 := TPgGeometric(Obj);
        end;
        Result := AnsiCompareStr(Geom1.AsString, Geom2.AsString);
      end;
    else
      Result := inherited CompareFieldValue(ValuePtr, ValueType, FieldDesc, RecBuf, Options);
    end;
  finally
    if FieldBufStatic <> nil then
      Marshal.FreeHGlobal(FieldBufStatic);
    if NeedFree then
      Obj.Free;
  end;
end;

function TPgSQLRecordSet.CompareFields(RecBuf1: IntPtr; RecBuf2: IntPtr; FieldDesc: TFieldDesc; Options: TCompareOptions = []): integer;
begin
  Options := Options + [coInvertNullOrder];
  Result := inherited CompareFields(RecBuf1, RecBuf2, FieldDesc, Options);
end;

{$ENDIF}

procedure TPgSQLRecordSet.SetToEnd;
begin
  FetchAll;

  inherited;
end;

function TPgSQLRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prOIDAsInt:
      FCommand.FOIDAsInt := Value;
    prCacheBlobs:
      FCacheBlobs := Value;
    prDeferredBlobRead:
      FDeferredBlobRead := Value;
    prCommandTimeout: begin
      Assert(FCommand <> nil);
      FCommand.FCommandTimeout := Value;
    end;
    prCursorAsString:
      FCursorAsString := Value;
    prUnknownAsString:
      FUnknownAsString := Value;
    prFieldsAsText:
      FCommand.FFieldsAsText := Value;
    prSimpleQueryExecute:
      FCommand.FForceSimpleProtocol := Value;
    prUseParamTypes:
      FCommand.FUseParamTypes := Value;
    prCursorWithHold: begin
      // Command should be unprepared on CursorWithHold prop changed
      if FCursorWithHold <> Value then
        UnPrepare;
      FCursorWithHold := Value;
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TPgSQLRecordSet.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRowsFetched:
      Value := FRowsFetched;
    prOIDAsInt:
      Value := FCommand.FOIDAsInt;
    prCacheBlobs:
      Value := FCacheBlobs;
    prDeferredBlobRead:
      Value := FDeferredBlobRead;
    prCommandTimeout: begin
      Assert(FCommand <> nil);
      Value := FCommand.FCommandTimeout;
    end;
    prUnknownAsString:
      Value := FUnknownAsString;
    prFieldsAsText:
      Value := FCommand.FFieldsAsText;
    prSimpleQueryExecute:
      Value := FCommand.FForceSimpleProtocol;
    prUseParamTypes:
      Value := FCommand.FUseParamTypes;
    prCursorWithHold:
      Value := FCursorWithHold;
  else
    Result := inherited GetProp(Prop, Value);
  end
end;

procedure TPgSQLRecordSet.CreateCommand;
begin
  SetCommand(TPgSQLCommand.Create);
end;

procedure TPgSQLRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TPgSQLCommand(Value);
  if FCommand <> nil then
    FCommand.FRecordSet := Self;
end;

function TPgSQLRecordSet.UsedConnection: TPgSQLConnection;
begin
  Result := FCommand.FUsedConnection;
end;

function TPgSQLRecordSet.InternalGetObject(FieldNo: word; RecBuf: IntPtr): TSharedObject;
begin
  Result := inherited InternalGetObject(FieldNo, RecBuf);
end;

procedure TPgSQLRecordSet.InternalPrepare;
begin
  inherited;

  InitFetchCursor;
end;

procedure TPgSQLRecordSet.InternalUnPrepare;
begin
  if not FCommand.NativeCursor then
    try
      FCommand.Unprepare;
    finally
      FCommand.SetCursor(nil);
    end;

  inherited;

  CommandType := ctUnknown;
end;

procedure TPgSQLRecordSet.InitFetchCursor;
var
  RefCursor: TPgCursor;
begin
  RefCursor := nil;
  if FCommand.NativeCursor and (FCommand.FOutParamsCount > 0) then
    RefCursor := FCommand.GetFirstCursor;

  if RefCursor <> nil then begin
    if FCommand.GetCursorState < csExecuted then
      FCommand.Execute();

    FCommand.SetCursor(RefCursor);
    try
      FCommand.Prepare;
    except
      FCommand.SetCursor(nil);
      FCommand.Unprepare;
      raise;
    end;
    FFetchCursor := RefCursor;
  end
  else
    FFetchCursor := FCommand.FCursorRef;

  if FCommand.UseSimpleProtocol then begin // no fields on prepare
    if not FCommand.NativeCursor then
      CommandType := ctCursor
    else
      CommandType := ctStatement;
  end
  else
    SetCommandType;
end;

function TPgSQLRecordSet.NeedInitFieldsOnFetch: boolean;
begin
  Result := not FCommand.NativeCursor;
end;

procedure TPgSQLRecordSet.InternalInitFields;

  procedure DescribeFieldDesc(Field: TPgSQLFieldDesc; ItemDesc: TPgSQLItemDesc; FieldNo: Integer);
  var
    DataType, SubDataType: word;
    Length, Scale, Size: integer;
    Fixed: boolean;
    ObjectType: TObjectType;
  begin
    if UsedConnection.FUseUnicode then
      Field.Name := UTF8Decode(ItemDesc.FieldName)
    else
      Field.Name := _string(ItemDesc.FieldName);

    Field.ActualName := Field.Name;

    Field.FieldNo := FieldNo;
    Field.ActualFieldNo := FieldNo;

    Field.TableOID := ItemDesc.TableOid;
    Field.TableCol :=  ItemDesc.TableCol;
    Field.FIsTextMode := FCommand.GetFormatCode(ItemDesc.TypeOid, True) = 0;

    UsedConnection.FTypes.DetectDataType(ItemDesc.TypeOid, ItemDesc.TypeModifier,
      DataType, SubDataType, Length, Scale, Size, Fixed, ObjectType,
      FLongStrings, FFlatBuffers, FCommand.FOIDAsInt,
      FCommand.EnableBCD, {$IFNDEF FPC}FCommand.EnableFMTBCD{$ELSE}False{$ENDIF}, FCursorAsString,
      FUnknownAsString, FCommand.FFieldsAsText);

    // OID column 
    if (_CompareText(Field.Name, 'OID') = 0) and
      (Field.TableCol < 0) and
      (DataType = dtPgLargeObject)
    then
      DataType := dtInteger;

    Field.DataType := DataType;
    Field.SubDataType := SubDataType;
    Field.Size := Size;
    Field.Length := Length;
    Field.Scale := Scale;
    Field.Fixed := Fixed;
    Field.ObjectType := ObjectType;
  end;

var
  OldCursorState: TCursorState;
  Field: TPgSQLFieldDesc;
  PgSQLItemDescs: TPgSQLItemDescs;
  FieldsCount: Integer;
  i: integer;
begin
  OldCursorState := FCommand.GetCursorState;
  if OldCursorState = csInactive then begin
    if not FCommand.UseSimpleProtocol then
      InternalPrepare
    else begin
      FDescribeExecute := True;
      try
        ExecCommand;
      finally
        FDescribeExecute := False;
      end;
    end;
  end;

  try
    if CommandType <> ctCursor then
      raise Exception.Create(SNotRows);

    UsedConnection.FProtocol.DescribeFields(FFetchCursor.FStmtHandle, PgSQLItemDescs);

    FieldsCount := Length(PgSQLItemDescs);
    SetLength(FTopFields, FieldsCount);
    for i := 0 to FieldsCount - 1 do begin
      Field := TPgSQLFieldDesc(GetFieldDescType.Create);
      DescribeFieldDesc(Field, PgSQLItemDescs[i], FFields.Count + 1);

      FTopFields[i] := FFields.Count;
      FFields.Add(Field);

      if Field.DataType = dtObject then
        InitObjectFields(Field.ObjectType, Field);
    end;

    FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, TablesInfo);
    TablesInfo.CaseSensitive := True;
    if not (FCommand.GetCursorState in [csExecuted, csFetching, csFetchingAll]) then
      GetExtFieldsInfo
    else
      FNeedExtFieldsInfo := True;
  finally
    if OldCursorState = csInactive then
      InternalUnPrepare;
  end;
end;

procedure TPgSQLRecordSet.ClearFields;
begin
  inherited;

  FTopFields := nil;
end;

procedure TPgSQLRecordSet.ExecFetch(DisableInitFields: boolean);
begin
  ExecCommand;

  if CommandType <> ctCursor then
    raise Exception.Create(SNotRows);

  if not DisableInitFields and
    (not Prepared or (Fields.Count = 0) or NeedInitFieldsOnFetch)
  then
    InitFields;

  if FFetchAll then
    FetchAll
  else
    Fetch;
end;

procedure TPgSQLRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  inherited;

  if FNeedExtFieldsInfo then begin
    GetExtFieldsInfo;
    DoAfterFetch;
  end;
end;

procedure TPgSQLRecordSet.InternalClose;
begin
  FCommand.PerformClosePortal;

  inherited;

  if not Prepared then
    InternalUnprepare;
end;

function TPgSQLRecordSet.Fetch(FetchBack: boolean = False): boolean;
var
  NoData: boolean;
begin
  Result := Fetch(FetchBack, NoData);
end;

function TPgSQLRecordSet.Fetch(FetchBack: boolean; var NoData: boolean): boolean;
var
  Cancel: boolean;
  OldCommandType: TCommandType;
begin
  Result := False;

  if (FCommand.GetCursorState >= csExecuted) and
    (FCommand.GetCursorState <> csFetched)
  then begin
    DoBeforeFetch(Cancel);
    if Cancel then
      Exit;
    try
      try
        Result := InternalFetch(FetchBack, NoData);
      except
        FEOF := True;
        raise;
      end;
    finally
      DoAfterFetch;
    end;
  end;

  if FCommand.GetCursorState = csFetched then begin
    FCommand.PerformClosePortal;
    if not Prepared then begin
      OldCommandType := CommandType;
      InternalUnPrepare;
      // We need to save old CommandType to save old FieldDescs on Refresh.
      // This is need for SQL Generator (old FieldDescs have TableInfo field set)
      CommandType := OldCommandType;
      Prepared := False;
    end;
  end;
end;

class function TPgSQLRecordSet.IsBlobFieldType(DataType: word): boolean;
begin
  Result := (DataType = dtPgLargeObject) or inherited IsBlobFieldType(DataType);
end;

class function TPgSQLRecordSet.IsComplexFieldType(DataType: word): boolean;
begin
  case DataType of
    dtPgLargeObject,
    dtPgDate, dtPgTime, dtPgTimeStamp, dtPgInterval,
    dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle,
    dtCursor, dtObject:
      Result := True;
  else
    Result := inherited IsComplexFieldType(DataType);
  end;
end;

procedure TPgSQLRecordSet.SetCommandType;
begin
  if FCommand.RowsReturn then
    FCommandType := ctCursor
  else
    FCommandType := ctStatement;
end;

procedure TPgSQLRecordSet.ReceiveFetchBuffer(Source: TPgSQLNet; Size: integer;
  FetchBlock: IntPtr; Row, Col: integer);
var
  Field: TPgSQLFieldDesc;
  IsNull: boolean;
  RecBuf, FieldBuf, Buf, HeapBuffer: IntPtr;
begin
  Field := TPgSQLFieldDesc(FFields[FTopFields[Col]]);
  RecBuf := PtrOffset(FetchBlock, SizeOf(TBlockHeader) + (RecordSize + SizeOf(TItemHeader)) * Row +
    SizeOf(TItemHeader));
  FieldBuf := PtrOffset(RecBuf, Field.Offset);

  CreateComplexField(RecBuf, FTopFields[Col], True);

  IsNull := Size = -1;
  SetNull(FTopFields[Col] + 1, RecBuf, IsNull);

  if not IsNull then begin
    case Field.DataType of
      dtExtString: begin
        HeapBuffer := StringHeap.NewBuf(Size + 1);
        Buf := HeapBuffer;
      end;
      dtExtWideString: begin
        HeapBuffer := StringHeap.NewBuf((Size + 1) * 2);
        Buf := HeapBuffer;
      end;
    else
      HeapBuffer := nil;
      Buf := FieldBuf;
    end;

    if Field.FIsTextMode then
      TPgTextConverter.ReadValue(Source, Size, Buf, Field.DataType, Field.SubDataType,
        Field.Fixed and TrimFixedChar, UsedConnection)
    else
      TPgBinaryConverter.ReadValue(Source, Size, Buf, Field.DataType, Field.SubDataType,
        Field.Fixed and TrimFixedChar, UsedConnection);

    if HeapBuffer <> nil then
      Marshal.WriteIntPtr(FieldBuf, HeapBuffer);
  end;
end;

function TPgSQLRecordSet.InternalFetch(FetchBack: boolean; var NoData: boolean): boolean;

  procedure InitBlock(pHBlock: PBlockHeader);
  var
    i, j, ItemSize: integer;
    Ptr: IntPtr;
    Field: TFieldDesc;
  begin
    if HasComplexFields then begin
      ItemSize := RecordSize + sizeof(TItemHeader);
      for i := 0 to FFetchRows - 1 do begin
        Ptr := PtrOffset(pHBlock, sizeof(TBlockHeader) + i * ItemSize + sizeof(TItemHeader));

        for j := 0 to FieldCount - 1 do begin
          Field := Fields[j];
          if not Field.HasParent and (Field.FieldDescKind <> fdkCalculated) then
            case Field.DataType of
              dtBlob, dtMemo, dtWideMemo, dtVariant, dtExtString, dtExtWideString, dtExtVarBytes,
              dtPgLargeObject, dtPgDate, dtPgTime, dtPgTimeStamp, dtPgInterval,
              dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle,
              dtCursor, dtObject:
                Marshal.WriteIntPtr(Ptr, Field.Offset, nil);
            end;
        end;
      end;
    end;
  end;

  procedure ClearBlock(pHBlock: PBlockHeader);
  var
    i, ItemSize: integer;
  begin
    if HasComplexFields then begin
      ItemSize := RecordSize + sizeof(TItemHeader);
      for i := 0 to FFetchRows - 1 do
        FreeComplexFields(PtrOffset(pHBlock, sizeof(TBlockHeader) + i * ItemSize + sizeof(TItemHeader)), True);
    end;
  end;

  procedure ReadBlobs(FirstItem: PItemHeader);
  var
    i: integer;
    Item: PItemHeader;
    ObjPtr: IntPtr;
    SharedPiece: PPieceHeader;
    Lob: TPgSQLLargeObject;
  begin
    Item := FirstItem;
    SharedPiece := nil;
    try
      while IntPtr(Item) <> nil do begin
        for i := 0 to Fields.Count - 1 do begin
          if (Fields[i].DataType = dtPgLargeObject) then begin
            ObjPtr := Marshal.ReadIntPtr(Item, SizeOf(TItemHeader) + Fields[i].Offset);
            Lob := TPgSQLLargeObject(GetGCHandleTarget(ObjPtr));
            if Lob.IsCreated then begin
              if IntPtr(SharedPiece) = nil then
                TBlob.AllocPiece(SharedPiece, DefaultPieceSize);
              Lob.ReadBlob(SharedPiece);
            end;
          end;
        end;
        Item := Item.Next;
      end;
    finally
      if IntPtr(SharedPiece) <> nil then
        Marshal.FreeHGlobal(SharedPiece);
    end;
  end;

var
  pHBlock: PBlockHeader;
  RowsObtained: Integer;
  NewBlock: Boolean;
  OldFirstItem: IntPtr;
  OldLastItem: IntPtr;
begin
  Result := False;

  NewBlock := (IntPtr(BlockMan.FirstBlock) = nil) or not FUnidirectional;
  if NewBlock then
    BlockMan.AllocBlock(pHBlock, FFetchRows)
  else begin
    phBlock := BlockMan.FirstBlock; // overwrite data in unidirectional mode
    ClearBlock(phBlock);
  end;
  InitBlock(phBlock);

  OldFirstItem := FirstItem;
  OldLastItem := LastItem;
  try
    RowsObtained := UsedConnection.FProtocol.FetchStmt(FFetchCursor.FStmtHandle, FFetchRows, pHBlock, ReceiveFetchBuffer);

    if FCommand.GetCursorState < csFetching then
      FCommand.SetCursorState(csFetching);

    Result := RowsObtained > 0;

    NoData := UsedConnection.FProtocol.NoData(FFetchCursor.FStmtHandle);
    if NoData and not UsedConnection.FProtocol.HasMoreResultSets then
      FCommand.SetCursorState(csFetched);

    if RowsObtained > 0 then begin

      CreateBlockStruct(pHBlock, RowsObtained, FetchBack);
      FRowsFetched := RecordCount;

      if not FDeferredBlobRead and FCacheBlobs then begin
        if not FFetchAll then
          ReadBlobs(PtrOffset(pHBlock, sizeof(TBlockHeader)))
        else
        if FCommand.GetCursorState = csFetched then
          ReadBlobs(FirstItem);
      end;
    end
    else
      BlockMan.FreeBlock(pHBlock);
  except
    on E: Exception do begin
      if NewBlock then begin
        // BlockMan.FirstBlock = nil means that dataset was closed after some
        // fatal error and all blocks are already freed.
        if IntPtr(BlockMan.FirstBlock) <> nil then begin
          BlockMan.FreeBlock(pHBlock);
          // restore first and last items
          FirstItem := OldFirstItem;
          LastItem := OldLastItem;
          if IntPtr(FirstItem) <> nil then
            FirstItem.Prev := nil;
          if IntPtr(LastItem) <> nil then
            LastItem.Next := nil;
        end;
      end;
      if E is EpgError then begin
        FCommand.SetCursorState(csFetched);
        UsedConnection.ProcessInternalException(EPgError(E), Component);
      end
      else
        raise;
    end;
  end;
end;

procedure TPgSQLRecordSet.GetExtFieldsInfo;
const
  fnNAME = 1;
  fnIDX  = 2;
  fnNOTNULL = 3;
  fnDEFAULT = 4;
  fnOID  = 5;
  fnNUM  = 6;
var
  ColumnsInfo: TColumnsInfo;
  ColInfo: TColumnInfo;
  i, j, p: integer;
  Value: variant;

  RelName: _string;
  SchemaName: _string;
  TableName: _string;
  DefVal: _string;
  SQL: _StringBuilder;
  CaseSQL: _StringBuilder;
  WhereSQL: _StringBuilder;

  FieldDesc: TPgSQLFieldDesc;
  RecordSet: TPgSQLRecordSet;
  RecBuf: IntPtr;

  DefaultTable: integer;
  AsteriskCount: integer;

  AsteriskTableIndex: integer;
  Located: boolean;

  RequestServerInfo : boolean;

  procedure SetDefaultExpr(FieldDesc: TPgSQLFieldDesc; DefValue: _string);
  begin
    FieldDesc.DefaultExpr := DefValue;
  end;

  function Locate1(FieldNo: integer; Value: _string): boolean;
  var
    v: variant;
  begin
    RecordSet.SetToBegin;
    while True do begin
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        break;
      RecordSet.GetFieldAsVariant(FieldNo, RecBuf, v);
      if _VarToStr(v) = Value then begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;

  function Locate2(FieldNo1, FieldNo2, Value1, Value2: integer): boolean;
  var
    v: variant;
  begin
    RecordSet.SetToBegin;
    while True do begin
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        break;
      RecordSet.GetFieldAsVariant(FieldNo1, RecBuf, v);
      if v = Value1 then begin
        RecordSet.GetFieldAsVariant(FieldNo2, RecBuf, v);
        if v = Value2 then begin
          Result := True;
          exit;
        end;
      end;
    end;
    Result := False;
  end;

begin
  FNeedExtFieldsInfo := False;

{$IFDEF LITE}
  if not FExtendedFieldsInfo then
    exit;
{$ENDIF}

  if TablesInfo.Count = 0 then
    Exit;

  RecordSet := nil;
  RecBuf := nil;
  DefaultTable := -1; // table index for fields from '*'

  // we parse SQL in order to:
  //  a) get ActualFieldName (not Alias)
  //  b) get tables for fields
  //  c) get asterisks
  ColumnsInfo := TColumnsInfo.Create;
  try
    FCommand.SQLInfo.ParseColumnsInfo(FCommand.SQL, ColumnsInfo);

    // Parse tables info in select list
    AsteriskCount := 0;
    for i := 0 to ColumnsInfo.Count - 1 do begin
      ColInfo := ColumnsInfo[i];
      if ColInfo.Table <> '' then
        for j := 0 to TablesInfo.Count - 1 do
          if (ColInfo.Table = TablesInfo[j].TableAlias) or
            (ColInfo.Table = TablesInfo[j].TableName)
          then begin
            ColInfo.TableIndex := j;
            Break;
          end;
      if ColInfo.Name = '*' then begin
        Inc(AsteriskCount);
        if AsteriskCount = 1 then begin // for first '*'
          if ColInfo.Table = '' then
            DefaultTable := 0
          else
            DefaultTable := ColInfo.TableIndex; // can be -1 for table that is not present in TablesInfo
        end;
      end;
    end;

    RequestServerInfo := ({RequestServerInfo and} FExtendedFieldsInfo) or
      FFieldsOrigin or FDefaultValues;

    // request additional information from server
    if RequestServerInfo then begin
      RecordSet := TPgSQLRecordSet.Create;
      RecordSet.SetConnection(UsedConnection);
      RecordSet.SetProp(prOIDAsInt, True);
      RecordSet.SetProp(prExtendedFieldsInfo, False);

      SQL := _StringBuilder.Create(4096);
      CaseSQL :=  _StringBuilder.Create(4096);
      WhereSQL :=  _StringBuilder.Create(4096);
      try
        for i := 0 to TablesInfo.Count - 1 do begin
          TableName := '';
          SchemaName := '';

          RelName := TablesInfo.Items[i].TableName;
          j := Length(RelName);
          p := j;
          while j >= 0 do begin
            if (j = 0) or (RelName[j] = '.') then begin
              if TableName = '' then
                TableName := Copy(RelName, j + 1, p)
              else
              if SchemaName = '' then
                SchemaName := Copy(RelName, j + 1, p);
             p := j - 1;
            end;
            Dec(j);
          end;

          TableName := PgSQLInfo.ToStringConst(PgSQLInfo.NormalizeName(TableName, False, True));
          CaseSQL.Append(' when relname = ' + TableName + ' then ' + IntToStr(i));
          if i > 0 then
            WhereSQL.Append(' or ');

          if UsedConnection.FProtocolVersion = pv30 then
            WhereSQL.Append('(pc.relname = ' + TableName + ')')
          else begin
            WhereSQL.Append('((pc.relname = ' + TableName + ')');
            if SchemaName <> '' then begin
              SchemaName := PgSQLInfo.ToStringConst(PgSQLInfo.NormalizeName(SchemaName, False, True));
              WhereSQL.Append(' and (pn.nspname = ' + SchemaName + '))')
            end
            else
              WhereSQL.Append(' and (pn.nspname = current_schema()))')
          end;
        end;

        SQL.Append('select');

        // select list
        SQL.Append(' pa.attname, case '  + CaseSQL.ToString + ' end as idx, pa.attnotnull');
        //if FDataSet.Options.DefaultValues then
          SQL.Append(', pd.adsrc');
        if UsedConnection.FProtocolVersion = pv30 then
          SQL.Append(', pc.oid, pa.attnum');

        // from
        SQL.Append(' from pg_attribute pa');

        // join pg_class
        SQL.Append(' join pg_catalog.pg_class pc on (pa.attrelid = pc.oid)');
        // join pg_namespace
        if not (UsedConnection.FProtocolVersion = pv30) then
          SQL.Append(' join pg_catalog.pg_namespace pn on (pc.relnamespace = pn.oid)');
        // join pg_attrdef
        //if FDataSet.Options.DefaultValues then
          SQL.Append(' left join pg_catalog.pg_attrdef pd on (pd.adnum = pa.attnum) and (pd.adrelid = pc.oid)');

        // where
        SQL.Append(' where (pa.attnum > 0)');
        SQL.Append(' and (' + WhereSQL.ToString + ')');

        // order by
        SQL.Append(' order by idx, pa.attnum');

        RecordSet.SetSQL(SQL.ToString);
      finally
        SQL.Free;
        CaseSQL.Free;
        WhereSQL.Free;
      end;
      RecordSet.Open;
      RecordSet.AllocRecBuf(RecBuf);

      // supplement parsed information
      if not (UsedConnection.FProtocolVersion = pv30) then begin
        p := 0;
        while p < ColumnsInfo.Count do begin
          if ColumnsInfo[p].Name = '*' then begin
            AsteriskTableIndex := ColumnsInfo[p].TableIndex;
            ColumnsInfo[p].Free;
            ColumnsInfo.Delete(p);

            RecordSet.SetToBegin;
            while True do begin
              RecordSet.GetNextRecord(RecBuf);
              if RecordSet.Eof then
                break;
              Value := Unassigned;
              if AsteriskTableIndex <> -1 then
                RecordSet.GetFieldAsVariant(fnIDX, RecBuf, Value);
              if (AsteriskTableIndex = -1) or
                (Value = AsteriskTableIndex)
              then begin
                ColInfo := TColumnInfo.Create;
                RecordSet.GetFieldAsVariant(fnNAME, RecBuf, Value);
                ColInfo.Name := _VarToStr(Value);
                RecordSet.GetFieldAsVariant(fnIDX, RecBuf, Value);
                ColInfo.TableIndex := Value;
                //if FDataSet.Options.DefaultValues then
                  RecordSet.GetFieldAsVariant(fnDEFAULT, RecBuf, Value);
                  ColInfo.Expr := _VarToStr(Value);
                RecordSet.GetFieldAsVariant(fnNOTNULL, RecBuf, Value);
                ColInfo.Required := Boolean(Value);
                ColumnsInfo.Insert(p, ColInfo);
                Inc(p);
              end;
            end;
          end
          else begin
            Located := Locate1(fnNAME, ColumnsInfo[p].Name);
            if Located then begin
              RecordSet.GetFieldAsVariant(fnIDX, RecBuf, Value);
              ColumnsInfo[p].TableIndex := Value;
              //if FDataSet.Options.DefaultValues then
                RecordSet.GetFieldAsVariant(fnDEFAULT, RecBuf, Value);
                ColumnsInfo[p].Expr := _VarToStr(Value);
              RecordSet.GetFieldAsVariant(fnNOTNULL, RecBuf, Value);
              ColumnsInfo[p].Required := Boolean(Value);
            end;
            Inc(p);
          end;
        end;
      end;
    end;

    for i := 0 to FieldCount - 1 do begin
      FieldDesc := TPgSQLFieldDesc(Fields[i]);
      if FieldDesc.HasParent or (FieldDesc.FieldDescKind <> fdkData) then
        continue;

      // field is not represented in ParsedFDs list because:
      // a) it is expression
      // b) it belongs to the table that have not got into TablesInfo
      ColInfo := nil;
      for j := 0 to ColumnsInfo.Count - 1 do
        if ((FieldDesc.ActualName = ColumnsInfo[j].Alias) or
          (ColumnsInfo[j].Alias = '') and (FieldDesc.ActualName = ColumnsInfo[j].Name)) and
          not ColumnsInfo[j].Used
        then begin
          ColInfo := ColumnsInfo[j];
          ColInfo.Used := True;
          break;
        end;

      if RequestServerInfo and (UsedConnection.FProtocolVersion = pv30) then begin
        Assert(RecordSet <> nil);
        if FieldDesc.TableOID > 0 then
          if Locate2(fnOID, fnNUM, FieldDesc.TableOID, FieldDesc.TableCol) then begin
            RecordSet.GetFieldAsVariant(fnNAME, RecBuf, Value);
            FieldDesc.ActualName := _VarToStr(Value);
            // fnIDX can be incorrect if FROM contains the same table several times
            if (ColInfo <> nil) and (ColInfo.TableIndex >= 0) then
              FieldDesc.TableInfo := TablesInfo[ColInfo.TableIndex]
            else begin
              RecordSet.GetFieldAsVariant(fnIDX, RecBuf, Value);
              FieldDesc.TableInfo := TablesInfo.Items[Value];
            end;
            RecordSet.GetFieldAsVariant(fnDEFAULT, RecBuf, Value);
            DefVal := _VarToStr(Value);
            if (FieldDesc.DataType in [dtInteger, dtLargeint]) and (Pos('nextval', DefVal) > 0) then
              FieldDesc.IsAutoIncrement := True; // serial, bigserial
            //if FDataSet.Options.DefaultValues then
              SetDefaultExpr(FieldDesc, DefVal);
            if not FieldDesc.IsAutoIncrement then begin
              RecordSet.GetFieldAsVariant(fnNOTNULL, RecBuf, Value);
              FieldDesc.Required := Boolean(Value);
            end;
          end;
      end
      else begin
        if ColInfo <> nil then begin
          if ColInfo.Name <> '' then begin
            if ColInfo.Alias <> '' then
              FieldDesc.ActualName := ColInfo.Name;

            if ColInfo.TableIndex >= 0 then
              FieldDesc.TableInfo := TablesInfo[ColInfo.TableIndex]
            else
            // when we don't request additional information from server we suppose that
            // all fields belong to the first table in the FROM list
            if not RequestServerInfo then
              FieldDesc.TableInfo := TablesInfo[0];

            if RequestServerInfo then begin
              DefVal := ColInfo.Expr;
              if (FieldDesc.DataType in [dtInteger, dtLargeint]) and (Pos('nextval', DefVal) > 0) then
                FieldDesc.IsAutoIncrement := True; // serial, bigserial
              //if FDataSet.Options.DefaultValues then
                SetDefaultExpr(FieldDesc, DefVal);
              FieldDesc.Required := not FieldDesc.IsAutoIncrement and ColInfo.Required;
            end;
          end;
        end
        else
          // when we don't request additional information from server we suppose that
          // field that belongs to the same table as '*' does
          if (not RequestServerInfo) and (DefaultTable <> -1) then
            FieldDesc.TableInfo := TablesInfo[DefaultTable];
      end;
    end;
  finally
    if RecordSet <> nil then begin
      if RecBuf <> nil then
        RecordSet.FreeRecBuf(RecBuf);
      RecordSet.Free;
    end;

    ColumnsInfo.Free;
  end;
end;

{ TPgSQLMetaData }

function TPgSQLMetaData.GetConnection: TPgSQLConnection;
begin
  Result := TPgSQLRecordSet(FRecordSet).UsedConnection;
end;

function TPgSQLMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TPgSQLRecordSet.Create;
  Result.SetProp(prOIDAsInt, True);
end;

function TPgSQLMetaData.InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData;
begin
  if MetaDataKind = 'sequences' then
    Result := GetSequences(Restrictions)
  else
  if MetaDataKind = 'schemas' then
    Result := GetSchemas(Restrictions)
  else
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions);
end;

procedure TPgSQLMetaData.InternalGetMetaDataKindsList(List: _TStringList);
begin
  inherited;

  List.Add('Databases');
  List.Add('DataTypes');
  List.Add('Users');
  List.Add('Sequences');
  List.Add('Schemas');

  List.Sort;
end;

procedure TPgSQLMetaData.InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string);
begin
  List.Clear;

  if MetaDataKind = 'datatypes' then begin
    List.Add('DATATYPE_SCHEMA');
    List.Add('DATATYPE_NAME');
    List.Add('DATATYPE_OID');
    List.Add('DATATYPE_TYPE');
  end
  else
  if MetaDataKind = 'sequences' then begin
    List.Add('SEQUENCE_SCHEMA');
    List.Add('SEQUENCE_NAME');
    List.Add('SEQUENCE_OID');
  end
  else
    inherited;
end;

function TPgSQLMetaData.GetTables(Restrictions: _TStrings): TData;
const
  fmtGetTablesSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, n.nspname AS TABLE_SCHEMA, c.relname AS TABLE_NAME, ' +
    'c.oid AS TABLE_OID, ' +
    'CASE ' +
    '  WHEN c.relkind = ''r'' THEN ''TABLE'' ' +
    '  WHEN c.relkind = ''v'' THEN ''VIEW'' ' +
    'END::varchar(5) AS TABLE_TYPE ' +
    'FROM pg_class c ' +
    '  INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    'WHERE %s c.relkind IN (%s) ORDER BY n.nspname, c.relname';
var
  WhereClause, TableName, Schema, TableOID, TableTypes, QuotedTypes, Scope: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else
    AddWhere(WhereClause, 'n.nspname', '"' + GetConnection.GetCachedSchema + '"');
  AddWhere(WhereClause, 'c.relname', TableName);
  AddWhere(WhereClause, 'c.oid', TableOID);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  if _UpperCase(TableTypes) = 'TABLE' then
    TableTypes := 'r'
  else
  if _UpperCase(TableTypes) = 'VIEW' then
    TableTypes := 'v';

  if TableTypes <> '' then
    QuotedTypes := PgSQLInfo.ToStringConst(TableTypes)
  else
    QuotedTypes := '''r'', ''v''';

  FRecordSet.SetSQL(_Format(fmtGetTablesSQL, [WhereClause, QuotedTypes]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetColumns(Restrictions: _TStrings): TData;
const
  fmtGetColumnsSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, n.nspname AS TABLE_SCHEMA, c.relname AS TABLE_NAME, ' +
    'c.oid AS TABLE_OID, a.attname AS COLUMN_NAME, a.attnum AS POSITION, ' +
    'a.atttypid AS DATA_TYPE, a.attlen AS DATA_LENGTH, ' +
    '0 AS DATA_PRECISION, 0 AS DATA_SCALE, a.atttypmod AS TYPE_MODIFIER, ' +
    '(not a.attnotnull)::integer AS NULLABLE, ' +
    'pg_get_expr(ad.adbin, ad.adrelid) AS DEFAULT_VALUE ' +
    'FROM pg_class c ' +
    '  INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    '  INNER JOIN pg_attribute a ON a.attrelid = c.oid ' +
    '  LEFT JOIN pg_attrdef ad ON a.attrelid = ad.adrelid and a.attnum = ad.adnum ' +
    'WHERE %s a.attnum > 0 AND not a.attisdropped ORDER BY n.nspname, c.relname, a.attnum';
var
  WhereClause, TableName, Schema, TableOID, ColumnName, SQL: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'n.nspname', Schema);
  AddWhere(WhereClause, 'c.relname', TableName);
  AddWhere(WhereClause, 'c.oid', TableOID);
  AddWhere(WhereClause, 'a.attname', ColumnName);

  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';
  SQL := _Format(fmtGetColumnsSQL, [WhereClause]);

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetProcedures(Restrictions: _TStrings): TData;
const
  fmtGetProceduresSQL = 'SELECT ' +
    'current_database() AS PROCEDURE_CATALOG, ' +
    'n.nspname AS PROCEDURE_SCHEMA, p.proname AS PROCEDURE_NAME, ' +
    '''FUNCTION'' AS PROCEDURE_TYPE ' +
    'FROM pg_proc p ' +
    '  INNER JOIN pg_namespace n ON n.oid = p.pronamespace ' +
    '%s ORDER BY n.nspname, p.proname';
var
  WhereClause, ProcName, Schema, Scope: _string;
begin
  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else
    AddWhere(WhereClause, 'n.nspname', '"' + GetConnection.GetCachedSchema + '"');
  AddWhere(WhereClause, 'p.proname', ProcName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetProceduresSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

{$IFNDEF LITE}
procedure TPgSQLMetaData.CreateProcedureParametersFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 100);
  AddField('PROCEDURE_SCHEMA', dtString, 100);
  AddField('PROCEDURE_NAME', dtString, 100);
  AddField('PARAMETER_NAME', dtString, 100);
  AddField('POSITION', dtInt32);
  AddField('DIRECTION', dtString, 10);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  FMemData.InitFields;
end;

function TPgSQLMetaData.GetProcedureParameters(Restrictions: _TStrings): TData;
var
  Schema, ProcName, OverloadStr, Database: _string;
  Overload: integer;
  ParamsInfo: TProcParamsInfo;
  i, a: integer;
  s: _string;
  HasOutParams: boolean;
  Mode: _char;
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
  Schema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  if Schema <> '' then
    Schema := PgSQLInfo.NormalizeName(Schema, False, True)
  else
    Schema := GetConnection.GetCachedSchema;

  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ProcName := PgSQLInfo.NormalizeName(ProcName, False, True);
  if ProcName = '' then
    raise Exception.Create('PROCEDURE_NAME restriction must be set');

  OverloadStr := Trim(Restrictions.Values['PROCEDURE_OVERLOAD']);
  if OverloadStr <> '' then begin
    Overload := StrToInt(OverloadStr);
    if Overload <= 0 then
      Overload := 1;
  end
  else
    Overload := 1;

  TPgSQLRecordSet(FRecordSet).FCommand.GetProcParamsInfo(Schema, ProcName, Overload, ParamsInfo);

  CreateProcedureParametersFields;
  FMemData.Open;

  Database := GetConnection.FDatabase;

  HasOutParams := False;
  for i := 0 to Length(ParamsInfo.Modes) - 1 do
    if ParamsInfo.Modes[i] <> 'i' then begin
      HasOutParams := True;
      break;
    end;

  FMemDataHelper.AllocBuffer;

  if HasOutParams then
    a := 0
  else
    a := -1;

  for i := a to High(ParamsInfo.TypeOIDs) do begin
    FMemDataHelper.InitRecord;

    FMemDataHelper.FieldValues[dnCATALOG] := Database;
    FMemDataHelper.FieldValues[dnSCHEMA] := Schema;
    FMemDataHelper.FieldValues[dnPROC_NAME] := ProcName;
    if (i >= 0) and (i < Length(ParamsInfo.Names)) and (ParamsInfo.Names[i] <> '""') then
      FMemDataHelper.FieldValues[dnPARAM_NAME] := ParamsInfo.Names[i];
    FMemDataHelper.FieldValues[dnPOSITION] := i + 1;

    if i = -1 then
      Mode := 'o'
    else
    if i < Length(ParamsInfo.Modes) then begin
      s := ParamsInfo.Modes[i];
      if s <> '' then
        Mode := s[1]
      else
        Mode := 'i';
    end
    else
      Mode := 'i';

    case Mode of
      'i':
        Direction := 'IN';
      'o':
        Direction := 'OUT';
      'b':
        Direction := 'IN/OUT';
    end;
    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;

    if i = -1 then
      FMemDataHelper.FieldValues[dnDATA_TYPE] := ParamsInfo.RetTypeOid
    else
      FMemDataHelper.FieldValues[dnDATA_TYPE] := ParamsInfo.TypeOIDs[i];

    FMemDataHelper.AppendRecord;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;
{$ENDIF}

function TPgSQLMetaData.GetIndexes(Restrictions: _TStrings): TData;
const
  fmtGetIndexesSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS TABLE_SCHEMA, t.relname AS TABLE_NAME, t.oid AS TABLE_OID, ' +
    'current_database() AS INDEX_CATALOG, ' +
    'n.nspname AS INDEX_SCHEMA, c.relname AS INDEX_NAME, c.oid AS INDEX_OID, ' +
    'i.indisunique::integer AS UNIQUE ' +
    'FROM pg_index i ' +
    '  INNER JOIN pg_class c ON c.oid = i.indexrelid ' +
    '  INNER JOIN pg_class t ON t.oid = i.indrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.relnamespace ' +
    '%s ORDER BY n.nspname, t.relname, c.relname';
var
  WhereClause, TableName, TableSchema, TableOID, IndexName, IndexOID: _string;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  IndexOID := Trim(Restrictions.Values['INDEX_OID']);

  WhereClause := '';
  AddWhere(WhereClause, 'n.nspname', TableSchema);
  AddWhere(WhereClause, 't.relname', TableName);
  AddWhere(WhereClause, 'i.indrelid', TableOID);
  AddWhere(WhereClause, 'c.relname', IndexName);
  AddWhere(WhereClause, 'i.indexrelid', IndexOID);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetIndexesSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetIndexColumns(Restrictions: _TStrings): TData;
const
  // ORDER BY indisprimary::integer DESC - index for Primary Key must be first

  fmtGetIndexColumnsSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS TABLE_SCHEMA, t.relname AS TABLE_NAME, t.oid AS TABLE_OID, ' +
    'current_database() AS INDEX_CATALOG, ' +
    'n.nspname AS INDEX_SCHEMA, c.relname AS INDEX_NAME, c.oid AS INDEX_OID, ' +
    'a.attname AS COLUMN_NAME, a.attnum AS COLUMN_POSITION, 0 AS DESCENDING ' +
  {$IFDEF LITE}
    ', i.indisunique::integer AS UNIQUE ' +
  {$ENDIF}
    'FROM pg_index i ' +
    '  INNER JOIN pg_class c ON c.oid = i.indexrelid ' +
    '  INNER JOIN pg_class t ON t.oid = i.indrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.relnamespace ' +
    '  INNER JOIN pg_attribute a ON a.attrelid = i.indrelid and a.attnum = any (i.indkey) ' +
    '%s ORDER BY indisprimary::integer DESC, n.nspname, t.relname, c.relname, a.attnum';
    
  fmtGetIndexColumnsSQL83 = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS TABLE_SCHEMA, t.relname AS TABLE_NAME, t.oid AS TABLE_OID, ' +
    'current_database() AS INDEX_CATALOG, ' +
    'n.nspname AS INDEX_SCHEMA, c.relname AS INDEX_NAME, c.oid AS INDEX_OID, ' +
    'a.attname AS COLUMN_NAME, a.attnum AS COLUMN_POSITION,' +
    'indoption[index] & 1 AS DESCENDING ' +
  {$IFDEF LITE}
    ', indisunique::integer AS UNIQUE ' +
  {$ENDIF}
    'FROM ' +
    '  (SELECT generate_series(0, indnatts - 1), indrelid, indexrelid, indkey,' +
    '   indoption, indisunique, indisprimary FROM pg_index i)' +
    '   i(index, indrelid, indexrelid, indkey, indoption, indisunique, indisprimary)' +
    '  INNER JOIN pg_class c ON c.oid = indexrelid ' +
    '  INNER JOIN pg_class t ON t.oid = indrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.relnamespace ' +
    '  INNER JOIN pg_attribute a ON a.attrelid = indrelid and a.attnum = indkey[index] ' +
    '%s ORDER BY indisprimary::integer DESC, n.nspname, t.relname, c.relname, a.attnum';
var
  WhereClause, TableName, TableSchema, TableOID, IndexName, IndexOID, Uniqueness, SQL: _string;
begin
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  IndexOID := Trim(Restrictions.Values['INDEX_OID']);
  Uniqueness := Trim(Restrictions.Values['UNIQUE']);

  WhereClause := '';
  AddWhere(WhereClause, 'n.nspname', TableSchema);
  AddWhere(WhereClause, 't.relname', TableName);
  AddWhere(WhereClause, 'i.indrelid', TableOID);
  AddWhere(WhereClause, 'c.relname', IndexName);
  AddWhere(WhereClause, 'i.indexrelid', IndexOID);

  if (Uniqueness = '0') or (Uniqueness = '1') then begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    if Uniqueness = '1' then
      WhereClause := WhereClause + 'i.indisunique'
    else
      WhereClause := WhereClause + 'not i.indisunique';
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;


  Assert(FRecordSet <> nil);
  Assert(TPgSqlRecordSet(FRecordSet).FCommand.FConnection <> nil);
  if TPgSqlRecordSet(FRecordSet).FCommand.FConnection.VersionIsEqualOrHigher(8, 3) then
    SQL := _Format(fmtGetIndexColumnsSQL83, [WhereClause])
  else
    SQL := _Format(fmtGetIndexColumnsSQL, [WhereClause]);

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetConstraints(Restrictions: _TStrings): TData;
const
  fmtGetConstraintsSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, n.nspname AS TABLE_SCHEMA, c.relname AS TABLE_NAME, ' +
    'c.oid AS TABLE_OID, con.conname AS CONSTRAINT_NAME, ' +
    'CASE con.contype ' +
    '  WHEN ''c'' THEN ''CHECK'' ' +
    '  WHEN ''f'' THEN ''FOREIGN KEY'' ' +
    '  WHEN ''p'' THEN ''PRIMARY KEY'' ' +
    '  WHEN ''u'' THEN ''UNIQUE'' ' +
    'END::varchar(11) AS CONSTRAINT_TYPE ' +
    'FROM pg_constraint con ' +
    '  INNER JOIN pg_class c ON c.oid = con.conrelid ' +
    '  INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    '%s ORDER BY n.nspname, c.relname, con.conname ';
var
  WhereClause, Schema, TableName, TableOID, ConstraintName, Types: _string;
begin
  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableOID := Trim(Restrictions.Values['TABLE_OID']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  Types := _LowerCase(Trim(Restrictions.Values['CONSTRAINT_TYPE']));

  WhereClause := '';
  AddWhere(WhereClause, 'n.nspname', Schema);
  AddWhere(WhereClause, 'c.relname', TableName);
  AddWhere(WhereClause, 'con.conrelid', TableOID);
  AddWhere(WhereClause, 'con.conname', ConstraintName);

  if Types <> '' then begin
    if Types = 'check' then
      Types := 'c'
    else
    if Types = 'foreign key' then
      Types := 'f'
    else
    if Types = 'primary key' then
      Types := 'p'
    else
    if Types = 'unique' then
      Types := 'u';

    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'con.contype = ' + PgSQLInfo.ToStringConst(Types);
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.SetSQL(_Format(fmtGetConstraintsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetDataTypes(Restrictions: _TStrings): TData;
const
  fmtGetDataTypesSQL = 'SELECT ' +
    'current_database() AS DATATYPE_CATALOG, n.nspname AS DATATYPE_SCHEMA, ' +
    't.typname AS DATATYPE_NAME, t.oid AS DATATYPE_OID, t.typlen AS DATATYPE_LENGTH, ' +
    'CASE ' +
    '  WHEN t.typtype = ''b'' THEN ''base'' ' +
    '  WHEN t.typtype = ''c'' THEN ''composite'' ' +
    '  WHEN t.typtype = ''d'' THEN ''domain'' ' +
    '  WHEN t.typtype = ''e'' THEN ''enum'' ' +
    '  WHEN t.typtype = ''p'' THEN ''pseudo'' ' +
    'END::varchar(9) AS DATATYPE_TYPE, t.typrelid AS TABLE_OID ' +
    'FROM pg_type t ' +
    '  INNER JOIN pg_namespace n ON n.oid = t.typnamespace ' +
    '%s ORDER BY n.nspname, t.typname';
var
  WhereClause, TypeName, Schema, OID, TypeType, SQL: _string;
begin
  Schema := Trim(Restrictions.Values['DATATYPE_SCHEMA']);
  TypeName := Trim(Restrictions.Values['DATATYPE_NAME']);
  OID := Trim(Restrictions.Values['DATATYPE_OID']);
  TypeType := _LowerCase(Trim(Restrictions.Values['DATATYPE_TYPE']));

  WhereClause := '';
  AddWhere(WhereClause, 'n.nspname', Schema);
  AddWhere(WhereClause, 't.typname', TypeName);
  AddWhere(WhereClause, 't.oid', OID);

  if TypeType <> '' then begin
    if TypeType = 'base' then
      TypeType := 'b'
    else
    if TypeType = 'composite' then
      TypeType := 'c'
    else
    if TypeType = 'domain' then
      TypeType := 'd'
    else
    if TypeType = 'enum' then
      TypeType := 'e'
    else
    if TypeType = 'pseudo' then
      TypeType := 'p';

    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 't.typtype = ' + PgSQLInfo.ToStringConst(TypeType);
  end;

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;
  SQL := _Format(fmtGetDataTypesSQL, [WhereClause]);

  FRecordSet.SetSQL(SQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetSequences(Restrictions: _TStrings): TData;
const
  fmtGetSequencesSQL = 'SELECT ' +
    'current_database() AS TABLE_CATALOG, ' +
    'n.nspname AS SEQUENCE_SCHEMA, c.relname AS SEQUENCE_NAME, c.oid AS SEQUENCE_OID ' +
    'FROM pg_class c ' +
    '  INNER JOIN pg_namespace n ON n.oid = c.relnamespace ' +
    'WHERE %s c.relkind IN (''S'') ORDER BY n.nspname, c.relname';
var
  WhereClause, Schema, ObjectName, OID, Scope: _string;
begin
  Schema := Trim(Restrictions.Values['SEQUENCE_SCHEMA']);
  ObjectName := Trim(Restrictions.Values['SEQUENCE_NAME']);
  OID := Trim(Restrictions.Values['SEQUENCE_OID']);
  Scope := _UpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if Scope <> 'LOCAL' then
    AddWhere(WhereClause, 'n.nspname', Schema)
  else
    AddWhere(WhereClause, 'n.nspname', '"' + GetConnection.GetCachedSchema + '"');
  AddWhere(WhereClause, 'c.relname', ObjectName);
  AddWhere(WhereClause, 'c.oid', OID);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  FRecordSet.SetSQL(_Format(fmtGetSequencesSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetSchemas(Restrictions: _TStrings): TData;
const
  fmtGetSchemasSQL = 'SELECT ' +
    'current_database() AS SCHEMA_CATALOG, ' +
    'n.nspname AS SCHEMA_NAME, n.oid AS SCHEMA_OID ' +
    'FROM pg_namespace n ' +
    'ORDER BY n.nspname';
begin
  FRecordSet.SetSQL(fmtGetSchemasSQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetDatabases(Restrictions: _TStrings): TData;
const
  fmtGetDatabasesSQL = 'SELECT ' +
    'datname AS DATABASE_NAME, pg_encoding_to_char(encoding) AS DATABASE_CHARSET ' +
    'FROM pg_database ' +
    'ORDER BY datname';
begin
  FRecordSet.SetSQL(fmtGetDatabasesSQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TPgSQLMetaData.GetUsers(Restrictions: _TStrings): TData;
const
  fmtGetUsersSQL = 'SELECT ' +
    'usename AS USER_NAME ' +
    'FROM pg_user ' +
    'ORDER BY usename';
begin
  FRecordSet.SetSQL(fmtGetUsersSQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

{$IFNDEF LITE}

{ TPgSQLAlerter }

constructor TPgSQLAlerter.Create;
begin
  inherited;
end;

destructor TPgSQLAlerter.Destroy;
begin
  inherited;
end;

function TPgSQLAlerter.SetProp(Prop: integer; const Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited SetProp(Prop, Value);
  //end;
end;

function TPgSQLAlerter.GetProp(Prop: integer; var Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited GetProp(Prop, Value);
  //end;
end;

procedure TPgSQLAlerter.SendEvent(const EventName, Message: _string);
begin
  if (FConnection <> nil) and (TPgSQLConnection(FConnection).VersionIsEqualOrHigher(9, 0)) then
    TPgSQLConnection(FConnection).ExecCommand('NOTIFY ' + EventName + ', ''' + Message + '''')
  else
    TPgSQLConnection(FConnection).ExecCommand('NOTIFY ' + EventName);
end;

procedure TPgSQLAlerter.Start;
begin
  if not FActive then begin
    TPgSQLConnection(FConnection).GetNotificationsHandler.RegisterEvents(
      FEventNames, DoOnEvent);
    FActive := True;
  end;
end;

procedure TPgSQLAlerter.Stop;
begin
  if FActive then begin
    TPgSQLConnection(FConnection).GetNotificationsHandler.UnregisterEvents(
      FEventNames, Self);
    FActive := False;
  end;
end;

procedure TPgSQLAlerter.DoOnEvent(const EventName: _string; const PID: integer; const Message: _string);
var
  InhOnEvent: TCRAlerterEventCallback;
begin
  InhOnEvent := inherited OnEvent;
  if Assigned(InhOnEvent) then
    InhOnEvent(EventName, Message);

  if Assigned(FOnEvent) then
    FOnEvent(EventName, PID, Message);
end;

{ TPgSQLNotificationsHandler }

{$IFDEF MSWINDOWS}

const
  WM_NOTIFICATION = WM_USER + 1;

function WndProc(hWindow: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; {$IFNDEF CLR} stdcall; {$ENDIF}
var
  Handler: TPgSQLNotificationsHandler;
  Notif: TPgSQLNotification;
begin
  Result := 0;
  case Msg of
    WM_NOTIFICATION: begin
      try
        Handler := TPgSQLNotificationsHandler(GetGCHandleTarget(IntPtr(wParam)));
        Notif := TPgSQLNotification(GetGCHandleTarget(IntPtr(lParam)));
        FreeGCHandle(IntPtr(lParam));
        try
          Handler.DoOnNotification(Notif.PID, Notif.Name, Notif.Message);
        finally
          Notif.Free;
        end;
      except
        if Assigned(ApplicationHandleException) then
          ApplicationHandleException(nil);
      end;
    end
    else
      Result := DefWindowProc(hWindow, Msg, wParam, lParam);
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
    lpszClassName: 'PGDACUtilWnd'
  );

procedure AllocWnd;
var
  TempClass: {$IFDEF CLR}TWndClassInfo{$ELSE}TWndClass{$ENDIF};
  ClassRegistered: boolean;
{$IFDEF CLR}
  WndClassPtr: IntPtr;
{$ENDIF}
  WndProcPtr: IntPtr;
begin
  if hUtilWindow = 0 then begin
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

      hUtilWindow := Windows.RegisterClass(WndClass);
    end;

    hUtilWindow := Windows.CreateWindowEx(WS_EX_TOOLWINDOW, 'PGDACUtilWnd',
      '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
    Assert(hUtilWindow > 0);
    Windows.SetWindowLong(hUtilWindow, GWL_WNDPROC, Longint(WndProcPtr));
  end;
end;

{$ENDIF}

constructor TPgSQLNotificationsHandler.Create(BaseConnection: TPgSQLConnection);
begin
  inherited Create;

  FBaseConnection := BaseConnection;
  FConnection := TPgSQLConnection.Create;
  FConnection.OnNotification := ProcessNotification;
  FLockConnection := TCriticalSection.Create;
  FAllowLockEvent := TEvent.Create(nil, True, True, '');
end;

destructor TPgSQLNotificationsHandler.Destroy;
begin
  FConnection.Free;
  FLockConnection.Free;
  FAllowLockEvent.Free;
{$IFDEF MSWINDOWS}
  if FGCHandle <> nil then
    FreeGCHandle(FGCHandle);
{$ENDIF}

  inherited;
end;

procedure TPgSQLNotificationsHandler.RegisterEvents(Events: _TStrings;
  Callback: TPgSQLNotificationEvent);
var
  i, j, l: integer;
  NormName: _string;
begin
  try
    for i := 0 to Events.Count - 1 do begin
      NormName := PgSQLInfo.NormalizeName(Events[i], False, True);
      j := EventIndex(NormName);
      if j < 0 then begin
        Listen(NormName);
        j := Length(FEvents);
        SetLength(FEvents, j + 1);
        FEvents[j].EventName := NormName;
      end;
      l := Length(FEvents[j].Callbacks);
      SetLength(FEvents[j].Callbacks, l + 1);
      FEvents[j].Callbacks[l] := Callback;
    end;
  finally
    if FConnectionIsLocked then begin
      FLockConnection.Leave;
      FConnectionIsLocked := False;
    end;
  end;
end;

procedure TPgSQLNotificationsHandler.UnregisterEvents(Events: _TStrings;
  CallbackObject: TObject);
var
  i: integer;
{$IFNDEF CLR}
  j, k, l, l2, m: integer;
  NormName: _string;
{$ENDIF}
  UnlistenNames: array of _string;
begin
  try
  {$IFNDEF CLR}
    for i := 0 to Events.Count - 1 do begin
      NormName := PgSQLInfo.NormalizeName(Events[i], False, True);
      j := EventIndex(NormName);
      if j < 0 then
        continue;
      l := Length(FEvents[j].Callbacks);
      for k := 0 to l - 1 do begin
        if TMethod(FEvents[j].Callbacks[k]).Data = CallbackObject then begin
          if l > 1 then begin
            for m := k + 1 to l - 1 do
              FEvents[j].Callbacks[m - 1] := FEvents[j].Callbacks[m];
            SetLength(FEvents[j].Callbacks, l - 1);
          end
          else begin
            for m := j + 1 to High(FEvents) do
              FEvents[m - 1] := FEvents[m];
            SetLength(FEvents, Length(FEvents) - 1);

            l2 := Length(UnlistenNames);
            SetLength(UnlistenNames, l2 + 1);
            UnlistenNames[l2] := NormName;
          end;
          break;
        end;
      end;
    end;
  {$ELSE}
    SetLength(FEvents, 0);
  {$ENDIF}

    if Length(FEvents) = 0 then
      Stop
    else
      for i := 0 to High(UnlistenNames) do
        Unlisten(UnlistenNames[i]);
  finally
    if FConnectionIsLocked then begin
      FLockConnection.Leave;
      FConnectionIsLocked := False;
    end;
  end;
end;

function TPgSQLNotificationsHandler.EventIndex(const Name: _string): integer;
var
  i: integer;
begin
  for i := 0 to High(FEvents) do begin
    if FEvents[i].EventName = Name then begin
      Result := i;
      exit;
    end;
  end;
  Result := -1;
end;

procedure TPgSQLNotificationsHandler.Start;
begin
  FConnection.Assign(FBaseConnection);
  FConnection.Connect('');
  FLockConnection.Enter;
  FConnectionIsLocked := True;

{$IFDEF MSWINDOWS}
  if hUtilWindow = 0 then
    AllocWnd;
{$ENDIF}    
  FThread := TPgSQLNotificationsThread.Create(True);
  FThread.FHandler := Self;
  FThread.Resume;
end;

procedure TPgSQLNotificationsHandler.Stop;
begin
  FThread.Terminate;
  if not FConnectionIsLocked then
    LockConnection;
  try
    FConnection.Disconnect;
  finally
    FLockConnection.Leave;
    FConnectionIsLocked := False;
  end;
  FThread.WaitFor;
  FThread.Free;
  FThread := nil;
end;

procedure TPgSQLNotificationsHandler.Listen(const Name: _string);
begin
  if not FConnection.GetConnected then
    Start
  else begin
    if not FConnectionIsLocked then
      LockConnection;
  end;

  FConnection.ExecCommand('LISTEN "' + Name + '"');
end;

procedure TPgSQLNotificationsHandler.Unlisten(const Name: _string);
begin
  if not FConnectionIsLocked then
    LockConnection;

  FConnection.ExecCommand('UNLISTEN "' + Name + '"');
end;

procedure TPgSQLNotificationsHandler.LockConnection;
begin
  FAllowLockEvent.ResetEvent;
  FConnection.FProtocol.TerminateMessageLoop;
  FLockConnection.Enter; // wait until ProcessMessageQueue exits
  FAllowLockEvent.SetEvent;
  FConnectionIsLocked := True;
end;

{$IFDEF MSWINDOWS}
function TPgSQLNotificationsHandler.GetGCHandle: IntPtr;
begin
  if FGCHandle = nil then
    FGCHandle := AllocGCHandle(Self);
  Result := FGCHandle;
end;
{$ENDIF}

procedure TPgSQLNotificationsHandler.ProcessNotification(const Name: _string; const PID: integer; const Message: _string);
var
  Notif: TPgSQLNotification;
begin
  Notif := TPgSQLNotification.Create;
  Notif.PID := PID;
  Notif.Name := Name;
  Notif.Message := Message;
{$IFDEF MSWINDOWS}
  PostMessage(hUtilWindow, WM_NOTIFICATION, {$IFNDEF CLR}NativeInt{$ELSE}Integer{$ENDIF}(GCHandle), {$IFNDEF CLR}NativeInt{$ELSE}Integer{$ENDIF}(AllocGCHandle(Notif)));
{$ELSE}
  try
    DoOnNotification(Notif.PID, Notif.Name, Notif.Message);
  finally
    Notif.Free;
  end;
{$ENDIF}
end;

procedure TPgSQLNotificationsHandler.DoOnNotification(NotificationPID: Integer; const NotificationName: _string; const NotificationMsg: _string);
var
  i: integer;
  index: integer;
begin
  index := EventIndex(NotificationName);
  if index >= 0 then begin
    for i := 0 to High(FEvents[index].Callbacks) do begin
      if Assigned(FEvents[index].Callbacks[i]) then
        FEvents[index].Callbacks[i](NotificationName, NotificationPID, NotificationMsg);
    end;
  end;

end;

{ TPgSQLNotificationsThread }

procedure TPgSQLNotificationsThread.Execute;
var
  Protocol: TPgSQLProtocol;
begin
  Protocol := FHandler.FConnection.GetProtocol;
  Assert(Protocol <> nil);

  while not Terminated do begin
    FHandler.FAllowLockEvent.WaitFor(INFINITE);
    if Terminated then
      exit;
    FHandler.FLockConnection.Enter;
    try
      Protocol.ProcessMessageQueue(#0, False, True);
    except
      FHandler.FLockConnection.Leave;
      exit;
    end;
    FHandler.FLockConnection.Leave;
  end;
end;

{ TPgSQLLoader }

constructor TPgSQLLoader.Create;
begin
  inherited;

  FCommand := TPgSQLCommand.Create;
  FBufferSize := WriteBufferSize;
end;

destructor TPgSQLLoader.Destroy;
begin
  FCommand.Free;

  inherited;
end;

function TPgSQLLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prBufferSize:
      FBufferSize := Value;
    prTextMode:
      FTextMode := Value;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

function TPgSQLLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prBufferSize:
      Value := FBufferSize;
    prTextMode:
      Value := FTextMode;
  else
    Assert(False, IntToStr(Prop));
    Result := False;
  end;
end;

class function TPgSQLLoader.GetColumnClass: TCRLoaderColumnClass;
begin
  Result := TPgSQLLoaderColumn;
end;

class function TPgSQLLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TPgSQLRecordSet;
end;

procedure TPgSQLLoader.FillColumn(Column: TCRLoaderColumn; FieldDesc: TFieldDesc);
begin
  inherited;

  with TPgSQLLoaderColumn(Column) do begin
    FSubDataType := FieldDesc.SubDataType;
    FRowType := TPgRowType(FieldDesc.ObjectType);
    if FRowType <> nil then
      FRowTypeName := FRowType.Name;
  end;
end;

procedure TPgSQLLoader.Reset;
begin
  SetLength(FRowValues, 0); // to clear
  FPrepared := False;
  FBlockOpened := False;

  inherited;
end;

procedure TPgSQLLoader.Prepare;
var
  SQL: _StringBuilder;
  i: integer;
  Col: TPgSQLLoaderColumn;
begin
  inherited;

  FIsTextMode := FTextMode or (FConnection.GetProtocolVersion = pv20);

  SetLength(FRowValues, Columns.Count);

  for i := 0 to Columns.Count - 1 do begin
    Col := TPgSQLLoaderColumn(Columns[i]);
    if not FIsTextMode and (Col.DataType = dtObject) and (Col.FRowType = nil) then
      Col.FRowType := TPgRowType(FConnection.GetTypes.GetRowType(Col.FRowTypeName));
  end;

  SQL := _StringBuilder.Create(1024);
  try
    SQL.Append('COPY ');
    if not FIsTextMode then
      SQL.Append('BINARY ');

    SQL.Append(PgSQLInfo.NormalizeName(TableName) + '(');
    for i := 0 to Columns.Count - 1 do begin
      if i > 0 then
        SQL.Append(', ');
      SQL.Append(PgSQLInfo.NormalizeName(Columns[i].Name));
    end;
    SQL.Append(') FROM STDIN');

    FCommand.SetSQL(SQL.ToString);
  finally
    SQL.Free;
  end;

  FCommand.SetConnection(FConnection);
  FCommand.Prepare;
  FCommand.Execute;

  if not FIsTextMode then begin
    BeginDataBlock;
    WriteHeader;
  end;

  FPrepared := True;
end;

procedure TPgSQLLoader.PutColumnData(Col, Row: integer; const Value: variant);
begin
  if (FLastRow <> -1) and (Row > FLastRow + 1) then
    LoadRow;

  inherited;

  FRowValues[Col] := Value;
end;

procedure TPgSQLLoader.DoLoad;
begin
  if FLastRow >= FLoadedRows then
    LoadRow;
end;

procedure TPgSQLLoader.LoadRow;
var
  i: integer;
  s: AnsiString;
  Col: TPgSQLLoaderColumn;
  Value: variant;
  UseUnicode: boolean;
begin
  FConnection.GetProp(prUseUnicode, Value);
  UseUnicode := Value;

  BeginDataBlock;

  if not FIsTextMode then
    FNet.WriteInt16(Columns.Count);

  for i := 0 to Columns.Count - 1 do begin
    Col := TPgSQLLoaderColumn(Columns[i]);
    Value := FRowValues[i];
    if FIsTextMode then begin
      if VarIsNull(Value) or VarIsEmpty(Value) then
        s := '\N'
      else
        s := TPgTextConverter.ValueToText(Value, Col.DataType,
          UseUnicode, False, True);
      FNet.WriteBytes(TValueArr(s), 0, Length(s));
      if i < Columns.Count - 1 then
        FNet.WriteChar(#9);
    end
    else
      TPgBinaryConverter.WriteValue(Value, FNet, Col.DataType, Col.FSubDataType,
        Col.FRowType, FConnection);
  end;

  if FIsTextMode then
    FNet.WriteChar(#10);

  if FNet.GetWriteBufferSize >= FBufferSize then
    EndDataBlock;

  Inc(FLoadedRows);
end;

procedure TPgSQLLoader.BeginDataBlock;
begin
  if not FBlockOpened then begin
    FConnection.GetProtocol.BeginCopyDataBlock(FNet);
    FBlockOpened := True;
  end;
end;

procedure TPgSQLLoader.EndDataBlock;
begin
  if FBlockOpened then begin
    FConnection.GetProtocol.EndCopyDataBlock;
    FBlockOpened := False;
  end;
end;

procedure TPgSQLLoader.WriteHeader;
var
  s: AnsiString;
begin
  s := 'PGCOPY'#10#255#13#10#0;
  FNet.WriteBytes(TValueArr(s), 0, Length(s));
  FNet.WriteInt32(0); // flags
  FNet.WriteInt32(0); // extension
end;

procedure TPgSQLLoader.Finish;
begin
  if FPrepared then begin
    BeginDataBlock;
    if FIsTextMode then begin
      FNet.WriteChar('\');
      FNet.WriteChar('.');
      FNet.WriteChar(#10);
    end
    else
      FNet.WriteInt16(-1); // trailer

    EndDataBlock;
    FConnection.GetProtocol.PutCopyEnd;

    FCommand.UnPrepare;
  end;

  inherited;
end;

procedure TPgSQLLoader.SetConnection(Value: TCRConnection);
begin
  inherited;

  FConnection := TPgSQLConnection(Value);
end;

{$ENDIF}

initialization
  PgSQLInfo := TPgSQLInfo.Create(nil);


finalization
  PgSQLInfo.Free;


end.

