{$IFNDEF CLR}
{$I UniDac.inc}
unit Uni;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
{$IFDEF CLR}
  System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
  SysUtils, Classes, Variants, DB,
  MemData, CRAccess, CRConnectionPool, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  DBAccess, MemUtils, CRParser, CRVio, UniProvider;


{$I UniDacVer.inc}

type
  TUniConnection = class;
  TUniParam = class;
  TUniParams = class;
  TUniSQL = class;
  TUniTransaction = class;
  TCustomUniDataSet = class;
  TUniUpdateSQL = class;
  TUniCursor = class;

  EUniError = class(EDAError)
  private
    FInnerError: EDAError;
  public
    constructor Create(AInnerError: EDAError);
    destructor Destroy; override;

    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;

    property InnerError: EDAError read FInnerError;
  end;

  TStringOptionsHolder = class
  private
    FValues: _TStrings;
    FIsModified: boolean;
  protected
    procedure ValuesChanging(Sender: TObject); virtual;
    procedure ValuesChanged(Sender: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Values: _TStrings read FValues;
    property IsModified: boolean read FIsModified write FIsModified;
  end;

  TUniConnectionOptions = class(TDAConnectionOptions)
  private
    function GetConvertEOL: boolean;
    procedure SetConvertEOL(Value: boolean);

  published
    property ConvertEOL: boolean read GetConvertEOL write SetConvertEOL default False;

    property DisconnectedMode;
    property KeepDesignConnected;
    property LocalFailover;
    property DefaultSortType;
    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}
  end;

  TUniConnectionStringOptions = class(TStringOptionsHolder)
  private
    FOwner: TUniConnection;
  protected
    procedure ValuesChanging(Sender: TObject); override;
  public
    constructor Create(AOwner: TUniConnection);
  end;

  TUniMacro = class(TCollectionItem)
  private
    FName: _string;
    FValue: _string;
    FCondition: _string;

    procedure SetName(const Value: _string);
    procedure SetValue(const Value: _string);
    procedure SetCondition(const Value: _string);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    //function GetDisplayName: string; override;

  public

  published
    property Name: _string read FName write SetName;
    property Value: _string read FValue write SetValue;
    property Condition: _string read FCondition write SetCondition;
  end;

  TUniMacros = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TUniMacro;
    procedure SetItem(Index: integer; Value: TUniMacro);
    procedure NotifyOwner;

  protected
    procedure Update(Item: TCollectionItem); override;

  public
    constructor Create(Owner: TPersistent);
    procedure Add(const Name, Value: _string; const Condition: _string = '');
    function FindMacro(const Name: _string): TUniMacro;
    function MacroByName(const Name: _string): TUniMacro;

    property Items[Index: integer]: TUniMacro read GetItem write SetItem; default;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniConnection = class(TCustomDAConnection)
  private
    FInternalTransaction: TDATransaction;
    FProviderName: string;
    FProvider: TUniProvider;
    FPort: integer;
    FDatabase: _string;
    FSpecificOptions: TUniConnectionStringOptions;
    FMacros: TUniMacros;
    FSQLFormatter: TUniSqlFormatter;
    FMacroNames: _TStringList;
    FMacroValues: _TStringList;
    FMacrosChanged: boolean;
    FMacrosVersion: integer;

    function GetProviderName: string;
    procedure SetProviderName(Value: string);

  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(Value: integer);
    procedure SetDatabase(const Value: _string);
    function GetSpecificOptions: _TStrings;
    procedure SetSpecificOptions(Value: _TStrings);
    function GetTransaction(Index: Integer): TUniTransaction;
    function GetOptions: TUniConnectionOptions;
    procedure SetOptions(Value: TUniConnectionOptions);
    procedure SetMacros(Value: TUniMacros);
    function IsMacrosStored: boolean;

  protected
    FIConnection: TCRConnection;

    procedure CheckProvider;
    function CanGetProvider: boolean;
    function GetProvider: TUniProvider;

    function GetIConnectionClass: TCRConnectionClass; override;
    function GetICommandClass: TCRCommandClass; override;
    function GetIRecordSetClass: TCRRecordSetClass; override;
    function GetIMetaDataClass: TCRMetaDataClass; override;

    procedure CreateIConnection; override;
    procedure SetIConnection(Value: TCRConnection); override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;

    function SQLMonitorClass: TClass; override;
    function ConnectDialogClass: TConnectDialogClass; override;
    function CreateOptions: TDAConnectionOptions; override;
    function IsMultipleTransactionsSupported: boolean; override;
    function UsedTransaction: TDATransaction; override;
    function GetInTransaction: boolean; override;

    procedure DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean; ReconnectAttempt: integer;
      var ConnLostCause: TConnLostCause); override;
    procedure AssignConnectOptions(Source: TCustomDAConnection); override;
    procedure CheckSqlFormatter;
    procedure DetectActiveMacros;
    procedure ExpandMacros(var SQL: _string);

    function DefaultTableSchema: _string; override;

    function GetServerVersion: _string;
    function GetServerVersionFull: _string;
    function GetClientVersion: _string;

    // CLR
    property DefaultTransaction;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateDataSet: TCustomDADataSet; override;
    function CreateSQL: TCustomDASQL; override;
    function CreateTransaction: TDATransaction; override;
    function CreateMetaData: TDAMetaData; override;

    procedure AssignConnect(Source: TUniConnection);
    function ParamByName(const Name: _string): TUniParam;
    function MacroByName(const Name: _string): TUniMacro;
    function ActiveMacroValueByName(const Name: _string): Variant;

    procedure StartTransaction; overload; override;
    procedure StartTransaction(IsolationLevel: TCRIsolationLevel; ReadOnly: boolean = False); reintroduce; overload;
    procedure CommitRetaining;
    procedure RollbackRetaining;
    procedure Savepoint(const Name: _string);
    procedure RollbackToSavepoint(const Name: _string);
    procedure ReleaseSavepoint(const Name: _string);

    property TransactionCount;
    property Transactions[Index: integer]: TUniTransaction read GetTransaction;
    property ServerVersion: _string read GetServerVersion;
    property ServerVersionFull: _string read GetServerVersionFull;
    property ClientVersion: _string read GetClientVersion;

  published
    property ProviderName: string read GetProviderName write SetProviderName;
    property Port: integer read FPort write SetPort default 0;
    property Database: _string read FDatabase write SetDatabase;
    property SpecificOptions: _TStrings read GetSpecificOptions write SetSpecificOptions;
    property Options: TUniConnectionOptions read GetOptions write SetOptions;
    property Macros: TUniMacros read FMacros write SetMacros stored IsMacrosStored;

    property IOHandler;
    property PoolingOptions;
    property Pooling;
    property Debug;
    property Username;
    property Password;
    property Server;
    property Connected stored IsConnectedStored;
    property ConnectDialog;
    property LoginPrompt;

    property AfterConnect;
    property BeforeConnect;
    property AfterDisconnect;
    property BeforeDisconnect;
    property OnLogin;
    property OnError;
    property OnConnectionLost;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniTransaction = class(TDATransaction)
  private
    function GetDefaultConnection: TUniConnection;
    procedure SetDefaultConnection(Value: TUniConnection);
    function GetConnection(Index: integer): TUniConnection;

  protected
    function GetProvider: TUniProvider;

    function GetITransactionClass: TCRTransactionClass; override;
    function SQLMonitorClass: TClass; override;
    function CanAutoCommitExplicitTransaction: boolean; override;

    procedure DoSavepoint(const Name: _string); override;
    procedure DoReleaseSavepoint(const Name: _string); override;
    procedure DoRollbackToSavepoint(const Name: _string); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure CommitRetaining;
    procedure RollbackRetaining;

    procedure Savepoint(Name: _string);
    procedure ReleaseSavePoint(Name: _string);
    procedure RollbackToSavepoint(Name: _string);

    procedure AddConnection(Connection: TUniConnection);
    procedure RemoveConnection(Connection: TUniConnection);

    property Connections[Index: integer]: TUniConnection read GetConnection;
    property ConnectionsCount;

  published
    property DefaultConnection: TUniConnection read GetDefaultConnection write SetDefaultConnection;

    property TransactionType;
    property IsolationLevel;
    property ReadOnly;
    property DefaultCloseAction;

    property OnError;
    property OnStart;
    property OnCommit;
    property OnRollback;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniSQL = class(TCustomDASQL)
  private
    FSpecificOptions: TStringOptionsHolder;
    FFixedUsedTransaction: TDATransaction;
    FWriteAllParams: boolean;
    FParamRefs: TList;
    FLockParamRefsReset: boolean;
    FMacrosVersion: integer;
    FEnableUniSQL: boolean;

    procedure ResetParamRefs;

    function GetParams: TUniParams;
    procedure SetParams(Value: TUniParams);
    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetSpecificOptions: _TStrings;
    procedure SetSpecificOptions(Value: _TStrings);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);

  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure SetICommand(Value: TCRCommand); override;
    function GetDataTypesMap: TDataTypesMapClass; override;

    function CanGetProvider: boolean;
    function GetProvider: TUniProvider;

    function CreateParamsObject: TDAParams; override;

    function UsedTransaction: TDATransaction; override;

    procedure BeginConnection(NoConnectCheck: boolean = True); override;
    procedure EndConnection; override;

    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalExecute(Iters: integer); override;
    function GetFinalSQL: _string; override;
    procedure CheckUniMacros;

    function NeedRecreateProcCall: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function ParseSQL(const SQL: _string; Params: TDAParams; RenamePrefix: _string = ''): _string; override;
    procedure AssembleSQL; override;
    procedure CreateParams; override;
    procedure WriteParams(WriteValue: boolean = True); override;
    procedure ReadParams; override;
    function FindResultParam: TDAParam; override;
    procedure InternalCreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean = False); override;

    // CLR cross-assembly
    function UsedConnection: TCustomDAConnection; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BreakExec;

    function FindParam(const Value: _string): TUniParam;
    function ParamByName(const Value: _string): TUniParam;

    procedure CreateProcCall(const Name: _string);

    property LastInsertId: int64 read FLastInsertId;

  published
    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
    property Params: TUniParams read GetParams write SetParams stored False;
    property SpecificOptions: _TStrings read GetSpecificOptions write SetSpecificOptions;

    property ParamCheck; // before SQL
    property SQL;
    property Macros;
    property Debug;

    property BeforeExecute;
    property AfterExecute;
  end;

  TUniDataSetOptions = class(TDADataSetOptions)
  public
    constructor Create(Owner: TCustomDADataSet);

  published
    property FullRefresh;
    property TrimFixedChar;
    property TrimVarChar;
    property SetEmptyStrToNull;
    property PrepareUpdateSQL;
    property SetFieldsReadOnly default True;
    property RequiredFields;
    property StrictUpdate;
    property NumberRange;
    property QueryRecCount;
    property AutoPrepare;
    property ReturnParams;
    property LongStrings;
    property FlatBuffers;
    property RemoveOnRefresh;
    property QuoteNames;
    property DetailDelay;
  {$IFDEF HAVE_COMPRESS}
    property CompressBlobMode;
  {$ENDIF}
    property LocalMasterDetail;
    property CacheCalcFields;
    property FieldsOrigin;
    property DefaultValues;
    property UpdateBatchSize;
    property UpdateAllFields;
    property EnableBCD;
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}
  {$ENDIF}
  end;

  TUniDataSetStringOptions = class(TStringOptionsHolder)
  private
    FOwner: TCustomUniDataSet;
  protected
    procedure ValuesChanging(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomUniDataSet);  
  end;

  TCustomUniDataSet = class(TCustomDADataSet)
  private
    FSpecificOptions: TUniDataSetStringOptions;
    FFixedUsedTransaction: TUniTransaction;
    FCursor: TCRCursor;
    FLockFetchAll: boolean;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetOptions: TUniDataSetOptions;
    procedure SetOptions(Value: TUniDataSetOptions);
    function GetSpecificOptions: _TStrings;
    procedure SetSpecificOptions(Value: _TStrings);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);
    function GetUpdateTransaction: TUniTransaction;
    procedure SetUpdateTransaction(Value: TUniTransaction);
    function GetParams: TUniParams;
    procedure SetParams(Value: TUniParams);
    function GetUpdateObject: TUniUpdateSQL;
    procedure SetUpdateObject(Value: TUniUpdateSQL);

  protected
    procedure AssignTo(Dest: TPersistent); override;

    function GetCRCursor: TCRCursor; override;
    procedure SetCRCursor(Value: TCRCursor); override;

    function CanGetProvider: boolean;
    function GetProvider: TUniProvider;

    procedure CheckInactive; override;
    procedure SetIRecordSet(Value: TData); override;

    procedure CreateCommand; override;

    function GetDataSetServiceClass: TDataSetServiceClass; override;
    procedure SetDataSetService(Value: TDataSetService); override;

    function CreateOptions: TDADataSetOptions; override;

    procedure InternalExecute; override;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure CloseCursor; override;

    procedure SetTransaction(Value: TDATransaction); override;
    function UsedTransaction: TDATransaction; override;
    procedure BeginConnection(NoConnectCheck: boolean = True); override;
    procedure EndConnection; override;
    function UsedConnection: TCustomDAConnection; override;
  {$IFNDEF FPC}
    function GetPSTransaction: TDATransaction;
    function PSInTransaction: Boolean; override;
    procedure PSStartTransaction; override;
  {$ENDIF}

  { Field Management }
    function GetDataTypesMap: TDataTypesMapClass; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;

  { SQL Modifications }
    function SQLGetFrom(SQLText: _string): _string; override;
    function SQLAddWhere(SQLText, Condition: _string): _string; override;
    function SQLDeleteWhere(SQLText: _string): _string; override;
    function SQLGetWhere(SQLText: _string): _string; override;
    function SQLSetOrderBy(SQLText: _string; Fields: _string): _string; override;
    function SQLGetOrderBy(SQLText: _string): _string; override;

    procedure SetLockFetchAll(Value: boolean);
    function GetFetchAll: boolean; override;
    procedure SetFetchAll(Value: boolean); override;
    procedure QuickOpen(var Info: TQuickOpenInfo; Refresh: boolean = False); override;
    procedure Restore(const Info: TQuickOpenInfo; RestoreActive: boolean = True); override;

    property LockFetchAll: boolean read FLockFetchAll write SetLockFetchAll;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindParam(const Value: _string): TUniParam;
    function ParamByName(const Value: _string): TUniParam;

    procedure Prepare; override;
    procedure UnPrepare; override;
    procedure CreateProcCall(const Name: _string);
    function OpenNext: boolean;
    procedure RefreshQuick(const CheckDeleted: boolean);

    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
    property UpdateTransaction: TUniTransaction read GetUpdateTransaction write SetUpdateTransaction;
    property Params: TUniParams read GetParams write SetParams stored False;
    property Options: TUniDataSetOptions read GetOptions write SetOptions;
    property SpecificOptions: _TStrings read GetSpecificOptions write SetSpecificOptions;
    property UpdateObject: TUniUpdateSQL read GetUpdateObject write SetUpdateObject;
    property LastInsertId: int64 read FLastInsertId;

    property KeyFields;
    property LockMode;
    property DMLRefresh;
    property Cursor;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniQuery = class(TCustomUniDataSet)
  published
    property UpdatingTable;
    property KeyFields;
    property SQLInsert;
    property SQLDelete;
    property SQLUpdate;
    property SQLLock;
    property SQLRefresh;
    property LocalUpdate;

    property Connection;
    property Transaction;
    property UpdateTransaction;
    property ParamCheck; // before SQL
    property SQL;
    property MasterSource;
    property MasterFields;
    property DetailFields;
    property Debug;
    property Macros;
    property Params;
    property FetchRows;
    property ReadOnly;
    property UniDirectional;
    property CachedUpdates;
    property FilterSQL;
    property DMLRefresh;
    property LockMode;
    property RefreshOptions;
    property Options;
    property SpecificOptions;

    property BeforeExecute;
    property AfterExecute;
    property BeforeUpdateExecute;
    property AfterUpdateExecute;
    property OnUpdateError;
    property OnUpdateRecord;
    property BeforeFetch;
    property AfterFetch;

    property UpdateObject;

    property Active;
    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
  {$IFNDEF FPC}
    property ObjectView default False;
  {$ENDIF}

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AfterRefresh;
    property BeforeRefresh;

    property Fields;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniTable = class (TCustomUniDataSet)
  private
    FTableName: _string;
    FOrderFields: _string;

    procedure SetTableName(const Value: _string);
    procedure SetOrderFields(const Value: _string);

  {$IFNDEF FPC}
  protected
  { IProviderSupport }
    function PSGetTableName: string; override;
    procedure PSSetParams(AParams: DB.TParams); override;
    procedure PSSetCommandText(const CommandText: string); override;
  {$ENDIF}

  protected
    procedure SetDataSetService(Value: TDataSetService); override;
  { Open/Close }
    procedure OpenCursor(InfoQuery: boolean); override;

    procedure AssignTo(Dest: TPersistent); override;

  { SQL Modifications }
    procedure CheckSQL; override;
    procedure SetFilterSQL(const Value: _string); override;
    function GetFinalSQL: _string; override;

  public
    constructor Create(Owner: TComponent); override;

  { Open/Close }
    procedure PrepareSQL;
    procedure Prepare; override;
    procedure Execute; override;

  published
    property TableName: _string read FTableName write SetTableName;
    property OrderFields: _string read FOrderFields write SetOrderFields;

    property MasterFields;
    property DetailFields;
    property MasterSource;
    property ReadOnly;
    property KeyFields;
    property Connection;
    property Transaction;
    property UpdateTransaction;
    property FilterSQL;
    property DMLRefresh;
    property Debug;
    property Params;
    property FetchRows;
    property UniDirectional;
    property CachedUpdates;
    property LockMode default lmOptimistic;
    property RefreshOptions;

    property OnUpdateError;
    property OnUpdateRecord;

    property UpdateObject;

    property Active;
    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
  {$IFNDEF FPC}
    property ObjectView default False;
  {$ENDIF}

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AfterRefresh;
    property BeforeRefresh;

    property Fields;
    property Options;
    property SpecificOptions;
  end;


{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniStoredProc = class (TCustomUniDataSet)
  private
    FStoredProcName: _string;
    FIsQuery: boolean;

    procedure SetStoredProcName(const Value: _string);

  {$IFNDEF FPC}
  protected
  { IProviderSupport }
    procedure PSSetCommandText(const CommandText: string); override;
  {$ENDIF}

  protected
    procedure CreateCommand; override;
    procedure BeforeOpenCursor(InfoQuery: boolean); override;
    procedure DoBeforeExecute; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Prepare; override;
    procedure PrepareSQL(IsQuery: boolean = False);

    procedure ExecProc; // for BDE compatibility

  published
    property StoredProcName: _string read FStoredProcName write SetStoredProcName;

    property SQLInsert;
    property SQLDelete;
    property SQLUpdate;
    property SQLLock;
    property SQLRefresh;

    property Connection;
    property Transaction;
    property UpdateTransaction;
    property Debug;
    property Params;
    property FetchRows;
    property ReadOnly;
    property UniDirectional;
    property CachedUpdates;
    property LockMode;
    property RefreshOptions;
    property Options;
    property SpecificOptions;

    property BeforeExecute;
    property AfterExecute;
    property BeforeUpdateExecute;
    property AfterUpdateExecute;
    property OnUpdateError;
    property OnUpdateRecord;

    property UpdateObject;

    property Active;
    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
  {$IFNDEF FPC}
    property ObjectView default False;
  {$ENDIF}
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AfterRefresh;
    property BeforeRefresh;

    property Fields;
  end;

{ TUniUpdateSQL }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniUpdateSQL = class (TCustomDAUpdateSQL)
  protected
    function DataSetClass: TCustomDADataSetClass; override;
    function SQLClass: TCustomDASQLClass; override;
  end;

{ TUniDataSource }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniDataSource = class (TCRDataSource)
  end;

{ TUniMetaData }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TUniMetaData = class (TDAMetaData)
  private
    FFixedUsedTransaction: TDATransaction;
    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);

  protected
    procedure CloseCursor; override;
    function UsedTransaction: TDATransaction; override;
    procedure SetTransaction(Value: TDATransaction); override;
    procedure BeginConnection; override;
    procedure EndConnection; override;

  published
    property Active;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeScroll;
    property AfterScroll;
    property OnFilterRecord;

    property MetaDataKind;
    property Restrictions;

    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
  end;

{ TUniBlob }

  TUniBlob = class(TCompressedBlob)
  private
    FNativeBlob: TBlob;

    function GetNativeBlob: TBlob;
    procedure SetNativeBlob(Value: TBlob);
  public
    destructor Destroy; override;

    procedure Disconnect; override;
  end;

{ TUniCursor }

  TUniCursor = class(TCRCursor)
  private
    FNativeCursor: TCRCursor;

  protected

    function GetNativeCursor: TCRCursor;
    procedure SetNativeCursor(Value: TCRCursor);
  public
    destructor Destroy; override;

    procedure Disconnect; override;
    function CanFetch: boolean; override;
  end;

{ TUniParam }

  TUniParam = class(TDAParam)
  protected
    procedure CreateObject; override;
    function GetNativeParamObject: TSharedObject; override;

    function IsBlobDataType(DataType: TFieldType): boolean; override;
    function IsObjectDataType(DataType: TFieldType): boolean; overload; override;
    function IsObjectDataType: boolean; overload; // CLR cross-assembly
  public
    property AsCursor;
  published  
    property National default False;
  end;

{ TUniParams }

  TUniParams = class(TDAParams)
  private
    function GetItem(Index: integer): TUniParam;
    procedure SetItem(Index: integer; Value: TUniParam);

    function GetOwner: TPersistent; reintroduce;// CLR cross-assembly
    procedure SetOwner(Owner: TPersistent);
  protected
    procedure Update(Item: TCollectionItem); override;

    property ParamsChangeType: TParamsChangeType read FParamsChangeType write FParamsChangeType; // required for CLR
  public
    constructor Create(Owner: TPersistent);

    function FindParam(const Value: _string): TUniParam;
    function ParamByName(const Value: _string): TUniParam;

    property Items[Index: integer]: TUniParam read GetItem write SetItem; default;
  end;

  TUniUtils = class
  public
    class function CanGetProvider(Connection: TUniConnection): boolean;
    class function GetProvider(Connection: TUniConnection): TUniProvider;
    class function GetCRConnection(Connection: TUniConnection): TCRConnection;
  end;

  procedure GetServerList(ProviderName: string; List: _TStrings; SpecificOptions: _TStrings = nil);

var
  DefConnectDialogClassProc: function: TClass = nil;

var
  EnableUniSQL: boolean = True;

const
  DACProductName = 'UniDAC';



implementation

uses
  DAConsts, UniConsts, UniSQLMonitor;


procedure CheckProviderName(ProviderName: string);
var
  UniProviderDesc: TUniProviderDesc;
begin
  if (ProviderName = '') then
    DatabaseError(SProviderNotDefined);

  if (ProviderName <> '') then begin
    UniProviderDesc := UniProviders.GetProviderDesc(ProviderName);
    raise Exception.CreateFmt(SProviderNotRegistered,
      [UniProviderDesc.ProviderName,
       UniProviderDesc.ProviderUnitName,
       UniProviderDesc.ProviderComponentName]);
  end;
end;


procedure HandleInternalError({$IFDEF CLR}E: Exception{$ENDIF});
{$IFNDEF CLR}
var
  E: TObject;
{$ENDIF}
begin
{$IFNDEF CLR}
  E := AcquireExceptionObject;
{$ENDIF}

  if E is EDAError then
    raise EUniError.Create(EDAError(E))
  else
    raise E;
end;

{ EUniError }

constructor EUniError.Create(AInnerError: EDAError);
begin
{$IFDEF CLR}
  inherited Create(AInnerError.ErrorCode, AInnerError.Message);
{$ELSE}
  inherited
  Message := AInnerError.Message;
{$ENDIF}
  FErrorCode := AInnerError.ErrorCode;
  FComponent := AInnerError.Component;
  FInnerError := AInnerError;
end;

destructor EUniError.Destroy;
begin
  FInnerError.Free;
  
  inherited;
end;

function EUniError.IsFatalError: boolean;
begin
  if FInnerError <> nil then
    Result := FInnerError.IsFatalError
  else
    Result := False;
end;

function EUniError.IsKeyViolation: boolean;
begin
  if FInnerError <> nil then
    Result := FInnerError.IsKeyViolation
  else
    Result := False;
end;

{ TUniOptionsKeeper }

constructor TStringOptionsHolder.Create;
begin
  inherited;

  FValues := _TStringList.Create;
  _TStringList(FValues).OnChanging := ValuesChanging;
  _TStringList(FValues).OnChange := ValuesChanged;
end;

destructor TStringOptionsHolder.Destroy;
begin
  FValues.Free;

  inherited;
end;

procedure TStringOptionsHolder.ValuesChanging(Sender: TObject);
begin

end;

procedure TStringOptionsHolder.ValuesChanged(Sender: TObject);
begin
  FIsModified := True;
end;

{ TUniConnectionOptions }

function TUniConnectionOptions.GetConvertEOL: boolean;
begin
  Result := FOwner.ConvertEOL;
end;

procedure TUniConnectionOptions.SetConvertEOL(Value: boolean);
begin
  FOwner.ConvertEOL := Value;
end;

{ TUniConnectionStringOptions }

constructor TUniConnectionStringOptions.Create(AOwner: TUniConnection);
begin
  inherited Create;

  FOwner := AOwner;
end;

procedure TUniConnectionStringOptions.ValuesChanging(Sender: TObject);
begin
  Assert(FOwner <> nil);
  if FOwner.Connected then
    FOwner.Disconnect;
end;

{ TUniMacro }

procedure TUniMacro.AssignTo(Dest: TPersistent);
begin
  if Dest is TUniMacro then begin
    TUniMacro(Dest).Name := Name;
    TUniMacro(Dest).Value := Value;
    TUniMacro(Dest).Condition := Condition;
  end
  else
    inherited;
end;

procedure TUniMacro.SetName(const Value: _string);
begin
  if Value <> FName then begin
    FName := Value;
    TUniMacros(Collection).NotifyOwner;
  end;
end;

procedure TUniMacro.SetValue(const Value: _string);
begin
  if Value <> FValue then begin
    FValue := Value;
    TUniMacros(Collection).NotifyOwner;
  end;
end;

procedure TUniMacro.SetCondition(const Value: _string);
begin
  if Value <> FCondition then begin
    FCondition := Value;
    TUniMacros(Collection).NotifyOwner;
  end;
end;

{ TUniMacros }

constructor TUniMacros.Create(Owner: TPersistent);
begin
  inherited Create(Owner, TUniMacro);
end;

procedure TUniMacros.Add(const Name, Value: _string; const Condition: _string = '');
var
  Def: TUniMacro;
begin
  Def := TUniMacro(inherited Add);
  Def.Name := Name;
  Def.Value := Value;
  Def.Condition := Condition;
end;

function TUniMacros.FindMacro(const Name: _string): TUniMacro;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if _SameText(Result.Name, Name) then
      Exit;
  end;
  Result := nil;
end;

function TUniMacros.MacroByName(const Name: _string): TUniMacro;
begin
  Result := FindMacro(Name);

  if Result = nil then begin
    Result := TUniMacro(inherited Add);
    Result.Name := Name;
  end;
end;

function TUniMacros.GetItem(Index: integer): TUniMacro;
begin
  Result := TUniMacro(inherited Items[Index]);
end;

procedure TUniMacros.SetItem(Index: integer; Value: TUniMacro);
begin
  inherited SetItem(Index, Value);
end;

procedure TUniMacros.NotifyOwner;
begin
  if (UpdateCount = 0) and (Owner is TUniConnection) then begin
    TUniConnection(Owner).FMacrosChanged := True;
    Inc(TUniConnection(Owner).FMacrosVersion);
  end;
end;

procedure TUniMacros.Update(Item: TCollectionItem);
begin
  inherited;

  NotifyOwner;
end;

{ TUniConnection }

constructor TUniConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSpecificOptions := TUniConnectionStringOptions.Create(Self);

  FInternalTransaction := CreateTransaction;
  TUniTransaction(FInternalTransaction).DefaultConnection := Self;
  TUniTransaction(FInternalTransaction).IsolationLevel := ilReadCommitted;

  FMacros := TUniMacros.Create(Self);
  FMacroNames := _TStringList.Create;
  FMacroValues := _TStringList.Create;
end;

destructor TUniConnection.Destroy;
begin
  FSpecificOptions.Free;
  FInternalTransaction.Free;
  FMacros.Free;
  FMacroNames.Free;
  FMacroValues.Free;
  FSQLFormatter.Free;

  inherited;
end;

procedure TUniConnection.CheckProvider;
begin
  if FProvider = nil then
    CheckProviderName(ProviderName);
end;

function TUniConnection.CanGetProvider: boolean;
begin
  Result := FProvider <> nil;
end;

function TUniConnection.GetProvider: TUniProvider;
begin
  CheckProvider;
  Result := FProvider;
end;

function TUniConnection.GetIConnectionClass: TCRConnectionClass;
begin
  Result := FProvider.GetConnectionClass;
end;

function TUniConnection.GetICommandClass: TCRCommandClass;
begin
  CheckProvider;

  Result := FProvider.GetCommandClass;
end;

function TUniConnection.GetIRecordSetClass: TCRRecordSetClass;
begin
  CheckProvider;

  Result := FProvider.GetRecordSetClass;
end;

function TUniConnection.GetIMetaDataClass: TCRMetaDataClass;
begin
  CheckProvider;

  Result := FProvider.GetMetaDataClass;
end;

procedure TUniConnection.CreateIConnection;
var
  Connection: TCRConnection;
  ConnectionParameters: TCRConnectionParameters;

  procedure SetSpecificObjectProps(SetAllProps: boolean);
  begin
    FProvider.SetObjectProps(Connection, FSpecificOptions.Values, SetAllProps);
    FSpecificOptions.IsModified := False;
  end;

begin
  CheckProvider;

  Connection := FIConnection;

  if Connection = nil then begin
    if Pooling and FProvider.IsPoolingSupported then begin
      ConnectionParameters := FProvider.GetConnectionParametersClass.Create;
      try
        ConnectionParameters.MinPoolSize := PoolingOptions.MinPoolSize;
        ConnectionParameters.MaxPoolSize := PoolingOptions.MaxPoolSize;
        ConnectionParameters.ConnectionLifeTime := PoolingOptions.ConnectionLifetime;
        ConnectionParameters.Validate := PoolingOptions.Validate;
        ConnectionParameters.Username := Username;
        ConnectionParameters.Password := Password;
        ConnectionParameters.Server := Server;
        ConnectionParameters.IOHandler := FIOHandler;
        ConnectionParameters.OnError := DoError;
        if FProvider.IsDatabaseSupported then //upd1
          ConnectionParameters.SetProp(prDatabase, FDatabase);
        if FProvider.IsPortSupported then
          ConnectionParameters.SetProp(prPort, Port);

        FProvider.SetObjectProps(ConnectionParameters, SpecificOptions, True);

        Connection := FProvider.GetConnectionPoolingManagerClass.GetConnection(
          ConnectionParameters, TUniSQLMonitor);
      finally
        ConnectionParameters.Free;
      end;
    end
    else begin
      Connection := GetIConnectionClass.Create;
      Connection.IOHandler := FIOHandler;
      if FProvider.IsDatabaseSupported then
        Connection.SetProp(prDatabase, FDatabase);
      if FProvider.IsPortSupported then
        Connection.SetProp(prPort, Port);
    end;

    Connection.SetProp(prDisconnectedMode, Options.DisconnectedMode);
    Connection.SetProp(prEnableBCD, Options.EnableBCD);
  {$IFDEF VER6P}
  {$IFNDEF FPC}
    Connection.SetProp(prEnableFMTBCD, Options.EnableFMTBCD);
  {$ENDIF}
  {$ENDIF}
    // if connection is just created we need to set all options
    SetSpecificObjectProps(True);

    SetIConnection(Connection);
  end;

  if FSpecificOptions.IsModified then
    SetSpecificObjectProps(False);
end;

procedure TUniConnection.SetIConnection(Value: TCRConnection);
begin
  inherited;

  FIConnection := Value;
end;

function TUniConnection.CreateDataSet: TCustomDADataSet;
begin
  Result := TCustomUniDataSet.Create(nil);  
  Result.Connection := Self;
end;

function TUniConnection.CreateSQL: TCustomDASQL;
begin
  Result := TUniSQL.Create(nil);
  Result.Connection := Self;
end;

function TUniConnection.CreateTransaction: TDATransaction;
begin
  Result := TUniTransaction.Create(nil);
  Result.DefaultConnection := Self;
end;

function TUniConnection.CreateMetaData: TDAMetaData;
begin
  Result := TUniMetaData.Create(nil);
  Result.Connection := Self;
end;

procedure TUniConnection.AssignConnect(Source: TUniConnection);
begin
  inherited AssignConnect(Source);
end;

procedure TUniConnection.DoConnect;
var
  Database: Variant;
begin
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;

  if FProvider.IsDatabaseSupported and (FDatabase = '') then begin
    FIConnection.GetProp(prDatabase, Database);
    FDatabase := _string(Database);
  end;
end;

procedure TUniConnection.DoDisconnect;
begin
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;
end;

function TUniConnection.SQLMonitorClass: TClass;
begin
  Result := TUniSQLMonitor;
end;

function TUniConnection.ConnectDialogClass: TConnectDialogClass;
begin
  if Assigned(DefConnectDialogClassProc) then
    Result := TConnectDialogClass(DefConnectDialogClassProc)
  else
    Result := nil;
end;

function TUniConnection.CreateOptions: TDAConnectionOptions;
begin
  Result := TUniConnectionOptions.Create(Self);
end;

function TUniConnection.IsMultipleTransactionsSupported: boolean;
begin
  Result := GetProvider.IsDataSetNeedTransaction;
end;

function TUniConnection.GetProviderName: string;
begin
  Result := FProviderName;
end;

procedure TUniConnection.SetProviderName(Value: string);
begin
  if Connected then
    DatabaseError('This operation is not allowed on active connection');

  if AnsiCompareText(Trim(Value), FProviderName) <> 0 then begin
    if Trim(Value) = '' then
      FProvider := nil
    else
      FProvider := UniProviders.GetProvider(Value);

    FreeIConnection; // TODO:
  end;

  FProviderName := Trim(Value);
  FMacrosChanged := True;
end;

procedure TUniConnection.SetDatabase(const Value: _string);
begin
  if FDatabase <> Trim(Value) then begin
    if Connected and not FIConnection.CanChangeDatabase then
      Disconnect;
    FDatabase := Trim(Value);
    if FIConnection <> nil then
      FIConnection.SetProp(prDatabase, Value);
  end;
end;

procedure TUniConnection.SetPort(Value: integer);
begin
  if FPort <> Value then begin
    Disconnect;
    FPort := Value;
    if FIConnection <> nil then
      FIConnection.SetProp(prPort, Value);
  end;
end;

function TUniConnection.GetSpecificOptions: _TStrings;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniConnection.SetSpecificOptions(Value: _TStrings);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TUniConnection.GetTransaction(Index: Integer): TUniTransaction;
begin
  Result := TUniTransaction(inherited Transactions[Index]);
end;

function TUniConnection.GetOptions: TUniConnectionOptions;
begin
  Result := TUniConnectionOptions(inherited Options);
end;

procedure TUniConnection.SetOptions(Value: TUniConnectionOptions);
begin
  inherited Options := Value;
end;

procedure TUniConnection.SetMacros(Value: TUniMacros);
begin
  FMacros.Assign(Value);
end;

function TUniConnection.IsMacrosStored: boolean;
begin
  Result := FMacros.Count > 0;
end;

function TUniConnection.ParamByName(const Name: _string): TUniParam;
begin
  Result := TUniParam(inherited ParamByName(Name));
end;

function TUniConnection.MacroByName(const Name: _string): TUniMacro;
begin
  Result := FMacros.MacroByName(Name);
end;

function TUniConnection.ActiveMacroValueByName(const Name: _string): Variant;
var
  Index: Integer;
begin
  Result := null;

  if CanGetProvider then begin
    CheckSqlFormatter;
    if FMacrosChanged then
      DetectActiveMacros;

    Index := FMacroNames.IndexOf(Name);
    if Index >= 0 then
      Result := FMacroValues[Index];
  end;
end;

function TUniConnection.UsedTransaction: TDATransaction;
begin
  if IsMultipleTransactionsSupported and not DefaultTransaction.Active then
    Result := FInternalTransaction
  else
    Result := inherited UsedTransaction;
end;

function TUniConnection.GetInTransaction: boolean;
begin
  Result := DefaultTransaction.Active;
end;

procedure TUniConnection.StartTransaction;
begin
  StartTransaction(ilReadCommitted);
end;

procedure TUniConnection.StartTransaction(IsolationLevel: TCRIsolationLevel; ReadOnly: boolean = False);
begin
  if not DefaultTransaction.Active then begin
    TUniTransaction(DefaultTransaction).IsolationLevel := IsolationLevel;
    TUniTransaction(DefaultTransaction).ReadOnly := ReadOnly;
  end;

  inherited StartTransaction;
end;

procedure TUniConnection.CommitRetaining;
begin
  DoCommitRetaining;
end;

procedure TUniConnection.RollbackRetaining;
begin
  DoRollbackRetaining;
end;

procedure TUniConnection.Savepoint(const Name: _string);
begin
  DoSavepoint(Name);
end;

procedure TUniConnection.ReleaseSavepoint(const Name: _string);
begin
  DoReleaseSavepoint(Name);
end;

procedure TUniConnection.RollbackToSavepoint(const Name: _string);
begin
  DoRollbackToSavepoint(Name);
end;

function TUniConnection.GetServerVersion: _string;
begin
  if not Connected then
    raise Exception.Create(SConnectionNotConnected);

  Result := FIConnection.GetServerVersion;
end;

function TUniConnection.GetServerVersionFull: _string;
begin
  if not Connected then
    raise Exception.Create(SConnectionNotConnected);

  Result := FIConnection.GetServerVersionFull;
end;

function TUniConnection.GetClientVersion: _string;
begin
  if not Connected then
    raise Exception.Create(SConnectionNotConnected);

  Result := FIConnection.GetClientVersion;
end;

function TUniConnection.DefaultTableSchema: _string;
var
  Provider: TUniProvider;
begin
  if ProviderName = '' then
    DatabaseError(SProviderNotDefined);

  Provider := UniProviders.GetProvider(ProviderName);

  if Provider = nil then
    CheckProviderName(ProviderName);
    
  Result := Provider.DefaultTableSchema;
end;

procedure TUniConnection.DoError(E: Exception; var Fail, Reconnect, Reexecute: boolean;
  ReconnectAttempt: integer; var ConnLostCause: TConnLostCause);
var
  UniErr: EUniError;
begin
  if E is EDAError then begin
    UniErr := EUniError.Create(EDAError(E));
    try
      inherited DoError(UniErr, Fail, Reconnect, Reexecute, ReconnectAttempt, ConnLostCause);
    finally
      UniErr.FInnerError := nil;
      UniErr.Free;
    end;
  end
  else
    inherited DoError(E, Fail, Reconnect, Reexecute, ReconnectAttempt, ConnLostCause);
end;

procedure TUniConnection.AssignConnectOptions(Source: TCustomDAConnection);
var
  i: integer;
begin
  inherited;

  ProviderName := TUniConnection(Source).ProviderName;
  Database := TUniConnection(Source).Database;
  Port := TUniConnection(Source).Port;
  // TStringList.Assign does not work across modules
  SpecificOptions.Clear;
  for i := 0 to TUniConnection(Source).SpecificOptions.Count - 1 do
    SpecificOptions.Add(TUniConnection(Source).SpecificOptions[i]);
end;

procedure TUniConnection.CheckSqlFormatter;
var
  Cls: TUniSqlFormatterClass;
begin
  Cls := FProvider.GetSqlFormatterClass;
  if not (FSQLFormatter is Cls) then begin
    FSQLFormatter.Free;
    FSQLFormatter := Cls.Create;
    FSQLFormatter.SetUserMacros(FMacroNames, FMacroValues);
    FSQLFormatter.SetParserClass(FProvider.GetParserClass);
  end;
end;

procedure TUniConnection.DetectActiveMacros;

  function GetActive(Depth: integer; const MacroName: _string): boolean;
  var
    i: integer;
    Macro: TUniMacro;
    Name, Cond: _string;
  begin
    if Depth > Macros.Count then
      raise Exception.Create(SCyclicConditions);

    Result := FSQLFormatter.CheckIfCondition(MacroName);
    if not Result then begin
      for i := 0 to Macros.Count - 1 do begin
        Macro := Macros[i];
        Name := _UpperCase(Macro.Name);
        if (Name = MacroName) then begin
          Cond := Trim(_UpperCase(Macro.Condition));
          if (Cond = '') or GetActive(Depth + 1, Cond) then begin
            Result := True;
            break;
          end;
        end;
      end;
    end;
  end;

var
  i, j, k: integer;
  Macro: TUniMacro;
  UncondMacros: _TStringList;
  Name, Cond: _string;
begin
  FMacroNames.Clear;
  FMacroValues.Clear;
  UncondMacros := _TStringList.Create;
  try
    for i := 0 to Macros.Count - 1 do begin
      Macro := Macros[i];
      Cond := Trim(_UpperCase(Macro.Condition));

      if (Cond = '') or GetActive(1, Cond) then begin
        Name := _UpperCase(Macro.Name);
        j := FMacroNames.IndexOf(Name);

        if j >= 0 then begin
          k := UncondMacros.IndexOf(Name);
          if (Cond <> '') and (k >= 0) then begin
            // replace value with conditional macro
            FMacroValues[j] := Macro.Value;
            UncondMacros.Delete(k);
          end
          else
            // ignore value
        end
        else begin
          FMacroNames.Add(Name);
          FMacroValues.Add(Macro.Value);
        end;

        if Cond = '' then
          UncondMacros.Add(Name);
      end;
    end;
  finally
    UncondMacros.Free;
  end;
  FMacrosChanged := False;
end;

procedure TUniConnection.ExpandMacros(var SQL: _string);
begin
  if CanGetProvider then begin
    CheckSqlFormatter;
    if FMacrosChanged then
      DetectActiveMacros;
    FSQLFormatter.Expand(SQL);
  end;
end;

{ TUniSQL }

constructor TUniSQL.Create(AOwner: TComponent);
begin
  inherited;

  FSpecificOptions := TStringOptionsHolder.Create;

  TUniParams(Params).SetOwner(Self);
  FParamRefs := TList.Create;
  FEnableUniSQL := EnableUniSQL;

  AutoCommit := True;
end;

destructor TUniSQL.Destroy;
begin
  FSpecificOptions.Free;
  FParamRefs.Free;

  inherited;
end;

procedure TUniSQL.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniSQL then begin
    TUniSQL(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

procedure TUniSQL.SetICommand(Value: TCRCommand);
begin
  inherited;

  if FICommand <> nil then
    GetProvider.SetObjectProps(FICommand, FSpecificOptions.Values, True)
end;

function TUniSQL.GetDataTypesMap: TDataTypesMapClass;
begin
  if CanGetProvider then
    Result := GetProvider.GetDataTypesMap
  else
    Result := inherited GetDataTypesMap
end;

function TUniSQL.CreateParamsObject: TDAParams;
begin
  // Note: Owner should be <> nil only for TUniSQL.Params, see ResetParamRefs
  Result := TUniParams.Create(nil);
end;

function TUniSQL.UsedTransaction: TDATransaction;
begin
  if FFixedUsedTransaction <> nil then
    Result := FFixedUsedTransaction
  else
    Result := inherited UsedTransaction;
end;

procedure TUniSQL.BeginConnection(NoConnectCheck: boolean = True);
begin
  inherited;

  if FSpecificOptions.IsModified then begin
    GetProvider.SetObjectProps(FICommand, FSpecificOptions.Values, False);
    FSpecificOptions.IsModified := False;
  end;

  if FFixedUsedTransaction = nil then
    FFixedUsedTransaction := UsedTransaction;
end;

procedure TUniSQL.EndConnection;
begin
  inherited;

  if not Prepared then
    FFixedUsedTransaction := nil;
end;

procedure TUniSQL.InternalPrepare;
begin
  CheckUniMacros;
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;
end;

procedure TUniSQL.InternalUnPrepare;
begin
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;
end;

procedure TUniSQL.InternalExecute(Iters: integer);
begin
  CheckUniMacros;
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;
end;

procedure TUniSQL.BreakExec;
begin
  if FICommand <> nil then
    FICommand.BreakExec;
end;

function TUniSQL.GetFinalSQL: _string;
var
  UsedCon: TUniConnection;
begin
  Result := inherited GetFinalSQL;

  if FEnableUniSQL then begin
    UsedCon := TUniConnection(UsedConnection);
    if UsedCon <> nil then
      UsedCon.ExpandMacros(Result);
  end;
end;

procedure TUniSQL.CheckUniMacros;
var
  UsedCon: TUniConnection;
begin
  if EnableUniSQL then begin
    UsedCon := TUniConnection(UsedConnection);
    if (UsedCon <> nil) and (FMacrosVersion < UsedCon.FMacrosVersion) then begin
      if FICommand <> nil then
        SetICommandSQL;
      FMacrosVersion := UsedCon.FMacrosVersion;
    end;
  end;
end;

function TUniSQL.IsInOutParamSupported: boolean;
begin
  Result := GetProvider.IsInOutParamSupported;
end;

function TUniSQL.NeedRecreateProcCall: boolean;
begin
  Result := GetProvider.NeedRecreateProcCall;
end;

function TUniSQL.ParseSQL(const SQL: _string; Params: TDAParams; RenamePrefix: _string = ''): _string;
var
  ParsedSQL: _StringBuilder;
  Parser: TSQLParser;
  StartPos: integer;

  LeftQuote, RightQuote: _char;
  AllParams: _TStringList;

  procedure ParseSQLParam;
  var
    Code: integer;
    St: _string;
    DogPresent: boolean;
    l: integer;
    ParamName: _string;
  begin
    Code := Parser.GetNext(St);
    DogPresent := St = '@';
    if DogPresent then
      Code := Parser.GetNext(St); // Omit '@' in ParamName for BDE compatibility

    if ((Params <> nil) or (RenamePrefix <> '')) and ((Code = lcIdent) or (Code = lcNumber) or (Parser.KeywordLexems.IndexOf(St) <> -1)) // and (St <> '=')
    then begin
      if DogPresent then
        ParamName := '@' + St
      else
        ParamName := St;

      l := Length(ParamName);
      // remove quotes
      if (ParamName[1] = LeftQuote) and (ParamName[l] = RightQuote) then
        ParamName := Copy(ParamName, 2, l - 2);

      if Params <> nil then begin
        if AllParams.IndexOf(ParamName) < 0 then begin
          TDAParam(Params.Add).Name := ParamName;
          AllParams.Add(ParamName);
        end;
        ParsedSQL.Append('?');
      end
      else
        ParsedSQL.Append(RenamePrefix + ParamName);
    end
    else // Labels in SQL Server, MySQL syntax and PL SQL Blocks (a := b).
    begin
      ParsedSQL.Append(':');
      if DogPresent then
        ParsedSQL.Append('@');
      ParsedSQL.Append(St);
    end;
  end;

begin
  LeftQuote := '"';
  RightQuote := '"';

  ParsedSQL := _StringBuilder.Create(Length(SQL) + Length(SQL) div 2);
  AllParams := _TStringList.Create;
  AllParams.Sorted := True;
  try
    Parser := TSQLParser.Create(SQL);
    try
      if Params <> nil then begin
        Params.BeginUpdate;
        Params.Clear;
      end;
      Parser.OmitBlank := False;
      Parser.OmitComment := True;
      Parser.QuotedString := True;
      Parser.ToBegin;
      StartPos := Parser.CurrPos;
      while Parser.ToLexem(':') do begin
        ParsedSQL.Append(Copy(SQL, StartPos + 1, Parser.CurrPos - StartPos - 1));
        ParseSQLParam;

        StartPos := Parser.CurrPos;
      end;
      ParsedSQL.Append(Copy(SQL, StartPos + 1, Parser.CurrPos - StartPos));
    finally
      Parser.Free;
      if Params <> nil then
        Params.EndUpdate;
    end;
    Result := ParsedSQL.ToString;
  finally
    ParsedSQL.Free;
    AllParams.Free;
  end;
end;

procedure TUniSQL.AssembleSQL;
var
  List: TDAParams;
  NativeSQL: _string;
begin
  FWriteAllParams := FLockScanParams;

  if CanGetProvider then begin
    inherited;
  end
  else begin
    if FDataSet = nil then
      NativeSQL := FinalSQL
    else
      NativeSQL := FDataSet.FinalSQL;

    List := CreateParamsObject;
    try
      List.BeginUpdate;
      try
        ParseSQL(NativeSQL, List);
        List.AssignValues(Params);
      finally
        List.EndUpdate;
      end;

      Params.Clear;
      Params.Assign(List);
    finally
      List.Free;
    end;
  end;
end;

procedure TUniSQL.CreateParams;
var
  ParamDesc: TParamDesc;
  Param: TDAParam;
  i, j: integer;
  AllParams: _TStringList;
begin
  AllParams := _TStringList.Create;
  AllParams.Sorted := True;
  FLockParamRefsReset := True;
  Params.BeginUpdate;
  try
    Params.Clear;
    FParamRefs.Clear;
    for i := 0 to FICommand.GetParamCount - 1 do begin
      ParamDesc := FICommand.GetParam(i);
      j := AllParams.IndexOf(ParamDesc.GetName);
      if j = -1 then begin
        Param := Params.Add as TDAParam;
        AssignParamDesc(Param, ParamDesc);
        AllParams.AddObject(ParamDesc.GetName, Param);
      end
      else
        Param := TDAParam(AllParams.Objects[j]);

      FParamRefs.Add(Param);
    end;
  finally
    Params.EndUpdate;
    FLockParamRefsReset := False;
    AllParams.Free;
  end;
end;

procedure TUniSQL.WriteParams(WriteValue: boolean = True);
var
  Param: TDAParam;
  ParamDesc: CRAccess.TParamDesc;
  i: integer;
  UseParamRefs: boolean;
begin
  if (ParamCheck and not FWriteAllParams) or (Params.ParamsChangeType = ctUsers) then begin
    UseParamRefs := FParamRefs.Count > 0;
    for i := 0 to FICommand.GetParamCount - 1 do begin
      ParamDesc := FICommand.GetParam(i);
      if UseParamRefs then
        Param := TDAParam(FParamRefs[i])
      else begin
        Param := FindParam(ParamDesc.GetName);
        FParamRefs.Add(Param);
      end;

      if Param <> nil then begin
        AssignParam(ParamDesc, Param);

        if WriteValue then begin
        {$IFDEF PERF_COUNTER}
          PerfCounters[5].Start;
        {$ENDIF}
          AssignParamValue(ParamDesc, Param);
        {$IFDEF PERF_COUNTER}
          PerfCounters[5].Stop;
        {$ENDIF}
        end;
      end;
    end;

    if Params.ParamsChangeType = ctUsers then begin
      if FICommand.GetParamCount <> Params.Count then // for ReadParams
        FParamRefs.Clear
      else
        for i := 0 to FICommand.GetParamCount - 1 do begin
          if FICommand.GetParam(i).GetName <> Params[i].Name then begin
            FParamRefs.Clear;
            break;
          end;
        end;

      inherited;
    end
    else
      Params.ParamsChangeType := ctGenerated;
  end
  else
    inherited;
end;

procedure TUniSQL.ReadParams;
var
  i: integer;
  Param: TDAParam;
  ParamDesc: TParamDesc;
  CanReadParams: Variant;
begin
  if ParamCheck and not FWriteAllParams then begin  //upd1 merge with dbaccess
    Assert(FICommand <> nil);
    FICommand.GetProp(prCanReadParams, CanReadParams);
    if CanReadParams then begin
      for i := 0 to FICommand.GetParamCount - 1 do begin
        ParamDesc := FICommand.GetParam(i);
        if FParamRefs.Count > 0 then
          Param := TDAParam(FParamRefs[i])
        else
          Param := Params.FindParam(ParamDesc.GetName);

        if (Param <> nil) and (Param.ParamType <> ptInput) and
          (IsInOutParamSupported or (Param.ParamType <> ptUnknown)) // if in/out not supported treat Unknown as Input
        then
          AssignParamDescValue(Param, ParamDesc);
      end;

      FICommand.SetProp(prCanReadParams, False); // For SDAC
    end;
  end
  else
    inherited;
end;

procedure TUniSQL.ResetParamRefs;
begin
  if not (csDestroying in ComponentState) and not FLockParamRefsReset then
    FParamRefs.Clear;
end;

function TUniSQL.FindResultParam: TDAParam;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Params.Count - 1 do
    if Params[i].ParamType = ptResult then
      Result := Params[i];

  if Result <> nil then
    if Result.DataType = ftUnknown then
      Result.DataType := ftInteger;    
end;

function TUniSQL.FindParam(const Value: _string): TUniParam;
begin
  Result := Params.FindParam(Value);
end;

function TUniSQL.ParamByName(const Value: _string): TUniParam;
begin
  Result := Params.ParamByName(Value);
end;

procedure TUniSQL.CreateProcCall(const Name: _string);
begin
  InternalCreateProcCall(Name, True);
end;

function TUniSQL.GetParams: TUniParams;
begin
  Result := TUniParams(inherited Params);
end;

procedure TUniSQL.SetParams(Value: TUniParams);
begin
  inherited Params := Value;
end;

function TUniSQL.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniSQL.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniSQL.GetSpecificOptions: _TStrings;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniSQL.SetSpecificOptions(Value: _TStrings);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TUniSQL.GetUniTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TUniSQL.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

function TUniSQL.CanGetProvider: boolean;
begin
  Result := (UsedConnection <> nil) and (TUniConnection(UsedConnection).CanGetProvider);
end;

function TUniSQL.GetProvider: TUniProvider;
begin
  if UsedConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniConnection(UsedConnection).GetProvider;
end;

procedure TUniSQL.InternalCreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean = False);
begin
  try
    if not NeedDescribe then
      FWriteAllParams := True;

    inherited;
    FEnableUniSQL := False;
  finally
    FWriteAllParams := False;
  end;
end;

function TUniSQL.UsedConnection: TCustomDAConnection;
begin
  Result := inherited UsedConnection;
end;

{ TUniDataSetOptions }

constructor TUniDataSetOptions.Create(Owner: TCustomDADataSet);
begin
  inherited;

  SetFieldsReadOnly := True;
end;

{ TUniDataSetStringOptions }

constructor TUniDataSetStringOptions.Create(AOwner: TCustomUniDataSet);
begin
  inherited Create;

  FOwner := AOwner;
end;

procedure TUniDataSetStringOptions.ValuesChanging(Sender: TObject);
begin
  Assert(FOwner <> nil);
  FOwner.CheckInactive;
end;

{ TCustomUniDataSet }

constructor TCustomUniDataSet.Create(AOwner: TComponent);
begin
  inherited;

  FSpecificOptions := TUniDataSetStringOptions.Create(Self);
end;

destructor TCustomUniDataSet.Destroy;
begin
  FSpecificOptions.Free;
  FCursor.Free;

  inherited;
end;

procedure TCustomUniDataSet.CreateProcCall(const Name: _string);
begin
  InternalCreateProcCall(Name, True);
end;

function TCustomUniDataSet.OpenNext: boolean;
begin
  Result := DoOpenNext;
end;

procedure TCustomUniDataSet.RefreshQuick(const CheckDeleted: boolean);
begin
  InternalRefreshQuick(CheckDeleted);
end;

function TCustomUniDataSet.FindParam(const Value: _string): TUniParam;
begin
  Result := Params.FindParam(Value);
end;

function TCustomUniDataSet.ParamByName(const Value: _string): TUniParam;
begin
  Result := Params.ParamByName(Value);
end;

function TCustomUniDataSet.CanGetProvider: boolean;
begin
  Result := (UsedConnection <> nil) and (TUniConnection(UsedConnection).CanGetProvider);
end;

function TCustomUniDataSet.GetProvider: TUniProvider;
begin
  if UsedConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniConnection(UsedConnection).GetProvider;
end;

procedure TCustomUniDataSet.CheckInactive;
begin
  inherited CheckInactive;
end;

procedure TCustomUniDataSet.SetIRecordSet(Value: TData);
begin
  inherited;

  if FICommand <> nil then
    GetProvider.SetObjectProps(FICommand, FSpecificOptions.Values, True);

  if FIRecordSet <> nil then begin
    FIRecordSet.SetProp(prLockFetchAll, FLockFetchAll);
    GetProvider.SetObjectProps(FIRecordSet, FSpecificOptions.Values, True);
  end;
end;

procedure TCustomUniDataSet.CreateCommand;
begin
  SetCommand(TUniSQL.Create(Self));
end;

function TCustomUniDataSet.GetDataSetServiceClass: TDataSetServiceClass;
begin
  Result := GetProvider.GetDataSetServiceClass;
end;

procedure TCustomUniDataSet.SetDataSetService(Value: TDataSetService);
begin
  inherited;

  if FDataSetService <> nil then
    GetProvider.SetObjectProps(FDataSetService, FSpecificOptions.Values, True);
end;

function TCustomUniDataSet.CreateOptions: TDADataSetOptions;
begin
  Result := TUniDataSetOptions.Create(Self);
end;

procedure TCustomUniDataSet.SetTransaction(Value: TDATransaction);
begin
  if (FFixedUsedTransaction <> nil) and (Value <> FFixedUsedTransaction) and
    (UsedConnection <> nil) and TUniConnection(UsedConnection).IsMultipleTransactionsSupported
  then begin
    Disconnect;

    if not Prepared and ((FIRecordSet = nil) or not FIRecordSet.Active) then
      FFixedUsedTransaction := nil;
  end;

  inherited;
end;

function TCustomUniDataSet.UsedTransaction: TDATransaction;
begin
  if FFixedUsedTransaction <> nil then
    Result := FFixedUsedTransaction
  else
    Result := inherited UsedTransaction;
end;

procedure TCustomUniDataSet.BeginConnection(NoConnectCheck: boolean = True);
begin
  inherited;

  if FSpecificOptions.IsModified then begin
    GetProvider.SetObjectProps(FIRecordSet, FSpecificOptions.Values, False);
    GetProvider.SetObjectProps(FDataSetService, FSpecificOptions.Values, False);
    FSpecificOptions.IsModified := False;
  end;

  if FFixedUsedTransaction = nil then
    FFixedUsedTransaction := TUniTransaction(UsedTransaction);
end;

procedure TCustomUniDataSet.EndConnection;
begin
  inherited;

  if not Prepared and ((FIRecordSet = nil) or not FIRecordSet.Active) then
    FFixedUsedTransaction := nil;
end;

function TCustomUniDataSet.UsedConnection: TCustomDAConnection;
begin
  Result := inherited UsedConnection;
end;

{$IFNDEF FPC}
function TCustomUniDataSet.GetPSTransaction: TDATransaction;
var
  Con: TUniConnection;
begin
  Result := UsedUpdateTransaction;
  Con := TUniConnection(UsedConnection);
  if Result = Con.FInternalTransaction then
    Result := Con.DefaultTransaction;
end;

function TCustomUniDataSet.PSInTransaction: Boolean;
begin
  Result := GetPSTransaction.Active;
end;

procedure TCustomUniDataSet.PSStartTransaction;
begin
  GetPSTransaction.StartTransaction;
end;
{$ENDIF}

procedure TCustomUniDataSet.Prepare;
begin
  TUniSQL(FCommand).CheckUniMacros;
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;
end;

procedure TCustomUniDataSet.UnPrepare;
begin
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;
end;

procedure TCustomUniDataSet.InternalExecute;
begin
  TUniSQL(FCommand).CheckUniMacros;
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;
end;

procedure TCustomUniDataSet.OpenCursor(InfoQuery: boolean);
begin
  TUniSQL(FCommand).CheckUniMacros;
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;
end;

procedure TCustomUniDataSet.CloseCursor;
begin
  try
    inherited;
  except
  {$IFDEF CLR}
    on E: Exception do
      HandleInternalError(E);
  {$ELSE}
    HandleInternalError;
  {$ENDIF}
  end;

  if not Prepared then
    FFixedUsedTransaction := nil;
end;

function TCustomUniDataSet.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TCustomUniDataSet.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TCustomUniDataSet.GetOptions: TUniDataSetOptions;
begin
  Result := TUniDataSetOptions(inherited Options);
end;

function TCustomUniDataSet.GetSpecificOptions: _TStrings;
begin
  Result := FSpecificOptions.Values;
end;

procedure TCustomUniDataSet.SetOptions(Value: TUniDataSetOptions);
begin
  Options.Assign(Value);
end;

procedure TCustomUniDataSet.SetSpecificOptions(Value: _TStrings);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TCustomUniDataSet.GetUniTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TCustomUniDataSet.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

function TCustomUniDataSet.GetUpdateTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited UpdateTransaction);
end;

procedure TCustomUniDataSet.SetUpdateTransaction(Value: TUniTransaction);
begin
  inherited UpdateTransaction := Value;
end;

function TCustomUniDataSet.GetParams: TUniParams;
begin
  Result := TUniParams(inherited Params);
end;

procedure TCustomUniDataSet.SetParams(Value: TUniParams);
begin
  inherited Params := Value;
end;

function TCustomUniDataSet.GetUpdateObject: TUniUpdateSQL;
begin
  Result := TUniUpdateSQL(inherited UpdateObject);
end;

procedure TCustomUniDataSet.SetUpdateObject(Value: TUniUpdateSQL);
begin
  inherited UpdateObject := Value;
end;

function TCustomUniDataSet.GetCRCursor: TCRCursor;
begin
  if FCursor <> nil then
    Result := FCursor
  else
    Result := inherited GetCRCursor;
end;

procedure TCustomUniDataSet.SetCRCursor(Value: TCRCursor);
begin
  if Value <> FCursor then begin
    if FCursor <> nil then
      FCursor.Free;
    FCursor := Value;
    if FCursor <> nil then
      FCursor.AddRef;

    if Value is TUniCursor then
      inherited SetCRCursor(TUniCursor(Value).GetNativeCursor)
    else
      inherited SetCRCursor(Value);
  end;
end;

procedure TCustomUniDataSet.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomUniDataSet then begin
    TCustomUniDataSet(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TCustomUniDataSet.GetDataTypesMap: TDataTypesMapClass;
begin
  Result := GetProvider.GetDataTypesMap;
end;

function TCustomUniDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftCursor then
    Result := TDACursorField
  else
    Result := inherited GetFieldClass(FieldType);
end;

function TCustomUniDataSet.SQLGetFrom(SQLText: _string): _string;
begin
  if CanGetProvider then
    Result := _GetFrom(SQLText, GetProvider.GetParserClass, False)
  else
    Result := _GetFrom(SQLText, TSQLParser, False);
end;

function TCustomUniDataSet.SQLAddWhere(SQLText, Condition: _string): _string;
var
  UsedCon: TUniConnection;
begin
  if CanGetProvider then begin
    if TUniSQL(FCommand).FEnableUniSQL then begin
      UsedCon := TUniConnection(UsedConnection);
      if UsedCon <> nil then
        UsedCon.ExpandMacros(Condition);
    end;
    Result := _AddWhere(SQLText, Condition, GetProvider.GetParserClass, False);
  end
  else
    Result := _AddWhere(SQLText, Condition, TSQLParser, False);
end;

function TCustomUniDataSet.SQLDeleteWhere(SQLText: _string): _string;
begin
  if CanGetProvider then
    Result := _SetWhere(SQLText, '', GetProvider.GetParserClass, True)
  else
    Result := _SetWhere(SQLText, '', TSQLParser, True);
end;

function TCustomUniDataSet.SQLGetWhere(SQLText: _string): _string;
begin
  if CanGetProvider then
    Result := _GetWhere(SQLText, GetProvider.GetParserClass, False)
  else
    Result := _GetWhere(SQLText, TSQLParser, False);
end;

function TCustomUniDataSet.SQLSetOrderBy(SQLText: _string; Fields: _string): _string;
var
  UsedCon: TUniConnection;
begin
  if CanGetProvider then begin
    if TUniSQL(FCommand).FEnableUniSQL then begin
      UsedCon := TUniConnection(UsedConnection);
      if UsedCon <> nil then
        UsedCon.ExpandMacros(Fields);
    end;
    Result := _SetOrderBy(SQLText, Fields, GetProvider.GetParserClass);
  end
  else
    Result := _SetOrderBy(SQLText, Fields, TSQLParser);
end;

function TCustomUniDataSet.SQLGetOrderBy(SQLText: _string): _string;
begin
  if CanGetProvider then
    Result := _GetOrderBy(SQLText, GetProvider.GetParserClass)
  else
    Result := _GetOrderBy(SQLText, TSQLParser);
end;

procedure TCustomUniDataSet.SetLockFetchAll(Value: boolean);
begin
  if Value <> FLockFetchAll then begin
    FLockFetchAll := Value; 
    if FIRecordSet <> nil then
      FIRecordSet.SetProp(prLockFetchAll, Value);
  end;
end;

function TCustomUniDataSet.GetFetchAll: boolean;
var
  Value: Variant;
begin
  if FIRecordSet <> nil then begin
    FIRecordSet.GetProp(prFetchAll, Value);
    Result := Value;
  end
  else
    Result := False; // anti-warning
end;

procedure TCustomUniDataSet.SetFetchAll(Value: boolean);
var
  OldLock: boolean;
begin
  OldLock := LockFetchAll;
  LockFetchAll := False;
  try
    inherited;
  finally
    LockFetchAll := OldLock;
  end;
end;

procedure TCustomUniDataSet.QuickOpen(var Info: TQuickOpenInfo; Refresh: boolean = False);
var
  OldLock: boolean;
begin
  OldLock := LockFetchAll;
  LockFetchAll := True;
  try
    inherited;
  finally
    LockFetchAll := OldLock;
  end;
end;

procedure TCustomUniDataSet.Restore(const Info: TQuickOpenInfo; RestoreActive: boolean = True);
var
  OldLock: boolean;
begin
  OldLock := LockFetchAll;
  LockFetchAll := True;
  try
    inherited;
  finally
    LockFetchAll := OldLock;
  end;
end;

{ TUniParams }

constructor TUniParams.Create(Owner: TPersistent);
begin
  inherited Create(TUniParam);

  FOwner := Owner;  
  FNeedsUpdateItem := True;
end;

function TUniParams.GetOwner: TPersistent; // CLR cross-assembly
begin
  Result := FOwner;
end;

procedure TUniParams.SetOwner(Owner: TPersistent);
begin
  FOwner := Owner;
end;

function TUniParams.GetItem(Index: integer): TUniParam;
begin
  Result := TUniParam(inherited Items[Index]);
end;

procedure TUniParams.SetItem(Index: integer; Value: TUniParam);
begin
  inherited Items[Index] := Value;
end;

procedure TUniParams.Update(Item: TCollectionItem);
begin
  if FOwner <> nil then
    TUniSQL(FOwner).ResetParamRefs;

  inherited;
end;

function TUniParams.ParamByName(const Value: _string): TUniParam;
begin
  Result := TUniParam(inherited ParamByName(Value));
end;

function TUniParams.FindParam(const Value: _string): TUniParam;
begin
  Result := TUniParam(inherited FindParam(Value));
end;

{ TUniTransaction }

constructor TUniTransaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TUniTransaction.Destroy;
begin
  inherited;
end;

function TUniTransaction.GetProvider: TUniProvider;
begin
  if UsedConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniConnection(UsedConnection).GetProvider;
end;

function TUniTransaction.GetITransactionClass: TCRTransactionClass;
begin
  if TransactionType = ttNative then
    Result := GetProvider.GetTransactionClass
  else
    Result := inherited GetITransactionClass;
end;

function TUniTransaction.SQLMonitorClass: TClass;
begin
  Result := TUniSQLMonitor;
end;

function TUniTransaction.CanAutoCommitExplicitTransaction: boolean;
begin
  Result := False;
end;

procedure TUniTransaction.Commit;
begin
  CheckActive;

  inherited;
end;

procedure TUniTransaction.Rollback;
begin
  CheckActive;

  inherited;
end;

procedure TUniTransaction.CommitRetaining;
begin
  DoCommitRetaining;
end;

procedure TUniTransaction.RollbackRetaining;
begin
  DoRollbackRetaining;
end;

procedure TUniTransaction.Savepoint(Name: _string);
begin
  DoSavepoint(Name);
end;

procedure TUniTransaction.ReleaseSavePoint(Name: _string);
begin
  DoReleaseSavePoint(Name);
end;

procedure TUniTransaction.RollbackToSavepoint(Name: _string);
begin
  DoRollbackToSavepoint(Name);
end;

procedure TUniTransaction.DoSavepoint(const Name: _string);
begin
  CheckActive;

  inherited;
end;

procedure TUniTransaction.DoReleaseSavePoint(const Name: _string);
begin
  CheckActive;

  inherited;
end;

procedure TUniTransaction.DoRollbackToSavepoint(const Name: _string);
begin
  CheckActive;

  inherited;
end;

procedure TUniTransaction.AddConnection(Connection: TUniConnection);
begin
  DoAddConnection(Connection);
end;

procedure TUniTransaction.RemoveConnection(Connection: TUniConnection);
begin
  DoRemoveConnection(Connection);
end;

function TUniTransaction.GetDefaultConnection: TUniConnection;
begin
  Result := TUniConnection(inherited DefaultConnection);
end;

procedure TUniTransaction.SetDefaultConnection(Value: TUniConnection);
begin
  inherited DefaultConnection := Value;
end;

function TUniTransaction.GetConnection(Index: integer): TUniConnection;
begin
  Result := TUniConnection(inherited Connections[Index]);
end;

{ TUniTable }

constructor TUniTable.Create(Owner: TComponent);
begin
  inherited;

  LockMode := lmOptimistic;
end;

procedure TUniTable.SetDataSetService(Value: TDataSetService);
begin
  inherited;

  if FDataSetService <> nil then
    SQL.Clear;
end;

procedure TUniTable.PrepareSQL;
begin
  if SQL.Count = 0 then begin
    if TableName = '' then
      DatabaseError(STableNameNotDefined);

    CheckDataSetService;
    SQL.Text := TDADataSetService(FDataSetService).SQLGenerator.GenerateTableSQL(TableName, OrderFields);
  end;
end;

procedure TUniTable.Prepare;
begin
  // User can select provider at this point.
  // It is needed for PrepareSQL
  BeginConnection;
  try
    PrepareSQL;

    inherited;
  finally
    EndConnection;
  end;
end;

procedure TUniTable.Execute;
begin
  BeginConnection;
  try
    PrepareSQL;

    inherited;
  finally
    EndConnection;
  end;
end;

procedure TUniTable.SetTableName(const Value: _string);
begin
  if Value <> FTableName then begin
    if not (csLoading in ComponentState) then
      SQL.Clear;

    FTableName := Trim(Value);
  end;
end;

procedure TUniTable.SetOrderFields(const Value: _string);
var
  OldActive: boolean;
begin
  if Value <> FOrderFields then begin
    OldActive := Active;

    FOrderFields := Value;

    if not (csLoading in ComponentState) then
      SQL.Clear;

    if OldActive then
      Open;
  end;
end;

{$IFNDEF FPC}
{ IProviderSupport }

function TUniTable.PSGetTableName: string;
begin
  Result := AnsiUpperCase(TableName); // SQLResolver quotes table name
end;

procedure TUniTable.PSSetParams(AParams: DB.TParams);
var
  St: _string;
  i: integer;
begin
  if (Params.Count <> AParams.Count) then begin
    SQL.Text := '';
    St := '';
    
    for i := 0 to AParams.count - 1 do begin
      if St <> '' then
        St := St + ' AND ';
      St := St + AParams[i].Name + ' = :' + AParams[i].Name;
    end;

    PrepareSQL;

    if St <> '' then
      AddWhere(St);
  end;

  inherited;
end;

procedure TUniTable.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    TableName := CommandText;
end;
{$ENDIF}

procedure TUniTable.OpenCursor(InfoQuery: boolean);
begin
  // User can select provider at this point.
  // It is need for PrepareSQL
  BeginConnection;
  try
    PrepareSQL;

    inherited;
  finally
    EndConnection;
  end;
end;

procedure TUniTable.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniTable then begin
    TUniTable(Dest).SQL.Text := SQL.Text;
    TUniTable(Dest).FTableName := TableName;
    TUniTable(Dest).FOrderFields := OrderFields;
  end;
end;

procedure TUniTable.CheckSQL;
begin
  PrepareSQL;
end;

procedure TUniTable.SetFilterSQL(const Value: _string);
begin
  if SQL.Count = 0 then
    FFilterSQL := Value
  else
    inherited;
end;

function TUniTable.GetFinalSQL: _string;
begin
  {if (TableName <> '') and not (csLoading in ComponentState) then
    PrepareSQL;} // GetFinalSQL is called from PrepareSQL\BeginConnection

  Result := inherited GetFinalSQL;
end;

{ TUniStoredProc }

procedure TUniStoredProc.AssignTo(Dest: TPersistent);
begin
  if Dest is TUniStoredProc then begin
    TUniStoredProc(Dest).SQL.Text := SQL.Text;
    TUniStoredProc(Dest).FStoredProcName := FStoredProcName;
    TUniStoredProc(Dest).FIsQuery := FIsQuery;
  end;

  inherited;
end;

procedure TUniStoredProc.PrepareSQL(IsQuery: boolean = False);
begin
  if (IsQuery <> FIsQuery) or (SQL.Count = 0) then begin
    if StoredProcName = '' then
      DatabaseError(SStoredProcNotDefined);

    InternalCreateProcCall(StoredProcName, Params.Count = 0, IsQuery);
    FIsQuery := IsQuery;
  end;
end;

procedure TUniStoredProc.Prepare;
begin
  if not Prepared then
    PrepareSQL(False);

  inherited;
end;

procedure TUniStoredProc.CreateCommand;
begin
  inherited;

  TUniSQL(FCommand).FEnableUniSQL := False;
end;

procedure TUniStoredProc.DoBeforeExecute;
begin
  if SQL.Count = 0 then
    PrepareSQL(False);

  inherited;
end;

procedure TUniStoredProc.BeforeOpenCursor(InfoQuery: boolean);
begin
  if SQL.Count = 0 then
    PrepareSQL(True);

  inherited;
end;

procedure TUniStoredProc.ExecProc;
begin
  Execute;
end;

procedure TUniStoredProc.SetStoredProcName(const Value: _string);
begin
  if Value <> FStoredProcName then begin
    if not (csReading in ComponentState) then
      SQL.Text := '';
    FStoredProcName := Trim(Value);
  end;
end;

{$IFNDEF FPC}
procedure TUniStoredProc.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    StoredProcName := CommandText;
end;
{$ENDIF}

{ TUniUpdateSQL }

function TUniUpdateSQL.DataSetClass: TCustomDADataSetClass;
begin
  Result := TCustomUniDataSet;
end;

function TUniUpdateSQL.SQLClass: TCustomDASQLClass;
begin
  Result := TUniSQL;
end;

{ TUniMetaData }

function TUniMetaData.UsedTransaction: TDATransaction;
begin
  if FFixedUsedTransaction <> nil then
    Result := FFixedUsedTransaction
  else
    Result := inherited UsedTransaction;
end;

procedure TUniMetaData.SetTransaction(Value: TDATransaction);
begin
  if (FFixedUsedTransaction <> nil) and (Value <> FFixedUsedTransaction) and
    (UsedConnection <> nil) and TUniConnection(UsedConnection).IsMultipleTransactionsSupported
  then begin
    Close;

    if not Prepared and not Active then
      FFixedUsedTransaction := nil;
  end;

  inherited;
end;

procedure TUniMetaData.BeginConnection;
begin
  inherited;

  if FFixedUsedTransaction = nil then
    FFixedusedTransaction := UsedTransaction;
end;

procedure TUniMetaData.EndConnection;
begin
  inherited;

  if not Prepared and not Active then
    FFixedUsedTransaction := nil;
end;

procedure TUniMetaData.CloseCursor;
begin
  inherited;

  if not Prepared then
    FFixedUsedTransaction := nil;
end;

function TUniMetaData.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniMetaData.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniMetaData.GetUniTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TUniMetaData.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

{ TUniParam }

procedure TUniParam.CreateObject;
begin
  case DataType of
    ftCursor:
      FParamObject := TUniCursor.Create;
  else
    if IsBlobDataType then begin
      FParamObject := TUniBlob.Create;
    {$IFDEF VER10P}  
      if DataType = ftWideMemo then
        TUniBlob(FParamObject).IsUnicode := True;
    {$ENDIF}   
    end
    else
      inherited;
  end;
end;

function TUniParam.GetNativeParamObject: TSharedObject;
var
  Provider: TUniProvider;
  UniBlob: TUniBlob;
  UniCursor: TUniCursor;
  ClassType: TClass;
begin
  if (FParamObject is TUniBlob) or (FParamObject is TUniCursor) then begin
    Provider := nil;
    if TUniParams(GetOwner).GetOwner is TUniSQL then
      if TUniSQL(TUniParams(GetOwner).GetOwner).UsedConnection <> nil then
        Provider := TUniConnection(TUniSQL(TUniParams(GetOwner).GetOwner).UsedConnection).GetProvider;

    if Provider = nil then
      DatabaseError(SConnectionNotDefined);

    ClassType := Provider.GetParamObjectClass(Self);

    Result := nil; // to suppress warning
    if FParamObject is TUniBlob then begin
      UniBlob := TUniBlob(FParamObject);
      if (UniBlob.GetNativeBlob = nil) or (UniBlob.GetNativeBlob.ClassType <> ClassType) then
        UniBlob.SetNativeBlob(Provider.CreateParamObject(Self) as TBlob);
      Result := UniBlob.GetNativeBlob;
    end
    else
    if FParamObject is TUniCursor then begin
      UniCursor := TUniCursor(FParamObject);
      if (UniCursor.GetNativeCursor = nil) or (UniCursor.GetNativeCursor.ClassType <> ClassType) then
        UniCursor.SetNativeCursor(Provider.CreateParamObject(Self) as TCRCursor);
      Result := UniCursor.GetNativeCursor;
    end
  end
  else
    Result := inherited GetNativeParamObject;
end;

function TUniParam.IsBlobDataType(DataType: TFieldType): boolean;
begin
  Result := inherited IsBlobDataType(DataType) or
    (DataType = ftOraClob) or (DataType = ftOraBlob);
end;

function TUniParam.IsObjectDataType(DataType: TFieldType): boolean;
begin
  Result := inherited IsObjectDataType(DataType) or
    (DataType = ftOraClob) or (DataType = ftOraBlob) or
    (DataType = ftCursor) or (DataType = ftADT) or (DataType = ftArray);
end;

function TUniParam.IsObjectDataType: boolean;
begin
  Result := inherited IsObjectDataType;
end;

{ TUniBlob }

destructor TUniBlob.Destroy;
begin
  FNativeBlob.Free;

  inherited;
end;

procedure TUniBlob.Disconnect;
begin
  if FNativeBlob <> nil then
    FNativeBlob.Disconnect;
end;

function TUniBlob.GetNativeBlob: TBlob;
begin
  Result := FNativeBlob;
end;

procedure TUniBlob.SetNativeBlob(Value: TBlob);
begin
  if Value <> FNativeBlob then begin
    FNativeBlob.Free;
    FNativeBlob := Value;

    if FNativeBlob <> nil then
      FNativeBlob.SetData(FData);
  end;
end;

{ TUniCursor }

destructor TUniCursor.Destroy;
begin
  FNativeCursor.Free;

  inherited;
end;

function TUniCursor.GetNativeCursor: TCRCursor;
begin
  Result := FNativeCursor;
end;

procedure TUniCursor.SetNativeCursor(Value: TCRCursor);
begin
  if Value <> FNAtiveCursor then begin
    FNativeCursor.Free;
    FNativeCursor := Value;
  end;
end;

procedure TUniCursor.Disconnect;
begin
  if FNativeCursor <> nil then
    FNativeCursor.Disconnect;
end;

function TUniCursor.CanFetch: boolean;
begin
  if FNativeCursor = nil then
    Result := False
  else
    Result := FNativeCursor.CanFetch;
end;

{ TUniUtils }

class function TUniUtils.GetProvider(Connection: TUniConnection): TUniProvider;
begin
  Connection.CheckProvider;
  Result := Connection.FProvider;
end;

class function TUniUtils.GetCRConnection(Connection: TUniConnection): TCRConnection;
begin
  Result := Connection.FIConnection;
end;

class function TUniUtils.CanGetProvider(Connection: TUniConnection): boolean;
begin
  Result := Connection.FProvider <> nil;
end;

procedure GetServerList(ProviderName: string; List: _TStrings; SpecificOptions: _TStrings = nil);
var
  Provider: TUniProvider;
  ServerEnumerator: TCRServerEnumerator;
begin
  if ProviderName = '' then
    DatabaseError(SProviderNotDefined);

  Provider := UniProviders.GetProvider(ProviderName);

  if Provider = nil then
    CheckProviderName(ProviderName);

  ServerEnumerator := Provider.GetServerEnumeratorClass.Create;
  try
    if SpecificOptions <> nil then
      Provider.SetObjectProps(ServerEnumerator, SpecificOptions, False);

    ServerEnumerator.GetServerList(List);
  finally
    ServerEnumerator.Free;
  end;
end;

initialization

finalization

end.

