
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ODBCDac.inc}
unit ODBCClassesUni;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  Classes, SysUtils, Variants, SyncObjs, FMTBcd,
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
  MemUtils, MemData, CRAccess, CRParser,
{$IFNDEF UNIDACPRO}
  ODBCCall, ODBCError, ODBCParser;
{$ELSE}
  ODBCCallUni, ODBCErrorUni, ODBCParserUni;
{$ENDIF}

const
  dtBlobLocator = 100;
  dtClobLocator = 101;
  dtDBClobLocator = 102;

  prODBCBase              = 1000;
  prUseUnicode            = prODBCBase + 1;
  prConnectionTimeout     = prODBCBase + 2;
  prCommandTimeout        = prODBCBase + 3;
  prDSNType               = prODBCBase + 4;
  prDetectFieldsOnPrepare = prODBCBase + 5;

  dsMaxStringSize = 8192;
  DefaultStringFieldLength = dsMaxStringSize div 2 - 1;
  DefaultParamSize = 8000;

type
  TDSNType = (ntAuto, ntName, ntFile, ntConnectionString);

  TODBCRecordSet = class;

{ TODBCConnection }

  TODBCConnection = class(TCRConnection)
  private
    FCli: TODBCCli;
    FSQLHDbc: TSQLHDbc;
    FUseUnicode: boolean;
    FConnectionTimeout: integer;
    FDSNType: TDSNType;
    FDriverODBCVer: word;
    FIdentCase: TIdentCase;
    FDetectFieldsOnPrepare: boolean;

    function GetDriverInfo(InfoType: word): _string;
    procedure ObtainDriverInfo;

  protected
    FRecordSet: TODBCRecordSet;
    FCachedCatalog: _string;
    FCachedSchema: _string;

    function CreateSQLInfo: TSQLInfo; override;
    function GetConnectionString: _string; virtual;
    function GetCli: TODBCCli; virtual;
    procedure ApplyConnectProps; virtual;
    function IsCommentAllowed: boolean; virtual;
    function IsEmptyStringAllowed: boolean; virtual;
    function IsBlockFetchAllowed: boolean; virtual;
    function IsReturnValueAllowed: boolean; virtual;
    function OutParamIsInOut: boolean; virtual;
    procedure CheckRecordSet;
    function IsDriverPresent(const Name: string): boolean;

    property EnableBCD;
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
    function GetRecordSetClass: TCRRecordSetClass; virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Check(ErrorCode: smallint);
    procedure ProcessError(HandleType: smallint; Handle: TSQLHandle; ErrorCode: smallint; Component: TObject); virtual;

    procedure Connect(const ConnectString: _string); override;
    procedure Disconnect; override;
    function CheckIsValid: boolean; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function GetServerVersion: _string; override;
    function GetServerVersionFull: _string; override;
    function GetClientVersion: _string; override;
    function GetDriverODBCVersion: _string;

    function CanChangeDatabase: boolean; override;
    function GetCurrentCatalog: _string; virtual;
    function GetCachedCatalog: _string; virtual;
    function GetCurrentSchema: _string; virtual;
    function GetCachedSchema: _string; virtual;
    procedure ExecSQL(const SQL: _string);

    property Cli: TODBCCli read FCli;
    property SQLHDbc: TSQLHDbc read FSQLHDbc;
  end;

{ TODBCTransaction }

  TODBCTransaction = class(TCRTransaction)
  public
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure Savepoint(const Name: _string); override;
    procedure ReleaseSavepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;
  end;

{ TODBCParamDesc }

  TODBCParamDesc = class (TParamDesc)
  private
    FBuffer: IntPtr;
    FBufferSize: integer;
    FIndicator: IntPtr;

  protected
    FEnableMSec: boolean;

    procedure AllocBuffer;
    procedure FreeBuffer;
    procedure WriteBuffer(AllowDataAtExec, AllowEmptyString: boolean; var DataAtExec: boolean;
      var Precision, Scale: integer);
    function WriteAnsiString(const Value: variant): integer;
    function WriteWideString(const Value: variant): integer;
    function WriteBytes(const Value: variant): integer;
    procedure ReadBuffer;
    function ReadAnsiString(Len: integer): AnsiString;
    function ReadWideString(Len: integer): WideString;
    function ReadBytes(Len: integer): variant;

    property Name;
    property DataType;
    property SubDataType;
    property ParamType;
    property Size;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetDataType(Value: word); override;
    procedure SetParamType(Value: TParamDirection); override;
    procedure SetSize(Value: integer); override;

    function GetBlob: TBlob;
  end;

{ TODBCSQLInfo }

  TODBCSQLInfo = class(TSQLInfo)
  private
    FConnection: TODBCConnection;
  public
    constructor Create(ParserClass: TSQLParserClass); override;
    function IdentCase: TIdentCase; override;
  end;

{ TODBCCommand }

  TODBCCommand = class(TCRCommand)
  private
    FStmt: TSQLHStmt;
    FCursorState: TCursorState;
    FRowsAffected: integer;
    FRecordSetExec: boolean;

    FCommandTimeout: integer;

    function Cli: TODBCCli;
    procedure BindParams;
    procedure ReadOutParams;
    procedure WriteBlobs;
    procedure WriteBlob(const Value: variant);
    function ReadBlob(Blob: TBlob; FieldNo: integer; DataType: word; var SharedPiece: PPieceHeader): boolean;
    function RemoveComments(const Value: _string): _string;

  protected
    FConnection: TODBCConnection;

    procedure Check(ErrorCode: smallint);
    procedure InternalPrepare; virtual;
    procedure InternalExecute; virtual;
    function GetODBCParamType(ParamType: TParamDirection): smallint;
    function GetCDataType(DataType, SubDataType: word): smallint; virtual;
    function GetSQLDataType(DataType, SubDataType: word): smallint; virtual;
    procedure SetSpecificParamPrec(Param: TODBCParamDesc; var Prec, Scale: integer); virtual;
    function GetBlobSize(SQLLength: longword): Integer; virtual;
    procedure DetectDataType(SQLDataType: smallint; SQLLength: longword; SQLScale: smallint;
      var DataType, SubDataType: word; var Length, Scale: integer;
      var Fixed: boolean; LongStrings, FlatBuffers, FieldsAsString: boolean);

    property EnableBCD;
  {$IFNDEF FPC}
    property EnableFMTBCD;
  {$ENDIF}

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SetConnection(Value: TCRConnection); override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    function GetParamDescType: TParamDescClass; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;
    procedure Execute(Iters: integer = 1); override;
    procedure InitProcParams(const Name: _string);
    function CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string; override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    property SQLHStmt: TSQLHStmt read FStmt;
  end;

{ TODBCRecordSet }

  TODBCRecordSet = class(TCRRecordSet)
  private
    FRowsFetched: integer;
    FRowsFetchedBuf: IntPtr;
    FFetchBlock: IntPtr;
    FFetchBlockItemSize: integer;
    FHasConvertedFields: boolean;
    FFieldsAsString: boolean;

    function Cli: TODBCCli;
    procedure DescribeFieldDesc(Field: TCRFieldDesc; Index: integer);
    function InternalFetch: boolean;
    procedure AllocFetchBlock;
    procedure FreeFetchBlock;
    procedure InitBlock(pHBlock: IntPtr);
    procedure ClearBlock(pHBlock: IntPtr; FromRow: integer);
    procedure ReadFetchBlock(RecBuf: IntPtr; var SharedPiece: PPieceHeader);
    function  BindColumnsForBlockFetch: Smallint;
    procedure BindColumns(RecBuf: IntPtr; FetchRows: integer);

  protected
    FCommand: TODBCCommand;

    procedure Check(ErrorCode: smallint);
    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
    procedure DetectCommandType; virtual;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalInitFields; override;
    procedure InternalClose; override;
    function Fetch(FetchBack: boolean = False): boolean; override;
    function GetIndicatorSize: word; override;

    function IsConvertedFieldType(DataType: word): boolean;
    function GetFieldFetchBlockSize(Field: TFieldDesc): integer; virtual;
    function GetFieldBindSize(Field: TFieldDesc): integer; virtual;
    function ReadFetchBlockField(Field: TFieldDesc; Source, Dest: IntPtr; Len: integer; var SharedPiece: PPieceHeader): boolean; virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure ExecCommand; override;
    function IsFullReopen: boolean; override;
    procedure Reopen; override;
    procedure SetToEnd; override;
    function GetNull(FieldNo: word; RecBuf: IntPtr): boolean; override;
    procedure SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean); override;
  end;

{ TODBCMetaDataCommand }

  TODBCMetaDataKind = (mkTables, mkColumns, mkProcedures, mkProcedureColumns,
    mkStatistics, mkSpecialColumns);

  TMetaDataArgs = class
  public
    CatalogName: _string;
    SchemaName: _string;
    ObjectName: _string;
    ObjectType: _string;
    ColumnName: _string;
    Param1, Param2, Param3: smallint;
  end;

  TODBCMetaDataCommand = class(TODBCCommand)
  private
    FMetaDataKind: TODBCMetaDataKind;
    FMetaDataArgs: TMetaDataArgs;

  protected
    procedure InternalPrepare; override;
    procedure InternalExecute; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    property MetaDataKind: TODBCMetaDataKind read FMetaDataKind write FMetaDataKind;
    property MetaDataArgs: TMetaDataArgs read FMetaDataArgs;
  end;

{ TODBCMetaDataRecordSet }

  TODBCMetaDataRecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
    procedure DetectCommandType; override;
  end;

{ TODBCMetaData }

  TODBCMetaData = class (TCRMetaData)
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

    function GetSpecialColumns(Restrictions: _TStrings): TData;
    procedure CreateSpecialColumnsFields;
    procedure CopySpecialColumnsData(Restrictions: _TStrings);

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{$IFNDEF LITE}
  TODBCLoader = class (TCRSimpleLoader)
  protected
    procedure CreateCommand; override;
    class function GetRecordSetClass: TCRRecordSetClass; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
  end;
{$ENDIF}

implementation

uses
  Math, StrUtils, DAConsts,
{$IFNDEF UNIDACPRO}
  ODBCConsts;
{$ELSE}
  ODBCConstsUni;
{$ENDIF}

const
  BCDStrSize = 255;
  GuidStrSize = 38;

var
  LocaleDecSeparator: char;

{ TODBCConnection }

constructor TODBCConnection.Create;
begin
  inherited;

  FDetectFieldsOnPrepare := True;
  CheckRecordSet;
end;

destructor TODBCConnection.Destroy;
begin
  Disconnect;

  FRecordSet.Free;

  inherited;
end;

function TODBCConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prUseUnicode:
      FUseUnicode := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prDSNType:
      FDSNType := TDSNType(Value);
    prDetectFieldsOnPrepare:
      FDetectFieldsOnPrepare := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TODBCConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prMaxStringSize:
      Value := DefaultParamSize;
    prUseUnicode:
      Value := FUseUnicode;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prDSNType:
      Value := Variant(FDSNType);
    prDetectFieldsOnPrepare:
      Value := FDetectFieldsOnPrepare;  
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TODBCConnection.Check(ErrorCode: smallint);
begin
  if IsODBCError(ErrorCode) then
    ProcessError(SQL_HANDLE_DBC, FSQLHDbc, ErrorCode, Component);
end;

procedure TODBCConnection.ProcessError(HandleType: smallint; Handle: TSQLHandle;
  ErrorCode: smallint; Component: TObject);
var
  Error: EODBCError;
  Fail, NeedFreeError: boolean;
begin
  NeedFreeError := True;
  Error := FCli.GetODBCError(HandleType, Handle, ErrorCode);
  try
  {$IFNDEF LITE}
    Error.Component := Component;
  {$ENDIF}
    Fail := True;
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

procedure TODBCConnection.Connect(const ConnectString: _string);
var
  ConStr: _string;
  Len: smallint;
begin
  if FConnected then
    Exit;

  if FCli = nil then
    FCli := GetCli;

  if not FCli.IsInited then
    FCli.InitEnvironment;

  Check(FCli.SQLAllocHandle(SQL_HANDLE_DBC, FCli.SQLHEnv, FSQLHDbc));
  try
    if FConnectionTimeout <> 0 then
      Check(FCli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_LOGIN_TIMEOUT, FConnectionTimeout, 0));

    ConStr := GetConnectionString;

    Check(FCli.SQLDriverConnect(FSQLHDbc, 0, _PChar(ConStr), Length(ConStr),
      nil, 0, Len, SQL_DRIVER_NOPROMPT));

    FConnected := True;
    FNativeConnection := True;

    ObtainDriverInfo;

    ApplyConnectProps;

    inherited;

  except
    on EFailOver do;
    else begin
      if FSQLHDbc <> nil then begin
        FCli.SQLFreeHandle(SQL_HANDLE_DBC, FSQLHDbc);
        FSQLHDbc := nil;
      end;
      FConnected := False;
      raise;
    end;
  end;
end;

procedure TODBCConnection.Disconnect;
begin
  if not FConnected then
    exit;

  FConnected := False;
  try
    if FNativeConnection then
      Check(FCli.SQLDisconnect(FSQLHDbc));
  finally
    FCli.SQLFreeHandle(SQL_HANDLE_DBC, FSQLHDbc);
    FSQLHDbc := nil;
    FNativeConnection := True;
    FCachedCatalog := '';
    FCachedSchema := '';
  end;

  inherited;
end;

function TODBCConnection.CheckIsValid: boolean;
begin
  Result := True;
end;

procedure TODBCConnection.Assign(Source: TCRConnection);
var
  Src: TODBCConnection;
begin
  inherited;

  Src := TODBCConnection(Source);
  FUseUnicode := Src.FUseUnicode;
  FConnectionTimeout := Src.FConnectionTimeout;
  FDSNType := Src.FDSNType;
end;

procedure TODBCConnection.AssignConnect(Source: TCRConnection);
var
  Src: TODBCConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TODBCConnection(Source);
      Assign(Src);

      FCli := Src.FCli;
      FSQLHDbc := Src.FSQLHDbc;
      FDriverODBCVer := Src.FDriverODBCVer;
      FIdentCase := Src.FIdentCase;
      FCachedCatalog := Src.FCachedCatalog;
      FCachedSchema := Src.FCachedSchema;

      FInternalTransaction.AssignConnect(Src.FInternalTransaction);

      FConnected := True;
      FNativeConnection := False;
    end;
  end;
end;

function TODBCConnection.GetServerVersion: _string;
begin
  Result := GetDriverInfo(SQL_DBMS_VER);
end;

function TODBCConnection.GetServerVersionFull: _string;
begin
  Result := GetDriverInfo(SQL_DBMS_NAME) + ' ' + GetDriverInfo(SQL_DBMS_VER);
end;

function TODBCConnection.GetClientVersion: _string;
begin
  Result := GetDriverInfo(SQL_DRIVER_VER);
end;

function TODBCConnection.GetDriverODBCVersion: _string;
begin
  Result := GetDriverInfo(SQL_DRIVER_ODBC_VER);
end;

function TODBCConnection.CanChangeDatabase: boolean;
begin
  Result := False; 
end;

function TODBCConnection.GetCurrentCatalog: _string;
begin
  Result := '';
end;

function TODBCConnection.GetCachedCatalog: _string;
begin
  if FCachedCatalog = '' then
    FCachedCatalog := GetCurrentCatalog;

  Result := FCachedCatalog;
end;

function TODBCConnection.GetCurrentSchema: _string;
begin
  Result := '';
end;

function TODBCConnection.GetCachedSchema: _string;
begin
  if FCachedSchema = '' then
    FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;

procedure TODBCConnection.ExecSQL(const SQL: _string);
begin
  CheckRecordSet;

  FRecordSet.SetSQL(SQL);
  FRecordSet.ExecCommand;
end;

function TODBCConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TODBCCommand;
end;

function TODBCConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TODBCTransaction;
end;

function TODBCConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TODBCRecordSet;
end;

function TODBCConnection.CreateSQLInfo: TSQLInfo;
begin
  Result := inherited CreateSQLInfo;
  TODBCSQLInfo(Result).FConnection := Self;
end;

function TODBCConnection.GetConnectionString: _string;
var
  ActualDSNType: TDSNType;
begin
  ActualDSNType := FDSNType;

  if ActualDSNType = ntAuto then begin
    if Pos('=', FServer) > 0 then
      ActualDSNType := ntConnectionString
    else
    if Pos('.dsn', FServer) > 0 then
      ActualDSNType := ntFile
    else
      ActualDSNType := ntName;
  end;

  case ActualDSNType of
    ntName:
      Result := _Format('DSN=%s;UID=%s;PWD=%s', [FServer, FUsername, FPassword]);
    ntFile:
      Result := _Format('FILEDSN=%s;UID=%s;PWD=%s', [FServer, FUsername, FPassword]);
    ntConnectionString: begin
      Result := FServer;
      if (Pos('UID=', Result) = 0) and (FUsername <> '') then
        Result := Result + ';UID=' +FUsername;
      if (Pos('PWD=', Result) = 0) and (FPassword <> '') then
        Result := Result + ';PWD=' +FPassword;
    end;
  end;
end;

function TODBCConnection.GetCli: TODBCCli;
begin
  Result := GetODBCCli;
end;

procedure TODBCConnection.ApplyConnectProps;
begin
end;

function TODBCConnection.IsCommentAllowed: boolean;
begin
  Result := True;
end;

function TODBCConnection.IsEmptyStringAllowed: boolean;
begin
  Result := True;
end;

function TODBCConnection.IsBlockFetchAllowed: boolean;
begin
  Result := Hi(FDriverODBCVer) >= 3;
end;

function TODBCConnection.IsReturnValueAllowed: boolean;
begin
  Result := True;
end;

function TODBCConnection.OutParamIsInOut: boolean;
begin
  Result := False;
end;

procedure TODBCConnection.CheckRecordSet;
begin
  if FRecordSet = nil then begin
    FRecordSet := TODBCRecordSet(GetRecordSetClass.Create);
    FRecordSet.SetConnection(Self);
  end;
end;

function TODBCConnection.IsDriverPresent(const Name: string): boolean;
const
  BufSize = 256;
var
  Buf: IntPtr;
  Res, Len, Len2: smallint;
  s: _string;
begin
  Result := False;

  Buf := Marshal.AllocHGlobal(BufSize * SizeOf(_char));
  try
    Res := Cli.SQLDrivers(Cli.SQLHEnv, SQL_FETCH_FIRST, Buf, BufSize, Len, nil, 0, Len2);
    repeat
      if Res = SQL_NO_DATA then
        break;
      s := PtrToXString(Buf);
      if _SameText(s, Name) then begin
        Result := True;
        exit;
      end;
      Res := Cli.SQLDrivers(Cli.SQLHEnv, SQL_FETCH_NEXT, Buf, BufSize, Len, nil, 0, Len2);
    until False;
  finally
    Marshal.FreeHGlobal(Buf);
  end;
end;

function TODBCConnection.GetDriverInfo(InfoType: word): _string;
const
  BufSize = 256;
var
  Buf: IntPtr;
  Len: smallint;
begin
  Buf := Marshal.AllocHGlobal(BufSize * SizeOf(_char));
  try
    Check(FCli.SQLGetInfoStr(FSQLHDbc, InfoType, Buf, BufSize, Len));
    Result := PtrToXString(Buf);
  finally
    Marshal.FreeHGlobal(Buf);
  end;
end;

procedure TODBCConnection.ObtainDriverInfo;
var
  DrvODBCVer: _string;
  p, Min, Maj: integer;
  ValueNInt: NativeInt;
begin
  DrvODBCVer := GetDriverODBCVersion;
  p := Pos('.', DrvODBCVer);
  if p > 0 then begin
    Maj := StrToIntDef(Copy(DrvODBCVer, 1, p - 1), 0);
    Min := StrToIntDef(Copy(DrvODBCVer, p + 1, MaxInt), 0);
  end
  else begin
    Maj := StrToIntDef(DrvODBCVer, 0);
    Min := 0;
  end;
  FDriverODBCVer := Maj shl 8 or Min;

  Check(FCli.SQLGetInfoInt(FSQLHDbc, SQL_IDENTIFIER_CASE, ValueNInt, 4, nil));
  case Word(ValueNInt) of
    SQL_IC_UPPER:
      FIdentCase := icUpper;
    SQL_IC_LOWER:
      FIdentCase := icLower;
    SQL_IC_MIXED:
      FIdentCase := icMixed;
    SQL_IC_SENSITIVE:
      FIdentCase := icMixedCaseSensitive;
  else
    FIdentCase := icMixed;
  end;
end;

{ TODBCTransaction }

procedure TODBCTransaction.StartTransaction;
var
  Connection: TODBCConnection;
  ODBCLevel: integer;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  Connection := TODBCConnection(FConnections[0]);

  if not Connection.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);

  case FIsolationLevel of
    ilReadUncommitted:
      ODBCLevel := SQL_TXN_READ_UNCOMMITTED;
    ilReadCommitted:
      ODBCLevel := SQL_TXN_READ_COMMITTED;
    ilRepeatableRead:
      ODBCLevel := SQL_TXN_REPEATABLE_READ;
    ilIsolated, ilSnapshot:
      ODBCLevel := SQL_TXN_SERIALIZABLE;
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;

  with Connection do begin
    Check(FCli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_TXN_ISOLATION, ODBCLevel, 0));
    Check(FCli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF, 0));
  end;

  FActive := True;
  FNativeTransaction := True;
end;

procedure TODBCTransaction.Commit;
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  if FNativeTransaction then
    with Connection do begin
      Check(FCli.SQLEndTran(SQL_HANDLE_DBC, FSQLHDbc, SQL_COMMIT));
      Check(FCli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON, 0));
    end;

  FActive := False;
end;

procedure TODBCTransaction.Rollback;
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  if FNativeTransaction then
    with Connection do begin
      Check(FCli.SQLEndTran(SQL_HANDLE_DBC, FSQLHDbc, SQL_ROLLBACK));
      Check(FCli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON, 0));
    end;

  FActive := False;
end;

procedure TODBCTransaction.Savepoint(const Name: _string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecSQL('SAVEPOINT ' + Name);
end;

procedure TODBCTransaction.ReleaseSavepoint(const Name: _string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecSQL('RELEASE SAVEPOINT ' + Name);
end;

procedure TODBCTransaction.RollbackToSavepoint(const Name: _string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecSQL('ROLLBACK TO SAVEPOINT ' + Name);
end;

{ TODBCParamDesc }

constructor TODBCParamDesc.Create;
begin
  inherited Create;

  FBufferSize := -1;
  FIndicator := Marshal.AllocHGlobal(SizeOf(IntPtr));
  FEnableMSec := true;
end;

destructor TODBCParamDesc.Destroy;
begin
  FreeBuffer;
  Marshal.FreeHGlobal(FIndicator);

  inherited;
end;

procedure TODBCParamDesc.SetDataType(Value: word);
begin
  if Value <> FDataType then begin
    FreeBuffer;

    FDataType := Value;
  end;
end;

procedure TODBCParamDesc.SetParamType(Value: TParamDirection);
begin
  if Value <> FParamType then begin
    FreeBuffer;

    FParamType := Value;
  end;
end;

procedure TODBCParamDesc.SetSize(Value: integer);
begin
  if Value <> FSize then begin
    case FDataType of
      dtString, dtWideString, dtBytes, dtVarBytes:
        FreeBuffer;
    end;

    FSize := Value;
  end;
end;

function TODBCParamDesc.GetBlob: TBlob;
var
  Obj: TObject;
begin
  case VarType(Value) of
    {$IFDEF CLR}varObject{$ELSE}varByRef{$IFDEF FPC} or varVariant{$ENDIF}{$ENDIF}: begin
    {$IFDEF CLR}
      Obj := Value;
    {$ELSE}
      Obj := TObject(TVarData(Value).VPointer);
    {$ENDIF}
      if not (Obj is TBlob) then
        raise Exception.Create(SCannotConvertType);
      Result := TBlob(Obj);
    end;
  else
    Result := nil;
  end;
end;

procedure TODBCParamDesc.AllocBuffer;
begin
  if FBufferSize >= 0 then
    exit;

  if FDataType = dtUnknown then
    raise Exception.Create(SUnknownDataType);

  case FDataType of
    dtString, dtBytes, dtVarBytes:
      if FSize <> 0 then
        FBufferSize := FSize + 1
      else
        FBufferSize := 1;
    dtWideString:
      if FSize <> 0 then
        FBufferSize := (FSize + 1) * 2
      else
        FBufferSize := 2;
    dtBlob, dtMemo:
      FBufferSize := 1;
    dtWideMemo:
      FBufferSize := 2;
    dtInt8:
      FBufferSize := SizeOf(Byte);
    dtSmallInt, dtWord:
      FBufferSize := SizeOf(SmallInt);
    dtInteger:
      FBufferSize := SizeOf(Integer);
    dtLargeInt:
      FBufferSize := SizeOf(Int64);
    dtBoolean:
      FBufferSize := sizeOf(SmallInt);
    dtFloat, dtCurrency:
      FBufferSize := sizeof(Double);
    dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}:
      FBufferSize := BCDStrSize;
    dtDate:
      FBufferSize := SizeOf(TSQLDateStruct);
    dtTime:
      FBufferSize := SizeOf(TSQLTimeStruct);
    dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}:
      FBufferSize := SizeOf(TSQLTimeStampStruct);
    dtGuid:
      FBufferSize := GuidStrSize;
  else
    raise Exception.Create(SDataTypeNotSupported);
  end;

  if FBufferSize > 0 then begin
    FBuffer := Marshal.AllocHGlobal(FBufferSize);
    FillChar(FBuffer, FBufferSize, $00);
  end;
end;

procedure TODBCParamDesc.FreeBuffer;
begin
  if (FBufferSize > 0) and (FBuffer <> nil) then
    Marshal.FreeHGlobal(FBuffer);

  FBuffer := nil;
  FBufferSize := -1;
end;

procedure TODBCParamDesc.WriteBuffer(AllowDataAtExec, AllowEmptyString: boolean;
  var DataAtExec: boolean; var Precision, Scale: integer);
var
  Len: integer;
  i64: int64;
  Cur: currency;
  Str: string;
{$IFNDEF FPC}
  Bcd: TBcd;
{$ENDIF}
  Year, MSec: word;
  DateRec: TSQLDateStruct;
  TimeRec: TSQLTimeStruct;
  TsRec: TSQLTimeStampStruct;
  Blob: TBlob;
begin
  AllocBuffer;

  if VarIsNull(Value) or VarIsEmpty(Value) or
    not AllowEmptyString and VarIsStr(Value) and (Value = '')
  then begin
    Marshal.WriteIntPtr(FIndicator, IntPtr(SQL_NULL_DATA));
    exit;
  end;

  Len := 0;
  DataAtExec := False;
  Precision := 0;
  Scale := 0;
  case FDataType of
    dtString:
      Len := WriteAnsiString(Value);
    dtWideString:
      Len := WriteWideString(Value);
    dtBytes, dtVarBytes:
      Len := WriteBytes(Value);
    dtInt8:
      Marshal.WriteByte(FBuffer, Byte(Value));
    dtSmallInt, dtWord:
      Marshal.WriteInt16(FBuffer, Smallint(Value));
    dtInteger:
      Marshal.WriteInt32(FBuffer, Integer(Value));
    dtLargeInt: begin
      i64 := Value;
      Marshal.WriteInt64(FBuffer, i64);
    end;
    dtBoolean:
      Marshal.WriteInt16(FBuffer, Smallint(Boolean(Value)));
    dtFloat, dtCurrency:
      Marshal.WriteInt64(FBuffer, BitConverter.DoubleToInt64Bits(Value));
    dtBCD: begin
      Cur := Currency(Value);
      Str := CurrToStr(Cur);
      if DecimalSeparator <> '.' then
        Str := StringReplace(Str, DecimalSeparator, '.', []);
      Len := WriteAnsiString(AnsiString(Str));
    end;
  {$IFNDEF FPC}
    dtFMTBCD: begin
      if VarType(Value) = VarFMTBcd then
        Bcd := VarToBcd(Value)
      else
        Bcd := StrToBcd(Value);

      Precision := Bcd.Precision;
      Scale := Bcd.SignSpecialPlaces and $3F;

      Str := BcdToStr(Bcd);
      if DecimalSeparator <> '.' then
        Str := StringReplace(Str, DecimalSeparator, '.', []);
      Len := WriteAnsiString(AnsiString(Str));
    end;
  {$ENDIF}
    dtDate: begin
      DecodeDate(Value, Year, DateRec.Month, DateRec.Day);
      DateRec.Year := Year;
    {$IFNDEF CLR}
      TSQLDateStruct(FBuffer^) := DateRec;
    {$ENDIF}
    end;
    dtTime: begin
      DecodeTime(Value, TimeRec.Hour, TimeRec.Minute, TimeRec.Second, MSec);
    {$IFNDEF CLR}
      TSQLTimeStruct(FBuffer^) := TimeRec;
    {$ENDIF}
    end;
    dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
      DecodeDate(Value, Year, TsRec.Month, TsRec.Day);
      TsRec.Year := Year;
      DecodeTime(Value, TsRec.Hour, TsRec.Minute, TsRec.Second, MSec);
      if FEnableMSec then
        TsRec.Fraction := MSec * 1000000
      else
        TsRec.Fraction := 0;
    {$IFNDEF CLR}
      TSQLTimeStampStruct(FBuffer^) := TsRec;
    {$ENDIF}
    end;
    dtGuid:
      Len := WriteAnsiString(Value);
    dtBlob, dtMemo, dtWideMemo: begin
      Blob := GetBlob;
      if Blob <> nil then
        Len := Blob.Size
      else
        Len := 1;

      if Len = 0 then
        Len := SQL_NULL_DATA
      else
      if AllowDataAtExec then begin
        Len := SQL_DATA_AT_EXEC;
        DataAtExec := True;
      end
      else begin
        if Blob = nil then
          raise Exception.Create(SCannotConvertType);
        Len := Blob.Size;
      end;
    end;
  else
    raise Exception.Create(SDataTypeNotSupported);
  end;

  Marshal.WriteIntPtr(FIndicator, IntPtr(Len));
end;

function TODBCParamDesc.WriteAnsiString(const Value: variant): integer;
var
  AStr: AnsiString;
  Len: integer;
  SPtr: IntPtr;
begin
  AStr := AnsiString(Value);
  Len := Length(AStr);
  if Len > FBufferSize then
    Len := FBufferSize;

  SPtr := Marshal.StringToHGlobalAnsi(AStr);
  try
    if FConvertEOL then
      Len := RemoveCRString(SPtr, FBuffer, Len, Length(AStr))
    else begin
      CopyBuffer(SPtr, FBuffer, Len);
    end;
  finally
    FreeString(SPtr);
  end;

  Result := Len;
end;

function TODBCParamDesc.WriteWideString(const Value: variant): integer;
var
  WStr: WideString;
  Len: integer;
  SPtr: IntPtr;
begin
  WStr := WideString(Value);
  Len := Length(WStr);
  if Len * 2 > FBufferSize then
    Len := FBufferSize div 2;

  SPtr := Marshal.StringToHGlobalUni(WStr);
  try
    if FConvertEOL then
      Len := RemoveCRUnicode(SPtr, FBuffer, Len, Length(WStr))
    else
      CopyBuffer(SPtr, FBuffer, Len * 2);
  finally
    FreeString(SPtr);
  end;

  Result := Len * 2;
end;

function TODBCParamDesc.WriteBytes(const Value: variant): integer;
var
  AStr: AnsiString;
  Bytes: TBytes;
  Len: integer;
begin
  Bytes := nil;
  if VarIsStr(Value) then begin
    AStr := AnsiString(Value);
    Len := Length(AStr);
    if Len > FBufferSize then
      Len := FBufferSize;

    Marshal.Copy(TBytes(AStr), 0, FBuffer, Len);
  end
  else begin
    Bytes := Value;
    Len := Length(Bytes);
    if Len > FBufferSize then
      Len := FBufferSize;

    Marshal.Copy(Bytes, 0, FBuffer, Len);
  end;

  Result := Len;
end;

procedure TODBCParamDesc.ReadBuffer;
var
  Len: integer;
  Str: string;
  Date: TDateTime;
  DateRec: TSQLDateStruct;
  TimeRec: TSQLTimeStruct;
{$IFNDEF CLR}
  TsRec: TSQLTimeStampStruct;
{$ENDIF}
  Blob: TBlob;
begin
  Len := Marshal.ReadInt32(FIndicator);
  if Len = SQL_NULL_DATA then begin
    Value := Null;
    exit;
  end;

  case FDataType of
    dtString:
      Value := ReadAnsiString(Len);
    dtWideString:
      Value := ReadWideString(Len);
    dtBytes, dtVarBytes:
      Value := ReadBytes(Len);
    dtInt8:
      Value := Marshal.ReadByte(FBuffer);
    dtWord:
      Value := Word(Marshal.ReadInt16(FBuffer));
    dtSmallInt:
      Value := Marshal.ReadInt16(FBuffer);
    dtInteger:
      Value := Marshal.ReadInt32(FBuffer);
    dtLargeInt:
      Value := Marshal.ReadInt64(FBuffer);
    dtBoolean:
      Value := Boolean(Marshal.ReadInt16(FBuffer));
    dtFloat, dtCurrency:
      Value := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FBuffer));
    dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}: begin
      Str := string(ReadAnsiString(0));
      if DecimalSeparator <> '.' then
        Str := StringReplace(Str, '.', DecimalSeparator, []);
    {$IFNDEF FPC}
      if FDataType = dtFMTBCD then
        Value := VarFMTBcdCreate(StrToBcd(Str))
      else
    {$ENDIF}
        Value := StrToCurr(Str);
    end;
    dtDate: begin
    {$IFNDEF CLR}
      DateRec := TSQLDateStruct(FBuffer^);
    {$ENDIF}
      if TryEncodeDate(DateRec.Year, DateRec.Month, DateRec.Day, Date) then
        Value := Date
      else
        Value := MinDateTime;
    end;
    dtTime: begin
    {$IFNDEF CLR}
      TimeRec := TSQLTimeStruct(FBuffer^);
    {$ENDIF}
      if TryEncodeTime(TimeRec.Hour, TimeRec.Minute, TimeRec.Second, 0, Date) then
        Value := Date
      else
        Value := 0;
    end;
    dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
    {$IFNDEF CLR}
      TsRec := TSQLTimeStampStruct(FBuffer^);
      if TryEncodeDateTime(TsRec.Year, TsRec.Month, TsRec.Day,
        TsRec.Hour, TsRec.Minute, TsRec.Second, TsRec.Fraction div 1000000, Date)
      then
        Value := Date
      else
    {$ENDIF}
        Value := MinDateTime;
    end;
    dtGuid: begin
      Str := string(ReadAnsiString(0));
      if (Str <> '') and (Str[1] <> '{') then
        Str := '{' + Str + '}';
      Value := Str;
    end;
    dtBlob, dtMemo, dtWideMemo: begin
      Blob := GetBlob;
      Blob.FirstPiece.Used := Len;
    end;
  else
    raise Exception.Create(SDataTypeNotSupported);
  end;
end;

function TODBCParamDesc.ReadAnsiString(Len: integer): AnsiString;
var
  Ptr: IntPtr;
begin
  if FConvertEOL then begin
    Ptr := Marshal.AllocHGlobal(FSize * 4);
    try
      Len := AddCRString(FBuffer, Ptr, FSize);
      Result := Marshal.PtrToStringAnsi(Ptr, Len);
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end
  else
    Result := Marshal.PtrToStringAnsi(FBuffer, Len);
end;

function TODBCParamDesc.ReadWideString(Len: integer): WideString;
var
  Ptr: IntPtr;
begin
  if FConvertEOL then begin
    Ptr := Marshal.AllocHGlobal(FSize * 4);
    try
      Len := AddCRUnicode(FBuffer, Ptr, FSize);
      Result := Marshal.PtrToStringUni(Ptr, Len);
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end
  else
    Result := Marshal.PtrToStringUni(FBuffer, Len div 2);
end;

function TODBCParamDesc.ReadBytes(Len: integer): variant;
{$IFNDEF CLR}
var
  Ptr: IntPtr;
{$ENDIF}
begin
  Result := VarArrayCreate([0, Len - 1], varByte);
{$IFNDEF CLR}
  Ptr := VarArrayLock(Result);
  try
    CopyBuffer(FBuffer, Ptr, Len);
  finally
    VarArrayUnlock(Result);
  end;
{$ENDIF}
end;

{ TODBCSQLInfo }

constructor TODBCSQLInfo.Create(ParserClass: TSQLParserClass);
begin
  inherited Create(ParserClass);
  FConnection := nil;
end;

function TODBCSQLInfo.IdentCase: TIdentCase;
begin
  if FConnection <> nil then
    Result := FConnection.FIdentCase
  else
    Result := icMixed;
end;

{ TODBCCommand }

constructor TODBCCommand.Create;
begin
  inherited;

end;

destructor TODBCCommand.Destroy;
begin

  inherited;
end;

function TODBCCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommandTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TODBCCommand.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRowsProcessed:
      Value := FRowsAffected;
    prCommandTimeout:
      Value := FCommandTimeout;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TODBCCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TODBCConnection(Value);
  end;
end;

class function TODBCCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TODBCSQLInfo;
end;

class function TODBCCommand.GetParserClass: TSQLParserClass;
begin
  Result := TODBCParser;
end;

function TODBCCommand.GetParamDescType: TParamDescClass;
begin
  Result := TODBCParamDesc;
end;

procedure TODBCCommand.Prepare;
begin
  if FStmt <> nil then
    Exit;

  if FConnection = nil then
    raise Exception.Create(SConnectionNotDefined);

  Check(Cli.SQLAllocHandle(SQL_HANDLE_STMT, FConnection.FSQLHDbc, FStmt));
  if FCommandTimeout <> 0 then
    Check(Cli.SQLSetStmtAttrInt(FStmt, SQL_ATTR_QUERY_TIMEOUT, FCommandTimeout, 0));

  try
    InternalPrepare;
  except
    Cli.SQLFreeHandle(SQL_HANDLE_STMT, FStmt);
    FStmt := nil;
    raise;
  end;

  FCursorState := csPrepared;
end;

procedure TODBCCommand.Unprepare;
begin
  if FStmt = nil then
    Exit;

  Cli.SQLFreeHandle(SQL_HANDLE_STMT, FStmt);
  FStmt := nil;
  FCursorState := csInactive;
end;

function TODBCCommand.GetPrepared: boolean;
begin
  Result := FStmt <> nil;
end;

procedure TODBCCommand.Execute(Iters: integer = 1);
var
  NeedPrepare: boolean;
begin
  if GetCursorState > csPrepared then
    Exit;

  try
    NeedPrepare := not GetPrepared;
    if NeedPrepare then
      Prepare;

    try
      InternalExecute;
    finally
      if NeedPrepare then
        UnPrepare;
    end;

    if not FRecordSetExec and not NeedPrepare then
      Cli.SQLCloseCursor(FStmt); 

  except
    if not FRecordSetExec and Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;
  if not FRecordSetExec and Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TODBCCommand.InitProcParams(const Name: _string);
var
  RecordSet: TODBCMetaDataRecordSet;
  Info: TExtTableInfo;
  RecBuf: IntPtr;
  Param: TParamDesc;
  v: variant;
  s: _string;
  SQLParamType, SQLDataType: smallint;
  SQLLength: integer;
  SQLScale: smallint;
  DataType, SubDataType: word;
  Len, Scale: integer;
  Fixed: boolean;
begin
  SQLInfo.SplitObjectName(Name, Info);
  Info.Table := SQLInfo.NormalizeName(Info.Table, False, True);
  Info.Schema := SQLInfo.NormalizeName(Info.Schema, False, True);
  if Info.Schema = '' then
    Info.Schema := FConnection.GetCachedSchema;
  Info.Catalog := SQLInfo.NormalizeName(Info.Catalog, False, True);
  if Info.Catalog = '' then
    Info.Catalog := FConnection.GetCachedCatalog;  

  RecordSet := TODBCMetaDataRecordSet.Create;
  try
    RecordSet.SetConnection(FConnection);
    with TODBCMetaDataCommand(RecordSet.GetCommand) do begin
      FMetaDataKind := mkProcedureColumns;
      with FMetaDataArgs do begin
        CatalogName := Info.Catalog;
        SchemaName := Info.Schema;
        ObjectName := Info.Table;
        ColumnName := '%';
      end;
    end;

    RecordSet.Open;
    RecordSet.AllocRecBuf(RecBuf);
    try
      FParams.Clear;
      repeat
        RecordSet.GetNextRecord(RecBuf);
        if RecordSet.Eof then
          break;

        RecordSet.GetFieldAsVariant(5, RecBuf, v);
        SQLParamType := v;
        if SQLParamType = SQL_RESULT_COL then
          continue;
        if (SQLParamType = SQL_RETURN_VALUE) and not FConnection.IsReturnValueAllowed then
          continue;

        Param := AddParam;

        case SQLParamType of
          SQL_PARAM_TYPE_UNKNOWN:
            Param.SetParamType(pdUnknown);
          SQL_PARAM_INPUT:
            Param.SetParamType(pdInput);
          SQL_PARAM_INPUT_OUTPUT:
            Param.SetParamType(pdInputOutput);
          SQL_PARAM_OUTPUT:
            if FConnection.OutParamIsInOut then
              Param.SetParamType(pdInputOutput)
            else
              Param.SetParamType(pdOutput);
          SQL_RETURN_VALUE:
            Param.SetParamType(pdResult);
        end;

        RecordSet.GetFieldAsVariant(4, RecBuf, v);
        s := _VarToStr(v);
        if (s <> '') and (s[1] = '@') then
          Delete(s, 1, 1);
        Param.SetName(s);

        RecordSet.GetFieldAsVariant(6, RecBuf, v);
        SQLDataType := v;

        RecordSet.GetFieldAsVariant(8, RecBuf, v);
        SQLLength := v;

        RecordSet.GetFieldAsVariant(10, RecBuf, v);
        if VarIsNull(v) then
          SQLScale := 0
        else
          SQLScale := v;

        DetectDataType(SQLDataType, SQLLength, SQLScale, DataType,
          SubDataType, Len, Scale, Fixed, True, True, False);

        Param.SetDataType(DataType);
        Param.SetSubDataType(SubDataType);
        Param.SetSize(Len);
      until False;

    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
    RecordSet.Close;
  finally
    RecordSet.Free;
  end;
end;

function TODBCCommand.CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string;
var
  s, si: _string;
  f, i: integer;
begin
  if NeedDescribe then
    InitProcParams(Name);

  if (FParams.Count > 0) and (FParams[0].GetParamType = pdResult) then begin
    f := 1;
    s := '{call ' + ':' + FParams[0].GetName + ' = ' + Name + '(';
    si := '{call ? = ' + Name + '(';
  end
  else begin
    f := 0;
    s := '{call ' + Name + '(';
    si := s;
  end;

  for i := f to FParams.Count - 1 do begin
    if i > f then begin
      s := s + ', ';
      si := si + ', ';
    end;
    s := s + ':' + FParams[i].GetName;
    si := si + '?';
  end;

  s := s + ')}';
  si := si + ')}';

  FSQL := si;
  FUserSQL := s;
  Result := s;
end;

function TODBCCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TODBCCommand.SetCursorState(Value: TCursorState);
begin
  if Value <> FCursorState then begin
    if FCursorState = csFetching then
      Cli.SQLCloseCursor(FStmt);
    FCursorState := Value;
  end;
end;

procedure TODBCCommand.Check(ErrorCode: smallint);
begin
  if IsODBCError(ErrorCode) then
    FConnection.ProcessError(SQL_HANDLE_STMT, FStmt, ErrorCode, Component);
end;

procedure TODBCCommand.InternalPrepare;
var
  SQL: _string;
begin
  if Trim(FSQL) = '' then
    raise Exception.Create(SEmptySQLStatement);

  if not FConnection.IsCommentAllowed then
    SQL := RemoveComments(FSQL)
  else
    SQL := FSQL;

  Check(Cli.SQLPrepare(FStmt, _PChar(SQL), Length(SQL)));
end;

procedure TODBCCommand.InternalExecute;
var
  Res: smallint;
  ValueNInt: NativeInt;
begin
  BindParams;

  Res := Cli.SQLExecute(FStmt);
  if Res = SQL_NEED_DATA then
    WriteBlobs
  else
  if Res = SQL_NO_DATA then
    FRowsAffected := 0
  else
    Check(Res);

  if Res <> SQL_NO_DATA then begin
    Check(Cli.SQLRowCount(FStmt, ValueNInt));
    FRowsAffected := ValueNInt;
  end;

  ReadOutParams;
end;

function TODBCCommand.Cli: TODBCCli;
begin
  Result := FConnection.FCli;
end;

procedure TODBCCommand.BindParams;
var
  i, DataSize, Scale, ValPrec, ValScale, BufSize: integer;
  Param: TODBCParamDesc;
  ValPtr: IntPtr;
  DataAtExec: boolean;
  DataType: word;
  Blob: TBlob;
  Piece: PPieceHeader;
begin
  for i := 0 to Params.Count - 1 do begin
    Param := TODBCParamDesc(Params[i]);
    Param.AllocBuffer;
    if Param.ParamType in [pdUnknown, pdInput, pdInputOutput] then
      Param.WriteBuffer(Param.ParamType = pdInput, FConnection.IsEmptyStringAllowed,
        DataAtExec, ValPrec, ValScale)
    else begin
      DataAtExec := False;
      ValPrec := 0;
      ValScale := 0;
    end;

    {if (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) and
      (Param.ParamType in [pdOutput, pdResult])
    then
      continue;}

    DataType := Param.DataType;
    DataSize := Param.Size;
    Scale := 0;
    case Param.DataType of
      dtBCD: begin
        DataSize := 18;
        Scale := 4;
      end;
    {$IFNDEF FPC}
      dtFMTBCD: begin
        DataSize := ValPrec;
        Scale := ValScale;
      end;
    {$ENDIF}
      dtBlob, dtMemo, dtWideMemo:
        DataSize := MaxInt;
    end;
    SetSpecificParamPrec(Param, DataSize, Scale);

    BufSize := Param.FBufferSize;
    if DataAtExec then
      ValPtr := IntPtr(i)
    else
      ValPtr := Param.FBuffer;

    if DataType in [dtBlob, dtMemo, dtWideMemo] then begin
      Blob := Param.GetBlob;

      if (Blob <> nil) and ((DataType = dtMemo) or (DataType = dtWideMemo)) then begin
        if Blob.IsUnicode then
          DataType := dtWideMemo
        else
          DataType := dtMemo
      end;

      if not DataAtExec then begin
        if Param.ParamType = pdInput then begin
          if Blob <> nil then begin
            Blob.Defrag;
            Piece := Blob.FirstPiece;
            if IntPtr(Piece) <> nil then begin
              BufSize := Piece.Size;
              ValPtr := PtrOffset(Piece, SizeOf(TPieceHeader));
            end;
          end;
        end
        else begin
          if Blob = nil then
            raise Exception.Create(SCannotConvertType);

          if Param.Size <> 0 then
            BufSize := Param.Size
          else
            BufSize := DefaultParamSize;

          if DataType = dtWideMemo then
            BufSize := BufSize * 2;

          Blob.Defrag;
          Piece := Blob.FirstPiece;

          if IntPtr(Piece) = nil then begin
            Blob.AllocPiece(Piece, BufSize);
            Blob.AppendPiece(Piece);
          end
          else
          if Piece.Size < BufSize then
            Blob.ReallocPiece(Piece, BufSize);

          ValPtr := PtrOffset(Piece, SizeOf(TPieceHeader));
        end;
      end;
    end;

    Check(Cli.SQLBindParameter(FStmt, i + 1, GetODBCParamType(Param.ParamType),
      GetCDataType(DataType, Param.SubDataType),
      GetSQLDataType(DataType, Param.SubDataType),
      DataSize, Scale, ValPtr, BufSize, Param.FIndicator));
  end;
end;

procedure TODBCCommand.ReadOutParams;
var
  i: integer;
  Param: TODBCParamDesc;
begin
  for i := 0 to Params.Count - 1 do begin
    Param := TODBCParamDesc(Params[i]);
    if Param.ParamType <> pdInput then begin
      //if not (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) then
        Param.ReadBuffer
    end;
  end;
end;

function TODBCCommand.GetODBCParamType(ParamType: TParamDirection): smallint;
begin
  case ParamType of
    pdInput, pdUnknown:
      Result := SQL_PARAM_INPUT;
    pdInputOutput:
      Result := SQL_PARAM_INPUT_OUTPUT;
    pdOutput, pdResult:
      Result := SQL_PARAM_OUTPUT;
  else
    Assert(False);
    Result := 0;
  end;
end;

function TODBCCommand.GetCDataType(DataType, SubDataType: word): smallint;
begin
  case DataType of
    dtString, dtExtString, dtMemo:
      Result := SQL_C_CHAR;
    dtWideString, dtExtWideString, dtWideMemo:
      Result := SQL_C_WCHAR;
    dtBytes, dtVarBytes, dtExtVarBytes, dtBlob:
      Result := SQL_C_BINARY;
    dtInt8:
      Result := SQL_C_UTINYINT;
    dtWord:
      Result := SQL_C_USHORT;
    dtSmallint:
      Result := SQL_C_SSHORT;
    dtInteger:
      Result := SQL_C_SLONG;
    dtLargeint:
      Result := SQL_C_SBIGINT;
    dtFloat, dtCurrency:
      Result := SQL_C_DOUBLE;
    dtBoolean:
      Result := SQL_C_SSHORT;
    dtDate:
      Result := SQL_C_TYPE_DATE;
    dtTime:
      Result := SQL_C_TYPE_TIME;
    dtDateTime:
      Result := SQL_C_TYPE_TIMESTAMP;
    dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}:
      Result := SQL_C_CHAR;
    dtGuid:
      Result := SQL_C_CHAR;
  else
    Assert(False);
    Result := 0;
  end;
end;

function TODBCCommand.GetSQLDataType(DataType, SubDataType: word): smallint;
begin
  case DataType of
    dtString, dtExtString:
      Result := SQL_VARCHAR;
    dtMemo:
      Result := SQL_LONGVARCHAR;
    dtWideString, dtExtWideString:
      Result := SQL_WVARCHAR;
    dtWideMemo:
      Result := SQL_WLONGVARCHAR;
    dtBytes, dtVarBytes, dtExtVarBytes:
      Result := SQL_VARBINARY;
    dtBlob:
      Result := SQL_LONGVARBINARY;
    dtInt8, dtWord:
      Result := SQL_TINYINT;
    dtSmallint:
      Result := SQL_SMALLINT;
    dtInteger:
      Result := SQL_INTEGER;
    dtLargeint:
      Result := SQL_BIGINT;
    dtFloat, dtCurrency:
      Result := SQL_DOUBLE;
    dtBoolean:
      Result := SQL_BIT;
    dtDate:
      Result := SQL_TYPE_DATE;
    dtTime:
      Result := SQL_TYPE_TIME;
    dtDateTime:
      Result := SQL_TYPE_TIMESTAMP;
    dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}:
      Result := SQL_NUMERIC;
    dtGuid:
      Result := SQL_GUID;
  else
    Assert(False);
    Result := 0;
  end;
end;

procedure TODBCCommand.SetSpecificParamPrec(Param: TODBCParamDesc; var Prec, Scale: integer);
begin
end;

function TODBCCommand.GetBlobSize(SQLLength: longword): Integer;
begin
  if Integer(SQLLength) = MaxInt then
    Result := 0
  else
    Result := Integer(SQLLength);
end;

procedure TODBCCommand.DetectDataType(SQLDataType: smallint; SQLLength: longword; SQLScale: smallint;
  var DataType, SubDataType: word; var Length, Scale: integer;
  var Fixed: boolean; LongStrings, FlatBuffers, FieldsAsString: boolean);
const
  MaxBCDIntPrec = MaxBcdPrecision - MaxBcdScale;
var
  IntPrec: integer;
begin
  Fixed := False;
  Length := 0;
  Scale := 0;
  SubDataType := 0;

  if FieldsAsString then begin
    case SQLDataType of
      SQL_CHAR, SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CLOB,
      SQL_WCHAR, SQL_WVARCHAR, SQL_GRAPHIC, SQL_VARGRAPHIC,
      SQL_WLONGVARCHAR, SQL_LONGVARGRAPHIC, SQL_DBCLOB: ;
    else
      SQLDataType := SQL_VARCHAR;
      SQLLength := 255;
    end;
  end;

  case SQLDataType of
    SQL_CHAR, SQL_VARCHAR: begin
      if (SQLLength < 255) or
        (LongStrings and (SQLLength <= DefaultStringFieldLength))
      then begin
        if (SQLLength >= FlatBufferLimit) and not FlatBuffers then
          DataType := dtExtString
        else
          DataType := dtString;
      end
      else
        DataType := dtMemo;

      Fixed := SQLDataType = SQL_CHAR;
      Length := SQLLength;
    end;
    SQL_LONGVARCHAR:
      DataType := dtMemo;
    SQL_CLOB: begin
      DataType := dtMemo;
      SubDataType := dtClobLocator;
    end;
    SQL_WCHAR, SQL_WVARCHAR, SQL_GRAPHIC, SQL_VARGRAPHIC: begin
      if (SQLLength < 255) or
        (LongStrings and (SQLLength <= DefaultStringFieldLength))
      then begin
        if (SQLLength >= FlatBufferLimit) and not FlatBuffers then
          DataType := dtExtWideString
        else
          DataType := dtWideString;
      end
      else
        DataType := dtWideMemo;

      Fixed := (SQLDataType = SQL_WCHAR) or (SQLDataType = SQL_GRAPHIC);
      Length := SQLLength;
    end;
    SQL_WLONGVARCHAR, SQL_LONGVARGRAPHIC:
      DataType := dtWideMemo;
    SQL_DBCLOB: begin
      DataType := dtWideMemo;
      SubDataType := dtDBClobLocator;
    end;
    SQL_DECIMAL, SQL_NUMERIC: begin
    {$IFNDEF FPC}
      if EnableFMTBCD or FConnection.EnableFMTBCD then begin
        DataType := dtFMTBCD;
        if (SQLLength > MaxFMTBcdDigits) or (SQLLength <= 0) then
          Length := MaxFMTBcdDigits
        else
          Length := SQLLength;
        if Word(SQLScale) > Length then // if length was reduced
          Scale := Length
        else
          Scale := SQLScale;
      end
      else
    {$ENDIF}
      if EnableBCD or FConnection.EnableBCD then begin
        DataType := dtBCD;
        if SQLScale > MaxBcdScale then
          Scale := MaxBcdScale
        else
          Scale := SQLScale;

        IntPrec := Integer(SQLLength) - SQLScale;
        if IntPrec > MaxBCDIntPrec then
          Length := MaxBCDIntPrec + Scale
        else
          Length := IntPrec + Scale;
      end
      else
        DataType := dtFloat;
    end;
    SQL_SMALLINT:
      DataType := dtSmallint;
    SQL_INTEGER:
      DataType := dtInteger;
    SQL_REAL, SQL_FLOAT, SQL_DOUBLE:
      DataType := dtFloat;
    SQL_BIT:
      DataType := dtBoolean;
    SQL_TINYINT:
      DataType := dtWord;
    SQL_BIGINT:
      DataType := dtLargeint;
    SQL_BINARY: begin
      DataType := dtBytes;
      Length := SQLLength;
    end;
    SQL_VARBINARY, SQL_LONGVARBINARY: begin
      if SQLLength <= DefaultStringFieldLength
      then begin
        if (SQLLength >= FlatBufferLimit) and not FlatBuffers then
          DataType := dtExtVarBytes
        else
          DataType := dtVarBytes;
      end
      else
        DataType := dtBlob;

      if SQLDataType = SQL_LONGVARBINARY then
        Length := GetBlobSize(SQLLength)
      else
        Length := SQLLength;
    end;
    SQL_BLOB: begin
      DataType := dtBlob;
      SubDataType := dtBlobLocator;
    end;
    SQL_TYPE_DATE, SQL_DATE:
      DataType := dtDate;
    SQL_TYPE_TIME, SQL_TIME:
      DataType := dtTime;
    SQL_TYPE_TIMESTAMP, SQL_TIMESTAMP:
      DataType := dtDateTime;
    SQL_GUID: begin
      DataType := dtGuid;
      Length := GuidStrSize;
    end;
  else
    raise Exception.Create(SDataTypeNotSupported);
  end;

  if FConnection.FUseUnicode then
    case DataType of
      dtString:
        DataType := dtWideString;
      dtExtString:
        DataType := dtExtWideString;
      dtMemo:
        DataType := dtWideMemo;
    end;
end;

procedure TODBCCommand.WriteBlobs;
var
  i, Res: integer;
  Param: TODBCParamDesc;
  ValPtr: IntPtr;
begin
  repeat
    Res := Cli.SQLParamData(FStmt, ValPtr);
    if Res <> SQL_NEED_DATA then begin
      Check(Res);
      exit;
    end;

    i := Integer(ValPtr);
    Param := TODBCParamDesc(Params[i]);
    WriteBlob(Param.Value);
  until False;
end;

procedure TODBCCommand.WriteBlob(const Value: variant);
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
      if not (Obj is TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      Piece := Blob.FirstPiece;
      while IntPtr(Piece) <> nil do begin
        BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
        Check(Cli.SQLPutData(FStmt, BlobData, Piece.Used));
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
        Check(Cli.SQLPutData(FStmt, BlobData, hb - lb + 1));
      finally
        VarArrayUnlock(Value);
      end;
    end;
  {$ENDIF}
  else
    s := AnsiString(VarToStr(Value));
    BlobData := Marshal.StringToHGlobalAnsi(s);
    try
      Check(Cli.SQLPutData(FStmt, BlobData, Length(s)));
    finally
      FreeString(BlobData);
    end;
  end;
end;

function TODBCCommand.ReadBlob(Blob: TBlob; FieldNo: integer; DataType: word;
  var SharedPiece: PPieceHeader): boolean;
var
  Res: smallint;
  Len: integer;
  ValueNInt: NativeInt;
  Piece, Piece2: PPieceHeader;
  NullTerminatorSize: cardinal;
begin
  case GetCDataType(DataType, 0) of
    SQL_C_CHAR:
      NullTerminatorSize := 1;
    SQL_C_WCHAR:
      NullTerminatorSize := 2;
    else
      NullTerminatorSize := 0;
  end;

  if IntPtr(SharedPiece) = nil then
    Blob.AllocPiece(Piece, Blob.PieceSize)
  else
    Piece := SharedPiece;

  Result := True;
  repeat
    Res := Cli.SQLGetData(FStmt, FieldNo, GetCDataType(DataType, 0),
      PtrOffset(Piece, SizeOf(TPieceHeader)), Blob.PieceSize, ValueNInt);
    Len := ValueNInt;

    if Res = SQL_NO_DATA then begin
      if IntPtr(Piece) <> IntPtr(SharedPiece) then
        Marshal.FreeHGlobal(Piece);
      break;
    end;

    if IsODBCError(Res) then begin
      if IntPtr(Piece) <> IntPtr(SharedPiece) then
        Marshal.FreeHGlobal(Piece);
      Check(Res);
    end;

    if (Len = SQL_NULL_DATA) or (Len = 0) then begin
      if IntPtr(Piece) <> IntPtr(SharedPiece) then
        Marshal.FreeHGlobal(Piece);
      Result := False;
      break;
    end;

    // for SQL_C_CHAR and  SQL_C_WCHAR
    // Len = Length(string) for last part of text
    // Len = Length(string) + NullTerminatorSize for previous parts of text
    if Len >= Piece.Size then
      Piece.Used := Piece.Size - Integer(NullTerminatorSize)
    else
      Piece.Used := Len;

    if Piece = SharedPiece then begin
      if Piece.Used < Piece.Size div 2 then begin
        Blob.AllocPiece(Piece2, Piece.Used);
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
        Blob.CompressPiece(Piece);

    Blob.AppendPiece(Piece);

    if Res = SQL_SUCCESS then
      break
    else
      Blob.AllocPiece(Piece, Blob.PieceSize);
  until False;
end;

function TODBCCommand.RemoveComments(const Value: _string): _string;
var
  Parser: TSQLParser;
  Lexem: _string;
begin
  Parser := GetParserClass.Create(Value);
  try
    Parser.OmitBlank := False;
    Parser.OmitComment := True;
    Parser.Uppered := False;
    Parser.QuotedString := True;

    Result := '';
    while Parser.GetNext(Lexem) <> lcEnd do
      Result := Result + Lexem;
  finally
    Parser.Free;
  end;
end;

{ TODBCRecordSet }

constructor TODBCRecordSet.Create;
begin
  inherited Create;

  FFetchAll := True;
  FFetchRows := 25;
  FRowsFetchedBuf := Marshal.AllocHGlobal(SizeOf(IntPtr));
end;

destructor TODBCRecordSet.Destroy;
begin
  Close;
  Marshal.FreeHGlobal(FRowsFetchedBuf);
  FreeFetchBlock;

  inherited;
end;

function TODBCRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommand.FCommandTimeout := Value;
    prFieldsAsString:
      FFieldsAsString := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TODBCRecordSet.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      Value := FCommand.FCommandTimeout;
    prFieldsAsString:
      Value := FFieldsAsString;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TODBCRecordSet.ExecCommand;
var
  NeedPrepare: boolean;
begin
  FCommand.FRecordSetExec := True;
  try
    NeedPrepare := not Prepared;
    if NeedPrepare then
      InternalPrepare;

    try
      inherited;

      // for stored procedures
      if not FCommand.FConnection.FDetectFieldsOnPrepare or (CommandType = ctStatement) then begin
        DetectCommandType;
        // sometimes without SQLMoreResults ODBC returns zero columns count for select queries
        if CommandType = ctStatement then begin
          Cli.SQLMoreResults(FCommand.FStmt);
          DetectCommandType;
        end;
      end;

      if CommandType = ctCursor then
        FCommand.SetCursorState(csExecuted)

    except
      if NeedPrepare then
        InternalUnprepare;
      raise;
    end;

    if CommandType <> ctCursor then
      if NeedPrepare then
        InternalUnprepare;

  except
    FCommand.FRecordSetExec := False;
    if Assigned(FCommand.AfterExecute) then
      FCommand.AfterExecute(False);
    raise;
  end;
  FCommand.FRecordSetExec := False;
  if Assigned(FCommand.AfterExecute) then
    FCommand.AfterExecute(True);
end;

function TODBCRecordSet.IsFullReopen: boolean;
begin
  Result := False;
end;

procedure TODBCRecordSet.Reopen;
begin
  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  FreeData;
  InitData;
  if Assigned(FOnDataChanged) then
    // perform dataset resync to prevent AV if grid is repainted in BeforeFetch/AfterFetch events
    FOnDataChanged;

  InternalOpen(True);
end;

procedure TODBCRecordSet.SetToEnd;
begin
  FetchAll;

  inherited;
end;

function TODBCRecordSet.GetNull(FieldNo: word; RecBuf: IntPtr): boolean;
begin
  Result := Marshal.ReadInt32(RecBuf, DataSize + (FieldNo - 1) * SizeOf(IntPtr)) = SQL_NULL_DATA;
  if Result then
    Result := GetNullByBlob(FieldNo, RecBuf);
end;

procedure TODBCRecordSet.SetNull(FieldNo: word; RecBuf: IntPtr; Value: boolean);
var
  Flag: integer;
  Field: TFieldDesc;
  Blob: TBlob;
begin
  Field := Fields[FieldNo - 1];
  if Value then
    Flag := SQL_NULL_DATA
  else
    Flag := 0;

  Marshal.WriteIntPtr(RecBuf, DataSize + (FieldNo - 1) * SizeOf(IntPtr), IntPtr(Flag));

  if Value and IsBlobFieldType(Field.DataType) then begin // clear Blob value
    Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
    if Blob <> nil then
      Blob.Clear;
  end;
end;

procedure TODBCRecordSet.Check(ErrorCode: smallint);
begin
  if IsODBCError(ErrorCode) then
    FCommand.FConnection.ProcessError(SQL_HANDLE_STMT, FCommand.FStmt, ErrorCode, Component);
end;

procedure TODBCRecordSet.CreateCommand;
begin
  SetCommand(TODBCCommand.Create);
end;

procedure TODBCRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TODBCCommand(Value);
end;

procedure TODBCRecordSet.DetectCommandType;
var
  ColumnCount: smallint;
begin
  ColumnCount := 0;
  Check(Cli.SQLNumResultCols(FCommand.FStmt, ColumnCount));
  if ColumnCount > 0 then
    CommandType := ctCursor
  else
    CommandType := ctStatement;
end;

procedure TODBCRecordSet.InternalPrepare;
begin
  inherited;

  if FCommand.FConnection.FDetectFieldsOnPrepare then
    DetectCommandType;
end;

procedure TODBCRecordSet.InternalUnPrepare;
begin
  inherited;

  CommandType := ctUnknown;
end;

function TODBCRecordSet.Cli: TODBCCli;
begin
  Result := FCommand.FConnection.FCli;
end;

procedure TODBCRecordSet.DescribeFieldDesc(Field: TCRFieldDesc; Index: integer);
const
  BufSize = 256;
var
  Buf: IntPtr;
  NameSize, SQLDataType, SQLScale, Nullable: smallint;
  ValueUNInt: NativeUInt;
  ValueNInt: NativeInt;
  SQLLength: longword;
  DataType, SubDataType: word;
  Len, Scale: integer;
  Fixed: boolean;
begin
  Field.FieldNo := Index + 1;
  Field.ActualFieldNo := Index + 1;

  Buf := Marshal.AllocHGlobal(BufSize * SizeOf(_char));
  try
    Check(Cli.SQLDescribeCol(FCommand.FStmt, Index + 1, Buf, BufSize, NameSize,
      SQLDataType, ValueUNInt, SQLScale, Nullable));
    SQLLength := ValueUNInt;

    Field.Name := PtrToXString(Buf);
    Field.ActualName := Field.Name;
  finally
    Marshal.FreeHGlobal(Buf);
  end;

  Field.Required := Nullable = SQL_NO_NULLS;

  Check(Cli.SQLColAttributeInt(FCommand.FStmt, Index + 1, SQL_DESC_AUTO_UNIQUE_VALUE,
    nil, 0, NameSize, ValueNInt));

  if ValueNInt = SQL_TRUE then begin
    Field.IsAutoIncrement := True;
    Field.Required := False;
  end;

  Check(Cli.SQLColAttributeInt(FCommand.FStmt, Index + 1, SQL_DESC_UPDATABLE,
    nil, 0, NameSize, ValueNInt));

  if ValueNInt = SQL_ATTR_READONLY then
    Field.ReadOnly := True;

  FCommand.DetectDataType(SQLDataType, SQLLength, SQLScale,
    DataType, SubDataType, Len, Scale, Fixed, FLongStrings, FFlatBuffers,
    FFieldsAsString);

  Field.DataType := DataType;
  Field.SubDataType := SubDataType;
  Field.Length := Len;
  Field.Scale := Scale;
  Field.Fixed := Fixed;

  case Field.DataType of
    dtString, dtGuid:
      Field.Size := Field.Length + 1;
    dtWideString:
      Field.Size := (Field.Length + 1) * 2;
    dtVarBytes:
      Field.Size := Field.Length + 2;
    dtBytes:
      Field.Size := Field.Length;
    dtInt8:
      Field.Size := SizeOf(Byte);
    dtSmallInt, dtWord:
      Field.Size := SizeOf(SmallInt);
    dtInteger:
      Field.Size := SizeOf(Integer);
    dtLargeInt:
      Field.Size := SizeOf(Int64);
    dtBoolean:
      Field.Size := sizeOf(WordBool);
    dtFloat, dtCurrency, dtBCD:
      Field.Size := SizeOf(Double);
  {$IFNDEF FPC}
    dtFMTBCD:
      Field.Size := SizeOf(TBcd);
  {$ENDIF}
    dtDate, dtTime, dtDateTime:
      Field.Size := SizeOf(TDateTime);
  else
    Field.Size := SizeOf(IntPtr);
  end;
end;

procedure TODBCRecordSet.InternalInitFields;
var
  OldCursorState: TCursorState;
  Field: TCRFieldDesc;
  FieldsCount: smallint;
  i: integer;
begin
  OldCursorState := FCommand.GetCursorState;
  if OldCursorState = csInactive then
    InternalPrepare;

  try
    if CommandType <> ctCursor then
      raise Exception.Create(SNotRows);

    TablesInfo.CaseSensitive := FCommand.SQLInfo.IdentCase <> icMixed;
    FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, TablesInfo);

    FHasConvertedFields := False;
    Check(Cli.SQLNumResultCols(FCommand.FStmt, FieldsCount));
    for i := 0 to FieldsCount - 1 do begin
      Field := TCRFieldDesc(GetFieldDescType.Create);
      DescribeFieldDesc(Field, i);

      FHasConvertedFields := FHasConvertedFields or IsConvertedFieldType(Field.DataType);
      FFields.Add(Field);
    end;

  finally
    if OldCursorState = csInactive then
      InternalUnPrepare;
  end;
end;

procedure TODBCRecordSet.InternalClose;
begin
  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  inherited;
  FreeFetchBlock;

  if not Prepared then
    InternalUnprepare;
end;

function TODBCRecordSet.Fetch(FetchBack: boolean = False): boolean;
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

  if (FCommand.GetCursorState = csFetched) and not Prepared then begin
    OldCommandType := CommandType;
    InternalUnPrepare;
    // We need to save old CommandType to save old FieldDescs on Refresh.
    CommandType := OldCommandType;
  end;
end;

function TODBCRecordSet.GetIndicatorSize: word;
begin
  Result := FieldCount * SizeOf(IntPtr);
end;

function TODBCRecordSet.InternalFetch: boolean;
var
  pHBlock: PBlockHeader;
  NewBlock: Boolean;
  OldFirstItem, OldLastItem, Item, RecBuf: IntPtr;
  Rows: integer;
  Res: smallint;
  SharedPiece: PPieceHeader;
begin
  NewBlock := (IntPtr(BlockMan.FirstBlock) = nil) or not FUnidirectional;

  if NewBlock then begin
    BlockMan.AllocBlock(pHBlock, FFetchRows);
    if HasComplexFields or FHasConvertedFields then begin
      if IntPtr(FFetchBlock) = nil then
        AllocFetchBlock;
      InitBlock(pHBlock);
    end;
  end
  else begin
    phBlock := BlockMan.FirstBlock; // overwrite data in unidirectional mode
    ClearBlock(pHBlock, 0);
    InitBlock(pHBlock);
  end;

  OldFirstItem := FirstItem;
  OldLastItem := LastItem;
  try
    Item := PtrOffset(phBlock, SizeOf(TBlockHeader));

    Rows := -1;

    if (FFetchRows > 1) and
        FCommand.FConnection.IsBlockFetchAllowed and
        not HasComplexFields and
        not FHasConvertedFields
    then
      // some drivers for ODBC ver. 3 don't support Block Fetch
      // in this case BindColumnsForBlockFetch returns error
      if not IsODBCError(BindColumnsForBlockFetch) then begin
        RecBuf := PtrOffset(Item, SizeOf(TItemHeader));
        BindColumns(RecBuf, FFetchRows);
        Res := Cli.SQLFetch(FCommand.FStmt);
        if Res = SQL_NO_DATA then
          Rows := 0
        else begin
          Check(Res);
          Rows := Marshal.ReadInt32(FRowsFetchedBuf);
        end;
      end;

    // if Row = -1 then no records was fetched in the Block Fetch
    // in this case use standard Fetch
    if Rows = -1 then begin
      Rows := 0;
      SharedPiece := nil;
      while True do begin
        RecBuf := PtrOffset(Item, SizeOf(TItemHeader));
        BindColumns(RecBuf, 1);
        Res := Cli.SQLFetch(FCommand.FStmt);
        if Res = SQL_NO_DATA then begin
          break;
        end
        else
          Check(Res);

        Inc(Rows);
        ReadFetchBlock(RecBuf, SharedPiece);

        if Rows = FFetchRows then
          break;
        Item := PtrOffset(Item, RecordSize + SizeOf(TItemHeader));
      end;
      if IntPtr(SharedPiece) <> nil then
        Marshal.FreeHGlobal(SharedPiece);
    end;

    if FCommand.GetCursorState < csFetching then
      FCommand.SetCursorState(csFetching);

    if Rows < FFetchRows then begin
      FCommand.SetCursorState(csFetched);
      FreeFetchBlock;
    end;

    ClearBlock(phBlock, Rows);
    if Rows > 0 then begin
      CreateBlockStruct(phBlock, Rows);
      FRowsFetched := RecordCount;
    end
    else
      BlockMan.FreeBlock(phBlock);

    Result := Rows > 0;
  except
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
    raise;
  end;
end;

function TODBCRecordSet.IsConvertedFieldType(DataType: word): boolean;
begin
  case DataType of
    dtVarBytes, dtDate, dtTime, dtDateTime, dtGuid,
    dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
      Result := True;
  else
    Result := False;  
  end;
end;

function TODBCRecordSet.GetFieldFetchBlockSize(Field: TFieldDesc): integer;
begin
  case Field.DataType of
   dtBlob, dtMemo, dtWideMemo:
     if Field.Length = 0 then begin
       Result := 0;
       exit;
     end;
  end;

  case Field.DataType of
    dtExtString, dtMemo:
      Result := Field.Length + 1;
    dtExtWideString, dtWideMemo:
      Result := (Field.Length + 1) * 2;
    dtVarBytes, dtExtVarBytes, dtBlob:
      Result := Field.Length;
    dtDate:
      Result := SizeOf(TSQLDateStruct);
    dtTime:
      Result := SizeOf(TSQLTimeStruct);
    dtDateTime:
      Result := SizeOf(TSQLTimeStampStruct);
    dtBCD{$IFDEF VER6P}{$IFNDEF FPC}, dtFMTBCD{$ENDIF}{$ENDIF}:
      Result := BCDStrSize + 1;
    dtGuid:
      Result := GuidStrSize + 1;
  else
    Result := 0;
  end;
end;

function TODBCRecordSet.GetFieldBindSize(Field: TFieldDesc): integer;
begin
  case Field.DataType of
   dtBlob, dtMemo, dtWideMemo:
     if Field.Length = 0 then begin
       Result := -1;
       exit;
     end;
  end;

  case Field.DataType of
    dtString, dtExtString, dtMemo:
      Result := Field.Length + 1;
    dtWideString, dtExtWideString, dtWideMemo:
      Result := (Field.Length + 1) * 2;
    dtBytes, dtVarBytes, dtExtVarBytes, dtBlob:
      Result := Field.Length;
    dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}:
      Result := BCDStrSize + 1;
    dtGuid:
      Result := GuidStrSize + 1;
  else
    Result := 0;
  end;
end;

procedure TODBCRecordSet.AllocFetchBlock;
var
  i: integer;
  Field: TFieldDesc;
begin
  if FFetchBlock <> nil then
    FreeFetchBlock;

  FFetchBlockItemSize := 0;
  for i := 0 to FieldCount - 1 do begin
    Field := Fields[i];
    FFetchBlockItemSize := FFetchBlockItemSize + GetFieldFetchBlockSize(Field);
  end;

  if FFetchBlockItemSize > 0 then
    FFetchBlock := Marshal.AllocHGlobal(FFetchBlockItemSize);
end;

procedure TODBCRecordSet.FreeFetchBlock;
begin
  if FFetchBlock <> nil then
    Marshal.FreeHGlobal(FFetchBlock);
  FFetchBlock := nil;
  FFetchBlockItemSize := 0;
end;

procedure TODBCRecordSet.InitBlock(pHBlock: IntPtr);
var
  i: integer;
  Item, RecBuf: IntPtr;
begin
  if not HasComplexFields then
    Exit;

  Item := PtrOffset(pHBlock, SizeOf(TBlockHeader));
  for i := 0 to FFetchRows - 1 do begin
    RecBuf := PtrOffset(Item, sizeof(TItemHeader));
    CreateComplexFields(RecBuf, True);
    Item := PtrOffset(Item, RecordSize + sizeof(TItemHeader));
  end;
end;

procedure TODBCRecordSet.ClearBlock(pHBlock: IntPtr; FromRow: integer);
var
  i, ItemSize: integer;
begin
  if HasComplexFields then begin
    ItemSize := RecordSize + sizeof(TItemHeader);
    for i := FromRow to FFetchRows - 1 do
      FreeComplexFields(PtrOffset(pHBlock, sizeof(TBlockHeader) + i * ItemSize + sizeof(TItemHeader)), True);
  end;
end;

procedure TODBCRecordSet.ReadFetchBlock(RecBuf: IntPtr; var SharedPiece: PPieceHeader);
var
  i, ComplexFieldOffset, Len: integer;
  Field: TFieldDesc;
  Source, Dest: IntPtr;
begin
  ComplexFieldOffset := 0;
  for i := 0 to Fields.Count - 1 do begin
    Field := Fields[i];
    if Field.FieldDescKind <> fdkData then
      continue;

    Len := Marshal.ReadInt32(RecBuf, DataSize + i * SizeOf(IntPtr));
    if Len <> SQL_NULL_DATA then begin
      Source := PtrOffset(FFetchBlock, ComplexFieldOffset);
      Dest := PtrOffset(RecBuf, Field.Offset);
      if not ReadFetchBlockField(Field, Source, Dest, Len, SharedPiece) then
        Marshal.WriteIntPtr(RecBuf, DataSize + i * SizeOf(IntPtr), IntPtr(SQL_NULL_DATA));
    end;
    ComplexFieldOffset := ComplexFieldOffset + GetFieldFetchBlockSize(Field);
  end;
end;

function TODBCRecordSet.ReadFetchBlockField(Field: TFieldDesc; Source, Dest: IntPtr;
  Len: integer; var SharedPiece: PPieceHeader): boolean;
var
  Buf: IntPtr;
  Blob: TBlob;
  Piece: PPieceHeader;
  Date: TDateTime;
  DateRec: TSQLDateStruct;
  TimeRec: TSQLTimeStruct;
{$IFNDEF CLR}
  TsRec: TSQLTimeStampStruct;
{$ENDIF}
  Str: string;
  Cur: currency;
  i64: int64;
  AStr: AnsiString;
{$IFNDEF FPC}
  Bcd: TBcd;
{$IFDEF CLR}
  Bytes: TBytes;
{$ENDIF}
{$ENDIF}
begin
  Result := True;
  case Field.DataType of
    dtExtString:
      Marshal.WriteIntPtr(Dest, StringHeap.AllocStr(Source, Field.Fixed and TrimFixedChar, Len));
    dtExtWideString:
      Marshal.WriteIntPtr(Dest, StringHeap.AllocWideStr(Source, Field.Fixed and TrimFixedChar, Len div 2));
    dtMemo: begin
      if Field.Length = 0 then begin
        if IntPtr(SharedPiece) = nil then
          TBlob.AllocPiece(SharedPiece, DefaultPieceSize);
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
        Result := FCommand.ReadBlob(Blob, Field.FieldNo, Field.DataType, SharedPiece);
      end
      else
      if Len > 0 then begin
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
        Blob.AllocPiece(Piece, Len);
        Blob.AppendPiece(Piece);
        CopyBuffer(Source, PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader)), Len);
        Blob.FirstPiece.Used := Len;
      end;
    end;
    dtWideMemo: begin
      if Field.Length = 0 then begin
        if IntPtr(SharedPiece) = nil then
          TBlob.AllocPiece(SharedPiece, DefaultPieceSize);
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
        Result := FCommand.ReadBlob(Blob, Field.FieldNo, Field.DataType, SharedPiece);
      end
      else
      if Len > 0 then begin
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
        Blob.AllocPiece(Piece, Len);
        Blob.AppendPiece(Piece);
        CopyBuffer(Source, PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader)), Len);
        Blob.FirstPiece.Used := Len;
      end;
    end;
    dtVarBytes: begin
      Marshal.WriteInt16(Dest, Word(Len));
      CopyBuffer(Source, PtrOffset(Dest, SizeOf(Word)), Len);
    end;
    dtExtVarBytes: begin
      Buf := StringHeap.NewBuf(Len + SizeOf(Word));
      Marshal.WriteIntPtr(Dest, Buf);
      Marshal.WriteInt16(Buf, Word(Len));
      CopyBuffer(Source, PtrOffset(Buf, SizeOf(Word)), Len);
    end;
    dtBlob:
      if Field.Length = 0 then begin
        if IntPtr(SharedPiece) = nil then
          TBlob.AllocPiece(SharedPiece, DefaultPieceSize);
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
        Result := FCommand.ReadBlob(Blob, Field.FieldNo, Field.DataType, SharedPiece);
      end
      else
      if Len > 0 then begin
        Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
        Blob.AllocPiece(Piece, Len);
        Blob.AppendPiece(Piece);
        CopyBuffer(Source, PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader)), Len);
        Blob.FirstPiece.Used := Len;
      end;
    dtDate: begin
    {$IFNDEF CLR}
      DateRec := TSQLDateStruct(Source^);
    {$ENDIF}
      if not TryEncodeDate(DateRec.Year, DateRec.Month, DateRec.Day, Date) then
        Date := MinDateTime;
      Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
    end;
    dtTime: begin
    {$IFNDEF CLR}
      TimeRec := TSQLTimeStruct(Source^);
    {$ENDIF}
      if not TryEncodeTime(TimeRec.Hour, TimeRec.Minute, TimeRec.Second, 0, Date) then
        Date := 0;
      Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
    end;
    dtDateTime: begin
    {$IFNDEF CLR}
      TsRec := TSQLTimeStampStruct(Source^);
      if not TryEncodeDateTime(TsRec.Year, TsRec.Month, TsRec.Day,
        TsRec.Hour, TsRec.Minute, TsRec.Second, TsRec.Fraction div 1000000, Date)
      then
    {$ENDIF}
        Date := MinDateTime;
      Marshal.WriteInt64(Dest, BitConverter.DoubleToInt64Bits(Date));
    end;
    dtBCD: begin
      Str := string(Marshal.PtrToStringAnsi(Source));
      if Str <> '' then begin
        if DecimalSeparator <> '.' then
          Str := StringReplace(Str, '.', DecimalSeparator, []);
        if DecimalSeparator <> LocaleDecSeparator then
          Str := StringReplace(Str, LocaleDecSeparator, DecimalSeparator, []);
        Cur := StrToCurrDef(Str, 0);
      {$IFNDEF CLR}
        i64 := PInt64(@Cur)^;
      {$ELSE}
        i64 := Decimal.ToOACurrency(Cur);
      {$ENDIF}
        Marshal.WriteInt64(Dest, i64);
      end
      else
        Result := False;
    end;
  {$IFNDEF FPC}
    dtFMTBCD: begin
      Str := string(Marshal.PtrToStringAnsi(Source));
      if Str <> '' then begin
        if DecimalSeparator <> '.' then
          Str := StringReplace(Str, '.', DecimalSeparator, []);
        if DecimalSeparator <> LocaleDecSeparator then
          Str := StringReplace(Str, LocaleDecSeparator, DecimalSeparator, []);
        if not TryStrToBcd(Str, Bcd) then begin
          Bcd := NullBcd;
        {$IFNDEF CLR}
          Bcd.Precision := 1;
        {$ENDIF}
        end;
      {$IFNDEF CLR}
        PBcd(Dest)^ := Bcd;
      {$ELSE}
        Bytes := TBcd.ToBytes(Bcd);
        Marshal.Copy(Bytes, 0, Dest, SizeOfTBcd);
      {$ENDIF}
      end
      else
        Result := False;
    end;
  {$ENDIF}
    dtGuid: begin
      AStr := Marshal.PtrToStringAnsi(Source);
      if (AStr <> '') and (AStr[1] <> '{') then
        AStr := '{' + AStr + '}';
      Len := Length(AStr);
      if Len > GuidStrSize then
        Len := GuidStrSize;
      Marshal.Copy(TBytes(AStr), 0, Dest, Len);
      Marshal.WriteByte(Dest, Len, 0);
    end;
  end;
end;

function TODBCRecordSet.BindColumnsForBlockFetch: Smallint;
begin
  Result := Cli.SQLSetStmtAttrInt(FCommand.FStmt, SQL_ATTR_ROW_BIND_TYPE, RecordSize + SizeOf(TItemHeader), 0);
  if IsODBCError(Result) then
    exit;
  Result := Cli.SQLSetStmtAttrPtr(FCommand.FStmt, SQL_ATTR_ROWS_FETCHED_PTR, FRowsFetchedBuf, 0);
end;

procedure TODBCRecordSet.BindColumns(RecBuf: IntPtr; FetchRows: integer);
var
  i, Len, ComplexFieldOffset: integer;
  Field: TFieldDesc;
  CDataType: smallint;
  ValueBuf, Ind: IntPtr;
begin
  Check(Cli.SQLSetStmtAttrInt(FCommand.FStmt, SQL_ATTR_ROW_ARRAY_SIZE, FetchRows, 0));

  ComplexFieldOffset := 0;
  for i := 0 to Fields.Count - 1 do begin
    Field := Fields[i];
    if Field.FieldDescKind <> fdkData then
      continue;

    Len := GetFieldBindSize(Field);
    if Len = -1 then begin
      Ind := PtrOffset(RecBuf, DataSize + i * SizeOf(IntPtr));
      Marshal.WriteIntPtr(Ind, nil);
      continue;
    end;

    CDataType := FCommand.GetCDataType(Field.DataType, Field.SubDataType);

    if IsComplexFieldType(Field.DataType) or IsConvertedFieldType(Field.DataType) then begin
      ValueBuf := PtrOffset(FFetchBlock, ComplexFieldOffset);
      ComplexFieldOffset := ComplexFieldOffset + GetFieldFetchBlockSize(Field);
    end
    else
      ValueBuf := PtrOffset(RecBuf, Field.Offset);
    Ind := PtrOffset(RecBuf, DataSize + i * SizeOf(IntPtr));
    Check(Cli.SQLBindCol(FCommand.FStmt, Field.FieldNo, CDataType, ValueBuf, Len, Ind));
  end;
end;

{ TODBCMetaDataCommand }

constructor TODBCMetaDataCommand.Create;
begin
  inherited;

  FMetaDataArgs := TMetaDataArgs.Create;
end;

destructor TODBCMetaDataCommand.Destroy;
begin
  FMetaDataArgs.Free;

  inherited;
end;

procedure TODBCMetaDataCommand.InternalPrepare;
begin
  //Check(Cli.SQLSetStmtAttrInt(FStmt, SQL_ATTR_METADATA_ID, SQL_TRUE, 0));
end;

procedure TODBCMetaDataCommand.InternalExecute;
var
  Catalog, Schema: _PChar;
begin
  with FMetaDataArgs do begin
    if CatalogName <> '' then
      Catalog := _PChar(CatalogName)
    else
      Catalog := nil;
    if SchemaName <> '' then
      Schema := _PChar(SchemaName)
    else
      Schema := nil;

    case FMetaDataKind of
      mkTables:
        Check(Cli.SQLTables(FStmt, Catalog, Length(CatalogName),
          Schema, Length(SchemaName),
          _PChar(ObjectName), Length(ObjectName),
          _PChar(ObjectType), Length(ObjectType)));
      mkColumns:
        Check(Cli.SQLColumns(FStmt, Catalog, Length(CatalogName),
          Schema, Length(SchemaName),
          _PChar(ObjectName), Length(ObjectName),
          _PChar(ColumnName), Length(ColumnName)));
      mkProcedures:
        Check(Cli.SQLProcedures(FStmt, Catalog, Length(CatalogName),
          Schema, Length(SchemaName),
          _PChar(ObjectName), Length(ObjectName)));
      mkProcedureColumns:
        Check(Cli.SQLProcedureColumns(FStmt, Catalog, Length(CatalogName),
          Schema, Length(SchemaName),
          _PChar(ObjectName), Length(ObjectName),
          _PChar(ColumnName), Length(ColumnName)));
      mkStatistics:
        Check(Cli.SQLStatistics(FStmt, Catalog, Length(CatalogName),
          Schema, Length(SchemaName),
          _PChar(ObjectName), Length(ObjectName),
          Param1, Param2));
      mkSpecialColumns:
        Check(Cli.SQLSpecialColumns(FStmt, Param1, Catalog, Length(CatalogName),
          Schema, Length(SchemaName),
          _PChar(ObjectName), Length(ObjectName),
          Param2, Param3));
    else
      Assert(False);
    end;
  end;
end;

{ TODBCMetaDataRecordSet }

procedure TODBCMetaDataRecordSet.CreateCommand;
begin
  SetCommand(TODBCMetaDataCommand.Create);
end;

procedure TODBCMetaDataRecordSet.DetectCommandType;
begin
  CommandType := ctCursor;
end;

{ TODBCMetaData }

constructor TODBCMetaData.Create;
begin
  inherited;
end;

destructor TODBCMetaData.Destroy;
begin
  inherited;
end;

function TODBCMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TODBCMetaDataRecordSet.Create;
end;

function TODBCMetaData.InternalGetMetaData(const MetaDataKind: _string; Restrictions: _TStrings): TData;
begin
  if MetaDataKind = 'specialcolumns' then
    Result := GetSpecialColumns(Restrictions)
  else
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions);
end;

procedure TODBCMetaData.InternalGetMetaDataKindsList(List: _TStringList);
begin
  inherited;

  List.Add('SpecialColumns');
  List.Sort;
end;

procedure TODBCMetaData.InternalGetRestrictionsList(List: _TStringList; const MetaDataKind: _string);
begin
  List.Clear;

  if MetaDataKind = 'specialcolumns' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('COLUMN_TYPE');
    List.Add('SCOPE');
    List.Add('NULLABLE');
  end
  else
    inherited;
end;

function TODBCMetaData.GetTables(Restrictions: _TStrings): TData;
var
  Scope: _string;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkTables;
    with FMetaDataArgs do begin
      Scope := AnsiUpperCase(Trim(Restrictions.Values['SCOPE']));
      if Scope = 'LOCAL' then begin
        CatalogName := TODBCCommand(FRecordSet.GetCommand).FConnection.GetCachedCatalog;
        SchemaName := TODBCCommand(FRecordSet.GetCommand).FConnection.GetCachedSchema;
        ObjectType := '''TABLE'',''VIEW'',''SYNONYM'',''ALIAS''';
      end
      else begin
        CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
        CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
        SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
        SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
        ObjectType := Trim(Restrictions.Values['TABLE_TYPE']);
        if ObjectType = '' then
          ObjectType := '''TABLE'',''SYSTEM TABLE'',''VIEW'',''SYNONYM'',''ALIAS''';
      end;
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      if ObjectName <> '' then
        ObjectName := SQLInfo.NormalizeName(ObjectName, False, True)
      else
        ObjectName := '%';
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateTablesFields;
      FMemData.Open;
      Result := FMemData;
      exit;
    end;
  end;

  CreateTablesFields;
  FMemData.Open;
  CopyTablesData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyTablesData(Restrictions: _TStrings);
const
  snCATALOG       = 1;
  snSCHEMA        = 2;
  snTABLE_NAME    = 3;
  snTABLE_TYPE    = 4;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snCATALOG, snSCHEMA, snTABLE_NAME, snTABLE_TYPE],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnTABLE_TYPE]);
    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetColumns(Restrictions: _TStrings): TData;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkColumns;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      if ObjectName <> '' then
        ObjectName := SQLInfo.NormalizeName(ObjectName, False, True)
      else
        ObjectName := '%';
      ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);
      if ColumnName <> '' then
        ColumnName := SQLInfo.NormalizeName(ColumnName, False, True)
      else
        ColumnName := '%';
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateColumnsFields;
      FMemData.Open;
      Result := FMemData;
      exit;
    end;
  end;

  CreateColumnsFields;
  FMemData.Open;
  CopyColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyColumnsData(Restrictions: _TStrings);
const
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

  snCATALOG       = 1;
  snSCHEMA        = 2;
  snTABLE_NAME    = 3;
  snCOLUMN_NAME   = 4;
  snPOSITION      = 17;
  snTYPE_NAME     = 6;
  snCOLUMN_SIZE   = 7;
  snSCALE         = 9;
  snNULLABLE      = 11;
  snDEFAULT       = 13;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snTABLE_NAME, snCOLUMN_NAME, snTYPE_NAME, snCOLUMN_SIZE, snCOLUMN_SIZE, snSCALE, snNULLABLE],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnCOLUMN_NAME, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE, dnNULLABLE]);

    if FRecordSet.FieldCount >= 17 then begin
      FMemDataHelper.FieldValues[dnPOSITION] := FRecordSetHelper.FieldValues[snPOSITION];
      FMemDataHelper.FieldValues[dnDEFAULT] := FRecordSetHelper.FieldValues[snDEFAULT];
    end
    else
      FMemDataHelper.FieldValues[dnPOSITION] := FRecordSet.RecordNo;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetProcedures(Restrictions: _TStrings): TData;
var
  Scope: _string;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkProcedures;
    with FMetaDataArgs do begin
      Scope := AnsiUpperCase(Trim(Restrictions.Values['SCOPE']));
      if Scope = 'LOCAL' then begin
        CatalogName := TODBCCommand(FRecordSet.GetCommand).FConnection.GetCachedCatalog;
        SchemaName := TODBCCommand(FRecordSet.GetCommand).FConnection.GetCachedSchema;
      end
      else begin
        CatalogName := Trim(Restrictions.Values['PROCEDURE_CATALOG']);
        CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
        SchemaName := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
        SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      end;
      ObjectName := Trim(Restrictions.Values['PROCEDURE_NAME']);
      if ObjectName <> '' then
        ObjectName := SQLInfo.NormalizeName(ObjectName, False, True)
      else
        ObjectName := '%';
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateProceduresFields;
      FMemData.Open;
      Result := FMemData;
      exit;
    end;
  end;

  CreateProceduresFields;
  FMemData.Open;
  CopyProceduresData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyProceduresData(Restrictions: _TStrings);
const
  snCATALOG       = 1;
  snSCHEMA        = 2;
  snPROC_NAME     = 3;
  snPROC_TYPE     = 8;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnPROC_NAME     = 3;
  dnPROC_TYPE     = 4;
var
  ProcType: integer;
  s: string;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snCATALOG, snSCHEMA, snPROC_NAME],
      [dnCATALOG, dnSCHEMA, dnPROC_NAME]);

    ProcType := FRecordSetHelper.FieldValues[snPROC_TYPE];
    if ProcType = SQL_PT_FUNCTION then
      s := 'FUNCTION'
    else
      s := 'PROCEDURE';

    FMemDataHelper.FieldValues[dnPROC_TYPE] := s;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetProcedureParameters(Restrictions: _TStrings): TData;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkProcedureColumns;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['PROCEDURE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['PROCEDURE_NAME']);
      if ObjectName <> '' then
        ObjectName := SQLInfo.NormalizeName(ObjectName, False, True)
      else
        ObjectName := '%';
      ColumnName := Trim(Restrictions.Values['PARAMETER_NAME']);
      if ColumnName <> '' then
        ColumnName := SQLInfo.NormalizeName(ColumnName, False, True)
      else
        ColumnName := '%';
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateProcedureParametersFields;
      FMemData.Open;
      Result := FMemData;
      exit;
    end;
  end;

  CreateProcedureParametersFields;
  FMemData.Open;
  CopyProcedureParametersData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyProcedureParametersData(Restrictions: _TStrings);
const
  dnCATALOG    = 1;
  dnSCHEMA     = 2;
  dnPROC_NAME  = 3;
  dnPARAM_NAME = 4;
  dnPOSITION   = 5;
  dnDIRECTION  = 6;
  dnDATA_TYPE  = 7;
  dnLENGTH     = 8;
  dnPRECISION  = 9;
  dnSCALE      = 10;

  snCATALOG    = 1;
  snSCHEMA     = 2;
  snPROC_NAME  = 3;
  snPARAM_NAME = 4;
  snPOSITION   = 18;
  snPARAM_TYPE = 5;
  snTYPE_NAME  = 7;
  snCOLUMN_SIZE = 8;
  snSCALE      = 10;

var
  ParamType: integer;
  Direction: string;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    ParamType := FRecordSetHelper.FieldValues[snPARAM_TYPE];
    if ParamType = SQL_RESULT_COL then
      continue;

    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snPROC_NAME, snPARAM_NAME, snTYPE_NAME, snCOLUMN_SIZE, snCOLUMN_SIZE, snSCALE],
      [dnCATALOG, dnSCHEMA, dnPROC_NAME, dnPARAM_NAME, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE]);

    case ParamType of
      SQL_PARAM_INPUT:
        Direction := 'IN';
      SQL_PARAM_INPUT_OUTPUT:
        Direction := 'IN/OUT';
      SQL_PARAM_OUTPUT, SQL_RETURN_VALUE:
        Direction := 'OUT';
    end;

    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;

    if FRecordSet.FieldCount >= 18 then
      FMemDataHelper.FieldValues[dnPOSITION] := FRecordSetHelper.FieldValues[snPOSITION]
    else
      FMemDataHelper.FieldValues[dnPOSITION] := FRecordSet.RecordNo;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetIndexes(Restrictions: _TStrings): TData;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkStatistics;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      ObjectName := SQLInfo.NormalizeName(ObjectName, False, True);
      Param1 := SQL_INDEX_ALL;
      Param2 := SQL_QUICK;
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateIndexesFields;
      FMemData.Open;
      Result := FMemData;
      exit;
    end;
  end;

  CreateIndexesFields;
  FMemData.Open;
  CopyIndexesData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyIndexesData(Restrictions: _TStrings);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snNON_UNIQUE      = 4;
  snINDEX_QUAL      = 5;
  snINDEX_NAME      = 6;
  snTYPE            = 7;
  snPOSITION        = 8;
  snCOLUMN_NAME     = 9;
  snSORT_ORDER      = 10;

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
var
  OldIndexName, IndexName: _string;
  i: integer;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  OldIndexName := '';
  while FRecordSetHelper.NextRecord do begin
    i := FRecordSetHelper.FieldValues[snTYPE];
    if i = SQL_TABLE_STAT then
      continue;

    IndexName := _VarToStr(FRecordSetHelper.FieldValues[snINDEX_NAME]);
    if IndexName = OldIndexName then
      continue;
    OldIndexName := IndexName;

    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snTABLE_CATALOG, snTABLE_SCHEMA, snINDEX_NAME],
      [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnINDEX_CATALOG, dnINDEX_SCHEMA, dnINDEX_NAME]);

    if FRecordSetHelper.FieldValues[snNON_UNIQUE] = SQL_FALSE then
      i := 1
    else
      i := 0;
    FMemDataHelper.FieldValues[dnUNIQUE] := i;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetIndexColumns(Restrictions: _TStrings): TData;
var
  Uniqueness: string;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkStatistics;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      ObjectName := SQLInfo.NormalizeName(ObjectName, False, True);
      Uniqueness := Trim(Restrictions.Values['UNIQUE']);
      if Uniqueness = '1' then
        Param1 := SQL_INDEX_UNIQUE
      else
        Param1 := SQL_INDEX_ALL;
      Param2 := SQL_QUICK;
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateIndexColumnsFields;
      FMemData.Open;
      Result := FMemData;
      exit;
    end;
  end;

  CreateIndexColumnsFields;
  FMemData.Open;
  CopyIndexColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyIndexColumnsData(Restrictions: _TStrings);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snNON_UNIQUE      = 4;
  snINDEX_QUAL      = 5;
  snINDEX_NAME      = 6;
  snTYPE            = 7;
  snPOSITION        = 8;
  snCOLUMN_NAME     = 9;
  snSORT_ORDER      = 10;

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
  s: string;
  i: integer;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    i := FRecordSetHelper.FieldValues[snTYPE];
    if i = SQL_TABLE_STAT then
      continue;

    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snTABLE_CATALOG, snTABLE_SCHEMA, snINDEX_NAME, snCOLUMN_NAME, snPOSITION],
      [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnINDEX_CATALOG, dnINDEX_SCHEMA, dnINDEX_NAME, dnCOLUMN_NAME, dnPOSITION]);

    s := FRecordSetHelper.FieldValues[snSORT_ORDER];
    if s = 'D' then
      s := 'DESC'
    else
      s := 'ASC';
    FMemDataHelper.FieldValues[dnSORT_ORDER] := s;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetConstraints(Restrictions: _TStrings): TData;
begin
  CreateConstraintsFields;
  FMemData.Open;
  Result := FMemData;
end;

function TODBCMetaData.GetSpecialColumns(Restrictions: _TStrings): TData;
var
  ColumnType, Scope, Nullable: string;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkSpecialColumns;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      ObjectName := SQLInfo.NormalizeName(ObjectName, False, True);
      ColumnType := AnsiUpperCase(Trim(Restrictions.Values['COLUMN_TYPE']));
      if ColumnType = 'ROWVER' then
        Param1 := SQL_ROWVER
      else
        Param1 := SQL_BEST_ROWID;
      Scope := AnsiUpperCase(Trim(Restrictions.Values['SCOPE']));
      if Scope = 'SESSION' then
        Param2 := SQL_SCOPE_SESSION
      else
      if Scope = 'TRANSACTION' then
        Param2 := SQL_SCOPE_TRANSACTION
      else
        Param2 := SQL_SCOPE_CURROW;
      Nullable := Trim(Restrictions.Values['NULLABLE']);
      if Nullable = '0' then
        Param3 := SQL_NO_NULLS
      else
        Param3 := SQL_NULLABLE;
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateSpecialColumnsFields;
      FMemData.Open;
      Result := FMemData;
      exit;
    end;
  end;

  CreateSpecialColumnsFields;
  FMemData.Open;
  CopySpecialColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CreateSpecialColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('COLUMN_POSITION', dtInt32);
  FMemData.InitFields;
end;

procedure TODBCMetaData.CopySpecialColumnsData(Restrictions: _TStrings);
const
  snSCOPE           = 1;
  snCOLUMN_NAME     = 2;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnCOLUMN_NAME     = 4;
  dnPOSITION        = 5;
var
  CatalogName, SchemaName, ObjectName: _string;
begin
  CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
  if CatalogName <> '' then
    CatalogName := FRecordSet.GetCommand.SQLInfo.NormalizeName(CatalogName, False, True)
  else
    CatalogName := TODBCConnection(FRecordSet.GetCommand.GetConnection).GetCachedCatalog;
  SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
  if SchemaName <> '' then
    SchemaName := FRecordSet.GetCommand.SQLInfo.NormalizeName(SchemaName, False, True)
  else
    SchemaName := TODBCConnection(FRecordSet.GetCommand.GetConnection).GetCachedSchema;
  ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
  ObjectName := FRecordSet.GetCommand.SQLInfo.NormalizeName(ObjectName, False, True);

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;

    CopyRecord([snCOLUMN_NAME], [dnCOLUMN_NAME]);

    FMemDataHelper.FieldValues[dnTABLE_CATALOG] := CatalogName;
    FMemDataHelper.FieldValues[dnTABLE_SCHEMA] := SchemaName;
    FMemDataHelper.FieldValues[dnTABLE_NAME] := ObjectName;
    FMemDataHelper.FieldValues[dnPOSITION] := FRecordSet.RecordNo;

    FMemDataHelper.AppendRecord;
  end;
end;

{$IFNDEF LITE}

{ TODBCLoader }

constructor TODBCLoader.Create;
begin
  inherited;
end;

destructor TODBCLoader.Destroy;
begin
  inherited;
end;

function TODBCLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := inherited SetProp(Prop, Value);
end;

function TODBCLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited GetProp(Prop, Value);
end;

procedure TODBCLoader.CreateCommand;
begin
  FCommand := TODBCCommand.Create;
end;

class function TODBCLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TODBCRecordSet;
end;

{$ENDIF}

initialization
  LocaleDecSeparator := DecimalSeparator{$IFDEF CLR}[1]{$ENDIF};
end.
