
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I SQLiteDac.inc}
unit LiteClassesUni;
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
  {$IFNDEF LITE}LiteFunction,{$ENDIF} LiteCall, LiteError, LiteParser, LiteCollation;
{$ELSE}
  {$IFNDEF LITE}LiteFunctionUni,{$ENDIF} LiteCallUni, LiteErrorUni, LiteParserUni, LiteCollationUni;
{$ENDIF}

const
  prLiteBase = 1000;
  prUseUnicode = prLiteBase + 1;
  prClientLibrary = prLiteBase + 2;
  prDumpData = prLiteBase + 3;
  prEncryptionKey = prLiteBase + 4;
  prASCIIDataBase = prLiteBase + 5;
  prEnableSharedCache = prLiteBase + 6;
  prBusyTimeout =  prLiteBase + 7;
  prReadUncommitted = prLiteBase + 8;
{$IFDEF LITE}
  prNewEncryptionKey = prLiteBase + 9;
{$ENDIF}
  prDefaultCollations = prLiteBase + 10;

  dsMaxStringSize = 8192;
  DefaultStringFieldLength = dsMaxStringSize div 2 - 1;


type
  TSQLiteCommand = class;
  TSQLiteTypes = class;

  TSQLiteConnection = class(TCRConnection)
  private
    FSQLite: Tsqlite3;
    FCommand: TSQLiteCommand;
    FLiteTypes: TSQLiteTypes;
    FCollationManager: TSQLiteCollationManager;
  {$IFNDEF LITE}
    FFunctionManager: TSQLiteFunctionManager;
  {$ENDIF}
    FDatabase: string;
    FClientLibrary: string;
    FUseUnicode: boolean;
    FASCIIDataBase: boolean;
    FEncryptionKey: string;
  {$IFDEF LITE}
    FChangeEncryptionKey: boolean; // if need change encryption key
    FNewEncryptionKey: string; // for change password in dbExpres
  {$ENDIF}
    FEnableSharedCache: boolean;
    FBusyTimeout: integer;
    FReadUncommitted: boolean;
    FDefaultCollations: boolean;
    FLastInsertId: int64;

    procedure SetASCIIDataBase(const Value: boolean);
    procedure SetBusyTimeout(const Value: integer);
    procedure SetReadUncommitted(const Value: boolean);
    procedure SetDefaultCollations(const Value: boolean);
    procedure CheckCommand;
  protected
    procedure ExecCommand(const SQL: _string);

    procedure InternalSetBusyTimeout(const Value: integer);
    procedure InternalSetReadUncommitted(const Value: boolean);
    procedure InternalSetDefaultCollations(const Value: boolean);

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

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Check(ErrorCode: integer);
    procedure CheckExtended(ErrorCode: integer);
    procedure ProcessError(ErrorCode: integer; ErrorMsg:_string; Component: TObject); virtual;

    procedure Connect(const ConnectString: _string); override;
    procedure Disconnect; override;
    function CheckIsValid: boolean; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function GetServerVersion: _string; override;
    function GetServerVersionFull: _string; override;
    function GetClientVersion: _string; override;
    function CanChangeDatabase: boolean; override;
    function GetTypes: TSQLiteTypes;
    function GetCollationManager: TSQLiteCollationManager;
  {$IFNDEF LITE}
    function GetFunctionManager: TSQLiteFunctionManager;
  {$ENDIF}

    { String encoding}
    function IsUnicodeDataBase: boolean;
    function EncodeString(AString: AnsiString): AnsiString; overload;
    function EncodeString(AString: AnsiString; UseUTF8: boolean): AnsiString; overload;
    function EncodeString(WString: WideString): AnsiString; overload;
    function EncodeString(WString: WideString; UseUTF8: boolean): AnsiString; overload;
    function DecodeStringA(AString: AnsiString): AnsiString; overload;
    function DecodeStringA(AString: AnsiString; UseUTF8: boolean): AnsiString; overload;
    function DecodeStringW(AString: AnsiString): WideString; overload;
    function DecodeStringW(AString: AnsiString; UseUTF8: boolean): WideString; overload;

    { SQLite encryption }
    procedure EncryptDatabase(NewKey: _string);
    procedure DecryptDatabase(Key: _String);

    property ASCIIDataBase: boolean read FASCIIDataBase;
    property SQLite: Tsqlite3 read FSQLite;
    property UseUnicode: boolean read FUseUnicode;
  end;

  TSQLiteTransaction = class(TCRTransaction)
  public
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: _string); override;
    procedure ReleaseSavepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;
  end;

  TSQLiteTypes = class
  private
    FConnection: TSQLiteConnection;

    procedure GetTypeAttr(DataType: Integer; TypeModifier: Integer;
      var Length, Scale: integer);
  protected
  public
    constructor Create(Connection: TSQLiteConnection);
    destructor Destroy; override;

    procedure DetectDataType(TypeOID, TypeModifier: integer;
      var DataType, SubDataType: word; var Length, Scale, Size: integer;
      var Fixed: boolean; var ObjectType: TObjectType;
      LongStrings, FlatBuffers, EnableBCD, EnableFMTBCD,
      CursorAsString, FieldsAsText: boolean);
    function GetInternalType(TypeCode: integer): word; overload;
    function GetInternalType(TypeCode: integer; var ObjectType: TObjectType): word; overload;

    function GetSQLiteType(DataType: word; var NeedConvertToText: boolean): word;
    function GetVarType(VarType: TVarType): word;
    function ConvertToText(DataType: word; Value: Variant): AnsiString;
    function ConvertBlob(Value: Variant; var DestrType: IntPtr): TBytes;
    function ConvertMemo(Value: Variant): AnsiString;
  end;

  TSQLiteCommand = class(TCRCommand)
  private
    FConnection: TSQLiteConnection;
    FStmt: Tsqlite3_stmt;
    FCursorState: TCursorState;
    FExecResult: integer;
    FRowsAffected: integer;
    FLockAfterExecute: boolean;

    procedure BindParams;
    procedure BindBlob(Index: integer; const Value: variant);
    procedure BindMemo(Index: integer; const Value: variant);

  protected
    procedure Check(ErrorCode: integer);

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
    class function GetTableInfoClass: TTableInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure SetConnection(Value: TCRConnection); override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;
    procedure Execute(Iters: integer = 1); override;
    function ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string; override;
    procedure InitProcParams(const Name: _string; Overload: integer);
    function CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string; override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;
  end;

  TSQLiteRecordSet = class(TCRRecordSet)
  private
    FCommand: TSQLiteCommand;
    FRowsFetched: integer;

    FDumpData: boolean;

    procedure DescribeFieldDesc(Field: TCRFieldDesc; Index: integer);
    function InternalFetch: boolean;
    procedure ReadFieldValues(RecBuf: IntPtr);
    procedure ReadFieldValuesForDump(RecBuf: IntPtr);

  protected
    procedure Check(ErrorCode: integer);
    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalInitFields; override;
    procedure InternalClose; override;
    function Fetch(FetchBack: boolean = False): boolean; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure ExecCommand; override;
    function IsFullReopen: boolean; override;
    procedure Reopen; override;
    procedure SetToEnd; override;
  end;

  TSQLiteTableInfo = class(TCRTableInfo)
  protected
    FTableNameFull: _string;
    function GetTableNameFull: _string; override;
    procedure SetTableNameFull(const Value: _string); override;
  end;

{ TSQLiteMetaData }

  TColumnInfo = record
    Name: _string;
    DataType: _string;
    IsAutoincrement: boolean;
    Default: _string;
  end;

  TIndexColumnInfo = record
    ColumnIndex: integer;
    IsDesc: boolean;
  end;

  TIndexType = (itPrimaryKey, itUnique, itNonUnique);

  TIndexInfo = record
    IndexType: TIndexType;
    ColumnInfo: array of TIndexColumnInfo;
  end;

  TTableMembers = record
    Columns: array of TColumnInfo;
    Indexes: array of TIndexInfo;
  end;

{$IFDEF LITE}
  {$DEFINE DBX_METADATA}
{$ENDIF}
  
  TSQLiteMetaData = class (TCRMetaData)
  protected
    function GetConnection: TSQLiteConnection;
    function GetTypesForSQL(const ObjectTypes: _string; AllTypes: array of _string): _string;
    function CreateRecordSet: TCRRecordSet; override;
    procedure ParseTableSQL(const SQL: _string; var Members: TTableMembers);
    procedure ParseIndexSQL(const SQL: _string; var Members: TTableMembers);

    function GetTables(Restrictions: _TStrings): TData; override;
    function GetColumns(Restrictions: _TStrings): TData; override;
    function GetProcedures(Restrictions: _TStrings): TData; override;
    function GetProcedureParameters(Restrictions: _TStrings): TData; override;
    function GetIndexes(Restrictions: _TStrings): TData; override;
    function GetIndexColumns(Restrictions: _TStrings): TData; override;
    function GetConstraints(Restrictions: _TStrings): TData; override;

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TSQLiteInfo = class (TSQLInfo)
  public
    function IdentCase: TIdentCase; override;
    function ParamQuoteAllowed: boolean; override;
    procedure ParseTablesInfo(const SQL: _string; TablesInfo: TCRTablesInfo); override;
  end;

{$IFNDEF LITE}
  TSQLiteLoader = class (TCRSimpleLoader)
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

var
  SQLiteInfo: TSQLiteInfo;

implementation

uses
  Math, StrUtils, DAConsts,
{$IFNDEF UNIDACPRO}
  LiteConsts;
{$ELSE}
  LiteConstsUni;
{$ENDIF}

var
  ActiveConnectionsCount: integer;
  hLockConnectCount: TCriticalSection;

{ TSQLiteConnection }

constructor TSQLiteConnection.Create;
begin
  inherited;

  FBusyTimeout := 0;
  FEnableSharedCache := False;
  FEncryptionKey := '';
{$IFDEF LITE}
  FNewEncryptionKey := '';
{$ENDIF}
  FReadUncommitted := False;
  FCommand := nil;
  FLiteTypes := nil;
  FCollationManager := nil;
{$IFNDEF LITE}
  FFunctionManager := nil;
{$ENDIF}
end;

destructor TSQLiteConnection.Destroy;
begin
  Disconnect;
{$IFNDEF LITE}
  FFunctionManager.Free;
{$ENDIF}
  FCollationManager.Free;
  FLiteTypes.Free;
  FCommand.Free;

  inherited;
end;

procedure TSQLiteConnection.SetASCIIDataBase(const Value: boolean);
begin
  if FASCIIDataBase <> Value then
    Disconnect;

  FASCIIDataBase := Value;
end;

procedure TSQLiteConnection.SetBusyTimeout(const Value: integer);
begin
  FBusyTimeout := Value;
  if FConnected then
    InternalSetBusyTimeout(FBusyTimeout);
end;

procedure TSQLiteConnection.SetReadUncommitted(const Value: boolean);
begin
  FReadUncommitted := Value;
  if FConnected then
    InternalSetReadUncommitted(FReadUncommitted);
end;

procedure TSQLiteConnection.SetDefaultCollations(const Value: boolean);
begin
  FDefaultCollations := Value;
  if FConnected then
    InternalSetDefaultCollations(Value);
end;

procedure TSQLiteConnection.CheckCommand;
begin
  if FCommand = nil then begin
    FCommand := TSQLiteCommand.Create;
    FCommand.SetConnection(Self);
  end;
end;

procedure TSQLiteConnection.ExecCommand(const SQL: _string);
begin
  CheckCommand;

  FCommand.SetSQL(SQL);
  FCommand.Execute;
end;

procedure TSQLiteConnection.InternalSetBusyTimeout(const Value: integer);
begin
  Check(sqlite3_busy_timeout(FSQLite, FBusyTimeout));
end;

procedure TSQLiteConnection.InternalSetReadUncommitted(const Value: boolean);
begin
  if Value then
    ExecCommand('PRAGMA read_uncommitted = true')
  else
    ExecCommand('PRAGMA read_uncommitted = false');
end;

procedure TSQLiteConnection.InternalSetDefaultCollations(const Value: boolean);
begin
  if Value then
    GetCollationManager.RegisterDefaultCollations
  else
    GetCollationManager.UnRegisterDefaultCollations
end;

function TSQLiteConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prClientLibrary:
      FClientLibrary := Value;
    prUseUnicode:
      FUseUnicode := Value;
    prASCIIDataBase:
      SetASCIIDataBase(Value);
    prEncryptionKey:
      FEncryptionKey := Value;
{$IFDEF LITE}
    prNewEncryptionKey: begin
      FChangeEncryptionKey := True;
      FNewEncryptionKey := Value;
    end;
{$ENDIF}
    prEnableSharedCache:
      FEnableSharedCache:= Value;
    prBusyTimeout:
      SetBusyTimeout(Value);
    prReadUncommitted:
      SetReadUncommitted(Value);
    prDefaultCollations:
      SetDefaultCollations(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TSQLiteConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prClientLibrary:
      Value := FClientLibrary;
    prUseUnicode:
      Value := FUseUnicode;
    prASCIIDataBase:
      Value := FASCIIDataBase;
    prLastInsertId:
      Value := FLastInsertId;
    prEnableSharedCache:
      Value := FEnableSharedCache;
    prEncryptionKey:
      Value := FEncryptionKey;
{$IFDEF LITE}
    prNewEncryptionKey:
      Value := FNewEncryptionKey;
{$ENDIF}
    prBusyTimeout:
      Value := FBusyTimeout;
    prReadUncommitted:
      Value := FReadUncommitted;
    prDefaultCollations:
      Value := FDefaultCollations;  
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TSQLiteConnection.Check(ErrorCode: integer);
var
  ErrorMsg: _string;
begin
  if ErrorCode = 0 then
    exit;

  GetLiteErrorMsg(FSQLite, ErrorMsg);
  ProcessError(ErrorCode, ErrorMsg, Component);
end;

procedure TSQLiteConnection.CheckExtended(ErrorCode: integer);
var
  liteErrorCode: integer;
  ErrorMsg: _string;
begin
  if ErrorCode = 0 then
    exit;

  // check if SQLite can return error
  GetLiteErrorCode(FSQLite, liteErrorCode);
  if liteErrorCode <> 0 then
    // if SQLite can return error then use standard method
    Check(liteErrorCode)
  else begin
    // if SQLite cannot return error then try get internal error code and msg
    GetPredefinedErrorMsg(ErrorCode, ErrorMsg);
    ProcessError(ErrorCode, ErrorMsg, Component);
  end;
end;

procedure TSQLiteConnection.ProcessError(ErrorCode: integer; ErrorMsg: _string; Component: TObject);
var
  Error: ESQLiteError;
  Fail, NeedFreeError: boolean;
begin
  NeedFreeError := True;
  Error := ESQLiteError.Create(ErrorCode, ErrorMsg);
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

procedure TSQLiteConnection.Connect(const ConnectString: _string);
begin
  if FConnected then
    Exit;

  hLockConnectCount.Enter;
  try
    if (FClientLibrary <> '') and (FClientLibrary <> SQLiteDLL) then begin
      if ActiveConnectionsCount > 0 then
        raise Exception.Create(SClientLibraryDiffers)

      else begin
        FreeSQLite;
      {$IFNDEF CLR}
        SQLiteDLL := FClientLibrary;
      {$ENDIF}
      end;
    end;

    Inc(ActiveConnectionsCount);
  finally
    hLockConnectCount.Leave;
  end;

  try
    if not SQLiteInited then
      InitSQLite;

    FSQLite := nil;
    try
      // shared cache should be enabled or disabled before open connection
      if FEnableSharedCache then
        Check(sqlite3_enable_shared_cache(1))
      else
        Check(sqlite3_enable_shared_cache(0));

      Check(sqlite3_open(PAnsiChar(UTF8Encode(FDatabase)), FSQLite));

      if FEncryptionKey <> '' then
        DecryptDatabase(FEncryptionKey);

{$IFDEF LITE}
      if FChangeEncryptionKey then
      try
        EncryptDatabase(FNewEncryptionKey);
      finally
        FChangeEncryptionKey := false;
        FNewEncryptionKey := '';
      end;
{$ENDIF}

      FConnected := True;

      InternalSetBusyTimeout(FBusyTimeout);
      InternalSetReadUncommitted(FReadUncommitted);
      if FDefaultCollations then
        InternalSetDefaultCollations(FDefaultCollations);

      FNativeConnection := True;

      inherited;

    except
      on EFailOver do;
      else begin
        if FSQLite <> nil then begin
          sqlite3_close(FSQLite);
          FSQLite := nil;
        end;
        FConnected := False;
        raise;
      end;
    end;

  except
    InterlockedDecrement(ActiveConnectionsCount);
    raise;
  end;
end;

procedure TSQLiteConnection.Disconnect;
begin
  if not FConnected then
    exit;

  FConnected := False;
  try
    if FNativeConnection then
      Check(sqlite3_close(FSQLite));
  finally
    FSQLite := nil;
    FNativeConnection := True;
    InterlockedDecrement(ActiveConnectionsCount);
  end;

  inherited;
end;

function TSQLiteConnection.CheckIsValid: boolean;
begin
  Result := True;
end;

procedure TSQLiteConnection.Assign(Source: TCRConnection);
var
  Src: TSQLiteConnection;
begin
  inherited;

  Src := TSQLiteConnection(Source);
  FDatabase := Src.FDatabase;
  FClientLibrary := Src.FClientLibrary;
  FASCIIDataBase := Src.FASCIIDataBase;
  FBusyTimeout := src.FBusyTimeout;
  FDefaultCollations := src.FDefaultCollations;
  FEncryptionKey := src.FEncryptionKey;
  FEnableSharedCache := src.FEnableSharedCache;
  FReadUncommitted := src.FReadUncommitted;
  FUseUnicode := Src.FUseUnicode;
end;

procedure TSQLiteConnection.AssignConnect(Source: TCRConnection);
var
  Src: TSQLiteConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TSQLiteConnection(Source);
      Assign(Src);
      FSQLite := Src.FSQLite;
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := True;
      FNativeConnection := False;
    end;
  end;
end;

function TSQLiteConnection.GetServerVersion: _string;
begin
  Result := GetClientVersion;
end;

function TSQLiteConnection.GetServerVersionFull: _string;
begin
  Result := GetClientVersion;
end;

function TSQLiteConnection.GetClientVersion: _string;
begin
  Result := _string(AnsiString(sqlite3_libversion));
end;

function TSQLiteConnection.CanChangeDatabase: boolean;
begin
  Result := False;
end;

function TSQLiteConnection.GetTypes: TSQLiteTypes;
begin
  if FLiteTypes = nil then
    FLiteTypes := TSQLiteTypes.Create(self);
  Result := FLiteTypes;
end;

function TSQLiteConnection.GetCollationManager: TSQLiteCollationManager;
begin
  if FCollationManager = nil then
    FCollationManager := TSQLiteCollationManager.Create(self);

  Result := FCollationManager;
end;

{$IFNDEF LITE}
function TSQLiteConnection.GetFunctionManager: TSQLiteFunctionManager;
begin
  if FFunctionManager = nil then
    FFunctionManager := TSQLiteFunctionManager.Create(self);

  Result := FFunctionManager;
end;
{$ENDIF}

function TSQLiteConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TSQLiteCommand;
end;

function TSQLiteConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TSQLiteTransaction;
end;

function TSQLiteConnection.IsUnicodeDataBase: boolean;
begin
  Result := FUseUnicode or not FASCIIDataBase;
end;

function TSQLiteConnection.EncodeString(AString: AnsiString): AnsiString;
begin
  Result := EncodeString(AString, IsUnicodeDataBase);
end;

function TSQLiteConnection.EncodeString(AString: AnsiString; UseUTF8: boolean): AnsiString;
begin
  if UseUTF8 then
    Result := UTF8Encode(WideString(AString))
  else
    Result := AString;
end;

function TSQLiteConnection.EncodeString(WString: WideString): AnsiString;
begin
  Result := EncodeString(WString, IsUnicodeDataBase);
end;

function TSQLiteConnection.EncodeString(WString: WideString; UseUTF8: boolean): AnsiString;
begin
  if UseUTF8 then
    Result := UTF8Encode(WString)
  else
    Result := AnsiString(WString);
end;

function TSQLiteConnection.DecodeStringA(AString: AnsiString): AnsiString;
begin
  Result := DecodeStringA(AString, IsUnicodeDataBase);
end;

function TSQLiteConnection.DecodeStringA(AString: AnsiString; UseUTF8: boolean): AnsiString;
begin
  if UseUTF8 then
    Result := AnsiString(UTF8Decode(AString))
  else
    Result := AString;
end;

function TSQLiteConnection.DecodeStringW(AString: AnsiString): WideString;
begin
  Result := DecodeStringW(AString, IsUnicodeDataBase);
end;

function TSQLiteConnection.DecodeStringW(AString: AnsiString; UseUTF8: boolean): WideString;
begin
  if UseUTF8 then
    Result := UTF8Decode(AString)
  else
    Result := WideString(AString);
end;

procedure TSQLiteConnection.EncryptDatabase(NewKey: _string);
var
  pNewKey: PAnsiChar;
begin
  pNewKey := PAnsiChar(AnsiString(NewKey));
  CheckExtended(sqlite3_rekey(FSQLite, pNewKey, Length(pNewKey)));
end;

procedure TSQLiteConnection.DecryptDatabase(Key: _String);
var
  pKey: PAnsiChar;
begin
  pKey := PAnsiChar(AnsiString(Key));
  Check(sqlite3_key(FSQLite, pKey, Length(pKey)));
end;

procedure TSQLiteTransaction.StartTransaction;
var
  CommandText: string;
  Connection: TSQLiteConnection;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  Connection := TSQLiteConnection(FConnections[0]);

  if not Connection.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);

  CommandText := 'BEGIN ';

  case FIsolationLevel of
    ilReadUncommitted, ilReadCommitted, ilRepeatableRead:
      CommandText := CommandText + 'DEFERRED';
    ilIsolated, ilSnapshot:
      CommandText := CommandText + 'IMMEDIATE';
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;

  Connection.ExecCommand(CommandText);

  FActive := True;
  FNativeTransaction := True;
end;

procedure TSQLiteTransaction.Commit;
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  if FNativeTransaction then
    Connection.ExecCommand('COMMIT');

  FActive := False;
end;

procedure TSQLiteTransaction.Rollback;
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  if FNativeTransaction then
    Connection.ExecCommand('ROLLBACK');

  FActive := False;
end;

procedure TSQLiteTransaction.Savepoint(const Name: _string);
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  Connection.ExecCommand('SAVEPOINT ' + Name);
end;

procedure TSQLiteTransaction.ReleaseSavepoint(const Name: _string);
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  Connection.ExecCommand('RELEASE SAVEPOINT ' + Name);
end;

procedure TSQLiteTransaction.RollbackToSavepoint(const Name: _string);
var
  Connection: TSQLiteConnection;
begin
  CheckActive;

  Connection := TSQLiteConnection(FConnections[0]);
  Connection.ExecCommand('ROLLBACK TO SAVEPOINT ' + Name);
end;

{ TSQLiteTypes }

constructor TSQLiteTypes.Create(Connection: TSQLiteConnection);
begin
  inherited Create;
  FConnection := Connection;
end;

destructor TSQLiteTypes.Destroy;
begin
  inherited;
end;

procedure TSQLiteTypes.GetTypeAttr(DataType: Integer; TypeModifier: Integer;
  var Length, Scale: integer);
begin
  Length := 0;
  Scale := 0;

  case DataType of
    dtString:
      if TypeModifier <> -1 then
        Length := TypeModifier - 4;
  end;
end;

procedure TSQLiteTypes.DetectDataType(TypeOID, TypeModifier: integer;
  var DataType, SubDataType: word; var Length, Scale, Size: integer;
  var Fixed: boolean; var ObjectType: TObjectType;
  LongStrings, FlatBuffers, EnableBCD, EnableFMTBCD,
  CursorAsString, FieldsAsText: boolean);
const
  dsMaxStringSize = 8192;
begin
  DataType := GetInternalType(TypeOID, ObjectType);
  SubDataType := 0;
  Fixed := False;

  GetTypeAttr(DataType, TypeModifier, Length, Scale);

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
    end;
  end;

  case DataType of
    dtString: begin
      if Length = 0 then begin
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

function TSQLiteTypes.GetInternalType(TypeCode: integer): word;
var
  ObjectType: TObjectType;
begin
  Result := GetInternalType(TypeCode, ObjectType);
end;

function TSQLiteTypes.GetInternalType(TypeCode: integer; var ObjectType: TObjectType): word;
begin
  ObjectType := nil;
  case TypeCode of
    SQLITE_INTEGER:
      Result := dtInteger;
    SQLITE_FLOAT:
      Result := dtFloat;
    SQLITE_TEXT:
      Result := dtString;
    SQLITE_BLOB:
      Result := dtBlob;
  else
    Result := dtUnknown;
  end;
end;

function TSQLiteTypes.GetSQLiteType(DataType: word; var NeedConvertToText: boolean): word;
begin
  NeedConvertToText := False;

  case DataType of
    dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32, dtBoolean:
      Result := SQLITE_INTEGER;
    dtFloat, dtCurrency, dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}:
      Result := SQLITE_FLOAT;
    dtDate: begin
      Result := SQLITE_TEXT;
      NeedConvertToText := True;
    end;
    dtTime: begin
      Result := SQLITE_TEXT;
      NeedConvertToText := True;
    end;
    dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
      Result := SQLITE_TEXT;
      NeedConvertToText := True;
    end;
    dtBlob, dtBytes, dtVarBytes:
      Result := SQLITE_BLOB;
  else
    // Unknown types save as TEXT
    Result := SQLITE_TEXT;
  end;
end;

function TSQLiteTypes.GetVarType(VarType: TVarType): word;
begin
  case VarType of
    varShortInt, varByte:
      Result := dtInt8;
    varSmallInt:
      Result := dtInt16;
    varWord:
      Result := dtUInt16;
    varInteger:
      Result := dtInt32;
    varLongWord:
      Result := dtUInt32;
    varInt64:
      Result := dtInt64;
    varBoolean:
      Result := dtBoolean;
    varSingle, varDouble:
      Result := dtFloat;
    varCurrency:
      Result := dtCurrency;
    varDate:
      Result := dtDateTime;
    varArray + varByte:
      Result := dtVarBytes;
{$IFNDEF CLR}
    varByRef:
      Result := dtVarBytes;
    varAny:
      Result := dtBytes;
{$ENDIF}
  else
    Result := dtUnknown;
  end;
end;

function TSQLiteTypes.ConvertToText(DataType: word; Value: Variant): AnsiString;
begin
  case DataType of
    dtDate:
      Result := AnsiString(FormatDateTime('yyyy-mm-dd', Value));
    dtTime:
      Result := AnsiString(FormatDateTime('hh'':''nn'':''ss', Value));
    dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}:
      Result := AnsiString(FormatDateTime('yyyy-mm-dd hh'':''nn'':''ss', Value));
  else
    Result := AnsiString(VarToStr(Value));
  end;
end;

function TSQLiteTypes.ConvertBlob(Value: Variant; var DestrType: IntPtr): TBytes;
var
  Obj: TObject;
  Blob: TBlob;
{$IFNDEF CLR}
//  pBlob: pointer;
//  lb, hb: integer;
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
      Blob.Defrag;
      Marshal.Copy(PtrOffset(Blob.FirstPiece, SizeOf(TPieceHeader)), Result, 0, Blob.FirstPiece.Used);
      DestrType := SQLITE_STATIC;
    end;
  {$IFNDEF CLR}
    varArray or varByte: begin
      Result := Value;
      {
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      pBlob := VarArrayLock(Value);
      try
        Marshal.Copy(pBlob, Result, 0, hb - lb + 1);
      finally
        VarArrayUnlock(Value);
      end;
      }
      DestrType := SQLITE_TRANSIENT;
    end;
  {$ENDIF}
  else
    Result := TBytes(AnsiString(VarToStr(Value)));
    DestrType := SQLITE_TRANSIENT;
  end;
end;

function TSQLiteTypes.ConvertMemo(Value: Variant): AnsiString;
var
  ws: WideString;
  Obj: TObject;
  Blob: TBlob;
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
      ws := Blob.AsWideString;
    end;
  else
    ws := VarToWideStr(Value);
  end;

  // encode string: UTF8 or ASCII
  Result := FConnection.EncodeString(ws);
end;

{ TSQLiteCommand }

constructor TSQLiteCommand.Create;
begin
  inherited;

end;

destructor TSQLiteCommand.Destroy;
begin

  inherited;
end;

class function TSQLiteCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TSQLiteInfo;
end;

function TSQLiteCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  {Result := True;
  case Prop of
  else}
    Result := inherited SetProp(Prop, Value);
  //end;
end;

function TSQLiteCommand.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRowsProcessed:
      Value := FRowsAffected;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TSQLiteCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TSQLiteConnection(Value);
  end;
end;

class function TSQLiteCommand.GetTableInfoClass: TTableInfoClass;
begin
  Result := TSQLiteTableInfo;
end;

class function TSQLiteCommand.GetParserClass: TSQLParserClass;
begin
  Result := TLiteParser;
end;

procedure TSQLiteCommand.Prepare;
var
  Sql8: AnsiString;
begin
  if FStmt <> nil then
    Exit;

  if FConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
  if Trim(FSQL) = '' then
    raise Exception.Create(SEmptySQLStatement);

  // encode string: UTF8 or ASCII
  Sql8 := FConnection.EncodeString(FSQL);

  // FStmt will be NULL on error
  Check(sqlite3_prepare_v2(FConnection.FSQLite, PAnsiChar(Sql8),
    Length(Sql8), FStmt, nil));

  FCursorState := csPrepared;
end;

procedure TSQLiteCommand.Unprepare;
begin
  if FStmt = nil then
    Exit;

  sqlite3_finalize(FStmt);
  FStmt := nil;
  FCursorState := csInactive;
end;

function TSQLiteCommand.GetPrepared: boolean;
begin
  Result := FStmt <> nil;
end;

procedure TSQLiteCommand.Execute(Iters: integer = 1);
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
      sqlite3_reset(FStmt);

      BindParams;

      FExecResult := sqlite3_step(FStmt);
      if not ( (FExecResult = SQLITE_ROW) or (FExecResult = SQLITE_DONE) ) then
        Check(FExecResult);

      FConnection.FLastInsertId := sqlite3_last_insert_rowid(FConnection.FSQLite);
      FRowsAffected := sqlite3_changes(FConnection.FSQLite);
    finally
      if NeedPrepare then
        UnPrepare;
    end;

  except
    if not FLockAfterExecute and Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;
  if not FLockAfterExecute and Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

function TSQLiteCommand.ParseSQL(const SQL: _string; Params: TParamDescs; ReplaceAll: boolean = True; const RenamePrefix: _string = ''): _string;
var
  Param: TParamDesc;
  Parser: TLiteParser;
  Code: integer;
  St: _string;
  AllParams: _TStringList;
begin
  Result := SQL;

  if not FScanParams then
    exit;

  Assert(Params <> nil);
  Params.Clear;
  Parser := TLiteParser.Create(SQL);
  Parser.DecSeparator := '.';
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  AllParams := _TStringList.Create;
  AllParams.Sorted := True;
  try
    Parser.ToBegin;
    repeat
      Code := Parser.GetNext(St);
      while (Code <> lcEnd) and (St <> ':') do begin
        Code := Parser.GetNext(St);
      end;
      if St = ':' then begin
        Code := Parser.GetNext(St);
        if (Code = lcIdent) or (Code = lcNumber) or (Code > Parser.SymbolLexems.Count)
          and (Code <= Parser.SymbolLexems.Count + Parser.KeywordLexems.Count)
        then begin
          // remove quotes
          if St[1] = '"' then begin
            Delete(St, 1, 1);
            if St[Length(St)] = '"' then
              SetLength(St, Length(St) - 1);
          end;

          if AllParams.IndexOf(St) < 0 then begin
            Param := TParamDesc.Create;
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

procedure TSQLiteCommand.InitProcParams(const Name: _string; Overload: integer);
begin
  raise Exception.Create(SStoredProcNotSupported);
end;

function TSQLiteCommand.CreateProcCall(const Name: _string; NeedDescribe: boolean; IsQuery: boolean): _string;
begin
  raise Exception.Create(SStoredProcNotSupported);
end;

function TSQLiteCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TSQLiteCommand.SetCursorState(Value: TCursorState);
begin
  FCursorState := Value;
end;

procedure TSQLiteCommand.Check(ErrorCode: integer);
begin
    FConnection.Check(ErrorCode);
end;

procedure TSQLiteCommand.BindParams;
var
  Count, i: integer;
  Param: TParamDesc;
  str_val: AnsiString;
  int_val: int64;
  float_val: double;
begin
  Count := sqlite3_bind_parameter_count(FStmt);
  if Count <> Params.Count then
    raise Exception.CreateFmt(SIncorrectParamCount, [Count, Params.Count]);

  for i := 0 to Count - 1 do begin
    Param := Params[i];

    if Param.GetNull then begin
      Check(sqlite3_bind_null(FStmt, i + 1));
      continue;
    end;

    case Param.GetDataType of
      dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32, dtBoolean: begin
        int_val := Param.Value;
        Check(sqlite3_bind_int64(FStmt, i + 1, int_val));
      end;
      dtFloat, dtCurrency, dtBCD{$IFNDEF FPC}, dtFMTBCD{$ENDIF}: begin
        float_val := Param.Value;
        Check(sqlite3_bind_double(FStmt, i + 1, float_val));
      end;
      dtDate: begin
        str_val := AnsiString(FormatDateTime('yyyy-mm-dd', Param.Value));
        Check(sqlite3_bind_text(FStmt, i + 1, PAnsiChar(str_val), Length(str_val),
          SQLITE_TRANSIENT));
      end;
      dtTime: begin
        str_val := AnsiString(FormatDateTime('hh'':''nn'':''ss', Param.Value));
        Check(sqlite3_bind_text(FStmt, i + 1, PAnsiChar(str_val), Length(str_val),
          SQLITE_TRANSIENT));
      end;
      dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
        str_val := AnsiString(FormatDateTime('yyyy-mm-dd hh'':''nn'':''ss', Param.Value));
        Check(sqlite3_bind_text(FStmt, i + 1, PAnsiChar(str_val), Length(str_val),
          SQLITE_TRANSIENT));
      end;
      dtBlob, dtBytes, dtVarBytes:
        BindBlob(i, Param.Value);
    else
      BindMemo(i, Param.Value);
    end;
  end;
end;

procedure TSQLiteCommand.BindBlob(Index: integer; const Value: variant);
var
  s: AnsiString;
  Obj: TObject;
  Blob: TBlob;
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
      Blob.Defrag;
      BlobData := PtrOffset(Blob.FirstPiece, SizeOf(TPieceHeader));
      Check(sqlite3_bind_blob(FStmt, Index + 1, BlobData, Blob.FirstPiece.Used,
        SQLITE_STATIC));
    end;
  {$IFNDEF CLR}
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        Check(sqlite3_bind_blob(FStmt, Index + 1, BlobData, hb - lb + 1,
          SQLITE_TRANSIENT));
      finally
        VarArrayUnlock(Value);
      end;
    end;
  {$ENDIF}
  else
    s := AnsiString(VarToStr(Value));
  {$IFNDEF CLR}
    BlobData := PAnsiChar(s);
  {$ELSE}
    BlobData := Marshal.AllocHGlobal(Length(s));
    Marshal.Copy(TBytes(s), 0, BlobData, Length(s));
  {$ENDIF}
    try
      Check(sqlite3_bind_blob(FStmt, Index + 1, BlobData, Length(s),
        SQLITE_TRANSIENT));
    finally
    {$IFDEF CLR}
      Marshal.FreeHGlobal(BlobData);
    {$ENDIF}
    end;
  end;
end;

procedure TSQLiteCommand.BindMemo(Index: integer; const Value: variant);
var
  ws: WideString;
  sa: AnsiString;
  Obj: TObject;
  Blob: TBlob;
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
      ws := Blob.AsWideString;
    end;
  else
    ws := VarToWideStr(Value);
  end;

  // encode string: UTF8 or ASCII
  sa := FConnection.EncodeString(ws);

  Check(sqlite3_bind_text(FStmt, Index + 1, PAnsiChar(sa), Length(sa),
    SQLITE_TRANSIENT));
end;

{ TSQLiteRecordSet }

constructor TSQLiteRecordSet.Create;
begin
  inherited Create;

  FFetchAll := False;
  FFetchRows := 25;
end;

destructor TSQLiteRecordSet.Destroy;
begin
  Close;

  inherited;
end;

function TSQLiteRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDumpData:
      FDumpData := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TSQLiteRecordSet.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRowsFetched:
      Value := FRecordCount;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TSQLiteRecordSet.ExecCommand;
var
  NeedPrepare: boolean;
begin
  FCommand.FLockAfterExecute := True;
  try
    NeedPrepare := not Prepared;
    if NeedPrepare then
      InternalPrepare;

    try
      inherited;

      if CommandType = ctCursor then begin
        if FCommand.FExecResult = SQLITE_DONE then
          FCommand.SetCursorState(csFetched)
        else
          FCommand.SetCursorState(csExecuted)
      end;

    except
      if NeedPrepare then
        InternalUnprepare;
      raise;
    end;

    if CommandType <> ctCursor then
      if NeedPrepare then
        InternalUnprepare;

  except
    FCommand.FLockAfterExecute := False;
    if Assigned(FCommand.AfterExecute) then
      FCommand.AfterExecute(False);
    raise;
  end;
  FCommand.FLockAfterExecute := False;
  if Assigned(FCommand.AfterExecute) then
    FCommand.AfterExecute(True);
end;

function TSQLiteRecordSet.IsFullReopen: boolean;
begin
  Result := False;
end;

procedure TSQLiteRecordSet.Reopen;
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

procedure TSQLiteRecordSet.SetToEnd;
begin
  FetchAll;

  inherited;
end;

procedure TSQLiteRecordSet.Check(ErrorCode: integer);
begin
  FCommand.FConnection.Check(ErrorCode);
end;

procedure TSQLiteRecordSet.CreateCommand;
begin
  SetCommand(TSQLiteCommand.Create);
end;

procedure TSQLiteRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TSQLiteCommand(Value);
end;

procedure TSQLiteRecordSet.InternalPrepare;
begin
  inherited;

  if sqlite3_column_count(FCommand.FStmt) > 0 then
    CommandType := ctCursor
  else
    CommandType := ctStatement;
end;

procedure TSQLiteRecordSet.InternalUnPrepare;
begin
  inherited;

  CommandType := ctUnknown;
end;

procedure TSQLiteRecordSet.DescribeFieldDesc(Field: TCRFieldDesc; Index: integer);
var
  TableName, DatabaseName, DataTypeName, s: _string;
  TableInfo: TCRTableInfo;
  DataType, p1, p2, Len, Scale: integer;
begin
  Field.FieldNo := Index + 1;
  Field.ActualFieldNo := Index + 1;
  Field.Name := FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_name(FCommand.FStmt, Index)));
  if IsMetaDataAPIAvailable then
    Field.ActualName := FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_origin_name(FCommand.FStmt, Index)))
  else
    Field.ActualName := Field.Name;

  Field.TableInfo := nil;
  if IsMetaDataAPIAvailable then begin
    TableName := FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_table_name(FCommand.FStmt, Index)));
    if TableName <> '' then begin
      // TableName can containt "."
      TableName := SQLiteInfo.QuoteIfNeed(TableName);
      TableInfo := TablesInfo.FindByName(TableName);
      if TableInfo = nil then begin
        TableInfo := TablesInfo.Add;
        TableInfo.TableName := TableName;
        TableInfo.TableAlias := '';
        DatabaseName := FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_database_name(FCommand.FStmt, Index)));
        if DatabaseName <> '' then
          TableName := DatabaseName + '.' + TableName;
        TableInfo.TableNameFull := TableName;
      end;
      Field.TableInfo := TableInfo;
    end;
  end;

  if FDumpData then
    Field.DataType := dtWideMemo
  else begin
    DataTypeName := _UpperCase(FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_decltype(FCommand.FStmt, Index))));
    if DataTypeName <> '' then begin
      // set DataType basing on column affinity
      Len := 0;
      Scale := 0;
      p1 := Pos('(', DataTypeName);
      p2 := Pos(')', DataTypeName);
      if (p1 > 0) and (p2 > 0) then begin
        s := Trim(Copy(DataTypeName, p1 + 1, p2 - p1 - 1));
        DataTypeName := Copy(DataTypeName, 1, p1 - 1);
        p1 := Pos(',', s);
        if p1 > 0 then begin
          Len := StrToIntDef(Trim(Copy(s, 1, p1 - 1)), 0);
          Scale := StrToIntDef(Trim(Copy(s, p1 + 1, Length(s) - p1)), 0);
        end
        else
          Len := StrToIntDef(Trim(s), 0);
      end;
      if (DataTypeName = 'INT2') or
         (DataTypeName = 'SMALLINT') or
         (DataTypeName = 'SMALL INT') or
         (DataTypeName = 'TINYINT') or
         (DataTypeName = 'TINY INT')
      then
        Field.DataType := dtSmallint // Int16
      else
      if (DataTypeName = 'BIGINT') or
         (DataTypeName = 'BIG INT') or
         (DataTypeName = 'UNSIGNED BIG INT') or
         (DataTypeName = 'UNSIGNED BIGINT') or
         (DataTypeName = 'INT8')
      then
        Field.DataType := dtLargeint // Int64
      else
      if (DataTypeName = 'INT') or
         (DataTypeName = 'INTEGER') or
         (DataTypeName = 'MEDIUMINT') or
         (DataTypeName = 'MEDIUM INT')
      then
        Field.DataType := dtInteger // Int32
      else
      // other integers
      if Pos('INT', DataTypeName) > 0 then
        Field.DataType := dtInteger
      else
      if (Pos('CHAR', DataTypeName) > 0) then begin
        Field.DataType := dtString;
        Field.Length := Len;
      end
      else
      if AnsiStartsStr('TEXT', DataTypeName) or
         AnsiStartsStr('CLOB', DataTypeName)
      then
        Field.DataType := dtMemo
      else
      if AnsiStartsStr('BLOB', DataTypeName) then
        Field.DataType := dtBlob
      else
      if AnsiStartsStr('REAL', DataTypeName) or
         AnsiStartsStr('FLOAT', DataTypeName) or
         AnsiStartsStr('DOUBLE', DataTypeName)
      then
        Field.DataType := dtFloat
      else
      if (DataTypeName = 'NUMERIC') or
         (DataTypeName = 'DECIMAL') or
         (DataTypeName = 'NUMBER')
      then begin
      {$IFNDEF FPC}
        if FCommand.EnableFMTBCD or FCommand.FConnection.EnableFMTBCD then begin
          Field.DataType := dtFMTBCD;
          if (Len > MaxFMTBcdDigits) or (Len <= 0) then
            Field.Length := MaxFMTBcdDigits
          else
            Field.Length := Len;
          if Scale > Field.Length then // if length was reduced
            Field.Scale := Field.Length
          else
            Field.Scale := Scale;
        end
        else
      {$ENDIF}
        if FCommand.EnableBCD or FCommand.FConnection.EnableBCD then begin
          Field.DataType := dtBCD;
          if (Len > MaxBcdPrecision) or (Len <= 0) then
            Field.Length := MaxBcdPrecision
          else
            Field.Length := Len;
          if Scale > MaxBcdScale then
            Field.Scale := MaxBcdScale
          else
            Field.Scale := Scale;
        end
        else
          Field.DataType := dtFloat;
      end
      else
      if DataTypeName = 'MONEY' then
        Field.DataType := dtCurrency
      else
      if AnsiStartsStr('BOOL', DataTypeName) then
        Field.DataType := dtBoolean
      else
      if (DataTypeName = 'BINARY') or
         (DataTypeName = 'VARBINARY')
      then begin
        Field.DataType := dtVarBytes;
        Field.Length := Len;
      end
      else
      if DataTypeName = 'DATE' then
        Field.DataType := dtDate
      else
      if DataTypeName = 'TIME' then
        Field.DataType := dtTime
      else
      if (DataTypeName = 'DATETIME') or
         (DataTypeName = 'TIMESTAMP') 
      then
        Field.DataType := dtDateTime
      else
        Field.DataType := dtString
    end
    else
    if Field.ActualName = '' then begin
      // set DataType basing on type of values from first record
      DataType := sqlite3_column_type(FCommand.FStmt, Index);
      case DataType of
        SQLITE_INTEGER:
          Field.DataType := dtLargeint;
        SQLITE_FLOAT:
          Field.DataType := dtFloat;
        SQLITE_TEXT, SQLITE_NULL:
          Field.DataType := dtString;
        SQLITE_BLOB:
          Field.DataType := dtBlob;
      else
        Assert(False);
      end;
    end
    else
      Field.DataType := dtString;

    if Field.DataType = dtString then begin
      if Field.Length <= 0 then
        Field.Length := DefaultStringFieldLength;
      if (Field.Length < 255) or
        (FLongStrings and (Field.Length <= DefaultStringFieldLength))
      then begin
        if (Field.Length >= FlatBufferLimit) and not FFlatBuffers then
          Field.DataType := dtExtString;
      end
      else
        Field.DataType := dtMemo;
    end
    else
    if Field.DataType = dtVarBytes then begin
      if (Field.Length > 0) and (Field.Length <= DefaultStringFieldLength)
      then begin
        if (Field.Length >= FlatBufferLimit) and not FFlatBuffers then
          Field.DataType := dtExtVarBytes;
      end
      else
        Field.DataType := dtBlob;
    end;

    if FCommand.FConnection.FUseUnicode then
      case Field.DataType of
        dtString:
          Field.DataType := dtWideString;
        dtExtString:
          Field.DataType := dtExtWideString;
        dtMemo:
          Field.DataType := dtWideMemo;
      end;
  end;

  case Field.DataType of
    dtString:
      Field.Size := Field.Length + 1;
    dtWideString:
      Field.Size := (Field.Length + 1) * 2;
    dtVarBytes:
      Field.Size := Field.Length + 2;
    dtSmallInt:
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

procedure TSQLiteRecordSet.InternalInitFields;
var
  OldCursorState: TCursorState;
  Field: TCRFieldDesc;
  FieldsCount: Integer;
  i: integer;
begin
  OldCursorState := FCommand.GetCursorState;
  if OldCursorState = csInactive then
    InternalPrepare;

  try
    if CommandType <> ctCursor then
      raise Exception.Create(SNotRows);

    if not IsMetaDataAPIAvailable then begin
      TablesInfo.CaseSensitive := False;
      FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, TablesInfo);
    end;  

    FieldsCount := sqlite3_column_count(FCommand.FStmt);
    for i := 0 to FieldsCount - 1 do begin
      Field := TCRFieldDesc(GetFieldDescType.Create);
      DescribeFieldDesc(Field, i);

      FFields.Add(Field);
    end;
  finally
    if OldCursorState = csInactive then
      InternalUnPrepare;
  end;
end;

procedure TSQLiteRecordSet.InternalClose;
begin
  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  inherited;
  if not Prepared then
    InternalUnprepare;
end;

function TSQLiteRecordSet.Fetch(FetchBack: boolean = False): boolean;
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
        InternalFetch;
        Result := True;
      except
        FEOF := True;
        raise;
      end;
    finally
      DoAfterFetch;
    end;
  end;

  if (FCommand.GetCursorState = csFetched) and not Prepared {AutoPrepare}
  then begin
    OldCommandType := CommandType;
    InternalUnPrepare;
    // We need to save old CommandType to save old FieldDescs on Refresh.
    CommandType := OldCommandType;
  end;
end;

function TSQLiteRecordSet.InternalFetch: boolean;

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
              dtBlob, dtMemo, dtWideMemo, dtVariant, dtExtString, dtExtWideString, dtExtVarBytes:
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

var
  pHBlock: PBlockHeader;
  NewBlock: Boolean;
  OldFirstItem, OldLastItem, Item: IntPtr;
  Row, Res: integer;
begin
  NewBlock := (IntPtr(BlockMan.FirstBlock) = nil) or not FUnidirectional;

  if NewBlock then
    BlockMan.AllocBlock(pHBlock, FFetchRows)
  else begin
    phBlock := BlockMan.FirstBlock; // overwrite data in unidirectional mode
    ClearBlock(pHBlock);
  end;
  InitBlock(pHBlock);

  OldFirstItem := FirstItem;
  OldLastItem := LastItem;
  Result := True;
  try
    Item := PtrOffset(phBlock, SizeOf(TBlockHeader));
    Row := 1;
    while True do begin
      if FDumpData then
        ReadFieldValuesForDump(PtrOffset(Item, SizeOf(TItemHeader)))
      else
        ReadFieldValues(PtrOffset(Item, SizeOf(TItemHeader)));

      Res := sqlite3_step(FCommand.FStmt);
      if Res = SQLITE_DONE then begin
        Result := False;
        break;
      end
      else
        if Res <> SQLITE_ROW then
          Check(Res);

      if Row = FFetchRows then
        break;
      Item := PtrOffset(Item, RecordSize + SizeOf(TItemHeader));
      Inc(Row);
    end;

    if FCommand.GetCursorState < csFetching then
      FCommand.SetCursorState(csFetching);

    if not Result then
      FCommand.SetCursorState(csFetched);

    CreateBlockStruct(pHBlock, Row);
    FRowsFetched := RecordCount;
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

procedure TSQLiteRecordSet.ReadFieldValues(RecBuf: IntPtr);

  procedure MoveToBufferAnsi(const Str: AnsiString; Buf: IntPtr; Len: integer);
  begin
  {$IFNDEF CLR}
    Move(Pointer(Str)^, Buf^, Len);
  {$ELSE}
    Marshal.Copy(TBytes(Str), 0, Buf, Len);
  {$ENDIF}
  end;

  procedure MoveToBufferUni(const Str: WideString; Buf: IntPtr; Len: integer);
  {$IFDEF CLR}
  var
    Bytes: TBytes;
  {$ENDIF}
  begin
  {$IFNDEF CLR}
    Move(Pointer(Str)^, Buf^, Len * 2);
  {$ELSE}
    SetLength(Bytes, Len * 2);
    Encoding.Unicode.GetBytes(Str, 0, Len, Bytes, 0);
    Marshal.Copy(Bytes, 0, Buf, Len * 2);
  {$ENDIF}
  end;

var
  i, ValueType, Len: integer;
  Field: TFieldDesc;
  FieldBuf, HeapBuf, BlobData: IntPtr;
  Blob: TBlob;
  Piece: PPieceHeader;
  val_int: integer;
  val_int64: int64;
  val_float: double;
  val_cur: currency;
  val_bcd: TBcd;
  val_str: string;
  val_ws: WideString;
  val_as: AnsiString;
  val_blob: IntPtr;
  val_date: TDateTime;
{$IFDEF CLR}
  Bytes: TBytes;
{$ENDIF}
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ELSE}
  OldDateSeparator, OldTimeSeparator: char;
  OldDateFormat, OldTimeFormat: string;
{$ENDIF}
begin
  CreateComplexFields(RecBuf, True);

  for i := 0 to FFields.Count - 1 do begin
    Field := FFields[i];
    FieldBuf := PtrOffset(RecBuf, Field.Offset);
    ValueType := sqlite3_column_type(FCommand.FStmt, i);
    if ValueType = SQLITE_NULL then
      Marshal.WriteByte(RecBuf, DataSize + i, 1)
    else begin
      Marshal.WriteByte(RecBuf, DataSize + i, 0);
      case Field.DataType of
        dtSmallint: begin
          val_int := sqlite3_column_int(FCommand.FStmt, i);
          Marshal.WriteInt16(FieldBuf, val_int);
        end;
        dtInteger: begin
          val_int := sqlite3_column_int(FCommand.FStmt, i);
          Marshal.WriteInt32(FieldBuf, val_int);
        end;
        dtLargeint: begin
          val_int64 := sqlite3_column_int64(FCommand.FStmt, i);
          Marshal.WriteInt64(FieldBuf, val_int64);
        end;
        dtFloat, dtCurrency: begin
          val_float := sqlite3_column_double(FCommand.FStmt, i);
          Marshal.WriteInt64(FieldBuf,
            BitConverter.DoubleToInt64Bits(val_float));
        end;
        dtBCD: begin
          if ValueType = SQLITE_INTEGER then
            val_cur := sqlite3_column_int64(FCommand.FStmt, i)
          else
            val_cur := sqlite3_column_double(FCommand.FStmt, i);
        {$IFDEF CLR}
          val_int64 := Decimal.ToOACurrency(val_cur);
        {$ELSE}
          val_int64 := PInt64(@val_cur)^;
        {$ENDIF}
          Marshal.WriteInt64(FieldBuf, val_int64);
        end;
      {$IFNDEF FPC}
        dtFMTBCD: begin
          if ValueType = SQLITE_INTEGER then
            val_str := IntToStr(sqlite3_column_int64(FCommand.FStmt, i))
          else
            val_str := FloatToStr(sqlite3_column_double(FCommand.FStmt, i));
          val_bcd := StrToBcd(val_str);
        {$IFDEF CLR}
          Bytes := TBcd.ToBytes(val_bcd);
          Marshal.Copy(Bytes, 0, FieldBuf, SizeOfTBcd);
        {$ELSE}
          PBcd(FieldBuf)^ := val_bcd;
        {$ENDIF}
        end;
      {$ENDIF}
        dtBoolean: begin
          val_int64 := sqlite3_column_int64(FCommand.FStmt, i);
          if val_int64 <> 0 then
            val_int64 := 1;
          Marshal.WriteInt16(FieldBuf, val_int64);
        end;
        dtString: begin
          val_as := FCommand.FConnection.DecodeStringA(AnsiString(sqlite3_column_text(FCommand.FStmt, i)));
          Len := Min(Length(val_as), Field.Length);
          MoveToBufferAnsi(val_as, FieldBuf, Len);
          Marshal.WriteByte(FieldBuf, Len, 0);
        end;
        dtWideString: begin
          val_ws := FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_text(FCommand.FStmt, i)), True);
          Len := Min(Length(val_ws), Field.Length);
          MoveToBufferUni(val_ws, FieldBuf, Len);
          Marshal.WriteInt16(FieldBuf, Len * 2, 0);
        end;
        dtExtString: begin
          val_as := FCommand.FConnection.DecodeStringA(AnsiString(sqlite3_column_text(FCommand.FStmt, i)));
          Len := Min(Length(val_as), Field.Length);
          HeapBuf := StringHeap.NewBuf(Len + 1);
          MoveToBufferAnsi(val_as, HeapBuf, Len);
          Marshal.WriteByte(HeapBuf, Len, 0);
          Marshal.WriteIntPtr(FieldBuf, HeapBuf);
        end;
        dtExtWideString: begin
          val_ws := FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_text(FCommand.FStmt, i)), True);
          Len := Min(Length(val_ws), Field.Length);
          HeapBuf := StringHeap.NewBuf((Len + 1) * 2);
          MoveToBufferUni(val_ws, HeapBuf, Len);
          Marshal.WriteInt16(HeapBuf, Len * 2, 0);
          Marshal.WriteIntPtr(FieldBuf, HeapBuf);
        end;
        dtMemo: begin
          val_as := FCommand.FConnection.DecodeStringA(AnsiString(sqlite3_column_text(FCommand.FStmt, i)));
          Len := Length(val_as);
          if Len > 0 then begin
            Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf)));
            Blob.AllocPiece(Piece, Len);
            BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
            MoveToBufferAnsi(val_as, BlobData, Len);
            Piece.Used := Len;
            Blob.AppendPiece(Piece);
          end;
        end;
        dtWideMemo: begin
          val_ws := FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_text(FCommand.FStmt, i)), True);
          Len := Length(val_ws);
          if Len > 0 then begin
            Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf)));
            Blob.AllocPiece(Piece, Len * 2);
            BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
            MoveToBufferUni(val_ws, BlobData, Len);
            Piece.Used := Len * 2;
            Blob.AppendPiece(Piece);
          end;
        end;
        dtBlob: begin
          Len := sqlite3_column_bytes(FCommand.FStmt, i);
          if Len > 0 then begin
            val_blob := sqlite3_column_blob(FCommand.FStmt, i);
            Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf)));
            Blob.AllocPiece(Piece, Len);
            BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
            CopyBuffer(val_blob, BlobData, Len);
            Piece.Used := Len;
            Blob.AppendPiece(Piece);
          end;
        end;
        dtVarBytes: begin
          Len := sqlite3_column_bytes(FCommand.FStmt, i);
          Marshal.WriteInt16(FieldBuf, Len);
          if Len > 0 then begin
            val_blob := sqlite3_column_blob(FCommand.FStmt, i);
            CopyBuffer(val_blob, PtrOffset(FieldBuf, 2), Len);
          end;
        end;
        dtExtVarBytes: begin
          Len := sqlite3_column_bytes(FCommand.FStmt, i);
          HeapBuf := StringHeap.NewBuf(Len + 2);
          Marshal.WriteInt16(HeapBuf, Len);
          if Len > 0 then begin
            val_blob := sqlite3_column_blob(FCommand.FStmt, i);
            CopyBuffer(val_blob, PtrOffset(HeapBuf, 2), Len);
          end;
          Marshal.WriteIntPtr(FieldBuf, HeapBuf);
        end;
        dtDate, dtTime, dtDateTime: begin
          val_str := FCommand.FConnection.DecodeStringW(AnsiString(sqlite3_column_text(FCommand.FStmt, i)));
        {$IFDEF VER7P}
          FmtSet.DateSeparator := '-';
          FmtSet.ShortDateFormat := 'yyyy-MM-dd';
          FmtSet.TimeSeparator := ':';
          FmtSet.ShortTimeFormat := 'hh:mm:ss';
          val_date := StrToDateTimeDef(val_str, 0, FmtSet);
        {$ELSE}
          OldDateSeparator := DateSeparator;
          OldDateFormat := ShortDateFormat;
          OldTimeSeparator := TimeSeparator;
          OldTimeFormat := LongTimeFormat;
          DateSeparator := '-';
          ShortDateFormat := 'yyyy-MM-dd';
          TimeSeparator := ':';
          ShortTimeFormat := 'hh:mm:ss';
          try
            val_date := StrToDateTimeDef(val_str, 0);
          finally
            DateSeparator := OldDateSeparator;
            ShortDateFormat := OldDateFormat;
            LongTimeFormat := OldTimeFormat;
            TimeSeparator := OldTimeSeparator;
          end;
        {$ENDIF}
          if Field.DataType = dtDate then
            val_date := Trunc(val_date)
          else
          if Field.DataType = dtTime then
            val_date := Abs(Frac(val_date));
          Marshal.WriteInt64(FieldBuf, BitConverter.DoubleToInt64Bits(val_date));
        end;
      else
        Assert(False);
      end;
    end;
  end;
end;

procedure TSQLiteRecordSet.ReadFieldValuesForDump(RecBuf: IntPtr);

  procedure MoveToBuffer(const Str: WideString; Buf: IntPtr; Len: integer);
  {$IFDEF CLR}
  var
    Bytes: TBytes;
  {$ENDIF}
  begin
  {$IFNDEF CLR}
    Move(Pointer(Str)^, Buf^, Len * 2);
  {$ELSE}
    SetLength(Bytes, Len * 2);
    Encoding.Unicode.GetBytes(Str, 0, Len, Bytes, 0);
    Marshal.Copy(Bytes, 0, Buf, Len * 2);
  {$ENDIF}
  end;

var
  i, ValueType, Len: integer;
  Field: TFieldDesc;
  FieldBuf, BlobData: IntPtr;
  Blob: TBlob;
  Piece: PPieceHeader;
  val_str: WideString;
  val_blob: IntPtr;
  sa: AnsiString;
{$IFDEF CLR}
  Bytes, Hex: TBytes;
{$ENDIF}
begin
  CreateComplexFields(RecBuf, True);

  for i := 0 to FFields.Count - 1 do begin
    Field := FFields[i];
    Marshal.WriteByte(RecBuf, DataSize + i, 0);
    FieldBuf := PtrOffset(RecBuf, Field.Offset);
    ValueType := sqlite3_column_type(FCommand.FStmt, i);
    case ValueType of
      SQLITE_INTEGER:
        val_str := IntToStr(sqlite3_column_int64(FCommand.FStmt, i));
      SQLITE_FLOAT:
        val_str := StringReplace(FloatToStr(sqlite3_column_double(FCommand.FStmt, i)),
          DecimalSeparator, '.', []);
      SQLITE_TEXT:
        val_str := FCommand.FConnection.DecodeStringW(QuotedStr(AnsiString(sqlite3_column_text(FCommand.FStmt, i))));
      SQLITE_BLOB: begin
        Len := sqlite3_column_bytes(FCommand.FStmt, i);
        val_blob := sqlite3_column_blob(FCommand.FStmt, i);
      {$IFDEF CLR}
        SetLength(Bytes, Len);
        Marshal.Copy(val_blob, Bytes, 0, Len);
        SetLength(Hex, Len * 2);
        BinToHex(Bytes, 0, Hex, 0, Len);
        sa := AnsiString(Hex);
      {$ELSE}
        SetLength(sa, Len * 2);
        BinToHex(val_blob, PAnsiChar(sa), Len);
      {$ENDIF}
        val_str := WideString('X''' + sa + '''');
      end;
      SQLITE_NULL:
        val_str := 'NULL';
    end;

    Len := Length(val_str);
    if Len > 0 then begin
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf)));
      Blob.AllocPiece(Piece, Len * 2);
      BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
      MoveToBuffer(val_str, BlobData, Len);
      Piece.Used := Len * 2;
      Blob.AppendPiece(Piece);
    end;
  end;
end;

{ TSQLiteTableInfo }

function TSQLiteTableInfo.GetTableNameFull: _string;
begin
  Result := FTableNameFull;
end;

procedure TSQLiteTableInfo.SetTableNameFull(const Value: _string);
begin
  FTableNameFull := Value;
end;

{ TSQLiteMetaData }

constructor TSQLiteMetaData.Create;
begin
  inherited;
end;

destructor TSQLiteMetaData.Destroy;
begin
  inherited;
end;

function TSQLiteMetaData.GetConnection: TSQLiteConnection;
begin
  Result := TSQLiteRecordSet(FRecordSet).FCommand.FConnection;
end;

function TSQLiteMetaData.GetTypesForSQL(const ObjectTypes: _string; AllTypes: array of _string): _string;
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

function TSQLiteMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TSQLiteRecordSet.Create;
end;

procedure TSQLiteMetaData.ParseTableSQL(const SQL: _string; var Members: TTableMembers);
var
  Lexem, s: _string;
  Parser: TLiteParser;
  BracketCount, Code, i, c, j: integer;
  IsLast, InDataType, InDefault, InConstraints: boolean;
begin
  Members.Columns := nil;
  Members.Indexes := nil;

  Parser := TLiteParser.Create(SQL);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    Parser.Uppered := False;
    Parser.QuotedString := True;
    if Parser.ToLexem('(') then begin
      BracketCount := 0;
      IsLast := False;
      InConstraints := False;
      c := -1;
      repeat
        Code := Parser.GetNext(Lexem);
        if (Code = lcSymbol) and (Lexem = '[') then
          Code := Parser.GetNext(Lexem);
        if (Code = lcIdent) or
           (Code > Parser.SymbolLexems.Count) and (Code <= Parser.SymbolLexems.Count + Parser.KeywordLexems.Count)
        then begin
          s := _LowerCase(Lexem);
          if (s = 'constraint') or (s = 'primary') or (s = 'foreign')
            or (s = 'check') or (s = 'unique')
          then begin
            InConstraints := True;
            break;
          end;
          c := Length(Members.Columns);
          SetLength(Members.Columns, c + 1);
          Members.Columns[c].Name := SQLiteInfo.UnQuote(Lexem);
        end
        else
          break;

        InDataType := True;
        InDefault := False;
        while True do begin
          Code := Parser.GetNext(Lexem);

          if (c >= 0) and
            ( (Code = lcIdent) or (Code > Parser.SymbolLexems.Count) and
            (Code <= Parser.SymbolLexems.Count + Parser.KeywordLexems.Count) )
          then begin
            s := _LowerCase(Lexem);
            if (s = 'primary') or (s = 'unique') then begin
              i := Length(Members.Indexes);
              SetLength(Members.Indexes, i + 1);
              if s = 'primary' then
                Members.Indexes[i].IndexType := itPrimaryKey
              else if s = 'unique' then
                Members.Indexes[i].IndexType := itUnique
              else
                Members.Indexes[i].IndexType := itNonUnique;
              SetLength(Members.Indexes[i].ColumnInfo, 1);
              Members.Indexes[i].ColumnInfo[0].ColumnIndex := c;
              InDataType := False;
              if s = 'primary' then begin
                if _LowerCase(Members.Columns[c].DataType) = 'integer' then
                  Members.Columns[c].IsAutoincrement := True;
                Parser.GetNextCode; // key
                Code := Parser.GetNext(Lexem);
                if Code <> lcEnd then
                  Members.Indexes[i].ColumnInfo[0].IsDesc := _LowerCase(Lexem) = 'desc';
              end;
            end
            else
            if (s = 'foreign') or (s = 'check') or (s = 'autoincrement')
              or (s = 'not') or (s = 'null')
            then
              InDataType := False
            else
            if s = 'default' then begin
              InDefault := True;
              InDataType := False;
              continue;
            end
            else
            if InDataType then begin
              if Members.Columns[c].DataType <> '' then
                Members.Columns[c].DataType := Members.Columns[c].DataType + ' ';
              Members.Columns[c].DataType := Members.Columns[c].DataType + Lexem;
            end;
          end;

          if (c >= 0) then
            if InDefault then begin
              Members.Columns[c].Default := Lexem;
              InDefault := False;
            end;

          if Code = lcEnd then begin
            IsLast := True;
            break;
          end;

          if (Lexem = ',') and (BracketCount = 0) then
            break
          else
          if Lexem = '(' then
            Inc(BracketCount)
          else
          if Lexem = ')' then
            if BracketCount = 0 then begin
              IsLast := True;
              break
            end
            else
              Dec(BracketCount);
        end;
      until IsLast;

      if not InConstraints then
        exit;

      IsLast := False;
      repeat
        s := _LowerCase(Lexem);
        if s = 'constraint' then
          repeat
            Code := Parser.GetNext(Lexem);
            s := _LowerCase(Lexem);
          until (Code = lcEnd) or (s = 'primary') or (s = 'foreign')
            or (s = 'check') or (s = 'unique');

        if (s = 'primary') or (s = 'unique') then begin
          if not Parser.ToLexem('(') then
            break;
          i := Length(Members.Indexes);
          SetLength(Members.Indexes, i + 1);
          if s = 'primary' then
            Members.Indexes[i].IndexType := itPrimaryKey
          else if s = 'unique' then
            Members.Indexes[i].IndexType := itUnique
          else
            Members.Indexes[i].IndexType := itNonUnique;
          Members.Indexes[i].ColumnInfo := nil;
          repeat
            Code := Parser.GetNext(Lexem);
            if (Code = lcEnd) or (Lexem = ')') then
              break;
            if (Lexem <> ',') then begin
              for c := 0 to Length(Members.Columns) - 1 do
                if _LowerCase(Lexem) = _LowerCase(Members.Columns[c].Name) then begin
                  j := Length(Members.Indexes[i].ColumnInfo);
                  SetLength(Members.Indexes[i].ColumnInfo, j + 1);
                  Members.Indexes[i].ColumnInfo[j].ColumnIndex := c;

                  Code := Parser.GetNext(Lexem); // asc, desc
                  if Code <> lcEnd then begin
                    Members.Indexes[i].ColumnInfo[j].IsDesc := _LowerCase(Lexem) = 'desc';
                    if Lexem = ')' then
                      Parser.Back;
                  end;
                  break;
                end;
            end;
          until False;
        end;

        while True do begin
          Code := Parser.GetNext(Lexem);

          if Code = lcEnd then begin
            IsLast := True;
            break;
          end;

          if (Lexem = ',') and (BracketCount = 0) then
            break
          else
          if Lexem = '(' then
            Inc(BracketCount)
          else
          if Lexem = ')' then
            if BracketCount = 0 then begin
              IsLast := True;
              break
            end
            else
              Dec(BracketCount);
        end;

        if IsLast then
          break;

        Code := Parser.GetNext(Lexem);
      until Code = lcEnd;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TSQLiteMetaData.ParseIndexSQL(const SQL: _string; var Members: TTableMembers);
var
  Lexem: _string;
  Parser: TLiteParser;
  Code, c, j: integer;
  s: string;
begin
  Members.Indexes := nil;

  Parser := TLiteParser.Create(SQL);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    Parser.Uppered := False;
    Parser.QuotedString := True;
    repeat
      Code := Parser.GetNext(Lexem);
      s := _LowerCase(Lexem);
      if s = 'create' then
        break;
    until Code = lcEnd;

    Parser.GetNext(Lexem);
    s := _LowerCase(Lexem);

    if Parser.ToLexem('(') then begin
      SetLength(Members.Indexes, 1);
      if s = 'unique' then
        Members.Indexes[0].IndexType := itUnique
      else
        Members.Indexes[0].IndexType := itNonUnique;
        
      repeat
        Code := Parser.GetNext(Lexem);
        if (Code = lcEnd) or (Lexem = ')') then
          break;
        if (Lexem <> ',') then begin
          Lexem := _LowerCase(SQLiteInfo.UnQuote(Lexem));
          for c := 0 to Length(Members.Columns) - 1 do
            if Lexem = _LowerCase(SQLiteInfo.UnQuote(Members.Columns[c].Name)) then begin
              j := Length(Members.Indexes[0].ColumnInfo);
              SetLength(Members.Indexes[0].ColumnInfo, j + 1);
              Members.Indexes[0].ColumnInfo[j].ColumnIndex := c;

              Code := Parser.GetNext(Lexem); // asc, desc
              if Code <> lcEnd then
                Members.Indexes[0].ColumnInfo[j].IsDesc := _LowerCase(Lexem) = 'desc';
              break;
            end;
        end;
      until False;
    end;
  finally
    Parser.Free;
  end;
end;

function TSQLiteMetaData.GetTables(Restrictions: _TStrings): TData;
const
  SQL = 'SELECT name, type ' +
    'FROM sqlite_master ' +
    'WHERE %s lower(type) IN (%s) ORDER BY name';

{$IFDEF DBX_METADATA}
  dnRECNO         = 1;
  dnCATALOG_NAME  = 2;
  dnSCHEMA_NAME   = 3;
  dnTABLE_NAME    = 4;
  dnTABLE_TYPE    = 5;
{$ELSE}
  dnCATALOG_NAME  = 1;
  dnSCHEMA_NAME   = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
{$ENDIF}
var
  WhereClause, TableName, TableTypes, QuotedTypes: _string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  TableTypes := Trim(Restrictions.Values['TABLE_TYPE']);

  WhereClause := '';
  AddWhere(WhereClause, 'lower(name)', WideLowerCase(TableName));
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';

  QuotedTypes := GetTypesForSQL(WideLowerCase(TableTypes), ['table', 'view']);

  FRecordSet.SetSQL(_Format(SQL, [WhereClause, QuotedTypes]));
  FRecordSet.Open;

  CreateTablesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
  {$IFDEF DBX_METADATA}
    CopyRecord([1], [dnTABLE_NAME]);
    if LowerCase(FRecordSetHelper.FieldValues[2]) = 'table' then
      FMemDataHelper.FieldValues[dnTABLE_TYPE] := 1
    else if LowerCase(FRecordSetHelper.FieldValues[2]) = 'view' then
      FMemDataHelper.FieldValues[dnTABLE_TYPE] := 2
    else
      FMemDataHelper.FieldValues[dnTABLE_TYPE] := 0; // unknown
  {$ELSE}
    CopyRecord([1, 2], [dnTABLE_NAME, dnTABLE_TYPE]);
  {$ENDIF}
    FMemDataHelper.AppendRecord;
  end;

  FRecordSet.Close;
  FMemData.SetToBegin;
  Result := FMemData;
end;

function TSQLiteMetaData.GetColumns(Restrictions: _TStrings): TData;
const
  SQL = 'SELECT sql FROM sqlite_master ' +
    'WHERE Lower(name) = ''%s'' AND type = ''table''';

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
  TableName, ColumnName, TableSQL: _string;
  Members: TTableMembers;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  FRecordSet.SetSQL(_Format(SQL, [_LowerCase(TableName)]));
  FRecordSet.Open;

  CreateColumnsFields;
  FMemData.Open;

  FRecordSetHelper.AllocBuffer;
  if FRecordSetHelper.NextRecord then begin
    TableSQL := _VarToStr(FRecordSetHelper.FieldValues[1]);
    if TableSQL <> '' then begin
      ParseTableSQL(TableSQL, Members);
      FMemDataHelper.AllocBuffer;
      for i := 0 to Length(Members.Columns) - 1 do begin
        if (ColumnName = '') or
          (_LowerCase(Members.Columns[i].Name) = _LowerCase(ColumnName))
        then begin
          FMemDataHelper.InitRecord;
          FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
          FMemDataHelper.FieldValues[dnCOLUMN_NAME] := Members.Columns[i].Name;
          FMemDataHelper.FieldValues[dnPOSITION] := i + 1;
          FMemDataHelper.FieldValues[dnDATA_TYPE] := Members.Columns[i].DataType;
          if Members.Columns[i].IsAutoincrement then
            FMemDataHelper.FieldValues[dnNULLABLE] := 0
          else
            FMemDataHelper.FieldValues[dnNULLABLE] := 1;
          FMemDataHelper.FieldValues[dnDEFAULT] := Members.Columns[i].Default;
          FMemDataHelper.AppendRecord;
        end;
      end;
    end;
  end;

  FRecordSet.Close;
  FMemData.SetToBegin;
  Result := FMemData;
end;

function TSQLiteMetaData.GetProcedures(Restrictions: _TStrings): TData;
begin
  CreateProceduresFields;
  FMemData.Open;
  Result := FMemData;
end;

function TSQLiteMetaData.GetProcedureParameters(Restrictions: _TStrings): TData;
begin
  CreateProcedureParametersFields;
  FMemData.Open;
  Result := FMemData;
end;

function TSQLiteMetaData.GetIndexes(Restrictions: _TStrings): TData;
const
  SQL1 = 'SELECT sql FROM sqlite_master ' +
    'WHERE Lower(name) = ''%s'' AND type = ''table''';

  SQL2 = 'SELECT name, sql FROM sqlite_master ' +
    'WHERE Lower(tbl_name) = ''%s'' AND type = ''index'' AND sql is not null';

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;

var
  TableName, DDL, IndName: _string;
  Members: TTableMembers;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  //IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  FRecordSet.SetSQL(_Format(SQL1, [_LowerCase(TableName)]));
  FRecordSet.Open;

  CreateIndexesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  FRecordSetHelper.AllocBuffer;
  if FRecordSetHelper.NextRecord then begin
    DDL := _VarToStr(FRecordSetHelper.FieldValues[1]);
    if DDL <> '' then begin
      ParseTableSQL(DDL, Members);
      for i := 0 to Length(Members.Indexes) - 1 do begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
        FMemDataHelper.FieldValues[dnINDEX_NAME] := 'autoindex' + IntToStr(i);
        FMemDataHelper.FieldValues[dnUNIQUE] := 1;
        FMemDataHelper.AppendRecord;
      end;
    end;
  end;
  FRecordSet.Close;

  FRecordSet.SetSQL(_Format(SQL2, [_LowerCase(TableName)]));
  FRecordSet.Open;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    IndName := _VarToStr(FRecordSetHelper.FieldValues[1]);
    DDL := _VarToStr(FRecordSetHelper.FieldValues[2]);
    if DDL <> '' then begin
      ParseIndexSQL(DDL, Members);
      for i := 0 to Length(Members.Indexes) - 1 do begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
        FMemDataHelper.FieldValues[dnINDEX_NAME] := IndName;
        if Pos('unique', _LowerCase(DDL)) > 0 then
          FMemDataHelper.FieldValues[dnUNIQUE] := 1
        else
          FMemDataHelper.FieldValues[dnUNIQUE] := 0;
        FMemDataHelper.AppendRecord;
      end;
    end;
  end;
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TSQLiteMetaData.GetIndexColumns(Restrictions: _TStrings): TData;
const
  SQL1 = 'SELECT sql FROM sqlite_master ' +
    'WHERE Lower(name) = ''%s'' AND type = ''table''';

  SQL2 = 'SELECT name, sql FROM sqlite_master ' +
    'WHERE Lower(tbl_name) = ''%s'' AND type = ''index'' AND sql is not null';

{$IFDEF DBX_METADATA}
  dnRECNO = 1;
  dnCATALOG_NAME = 2;
  dnSCHEMA_NAME = 3;
  dnTABLE_NAME = 4;
  dnINDEX_NAME = 5;
  dnCOLUMN_NAME = 6;
  dnPOSITION = 7;
  dnPKEY_NAME = 8;
  dnINDEX_TYPE = 9;
  dnSORT_ORDER = 10;
  dnFILTER = 11;
{$ELSE}
  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnINDEX_CATALOG   = 4;
  dnINDEX_SCHEMA    = 5;
  dnINDEX_NAME      = 6;
  dnCOLUMN_NAME     = 7;
  dnPOSITION        = 8;
  dnSORT_ORDER      = 9;
{$ENDIF}

var
  TableName, IndexName, Uniqueness: _string;
  DDL, IndName: _string;
{$IFDEF DBX_METADATA}
  IndType: Integer;
{$ENDIF}
  Members: TTableMembers;
  i, j: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);
  Uniqueness := Trim(Restrictions.Values['UNIQUE']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  CreateIndexColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  FRecordSet.SetSQL(_Format(SQL1, [_LowerCase(TableName)]));
  FRecordSet.Open;

  FRecordSetHelper.AllocBuffer;
  if FRecordSetHelper.NextRecord then begin
    DDL := _VarToStr(FRecordSetHelper.FieldValues[1]);
    if DDL <> '' then begin
      ParseTableSQL(DDL, Members);
      if Uniqueness <> '0' then begin
        for i := 0 to Length(Members.Indexes) - 1 do begin
          IndName := 'autoindex' + IntToStr(i);
          if (IndexName <> '') and (IndexName <> IndName) then
            continue;
        {$IFDEF DBX_METADATA}
          case Members.Indexes[i].IndexType of
            itPrimaryKey:
              IndType := 6;
            itUnique:
              IndType := 2
          else
            IndType := 1;
          end;
        {$ENDIF}

          for j := 0 to Length(Members.Indexes[i].ColumnInfo) - 1 do begin
            FMemDataHelper.InitRecord;
            FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
            FMemDataHelper.FieldValues[dnINDEX_NAME] := IndName;
            FMemDataHelper.FieldValues[dnCOLUMN_NAME] :=
              Members.Columns[Members.Indexes[i].ColumnInfo[j].ColumnIndex].Name;
            FMemDataHelper.FieldValues[dnPOSITION] := j + 1;
            if Members.Indexes[i].ColumnInfo[j].IsDesc then
              FMemDataHelper.FieldValues[dnSORT_ORDER] := 'DESC'
            else
              FMemDataHelper.FieldValues[dnSORT_ORDER] := 'ASC';
          {$IFDEF DBX_METADATA}
            FMemDataHelper.FieldValues[dnINDEX_TYPE] := IndType;
            if IndType = 6 then
              FMemDataHelper.FieldValues[dnPKEY_NAME] := IndName;
          {$ENDIF}
            FMemDataHelper.AppendRecord;
          end;
        end;
      end;
    end;
  end;
  FRecordSet.Close;

  FRecordSet.SetSQL(_Format(SQL2, [_LowerCase(TableName)]));
  FRecordSet.Open;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    IndName := _VarToStr(FRecordSetHelper.FieldValues[1]);
    if (IndexName <> '') and (IndexName <> IndName) then
      continue;
    DDL := _VarToStr(FRecordSetHelper.FieldValues[2]);
    if DDL <> '' then begin
      if ((Uniqueness <> '0') or (Pos('unique', _LowerCase(DDL)) = 0)) and
        ((Uniqueness <> '1') or (Pos('unique', _LowerCase(DDL)) > 0))
      then begin
        ParseIndexSQL(DDL, Members);
        for i := 0 to Length(Members.Indexes) - 1 do begin
        {$IFDEF DBX_METADATA}
          case Members.Indexes[i].IndexType of
            itPrimaryKey:
              IndType := 6;
            itUnique:
              IndType := 2
          else
            IndType := 1;
          end;
        {$ENDIF}
          for j := 0 to Length(Members.Indexes[i].ColumnInfo) - 1 do begin
            FMemDataHelper.InitRecord;
            FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
            FMemDataHelper.FieldValues[dnINDEX_NAME] := IndName;
            FMemDataHelper.FieldValues[dnCOLUMN_NAME] :=
              Members.Columns[Members.Indexes[i].ColumnInfo[j].ColumnIndex].Name;
            FMemDataHelper.FieldValues[dnPOSITION] := j + 1;
            if Members.Indexes[i].ColumnInfo[j].IsDesc then
              FMemDataHelper.FieldValues[dnSORT_ORDER] := 'DESC'
            else
              FMemDataHelper.FieldValues[dnSORT_ORDER] := 'ASC';
          {$IFDEF DBX_METADATA}
            FMemDataHelper.FieldValues[dnINDEX_TYPE] := IndType;
            if IndType = 6 then
              FMemDataHelper.FieldValues[dnPKEY_NAME] := IndName;
          {$ENDIF}
            FMemDataHelper.AppendRecord;
          end;
        end;
      end;
    end;
  end;
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TSQLiteMetaData.GetConstraints(Restrictions: _TStrings): TData;
begin
  CreateConstraintsFields;
  FMemData.Open;
  Result := FMemData;
end;

{ TLiteSQLInfo }

function TSQLiteInfo.IdentCase: TIdentCase;
begin
  Result := icMixed;
end;

function TSQLiteInfo.ParamQuoteAllowed: boolean;
begin
  Result := false;
end;

procedure TSQLiteInfo.ParseTablesInfo(const SQL: _string; TablesInfo: TCRTablesInfo);
var
  i: integer;
begin
  inherited ParseTablesInfo(SQL, TablesInfo);
  for i := 0 to TablesInfo.Count - 1 do
    TSQLiteTableInfo(TablesInfo[i]).TableNameFull := TablesInfo[i].TableName;
end;

{$IFNDEF LITE}

{ TSQLiteLoader }

constructor TSQLiteLoader.Create;
begin
  inherited;
end;

destructor TSQLiteLoader.Destroy;
begin
  inherited;
end;

function TSQLiteLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := inherited SetProp(Prop, Value);
end;

function TSQLiteLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited GetProp(Prop, Value);
end;

procedure TSQLiteLoader.CreateCommand;
begin
  FCommand := TSQLiteCommand.Create;
end;

class function TSQLiteLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TSQLiteRecordSet;
end;

{$ENDIF}

initialization
  ActiveConnectionsCount := 0;
  hLockConnectCount := TCriticalSection.Create;
  SQLiteInfo := TSQLiteInfo.Create(nil);

finalization
  hLockConnectCount.Free;
  SQLiteInfo.Free;

end.
