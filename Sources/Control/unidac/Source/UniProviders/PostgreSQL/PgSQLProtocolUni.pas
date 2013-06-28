
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgSQLProtocolUni;

{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, Types, Variants,
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text, System.Net.Sockets,
{$ELSE}
  CLRClasses,
{$ENDIF}
  MemUtils, CRVio,
{$IFNDEF UNIDACPRO}
  PgSQLNet, PgCall, PgError;
{$ELSE}
  PgSQLNetUni, PgCallUni, PgErrorUni;
{$ENDIF}

const
 { PostgreSQL protocol messages }

  PG_MSG_ASCIIROW = 'D';
  PG_MSG_AUTHENTICATION = 'R';

  //Authentication request types
  PG_MSG_AUTHENTICATION_OK = 0;
  PG_MSG_AUTHENTICATION_KERBEROSV4 = 1;
  PG_MSG_AUTHENTICATION_KERBEROSV5 = 2;
  PG_MSG_AUTHENTICATION_CLEARTEXTPASSWORD = 3;
  PG_MSG_AUTHENTICATION_CRYPTPASSWORD = 4;
  PG_MSG_AUTHENTICATION_MD5PASSWORD = 5;
  PG_MSG_AUTHENTICATION_SCMCREDENTIAL = 6;

  PG_MSG_BACKENDKEYDATA = 'K';
  PG_MSG_BINARYROW = 'B';

  PG_MSG_COMPLETED_RESPONSE = 'C';
  PG_MSG_CURSOR_RESPONSE = 'P';
  PG_MSG_EMPTY_QUERY_RESPONSE = 'I';
  PG_MSG_ERROR_RESPONSE = 'E';
  PG_MSG_FUNCTION_CALL = 'F';
  PG_MSG_FUNCTION_RESULT_RESPONSE = 'V';

  PG_MSG_FUNCTION_RESULT_NONEMPTY_RESPONSE = 'G';
  PG_MSG_FUNCTION_RESULT_VOID_RESPONSE = '0';
  PG_MSG_NOTICE_RESPONSE = 'N';
  PG_MSG_NOTIFICATION_RESPONSE = 'A';

  PG_MSG_QUERY = 'Q';
  PG_MSG_READY_FOR_QUERY = 'Z';
  PG_MSG_ROW_DESCRIPTION = 'T';
  PG_MSG_TERMINATE = 'X';

  PG_MSG_COPY_DATA = 'd';
  PG_MSG_COPY_DONE = 'c';
  PG_MSG_COPY_FAIL = 'f';
  PG_MSG_COPYIN_RESPONSE = 'G';
  PG_MSG_COPYOUT_RESPONSE = 'H';

  PG_MSG_FUNCTION_RESULT = 'V';

  // Protocol 3.0 only
  PG_MSG_PARAMETER_STATUS = 'S';
  PG_MSG_PARSE_COMPLETE = '1';
  PG_MSG_BIND_COMPLETE = '2';
  PG_MSG_CLOSE_COMPLETE = '3';
  PG_MSG_PARAMETER_DESCRIPTION = 't';
  PG_MSG_PORTAL_SUSPENDED = 's';
  PG_MSG_NODATA = 'n';

  PG_SSL_REQUEST_CODE = 80877103;
  PG_MSG_SSL_RESPONSE_OK = 'S';

  
type
  TPgSQLProtocol = class;
  TPgSQLProtocolClass = class of TPgSQLProtocol;

  TPgProtocolState = (psNone, psError, psAuthenticating, psAuthenticated, psBackendParemeters, psIsReadyForQuery,
    psStmtPreparing, psStmtBinding, psStmtExecuting, psStmtFetching, psFunctionExecuting, psFunctionExecuted,
    psCopyIn);
  
  TParsedStmtType =(stUnknown, stSelect, stCursor);

  TPgProtocolNoticeEvent = procedure(Errors: TPgErrors) of object;
  TPgProtocolNotificationEvent = procedure(const Name: AnsiString; const PID: integer; const PayLoad: AnsiString) of object;

  TBindParamProc = procedure(Dest: TPgSQLNet; ParamNo: integer; const ItemDesc: TPgSQLItemDesc) of object;
  TFetchFieldProc = procedure(Source: TPgSQLNet; Size: integer; FetchBlock: IntPtr; RowNo, FieldNo: integer) of object;


{ TPgSQLStatement }

  TPgSQLStatement = class
  private
    FSimpleQueryExecute: boolean;

    FStatementID: AnsiString;

    FStmtType: AnsiString;

    FLastInsertOID: Int64;
    FRowsAffected: integer;

    FAutoUnprepare: boolean;
    FFetchAll: boolean;
    FFetchSize: integer;
    FPrepared: boolean;
    FWithHold: boolean;
    FNoData: boolean;

    FParamsCount: Integer;
    FParams: TPgSQLItemDescs;

    FFieldsCount: Integer;
    FFields: TPgSQLItemDescs;
    FFieldNulls: TBytes;
  public
    constructor Create;
    destructor Destroy; override;

    property AutoUnprepare: boolean read FAutoUnprepare write FAutoUnprepare;
    property FetchAll: boolean read FFetchAll write FFetchAll;
    property FetchRows: Integer read FFetchSize write FFetchSize;
    property Prepared: boolean read FPrepared;
    property WithHold: boolean read FWithHold write FWithHold;
  end;

  PPgSQLStatement = IntPtr;

{ TPgSQLResultSet }   

  TPgSQLResultSet = class
  private
    FProtocol: TPgSQLProtocol;

    FValues: TStrings;
    FRecordsCount: integer;
    FFieldsCount: integer;

    procedure FetchFieldProc(Source: TPgSQLNet; Size: integer; FetchBlock: IntPtr;
      Row, FieldNo: integer);
    function GetValue(FieldNo, RecordNo: integer): AnsiString;
  public
    constructor Create(Protocol: TPgSQLProtocol);
    destructor Destroy; override;

    procedure Open(const SQL: AnsiString);

    property RecordsCount: integer read FRecordsCount;
    property Value[FieldNo, RecordNo: integer]: AnsiString read GetValue;
  end;

{ TPgSQLLargeObjectsAPI }  

  TPgSQLLargeObjectsAPI = class
    lo_creat: OID;
    lo_create: OID;
    lo_open: OID;
    lo_write: OID;
    lo_read: OID;
    lo_lseek: OID;
    lo_tell: OID;
    lo_close: OID;
    lo_unlink: OID;
  end;

  TPgSQLFuncDataType = (fdtInteger, fdtBuffer);

  TPgSQLFuncParam = record
    IsNull: boolean;
    Size: integer;
    case DataType: TPgSQLFuncDataType of
      fdtInteger: (VInteger: integer);
      fdtBuffer: (VPointer: IntPtr);
  end;

{$HPPEMIT 'struct TPgSQLFuncParam;'}

  TPgSQLFuncParams = array of TPgSQLFuncParam;

  _TSSLMode = (_smDisable, _smRequire, _smPrefer, _smAllow);

{ TPgSQLProtocol }   

  TPgSQLProtocol = class
  private
    FServer: AnsiString;
    FPort: integer;
    FConnectionTimeout: integer;
    FKey: Integer;
    FProcessID: Integer;
    FServerDefaults: TStrings;

    FServerVersionFull: AnsiString;
    FServerVersion: AnsiString;
    FMajorServerVersion: integer;
    FMinorServerVersion: integer;

    FStmtCounter: Int64;
    FOnNotice: TPgProtocolNoticeEvent;
    FOnNotification: TPgProtocolNotificationEvent;
    FLargeObjectsAPI: TPgSQLLargeObjectsAPI;
    FTerminated: boolean;

    procedure CheckLargeObjectsAPI;
  protected
    FNet: TPgSQLNet;
    FProtocolState: TPgProtocolState;
    FActiveStmt: TPgSQLStatement;
    FHasMoreResultSets: boolean;
    FActiveError: EPgError;
    FActiveNotices: TPgErrors;
    FIOHandler: TCRIOHandler;
    FSSLMode: _TSSLMode;
    FSSL_CACert: string;
    FSSL_Cert: string;
    FSSL_Key: string;
    FSSL_CipherList: string;

    function CreateVio(): TCRVio;
    procedure TryToInitiateSSL(Net: TPgSQLNet);
    procedure SetProtocolState(Value: TPgProtocolState; ActiveStmt: TPgSQLStatement = nil);

    function GenerateStmtID: AnsiString;
    function GenerateFetchCall(const Cursor: AnsiString; FetchAll: boolean; FetchSize: Integer): AnsiString;

    // server requesting functions
    procedure PutMsgStart(MsgType: AnsiChar); virtual;
    procedure PutMsgEnd; virtual;
    procedure Flush; virtual;

    // server response processing functions
    function ProcessMessage(Response: AnsiChar): Boolean; virtual;
    function GetSize: integer; virtual; abstract;

    // error control
    procedure RaiseError(E: EPgError); overload;
    procedure RaiseError(const Msg: AnsiString); overload;
    function ReadError(IsError: Boolean): EPgError; virtual; abstract;

    // connection control functions
    procedure StartConnection(const NeedSSL: boolean; const Database, UserName, ApplicationName: AnsiString);
    procedure SendStartUpMsg(const Database, UserName, ApplicationName: AnsiString); virtual; abstract;
    procedure Authenticate(const UserName, Password: AnsiString);
    procedure AuthenticateClearText(const Password: AnsiString); virtual; abstract;
    procedure AuthenticateCrypt(const Password: AnsiString); virtual; abstract;
    function CalMD5PasswordHash(const UserName, Password: AnsiString; const Salt: TBytes): AnsiString;
    procedure AuthenticateMD5(const UserName, Password: AnsiString); virtual; abstract;

    // server parameters
    procedure RequestServerVersion;

    // command execution functions
    procedure InternalExecuteStmt(Stmt: TPgSQLStatement; const SQL: AnsiString; ProcessResponse: boolean = True);
    procedure ReadColDescs; virtual; abstract;
    procedure ReadNotifications; virtual; abstract;
    procedure FetchRow(Stmt: TPgSQLStatement; RowNo: integer; FetchBlock: IntPtr;
      FetchFieldProc: TFetchFieldProc); virtual; abstract;
    procedure ReadCompleteStatus; virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    // connection constrol functions
    procedure SetSSLOptions(const SSLMode: _TSSLMode; IOHandler: TCRIOHandler;
      const SSL_CACert, SSL_Cert, SSL_Key, SSL_CipherList: string);
    procedure Connect(const Server: AnsiString; Port, ConnectionTimeout: integer;
      const Database, UserName, Password, ApplicationName: AnsiString);
    procedure Disconnect;
    function GetBackendPID: integer;
    procedure ProcessMessageQueue(RequiredResponse: AnsiChar = #0; WhileEqual: Boolean = False; Wait: boolean = False);
    procedure TerminateMessageLoop;

    // command execution functions
    procedure CloseStmt(Stmt: TPgSQLStatement); virtual;
    // simple query execution protocol
    procedure ExecuteStmt(Stmt: TPgSQLStatement; const SQL: AnsiString; ParsedSQLType: TParsedStmtType); virtual;
    // binary query execution protocol
    procedure PrepareStmt(Stmt: TPgSQLStatement; const SQL: AnsiString;
      ParsedSQLType: TParsedStmtType; const ParamTypes: TIntegerDynArray); virtual;
    procedure UnPrepareStmt(Stmt: TPgSQLStatement); virtual;
    procedure DescribeParams(Stmt: TPgSQLStatement; var Params: TPgSQLItemDescs);
    procedure BindPreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc); virtual;
    procedure ExecutePreparedStmt(Stmt: TPgSQLStatement); virtual;
    procedure BindExecutePreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc); virtual;
    
    procedure RequestCancel;
    procedure Ping(ProcessResponse: boolean = True); virtual; abstract;

    procedure DescribeFields(Stmt: TPgSQLStatement; var Fields: TPgSQLItemDescs);
    function FetchStmt(Stmt: TPgSQLStatement; Rows: integer; FetchBlock: IntPtr;
      FetchFieldProc: TFetchFieldProc): integer; virtual;

    function LastInsertOID(Stmt: TPgSQLStatement): Int64;
    function RowsAffected(Stmt: TPgSQLStatement): integer;
    function StatementType(Stmt: TPgSQLStatement): AnsiString;
    function RowsReturn(Stmt: TPgSQLStatement): boolean;
    function NoData(Stmt: TPgSQLStatement): boolean;
    function HasMoreResultSets: boolean;
    function IsVoidFunc(Stmt: TPgSQLStatement): boolean;
    function GetServerParameter(const Name: AnsiString): AnsiString;

    // server function call
    procedure CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
      var OutParam: TPgSQLFuncParam); virtual; abstract;
    procedure CallServerFuncInt(FuncOID: OID; InParams: array of integer;
      var OutParam: integer);

    // large objects protocol
    function lo_create(Mode: integer): OID;
    function lo_open(ObjOID: OID; Mode: integer): integer;
    function lo_write(ObjHandle: integer; Buffer: IntPtr; Count: integer): integer;
    function lo_read(ObjHandle: integer; Buffer: IntPtr; Count: integer): integer;
    function lo_lseek(ObjHandle: integer; Offset: integer; Origin: integer): integer;
    function lo_tell(ObjHandle: integer): integer;
    function lo_close(ObjHandle: integer): integer;
    function lo_unlink(ObjOID: OID): integer;

    // COPY command support
    procedure BeginCopyDataBlock(out Net: TPgSQLNet); virtual;
    procedure EndCopyDataBlock; virtual;
    procedure PutCopyEnd; virtual;

    procedure SetTimeout(Value: integer);

    property Net: TPgSQLNet read FNet;
    property ProtocolState: TPgProtocolState read FProtocolState;
    property ServerVersionFull: AnsiString read FServerVersionFull;
    property ServerVersion: AnsiString read FServerVersion;
    property MajorServerVersion: integer read FMajorServerVersion;
    property MinorServerVersion: integer read FMinorServerVersion;
    property OnNotice: TPgProtocolNoticeEvent read FOnNotice write FOnNotice;
    property OnNotification: TPgProtocolNotificationEvent read FOnNotification write FOnNotification;
  end;

  TPgSQLProtocol20 = class(TPgSQLProtocol)
  protected
    function ProcessMessage(Response: AnsiChar): Boolean; override;
    function GetSize: integer; override;

    procedure SendStartUpMsg(const Database, UserName, ApplicationName: AnsiString); override;
    procedure AuthenticateClearText(const Password: AnsiString); override;
    procedure AuthenticateCrypt(const Password: AnsiString); override;
    procedure AuthenticateMD5(const UserName, Password: AnsiString); override;

    function ReadError(IsError: Boolean): EPgError; override;

    procedure ReadColDescs; override;
    procedure ReadNotifications; override;
    procedure FetchRow(Stmt: TPgSQLStatement; RowNo: integer; FetchBlock: IntPtr;
      FetchFieldProc: TFetchFieldProc); override;
  public
    procedure CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
      var OutParam: TPgSQLFuncParam); override;

    procedure Ping(ProcessResponse: boolean = True); override;

    // COPY command support
    procedure EndCopyDataBlock; override;
    procedure PutCopyEnd; override;
  end;

  TPgSQLProtocol30 = class(TPgSQLProtocol)
  private
    FtxnStatus: AnsiChar;

    function GetPortalName(const StmtID: AnsiString): AnsiString;
    function GetStatementName(const StmtID: AnsiString): AnsiString;

    procedure ParseCommand(const StatementID: AnsiString; const SQL: AnsiString; const ParamTypes: TIntegerDynArray);
    procedure DescribeCommand(const StatementID: AnsiString);
    procedure BindCommand(const StatementID: AnsiString; Fields: TPgSQLItemDescs; Params: TPgSQLItemDescs; ParamsCount: Integer; BindParamProc: TBindParamProc);
    procedure ExecuteCommand(const StatementID: AnsiString; FetchSize: integer);

    procedure ClosePortalCommand(const StatementID: AnsiString);
    procedure CloseStatementCommand(const StatementID: AnsiString);

    procedure HoldCommand;
    procedure SyncCommand;
  protected
    procedure PutMsgStart(MsgType: AnsiChar); override;
    procedure PutMsgEnd; override;
    function ProcessMessage(Response: AnsiChar): Boolean; override;
    function GetSize: integer; override;

    function ReadError(IsError: Boolean): EPgError; override;

    procedure SendStartUpMsg(const Database, Username, ApplicationName: AnsiString); override;
    procedure AuthenticateClearText(const Password: AnsiString); override;
    procedure AuthenticateCrypt(const Password: AnsiString); override;
    procedure AuthenticateMD5(const UserName, Password: AnsiString); override;

    procedure ReadParamDescs;
    procedure ReadColDescs; override;
    procedure ReadNotifications; override;
    procedure FetchRow(Stmt: TPgSQLStatement; RowNo: integer; FetchBlock: IntPtr;
      FetchFieldProc: TFetchFieldProc); override;
    procedure ReadCopyInResponse;

    procedure ClosePortal(Stmt: TPgSQLStatement);
    procedure ClosePreparedStmt(Stmt: TPgSQLStatement);

    procedure Hold;
    procedure Sync;

  public
    constructor Create; override;

    procedure CloseStmt(Stmt: TPgSQLStatement); override;

    procedure PrepareStmt(Stmt: TPgSQLStatement; const SQL: AnsiString;
      ParsedSQLType: TParsedStmtType; const ParamTypes: TIntegerDynArray); override;
    procedure UnPrepareStmt(Stmt: TPgSQLStatement); override;
    procedure BindPreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc); override;
    procedure ExecutePreparedStmt(Stmt: TPgSQLStatement); override;
    procedure BindExecutePreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc); override;

    procedure Ping(ProcessResponse: boolean = True); override;

    procedure CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
      var OutParam: TPgSQLFuncParam); override;

    // COPY command support
    procedure BeginCopyDataBlock(out Net: TPgSQLNet); override;
    procedure EndCopyDataBlock; override;
    procedure PutCopyEnd; override;
  end;


implementation

uses
  DAConsts,
{$IFNDEF UNIDACPRO}
  PgConsts, PgCrypt,
{$ELSE}
  PgConstsUni, PgCryptUni,
{$ENDIF}
{$IFDEF HAVE_OPENSSL}
  CRVioTcpSSL,
{$ENDIF}
  CRVioTcp;


function StringToSeverity(const Str: AnsiString): TPgSeverity;
begin
  if Str = 'ERROR' then
    Result := {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sError
  else
  if Str = 'FATAL' then
    Result := sFatal
  else
  if Str = 'PANIC' then
    Result := sPanic
  else
  if Str = 'WARNING' then
    Result := sWarning
  else
  if Str = 'NOTICE' then
    Result := sNotice
  else
  if Str = 'DEBUG' then
    Result := sDebug
  else
  if Str = 'INFO' then
    Result := sInfo
  else
  if Str = 'LOG' then
    Result := sLog
  else
    Result := {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sError;
end;

procedure SplitErrorMsg(var Msg: AnsiString; var Severity: TPgSeverity);
var
  Idx: integer;
  StrSev: AnsiString;
begin
  Idx := Pos(AnsiString(':'), Msg);
  if Idx > 0 then begin
    StrSev := Trim(Copy(Msg, 1, Idx - 1));
    Msg := Trim(Copy(Msg, Idx + 1, Length(Msg) - Idx));
    Severity := StringToSeverity(StrSev);
  end
  else begin
    Msg := Trim(Msg);
    Severity := {$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sError;
  end;
end;

{ TPgSQLStatement }

constructor TPgSQLStatement.Create;
begin
  inherited;

  FNoData := True;
  FFetchAll := True;
  FAutoUnprepare := False;
  FPrepared := False; 
end;

destructor TPgSQLStatement.Destroy;
begin
  inherited;
end;

{ TPgSQLResultSet }

constructor TPgSQLResultSet.Create(Protocol: TPgSQLProtocol);
begin
  inherited Create;

  FProtocol := Protocol;
  FValues := TStringList.Create;
end;

destructor TPgSQLResultSet.Destroy;
begin
  FValues.Free;

  inherited;
end;

procedure TPgSQLResultSet.FetchFieldProc(Source: TPgSQLNet; Size: integer;
  FetchBlock: IntPtr; Row, FieldNo: integer);
begin
  if Size = 0 then
    FValues.Add('')
  else
    FValues.Add(string(Source.ReadString(Size)));
end;

function TPgSQLResultSet.GetValue(FieldNo, RecordNo: integer): AnsiString;
begin
  Result := AnsiString(FValues[RecordNo * FFieldsCount + FieldNo]);
end;

procedure TPgSQLResultSet.Open(const SQL: AnsiString);
var
  Stmt: TPgSQLStatement;
begin
  FValues.Clear;
  FRecordsCount := 0;

  Stmt := TPgSQLStatement.Create;
  try
    FProtocol.InternalExecuteStmt(Stmt, SQL);

    FFieldsCount := Stmt.FFieldsCount;

    while not FProtocol.NoData(Stmt) do begin
      FValues.Capacity := FValues.Capacity + 25;
      Inc(FRecordsCount, FProtocol.FetchStmt(Stmt, 25, nil, FetchFieldProc));
    end;
  finally
    FProtocol.CloseStmt(Stmt);
    Stmt.Free;
  end;
end;
  
{ TPgSQLProtocol }

constructor TPgSQLProtocol.Create;
begin
  inherited Create;

  FNet := TPgSQLNet.Create;
  FServerDefaults := TStringList.Create;
end;

destructor TPgSQLProtocol.Destroy;
begin
  FNet.Free;
  FServerDefaults.Free;
  FLargeObjectsAPI.Free;

  inherited;
end;

procedure TPgSQLProtocol.SetSSLOptions(const SSLMode: _TSSLMode; IOHandler: TCRIOHandler;
  const SSL_CACert, SSL_Cert, SSL_Key, SSL_CipherList: string);
begin
  FSSLMode := SSLMode;
  FIOHandler := IOHandler;
  FSSL_CACert := StringReplace(SSL_CACert, '\', '/', [rfReplaceAll]);
  FSSL_Cert := StringReplace(SSL_Cert, '\', '/', [rfReplaceAll]);
  FSSL_Key := StringReplace(SSL_Key, '\', '/', [rfReplaceAll]);
  FSSL_CipherList := SSL_CipherList;
end;

procedure TPgSQLProtocol.Connect(const Server: AnsiString; Port, ConnectionTimeout: integer;
  const Database, UserName, Password, ApplicationName: AnsiString);
var
  useSSL: boolean;
  appName: AnsiString;
begin
  FServer := Server;
  FPort := Port;
  FConnectionTimeout := ConnectionTimeout;

  useSSL := False;
  appName := ApplicationName;

  while True do begin
    StartConnection(useSSL, Database, UserName, appName);

    try
      Authenticate(UserName, Password);
    except
      on E: Exception do begin
        if (not useSSL) and
           (FSSLMode = _smAllow) and
           (not(E is EPgError) or ((E is EPgError) and (EPgError(E).Severity = sFatal)))
        then begin
          // repeat connection with SSL
          useSSL := True;
          continue;
        end
        else
          raise;
      end;
    end;

    if FProtocolState <> psAuthenticated then
      Exit;

    // Get backend cancel key data and server parameters
    try
      ProcessMessageQueue(PG_MSG_BACKENDKEYDATA, False);
    except
      on E: EPgError do begin
        if (EPgError(E).Severity = sFatal) and
           (EPgError(E).ErrorCode = '42704')
        then begin
          // repeat connection without ApplicationName
          // PostgreSQL before ver 9.0 doesn't support ApplicationName
          appName := '';
          continue;
        end;
      end;
    end;

    // Check whether server is ready for query
    // if not this method throws exception
    ProcessMessageQueue(PG_MSG_NOTICE_RESPONSE, True);

    break;
  end;

  RequestServerVersion;
end;

procedure TPgSQLProtocol.Disconnect;
begin
  FNet.WriteChar(PG_MSG_TERMINATE);
  FNet.WriteInt32(4);
  try
    Flush;
  except
    on EPgError do ;
  end;
  FNet.Vio := nil;
end;

function TPgSQLProtocol.GetBackendPID: integer;
begin
  Result := FProcessID;
end;

procedure TPgSQLProtocol.CloseStmt(Stmt: TPgSQLStatement);
var
  ClStmt: TPgSQLStatement;
begin
  try
    if Stmt.FStatementID <> '' then begin
      ClStmt := TPgSQLStatement.Create;
      try
        InternalExecuteStmt(ClStmt, 'CLOSE ' + Stmt.FStatementID);
      finally
        ClStmt.Free;
      end;
    end;
  except
  end;
end;

procedure TPgSQLProtocol.ExecuteStmt(Stmt: TPgSQLStatement; const SQL: AnsiString;
  ParsedSQLType: TParsedStmtType);
var
  FinalSQL: AnsiString;
begin
  FinalSQL := '';

  Stmt.FStatementID := '';
  
  if (ParsedSQLType = stCursor) then
    Stmt.FStatementID := SQL
  else
  if (ParsedSQLType = stSelect) and (not Stmt.FFetchAll) then begin
    Stmt.FStatementID := GenerateStmtID;
    FinalSQL := 'DECLARE ' + Stmt.FStatementID + ' CURSOR ';
    if Stmt.FWithHold then
      FinalSQL := FinalSQL + 'WITH HOLD ';
    FinalSQL := FinalSQL + 'FOR ' + SQL + #13#10';';
  end;

  if Stmt.FStatementID <> '' then
    FinalSQL := FinalSQL + GenerateFetchCall(Stmt.FStatementID, Stmt.FFetchAll, Stmt.FFetchSize)
  else
    FinalSQL := SQL;

  InternalExecuteStmt(Stmt, FinalSQL);
end;

procedure TPgSQLProtocol.PrepareStmt(Stmt: TPgSQLStatement; const SQL: AnsiString;
  ParsedSQLType: TParsedStmtType; const ParamTypes: TIntegerDynArray);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.UnPrepareStmt(Stmt: TPgSQLStatement);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.DescribeParams(Stmt: TPgSQLStatement; var Params: TPgSQLItemDescs);
begin
  Assert(Stmt <> nil);
  Params := Stmt.FParams;
end;

procedure TPgSQLProtocol.BindPreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.ExecutePreparedStmt(Stmt: TPgSQLStatement);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.BindExecutePreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc);
begin
  Assert(False, 'Supported in 3.0 protocol only');
end;

procedure TPgSQLProtocol.RequestCancel;
const
  CANCEL_REQUEST_CODE = 1234 shl 16 or 5678;
var
  Net: TPgSQLNet;
begin
  Net := TPgSQLNet.Create;
  try
    Net.Vio := CreateVio;
    if FSSLMode in [_smRequire, _smPrefer] then
      TryToInitiateSSL(Net);

    // packet size
    Net.WriteInt32(16);
    // protocol version
    Net.WriteInt32(CANCEL_REQUEST_CODE);
    Net.WriteInt32(FProcessID);
    Net.WriteInt32(FKey);
    Net.FlushSend;
  finally
    Net.Free;
  end;
end;

procedure TPgSQLProtocol.DescribeFields(Stmt: TPgSQLStatement; var Fields: TPgSQLItemDescs);
begin
  Fields := Stmt.FFields;
end;

function TPgSQLProtocol.FetchStmt(Stmt: TPgSQLStatement; Rows: integer; FetchBlock: IntPtr;
  FetchFieldProc: TFetchFieldProc): integer;
begin
  Result := 0;
  FHasMoreResultSets := False;

  if FProtocolState <> psStmtFetching then begin
    if Stmt.FFetchAll then
      Exit;

    if Stmt.FSimpleQueryExecute then
      InternalExecuteStmt(Stmt, GenerateFetchCall(Stmt.FStatementID, Stmt.FFetchAll, Stmt.FFetchSize))
    else
      ExecutePreparedStmt(Stmt);
  end;

  while (Result < Rows) and (FProtocolState = psStmtFetching) do begin
    Assert(FActiveStmt = Stmt);

    FetchRow(Stmt, Result, FetchBlock, FetchFieldProc);

    Inc(Result);
    ProcessMessageQueue;
    if FHasMoreResultSets then
      break;
  end;

  if Stmt.FFetchAll then
    Stmt.FNoData := (FProtocolState <> psStmtFetching) or FHasMoreResultSets
  else
    Stmt.FNoData := Result <> Rows;
end;

function TPgSQLProtocol.LastInsertOID(Stmt: TPgSQLStatement): Int64;
begin
  Result := Stmt.FLastInsertOID;
end;

function TPgSQLProtocol.RowsAffected(Stmt: TPgSQLStatement): integer;
begin
  Result := Stmt.FRowsAffected;
end;

function TPgSQLProtocol.StatementType(Stmt: TPgSQLStatement): AnsiString;
begin
  Result := Stmt.FStmtType;
end;

function TPgSQLProtocol.RowsReturn(Stmt: TPgSQLStatement): boolean;
begin
  Result := Stmt.FFieldsCount > 0;
end;

function TPgSQLProtocol.NoData(Stmt: TPgSQLStatement): boolean;
begin
  Result := Stmt.FNoData;
end;

function TPgSQLProtocol.HasMoreResultSets: boolean;
begin
  Result := FHasMoreResultSets;
end;

function TPgSQLProtocol.IsVoidFunc(Stmt: TPgSQLStatement): boolean;
begin
  Result := (Stmt.FFieldsCount = 1) and (Stmt.FFields[0].TypeOID = SQL_VOID);
end;

function TPgSQLProtocol.GetServerParameter(const Name: AnsiString): AnsiString;
begin
  Result := AnsiString(FServerDefaults.Values[string(Name)]);
end;

procedure TPgSQLProtocol.CallServerFuncInt(FuncOID: OID; InParams: array of integer;
  var OutParam: integer);
var
  i: integer;
  InParamRecs: TPgSQLFuncParams;
  OutParamRec: TPgSQLFuncParam;
begin
  SetLength(InParamRecs, Length(InParams));
  for i := 0 to Length(InParams) - 1 do
    if VarIsNull(InParams[i]) then
      InParamRecs[i].IsNull := True
    else begin
      InParamRecs[i].IsNull := False;    
      case VarType(InParams[i]) of
        varInteger: begin
          InParamRecs[i].Size := 4;
          InParamRecs[i].DataType := fdtInteger;
          InParamRecs[i].VInteger := InParams[i]; 
        end;  
      else
        raise Exception.Create('');
      end;
    end;

  OutParamRec.DataType := fdtInteger;    

  CallServerFunc(FuncOID, InParamRecs, OutParamRec);
  
  OutParam := OutParamRec.VInteger;
end;

function TPgSQLProtocol.lo_create(Mode: integer): OID;
begin
  CheckLargeObjectsApi;

  CallServerFuncInt(FLargeObjectsAPI.lo_creat, [Mode], Result);
end;

function TPgSQLProtocol.lo_open(ObjOID: OID; Mode: integer): integer;
begin
  CheckLargeObjectsApi;
  
  CallServerFuncInt(FLargeObjectsAPI.lo_open, [ObjOID, Mode], Result);
end;

function TPgSQLProtocol.lo_write(ObjHandle: integer; Buffer: IntPtr; Count: integer): integer;
var
  WriteParams: TPgSQLFuncParams;
  ResultParam: TPgSQLFuncParam;
begin
  CheckLargeObjectsAPI;

  SetLength(WriteParams, 2);
  WriteParams[0].IsNull := False;
  WriteParams[0].DataType := fdtInteger;
  WriteParams[0].VInteger := ObjHandle;

  WriteParams[1].IsNull := False;
  WriteParams[1].DataType := fdtBuffer;
  WriteParams[1].Size := Count;
  WriteParams[1].VPointer := Buffer;

  ResultParam.DataType := fdtInteger;

  CallServerFunc(FLargeObjectsAPI.lo_write, WriteParams, ResultParam);
  Result := ResultParam.VInteger;
end;

function TPgSQLProtocol.lo_read(ObjHandle: integer; Buffer: IntPtr; Count: integer): integer;
var
  ReadParams: TPgSQLFuncParams; 
  ResultParam: TPgSQLFuncParam;
begin
  CheckLargeObjectsAPI;

  SetLength(ReadParams, 2);

  ReadParams[0].IsNull := False;
  ReadParams[0].DataType := fdtInteger;
  ReadParams[0].VInteger := ObjHandle;

  ReadParams[1].IsNull := False;
  ReadParams[1].DataType := fdtInteger;
  ReadParams[1].VInteger := Count;

  ResultParam.DataType := fdtBuffer;
  ResultParam.VPointer := Buffer;

  CallServerFunc(FLargeObjectsAPI.lo_read, ReadParams, ResultParam);

  Result := ResultParam.Size;
end;

function TPgSQLProtocol.lo_lseek(ObjHandle: integer; Offset: integer; Origin: integer): integer;
begin
  CheckLargeObjectsAPI;

  CallServerFuncInt(FLargeObjectsAPI.lo_lseek, [ObjHandle, Offset, Origin], Result);
end;

function TPgSQLProtocol.lo_tell(ObjHandle: integer): integer;
begin
  CheckLargeObjectsAPI;

  CallServerFuncInt(FLargeObjectsAPI.lo_tell, ObjHandle, Result);
end;

function TPgSQLProtocol.lo_close(ObjHandle: integer): integer;
begin
  CheckLargeObjectsAPI;

  CallServerFuncInt(FLargeObjectsAPI.lo_close, [ObjHandle], Result);
end;

function TPgSQLProtocol.lo_unlink(ObjOID: OID): integer;
begin
  CheckLargeObjectsAPI;

  CallServerFuncInt(FLargeObjectsAPI.lo_unlink, [ObjOID], Result);
end;

procedure TPgSQLProtocol.BeginCopyDataBlock(out Net: TPgSQLNet);
begin
  Assert(FProtocolState = psCopyIn);
  Net := FNet;
end;

procedure TPgSQLProtocol.EndCopyDataBlock;
begin
  Assert(FProtocolState = psCopyIn);
end;

procedure TPgSQLProtocol.PutCopyEnd;
begin
  Assert(FProtocolState = psCopyIn);
end;

procedure TPgSQLProtocol.SetTimeout(Value: integer);
begin
  Assert(FNet <> nil);
  Assert(FNet.Vio <> nil);

  FNet.Vio.Timeout := Value;
end;

function TPgSQLProtocol.CreateVio(): TCRVio;
var
  Mess: string;
begin
  if (FIOHandler <> nil) and (FIOHandler.HandlerType <> 'ssl') then
    Result := TCRVioHandler.Create(string(FServer), FPort, FIOHandler, FSSL_Key, FSSL_Cert, FSSL_CACert)
  else
  if FSSLMode = _smDisable then
    Result := TCRVioTCP.Create(FServer, FPort)
  else
  if FIOHandler <> nil then
    Result := TCRVioHandler.Create(string(FServer), FPort, FIOHandler, FSSL_Key, FSSL_Cert, FSSL_CACert)
  else
  {$IFDEF HAVE_OPENSSL}
    Result := TCRVioTcpSSL.Create(FServer, FPort,
      AnsiString(FSSL_Key), AnsiString(FSSL_Cert), AnsiString(FSSL_CACert), '', AnsiString(FSSL_CipherList));
  {$ELSE}
    raise EPgError.Create(sError, 'Can''t create SSL connection');
  {$ENDIF}

  try
    Result.Timeout := FConnectionTimeout;
    Result.Connect;
  except
    on E: Exception do begin
      Mess := Result.LastError;
      Result.Free;
      if E is SocketException then
        raise EPgError.Create(sFatal, Mess)
      else
        raise;
    end;
  end;
end;

procedure TPgSQLProtocol.TryToInitiateSSL(Net: TPgSQLNet);
var
  SSLAllowed: boolean;
begin
  // InitiateSSL
  Net.WriteInt32(8);
  Net.WriteInt32(PG_SSL_REQUEST_CODE);
  Net.FlushSend;

  SSLAllowed := Net.ReadChar = PG_MSG_SSL_RESPONSE_OK;

  if not SSLAllowed and (FSSLMode = _smRequire) then
    raise EPgError.Create(sFatal, 'SSL connection is not allowed');

  if SSLAllowed then begin
    if Net.Vio is TCRVioHandler then
      TCRVioHandler(Net.Vio).IsSecure := True
    else
  {$IFDEF HAVE_OPENSSL}
    if Net.Vio is TCRVioTcpSSL then
      TCRVioTcpSSL(Net.Vio).IsSecure := True
    else
  {$ENDIF}
      Assert(False);
  end;
end;

procedure TPgSQLProtocol.SetProtocolState(Value: TPgProtocolState; ActiveStmt: TPgSQLStatement = nil);
begin
  FProtocolState := Value;

  if FProtocolState in [psStmtPreparing, psStmtExecuting, psIsReadyForQuery]
  then
    FActiveStmt := ActiveStmt;

  if FProtocolState = psStmtFetching then
    FActiveStmt.FNoData := False;
end;

function TPgSQLProtocol.GenerateStmtID: AnsiString;
begin
  Result := 'ST' + AnsiString(IntToStr(Cardinal(Self{$IFDEF CLR}.GetHashCode{$ENDIF})) + IntToStr(FStmtCounter));
  if FStmtCounter = High(Int64) then
    FStmtCounter := 0
  else
    Inc(FStmtCounter);
end;

function TPgSQLProtocol.GenerateFetchCall(const Cursor: AnsiString; FetchAll: boolean; FetchSize: Integer): AnsiString;
begin
  if FetchAll then
    Result := 'FETCH ALL FROM ' + Cursor
  else
    Result := 'FETCH FORWARD ' + AnsiString(IntToStr(FetchSize)) + ' FROM ' + Cursor;
end;

procedure TPgSQLProtocol.PutMsgStart(MsgType: AnsiChar);
begin
  FNet.WriteChar(MsgType);
end;

procedure TPgSQLProtocol.PutMsgEnd;
begin
end;

procedure TPgSQLProtocol.Flush;
begin
  FNet.FlushSend;
end;

function TPgSQLProtocol.ProcessMessage(Response: AnsiChar): Boolean;
var
  ParamName: AnsiString;
  ParamValue: AnsiString;
begin
  Result := True;
  case Response of
    PG_MSG_BACKENDKEYDATA: begin
      FProcessID := FNet.ReadInt32;
      FKey := FNet.ReadInt32;
    end;
    PG_MSG_ERROR_RESPONSE: begin
      FActiveError := ReadError(True);
      Result := not FActiveError.IsFatalError;
    end;
    PG_MSG_NOTICE_RESPONSE: begin
      if FActiveNotices = nil then
        FActiveNotices := TPgErrors.Create;
      FActiveNotices.Add(ReadError(False));
    end;  
    PG_MSG_NOTIFICATION_RESPONSE:
      ReadNotifications;
    PG_MSG_PARAMETER_STATUS: begin
      ParamName := FNet.ReadString;
      ParamValue := FNet.ReadString;
      FServerDefaults.Values[string(ParamName)] := string(ParamValue);
    end;
    PG_MSG_READY_FOR_QUERY: begin
      SetProtocolState(psIsReadyForQuery);
      Result := False;
    end;
    PG_MSG_ROW_DESCRIPTION: begin
      FHasMoreResultSets := FProtocolState = psStmtFetching;
      ReadColDescs;
      Result := FActiveStmt.FSimpleQueryExecute;
    end;
    PG_MSG_ASCIIROW: begin
      SetProtocolState(psStmtFetching);
      Result := False;
    end;
    PG_MSG_COMPLETED_RESPONSE: begin
      ReadCompleteStatus;
    end;
    PG_MSG_NODATA: begin
      FNet.ReadInt32;
      Result := False;
    end;
    PG_MSG_EMPTY_QUERY_RESPONSE: begin
      FActiveError := EPgError.Create({$IFDEF UNIDACPRO}PgErrorUni{$ELSE}PgError{$ENDIF}.sError, SEmptySQLStatement);
//      Result := False;
    end;
    PG_MSG_PORTAL_SUSPENDED: begin
      FNet.ReadInt32;
      SetProtocolState(psIsReadyForQuery);
      Result := False;
    end;
    PG_MSG_COPYIN_RESPONSE: begin
      SetProtocolState(psCopyIn);
      Result := False;
    end;
    PG_MSG_FUNCTION_RESULT: begin
      SetProtocolState(psFunctionExecuted);
      Result := False;
    end;
  else
    RaiseError(AnsiString(sUnexpectedServerResponse) + ' : ' + Response);
  end;
end;

procedure TPgSQLProtocol.ProcessMessageQueue(RequiredResponse: AnsiChar; WhileEqual: Boolean; Wait: boolean);
var
  Response: AnsiChar;
begin
  Assert(FActiveError = nil);
  Assert(FActiveNotices = nil);

  Response := #255;
  if WhileEqual then
    Response := RequiredResponse;

  while Wait or ((Response = RequiredResponse) = WhileEqual) do begin
    if Wait then begin
      if FNet.GetReadBufferAvailable < 1 then
        FNet.Vio.WaitForData;
      if FTerminated then begin
        Wait := False;
        FTerminated := False;
      end;
    end;
    Response := FNet.ReadChar;
    if not ProcessMessage(Response) and not Wait then
      Break;
  end;

  if FActiveNotices <> nil then
    try
      if Assigned(FOnNotice) then
        FOnNotice(FActiveNotices);
    finally
      FActiveNotices.Free;
      FActiveNotices := nil;
    end;

  if FActiveError <> nil then
    try
      RaiseError(FActiveError);
    finally
      FActiveError := nil;
    end;
end;

procedure TPgSQLProtocol.TerminateMessageLoop;
begin
  FTerminated := True;
  Ping(False); // to unlock thread waiting for data in socket
end;

procedure TPgSQLProtocol.RaiseError(E: EPgError);
begin
  raise E;
end;

procedure TPgSQLProtocol.RaiseError(const Msg: AnsiString);
begin
  RaiseError(EPgError.Create(sFatal, string(Msg)));
end;

procedure TPgSQLProtocol.StartConnection(const NeedSSL: boolean;
  const Database, UserName, ApplicationName: AnsiString);
begin
  FNet.Vio := CreateVio();
  if (FSSLMode in [_smRequire, _smPrefer]) or NeedSSL then
    TryToInitiateSSL(FNet);

  SendStartUpMsg(Database, UserName, ApplicationName);
end;

procedure TPgSQLProtocol.Authenticate(const UserName, Password: AnsiString);
var
  Response: AnsiChar;
  AuthMethod: Integer;
begin
  AuthMethod := -1;

  SetProtocolState(psAuthenticating);

  while AuthMethod <> 0 do begin
    Response := FNet.ReadChar;

    if Response = PG_MSG_ERROR_RESPONSE then
      RaiseError(ReadError(True));

    if Response <> PG_MSG_AUTHENTICATION then
      RaiseError(AnsiString(SUnexpectedServerResponse));

    GetSize;

    AuthMethod := FNet.ReadInt32;
    case AuthMethod of
      PG_MSG_AUTHENTICATION_OK:
        SetProtocolState(psAuthenticated);
      PG_MSG_AUTHENTICATION_CLEARTEXTPASSWORD:
        AuthenticateClearText(Password);
      PG_MSG_AUTHENTICATION_CRYPTPASSWORD:
        // !!!This authentication method is allowed only for users, that was created with UNENCRYPTED password
        AuthenticateCrypt(Password);
      PG_MSG_AUTHENTICATION_MD5PASSWORD:
        AuthenticateMD5(UserName, Password);
      PG_MSG_AUTHENTICATION_KERBEROSV4, PG_MSG_AUTHENTICATION_KERBEROSV5,
      PG_MSG_AUTHENTICATION_SCMCREDENTIAL:
        RaiseError(AnsiString(SUnknownAuthMethod));
    else
      RaiseError(AnsiString(SUnknownAuthMethod));
    end;
  end;
end;

function TPgSQLProtocol.CalMD5PasswordHash(const UserName, Password: AnsiString; const Salt: TBytes): AnsiString;
var
  Hash: TBytes;
  UserWithPwd, UserWithSalt: TBytes;
  MD5: TMD5;
  MD5str: AnsiStringBuilder;
  i: Integer;
begin
  MD5str := nil;
  MD5 := TMD5.Create;
  try
    SetLength(UserWithPwd, Length(UserName) + Length(Password));
    Encoding.Default.GetBytes(Password, 0, Length(Password), UserWithPwd, 0);
    Encoding.Default.GetBytes(UserName, 0, Length(UserName), UserWithPwd, Length(Password));
    Hash := MD5.ComputeHash(UserWithPwd);

    MD5str := AnsiStringBuilder.Create(Length(Hash) * 2);
    for i := 0 to Length(Hash) - 1 do
      MD5str.Append(LowerCase(AnsiString(IntToHex(Hash[i], 2))));

    SetLength(UserWithSalt, MD5str.Length + Length(Salt));
    Encoding.Default.GetBytes(MD5str.ToString, 0, MD5str.Length, UserWithSalt, 0);
    Buffer.BlockCopy(Salt, 0, UserWithSalt, MD5str.Length, Length(Salt));
    Hash := MD5.ComputeHash(UserWithSalt);

    MD5str.Length := 0;
    MD5str.Append('md5');
    for i := 0 to Length(Hash) - 1 do
      MD5str.Append(LowerCase(AnsiString(IntToHex(Hash[i], 2))));

    Result := MD5str.ToString;
  finally
    MD5str.Free;
    MD5.Free;
  end;
end;

procedure TPgSQLProtocol.RequestServerVersion;
var
  RecordSet: TPgSQLResultSet;
  Pos: integer;
  i: integer;
begin
  RecordSet := TPgSQLResultSet.Create(Self);
  try
    RecordSet.Open('SELECT VERSION()');
    Assert(RecordSet.RecordsCount = 1, 'Unable to get server version');

    FServerVersionFull := RecordSet.Value[0, 0];

    Pos := 0;
    for i := 1 to Length(FServerVersionFull) do begin
      if (FServerVersionFull[i] = ' ') or (FServerVersionFull[i] = ',') then
        if Pos = 0 then
          Pos := i
        else begin
          FServerVersion := Copy(FServerVersionFull, Pos + 1, i - Pos - 1);
          Break;
        end;
    end;

    Pos := 1;
    for i := 1 to Length(FServerVersion) do
      if FServerVersion[i] = '.' then
        if Pos = 1 then begin
          FMajorServerVersion := StrToIntDef(string(Copy(FServerVersion, Pos, i - Pos)), 0);
          Pos := i + 1;
        end
        else begin
          FMinorServerVersion := StrToIntDef(string(Copy(FServerVersion, Pos, i - Pos)), 0);
          Break;
        end;
  finally
    RecordSet.Free;
  end;
end;

procedure TPgSQLProtocol.InternalExecuteStmt(Stmt: TPgSQLStatement; const SQL: AnsiString; ProcessResponse: boolean = True);
begin
  SetProtocolState(psStmtExecuting, Stmt);

  Stmt.FSimpleQueryExecute := True;

  // Send query to the server
  PutMsgStart('Q');
  FNet.WriteString(SQL);
  PutMsgEnd;

  Flush;

  if ProcessResponse then
    ProcessMessageQueue;
end;

procedure TPgSQLProtocol.ReadCompleteStatus;
var
  StatusStr: AnsiString;

  FirstSep: integer;
  SecondSep: integer;

  TypeStr: AnsiString;
  i: integer;
  ValCode: Integer;
begin
  GetSize;

  if FProtocolState = psFunctionExecuting then
    Exit;

  StatusStr := FNet.ReadString;

  FirstSep := 0;
  SecondSep := 0;
  for i := 1 to Length(StatusStr) do
    if (StatusStr[i] = ' ') then
      if FirstSep = 0 then
        FirstSep := i
      else
      if SecondSep = 0 then begin
        SecondSep := i;
        Break;
      end;

  if FirstSep = 0 then
    FirstSep := Length(StatusStr) + 1;

  TypeStr := Copy(StatusStr, 1, FirstSep - 1);

  FActiveStmt.FStmtType := TypeStr;

  if CompareText(TypeStr, AnsiString('INSERT')) = 0 then begin
    Val(string(Copy(StatusStr, FirstSep + 1, i - FirstSep - 1)), FActiveStmt.FLastInsertOID, ValCode);
    FirstSep := SecondSep;
  end
  else
    FActiveStmt.FLastInsertOID := 0;

  if (CompareText(TypeStr, AnsiString('INSERT')) = 0) or
    (CompareText(TypeStr, AnsiString('UPDATE')) = 0) or
    (CompareText(TypeStr, AnsiString('DELETE')) = 0)
  then
    Val(string(Copy(StatusStr, FirstSep + 1, Length(StatusStr) - 1)), FActiveStmt.FRowsAffected, ValCode)
  else
    FActiveStmt.FRowsAffected := 0;
end;

procedure TPgSQLProtocol.CheckLargeObjectsApi;
var
  SQL: AnsiString;
  ResultSet: TPgSQLResultSet;
  i: integer;
  FuncName: AnsiString;
  FuncOID: integer;  
begin
  if FLargeObjectsAPI = nil then
    FLargeObjectsAPI := TPgSQLLargeObjectsAPI.Create
  else  
    Exit;

  Assert((MajorServerVersion > 7) or ((MajorServerVersion = 7) and (MinorServerVersion >= 3)));

  SQL := 'select proname, oid from pg_catalog.pg_proc where proname in (' +
    '''lo_open'',''lo_close'', ''lo_creat'',''lo_create'', ''lo_unlink'', ''lo_lseek'',' +
    '''lo_tell'',''loread'',''lowrite'') and pronamespace = (select oid from pg_catalog.' +
    'pg_namespace where nspname = ''pg_catalog'')';

  ResultSet := TPgSQLResultSet.Create(Self);
  try
    ResultSet.Open(SQL);

      for i := 0 to ResultSet.RecordsCount - 1 do begin
        FuncName := ResultSet.Value[0, i];
        FuncOID := StrToInt(string(ResultSet.Value[1, i]));

        with FLargeObjectsAPI do
          if FuncName = 'lo_open' then
            lo_open := FuncOID
          else
          if FuncName = 'lo_close' then
            lo_close := FuncOID
          else
          if FuncName = 'lo_creat' then
            lo_creat := FuncOID
          else
          if FuncName = 'lo_create' then
            lo_create := FuncOID
          else
          if FuncName = 'lo_unlink' then
            lo_unlink := FuncOID
          else
          if FuncName = 'lo_lseek' then
            lo_lseek := FuncOID
          else
          if FuncName = 'lo_tell' then
            lo_tell := FuncOID
          else
          if FuncName = 'loread' then
            lo_read := FuncOID
          else
          if FuncName = 'lowrite' then
            lo_write := FuncOID;
      end;    
  finally
    ResultSet.Free;
  end;
end;

{ TPgSQLProtocol20 }

procedure TPgSQLProtocol20.CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
  var OutParam: TPgSQLFuncParam);
var
  i: integer;
  FuncExecResult: AnsiChar;
  ParamsCount: integer;
begin
  SetProtocolState(psFunctionExecuting);
  ParamsCount := Length(InParams);

  PutMsgStart('F');
  FNet.WriteString(' ');        // dummy string
  FNet.WriteInt32(FuncOID);     // function ID
  FNet.WriteInt32(ParamsCount); // # of format codes

  for i := 0 to ParamsCount - 1 do
    if InParams[i].IsNull then
      FNet.WriteInt32(-1)
    else
      case InParams[i].DataType of
        fdtInteger: begin
          FNet.WriteInt32(4);
          FNet.WriteInt32(InParams[i].VInteger);
        end;
        fdtBuffer: begin
          FNet.WriteInt32(InParams[i].Size);
          FNet.WriteBytes(InParams[i].VPointer, 0, InParams[i].Size);
        end;
      end;  

  PutMsgEnd;

  Flush;

  ProcessMessageQueue();

  Assert(FProtocolState = psFunctionExecuted);

  while True do begin
    FuncExecResult := FNet.ReadChar;
    case FuncExecResult of
      'G': begin
        OutParam.Size :=  FNet.ReadInt32;
        case OutParam.DataType of
          fdtInteger:
            OutParam.VInteger := FNet.ReadInt32;
          fdtBuffer:
            FNet.ReadBytes(OutParam.VPointer, 0, OutParam.Size);
        end;
      end;
      '0': begin
        FProtocolState := psIsReadyForQuery;
        Break;
      end;
    else
      RaiseError(AnsiString(SUnexpectedServerResponse));
    end;
  end;
  ProcessMessageQueue;
end;

procedure TPgSQLProtocol20.Ping(ProcessResponse: boolean = True);
var
  Stmt: TPgSQLStatement;
begin
  Stmt := TPgSQLStatement.Create;
  try
    InternalExecuteStmt(Stmt, '', ProcessResponse);
  finally
    Stmt.Free;
  end;
end;

procedure TPgSQLProtocol20.EndCopyDataBlock;
begin
  inherited;

  Flush;
end;

procedure TPgSQLProtocol20.PutCopyEnd;
begin
  inherited;

  Flush;
  ProcessMessageQueue;
end;

function TPgSQLProtocol20.ProcessMessage(Response: AnsiChar): Boolean;
begin
  Result := True;
  case Response of
    PG_MSG_EMPTY_QUERY_RESPONSE: begin
      FNet.ReadWord;
      Result := False;
    end;
    PG_MSG_CURSOR_RESPONSE:
      FNet.ReadString;
  else
    Result := inherited ProcessMessage(Response);
  end;
end;

function TPgSQLProtocol20.GetSize: integer;
begin
  Result := 0;
end;

procedure TPgSQLProtocol20.SendStartUpMsg(const Database, UserName, ApplicationName: AnsiString);
const
  ProtocolVersion20 = $20000;
begin
  // Startup packet size
  FNet.EnterSizeBlock;
  // Protocol version
  FNet.WriteInt32(ProtocolVersion20);
  FNet.WriteString(Database, 64);
  FNet.WriteString(UserName, 32);
  // Arguments
  FNet.WriteString('', 64);
  // Unused
  FNet.WriteString('', 64);
  // Optional tty
  FNet.WriteString('', 64);
  FNet.LeaveSizeBlock(True);

  Flush;
end;

procedure TPgSQLProtocol20.AuthenticateClearText(const Password: AnsiString);
begin
  FNet.EnterSizeBlock;
  FNet.WriteString(Password);
  FNet.LeaveSizeBlock(True);
  Flush;
end;

procedure TPgSQLProtocol20.AuthenticateCrypt(const Password: AnsiString);
var
  Salt: TBytes;
  SaltStr, Crypted: AnsiString;
begin
  SetLength(Salt, 2);
  FNet.ReadBytes(TValueArr(Salt), 0, 2);
  SaltStr := Encoding.Default.GetString(Salt, 0, 2);
  Crypted := TUnixCrypt.Crypt(SaltStr, Password);

  FNet.EnterSizeBlock;
  FNet.WriteString(Crypted);
  FNet.LeaveSizeBlock(True);
  Flush;
end;

procedure TPgSQLProtocol20.AuthenticateMD5(const UserName, Password: AnsiString);
var
  Salt: TBytes;
  PasswordHash: AnsiString;
begin
  SetLength(Salt, 4);
  FNet.ReadBytes(TValueArr(Salt), 0, 4);
  PasswordHash := CalMD5PasswordHash(UserName, Password, Salt);

  FNet.EnterSizeBlock;
  FNet.WriteString(PasswordHash);
  FNet.LeaveSizeBlock(True);
  Flush;
end;

function TPgSQLProtocol20.ReadError(IsError: Boolean): EPgError;
var
  Msg: AnsiString;
  Severity: TPgSeverity;
begin
  Msg := FNet.ReadString;

  SplitErrorMsg(Msg, Severity);

  Result := EPgError.Create(Severity, string(Msg));
  if IsError then
    SetProtocolState(psError);
end;

procedure TPgSQLProtocol20.ReadColDescs;
var
  i: Integer;
begin
  Assert(FActiveStmt <> nil);

  FActiveStmt.FFieldsCount := FNet.ReadWord;
  SetLength(FActiveStmt.FFieldNulls, (FActiveStmt.FFieldsCount + 7) div 8);

  SetLength(FActiveStmt.FFIelds, FActiveStmt.FFieldsCount);
  for i := 0 to FActiveStmt.FFieldsCount - 1 do
    with FActiveStmt.FFields[i] do begin
      FieldName := FNet.ReadString;
      TypeOid := FNet.ReadInt32;
      TypeSize := FNet.ReadWord;
      TypeModifier := FNet.ReadInt32;
    end;
end;

procedure TPgSQLProtocol20.FetchRow(Stmt: TPgSQLStatement; RowNo: integer;
  FetchBlock: IntPtr; FetchFieldProc: TFetchFieldProc);
var
  i: integer;
  Size: integer;
  IsNull: boolean;
  BytePos: integer;
  BitPos: integer;
begin
  FNet.ReadBytes(TValueArr(Stmt.FFieldNulls), 0, Length(Stmt.FFieldNulls));

  BytePos := 0;
  BitPos := 7;
  for i := 0 to Stmt.FFieldsCount - 1 do begin
    IsNull := Stmt.FFieldNulls[BytePos] shr BitPos and 1 = 0;
    Dec(BitPos);
    if BitPos = -1 then begin
      Inc(BytePos);
      BitPos := 7;
    end;

    if IsNull then
      Size := -1
    else
      Size := FNet.ReadInt32 - 4;

    FetchFieldProc(FNet, Size, FetchBlock, RowNo, i);
  end;  
end;

procedure TPgSQLProtocol20.ReadNotifications;
var
  PID: integer;
  Name: AnsiString;
begin
  PID := FNet.ReadInt32;
  Name := FNet.ReadString;
  if Assigned(FOnNotification) then
    FOnNotification(Name, PID, '');
end;

{ TPgSQLProtocol30 }

constructor TPgSQLProtocol30.Create;
begin
  inherited;
end;

function TPgSQLProtocol30.GetPortalName(const StmtID: AnsiString): AnsiString;
begin
  Result := 'PORTAL' + StmtID;
end;

function TPgSQLProtocol30.GetStatementName(const StmtID: AnsiString): AnsiString;
begin
  Result := 'PRSTMT' + StmtID;
end;

procedure TPgSQLProtocol30.HoldCommand;
begin
  FNet.WriteChar('H');
  FNet.WriteInt32(4);
end;

procedure TPgSQLProtocol30.SyncCommand;
begin
  FNet.WriteChar('S');
  FNet.WriteInt32(4);
end;

procedure TPgSQLProtocol30.ClosePortalCommand(const StatementID: AnsiString);
begin
  PutMsgStart('C');
  FNet.WriteChar('P');
  FNet.WriteString(GetPortalName(StatementID));
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.CloseStatementCommand(const StatementID: AnsiString);
begin
  PutMsgStart('C');
  FNet.WriteChar('S');
  FNet.WriteString(GetStatementName(StatementID));
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.ParseCommand(const StatementID: AnsiString; const SQL: AnsiString; const ParamTypes: TIntegerDynArray);
var
  i: integer;
begin
  // Parse statemenet
  PutMsgStart('P');
  FNet.WriteString(GetStatementName(StatementID));
  FNet.WriteString(SQL);
  FNet.WriteInt16(Length(ParamTypes)); // number of datatypes for paremeters
  for i := 0 to Length(ParamTypes) - 1 do
    FNet.WriteInt32(ParamTypes[i]);
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.DescribeCommand(const StatementID: AnsiString);
begin
  // Describe statement
  PutMsgStart('D');
  FNet.WriteChar('S');
  FNet.WriteString(GetStatementName(StatementID));
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.BindCommand(const StatementID: AnsiString; Fields: TPgSQLItemDescs; Params: TPgSQLItemDescs; ParamsCount: Integer; BindParamProc: TBindParamProc);
var
  i: integer;

  procedure SendFormatCodes(ItemDescs: TPgSQLItemDescs);
  var
    i: integer;
    IsItemsValid: boolean;
  begin
    IsItemsValid := True;
    for i := 0 to Length(ItemDescs) - 1 do
      if ItemDescs[i].FormatCode = 0 then
        IsItemsValid := False;

    if IsItemsValid then begin
      FNet.WriteInt16(1);
      FNet.WriteInt16(1);
    end
    else begin
      FNet.WriteInt16(Length(ItemDescs));
      for i := 0 to Length(ItemDescs) - 1 do
        FNet.WriteInt16(ItemDescs[i].FormatCode);
    end;
  end;

begin
  PutMsgStart('B');

  FNet.WriteString(GetPortalName(StatementID));
  FNet.WriteString(GetStatementName(StatementID));

  // params
  SendFormatCodes(Params);
  FNet.WriteInt16(ParamsCount);
  try
    for i := 0 to ParamsCount - 1 do
      BindParamProc(FNet, i, Params[i]);
  except
    PutMsgEnd;
    FNet.ClearWriteBuffer;
    raise;
  end;

  // fields
  SendFormatCodes(Fields);

  PutMsgEnd;
end;

procedure TPgSQLProtocol30.ExecuteCommand(const StatementID: AnsiString; FetchSize: integer);
begin
  PutMsgStart('E');
  FNet.WriteString(GetPortalName(StatementID));
  FNet.WriteInt32(FetchSize);
  PutMsgEnd;
end;

procedure TPgSQLProtocol30.CloseStmt(Stmt: TPgSQLStatement);
begin
  if not Stmt.FSimpleQueryExecute then begin
    // Portal is required when Bind and Execute is sent to the server in defferent Net packects
//    if (Stmt.FStatementID <> '') and (FProtocolState <> psCopyIn) then
//      try
//        ClosePortal(Stmt);
//      except
//      end;
  end
  else
    inherited;
end;

procedure TPgSQLProtocol30.PrepareStmt(Stmt: TPgSQLStatement; const SQL: AnsiString;
  ParsedSQLType: TParsedStmtType; const ParamTypes: TIntegerDynArray);
var
  FinalSQL: AnsiString;
begin
  Assert(Stmt.FStatementID = '');
  Stmt.FPrepared := False;
  Stmt.FAutoUnprepare := False; // reset value before preparing
  SetProtocolState(psStmtPreparing, Stmt);

  if ParsedSQLType = stCursor then
    FinalSQL := 'FETCH ALL FROM ' + SQL
  else
    FinalSQL := SQL;

  Stmt.FStatementID := GenerateStmtID;
  try
    ParseCommand(Stmt.FStatementID, FinalSQL, ParamTypes);
    DescribeCommand(Stmt.FStatementID);

    Sync;

    ProcessMessageQueue;
    ProcessMessageQueue;

    Stmt.FPrepared := True;
  except
    CloseStmt(Stmt);
    raise;
  end;

  SetProtocolState(psIsReadyForQuery);
end;

procedure TPgSQLProtocol30.UnPrepareStmt(Stmt: TPgSQLStatement);
begin
  try
    if not Stmt.FSimpleQueryExecute and (Stmt.FStatementID <> '') then begin
      ClosePreparedStmt(Stmt);
    end;
  except
  end;
end;

procedure TPgSQLProtocol30.BindPreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc);
begin
  // we should close portal before new bind
  // portal can be not close if statement was open in transaction
  // if we close not exist portal - we don't get any errors
  ClosePortalCommand(Stmt.FStatementID);

  BindCommand(Stmt.FStatementID, Stmt.FFields, Stmt.FParams, Stmt.FParamsCount, BindParamProc);
  Sync;
  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.ExecutePreparedStmt(Stmt: TPgSQLStatement);
begin
  SetProtocolState(psStmtExecuting, Stmt);

  ExecuteCommand(Stmt.FStatementID, Stmt.FFetchSize);
  Sync;
end;

procedure TPgSQLProtocol30.BindExecutePreparedStmt(Stmt: TPgSQLStatement; BindParamProc: TBindParamProc);
begin
  // we should close portal before new bind
  // portal can be not close if statement was open in transaction
  // if we close not exist portal - we don't get any errors
  ClosePortalCommand(Stmt.FStatementID);

  // Send Bind and Execute in the same Net packet increase performance highly !!!
  SetProtocolState(psStmtExecuting, Stmt);

  BindCommand(Stmt.FStatementID, Stmt.FFields, Stmt.FParams, Stmt.FParamsCount, BindParamProc);
  ExecuteCommand(Stmt.FStatementID, Stmt.FFetchSize);
  // statement can be unprepared before fetching !!!
  // so it acceptable for FetchAll=false also
  // so if we need unprepare in the same Net packet then send Close command
  if Stmt.FAutoUnprepare then begin
    CloseStatementCommand(Stmt.FStatementID);
    Stmt.FPrepared := False;
    Stmt.FAutoUnprepare := False;
  end;

  Sync;
  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.CallServerFunc(FuncOID: OID; InParams: TPgSQLFuncParams;
  var OutParam: TPgSQLFuncParam);
var
  i: integer;
  ParamsCount: integer;
begin
  SetProtocolState(psFunctionExecuting);
  ParamsCount := Length(InParams);

  PutMsgStart('F');

  FNet.WriteInt32(FuncOID);
  FNet.WriteInt16(1); //# of format codes
  FNet.WriteInt16(1); //format code: BINARY
  FNet.WriteInt16(ParamsCount);

  for i := 0 to ParamsCount - 1 do
    if InParams[i].IsNull then
      FNet.WriteInt32(-1)
    else
      case InParams[i].DataType of
        fdtInteger: begin
          FNet.WriteInt32(4);
          FNet.WriteInt32(InParams[i].VInteger);
        end;
        fdtBuffer: begin
          FNet.WriteInt32(InParams[i].Size);
          FNet.WriteBytes(InParams[i].VPointer, 0, InParams[i].Size);
        end;
      end;  

  FNet.WriteInt16(1); // result format code: BINARY
  PutMsgEnd;

  Hold;

  Assert(FProtocolState = psFunctionExecuted);

  FNet.ReadInt32; // packet length
  OutParam.Size :=  FNet.ReadInt32;
  case OutParam.DataType of
    fdtInteger: begin
      case OutParam.Size of
        2:
          OutParam.VInteger := Integer(FNet.ReadWord);
        4:
          OutParam.VInteger := FNet.ReadInt32;
      end;
    end;
    fdtBuffer:
      FNet.ReadBytes(OutParam.VPointer, 0, OutParam.Size);
  end;

  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.BeginCopyDataBlock(out Net: TPgSQLNet);
begin
  inherited BeginCopyDataBlock(Net);

  PutMsgStart(PG_MSG_COPY_DATA);
end;

procedure TPgSQLProtocol30.EndCopyDataBlock;
begin
  inherited;

  PutMsgEnd;
  Flush;
end;

procedure TPgSQLProtocol30.PutCopyEnd;
begin
  inherited;

  FNet.WriteChar(PG_MSG_COPY_DONE);
  FNet.WriteInt32(4);
  Flush;

  Sync;
end;

procedure TPgSQLProtocol30.PutMsgStart(MsgType: AnsiChar);
begin
  inherited;

  FNet.EnterSizeBlock;
end;

procedure TPgSQLProtocol30.PutMsgEnd;
begin
  FNet.LeaveSizeBlock(True);
end;

function TPgSQLProtocol30.ProcessMessage(Response: AnsiChar): Boolean;
begin
  Result := True;
  case Response of
    PG_MSG_BACKENDKEYDATA, PG_MSG_PARAMETER_STATUS, PG_MSG_EMPTY_QUERY_RESPONSE: begin
      FNet.ReadInt32;
      Result := inherited ProcessMessage(Response);
    end;
    PG_MSG_PARAMETER_DESCRIPTION:
      ReadParamDescs;
    PG_MSG_PARSE_COMPLETE: begin
      FNet.ReadInt32;
      Result := False;
    end;
    PG_MSG_BIND_COMPLETE: begin
      FNet.ReadInt32;
      Result := False;
    end;
    PG_MSG_CLOSE_COMPLETE:
      FNet.ReadInt32;
    PG_MSG_READY_FOR_QUERY: begin
      FNet.ReadInt32; // skip size
      FTxnStatus := FNet.ReadChar;
      Result := inherited ProcessMessage(Response);
    end;
    PG_MSG_COPYIN_RESPONSE: begin
      ReadCopyInResponse;
      Result := inherited ProcessMessage(Response);
    end;
    PG_MSG_PORTAL_SUSPENDED: begin
      if FActiveStmt.FSimpleQueryExecute then
        Result := inherited ProcessMessage(Response)
      else begin
        inherited ProcessMessage(Response);
        Result := True;
      end;
    end;
  else
    Result := inherited ProcessMessage(Response);
  end;
end;

function TPgSQLProtocol30.GetSize: integer;
begin
  Result := FNet.ReadInt32;
end;

function TPgSQLProtocol30.ReadError(IsError: Boolean): EPgError;
var
  Size: Integer;
  ParamChar: AnsiChar;
  ParamValue: AnsiString;

  Severity: TPgSeverity;
  Msg: AnsiString;

  ErrorCode: AnsiString;
  DetailMsg: AnsiString;
  Hint: AnsiString;
  Position: Integer;
  LineNumber: Integer;
  CallStack: AnsiString;
  FileName: AnsiString;
  Proc: AnsiString;  
begin
  Size := GetSize;

  // Check the messageLength value. If it is 1178686529, this would be the
  // "FATA" string
  if Size = 1178686529 then begin
    Msg := 'FATA' + FNet.ReadString;

    SplitErrorMsg(Msg, Severity);

    Result := EPgError.Create(Severity, string(Msg));
  end
  else begin
    Position := 0;
    LineNumber := 0;

    while True do begin
      ParamChar := FNet.ReadChar;
      if ParamChar = #0 then
        Break;

      ParamValue := FNet.ReadString;
      case ParamChar of
        'S':
          Severity := StringToSeverity(ParamValue);
        'C':
          ErrorCode := ParamValue;
        'M':
          Msg := ParamValue;
        'D':
          DetailMsg := ParamValue;
        'H':
          Hint := ParamValue;
        'P':
          Position := StrToInt(string(ParamValue));
        'L':
          LineNumber := StrToInt(string(ParamValue));
        'W':
          CallStack := ParamValue;
        'F':
          FileName := ParamValue;
        'R':
          Proc := ParamValue;
      end;
    end;

    Result := EPgError.Create(Severity, string(ErrorCode), string(Msg), string(DetailMsg),
      string(Hint), string(CallStack), string(FileName), string(Proc), Position, LineNumber);
  end;

  if IsError then
    SetProtocolState(psError);
end;

procedure TPgSQLProtocol30.SendStartUpMsg(const Database, Username, ApplicationName: AnsiString);
const
  ProtocolVersion30 = $30000;
begin
  FNet.EnterSizeBlock;
  FNet.WriteInt32(protocolVersion30);
  FNet.WriteString('user');
  FNet.WriteString(Username);
  FNet.WriteString('database');
  FNet.WriteString(Database);
  // ApplicationName was supported in PostgreSQL 9.0 
  if ApplicationName <> '' then begin
    FNet.WriteString('application_name');
    FNet.WriteString(ApplicationName);
  end;
  FNet.WriteByte(0);
  FNet.LeaveSizeBlock(True);
  Flush;
end;

procedure TPgSQLProtocol30.AuthenticateClearText(const Password: AnsiString);
begin
  PutMsgStart('p');
  FNet.WriteString(Password);
  PutMsgEnd;

  Flush;
end;

procedure TPgSQLProtocol30.AuthenticateCrypt(const Password: AnsiString);
var
  Salt: TBytes;
  SaltStr, Crypted: AnsiString;
begin
  SetLength(Salt, 2);
  FNet.ReadBytes(TValueArr(Salt), 0, 2);
  SaltStr := Encoding.Default.GetString(Salt, 0, 2);
  Crypted := TUnixCrypt.Crypt(SaltStr, Password);

  PutMsgStart('p');
  FNet.WriteString(Crypted);
  PutMsgEnd;
  Flush;
end;

procedure TPgSQLProtocol30.AuthenticateMD5(const UserName, Password: AnsiString);
var
  Salt: TBytes;
  PasswordHash: AnsiString;
begin
  SetLength(Salt, 4);
  FNet.ReadBytes(TValueArr(Salt), 0, 4);
  PasswordHash := CalMD5PasswordHash(UserName, Password, Salt);

  PutMsgStart('p');
  FNet.WriteString(PasswordHash);
  PutMsgEnd;
  Flush;
end;

procedure TPgSQLProtocol30.ReadParamDescs;
var
  i: Integer;
begin
  Assert(FActiveStmt <> nil);
  FNet.ReadInt32; // skip size

  FActiveStmt.FParamsCount := FNet.ReadWord;
  SetLength(FActiveStmt.FParams, FActiveStmt.FParamsCount);

  for i := 0 to FActiveStmt.FParamsCount - 1 do
    FActiveStmt.FParams[i].TypeOid := FNet.ReadInt32;
end;

procedure TPgSQLProtocol30.ReadColDescs;
var
  i: Integer;
begin
  Assert(FActiveStmt <> nil);
  FNet.ReadInt32; // skip size

  FActiveStmt.FFieldsCount := FNet.ReadWord;
  SetLength(FActiveStmt.FFields, FActiveStmt.FFieldsCount);
  for i := 0 to FActiveStmt.FFieldsCount - 1 do
    with FActiveStmt.FFields[i] do begin
      FieldName := FNet.ReadString;
      TableOid := FNet.ReadInt32;
      TableCol := FNet.ReadInt16;
      TypeOid := FNet.ReadInt32;
      TypeSize := FNet.ReadInt16;
      TypeModifier := FNet.ReadInt32;
      FormatCode := FNet.ReadInt16;
    end;
end;

procedure TPgSQLProtocol30.FetchRow(Stmt: TPgSQLStatement; RowNo: integer; FetchBlock: IntPtr;
  FetchFieldProc: TFetchFieldProc);
var
  i: integer;
  Size: integer;
begin
  GetSize;

  FNet.ReadWord; // fields count

  for i := 0 to Stmt.FFieldsCount - 1 do begin
    Size := FNet.ReadInt32;

    FetchFieldProc(FNet, Size, FetchBlock, RowNo, i);
  end;
end;

procedure TPgSQLProtocol30.ReadNotifications;
var
  PID: integer;
  Name: AnsiString;
  PayLoad: AnsiString;
begin
  FNet.ReadInt32;
  PID := FNet.ReadInt32;
  Name := FNet.ReadString;
  PayLoad := FNet.ReadString; // supported since PostgreSQL 9.0 (before is empty always)
  if Assigned(FOnNotification) then
    FOnNotification(Name, PID, PayLoad);
end;

procedure TPgSQLProtocol30.ReadCopyInResponse;
var
  ColCount, i: integer;
begin
  FNet.ReadInt32;
  FNet.ReadByte; // binaryTuples
  ColCount := FNet.ReadInt16;
  for i := 0 to ColCount - 1 do
    FNet.ReadInt16; // fformat
end;

procedure TPgSQLProtocol30.Hold;
begin
  HoldCommand;

  Flush;

  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.Sync;
begin
  SyncCommand;
  Flush;

  ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.Ping(ProcessResponse: boolean = True);
begin
  SyncCommand;
  Flush;

  if ProcessResponse then
    ProcessMessageQueue;
end;

procedure TPgSQLProtocol30.ClosePortal(Stmt: TPgSQLStatement);
begin
  ClosePortalCommand(Stmt.FStatementID);
  Sync;
end;

procedure TPgSQLProtocol30.ClosePreparedStmt(Stmt: TPgSQLStatement);
begin
  // avoid sending Close command if statement was unprepared automatically
  if not Stmt.FAutoUnprepare then begin
    CloseStatementCommand(Stmt.FStatementID);
    Sync;
    Stmt.FPrepared := False;
    Stmt.FAutoUnprepare := False;
  end;
end;

end.

