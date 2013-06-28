
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright (c) 1998-2011 Devart. All right reserved.
//  Connection Pool
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}
unit OraConnectionPoolUni;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  Classes, SyncObjs, CRConnectionPool, CRAccess, MemUtils, DASQLMonitor,
{$IFDEF CLR}
  System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  OraCall, OraClasses;
{$ELSE}
  OraCallUni, OraClassesUni;
{$ENDIF}

const
  prPoolingType = 101;

type
  TOraPoolingType = (optLocal, optOCI{$IFDEF MSWINDOWS}{$IFNDEF LITE}, optMTS{$ENDIF}{$ENDIF});

  TOraConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    Direct: boolean;
    HomeName: string;
    UseUnicode: boolean;
    ConnectMode: TConnectMode;
    UseOCI7: boolean;
    PoolingType: TOraPoolingType;
    StatementCache: boolean;
    StatementCacheSize: integer;
    ConnectionTimeout: integer;
    OptimizerMode: TOptimizerMode;
    ClientIdentifier: _string;
    Schema: _string;
    ProxyUserName: _string;
    ProxyPassword: _string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TOraLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;

  TOraOCIConnectionPool = class(TCRConnectionPool)
  private
    hOCISPool: pOCISPool;
    FPoolName: _string;

    procedure CreateOCIPool;
    procedure FreeOCIPool;
  protected
    procedure InternalPutConnection(CRConnection: TCRConnection); override;
  public
    destructor Destroy; override;

    function GetConnection: TCRConnection; override;
  end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
  TOraMTSConnectionPool = class(TCRConnectionPool)
  protected
    procedure InternalPutConnection(CRConnection: TCRConnection); override;
  public
    function GetConnection: TCRConnection; override;
  end;
{$ENDIF}
{$ENDIF}

  TOraConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
    function InternalGetConnection(ConnectionParameters: TCRConnectionParameters): TCRConnection; override;
    function InternalCheckConnection(Connection: TCRConnection): TCRConnection; override;
  public
    class procedure Clear;  {$IFDEF CLR}static;{$ENDIF}
    class procedure AsyncClear;  {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;

implementation

uses
  SysUtils, MemData, DAConsts, {$IFNDEF UNIDACPRO}OraConsts{$ELSE}OraConstsUni{$ENDIF},
  {$IFNDEF UNIDACPRO}OraError{$ELSE}OraErrorUni{$ENDIF};

var
  ConnectionPoolManager: TOraConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;

{ TOraConnectionParameters}

procedure TOraConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TOraConnectionParameters then begin
    TOraConnectionParameters(Dest).Direct := Direct;
    TOraConnectionParameters(Dest).HomeName := HomeName;  
    TOraConnectionParameters(Dest).UseUnicode := UseUnicode;  
    TOraConnectionParameters(Dest).ConnectMode := ConnectMode;
    TOraConnectionParameters(Dest).UseOCI7 := UseOCI7;
    TOraConnectionParameters(Dest).PoolingType := PoolingType;
    TOraConnectionParameters(Dest).StatementCache := StatementCache;
    TOraConnectionParameters(Dest).StatementCacheSize := StatementCacheSize;
    TOraConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;    
    TOraConnectionParameters(Dest).OptimizerMode := OptimizerMode;
    TOraConnectionParameters(Dest).ClientIdentifier := ClientIdentifier;
    TOraConnectionParameters(Dest).Schema := Schema;
  end;

  inherited;
end;

function TOraConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  O: TOraConnectionParameters;
begin
  Result := False;
  if Obj <> nil then begin
    O := TOraConnectionParameters(obj);
    Result :=
      (MinPoolSize = O.MinPoolSize) and
      (MaxPoolSize = O.MaxPoolSize) and
      (ConnectionLifeTime = O.ConnectionLifeTime) and
      (Validate = O.Validate) and
      _SameText(Server, O.Server) and
      (ConnectMode = O.ConnectMode) and
      (UseOCI7 = O.UseOCI7) and
      (PoolingType = O.PoolingType) and
      (StatementCache = O.StatementCache) and
      (StatementCacheSize = O.StatementCacheSize)and
      (ConnectionTimeout = O.ConnectionTimeout) and
      (OptimizerMode = O.OptimizerMode) and
      (ClientIdentifier = O.ClientIdentifier) and
      _SameText(Schema, O.Schema) and
      ((_SameText(Username, O.Username) and
      (Password = O.Password)) or (PoolingType = optOCI)) and
      (UseUnicode = O.UseUnicode);
  end;
end;

function TOraConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirect:
      Direct := Value;
    prHomeName:
      HomeName := Value;
    prUseUnicode:
      UseUnicode := Value;
    prConnectMode:
      ConnectMode := TConnectMode(Value);
    prUseOCI7:
      UseOCI7 := Value;
    prPoolingType:
      PoolingType := TOraPoolingType(Value);
    prStatementCache:
      StatementCache := Value;
    prStatementCacheSize:
      StatementCacheSize := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prOptimizerMode:
      OptimizerMode := TOptimizerMode(Value);
    prClientIdentifier:
      ClientIdentifier := Value;
    prSchema:
      Schema := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TOraLocalConnectionPool }

procedure SetParameters(Connection: TOCIConnection; Parameters: TOraConnectionParameters);
begin
  if not (PossibleOCICallStyles = [OCI80]) then begin
    if Parameters.UseOCI7 then
      Connection.SetOCICallStyle(OCI73)
    else
      Connection.SetOCICallStyle({$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.OCICallStyle);
  end;
  Connection.SetProp(prConnectMode, Variant(Parameters.ConnectMode));
  Connection.SetProp(prDirect, Parameters.Direct);
  Connection.SetProp(prHomeName, Parameters.HomeName);
  Connection.SetProp(prUseUnicode, Parameters.UseUnicode);
  Connection.SetProp(prStatementCache, Parameters.StatementCache);
  Connection.SetProp(prStatementCacheSize, Parameters.StatementCacheSize);
  Connection.SetProp(prConnectionTimeOut, Parameters.ConnectionTimeout);
  Connection.SetProp(prOptimizerMode, Variant(Parameters.OptimizerMode));
  Connection.SetProp(prClientIdentifier, Parameters.ClientIdentifier);
  Connection.SetProp(prSchema, Parameters.Schema);
  Connection.OnError := Parameters.OnError;
end;

function TOraLocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TOCIConnection.Create;
  try
    StartWait;
    try
      Result.SetUsername(ConnectionParameters.Username);
      Result.SetPassword(ConnectionParameters.Password);
      Result.SetServer(ConnectionParameters.Server);
      SetParameters(TOCIConnection(Result), TOraConnectionParameters(ConnectionParameters));
      Result.Connect('');
    finally
      StopWait
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TOraOciConnectionPool }

destructor TOraOCIConnectionPool.Destroy;
begin
  FreeOCIPool;

  inherited;
end;

procedure TOraOCIConnectionPool.CreateOCIPool;
var
  PoolName, p: IntPtr;
  PoolNameLen: Cardinal;
  Mode: Cardinal;
  Res, Size: integer;
begin
  if not OCIInited then
    OCIInit;

  Check(OCIHandleAlloc(hOCIEnv, hOCISPool, OCI_HTYPE_SPOOL, 0, nil));


  Mode := OCI_DEFAULT;

  if TOraConnectionParameters(ConnectionParameters).StatementCache then
   Mode := OCI_SPC_STMTCACHE;

  with TOraConnectionParameters(ConnectionParameters) do begin
    p := StringToHGlobalOCI(Server, Size, OCIUnicode);
    Res := OCISessionPoolCreate(hOCIEnv, hOCIError, hOCISPool, PoolName, PoolNameLen,
      p, Size, MinPoolSize, MaxPoolSize, 0, nil, 0, nil, 0, Mode);
    Marshal.FreeCoTaskMem(p);
    Check(Res);
  end;
  FPoolName := PtrToStringOCI(PoolName, OCIUnicode);
end;

procedure TOraOCIConnectionPool.FreeOCIPool;
begin
  if FPoolName <> '' then
    Check(OCISessionPoolDestroy(hOCISPool, hOCIError, OCI_DEFAULT));

  if hOCISPool <> nil then
    OCIHandleFree(hOCISPool, OCI_HTYPE_SPOOL);
end;

function TOraOCIConnectionPool.GetConnection: TCRConnection;
begin
  if not OCIInited then
    InitOCI;

  if PossibleOCICallStyles = [OCI80] then
    RaiseError(SOCIPoolNotSupportedWithDirect);

  if OCIVersion < 9200 then
    RaiseError(SOCIPoolNotSupported);

  if FPoolName = '' then
    CreateOCIPool;

  Result := TOCIConnection.Create;
  try
    Result.SetUsername(ConnectionParameters.Username);
    Result.SetPassword(ConnectionParameters.Password);
    SetParameters(TOCIConnection(Result), TOraConnectionParameters(ConnectionParameters));

    TOCIConnection(Result).SetConnectionType(ctOCIPooled);
    TOCIConnection(Result).Connect(FPoolName);
    Result.Pool := Self;
    InterlockedIncrement(FTakenConnectionsCount);
  except
    Result.Free;
    raise;
  end;
end;

procedure TOraOCIConnectionPool.InternalPutConnection(CRConnection: TCRConnection);
begin
  CRConnection.Disconnect;
  CRConnection.Free;
  InterlockedDecrement(FTakenConnectionsCount);
end;

{$IFDEF MSWINDOWS}
{$IFNDEF LITE}
{ TOraMTSConnectionPool }

function TOraMTSConnectionPool.GetConnection: TCRConnection;
begin
  if not OCIInited then
    OCIInit;

  if PossibleOCICallStyles = [OCI80] then
    RaiseError(SMTSPoolNotSupportedWithDirect);

  InitMTS;

  Result := TOCIConnection.Create;
  try
    Result.SetUsername(ConnectionParameters.Username);
    Result.SetPassword(ConnectionParameters.Password);
    Result.SetServer(ConnectionParameters.Server);
    SetParameters(TOCIConnection(Result), TOraConnectionParameters(ConnectionParameters));
    TOCIConnection(Result).SetConnectionType(ctMTSPooled);
    TOCIConnection(Result).Connect('');
    Result.Pool := Self;
    InterlockedIncrement(FTakenConnectionsCount);
  except
    Result.Free;
    raise;
  end;
end;

procedure TOraMTSConnectionPool.InternalPutConnection(CRConnection: TCRConnection);
begin
  CRConnection.Disconnect;
  CRConnection.Free;
  InterlockedDecrement(FTakenConnectionsCount);
end;
{$ENDIF}
{$ENDIF}

{ TOraConnectionPoolManager }

function TOraConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  case TOraConnectionParameters(ConnectionParameters).PoolingType of
    optLocal : Result := TOraLocalConnectionPool.Create(Self, ConnectionParameters);
    optOCI   : Result := TOraOciConnectionPool.Create(Self, ConnectionParameters);
  {$IFDEF MSWINDOWS}
  {$IFNDEF LITE}
    optMTS   : Result := TOraMTSConnectionPool.Create(Self, ConnectionParameters);
  {$ENDIF}
  {$ENDIF}
  else
    Result := nil;
    Assert(False);
  end;
end;

class procedure TOraConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class procedure TOraConnectionPoolManager.AsyncClear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalAsyncClear;
end;

class function TOraConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TOraConnectionPoolManager.Create;
      ConnectionPoolManager.SQLMonitorClass := SQLMonitorClass;
    end;
  finally
    LockPoolManagerCreate.Leave;
  end;

  Result := ConnectionPoolManager.InternalGetConnection(ConnectionParameters);
end;

function TOraConnectionPoolManager.InternalGetConnection(
  ConnectionParameters: TCRConnectionParameters): TCRConnection;
var
  ProxyConnection: TCRConnection;
  OraConnectionParams: TOraConnectionParameters;
begin
  FLockGet.Enter;
  try
    Result := inherited InternalGetConnection(ConnectionParameters);
    OraConnectionParams := TOraConnectionParameters(ConnectionParameters);

    if (OraConnectionParams.ProxyUserName <> '') or (OraConnectionParams.ProxyPassword <> '') then begin
      ProxyConnection := Result;
      Result := TOCIConnection.Create;
      Result.SetUsername(OraConnectionParams.ProxyUserName);
      Result.SetPassword(OraConnectionParams.ProxyPassword);
      SetParameters(TOCIConnection(Result), TOraConnectionParameters(ConnectionParameters));
      TOCIConnection(Result).ProxyConnection := TOCIConnection(ProxyConnection);
      Result.Pool := ProxyConnection.Pool;
      Result.Connect('');
    end;
  finally
    FLockGet.Leave;
  end;
end;

function TOraConnectionPoolManager.InternalCheckConnection(
  Connection: TCRConnection): TCRConnection;
begin
  if TOCIConnection(Connection).ProxyConnection <> nil then begin
    Result := TOCIConnection(Connection).ProxyConnection;
    Connection.Free;
  end
  else
    Result := inherited InternalCheckConnection(Connection);
end;

{$IFDEF WIN32_64}
{$IFNDEF FPC}

{$IFNDEF VER6P}
type
  TDLLProc = procedure (Reason: Integer);
{$ENDIF}

var
  OldDLLProc: TDLLProc;

procedure LibraryProc(Reason: integer);
begin
  if Reason = DLL_PROCESS_DETACH then begin
    ConnectionPoolManager.Free;
    ConnectionPoolManager := nil;
  end;
  if Assigned(OldDLLProc) then
    OldDLLProc(Reason);
end;
{$ENDIF}
{$ENDIF}

initialization
{$IFDEF WIN32_64}
{$IFNDEF FPC}
  OldDLLProc := DLLProc;
  DLLProc := @LibraryProc;
{$ENDIF}
{$ENDIF}
  ConnectionPoolManager := nil;
  LockPoolManagerCreate := TCriticalSection.Create;

finalization
  ConnectionPoolManager.Free;
  ConnectionPoolManager := nil;
  LockPoolManagerCreate.Free;  

end.
