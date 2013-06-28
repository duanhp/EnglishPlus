
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MyConnectionPoolUni;
{$ENDIF}

interface

uses
  Classes, CRConnectionPool, CRAccess, MemUtils, MemData, CRVio, DASQLMonitor,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  MyClasses;
{$ELSE}
  MyClassesUni;
{$ENDIF}

type
  TMyConnectionParameters = class(TCRConnectionParameters)
  protected
    FEmbParams: TStrings;
    procedure SetEmbParams(Value: TStrings);
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: _string; override;

  public
    Database: string;
    Port: integer;
    ConnectionTimeout: integer;
    Compress: boolean;
    UseUnicode: boolean;
    Charset: string;
    Protocol: TMyProtocol;
    Embedded: boolean;
  {$IFDEF HAVE_DIRECT}
    Direct: boolean;
  {$ENDIF}
    SSL_Chipher: string;
    SSL_CA: string;
    SSL_Key: string;
    SSL_Cert: string;
    property EmbParams: TStrings read FEmbParams write SetEmbParams;

    constructor Create; override;
    destructor Destroy; override;
    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TMyLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;


  TMyConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  public
    class procedure Clear;  {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;

implementation

uses
{$IFDEF LINUX}
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF HAVE_DIRECT}
  {$IFNDEF UNIDACPRO}MySqlApiDirect{$ELSE}MySqlApiDirectUni{$ENDIF},
{$ENDIF}
  SysUtils, SyncObjs,
  {$IFNDEF UNIDACPRO}MySqlApi{$ELSE}MySqlApiUni{$ENDIF};

var
  ConnectionPoolManager: TMyConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;

{ TMyConnectionParameters}

constructor TMyConnectionParameters.Create;
begin
  inherited;
  FEmbParams := TStringList.Create;
end;

destructor TMyConnectionParameters.Destroy;
begin
  FEmbParams.Free;
  inherited;
end;

procedure TMyConnectionParameters.SetEmbParams(Value: TStrings);
begin
  FEmbParams.Assign(Value);
end;

procedure TMyConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TMyConnectionParameters then begin
    TMyConnectionParameters(Dest).Database := Database;
    TMyConnectionParameters(Dest).Port := Port;
    TMyConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
    TMyConnectionParameters(Dest).Compress := Compress;
    TMyConnectionParameters(Dest).UseUnicode := UseUnicode;
    TMyConnectionParameters(Dest).Charset := Charset;
    TMyConnectionParameters(Dest).Protocol := Protocol;
    TMyConnectionParameters(Dest).Embedded := Embedded;

  {$IFDEF HAVE_DIRECT}
    TMyConnectionParameters(Dest).Direct := Direct;
  {$ENDIF}
    TMyConnectionParameters(Dest).SSL_Chipher := SSL_Chipher;
    TMyConnectionParameters(Dest).SSL_CA := SSL_CA;
    TMyConnectionParameters(Dest).SSL_Key := SSL_Key;
    TMyConnectionParameters(Dest).SSL_Cert := SSL_Cert;
    TMyConnectionParameters(Dest).EmbParams.Assign(EmbParams);
  end;

  inherited;
end;

function TMyConnectionParameters.ConnectParamsToString: _string;
begin
  Result := inherited ConnectParamsToString + _Format(
    'Port=%d'#13'Database=%s'#13,
    [Port, Database]);
end;

function TMyConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  O: TMyConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TMyConnectionParameters) then begin
    O := TMyConnectionParameters(obj);
    Result :=
      (AnsiCompareText(O.Database, Database) = 0) and
      (O.Port = Port) and
      (O.ConnectionTimeout = ConnectionTimeout) and
      (O.Compress = Compress) and
      (O.UseUnicode = UseUnicode) and
      (O.Charset = Charset) and
      (O.Protocol = Protocol) and
      (O.Embedded = Embedded)
    {$IFDEF HAVE_DIRECT}
      and (O.Direct = Direct)
    {$ENDIF}
    {$IFDEF HAVE_OPENSSL}
      and (
        (Protocol <> mpSSL) or
        ((O.SSL_Chipher = SSL_Chipher) and
         (O.SSL_CA = SSL_CA) and
         (O.SSL_Key = SSL_Key) and
         (O.SSL_Cert = SSL_Cert))
      )
    {$ENDIF}
      ;
  end;
end;

function TMyConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prCharset:
      Charset := Value;
    prUseUnicode:
      UseUnicode := Value;
    prCompress:
      Compress := Value;
    prProtocol:
      Protocol := TMyProtocol(Value);
    prEmbedded:
      Embedded := Value;
    prEmbParams:
      FEmbParams.Text := Trim(Value);
  {$IFDEF HAVE_DIRECT}
    prDirect:
      Direct := Value;
  {$ENDIF}
    prSSL_Chipher:
      SSL_Chipher := Value;
    prSSL_CA:
      SSL_CA := Value;
    prSSL_Key:
      SSL_Key := Value;
    prSSL_Cert:
      SSL_Cert := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TMyLocalConnectionPool }

function TMyLocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TMySQLConnection.Create;

  try
    Result.SetProp(prDatabase, TMyConnectionParameters(ConnectionParameters).Database);
    Result.SetProp(prConnectionTimeout, TMyConnectionParameters(ConnectionParameters).ConnectionTimeout);
    Result.SetProp(prPort, TMyConnectionParameters(ConnectionParameters).Port);
    Result.SetProp(prEmbedded, TMyConnectionParameters(ConnectionParameters).Embedded);
  {$IFDEF HAVE_DIRECT}
    Result.SetProp(prDirect, TMyConnectionParameters(ConnectionParameters).Direct);
  {$ENDIF}
    Result.SetProp(prCompress, TMyConnectionParameters(ConnectionParameters).Compress);
    Result.SetProp(prUseUnicode, TMyConnectionParameters(ConnectionParameters).UseUnicode);
    Result.SetProp(prCharset, TMyConnectionParameters(ConnectionParameters).Charset);
    Result.SetProp(prProtocol, Integer(TMyConnectionParameters(ConnectionParameters).Protocol));

    Result.IOHandler := ConnectionParameters.IOHandler;
    Result.HttpOptions := ConnectionParameters.HttpOptions;
  {$IFDEF HAVE_OPENSSL}
    Result.SetProp(prSSL_Chipher, TMyConnectionParameters(ConnectionParameters).SSL_Chipher);
    Result.SetProp(prSSL_CA, TMyConnectionParameters(ConnectionParameters).SSL_CA);
    Result.SetProp(prSSL_Key, TMyConnectionParameters(ConnectionParameters).SSL_Key);
    Result.SetProp(prSSL_Cert, TMyConnectionParameters(ConnectionParameters).SSL_Cert);
  {$ENDIF}

    Result.SetProp(prEmbParams, TMyConnectionParameters(ConnectionParameters).EmbParams.Text);
    Result.OnError := ConnectionParameters.OnError;

    StartWait;
    try
      Result.SetUsername(ConnectionParameters.Username);
      Result.SetPassword(ConnectionParameters.Password);
      Result.SetServer(ConnectionParameters.Server);

      Result.Connect('');
    finally
      StopWait;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TMyConnectionPoolManager }

function TMyConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TMyLocalConnectionPool.Create(Self, ConnectionParameters);
end;

class procedure TMyConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class function TMyConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TMyConnectionPoolManager.Create;
      ConnectionPoolManager.SQLMonitorClass := SQLMonitorClass;
    end;
  finally
    LockPoolManagerCreate.Leave;
  end;

  Result := ConnectionPoolManager.InternalGetConnection(ConnectionParameters);
end;

{$IFDEF WIN32}
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
{$IFDEF WIN32}
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
