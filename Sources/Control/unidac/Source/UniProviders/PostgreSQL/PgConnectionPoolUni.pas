
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgConnectionPoolUni;

{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, SyncObjs, MemUtils, MemData, CRAccess, CRConnectionPool,
  CRVio, DASQLMonitor,
{$IFNDEF UNIDACPRO}
  PgClasses;
{$ELSE}
  PgClassesUni;
{$ENDIF}

type
  TPgConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: _string; override;
  public
    Database: string;
    Port: integer;
    ProtocolVersion: TProtocolVersion;
    ApplicationName: string;
    Charset: string;
    UseUnicode: boolean;
    Schema: _string;
    ConnectionTimeout: integer;

    SSL_Mode: TSSLMode;
    SSL_CACert: string;
    SSL_Cert: string;
    SSL_Key: string;
    SSL_CipherList: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TPgLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;

  TPgConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  public
    class procedure Clear; {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;
  
implementation

var
  ConnectionPoolManager: TPgConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;

{ TPgConnectionPoolParamters }  

function TPgConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  PgObj: TPgConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TPgConnectionParameters) then begin
    PgObj := TPgConnectionParameters(Obj);
    Result :=
      SameText(Database, PgObj.Database) and
      (Port = PgObj.Port) and
      (ProtocolVersion = PgObj.ProtocolVersion) and
      SameText(Charset, PgObj.Charset) and
      (UseUnicode = PgObj.UseUnicode) and
      _SameText(Schema, PgObj.Schema) and
      (ConnectionTimeout = PgObj.ConnectionTimeout) and
      _SameText(ApplicationName, PgObj.ApplicationName) and 
      (SSL_Mode = PgObj.SSL_Mode) and
      ((SSL_Mode = smDisable) or
       ((SSL_CACert = PgObj.SSL_CACert) and
        (SSL_Cert = PgObj.SSL_Cert) and
        (SSL_Key = PgObj.SSL_Key) and
        (SSL_CipherList = PgObj.SSL_CipherList))
      );
  end;
end;

function TPgConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prCharset:
      Charset := Value;
    prUseUnicode:
      UseUnicode := Value;
    prSchema:
      Schema := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prProtocolVersion:
      ProtocolVersion := TProtocolVersion(Value);
    prApplicationName:
      ApplicationName:= Value;

    prSSL_Mode:
      SSL_Mode := TSSLMode(Value);
    prSSL_CACert:
      SSL_CACert := Value;
    prSSL_Cert:
      SSL_Cert := Value;
    prSSL_Key:
      SSL_Key := Value;
    prSSL_CipherList:
      SSL_CipherList := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TPgConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TPgConnectionParameters then begin
    TPgConnectionParameters(Dest).Database := Database;
    TPgConnectionParameters(Dest).Port := Port;
    TPgConnectionParameters(Dest).ProtocolVersion := ProtocolVersion;
    TPgConnectionParameters(Dest).Charset := Charset;
    TPgConnectionParameters(Dest).UseUnicode := UseUnicode;
    TPgConnectionParameters(Dest).Schema := Schema;
    TPgConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
    TPgConnectionParameters(Dest).ApplicationName := ApplicationName;

    TPgConnectionParameters(Dest).SSL_Mode := SSL_Mode;
    TPgConnectionParameters(Dest).SSL_CACert := SSL_CACert;
    TPgConnectionParameters(Dest).SSL_Cert := SSL_Cert;
    TPgConnectionParameters(Dest).SSL_Key := SSL_Key;
    TPgConnectionParameters(Dest).SSL_CipherList := SSL_CipherList;
  end;
end;

function TPgConnectionParameters.ConnectParamsToString: _string;
begin
  Result := inherited ConnectParamsToString + _Format(
    'Port=%d'#13'Database=%s'#13,
    [Port, Database]);
end;

{ TPgLocalConnectionPool }

function TPgLocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TPgSQLConnection.Create;
  try
    StartWait;
    try
      Result.SetProp(prDatabase, TPgConnectionParameters(ConnectionParameters).Database);
      Result.SetProp(prPort, TPgConnectionParameters(ConnectionParameters).Port);
      Result.SetProp(prProtocolVersion, Variant(TPgConnectionParameters(ConnectionParameters).ProtocolVersion));
      Result.SetProp(prCharset, TPgConnectionParameters(ConnectionParameters).Charset);
      Result.SetProp(prUseUnicode, TPgConnectionParameters(ConnectionParameters).UseUnicode);
      Result.SetProp(prSchema, TPgConnectionParameters(ConnectionParameters).Schema);
      Result.SetProp(prConnectionTimeout, TPgConnectionParameters(ConnectionParameters).ConnectionTimeout);
      Result.SetProp(prApplicationName, TPgConnectionParameters(ConnectionParameters).ApplicationName);

      Result.SetProp(prSSL_Mode, Integer(TPgConnectionParameters(ConnectionParameters).SSL_Mode));
      Result.SetProp(prSSL_CACert, TPgConnectionParameters(ConnectionParameters).SSL_CACert);
      Result.SetProp(prSSL_Cert, TPgConnectionParameters(ConnectionParameters).SSL_Cert);
      Result.SetProp(prSSL_Key, TPgConnectionParameters(ConnectionParameters).SSL_Key);
      Result.SetProp(prSSL_CipherList, TPgConnectionParameters(ConnectionParameters).SSL_CipherList);
      Result.IOHandler := ConnectionParameters.IOHandler;
      Result.OnError := ConnectionParameters.OnError;

      Result.SetUsername(ConnectionParameters.UserName);
      Result.SetPassword(ConnectionParameters.Password);
      Result.SetServer(ConnectionParameters.Server);

      Result.Connect('');
    finally
      StopWait
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TPgConnectionPoolManager }

function TPgConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TPgLocalConnectionPool.Create(Self, ConnectionParameters);
end;

class procedure TPgConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class function TPgConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TPgConnectionPoolManager.Create;
      ConnectionPoolManager.SQLMonitorClass := SQLMonitorClass;
    end;
  finally
    LockPoolManagerCreate.Leave;
  end;    

  Result := ConnectionPoolManager.InternalGetConnection(ConnectionParameters);
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
