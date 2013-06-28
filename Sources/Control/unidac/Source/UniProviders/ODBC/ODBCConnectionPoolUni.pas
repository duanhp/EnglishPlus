
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ODBCDac.inc}
unit ODBCConnectionPoolUni;

{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, SyncObjs,
  MemUtils, MemData, CRAccess, CRConnectionPool, DASQLMonitor,
{$IFNDEF UNIDACPRO}
  ODBCClasses;
{$ELSE}
  ODBCClassesUni;
{$ENDIF}

type
  TODBCConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    DSNType: TDSNType;
    ConnectionTimeout: integer;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TODBCLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;

  TODBCConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  public
    class procedure Clear; {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;
  
implementation

var
  ConnectionPoolManager: TODBCConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;

{ TPgConnectionPoolParamters }

function TODBCConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ODBCObj: TODBCConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TODBCConnectionParameters) then begin
    ODBCObj := TODBCConnectionParameters(Obj);
    Result :=
      (DSNType = ODBCObj.DSNType) and
      (ConnectionTimeout = ODBCObj.ConnectionTimeout);
  end;
end;

function TODBCConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDSNType:
      DSNType := TDSNType(Value);
    prConnectionTimeout:
      ConnectionTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TODBCConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TODBCConnectionParameters then begin
    TODBCConnectionParameters(Dest).DSNType := DSNType;
    TODBCConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
  end;
end;

{ TODBCLocalConnectionPool }

function TODBCLocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TODBCConnection.Create;
  try
    StartWait;
    try
      Result.SetProp(prDSNType, Variant(TODBCConnectionParameters(ConnectionParameters).DSNType));
      Result.SetProp(prConnectionTimeout, TODBCConnectionParameters(ConnectionParameters).ConnectionTimeout);
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

{ TODBCConnectionPoolManager }

function TODBCConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TODBCLocalConnectionPool.Create(Self, ConnectionParameters);
end;

class procedure TODBCConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class function TODBCConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TODBCConnectionPoolManager.Create;
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
