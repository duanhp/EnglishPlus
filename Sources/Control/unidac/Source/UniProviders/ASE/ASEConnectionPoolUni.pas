
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ASEDac.inc}
unit ASEConnectionPoolUni;

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
  ODBCClasses, ASEClasses;
{$ELSE}
  ODBCClassesUni, ASEClassesUni;
{$ENDIF}

type
  TASEConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: _string; override;
  public
    Database: _string;
    Port: integer;
    ConnectionTimeout: integer;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TASELocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;

  TASEConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  public
    class procedure Clear; {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;
  
implementation

var
  ConnectionPoolManager: TASEConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;

{ TASEConnectionPoolParamters }

function TASEConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ASEObj: TASEConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TASEConnectionParameters) then begin
    ASEObj := TASEConnectionParameters(Obj);
    Result :=
      (Database = ASEObj.Database) and
      (Port = ASEObj.Port) and
      (ConnectionTimeout = ASEObj.ConnectionTimeout);
  end;
end;

function TASEConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prPort:
      Port := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TASEConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TASEConnectionParameters then begin
    TASEConnectionParameters(Dest).Database := Database;
    TASEConnectionParameters(Dest).Port := Port;
    TASEConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
  end;
end;

function TASEConnectionParameters.ConnectParamsToString: _string;
begin
  Result := inherited ConnectParamsToString + _Format(
    'Port=%d'#13'Database=%s'#13,
    [Port, Database]);
end;

{ TASELocalConnectionPool }

function TASELocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TASEConnection.Create;
  try
    StartWait;
    try
      Result.SetProp(prDatabase, TASEConnectionParameters(ConnectionParameters).Database);
      Result.SetProp(prPort, TASEConnectionParameters(ConnectionParameters).Port);
      Result.SetProp(prConnectionTimeout, TASEConnectionParameters(ConnectionParameters).ConnectionTimeout);
      Result.OnError := ConnectionParameters.OnError;

      Result.SetUsername(ConnectionParameters.UserName);
      Result.SetPassword(ConnectionParameters.Password);

      Result.Connect('');
    finally
      StopWait
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ TASEConnectionPoolManager }

function TASEConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TASELocalConnectionPool.Create(Self, ConnectionParameters);
end;

class procedure TASEConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class function TASEConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TASEConnectionPoolManager.Create;
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
