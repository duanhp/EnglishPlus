
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DB2Dac.inc}
unit DB2ConnectionPoolUni;

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
  ODBCClasses, DB2Classes;
{$ELSE}
  ODBCClassesUni, DB2ClassesUni;
{$ENDIF}

type
  TDB2ConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    Schema: _string;
    FunctionPath: _string;
    ConnectionTimeout: integer;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TDB2LocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;

  TDB2ConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  public
    class procedure Clear; {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;
  
implementation

var
  ConnectionPoolManager: TDB2ConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;

{ TPgConnectionPoolParamters }

function TDB2ConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ODBCObj: TDB2ConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TDB2ConnectionParameters) then begin
    ODBCObj := TDB2ConnectionParameters(Obj);
    Result :=
      (Schema = ODBCObj.Schema) and
      (FunctionPath = ODBCObj.FunctionPath) and
      (ConnectionTimeout = ODBCObj.ConnectionTimeout);
  end;
end;

function TDB2ConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prSchema:
      Schema := Value;
    prFunctionPath:
      FunctionPath := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TDB2ConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TDB2ConnectionParameters then begin
    TDB2ConnectionParameters(Dest).Schema := Schema;
    TDB2ConnectionParameters(Dest).FunctionPath := FunctionPath;
    TDB2ConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
  end;
end;

{ TDB2LocalConnectionPool }

function TDB2LocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TDB2Connection.Create;
  try
    StartWait;
    try
      Result.SetProp(prSchema, TDB2ConnectionParameters(ConnectionParameters).Schema);
      Result.SetProp(prFunctionPath, TDB2ConnectionParameters(ConnectionParameters).FunctionPath);
      Result.SetProp(prConnectionTimeout, TDB2ConnectionParameters(ConnectionParameters).ConnectionTimeout);
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

{ TDB2ConnectionPoolManager }

function TDB2ConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TDB2LocalConnectionPool.Create(Self, ConnectionParameters);
end;

class procedure TDB2ConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class function TDB2ConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TDB2ConnectionPoolManager.Create;
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
