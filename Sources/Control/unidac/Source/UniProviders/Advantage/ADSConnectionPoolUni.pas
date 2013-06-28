
//////////////////////////////////////////////////
//  Advantage Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ADSDac.inc}
unit ADSConnectionPoolUni;

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
  ODBCClasses, ADSClasses;
{$ELSE}
  ODBCClassesUni, ADSClassesUni;
{$ENDIF}

type
  TADSConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: _string; override;
  public
    Database: _string;
    ConnectionTimeout: integer;
    ServerTypes: string;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TADSLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;

  TADSConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  public
    class procedure Clear; {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;
  
implementation

var
  ConnectionPoolManager: TADSConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;

{ TADSConnectionPoolParamters }

function TADSConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  ADSObj: TADSConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TADSConnectionParameters) then begin
    ADSObj := TADSConnectionParameters(Obj);
    Result :=
      (Database = ADSObj.Database) and
      (ConnectionTimeout = ADSObj.ConnectionTimeout) and
      SameText(ServerTypes, ADSObj.ServerTypes);
  end;
end;

function TADSConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prConnectionTimeout:
      ConnectionTimeout := Value;
    prADSServerTypes:
      ServerTypes := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TADSConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TADSConnectionParameters then begin
    TADSConnectionParameters(Dest).Database := Database;
    TADSConnectionParameters(Dest).ConnectionTimeout := ConnectionTimeout;
    TADSConnectionParameters(Dest).ServerTypes := ServerTypes;
  end;
end;

function TADSConnectionParameters.ConnectParamsToString: _string;
begin
  Result := inherited ConnectParamsToString + _Format(
    'Database=%s'#13,
    [Database]);
end;

{ TADSLocalConnectionPool }

function TADSLocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TADSConnection.Create;
  try
    StartWait;
    try
      Result.SetProp(prDatabase, TADSConnectionParameters(ConnectionParameters).Database);
      Result.SetProp(prConnectionTimeout, TADSConnectionParameters(ConnectionParameters).ConnectionTimeout);
      Result.SetProp(prADSServerTypes, TADSConnectionParameters(ConnectionParameters).ServerTypes);
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

{ TADSConnectionPoolManager }

function TADSConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TADSLocalConnectionPool.Create(Self, ConnectionParameters);
end;

class procedure TADSConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class function TADSConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TADSConnectionPoolManager.Create;
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
