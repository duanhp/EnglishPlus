
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright (c) 2006-2011 Devart. All right reserved.
//  Connection Pool
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I IbDac.inc}
unit IBCConnectionPoolUni;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, TypInfo, SyncObjs,
  MemUtils, CRConnectionPool, CRAccess, DASQLMonitor,
  {$IFNDEF UNIDACPRO}IBCClasses{$ELSE}IBCClassesUni{$ENDIF};

type
  TIBCConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: _string; override;
  public
    Database: string;
    Protocol: _TIBCProtocol;
    DBParams: TStringList;
    Charset: string;
    SQLDialect: integer;
    UseUnicode: boolean;
    Role: string;

    constructor Create; override;
    destructor Destroy; override;
    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;


  TIBCLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    function CreateNewConnector: TCRConnection; override;
  end;

  TIBCConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    function CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  public
    class procedure Clear;  {$IFDEF CLR}static;{$ENDIF}
    class function GetConnection(ConnectionParameters: TCRConnectionParameters; SQLMonitorClass: TDASQLMonitorClass): TCRConnection; override;
  end;

implementation

uses
  MemData, {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF};

var
  ConnectionPoolManager: TIBCConnectionPoolManager;
  LockPoolManagerCreate: TCriticalSection;
    
{ TIBCConnectionParameters}

constructor TIBCConnectionParameters.Create;
begin
  inherited;
  DBParams := TStringList.Create;
end;

destructor TIBCConnectionParameters.Destroy;
begin
  DBParams.Free;

  inherited;
end;

procedure TIBCConnectionParameters.AssignTo(Dest: TPersistent);
begin
  if Dest is TIBCConnectionParameters then begin
    TIBCConnectionParameters(Dest).Database := Database;
    TIBCConnectionParameters(Dest).Protocol := Protocol;
    TIBCConnectionParameters(Dest).DBParams.Assign(DBParams);
    TIBCConnectionParameters(Dest).Charset := Charset;
    TIBCConnectionParameters(Dest).SQLDialect := SQLDialect;
    TIBCConnectionParameters(Dest).UseUnicode := UseUnicode;
    TIBCConnectionParameters(Dest).Role := Role;
  end;
  inherited;
end;

function ListCompareFunction(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List[Index1], List[Index2]);
end;

function CompareParamLists(List1, List2: TStringList): boolean;
var
  i: integer;

  function GetName(Str: string; Index: integer): string;
  begin
    Result := Str;
    if Index > 0 then
      Result := Trim(Copy(Str, 1, Index - 1));
  end;

  function GetValue(Str: string; Index: integer): string;
  begin
    Result := Str;
    if Index > 0 then
      Result := Trim(Copy(Str, Index + 1, Length(str) - Index));
  end;

  function CompareItems(Item1, Item2: string): boolean;
  var
    i1, i2: integer;
  begin
    i1 := pos('=', Item1);
    i2 := pos('=', Item2);
    Result := (AnsiCompareText(GetName(Item1, i1), GetName(Item2, i2)) = 0)
      and (AnsiCompareText(GetValue(Item1, i1), GetValue(Item2, i2)) = 0);
  end;
begin
  Result := True;
  if List1.Count <> List2.Count then
    Result := False
  else begin
    List1.CustomSort(ListCompareFunction);
    List2.CustomSort(ListCompareFunction);

    for i := 0 to List1.Count - 1 do
      if not CompareItems(List1[i], List2[i]) then begin
        Result := False;
        break;
      end;
  end;
end;

function TIBCConnectionParameters.Equals(Obj: TCRConnectionParameters): boolean;
var
  O: TIBCConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TIBCConnectionParameters) then begin
    O := TIBCConnectionParameters(obj);
    Result :=
      (AnsiCompareText(Database, O.Database) = 0) and
      (Protocol = O.Protocol) and
      (AnsiCompareText(Charset, O.Charset) = 0) and
      (SQLDialect = O.SQLDialect) and
      (UseUnicode = O.UseUnicode) and
      (Role = O.Role) and
      (CompareParamLists(DBParams, O.DBParams));
  end;
end;

function TIBCConnectionParameters.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prProtocol:
      Protocol := _TIBCProtocol(Value);
    prCharset:
      Charset := Value;
    prSQLDialect:
      SQLDialect := Value;
    prUseUnicode:
      UseUnicode := Value;
    prRole:
      Role := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TIBCConnectionParameters.ConnectParamsToString: _string;
begin
  Result := inherited ConnectParamsToString + _Format(
    'Protocol=' + GetEnumName(TypeInfo(_TIBCProtocol), Integer(Protocol)) + #13 +
    'Database=%s'#13'SQLDialect=%d'#13,
    [Database, SQLDialect]);
end;

{ TIBCLocalConnectionPool }

function TIBCLocalConnectionPool.CreateNewConnector: TCRConnection;
begin
  Result := TGDSConnection.Create;
  try
    StartWait;
    try
      Result.SetProp(prDatabase, TIBCConnectionParameters(ConnectionParameters).Database);
      Result.SetProp(prProtocol, Variant(TIBCConnectionParameters(ConnectionParameters).Protocol));
      Result.SetProp(prCharset, TIBCConnectionParameters(ConnectionParameters).Charset);
      Result.SetProp(prSQLDialect, TIBCConnectionParameters(ConnectionParameters).SQLDialect);
      Result.SetProp(prUseUnicode, TIBCConnectionParameters(ConnectionParameters).UseUnicode);
      Result.SetProp(prRole, TIBCConnectionParameters(ConnectionParameters).Role);
      TGDSConnection(Result).SetParams(TIBCConnectionParameters(ConnectionParameters).DBParams);
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

{TIBCConnectionPoolManager}

function TIBCConnectionPoolManager.CreateCRConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TIBCLocalConnectionPool.Create(Self, ConnectionParameters);
end;

class procedure TIBCConnectionPoolManager.Clear;
begin
  if ConnectionPoolManager <> nil then
    ConnectionPoolManager.InternalClear;
end;

class function TIBCConnectionPoolManager.GetConnection(ConnectionParameters: TCRConnectionParameters;
  SQLMonitorClass: TDASQLMonitorClass): TCRConnection;
begin
  LockPoolManagerCreate.Enter;
  try
    if ConnectionPoolManager = nil then begin
      ConnectionPoolManager := TIBCConnectionPoolManager.Create;
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
  DLLProc := {$IFDEF VER6P}LibraryProc{$ELSE}@LibraryProc{$ENDIF};
{$ENDIF}
{$ENDIF}
  ConnectionPoolManager := nil;
  LockPoolManagerCreate := TCriticalSection.Create;
  
finalization
  ConnectionPoolManager.Free;
  ConnectionPoolManager := nil;
  LockPoolManagerCreate.Free;
  
end.

