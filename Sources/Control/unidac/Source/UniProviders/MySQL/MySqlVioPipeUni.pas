
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlVioPipeUni;
{$ENDIF}

interface

{$IFDEF MSWINDOWS}

uses
{$IFDEF CLR}
  System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  Windows, Classes, SysUtils, MemUtils, CRVio,
{$IFNDEF UNIDACPRO}
  MyCall;
{$ELSE}
  MyCallUni;
{$ENDIF}

const
  MYSQL_NAMEDPIPE = 'MySQL';

type
  TMySqlVioPipe = class(TCRVio)
  protected
    FPipe: THandle;
    Fhostname: string;
    Fport: integer;
    Funix_socket: string;
    Ftimeout: integer;

    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;

  public
    constructor Create(const hostname: string; const port: integer; const unix_socket: PAnsiChar);

    procedure Connect; override;
    procedure Close; override;

    function ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer; override;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  {$IFNDEF UNIDACPRO}MySqlErrors{$ELSE}MySqlErrorsUni{$ENDIF};

{ TMySqlVioPipe }

constructor TMySqlVioPipe.Create(const hostname: string; const port: integer; const unix_socket: PAnsiChar);
begin
  inherited Create;
  Fhostname := hostname;
  Fport := port;
  Funix_socket := string(unix_socket);
  FPipe := INVALID_HANDLE_VALUE;
end;

function TMySqlVioPipe.GetTimeout: integer;
begin
  Result := Ftimeout;
end;

procedure TMySqlVioPipe.SetTimeout(Value: integer);
begin
//  Assert(Fsd = -1);
  Ftimeout := Value;
end;

procedure TMySqlVioPipe.Connect;
var
  szPipeName: string;
  dwMode: DWORD;
  i: int;
  
begin
  inherited;
  if Funix_socket = '' then
    Funix_socket := MYSQL_NAMEDPIPE;

  if (Fhostname = '') or (AnsiCompareText(Fhostname, LOCAL_HOST) = 0) then
    Fhostname := LOCAL_HOST_NAMEDPIPE;

  szPipeName := '\\' + Fhostname + '\pipe\' + Funix_socket;

  for i := 0 to 99 do begin (* Don't retry forever *)
    FPipe := CreateFile(PChar(szPipeName),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING,
      0,
      0);
    if FPipe <> INVALID_HANDLE_VALUE then
      Break;
    if GetLastError <> ERROR_PIPE_BUSY then
      raise EMySqlException.Create(CR_NAMEDPIPEOPEN_ERROR, SysErrorMessage(GetLastError));
    (* wait for an other instance *)
    if not WaitNamedPipe(PChar(szPipeName), Ftimeout * 1000) then
      raise EMySqlException.Create(CR_NAMEDPIPEWAIT_ERROR);
  end;
  if FPipe = INVALID_HANDLE_VALUE then
    raise EMySqlException.Create(CR_NAMEDPIPEOPEN_ERROR);

  dwMode := PIPE_READMODE_BYTE or PIPE_WAIT;
  if not SetNamedPipeHandleState(FPipe, dwMode, nil, nil) then begin
    FPipe := INVALID_HANDLE_VALUE;
    CloseHandle(FPipe);
    raise EMySqlException.Create(CR_NAMEDPIPESETSTATE_ERROR);
  end;
end;

procedure TMySqlVioPipe.Close;
begin
  CloseHandle(FPipe);
end;

{$IFDEF CLR}
[DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'ReadFile')]
function ReadFile(hFile: THandle; lpBuffer: IntPtr; nNumberOfBytesToRead: int;
  out lpNumberOfBytesRead: DWORD; lpOverlapped: IntPtr): BOOL; external;

[DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'WriteFile')]
function WriteFile(hFile: THandle; lpBuffer: IntPtr; nNumberOfBytesToWrite: int;
  out lpNumberOfBytesWritten: DWORD; lpOverlapped: IntPtr): BOOL; external;

function TMySqlVioPipe.ReadNoWait(var buffer: TBytes; offset,
  count: integer): integer;
var
  len1: longword;
  h: GCHandle;
  bufPtr: IntPtr;
begin
  h := GCHandle.Alloc(buffer, GCHandleType.Pinned);
  bufPtr := Marshal.UnsafeAddrOfPinnedArrayElement(buffer, offset);
  if not ReadFile(FPipe, bufPtr, count, len1, nil) then
    Result := 0
  else
    Result := len1;
  h.Free;
end;

function TMySqlVioPipe.WriteNoWait(const buffer: TBytes; offset,
  count: integer): integer;
var
  len1: longword;
  h: GCHandle;
  bufPtr: IntPtr;
begin
  if count > $FFFF then
    count := $FFFF;

  h := GCHandle.Alloc(buffer, GCHandleType.Pinned);
  bufPtr := Marshal.UnsafeAddrOfPinnedArrayElement(buffer, offset);
  if not WriteFile(FPipe, bufPtr, count, len1, nil) then
    Result := 0
  else
    Result := len1;
  h.Free;
end;

{$ELSE}

function TMySqlVioPipe.ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset,
  count: integer): integer;
var
  len1: longword;
begin
  if not ReadFile(FPipe, buffer[offset], count, len1, nil) then
    Result := 0
  else
    Result := len1;
end;

function TMySqlVioPipe.WriteNoWait(const buffer: TValueArr; offset,
  count: integer): integer;
var
  len1: longword;
begin
  if count > $FFFF then
    count := $FFFF;

  if not WriteFile(FPipe, buffer[offset], count, len1, nil) then
    Result := 0
  else
    Result := len1;
end;
{$ENDIF}

{$ENDIF}

end.
