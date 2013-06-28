//////////////////////////////////////////////////
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}
unit CRVioTcp;
{$ENDIF}

interface

uses
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  {$IFNDEF BCB}
  {$NOINCLUDE winsock}
  {$HPPEMIT '#include <winsock2.h>'}
  {$ENDIF}
  WinSock, Windows,
{$ELSE}
  Libc,
{$ENDIF}
{$IFDEF FPC}
  Sockets,
{$ENDIF}
  MemUtils, CRVio;

const
  LOCAL_HOST = 'localhost';
  CR_IPSOCK_ERROR = 2005;
  CR_UNKNOWN_HOST = 2005;
  CR_CONN_HOST_ERROR = 2003;
  CR_SELECT_ERROR = 2006;

type
  SocketException = class(Exception)
  protected
    FErrorCode: Integer;
  public
    constructor Create(errorCode: Integer);

    property ErrorCode: Integer read FErrorCode;
  end;

  TCRVioTcp = class(TCRVio)
  protected
    Fhostname: AnsiString;
    Fport: integer;
    Ftimeout: integer;

    FSd: longint;
    ffcntl_mode: longint;

    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;
    function fastsend: longint;
    function keepalive(onoff: boolean): longint;
    function IPByHostName(const HostName: AnsiString): longint;

  public
    constructor Create(const hostname: AnsiString; const port: integer); overload;

    procedure Connect; override;
    procedure Close; override;

    function ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer; override;
    function Write(const buffer: TValueArr; offset, count: integer): integer; override;
  {$IFDEF MSWINDOWS}
    function WaitForData(Timeout: integer = -1): boolean; override;
  {$ENDIF}

    function GetSocket: longint; virtual;
  end;

  THostNameResolver = class(TThread)
  private
    FVio: TCRVioTcp;
    FIPAddr: LongInt;

  protected
    procedure Execute; override;

  public
    constructor Create(Vio: TCRVioTcp);
  end;

implementation

{$IFDEF MSWINDOWS}
var
  WsaData: TWSAData; // on windows winsock
{$ENDIF}

const
  SD_BOTH = 2;

{ SocketException }

constructor SocketException.Create(errorCode: Integer);
begin
  inherited Create('');
  FErrorCode := ErrorCode;
end;

{ TCRVioTcp }

constructor TCRVioTcp.Create(const hostname: AnsiString; const port: integer);
begin
  inherited Create;
  Fhostname := hostname;
  Fport := port;

  FSd :=-1;
  ffcntl_mode :=0;
  Ftimeout := 30;
end;

function TCRVioTcp.GetTimeout: integer;
begin
  Result := Ftimeout;
end;

{$IFDEF MSWINDOWS}
procedure TCRVioTcp.SetTimeout(Value: integer);
var
  i: Int64;
begin
  if Value = Ftimeout then
    Exit;

//  Assert(Fsd = -1);
  Ftimeout := Value;

  if FSd <> -1 then begin
    i := Int64(Ftimeout) * 1000;
    //ms-help://MS.MSDNQTR.2003FEB.1033/winsock/winsock/socket_options_for_windows_nt_and_windows_95_98_me_2.htm
    Win32Check(setsockopt(FSd, SOL_SOCKET, SO_RCVTIMEO, @i, sizeof(i)) = 0);
  end;
end;
{$ELSE}
procedure TCRVioTcp.SetTimeout(Value: integer);
begin
  Ftimeout := Value;
end;
{$ENDIF}

function TCRVioTcp.fastsend: longint;
var
  nodelay: longint;
begin
  Result := 0;
  if setsockopt(fsd, IPPROTO_IP, IP_TOS, nil, 0) <> 0 then
  begin
    nodelay := 1;
    if (setsockopt(fsd, IPPROTO_TCP, TCP_NODELAY, IntPtr(@nodelay), sizeof(nodelay))) <> 0 then
      Result := -1;
  end;
end;

function TCRVioTcp.keepalive(onoff: boolean): longint;
var
  opt: longint;
begin
  if onoff then
    opt := 1
  else
    opt := 0;

  result := setsockopt(fsd, SOL_SOCKET, SO_KEEPALIVE, IntPtr(@opt), sizeof(opt));
end;

function TCRVioTcp.IPByHostName(const HostName: AnsiString): longint;
var
  hp: PHostEnt;
begin
  hp := gethostbyname(PAnsiChar(Fhostname));
  if hp = nil then begin
  {$IFDEF MSWINDOWS}
    FLastError := Format('Socket error on connect. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
  {$ENDIF}
    Result := longint(INADDR_NONE);
  end
  else
    Result := PLongInt(hp.h_addr^)^;
end;

procedure TCRVioTcp.Connect;
var
{$IFDEF MSWINDOWS}
  i: integer;
  NameResolver: THostNameResolver;
{$ENDIF}
  sock_addr: TSockAddr;
  ip_addr: longint;
  OldTimeout: integer;
begin
  Assert(Fsd = -1);
  inherited;

{$IFDEF MSWINDOWS}
  if WsaData.wVersion = 0 then begin // not initialized
    i := WSAStartup($0101, WsaData);
    if i <> 0 then
      raise SocketException.Create(i);
  end;
{$ENDIF}

  if Fhostname = '' then
    Fhostname := LOCAL_HOST;
  FSd := socket(AF_INET, SOCK_STREAM, 0); //try grab a socket
  if FSd = SOCKET_ERROR then //error?
    raise SocketException.Create(CR_IPSOCK_ERROR);

  try
  {$IFDEF MSWINDOWS}
    // arg:=0;
    // ioctlsocket(FSd, FIONBIO, longint(arg));
  {$ELSE}
    ffcntl_mode := fcntl(FSd, F_GETFL);
  {$ENDIF}

    //try to resolve the host
    System.FillChar(sock_addr, SizeOf(sock_addr), #0);
    sock_addr.sin_family := AF_INET;
    ip_addr := inet_addr(PAnsiChar(Fhostname));

    if ip_addr = longint(INADDR_NONE) then begin
    {$IFDEF MSWINDOWS}
      if Ftimeout > 0 then begin
        NameResolver := THostNameResolver.Create(Self);
        try
          if WaitForSingleObject(NameResolver.Handle, DWORD(Ftimeout) * 1000) = WAIT_OBJECT_0 then
            ip_addr := NameResolver.FIPAddr
          else begin
            TerminateThread(NameResolver.Handle, 0);
            ip_addr := longint(INADDR_NONE);
            FLastError := 'Connection timed out';
          end;
        finally
          NameResolver.Free;
        end;
      end
      else
        ip_addr := IPByHostName(Fhostname);
    {$ELSE}
      ip_addr := IPByHostName(Fhostname);
    {$ENDIF}
      if ip_addr = longint(INADDR_NONE) then
        raise SocketException.Create(CR_UNKNOWN_HOST);
    end;

    sock_addr.sin_addr.S_addr := u_long(ip_addr);

    sock_addr.sin_port := htons(Fport);

  {$IFNDEF FPC}
    if ({$IFDEF MSWINDOWS}WinSock{$ELSE}Libc{$ENDIF}.connect(FSd, TSockAddr(sock_addr), sizeof(sock_addr)) <0)
  {$ELSE}
    // Libc.connect don't work due to unknown cause
    if (fpconnect(FSd, PSockAddr(@sock_addr), sizeof(sock_addr)) <0)
  {$ENDIF}
      {$IFDEF MSWINDOWS}and
      (WSAGetLastError <> WSAEWOULDBLOCK){$ELSE} {and ??? errno = EWOUDLBLOCK}{$ENDIF} then begin
    {$IFDEF MSWINDOWS}
      FLastError := Format('Socket error on connect. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
    {$ENDIF}
      raise SocketException.Create(CR_CONN_HOST_ERROR);
    end;

    fastsend; //attempt to use fastsend TODO:

    keepalive(True); // TODO:

    // Force to set timeout 
    OldTimeout := FTimeout;
    FTimeout := -1;
    Timeout := OldTimeout;

  except
    ffcntl_mode := 0; //reset mode
  {$IFDEF MSWINDOWS}
    closesocket(FSd);
  {$ELSE}
    Libc.__close(FSd);
  {$ENDIF}
    FSd := -1;
    raise;
  end;
end;

procedure TCRVioTcp.Close;
begin
  if FSd <> -1 then begin
    shutdown(fsd, SD_BOTH);

  {$IFDEF MSWINDOWS}
    closesocket(FSd);
  {$ELSE}
    Libc.__close(FSd);
  {$ENDIF}
    FSd := -1;
  end;
  ffcntl_mode := 0;
end;

function TCRVioTcp.ReadNoWait({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer): integer;
begin
  FLastError := '';
  Result := recv(FSd, buffer[offset], count, 0);
  if Result = SOCKET_ERROR then begin
  {$IFDEF MSWINDOWS}
    FLastError := Format('Socket error on read. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
  {$ENDIF}
    Result := 0; // silent error handling
  end;
end;

function TCRVioTcp.WriteNoWait(const{performance opt} buffer: TValueArr; offset, count: integer): integer;
begin
  FLastError := '';
  Result := send(FSd, buffer[offset], count, 0);
  if Result = SOCKET_ERROR then begin
  {$IFDEF MSWINDOWS}
    FLastError := Format('Socket error on write. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
  {$ENDIF}
    Result := 0; // silent error handling
  end;
end;

function TCRVioTcp.Write(const buffer: TValueArr; offset,
  count: integer): integer;
begin
  if FSd = -1 then
    Result := 0
  else
    Result := inherited Write(buffer, offset, count);
end;

{$IFDEF MSWINDOWS}
function TCRVioTcp.WaitForData(Timeout: integer = -1): boolean;
var
  Res: integer;
  ss: TFDSet;
  time: TTimeVal;
  ptime: PTimeVal;
begin
  ss.fd_count := 1;
  ss.fd_array[0] := FSd;
  if Timeout >= 0 then begin
    time.tv_sec := Timeout;
    time.tv_usec := 0;
    ptime := @time;
  end
  else
    ptime := nil;

  FLastError := '';
  Res := select(0, @ss, nil, nil, ptime);
  if Res = SOCKET_ERROR then begin
    FLastError := Format('Socket error on select. WSAGetLastError return %d($%X)', [WSAGetLastError, WSAGetLastError]);
    raise SocketException.Create(CR_SELECT_ERROR);
  end;
  Result := Res = 1;
end;
{$ENDIF}

function TCRVioTcp.GetSocket: longint;
begin
  Result := FSd;
end;

{ THostNameResolver }

constructor THostNameResolver.Create(Vio: TCRVioTcp);
begin
  inherited Create(True);

  FVio := Vio;
  Resume;
end;

procedure THostNameResolver.Execute;
begin
  FIPAddr := FVio.IPByHostName(FVio.Fhostname);
end;

end.
