{$I ..\Dac.inc}

unit Devart.Dac.CRVioTcp;

interface

uses
  System.Text, System.Net, System.Net.Sockets, System.Threading,
  Classes, CRVio;

type
  TCRTcpClient = class(TcpClient)
  public
    constructor Create;
    function Handle: IntPtr;
  end;

  TCRVioTcp = class(TCRVio)
  protected
    Ftcp: TCRTcpClient;
    Fstream: NetworkStream;
    Fhostname: string;
    Fport: integer;
    Ftimeout: integer;

    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;

  public
    constructor Create(const hostname: string; const port: integer);

    procedure Connect; override;
    procedure Close; override;
    function Read(var{performance opt} buffer: TBytes; offset, count: integer): integer; override;
    function Write(const{performance opt} buffer: TBytes; offset, count: integer): integer; override;
    function ReadNoWait(var{performance opt} buffer: TBytes; offset, count: integer): integer; override;
    function WriteNoWait(const{performance opt} buffer: TBytes; offset, count: integer): integer; override;

    function GetSocket: IntPtr; virtual;
  end;

implementation

uses
  SysUtils,
  Windows, Math;

function IsIpAddress(const hostname: string): boolean;
var
  i: integer;
  ch: char;
begin
  Result := hostname = '';
  if not Result then
    Exit;

  for i := 1 to Length(hostname) do begin
    ch := hostname[i];
    if ((ch < '0') or (ch > '9')) and (ch <> '.') then begin
      Result := False;
      Exit;
    end;
  end;
end;

{ TCRTcpClient }

constructor TCRTcpClient.Create;
begin
  inherited;
  GC.SuppressFinalize(Client);
end;

function TCRTcpClient.Handle: IntPtr;
begin
  Assert(Client <> nil);
  Result := Client.Handle;
end;

{ TCRVioTcp }

constructor TCRVioTcp.Create(const hostname: string; const port: integer);
begin
  inherited Create;
  Fhostname := hostname;
  Fport := port;
end;

function TCRVioTcp.GetTimeout: integer;
begin
  Result := Ftimeout;
end;

procedure TCRVioTcp.SetTimeout(Value: integer);
begin
  if Value > MaxInt div 1000 then
    Value := MaxInt div 1000;

  if timeout <> Value then begin
    if FTcp <> nil then
      FTcp.ReceiveTimeout := value * 1000;
    Ftimeout := value;
  end;
end;

procedure TCRVioTcp.Connect;
var
  ip: IPAddress;
begin
  inherited;

  Ftcp := TCRTcpClient.Create;
  Ftcp.ReceiveTimeout := Ftimeout * 1000;

  Ftcp.NoDelay := True;
  if IsIpAddress(Fhostname) then begin
    ip := IPAddress.Parse(Fhostname);
    Ftcp.Connect(ip, Fport);
  end
  else
    Ftcp.Connect(Fhostname, Fport);
  Fstream := Ftcp.GetStream;
  GC.SuppressFinalize(Fstream);
end;

procedure TCRVioTcp.Close;
begin
  if Fstream <> nil then begin
    Fstream.Close;
    Fstream.Free;
    Fstream := nil;
  end;

  if FTcp <> nil then begin
    Ftcp.Close;
    Ftcp.Free;
    Ftcp := nil;
  end;
end;

function TCRVioTcp.ReadNoWait(var{performance opt} buffer: TBytes; offset, count: integer): integer;
const
  MaxRecvSize: integer = 131072;
begin
  try
    Result := Fstream.Read(buffer, offset, Math.Min(MaxRecvSize, count));
  except
    Result := 0;
  end;
end;

function TCRVioTcp.WriteNoWait(const{performance opt} buffer: TBytes; offset, count: integer): integer;
begin
  try
    Fstream.Write(buffer, offset, count);
    Result := count;
  except
    Result := 0;
  end;
end;

function TCRVioTcp.Read(var{performance opt} buffer: TBytes; offset, count: integer): integer;
begin
  if Fstream = nil then
    Result := 0
  else
    Result := inherited Read(buffer, offset, count);
end;

function TCRVioTcp.Write(const{performance opt} buffer: TBytes; offset, count: integer): integer;
begin
  if Fstream = nil then
    Result := 0
  else
    Result := inherited Write(buffer, offset, count);
end;

function TCRVioTcp.GetSocket: IntPtr;
begin
  Result := Ftcp.Handle;
end;

end.
