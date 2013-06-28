
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlErrorsUni;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils,
  {$IFNDEF UNIDACPRO}MyCall{$ELSE}MyCallUni{$ENDIF};

const
  ER_NET_PACKETS_OUT_OF_ORDER = 1156;
(*
  UNKNOWN_ERROR = 2000;
  SOCKET_CREATE_ERROR = 2001;
  CONNECTION_ERROR = 2002;
  CONN_HOST_ERROR = 2003;
  IPSOCK_ERROR  = 2005;
  UNKNOWN_HOST  = 2005;
  SERVER_GONE_ERROR = 2006;
  VERSION_ERROR = 2007;
  OUT_OF_MEMORY = 2008;
  WRONG_HOST_INFO = 2009;
  LOCALHOST_CONNECTION = 2010;
  TCP_CONNECTION = 2011;
  SERVER_HANDSHAKE_ERR = 2012;
  SERVER_LOST  = 2013;
  COMMANDS_OUT_OF_SYNC = 2014;
  NAMEDPIPE_CONNECTION = 2015;
  NAMEDPIPEWAIT_ERROR  = 2016;
  NAMEDPIPEOPEN_ERROR  = 2017;
  NAMEDPIPESETSTATE_ERROR = 2018;
  CANT_READ_CHARSET = 2019;
  NET_PACKET_TOO_LARGE = 2020;
  EMBEDDED_CONNECTION = 2021;
  PROBE_SLAVE_STATUS   = 2022;
  PROBE_SLAVE_HOSTS    = 2023;
  PROBE_SLAVE_CONNECT  = 2024;
  PROBE_MASTER_CONNECT = 2025;
  SSL_CONNECTION_ERROR = 2026;
  MALFORMED_PACKET     = 2027;

  { new 4.1 error codes }
  NULL_POINTER  = 2028;
  NO_PREPARE_STMT = 2029;
  NOT_ALL_PARAMS_BOUND = 2030;
  DATA_TRUNCATED = 2031;
  NO_PARAMETERS_EXISTS = 2032;
  INVALID_PARAMETER_NO = 2033;
  INVALID_BUFFER_USE = 2034;
  UNSUPPORTED_PARAM_TYPE = 2035;

  SHARED_MEMORY_CONNECTION             = 2036;
  SHARED_MEMORY_CONNECT_REQUEST_ERROR  = 2037;
  SHARED_MEMORY_CONNECT_ANSWER_ERROR   = 2038;
  SHARED_MEMORY_CONNECT_FILE_MAP_ERROR = 2039;
  SHARED_MEMORY_CONNECT_MAP_ERROR      = 2040;
  SHARED_MEMORY_FILE_MAP_ERROR         = 2041;
  SHARED_MEMORY_MAP_ERROR              = 2042;
  SHARED_MEMORY_EVENT_ERROR      = 2043;
  SHARED_MEMORY_CONNECT_ABANDODED_ERROR = 2044;
  SHARED_MEMORY_CONNECT_SET_ERROR      = 2045;
  CONN_UNKNOW_PROTOCOL   = 2046;
  INVALID_CONN_HANDLE   = 2047;
  *)

  ClientErrors: array [0..47] of string = (
    'Unknown MySQL error',
    'Can''t create UNIX socket (%d)',
    'Can''t connect to local MySQL server through socket ''%-.64s'' (%d)',
    'Can''t connect to MySQL server on ''%s'' (%d)',
    'Can''t create TCP/IP socket (%d)',
    'Unknown MySQL Server Host ''%-.64s'' (%d)',
    'MySQL server has gone away',
    'Protocol mismatch. Server Version = %d Client Version = %d',
    'MySQL client run out of memory',
    'Wrong host info',
    'Localhost via UNIX socket',
    '%-.64s via TCP/IP',
    'Error in server handshake',
    'Lost connection to MySQL server during query',
    'Commands out of sync;  You can''t run this command now',
    '%-.64s via named pipe',
    'Can''t wait for named pipe to host: %s  pipe: %s (%s)',
    'Can''t open named pipe to host: %s  pipe: %s (%s)',
    'Can''t set state of named pipe to host: %s  pipe: %s (%s)',
    'Can''t initialize character set %s (path: %s)',
    'Got packet bigger than ''max_allowed_packet''',
    'Embedded server',
    'Error on SHOW SLAVE STATUS:',
    'Error on SHOW SLAVE HOSTS:',
    'Error connecting to slave:',
    'Error connecting to master:',
    'SSL connection error',
    'Malformed packet',
    'Invalid use of null pointer',
    'Statement not prepared',
    'Not all parameters data supplied',
    'Data truncated',
    'No parameters exists in the statement',
    'Invalid parameter number',
    'Can''t send long data for non string or binary data types (parameter: %s)',
    'Using unsupported buffer type: %s  (parameter: %s)',
    'Shared memory (%s)',
    'Can''t open shared memory. Request event don''t create  (%s)',
    'Can''t open shared memory. Answer event don''t create  (%s)',
    'Can''t open shared memory. File mapping don''t create  (%s)',
    'Can''t open shared memory. Map of memory don''t create  (%s)',
    'Can''t open shared memory. File mapping don''t create for client (%s)',
    'Can''t open shared memory. Map of memory don''t create for client (%s)',
    'Can''t open shared memory. %s event don''t create for client (%s)',
    'Can''t open shared memory. Server abandoded and don''t sent the answer event (%s)',
    'Can''t open shared memory. Can''t send the request event to server (%s)',
    'Wrong or unknown protocol',
    'Invalid connection handle');

type
  EMySqlException = class(Exception)
  protected
    FErrorCode: integer;

  public
    constructor Create(ErrorCode: integer); overload;
    constructor Create(ErrorCode: integer; Msg: string); overload;
    constructor Create(ErrorCode: integer; const Args: array of const; Msg: string); overload;

    property ErrorCode: integer read FErrorCode;
  end;


function GetError(e: integer): string; overload;
function GetError(e: integer; const Args: array of const): string; overload;

implementation

const
  MinError: integer = 2000; (* For easier client code *)
  //MaxError: integer = 2999;

function GetError(e: integer): string;
begin
  e := e - MinError;
  if (e < Low(ClientErrors)) or (e > High(ClientErrors)) then
    Result := ''
  else
    Result := ClientErrors[e];
end;

function GetError(e: integer; const Args: array of const): string;
begin
  Result := GetError(e);
  Result := Format(Result, Args);
end;

{ MySqlException }

constructor EMySqlException.Create(ErrorCode: integer);
begin
{$IFDEF CLR}
  inherited Create(GetError(ErrorCode));
{$ELSE}
  inherited
  Message := GetError(ErrorCode);
{$ENDIF}

  FErrorCode := ErrorCode;
end;

constructor EMySqlException.Create(ErrorCode: integer; Msg: string);
begin
  Create(ErrorCode, [], Msg);
end;

constructor EMySqlException.Create(ErrorCode: integer; const Args: array of const; Msg: string);
var
  s: string;
begin
  s := GetError(ErrorCode);
  s := Format(s, Args);
  if Msg <> '' then
    s := s + #$D#$A + Msg;

{$IFDEF CLR}
  inherited Create(s);
{$ELSE}
  inherited
  Message := s;
{$ENDIF}
  FErrorCode := ErrorCode;
end;

end.
