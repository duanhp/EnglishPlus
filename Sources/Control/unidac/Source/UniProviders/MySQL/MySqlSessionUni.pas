
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlSessionUni;
{$ENDIF}

interface

uses
{$IFDEF CLR}
  System.Security.Cryptography,
  System.Collections, System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF VER6P}
  Types,
{$ENDIF}
  SysUtils, Classes, Math, MemUtils, CRVio,
{$IFNDEF UNIDACPRO}
  MySqlNet, MyCall;
{$ELSE}
  MySqlNetUni, MyCallUni;
{$ENDIF}

const
  ScrambleLength = 8;
  Scramble41Length = 20;
  SHA1HashSize: integer = 20;
  ConnectTimeout: integer = 20;

type
{$IFNDEF VER6P}
  TInt64DynArray        = array of Int64;
{$ENDIF}

{ TSHA1Managed }

  TSHA1Managed = class
  protected
    H: array[0..4] of uint;
    W: array[0..79] of uint;

    function S(n: integer; X: uint): uint;
    function K(t: integer): uint;
    function f(t: integer; B, C, D: uint): uint;
    procedure DoCycle;
    procedure ComputeSHA1(const buffer: TBytes; offset, length: integer);

  public
    function ComputeHash(const buffer: TBytes; offset, length: integer): TBytes; overload;
    function ComputeHash(const buffer: TBytes): TBytes; overload;
  end;
  //  TSHA1Managed = System.Security.Cryptography.SHA1Managed;

  TrandStruct  = record
    seed1: long;
    seed2: long;
    maxValue: long;
    maxValueDbl: double;
  end;


  TMySqlSession = class
  protected
    Ferrno: longword;
    Ferror: AnsiString;

    isClosed: boolean;
    FResultSet: TObject; // used for FetchAll = False
    FIOHandler: TCRIOHandler;
    FHttpOptions: THttpOptions;

    function Getprotocol41: bool;
    procedure Authenticate(password: AnsiString);

  public
    net: TMySqlNet;

    // session parameters
    host,
    user,
    password,
    database,
    serverVersion,
    hostInfo,
    info: AnsiString;
    charset: AnsiString;

    clientFlag: longword;

    port,
    _connectTimeout,
    commandTimeout,
    threadId,
    serverCapabilities,
    serverStatus,
    serverLanguage,
    warningCount: integer;
    status: TMySqlStatus;
    scrambleBuff: TBytes;
    protocolType: MYSQL_PROTOCOL_TYPE;

    // result set
    fields: TMySqlFieldInfos;
    affectedRows: long;
    insertId: long;
    extraInfo: long;
    fieldCount: int;

    // SSL
    SSL_key, SSL_cert, SSL_ca, SSL_capath, SSL_cipher: AnsiString;

    FSkipPacket: boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Close;
    procedure FreeQuery;
    procedure WriteCommand(command: TServerCommand);
    procedure SimpleCommand(command: TServerCommand; args: TBytes; length: integer; skipCheck: bool);
    procedure ReadQueryResult;

    procedure Connect(host, user, passwd, db: AnsiString; port: longword; unix_socket: PAnsiChar; clientflag: longword);

    class procedure PasswordHashStage1(var _to: TBytes; const password: AnsiString);
    class procedure PasswordHashStage2(var _to: TBytes; salt: TValueArr; saltOffset: int);
    class procedure PasswordCrypt(from: TValueArr; fromIndex: int; _to: TBytes; password: TBytes; length: int);
    class procedure MyCrypt(_to: TBytes; from: TBytes; length: int);

    procedure scramble(password: AnsiString);

    class function MyRandom(var randSt: TrandStruct): double;
    class procedure randominit(var randSt: TrandStruct; seed1, seed2: long);
    class procedure hashPassword(result: TInt64DynArray; password: TBytes);
    procedure scramble41(password: AnsiString);
    procedure SelectDb(database: AnsiString);
    function Statistics: AnsiString;
    procedure Ping;

    property protocol41: bool read Getprotocol41;
    property errno: longword read Ferrno write Ferrno;
    property error: AnsiString read Ferror write Ferror;
    property ResultSet: TObject read FResultSet write FResultSet; // used for FetchAll = False
    property SkipPacket: boolean read FSkipPacket write FSkipPacket;

    property IOHandler: TCRIOHandler read FIOHandler write FIOHandler;
    property HttpOptions: THttpOptions read FHttpOptions write FHttpOptions;
  end;

implementation

uses
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$IFNDEF UNIDACPRO}MySqlVioPipe{$ELSE}MySqlVioPipeUni{$ENDIF},
{$ENDIF}
{$IFDEF HAVE_OPENSSL}
  CRVioTcpSSL,
{$ENDIF}
{$IFNDEF LITE}
  CRVioHttp,
{$ENDIF}
  CRVioTcp,
{$IFNDEF UNIDACPRO}
  MySqlErrors, MySqlResultSet;
{$ELSE}
  MySqlErrorsUni, MySqlResultSetUni;
{$ENDIF}

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

constructor TMySqlSession.Create;
begin
  inherited;

  net := TMySqlNet.Create;
  SetLength(scrambleBuff, Scramble41Length);
end;

destructor TMySqlSession.Destroy;
begin
  net.Free;
  net := nil;

  inherited;
end;

procedure TMySqlSession.Close;
begin
  if isClosed then
    Exit;

  isClosed := true;

  // FreeQuery; - optimization for FetchAll = False
  status := msReady;
  try
    SimpleCommand(scQuit, nil, 0, true);
  finally
    net.Close;
  end;
end;

procedure TMySqlSession.FreeQuery;
var
  row: TBytes;
  len: integer;
begin
  SetLength(row, 0);
  if status <> msReady then begin
    try
      if net = nil then
        Exit;
      if net.Compress then begin
        row := net.ReceiveBuffer;
        while (Length(row) > 8) or (row[0] <> 254) do
          row := net.ReceiveBuffer;
      end
      else
      begin
        SetLength(row, 65536);
        len := net.SkipBuffer(row, 65536, true);
        while (len > 8) or (row[0] <> 254) do
          len := net.SkipBuffer(row, 65536, len <> 16777215);
      end;

{
      //++ not synced with MySqlDirect
      row := net.ReceiveBuffer;
      while (Length(row) > 8) or (row[0] <> 254) do
        row := net.ReceiveBuffer;}

    finally
      status := msReady;
      if SkipPacket then begin
        row := net.ReceiveBuffer;
        if (Length(row) > 0) and (row[0] <> 0) then
          FreeQuery;
        SkipPacket := False;
      end;
    end;
  end;
end;

procedure TMySqlSession.WriteCommand(command: TServerCommand);
begin
  if status <> msReady then
    raise EMySqlException.Create(CR_COMMANDS_OUT_OF_SYNC);

  info := '';
  affectedRows := 0;

  net.NewCommand;
  net.WriteByte(byte(command));
end;


procedure TMySqlSession.SimpleCommand(command: TServerCommand; args: TBytes; length: integer; skipCheck: bool);
begin
  if net.vio = nil then
    Exit;

  net.vio.Timeout := commandTimeout;

  WriteCommand(command);
  if args <> nil then
    net.WriteOrSendBytes(args, 0, length);
  net.Send;

  if not skipCheck then
    net.Receive;
end;

procedure TMySqlSession.ReadQueryResult;
  procedure SendFileToServer(FileName: AnsiString);
    function MY_ALIGN(const A, L: integer): integer;
    begin
      Result := (((A) + (L) - 1) and not((L) - 1));
    end;
  const
    IO_SIZE = 4096;
  var
    buf: TBytes;
    f: TFileStream;
    c, r: Int64;
  begin
    f := nil;
    try
      net.Clear;
      f := TFileStream.Create(string(FileName), fmOpenRead + fmShareDenyWrite);
      SetLength(buf, MY_ALIGN(BufferLength - 16, IO_SIZE));

      c := f.Size;

      while c > 0 do begin
        r := Min(c, Length(buf));
        f.ReadBuffer(buf[0], r);
        net.WriteBytes(buf, 0, r);
        net.Send;
        Dec(c, r);
      end;
    finally
      f.Free;

      // Send empty packet to mark end of file
      net.SendEmpty;
      net.Receive;
    end;
  end;

var
  fieldsResult: TMySqlResultSet;
  t, pos1: integer;

begin
  serverStatus := SERVER_STATUS_AUTOCOMMIT; // reset previous state
  
  net.Receive;

  while True do begin
    pos1 := net.Position;
    fieldCount := int(net.ReadFieldLength);
    if fieldCount = 0 then begin
      affectedRows := net.ReadFieldLength;
      insertId := net.ReadFieldLength;
      if protocol41 or ((serverCapabilities and CLIENT_TRANSACTIONS) <> 0) then
        serverStatus := net.ReadInt16;
      if protocol41 then
        warningCount := net.ReadInt16;
      info := net.ReadString;
      Exit;
    end
    else
    if fieldCount = {$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.NullLength then begin // LOAD DATA LOCAL INFILE
      net.Position := pos1 + 1;
      SendFileToServer(net.ReadString);
    end
    else
      Break;
  end;
  if (serverStatus and SERVER_STATUS_AUTOCOMMIT) = 0 then
    serverStatus := serverStatus or SERVER_STATUS_IN_TRANS;
  if net.Length > net.Position then
    extraInfo := net.ReadFieldLength; (* Maybe number of rec *)

  if protocol41 then
    t := 7
  else
    t := 5;
  fieldsResult := TMySqlResultSet.Create(Self, nil, t, nil);
  try
    fieldsResult.ReadRows(False);
    fields := fieldsResult.ReadFieldsInfo(fieldCount);
    status := msGetResult;
    warningCount := 0;
  finally
    fieldsResult.Free;
  end;
end;

function TMySqlSession.Getprotocol41: bool;
begin
  Result := ((serverCapabilities and CLIENT_PROTOCOL_41) <> 0) and ((clientFlag and CLIENT_PROTOCOL_41) <> 0);
end;

procedure TMySqlSession.Authenticate(password: AnsiString);
var
  buff, passwordHash: TBytes;
  pktLength, position: integer;
  _buffer: TValueArr;
begin
  SetLength(buff, Scramble41Length);
  SetLength(passwordHash, Scramble41Length);

  net.Receive;
  pktLength := net.Length;
  position := net.Position;
  _buffer := net.Buffer;

  if (serverCapabilities and CLIENT_SECURE_CONNECTION) <> 0 then begin
    if (pktLength = 1) and (Byte(_buffer[position]) = 254) then begin
      net.Clear;
      scramble(password);
      net.Send;
      net.Receive;
    end
    else
    if (pktLength = 24) and (Byte(_buffer[position]) <> 0) then begin
      net.Clear;
      if Byte(_buffer[position]) <> Byte('*') then begin
        PasswordHashStage1(buff, password);
        Buffer.BlockCopy(buff, 0, passwordHash, 0, Scramble41Length);
        PasswordHashStage2(passwordHash, _buffer, position);
        PasswordCrypt(_buffer, position + 4, scrambleBuff, passwordHash, Scramble41Length);
      {$IFDEF CLR}
        PasswordCrypt(scrambleBuff, 0, buff, buff, Scramble41Length);
      {$ELSE}
        PasswordCrypt(@scrambleBuff[0], 0, buff, buff, Scramble41Length);
      {$ENDIF}
        net.WriteBytes(buff, 0, Scramble41Length);
      end
      else
      begin
        PasswordCrypt(_buffer, position + 4, scrambleBuff, passwordHash, Scramble41Length);
        scramble(password);
      end;
      net.Send;
      net.Receive;
    end;
  end;
end;

procedure TMySqlSession.Connect(host, user, passwd, db: AnsiString; port: longword; unix_socket: PAnsiChar; clientflag: longword);
  procedure SwithchToSSLIfNeed;
  begin
  {$IFDEF HAVE_OPENSSL}
    if (clientflag and CLIENT_SSL) <> 0 then begin
      //??? struct st_mysql_options *options= &mysql->options;
      net.Send;
      // Do the SSL layering.
      if net.vio is TCRVioTcpSSL then
        TCRVioTcpSSL(net.vio).IsSecure := True
      else
      if net.vio is TCRVioHandler then
        TCRVioHandler(net.vio).IsSecure := True;

      if (clientflag and CLIENT_PROTOCOL_41) <> 0 then
        net.WriteInt32(0) // unused
      else
        net.WriteInt16(0); // unused
    end;
  {$ENDIF}
  end;

var
  pktLength, pktPosition: integer;
  sha1: boolean;
  zeroBytes: TBytes;
  i: integer;
begin
  if host = '' then
    host := Self.host;
  if user = '' then
    user := Self.user;
  if passwd = '' then
    passwd := Self.password;
  if db = '' then
    db := Self.database;
  if port = 0 then
    port := Self.port;

  if port = 0 then
    port := MYSQL_PORT;
  if host = '' then
    host := LOCAL_HOST;

  serverStatus := SERVER_STATUS_AUTOCOMMIT;
  if FIOHandler <> nil then begin
    net.vio := TCRVioHandler.Create(string(host), port, FIOHandler, string(SSL_key), string(SSL_cert), string(SSL_ca));
    hostInfo := host + ' via ' + AnsiString(FIOHandler.ClassName);
  end
  else
{$IFNDEF LITE}
  if FHttpOptions <> nil then begin
    net.vio := TCRVioHttp.Create(FHttpOptions, host, port);
    hostInfo := host + ' via HTTP';
  end
  else
{$ENDIF}
  if (protocolType = MYSQL_PROTOCOL_DEFAULT) or (protocolType = MYSQL_PROTOCOL_TCP) then begin
    net.vio := TCRVioTcp.Create(host, port);
    hostInfo := host + ' via TCP/IP';
  end
  else
{$IFDEF HAVE_OPENSSL}
  if protocolType = MYSQL_PROTOCOL_SSL then begin
    net.vio := TCRVioTcpSSL.Create(host, port, SSL_key, SSL_cert, SSL_ca, SSL_capath, SSL_cipher);
    hostInfo := host + ' via SSL';
  end
  else
{$ENDIF}
{$IFDEF MSWINDOWS}
  if protocolType = MYSQL_PROTOCOL_PIPE then begin
    net.vio := TMySqlVioPipe.Create(string(host), port, unix_socket);
    hostInfo := host + ' via pipes';
  end
  else
{$ENDIF}
  if net.vio = nil then
    raise EMySqlException.Create(CONN_UNKNOW_PROTOCOL);
  net.vio.Timeout := _connectTimeout;
  try
    net.vio.Connect;
  except
    if net.vio is TCRVioHandler then
      raise
    else
      raise EMySqlException.Create(CR_CONN_HOST_ERROR, [host, 10061], net.vio.LastError);
  end;
  net.Receive;
  if net.ReadByte <> PROTOCOL_VERSION then
    raise EMySqlException.Create(CR_VERSION_ERROR);

  serverVersion := net.ReadString;
  threadId := net.ReadInt32;
  net.ReadBytes(scrambleBuff, 0, {$IFDEF CLR}Devart.MyDac.{$ENDIF}{$IFNDEF UNIDACPRO}MySqlSession{$ELSE}MySqlSessionUni{$ENDIF}.ScrambleLength);
  pktLength := net.Length;
  pktPosition := net.Position;
  net.Position := net.Position + 1;
  if pktLength > pktPosition + 2 then
    serverCapabilities := net.ReadByte + (Word(net.ReadByte) shl 8);
  if pktLength >= pktPosition + 18 then begin
    serverLanguage := net.ReadByte;
    serverStatus := net.ReadInt16;
    net.Position := pktPosition + 18 + 1;
  end;
  sha1 := false;
  if pktLength >= pktPosition + 18 +
    {$IFDEF CLR}Devart.MyDac.{$ENDIF}{$IFNDEF UNIDACPRO}MySqlSession{$ELSE}MySqlSessionUni{$ENDIF}.Scramble41Length -
    {$IFDEF CLR}Devart.MyDac.{$ENDIF}{$IFNDEF UNIDACPRO}MySqlSession{$ELSE}MySqlSessionUni{$ENDIF}.ScrambleLength then begin
    net.ReadBytes(scrambleBuff,
      {$IFDEF CLR}Devart.MyDac.{$ENDIF}{$IFNDEF UNIDACPRO}MySqlSession{$ELSE}MySqlSessionUni{$ENDIF}.ScrambleLength,
      {$IFDEF CLR}Devart.MyDac.{$ENDIF}{$IFNDEF UNIDACPRO}MySqlSession{$ELSE}MySqlSessionUni{$ENDIF}.Scramble41Length -
      {$IFDEF CLR}Devart.MyDac.{$ENDIF}{$IFNDEF UNIDACPRO}MySqlSession{$ELSE}MySqlSessionUni{$ENDIF}.ScrambleLength);
    sha1 := true;
  end;
  Self.host := host;
  Self.port := port;
  net.Clear;

{$IFNDEF HAVE_COMPRESS_INTERFACE}
  clientflag := clientflag and not CLIENT_COMPRESS;
{$ENDIF}
  if (clientflag and CLIENT_MULTI_STATEMENTS) <> 0 then
    clientflag := clientflag or CLIENT_MULTI_RESULTS;

  clientflag := clientflag or Self.clientFlag or CLIENT_CAPABILITIES;
{$IFDEF HAVE_OPENSSL}
  if protocolType = MYSQL_PROTOCOL_SSL then
    clientflag := clientflag or CLIENT_SSL;
{$ENDIF}

  //if db <> '' then
    clientflag := clientflag or CLIENT_CONNECT_WITH_DB;

  (* Remove options that server doesn't support *)
  clientflag := ((clientflag and
    not (CLIENT_COMPRESS or CLIENT_SSL or CLIENT_PROTOCOL_41)) or
    (clientflag and Self.serverCapabilities));
{$IFNDEF HAVE_COMPRESS_INTERFACE}
  clientflag := clientflag and not CLIENT_COMPRESS;
{$ENDIF}
  //clientflag := (clientflag and not (CLIENT_COMPRESS or CLIENT_SSL{ or CLIENT_PROTOCOL_41}) or (clientflag and Self.serverCapabilities));

{$IFDEF HAVE_OPENSSL}
  if (((Self.serverCapabilities and CLIENT_SSL) <> 0) and
    ((protocolType = MYSQL_PROTOCOL_SSL) or ((clientflag and CLIENT_SSL) <> 0))) then
    clientflag := clientflag or CLIENT_SSL
  else
    clientflag := clientflag and not CLIENT_SSL;
{$ENDIF}
  if (clientflag and CLIENT_PROTOCOL_41) <> 0 then begin
    net.WriteInt32(clientflag);
    SwithchToSSLIfNeed;
    net.WriteInt32({$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.MaxAllowedPacket);
    if charset = 'utf8' then
      net.WriteByte(33) /// utf8 charset
    else
      net.WriteByte(8); /// latin1 charset
    SetLength(zeroBytes, 32 - 9);
    for i := Low(zeroBytes) to High(zeroBytes) do
      zeroBytes[i] := 0;
    net.WriteBytes(zeroBytes);
  end
  else begin
    net.WriteInt16(clientflag);
    SwithchToSSLIfNeed;
    net.WriteInt24({$IFNDEF UNIDACPRO}MySqlNet{$ELSE}MySqlNetUni{$ENDIF}.MaxAllowedPacket and $ffff);
  end;

  Self.clientFlag := clientflag;
  if user <> '' then
    net.WriteString(user)
  else
    net.WriteByte(0);

  if (Self.serverCapabilities and CLIENT_SECURE_CONNECTION) <> 0 then begin
    if passwd <> '' then begin
      if sha1 then begin
        net.WriteByte({$IFDEF CLR}Devart.MyDac.{$ENDIF}{$IFNDEF UNIDACPRO}MySqlSession{$ELSE}MySqlSessionUni{$ENDIF}.Scramble41Length);
        scramble41(passwd);
      end
      else begin
        net.Fill(byte('x'), {$IFDEF CLR}Devart.MyDac.{$ENDIF}{$IFNDEF UNIDACPRO}MySqlSession{$ELSE}MySqlSessionUni{$ENDIF}.ScrambleLength);
        net.WriteByte(0);
      end;
    end
    else
      net.WriteByte(0);
  end
  else begin
    if passwd <> '' then
      scramble(passwd)
    else
      net.WriteByte(0);
  end;
  if {(db <> '') and }((Self.serverCapabilities and CLIENT_CONNECT_WITH_DB) <> 0) then begin
    net.WriteString(db);
    Self.database := db;
  end;
//  net.Length := net.Length - 1;
//  net.Position := net.Length;
  net.Send;
  Authenticate(passwd);
{$IFDEF HAVE_COMPRESS_INTERFACE}
  net.Compress := (clientflag and CLIENT_COMPRESS) <> 0;
{$ENDIF}
end;

class procedure TMySqlSession.PasswordHashStage1(var _to: TBytes; const password: AnsiString);
var
  pwdWithoutSpaces: AnsiString;
  sha1: TSHA1Managed;
  pwd, result: TBytes;
begin
  // Remove spaces
  pwdWithoutSpaces := StringReplace(password, AnsiString(' '), AnsiString(''), [rfReplaceAll]);
  pwdWithoutSpaces := StringReplace(pwdWithoutSpaces, AnsiString(#9), AnsiString(''), [rfReplaceAll]);

  sha1 := TSHA1Managed.Create;
  try
    pwd := Encoding.Default.GetBytes(pwdWithoutSpaces);
    result := sha1.ComputeHash(pwd);
    Buffer.BlockCopy(result, 0, _to, 0, Length(result));
  finally
    sha1.Free;
  end;
end;

class procedure TMySqlSession.PasswordHashStage2(var _to: TBytes; salt: TValueArr; saltOffset: int);
var
  buff: TBytes;
  hash: TBytes;
  sha1: TSHA1Managed;
begin
  SetLength(buff, 24);
{$IFDEF CLR}
  Buffer.BlockCopy(salt, saltOffset, buff, 0, 4);
{$ELSE}
  Move((PtrOffset(salt, saltOffset))^, buff[0], 4);
{$ENDIF}
  Buffer.BlockCopy(_to, 0, buff, 4, 20);

  sha1 := TSHA1Managed.Create;
  try
    hash := sha1.ComputeHash(buff);
    Buffer.BlockCopy(hash, 0, _to, 0, Length(hash));
  finally
    sha1.Free;
  end;
end;

class procedure TMySqlSession.PasswordCrypt(from: TValueArr; fromIndex: int; _to: TBytes; password: TBytes; length: int);
var
  i: integer;
begin
  for i := 0 to length - 1 do
    _to[i] := Byte(Byte(from[fromIndex + i]) xor password[i]);
end;

class procedure TMySqlSession.MyCrypt(_to: TBytes; from: TBytes; length: int);
var
  i: integer;
begin
  for i := 0 to length - 1 do
    _to[i] := _to[i] xor from[i];
end;

procedure TMySqlSession.scramble(password: AnsiString);
var
  randSt: TrandStruct;
  hashPass: TInt64DynArray;
  hashMessage: TInt64DynArray;
  messageBuffer: TBytes;
  msg: int;
  toStart, toIndex: int;
  extra: byte;
  t: TBytes;
  _to: TValueArr;
  b: byte;

begin
{    OFS('----------------------', 'd:\new.txt');
    OFS('TMySqlSession.scramble', 'd:\new.txt');
    OFS('scrambleBuff', 'd:\new.txt');
    OFS(Copy(scrambleBuff, 0, 8), 'd:\new.txt');}

  SetLength(messageBuffer, 8);
  SetLength(hashPass, 2);
  SetLength(hashMessage, 2);
  SetLength(t, 0);

  msg := 0;
  Buffer.BlockCopy(scrambleBuff, 0, messageBuffer, 0, 8);
  if password <> '' then begin
    toStart := net.Position;
    t := Encoding.Default.GetBytes(password);
//    OFS(t, 'd:\new.txt');
    hashPassword(hashPass, t);
    hashPassword(hashMessage, messageBuffer);

{    OFS('hashPass[0] = ' + IntToHex(hashPass[0], 16), 'd:\new.txt');
    OFS('hashPass[1] = ' + IntToHex(hashPass[1], 16), 'd:\new.txt');
    OFS('hashMessage[0] = ' + IntToHex(hashMessage[0], 16), 'd:\new.txt');
    OFS('hashMessage[1] = ' + IntToHex(hashMessage[1], 16), 'd:\new.txt');
    OFS(messageBuffer, 'd:\new.txt');}

    randominit(randSt, hashPass[0] xor hashMessage[0], hashPass[1] xor hashMessage[1]);
    while msg < Length(messageBuffer) do begin
      Inc(msg);
      b := byte(Math.Floor(MyRandom(randSt)*31)+64);
      // OFS(IntToHex(b, 2), 'd:\new.txt');
      net.WriteByte(b);
    end;
    extra := byte(Math.Floor(MyRandom(randSt)*31));
    toIndex := net.Position;
    _to := net.Buffer;
    //  OFS('-----------', 'd:\new.txt');
    while toStart <> toIndex do begin
      Byte(_to[toStart]) := Byte(_to[toStart]) xor extra;
      // OFS(IntToHex(_to[toStart], 2), 'd:\new.txt');
      Inc(toStart);
    end;
  end;
  net.WriteByte(0);
end;

class function TMySqlSession.MyRandom(var randSt: TrandStruct): double;
var
  d: double;
begin
  randSt.seed1 := (randSt.seed1*3 + randSt.seed2) mod randSt.maxValue;
  randSt.seed2 := (randSt.seed1 + randSt.seed2+33) mod randSt.maxValue;
  d := randSt.seed1;
  Result := (d / randSt.maxValueDbl);
end;

class procedure TMySqlSession.randominit(var randSt: TrandStruct; seed1, seed2: long);
begin

  (* For mysql 3.21.# *)
  randSt.maxValue := $3FFFFFFF;
  randSt.maxValueDbl := randSt.maxValue;
  randSt.seed1 := seed1 mod randSt.maxValue;
  randSt.seed2 := seed2 mod randSt.maxValue;
end;


class procedure TMySqlSession.hashPassword(result: TInt64DynArray; password: TBytes);
var
  nr, add, nr2, tmp: long;
  i: integer;
begin
  nr := 1345345333;
  add := 7;
  nr2 := $12345671;
  for i := 0 to Length(password) - 1 do begin
    if (password[i] = Byte(' ')) or (password[i] = Byte(#9)) then
      continue; (* skipp space in password *)
    tmp := long(password[i]);
    nr := nr xor ((((nr and 63)+add)*tmp) + (nr shl 8));
    Inc(nr2, (nr2 shl 8) xor nr);
    Inc(add, tmp);                         
  end;
  result[0] := nr and ((long(1) shl 31) - long(1)); (* Don't use sign bit (str2int) *);
  result[1] := nr2 and ((long(1) shl 31) - long(1));
end;

procedure TMySqlSession.scramble41(password: AnsiString);
var
  ctx: TSHA1Managed;
  passwordBuff, hashStage1, hashStage2, messageBuff, hashStageTo: TBytes;
begin
  ctx := TSHA1Managed.Create;
  try
    passwordBuff := Encoding.Default.GetBytes(password);
    hashStage1 := ctx.ComputeHash(passwordBuff);
    hashStage2 := ctx.ComputeHash(hashStage1);
    SetLength(messageBuff, Scramble41Length + SHA1HashSize);
    Buffer.BlockCopy(scrambleBuff, 0, messageBuff, 0, Scramble41Length);
    Buffer.BlockCopy(hashStage2, 0, messageBuff, Scramble41Length, SHA1HashSize);
    hashStageTo := ctx.ComputeHash(messageBuff);
    MyCrypt(hashStageTo, hashStage1, Scramble41Length);
    net.WriteBytes(hashStageTo);
  finally
    ctx.Free;
  end;
end;


procedure TMySqlSession.SelectDb(database: AnsiString);
var
  buff: TBytes;
begin
  buff := Encoding.Default.GetBytes(database);
  SimpleCommand(scInitDb, buff, Length(buff), false);
  Self.database := database;
end;

function TMySqlSession.Statistics: AnsiString;
begin
  SimpleCommand(scStatistics, nil, 0, false);
  Result := net.ReadString(net.Length);
end;

procedure TMySqlSession.Ping;
begin
  SimpleCommand(scPing, nil, 0, false);
end;

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

{ TSHA1Managed }

function TSHA1Managed.S(n: integer; X: uint): uint;
begin
  Result := (X shl n) or (X shr (32 - n));
end;

function TSHA1Managed.K(t: integer): uint;
begin
  if (0 <= t) and (t <= 19) then
    Result := $5A827999
  else
  if (20 <= t) and (t <= 39) then
    Result := $6ED9EBA1
  else
  if (40 <= t) and (t <= 59) then
    Result := $8F1BBCDC
  else
  if (60 <= t) and (t <= 79) then
    Result := $CA62C1D6
  else
    Result := 0;
end;

function TSHA1Managed.f(t: integer; B, C, D: uint): uint;
begin
  if (0 <= t) and (t <= 19) then
    Result := (B and C) or ((not B) and D)
  else
  if (20 <= t) and (t <= 39) then
    Result := B xor C xor D
  else
  if (40 <= t) and (t <= 59) then
    Result := (B and C) or (B and D) or (C and D)
  else
  if (60 <= t) and (t <= 79) then
    Result := B xor C xor D
  else
    Result := 0;
end;

procedure TSHA1Managed.DoCycle;
var
  t: integer;
  A, B, C, D, E: uint;
  temp: uint;
begin
  for t := 16 to 80 - 1 do
    W[t] := S(1, W[t-3] xor W[t-8] xor W[t-14] xor W[t-16]);

  A := H[0];
  B := H[1];
  C := H[2];
  D := H[3];
  E := H[4];

  for t := 0 to 79 do begin
    temp := S(5,A) + f(t,B,C,D) + E + W[t] + K(t);
    E := D;
    D := C;
    C := S(30,B);
    B := A;
    A := temp;
  end;

  H[0] := H[0] + A;
  H[1] := H[1] + B;
  H[2] := H[2] + C;
  H[3] := H[3] + D;
  H[4] := H[4] + E;
end;

procedure TSHA1Managed.ComputeSHA1(const buffer: TBytes; offset, length: integer);
var
  _length, _offset, count, u, u1: uint;
  i: uint;
  j, k: integer;
begin
  count := 0;
  _length := uint(length);
  _offset := uint(offset);

  H[0] := $67452301;
  H[1] := $EFCDAB89;
  H[2] := $98BADCFE;
  H[3] := $10325476;
  H[4] := $C3D2E1F0;

  while (_length - count) >= 64 do begin
    for i := 0 to 16 - 1 do
      W[i] := 0;
    for i := 0 to 64 - 1 do begin
      k := 8*(3-(i mod 4));
      j := i div 4;
      W[j] := W[j] + (uint(buffer[_offset+i+count])) shl k;
    end;

    DoCycle;

    Inc(count, 64);
  end;

  if _length - count <= 55 then begin
    for u := 0 to 16 - 1 do
      W[u] := 0;
    for u := count to _length - 1 do begin
      j := (u-count) div 4;
      k := 8*(3-(u-count) mod 4);

      u1 := _offset+u;
      W[j] := W[j] + (uint(buffer[u1])) shl k;
    end;
    j := (_length - count) div 4;
    k := 8*(3-(_length-count) mod 4);
    W[j] := W[j] + uint($80) shl k;
    W[15] := uint(_length) * 8;

    DoCycle();
  end
  else
  begin
    for u := 0 to 16 - 1 do
      W[u] := 0;
    for u := count to _length - 1 do begin
      j := (u-count) div 4;
      k := 8*(3-(u-count) mod 4);
      W[j] := W[j] + (uint(buffer[_offset+u])) shl k;
    end;
    j := (_length - count) div 4;
    k := 8*(3-(_length-count) mod 4);
    W[j] := W[j] + uint($80) shl k;

    DoCycle();

    for u := 0 to 16 - 1 do
      W[u] := 0;
    W[15] := uint(_length) * 8;

    DoCycle();
  end;
end;

function TSHA1Managed.ComputeHash(const buffer: TBytes; offset, length: integer): TBytes;
var
  i, j: integer;
  h_: uint;
begin
  ComputeSHA1(buffer, offset, length);

  SetLength(result, 20);
  j := 0;
  for i := 0 to 5 - 1 do begin
    h_ := H[i];
    result[j + 3] := byte(h_);
    result[j + 2] := byte(h_ shr 8);
    result[j + 1] := byte(h_ shr 16);
    result[j] := byte(h_ shr 24);
    Inc(j, 4);
  end;
end;

function TSHA1Managed.ComputeHash(const buffer: TBytes): TBytes;
begin
  Result := ComputeHash(buffer, 0, Length(buffer));
end;

end.
