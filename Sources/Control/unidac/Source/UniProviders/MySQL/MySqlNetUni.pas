
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlNetUni;
{$ENDIF}

interface

uses
{$IFDEF CLR}
  System.IO, System.Text, System.Runtime.InteropServices,
{$ELSE}
{$IFDEF MSWINDOWS}
  WinSock, {$IFNDEF FPC}ScktComp,{$ENDIF}
{$ENDIF}
  CLRClasses,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  SysUtils, Classes, Math, MemUtils, CRVio,
{$IFNDEF UNIDACPRO}
  MyCall;
{$ELSE}
  MyCallUni;
{$ENDIF}

const
  NET_HEADER_SIZE: integer = 4; // NET_HEADER_SIZE
  COMP_HEADER_SIZE: integer = 3; // COMP_HEADER_SIZE
  BufferLength: integer = 8192; // net_buffer_length
  MaxAllowedPacket: integer = 1024 * 1024 * 1024; // max_allowed_packet
  MaxPacketLength: integer = 256 * 256 * 256 - 1; // max_packet_length
  NullLength: integer = -1; // NULL_LENGTH

type
  TMySqlNet = class
  protected
    Fvio: TCRVio;
    Fbuffer: MemoryStream;
  {$IFDEF HAVE_COMPRESS_INTERFACE}
    FCompressBuffer: MemoryStream;
  {$ENDIF}

    (*
    see mysql-4.0.21 src\libmysql\net.c  net->FPacketNumber variable
    -----------------------------
    FPacketNumber and FComprPacketNumber used  in
      net_flush. Called from net_write_command, libmysql.c mysql_real_connect
        if (net->compress) net->FPacketNumber=net->FComprPacketNumber;
      my_net_write, net_write_command (libmysql.c simple_command)
        while (len >= MAX_PACKET_LENGTH) buff[3]=net->FPacketNumber++
      net_real_write. Called from net_flush, net_write_buff (Called from my_net_write(libmysql.c mysql_real_connect))
        if Compr then b[3]=net->FComprPacketNumber++
      my_real_read. Called from my_net_read (libmysql.c net_safe_read)
        Check net->FPacketNumber
        net->FComprPacketNumber= ++net->FPacketNumber;
    -----------------------------
    simple_command (libmysql.c) sequence:
      net_write_command
        for each full MPL packet do
          net_write_buff
            net_real_write
              if Compr then FComprPacketNumber++
              vio_write
          Save FPacketNumber
          FPacketNumber++
        net_write_buff
          net_real_write
            if Compr then
              Save FComprPacketNumber
              FComprPacketNumber++
            vio_write
        FPacketNumber++
        net_flush
          net_real_write
            if Compr then
              Save FComprPacketNumber
              FComprPacketNumber++
            vio_write
          if Compr then FPacketNumber = FComprPacketNumber
    -----------------------------
    simple_command (libmysql.c) simplified sequence:
      net_write_command
        for each MPL packet do (RealSendPacket)
          net_write_buf
            net_real_write
              if Compr then
                Save FComprPacketNumber
                FComprPacketNumber++
              vio_write
          Save FPacketNumber (1)
          FPacketNumber++
        if Compr then FPacketNumber = FComprPacketNumber
    -----------------------------
    net_safe_read (libmysql.c) sequence:
      my_net_read
        for each MPL packet do
          my_real_read
            vio_read
            Check FPacketNumber
            FPacketNumber++
            FComprPacketNumber = FPacketNumber
          my_uncompress
    *)
    FPacketNumber, FComprPacketNumber: byte; 
    FHeader, FCompressHeader: TBytes;
    FCompress: boolean;

    function GetBuffer: TValueArr;
    function GetLength: integer;
    procedure SetLength(Value: integer);
    function GetPosition: integer;
    procedure SetPosition(Value: integer);

    procedure WriteHeader(Len: integer);
    procedure RealSend(const buffer: TValueArr; offset, count: integer); // net_real_write analog
    procedure RealReceive({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer); // net_real_read analog
    function ReceiveHeader: integer;

    procedure SetVio(Value: TCRVio);
    procedure VioRead({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer);
    procedure VioWrite(const{performance opt} buffer: TValueArr; offset, count: integer);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;

    procedure Send;
    procedure SendEmpty; // useful for "LOAD DATA LOCAL INFILE" statement
    procedure Receive;
    function ReceiveBuffer: TBytes;
    function SkipBuffer(out buf: TBytes; len: integer; firstPacket: boolean): integer;

    procedure Clear;

    procedure NewCommand;
    procedure WriteByte(value: byte);
    procedure WriteBytes(const buffer: TBytes; offset, count: integer); overload;
    procedure WriteBytes(const buffer: TBytes); overload;
    procedure WriteBytes(buffer: {$IFDEF CLR}IntPtr{$ELSE}PAnsiChar{$ENDIF}; offset, count: integer); overload;
    procedure WriteOrSendBytes(const buffer: TBytes; offset, count: integer);

    procedure WriteBool(value: boolean);
    procedure WriteInt16(value: integer);
    procedure WriteInt24(value: integer);
    procedure WriteInt32(value: integer);
    procedure WriteInt64(value: Int64);
    procedure WriteDouble(value: double);
    procedure WriteString(const value: AnsiString); overload;
  {$IFNDEF CLR}
    procedure WriteString(value: PAnsiChar); overload;
  {$ENDIF}
    procedure WriteFieldLength(length: integer);
    procedure Fill(pattern: byte; count: integer);

    procedure ReadBytes(var buffer: TBytes; offset, count: integer);
    function ReadByte: byte;
    function ReadInt16: short;
    function ReadUInt16: integer;
    function ReadInt24: integer;
    function ReadInt32: integer;
    function ReadInt64: Int64;
    function ReadDouble: double;
    function ReadString(l: integer): AnsiString; overload;
    function ReadString: AnsiString; overload;
    procedure ReadError;
    function ReadFieldLength: Int64;


    property Buffer: TValueArr read GetBuffer;
    property Length: integer read GetLength write SetLength;
    property Position: integer read GetPosition write SetPosition;
    property Compress: boolean read FCompress write FCompress;

    property vio: TCRVio read Fvio write SetVio;
  end;

{$IFDEF HAVE_COMPRESS_INTERFACE}
{$IFNDEF HAVE_COMPRESS_INTERNAL}

//zlib imports
{$IFDEF MSWINDOWS}
{$L zlib\compress.obj}
{$L zlib\uncompr.obj}
{$L zlib\deflate.obj}
{$L zlib\adler32.obj}
{$L zlib\trees.obj}

{$L zlib\inflate.obj}
{$L zlib\infblock.obj}
{$L zlib\infcodes.obj}
{$L zlib\inffast.obj}
{$L zlib\inftrees.obj}
{$L zlib\infutil.obj}
function compress(dest: IntPtr; destLen: IntPtr; const source: IntPtr; sourceLen: longint): longint; external;
function uncompress(dest: IntPtr; destLen: IntPtr; source: IntPtr; sourceLen: longint): longint; external;
{$ELSE}
var
  compress: function(dest: pointer; destLen: pointer; const source: pointer; sourceLen: longint): longint; cdecl;
  uncompress: function(dest: pointer; destLen: pointer; source: pointer; sourceLen: longint): longint; cdecl;
{$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

uses
{$IFDEF VIO_DEBUG}
  Debug,
{$ENDIF}
  {$IFNDEF UNIDACPRO}MySqlErrors{$ELSE}MySqlErrorsUni{$ENDIF};

{$IFDEF HAVE_COMPRESS_INTERFACE}
{$IFNDEF HAVE_COMPRESS_INTERNAL}
const
  _z_errmsg: array[0..9] of PAnsiChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    '',                     // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

{$IFDEF MSWINDOWS}
//needed by zlib
procedure _memset(P: IntPtr; B: Byte; count: Integer);cdecl;
begin
  FillChar(P, count, B);
end;

//needed by zlib
function zcalloc(AppData: IntPtr; Items, Size: Integer): IntPtr;
begin
  Result := Marshal.AllocHGlobal(Items * Size);
end;

//needed by zlib
procedure zcfree(AppData, Block: IntPtr);
begin
  Marshal.FreeHGlobal(Block);
end;

procedure _memcpy(dest, source: IntPtr; count: Integer); cdecl;
begin
  CopyBuffer(source, dest, count);
end;
{$ENDIF}
{$ENDIF}

function my_uncompress(packet: {$IFNDEF FPC}PByte{$ELSE}PChar{$ENDIF}; _len: PLongint; complen: PLongint): boolean;
var
  compbuf: PByte;
begin
{$IFDEF VIO_DEBUG}
  OFS('+my_uncompress(..., ' + IntToStr(_len^) + ', ' + IntToStr(complen^) + ')');
  OFS(packet, _len^);
  try
{$ENDIF}
  if (complen^ <> 0) then begin //do we have anything to uncompress
    getmem(compbuf, complen^);
    if (compbuf = nil) then begin//out of memory
      Result := false;
      exit;
    end;
    CheckZLib;
    try
      DoUncompress(compbuf, complen, packet, _len^);
    except
      freemem(compbuf);
      Result := false;
      exit;
    end;
    _len^ := complen^; //give back the uncompressed packet

    CopyBuffer(compbuf, packet, _len^);
    freemem(compbuf);
  end;
  Result := true;
{$IFDEF VIO_DEBUG}
  finally
    OFS('-------------------');
    OFS(packet, _len^);
    OFS('-my_uncompress(..., ' + IntToStr(_len^) + ', ' + IntToStr(complen^) + ') = ' + BoolToStr(result, True));
  end;
{$ENDIF}
end;
{$ENDIF}

{ TMySqlNet }

constructor TMySqlNet.Create;
begin
  inherited;
  Fbuffer := MemoryStream.Create(BufferLength);
{$IFDEF HAVE_COMPRESS_INTERFACE}
  FCompressBuffer := MemoryStream.Create(MIN_COMPRESS_LENGTH * 2);
{$ENDIF}
  {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.SetLength(FHeader, NET_HEADER_SIZE);
  {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.SetLength(FCompressHeader, NET_HEADER_SIZE + COMP_HEADER_SIZE);
end;

destructor TMySqlNet.Destroy;
begin
  try
    Close;
  except
  end;

{$IFDEF HAVE_COMPRESS_INTERFACE}
  FCompressBuffer.Free;
{$ENDIF}
  Fbuffer.Free;
  inherited;
end;

procedure TMySqlNet.Close;
begin
  if Fbuffer <> nil then
    Fbuffer.Close;

{$IFDEF HAVE_COMPRESS_INTERFACE}
  if FCompressBuffer <> nil then
    FCompressBuffer.Close;
{$ENDIF}

  if Fvio <> nil then
    try
      Fvio.Close;
    finally
      Fvio.Free;
      Fvio := nil;
    end;
end;

procedure TMySqlNet.Send; // net_flush
begin
  if Length <> NET_HEADER_SIZE then begin
    WriteHeader(Length - NET_HEADER_SIZE);
    RealSend({$IFDEF CLR}Buffer{$ELSE}@Buffer[0]{$ENDIF}, 0, Length);
    Clear;
  end;
{$IFDEF HAVE_COMPRESS_INTERFACE}
  if FCompress then
    FPacketNumber := FComprPacketNumber;
{$ENDIF}
end;

procedure TMySqlNet.SendEmpty;
begin
  Assert(Length = NET_HEADER_SIZE);
  WriteHeader(0);
  RealSend({$IFDEF CLR}Buffer{$ELSE}@Buffer[0]{$ENDIF}, 0, Length);
  
{$IFDEF HAVE_COMPRESS_INTERFACE}
  if FCompress then
    FPacketNumber := FComprPacketNumber;
{$ENDIF}
end;

procedure TMySqlNet.WriteHeader(Len: integer);
var
  OldPosition: integer;
begin
  OldPosition := Position;
  Position := 0;
  WriteInt24(Len);
  WriteByte(FPacketNumber);
  if FPacketNumber <> $FF then
    Inc(FPacketNumber)
  else
    FPacketNumber := 0;
  Position := OldPosition;
end;

procedure TMySqlNet.RealSend(const buffer: TValueArr; offset, count{"ulong len" variable from net_real_write}: integer); // net_real_write analog
{
(a) send buffer without compression (Compress = False)
(b) send small packets without compression (count < MIN_COMPRESS_LENGTH). Temporary buffer for header need
(c) send big packets without compression (ZIP, JPG etc).
(d) send big packets with compression (TXT etc). Temporary buffer for header and compressed data need
}
{$IFDEF HAVE_COMPRESS_INTERFACE}
var
  header_length: integer;
  TempBuffer: TBytes; // "uchar *b" variable from net_real_write
  complen: integer;
  write_count: integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERFACE}
  if FCompress then begin
    header_length := NET_HEADER_SIZE + COMP_HEADER_SIZE;

    // (b)
    if count <= MIN_COMPRESS_LENGTH then begin
      {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.SetLength(TempBuffer, header_length + count);
      complen := 0;
    end
    else // try to compress
    begin
      // see my_compress_alloc
      // *complen=  *len * 120 / 100 + 12;
      complen := count + (count div 5) + 12;
      {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.SetLength(TempBuffer, header_length + complen);
      CheckZLib;
      try
        DoCompress(@TempBuffer[header_length], @complen, @buffer[offset], count);
      except
        complen := 0; // (d)
      end;
    end;

    if complen = 0 then begin // (b) or (d) - compression failed
      Move(buffer[offset], TempBuffer[header_length], count);
      write_count := header_length + count;

      // int3store(&b[NET_HEADER_SIZE],complen);
      TempBuffer[4] := byte(complen);
      TempBuffer[5] := byte(complen shr 8);
      TempBuffer[6] := byte(complen shr 16);
    end
    else  // (c) - succesfully compressed
    begin
      write_count := header_length + complen;

      // int3store(&b[NET_HEADER_SIZE],complen);
      TempBuffer[4] := byte(count);
      TempBuffer[5] := byte(count shr 8);
      TempBuffer[6] := byte(count shr 16);
    end;

    // int3store(b,len);
    TempBuffer[0] := byte((write_count - header_length));
    TempBuffer[1] := byte((write_count - header_length) shr 8);
    TempBuffer[2] := byte((write_count - header_length) shr 16);

    // b[3]=(uchar) (net->compress_pkt_nr++);
    TempBuffer[3] := byte(FComprPacketNumber);
    if FComprPacketNumber <> $FF then
      Inc(FComprPacketNumber)
    else
      FComprPacketNumber := 0;

    VioWrite({$IFDEF CLR}TempBuffer{$ELSE}@TempBuffer[0]{$ENDIF}, 0, write_count);
  end
  else
{$ENDIF}
    VioWrite(buffer, offset, count); //(a)
end;

procedure TMySqlNet.RealReceive({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer); // net_real_read analog
{$IFDEF HAVE_COMPRESS_INTERFACE}
  function ReceiveCompHeader(out complen: integer): integer;
  var
    OutOfOrder: boolean;
    count: integer;
    OldComprPacketNumber: integer;

  begin
    Assert(Fvio <> nil);
    Assert(FCompress);
    count := NET_HEADER_SIZE + COMP_HEADER_SIZE;

  {$IFDEF CLR}
    VioRead(FCompressHeader, 0, count);
  {$ELSE}
    VioRead(PAnsiChar(@FCompressHeader[0]), 0, count);
  {$ENDIF}

    OldComprPacketNumber := integer(FPacketNumber);
    OutOfOrder := OldComprPacketNumber <> integer(FCompressHeader[3]);
    if FPacketNumber <> $FF then
      Inc(FPacketNumber)
    else
      FPacketNumber := 0;
    FComprPacketNumber := FPacketNumber;
  {$IFDEF VIO_DEBUG}
    if OutOfOrder then
      OFS(Format('ReceiveCompHeader: Net packets out of order: received[%d], expected[%d]', [FCompressHeader[3], OldComprPacketNumber]));
  {$ENDIF}
    if OutOfOrder then
      raise EMySqlException.Create({$IFNDEF UNIDACPRO}MySqlErrors{$ELSE}MySqlErrorsUni{$ENDIF}.ER_NET_PACKETS_OUT_OF_ORDER,
        Format('ReceiveCompHeader: Net packets out of order: received[%d], expected[%d]', [FCompressHeader[3], OldComprPacketNumber]));

    FCompressHeader[3] := 0;
    Result := BitConverter.ToInt32(FCompressHeader, 0);

    complen := FCompressHeader[4] or FCompressHeader[5] shl 8 or FCompressHeader[6] shl 16;
  end;
var
  l, complen, ToRead: integer;
  buf: TValueArr;
  b: boolean;
{$ENDIF}
begin
  if not FCompress then
    VioRead(buffer, offset, count)
{$IFDEF HAVE_COMPRESS_INTERFACE}
  else
  begin
    // copy rest from FCompressBuffer
    l := FCompressBuffer.Length - FCompressBuffer.Position;
    if l > count then
      l := count;

    if l > 0 then begin
      FCompressBuffer.Read(buffer, offset, l);
      count := count - l;
      offset := offset + l;
    end;

    while count > 0 do begin
      FCompressBuffer.Position := 0;
      FCompressBuffer.Length := 0;

      // read new data from vio
      repeat
        l := ReceiveCompHeader(complen);
        if complen > 0 then begin
          FCompressBuffer.Length := FCompressBuffer.Position + complen;

          buf := FCompressBuffer.GetBuffer;
          VioRead(buf, FCompressBuffer.Position, l);

          if my_uncompress(@buf[FCompressBuffer.Position], @l, @complen) then
            FCompressBuffer.Position := FCompressBuffer.Position + complen
          else
            raise EMySqlException.Create(ER_NET_UNCOMPRESS_ERROR);
          //FCompressBuffer.Length := FCompressBuffer.Length - 1;
        end
        else
        begin
          FCompressBuffer.Length := FCompressBuffer.Position + l;

          buf := FCompressBuffer.GetBuffer;
          VioRead(buf, FCompressBuffer.Position, l);
        end;
      until l <> MaxPacketLength;

      // Assert(count <= FCompressBuffer.Length, Format('count > FCompressBuffer.Length, count = %d, FCompressBuffer.Length = %d', [count, FCompressBuffer.Length]));

      FCompressBuffer.Position := 0;
      b := count >= FCompressBuffer.Length; // CompressBuffer will be empty after read
      ToRead := Min(count, FCompressBuffer.Length);
      FCompressBuffer.Read(buffer, offset, ToRead);
      FCompressBuffer.Position := count;
      if b then
        FCompressBuffer.Length := 0;
      Inc(Offset, ToRead);
      Dec(count, ToRead);
    end;
  end;
{$ENDIF}
end;

function TMySqlNet.ReceiveHeader: integer;
var
  OutOfOrder: boolean;
  count: integer;
  OldPacketNumber: integer;

begin
  Assert(Fvio <> nil);
  count := NET_HEADER_SIZE;

{$IFDEF CLR}
  RealReceive(FHeader, 0, count);
{$ELSE}
  RealReceive(PAnsiChar(@FHeader[0]), 0, count);
{$ENDIF}

  if not FCompress then begin // see MySQL 4.0.21 sources, unit net_serv.cpp, procedure my_real_read
    OldPacketNumber := integer(FPacketNumber);
    OutOfOrder := OldPacketNumber <> integer(FHeader[3]);
    if FPacketNumber <> $FF then
      Inc(FPacketNumber)
    else
      FPacketNumber := 0;
    {$IFDEF VIO_DEBUG}
      if OutOfOrder then
        OFS(Format('ReceiveHeader: Net packets out of order: received[%d], expected[%d]', [FHeader[3], OldPacketNumber]));
    {$ENDIF}
    FComprPacketNumber := FPacketNumber;
    if OutOfOrder then
      raise EMySqlException.Create({$IFNDEF UNIDACPRO}MySqlErrors{$ELSE}MySqlErrorsUni{$ENDIF}.ER_NET_PACKETS_OUT_OF_ORDER,
        Format('ReceiveHeader: Net packets out of order: received[%d], expected[%d]', [FHeader[3], OldPacketNumber]));
  end;

  FHeader[3] := 0;
  Result := BitConverter.ToInt32(FHeader, 0);
end;

procedure TMySqlNet.Receive;
var
  l: integer;
  buf: TValueArr;
  offset: integer;

begin
  Position := 0;
  offset := 0;
  //OFS(Format('+TMySqlNet.Receive: FPacketNumber=%d, FComprPacketNumber=%d', [FPacketNumber, FComprPacketNumber]));

  repeat
    l := ReceiveHeader;
    Length := offset + l;
    buf := Buffer;
    RealReceive(buf, offset, l);
    Inc(offset, l);
  until l <> MaxPacketLength;
  if Byte(buf[0]) = 255 then
    ReadError;
  //OFS(Format('-TMySqlNet.Receive: FPacketNumber=%d, FComprPacketNumber=%d', [FPacketNumber, FComprPacketNumber]));
end;

function TMySqlNet.ReceiveBuffer: TBytes;
var
  l: integer;
begin
  l := ReceiveHeader;
  Assert(l > 0, 'TMySqlNet.ReceiveBuffer l = ' + IntToStr(l));
  if l = MaxPacketLength then
    raise ArgumentException.Create('Cannot retrieve huge data in FetchAll mode');
{$IFDEF CLR}
  Borland.Delphi.System.SetLength(Result, l);
  RealReceive(Result, 0, l);
{$ELSE}
  System.SetLength(Result, l);
  RealReceive(@Result[0], 0, l);
{$ENDIF}
  if Result[0] = 255 then begin
    Position := 0;
    Length := l;
  {$IFDEF CLR}
    System.Buffer.BlockCopy(Result, 0, Buffer, 0, l);
  {$ELSE}
    Move(Result[0], Buffer^, l);
  {$ENDIF}
    ReadError;
  end;
end;

function TMySqlNet.SkipBuffer(out buf: TBytes; len: integer; firstPacket: boolean): integer;
// public int SkipBuffer(byte [] buf, int length, bool firstPacket) {
var
  count: integer;
  total,
  toReceive: integer;
  row0: byte;
begin
  count := ReceiveHeader;
  total := 0;
  row0 := 0;

  while count <> total do begin
    toReceive := Min(count - total, len);

    {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.SetLength(buf, toReceive);
    VioRead({$IFDEF CLR}buf{$ELSE}@buf[0]{$ENDIF}, 0, toReceive);

    if total = 0 then begin
      row0 := buf[0];
      if firstPacket and (buf[0] = 255) then begin
        Fbuffer.Position := 0;
        Fbuffer.SetLength(len);
      {$IFDEF CLR}
        System.Buffer.BlockCopy(buf, 0, Fbuffer.GetBuffer, 0, len);
      {$ELSE}
        Move(buf[0], Fbuffer.GetBuffer^, len);
      {$ENDIF}
        ReadError();
      end;
    end;
    Inc(total, toReceive);
  end;
  if firstPacket then
    buf[0] := row0
  else
    buf[0] := 0;
  Result := count;
end;

function TMySqlNet.GetBuffer: TValueArr;
begin
  Result := Fbuffer.GetBuffer;
end;

function TMySqlNet.GetLength: integer;
begin
  Result := Fbuffer.Length;
end;

procedure TMySqlNet.SetLength(Value: integer);
begin
  Fbuffer.SetLength(value);
end;

function TMySqlNet.GetPosition: integer;
begin
  Result := Fbuffer.Position;
end;

procedure TMySqlNet.SetPosition(Value: integer);
begin
  Fbuffer.Position := value;
end;

procedure TMySqlNet.Clear;
begin
  Position := NET_HEADER_SIZE;
  Length := NET_HEADER_SIZE;
end;

procedure TMySqlNet.NewCommand;
begin
  FPacketNumber := 0;
  FComprPacketNumber := 0;
  Clear;
end;

{$IFDEF CLR}
procedure TMySqlNet.WriteBytes(buffer: IntPtr; offset, count: integer);
var
  b: TBytes;
begin
  Borland.Delphi.System.SetLength(b, count - offset);
  Marshal.Copy(buffer, b, 0, count - offset);
  WriteBytes(b);
end;
{$ELSE}
procedure TMySqlNet.WriteBytes(const buffer: TBytes; offset, count: integer); // similar to net_write_command, my_net_write
begin
  WriteBytes(PAnsiChar(@buffer[0]), offset, count);
end;
{$ENDIF}

{$IFDEF CLR}
procedure TMySqlNet.WriteBytes(const buffer: TBytes; offset, count: integer);
{$ELSE}
procedure TMySqlNet.WriteBytes(buffer: PAnsiChar; offset, count: integer);
{$ENDIF}
begin
  Fbuffer.Write(buffer, offset, count);
end;

procedure TMySqlNet.WriteBytes(const buffer: TBytes);
begin
  WriteBytes(buffer, 0, {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Length(buffer));
end;

procedure TMySqlNet.WriteOrSendBytes(const buffer: TBytes; offset, count: integer); // similar to net_write_command, my_net_write
  procedure WriteBuff(offset, count, UncomprPacketLen: integer); // net_write_buff. Copy block from buffer to Self.FBuffer. Flush if need
  var
    left_length: integer;
  begin
    Assert(Length >= NET_HEADER_SIZE);
    Assert(Position = Length);
    if Length < BufferLength then begin
      left_length := Min(BufferLength - Length, count);
      Self.Fbuffer.Write(buffer, offset, left_length);
      count := count - left_length;
      offset := offset + left_length;
    end;

    if count > 0 then begin
      if Length >= NET_HEADER_SIZE then begin
        WriteHeader(UncomprPacketLen);
        RealSend({$IFDEF CLR}Self.Fbuffer.GetBuffer{$ELSE}{$IFDEF FPC}Self.Fbuffer.GetBuffer{$ELSE}@Self.Fbuffer.GetBuffer[0]{$ENDIF}{$ENDIF}, 0, Length); // Send 8192 bytes from buffer
        Clear;
      end;

      // Rest of buffer direct send
      RealSend({$IFDEF CLR}buffer{$ELSE}PAnsiChar(@buffer[0]){$ENDIF}, offset, count);
    end;
  end;

var
  Total, Delta: integer;
  len: integer;
begin
  Total := count + (Length - NET_HEADER_SIZE);
  if Total >= MaxPacketLength then begin
    Delta := (Length - NET_HEADER_SIZE);
    repeat
      len := MaxPacketLength - Delta;

      // Write data
      WriteBuff(offset, len, MaxPacketLength);
      offset := offset + len;
      Total := Total - len - Delta;
      Delta := 0;
    until Total < MaxPacketLength;
    WriteBuff(offset, Total, Total);
  end
  else
    WriteBuff(offset, count, Total);
end;

procedure TMySqlNet.WriteByte(value: byte);
begin
  Fbuffer.WriteByte(value);
end;

procedure TMySqlNet.WriteBool(value: boolean);
begin
  if value then
    Fbuffer.WriteByte(1)
  else
    Fbuffer.WriteByte(0);
end;

procedure TMySqlNet.WriteInt16(value: integer);
begin
  Fbuffer.WriteByte(byte(value));
  Fbuffer.WriteByte(byte(value shr 8));
end;

procedure TMySqlNet.WriteInt24(value: integer);
begin
  Fbuffer.WriteByte(byte(value));
  Fbuffer.WriteByte(byte(value shr 8));
  Fbuffer.WriteByte(byte(value shr 16));
end;

procedure TMySqlNet.WriteInt32(value: integer);
begin
  Fbuffer.WriteByte(byte(value));
  Fbuffer.WriteByte(byte(value shr 8));
  Fbuffer.WriteByte(byte(value shr 16));
  Fbuffer.WriteByte(byte(value shr 24));
end;

procedure TMySqlNet.WriteInt64(value: Int64);
var
  data: TBytes;
begin
  data := BitConverter.GetBytes(value);
  Fbuffer.Write(data, 0, 8);
end;

procedure TMySqlNet.WriteDouble(value: double);
var
  data: TBytes;
begin
  data := BitConverter.GetBytes(value);
  Fbuffer.Write(data, 0, 8);
end;

{$IFDEF CLR}
procedure TMySqlNet.WriteString(const value: AnsiString);
var
  buf: TBytes;
  l: integer;
begin
  l := Length + Borland.Delphi.System.Length(value) + 1;
  Length := l;
  buf := Buffer;
  if value <> '' then
    Encoding.Default.GetBytes(value, 0, Borland.Delphi.System.Length(value), buf, Position);
  buf[l - 1] := 0;
  Position := l;
end;
{$ELSE}
procedure TMySqlNet.WriteString(const value: AnsiString);
begin
  WriteString(PAnsiChar(value));
end;

procedure TMySqlNet.WriteString(value: PAnsiChar);
var
  Len: integer;
begin
  Len := StrLen(value);
  WriteBytes(value, 0, Len + 1);
end;
{$ENDIF}

procedure TMySqlNet.WriteFieldLength(length: integer);
begin
  if length < 251 then
    WriteByte(byte(length))
  else
  if length < 65536 then begin
    WriteByte(252);
    WriteInt16(length);
  end
  else
  if length < 16777216 then begin
    WriteByte(253);
    WriteInt24(length);
  end
  else
  begin
    WriteByte(254);
    WriteInt64(length);
  end;
end;

procedure TMySqlNet.Fill(pattern: byte; count: integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    WriteByte(pattern);
end;

procedure TMySqlNet.ReadBytes(var buffer: TBytes; offset, count: integer);
begin
  Self.Fbuffer.Read(buffer, offset, count)
end;

function TMySqlNet.ReadByte: byte;
begin
  Result := byte(Fbuffer.ReadByte);
end;

function TMySqlNet.ReadInt16: short;
begin
  Result := short((Fbuffer.ReadByte or Fbuffer.ReadByte shl 8));
end;

function TMySqlNet.ReadUInt16: integer;
begin
  Result := (Fbuffer.ReadByte or Fbuffer.ReadByte shl 8);
end;

function TMySqlNet.ReadInt24: integer;
begin
  Result := Fbuffer.ReadByte or Fbuffer.ReadByte shl 8 or Fbuffer.ReadByte shl 16;
end;

function TMySqlNet.ReadInt32: integer;
begin
  Result := Fbuffer.ReadByte or Fbuffer.ReadByte shl 8 or Fbuffer.ReadByte shl 16 or Fbuffer.ReadByte shl 24;
end;

function TMySqlNet.ReadInt64: Int64;
var
  p: integer;
begin
  p := Position + 8;
  if p > Length then
    raise ArgumentException.Create;
  Result := BitConverter.ToInt64(Buffer, Position); /// CR11635
  Position := p;
end;

function TMySqlNet.ReadDouble: double;
var
  p: integer;
begin
  p := Position + 8;
  if p > Length then
    raise ArgumentException.Create;
  Position := p;
  Result := BitConverter.ToDouble(Buffer, position);
end;

function TMySqlNet.ReadString(l: integer): AnsiString;
var
  p: integer;
begin
  p := Position;
  Position := Position + l;
{$IFDEF CLR}
  Result := Encoding.Default.GetString(Buffer, p, l);
{$ELSE}
  Result := Marshal.PtrToStringAnsi(Buffer + p, l);
{$ENDIF}
end;

function TMySqlNet.ReadString: AnsiString;
var
  buf: TValueArr;
  p: integer;
  l, i: integer;
begin
  buf := Buffer;
  p := Position;
  l := Length;
  for i := p to l - 1 do
    if Byte(buf[i]) = 0 then begin
      Position := i + 1;
    {$IFDEF CLR}
      Result := Encoding.Default.GetString(buf, p, i - p);
    {$ELSE}
      Result := Marshal.PtrToStringAnsi(buf + p, i - p);
    {$ENDIF}
      Exit;
    end;
  Position := l;
  if l = p then
    Result := ''
  else
  {$IFDEF CLR}
    Result := Encoding.Default.GetString(buf, p, l - p);
  {$ELSE}
    Result := Marshal.PtrToStringAnsi(buf + p, l - p);
  {$ENDIF}
end;

procedure TMySqlNet.ReadError;
var
  msg: AnsiString;
  code: integer;
begin
  if ReadByte = 255 then begin
    code := ReadInt16;
    msg := ReadString;
    raise EMySqlException.Create(code, string(msg));
  end;
end;

function TMySqlNet.ReadFieldLength: Int64;
var
  value: integer;
begin
  value := ReadByte;
  case value of
    251:
      Result := NullLength;
    252:
      Result := ReadUInt16();
    253:
      Result := ReadInt24();
    254:
      Result := ReadInt64();
    255:
      raise ArgumentException.Create;
    else
      Result := value;
  end;
end;

procedure TMySqlNet.SetVio(Value: TCRVio);
begin
  if fvio <> nil then begin
    fvio.Close;
    fvio.Free;
  end;
  fvio := Value;
end;

procedure TMySqlNet.VioRead({$IFDEF CLR}var{performance opt}{$ENDIF} buffer: TValueArr; offset, count: integer);
var
  readed: integer;
begin
  readed := Fvio.Read(buffer, offset, count);
  if readed <> count then
    raise EMySqlException.Create(CR_SERVER_LOST, Fvio.LastError);
end;

procedure TMySqlNet.VioWrite(const{performance opt} buffer: TValueArr; offset, count: integer);
var
  writed: integer;
begin
  writed := Fvio.Write(buffer, offset, count);
  if writed <> count then
    raise EMySqlException.Create(CR_SERVER_LOST, Fvio.LastError);
end;

{$IFDEF HAVE_COMPRESS_INTERFACE}
{$IFDEF LINUX}
var
  hZLib: pointer = nil;

function NotLink: integer;
begin
  raise Exception.Create('ZLib function is not linked');
end;

procedure AssignProc(var Proc: pointer; const Name: string);
begin
  Proc := dlsym(hZLib, PChar(Name));
  if Proc = nil then
    Proc := @NotLink;
end;

initialization
  hZLib := nil;
  hZLib := dlopen('libz.so', RTLD_LAZY);

  AssignProc(@compress, 'compress');
  AssignProc(@uncompress, 'uncompress');

finalization
  if hZLib <> nil then 
    dlclose(hZLib);
{$ENDIF}
{$ENDIF}

end.
