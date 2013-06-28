
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgSQLNetUni;

{$ENDIF}

interface

uses
  Classes, Math, SysUtils,
{$IFDEF CLR}
  System.IO, System.Runtime.InteropServices, System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
  MemUtils, CRVio;

const
  OffsetStackDelta = 25;
  ReadBufferSize = 1024 * 64;
  WriteBufferSize = 1024 * 8;

type
  TPgSQLNet = class
  private
    FVio: TCRVio;
    FReadBuffer: MemoryStream;
    FWriteBuffer: MemoryStream;
    FReadBufLen: integer;

    FOffsetStack: array of integer;
    FOffsetStackPos: integer;

    procedure SetVio(const Value: TCRVio);
    procedure PushOffset(const Offset: integer);
    procedure PopOffset(var Offset: integer);

    procedure UpdateReadBuffer;
    procedure CheckReadBuffer;
    function ReadStringToStream(Count: integer): MemoryStream;
    function VioRead({$IFDEF CLR}var{$ENDIF} Buffer: TValueArr; Offset, Count: integer): integer;
    procedure VioWrite(const Buffer: TValueArr; Offset, Count: integer);

  public
    constructor Create;
    destructor Destroy; override;

    function FlushSend: Boolean;

    procedure WriteBytes({$IFDEF CLR}const{$ENDIF} Buffer: TValueArr; Offset, Count: integer); overload;
  {$IFDEF CLR}
    procedure WriteBytes(Buffer: IntPtr; Offset, Count: integer); overload;
  {$ENDIF}
    procedure WriteByte(Value: Byte);
    procedure WriteWord(Value: Word);
    procedure WriteInt16(Value: Smallint);
    procedure WriteInt32(Value: integer); overload;
    procedure WriteInt32(Value: integer; Offset: integer); overload;
    procedure WriteInt64(Value: Int64);
    procedure WriteDouble(Value: Double);
    procedure WriteSingle(Value: Single);
    procedure WriteChar(Value: AnsiChar);
    procedure WriteString(const Value: AnsiString); overload;
    procedure WriteString(const Value: AnsiString; Len: integer); overload;

    procedure ReadBytes({$IFDEF CLR}var{$ENDIF} Buffer: TValueArr; Offset, Count: integer); overload;
  {$IFDEF CLR}
    procedure ReadBytes(Buffer: IntPtr; Offset, Count: integer); overload;
  {$ENDIF}
    function ReadByte: Byte;
    function ReadWord: word;
    function ReadInt16: Smallint;
    function ReadInt32: integer;
    function ReadInt64: Int64;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadChar: AnsiChar;
    function ReadString: AnsiString; overload;
    function ReadString(Count: integer): AnsiString; overload;
    procedure ReadString(Buffer: IntPtr; Count: integer; AddNull: boolean = True); overload;
    function ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer;

    procedure EnterSizeBlock;
    procedure LeaveSizeBlock(AllowSizeRoom: boolean = False);
    procedure ClearWriteBuffer;

    function GetWriteBufferSize: integer;
    function GetReadBufferAvailable: integer;

    property Vio: TCRVio read FVio write SetVio;
  end;

implementation

uses
  {$IFNDEF UNIDACPRO}PgError{$ELSE}PgErrorUni{$ENDIF};

{ TPgSQLNet }

constructor TPgSQLNet.Create;
begin
  inherited;

  FReadBuffer := MemoryStream.Create(ReadBufferSize);
  FReadBuffer.SetLength(ReadBufferSize);
  FWriteBuffer := MemoryStream.Create(WriteBufferSize);
end;

destructor TPgSQLNet.Destroy;
begin
  SetVio(nil);
  FReadBuffer.Free;
  FWriteBuffer.Free;

  inherited;
end;

procedure TPgSQLNet.SetVio(const Value: TCRVio);
begin
  if FVio <> nil then begin
    FVio.Close;
    FVio.Free;
  end;
  FVio := Value;
end;

function TPgSQLNet.VioRead({$IFDEF CLR}var{$ENDIF} Buffer: TValueArr; Offset, Count: integer): integer;
begin
  Result := FVio.ReadNoWait(Buffer, Offset, Count);
  if Result = 0 then
    raise EPgError.Create(sFatal, FVio.LastError);
end;

procedure TPgSQLNet.VioWrite(const Buffer: TValueArr; Offset, Count: integer);
var
  Res: integer;
begin
  Res := FVio.Write(Buffer, Offset, Count);
  if Res <> Count then
    raise EPgError.Create(sFatal, FVio.LastError);
end;

procedure TPgSQLNet.PushOffset(const Offset: integer);
begin
  Inc(FOffsetStackPos);
  if FOffsetStackPos >= Length(FOffsetStack) then
    SetLength(FOffsetStack, Length(FOffsetStack) + OffsetStackDelta);
  FOffsetStack[FOffsetStackPos] := Offset;
end;

procedure TPgSQLNet.PopOffset(var Offset: integer);
begin
  Offset := FOffsetStack[FOffsetStackPos];
  Dec(FOffsetStackPos);
end;

procedure TPgSQLNet.UpdateReadBuffer;
var
  Buf: TValueArr;
begin
  FReadBuffer.Position := 0;
  Buf := FReadBuffer.GetBuffer;
  FReadBufLen := VioRead(Buf, 0, ReadBufferSize);
end;

procedure TPgSQLNet.CheckReadBuffer;
begin
  if (FReadBufLen = 0) or (FReadBuffer.Position >= FReadBufLen) then
    UpdateReadBuffer;
end;

function TPgSQLNet.FlushSend: Boolean;
begin
  Result := False;
  if FWriteBuffer.Length > 0 then begin
    VioWrite(FWriteBuffer.GetBuffer, 0, FWriteBuffer.Length);
    FWriteBuffer.SetLength(0);
    FWriteBuffer.Position := 0;
    Result := True;
  end;
end;

procedure TPgSQLNet.WriteBytes({$IFDEF CLR}const{$ENDIF} Buffer: TValueArr; Offset, Count: integer);
begin
  FWriteBuffer.Write(Buffer, Offset, Count);
end;

{$IFDEF CLR}
procedure TPgSQLNet.WriteBytes(Buffer: IntPtr; Offset, Count: integer);
var
  Bytes: TBytes;
begin
  Marshal.Copy(Buffer, Bytes, 0, Count);
  FWriteBuffer.Write(Bytes, Offset, Count);
end;
{$ENDIF}

procedure TPgSQLNet.WriteByte(Value: Byte);
begin
  FWriteBuffer.WriteByte(Value);
end;

procedure TPgSQLNet.WriteWord(Value: Word);
begin
  WriteByte(Byte(Value shr 8));
  WriteByte(Byte(Value));
end;

procedure TPgSQLNet.WriteInt16(Value: Smallint);
begin
  WriteByte(Byte(Value shr 8));
  WriteByte(Byte(Value));
end;

procedure TPgSQLNet.WriteInt32(Value: integer);
begin
  WriteByte(Byte(Value shr 24));
  WriteByte(Byte(Value shr 16));
  WriteByte(Byte(Value shr 8));
  WriteByte(Byte(Value));
end;

procedure TPgSQLNet.WriteInt32(Value: integer; Offset: integer);
var
  OldPos: integer;
begin
  Oldpos := FWriteBuffer.Position;
  try
    FWriteBuffer.Position := Offset;
    WriteInt32(Value);
  finally
    FWriteBuffer.Position := OldPos;
  end;
end;

procedure TPgSQLNet.WriteInt64(Value: Int64);
var
  i: integer;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
{$IFDEF CLR}
  b := BitConverter.GetBytes(Value);
{$ENDIF}
  for i := 0 to 7 do
  {$IFNDEF CLR}
    WriteByte(PByte(PtrOffset(@Value, 7 - i))^);
  {$ELSE}
    WriteByte(b[7 - i]);
  {$ENDIF}
end;

procedure TPgSQLNet.WriteDouble(Value: Double);
var
  i: integer;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
{$IFDEF CLR}
  b := BitConverter.GetBytes(Value);
{$ENDIF}
  for i := 0 to 7 do
  {$IFNDEF CLR}
    WriteByte(PByte(PtrOffset(@Value, 7 - i))^);
  {$ELSE}
    WriteByte(b[7 - i]);
  {$ENDIF}
end;

procedure TPgSQLNet.WriteSingle(Value: Single);
var
  i: integer;
begin
{$IFNDEF CLR}
  i := PInteger(@Value)^;
{$ELSE}
  i := BitConverter.ToInt32(BitConverter.GetBytes(Value), 0);
{$ENDIF}
  WriteInt32(i);
end;

procedure TPgSQLNet.WriteChar(Value: AnsiChar);
begin
  WriteByte(Byte(Value));
end;

procedure TPgSQLNet.WriteString(const Value: AnsiString);
begin
  WriteBytes(TValueArr(Value), 0, Length(Value));
  WriteByte(0);
end;

procedure TPgSQLNet.WriteString(const Value: AnsiString; Len: integer);
var
  TmpBuf: TBytes;
  TmpBufLen: integer;
begin
  WriteString(Copy(Value, 1, Len));

  TmpBufLen := Len - Length(Value) - 1;
  if TmpBufLen > 0 then begin
    SetLength(TmpBuf, TmpBufLen);
    WriteBytes(TValueArr(TmpBuf), 0, TmpBufLen);
  end;
end;

procedure TPgSQLNet.ReadBytes({$IFDEF CLR}var{$ENDIF} Buffer: TValueArr; Offset, Count: integer);
var
  ReadCount: integer;
  PieceLen: integer;
begin
  ReadCount := 0;
  while ReadCount < Count do begin
    CheckReadBuffer;

    PieceLen := Min(Count - ReadCount, FReadBufLen - FReadBuffer.Position);
    FReadBuffer.Read(Buffer, Offset + ReadCount, PieceLen);
    ReadCount := ReadCount + PieceLen;
  end;
end;

{$IFDEF CLR}
procedure TPgSQLNet.ReadBytes(Buffer: IntPtr; Offset, Count: integer);
var
  Buf: TValueArr;
begin
  SetLength(Buf, Count);
  ReadBytes(Buf, Offset, Count);
  Marshal.Copy(Buf, 0, Buffer, Count);
end;
{$ENDIF}

function TPgSQLNet.ReadByte: Byte;
begin
  CheckReadBuffer;
  Result := FReadBuffer.ReadByte;
end;

function TPgSQLNet.ReadWord: word;
begin
  Result := ReadByte shl 8 or ReadByte;
end;

function TPgSQLNet.ReadInt16: Smallint;
begin
  Result := Smallint(ReadByte shl 8 or ReadByte);
end;

function TPgSQLNet.ReadInt32: integer;
begin
  Result := ReadByte shl 24 or ReadByte shl 16 or ReadByte shl 8 or ReadByte;
end;

function TPgSQLNet.ReadInt64: Int64;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 8);
  ReadBytes(TValueArr(Buf), 0, 8);

  for i := 0 to 3 do begin
     Buf[i] := Buf[i] xor Buf[7 - i];
     Buf[7 - i] := Buf[i] xor Buf[7 - i];
     Buf[i] := Buf[i] xor Buf[7 - i];
  end;  
  
  Result := BitConverter.ToInt64(Buf, 0);
end;

function TPgSQLNet.ReadSingle: Single;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 4);
  ReadBytes(TValueArr(Buf), 0, 4);

  for i := 0 to 1 do begin
     Buf[i] := Buf[i] xor Buf[3 - i];
     Buf[3 - i] := Buf[i] xor Buf[3 - i];
     Buf[i] := Buf[i] xor Buf[3 - i];
  end;

  Result := BitConverter.ToSingle(Buf, 0);
end;

function TPgSQLNet.ReadDouble: Double;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 8);
  ReadBytes(TValueArr(Buf), 0, 8);

  for i := 0 to 3 do begin
     Buf[i] := Buf[i] xor Buf[7 - i];
     Buf[7 - i] := Buf[i] xor Buf[7 - i];
     Buf[i] := Buf[i] xor Buf[7 - i];
  end;

  Result := BitConverter.ToDouble(Buf, 0);
end;

function TPgSQLNet.ReadChar: AnsiChar;
begin
  Result := AnsiChar(ReadByte);
end;

function TPgSQLNet.ReadStringToStream(Count: integer): MemoryStream;
var
  i, p, Len: integer;
  Buf: TValueArr;

  procedure CopyToLocalBuffer;
  begin
    if Result = nil then
      if Count < 0 then
        Result := MemoryStream.Create(512)
      else
        Result := MemoryStream.Create(Count);

    Result.Write(Buf, p, i - p);
  end;

begin
  Len := 0;
  Result := nil;

  CheckReadBuffer;

  p := FReadBuffer.Position;
  i := p;
  Buf := FReadBuffer.GetBuffer;

  while ((Count < 0) or (Len < Count)) and (Byte(Buf[i]) <> 0) do begin
    Inc(i);
    Inc(Len);

    if i >= FReadBufLen then begin
      CopyToLocalBuffer;
      UpdateReadBuffer;
      p := 0;
      i := 0;
    end;
  end;

  if i > p then
    CopyToLocalBuffer;

  if Count < 0 then
    FReadBuffer.Position := i + 1
  else
    FReadBuffer.Position := i;
end;

function TPgSQLNet.ReadString(Count: integer): AnsiString;
var
  Len: integer;
  Stream: MemoryStream;
  Buf: TValueArr;
begin
  Stream := ReadStringToStream(Count);
  try
    if Stream <> nil then begin
      Buf := Stream.GetBuffer;
      Len := Stream.Length;

    {$IFDEF CLR}
      Result := Encoding.Default.GetString(Buf, 0, Len);
    {$ELSE}
      Result := Marshal.PtrToStringAnsi(Buf, Len);
    {$ENDIF}
    end
    else
      Result := '';
  finally
    Stream.Free;
  end;
end;

function TPgSQLNet.ReadString: AnsiString;
begin
  Result := ReadString(-1);
end;

procedure TPgSQLNet.ReadString(Buffer: IntPtr; Count: integer; AddNull: boolean = True);
begin
  ReadBytes(Buffer, 0, Count);
  if AddNull then
    Marshal.WriteByte(Buffer, Count, 0);
end;

function TPgSQLNet.ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer;
var
  Stream: MemoryStream;
  Buf: TValueArr;
  Size: integer;
  s: WideString;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  Stream := ReadStringToStream(Count);
  try
    if Stream <> nil then begin
      Buf := Stream.GetBuffer;
      Size := Stream.Length;

      s := Encoding.UTF8.{$IFDEF CLR}GetString{$ELSE}GetWideString{$ENDIF}
        (TBytes(Buf), 0, Size);
    end
    else
      s := '';
  finally
    Stream.Free;
  end;

  Result := Length(s) * 2;
{$IFDEF CLR}
  b := Encoding.Unicode.GetBytes(s);
  Marshal.Copy(b, 0, Buffer, Result);
{$ELSE}
  Move(s[1], Buffer^, Result);
{$ENDIF}
  if AddNull then
    Marshal.WriteInt16(Buffer, Result, 0);
end;

procedure TPgSQLNet.EnterSizeBlock;
begin
  PushOffset(FWriteBuffer.Position);
  WriteInt32(0);
end;

procedure TPgSQLNet.LeaveSizeBlock(AllowSizeRoom: boolean = False);
var
  Offset: integer;
begin
  PopOffset(Offset);
  if AllowSizeRoom then
    WriteInt32(FWriteBuffer.Position - Offset, Offset)
  else
    WriteInt32(FWriteBuffer.Position - Offset - 4, Offset)
end;

procedure TPgSQLNet.ClearWriteBuffer;
begin
  FWriteBuffer.SetLength(0);
  FWriteBuffer.Position := 0;
end;

function TPgSQLNet.GetWriteBufferSize: integer;
begin
  Result := FWriteBuffer.Length;
end;

function TPgSQLNet.GetReadBufferAvailable: integer;
begin
  Result := FReadBufLen - FReadBuffer.Position; 
end;

end.
