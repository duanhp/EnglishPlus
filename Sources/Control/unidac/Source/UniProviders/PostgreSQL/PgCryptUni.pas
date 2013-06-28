
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgCryptUni;

{$ENDIF}

{$A+,B-,E-,F-,G+,H+,I-,J+,K-,N+,P+,Q-,R-,S-,T-,V+,W-,X+,Y-} // from SB
interface

uses
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  Classes, SysUtils;

type
{$IFDEF CLR}
  TIntArray     = array of Longword;
{$ELSE}
  TIntegerArray = array[0..1023] of LongWord;
  TIntArray     = ^TIntegerArray;
{$ENDIF}

{$IFNDEF VER7P}
  UInt64 = Int64;
{$ENDIF}

  TMD5 = class
  private
    FCount: LongWord;
    FBuffer: array[0..15] of LongWord;
    FDigest: array[0..3] of LongWord;

    procedure Init;
    procedure Calc(const Data: TBytes; Offset: Integer; DataSize: Integer);
    procedure Done;
    procedure Transform({$IFDEF CLR}const{$ENDIF} Buffer: TIntArray);

  public
    function ComputeHash(const Data: TBytes): TBytes;
  end;

  TUnixCrypt = class
  private
    class function URShift(const Number, Bits: Integer): Integer;
    class procedure PermOp(const a, b, n, m: Integer; Results: TIntArray);
    class function HPermOp(const a, n, m: Integer): Integer;
    class function GetDesKeys(const Key: array of Shortint): TIntArray;
    class function Encrypt(const L, R, S, E0, E1: Integer; buf: TIntArray): Integer;
    class function Body(Schedule: TIntArray; Eswap0, Eswap1: Integer): TIntArray;

  public
    class function Crypt(const Salt, Original: AnsiString): AnsiString;
  end;


implementation

{$I UnixCrypt.inc}

{ TMD5 }

procedure TMD5.Init;
begin
{$IFNDEF CLR}
  FillChar(FBuffer, SizeOf(FBuffer), 0);
{$ELSE}
  &Array.Clear(FBuffer, 0, Length(FBuffer));
{$ENDIF}

  FDigest[0] := $67452301;
  FDigest[1] := $EFCDAB89;
  FDigest[2] := $98BADCFE;
  FDigest[3] := $10325476;
  FCount := 0;
end;

function TMD5.ComputeHash(const Data: TBytes): TBytes;
begin
  Init;
  Calc(Data, 0, Length(Data));
  Done;

  SetLength(Result, 16);
  Buffer.BlockCopy({$IFNDEF CLR}@{$ENDIF}FDigest, 0, Result, 0, 16);
end;

procedure TMD5.Calc(const Data: TBytes; Offset: Integer; DataSize: Integer);
var
  Index: Integer;
  Buf: TIntArray;
{$IFDEF CLR}
  Pos: Integer;
{$ENDIF}
begin
  if DataSize <= 0 then Exit;

  Index := FCount and $3F;
  Inc(FCount, DataSize);
  if Index > 0 then
  begin
    if DataSize < 64 - Index then
    begin
      Buffer.BlockCopy(Data, Offset, {$IFNDEF CLR}@{$ENDIF}FBuffer, Index, DataSize);
      Exit;
    end;
    Buffer.BlockCopy(Data, Offset, {$IFNDEF CLR}@{$ENDIF}FBuffer, Index, 64 - Index);
    Transform({$IFNDEF CLR}@{$ENDIF}FBuffer);
    Index := 64 - Index;
    Dec(DataSize, Index);
  end;

{$IFDEF CLR}
  SetLength(Buf, 16);
  Pos := Index + Offset;
{$ELSE}
  Buf := @Data[Index + Offset];
{$ENDIF}

  Inc(Index, DataSize and not $3F);
  while DataSize >= 64 do
  begin
  {$IFDEF CLR}
    Buffer.BlockCopy(Data, Pos, Buf, 0, 64);
  {$ENDIF}

    Transform(Buf);
    Inc({$IFDEF CLR}Pos{$ELSE}PAnsiChar(Buf){$ENDIF}, 64);
    Dec(DataSize, 64);
  end;
  Buffer.BlockCopy(Data, Index + Offset, {$IFNDEF CLR}@{$ENDIF}FBuffer, 0, DataSize);
end;

procedure TMD5.Done;
const
  NullArray: array[0..15] of LongWord = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
var
  I: Integer;
  S: Int64;
begin
  I := FCount and $3F;
  Buffer.SetByte({$IFNDEF CLR}@{$ENDIF}FBuffer, I, $80);
  Inc(I);
  if I > 64 - 8 then
  begin
    Buffer.BlockCopy({$IFNDEF CLR}@{$ENDIF}NullArray, 0, {$IFNDEF CLR}@{$ENDIF}FBuffer, I, 64 - I); // Fill 0
    Transform({$IFNDEF CLR}@{$ENDIF}FBuffer);
    I := 0;
  end;
  Buffer.BlockCopy({$IFNDEF CLR}@{$ENDIF}NullArray, 0, {$IFNDEF CLR}@{$ENDIF}FBuffer, I, 64 - I);
  S := UInt64(FCount) shl 3;
  FBuffer[16 - 2] := Longword(S);
  FBuffer[16 - 1] := Longword(S shr 32);

  Transform({$IFNDEF CLR}@{$ENDIF}FBuffer);

{$IFNDEF CLR}
  FillChar(FBuffer, SizeOf(FBuffer), 0);
{$ELSE}
  &Array.Clear(FBuffer, 0, Length(FBuffer));
{$ENDIF}
end;

procedure TMD5.Transform({$IFDEF CLR}const{$ENDIF} Buffer: TIntArray);
var
  A, B, C, D: LongWord;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, Buffer[ 0] + $D76AA478 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 1] + $E8C7B756 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 2] + $242070DB + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 3] + $C1BDCEEE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 4] + $F57C0FAF + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 5] + $4787C62A + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 6] + $A8304613 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 7] + $FD469501 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 8] + $698098D8 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 9] + $8B44F7AF + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[10] + $FFFF5BB1 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[11] + $895CD7BE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[12] + $6B901122 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[13] + $FD987193 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[14] + $A679438E + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[15] + $49B40821 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;

  Inc(A, Buffer[ 1] + $F61E2562 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 6] + $C040B340 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[11] + $265E5A51 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 0] + $E9B6C7AA + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 5] + $D62F105D + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[10] + $02441453 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[15] + $D8A1E681 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 4] + $E7D3FBC8 + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 9] + $21E1CDE6 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[14] + $C33707D6 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 3] + $F4D50D87 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 8] + $455A14ED + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[13] + $A9E3E905 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 2] + $FCEFA3F8 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 7] + $676F02D9 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[12] + $8D2A4C8A + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;

  Inc(A, Buffer[ 5] + $FFFA3942 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 8] + $8771F681 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[11] + $6D9D6122 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[14] + $FDE5380C + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 1] + $A4BEEA44 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 4] + $4BDECFA9 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 7] + $F6BB4B60 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[10] + $BEBFBC70 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[13] + $289B7EC6 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 0] + $EAA127FA + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 3] + $D4EF3085 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 6] + $04881D05 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 9] + $D9D4D039 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[12] + $E6DB99E5 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[15] + $1FA27CF8 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 2] + $C4AC5665 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;

  Inc(A, Buffer[ 0] + $F4292244 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 7] + $432AFF97 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[14] + $AB9423A7 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 5] + $FC93A039 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[12] + $655B59C3 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 3] + $8F0CCC92 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[10] + $FFEFF47D + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 1] + $85845DD1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 8] + $6FA87E4F + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[15] + $FE2CE6E0 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 6] + $A3014314 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[13] + $4E0811A1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 4] + $F7537E82 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[11] + $BD3AF235 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 2] + $2AD7D2BB + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 9] + $EB86D391 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;

{ TUnixCrypt }

const
  Iterations = 16;

class function TUnixCrypt.URShift(const Number, Bits: Integer): Integer;
begin
  if Number >= 0 then
    Result := Number shr Bits
  else
    Result := (Int64(Number) shr Bits) + (2 shl not Bits);
end;

class procedure TUnixCrypt.PermOp(const a, b, n, m: Integer; Results: TIntArray);
var
  t: Integer;
begin
  t := (URShift(a, n) xor b) and m;
  Results[0] := a xor (t shl n);
  Results[1] := b xor t;
end;

class function TUnixCrypt.HPermOp(const a, n, m: Integer): Integer;
var
  t: Integer;
begin
  t := ((a shl (16 - n)) xor a) and m;
  Result := a xor t xor URShift(t, (16 - n));
end;

class function TUnixCrypt.GetDesKeys(const Key: array of Shortint): TIntArray;

  function ByteToUnsigned(b: Shortint): Integer;
  begin
    Result := b;
    if Result < 0 then
      Result := Result + 256;
  end;

  function FourBytesToInt(const b: array of Shortint; Offset: Integer): Integer;
  begin
    Result := ByteToUnsigned(b[Offset]);
    Inc(Offset);
    Result := Result or (ByteToUnsigned(b[Offset]) shl 8);
    Inc(Offset);
    Result := Result or (ByteToUnsigned(b[Offset]) shl 16);
    Inc(Offset);
    Result := Result or (ByteToUnsigned(b[Offset]) shl 24);
  end;

var
  Results: TIntArray;
  c, d: Integer;
  s, t: Integer;
  i, j: Integer;
begin
{$IFDEF CLR}
  SetLength(Result, Iterations * 2);
  SetLength(Results, 2);
{$ELSE}
  GetMem(Result, Iterations * 2 * SizeOf(Longword));
  GetMem(Results, 2 * SizeOf(Longword));
{$ENDIF}

  c := FourBytesToInt(Key, 0);
  d := FourBytesToInt(Key, 4);

  PermOp(d, c, 4, $0f0f0f0f, Results);
  d := Results[0];
  c := Results[1];

  c := HPermOp(c, -2, Integer($cccc0000));
  d := HPermOp(d, -2, Integer($cccc0000));

  PermOp(d, c, 1, $55555555, Results);
  d := Results[0];
  c := Results[1];

  PermOp(c, d, 8, $00ff00ff, Results);
  c := Results[0];
  d := Results[1];

  PermOp(d, c, 1, $55555555, Results);
  d := Results[0];
  c := Results[1];

  d := ((d and $000000ff) shl 16) or (d and $0000ff00) or URShift(d and $00ff0000, 16) or URShift(c and $f0000000, 4);
  c := c and $0fffffff;

  j := 0;
  for i := 0 to Iterations - 1 do begin
    if Shifts2[i] then begin
      c := URShift(c, 2) or (c shl 26);
      d := URShift(d, 2) or (d shl 26);
    end
    else begin
      c := URShift(c, 1) or (c shl 27);
      d := URShift(d, 1) or (d shl 27);
    end;

    c := c and $0fffffff;
    d := d and $0fffffff;

    s := Skb[0][(c) and $3f] or Skb[1][(URShift(c, 6) and $03) or (URShift(c, 7) and $3c)] or Skb[2][(URShift(c, 13) and $0f) or (URShift(c, 14) and $30)] or Skb[3][(URShift(c, 20) and $01) or (URShift(c, 21) and $06) or (URShift(c, 22) and $38)];
    t := Skb[4][(d) and $3f] or Skb[5][(URShift(d, 7) and $03) or (URShift(d, 8) and $3c)] or Skb[6][URShift(d, 15) and $3f] or Skb[7][(URShift(d, 21) and $0f) or (URShift(d, 22) and $30)];

    Result[j] := ((t shl 16) or (s and $0000ffff)) and $ffffffff;
    Inc(j);
    s := URShift(s, 16) or (t and Integer($ffff0000));

    s := (s shl 4) or URShift(s, 28);
    Result[j] := s and $ffffffff;
    Inc(j);
  end;
end;

class function TUnixCrypt.Encrypt(const L, R, S, E0, E1: Integer; buf: TIntArray): Integer;
var
  t, u, v: Integer;
begin
  v := R xor URShift(R, 16);
  u := v and E0;
  v := v and E1;
  u := (u xor (u shl 16)) xor R xor Integer(buf[S]);
  t := (v xor (v shl 16)) xor R xor Integer(buf[S + 1]);
  t := URShift(t, 4) or (t shl 28);

  Result := L xor (Integer(SPtrans[1][t and $3f] or SPtrans[3][URShift(t, 8) and $3f] or SPtrans[5][URShift(t, 16) and $3f] or SPtrans[7][URShift(t, 24) and $3f] or SPtrans[0][u and $3f] or SPtrans[2][URShift(u, 8) and $3f] or SPtrans[4][URShift(u, 16) and $3f] or SPtrans[6][URShift(u, 24) and $3f]));
end;

class function TUnixCrypt.Body(Schedule: TIntArray; Eswap0, Eswap1: Integer): TIntArray;
var
  Left, Right, t: Integer;
  i, j: Integer;
begin
  Left := 0;
  Right := 0;

  for j := 0 to 24 do begin
    i := 0;
    while i < Iterations * 2 do begin
      Left := Encrypt(Left, Right, i, Eswap0, Eswap1, Schedule);
      Right := Encrypt(Right, Left, i + 2, Eswap0, Eswap1, Schedule);
      i := i + 4;
    end;

    t := Left;
    Left := Right;
    Right := t;
  end;

  t := Right;
  Right := URShift(Left, 1) or (Left shl 31);
  Left := URShift(t, 1) or (t shl 31);

  Left := Left and $ffffffff;
  Right := Right and $ffffffff;

{$IFDEF CLR}
  SetLength(Result, 2);
{$ELSE}
  GetMem(Result, 2 * SizeOf(Longword));
{$ENDIF}
  PermOp(Right, Left, 1, $55555555, Result);
  Right := Result[0];
  Left := Result[1];

  PermOp(Left, Right, 8, $00ff00ff, Result);
  Left := Result[0];
  Right := Result[1];

  PermOp(Right, Left, 2, $33333333, Result);
  Right := Result[0];
  Left := Result[1];

  PermOp(Left, Right, 16, $0000ffff, Result);
  Left := Result[0];
  Right := Result[1];

  PermOp(Right, Left, 4, $0f0f0f0f, Result);
  Right := Result[0];
  Left := Result[1];

  Result[0] := Left;
  Result[1] := Right;
end;

class function TUnixCrypt.Crypt(const Salt, Original: AnsiString): AnsiString;

  procedure IntToFourBytes(Value: Integer; var buf: array of Shortint; Offset: Integer);
  begin
    buf[Offset] := Shortint(Value and $ff);
    Inc(Offset);
    buf[Offset] := Shortint(URShift(Value, 8) and $ff);
    Inc(Offset);
    buf[Offset] := Shortint(URShift(Value, 16) and $ff);
    Inc(Offset);
    buf[Offset] := Shortint(URShift(Value, 24) and $ff);
  end;

var
  _Salt: AnsiString;
  CharZero, CharOne: AnsiChar;
  eSwap0, eSwap1: Integer;
  Key, b: array of Shortint;
  LenOriginal: Integer;
  iChar: Integer;
  Schedule, OutRenamed: TIntArray;
  y, u, c: Integer;
  i, j: Integer;
begin
  _Salt := Salt;
  while Length(_Salt) < 2 do
    _Salt := _Salt + 'A';

  SetLength(Result, 13);

  CharZero := _Salt[1];
  CharOne := _Salt[2];

  Result[1] := CharZero;
  Result[2] := CharOne;

  eSwap0 := ConSalt[Integer(CharZero)];
  eSwap1 := ConSalt[Integer(CharOne)] shl 4;

  SetLength(Key, 8);
  LenOriginal := Length(Original);
  for i := 0 to 7 do begin
    if i < LenOriginal then begin
      iChar := Integer(Original[i + 1]);
      Key[i] := Shortint(iChar shl 1);
    end
    else
      Key[i] := 0;
  end;

  Schedule := GetDesKeys(Key);
  OutRenamed := Body(Schedule, eSwap0, eSwap1);

  SetLength(b, 9);
  IntToFourBytes(OutRenamed[0], b, 0);
  IntToFourBytes(OutRenamed[1], b, 4);
  b[8] := 0;

  y := 0;
  u := $80;
  for i := 3 to 13 do begin
    c := 0;
    for j := 0 to 5 do begin
      c := c shl 1;

      if (Integer(b[y]) and u) <> 0 then
        c := c or 1;

      u := URShift(u, 1);

      if u = 0 then begin
        Inc(y);
        u := $80;
      end;

      Result[i] := AnsiChar(CovToChar[c]);
    end;
  end;
end;

end.

