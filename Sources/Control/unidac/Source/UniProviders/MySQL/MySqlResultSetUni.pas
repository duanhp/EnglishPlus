
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlResultSetUni;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.Collections,
{$ELSE}
  CLRClasses,
{$ENDIF}
  SysUtils, MemUtils,
{$IFNDEF UNIDACPRO}
  MyCall, MySqlBind, MySqlSession;
{$ELSE}
  MyCallUni, MySqlBindUni, MySqlSessionUni;
{$ENDIF}

type
{$IFDEF CLR}
  TRows = ArrayList;
{$ELSE}
  TRows = class
  protected
    FList: array of TBytes;

    function Get(Index: Integer): TBytes;
    function GetCount: integer;
  public
    destructor Destroy; override;

    procedure Add(const Row: TBytes);
    procedure Clear;

    property Rows[Index: Integer]: TBytes read Get; default;
    property Count: integer read GetCount;
  end;
{$ENDIF}

  TMySqlResultSet = class
  protected
    frows: TRows;
    frowIndex: int;
    frowCount: int;
    ffieldCount: int;
    frowBuffer: TValueArr;
    fresultBind: TMySqlBinds;
    ffields: TMySqlFieldInfos;
    fsession: TMySqlSession;
    feof: bool;

    FBind: TMySqlBinds;

    FGCHandleData: IntPtr; // frowBuffer

    function ReadRow: bool;
    function GetRowCount: int;

    procedure ClearGCHandleData;
    function GetGCHandleData: IntPtr;

    procedure SetresultBind(Value: TMySqlBinds);

  public
    constructor Create(session: TMySqlSession); overload;
    constructor Create(session: TMySqlSession; fields: TMySqlFieldInfos; fieldCount: int; resultBind: TMySqlBinds); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure ReadRows(readStatus: bool);

    function Read(var resultBind: TMySqlBinds): bool;
    procedure Seek(offset: int);

    function ReadFieldsInfo(fieldCount: int): TMySqlFieldInfos;
    property GCHandleData: IntPtr read GetGCHandleData;

    property Session: TMySqlSession read fsession write fsession;
    property RowCount: int read GetRowCount;
    property FieldCount: int read ffieldCount;
    property Fields: TMySqlFieldInfos read ffields;
    property Eof: bool read feof;
    property ResultBind: TMySqlBinds read fresultBind write SetresultBind;

    property Bind: TMySqlBinds read FBind write FBind;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  MySqlNet, MySqlErrors;
{$ELSE}
  MySqlNetUni, MySqlErrorsUni;
{$ENDIF}

constructor TMySqlResultSet.Create(session: TMySqlSession);
begin
  Create(session, session.fields, session.fieldCount, nil);
end;

constructor TMySqlResultSet.Create(session: TMySqlSession; fields: TMySqlFieldInfos; fieldCount: int; resultBind: TMySqlBinds);
begin
  inherited Create;
  Assert(session <> nil);
  Self.fsession := session;
  Self.ffields := fields;
  Self.ffieldCount := fieldCount;
  Self.resultBind := resultBind;

  FBind := TMySqlBinds.Create(fieldCount);
end;

destructor TMySqlResultSet.Destroy;
begin
  frows.Free;
  FBind.Free;
  FResultBind.Free;
  inherited;
end;

procedure TMySqlResultSet.ClearGCHandleData;
begin
  if FGCHandleData = nil then
    Exit;

  FreeGCHandle(FGCHandleData);
  FGCHandleData := nil;
end;

function TMySqlResultSet.GetGCHandleData: IntPtr;
begin
  Assert(frowBuffer <> nil);
  if FGCHandleData = nil then 
    FGCHandleData := AllocGCHandle(frowBuffer, True);
  Result := FGCHandleData;
end;

procedure TMySqlResultSet.SetresultBind(Value: TMySqlBinds);
begin
  fresultBind.Free;
  fresultBind := Value;
end;

procedure TMySqlResultSet.Clear;
begin
  ClearGCHandleData;

  frowBuffer := nil;
  resultBind := nil;
  ffields := nil;
  if frows <> nil then
    frows.Clear
  else
    if fsession <> nil then
      fsession.FreeQuery;

  frows.Free;
  frows := nil;
  fsession := nil;

  FBind.Free;
  FBind := nil;
end;

procedure TMySqlResultSet.ReadRows(readStatus: bool);
var
  net: TMySqlNet;
  row: TBytes;
begin
  net := session.net;

  frows.Free;
  frows := TRows.Create;

  frowIndex := 0;
  row := net.ReceiveBuffer;
  while (Length(row) >= 8) or (row[0] <> 254) do begin
    frows.Add(row);
    row := net.ReceiveBuffer;
  end;
  feof := (frows.Count = 0);
  if readStatus then begin
    (* TODO:
    row = net.TryReceiveBuffer();
    while (row != null)
      row = net.TryReceiveBuffer();
    if (row != null && row.Length > 1)
      session.warningCount = BitConverter.ToInt16(row, 1);
    *)
  end;
  if Length(row) > 1 then begin // MySQL 4.1 protocol
    Assert(Length(row) >= 5);
    session.warningCount := row[1] + (row[2] shl 8);
    session.serverStatus := row[3] + (row[4] shl 8);
  end;
end;

function TMySqlResultSet.ReadRow: bool; // client.c: read_one_row
var
  net: TMySqlNet;
  row: TValueArr;
  OldPosition: integer;
begin
  ClearGCHandleData;

  net := session.net;
  net.Receive;
  row := net.Buffer;
  if (net.Length > 8) or (Byte(row[0]) <> 254) then begin
    frowBuffer := row;
    Inc(frowCount);
    Result := true;
  end
  else
  begin
    if net.Length > 1 then begin // MySQL 4.1 protocol
      OldPosition := net.Position;
      try
        // mysql->warning_count= uint2korr(net->read_pos+1);
        net.Position := OldPosition + 1;
        session.warningCount := net.ReadUInt16;

        // mysql->server_status= uint2korr(net->read_pos+3);
        net.Position := OldPosition + 3; /// d5 bug
        session.serverStatus := net.ReadUInt16;
      finally
        net.Position := OldPosition;
      end;
    end;
    session.status := msReady;
    frowBuffer := nil;
    feof := true;
    Result := false;
  end;
end;


function TMySqlResultSet.Read(var resultBind: TMySqlBinds): bool;
  procedure FetchFieldsSelfResultBind;
  var
    offset: int;
    bitMask: int;
    nullOfs: int;
    i: integer;
    fieldType: TMySqlFieldType;
    length: int;

    b: TmySqlBind;

  begin
    resultBind := Self.resultBind;
    offset := (fieldCount + 9) div 8 + 1;
    bitMask := 4;
    nullOfs := 1;

    for i := 0 to fieldCount - 1 do begin
      fieldType := fields[i]._Type;
      resultBind.Binds[i]._Type := fieldType;
      if (Byte(frowBuffer[nullOfs]) and bitMask) <> 0 then
        resultBind.Binds[i].IsNull := true
      else
      begin
        resultBind.Binds[i].IsNull := false;
        case fieldType of
          FIELD_TYPE_TINY:
            length := 1;
          FIELD_TYPE_SHORT,
          FIELD_TYPE_YEAR:
            length := 2;
          FIELD_TYPE_INT24,
          FIELD_TYPE_LONG:
            length := 4;
          FIELD_TYPE_LONGLONG:
            length := 8;
          FIELD_TYPE_FLOAT:
            length := 4;
          FIELD_TYPE_DOUBLE:
            length := 8;
          else
          begin
            length := Byte(frowBuffer[offset]);
            Inc(offset);
            case length of
              251: begin
                resultBind.Binds[i].IsNull := true;
                length := 0;
              end;
              252: begin
                length := BitConverter.ToUInt16(frowBuffer, offset);
                Inc(offset, 2);
              end;
              253: begin
                length := BitConverter.ToInt32(frowBuffer, offset) and $FFFFFF;
                Inc(offset, 3);
              end;
              254: begin
                length := BitConverter.ToInt32(frowBuffer, offset);
                Inc(offset, 8);
              end;
              255:
                raise ArgumentException.Create;
            end;
          end;
        end;
        b := resultBind.Binds[i];
        b.Length := length;
        b.Offset := offset;
        Inc(offset, length);
      end;
      bitMask := bitMask shl 1;
      if (bitMask and 255) = 0 then begin
        bitMask := 1;
        Inc(nullOfs);
      end;
    end;
  end;

  procedure FetchFields;
  var
    offset: int;
    i: integer;
    length: int;

    b: TmySqlBind;

  begin
    if resultBind = nil then
      raise EMySqlException.Create(NULL_POINTER);

    offset := 0;
    for i := 0 to fieldCount - 1 do begin
      length := Byte(frowBuffer[offset]);
      Inc(offset);

      case length of
        251: begin
          resultBind.Binds[i].Length := 0;
          resultBind.Binds[i].Offset := 0;
          resultBind.Binds[i].IsNull := true;
          continue;
        end;
        252: begin
          length := BitConverter.ToUInt16(frowBuffer, offset);
          Inc(offset, 2);
        end;
        253: begin
          length := BitConverter.ToInt32(frowBuffer, offset) and $FFFFFF;
          Inc(offset, 3);
        end;
        254: begin
          length := BitConverter.ToInt32(frowBuffer, offset);
          Inc(offset, 8);
        end;
        255:
          raise ArgumentException.Create;
      end;
      b := resultBind.Binds[i];
      b.Length := length;
      b.Offset := offset;
      b.IsNull := false;
      Inc(offset, length);
    end;
  end;

begin
  if eof then begin
    Result := False;
    Exit;
  end;

  if frows = nil then begin
    if ReadRow then begin
      if Self.resultBind <> nil then
        FetchFieldsSelfResultBind
      else
        FetchFields;
      Result := True;
    end
    else
      Result := False;
  end
  else
  begin
  {$IFDEF CLR}
    frowBuffer := TBytes(frows[frowIndex]);
  {$ELSE}
    frowBuffer := @TBytes(frows[frowIndex])[0];
  {$ENDIF}
    if Self.resultBind <> nil then
      FetchFieldsSelfResultBind
    else
      FetchFields;
    Inc(frowIndex);
    feof := (frowIndex >= frows.Count);
    Result := True;
  end;

  if Result then begin
  {$IFDEF CLR}
    resultBind.Buffer := frowBuffer;
  {$ELSE}
    resultBind.Buffer := @frowBuffer[0];
  {$ENDIF}
  end;
end;

procedure TMySqlResultSet.Seek(offset: int);
begin
  if frows <> nil then begin
    if offset >= frows.Count then
      raise ArgumentException.Create;
    frowIndex := offset;
    feof := false;
  end
  else
    raise NotSupportedException.Create;
end;

function TMySqlResultSet.GetRowCount: int;
begin
  if frows <> nil then
    Result := frows.Count
  else
    Result := rowCount;
end;

function TMySqlResultSet.ReadFieldsInfo(fieldCount: int): TMySqlFieldInfos;
var
  bind: TMySqlBinds;

  // Copied for MySql.Bind
  function GetString(Offset, Length: integer): AnsiString;
  begin
    if Length > 0 then
    {$IFDEF CLR}
      Result := Encoding.Default.GetString(bind.Buffer, Offset, Length)
    {$ELSE}
      Result := Marshal.PtrToStringAnsi(@bind.Buffer[Offset], Length)
    {$ENDIF}
    else
      Result := '';
  end;

  function GetByte(Offset: integer): Byte;
  begin
  {$IFDEF CLR}
    Result := bind.Buffer[Offset];
  {$ELSE}
    Result := Byte(bind.Buffer[Offset]);
  {$ENDIF}
  end;

  function GetInt16(Offset: integer): Int16;
  begin
    Result := Int16(
      Integer(bind.Buffer[Offset]) +
      Integer(bind.Buffer[Offset + 1]) shl 8);
  end;

  function GetInt24(Offset: integer): integer;
  begin
    Result :=
      Integer(bind.Buffer[Offset]) +
      Integer(bind.Buffer[Offset + 1]) shl 8 +
      Integer(bind.Buffer[Offset + 2]) shl 16;
  end;

  function GetInt32(Offset: integer): integer;
  begin
    Result :=
      Integer(bind.Buffer[Offset]) +
      Integer(bind.Buffer[Offset + 1]) shl 8 +
      Integer(bind.Buffer[Offset + 2]) shl 16 +
      Integer(bind.Buffer[Offset + 3]) shl 32;
  end;

var
  i: integer;
  catalog, database, table, orgTable, name, orgName: AnsiString;
  charsetnr, length: int;
  _type: TMySqlFieldType;
  flags, scale: int;
  defaultValue: AnsiString;
  maxLength: int;
begin
  bind := nil;
  try
    SetLength(Result, fieldCount);
    if fsession.protocol41 then begin
      bind := TMySqlBinds.Create(7);
      for i := 0 to fieldCount - 1 do begin
        Read(bind);
        catalog := GetString(bind.Binds[0].Offset, bind.Binds[0].Length);
        database := GetString(bind.Binds[1].Offset, bind.Binds[1].Length);
        table := GetString(bind.Binds[2].Offset, bind.Binds[2].Length);
        orgTable := GetString(bind.Binds[3].Offset, bind.Binds[3].Length);
        name := GetString(bind.Binds[4].Offset, bind.Binds[4].Length);
        orgName := GetString(bind.Binds[5].Offset, bind.Binds[5].Length);

        charsetnr := GetInt16(bind.Binds[6].Offset);
        length := GetInt32(bind.Binds[6].Offset + 2);
        _type := TMySqlFieldType(GetByte(bind.Binds[6].Offset + 6));
        flags := GetInt16(bind.Binds[6].Offset + 7);
        scale := GetInt16(bind.Binds[6].Offset + 9);
        defaultValue := '';
        maxLength := 0;
        Result[i] := MySqlFieldInfo(name, orgName, table, orgTable, database, defaultValue, length, scale, maxLength, flags, charsetnr, _type);
      end;
    end
    else begin
      bind := TMySqlBinds.Create(5);
      for i := 0 to fieldCount - 1 do begin
        Read(bind);
        orgTable := GetString(bind.Binds[0].Offset, bind.Binds[0].Length);
        name := GetString(bind.Binds[1].Offset, bind.Binds[1].Length);
        length := GetInt24(bind.Binds[2].Offset);
        _type := TMySqlFieldType(GetByte(bind.Binds[3].Offset));
        if (fsession.serverCapabilities and CLIENT_LONG_FLAG) <> 0 then begin
          flags := GetInt16(bind.Binds[4].Offset);
          scale := GetByte(bind.Binds[4].Offset + 2);
        end
        else
        begin
          flags := GetByte(bind.Binds[4].Offset);
          scale := GetByte(bind.Binds[4].Offset + 1);
        end;
        defaultValue := '';
        maxLength := 0;
        Result[i] := MySqlFieldInfo(name, '', orgTable, orgTable, '', defaultValue, length, scale, maxLength, flags, 0, _type);
      end;
    end;
  finally
    bind.Free;
  end;
end;

{ TRows }

{$IFNDEF CLR}
destructor TRows.Destroy;
begin
  if FList <> nil then
    Clear;
  inherited;
end;

procedure TRows.Add(const Row: TBytes);
var
  l: integer;
begin
  l := Length(FList);
  SetLength(FList, l + 1);
  FList[l] := Row;
end;

procedure TRows.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    SetLength(FList[i], 0);
    FList[i] := nil;
  end;
end;

function TRows.Get(Index: Integer): TBytes;
begin
  Result := TBytes(FList[Index]);
end;

function TRows.GetCount: integer;
begin
  Result := Length(FList);
end;
{$ENDIF}

end.
