
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlStmtUni;
{$ENDIF}

interface

uses
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  SysUtils, CRAccess,
{$IFNDEF UNIDACPRO}
  MySqlSession, MySqlBind, MyCall, MySqlResultSet;
{$ELSE}
  MySqlSessionUni, MySqlBindUni, MyCallUni, MySqlResultSetUni;
{$ENDIF}

type
  TMySqlFieldTypes = array of TMySqlFieldType;

  TMySqlStmt = class
  protected
    Ferrno: longword;
    Ferror: AnsiString;

    fsession: TMySqlSession;
    fresult: TMySqlResultSet;
    fstmtId: int;
    fstate: TPrepStmtState;
    fparamCount: int;
    ffieldCount: int;
    ffields: TMySqlFieldInfos;
    faffectedRows: long;
    FSendedTypes: TMySqlFieldTypes;

    procedure SetResult(Value: TMySqlResultSet);

  public
    constructor Create(session: TMySqlSession);
    destructor Destroy; override;

    procedure Close;
    procedure FreeResult;
    procedure Prepare(query: TBytes; length: int);
    procedure BindResult(bind: TMySqlBinds);
    procedure Execute(Params: TParamDescs; const Unicode: boolean);
    function Fetch(var bind: TMySqlBinds): int;

    property errno: longword read Ferrno write Ferrno;
    property error: AnsiString read Ferror write Ferror;

    property AffectedRows: long read faffectedRows;
    property Session: TMySqlSession read fsession;
    property Result: TMySqlResultSet read fresult write SetResult;
    property ParamCount: int read fparamCount;
    property FieldCount: int read ffieldCount;
  end;

implementation

uses
{$IFDEF VER6P}
  Variants, FMTBcd, DateUtils,
{$ENDIF}
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
{$IFDEF CLR}
  System.Text,
{$ENDIF}
  MemData, MemUtils,
{$IFNDEF UNIDACPRO}
  MySqlNet, MySqlErrors, MySqlApi, MyClasses;
{$ELSE}
  MySqlNetUni, MySqlErrorsUni, MySqlApiUni, MyClassesUni;
{$ENDIF}

const
  NoData: int = 100;

constructor TMySqlStmt.Create(session: TMySqlSession);
begin
  inherited Create;
  Self.fsession := session;
  fstate := psUnknown;
end;

destructor TMySqlStmt.Destroy;
begin
  if result <> nil then begin
    result.ResultBind := nil;
    result := nil;
  end;
//  fsession.FreeQuery;
  Close;
  inherited;
end;

procedure TMySqlStmt.SetResult(Value: TMySqlResultSet);
begin
  if fresult <> nil then
    fresult.Free;
  fresult := Value;
end;

procedure TMySqlStmt.Close;
var
  net: TMySqlNet;
begin
  session.FreeQuery();
  if fstate <> psUnknown then begin
    session.WriteCommand(scCloseStmt);
    net := session.net;
    net.WriteInt32(fstmtId);
    net.Send;
    fstate := psUnknown;
    fstmtId := 0;
  end;
  ffieldCount := 0;
  fparamCount := 0;
  if result <> nil then begin
    result.Clear;
    fresult.Free;
    result := nil;
  end;
  ffields := nil;
end;

procedure TMySqlStmt.FreeResult;
begin
  if session <> nil then
    session.FreeQuery;
  if result <> nil then begin
    result.Clear;
    // result.Free; - implicitly called from "result := nil"
    result := nil;
  end;
end;

procedure TMySqlStmt.Prepare(query: TBytes; length: int);
var
  net: TMySqlNet;
  fieldsResult: TMySqlResultSet;
  paramsResult: TMySqlResultSet;
begin
  session.SimpleCommand(scPrepare, query, length, true);

  net := session.net;
  net.Receive;
  if net.ReadByte = 255 then
    raise EMySqlException.Create(CR_SERVER_LOST);
  fstmtId := net.ReadInt32;
  ffieldCount := net.ReadInt16;
  fparamCount := net.ReadInt16;
  result := nil;

  if paramCount > 0 then begin
    paramsResult := TMySqlResultSet.Create(session, nil, 5, nil);
    try
      paramsResult.ReadRows(false);
    finally
      paramsResult.Free;
    end;
  end;
  
  if ffieldCount <> 0 then begin
    if (session.serverStatus and SERVER_STATUS_AUTOCOMMIT) = 0 then
      session.serverStatus := session.serverStatus and SERVER_STATUS_IN_TRANS;
    if net.Length > net.Position then
      session.extraInfo := net.ReadFieldLength;
    if session.protocol41 then
      fieldsResult := TMySqlResultSet.Create(session, nil, 7, nil)
    else
      fieldsResult := TMySqlResultSet.Create(session, nil, 5, nil);
    try
      fieldsResult.ReadRows(False);
      ffields := fieldsResult.ReadFieldsInfo(ffieldCount);
      result := TMySqlResultSet.Create(session, ffields, ffieldCount, nil);
    finally
      fieldsResult.Free;
    end;
  end;

  fstate := psPrepare;
end;

procedure TMySqlStmt.BindResult(bind: TMySqlBinds);
begin
  if fstate = psUnknown then
    raise EMySqlException.Create(NO_PREPARE_STMT);

  if ffieldCount = 0 then
    raise EMySqlException.Create(CR_UNKNOWN_ERROR);

  fresult.ResultBind := bind;
end;

procedure TMySqlStmt.Execute(Params: TParamDescs; const Unicode: boolean);
var
  net: TMySqlNet;

  procedure SaveParamValue(ParamDesc: TParamDesc);
  var
    InternalType: word;
    ParamVarType: TVarType;
  {$IFDEF CLR}
    b: TBytes;
  {$ELSE}
    ParamVarPtr: IntPtr;
    ws: WideString;
    u: UTF8String;
  {$ENDIF}
  {$IFDEF VER6P}
    i64: Int64;
  {$ENDIF}
    l: Integer;
    Blob: TBlob;
    Piece: PPieceHeader;
    AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
    s: string;

  begin
    InternalType := ParamDesc.GetDataType;
    ParamVarType := VarType(ParamDesc.Value);

    case InternalType of
      dtBoolean: begin
        //net.WriteFieldLength(sizeof(byte));
        net.WriteByte(Byte(WordBool(Boolean(ParamDesc.Value)))); // Convert to boolean is useful to bypass Delphi bug
      end;
      dtInt8: begin
        //net.WriteFieldLength(sizeof(byte));
        net.WriteByte(Byte(ParamDesc.Value));
      end;
      dtInt16: begin
        //net.WriteFieldLength(sizeof(smallint));
        net.WriteInt16(SmallInt(ParamDesc.Value));
      end;
      dtUInt16: begin
        //net.WriteFieldLength(sizeof(word));
        net.WriteInt32(Word(ParamDesc.Value));
      end;
      dtInt32: begin
        //net.WriteFieldLength(sizeof(integer));
        net.WriteInt32(Integer(ParamDesc.Value));
      end;
      dtUInt32: begin
        //net.WriteFieldLength(sizeof(longword));
        net.WriteInt32(Integer(LongWord(ParamDesc.Value)));
      end;
      dtInt64: begin
        //net.WriteFieldLength(sizeof(Int64));
      {$IFDEF VER6P}
        i64 := ParamDesc.Value; // Explicit Convert!
        net.WriteInt64(i64);
      {$ELSE}
        if ParamVarType in [$000E, $0014] then
          net.WriteInt64(PInt64(@TVarData(ParamDesc.Value).VInteger)^)
        else
          net.WriteInt64(TVarData(ParamDesc.Value).VInteger);
      {$ENDIF}
      end;

      // Float fields
      dtFloat: begin
        //net.WriteFieldLength(sizeof(double));
        net.WriteInt64(BitConverter.DoubleToInt64Bits(Double(ParamDesc.Value)));
      end;

      // Long fields
      dtBlob, dtBytes, dtVarBytes, // BytesByRef
      dtMemo, dtWideMemo, dtString, dtWideString: begin // CharsByRef
        if ParamVarType = varArray + varByte then begin
        {$IFDEF CLR}
          l := VarArrayHighBound(ParamDesc.Value, 1) - VarArrayLowBound(ParamDesc.Value, 1) + 1;
          net.WriteFieldLength(l);
          net.WriteBytes(ParamDesc.Value, 0, l);
        {$ELSE}
          ParamVarPtr := TVarData(ParamDesc.Value).VArray.Data;
          l := TVarData(ParamDesc.Value).VArray.Bounds[0].ElementCount;
          net.WriteFieldLength(l);
          net.WriteBytes(PAnsiChar(ParamVarPtr), 0, l);
        {$ENDIF}
        end
        else
      {$IFDEF CLR}
        if ParamDesc.Value is TBlob then begin
          Assert(ParamDesc.Value <> nil);
          Blob := TBlob(ParamDesc.Value);
        {$IFDEF HAVE_COMPRESS}
          if Blob is TCompressedBlob then
            TCompressedBlob(Blob).Compressed := False; // may be non-optimal
        {$ENDIF}

          l := Integer(Blob.Size);
          net.WriteFieldLength(l);

          Piece := Blob.FirstPiece;
          while IntPtr(Piece) <> nil do begin
            net.WriteBytes(PtrOffset(Piece, sizeof(TPieceHeader)), 0, Piece.Used);
            Piece := Piece.Next;
          end;
        end
      {$ELSE}
        if ParamVarType = varByRef{$IFDEF FPC} or varVariant{$ENDIF} then begin
          Assert(TVarData(ParamDesc.Value).VPointer <> nil);
          // Assert(TObject(TVarData(ParamDesc.Value).VPointer) is TBlob); - trial
          Blob := TVarData(ParamDesc.Value).VPointer;
        {$IFDEF HAVE_COMPRESS}
          if Blob is TCompressedBlob then
            TCompressedBlob(Blob).Compressed := False; // may be non-optimal
        {$ENDIF}
          net.WriteFieldLength(Integer(Blob.Size));

          Piece := Blob.FirstPiece;
          while Piece <> nil do begin
            net.WriteBytes(PAnsiChar(PtrOffset(Piece, sizeof(TPieceHeader))), 0, Piece.Used);
            Piece := Piece.Next;
          end;
        end
      {$ENDIF}
        else
          if Unicode and (ParamDesc.GetDataType in [dtMemo, dtWideMemo, dtString, dtWideString]) then begin
          {$IFDEF CLR}
            s := ParamDesc.Value;
            b := Encoding.UTF8.GetBytes(s);
            l := Length(b);
            net.WriteFieldLength(l);
            net.WriteBytes(b);
          {$ELSE}
            if ParamDesc.GetDataType = dtWideString then begin
              ws := ParamDesc.Value;
              u := UTF8Encode(ws);
            end
            else
            begin
              s := ParamDesc.Value;
              u := AnsiToUTF8(s);
            end;
            l := Length(u);
            net.WriteFieldLength(l);
            net.WriteBytes(PAnsiChar(u), 0, l);
          {$ENDIF}
          end
          else
          begin
          // CharsByRef Input parameter
          {$IFDEF CLR}
            s := ParamDesc.Value;
            if (ParamDesc.GetDataType in [dtMemo, dtWideMemo, dtString, dtWideString]) then
              l := Length(s)
            else
              l := Integer(ParamDesc.GetSize);
            net.WriteFieldLength(l);
            b := Encoding.Default.GetBytes(s);
            net.WriteBytes(b);
          {$ELSE}
            ParamVarPtr := TVarData(ParamDesc.Value).VPointer;
            if (ParamDesc.GetDataType in [dtMemo, dtWideMemo, dtString, dtWideString]) then
              if ParamVarPtr <> nil then begin
                if (ParamVarType = varOleStr) {$IFDEF VER12P}or (ParamVarType = varUString){$ENDIF} then begin // WideString
                  ParamVarPtr := PAnsiChar(AnsiString(ParamDesc.Value));
                  l := Length(ParamDesc.Value);
                end
                else // Pascal string
                  l := StrLen(PAChar(ParamVarPtr))
              end
              else
                l := 0
            else
              l := Integer(ParamDesc.GetSize);
            net.WriteFieldLength(l);
            net.WriteBytes(PAnsiChar(ParamVarPtr), 0, l);
          {$ENDIF}
          end;
      end;

      // DateTime fields
      dtDate: begin
        net.WriteFieldLength(4);

        DecodeDate(ParamDesc.Value, AYear, AMonth, ADay);
        net.WriteInt16(AYear);
        net.WriteByte(AMonth);
        net.WriteByte(ADay);
      end;

      dtTime: begin
        net.WriteFieldLength(12);

        DecodeTime(ParamDesc.Value, AHour, AMinute, ASecond, AMilliSecond);
        net.WriteByte(0); // MYSQL_TIME.neg
        net.WriteInt32(0);
        net.WriteByte(AHour);
        net.WriteByte(AMinute);
        net.WriteByte(ASecond);
        net.WriteInt32(AMilliSecond * 1000);
      end;

      dtDateTime: begin
        net.WriteFieldLength(11);

        DecodeDateTime(ParamDesc.Value, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
        net.WriteInt16(AYear);
        net.WriteByte(AMonth);
        net.WriteByte(ADay);
        net.WriteByte(AHour);
        net.WriteByte(AMinute);
        net.WriteByte(ASecond);
        net.WriteInt32(AMilliSecond * 1000);
      end;

    {$IFDEF VER6P}
    {$IFNDEF FPC}
      dtFMTBCD,
    {$ENDIF}
    {$ENDIF}
      dtBCD: begin
        s := BCDParamDescToStr(ParamDesc);
        l := Length(s);
        net.WriteFieldLength(l);
      {$IFDEF CLR}
        b := Encoding.Default.GetBytes(s);
        net.WriteBytes(b);
      {$ELSE}
        net.WriteBytes(PAnsiChar(AnsiString(s)), 0, l);
      {$ENDIF}
      end;
      else
        Assert(False, Format('Invalid internal field type $%X (%d)', [InternalType, InternalType]));
    end;
  end;

var
  nilCount, nilOfs: int;
  i, j, ParamOffset: integer;

  sendTypes: boolean;
  TypesToSend: TMySqlFieldTypes;
begin
  if (Params.Count > 0) and (Params[0].GetParamType = pdResult) then
    ParamOffset := 1
  else
    ParamOffset := 0;

  session.WriteCommand(scExecute);
  net := session.net;
  net.WriteInt32(fstmtId);

  net.WriteByte(0); // no flags
  net.WriteInt32(1); // iteration count

  if (fparamCount <> 0) and (fparamCount = Params.Count - ParamOffset) then begin
    // Process Param.IsNull
    nilCount := (fparamCount + 7) div 8;
    nilOfs := net.Position;
    net.Fill(0, nilCount);
    for i := 0 to fparamCount - 1 do
      if Params[i + ParamOffset].GetNull then begin
        j := nilOfs + i div 8;
        Byte(net.Buffer[j]) := Byte(net.Buffer[j]) or Byte(1 shl (i and 7));
      end;

    // Process types
    SetLength(TypesToSend, fparamCount);
    for i := 0 to fparamCount - 1 do
      TypesToSend[i] := ConvertInternalTypeMySQLFormat(Params[i + ParamOffset].GetDataType);

    sendTypes := Length(TypesToSend) <> Length(FSendedTypes);
    if not sendTypes then
      for i := 0 to fparamCount - 1 do
        if TypesToSend[i] <> FSendedTypes[i] then begin
          sendTypes := True;
          Break;
        end;

    net.WriteBool(sendTypes);
    if sendTypes then begin
      for i := 0 to fparamCount - 1 do
        net.WriteInt16(short(TypesToSend[i]));
      FSendedTypes := TypesToSend;
    end;

    // Process parameter values
    for i := 0 to fparamCount - 1 do
      if not Params[i + ParamOffset].GetNull then
        SaveParamValue(Params[i + ParamOffset]);
  end;
  net.Send;
  session.ReadQueryResult;
  if (session.fieldCount <> 0) and (session.fields <> nil) then begin
    ffields := session.fields;
    ffieldCount := session.fieldCount;
    session.fieldCount := 0;
    SetLength(fsession.fields, 0);
    result := TMySqlResultSet.Create(session, ffields, ffieldCount, nil);
  end;
  if ffieldCount > 0 then
    session.status := msUseResult;
  faffectedRows := session.affectedRows;
end;

function TMySqlStmt.Fetch(var bind: TMySqlBinds): int;
begin
  bind := nil;
  if fresult.Read(Bind) then begin
    Result := 0;
    Exit;
  end
  else begin
    Result := NoData;

    // On executing of prepared StoredProc it is necessary to skip the last data block
    if session.SkipPacket and (session.serverStatus and SERVER_MORE_RESULTS_EXISTS <> 0) then begin
      session.net.Receive;
      session.SkipPacket := False;
    end;
  end;
end;

end.
