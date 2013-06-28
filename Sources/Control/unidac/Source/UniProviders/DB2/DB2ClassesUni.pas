
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DB2Dac.inc}
unit DB2ClassesUni;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  Classes, SysUtils, Variants, SyncObjs,
{$IFNDEF FPC}
  FMTBcd,
{$ENDIF}
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
  MemUtils, MemData, CRAccess, CRParser,
{$IFNDEF UNIDACPRO}
  ODBCCall, ODBCClasses, DB2Call;
{$ELSE}
  ODBCCallUni, ODBCClassesUni, DB2CallUni;
{$ENDIF}

const
  prDB2Base = 2000;

  prSchema        = prDB2Base + 1;
  prFunctionPath  = prDB2Base + 2;

type
  TDB2RecordSet = class;

{ TDB2Connection }

  TDB2Connection = class(TODBCConnection)
  private
    FDatabase: string;
    FPort: integer;
    FSchema: _string;
    FFunctionPath: _string;

  protected
    function GetConnectionString: _string; override;
    function GetCli: TODBCCli; override;
    procedure ApplyConnectProps; override;

  public
    function GetCommandClass: TCRCommandClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
    function GetCurrentSchema: _string; override;
    procedure SetCurrentSchema(Value: _string);
    function GetCachedSchema: _string; override;
    procedure SetCurrentPath(Value: _string);
  end;

{ TDB2Transaction }

  TDB2Transaction = class(TODBCTransaction)
  public
    procedure Savepoint(const Name: _string); override;
  end;

{ TDB2Command }

  TDB2Command = class(TODBCCommand)
  protected
    function GetCDataType(DataType, SubDataType: word): smallint; override;
    function GetSQLDataType(DataType, SubDataType: word): smallint; override;
  end;

{ TDB2RecordSet }

  TDB2RecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
    function GetFieldFetchBlockSize(Field: TFieldDesc): integer; override;
    function GetFieldBindSize(Field: TFieldDesc): integer; override;
    function ReadFetchBlockField(Field: TFieldDesc; Source, Dest: IntPtr; Len: integer; var SharedPiece: PPieceHeader): boolean; override;

  public
    procedure CreateComplexField(RecBuf: IntPtr; FieldIndex: integer; WithBlob: boolean); override;
  end;

{ TDB2Lob }

  {TDB2Lob = class (TCompressedBlob)
  private
    FCommand: TDB2Command;
    FLocator: integer;
    FDataType: word;
    FNeedReadLob: boolean;
    FCached: boolean;

    function Cli: TDB2Cli;
    function GetLocatorType(DataType: word): smallint;

  public
    constructor Create(Command: TDB2Command; DataType: smallint);

    procedure ReadLob;

    property Locator: integer read FLocator write FLocator;
    property DataType: word read FDataType write FDataType;
  end;}

{$IFNDEF LITE}
  TDB2Loader = class (TODBCLoader)
  protected
    procedure CreateCommand; override;
    class function GetRecordSetClass: TCRRecordSetClass; override;
  end;
{$ENDIF}

implementation

uses
  DAConsts,
{$IFNDEF UNIDACPRO}
  ODBCConsts;
{$ELSE}
  ODBCConstsUni;
{$ENDIF}

{ TDB2Connection }

function TDB2Connection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prPort:
      FPort := Value;
    prSchema: begin
      if Value <> FSchema then begin
        if GetConnected then
          SetCurrentSchema(Value);
        FSchema := Value;
      end;
    end;
    prFunctionPath: begin
      if Value <> FFunctionPath then begin
        if GetConnected then
          SetCurrentPath(Value);
        FFunctionPath := Value;
      end;
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TDB2Connection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prPort:
      Value := FPort;
    prSchema:
      Value := FSchema;
    prFunctionPath:
      Value := FFunctionPath;
  else
    Result := inherited GetProp(Prop, Value);
  end
end;

procedure TDB2Connection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TDB2Connection(Source).FDatabase;
  FPort := TDB2Connection(Source).FPort;
  FSchema := TDB2Connection(Source).FSchema;
  FFunctionPath := TDB2Connection(Source).FFunctionPath;
end;

function TDB2Connection.GetCurrentSchema: _string;
var
  RecBuf: IntPtr;
  v: variant;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  CheckRecordSet;
  FRecordSet.SetSQL('VALUES (CURRENT SCHEMA)');
  FRecordSet.Open;
  FRecordSet.AllocRecBuf(RecBuf);
  try
    FRecordSet.GetNextRecord(RecBuf);
    if FRecordSet.Eof then begin
      Result := '';
      exit;
    end;
    FRecordSet.GetFieldAsVariant(1, RecBuf, v);
    Result := Trim(_VarToStr(v));
    FRecordSet.Close;
  finally
    Marshal.FreeHGlobal(RecBuf);
  end;
end;

function TDB2Connection.GetCachedSchema: _string;
begin
  if FCachedSchema = '' then
    if FSchema <> '' then
      FCachedSchema := FRecordSet.GetCommand.SQLInfo.NormalizeName(FSchema, False, True)
    else
      FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;

procedure TDB2Connection.SetCurrentSchema(Value: _string);
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if Value = '' then
    Value := FUsername;

  ExecSQL('SET SCHEMA ' + Value);
  FSchema := Value;
  FCachedSchema := '';
end;

procedure TDB2Connection.SetCurrentPath(Value: _string);
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if Value = '' then
    Value := 'SYSTEM PATH, USER';

  CheckRecordSet;
  FRecordSet.SetSQL('SET PATH ' + Value);
  FRecordSet.ExecCommand;
  FFunctionPath := Value;
end;

function TDB2Connection.GetCommandClass: TCRCommandClass;
begin
  Result := TDB2Command;
end;

function TDB2Connection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TDB2Transaction;
end;

function TDB2Connection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TDB2RecordSet;
end;

function TDB2Connection.GetConnectionString: _string;
var
  Port: integer;
  Database: string;
begin
  Port := FPort;
  if Port = 0 then
    Port := 50000;

  Database := Trim(FDatabase);

  Result := _Format('driver={IBM DB2 ODBC DRIVER};database=%s;hostname=%s;port=%d;protocol=TCPIP; uid=%s; pwd=%s',
            [Database, FServer, Port, FUsername, FPassword]);
end;

function TDB2Connection.GetCli: TODBCCli;
begin
  Result := GetODBCCli;
end;

procedure TDB2Connection.ApplyConnectProps;
begin
  inherited;

  if FSchema <> '' then
    SetCurrentSchema(FSchema);
  if FFunctionPath <> '' then
    SetCurrentPath(FFunctionPath);
end;

{ TDB2Transaction }

procedure TDB2Transaction.Savepoint(const Name: _string);
var
  Connection: TDB2Connection;
begin
  CheckActive;

  Connection := TDB2Connection(FConnections[0]);
  Connection.ExecSQL('SAVEPOINT ' + Name + ' ON ROLLBACK RETAIN CURSORS');
end;

{ TDB2Command }

function TDB2Command.GetCDataType(DataType, SubDataType: word): smallint;
begin
  {case SubDataType of
    dtBlobLocator:
      Result := SQL_C_BLOB_LOCATOR;
    dtClobLocator:
      Result := SQL_C_CLOB_LOCATOR;
    dtDBClobLocator:
      Result := SQL_C_DBCLOB_LOCATOR;
  else}
    Result := inherited GetCDataType(DataType, SubDataType);
  //end;
end;

function TDB2Command.GetSQLDataType(DataType, SubDataType: word): smallint;
begin
  {case SubDataType of
    dtBlobLocator:
      Result := SQL_BLOB;
    dtClobLocator:
      Result := SQL_CLOB;
    dtDBClobLocator:
      Result := SQL_DBCLOB;
  else}
    Result := inherited GetSQLDataType(DataType, SubDataType);
  //end;
end;

{ TDB2RecordSet }

procedure TDB2RecordSet.CreateComplexField(RecBuf: IntPtr; FieldIndex: integer; WithBlob: boolean);
{var
  Field: TFieldDesc;
  Lob: TDB2Lob;}
begin
  {Field := Fields[FieldIndex];
  if not Field.HasParent and (Field.FieldDescKind <> fdkCalculated) then
    if Field.SubDataType in [dtBlobLocator, dtClobLocator, dtDBClobLocator] then begin
      Lob := TDB2Lob.Create(TDB2Command(FCommand), Field.SubDataType);
      if Field.DataType = dtWideMemo then
        Lob.IsUnicode := True;
      Lob.EnableRollback;
      SetObject(FieldIndex + 1, RecBuf, Lob);
    end
    else}
      inherited;
end;

procedure TDB2RecordSet.CreateCommand;
begin
  SetCommand(TDB2Command.Create);
end;

function TDB2RecordSet.GetFieldFetchBlockSize(Field: TFieldDesc): integer;
begin
  {if Field.SubDataType in [dtBlobLocator, dtClobLocator, dtDBClobLocator] then
    Result := SizeOf(Integer)
  else}
    Result := inherited GetFieldFetchBlockSize(Field);
end;

function TDB2RecordSet.GetFieldBindSize(Field: TFieldDesc): integer;
begin
  {if Field.SubDataType in [dtBlobLocator, dtClobLocator, dtDBClobLocator] then
    Result := 0
  else}
    Result := inherited GetFieldBindSize(Field);
end;

function TDB2RecordSet.ReadFetchBlockField(Field: TFieldDesc; Source, Dest: IntPtr;
  Len: integer; var SharedPiece: PPieceHeader): boolean;
{var
  Lob: TDB2Lob;}
begin
  {if Field.SubDataType in [dtBlobLocator, dtClobLocator, dtDBClobLocator] then begin
    Lob := TDB2Lob(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
    Lob.Locator := Marshal.ReadInt32(Source);
    Lob.ReadLob;
    Result := True;
  end
  else}
    Result := inherited ReadFetchBlockField(Field, Source, Dest, Len, SharedPiece);
end;

{ TDB2Lob }

(*constructor TDB2Lob.Create(Command: TDB2Command; DataType: smallint);
begin
  inherited Create;

  FCommand := Command;
  FDataType := DataType;
end;

procedure TDB2Lob.ReadLob;
var
  LocType: smallint;
  Len, Ind: integer;
  Piece: PPieceHeader;
begin
  //CheckAlloc;
  //CheckSession;

  FData.Clear;

  LocType := GetLocatorType(FDataType);
  FCommand.Check(Cli.SQLGetLength(FCommand.SQLHStmt, LocType, FLocator, Len, Ind));

  if Len > 0 then begin
    AllocPiece(Piece, Len);
    try
      FCommand.Check(Cli.SQLGetSubString(FCommand.SQLHStmt, LocType, FLocator, 0, Len, SQL_C_BINARY,
        PtrOffset(Piece, Sizeof(TPieceHeader)), Len, Len, Ind));
      Piece.Used := Len;
      AppendPiece(Piece);
    except
      FreePiece(Piece);
      raise;
    end;
  end;

  FNeedReadLob := False;
  FCached := True;
end;

function TDB2Lob.Cli: TDB2Cli;
begin
  Result := TDB2Cli(FCommand.FConnection.Cli);
end;

function TDB2Lob.GetLocatorType(DataType: word): smallint;
begin
  case DataType of
    dtBlobLocator:
      Result := SQL_C_BLOB_LOCATOR;
    dtClobLocator:
      Result := SQL_C_CLOB_LOCATOR;
    dtDBClobLocator:
      Result := SQL_C_DBCLOB_LOCATOR;
  else
    Assert(False);
    Result := 0;
  end;
end;*)

{$IFNDEF LITE}

{ TDB2Loader }

procedure TDB2Loader.CreateCommand;
begin
  FCommand := TDB2Command.Create;
end;

class function TDB2Loader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TDB2RecordSet;
end;

{$ENDIF}

end.
