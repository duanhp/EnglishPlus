
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I SQLiteDac.inc}
unit LiteFunctionUni;
{$ENDIF}

interface

uses
  SysUtils, Classes, DB, Variants,
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  DAConsts, MemUtils, CRAccess,
{$IFNDEF UNIDACPRO}
  LiteCall, LiteError;
{$ELSE}
  LiteCallUni, LiteErrorUni;
{$ENDIF}

type

  TVariants = array of Variant;

  TLiteFunction = function(InValues: array of Variant): Variant;

  TCustomLiteFunctionDesc = class
  private
    FConnection: TCRConnection;
    FName: _string;
    FParamCount: Integer;
    FTextRepresentation: Integer;
  protected
    procedure RegisterFunction;
    procedure UnregisterFunction;

    function GetInParams(Context: Tsqlite3_context; ParamCount: Integer; pData: IntPtr): TVariants;
    procedure SetResult(Context: Tsqlite3_context; Value: Variant);

    procedure DoFunction(Context: Tsqlite3_context; ParamCount: Integer; pData: IntPtr); virtual; abstract;
  public
    constructor Create(Connection: TCRConnection; Name: _string; ParamCount: Integer);
    destructor Destroy; override;

    property Name: _string read FName;
    property ParamCount: Integer read FParamCount;
    property TextRepresentation: Integer read FTextRepresentation;
  end;

  TLiteFunctionDesc = class(TCustomLiteFunctionDesc)
  private
    FLiteFunction: TLiteFunction;
  protected
    procedure DoFunction(Context: Tsqlite3_context; ParamCount: Integer; pData: IntPtr); override;
  public
    constructor Create(Connection: TCRConnection; Name: _string; ParamCount: Integer; LiteFunction: TLiteFunction);

    property LiteFunction: TLiteFunction read FLiteFunction write FLiteFunction;
  end;

  TSQLiteFunctionManager = class
  private
    FConnection: TCRConnection;
    FFunctionList: TList;
  protected
    procedure InternalAddFunction(LiteFunctionDesc: TCustomLiteFunctionDesc);
    procedure InternalRemoveFunction(LiteFunctionDesc: TCustomLiteFunctionDesc);

    function FindFunction(Name: _string; ParamCount: Integer): TCustomLiteFunctionDesc;
    procedure UnRegistrAllFunctions;
  public
    constructor Create(Connection: TCRConnection);
    destructor Destroy; override;

    procedure RegisterFunction(Name: _string; ParamCount: Integer; LiteFunction: TLiteFunction);
    procedure UnRegisterFunction(Name: _string; ParamCount: Integer);
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  LiteClasses;
{$ELSE}
  LiteClassesUni;
{$ENDIF}

var
  CallBackLiteFunctionCode : array[0..23] of byte =
   ($55,
    $8B, $EC,

    $8B, $45, $10, $50,
    $8B, $45, $C, $50,
    $8B, $45, $8, $50,

    $B8, $00, $00, $00, $00,
    $FF, $D0,

    $5D,
    $C3);

  CallBackLiteFunctionPtr: IntPtr;
{$IFDEF CLR}
  HCallBackLiteFunction: GCHandle;
  CallBackLiteFunctionRec: packed record
    Ptr: TCallBackLiteFunction;
  end;
  CallbackRecPtr: IntPtr;
{$ENDIF}

{ Function }

procedure CallBackLiteFunction(Context: Tsqlite3_context;
                               ParamCount: Integer;
                               pData: IntPtr
                              ); {$IFNDEF CLR}cdecl;{$ENDIF}
var
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  pUserData := sqlite3_user_data(Context);
{$IFNDEF CLR}
  functionDesc := TCustomLiteFunctionDesc(pUserData);
{$ELSE}
  functionDesc := TCustomLiteFunctionDesc(GCHandle(pUserData).Target);
{$ENDIF}
  functionDesc.DoFunction(Context, ParamCount, pData);
end;

{ TCustomLiteFunctionDesc }

constructor TCustomLiteFunctionDesc.Create(Connection: TCRConnection; Name: _string; ParamCount: Integer);
begin
  inherited Create;

  FConnection := Connection;
  FName := Name;
  FParamCount := ParamCount;

  if TSQLiteConnection(FConnection).IsUnicodeDataBase then
    FTextRepresentation := SQLITE_UTF16
  else
    FTextRepresentation := SQLITE_UTF8;
end;

destructor TCustomLiteFunctionDesc.Destroy;
begin
  inherited;
end;

procedure TCustomLiteFunctionDesc.RegisterFunction;
var
  pName: PAnsiChar;
  pSelf: IntPtr;
begin
  pName := PAnsiChar(AnsiString(Name));
{$IFNDEF CLR}
  pSelf := Self;
{$ELSE}
  pSelf := IntPtr(GCHandle.Alloc(Self, GCHandleType.Normal));
{$ENDIF}
  sqlite3_create_function(TSQLiteConnection(FConnection).SQLite, pName, ParamCount, TextRepresentation, pSelf, CallBackLiteFunctionPtr, nil, nil);
end;

procedure TCustomLiteFunctionDesc.UnregisterFunction;
var
  pName: PAnsiChar;
begin
  if FConnection.GetConnected then begin
    pName := PAnsiChar(AnsiString(Name));
    sqlite3_create_function(TSQLiteConnection(FConnection).SQLite, pName, ParamCount, TextRepresentation, nil, nil, nil, nil);
  end;  
end;

function TCustomLiteFunctionDesc.GetInParams(Context: Tsqlite3_context; ParamCount: Integer; pData: IntPtr): TVariants;
var
  i: integer;
  liteConnection: TSQLiteConnection;
  ParamType: Integer;
  ParamSize: Integer;
  pParam: IntPtr;
  aStrParam: AnsiString;
  wStrParam: WideString;
  blobData: TBytes;
begin
  SetLength(Result, ParamCount);

  liteConnection := TSQLiteConnection(FConnection);
  for i := 0 to ParamCount - 1 do begin
    pParam := Marshal.ReadIntPtr(pData, i * sizeof(IntPtr));
    ParamType := sqlite3_value_type(pParam);

    case ParamType of
      SQLITE_INTEGER:
        Result[i] := sqlite3_value_int64(pParam);
      SQLITE_FLOAT:
        Result[i] := sqlite3_value_double(pParam);
      SQLITE_TEXT:
        if liteConnection.IsUnicodeDataBase then begin
          ParamSize := sqlite3_value_bytes16(pParam);
          wStrParam := Marshal.PtrToStringUni(sqlite3_value_text16(pParam), ParamSize shr 1);
          if liteConnection.UseUnicode then
            Result[i] := wStrParam
          else
            Result[i] := AnsiString(wStrParam);
        end
        else begin
          ParamSize := sqlite3_value_bytes(pParam);
          aStrParam := Marshal.PtrToStringAnsi(sqlite3_value_text(pParam), ParamSize);
          Result[i] := aStrParam;
        end;
      SQLITE_BLOB: begin
        ParamSize := sqlite3_value_bytes(pParam);
        SetLength(blobData, ParamSize);
        Marshal.Copy(sqlite3_value_blob(pParam), blobData, 0, ParamSize);
        Result[i] := blobData;
      end;
      SQLITE_NULL:
        Result[i] := null;
    else
      raise Exception.Create(SUnknownDataType);
    end;
  end;
end;

procedure TCustomLiteFunctionDesc.SetResult(Context: Tsqlite3_context; Value: Variant);
var
  liteConnection: TSQLiteConnection;
  liteType: Integer;
  needConvertToText: boolean;
  intValue: Integer;
  floatValue: Double;
  aStrValue: AnsiString;
  pStrValue: IntPtr;
  blobValue: TBytes;
  pBlobValue: IntPtr;
  pDestrType: IntPtr;
begin
  liteConnection := TSQLiteConnection(FConnection);

  if VarType(Value) = varNull then begin
    sqlite3_result_null(Context);
    exit;
  end;

  liteType := liteConnection.GetTypes.GetSQLiteType(liteConnection.GetTypes.GetVarType(VarType(Value)), needConvertToText);

  case liteType of
    SQLITE_INTEGER: begin
      intValue := Value;
      sqlite3_result_int64(Context, intValue);
    end;
    SQLITE_FLOAT: begin
      floatValue := Value;
      sqlite3_result_double(Context, floatValue);
    end;
    SQLITE_TEXT: begin
      if needConvertToText then
        aStrValue := liteConnection.GetTypes.ConvertToText(liteConnection.GetTypes.GetVarType(VarType(Value)), Value)
      else
        aStrValue := liteConnection.GetTypes.ConvertMemo(Value);

      pStrValue := Marshal.StringToHGlobalAnsi(aStrValue);
      try
        sqlite3_result_text(Context, pStrValue, Length(aStrValue), SQLITE_TRANSIENT);
      finally
        FreeString(pStrValue);
      end;
    end;
    SQLITE_BLOB: begin
      blobValue := liteConnection.GetTypes.ConvertBlob(Value, pDestrType);
    {$IFNDEF CLR}
      pBlobValue := blobValue;
      sqlite3_result_blob(Context, pBlobValue, Length(blobValue), pDestrType);
    {$ELSE}
      pBlobValue := Marshal.AllocHGlobal(Length(blobValue));
      Marshal.Copy(blobValue, 0, pBlobValue, Length(blobValue));
      try
        sqlite3_result_blob(Context, pBlobValue, Length(blobValue), pDestrType);
      finally
        Marshal.FreeHGlobal(pBlobValue);
      end;
    {$ENDIF}
    end;
    SQLITE_NULL:
      sqlite3_result_null(Context);
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

{ TLiteFunctionDesc }

constructor TLiteFunctionDesc.Create(Connection: TCRConnection; Name: _string; ParamCount: Integer; LiteFunction: TLiteFunction);
begin
  inherited Create(Connection, Name, ParamCount);
  FLiteFunction := LiteFunction;
end;

procedure TLiteFunctionDesc.DoFunction(Context: Tsqlite3_context; ParamCount: Integer; pData: IntPtr);
var
  ResultValue: Variant;
begin
    ResultValue := FLiteFunction(GetInParams(Context, ParamCount, pData));
    SetResult(Context, ResultValue);
end;

{ TSQLiteFunctionManager }

constructor TSQLiteFunctionManager.Create(Connection: TCRConnection);
begin
  inherited Create;
  FConnection := Connection;
  FFunctionList := TList.Create;
end;

destructor TSQLiteFunctionManager.Destroy;
begin
  UnRegistrAllFunctions;
  FFunctionList.Free;
  inherited;
end;

procedure TSQLiteFunctionManager.InternalAddFunction(LiteFunctionDesc: TCustomLiteFunctionDesc);
var
  ExistLiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  ExistLiteFunctionDesc := FindFunction(LiteFunctionDesc.Name, LiteFunctionDesc.ParamCount);
  if ExistLiteFunctionDesc <> nil then
    InternalRemoveFunction(ExistLiteFunctionDesc);

  LiteFunctionDesc.RegisterFunction;
  FFunctionList.Add(LiteFunctionDesc);
end;

procedure TSQLiteFunctionManager.InternalRemoveFunction(LiteFunctionDesc: TCustomLiteFunctionDesc);
begin
  LiteFunctionDesc.UnregisterFunction;
  FFunctionList.Remove(LiteFunctionDesc);
  LiteFunctionDesc.Free;
end;

function TSQLiteFunctionManager.FindFunction(Name: _string; ParamCount: Integer): TCustomLiteFunctionDesc;
var
  i: integer;
begin
  for i := 0 to FFunctionList.Count - 1 do
  begin
    Result := TCustomLiteFunctionDesc(FFunctionList[i]);
    if (Result.Name = Name) and (Result.ParamCount = ParamCount) then
      exit;
  end;

  // if not found
  Result := nil;
end;

procedure TSQLiteFunctionManager.UnRegistrAllFunctions;
var
  i: integer;
  isConnected: boolean;
begin
  isConnected := FConnection.GetConnected;

  // unregister Functions
  for i := 0 to FFunctionList.Count - 1 do begin
    if isConnected then
      TCustomLiteFunctionDesc(FFunctionList[i]).UnregisterFunction;
    TCustomLiteFunctionDesc(FFunctionList[i]).Free;
  end;

  FFunctionList.Clear;
end;

procedure TSQLiteFunctionManager.RegisterFunction(Name: _string; ParamCount: Integer; LiteFunction: TLiteFunction);
var
  LiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  LiteFunctionDesc := TLiteFunctionDesc.Create(FConnection, Name, ParamCount, LiteFunction);
  InternalAddFunction(LiteFunctionDesc);
end;

procedure TSQLiteFunctionManager.UnRegisterFunction(Name: _string; ParamCount: Integer);
var
  LiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  LiteFunctionDesc := FindFunction(Name, ParamCount);
  if LiteFunctionDesc <> nil then
    InternalRemoveFunction(LiteFunctionDesc);
end;

initialization
{$IFNDEF CLR}
  CallBackLiteFunctionPtr := @CallBackLiteFunction;
{$ELSE}
  CallBackLiteFunctionPtr := Marshal.AllocHGlobal(Length(CallBackLiteFunctionCode));
  Marshal.Copy(TBytes(CallBackLiteFunctionCode), 0, CallBackLiteFunctionPtr, Length(CallBackLiteFunctionCode));

  CallbackRecPtr := Marshal.AllocHGlobal(20);
  try
    CallBackLiteFunctionRec.Ptr := CallBackLiteFunction;
    HCallBackLiteFunction := GCHandle.Alloc(@CallBackLiteFunctionRec.Ptr, GCHandleType.Normal);
    Marshal.StructureToPtr(CallBackLiteFunctionRec, CallbackRecPtr, False);
    Marshal.WriteIntPtr(CallBackLiteFunctionPtr, 16, Marshal.ReadIntPtr(CallbackRecPtr));
  finally
    Marshal.FreeHGlobal(CallbackRecPtr);
  end;
{$ENDIF}

finalization
{$IFNDEF CLR}
  CallBackLiteFunctionPtr := nil;
{$ELSE}
  HCallBackLiteFunction.Free;
  Marshal.FreeHGlobal(CallBackLiteFunctionPtr);
{$ENDIF}

end.
