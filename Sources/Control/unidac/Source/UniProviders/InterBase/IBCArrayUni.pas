
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 2006-2011 Devart. All right reserved.
//  InterBase Arrays
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I IbDac.inc}
unit IBCArrayUni;
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFDEF CLR}
  System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  MemData, MemUtils,
{$IFNDEF UNIDACPRO}
  IBCCall, IBCClasses;
{$ELSE}
  IBCCallUni, IBCClassesUni;
{$ENDIF}

type
  TIBCArrayType = class;
  TIBCIntegerDynArray = array of integer;

  TARRAYDESCType = (adGDS, adGDS7);

  TDescAccessor = class
  public
    FDesc: IntPtr;
    FArrayDescType: TARRAYDESCType;
    function GetDescVersion: SmallInt;
    procedure SetDescVersion(Value: SmallInt);
    function GetColumnName: string;
    procedure SetColumnName(Value: string);
    function GetRelationName: string;
    procedure SetRelationName(value: string);
    function GetLength: SmallInt;
    procedure SetLength(Value: SmallInt);
    function GetDimensions: SmallInt;
    procedure SetDimensions(Value: SmallInt);
    function GetHighBound(Dimension: integer): SmallInt;
    procedure SetHighBound(Dimension: integer; Value: SmallInt);
    function GetLowBound(Dimension: integer): SmallInt;
    procedure SetLowBound(Dimension: integer; Value: SmallInt);
    function GetDataType: byte;
    procedure SetDataType(Value: byte);
    function GetScale: ShortInt;
    procedure SetScale(Value: ShortInt);
  protected
    procedure SetDesc(Desc: IntPtr);
  public
    constructor Create(ArrayDescType: TARRAYDESCType; Desc: IntPtr);
    procedure Clear;
    property DescVersion: SmallInt read GetDescVersion write SetDescVersion;
    property ColumnName: string read GetColumnName write SetColumnName;
    property RelationName: string read GetRelationName write SetRelationName;
    property Length: SmallInt read GetLength write SetLength;
    property Dimensions: SmallInt read GetDimensions write SetDimensions;
    property HighBound[Dimension: integer]: SmallInt read GetHighBound write SetHighBound;
    property LowBound[Dimension: integer]: SmallInt read GetLowBound write SetLowBound;
    property DataType: byte read GetDataType write SetDataType;
    property Scale: ShortInt read GetScale write SetScale;
  end;

  TCustomIBCArray = class(TDBObject)
  private
    FStatusVector: TStatusVector;
    FDbHandle: PISC_DB_HANDLE;
    FTrHandle: PISC_TR_HANDLE;
    FConnection: TGDSConnection;
    FTransaction: TGDSTransaction;
    FArrayID: PISC_QUAD;
    FArrayDesc: IntPtr;
    FTableName: string;
    FColumnName: string;
    FArrayBuffer: IntPtr;
    FCached: boolean;
    FNativeDesc: boolean;
    FFirstWrite: boolean;

    FArrayDimensions: integer;
    FArrayHighBounds: array of integer;
    FArrayLowBounds: array of integer;

    FModified: boolean;

    procedure AllocBuffer;
    procedure FreeBuffer;

    procedure PutArray(Offset: integer; Size: integer);

    function BufItemToVariant(Offset: integer): variant;
    function BufToVarArray(Bounds: array of integer): variant;

    procedure VariantToBufItem(const Value: variant; Offset: integer);
    procedure VarArrayToBuf(const Values: variant);

    procedure Check(Status: ISC_STATUS);
    procedure CheckBounds(Bounds: array of integer);
    procedure CheckCachedIndices(Indices: array of integer);
    procedure CheckArrayIndices(Indices: array of integer);

    function GetGDS: TGDS;

    function GetArrayID: TISC_QUAD;
    procedure SetArrayID(const Value: TISC_QUAD);

    function GetDbHandle: TISC_DB_HANDLE;
    procedure SetDbHandle(Value: TISC_DB_HANDLE);

    function GetTrHandle: TISC_TR_HANDLE;
    procedure SetTrHandle(Value: TISC_TR_HANDLE);

    procedure SetConnection(Value: TGDSConnection);
    procedure SetTransaction(Value: TGDSTransaction);

    function GetArrayType: TIBCArrayType;
    procedure SetArrayType(Value: TIBCArrayType);

    procedure SetColumnName(const Value: string);
    procedure SetTableName(const Value: string);

    procedure SetArrayDimensions(Value: integer);

    function GetArrayHighBound(Dimension: integer): integer;
    procedure SetArrayHighBound(Dimension: integer; Value: integer);

    function GetArrayLowBound(Dimension: integer): integer;
    procedure SetArrayLowBound(Dimension: integer; Value: integer);

    function GetCachedDimensions: integer;
    function GetCachedHighBound(Dimension: integer): integer;
    function GetCachedLowBound(Dimension: integer): integer;

    function GetItemScale: integer;
    procedure SetItemScale(Value: integer);

    function GetItemSize: integer;
    procedure SetItemSize(Value: integer);

    function GetItemOffset(Indices: array of integer): integer;
    function GetSliceOffset(Bounds: array of integer): integer;

    function GetItemCount(Bounds: array of integer): integer;

    function GetItemVarType: integer;

    function GetItems: variant;
    procedure SetItems(const Values: variant);

    function GetIsNull: boolean;
    procedure SetIsNull(const Value: boolean);

    function GetArraySize: integer;
    function GetSliceSize(Bounds: array of integer): integer;
    function GetCachedSize: integer;

    procedure SetCached(Value: boolean);
    function GetArrayIndices(Name: string): TIBCIntegerDynArray;

    function GetAsString: string;
    procedure SetAsString(Value: string);

  protected
    FInternalItemType: word;
    FDescAccessor: TDescAccessor;

    procedure Init(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
    function GetInternalItemType: Word;
    procedure GetAttributeValue(const Name: _string; var AttrBuf: IntPtr; var IsBlank, NativeBuffer: boolean); override;
    procedure SetAttributeValue(const Name: _string; Source: IntPtr); override;
    function GetAttrIsNull(const Name: _string): boolean; override;
    procedure SetArrayDesc(Desc: IntPtr);
    function GetArrayIDPtr: PISC_QUAD;

  public
    constructor Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE); overload;
    constructor Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE; TableName, ColumnName: string); overload;
    constructor Create(Connection: TGDSConnection; Transaction: TGDSTransaction; TableName, ColumnName: string); overload;
    destructor Destroy; override;

    procedure Disconnect; override;

    procedure GetArrayInfo;

    procedure CreateTemporaryArray;

    procedure ReadArray;
    procedure ReadArraySlice(Bounds: array of integer);
    procedure ReadArrayItem(Indices: array of integer);

    procedure WriteArray;
    procedure WriteArraySlice(Bounds: array of integer);

    procedure ClearArray;

    procedure Assign(Source: TCustomIBCArray);
    property Modified: boolean read FModified;

    property GDS: TGDS read GetGDS;
    property DbHandle: TISC_DB_HANDLE read GetDbHandle write SetDbHandle;
    property TrHandle: TISC_TR_HANDLE read GetTrHandle write SetTrHandle;
    property Connection: TGDSConnection read FConnection write SetConnection;
    property Transaction: TGDSTransaction read FTransaction write SetTransaction;
    property ArrayType: TIBCArrayType read GetArrayType write SetArrayType;

    property TableName: string read FTableName write SetTableName;
    property ColumnName: string read FColumnName write SetColumnName;

    property ArrayID: TISC_QUAD read GetArrayID write SetArrayID;

    property ArrayDimensions: integer read FArrayDimensions write SetArrayDimensions;
    property ArrayLowBound[Dimension: integer]: integer read GetArrayLowBound write SetArrayLowBound;
    property ArrayHighBound[Dimension: integer]: integer read GetArrayHighBound write SetArrayHighBound;
    property ArraySize: integer read GetArraySize;

    property Cached: boolean read FCached write SetCached;
    property IsNull: boolean read GetIsNull write SetIsNull;

    property CachedDimensions: integer read GetCachedDimensions;
    property CachedLowBound[Dimension: integer]: integer read GetCachedLowBound;
    property CachedHighBound[Dimension: integer]: integer read GetCachedHighBound;
    property CachedSize: integer read GetCachedSize;
    property ItemSize: integer read GetItemSize write SetItemSize;
    property ItemScale: integer read GetItemScale write SetItemScale;

    property Items: variant read GetItems write SetItems;
    function GetItemsSlice(Bounds: array of integer): variant;
    procedure SetItemsSlice(const Values: variant);

    function GetItemValue(Indices: array of integer): variant;
    procedure SetItemValue(Indices: array of integer; Value: variant);

    function GetItemAsString(Indices: array of integer): string;
    procedure SetItemAsString(Indices: array of integer; Value: string);

    function GetItemAsWideString(Indices: array of integer): WideString;
    procedure SetItemAsWideString(Indices: array of integer; Value: WideString);

    function GetItemAsInteger(Indices: array of integer): integer;
    procedure SetItemAsInteger(Indices: array of integer; Value: integer);

    function GetItemAsSmallInt(Indices: array of integer): SmallInt;
    procedure SetItemAsSmallInt(Indices: array of integer; Value: SmallInt);

    function GetItemAsFloat(Indices: array of integer): double;
    procedure SetItemAsFloat(Indices: array of integer; Value: double);

    function GetItemAsDateTime(Indices: array of integer): TDateTime;
    procedure SetItemAsDateTime(Indices: array of integer; Value: TDateTime);

    property AsString: string read GetAsString write SetAsString;
  end;

  TIBCArrayType = class(TObjectType)
  private
    FDbHandle: PISC_DB_HANDLE;
    FTrHandle: PISC_TR_HANDLE;
    FConnection: TGDSConnection;
    FTransaction: TGDSTransaction;
    FTableName: string;
    FColumnName: string;
    FStatusVector: TStatusVector;
    FArrayDesc: IntPtr;
    FDescAccessor: TDescAccessor;
    FLowBound: integer;
    FADType: TARRAYDESCTYPE;

    function GetGDS: TGDS;
    function GetDbHandle: TISC_DB_HANDLE;
    procedure SetDbHandle(Value: TISC_DB_HANDLE);
    function GetTrHandle: TISC_TR_HANDLE;
    procedure SetTrHandle(Value: TISC_TR_HANDLE);
    procedure SetConnection(Value: TGDSConnection);
    procedure SetTransaction(Value: TGDSTransaction);
    procedure Check(Status: ISC_STATUS);
    procedure SetLowBound(const Value: integer);

  protected
    procedure Init(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE;
      TableName, ColumnName: string; NeedDescribe: boolean = True);

  public
    constructor Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE;
      TableName, ColumnName: string; NeedDescribe: boolean = True); overload;
    constructor Create(Connection: TGDSConnection; Transaction: TGDSTransaction;
      TableName, ColumnName: string; NeedDescribe: boolean = True); overload;
    destructor Destroy; override;

    procedure Describe(TableName, ColumnName: string);

    property GDS: TGDS read GetGDS;
    property DbHandle: TISC_DB_HANDLE read GetDbHandle write SetDbHandle;
    property TrHandle: TISC_TR_HANDLE read GetTrHandle write SetTrHandle;
    property Connection: TGDSConnection read FConnection write SetConnection;
    property Transaction: TGDSTransaction read FTransaction write SetTransaction;
    property ArrayDesc: IntPtr read FArrayDesc;
    property LowBound: integer read FLowBound write SetLowBound;
  end;

  TIBCArrayUtils = class
    class procedure SetArrayDesc(Obj: TCustomIBCArray; Desc: IntPtr);
    class function GetArrayIDPtr(Obj: TCustomIBCArray): PISC_QUAD;
  end;

implementation

uses
  CRParser, DAConsts, CRAccess,
{$IFNDEF UNIDACPRO}
  IBCParser, IBCError, IBCConsts;
{$ELSE}
  IBCParserUni, IBCErrorUni, IBCConstsUni;
{$ENDIF}

function ARRAYDESC_LENGTH(ArrayDescType: TARRAYDESCType): integer;
begin
  if ArrayDescType = adGDS7 then
    Result := SizeOfISC_ARRAY_DESC_V2
  else
    Result := SizeOfISC_ARRAY_DESC;
end;

procedure GetItemType(DescAccessor: TDescAccessor; var VariantType: Integer; var InternalType: Word; var InternalSize :Integer);
begin
  case DescAccessor.DataType of
    blr_double, blr_float, blr_d_float,
    blr_int64: begin
      VariantType := varDouble;
      InternalType := dtFloat;
      InternalSize := SizeOf(Double);
    end;
    blr_long: begin
      VariantType := varInteger;
      InternalType := dtInteger;
      InternalSize := SizeOf(Integer);
    end;
    blr_short: begin
      VariantType := varSmallint;
      InternalType := dtSmallInt;
      InternalSize := SizeOf(SmallInt);
    end;
    blr_boolean_dtype: begin
      VariantType := varBoolean;
      InternalType := dtBoolean;
      InternalSize := SizeOf(WordBool);
    end;
    blr_sql_date: begin
      VariantType := varDate;
      InternalType := dtDate;
      InternalSize := SizeOf(TDateTime);
    end;
    blr_sql_time: begin
      VariantType := varDate;
      InternalType := dtTime;
      InternalSize := SizeOf(TDateTime);
    end;
    blr_timestamp: begin
      VariantType := varDate;
      InternalType := dtDateTime;
      InternalSize := SizeOf(TDateTime);
    end;
    blr_cstring, blr_cstring2,
    blr_text, blr_text2,
    blr_Varying, blr_varying2: begin
      VariantType := varVariant;
      InternalType := dtString;
      InternalSize := DescAccessor.Length + 1;
    end
    else begin
      VariantType := varVariant;
      InternalType := dtUnknown;
      InternalSize := 0;
    end;  
  end;
end;

procedure AdaptArrayDesc(DescAccessor: TDescAccessor);
begin
  // blr_int64 always get as blr_double
  // blr_long get as blr_long or blr_double depend on Scale
  // blr_short get as blr_short or blr_double depend on Scale
  // blr_float get as blr_double for remove any bugs with percision
  if DescAccessor.DataType in [blr_int64, blr_float] then begin
    DescAccessor.DataType := blr_double;
    DescAccessor.Length := 8;
  end
  else
  if (DescAccessor.DataType in [blr_long, blr_short]) and (DescAccessor.Scale <> 0) then begin
    DescAccessor.DataType := blr_double;
    DescAccessor.Length := 8;
  end;
end;

{$IFNDEF VER6P}
type
  TVarType = Word;

function SafeArrayPtrOfIndex(VarArray: PVarArray; Indices: Pointer; var pvData: Pointer): HResult; stdcall;
  external 'oleaut32.dll' name 'SafeArrayPtrOfIndex';

function SafeArrayGetElement(VarArray: PVarArray; Indices, Data: Pointer): Integer; stdcall;
  external 'oleaut32.dll' name 'SafeArrayGetElement';

function SafeArrayPutElement(VarArray: PVarArray; Indices, Data: Pointer): Integer; stdcall;
  external 'oleaut32.dll' name 'SafeArrayPutElement';

function GetVarArray(const A: Variant): PVarArray;
begin
  if TVarData(A).VType and varArray = 0 then
    raise Exception.Create(SVarIsNotArray);
  if TVarData(A).VType and varByRef <> 0 then
    Result := PVarArray(TVarData(A).VPointer^)
  else
    Result := TVarData(A).VArray;
end;

function _VarArrayGet(var A: Variant; IndexCount: Integer;
  Indices: Integer): Variant; cdecl;
var
  VarArrayPtr: PVarArray;
  VarType: Integer;
  P: Pointer;
begin
  if TVarData(A).VType and varArray = 0 then
    raise Exception.Create(SVarIsNotArray);
  VarArrayPtr := GetVarArray(A);
  if VarArrayPtr^.DimCount <> IndexCount then
    raise Exception.create(SVarArrayBounds);
  VarType := TVarData(A).VType and varTypeMask;
  VarClear(Result);
  if VarType = varVariant then begin
    if SafeArrayPtrOfIndex(VarArrayPtr, @Indices, P) <> 0 then
      raise Exception.Create(SVarArrayBounds);
    Result := PVariant(P)^;
  end
  else begin
    if SafeArrayGetElement(VarArrayPtr, @Indices, @TVarData(Result).VPointer) <> 0 then
      raise Exception.Create(SVarArrayBounds);
    TVarData(Result).VType := VarType;
  end;
end;

function VarArrayGet(const A: Variant; const Indices: array of Integer): Variant;
asm
        PUSH    EBX

        MOV     EBX,ECX
        INC     EBX
        JLE     @@endLoop
@@loop:
        PUSH    [EDX+ECX*4].Integer
        DEC     ECX
        JNS     @@loop
@@endLoop:
        PUSH    EBX
        PUSH    EAX
        MOV     EAX,[EBP+8]
        PUSH    EAX
        CALL    _VarArrayGet
        LEA     ESP,[ESP+EBX*4+3*4]

        POP     EBX
end;

procedure VarStringToOleStr(var Dest: Variant; const Source: Variant);
var
  OleStrPtr: PWideChar;
begin
  OleStrPtr := StringToOleStr(string(TVarData(Source).VString));
  VarClear(Dest);
  TVarData(Dest).VType := varOleStr;
  TVarData(Dest).VOleStr := OleStrPtr;
end;

procedure _VarArrayPut(var A: Variant; const Value: Variant;
  IndexCount: Integer; Indices: Integer); cdecl;
type
  TAnyPutArrayProc = procedure (var A: Variant; const Value: Variant; Index: Integer);
var
  VarArrayPtr: PVarArray;
  VarType: Integer;
  P: Pointer;
  Temp: TVarData;
begin
  if TVarData(A).VType and varArray = 0 then
    raise Exception.Create(SVarIsNotArray);
  VarArrayPtr := GetVarArray(A);
  if VarArrayPtr^.DimCount <> IndexCount then
    raise Exception.Create(SVarArrayBounds);
  VarType := TVarData(A).VType and varTypeMask;
  if (VarType = varVariant) and (TVarData(Value).VType <> varString) then
  begin
    if SafeArrayPtrOfIndex(VarArrayPtr, @Indices, P) <> 0 then
      raise Exception.Create(SVarArrayBounds);
    PVariant(P)^ := Value;
  end else
  begin
    Temp.VType := varEmpty;
    try
      if VarType = varVariant then
      begin
        VarStringToOleStr(Variant(Temp), Value);
        P := @Temp;
      end else
      begin
        VarCast(Variant(Temp), Value, VarType);
        case VarType of
          varOleStr, varDispatch, varUnknown:
            P := Temp.VPointer;
        else
          P := @Temp.VPointer;
        end;
      end;
      if SafeArrayPutElement(VarArrayPtr, @Indices, P) <> 0 then
        raise Exception.Create(SVarArrayBounds);
    finally
      VarClear(Variant(Temp));
    end;
  end;
end;

procedure VarArrayPut(var A: Variant; const Value: Variant; const Indices: array of Integer);
asm
        PUSH    EBX

        MOV     EBX,[EBP+8]

        TEST    EBX,EBX
        JS      @@endLoop
@@loop:
        PUSH    [ECX+EBX*4].Integer
        DEC     EBX
        JNS     @@loop
@@endLoop:
        MOV     EBX,[EBP+8]
        INC     EBX
        PUSH    EBX
        PUSH    EDX
        PUSH    EAX
        CALL    _VarArrayPut
        LEA     ESP,[ESP+EBX*4+3*4]

        POP     EBX
end;

function VarIsType(const V: Variant; AVarType: TVarType): Boolean;
begin
  Result := VarType(V) = AVarType; 
end;
{$ENDIF}

{ TDescAccessor }

constructor TDescAccessor.Create(ArrayDescType: TARRAYDESCType; Desc: IntPtr);
begin
  inherited Create;
  FDesc := Desc;
  FArrayDescType := ArrayDescType;
end;

procedure TDescAccessor.Clear;
begin
  FillChar(FDesc, ARRAYDESC_LENGTH(FArrayDescType), $00);
end;

function TDescAccessor.GetDescVersion: SmallInt;
begin
  if FArrayDescType = adGDS then
    Result := 1
  else
    Result := Marshal.ReadInt16(FDesc, 0);
end;

procedure TDescAccessor.SetDescVersion(Value: SmallInt);
begin
  if FArrayDescType = adGDS7 then
    Marshal.WriteInt16(FDesc, 0, Value);
end;

function TDescAccessor.GetColumnName: string;
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 4
  else
    Offset := 8;
  Result := string(Marshal.PtrToStringAnsi(IntPtr(Integer(FDesc) + Offset)));
end;

procedure TDescAccessor.SetColumnName(Value: string);
var
  Len: integer;
  SPtr: IntPtr;
  MetaLen: integer;
  Offset: integer;
begin
  if Value <> '' then begin
    if FArrayDescType = adGDS then begin
      MetaLen := 32;
      Offset := 4;
    end
    else begin
      MetaLen := METADATALENGTH;
      Offset := 8;
    end;
    Len := {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Length(Value);
    if Len > MetaLen then
      Len := MetaLen;
    if Len > 0 then begin
      SPtr := Marshal.StringToHGlobalAnsi(AnsiString(Value));
      try
        CopyBuffer(SPtr, PtrOffset(FDesc, Offset), Len);
      finally
        Marshal.FreeCoTaskMem(SPtr);
      end;
      Marshal.WriteByte(PtrOffset(FDesc, Offset), Len, $00);
    end;
  end;
end;

function TDescAccessor.GetRelationName: string;
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 4 + 32
  else
    Offset := 8 + METADATALENGTH;
  Result := string(Marshal.PtrToStringAnsi(PtrOffset(FDesc, Offset)));
end;

procedure TDescAccessor.SetRelationName(Value: string);
var
  Len: integer;
  SPtr: IntPtr;
  MetaLen: integer;
  Offset: integer;
begin
  if Value <> '' then begin
    if FArrayDescType = adGDS then begin
      MetaLen := 32;
      Offset := 4 + 32
    end
    else begin
      MetaLen := METADATALENGTH;
      Offset := 8 + METADATALENGTH;
    end;
    Len := {$IFDEF CLR}Borland.Delphi.{$ENDIF}System.Length(Value);
    if Len > MetaLen then
      Len := MetaLen;
    if Len > 0 then begin
      SPtr := Marshal.StringToHGlobalAnsi(AnsiString(Value));
      try
        CopyBuffer(SPtr, PtrOffset(FDesc, Offset), Len);
      finally
        Marshal.FreeCoTaskMem(SPtr);
      end;
      Marshal.WriteByte(PtrOffset(FDesc, Offset), Len, $00);
    end;
  end;
end;

function TDescAccessor.GetLength: SmallInt;
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 2
  else
    Offset := 6;
  Result := Marshal.ReadInt16(FDesc, Offset);
end;

procedure TDescAccessor.SetLength(Value: SmallInt);
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 2
  else
    Offset := 6;
  Marshal.WriteInt16(FDesc, Offset, Value);
end;

function TDescAccessor.GetDimensions: SmallInt;
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 4 + 32 * 2
  else
    Offset := 8 + METADATALENGTH * 2;
  Result := Marshal.ReadInt16(FDesc, Offset);
end;

procedure TDescAccessor.SetDimensions(Value: SmallInt);
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 4 + 32 * 2
  else
    Offset := 8 + METADATALENGTH * 2;
  Marshal.WriteInt16(FDesc, Offset, Value);
end;

function TDescAccessor.GetHighBound(Dimension: integer): SmallInt;
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 8 + (32 * 2) + (Dimension * SizeOfISC_ARRAY_BOUND) + 2
  else
    Offset := 12 + (METADATALENGTH * 2) + (Dimension * SizeOfISC_ARRAY_BOUND) + 2;
  Result := Marshal.ReadInt16(FDesc, Offset);
end;

procedure TDescAccessor.SetHighBound(Dimension: integer; Value: SmallInt);
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 8 + (32 * 2) + (Dimension * SizeOfISC_ARRAY_BOUND) + 2
  else
    Offset := 12 + (METADATALENGTH * 2) + (Dimension * SizeOfISC_ARRAY_BOUND) + 2;
  Marshal.WriteInt16(FDesc, Offset, Value);
end;

function TDescAccessor.GetLowBound(Dimension: integer): SmallInt;
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 8 + (32 * 2) + (Dimension * SizeOfISC_ARRAY_BOUND)
  else
    Offset := 12 + (METADATALENGTH * 2) + (Dimension * SizeOfISC_ARRAY_BOUND);
  Result := Marshal.ReadInt16(FDesc, Offset)
end;

procedure TDescAccessor.SetLowBound(Dimension: integer; Value: SmallInt);
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 8 + (32 * 2) + (Dimension * SizeOfISC_ARRAY_BOUND)
  else
    Offset := 12 + (METADATALENGTH * 2) + (Dimension * SizeOfISC_ARRAY_BOUND);
  Marshal.WriteInt16(FDesc, Offset, Value);
end;

function TDescAccessor.GetDataType: byte;
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 0
  else
    Offset := 2;
  Result := Marshal.ReadByte(FDesc, Offset);
end;

procedure TDescAccessor.SetDataType(Value: byte);
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 0
  else
    Offset := 2;
  Marshal.WriteByte(FDesc, Offset, Value);
end;

function TDescAccessor.GetScale: ShortInt;
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 1
  else
    Offset := 4;
  Result := ShortInt(Marshal.ReadByte(FDesc, Offset));
end;

procedure TDescAccessor.SetScale(Value: ShortInt);
var
  Offset: integer;
begin
  if FArrayDescType = adGDS then
    Offset := 1
  else
    Offset := 4;
  Marshal.WriteByte(FDesc, Offset, Value);
end;

procedure TDescAccessor.SetDesc(Desc: IntPtr);
begin
  FDesc := Desc;
end;

{ TCustomIBCArray }

constructor TCustomIBCArray.Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
begin
  inherited Create;

  Init(DbHandle, TrHandle);
end;

constructor TCustomIBCArray.Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE; TableName, ColumnName: string);
begin
  inherited Create;

  if TableName = '' then
    raise Exception.Create(SInvalidTable);
  if ColumnName = '' then
    raise Exception.Create(SInvalidColumn);
  FTableName := TableName;
  FColumnName := ColumnName;
  //GetArrayInfo;

  Init(DbHandle, TrHandle);
end;

constructor TCustomIBCArray.Create(Connection: TGDSConnection; Transaction: TGDSTransaction; TableName, ColumnName: string);
begin
  inherited Create;

  FConnection := Connection;
  FTransaction := Transaction;
  if TableName = '' then
    raise Exception.Create(SInvalidTable);
  if ColumnName = '' then
    raise Exception.Create(SInvalidColumn);
  FTableName := TableName;
  FColumnName := ColumnName;
  //GetArrayInfo;

  if (FConnection <> nil) and (FTransaction <> nil) then
    Init(FConnection.GetDatabaseHandle, FTransaction.GetTransactionHandle)
  else
    Init(nil, nil);
end;

procedure TCustomIBCArray.Init(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE);
var
  ADType: TARRAYDESCType;
begin
  if GDS.Version >= 7 then
    ADType := adGDS7
  else
    ADType := adGDS;
  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FDbHandle := Marshal.AllocHGlobal(SizeOf(TISC_DB_HANDLE));
  Marshal.WriteIntPtr(FDbHandle, DbHandle);
  FTrHandle := Marshal.AllocHGlobal(SizeOf(TISC_TR_HANDLE));
  Marshal.WriteIntPtr(FTrHandle, TrHandle);
  FArrayDesc := Marshal.AllocHGlobal(ARRAYDESC_LENGTH(ADType));
  FDescAccessor := TDescAccessor.Create(ADType, FArrayDesc);
  FDescAccessor.Clear;
  FArrayID := Marshal.AllocHGlobal(SizeOf(TISC_QUAD));
  Marshal.WriteInt64(FArrayID, 0);
  FArrayBuffer := nil;
  FCached := False;
  FInternalItemType := dtUnknown;
  FNativeDesc := True;
  FModified := False;
  FFirstWrite := True;
end;

destructor TCustomIBCArray.Destroy;
begin
  FreeBuffer;
  Marshal.FreeHGlobal(FArrayID);
  FDescAccessor.Free;
  if (FArrayDesc <> nil) and FNativeDesc then
    Marshal.FreeHGlobal(FArrayDesc);
  Marshal.FreeHGlobal(FStatusVector);
  Marshal.FreeHGlobal(FDbHandle);
  Marshal.FreeHGlobal(FTrHandle);
  ObjectType.Free;

  inherited Destroy;
end;

procedure TCustomIBCArray.Disconnect;
begin
  SetArrayDesc(nil);
  ArrayID := 0;
end;

function TCustomIBCArray.GetDbHandle: TISC_DB_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FDbHandle);
end;

procedure TCustomIBCArray.SetDbHandle(Value: TISC_DB_HANDLE);
begin
  if GetDbHandle <> Value then begin
    SetArrayID(0);
    Marshal.WriteIntPtr(FDbHandle, Value);
  end;
end;

function TCustomIBCArray.GetTrHandle: TISC_TR_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FTrHandle);
end;

procedure TCustomIBCArray.SetTrHandle(Value: TISC_TR_HANDLE);
begin
  if GetTrHandle <> Value then begin
    SetArrayID(0);
    Marshal.WriteIntPtr(FTrHandle, Value);
  end;
end;

procedure TCustomIBCArray.SetConnection(Value: TGDSConnection);
begin
  if Value <> nil then
    SetDbHandle(Value.GetDatabaseHandle);
  FConnection := Value;    
end;

procedure TCustomIBCArray.SetTransaction(Value: TGDSTransaction);
begin
  if Value <> nil then
    SetTrHandle(Value.GetTransactionHandle);
  FTransaction := Value;
end;

function TCustomIBCArray.GetArrayType: TIBCArrayType;
begin
  Result := TIBCArrayType(ObjectType);
end;

procedure TCustomIBCArray.SetArrayType(Value: TIBCArrayType);
begin
  SetObjectType(Value);
  if Value <> nil then
    SetArrayDesc(Value.ArrayDesc);
end;

function TCustomIBCArray.GetGDS: TGDS;
begin
  if FConnection <> nil then
    Result := FConnection.GDS
  else
    Result := GDSList.GetGDS('');
end;

function TCustomIBCArray.GetArrayID: TISC_QUAD;
begin
  Result := Marshal.ReadInt64(FArrayID);
end;

procedure TCustomIBCArray.SetArrayID(const Value: TISC_QUAD);
begin
  Marshal.WriteInt64(FArrayID, Value);
end;

procedure TCustomIBCArray.SetColumnName(const Value: string);
begin
  FColumnName := Value;
  FDescAccessor.ColumnName := FColumnName;
end;

procedure TCustomIBCArray.SetTableName(const Value: string);
begin
  FTableName := Value;
  FDescAccessor.RelationName := FTableName;
end;

procedure TCustomIBCArray.AllocBuffer;
var
  Size: integer;
begin
  FreeBuffer;
  Size := GetCachedSize;
  FArrayBuffer := Marshal.AllocHGlobal(Size);
  FillChar(FArrayBuffer, Size, $00);
end;

procedure TCustomIBCArray.FreeBuffer;
begin
  if FArrayBuffer <> nil then begin
    Marshal.FreeHGlobal(FArrayBuffer);
    FArrayBuffer := nil;
  end;
end;

procedure TCustomIBCArray.SetCached(Value: boolean);
begin
  if FCached <> Value then begin
    if (FArrayBuffer <> nil) and (not Value) then
      raise Exception.Create(SCannotDisableArrayCache)
    else
    if GetArrayID <> 0 then begin
      GetArrayInfo;
      ReadArray;
    end;
    FCached := Value;
  end;
end;

procedure TCustomIBCArray.SetArrayDesc(Desc: IntPtr);
var
  i: integer;
begin
  if (FArrayDesc <> nil) and FNativeDesc then begin
    Marshal.FreeHGlobal(FArrayDesc);
    FArrayDesc := nil;
    FNativeDesc := False;
  end;
  FArrayDesc := Desc;
  FDescAccessor.SetDesc(FArrayDesc);
  if FArrayDesc <> nil then begin
    AdaptArrayDesc(FDescAccessor);
    FDescAccessor.Scale := ABS(FDescAccessor.Scale);
    ArrayDimensions := FDescAccessor.Dimensions;
    for i := 0 to FArrayDimensions - 1 do begin
      FArrayHighBounds[i] := FDescAccessor.GetHighBound(i);
      FArrayLowBounds[i] := FDescAccessor.GetLowBound(i);
    end;
    FInternalItemType := GetInternalItemType;
  end;
end;

function TCustomIBCArray.GetArrayIDPtr: PISC_QUAD;
begin
  Result := FArrayID;
end;

procedure TCustomIBCArray.CreateTemporaryArray;
begin
  AllocBuffer;
  SetArrayID(0);
  FDescAccessor.DescVersion := ARR_DESC_CURRENT_VERSION;
  WriteArray;
  if not FCached then
    FreeBuffer;
end;

procedure TCustomIBCArray.Check(Status: ISC_STATUS);
var
  SqlErrMsg, Msg: _string;
  ErrorNumber, ErrorCode: integer;
begin
  if Status > 0 then begin
    GDS.Busy;
    ErrorCode := GDS.isc_sqlcode(FStatusVector);
    GDS.Release;
    GDS.GetIBError(ErrorCode, False, ErrorNumber, FStatusVector, Msg, SqlErrMsg);
    raise EIBCError.Create(ErrorCode, ErrorNumber, Msg, SqlErrMsg);
  end;
end;

procedure TCustomIBCArray.CheckBounds(Bounds: array of integer);
var
  i: integer;
begin
  for i := 0 to ((High(Bounds) + 1) div 2) - 1 do begin
    if Bounds[i * 2] < FDescAccessor.LowBound[i] then
      raise Exception.CreateFmt(SArrayLowBoundError, [Bounds[i * 2]]);

    if Bounds[i * 2 + 1] > FDescAccessor.HighBound[i] then
      raise Exception.CreateFmt(SArrayHighBoundError,  [Bounds[i * 2 + 1]]);
  end;
end;

procedure TCustomIBCArray.CheckCachedIndices(Indices: array of integer);
var
  i: integer;
begin
  if Length(Indices) <> FDescAccessor.Dimensions then
    raise Exception.Create(SArrayDimensionError);
  for i := 0 to Length(Indices) - 1 do
    if (Indices[i] > FDescAccessor.HighBound[i]) or (Indices[i] < FDescAccessor.LowBound[i]) then
      raise Exception.CreateFmt(SArrayIndexError, [Indices[i]]);
end;

procedure TCustomIBCArray.CheckArrayIndices(Indices: array of integer);
var
  i: integer;
begin
  if Length(Indices) <> FArrayDimensions then
    raise Exception.Create(SArrayDimensionError);
  for i := 0 to Length(Indices) - 1 do
    if (Indices[i] > FArrayHighBounds[i]) or (Indices[i] < FArrayLowBounds[i]) then
      raise Exception.CreateFmt(SArrayIndexError, [Indices[i]]);
end;

procedure TCustomIBCArray.GetArrayInfo;
var
  pTableName, pColumnName: IntPtr;
  Res: ISC_STATUS;
  i: integer;
begin
  FDescAccessor.Clear;
  pTableName := Marshal.StringToHGlobalAnsi(AnsiString(FTableName));
  pColumnName := Marshal.StringToHGlobalAnsi(AnsiString(FColumnName));
  try
    GDS.Busy;
    Res := GDS.isc_array_lookup_bounds(FStatusVector, FDbHandle, FTrHandle,
      pTableName, pColumnName, FArrayDesc);
    GDS.Release;
    Check(Res);
    FDescAccessor.Scale := ABS(FDescAccessor.Scale);
    ArrayDimensions := FDescAccessor.Dimensions;
    for i := 0 to FArrayDimensions - 1 do begin
      FArrayHighBounds[i] := FDescAccessor.GetHighBound(i);
      FArrayLowBounds[i] := FDescAccessor.GetLowBound(i);
    end;
    FInternalItemType := GetInternalItemType;
  finally
    Marshal.FreeCoTaskMem(pTableName);
    Marshal.FreeCoTaskMem(pColumnName);
  end;
end;

function TCustomIBCArray.GetArraySize: integer;
var
  ItemCount: integer;
  i: integer;
begin
  Result := 1;
  for i := 0 to FArrayDimensions - 1 do begin
    ItemCount := ArrayHighBound[i] - ArrayLowBound[i] + 1;
    Result := Result * ItemCount;
  end;
  Result := Result * GetItemSize;
end;

function TCustomIBCArray.GetSliceSize(Bounds: array of integer): integer;
var
  ItemCount: integer;
  i: integer;
begin
  Result := 1;
  for i := 0 to ((High(Bounds) + 1) div 2) - 1 do begin
    ItemCount := Bounds[i * 2 + 1] - Bounds[i * 2] + 1;
    Result := Result * ItemCount;
  end;
  Result := Result * GetItemSize;
end;

function TCustomIBCArray.GetCachedSize: integer;
var
  ItemCount: integer;
  i: integer;
begin
  Result := 1;
  for i := 0 to FDescAccessor.Dimensions - 1 do begin
    ItemCount := FDescAccessor.HighBound[i] - FDescAccessor.LowBound[i] + 1;
    Result := Result * ItemCount;
  end;
  Result := Result * GetItemSize;
end;

procedure TCustomIBCArray.ReadArray;
var
  Length: IntPtr;
  Res: ISC_STATUS;
begin
  AllocBuffer;
  Length := Marshal.AllocHGlobal(SizeOf(integer));
  try
    Marshal.WriteInt32(Length, GetCachedSize);
    GDS.Busy;
    Res := GDS.isc_array_get_slice(FStatusVector, FDbHandle, FTrHandle, FArrayID,
      FArrayDesc, FArrayBuffer, Length);
    GDS.Release;
    Check(Res);
  finally
    Marshal.FreeHGlobal(Length);
  end;
end;

procedure TCustomIBCArray.ReadArraySlice(Bounds: array of integer);
var
  i: integer;
begin
  CheckBounds(Bounds);
  FDescAccessor.Dimensions := (High(Bounds) + 1) div 2;
  for i := 0 to FDescAccessor.Dimensions - 1 do begin
    FDescAccessor.LowBound[i] := Bounds[i * 2];
    FDescAccessor.HighBound[i] := Bounds[i * 2 + 1];
  end;
  ReadArray;
end;

procedure TCustomIBCArray.ReadArrayItem(Indices: array of integer);
var
  Bounds: array of integer;
  i: integer;
begin
  SetLength(Bounds, Length(Indices) * 2);
  for i := 0 to Length(Indices) - 1 do begin
    Bounds[i * 2] := Indices[i];
    Bounds[i * 2 + 1] := Indices[i];
  end;
  ReadArraySlice(Bounds);
end;

procedure TCustomIBCArray.WriteArray;
begin
  PutArray(0, GetCachedSize);
end;

procedure TCustomIBCArray.WriteArraySlice(Bounds: array of integer);
var
  TempDesc: IntPtr;
  Offset, Size: integer;
  i: integer;
  ADType: TARRAYDESCType;
begin
  if FCached then begin
    CheckBounds(Bounds);

    if GDS.Version >= 7 then
      ADType := adGDS7
    else
      ADType := adGDS;
    TempDesc := Marshal.AllocHGlobal(ARRAYDESC_LENGTH(ADType));
    try
      CopyBuffer(FArrayDesc, TempDesc, ARRAYDESC_LENGTH(ADType));
      try
        FDescAccessor.Dimensions := (High(Bounds) + 1) div 2;
        for i := 0 to FDescAccessor.Dimensions - 1 do begin
          FDescAccessor.LowBound[i] := Bounds[i * 2];
          FDescAccessor.HighBound[i] := Bounds[i * 2 + 1];
        end;
        Offset := GetSliceOffset(Bounds);
        Size := GetSliceSize(Bounds);
        PutArray(Offset, Size);
      finally
        CopyBuffer(TempDesc, FArrayDesc, ARRAYDESC_LENGTH(ADType));
      end;
    finally
      Marshal.FreeHGlobal(TempDesc);
    end;
  end;  
end;

procedure TCustomIBCArray.ClearArray;
begin
  if not Cached then
    AllocBuffer
  else
    FillChar(FArrayBuffer, GetCachedSize, $00);
  if not Cached then begin
    WriteArray;
    FreeBuffer;
  end;
end;

procedure TCustomIBCArray.PutArray(Offset: integer; Size: integer);
var
  Length: IntPtr;
  Res: ISC_STATUS;
begin
  if FArrayBuffer <> nil then begin
    Length := Marshal.AllocHGlobal(SizeOf(integer));
    try
      if (not FFirstWrite) and FCached then
        SetArrayID(0);
      Marshal.WriteInt32(Length, Size);
      GDS.Busy;
      Res := GDS.isc_array_put_slice(FStatusVector, FDbHandle, FTrHandle, FArrayID,
        FArrayDesc, PtrOffset(FArrayBuffer, Offset), Length);
      GDS.Release;
      Check(Res);
      FModified := False;
      FFirstWrite := False;
    finally
      Marshal.FreeHGlobal(Length);
    end;
  end;
end;

procedure TCustomIBCArray.SetArrayDimensions(Value: integer);
const
  MAX_DIMENSIONS = 16;
begin
  if (Value < 0) or (Value > MAX_DIMENSIONS) then
    raise Exception.Create(SInvalidDimension);
  FArrayDimensions := Value;
  SetLength(FArrayHighBounds, FArrayDimensions);
  SetLength(FArrayLowBounds, FArrayDimensions);
  FDescAccessor.Dimensions := FArrayDimensions;
end;

function TCustomIBCArray.GetArrayHighBound(Dimension: integer): integer;
begin
  if FArrayDimensions <> Length(FArrayHighBounds) then
    raise Exception.Create(SInvalidDimension);
  if (Dimension < 0) or (Dimension >= FArrayDimensions) then
    raise Exception.Create(SInvalidDimension);
  Result := FArrayHighBounds[Dimension];
end;

procedure TCustomIBCArray.SetArrayHighBound(Dimension: integer; Value: integer);
begin
  if FArrayDimensions <> Length(FArrayHighBounds) then
    raise Exception.Create(SInvalidDimension);
  if (Dimension < 0) or (Dimension >= FArrayDimensions) then
    raise Exception.Create(SInvalidDimension);
  FArrayHighBounds[Dimension] := Value;
  FDescAccessor.HighBound[Dimension] := Value;
end;

function TCustomIBCArray.GetArrayLowBound(Dimension: integer): integer;
begin
  if FArrayDimensions <> Length(FArrayLowBounds) then
    raise Exception.Create(SInvalidDimension);
  if (Dimension < 0) or (Dimension >= FArrayDimensions) then
    raise Exception.Create(SInvalidDimension);
  Result := FArrayLowBounds[Dimension];
end;

procedure TCustomIBCArray.SetArrayLowBound(Dimension: integer; Value: integer);
begin
  if FArrayDimensions <> Length(FArrayLowBounds) then
    raise Exception.Create(SInvalidDimension);
  if (Dimension < 0) or (Dimension >= FArrayDimensions) then
    raise Exception.Create(SInvalidDimension);
  FArrayLowBounds[Dimension] := Value;
  FDescAccessor.LowBound[Dimension] := Value;
end;

function TCustomIBCArray.GetCachedDimensions: integer;
begin
  Result := FDescAccessor.Dimensions;
end;

function TCustomIBCArray.GetCachedHighBound(Dimension: integer): integer;
begin
  Result := FDescAccessor.HighBound[Dimension];
end;

function TCustomIBCArray.GetCachedLowBound(Dimension: integer): integer;
begin
  Result := FDescAccessor.LowBound[Dimension];
end;

function TCustomIBCArray.GetInternalItemType: Word;
var
  VariantType: integer;
  InternalSize: integer;
begin
  GetItemType(FDescAccessor, VariantType, Result, InternalSize);
end;

function TCustomIBCArray.GetItemScale: integer;
begin
  Result := FDescAccessor.Scale;
end;

procedure TCustomIBCArray.SetItemScale(Value: integer);
begin
  FDescAccessor.Scale := Value;
end;

function TCustomIBCArray.GetItems: Variant;
var
  Bounds: array of integer;
  i: integer;
begin
  if (not FCached) or (FArrayBuffer = nil) then
    ReadArray;
  SetLength(Bounds, FDescAccessor.Dimensions * 2);
  for i := 0 to FDescAccessor.Dimensions - 1 do begin
    Bounds[i * 2] := FDescAccessor.LowBound[i];
    Bounds[i * 2 + 1] := FDescAccessor.HighBound[i];
  end;
  Result := BufToVarArray(Bounds);
  if not FCached then
    FreeBuffer
end;

function TCustomIBCArray.GetItemsSlice(Bounds: array of integer): Variant;
begin
  if (not FCached) or (FArrayBuffer = nil) then
    ReadArraySlice(Bounds);
  Result := BufToVarArray(Bounds);
  if not FCached then
    FreeBuffer;
end;

procedure TCustomIBCArray.SetItemsSlice(const Values: variant);
var
  Bounds: array of integer;
  i: integer;
begin
  if VarIsEmpty(Values) or VarIsNull(Values) then
    ClearArray
  else
  if VarIsArray(Values) then begin
    if not FCached then
      AllocBuffer;
    SetLength(Bounds, VarArrayDimCount(Values) * 2);
    for i := 0 to ((High(Bounds) + 1) div 2) - 1 do begin
      Bounds[i * 2] := VarArrayLowBound(Values, i + 1);
      Bounds[i * 2 + 1] := VarArrayHighBound(Values, i + 1);
    end;
    CheckBounds(Bounds);
    VarArrayToBuf(Values);
    if not FCached then begin
      WriteArraySlice(Bounds);
      FreeBuffer;
    end
    else
      FModified := True;
  end
  else
    raise Exception.Create(SVarIsNotArray);
end;

procedure TCustomIBCArray.SetItems(const Values: Variant);
var
  i: integer;
begin
  if VarIsEmpty(Values) or VarIsNull(Values) then
    ClearArray
  else
  if VarIsArray(Values) then begin
    if (not FCached) or (FArrayBuffer = nil) then
      AllocBuffer;
    if FDescAccessor.Dimensions <> VarArrayDimCount(Values) then
      raise Exception.Create(SArrayDimensionError);
    for i := 0 to FDescAccessor.Dimensions - 1 do begin
      if (FDescAccessor.HighBound[i] - FDescAccessor.LowBound[i] + 1) <>
        (VarArrayHighBound(Values, i + 1) - VarArrayLowBound(Values, i + 1) + 1) then
        raise Exception.Create(SInvalidDimension);
    end;
    VarArrayToBuf(Values);
    if not FCached then begin
      WriteArray;
      FreeBuffer;
    end
    else
      FModified := True;
  end
  else
    raise Exception.Create(SVarIsNotArray);
end;

function TCustomIBCArray.GetItemCount(Bounds: array of integer): integer;
var
  i: integer;
begin
  Result := 1;
  for i := 0 to ((High(Bounds) + 1) div 2) - 1 do
    Result := Result * (Bounds[i * 2 + 1] - Bounds[i * 2] + 1);
end;

function TCustomIBCArray.GetItemSize: integer;
begin
  Result := FDescAccessor.Length;
  case FDescAccessor.DataType of
    blr_varying, blr_varying2:
      Inc(Result, 2);
  end;
end;

procedure TCustomIBCArray.SetItemSize(Value: integer);
begin
  FDescAccessor.Length := Value;
end;

function TCustomIBCArray.GetItemVarType: integer;
var
  InternalType: Word;
  InternalSize: integer;
begin
  GetItemType(FDescAccessor, Result, InternalType, InternalSize);
end;

function TCustomIBCArray.GetItemOffset(Indices: array of integer): integer;

  function GetItemsCount(Index, SubCount: integer): integer;
  begin
    Result := (Indices[Index] - FDescAccessor.LowBound[Index]) * SubCount;
    if Index = 0 then
      Exit;
    SubCount := SubCount * (FDescAccessor.HighBound[Index] - FDescAccessor.LowBound[Index] + 1);
    Result := Result + GetItemsCount(Index - 1, SubCount);
  end;

begin
  if Length(Indices) > 0 then
    Result := GetItemsCount(High(Indices), 1)
  else
    Result := 0;
  Result := Result * GetItemSize;
end;

function TCustomIBCArray.GetSliceOffset(Bounds: array of integer): integer;
var
  i: integer;
  SliceItemCount: integer;

begin
  Result := 0;
  i := ((High(Bounds) + 1) div 2) - 1;
  SliceItemCount := Bounds[i * 2 + 1] - Bounds[i * 2] + 1;
  for i := ((High(Bounds) + 1) div 2) - 1 downto 0 do
    Result := Result + (Bounds[i * 2] - FArrayLowBounds[i]) * SliceItemCount;
  Result := Result * GetItemSize;
end;

function TCustomIBCArray.BufItemToVariant(Offset: integer): variant;
var
  tm_date: TCTimeStructure;
begin
  case FDescAccessor.DataType of
    blr_long:
      Result := Marshal.ReadInt32(FArrayBuffer, Offset);
    blr_short:
      Result := Marshal.ReadInt16(FArrayBuffer, Offset);
    blr_float:
    {$IFDEF CLR}
      Result := BitConverter.ToSingle(BitConverter.GetBytes(Marshal.ReadInt32(FArrayBuffer, Offset)), 0);
    {$ELSE}
      Result := Single(PtrOffset(FArrayBuffer, Offset)^);
    {$ENDIF}
    blr_double:
      Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FArrayBuffer, Offset));
    blr_int64:
      if (FDescAccessor.Scale <> 0) then
        Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FArrayBuffer, Offset))
      else
      {$IFDEF VER6P}
        Result := Marshal.ReadInt64(FArrayBuffer, Offset);
      {$ELSE}
        begin
          TVarData(Result).VType := varDecimal;
          TVarDataD6(Result).VInt64 := Int64(PtrOffset(FArrayBuffer, Offset)^);
        end;
      {$ENDIF}
    blr_boolean_dtype:
      Result := Marshal.ReadInt16(FArrayBuffer, Offset) = 1;
    blr_sql_date: begin
      GDS.Busy;
      GDS.isc_decode_sql_date(PtrOffset(FArrayBuffer, Offset), tm_date);
      GDS.Release;
      Result := EncodeDate(tm_date.tm_year + 1900, tm_date.tm_mon + 1, tm_date.tm_mday);
    end;
    blr_sql_time: begin
      GDS.Busy;
      GDS.isc_decode_sql_time(PtrOffset(FArrayBuffer, Offset), tm_date);
      GDS.Release;
      Result := EncodeTime(tm_date.tm_hour, tm_date.tm_min, tm_date.tm_sec, 0);
    end;
    blr_timestamp: begin
      GDS.Busy;
      GDS.isc_decode_date(PtrOffset(FArrayBuffer, Offset), tm_date);
      GDS.Release;
      Result := EncodeDate(tm_date.tm_year + 1900, tm_date.tm_mon + 1, tm_date.tm_mday);
      if Result >= 0 then
        Result := Result + EncodeTime(tm_date.tm_hour, tm_date.tm_min, tm_date.tm_sec, 0)
      else
        Result := Result - EncodeTime(tm_date.tm_hour, tm_date.tm_min, tm_date.tm_sec, 0);
    end;
    blr_text, blr_text2, blr_cstring, blr_cstring2:
      Result := Marshal.PtrToStringAnsi(PtrOffset(FArrayBuffer, Offset), FDescAccessor.Length);
    blr_varying, blr_varying2:
      Result := Marshal.PtrToStringAnsi(PtrOffset(FArrayBuffer, Offset));
    else
      Result := Unassigned;
  end;
end;

function TCustomIBCArray.BufToVarArray(Bounds: array of integer): variant;
var
  Indices: array of integer;
  Offset: integer;
  i: integer;

  procedure UpdateIndices;
  var
    i: integer;
  begin
    for i := VarArrayDimCount(Result) - 1 downto 0 do
      if Indices[i] = VarArrayHighBound(Result, i + 1) then
        Indices[i] := VarArrayLowBound(Result, i + 1)
      else begin
        Indices[i] := Indices[i] + 1;
        Break;
      end;
  end;

begin
  CheckBounds(Bounds);
  Result := VarArrayCreate(Bounds, GetItemVarType);
  SetLength(Indices, FDescAccessor.Dimensions);
  for i := 0 to FDescAccessor.Dimensions - 1 do
    Indices[i] := VarArrayLowBound(Result, i + 1);
  Offset := GetSliceOffset(Bounds);
  for i := 0 to GetItemCount(Bounds) - 1 do begin
    VarArrayPut(Result, BufItemToVariant(Offset), Indices);
    UpdateIndices;
    Inc(Offset, GetItemSize);
  end;
end;

procedure TCustomIBCArray.VariantToBufItem(const Value: variant; Offset: integer);
var
  SPtr: IntPtr;
  Len: integer;
begin
  case FDescAccessor.DataType of
    blr_long:
      if VarIsType(Value, varInteger) then
        Marshal.WriteInt32(FArrayBuffer, Offset, Value);
    blr_short:
      if VarIsType(Value, varSmallint) then
        Marshal.WriteInt16(FArrayBuffer, Offset, SmallInt(Value));
    blr_float:
      if VarIsType(Value, varSingle) then
      {$IFDEF CLR}
        Marshal.WriteInt32(FArrayBuffer, Offset, BitConverter.ToInt32(BitConverter.GetBytes(Single(Value)), 0));
      {$ELSE}
        Single(PtrOffset(FArrayBuffer, Offset)^) := Single(Value);
      {$ENDIF}
    blr_double:
      if VarIsType(Value, varDouble) then
      {$IFDEF CLR}
        Marshal.WriteInt64(FArrayBuffer, Offset, BitConverter.DoubleToInt64Bits(Value));
      {$ELSE}
        Double(PtrOffset(FArrayBuffer, Offset)^) := Double(Value);
      {$ENDIF}
    blr_int64:
        Marshal.WriteInt64(FArrayBuffer, Offset, BitConverter.DoubleToInt64Bits(Value));
    blr_boolean_dtype:
      if VarIsType(Value, varBoolean) then
        Marshal.WriteInt16(FArrayBuffer, Offset, SmallInt(WordBool(Value)));
    blr_sql_date:
      DateTimeToSQLDate(TDateTime(Value), PtrOffset(FArrayBuffer, Offset));
    blr_sql_time:
      DateTimeToSQLTime(TDateTime(Value), PtrOffset(FArrayBuffer, Offset));
    blr_timestamp:
      DateTimeToSQLTimeStamp(TDateTime(Value), PtrOffset(FArrayBuffer, Offset));
    blr_text, blr_text2, blr_cstring, blr_cstring2, blr_varying, blr_varying2:
      if Value <> '' then begin
        Len := Length(Value);
        if Len > FDescAccessor.Length then
          Len := FDescAccessor.Length;
        if Len > 0 then begin
          SPtr := Marshal.StringToHGlobalAnsi(AnsiString(Value));
          try
            FillChar(PtrOffset(FArrayBuffer, Offset), FDescAccessor.Length, $00);
            CopyBuffer(SPtr, PtrOffset(FArrayBuffer, Offset), Len);
          finally
            Marshal.FreeCoTaskMem(SPtr);
          end;
          Marshal.WriteByte(PtrOffset(FArrayBuffer, Offset), Len, $00);
        end;
      end;
    else
      raise Exception.Create(SDataTypeNotSupported);
  end;
end;

procedure TCustomIBCArray.VarArrayToBuf(const Values: Variant);
var
  i: integer;
  Value: Variant;
  ItemCount: integer;
  Indices: array of integer;
  VarDimesion: integer;

  procedure UpdateIndices;
  var
    i: integer;
  begin
    for i := VarArrayDimCount(Values) - 1 downto 0 do
      if Indices[i] = VarArrayHighBound(Values, i + 1) then
        Indices[i] := VarArrayLowBound(Values, i + 1)
      else begin
        Indices[i] := Indices[i] + 1;
        Break;
      end;
  end;

begin
  VarDimesion := VarArrayDimCount(Values);
  SetLength(Indices, VarDimesion);
  ItemCount := 1;
  for i := 0 to VarDimesion - 1 do begin
    ItemCount := ItemCount * (VarArrayHighBound(Values, i + 1) - VarArrayLowBound(Values, i + 1) + 1);
    Indices[i] := VarArrayLowBound(Values, i + 1);
  end;
  if FArrayBuffer = nil then
    AllocBuffer;
  for i := 0 to ItemCount - 1 do begin
    Value := VarArrayGet(Values, Indices);
    VariantToBufItem(Value, GetItemOffset(Indices));
    UpdateIndices;
  end;
end;

procedure TCustomIBCArray.Assign(Source: TCustomIBCArray);
begin
  FCached := Source.FCached;
  if GetCachedSize <> Source.GetCachedSize then
    FreeBuffer;
  DbHandle := Source.DbHandle;
  TrHandle := Source.TrHandle;
  FTableName := Source.FTableName;
  FColumnName := Source.FColumnName;
  ArrayID := Source.ArrayID;
  SetArrayDesc(Source.FArrayDesc);
  if Source.FArrayBuffer <> nil then begin
    if FArrayBuffer = nil then
      AllocBuffer;
    MemUtils.CopyBuffer(Source.FArrayBuffer, FArrayBuffer, GetCachedSize);
  end;
end;

function TCustomIBCArray.GetIsNull: boolean;
begin
  Result := (GetArrayID = 0) and (FArrayBuffer = nil);
end;

procedure TCustomIBCArray.SetIsNull(const Value: boolean);
begin
  SetArrayID(0);
  FreeBuffer;
end;

function TCustomIBCArray.GetItemValue(Indices: array of integer): Variant;
var
  Offset: integer;
begin
  if not FCached then begin
    CheckArrayIndices(Indices);
    ReadArrayItem(Indices);
    Offset := 0;
  end
  else begin
    CheckCachedIndices(Indices);
    if FArrayBuffer = nil then
      ReadArray;
    Offset := GetItemOffset(Indices);
  end;
  Result := BufItemToVariant(Offset);
  if not FCached then
    FreeBuffer;
end;

procedure TCustomIBCArray.SetItemValue(Indices: array of integer; Value: Variant);
var
  Offset: integer;
  TempDesc: IntPtr;
  i: integer;
  ADType: TARRAYDESCType;
begin
  TempDesc := nil;
  ADType := adGDS7;
  if not FCached then begin
    if GDS.Version >= 7 then
      ADType := adGDS7
    else
      ADType := adGDS;
    TempDesc := Marshal.AllocHGlobal(ARRAYDESC_LENGTH(ADType));
    CopyBuffer(FArrayDesc, TempDesc, ARRAYDESC_LENGTH(ADType));
    FDescAccessor.Dimensions := Length(Indices);
    for i := 0 to FDescAccessor.Dimensions - 1 do begin
      FDescAccessor.LowBound[i] := Indices[i];
      FDescAccessor.HighBound[i] := Indices[i];
    end;
    AllocBuffer;
    Offset := 0;
  end
  else begin
    if FArrayBuffer = nil then
      AllocBuffer;
    Offset := GetItemOffset(Indices);
  end;
  VariantToBufItem(Value, Offset);
  if not FCached then begin
    PutArray(0, GetItemSize);
    if TempDesc <> nil then begin
      CopyBuffer(TempDesc, FArrayDesc, ARRAYDESC_LENGTH(ADType));
      Marshal.FreeHGlobal(TempDesc);
    end;
    FreeBuffer;
  end
  else
    FModified := True;
end;

function TCustomIBCArray.GetItemAsString(Indices: array of integer): string;
begin
  if FInternalItemType = dtString then
    Result := GetItemValue(Indices)
  else
    Result := '';
end;

procedure TCustomIBCArray.SetItemAsString(Indices: array of integer; Value: string);
begin
  if FInternalItemType = dtString then
    SetItemValue(Indices, Value);
end;

function TCustomIBCArray.GetItemAsWideString(Indices: array of integer): WideString;
begin
  if FInternalItemType = dtWideString then
    Result := GetItemValue(Indices)
  else
  if FInternalItemType = dtString then
    Result := GetItemAsString(Indices)
  else
    Result := '';
end;

procedure TCustomIBCArray.SetItemAsWideString(Indices: array of integer; Value: WideString);
begin
  if FInternalItemType in [dtWideString, dtString] then
    SetItemValue(Indices, Value);
end;

function TCustomIBCArray.GetItemAsInteger(Indices: array of integer): integer;
begin
  if FInternalItemType = dtInteger then
    Result := GetItemValue(Indices)
  else
    Result := 0;
end;

procedure TCustomIBCArray.SetItemAsInteger(Indices: array of integer; Value: integer);
begin
  if FInternalItemType = dtInteger then
    SetItemValue(Indices, Value);
end;

function TCustomIBCArray.GetItemAsSmallInt(Indices: array of integer): SmallInt;
begin
  if FInternalItemType = dtSmallInt then
    Result := GetItemValue(Indices)
  else
    Result := 0;
end;

procedure TCustomIBCArray.SetItemAsSmallInt(Indices: array of integer; Value: SmallInt);
begin
  if FInternalItemType = dtSmallInt then
    SetItemValue(Indices, Value);
end;

function TCustomIBCArray.GetItemAsFloat(Indices: array of integer): double;
begin
  if FInternalItemType = dtFloat then
    Result := GetItemValue(Indices)
  else
    Result := 0;
end;

procedure TCustomIBCArray.SetItemAsFloat(Indices: array of integer; Value: double);
begin
  if FInternalItemType = dtFloat then
    SetItemValue(Indices, Value);
end;

function TCustomIBCArray.GetItemAsDateTime(Indices: array of integer): TDateTime;
begin
  if FInternalItemType in [dtDate, dtTime, dtDateTime] then
    Result := GetItemValue(Indices)
  else
    Result := 0;
end;

procedure TCustomIBCArray.SetItemAsDateTime(Indices: array of integer; Value: TDateTime);
begin
  if FInternalItemType in [dtDate, dtTime, dtDateTime] then
    SetItemValue(Indices, Value);
end;

{ TIBCType }

constructor TIBCArrayType.Create(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE;
  TableName, ColumnName: string; NeedDescribe: boolean = True);
begin
  inherited Create;

  Init(DbHandle, TrHandle, TableName, ColumnName, NeedDescribe);
end;

constructor TIBCArrayType.Create(Connection: TGDSConnection; Transaction: TGDSTransaction;
  TableName, ColumnName: string; NeedDescribe: boolean = True);
begin
  inherited Create;

  FConnection := Connection;
  FTransaction := Transaction;
  if (FConnection <> nil) and (FTransaction <> nil) then
    Init(FConnection.GetDatabaseHandle, FTransaction.GetTransactionHandle,
      TableName, ColumnName, NeedDescribe)
  else
    Init(nil, nil, TableName, ColumnName, NeedDescribe);
end;

procedure TIBCArrayType.Init(DbHandle: TISC_DB_HANDLE; TrHandle: TISC_TR_HANDLE;
  TableName, ColumnName: string; NeedDescribe: boolean = True);
begin
  if GDS.Version >= 7 then
    FADType := adGDS7
  else
    FADType := adGDS;
  FStatusVector := Marshal.AllocHGlobal(20 * SizeOf(ISC_Status));
  FDbHandle := Marshal.AllocHGlobal(SizeOf(TISC_DB_HANDLE));
  Marshal.WriteIntPtr(FDbHandle, DbHandle);
  FTrHandle := Marshal.AllocHGlobal(SizeOf(TISC_TR_HANDLE));
  Marshal.WriteIntPtr(FTrHandle, TrHandle);
  FArrayDesc := Marshal.AllocHGlobal(ARRAYDESC_LENGTH(FADType));
  FDescAccessor := TDescAccessor.Create(FADType, FArrayDesc);
  FDescAccessor.Clear;
  FTableName := TableName;
  FColumnName := ColumnName;
  if NeedDescribe then
    Describe(TableName, ColumnName)
end;

destructor TIBCArrayType.Destroy;
begin
  FDescAccessor.Free;
  Marshal.FreeHGlobal(FArrayDesc);
  Marshal.FreeHGlobal(FTrHandle);
  Marshal.FreeHGlobal(FDbHandle);
  Marshal.FreeHGlobal(FStatusVector);
  
  inherited Destroy;
end;

function TIBCArrayType.GetGDS: TGDS;
begin
  if FConnection <> nil then
    Result := FConnection.GDS
  else
    Result := GDSList.GetGDS('');
end;

function TIBCArrayType.GetDbHandle: TISC_DB_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FDbHandle);
end;

procedure TIBCArrayType.SetDbHandle(Value: TISC_DB_HANDLE);
begin
  if GetDbHandle <> Value then
    Marshal.WriteIntPtr(FDbHandle, Value);
end;

function TIBCArrayType.GetTrHandle: TISC_TR_HANDLE;
begin
  Result := Marshal.ReadIntPtr(FTrHandle);
end;

procedure TIBCArrayType.SetTrHandle(Value: TISC_TR_HANDLE);
begin
  if GetTrHandle <> Value then
    Marshal.WriteIntPtr(FTrHandle, Value);
end;

procedure TIBCArrayType.SetConnection(Value: TGDSConnection);
begin
  if Value <> nil then
    SetDbHandle(Value.GetDatabaseHandle);
  FConnection := Value;    
end;

procedure TIBCArrayType.SetTransaction(Value: TGDSTransaction);
begin
  if Value <> nil then
    SetTrHandle(Value.GetTransactionHandle);
  FTransaction := Value;
end;

procedure TIBCArrayType.Check(Status: ISC_STATUS);
var
  SqlErrMsg, Msg: _string;
  ErrorNumber, ErrorCode: integer;
begin
  if Status > 0 then begin
    GDS.Busy;
    ErrorCode := GDS.isc_sqlcode(FStatusVector);
    GDS.Release;
    GDS.GetIBError(ErrorCode, False, ErrorNumber, FStatusVector, Msg, SqlErrMsg);
    raise EIBCError.Create(ErrorCode, ErrorNumber, Msg, SqlErrMsg);
  end;
end;

procedure TIBCArrayType.Describe(TableName, ColumnName: string);
var
  pTableName, pColumnName: IntPtr;
  Res: ISC_STATUS;

  procedure CreateAttr(ArrayType: TIBCArrayType; Dimension: integer);
  var
    Attribute: TAttribute;
    VariantType: integer;
    InternalType: word;
    InternalSize: integer;
  begin
    AdaptArrayDesc(FDescAccessor);
    Attribute := TAttribute.Create;
    ArrayType.FAttributes.Add(Attribute);
    Attribute.Owner := Self;
    Attribute.AttributeNo := ArrayType.FAttributes.Count;
    Attribute.Scale := Word(ABS(ArrayType.FDescAccessor.Scale));
    Attribute.Length := FDescAccessor.Length;
    if Dimension = ArrayType.FDescAccessor.Dimensions then begin
      GetItemType(ArrayType.FDescAccessor, VariantType, InternalType, InternalSize);
      Attribute.DataType := InternalType;
      Attribute.Size := InternalSize;
    end
    else begin
      Attribute.DataType := dtArray;
      Attribute.Size := SizeOf(IntPtr);
      if (FConnection <> nil) and (FTransaction <> nil) then
        Attribute.ObjectType := TIBCArrayType.Create(FConnection, FTransaction, FTableName, FColumnName, False)
      else
        Attribute.ObjectType := TIBCArrayType.Create(DBHandle, TrHandle, FTableName, FColumnName, False);
      TIBCArrayType(Attribute.ObjectType).FName := TableName + '.' + ColumnName;
      CopyBuffer(ArrayType.FArrayDesc, TIBCArrayType(Attribute.ObjectType).FArrayDesc, ARRAYDESC_LENGTH(FADType));
      TIBCArrayType(Attribute.ObjectType).LowBound := FDescAccessor.LowBound[Dimension];
      TIBCArrayType(Attribute.ObjectType).FSize := FDescAccessor.HighBound[Dimension] -
        FDescAccessor.LowBound[Dimension] + 1;
      TIBCArrayType(Attribute.ObjectType).FDataType := dtArray;
      CreateAttr(TIBCArrayType(Attribute.ObjectType), Dimension + 1);
    end;
  end;

begin
  ClearAttributes;
  pTableName := Marshal.StringToHGlobalAnsi(AnsiString(TableName));
  pColumnName := Marshal.StringToHGlobalAnsi(AnsiString(ColumnName));
  try
    GDS.Busy;
    Res := GDS.isc_array_lookup_bounds(FStatusVector, FDbHandle, FTrHandle,
      pTableName, pColumnName, FArrayDesc);
    GDS.Release;
    Check(Res);
  finally
    Marshal.FreeCoTaskMem(pTableName);
    Marshal.FreeCoTaskMem(pColumnName);
  end;

  FName := TableName + '.' + ColumnName;
  if FDescAccessor.Dimensions > 0 then begin
    FSize := FDescAccessor.HighBound[0] - FDescAccessor.LowBound[0] + 1;
    FLowBound := FDescAccessor.LowBound[0];
    FDataType := dtArray;
    CreateAttr(Self, 1);
  end
  else
    FSize := 0;
end;

function TCustomIBCArray.GetArrayIndices(Name: string): TIBCIntegerDynArray;
var
  IdxStart: integer;
  i: integer;
begin
  IdxStart := 0;
  for i := 1 to Length(Name) do begin
    if Name[i] = '[' then
      IdxStart := i;
    if (Name[i] = ']') and (IdxStart > 0) then begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := StrToInt(Copy(Name, IdxStart + 1, i - IdxStart - 1));
      IdxStart := 0;
    end;
  end;
end;

procedure TCustomIBCArray.GetAttributeValue(const Name: _string; var AttrBuf: IntPtr;
  var IsBlank, NativeBuffer: boolean);
var
  Indices: TIBCIntegerDynArray;
  ObjType: TObjectType;
  Offset, i: integer;
  Ptr: IntPtr;
begin
  Indices := nil;
  IsBlank := IsNull;

  if IsBlank and
    not (FInternalItemType in [dtObject, dtReference, dtArray])
  then
    Exit;

  if (not FCached) or (FArrayBuffer = nil) then
    ReadArray;

  Indices := GetArrayIndices(Name);
  ObjType := ObjectType;
  for i := 1 to High(Indices) do begin
    ObjType := ObjType.Attributes[0].ObjectType;
    Assert(ObjType <> nil);
  end;

  AttrBuf := Marshal.AllocHGlobal(ObjType.Attributes[0].Size);
  NativeBuffer := False;
  try
    Offset := GetItemOffset(Indices);
    Ptr := PtrOffset(FArrayBuffer, Offset);
    CopyBuffer(Ptr, AttrBuf, FDescAccessor.Length);
    if FDescAccessor.DataType in [blr_text, blr_text2, blr_cstring, blr_cstring2,
      blr_varying, blr_varying2] then
      Marshal.WriteByte(AttrBuf, FDescAccessor.Length, $00);
  except
    Marshal.FreeHGlobal(AttrBuf);
    raise;
  end;
end;

procedure TCustomIBCArray.SetAttributeValue(const Name: _string; Source: IntPtr);
var
  Offset: integer;
  Ptr: IntPtr;
begin
  Offset := GetItemOffset(GetArrayIndices(Name));
  if FArrayBuffer = nil then
    AllocBuffer;
  Ptr := PtrOffset(FArrayBuffer, Offset);
  CopyBuffer(Source, Ptr, FDescAccessor.Length);
  FModified := True;
end;

function TCustomIBCArray.GetAttrIsNull(const Name: _string): boolean;
begin
  Result := GetIsNull;
end;

function TCustomIBCArray.GetAsString: string;
var
  Values: variant;
  Value: variant;
  Dimesions: integer;
  Indices: array of integer;
  ItemCount: integer;
  i: integer;

  procedure UpdateIndices(CurrentItem: integer);
  var
    i, j: integer;
    BeginCount: integer;
  begin
    BeginCount := 0;
    for i := VarArrayDimCount(Values) - 1 downto 0 do
      if Indices[i] = VarArrayHighBound(Values, i + 1) then begin
        Indices[i] := VarArrayLowBound(Values, i + 1);
        Result := Result + ')';
        Inc(BeginCount);
        if (i > 0) and (Indices[i - 1] < VarArrayHighBound(Values, i)) then begin
          Result := Result + ', ';
          if CurrentItem < ItemCount - 1 then begin
            for j := 0 to BeginCount - 1 do
              Result := Result + '(';
            BeginCount := 0;
          end;
        end;
      end
      else begin
        Indices[i] := Indices[i] + 1;
        if i = VarArrayDimCount(Values) - 1 then
          Result := Result + '; ';
        Break;
      end;
  end;

begin
  Values := Items;
  Dimesions := VarArrayDimCount(Values);
  ItemCount := 1;
  SetLength(Indices, Dimesions);
  for i := 0 to Dimesions - 1 do begin
    ItemCount := ItemCount * (VarArrayHighBound(Values, i + 1) - VarArrayLowBound(Values, i + 1) + 1);
    Indices[i] := VarArrayLowBound(Values, i + 1);
    Result := Result + '(';
  end;
  for i := 0 to ItemCount - 1 do begin
    Value := VarArrayGet(Values, Indices);
    if VarIsType(Value, varSmallint) or VarIsType(Value, varInteger) then
      Result := Result + IntToStr(Value)
    else
    if VarIsType(Value, varSingle) or VarIsType(Value, varDouble) then
      Result := Result + FloatToStr(Value)
    else
    if VarIsType(Value, varDate) then
      Result := Result + DateTimeToStr(Value)
    else
    if VarIsType(Value, varBoolean) then
      Result := Result + BoolToStr(Boolean(WordBool(Value)), True)
    else
      Result := Result + '''' + Value + '''';
    UpdateIndices(i);
  end;
end;

procedure TCustomIBCArray.SetAsString(Value: string);
var
  Parser: TIBCParser;
  Lexem: _string;
  Code, PrevCode: integer;
  Indices: array of integer;
  TmpValue: variant;
  i: integer;
  
  procedure IncIndices;
  var
    i: integer;
  begin
    for i := FDescAccessor.Dimensions - 1 downto 0 do
      if Indices[i] = FDescAccessor.HighBound[i] then
        Indices[i] := FDescAccessor.LowBound[i]
      else begin
        Inc(Indices[i]);
        Break;
      end;
  end;
  
begin
  PrevCode := 0;
  SetLength(Indices, FDescAccessor.Dimensions);
  for i := 0 to High(Indices) do
    Indices[i] := FDescAccessor.LowBound[i];
  Parser := TIBCParser.Create(Value);
  try
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    Parser.ToBegin;
    Code := Parser.GetNext(Lexem);
    repeat
      if (PrevCode = lcNumber) or (PrevCode = lcString) then begin
        if Code = lcSymbol then begin
          if (Lexem <> ';') and (Lexem <> ')') then
            raise Exception.Create('');
        end
        else
          raise Exception.Create('');
      end
      else
      if (Code = lcNumber) then begin
        case FInternalItemType of
          dtFloat:
            TmpValue := StrToFloat(Lexem);
          dtInteger, dtSmallInt:
            TmpValue := StrToInt(Lexem);
          dtBoolean:
            TmpValue := StrToBool(Lexem);
         dtDate, dtTime, dtDateTime:
           TmpValue := StrToDateTime(Lexem);
         else
           TmpValue := Unassigned;
           raise Exception.Create('Invalid value');
        end;
        SetItemValue(Indices, TmpValue);
        IncIndices;
      end
      else
      if Code = lcString then begin
        case FInternalItemType of
          dtBoolean:
            TmpValue := StrToBool(Lexem);
          dtString:
            TmpValue := Lexem;
          else
            TmpValue := Unassigned;
            raise Exception.Create('Invalid value');
        end;
        SetItemValue(Indices, TmpValue);
        IncIndices;
      end;
      Code := Parser.GetNext(Lexem);
    until Code = lcEnd;
  finally
    Parser.Free;
  end;
end;

procedure TIBCArrayType.SetLowBound(const Value: integer);
begin
  FLowBound := Value;
end;

{ TIBCArrayUtils }

class procedure TIBCArrayUtils.SetArrayDesc(Obj: TCustomIBCArray; Desc: IntPtr);
begin
  Obj.SetArrayDesc(Desc);
end;

class function TIBCArrayUtils.GetArrayIDPtr(Obj: TCustomIBCArray): PISC_QUAD;
begin
  Result := obj.GetArrayIDPtr;
end;

end.
