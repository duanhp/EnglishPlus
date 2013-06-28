
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  Oracle Objects Support
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}

unit OraObjectsUni;
{$ENDIF}

interface
uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  OraCall, OraClasses,
{$ELSE}
  OraCallUni, OraClassesUni,
{$ENDIF}
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  MemUtils, MemData, CRAccess;

type
{ TOraType }

  TOraType = class (TObjectType)
  private
    FConnection: TCRConnection;
    FDataSize: word; // instance size
    FIndicatorSize: word;
    FTypeCode: integer;
    FSvcCtx: pOCISvcCtx;
    FFinal: boolean;
    FAlignNeaded: boolean;

    FValid: boolean;

  protected
    hType: pOCIType;

    procedure ClearAttributes;

  public
    constructor Create(Connection: TCRConnection; Name: _string); overload;
    constructor Create(SvcCtx: pOCISvcCtx; Name: _string); overload;
    destructor Destroy; override;

    procedure Init(Name: _string);

    procedure Describe(SvcCtx: pOCISvcCtx; Name: _string); overload;
    procedure Describe(SvcCtx: pOCISvcCtx); overload;

    property TDO: pOCIType read hType;
    property DataSize: word read FDataSize;
    property IndicatorSize: word read FIndicatorSize;
    property Final: boolean read FFinal;

    property Valid: boolean read FValid;
  end;

  TObjectTypes = class (TThreadList)
  public
    function FindType(SvcCtx: pOCISvcCtx; Name: _string): TObjectType;
    procedure ClearTypes(SvcCtx: pOCISvcCtx; LeaveTypes: boolean);
  end;

{ TOraObject }

  TOraArray = class;

  TOraObject = class (TDBObject)
  private
    FObjectType: TOraType;
    FSvcCtx: pOCISvcCtx;
    FOCIEnv: pOCIEnv;
    FUnicodeEnv: boolean;
    FNativeInstance: boolean;
    FObjects: TList;            //This list contains Objects within indexes corresponding to
                                //AttributeNO, so some List[i] could be nil 
    FAutoCreate: boolean;
    FOwner: TOraObject;
    FOCIError: pOCIError;

    procedure SetOCISvcCtx(Value: pOCISvcCtx);
    procedure SetObjectType(Value: TOraType);
    procedure SetInstance(Value: IntPtr);
    function GetFInstance: IntPtr;
    procedure SetFInstance(Value: IntPtr);
    function GetFIndicator: IntPtr;
    procedure SetFIndicator(Value: IntPtr);

    function GetAsOCIDate(Name: _string): OCIDate;
    procedure SetAsOCIDate(Name: _string; Value: OCIDate);
    function GetAsOCINumber(Name: _string): OCINumber;
    procedure SetAsOCINumber(Name: _string; const Value: OCINumber);
    function GetAsOCIString(Name: _string): pOCIString;
    procedure SetAsOCIString(Name: _string; Value: pOCIString);

    function GetAsDateTime(Name: _string): TDateTime;
    procedure SetAsDateTime(Name: _string; Value: TDateTime);
    function GetAsFloat(Name: _string): double;
    procedure SetAsFloat(Name: _string; Value: double);
    function GetAsInteger(Name: _string): integer;
    procedure SetAsInteger(Name: _string; Value: integer);
    function GetAsLargeInt(Name: _string): int64;
    procedure SetAsLargeInt(Name: _string; Value: int64);

    function GetAsString(Name: _string): string;
    procedure SetAsString(Name: _string; const Value: string);
    function GetAsAnsiString(Name: _string): AnsiString;
    procedure SetAsAnsiString(Name: _string; const Value: AnsiString);
    function GetAsWideString(Name: _string): WideString;
    procedure SetAsWideString(Name: _string; const Value: WideString);

    function GetComplexAttribute(const Name: _string): TSharedObject;
    function GetAsObject(Name: _string): TOraObject;
//    procedure SetAsObject(Name: _string; Value: TOraObject);
    function GetAsArray(Name: _string): TOraArray;
    function GetAsLob(Name: _string): TOraLob;
    function GetAsOraTimeStamp(Name: _string): TOraTimeStamp;
    function GetAsOraInterval(Name: _string): TOraInterval;

  protected
    FInstancePtr: IntPtr;
    FIndicatorPtr: IntPtr;
    FCached: boolean;

    procedure Check(Status: sword);

    procedure CheckType; virtual;
    procedure CheckSession;
    procedure CheckAlloc(RaiseException: boolean = True);
    procedure CheckNotCached;
    procedure AllocNewObject;

    procedure FreeObjects;

    procedure GetAttributeValue(const Name: _string; var AttrBuf: IntPtr; var IsBlank, NativeBuffer: boolean); override;
    procedure SetAttributeValue(const Name: _string; Source: IntPtr); override;
    function GetAttrIsNull(const Name: _string): boolean; override;
    procedure SetAttrIsNull(const Name: _string; Value: boolean);

    procedure GetAttribute(Name: _string; var Value: IntPtr; var Ind: OCIInd;
      var Indicator: IntPtr; AutoAlloc: boolean = False; MakeNotNull: boolean = False);
    procedure SetAttribute(Name: _string; Value: IntPtr; Ind: OCIInd);

    function GetIsNull: boolean; virtual;
    procedure SetIsNull(Value: boolean); virtual;

    function GetIndicatorPtr: IntPtr; virtual;

    function GetChildObject(Attr: TAttribute; ArrayIndex: integer = -1): TSharedObject;
    procedure ChildAlloc(Child: TOraObject); virtual;

    function GetOCIRef: pOCIRef; virtual;

    property FInstance: IntPtr read GetFInstance write SetFInstance;
    property FIndicator: IntPtr read GetFIndicator write SetFIndicator;

  public
    constructor Create(ObjectType: TOraType = nil);
    destructor Destroy; override;

    procedure AllocObject; overload; virtual;
    procedure AllocObject(SvcCtx: pOCISvcCtx); overload;
    procedure AllocObject(TypeName: _string); overload; virtual;
    procedure AllocObject(SvcCtx: pOCISvcCtx; TypeName: _string); overload; virtual;

    procedure FreeObject(FreeChild: boolean = True); virtual;
    procedure Disconnect; override;

    procedure CreateObject(SvcCtx: pOCISvcCtx; TypeName: _string); // obsolete

    procedure CacheObject;

    procedure Lock;
    procedure Flush;
    procedure Refresh;
    procedure MarkDelete;
    procedure MarkUpdate;
    procedure Unmark;
    procedure WriteLobs;
    procedure ResetInstance(NewInstance, NewIndicator: IntPtr);

    function Exists: boolean;
    function IsLocked: boolean;
    function IsDirty: boolean;

    procedure Assign(Source: TOraObject); virtual;
    procedure OraObjectAssign(Source, Dest: IntPtr; OType: TOraType);
    procedure NestTableAssign(Source, Dest: IntPtr; OType: TOraType);

    property AttrIsNull[const Name: _string]: boolean read GetAttrIsNull write SetAttrIsNull;

    property AttrAsOCIDate[Name: _string]: OCIDate read GetAsOCIDate write SetAsOCIDate;
    property AttrAsOCINumber[Name: _string]: OCINumber read GetAsOCINumber write SetAsOCINumber;
    property AttrAsOCIString[Name: _string]: pOCIString read GetAsOCIString write SetAsOCIString;

    property AttrAsDateTime[Name: _string]: TDateTime read GetAsDateTime write SetAsDateTime;
    property AttrAsFloat[Name: _string]: double read GetAsFloat write SetAsFloat;
    property AttrAsInteger[Name: _string]: integer read GetAsInteger write SetAsInteger;
    property AttrAsLargeInt[Name: _string]: int64 read GetAsLargeInt write SetAsLargeInt;
    property AttrAsString[Name: _string]: string read GetAsString write SetAsString;
    property AttrAsAnsiString[Name: _string]: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AttrAsWideString[Name: _string]: WideString read GetAsWideString write SetAsWideString;
    property AttrAsObject[Name: _string]: TOraObject read GetAsObject;// write SetAsObject;
    property AttrAsArray[Name: _string]: TOraArray read GetAsArray;
    property AttrAsLob[Name: _string]: TOraLob read GetAsLob;
    property AttrAsTimeStamp[Name: _string]: TOraTimeStamp read GetAsOraTimeStamp;
    property AttrAsInterval[Name: _string]: TOraInterval read GetAsOraInterval;

    property ObjectType: TOraType read FObjectType write SetObjectType;
    property OCISvcCtx: pOCISvcCtx read FSvcCtx write SetOCISvcCtx;
    property OCIError: pOCIError read FOCIError write FOCIError;
    property Instance: IntPtr read GetFInstance write SetInstance;
    property Indicator: IntPtr read GetFIndicator write SetFIndicator;
    property InstancePtr: IntPtr read FInstancePtr;
    property IndicatorPtr: IntPtr read GetIndicatorPtr;
    property IsNull: boolean read GetIsNull write SetIsNull;
    property AutoCreate: boolean read FAutoCreate write FAutoCreate;
    property NativeInstance: boolean read FNativeInstance write FNativeInstance;
  end;


{ TOraXML }

  TOraXML = class (TOraObject)
  private
    phOCIDescriptor: pOCIDescriptor;
    CacheValue: PAChar;
    LocalIndicator: IntPtr;// for 10102
    LocalIndicatorPtr: IntPtr;

    procedure CreateXMLStream;
    procedure FreeXMLStream;
    function GetAsString: string;
    procedure SetAsString(Value: string);
    function ObjectIsNull: boolean;
    procedure StartRead;
    function Read(Count: cardinal; Dest: IntPtr): cardinal;

  protected
    function GetIsNull: boolean; override;
    procedure SetIsNull(Value: boolean); override;
    function GetIndicatorPtr: IntPtr; override;
    procedure CheckType; override;

  public
    constructor Create(ObjectType: TOraType = nil);
    destructor Destroy; override;

    procedure AllocObject; overload; override;
    procedure AllocObject(TypeName: _string); overload; override;
    procedure AllocObject(SvcCtx: pOCISvcCtx; TypeName: _string); overload; override;
    procedure AllocObject(SvcCtx: pOCISvcCtx; OraLob: TOraLob); overload;

    procedure FreeObject(FreeChild: boolean = True); override;

    procedure Assign(Source: TOraObject); override;

    procedure ReadXML;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure Extract(RetDoc: TOraXML; XPathExpr: string; NSmap: string = '');
    function Exists(XPathExpr: string; NSmap: string = ''): boolean;
    procedure Transform(XSLDoc: TOraXML; RetDoc: TOraXML);
    procedure GetSchema(SchemaDoc: TOraXML; var SchemaURL: string; var RootElem: string);
    function Validate(SchemaURL: string): boolean;
    function IsSchemaBased: boolean;

    property AsString: string read GetAsString write SetAsString;
  end;


{ TOraRef }

  TOraRef = class (TOraObject)
  private
    InvalidObject: boolean;

    function GethOCIRef: pOCIRef;
    procedure SethOCIRef(Value: pOCIRef);
    procedure SetOCIRef(Value: pOCIRef);
    function GetOCIRefPtr: ppOCIRef;

  protected
    FpOCIRef: ppOCIRef;

    function GetIsNull: boolean; override;
    procedure SetIsNull(Value: boolean); override;

    function GetAsHex: string;

    property FOCIRef: pOCIRef read GethOCIRef write SethOCIRef;
  public
    constructor Create(ObjectType: TOraType);
    destructor Destroy; override;

    procedure Pin;
    procedure Unpin; 

    function RefIsNull: boolean;
    procedure Clear;

    procedure Assign(Source: TOraObject); override;

    property OCIRef: pOCIRef read GethOCIRef write SetOCIRef;
    property OCIRefPtr: ppOCIRef read GetOCIRefPtr;
    property AsHex: string read GetAsHex;
  end;

{ TOraArray }

  TOraArray = class (TOraObject)
  private
    procedure SetSize(Value: integer);
    function GetMaxSize: integer;
    function GetItemType: word;
    function GetItemSubType: word;

    function GetItemExists(Index: integer): boolean;
    function GetItemIsNull(Index: integer): boolean;
    procedure SetItemIsNull(Index: integer; Value: boolean);

    function GetItemAsOCIString(Index: integer): pOCIString;
    procedure SetItemAsOCIString(Index: integer; Value: pOCIString);

    function GetItemAsDateTime(Index: integer): TDateTime;
    procedure SetItemAsDateTime(Index: integer; Value: TDateTime);
    function GetItemAsFloat(Index: integer): double;
    procedure SetItemAsFloat(Index: integer; Value: double);
    function GetItemAsInteger(Index: integer): integer;
    procedure SetItemAsInteger(Index: integer; Value: integer);
    function GetItemAsLargeInt(Index: integer): int64;
    procedure SetItemAsLargeInt(Index: integer; Value: int64);

    function GetItemAsString(Index: integer): string;
    procedure SetItemAsString(Index: integer; const Value: string);
    function GetItemAsAnsiString(Index: integer): AnsiString;
    procedure SetItemAsAnsiString(Index: integer; const Value: AnsiString);
    function GetItemAsWideString(Index: integer): WideString;
    procedure SetItemAsWideString(Index: integer; const Value: WideString);

    function GetItemAsObject(Index: integer): TOraObject;
    procedure SetItemAsObject(Index: integer; Value: TOraObject);
    function GetItemAsRef(Index: integer): TOraRef;
    procedure SetItemAsRef(Index: integer; Value: TOraRef);
    function GetItemAsLob(Index: integer): TOraLob;
    procedure SetItemAsLob(Index: integer; Value: TOraLob);

  protected
    procedure CheckType; override;

    procedure CheckIndex(Index: integer);

    procedure ChildAlloc(Child: TOraObject); override;

    function GetSize: integer; virtual;

  public
    procedure Clear;
    function AppendItem: integer;
    procedure InsertItem(Index: integer);

    procedure Assign(Source: TOraObject); override;

    property Size: integer read GetSize write SetSize;
    property MaxSize: integer read GetMaxSize;
    property ItemType: word read GetItemType;
    property ItemSubType: word read GetItemSubType;

    property ItemExists[Index: integer]: boolean read GetItemExists;
    property ItemIsNull[Index: integer]: boolean read GetItemIsNull write SetItemIsNull;

    property ItemAsOCIString[Index: integer]: pOCIString read GetItemAsOCIString write SetItemAsOCIString;

    property ItemAsDateTime[Index: integer]: TDateTime read GetItemAsDateTime write SetItemAsDateTime;
    property ItemAsFloat[Index: integer]: double read GetItemAsFloat write SetItemAsFloat;
    property ItemAsInteger[Index: integer]: integer read GetItemAsInteger write SetItemAsInteger;
    property ItemAsLargeInt[Index: integer]: int64 read GetItemAsLargeInt write SetItemAsLargeInt;
    property ItemAsString[Index: integer]: string read GetItemAsString write SetItemAsString;
    property ItemAsAnsiString[Index: integer]: AnsiString read GetItemAsAnsiString write SetItemAsAnsiString;
    property ItemAsWideString[Index: integer]: WideString read GetItemAsWideString write SetItemAsWideString;
    property ItemAsObject[Index: integer]: TOraObject read GetItemAsObject write SetItemAsObject;
    property ItemAsRef[Index: integer]: TOraRef read GetItemAsRef write SetItemAsRef;
    property ItemAsLob[Index: integer]: TOraLob read GetItemAsLob write SetItemAsLob;
  end;

{ TOraNestTable }

  TOraNestTable = class (TOraArray)
  private

  protected
    procedure CheckType; override;

    function GetSize: integer; override;

  public
    procedure Assign(Source: TOraObject); override;
    procedure DeleteItem(Index: integer);

    property Size: integer read GetSize;
  end;

{ TRefData }

  TRefData = class (TData)
  private
    FRef: TOraRef;
    FIncludeObjectField: boolean;

    procedure SetRef(Value: TOraRef);

  protected
    procedure InternalPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override; // PrepareData;

  { Fields }
    procedure InternalInitFields; override;

  { Edit }

  public
    constructor Create;
    destructor Destroy; override;

    procedure Reopen; override;

  { Records }
    procedure GetRecord(RecBuf: IntPtr); override;
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure GetPriorRecord(RecBuf: IntPtr); override;
    procedure PutRecord(RecBuf: IntPtr); override;

    procedure AppendRecord(RecBuf: IntPtr); override;
    procedure InsertRecord(RecBuf: IntPtr); override;
    procedure UpdateRecord(RecBuf: IntPtr); override;
    procedure DeleteRecord; override;

    class function IsComplexFieldType(DataType: word): boolean; override;
    procedure CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean); override;

  { Bookmarks }
    procedure GetBookmark(Bookmark: PRecBookmark); override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;

    property Ref: TOraRef read FRef write SetRef;
    property IncludeObjectField: boolean read FIncludeObjectField write FIncludeObjectField;
  end;

{ TTableData }

  TTableData = class (TMemData)
  private
    FTable: TOraNestTable;
    FIndexOfs: word;
    FIncludeObjectField: boolean;

    procedure SetTable(Value: TOraNestTable);

  protected
    LastIndex: integer;

    procedure Check(Status: sword);

    procedure InternalPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override; // PrepareData;

  { Fields }
    procedure InternalInitFields; override;
    //procedure GetObjectFields(RecBuf: IntPtr; Obj: TDBObject; var FieldNo: word);
    //procedure PutObjectFields(RecBuf: IntPtr; Obj: TDBObject; var FieldNo: word);

    function GetIndicatorSize: word; override;
  { Edit }
    procedure InternalAppend(RecBuf: IntPtr); override;
    procedure InternalDelete; override;
    procedure InternalUpdate(RecBuf: IntPtr); override;

    function Fetch(FetchBack: boolean = False): boolean; override;
    function GetRecordCount: longint; override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Reopen; override;

  { Fields }
    procedure InitFields; override;
    class function IsBlobFieldType(DataType: word): boolean; override;

  { Records }
    procedure GetRecord(RecBuf: IntPtr); override;
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure PutRecord(RecBuf: IntPtr); override;

    procedure CancelRecord(RecBuf: IntPtr); override;

    class function IsComplexFieldType(DataType: word): boolean; override;
    procedure FetchComplexFields(RecBuf: IntPtr; WithBlob: boolean);
    procedure CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean); override;
    procedure CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean); override;

    property Table: TOraNestTable read FTable write SetTable;
    property IncludeObjectField: boolean read FIncludeObjectField write FIncludeObjectField;
  end;

  function GetOraType(SvcCtx: pOCISvcCtx; Name: _string): TObjectType;

  function GetObjectCacheMaxSize: integer;
  procedure SetObjectCacheMaxSize(Value: integer);
  function GetObjectCacheOptSize: integer;
  procedure SetObjectCacheOptSize(Value: integer);

var
  ObjectTypes: TObjectTypes;

implementation

uses
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text,
{$ENDIF}
  DAConsts,
{$IFNDEF UNIDACPRO}
  OraConsts, OraError;
{$ELSE}
  OraConstsUni, OraErrorUni;
{$ENDIF}

const
  SInvalidAttrName = 'Invalid attribute name';
  SCannotConvert = 'Cannot convert type to ';
  //NullInd: OCIInd = OCI_IND_NULL;

var
  NotNullInd: IntPtr;
  NullIndStruct: IntPtr;
  NullOCINumber: IntPtr;
  NullOCIDate: IntPtr;
  NullOCIString: IntPtr;

{ TOraType }

constructor TOraType.Create(Connection: TCRConnection; Name: _string);
begin
  inherited Create;

  FConnection := Connection;
  FSvcCtx := TOCIConnection(Connection).GetSvcCtx;

  Init(Name);
end;

constructor TOraType.Create(SvcCtx: pOCISvcCtx; Name: _string);
begin
  inherited Create;

  FSvcCtx := SvcCtx;

  Init(Name);
end;

destructor TOraType.Destroy;
begin
  ObjectTypes.Remove(Self);

  inherited;
end;

procedure TOraType.Init(Name: _string);
begin
  if ObjectTypes = nil then
    ObjectTypes := TObjectTypes.Create;

  ObjectTypes.Add(Self);

  Describe(FSvcCtx, Name);
end;

procedure TOraType.ClearAttributes;
begin
  inherited ClearAttributes;
end;

procedure TOraType.Describe(SvcCtx: pOCISvcCtx; Name: _string);
var
  UnicodeEnv: boolean;

  procedure Check(Status: sword);
  begin
    if Status <> OCI_SUCCESS then
      {$IFDEF CLR}Devart.Odac.{$ENDIF}{$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.
      {$IFNDEF FPC}OraError{$ELSE}DoOraError{$ENDIF}(Status, UnicodeEnv, hOCIError);
  end;

  procedure CreateAttr(hAttr: pOCIParam);
  var
    Attribute: TAttribute;
    Ptr, ValuePtr, StrPtr: IntPtr;
    ValueInt: Integer;
    Len: integer;
    DataSize: ub2;
    DataType: ub2;
    Scale: sb1;
    Prec: ub1;
    TypeName: _string;
    SchemaName: _string;
    Res: sword;
    IndicatorSize: word;
    v: variant;
    UseUnicode: boolean;
  begin
    Attribute := TAttribute.Create;
    FAttributes.Add(Attribute);
    Attribute.Owner := Self;

    Ptr := Marshal.AllocHGlobal(sizeof(integer));
    try
      Res := -1;
      StrPtr := nil;
      ValuePtr := OrdinalToPtr(StrPtr);
      try
        Res := OCIAttrGet1(hAttr, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_NAME, hOCIError);
      finally
        PtrToOrdinal(ValuePtr, StrPtr);

        if (Res = OCI_SUCCESS) and (ValuePtr <> nil) and (StrPtr <> nil) then begin
          Len := Marshal.ReadInt32(Ptr);
          if OCIVersion < 8100 then
            Attribute.Name := PtrToStringOCI(StrPtr, UnicodeEnv) // with 0 terminator and Len += 1
          else
            Attribute.Name := PtrToStringOCI(StrPtr, Len, UnicodeEnv);
        end
        else
          Attribute.Name := 'ELEMENT';
        if Attribute.Name = '' then
          Attribute.Name := 'ELEMENT';
      end;

      Check(OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_SIZE, hOCIError));
      DataSize := Word(ValueInt);
      Check(OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_DATA_TYPE, hOCIError));
      DataType := Word(ValueInt);

      Attribute.Length := 0;
      Attribute.Scale := 0;
      Attribute.AttributeNo := FAttributes.Count;

      IndicatorSize := SizeOf(OCIInd);

      case DataType of
        SQLT_CHR,SQLT_AFC: begin
          Attribute.Fixed := DataType = SQLT_AFC;
          if UnicodeEnv then begin
            Attribute.DataType := dtWideString;
            Attribute.Size := (DataSize + 1) * 2;
          end
          else begin
            Attribute.DataType := dtString;
            Attribute.Size := DataSize + 1;  // for null terminator
          end;
          Attribute.Length := DataSize;
          Attribute.DataSize := SizeOf(pOCIString);
        end;
        SQLT_NUM: begin
          Check(OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_PRECISION, hOCIError));
          Prec := ub1(ValueInt);
          Check(OCIAttrGet2(hAttr, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_SCALE, hOCIError));
          Scale := sb1(ValueInt);
          Attribute.Length := Prec;
          Attribute.Scale := Abs(Scale);
          Attribute.DataSize := OCI_NUMBER_SIZE;

          if (Prec <= IntegerPrecision) and (Attribute.Scale = 0) then begin
            Attribute.DataType := dtInteger;
            Attribute.Size := sizeof(Integer);
          end
          else
          if (Prec <= LargeIntPrecision) and (Attribute.Scale = 0) then begin
            Attribute.DataType := dtLargeint;
            Attribute.Size := sizeof(Int64);
          end
          else begin
            Attribute.DataType := dtFloat;
            Attribute.Size := SizeOf(Double);
          end;
        end;
        SQLT_DAT: begin
          Attribute.DataType := dtDateTime;
          Attribute.Size := SizeOf(TDateTime);
          Attribute.DataSize := (SizeOf(OCIDate) + 3) and not 3; // dword align
        end;
        SQLT_NTY, SQLT_REF: begin
          Attribute.Size := SizeOf(IntPtr);

          StrPtr := nil;
          ValuePtr := OrdinalToPtr(StrPtr);
          try
            Check(OCIAttrGet1(hAttr, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_TYPE_NAME, hOCIError));
          finally
            PtrToOrdinal(ValuePtr, StrPtr);
            Len := Marshal.ReadInt32(Ptr);
            if OCIVersion < 8100 then
              TypeName := PtrToStringOCI(StrPtr, UnicodeEnv) // with 0 terminator and Len += 1
            else
              TypeName := PtrToStringOCI(StrPtr, Len, UnicodeEnv);
          end;
          StrPtr := nil;
          ValuePtr := OrdinalToPtr(StrPtr);
          try
            Check(OCIAttrGet1(hAttr, OCI_DTYPE_PARAM, ValuePtr, Ptr, OCI_ATTR_SCHEMA_NAME, hOCIError));
          finally
            PtrToOrdinal(ValuePtr, StrPtr);
            Len := Marshal.ReadInt32(Ptr);
            if OCIVersion < 8100 then
              SchemaName := PtrToStringOCI(StrPtr, UnicodeEnv) // with 0 terminator and Len += 1
            else
              SchemaName := PtrToStringOCI(StrPtr, Len, UnicodeEnv);
          end;

          if ObjectTypes <> nil then
            Attribute.ObjectType := ObjectTypes.FindType(SvcCtx, QuotedOCIName(SchemaName + '.' + TypeName));
          if Attribute.ObjectType = nil then begin
            Attribute.ObjectType := TOraType.Create(SvcCtx, QuotedOCIName(SchemaName + '.' + TypeName));
            Attribute.ObjectType.Release;
          end;

          if DataType = SQLT_NTY then
            Attribute.DataType := Attribute.ObjectType.DataType
          else
            Attribute.DataType := dtReference;

          if (Attribute.DataType = dtObject) and TOraType(Attribute.ObjectType).FFinal then begin
            Attribute.DataSize := TOraType(Attribute.ObjectType).DataSize;
            if TOraType(Attribute.ObjectType).FAlignNeaded then
              Attribute.DataSize := (Attribute.DataSize + 3) and not 3;
            IndicatorSize := TOraType(Attribute.ObjectType).IndicatorSize;
          end
          else begin
            Attribute.DataSize := SizeOf(IntPtr);
            IndicatorSize := SizeOf(OCIInd);
          end;
        end;
        SQLT_BLOB: begin
          Attribute.DataType := dtOraBlob;
          Attribute.Size := SizeOf(IntPtr);
          Attribute.DataSize := SizeOf(pOCILobLocator);
        end;
        SQLT_CLOB: begin
          if FConnection <> nil then begin
            FConnection.GetProp(prUseUnicode, v);
            UseUnicode := v;
          end
          else
            UseUnicode := False;

          if UseUnicode then
            Attribute.DataType := dtWideOraClob
          else
            Attribute.DataType := dtOraClob;
          Attribute.Size := SizeOf(IntPtr);
          Attribute.DataSize := SizeOf(pOCILobLocator);
        end;
        SQLT_BFILEE: begin
          Attribute.DataType := dtBFILE;
          Attribute.Size := sizeof(IntPtr);
          Attribute.DataSize := SizeOf(pOCILobLocator);
        end;
        SQLT_CFILEE: begin
          Attribute.DataType := dtCFILE;
          Attribute.Size := sizeof(IntPtr);
          Attribute.DataSize := SizeOf(pOCILobLocator);
        end;
        SQLT_BIN: begin  // WAR  RAW as STRING
          Attribute.DataType := dtString;
          Attribute.Size := DataSize * 2 + 1;  // for terminator and heximal represent
          Attribute.Length := DataSize * 2;
          Attribute.DataSize := SizeOf(pOCIString);
        end;
        SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ, SQLT_INTERVAL_YM,
        SQLT_INTERVAL_DS:
        begin
          if OCIVersion < 9000 then
            RaiseError(SDataTypeNotSupported);
          case DataType of
            SQLT_TIMESTAMP:
              Attribute.DataType := dtTimeStamp;
            SQLT_TIMESTAMP_TZ:
              Attribute.DataType := dtTimeStampTZ;
            SQLT_TIMESTAMP_LTZ:
              Attribute.DataType := dtTimeStampLTZ;
            SQLT_INTERVAL_YM:
              Attribute.DataType := dtIntervalYM;
            SQLT_INTERVAL_DS:
              Attribute.DataType := dtIntervalDS;
          end;
          Attribute.Size := SizeOf(IntPtr);
          Attribute.DataSize := SizeOf(IntPtr);
        end;
        SQLT_IBFLOAT: begin
          Attribute.DataType := dtFloat;
          Attribute.SubDataType := dtBFloat;
          Attribute.Size := SizeOf(Double);
          Attribute.DataSize := SizeOf(Single);
          //Attribute.Length := 38;
          //Attribute.Scale := 127;
        end;
        SQLT_IBDOUBLE: begin
          Attribute.DataType := dtFloat;
          Attribute.SubDataType := dtBDouble;
          Attribute.Size := SizeOf(Double);
          Attribute.DataSize := SizeOf(Double);
          //Attribute.Length := 38;
          //Attribute.Scale := 127;
        end;
      else
        Assert(False, SUnknownDataType);
      end;

    // dword alignment                    // can't dword align dtDateTime, dtObject !!!
      if (Attribute.DataType in [dtString, dtWideString, {dtDateTime, dtObject, }
          dtReference, dtArray, dtTable, dtOraBlob, dtBFILE, dtCFILE,
          dtOraClob, dtWideOraClob,
          dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ, dtIntervalYM, dtIntervalDS])
      then begin
        // Align to size of Pointer
        FDataSize := (FDataSize + (sizeof(IntPtr) - 1)) and not (sizeof(IntPtr) - 1);
        FAlignNeaded := True;
      end;

      if (Attribute.DataType = dtObject) and
        ((TOraType(Attribute.ObjectType).FAlignNeaded) or (not TOraType(Attribute.ObjectType).Final))
      then begin
        FDataSize := (FDataSize + 3) and not 3;
        FAlignNeaded := True;
      end;

      Attribute.Offset := FDataSize;

      Inc(FDataSize, Attribute.DataSize);

      Attribute.IndicatorOffset := FIndicatorSize;
      Inc(FIndicatorSize, IndicatorSize)
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end;

var
  hDescribe: pOCIDescribe;
  hParam: pOCIParam;
  hAttrList: pOCIParam;
  hAttr: pOCIParam;
  hCollElement: pOCIParam;
  Count: integer;
  i, Res, SSize, TSize: integer;
  TypeName: _string;
  SchemaName: _string;
  AName: _string;
  TypeCode: OCITypeCode;
  hOCIEnv: pOCIEnv;
  ValuePtr: IntPtr;
  ValueInt: Integer;
  pName: IntPtr;
  pS, pT: IntPtr;
begin
  if SvcCtx = nil then
    RaiseError(SSvcCtxNotDefined);

  if FConnection <> nil then begin
    UnicodeEnv := TOCIConnection(FConnection).UnicodeEnv;
    hOCIEnv := TOCIConnection(FConnection).OCIEnv;
  end
  else begin
    UnicodeEnv := False;
    ValuePtr := OrdinalToPtr(hOCIEnv);
    try
      Check(OCIAttrGet1(SvcCtx, OCI_HTYPE_SVCCTX, ValuePtr, nil, OCI_ATTR_ENV, hOCIError));
    finally
      PtrToOrdinal(ValuePtr, hOCIEnv);
    end;
    UnicodeEnv := IsUnicodeEnv(hOCIEnv);
  end;
  ClearAttributes;
  FDataSize := 0;
  FIndicatorSize := SizeOf(OCIInd);

  Name := QuotedSQLName(Name);
  AName := Name;
  i := Pos('.', Name);
  if i > 0 then begin
    TypeName := OCISQLInfo.UnQuote(Copy(Name, i + 1, Length(Name)));
    SchemaName := OCISQLInfo.UnQuote(Copy(Name, 1, i - 1));
  end
  else begin
    TypeName := OCISQLInfo.UnQuote(Name);
    SchemaName := '';
  end;

  pS := StringToHGlobalOCI(SchemaName, SSize, UnicodeEnv);
  pT := StringToHGlobalOCI(TypeName, TSize, UnicodeEnv);
  Res := OCITypeByName(hOCIEnv, hOCIError, SvcCtx, pS, SSize, pT, TSize,
    nil, 0, OCI_DURATION_SESSION, OCI_TYPEGET_HEADER, hType);
  FreeStringOCI(pS, UnicodeEnv);
  FreeStringOCI(pT, UnicodeEnv);
  Check(Res);

  Check(OCIHandleAlloc(hOCIEnv, hDescribe, OCI_HTYPE_DESCRIBE, 0, nil));
  try
    pName := StringToHGlobalOCI(AName, SSize, UnicodeEnv);
    try
      Check(OCIDescribeAny(SvcCtx, hOCIError, pName, SSize,
        OCI_OTYPE_NAME, 0, OCI_PTYPE_TYPE, hDescribe));
    finally
      FreeStringOCI(pName, UnicodeEnv);
    end;

    ValuePtr := OrdinalToPtr(hParam);
    try
      Check(OCIAttrGet1(hDescribe, OCI_HTYPE_DESCRIBE, ValuePtr, nil, OCI_ATTR_PARAM, hOCIError));
    finally
      PtrToOrdinal(ValuePtr, hParam);
    end;

    FName := Name;

    Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_TYPECODE, hOCIError));
    TypeCode := sb2(ValueInt);

    if OCIVersion >= 9000 then begin
      Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_IS_FINAL_TYPE, hOCIError));
      FFinal := boolean(ValueInt);
    end
    else
      FFinal := True;

    FTypeCode := TypeCode;
    if (TypeCode = OCI_TYPECODE_OBJECT) or (TypeCode = OCI_TYPECODE_OPAQUE) then begin
      FDataType := dtObject;
      if (FName = '"SYS"."XMLTYPE"') and (OCIVersion >= 9200) then
        FDataType := dtXML;

      FSize := 0;

      Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_NUM_TYPE_ATTRS, hOCIError));
      Count := sb2(ValueInt);

      ValuePtr := OrdinalToPtr(hAttrList);
      try
        Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil, OCI_ATTR_LIST_TYPE_ATTRS, hOCIError));
      finally
        PtrToOrdinal(ValuePtr, hAttrList);
      end;

      for i := 1 to Count do begin
        Check(OCIParamGet(hAttrList, OCI_DTYPE_PARAM, hOCIError, hAttr, i));
//          hAttr := Marshal.ReadIntPtr(Ptr);

        try
          CreateAttr(hAttr);
        finally
          // free memory after OCIParamGet
          OCIDescriptorFree(hAttr, OCI_DTYPE_PARAM);
        end;
      end;
    end
    else
      if TypeCode = OCI_TYPECODE_NAMEDCOLLECTION then begin
        Check(OCIAttrGet2(hParam, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_COLLECTION_TYPECODE, hOCIError));
        TypeCode := sb2(ValueInt);

        case TypeCode of
          OCI_TYPECODE_VARRAY:
            FDataType := dtArray;
          OCI_TYPECODE_TABLE:
            FDataType := dtTable;
        else
          Assert(False);
        end;

        ValuePtr := OrdinalToPtr(hCollElement);
        try
          Check(OCIAttrGet1(hParam, OCI_DTYPE_PARAM, ValuePtr, nil, OCI_ATTR_COLLECTION_ELEMENT, hOCIError));
        finally
          PtrToOrdinal(ValuePtr, hCollElement);
        end;
        Check(OCIAttrGet2(hCollElement, OCI_DTYPE_PARAM, ValueInt, nil, OCI_ATTR_NUM_ELEMENTS, hOCIError));
        FSize := Integer(ValueInt);

        CreateAttr(hCollElement);
      end
      else
        Assert(False);
  finally
    OCIHandleFree(hDescribe, OCI_HTYPE_DESCRIBE);
  end;

  FValid := True;
end;

procedure TOraType.Describe(SvcCtx: pOCISvcCtx);
var
  TypeName: _string;
  SchemaName: _string;
  ValuePtr: IntPtr;
  i, Res, SSize, TSize: integer;
  hOCIEnv: pOCIEnv;
  UnicodeEnv: boolean;
  pS, pT: IntPtr;
begin
  if SvcCtx = nil then
    RaiseError(SSvcCtxNotDefined);

  FSvcCtx := SvcCtx;

  if FConnection <> nil then begin
    UnicodeEnv := TOCIConnection(FConnection).UnicodeEnv;
    hOCIEnv := TOCIConnection(FConnection).OCIEnv;
  end
  else begin
    ValuePtr := OrdinalToPtr(hOCIEnv);
    try
      Check(OCIAttrGet1(SvcCtx, OCI_HTYPE_SVCCTX, ValuePtr, nil, OCI_ATTR_ENV, hOCIError));
    finally
      PtrToOrdinal(ValuePtr, hOCIEnv);
    end;
    UnicodeEnv := IsUnicodeEnv(hOCIEnv);
  end;

  i := Pos('.', Name);
  if i > 0 then begin
    TypeName := OCISQLInfo.UnQuote(Copy(Name, i + 1, Length(Name)));
    SchemaName := OCISQLInfo.UnQuote(Copy(Name, 1, i - 1));
  end
  else begin
    TypeName := OCISQLInfo.UnQuote(Name);
    SchemaName := '';
  end;

  pS := StringToHGlobalOCI(SchemaName, SSize, UnicodeEnv);
  pT := StringToHGlobalOCI(TypeName, TSize, UnicodeEnv);
  Res := OCITypeByName(hOCIEnv, hOCIError, SvcCtx, pS, SSize, pT, TSize,
    nil, 0, OCI_DURATION_SESSION, OCI_TYPEGET_HEADER, hType);
  FreeStringOCI(pS, UnicodeEnv);
  FreeStringOCI(pT, UnicodeEnv);
  Check(Res, UnicodeEnv);

  FValid := True;
end;

{ TObjectTypes }

procedure TObjectTypes.ClearTypes(SvcCtx: pOCISvcCtx; LeaveTypes: boolean);
var
  i: integer;
  List: TList;
  oraType: TOraType;
begin
  List := LockList;
  try
    // addref for all object types
    for i := 0 to List.Count - 1 do begin
      oraType := TOraType(List.Items[i]);
      if (oraType.FSvcCtx = SvcCtx) then

        if LeaveTypes then
          oraType.FValid := False  //Mark expired types as invalid
        else
          oraType.AddRef;
    end;

    if LeaveTypes then
      Exit;

    // free circular links
    for i := 0 to List.Count - 1 do begin
      oraType := TOraType(List.Items[i]);
      if (oraType.FSvcCtx = SvcCtx) then
        oraType.ClearAttributes;
    end;
    i := 0;
    while i < List.Count do begin
      oraType := TOraType(List.Items[i]);
      if (oraType.FSvcCtx = SvcCtx) then
        oraType.Release
      else
        inc(i);
    end;
  finally
    UnlockList;
  end;
end;

function TObjectTypes.FindType(SvcCtx: pOCISvcCtx; Name: _string): TObjectType;
var
  i: integer;
  List: TList;
begin
  Name := QuotedSQLName(Name);
  Result := nil;
  List := LockList;
  try
    for i := 0 to List.Count - 1 do
      if (_UpperCase(TObjectType(List.Items[i]).Name) = Name) and
        (TOraType(List.Items[i]).FSvcCtx = SvcCtx)
      then begin
        Result := TObjectType(List.Items[i]);
        break;
      end;
  finally
    UnlockList;
  end;
end;

{ TOraObject }

constructor TOraObject.Create(ObjectType: TOraType);
begin
  inherited Create;

  FOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError;
  FInstancePtr := Marshal.AllocHGlobal(Sizeof(IntPtr));
  Marshal.WriteIntPtr(FInstancePtr, nil);
  FIndicatorPtr := Marshal.AllocHGlobal(Sizeof(IntPtr));
  Marshal.WriteIntPtr(FIndicatorPtr, nil);
  SetObjectType(ObjectType);
  FAutoCreate := True;
end;

destructor TOraObject.Destroy;
begin
  if FObjectType <> nil then
    FObjectType.Release;

  FreeObject;
  FreeObjects;

  Marshal.FreeHGlobal(FInstancePtr);
  Marshal.FreeHGlobal(FIndicatorPtr);
  inherited;
end;

procedure TOraObject.Check(Status: sword);
begin
  if Status <> OCI_SUCCESS then
    {$IFDEF CLR}Devart.Odac.{$ENDIF}{$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.
    {$IFNDEF FPC}OraError{$ELSE}DoOraError{$ENDIF}(Status, FUnicodeEnv, FOCIError);
end;

procedure TOraObject.CheckType;
begin
  if FObjectType = nil then
    RaiseError(SNoObjectType);
end;

procedure TOraObject.CheckSession;
begin
  if FSvcCtx = nil then
    RaiseError(SNoOCISvcCtx);
end;

procedure TOraObject.CheckAlloc(RaiseException: boolean);
begin
  if (FInstance = nil) or (FIndicator = nil) then begin
    if FAutoCreate then
      AllocObject;
    if RaiseException and (FInstance = nil) then
      RaiseError(SObjectNotAllocated);
  end;
end;

procedure TOraObject.CheckNotCached;
begin
  if FCached then
    RaiseError(SObjectIsCached);
end;

procedure TOraObject.AllocNewObject;
var
  TypeCode: OCITypeCode;
  Instance: IntPtr;
begin
  if FObjectType.DataType = dtObject then
    TypeCode := FObjectType.FTypeCode // OCI_TYPECODE_OBJECT
  else
    TypeCode := OCI_TYPECODE_NAMEDCOLLECTION;

  Check(OCIObjectNew(FOCIEnv, FOCIError, FSvcCtx, TypeCode, FObjectType.hType, nil,
    OCI_DURATION_SESSION, 1, Instance)); //If we need to pin Object then we should set Value to 0

  FInstance := Instance;
  FNativeInstance := True;
end;

procedure TOraObject.AllocObject;
var
  NewObject: boolean;
begin
  CheckType;
  CheckSession;

  if FOwner = nil then begin
    if FInstance = nil then begin
      AllocNewObject;
      NewObject := True;
    end
    else
      NewObject := False;
    if (FIndicator = nil) and (FInstance <> nil) then
      Check(OCIObjectGetInd(FOCIEnv, FOCIError, FInstance, FIndicatorPtr));
    // We cannot get NULL indicator on fetch for collections
    // So collection is NOT NULL until user explicitly set IsNull := True
    if (FObjectType.DataType = dtObject) and NewObject then
      Marshal.WriteInt16(FIndicator, OCI_IND_NULL);
  end
  else
    FOwner.ChildAlloc(Self);
end;

procedure TOraObject.FreeObject(FreeChild: boolean = True);
begin
  if FNativeInstance and (FInstance <> nil) then begin
    if not FCached then
      if FObjectType.Valid then
        // Check() doen't need here because exception will prevent executing subsequent code
        OCIObjectFree(FOCIEnv, FOCIError, FInstance, OCI_OBJECTFREE_FORCE);
    FInstance := nil;
    FIndicator := nil;
    FNativeInstance := False;
  end
  else begin
    FInstance := nil;
    FIndicator := nil;
  end;

  if FreeChild then
    FreeObjects;
end;

procedure TOraObject.AllocObject(TypeName: _string);
begin
  CheckSession;

  if TypeName <> '' then begin
    SetObjectType(TOraType.Create(FSvcCtx, TypeName));
    ObjectType.Release;
  end;

  AllocObject;
end;

procedure TOraObject.AllocObject(SvcCtx: pOCISvcCtx; TypeName: _string);
begin
  OCISvcCtx := SvcCtx;

  AllocObject(TypeName);
end;

procedure TOraObject.AllocObject(SvcCtx: pOCISvcCtx);
begin
  OCISvcCtx := SvcCtx;

  AllocObject;
end;

procedure TOraObject.Disconnect;
begin
  ObjectType := nil;
end;

procedure TOraObject.FreeObjects;
begin
  if FObjects <> nil then begin
    while FObjects.Count > 0 do begin
      TSharedObject(FObjects[0]).Free;
      FObjects.Delete(0);
    end;
    FObjects.Free;
    FObjects := nil;
  end;
end;

procedure TOraObject.CreateObject(SvcCtx: pOCISvcCtx; TypeName: _string);
begin
  AllocObject(SvcCtx, TypeName);
end;

procedure TOraObject.CacheObject;
begin
  FCached := True; //we should check this prop to deny OCI function execution in this object
end;

procedure TOraObject.Assign(Source: TOraObject);
begin
  CheckSession;
  CheckAlloc;

  if Source.Instance <> nil then begin
    Check(OCIObjectCopy(FOCIEnv, FOCIError, FSvcCtx, Source.Instance, Source.Indicator,
      Instance, Indicator, FObjectType.hType, OCI_DURATION_SESSION, 0));
    if OCIVersion >= 9000 then
      OraObjectAssign(Source.Instance, Instance, ObjectType);
  end;
end;

procedure TOraObject.OraObjectAssign(Source, Dest: IntPtr; OType: TOraType);
var
  j: integer;
  Attr: TAttribute;
begin
  if (Source = nil) or (Dest = nil) then
    Exit;
  for j := 0 to OType.AttributeCount - 1 do begin
    Attr := OType.Attributes[j];
    if Attr.ObjectType <> nil then
      case Attr.DataType of
        dtObject:
          if TOraType(Attr.ObjectType).FFinal then
            OraObjectAssign(PtrOffset(Source, Attr.Offset), PtrOffset(Dest, Attr.Offset),
              TOraType(Attr.ObjectType))
          else begin
            OraObjectAssign(Marshal.ReadIntPtr(Source, Attr.Offset),
              Marshal.ReadIntPtr(Dest, Attr.Offset), TOraType(Attr.ObjectType));
          end;
        dtTable:
          NestTableAssign(Marshal.ReadIntPtr(Source, Attr.Offset),
              Marshal.ReadIntPtr(Dest, Attr.Offset), TOraType(Attr.ObjectType))
      end;
  end;

end;

procedure TOraObject.NestTableAssign(Source, Dest: IntPtr; OType: TOraType);
var
  i,j: integer;
  E,Exists: tbool;
  destelem, elem, elemind: IntPtr;
  ASize: integer;
  Attr: TAttribute;
begin
  if (Source = nil) or (Dest = nil) or not (TAttribute(OType.Attributes[0]).DataType in [dtObject, dtArray, dtTable]) then
    Exit;
  Exists := 1;
  Check(OCITableSize(FOCIEnv, hOCIError, Source, ASize));
  for i := 1 to ASize do begin
    Check(OCICollGetElem(FOCIEnv, hOCIError, Source, i - 1, Exists,
      elem, elemind));
    if Exists <> 1 then
      Exit;
    Check(OCICollAssignElem(FOCIEnv, hOCIError, i - 1, elem, elemind, Dest));
    E := 1;
    Check(OCICollGetElem(FOCIEnv, hOCIError, Dest, i - 1, E,
      destelem, elemind));
    for j := 0 to OType.AttributeCount - 1 do begin
      Attr := OType.Attributes[j];
      if Attr.DataType = dtObject then
        OraObjectAssign(PtrOffset(elem, Attr.Offset), PtrOffset(destelem, Attr.Offset),
          TOraType(Attr.ObjectType));
    end;
  end;
end;

procedure TOraObject.ChildAlloc(Child: TOraObject);
var
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
  i: integer;
begin
  for i := 0 to FObjects.Count - 1 do
    if Child = FObjects[i] then begin
      GetAttribute(ObjectType.Attributes[i].Name, Value, Ind, IndPtr, True);
      Child.Instance := Value;
      Child.Indicator := IndPtr;
    end;
end;

function TOraObject.GetOCIRef: pOCIRef;
begin
  Result := nil;
  CheckAlloc;

  Check(OCIObjectNew(FOCIEnv, FOCIError, FSvcCtx, OCI_TYPECODE_REF, FObjectType.hType, nil,
  OCI_DURATION_DEFAULT, 1, Result));

  Check(OCIObjectGetObjectRef(FOCIEnv, FOCIError, FInstance, Result));
end;

procedure TOraObject.Lock;
begin
  CheckAlloc;
  CheckNotCached;  
  Check(OCIObjectLock(FOCIEnv, FOCIError, FInstance));
end;

procedure TOraObject.Flush;
begin
  CheckAlloc;
  CheckNotCached;  
  Check(OCIObjectFlush(FOCIEnv, FOCIError, FInstance));
end;

procedure TOraObject.Refresh;
begin
  CheckAlloc;
  CheckNotCached;  
  Check(OCIObjectRefresh(FOCIEnv, FOCIError, FInstance));
end;

procedure TOraObject.MarkDelete;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCIObjectMarkDelete(FOCIEnv, FOCIError, FInstance));
end;

procedure TOraObject.MarkUpdate;
begin
  CheckAlloc;
  CheckNotCached;  
  Check(OCIObjectMarkUpdate(FOCIEnv, FOCIError, FInstance));
end;

procedure TOraObject.Unmark;
begin
  CheckAlloc;
  CheckNotCached;  
  Check(OCIObjectUnmark(FOCIEnv, FOCIError, FInstance));
end;

procedure TOraObject.WriteLobs;
var
  i: integer;
  Obj: TObject;
  Attr: TAttribute;
begin
  // Use SetAttribute for LOB attrubites.
  // Direct writing LOB locator to object causes AV in OCI. 
  if (OCIVersion >= 8100) and not FUnicodeEnv and (FObjects <> nil) then
    for i := 0 to FObjects.Count - 1 do begin
      Obj := TObject(FObjects[i]);
      if Obj <> nil then begin
        if FObjectType.DataType in [dtArray, dtTable] then
          Attr := FObjectType.Attributes[0]
        else
          Attr := FObjectType.Attributes[i];
        case Attr.DataType of
          dtOraBlob, dtOraClob, dtWideOraClob: begin
            if Attr.DataType = dtOraBlob then
              TOraLob(Obj).CreateTemporary(ltBlob)
            else
              TOraLob(Obj).CreateTemporary(ltClob);
            TOraLob(Obj).WriteLob;
            if FObjectType.DataType in [dtArray, dtTable] then
              SetAttribute('[' + IntToStr(i) +']', TOraLob(Obj).OCILobLocator, OCI_IND_NOTNULL)
            else
              SetAttribute(Attr.Name, TOraLob(Obj).OCILobLocator, OCI_IND_NOTNULL);
          end;
          dtObject:
            TOraObject(Obj).WriteLobs;
        end;
      end
    end;
end;

procedure TOraObject.ResetInstance(NewInstance, NewIndicator: IntPtr);
var
  i: integer;
  Obj: TObject;
  Attr: TAttribute;
  Value: IntPtr;
  Ind: OCIInd;
  ObjIndicator: IntPtr;
begin
  if NewInstance <> Instance then begin
    FreeObject(False);
    FInstance := NewInstance;
    FIndicator := NewIndicator;
    if FObjects <> nil then begin
      for i := 0 to FObjects.Count - 1 do begin
        Obj := TObject(FObjects[i]);
        if Obj <> nil then begin
          if FObjectType.DataType in [dtArray, dtTable] then
            Attr := FObjectType.Attributes[0]
          else
            Attr := FObjectType.Attributes[i];
          GetAttribute(Attr.Name, Value, Ind, ObjIndicator);
          case Attr.DataType of
            dtOraBlob, dtOraClob, dtWideOraClob:
              TOraLob(Obj).OCILobLocator := Value;
            dtObject:
              TOraObject(Obj).ResetInstance(Value, ObjIndicator);
          end;
        end
      end;
    end;
  end;
end;

function TOraObject.Exists: boolean;
var
  Res: tbool;
begin
  CheckAlloc;
  CheckNotCached;
  Check(OCIObjectExists(FOCIEnv, FOCIError, FInstance, Res));
  Result := Res = 1;
end;

function TOraObject.IsLocked: boolean;
var
  Res: tbool;
begin
  CheckAlloc;
  CheckNotCached;  
  Check(OCIObjectIsLocked(FOCIEnv, FOCIError, FInstance, Res));
  Result := Res = 1;
end;

function TOraObject.IsDirty: boolean;
var
  Res: tbool;
begin
  CheckAlloc;
  Check(OCIObjectIsDirty(FOCIEnv, FOCIError, FInstance, Res));
  Result := Res = 1;
end;

const
  MaxNames = 30;

procedure TOraObject.GetAttribute(Name: _string;
  var Value: IntPtr; var Ind: OCIInd; var Indicator: IntPtr;
  AutoAlloc: boolean = False; MakeNotNull: boolean = False);
var
  iPos, IndexPos: integer;
  i: integer;
  St: _string;
  Attr: TAttribute;
  Index, ArrSize: integer;
  Exists: tbool;
  OType: TOraType;
  IsNotNull, OldNull: boolean;
  IndPtr, Instance, ItemInstance: IntPtr;
  OraObj: TOraObject;
begin
  CheckAlloc;

  Name := _UpperCase(Name);

  Value := FInstance;
  Indicator := FIndicator;
  OType := ObjectType;
  if MakeNotNull then begin
    Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
    IsNotNull := True;
  end
  else
    IsNotNull := not IsNull;
  repeat
    Name := TrimLeft(Name);

    iPos := Pos('.', Name);
    IndexPos := Pos('[', Name);

    if IndexPos = 1 then begin
      i := Pos(']', Name);
      if i > 0 then
        Index := StrToInt(Copy(Name, 2, i - 2))
      else begin
        RaiseError(SInvalidAttrName);
        Index := -1;
      end;
      if (i + 1 <= Length(Name)) and (Name[i + 1] = '.') then
        Inc(i);

      St := 'ELEMENT';
      Name := Copy(Name, i + 1, Length(Name));
      Attr := OType.Attributes[0];
      if Attr = nil then
        RaiseError(SInvalidAttrName);

      if AutoAlloc then begin
        Check(OCICollSize(FOCIEnv, FOCIError, Value, ArrSize));
        if Index >= ArrSize then
          for i := ArrSize to Index do begin
            case Attr.DataType of
              dtDateTime:
                ItemInstance := NullOCIDate;
              dtInteger, dtLargeInt, dtFloat:
                ItemInstance := NullOCINumber;
              dtString, dtWideString:
                ItemInstance := NullOCIString;
              dtObject: begin
                ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
                // Check(OCIObjectNew());
              end;
            else
              ItemInstance := nil;
              Assert(False, SUnknownDataType);
            end;

            // WAR need real Indicator for object
            Check(OCICollAppend(FOCIEnv, FOCIError, ItemInstance, NullIndStruct, Value));
            if Attr.DataType = dtObject then
              Marshal.FreeHGlobal(ItemInstance);
          end;
      end;

      Check(OCICollGetElem(FOCIEnv, FOCIError, Value, Index, Exists,
        Value, Indicator));

      if Exists = 1 then begin
        if MakeNotNull then begin
          Ind := OCI_IND_NOTNULL;
          Marshal.WriteInt16(Indicator, Ind);
        end
        else
          Ind := Marshal.ReadInt16(Indicator)
      end
      else begin
        Ind := OCI_IND_NULL;
        Value := nil;
        Indicator := nil;
        Exit;
      end;

      if Name <> '' then
        OType := TOraType(Attr.ObjectType);
    end
    else begin
      if (iPos > 0) and ((iPos < IndexPos) or (IndexPos = 0)) then begin
        St := Copy(Name, 1, iPos - 1);
        Name := Copy(Name, iPos + 1, Length(Name));
      end
      else
        if IndexPos > 0 then begin
          St := Copy(Name, 1, IndexPos - 1);
          Name := Copy(Name, IndexPos, Length(Name));
        end
        else begin
          St := Name;
          Name := '';
        end;

      Attr := OType.FindAttribute(St);
      if Attr = nil then
        RaiseError(SInvalidAttrName);

      Value := PtrOffset(Value, Attr.Offset);
      Indicator := PtrOffset(Indicator, Attr.IndicatorOffset);

      if IsNotNull then begin
        Ind := Marshal.ReadInt16(Indicator);
        OldNull := (Ind <> OCI_IND_NOTNULL);
        if MakeNotNull and OldNull then begin
          Ind := OCI_IND_NOTNULL;
          Marshal.WriteInt16(Indicator, Ind);
          IsNotNull := True;
        end
        else
          IsNotNull := not OldNull;
      end
      else begin
        Ind := OCI_IND_NULL;
        OldNull := False;
      end;

      //if Name <> '' then begin
      if (Attr.ObjectType <> nil) then begin
        OType := TOraType(Attr.ObjectType);
        if ((OType.DataType in [dtArray, dtTable]) or not OType.FFinal) and (Value <> nil) then begin
          Instance := Marshal.ReadIntPtr(Value);

          if OType.DataType = dtObject then begin // not final object
            OraObj := nil;
            if OldNull then
              if AutoAlloc then begin
                // If attribute was NULL not we don't use Instance pointer from
                // this attribute. Probably it is a ramdom value.
                OraObj := TOraObject(GetChildObject(Attr));
                if OraObj.FInstance = nil then // not yet allocated
                  OraObj.AllocNewObject;
                Instance := OraObj.FInstance;
                Marshal.WriteIntPtr(Value, Instance);
              end
              else
                Instance := nil;

            if Instance <> nil then begin
              if OraObj <> nil then
                IndPtr := OraObj.FIndicatorPtr
              else
                IndPtr := OrdinalToPtr(Indicator);
              try
                Check(OCIObjectGetInd(FOCIEnv, FOCIError, Instance, IndPtr));
              finally
                if OraObj <> nil then
                  Indicator := Marshal.ReadIntPtr(IndPtr)
                else
                  PtrToOrdinal(IndPtr, Indicator);
              end;
              if MakeNotNull then
                Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);
            end
            else
              Indicator := nil;
          end;
          Value := Instance;
        end;

        if Value = nil then
          Exit;
      end;
    end;
  until (iPos = 0) and ((IndexPos = 0) or (Name = ''));
end;

// this procedure cannot be used in Unicode environment because of OCI bug:
// OCIObjectSetAttr compares 1 character of name
procedure TOraObject.SetAttribute(Name: _string; Value: IntPtr; Ind: OCIInd);
var
  NamesArray: IntPtr;
  Names: array [0..MaxNames] of _string;
  Lengths: IntPtr;
  Types: array [1..MaxNames] of TOraType;
  Attrs: array [0..MaxNames] of TAttribute;
  NameCount: integer;
  Instance, InstancePtr: IntPtr;
  Indicator: IntPtr;
  TDO: pOCIType;
  TDOPtr: IntPtr;
  i, Size: integer;
  IndexPos: integer;
  Index: integer;
  Exists: tbool;
  ArrSize: integer;
  OType: TObjectType;
  Attr: TAttribute;
  ItemInstance, ItemInstancePtr: IntPtr;
  ItemIndicator, ItemIndicatorPtr: IntPtr;
  ItemInd: OCIInd;
  ItemIndPtr: IntPtr;
  Curr: integer;
  NullBefore: boolean;
  TempInd: IntPtr;
begin
  CheckAlloc;

  Name := _UpperCase(Name);

  Instance := FInstance;
  Indicator := FIndicator;
  TDO := ObjectType.TDO;
  OType := ObjectType;

  Lengths := Marshal.AllocHGlobal(sizeof(integer) * (MaxNames + 1));
  try
    repeat
      Name := TrimLeft(Name);

      IndexPos := Pos('[', Name);
      if IndexPos = 1 then begin
        i := Pos(']', Name);
        if i > 0 then
          Index := StrToInt(Copy(Name, 2, i - 2))
        else begin
          RaiseError(SInvalidAttrName);
          Index := -1;
        end;

        if (i + 1 <= Length(Name)) and (Name[i + 1] = '.') then
          Inc(i);

        Name := Copy(Name, i + 1, Length(Name));
        Attr := OType.Attributes[0];
        if Attr = nil then
          RaiseError(SInvalidAttrName);

        Check(OCICollSize(FOCIEnv, FOCIError, Instance, ArrSize));
        if Index >= ArrSize then
          for i := ArrSize to Index do begin
            case Attr.DataType of
              dtDateTime:
                ItemInstance := NullOCIDate;
              dtInteger, dtLargeInt, dtFloat:
                ItemInstance := NullOCINumber;
              dtString, dtWideString:
                ItemInstance := NullOCIString;
              dtObject: begin
                ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
                // Check(OCIObjectNew());
              end;
            else
              ItemInstance := nil;
              Assert(False, SUnknownDataType);
            end;

          // WAR need real Indicator for object
            Check(OCICollAppend(FOCIEnv, FOCIError, ItemInstance, NullIndStruct, Instance));
            if Attr.DataType = dtObject then
              Marshal.FreeHGlobal(ItemInstance);
          end;

        if Name = '' then begin
          if Value = nil then begin
            Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists, Value, TempInd));
            Value := Marshal.ReadIntPtr(Value);  // WAR for dtString only
          end;

          TempInd := Marshal.AllocHGlobal(sizeof(Ind));
          Marshal.WriteInt16(TempInd, Ind);
          try
            Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, Value, TempInd, Instance))
          finally
            Marshal.FreeHGlobal(TempInd);
          end;
        end
        else begin
          if Ind = OCI_IND_NOTNULL then
            Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

          Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
            Instance, Indicator));

          OType := OType.Attributes[0].ObjectType;
          TDO := TOraType(OType).TDO; // WAR for array of objects
        end;
      end
      else begin
        NameCount := 0;
        repeat
          i := Pos('.', Name);
          if (i > 0) and ((i < IndexPos) or (IndexPos = 0)) then begin
            Names[NameCount] := Copy(Name, 1, i - 1);
            Name := Copy(Name, i + 1, Length(Name));
          end
          else
            if IndexPos > 0 then begin
              Names[NameCount] := Copy(Name, 1, IndexPos - 1);
              Name := Copy(Name, IndexPos, Length(Name));
            end
            else
              Names[NameCount] := Name;

          Attr := OType.FindAttribute(Names[NameCount]);
          Attrs[NameCount] := Attr;
          if Attr = nil then
            RaiseError(SInvalidAttrName);
          if Name <> '' then
            OType := Attr.ObjectType;

          Types[NameCount + 1] := TOraType(OType);
          Inc(NameCount);
        until (i = 0) or (i > IndexPos) and (IndexPos <> 0);

        NamesArray := Marshal.AllocHGlobal((MaxNames + 1) * sizeof(IntPtr));
        for i := 0 to NameCount - 1 do begin
          Marshal.WriteIntPtr(NamesArray, i * sizeof(IntPtr), StringToHGlobalOCI(Names[i], Size, FUnicodeEnv));
          Marshal.WriteInt32(Lengths, i * sizeof(integer), Size);
        end;
        try
          if IndexPos > 0 then begin
          // getting array
            // set array to notnull OPTIM?
            Check(OCIObjectSetAttr(FOCIEnv, FOCIError, Instance, Indicator, TDO,
              NamesArray, Lengths, NameCount, nil, 0, OCI_IND_NOTNULL, nil, nil));

            InstancePtr := OrdinalToPtr(Instance);
            TDOPtr := OrdinalToPtr(TDO);
            try
              Check(OCIObjectGetAttr(FOCIEnv, FOCIError, Instance, Indicator, TDO,
                NamesArray, Lengths, NameCount, nil, 0, nil, nil, InstancePtr, TDOPtr));
            finally
              PtrToOrdinal(InstancePtr, Instance);
              PtrToOrdinal(TDOPtr, TDO);
            end;

            Instance := Marshal.ReadIntPtr(Instance);
          end
          else begin
            Curr := 0;
            if (NameCount > 1) and (Ind = OCI_IND_NOTNULL) then // for object attributes
              for i := 1 to NameCount - 1 do begin
                NullBefore := not Types[i].FFinal and
                  (Marshal.ReadInt16(Indicator, Attrs[i-1].IndicatorOffset) = OCI_IND_NULL);
                Check(OCIObjectSetAttr(FOCIEnv, FOCIError, Instance, Indicator, TDO,
                  PtrOffset(NamesArray, Curr), PtrOffset(Lengths, Curr), i - Curr, nil, 0, OCI_IND_NOTNULL, nil, nil));
                if not Types[i].FFinal then begin
                  ItemIndicatorPtr := OrdinalToPtr(ItemIndicator);
                  ItemIndPtr := OrdinalToPtr(ItemInd);
                  ItemInstancePtr := OrdinalToPtr(ItemInstance);
                  try
                    Check(OCIObjectGetAttr(FOCIEnv, FOCIError, Instance, Indicator, TDO,
                      PtrOffset(NamesArray, Curr), PtrOffset(Lengths, Curr), i - Curr, nil, 0, ItemIndPtr, ItemIndicatorPtr, ItemInstancePtr, nil));
                  finally
                    PtrToOrdinal(ItemInstancePtr, ItemInstance);
                    PtrToOrdinal(ItemIndPtr, ItemInd);
                    PtrToOrdinal(ItemIndicatorPtr, ItemIndicator);
                  end;
                  if (ItemInd = OCI_IND_NULL) or NullBefore then begin
                    FillChar(ItemIndicator, Types[i].IndicatorSize, byte(OCI_IND_NULL));
                    Marshal.WriteInt16(ItemIndicator, OCI_IND_NOTNULL);
                  end;
                  Instance := ItemInstance;
                  Indicator := ItemIndicator;
                  TDO := Types[i].TDO;
                  Curr := i;
                end;
              end;
            // setting attribute
                Check(OCIObjectSetAttr(FOCIEnv, FOCIError, Instance, Indicator, TDO,
                  PtrOffset(NamesArray, Curr * sizeof(IntPtr)), PtrOffset(Lengths, Curr * sizeof(IntPtr)), NameCount - Curr, nil, 0, Ind, nil, Value));
          end;
        finally
          for i := 0 to NameCount - 1 do
            FreeStringOCI(Marshal.ReadIntPtr(NamesArray, i * sizeof(IntPtr)), FUnicodeEnv);
          Marshal.FreeHGlobal(NamesArray);
        end;

        {if Name <> '' then
          OType := Attr.ObjectType;}
      end;
    until (IndexPos = 0) or (Name = '');
  finally
    Marshal.FreeHGlobal(Lengths);
  end;

  if (Ind = OCI_IND_NOTNULL) then
    Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object
end;

procedure TOraObject.GetAttributeValue(const Name: _string; var AttrBuf: IntPtr;
  var IsBlank, NativeBuffer: boolean);
var
  Attr: TAttribute;
  Ptr: IntPtr;
  Size: integer;
  OCIStr: pOCIString;
begin
  IsBlank := AttrIsNull[Name];
  Attr := ObjectType.AttributeByName(Name);

  if IsBlank and
    not (Attr.DataType in [dtObject,dtReference,dtArray,dtTable,dtOraBlob,dtOraClob,dtWideOraClob,
      dtBFILE,dtCFILE{,dtTimeStamp,dtTimeStampTZ,dtTimeStampLTZ,dtIntervalYM,dtIntervalDS}])
  then
    Exit;

  AttrBuf := Marshal.AllocHGlobal(Attr.Size);
  NativeBuffer := False;
  try
    case Attr.DataType of
      dtString: begin
        OCIStr := AttrAsOCIString[Name];
        Size := OCIStringSize(FOCIEnv, OCIStr);
        Ptr := OCIStringPtr(FOCIEnv, OCIStr);
        if Ptr <> nil then begin
          if Size > Attr.Length then
            Size := Attr.Length;
          CopyBuffer(Ptr, AttrBuf, Size);
          Marshal.WriteByte(AttrBuf, Size, 0);
        end
        else begin
          Marshal.WriteByte(AttrBuf, 0);
        end;
      end;
      dtWideString: begin
        OCIStr := AttrAsOCIString[Name];
        Size := OCIStringSize(FOCIEnv, OCIStr);
        Ptr := OCIStringPtr(FOCIEnv, OCIStr);
        if Ptr <> nil then begin
          if Size > Attr.Length * 2 then
            Size := Attr.Length * 2;
          CopyBuffer(Ptr, AttrBuf, Size);
          Marshal.WriteInt16(AttrBuf, Size, 0);
        end
        else begin
          Marshal.WriteInt16(AttrBuf, 0);
        end;
      end;
      dtInteger:
        Marshal.WriteInt32(AttrBuf, AttrAsInteger[Name]);
      dtLargeInt:
        Marshal.WriteInt64(AttrBuf, AttrAsLargeInt[Name]);
      dtFloat:
        Marshal.WriteInt64(AttrBuf, BitConverter.DoubleToInt64Bits(AttrAsFloat[Name]));
      dtDateTime:
        Marshal.WriteInt64(AttrBuf, BitConverter.DoubleToInt64Bits(AttrAsDateTime[Name]));
      dtObject,dtReference,dtArray,dtTable:
        Marshal.WriteIntPtr(AttrBuf, AttrAsObject[Name].GCHandle);
      dtOraBlob,dtOraClob,dtWideOraClob,dtBFILE,dtCFILE:
        Marshal.WriteIntPtr(AttrBuf, AttrAsLob[Name].GCHandle);
      dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ:
        Marshal.WriteIntPtr(AttrBuf, GetAsOraTimeStamp(Name).GCHandle);
      dtIntervalYM, dtIntervalDS:
        Marshal.WriteIntPtr(AttrBuf, GetAsOraInterval(Name).GCHandle);
    else
      Assert(False, SUnknownDataType);
    end;
  except
    Marshal.FreeHGlobal(AttrBuf);
    raise;
  end;
end;

procedure TOraObject.SetAttributeValue(const Name: _string; Source: IntPtr);
var
  Attr: TAttribute;
  BFile: TOraFile;
begin
  Attr := ObjectType.AttributeByName(Name);
  if Source = nil then begin
    case Attr.DataType of
      dtOraBlob, dtOraClob, dtWideOraClob:
        AttrIsNull[Name] := (AttrAsLob[Name].Size = 0);
      dtBFILE, dtCFILE: begin
        BFile := TOraFile(AttrAsLob[Name]);
        AttrIsNull[Name] := (BFile.FileDir = '') and (BFile.FileName = '');
      end
    else
      AttrIsNull[Name] := True
    end;
    Exit;
  end;

  case Attr.DataType of
    dtString:
      AttrAsAnsiString[Name] := Marshal.PtrToStringAnsi(Source);
    dtWideString:
      AttrAsWideString[Name] := Marshal.PtrToStringUni(Source);
    dtInteger:
      AttrAsInteger[Name] := Marshal.ReadInt32(Source);
    dtLargeInt:
      AttrAsLargeInt[Name] := Marshal.ReadInt64(Source);
    dtFloat:
      AttrAsFloat[Name] := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source));
    dtDateTime:
      AttrAsDateTime[Name] := MemUtils.TimeStampToDateTime(MSecsToTimeStamp(Trunc(
        BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source))
      )));
    dtBFILE, dtCFILE:
      AttrIsNull[Name] := AttrAsLob[Name].Size = 0;
    dtOraBlob, dtOraClob,dtWideOraClob:
  else
    RaiseError(SUnknownDataType);
  end;
end;

function TOraObject.GetIsNull: boolean;
var
  Ind: OCIInd;
begin
  if (ObjectType <> nil) and (ObjectType.FTypeCode = OCI_TYPECODE_OPAQUE) then begin
    Result := FInstance = nil;
    exit;
  end;

  if (ObjectType <> nil) and ((FOwner <> nil) or (FInstance <> nil)) then
    CheckAlloc;

  if FIndicator <> nil then begin
    Ind := Marshal.ReadInt16(FIndicator);
    Result := Ind <> OCI_IND_NOTNULL;
  end
  else
    Result := True;
end;

procedure TOraObject.SetIsNull(Value: boolean);
begin
  if FOwner <> nil then
    CheckAlloc;

  if FIndicator <> nil then
    if Value then
      Marshal.WriteInt16(FIndicator, OCI_IND_NULL)
    else
      Marshal.WriteInt16(FIndicator, OCI_IND_NOTNULL);
end;

function TOraObject.GetIndicatorPtr: IntPtr;
begin
  Result := FIndicatorPtr;
end;

function TOraObject.GetAttrIsNull(const Name: _string): boolean;
var
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  GetAttribute(Name, Value, Ind, IndPtr);
  Result := Ind <> OCI_IND_NOTNULL;
end;

procedure TOraObject.SetAttrIsNull(const Name: _string; Value: boolean);
var
  ValuePtr, IndPtr: IntPtr;
  Ind: OCIInd;
begin
  if Value then begin
    GetAttribute(Name, ValuePtr, Ind, IndPtr);
    if IndPtr <> nil then
      Marshal.WriteInt16(IndPtr, OCI_IND_NULL);
  end
  else
    GetAttribute(Name, ValuePtr, Ind, IndPtr, True, True);
end;

function TOraObject.GetAsOCIDate(Name: _string): OCIDate;
var
  Attr: TAttribute;
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  Attr := ObjectType.AttributeByName(Name);

  case Attr.DataType of
    dtDateTime: begin
      GetAttribute(Name, Value, Ind, IndPtr);
    {$IFDEF CLR}
      Result := OCIDate(Marshal.PtrToStructure(Value, typeof(OCIDate)));
    {$ELSE}
      Move(Value^, Result, sizeof(OCIDate));
    {$ENDIF}
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCIDate');
    Result.OCIDateYYYY := 0; // anti warning
  end;
end;

procedure TOraObject.SetAsOCIDate(Name: _string; Value: OCIDate);
var
  Attr: TAttribute;
  ValuePtr, IndPtr: IntPtr;
  Ind: OCIInd;
begin
  Attr := ObjectType.AttributeByName(Name);

  case Attr.DataType of
    dtDateTime: begin
      GetAttribute(Name, ValuePtr, Ind, IndPtr, True, True);
      Assert(ValuePtr <> nil);
    {$IFDEF CLR}
      Marshal.StructureToPtr(Value, ValuePtr, False);
    {$ELSE}
      Move(Value, ValuePtr^, sizeof(OCIDate));
    {$ENDIF}
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCIDate');
  end;
end;

function TOraObject.GetAsOCINumber(Name: _string): OCINumber;
var
  Attr: TAttribute;
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  Attr := ObjectType.AttributeByName(Name);

  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      GetAttribute(Name, Value, Ind, IndPtr);
    {$IFDEF CLR}
      Marshal.Copy(Value, Result.OCINumberPart, 0, OCI_NUMBER_SIZE);
    {$ELSE}
      Move(Value^, Result.OCINumberPart, OCI_NUMBER_SIZE);
    {$ENDIF}
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
    Result.OCINumberPart[0] := 0; // anti warning
  end;
end;

procedure TOraObject.SetAsOCINumber(Name: _string; const Value: OCINumber);
var
  Attr: TAttribute;
  ValuePtr, IndPtr: IntPtr;
  Ind: OCIInd;
begin
  Attr := ObjectType.AttributeByName(Name);

  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      GetAttribute(Name, ValuePtr, Ind, IndPtr, True, True);
      Assert(ValuePtr <> nil);
    {$IFDEF CLR}
      Marshal.Copy(Value.OCINumberPart, 0, ValuePtr, OCI_NUMBER_SIZE);
    {$ELSE}
      Move(Value.OCINumberPart, ValuePtr^, OCI_NUMBER_SIZE);
    {$ENDIF}
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
  end;
end;

function TOraObject.GetAsOCIString(Name: _string): pOCIString;
var
  Attr: TAttribute;
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  Attr := ObjectType.AttributeByName(Name);

  case Attr.DataType of
    dtString, dtWideString: begin
      GetAttribute(Name, Value, Ind, IndPtr);
      Result := Marshal.ReadIntPtr(Value)
      {if Ind >= OCI_IND_NOTNULL then
        Result := pOCIString(Value^)}
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCIString');
  end;
end;

procedure TOraObject.SetAsOCIString(Name: _string; Value: pOCIString);
var
  Attr: TAttribute;
  lOCIString: ppOCIString;
  Ind: OCIInd;
  IndPtr: IntPtr;
begin
  Attr := ObjectType.AttributeByName(Name);

  case Attr.DataType of
    dtString, dtWideString: begin
      GetAttribute(Name, lOCIString, Ind, IndPtr, True, True);
      Check(OCIStringAssign(FOCIEnv, FOCIError, Value, lOCIString));
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCIString');
  end;
end;

function TOraObject.GetAsDateTime(Name: _string): TDateTime;
var
  lOCIDate: OCIDate;
  Time: TDateTime;
begin
  if not AttrIsNull[Name] then begin
    lOCIDate := AttrAsOCIDate[Name];

    Result := EncodeDate(lOCIDate.OCIDateYYYY, lOCIDate.OCIDateMM, lOCIDate.OCIDateDD);
    Time := EncodeTime(lOCIDate.OCIDateTime.OCITimeHH, lOCIDate.OCIDateTime.OCITimeMI, lOCIDate.OCIDateTime.OCITimeSS, 0);
    if Result < 0 then
      Result := Result - Time
    else
      Result := Result + Time;
  end
  else
    Result := 0;
end;

procedure TOraObject.SetAsDateTime(Name: _string; Value: TDateTime);
var
  Year, Month, Day: word;
  Hour, Min, Sec, MSec: word;
  lOCIDate: OCIDate;
begin
  DecodeDate(Value, Year, Month, Day);
  DecodeTime(Value, Hour, Min, Sec, MSec);

  lOCIDate.OCIDateYYYY := Year;
  lOCIDate.OCIDateMM := Month;
  lOCIDate.OCIDateDD := Day;
  lOCIDate.OCIDateTime.OCITimeHH := Hour;
  lOCIDate.OCIDateTime.OCITimeMI := Min;
  lOCIDate.OCIDateTime.OCITimeSS := Sec;

  AttrAsOCIDate[Name] := lOCIDate;
end;

function TOraObject.GetAsFloat(Name: _string): double;
var
  Attr: TAttribute;
  Ind: OCIInd;
  Value: IntPtr;
  IndPtr: IntPtr;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  if not AttrIsNull[Name] then begin
    Attr := ObjectType.AttributeByName(Name);
    case Attr.DataType of
      dtInteger, dtLargeInt, dtFloat: begin
        GetAttribute(Name, Value, Ind, IndPtr);
        case Attr.SubDataType of
          dtBDouble:
            Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Value));
          dtBFloat: begin
          {$IFNDEF CLR}
            Result := Single(Value^);
          {$ELSE}
            SetLength(b, 4);
            Marshal.Copy(Value, b, 0, 4);
            Result := BitConverter.ToSingle(b, 0);
          {$ENDIF}
          end;
        else
          Check(OCINumberToReal(FOCIError, Value, SizeOf(Double), Result));
        end;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'OCINumber');
      Result := 0; // anti warning
    end;
  end
  else
    Result := 0;
end;

procedure TOraObject.SetAsFloat(Name: _string; Value: double);
var
  Attr: TAttribute;
  ValuePtr, IndPtr: IntPtr;
  Ind: OCIInd;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  Attr := ObjectType.AttributeByName(Name);
  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      GetAttribute(Name, ValuePtr, Ind, IndPtr, True, True);
      Assert(ValuePtr <> nil);
      case Attr.SubDataType of
        dtBDouble:
          Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
        dtBFloat: begin
        {$IFNDEF CLR}
          Single(ValuePtr^) := Value;
        {$ELSE}
          b := BitConverter.GetBytes(Single(Value));
          Marshal.Copy(b, 0, ValuePtr, 4);
        {$ENDIF}
        end;
      else
        Check(OCINumberFromReal(FOCIError, Value, SizeOf(Double), ValuePtr));
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
  end;
end;

function TOraObject.GetAsInteger(Name: _string): integer;
var
  Attr: TAttribute;
  Ind: OCIInd;
  Value: IntPtr;
  IndPtr: IntPtr;
  Val: int64;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  if not AttrIsNull[Name] then begin
    Attr := ObjectType.AttributeByName(Name);
    case Attr.DataType of
      dtInteger, dtLargeInt, dtFloat: begin
        GetAttribute(Name, Value, Ind, IndPtr);
        case Attr.SubDataType of
          dtBDouble:
            Result := Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Value)));
          dtBFloat: begin
          {$IFNDEF CLR}
            Result := Trunc(Single(Value^));
          {$ELSE}
            SetLength(b, 4);
            Marshal.Copy(Value, b, 0, 4);
            Result := Trunc(BitConverter.ToSingle(b, 0));
          {$ENDIF}
          end;
        else
          Check(OCINumberToInt(FOCIError, Value, SizeOf(Integer), OCI_NUMBER_SIGNED, Val));
          Result := Integer(Val);
        end;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'OCINumber');
    end;
  end
  else
    Result := 0;
end;

procedure TOraObject.SetAsInteger(Name: _string; Value: integer);
var
  Attr: TAttribute;
  ValuePtr, IndPtr: IntPtr;
  Ind: OCIInd;
  Val: int64;
{$IFDEF CLR}
  b: TBytes;
  s: single;
{$ENDIF}
begin
  Attr := ObjectType.AttributeByName(Name);
  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      GetAttribute(Name, ValuePtr, Ind, IndPtr, True, True);
      Assert(ValuePtr <> nil);
      case Attr.SubDataType of
        dtBDouble:
          Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
        dtBFloat: begin
        {$IFNDEF CLR}
          Single(ValuePtr^) := Value;
        {$ELSE}
          s := Value;
          b := BitConverter.GetBytes(s);
          Marshal.Copy(b, 0, ValuePtr, 4);
        {$ENDIF}
        end;
      else
        Val := Value;
        Check(OCINumberFromInt(FOCIError, Val, SizeOf(Integer), OCI_NUMBER_SIGNED, ValuePtr));
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
  end;
end;

function TOraObject.GetAsLargeInt(Name: _string): int64;
var
  Attr: TAttribute;
  Ind: OCIInd;
  Value: IntPtr;
  IndPtr: IntPtr;
  Val: int64;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  if not AttrIsNull[Name] then begin
    Attr := ObjectType.AttributeByName(Name);
    case Attr.DataType of
      dtInteger, dtLargeInt, dtFloat: begin
        GetAttribute(Name, Value, Ind, IndPtr);
        case Attr.SubDataType of
          dtBDouble:
            Result := Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Value)));
          dtBFloat: begin
          {$IFNDEF CLR}
            Result := Trunc(Single(Value^));
          {$ELSE}
            SetLength(b, 4);
            Marshal.Copy(Value, b, 0, 4);
            Result := Trunc(BitConverter.ToSingle(b, 0));
          {$ENDIF}
          end;
        else
          Check(OCINumberToInt(FOCIError, Value, SizeOf(Int64), OCI_NUMBER_SIGNED, Val));
          Result := Val;
        end;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'OCINumber');
    end;
  end
  else
    Result := 0;
end;

procedure TOraObject.SetAsLargeInt(Name: _string; Value: int64);
var
  Attr: TAttribute;
  ValuePtr, IndPtr: IntPtr;
  Ind: OCIInd;
{$IFDEF CLR}
  b: TBytes;
  s: single;
{$ENDIF}
begin
  Attr := ObjectType.AttributeByName(Name);
  case Attr.DataType of
    dtInteger, dtLargeInt, dtFloat: begin
      GetAttribute(Name, ValuePtr, Ind, IndPtr, True, True);
      Assert(ValuePtr <> nil);
      case Attr.SubDataType of
        dtBDouble:
          Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
        dtBFloat: begin
        {$IFNDEF CLR}
          Single(ValuePtr^) := Value;
        {$ELSE}
          s := Value;
          b := BitConverter.GetBytes(s);
          Marshal.Copy(b, 0, ValuePtr, 4);
        {$ENDIF}
        end;
      else
        Check(OCINumberFromInt(FOCIError, Value, SizeOf(Int64), OCI_NUMBER_SIGNED, ValuePtr));
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'OCINumber');
  end;
end;

function TOraObject.GetAsString(Name: _string): string;
begin
{$IFDEF VER12P}
  Result := AttrAsWideString[Name];
{$ELSE}
{$IFDEF CLR}
  Result := AttrAsWideString[Name];
{$ELSE}
  Result := AttrAsAnsiString[Name];
{$ENDIF}
{$ENDIF}
end;

procedure TOraObject.SetAsString(Name: _string; const Value: string);
begin
{$IFDEF VER12P}
  AttrAsWideString[Name] := Value;
{$ELSE}
{$IFDEF CLR}
  AttrAsWideString[Name] := Value;
{$ELSE}
  AttrAsAnsiString[Name] := Value;
{$ENDIF}
{$ENDIF}
end;

function TOraObject.GetAsAnsiString(Name: _string): AnsiString;
var
  lOCIString: pOCIString;
  Attr: TAttribute;
begin
  if not AttrIsNull[Name] then begin
    lOCIString := AttrAsOCIString[Name];
    Attr := ObjectType.AttributeByName(Name);
    case Attr.DataType of
      dtString:
        Result := Marshal.PtrToStringAnsi(OCIStringPtr(FOCIEnv, lOCIString));
      dtWideString:
        Result := AnsiString(Marshal.PtrToStringUni(OCIStringPtr(FOCIEnv, lOCIString)));
    end;
  end
  else
    Result := '';
end;

procedure TOraObject.SetAsAnsiString(Name: _string; const Value: AnsiString);
var
  Attr: TAttribute;
  lOCIString: ppOCIString;
  Ind: OCIInd;
  IndPtr: IntPtr;
  ValuePtr: IntPtr;
  Res: integer;
begin
  Attr := ObjectType.AttributeByName(Name);

  case Attr.DataType of
    dtString: begin
      GetAttribute(Name, lOCIString, Ind, IndPtr, True, True);
      Assert(lOCIString <> nil);
      ValuePtr := Marshal.StringToHGlobalAnsi(Value);
      Res := OCIStringResize(FOCIEnv, FOCIError, Length(Value) + 1, lOCIString); // bug in OCI 8.1
      if Res = 0 then
        Res := OCIStringAssignText(FOCIEnv, FOCIError, ValuePtr, Length(Value), lOCIString);
      FreeString(ValuePtr);
      Check(Res);
    end;
    dtWideString:
      AttrAsWideString[Name] := WideString(Value);
  else
    raise EConvertError.Create(SCannotConvert + 'OCIString');
  end;
end;

function TOraObject.GetAsWideString(Name: _string): WideString;
var
  lOCIString: pOCIString;
  Attr: TAttribute;
begin
  if not AttrIsNull[Name] then begin
    lOCIString := AttrAsOCIString[Name];
    Attr := ObjectType.AttributeByName(Name);
    case Attr.DataType of
      dtString:
        Result := WideString(Marshal.PtrToStringAnsi(OCIStringPtr(FOCIEnv, lOCIString)));
      dtWideString:
        Result := Marshal.PtrToStringUni(OCIStringPtr(FOCIEnv, lOCIString));
    end;
  end
  else
    Result := '';
end;

procedure TOraObject.SetAsWideString(Name: _string; const Value: WideString);
var
  Attr: TAttribute;
  lOCIString: ppOCIString;
  Ind: OCIInd;
  IndPtr: IntPtr;
  ValuePtr: IntPtr;
  Res: integer;
begin
  Attr := ObjectType.AttributeByName(Name);

  case Attr.DataType of
    dtWideString: begin
      GetAttribute(Name, lOCIString, Ind, IndPtr, True, True);
      Assert(lOCIString <> nil);
      ValuePtr := Marshal.StringToHGlobalUni(Value);
      Res := OCIStringAssignText(FOCIEnv, FOCIError, ValuePtr, Length(Value) * 2, lOCIString);
      FreeString(ValuePtr);
      Check(Res);
    end;
    dtString:
      AttrAsAnsiString[Name] := AnsiString(Value);
  else
    raise EConvertError.Create(SCannotConvert + 'OCIString');
  end;
end;

function TOraObject.GetChildObject(Attr: TAttribute; ArrayIndex: integer = -1): TSharedObject;
var
  Index: integer;
  Value: IntPtr;
  Ind: OCIInd;
  IndPtr: IntPtr;
  AttrName: _string;
begin
  if ArrayIndex = -1 then
  begin
    AttrName := Attr.Name;
    Index := Attr.AttributeNo;
  end
  else
  begin
    AttrName := '[' + IntToStr(ArrayIndex) + ']';
    Index := ArrayIndex + 1;
  end;  

  case Attr.DataType of
    dtObject, dtReference, dtArray, dtTable: begin
      if (FObjects <> nil) and (Index <= FObjects.Count) then
        Result := TSharedObject(FObjects[Index - 1])
      else
        Result := nil;

      if Result = nil then begin
        if FObjects = nil then
          FObjects := TList.Create;

        case Attr.DataType of
          dtObject:
            Result := TOraObject.Create(TOraType(Attr.ObjectType));
          dtReference: begin
            Result := TOraRef.Create(TOraType(Attr.ObjectType));
            GetAttribute(AttrName, Value, Ind, IndPtr);
            if Value = nil then
              TOraRef(Result).OCIRef := nil
            else
              TOraRef(Result).OCIRef := Marshal.ReadIntPtr(Value);

            if not TOraRef(Result).IsNull then
              TOraRef(Result).Pin;  // ??? optim
          end;
          dtArray:
            Result := TOraArray.Create(TOraType(Attr.ObjectType));
          dtTable:
            Result := TOraNestTable.Create(TOraType(Attr.ObjectType));
        else
          raise EConvertError.Create(SCannotConvert + 'Object');
        end;
        TOraObject(Result).FOwner := Self;
        //Result.ObjectType := TOraType(Attr.ObjectType);
        TOraObject(Result).OCISvcCtx := FSvcCtx;

        if Index > FObjects.Count then
          FObjects.Count := Index + 1;
        FObjects[Index - 1] := Result;
      end;
    end;
    dtOraBlob, dtOraClob, dtWideOraClob: begin
      if (FObjects <> nil) and (Index <= FObjects.Count) then
        Result := TSharedObject(FObjects[Index - 1])
      else
        Result := nil;

      if Result = nil then begin
        if FObjects = nil then
          FObjects := TList.Create;

        Result := TOraLob.Create(FSvcCtx);
        if Attr.DataType = dtWideOraClob then
          TOraLob(Result).IsUnicode := True;
        GetAttribute(AttrName, Value, Ind, IndPtr);
        TOraLob(Result).OCILobLocator := Marshal.ReadIntPtr(Value);
        if Ind = OCI_IND_NOTNULL then
          TOraLob(Result).ReadLob;

        if Index > FObjects.Count then
          FObjects.Count := Index + 1;
        FObjects[Index - 1] := Result;
      end;
    end;
    dtBFILE, dtCFILE: begin
      if (FObjects <> nil) and (Index <= FObjects.Count) then
        Result := TSharedObject(FObjects[Index - 1])
      else
        Result := nil;

      if Result = nil then begin
        if FObjects = nil then
          FObjects := TList.Create;

        Result := TOraFile.Create(FSvcCtx);
        GetAttribute(AttrName, Value, Ind, IndPtr);
        TOraFile(Result).OCILobLocator := Marshal.ReadIntPtr(Value);
        if Ind = OCI_IND_NOTNULL then begin
          TOraFile(Result).Open;
          TOraFile(Result).ReadLob;
          TOraFile(Result).Close;
        end;

        if Index > FObjects.Count then
          FObjects.Count := Index + 1;
        FObjects[Index - 1] := Result;
      end;
    end;
    dtTimeStamp, dtTimeStampTZ, dtTimeStampLTZ: begin
      if (FObjects <> nil) and (Index <= FObjects.Count) then
        Result := TSharedObject(FObjects[Index - 1])
      else
        Result := nil;

      if Result = nil then begin
        if FObjects = nil then
          FObjects := TList.Create;

        Result := TOraTimeStamp.Create(Attr.DataType);
        GetAttribute(AttrName, Value, Ind, IndPtr);
        TOraTimeStamp(Result).OCIDateTime := Marshal.ReadIntPtr(Value);
        TOraTimeStamp(Result).IsNull := Ind <> OCI_IND_NOTNULL;

        if Index > FObjects.Count then
          FObjects.Count := Index + 1;
        FObjects[Index - 1] := Result;
      end;
    end;
    dtIntervalYM, dtIntervalDS: begin
      if (FObjects <> nil) and (Index <= FObjects.Count) then
        Result := TSharedObject(FObjects[Index - 1])
      else
        Result := nil;

      if Result = nil then begin
        if FObjects = nil then
          FObjects := TList.Create;

        Result := TOraInterval.Create(Attr.DataType);
        GetAttribute(AttrName, Value, Ind, IndPtr);
        TOraInterval(Result).OCIInterval := Marshal.ReadIntPtr(Value);
        TOraInterval(Result).IsNull := Ind <> OCI_IND_NOTNULL;

        if Index > FObjects.Count then
          FObjects.Count := Index + 1;
        FObjects[Index - 1] := Result;
      end;
    end;
  else
    Result := nil;
  end;
end;

function TOraObject.GetComplexAttribute(const Name: _string): TSharedObject;
var
  Attr: TAttribute;
  i: integer;
  iPos: integer;
  Index: integer;
  IndexPos: integer;
begin
  iPos := Pos('.', Name);
  if iPos > 0 then
    Result := GetAsObject(copy(Name, 1, iPos - 1)).GetComplexAttribute(copy(Name, iPos + 1, Length(Name)-iPos))
  else begin
    CheckAlloc;

    Attr := ObjectType.AttributeByName(Name);

    Index := -1;
    IndexPos := Pos('[', Name);
    if IndexPos = 1 then begin
      i := Pos(']', Name);
      if i > 0 then
        Index := StrToInt(Copy(Name, 2, i - 2))
      else
        RaiseError(SInvalidAttrName);
    end;

    Result := GetChildObject(Attr, Index);
  end;
end;

function TOraObject.GetAsObject(Name: _string): TOraObject;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraObject then
    Result := TOraObject(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Object');
end;

function TOraObject.GetAsArray(Name: _string): TOraArray;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraArray then
    Result := TOraArray(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Array');
end;

function TOraObject.GetAsLob(Name: _string): TOraLob;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraLob then
    Result := TOraLob(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Lob');
end;

function TOraObject.GetAsOraTimeStamp(Name: _string): TOraTimeStamp;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraTimeStamp then
    Result := TOraTimeStamp(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'TimeStamp');
end;

function TOraObject.GetAsOraInterval(Name: _string): TOraInterval;
var
  Res: TSharedObject;
begin
  Res := GetComplexAttribute(Name);
  if Res is TOraInterval then
    Result := TOraInterval(Res)
  else
    raise EConvertError.Create(SCannotConvert + 'Interval');
end;

procedure TOraObject.SetInstance(Value: IntPtr);
begin
  if Value <> FInstance then begin
    FreeObject;
    FInstance := Value;
  end;
end;

function TOraObject.GetFInstance: IntPtr;
begin
  Result := Marshal.ReadIntPtr(FInstancePtr);
end;

procedure TOraObject.SetFInstance(Value: IntPtr);
begin
  Marshal.WriteIntPtr(FInstancePtr, Value);
end;

function TOraObject.GetFIndicator: IntPtr;
begin
  Result := Marshal.ReadIntPtr(FIndicatorPtr);
end;

procedure TOraObject.SetFIndicator(Value: IntPtr);
begin
  Marshal.WriteIntPtr(FIndicatorPtr, Value);
end;

procedure TOraObject.SetOCISvcCtx(Value: pOCISvcCtx);
var
  ValuePtr: IntPtr;
begin
  if FOCIError = nil then
    FOCIError := {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF}.hOCIError;
  if Value <> FSvcCtx then begin
    {if FSvcCtx <> nil then
      FreeObject;}

    FSvcCtx := Value;
    FUnicodeEnv := False;
    if FSvcCtx <> nil then begin
      ValuePtr := OrdinalToPtr(FOCIEnv);
      try
        Check(OCIAttrGet1(FSvcCtx, OCI_HTYPE_SVCCTX, ValuePtr, nil, OCI_ATTR_ENV, FOCIError));
      finally
        PtrToOrdinal(ValuePtr, FOCIEnv);
      end;
      FUnicodeEnv := IsUnicodeEnv(FOCIEnv, FOCIError);
    end;
  end;
end;

procedure TOraObject.SetObjectType(Value: TOraType);
begin
  if Value <> FObjectType then begin
    FreeObject;

    inherited SetObjectType(Value);

    FObjectType := Value;
    if Value <> nil then
      OCISvcCtx := Value.FSvcCtx;
  end;
end;

{ TOraRef }

constructor TOraRef.Create(ObjectType: TOraType);
begin
  inherited Create(ObjectType);

  FpOCIRef := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(FpOCIRef, nil);
end;

destructor TOraRef.Destroy;
begin
  Marshal.FreeHGlobal(FpOCIRef);

  inherited;
end;

procedure TOraRef.Pin;
var
  Res: sword;
begin
  Assert(FInstance = nil);

  if IsNull{FOCIRef = nil} then
    RaiseError('OCIRef is not assigned');

  if FInstance = nil then begin
    Res := OCIObjectPin(FOCIEnv, FOCIError, FOCIRef, nil, OCI_PIN_ANY, OCI_DURATION_SESSION,
      OCI_LOCK_NONE, FInstancePtr);
    if Res <> OCI_SUCCESS then begin // WAR
      InvalidObject := True;
      Exit;
    end;
  end;
  if FIndicator = nil then
    Check(OCIObjectGetInd(FOCIEnv, FOCIError, FInstance, FIndicatorPtr));
end;

procedure TOraRef.Unpin;
begin
  inherited;
  FreeObject;
end;

procedure TOraRef.Assign(Source: TOraObject);
var
  Val: pOCIRef;
begin
   if Source is TOraRef then begin
     if TOraRef(Source).RefIsNull then
       Clear
     else begin
       Val := nil;
       Check(OCIRefAssign(FOCIEnv, FOCIError, TOraRef(Source).OCIRef, Val));
       FOCIRef := Val;
     end;
   end
   else
     inherited;
end;

function TOraRef.RefIsNull: boolean;
begin
  Result := OCIRefIsNull(FOCIEnv, FOCIRef) = 1;
end;

procedure TOraRef.Clear;
begin
  if FOCIRef <> nil then begin
    Check(OCIRefClear(FOCIEnv, FOCIRef));
    FOCIRef := nil;
    InvalidObject := False;
  end;
end;

function TOraRef.GethOCIRef: pOCIRef;
begin
  Result := Marshal.ReadIntPtr(FpOCIRef);
end;

procedure TOraRef.SethOCIRef(Value: pOCIRef);
begin
  Marshal.WriteIntPtr(FpOCIRef, Value);
end;

procedure TOraRef.SetOCIRef(Value: pOCIRef);
begin
  if Value <> FOCIRef then begin
    FreeObject;
    FOCIRef := Value;
    InvalidObject := False;
  end;
end;

function TOraRef.GetOCIRefPtr: ppOCIRef;
begin
  Result := FpOCIRef;
end;

function TOraRef.GetIsNull: boolean;
begin
  if FOCIRef <> nil then begin
    Result := RefIsNull or InvalidObject;
  end
  else
    Result := True;
end;

procedure TOraRef.SetIsNull(Value: boolean);
begin
  if Value then
    Clear;
end;

function TOraRef.GetAsHex: string;
var
  Len: cardinal;
  Ptr: IntPtr;
begin
  if FOCIRef <> nil then begin
    Len := 1000;
    Ptr := Marshal.AllocHGlobal(Len);
    try
      Check(OCIRefToHex(FOCIEnv, FOCIError, FOCIRef, Ptr, Len));
      Result := string(Marshal.PtrToStringAnsi(Ptr, Len));
    finally
      Marshal.FreeHGlobal(Ptr);
    end;
  end
  else
    Result := '';
end;

{ TOraXML }

constructor TOraXML.Create(ObjectType: TOraType = nil);
begin
  if (ObjectType <> nil) and (ObjectType.DataType <> dtXML) then
    RaiseError('Type of object must be XML');

  inherited Create(ObjectType);

  CacheValue :=nil;
  phOCIDescriptor := nil;
  LocalIndicator := Marshal.AllocHGlobal(sizeof(Word));
  Marshal.WriteInt16(LocalIndicator, OCI_IND_NULL);
  LocalIndicatorPtr := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(LocalIndicatorPtr, LocalIndicator);
end;

destructor TOraXML.Destroy;
begin
  Marshal.FreeHGlobal(LocalIndicator);
  Marshal.FreeHGlobal(LocalIndicatorPtr);

  inherited;
end;

function TOraXML.ObjectIsNull: boolean;
begin
  Result := (Marshal.ReadInt16(LocalIndicator) <> OCI_IND_NOTNULL) and (inherited GetIsNull);
end;

function TOraXML.GetIsNull: boolean;
begin
  Result := CacheValue = nil;
end;

procedure TOraXML.SetIsNull(Value: boolean);
begin
  FreeObjects;
  Marshal.WriteInt16(LocalIndicator, OCI_IND_NULL);
  if CacheValue <> nil then begin
    Marshal.FreeHGlobal(CacheValue);
    CacheValue := nil;
  end;
end;

function TOraXML.GetIndicatorPtr: IntPtr;
begin
  if Marshal.ReadInt16(LocalIndicator) = OCI_IND_NOTNULL then
    Result := LocalIndicatorPtr
  else
    Result := inherited GetIndicatorPtr;
end;

procedure TOraXML.CheckType;
var
  ObjType: TOraType;
begin
  if FObjectType = nil then begin
    ObjType := nil;
    if ObjectTypes <> nil then begin
      ObjType := TOraType(ObjectTypes.FindType(FSvcCtx, 'SYS.XMLTYPE'));
      SetObjectType(ObjType);
    end;
    if ObjType = nil then begin
      ObjType := TOraType.Create(FSvcCtx, 'SYS.XMLTYPE');
      SetObjectType(ObjType);
      ObjectType.Release;
    end;
  end
  else begin
    if FObjectType.DataType <> dtXML  then
      RaiseError('Type of object must be XML');
  end;
end;

procedure TOraXML.CreateXMLStream;
begin
  if phOCIDescriptor = nil then
  begin
    CheckAlloc;
    Check(OCIDescriptorAlloc(FOCIEnv, phOCIDescriptor, $4A, 0, nil));
    try
      Check(OCIPStreamFromXMLType(FOCIError, phOCIDescriptor, FInstance, 0));
    except
      Check(OCIDescriptorFree( phOCIDescriptor, $4A));
      phOCIDescriptor := nil;
      raise;
    end;
  end;
end;

procedure TOraXML.FreeXMLStream;
begin
  if phOCIDescriptor <> nil then
  begin
    Check(OCIPStreamClose(FOCIError, phOCIDescriptor));
    Check(OCIDescriptorFree( phOCIDescriptor, $4A));
    phOCIDescriptor := nil;

    FNativeInstance := True;     //Fixed bug with memory leak
    inherited FreeObject();
  end;
end;

function TOraXML.GetAsString: string;
begin
  Result := '';
  if CacheValue <> nil then
    Result := string(Marshal.PtrToStringAnsi(CacheValue))
  else
    Result := '';
end;

procedure TOraXML.SetAsString(Value: string);
begin
  CheckType;
  FreeObject;
  if Value <> '' then begin
    CacheValue := Marshal.AllocHGlobal(Length(Value) + 1);
    StrCopy(CacheValue, Marshal.StringToHGlobalAnsi(AnsiString(Value)));
    Marshal.WriteByte(CacheValue, Length(Value), 0);
  end;
end;

procedure TOraXML.AllocObject;
var
  OraLob: TOraLob;
  OCIStr: pOCIString;
  lOCIStr: ppOCIString;
  Instance: IntPtr;
begin
  CheckType;
  CheckSession;

  if FOwner = nil then begin
    if FInstance = nil then begin
      if not IsNull then begin
        if (CacheValue <> nil) and (StrLen(CacheValue) > 4000) then begin
          OraLob := TOraLob.Create(FSvcCtx);
          try
            OraLob.CreateTemporary(ltClob);
            OraLob.Write(0, StrLen(CacheValue), CacheValue);
            OraLob.WriteLob;
            Check(OCIXMLTypeCreateFromSrc(FSvcCtx, FOCIError, OCI_DURATION_SESSION,
              2, OraLob.OCILobLocator, 0, Instance));
            FInstance := Instance;
          finally
            OraLob.Free;
          end;
        end
        else begin
          Check(OCIObjectNew(FOCIEnv, FOCIError, FSvcCtx, OCI_TYPECODE_VARCHAR, nil, nil,
            OCI_DURATION_SESSION, 1, OCIStr));
          lOCIStr := OrdinalToPtr(OCIStr);
          try
            Check(OCIStringAssignText(FOCIEnv, FOCIError, CacheValue, StrLen(CacheValue), lOCIStr));
            Check(OCIXMLTypeCreateFromSrc(FSvcCtx, FOCIError, OCI_DURATION_SESSION,
              1, Marshal.ReadIntPtr(lOCIStr), 0, Instance));
            FInstance := Instance;
          finally
            PtrToOrdinal(lOCIStr, OCIStr);
            Check(OCIObjectFree(FOCIEnv, FOCIError, OCIStr, OCI_OBJECTFREE_FORCE));
          end;
        end;

        if (FIndicator = nil) and (FInstance <> nil) then begin
          Check(OCIObjectGetInd(FOCIEnv, FOCIError, FInstance, FIndicatorPtr));
          if OCIVersion >= 10102 then
            Marshal.WriteInt16(LocalIndicator, OCI_IND_NOTNULL);
        end;
      end
      else begin
        Check(OCIXMLTypeNew(FSvcCtx, FOCIError, OCI_DURATION_SESSION,
                 '', 0, '', 0, Instance));
        FInstance := Instance;
        if (FIndicator = nil) and (FInstance <> nil) then
           Check(OCIObjectGetInd(FOCIEnv, FOCIError, FInstance, FIndicatorPtr));
      end;

      FNativeInstance := True;
    end;
  end
  else
    FOwner.ChildAlloc(Self);
end;

procedure TOraXML.AllocObject(SvcCtx: pOCISvcCtx; OraLob: TOraLob);
var
  Instance: IntPtr;
begin
  FreeObject;

  OCISvcCtx := SvcCtx;
  CheckType;
  CheckSession;

  if FOwner = nil then begin
    Check(OCIXMLTypeCreateFromSrc(FSvcCtx, FOCIError, OCI_DURATION_SESSION,
      2, OraLob.OCILobLocator, 0, Instance));
    FInstance := Instance;

    FNativeInstance := True;
    if (FIndicator = nil) and (FInstance <> nil) then
      Check(OCIObjectGetInd(FOCIEnv, FOCIError, FInstance, FIndicatorPtr));

    // Read XML to CashedValue if OraXML is inited by OraLob
    ReadXML;
  end
  else
    FOwner.ChildAlloc(Self);
end;

procedure TOraXML.AllocObject(TypeName: _string);
begin
  if QuotedSQLName(TypeName) <> '"SYS"."XMLTYPE"' then
    RaiseError('Type of object must be XML');

  inherited AllocObject(TypeName);
end;

procedure TOraXML.AllocObject(SvcCtx: pOCISvcCtx; TypeName: _string);
begin
  if QuotedSQLName(TypeName) <> '"SYS"."XMLTYPE"' then
    RaiseError('Type of object must be XML');

  inherited AllocObject(SvcCtx, TypeName);
end;

procedure TOraXML.FreeObject(FreeChild: boolean = True);
begin
  FreeXMLStream;
  if CacheValue <> nil then begin
    Marshal.FreeHGlobal(CacheValue);
    CacheValue := nil;

    inherited FreeObject(FreeChild);
  end
  else begin
    FInstance := nil;
    FIndicator := nil;
    FNativeInstance := False;
  end;
end;

procedure TOraXML.Assign(Source: TOraObject);
begin
  FreeObject;
  with TOraXML(Source) do begin
    Self.SetAsString(AsString);
  end;
end;

procedure TOraXML.StartRead;
begin
  FreeXMLStream;
  CreateXMLStream;
end;

function TOraXML.Read(Count: cardinal; Dest: IntPtr): cardinal;
var
  Len: int64;
begin
  Result := 0;
  if ObjectIsNull then begin // to avoid error on OCI function call
    Exit;
  end;
  CreateXMLStream;
  Len := Count;
  Check(OCIPStreamRead(FOCIError, phOCIDescriptor, Dest, Len, 0));
  Result := Len;
end;

procedure TOraXML.ReadXML;
var
  TotalReaded, Readed: integer;
  Buffer, Ptr: IntPtr;
begin
  if CacheValue <> nil then begin
    Marshal.FreeHGlobal(CacheValue);
    CacheValue := nil;
  end;

  if ObjectIsNull then
    Exit;

  TotalReaded := 0;
  StartRead;
  Buffer := Marshal.AllocHGlobal(DefaultPieceSize);
  try
    repeat
      Readed := Read(DefaultPieceSize-1, Buffer);
      Marshal.WriteByte(Buffer, Readed, 0);
      if Readed <> 0 then begin
        Ptr := Marshal.AllocHGlobal(Readed + TotalReaded + 1);
        if TotalReaded <> 0 then
          CopyBuffer(CacheValue, Ptr, TotalReaded);
        CopyBuffer(Buffer, PtrOffset(Ptr, TotalReaded), Readed + 1);
        if CacheValue <> nil then
          Marshal.FreeHGlobal(CacheValue);
        Inc(TotalReaded, Readed);
        CacheValue := Ptr;
      end;
    until Readed = 0;
  finally
    Marshal.FreeHGlobal(Buffer);
  end;
  FreeXMLStream;
end;

procedure TOraXML.LoadFromStream(Stream: TStream);
var
  Buffer: TBytes;
begin
  CheckType;
  FreeObject;

  if Stream.Size > 0 then begin
    CacheValue := Marshal.AllocHGlobal(Stream.Size + 1);
{$IFNDEF CLR}
    Buffer := IntPtr(CacheValue);
{$ELSE}
    SetLength(Buffer, Stream.Size + 1);
{$ENDIF}
    Stream.Seek(0, soFromBeginning);
    Stream.ReadBuffer(Buffer{$IFNDEF CLR}[0]{$ENDIF}, Stream.Size);
{$IFDEF CLR}
    Marshal.Copy(Buffer, 0, CacheValue, Stream.Size);
{$ENDIF}
    Marshal.WriteByte(CacheValue, Stream.Size, 0);
  end;
end;

procedure TOraXML.SaveToStream(Stream: TStream);
var
  Buffer: TBytes;
begin
//  if CacheIsNull then begin
//    if ObjectIsNull then
//      Exit;
//    ReadXML;
//  end;

  if CacheValue <> nil then begin
{$IFNDEF CLR}
    Buffer := IntPtr(CacheValue);
{$ELSE}
    SetLength(Buffer, StrLen(CacheValue));
    Marshal.Copy(CacheValue, Buffer, 0 , StrLen(CacheValue));
{$ENDIF}
    Stream.WriteBuffer(Buffer{$IFNDEF CLR}[0]{$ENDIF}, StrLen(CacheValue));
  end;
end;

procedure TOraXML.Extract(RetDoc: TOraXML; XPathExpr: string; NSmap: string);
var
  Instance: IntPtr;
begin
  if RetDoc = nil then
    Exit;
  AllocObject;
  RetDoc.FreeObject;
  RetDoc.CheckType;
  if IsNull then begin
    RetDoc.SetIsNull(True);
    exit;
  end;
  Check(OCIXMLTypeExtract(FOCIError,  FInstance, OCI_DURATION_SESSION,
    PAnsiChar(AnsiString(XPathExpr)), Length(XPathExpr),
    PAnsiChar(AnsiString(NSmap)), length(NSmap),
    Instance));
  RetDoc.FInstance := Instance;
  if (RetDoc.FIndicator = nil) and (RetDoc.FInstance <> nil) then begin
    Check(OCIObjectGetInd(RetDoc.FOCIEnv, FOCIError, RetDoc.FInstance, RetDoc.FIndicatorPtr));
    if OCIVersion >= 10102 then
      Marshal.WriteInt16(RetDoc.LocalIndicator, OCI_IND_NOTNULL);
  end;
end;

procedure TOraXML.Transform(XSLDoc: TOraXML; RetDoc: TOraXML);
var
  Instance: IntPtr;
begin
  if RetDoc = nil then
    Exit;
  AllocObject;
  RetDoc.FreeObject;
  RetDoc.CheckType;
  if IsNull then begin
    RetDoc.SetIsNull(True);
    exit;
  end;
  XSLDoc.AllocObject;
  Check(OCIXMLTypeTransform(FOCIError,  OCI_DURATION_SESSION, FInstance,
    XSLDoc.FInstance, Instance));
  RetDoc.FInstance := Instance;
  if (RetDoc.FIndicator = nil) and (RetDoc.FInstance <> nil) then begin
    Check(OCIObjectGetInd(RetDoc.FOCIEnv, FOCIError, RetDoc.FInstance, RetDoc.FIndicatorPtr));
    if OCIVersion >= 10102 then
      Marshal.WriteInt16(RetDoc.LocalIndicator, OCI_IND_NOTNULL);
  end;
end;

function TOraXML.Exists(XPathExpr: string; NSmap: string): boolean;
var
  Res: Longword;
begin
  if (OCIVersion = 9201) or (OCIVersion >= 10102) then
    RaiseError('Function not supported');
  AllocObject;
  Check(OCIXMLTypeExists(FOCIError, FInstance,
    PAnsiChar(AnsiString(XPathExpr)), Length(XPathExpr),
    PAnsiChar(AnsiString(NSmap)), length(NSmap),Res));
  Result := (Res <> 0);
end;

function TOraXML.Validate(SchemaURL: string): boolean;
var
  Res: Longword;
begin
  AllocObject;
  Check(OCIXMLTypeValidate(FOCIError, FInstance, PAnsiChar(AnsiString(SchemaURL)),
    Length(SchemaURL), Res));
  Result := (Res <> 0);
end;

function TOraXML.IsSchemaBased: boolean;
var
  Res: Longword;
begin
  AllocObject;
  Check(OCIXMLTypeIsSchemaBased(FOCIError, FInstance, Res));
  Result := (Res <> 0);
end;

procedure TOraXML.GetSchema(SchemaDoc: TOraXML; var SchemaURL: string; var RootElem: string);
var
  pSchemaURL, pRootelem: IntPtr;
  SchemaURL_Len, Rootelem_Len: ub4;
  Instance: IntPtr;
begin
  AllocObject;
  SchemaDoc.FreeObject;
  SchemaDoc.CheckType;
  if IsNull then begin
    SchemaDoc.SetIsNull(True);
    exit;
  end;

  Check(OCIXMLTypeGetSchema(FOCIError, FInstance,
    Instance,
    pSchemaURL, schemaURL_Len,
    pRootelem, Rootelem_Len));
  SchemaDoc.FInstance := Instance;
  if (SchemaDoc.FIndicator = nil) and (SchemaDoc.FInstance <> nil) then begin
    Check(OCIObjectGetInd(SchemaDoc.FOCIEnv, FOCIError, SchemaDoc.FInstance, SchemaDoc.FIndicatorPtr));
    if OCIVersion >= 10102 then
      Marshal.WriteInt16(SchemaDoc.LocalIndicator, OCI_IND_NOTNULL);
  end;

  if schemaURL_Len > 0 then
    SchemaURL := string(Marshal.PtrToStringAnsi(pSchemaURL, schemaURL_Len))
  else
    SchemaURL := '';
  if Rootelem_Len > 0 then
    RootElem := string(Marshal.PtrToStringAnsi(pRootelem, Rootelem_Len))
  else
    RootElem := '';
end;


{ TOraArray }

procedure TOraArray.CheckType;
begin
  inherited;

  if not (FObjectType.DataType in [dtArray,dtTable]) then
    RaiseError('Type of object must be Array');
end;

procedure TOraArray.CheckIndex(Index: integer);
begin
  if Index >= Size then
    Size := Index + 1;
end;

function TOraArray.AppendItem: integer;
var
  ItemInstance: IntPtr;
  Obj: TOraObject;
begin
  CheckAlloc;

  Obj := nil;
  case ItemType of
    dtDateTime:
      ItemInstance := NullOCIDate;
    dtInteger, dtLargeInt, dtFloat:
      ItemInstance := NullOCINumber;
    dtString, dtWideString:
      ItemInstance := NullOCIString;
    dtBlob, dtOraBlob, dtOraClob, dtNClob, dtWideOraClob: 
      ItemInstance := nil;
    dtObject: begin
      Obj := TOraObject.Create(TOraType(ObjectType.Attributes[0].ObjectType));
      ItemInstance := Obj.Instance;
      // Check(OCIObjectNew());
    end;
  else
    ItemInstance := nil;
    Assert(False, SUnknownDataType);
  end;

  try
    Check(OCICollAppend(FOCIEnv, FOCIError, ItemInstance, NullIndStruct, Instance));
  finally
    if ItemType = dtObject then
      Obj.Free;
  end;

  Check(OCICollSize(FOCIEnv, FOCIError, Instance, Result));
  Dec(Result);
end;

procedure TOraArray.InsertItem(Index: integer);
var
  i, ArrSize: integer;
begin
  ArrSize := Size;
  for i := ArrSize to Index do
    AppendItem;
end;

procedure TOraArray.ChildAlloc(Child: TOraObject);
var
  Exists: tbool;
  ObjectPtr: IntPtr;
  ItemIndicator: IntPtr;
  i: integer;
begin
  for i := 0 to FObjects.Count - 1 do
    if Child = FObjects[i] then begin
      InsertItem(i);

      Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, i, Exists,
        ObjectPtr, ItemIndicator));

      if Exists = 1 then begin
        Child.Instance := ObjectPtr;
        Child.Indicator := ItemIndicator;
      end;
    end;
end;

function TOraArray.GetItemExists(Index: integer): boolean;
var
  ItemInstance: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    ItemInstance, ItemIndicator));
  Result := Exists = 1;
end;

function TOraArray.GetItemIsNull(Index: integer): boolean;
var
  ItemInstance: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    ItemInstance, ItemIndicator));
  if Exists = 1 then
    Result := Marshal.ReadInt16(ItemIndicator) <> OCI_IND_NOTNULL
  else
    Result := False;
end;

procedure TOraArray.SetItemIsNull(Index: integer; Value: boolean);
var
  ItemInstance: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;
  CheckIndex(Index);

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    ItemInstance, ItemIndicator));

  if Value then
    Marshal.WriteInt16(ItemIndicator, OCI_IND_NULL)
  else
    Marshal.WriteInt16(ItemIndicator, OCI_IND_NOTNULL);

  Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, ItemInstance, ItemIndicator,
    Instance));
end;

function TOraArray.GetItemAsOCIString(Index: integer): pOCIString;
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    lOCIString, ItemIndicator));

  if Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL then
    Result := Marshal.ReadIntPtr(lOCIString)
  else
    Result := nil;
end;
procedure TOraArray.SetItemAsOCIString(Index: integer; Value: pOCIString);
begin
  CheckAlloc;
  CheckIndex(Index);

  Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, Value, NotNullInd,
    Instance));

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsDateTime(Index: integer): TDateTime;
var
  lOCIDate: pOCIDate;
  ItemIndicator: IntPtr;
  Exists: tbool;
  Time: TDateTime;
begin
  CheckAlloc;

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    lOCIDate, ItemIndicator));

  if Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL then
    case ItemType of
      dtDateTime: begin
        Result := EncodeDate(Marshal.ReadInt16(lOCIDate), Marshal.ReadByte(lOCIDate, 2), Marshal.ReadByte(lOCIDate, 3));
        Time := EncodeTime(Marshal.ReadByte(lOCIDate, 4), Marshal.ReadByte(lOCIDate, 5),
          Marshal.ReadByte(lOCIDate, 6), 0);
        if Result < 0 then
          Result := Result - Time
        else
          Result := Result + Time;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'Date');
    end
  else
    Result := 0;
end;

procedure TOraArray.SetItemAsDateTime(Index: integer; Value: TDateTime);
var
  lOCIDate: pOCIDate;
  Year, Month, Day: word;
  Hour, Min, Sec, MSec: word;
begin
  CheckAlloc;
  CheckIndex(Index);

  lOCIDate := Marshal.AllocHGlobal(sizeof(OCIDate));
  try
    case ItemType of
      dtDateTime: begin
        DecodeDate(Value, Year, Month, Day);
        DecodeTime(Value, Hour, Min, Sec, MSec);

        Marshal.WriteInt16(lOCIDate, Year);
        Marshal.WriteByte(lOCIDate, 2, Lo(Month));
        Marshal.WriteByte(lOCIDate, 3, Lo(Day));
        Marshal.WriteByte(lOCIDate, 4, Lo(Hour));
        Marshal.WriteByte(lOCIDate, 5, Lo(Min));
        Marshal.WriteByte(lOCIDate, 6, Lo(Sec));
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'Float');
    end;

    Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, lOCIDate, NotNullInd,
      Instance));
  finally
    Marshal.FreeHGlobal(lOCIDate);
  end;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsFloat(Index: integer): double;
var
  Elem: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  CheckAlloc;

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
        Elem, ItemIndicator));

      if (Exists = 1) and (Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL) then
        case ItemSubType of
          dtBDouble:
            Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Elem));
          dtBFloat: begin
          {$IFNDEF CLR}
            Result := Single(Elem^);
          {$ELSE}
            SetLength(b, 4);
            Marshal.Copy(Elem, b, 0, 4);
            Result := BitConverter.ToSingle(b, 0);
          {$ENDIF}
          end;
        else
          Check(OCINumberToReal(FOCIError, Elem, SizeOf(Double), Result));
        end
      else
        Result := 0;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Float');
  end;
end;

procedure TOraArray.SetItemAsFloat(Index: integer; Value: double);
var
  ValuePtr: IntPtr;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      ValuePtr := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
      try
        case ItemSubType of
          dtBDouble:
            Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
          dtBFloat: begin
          {$IFNDEF CLR}
            Single(ValuePtr^) := Value;
          {$ELSE}
            b := BitConverter.GetBytes(Single(Value));
            Marshal.Copy(b, 0, ValuePtr, 4);
          {$ENDIF}
          end;
        else
          Check(OCINumberFromReal(FOCIError, Value, SizeOf(Double), ValuePtr));
        end;

        Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, ValuePtr, NotNullInd,
          Instance));
        Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
      finally
        Marshal.FreeHGlobal(ValuePtr);
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Float');
  end;
end;

function TOraArray.GetItemAsInteger(Index: integer): integer;
var
  Elem: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
  Val: int64;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  CheckAlloc;

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
        Elem, ItemIndicator));

      if (Exists = 1) and (Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL) then
        case ItemSubType of
          dtBDouble:
            Result := Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Elem)));
          dtBFloat: begin
          {$IFNDEF CLR}
            Result := Trunc(Single(Elem^));
          {$ELSE}
            SetLength(b, 4);
            Marshal.Copy(Elem, b, 0, 4);
            Result := Trunc(BitConverter.ToSingle(b, 0));
          {$ENDIF}
          end;
        else
          Check(OCINumberToInt(FOCIError, Elem, SizeOf(Integer), OCI_NUMBER_SIGNED, Val));
          Result := Integer(Val);
        end
      else
        Result := 0;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Integer');
  end;
end;

procedure TOraArray.SetItemAsInteger(Index: integer; Value: integer);
var
  ValuePtr: IntPtr;
  Val: int64;
{$IFDEF CLR}
  b: TBytes;
  s: single;
{$ENDIF}
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      ValuePtr := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
      try
        case ItemSubType of
          dtBDouble:
            Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
          dtBFloat: begin
          {$IFNDEF CLR}
            Single(ValuePtr^) := Value;
          {$ELSE}
            s := Value;
            b := BitConverter.GetBytes(s);
            Marshal.Copy(b, 0, ValuePtr, 4);
          {$ENDIF}
          end;
        else
          Val := Value;
          Check(OCINumberFromInt(FOCIError, Val, SizeOf(Integer), OCI_NUMBER_SIGNED, ValuePtr));
        end;

        Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, ValuePtr, NotNullInd,
          Instance));
        Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
      finally
        Marshal.FreeHGlobal(ValuePtr);
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Integer');
  end;
end;

function TOraArray.GetItemAsLargeInt(Index: integer): int64;
var
  Elem: IntPtr;
  ItemIndicator: IntPtr;
  Exists: tbool;
{$IFDEF CLR}
  b: TBytes;
{$ENDIF}
begin
  CheckAlloc;

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
        Elem, ItemIndicator));

      if (Exists = 1) and (Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL) then
        case ItemSubType of
          dtBDouble:
            Result := Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Elem)));
          dtBFloat: begin
          {$IFNDEF CLR}
            Result := Trunc(Single(Elem^));
          {$ELSE}
            SetLength(b, 4);
            Marshal.Copy(Elem, b, 0, 4);
            Result := Trunc(BitConverter.ToSingle(b, 0));
          {$ENDIF}
          end;
        else
          Check(OCINumberToInt(FOCIError, Elem, SizeOf(Int64), OCI_NUMBER_SIGNED, Result));
        end
      else
        Result := 0;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Integer');
  end;
end;

procedure TOraArray.SetItemAsLargeInt(Index: integer; Value: int64);
var
  ValuePtr: IntPtr;
{$IFDEF CLR}
  b: TBytes;
  s: single;
{$ENDIF}
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtInteger, dtLargeInt, dtFloat: begin
      ValuePtr := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
      try
        case ItemSubType of
          dtBDouble:
            Marshal.WriteInt64(ValuePtr, BitConverter.DoubleToInt64Bits(Value));
          dtBFloat: begin
          {$IFNDEF CLR}
            Single(ValuePtr^) := Value;
          {$ELSE}
            s := Value;
            b := BitConverter.GetBytes(s);
            Marshal.Copy(b, 0, ValuePtr, 4);
          {$ENDIF}
          end;
        else
          Check(OCINumberFromInt(FOCIError, Value, SizeOf(Int64), OCI_NUMBER_SIGNED, ValuePtr));
        end;

        Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, ValuePtr, NotNullInd,
          Instance));
        Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
      finally
        Marshal.FreeHGlobal(ValuePtr);
      end;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Integer');
  end;
end;

function TOraArray.GetItemAsString(Index: integer): string;
begin
{$IFDEF VER12P}
  Result := ItemAsWideString[Index];
{$ELSE}
{$IFDEF CLR}
  Result := ItemAsWideString[Index];
{$ELSE}
  Result := ItemAsAnsiString[Index];
{$ENDIF}
{$ENDIF}
end;

procedure TOraArray.SetItemAsString(Index: integer; const Value: string);
begin
{$IFDEF VER12P}
  ItemAsWideString[Index] := Value;
{$ELSE}
{$IFDEF CLR}
  ItemAsWideString[Index] := Value;
{$ELSE}
  ItemAsAnsiString[Index] := Value;
{$ENDIF}
{$ENDIF}
end;

function TOraArray.GetItemAsAnsiString(Index: integer): AnsiString;
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    IntPtr(lOCIString), ItemIndicator));

  if Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL then
    case ItemType of
      dtString:
        Result := Marshal.PtrToStringAnsi(OCIStringPtr(FOCIEnv, Marshal.ReadIntPtr(lOCIString)));
      dtWideString:
        Result := AnsiString(Marshal.PtrToStringUni(OCIStringPtr(FOCIEnv, Marshal.ReadIntPtr(lOCIString))));
    else
      raise EConvertError.Create(SCannotConvert + 'String');
      Result := '';
    end
  else
    Result := '';
end;

procedure TOraArray.SetItemAsAnsiString(Index: integer; const Value: AnsiString);
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
  ValuePtr: IntPtr;
  Res: integer;
  Val: AnsiString;
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtString: begin
      Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
        IntPtr(lOCIString), ItemIndicator));
      Assert(Exists = 1);
      Val := Value;
      if Val = '' then
        Val := #0;
      ValuePtr := Marshal.StringToHGlobalAnsi(Value);
      Res := OCIStringAssignText(FOCIEnv, FOCIError, ValuePtr, Length(Val), lOCIString);
      FreeString(ValuePtr);
      Check(Res);
    end;
    dtWideString: begin
      ItemAsWideString[Index] := WideString(Value);
      Exit;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'String');
  end;

  Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, Marshal.ReadIntPtr(lOCIString), NotNullInd,
    Instance));

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsWideString(Index: integer): WideString;
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
begin
  CheckAlloc;

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    IntPtr(lOCIString), ItemIndicator));

  if Marshal.ReadInt16(ItemIndicator) = OCI_IND_NOTNULL then
    case ItemType of
      dtString:
        Result := WideString(Marshal.PtrToStringAnsi(OCIStringPtr(FOCIEnv, Marshal.ReadIntPtr(lOCIString))));
      dtWideString:
        Result := Marshal.PtrToStringUni(OCIStringPtr(FOCIEnv, Marshal.ReadIntPtr(lOCIString)));
    else
      raise EConvertError.Create(SCannotConvert + 'String');
      Result := '';
    end
  else
    Result := '';
end;

procedure TOraArray.SetItemAsWideString(Index: integer; const Value: WideString);
var
  lOCIString: ppOCIString;
  ItemIndicator: IntPtr;
  Exists: tbool;
  ValuePtr: IntPtr;
  Res: integer;
begin
  CheckAlloc;
  CheckIndex(Index);

  case ItemType of
    dtWideString: begin
      Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
        IntPtr(lOCIString), ItemIndicator));
      Assert(Exists = 1);
      ValuePtr := Marshal.StringToHGlobalUni(Value);
      Res := OCIStringAssignText(FOCIEnv, FOCIError, ValuePtr, Length(Value) * 2, lOCIString);
      FreeString(ValuePtr);
      Check(Res);
    end;
    dtString: begin
      ItemAsAnsiString[Index] := AnsiString(Value);
      Exit;
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'String');
  end;

  Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, Marshal.ReadIntPtr(lOCIString), NotNullInd,
    Instance));

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsObject(Index: integer): TOraObject;
var
  ItemIndicator: IntPtr;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ArrSize, i: integer;
  Attr: TAttribute;
  ItemInstance: IntPtr;
begin
  CheckAlloc;

  if ItemType <> dtObject then
    raise EConvertError.Create(SCannotConvert + 'Object');

  Check(OCICollSize(FOCIEnv, FOCIError, Instance, ArrSize));

  Attr := ObjectType.Attributes[0];
  if Attr = nil then
    RaiseError(SInvalidAttrName);
  if Index >= ArrSize then
    for i := ArrSize to Index do begin
      case Attr.DataType of
        dtObject: begin
          ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
          // Check(OCIObjectNew());
        end;
      else
        raise EConvertError.Create(SCannotConvert + 'Object');
      end;

    // WAR need real Indicator for object
      Check(OCICollAppend(FOCIEnv, FOCIError, ItemInstance, NullIndStruct, Instance));
      if Attr.DataType = dtObject then
        Marshal.FreeHGlobal(ItemInstance);
    end;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    ObjectPtr, ItemIndicator));
  if (FObjects <> nil) and (Index < FObjects.Count) then
    Result := TOraObject(FObjects[Index])
  else
    Result := nil;

  if Result = nil then begin
    if FObjects = nil then
      FObjects := TList.Create;

    case ItemType of
      dtObject:
        Result := TOraObject.Create;
    else
      raise EConvertError.Create(SCannotConvert + 'Object');
    end;
    Result.FOwner := Self;
    Result.ObjectType := TOraType(ObjectType.Attributes[0].ObjectType);
    Result.OCISvcCtx := FSvcCtx;

    while Index >= FObjects.Count do
      FObjects.Add(nil);
    FObjects[Index] := Result;
  end;

  if {(sb2(ItemIndicator^) = OCI_IND_NOTNULL) and} (Exists = 1) then begin
    Result.Instance := ObjectPtr;
    Result.Indicator := ItemIndicator;
          {case Attr.DataType of
            dtObject:
              Result := TOraObject.Create(TOraType(Attr.ObjectType));
            dtReference: begin
              Result := TOraRef.Create(TOraType(Attr.ObjectType));
              GetAttribute(Name, Value, Ind);
              TOraRef(Result).OCIRef := IntPtr(Value^);
              if not TOraRef(Result).IsNull then
                TOraRef(Result).Pin;  // ??? optim
            end;
            dtArray:
              Result := TOraArray.Create(TOraType(Attr.ObjectType));
            dtTable:
              Result := TOraNestTable.Create(TOraType(Attr.ObjectType));
          end;}
  end;
end;

procedure TOraArray.SetItemAsObject(Index: integer; Value: TOraObject);
begin
  CheckAlloc;
  CheckIndex(Index);

  // WAR need modify TObjects

  case ItemType of
    dtObject: begin
      Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, Value.Instance, Value.Indicator,
        Instance));
      //Value.FNativeInstance := False;  // ???
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Integer');
  end;

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsRef(Index: integer): TOraRef;
var
  ItemIndicator: IntPtr;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ArrSize, i: integer;
  Attr: TAttribute;
  ItemInstance: IntPtr;
begin
  CheckAlloc;

  if ItemType <> dtReference then
    raise EConvertError.Create(SCannotConvert + 'Reference');

  Check(OCICollSize(FOCIEnv, FOCIError, Instance, ArrSize));

  Attr := ObjectType.Attributes[0];
  if Attr = nil then
    RaiseError(SInvalidAttrName);
  if Index >= ArrSize then
    for i := ArrSize to Index do begin
      case Attr.DataType of
        dtReference: begin
          ItemInstance := Marshal.AllocHGlobal(TOraType(Attr.ObjectType).DataSize);
          // Check(OCIObjectNew());
        end;
      else
        raise EConvertError.Create(SCannotConvert + 'Reference');
      end;

    // WAR need real Indicator for object
      Check(OCICollAppend(FOCIEnv, FOCIError, ItemInstance, NullIndStruct, Instance));
      if Attr.DataType = dtReference then
        Marshal.FreeHGlobal(ItemInstance);
    end;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    ObjectPtr, ItemIndicator));
  if (FObjects <> nil) and (Index < FObjects.Count) then
    Result := TOraRef(FObjects[Index])
  else
    Result := nil;

  if Result = nil then begin
    if FObjects = nil then
      FObjects := TList.Create;

    case ItemType of
      dtReference:
        Result := TOraRef.Create(nil);
    else
      raise EConvertError.Create(SCannotConvert + 'Reference');
    end;
    Result.FOwner := Self;
    Result.ObjectType := TOraType(ObjectType.Attributes[0].ObjectType);
    Result.OCISvcCtx := FSvcCtx;

    while Index >= FObjects.Count do
      FObjects.Add(nil);
    FObjects[Index] := Result;
  end;

  if {(sb2(ItemIndicator^) = OCI_IND_NOTNULL) and} (Exists = 1) then begin
    Result.OCIRef := Marshal.ReadIntPtr(ObjectPtr);
  end;
end;

procedure TOraArray.SetItemAsRef(Index: integer; Value: TOraRef);
begin
  CheckAlloc;
  CheckIndex(Index);

  // WAR need modify TObjects

  case ItemType of
    dtReference: begin
      Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, Value.OCIRef, NotNullInd,
        Instance));
      //Value.FNativeInstance := False;  // ???
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Reference');
  end;

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

function TOraArray.GetItemAsLob(Index: integer): TOraLob;
var
  ItemIndicator: IntPtr;
  Exists: tbool;
  ObjectPtr: IntPtr;
  ArrSize, i: integer;
  Attr: TAttribute;
  Ind: Integer;
begin
  CheckAlloc;

  if not ItemType in [dtBlob, dtOraBlob, dtOraClob, dtNClob, dtWideOraClob] then
    raise EConvertError.Create(SCannotConvert + 'Lob');

  Check(OCICollSize(FOCIEnv, FOCIError, Instance, ArrSize));

  Attr := ObjectType.Attributes[0];
  if Attr = nil then
    RaiseError(SInvalidAttrName);
  if Index >= ArrSize then
    for i := ArrSize to Index do
      AppendItem;
  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for object

  Check(OCICollGetElem(FOCIEnv, FOCIError, Instance, Index, Exists,
    ObjectPtr, ItemIndicator));

  if (FObjects <> nil) and (Index < FObjects.Count) then
    Result := TOraLob(FObjects[Index])
  else
    Result := nil;

  if Result = nil then begin
    if FObjects = nil then
      FObjects := TList.Create;

    case ItemType of
      dtBlob, dtOraBlob: begin
        Result := TOraLob.Create(FSvcCtx);
        Result.LobType := ltBlob;
      end;
      dtOraClob, dtWideOraClob: begin
        Result := TOraLob.Create(FSvcCtx);
        Result.LobType := ltClob;
      end;
      dtNClob: begin
        Result := TOraLob.Create(FSvcCtx);
        Result.LobType := ltNClob;
      end;
    else
      raise EConvertError.Create(SCannotConvert + 'Lob');
    end;

    Ind := Marshal.ReadInt16(ItemIndicator);
    if Ind = OCI_IND_NOTNULL then begin
      Result.OCILobLocator := Marshal.ReadIntPtr(ObjectPtr);
      Result.ReadLob;
    end;

    while Index >= FObjects.Count do
      FObjects.Add(nil);
    FObjects[Index] := Result;
  end;
end;

procedure TOraArray.SetItemAsLob(Index: integer; Value: TOraLob);
begin
  CheckAlloc;
  CheckIndex(Index);

  // WAR need modify TObjects

  case ItemType of
    dtBlob, dtOraBlob, dtOraClob, dtNClob, dtWideOraClob: begin
      Check(OCICollAssignElem(FOCIEnv, FOCIError, Index, Value.OCILobLocator, NotNullInd,
        Instance));
      //Value.FNativeInstance := False;  // ???
    end;
  else
    raise EConvertError.Create(SCannotConvert + 'Lob');
  end;

  Marshal.WriteInt16(Indicator, OCI_IND_NOTNULL);  // for array
end;

procedure TOraArray.Assign(Source: TOraObject);
begin
  if Source is TOraArray then begin
    CheckAlloc;

    Check(OCICollAssign(FOCIEnv, FOCIError, Source.Instance, Instance));
  end
  else
    inherited;
end;

function TOraArray.GetSize: integer;
begin
  CheckAlloc;

  Check(OCICollSize(FOCIEnv, FOCIError, Instance, Result))
end;

procedure TOraArray.SetSize(Value: integer);
var
  ASize: integer;
begin
  CheckAlloc;

  ASize := Size;
  if Value > ASize then
    InsertItem(Value - 1)
  else
    Check(OCICollTrim(FOCIEnv, FOCIError, ASize - Value, Instance));
end;

function TOraArray.GetMaxSize: integer;
begin
  CheckAlloc;

  Result := OCICollMax(FOCIEnv, Instance);
end;

function TOraArray.GetItemType;
begin
  CheckType;

  Result := ObjectType.Attributes[0].DataType;
end;

function TOraArray.GetItemSubType;
begin
  CheckType;

  Result := ObjectType.Attributes[0].SubDataType;
end;

procedure TOraArray.Clear;
begin
  CheckAlloc;

  FreeObjects;
  Check(OCICollTrim(FOCIEnv, FOCIError, GetSize, Instance));
end;


{ TOraNestTable }

procedure TOraNestTable.CheckType;
begin
  inherited;

  if not (FObjectType.DataType in [dtTable]) then
    RaiseError('Type of object must be Table');
end;

procedure TOraNestTable.Assign(Source: TOraObject);
begin
  inherited;

  if (Source is TOraNestTable) and (OCIVersion >= 9000) then
    NestTableAssign(Source.Instance, Instance, ObjectType);
end;

procedure TOraNestTable.DeleteItem(Index: integer);
begin
  Check(OCITableDelete(FOCIEnv, FOCIError, Index, Instance));
end;

function TOraNestTable.GetSize: integer;
begin
  CheckAlloc;

  Check(OCITableSize(FOCIEnv, FOCIError, Instance, Result))
end;

{ TRefData }

constructor TRefData.Create;
begin
  inherited;

  FRequireEmptyStrToNull := True;
end;

destructor TRefData.Destroy;
begin
  inherited;

  if FRef <> nil then
    FRef.Release;
end;

procedure TRefData.InternalPrepare;
begin
  if FRef = nil then
    RaiseError('Reference is not defined');
end;

procedure TRefData.InternalOpen(DisableInitFields: boolean = False); // PrepareData;
begin
  inherited;

  FEOF := False;
  if not DisableInitFields then
    InitFields;
end;

procedure TRefData.Reopen;
begin
  FreeData;
  InitData;
  InternalOpen; // PrepareData;
end;

{ Fields }

procedure TRefData.InternalInitFields;
var
  ObjectType: TObjectType;
  Field: TFieldDesc;
begin
  ObjectType := FRef.ObjectType;

  Field := TFieldDesc.Create;
  Field.Name := 'REF';
  Field.DataType := ObjectType.DataType;
  Field.Size := SizeOf(IntPtr);
  Field.ObjectType := ObjectType;
  Field.HiddenObject := not FIncludeObjectField;
  Field.FieldNo := 1;

  FFields.Add(Field);

  InitObjectFields(ObjectType, Field);
end;

{ Records }

procedure TRefData.GetRecord(RecBuf: IntPtr);
begin
  if not FEOF and not FBOF and not FRef.IsNull then
    Marshal.WriteIntPtr(RecBuf, FRef.GCHandle);
end;

procedure TRefData.PutRecord(RecBuf: IntPtr);
begin
  if not FEOF and not FBOF and not FRef.IsNull then begin
    Assert(FRef = GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf)));
    FRef := TOraRef(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf)));
  end;
end;

procedure TRefData.GetNextRecord(RecBuf: IntPtr);
begin
  if not EOF then
    if BOF then
      if not FRef.IsNull then begin
        FBOF := False;
        GetRecord(RecBuf);
      end
      else
        FEOF := True
    else
      FEOF := True;
end;

procedure TRefData.GetPriorRecord(RecBuf: IntPtr);
begin
  if not BOF then
    if EOF then
      if not FRef.IsNull then begin
        FEOF := False;
        GetRecord(RecBuf);
      end
      else
        FBOF := True
    else
      FBOF := True;
end;

procedure TRefData.AppendRecord(RecBuf: IntPtr);
begin
end;

procedure TRefData.InsertRecord(RecBuf: IntPtr);
begin
  AppendRecord(RecBuf);
end;

procedure TRefData.UpdateRecord(RecBuf: IntPtr);
begin
  FRef.MarkDelete;//Update;
  FRef.Flush;
end;

procedure TRefData.DeleteRecord;
begin
end;

class function TRefData.IsComplexFieldType(DataType: word): boolean;
begin
  Result := TOCIRecordSet.IsComplexFieldType(DataType);
end;

procedure TRefData.CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Ptr: IntPtr;
  OraObject: TOraObject;
  OraRef: TOraRef;
begin
  inherited;

  for i := 0 to FieldCount - 1 do
    if not Fields[i].HasParent and
      (not(IsBlobFieldType(Fields[i].DataType)) or WithBlob)
    then begin
      Ptr := PtrOffset(RecBuf, Fields[i].Offset);
      case Fields[i].DataType of
        dtXML: begin
          OraObject := TOraXML.Create(TOraType(Fields[i].ObjectType));
          OraObject.OCIError := FRef.OCIError;
          OraObject.AllocObject(FRef.OCISvcCtx);
          Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
        end;
        dtObject: begin
          OraObject := TOraObject.Create(TOraType(Fields[i].ObjectType));
          OraObject.OCIError := FRef.OCIError;
          OraObject.AllocObject(FRef.OCISvcCtx);
          Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
        end;
        dtReference: begin
          OraRef := TOraRef.Create(TOraType(Fields[i].ObjectType));
          OraRef.OCIError := FRef.OCIError;
          Marshal.WriteIntPtr(Ptr, OraRef.GCHandle);
        end;
      end;
    end;
end;

procedure TRefData.FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
begin
  for i := 0 to FieldCount - 1 do
    if not Fields[i].HasParent and
      (not(IsBlobFieldType(Fields[i].DataType)) or WithBlob)
    then
      case Fields[i].DataType of
        dtObject,dtReference:
          TSharedObject(
            GetGCHandleTarget(
              Marshal.ReadIntPtr(RecBuf, Fields[i].Offset)
            )
          ).Free;
      end;

  inherited;
end;

procedure TRefData.CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean);
var
  i: integer;
  SrcPtr: IntPtr;
  DestPtr: IntPtr;
  OraObjectSrc,OraObjectDest: TOraObject;
begin
  inherited;

  for i := 0 to FieldCount - 1 do
    if not Fields[i].HasParent and
      (not(IsBlobFieldType(Fields[i].DataType)) or WithBlob)
    then begin
      SrcPtr := Marshal.ReadIntPtr(Source, Fields[i].Offset);
      DestPtr := Marshal.ReadIntPtr(Dest, Fields[i].Offset);
      case Fields[i].DataType of
        dtObject,dtReference: begin
          OraObjectSrc := TOraObject(GetGCHandleTarget(SrcPtr));
          OraObjectDest := TOraObject(GetGCHandleTarget(DestPtr));
          OraObjectDest.Assign(OraObjectSrc);
        end;
      end;
    end;
end;

{ Bookmarks }

procedure TRefData.GetBookmark(Bookmark: PRecBookmark);
begin
end;

procedure TRefData.SetToBookmark(Bookmark: PRecBookmark);
begin
  FEOF := False;
  FBOF := False;
end;

procedure TRefData.SetRef(Value: TOraRef);
begin
  if Value <> FRef then begin
    if FRef <> nil then
      FRef.Release;

    FRef := Value;

    if FRef <> nil then
      FRef.AddRef;
  end;
end;

{ TTableData }

constructor TTableData.Create;
begin
  inherited;

  FRequireEmptyStrToNull := True;
end;

destructor TTableData.Destroy;
begin
  inherited;

  if FTable <> nil then
    FTable.Release;
end;

procedure TTableData.Check(Status: sword);
begin
  FTable.Check(Status);
end;

procedure TTableData.InternalPrepare;
begin
  {if FTable = nil then
    RaiseError('Table is not defined');} //WAR
end;

procedure TTableData.InternalOpen(DisableInitFields: boolean = False); //PrepareData;
begin
  inherited;

  FEOF := False;
  LastIndex := -1;
  if not DisableInitFields then
    InitFields;
end;

procedure TTableData.Reopen;
begin
  FreeData;
  InitData;
  InternalOpen; // PrepareData;
end;

{ Fields }

procedure TTableData.InternalInitFields;
var
  ObjectType: TObjectType;
  Field: TFieldDesc;
begin
  if FTable <> nil then begin
    if FTable.ItemType = dtObject then begin
      ObjectType := FTable.ObjectType.Attributes[0].ObjectType;

      Field := TFieldDesc.Create;
      Field.Name := FTable.ObjectType.Attributes[0].Name;
      Field.DataType := ObjectType.DataType;
      Field.Size := SizeOf(IntPtr);
      Field.ObjectType := ObjectType;
      Field.HiddenObject := not FIncludeObjectField;
      Field.FieldNo := 1;

      FFields.Add(Field);
    end
    else begin
      ObjectType := FTable.ObjectType;
      Field := nil;
    end;

    InitObjectFields(ObjectType, Field);

    case FTable.ItemType of
      dtDateTime:
        Fields[0].Size := SizeOf(TDateTime);
      dtFloat:
        Fields[0].Size := SizeOf(Double);
      dtInteger:
        Fields[0].Size := SizeOf(Integer);
      dtLargeInt:
        Fields[0].Size := SizeOf(Int64);
      dtString:
        Fields[0].Size := Fields[0].Length + 1;
      dtWideString:
        Fields[0].Size := (Fields[0].Length + 1) * 2;
    end;
  end;
end;

function TTableData.GetIndicatorSize: word;
begin
  Result := inherited GetIndicatorSize + SizeOf(Integer); // for index
end;

procedure TTableData.InitFields;
begin
  inherited;

  FIndexOfs := RecordSize - SizeOf(Integer);
end;

class function TTableData.IsBlobFieldType(DataType: word): boolean;
begin
  Result := TOCIRecordSet.IsBlobFieldType(DataType);
end;

{ Records }

{procedure TTableData.GetObjectFields(RecBuf: IntPtr; Obj: TDBObject; var FieldNo: word);
var
  i: integer;
  ObjectType: TObjectType;
  IsBlank: boolean;
begin
  ObjectType := Obj.ObjectType;
  for i := 0 to ObjectType.AttributeCount - 1 do
    case ObjectType.Attributes[i].DataType of
      dtDateTime, dtFloat, dtInteger, dtLargeInt, dtString, dtWideString: begin
        TOraObject(Obj).GetAttributeValue(ObjectType.Attributes[i].Name,
          PtrOffset(RecBuf, Fields[FieldNo - 1].Offset), IsBlank);
        SetNull(FieldNo, RecBuf, IsBlank);
        Inc(FieldNo);
      end;
    else
      Assert(False, SUnknownDataType);
    end;
end;

procedure TTableData.PutObjectFields(RecBuf: IntPtr; Obj: TDBObject; var FieldNo: word);
var
  i: integer;
  ObjectType: TObjectType;
begin
  ObjectType := Obj.ObjectType;
  for i := 0 to ObjectType.AttributeCount - 1 do
    case ObjectType.Attributes[i].DataType of
      dtDateTime, dtFloat, dtInteger, dtLargeInt, dtString, dtWideString: begin
        if GetNull(FieldNo, RecBuf) then
          TOraObject(Obj).SetAttributeValue(ObjectType.Attributes[i].Name, nil)
        else
          TOraObject(Obj).SetAttributeValue(ObjectType.Attributes[i].Name,
            PtrOffset(RecBuf, Fields[FieldNo - 1].Offset));
        Inc(FieldNo);
      end;
    else
      Assert(False, SUnknownDataType);
    end;
end;}

procedure TTableData.GetRecord(RecBuf: IntPtr);
var
  Index, Len: integer;
  IsNull: boolean;
  //FieldNo: word;
  ansis: AnsiString;
  ws: WideString;
begin
  inherited;
  
  if not(EOF or BOF or (IntPtr(CurrentItem) = nil)) then begin
    Index := Marshal.ReadInt32(CurrentItem, SizeOf(TItemHeader) + FIndexOfs);
    IsNull := FTable.ItemIsNull[Index];
    SetNull(1, RecBuf, IsNull);

    if not IsNull then
      case FTable.ItemType of
        dtDateTime:
          Marshal.WriteInt64(RecBuf, BitConverter.DoubleToInt64Bits(double(FTable.ItemAsDateTime[Index])));
        dtFloat:
          Marshal.WriteInt64(RecBuf, BitConverter.DoubleToInt64Bits(FTable.ItemAsFloat[Index]));
        dtInteger:
          Marshal.WriteInt32(RecBuf, FTable.ItemAsInteger[Index]);
        dtLargeInt:
          Marshal.WriteInt64(RecBuf, FTable.ItemAsLargeInt[Index]);
        dtString: begin
          ansis := FTable.ItemAsAnsiString[Index];
          Len := Length(ansis);
          if Len > FTable.ObjectType.Attributes[0].Length then
            Len := FTable.ObjectType.Attributes[0].Length;
          CopyBufferAnsi(ansis, RecBuf, Len + 1);
        end;
        dtWideString: begin
          ws := FTable.ItemAsWideString[Index];
          Len := Length(ws);
          if Len > FTable.ObjectType.Attributes[0].Length then
            Len := FTable.ObjectType.Attributes[0].Length;
          CopyBufferUni(ws, RecBuf, (Len + 1) * 2);
        end;
        dtObject, dtReference: begin
          Marshal.WriteIntPtr(RecBuf, Marshal.ReadIntPtr(CurrentItem, SizeOf(TItemHeader)));
          {FieldNo := 1;
          GetObjectFields(RecBuf, TOraObject(IntPtr(PChar(CurrentItem) + SizeOf(TItemHeader))^), FieldNo);}
        end;
      else
        Assert(False, SUnknownDataType);
      end;
  end;
end;

procedure TTableData.PutRecord(RecBuf: IntPtr);
var
  Index: integer;
begin
  inherited;
  
  if not(EOF or BOF or (IntPtr(CurrentItem) = nil)) then begin
    Index := Marshal.ReadInt32(RecBuf, FIndexOfs);
    if not(FTable.ItemType in [dtObject]) and GetNull(1, RecBuf) then
      FTable.ItemIsNull[Index] := True
    else
      case FTable.ItemType of
        dtDateTime:
          FTable.ItemAsDateTime[Index] := TDateTime(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(RecBuf)));
        dtFloat:
          FTable.ItemAsFloat[Index] := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(RecBuf));
        dtInteger:
          FTable.ItemAsInteger[Index] := Marshal.ReadInt32(RecBuf);
        dtLargeInt:
          FTable.ItemAsLargeInt[Index] := Marshal.ReadInt64(RecBuf);
        dtString:
          FTable.ItemAsAnsiString[Index] := Marshal.PtrToStringAnsi(RecBuf);
        dtWideString:
          FTable.ItemAsWideString[Index] := Marshal.PtrToStringUni(RecBuf);
        dtObject: begin
          Marshal.WriteIntPtr(CurrentItem, SizeOf(TItemHeader), Marshal.ReadIntPtr(RecBuf));
          {FieldNo := 1;
          PutObjectFields(RecBuf, TOraObject(IntPtr(PChar(CurrentItem) + SizeOf(TItemHeader))^), FieldNo);}
        end;
      else
        Assert(False, SUnknownDataType);
      end;
  end;
end;

procedure TTableData.GetNextRecord(RecBuf: IntPtr);
var
  Item: PItemHeader;
  Res: boolean;

  procedure OmitRecords;
  begin
  end;
begin
  if not EOF then begin
    if BOF then begin
      if IntPtr(FirstItem) = nil then begin
        Res := Fetch;
        if Res then
          FBOF := False
        else
          FEOF := True;
      end
      else
        FBOF := False;

      CurrentItem := FirstItem;
      OmitRecords;
      if IntPtr(CurrentItem) = nil then
        FEOF := True
      else
        GetRecord(RecBuf);
    end
    else begin
      Item := CurrentItem;
      CurrentItem := CurrentItem.Next;
      OmitRecords;
      if IntPtr(CurrentItem) = nil then begin
        Res := Fetch;

        if Res then begin
          CurrentItem := Item.Next;
          GetRecord(RecBuf)
        end
        else
          FEOF := True
      end
      else
        GetRecord(RecBuf);
    end;
  end;
end;

class function TTableData.IsComplexFieldType(DataType: word): boolean;
begin
  Result := TOCIRecordSet.IsComplexFieldType(DataType);
end;

procedure TTableData.FetchComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Ptr: IntPtr;
  OraObject: TOraObject;
begin
  inherited CreateComplexFields(RecBuf, WithBlob);

  for i := 0 to FieldCount - 1 do
    if not Fields[i].HasParent then begin
      case Fields[i].DataType of
        dtObject: begin
          Ptr := PtrOffset(RecBuf, Fields[i].Offset);
          OraObject := FTable.ItemAsObject[LastIndex];
          OraObject.AddRef;
          Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
        end;
        dtReference: begin
          Ptr := PtrOffset(RecBuf, Fields[i].Offset);
          OraObject := FTable.ItemAsRef[LastIndex];
          OraObject.AddRef;
          Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
        end;
      end;
    end;
end;

procedure TTableData.CreateComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
  Ptr: IntPtr;
  OraObject: TOraObject;
  OraRef: TOraRef;
begin
  inherited;

  for i := 0 to FieldCount - 1 do
    if not Fields[i].HasParent and
      (not(IsBlobFieldType(Fields[i].DataType)) or WithBlob)
    then begin
      Ptr := PtrOffset(RecBuf, Fields[i].Offset);
      case Fields[i].DataType of
        dtXML: begin
          OraObject := TOraXML.Create(TOraType(Fields[i].ObjectType));
          OraObject.OCIError := FTable.OCIError;
          OraObject.AllocObject(FTable.OCISvcCtx);
          Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
        end;
        dtObject: begin
          OraObject := TOraObject.Create(TOraType(Fields[i].ObjectType));
          OraObject.OCIError := FTable.OCIError;
          OraObject.AllocObject(FTable.OCISvcCtx);
          Marshal.WriteIntPtr(Ptr, OraObject.GCHandle);
        end;
        dtReference: begin
          OraRef := TOraRef.Create(TOraType(Fields[i].ObjectType));
          OraRef.OCIError := FTable.OCIError;
          Marshal.WriteIntPtr(Ptr, OraRef.GCHandle);
        end;
      end;
    end;
end;

procedure TTableData.FreeComplexFields(RecBuf: IntPtr; WithBlob: boolean);
var
  i: integer;
begin
  for i := 0 to FieldCount - 1 do
    if not Fields[i].HasParent and
      (not(IsBlobFieldType(Fields[i].DataType)) or WithBlob)
    then
      case Fields[i].DataType of
        dtObject,dtReference:
          TSharedObject(
            GetGCHandleTarget(Marshal.ReadIntPtr(
              PtrOffset(RecBuf, Fields[i].Offset)
            ))
          ).Release;
      end;

  inherited;
end;

procedure TTableData.CopyComplexFields(Source: IntPtr; Dest: IntPtr; WithBlob: boolean);
var
  i: integer;
  SrcPtr: IntPtr;
  DestPtr: IntPtr;
  OraObjectSrc,OraObjectDest: TOraObject;
begin
  inherited;

  for i := 0 to FieldCount - 1 do
    if not Fields[i].HasParent and
      (not(IsBlobFieldType(Fields[i].DataType)) or WithBlob)
    then begin
      SrcPtr := Marshal.ReadIntPtr(
        PtrOffset(Source, Fields[i].Offset)
      );
      DestPtr := Marshal.ReadIntPtr(
        PtrOffset(Dest, Fields[i].Offset)
      );
      case Fields[i].DataType of
        dtObject,dtReference: begin
          OraObjectSrc := TOraObject(GetGCHandleTarget(SrcPtr));
          OraObjectDest := TOraObject(GetGCHandleTarget(DestPtr));
          OraObjectDest.Assign(OraObjectSrc);
        end;
      end;
    end;
end;

{ Fetching }

function TTableData.Fetch(FetchBack: boolean = False): boolean;
var
  Exists: tbool;
  Item: PItemHeader;
begin
  if FTable <> nil then begin
    FTable.CheckAlloc(False);
    if not FTable.IsNull then begin
      Check(OCITableNext(FTable.FOCIEnv, hOCIError, LastIndex, FTable.Instance, LastIndex, Exists));
      Result := Exists = 1;
    end else
      Result := False;
  end
  else
    Result := False;

  if Result then begin
    BlockMan.AllocItem(Item);
    Marshal.WriteInt32(Item, SizeOf(TItemHeader) + FIndexOfs, LastIndex );

    if FTable.ItemType in [dtObject, dtReference] then
      FetchComplexFields(PtrOffset(Item, SizeOf(TItemHeader)), True);

  // Create Items
    if IntPtr(FirstItem) = nil then
      FirstItem := Item;

    Item.Prev := LastItem;
    Item.Next := nil;
    Item.Flag := flUsed;

    if IntPtr(LastItem) <> nil then begin
      LastItem.Next := Item;
      Item.Order := LastItem.Order + 1;
    end
    else
      Item.Order := 1;

    LastItem := Item;

    Item.Rollback := nil;
    Item.Status := isUnmodified;
    Item.UpdateResult := urNone;
  end;
end;

{ Edit }

procedure TTableData.InternalAppend(RecBuf: IntPtr);
var
  Exists: tbool;
  OraObject: TOraObject;
  Instance, Indicator: IntPtr;
begin
  Indicator := NullIndStruct;
  OraObject := nil;
  case FTable.ItemType of
    dtDateTime:
      Instance := NullOCIDate;
    dtInteger, dtLargeInt, dtFloat:
      Instance := NullOCINumber;
    dtString, dtWideString:
      Instance := NullOCIString;
    dtObject: begin
      OraObject := TOraObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf)));
      Instance := OraObject.Instance;
      Indicator := OraObject.Indicator;
    end;
  else
    Instance := nil;
    Assert(False, SUnknownDataType);
  end;

  Check(OCICollAppend(FTable.FOCIEnv, hOCIError, Instance, Indicator, FTable.Instance));
  Check(OCICollSize(FTable.FOCIEnv, hOCIError, FTable.Instance, LastIndex));
  Dec(LastIndex);
  if FTable.ItemType = dtObject then begin
    Exists := 1;
    OCICollGetElem(FTable.FOCIEnv, hOCIError, FTable.Instance, LastIndex, Exists, Instance, Indicator);
    OraObject.ResetInstance(Instance, Indicator);
    with FTable do begin
      if FObjects = nil then
        FObjects := TList.Create;
      FObjects.Count := LastIndex + 1;
      FObjects[LastIndex] := OraObject;
      OraObject.AddRef;
    end;
  end;

  Marshal.WriteInt32(RecBuf, FIndexOfs, LastIndex);
end;

procedure TTableData.InternalDelete;
var
  Index: integer;
begin
  Index := Marshal.ReadInt32(CurrentItem, SizeOf(TItemHeader) + FIndexOfs);

  FTable.DeleteItem(Index);
end;

procedure TTableData.InternalUpdate;
begin
end;

procedure TTableData.CancelRecord(RecBuf: IntPtr);
var
  Index: integer;
  OraObject: TOraObject;
begin
  inherited;

  if FTable.ItemType = dtObject then begin
  // Updates table item to old value
    Index := Marshal.ReadInt32(CurrentItem, SizeOf(TItemHeader) + FIndexOfs);
    OraObject := TOraObject(GetGCHandleTarget(
      Marshal.ReadIntPtr(CurrentItem, SizeOf(TItemHeader))
    ));
    FTable.ItemAsObject[Index] := OraObject;
  end;
end;

procedure TTableData.SetTable(Value: TOraNestTable);
begin
  if Value <> FTable then begin
    if FTable <> nil then
      FTable.Release;

    FTable := Value;

    if FTable <> nil then
      FTable.AddRef;
  end;
end;

function GetObjectCacheMaxSize: integer;
begin
//  CheckOCIInited;
  Check(OCIAttrGet2(hOCIEnv, OCI_HTYPE_ENV, Result, nil, OCI_ATTR_CACHE_MAX_SIZE, hOCIError), OCIUnicode);
end;

procedure SetObjectCacheMaxSize(Value: integer);
begin
  Check(OCIAttrSet2(hOCIEnv, OCI_HTYPE_ENV, Value, 0, OCI_ATTR_CACHE_MAX_SIZE, hOCIError), OCIUnicode);
end;

function GetObjectCacheOptSize: integer;
begin
  Check(OCIAttrGet2(hOCIEnv, OCI_HTYPE_ENV, Result, nil, OCI_ATTR_CACHE_OPT_SIZE, hOCIError), OCIUnicode);
end;

procedure SetObjectCacheOptSize(Value: integer);
begin
  Check(OCIAttrSet2(hOCIEnv, OCI_HTYPE_ENV, Value, 0, OCI_ATTR_CACHE_OPT_SIZE, hOCIError), OCIUnicode);
end;

function GetOraType(SvcCtx: pOCISvcCtx; Name: _string): TObjectType;
begin
  if ObjectTypes <> nil then
    Result := ObjectTypes.FindType(SvcCtx, Name)
  else
    Result := nil;

  if Result = nil then
    Result := TOraType.Create(SvcCtx, Name);
end;

function TTableData.GetRecordCount: longint;
begin
  Result := LastIndex + 1;
end;

var
  i: integer;

initialization
  ObjectTypes := nil;
  NotNullInd := Marshal.AllocHGlobal(sizeof(OCIInd));
  Marshal.WriteInt16(NotNullInd, OCI_IND_NOTNULL);
  NullIndStruct := Marshal.AllocHGlobal(100);
  for i := 0 to 49 do
    Marshal.WriteInt16(NullIndStruct, i * 2, -1);
  NullOCINumber := Marshal.AllocHGlobal(OCI_NUMBER_SIZE);
  NullOCIDate := Marshal.AllocHGlobal(sizeof(OCIDate));
  NullOCIString := Marshal.AllocHGlobal(sizeof(IntPtr));
  Marshal.WriteIntPtr(NullOCIString, nil);

finalization
  if ObjectTypes <> nil then begin
    {$IFDEF DEBUG}
    with ObjectTypes.LockList do
    try
      Assert(Count = 0, IntToStr(Count) + ' ObjecType(s) hasn''t been released');
    finally
      ObjectTypes.UnlockList;
    end;
    {$ENDIF}
    ObjectTypes.Free;
    ObjectTypes := nil;
  end;
  Marshal.FreeHGlobal(NotNullInd);
  Marshal.FreeHGlobal(NullIndStruct);
  Marshal.FreeHGlobal(NullOCINumber);
  Marshal.FreeHGlobal(NullOCIDate);
  Marshal.FreeHGlobal(NullOCIString);
end.

