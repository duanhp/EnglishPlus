
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgObjectsUni;

{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, Variants, Math, DateUtils, FMTBcd,
{$IFNDEF FPC}
  SqlTimSt,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  MemUtils, MemData, CRAccess,
{$IFNDEF UNIDACPRO}
  PgCall, PgSQLProtocol, PgClasses;
{$ELSE}
  PgCallUni, PgSQLProtocolUni, PgClassesUni;
{$ENDIF}

const
  POSTGRES_EPOCH_JDATE = 2451545;
  PostgresBaseDate = 36526.0;  // TDateTime(01.01.2000)
  McSecsPerSec = 1000000;
  McSecsPerDay = Int64(SecsPerDay) * McSecsPerSec;

  JulianMinYear = -4713;
  JulianMinMonth = 11;
  JulianMinDay = 24;
  JulianMaxYear = 5874898;

type

{ TPgRefCursor }

  TPgRefCursor = class(TPgCursor)
  private
    FConnection: TPgSQLConnection;
    FCursorName: _string;
  protected
  public
    destructor Destroy; override;

    procedure Assign(Source: TPgCursor); overload;
    procedure Assign(Connection: TPgSQLConnection; CursorName: _string); overload;
    function CanFetch: boolean; override;

    property CursorName: _string read FCursorName;
  end;

{ TPgSQLLargeObject }

  TPgSQLLargeObject = class(TCompressedBlob)
  private
    FConnection: TPgSQLConnection; // don't use FConnection directly. In PgDac it can be nil
    FOID: integer;
    FOldOID: integer;
    FObjHandle: integer;
    FIsOpened: boolean;
    FPosition: Cardinal;
    FCached: boolean;

    procedure ResetOldOID;

    procedure SetOID(Value: OID);
    procedure SetCached(Value: boolean);
  protected
    function GetConnection: TPgSQLConnection; virtual;
    function GetProtocol: TPgSQLProtocol;

    procedure CheckConnection;
    procedure CheckCreated;
    procedure CheckOpened;

    function Seek(Position: integer; Origin: TSeekOrigin): integer;

    function GetSize: Cardinal; override;
  public
    constructor Create(Connection: TPgSQLConnection);
    destructor Destroy; override;

    function IsCreated: boolean;

    procedure CreateObject;
    procedure OpenObject();
    procedure CloseObject();
    procedure UnlinkObject();

    procedure FreeBlob; override;
    procedure Disconnect; override;
    
    procedure ReadBlob; overload;
    procedure ReadBlob(var SharedPiece: PPieceHeader); overload;
    procedure WriteBlob;

    procedure Write(Position, Count: Cardinal; Source: IntPtr); override;
    function Read(Position: Cardinal; Count: Cardinal; Dest: IntPtr): Cardinal; override;

    procedure Commit; override;
    procedure Cancel; override;

    property Connection: TPgSQLConnection read GetConnection write FConnection;
    property OID: OID read FOID write SetOID;
    property Cached: boolean read FCached write SetCached;
  end;

{ TCustomPgTimeStamp }

  TCustomPgTimeStamp = class (TSharedObject)
  private
    FDays: integer;
    FTicks: int64;
    FHasTimeZone: boolean;

    class procedure InternalDecodeDate(Days: integer; var Year, Month, Day: integer);
    class procedure InternalDecodeTime(Ticks: int64;
      var Hour, Minute, Second, Microsecond: integer);
    class procedure InternalDecodeDateTime(Days: integer; Ticks: int64;
      var Year, Month, Day, Hour, Minute, Second, Microsecond: integer);
    class procedure InternalEncodeDate(var Days: integer; Year, Month, Day: integer);
    class procedure InternalEncodeTime(var Ticks: int64;
      Hour, Minute, Second, Microsecond: integer);
    class procedure InternalEncodeDateTime(var Days: integer; var Ticks: int64;
      Year, Month, Day, Hour, Minute, Second, Microsecond: integer);

    function GetAsString: string; virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
    function GetAsDateTime: TDateTime; virtual; abstract;
    procedure SetAsDateTime(Value: TDateTime); virtual; abstract;
  {$IFNDEF FPC}
    function GetAsSQLTimeStamp: TSQLTimeStamp; virtual; abstract;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual; abstract;
  {$ENDIF}
    function GetTimeZoneOffset: integer; virtual;
    function GetIsInfinity: boolean;
    function GetIsPosInfinity: boolean;
    function GetIsNegInfinity: boolean;
    procedure SetIsPosInfinity(Value: boolean);
    procedure SetIsNegInfinity(Value: boolean);

  public
    class function ConvertToString(Days: integer; Ticks: int64;
      HasTimeZone: boolean; TimeZoneOffset: integer; const Format: string): string;
    class procedure FromString(const Value: string; var Days: integer; var Ticks: int64;
      var TimeZoneOffset: integer; DateUsed, TimeUsed, ForcePostgreFormat: boolean);

    procedure Assign(Source: TCustomPgTimeStamp); virtual;
    function Compare(Value: TCustomPgTimeStamp): integer; virtual; abstract;
    procedure DecodeDate(var Year, Month, Day: integer);
    procedure DecodeTime(var Hour, Minute, Second, Microsecond: integer);
    procedure DecodeDateTime(var Year, Month, Day, Hour, Minute, Second, Microsecond: integer);
    procedure EncodeDate(Year, Month, Day: integer);
    procedure EncodeTime(Hour, Minute, Second, Microsecond: integer);
    procedure EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Microsecond: integer);

    property Days: integer read FDays write FDays;
    property Ticks: int64 read FTicks write FTicks;
    property HasTimeZone: boolean read FHasTimeZone write FHasTimeZone;
    property TimeZoneOffset: integer read GetTimeZoneOffset;
    property IsInfinity: boolean read GetIsInfinity write SetIsPosInfinity;
    property IsPosInfinity: boolean read GetIsPosInfinity write SetIsPosInfinity;
    property IsNegInfinity: boolean read GetIsNegInfinity write SetIsNegInfinity;

    property AsString: string read GetAsString write SetAsString;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  {$IFNDEF FPC}
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
  {$ENDIF}
  end;

{ TPgTimeStamp }

  TPgTimeStamp = class (TCustomPgTimeStamp)
  private
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetAsDateTime: TDateTime; override;
    procedure SetAsDateTime(Value: TDateTime); override;
  {$IFNDEF FPC}
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
  {$ENDIF}

  public
    class function ToDateTime(Days: integer; Ticks: int64): TDateTime;
    class procedure FromDateTime(Value: TDateTime; var Days: integer; var Ticks: int64);
  {$IFNDEF FPC}
    class procedure FromSQLTimeStamp(const Value: TSQLTimeStamp; var Days: integer; var Ticks: int64);
  {$ENDIF}
    class procedure FromString(const Value: string; var Days: integer; var Ticks: int64;
      ForcePostgreFormat: boolean);

    function Compare(Value: TCustomPgTimeStamp): integer; override;
  end;

{ TPgDate }

  TPgDate = class (TCustomPgTimeStamp)
  private
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetAsDateTime: TDateTime; override;
    procedure SetAsDateTime(Value: TDateTime); override;
  {$IFNDEF FPC}
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
  {$ENDIF}

  public
    class function ToDateTime(Days: integer): TDateTime;
    class procedure FromDateTime(Value: TDateTime; var Days: integer);
  {$IFNDEF FPC}
    class procedure FromSQLTimeStamp(const Value: TSQLTimeStamp; var Days: integer);
  {$ENDIF}
    class function ConvertToString(Days: integer; const Format: string): string;
    class procedure FromString(const Value: string; var Days: integer; ForcePostgreFormat: boolean);

    function Compare(Value: TCustomPgTimeStamp): integer; override;
  end;

{ TPgTime }

  TPgTime = class (TCustomPgTimeStamp)
  private
    FTimeZoneOffset: integer; // seconds

    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetAsDateTime: TDateTime; override;
    procedure SetAsDateTime(Value: TDateTime); override;
  {$IFNDEF FPC}
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
  {$ENDIF}
    function GetTimeZoneOffset: integer; override;

  public
    class function ToDateTime(Ticks: int64): TDateTime;
    class procedure FromDateTime(Value: TDateTime; var Ticks: int64);
  {$IFNDEF FPC}
    class procedure FromSQLTimeStamp(const Value: TSQLTimeStamp; var Ticks: int64);
  {$ENDIF}
    class function ConvertToString(Ticks: int64; HasTimeZone: boolean;
      TimeZoneOffset: integer; const Format: string): string;
    class procedure FromString(const Value: string; var Ticks: int64; var TimeZoneOffset: integer;
      ForcePostgreFormat: boolean);

    procedure Assign(Source: TCustomPgTimeStamp); override;
    function Compare(Value: TCustomPgTimeStamp): integer; override;

    property HasTimeZone;
    property TimeZoneOffset: integer read FTimeZoneOffset write FTimeZoneOffset;
  end;

{ TPgInterval }

  TPgInterval = class (TSharedObject)
  private
    FMonths: integer;
    FDays: integer;
    FSeconds: double;

    function GetAsString: string;
    procedure SetAsString(const Value: string);

  public
    class function ConvertToString(Months, Days: integer; Seconds: double): string;
    class procedure FromString(const Value: string; var Months, Days: integer; var Seconds: double);

    procedure Assign(Source: TPgInterval);
    function Compare(Value: TPgInterval): integer;
    procedure DecodeInterval(var Years, Months, Days, Hours, Minutes, Seconds, Microseconds: integer);
    procedure EncodeInterval(Years, Months, Days, Hours, Minutes, Seconds, Microseconds: integer);

    property MonthsFull: integer read FMonths write FMonths;
    property Days: integer read FDays write FDays;
    property SecondsFull: double read FSeconds write FSeconds;
    property AsString: string read GetAsString write SetAsString;
  end;

{$IFNDEF LITE}

{ Geometric types }

  TPgGeometric = class(TSharedObject)
  protected
    function GetAsString: string; virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
  public
    procedure Assign(Source: TPgGeometric); virtual; abstract;
    property AsString: string read GetAsString write SetAsString;
  end;

{ TPgPoint }

  TPgPoint = class(TPgGeometric)
  private
    FX: Double;
    FY: Double;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;        
  public
    procedure Assign(Source: TPgGeometric); override;

    property X: Double read FX write FX;
    property Y: Double read FY write FY;
  end;

{ TPgPointsArray }

  TPgPointsArray = class(TPgGeometric)
  protected
    FPoints: array of TPgPoint;

    function GetPoint(Index: integer): TPgPoint;
    function GetCount: integer;
    procedure SetCount(Value: integer);
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetIsClosed: boolean; virtual;
    procedure SetIsClosed(Value: boolean); virtual;

    property Points[Index: integer]: TPgPoint read GetPoint; default;
    property Count: integer read GetCount write SetCount;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPgGeometric); override;
  end;

{ TPgLSeg }

  TPgLSeg = class(TPgPointsArray)
  public
    constructor Create;

    property StartPoint: TPgPoint index 0 read GetPoint;
    property EndPoint: TPgPoint index 1 read GetPoint;
  end;

{ TPgBox }

  TPgBox = class(TPgPointsArray)
  public
    constructor Create;

    property LowerLeft: TPgPoint index 0 read GetPoint;
    property UpperRight: TPgPoint index 1 read GetPoint;
  end;

{ TPgPath }

  TPgPath = class(TPgPointsArray)
  private
    FIsClosed: boolean;
  protected
    function GetIsClosed: boolean; override;
    procedure SetIsClosed(Value: boolean); override;
  public
    property Points;
    property Count;
    property IsClosedPath: boolean read FIsClosed write FIsClosed;
  end;

{ TPgPolygon }

  TPgPolygon = class(TPgPointsArray)
  public
    property Points;
    property Count;
  end; 

{ TPgSQLCircle }

  TPgCircle = class(TPgGeometric)
  private
    FCenter: TPgPoint;
    FRadius: Double;
  protected   
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Assign(Source: TPgGeometric); override;

    property Center: TPgPoint read FCenter;
    property Radius: Double read FRadius write FRadius;
  end;

{ TPgAttribute }

  TPgAttribute = class (TAttribute)
  private
    FTypeOID: integer;
  public
    property TypeOID: integer read FTypeOID write FTypeOID;
  end;

{ TPgRowType }

  TPgRowType = class (TObjectType)
  private
    FTypeOID: Integer;
    FTableOID: Integer;
    FBufferSize: Integer;

    function InternalDescribe(Connection: TPgSQLConnection; const Restrictions: _string): boolean;
    procedure DescribeAttributes(Connection: TPgSQLConnection);

  public
    function Describe(Connection: TPgSQLConnection; const TypeName: _string): boolean; overload;
    function Describe(Connection: TPgSQLConnection; TypeOID: integer): boolean; overload;

    property TypeOID: Integer read FTypeOID;
    property TableOID: Integer read FTableOID;
    property BufferSize: Integer read FBufferSize;
  end;

  TPgRow = class (TDBObject)
  private
    FBuffer: IntPtr;

  protected
    procedure CheckRowType;

    procedure GetAttributeValue(const Name: _string; var AttrBuf: IntPtr; var IsBlank, NativeBuffer: boolean); override;
    procedure SetAttributeValue(const Name: _string; Source: IntPtr); override;
    function GetAttrIsNull(const Name: _string): boolean; override;
    procedure SetAttrIsNull(const Name: _string; Value: boolean);

    procedure AllocBuffer;
    procedure FreeBuffer;
    function GetBuffer: IntPtr;
    procedure CreateComplexFields;
    procedure FreeComplexFields;

    function GetRowType: TPgRowType;
    procedure SetRowType(Value: TPgRowType);
    function GetAttrValue(const Name: _string): variant;
    procedure SetAttrValue(const Name: _string; const Value: variant);

    function GetAsObject(const Name: _string; DataType: word): TSharedObject;
    procedure SetAsObject(const Name: _string; DataType: word; Value: TSharedObject);
    function GetAsPgDate(const Name: _string): TPgDate;
    procedure SetAsPgDate(const Name: _string; Value: TPgDate);
    function GetAsPgTime(const Name: _string): TPgTime;
    procedure SetAsPgTime(const Name: _string; Value: TPgTime);
    function GetAsPgTimeStamp(const Name: _string): TPgTimeStamp;
    procedure SetAsPgTimeStamp(const Name: _string; Value: TPgTimeStamp);
    function GetAsPgInterval(const Name: _string): TPgInterval;
    procedure SetAsPgInterval(const Name: _string; Value: TPgInterval);
    function GetAsPgLargeObject(const Name: _string): TPgSQLLargeObject;
    procedure SetAsPgLargeObject(const Name: _string; Value: TPgSQLLargeObject);
    function GetAsPgPoint(const Name: _string): TPgPoint;
    procedure SetAsPgPoint(const Name: _string; Value: TPgPoint);
    function GetAsPgLSeg(const Name: _string): TPgLSeg;
    procedure SetAsPgLSeg(const Name: _string; Value: TPgLSeg);
    function GetAsPgBox(const Name: _string): TPgBox;
    procedure SetAsPgBox(const Name: _string; Value: TPgBox);
    function GetAsPgPath(const Name: _string): TPgPath;
    procedure SetAsPgPath(const Name: _string; Value: TPgPath);
    function GetAsPgPolygon(const Name: _string): TPgPolygon;
    procedure SetAsPgPolygon(const Name: _string; Value: TPgPolygon);
    function GetAsPgCircle(const Name: _string): TPgCircle;
    procedure SetAsPgCircle(const Name: _string; Value: TPgCircle);
    function GetAsPgCursor(const Name: _string): TPgCursor;
    procedure SetAsPgCursor(const Name: _string; Value: TPgCursor);
    function GetAsPgRow(const Name: _string): TPgRow;
    procedure SetAsPgRow(const Name: _string; Value: TPgRow);

    function GetAsString: string;
    procedure SetAsString(const Value: string);

  public
    constructor Create(RowType: TPgRowType = nil);
    destructor Destroy; override;

    procedure Assign(Source: TPgRow);
    function AttrValueEx(const Name: _string; AllowObject: boolean): variant;

    property RowType: TPgRowType read GetRowType write SetRowType;
    property Buffer: IntPtr read GetBuffer;
    property AsString: string read GetAsString write SetAsString;

    property AttrIsNull[const Name: _string]: boolean read GetAttrIsNull write SetAttrIsNull;
    property AttrValue[const Name: _string]: variant read GetAttrValue write SetAttrValue;

    property AttrAsPgDate[const Name: _string]: TPgDate read GetAsPgDate write SetAsPgDate;
    property AttrAsPgTime[const Name: _string]: TPgTime read GetAsPgTime write SetAsPgTime;
    property AttrAsPgTimeStamp[const Name: _string]: TPgTimeStamp read GetAsPgTimeStamp write SetAsPgTimeStamp;
    property AttrAsPgInterval[const Name: _string]: TPgInterval read GetAsPgInterval write SetAsPgInterval;
    property AttrAsPgLargeObject[const Name: _string]: TPgSQLLargeObject read GetAsPgLargeObject write SetAsPgLargeObject;
    property AttrAsPgPoint[const Name: _string]: TPgPoint read GetAsPgPoint write SetAsPgPoint;
    property AttrAsPgLSeg[const Name: _string]: TPgLSeg read GetAsPgLSeg write SetAsPgLSeg;
    property AttrAsPgBox[const Name: _string]: TPgBox read GetAsPgBox write SetAsPgBox;
    property AttrAsPgPath[const Name: _string]: TPgPath read GetAsPgPath write SetAsPgPath;
    property AttrAsPgPolygon[const Name: _string]: TPgPolygon read GetAsPgPolygon write SetAsPgPolygon;
    property AttrAsPgCircle[const Name: _string]: TPgCircle read GetAsPgCircle write SetAsPgCircle;
    property AttrAsPgCursor[const Name: _string]: TPgCursor read GetAsPgCursor write SetAsPgCursor;
    property AttrAsPgRow[const Name: _string]: TPgRow read GetAsPgRow write SetAsPgRow;
  end;

{$ENDIF NDEF LITE}

  TPgObjectsUtils = class
  public
    class function GetLargeObjectOIDChanged(Obj: TPgSQLLargeObject): boolean;
    class procedure LargeObjectResetOldOID(Obj: TPgSQLLargeObject);
  end;

{$IFDEF MSWINDOWS}
function GetUTCOffsetByUTCTime(Time: TDateTime): integer; // seconds
function GetUTCOffset(Time: TDateTime): integer;
{$ENDIF}
function AddTimeSpan(Value: TDateTime; Span: double): TDateTime; overload;
procedure AddTimeSpan(var Days: integer; var Ticks: int64; Span: int64); overload;
{$IFDEF MSWINDOWS}
function UTCTimeToLocalTime(UTCTime: TDateTime): TDateTime; overload;
function LocalTimeToUTCTime(LocalTime: TDateTime): TDateTime; overload;
procedure UTCTimeToLocalTime(var Days: integer; var Ticks: int64); overload;
procedure LocalTimeToUTCTime(var Days: integer; var Ticks: int64); overload;
{$ENDIF}

{$IFDEF WIN32_64}
var
  PgTimeZoneInformation: TTimeZoneInformation;
{$ENDIF}

implementation

uses
  DAConsts, {$IFNDEF UNIDACPRO}PgConsts{$ELSE}PgConstsUni{$ENDIF};

{ TPgRefCursor }

destructor TPgRefCursor.Destroy;
begin
  inherited;
end;

procedure TPgRefCursor.Assign(Source: TPgCursor);
begin
  FCursorName := TPgRefCursor(Source).CursorName;
  FConnection := TPgRefCursor(Source).FConnection;
end;

procedure TPgRefCursor.Assign(Connection: TPgSQLConnection; CursorName: _string);
begin
  FConnection := Connection;
  FCursorName := CursorName;
end;

function TPgRefCursor.CanFetch: boolean;
begin
  Result := FCursorName <> '';
end;

{ TPgSQLLargeObject }

constructor TPgSQLLargeObject.Create(Connection: TPgSQLConnection);
begin
  inherited Create;

  FCached := True;
  FConnection := Connection;
end;

destructor TPgSQLLargeObject.Destroy;
begin
  CloseObject;

  inherited;
end;

procedure TPgSQLLargeObject.CreateObject;
begin
  CheckConnection;
  CloseObject;
  
  SetOID(GetProtocol.lo_create(0)); // mode param is reseved

  OpenObject; 
end;

procedure TPgSQLLargeObject.OpenObject();
begin
  CheckConnection;
  CloseObject;

  FObjHandle := GetProtocol.lo_open(FOID, $00020000 or $00040000); //TODO: поддержкать read, write
   
  FIsOpened := True;
  FPosition := 0;  
end;

procedure TPgSQLLargeObject.CloseObject();
begin
  if not FIsOpened then
    Exit;

  try
    GetProtocol.lo_close(FObjHandle);  // the object
  except
  end;

  FIsOpened := False;
  FPosition := 0;  
end;

procedure TPgSQLLargeObject.UnlinkObject();
begin
  CheckConnection;
  CloseObject;

  if not IsCreated then
    Assert(False); // TODO message

  GetProtocol.lo_unlink(OID);
  FOID := 0;  
end;

procedure TPgSQLLargeObject.FreeBlob;
begin
  CloseObject;
end;

procedure TPgSQLLargeObject.Disconnect;
begin
  FreeBlob;
end;

procedure TPgSQLLargeObject.Write(Position, Count: Cardinal; Source: IntPtr);
begin
  if not FCached then begin
    CheckConnection;
    CheckCreated;
    CheckOpened;

    if Position <> FPosition then
      Seek(Position, soBeginning);

    Inc(FPosition, GetProtocol.lo_write(FObjHandle, Source, Count));
  end
  else
    inherited Write(Position, Count, Source)
end;

function TPgSQLLargeObject.Read(Position, Count: Cardinal; Dest: IntPtr): Cardinal;
begin
  if not FCached then begin
    CheckConnection;
    CheckOpened;

    if Position <> FPosition then
      Seek(Position, soBeginning);

    Result := GetProtocol.lo_read(FObjHandle, Dest, Count); 
    Inc(FPosition, Result);
  end
  else
    Result := inherited Read(Position, Count, Dest);
end;

procedure TPgSQLLargeObject.Commit;
begin
  inherited;

  ResetOldOID;
end;

procedure TPgSQLLargeObject.Cancel;
begin
  inherited;

  ResetOldOID;
end;

procedure TPgSQLLargeObject.WriteBlob;
var
  Piece: PPieceHeader;
  BufLen: Integer;
  Offset: Integer;
  BytesToWrite: word;
  BytesWrote: word;
begin
  Assert(Cached);

  CheckConnection;
  CheckCreated;
  CheckOpened;
  try
    Seek(0, soBeginning);

    Piece := FirstPiece;

    while IntPtr(Piece) <> nil do begin
      Offset := 0;
      BufLen := Piece.Used;

      if BufLen > PieceSize then
        BytesToWrite := PieceSize
      else
        BytesToWrite := Piece.Used;

      while BufLen > 0 do begin
        BytesWrote := GetProtocol.lo_write(FObjHandle, PtrOffset(Piece, Sizeof(TPieceHeader) + Offset), BytesToWrite);
        Inc(FPosition, BytesWrote);
        Dec(BufLen, BytesWrote);
        Inc(Offset, BytesWrote);
      end;
      Piece := Piece.Next;
    end;
  finally
    CloseObject;
  end;
end;

procedure TPgSQLLargeObject.ReadBlob;
var
  Piece: PPieceHeader;
begin
  Piece := nil;
  ReadBlob(Piece);
end;

procedure TPgSQLLargeObject.ReadBlob(var SharedPiece: PPieceHeader);
var
  Piece, Piece2: PPieceHeader;
  BytesRead: integer;
begin
  CheckConnection;
  CheckOpened;
  try
    FData.Clear;

    Seek(0, soBeginning);

    if IntPtr(SharedPiece) = nil then
      AllocPiece(Piece, PieceSize)
    else
      Piece := SharedPiece;

    repeat
      BytesRead := GetProtocol.lo_read(FObjHandle, PtrOffset(Piece, Sizeof(TPieceHeader)),
        PieceSize);

      Piece.Used := BytesRead;

      if BytesRead = 0 then begin
        if IntPtr(Piece) <> SharedPiece then
          FreePiece(Piece);

        Exit;  
      end;

      if Piece = SharedPiece then begin
        if Piece.Used < Piece.Size div 2 then begin
          AllocPiece(Piece2, Piece.Used);
          CopyBuffer(PtrOffset(Piece, SizeOf(TPieceHeader)), 
                     PtrOffset(Piece2, SizeOf(TPieceHeader)), Piece.Used);
          Piece2.Used := Piece.Used;
          Piece := Piece2;
        end
        else
          SharedPiece := nil;
      end
      else
        if Piece.Used < Piece.Size div 2 then
          CompressPiece(Piece);

      AppendPiece(Piece);

      if BytesRead < Integer(PieceSize) then
        break
      else
        AllocPiece(Piece, PieceSize);
    until False;
  finally
    CloseObject;
  end;
end;

function TPgSQLLargeObject.GetConnection: TPgSQLConnection;
begin
  Result := TPgSQLConnection(FConnection);
end;

function TPgSQLLargeObject.GetProtocol: TPgSQLProtocol;
begin
  Result := GetConnection.GetProtocol;
end;

function TPgSQLLargeObject.IsCreated: boolean;
begin
  Result := OID <> 0;
end;

procedure TPgSQLLargeObject.CheckConnection;
var
  Connection: TPgSQLConnection;
begin
  Connection := GetConnection;
  if Connection = nil then
    raise Exception.Create(SConnectionNotDefined);
  if not Connection.GetConnected then
    raise Exception.Create(SConnectionNotConnected);
  if not Connection.GetInTransaction then
    raise Exception.Create(SBlobNeedTransaction);
end;

procedure TPgSQLLargeObject.CheckCreated;
begin
  if not IsCreated then
    CreateObject;
end;

procedure TPgSQLLargeObject.CheckOpened;
begin
  if not FIsOpened then
    OpenObject;
end;

function TPgSQLLargeObject.Seek(Position: integer; Origin: TSeekOrigin): integer;
begin
  CheckConnection;
  CheckOpened;

  Result := GetProtocol.lo_lseek(FObjHandle, Position, Integer(Origin));
  FPosition := Result;
end;

function TPgSQLLargeObject.GetSize: Cardinal;
begin
  if not FCached then begin
    if not IsCreated then
      Result := 0
    else
      Result := Seek(0, soEnd);
  end  
  else
    Result := inherited GetSize;
end;

procedure TPgSQLLargeObject.ResetOldOID;
begin
  FOldOID := FOID;
end;

procedure TPgSQLLargeObject.SetOID(Value: OID);
begin
  if FOID <> Value then begin
    CloseObject;

    FOldOID := FOID;   
    FOID := Value;
  end;
end;

procedure TPgSQLLargeObject.SetCached(Value: boolean);
begin
  FCached := Value;
end;

{ TCustomPgTimeStamp }

{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
function GetDayOfWeek(year, month, targetDayOfWeek, numberOfSunday, hour, minute,
  second, millisecond: integer): TDateTime;
var
  time: TDateTime;
  days: integer;
begin
  if numberOfSunday <= 4 then begin
    time := EncodeDateTime(year, month, 1, hour, minute, second, millisecond);
    days := targetDayOfWeek - (DayOfWeek(time) - 1);
    if days < 0 then
      days := days + 7;
    days := days + 7 * (numberOfSunday - 1);
    if days > 0 then
      time := time + days;
  end
  else begin
    time := EncodeDateTime(year, month, DaysInAMonth(year, month), hour, minute, second, millisecond);
    days := DayOfWeek(time) - 1 - targetDayOfWeek;
    if days < 0 then
      days := days + 7;
    if days > 0 then
      time := time - days;
  end;
  Result := time;
end;

procedure GetDaylightChanges(AYear: integer; var StartTime, EndTime: TDateTime);
begin
  if PgTimeZoneInformation.DaylightDate.wMonth = 0 then begin
    StartTime := NaN;
    EndTime := NaN;
  end
  else begin
    with PgTimeZoneInformation.DaylightDate do
      StartTime := GetDayOfWeek(AYear, wMonth, wDayOfWeek, wDay,
        wHour, wMinute, wSecond, wMilliseconds);
    with PgTimeZoneInformation.StandardDate do
      EndTime := GetDayOfWeek(AYear, wMonth, wDayOfWeek, wDay,
        wHour, wMinute, wSecond, wMilliseconds);
  end;
end;
{$ENDIF}

function GetUTCOffsetByUTCTime(Time: TDateTime): integer; // seconds
{$IFNDEF CLR}
var
  Offset: integer;
  StartTime, EndTime, time1, time2: TDateTime;
{$ENDIF}
begin
{$IFDEF CLR}
  Result := Round(TimeZone.CurrentTimeZone.GetUTCOffset(Time).TotalSeconds);
{$ELSE}
  Offset := -PgTimeZoneInformation.Bias;
  GetDaylightChanges(YearOf(Time), StartTime, EndTime);

  if not IsNan(StartTime) then begin
    time1 := StartTime - Offset / MinsPerDay;
    time2 := (EndTime - Offset / MinsPerDay) - (-PgTimeZoneInformation.DaylightBias / MinsPerDay);

    if (time >= time1) and (time < time2) then
      Offset := Offset + (-PgTimeZoneInformation.DaylightBias);
  end;

  Result := Offset * SecsPerMin;
{$ENDIF}
end;

function GetUTCOffset(Time: TDateTime): integer;
{$IFNDEF CLR}
var
  Offset: integer;
  StartTime, EndTime, time1: TDateTime;
{$ENDIF}
begin
{$IFDEF CLR}
  Result := Round(TimeZone.CurrentTimeZone.GetUTCOffset(Time).TotalSeconds);
{$ELSE}
  Offset := -PgTimeZoneInformation.Bias;
  GetDaylightChanges(YearOf(Time), StartTime, EndTime);

  if not IsNan(StartTime) then begin
    time1 := StartTime + (-PgTimeZoneInformation.DaylightBias / MinsPerDay);

    if (time >= time1) and (time < EndTime) then
      Offset := Offset + (-PgTimeZoneInformation.DaylightBias);
  end;

  Result := Offset * SecsPerMin;
{$ENDIF}
end;
{$ENDIF}

function AddTimeSpan(Value: TDateTime; Span: double): TDateTime; overload;
var
  Date, Time, SpanDate, SpanTime: double;
begin
  Date := {$IFNDEF CLR}System.{$ENDIF}Int(Value);
  Time := Abs(Frac(Value));
  SpanDate := {$IFNDEF CLR}System.{$ENDIF}Int(Span);
  SpanTime := Frac(Span);

  Date := Date + SpanDate;
  Time := Time + SpanTime;

  if Time < 0 then begin
    Date := Date - 1;
    Time := Time + 1;
  end
  else
  if Time >= 1 then begin
    Date := Date + 1;
    Time := Time - 1;
  end;

  if Date >= 0 then
    Result := Date + Time
  else
    Result := Date - Time;
end;

procedure AddTimeSpan(var Days: integer; var Ticks: int64; Span: int64); overload;
begin
  Ticks := Ticks + Span;

  if Ticks < 0 then begin
    Days := Days - 1;
    Ticks := Ticks + McSecsPerDay;
  end
  else
  if Ticks >= McSecsPerDay then begin
    Days := Days + 1;
    Ticks := Ticks - McSecsPerDay;
  end;
end;

{$IFDEF MSWINDOWS}
function UTCTimeToLocalTime(UTCTime: TDateTime): TDateTime; overload;
var
  Offset: double;
begin
  Offset := GetUTCOffsetByUTCTime(UTCTime) / SecsPerDay;
  Result := AddTimeSpan(UTCTime, Offset);
  if Result < MinDateTime then
    Result := MinDateTime;
end;

function LocalTimeToUTCTime(LocalTime: TDateTime): TDateTime; overload;
var
  Offset: double;
begin
  Offset := GetUTCOffset(LocalTime) / SecsPerDay;
  Result := AddTimeSpan(LocalTime, -Offset);
  if Result < MinDateTime then
    Result := MinDateTime;
end;

procedure UTCTimeToLocalTime(var Days: integer; var Ticks: int64); overload;
var
  Offset: int64;
begin
  Offset := GetUTCOffsetByUTCTime(TPgTimeStamp.ToDateTime(Days, Ticks)) * Int64(McSecsPerSec);
  AddTimeSpan(Days, Ticks, Offset);
end;

procedure LocalTimeToUTCTime(var Days: integer; var Ticks: int64); overload;
var
  Offset: int64;
begin
  Offset := GetUTCOffsetByUTCTime(TPgTimeStamp.ToDateTime(Days, Ticks)) * Int64(McSecsPerSec);
  AddTimeSpan(Days, Ticks, -Offset);
end;
{$ENDIF}

procedure TCustomPgTimeStamp.Assign(Source: TCustomPgTimeStamp);
begin
  FDays := Source.FDays;
  FTicks := Source.FTicks;
  FHasTimeZone := Source.FHasTimeZone;
end;

class procedure TCustomPgTimeStamp.InternalDecodeDate(Days: integer; var Year, Month, Day: integer);
var
  julian, quad, extra: cardinal;
  y: integer;
begin
  Assert(Days >= -POSTGRES_EPOCH_JDATE);

  julian := Days + POSTGRES_EPOCH_JDATE;
  julian := julian + 32044;
  quad := julian div 146097;
  extra := (julian - quad * 146097) * 4 + 3;
  julian := julian + 60 + quad * 3 + extra div 146097;
  quad := julian div 1461;
  julian := julian - quad * 1461;
  y := julian * 4 div 1461;
  if y <> 0 then
    julian := (julian + 305) mod 365
  else
    julian := (julian + 306) mod 366;
  julian := julian + 123;
  y := y + integer(quad * 4);
  year := y - 4800;
  quad := julian * 2141 div 65536;
  day := julian - 7834 * quad div 256;
  month := (quad + 10) mod 12 + 1;

  if year <= 0 then
    Dec(year);
end;

class procedure TCustomPgTimeStamp.InternalDecodeTime(Ticks: int64; var Hour, Minute, Second, Microsecond: integer);
begin
  hour := Ticks div 3600000000;
  Ticks := Ticks mod 3600000000;
  minute := Ticks div 60000000;
  Ticks := Ticks mod 60000000;
  second := Ticks div 1000000;
  microsecond := Ticks mod 1000000;
end;

class procedure TCustomPgTimeStamp.InternalDecodeDateTime(Days: integer; Ticks: int64;
  var Year, Month, Day, Hour, Minute, Second, Microsecond: integer);
begin
  InternalDecodeDate(Days, Year, Month, Day);
  InternalDecodeTime(Ticks, Hour, Minute, Second, Microsecond);
end;

class procedure TCustomPgTimeStamp.InternalEncodeDate(var Days: integer; Year, Month, Day: integer);
var
  julian, century: integer;
  IsValid: boolean;
begin
  if year < 0 then
    year := year + 1;

  IsValid := ((year > JulianMinYear) or
      ((year = JulianMinYear) and ((month > JulianMinMonth) or
        ((month = JulianMinMonth) and (day >= JulianMinDay)))))
    and (year < JulianMaxYear);

  if not IsValid then
    raise EConvertError.Create('Value is either too large or too small for PgTimeStamp');

  if (month < 1) or (month > 12) then
    raise EConvertError.Create('Month is out of range');

  if (day < 1) or (day > DaysInAMonth(Year, Month)) then
    raise EConvertError.Create('Day is out of range');

  if month > 2 then begin
    month := month + 1;
    year := year + 4800;
  end
  else begin
    month := month + 13;
    year := year + 4799;
  end;

  century := year div 100;
  julian := year * 365 - 32167;
  julian := julian + year div 4 - century + century div 4;
  julian := julian + 7834 * month div 256 + day;

  Days := julian - POSTGRES_EPOCH_JDATE;
end;

class procedure TCustomPgTimeStamp.InternalEncodeTime(var Ticks: int64;
  Hour, Minute, Second, Microsecond: integer);
begin
  if (hour < 0) or (hour > 23) then
    raise EConvertError.Create('Hour is out of range');
  if (minute < 0) or (minute > 59) then
    raise EConvertError.Create('Minute is out of range');
  if (second < 0) or (second > 59) then
    raise EConvertError.Create('Second is out of range');
  if (microsecond < 0) or (microsecond > 999999) then
    raise EConvertError.Create('Microsecond is out of range');

  Ticks := Int64(((Hour * 60) + Minute) * 60 + Second) * McSecsPerSec + Int64(Microsecond);
end;

class procedure TCustomPgTimeStamp.InternalEncodeDateTime(var Days: integer; var Ticks: int64;
  Year, Month, Day, Hour, Minute, Second, Microsecond: integer);
begin
  InternalEncodeDate(Days, Year, Month, Day);
  InternalEncodeTime(Ticks, Hour, Minute, Second, Microsecond);
end;

procedure TCustomPgTimeStamp.DecodeDate(var Year, Month, Day: integer);
begin
  InternalDecodeDate(FDays, Year, Month, Day);
end;

procedure TCustomPgTimeStamp.DecodeTime(var Hour, Minute, Second, Microsecond: integer);
begin
  InternalDecodeTime(FTicks, Hour, Minute, Second, Microsecond);
end;

procedure TCustomPgTimeStamp.DecodeDateTime(var Year, Month, Day, Hour, Minute, Second, Microsecond: integer);
begin
  InternalDecodeDateTime(FDays, FTicks, Year, Month, Day, Hour, Minute, Second, Microsecond);
end;

procedure TCustomPgTimeStamp.EncodeDate(Year, Month, Day: integer);
begin
  InternalEncodeDate(FDays, Year, Month, Day);
end;

procedure TCustomPgTimeStamp.EncodeTime(Hour, Minute, Second, Microsecond: integer);
begin
  InternalEncodeTime(FTicks, Hour, Minute, Second, Microsecond);
end;

procedure TCustomPgTimeStamp.EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Microsecond: integer);
begin
  InternalEncodeDateTime(FDays, FTicks, Year, Month, Day, Hour, Minute, Second, Microsecond);
end;

function TCustomPgTimeStamp.GetTimeZoneOffset: integer;
begin
{$IFDEF MSWINDOWS}
  if FHasTimeZone then
    Result := GetUTCOffset(AsDateTime)
  else
{$ENDIF}
    Result := 0;
end;

function TCustomPgTimeStamp.GetIsInfinity: boolean;
begin
  Result := (FTicks = High(Int64)) or (FTicks = Low(Int64));
end;

function TCustomPgTimeStamp.GetIsPosInfinity: boolean;
begin
  Result := FTicks = High(Int64);
end;

function TCustomPgTimeStamp.GetIsNegInfinity: boolean;
begin
  Result := FTicks = Low(Int64);
end;

procedure TCustomPgTimeStamp.SetIsPosInfinity(Value: boolean);
begin
  FTicks := High(Int64);
end;

procedure TCustomPgTimeStamp.SetIsNegInfinity(Value: boolean);
begin
  FTicks := Low(Int64);
end;

class function TCustomPgTimeStamp.ConvertToString(Days: integer; Ticks: int64;
  HasTimeZone: boolean; TimeZoneOffset: integer; const Format: string): string;
var
  Year, Month, Day, Hour, Min, Sec, McSec: integer;
  DateDecoded, TimeDecoded: boolean;
  TzHours, TzMins, TzSecs: integer;
  sb: StringBuilder;
  AppendLevel: integer;

  procedure GetDate;
  begin
    if not DateDecoded then
    begin
      InternalDecodeDate(Days, Year, Month, Day);
      DateDecoded := True;
    end;
  end;

  procedure GetTime;
  begin
    if not TimeDecoded then
    begin
      InternalDecodeTime(Ticks, Hour, Min, Sec, McSec);
      TimeDecoded := True;
    end;
  end;

  function DayOfWeek: integer;
  begin
    Result := (Days - 1) mod 7;
    if Result < 0 then
      Inc(Result, 7);
    Inc(Result);
  end;

  procedure AppendString(const S: string);
  begin
    sb.Append(S);
  end;

  function LeftPad(const S: string; Count: integer): string;
  begin
    if Length(S) < Count then
      Result := StringOfChar('0', Count - Length(S)) + S
    else
      Result := S;
  end;

  procedure AppendNumber(Number, Digits: Integer);
  begin
    sb.Append(LeftPad(IntToStr(Number), Digits));
  end;

  procedure AppendFormat(const Format: string);
  var
    Starter, Token, LastToken: Char;
    Use12HourClock, BetweenQuotes: Boolean;
    CurrPos, Count, P, H: Integer;

    procedure GetCount;
    var
      p: integer;
    begin
      p := CurrPos;
      while (CurrPos <= Length(Format)) and (Format[CurrPos] = Starter) do Inc(CurrPos);
      Count := CurrPos - p + 1;
    end;

  begin
    if (Format <> '') and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      Use12HourClock := False;
      CurrPos := 1;
      while CurrPos <= Length(Format) do
      begin
        Starter := Format[CurrPos];
        Inc(CurrPos);
        Token := Starter;
        case Token of
          'a'..'z': Dec(Token, 32);
        end;
        case Token of
          'A'..'Z':
            begin
              if (Token = 'M') and (LastToken = 'H') then Token := 'N';
              LastToken := Token;
            end;
        end;
        case Token of
          'Y':
            begin
              GetCount;
              GetDate;
              if Count <= 2 then
                AppendNumber(Abs(Year) mod 100, 2) else
                AppendNumber(Abs(Year), 4);
            end;
          'M':
            begin
              GetCount;
              GetDate;
              case Count of
                1, 2: AppendNumber(Month, Count);
                3: AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}ShortMonthNames[Month]);
              else
                AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}LongMonthNames[Month]);
              end;
            end;
          'D':
            begin
              GetCount;
              case Count of
                1, 2:
                  begin
                    GetDate;
                    AppendNumber(Day, Count);
                  end;
                3: AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDayNames[DayOfWeek]);
                4: AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}LongDayNames[DayOfWeek]);
                5: AppendFormat({$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDateFormat);
              else
                AppendFormat({$IFDEF VER16P}FormatSettings.{$ENDIF}LongDateFormat);
              end;
            end;
          'H':
            begin
              GetCount;
              GetTime;
              BetweenQuotes := False;
              P := CurrPos;
              while P <= Length(Format) do
              begin
                case Format[P] of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if AnsiSameText('AM/PM', Copy(Format, P, 5))
                        or AnsiSameText('A/P', Copy(Format, P, 3))
                        or AnsiSameText('AMPM', Copy(Format, P, 4))
                      then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"': BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := Hour;
              if Use12HourClock then
                if H = 0 then H := 12 else if H > 12 then Dec(H, 12);
              if Count > 2 then Count := 2;
              AppendNumber(H, Count);
            end;
          'N':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Min, Count);
            end;
          'S':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Sec, Count);
            end;
          'T':
            begin
              GetCount;
              if Count = 1 then
                AppendFormat({$IFDEF VER16P}FormatSettings.{$ENDIF}ShortTimeFormat) else
                AppendFormat({$IFDEF VER16P}FormatSettings.{$ENDIF}LongTimeFormat);
            end;
          'Z':
            begin
              GetCount;
              GetTime;
              if Count <= 3 then
                AppendNumber(McSec div 1000, Count)
              else
                AppendNumber(McSec, 6);
            end;
          'A':
            begin
              GetTime;
              P := CurrPos - 1;
              if AnsiSameText('AM/PM', Copy(Format, P, 5)) then
              begin
                if Hour >= 12 then Inc(P, 3);
                AppendString(Copy(Format, P, 2));
                Inc(CurrPos, 4);
                Use12HourClock := TRUE;
              end else
              if AnsiSameText('A/P', Copy(Format, P, 3)) then
              begin
                if Hour >= 12 then Inc(P, 2);
                AppendString(Copy(Format, P, 1));
                Inc(CurrPos, 2);
                Use12HourClock := TRUE;
              end else
              if AnsiSameText('AMPM', Copy(Format, P, 4)) then
              begin
                if Hour < 12 then
                  AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}TimeAMString) else
                  AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}TimePMString);
                Inc(CurrPos, 3);
                Use12HourClock := TRUE;
              end else
              if AnsiSameText('AAAA', Copy(Format, P, 4)) then
              begin
                GetDate;
                AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}LongDayNames[DayOfWeek]);
                Inc(CurrPos, 3);
              end else
              if AnsiSameText('AAA', Copy(Format, P, 3)) then
              begin
                GetDate;
                AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDayNames[DayOfWeek]);
                Inc(CurrPos, 2);
              end else
              AppendString(Starter);
            end;
          'C':
            begin
              GetCount;
              AppendFormat({$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDateFormat);
              GetTime;
              //if (Hour <> 0) or (Min <> 0) or (Sec <> 0) then // time is required to append timezone
              begin
                AppendString(' ');
                AppendFormat({$IFDEF VER16P}FormatSettings.{$ENDIF}LongTimeFormat);
              end;
            end;
          '/':
            if {$IFDEF VER16P}FormatSettings.{$ENDIF}DateSeparator <> #0 then
              AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}DateSeparator);
          ':':
            if {$IFDEF VER16P}FormatSettings.{$ENDIF}TimeSeparator <> #0 then
              AppendString({$IFDEF VER16P}FormatSettings.{$ENDIF}TimeSeparator);
          '''', '"':
            begin
              P := CurrPos;
              while (CurrPos <= Length(Format)) and (Format[CurrPos] <> Starter) do
              begin
                Inc(CurrPos);
              end;
              AppendString(Copy(Format, P, CurrPos - P));
              if CurrPos <= Length(Format) then Inc(CurrPos);
            end;
        else
          AppendString(Starter);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  if Ticks = High(Int64) then begin
    Result := 'infinity';
    exit;
  end;
  if Ticks = Low(Int64) then begin
    Result := '-infinity';
    exit;
  end;

  AppendLevel := 0;
  DateDecoded := False;
  TimeDecoded := False;
  sb := StringBuilder.Create(128);
  try
    if Format <> '' then
      AppendFormat(Format)
    else
      AppendFormat('C');

    if HasTimeZone then begin
      TzHours := Abs(TimeZoneOffset) div 3600;
      TzSecs := Abs(TimeZoneOffset) mod 3600;
      TzMins := TzSecs div 60;
      TzSecs := TzSecs mod 60;

      if TimeZoneOffset < 0 then
        sb.Append('-')
      else
        sb.Append('+');
      AppendNumber(TzHours, 2);
      if (TzMins <> 0) or (TzSecs <> 0) then begin
        sb.Append({$IFDEF VER16P}FormatSettings.{$ENDIF}TimeSeparator);
        AppendNumber(TzMins, 2);
      end;
      if TzSecs <> 0 then begin
        sb.Append({$IFDEF VER16P}FormatSettings.{$ENDIF}TimeSeparator);
        AppendNumber(TzSecs, 2);
      end;
    end;

    if DateDecoded and (year < 0) then
      sb.Append(' BC');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

type
  TDateOrder = (doMDY, doDMY, doYMD);

procedure ScanBlanks(const S: string; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  Pos := I;
end;

function ScanNumber(const S: string; var Pos: Integer;
  var Number: integer; var CharCount: Byte): Boolean;
var
  I, N: Integer;
begin
  Result := False;
  CharCount := 0;
  ScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and (S[I] >= '0') and (S[I] <= '9') do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    CharCount := I - Pos;
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function ScanNumberToString(const S: string; var Pos: Integer;
  var Number: string; var CharCount: Byte): Boolean;
var
  I: Integer;
begin
  Result := False;
  CharCount := 0;
  ScanBlanks(S, Pos);
  I := Pos;
  while (I <= Length(S)) and (S[I] >= '0') and (S[I] <= '9') do
    Inc(I);
  if I > Pos then
  begin
    CharCount := I - Pos;
    Number := Copy(S, Pos, CharCount);
    Pos := I;
    Result := True;
  end;
end;

function ScanString(const S: string; var Pos: Integer;
  const Symbol: string): Boolean;
begin
  Result := False;
  if Symbol <> '' then
  begin
    ScanBlanks(S, Pos);
    if AnsiCompareText(Symbol, Copy(S, Pos, Length(Symbol))) = 0 then
    begin
      Inc(Pos, Length(Symbol));
      Result := True;
    end;
  end;
end;

function ScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;
begin
  Result := False;
  ScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then
  begin
    Inc(Pos);
    Result := True;
  end;
end;

function GetDateOrder(const DateFormat: string): TDateOrder;
var
  I: Integer;
begin
  Result := doMDY;

  I := 1;
  while I <= Length(DateFormat) do
  begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'E': Result := doYMD;
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
end;

function ScanDate(const S: string; var Pos: integer;
  var Year, Month, Day: integer; ForcePostgreFormat: boolean): boolean;
var
  DateOrder: TDateOrder;
  N1, N2, N3: integer;
  L1, L2, L3, YearLen: Byte;
  Sep: char;
  CenturyBase: Integer;
begin
  Result := False;
  Year := 0;
  Month := 0;
  Day := 0;
  YearLen := 0;

  if not ScanNumber(S, Pos, N1, L1) then
    Exit;

  ScanBlanks(S, Pos);
  if ForcePostgreFormat or (L1 = 4) and (S[Pos] = '-') then begin // Postgre format
    Sep := '-';
    DateOrder := doYMD;
  end
  else begin
    Sep := {$IFDEF VER16P}FormatSettings.{$ENDIF}DateSeparator{$IFDEF CLR}[1]{$ENDIF};
    DateOrder := GetDateOrder({$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDateFormat);
  end;

  if not (ScanChar(S, Pos, Sep) and ScanNumber(S, Pos, N2, L2) and
    ScanChar(S, Pos, Sep) and ScanNumber(S, Pos, N3, L3))
  then
    Exit;

  case DateOrder of
    doMDY: begin Year := N3; YearLen := L3; Month := N1; Day := N2; end;
    doDMY: begin Year := N3; YearLen := L3; Month := N2; Day := N1; end;
    doYMD: begin Year := N1; YearLen := L1; Month := N2; Day := N3; end;
  end;
  if (YearLen <= 2) then
  begin
    CenturyBase := CurrentYear - {$IFDEF VER16P}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow;
    Inc(Year, CenturyBase div 100 * 100);
    if ({$IFDEF VER16P}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow > 0) and (Year < CenturyBase) then
      Inc(Year, 100);
  end;

  ScanChar(S, Pos, {$IFDEF VER16P}FormatSettings.{$ENDIF}DateSeparator{$IFDEF CLR}[1]{$ENDIF});
  Result := True;
end;

function ScanTime(const S: string; var Pos: Integer;
  var Hour, Min, Sec, McSec, TZOffset: integer; ForcePostgreFormat: boolean): Boolean;
var
  BaseHour, TzHour, TzMin, TzSec, TzDummyMcSec: Integer;
  TimeSep, DecSep: char;
  NegOffset: boolean;

  function RightPad(const S: string; Count: integer): string;
  begin
    if Length(S) > Count then
      Result := Copy(S, 1, Count)
    else
    if Length(S) < Count then
      Result := S + StringOfChar('0', Count - Length(S))
    else
      Result := S;
  end;

  procedure ScanAMPM;
  begin
    if ScanString(S, Pos, {$IFDEF VER16P}FormatSettings.{$ENDIF}TimeAMString) or ScanString(S, Pos, 'AM') then
      BaseHour := 0
    else if ScanString(S, Pos, {$IFDEF VER16P}FormatSettings.{$ENDIF}TimePMString) or ScanString(S, Pos, 'PM') then
      BaseHour := 12;
  end;

  function ScanTimeBlock(var Hour, Min, Sec, McSec: integer): boolean;
  var
    Junk: Byte;
    SMcSec: string;
  begin
    Result := False;
    Min := 0;
    Sec := 0;
    McSec := 0;
    if not ScanNumber(S, Pos, Hour, Junk) then Exit;
    if ScanChar(S, Pos, TimeSep) then
    begin
      if not ScanNumber(S, Pos, Min, Junk) then Exit;
      if ScanChar(S, Pos, TimeSep) then
      begin
        if not ScanNumber(S, Pos, Sec, Junk) then Exit;
        if ScanChar(S, Pos, DecSep) or (DecSep <> '.') and ScanChar(S, Pos, '.') then
          if ScanNumberToString(S, Pos, SMcSec, Junk) then
            McSec := StrToInt(RightPad(SMcSec, 6))
          else
            Exit;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;
  TZOffset := MaxInt;
  BaseHour := -1;

  if ForcePostgreFormat then begin
    DecSep := '.';
    TimeSep := ':';
  end
  else begin
    DecSep := {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator{$IFDEF CLR}[1]{$ENDIF};
    TimeSep := {$IFDEF VER16P}FormatSettings.{$ENDIF}TimeSeparator{$IFDEF CLR}[1]{$ENDIF};

    ScanAMPM;
    if BaseHour >= 0 then ScanBlanks(S, Pos);
  end;

  if not ScanTimeBlock(Hour, Min, Sec, McSec) then
    Exit;

  if not ForcePostgreFormat and (BaseHour < 0) then
    ScanAMPM;

  ScanBlanks(S, Pos);
  if (S[Pos] = '+') or (S[Pos] = '-') then begin
    NegOffset := (S[Pos] = '-');
    Inc(Pos);
    if ScanTimeBlock(TzHour, TzMin, TzSec, TzDummyMcSec) then begin
      TZOffset := TzHour * 3600 + TzMin * 60 + TzSec;
      if NegOffset then
        TZOffset := -TZOffset;
    end
    else
      Exit;
  end;

  if not ForcePostgreFormat and (BaseHour < 0) then
    ScanAMPM;

  if BaseHour >= 0 then
  begin
    if (Hour = 0) or (Hour > 12) then Exit;
    if Hour = 12 then Hour := 0;
    Inc(Hour, BaseHour);
  end;

  Result := True;
end;

class procedure TCustomPgTimeStamp.FromString(const Value: string;
  var Days: integer; var Ticks: int64; var TimeZoneOffset: integer;
  DateUsed, TimeUsed, ForcePostgreFormat: boolean);

  procedure InvalidTimeStamp;
  begin
    if not TimeUsed then
      raise EConvertError.Create('Invalid DATE string')
    else
    if not DateUsed then
      raise EConvertError.Create('Invalid TIME string')
    else
      raise EConvertError.Create('Invalid TIMESTAMP string');
  end;

var
  Pos: integer;
  Year, Month, Day, Hour, Min, Sec, McSec: integer;
  DateFound, TimeFound, Neg: boolean;
begin
  Pos := 1;

  if DateUsed and TimeUsed then begin
    ScanBlanks(Value, Pos);
    Neg := False;
    if (Value[Pos] = '+') or (Value[Pos] = '-') then begin
      Neg := (Value[Pos] = '-');
      Inc(Pos);
    end;
    if ScanString(Value, Pos, 'INFINITY') then begin
      ScanBlanks(Value, Pos);
      if Pos <= Length(Value) then
        InvalidTimeStamp;
      if Neg then
        Ticks := Low(Int64)
      else
        Ticks := High(Int64);
      Days := 0;
      TimeZoneOffset := MaxInt;
      exit;
    end;
    Pos := 1;
  end;

  DateFound := False;
  TimeFound := False;

  if DateUsed then begin
    DateFound := ScanDate(Value, Pos, Year, Month, Day, ForcePostgreFormat);
    if not DateFound then
      Pos := 1;
  end;

  if TimeUsed and (Pos <= Length(Value)) then
    TimeFound := ScanTime(Value, Pos, Hour, Min, Sec, McSec, TimeZoneOffset, ForcePostgreFormat);

  if not (DateFound or TimeFound) then
    InvalidTimeStamp;

  if Pos <= Length(Value) then begin
    if DateFound then begin
      if ScanString(Value, Pos, 'BC') then
        Year := -Year
      else
        ScanString(Value, Pos, 'AD');
    end;
    ScanBlanks(Value, Pos);
    if Pos <= Length(Value) then
      InvalidTimeStamp;
  end;

  if DateFound then
    InternalEncodeDate(Days, Year, Month, Day)
  else
    Days := 0;

  if TimeFound then
    InternalEncodeTime(Ticks, Hour, Min, Sec, McSec)
  else
    Ticks := 0;
end;

{ TPgTimeStamp }

class function TPgTimeStamp.ToDateTime(Days: integer; Ticks: int64): TDateTime;
begin
  if Ticks = High(Int64) then begin
    Result := MaxDateTime;
    exit;
  end;
  if Ticks = Low(Int64) then begin
    Result := MinDateTime;
    exit;
  end;

  Result := AddTimeSpan(PostgresBaseDate, Days + Ticks / McSecsPerDay);
  if Result < MinDateTime then
    Result := MinDateTime
  else
  if Result > MaxDateTime then
    Result := MaxDateTime;
end;

class procedure TPgTimeStamp.FromDateTime(Value: TDateTime; var Days: integer; var Ticks: int64);
var
  Time: double;
begin
  Days := Trunc(Value) - Trunc(PostgresBaseDate);
  Time := Abs(Frac(Value));
  Ticks := Round(Time * McSecsPerDay);
end;

{$IFNDEF FPC}
class procedure TPgTimeStamp.FromSQLTimeStamp(const Value: TSQLTimeStamp; var Days: integer; var Ticks: int64);
begin
  InternalEncodeDateTime(Days, Ticks, Value.Year, Value.Month, Value.Day,
    Value.Hour, Value.Minute, Value.Second, Value.Fractions * 1000);
end;
{$ENDIF}

class procedure TPgTimeStamp.FromString(const Value: string; var Days: integer; var Ticks: int64;
  ForcePostgreFormat: boolean);
var
  TZOffset: integer;
{$IFDEF MSWINDOWS}
  CurOffset: integer;
{$ENDIF}
begin
  inherited FromString(Value, Days, Ticks, TZOffset, True, True, ForcePostgreFormat);
{$IFDEF MSWINDOWS}
  if TZOffset <> MaxInt then begin
    CurOffset := GetUTCOffset(ToDateTime(Days, Ticks));
    AddTimeSpan(Days, Ticks, Int64(CurOffset - TZOffset) * McSecsPerSec);
  end;
{$ENDIF}
end;

function TPgTimeStamp.Compare(Value: TCustomPgTimeStamp): integer;
begin
  if FTicks = High(Int64) then begin
    if Value.FTicks = High(Int64) then
      Result := 0
    else
      Result := 1;
  end
  else
  if FTicks = Low(Int64) then begin
    if Value.FTicks = Low(Int64) then
      Result := 0
    else
      Result := -1;
  end
  else
  if FDays > Value.FDays then
    Result := 1
  else
  if FDays < Value.FDays then
    Result := -1
  else begin
    if FTicks > Value.FTicks then
      Result := 1
    else
    if FTicks < Value.FTicks then
      Result := -1
    else
      Result := 0;
  end;
end;

function TPgTimeStamp.GetAsString: string;
begin
  Result := ConvertToString(FDays, FTicks, FHasTimeZone, GetTimeZoneOffset, '');
end;

procedure TPgTimeStamp.SetAsString(const Value: string);
begin
  FromString(Value, FDays, FTicks, False);
end;

function TPgTimeStamp.GetAsDateTime: TDateTime;
begin
  Result := ToDateTime(FDays, FTicks);
end;

procedure TPgTimeStamp.SetAsDateTime(Value: TDateTime);
begin
  FromDateTime(Value, FDays, FTicks);
end;

{$IFNDEF FPC}
function TPgTimeStamp.GetAsSQLTimeStamp: TSQLTimeStamp;
var
  Year, Month, Day, Hour, Min, Sec, McSec: integer;
  IsMin, IsMax: boolean;
begin
  IsMin := FTicks = Low(Int64);
  IsMax := FTicks = High(Int64);

  if not (IsMin or IsMax) then begin
    DecodeDateTime(Year, Month, Day, Hour, Min, Sec, McSec);
    if Year < 0 then
      IsMin := True
    else
    if Year > 9999 then
      IsMax := True;
  end;

  if IsMin then begin
    Result.Year := 1;
    Result.Month := 1;
    Result.Day := 1;
    Result.Hour := 0;
    Result.Minute := 0;
    Result.Second := 0;
    Result.Fractions := 0;
    exit;
  end;

  if IsMax then begin
    Result.Year := 9999;
    Result.Month := 12;
    Result.Day := 31;
    Result.Hour := 23;
    Result.Minute := 59;
    Result.Second := 59;
    Result.Fractions := 999;
    exit;
  end;

  Result.Year := Year;
  Result.Month := Month;
  Result.Day := Day;
  Result.Hour := Hour;
  Result.Minute := Min;
  Result.Second := Sec;
  Result.Fractions := McSec div 1000;
end;

procedure TPgTimeStamp.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  FromSQLTimeStamp(Value, FDays, FTicks);
end;
{$ENDIF}

{ TPgDate }

class function TPgDate.ToDateTime(Days: integer): TDateTime;
begin
  Result := PostgresBaseDate + Days;
  if Result < MinDateTime then
    Result := MinDateTime
  else
  if Result > MaxDateTime then
    Result := MaxDateTime;
end;

class procedure TPgDate.FromDateTime(Value: TDateTime; var Days: integer);
begin
  Days := Trunc(Value) - Trunc(PostgresBaseDate);
end;

{$IFNDEF FPC}
class procedure TPgDate.FromSQLTimeStamp(const Value: TSQLTimeStamp; var Days: integer);
begin
  InternalEncodeDate(Days, Value.Year, Value.Month, Value.Day);
end;
{$ENDIF}

class function TPgDate.ConvertToString(Days: integer; const Format: string): string;
begin
  Result := inherited ConvertToString(Days, 0, False, 0, Format);
end;

class procedure TPgDate.FromString(const Value: string; var Days: integer; ForcePostgreFormat: boolean);
var
  Ticks: int64;
  TZOffset: integer;
begin
  inherited FromString(Value, Days, Ticks, TZOffset, True, False, ForcePostgreFormat);
end;

function TPgDate.Compare(Value: TCustomPgTimeStamp): integer;
begin
  if FDays > Value.FDays then
    Result := 1
  else
  if FDays < Value.FDays then
    Result := -1
  else
    Result := 0;
end;

function TPgDate.GetAsString: string;
begin
  Result := ConvertToString(FDays, {$IFDEF VER16P}FormatSettings.{$ENDIF}ShortDateFormat);
end;

procedure TPgDate.SetAsString(const Value: string);
begin
  FromString(Value, FDays, False);
end;

function TPgDate.GetAsDateTime: TDateTime;
begin
  Result := ToDateTime(FDays);
end;

procedure TPgDate.SetAsDateTime(Value: TDateTime);
begin
  FromDateTime(Value, FDays);
end;

{$IFNDEF FPC}
function TPgDate.GetAsSQLTimeStamp: TSQLTimeStamp;
var
  Year, Month, Day: integer;
  IsMin, IsMax: boolean;
begin
  DecodeDate(Year, Month, Day);
  IsMin := False;
  IsMax := False;
  if Year < 0 then
    IsMin := True
  else
  if Year > 9999 then
    IsMax := True;

  if IsMin then begin
    Result.Year := 1;
    Result.Month := 1;
    Result.Day := 1;
  end
  else
  if IsMax then begin
    Result.Year := 9999;
    Result.Month := 12;
    Result.Day := 31;
  end
  else begin
    Result.Year := Year;
    Result.Month := Month;
    Result.Day := Day;
  end;

  Result.Hour := 0;
  Result.Minute := 0;
  Result.Second := 0;
  Result.Fractions := 0;
end;

procedure TPgDate.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  FromSQLTimeStamp(Value, FDays);
end;
{$ENDIF}

{ TPgTime }

class function TPgTime.ToDateTime(Ticks: int64): TDateTime;
begin
  Result := Ticks / McSecsPerDay;
end;

class procedure TPgTime.FromDateTime(Value: TDateTime; var Ticks: int64);
var
  Time: double;
begin
  Time := Abs(Frac(Value));
  Ticks := Round(Time * McSecsPerDay);
end;

{$IFNDEF FPC}
class procedure TPgTime.FromSQLTimeStamp(const Value: TSQLTimeStamp; var Ticks: int64);
begin
  InternalEncodeTime(Ticks, Value.Hour, Value.Minute, Value.Second, Value.Fractions * 1000);
end;
{$ENDIF}

class function TPgTime.ConvertToString(Ticks: int64; HasTimeZone: boolean;
  TimeZoneOffset: integer; const Format: string): string;
begin
  Result := inherited ConvertToString(0, Ticks, HasTimeZone, TimeZoneOffset, Format);
end;

class procedure TPgTime.FromString(const Value: string; var Ticks: int64; var TimeZoneOffset: integer;
  ForcePostgreFormat: boolean);
var
  Days: integer;
  TZOffset: integer;
begin
  inherited FromString(Value, Days, Ticks, TZOffset, False, True, ForcePostgreFormat);
  if (TZOffset <> MaxInt) or ForcePostgreFormat then
    TimeZoneOffset := TZOffset
  else
  {$IFDEF MSWINDOWS}
    TimeZoneOffset := GetUTCOffset(Now);
  {$ELSE}
    TimeZoneOffset := 0;
  {$ENDIF}
end;

procedure TPgTime.Assign(Source: TCustomPgTimeStamp);
begin
  inherited;

  if Source is TPgTime then
    FTimeZoneOffset := TPgTime(Source).FTimeZoneOffset;
end;

function TPgTime.Compare(Value: TCustomPgTimeStamp): integer;
begin
  if FTicks > Value.FTicks then
    Result := 1
  else
  if FTicks < Value.FTicks then
    Result := -1
  else
    Result := 0;
end;

function TPgTime.GetAsString: string;
begin
  Result := ConvertToString(FTicks, FHasTimeZone, FTimeZoneOffset, {$IFDEF VER16P}FormatSettings.{$ENDIF}LongTimeFormat);
end;

procedure TPgTime.SetAsString(const Value: string);
begin
  FromString(Value, FTicks, FTimeZoneOffset, False);
end;

function TPgTime.GetAsDateTime: TDateTime;
begin
  Result := ToDateTime(FTicks);
end;

procedure TPgTime.SetAsDateTime(Value: TDateTime);
begin
  FromDateTime(Value, FTicks);
end;

{$IFNDEF FPC}
function TPgTime.GetAsSQLTimeStamp: TSQLTimeStamp;
var
  Hour, Min, Sec, McSec: integer;
begin
  DecodeTime(Hour, Min, Sec, McSec);
  Result.Year := 0;
  Result.Month := 0;
  Result.Day := 0;
  Result.Hour := Hour;
  Result.Minute := Min;
  Result.Second := Sec;
  Result.Fractions := McSec div 1000;
end;

procedure TPgTime.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  FromSQLTimeStamp(Value, FTicks);
end;
{$ENDIF}

function TPgTime.GetTimeZoneOffset: integer;
begin
  Result := FTimeZoneOffset;
end;

{ TPgInterval }

procedure TPgInterval.Assign(Source: TPgInterval);
begin
  FMonths := Source.FMonths;
  FDays := Source.FDays;
  FSeconds := Source.FSeconds;
end;

function TPgInterval.Compare(Value: TPgInterval): integer;
begin
  if FMonths > Value.FMonths then
    Result := 1
  else
  if FMonths < Value.FMonths then
    Result := -1
  else begin
    if FDays > Value.FDays then
      Result := 1
    else
    if FDays < Value.FDays then
      Result := -1
    else begin
      if FSeconds > Value.FSeconds then
        Result := 1
      else
      if FSeconds < Value.FSeconds then
        Result := -1
      else
        Result := 0;
    end;
  end;
end;

procedure TPgInterval.DecodeInterval(var Years, Months, Days,
  Hours, Minutes, Seconds, Microseconds: integer);
begin
  Years := FMonths div 12;
  Months := FMonths mod 12;
  Days := FDays;

  Microseconds := Round(Frac(FSeconds) * 1E6);
  Seconds := Trunc(FSeconds);
  Hours := Seconds div 3600;
  Seconds := Seconds mod 3600;
  Minutes := Seconds div 60;
  Seconds := Seconds mod 60;
end;

procedure TPgInterval.EncodeInterval(Years, Months, Days,
  Hours, Minutes, Seconds, Microseconds: integer);
begin
  FMonths := Years * 12 + Months;
  FDays := Days;
  FSeconds := Hours * 3600 + Minutes * 60 + Seconds + Microseconds / 1E6;
end;

class function TPgInterval.ConvertToString(Months, Days: integer; Seconds: double): string;
var
  Years, Mon, Hours, Mins, Secs: integer;
  FracSecs: double;
  s: string;
begin
  Years := Months div 12;
  Mon := Months mod 12;

  Secs := Abs(Trunc(Seconds));
  FracSecs := Abs(Frac(Seconds));
  Hours := Secs div 3600;
  Secs := Secs mod 3600;
  Mins := Secs div 60;
  Secs := Secs mod 60;

  Result := '';
  if Years <> 0 then
    Result := Result + IntToStr(Years) + ' years ';
  if Mon <> 0 then
    Result := Result + IntToStr(Mon) + ' mons ';
  if Days <> 0 then begin
    if (Months < 0) and (Days > 0) then
      Result := Result + '+';
    Result := Result + IntToStr(Days) + ' days ';
  end;

  if Seconds <> 0 then begin
    if Seconds < 0 then
      Result := Result + '-'
    else
    if (Days < 0) or (Days = 0) and (Months < 0) then
      Result := Result + '+';
    if Hours < 10 then
      Result := Result + '0';
    Result := Result + IntToStr(Hours) + ':';
    if Mins < 10 then
      Result := Result + '0';
    Result := Result + IntToStr(Mins) + ':';
    if Secs < 10 then
      Result := Result + '0';
    Result := Result + IntToStr(Secs);
    if FracSecs <> 0 then begin
      s := FormatFloat('.######', FracSecs);
      if s <> '' then begin
        s[1] := '.';
        Result := Result + s;
      end;
    end;
  end;
end;

class procedure TPgInterval.FromString(const Value: string;
  var Months, Days: integer; var Seconds: double);
var
  Secs, McSecs: int64;
  FmtSecs: double;
  timeCounter, i, startPos, endPos, len: integer;
  NegTime, IsDigit: boolean;
  lastChar: char;
  s: string;

  procedure InvalidInterval;
  begin
    raise EConvertError.Create('Invalid interval string');
  end;

begin
  Months := 0;
  Days := 0;
  Secs := 0;
  McSecs := 0;
  FmtSecs := 0;
  TimeCounter := 0;
  NegTime := False;

  startPos := 0;
  endPos := 0;
  len := Length(Value);
  lastChar := #0;
  for i := 1 to len do begin
    isDigit := (Value[i] >= '0') and (Value[i] <= '9');
    if IsDigit or (Value[i] = '-') or (Value[i] = '+') then begin
      if startPos = 0 then
        startPos := i
      else begin
        if not isDigit then
          InvalidInterval;
      end
    end
    else begin
      if (startPos > 0) and (endPos = 0) then
        endPos := i;

      if Value[i] = ' ' then
        continue;

      lastChar := Value[i];
      if (endPos > 1) and (startPos > 0) then
        case lastChar of
          'y', 'Y': begin
            Months := Months + StrToInt(Copy(Value, startPos, endPos - startPos)) * 12;
            endPos := 0;
            startPos := 0;
          end;
          'm', 'M': begin
            if (Value[i + 1] = 'i') and (Value[i + 2] = 'l') then // milliseconds
              McSecs := McSecs + StrToInt(Copy(Value, startPos, endPos - startPos)) * 1000
            else
            if (Value[i + 1] = 'i') and (Value[i + 2] = 'c') then // microseconds
              McSecs := McSecs + StrToInt(Copy(Value, startPos, endPos - startPos))
            else
            if Value[i + 1] = 'i' then
              Secs := Secs + StrToInt(Copy(Value, startPos, endPos - startPos)) * 60
            else
              Months := Months + StrToInt(Copy(Value, startPos, endPos - startPos));
            endPos := 0;
            startPos := 0;
          end;
          'w', 'W': begin
            Days := Days + StrToInt(Copy(Value, startPos, endPos - startPos)) * 7;
            endPos := 0;
            startPos := 0;
          end;
          'd', 'D': begin
            Days := Days + StrToInt(Copy(Value, startPos, endPos - startPos));
            endPos := 0;
            startPos := 0;
          end;
          's', 'S': begin
            Secs := Secs + StrToInt(Copy(Value, startPos, endPos - startPos));
            endPos := 0;
            startPos := 0;
          end;
          'h', 'H': begin
            Secs := Secs + StrToInt(Copy(Value, startPos, endPos - startPos)) * 3600;
            endPos := 0;
            startPos := 0;
          end;
          ':': begin
            case timeCounter of
              0: begin
                FmtSecs := Abs(StrToInt(Copy(Value, startPos, endPos - startPos))) * 3600;
                if Value[startPos] = '-' then
                  NegTime := True;
              end;
              1: begin
                if Value[startPos] = '-' then
                  InvalidInterval;
                FmtSecs := FmtSecs + StrToInt(Copy(Value, startPos, endPos - startPos)) * 60;
              end;
            else
              InvalidInterval;
            end;
            Inc(timeCounter);
            endPos := 0;
            startPos := 0;
          end;
          '.': begin
            if Value[startPos] = '-' then
              InvalidInterval;
            FmtSecs := FmtSecs + StrToInt(Copy(Value, startPos, endPos - startPos));
            endPos := 0;
            startPos := 0;
          end;
        end;
    end;
  end;

  if startPos > 0 then begin
    if endPos = 0 then
      endPos := len + 1;

    case lastChar of
      '.': begin
        if Value[startPos] = '-' then
          InvalidInterval;
        s := '0' + {$IFDEF VER16P}FormatSettings.{$ENDIF}DecimalSeparator + Copy(Value, startPos, endPos - startPos);
        FmtSecs := FmtSecs + StrToFloat(s);
      end;
    else
      case timeCounter of
        0: begin
          FmtSecs := FmtSecs + Abs(StrToInt(Copy(Value, startPos, endPos - startPos)));
          if Value[startPos] = '-' then
            NegTime := True;
        end;
        1: begin
          if Value[startPos] = '-' then
            InvalidInterval;
          FmtSecs := FmtSecs + StrToInt(Copy(Value, startPos, endPos - startPos)) * 60;
        end;
      else
        if Value[startPos] = '-' then
          InvalidInterval;
        FmtSecs := FmtSecs + StrToInt(Copy(Value, startPos, endPos - startPos));
      end;
    end;
  end;

  if NegTime then
    FmtSecs := -FmtSecs;

  Seconds := Secs + FmtSecs + McSecs / McSecsPerSec;
end;

function TPgInterval.GetAsString: string;
begin
  Result := ConvertToString(FMonths, FDays, FSeconds);
end;

procedure TPgInterval.SetAsString(const Value: string);
begin
  FromString(Value, FMonths, FDays, FSeconds);
end;

{$IFNDEF LITE}

{ TPgPoint }

procedure TPgPoint.Assign(Source: TPgGeometric);
begin
  if not (Source is TPgPoint) then
     Exit;
     
  FX := TPgPoint(Source).FX;
  FY := TPgPoint(Source).FY;
end;

function TPgPoint.GetAsString: string;
var
  x, y: string;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
  x := FloatToStr(FX, FmtSet);
  y := FloatToStr(FY, FmtSet);
{$ELSE}
  x := FloatToStr(FX);
  if DecimalSeparator <> '.' then
    x := StringReplace(x, DecimalSeparator, '.', []);
  y := FloatToStr(FY);
  if DecimalSeparator <> '.' then
    y := StringReplace(y, DecimalSeparator, '.', []);
{$ENDIF}
  Result := '(' + x + ',' + y + ')';
end;

procedure TPgPoint.SetAsString(const Value: string);

  procedure InvalidString;
  begin
    raise EConvertError.Create('Invalid POINT string');
  end;

var
  SepPos: integer;
  s, x, y: string;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
  s := Trim(Value);
  if s[1] = '(' then begin
    if s[Length(s)] <> ')' then
      InvalidString;
    s := Copy(s, 2, Length(s) - 2);
  end;

  SepPos := Pos(',', s);
  if SepPos = 0 then
    InvalidString;
  x := Trim(Copy(s, 1, SepPos - 1));
  y := Trim(Copy(s, SepPos + 1, MaxInt));
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
  FX := StrToFloat(x, FmtSet);
  FY := StrToFloat(y, FmtSet);
{$ELSE}
  if DecimalSeparator <> '.' then
    x := StringReplace(x, '.', DecimalSeparator, []);
  FX := StrToFloat(x);
  if DecimalSeparator <> '.' then
    y := StringReplace(y, '.', DecimalSeparator, []);
  FY := StrToFloat(y);
{$ENDIF}
end;

{ TPgPointsArray }

destructor TPgPointsArray.Destroy;
begin
  SetCount(0);

  inherited;
end;

procedure TPgPointsArray.Assign(Source: TPgGeometric);
var
  i: integer;
begin
  if not (Source is TPgPointsArray) then
    Exit;
     
  SetCount(TPgPointsArray(Source).GetCount);
  for i := 0 to Length(FPoints) - 1 do
    FPoints[i].Assign(TPgPointsArray(Source).FPoints[i]);
end;

function TPgPointsArray.GetAsString: string;
var
  i: integer;
begin
  if GetIsClosed then
    Result := '('
  else
    Result := '[';
  for i := 0 to Length(FPoints) - 1 do begin
    if i > 0 then
      Result := Result + ',';
    Result := Result + FPoints[i].AsString;
  end;
  if GetIsClosed then
    Result := Result + ')'
  else
    Result := Result + ']';
end;

procedure TPgPointsArray.SetAsString(const Value: string);

  procedure InvalidString;
  begin
    raise EConvertError.Create('Invalid geometric string');
  end;

var
  s, s2: string;
  i: integer;
  ParsedCount: integer;
  StartPos: integer;
  Index: integer;
  First: boolean;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
{$ENDIF}

  s := Trim(Value);
  if s[1] = '[' then begin
    if s[Length(s)] <> ']' then
      InvalidString;
    s := Copy(s, 2, Length(s) - 2);
    SetIsClosed(False);
  end
  else
    SetIsClosed(True);

  ParsedCount := 0;
  for i := 1 to Length(s) do
    if s[i] = ',' then
      Inc(ParsedCount);
  Count := (ParsedCount + 1) div 2;

  StartPos := 1;
  Index := 0;
  First := True;
  for i := 1 to Length(s) do
    if s[i] = '(' then
      StartPos := i + 1
    else
    if (s[i] = ',') or (s[i] = ')') then begin
      if (Index < Count) and (i - StartPos > 0) then begin
        s2 := Trim(Copy(s, StartPos, i - StartPos));
      {$IFNDEF VER7P}
        if DecimalSeparator <> '.' then
          s2 := StringReplace(s2, '.', DecimalSeparator, []);
      {$ENDIF}
        if First then
          Points[Index].X := StrToFloat(s2 {$IFDEF VER7P}, FmtSet{$ENDIF})
        else
          Points[Index].Y := StrToFloat(s2 {$IFDEF VER7P}, FmtSet{$ENDIF});
        if not First then
          Inc(Index);
        First := not First;
        StartPos := i + 1;
      end;
    end;
end;

function TPgPointsArray.GetIsClosed: boolean;
begin
  Result := True;
end;

procedure TPgPointsArray.SetIsClosed(Value: boolean);
begin
end;

function TPgPointsArray.GetPoint(Index: integer): TPgPoint;
begin
  Result := FPoints[Index];
end;

function TPgPointsArray.GetCount: integer;
begin
  Result := Length(FPoints);
end;

procedure TPgPointsArray.SetCount(Value: integer);
var
  i: integer;
  OldCount: integer;
begin
  OldCount := Length(FPoints);
  if Value > OldCount then begin
    SetLength(FPoints, Value);
    for i := OldCount to Value - 1 do
      FPoints[i] := TPgPoint.Create;
  end
  else begin
    for i := Value to OldCount - 1 do
      FPoints[i].Free;
    SetLength(FPoints, Value);  
  end;
end;

{ TPgBox }

constructor TPgBox.Create;
begin
  inherited;

  SetCount(2);
end;

{ TPgLSeg }

constructor TPgLSeg.Create;
begin
  inherited;

  SetCount(2);
end;

{ TPgPath }

function TPgPath.GetIsClosed: boolean;
begin
  Result := FIsClosed;
end;

procedure TPgPath.SetIsClosed(Value: boolean);
begin
  FIsClosed := Value;
end;

{ TPgSQLCircle }

constructor TPgCircle.Create;
begin
  inherited;

  FCenter := TPgPoint.Create;
end;

destructor TPgCircle.Destroy;
begin
  FCenter.Free;
  
  inherited;
end;

procedure TPgCircle.Assign(Source: TPgGeometric);
begin
  if not (Source is TPgCircle) then
    Exit;
    
  FCenter.Assign(TPgCircle(Source).FCenter);
  FRadius := TPgCircle(Source).FRadius;
end;

function TPgCircle.GetAsString;
var
  c, r: string;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
  c := FCenter.GetAsString;
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
  r := FloatToStr(FRadius, FmtSet);
{$ELSE}
  r := FloatToStr(FRadius);
  if DecimalSeparator <> '.' then
    r := StringReplace(r, DecimalSeparator, '.', []);
{$ENDIF}
  Result := '<' + c + ',' + r + '>';
end;

procedure TPgCircle.SetAsString(const Value: string);
var
  i: integer;
  InPoint: boolean;
  StartPos: integer;
  s: string;
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
begin
{$IFDEF VER7P}
  FmtSet.DecimalSeparator := '.';
{$ENDIF}
  InPoint := True;
  StartPos := 0;
  for i := 1 to Length(Value) do
    if (Value[i] = '<') or (Value[i] = '(') then
      StartPos := i + 1
    else
    if Value[i] = ',' then begin
      if StartPos > 0 then begin
        s := Trim(Copy(Value, StartPos, i - StartPos));
      {$IFNDEF VER7P}
        if DecimalSeparator <> '.' then
          s := StringReplace(s, '.', DecimalSeparator, []);
      {$ENDIF}
        Center.X := StrToFloat(s {$IFDEF VER7P}, FmtSet{$ENDIF});
      end;
      StartPos := i + 1;
    end
    else
    if (Value[i] = '>') or (Value[i] = ')') then begin
      if StartPos > 0 then
        if InPoint then begin
          s := Trim(Copy(Value, StartPos, i - StartPos));
        {$IFNDEF VER7P}
          if DecimalSeparator <> '.' then
            s := StringReplace(s, '.', DecimalSeparator, []);
        {$ENDIF}
          Center.Y := StrToFloat(s {$IFDEF VER7P}, FmtSet{$ENDIF});
          InPoint := False;
        end
        else begin
          s := Trim(Copy(Value, StartPos, i - StartPos));
        {$IFNDEF VER7P}
          if DecimalSeparator <> '.' then
            s := StringReplace(s, '.', DecimalSeparator, []);
        {$ENDIF}
          Radius := StrToFloat(s {$IFDEF VER7P}, FmtSet{$ENDIF});
        end;
      StartPos := 0;
    end
end;

{ TPgRowType }

function TPgRowType.Describe(Connection: TPgSQLConnection; const TypeName: _string): boolean;
var
  Restrictions: _string;
  p: integer;
  Schema, Name: _string;
begin
  p := Pos('.', TypeName);
  if p > 0 then begin
    Schema := PgSQLInfo.NormalizeName(Copy(TypeName, 1, p - 1), False, True);
    Name := PgSQLInfo.NormalizeName(Copy(TypeName, p + 1, MaxInt), False, True);
  end
  else begin
    Schema := Connection.GetCurrentSchema;
    Name := PgSQLInfo.NormalizeName(TypeName, False, True);
  end;
  Restrictions := 'DATATYPE_SCHEMA=' + Schema + #13#10'DATATYPE_NAME=' + Name +
    #13#10'DATATYPE_TYPE=c';
  Result := InternalDescribe(Connection, Restrictions);
end;

function TPgRowType.Describe(Connection: TPgSQLConnection; TypeOID: integer): boolean;
var
  Restrictions: _string;
begin
  Restrictions := 'DATATYPE_OID=' + IntToStr(TypeOID) + #13#10'DATATYPE_TYPE=c';
  Result := InternalDescribe(Connection, Restrictions);
end;

function TPgRowType.InternalDescribe(Connection: TPgSQLConnection; const Restrictions: _string): boolean;
var
  Md: TPgSQLMetaData;
  Restr: _TStringList;
  Data: TData;
  RecBuf: IntPtr;
  i: integer;
  v: variant;
  SchemaName, TypeName: _string;
begin
  Result := True;

  Md := TPgSQLMetaData.Create;
  try
    Restr := _TStringList.Create;
    try
      Restr.Text := Restrictions;
      Data := Md.GetMetaData(Connection, Connection.GetInternalTransaction, 'DataTypes', Restr);
    finally
      Restr.Free;
    end;
    if Data.RecordCount = 0 then begin
      Result := False;
      Exit;
    end;
    Data.AllocRecBuf(RecBuf);
    try
      Data.GetNextRecord(RecBuf);

      i := Data.FieldByName('DATATYPE_OID').FieldNo;
      Data.GetFieldAsVariant(i, RecBuf, v);
      FTypeOID := v;

      i := Data.FieldByName('TABLE_OID').FieldNo;
      Data.GetFieldAsVariant(i, RecBuf, v);
      FTableOID := v;

      i := Data.FieldByName('DATATYPE_SCHEMA').FieldNo;
      Data.GetFieldAsVariant(i, RecBuf, v);
      SchemaName := _VarToStr(v);

      i := Data.FieldByName('DATATYPE_NAME').FieldNo;
      Data.GetFieldAsVariant(i, RecBuf, v);
      TypeName := _VarToStr(v);

      FName := PgSQLInfo.NormalizeName(SchemaName) + '.' +
        PgSQLInfo.NormalizeName(TypeName);
    finally
      Data.FreeRecBuf(RecBuf);
    end;
  finally
    Md.Free;
  end;
  DescribeAttributes(Connection);
end;

procedure TPgRowType.DescribeAttributes(Connection: TPgSQLConnection);

  procedure DescribeAttribute(const Name: _string; TypeOID, TypeModifier: integer);
  var
    Attribute: TPgAttribute;
    DataType, SubDataType: word;
    Length, Scale, Size: integer;
    Fixed: boolean;
    ObjectType: TObjectType;
  begin
    Attribute := TPgAttribute.Create;
    FAttributes.Add(Attribute);
    Attribute.Owner := Self;
    Attribute.AttributeNo := FAttributes.Count;
    Attribute.Name := Name;
    Attribute.TypeOID := TypeOID;

    Connection.GetTypes.DetectDataType(TypeOID, TypeModifier,
      DataType, SubDataType, Length, Scale, Size, Fixed, ObjectType,
      True, False, True, False, False, False, False, False);

    Attribute.DataType := DataType;
    Attribute.SubDataType := SubDataType;
    Attribute.Size := Size;
    Attribute.Length := Length;
    Attribute.Scale := Scale;
    Attribute.Fixed := Fixed;
    Attribute.ObjectType := ObjectType;

    Attribute.Offset := FSize;
    Inc(FSize, Size);
  end;

var
  Md: TPgSQLMetaData;
  Restr: _TStringList;
  Data: TData;
  RecBuf: IntPtr;
  ColNameNo, ColTypeNo, TypeModNo: integer;
  v: variant;
  ColName: _string;
  ColType, TypeMod: integer;
begin
  ClearAttributes;
  FSize := 0;

  Md := TPgSQLMetaData.Create;
  try
    Restr := _TStringList.Create;
    try
      Restr.Text := 'TABLE_OID=' + IntToStr(FTableOID);
      Data := Md.GetMetaData(Connection, Connection.GetInternalTransaction, 'Columns', Restr);
    finally
      Restr.Free;
    end;
    if Data.RecordCount = 0 then
      Exit;
    ColNameNo := Data.FieldByName('COLUMN_NAME').FieldNo;
    ColTypeNo := Data.FieldByName('DATA_TYPE').FieldNo;
    TypeModNo := Data.FieldByName('TYPE_MODIFIER').FieldNo;
    Data.AllocRecBuf(RecBuf);
    try
      Data.GetNextRecord(RecBuf);
      while not Data.Eof do begin
        Data.GetFieldAsVariant(ColNameNo, RecBuf, v);
        ColName := _VarToStr(v);
        Data.GetFieldAsVariant(ColTypeNo, RecBuf, v);
        ColType := v;
        Data.GetFieldAsVariant(TypeModNo, RecBuf, v);
        TypeMod := v;
        DescribeAttribute(ColName, ColType, TypeMod);
        Data.GetNextRecord(RecBuf);
      end;
    finally
      Data.FreeRecBuf(RecBuf);
    end;
  finally
    Md.Free;
  end;

  FDataType := dtObject;
  FBufferSize := FSize + AttributeCount; // for indicators
end;

{ TPgRow }

constructor TPgRow.Create(RowType: TPgRowType = nil);
begin
  inherited Create;

  SetRowType(RowType);
end;

destructor TPgRow.Destroy;
begin
  FreeBuffer;

  inherited;
end;

procedure TPgRow.Assign(Source: TPgRow);
var
  Buffer, Src, Dest, SrcPtr, DestPtr: IntPtr;
  i, Size: integer;
  Attr: TAttribute;
begin
  Assert(Source.ObjectType = ObjectType);
  if Source.FBuffer = nil then begin
    FreeBuffer;
    exit;
  end;

  Buffer := GetBuffer;
  CopyBuffer(PtrOffset(Source.FBuffer, ObjectType.Size), 
             PtrOffset(FBuffer, ObjectType.Size), ObjectType.AttributeCount);

  for i := 0 to ObjectType.AttributeCount - 1 do begin
    Attr := ObjectType.Attributes[i];
    Src := PtrOffset(Source.FBuffer, Attr.Offset);
    SrcPtr := Marshal.ReadIntPtr(Src);
    Dest := PtrOffset(Buffer, Attr.Offset);
    DestPtr := Marshal.ReadIntPtr(Dest);
    case Attr.DataType of
      dtExtString: begin
        if DestPtr <> nil then
          Marshal.FreeHGlobal(DestPtr);
        if SrcPtr <> nil then begin
          Size := StrLen(PAChar(SrcPtr)) + 1;
          DestPtr := Marshal.AllocHGlobal(Size);
          CopyBuffer(SrcPtr, DestPtr, Size);
        end
        else
          DestPtr := nil;
        Marshal.WriteIntPtr(Dest, DestPtr);
      end;
      dtExtWideString: begin
        if DestPtr <> nil then
          Marshal.FreeHGlobal(DestPtr);
        if SrcPtr <> nil then begin
          Size := (StrLenW(SrcPtr) + 1) * 2;
          DestPtr := Marshal.AllocHGlobal(Size);
          CopyBuffer(SrcPtr, DestPtr, Size);
        end
        else
          DestPtr := nil;
        Marshal.WriteIntPtr(Dest, DestPtr);
      end;
      dtPgDate, dtPgTime, dtPgTimeStamp:
        TCustomPgTimeStamp(GetGCHandleTarget(DestPtr)).Assign(TCustomPgTimeStamp(GetGCHandleTarget(SrcPtr)));
      dtPgInterval:
        TPgInterval(GetGCHandleTarget(DestPtr)).Assign(TPgInterval(GetGCHandleTarget(SrcPtr)));
      dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
        TPgGeometric(GetGCHandleTarget(DestPtr)).Assign(TPgGeometric(GetGCHandleTarget(SrcPtr)));
      dtObject:
        TPgRow(GetGCHandleTarget(DestPtr)).Assign(TPgRow(GetGCHandleTarget(SrcPtr)));
    else
      CopyBuffer(Src, Dest, Attr.Size);
    end;
  end;
end;

procedure TPgRow.AllocBuffer;
begin
  FBuffer := Marshal.AllocHGlobal(RowType.BufferSize);
  FillChar(FBuffer, ObjectType.Size, 0);
  FillChar(PtrOffset(FBuffer, ObjectType.Size), ObjectType.AttributeCount, Byte(-1));
end;

procedure TPgRow.FreeBuffer;
begin
  if FBuffer <> nil then begin
    FreeComplexFields;
    Marshal.FreeHGlobal(FBuffer);
    FBuffer := nil;
  end;
end;

function TPgRow.GetBuffer: IntPtr;
begin
  if FBuffer = nil then begin
    AllocBuffer;
    CreateComplexFields;
  end;

  Result := FBuffer;
end;

procedure TPgRow.CreateComplexFields;
var
  i: integer;
  Attr: TAttribute;
begin
  for i := 0 to ObjectType.AttributeCount - 1 do begin
    Attr := ObjectType.Attributes[i];
    TPgSQLRecordSet.InternalCreateComplexField(FBuffer, nil, True, Attr.Offset,
      Attr.DataType, Attr.ObjectType, nil, True);
  end;
end;

procedure TPgRow.FreeComplexFields;
var
  i: integer;
  Attr: TAttribute;
  StrPtr: IntPtr;
  Obj: TSharedObject;
begin
  for i := 0 to ObjectType.AttributeCount - 1 do begin
    Attr := ObjectType.Attributes[i];
    case Attr.DataType of
      dtExtString, dtExtWideString: begin
        StrPtr := Marshal.ReadIntPtr(FBuffer, Attr.Offset);
        if StrPtr <> nil then
          Marshal.FreeHGlobal(StrPtr);
      end;
      dtBlob, dtMemo, dtWideMemo,
      dtPgLargeObject,
      dtPgDate, dtPgTime, dtPgTimeStamp, dtPgInterval,
      dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle,
      dtCursor, dtObject: begin
        Obj := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(FBuffer, Attr.Offset)));
        Obj.Free;
      end;
    end;
  end;
end;

function TPgRow.GetRowType: TPgRowType;
begin
  Result := TPgRowType(ObjectType);
end;

procedure TPgRow.SetRowType(Value: TPgRowType);
begin
  if Value <> ObjectType then begin
    FreeBuffer;

    inherited SetObjectType(Value);
  end;
end;

procedure TPgRow.CheckRowType;
begin
  if ObjectType = nil then
    raise Exception.Create(SRowTypeNotSet);
end;

procedure TPgRow.GetAttributeValue(const Name: _string; var AttrBuf: IntPtr;
  var IsBlank, NativeBuffer: boolean);
var
  Attr: TAttribute;
  Ind: byte;
begin
  NativeBuffer := True;

  if FBuffer = nil then begin
    IsBlank := True;
    exit;
  end;

  CheckRowType;

  Attr := ObjectType.AttributeByName(Name);
  Ind := Marshal.ReadByte(FBuffer, ObjectType.Size + Attr.AttributeNo - 1);
  IsBlank := Ind = Byte(-1);
  AttrBuf := PtrOffset(FBuffer, Attr.Offset);
end;

procedure TPgRow.SetAttributeValue(const Name: _string; Source: IntPtr);
var
  Buffer, FieldPtr, StrPtr: IntPtr;
  Attr: TAttribute;
  Size: integer;
  DateTime: TDateTime;
  Ts: TTimeStamp;
begin
  CheckRowType;

  Buffer := GetBuffer;
  Attr := ObjectType.AttributeByName(Name);
  FieldPtr := PtrOffset(Buffer, Attr.Offset);
  case Attr.DataType of
    dtExtString: begin
      StrPtr := Marshal.ReadIntPtr(FieldPtr);
      if StrPtr <> nil then
        Marshal.FreeHGlobal(StrPtr);
      if Source <> nil then begin
        Size := Min(StrLen(PAChar(Source)), Attr.Length) + 1;
        StrPtr := Marshal.AllocHGlobal(Size);
        CopyBuffer(Source, StrPtr, Size);
      end
      else
        StrPtr := nil;
      Marshal.WriteIntPtr(FieldPtr, StrPtr);
    end;
    dtExtWideString: begin
      StrPtr := Marshal.ReadIntPtr(FieldPtr);
      if StrPtr <> nil then
        Marshal.FreeHGlobal(StrPtr);
      if Source <> nil then begin
        Size := (Min(StrLenW(Source), Attr.Length) + 1) * 2;
        StrPtr := Marshal.AllocHGlobal(Size);
        CopyBuffer(Source, StrPtr, Size);
      end
      else
        StrPtr := nil;
      Marshal.WriteIntPtr(FieldPtr, StrPtr);
    end;
    dtDateTime: begin
    {$IFDEF CLR}
      DateTime := MemUtils.TimeStampToDateTime(MSecsToTimeStamp(Trunc(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(Source)))));
    {$ELSE}{$IFDEF FPC}
      DateTime := MemUtils.TimeStampToDateTime(MSecsToTimeStamp(Trunc(Double(Source^))));
    {$ELSE}
      DateTime := MemUtils.TimeStampToDateTime(MSecsToTimeStamp(TDateTime(Source^)));
    {$ENDIF}{$ENDIF}
      Marshal.WriteInt64(FieldPtr, BitConverter.DoubleToInt64Bits(DateTime));
    end;
    dtDate: begin
      Ts.Date := Marshal.ReadInt32(Source);
      Ts.Time := 0;
      DateTime := MemUtils.TimeStampToDateTime(Ts);
      Marshal.WriteInt64(FieldPtr, BitConverter.DoubleToInt64Bits(DateTime));
    end;
    dtTime: begin
    {$IFNDEF FPC}
      Ts.Date := DateDelta;
      Ts.Time := Marshal.ReadInt32(Source);
      DateTime := MemUtils.TimeStampToDateTime(Ts);
    {$ELSE}
      DateTime := TDateTime(Source^);
    {$ENDIF}
      Marshal.WriteInt64(FieldPtr, BitConverter.DoubleToInt64Bits(DateTime));
    end;
  else
    CopyBuffer(Source, FieldPtr, Attr.Size);
  end;
  Marshal.WriteByte(FBuffer, ObjectType.Size + Attr.AttributeNo - 1, 0);
end;

function TPgRow.GetAttrIsNull(const Name: _string): boolean;
var
  Value: IntPtr;
  NativeBuffer: boolean;
begin
  GetAttributeValue(Name, Value, Result, NativeBuffer);
end;

procedure TPgRow.SetAttrIsNull(const Name: _string; Value: boolean);
var
  Ind: byte;
  Attr: TAttribute;
begin
  CheckRowType;

  Attr := ObjectType.AttributeByName(Name);
  if Value then
    Ind := Byte(-1)
  else
    Ind := 0;
  Marshal.WriteByte(FBuffer, ObjectType.Size + Attr.AttributeNo - 1, Ind);
end;

function TPgRow.GetAttrValue(const Name: _string): variant;
begin
  Result := AttrValueEx(Name, False);
end;

function TPgRow.AttrValueEx(const Name: _string; AllowObject: boolean): variant;
var
  IsBlank, NativeBuffer: boolean;
  Attr: TAttribute;
  AttrBuf: IntPtr;
{$IFDEF CLR}
  Bytes: TBytes;
{$ENDIF}
  Obj: TSharedObject;
begin
  GetAttributeValue(Name, AttrBuf, IsBlank, NativeBuffer);

  if IsBlank then begin
    Result := Null;
    exit;
  end;

  Attr := ObjectType.AttributeByName(Name);
  case Attr.DataType of
    dtString:
      Result := Marshal.PtrToStringAnsi(AttrBuf);
    dtWideString:
      Result := Marshal.PtrToStringUni(AttrBuf);
    dtExtString:
      Result := Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(AttrBuf));
    dtExtWideString:
      Result := Marshal.PtrToStringUni(Marshal.ReadIntPtr(AttrBuf));
    dtSmallInt:
      Result := Marshal.ReadInt16(AttrBuf);
    dtInteger:
      Result := Marshal.ReadInt32(AttrBuf);
    dtLargeInt:
      Result := Marshal.ReadInt64(AttrBuf);
    dtBoolean:
      Result := Boolean(Marshal.ReadByte(AttrBuf));
    dtFloat:
      Result := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(AttrBuf));
    dtCurrency, dtBCD:
    {$IFNDEF CLR}
      Result := PCurrency(AttrBuf)^;
    {$ELSE}
      Result := Decimal.FromOACurrency(Marshal.ReadInt64(AttrBuf));
    {$ENDIF}
  {$IFNDEF FPC}
    dtFMTBCD: begin
    {$IFNDEF CLR}
      VarFMTBcdCreate(Result, PBcd(AttrBuf)^);
    {$ELSE}
      SetLength(Bytes, SizeOfTBcd);
      Marshal.Copy(AttrBuf, Bytes, 0, SizeOfTBcd);
      VarFMTBcdCreate(Result, TBcd.FromBytes(Bytes));
    {$ENDIF}
    end;
  {$ENDIF}
    dtDate, dtTime, dtDateTime:
      Result := TDateTime(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(AttrBuf)));
  else
    if TPgSQLRecordSet.IsComplexFieldType(Attr.DataType) then begin
      Obj := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(AttrBuf)));
      if AllowObject then begin
      {$IFDEF CLR}
        Result := Variant(Obj);
      {$ELSE}
        Result := Unassigned;
        TVarData(Result).VType := varByRef{$IFDEF FPC} or varVariant{$ENDIF};
        TVarData(Result).VPointer := Obj;
      {$ENDIF}
      end
      else begin
        case Attr.DataType of
          dtBlob, dtMemo, dtWideMemo, dtPgLargeObject:
            if TBlob(Obj).IsUnicode then
              Result := TBlob(Obj).AsWideString
            else
              Result := TBlob(Obj).AsAnsiString;
          dtPgDate, dtPgTime, dtPgTimeStamp:
            Result := TCustomPgTimeStamp(Obj).AsString;
          dtPgInterval:
            Result := TPgInterval(Obj).AsString;
          dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
            Result := TPgGeometric(Obj).AsString;
          dtObject:
            Result := TPgRow(Obj).AsString;
          dtCursor:
            Result := TPgRefCursor(Obj).CursorName;
        else
          raise EConvertError.Create(SCannotConvertType);
          Result := Null;
        end;
      end;
    end
    else begin
      raise EConvertError.Create(SCannotConvertType);
      Result := Null; // anti warning
    end;
  end;
end;

procedure TPgRow.SetAttrValue(const Name: _string; const Value: variant);
var
  Attr: TAttribute;
  Buffer, AttrBuf: IntPtr;
{$IFNDEF FPC}
  Bcd: TBcd;
{$ENDIF}
  i64: int64;
  Obj: TSharedObject;
begin
  CheckRowType;

  if VarIsNull(Value) or VarIsEmpty(Value) then begin
    AttrIsNull[Name] := True;
    exit;
  end;

  Attr := ObjectType.AttributeByName(Name);

  if TPgSQLRecordSet.IsComplexFieldType(Attr.DataType) then begin
    Buffer := GetBuffer;
    AttrBuf := PtrOffset(Buffer, Attr.Offset);
    Obj := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(AttrBuf)));
    case Attr.DataType of
      dtBlob, dtMemo, dtWideMemo, dtPgLargeObject:
        if TBlob(Obj).IsUnicode then
          TBlob(Obj).AsWideString := Value
        else
          TBlob(Obj).AsAnsiString := AnsiString(Value);
      dtPgDate, dtPgTime, dtPgTimeStamp:
        TCustomPgTimeStamp(Obj).AsString := Value;
      dtPgInterval:
        TPgInterval(Obj).AsString := Value;
      dtPgPoint, dtPgLSeg, dtPgBox, dtPgPath, dtPgPolygon, dtPgCircle:
        TPgGeometric(Obj).AsString := Value;
      dtObject:
        TPgRow(Obj).AsString := Value;
      //dtCursor:
    else
      raise EConvertError.Create(SCannotConvertType);
    end;
    AttrIsNull[Name] := False;
  end
  else begin
    if not (Attr.DataType in [dtString, dtExtString, dtWideString, dtExtWideString]) then
      AttrBuf := Marshal.AllocHGlobal(Attr.Size)
    else
      AttrBuf := nil;

    try
      case Attr.DataType of
        dtString, dtExtString:
          SetAttributeValue(Name, Marshal.StringToHGlobalAnsi(AnsiString(Value)));
        dtWideString, dtExtWideString:
          SetAttributeValue(Name, Marshal.StringToHGlobalUni(WideString(Value)));
        dtSmallInt:
          Marshal.WriteInt16(AttrBuf, Smallint(Value));
        dtInteger:
          Marshal.WriteInt32(AttrBuf, Value);
        dtLargeInt: begin
          i64 := Value;
          Marshal.WriteInt64(AttrBuf, i64);
        end;
        dtBoolean:
          Marshal.WriteByte(AttrBuf, Value);
        dtFloat:
          Marshal.WriteInt64(AttrBuf, BitConverter.DoubleToInt64Bits(Value));
        dtCurrency, dtBCD:
        {$IFNDEF CLR}
          PCurrency(AttrBuf)^ := Value;
        {$ELSE}
          Marshal.WriteInt64(AttrBuf, Decimal.ToOACurrency(Value));
        {$ENDIF}
      {$IFNDEF FPC}
        dtFMTBCD: begin
          case VarType(Value) of
            varSmallint, varInteger, varByte, varWord, varShortInt:
              Bcd := IntegerToBcd(Value);
            varLongWord, varInt64: begin
              i64 := Value;
              Bcd := StrToBcd(IntToStr(i64));
            end;
            varSingle, varDouble, varCurrency:
              Bcd := DoubleToBcd(Value);
          else
            if VarIsStr(Value) then
              Bcd := StrToBcd(Value)
            else
            if VarType(Value) = VarFMTBcd then
              Bcd := VarToBcd(Value)
            else
              raise EConvertError.Create(SCannotConvertType);
          end;
        {$IFNDEF CLR}
          PBcd(AttrBuf)^ := Bcd;
        {$ELSE}
          Marshal.Copy(TBcd.ToBytes(Bcd), 0, AttrBuf, SizeOfTBcd);
        {$ENDIF}
        end;
      {$ENDIF}
        dtDate, dtTime, dtDateTime:
          // TODO: add support for date strings in Pg format 
          Marshal.WriteInt64(AttrBuf, BitConverter.DoubleToInt64Bits(TDateTime(Value)));
      else
        raise EConvertError.Create(SCannotConvertType);
      end;

      if AttrBuf <> nil then
        SetAttributeValue(Name, AttrBuf);
    finally
      if AttrBuf <> nil then
        Marshal.FreeHGlobal(AttrBuf);
    end;
  end;
end;

function TPgRow.GetAsObject(const Name: _string; DataType: word): TSharedObject;
var
  Attr: TAttribute;
  Buffer, AttrBuf: IntPtr;
begin
  CheckRowType;

  Attr := ObjectType.AttributeByName(Name);
  if Attr.DataType <> DataType then
    raise EConvertError.Create(SCannotConvertType);

  Buffer := GetBuffer;
  AttrBuf := PtrOffset(Buffer, Attr.Offset);
  Result := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(AttrBuf)));
end;

procedure TPgRow.SetAsObject(const Name: _string; DataType: word; Value: TSharedObject);
var
  Attr: TAttribute;
  Buffer, AttrBuf: IntPtr;
  Obj: TSharedObject;
begin
  CheckRowType;

  Attr := ObjectType.AttributeByName(Name);
  if Attr.DataType <> DataType then
    raise EConvertError.Create(SCannotConvertType);

  Buffer := GetBuffer;
  AttrBuf := PtrOffset(Buffer, Attr.Offset);
  Obj := TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(AttrBuf)));
  if Value <> Obj then begin
    Obj.Free;
    Marshal.WriteIntPtr(AttrBuf, Value.GCHandle);
    Value.AddRef;
  end;
  AttrIsNull[Name] := False;
end;

function TPgRow.GetAsPgDate(const Name: _string): TPgDate;
begin
  Result := GetAsObject(Name, dtPgDate) as TPgDate;
end;

procedure TPgRow.SetAsPgDate(const Name: _string; Value: TPgDate);
begin
  SetAsObject(Name, dtPgDate, Value);
end;

function TPgRow.GetAsPgTime(const Name: _string): TPgTime;
begin
  Result := GetAsObject(Name, dtPgTime) as TPgTime;
end;

procedure TPgRow.SetAsPgTime(const Name: _string; Value: TPgTime);
begin
  SetAsObject(Name, dtPgTime, Value);
end;

function TPgRow.GetAsPgTimeStamp(const Name: _string): TPgTimeStamp;
begin
  Result := GetAsObject(Name, dtPgTimeStamp) as TPgTimeStamp;
end;

procedure TPgRow.SetAsPgTimeStamp(const Name: _string; Value: TPgTimeStamp);
begin
  SetAsObject(Name, dtPgTimeStamp, Value);
end;

function TPgRow.GetAsPgInterval(const Name: _string): TPgInterval;
begin
  Result := GetAsObject(Name, dtPgInterval) as TPgInterval;
end;

procedure TPgRow.SetAsPgInterval(const Name: _string; Value: TPgInterval);
begin
  SetAsObject(Name, dtPgInterval, Value);
end;

function TPgRow.GetAsPgLargeObject(const Name: _string): TPgSQLLargeObject;
begin
  Result := GetAsObject(Name, dtPgLargeObject) as TPgSQLLargeObject;
end;

procedure TPgRow.SetAsPgLargeObject(const Name: _string; Value: TPgSQLLargeObject);
begin
  SetAsObject(Name, dtPgLargeObject, Value);
end;

function TPgRow.GetAsPgPoint(const Name: _string): TPgPoint;
begin
  Result := GetAsObject(Name, dtPgPoint) as TPgPoint;
end;

procedure TPgRow.SetAsPgPoint(const Name: _string; Value: TPgPoint);
begin
  SetAsObject(Name, dtPgPoint, Value);
end;

function TPgRow.GetAsPgLSeg(const Name: _string): TPgLSeg;
begin
  Result := GetAsObject(Name, dtPgLSeg) as TPgLSeg;
end;

procedure TPgRow.SetAsPgLSeg(const Name: _string; Value: TPgLSeg);
begin
  SetAsObject(Name, dtPgLSeg, Value);
end;

function TPgRow.GetAsPgBox(const Name: _string): TPgBox;
begin
  Result := GetAsObject(Name, dtPgBox) as TPgBox;
end;

procedure TPgRow.SetAsPgBox(const Name: _string; Value: TPgBox);
begin
  SetAsObject(Name, dtPgBox, Value);
end;

function TPgRow.GetAsPgPath(const Name: _string): TPgPath;
begin
  Result := GetAsObject(Name, dtPgPath) as TPgPath;
end;

procedure TPgRow.SetAsPgPath(const Name: _string; Value: TPgPath);
begin
  SetAsObject(Name, dtPgPath, Value);
end;

function TPgRow.GetAsPgPolygon(const Name: _string): TPgPolygon;
begin
  Result := GetAsObject(Name, dtPgPolygon) as TPgPolygon;
end;

procedure TPgRow.SetAsPgPolygon(const Name: _string; Value: TPgPolygon);
begin
  SetAsObject(Name, dtPgPolygon, Value);
end;

function TPgRow.GetAsPgCircle(const Name: _string): TPgCircle;
begin
  Result := GetAsObject(Name, dtPgCircle) as TPgCircle;
end;

procedure TPgRow.SetAsPgCircle(const Name: _string; Value: TPgCircle);
begin
  SetAsObject(Name, dtPgCircle, Value);
end;

function TPgRow.GetAsPgCursor(const Name: _string): TPgCursor;
begin
  Result := GetAsObject(Name, dtCursor) as TPgCursor;
end;

procedure TPgRow.SetAsPgCursor(const Name: _string; Value: TPgCursor);
begin
  SetAsObject(Name, dtCursor, Value);
end;

function TPgRow.GetAsPgRow(const Name: _string): TPgRow;
begin
  Result := GetAsObject(Name, dtObject) as TPgRow;
end;

procedure TPgRow.SetAsPgRow(const Name: _string; Value: TPgRow);
begin
  SetAsObject(Name, dtObject, Value);
end;

function TPgRow.GetAsString: string;

  function QuoteNeeded(const Value: string): boolean;
  var
    i: integer;
  begin
    if Value = '' then
      Result := True
    else begin
      Result := False;
      for i := 1 to Length(Value) do
        case Value[i] of
          '(', ')', ',', '"', '\', ' ': begin
            Result := True;
            exit;
          end;
        end;
    end;
  end;

  procedure EscapeValue(const Value: string; sb: StringBuilder);
  var
    qn: boolean;
    i, Start: integer;
    c: char;
  begin
    qn := QuoteNeeded(Value);
    if qn then
      sb.Append('"');
    Start := 1;
    for i := 1 to Length(Value) do begin
      c := Value[i];
      if (c = '\') or (c = '"') then begin
        sb.Append(Value, Start - 1, i - Start);
        sb.Append('\');
        sb.Append(c);
        Start := i + 1;
      end;
    end;
    sb.Append(Value, Start - 1, Length(Value) - Start + 1);
    if qn then
      sb.Append('"');
  end;

var
  sb: StringBuilder;
  i: integer;
  Attr: TAttribute;
  Value: variant;
  sa: AnsiString;
  s: string;
begin
  if ObjectType = nil then begin
    Result := '';
    exit;
  end;

  sb := StringBuilder.Create(1024);
  try
    sb.Append('(');
    for i := 0 to ObjectType.AttributeCount - 1 do begin
      if i > 0 then
        sb.Append(',');
      Attr := ObjectType.Attributes[i];
      Value := AttrValue[Attr.Name];
      if not VarIsNull(Value) then begin
        sa := TPgTextConverter.ValueToText(Value, Attr.DataType, True, False);
        s := UTF8Decode(sa);
        EscapeValue(s, sb);
      end;
    end;
    sb.Append(')');
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TPgRow.SetAsString(const Value: string);
var
  p: integer;

  procedure InvalidString;
  begin
    raise EConvertError.Create('Invalid ROW string');
  end;

  procedure CheckLength;
  begin
    if p > Length(Value) then
      InvalidString;
  end;

var
  Start, ValStart, i, j: integer;
  Attr: TAttribute;
  InQuote: boolean;
  c: char;
  s: string;
begin
  if (Value = '') and (ObjectType = nil) then
    exit;

  CheckRowType;

  if ObjectType.AttributeCount = 0 then
    exit;

  p := 1;
  ScanBlanks(Value, p);
  CheckLength;

  if Value[p] <> '(' then
    InvalidString;
  Inc(p);
  CheckLength;

  i := 0;
  Attr := nil;
  InQuote := False;
  ValStart := p;
  Start := p;
  repeat
    if Attr = nil then begin
      if i = ObjectType.AttributeCount then
        InvalidString;
      Attr := ObjectType.Attributes[i];
      InQuote := False;
      ValStart := p;
      Start := p;
    end;

    c := Value[p];
    case c of
      ',', ')':
        if not InQuote then begin
          if p = ValStart then
            AttrIsNull[Attr.Name] := True
          else begin
            s := s + Copy(Value, Start, p - Start);
            AttrValue[Attr.Name] := s;
          end;

          Inc(i);
          Attr := nil;
          s := '';
          if c = ')' then begin
            Inc(p);
            break;
          end;
        end;
      '\': begin
        s := s + Copy(Value, Start, p - Start);
        Inc(p);
        Start := p;
      end;
      '"': begin
        if p >= Length(Value) then
          InvalidString;
        if InQuote then begin
          if Value[p + 1] <> '"' then begin
            s := s + Copy(Value, Start, p - Start);
            Start := p + 1;
            InQuote := False;
          end
          else begin
            s := s + Copy(Value, Start, p - Start) + '"';
            Inc(p);
            Start := p + 1;
          end;
        end
        else begin
          s := s + Copy(Value, Start, p - Start);
          InQuote := True;
          Start := p + 1;
        end;
      end;
    end;

    Inc(p);
    CheckLength;
  until False;

  ScanBlanks(Value, p);
  if p <= Length(Value) then
    InvalidString;

  for j := i to ObjectType.AttributeCount - 1 do begin
    Attr := ObjectType.Attributes[j];
    AttrIsNull[Attr.Name] := True;
  end;
end;

{$ENDIF NDEF LITE}

{ TPgObjectsUtils }

class function TPgObjectsUtils.GetLargeObjectOIDChanged(Obj: TPgSQLLargeObject): boolean;
begin
  Assert(Obj <> nil);
  Result := Obj.FOID <> Obj.FOldOID;
end;

class procedure TPgObjectsUtils.LargeObjectResetOldOID(Obj: TPgSQLLargeObject);
begin
  Obj.ResetOldOID;
end;

initialization
{$IFDEF WIN32_64}
  GetTimeZoneInformation(PgTimeZoneInformation);
{$ENDIF}

end.
