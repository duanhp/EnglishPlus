
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I SQLiteDac.inc}
unit LiteCollationUni;
{$ENDIF}

interface

uses
  SysUtils, Classes, DB, Variants,
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  MemUtils, CRAccess,
{$IFNDEF UNIDACPRO}
  LiteCall, LiteError;
{$ELSE}
  LiteCallUni, LiteErrorUni;
{$ENDIF}

type
  TLiteCollation = function(Str1, Str2: _string): Integer;

  TLiteAnsiCollation = function(Str1, Str2: AnsiString): Integer;

  TLiteWideCollation = function(Str1, Str2: WideString): Integer;

  TCustomLiteCollationDesc = class
  private
    FConnection: TCRConnection;
    FName: _string;
    FTextRepresentation: Integer;
  protected
    procedure RegisterCollation;
    procedure UnregisterCollation;

    function GetAnsiStr(StrSize: Integer; const pStr: IntPtr): AnsiString;
    function GetWideStr(StrSize: Integer; const pStr: IntPtr): WideString;

    function DoCollate(StrSize1: Integer; const pStr1: IntPtr; StrSize2: Integer; const pStr2: IntPtr): Integer; virtual; abstract;
  public
    constructor Create(Connection: TCRConnection; Name: _string); overload;
    destructor Destroy; override;

    property Name: _string read FName;
    property TextRepresentation: Integer read FTextRepresentation;
  end;

  TLiteCollationDesc = class(TCustomLiteCollationDesc)
  private
    FLiteCollation: TLiteCollation;
  protected
    function DoCollate(StrSize1: Integer; const pStr1: IntPtr; StrSize2: Integer; const pStr2: IntPtr): Integer; override;
  public
    constructor Create(Connection: TCRConnection; Name: _string; LiteCollation: TLiteCollation);

    property LiteCollation: TLiteCollation read FLiteCollation write FLiteCollation;
  end;

  TLiteAnsiCollationDesc = class(TCustomLiteCollationDesc)
  private
    FLiteAnsiCollation: TLiteAnsiCollation;
  protected
    function DoCollate(StrSize1: Integer; const pStr1: IntPtr; StrSize2: Integer; const pStr2: IntPtr): Integer; override;
  public
    constructor Create(Connection: TCRConnection; Name: _string; LiteAnsiCollation: TLiteAnsiCollation);

    property LiteAnsiCollation: TLiteAnsiCollation read FLiteAnsiCollation write FLiteAnsiCollation;
  end;

  TLiteWideCollationDesc = class(TCustomLiteCollationDesc)
  private
    FLiteWideCollation: TLiteWideCollation;
  protected
    function DoCollate(StrSize1: Integer; const pStr1: IntPtr; StrSize2: Integer; const pStr2: IntPtr): Integer; override;
  public
    constructor Create(Connection: TCRConnection; Name: _string; LiteWideCollation: TLiteWideCollation);

    property LiteWideCollation: TLiteWideCollation read FLiteWideCollation write FLiteWideCollation;
  end;

  TSQLiteCollationManager = class
  private
    FConnection: TCRConnection;
    FCollationList: TList;
  protected
    procedure InternalAddCollation(LiteCollationDesc: TCustomLiteCollationDesc);
    procedure InternalRemoveCollation(LiteCollationDesc: TCustomLiteCollationDesc);

    function FindCollation(Name: _string): TCustomLiteCollationDesc;
    procedure UnRegistrAllCollations;
  public
    constructor Create(Connection: TCRConnection);
    destructor Destroy; override;

    procedure RegisterCollation(Name: _string; LiteCollation: TLiteCollation);
    procedure UnRegisterCollation(Name: _string);

    procedure RegisterAnsiCollation(Name: _string; LiteAnsiCollation: TLiteAnsiCollation);
    procedure UnRegisterAnsiCollation(Name: _string);

    procedure RegisterWideCollation(Name: _string; LiteWideCollation: TLiteWideCollation);
    procedure UnRegisterWideCollation(Name: _string);

    procedure RegisterDefaultCollations;
    procedure UnRegisterDefaultCollations;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  LiteClasses;
{$ELSE}
  LiteClassesUni;
{$ENDIF}

var
  CallBackLiteCollationCode : array[0..39] of byte =
   ($55,
    $8B, $EC,
    $51,
    $8B, $45, $18, $50,
    $8B, $45, $14, $50,
    $8B, $45, $10, $50,
    $8B, $45, $C, $50,
    $8B, $45, $8, $50,

    $B8, $00, $00, $00, $00,
    $FF, $D0,

    $89, $45, $FC,
    $8B, $45, $FC,

    $59,
    $5D,
    $C3);

  CallBackLiteCollationPtr: IntPtr;
{$IFDEF CLR}
  HCallBackLiteCollation: GCHandle;
  CallBackLiteCollationRec: packed record
    Ptr: TCallBackLiteCollation;
  end;
  CallbackRecPtr: IntPtr;
{$ENDIF}

{ Function }

function CallBackLiteCollation(pUserData: IntPtr;
                               StrSize1: Integer; const pStr1: IntPtr;
                               StrSize2: Integer; const pStr2: IntPtr
                              ): Integer; {$IFNDEF CLR}cdecl;{$ENDIF}
var
  collationDesc: TCustomLiteCollationDesc;
begin
{$IFNDEF CLR}
  collationDesc := TCustomLiteCollationDesc(pUserData);
{$ELSE}
  collationDesc := TCustomLiteCollationDesc(GCHandle(pUserData).Target);
{$ENDIF}
  Result := collationDesc.DoCollate(StrSize1, pStr1, StrSize2, pStr2);
end;

function DefaultUnicodeNoCase(WStr1, WStr2: WideString): Integer;
var
  upperStr1: WideString;
  upperStr2: WideString;
begin
  upperStr1 := WideUpperCase(WStr1);
  upperStr2 := WideUpperCase(WStr2);

  if upperStr1 > upperStr2 then
    Result := 1
  else if upperStr1 < upperStr2 then
    Result := -1
  else
    Result :=  0;
end;

{ TCustomLiteCollationDesc }

constructor TCustomLiteCollationDesc.Create(Connection: TCRConnection; Name: _string);
begin
  inherited Create;

  FConnection := Connection;
  FName := Name;
  
  if TSQLiteConnection(FConnection).IsUnicodeDataBase then
    FTextRepresentation := SQLITE_UTF16
  else
    FTextRepresentation := SQLITE_UTF8;
end;

destructor TCustomLiteCollationDesc.Destroy;
begin
  inherited;
end;

procedure TCustomLiteCollationDesc.RegisterCollation;
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
  sqlite3_create_collation(TSQLiteConnection(FConnection).SQLite, pName, FTextRepresentation, pSelf, CallBackLiteCollationPtr);
end;

procedure TCustomLiteCollationDesc.UnregisterCollation;
var
  pName: PAnsiChar;
begin
  if FConnection.GetConnected then begin
    pName := PAnsiChar(AnsiString(Name));
    sqlite3_create_collation(TSQLiteConnection(FConnection).SQLite, pName, FTextRepresentation, nil, nil);
  end;  
end;

function TCustomLiteCollationDesc.GetAnsiStr(StrSize: Integer; const pStr: IntPtr): AnsiString;
begin
  Result := Marshal.PtrToStringAnsi(pStr, StrSize);
end;

function TCustomLiteCollationDesc.GetWideStr(StrSize: Integer; const pStr: IntPtr): WideString;
begin
  Result := Marshal.PtrToStringUni(pStr, StrSize shr 1);
end;

{ TLiteCollationDesc }

constructor TLiteCollationDesc.Create(Connection: TCRConnection; Name: _string; LiteCollation: TLiteCollation);
begin
  inherited Create(Connection, Name);
  FLiteCollation := LiteCollation;
end;

function TLiteCollationDesc.DoCollate(StrSize1: Integer; const pStr1: IntPtr;
                                      StrSize2: Integer; const pStr2: IntPtr): Integer;
begin
  if TSQLiteConnection(FConnection).IsUnicodeDataBase then
    Result := FLiteCollation(GetWideStr(StrSize1, pStr1), GetWideStr(StrSize2, pStr2))
  else
    Result := FLiteCollation(GetAnsiStr(StrSize1, pStr1), GetAnsiStr(StrSize2, pStr2));
end;

{ TLiteAnsiCollationDesc }

constructor TLiteAnsiCollationDesc.Create(Connection: TCRConnection; Name: _string; LiteAnsiCollation: TLiteAnsiCollation);
begin
  inherited Create(Connection, Name);
  FLiteAnsiCollation := LiteAnsiCollation;
end;

function TLiteAnsiCollationDesc.DoCollate(StrSize1: Integer; const pStr1: IntPtr;
                                          StrSize2: Integer; const pStr2: IntPtr): Integer;
begin
  if TSQLiteConnection(FConnection).IsUnicodeDataBase then
    Result := FLiteAnsiCollation(GetWideStr(StrSize1, pStr1), GetWideStr(StrSize2, pStr2))
  else
    Result := FLiteAnsiCollation(GetAnsiStr(StrSize1, pStr1), GetAnsiStr(StrSize2, pStr2));
end;

{ TLiteWideCollationDesc }

constructor TLiteWideCollationDesc.Create(Connection: TCRConnection; Name: _string; LiteWideCollation: TLiteWideCollation);
begin
  inherited Create(Connection, Name);
  FLiteWideCollation := LiteWideCollation;
end;

function TLiteWideCollationDesc.DoCollate(StrSize1: Integer; const pStr1: IntPtr;
                                          StrSize2: Integer; const pStr2: IntPtr): Integer;
begin
  if TSQLiteConnection(FConnection).IsUnicodeDataBase then
    Result := FLiteWideCollation(GetWideStr(StrSize1, pStr1), GetWideStr(StrSize2, pStr2))
  else
    Result := FLiteWideCollation(GetAnsiStr(StrSize1, pStr1), GetAnsiStr(StrSize2, pStr2));
end;

{ TSQLiteCollationManager }

constructor TSQLiteCollationManager.Create(Connection: TCRConnection);
begin
  inherited Create;
  FConnection := Connection;
  FCollationList := TList.Create;
end;

destructor TSQLiteCollationManager.Destroy;
begin
  UnRegistrAllCollations;
  FCollationList.Free;
  inherited;
end;

procedure TSQLiteCollationManager.InternalAddCollation(LiteCollationDesc: TCustomLiteCollationDesc);
var
  ExistLiteCollationDesc: TCustomLiteCollationDesc;
begin
  ExistLiteCollationDesc := FindCollation(LiteCollationDesc.Name);
  if ExistLiteCollationDesc <> nil then
    InternalRemoveCollation(ExistLiteCollationDesc);

  LiteCollationDesc.RegisterCollation;
  FCollationList.Add(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.InternalRemoveCollation(LiteCollationDesc: TCustomLiteCollationDesc);
begin
  LiteCollationDesc.UnregisterCollation;
  FCollationList.Remove(LiteCollationDesc);
  LiteCollationDesc.Free;
end;

function TSQLiteCollationManager.FindCollation(Name: _string): TCustomLiteCollationDesc;
var
  i: integer;
begin
  for i := 0 to FCollationList.Count - 1 do
  begin
    Result := TCustomLiteCollationDesc(FCollationList[i]);
    if Result.Name = Name then
      exit;
  end;

  // if not found
  Result := nil;
end;

procedure TSQLiteCollationManager.UnRegistrAllCollations;
var
  i: integer;
begin
  // unregister collations
  for i := 0 to FCollationList.Count - 1 do begin
    TCustomLiteCollationDesc(FCollationList[i]).UnregisterCollation;
    TCustomLiteCollationDesc(FCollationList[i]).Free;
  end;
  FCollationList.Clear;
end;

procedure TSQLiteCollationManager.RegisterCollation(Name: _string; LiteCollation: TLiteCollation);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteCollationDesc.Create(FConnection, Name, LiteCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.UnRegisterCollation(Name: _string);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := FindCollation(Name);
  if LiteCollationDesc <> nil then
    InternalRemoveCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterAnsiCollation(Name: _string; LiteAnsiCollation: TLiteAnsiCollation);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteAnsiCollationDesc.Create(FConnection, Name, LiteAnsiCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.UnRegisterAnsiCollation(Name: _string);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := FindCollation(Name);
  if LiteCollationDesc <> nil then
    InternalRemoveCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterWideCollation(Name: _string; LiteWideCollation: TLiteWideCollation);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteWideCollationDesc.Create(FConnection, Name, LiteWideCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.UnRegisterWideCollation(Name: _string);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := FindCollation(Name);
  if LiteCollationDesc <> nil then
    InternalRemoveCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterDefaultCollations;
begin
  RegisterWideCollation('UniNoCase', DefaultUnicodeNoCase);
end;

procedure TSQLiteCollationManager.UnRegisterDefaultCollations;
begin
  UnRegisterWideCollation('UniNoCase');
end;

initialization
{$IFNDEF CLR}
  CallBackLiteCollationPtr := @CallBackLiteCollation;
{$ELSE}
  CallBackLiteCollationPtr := Marshal.AllocHGlobal(Length(CallBackLiteCollationCode));
  Marshal.Copy(TBytes(CallBackLiteCollationCode), 0, CallBackLiteCollationPtr, Length(CallBackLiteCollationCode));

  CallbackRecPtr := Marshal.AllocHGlobal(20);
  try
    CallBackLiteCollationRec.Ptr := CallBackLiteCollation;
    HCallBackLiteCollation := GCHandle.Alloc(@CallBackLiteCollationRec.Ptr, GCHandleType.Normal);
    Marshal.StructureToPtr(CallBackLiteCollationRec, CallbackRecPtr, False);
    Marshal.WriteIntPtr(CallBackLiteCollationPtr, 25, Marshal.ReadIntPtr(CallbackRecPtr));
  finally
    Marshal.FreeHGlobal(CallbackRecPtr);
  end;
{$ENDIF}

finalization
{$IFNDEF CLR}
  CallBackLiteCollationPtr := nil;
{$ELSE}
  HCallBackLiteCollation.Free;
  Marshal.FreeHGlobal(CallBackLiteCollationPtr);
{$ENDIF}

end.
