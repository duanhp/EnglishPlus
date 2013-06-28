
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ASEDac.inc}
unit ASEClassesUni;
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
  ODBCCall, ODBCClasses, ASEParser;
{$ELSE}
  ODBCCallUni, ODBCClassesUni, ASEParserUni;
{$ENDIF}

const
  prASEBase = 2000;
  prAnsiNull = prASEBase + 1;
  prApplicationName = prASEBase + 2;

type
  TASEDriverType = (ad12, ad15);

{ TASEConnection }

  TASEConnection = class(TODBCConnection)
  private
    FDatabase: string;
    FPort: integer;
    FDriverType: TASEDriverType;
    FAnsiNull: Boolean;
    FServiceConnection: TASEConnection;
    FApplicationName: string;
    procedure SetAnsiNull(const Value: boolean);

  protected
    function GetConnectionString: _string; override;
    function IsReturnValueAllowed: boolean; override;
    function OutParamIsInOut: boolean; override;
    procedure SetDatabase(const Value: string);
    procedure InternalSetAnsiNull(const Value: boolean);

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetTransactionClass: TCRTransactionClass; override;

    procedure Connect(const ConnectString: _string); override;
    procedure Disconnect; override;
    function GetServiceConnection: TASEConnection;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
    function CanChangeDatabase: boolean; override;
    function GetCurrentCatalog: _string; override;
    function GetCachedCatalog: _string; override;
    function GetCurrentSchema: _string; override;
  end;

{ TASETransaction }

  TASETransaction = class(TODBCTransaction)
  public
    procedure Savepoint(const Name: _string); override;
    procedure ReleaseSavepoint(const Name: _string); override;
    procedure RollbackToSavepoint(const Name: _string); override;
  end;

{ TASEParamDesc }

  TASEParamDesc = class (TODBCParamDesc)
  public
    constructor Create; override;
  end;

{ TASESQLInfo }

  TASESQLInfo = class(TODBCSQLInfo)
  public
    function LeftQuote: _char; override;
    function RightQuote: _char; override;
  end;

{ TASECommand }

  TASECommand = class(TODBCCommand)
  protected
    procedure SetSpecificParamPrec(Param: TODBCParamDesc; var Prec, Scale: integer); override;

  public
    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    function GetParamDescType: TParamDescClass; override;
  end;

{ TASERecordSet }

  TASERecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
  end;

{ TASEMetaData }

  TASEMetaData = class(TODBCMetaData)
  protected
    FRecordSet2: TASERecordSet;

    procedure InternalGetMetaDataKindsList(List: _TStringList); override;

    function GetDatabases(Restrictions: _TStrings): TData; override;

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{$IFNDEF LITE}

{ TASELoader }

  TASELoader = class (TODBCLoader)
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

{ TASEConnection }

constructor TASEConnection.Create;
begin
  inherited;
  
  FAnsiNull := True; // Set the AnsiNull option to On by default (old behavior)
end;

destructor TASEConnection.Destroy;
begin
  FServiceConnection.Free;
  FServiceConnection := nil;

  inherited;
end;

procedure TASEConnection.SetAnsiNull(const Value: boolean);
begin
  if FAnsiNull <> Value then
  begin
    FAnsiNull := Value;
    if FConnected then
      InternalSetAnsiNull(FAnsiNull);
  end;
end;

function TASEConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TASECommand;
end;

function TASEConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TASETransaction;
end;

function TASEConnection.GetConnectionString: _string;
var
  Port: integer;
  Server: string;
  Database: string;
  ApplicationName: string;
begin
  Server := Trim(FServer);
  if Server = '' then
    Server := 'localhost';

  Port := FPort;
  if Port = 0 then
    Port := 5000;

  Database := Trim(FDatabase);
  ApplicationName := Trim(FApplicationName);

  if IsDriverPresent('Adaptive Server Enterprise') then begin
    Result := _Format('DRIVER={Adaptive Server Enterprise};UID=%s;PWD=%s;server=%s;port=%d',
      [FUsername, FPassword, Server, Port]);
    if Database <> '' then
      Result := Result + ';database= ' + Database;
    if ApplicationName <> '' then
      Result:= Result + ';App= ' + ApplicationName;

    FDriverType := ad15;
  end
  else begin
    Result := _Format('DRIVER={Sybase ASE ODBC Driver};UID=%s;PWD=%s;NA=%s,%d',
      [FUsername, FPassword, Server, Port]);
    if Database <> '' then
      Result := Result + ';DB= ' + Database;
    if ApplicationName <> '' then
      Result:= Result + ';App= ' + ApplicationName;
    FDriverType := ad12;
  end;
end;

function TASEConnection.IsReturnValueAllowed: boolean;
begin
  Result := FDriverType <> ad15;
end;

function TASEConnection.OutParamIsInOut: boolean;
begin
  Result := True;
end;

procedure TASEConnection.SetDatabase(const Value: string);
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if Value = '' then
    raise Exception.Create(SWrongDatabaseName);

  ExecSQL('USE ' + FRecordSet.GetCommand.SQLInfo.NormalizeName(Value));
  FDatabase := Value;
  FCachedCatalog := '';
end;

procedure TASEConnection.InternalSetAnsiNull(const Value: boolean);
begin
  if Value then
    ExecSQL('set ansinull on')
  else
    ExecSQL('set ansinull off');
end;

procedure TASEConnection.Connect(const ConnectString: _string);
begin
  inherited;

  InternalSetAnsiNull(FAnsiNull);
end;

procedure TASEConnection.Disconnect;
begin
  if FServiceConnection <> nil then
    FServiceConnection.Disconnect;

  inherited;
end;

function TASEConnection.GetServiceConnection;
begin
  if FServiceConnection = nil then
    FServiceConnection := TASEConnection.Create;

  if not FServiceConnection.FConnected then begin
    FServiceConnection.Assign(Self);
    FServiceConnection.Connect('');
  end;

  Result := FServiceConnection;
end;

function TASEConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase: begin
      if Value <> FDatabase then begin
        if GetConnected then
          SetDatabase(Value);
        FDatabase := Value;
      end;
    end;
    prPort:
      FPort := Value;
    prAnsiNull:
      SetAnsiNull(Value);
    prApplicationName:
      FApplicationName := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TASEConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prPort:
      Value := FPort;
    prAnsiNull:
      Value := FAnsiNull;
    prMaxStringSize:
      Value := 1960;
    prApplicationName:
      Value := FApplicationName;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TASEConnection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TASEConnection(Source).FDatabase;
  FPort := TASEConnection(Source).FPort;
  FAnsiNull := TASEConnection(Source).FAnsiNull;
  FApplicationName := TASEConnection(Source).FApplicationName;
end;

function TASEConnection.CanChangeDatabase: boolean;
begin
  Result := True; 
end;

function TASEConnection.GetCurrentCatalog: _string;
var
  RecBuf: IntPtr;
  v: variant;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  CheckRecordSet;
  FRecordSet.SetSQL('SELECT db_name()');
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

function TASEConnection.GetCachedCatalog: _string;
begin
  if FCachedCatalog = '' then
    if FDatabase <> '' then
      FCachedCatalog := FRecordSet.GetCommand.SQLInfo.NormalizeName(FDatabase, False, True)
    else
      FCachedCatalog := GetCurrentCatalog;

  Result := FCachedCatalog;
end;

function TASEConnection.GetCurrentSchema: _string;
var
  RecBuf: IntPtr;
  v: variant;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  CheckRecordSet;
  FRecordSet.SetSQL('SELECT USER');
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

{ TASETransaction }

procedure TASETransaction.Savepoint(const Name: _string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecSQL('SAVE TRANSACTION ' + Name);
end;

procedure TASETransaction.ReleaseSavepoint(const Name: _string);
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TASETransaction.RollbackToSavepoint(const Name: _string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecSQL('ROLLBACK ' + Name);
end;

{ TASEParamDesc }

constructor TASEParamDesc.Create;
begin
  inherited;
  FEnableMSec := False;
end;

{ TASESQLInfo }

function TASESQLInfo.LeftQuote: _char;
begin
  Result := '[';
end;

function TASESQLInfo.RightQuote: _char;
begin
  Result := ']';
end;

{ TASECommand }

class function TASECommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TASESQLInfo;
end;

class function TASECommand.GetParserClass: TSQLParserClass;
begin
  Result := TASEParser;
end;

function TASECommand.GetParamDescType: TParamDescClass;
begin
  Result := TASEParamDesc;
end;

procedure TASECommand.SetSpecificParamPrec(Param: TODBCParamDesc; var Prec, Scale: integer);
begin
  case Param.GetDataType of
    dtDateTime:
      Prec := 19;
  end;
end;

{ TASERecordSet }

procedure TASERecordSet.CreateCommand;
begin
  SetCommand(TASECommand.Create);
end;

{ TASEMetaData }

constructor TASEMetaData.Create;
begin
  inherited;

  FRecordSet2 := TASERecordSet.Create;
  FRecordSet2.SetProp(prFetchAll, True);
  FRecordSet2.SetProp(prFlatBuffers, False);
end;

destructor TASEMetaData.Destroy;
begin
  FRecordSet2.Free;

  inherited;
end;

procedure TASEMetaData.InternalGetMetaDataKindsList(List: _TStringList);
begin
  inherited;

  List.Add('Databases');
  List.Sort;
end;

function TASEMetaData.GetDatabases(Restrictions: _TStrings): TData;
const
  SQL = 'SELECT name AS DATABASE_NAME FROM master.dbo.sysdatabases';
begin
  FRecordSet2.SetConnection(FRecordSet.GetCommand.GetConnection);
  FRecordSet2.SetSQL(SQL);
  FRecordSet2.Open;
  Result := FRecordSet2;
end;

{$IFNDEF LITE}

{ TASELoader }

procedure TASELoader.CreateCommand;
begin
  FCommand := TASECommand.Create;
end;

class function TASELoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TASERecordSet;
end;

{$ENDIF}

end.
