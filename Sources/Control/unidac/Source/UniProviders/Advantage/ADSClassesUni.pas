
//////////////////////////////////////////////////
//  Advantage Database Server Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ADSDac.inc}
unit ADSClassesUni;
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
  ODBCCall, ODBCClasses;
{$ELSE}
  ODBCCallUni, ODBCClassesUni;
{$ENDIF}

const
  prADSBase = 2000;

  prADSServerTypes   = prADSBase + 1;

resourcestring
  SInvalidServerTypes = 'Invalid server types';

type
  TADSServerType = (stLocal, stRemote, stInternet);
  TADSServerTypes = set of TADSServerType;

{ TADSConnection }

  TADSConnection = class(TODBCConnection)
  private
    FDatabase: string;
    FServerTypes: string;

    function ParseServerTypes(const ServerTypes: string): TADSServerTypes;

  protected
    function GetConnectionString: _string; override;
    function IsBlockFetchAllowed: boolean; override;

  public
    function GetCommandClass: TCRCommandClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
  end;

{ TADSCommand }

  TADSCommand = class(TODBCCommand)
  end;

{ TADSRecordSet }

  TADSRecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
  end;

{$IFNDEF LITE}

{ TADSLoader }

  TADSLoader = class (TODBCLoader)
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

{ TADSConnection }

function TADSConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TADSCommand;
end;

function TADSConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prADSServerTypes:
      FServerTypes := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TADSConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prADSServerTypes:
      Value := FServerTypes;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TADSConnection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TADSConnection(Source).FDatabase;
end;

function TADSConnection.GetConnectionString: _string;
var
  ServerTypes: TADSServerTypes;
  i: integer;
begin
  ServerTypes := ParseServerTypes(FServerTypes);
  if ServerTypes = [] then
    raise Exception.Create(SInvalidServerTypes);

  i := 0;
  if stLocal in ServerTypes then
    i := 1;
  if stRemote in ServerTypes then
    i := i + 2;
  if stInternet in ServerTypes then
    i := i + 4;

  Result := _Format('DRIVER={Advantage StreamlineSQL ODBC};DataDirectory=%s;UID=%s;PWD=%s;ServerTypes=%d',
    [FDatabase, FUsername, FPassword, i]);
end;

function TADSConnection.IsBlockFetchAllowed: boolean;
begin
  Result := False;
end;

function TADSConnection.ParseServerTypes(const ServerTypes: string): TADSServerTypes;
var
  Str: string;
begin
  Str := Trim(ServerTypes);
  if Str = '' then begin
    Result := [stRemote, stInternet];
    exit;
  end;

  Result := [];
  if Pos('ALS', Str) > 0 then
    Include(Result, stLocal);
  if Pos('ADS', Str) > 0 then
    Include(Result, stRemote);
  if Pos('AIS', Str) > 0 then
    Include(Result, stInternet);
end;

{ TADSRecordSet }

procedure TADSRecordSet.CreateCommand;
begin
  SetCommand(TADSCommand.Create);
end;

{$IFNDEF LITE}

{ TADSLoader }

procedure TADSLoader.CreateCommand;
begin
  FCommand := TADSCommand.Create;
end;

class function TADSLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TADSRecordSet;
end;

{$ENDIF}

end.
