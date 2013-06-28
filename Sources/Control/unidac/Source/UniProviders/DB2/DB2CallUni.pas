
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DB2Dac.inc}
unit DB2CallUni;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
  MemUtils,
{$IFNDEF UNIDACPRO}
  ODBCCall;
{$ELSE}
  ODBCCallUni;
{$ENDIF}

const
  DB2DLLName = 'db2cli.dll';

{$IFNDEF CLR}
var
  DB2DLL: string;
{$ELSE}
const
  DB2DLL = DB2DLLName;
{$ENDIF}

type
  _SQLGetLength = function(
    StatementHandle: TSQLHStmt;
    LocatorCType: smallint;
    Locator: integer;
    var StringLength: integer;
    var IndicatorValue: integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  _SQLGetSubString = function(
    StatementHandle: TSQLHStmt;
    LocatorCType: smallint;
    SourceLocator: integer;
    FromPosition: cardinal;
    ForLength: cardinal;
    TargetCType: smallint;
    DataPtr: IntPtr;
    BufferLength: integer;
    var StringLength: integer;
    var IndicatorValue: integer
  ): smallint; {$IFNDEF CLR}{$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};{$ENDIF}

  TDB2Cli = class (TODBCCli)
  protected
    function GetLibName: string; override;
    procedure InitFunctions; override;

  public
    SQLGetLength: _SQLGetLength;
    SQLGetSubString: _SQLGetSubString;
  end;

function GetDB2Cli: TDB2Cli;

implementation

(*{$IFDEF CLR}
uses
{$IFNDEF UNIDACPRO}
  DB2CallCLR;
{$ELSE}
  DB2CallCLRUni;
{$ENDIF}
{$ENDIF}*)

var
  DB2Cli: TDB2Cli;
  LockDB2Cli: TCriticalSection;

function GetDB2Cli: TDB2Cli;
begin
  LockDB2Cli.Enter;
  try
    if DB2Cli = nil then
      DB2Cli := TDB2Cli.Create(LockDB2Cli);
  finally
    LockDB2Cli.Leave;
  end;

  Result := DB2Cli;
end;

function TDB2Cli.GetLibName: string;
begin
{$IFNDEF CLR}
  if DB2DLL = '' then
    DB2DLL := DB2DLLName;
{$ENDIF}
  Result := DB2DLL;
end;

procedure TDB2Cli.InitFunctions;
begin
  inherited;

{$IFDEF CLR}
  //SQLGetLength := {$IFNDEF UNIDACPRO}DB2CallCLR{$ELSE}DB2CallCLRUni{$ENDIF}.SQLGetLength;
  //SQLGetSubString := {$IFNDEF UNIDACPRO}DB2CallCLR{$ELSE}DB2CallCLRUni{$ENDIF}.SQLGetSubString;
{$ELSE}
  SQLGetLength := GetProc('SQLGetLength');
  SQLGetSubString := GetProc('SQLGetSubString');
{$ENDIF}
end;

initialization
  LockDB2Cli := TCriticalSection.Create;
  DB2Cli := nil;

finalization
{$IFNDEF CLR}
  DB2Cli.Free;
{$ENDIF}
  LockDB2Cli.Free;

end.

