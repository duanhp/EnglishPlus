
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgErrorUni;

{$ENDIF}

interface

uses
{$IFNDEF LITE}
  DBAccess,
{$ENDIF}
  Classes, SysUtils;

type
  TPgSeverity = (sError, sFatal, sPanic, sWarning, sNotice, sDebug, sInfo, sLog);

  EPgError = class({$IFDEF LITE}Exception{$ELSE}EDAError{$ENDIF})
  private
    FSeverity: TPgSeverity;
    FErrorCode: string;
    FDetailMsg: string;
    FHint: string;
    FCallStack: string;
    FFileName: string;
    FProcedureName: string;
    FPosition: integer;
    FLineNumber: integer;
  public
    constructor Create(Severity: TPgSeverity; Msg: string); overload;
    constructor Create(Severity: TPgSeverity; ErrorCode, Msg, DetailMsg, Hint, CallStack,
      FileName, ProcedureName: string; Position, LineNumber: integer); overload;

    function IsFatalError: boolean; {$IFNDEF LITE}override;{$ENDIF}
  {$IFNDEF LITE}
    function IsKeyViolation: boolean; override;
  {$ENDIF}

    property Severity: TPgSeverity read FSeverity;
    property ErrorCode: string read FErrorCode;
    property DetailMsg: string read FDetailMsg;
    property Hint: string read FHint;
    property CallStack: string read FCallStack;
    property FileName: string read FFileName;
    property ProcedureName: string read FProcedureName;
    property Position: integer read FPosition;
    property LineNumber: integer read FLineNumber;
  end;

  TPgErrors = class(TList)
  private
    function GetError(Index: integer): EPgError;
  public
    destructor Destroy; override;
    property Errors[Index: integer]: EPgError read GetError; default;
  end;

implementation

uses
  {$IFNDEF UNIDACPRO}PgSQLProtocol{$ELSE}PgSQLProtocolUni{$ENDIF};

constructor EPgError.Create(Severity: TPgSeverity; Msg: string);
begin
  inherited Create({$IFNDEF LITE}0,{$ENDIF} Msg);

  FSeverity := Severity;
end;

constructor EPgError.Create(Severity: TPgSeverity; ErrorCode, Msg, DetailMsg, Hint, CallStack,
  FileName, ProcedureName: string; Position, LineNumber: Integer);
begin
  inherited Create({$IFNDEF LITE}StrToIntDef(ErrorCode, 0),{$ENDIF} Msg);

  FSeverity := Severity;
  FErrorCode := ErrorCode;
  FDetailMsg := DetailMsg;
  FHint := Hint;
  FCallStack := CallStack;
  FFileName := FileName;
  FProcedureName := ProcedureName;
  FPosition := Position;
  FLineNumber := LineNumber;
end;

function EPgError.IsFatalError: boolean;
begin
  Result := FSeverity in [sFatal, sPanic];
end;

{$IFNDEF LITE}
function EPgError.IsKeyViolation: boolean;
begin
  Result := FErrorCode = '23505';
end;
{$ENDIF}

{ TPgErrors }

destructor TPgErrors.Destroy;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Errors[i].Free;

  inherited;
end;

function TPgErrors.GetError(Index: integer): EPgError;
begin
  Result := EPgError(inherited Items[Index]);
end;

end.
