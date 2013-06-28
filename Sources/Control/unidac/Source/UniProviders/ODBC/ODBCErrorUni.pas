
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ODBCDac.inc}
unit ODBCErrorUni;
{$ENDIF}

interface

uses
  SysUtils, Classes, MemUtils {$IFNDEF LITE}, DB, DBAccess{$ENDIF};

type

{ EODBCError }

  EODBCError = class({$IFDEF LITE}Exception{$ELSE}EDAError{EDatabaseError}{$ENDIF})
  private
  {$IFDEF LITE}
    FErrorCode: integer;
  {$ENDIF}
    FState: string;
    FNativeErrorCode: integer;

  public
    constructor Create(ErrorCode: integer; ErrorMsg: _string; State: string;
      NativeErrorCode: integer);
    destructor Destroy; override;

  {$IFNDEF LITE}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}

  {$IFDEF LITE}
    property ErrorCode: integer read FErrorCode;
  {$ENDIF}
    property State: string read FState;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  ODBCCall;
{$ELSE}
  ODBCCallUni;
{$ENDIF}

{ EODBCError }

constructor EODBCError.Create(ErrorCode: integer; ErrorMsg: _string; State: string;
  NativeErrorCode: integer);
begin
{$IFDEF CLR}
  inherited Create({$IFNDEF LITE}ErrorCode, {$ENDIF}ErrorMsg);
{$ELSE}
  inherited
  Message := ErrorMsg;
{$ENDIF}
  FErrorCode := ErrorCode;
  FState := State;
  FNativeErrorCode := NativeErrorCode;
end;

destructor EODBCError.Destroy;
begin
  inherited;
end;

{$IFNDEF LITE}
function EODBCError.IsFatalError: boolean;
begin
  Result := False;
end;

function EODBCError.IsKeyViolation: boolean;
begin
  Result := Copy(FState, 1, 2) = '23';
end;
{$ENDIF}

end.
