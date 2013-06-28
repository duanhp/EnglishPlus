
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  InterBase Error
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I IbDac.inc}
unit IBCErrorUni;
{$ENDIF}


interface
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  SysUtils, Classes, MemUtils, {$IFNDEF LITE} DB, DBAccess,{$ENDIF}
  {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF};

type

{ EIBCError }

  EIBCError = class({$IFDEF LITE}Exception{$ELSE}EDAError{EDatabaseError}{$ENDIF})
  private
    FSQLErrorMsg: _string;
    FErrorNumber: integer;
  {$IFDEF LITE}
    FErrorCode: integer;
  {$ENDIF}
    FSender: TComponent;

  public
    constructor Create(ErrorCode: integer; ErrorNumber: integer; ErrorMsg: _string; SQLErrorMsg: _string);
    destructor Destroy; override;

    class function IsFatalError(ErrorNumber: integer): boolean; reintroduce; overload;
  {$IFNDEF LITE}
    function IsFatalError: boolean; overload; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}

    property SQLErrorMsg: _string read FSQLErrorMsg;
    property ErrorNumber: integer read FErrorNumber;
  {$IFDEF LITE}
    property ErrorCode: integer read FErrorCode;
  {$ENDIF}
    property Sender: TComponent read FSender write FSender;
  end;

  procedure RaiseError(Msg: string);

  procedure RaiseIBCError(ErrorCode, ErrorNumber: integer; ErrorMsg: _string; SQLErrorMsg: _string);

const
  //integrity check
  isc_integ_fail           = 335544342;
  isc_no_dup               = 335544349;
  isc_unique_key_violation = 335544665;

  isc_deadlock      =  335544336;
  isc_lock_conflict =  335544345;
  isc_segment       =  335544366;
  isc_field_not_defined = 335544396;

  //Fatal Error codes
  isc_network_error =  335544721;
  isc_lost_db_connection = 335544741;
  isc_conn_lost     = 335544648;

  //user_password error
  isc_invalid_user_password = 335544472;

implementation

procedure RaiseError(Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure RaiseIBCError(ErrorCode, ErrorNumber: integer; ErrorMsg: _string; SQLErrorMsg: _string);
begin
  raise EIBCError.Create(ErrorCode, ErrorNumber, ErrorMsg, SQLErrorMsg);
end;

{ EIBCError }

constructor EIBCError.Create(ErrorCode: integer; ErrorNumber: integer; ErrorMsg: _string; SQLErrorMsg: _string);
begin
{$IFDEF CLR}
  inherited Create({$IFNDEF LITE}ErrorCode, {$ENDIF}ErrorMsg);
{$ELSE}
  inherited
  Message := ErrorMsg;
{$ENDIF}
  FSQLErrorMsg := SQLErrorMsg;
  FErrorNumber := ErrorNumber;
  FErrorCode := ErrorCode;
  FSender := nil;
end;

destructor EIBCError.Destroy;
begin
  inherited;
end;

class function EIBCError.IsFatalError(ErrorNumber: integer): boolean;
begin
  case ErrorNumber of
    isc_network_error, isc_lost_db_connection, isc_conn_lost:
      Result := True;
  else
    Result := False;
  end;
end;

{$IFNDEF LITE}
function EIBCError.IsFatalError: boolean;
begin
  Result := IsFatalError(ErrorNumber);
end;

function EIBCError.IsKeyViolation: boolean;
begin
  Result := ErrorNumber = isc_unique_key_violation;
end;
{$ENDIF}

end.
