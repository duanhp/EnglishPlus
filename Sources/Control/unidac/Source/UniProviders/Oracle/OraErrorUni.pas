
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  Oracle Error
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Odac.inc}
unit OraErrorUni;
{$ENDIF}

interface

uses
  SysUtils, Classes, MemUtils, {$IFNDEF LITE}DB, DBAccess,{$ENDIF}
  {$IFNDEF UNIDACPRO}OraCall{$ELSE}OraCallUni{$ENDIF};

type

{ EOracleError }

  EOraError = class({$IFDEF LITE}Exception{$ELSE}EDAError{EDatabaseError}{$ENDIF})
  private
  {$IFDEF LITE}
    FErrorCode: integer;
  {$ENDIF}
    FSender: TComponent;

  public
    constructor Create(ErrorCode: integer; Msg: _string; Component: TObject = nil);
    destructor Destroy; override;

  {$IFNDEF LITE}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}

  {$IFDEF LITE}
    property ErrorCode: integer read FErrorCode;
  {$ENDIF}
    property Sender: TComponent read FSender write FSender;
  end;

  procedure RaiseError(Msg: string);

  procedure RaiseOraError(ErrorCode: integer; Msg: _string);

const
  erKeyViol           = 0;
  erRequiredFieldMissing = 1;
  erCheck             = 2;
  erLockRecord        = -54;
  erParentKeyNotFound = -2291;
  erChildRecordCount  = -2292;

implementation

procedure RaiseError(Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure RaiseOraError(ErrorCode: integer; Msg: _string);
begin
  raise EOraError.Create(ErrorCode, Msg);
end;

{ EOracleError }

constructor EOraError.Create(ErrorCode: integer; Msg: _string; Component: TObject = nil);
begin
{$IFDEF CLR}
  inherited Create({$IFNDEF LITE}ErrorCode, {$ENDIF}Msg);
{$ELSE}
  inherited
  Message := Msg;
{$ENDIF}
  FErrorCode := ErrorCode;
  FSender := nil;
{$IFNDEF LITE}
  FComponent := Component;
{$ENDIF}
end;

destructor EOraError.Destroy;
begin
  inherited;
end;

{$IFNDEF LITE}
function EOraError.IsFatalError: boolean;
begin
  Result := (ErrorCode = OCI_SESSION_KILLED) or
    (ErrorCode = OCI_NOT_LOGGEDON) or
    (ErrorCode = OCI_EOF_COMMUNICATION) or
    (ErrorCode = OCI_NOT_CONNECTED) or
    (ErrorCode = 12203) or // after break;
    (ErrorCode = 12571) or
    (ErrorCode = 12514) or
    (ErrorCode = 12152); //Oracle 9.2.0.6 fix (CR 8513)
end;

function EOraError.IsKeyViolation: boolean;
begin
  Result := ErrorCode = 1;
end;
{$ENDIF}

end.
