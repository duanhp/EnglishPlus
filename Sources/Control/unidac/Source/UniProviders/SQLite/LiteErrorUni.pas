
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I SQLiteDac.inc}
unit LiteErrorUni;
{$ENDIF}

interface

uses
  SysUtils, Classes, MemUtils, {$IFNDEF LITE} DB, DBAccess,{$ENDIF}
  {$IFNDEF UNIDACPRO}LiteCall{$ELSE}LiteCallUni{$ENDIF};

type

{ ESQLiteError }

  ESQLiteError = class({$IFDEF LITE}Exception{$ELSE}EDAError{EDatabaseError}{$ENDIF})
  private
  {$IFDEF LITE}
    FErrorCode: integer;
  {$ENDIF}

  public
    constructor Create(ErrorCode: integer; ErrorMsg: _string);
    destructor Destroy; override;

    function IsFatalError: boolean; {$IFNDEF LITE}override;{$ENDIF}
  {$IFNDEF LITE}
    function IsKeyViolation: boolean; override;
  {$ENDIF}

  {$IFDEF LITE}
    property ErrorCode: integer read FErrorCode;
  {$ENDIF}
  end;

implementation

{ ESQLiteError }

constructor ESQLiteError.Create(ErrorCode: integer; ErrorMsg: _string);
begin
{$IFDEF CLR}
  inherited Create({$IFNDEF LITE}ErrorCode, {$ENDIF}ErrorMsg);
{$ELSE}
  inherited
  Message := ErrorMsg;
{$ENDIF}
  FErrorCode := ErrorCode;
end;

destructor ESQLiteError.Destroy;
begin
  inherited;
end;

function ESQLiteError.IsFatalError: boolean;
begin
  Result := False;
end;

{$IFNDEF LITE}

function ESQLiteError.IsKeyViolation: boolean;
begin
  Result := FErrorCode = SQLITE_CONSTRAINT;
end;

{$ENDIF}

end.
