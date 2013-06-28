
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlBindUni;
{$ENDIF}

interface

uses
{$IFDEF CLR}
  System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, MemUtils,
{$IFNDEF UNIDACPRO}
  MyCall, MySqlType;
{$ELSE}
  MyCallUni, MySqlTypeUni;
{$ENDIF}

type
{$IFDEF CLR}
  EncodingClass = Encoding;
{$ENDIF}

{$IFNDEF CLR}
  {$RANGECHECKS OFF} // Internal Error C1091 (d7): Result := ulong(result1) * 10000000000 + ulong(result2) * 10 + rowBuffer[_end] - byte('0');
{$ENDIF}

  TMySqlBinds = class;

  TMySqlBind = class
  public
    IsNull: boolean;
    Offset: integer;
    Length: integer;
    _Type: TMySqlFieldType;
    DbType: TMySqlType;
    Unicode: boolean;
  end;

  TMySqlBinds = class
  public
    Binds: array of TMySqlBind;
    Buffer: TValueArr;

    constructor Create(Len: integer);
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF VER6P}
  DateUtils,
{$ENDIF}
  SysConst;

const
  SCannotConvert = 'Cannot Convert';

// Copied from SysUtils
procedure ConvertError(const ResString: string);
begin
  raise EConvertError.Create(ResString); // ..CreateRes(ResString);
end;

procedure ConvertErrorFmt(const ResString: string; const Args: array of const);
begin
  raise EConvertError.Create(Format(ResString, Args)); // ..CreateResFmt(ResString, Args);
end;
//

{ TMySqlBinds }

constructor TMySqlBinds.Create(Len: integer);
var
  i: integer;
begin
  inherited Create;

  SetLength(Binds, Len);
  for i := 0 to Length(Binds) - 1 do
    Binds[i] := TMySqlBind.Create;
end;

destructor TMySqlBinds.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(Binds) - 1 do
    Binds[i].Free;

  inherited;
end;

end.
