
//////////////////////////////////////////////////
//  MS Access Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I AccessDac.inc}
unit AccessClassesUni;
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
  ODBCCall, ODBCClasses, AccessParser;
{$ELSE}
  ODBCCallUni, ODBCClassesUni, AccessParserUni;
{$ENDIF}

const
  prAccessBase = 2000;

  prSystemDatabase =  prAccessBase + 1;
  prExtendedAnsiSQL = prAccessBase + 2;
  prExclusiveLock   = prAccessBase + 3;

type
{ TAccessConnection }

  TAccessConnection = class(TODBCConnection)
  private
    FDatabase: string;
    FSystemDatabase: string;
    FExtendedAnsiSQL: integer;
    FExclusiveLock: Boolean;
  protected
    function GetConnectionString: _string; override;
    function IsCommentAllowed: boolean; override;
    function IsEmptyStringAllowed: boolean; override;

  public
    function GetCommandClass: TCRCommandClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
  end;

{ TAccessSQLInfo }

  TAccessSQLInfo = class(TODBCSQLInfo)
  public
    function LeftQuote: _char; override;
    function RightQuote: _char; override;
    function IdentCase: TIdentCase; override;
  end;

{ TAccessCommand }

  TAccessCommand = class(TODBCCommand)
  protected
    function GetBlobSize(SQLLength: longword): Integer; override;
  public
    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
  end;

{ TAccessRecordSet }

  TAccessRecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
  end;

{$IFNDEF LITE}

{ TAccessLoader }

  TAccessLoader = class (TODBCLoader)
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

{ TAccessConnection }

function TAccessConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TAccessCommand;
end;

function TAccessConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prSystemDatabase:
      FSystemDatabase := Value;
    prExtendedAnsiSQL:
      FExtendedAnsiSQL := Value;
    prExclusiveLock:
      FExclusiveLock := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TAccessConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prSystemDatabase:
      Value := FSystemDatabase;
    prMaxStringSize:
      Value := 255;
    prExtendedAnsiSQL:
      Value := FExtendedAnsiSQL;
    prExclusiveLock:
      Value:= FExclusiveLock;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TAccessConnection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TAccessConnection(Source).FDatabase;
  FSystemDatabase := TAccessConnection(Source).FSystemDatabase;
  FExtendedAnsiSQL := TAccessConnection(Source).FExtendedAnsiSQL;
  FExclusiveLock := TAccessConnection(Source).FExclusiveLock;
end;

function TAccessConnection.GetConnectionString: _string;
var
  DefaultDir: string;
begin
  DefaultDir := ExtractFilePath(FDatabase);
  if IsDriverPresent('Microsoft Access Driver (*.mdb, *.accdb)') then
    Result := _Format('DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DefaultDir=%s;DBQ=%s;UID=%s;PWD=%s',
      [DefaultDir, FDatabase, FUsername, FPassword])
  else
    Result := _Format('DRIVER={Microsoft Access Driver (*.mdb)};DefaultDir=%s;DBQ=%s;UID=%s;PWD=%s',
      [DefaultDir, FDatabase, FUsername, FPassword]);

  if FExclusiveLock then
    Result:= Result +  ';Exclusive=1';

  if FSystemDatabase <> '' then
    Result := Result + ';SystemDB=' + FSystemDatabase;

  if FExtendedAnsiSQL <> 0 then
    Result := Result + ';ExtendedAnsiSQL=' + IntToStr(FExtendedAnsiSQL);
end;

function TAccessConnection.IsCommentAllowed: boolean;
begin
  Result := False;
end;

function TAccessConnection.IsEmptyStringAllowed: boolean;
begin
  Result := False;
end;

{ TAccessSQLInfo }

function TAccessSQLInfo.LeftQuote: _char;
begin
  Result := '[';
end;

function TAccessSQLInfo.RightQuote: _char;
begin
  Result := ']';
end;

function TAccessSQLInfo.IdentCase: TIdentCase;
begin
  Result := icMixed;
end;

{ TAccessCommand }

function TAccessCommand.GetBlobSize(SQLLength: longword): Integer;
begin
  if Integer(SQLLength) = 1073741823 then
    Result := 0
  else
    Result := inherited GetBlobSize(SQLLength);
end;

class function TAccessCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TAccessSQLInfo;
end;

class function TAccessCommand.GetParserClass: TSQLParserClass;
begin
  Result := TAccessParser;
end;

{ TAccessRecordSet }

procedure TAccessRecordSet.CreateCommand;
begin
  SetCommand(TAccessCommand.Create);
end;

{$IFNDEF LITE}

{ TAccessLoader }

procedure TAccessLoader.CreateCommand;
begin
  FCommand := TAccessCommand.Create;
end;

class function TAccessLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TAccessRecordSet;
end;

{$ENDIF}

end.
