
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DBFDac.inc}
unit DBFClassesUni;
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
  prDBFBase = 2000;

  prCollatingSequence = prDBFBase + 1;

type
{ TDBFConnection }

  TDBFConnection = class(TODBCConnection)
  private
    FDatabase: string;
    FCollatingSequence: string;
  protected
    function GetConnectionString: _string; override;

  public
    function GetCommandClass: TCRCommandClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
  end;

{ TDBFSQLInfo }

  TDBFSQLInfo = class(TODBCSQLInfo)
  public
    function IdentCase: TIdentCase; override;
  end;

{ TDBFCommand }

  TDBFCommand = class(TODBCCommand)
  protected
  public
    class function GetSQLInfoClass: TSQLInfoClass; override;
  end;

{ TDBFRecordSet }

  TDBFRecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
  end;

{$IFNDEF LITE}

{ TDBFLoader }

  TDBFLoader = class (TODBCLoader)
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

{ TDBFConnection }

function TDBFConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TDBFCommand;
end;

function TDBFConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prCollatingSequence:
      FCollatingSequence := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TDBFConnection.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    // max string parameter length
    prMaxStringSize:
      Value := 254;
    prDatabase:
      Value := FDatabase;
    prCollatingSequence:
      Value := FCollatingSequence;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TDBFConnection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TDBFConnection(Source).FDatabase;
end;

function TDBFConnection.GetConnectionString: _string;
var
  DefaultDir: string;
begin
  DefaultDir := ExtractFilePath(FDatabase);
  Result := _Format('DRIVER={Microsoft dBase Driver (*.dbf)};DefaultDir=%s;DBQ=%s;UID=%s;PWD=%s',
    [DefaultDir, FDatabase, FUsername, FPassword]);

  if FCollatingSequence <> '' then
    Result := Result + ';CollatingSequence=' + FCollatingSequence;
end;

{ TDBFSQLInfo }

function TDBFSQLInfo.IdentCase: TIdentCase;
begin
  Result := icMixed;
end;

{ TDBFCommand }

class function TDBFCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TDBFSQLInfo;
end;

{ TDBFRecordSet }

procedure TDBFRecordSet.CreateCommand;
begin
  SetCommand(TDBFCommand.Create);
end;

{$IFNDEF LITE}

{ TDBFLoader }

procedure TDBFLoader.CreateCommand;
begin
  FCommand := TDBFCommand.Create;
end;

class function TDBFLoader.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TDBFRecordSet;
end;

{$ENDIF}

end.
