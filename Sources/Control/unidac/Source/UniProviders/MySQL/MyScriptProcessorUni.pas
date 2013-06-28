{$IFNDEF CLR}
{$I MyDac.inc}
unit MyScriptProcessorUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, MemUtils, DBAccess, DAScript, CRParser;

type
  TMyScriptProcessor = class (TDAScriptProcessor)
  private
    FPrevCode, FCurrCode: integer;
  protected
    function GetParserClass: TSQLParserClass; override;
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
    function ExecuteNext: boolean; override;
    function CanOptimize(const SQL: _string; const StatementType: integer): boolean; override;
    function IsBlankEndsDelimeter: boolean; override;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  MyParser, MyClasses;
{$ELSE}
  MyParserUni, MyClassesUni;
{$ENDIF}

{ TMyScriptProcessor }

function TMyScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TMyParser;
end;

function TMyScriptProcessor.ExecuteNext: boolean;
begin
  FCurrCode := 0;
  Result := inherited ExecuteNext;
end;

procedure TMyScriptProcessor.CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean);
begin
  if (Code <> lcBlank) and (Code <> lcComment) then begin
    FPrevCode := FCurrCode;
    FCurrCode := Code;

    // Set statement types
    if FCurrCode = lxDELIMITER then begin
      FDelimiterState := dsDelimiter;
      StatementType := ST_DELIMETER;
    end;

    if FCurrDelimiter = ';' then
      if (FPrevCode = lxCREATE) and
        ((FCurrCode = lxPROCEDURE) or (FCurrCode = lxFUNCTION) or (FCurrCode = lxTRIGGER))
      then
        StatementType := ST_SPECIFIC_SQL;
  end;
end;

function TMyScriptProcessor.CanOptimize(const SQL: _string; const StatementType: integer): boolean;
var
  IConnection: TMySQLConnection;
begin
  IConnection := TDBAccessUtils.GetIConnection(UsedConnection) as TMySQLConnection;
  Result := IConnection.IsClient41 and IConnection.IsServer41 and inherited CanOptimize(SQL, StatementType);
end;

function TMyScriptProcessor.IsBlankEndsDelimeter: boolean;
begin
  Result := True;
end;

end.
