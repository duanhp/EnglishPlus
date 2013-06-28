{$IFNDEF CLR}
{$I Odac.inc}
unit OraScriptProcessorUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, MemUtils, DBAccess, DAScript, CRParser, CRAccess;

const
  ST_NORMAL     = 100;  // normal SQL query
  ST_SQLPLUS    = $100; // mask for SQL*Plus commands
  ST_IGNORED    = ST_SQLPLUS + 1;  // ignored SQL*Plus command
  ST_CONNECT    = ST_SQLPLUS + 2;  // SQL*Plus command CONNECT
  ST_DISCONNECT = ST_SQLPLUS + 3;  // SQL*Plus command DISCONNECT
  ST_DEFINE     = ST_SQLPLUS + 4;  // SQL*Plus command DEFINE
  ST_EXECUTE    = ST_SQLPLUS + 5;  // SQL*Plus command EXECUTE

type
  TOraScriptProcessor = class (TDAScriptProcessor)
  protected
    FCodes: array of Integer;
    FStatementType: integer;

    function GetParserClass: TSQLParserClass; override;
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
    function GetReady(Code: integer): boolean; override;
    procedure DoBeforeStatementExecute(var SQL: _string; StatementType: integer; var Omit: boolean); override;

  public
    constructor Create(Owner: TDAScript); override;
  end;

implementation

uses
  {$IFNDEF UNIDACPRO}OraParser{$ELSE}OraParserUni{$ENDIF}, DAConsts,
  {$IFNDEF UNIDACPRO}OraConsts{$ELSE}OraConstsUni{$ENDIF},
  {$IFNDEF UNIDACPRO}OraClasses{$ELSE}OraClassesUni{$ENDIF};

{ TOraScriptProcessor }

const
   HighCodeIndex = 2;

constructor TOraScriptProcessor.Create(Owner: TDAScript);
begin
  inherited;

  SetLength(FCodes, HighCodeIndex + 1);
end;

function TOraScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TOraParser;
end;

procedure TOraScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
var
  i: integer;
begin
  inherited;

  //Analize and set StatementType
  if (Code <> lcBlank) and (Code <> lcComment) then begin
    if StatementType = ST_UNKNOWN then
      for i := 0 to HighCodeIndex - 1 do
        FCodes[i] := 0
    else
      for i := 0 to HighCodeIndex - 1 do
        FCodes[i] := FCodes[i + 1];

    FCodes[HighCodeIndex] := Code;

    case StatementType of
      ST_UNKNOWN:
        case Code of
          lxBEGIN, lxDECLARE:
            StatementType := ST_SPECIFIC_SQL;

          // check SQL*Plus commands
          lxEXIT, lxPAUSE, lxPROMPT, lxQUIT, lxREMARK, lxREM, lxUNDEFINE:
            StatementType := ST_IGNORED;
          lxCONNECT:
            StatementType := ST_CONNECT;
          lxDISCONNECT:
            StatementType := ST_DISCONNECT;
          lxDEFINE:
            StatementType := ST_DEFINE;
          lxEXECUTE:
            StatementType := ST_EXECUTE;
        else
          StatementType := ST_NORMAL;
        end;
      ST_NORMAL:
        case Code of
          lxPROCEDURE, lxFUNCTION, lxPACKAGE, lxTRIGGER:
            if (FCodes[HighCodeIndex - 1] = lxCREATE) and (FCodes[HighCodeIndex - 2] = 0) or
              (FCodes[HighCodeIndex - 1] = lxREPLACE) and (FCodes[HighCodeIndex - 2] = lxOR)
            then
              StatementType := ST_SPECIFIC_SQL;
          lxBODY:
            if FCodes[HighCodeIndex - 1] = lxTYPE then
              StatementType := ST_SPECIFIC_SQL;
          lxJAVA:
            StatementType := ST_SPECIFIC_SQL;

          // SQL*Plus commands
          lxDEFINE:
            if (FCodes[HighCodeIndex - 1] = lxSET) and (FCodes[HighCodeIndex - 2] = 0) then
              StatementType := ST_IGNORED;
        end;
    end;
  end;

  FStatementType := StatementType;
end;

function TOraScriptProcessor.GetReady(Code: integer): boolean;
begin
  if (FStatementType and ST_SQLPLUS <> 0) and ((Pos(#13, FSt) > 0) or (Pos(#10, FSt) > 0)) then
    Result := True
  else
    Result := False;
end;

procedure TOraScriptProcessor.DoBeforeStatementExecute(var SQL: _string; StatementType: integer; var Omit: boolean);
var
  SQLParser: TParser;
  MacroName, MacroVal: _string;
  Code: integer;
  St: _string;
  Macro: TMacro;
  ConnectStr: _string;
  AUsername, APassword, AServer: _string;
  AConnectMode: TConnectMode;

  procedure BypassBlanks;
  begin
    repeat
      Code := SQLParser.GetNext(St);
    until (Code <> lcBlank) or (Code = lcEnd);
  end;

  procedure RaiseException;
  begin
    raise Exception.CreateFmt(SInvalidLexem, [St, SQLParser.CurrPos + 1 - Length(St), SQL]);
  end;

begin
  if Omit then
    Exit;
    
  case StatementType of
    ST_IGNORED:
      Omit := True;

    ST_COMMENT:
      Omit := True;    

    ST_CONNECT: begin
      SQLParser := GetSQLParser(SQL);

      Code := SQLParser.GetNext(St); // lxCONNECT
      BypassBlanks;

      ConnectStr := '';
      while (Code <> lcEnd) do begin
        ConnectStr := ConnectStr + St;
        Code := SQLParser.GetNext(St);
      end;

      ParseConnectString(ConnectStr, AUsername, APassword, AServer, AConnectMode);
      with UsedConnection do begin
        Username := AUsername;
        Password := APassword;
        Server := AServer;
        Connect;
      end;
      Omit := True;
    end;

    ST_DISCONNECT: begin
      UsedConnection.Disconnect;
      Omit := True;
    end;

    ST_DEFINE: begin
      SQLParser := GetSQLParser(SQL);

      Code := SQLParser.GetNext(St); // lxDEFINE
      BypassBlanks;

      if (Code = lcIdent) or (Code = lcNumber) or (Code > SQLParser.SymbolLexems.Count)
        and (Code <= SQLParser.SymbolLexems.Count + SQLParser.KeywordLexems.Count)
      then
        MacroName := St
      else
        RaiseException;

      Code := SQLParser.GetNext(St);

      // for names that begin with number
      if (Code = lcIdent) or (Code > SQLParser.SymbolLexems.Count)
        and (Code <= SQLParser.SymbolLexems.Count + SQLParser.KeywordLexems.Count)
      then begin
        MacroName := MacroName + St;
        Code := SQLParser.GetNext(St);
      end;

      if Code = lcBlank then
        BypassBlanks;

      if St <> '=' then
        RaiseException;

      BypassBlanks;

      if OCISQLInfo.IsQuoted(St) then
        MacroVal := OCISQLInfo.UnQuote(St)
      else begin
        MacroVal := St;
        repeat
          Code := SQLParser.GetNext(St);
          if (Code = lcEnd) or (Code = lcBlank) then
            Break;
          MacroVal := MacroVal + St;
        until False;
      end;

      Macro := FOwner.Macros.FindMacro(MacroName);
      if Macro = nil then begin
        Macro := TMacro(FOwner.Macros.Add);
        Macro.Name := MacroName;
      end;
      Macro.Value := MacroVal;

      Omit := True;
    end;

    ST_EXECUTE: begin
      Delete(SQL, 1, 7);
      SQL := 'BEGIN'#13#10 + SQL + ';'#13#10'END;';
    end;
  end;
end;

end.
