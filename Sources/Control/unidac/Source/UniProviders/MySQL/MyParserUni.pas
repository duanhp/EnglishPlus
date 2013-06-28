
//////////////////////////////////////////////////
//  Data Access Components for MySQL
//  Copyright © 1998-2011 Devart. All right reserved.
//  My Parser
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MyParserUni;
{$ENDIF}

interface
uses
{$IFNDEF CLR}
  CLRClasses, 
{$ENDIF} 
  MemUtils,  CRParser;

const
  lxMyFirst     = 1000;
  lxCHARSET     = lxMyFirst;
  lxCOLUMNS     = lxCHARSET + 1;
  lxCREATE      = lxCOLUMNS + 1;
  lxCROSS       = lxCREATE + 1;
  lxDELIMITER   = lxCROSS + 1;
  lxDESC        = lxDELIMITER + 1;
  lxDESCRIBE    = lxDESC + 1;
  lxEXPLAIN     = lxDESCRIBE + 1;
  lxFORCE       = lxEXPLAIN + 1;
  lxFUNCTION    = lxFORCE + 1;
  lxIGNORE      = lxFUNCTION + 1;
  lxNATURAL     = lxIGNORE + 1;
  lxPRIVILEGES  = lxNATURAL + 1;
  lxPROCEDURE   = lxPRIVILEGES + 1;
  lxREPLACE     = lxPROCEDURE + 1;
  lxSHOW        = lxREPLACE + 1;
  lxSLAVE       = lxSHOW + 1;
  lxSTATUS      = lxSLAVE + 1;
  lxSTRAIGHT_JOIN = lxSTATUS + 1;
  lxTRIGGER     = lxSTRAIGHT_JOIN + 1;
  lxUSE         = lxTRIGGER + 1;
  lxHANDLER     = lxUSE + 1;
  lxDISTINCTROW = lxHANDLER + 1;
  lxENGINES     = lxDISTINCTROW + 1;
  lxPROCESSLIST = lxENGINES + 1;

type
  TMyParser = class (TSQLParser)
  protected
    function IsAlpha(Ch: _char): boolean; override;
    function IsStringQuote(Ch: _char): boolean; override;
    procedure ToRightQuote(RightQuote: _char); override;
 {$IFNDEF CLR}
 {$IFNDEF IS_UNICODE}
    procedure ToRightQuoteA(RightQuote: AnsiChar); override;
 {$ENDIF}
 {$ENDIF}
    function IsIdentQuote(Ch: _char): boolean; override;
    function IsInlineComment(Ch: _char; Pos: integer): boolean; override;

    procedure InitParser; override;
  public
  end;

implementation

uses
  Classes, SysUtils;

var
  MyKeywordLexems: _TStringList;

{ TMyParser }

procedure TMyParser.InitParser;
begin
  inherited;

  FKeywordLexems := MyKeywordLexems;

  CommentBegin := '/*';
  CommentEnd := '*/';

(*
SELECT
    [ALL | DISTINCT | DISTINCTROW ]
      [HIGH_PRIORITY]
      [STRAIGHT_JOIN]
      [SQL_SMALL_RESULT] [SQL_BIG_RESULT] [SQL_BUFFER_RESULT]
      [SQL_CACHE | SQL_NO_CACHE] [SQL_CALC_FOUND_ROWS]
    select_expr, ...
    [INTO OUTFILE 'file_name' export_options
      | INTO DUMPFILE 'file_name']
    [FROM table_references
      [WHERE where_definition]
      [GROUP BY {col_name | expr | position}
        [ASC | DESC], ... [WITH ROLLUP]]
      [HAVING where_definition]
      [ORDER BY {col_name | expr | position}
        [ASC | DESC] , ...]
      [LIMIT {[offset,] row_count | row_count OFFSET offset}]
      [PROCEDURE procedure_name(argument_list)]
      [FOR UPDATE | LOCK IN SHARE MODE]]
*)

  SetLength(FClauses, 8);
  FClauses[0] := lxWHERE;
  FClauses[1] := lxGROUP;
  FClauses[2] := lxHAVING;
  FClauses[3] := lxORDER;
  FClauses[4] := lxLIMIT;
  FClauses[5] := lxPROCEDURE;
  FClauses[6] := lxFOR;
  FClauses[7] := lxLOCK;
end;

function TMyParser.IsAlpha(Ch: _char): boolean;
begin
  Result := (Ch <> '#') and (Ch <> '$') and inherited IsAlpha(Ch);
end;

function TMyParser.IsStringQuote(Ch: _char): boolean;
begin
  Result := (Ch = '''')
    or (Ch = '"' {WAR if not MySQL ANSI mode});
end;

{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
procedure TMyParser.ToRightQuoteA(RightQuote: AnsiChar);
var
  i: integer;
  p: IntPtr;
  Ready: boolean;
begin
  repeat

    i := TextLength - Pos + 1 {Pos is counted from 1} + 1 {#0};
    p := PtrOffset(PChar(FCurrentBlock), Pos - 1);
    asm
    {$IFDEF CPUX64}
      PUSH RAX
      PUSH RCX
      PUSH RDI

      MOV RDI, p;
      MOV ECX, i
      MOV AL, RightQuote
      REPNE   SCASB
      MOV i, ECX

      POP RDI
      POP RCX
      POP RAX
    {$ELSE}
      PUSHAD

      MOV EDI, p;
      MOV ECX, i
      MOV AL, RightQuote
      REPNE   SCASB
      MOV i, ECX

      POPAD
    {$ENDIF}
    end;
    Pos := TextLength - i + 1;
    // Assert(Text[Pos] <> #0);

    Ready := True;
    i := Pos;
    while i > 2 do begin
      Dec(i);
      if Text[i] <> '\' then begin
        // ???.'
        Ready := True;
        Break;
      end;

      // ???\'
      Dec(i);
      if Text[i] <> '\' then begin
        // ??.\'
        Ready := False; // Continue scanning
        Break;
      end;

      // ??\\'
    end;
    if not Ready then
      Inc(Pos);
  until Ready;
end;
{$ENDIF}
{$ENDIF}

procedure TMyParser.ToRightQuote(RightQuote: _char);
var
  c: _char;
begin
{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
  if not FAdvancedStringParsing and (FStream = nil) then begin
    ToRightQuoteA(RightQuote);
    Exit;
  end;
{$ENDIF}
{$ENDIF}

  while Pos <= TextLength do begin
    c := Text[Pos];
    if (c = #13) or (c = #10) then begin
      if (Pos < TextLength) and (Text[Pos + 1] = #10) then
        Inc(Pos);
      Inc(FCurrLine);
      FCurrBegLine := Pos + 1;
    end
    else
    if c = '\' then // Escape character
      Inc(Pos)
    else
    if c = RightQuote then
      Break;
    Inc(Pos);
  end;
end;

function TMyParser.IsIdentQuote(Ch: _char): boolean;
begin
  case Ch of
    '`' {, '"' WAR if MySQL ANSI mode}:
      Result := True;
    else
      Result := False;
  end;
end;

function TMyParser.IsInlineComment(Ch: _char; Pos: integer): boolean;
begin
  Result := Ch = '#';

  if not Result then
    Result := (TextLength >= Pos + 3) and (Ch = '-') and (Text[Pos + 1] = '-') and (Text[Pos + 2] = ' ');
    // Must be '-- ', see http://dev.mysql.com/doc/mysql/en/ansi-diff-comments.html for details
end;

initialization
  MyKeywordLexems := _TStringList.Create;
  MyKeywordLexems.Assign(SQLKeywordLexems);

  MyKeywordLexems.AddObject('CHARSET', TObject(Integer(lxCHARSET)));
  MyKeywordLexems.AddObject('COLUMNS', TObject(Integer(lxCOLUMNS)));
  MyKeywordLexems.AddObject('PRIVILEGES', TObject(Integer(lxPRIVILEGES)));
  MyKeywordLexems.AddObject('PROCEDURE', TObject(Integer(lxPROCEDURE)));
  MyKeywordLexems.AddObject('FUNCTION', TObject(Integer(lxFUNCTION)));
  MyKeywordLexems.AddObject('CREATE', TObject(Integer(lxCREATE)));
  MyKeywordLexems.AddObject('REPLACE', TObject(Integer(lxREPLACE)));
  MyKeywordLexems.AddObject('DELIMITER', TObject(Integer(lxDELIMITER)));
  MyKeywordLexems.AddObject('DESC', TObject(Integer(lxDESC)));
  MyKeywordLexems.AddObject('DESCRIBE', TObject(Integer(lxDESCRIBE)));
  MyKeywordLexems.AddObject('EXPLAIN', TObject(Integer(lxEXPLAIN)));
  MyKeywordLexems.AddObject('USE', TObject(Integer(lxUSE)));
  MyKeywordLexems.AddObject('IGNORE', TObject(Integer(lxIGNORE)));
  MyKeywordLexems.AddObject('FORCE', TObject(Integer(lxFORCE)));
  MyKeywordLexems.AddObject('CROSS', TObject(Integer(lxCROSS)));
  MyKeywordLexems.AddObject('SHOW', TObject(Integer(lxSHOW)));
  MyKeywordLexems.AddObject('SLAVE', TObject(Integer(lxSLAVE)));
  MyKeywordLexems.AddObject('STATUS', TObject(Integer(lxSTATUS)));
  MyKeywordLexems.AddObject('STRAIGHT_JOIN', TObject(Integer(lxSTRAIGHT_JOIN)));
  MyKeywordLexems.AddObject('TRIGGER', TObject(Integer(lxTRIGGER)));
  MyKeywordLexems.AddObject('NATURAL', TObject(Integer(lxNATURAL)));
  MyKeywordLexems.AddObject('HANDLER', TObject(Integer(lxHANDLER)));
  MyKeywordLexems.AddObject('DISTINCTROW', TObject(Integer(lxDISTINCTROW)));
  MyKeywordLexems.AddObject('ENGINES', TObject(Integer(lxENGINES)));
  MyKeywordLexems.AddObject('PROCESSLIST', TObject(Integer(lxPROCESSLIST)));

  MyKeywordLexems.CustomSort(CRCmpStrings);

finalization
  MyKeywordLexems.Free;

end.
