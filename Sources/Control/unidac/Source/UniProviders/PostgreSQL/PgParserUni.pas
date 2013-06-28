//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgParserUni;

{$ENDIF}

interface

uses
  Classes, MemUtils, CRParser;

const
  lxPgFirst  = 1000;
  lxEXCEPT   = lxPgFirst;

type
  TPgParser = class(TSQLParser)
  protected
    procedure InitParser; override;
    procedure ToRightQuoteP(RightQuote: _char); override;
{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
    procedure ToRightQuoteA(RightQuote: AnsiChar); override;
{$ENDIF}
{$ENDIF}
  end;

implementation

var
  PgKeywordLexems: _TStringList;

{ TPgParser }

procedure TPgParser.InitParser;
begin
  inherited;

  FKeywordLexems := PgKeywordLexems;
  CommentBegin := '/*';
  CommentEnd := '*/';
  DollarQuoting := True;

(*
    SELECT [ ALL | DISTINCT [ ON ( expression [, ...] ) ] ]
    * | expression [ AS output_name ] [, ...]
    [ FROM from_item [, ...] ]
    [ WHERE condition ]
    [ GROUP BY expression [, ...] ]
    [ HAVING condition [, ...] ]
    [ { UNION | INTERSECT | EXCEPT } [ ALL ] select ]
    [ ORDER BY expression [ ASC | DESC | USING operator ] [, ...] ]
    [ LIMIT { count | ALL } ]
    [ OFFSET start ]
    [ FOR { UPDATE | SHARE } [ OF table_name [, ...] ] [ NOWAIT ] [...] ]


    where from_item can be one of:

    [ ONLY ] table_name [ * ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    ( select ) [ AS ] alias [ ( column_alias [, ...] ) ]
    function_name ( [ argument [, ...] ] ) [ AS ] alias [ ( column_alias [, ...] | column_definition [, ...] ) ]
    function_name ( [ argument [, ...] ] ) AS ( column_definition [, ...] )
    from_item [ NATURAL ] join_type from_item [ ON join_condition | USING ( join_column [, ...] ) ]
*)

  SetLength(FClauses, 10);
  FClauses[0] := lxWHERE;
  FClauses[1] := lxGROUP;
  FClauses[2] := lxHAVING;
  FClauses[3] := lxUNION;     // UNION/INTERSECT/EXCEPT
  FClauses[4] := lxINTERSECT; // UNION/INTERSECT/EXCEPT
  FClauses[5] := lxEXCEPT;    // UNION/INTERSECT/EXCEPT
  FClauses[6] := lxORDER;
  FClauses[7] := lxLIMIT;
  FClauses[8] := lxOFFSET;
  FClauses[9] := lxFOR;
end;

procedure TPgParser.ToRightQuoteP(RightQuote: _char);
begin
  // for strings with escape symbols: E'STEPHEN O\'KEEFE'
  if (Pos >= 3) and (Text[Pos - 2] = 'E') then begin
    while Pos <= TextLength do begin
      inherited ToRightQuoteP(RightQuote);
      if Text[Pos - 1] = '\' then
        Inc(Pos)
      else
        break;
    end;
  end
  else
    inherited ToRightQuoteP(RightQuote);
end;

{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
procedure TPgParser.ToRightQuoteA(RightQuote: AnsiChar);
begin
  // for strings with escape symbols: E'STEPHEN O\'KEEFE'
  if (Pos >= 3) and (Text[Pos - 2] = 'E') then begin
    while Pos <= TextLength do begin
      inherited ToRightQuoteA(RightQuote);
      if Text[Pos - 1] = '\' then
        Inc(Pos)
      else
        break;
    end;
  end
  else
    inherited ToRightQuoteA(RightQuote);
end;
{$ENDIF}
{$ENDIF}

initialization
  PgKeywordLexems := _TStringList.Create;
  PgKeywordLexems.Assign(SQLKeywordLexems);
  PgKeywordLexems.AddObject('EXCEPT',    TObject(Integer(lxEXCEPT)));
  PgKeywordLexems.AddObject('ONLY',      TObject(Integer(lxONLY)));
  PgKeywordLexems.CustomSort(CRCmpStrings);

finalization
  PgKeywordLexems.Free;

end.
