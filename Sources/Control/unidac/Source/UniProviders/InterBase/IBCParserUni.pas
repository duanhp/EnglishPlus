
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  IBC Parser
//////////////////////////////////////////////////


{$IFNDEF CLR}
unit IBCParserUni;
{$ENDIF}

interface
uses
  Classes, MemUtils, CRParser;

var
  IBCKeywordLexems: _TStringList;

const
  lxIBCFirst  = 1000;
  lxALTER     = lxIBCFirst;
  lxAUTODDL   = lxALTER + 1;
  lxBATCH     = lxAUTODDL + 1;
  lxBLOCK     = lxBATCH + 1;
  lxCONNECT   = lxBLOCK + 1;
  lxCREATE    = lxCONNECT + 1;
  lxDATABASE  = lxCREATE + 1;
  lxDECLARE   = lxDATABASE + 1;
  lxDIALECT   = lxDECLARE + 1;
  lxDROP      = lxDIALECT + 1;
  lxFUNCTION  = lxDROP + 1;
  lxGENERATOR = lxFUNCTION + 1;
  lxGRANT     = lxGENERATOR + 1;
  lxNAMES     = lxGRANT + 1;
  lxPLAN      = lxNAMES + 1;
  lxPROCEDURE = lxPLAN + 1;
  lxRECONNECT = lxPROCEDURE + 1;
  lxRETAIN    = lxRECONNECT + 1;
  lxREVOKE    = lxRETAIN + 1;
  lxROWS      = lxREVOKE + 1;
  lxSCHEMA    = lxROWS + 1;
  lxSQL       = lxSCHEMA + 1;
  lxSTART     = lxSQL + 1;
  lxTERM      = lxSTART + 1;
  lxTRIGGER   = lxTERM + 1;
  lxTYPE      = lxTRIGGER + 1;
  lxWITH      = lxTYPE + 1;

type
  TIBCParser = class (TSQLParser)
  protected
    procedure InitParser; override;

  public
  end;

implementation
uses
  SysUtils;

{ TIBCParser }

procedure TIBCParser.InitParser;
begin
  inherited;

  FKeywordLexems := IBCKeywordLexems;
  CommentBegin := '/*';
  CommentEnd := '*/';

(*  SELECT [TRANSACTION transaction]
    [DISTINCT | ALL]
    {* | val [, val …]}
    [INTO :var [, :var …]]
    FROM tableref [, tableref …]
    [WHERE search_condition]
    [GROUP BY col [COLLATE collation] [, col [COLLATE collation] …]
    [HAVING search_condition]
    [UNION [ALL] select_expr]
    [PLAN plan_expr]
    [ORDER BY order_list]
    [ROWS value [TO upper_value] [BY step_value][PERCENT][WITH TIES]]
    [FOR UPDATE [OF col [, col …]]];*)


  SetLength(FClauses, 8);
  FClauses[0] := lxWHERE;
  FClauses[1] := lxGROUP;
  FClauses[2] := lxHAVING;
  FClauses[3] := lxUNION;
  FClauses[4] := lxPLAN;
  FClauses[5] := lxORDER;
  FClauses[6] := lxROWS;
  FClauses[7] := lxFOR;
end;

initialization
  IBCKeywordLexems := _TStringList.Create;
  IBCKeywordLexems.Assign(SQLKeywordLexems);

  IBCKeywordLexems.AddObject('ALTER',     TObject(Integer(lxALTER    )));
  IBCKeywordLexems.AddObject('AUTODDL',   TObject(Integer(lxAUTODDL  )));
  IBCKeywordLexems.AddObject('BATCH',     TObject(Integer(lxBATCH    )));
  IBCKeywordLexems.AddObject('BLOCK',     TObject(Integer(lxBLOCK    )));
  IBCKeywordLexems.AddObject('CONNECT',   TObject(Integer(lxCONNECT  )));
  IBCKeywordLexems.AddObject('CREATE',    TObject(Integer(lxCREATE   )));
  IBCKeywordLexems.AddObject('DATABASE',  TObject(Integer(lxDATABASE )));
  IBCKeywordLexems.AddObject('DECLARE',   TObject(Integer(lxDECLARE  )));
  IBCKeywordLexems.AddObject('DIALECT',   TObject(Integer(lxDIALECT  )));
  IBCKeywordLexems.AddObject('DROP',      TObject(Integer(lxDROP     )));
  IBCKeywordLexems.AddObject('FUNCTION',  TObject(Integer(lxFUNCTION )));
  IBCKeywordLexems.AddObject('GENERATOR', TObject(Integer(lxGENERATOR)));
  IBCKeywordLexems.AddObject('GRANT',     TObject(Integer(lxGRANT    )));
  IBCKeywordLexems.AddObject('NAMES',     TObject(Integer(lxNAMES    )));
  IBCKeywordLexems.AddObject('PLAN',      TObject(Integer(lxPLAN     )));
  IBCKeywordLexems.AddObject('PROCEDURE', TObject(Integer(lxPROCEDURE)));
  IBCKeywordLexems.AddObject('RECONNECT', TObject(Integer(lxRECONNECT)));
  IBCKeywordLexems.AddObject('RETAIN',    TObject(Integer(lxRETAIN   )));
  IBCKeywordLexems.AddObject('REVOKE',    TObject(Integer(lxREVOKE   )));
  IBCKeywordLexems.AddObject('ROWS',      TObject(Integer(lxROWS     )));
  IBCKeywordLexems.AddObject('SCHEMA',    TObject(Integer(lxSCHEMA   )));
  IBCKeywordLexems.AddObject('SQL',       TObject(Integer(lxSQL      )));
  IBCKeywordLexems.AddObject('START',     TObject(Integer(lxSTART    )));
  IBCKeywordLexems.AddObject('TERM',      TObject(Integer(lxTERM     )));
  IBCKeywordLexems.AddObject('TRIGGER',   TObject(Integer(lxTRIGGER  )));
  IBCKeywordLexems.AddObject('TYPE',      TObject(Integer(lxTYPE     )));
  IBCKeywordLexems.AddObject('WITH',      TObject(Integer(lxWITH     )));
  IBCKeywordLexems.CustomSort(CRCmpStrings);

finalization
  IBCKeywordLexems.Free;
end.
