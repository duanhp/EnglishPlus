
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  Ora Parser
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}
unit OraParserUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, CRParser, MemUtils;

var
  OraKeywordLexems: _TStringList;

const
  lxOraFirst  = 1000;
  lxALTER     = lxOraFirst;
  lxBODY      = lxALTER + 1;
  lxCONNECT   = lxBODY + 1;
  lxCREATE    = lxCONNECT + 1;
  lxDECLARE   = lxCREATE + 1;
  lxDEFINE    = lxDECLARE + 1;
  lxDISCONNECT = lxDEFINE + 1;
  lxEND       = lxDISCONNECT + 1;
  lxEXIT      = lxEND + 1;
  lxEXPLAIN   = lxEXIT + 1;
  lxFUNCTION  = lxEXPLAIN + 1;
  lxJAVA      = lxFUNCTION + 1;
  lxPACKAGE   = lxJAVA + 1;
  lxPARTITION = lxPACKAGE + 1;
  lxPAUSE     = lxPARTITION + 1;
  lxPROCEDURE = lxPAUSE + 1;
  lxPROMPT    = lxPROCEDURE + 1;
  lxQUIT      = lxPROMPT + 1;
  lxREM       = lxQUIT + 1;
  lxREMARK    = lxREM + 1;
  lxREPLACE   = lxREMARK + 1;
  lxSTART     = lxREPLACE + 1;
  lxTRIGGER   = lxSTART + 1;
  lxTYPE      = lxTRIGGER + 1;
  lxUNDEFINE  = lxTYPE + 1;
  lxUNIQUE    = lxUNDEFINE + 1;
  lxWITH      = lxUNIQUE + 1;
  lxWRAPPED   = lxWITH + 1;

type
  TOraParser = class (TSQLParser)
  private
    FInWrapped: boolean;

  protected
    procedure InitParser; override;

  public
    function GetNext(out Lexem: _string): integer; override;
    function IsMacroAllowed(Code: integer): boolean; override;
    class function IsNumericMacroNameAllowed: boolean; override;
    class function IsSelectModifier(Code: integer): boolean; override;
    class function IsFunctionOrConst(const UpperedName: _string): boolean; override;
    class function IsQuasiColumn(const UpperedName: _string): boolean; override;
  end;

implementation

{ TOraParser }

procedure TOraParser.InitParser;
begin
  inherited;

  FKeywordLexems := OraKeywordLexems;
  CommentBegin := '/*';
  CommentEnd := '*/';
  AlternativeQuoting := True;

  SetLength(FClauses, 10);
  FClauses[0] := lxWHERE;
  FClauses[1] := lxSTART;
  FClauses[2] := lxCONNECT;
  FClauses[3] := lxGROUP;
  FClauses[4] := lxHAVING;
  FClauses[5] := lxUNION;
  FClauses[6] := lxINTERSECT;
  FClauses[7] := lxMINUS;
  FClauses[8] := lxORDER;
  FClauses[9] := lxFOR; 
end;

function TOraParser.GetNext(out Lexem: _string): integer;
begin
  Result := inherited GetNext(Lexem);
  if Result = lxWRAPPED then
    FInWrapped := True
  else
    if FInWrapped and (Lexem = '/') and (PrevCol = 0) then
      FInWrapped := False;
end;

function TOraParser.IsMacroAllowed(Code: integer): boolean;
begin
  if FInWrapped then
    Result := False
  else
    Result := inherited IsMacroAllowed(Code);
end;

class function TOraParser.IsNumericMacroNameAllowed: boolean;
begin
  Result := True;
end;

class function TOraParser.IsSelectModifier(Code: integer): boolean;
begin
  Result := inherited IsSelectModifier(Code) or (Code = lxUNIQUE);
end;

class function TOraParser.IsFunctionOrConst(const UpperedName: _string): boolean;
begin
  if inherited IsFunctionOrConst(UpperedName) or
    (UpperedName = 'SYSDATE') or
    (UpperedName = 'USER') or
    (UpperedName = 'UID')
  then
    Result := True
  else
    Result := False;
end;

class function TOraParser.IsQuasiColumn(const UpperedName: _string): boolean;
begin
  if UpperedName = 'ROWID' then
    Result := True
  else
    Result := False;
end;

initialization
  OraKeywordLexems := _TStringList.Create;
  OraKeywordLexems.Assign(SQLKeywordLexems);

  OraKeywordLexems.AddObject('ALTER',     TObject(Integer(lxALTER    )));
  OraKeywordLexems.AddObject('BODY',      TObject(Integer(lxBODY     )));
  OraKeywordLexems.AddObject('CONNECT',   TObject(Integer(lxCONNECT  )));
  OraKeywordLexems.AddObject('CREATE',    TObject(Integer(lxCREATE   )));
  OraKeywordLexems.AddObject('DECLARE',   TObject(Integer(lxDECLARE  )));
  OraKeywordLexems.AddObject('DEFINE',    TObject(Integer(lxDEFINE   )));
  OraKeywordLexems.AddObject('DISCONNECT', TObject(Integer(lxDISCONNECT)));
  OraKeywordLexems.AddObject('END',       TObject(Integer(lxEND      )));
  OraKeywordLexems.AddObject('EXIT',      TObject(Integer(lxEXIT     )));
  OraKeywordLexems.AddObject('EXPLAIN',   TObject(Integer(lxEXPLAIN  )));
  OraKeywordLexems.AddObject('FUNCTION',  TObject(Integer(lxFUNCTION )));
  OraKeywordLexems.AddObject('JAVA',      TObject(Integer(lxJAVA     )));
  OraKeywordLexems.AddObject('PACKAGE',   TObject(Integer(lxPACKAGE  )));
  OraKeywordLexems.AddObject('PAUSE',     TObject(Integer(lxPAUSE    )));
  OraKeywordLexems.AddObject('PARTITION', TObject(Integer(lxPARTITION)));
  OraKeywordLexems.AddObject('PROCEDURE', TObject(Integer(lxPROCEDURE)));
  OraKeywordLexems.AddObject('PROMPT',    TObject(Integer(lxPROMPT   )));
  OraKeywordLexems.AddObject('QUIT',      TObject(Integer(lxQUIT     )));
  OraKeywordLexems.AddObject('REM',       TObject(Integer(lxREM      )));
  OraKeywordLexems.AddObject('REMARK',    TObject(Integer(lxREMARK   )));
  OraKeywordLexems.AddObject('REPLACE',   TObject(Integer(lxREPLACE  )));
  OraKeywordLexems.AddObject('START',     TObject(Integer(lxSTART    )));
  OraKeywordLexems.AddObject('TRIGGER',   TObject(Integer(lxTRIGGER  )));
  OraKeywordLexems.AddObject('TYPE',      TObject(Integer(lxTYPE     )));
  OraKeywordLexems.AddObject('UNDEFINE',  TObject(Integer(lxUNDEFINE )));
  OraKeywordLexems.AddObject('WITH',      TObject(Integer(lxWITH     )));
  OraKeywordLexems.AddObject('WRAPPED',   TObject(Integer(lxWRAPPED  )));
  OraKeywordLexems.CustomSort(CRCmpStrings);

finalization
  OraKeywordLexems.Free;
end.
