{$IFNDEF CLR}

{$I AccessDac.inc}
unit AccessParserUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, CRParser, MemUtils;

type
  TAccessParser = class (TSQLParser)
  protected
    function IsIdentQuote(Ch: _char): boolean; override;
    procedure ToRightQuote(LeftQuote: _char); override;
  end;

implementation

function TAccessParser.IsIdentQuote(Ch: _char): boolean;
begin
  case Ch of
    '"', '[', ']':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TAccessParser.ToRightQuote(LeftQuote: _char);
begin
  if LeftQuote = '[' then
    inherited ToRightQuote(']')
  else
    inherited;
end;

end.
