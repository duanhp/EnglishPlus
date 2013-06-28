
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I SQLiteDac.inc}
unit LiteParserUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, CRParser, MemUtils;

type
  TLiteParser = class (TSQLParser)
  protected
    function IsIdentQuote(Ch: _char): boolean; override;
  end;

implementation

function TLiteParser.IsIdentQuote(Ch: _char): boolean;
begin
  Result := False;
  case Ch of
    '`', '"':
      Result := True;
  end;
end;

end.
