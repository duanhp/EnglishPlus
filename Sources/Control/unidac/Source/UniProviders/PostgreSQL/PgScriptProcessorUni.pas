
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgScriptProcessorUni;

{$ENDIF}

interface

uses
  Classes, SysUtils, MemUtils, DBAccess, DAScript, CRParser;

type
  TPgScriptProcessor = class (TDAScriptProcessor)
  protected
    function GetParserClass: TSQLParserClass; override;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  PgParser;
{$ELSE}
  PgParserUni;
{$ENDIF}

{ TPgScriptProcessor }

function TPgScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TPgParser;
end;

end.
