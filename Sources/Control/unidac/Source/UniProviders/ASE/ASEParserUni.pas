
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ASEDac.inc}
unit ASEParserUni;

{$ENDIF}

interface

uses
  Classes, MemUtils, CRParser;

const
  lxASEFirst = 1000;
  lxGO   = lxASEFirst;

type
  TASEParser = class(TSQLParser)
  protected
    procedure InitParser; override;
  end;

implementation

var
  ASEKeywordLexems: _TStringList;

{ TASEParser }

procedure TASEParser.InitParser;
begin
  inherited;

  FKeywordLexems := ASEKeywordLexems;
end;

initialization
  ASEKeywordLexems := _TStringList.Create;
  ASEKeywordLexems.Assign(SQLKeywordLexems);
  ASEKeywordLexems.AddObject('GO',    TObject(Integer(lxGO)));
  ASEKeywordLexems.CustomSort(CRCmpStrings);

finalization
  ASEKeywordLexems.Free;

end.
