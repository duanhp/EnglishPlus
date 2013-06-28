{$IFNDEF CLR}

{$I UniDac.inc}

unit UniScriptEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons,
  SysUtils, DB, Classes,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DBAccess, MemUtils, DAScript,
  CREditor, CRTabEditor, DAScriptEditor, CRFrame, UniSQLOptionsFrame;

type
  TUniScriptEditorForm = class(TDAScriptEditorForm)
    shOptions: TTabSheet;

  protected
    FOptionsFrame: TUniSQLOptionsFrame;

    procedure DoInit; override;
    function GetFrameByInitProp: TCRFrame; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniScriptEditor.dfm}
{$ENDIF}
{$ENDIF}

procedure TUniScriptEditorForm.DoInit;
begin
  inherited;

  FOptionsFrame := AddTab(TUniSQLOptionsFrame, shOptions) as TUniSQLOptionsFrame;
end;

function TUniScriptEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else
    Result := inherited GetFrameByInitProp;
end;

initialization
{$IFDEF FPC}
{$I UniScriptEditor.lrs}
{$ENDIF}

end.
 
