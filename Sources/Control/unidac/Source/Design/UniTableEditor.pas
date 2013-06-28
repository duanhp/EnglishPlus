{$IFNDEF CLR}

{$I UniDac.inc}

unit UniTableEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DATableEditor, DATableSQLFrame, DBAccess, Uni, CRFrame, UniTableSQLFrame, UniSQLOptionsFrame;

type
  TUniTableEditorForm = class(TDATableEditorForm)
    shOptions: TTabSheet;

  protected
    FOptionsFrame: TUniSQLOptionsFrame;

    procedure DoInit; override;
    function GetSQLFrameClass: TDATableSQLFrameClass; override;
    function GetFrameByInitProp: TCRFrame; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniTableEditor.dfm}
{$ENDIF}
{$ENDIF}

{ TUniTableEditorForm }

procedure TUniTableEditorForm.DoInit;
begin
  inherited;

  FOptionsFrame := AddTab(TUniSQLOptionsFrame, shOptions) as TUniSQLOptionsFrame;
end;

function TUniTableEditorForm.GetSQLFrameClass: TDATableSQLFrameClass;
begin
  Result := TUniTableSQLFrame;
end;

function TUniTableEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else
    Result := inherited GetFrameByInitProp;
end;

initialization
{$IFDEF FPC}
{$I UniTableEditor.lrs}
{$ENDIF}

end.
