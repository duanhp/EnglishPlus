{$IFNDEF CLR}

{$I UniDac.inc}

unit UniStoredProcEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, DB, Classes, DBAccess, DAStoredProcEditor, CRFrame, UniSQLOptionsFrame;

type
  TUniStoredProcEditorForm = class(TDAStoredProcEditorForm)
    shOptions: TTabSheet;

  protected
    FOptionsFrame: TUniSQLOptionsFrame;

    procedure DoInit; override;
    function GetFrameByInitProp: TCRFrame; override;

  public
    property StoredProc;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniStoredProcEditor.dfm}
{$ENDIF}
{$ENDIF}

uses
  DASQLFrame, DAPAramsFrame, DAMacrosFrame, DAUpdateSQLFrame, DASQLGeneratorFrame,
  DASPCallFrame, UniParamsFrame, UniSPCallFrame;

{ TUniStoredProcEditorForm }

procedure TUniStoredProcEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TUniSPCallFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TUniParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FSPCallFrame := AddTab(TUniSPCallFrame, shGeneratorSPC) as TDASPCallFrame;
  FUpdateSQLFrame := AddTab(TDAUpdateSQLFrame, shEditSQL) as TDAUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TDASQLGeneratorFrame, shGenerator) as TDASQLGeneratorFrame;
  FOptionsFrame := AddTab(TUniSQLOptionsFrame, shOptions) as TUniSQLOptionsFrame;
  shGenerator.TabVisible := False;

  inherited;
end;

function TUniStoredProcEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else
    Result := inherited GetFrameByInitProp;
end;

initialization
{$IFDEF FPC}
{$I UniStoredProcEditor.lrs}
{$ENDIF}

end.
