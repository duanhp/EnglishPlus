{$IFNDEF CLR}

{$I UniDac.inc}

unit UniSQLEditor;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, DBCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Db, DBAccess, Uni, CREditor, DASQLComponentEditor, CRFrame, DASQLFrame, UniSQLOptionsFrame;

type
  TUniSQLEditorForm = class(TDASQLEditorForm)
    shOptions: TTabSheet;
    btMacros: TButton;
    procedure btMacrosClick(Sender: TObject);

  protected
    FOptionsFrame: TUniSQLOptionsFrame;

    procedure DoInit; override;
    function GetFrameByInitProp: TCRFrame; override;

  public
    property SQL;

  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniSQLEditor.dfm}
{$ENDIF}
{$ENDIF}

uses
  DAParamsFrame, DAMacrosFrame, DASPCallFrame, UniParamsFrame, UniSPCallFrame,
  UniDesignUtils, UniConnectionEditor;

{ TUniSQLEditorForm }

procedure TUniSQLEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TDASQLFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TUniParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FSPCallFrame := AddTab(TUniSPCallFrame, shGeneratorSPC) as TDASPCallFrame;
  FOptionsFrame := AddTab(TUniSQLOptionsFrame, shOptions) as TUniSQLOptionsFrame;

  btMacros.Enabled := SQL.Connection <> nil;

  inherited;
end;

function TUniSQLEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else
    Result := inherited GetFrameByInitProp;
end;

procedure TUniSQLEditorForm.btMacrosClick(Sender: TObject);
var
  CREditor: TCREditorForm;
  mr: integer;
begin
  if SQL.Connection <> nil then begin
    CREditor := TUniConnectionEditorForm.Create(nil, TUniDesignUtils);
    try
      CREditor.Component := SQL.Connection;
      TCREditorForm(CREditor).InitialProperty := 'Macros';

      mr := CREditor.ShowModal;
      if mr = mrOk then
        Modified := True;
    finally
      CREditor.Free;
    end;
  end;
end;

initialization
{$IFDEF FPC}
{$I UniSQLEditor.lrs}
{$ENDIF}

end.

