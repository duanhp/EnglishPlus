{$IFNDEF CLR}

{$I UniDac.inc}

unit UniQueryEditor;
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
  CREditor, CRTabEditor, DASQLComponentEditor, DAQueryEditor, Uni, Db, DBAccess,
  CRFrame, UniQueryOptionsFrame;

type
  TUniQueryEditorForm = class(TDAQueryEditorForm)
    shOptions: TTabSheet;
    btMacros: TButton;
    procedure btMacrosClick(Sender: TObject);

  protected
    FOptionsFrame: TUniQueryOptionsFrame;

    procedure DoInit; override;
    function GetFrameByInitProp: TCRFrame; override;

  public
    property Query;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniQueryEditor.dfm}
{$ENDIF}
{$ENDIF}

uses
  CRColFrame, DASQLFrame, DAParamsFrame, UniParamsFrame, DAMacrosFrame,
  DASPCallFrame, DASQLGeneratorFrame, DAUpdateSQLFrame, UniSPCallFrame,
  UniDesignUtils, UniConnectionEditor;

{ TUniQueryEditorForm }

procedure TUniQueryEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TDASQLFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TUniParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FSPCallFrame := AddTab(TUniSPCallFrame, shGeneratorSPC) as TDASPCallFrame;
  FUpdateSQLFrame := AddTab(TDAUpdateSQLFrame, shEditSQL) as TDAUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TDASQLGeneratorFrame, shGenerator) as TDASQLGeneratorFrame;
  FOptionsFrame := AddTab(TUniQueryOptionsFrame, shOptions) as TUniQueryOptionsFrame;

  btMacros.Enabled := Query.Connection <> nil;

  inherited;
end;

function TUniQueryEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else
    Result := inherited GetFrameByInitProp;
end;

procedure TUniQueryEditorForm.btMacrosClick(Sender: TObject);
var
  CREditor: TCREditorForm;
  mr: integer;
begin
  if Query.Connection <> nil then begin
    CREditor := TUniConnectionEditorForm.Create(nil, TUniDesignUtils);
    try
      CREditor.Component := Query.Connection;
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
{$I UniQueryEditor.lrs}
{$ENDIF}

end.

