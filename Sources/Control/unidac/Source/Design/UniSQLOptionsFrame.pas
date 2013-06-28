
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  UniSQLOptions Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniSQLOptionsFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils, MemUtils, CRFrame, Uni, UniSpecificOptionsFrame;

type
  TUniSQLOptionsFrame = class(TCRFrame)
    pnOptions: TPanel;
  protected
    FOptionsFrame: TUniSpecificOptionsFrame;

    procedure DoActivate; override;
  public
    constructor Create(Owner: TComponent); override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniSQLOptionsFrame.dfm}
{$ENDIF}
{$ENDIF}

uses
  DBAccess, CREditor, DASQLComponentEditor, DAScript, UniScript, UniLoader, UniDump,
  UniAlerter;

constructor TUniSQLOptionsFrame.Create(Owner: TComponent);
begin
  inherited;

  FOptionsFrame := TUniSpecificOptionsFrame.Create(Self);
  FOptionsFrame.Parent := pnOptions;
end;

procedure TUniSQLOptionsFrame.DoActivate;
var
  OptionsType: TOptionsType;
  SpecificOptions: _TStrings;
  Connection: TUniConnection;
begin
  if Editor.LocalComponent is TUniSQL then begin
    Connection := TUniConnection(TDBAccessUtils.UsedConnection(TUniSQL(Editor.LocalComponent)));
    SpecificOptions := TUniSQL(Editor.LocalComponent).SpecificOptions;
    OptionsType := otSQL;
  end
  else
  if Editor.LocalComponent is TCustomUniDataSet then begin
    Connection := TUniConnection(TDBAccessUtils.UsedConnection(TCustomUniDataSet(Editor.LocalComponent)));
    SpecificOptions := TCustomUniDataSet(Editor.LocalComponent).SpecificOptions;
    OptionsType := otDataSet;
  end
  else
  if Editor.LocalComponent is TUniScript then begin
    Connection := TUniConnection(TDAScriptUtils.UsedConnection(TUniScript(Editor.LocalComponent)));
    SpecificOptions := TUniScript(Editor.LocalComponent).SpecificOptions;
    OptionsType := otScript;
  end
  else
  if Editor.LocalComponent is TUniLoader then begin
    Connection := TUniLoader(Editor.LocalComponent).Connection;
    SpecificOptions := TUniLoader(Editor.LocalComponent).SpecificOptions;
    OptionsType := otLoader;
  end
  else
  if Editor.LocalComponent is TUniDump then begin
    Connection := TUniDump(Editor.LocalComponent).Connection;
    SpecificOptions := TUniDump(Editor.LocalComponent).SpecificOptions;
    OptionsType := otDump;
  end
  else
  if Editor.LocalComponent is TUniAlerter then begin
    Connection := TUniAlerter(Editor.LocalComponent).Connection;
    SpecificOptions := TUniAlerter(Editor.LocalComponent).SpecificOptions;
    OptionsType := otAlerter;
  end
  else begin
    Assert(False);
    exit;
  end;

  if (Connection <> nil) then
    FOptionsFrame.LoadOptions(Connection.ProviderName, OptionsType, SpecificOptions)
  else
    FOptionsFrame.LoadOptions('', OptionsType, SpecificOptions);
end;

initialization
{$IFDEF FPC}
{$I UniSQLOptionsFrame.lrs}
{$ENDIF}

end.
