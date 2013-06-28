
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAParamValueEditor;
{$ENDIF}

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFNDEF KYLIX}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,
{$ELSE}
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  QExtCtrls, QComCtrls, QButtons,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DB, CREditor;

type
  TDAParamValueEditor = class(TCREditorForm)
    Memo: TMemo;
    //udIndex: TUpDown;

    procedure MemoChange(Sender: TObject);

  protected
    function GetValue: string;
    procedure SetValue(Value: string);
    
    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;

  public
    property Value: string read GetValue write SetValue;
  end;

implementation

{$IFDEF VER6P}
uses
  Variants;
{$ENDIF}

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DAParamValueEditor.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}
{$ENDIF}

function TDAParamValueEditor.GetValue: string;
begin
  Result := Memo.Lines.Text;
end;

procedure TDAParamValueEditor.SetValue(Value: string);
begin
  Memo.Lines.Text := Value;
  Modified := False;
end;

procedure TDAParamValueEditor.MemoChange(Sender: TObject);
begin
  inherited;
  Modified := True;
end;

function TDAParamValueEditor.GetComponent: TComponent;
begin
  Result := nil;
end;

procedure TDAParamValueEditor.SetComponent(Value: TComponent);
begin
end; 

{$IFDEF FPC}
initialization
  {$i DAParamValueEditor.lrs}
{$ENDIF}
end.
