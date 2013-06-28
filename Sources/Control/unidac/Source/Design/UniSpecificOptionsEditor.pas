
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}
unit UniSpecificOptionsEditor;
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
  DB, DBAccess, Uni, CREditor, CRTabEditor, CRFrame, UniSQLOptionsFrame;

type
  TUniSpecificOptionsEditorForm = class(TCRTabEditorForm)
    shOptions: TTabSheet;

  protected
    FLocalComponent, FComponent: TComponent;
    FOptionsFrame: TUniSQLOptionsFrame;

    procedure DoInit; override;
    procedure DoFinish; override;
    procedure DoSave; override;
    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetLocalComponent: TComponent; override;
    function GetFrameByInitProp: TCRFrame; override;
  public
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniSpecificOptionsEditor.dfm}
{$ENDIF}
{$ENDIF}

{ TUniSpecificOptionsEditorForm }

procedure TUniSpecificOptionsEditorForm.DoInit;
begin
  inherited;

  FLocalComponent := TComponentClass(FComponent.ClassType).Create(nil);
  //TDBAccessUtils.SetDesigning(FLocalComponent, csDesigning in FComponent.ComponentState);
  FLocalComponent.Assign(FComponent);
  FCRDesignUtilsClass.SetDesignCreate(FLocalComponent, True);

  FOptionsFrame := AddTab(TUniSQLOptionsFrame, shOptions) as TUniSQLOptionsFrame;
end;

procedure TUniSpecificOptionsEditorForm.DoFinish;
begin
  FLocalComponent.Free;
  FLocalComponent := nil;
  inherited;
end;

procedure TUniSpecificOptionsEditorForm.DoSave;
begin
  inherited;
  FComponent.Assign(FLocalComponent);
end;

function TUniSpecificOptionsEditorForm.GetComponent: TComponent;
begin
  Result := FComponent;
end;

procedure TUniSpecificOptionsEditorForm.SetComponent(Value: TComponent);
begin
  FComponent := Value;
end;

function TUniSpecificOptionsEditorForm.GetLocalComponent: TComponent;
begin
  Result := FLocalComponent;
end;

function TUniSpecificOptionsEditorForm.GetFrameByInitProp: TCRFrame;
begin
  Result := FOptionsFrame;
end;

initialization
{$IFDEF FPC}
{$I UniSpecificOptionsEditor.lrs}
{$ENDIF}

end.
