
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  UniQueryOptions Editor Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniQueryOptionsFrame;
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
  Classes, SysUtils, CRFrame, Uni, UniSQLOptionsFrame;

type
  TUniQueryOptionsFrame = class(TUniSQLOptionsFrame)
    Label5: TLabel;
    cbLock1: TComboBox;
    lbUpdatingTable: TLabel;
    edUpdatingTable: TEdit;
    lbKeyFieldsLabel1: TLabel;
    edKeyFields: TEdit;
    procedure cbLock1Change(Sender: TObject);
    procedure edKeyFieldsExit(Sender: TObject);
    procedure edUpdatingTableExit(Sender: TObject);
  protected
    function GetLocalQuery: TUniQuery;
    procedure DoActivate; override;
    property LocalQuery: TUniQuery read GetLocalQuery;
  public
    function ActiveControl: TWinControl; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniQueryOptionsFrame.dfm}
{$ENDIF}
{$ENDIF}

uses
  DBAccess, CREditor, UniQueryEditor;

function TUniQueryOptionsFrame.GetLocalQuery: TUniQuery;
begin
  Result := nil;
  if FEditor is TUniQueryEditorForm then begin
    Result := TUniQueryEditorForm(FEditor).LocalComponent as TUniQuery;
    Assert(Result <> nil);
  end
  else
    Assert(False);
end;

function TUniQueryOptionsFrame.ActiveControl: TWinControl;
begin
  Result := cbLock1;
end;

procedure TUniQueryOptionsFrame.DoActivate;
begin
  inherited;

  cbLock1.ItemIndex := Integer(LocalQuery.LockMode);
  edUpdatingTable.Text := TDBAccessUtils.GetUpdatingTable(LocalQuery);
  edKeyFields.Text := LocalQuery.KeyFields;
end;

procedure TUniQueryOptionsFrame.cbLock1Change(Sender: TObject);
begin
  LocalQuery.LockMode := TLockMode(cbLock1.ItemIndex);
end;

procedure TUniQueryOptionsFrame.edKeyFieldsExit(Sender: TObject);
begin
  try
    LocalQuery.KeyFields := edKeyFields.Text;
  finally
    edKeyFields.Text := LocalQuery.KeyFields;
  end;
end;

procedure TUniQueryOptionsFrame.edUpdatingTableExit(Sender: TObject);
begin
  TDBAccessUtils.SetUpdatingTable(LocalQuery, edUpdatingTable.Text);
end;

initialization
{$IFDEF FPC}
{$I UniQueryOptionsFrame.lrs}
{$ENDIF}

end.
