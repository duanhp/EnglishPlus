
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  UniTableSQL Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniTableSQLFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls,
  Classes, SysUtils,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DBAccess, CRFrame, CRTabEditor, CREditor, DATableSQLFrame;

type
  TUniTableSQLFrame = class(TDATableSQLFrame)
    cbAllTables: TCheckBox;
    procedure cbAllTablesClick(Sender: TObject);
  protected
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniTableSQLFrame.dfm}
{$ENDIF}
{$ENDIF}

uses
  Uni;

{ TUniTableSQLFrame }

procedure TUniTableSQLFrame.cbAllTablesClick(Sender: TObject);
begin
  FListGot := False;
  FAllTables := cbAllTables.Checked;
end;

initialization
{$IFDEF FPC}
{$I UniTableSQLFrame.lrs}
{$ENDIF}

end.

