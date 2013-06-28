unit InheritedConnectForm;

interface

uses
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Grids, DBGrids, StdCtrls, ToolWin, ComCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DB, UniConnectForm;
  
type
  TfmInheritedConnect = class(TUniConnectForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmInheritedConnect: TfmInheritedConnect;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ENDIF}
{$IFDEF WIN32}
{$R *.dfm}
{$ENDIF}
{$IFDEF WIN64}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

initialization
  if GetClass('TfmInheritedConnect') = nil then
    Classes.RegisterClass(TfmInheritedConnect);

{$IFDEF FPC}
{$I InheritedConnectForm.lrs}
{$ENDIF}

end.
