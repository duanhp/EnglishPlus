
unit Fetch;

interface

uses
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, DBCtrls, ExtCtrls,
  Grids, DBGrids, StdCtrls, ToolWin, ComCtrls, UniDacVcl,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, Classes;

type
  TFetchForm = class(TForm)
    Label1: TLabel;
    btCancel: TButton;
    procedure btCancelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FetchForm: TFetchForm;

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

procedure TFetchForm.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFetchForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
{$IFNDEF LINUX}
  if Key = VK_ESCAPE then
    Close;
{$ENDIF}
end;

initialization
{$IFDEF FPC}
{$I Fetch.lrs}
{$ENDIF}

end.
