unit UpdateAction;

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type
  TUpdateActionForm = class(TForm)
    Panel1: TPanel;
    Panel5: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lbField: TLabel;
    lbMessage: TLabel;
    Panel7: TPanel;
    rgAction: TRadioGroup;
    Panel4: TPanel;
    rgKind: TRadioGroup;
    Panel9: TPanel;
    Panel2: TPanel;
    btOk: TSpeedButton;
    procedure btOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UpdateActionForm: TUpdateActionForm;

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

procedure TUpdateActionForm.btOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

{$IFDEF FPC}
initialization
  {$i UpdateAction.lrs}
{$ENDIF}

end.
