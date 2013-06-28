
unit UniDacAbout;

interface
uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
{$IFNDEF LINUX}
  Windows, ShellAPI,
{$ENDIF}
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TUniDacAboutForm = class(TForm)
    OKBtn: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbMail: TLabel;
    lbWeb: TLabel;
    Label4: TLabel;
    Bevel1: TBevel;
    Label10: TLabel;
    lbForum: TLabel;
    Bevel2: TBevel;
    procedure lbWebClick(Sender: TObject);
    procedure lbMailClick(Sender: TObject);
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbMailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbForumMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbForumClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UniDacAboutForm: TUniDacAboutForm;

procedure ShowAbout;

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

procedure ShowAbout;
begin
  with TUniDacAboutForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TUniDacAboutForm.lbWebClick(Sender: TObject);
begin
{$IFNDEF LINUX}
  ShellExecute(0, 'open', {$IFNDEF CLR}PChar{$ENDIF}('http://www.devart.com/unidac'), '', '', SW_SHOW);
  lbWeb.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TUniDacAboutForm.lbMailClick(Sender: TObject);
begin
{$IFNDEF LINUX}
  ShellExecute(0, 'open', {$IFNDEF CLR}PChar{$ENDIF}('mailto:unidac@devart.com'), 'zxczxc', '', SW_SHOW);
  lbMail.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TUniDacAboutForm.lbForumClick(Sender: TObject);
begin
{$IFNDEF LINUX}
  ShellExecute(0, 'open', {$IFNDEF CLR}PChar{$ENDIF}('www.devart.com/forums/viewforum.php?f=28'), '', '', SW_SHOW);
  lbWeb.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TUniDacAboutForm.lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TUniDacAboutForm.lbMailMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  lbMail.Font.Color := $4080FF;
end;

procedure TUniDacAboutForm.lbForumMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbForum.Font.Color := $4080FF;
end;

procedure TUniDacAboutForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
  lbMail.Font.Color := $FF0000;
  lbForum.Font.Color := $FF0000;
end;

{$IFDEF FPC}
initialization
  {$i UniDacAbout.lrs}
{$ENDIF}

end.
