unit About;

interface
uses
  Classes, SysUtils,
{$IFNDEF LINUX}
  Windows, Menus, ImgList, StdCtrls, ComCtrls, Buttons, ExtCtrls, Graphics,
  Controls, Forms, Dialogs, ShellApi;
{$ELSE}
  QMenus, QImgList, QStdCtrls, QComCtrls, QButtons, QExtCtrls, QGraphics,
  QControls, QForms, QDialogs;
{$ENDIF}

type
  TAboutForm = class(TForm)
    OKBtn: TButton;
    Label3: TLabel;
    lbWeb: TLabel;
    lbDemo: TLabel;
    meAbout: TMemo;
    procedure lbWebClick(Sender: TObject);
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
 AboutForm: TAboutForm;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ENDIF}
{$IFDEF WIN32}
{$R *.dfm}
{$ENDIF}
{$IFDEF WIN64}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TAboutForm.lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TAboutForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
end;

procedure TAboutForm.lbWebClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', 'www.devart.com', '', '', SW_SHOW);
  lbWeb.Font.Color := $FF0000;  
{$ENDIF}
end;

end.
