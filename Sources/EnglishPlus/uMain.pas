unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.ExtCtrls, Vcl.OleCtrls, SHDocVw, Vcl.StdCtrls, Vcl.Buttons;

type
  TForm1 = class(TForm)
    PlCaption: TPanel;
    PlContent: TPanel;
    PLbtmBar: TPanel;
    PLSearch: TPanel;
    WBWdContent: TWebBrowser;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Edit1: TEdit;
    SpeedButton3: TSpeedButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
