unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.ExtCtrls, Vcl.OleCtrls, SHDocVw, Vcl.StdCtrls, Vcl.Buttons;

type
  TFmMain = class(TForm)
    PlCaption: TPanel;
    PlContent: TPanel;
    PLSearch: TPanel;
    WBWdContent: TWebBrowser;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmMain: TFmMain;

implementation

{$R *.dfm}

uses
  uSearchBar;

procedure TFmMain.FormCreate(Sender: TObject);
begin
  FmSearchBar:=TFmSearchBar.Create(nil);
  FmSearchBar.Align:=alClient;
  FmSearchBar.Parent:=Self.PLSearch;
  FmSearchBar.Show;
end;

end.
