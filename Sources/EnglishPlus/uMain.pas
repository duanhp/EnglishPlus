unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.ExtCtrls, Vcl.OleCtrls, SHDocVw, Vcl.StdCtrls, Vcl.Buttons,
  Data.Bind.EngExt, Vcl.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.Components;

type
  TFmMain = class(TForm)
    PlCaption: TPanel;
    PlContent: TPanel;
    PLSearch: TPanel;
    Image1: TImage;
    Panel1: TPanel;
    PlLeft: TPanel;
    WebBrowser1: TWebBrowser;
    Image2: TImage;
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
