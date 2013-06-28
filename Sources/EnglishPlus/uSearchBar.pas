unit uSearchBar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TSearchWordEevet=procedure(AWord:string) of object;
 //
  TFmSearchBar = class(TForm)
    EdSearchText: TEdit;
    BtnGo: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EdSearchTextKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BtnGoClick(Sender: TObject);
  private
    { Private declarations }
    FHisResult:TStringList;
    FCurrPos:Integer;
    FOnSearchWord: TSearchWordEevet;
    procedure SelectWord(SelWord:string);
  public
    { Public declarations }
    property OnSearchWord:TSearchWordEevet  read FOnSearchWord write FOnSearchWord;
  end;

var
  FmSearchBar: TFmSearchBar;

implementation

{$R *.dfm}

uses
  uSearchList;

procedure TFmSearchBar.BtnGoClick(Sender: TObject);
begin
  //..
  if Assigned(FOnSearchWord) then
    FOnSearchWord(Self.EdSearchText.Text);
end;

procedure TFmSearchBar.EdSearchTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AObj:TFmSearchList;
  P:TPoint;
begin
  AObj:=GetSearchListobj();
  AObj.SearchWord(Trim(EdSearchText.Text));
  P.X:=EdSearchText.Left;
  p.Y:=EdSearchText.Top;
  p:=Self.ClientToScreen(p);
  AObj.Left:=p.X;
  AObj.Top:=p.Y+EdSearchText.Height;
  AObj.Width:=EdSearchText.Width;
  AObj.Show;
  EdSearchText.SetFocus;
end;

procedure TFmSearchBar.FormCreate(Sender: TObject);
begin
  FHisResult:=TStringList.Create;
end;

procedure TFmSearchBar.FormDestroy(Sender: TObject);
begin
  FHisResult.Free;
end;

procedure TFmSearchBar.SelectWord(SelWord: string);
begin
  //..
  Self.EdSearchText.Text:=SelWord;
end;

end.
