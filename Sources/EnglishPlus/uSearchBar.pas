unit uSearchBar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TSearchWordinfoPro=procedure(const AWord:string) of object;
  TFmSearchBar = class(TForm)
    BtnPre: TButton;
    BtnNext: TButton;
    EdSearchEdit: TEdit;
    BtnGo: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Image10: TImage;
    Image11: TImage;
    procedure EdSearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure BtnGoClick(Sender: TObject);
    procedure EdSearchEditKeyPress(Sender: TObject; var Key: Char);
  private
    FOnSearchWordInfo: TSearchWordinfoPro;
    { Private declarations }
    procedure SelWordCallBack(const AWord:string);
    procedure SearchWord(const Value:string);
    procedure AdjustWordListFormPos();
  public
    { Public declarations }
    property OnSearchWordInfo: TSearchWordinfoPro read FOnSearchWordInfo write FOnSearchWordInfo;
  end;

var
  FmSearchBar: TFmSearchBar;

implementation

uses
  uSearchList;

{$R *.dfm}

{ TFmSearchBar }

procedure TFmSearchBar.AdjustWordListFormPos;
var
  P:TPoint;
begin
  if Assigned(FmSearchList) then
  begin
    P.X:=EdSearchEdit.Left;
    p.Y:=EdSearchEdit.Top;
    p:=self.ClientToScreen(p);
    FmSearchList.Left:=p.X;
    FmSearchList.Top:=p.Y+EdSearchEdit.Height+4;
    FmSearchList.Width:=EdSearchEdit.Width+20;
  end;
end;

procedure TFmSearchBar.BtnGoClick(Sender: TObject);
begin
  if Assigned(FOnSearchWordInfo) and (Trim(EdSearchEdit.Text)<>'') then
     FOnSearchWordInfo(Trim(EdSearchEdit.Text));
end;

procedure TFmSearchBar.EdSearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Trim(EdSearchEdit.Text)<>'' then
    SearchWord(EdSearchEdit.Text);
end;

procedure TFmSearchBar.EdSearchEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
  begin
    if Assigned(FmSearchList) then
      FmSearchList.Close;
  end;
end;

procedure TFmSearchBar.FormResize(Sender: TObject);
begin
  AdjustWordListFormPos();
end;

procedure TFmSearchBar.SearchWord(const Value: string);
begin
  if Assigned(FmSearchList)=False then
     FmSearchList:=TFmSearchList.Create(nil);
  FmSearchList.OnSelWordEvent:=SelWordCallBack;
  FmSearchList.SearchList(Value);
  AdjustWordListFormPos();
  if FmSearchList.Showing=False then
    FmSearchList.Show;
  EdSearchEdit.SetFocus;
end;

procedure TFmSearchBar.SelWordCallBack(const AWord: string);
begin
  EdSearchEdit.Text:=AWord;
end;

end.
