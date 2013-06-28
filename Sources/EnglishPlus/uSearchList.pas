unit uSearchList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TSelWordEvent=procedure(SelWord:string) of object;
  //..
  TFmSearchList = class(TForm)
    LBList: TListBox;
    procedure LBListDblClick(Sender: TObject);
  private
    FSelWordEvent: TSelWordEvent;
    { Private declarations }
  public
    { Public declarations }
    procedure SearchWord(const sWord:string);
    property OnSelWordEvent:TSelWordEvent  read FSelWordEvent write FSelWordEvent;
  end;

var
  FmSearchList: TFmSearchList;

function GetSearchListobj(): TFmSearchList;

implementation

{$R *.dfm}

function GetSearchListobj(): TFmSearchList;
begin
  if Assigned(FmSearchList)=False then
    FmSearchList:=TFmSearchList.Create(nil);
  Result:=FmSearchList;
end;

{ TFmSearchList }

procedure TFmSearchList.LBListDblClick(Sender: TObject);
begin
  if (Assigned(FSelWordEvent)) and (LBList.ItemIndex>0) then
     FSelWordEvent(LBList.Items[LBList.ItemIndex]);
end;

procedure TFmSearchList.SearchWord(const sWord: string);
begin
  //..
end;

initialization

finalization
  if Assigned(FmSearchList) then
    FmSearchList.Free;

end.
