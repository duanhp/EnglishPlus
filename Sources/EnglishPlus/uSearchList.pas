unit uSearchList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  //..
  TSelWordEvent=procedure(const AWord:string) of object;
  //
  TFmSearchList = class(TForm)
    LbWordList: TListBox;
    procedure LbWordListDblClick(Sender: TObject);
  private
    FOnSelWordEvent: TSelWordEvent;
    { Private declarations }
  public
    { Public declarations }
    procedure SearchList(const AValue:string);
    property OnSelWordEvent:TSelWordEvent  read FOnSelWordEvent write FOnSelWordEvent;
  end;

var
  FmSearchList: TFmSearchList;

implementation

{$R *.dfm}

procedure TFmSearchList.LbWordListDblClick(Sender: TObject);
begin
  //..
  if LbWordList.ItemIndex>=0 then
  begin
    if Assigned(FOnSelWordEvent) then
      FOnSelWordEvent(LbWordList.Items[LbWordList.ItemIndex]);
  end;
  Close;
end;

procedure TFmSearchList.SearchList(const AValue: string);
begin

end;

initialization

finalization
  if Assigned(FmSearchList) then
     FmSearchList.Free;

end.
