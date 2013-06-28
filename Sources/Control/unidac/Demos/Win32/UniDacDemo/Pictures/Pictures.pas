unit Pictures;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Db, Grids, DBGrids,  StdCtrls, ToolWin,
  ComCtrls, ExtDlgs, Buttons, MemDS, DBAccess, Uni, 
  DemoFrame, UniDacDemoForm;

type
  TPicturesFrame = class(TDemoFrame)
    OpenPictureDialog: TOpenPictureDialog;
    ScrollBox1: TScrollBox;
    SavePictureDialog: TSavePictureDialog;
    ToolBar: TPanel;
    Panel2: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    DBNavigator: TDBNavigator;
    DBGrid: TDBGrid;
    ToolBar1: TPanel;
    Panel1: TPanel;
    btLoad: TSpeedButton;
    btSave: TSpeedButton;
    btClear: TSpeedButton;
    Splitter1: TSplitter;
    dsPictures: TDataSource;
    quPictures: TUniQuery;
    DBImage: TDBImage;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    // Demo management
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;
  end;

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

procedure TPicturesFrame.btOpenClick(Sender: TObject);
begin
  quPictures.Open;
end;

procedure TPicturesFrame.btCloseClick(Sender: TObject);
begin
  quPictures.Close;
end;

procedure TPicturesFrame.btLoadClick(Sender: TObject);
var
  BlobField: TBlobField;
  FileName: string;
begin
  if OpenPictureDialog.Execute then begin
    if quPictures.State in [dsBrowse] then
      quPictures.Edit;
    BlobField := quPictures.FieldByName('Picture') as TBlobField;
    FileName := OpenPictureDialog.FileName;
    BlobField.LoadFromFile(FileName);
  end;
end;

procedure TPicturesFrame.btSaveClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then begin
    TBlobField(quPictures.FieldByName('Picture')).
      SaveToFile(SavePictureDialog.FileName);
  end;
end;

procedure TPicturesFrame.btClearClick(Sender: TObject);
begin
  if quPictures.State in [dsBrowse] then
    quPictures.Edit;
  TBlobField(quPictures.FieldByName('Picture')).Clear;
end;

// Demo management
procedure TPicturesFrame.Initialize;
begin
  inherited;
  quPictures.Connection := Connection as TUniConnection;
end;

procedure TPicturesFrame.SetDebug(Value: boolean);
begin
  quPictures.Debug := Value;
end;

end.



