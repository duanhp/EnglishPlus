unit CRDBGrid;

interface

uses
  Classes, SysUtils,
  Windows, Menus, ImgList, StdCtrls, ComCtrls, Buttons, ExtCtrls, Graphics,
  Controls, Forms, Dialogs, DBCtrls, Grids, DBGrids, UniDacVcl,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  DB, DBAccess, Uni, DemoFrame, DAScript, UniScript, ActnList, MemDS,
  CRGrid;
  
type
  TCRDBGridFrame = class(TDemoFrame)
    UniQuery: TUniQuery;
    UniDataSource: TUniDataSource;
    CRDBGrid: TCRDBGrid;
    ActionList1: TActionList;
    actSearchBar: TAction;
    actFilterBar: TAction;
    Panel1: TPanel;
    Panel3: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    DBNavigator1: TDBNavigator;
    chkFiltered: TCheckBox;
    chkFilterBar: TCheckBox;
    chkSearchBar: TCheckBox;
    chkRecCount: TCheckBox;
    chkStretch: TCheckBox;
    Panel2: TPanel;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure actSearchBarExecute(Sender: TObject);
    procedure actSearchBarUpdate(Sender: TObject);
    procedure actFilterBarExecute(Sender: TObject);
    procedure actFilterBarUpdate(Sender: TObject);
    procedure chkFilteredClick(Sender: TObject);
    procedure chkRecCountClick(Sender: TObject);
    procedure chkStretchClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetDebug(Value: boolean); override;
    procedure Initialize; override;
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

procedure TCRDBGridFrame.btOpenClick(Sender: TObject);
begin
  UniQuery.Open;
end;

procedure TCRDBGridFrame.SetDebug(Value: boolean);
begin
  UniQuery.Debug := Value;
end;

procedure TCRDBGridFrame.Initialize;
begin
  UniQuery.Connection := TUniConnection(Connection);
end;

procedure TCRDBGridFrame.btCloseClick(Sender: TObject);
begin
  UniQuery.Close;
end;

procedure TCRDBGridFrame.actSearchBarExecute(Sender: TObject);
begin
  actSearchBar.Checked := not actSearchBar.Checked;
  if actSearchBar.Checked then
    CRDBGrid.OptionsEx := CRDBGrid.OptionsEx + [dgeSearchBar]
  else
    CRDBGrid.OptionsEx := CRDBGrid.OptionsEx - [dgeSearchBar];
end;

procedure TCRDBGridFrame.actSearchBarUpdate(Sender: TObject);
begin
  actSearchBar.Checked := dgeSearchBar in CRDBGrid.OptionsEx;
end;

procedure TCRDBGridFrame.actFilterBarExecute(Sender: TObject);
begin
  actFilterBar.Checked := not actFilterBar.Checked;
  if actFilterBar.Checked then
    CRDBGrid.OptionsEx := CRDBGrid.OptionsEx + [dgeFilterBar]
  else
    CRDBGrid.OptionsEx := CRDBGrid.OptionsEx - [dgeFilterBar];
end;

procedure TCRDBGridFrame.actFilterBarUpdate(Sender: TObject);
begin
  actFilterBar.Checked := dgeFilterBar in CRDBGrid.OptionsEx;
end;

procedure TCRDBGridFrame.chkFilteredClick(Sender: TObject);
begin
  CRDBGrid.Filtered := chkFiltered.Checked;
end;

procedure TCRDBGridFrame.chkRecCountClick(Sender: TObject);
begin
  if chkRecCount.Checked then
    CRDBGrid.OptionsEx := CRDBGrid.OptionsEx + [dgeRecordCount]
  else
    CRDBGrid.OptionsEx := CRDBGrid.OptionsEx - [dgeRecordCount];
end;

procedure TCRDBGridFrame.chkStretchClick(Sender: TObject);
begin
  if chkStretch.Checked then
    CRDBGrid.OptionsEx := CRDBGrid.OptionsEx + [dgeStretch]
  else
    CRDBGrid.OptionsEx := CRDBGrid.OptionsEx - [dgeStretch];
end;

end.
