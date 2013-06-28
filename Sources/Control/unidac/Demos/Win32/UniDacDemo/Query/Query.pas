unit Query;

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Db, Grids, DBGrids, StdCtrls, ToolWin, ComCtrls, Buttons,
{$IFNDEF FPC}
  MemDS,
{$ELSE}
  MemDataSet,
{$ENDIF}
  DBAccess, Uni, UnidacVcl, DemoFrame, UniDacDemoForm;

type
  TQueryFrame = class(TDemoFrame)
    DataSource: TDataSource;
    UniQuery: TUniQuery;
    DBGrid: TDBGrid;
    Panel9: TPanel;
    btRefreshRecord: TSpeedButton;
    DBNavigator: TDBNavigator;
    Splitter1: TSplitter;
    ToolBar: TPanel;
    Panel1: TPanel;
    btClose: TSpeedButton;
    btOpen: TSpeedButton;
    Panel2: TPanel;
    cbRefreshBeforeEdit: TCheckBox;
    cbRefreshAfterInsert: TCheckBox;
    cbRefreshAfterUpdate: TCheckBox;
    Panel5: TPanel;
    StaticText1: TLabel;
    edFetchRows: TEdit;
    Panel6: TPanel;
    Label5: TLabel;
    edFilter: TEdit;
    cbFiltered: TCheckBox;
    Panel7: TPanel;
    edUpdatingTable: TEdit;
    Label2: TLabel;
    Memo: TMemo;
    btPrepare: TSpeedButton;
    btUnPrepare: TSpeedButton;
    btExecute: TSpeedButton;
    btSaveToXML: TSpeedButton;
    SaveDialog: TSaveDialog;
    cbDMLRefresh: TCheckBox;
    Panel3: TPanel;
    Label1: TLabel;
    cmbLockMode: TComboBox;
    cbFetchAll: TCheckBox;
    Panel4: TPanel;
    btLockRecord: TSpeedButton;
    btUnLock: TSpeedButton;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btRefreshRecordClick(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure DataSourceStateChange(Sender: TObject);
    procedure cbFilteredClick(Sender: TObject);
    procedure cbRefreshOptionsClick(Sender: TObject);
    procedure UniQueryAfterOpen(DataSet: TDataSet);
    procedure edFetchRowsExit(Sender: TObject);
    procedure btPrepareClick(Sender: TObject);
    procedure btUnPrepareClick(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
    procedure btSaveToXMLClick(Sender: TObject);
    procedure btLockRecordClick(Sender: TObject);
    procedure cbDMLRefreshClick(Sender: TObject);
    procedure btUnLockClick(Sender: TObject);
    procedure cbFetchAllClick(Sender: TObject);
    procedure edUpdatingTableExit(Sender: TObject);
    procedure cmbLockModeChange(Sender: TObject);
    procedure edFilterExit(Sender: TObject);
  private
    { Private declarations }
    procedure AssignProperties(PropertyNameToSet: string = '');    procedure ShowState;
  public
    // Demo management
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ENDIF}
{$IFDEF WIN32}
{$R *.dfm}
{$ENDIF}
{$IFDEF WIN64}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

procedure TQueryFrame.ShowState;
var
  St: string;

  procedure AddSt(S:string);
  begin
    if St <> '' then
      St := St + ', ';
    St := St + S;
  end;

begin
  St := '';

  if UniQuery.Prepared then begin
    AddSt('Prepared');

    if UniQuery.IsQuery then
      AddSt('IsQuery');
  end;

  if UniQuery.Active then
    AddSt('Active')
  else
    AddSt('Inactive');

  if UniQuery.Executing then
    AddSt('Executing');

  if UniQuery.Fetching then
    AddSt('Fetching');

  edUpdatingTable.Text := UniQuery.UpdatingTable;

  UniDACForm.StatusBar.Panels[2].Text := St;
end;

procedure TQueryFrame.AssignProperties(PropertyNameToSet: string = '');  // empty string means all properties
begin
  if CheckProperty('FetchRows', PropertyNameToSet) then
    try
      UniQuery.FetchRows := StrToInt(edFetchRows.Text);
    except
      edFetchRows.SetFocus;
      raise;
    end;

  if CheckProperty('FetchAll', PropertyNameToSet) then
    try
      UniQuery.SpecificOptions.Values['FetchAll'] := BoolToStr(cbFetchAll.Checked, True);
    except
      cbFetchAll.Checked := StrToBool(UniQuery.SpecificOptions.Values['FetchAll']);
      raise;
    end;

  if CheckProperty('UpdatingTable', PropertyNameToSet) then
    try
      UniQuery.UpdatingTable := edUpdatingTable.Text;
    except
      edUpdatingTable.SetFocus;
      raise;
    end;

  if CheckProperty('Filter', PropertyNameToSet) then
    UniQuery.Filter := edFilter.Text;

  if CheckProperty('Filtered', PropertyNameToSet) then
    try
      UniQuery.Filtered := cbFiltered.Checked;
    finally
      cbFiltered.Checked := UniQuery.Filtered;
    end;

  if CheckProperty('SQL', PropertyNameToSet) then
    if Trim(UniQuery.SQL.Text) <> Trim(Memo.Lines.Text) then
      UniQuery.SQL.Assign(Memo.Lines);

  if CheckProperty('RefreshOptions', PropertyNameToSet) then begin
    if cbRefreshBeforeEdit.Checked then
      UniQuery.RefreshOptions := UniQuery.RefreshOptions + [roBeforeEdit]
    else
      UniQuery.RefreshOptions := UniQuery.RefreshOptions - [roBeforeEdit];
    if cbRefreshAfterInsert.Checked then
      UniQuery.RefreshOptions := UniQuery.RefreshOptions + [roAfterInsert]
    else
      UniQuery.RefreshOptions := UniQuery.RefreshOptions - [roAfterInsert];
    if cbRefreshAfterUpdate.Checked then
      UniQuery.RefreshOptions := UniQuery.RefreshOptions + [roAfterUpdate]
    else
      UniQuery.RefreshOptions := UniQuery.RefreshOptions - [roAfterUpdate];
  end;

  if CheckProperty('DMLRefresh', PropertyNameToSet) then
    UniQuery.DMLRefresh := cbDMLRefresh.Checked;

  if CheckProperty('LockMode', PropertyNameToSet) then
    case cmbLockMode.ItemIndex of
      1:
        UniQuery.LockMode := lmOptimistic;
      2:
        UniQuery.LockMode := lmPessimistic;
    else
      UniQuery.LockMode := lmNone;
    end;
end;

procedure TQueryFrame.btOpenClick(Sender: TObject);
begin
  try
    AssignProperties;
    UniQuery.Open;
  finally
    ShowState;
  end;
end;

procedure TQueryFrame.btCloseClick(Sender: TObject);
begin
  UniQuery.Close;
  ShowState;
end;

procedure TQueryFrame.btRefreshRecordClick(Sender: TObject);
begin
  UniQuery.RefreshRecord;
end;

procedure TQueryFrame.DataSourceStateChange(Sender: TObject);
begin
  UniDACForm.StatusBar.Panels[1].Text := 'Record ' + IntToStr(UniQuery.RecNo) + ' of ' + IntToStr(UniQuery.RecordCount);
end;

procedure TQueryFrame.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  DataSourceStateChange(nil);
end;

procedure TQueryFrame.cbFilteredClick(Sender: TObject);
begin
  AssignProperties('Filtered');
end;

procedure TQueryFrame.edFilterExit(Sender: TObject);
begin
  AssignProperties('Filter');
end;

procedure TQueryFrame.cbRefreshOptionsClick(Sender: TObject);
begin
  AssignProperties('RefreshOptions');
end;

procedure TQueryFrame.cbDMLRefreshClick(Sender: TObject);
begin
  AssignProperties('DMLRefresh');
end;

procedure TQueryFrame.cbFetchAllClick(Sender: TObject);
begin
  AssignProperties('FetchAll');
end;

procedure TQueryFrame.edFetchRowsExit(Sender: TObject);
begin
  AssignProperties('FetchRows');
end;

procedure TQueryFrame.edUpdatingTableExit(Sender: TObject);
begin
  AssignProperties('UpdatingTable');
end;

procedure TQueryFrame.cmbLockModeChange(Sender: TObject);
begin
  AssignProperties('LockMode');
end;

procedure TQueryFrame.UniQueryAfterOpen(DataSet: TDataSet);
begin
  ShowState;
end;

procedure TQueryFrame.btPrepareClick(Sender: TObject);
begin
  try
    AssignProperties;
    UniQuery.Prepare;
  finally
    ShowState;
  end;
end;

procedure TQueryFrame.btUnPrepareClick(Sender: TObject);
begin
  UniQuery.UnPrepare;
  ShowState;
end;

procedure TQueryFrame.btExecuteClick(Sender: TObject);
begin
  try
    AssignProperties;
    UniQuery.Execute;
  finally
    ShowState;
  end;
end;

procedure TQueryFrame.btSaveToXMLClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    UniQuery.SaveToXML(SaveDialog.FileName);
end;

procedure TQueryFrame.btLockRecordClick(Sender: TObject);
begin
  UniQuery.Lock;
end;

procedure TQueryFrame.btUnLockClick(Sender: TObject);
begin
  UniQuery.UnLock;
end;

// Demo management
procedure TQueryFrame.Initialize;
begin
  inherited;
  UniQuery.Connection := Connection as TUniConnection;
  edFetchRows.Text := IntToStr(UniQuery.FetchRows);

  cbRefreshBeforeEdit.Checked := roBeforeEdit in UniQuery.RefreshOptions;
  cbRefreshAfterInsert.Checked := roAfterInsert in UniQuery.RefreshOptions;
  cbRefreshAfterUpdate.Checked := roAfterUpdate in UniQuery.RefreshOptions;

  edFilter.Text := UniQuery.Filter;
  cbFiltered.Checked := UniQuery.Filtered;

  Memo.Lines.Text := UniQuery.SQL.Text;
end;

procedure TQueryFrame.SetDebug(Value: boolean);
begin
  UniQuery.Debug := Value;
end;

{$IFDEF FPC}
initialization
  {$i Query.lrs}
{$ENDIF}

end.

