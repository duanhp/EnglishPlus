unit CachedUpdates;

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
  DBAccess, Uni, UniDacVcl, DemoFrame, UniDacDemoForm;

type
  TCachedUpdatesFrame = class(TDemoFrame)
    DBGrid: TDBGrid;
    DataSource: TDataSource;
    UniQuery: TUniQuery;
    Panel8: TPanel;
    ToolBar: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    RefreshRecord: TSpeedButton;
    DBNavigator: TDBNavigator;
    Panel1: TPanel;
    Label2: TLabel;
    Panel3: TPanel;
    btApply: TSpeedButton;
    btCommit: TSpeedButton;
    btCancel: TSpeedButton;
    btRevertRecord: TSpeedButton;
    Panel2: TPanel;
    cbCachedUpdates: TCheckBox;
    cbCustomUpdate: TCheckBox;
    Panel4: TPanel;
    Label3: TLabel;
    Panel5: TPanel;
    btStartTrans: TSpeedButton;
    btCommitTrans: TSpeedButton;
    btRollBackTrans: TSpeedButton;
    Panel6: TPanel;
    Label1: TLabel;
    cbDeleted: TCheckBox;
    cbInserted: TCheckBox;
    cbModified: TCheckBox;
    cbUnmodified: TCheckBox;
    Panel7: TPanel;
    Label4: TLabel;
    edUpdateBatchSize: TEdit;
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure btStartTransClick(Sender: TObject);
    procedure btCommitTransClick(Sender: TObject);
    procedure btRollbackTransClick(Sender: TObject);
    procedure cbCachedUpdatesClick(Sender: TObject);
    procedure UniQueryUpdateError(DataSet: TDataSet; E: EDatabaseError;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure UniQueryUpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure cbCustomUpdateClick(Sender: TObject);
    procedure UniQueryCalcFields(DataSet: TDataSet);
    procedure btCommitClick(Sender: TObject);
    procedure cbUnmodifiedClick(Sender: TObject);
    procedure cbModifiedClick(Sender: TObject);
    procedure cbInsertedClick(Sender: TObject);
    procedure cbDeletedClick(Sender: TObject);
    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure DataSourceStateChange(Sender: TObject);
    procedure DBGridDrawDataCell(Sender: TObject; const Rect: TRect;
      Field: TField; State: TGridDrawState);
    procedure btRevertRecordClick(Sender: TObject);
    procedure RefreshRecordClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowTrans;
    procedure ShowPending;
    procedure ShowUpdateRecordTypes;
  public
    destructor Destroy; override;

    // Demo management
    procedure Initialize; override;
    procedure SetDebug(Value: boolean); override;
  end;

implementation

uses
  UpdateAction;

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

procedure TCachedUpdatesFrame.ShowTrans;
begin
  if UniQuery.Connection .InTransaction then
    UniDACForm.StatusBar.Panels[2].Text := 'In Transaction'
  else
    UniDACForm.StatusBar.Panels[2].Text := '';
end;

procedure TCachedUpdatesFrame.ShowPending;
begin
  if UniQuery.UpdatesPending then
    UniDACForm.StatusBar.Panels[1].Text := 'Updates Pending'
  else
    UniDACForm.StatusBar.Panels[1].Text := '';
end;

procedure TCachedUpdatesFrame.ShowUpdateRecordTypes;
begin
  if UniQuery.CachedUpdates then begin
    cbUnmodified.Checked := rtUnmodified in UniQuery.UpdateRecordTypes;
    cbModified.Checked := rtModified in UniQuery.UpdateRecordTypes;
    cbInserted.Checked := rtInserted in UniQuery.UpdateRecordTypes;
    cbDeleted.Checked := rtDeleted in UniQuery.UpdateRecordTypes;
  end;
end;

procedure TCachedUpdatesFrame.btOpenClick(Sender: TObject);
begin
  UniQuery.Open;
end;

procedure TCachedUpdatesFrame.btCloseClick(Sender: TObject);
begin
  UniQuery.Close;
end;

procedure TCachedUpdatesFrame.btApplyClick(Sender: TObject);
begin
  UniQuery.Options.UpdateBatchSize := StrToInt(edUpdateBatchSize.Text);
  UniQuery.ApplyUpdates;
  ShowPending;
end;

procedure TCachedUpdatesFrame.btCommitClick(Sender: TObject);
begin
  UniQuery.CommitUpdates;
  ShowPending;
end;

procedure TCachedUpdatesFrame.btCancelClick(Sender: TObject);
begin
  UniQuery.CancelUpdates;
  ShowPending;
end;

procedure TCachedUpdatesFrame.btStartTransClick(Sender: TObject);
begin
  UniQuery.Connection.StartTransaction;
  ShowTrans;
end;

procedure TCachedUpdatesFrame.btCommitTransClick(Sender: TObject);
begin
  UniQuery.Connection.Commit;
  ShowTrans;
end;

procedure TCachedUpdatesFrame.btRollbackTransClick(Sender: TObject);
begin
  UniQuery.Connection.Rollback;
  ShowTrans;
end;

destructor TCachedUpdatesFrame.Destroy;
begin
  FreeAndNil(UpdateActionForm);
  inherited;
end;

procedure TCachedUpdatesFrame.cbCachedUpdatesClick(Sender: TObject);
begin
  try
    UniQuery.CachedUpdates := cbCachedUpdates.Checked;
  except
    cbCachedUpdates.Checked := UniQuery.CachedUpdates;
    raise;
  end;
  ShowUpdateRecordTypes;
end;

procedure TCachedUpdatesFrame.UniQueryUpdateError(DataSet: TDataSet; E: EDatabaseError;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  UpdateActionForm.rgAction.ItemIndex := Ord(UpdateAction);
  UpdateActionForm.rgKind.ItemIndex := Ord(UpdateKind);
  UpdateActionForm.lbField.Caption := String(DataSet.Fields[0].Value);
  UpdateActionForm.lbMessage.Caption := E.Message;
  UpdateActionForm.ShowModal;
  UpdateAction := TUpdateAction(UpdateActionForm.rgAction.ItemIndex);
end;

procedure TCachedUpdatesFrame.UniQueryUpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  UpdateActionForm.rgAction.ItemIndex := Ord(UpdateAction);
  UpdateActionForm.rgKind.ItemIndex := Ord(UpdateKind);
  UpdateActionForm.lbField.Caption := String(DataSet.Fields[0].NewValue);
  UpdateActionForm.lbMessage.Caption := '';
  UpdateActionForm.ShowModal;
  UpdateAction := TUpdateAction(UpdateActionForm.rgAction.ItemIndex);
end;

procedure TCachedUpdatesFrame.cbCustomUpdateClick(Sender: TObject);
begin
  if cbCustomUpdate.Checked then
    UniQuery.OnUpdateRecord := UniQueryUpdateRecord
  else
    UniQuery.OnUpdateRecord := nil;
end;

procedure TCachedUpdatesFrame.UniQueryCalcFields(DataSet: TDataSet);
var
  St:string;
begin
  case Ord(TCustomUniDataSet(DataSet).UpdateStatus) of
    0: St := 'Unmodified';
    1: St := 'Modified';
    2: St := 'Inserted';
    3: St := 'Deleted';
  end;
  DataSet.FieldByName('Status').AsString := St;

{  case Ord(TUniDataSet(DataSet).UpdateResult) of
    0: St := 'Fail';
    1: St := 'Abort';
    2: St := 'Skip';
    3: St := 'Applied';
  end;
  DataSet.FieldByName('Result').AsString := St;}
end;

procedure TCachedUpdatesFrame.cbUnmodifiedClick(Sender: TObject);
begin
  if cbUnmodified.Checked then
    UniQuery.UpdateRecordTypes := UniQuery.UpdateRecordTypes + [rtUnmodified]
  else
    UniQuery.UpdateRecordTypes := UniQuery.UpdateRecordTypes - [rtUnmodified];
end;

procedure TCachedUpdatesFrame.cbModifiedClick(Sender: TObject);
begin
  if cbModified.Checked then
    UniQuery.UpdateRecordTypes := UniQuery.UpdateRecordTypes + [rtModified]
  else
    UniQuery.UpdateRecordTypes := UniQuery.UpdateRecordTypes - [rtModified];
end;

procedure TCachedUpdatesFrame.cbInsertedClick(Sender: TObject);
begin
  if cbInserted.Checked then
    UniQuery.UpdateRecordTypes := UniQuery.UpdateRecordTypes + [rtInserted]
  else
    UniQuery.UpdateRecordTypes := UniQuery.UpdateRecordTypes - [rtInserted];
end;

procedure TCachedUpdatesFrame.cbDeletedClick(Sender: TObject);
begin
  if cbDeleted.Checked then
    UniQuery.UpdateRecordTypes := UniQuery.UpdateRecordTypes + [rtDeleted]
  else
    UniQuery.UpdateRecordTypes := UniQuery.UpdateRecordTypes - [rtDeleted];
end;

procedure TCachedUpdatesFrame.DataSourceStateChange(Sender: TObject);
begin
  ShowPending;
  UniDACForm.StatusBar.Panels[3].Text := 'Record ' + IntToStr(UniQuery.RecNo) + ' of ' + IntToStr(UniQuery.RecordCount) ;
end;

procedure TCachedUpdatesFrame.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  DataSourceStateChange(nil);
end;

procedure TCachedUpdatesFrame.DBGridDrawDataCell(Sender: TObject; const Rect: TRect;
  Field: TField; State: TGridDrawState);
begin
{$IFNDEF FPC}
  if UniQuery.UpdateResult in [uaFail,uaSkip] then
    TDBGrid(Sender).Canvas.Brush.Color := clRed
  else
    if UniQuery.UpdateStatus <> usUnmodified then
      TDBGrid(Sender).Canvas.Brush.Color := clYellow;

  TDBGrid(Sender).DefaultDrawDataCell(Rect, Field, State);
{$ENDIF}
end;

procedure TCachedUpdatesFrame.btRevertRecordClick(Sender: TObject);
begin
  UniQuery.RevertRecord;
  ShowPending;
end;

procedure TCachedUpdatesFrame.RefreshRecordClick(Sender: TObject);
begin
  UniQuery.RefreshRecord;
end;

// Demo management
procedure TCachedUpdatesFrame.Initialize;
begin
  inherited;
  UniQuery.Connection := Connection as TUniConnection;
  UpdateActionForm := TUpdateActionForm.Create(nil);
  cbCachedUpdates.Checked := UniQuery.CachedUpdates;
  ShowUpdateRecordTypes;
end;

procedure TCachedUpdatesFrame.SetDebug(Value: boolean);
begin
  UniQuery.Debug := Value;
end;

{$IFDEF FPC}
initialization
  {$i CachedUpdates.lrs}
{$ENDIF}

end.
