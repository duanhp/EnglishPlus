unit Main;

interface

uses
  Classes, SysUtils,
{$IFNDEF LINUX}
  Windows, Menus, ImgList, StdCtrls, ComCtrls, Buttons, ExtCtrls, Graphics,
  Controls, Forms, Dialogs, Grids, DBCtrls, DBGrids,
{$ELSE}
  QMenus, QImgList, QStdCtrls, QComCtrls, QButtons, QExtCtrls, QGraphics,
  QControls, QForms, QDialogs, QGrids, QDBCtrls, QDBGrids,
{$ENDIF}
  DB, MemData, DBAccess,
  Data, About;

type
  TMainForm = class(TForm)
    DBGrid1: TDBGrid;
    pnTop: TPanel;
    btConnect: TSpeedButton;
    btDisconnect: TSpeedButton;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    pnMaster: TPanel;
    pnRight: TPanel;
    pnLeft: TPanel;
    pnMiddle: TPanel;
    DBGrid2: TDBGrid;
    Panel7: TPanel;
    DBNavigator2: TDBNavigator;
    Panel8: TPanel;
    cbFailover: TCheckBox;
    cbLocalMasterDetail: TCheckBox;
    cbCachedUpdates: TCheckBox;
    cbPooling: TCheckBox;
    pnPooling: TPanel;
    Panel10: TPanel;
    cbValidate: TCheckBox;
    edMaxPoolSize: TEdit;
    edMinPoolSize: TEdit;
    edConnectionLifetime: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pnBottom: TPanel;
    meLog: TMemo;
    cbDisconnectedMode: TCheckBox;
    cbFetchAll: TCheckBox;
    Panel12: TPanel;
    btApply: TSpeedButton;
    btCancel: TSpeedButton;
    btCommit: TSpeedButton;
    Label1: TLabel;
    Panel5: TPanel;
    btStartTrans: TSpeedButton;
    btCommitTrans: TSpeedButton;
    btRollbackTrans: TSpeedButton;
    Label6: TLabel;
    StatusBar: TStatusBar;
    btKillSession: TSpeedButton;
    coRetryMode: TComboBox;
    Label7: TLabel;
    pnDetail: TPanel;
    Splitter: TSplitter;
    DBNavigator1: TDBNavigator;
    Panel9: TPanel;
    lbAbout: TLabel;
    pnFailover: TPanel;
    Panel16: TPanel;
    Panel18: TPanel;
    Splitter1: TSplitter;
    Panel15: TPanel;
    Panel17: TPanel;
    Panel1: TPanel;
    cbDebug: TCheckBox;
    Panel2: TPanel;
    btDrop: TSpeedButton;
    btCreate: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure btDisconnectClick(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure cbFailoverClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbPoolingClick(Sender: TObject);
    procedure edMaxPoolSizeExit(Sender: TObject);
    procedure edMinPoolSizeExit(Sender: TObject);
    procedure edConnectionLifetimeExit(Sender: TObject);
    procedure cbValidateClick(Sender: TObject);
    procedure cbCachedUpdatesClick(Sender: TObject);
    procedure cbDisconnectedModeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbLocalMasterDetailClick(Sender: TObject);
    procedure cbFetchAllClick(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure btCommitClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure btStartTransClick(Sender: TObject);
    procedure btCommitTransClick(Sender: TObject);
    procedure btRollbackTransClick(Sender: TObject);
    procedure btKillSessionClick(Sender: TObject);
    procedure lbAboutClick(Sender: TObject);
    procedure cbDebugClick(Sender: TObject);
    procedure lbAboutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnTopMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btCreateDropClick(Sender: TObject);
  private
    FActivated,
    FShouldNotUpdateControls: boolean;
    procedure OptionsToEditors;
    procedure EditorsToOptions;
    function GetShouldNotUpdateControls: boolean;

    procedure ConnectionAfterConnect(Sender: TObject);
    procedure ConnectionAfterDisconnect(Sender: TObject);
    procedure ConnectionConnectionLost(Sender: TObject;
      Component: TComponent; ConnLostCause: TConnLostCause;
      var RetryMode: TRetryMode);
    procedure dsUpdateData(Sender: TObject);
    procedure dsDataChange(Sender: TObject; Field: TField);

    procedure ShowPending;
    procedure ShowTrans;
  public
    property ShouldNotUpdateControls: boolean read GetShouldNotUpdateControls;
  end;

var
  MainForm: TMainForm;

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
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

{$IFNDEF LINUX}
  {$IFNDEF VER130}
  {$IFNDEF VER140}
  {$IFNDEF CLR}
    {$DEFINE XPMAN}
    {$R WindowsXP.res}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF XPMAN}
uses
  UxTheme;
{$ENDIF}

function TMainForm.GetShouldNotUpdateControls: boolean;
begin
  Result := not FActivated or FShouldNotUpdateControls;
end;

procedure TMainForm.OptionsToEditors;
begin
  FShouldNotUpdateControls := True;
  cbFailover.Checked := DM.Connection.Options.LocalFailover;
  cbPooling.Checked := DM.Connection.Pooling;
  with DM.Connection.PoolingOptions do begin
    edMaxPoolSize.Text := IntToStr(MaxPoolSize);
    edMinPoolSize.Text := IntToStr(MinPoolSize);
    edConnectionLifetime.Text := IntToStr(ConnectionLifetime);
    cbValidate.Checked := Validate;
  end;
  cbCachedUpdates.Checked := DM.quDetail.CachedUpdates;
  cbLocalMasterDetail.Checked := DM.quDetail.Options.LocalMasterDetail;
  try
    cbFetchAll.Checked := StrToBool(DM.quDetail.SpecificOptions.Values['FetchAll']);
  except
    DM.quDetail.SpecificOptions.Values['FetchAll'] := BoolToStr(cbFetchAll.Checked, True);
  end;
  cbDisconnectedMode.Checked := DM.Connection.Options.DisconnectedMode;
  cbDebug.Checked := DM.quMaster.Debug;
  FShouldNotUpdateControls := False;
end;

procedure TMainForm.EditorsToOptions;
var
  OnExit: TNotifyEvent;
begin
  if ActiveControl is TEdit then
    OnExit := TEdit(ActiveControl).OnExit
  else
  if ActiveControl is TMemo then
    OnExit := TMemo(ActiveControl).OnExit
  else
    Exit;
  if Assigned(OnExit) then
    OnExit(nil);
end;

procedure TMainForm.FormCreate(Sender: TObject);
{$IFDEF XPMAN}
  procedure UpdateStyle(Control: TWinControl);
  var
    Panel: TPanel;
    i: integer;
  begin
    for i := 0 to Control.ControlCount - 1 do begin
      if Control.Controls[i] is TSpeedButton then
        TSpeedButton(Control.Controls[i]).Flat := False
      else
      if Control.Controls[i] is TDBNavigator then
        TDBNavigator(Control.Controls[i]).Flat := False;
      if Control.Controls[i] is TWinControl then begin
        if (Control.Controls[i] is TPanel) then begin
            Panel := TPanel(Control.Controls[i]);
            Panel.ParentBackground := False;
            Panel.Color := clBtnFace;
          end;
        UpdateStyle(TWinControl(Control.Controls[i]));
      end;
    end;
  end;
{$ENDIF}

begin
  DM := TDM.Create(nil);
  AboutForm := TAboutForm.Create(nil);
{$IFDEF XPMAN}
  if UseThemes then
    UpdateStyle(Self);
{$ENDIF}
  DM.Connection.AfterConnect := ConnectionAfterConnect;
  DM.Connection.AfterDisconnect := ConnectionAfterDisconnect;;
  DM.Connection.OnConnectionLost := ConnectionConnectionLost;
  DM.dsDetail.OnStateChange := dsUpdateData;
  DM.dsDetail.OnDataChange := dsDataChange;
  DM.dsMaster.OnStateChange := dsUpdateData;
  DM.dsMaster.OnDataChange := dsDataChange;
  OptionsToEditors;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DM.Free;
  AboutForm.Free;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  FActivated := True;
end;

procedure TMainForm.btConnectClick(Sender: TObject);
begin
  EditorsToOptions;
  DM.Connection.Connect;
end;

procedure TMainForm.btDisconnectClick(Sender: TObject);
begin
  EditorsToOptions;
  DM.Connection.Disconnect;
end;

procedure TMainForm.btOpenClick(Sender: TObject);
begin
  EditorsToOptions;
  DM.quMaster.Open;
  DM.quDetail.Open;
end;

procedure TMainForm.btCloseClick(Sender: TObject);
begin
  EditorsToOptions;
  DM.quMaster.Close;
  DM.quDetail.Close;
end;

procedure TMainForm.cbDisconnectedModeClick(Sender: TObject);
begin
  if ShouldNotUpdateControls then
    Exit;
  try
    DM.Connection.Options.DisconnectedMode := cbDisconnectedMode.Checked;
  except
    OptionsToEditors;
    raise;
  end;
end;

procedure TMainForm.cbFailoverClick(Sender: TObject);
begin
  if ShouldNotUpdateControls then
    Exit;
  DM.Connection.Options.LocalFailover := cbFailover.Checked;
end;

procedure TMainForm.cbPoolingClick(Sender: TObject);
begin
  if ShouldNotUpdateControls then
    Exit;
  DM.Connection.Pooling := cbPooling.Checked;
end;

procedure TMainForm.edMaxPoolSizeExit(Sender: TObject);
begin
  try
    DM.Connection.PoolingOptions.MaxPoolSize := StrToInt(edMaxPoolSize.Text);
  except
    OptionsToEditors;
    raise;
  end;
end;

procedure TMainForm.edMinPoolSizeExit(Sender: TObject);
begin
  try
    DM.Connection.PoolingOptions.MinPoolSize := StrToInt(edMinPoolSize.Text);
  except
    OptionsToEditors;
    raise;
  end;
end;

procedure TMainForm.edConnectionLifetimeExit(Sender: TObject);
begin
  try
    DM.Connection.PoolingOptions.ConnectionLifetime := StrToInt(edConnectionLifetime.Text);
  except
    OptionsToEditors;
    raise;
  end;
end;

procedure TMainForm.cbValidateClick(Sender: TObject);
begin
  if ShouldNotUpdateControls then
    Exit;
  DM.Connection.PoolingOptions.Validate := cbValidate.Checked;
end;

procedure TMainForm.cbCachedUpdatesClick(Sender: TObject);
begin
  if ShouldNotUpdateControls then
    Exit;
  try
    DM.quDetail.CachedUpdates := cbCachedUpdates.Checked;
    DM.quMaster.CachedUpdates := cbCachedUpdates.Checked;    
  except
    OptionsToEditors;
    raise;
  end;
end;

procedure TMainForm.ConnectionConnectionLost(Sender: TObject;
  Component: TComponent; ConnLostCause: TConnLostCause;
  var RetryMode: TRetryMode);
var
  Msg: string;
begin
  case ConnLostCause of
    clUnknown:
      Msg := 'for reasons not known';
    clExecute:
      Msg := 'during SQL execution';
    clOpen:
      Msg := 'during query opening';
    clApply:
      Msg := 'during DataSet.ApplyUpdates';
    clServiceQuery:
      Msg := 'during service information request';
    clTransStart:
      Msg := 'during transaction start';
    clConnectionApply:
      Msg := 'during Connection.ApplyUpdates';
    clConnect:
      Msg := 'during connection establishing';
  end;
  meLog.Lines.Add(TimeToStr(Now) + ' ' + Component.Name + ' - Connection lost ' + Msg);
  if coRetryMode.ItemIndex <> 0 then
    RetryMode := TRetryMode(coRetryMode.ItemIndex - 1)
end;

procedure TMainForm.ConnectionAfterConnect(Sender: TObject);
begin
  btConnect.Enabled := False;
  btDisconnect.Enabled := True;
  btKillSession.Enabled := True;
end;

procedure TMainForm.ConnectionAfterDisconnect(Sender: TObject);
begin
  btDisconnect.Enabled := False;
  btKillSession.Enabled := False;  
  btConnect.Enabled := True;
end;

procedure TMainForm.dsUpdateData(Sender: TObject);
begin
  ShowPending;
end;

procedure TMainForm.dsDataChange(Sender: TObject; Field: TField);
begin
  ShowPending;
end;

procedure TMainForm.cbLocalMasterDetailClick(Sender: TObject);
begin
  try
    DM.quDetail.Options.LocalMasterDetail := cbLocalMasterDetail.Checked;
  except
    OptionsToEditors;
    raise;
  end;
end;

procedure TMainForm.cbFetchAllClick(Sender: TObject);
begin
  try
    DM.quDetail.SpecificOptions.Values['FetchAll'] := BoolToStr(cbFetchAll.Checked, True);
  except
    cbFetchAll.Checked := StrToBool(DM.quDetail.SpecificOptions.Values['FetchAll']);
    raise;
  end;
  try
    DM.quMaster.SpecificOptions.Values['FetchAll'] := BoolToStr(cbFetchAll.Checked, True);
  except
    cbFetchAll.Checked := StrToBool(DM.quMaster.SpecificOptions.Values['FetchAll']);
    raise;
  end;
end;

procedure TMainForm.ShowPending;
begin
  if DM.quMaster.UpdatesPending then
    StatusBar.Panels[0].Text := 'Master Updates Pending'
  else
    StatusBar.Panels[0].Text := '';
  if DM.quDetail.UpdatesPending then
    StatusBar.Panels[1].Text := 'Detail Updates Pending'
  else
    StatusBar.Panels[1].Text := '';
end;

procedure TMainForm.ShowTrans;
begin
  if DM.InTransaction then
    StatusBar.Panels[2].Text := 'UpdateTransaction is Active'
  else
    StatusBar.Panels[2].Text := '';
end;

procedure TMainForm.btApplyClick(Sender: TObject);
begin
  if DM.quMaster.UpdatesPending then
    DM.quMaster.ApplyUpdates;
  if DM.quDetail.UpdatesPending then
    DM.quDetail.ApplyUpdates;
  ShowPending;
end;

procedure TMainForm.btCommitClick(Sender: TObject);
begin
  DM.quMaster.CommitUpdates;
  DM.quDetail.CommitUpdates;
  ShowPending;
end;

procedure TMainForm.btCancelClick(Sender: TObject);
begin
  if DM.quMaster.UpdatesPending then
    DM.quMaster.CancelUpdates;
  if DM.quDetail.UpdatesPending then
    DM.quDetail.CancelUpdates;
  ShowPending;
end;

procedure TMainForm.btStartTransClick(Sender: TObject);
begin
  DM.StartTransaction;
  ShowTrans;
end;

procedure TMainForm.btCommitTransClick(Sender: TObject);
begin
  DM.CommitTransaction;
  ShowTrans;
end;

procedure TMainForm.btRollbackTransClick(Sender: TObject);
begin
  DM.RollbackTransaction;
  ShowTrans;
end;

procedure TMainForm.btKillSessionClick(Sender: TObject);
begin
  try
    DM.KillSession;
    btKillSession.Enabled := False;
    meLog.Lines.Add(TimeToStr(Now) + ' Session was killed');
  except
    on e: Exception do
      meLog.Lines.Add(TimeToStr(Now) + ' ' + Trim(e.Message));
  end;
end;

procedure TMainForm.lbAboutMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  lbAbout.Font.Color := $4080FF;
end;

procedure TMainForm.pnTopMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbAbout.Font.Color := $FF0000;
end;

procedure TMainForm.lbAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
  lbAbout.Font.Color := $FF0000;  
end;

procedure TMainForm.cbDebugClick(Sender: TObject);
begin
  DM.quMaster.Debug := cbDebug.Checked;
  DM.quDetail.Debug := cbDebug.Checked;
  DM.scCreate_MySQL.Debug := cbDebug.Checked;
  DM.scCreate_SQLServer.Debug := cbDebug.Checked;
  DM.scCreate_InterBase.Debug := cbDebug.Checked;
  DM.scCreate_Oracle.Debug := cbDebug.Checked;
  DM.scDrop.Debug := cbDebug.Checked;
  DM.scDrop_InterBase.Debug := cbDebug.Checked;
end;

procedure TMainForm.btCreateDropClick(Sender: TObject);
var
  s: string;
begin
 if Sender = btDrop then
   s := 'removed from database'
 else
   s := 'created in database';
 if MessageDlg(Format('Objects required for the demo will be %s. Continue?', [s]),
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    if Sender = btCreate then
      DM.CreateTables
    else
      DM.DropTables;
 end;
end;

end.
