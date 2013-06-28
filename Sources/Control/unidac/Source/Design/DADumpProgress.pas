//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2011 Devart. All right reserved.
//  DB Access
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DADumpProgress;
{$ENDIF}

interface

uses
  SysUtils, Classes, 
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFNDEF KYLIX}
  Graphics, Controls, Forms, Dialogs,
  ComCtrls, Grids, DBGrids, DBCtrls, Buttons, ExtCtrls, StdCtrls,
{$ELSE}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls,
  QComCtrls, QGrids, QDBGrids, QDBCtrls, QButtons, QExtCtrls, Qt, QTypes,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  MemUtils, DBAccess, DADump;

type
  TDADumpProgressForm = class(TForm)
    Label1: TLabel;
    BitBtn1: TBitBtn;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    procedure DADumpBackupProgress(Sender: TObject; TableName: _string;
      TableNum, TableCount, Percent: Integer);
    procedure DADumpRestoreProgress(Sender: TObject; Percent: Integer);
    procedure FormActivate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
  protected
    IsBackup: boolean;
    WaitForTerminate: boolean;
    InProgress: boolean;

  {$IFNDEF FPC}
    procedure Process;
  {$ENDIF}

  {$IFDEF KYLIX}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
  {$ENDIF}
  public
    DADump: TDADump;

    procedure Backup;
    procedure Restore;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R DADumpProgress.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}
{$ENDIF}

{$IFDEF KYLIX}
const
  QEventType_CMFormActivate = QEventType(Integer(QEventType_ClxBase) + $1000);
{$ENDIF}

{ TDADumpProgressForm }

procedure TDADumpProgressForm.Backup;
var
  OldProgress: TDABackupProgressEvent;
begin
  Assert(DADump <> nil);
  OldProgress := DADump.OnBackupProgress;
  try
    Label1.Caption := 'Backup';
    IsBackup := True;
    DADump.OnBackupProgress := DADumpBackupProgress;

  {$IFDEF KYLIX}
    // ShowMessage('1 ' + IntToStr(Integer(Self)));
    QApplication_postEvent(Handle, QCustomEvent_create(QEventType_CMFormActivate, Self));
    // ShowMessage('2');
  {$ENDIF}
    ShowModal;
  finally
    DADump.OnBackupProgress := OldProgress;
  end;
end;

procedure TDADumpProgressForm.Restore;
var
  OldProgress: TDARestoreProgressEvent;

begin
  Assert(DADump <> nil);
  OldProgress := DADump.OnRestoreProgress;
  try
    Label1.Caption := 'Restore';
    IsBackup := False;
    DADump.OnRestoreProgress := DADumpRestoreProgress;
    ShowModal;
  finally
    DADump.OnRestoreProgress := OldProgress;
  end;
end;

procedure TDADumpProgressForm.DADumpBackupProgress(Sender: TObject;
  TableName: _string; TableNum, TableCount, Percent: Integer);
begin
  if WaitForTerminate then
    SysUtils.Abort;

  Label1.Caption := 'Backup ' + TableName;
  ProgressBar1.Max := TableCount;
  ProgressBar1.Position := TableNum;
  ProgressBar2.Position := Percent;

  Update;
  Application.ProcessMessages;
 end;

procedure TDADumpProgressForm.DADumpRestoreProgress(Sender: TObject;
  Percent: Integer);
begin
  if WaitForTerminate then
    SysUtils.Abort;

  ProgressBar2.Position := Percent;

  Update;
  Application.ProcessMessages;
end;

{$IFNDEF FPC}
procedure TDADumpProgressForm.Process;
begin
  ProgressBar1.Enabled := IsBackup;

  WaitForTerminate := False;
  InProgress := True;
  Cursor := crSQLWait;
  try
    try
      if IsBackup then
        DADump.Backup
      else
        DADump.Restore;
    except
      on E: Exception do
        Application.ShowException(E);
    end;
  finally
    InProgress := False;
    Cursor := crDefault;
  {$IFNDEF KYLIX}
    PostMessage(Handle, WM_CLOSE, 0, 0);
  {$ELSE}
    QApplication_postEvent(Handle, QCloseEvent_create);
  {$ENDIF}
  end;
end;
{$ENDIF}

procedure TDADumpProgressForm.FormActivate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
  Process;
{$ENDIF}
{$ENDIF}
end;

{$IFDEF KYLIX}
function TDADumpProgressForm.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  Result := True;
  if QEvent_type(Event) = QEventType_CMFormActivate then
    Process
  else
    Result := inherited EventFilter(Sender, Event);
end;
{$ENDIF}

procedure TDADumpProgressForm.BitBtn1Click(Sender: TObject);
begin
  WaitForTerminate := True;
  Close;
end;

procedure TDADumpProgressForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  WaitForTerminate := True;
  while InProgress do
    Sleep(300);
end;

procedure TDADumpProgressForm.FormResize(Sender: TObject);
begin
{$IFDEF KYLIX}
  ClientHeight := 131;
  ClientWidth := 248;
  ProgressBar1.Width := 232;
  ProgressBar2.Width := 232;
  BitBtn1.Left := 86;
{$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$i DADumpProgress.lrs}
{$ENDIF}

end.
