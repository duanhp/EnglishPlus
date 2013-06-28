unit Dump;

interface

uses
{$IFDEF FPC}
  LResources,
{$ENDIF}
{$IFNDEF LINUX}
  Windows, Messages,
{$ENDIF}
  Classes, SysUtils, Db, Graphics,
  Controls, Forms, Dialogs, Buttons, DBCtrls, ExtCtrls, 
  Grids, DBGrids, StdCtrls, ToolWin, ComCtrls, UniDacVcl,
  DBAccess, Uni, UniDump, DADump, DemoFrame, UniDacDemoForm;

type
  TDumpFrame = class(TDemoFrame)
    UniDump: TUniDump;
    meSQL: TMemo;
    Panel2: TPanel;
    Panel3: TPanel;
    btBackup: TSpeedButton;
    btBackupSQL: TSpeedButton;
    btRestore: TSpeedButton;
    Panel6: TPanel;
    edTbNames: TEdit;
    Label1: TLabel;
    Panel7: TPanel;
    Label2: TLabel;
    edQuery: TEdit;
    pnResult: TPanel;
    ProgressBar: TProgressBar;
    lbTableName: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel4: TPanel;
    cbAddDrop: TCheckBox;
    Panel5: TPanel;
    cbQuoteNames: TCheckBox;
    procedure btBackupClick(Sender: TObject);
    procedure btRestoreClick(Sender: TObject);
    procedure btBackupSQLClick(Sender: TObject);
    procedure UniDumpRestoreProgress(Sender: TObject; Percent: Integer);
    procedure UniDumpBackupProgress(Sender: TObject; TableName: String;
      ObjectNum, TableCount, Percent: Integer);
  private
    { Private declarations }
  public
    procedure SetOptions;

    // Demo management
    procedure Initialize; override;
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

procedure TDumpFrame.SetOptions;
begin
  UniDump.TableNames := edTbNames.Text;
  UniDump.Options.AddDrop := cbAddDrop.Checked;
  UniDump.Options.QuoteNames := cbQuoteNames.Checked;
end;

procedure TDumpFrame.btBackupClick(Sender: TObject);
begin
  try
    SetOptions;
    UniDump.SQL.Clear;
    UniDump.Backup;
  finally
    ProgressBar.Position := 0;
    lbTableName.Caption := '';
    lbTableName.Parent.Repaint;
    meSQL.Lines.Assign(UniDump.SQL);
  end;
end;

procedure TDumpFrame.btRestoreClick(Sender: TObject);
begin
  ProgressBar.Position := 0;
  lbTableName.Caption := '';
  lbTableName.Parent.Repaint;
  UniDump.SQL.Assign(meSQL.Lines);
  try
    UniDump.Restore;
  finally
    ProgressBar.Position := 0;
  end;
end;

procedure TDumpFrame.btBackupSQLClick(Sender: TObject);
begin
  try
    SetOptions;
    UniDump.BackupQuery(edQuery.Text);
  finally
    ProgressBar.Position := 0;
    lbTableName.Caption := '';
    lbTableName.Parent.Repaint;
    meSQL.Lines.Assign(UniDump.SQL);
  end;
end;

procedure TDumpFrame.UniDumpRestoreProgress(Sender: TObject; Percent: Integer);
begin
  ProgressBar.Position := Percent;
end;

procedure TDumpFrame.UniDumpBackupProgress(Sender: TObject;
  TableName: String; ObjectNum, TableCount, Percent: Integer);
begin
  if lbTableName.Caption <> TableName then begin
    lbTableName.Caption := TableName;
    pnResult.Repaint;
  end;
  ProgressBar.Position := Percent;
end;

// Demo management
procedure TDumpFrame.Initialize;
begin
  UniDump.Connection := Connection as TUniConnection;
end;

initialization
{$IFDEF FPC}
{$I Dump.lrs}
{$ENDIF}
end.
