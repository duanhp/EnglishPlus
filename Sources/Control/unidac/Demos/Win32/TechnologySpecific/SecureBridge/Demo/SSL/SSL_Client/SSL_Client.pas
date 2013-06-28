unit SSL_Client;

{$I ..\..\Base\SBDemo.inc}
interface

uses
  Classes, SysUtils, DB,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, Grids, DBGrids, StdCtrls, ToolWin, ComCtrls,
  Buttons, Spin, DemoFrame, MemDS, DBAccess, Uni, UniProvider,
  UniDacVcl, ScBridge, ScCryptoAPIStorage, CRSSLIOHandler, CRVio,
{$IFNDEF CLR}
  OracleUniProvider,
  SQLServerUniProvider,
  InterBaseUniProvider,
  MySQLUniProvider,
  PostgreSQLUniProvider
{$ELSE}
  System.ComponentModel,
  Devart.UniDac.Oracle.OracleUniProvider,
  Devart.UniDac.SQLServer.SQLServerUniProvider,
  Devart.UniDac.InterBase.InterBaseUniProvider,
  Devart.UniDac.MySQL.MySQLUniProvider,
  Devart.UniDac.PostgreSQL.PostgreSQLUniProvider
{$ENDIF}
  ;

type
  TSSLClientFrame = class(TDemoFrame)
    Panel1: TPanel;
    Panel4: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel5: TPanel;
    Label2: TLabel;
    btConnectDB: TSpeedButton;
    btDisconnectDB: TSpeedButton;
    DBGrid: TDBGrid;
    UniConnection: TUniConnection;
    UniTable: TUniTable;
    DataSource: TDataSource;
    Label10: TLabel;
    edDBHost: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    edDBUserName: TEdit;
    Label13: TLabel;
    edDBPassword: TEdit;
    Label14: TLabel;
    seDBPort: TSpinEdit;
    cbDBDatabase: TComboBox;
    Panel7: TPanel;
    lbTableName: TLabel;
    cbTableName: TComboBox;
    Panel9: TPanel;
    btOpen: TSpeedButton;
    btClose: TSpeedButton;
    Panel8: TPanel;
    CRSSLIOHandler: TCRSSLIOHandler;
    ScCryptoAPIStorage: TScCryptoAPIStorage;
    DBNavigator: TDBNavigator;
    Panel3: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edCACertName: TEdit;
    edKeyName: TEdit;
    cbRandomization: TCheckBox;
    cbSSL: TCheckBox;
    sbCACertName: TSpeedButton;
    edCertName: TEdit;
    sbCertName: TSpeedButton;
    sbKeyName: TSpeedButton;
    OpenDialog: TOpenDialog;
    Label6: TLabel;
    cbProvider: TComboBox;
    procedure btConnectDBClick(Sender: TObject);
    procedure btDisconnectDBClick(Sender: TObject);
    procedure UniConnectionAfterConnect(Sender: TObject);
    procedure UniTableAfterClose(DataSet: TDataSet);
    procedure UniTableAfterOpen(DataSet: TDataSet);
    procedure btOpenClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure cbTableNameDropDown(Sender: TObject);
    procedure cbTableNameChange(Sender: TObject);
    procedure cbDBDatabaseDropDown(Sender: TObject);
    procedure cbDBDatabaseChange(Sender: TObject);
    procedure UniConnectionBeforeConnect(Sender: TObject);
    procedure edDBHostChange(Sender: TObject);
    procedure sbCACertNameClick(Sender: TObject);
    procedure sbKeyNameClick(Sender: TObject);
    procedure sbCertNameClick(Sender: TObject);
  private
    procedure CheckRandomize;
  {$IFDEF MSWINDOWS}
    function LoadState: boolean;
    function SaveState: boolean;
    function KeyPath: string;
  {$ENDIF}
  public
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Finalize; override;
  end;

var
  SSLClientFrame: TSSLClientFrame;

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

uses
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
  ScConsts, ScSSHUtil, SSLDacDemoForm,
  MyClasses, PgClasses;

const
  CertFilter = 'All formats |*.pem;*.crt;*.cer|PEM format (*.pem;*.crt)|*.pem;*.crt|DER format (*.cer)|*.cer|All files (*.*)|*.*';
  KeyFilter = 'All formats |*.key;*.ssl;*.pem;*.ietf;*.pub;*.ietfpub|OpenSSL format (*.ssl)|*.ssl|PKCS8 format (*.pem)|*.pem|IETF format (*.ietf)|*.ietf|Public key (*.pub)|*.pub|Public IETF key (*.ietfpub)|*.ietfpub|All files (*.*)|*.*';

destructor TSSLClientFrame.Destroy;
begin
  UniConnection.Close;
  inherited;
end;

procedure TSSLClientFrame.Initialize;
begin
  inherited;

{$IFDEF MSWINDOWS}
  LoadState;
{$ENDIF}
  UniProviders.GetProviderNames(cbProvider.Items);
end;

procedure TSSLClientFrame.Finalize;
begin
{$IFDEF MSWINDOWS}
  SaveState;
{$ENDIF}

  inherited;
end;

procedure TSSLClientFrame.CheckRandomize;
begin
  if not SSLDacForm.Randomized and not cbRandomization.Checked then begin
    SSLDacForm.Randomize;
    if not SSLDacForm.Randomized and not cbRandomization.Checked then
      raise Exception.Create('Data for the random generator has not been generated');
  end;
end;

procedure TSSLClientFrame.btConnectDBClick(Sender: TObject);
begin
  UniConnection.Connect;
end;

procedure TSSLClientFrame.btDisconnectDBClick(Sender: TObject);
begin
  UniConnection.Disconnect;
end;

procedure TSSLClientFrame.edDBHostChange(Sender: TObject);
begin
  UniConnection.Disconnect;
end;

procedure TSSLClientFrame.UniConnectionAfterConnect(Sender: TObject);
begin
  btConnectDB.Enabled := not UniConnection.Connected;
  btDisconnectDB.Enabled := UniConnection.Connected;
  btOpen.Enabled := UniConnection.Connected and (cbTableName.Text <> '');
  cbTableName.Enabled := UniConnection.Connected;
end;

procedure TSSLClientFrame.UniTableAfterOpen(DataSet: TDataSet);
begin
  btOpen.Enabled := False;
  btClose.Enabled := True;
end;

procedure TSSLClientFrame.UniTableAfterClose(DataSet: TDataSet);
begin
  btOpen.Enabled := not btConnectDB.Enabled and (cbTableName.Text <> '');
  btClose.Enabled := False;
end;

procedure TSSLClientFrame.btOpenClick(Sender: TObject);
begin
  UniTable.Open;
end;

procedure TSSLClientFrame.btCloseClick(Sender: TObject);
begin
  UniTable.Close;
end;

procedure TSSLClientFrame.cbTableNameDropDown(Sender: TObject);
begin
  if UniConnection.Connected then
    UniConnection.GetTableNames(cbTableName.Items)
  else
    cbTableName.Items.Clear;
end;

procedure TSSLClientFrame.cbTableNameChange(Sender: TObject);
begin
  UniTable.TableName := cbTableName.Text;
  btOpen.Enabled := UniConnection.Connected and (cbTableName.Text <> '');
end;

{$IFDEF MSWINDOWS}
function TSSLClientFrame.SaveState: boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    with Registry do begin
      OpenKey(KeyPath + '\' + TSSLClientFrame.ClassName, True);
      WriteString('CACertName', edCACertName.Text);
      WriteString('ClientCertName', edCertName.Text);
      WriteString('CertPrivateKeyName', edKeyName.Text);

      WriteString('Provider', cbProvider.Text);
      WriteString('DBHost', edDBHost.Text);
      WriteInteger('DBPort', seDBPort.Value);
      WriteString('DBUserName', edDBUserName.Text);
      WriteString('DBDatabase', cbDBDatabase.Text);
      WriteBool('Silent randomization', cbRandomization.Checked);
      WriteBool('Use SSL', cbSSL.Checked);
    end;
  finally
    Registry.Free;
  end;

  Result := True;
end;

function TSSLClientFrame.LoadState: boolean;
var
  Registry: TRegistry;
begin
  Result := False;
  Registry := TRegistry.Create(KEY_READ OR KEY_WRITE);
  try
    with Registry do begin
      if OpenKey(KeyPath + '\' + TSSLClientFrame.ClassName, False) then begin
        if ValueExists('CACertName') then
          edCACertName.Text := ReadString('CACertName');
        if ValueExists('ClientCertName') then
          edCertName.Text := ReadString('ClientCertName');
        if ValueExists('CertPrivateKeyName') then
          edKeyName.Text := ReadString('CertPrivateKeyName');

        if ValueExists('Provider') then
          cbProvider.Text := ReadString('Provider');
        if ValueExists('DBHost') then
          edDBHost.Text := ReadString('DBHost');
        if ValueExists('DBPort') then
          seDBPort.Value := ReadInteger('DBPort');
        if ValueExists('DBUserName') then
          edDBUserName.Text := ReadString('DBUserName');
        if ValueExists('DBDatabase') then
          cbDBDatabase.Text := ReadString('DBDatabase');
        if ValueExists('Silent randomization') then
          cbRandomization.Checked := ReadBool('Silent randomization');
        if ValueExists('Use SSL') then
          cbSSL.Checked := ReadBool('Use SSL');
        Result := True;
      end;
    end;
  finally
    Registry.Free;
  end;
end;

function TSSLClientFrame.KeyPath: string;
begin
  Result := '\SOFTWARE\Devart\SecureBridge\Demos';
end;
{$ENDIF}

procedure TSSLClientFrame.cbDBDatabaseDropDown(Sender: TObject);
begin
  UniConnection.GetDatabaseNames(cbDBDatabase.Items)
end;

procedure TSSLClientFrame.cbDBDatabaseChange(Sender: TObject);
begin
  UniTable.Close;
  UniConnection.Database := cbDBDatabase.Text;
  cbTableName.Text := '';
end;

procedure TSSLClientFrame.UniConnectionBeforeConnect(Sender: TObject);
var
  Cert: TScCertificate;
begin
  if cbSSL.Checked then begin
    ScCryptoAPIStorage.Certificates.Clear;
    Cert := TScCertificate.Create(ScCryptoAPIStorage.Certificates);
    Cert.CertName := CRSSLIOHandler.CACertName;
    Cert.ImportFrom(edCACertName.Text);

    Cert := TScCertificate.Create(ScCryptoAPIStorage.Certificates);
    Cert.CertName := CRSSLIOHandler.CertName;
    Cert.ImportFrom(edCertName.Text);
    Cert.Key.ImportFrom(edKeyName.Text);

    CheckRandomize;
    UniConnection.IOHandler := CRSSLIOHandler;
    UniConnection.SpecificOptions.Values['SSLMode'] := 'smRequire'; // for Pg
    UniConnection.SpecificOptions.Values['Protocol'] := 'mpSSL';  // for MySQL
  end
  else begin
    UniConnection.IOHandler := nil;
    UniConnection.SpecificOptions.Values['SSLMode'] := 'smDisable'; // for Pg
    UniConnection.SpecificOptions.Values['Protocol'] := 'mpDefault'; // for MySQL
  end;

  UniConnection.ProviderName := cbProvider.Text;
  UniConnection.Server := edDBHost.Text;
  UniConnection.Port := seDBPort.Value;
  UniConnection.Username := edDBUserName.Text;
  UniConnection.Password := edDBPassword.Text;
  UniConnection.Database := cbDBDatabase.Text;
end;

procedure TSSLClientFrame.sbCACertNameClick(Sender: TObject);
begin
  OpenDialog.Filter := CertFilter;
  OpenDialog.Title := 'Import certificate';
  if OpenDialog.Execute then
    edCACertName.Text := OpenDialog.FileName;
end;

procedure TSSLClientFrame.sbCertNameClick(Sender: TObject);
begin
  OpenDialog.Filter := CertFilter;
  OpenDialog.Title := 'Import certificate';
  if OpenDialog.Execute then
    edCertName.Text := OpenDialog.FileName;
end;

procedure TSSLClientFrame.sbKeyNameClick(Sender: TObject);
begin
  OpenDialog.Filter := KeyFilter;
  OpenDialog.Title := 'Import key';
  if OpenDialog.Execute then
    edKeyName.Text := OpenDialog.FileName;
end;

end.
